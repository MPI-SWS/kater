/*
 * KATER -- Automating Weak Memory Model Metatheory
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, you can access it online at
 * http://www.gnu.org/licenses/gpl-3.0.html.
 */

#include "Kater.hpp"

#include "AdjList.hpp"
#include "Config.hpp"
#include "GenMCPrinter.hpp"
#include "NFAUtils.hpp"
#include "RegExpUtils.hpp"
#include "Saturation.hpp"
#include "Utils.hpp"
#include "Visitor.hpp"

#include <algorithm>
#include <deque>
#include <execution>
#include <numeric>
#include <optional>
#include <string>
#include <utility>

#define DEBUG_TYPE "kater"

/*************************************************************
 *              Types and generic utils functions
 ************************************************************/

/* Allowed LHS of comp scheme: [A];po;[B] */
struct CompSchemeLHS {
	std::optional<PredicateSet> pre;
	std::optional<PredicateSet> post;
};

/* Allowed RHS of comp scheme: [A];po;[C];po;[B] */
struct CompSchemeRHS {
	std::optional<PredicateSet> pre;
	PredicateSet fence;
	std::optional<PredicateSet> post;
};

/*************************************************************
 *              Regexp utilities
 ************************************************************/

void expandRfs(SubsetConstraint &subsetC, const KatModule &module)
{
	/* We will expand rf on the LHS, but only if the RHS uses rfe/rfi */
	auto rhsHasRfiRfe = false;
	visitRE(subsetC.getRHSRef(), [&](auto &re) {
		auto *charRE = dynamic_cast<const CharRE *>(&*re);
		if (charRE && charRE->getLabel().getRelation().has_value() &&
		    (charRE->getLabel().getRelation()->getID() ==
			     Relation::createBuiltin(Relation::BuiltinID::rfe).getID() ||
		     charRE->getLabel().getRelation()->getID() ==
			     Relation::createBuiltin(Relation::BuiltinID::rfi).getID()))
			rhsHasRfiRfe = true;
	});
	if (!rhsHasRfiRfe)
		return;

	auto rf = module.getRegisteredRE("rf");

	auto rfe = module.getRegisteredRE("rfe")->clone();
	auto rfi = module.getRegisteredRE("rfi")->clone();
	auto alt = AltRE::createOpt(std::move(rfe), std::move(rfi));
	replaceREWith(subsetC.getLHSRef(), rf, &*alt);
}

std::pair<PredicateSet, unsigned> collectPredicateKids(const RegExp *re, unsigned startIdx)
{
	PredicateSet result;

	auto i = startIdx;
	while (i < re->getNumKids() && re->getKid(i)->isPredicate()) {
		if (i == 0)
			result = dynamic_cast<const CharRE *>(re->getKid(i))
					 ->getLabel()
					 .getPreChecks();
		else
			result.insert(dynamic_cast<const CharRE *>(re->getKid(i))
					      ->getLabel()
					      .getPreChecks());
		++i;
	}
	return {result, i};
}

/*************************************************************
 *              NFA utilities
 ************************************************************/

void removeConsecutivePredicates(NFA &nfa, const Theory &theory)
{
	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s) {
		nfa.removeTransitionsIf(
			&*s, [&](auto &t) { return t.dest == &*s && t.label.isPredicate(); });
	});

	/* Collect states w/ incoming both preds + rels */
	std::vector<NFA::State *> toDuplicate;
	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s) {
		if (std::any_of(s->in_begin(), s->in_end(),
				[&](auto &t) { return t.label.isPredicate(); }) &&
		    std::any_of(s->in_begin(), s->in_end(),
				[&](auto &t) { return !t.label.isPredicate(); })) {
			toDuplicate.push_back(&*s);
		}
	});

	std::for_each(toDuplicate.begin(), toDuplicate.end(), [&](auto &s) {
		nfa.splitState(s, [&](auto &t) { return t.label.isPredicate(); });
	});

	addTransitivePredicateEdges(nfa, theory);

	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s) {
		nfa.removeTransitionsIf(
			&*s, [&](auto &t) { return t.dest == &*s && t.label.isPredicate(); });
	});

	/* NF: ensure that if S -ε-> S' and S' is final, S is final too */
	for (auto &sUP : nfa.states()) {
		for (auto &t : sUP->outs() | std::views::filter([&](auto &t) {
				       return t.dest->isAccepting() && t.label.isTruePredicate();
			       })) {
			nfa.makeAccepting(&*sUP);
		}
	}
}

void normalize(NFA &nfa, const Theory &theory)
{
	simplify(nfa, theory);
	saturateDomains(nfa, theory);
	breakToParts(nfa);
	removeConsecutivePredicates(nfa, theory);
	removeDeadStates(nfa);
}

void reduce(NFA &nfa, const Theory &theory)
{
	simplify(nfa, theory);
	std::for_each(nfa.accept_begin(), nfa.accept_end(), [&](auto &s) {
		nfa.removeTransitionsIf(&*s, [&](const NFA::Transition &t) {
			return std::any_of(nfa.start_begin(), nfa.start_end(),
					   [&](auto &q) { return q->hasOutgoingTo(t.dest); });
		});
	});
	simplify(nfa, theory);
}

void pruneNFA(NFA &nfa, const NFA &other, const Theory &theory)
{
	std::vector<Relation> orels;
	std::vector<PredicateSet> opreds;
	std::for_each(other.states_begin(), other.states_end(), [&](auto &s) {
		std::for_each(s->out_begin(), s->out_end(), [&](auto &t) {
			if (!t.label.isPredicate()) {
				orels.push_back(*t.label.getRelation());
			} else {
				opreds.push_back(t.label.getPreChecks());
			}
		});
	});

	std::sort(opreds.begin(), opreds.end());
	opreds.erase(std::unique(opreds.begin(), opreds.end()), opreds.end());
	std::sort(orels.begin(), orels.end());
	orels.erase(std::unique(orels.begin(), orels.end()), orels.end());

	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s) {
		nfa.removeTransitionsIf(&*s, [&](auto &t) {
			return (t.label.isRelation() &&
				std::none_of(orels.begin(), orels.end(),
					     [&](auto &r) {
						     return theory.isIncludedIn(
							     r, *t.label.getRelation());
					     })) ||
			       (t.label.isPredicate() &&
				std::none_of(t.label.getPreChecks().begin(),
					     t.label.getPreChecks().end(),
					     [&](auto &p) { return p.isComplement(); }) &&
				std::none_of(opreds.begin(), opreds.end(), [&](auto &p) {
					return theory.isIncludedIn(p, t.label.getPreChecks());
				}));
		});
	});
}

/*************************************************************
 *             Assumption expanding/saturation
 ************************************************************/

std::pair<bool, CompSchemeLHS> extractCompLHSInfo(const RegExp *lRE, const RegExp *poRE)
{
	CompSchemeLHS result;

	auto *seqRE = dynamic_cast<const SeqRE *>(lRE);
	if (!seqRE)
		return {false, result};

	auto i = 0U;
	auto [pre, i1] = collectPredicateKids(seqRE, i);
	if (i1 > i)
		result.pre = pre;
	i = i1;

	if (i >= seqRE->getNumKids() || *seqRE->getKid(i) != *poRE)
		return {false, result};
	++i;

	auto [post, i2] = collectPredicateKids(seqRE, i);
	if (i2 > i)
		result.post = post;
	i = i2;
	return {true, result};
}

std::pair<bool, CompSchemeRHS> extractCompRHSInfo(const RegExp *rRE, const RegExp *poRE)
{
	CompSchemeRHS result;

	auto *seqRE = dynamic_cast<const SeqRE *>(rRE);
	if (!seqRE)
		return {false, result};

	auto i = 0U;
	auto [pre, i1] = collectPredicateKids(seqRE, i);
	if (i1 > i)
		result.pre = pre;
	i = i1;

	if (i >= seqRE->getNumKids() || *seqRE->getKid(i) != *poRE) {
		return {false, result};
	}
	++i;

	auto [fence, i2] = collectPredicateKids(seqRE, i);
	if (i == i2)
		return {false, result};
	result.fence = fence;
	i = i2;

	if (i >= seqRE->getNumKids() || *seqRE->getKid(i) != *poRE) {
		return {false, result};
	}
	++i;

	auto [post, i3] = collectPredicateKids(seqRE, i);
	if (i3 > i)
		result.post = post;
	i = i3;
	return {true, result};
}

std::pair<bool, CompSchemeRHS> isSupportedCompScheme(const SubsetConstraint *assm,
						     const RegExp *poRE)
{
	auto [lok, lhs] = extractCompLHSInfo(assm->getLHS(), poRE);
	auto [rok, rhs] = extractCompRHSInfo(assm->getRHS(), poRE);

	if (!lok || !rok)
		return {false, rhs};
	if (rhs.pre.has_value() && lhs.pre != rhs.pre)
		return {false, rhs};
	if (rhs.post.has_value() && lhs.post != rhs.post)
		return {false, rhs};

	rhs.pre = lhs.pre;
	rhs.post = lhs.post;
	return {true, rhs};
}

NFA createCompilationNFA(const CompSchemeRHS &rhs, const Theory &theory)
{
	std::vector<std::unique_ptr<RegExp>> disj;
	for (auto &p : rhs.fence) {
		auto pc = p;
		pc.complement();
		disj.push_back(CharRE::create(TransLabel(std::nullopt, {pc})));
	}
	assert(!disj.empty());
	auto fenceExp = disj.size() == 1 ? std::move(disj[0]) : AltRE::create(std::move(disj));
	auto po = CharRE::create(TransLabel(Relation::createBuiltin(Relation::BuiltinID::po)));
	PredicateSet pre = rhs.pre.has_value() ? *rhs.pre : PredicateSet{};
	PredicateSet post = rhs.post.has_value() ? *rhs.post : PredicateSet{};

	auto reC = SeqRE::createOpt(
		CharRE::create(TransLabel(std::nullopt, pre)),
		StarRE::createOpt(SeqRE::createOpt(po->clone(), std::move(fenceExp))), po->clone(),
		CharRE::create(TransLabel(std::nullopt, post)));

	auto lNFA = reC->toNFA();
	normalize(lNFA, theory);
	return lNFA;
}

void expandSubsetAssumption(NFA &rhs, const SubsetConstraint *assm, const KatModule &module)
{
	auto &theory = module.getTheory();
	auto *lRE = assm->getLHS();
	auto *rRE = assm->getRHS();

	auto lNFA = lRE->toNFA();
	normalize(lNFA, theory);

	/* Handle `A <= 0` assumption */
	if (rRE->isFalse()) {
		saturateEmpty(rhs, std::move(lNFA));
		return;
	}

	/* Handle `A <= id` assumption */
	if (*rRE == *RegExp::createId()) {
		saturateID(rhs, std::move(lNFA));
		return;
	}

	/* Transform `A ; A <= A` to `transitive A` */
	auto *seqRE = dynamic_cast<const SeqRE *>(lRE);
	if (seqRE && *rRE == *seqRE->getKid(0) && *rRE == *seqRE->getKid(1) &&
	    dynamic_cast<const CharRE *>(rRE)) {
		auto rel = *dynamic_cast<const CharRE *>(rRE)->getLabel().getRelation();
		saturateTransitive(rhs, rel);
		return;
	}
	/* Transform `A+ <= A` to `transitive A` */
	auto *plusRE = dynamic_cast<const PlusRE *>(&*lRE);
	if (plusRE && *rRE == *plusRE->getKid(0) && dynamic_cast<const CharRE *>(rRE)) {
		auto rel = *dynamic_cast<const CharRE *>(rRE)->getLabel().getRelation();
		saturateTransitive(rhs, rel);
		return;
	}

	// FIXME: Also discard A <= A{*,+}
	/* Discard `A <= A?` assumption */
	auto *charRE = dynamic_cast<const CharRE *>(&*lRE);
	if (charRE && dynamic_cast<const QMarkRE *>(&*rRE) && *rRE->getKid(0) == *charRE) {
		return;
	}
	/* Handle `A <= builtin` assumption */
	auto *charRHS = dynamic_cast<const CharRE *>(&*rRE);
	if (charRHS) {
		saturateBuiltin(rhs, *dynamic_cast<const CharRE *>(rRE)->getLabel().getRelation(),
				std::move(lNFA), theory);
		return;
	}
	/* Handle `[A];po;[B] <= [A];po;[C];po;[B]` assumption */
	auto [supported, compRHS] = isSupportedCompScheme(assm, module.getRegisteredRE("po"));
	if (supported) {
		saturateEmpty(rhs, createCompilationNFA(compRHS, theory));
		return;
	}

	std::cerr << "[Warning] Ignoring unsupported assumption " << *assm << "\n";
	return;
}

void expandAssumption(NFA &rhs, const AssumeStatement *assm, const KatModule &module)
{
	assert(assm);

	auto *cst = assm->getConstraint();
	if (const auto *cc = dynamic_cast<const EqualityConstraint *>(cst)) {
		expandSubsetAssumption(rhs, cc, module);
		expandSubsetAssumption(rhs, cc, module);
		return;
	}
	if (const auto *tc = dynamic_cast<const TotalityConstraint *>(cst)) {
		saturateTotal(rhs,
			      *dynamic_cast<const CharRE *>(tc->getRE())->getLabel().getRelation());
		return;
	}
	// FIXME: What about SubsetSameEnds???
	if (const auto *ec = dynamic_cast<const SubsetConstraint *>(cst)) {
		expandSubsetAssumption(rhs, ec, module);
		return;
	}
	std::cerr << "Ignoring unsupported local assumption " << *assm << "\n";
	return;
}

/*************************************************************
 *              Inclusion checking
 ************************************************************/

void completeCounterexample(const NFA &nfa, NFA::State *s, Counterexample &cex)
{
	if (s->isAccepting())
		return;

	std::deque<std::pair<NFA::State *, Counterexample>> worklist(1, {s, cex});
	std::unordered_set<NFA::State *> visited({s});

	while (!worklist.empty()) {
		auto [s, c] = worklist.front();
		worklist.pop_front();

		if (s->isAccepting()) {
			cex = c;
			break;
		}

		for (auto it = s->out_begin(); it != s->out_end(); ++it) {
			auto nc(c);
			nc.extend(it->label);

			if (visited.contains(it->dest))
				continue;

			visited.insert(it->dest);
			worklist.emplace_back(it->dest, nc);
		}
	}
}

void Kater::printCounterexample(const Counterexample &cex) const
{
	std::cerr << "Counterexample: ";
	if (cex.empty()) {
		std::cerr << "ε\n";
		return;
	}

	auto i = 0U;
	std::for_each(cex.begin(), cex.end(), [&](auto &lab) {
		if (lab.isRelation()) {
			std::cerr << getModule().getTheory().getName(*lab.getRelation());
		} else {
			assert(lab.isPredicate());
			auto first = true;
			std::cerr << "[";
			for (auto &p : lab.getPreChecks()) {
				if (!first)
					std::cerr << "&";
				std::cerr << getModule().getTheory().getName(p);
				first = false;
			}
			std::cerr << "]";
		}
		std::cerr << " ";
		if (cex.getType() == Counterexample::Type::TUT && i++ == cex.getMismatch()) {
			std::cerr << "===> ";
		}
	});
	if (cex.getType() == Counterexample::Type::ANA) {
		std::cerr << "(A/NA)";
	}
	std::cerr << "\n";
}

VSet<Predicate> collectNegatedOutgoing(const VSet<NFA::State *> &states)
{
	VSet<Predicate> result;

	for (auto &s : states) {
		for (auto &t : s->outs()) {
			auto negIt = std::ranges::find_if(
				t.label.getPreChecks(), [&](auto &p) { return p.isComplement(); });
			if (negIt != t.label.getPreChecks().end())
				result.insert(*negIt);
		}
	}
	return result;
}

VSet<NFA::State *> getNextRHS(const TransLabel &label, const VSet<NFA::State *> &states,
			      const Theory &theory)
{
	VSet<NFA::State *> result;

	for (auto &s : states) {
		for (auto &ot : s->outs() | std::views::filter([&](auto &ot) {
					return theory.isIncludedIn(label, ot.label);
				}))
			result.insert(ot.dest);
	}
	return result;
}

std::vector<TransLabel> maybeSplitToComplements(const TransLabel &label,
						const VSet<Predicate> &negated,
						const Theory &theory)
{
	std::vector<TransLabel> result;

	for (const auto &p : negated) {
		/* First try with p */
		if (theory.composes(label, TransLabel(std::nullopt, {p}))) {
			auto l = label;
			l.merge(TransLabel(std::nullopt, {p}));
			result.push_back(l);
		}
		/* Then also with ~p */
		auto pneg = Predicate(p).complement();
		if (theory.composes(label, TransLabel(std::nullopt, {pneg}))) {
			auto l = label;
			l.merge(TransLabel(std::nullopt, {p}));
			result.push_back(l);
		}
	}

	/* If there were no negated transitions, or nothing composes w/ label, just give up */
	if (negated.empty())
		result.push_back(label);
	return result;
}

class SimState {

public:
	SimState() = delete;
	SimState(NFA::State *lhs, VSet<NFA::State *> rhs) : lhs(lhs), rhs(std::move(rhs)) {}
	SimState(NFA::State *lhs, VSet<NFA::State *> &&rhs) : lhs(lhs), rhs(std::move(rhs)) {}

	SimState(const SimState &) = default;
	SimState(SimState &&) = default;

	auto operator=(const SimState &) -> SimState & = default;
	auto operator=(SimState &&) -> SimState & = default;

	auto getLHS() const -> NFA::State * { return lhs; }
	auto getRHS() const -> const VSet<NFA::State *> & { return rhs; }

	auto isLHSAccepting() const -> bool { return lhs->isAccepting(); }
	auto isRHSAccepting() const -> bool
	{
		return std::any_of(rhs.begin(), rhs.end(),
				   [&](auto *s) { return s->isAccepting(); });
	}

	auto operator<=>(const SimState &other) const -> bool = default;

private:
	NFA::State *lhs;
	VSet<NFA::State *> rhs;
};

// XXX: FIXME
struct SimStateHasher {
	auto operator()(const SimState &p) const -> std::size_t
	{
		std::size_t hash = 0;
		hash_combine<unsigned>(hash, p.getLHS()->getId());
		std::for_each(p.getRHS().begin(), p.getRHS().end(),
			      [&](auto *s2) { hash_combine<unsigned>(hash, s2->getId()); });
		return hash;
	}
};

auto Kater::isDFASubLanguageOfNFA(NFA &nfa, const NFA &other) const -> InclusionResult
{
	KATER_DEBUG(std::cout << "Checking inclusion between automata:" << std::endl;
		    std::cout << nfa << "and " << other << std::endl;);

	if (other.getNumStarting() == 0) {
		Counterexample cex;
		return {nfa.acceptsNoString(cex), cex};
	}

	std::unordered_set<SimState, SimStateHasher> visited;
	std::deque<std::pair<SimState, Counterexample>> workList;

	std::for_each(nfa.start_begin(), nfa.start_end(), [&](auto *s1) {
		VSet<NFA::State *> ss(other.start_begin(), other.start_end());
		visited.insert({s1, ss});
		workList.push_back({{s1, ss}, Counterexample()});
	});
	while (!workList.empty()) {
		auto [state, c] = workList.front();
		workList.pop_front();

		if (state.isLHSAccepting() && !state.isRHSAccepting()) {
			c.setType(Counterexample::Type::ANA);
			return {false, c};
		}

		auto negated = collectNegatedOutgoing(state.getRHS());
		for (auto &t : state.getLHS()->outs()) {

			auto nc(c);
			nc.extend(t.label);

			auto labels =
				maybeSplitToComplements(t.label, negated, getModule().getTheory());
			for (auto &l : labels) {
				auto nextLHS = t.dest;
				auto nextRHS =
					getNextRHS(l, state.getRHS(), getModule().getTheory());

				/* If no matching step has been found, report a counterexample */
				SimState next = {nextLHS, nextRHS};
				if (nextRHS.empty()) {
					nc.setType(Counterexample::Type::TUT);
					completeCounterexample(nfa, next.getLHS(), nc);
					return {false, nc};
				}
				/* Proceed if the pair has not been examined */

				if (visited.count(next) != 0U)
					continue;
				visited.insert(next);
				workList.push_back({next, nc});
			}
		}
	}
	return {true, {}};
}

auto Kater::checkInclusion(SubsetConstraint &subsetC) const -> InclusionResult
{
	auto &module = getModule();
	auto &theory = module.getTheory();

	expandRfs(subsetC, module);

	auto nfa1 = subsetC.getLHS()->toNFA();
	normalize(nfa1, theory);
	removeSimilarTransitions(nfa1);
	if (subsetC.sameEnds()) {
		saturateInitFinalPreds(nfa1, theory);
		saturateLoc(nfa1, theory);
	}
	removeSimilarTransitions(nfa1);
	auto lhs = nfa1.to_DFA().first;

	auto nfa2 = subsetC.getRHS()->toNFA();
	normalize(nfa2, theory);
	for (const auto &assm : theory.assumes()) {
		expandAssumption(nfa2, &*assm, module);
		normalize(nfa2, theory);
	}
	pruneNFA(nfa2, lhs, theory);
	removeDeadStates(nfa2);
	normalize(nfa2, theory);
	if (subsetC.rotated()) {
		saturateRotate(nfa2, theory);
		normalize(nfa2, theory);
	}
	/* doing another similarity pass here would
	 * further minimize the NFA, but minimizing
	 * doesn't seem to help. The remaining time
	 * is determined by the LHS anyway, and these
	 * passes are costly */
	return isDFASubLanguageOfNFA(lhs, nfa2);
}

auto Kater::checkAssertion(Constraint &cst) -> InclusionResult
{
	if (getConf().verbose >= 2) {
		std::cout << "Checking assertion " << cst << std::endl;
	}

	InclusionResult result;
	auto visitor = make_visitor(
		type_list<SubsetConstraint, EqualityConstraint>{},
		[&](const SubsetConstraint &sc) {
			result = checkInclusion(const_cast<SubsetConstraint &>(sc));
		},
		[&](const EqualityConstraint &ec) {
			result = checkInclusion(const_cast<EqualityConstraint &>(ec));
			if (result.result) {
				auto invC = SubsetConstraint::create(ec.getRHS()->clone(),
								     ec.getLHS()->clone(),
								     ec.sameEnds(), ec.rotated());
				result = checkInclusion(*invC);
			}
		});
	visitor(cst);
	return result;
}

auto Kater::checkAssertions() -> bool
{
	/*
	 * First, simplify the module.
	 * TODO: In principle, we could optimize the REs too; if we
	 * decide to do so, the relevant routines should be placed in
	 * a new file.
	 */
	getModule().getTheory().simplify();

	/* Helper function to dispatch checkers as the different policies do not share a type */
	auto maybe_parallelize = [&]<typename F>(F f) {
#ifdef __cpp_lib_execution
		return getConf().parallel ? f(std::execution::par_unseq) : f(std::execution::seq);
#else
		return f(false); // ifdef types don't match but this is OK (false remains unused)
#endif
	};

	/* Go ahead and check */
	std::vector<std::pair<DbgInfo, Counterexample>> errors;
	auto status = true;
	maybe_parallelize([&](auto policy) {
		auto asserts = module->asserts();
		std::for_each(
#ifdef __cpp_lib_execution
			policy,
#endif
			asserts.begin(), asserts.end(), [&](auto &assert) {
				auto [ok, cex] = checkAssertion(*assert->getConstraint());
				if (!ok) {
					status = false;
					errors.push_back({assert->getDbgInfo(), cex});
				}
			});
	});

	/* Print all counterexamples */
	for (const auto &err : errors) {
		std::cerr << err.first << ": [Error] Assertion does not hold.\n";
		printCounterexample(err.second);
	}
	return status;
}

/*************************************************************
 *              Code export
 ************************************************************/

/*
 * Checks whether each kid of some MutRecRE is using productive recursive calls.
 * E.g., :
 *  - let rec x = x        => not productive
 *  - let rec x = x? ; po  => productive
 *  - let rec x = y | po
 *        and y = [REL]    => productive (x non productive, y productive)
 *
 * Returns a vector of pairs denoting whether each kid is productive, and its starting rec symbols
 */
auto collectProductiveRecursions(const MutRecRE *re) -> std::vector<std::pair<bool, VSet<Relation>>>
{
	/* Helper lambda to recurse to all kids */
	auto checkProdRec = [&](const RegExp *re, const VSet<Relation> &recs,
				auto &funRef) -> std::pair<bool, VSet<Relation>> {
		if (auto *charRE = dynamic_cast<const CharRE *>(re)) {
			/* [] are non-productive */
			if (charRE->isPredicate())
				return {false, {}};
			/* r in recs is also non productive, because it can only appear at the
			 * leftmost place of composition */
			if (recs.contains(*charRE->getLabel().getRelation()))
				return {false, {*charRE->getLabel().getRelation()}};
			/* all other relations are productive */
			return {true, {}};
		}
		/* r?, r* are non-productive */
		auto *qmarkRE = dynamic_cast<const QMarkRE *>(re);
		auto *starRE = dynamic_cast<const StarRE *>(re);
		if (qmarkRE || starRE) {
			return {false, funRef(re->getKid(0), recs, funRef).second};
		}
		/* r=r1;...;rn is productive if tail(r) is productive */
		if (auto *seqRE = dynamic_cast<const SeqRE *>(re)) {
			assert(seqRE->getNumKids() > 1);
			auto tailProductive =
				std::any_of(seqRE->kid_begin() + 1, seqRE->kid_end(), [&](auto &k) {
					return funRef(&*k, recs, funRef).first;
				});
			return {tailProductive, funRef(seqRE->getKid(0), recs, funRef).second};
		}

		/* In all other cases, just accumulate kid results */
		return std::accumulate(
			re->kid_begin(), re->kid_end(), std::pair<bool, VSet<Relation>>(true, {}),
			[&](auto acc, auto &elem) {
				auto result = funRef(&*elem, recs, funRef);
				if (!result.first)
					acc.second.insert(result.second);
				return std::make_pair(acc.first && result.first, acc.second);
			});
	};

	auto recs = re->recs();
	VSet<Relation> rels(std::ranges::begin(recs), std::ranges::end(recs));
	return std::accumulate(re->kid_begin(), re->kid_end(),
			       std::vector<std::pair<bool, VSet<Relation>>>(),
			       [&](auto acc, auto &elem) {
				       acc.push_back(checkProdRec(&*elem, rels, checkProdRec));
				       return std::move(acc);
			       });
}

/*
 * Returns a "call-graph" of productive calls:
 *  whenever a recursive relation X non-productively calls Y (see checkProductiveRecusion),
 *  the graph contains an edge Y->X
 */
auto constructRecCallGraph(const MutRecRE *re) -> AdjList<Relation, RelationHasher>
{
	auto recs = re->recs();
	std::vector<Relation> rels(std::ranges::begin(recs), std::ranges::end(recs));
	AdjList<Relation, RelationHasher> graph(rels);
	auto i = 0U;
	for (auto &[prod, starting] : collectProductiveRecursions(re)) {
		if (!prod) {
			for (auto &rel : starting)
				graph.addEdge(rel, re->getRec(i));
		}
		i++;
	}
	return graph;
}

auto containsMandatoryRegistrations(const KatModule &module) -> bool
{
	auto ppo = module.getPPODeclaration();
	if (!ppo) {
		std::cerr << "[Error] No top-level ppo definition provided\n";
		return false;
	}
	auto hb = module.getHBDeclaration();
	if (!hb) {
		std::cerr << "[Error] No top-level hb_stable definition provided\n";
		return false;
	}
	if (!dynamic_cast<const ViewExp *>(hb->getSaved())) {
		std::cerr << "[Error] hb_stable needs to be stored in a view\n";
		return false;
	}
	auto coh = module.getCOHDeclaration();
	if (!coh) {
		std::cerr << "[Error] No coherence constraint provided\n";
		return false;
	}
	if (!dynamic_cast<const ViewExp *>(coh->getSaved())) {
		std::cerr << "[Error] Coherence constraint needs to take a view argument\n";
		return false;
	}
	return true;
}

auto Kater::isPPOIntersectionInPPO(const AcyclicConstraint *acyc) const -> InclusionResult
{
	auto &theory = getModule().getTheory();
	auto nfa = acyc->getRE()->toNFA();

	auto poNFA = getModule().getRegisteredRE("po")->toNFA();
	normalize(poNFA, theory);
	removeSimilarTransitions(poNFA);
	auto polocNFA = getModule().getRegisteredRE("po-loc")->toNFA();
	normalize(polocNFA, theory);
	removeSimilarTransitions(polocNFA);

	nfa.star();
	normalize(nfa, theory);
	removeSimilarTransitions(nfa);

	nfa.clearAllStarting();
	nfa.clearAllAccepting();
	VSet<NFA::State *> toSplit;
	for (auto &sUP : nfa.states()) {
		for (auto &t : sUP->ins()) {
			if (t.label.isRelation() && theory.isEco(t.label)) {
				toSplit.insert(&*sUP);
			}
		}
	}
	for (auto *s : toSplit) {
		auto *d = nfa.splitState(s, [&](auto &t) { return theory.isEco(t.label); });
		for (auto &t : s->ins())
			nfa.makeAccepting(t.dest);
	}
	for (auto &sUP :
	     nfa.states() | std::views::filter([&](auto &sUP) {
		     return sUP->hasAllOutPredicates() && // !sUP->isAccepting() &&
			    std::none_of(sUP->out_begin(), sUP->out_end(),
					 [&](auto &t) { return t.dest->isAccepting(); });
	     })) {
		nfa.makeStarting(&*sUP);
	}
	for (auto &sUP : nfa.states())
		nfa.removeTransitionsIf(&*sUP, [&](auto &t) {
			return t.label.isRelation() && theory.isEco(t.label);
		});

	normalize(nfa, theory);
	removeSimilarTransitions(nfa);

	auto lhs = nfa.to_DFA().first;

	auto nfa2 = AltRE::createOpt(
			    SeqRE::createOpt(
				    StarRE::createOpt(CharRE::create(TransLabel(
					    Relation::createBuiltin(Relation::BuiltinID::any)))),
				    getModule().createPPORF(),
				    StarRE::createOpt(CharRE::create(TransLabel(
					    Relation::createBuiltin(Relation::BuiltinID::any))))),
			    RegExp::createId())
			    ->toNFA();
	normalize(nfa2, theory);
	for (const auto &assm : theory.assumes()) {
		expandAssumption(nfa2, &*assm, getModule());
		normalize(nfa2, theory);
	}
	pruneNFA(nfa2, lhs, theory);
	removeDeadStates(nfa2);
	normalize(nfa2, theory);
	return isDFASubLanguageOfNFA(lhs, nfa2);
}

auto Kater::checkExportRequirements() -> bool
{
	auto &module = getModule();

	if (!containsMandatoryRegistrations(module))
		return false;

	/* Ensure that pporf is implied by the acyclicity constraints */
	auto pporf = module.createPPORF();
	auto acycsView =
		module.exports() | std::views::filter([&](auto &stmt) {
			return dynamic_cast<const AcyclicConstraint *>(stmt->getConstraint());
		}) |
		std::views::transform([&](auto &stmt) {
			return dynamic_cast<const AcyclicConstraint *>(stmt->getConstraint());
		});
	auto acycDisj = std::accumulate(acycsView.begin(), acycsView.end(), RegExp::createFalse(),
					[&](auto re1, auto *acyc) {
						return AltRE::createOpt(re1->clone(),
									acyc->getRE()->clone());
					});
	auto noOOTA = SubsetConstraint::create(
		pporf->clone(), StarRE::createOpt(std::move(acycDisj)), false, false);
	auto [status, cex] = checkAssertion(*noOOTA);
	if (!status) {
		std::cerr << "[Warning] Acyclicity constraints do not preclude OOTA.\n";
		std::cerr << "OOTA needs to be enforced by the model checker\n";
		printCounterexample(cex);
		status = true; /* treat as a soft error */
	}

	/* Check extensibility */
	for (auto *ac : acycsView) {
		if (auto [ok, cex] = isPPOIntersectionInPPO(ac); !ok) {
			std::cerr << "[Error] acyclic constraint /\\ ar is not included in ppo: "
				  << *ac << "\n";
			printCounterexample(cex);
			status = false;
		}
	}

	/* Check correctness of "unless" clauses in acyclicity constraints */
	for (auto &stmt : module.exports()) {
		auto *currentCst = dynamic_cast<const AcyclicConstraint *>(stmt->getConstraint());
		if (!currentCst || !stmt->getUnless())
			continue;

		/* A: all acyclicity constraints */
		auto acycDisj = std::accumulate(acycsView.begin(), acycsView.end(),
						RegExp::createFalse(), [&](auto re1, auto *ac) {
							return AltRE::createOpt(
								re1->clone(), ac->getRE()->clone());
						});
		/* A_R: acyclicity constraints w/o the current one */
		auto acycDisjRest = std::accumulate(
			acycsView.begin(), acycsView.end(), RegExp::createFalse(),
			[&](auto re1, auto *ac) {
				return *ac->getRE() == *currentCst->getRE()
					       ? std::move(re1)
					       : AltRE::createOpt(re1->clone(),
								  ac->getRE()->clone());
			});

		/* it must be: A_R => A (assuming the unless holds) */
		module.getTheory().registerAssume(
			AssumeStatement::create(stmt->getUnless()->clone(), true));
		auto unlessOK = SubsetConstraint::create(std::move(acycDisj),
							 std::move(acycDisjRest), false, false);
		auto [status, cex] = checkAssertion(*unlessOK);
		if (!status) {
			std::cerr << "[Error] \"unless\" clause does not imply acyclicity "
				     "constraint: "
				  << *currentCst << "\n";
			printCounterexample(cex);
			status = false;
		}
		module.getTheory().clearTempAssumes();
	}

	/* Ensure that all saved relations are included in pporf;ppo and are transitive */
	auto *ppo = module.getPPODeclaration()->getRE();
	for (auto &let : module.lets()) {
		if (dynamic_cast<const NoSavedExp *>(let->getSaved()))
			continue;

		Counterexample cex;
		if (!dynamic_cast<const ViewExp *>(let->getSaved())) {
			auto savedInPO = SubsetConstraint::create(
				let->getRE()->clone(),
				StarRE::createOpt(SeqRE::createOpt(
					StarRE::createOpt(pporf->clone()), ppo->clone())),
				false, false);
			auto result = checkAssertion(*savedInPO);
			if (!result.result) {
				std::cerr << "[Error] Saved relation not included in pporf;ppo: "
					  << *let->getRE() << "\n";
				printCounterexample(result.cex);
				status = false;
			}
		} else {
			auto savedInPO = SubsetConstraint::create(
				let->getRE()->clone(),
				AltRE::createOpt(
					SeqRE::createOpt(StarRE::createOpt(module.createPORF()),
							 module.getRegisteredRE("po")->clone()),
					RegExp::createId()),
				false, false);
			auto result = checkAssertion(*savedInPO);
			if (!result.result) {
				std::cerr << "[Error] View not included in porf;po | id: "
					  << *let->getRE() << "\n";
				printCounterexample(result.cex);
				status = false;
			}
		}

		if (!dynamic_cast<const NoSavedExp *>(let->getSaved())) {
			cex.clear();
			// FIXME: If we ever allow customly-reduced relations, prefix needs to be
			// changed auto prefix = (sv.status == VarStatus::Reduce) ? sv.red->clone()
			// 					       : ppo->clone();
			auto prefix = ppo->clone();
			auto seqExp = SeqRE::createOpt(std::move(prefix), let->getRE()->clone());
			auto savedTrans = SubsetConstraint::createOpt(
				std::move(seqExp), let->getRE()->clone(), false, false);
			auto result = checkAssertion(*savedTrans);
			if (!result.result) {
				std::cerr << "[Error] Reduced relation not transitive: "
					  << *let->getRE() << "\n";
				printCounterexample(result.cex);
				status = false;
			}
		}

		/* For mutually recursive views, check that we can export them in some order
		 */
		if (dynamic_cast<const ViewExp *>(let->getSaved()) &&
		    dynamic_cast<const MutRecRE *>(let->getRE())) {
			if (!constructRecCallGraph(dynamic_cast<const MutRecRE *>(let->getRE()))
				     .transClosure()
				     .isIrreflexive()) {
				std::cerr << "[Error] Could not resolve view calculation "
					     "order: "
					  << *let->getRE() << "\n";
				status = false;
			}
		}
	}
	return status;
}

/* Helper function that, given an expression r ~ A | C;b+ applies FUN to b+ */
template <typename F> void foreachTrailingTransBuiltin(std::unique_ptr<RegExp> &reUP, F &&fun)
{
	if (auto *charRE = dynamic_cast<const CharRE *>(&*reUP))
		return;

	/* If r ~ A | B or r ~ A? or r ~ A* or r ~ r;A, recurse to kids */
	auto *qmarkRE = dynamic_cast<const QMarkRE *>(&*reUP);
	auto *altRE = dynamic_cast<const AltRE *>(&*reUP);
	auto *starRE = dynamic_cast<const AltRE *>(&*reUP);
	auto *mutRecRE = dynamic_cast<const MutRecRE *>(&*reUP);
	if (qmarkRE || altRE || starRE || mutRecRE) {
		for (auto &kRE : reUP->kids())
			foreachTrailingTransBuiltin(kRE, fun);
		return;
	}

	/* If r ~ A;...;C recurse to C */
	if (auto *seqRE = dynamic_cast<const SeqRE *>(&*reUP)) {
		foreachTrailingTransBuiltin(reUP->getKid(reUP->getNumKids() - 1), fun);
		return;
	}

	/* If r ~ B+ apply FUN if B is builtin; recurse otherwise */
	auto *plusRE = dynamic_cast<const PlusRE *>(&*reUP);
	assert(plusRE);
	if (auto *charRE = dynamic_cast<const CharRE *>(plusRE->getKid(0))) {
		if (charRE->isRelation() && charRE->getLabel().getRelation()->isBuiltin())
			fun(reUP);
		return;
	}
	foreachTrailingTransBuiltin(reUP->getKid(0), fun);
}

/*
 * Given the definition of a recursive expression r,
 * returns all builtins b such that r = r?;A;...;b+
 */
auto collectTrailingTransBuiltins(Relation rel, std::unique_ptr<RegExp> &re)
	-> VSet<std::unique_ptr<CharRE>>
{
	/* If it's not a sequence, quit */
	auto *seqRE = dynamic_cast<const SeqRE *>(&*re);
	if (!seqRE)
		return {};

	/* Otherwise, collect CharRE UPs, as we might want to edit those REs in place later */
	VSet<std::unique_ptr<CharRE>> result;

	/* Helper function that will be applied to collect builtins (re ~ b+) */
	auto collectTrailingFun = [&](std::unique_ptr<RegExp> &re) {
		result.insert(std::unique_ptr<CharRE>(
			dynamic_cast<CharRE *>(re->getKid(0)->clone().release())));
	};

	/* If r is a sequence, it has to start with r or r? (i.e., be recursive) */
	if (auto *charRE = dynamic_cast<const CharRE *>(seqRE->getKid(0))) {
		if (charRE->isRelation() && *charRE->getLabel().getRelation() == rel)
			foreachTrailingTransBuiltin(re->getKid(1), collectTrailingFun);
	}
	if (auto *qmarkRE = dynamic_cast<const QMarkRE *>(seqRE->getKid(0))) {
		auto *charRE = dynamic_cast<const CharRE *>(qmarkRE->getKid(0));
		if (charRE && charRE->isRelation() && *charRE->getLabel().getRelation() == rel)
			foreachTrailingTransBuiltin(re->getKid(1), collectTrailingFun);
	}
	return result;
}

void Kater::optimizeModuleForExport()
{
	auto &module = getModule();
	auto &theory = module.getTheory();

	/* Calculate export order for views */
	// FIXME: When we update to C++23, consider chunk_by()
	auto svars = module.lets();
	for (auto it = svars.begin(), ie = svars.end(); it != ie; ++it) {
		if (!dynamic_cast<const MutRecRE *>((*it)->getRE())) {
			continue;
		}

		auto prevIt = it;
		auto *mutRecRE = dynamic_cast<const MutRecRE *>((*it)->getRE());
		auto recs = mutRecRE->recs();
		auto graph = constructRecCallGraph(mutRecRE).transClosure();
		while (it != ie && dynamic_cast<const MutRecRE *>((*it)->getRE()) &&
		       std::ranges::find(recs, dynamic_cast<const MutRecRE *>((*it)->getRE())
						       ->getRelation()) != recs.end()) {
			++it;
		}
		std::sort(prevIt, it, [&](auto &let1, auto &let2) {
			auto rel1 = dynamic_cast<const MutRecRE *>(let1->getRE())->getRelation();
			auto rel2 = dynamic_cast<const MutRecRE *>(let2->getRE())->getRelation();
			return graph(rel1, rel2);
		});
		--it;
	}

	/* Optimize calculation for transitive views */
	for (auto &let : module.lets()) {
		auto *plusRE = dynamic_cast<const PlusRE *>(let->getRE());
		if (!plusRE || !dynamic_cast<const ViewExp *>(let->getSaved())) {
			continue;
		}

		auto rel = Relation::createUser();
		auto newViewRE =
			SeqRE::createOpt(QMarkRE::createOpt(CharRE::create(TransLabel(rel))),
					 plusRE->getKid(0)->clone());
		std::vector<std::unique_ptr<RegExp>> defs;
		defs.emplace_back(newViewRE->clone());
		auto newRE = MutRecRE::createOpt(rel, {rel}, std::move(defs));
		std::cerr << "[Warning] Transforming transitive expression ";
		let->getRE()->dump(std::cerr, &theory);
		std::cerr << " to " << *newRE << "\n";

		auto oldRE = plusRE->clone(); // copy so that it doesn't get replaced in place
		module.replaceAllUsesWith(&*oldRE, &*newRE);
		for (auto &let : module.lets()) {
			auto *viewExp = dynamic_cast<ViewExp *>(let->getSaved());
			if (!viewExp)
				continue;
			if (*viewExp->getRE() == *newRE)
				viewExp->setRE(newViewRE->clone());
		}
	}

	/* Replace builtins with their transitive counterparts for more efficient codegen */
	auto oldPo = module.getRegisteredRE("po")->clone();
	auto newPo = PlusRE::createOpt(module.getRegisteredRE("po-imm")->clone());
	auto oldPoloc = module.getRegisteredRE("po-loc")->clone();
	auto newPoloc = PlusRE::createOpt(module.getRegisteredRE("po-loc-imm")->clone());
	auto oldMo = module.getRegisteredRE("mo")->clone();
	auto newMo = PlusRE::createOpt(module.getRegisteredRE("mo-imm")->clone());
	auto oldFr = module.getRegisteredRE("fr")->clone();
	auto newFr = SeqRE::createOpt(module.getRegisteredRE("fr-imm")->clone(),
				      StarRE::createOpt(module.getRegisteredRE("mo-imm")->clone()));
	auto oldRmw = module.getRegisteredRE("rmw")->clone();
	auto newRmw = SeqRE::createOpt(module.getRegisteredRE("UR")->clone(),
				       module.getRegisteredRE("po-imm")->clone(),
				       module.getRegisteredRE("UW")->clone());
	module.replaceAllUsesWith(&*oldPo, &*newPo);
	module.replaceAllUsesWith(&*oldPoloc, &*newPoloc);
	module.replaceAllUsesWith(&*oldMo, &*newMo);
	module.replaceAllUsesWith(&*oldFr, &*newFr);
	module.replaceAllUsesWith(&*oldRmw, &*newRmw);

	/* Try and de-transitivize transitive primitives */
	for (auto &let : module.lets()) {
		auto *mutRecRE = dynamic_cast<MutRecRE *>(let->getRE());
		auto *viewExp = dynamic_cast<ViewExp *>(let->getSaved());
		if (!mutRecRE || !viewExp) {
			continue;
		}

		/* Helper function to check if a relation is closed w.r.t. some builtin */
		auto isRecREClosedWRTBuiltin = [&](auto &builtinRE) {
			auto recBuiltin = SeqRE::createOpt(mutRecRE->clone(), builtinRE->clone());
			auto recBuiltinInRec = SubsetConstraint::create(
				std::move(recBuiltin), mutRecRE->clone(), false, false);
			return checkAssertion(*recBuiltinInRec).result;
		};

		/* Get the respective recursive def */
		std::vector<std::pair<std::unique_ptr<RegExp>, std::unique_ptr<RegExp>>> toRepl;
		for (auto &builtinRE :
		     collectTrailingTransBuiltins(mutRecRE->getRelation(), viewExp->getRERef()) |
			     std::views::filter(isRecREClosedWRTBuiltin)) {
			std::cerr << "[Warning] Replacing " << *builtinRE
				  << "+ with its immediate counterpart in " << let->getName()
				  << " = " << *viewExp->getRE() << "\n";
			auto newRE = viewExp->getRE()->clone();
			auto toReplaceRE = PlusRE::create(builtinRE->clone());
			foreachTrailingTransBuiltin(newRE, [&](std::unique_ptr<RegExp> &builtin) {
				replaceREWith(builtin, &*toReplaceRE, &*builtinRE);
			});
			std::cerr << "Result: " << *newRE << "\n";

			toRepl.emplace_back(viewExp->getRE()->clone(), std::move(newRE));
		}

		for (auto &[from, to] : toRepl)
			module.replaceAllUsesWith(&*from, &*to);
	}
}

auto Kater::exportCode() -> bool
{
	if (!checkExportRequirements())
		return false;

	optimizeModuleForExport();

	GenMCPrinter p(getModule(), getConf());
	p.output();
	return true;
}
