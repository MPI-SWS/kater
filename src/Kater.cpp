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

#include "Config.hpp"
#include "NFAUtils.hpp"
#include "Printer.hpp"
#include "Saturation.hpp"
#include "Utils.hpp"
#include "Visitor.hpp"

#include <deque>
#include <execution>
#include <numeric>
#include <optional>
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

template <typename T> inline void hash_combine(std::size_t &seed, std::size_t v)
{
	std::hash<T> hasher;
	seed ^= hasher(v) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
}

/*************************************************************
 *              Regexp utilities
 ************************************************************/

void expandSavedVars(URE &r, const KatModule &module)
{
	for (int i = 0; i < r->getNumKids(); i++) {
		expandSavedVars(r->getKid(i), module);
	}
	if (auto *re = dynamic_cast<CharRE *>(&*r)) {
		if (re->getLabel().isBuiltin() || !module.isSavedID(re)) {
			return;
		}
		r = module.getSavedID(re);
		expandSavedVars(r, module);
	}
}

void expandRfs(URE &r, const KatModule &module)
{
	for (int i = 0; i < r->getNumKids(); i++) {
		expandRfs(r->getKid(i), module);
	}

	auto rf = module.getRegisteredID("rf");
	const auto *re = dynamic_cast<const CharRE *>(&*r);
	if ((re == nullptr) ||
	    re->getLabel().getRelation() !=
		    dynamic_cast<const CharRE *>(&*rf)->getLabel().getRelation()) {
		return;
	}

	auto rfe = module.getRegisteredID("rfe");
	auto rfi = module.getRegisteredID("rfi");

	auto re1 = re->clone();
	auto re2 = re->clone();
	auto l1 = TransLabel(dynamic_cast<CharRE *>(&*rfe)->getLabel().getRelation(),
			     re->getLabel().getPreChecks(), re->getLabel().getPostChecks());
	auto l2 = TransLabel(dynamic_cast<CharRE *>(&*rfi)->getLabel().getRelation(),
			     re->getLabel().getPreChecks(), re->getLabel().getPostChecks());
	dynamic_cast<CharRE *>(&*re1)->setLabel(l1);
	dynamic_cast<CharRE *>(&*re2)->setLabel(l2);

	r = AltRE::createOpt(std::move(re1), std::move(re2));
}

void transitivizeSaved(URE &r, const KatModule &module)
{
	for (int i = 0; i < r->getNumKids(); i++) {
		transitivizeSaved(r->getKid(i), module);
	}
	if (auto *re = dynamic_cast<CharRE *>(&*r)) {
		if (re->getLabel().isBuiltin()) {
			return;
		}
		auto sIt = std::find_if(module.svar_begin(), module.svar_end(), [&](auto &p) {
			return p.first == *re->getLabel().getRelation();
		});
		if (sIt == module.svar_end() || sIt->second.status == VarStatus::Normal) {
			return;
		}
		r = PlusRE::createOpt(re->clone());
	}
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

void simplify(NFA &nfa, const Theory &theory)
{
	KATER_DEBUG(std::cout << "Before simplification: " << nfa;);

	/* Don't bother with empty automata (dead state removal might add no-op states) */
	if (nfa.getNumStates() == 0)
		return;

	compactEdges(nfa, theory);
	removeDeadStates(nfa);
	removeSimilarTransitions(nfa);
	removeDeadStates(nfa);
	scmReduce(nfa);
	compactEdges(nfa, theory);
	scmReduce(nfa);
	removeDeadStates(nfa);
	removeSimilarTransitions(nfa);
	removeDeadStates(nfa);

	KATER_DEBUG(std::cout << "After simplification: " << nfa;);
}

void normalize(NFA &nfa, const Theory &theory)
{
	simplify(nfa, theory);
	saturateDomains(nfa, theory);
	breakToParts(nfa);
	removeConsecutivePredicates(nfa, theory);
	removeDeadStates(nfa);
}

void reduce(NFA &nfa, ReductionType /*t*/, const Theory &theory)
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

std::pair<bool, CompSchemeRHS> isSupportedCompScheme(const SubsetConstraint *assm, URE poRE)
{
	auto [lok, lhs] = extractCompLHSInfo(assm->getLHS(), &*poRE);
	auto [rok, rhs] = extractCompRHSInfo(assm->getRHS(), &*poRE);

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
	auto po_imm =
		CharRE::create(TransLabel(Relation::createBuiltin(Relation::BuiltinID::po_imm)));
	PredicateSet pre = rhs.pre.has_value() ? *rhs.pre : PredicateSet{};
	PredicateSet post = rhs.post.has_value() ? *rhs.post : PredicateSet{};

	auto reC = SeqRE::createOpt(
		CharRE::create(TransLabel(std::nullopt, pre)),
		StarRE::createOpt(SeqRE::createOpt(po_imm->clone(), std::move(fenceExp))),
		po_imm->clone(), CharRE::create(TransLabel(std::nullopt, post)));

	auto lNFA = reC->toNFA(theory);
	normalize(lNFA, theory);
	return lNFA;
}

void expandSubsetAssumption(NFA &rhs, const SubsetConstraint *assm, const KatModule &module)
{
	auto &theory = module.getTheory();
	auto *lRE = assm->getLHS();
	auto *rRE = assm->getRHS();

	auto lNFA = lRE->toNFA(theory);
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
	if (auto *seqRE = dynamic_cast<const SeqRE *>(lRE)) {
		if (*rRE == *seqRE->getKid(0) && *rRE == *seqRE->getKid(1)) {
			saturateTransitive(
				rhs, *dynamic_cast<const CharRE *>(rRE)->getLabel().getRelation());
			return;
		}
	}
	/* Transform `A+ <= A` to `transitive A` */
	if (auto *plusRE = dynamic_cast<const PlusRE *>(&*lRE)) {
		if (*rRE == *plusRE->getKid(0)) {
			saturateTransitive(
				rhs, *dynamic_cast<const CharRE *>(rRE)->getLabel().getRelation());
			return;
		}
	}
	// FIXME: Also discard A <= A{*,+}
	/* Discard `A <= A?` assumption */
	if (auto *charRE = dynamic_cast<const CharRE *>(&*lRE)) {
		if (auto *qmarkRE = dynamic_cast<const QMarkRE *>(&*rRE)) {
			if (*qmarkRE->getKid(0) == *charRE)
				return;
		}
	}
	/* Handle `A <= builtin` assumption */
	if (auto *charRE = dynamic_cast<const CharRE *>(&*rRE)) {
		saturateBuiltin(rhs, *dynamic_cast<const CharRE *>(rRE)->getLabel().getRelation(),
				std::move(lNFA), theory);
		return;
	}
	/* Handle `[A];po;[B] <= [A];po;[C];po;[B]` assumption */
	auto [supported, compRHS] = isSupportedCompScheme(assm, module.getRegisteredID("po"));
	if (supported) {
		saturateEmpty(rhs, createCompilationNFA(compRHS, theory));
		return;
	}

	std::cerr << "[Warning] Ignoring unsupported assumption " << *assm << "\n";
	return;
}

void expandAssumption(NFA &rhs, const Constraint *assm, const KatModule &module)
{
	assert(assm);

	if (const auto *cc = dynamic_cast<const EqualityConstraint *>(assm)) {
		expandSubsetAssumption(rhs, cc, module);
		expandSubsetAssumption(rhs, cc, module);
		return;
	}
	if (const auto *tc = dynamic_cast<const TotalityConstraint *>(assm)) {
		saturateTotal(
			rhs,
			*dynamic_cast<const CharRE *>(tc->getRelation())->getLabel().getRelation());
		return;
	}
	// FIXME: What about SubsetSameEnds???
	if (const auto *ec = dynamic_cast<const SubsetConstraint *>(assm)) {
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

auto Kater::checkInclusion(const SubsetConstraint &subsetC) const -> InclusionResult
{
	auto &module = getModule();
	auto &theory = module.getTheory();

	auto nfa1 = subsetC.getLHS()->toNFA(theory);
	normalize(nfa1, theory);
	removeSimilarTransitions(nfa1);
	if (subsetC.sameEnds()) {
		saturateInitFinalPreds(nfa1, theory);
		saturateLoc(nfa1, theory);
	}
	auto lhs = nfa1.to_DFA().first;

	auto nfa2 = subsetC.getRHS()->toNFA(theory);
	normalize(nfa2, theory);
	for (const auto &assm : theory.assumes()) {
		expandAssumption(nfa2, &*assm, module);
		normalize(nfa2, theory);
	}
	pruneNFA(nfa2, lhs, theory);
	removeDeadStates(nfa2);
	normalize(nfa2, theory);
	return isDFASubLanguageOfNFA(lhs, nfa2);
}

auto Kater::checkAssertion(Constraint *c) -> InclusionResult
{
	if (getConf().verbose >= 2) {
		std::cout << "Checking assertion " << *c << std::endl;
	}

	for (int i = 0; i < c->getNumKids(); i++) {
		expandSavedVars(c->getKid(i), getModule());
		expandRfs(c->getKid(i), getModule());
	}

	InclusionResult result;
	auto visitor = make_visitor(
		type_list<SubsetConstraint, EqualityConstraint>{},
		[&](const SubsetConstraint &sc) { result = checkInclusion(sc); },
		[&](const EqualityConstraint &ec) {
			result = checkInclusion(ec);
			if (result.result) {
				auto invC = SubsetConstraint::create(
					ec.getLHS()->clone(), ec.getRHS()->clone(), ec.sameEnds());
				result = checkInclusion(*invC);
			}
		});
	visitor(*c);
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

	/* Helper function to statespatch checkers as the different policies do not share a type...
	 */
	auto maybe_parallelize = [&]<typename F>(F f) {
		return getConf().parallel ? f(std::execution::par_unseq) : f(std::execution::seq);
	};

	/* Go ahead and check */
	std::vector<std::pair<KatModule::DbgInfo, Counterexample>> errors;
	auto status = true;
	maybe_parallelize([&](auto &policy) {
		std::for_each(policy, module->assert_begin(), module->assert_end(),
			      [&](const auto &assert) {
				      auto [ok, cex] = checkAssertion(&*assert.co);
				      if (!ok) {
					      status = false;
					      errors.push_back({assert.loc, cex});
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
 *              DInclusion checking
 ************************************************************/

auto Kater::checkExportRequirements() -> bool
{
	auto &module = getModule();

	/* Ensure that pporf is implied by the acyclicity constraints */
	auto ppo = module.getPPO();
	if (!ppo) {
		std::cerr << "[Error] No top-level ppo definition provided\n";
		return false;
	}
	auto hb = module.getHB();
	if (!hb) {
		std::cerr << "[Error] No top-level hb_stable definition provided\n";
		return false;
	}
	auto *hbRE = dynamic_cast<CharRE *>(&*hb);
	if ((hbRE == nullptr) || !hbRE->getLabel().getRelation()) {
		std::cerr << "[Error] hb_stable needs to be stored in a view\n";
		return false;
	}
	auto hbIt = std::find_if(module.svar_begin(), module.svar_end(), [&](auto &kv) {
		return kv.first == *hbRE->getLabel().getRelation() &&
		       kv.second.status == VarStatus::View;
	});
	if (hbIt == module.svar_end()) {
		std::cerr << "[Error] hb_stable needs to be stored in a view\n";
		return false;
	}

	auto pporf = module.getPPORF();
	auto acycDisj = std::accumulate(module.acyc_begin(), module.acyc_end(),
					RegExp::createFalse(), [&](auto re1, auto &co2) {
						return AltRE::createOpt(re1->clone(),
									co2->getKid(0)->clone());
					});
	auto noOOTA = SubsetConstraint::create(pporf->clone(),
					       StarRE::createOpt(std::move(acycDisj)), false);
	auto [status, cex] = checkAssertion(&*noOOTA);
	if (!status) {
		std::cerr << "[Warning] Acyclicity constraints do not preclude OOTA.\n";
		std::cerr << "OOTA needs to be enforced by the model checker\n";
		printCounterexample(cex);
		status = true; /* treat as a soft error */
	}

	/* Check correctness of "unless" clauses in acyclicity constraints */
	for (auto &acyc : module.acycs()) {
		auto *ac = dynamic_cast<AcyclicConstraint *>(&*acyc);
		if (!ac->getConstraint())
			continue;

		/* A: all acyclicity constraints */
		auto acycDisj = std::accumulate(
			module.acyc_begin(), module.acyc_end(), RegExp::createFalse(),
			[&](auto re1, auto &co2) {
				return AltRE::createOpt(re1->clone(), co2->getKid(0)->clone());
			});
		/* A_R: acyclicity constraints w/o the current one */
		auto acycDisjRest = std::accumulate(
			module.acyc_begin(), module.acyc_end(), RegExp::createFalse(),
			[&](auto re1, auto &co2) {
				return *co2->getKid(0) == *acyc->getKid(0)
					       ? std::move(re1)
					       : AltRE::createOpt(re1->clone(),
								  co2->getKid(0)->clone());
			});

		/* it must be: A_R => A (assuming the unless holds) */
		module.getTheory().registerTempAssume(ac->getConstraint()->clone());
		auto unlessOK = SubsetConstraint::create(std::move(acycDisj),
							 std::move(acycDisjRest), false);
		auto [status, cex] = checkAssertion(&*unlessOK);
		if (!status) {
			std::cerr << "[Error] \"unless\" clause does not imply acyclicity "
				     "constraint: "
				  << *ac << "\n";
			printCounterexample(cex);
			status = false;
		}
		module.getTheory().clearTempAssumes();
	}

	/* Ensure that all saved relations are included in pporf;ppo and are transitive */
	std::for_each(module.svar_begin(), module.svar_end(), [&](auto &kv) {
		auto &sv = kv.second;
		Counterexample cex;
		if (sv.status != VarStatus::View) {
			auto savedInPO = SubsetConstraint::create(
				sv.exp->clone(),
				StarRE::createOpt(SeqRE::createOpt(
					StarRE::createOpt(pporf->clone()), ppo->clone())),
				false);
			auto result = checkAssertion(&*savedInPO);
			if (!result.result) {
				std::cerr << "[Error] Saved relation not included in pporf;ppo: "
					  << *sv.exp << "\n";
				printCounterexample(result.cex);
				status = false;
			}
		} else {
			auto savedInPO = SubsetConstraint::create(
				sv.exp->clone(),
				StarRE::createOpt(SeqRE::createOpt(
					StarRE::createOpt(module.getPORF()->clone()),
					module.getRegisteredID("po"))),
				false);
			auto result = checkAssertion(&*savedInPO);
			if (!result.result) {
				std::cerr << "[Error] View not included in porf;po: " << *sv.exp
					  << "\n";
				printCounterexample(result.cex);
				status = false;
			}
		}

		if (sv.status != VarStatus::Normal) {
			cex.clear();
			auto prefix = (sv.status == VarStatus::Reduce) ? sv.red->clone()
								       : ppo->clone();
			auto seqExp = SeqRE::createOpt(std::move(prefix), sv.exp->clone());
			auto savedTrans = SubsetConstraint::createOpt(std::move(seqExp),
								      sv.exp->clone(), false);
			auto result = checkAssertion(&*savedTrans);
			if (!result.result) {
				std::cerr << "[Error] Reduced relation not transitive: " << *sv.exp
					  << "\n";
				printCounterexample(result.cex);
				status = false;
			}
		}
	});

	/* Ensure that only one coherence constraint has been given, and that it involves a view */
	if (module.getCohNum() != 1) {
		std::cerr << "[Error] Only one coherence constraint is supported\n";
		return false;
	}
	auto *coh = dynamic_cast<CharRE *>(&**module.coh_begin());
	if ((coh == nullptr) || !coh->getLabel().getRelation()) {
		std::cerr << "[Error] Coherence constraint needs to take a view argument\n";
		return false; /* skip the rest of the checks */
	}
	auto vIt = std::find_if(module.svar_begin(), module.svar_end(), [&](auto &kv) {
		return kv.first == *coh->getLabel().getRelation() &&
		       kv.second.status == VarStatus::View;
	});
	if (vIt == module.svar_end()) {
		std::cerr << "[Error] Coherence constraint needs to take a view argument\n";
		status = false;
	}
	return status;
}

void Kater::generateNFAs()
{
	auto &module = getModule();
	auto &theory = module.getTheory();
	auto &cnfas = getCNFAs();
	auto ppo = module.getPPO();

	auto i = 0U;
	std::for_each(module.svar_begin(), module.svar_end(), [&](auto &kv) {
		auto &v = kv.second;
		NFA n = v.exp->toNFA(theory);

		// FIXME: Use polymorphism
		if (v.status == VarStatus::Reduce) {
			if (getConf().verbose >= 3) {
				std::cout << "Generating NFA for reduce[" << i << "] = " << *v.exp
					  << std::endl;
			}

			auto *pRE = dynamic_cast<PlusRE *>(&*v.exp);
			assert(pRE); // FIXME: currently we implicitly assume this (see
				     // transitivize); maybe tip?
			assert(v.red);

			n = pRE->getKid(0)->toNFA(theory);

			simplify(n, theory);
			reduce(n, v.redT, theory);

			NFA rn = v.red->toNFA(theory);
			rn.star();
			simplify(rn, theory);
			rn.seq(std::move(n));
			simplify(rn, theory);

			if (getConf().verbose >= 3) {
				std::cout << "Generated NFA for reduce[" << i << "]: " << rn
					  << std::endl;
			}

			cnfas.addReduced(std::move(rn));
		} else if (v.status == VarStatus::View) {
			if (getConf().verbose >= 3) {
				std::cout << "Generating NFA for view[" << i << "] = " << *v.exp
					  << std::endl;
			}

			auto *pRE = dynamic_cast<PlusRE *>(&*v.exp);
			assert(pRE); // FIXME: currently we implicitly assume this (see
				     // transitivize); maybe tip?

			n = pRE->getKid(0)->toNFA(theory);
			simplify(n, theory);

			if (getConf().verbose >= 3) {
				std::cout << "Generated NFA for view[" << i << "]: " << n
					  << std::endl;
			}
			cnfas.addView(std::move(n));

			if (kv.first == dynamic_cast<CharRE *>(&**module.coh_begin())
						->getLabel()
						.getRelation()) {
				cnfas.setCohIndex(i);
			}
			if (kv.first ==
			    dynamic_cast<CharRE *>(&*module.getHB())->getLabel().getRelation()) {
				cnfas.setHbIndex(i);
			}
		} else {
			if (getConf().verbose >= 3) {
				std::cout << "Generating NFA for save[" << i << "] = " << *v.exp
					  << std::endl;
			}
			simplify(n, theory);
			if (getConf().verbose >= 3) {
				std::cout << "Generated NFA for save[" << i << "]: " << n
					  << std::endl;
			}
			cnfas.addSaved(std::move(n));
		}
		++i;
	});

	for (const auto &c : module.acycs()) {
		auto &r = c->getKid(0);
		if (getConf().verbose >= 3) {
			std::cout << "Generating NFA for acyclic " << *r << std::endl;
		}
		// Covert the regural expression to an NFA
		auto tr = r->clone();
		transitivizeSaved(tr, module);
		NFA n = tr->toNFA(theory);
		// Take the reflexive-transitive closure, which typically helps minizing the NFA.
		// Doing so is alright because the generated DFS code discounts empty paths anyway.
		n.star();
		if (getConf().verbose >= 4) {
			std::cout << "Non-simplified NFA: " << n << std::endl;
		}
		// Simplify the NFA
		simplify(n, theory);
		if (getConf().verbose >= 3) {
			std::cout << "Generated acyclic NFA: " << n
				  << "\nAcyclic size: " << n.size() << "\n";
		}

		auto u = dynamic_cast<const AcyclicConstraint *>(&*c)->getConstraint();
		if (!u || !u->isEmpty()) {
			if (u) {
				std::cerr << "[Warning] Ignoring non-emptiness constraint used for "
					     "unless clause: "
					  << *u << "\n";
			}
			cnfas.addAcyclic(std::move(n));
			continue;
		}

		auto tu = u->getKid(0)->clone();
		transitivizeSaved(tu, module);
		NFA m = tu->toNFA(theory);
		// m.star();
		simplify(m, theory);

		cnfas.addAcyclic(std::move(n),
				 dynamic_cast<const AcyclicConstraint *>(&*c)->getPrintInfo(),
				 {std::move(m)});
	}

	std::for_each(module.incl_begin(), module.incl_end(), [&](auto &ic) {
		const Constraint *c = nullptr; // FIXME: refactor
		if (auto *ec = dynamic_cast<const ErrorConstraint *>(&*ic)) {
			c = ec->getConstraint();
		} else if (auto *wc = dynamic_cast<const WarningConstraint *>(&*ic)) {
			c = wc->getConstraint();
		} else {
			assert(0);
		}

		if (getConf().verbose >= 3) {
			std::cout << "Generating NFA for inclusion " << *c->getKid(0)
				  << " <= " << *c->getKid(1) << std::endl;
		}

		// Covert the regural expression to an NFA
		auto tl = c->getKid(0)->clone();
		transitivizeSaved(tl, module);
		auto lhs = tl->toNFA(theory);
		// Take the reflexive-transitive closure, which typically helps minizing the NFA.
		// Doing so is alright because the generated DFS code discounts empty paths anyway.
		// lhs.star();
		if (getConf().verbose >= 4) {
			std::cout << "Non-simplified NFA (LHS): " << lhs << std::endl;
		}
		// Simplify the NFA
		simplify(lhs, theory);
		if (getConf().verbose >= 3) {
			std::cout << "Generated NFA (LHS): " << lhs << std::endl;
		}

		// Covert the regural expression to an NFA
		auto tr = c->getKid(1)->clone();
		transitivizeSaved(tr, module);
		auto rhs = tr->toNFA(theory);
		// Take the reflexive-transitive closure, which typically helps minizing the NFA.
		// Doing so is alright because the generated DFS code discounts empty paths anyway.
		// rhs.star();
		if (getConf().verbose >= 4) {
			std::cout << "Non-simplified NFA (RHS): " << rhs << std::endl;
		}
		// Simplify the NFA
		simplify(rhs, theory);
		if (getConf().verbose >= 3) {
			std::cout << "Generated NFA (RHS): " << rhs << std::endl;
		}

		auto j = -1;
		auto *rhsRE = dynamic_cast<const CharRE *>(c->getKid(1));
		if (rhsRE && rhsRE->getLabel().getRelation()) {
			for (auto sIt = module.svar_begin(), sE = module.svar_end(); sIt != sE;
			     ++sIt) {
				if (sIt->second.status == VarStatus::View &&
				    sIt->first == rhsRE->getLabel().getRelation()) {
					j = std::distance(module.svar_begin(), sIt);
				}
			}
		}

		cnfas.addInclusion(
			{Inclusion<NFA>(
				 std::move(lhs), std::move(rhs),
				 dynamic_cast<const ErrorConstraint *>(&*ic)
					 ? Constraint::Type::Error
					 : Constraint::Type::Warning,
				 dynamic_cast<const ErrorConstraint *>(&*ic)
					 ? dynamic_cast<const ErrorConstraint *>(&*ic)->getName()
					 : dynamic_cast<const WarningConstraint *>(&*ic)
						   ->getName()),
			 j});
	});

	NFA rec;
	std::for_each(module.rec_begin(), module.rec_end(), [&](auto &r) {
		if (getConf().verbose >= 3) {
			std::cout << "Generating NFA for recovery " << *r << std::endl;
		}
		// Covert the regural expression to an NFA
		auto tr = r->clone();
		transitivizeSaved(tr, module);
		NFA n = tr->toNFA(theory);
		// Take the reflexive-transitive closure, which typically helps minizing the NFA.
		// Doing so is alright because the generated DFS code discounts empty paths anyway.
		if (getConf().verbose >= 4) {
			std::cout << "Non-star, non-simplified rec NFA: " << n << std::endl;
		}
		n.star();
		if (getConf().verbose >= 4) {
			std::cout << "Non-simplified rec NFA: " << n << std::endl;
		}
		// Simplify the NFA
		simplify(n, theory);
		if (getConf().verbose >= 3) {
			std::cout << "Generated rec NFA: " << n << std::endl;
		}
		rec.alt(std::move(n));
	});
	if (module.getRecoveryNum() != 0u) {
		auto rf = module.getRegisteredID("rf");
		auto recov = module.getRegisteredID("REC");
		auto po = module.getRegisteredID("po");
		auto fr = module.getRegisteredID("fr");
		auto poInv = module.getRegisteredID("po");
		poInv->flip();

		auto rfRecovPoFr =
			SeqRE::createOpt(rf->clone(), recov->clone(), po->clone(), fr->clone());

		auto rfRecovPoInvFr =
			SeqRE::createOpt(rf->clone(), recov->clone(), poInv->clone(), fr->clone());

		rec.alt(rfRecovPoFr->toNFA(theory));
		rec.alt(rfRecovPoInvFr->toNFA(theory));

		rec.star();

		if (getConf().verbose >= 3) {
			std::cout << "Generated full rec NFA: " << rec << std::endl;
		}

		simplify(rec, theory);

		cnfas.addRecovery(std::move(rec));
		if (getConf().verbose >= 3) {
			std::cout << "Generated full rec NFA simplified: " << cnfas.getRecovery()
				  << std::endl;
		}
	}

	auto pporfNFA = module.getPPORF()->toNFA(theory);
	simplify(pporfNFA, theory);
	cnfas.addPPoRf(std::move(pporfNFA), module.isDepTracking());
	if (getConf().verbose >= 3) {
		std::cout << "Generated pporf NFA simplified: " << cnfas.getPPoRf().first
			  << std::endl;
	}

	cnfas.setDepTracking(module.isDepTracking());
}

auto Kater::exportCode(std::string &dirPrefix, std::string &outPrefix) -> bool
{
	if (!checkExportRequirements())
		return false;

	generateNFAs();

	Printer p(getModule(), dirPrefix, outPrefix);
	p.output(getCNFAs());
	return true;
}
