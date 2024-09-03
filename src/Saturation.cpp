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

#include "Saturation.hpp"

#include "NFAUtils.hpp"
#include "Theory.hpp"

#include <ranges>

namespace RV = std::views;

/*
 * Duplicate the automaton for each of the initial state(s)' different
 * outgoing predicate transitions. By doing so, we can later restrict
 * incoming transitions to final states.
 */
void duplicateStatesForInitPreds(NFA &nfa)
{
	/* Ensure accepting states have no outgoing transitions */
	nfa.flip();
	for (auto *s : nfa.starting() | RV::filter([](auto *s) { return s->hasIncoming(); })) {
		nfa.splitState(s, [](auto & /*t*/) { return false; });
	}
	nfa.flip();

	std::set<PredicateSet> preds;
	for (auto *s : nfa.starting()) {
		for (auto &t :
		     s->outs() | RV::filter([](auto &t) { return t.label.isPredicate(); })) {
			preds.insert(t.label.getPreChecks());
		}
	}

	if (preds.size() <= 1)
		return;

	NFA result;
	for (const auto &p : preds) {
		std::unordered_map<NFA::State *, NFA::State *> m;
		auto nfac = copy(nfa, &m);

		for (auto *s : nfa.starting()) {
			nfac.removeTransitionsIf(
				m[s], [&](auto &t) { return t.label.getPreChecks() != p; });
			nfa.removeTransitionsIf(
				s, [&](auto &t) { return t.label.getPreChecks() == p; });
		}
		if (result.getNumStates() == 0) {
			result = std::move(nfac);
		} else {
			result.alt(std::move(nfac));
		}
	}

	nfa.alt(std::move(result));
	removeDeadStates(nfa);
}

void saturateInitFinalPreds(NFA &nfa, const Theory &theory)
{
	duplicateStatesForInitPreds(nfa);

	/* Collect predicates */
	std::vector<NFA::Transition> ipreds;
	std::for_each(nfa.start_begin(), nfa.start_end(), [&](auto *s) {
		std::copy_if(s->out_begin(), s->out_end(), std::back_inserter(ipreds),
			     [&](auto &t) { return t.label.isPredicate(); });
	});

	if (ipreds.empty()) {
		return;
	}
	std::for_each(nfa.accept_begin(), nfa.accept_end(), [&](auto *s) {
		std::vector<NFA::Transition> toAdd;
		std::vector<NFA::Transition> toRemove;
		for (auto tIt = s->in_begin(), tE = s->in_end(); tIt != tE; ++tIt) {
			if (!tIt->label.isPredicate()) {
				continue;
			}

			toRemove.push_back(*tIt);
			auto reaching = calculateReachingTo(nfa, {tIt->dest});
			std::for_each(ipreds.begin(), ipreds.end(), [&](auto &ip) {
				auto t = *tIt;
				if (reaching.contains(ip.dest) &&
				    theory.composes(t.label, ip.label)) {
					t.label.merge(ip.label);
					toAdd.push_back(t);
				}
			});
		}
		nfa.removeInvertedTransitions(s, toRemove.begin(), toRemove.end());
		nfa.addInvertedTransitions(s, toAdd.begin(), toAdd.end());
	});
	removeDeadStates(nfa);
}

void saturateLoc(NFA &nfa, const Theory &theory)
{
	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s) {
		std::vector<NFA::Transition> toAdd;
		std::vector<NFA::Transition> toRemove;
		for (auto it = s->out_begin(); it != s->out_end(); ++it) {
			auto &t = *it;
			if (!theory.hasPerLoc(t.label)) {
				continue;
			}

			auto perloc = true;
			nfa.foreachPathReachableFrom(
				{t.dest}, [&](std::pair<NFA::State *, NFA::Transition> p) {
					perloc &= (p.second.label.isPredicate() ||
						   theory.isEco(p.second.label));
				});
			nfa.foreachPathReachingTo({&*s},
						  [&](std::pair<NFA::State *, NFA::Transition> p) {
							  perloc &= (p.second.label.isPredicate() ||
								     theory.isEco(p.second.label));
						  });
			if (!perloc) {
				continue;
			}
			toRemove.push_back(t);
			toAdd.push_back(
				NFA::Transition(TransLabel(theory.getPerLoc(t.label)), t.dest));
		}
		nfa.removeTransitions(&*s, toRemove.begin(), toRemove.end());
		nfa.addTransitions(&*s, toAdd.begin(), toAdd.end());
	});
}

void saturateID(NFA &nfa, NFA &&id)
{
	std::vector<NFA::State *> inits(id.start_begin(), id.start_end());
	std::vector<NFA::State *> fnals(id.accept_begin(), id.accept_end());

	id.clearAllStarting();
	id.clearAllAccepting();

	std::vector<NFA::State *> states;
	std::for_each(nfa.states_begin(), nfa.states_end(),
		      [&](auto &s) { states.push_back(&*s); });

	std::for_each(states.begin(), states.end(), [&](auto *s) {
		std::unordered_map<NFA::State *, NFA::State *> m;
		auto c = copy(id, &m);

		nfa.alt(std::move(c));
		std::for_each(inits.begin(), inits.end(),
			      [&](auto *i) { nfa.addEpsilonTransitionSucc(s, m[i]); });
		std::for_each(fnals.begin(), fnals.end(),
			      [&](auto *f) { nfa.addEpsilonTransitionSucc(m[f], s); });
	});
}

void saturateTransitive(NFA &nfa, const Relation &rel)
{
	TransLabel lab(rel);
	std::vector<NFA::State *> toDuplicate;

	lab.flip();
	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s) {
		if (std::any_of(s->in_begin(), s->in_end(),
				[&](auto &t) { return t.label == lab; })) {
			toDuplicate.push_back(&*s);
		}
	});

	std::for_each(toDuplicate.begin(), toDuplicate.end(), [&](auto *s) {
		if (std::any_of(s->in_begin(), s->in_end(),
				[&](auto &t) { return t.label != lab; })) {
			nfa.splitState(s, [&](auto &t) { return t.label == lab; });
		}
		NFA::addSelfTransition(s, TransLabel(rel));
	});
}

/*
 * Adds a path corresponding to PATH from SRC to DST in NFA,
 * by copying the PATH nfa.  Initial/final states in the copy
 * are discarded.
 */
void addNFAPath(NFA &nfa, const NFA &path, NFA::State *src, NFA::State *dst)
{
	std::unordered_map<NFA::State *, NFA::State *> m;
	VSet<NFA::State *> inits(path.start_begin(), path.start_end());
	VSet<NFA::State *> finals(path.accept_begin(), path.accept_end());

	auto lcopy = copy(path, &m);

	lcopy.clearAllAccepting();
	lcopy.clearAllStarting();

	nfa.alt(std::move(lcopy));
	for (auto *i : inits)
		nfa.addEpsilonTransition(src, m[i]);
	for (auto *f : finals)
		nfa.addEpsilonTransition(m[f], dst);
}

void saturateBuiltin(NFA &nfa, const Relation &rel, NFA sat, const Theory &theory)
{
	TransLabel lab(rel);

	/* Collect all paths but don't modify in place */
	std::vector<std::pair<NFA::State *, NFA::State *>> paths;
	auto relInTrans = [&](auto &t) { return theory.isIncludedIn(lab, t.label); };
	for (auto &s : nfa.states()) {
		for (auto &t : s->outs() | RV::filter(relInTrans))
			paths.emplace_back(&*s, t.dest);
	}

	for (auto &p : paths)
		addNFAPath(nfa, sat, p.first, p.second);
}

auto collectLabels(const NFA &nfa) -> std::vector<TransLabel>
{
	std::vector<TransLabel> labels;
	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s) {
		std::for_each(s->out_begin(), s->out_end(),
			      [&](auto &t) { labels.push_back(t.label); });
	});
	return labels;
}

void saturateEmpty(NFA &start, NFA &&empty)
{
	std::for_each(empty.start_begin(), empty.start_end(), [&](auto &s) {
		NFA::addSelfTransition(
			s, TransLabel(Relation::createBuiltin(Relation::BuiltinID::any)));
	});
	std::for_each(empty.accept_begin(), empty.accept_end(), [&](auto &a) {
		NFA::addSelfTransition(
			a, TransLabel(Relation::createBuiltin(Relation::BuiltinID::any)));
	});

	start.alt(std::move(empty));
}

void saturateTotal(NFA &nfa, const Relation &rel)
{
	TransLabel lab(rel);
	std::vector<NFA::State *> toDuplicate;

	lab.flip();
	for (auto it = nfa.states_begin(); it != nfa.states_end(); ++it) {
		auto *s = it->get();
		if (std::any_of(s->in_begin(), s->in_end(),
				[&](auto &t) { return t.label == lab; })) {
			toDuplicate.push_back(s);
		}
	}

	std::for_each(toDuplicate.begin(), toDuplicate.end(), [&](auto *s) {
		if (std::any_of(s->in_begin(), s->in_end(),
				[&](auto &t) { return t.label != lab; })) {
			nfa.splitState(s, [&](auto &t) { return t.label == lab; });
		}
		std::vector<NFA::Transition> ins(s->in_begin(), s->in_end());
		std::for_each(ins.begin(), ins.end(), [&](auto &t) {
			assert(t.label == lab);
			nfa.addEpsilonTransitionPred(s, t.dest); /* rel+ */
			nfa.addEpsilonTransitionSucc(t.dest, s); /* rel? */
		});
	});
}

void saturateDomains(NFA &nfa, const Theory &theory)
{
	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s) {
		/* toAdd: temp workaround due to std::set exposing const_iters only */
		std::vector<NFA::Transition> toAdd;
		std::vector<NFA::Transition> toRemove;
		for (auto it = s->out_begin(), ie = s->out_end(); it != ie; ++it) {
			auto lab = it->label;
			if (lab.isPredicate())
				continue;

			if (theory.composes(lab.getPreChecks(),
					    {theory.getDomain(*lab.getRelation())}) &&
			    theory.composes(lab.getPostChecks(),
					    {theory.getCodomain(*lab.getRelation())})) {
				lab.getPreChecks().insert(theory.getDomain(*lab.getRelation()));
				lab.getPostChecks().insert(theory.getCodomain(*lab.getRelation()));
				toAdd.push_back(NFA::Transition(lab, it->dest));
			}
			toRemove.push_back(*it);
		}
		nfa.removeTransitions(&*s, toRemove.begin(), toRemove.end());
		nfa.addTransitions(&*s, toAdd.begin(), toAdd.end());
	});
}

void saturateRotate(NFA &nfa, const Theory &theory)
{
	void simplify(NFA & nfa, const Theory &theory);
	NFA result;

	simplify(nfa, theory);
	auto isNonStarting = [](auto &s) { return !s->isStarting(); };
	for (auto &s : nfa.states() | std::views::filter(isNonStarting)) {

		std::unordered_map<NFA::State *, NFA::State *> m1;
		std::unordered_map<NFA::State *, NFA::State *> m2;

		auto rot1 = copy(nfa, &m1);
		auto rot2 = copy(nfa, &m2);

		rot1.clearAllStarting();
		rot1.makeStarting(m1[&*s]);

		rot2.clearAllAccepting();
		rot2.makeAccepting(m2[&*s]);

		rot1.seq(std::move(rot2));
		result.alt(std::move(rot1));
	}
	nfa.alt(std::move(result));
	removeDeadStates(nfa);
}
