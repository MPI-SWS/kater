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

#include "NFA.hpp"
#include "Config.hpp"
#include "Error.hpp"
#include "StatePair.hpp"
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <deque>
#include <fstream>
#include <iostream>
#include <map>
#include <ranges>
#include <unordered_map>
#include <utility>
#include <vector>

#define DEBUG_TYPE "nfa"

NFA::NFA(const TransLabel &c) : NFA()
{
	auto *init = createStarting();
	auto *fnal = createAccepting();
	addTransition(init, Transition(c, fnal));
}

auto NFA::flip() -> NFA &
{
	for (auto &s : states())
		s->flip();
	std::swap(getStarting(), getAccepting());
	return *this;
}

//-------------------------------------------------------------------

auto NFA::acceptsEmptyString() const -> bool
{
	return std::any_of(start_begin(), start_end(),
			   [this](auto &s) { return s->isAccepting(); });
}

auto NFA::acceptsNoString(Counterexample &cex) const -> bool
{
	std::vector<uint8_t> visited(getNumStates(), 0);
	std::vector<std::pair<State *, Counterexample>> workList;

	for (auto it = states_begin(); it != states_end(); it++) {
		if (!(*it)->isStarting()) {
			continue;
		}
		visited[(*it)->getId()] = 1;
		workList.emplace_back(it->get(), Counterexample());
	}
	while (!workList.empty()) {
		auto [s, c] = workList.back();
		workList.pop_back();
		if (s->isAccepting()) {
			cex = c;
			return false;
		}

		for (auto it = s->out_begin(); it != s->out_end(); it++) {
			if (visited[it->dest->getId()] != 0) {
				continue;
			}
			visited[it->dest->getId()] = 1;

			auto nc(c);
			nc.extend(it->label);
			workList.emplace_back(it->dest, nc);
		}
	}
	return true;
}

auto NFA::alt(NFA &&other) -> NFA &
{
	std::move(other.states_begin(), other.states_end(), std::back_inserter(nfa));
	getStarting().insert(getStarting().end(), other.getStarting().begin(),
			     other.getStarting().end());
	getAccepting().insert(getAccepting().end(), other.getAccepting().begin(),
			      other.getAccepting().end());
	compressStateIDs();
	return *this;
}

auto NFA::seq(NFA &&other) -> NFA &
{
	/* Merge the states first: the transitions added below are ordered by ID */
	auto thisAccepting = getAccepting();
	auto otherStarting = other.getStarting();
	std::move(other.states_begin(), other.states_end(), std::back_inserter(nfa));
	compressStateIDs();

	/* Add transitions `this->accepting --> other.starting.outgoing` */
	for (auto *acc : thisAccepting)
		for (auto *start : otherStarting)
			addTransitions(acc, start->out_begin(), start->out_end());

	/* Clear accepting states if necessary */
	if (!other.acceptsEmptyString()) {
		clearAllAccepting();
	}

	/* Clear starting states of `other` */
	other.clearAllStarting();

	/* Append `other`'s accepting states */
	getAccepting().insert(getAccepting().end(), other.getAccepting().begin(),
			      other.getAccepting().end());
	return *this;
}

auto NFA::plus() -> NFA &
{
	/* Add transitions `accepting --> starting` */
	for (auto *acc : accepting())
		for (auto *start : starting())
			addEpsilonTransitionSucc(acc, start);
	return *this;
}

auto NFA::or_empty() -> NFA &
{
	// Does the NFA already accept the empty string?
	if (acceptsEmptyString()) {
		return *this;
	}

	// Otherwise, find starting node with no incoming edges
	auto it =
		std::find_if(start_begin(), start_end(), [](auto &s) { return !s->hasIncoming(); });

	auto *s = (it != start_end()) ? *it : createStarting();
	makeAccepting(s);
	return *this;
}

auto NFA::star() -> NFA &
{
	std::vector<State *> exStarting;
	std::vector<State *> exAccepting;
	std::copy(start_begin(), start_end(), std::back_inserter(exStarting));
	std::copy(accept_begin(), accept_end(), std::back_inserter(exAccepting));

	clearAllStarting();
	clearAllAccepting();

	/* Create a state that will be the new starting/accepting; do
	 * not change its status yet so that addEpsilon does not add
	 * starting/accepting states */
	auto *i = createState();

	for (auto *es : exStarting)
		addEpsilonTransitionSucc(i, es);
	for (auto *acc : exAccepting)
		addEpsilonTransitionPred(acc, i);

	makeStarting(i);
	makeAccepting(i);
	assert(getNumAccepting() == getNumStarting() && getNumStarting() == 1);
	return *this;
}

/*
 * Gives a subset its unique representation. We do not use VSet here: we collect
 * the whole subset first and sort once, where VSet would insert one ID at a time.
 */
static void canonicalize(std::vector<unsigned> &ids)
{
	std::ranges::sort(ids);
	ids.erase(std::ranges::unique(ids).begin(), ids.end());
}

// Convert to a deterministic automaton using the subset construction
auto NFA::to_DFA() const -> std::pair<NFA, std::vector<std::vector<unsigned>>>
{
	/* Subsets are canonical, so equal ones hash alike */
	struct SubsetHasher {
		auto operator()(const std::vector<unsigned> &ids) const -> std::size_t
		{
			std::size_t hash = 0;
			for (auto id : ids) {
				hash_combine<unsigned>(hash, id);
			}
			return hash;
		}
	};

	NFA dfa;
	/* We name a subset by its sorted state IDs: comparing those beats comparing
	 * sets of pointers. We index dfaToNfa by DFA state ID, as the DFA numbers its
	 * states as it creates them and never removes one */
	std::unordered_map<std::vector<unsigned>, State *, SubsetHasher> nfaToDfaMap;
	std::vector<std::vector<unsigned>> dfaToNfa;

	std::vector<unsigned> ss;
	for (auto *s : starting()) {
		ss.push_back(s->getId());
	}
	canonicalize(ss);

	auto *start = dfa.createStarting();
	nfaToDfaMap.emplace(ss, dfa[start->getId()]);
	dfaToNfa.push_back(ss);

	std::vector<std::vector<unsigned>> worklist = {std::move(ss)};
	while (!worklist.empty()) {
		auto sc = std::move(worklist.back());
		worklist.pop_back();
		auto *ds = nfaToDfaMap[sc];

		/* Bucket the subset's outgoing transitions by label, so that each
		 * successor subset is built once instead of once per transition
		 * carrying that label */
		std::map<TransLabel, std::vector<unsigned>> successors;
		for (auto id : sc) {
			for (const auto &t : (*this)[id]->outs()) {
				successors[t.label].push_back(t.dest->getId());
			}
		}

		for (auto &[label, next] : successors) {
			canonicalize(next);
			auto it = nfaToDfaMap.find(next);
			State *nextDfa = nullptr;
			if (it != nfaToDfaMap.end()) {
				nextDfa = it->second;
			} else {
				nextDfa = dfa.createState();
				VERIFY(nextDfa->getId() == dfaToNfa.size(),
				       "subsets are addressed by DFA state ID");
				nfaToDfaMap.emplace(next, nextDfa);
				dfaToNfa.push_back(next);
				worklist.push_back(std::move(next));
			}
			NFA::addTransition(ds, Transition(label, nextDfa));
		}
	}

	for (auto &s : dfa.states()) {
		if (std::ranges::any_of(dfaToNfa[s->getId()], [&](unsigned id) -> bool {
			    return (*this)[id]->isAccepting();
		    })) {
			dfa.makeAccepting(&*s);
		}
	}
	return std::make_pair(std::move(dfa), std::move(dfaToNfa));
}

auto operator<<(std::ostream &ostr, const NFA &nfa) -> std::ostream &
{
	ostr << "[NFA with " << nfa.getNumStates() << " states]" << std::endl;
	ostr << "starting:";
	std::for_each(nfa.start_begin(), nfa.start_end(),
		      [&](auto &s) { ostr << " " << s->getId(); });
	ostr << " accepting:";
	std::for_each(nfa.accept_begin(), nfa.accept_end(),
		      [&](auto &s) { ostr << " " << s->getId(); });
	ostr << std::endl;
	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s) {
		std::for_each(s->out_begin(), s->out_end(), [&](const NFA::Transition &t) {
			ostr << "\t" << s->getId() << " --" << t.label << "--> " << t.dest->getId()
			     << std::endl;
		});
	});
	return ostr;
}
