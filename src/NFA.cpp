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
#include <deque>
#include <fstream>
#include <iostream>

#define DEBUG_TYPE "nfa"

NFA::NFA(const TransLabel &c) : NFA()
{
	auto *init = createStarting();
	auto *fnal = createAccepting();
	addTransition(init, Transition(c, fnal));
}

auto NFA::flip() -> NFA &
{
	std::for_each(states_begin(), states_end(), [](auto &s) { s->flip(); });
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
	std::unordered_set<State *> visited;
	std::vector<std::pair<State *, Counterexample>> workList;

	for (auto it = states_begin(); it != states_end(); it++) {
		if (!(*it)->isStarting()) {
			continue;
		}
		visited.insert(it->get());
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
			if (visited.count(it->dest) != 0u) {
				continue;
			}
			visited.insert(it->dest);

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
	return *this;
}

auto NFA::seq(NFA &&other) -> NFA &
{
	/* Add transitions `this->accepting --> other.starting.outgoing` */
	std::for_each(accept_begin(), accept_end(), [&](auto &a) {
		std::for_each(other.start_begin(), other.start_end(),
			      [&](auto &s) { addTransitions(a, s->out_begin(), s->out_end()); });
	});

	/* Clear accepting states if necessary */
	if (!other.acceptsEmptyString()) {
		clearAllAccepting();
	}

	/* Clear starting states of `other` */
	other.clearAllStarting();

	/* Move the states of the `other` NFA into our NFA and append accepting states */
	std::move(other.states_begin(), other.states_end(), std::back_inserter(nfa));
	getAccepting().insert(getAccepting().end(), other.getAccepting().begin(),
			      other.getAccepting().end());
	return *this;
}

auto NFA::plus() -> NFA &
{
	/* Add transitions `accepting --> starting` */
	std::for_each(accept_begin(), accept_end(), [&](auto &a) {
		std::for_each(start_begin(), start_end(),
			      [&](auto &s) { addEpsilonTransitionSucc(a, s); });
	});
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

	std::for_each(exStarting.begin(), exStarting.end(),
		      [&](auto *es) { addEpsilonTransitionSucc(i, es); });
	std::for_each(exAccepting.begin(), exAccepting.end(),
		      [&](auto *ea) { addEpsilonTransitionPred(ea, i); });

	makeStarting(i);
	makeAccepting(i);
	assert(getNumAccepting() == getNumStarting() && getNumStarting() == 1);
	return *this;
}

// Convert to a deterministic automaton using the subset construction
auto NFA::to_DFA() const -> std::pair<NFA, std::map<NFA::State *, std::set<NFA::State *>>>
{
	NFA dfa;
	std::map<std::set<State *>, State *> nfaToDfaMap; // m
	std::map<State *, std::set<State *>> dfaToNfaMap; // v

	auto *s = dfa.createStarting();
	auto ss = std::set<State *>(start_begin(), start_end());
	nfaToDfaMap.insert({ss, s});
	dfaToNfaMap.insert({s, ss});

	std::vector<std::set<State *>> worklist = {ss};
	while (!worklist.empty()) {
		auto sc = worklist.back();
		worklist.pop_back();

		// XXX: FIXME
		std::for_each(sc.begin(), sc.end(), [&](State *ns) {
			std::for_each(ns->out_begin(), ns->out_end(), [&](const Transition &t) {
				std::set<State *> next;
				std::for_each(sc.begin(), sc.end(), [&](State *ns2) {
					std::for_each(ns2->out_begin(), ns2->out_end(),
						      [&](const Transition &t2) {
							      if (t2.label == t.label) {
								      next.insert(t2.dest);
							      }
						      });
				});
				auto it = nfaToDfaMap.find(next);
				State *ds = nullptr;
				if (it != nfaToDfaMap.end()) {
					ds = it->second;
				} else {
					ds = dfa.createState();
					nfaToDfaMap.insert({next, ds});
					dfaToNfaMap.insert({ds, next});
					worklist.push_back(std::move(next));
				}
				NFA::addTransition(nfaToDfaMap[sc], Transition(t.label, ds));
			});
		});
	}

	std::for_each(dfaToNfaMap.begin(), dfaToNfaMap.end(), [&](auto &kv) {
		if (std::any_of(kv.second.begin(), kv.second.end(),
				[&](State *s) { return s->isAccepting(); })) {
			dfa.makeAccepting(kv.first);
		}
	});
	return std::make_pair(std::move(dfa), std::move(dfaToNfaMap));
}

template <typename T> auto operator<<(std::ostream &ostr, const std::set<T> &s) -> std::ostream &
{
	bool not_first = false;
	for (auto i : s) {
		if (not_first) {
			ostr << ", ";
		} else {
			not_first = true;
		}
		ostr << i;
	}
	return ostr;
}

template <> auto operator<<(std::ostream &ostr, const std::set<NFA::State *> &s) -> std::ostream &
{
	bool not_first = false;
	for (auto *i : s) {
		if (not_first) {
			ostr << ", ";
		} else {
			not_first = true;
		}
		ostr << i->getId();
	}
	return ostr;
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
