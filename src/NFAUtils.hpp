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

#ifndef NFA_UTILS_HPP
#define NFA_UTILS_HPP

#include "NFA.hpp"

class Theory;

template <typename F> void applyBidirectionally(F &&fun, NFA &nfa)
{
	fun(nfa);
	nfa.flip();
	fun(nfa);
	nfa.flip();
}

auto calculateReachableFrom(NFA &nfa, const std::vector<NFA::State *> &ss)
	-> std::unordered_set<NFA::State *>;

auto calculateReachingTo(NFA &nfa, const std::vector<NFA::State *> &ss)
	-> std::unordered_set<NFA::State *>;

void removeDeadStates(NFA &nfa);

void removeSimilarTransitions(NFA &nfa);

void scmReduce(NFA &nfa);

void removeRedundantSelfLoops(NFA &nfa);

void compactEdges(NFA &nfa, const Theory &theory);

void addTransitivePredicateEdges(NFA &nfa, const Theory &theory);

void breakToParts(NFA &nfa);

auto copy(const NFA &nfa, std::unordered_map<NFA::State *, NFA::State *> *uMap = nullptr) -> NFA;

#endif /* NFA_UTILS_HPP */
