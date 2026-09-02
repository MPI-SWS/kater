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

#include <cstdint>
#include <vector>

class Theory;

template <typename F> void applyBidirectionally(F &&fun, NFA &nfa)
{
	fun(nfa);
	nfa.flip();
	fun(nfa);
	nfa.flip();
}

/* Both return a getNumStates()-sized vector, indexed by state ID, with the
 * entries of the reachable states set */
/** Returns a flag per state ID, set for the states reachable from SS */
auto calculateReachableFrom(NFA &nfa, const std::vector<NFA::State *> &ss) -> std::vector<bool>;

/** Returns a flag per state ID, set for the states that can reach SS */
auto calculateReachingTo(NFA &nfa, const std::vector<NFA::State *> &ss) -> std::vector<bool>;

void removeDeadStates(NFA &nfa);

void removeSimilarTransitions(NFA &nfa);

/** Returns a states-squared matrix M, indexed by state ID, with M[s1][s2] set
 * iff S2 simulates S1. Not vector<bool>: the fixpoint indexes it heavily */
auto findSimilarStates(NFA &nfa) -> std::vector<uint8_t>;

void removeRedundantSelfLoops(NFA &nfa);

void compactEdges(NFA &nfa, const Theory &theory);

void addTransitivePredicateEdges(NFA &nfa, const Theory &theory);

void breakToParts(NFA &nfa);

void simplify(NFA &nfa, const Theory &theory);

auto copy(const NFA &nfa) -> NFA;

#endif /* NFA_UTILS_HPP */
