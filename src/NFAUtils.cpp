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

#include "NFAUtils.hpp"

#include "Error.hpp"
#include "NFA.hpp"
#include "Predicate.hpp"
#include "Relation.hpp"
#include "Theory.hpp"
#include "TransLabel.hpp"

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <iostream>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

#define DEBUG_TYPE "nfa-utils"

auto calculateReachableFrom(NFA &nfa, const std::vector<NFA::State *> &ss) -> std::vector<bool>
{
	std::vector<bool> visited(nfa.getNumStates(), false);
	std::vector<NFA::State *> workList;

	for (auto *s : ss) {
		visited[s->getId()] = true;
		workList.push_back(s);
	}
	while (!workList.empty()) {
		auto *s = workList.back();
		workList.pop_back();
		for (const auto &t : s->outs()) {
			if (visited[t.dest->getId()]) {
				continue;
			}
			visited[t.dest->getId()] = true;
			workList.push_back(t.dest);
		}
	}
	return visited;
}

auto calculateReachingTo(NFA &nfa, const std::vector<NFA::State *> &ss) -> std::vector<bool>
{
	nfa.flip();
	auto visited = calculateReachableFrom(nfa, ss);
	nfa.flip();
	return visited;
}

void removeDeadStatesDFS(NFA &nfa)
{
	auto useful = calculateReachingTo(nfa, {nfa.accept_begin(), nfa.accept_end()});

	nfa.removeStatesIf([&](NFA::State *s) -> bool { return !useful[s->getId()]; });

	if (nfa.getNumStarting() == 0) {
		nfa.createStarting();
	}
}

void removeDeadStates(NFA &nfa) { applyBidirectionally(removeDeadStatesDFS, nfa); }

/* Hashes a label by exactly the fields TransLabel::operator== compares */
struct TransLabelHasher {
	auto operator()(const TransLabel &lab) const -> std::size_t
	{
		std::size_t hash = 0;
		if (const auto &rel = lab.getRelation(); rel.has_value()) {
			hash_combine<Relation::ID>(hash, rel->getID());
			hash_combine<unsigned>(hash, rel->isInverse() ? 1U : 0U);
		}
		/* Predicate sets are sorted, so combining in order is canonical */
		for (const auto &pred : lab.getPreChecks()) {
			hash_combine<Predicate::ID>(hash, pred.getID());
		}
		for (const auto &pred : lab.getPostChecks()) {
			hash_combine<Predicate::ID>(hash, pred.getID());
		}
		return hash;
	}
};

/* An integer view of the transitions, so that the fixpoint never touches a
 * TransLabel. Distinct labels get distinct numbers */
struct TransitionIndex {
	/* Whether every label leaving S also leaves T, i.e. whether T can possibly
	 * match each of S's transitions */
	[[nodiscard]] auto labelsSubsume(uint32_t source, uint32_t target) const -> bool
	{
		for (auto word = 0U; word < maskWords; word++) {
			if ((labelMask[(source * maskWords) + word] &
			     ~labelMask[(target * maskWords) + word]) != 0) {
				return false;
			}
		}
		return true;
	}

	/* outs[s] holds the (label, destination) pairs of S's outgoing transitions */
	std::vector<std::vector<std::pair<uint32_t, uint32_t>>> outs;
	/* destsByLabel[s][l] holds the states reachable from S via label L */
	std::vector<std::vector<std::vector<uint32_t>>> destsByLabel;
	/* labelMask[s] holds the labels S leaves by, spread over maskWords words */
	unsigned maskWords = 0;
	std::vector<uint64_t> labelMask;
};

/* Whether NFA's state IDs are dense, i.e., each state's ID is its index */
static auto hasDenseStateIDs(const NFA &nfa) -> bool
{
	unsigned id = 0;
	return std::ranges::all_of(nfa.states(),
				   [&id](auto &s) -> bool { return s->getId() == id++; });
}

static auto indexTransitions(NFA &nfa) -> TransitionIndex
{
	assert(hasDenseStateIDs(nfa) && "state IDs are used as indices below");

	std::unordered_map<TransLabel, uint32_t, TransLabelHasher> labelToId;
	TransitionIndex index;

	index.outs.resize(nfa.getNumStates());
	for (auto &s : nfa.states()) {
		for (const auto &t : s->outs()) {
			/* Compute the ID first: argument evaluation order is unspecified */
			const auto nextId = static_cast<uint32_t>(labelToId.size());
			const auto labelId = labelToId.try_emplace(t.label, nextId).first->second;
			index.outs[s->getId()].emplace_back(labelId, t.dest->getId());
		}
	}

	index.destsByLabel.assign(nfa.getNumStates(),
				  std::vector<std::vector<uint32_t>>(labelToId.size()));
	for (auto src = 0U; src < index.outs.size(); src++) {
		for (auto [label, dest] : index.outs[src]) {
			index.destsByLabel[src][label].push_back(dest);
		}
	}

	static constexpr auto bits = 64U;
	index.maskWords = static_cast<unsigned>((labelToId.size() + bits - 1) / bits);
	index.labelMask.assign(nfa.getNumStates() * static_cast<size_t>(index.maskWords), 0);
	for (auto src = 0U; src < index.outs.size(); src++) {
		for (auto [label, dest] : index.outs[src]) {
			index.labelMask[(src * index.maskWords) + (label / bits)] |=
				UINT64_C(1) << (label % bits);
		}
	}
	return index;
}

/* Whether every outgoing transition of S1 is matched by an equally-labeled one
 * of S2 into a similar state */
static auto isSimilarTo(uint32_t s1, uint32_t s2, const std::vector<uint8_t> &similar,
			unsigned nrStates, const TransitionIndex &index) -> bool
{
	return std::ranges::all_of(index.outs[s1], [&](auto out) -> bool {
		auto [label, dest] = out;
		return std::ranges::any_of(index.destsByLabel[s2][label], [&](auto s2Dest) -> bool {
			return similar[(dest * nrStates) + s2Dest] != 0;
		});
	});
}

auto findSimilarStates(NFA &nfa) -> std::vector<uint8_t>
{
	const auto nrStates = static_cast<unsigned>(nfa.getNumStates());
	std::vector<uint8_t> similar(static_cast<size_t>(nrStates) * nrStates, 1);
	const auto index = indexTransitions(nfa);

	/* Remove accepting/non-accepting pairs */
	for (auto &acc : nfa.accepting()) {
		for (auto &s : nfa.states()) {
			if (!s->isAccepting()) {
				similar[(acc->getId() * nrStates) + s->getId()] = 0;
			}
		}
	}

	/* If S2 lacks a label S1 leaves by, it can never match that transition. We
	 * settle that here instead of letting the fixpoint rediscover it one
	 * transition at a time, which is where most of its first pass went */
	for (auto s1 = 0U; s1 < nrStates; s1++) {
		for (auto s2 = 0U; s2 < nrStates; s2++) {
			if (!index.labelsSubsume(s1, s2)) {
				similar[(s1 * nrStates) + s2] = 0;
			}
		}
	}

	bool changed = true;
	while (changed) {
		changed = false;
		for (auto s1 = 0U; s1 < nrStates; s1++) {
			for (auto s2 = 0U; s2 < nrStates; s2++) {
				if (similar[(s1 * nrStates) + s2] != 0 &&
				    !isSimilarTo(s1, s2, similar, nrStates, index)) {
					similar[(s1 * nrStates) + s2] = 0;
					changed = true;
				}
			}
		}
	}
	return similar;
}

void removeSimilarTransitionsOneDirection(NFA &nfa)
{
	auto statesNr = static_cast<unsigned>(nfa.getNumStates());
	std::vector<uint8_t> simMatrix = findSimilarStates(nfa);

	/*
	 * Similarity is a preorder, so mutual similarity is an equivalence, and
	 * every class collapses into a single state. Elect the first member of each
	 * class as its representative: since the relation is transitive, the first
	 * earlier state mutually similar to S already carries S's representative.
	 */
	auto mutuallySimilar = [&](unsigned s1, unsigned s2) -> bool {
		return simMatrix[(s1 * statesNr) + s2] == 1 && simMatrix[(s2 * statesNr) + s1] == 1;
	};

	std::vector<unsigned> representative(statesNr);
	// NOLINTNEXTLINE(modernize-use-ranges): libc++ has no std::ranges::iota
	std::iota(representative.begin(), representative.end(), 0U);
	for (auto s2 = 0U; s2 < statesNr; s2++) {
		for (auto s1 = 0U; s1 < s2; s1++) {
			if (mutuallySimilar(s1, s2)) {
				representative[s2] = representative[s1];
				break;
			}
		}
	}

	/* Merge each non-representative into its representative, once */
	std::vector<uint8_t> merged(statesNr, 0);
	for (auto state = 0U; state < statesNr; state++) {
		if (representative[state] == state) {
			continue;
		}
		auto *rep = nfa[representative[state]];
		if (nfa[state]->isStarting()) {
			nfa.makeStarting(rep);
		}
		nfa.addInvertedTransitions(rep, nfa[state]->in_begin(), nfa[state]->in_end());
		merged[state] = 1;
	}

	/* The removal renumbers the states, so record which old ID each new one
	 * stands for, and reindex simMatrix once it is done */
	std::vector<unsigned> oldIDs;
	for (auto &s : nfa.states()) {
		if (merged[s->getId()] == 0)
			oldIDs.push_back(s->getId());
	}
	nfa.removeStatesIf([&](NFA::State *s) -> bool { return merged[s->getId()] != 0; });

	const auto leftNr = static_cast<unsigned>(oldIDs.size());
	VERIFY(nfa.getNumStates() == leftNr, "removal has to preserve the state order");
	std::vector<uint8_t> simLeft(static_cast<size_t>(leftNr) * leftNr);
	for (auto s1 = 0U; s1 < leftNr; s1++) {
		for (auto s2 = 0U; s2 < leftNr; s2++) {
			simLeft[(s1 * leftNr) + s2] =
				simMatrix[(oldIDs[s1] * statesNr) + oldIDs[s2]];
		}
	}

	/* Transitions to similar states (has to happen after similar removal) */
	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s) {
		nfa.removeTransitionsIf(&*s, [&](auto &t1) {
			return std::any_of(s->out_begin(), s->out_end(), [&](auto &t2) {
				return t1 != t2 && t1.label == t2.label &&
				       simLeft[(t1.dest->getId() * leftNr) + t2.dest->getId()];
			});
		});
	});
}

void removeSimilarTransitions(NFA &nfa)
{
	applyBidirectionally(removeSimilarTransitionsOneDirection, nfa);
}

void removeRedundantSelfLoops(NFA &nfa)
{
	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s) {
		std::vector<NFA::Transition> toRemove;
		std::copy_if(
			s->out_begin(), s->out_end(), std::back_inserter(toRemove),
			[&](const NFA::Transition &t1) {
				return (t1.dest != &*s && !s->isAccepting() &&
					std::all_of(s->out_begin(), s->out_end(),
						    [&](const NFA::Transition &t2) {
							    return t2.label == t1.label &&
								   (t2.dest == &*s ||
								    std::find(t1.dest->out_begin(),
									      t1.dest->out_end(),
									      t2) !=
									    t1.dest->out_end());
						    }));
			});
		std::transform(toRemove.begin(), toRemove.end(), toRemove.begin(),
			       [&](auto &t) { return NFA::Transition(t.label, &*s); });
		nfa.removeTransitions(&*s, toRemove.begin(), toRemove.end());
	});
}

/* Join `[...]` edges with successor edges */
auto joinPredicateEdges(NFA &nfa, const Theory &theory) -> bool
{
	bool changed = false;
	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s) {
		std::vector<NFA::Transition> toRemove, toAdd;
		std::copy_if(
			s->out_begin(), s->out_end(), std::back_inserter(toRemove),
			[&](const NFA::Transition &t) {
				if (!t.label.isPredicate()) {
					return false;
				}
				if (t.dest->isAccepting() && t.dest != &*s) {
					return false;
				}
				KATER_DEBUG(std::cout << "Compacting edge " << s->getId() << " --"
						      << t.label << "--> " << t.dest->getId()
						      << std::endl;);
				if (t.dest != &*s) {
					std::for_each(
						t.dest->out_begin(), t.dest->out_end(),
						[&](const NFA::Transition &q) {
							if ((q.label.isRelation() ||
							     q.dest != &*s) &&
							    theory.composes(t.label, q.label)) {
								auto l = t.label;
								l.merge(q.label);
								toAdd.emplace_back(l, q.dest);
							}
						});
				}
				return true;
			});
		changed |= !toRemove.empty();
		nfa.removeTransitions(&*s, toRemove.begin(), toRemove.end());
		nfa.addTransitions(&*s, toAdd.begin(), toAdd.end());
	});
	return changed;
}

void compactEdges(NFA &nfa, const Theory &theory)
{
	auto compactFunction = [&](auto &nfa) {
		while (joinPredicateEdges(nfa, theory))
			;
		removeRedundantSelfLoops(nfa);
	};

	applyBidirectionally(compactFunction, nfa);
}

/** Returns a copy of NFA. The copy of state i is the copy's state i */
auto copy(const NFA &nfa) -> NFA
{
	NFA result;

	for (const auto &s : nfa.states()) {
		auto *copied = result.createState();
		if (s->isStarting())
			result.makeStarting(copied);
		if (s->isAccepting())
			result.makeAccepting(copied);
	}
	for (const auto &s : nfa.states()) {
		for (const auto &t : s->outs())
			NFA::addTransition(result[s->getId()], t.copyTo(result[t.dest->getId()]));
	}
	return result;
}

void breakIntoMultiple(NFA &nfa, NFA::State *s, const NFA::Transition &t)
{
	if (t.label.isPredicate()) {
		return;
	}

	auto *curr = s;

	curr = nfa.addTransitionToFresh(curr, TransLabel(std::nullopt, t.label.getPreChecks()));
	curr = nfa.addTransitionToFresh(curr, TransLabel(t.label.getRelation()));
	nfa.addTransition(
		curr, NFA::Transition(TransLabel(std::nullopt, t.label.getPostChecks()), t.dest));
}

void breakToParts(NFA &nfa)
{
	std::vector<std::pair<NFA::State *, NFA::Transition>> toBreak;
	std::vector<std::pair<NFA::State *, NFA::Transition>> toRemove;
	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s) {
		std::for_each(s->out_begin(), s->out_end(), [&](auto &t) {
			if (!t.label.isPredicate()) {
				toBreak.push_back({&*s, t});
				toRemove.push_back({&*s, t});
			}
		});
	});
	std::for_each(toBreak.begin(), toBreak.end(),
		      [&](auto &p) { breakIntoMultiple(nfa, p.first, p.second); });
	std::for_each(toRemove.begin(), toRemove.end(),
		      [&](auto &p) { nfa.removeTransition(p.first, p.second); });
	removeDeadStates(nfa);
}

void addTransitivePredicateEdges(NFA &nfa, const Theory &theory)
{
	std::vector<std::pair<NFA::State *, NFA::Transition>> toRemove;
	std::vector<NFA::Transition> toCreateStarting;

	for (auto it = nfa.states_begin(); it != nfa.states_end(); ++it) {
		auto &s = *it;
		std::vector<NFA::Transition> toAdd;
		for (auto outIt = s->out_begin(); outIt != s->out_end(); ++outIt) {
			if (!outIt->label.isPredicate()) {
				continue;
			}

			for (auto outIt2 = outIt->dest->out_begin();
			     outIt2 != outIt->dest->out_end(); ++outIt2) {
				if (!outIt2->label.isPredicate()) {
					continue;
				}

				if (theory.composes(outIt->label, outIt2->label)) {
					auto l = outIt->label;
					l.merge(outIt2->label);
					toAdd.emplace_back(l, outIt2->dest);
				}
				if (outIt->dest->isStarting()) {
					toCreateStarting.push_back(*outIt2);
				}
				toRemove.emplace_back(outIt->dest, *outIt2);
			}
		}
		nfa.addTransitions(&*s, toAdd.begin(), toAdd.end());
	}
	if (!toCreateStarting.empty()) {
		auto *n = nfa.createStarting();
		nfa.addTransitions(n, toCreateStarting.begin(), toCreateStarting.end());
	}
	std::for_each(toRemove.begin(), toRemove.end(),
		      [&](auto &p) { nfa.removeTransition(p.first, p.second); });
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
	compactEdges(nfa, theory);
	removeDeadStates(nfa);
	removeSimilarTransitions(nfa);
	removeDeadStates(nfa);

	KATER_DEBUG(std::cout << "After simplification: " << nfa;);
}
