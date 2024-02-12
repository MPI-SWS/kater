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
#include "Theory.hpp"

#define DEBUG_TYPE "nfa-utils"

auto calculateReachableFrom(NFA &nfa, const std::vector<NFA::State *> &ss)
	-> std::unordered_set<NFA::State *>
{
	std::unordered_set<NFA::State *> visited;
	std::vector<NFA::State *> workList;

	for (auto *s : ss) {
		visited.insert(s);
		workList.push_back(s);
	}
	while (!workList.empty()) {
		auto *s = workList.back();
		workList.pop_back();
		for (auto it = s->out_begin(); it != s->out_end(); it++) {
			if (visited.count(it->dest) != 0u) {
				continue;
			}
			visited.insert(it->dest);
			workList.push_back(it->dest);
		}
	}
	return visited;
}

auto calculateReachingTo(NFA &nfa, const std::vector<NFA::State *> &ss)
	-> std::unordered_set<NFA::State *>
{
	nfa.flip();
	auto visited = calculateReachableFrom(nfa, ss);
	nfa.flip();
	return visited;
}

void removeDeadStatesDFS(NFA &nfa)
{
	auto useful = calculateReachingTo(nfa, {nfa.accept_begin(), nfa.accept_end()});

	std::set<NFA::State *> toRemove;
	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s) {
		if (!useful.count(&*s)) {
			toRemove.insert(&*s);
		}
	});
	nfa.removeStatesIf([&](NFA::State *s) { return toRemove.contains(s); });

	if (nfa.getNumStarting() == 0) {
		nfa.createStarting();
	}
}

void removeDeadStates(NFA &nfa) { applyBidirectionally(removeDeadStatesDFS, nfa); }

template <typename T> inline void hash_combine(std::size_t &seed, std::size_t v)
{
	std::hash<T> hasher;
	seed ^= hasher(v) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
}

namespace {
using SPair = std::pair<NFA::State *, NFA::State *>;

struct SPairHasher {
	auto operator()(SPair p) const -> std::size_t
	{
		std::size_t hash = 0;
		hash_combine<unsigned>(hash, p.first->getId());
		hash_combine<unsigned>(hash, p.first->getId());
		return hash;
	}
};
} // namespace

auto isSimilarTo(
	const NFA & /*nfa*/, const NFA::Transition &t1, const NFA::Transition &t2,
	const std::unordered_map<NFA::State *, std::unordered_map<NFA::State *, bool>> &similar)
	-> bool
{
	return t1.label == t2.label && similar.find(t1.dest)->second.find(t2.dest)->second;
}

auto hasSimilarTransition(
	const NFA &nfa, NFA::State *s, const NFA::Transition &t1,
	const std::unordered_map<NFA::State *, std::unordered_map<NFA::State *, bool>> &similar)
	-> bool
{
	return std::any_of(s->out_begin(), s->out_end(),
			   [&](auto &t2) { return isSimilarTo(nfa, t1, t2, similar); });
}

// HERE: Optimize using set iterators + document comparison postcondition
auto isSimilarTo(
	const NFA &nfa, NFA::State *s1, NFA::State *s2,
	const std::unordered_map<NFA::State *, std::unordered_map<NFA::State *, bool>> &similar)
	-> bool
{
	return std::all_of(s1->out_begin(), s1->out_end(), [&](auto &t1) {
		return s2->hasOutgoing(t1) || hasSimilarTransition(nfa, s2, t1, similar);
	});
}

auto findSimilarStates(NFA &nfa)
	-> std::unordered_map<NFA::State *, std::unordered_map<NFA::State *, bool>>
{
	std::unordered_map<NFA::State *, std::unordered_map<NFA::State *, bool>> similar;

	std::unordered_map<NFA::State *, bool> initV;
	std::generate_n(
		std::inserter(initV, initV.begin()), nfa.getNumStates(),
		[sIt = nfa.states_begin()]() mutable { return std::make_pair(&**sIt++, true); });
	std::generate_n(std::inserter(similar, similar.begin()), nfa.getNumStates(),
			[sIt = nfa.states_begin(), v = initV]() mutable {
				return std::make_pair(&**sIt++, v);
			});

	/* Remove accepting/non-accepting pairs */
	std::for_each(nfa.accept_begin(), nfa.accept_end(), [&](auto &a) {
		std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s) {
			if (!s->isAccepting()) {
				similar[a][&*s] = false;
			}
		});
	});

	bool changed = true;
	while (changed) {
		changed = false;
		std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s1) {
			std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s2) {
				if (similar[&*s1][&*s2] && !isSimilarTo(nfa, &*s1, &*s2, similar)) {
					similar[&*s1][&*s2] = false;
					changed = true;
				}
			});
		});
	}
	return similar;
}

void removeSimilarTransitionsOneDirection(NFA &nfa)
{
	auto simMatrix = findSimilarStates(nfa);

	/* Similar states */
	std::unordered_set<::SPair, ::SPairHasher> similar;
	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s1) {
		std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s2) {
			if (&*s1 != &*s2 && simMatrix[&*s1][&*s2] && simMatrix[&*s2][&*s1] &&
			    !similar.count({&*s2, &*s1})) {
				similar.insert({&*s1, &*s2});
			}
		});
	});
	std::for_each(similar.begin(), similar.end(), [&](auto &p) {
		if (p.second->isStarting()) {
			nfa.makeStarting(p.first);
		}
		nfa.addInvertedTransitions(p.first, p.second->in_begin(), p.second->in_end());
	});
	std::unordered_set<NFA::State *> toRemove;
	for (auto &p : similar)
		toRemove.insert(p.second);
	nfa.removeStatesIf([&](NFA::State *s) { return toRemove.contains(s); });
	// std::for_each(similar.begin(), similar.end(), [&](auto &p){
	// 	nfa.removeState(p.second);
	// });

	/* Transitions to similar states (has to happen after similar removal) */
	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s) {
		nfa.removeTransitionsIf(&*s, [&](auto &t1) {
			return std::any_of(s->out_begin(), s->out_end(), [&](auto &t2) {
				return t1 != t2 && isSimilarTo(nfa, t1, t2, simMatrix);
			});
		});
	});
}

void removeSimilarTransitions(NFA &nfa)
{
	applyBidirectionally(removeSimilarTransitionsOneDirection, nfa);
}

// Return the state composition matrix, which is useful for minimizing the
// states of an NFA.  See Definition 3 of Kameda and Weiner: On the State
// Minimization of Nondeterministic Finite Automata
auto get_state_composition_matrix(NFA &nfa) -> std::unordered_map<NFA::State *, std::vector<char>>
{
	nfa.flip();
	auto p = nfa.to_DFA();
	auto &dfa = p.first;
	auto &dfaToNfaMap = p.second;
	nfa.flip();

	std::unordered_map<NFA::State *, std::vector<char>> result;

	// KATER_DEBUG(
	// 	std::cout << "State composition matrix: " << std::endl;
	// );
	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &si) {
		std::vector<char> row(dfaToNfaMap.size(), 0);
		auto i = 0U;
		std::for_each(dfaToNfaMap.begin(), dfaToNfaMap.end(), [&](auto &kv) {
			if (kv.second.find(&*si) != kv.second.end()) {
				row[i] = 1;
			}
			++i;
		});
		result.insert({&*si, row});
		// KATER_DEBUG(
		// 	std::cout << row << ": " << si->getId() << std::endl;
		// );
	});
	return result;
}

static auto is_subset(const std::vector<char> &a, const std::vector<char> &b) -> bool
{
	for (int k = 0; k < a.size(); ++k) {
		if ((a[k] != 0) && (b[k] == 0)) {
			return false;
		}
	}
	return true;
}

static void take_union(std::vector<char> &a, const std::vector<char> &b)
{
	for (int k = 0; k < a.size(); ++k) {
		a[k] |= b[k];
	}
}

static auto operator<<(std::ostream &ostr, const std::vector<char> &s) -> std::ostream &
{
	for (int c : s) {
		ostr << (c != 0 ? "1" : ".");
	}
	return ostr;
}

// Reduce the NFA using the state composition matrix (cf. Kameda and Weiner)
void scm_reduce(NFA &nfa)
{
	if (nfa.getNumStates() == 0) {
		return;
	}

	auto scm = get_state_composition_matrix(nfa);
	auto dfaSize = scm.begin()->second.size();
	std::vector<NFA::State *> toRemove;
	for (auto itI = nfa.states_begin(); itI != nfa.states_end(); /* ! */) {
		if ((*itI)->isStarting()) {
			++itI;
			continue;
		}
		/* Is I equal to the union of some other rows? */
		std::vector<char> newrow(dfaSize, 0);
		for (auto itJ = nfa.states_begin(); itJ != nfa.states_end(); ++itJ) {
			if (itI->get() != itJ->get() &&
			    is_subset(scm[itJ->get()], scm[itI->get()])) {
				take_union(newrow, scm[itJ->get()]);
			}
		}
		/* If not, skip, otherwise, erase */
		if (newrow != scm[itI->get()]) {
			++itI;
			continue;
		}
		KATER_DEBUG(std::cout << "erase node " << (*itI)->getId() << " with";);
		for (auto itJ = nfa.states_begin(); itJ != nfa.states_end(); ++itJ) {
			if (itI->get() != itJ->get() &&
			    is_subset(scm[itJ->get()], scm[itI->get()])) {
				KATER_DEBUG(std::cout << " " << (*itJ)->getId(););
				nfa.addInvertedTransitions(itJ->get(), (*itI)->in_begin(),
							   (*itI)->in_end());
			}
		}
		KATER_DEBUG(std::cout << std::endl;);
		scm.erase(itI->get());
		itI = nfa.removeState(itI);
	}
}

void scmReduce(NFA &nfa) { applyBidirectionally(scm_reduce, nfa); }

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
								// nfa.addTransition(&*s,
								// NFA::Transition(l, q.dest));
								toAdd.emplace_back(
									NFA::Transition(l, q.dest));
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

auto copy(const NFA &nfa, std::unordered_map<NFA::State *, NFA::State *> *uMap /* = nullptr */)
	-> NFA
{
	NFA result;
	std::unordered_map<NFA::State *, NFA::State *> mapping;

	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s1) {
		auto *s2 = result.createState();
		mapping[&*s1] = s2;
		if (s1->isStarting()) {
			result.makeStarting(s2);
		}
		if (s1->isAccepting()) {
			result.makeAccepting(s2);
		}
	});

	std::for_each(nfa.states_begin(), nfa.states_end(), [&](auto &s1) {
		std::for_each(s1->out_begin(), s1->out_end(), [&](auto &t) {
			NFA::addTransition(mapping[&*s1], t.copyTo(mapping[t.dest]));
		});
	});
	if (uMap != nullptr) {
		*uMap = std::move(mapping);
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
	std::unordered_map<NFA::State *, std::vector<NFA::Transition>> toDuplicateAccepting;

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
					auto trans = NFA::Transition(l, outIt2->dest);
					toAdd.push_back(trans);
					if (outIt->dest->isAccepting()) {
						toDuplicateAccepting[outIt2->dest].push_back(
							trans.flipTo(&*s));
					}
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
