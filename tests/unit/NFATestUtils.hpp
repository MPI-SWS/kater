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
 *
 */

#ifndef KATER_NFA_TEST_UTILS_HPP
#define KATER_NFA_TEST_UTILS_HPP

#include "NFA.hpp"
#include "Predicate.hpp"
#include "Relation.hpp"
#include "TransLabel.hpp"

#include <algorithm>
#include <array>
#include <cstdint>
#include <optional>
#include <random>
#include <ranges>
#include <set>
#include <sstream>
#include <string>
#include <unordered_set>
#include <vector>

/******************************************************************************
 ** Helpers for building NFAs in tests, and for inspecting the language they accept
 ******************************************************************************/

using OptPred = std::optional<Predicate::BuiltinID>;
using OptRel = std::optional<Relation::BuiltinID>;

/* Builds `[pre] ; rel ; [post]`, omitting the nullopt parts; an epsilon
 * transition if called with no arguments */
inline auto makeLabel(OptPred pre = {}, OptRel rel = {}, bool isInverse = false, OptPred post = {})
	-> TransLabel
{
	PredicateSet preChecks;
	PredicateSet postChecks;

	if (pre.has_value()) {
		preChecks = PredicateSet(Predicate::createBuiltin(*pre));
	}
	if (post.has_value()) {
		postChecks = PredicateSet(Predicate::createBuiltin(*post));
	}
	if (!rel.has_value()) {
		return TransLabel(std::nullopt, preChecks, postChecks);
	}

	auto relation = Relation::createBuiltin(*rel);
	if (isInverse) {
		relation.invert();
	}
	return TransLabel(relation, preChecks, postChecks);
}

/* A pseudo-random NFA, for cross-checking on inputs larger than a hand-written one. A
 * fifth of the transitions carry an inverted relation, so that mishandling
 * inverses shows up here too */
inline auto makeRandomNFA(unsigned nrStates, unsigned transitionsPerState,
			  unsigned acceptPercent = 1, unsigned seed = 42) -> NFA
{
	/* An arbitrary but fixed set of labels to draw from */
	static constexpr std::array relations = {Relation::po, Relation::rf, Relation::mo,
						 Relation::detour};
	static constexpr std::array predicates = {Predicate::BuiltinID::R, Predicate::BuiltinID::W,
						  Predicate::BuiltinID::F,
						  Predicate::BuiltinID::SC};
	static constexpr unsigned startPercent = 5;
	static constexpr unsigned epsilonPercent = 20;
	static constexpr unsigned invertedPercent = 40;

	NFA nfa;
	std::mt19937 gen(seed);
	std::uniform_int_distribution<unsigned> percent(1, 100);
	std::uniform_int_distribution<size_t> relIdx(0, relations.size() - 1);
	std::uniform_int_distribution<size_t> predIdx(0, predicates.size() - 1);

	std::vector<NFA::State *> states;
	states.reserve(nrStates);
	for (auto i = 0U; i < nrStates; i++) {
		if (percent(gen) <= acceptPercent) {
			states.push_back(nfa.createAccepting());
		} else if (i == 0 || percent(gen) <= startPercent) {
			states.push_back(nfa.createStarting());
		} else {
			states.push_back(nfa.createState());
		}
	}

	std::uniform_int_distribution<size_t> stateIdx(0, states.size() - 1);
	for (auto *src : states) {
		for (auto tr = 0U; tr < transitionsPerState; tr++) {
			auto *dst = states.at(stateIdx(gen));
			auto roll = percent(gen);
			auto label = roll <= epsilonPercent ? makeLabel()
							    : makeLabel(predicates.at(predIdx(gen)),
									relations.at(relIdx(gen)),
									roll <= invertedPercent);
			NFA::addTransition(src, NFA::Transition(label, dst));
		}
	}
	return nfa;
}

/* The definition of similarity written as directly as possible; the oracle for
 * the optimized implementation in NFAUtils.cpp */
inline auto findSimilarStatesReference(NFA &nfa) -> std::vector<uint8_t>
{
	const auto nrStates = static_cast<unsigned>(nfa.getNumStates());
	std::vector<uint8_t> similar(static_cast<size_t>(nrStates) * nrStates, 1);

	for (auto &acc : nfa.accepting()) {
		for (auto &s : nfa.states()) {
			if (!s->isAccepting()) {
				similar[(acc->getId() * nrStates) + s->getId()] = 0;
			}
		}
	}

	bool changed = true;
	while (changed) {
		changed = false;
		for (auto &s1 : nfa.states()) {
			for (auto &s2 : nfa.states()) {
				if (!similar[(s1->getId() * nrStates) + s2->getId()]) {
					continue;
				}
				for (auto &t1 : s1->outs()) {
					bool found = false;
					for (auto &t2 : s2->outs()) {
						if (t1.label == t2.label &&
						    similar[(t1.dest->getId() * nrStates) +
							    t2.dest->getId()]) {
							found = true;
							break;
						}
					}
					if (!found) {
						similar[(s1->getId() * nrStates) + s2->getId()] = 0;
						changed = true;
						break;
					}
				}
			}
		}
	}
	return similar;
}

/*
 * A word is a sequence of labels. Which passes preserve the language in this
 * sense:
 *   - yes: removeDeadStates, removeSimilarTransitions, to_DFA. We test those
 *     with sameLanguageUpTo().
 *   - no: compactEdges, breakToParts, saturate*, which rewrite labels. We test
 *     those with wellFormed() only.
 */
using Word = std::vector<TransLabel>;

/* Every distinct label appearing in NFA, sorted so the order is reproducible */
inline auto alphabetOf(const NFA &nfa) -> std::vector<TransLabel>
{
	std::vector<TransLabel> alphabet;

	for (const auto &s : nfa.states()) {
		for (const auto &t : s->outs()) {
			alphabet.push_back(t.label);
		}
	}
	std::ranges::sort(alphabet);
	alphabet.erase(std::ranges::unique(alphabet).begin(), alphabet.end());
	return alphabet;
}

/* Whether NFA accepts WORD, by subset simulation */
inline auto acceptsWord(const NFA &nfa, const Word &word) -> bool
{
	std::vector<NFA::State *> current(nfa.start_begin(), nfa.start_end());

	for (const auto &symbol : word) {
		std::vector<NFA::State *> next;
		for (auto *s : current) {
			for (const auto &t : s->outs()) {
				if (t.label == symbol) {
					next.push_back(t.dest);
				}
			}
		}
		std::ranges::sort(next);
		next.erase(std::ranges::unique(next).begin(), next.end());
		current = std::move(next);
		if (current.empty()) {
			return false;
		}
	}
	return std::ranges::any_of(current, [](auto *s) -> bool { return s->isAccepting(); });
}

/* Refuse to enumerate more words than this, rather than hang */
inline constexpr size_t defaultWordCap = 100000;

/*
 * Returns every word of length <= MAXLEN that NFA accepts, or nullopt past CAP
 * of them. We walk paths rather than all words, so the cost follows the
 * automaton and not |alphabet|^MAXLEN.
 */
inline auto acceptedWordsUpTo(const NFA &nfa, unsigned maxLen, size_t cap = defaultWordCap)
	-> std::optional<std::set<Word>>
{
	std::set<Word> words;
	Word word;
	auto overflowed = false;

	auto visit = [&](auto &self, NFA::State *s, unsigned depth) -> void {
		if (overflowed) {
			return;
		}
		if (s->isAccepting()) {
			words.insert(word);
			if (words.size() > cap) {
				overflowed = true;
				return;
			}
		}
		if (depth == maxLen) {
			return;
		}
		for (const auto &t : s->outs()) {
			word.push_back(t.label);
			self(self, t.dest, depth + 1);
			word.pop_back();
		}
	};
	for (auto *s : nfa.starting()) {
		visit(visit, s, 0);
	}
	if (overflowed) {
		return std::nullopt;
	}
	return words;
}

inline auto printWord(const Word &word) -> std::string
{
	std::ostringstream ostr;

	ostr << "[";
	for (const auto &symbol : word) {
		ostr << " " << symbol;
	}
	ostr << " ]";
	return ostr.str();
}

#endif /* KATER_NFA_TEST_UTILS_HPP */
