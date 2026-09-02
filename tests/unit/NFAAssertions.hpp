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

#ifndef KATER_NFA_ASSERTIONS_HPP
#define KATER_NFA_ASSERTIONS_HPP

/******************************************************************************
 ** gtest matchers over NFAs: do two automata accept the same words, and is an automaton well formed
 ******************************************************************************/

/* These live apart from NFATestUtils.hpp: kater_bench includes that one and
 * does not link gtest */

#include "NFATestUtils.hpp"

#include <gtest/gtest.h>

#include <algorithm>
#include <ranges>
#include <set>
#include <unordered_set>
#include <vector>

/*
 * Whether A and B accept the same words up to length MAXLEN. We bound the length
 * because the languages are usually infinite; MAXLEN is chosen per test to walk
 * that automaton's cycles more than once.
 */
inline auto sameLanguageUpTo(const NFA &a, const NFA &b, unsigned maxLen)
	-> ::testing::AssertionResult
{
	static constexpr auto maxReported = 5U;

	auto wordsA = acceptedWordsUpTo(a, maxLen);
	auto wordsB = acceptedWordsUpTo(b, maxLen);
	if (!wordsA.has_value() || !wordsB.has_value()) {
		return ::testing::AssertionFailure()
		       << "too many accepted words to enumerate; lower maxLen";
	}
	if (*wordsA == *wordsB) {
		return ::testing::AssertionSuccess();
	}

	auto result = ::testing::AssertionFailure();
	auto report = [&](const std::set<Word> &from, const std::set<Word> &missingIn,
			  const char *who) -> void {
		auto reported = 0U;
		for (const auto &word : from) {
			if (missingIn.contains(word) || reported++ >= maxReported) {
				continue;
			}
			result << "\n  only " << who << " accepts " << printWord(word);
		}
	};
	report(*wordsA, *wordsB, "the first");
	report(*wordsB, *wordsA, "the second");
	result << "\n  (" << wordsA->size() << " vs " << wordsB->size()
	       << " accepted words up to length " << maxLen << ")";
	return result;
}

/* Whether no two outgoing transitions of any state share a label */
inline auto isDeterministic(const NFA &nfa) -> ::testing::AssertionResult
{
	auto result = ::testing::AssertionSuccess();
	auto failed = false;

	for (const auto &s : nfa.states()) {
		std::vector<TransLabel> labels;
		for (const auto &t : s->outs()) {
			labels.push_back(t.label);
		}
		std::ranges::sort(labels);
		auto dup = std::ranges::adjacent_find(labels);
		if (dup == labels.end()) {
			continue;
		}
		if (!failed) {
			failed = true;
			result = ::testing::AssertionFailure();
		}
		result << "\n  state " << s->getId() << " has two transitions labeled " << *dup;
	}
	return result;
}

/*
 * Checks what holds of any NFA whatever the representation: dense state IDs,
 * destinations inside the automaton, an intact outgoing/incoming mirror, and
 * flags agreeing with the start/accept containers.
 */
inline auto wellFormed(const NFA &nfa) -> ::testing::AssertionResult
{
	auto result = ::testing::AssertionSuccess();
	auto failed = false;
	auto fail = [&]() -> ::testing::AssertionResult & {
		if (!failed) {
			failed = true;
			result = ::testing::AssertionFailure();
		}
		return result;
	};

	std::unordered_set<const NFA::State *> known;
	auto id = 0U;
	for (const auto &s : nfa.states()) {
		if (s->getId() != id) {
			fail() << "\n  state at index " << id << " has ID " << s->getId();
		}
		known.insert(&*s);
		++id;
	}

	for (const auto &s : nfa.states()) {
		for (const auto &t : s->outs()) {
			if (!known.contains(t.dest)) {
				fail() << "\n  state " << s->getId()
				       << " has an outgoing transition leaving the NFA";
				continue;
			}
			if (!t.dest->hasIncoming(t.flipTo(&*s))) {
				fail() << "\n  transition " << s->getId() << " --" << t.label
				       << "--> " << t.dest->getId() << " has no mirror in "
				       << t.dest->getId() << "'s incoming set";
			}
		}
		for (const auto &t : s->ins()) {
			if (!known.contains(t.dest)) {
				fail() << "\n  state " << s->getId()
				       << " has an incoming transition leaving the NFA";
				continue;
			}
			if (!t.dest->hasOutgoing(t.flipTo(&*s))) {
				fail() << "\n  incoming transition of " << s->getId() << " from "
				       << t.dest->getId() << " has no mirror in the outgoing set";
			}
		}
	}

	auto starting = nfa.starting();
	auto accepting = nfa.accepting();
	for (const auto &s : nfa.states()) {
		auto isStarting = std::ranges::find(starting, &*s) != starting.end();
		auto isAccepting = std::ranges::find(accepting, &*s) != accepting.end();
		if (s->isStarting() != isStarting) {
			fail() << "\n  state " << s->getId() << ": isStarting()=" << s->isStarting()
			       << " but membership=" << isStarting;
		}
		if (s->isAccepting() != isAccepting) {
			fail() << "\n  state " << s->getId()
			       << ": isAccepting()=" << s->isAccepting()
			       << " but membership=" << isAccepting;
		}
	}
	return result;
}

#endif /* KATER_NFA_ASSERTIONS_HPP */
