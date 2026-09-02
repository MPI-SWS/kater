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

#include "NFAAssertions.hpp"
#include "NFATestUtils.hpp"

#include "NFA.hpp"
#include "NFAUtils.hpp"

#include "Predicate.hpp"
#include "Relation.hpp"
#include "TransLabel.hpp"

#include <gtest/gtest.h>

#include <algorithm>
#include <array>
#include <set>
#include <string>
#include <utility>

/******************************************************************************
 ** Tests for the NFA passes in NFAUtils.cpp: that they keep the language, and the subset
 * construction
 ******************************************************************************/

/* checkall.sh only checks kater's exit status, so a pass that drops a word from
 * an automaton's language would go unnoticed there */

/* Words up to this length are compared; deep enough to walk every automaton's
 * cycles more than once, shallow enough to stay instant */
static constexpr unsigned wordLen = 6;

/* Labels used across the automata below */
static auto po() -> TransLabel { return makeLabel({}, Relation::po); }
static auto rf() -> TransLabel { return makeLabel({}, Relation::rf); }
static auto mo() -> TransLabel { return makeLabel({}, Relation::mo); }
static auto readPo() -> TransLabel { return makeLabel(Predicate::BuiltinID::R, Relation::po); }

/*
 * States 1 and 2 accept the same language, so similarity removal has something
 * to collapse.
 *
 *          ,--po--> (1) --rf--> ((3))
 *   -->(0)-|
 *          `--po--> (2) --rf--> ((4))
 */
static auto makeCollapsible() -> NFA
{
	NFA nfa;
	auto *s0 = nfa.createStarting();
	auto *s1 = nfa.createState();
	auto *s2 = nfa.createState();
	auto *s3 = nfa.createAccepting();
	auto *s4 = nfa.createAccepting();

	NFA::addTransition(s0, NFA::Transition(po(), s1));
	NFA::addTransition(s0, NFA::Transition(po(), s2));
	NFA::addTransition(s1, NFA::Transition(rf(), s3));
	NFA::addTransition(s2, NFA::Transition(rf(), s4));
	return nfa;
}

/*
 * A cycle, plus state 3 which leads nowhere: dead-state removal should drop it.
 *
 *                     ,-----------po----------,
 *                     v                       |
 *   -->(0) --po--> ((1)) --rf--> (2) ---------'
 *       `--mo--> (3)
 */
static auto makeWithDeadEnd() -> NFA
{
	NFA nfa;
	auto *s0 = nfa.createStarting();
	auto *s1 = nfa.createAccepting();
	auto *s2 = nfa.createState();
	auto *s3 = nfa.createState();

	NFA::addTransition(s0, NFA::Transition(po(), s1));
	NFA::addTransition(s1, NFA::Transition(rf(), s2));
	NFA::addTransition(s2, NFA::Transition(po(), s1));
	NFA::addTransition(s0, NFA::Transition(mo(), s3));
	return nfa;
}

/*
 * Both `po` transitions out of state 0 lead somewhere useful, so the subset
 * construction has to merge them.
 *
 *          ,--po--> (1) --rf--> ((3))
 *   -->(0)-|--po--> (2) --mo--> ((3))
 *          `--[R];po----------> ((3))
 */
static auto makeNondeterministic() -> NFA
{
	NFA nfa;
	auto *s0 = nfa.createStarting();
	auto *s1 = nfa.createState();
	auto *s2 = nfa.createState();
	auto *s3 = nfa.createAccepting();

	NFA::addTransition(s0, NFA::Transition(po(), s1));
	NFA::addTransition(s0, NFA::Transition(po(), s2));
	NFA::addTransition(s1, NFA::Transition(rf(), s3));
	NFA::addTransition(s2, NFA::Transition(mo(), s3));
	NFA::addTransition(s0, NFA::Transition(readPo(), s3));
	return nfa;
}

/*
 * Accepts nothing: the only state loops forever and never accepts.
 *
 *        ,-po-,
 *        v    |
 *   -->(0)----'
 */
static auto makeEmptyLanguage() -> NFA
{
	NFA nfa;
	auto *s0 = nfa.createStarting();
	NFA::addTransition(s0, NFA::Transition(po(), s0));
	return nfa;
}

/*
 * Accepts only the empty word: one state, both start and accepting, no
 * transitions.
 *
 *   -->((0))
 */
static auto makeEmptyWordOnly() -> NFA
{
	NFA nfa;
	auto *s0 = nfa.createStarting();
	nfa.makeAccepting(s0);
	return nfa;
}

/* A named automaton to run a pass against */
struct TestAutomaton {
	const char *name;
	NFA (*build)();
};

/* Every pass below is run against all of these */
static const std::array automata = {
	TestAutomaton{.name = "collapsible", .build = makeCollapsible},
	TestAutomaton{.name = "deadEnd", .build = makeWithDeadEnd},
	TestAutomaton{.name = "nondeterministic", .build = makeNondeterministic},
	TestAutomaton{.name = "emptyLanguage", .build = makeEmptyLanguage},
	TestAutomaton{.name = "emptyWordOnly", .build = makeEmptyWordOnly},
};

/* Runs PASS on every automaton above and checks it preserved the language */
template <typename F> static void expectPreservesLanguage(F &&pass)
{
	for (const auto &automaton : automata) {
		SCOPED_TRACE(automaton.name);

		auto original = automaton.build();
		auto reduced = automaton.build();
		std::forward<F>(pass)(reduced);

		EXPECT_TRUE(wellFormed(reduced));
		EXPECT_TRUE(sameLanguageUpTo(original, reduced, wordLen));
	}
}

/******************************************************************************
 ** The test automata, and checks that the oracles themselves can fail
 ******************************************************************************/

TEST(LanguageOracle, EnumeratesTheExpectedWords)
{
	auto nfa = makeCollapsible();

	auto words = acceptedWordsUpTo(nfa, wordLen);
	ASSERT_TRUE(words.has_value());
	/* po;rf is the only accepted word, and it is accepted twice over */
	EXPECT_EQ(*words, (std::set<Word>{Word{po(), rf()}}));
	EXPECT_TRUE(acceptsWord(nfa, Word{po(), rf()}));
	EXPECT_FALSE(acceptsWord(nfa, Word{po()}));
	EXPECT_FALSE(acceptsWord(nfa, Word{}));
}

TEST(LanguageOracle, DistinguishesDifferentLanguages)
{
	auto accepting = makeEmptyWordOnly();
	auto rejecting = makeEmptyLanguage();

	EXPECT_TRUE(acceptsWord(accepting, Word{}));
	EXPECT_FALSE(acceptsWord(rejecting, Word{}));
	/* The oracle must report a difference, or every test below is vacuous */
	EXPECT_FALSE(sameLanguageUpTo(accepting, rejecting, wordLen));
}

TEST(LanguageOracle, DetectsNondeterminismAndMalformedness)
{
	/* These two matchers must be able to fail, or every use of them below
	 * passes for the wrong reason */
	auto nondet = makeNondeterministic();
	EXPECT_FALSE(isDeterministic(nondet));
	EXPECT_TRUE(isDeterministic(nondet.to_DFA().first));

	/* A transition whose destination belongs to a different automaton is
	 * exactly what an index-based representation would get wrong */
	NFA host;
	NFA other;
	auto *inHost = host.createStarting();
	auto *inOther = other.createAccepting();
	NFA::addTransition(inHost, NFA::Transition(po(), inOther));
	EXPECT_FALSE(wellFormed(host));
}

/******************************************************************************
 ** The minimization passes
 ******************************************************************************/

TEST(RemoveDeadStates, PreservesLanguage)
{
	expectPreservesLanguage([](NFA &nfa) -> void { removeDeadStates(nfa); });
}

TEST(RemoveDeadStates, DropsTheDeadEnd)
{
	auto nfa = makeWithDeadEnd();
	const auto before = nfa.getNumStates();

	removeDeadStates(nfa);

	EXPECT_LT(nfa.getNumStates(), before);
	EXPECT_TRUE(wellFormed(nfa));
}

TEST(RemoveSimilarTransitions, PreservesLanguage)
{
	expectPreservesLanguage([](NFA &nfa) -> void { removeSimilarTransitions(nfa); });
}

TEST(RemoveRedundantSelfLoops, PreservesLanguage)
{
	expectPreservesLanguage([](NFA &nfa) -> void { removeRedundantSelfLoops(nfa); });
}

TEST(MinimizationPipeline, PreservesLanguage)
{
	/* The order simplify() uses, minus the label-rewriting passes */
	expectPreservesLanguage([](NFA &nfa) -> void {
		removeDeadStates(nfa);
		removeSimilarTransitions(nfa);
		removeDeadStates(nfa);
		removeDeadStates(nfa);
		removeSimilarTransitions(nfa);
		removeDeadStates(nfa);
	});
}

/******************************************************************************
 ** The subset construction
 ******************************************************************************/

TEST(ToDFA, IsDeterministic)
{
	for (const auto &automaton : automata) {
		SCOPED_TRACE(automaton.name);

		auto nfa = automaton.build();
		auto dfa = nfa.to_DFA().first;

		EXPECT_TRUE(isDeterministic(dfa));
		EXPECT_TRUE(wellFormed(dfa));
	}
}

TEST(ToDFA, PreservesLanguage)
{
	for (const auto &automaton : automata) {
		SCOPED_TRACE(automaton.name);

		auto nfa = automaton.build();
		auto dfa = nfa.to_DFA().first;

		EXPECT_TRUE(sameLanguageUpTo(nfa, dfa, wordLen));
	}
}

TEST(ToDFA, LeavesTheSourceAlone)
{
	auto nfa = makeNondeterministic();
	const auto states = nfa.getNumStates();
	auto words = acceptedWordsUpTo(nfa, wordLen);

	(void)nfa.to_DFA();

	EXPECT_EQ(nfa.getNumStates(), states);
	EXPECT_TRUE(wellFormed(nfa));
	EXPECT_EQ(acceptedWordsUpTo(nfa, wordLen), words);
}

TEST(ToDFA, MapsEveryDFAStateToNFAStates)
{
	auto nfa = makeNondeterministic();

	auto [dfa, dfaToNfa] = nfa.to_DFA();

	/* Every state of the DFA stands for a non-empty subset of NFA states,
	 * addressed by the DFA state's own ID and given as sorted NFA state IDs */
	ASSERT_EQ(dfaToNfa.size(), dfa.getNumStates());
	for (const auto &s : dfa.states()) {
		const auto &subset = dfaToNfa[s->getId()];
		EXPECT_FALSE(subset.empty()) << "DFA state " << s->getId() << " stands for nothing";
		EXPECT_TRUE(std::ranges::is_sorted(subset));
		EXPECT_TRUE(std::ranges::adjacent_find(subset) == subset.end())
			<< "subset of DFA state " << s->getId() << " has duplicates";
		for (auto id : subset) {
			EXPECT_LT(id, nfa.getNumStates());
		}
	}
}

/******************************************************************************
 ** Random inputs, where the hand-written automata run out
 ******************************************************************************/

TEST(RandomNFAs, MinimizationPreservesLanguage)
{
	/* Small enough that the accepted-word set stays enumerable, and biased
	 * towards accepting states so the languages are not trivially empty */
	static constexpr unsigned nrStates = 12;
	static constexpr unsigned transitionsPerState = 2;
	static constexpr unsigned acceptPercent = 35;
	static constexpr unsigned randomWordLen = 4;
	static constexpr unsigned nrSeeds = 25;

	for (auto seed = 1U; seed <= nrSeeds; seed++) {
		SCOPED_TRACE("seed " + std::to_string(seed));

		auto original = makeRandomNFA(nrStates, transitionsPerState, acceptPercent, seed);
		ASSERT_TRUE(wellFormed(original));

		/* Same seed, same automaton: NFA is not copyable */
		auto reduced = makeRandomNFA(nrStates, transitionsPerState, acceptPercent, seed);
		removeDeadStates(reduced);
		removeSimilarTransitions(reduced);

		EXPECT_TRUE(wellFormed(reduced));
		EXPECT_TRUE(sameLanguageUpTo(original, reduced, randomWordLen));
	}
}

TEST(RandomNFAs, ToDFAPreservesLanguage)
{
	static constexpr unsigned nrStates = 10;
	static constexpr unsigned transitionsPerState = 2;
	static constexpr unsigned acceptPercent = 35;
	static constexpr unsigned randomWordLen = 4;
	static constexpr unsigned nrSeeds = 25;

	for (auto seed = 1U; seed <= nrSeeds; seed++) {
		SCOPED_TRACE("seed " + std::to_string(seed));

		auto nfa = makeRandomNFA(nrStates, transitionsPerState, acceptPercent, seed);
		auto dfa = nfa.to_DFA().first;

		EXPECT_TRUE(isDeterministic(dfa));
		EXPECT_TRUE(wellFormed(dfa));
		EXPECT_TRUE(sameLanguageUpTo(nfa, dfa, randomWordLen));
	}
}

/******************************************************************************
 ** copy()
 ******************************************************************************/

TEST(Copy, ProducesAnIndependentEqualAutomaton)
{
	for (const auto &automaton : automata) {
		SCOPED_TRACE(automaton.name);

		auto original = automaton.build();
		auto duplicate = copy(original);

		EXPECT_TRUE(wellFormed(duplicate));
		EXPECT_EQ(duplicate.getNumStates(), original.getNumStates());
		EXPECT_EQ(duplicate.getNumStarting(), original.getNumStarting());
		EXPECT_EQ(duplicate.getNumAccepting(), original.getNumAccepting());
		EXPECT_TRUE(sameLanguageUpTo(original, duplicate, wordLen));

		/* State i of the copy stands for state i of the source, and is a
		 * genuinely different state carrying the same flags */
		for (const auto &s : original.states()) {
			auto *copied = duplicate[s->getId()];
			EXPECT_NE(copied, &*s);
			EXPECT_EQ(copied->isStarting(), s->isStarting());
			EXPECT_EQ(copied->isAccepting(), s->isAccepting());
			EXPECT_EQ(copied->getNumOutgoing(), s->getNumOutgoing());
		}

		/* Mutating the copy must not touch the original */
		auto words = acceptedWordsUpTo(original, wordLen);
		duplicate.removeStatesIf([](NFA::State *s) -> bool { return s->isAccepting(); });
		EXPECT_EQ(acceptedWordsUpTo(original, wordLen), words);
		EXPECT_TRUE(wellFormed(original));
	}
}
