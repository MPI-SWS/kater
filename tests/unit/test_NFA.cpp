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
#include "Relation.hpp"
#include "TransLabel.hpp"

#include <gtest/gtest.h>

#include <algorithm>
#include <set>
#include <vector>

/******************************************************************************
 ** Tests for the NFA class itself: state IDs, the mutators, and the combinators
 ******************************************************************************/

/* Returns the IDs of NFA's states, in iteration order */
static auto collectIDs(const NFA &nfa) -> std::vector<unsigned>
{
	std::vector<unsigned> ids;
	ids.reserve(nfa.getNumStates());
	for (const auto &s : nfa.states()) {
		ids.push_back(s->getId());
	}
	return ids;
}

/* Creates COUNT states and returns pointers to them */
static auto createStates(NFA &nfa, unsigned count) -> std::vector<NFA::State *>
{
	std::vector<NFA::State *> states;
	states.reserve(count);
	for (auto i = 0U; i < count; i++) {
		states.push_back(nfa.createState());
	}
	return states;
}

TEST(NFAStateIDs, CreateStateAssignsDenseIDs)
{
	NFA nfa;
	createStates(nfa, 4);

	EXPECT_EQ(collectIDs(nfa), (std::vector<unsigned>{0, 1, 2, 3}));
}

TEST(NFAStateIDs, RemoveStateKeepsIDsDense)
{
	NFA nfa;
	auto states = createStates(nfa, 4);

	nfa.removeState(states[1]);

	/* The state that slid into the erased slot must be renumbered too */
	EXPECT_EQ(collectIDs(nfa), (std::vector<unsigned>{0, 1, 2}));
}

TEST(NFAStateIDs, RemoveStatesIfKeepsIDsDense)
{
	NFA nfa;
	auto states = createStates(nfa, 4);

	nfa.removeStatesIf([&](NFA::State *s) -> bool { return s == states[1] || s == states[3]; });

	EXPECT_EQ(collectIDs(nfa), (std::vector<unsigned>{0, 1}));
}

TEST(NFAStateIDs, RemoveStatesIfHonorsStatefulPredicate)
{
	NFA nfa;
	auto states = createStates(nfa, 3);
	nfa.makeStarting(states[0]);
	nfa.makeAccepting(states[1]);

	/* Ensure removeStatesIf() doesn't erroneously use std::forward.
	 * Captured by value, so the predicate owns state that a move would strip. */
	const std::vector<NFA::State *> doomed{states[1]};
	nfa.removeStatesIf([doomed](NFA::State *s) -> bool {
		return std::ranges::find(doomed, s) != doomed.end();
	});

	EXPECT_EQ(nfa.getNumStates(), 2U);
	EXPECT_EQ(nfa.getNumAccepting(), 0U);
	EXPECT_EQ(collectIDs(nfa), (std::vector<unsigned>{0, 1}));
}

/******************************************************************************
 ** Mutators and combinators
 ******************************************************************************/

/* Every mutator must leave the automaton well formed, and every combinator must
 * compose languages the way its name promises */

/* Words up to this length are compared */
static constexpr unsigned wordLen = 6;

static auto po() -> TransLabel { return makeLabel({}, Relation::po); }
static auto rf() -> TransLabel { return makeLabel({}, Relation::rf); }

/*
 * Accepts exactly the one-letter word LAB.
 *
 *   -->(0) --lab--> ((1))
 */
static auto makeSingleton(const TransLabel &lab) -> NFA
{
	NFA nfa;
	auto *start = nfa.createStarting();
	auto *accept = nfa.createAccepting();
	NFA::addTransition(start, NFA::Transition(lab, accept));
	return nfa;
}

static auto wordsOf(const NFA &nfa) -> std::set<Word>
{
	auto words = acceptedWordsUpTo(nfa, wordLen);
	EXPECT_TRUE(words.has_value());
	return words.value_or(std::set<Word>{});
}

TEST(NFAMutators, AddAndRemoveTransitionKeepTheMirror)
{
	NFA nfa;
	auto *s0 = nfa.createStarting();
	auto *s1 = nfa.createAccepting();

	NFA::addTransition(s0, NFA::Transition(po(), s1));
	EXPECT_TRUE(wellFormed(nfa));
	EXPECT_EQ(s0->getNumOutgoing(), 1U);
	EXPECT_EQ(s1->getNumIncoming(), 1U);

	NFA::removeTransition(s0, NFA::Transition(po(), s1));
	EXPECT_TRUE(wellFormed(nfa));
	EXPECT_EQ(s0->getNumOutgoing(), 0U);
	EXPECT_EQ(s1->getNumIncoming(), 0U);
}

TEST(NFAMutators, SelfTransitionKeepsTheMirror)
{
	NFA nfa;
	auto *s0 = nfa.createStarting();
	nfa.makeAccepting(s0);

	NFA::addSelfTransition(s0, po());

	EXPECT_TRUE(wellFormed(nfa));
	EXPECT_EQ(s0->getNumOutgoing(), 1U);
	EXPECT_EQ(s0->getNumIncoming(), 1U);
}

TEST(NFAMutators, RemoveTransitionsIfKeepsTheMirror)
{
	NFA nfa;
	auto *s0 = nfa.createStarting();
	auto *s1 = nfa.createAccepting();
	NFA::addTransition(s0, NFA::Transition(po(), s1));
	NFA::addTransition(s0, NFA::Transition(rf(), s1));

	nfa.removeTransitionsIf(s0, [](const NFA::Transition &t) -> bool {
		return t.label == makeLabel({}, Relation::po);
	});

	EXPECT_TRUE(wellFormed(nfa));
	EXPECT_EQ(s0->getNumOutgoing(), 1U);
	EXPECT_EQ(s1->getNumIncoming(), 1U);
}

TEST(NFAMutators, RemoveStateDropsTransitionsPointingToIt)
{
	NFA nfa;
	auto *s0 = nfa.createStarting();
	auto *s1 = nfa.createState();
	auto *s2 = nfa.createAccepting();
	NFA::addTransition(s0, NFA::Transition(po(), s1));
	NFA::addTransition(s1, NFA::Transition(rf(), s2));
	NFA::addTransition(s0, NFA::Transition(rf(), s2));

	nfa.removeState(s1);

	/* No survivor may still refer to the removed state */
	EXPECT_TRUE(wellFormed(nfa));
	EXPECT_EQ(nfa.getNumStates(), 2U);
	EXPECT_EQ(wordsOf(nfa), (std::set<Word>{Word{rf()}}));
}

TEST(NFAMutators, FlipReversesTheAutomaton)
{
	NFA nfa;
	auto *s0 = nfa.createStarting();
	auto *s1 = nfa.createAccepting();
	NFA::addTransition(s0, NFA::Transition(po(), s1));
	NFA::addTransition(s1, NFA::Transition(rf(), s1));

	nfa.flip();

	EXPECT_TRUE(wellFormed(nfa));
	EXPECT_EQ(nfa.getNumStarting(), 1U);
	EXPECT_EQ(nfa.getNumAccepting(), 1U);
	/* po;rf reversed is rf(inverse);po(inverse) */
	auto poInv = makeLabel({}, Relation::po, true);
	auto rfInv = makeLabel({}, Relation::rf, true);
	EXPECT_TRUE(acceptsWord(nfa, Word{rfInv, poInv}));
	EXPECT_TRUE(acceptsWord(nfa, Word{poInv}));
}

TEST(NFAMutators, FlipTwiceIsIdentity)
{
	static constexpr unsigned nrStates = 12;
	static constexpr unsigned transitionsPerState = 2;
	static constexpr unsigned acceptPercent = 35;
	static constexpr unsigned seed = 7;

	auto nfa = makeRandomNFA(nrStates, transitionsPerState, acceptPercent, seed);
	auto before = wordsOf(nfa);

	nfa.flip();
	nfa.flip();

	EXPECT_TRUE(wellFormed(nfa));
	EXPECT_EQ(wordsOf(nfa), before);
}

TEST(NFAMutators, SplitStateKeepsTheAutomatonWellFormed)
{
	NFA nfa;
	auto *s0 = nfa.createStarting();
	auto *s1 = nfa.createState();
	auto *s2 = nfa.createAccepting();
	NFA::addTransition(s0, NFA::Transition(po(), s2));
	NFA::addTransition(s1, NFA::Transition(rf(), s2));

	/* s2 keeps its `po` incoming transition; the copy takes the rest */
	auto *copy = nfa.splitState(s2, [](const NFA::Transition &t) -> bool {
		return t.label == makeLabel({}, Relation::po, true);
	});

	EXPECT_TRUE(wellFormed(nfa));
	EXPECT_EQ(nfa.getNumStates(), 4U);
	EXPECT_TRUE(copy->isAccepting());
}

TEST(NFACombinators, AltAcceptsTheUnion)
{
	auto left = makeSingleton(po());
	auto right = makeSingleton(rf());
	auto expected = wordsOf(left);
	for (const auto &word : wordsOf(right)) {
		expected.insert(word);
	}

	auto combined = makeSingleton(po());
	combined.alt(makeSingleton(rf()));

	EXPECT_TRUE(wellFormed(combined));
	EXPECT_EQ(wordsOf(combined), expected);
}

TEST(NFACombinators, SeqAcceptsTheConcatenation)
{
	auto combined = makeSingleton(po());
	combined.seq(makeSingleton(rf()));

	EXPECT_TRUE(wellFormed(combined));
	EXPECT_EQ(wordsOf(combined), (std::set<Word>{Word{po(), rf()}}));
}

TEST(NFACombinators, StarAcceptsTheEmptyWordAndRepetitions)
{
	auto nfa = makeSingleton(po());
	nfa.star();

	EXPECT_TRUE(wellFormed(nfa));
	EXPECT_EQ(nfa.getNumStarting(), 1U);
	EXPECT_EQ(nfa.getNumAccepting(), 1U);
	EXPECT_TRUE(acceptsWord(nfa, Word{}));
	EXPECT_TRUE(acceptsWord(nfa, Word{po()}));
	EXPECT_TRUE(acceptsWord(nfa, Word{po(), po()}));
	EXPECT_FALSE(acceptsWord(nfa, Word{rf()}));
}

TEST(NFACombinators, PlusAcceptsRepetitionsButNotTheEmptyWord)
{
	auto nfa = makeSingleton(po());
	nfa.plus();

	EXPECT_TRUE(wellFormed(nfa));
	EXPECT_FALSE(acceptsWord(nfa, Word{}));
	EXPECT_TRUE(acceptsWord(nfa, Word{po()}));
	EXPECT_TRUE(acceptsWord(nfa, Word{po(), po()}));
}

TEST(NFACombinators, OrEmptyAddsTheEmptyWord)
{
	auto nfa = makeSingleton(po());
	ASSERT_FALSE(acceptsWord(nfa, Word{}));

	nfa.or_empty();

	EXPECT_TRUE(wellFormed(nfa));
	EXPECT_TRUE(acceptsWord(nfa, Word{}));
	EXPECT_TRUE(acceptsWord(nfa, Word{po()}));
}

TEST(NFAStartAccept, ClearingKeepsFlagsAndContainersInSync)
{
	NFA nfa;
	auto *s0 = nfa.createStarting();
	auto *s1 = nfa.createAccepting();
	NFA::addTransition(s0, NFA::Transition(po(), s1));

	nfa.clearStarting(s0);
	EXPECT_TRUE(wellFormed(nfa));
	EXPECT_EQ(nfa.getNumStarting(), 0U);

	nfa.makeStarting(s0);
	/* Marking twice must not duplicate the entry */
	nfa.makeStarting(s0);
	EXPECT_TRUE(wellFormed(nfa));
	EXPECT_EQ(nfa.getNumStarting(), 1U);

	nfa.clearAllAccepting();
	EXPECT_TRUE(wellFormed(nfa));
	EXPECT_EQ(nfa.getNumAccepting(), 0U);
}
