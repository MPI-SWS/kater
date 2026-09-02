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

#include "NFA.hpp"
#include "NFATestUtils.hpp"
#include "NFAUtils.hpp"
#include "Predicate.hpp"
#include "Relation.hpp"

#include <gtest/gtest.h>

#include <functional>
#include <ostream>
#include <sstream>
#include <string>
#include <vector>

/* Cross-checks the optimized similarity computation against the reference one */
static void expectMatchesReference(NFA &nfa)
{
	const auto nrStates = static_cast<unsigned>(nfa.getNumStates());
	const auto actual = findSimilarStates(nfa);
	const auto expected = findSimilarStatesReference(nfa);
	if (actual == expected) {
		return;
	}

	std::stringstream pairs;
	for (auto i = 0U; i < nrStates; i++) {
		for (auto j = 0U; j < nrStates; j++) {
			if (actual[(i * nrStates) + j] != expected[(i * nrStates) + j]) {
				pairs << " (" << i << "," << j << ")";
			}
		}
	}
	ADD_FAILURE() << "similar() differs from the reference for state pairs:" << pairs.str();
}

/* The fixtures below are shaped to be awkward for similarity: mutually reachable
 * states with equally-labeled transitions. Which relation and predicate they use
 * is arbitrary; only which labels coincide matters.
 *
 * In the drawings, `d` is the relation, `[R]d` and `d[R]` carry a pre- and a
 * post-check respectively, `>` marks a starting and `*` an accepting state */

/*
 * s1 and s2 reach the same three states, and differ only in that one of s1's
 * edges into s2 carries a pre-check that s2 has no counterpart for.
 *
 *       s0 --[R]d--> s1
 *
 *       s1 --d-----> s0      s2 --d--> s0
 *       s1 --d-----> s2      s2 --d--> s1
 *       s1 --[R]d--> s2      s2 --d--> s2  (self)
 */
static auto makePreCheckNFA() -> NFA
{
	NFA nfa;
	auto *s0 = nfa.createState();
	auto *s1 = nfa.createState();
	auto *s2 = nfa.createState();

	NFA::addTransition(s1, NFA::Transition(makeLabel({}, Relation::detour), s0));
	NFA::addTransition(s1, NFA::Transition(makeLabel({}, Relation::detour), s2));
	NFA::addTransition(
		s1, NFA::Transition(makeLabel(Predicate::BuiltinID::REL, Relation::detour), s2));

	NFA::addTransition(
		s0, NFA::Transition(makeLabel(Predicate::BuiltinID::REL, Relation::detour), s1));

	NFA::addTransition(s2, NFA::Transition(makeLabel({}, Relation::detour), s0));
	NFA::addTransition(s2, NFA::Transition(makeLabel({}, Relation::detour), s1));
	NFA::addTransition(s2, NFA::Transition(makeLabel({}, Relation::detour), s2));

	return nfa;
}

/*
 * s0 and s2 each have exactly one edge into s1, so they are similar unless the
 * inverse is distinguished from the relation itself.
 *
 *       s0 ---rf---> s1 <---rf^-1--- s2
 */
static auto makeInverseLabelNFA() -> NFA
{
	NFA nfa;
	auto *s0 = nfa.createState();
	auto *s1 = nfa.createState();
	auto *s2 = nfa.createState();

	NFA::addTransition(s0, NFA::Transition(makeLabel({}, Relation::rf), s1));
	NFA::addTransition(s2, NFA::Transition(makeLabel({}, Relation::rf, true), s1));

	return nfa;
}

/*
 * s3, s5 and s7 have byte-identical outgoing edge sets, so they are similar;
 * s6 is not, and the s6/s5 edge feeds that back into the s6-s7 cycle.
 *
 *       s3 --d[R]--> s1*     s5 --d[R]--> s1*     s7 --d[R]--> s1*
 *       s3 --d-----> s6      s5 --d-----> s6      s7 --d-----> s6
 *       s3 --d-----> s7      s5 --d-----> s7      s7 --d-----> s7  (self)
 *       >s3                  >s5
 *
 *       s6 --[R]d[R]--> s1*
 *       s6 --[R]d-----> s5
 *
 *       >s0, s2, s4 are isolated
 */
static auto makeCyclesNFA() -> NFA
{
	NFA nfa;
	nfa.createStarting(); /* s0, isolated */
	auto *s1 = nfa.createAccepting();
	nfa.createState(); /* s2, isolated */
	auto *s3 = nfa.createStarting();
	nfa.createState(); /* s4, isolated */
	auto *s5 = nfa.createStarting();
	auto *s6 = nfa.createState();
	auto *s7 = nfa.createState();

	const auto toAccepting = makeLabel({}, Relation::detour, false, Predicate::BuiltinID::REL);
	const auto plain = makeLabel({}, Relation::detour);
	const auto checked = makeLabel(Predicate::BuiltinID::REL, Relation::detour);

	NFA::addTransition(s3, NFA::Transition(toAccepting, s1));
	NFA::addTransition(s3, NFA::Transition(plain, s6));
	NFA::addTransition(s3, NFA::Transition(plain, s7));

	NFA::addTransition(s5, NFA::Transition(toAccepting, s1));
	NFA::addTransition(s5, NFA::Transition(plain, s6));
	NFA::addTransition(s5, NFA::Transition(plain, s7));

	NFA::addTransition(s6,
			   NFA::Transition(makeLabel(Predicate::BuiltinID::REL, Relation::detour,
						     false, Predicate::BuiltinID::REL),
					   s1));
	NFA::addTransition(s6, NFA::Transition(checked, s5));

	NFA::addTransition(s7, NFA::Transition(toAccepting, s1));
	NFA::addTransition(s7, NFA::Transition(plain, s6));
	NFA::addTransition(s7, NFA::Transition(plain, s7));

	return nfa;
}

/*
 * Small enough to work through by hand: s0 and s2 form a two-cycle that the
 * accepting state also feeds into, and s3's self-loop keeps it in play.
 *
 *       >s2 --d[R]--> s0     s0 --d--> s2
 *
 *       s3* --d----> s0
 *       s3* --d----> s2
 *       s3* --d----> s3  (self)
 *
 *       >s1 is isolated
 */
static auto makeSelfLoopNFA() -> NFA
{
	NFA nfa;
	auto *s0 = nfa.createState();
	nfa.createStarting(); /* s1, isolated */
	auto *s2 = nfa.createStarting();
	auto *s3 = nfa.createAccepting();

	const auto plain = makeLabel({}, Relation::detour);

	NFA::addTransition(
		s2, NFA::Transition(
			    makeLabel({}, Relation::detour, false, Predicate::BuiltinID::REL), s0));
	NFA::addTransition(s3, NFA::Transition(plain, s2));
	NFA::addTransition(s3, NFA::Transition(plain, s3));
	NFA::addTransition(s3, NFA::Transition(plain, s0));
	NFA::addTransition(s0, NFA::Transition(plain, s2));

	return nfa;
}

struct NFAFactory {
	std::string name;
	std::function<NFA()> make;
};

/* Keeps gtest from dumping the std::function's bytes into the test name */
static void PrintTo(const NFAFactory &factory, std::ostream *os) { *os << factory.name; }

class FindSimilarStatesTest : public ::testing::TestWithParam<NFAFactory> {};

TEST_P(FindSimilarStatesTest, MatchesReference)
{
	auto nfa = GetParam().make();

	expectMatchesReference(nfa);
}

// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
INSTANTIATE_TEST_SUITE_P(NFASimilaritySuite, FindSimilarStatesTest,
			 ::testing::Values(NFAFactory{"PreChecks", makePreCheckNFA},
					   NFAFactory{"InverseLabels", makeInverseLabelNFA},
					   NFAFactory{"Cycles", makeCyclesNFA},
					   NFAFactory{"SelfLoop", makeSelfLoopNFA}),
			 [](const ::testing::TestParamInfo<NFAFactory> &info) -> std::string {
				 return info.param.name;
			 });

/* Exercises far more label and state combinations than the fixtures above; see
 * kater_bench for the timings */
TEST(FindSimilarStates, MatchesReferenceOnRandomNFA)
{
	static constexpr unsigned nrStates = 400;
	static constexpr unsigned transitionsPerState = 14;

	auto nfa = makeRandomNFA(nrStates, transitionsPerState);
	expectMatchesReference(nfa);
}
