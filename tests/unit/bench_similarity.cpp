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

/* Times findSimilarStates() against the reference implementation. Correctness is
 * the unit tests' job, so this lives outside the `unit` label: run it with
 * `ctest -L benchmark`, or directly to pass a size */

#include "NFATestUtils.hpp"
#include "NFAUtils.hpp"

#include <chrono>
#include <cstddef>
#include <cstdlib>
#include <iostream>
#include <ratio>
#include <span>

constexpr unsigned defaultStates = 4661;
constexpr unsigned defaultTransitionsPerState = 14;
constexpr int decimal = 10;

static auto milliseconds(auto work) -> double
{
	const auto start = std::chrono::steady_clock::now();
	work();
	const auto end = std::chrono::steady_clock::now();
	return std::chrono::duration<double, std::milli>(end - start).count();
}

static auto parseCount(const char *arg) -> unsigned
{
	return static_cast<unsigned>(std::strtoul(arg, nullptr, decimal));
}

auto main(int argc, char **argv) -> int
{
	const auto args = std::span(argv, static_cast<size_t>(argc));
	auto nrStates = defaultStates;
	auto transitionsPerState = defaultTransitionsPerState;

	if (args.size() > 1) {
		nrStates = parseCount(args[1]);
	}
	if (args.size() > 2) {
		transitionsPerState = parseCount(args[2]);
	}
	if (nrStates == 0 || transitionsPerState == 0) {
		std::cerr << "usage: " << args[0] << " [states] [transitions-per-state]\n";
		return 1;
	}

	auto nfa = makeRandomNFA(nrStates, transitionsPerState);
	std::cout << "NFA: " << nrStates << " states, " << transitionsPerState
		  << " transitions/state\n";

	const auto reference =
		milliseconds([&]() -> void { (void)findSimilarStatesReference(nfa); });
	const auto optimized = milliseconds([&]() -> void { (void)findSimilarStates(nfa); });

	std::cout << "reference: " << reference << " ms\n"
		  << "optimized: " << optimized << " ms\n";
	if (optimized > 0.0) {
		std::cout << "speedup:   " << (reference / optimized) << "x\n";
	}
	return 0;
}
