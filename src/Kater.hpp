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

#ifndef KATER_KATER_HPP
#define KATER_KATER_HPP

#include "Config.hpp"
#include "KatModule.hpp"

/*
 * Given a KatModule resulted from parsing, transforms all regexps to
 * NFAs that will be used for code printing
 */
class Kater {

public:
	Kater() = delete;
	Kater(const Config &conf, std::unique_ptr<KatModule> mod)
		: config(conf), module(std::move(mod))
	{
	}

	[[nodiscard]] auto getModule() const -> const KatModule & { return *module; }
	auto getModule() -> KatModule & { return *module; }

	/* Check any user assertions and report errors.
	 * Returns whether any assertion was violated */
	auto checkAssertions() -> bool;

	auto exportCode() -> bool;

private:
	struct InclusionResult {
		bool result{};
		Counterexample cex{};
	};

	[[nodiscard]] auto getConf() const -> const Config & { return config; }

	auto isPPOIntersectionInPPO(const AcyclicConstraint *acyc) const -> InclusionResult;
	auto isDFASubLanguageOfNFA(NFA &nfa, const NFA &other) const -> InclusionResult;
	[[nodiscard]] auto checkInclusion(SubsetConstraint &subsetC) const -> InclusionResult;
	auto checkAssertion(Constraint &cst) -> InclusionResult;

	void optimizeModuleForExport();
	auto checkExportRequirements() -> bool;

	void printCounterexample(const Counterexample &cex) const;

	const Config &config;
	std::unique_ptr<KatModule> module;
};

#endif /* KATER_KATER_HPP */
