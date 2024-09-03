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

#ifndef KATER_GENMC_PRINTER_HPP
#define KATER_GENMC_PRINTER_HPP

#include "NFA.hpp"
#include "Printer.hpp"

#include <fstream>
#include <iostream>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

class KatModule;
class Config;
class DFSParameters;
class PredicateSet;
class Constraint;
class SubsetConstraint;
class AcyclicConstraint;
class WarningConstraint;
class CoherenceConstraint;
class LetStatement;

class GenMCPrinter : public Printer {

public:
	GenMCPrinter(const KatModule &module, const Config &conf);

	/** Outputs consistency checking routines conforming to GenMC's API */
	void output() override;

private:
	void printPredSet(std::ostream &ostr, const std::string &arg, const PredicateSet &ps);

	void printRelation(std::ostream &ostr, const std::string &res, const std::string &arg,
			   const std::optional<Relation> &r);
	void printTransLabel(std::ostream &ostr, const NFA::Transition &t, const std::string &res,
			     const std::string &arg, const std::string &saveRes = {});

	static auto getPrintedIdx(const LetStatement *let) -> unsigned { return letToIdxMap[let]; }
	static auto isMutRecRelation(Relation::ID id) -> bool
	{
		return mutRecRelToLetMap.contains(id);
	}
	static auto getMutRecRelationDeclaration(Relation::ID id) -> const LetStatement *
	{
		return mutRecRelToLetMap.at(id);
	}

	void printCalculator(const LetStatement *let, std::string prefix, std::string name);

	void printUnless(const Constraint *cst, std::string prefix, bool counterexample);
	void printSubset(const SubsetConstraint *cst, std::string prefix,
			 bool counterexample = false);
	void printAcyclic(const AcyclicConstraint *cst, std::string prefix);
	void printWarning(const WarningConstraint *cst, std::string prefix);
	void printCoherence(const CoherenceConstraint *cst);

	void printPPoRfHpp(const NFA &nfa, bool deps);
	void printPPoRfCpp(const NFA &nfa, bool deps);

	void printHeader();
	void printFooter();

	void outputDFSCode(const NFA &nfa, const DFSParameters &params);
	void outputSCCCode(const NFA &nfa, const DFSParameters &params);

	[[nodiscard]] auto shouldPrintSuccAcycChecks() const -> bool;

	/* Potentially expensive check that returns true if the
	 * model acyclicity axioms utilize rf-1 */
	[[nodiscard]] auto usesRfInvInAcycChecks() const -> bool;

	[[nodiscard]] auto getStateVisitAssignment(const NFA &nfa) const
		-> std::unordered_map<NFA::State *, bool>;

	auto hpp() -> std::ostream & { return *outHpp; }
	auto cpp() -> std::ostream & { return *outCpp; }

	/* Class name for resulting files */
	std::string className;

	/* Include-guard name for resulting files */
	std::string guardName;

	/* Streams for the header file */
	std::ofstream foutHpp; /* only set if we're writing to a file */
	std::ostream *outHpp = &std::cout;

	/* Streams for the implementation file */
	std::ofstream foutCpp; /* only set if we're writing to a file */
	std::ostream *outCpp = &std::cout;

	static std::unordered_map<Relation::ID, const LetStatement *> mutRecRelToLetMap;
	static std::unordered_map<const LetStatement *, unsigned> letToIdxMap;
	std::optional<unsigned> cohIndex{};
	std::optional<unsigned> hbIndex{};
};

#endif /* KATER_GENMC_PRINTER_HPP */
