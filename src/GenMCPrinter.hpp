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

#include "CodeBuilder.hpp"
#include "NFA.hpp"
#include "Printer.hpp"
#include "Statement.hpp"

#include <cstdint>
#include <fstream>
#include <iostream>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

class KatModule;
class Config;
enum class TraversalKind : std::uint8_t;
struct DFSParameters;
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
	GenMCPrinter(const GenMCPrinter &) = default;
	GenMCPrinter(GenMCPrinter &&) = delete;
	virtual ~GenMCPrinter() = default;

	auto operator=(const GenMCPrinter &) -> GenMCPrinter & = default;
	auto operator=(GenMCPrinter &&) -> GenMCPrinter & = delete;

	/** Outputs consistency checking routines conforming to GenMC's API */
	auto output() -> bool override;

private:
	auto printPredSet(const std::string &arg, const PredicateSet &ps) -> std::string;

	auto printRelation(const std::string &res, const std::string &arg,
			   const std::optional<Relation> &r) -> std::string;
	auto printTransLabel(const NFA::Transition &t, const std::string &res,
			     const std::string &arg, const std::string &saveRes = {})
		-> std::string;

	auto getPrintedIdx(const LetStatement *let) const -> unsigned
	{
		return letToIdxMap.at(let);
	}
	auto isMutRecRelation(Relation::ID id) const -> bool
	{
		return mutRecRelToLetMap.contains(id);
	}
	auto getMutRecRelationDeclaration(Relation::ID id) const -> const LetStatement *
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
	void printConsistency(const std::unordered_map<Statement *, std::string> &exportedNames);
	void printCacheCounters();
	void printFooter();

	void outputTraversal(const NFA &nfa, const DFSParameters &params, TraversalKind kind);

	[[nodiscard]] auto shouldPrintSuccAcycChecks() const -> bool;

	/* Potentially expensive check that returns true if the
	 * model acyclicity axioms utilize rf-1 */
	[[nodiscard]] auto usesRfInvInAcycChecks() const -> bool;

	[[nodiscard]] auto getStateVisitAssignment(const NFA &nfa) const -> std::vector<char>;

	auto hppB() -> CodeBuilder & { return *hppB_; }
	auto cppB() -> CodeBuilder & { return *cppB_; }

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

	/* Indent-aware builders wrapping outHpp / outCpp. Initialized in
	 * the ctor body once stream pointers are wired. */
	std::optional<CodeBuilder> hppB_;
	std::optional<CodeBuilder> cppB_;

	std::unordered_map<Relation::ID, const LetStatement *> mutRecRelToLetMap;
	std::unordered_map<const LetStatement *, unsigned> letToIdxMap;

	/* One per `[P];any;[P] <= id` export whose check is cached as a `P`-count. */
	struct CacheCounter {
		std::string prefix;    /* names recompute<prefix>() and cacheCounter<prefix> */
		std::string predCheck; /* `if (...)` guard selecting a counted label */
	};
	std::vector<CacheCounter> cacheCounters{};
};

#endif /* KATER_GENMC_PRINTER_HPP */
