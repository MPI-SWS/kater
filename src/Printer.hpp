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

#ifndef KATER_PRINTER_HPP
#define KATER_PRINTER_HPP

#include "CNFAs.hpp"
#include <fstream>
#include <iostream>
#include <optional>
#include <string>
#include <vector>

class Printer {

private:
	struct RelationOut {
		std::string succ;
		std::string pred;
	};

public:
	Printer(const std::string &dirPrefix, const std::string &outPrefix);

	/* Outputs RES */
	void output(const CNFAs &cnfas);

private:
//	void printInclusionError(const std::string &s, const NFA &lhs, const NFA &rhs);
//	void printInclusionWarning(const std::string &s, const NFA &lhs, const NFA &rhs);

	static void printPredSet(std::ostream &ostr, const std::string &arg,
			  const PredicateSet &ps);

	void printRelation(std::ostream& ostr, const std::string &res,
			   const std::string &arg, const TransLabel *r);
	void printTransLabel(const TransLabel *t, const std::string &res, const std::string &arg);

	static auto getCalcIdx(unsigned id) -> unsigned { return calcToIdxMap[id]; }

	void printCalculatorHpp(const NFA &nfa, unsigned id, VarStatus status);
	void printCalculatorCpp(const NFA &nfa, unsigned id, VarStatus status);

	void printInclusionHpp(const NFA &lhs, const NFA &rhs, unsigned id,
			       std::optional<unsigned> rhsViewIdx);
	void printInclusionCpp(const NFA &lhs, const NFA &rhs, unsigned id,
			       std::optional<unsigned> rhsViewIdx);

	void printAcyclicHpp(const NFA &nfa);
	void printAcyclicCpp(const NFA &nfa);

	void printRecoveryHpp(const NFA &nfa);
	void printRecoveryCpp(const NFA &nfa);

	void printPPoRfHpp(const NFA &nfa, bool deps);
	void printPPoRfCpp(const NFA &nfa, bool deps);

	void printHppHeader();
	void printCppHeader();

	void printHppFooter();
	void printCppFooter();

	void outputHpp(const CNFAs &nfas);
	void outputCpp(const CNFAs &cnfas);

	auto hpp() -> std::ostream & { return *outHpp; }
	auto cpp() -> std::ostream & { return *outCpp; }

	/* Prefix for the names to be printed (class name, filenames, etc) */
	std::string prefix;

	/* Class name for resulting files */
	std::string className;

	/* Include-guard name for resulting files */
	std::string guardName;

	/* Streams for the header file */
	std::ofstream foutHpp; /* only set if we're writing to a file */
	std::ostream* outHpp = &std::cout;

	/* Streams for the implementation file */
	std::ofstream foutCpp; /* only set if we're writing to a file */
	std::ostream* outCpp = &std::cout;

	static const std::unordered_map<Relation::Builtin, RelationOut> relationNames;
	static const std::unordered_map<PredicateMask, std::string> predicateNames;

	static std::vector<unsigned> calcToIdxMap;
	std::unordered_set<unsigned> viewCalcs;
};

#endif /* KATER_PRINTER_HPP */
