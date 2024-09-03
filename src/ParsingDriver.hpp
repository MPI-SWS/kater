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

#ifndef PARSING_DRIVER_HPP
#define PARSING_DRIVER_HPP

#include "Config.hpp"
#include "KatModule.hpp"
#include "Parser.hpp"
#include "TransLabel.hpp"
#include "Utils.hpp"
#include <algorithm>
#include <memory>
#include <ostream>
#include <ranges>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#define YY_DECL yy::parser::symbol_type yylex(ParsingDriver &drv)
YY_DECL;

class ParsingDriver {

private:
	struct State {
		State(yy::location loc, FILE *in, std::string dir, std::string prefix)
			: loc(loc), in(in), dir(std::move(dir)), prefix(std::move(prefix))
		{
		}

		yy::location loc;
		FILE *in;
		std::string dir;
		std::string prefix;
	};

public:
	ParsingDriver();

	auto getLocation() -> yy::location & { return location; }

	auto getRegisteredREOrCreateTmpRec(const std::string id, const yy::location &loc)
		-> std::unique_ptr<RegExp>
	{
		auto getMaybeQualifiedRE = [&](auto &id) {
			auto e = getModule()->getRegisteredRE(getQualifiedName(id));
			if (e)
				return e->clone();
			e = getModule()->getRegisteredRE(id);
			return e ? e->clone() : nullptr;
		};

		// If regexp does not exist, register it, but keep
		// track of it to ensure it's used in let rec
		auto re = getMaybeQualifiedRE(id);
		if (!re) {
			registerRelation(id, "", loc);
			recRels_.insert(findRegisteredRE(id));
			return findRegisteredRE(id)->clone();
		}

		// If already registered, ensure it's not hidden
		auto *charRE = dynamic_cast<const CharRE *>(&*re);
		if (!charRE)
			return re;
		auto &relOpt = charRE->getLabel().getRelation();
		auto &theory = getModule()->getTheory();
		if (relOpt.has_value() && theory.hasInfo(*relOpt) &&
		    theory.getInfo(*relOpt).hidden) {
			std::cerr << loc << ": ";
			std::cerr << "forbidden use of internal relation (" << id << ")\n";
			exit(EPARSE);
		}
		return re;
	}

	auto getRegisteredIDAsPredicate(const std::string &id, const yy::location &loc) -> Predicate
	{
		const auto *p = findRegisteredRE(id);
		if (!p) {
			std::cerr << loc << ": ";
			std::cerr << "undeclared predicate used in disjoint clause (" << id
				  << ")\n";
			exit(EPARSE);
		}
		if (!p->isPredicate()) {
			std::cerr << loc << ": ";
			std::cerr << "non-basic predicate used in disjoint clause (" << id << ")\n";
			exit(EPARSE);
		}
		const auto *charRE = dynamic_cast<const CharRE *>(p);
		const auto &lab = charRE->getLabel();
		assert(!lab.hasPostChecks());
		assert(lab.isPredicate());
		assert(lab.getPreChecks().size() == 1);
		return *charRE->getLabel().getPreChecks().begin();
	}

	void registerRelation(const std::string &id, const std::string &printID,
			      const yy::location &loc)
	{
		checkRelationDeclaration(id, printID, loc);

		RelationInfo info;
		info.name = getQualifiedName(id);
		info.genmc.succ = printID;
		info.genmc.pred = printID;
		info.dbg = {loc.end.filename, loc.end.line};
		getModule()->registerRelation(Relation::createUser(), std::move(info));
	}

	void registerPredicate(const std::string &id, const std::string &printID,
			       const yy::location &loc)
	{
		checkPredicateDeclaration(id, printID, loc);

		PredicateInfo info;
		info.name = getQualifiedName(id);
		info.genmc = printID;
		info.dbg = {loc.end.filename, loc.end.line};
		getModule()->registerPredicate(Predicate::createUser(), std::move(info));
	}

	void registerDerived(std::unique_ptr<LetStatement> let, const yy::location &loc);

	void registerViewDerived(std::unique_ptr<LetStatement> let, const yy::location &loc)
	{
		let->setName(getQualifiedName(let->getName()));
		checkDerivedDeclaration(let->getName(), let->getRE(), loc);
		getModule()->registerLet(std::move(let));
	}

	void registerRecDerived(std::vector<std::pair<std::string, std::unique_ptr<RegExp>>> defs,
				const yy::location &loc);

	void
	registerRecViewDerived(std::vector<std::pair<std::string, std::unique_ptr<RegExp>>> defs,
			       const yy::location &loc);

	// Handle "assert c" declaration in the input file
	void registerAssert(std::unique_ptr<AssertStatement> assrt)
	{
		getModule()->registerAssert(std::move(assrt));
	}

	// Handle "assume c" declaration in the input file
	void registerAssume(std::unique_ptr<AssumeStatement> assm)
	{
		getModule()->getTheory().registerAssume(std::move(assm));
	}

	void registerDisjointPreds(VSet<Predicate::ID> disjoint)
	{
		getModule()->getTheory().registerDisjointPreds(std::move(disjoint));
	}

	// Handle consistency constraint in the input file
	void registerExport(std::unique_ptr<ExportStatement> exp, const yy::location &loc)
	{
		auto *cohCst = dynamic_cast<CoherenceConstraint *>(exp->getConstraint());
		if (cohCst && !findRegisteredStatement(cohCst->getID())) {
			std::cerr << loc << ": ";
			std::cerr << "Uknown relation used for coherence constraint\n";
			exit(EPARSE);
		}
		if (cohCst && exp->isExtra()) {
			std::cerr << loc << ": ";
			std::cerr << "extra coherence constraints are unsupported\n";
			exit(EPARSE);
		}
		if (cohCst && getModule()->getCOHDeclaration()) {
			std::cerr << loc << ": ";
			std::cerr << "Only one coherence constraint is supported\n";
			exit(EPARSE);
		}
		/* Fix the name for the constraint --- maybe needs qualification */
		if (cohCst) {
			cohCst->setID(findRegisteredStatement(cohCst->getID())->getName());
		}
		getModule()->registerExport(std::move(exp), loc);
	}

	/* Invoke the parser on INPUT. Return 0 on success. */
	auto parse(const std::string &input) -> int;

	auto takeModule() -> std::unique_ptr<KatModule> { return std::move(module_); }

private:
	[[nodiscard]] auto tmp_recs() const { return std::ranges::ref_view(recRels_); }

	[[nodiscard]] auto getModule() const -> const KatModule * { return module_.get(); }
	auto getModule() -> KatModule * { return module_.get(); }

	[[nodiscard]] auto getPrefix() const -> const std::string & { return prefix; }

	[[nodiscard]] auto getQualifiedName(const std::string &id) const -> std::string
	{
		return getPrefix() + "::" + id;
	}
	[[nodiscard]] static auto getUnqualifiedName(const std::string &id) -> std::string
	{
		auto c = id.find_last_of(':');
		return id.substr(c != std::string::npos ? c + 1 : c, std::string::npos);
	}

	[[nodiscard]] auto isTmpRecursive(const RegExp *re) const -> bool
	{
		return std::ranges::any_of(recRels_, [&](auto *recRel) { return *recRel == *re; });
	}

	void clearTmpRecursive() { recRels_.clear(); }

	[[nodiscard]] auto findRegisteredRE(const std::string &id) const -> const RegExp *
	{
		const auto *e = getModule()->getRegisteredRE(getQualifiedName(id));
		return e ? e : getModule()->getRegisteredRE(id);
	}
	[[nodiscard]] auto findRegisteredStatement(const std::string &id) const
		-> const LetStatement *
	{
		const auto *e = getModule()->getRegisteredStatement(getQualifiedName(id));
		return e ? e : getModule()->getRegisteredStatement(id);
	}

	void checkPredicateDeclaration(const std::string &id, const std::string &printID,
				       const yy::location &loc);
	void checkRelationDeclaration(const std::string &id, const std::string &printID,
				      const yy::location &loc);
	void checkDerivedDeclaration(const std::string &id, const RegExp *re,
				     const yy::location &loc);
	void checkViewDeclaration(const std::string &id, const RegExp *re, const yy::location &loc);
	void checkMutRecDeclaration(
		const std::vector<std::pair<std::string, std::unique_ptr<RegExp>>> &defs,
		const yy::location &loc);
	void checkMutRecDisjunctLeftRec(const std::string &id, const RegExp *re,
					const VSet<Relation> &recs, const yy::location &loc);

	static auto isAllowedReduction(const std::string &idRed) -> bool
	{
		auto id = getUnqualifiedName(idRed);
		return id == "po" || id == "po-loc" || id == "po-imm";
	}

	void saveState();
	void restoreState();

	/* Location for lexing/parsing */
	yy::location location{};

	/* Current source file directory (used for includes) */
	std::string dir{};

	/* Current name prefix */
	std::string prefix{};

	/* Temporary symbols stored during parsing */
	VSet<const RegExp *> recRels_;

	/* The module build out of parsing */
	std::unique_ptr<KatModule> module_{};

	/* Parser state (for recursive calls) */
	std::vector<State> states{};
};

#endif /* PARSING_DRIVER_HPP */
