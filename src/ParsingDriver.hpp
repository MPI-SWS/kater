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
#include <algorithm>
#include <memory>
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
	using UCO = KatModule::UCO;

	ParsingDriver();

	auto getLocation() -> yy::location & { return location; }

	[[nodiscard]] auto getPrefix() const -> const std::string & { return prefix; }

	[[nodiscard]] auto getQualifiedName(const std::string &id) const -> std::string
	{
		return getPrefix() + "::" + id;
	}
	[[nodiscard]] static std::string getUnqualifiedName(const std::string &id)
	{
		auto c = id.find_last_of(':');
		return id.substr(c != std::string::npos ? c + 1 : c, std::string::npos);
	}

	void registerRelation(const std::string &id)
	{
		RelationInfo info;
		info.name = getQualifiedName(id);
		module->registerRelation(Relation::createUser(), std::move(info));
	}

	void registerPredicate(const std::string &id, const std::string &printID)
	{
		PredicateInfo info;
		info.name = getQualifiedName(id);
		info.genmc = printID;
		module->registerPredicate(Predicate::createUser(), std::move(info));
	}

	void registerDerived(const std::string &id, URE re)
	{
		module->registerDerived(getQualifiedName(id), std::move(re));
	}

	void registerSaveDerived(const std::string &idSave, const std::string &idRed, URE re,
				 const yy::location &loc)
	{
		if (!idRed.empty() && idRed != idSave && !isAllowedReduction(idRed)) {
			std::cerr << loc << ": ";
			std::cerr << "forbidden reduction encountered \"" << idRed << "\"\n";
			exit(EXIT_FAILURE);
		}
		if (!idRed.empty()) {
			std::string rname;
			if (idRed == idSave || module->getRegisteredID(getQualifiedName(idRed))) {
				rname = getQualifiedName(idRed);
			} else {
				rname = idRed;
			}
			if (idRed != idSave) {
				getRegisteredID(rname, loc); // ensure exists
			}

			module->registerSaveReduceDerived(getQualifiedName(idSave), rname,
							  std::move(re));
		} else {
			module->registerSaveDerived(getQualifiedName(idSave), std::move(re));
		}
	}

	void registerViewDerived(const std::string &id, URE re)
	{
		module->registerViewDerived(getQualifiedName(id), std::move(re));
	}

	// Handle "assert c" declaration in the input file
	void registerAssert(UCO c, const yy::location &loc)
	{
		module->registerAssert(std::move(c), loc);
	}

	// Handle "assume c" declaration in the input file
	void registerAssume(UCO c, const yy::location & /*loc*/)
	{
		module->getTheory().registerAssume(std::move(c));
	}

	void registerDisjointPreds(VSet<Predicate::ID> disjoint)
	{
		module->getTheory().registerDisjointPreds(std::move(disjoint));
	}

	// Handle consistency constraint in the input file
	void registerExport(UCO c, const yy::location &loc) { module->registerExport(&*c, loc); }

	auto getRegisteredID(const std::string &id, const yy::location &loc) -> URE
	{
		auto e = module->getRegisteredID(getQualifiedName(id));
		if (!e) {
			auto f = module->getRegisteredID(id);
			if (!f) {
				std::cerr << loc << ": ";
				std::cerr << "unknown relation encountered (" << id << ")\n";
				exit(EXIT_FAILURE);
			}
			e = std::move(f);
		}
		return std::move(e);
	}

	auto getRegisteredIDAsPredicate(const std::string &id, const yy::location &loc) -> Predicate
	{
		auto p = getRegisteredID(id, loc);
		if (!p->isPredicate()) {
			std::cerr << loc << ": ";
			std::cerr << "non-basic predicate used in disjoint clause (" << id << ")\n";
			exit(EXIT_FAILURE);
		}
		auto *charRE = dynamic_cast<CharRE *>(&*p);
		auto &lab = charRE->getLabel();
		assert(!lab.hasPostChecks());
		assert(lab.isPredicate());
		assert(lab.getPreChecks().size() == 1);
		return *charRE->getLabel().getPreChecks().begin();
	}

	/* Invoke the parser on INPUT. Return 0 on success. */
	auto parse(const std::string &input) -> int;

	auto takeModule() -> std::unique_ptr<KatModule> { return std::move(module); }

private:
	static auto isAllowedReduction(const std::string &idRed) -> bool
	{
		auto id = getUnqualifiedName(idRed);
		return id == "po" || id == "po-loc" || id == "po-imm";
	}

	void saveState();
	void restoreState();

	/* Location for lexing/parsing */
	yy::location location;

	/* Current source file directory (used for includes) */
	std::string dir;

	/* Current name prefix */
	std::string prefix;

	std::unique_ptr<KatModule> module;

	std::vector<State> states;
};

#endif /* PARSING_DRIVER_HPP */
