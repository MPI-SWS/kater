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

#include "ParsingDriver.hpp"

#include "Builtins.hpp"
#include "DbgInfo.hpp"
#include "Error.hpp"
#include "SavedExp.hpp"

#include <cerrno>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <iostream>
#include <memory>
#include <optional>
#include <system_error>
#include <utility>

#define DEBUG_TYPE "parser"

extern void lexer_push_file(FILE *);
extern void lexer_pop_file();

ParsingDriver::ParsingDriver() : module_(new KatModule) { registerBuiltins(*getModule()); }

void ParsingDriver::saveState() { states.emplace_back(getLocation(), dir, getPrefix()); }

void ParsingDriver::restoreState()
{
	if (states.empty())
		return;

	auto &s = states.back();
	lexer_pop_file();
	location = s.loc;
	dir = s.dir;
	prefix = s.prefix;
	states.pop_back();
}

auto ParsingDriver::parse(const std::string &name) -> int
{
	if (name.empty()) {
		std::cerr << "no input file provided\n";
		exit(EPARSE); // NOLINT(concurrency-mt-unsafe): parsing is single-threaded
	}

	/* Resolve the file against the includer's directory. The path is kept as
	 * written (only lexically normalized) for diagnostics */
	std::filesystem::path path(name);
	if (path.is_relative()) {
		path = std::filesystem::path(dir) / path;
	}
	path = path.lexically_normal();

	/* Canonicalize the path (e.g., collapse ./, ../, etc) so that each file is
	 * parsed at most once */
	std::error_code err;
	auto canonical = std::filesystem::weakly_canonical(path, err);
	if (!includedFiles_.insert(err ? path : canonical).second) {
		return 0;
	}

	/* Save parsing state, open the provided file, and automatically close it when scope ends */
	saveState();

	const std::unique_ptr<FILE, int (*)(FILE *)> file(std::fopen(path.string().c_str(), "r"),
							  &std::fclose);
	if (!file) {
		// NOLINTNEXTLINE(concurrency-mt-unsafe): parsing is single-threaded
		std::cerr << "cannot open " << path << ": " << strerror(errno) << "\n";
		exit(EPARSE); // NOLINT(concurrency-mt-unsafe): parsing is single-threaded
	}

	/* Store directory for nested includes (including trailing slash) */
	dir = path.parent_path().string();
	if (!dir.empty() && dir.back() != '/') {
		dir += "/";
	}

	/* Filename without extension */
	prefix = path.stem().string();

	lexer_push_file(file.get());
	auto pathString = path.string();
	location.initialize(&pathString);

	yy::parser parser(*this);

	// KATER_DEBUG(
	// 	if (config.debug)
	// 		parser.set_debug_level(2);
	// );

	auto res = parser.parse();

	/* If @ top-level, save ppo, hb_stable */
	if (states.size() == 1) {
		auto *module = getModule();
		auto *ppoLet = module->getRegisteredStatement(getQualifiedName("ppo"));
		auto *hbLet = module->getRegisteredStatement(getQualifiedName("hb"));
		module->registerPPO(ppoLet);
		module->registerHB(hbLet);
	}

	restoreState();

	return res;
}

void ParsingDriver::reportUndeclaredRelation(const std::string &id, const yy::location &loc)
{
	std::cerr << loc << ": undeclared relation/predicate (" << id << ")";

	/* If there is a definition with the same unqualified name in another namespace,
	 * suggest it (module internal definitions) */
	const auto *mod = getModule();
	auto shortName = getUnqualifiedName(id);
	VSet<std::string> hidden;
	std::vector<std::string> suggestions;

	for (const auto &[rid, info] : mod->getTheory().relations()) {
		if (info.hidden)
			hidden.insert(info.name);
	}
	for (const auto &let : mod->lets()) {
		const auto &name = let->getName();
		if (name != id && !hidden.contains(name) && getUnqualifiedName(name) == shortName)
			suggestions.push_back(name);
	}
	if (!suggestions.empty()) {
		std::cerr << "; did you mean ";
		for (auto i = 0U; i < suggestions.size(); ++i)
			std::cerr << (i != 0 ? ", " : "") << suggestions[i];
		std::cerr << "?";
	}
	std::cerr << "\n";
	exit(EPARSE);
}

void ParsingDriver::registerDerived(std::unique_ptr<LetStatement> let, const yy::location &loc)
{
	let->setName(getQualifiedName(let->getName()));
	checkRelationDeclaration(let->getName(), loc);
	getModule()->registerLet(std::move(let));
}

void ParsingDriver::checkRelationDeclaration(const std::string &id, const yy::location &loc)
{
	const auto *re = findRegisteredRE(id);
	if (!re || isTmpRecursive(re))
		return;

	const auto *charRE = dynamic_cast<const CharRE *>(&*re);
	if (charRE == nullptr)
		return;

	const auto &theory = getModule()->getTheory();
	const auto &lab = charRE->getLabel();
	std::optional<DbgInfo> prev;
	if (lab.isRelation())
		prev = theory.getInfo(*lab.getRelation()).dbg;
	else if (lab.isPredicate())
		prev = theory.getInfo(*lab.getPreChecks().begin()).dbg;
	else
		return;

	std::cerr << loc << ": " << "identifier " << id << " already declared";
	if (prev.has_value())
		std::cerr << " (previous declaration: " << *prev << ")";
	std::cerr << "\n";
	exit(EPARSE); // NOLINT(concurrency-mt-unsafe): parsing is single-threaded
}

void ParsingDriver::checkMutRecDisjunctLeftRec(const std::string &id, const RegExp *re,
					       const VSet<Relation> &recs, const yy::location &loc)
{
	auto checkMutRecDisjunct = [&](const std::string &id, const RegExp *re,
				       const VSet<Relation> &recs, auto &checkRef) -> bool {
		std::vector<bool> results;
		for (auto i = 0U; i < re->getNumKids(); i++) {
			results.push_back(checkRef(id, re->getKid(i), recs, checkRef));
		}

		const auto *seqRE = dynamic_cast<const SeqRE *>(re);
		const auto *starRE = dynamic_cast<const StarRE *>(re);
		const auto *plusRE = dynamic_cast<const PlusRE *>(re);
		auto containsRec = std::any_of(results.begin(), results.end(),
					       [&](auto result) { return result; });
		if (containsRec && (starRE || plusRE)) {
			std::cerr << loc << ": "
				  << "recursive relations cannot be used with (+) and (*) "
				     "quantifiers\n";
			exit(EPARSE);
		}
		if (containsRec && seqRE &&
		    std::any_of(std::next(results.begin()), results.end(),
				[&](auto result) { return result; })) {
			std::cerr << loc << ": "
				  << "recursive relations can only be used in a left-recursive "
				     "manner\n";
			exit(EPARSE);
		}

		const auto *charRE = dynamic_cast<const CharRE *>(&*re);
		if (!charRE)
			return containsRec;
		return charRE->isRelation() && std::ranges::any_of(recs, [&](auto &rel) {
			       return *charRE->getLabel().getRelation() == rel;
		       });
	};
	checkMutRecDisjunct(id, re, recs, checkMutRecDisjunct);
}

void ParsingDriver::checkPredicateDeclaration(const std::string &id, const yy::location &loc)
{
	const auto *re = findRegisteredRE(id);
	if (!re)
		return;

	const auto *charRE = dynamic_cast<const CharRE *>(&*re);
	if (charRE && charRE->getLabel().isPredicate() &&
	    getModule()->getTheory().hasInfo(*charRE->getLabel().getPreChecks().begin())) {
		const auto &info = getModule()->getTheory().getInfo(
			*charRE->getLabel().getPreChecks().begin());
		std::cerr << loc << ": ";
		std::cerr << "predicate " << id << " already declared";
		if (info.dbg.has_value())
			std::cerr << " (previous declaration: " << *info.dbg << ")";
		std::cerr << "\n";
		exit(EPARSE);
	}
}

void ParsingDriver::checkMutRecDeclaration(
	const std::vector<std::pair<std::string, std::unique_ptr<RegExp>>> &defs,
	const yy::location &loc)
{
	auto *module = getModule();

	/* Ensure relation names haven't already been declared */
	for (auto &[name, RE] : defs) {
		const auto *regRE = findRegisteredRE(name);
		if (!regRE) {
			std::cerr << loc << ": " << name
				  << " is not used in let rec body\n"; // warning
			getRegisteredREOrCreateTmpRec(name, loc);
			regRE = findRegisteredRE(name);
		}
		checkRelationDeclaration(name, loc);
	}
	/* We don't have to check declaration well-formedness
	 * here (this is done when tmp relations are created).
	 * But we do have to ensure that we didn't declare any "unknown"/unused relation */
	auto it = std::ranges::find_if(tmp_recs(), [&](auto *tmpRE) {
		return std::ranges::none_of(defs, [&](auto &nameRE) {
			return *findRegisteredRE(nameRE.first) == *tmpRE;
		});
	});
	if (it != std::ranges::end(tmp_recs())) {
		auto rel = *dynamic_cast<const CharRE *>(*it)->getLabel().getRelation();
		std::cerr << loc << ": ";
		std::cerr << "Undeclared relation (" << module->getTheory().getInfo(rel).name
			  << ") used in recursive definition of " << defs[0].first << "\n";
		exit(EPARSE);
	}

	/* Export some relevant info */
	auto recRelIDsView =
		defs | std::views::transform([&](auto &nameRE) {
			return *dynamic_cast<const CharRE *>(findRegisteredRE(nameRE.first))
					->getLabel()
					.getRelation();
		});
	auto relSet =
		VSet<Relation>(std::ranges::begin(recRelIDsView), std::ranges::end(recRelIDsView));

	/* Ensure recursive relations are used properly */
	for (auto &[name, RE] : defs) {
		checkMutRecDisjunctLeftRec(name, &*RE, relSet, loc);
	}
}

auto extractREs(const std::vector<std::pair<std::string, std::unique_ptr<RegExp>>> &defs)
	-> std::vector<std::unique_ptr<RegExp>>
{
	auto resView = defs |
		       std::views::transform([&](auto &nameRE) { return nameRE.second->clone(); });
	return std::vector<std::unique_ptr<RegExp>>(std::ranges::begin(resView),
						    std::ranges::end(resView));
}

void ParsingDriver::registerRecDerived(
	std::vector<std::pair<std::string, std::unique_ptr<RegExp>>> defs, const yy::location &loc)
{
	/* Check well-formedness */
	checkMutRecDeclaration(defs, loc);

	auto recRelIDsView =
		defs | std::views::transform([&](auto &nameRE) {
			return *dynamic_cast<const CharRE *>(findRegisteredRE(nameRE.first))
					->getLabel()
					.getRelation();
		});
	auto rels = std::vector<Relation>(std::ranges::begin(recRelIDsView),
					  std::ranges::end(recRelIDsView));

	/* Register a regexp for each of the names involved */
	for (auto &[name, re] : defs) {
		auto mutRecRE =
			MutRecRE::createOpt(*dynamic_cast<const CharRE *>(findRegisteredRE(name))
						     ->getLabel()
						     .getRelation(),
					    rels, extractREs(defs));
		getModule()->registerLet(
			LetStatement::create(getQualifiedName(name), std::move(mutRecRE),
					     NoSavedExp::create(), toDbgInfo(loc)));
	}
	clearTmpRecursive();
	setRecContext(false);
}

void ParsingDriver::registerRecViewDerived(
	std::vector<std::pair<std::string, std::unique_ptr<RegExp>>> defs, std::string codeToPrint,
	const yy::location &loc)
{
	checkMutRecDeclaration(defs, loc);

	auto recRelIDsView =
		defs | std::views::transform([&](auto &nameRE) {
			return *dynamic_cast<const CharRE *>(findRegisteredRE(nameRE.first))
					->getLabel()
					.getRelation();
		});
	auto rels = std::vector<Relation>(std::ranges::begin(recRelIDsView),
					  std::ranges::end(recRelIDsView));

	/* Register a (condensed) regexp for each of the names involved */
	for (auto &[name, re] : defs) {
		auto rel = *dynamic_cast<const CharRE *>(findRegisteredRE(name))
				    ->getLabel()
				    .getRelation();
		auto mutRecRE = MutRecRE::createOpt(rel, rels, extractREs(defs));
		getModule()->registerLet(LetStatement::create(
			getQualifiedName(name), std::move(mutRecRE), ViewExp::create(re->clone()),
			toDbgInfo(loc), codeToPrint));
	}
	clearTmpRecursive();
	setRecContext(false);
}
