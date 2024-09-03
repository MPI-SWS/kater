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

#include <cstring>
#include <fstream>
#include <iostream>

#define DEBUG_TYPE "parser"

extern FILE *yyin;
extern void yyrestart(FILE *);

ParsingDriver::ParsingDriver() : module_(new KatModule) { registerBuiltins(*getModule()); }

void ParsingDriver::saveState() { states.emplace_back(getLocation(), yyin, dir, getPrefix()); }

void ParsingDriver::restoreState()
{
	if (states.empty())
		return;

	auto &s = states.back();
	yyrestart(s.in);
	location = s.loc;
	dir = s.dir;
	prefix = s.prefix;
	states.pop_back();
}

auto ParsingDriver::parse(const std::string &name) -> int
{
	saveState();

	if (name.empty()) {
		std::cerr << "no input file provided" << std::endl;
		exit(EXIT_FAILURE);
	}

	auto path = dir + name;
	if ((yyin = fopen(path.c_str(), "r")) == nullptr) {
		std::cerr << "cannot open " << path << ": " << strerror(errno) << std::endl;
		exit(EXIT_FAILURE);
	}

	auto s = path.find_last_of("/");
	dir = path.substr(0, s != std::string::npos ? s + 1 : std::string::npos);

	auto d = path.find_last_of(".");
	prefix = path.substr(s != std::string::npos ? s + 1 : 0,
			     d == std::string::npos ? std::string::npos
						    : (s != std::string::npos ? d - s - 1 : d - 1));

	yyrestart(yyin);
	location.initialize(&path);

	yy::parser parser(*this);

	// KATER_DEBUG(
	// 	if (config.debug)
	// 		parser.set_debug_level(2);
	// );

	auto res = parser.parse();

	fclose(yyin);

	/* If @ top-level, save ppo, hb_stable */
	if (states.size() == 1) {
		auto *module = getModule();
		auto *ppoLet = module->getRegisteredStatement(getQualifiedName("ppo"));
		auto *hbLet = module->getRegisteredStatement(getQualifiedName("hb_stable"));
		module->registerPPO(ppoLet);
		module->registerHB(hbLet);
	}

	restoreState();

	return res;
}

auto collectUsedPrimitives(const RegExp *re) -> VSet<TransLabel>
{
	auto collectPrimitives = [&](const RegExp *re, VSet<TransLabel> &result,
				     auto &collectRef) -> void {
		for (auto i = 0U; i < re->getNumKids(); i++) {
			collectRef(re->getKid(i), result, collectRef);
		}
		if (const auto *charRE = dynamic_cast<const CharRE *>(&*re)) {
			result.insert(charRE->getLabel());
		}
	};
	VSet<TransLabel> result;
	collectPrimitives(re, result, collectPrimitives);
	return result;
}

void ParsingDriver::checkDerivedDeclaration(const std::string &id, const RegExp *re,
					    const yy::location &loc)
{
	checkRelationDeclaration(id, "", loc);

	auto *module = getModule();
	auto prims = collectUsedPrimitives(&*re);

	/* Unknown predicates are reported immediately; only search for relations.
	 * (Relations are declared as user relations when they are first encountered) */
	for (auto &lab : prims | std::views::filter([&](auto &lab) { return lab.isRelation(); })) {
		assert(module->getTheory().hasInfo(*lab.getRelation()));
		if (lab.getRelation()->isUser() && isTmpRecursive(&*CharRE::create(lab))) {
			std::cerr << loc << ": ";
			std::cerr << "Undeclared relation ("
				  << module->getTheory().getInfo(*lab.getRelation()).name
				  << ") used in definition of " << id << "\n";
			exit(EPARSE);
		}
	}
}

void ParsingDriver::registerDerived(std::unique_ptr<LetStatement> let, const yy::location &loc)
{
	let->setName(getQualifiedName(let->getName()));
	checkDerivedDeclaration(let->getName(), let->getRE(), loc);
	getModule()->registerLet(std::move(let));
}

void ParsingDriver::checkRelationDeclaration(const std::string &id, const std::string &printID,
					     const yy::location &loc)
{
	const auto *re = findRegisteredRE(id);
	if (!re || isTmpRecursive(re))
		return;

	const auto *charRE = dynamic_cast<const CharRE *>(&*re);
	if (charRE && charRE->getLabel().isRelation()) {
		const auto &info =
			getModule()->getTheory().getInfo(*charRE->getLabel().getRelation());

		std::cerr << loc << ": ";
		std::cerr << "identifier " << id << " already declared";
		if (info.dbg.has_value())
			std::cerr << " (previous declaration: " << *info.dbg << ")";
		std::cerr << "\n";
		exit(EPARSE);
	}
	if (charRE && charRE->getLabel().isPredicate()) {
		const auto &info = getModule()->getTheory().getInfo(
			*charRE->getLabel().getPreChecks().begin());

		std::cerr << loc << ": ";
		std::cerr << "identifier " << id << " already declared";
		if (info.dbg.has_value())
			std::cerr << " (previous declaration: " << *info.dbg << ")";
		std::cerr << "\n";
		exit(EPARSE);
	}
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
			std ::cerr << loc << ": "
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

void ParsingDriver::checkPredicateDeclaration(const std::string &id, const std::string &printID,
					      const yy::location &loc)
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
		checkRelationDeclaration(name, "", loc);
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
		getModule()->registerLet(LetStatement::create(
			getQualifiedName(name), std::move(mutRecRE), NoSavedExp::create(),
			DbgInfo(loc.end.filename, loc.end.line)));
	}
	clearTmpRecursive();
}

void ParsingDriver::registerRecViewDerived(
	std::vector<std::pair<std::string, std::unique_ptr<RegExp>>> defs, const yy::location &loc)
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
			DbgInfo(loc.end.filename, loc.end.line)));
	}
	clearTmpRecursive();
}
