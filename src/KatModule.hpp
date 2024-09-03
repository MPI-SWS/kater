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

#ifndef KATER_KAT_MODULE_HPP
#define KATER_KAT_MODULE_HPP

#include "DbgInfo.hpp"
#include "Parser.hpp"
#include "Theory.hpp"

class KatModule {

public:
	KatModule() = default;

	auto lets() const { return std::views::all(lets_); }
	auto lets() { return std::views::all(lets_); }

	auto exports() { return std::views::all(exports_); }
	auto exports() const { return std::views::all(exports_); }

	auto asserts() const { return std::views::all(asserts_); }
	auto asserts() { return std::views::all(asserts_); }

	auto getPPODeclaration() const -> const LetStatement * { return ppo_; }
	auto getHBDeclaration() const -> const LetStatement * { return hb_; }
	auto getCOHDeclaration() const -> const LetStatement * { return coh_; }

	auto createPPORF() const -> std::unique_ptr<RegExp>
	{
		return PlusRE::createOpt(AltRE::createOpt(
			getPPODeclaration()->getRE()->clone(), getRegisteredRE("rfe")->clone(),
			getRegisteredRE("tc")->clone(), getRegisteredRE("tj")->clone()));
	}
	auto createPORF() const -> std::unique_ptr<RegExp>
	{
		return PlusRE::createOpt(AltRE::createOpt(
			getRegisteredRE("po")->clone(), getRegisteredRE("rfe")->clone(),
			getRegisteredRE("tc")->clone(), getRegisteredRE("tj")->clone()));
	}

	auto getTheory() const -> const Theory & { return theory_; }
	auto getTheory() -> Theory & { return theory_; }

	void registerRelation(Relation r, RelationInfo info)
	{
		auto &theory = getTheory();
		registerLet(LetStatement::create(info.name, CharRE::create(TransLabel(r)),
						 NoSavedExp::create(), info.dbg));
		theory.registerRelation(r, info);

		if (!r.isUser() || info.locInfo == RelLocInfo::KeepsLoc)
			return;

		auto perlocName = info.name + "-loc";
		auto rloc = Relation::createUser();
		registerLet(LetStatement::create(perlocName, CharRE::create(TransLabel(rloc)),
						 NoSavedExp::create(), std::nullopt));
		auto rlocInfo = RelationInfo{
			.name = perlocName,
			.arity = info.arity, /* maybe Unknown? */
			.locInfo = RelLocInfo::KeepsLoc,
			.dom = info.dom,
			.codom = info.codom,
		};
		theory.registerRelation(rloc, rlocInfo);
		theory.registerPerLocPair(r, rloc);
	}

	void registerPredicate(Predicate p, PredicateInfo info)
	{
		registerLet(LetStatement::create(info.name,
						 CharRE::create(TransLabel(std::nullopt, {p})),
						 NoSavedExp::create(), std::nullopt));
		getTheory().registerPredicate(p, info);
	}

	void registerLet(std::unique_ptr<LetStatement> let)
	{
		/* In case we are overwriting */
		auto existing = std::find_if(lets_.begin(), lets_.end(), [&](const auto &olet) {
			return let->getName() == olet->getName();
		});
		if (existing != lets_.end())
			existing = lets_.erase(existing);
		lets_.insert(existing, std::move(let));
	}

	void registerAssert(std::unique_ptr<AssertStatement> asrt)
	{
		asserts_.push_back(std::move(asrt));
	}

	void registerPPO(LetStatement *ppo)
	{
		ppo_ = ppo;
		if (ppo)
			depTracking = (*ppo->getRE() != *getRegisteredRE("po"));
	}

	void registerHB(LetStatement *hb) { hb_ = hb; }

	void registerCOH(LetStatement *coh) { coh_ = coh; }

	// Handle consistency constraint in the input file
	void registerExport(std::unique_ptr<ExportStatement> stmt, const yy::location &loc);

	auto getRegisteredRE(const std::string &name) const -> const RegExp *
	{
		auto it = std::find_if(lets_.begin(), lets_.end(),
				       [&](auto &let) { return let->getName() == name; });
		return (it == lets_.end()) ? nullptr : (*it)->getRE();
	}
	auto getRegisteredStatement(const std::string &name) const -> const LetStatement *
	{
		auto it = std::find_if(lets_.begin(), lets_.end(),
				       [&](auto &let) { return let->getName() == name; });
		return (it == lets_.end()) ? nullptr : &*(*it);
	}
	auto getRegisteredStatement(const std::string &name) -> LetStatement *
	{
		auto it = std::find_if(lets_.begin(), lets_.end(),
				       [&](auto &let) { return let->getName() == name; });
		return (it == lets_.end()) ? nullptr : &*(*it);
	}

	auto isDepTracking() const -> bool
	{
		assert(depTracking.has_value());
		return *depTracking;
	}

	/** Replaces all uses of FROM with TO */
	void replaceAllUsesWith(const RegExp *from, const RegExp *to);

private:
	std::vector<std::unique_ptr<LetStatement>> lets_{};
	std::vector<std::unique_ptr<AssertStatement>> asserts_{};
	std::vector<std::unique_ptr<ExportStatement>> exports_{};
	Theory theory_{};

	/* Store some commonly used expressions for easy access */
	LetStatement *ppo_{};
	LetStatement *hb_{};
	LetStatement *coh_{};

	std::optional<bool> depTracking{};
};

#endif /* KATER_KAT_MODULE_HPP */
