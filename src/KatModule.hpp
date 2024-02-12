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

#include "Inclusion.hpp"
#include "KatModuleAPI.hpp"
#include "Parser.hpp"
#include "Theory.hpp"

class KatModule {

public:
	using UCO = std::unique_ptr<Constraint>;

	struct DbgInfo {
		DbgInfo(const std::string *name, int line)
			: filename(name != nullptr ? *name : ""), line(line)
		{
		}
		std::string filename;
		int line;

		friend auto operator<<(std::ostream &ostr, const DbgInfo &dbg) -> std::ostream &;
	};

private:
	using VarMap = std::unordered_map<std::string, URE>;
	using SavedVarMap = std::vector<std::pair<Relation, SavedVar>>;

	using Assert = struct {
		UCO co;
		DbgInfo loc;
	};

public:
	KatModule() = default;

	using var_iter = VarMap::iterator;
	using var_const_iter = VarMap::const_iterator;
	using svar_iter = SavedVarMap::iterator;
	using svar_const_iter = SavedVarMap::const_iterator;
	using rec_iter = std::vector<URE>::iterator;
	using rec_const_iter = std::vector<URE>::const_iterator;
	using coh_iter = std::vector<URE>::iterator;
	using coh_const_iter = std::vector<URE>::const_iterator;
	using incl_iter = std::vector<UCO>::iterator;
	using incl_const_iter = std::vector<UCO>::const_iterator;
	using assr_iter = std::vector<Assert>::iterator;
	using assr_const_iter = std::vector<Assert>::const_iterator;
	using assm_iter = std::vector<UCO>::iterator;
	using assm_const_iter = std::vector<UCO>::const_iterator;

	auto var_begin() -> var_iter { return variables.begin(); }
	auto var_end() -> var_iter { return variables.end(); }
	auto var_begin() const -> var_const_iter { return variables.begin(); }
	auto var_end() const -> var_const_iter { return variables.end(); }

	auto svar_begin() -> svar_iter { return savedVariables.begin(); }
	auto svar_end() -> svar_iter { return savedVariables.end(); }
	auto svar_begin() const -> svar_const_iter { return savedVariables.begin(); }
	auto svar_end() const -> svar_const_iter { return savedVariables.end(); }

	auto acyc_begin() { return acyclicityConstraints.begin(); }
	auto acyc_end() { return acyclicityConstraints.end(); }
	auto acycs() { return std::ranges::ref_view(acyclicityConstraints); }
	auto acyc_begin() const { return acyclicityConstraints.begin(); }
	auto acyc_end() const { return acyclicityConstraints.end(); }
	auto acycs() const { return std::ranges::ref_view(acyclicityConstraints); }

	auto rec_begin() -> rec_iter { return recoveryConstraints.begin(); }
	auto rec_end() -> rec_iter { return recoveryConstraints.end(); }
	auto rec_begin() const -> rec_const_iter { return recoveryConstraints.begin(); }
	auto rec_end() const -> rec_const_iter { return recoveryConstraints.end(); }

	auto coh_begin() -> coh_iter { return coherenceConstraints.begin(); }
	auto coh_end() -> coh_iter { return coherenceConstraints.end(); }
	auto coh_begin() const -> coh_const_iter { return coherenceConstraints.begin(); }
	auto coh_end() const -> coh_const_iter { return coherenceConstraints.end(); }

	auto incl_begin() -> incl_iter { return inclusionConstraints.begin(); }
	auto incl_end() -> incl_iter { return inclusionConstraints.end(); }
	auto incl_begin() const -> incl_const_iter { return inclusionConstraints.begin(); }
	auto incl_end() const -> incl_const_iter { return inclusionConstraints.end(); }

	auto assert_begin() -> assr_iter { return asserts.begin(); }
	auto assert_end() -> assr_iter { return asserts.end(); }
	auto assert_begin() const -> assr_const_iter { return asserts.begin(); }
	auto assert_end() const -> assr_const_iter { return asserts.end(); }

	auto getRegisteredNum() const -> size_t { return variables.size(); }

	auto getSavedNum() const -> size_t { return savedVariables.size(); }

	auto getAssertNum() const -> size_t { return asserts.size(); }

	auto getAcyclicNum() const -> size_t { return acyclicityConstraints.size(); }

	auto getRecoveryNum() const -> size_t { return recoveryConstraints.size(); }

	auto getInclusionNum() const -> size_t { return inclusionConstraints.size(); }

	auto getCohNum() const -> size_t { return coherenceConstraints.size(); }

	auto getPPO() const -> URE { return (ppo) ? ppo->clone() : nullptr; }
	auto getPPORF() const -> URE { return (pporf) ? pporf->clone() : nullptr; }
	auto getPORF() const -> URE { return (porf_) ? porf_->clone() : nullptr; }
	auto getHB() const -> URE { return (hb) ? hb->clone() : nullptr; }

	auto getTheory() const -> const Theory & { return theory_; }
	auto getTheory() -> Theory & { return theory_; }

	void registerRelation(Relation r, RelationInfo info)
	{
		auto &theory = getTheory();
		registerDerived(info.name, CharRE::create(TransLabel(r)));
		theory.registerRelation(r, info);

		if (!r.isUser() || info.locInfo == RelLocInfo::KeepsLoc)
			return;

		auto perlocName = info.name + "-loc";
		auto rloc = Relation::createUser();
		registerDerived(perlocName, CharRE::create(TransLabel(rloc)));
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
		registerDerived(info.name, CharRE::create(TransLabel(std::nullopt, {p})));
		getTheory().registerPredicate(p, info);
	}

	void registerDerived(const std::string &id, URE re)
	{
		variables.insert({id, std::move(re)});
	}

	void registerSaveDerived(const std::string &id, URE re)
	{
		auto r = Relation::createUser();
		registerDerived(id, CharRE::create(TransLabel(r)));
		savedVariables.push_back({r, SavedVar(std::move(re))});
	}

	void registerSaveReduceDerived(const std::string &idSave, const std::string &idRed, URE re)
	{
		auto r = Relation::createUser();
		registerDerived(idSave, CharRE::create(TransLabel(r)));
		savedVariables.push_back(
			{r, SavedVar(std::move(re),
				     idRed == idSave ? ReductionType::Self : ReductionType::Po,
				     getRegisteredID(idRed))});
	}

	void registerViewDerived(const std::string &id, URE re)
	{
		auto r = Relation::createUser();
		registerDerived(id, CharRE::create(TransLabel(r)));
		savedVariables.push_back({r, SavedVar(std::move(re), VarStatus::View)});
	}

	void registerAssert(UCO co, const yy::location &loc)
	{
		asserts.push_back({std::move(co), DbgInfo(loc.end.filename, loc.end.line)});
	}

	void registerPPO(URE r)
	{
		if (!r) {
			return;
		}

		// FIXME: Move porf registration to parsing ctor?
		auto po = getRegisteredID("po");
		ppo = std::move(r);

		depTracking = (*ppo != *po);

		/* Also create pporf since we are at it */
		auto rf = getRegisteredID("rfe");
		auto tc = getRegisteredID("tc");
		auto tj = getRegisteredID("tj");
		pporf = StarRE::createOpt(
			AltRE::createOpt(ppo->clone(), rf->clone(), tc->clone(), tj->clone()));
		porf_ = StarRE::createOpt(
			AltRE::createOpt(po->clone(), std::move(rf), std::move(tc), std::move(tj)));
	}

	void registerHB(URE r)
	{
		if (!r) {
			return;
		}

		hb = std::move(r);
	}

	// Handle consistency constraint in the input file
	void registerExport(const Constraint *c, const yy::location &loc);

	auto getRegisteredID(const std::string &id) const -> URE
	{
		auto it = variables.find(id);
		return (it == variables.end()) ? nullptr : it->second->clone();
	}

	auto isSavedID(const CharRE *re) const -> bool
	{
		auto ro = re->getLabel().getRelation();
		assert(ro.has_value());
		return std::find_if(savedVariables.begin(), savedVariables.end(), [&](auto &p) {
			       return p.first == *ro;
		       }) != savedVariables.end();
	}

	auto getSavedID(const CharRE *re) const -> URE
	{
		assert(isSavedID(re));
		auto ro = re->getLabel().getRelation();
		return std::find_if(savedVariables.begin(), savedVariables.end(),
				    [&](auto &p) { return p.first == *ro; })
			->second.exp->clone();
	}

	auto isDepTracking() const -> bool
	{
		assert(depTracking.has_value());
		return *depTracking;
	}

private:
	// Results from parsing the input file
	VarMap variables{};

	// XXX: Maybe it's better to even have two containers below
	//      so that saved/reduced variables are treated differently,
	//      but I've no idea how many variable categories we want to have.
	//      If just two, I prefer separated. If more, polymorphism.
	SavedVarMap savedVariables{};

	Theory theory_{};

	std::vector<Assert> asserts{};
	std::vector<UCO> acyclicityConstraints{};
	std::vector<URE> recoveryConstraints{};
	std::vector<UCO> inclusionConstraints{};
	std::vector<URE> coherenceConstraints{};
	URE ppo{};
	URE pporf{};
	URE porf_{};
	URE hb{};

	std::optional<bool> depTracking{};
};

#endif /* KATER_KAT_MODULE_HPP */
