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

#ifndef KATER_THEORY_HPP
#define KATER_THEORY_HPP

#include "Constraint.hpp"
#include "Predicate.hpp"
#include "Relation.hpp"
#include "VSet.hpp"

#include <cassert>
#include <ranges>

/*
 * Represents some amount of built-in knowledge.
 *
 *  - Currently represents all knowledge available to Kater, but
 *    it could be a particular subset too.
 *  - A Theory object could hold knowledge taken from different
 *    modules, if we ever decide to support this (currently a single
 *    module is supported).
 */
class Theory {

public:
	/** Creates an empty theory */
	Theory() = default;

	auto assume_begin() { return assumes_.begin(); }
	auto assume_end() { return assumes_.end(); }
	auto assumes() { return std::ranges::ref_view(assumes_); }

	[[nodiscard]] auto assume_begin() const { return assumes_.begin(); }
	[[nodiscard]] auto assume_end() const { return assumes_.end(); }
	[[nodiscard]] auto assumes() const { return std::ranges::ref_view(assumes_); }

	auto getAssumeNum() const -> size_t { return assumes_.size(); }

	void registerRelation(Relation r, RelationInfo info)
	{
		relations_[r.getID()] = std::move(info);
	}

	void registerPerLocPair(Relation r, Relation rloc) { perlocs_.insert({r.getID(), rloc}); }

	[[nodiscard]] auto getPerLoc(Relation r) const -> Relation
	{
		assert(hasPerLoc(r));
		return perlocs_.find(r.getID())->second;
	}
	[[nodiscard]] auto getPerLoc(const TransLabel &lab) const -> Relation
	{
		return getPerLoc(*lab.getRelation());
	}

	[[nodiscard]] auto hasPerLoc(Relation r) const -> bool
	{
		return perlocs_.contains(r.getID());
	}
	[[nodiscard]] auto hasPerLoc(const TransLabel &lab) const -> bool
	{
		return lab.isRelation() && perlocs_.contains(lab.getRelation()->getID());
	}

	void registerPredicate(Predicate p, PredicateInfo info)
	{
		predicates_[p.getID()] = std::move(info);
	}

	void registerDomain(Relation r, PredicateSet dom)
	{
		assert(relations_.contains(r.getID()));
		relations_[r.getID()].dom.insert(dom);
	}

	void registerCodomain(Relation r, PredicateSet codom)
	{
		assert(relations_.contains(r.getID()));
		relations_[r.getID()].codom.insert(codom);
	}

	// Handle "assume c" declaration in the input file
	void registerAssume(std::unique_ptr<Constraint> c) { assumes_.push_back(std::move(c)); }

	/* Sometimes it's convenient to have temporary/removable
	 * assumes (for internal checks) */
	void registerTempAssume(std::unique_ptr<Constraint> c)
	{
		tempAssumes_.insert(&*c);
		assumes_.push_back(std::move(c));
	}

	void clearTempAssumes()
	{
		assumes_.erase(
			std::remove_if(assumes_.begin(), assumes_.end(),
				       [&](auto &cUP) { return tempAssumes_.count(&*cUP) > 0; }),
			assumes_.end());
		tempAssumes_.clear();
	}

	void registerDisjointPreds(VSet<Predicate::ID> preds)
	{
		disjointPreds_.insert(preds);

		/* ∀ b ∈ preds, ∀ a ⊆ b: disjoint((preds \ {b}) ∪ a) */
		namespace RV = std::ranges::views;
		for (const auto &subsetKV : subsetPreds_) {
			for (auto &b : preds | RV::filter([&](auto &b) {
					       return subsetKV.second.contains(b);
				       })) {
				auto disjNew = preds;
				disjNew.erase(b);
				disjNew.insert(subsetKV.first);
				registerDisjointPreds(std::move(disjNew));
			}
		}
	}

	void registerInvalidPreds(VSet<Predicate::ID> preds)
	{
		if (preds.size() == 2) {
			registerDisjointPreds(preds);
			return;
		}
		invalidPreds_.insert(preds);
	}

	void registerSubsetPair(Predicate::ID a, Predicate::ID b)
	{
		subsetPreds_[a].insert(b);
		/* ∀ disjoint(S) st a ∈ S: disjoint((S \ {a}) ∪ b) */
		for (const auto &disjSet : disjointPreds_) {
			if (disjSet.contains(a)) {
				auto disjNew = disjSet;
				disjNew.erase(a);
				disjNew.insert(b);
				registerDisjointPreds(std::move(disjNew));
			}
		}
	}

	auto isSubPredOf(const Predicate::ID &a, const Predicate::ID &b) const -> bool
	{
		auto pIt = subsetPreds_.find(a);
		return pIt != subsetPreds_.end() && pIt->second.contains(b);
	}

	/** Returns true if A <= B */
	[[nodiscard]] auto isIncludedIn(const Relation &a, const Relation &b) const -> bool;
	[[nodiscard]] auto isIncludedIn(const Predicate &a, const Predicate &b) const -> bool;
	[[nodiscard]] auto isIncludedIn(const PredicateSet &a, const PredicateSet &b) const -> bool;
	[[nodiscard]] auto isIncludedIn(const TransLabel &a, const TransLabel &b) const -> bool;

	/** Returns whether two predicates compose */
	[[nodiscard]] auto composes(const PredicateSet &preds1, const PredicateSet &preds2) const
		-> bool;

	/**
	 * Returns whether two transitions A and B compose:
	 *    - post(A) ~ pre(B)
	 *    - codom(A) ~ dom(B) (even if domains are not explicitly present in the labels)
	 */
	[[nodiscard]] auto composes(const TransLabel &a, const TransLabel &b) const -> bool;

	auto isValid(const VSet<Predicate::ID> &preds) const -> bool
	{
		return std::ranges::find(invalidPreds_, preds) == invalidPreds_.end();
	}

	auto hasInfo(const Relation &r) const -> bool { return relations_.contains(r.getID()); }

	auto hasInfo(const Predicate &p) const -> bool { return predicates_.contains(p.getID()); }

	auto getInfo(const Relation &r) const -> const RelationInfo &
	{
		assert(relations_.contains(r.getID()));
		return relations_.find(r.getID())->second;
	}

	auto getInfo(const Predicate &p) const -> const PredicateInfo &
	{
		assert(predicates_.contains(p.getID()));
		return predicates_.find(p.getID())->second;
	}

	auto getName(const Relation &r) const -> const std::string &
	{
		assert(relations_.contains(r.getID()));
		return relations_.find(r.getID())->second.name;
	}

	auto getName(const Predicate &p) const -> const std::string &
	{
		assert(predicates_.contains(p.getID()));
		return predicates_.find(p.getID())->second.name;
	}

	auto getDomain(const Relation &r) const -> const PredicateSet &
	{
		const auto &info = getInfo(r);
		return r.isInverse() ? info.codom : info.dom;
	}

	auto getCodomain(const Relation &r) const -> const PredicateSet &
	{
		const auto &info = getInfo(r);
		return r.isInverse() ? info.dom : info.codom;
	}

	/** Tries to simplify the theory by dropping or transforming assumptions.
	 * Idempotent. */
	auto simplify() -> Theory &;

private:
	/** If CST contains domain/codomain information, incorporates it
	 * to the relevant assumptions. Returns whether that was the case. */
	auto tryIncorporateDomAssumption(const Constraint *cst) -> bool;

	/** If CST contains information about incompatible predicates,
	 * it incorporates it to the relevant assumptions. Returns whether
	 * that was the case. */
	auto tryIncorporateIncompatAssumption(const Constraint *cst) -> bool;

	/* It is tempting to use Relation as Key below; doing so, however,
	 * would force us to store duplicate information (for the reverse relation) */
	std::unordered_map<Relation::ID, RelationInfo> relations_{};
	std::unordered_map<Relation::ID, Relation> perlocs_{};
	std::unordered_map<Predicate::ID, PredicateInfo> predicates_{};
	std::unordered_map<Predicate::ID, VSet<Predicate::ID>> subsetPreds_{};
	VSet<VSet<Predicate::ID>> disjointPreds_{};
	VSet<VSet<Predicate::ID>> invalidPreds_{};

	std::vector<std::unique_ptr<Constraint>> assumes_;
	VSet<Constraint *> tempAssumes_;
};

#endif /* KATER_THEORY_HPP */
