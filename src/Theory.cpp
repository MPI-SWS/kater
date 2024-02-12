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

#include "Theory.hpp"

#include <numeric>
#include <ranges>

auto Theory::isIncludedIn(const Relation &a, const Relation &b) const -> bool
{
	using RB = Relation::BuiltinID;

	if (a == b) {
		return true;
	}

	/* r <= any */
	if (b.isBuiltin() && b.toBuiltin() == RB::any) {
		return true;
	}
	if (a.isBuiltin() && a.toBuiltin() == RB::any) {
		return false;
	}

	/* flipped? */
	if (a.isInverse() != b.isInverse()) {
		return false;
	}

	/* r&loc <= r */
	if (hasPerLoc(b) && getPerLoc(b) == a) {
		return true;
	}

	/* special cases for builtins */
	if (!a.isBuiltin() || !b.isBuiltin()) {
		return false;
	}

	auto lb = a.toBuiltin();
	auto rb = b.toBuiltin();

	/* rfe, rfi <= rf */
	if (rb == RB::rf && (lb == RB::rfi || lb == RB::rfe)) {
		return true;
	}

	/* moi, rfi, fri <= po,po-loc */
	if ((rb == RB::po_imm || rb == RB::po_loc_imm) && RB::WithinPoBegin <= lb &&
	    lb <= RB::WithinPoLast) {
		return true;
	}

	return false;
}

auto Theory::isIncludedIn(const Predicate &a, const Predicate &b) const -> bool
{
	return a == b ||
	       (!a.isComplement() && !b.isComplement() && isSubPredOf(a.getID(), b.getID())) ||
	       (a.isComplement() && b.isComplement() && isSubPredOf(b.getID(), a.getID()));
}

auto Theory::isIncludedIn(const PredicateSet &a, const PredicateSet &b) const -> bool
{
	/* [...] <= [] */
	if (b.empty())
		return true;
	if (a.empty())
		return false;

	/* Otherwise, A has to be more specific than B */
	namespace RG = std::ranges;
	return a.contains(b) || RG::all_of(b.preds(), [&](auto &bp) {
		       /* Each predicate of B needs to have a more specific one in A */
		       return RG::find_if(a.preds(), [&](auto &ap) {
				      return isIncludedIn(ap, bp);
			      }) != a.end();
	       });
}

auto Theory::isIncludedIn(const TransLabel &a, const TransLabel &b) const -> bool
{
	return isIncludedIn(a.getPreChecks(), b.getPreChecks()) &&
	       (!a.isRelation() ||
		(b.isRelation() && isIncludedIn(*a.getRelation(), *b.getRelation()))) &&
	       isIncludedIn(a.getPostChecks(), b.getPostChecks());
}

auto Theory::composes(const PredicateSet &preds1, const PredicateSet &preds2) const -> bool
{
	if (preds1.empty() || preds2.empty())
		return true;
	if (preds1 == preds2)
		return true;
	/* Whether exists p1 in PREDS1 and p2 in PREDS2 s.t. p1 = ~p2 */
	for (const auto &p1 : preds1) {
		for (const auto &p2 : preds2) {
			if (p1.getID() == p2.getID() && p1.isComplement() != p2.isComplement())
				return false;
		}
	}

	/* Whether PREDS contains any pair (p1,p2) s.t. p1 in PREDS1 and p2 in PREDS2 */
	auto hasPairFun = [&](const VSet<Predicate::ID> &preds) {
		for (const auto &p1 : preds1) {
			for (const auto &p2 : preds2) {
				if (p1.getID() != p2.getID() && !p1.isComplement() &&
				    !p2.isComplement() && preds.contains(p1.getID()) &&
				    preds.contains(p2.getID()))
					return true;
			}
		}
		return false;
	};
	if (std::ranges::any_of(disjointPreds_, hasPairFun))
		return false;

	/* Whether the combination has been declared invalid */
	VSet<Predicate::ID> combo;
	for (const auto &p1 : preds1)
		combo.insert(p1.getID());
	for (const auto &p2 : preds2)
		combo.insert(p2.getID());
	return isValid(combo);
}

auto Theory::composes(const TransLabel &a, const TransLabel &b) const -> bool
{
	// Should we register relations w/ domains builtin and simplify to below?
	// Then we would only have to saturate user-provided relations.
	//
	// return a.isPredicate() ?
	//        composes(a.getPreChecks(), b.getPreChecks()) :
	//        composes(a.getPostChecks(), b.getPreChecks());

	if (a.isPredicate() && b.isPredicate())
		return composes(a.getPreChecks(), b.getPreChecks());

	if (!a.isPredicate() && b.isPredicate()) {
		return composes(a.getPostChecks(), b.getPreChecks()) &&
		       (!hasInfo(*a.getRelation()) ||
			composes(getCodomain(*a.getRelation()), b.getPreChecks()));
	}

	if (a.isPredicate() && !b.isPredicate()) {
		return composes(a.getPreChecks(), b.getPreChecks()) &&
		       (!hasInfo(*b.getRelation()) ||
			composes(a.getPreChecks(), getDomain(*b.getRelation())));
	}

	return composes(a.getPostChecks(), b.getPreChecks()) &&
	       (!hasInfo(*a.getRelation()) ||
		(composes(getCodomain(*a.getRelation()), b.getPreChecks()) &&
		 (!hasInfo(*b.getRelation()) ||
		  composes(getCodomain(*a.getRelation()), getDomain(*b.getRelation()))))) &&
	       (!hasInfo(*b.getRelation()) ||
		composes(a.getPostChecks(), getDomain(*b.getRelation())));
}

auto Theory::tryIncorporateDomAssumption(const Constraint *cst) -> bool
{
	/*
	 * LHS has to be a base relation; RHS a base or a sequence.
	 * (Given the parsing assumption, RHS will be a sequence but this
	 * function can be a bit more generic.)
	 */
	const auto *subsetC = dynamic_cast<const SubsetConstraint *>(cst);
	if (!subsetC || !dynamic_cast<const CharRE *>(subsetC->getLHS()) ||
	    (!dynamic_cast<const SeqRE *>(subsetC->getRHS()) &&
	     !dynamic_cast<const CharRE *>(subsetC->getRHS())))
		return false;

	/* Ensure LHS is _just_ a base relation */
	const auto *lhs = dynamic_cast<const CharRE *>(subsetC->getLHS());
	if (!lhs->getLabel().isRelation() || !lhs->getLabel().getPreChecks().empty() ||
	    !lhs->getLabel().getPostChecks().empty())
		return false;

	PredicateSet dom;
	PredicateSet codom;
	/* Extract dom/codom if RHS contains the same base relation */
	if (const auto *rhs = dynamic_cast<const CharRE *>(subsetC->getRHS())) {
		/* If RHS is a base relation, it has to match the LHS */
		if (lhs->getLabel().getRelation() != rhs->getLabel().getRelation())
			return false;
		dom = rhs->getLabel().getPreChecks();
		codom = rhs->getLabel().getPostChecks();
	} else if (const auto *rhs = dynamic_cast<const SeqRE *>(subsetC->getRHS())) {
		/* If RHS is a sequence, it needs to contain exactly one relation */
		auto relIt = std::ranges::find_if(rhs->kids(), [&](auto &re) {
			const auto *charRE = dynamic_cast<const CharRE *>(&*re);
			return charRE &&
			       charRE->getLabel().getRelation() == lhs->getLabel().getRelation();
		});
		if (relIt == rhs->kid_end() ||
		    std::ranges::any_of(rhs->kid_begin(), relIt,
					[&](auto &re) { return !re->isPredicate(); }) ||
		    std::ranges::any_of(relIt + 1, rhs->kid_end(),
					[&](auto &re) { return !re->isPredicate(); }))
			return false;

		dom = std::accumulate(rhs->kid_begin(), relIt, PredicateSet{},
				      [&](auto acc, auto &re) {
					      auto *charRE = dynamic_cast<const CharRE *>(&*re);
					      acc.insert(charRE->getLabel().getPreChecks());
					      return acc;
				      });
		codom = std::accumulate(relIt + 1, rhs->kid_end(), PredicateSet{},
					[&](auto acc, auto &re) {
						auto *charRE = dynamic_cast<const CharRE *>(&*re);
						acc.insert(charRE->getLabel().getPreChecks());
						return acc;
					});
	} else {
		assert(0);
	}

	registerDomain(*lhs->getLabel().getRelation(), dom);
	registerCodomain(*lhs->getLabel().getRelation(), codom);
	return true;
}

auto Theory::tryIncorporateIncompatAssumption(const Constraint *cst) -> bool
{
	if (!cst->isEmpty() || !dynamic_cast<const SeqRE *>(cst->getKid(0)) ||
	    std::ranges::any_of(cst->getKid(0)->kids(),
				[&](auto &re) { return !re->isPredicate(); }))
		return false;

	const auto *seqRE = dynamic_cast<const SeqRE *>(cst->getKid(0));
	assert(std::ranges::all_of(seqRE->kids(), [](auto &re) {
		return re->isPredicate() &&
		       dynamic_cast<const CharRE *>(&*re)->getLabel().getPreChecks().size() == 1;
	}));

	/* Collect all incompatible predicates */
	auto incompat = std::accumulate(seqRE->kid_begin(), seqRE->kid_end(), VSet<Predicate::ID>{},
					[](auto acc, const auto &re) {
						acc.insert(dynamic_cast<const CharRE *>(&*re)
								   ->getLabel()
								   .getPreChecks()
								   .begin()
								   ->getID());
						return acc;
					});

	/* Should this be incorporates to disjoint preds? */
	if (incompat.size() == 2) {
		registerDisjointPreds(incompat);
	} else {
		/* It's an invalidity assumption */
		registerInvalidPreds(incompat);
	}
	return true;
}

auto Theory::simplify() -> Theory &
{
	/*
	 * We can drop assumptions of the following forms:
	 *   - r <= [A];r;[B], where r is some base relation ([A] and [B] are optional)
	 *   - [A;...;B] <= 0
	 */
	auto isDroppable = [this](auto &cstUP) {
		return tryIncorporateDomAssumption(&*cstUP) ||
		       tryIncorporateIncompatAssumption(&*cstUP);
	};

	assumes_.erase(std::remove_if(assume_begin(), assume_end(), isDroppable), assume_end());
	return *this;
}
