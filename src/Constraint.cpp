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

#include "Constraint.hpp"

auto Constraint::createEmpty(std::unique_ptr<RegExp> re, bool sameEnds, bool rotate)
	-> std::unique_ptr<Constraint>
{
	return SubsetConstraint::createOpt(std::move(re), RegExp::createFalse(), sameEnds, rotate);
}

auto Constraint::isEmpty() const -> bool
{
	const auto *subsetC = dynamic_cast<const SubsetConstraint *>(this);
	return subsetC && subsetC->getRHS()->isFalse();
}

auto EqualityConstraint::createOpt(std::unique_ptr<RegExp> lhs, std::unique_ptr<RegExp> rhs,
				   bool sameEnds, bool rotate) -> std::unique_ptr<Constraint>
{
	if (lhs->isFalse())
		return createEmpty(std::move(rhs), sameEnds, rotate);
	if (rhs->isFalse())
		return createEmpty(std::move(lhs), sameEnds, rotate);
	auto cUP = SubsetConstraint::createOpt(std::move(lhs), std::move(rhs), sameEnds, rotate);
	auto *subsetC = static_cast<SubsetConstraint *>(&*cUP);
	return EqualityConstraint::create(subsetC->getLHS()->clone(), subsetC->getRHS()->clone(),
					  subsetC->sameEnds(), subsetC->rotated());
}

auto SubsetConstraint::createOpt(std::unique_ptr<RegExp> lhs, std::unique_ptr<RegExp> rhs,
				 bool sameEnds, bool rotate) -> std::unique_ptr<SubsetConstraint>
{
	/* Convert `A <= rot B` to `A <= rotate(B)` */
	if (dynamic_cast<RotRE *>(&*rhs))
		return SubsetConstraint::createOpt(std::move(lhs), rhs->releaseKid(0), sameEnds,
						   true);

	/* Convert `rot A <= rotate(B)` to `A <=_rot B` */
	if (rotate && dynamic_cast<RotRE *>(&*lhs))
		return SubsetConstraint::createOpt(lhs->releaseKid(0), std::move(rhs), sameEnds,
						   true);

	/* Convert `sameEnds(A) <= B & id` to `sameEnds(A) <= B` */
	if (sameEnds && dynamic_cast<AndRE *>(&*rhs) && *rhs->getKid(1) == *RegExp::createId())
		return SubsetConstraint::createOpt(std::move(lhs), rhs->releaseKid(0), true,
						   rotate);

	/* Convert `A & id <= B` to `sameEnds(A) <= B` */
	if (dynamic_cast<AndRE *>(&*lhs) && *lhs->getKid(1) == *RegExp::createId()) {
		return SubsetConstraint::createOpt(lhs->releaseKid(0), std::move(rhs), true,
						   rotate);
	}

	/* Convert `A \ B <= C` to `A <= B | C` */
	if (!rotate && dynamic_cast<MinusRE *>(&*lhs)) {
		return SubsetConstraint::createOpt(
			lhs->releaseKid(0), AltRE::create(std::move(rhs), lhs->releaseKid(1)),
			sameEnds, rotate);
	}

	return create(std::move(lhs), std::move(rhs), sameEnds, rotate);
}

auto AcyclicConstraint::createOpt(std::unique_ptr<RegExp> re) -> std::unique_ptr<Constraint>
{
	/* Convert `acyclic (re+)` to `acyclic (re)` */
	if (auto *plusRE = dynamic_cast<PlusRE *>(&*re)) {
		return AcyclicConstraint::createOpt(plusRE->releaseKid(0));
	}

	return create(std::move(re));
}

auto CoherenceConstraint::createOpt(std::string id) -> std::unique_ptr<Constraint>
{
	return create(std::move(id));
}
