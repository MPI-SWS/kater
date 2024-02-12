#include "Constraint.hpp"
#include "Saturation.hpp"

auto Constraint::createEmpty(std::unique_ptr<RegExp> re, bool sameEnds)
	-> std::unique_ptr<Constraint>
{
	return SubsetConstraint::createOpt(std::move(re), RegExp::createFalse(), sameEnds);
}

auto Constraint::isEmpty() const -> bool
{
	return (dynamic_cast<const SubsetConstraint *>(this) != nullptr) && getNumKids() == 2 &&
	       getKid(1)->isFalse();
}

auto EqualityConstraint::createOpt(std::unique_ptr<RegExp> lhs, std::unique_ptr<RegExp> rhs,
				   bool sameEnds) -> std::unique_ptr<Constraint>
{
	if (lhs->isFalse())
		return createEmpty(std::move(rhs), sameEnds);
	if (rhs->isFalse())
		return createEmpty(std::move(lhs), sameEnds);
	auto cUP = SubsetConstraint::createOpt(std::move(lhs), std::move(rhs), sameEnds);
	auto *subsetC = static_cast<SubsetConstraint *>(&*cUP);
	return EqualityConstraint::create(subsetC->getLHS()->clone(), subsetC->getRHS()->clone(),
					  subsetC->sameEnds());
}

auto SubsetConstraint::createOpt(std::unique_ptr<RegExp> lhs, std::unique_ptr<RegExp> rhs,
				 bool sameEnds) -> std::unique_ptr<Constraint>
{
	/* Convert `A \ B <= C` to `A <= B | C` */
	if (auto *minusRE = dynamic_cast<MinusRE *>(&*lhs)) {
		return SubsetConstraint::createOpt(
			minusRE->releaseKid(0),
			AltRE::create(std::move(rhs), minusRE->releaseKid(1)), sameEnds);
	}
	/* Check if the form is A & id <= B */
	if (auto *andRE = dynamic_cast<AndRE *>(&*lhs)) {
		if (*andRE->getKid(1) != *RegExp::createId())
			return create(std::move(lhs), std::move(rhs), sameEnds);

		/* Convert `(A \ B) & id <= C` to `sameEnds(A) <= B | C` */
		if (auto *minusRE = dynamic_cast<MinusRE *>(&*andRE->getKid(0))) {
			return SubsetConstraint::createOpt(
				minusRE->releaseKid(0),
				AltRE::create(std::move(rhs), minusRE->releaseKid(1)), true);
		}
		/* Convert `A & id <= B & id` to `A & id <= B` */
		if (auto *andRE2 = dynamic_cast<AndRE *>(&*rhs)) {
			if (*andRE->getKid(1) == *RegExp::createId()) {
				return SubsetConstraint::createOpt(std::move(lhs),
								   andRE->releaseKid(0), true);
			}
		}

		/* Convert `A & id <= B` to `sameEnds(A) <= B` */
		return SubsetConstraint::createOpt(andRE->releaseKid(0), std::move(rhs), true);
	}
	return create(std::move(lhs), std::move(rhs), sameEnds);
}

auto AcyclicConstraint::createOpt(std::unique_ptr<RegExp> re, std::unique_ptr<Constraint> o,
				  std::string printInfo) -> std::unique_ptr<Constraint>
{
	/* Convert `acyclic (re+)` to `acyclic (re)` */
	if (auto *plusRE = dynamic_cast<PlusRE *>(&*re)) {
		return AcyclicConstraint::createOpt(plusRE->releaseKid(0), std::move(o),
						    std::move(printInfo));
	}

	return create(std::move(re), std::move(o), std::move(printInfo));
}

auto CoherenceConstraint::createOpt(std::unique_ptr<RegExp> re) -> std::unique_ptr<Constraint>
{
	return create(std::move(re));
}
