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

#include "KatModule.hpp"

std::ostream &operator<<(std::ostream &s, const KatModule::DbgInfo &dbg)
{
	return s << dbg.filename << ":" << dbg.line;
}

void KatModule::addConstraint(const Constraint *c, const std::string &s, const yy::location &loc)
{
	if (dynamic_cast<const AcyclicConstraint *>(c) && c->getType() == Constraint::Type::Consistency)
		acyclicityConstraints.push_back(c->getKid(0)->clone());
	else if (dynamic_cast<const RecoveryConstraint *>(c) && c->getType() == Constraint::Type::Consistency)
		recoveryConstraints.push_back(c->getKid(0)->clone());
	else if (dynamic_cast<const CoherenceConstraint *>(c))
		coherenceConstraints.push_back(c->getKid(0)->clone());
	else if (dynamic_cast<const SubsetConstraint *>(c))
		inclusionConstraints.push_back({c->getKid(0)->clone(),
					        c->getKid(1)->clone(), c->getType(), s});
	else if (auto conj = dynamic_cast<const ConjunctiveConstraint *>(c)) {
		addConstraint(conj->getConstraint1(), s, loc);
		addConstraint(conj->getConstraint2(), s, loc);
	} else {
		std::cerr << loc << ": [Warning] Ignoring the unsupported constraint:" << *c << "\n";
	}
}
