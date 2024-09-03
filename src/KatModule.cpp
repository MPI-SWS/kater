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
#include "RegExpUtils.hpp"

void KatModule::registerExport(std::unique_ptr<ExportStatement> stmt, const yy::location &loc)
{
	if (auto *cohCst = dynamic_cast<const CoherenceConstraint *>(stmt->getConstraint())) {
		registerCOH(getRegisteredStatement(cohCst->getID()));
	}
	exports_.push_back(std::move(stmt));
}

void KatModule::replaceAllUsesWith(const RegExp *from, const RegExp *to)
{
	/* A visitor to easily recurse over constraints */
	auto visitor = make_visitor(
		type_list<AcyclicConstraint, SubsetConstraint>{},
		[&](const AcyclicConstraint &cst) {
			replaceREWith(const_cast<AcyclicConstraint &>(cst).getRERef(), from, to);
		},
		[&](const SubsetConstraint &cst) {
			replaceREWith(const_cast<SubsetConstraint &>(cst).getLHSRef(), from, to);
			replaceREWith(const_cast<SubsetConstraint &>(cst).getRHSRef(), from, to);
		});

	/* Go over everything and replace */
	for (auto &let : lets()) {
		replaceREWith(let->getRERef(), from, to);
		auto &savedUP = let->getSavedRef();
		if (auto *vexp = dynamic_cast<ViewExp *>(&*savedUP))
			replaceREWith(vexp->getRERef(), from, to);
		else if (auto *sexp = dynamic_cast<SetExp *>(&*savedUP))
			replaceREWith(sexp->getRERef(), from, to);
	}
	for (auto &assm : getTheory().assumes()) {
		visitor(*assm->getConstraint());
	}
	for (auto &assrt : asserts()) {
		visitor(*assrt->getConstraint());
	}
	for (auto &stmt : exports()) {
		visitor(*stmt->getConstraint());
		if (stmt->getUnless())
			visitor(*stmt->getUnless());
	}
}
