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

#ifndef KATER_SATURATION_HPP
#define KATER_SATURATION_HPP

#include "NFA.hpp"

class Theory;

/*
 * Saturates the NFA by (1) conjuncting all outgoing
 * predicates from the initial states with Q (where Q is
 * the disjunction of all incoming predicates to final states),
 * and (2) conjuncting all incoming predicates to final
 * states with P (where P is the disjunction of all outgoing
 * predicates from initial states).
 *
 * Pre: NFA is in normal form
 */
void saturateInitFinalPreds(NFA &nfa, const Theory &theory);

/*
 * If all the preds/successors of a given relation R relation are
 * restricted to a single memory location, it replaces R with its
 * per-loc counterpart
 *
 * Pre: NFA is in normal form
 */
void saturateLoc(NFA &nfa, const Theory &theory);

void saturateID(NFA &nfa, NFA &&id);

void saturateTransitive(NFA &nfa, const Relation &rel);

void saturateBuiltin(NFA &nfa, const Relation &rel, NFA sat, const Theory &theory);

/*
 * Saturates the NFA given an NFA EMPTY that corresponds
 * to a relation R = 0
 */
void saturateEmpty(NFA &nfa, NFA &&empty);

/*
 * Saturates the NFA given a relation REL. total(REL)
 */
void saturateTotal(NFA &nfa, const Relation &rel);

/*
 * Saturates by adding domains/codomains for all builtins.
 * (If a dom/codom does not compose with the existing
 * pre/post-checks of a label, the label is removed.)
 *
 */
void saturateDomains(NFA &nfa, const Theory &theory);

/*
 * Saturates the NFA by rotating it.
 */
void saturateRotate(NFA &nfa, const Theory &theory);

#endif /* KATER_SATURATION_HPP */
