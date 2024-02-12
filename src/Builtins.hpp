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

#ifndef KATER_BUILTINS_HPP
#define KATER_BUILTINS_HPP

class KatModule;

/**
 * Given MODULE as input, registers KATER's builtins into the module:
 * names of base and derived relations/predicates (which become available
 * to the parser), as well as its base theory.
 */
void registerBuiltins(KatModule &module);

#endif /* KATER_BUILTINS_HPP */
