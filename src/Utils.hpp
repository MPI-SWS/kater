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

#ifndef KATER_UTILS_HPP
#define KATER_UTILS_HPP

#include "Error.hpp"
#include "NFA.hpp"

#include <fstream>
#include <iostream>

auto openFileForWriting(const std::string &filename) -> std::ofstream;

void printNFAToDot(const NFA &nfa, const std::string &filename);

#endif /* KATER_UTILS_HPP */
