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

#include "DbgInfo.hpp"

#include <ostream>

auto operator<<(std::ostream &ostr, const DbgInfo &dbg) -> std::ostream &
{
	/* Mirrors bison's location format: f:l, f:l.c, f:l.c1-c2, f:l1.c1-l2.c2 */
	ostr << dbg.filename << ":" << dbg.line;
	if (dbg.column == 0)
		return ostr;
	ostr << "." << dbg.column;
	if (dbg.endLine == dbg.line && dbg.endColumn > dbg.column)
		ostr << "-" << dbg.endColumn;
	else if (dbg.endLine > dbg.line)
		ostr << "-" << dbg.endLine << "." << dbg.endColumn;
	return ostr;
}
