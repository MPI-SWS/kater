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

#include <config.h>

#ifndef KATER_ERROR_HPP
#define KATER_ERROR_HPP

#define EPARSE 5
#define ECHECK 6
#define EPRINT 7

/* Provide LLVM-style DEBUG_WITH_TYPE utilities */
#ifdef ENABLE_KATER_DEBUG

extern bool katerDebug;

bool isCurrentDebugType(const char *);
void addDebugType(const char *);

#define DEBUG_WITH_TYPE(TYPE, X)					\
do {									\
	if (katerDebug && isCurrentDebugType(TYPE)) { X; }		\
} while (false)

#else /* !ENABLE_KATER_DEBUG */

#define isCurrentDebugType(X) (false)
#define addDebugType(X)
#define DEBUG_WITH_TYPE(TYPE, X) do {} while (0)
#endif /* ENABLE_KATER_DEBUG */

#define KATER_DEBUG(X) DEBUG_WITH_TYPE(DEBUG_TYPE, X)

#endif /* KATER_ERROR_HPP */
