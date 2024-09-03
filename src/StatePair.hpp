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

#ifndef KATER_STATE_PAIR_HPP
#define KATER_STATE_PAIR_HPP

#include "NFA.hpp"

#include <utility>

using StatePair = std::pair<NFA::State *, NFA::State *>;

template <typename T> inline void hash_combine(std::size_t &seed, std::size_t value)
{
	std::hash<T> hasher;
	seed ^= hasher(value) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
}

struct StatePairHasher {
	auto operator()(const StatePair &p) const -> std::size_t
	{
		std::size_t hash = 0;
		hash_combine<unsigned>(hash, p.first->getId());
		hash_combine<unsigned>(hash, p.first->getId());
		return hash;
	}
};

#endif /* KATER_STATE_PAIR_HPP */
