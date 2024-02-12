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

#ifndef KATER_COUNTEREXAMPLE_HPP
#define KATER_COUNTEREXAMPLE_HPP

#include "TransLabel.hpp"
#include <iostream>
#include <vector>

class Counterexample {

private:
	using ContainerT = std::vector<TransLabel>;

public:
	enum Type {
		NONE,
		TUT,
		ANA,
	};

	using iterator = ContainerT::iterator;
	using const_iterator = ContainerT::const_iterator;

	Counterexample() = default;

	auto begin() -> iterator { return cex.begin(); }
	auto end() -> iterator { return cex.end(); }
	[[nodiscard]] auto begin() const -> const_iterator { return cex.begin(); }
	[[nodiscard]] auto end() const -> const_iterator { return cex.end(); }

	[[nodiscard]] auto empty() const -> bool { return cex.empty(); }
	[[nodiscard]] auto size() const -> unsigned int { return cex.size(); }

	void extend(const TransLabel &lab) { cex.push_back(lab); }

	void clear()
	{
		cex.clear();
		typ = Type::NONE;
		index = 0;
	}

	[[nodiscard]] auto getMismatch() const -> unsigned int { return index; }

	[[nodiscard]] auto getType() const -> Type { return typ; }

	void setType(Type t)
	{
		typ = t;
		if (t == TUT) {
			assert(!cex.empty());
			index = cex.size() - 1;
		}
	}

	friend auto operator<<(std::ostream &ostr, const Counterexample &c) -> std::ostream &;

private:
	ContainerT cex;
	unsigned int index = 0;
	Type typ = Type::NONE;
};

#endif /* KATER_COUNTEREXAMPLE_HPP */
