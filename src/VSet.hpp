/*
 * GenMC -- Generic Model Checking.
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
 *
 * Author: Michalis Kokologiannakis <michalis@mpi-sws.org>
 */

#ifndef __VSET_HPP__
#define __VSET_HPP__

#include <algorithm>
#include <functional>
#include <initializer_list>
#include <iostream>
#include <vector>

template <class T> class VSet {
	/* Pre: Set needs to support random_access iterators */
	using Set = std::vector<T>;

public:
	VSet() = default;
	VSet(const VSet &) = default;
	VSet(VSet &&) noexcept = default;

	/* Pre: v is sorted and distinct */
	VSet(const std::vector<T> &v) : vset_(v) {}

	/* Pre: v is sorted and distinct */
	VSet(std::vector<T> &&v) : vset_(std::move(v)){};

	template <typename ITER> VSet(ITER begin, ITER end)
	{
		for (; begin != end; ++begin) {
			if (size() && max() < *begin) {
				vset_.push_back(*begin);
			} else {
				insert(*begin);
			}
		}
	}

	VSet(std::initializer_list<T> il)
	{
		for (auto it = il.begin(); it != il.end(); ++it) {
			if (size() && max() < *it) {
				vset_.push_back(*it);
			} else {
				insert(*it);
			}
		}
	}

	virtual ~VSet() = default;

	using const_iterator = typename Set::const_iterator;
	using const_reverse_iterator = typename Set::const_reverse_iterator;

	auto begin() const -> const_iterator { return vset_.cbegin(); };
	auto end() const -> const_iterator { return vset_.cend(); };
	auto rbegin() const -> const_reverse_iterator { return vset_.crbegin(); };
	auto rend() const -> const_reverse_iterator { return vset_.crend(); };

	auto insert(const T &el) -> std::pair<const_iterator, bool>
	{
		auto it = std::lower_bound(begin(), end(), el);
		return (it == end() || *it != el) ? std::make_pair(vset_.insert(it, el), true)
						  : std::make_pair(it, false);
	}

	template <typename ITER> void insert(ITER begin, ITER end)
	{
		for (; begin != end; ++begin) {
			if (size() && max() < *begin) {
				vset_.push_back(*begin);
			} else {
				insert(*begin);
			}
		}
	}

	/** A slightly optimized function for bulk insertions, that takes
	 * into account the structure of a VSet  */
	auto insert(const VSet<T> &s) -> int;

	/** A slightly optimized function for bulk deletion */
	auto erase(const T &el) -> int
	{
		auto it = std::lower_bound(begin(), end(), el);

		if (it == end() || *it != el)
			return 0;

		vset_.erase(it);
		return 1;
	}

	auto erase(const VSet<T> &S) -> int;

	template <typename F> void erase_if(F &&fun)
	{
		vset_.erase(std::remove_if(vset_.begin(), vset_.end(), fun), vset_.end());
	}

	auto count(const T &el) const -> int { return (find(el) != end()) ? 1 : 0; }

	auto contains(const T &t) const -> bool { return count(t) > 0; }

	auto find(const T &el) const -> const_iterator
	{
		auto it = std::lower_bound(begin(), end(), el);
		return (it == end() || *it != el) ? end() : it;
	}

	[[nodiscard]] auto size() const -> size_t { return vset_.size(); };

	[[nodiscard]] auto empty() const -> bool { return vset_.empty(); };

	void clear() { vset_.clear(); };

	auto subsetOf(const VSet<T> &s) const -> bool;

	[[nodiscard]] auto intersects(const VSet<T> &s) const -> bool;
	auto intersectWith(const VSet<T> &s) const -> VSet<T>;

	auto min() const -> const T & { return vset_[0]; };
	auto max() const -> const T & { return vset_.back(); };

	inline auto operator[](int i) const -> const T & { return vset_[i]; };

	auto operator<=>(const VSet<T> &) const = default;
	auto operator=(const VSet &) -> VSet & = default;
	auto operator=(VSet &&) noexcept -> VSet & = default;

	template <typename U>
	friend auto operator<<(std::ostream &s, const VSet<U> &set) -> std::ostream &;

private:
	Set vset_;
};

template <typename T> auto VSet<T>::insert(const VSet<T> &s) -> int
{
	/* Simply copy the contents of s if the current set is empty */
	if (empty()) {
		*this = s;
		return s.size();
	}

	/* Check s for trivial cases */
	if (s.empty())
		return 0;
	if (s.size() == 1)
		return insert(s[0]).second ? 1 : 0;

	/*
	 * First, count the elements of s not in this set, by iterating
	 * over the two sets in parallel
	 */
	auto count = 0;
	auto a = begin();
	auto b = s.begin();
	while (a != end() && b != s.end()) {
		/* If a[i] < b[i], maybe b[i] exists later in a */
		if (*a < *b) {
			++a;
		} else if (*a == *b) { /* b[i] exists in a, skip */
			++a;
			++b;
		} else { /* b[i] does not exist in a, increase count */
			++b;
			++count;
		}
	}

	/*
	 * If there are still elements in b that have not been processed,
	 * these should all should be inserted in a
	 */
	if (b != s.end())
		count += s.end() - b;

	if (count == 0)
		return 0;

	/*
	 * We will make the insertion in-place, in O(size(a) + size(b)) time.
	 * The new size of a will be size(a) + count. We do not use iterators
	 * because we need to resize a, and this would invalidate them.
	 */

	/* Keep the index of the last elements of a and b before resizing */
	int idxA = size() - 1;
	int idxB = s.size() - 1;
	vset_.resize(vset_.size() + count, vset_[0]); /* a is not empty */

	/* Iterate over the new a, and move fill each position appropriately */
	for (int i = size() - 1; i >= 0; i--) {
		if (idxA < 0 || (idxB >= 0 && (*this)[idxA] < s[idxB])) {
			/* No more elements in a, or a[idxA] < b[idxB] */
			vset_[i] = s[idxB];
			--idxB;
		} else if (idxA >= 0 && idxB >= 0 && (*this)[idxA] == s[idxB]) {
			/* since equal, it does not matter from where we copy */
			vset_[i] = (*this)[idxA];
			--idxA;
			--idxB;
		} else {
			/* No more elements in b, or a[i] > b[i] */
			vset_[i] = (*this)[idxA];
			--idxA;
		}
	}
	return count;
}

template <typename T> auto VSet<T>::erase(const VSet<T> &s) -> int
{
	if (empty() || s.empty())
		return 0;

	auto erased = 0;
	auto a = 0;    /* index in this */
	auto b = 0;    /* index in other */
	auto aMov = 0; /* Next position of this set to be filled */

	/* While iterating over the two sets, fill a appropriately */
	while (a < size() && b < s.size()) {
		/*
		 * This element of a should be erased. aMov is left unchanged; *
		 * it is pointing to the next position that needs to be filled.
		 * We will iterate over a and b to find the appropriate element
		 * to fill *aMov (this should be an element of a)
		 */
		if (vset_[a] == s.vset_[b]) {
			++a;
			++b;
			++erased;
		} else if (vset_[a] < s.vset_[b]) {
			/* This element of a should remain in the set */
			if (aMov != a)
				vset_[aMov] = vset_[a];
			++a;
			++aMov;
		} else {
			/* *a > *b, we need to check if *a appears later in b */
			++b;
		}
	}

	/* If we stopped whilst trying to find the next element for aMov... */
	if (aMov != a) {
		/* If a is not over copy the remaining elements */
		while (a < size()) {
			vset_[aMov] = vset_[a];
			++a;
			++aMov;
		}
		/* Resize the vector appropriately */
		vset_.resize(aMov, vset_[0]);
	}
	return erased;
}

template <typename T> auto VSet<T>::subsetOf(const VSet<T> &s) const -> bool
{
	if (size() > s.size())
		return false;

	auto a = begin();
	auto b = s.begin();
	while (a != end()) {
		/* If the remaining elements of a are more than those of b */
		if ((end() - a) > (s.end() - b))
			return false;
		/* If a contains an element not in b */
		if (*a < *b)
			return false;

		if (*a == *b) {
			++a;
			++b;
		} else {
			++b; /* Need to check further in b */
		}
	}
	return true;
}

template <typename T> auto VSet<T>::intersects(const VSet<T> &s) const -> bool
{
	auto a = begin();
	auto b = s.begin();
	while (a != end() && b != s.end()) {
		if (*a == *b)
			return true;

		if (*a < *b)
			++a;
		else
			++b;
	}
	return false;
}

template <typename T> VSet<T> VSet<T>::intersectWith(const VSet<T> &s) const
{
	VSet<T> result;

	auto a = begin();
	auto b = s.begin();
	while (a != end() && b != s.end()) {
		if (*a == *b) {
			result.insert(*a);
			++a;
			++b;
		} else if (*a < *b) {
			++a;
		} else {
			++b;
		}
	}
	return result;
}

template <typename T> auto operator<<(std::ostream &s, const VSet<T> &set) -> std::ostream &
{
	s << "[ ";
	for (auto i = 0U; i < set.size(); i++)
		s << set[i] << " ";
	s << "]";
	return s;
}

#endif /* __VSET_HPP__ */
