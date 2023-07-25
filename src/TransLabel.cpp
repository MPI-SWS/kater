#include "TransLabel.hpp"
#include <algorithm>
#include <cassert>
#include <iostream>
#include <set>
#include <sstream>
#include <unordered_map>
#include <vector>

auto TransLabel::merge(const TransLabel &other,
		       const std::function<bool(const TransLabel &)>& isValid) -> bool
{
	if (!isValid(*this) || !isValid(other)) {
		return false;
	}
	if (!isPredicate() && !other.isPredicate()) {
		return false;
	}
	if (!composesWith(other)) {
		return false;
	}

	if (isPredicate()) {
		/* Do not merge into THIS before ensuring combo is valid */
		TransLabel t(*this);
		t.getRelation() = other.getRelation();
		t.getPreChecks().merge(other.getPreChecks());
		assert(t.getPostChecks().empty());
		t.getPostChecks() = other.getPostChecks();
		if (isValid(t)) {
			*this = t;
		}
		return isValid(t);
	}

	assert(other.isPredicate());

	TransLabel t(*this);
	t.getPostChecks().merge(other.getPreChecks());
	if (isValid(t)) {
		*this = t;
	}
	return isValid(t);
}

auto TransLabel::composesWith(const TransLabel &other) const -> bool
{
	if (isPredicate() && other.isPredicate()) {
		return getPreChecks().composes(other.getPreChecks());
	}

	if (!isPredicate() && other.isPredicate()) {
		return getPostChecks().composes(other.getPreChecks()) &&
			(!isBuiltin() || getRelation()->getCodomain().composes(other.getPreChecks()));
	}

	if (isPredicate() && !other.isPredicate()) {
		return getPreChecks().composes(other.getPreChecks()) &&
			(!other.isBuiltin() ||
			 getPreChecks().composes(other.getRelation()->getDomain()));
	}

	return getPostChecks().composes(other.getPreChecks()) &&
		(!isBuiltin() || (getRelation()->getCodomain().composes(other.getPreChecks()) &&
				  (!other.isBuiltin() || getRelation()->getCodomain().composes(
					  other.getRelation()->getDomain())))) &&
		(!other.isBuiltin() ||
		 getPostChecks().composes(other.getRelation()->getDomain()));
}

auto TransLabel::toString() const -> std::string
{
	std::stringstream ss;
	if (isPredicate()) {
		ss << getPreChecks();
	} else {
		if (!getPreChecks().empty()) {
			ss << getPreChecks() << ";";
		}
		ss << getRelation()->getName();
		if (!getPostChecks().empty()) {
			ss << ";" << getPostChecks();
		}
	}
	return ss.str();
}
