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

#ifndef KATER_CONSTRAINT_HPP
#define KATER_CONSTRAINT_HPP

#include "RegExp.hpp"
#include "Visitor.hpp"

/*******************************************************************************
 **                           Constraint Class (Abstract)
 ******************************************************************************/

class Constraint : public VisitableContainer {

public:
	using ValidFunT = std::function<bool(const TransLabel &)>;

protected:
	Constraint() = default;

public:
	Constraint(const Constraint &) = delete;
	Constraint(Constraint &&) = default;
	auto operator=(const Constraint &) -> Constraint & = delete;
	auto operator=(Constraint &&) -> Constraint & = default;
	virtual ~Constraint() = default;

	static auto createEmpty(std::unique_ptr<RegExp> re, bool sameEnds, bool rotate)
		-> std::unique_ptr<Constraint>;

	[[nodiscard]] auto isEmpty() const -> bool;

	/** Returns a clone of the Constraint */
	[[nodiscard]] virtual auto clone() const -> std::unique_ptr<Constraint> = 0;

	friend auto operator<<(std::ostream &s, const Constraint &cst) -> std::ostream &;

private:
	/** Printing helper */
	virtual auto dump(std::ostream &s) const -> std::ostream & = 0;

	[[nodiscard]] auto isContainer() const -> bool override { return false; }

	void visitChildren(BaseVisitor &visitor) const override {}
};

inline auto operator<<(std::ostream &s, const Constraint &cst) -> std::ostream &
{
	return cst.dump(s);
}

/*******************************************************************************
 **                           Acyclicity Constraints
 ******************************************************************************/

class AcyclicConstraint : public Constraint {

protected:
	AcyclicConstraint(std::unique_ptr<RegExp> re) : re_(std::move(re)) {}

public:
	template <typename... Ts>
	static auto create(Ts &&...params) -> std::unique_ptr<AcyclicConstraint>
	{
		return std::unique_ptr<AcyclicConstraint>(
			new AcyclicConstraint(std::forward<Ts>(params)...));
	}

	/* NOTE: Might not return AcyclicConstraint */
	static auto createOpt(std::unique_ptr<RegExp> re) -> std::unique_ptr<Constraint>;

	/** Returns the regexp that should be acyclic */
	[[nodiscard]] auto getRE() const -> const RegExp * { return &*re_; }
	auto getRE() -> RegExp * { return &*re_; }

	[[nodiscard]] auto getRERef() const -> const std::unique_ptr<RegExp> & { return re_; }
	auto getRERef() -> std::unique_ptr<RegExp> & { return re_; }

	[[nodiscard]] auto clone() const -> std::unique_ptr<Constraint> override
	{
		return create(getRE()->clone());
	}

private:
	auto dump(std::ostream &s) const -> std::ostream & override
	{
		return s << "acyclic" << *getRE();
	}

	std::unique_ptr<RegExp> re_{};
};

/*******************************************************************************
 **                           Coherence Constraints
 ******************************************************************************/

class CoherenceConstraint : public Constraint {

protected:
	CoherenceConstraint(std::string id) : id_(std::move(id)) {}

public:
	template <typename... Ts>
	static auto create(Ts &&...params) -> std::unique_ptr<CoherenceConstraint>
	{
		return std::unique_ptr<CoherenceConstraint>(
			new CoherenceConstraint(std::forward<Ts>(params)...));
	}

	/* NOTE: Might not return CoherenceConstraint */
	static auto createOpt(std::string id) -> std::unique_ptr<Constraint>;

	/** Returns the relation name on which coherence is calculated */
	[[nodiscard]] auto getID() const -> const std::string & { return id_; }

	void setID(std::string id) { id_ = std::move(id); }

	[[nodiscard]] auto clone() const -> std::unique_ptr<Constraint> override
	{
		return create(getID());
	}

private:
	auto dump(std::ostream &s) const -> std::ostream & override
	{
		return s << "coherence " << getID();
	}

	std::string id_;
};

/*******************************************************************************
 **                           Subset Constraints
 ******************************************************************************/

class SubsetConstraint : public Constraint {

protected:
	SubsetConstraint(std::unique_ptr<RegExp> re1, std::unique_ptr<RegExp> re2, bool sameEnds,
			 bool rotate)
		: lhs_(std::move(re1)), rhs_(std::move(re2)), sameEnds_(sameEnds), rotate_(rotate)
	{
	}

public:
	template <typename... Ts>
	static auto create(Ts &&...params) -> std::unique_ptr<SubsetConstraint>
	{
		return std::unique_ptr<SubsetConstraint>(
			new SubsetConstraint(std::forward<Ts>(params)...));
	}

	static auto createOpt(std::unique_ptr<RegExp> re1, std::unique_ptr<RegExp> re2,
			      bool sameEnds, bool rotate) -> std::unique_ptr<SubsetConstraint>;

	[[nodiscard]] auto getLHS() const -> const RegExp * { return &*lhs_; }
	auto getLHS() -> RegExp * { return &*lhs_; }

	[[nodiscard]] auto getLHSRef() const -> const std::unique_ptr<RegExp> & { return lhs_; }
	auto getLHSRef() -> std::unique_ptr<RegExp> & { return lhs_; }

	[[nodiscard]] auto getRHS() const -> const RegExp * { return &*rhs_; }
	auto getRHS() -> RegExp * { return &*rhs_; }

	[[nodiscard]] auto getRHSRef() const -> const std::unique_ptr<RegExp> & { return rhs_; }
	auto getRHSRef() -> std::unique_ptr<RegExp> & { return rhs_; }

	[[nodiscard]] auto sameEnds() const -> bool { return sameEnds_; }
	[[nodiscard]] auto rotated() const -> bool { return rotate_; }

	[[nodiscard]] auto clone() const -> std::unique_ptr<Constraint> override
	{
		return create(getLHS()->clone(), getRHS()->clone(), sameEnds(), rotated());
	}

	auto dump(std::ostream &s) const -> std::ostream & override
	{
		return s << *getLHS() << " <= " << *getRHS();
	}

private:
	std::unique_ptr<RegExp> lhs_{};
	std::unique_ptr<RegExp> rhs_{};
	bool sameEnds_{};
	bool rotate_{};
};

/*******************************************************************************
 **                           Equality Constraint
 ******************************************************************************/

/*
 * An EqualityConstraint is a SubsetConstraint that holds both ways
 */
class EqualityConstraint : public SubsetConstraint {

protected:
	EqualityConstraint(std::unique_ptr<RegExp> re1, std::unique_ptr<RegExp> re2,
			   bool sameEndsLHS, bool rotateRHS)
		: SubsetConstraint(std::move(re1), std::move(re2), sameEndsLHS, rotateRHS)
	{
	}

public:
	template <typename... Ts>
	static auto create(Ts &&...params) -> std::unique_ptr<EqualityConstraint>
	{
		return std::unique_ptr<EqualityConstraint>(
			new EqualityConstraint(std::forward<Ts>(params)...));
	}

	static auto createOpt(std::unique_ptr<RegExp> e1, std::unique_ptr<RegExp> e2, bool sameEnds,
			      bool rotateRHS) -> std::unique_ptr<Constraint>;

	[[nodiscard]] auto clone() const -> std::unique_ptr<Constraint> override
	{
		return create(getLHS()->clone(), getRHS()->clone(), sameEnds(), rotated());
	}

	auto dump(std::ostream &s) const -> std::ostream & override
	{
		return s << *getLHS() << " = " << *getRHS() << "\n";
	}
};

/*******************************************************************************
 **                           Totality Constraint
 ******************************************************************************/

class TotalityConstraint : public Constraint {

protected:
	TotalityConstraint(std::unique_ptr<RegExp> re) : re_(std::move(re)) {}

public:
	template <typename... Ts>
	static auto create(Ts &&...params) -> std::unique_ptr<TotalityConstraint>
	{
		return std::unique_ptr<TotalityConstraint>(
			new TotalityConstraint(std::forward<Ts>(params)...));
	}

	static auto createOpt(std::unique_ptr<RegExp> re) -> std::unique_ptr<Constraint>
	{
		return create(std::move(re));
	}

	[[nodiscard]] auto getRE() const -> const RegExp * { return &*re_; }
	auto getRE() -> RegExp * { return &*re_; }

	[[nodiscard]] auto clone() const -> std::unique_ptr<Constraint> override
	{
		return create(getRE()->clone());
	}

private:
	auto dump(std::ostream &s) const -> std::ostream & override
	{
		return s << "total " << *getRE();
	}

	std::unique_ptr<RegExp> re_{};
};

/*******************************************************************************
 **                           Warning Constraint
 ******************************************************************************/

class WarningConstraint : public Constraint {
protected:
	WarningConstraint(std::string warningID) : warningName_(std::move(warningID)) {}

public:
	template <typename... Ts>
	static auto create(Ts &&...params) -> std::unique_ptr<WarningConstraint>
	{
		return std::unique_ptr<WarningConstraint>(
			new WarningConstraint(std::forward<Ts>(params)...));
	}

	/** Returns the ID of the warning */
	[[nodiscard]] auto getWarningName() const -> const std::string & { return warningName_; }

	[[nodiscard]] auto clone() const -> std::unique_ptr<Constraint> override
	{
		return create(getWarningName());
	}

private:
	auto dump(std::ostream &s) const -> std::ostream & override
	{
		return s << "warning(" << getWarningName();
	}

	std::string warningName_{};
};

/*******************************************************************************
 **                           Error Constraint
 ******************************************************************************/

/*
 * An error is just a really important warning
 */
class ErrorConstraint : public WarningConstraint {

protected:
	ErrorConstraint(std::string errorID) : WarningConstraint(std::move(errorID)) {}

public:
	template <typename... Ts>
	static auto create(Ts &&...params) -> std::unique_ptr<ErrorConstraint>
	{
		return std::unique_ptr<ErrorConstraint>(
			new ErrorConstraint(std::forward<Ts>(params)...));
	}

	[[nodiscard]] auto clone() const -> std::unique_ptr<Constraint> override
	{
		return create(getWarningName());
	}

private:
	auto dump(std::ostream &s) const -> std::ostream & override
	{
		return s << "error(" << getWarningName();
	}
};

#endif /* KATER_CONSTRAINT_HPP */
