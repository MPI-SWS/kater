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

#include "Counterexample.hpp"
#include "RegExp.hpp"
#include "Visitor.hpp"

/*******************************************************************************
 **                           Constraint Class (Abstract)
 ******************************************************************************/

class Constraint : public VisitableContainer {

public:
	using ValidFunT = std::function<bool(const TransLabel &)>;

	// TODO: Remove
	enum class Type { Consistency, Error, Warning };

protected:
	Constraint(std::vector<std::unique_ptr<RegExp>> &&kids = {}) : kids(std::move(kids)) {}

public:
	virtual ~Constraint() = default;

	[[nodiscard]] auto isEmpty() const -> bool;

	/* Fetches the i-th kid */
	[[nodiscard]] auto getKid(unsigned i) const -> const RegExp *
	{
		assert(i < kids.size() && "Index out of bounds!");
		return kids[i].get();
	}
	auto getKid(unsigned i) -> std::unique_ptr<RegExp> &
	{
		assert(i < kids.size() && "Index out of bounds!");
		return kids[i];
	}

	/* Sets the i-th kid to e */
	void setKid(unsigned i, std::unique_ptr<RegExp> e)
	{
		assert(i < getNumKids() && "Index out of bounds!");
		kids[i] = std::move(e);
	}

	/* Returns whether this RE has kids */
	[[nodiscard]] auto hasKids() const -> bool { return !kids.empty(); }

	/* The kids of this RE */
	[[nodiscard]] auto getNumKids() const -> size_t { return kids.size(); }

	/* Returns a clone of the Constraint */
	[[nodiscard]] virtual auto clone() const -> std::unique_ptr<Constraint> = 0;

	/* Dumps the Constraint */
	virtual auto dump(std::ostream &s) const -> std::ostream & = 0;

protected:
	using KidsC = std::vector<std::unique_ptr<RegExp>>;

	[[nodiscard]] auto getKids() const -> const KidsC & { return kids; }

	void addKid(std::unique_ptr<RegExp> k) { kids.push_back(std::move(k)); }

	static auto createEmpty(std::unique_ptr<RegExp> e, bool sameEnds)
		-> std::unique_ptr<Constraint>;

	[[nodiscard]] auto isContainer() const -> bool override { return false; }

	void visitChildren(BaseVisitor &visitor) const override {}

private:
	KidsC kids;
};

inline auto operator<<(std::ostream &s, const Constraint &re) -> std::ostream &
{
	return re.dump(s);
}

/*******************************************************************************
 **                           Acyclicity Constraints
 ******************************************************************************/

class AcyclicConstraint : public Constraint {

protected:
	AcyclicConstraint(std::unique_ptr<RegExp> e, std::unique_ptr<Constraint> o,
			  std::string info)
		: constraint_(std::move(o)), printInfo_(std::move(info))
	{
		addKid(std::move(e));
	}

public:
	template <typename... Ts>
	static auto create(Ts &&...params) -> std::unique_ptr<AcyclicConstraint>
	{
		return std::unique_ptr<AcyclicConstraint>(
			new AcyclicConstraint(std::forward<Ts>(params)...));
	}

	/* NOTE: Might not return AcyclicConstraint */
	static auto createOpt(std::unique_ptr<RegExp> re, std::unique_ptr<Constraint> o,
			      std::string info) -> std::unique_ptr<Constraint>;

	[[nodiscard]] auto getConstraint() const -> const Constraint * { return constraint_.get(); }

	[[nodiscard]] auto getPrintInfo() const -> const std::string & { return printInfo_; }

	[[nodiscard]] auto clone() const -> std::unique_ptr<Constraint> override
	{
		return create(getKid(0)->clone(),
			      getConstraint() ? getConstraint()->clone() : nullptr, getPrintInfo());
	}

	auto dump(std::ostream &s) const -> std::ostream & override
	{
		return s << "acyclic" << *getKid(0);
	}

private:
	std::unique_ptr<Constraint> constraint_{};
	std::string printInfo_;
};

/*******************************************************************************
 **                           Error Constraint
 ******************************************************************************/

class ErrorConstraint : public Constraint {

protected:
	ErrorConstraint(std::unique_ptr<Constraint> cst, std::string name)
		: constraint_(std::move(cst)), name_(std::move(name))
	{
	}

public:
	template <typename... Ts>
	static auto create(Ts &&...params) -> std::unique_ptr<ErrorConstraint>
	{
		return std::unique_ptr<ErrorConstraint>(
			new ErrorConstraint(std::forward<Ts>(params)...));
	}

	/* NOTE: Might not return ErrorConstraint */
	static auto createOpt(std::unique_ptr<RegExp> e1, std::unique_ptr<RegExp> e2,
			      std::string name) -> std::unique_ptr<Constraint>;

	[[nodiscard]] auto getName() const -> const std::string & { return name_; }

	[[nodiscard]] auto getConstraint() const -> const Constraint * { return constraint_.get(); }

	[[nodiscard]] auto clone() const -> std::unique_ptr<Constraint> override
	{
		return create(getConstraint()->clone(), getName());
	}

	auto dump(std::ostream &s) const -> std::ostream & override
	{
		return s << "error(" << getName() << "): " << *getConstraint() << "\n";
	}

private:
	std::unique_ptr<Constraint> constraint_;
	std::string name_;
};

/*******************************************************************************
 **                           Warning Constraint
 ******************************************************************************/

class WarningConstraint : public Constraint {

protected:
	WarningConstraint(std::unique_ptr<Constraint> cst, std::string name)
		: constraint_(std::move(cst)), name_(std::move(name))
	{
	}

public:
	template <typename... Ts>
	static auto create(Ts &&...params) -> std::unique_ptr<WarningConstraint>
	{
		return std::unique_ptr<WarningConstraint>(
			new WarningConstraint(std::forward<Ts>(params)...));
	}

	/* NOTE: Might not return WarningConstraint */
	static auto createOpt(std::unique_ptr<RegExp> e1, std::unique_ptr<RegExp> e2,
			      std::string name) -> std::unique_ptr<Constraint>;

	[[nodiscard]] auto getName() const -> const std::string & { return name_; }

	[[nodiscard]] auto getConstraint() const -> const Constraint * { return constraint_.get(); }

	[[nodiscard]] auto clone() const -> std::unique_ptr<Constraint> override
	{
		return create(getConstraint()->clone(), getName());
	}

	auto dump(std::ostream &s) const -> std::ostream & override
	{
		return s << "warning(" << getName() << "): " << *getKid(0) << " <= " << *getKid(1)
			 << "\n";
	}

private:
	std::unique_ptr<Constraint> constraint_;
	std::string name_;
};

/*******************************************************************************
 **                           Recovery Constraints
 ******************************************************************************/

class RecoveryConstraint : public Constraint {

protected:
	RecoveryConstraint(std::unique_ptr<RegExp> e) { addKid(std::move(e)); }

public:
	template <typename... Ts>
	static auto create(Ts &&...params) -> std::unique_ptr<RecoveryConstraint>
	{
		return std::unique_ptr<RecoveryConstraint>(
			new RecoveryConstraint(std::forward<Ts>(params)...));
	}

	[[nodiscard]] auto clone() const -> std::unique_ptr<Constraint> override
	{
		return create(getKid(0)->clone());
	}

	auto dump(std::ostream &s) const -> std::ostream & override
	{
		return s << "recovery" << *getKid(0);
	}
};

/*******************************************************************************
 **                           Coherence Constraints
 ******************************************************************************/

class CoherenceConstraint : public Constraint {

protected:
	CoherenceConstraint(std::unique_ptr<RegExp> e) { addKid(std::move(e)); }

public:
	template <typename... Ts>
	static auto create(Ts &&...params) -> std::unique_ptr<CoherenceConstraint>
	{
		return std::unique_ptr<CoherenceConstraint>(
			new CoherenceConstraint(std::forward<Ts>(params)...));
	}

	/* NOTE: Might not return AcyclicConstraint */
	static auto createOpt(std::unique_ptr<RegExp> re) -> std::unique_ptr<Constraint>;

	[[nodiscard]] auto clone() const -> std::unique_ptr<Constraint> override
	{
		return create(getKid(0)->clone());
	}

	auto dump(std::ostream &s) const -> std::ostream & override
	{
		return s << "coherence " << *getKid(0);
	}
};

/*******************************************************************************
 **                           Subset Constraints
 ******************************************************************************/

class SubsetConstraint : public Constraint {

protected:
	SubsetConstraint(std::unique_ptr<RegExp> e1, std::unique_ptr<RegExp> e2, bool sameEnds_)
		: sameEnds_(sameEnds_)
	{
		addKid(std::move(e1));
		addKid(std::move(e2));
	}

public:
	template <typename... Ts>
	static auto create(Ts &&...params) -> std::unique_ptr<SubsetConstraint>
	{
		return std::unique_ptr<SubsetConstraint>(
			new SubsetConstraint(std::forward<Ts>(params)...));
	}

	/* NOTE: Might not return SubsetConstraint */
	static auto createOpt(std::unique_ptr<RegExp> e1, std::unique_ptr<RegExp> e2, bool sameEnds)
		-> std::unique_ptr<Constraint>;

	[[nodiscard]] auto getLHS() const -> const RegExp * { return getKid(0); }
	auto getLHS() -> RegExp * { return getKid(0).get(); }

	[[nodiscard]] auto getRHS() const -> const RegExp * { return getKid(1); }
	auto getRHS() -> RegExp * { return getKid(1).get(); }

	[[nodiscard]] auto sameEnds() const -> bool { return sameEnds_; }

	[[nodiscard]] auto clone() const -> std::unique_ptr<Constraint> override
	{
		return create(getKid(0)->clone(), getKid(1)->clone(), sameEnds_);
	}

	auto dump(std::ostream &s) const -> std::ostream & override
	{
		return s << *getKid(0) << " <= " << *getKid(1);
	}

private:
	bool sameEnds_{};
};

/*******************************************************************************
 **                           Equality Constraint
 ******************************************************************************/

/*
 * An EqualityConstraint is a SubsetConstraint that holds both ways
 */
class EqualityConstraint : public SubsetConstraint {

protected:
	EqualityConstraint(std::unique_ptr<RegExp> e1, std::unique_ptr<RegExp> e2, bool sameEndsLHS)
		: SubsetConstraint(std::move(e1), std::move(e2), sameEndsLHS)
	{
	}

public:
	template <typename... Ts>
	static auto create(Ts &&...params) -> std::unique_ptr<EqualityConstraint>
	{
		return std::unique_ptr<EqualityConstraint>(
			new EqualityConstraint(std::forward<Ts>(params)...));
	}

	static auto createOpt(std::unique_ptr<RegExp> e1, std::unique_ptr<RegExp> e2, bool sameEnds)
		-> std::unique_ptr<Constraint>;

	auto flip() -> EqualityConstraint &
	{
		std::swap(getKid(0), getKid(1));
		return *this;
	}

	[[nodiscard]] auto clone() const -> std::unique_ptr<Constraint> override
	{
		return create(getKid(0)->clone(), getKid(1)->clone(), sameEnds());
	}

	auto dump(std::ostream &s) const -> std::ostream & override
	{
		return s << *getKid(0) << " = " << *getKid(1) << "\n";
	}
};

/*******************************************************************************
 **                           Totality Constraint
 ******************************************************************************/

class TotalityConstraint : public Constraint {

protected:
	TotalityConstraint(std::unique_ptr<RegExp> e) { addKid(std::move(e)); }

public:
	template <typename... Ts>
	static auto create(Ts &&...params) -> std::unique_ptr<TotalityConstraint>
	{
		return std::unique_ptr<TotalityConstraint>(
			new TotalityConstraint(std::forward<Ts>(params)...));
	}

	static auto createOpt(std::unique_ptr<RegExp> e) -> std::unique_ptr<Constraint>
	{
		return create(std::move(e));
	}

	[[nodiscard]] auto getRelation() const -> const RegExp * { return getKid(0); }
	auto getRelation() -> RegExp * { return getKid(0).get(); }

	[[nodiscard]] auto clone() const -> std::unique_ptr<Constraint> override
	{
		return create(getKid(0)->clone());
	}

	auto dump(std::ostream &s) const -> std::ostream & override
	{
		return s << "total " << *getKid(0);
	}
};

#endif /* KATER_CONSTRAINT_HPP */
