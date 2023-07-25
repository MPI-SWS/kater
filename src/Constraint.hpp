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
	enum class Type { Consistency, Error, Warning };

	using ValidFunT = std::function<bool(const TransLabel &)>;

protected:
	Constraint(std::vector<std::unique_ptr<RegExp> > &&kids = {})
		: kids(std::move(kids)) {}

public:
	virtual ~Constraint() = default;

	static auto createEmpty(std::unique_ptr<RegExp> e) -> std::unique_ptr<Constraint>;

	static auto
	createEquality(std::unique_ptr<RegExp> e1, std::unique_ptr<RegExp> e2) -> std::unique_ptr<Constraint>;

	[[nodiscard]] auto isEmpty() const -> bool;

	/* Returns the constraint type */
	[[nodiscard]] auto getType() const -> Type { return type; }

	/* Sets the constraint type to T */
	void setType(Type t) { type = t; }

	/* Fetches the i-th kid */
	[[nodiscard]] auto getKid(unsigned i) const -> const RegExp * {
		assert(i < kids.size() && "Index out of bounds!");
		return kids[i].get();
	}
	auto getKid(unsigned i) -> std::unique_ptr<RegExp> & {
		assert(i < kids.size() && "Index out of bounds!");
		return kids[i];
	}

	/* Sets the i-th kid to e */
	void setKid(unsigned i, std::unique_ptr<RegExp> e) {
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

	[[nodiscard]] auto getKids() const -> const KidsC & {
		return kids;
	}

	void addKid(std::unique_ptr<RegExp> k) {
		kids.push_back(std::move(k));
	}

	[[nodiscard]] auto isContainer() const -> bool override { return false; }

	void visitChildren(BaseVisitor &visitor) const override {}

private:
	Type type = Type::Consistency;
	KidsC kids;
};

inline auto operator<<(std::ostream &s, Constraint::Type t) -> std::ostream &
{
	switch (t) {
	case Constraint::Type::Consistency:
		return s << "consistency";
	case Constraint::Type::Error:
		return s << "error";
	case Constraint::Type::Warning:
		return s << "warning";
	default:
		return s << "UKNOWN";
	}
	return s;
}

inline auto operator<<(std::ostream &s, const Constraint& re) -> std::ostream &
{
	s << "[" <<re.getType() << "] ";
	return re.dump(s);
}

/*******************************************************************************
 **                           Conjunctive Constraints
 ******************************************************************************/

class ConjunctiveConstraint : public Constraint {

protected:
	ConjunctiveConstraint(std::unique_ptr<Constraint> c1,
			      std::unique_ptr<Constraint> c2)
		:  constraint1(std::move(c1)), constraint2(std::move(c2)) {}

public:
	template<typename... Ts>
	static auto create(Ts&&... params) -> std::unique_ptr<ConjunctiveConstraint> {
		return std::unique_ptr<ConjunctiveConstraint>(
			new ConjunctiveConstraint(std::forward<Ts>(params)...));
	}

	[[nodiscard]] auto clone() const -> std::unique_ptr<Constraint> override {
		return create(constraint1->clone(), constraint2->clone());
	}

	auto dump(std::ostream &s) const -> std::ostream & override {
		return s << "(" << *constraint1 << " && " << *constraint2 << ")";
	}

	[[nodiscard]] auto getConstraint1() const -> const Constraint * { return constraint1.get(); }
	[[nodiscard]] auto getConstraint2() const -> const Constraint * { return constraint2.get(); }

private:
	[[nodiscard]] auto isContainer() const -> bool override { return true; }

	void visitChildren(BaseVisitor &visitor) const override {
		visitor(*getConstraint1());
		visitor(*getConstraint2());
	}

	std::unique_ptr<Constraint> constraint1;
	std::unique_ptr<Constraint> constraint2;
};


/*******************************************************************************
 **                           Acyclicity Constraints
 ******************************************************************************/

class AcyclicConstraint : public Constraint {

protected:
	AcyclicConstraint(std::unique_ptr<RegExp> e)  { addKid(std::move(e)); }

public:
	template<typename... Ts>
	static auto create(Ts&&... params) -> std::unique_ptr<AcyclicConstraint> {
		return std::unique_ptr<AcyclicConstraint>(
			new AcyclicConstraint(std::forward<Ts>(params)...));
	}

	/* NOTE: Might not return AcyclicConstraint */
	static auto createOpt(std::unique_ptr<RegExp> re) -> std::unique_ptr<Constraint>;

	[[nodiscard]] auto clone() const -> std::unique_ptr<Constraint> override { return create(getKid(0)->clone()); }

	auto dump(std::ostream &s) const -> std::ostream & override { return s << "acyclic" << *getKid(0); }
};


/*******************************************************************************
 **                           Recovery Constraints
 ******************************************************************************/

class RecoveryConstraint : public Constraint {

protected:
	RecoveryConstraint(std::unique_ptr<RegExp> e)  { addKid(std::move(e)); }

public:
	template<typename... Ts>
	static auto create(Ts&&... params) -> std::unique_ptr<RecoveryConstraint> {
		return std::unique_ptr<RecoveryConstraint>(
			new RecoveryConstraint(std::forward<Ts>(params)...));
	}

	[[nodiscard]] auto clone() const -> std::unique_ptr<Constraint> override { return create(getKid(0)->clone()); }

	auto dump(std::ostream &s) const -> std::ostream & override { return s << "recovery" << *getKid(0); }
};


/*******************************************************************************
 **                           Coherence Constraints
 ******************************************************************************/

class CoherenceConstraint : public Constraint {

protected:
	CoherenceConstraint(std::unique_ptr<RegExp> e)  { addKid(std::move(e)); }

public:
	template<typename... Ts>
	static auto create(Ts&&... params) -> std::unique_ptr<CoherenceConstraint> {
		return std::unique_ptr<CoherenceConstraint>(
			new CoherenceConstraint(std::forward<Ts>(params)...));
	}

	/* NOTE: Might not return AcyclicConstraint */
	static auto createOpt(std::unique_ptr<RegExp> re) -> std::unique_ptr<Constraint>;

	[[nodiscard]] auto clone() const -> std::unique_ptr<Constraint> override { return create(getKid(0)->clone()); }

	auto dump(std::ostream &s) const -> std::ostream & override { return s << "coherence " << *getKid(0); }
};


/*******************************************************************************
 **                           Subset Constraints
 ******************************************************************************/

class SubsetConstraint : public Constraint {

protected:
	SubsetConstraint(std::unique_ptr<RegExp> e1, std::unique_ptr<RegExp> e2)
		 { addKid(std::move(e1)); addKid(std::move(e2)); }
public:
	template<typename... Ts>
	static auto create(Ts&&... params) -> std::unique_ptr<SubsetConstraint> {
		return std::unique_ptr<SubsetConstraint>(
			new SubsetConstraint(std::forward<Ts>(params)...));
	}

	/* NOTE: Might not return SubsetConstraint */
	static auto
	createOpt(std::unique_ptr<RegExp> e1, std::unique_ptr<RegExp> e2) -> std::unique_ptr<Constraint>;

	[[nodiscard]] auto getLHS() const -> const RegExp * { return getKid(0); }
	auto getLHS() -> RegExp * { return getKid(0).get(); }

	[[nodiscard]] auto getRHS() const -> const RegExp * { return getKid(1); }
	auto getRHS() -> RegExp * { return getKid(1).get(); }

	[[nodiscard]] auto clone() const -> std::unique_ptr<Constraint> override {
		return create(getKid(0)->clone(), getKid(1)->clone());
	}

	auto dump(std::ostream &s) const -> std::ostream & override {
		return s << *getKid(0) << " <= " << *getKid(1);
	}
};


/*******************************************************************************
 **                           SubsetSameEnds Constraint
 ******************************************************************************/

class SubsetSameEndsConstraint : public Constraint {

protected:
	SubsetSameEndsConstraint(std::unique_ptr<RegExp> e1, std::unique_ptr<RegExp> e2)
		 { addKid(std::move(e1)); addKid(std::move(e2)); }
public:
	template<typename... Ts>
	static auto create(Ts&&... params) -> std::unique_ptr<SubsetSameEndsConstraint> {
		return std::unique_ptr<SubsetSameEndsConstraint>(
			new SubsetSameEndsConstraint(std::forward<Ts>(params)...));
	}

	/* NOTE: Might not return SubsetSameEndsConstraint */
	static auto
	createOpt(std::unique_ptr<RegExp> e1, std::unique_ptr<RegExp> e2) -> std::unique_ptr<Constraint>;

	[[nodiscard]] auto getLHS() const -> const RegExp * { return getKid(0); }
	auto getLHS() -> RegExp * { return getKid(0).get(); }

	[[nodiscard]] auto getRHS() const -> const RegExp * { return getKid(1); }
	auto getRHS() -> RegExp * { return getKid(1).get(); }

	[[nodiscard]] auto clone() const -> std::unique_ptr<Constraint> override {
		return create(getKid(0)->clone(), getKid(1)->clone());
	}

	auto dump(std::ostream &s) const -> std::ostream & override {
		return s << *getKid(0) << " <=&id " << *getKid(1);
	}
};


/*******************************************************************************
 **                           Totality Constraint
 ******************************************************************************/

class TotalityConstraint : public Constraint {

protected:
	TotalityConstraint(std::unique_ptr<RegExp> e)
		 { addKid(std::move(e)); }
public:
	template<typename... Ts>
	static auto create(Ts&&... params) -> std::unique_ptr<TotalityConstraint> {
		return std::unique_ptr<TotalityConstraint>(
			new TotalityConstraint(std::forward<Ts>(params)...));
	}

	static auto
	createOpt(std::unique_ptr<RegExp> e) -> std::unique_ptr<Constraint> {
		return create(std::move(e));
	}

	[[nodiscard]] auto getRelation() const -> const RegExp * { return getKid(0); }
	auto getRelation() -> RegExp * { return getKid(0).get(); }

	[[nodiscard]] auto clone() const -> std::unique_ptr<Constraint> override {
		return create(getKid(0)->clone());
	}

	auto dump(std::ostream &s) const -> std::ostream & override {
		return s << "total " << *getKid(0);
	}
};


/*******************************************************************************
 **                           Transitivity Constraint
 ******************************************************************************/

class TransitivityConstraint : public Constraint {

protected:
	TransitivityConstraint(std::unique_ptr<RegExp> e)
		 { addKid(std::move(e)); }
public:
	template<typename... Ts>
	static auto create(Ts&&... params) -> std::unique_ptr<TransitivityConstraint> {
		return std::unique_ptr<TransitivityConstraint>(
			new TransitivityConstraint(std::forward<Ts>(params)...));
	}

	static auto
	createOpt(std::unique_ptr<RegExp> e) -> std::unique_ptr<Constraint> {
		return create(std::move(e));
	}

	[[nodiscard]] auto getRelation() const -> const RegExp * { return getKid(0); }
	auto getRelation() -> RegExp * { return getKid(0).get(); }

	[[nodiscard]] auto clone() const -> std::unique_ptr<Constraint> override {
		return create(getKid(0)->clone());
	}

	auto dump(std::ostream &s) const -> std::ostream & override {
		return s << "transitive " << *getKid(0);
	}
};

#endif /* KATER_CONSTRAINT_HPP */
