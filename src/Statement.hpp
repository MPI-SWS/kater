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

#ifndef KATER_STATEMENT_HPP
#define KATER_STATEMENT_HPP

#include <memory>
#include <optional>
#include <utility>

#include "Constraint.hpp"
#include "DbgInfo.hpp"
#include "SavedExp.hpp"

/*
 * An abstract class representing a .kat statement.
 * Such a statement is an export, assume or assert statement,
 * and always has a constraint component.
 */
class Statement {

protected:
	Statement(std::unique_ptr<Constraint> constraint) : constraint_(std::move(constraint)) {}

public:
	/* Constructors/destructors */
	Statement() = delete;
	Statement(const Statement &) = delete;
	Statement(Statement &&) = default;
	auto operator=(Statement &&) -> Statement & = default;
	auto operator=(const Statement &) -> Statement & = delete;
	virtual ~Statement() = default;

	/** Returns the constraint carried by the statement */
	[[nodiscard]] auto getConstraint() const -> const Constraint * { return &*constraint_; }
	[[nodiscard]] auto getConstraint() -> Constraint * { return &*constraint_; }
	[[nodiscard]] auto getConstraintRef() const -> const std::unique_ptr<Constraint> &
	{
		return constraint_;
	}
	[[nodiscard]] auto getConstraintRef() -> std::unique_ptr<Constraint> &
	{
		return constraint_;
	}

	/** Clones the statement*/
	[[nodiscard]] virtual auto clone() const -> std::unique_ptr<Statement> = 0;

private:
	/** Helper for printing */
	virtual auto dump(std::ostream &s) const -> std::ostream & = 0;
	friend auto operator<<(std::ostream &s, const Statement &stmt) -> std::ostream &;

	std::unique_ptr<Constraint> constraint_{};
	std::string printInfo_;
};

inline auto operator<<(std::ostream &s, const Statement &stmt) -> std::ostream &
{
	return stmt.dump(s);
}

/*
 * A let statement is a statement representing a definition in the .kat file.
 * It stores the name and ID of the expression defined, the associated regexp,
 * as well as whether the part of the expression that should be saved during
 * code export (if there exists such part).
 */
class LetStatement : public Statement {

protected:
	LetStatement(std::string name, std::unique_ptr<RegExp> exp, std::unique_ptr<SavedExp> saved,
		     std::optional<DbgInfo> dbg)
		: Statement(nullptr), id_(dispenser++), name_(std::move(name)),
		  exp_(std::move(exp)), saved_(std::move(saved)), dbg_(std::move(dbg))
	{
	}

public:
	template <typename... Ts>
	static auto create(Ts &&...params) -> std::unique_ptr<LetStatement>
	{
		return std::unique_ptr<LetStatement>(new LetStatement(std::forward<Ts>(params)...));
	}

	/** Returns the unique identifier for this let statement */
	[[nodiscard]] auto getID() const -> unsigned { return id_; }

	/** Returns the name of the defined relation */
	[[nodiscard]] auto getName() const -> const std::string & { return name_; }

	void setName(std::string name) { name_ = std::move(name); }

	/** Returns the defined relation */
	[[nodiscard]] auto getRE() const -> const RegExp * { return &*exp_; }
	auto getRE() -> RegExp * { return &*exp_; }
	auto getRERef() -> std::unique_ptr<RegExp> & { return exp_; }
	[[nodiscard]] auto getRERef() const -> const std::unique_ptr<RegExp> & { return exp_; }

	void setRE(std::unique_ptr<RegExp> exp) { exp_ = std::move(exp); }

	/** Returns the part of the relation to be saved */
	[[nodiscard]] auto getSaved() const -> const SavedExp * { return &*saved_; }
	auto getSaved() -> SavedExp * { return &*saved_; }
	auto getSavedRef() -> std::unique_ptr<SavedExp> & { return saved_; }

	void setSaved(std::unique_ptr<SavedExp> saved) { saved_ = std::move(saved); }

	/** Returns the debug info associated with the let */
	[[nodiscard]] auto getDbgInfo() const -> const std::optional<DbgInfo> & { return dbg_; }

	[[nodiscard]] auto clone() const -> std::unique_ptr<Statement> override
	{
		return create(name_, exp_->clone(), saved_->clone(), dbg_);
	}

protected:
	[[nodiscard]] auto dump(std::ostream &s) const -> std::ostream & override
	{
		return s << "let " << getName() << " = " << *getRE() << "(saved as " << *getSaved()
			 << ")";
	}

private:
	unsigned id_{};
	std::string name_{};
	std::unique_ptr<RegExp> exp_{};
	std::unique_ptr<SavedExp> saved_{};
	std::optional<DbgInfo> dbg_{};

	/* A unique ID for each let statement */
	static inline unsigned
		dispenser{}; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)
};

/*
 * Denotes a let statement that occured as a part of a recursive definition
 */
class LetRecStatement : public LetStatement {

protected:
	LetRecStatement(std::string name, std::unique_ptr<RegExp> exp,
			std::unique_ptr<SavedExp> saved, std::optional<DbgInfo> dbg)
		: LetStatement(name, std::move(exp), std::move(saved), std::move(dbg))
	{
	}

public:
	template <typename... Ts>
	static auto create(Ts &&...params) -> std::unique_ptr<LetRecStatement>
	{
		return std::unique_ptr<LetRecStatement>(
			new LetRecStatement(std::forward<Ts>(params)...));
	}

	[[nodiscard]] auto clone() const -> std::unique_ptr<Statement> override
	{
		return create(getName(), getRE()->clone(), getSaved()->clone(), getDbgInfo());
	}
};

/*
 * An export statement is a statement carrying a constraint to be printed
 * during code generation.
 * This class is abstract; subclasses denote the type of the export
 */
class ExportStatement : public Statement {

protected:
	ExportStatement(std::unique_ptr<Constraint> constraint, std::unique_ptr<Constraint> unless,
			std::optional<std::string> codeToPrint,
			std::optional<std::string> nameOfExtra)
		: Statement(std::move(constraint)), id_(dispenser++), unless_(std::move(unless)),
		  codeToPrint_(std::move(codeToPrint)), nameOfExtra_(std::move(nameOfExtra))
	{
	}

public:
	template <typename... Ts>
	static auto create(Ts &&...params) -> std::unique_ptr<ExportStatement>
	{
		return std::unique_ptr<ExportStatement>(
			new ExportStatement(std::forward<Ts>(params)...));
	}

	/** Returns the unique identifier for this export */
	[[nodiscard]] auto getID() const -> unsigned { return id_; }

	/** Returns the "unless" clause accompanying the export */
	auto getUnless() -> Constraint * { return &*unless_; }
	[[nodiscard]] auto getUnless() const -> const Constraint * { return &*unless_; }

	/** Returns the code that should be printed before the generation of the export */
	auto getCodeToPrint() -> std::optional<std::string> & { return codeToPrint_; }
	[[nodiscard]] auto getCodeToPrint() const -> const std::optional<std::string> &
	{
		return codeToPrint_;
	}

	[[nodiscard]] auto hasCodeToPrint() const -> bool { return codeToPrint_.has_value(); }

	/** If this is an extra export, returns the name of it*/
	auto getNameOfExtra() -> std::optional<std::string> & { return nameOfExtra_; }
	[[nodiscard]] auto getNameOfExtra() const -> const std::optional<std::string> &
	{
		return nameOfExtra_;
	}

	/** Returns true if this is an extra export that should not be used in consistency checks */
	[[nodiscard]] auto isExtra() const -> bool { return getNameOfExtra().has_value(); }

	[[nodiscard]] auto clone() const -> std::unique_ptr<Statement> override
	{
		return create(getConstraint()->clone(),
			      getUnless() ? getUnless()->clone() : nullptr, getCodeToPrint(),
			      getNameOfExtra());
	}

protected:
	[[nodiscard]] auto dump(std::ostream &s) const -> std::ostream & override
	{
		s << "export " << *getConstraint();
		if (getUnless())
			s << " unless " << *getUnless();
		return s;
	}

private:
	unsigned id_{};
	std::unique_ptr<Constraint> unless_{};
	std::optional<std::string> codeToPrint_{};
	std::optional<std::string> nameOfExtra_{};

	/* A unique ID for each export statement */
	static inline unsigned
		dispenser{}; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)
};

/*
 * An assume statement carries a constraint that will be used as an assumption during proofs
 */
class AssumeStatement : public Statement {

protected:
	AssumeStatement(std::unique_ptr<Constraint> constraint, bool temporary)
		: Statement(std::move(constraint)), temporary_(temporary)
	{
	}

public:
	template <typename... Ts>
	static auto create(Ts &&...params) -> std::unique_ptr<AssumeStatement>
	{
		return std::unique_ptr<AssumeStatement>(
			new AssumeStatement(std::forward<Ts>(params)...));
	}

	/** Returns true if this is a temporary assumption */
	[[nodiscard]] auto isTemporary() const -> bool { return temporary_; }

	void setTemporary(bool status) { temporary_ = status; }

	[[nodiscard]] auto clone() const -> std::unique_ptr<Statement> override
	{
		return create(getConstraint()->clone(), isTemporary());
	}

private:
	[[nodiscard]] auto dump(std::ostream &s) const -> std::ostream & override
	{
		return s << "assume " << *getConstraint() << (isTemporary() ? "(tmp)" : "");
	}

	bool temporary_{};
};

/*
 * An assert statement carries a constraint that has to be proven
 */
class AssertStatement : public Statement {

protected:
	AssertStatement(std::unique_ptr<Constraint> constraint, DbgInfo dbg)
		: Statement(std::move(constraint)), dbg_(std::move(dbg))
	{
	}

public:
	template <typename... Ts>
	static auto create(Ts &&...params) -> std::unique_ptr<AssertStatement>
	{
		return std::unique_ptr<AssertStatement>(
			new AssertStatement(std::forward<Ts>(params)...));
	}

	/** Returns the debug info associated with the assert */
	[[nodiscard]] auto getDbgInfo() const -> const DbgInfo & { return dbg_; }

	[[nodiscard]] auto clone() const -> std::unique_ptr<Statement> override
	{
		return create(getConstraint()->clone(), getDbgInfo());
	}

private:
	[[nodiscard]] auto dump(std::ostream &s) const -> std::ostream & override
	{
		return s << "assert " << *getConstraint() << " (" << dbg_ << ")";
	}

	DbgInfo dbg_;
};

#endif /* KATER_STATEMENT_HPP */
