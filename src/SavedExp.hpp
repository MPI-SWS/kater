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

#ifndef KATER_SAVED_EXP_HPP
#define KATER_SAVED_EXP_HPP

#include "Constraint.hpp"
#include "RegExp.hpp"

/*
 * An abstract class modeling different forms of saved REs.
 * During export, REs are saved as sets, views, or not saved at all.
 */
class SavedExp {

protected:
	SavedExp() = default;

public:
	/* Constructors/destructors */
	SavedExp(const SavedExp &) = delete;
	SavedExp(SavedExp &&) = default;
	auto operator=(SavedExp &&) -> SavedExp & = default;
	auto operator=(const SavedExp &) -> SavedExp & = delete;
	virtual ~SavedExp() = default;

	/** Clones the saved expression */
	[[nodiscard]] virtual auto clone() const -> std::unique_ptr<SavedExp> = 0;

	friend auto operator<<(std::ostream &s, const SavedExp &sexp) -> std::ostream &;

private:
	/** Printing helper */
	virtual auto dump(std::ostream &s) const -> std::ostream & = 0;
};

inline auto operator<<(std::ostream &s, const SavedExp &sexp) -> std::ostream &
{
	return sexp.dump(s);
}

/*
 * Denotes that no expression is saved
 */
class NoSavedExp : public SavedExp {

protected:
	NoSavedExp() = default;

public:
	template <typename... Ts> static auto create(Ts &&...params) -> std::unique_ptr<NoSavedExp>
	{
		return std::unique_ptr<NoSavedExp>(new NoSavedExp(std::forward<Ts>(params)...));
	}

	[[nodiscard]] auto clone() const -> std::unique_ptr<SavedExp> override { return create(); }

private:
	auto dump(std::ostream &s) const -> std::ostream & override { return s << "nosaved"; }
};

/*
 * Denotes that an expression is saved in a view.
 */
class ViewExp : public SavedExp {

protected:
	ViewExp(std::unique_ptr<RegExp> exp) : exp_(std::move(exp)) {}

public:
	template <typename... Ts> static auto create(Ts &&...params) -> std::unique_ptr<ViewExp>
	{
		return std::unique_ptr<ViewExp>(new ViewExp(std::forward<Ts>(params)...));
	}

	/** Returns the expression to be stored in the view */
	[[nodiscard]] auto getRE() const -> const RegExp * { return &*exp_; }
	auto getRE() -> RegExp * { return &*exp_; }
	auto getRERef() -> std::unique_ptr<RegExp> & { return exp_; }
	[[nodiscard]] auto getRERef() const -> const std::unique_ptr<RegExp> & { return exp_; }

	void setRE(std::unique_ptr<RegExp> exp) { exp_ = std::move(exp); }

	[[nodiscard]] auto clone() const -> std::unique_ptr<SavedExp> override
	{
		return create(getRE()->clone());
	}

private:
	auto dump(std::ostream &s) const -> std::ostream & override
	{
		return s << "view " << *getRE();
	}

	std::unique_ptr<RegExp> exp_{};
};

/*
 * Denotes that an expression is saved in a set.
 */
class SetExp : public SavedExp {

protected:
	SetExp(std::unique_ptr<RegExp> exp) : exp_(std::move(exp)) {}

public:
	template <typename... Ts> static auto create(Ts &&...params) -> std::unique_ptr<SetExp>
	{
		return std::unique_ptr<SetExp>(new SetExp(std::forward<Ts>(params)...));
	}

	/** Returns the expression to be stored in set view */
	[[nodiscard]] auto getRE() const -> const RegExp * { return &*exp_; }
	auto getRE() -> RegExp * { return &*exp_; }
	auto getRERef() -> std::unique_ptr<RegExp> & { return exp_; }
	[[nodiscard]] auto getRERef() const -> const std::unique_ptr<RegExp> & { return exp_; }

	[[nodiscard]] auto clone() const -> std::unique_ptr<SavedExp> override
	{
		return create(getRE()->clone());
	}

private:
	auto dump(std::ostream &s) const -> std::ostream & override
	{
		return s << "set " << *getRE();
	}

	std::unique_ptr<RegExp> exp_{};
};

#endif /* KATER_SAVED_EXP_HPP */
