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

#ifndef KATER_VISITOR_HPP
#define KATER_VISITOR_HPP

#include <type_traits>
#include <typeinfo>
#include <utility>

template <typename T>
auto get_most_derived(const T &obj) -> const void*;

/************************************************************
 *                 VisitableContainer class
 ************************************************************/

class BaseVisitor;

/*
 * Helper class that visitables should derive from if
 * they are containers (or we should recursively visit
 * their children).
 *
 * NOTE:
 *
 * 1. The base visitable should inherit from this, even
 * if only some child is a container. (BaseVisitor::operator()
 * switches on the hierarchy base.)
 *
 * 2. Currently does not support actions before/after visiting
 * the children, but adding an enum should be enough for this.
 */
class VisitableContainer {

protected:
    ~VisitableContainer() = default;

private:
    [[nodiscard]] virtual auto isContainer() const -> bool { return true; }

    virtual void visitChildren(BaseVisitor& visitor) const = 0;

    friend BaseVisitor;
};


/************************************************************
 *                 BaseVisitor class
 ************************************************************/

/*
 * Defines a visitor API --- switches on typeid
 */
class BaseVisitor {

public:
	template <typename T>
	void operator()(const T &obj) {
		if constexpr (std::is_base_of_v<VisitableContainer, T>) {
			if (static_cast<const VisitableContainer&>(obj).isContainer()) {
				static_cast<const VisitableContainer&>(obj).
					visitChildren(*this);
			} else {
				visit(get_most_derived(obj), typeid(obj));
			}
		} else {
			visit(get_most_derived(obj), typeid(obj));
		}
	}

protected:
	~BaseVisitor() = default;

private:
	virtual void visit(const void *ptr, const std::type_info &type) = 0;
};


/************************************************************
 *                 LambdaVisitor class
 ************************************************************/

/*
 * Combines multiple lambdas into one using C++17-folds.
 * Automatically constructs an if-then-else chain.
 */
template <typename Function, typename ... Types>
class LambdaVisitor : public BaseVisitor {

public:
	explicit LambdaVisitor(Function f) : f_(std::move(f)) {}

private:
	template <typename T>
	auto tryVisit(const void *ptr, const std::type_info& type) -> bool {
		if (type == typeid(T)) {
			f_(*static_cast<const T*>(ptr));
			return true;
		}
		return false;
	}

	void visit(const void *ptr, const std::type_info& type) override {
		(tryVisit<Types>(ptr, type) || ...);
	}

	Function f_;
};


/************************************************************
 *                 Utilities and helpers
 ************************************************************/

template <typename T>
auto get_most_derived(const T &obj) -> const void*
{
	if constexpr (!std::is_polymorphic_v<T> || std::is_final_v<T>) {
		return &obj;
	} else { /* else is necessary for constexpr */
		return dynamic_cast<const void *>(&obj);
}
}

template <typename... Functions>
auto overload(Functions... functions)
{
	struct lambda : Functions... {
		lambda(Functions... functions) : Functions(std::move(functions))... {}

		using Functions::operator()...;
	};

	return lambda(std::move(functions)...);
}

template <typename ... Types>
struct type_list {};

template <typename ... Types, typename ... Functions>
auto make_visitor(type_list<Types...> /*unused*/, Functions... funcs)
{
	auto overloaded = overload(std::move(funcs)...);
	return LambdaVisitor<decltype(overloaded), Types...>(std::move(overloaded));
}

#endif /* KATER_VISITOR_HPP */
