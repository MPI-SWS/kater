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

#ifndef KATER_CODE_BUILDER_HPP
#define KATER_CODE_BUILDER_HPP

#include <format>
#include <ostream>
#include <string>
#include <string_view>
#include <utility>

/**
 * CodeBuilder -- a thin wrapper around an std::ostream that maintains an
 * indentation level and provides RAII scopes for braces.
 */
class CodeBuilder {
public:
	/**
	 * RAII handle returned by block-opening helpers. On destruction it
	 * decreases the level and emits the close-brace line.
	 */
	class Scope {
	public:
		Scope(CodeBuilder &b, std::string close) : b_(&b), close_(std::move(close))
		{
			++b_->level_;
		}

		Scope(const Scope &) = delete;
		auto operator=(const Scope &) -> Scope & = delete;
		Scope(Scope &&o) noexcept : b_(o.b_), close_(std::move(o.close_))
		{
			o.b_ = nullptr;
		}
		auto operator=(Scope &&) -> Scope & = delete;

		~Scope()
		{
			if (b_ == nullptr)
				return;

			--b_->level_;
			if (close_.empty())
				return; /* bump()-style scope: no closing token. */

			b_->emitIndent();
			b_->os_ << close_;
		}

	private:
		CodeBuilder *b_{};
		std::string close_;
	};

	explicit CodeBuilder(std::ostream &os, char indentChar = '\t', int initialLevel = 0)
		: os_(os), indentChar_(indentChar), level_(initialLevel)
	{
	}

	CodeBuilder(const CodeBuilder &) = delete;
	auto operator=(const CodeBuilder &) -> CodeBuilder & = delete;
	CodeBuilder(CodeBuilder &&) = delete;
	auto operator=(CodeBuilder &&) -> CodeBuilder & = delete;
	~CodeBuilder() = default;

	[[nodiscard]] auto level() const -> int { return level_; }

	/** Emit an indented line, terminated by '\n'. */
	template <class... Args>
	auto line(std::format_string<Args...> fmt, Args &&...args) -> CodeBuilder &
	{
		emitIndent();
		os_ << std::format(fmt, std::forward<Args>(args)...);
		os_ << '\n';
		return *this;
	}

	/** Emit a single blank '\n' (no indentation). */
	auto blank() -> CodeBuilder &
	{
		os_ << '\n';
		return *this;
	}

	/** Emit a string verbatim; caller is responsible for any newlines. */
	auto raw(std::string_view s) -> CodeBuilder &
	{
		os_ << s;
		return *this;
	}

	/** K&R "if (cond) {\n" ... "}", no newline to allow for else/elif statements after */
	template <class... Args>
	[[nodiscard]] auto ifStmt(std::format_string<Args...> fmt, Args &&...args) -> Scope
	{
		emitIndent();
		os_ << "if (" << std::format(fmt, std::forward<Args>(args)...) << ") {\n";
		return {*this, "}"};
	}

	/** K&R "} else if(cond) {\n" ... "}\n" after an if statement, no newline to allow for
	 * else/elif statements after.  */
	template <class... Args>
	[[nodiscard]] auto elifStmt(std::format_string<Args...> fmt, Args &&...args) -> Scope
	{
		os_ << " else if (" << std::format(fmt, std::forward<Args>(args)...) << ") {\n";
		return {*this, "}"};
	}

	/** K&R " else {\n" ... "}\n" after an elif/if statement.  */
	template <class... Args> [[nodiscard]] auto elseStmt() -> Scope
	{
		os_ << " else {\n";

		return {*this, "}\n"};
	}

	/** K&R "for (spec) {\n" ... "}\n". */
	template <class... Args>
	[[nodiscard]] auto forLoop(std::format_string<Args...> fmt, Args &&...args) -> Scope
	{
		emitIndent();
		os_ << "for (" << std::format(fmt, std::forward<Args>(args)...) << ") {\n";
		return {*this, "}\n"};
	}

	/** K&R "while (spec) {\n" ... "}\n". */
	template <class... Args>
	[[nodiscard]] auto whileLoop(std::format_string<Args...> fmt, Args &&...args) -> Scope
	{
		emitIndent();
		os_ << "while (" << std::format(fmt, std::forward<Args>(args)...) << ") {\n";
		return {*this, "}\n"};
	}

	/**
	 * Allman-style function body:
	 *     <signature>
	 *     {
	 *         <body>
	 *     }
	 *     <blank>
	 * The Scope dtor emits the closing brace and a trailing blank line so
	 * adjacent function definitions are separated automatically.
	 */
	template <class... Args>
	[[nodiscard]] auto function(std::format_string<Args...> fmt, Args &&...args) -> Scope
	{
		emitIndent();
		os_ << std::format(fmt, std::forward<Args>(args)...) << '\n';
		emitIndent();
		os_ << "{\n";
		return {*this, "}\n\n"};
	}

	/** K&R for generic lambdas: e.g. "<sig> {\n" ... "};\n\n" */
	template <class... Args>
	[[nodiscard]] auto lambda(std::format_string<Args...> fmt, Args &&...args) -> Scope
	{
		emitIndent();
		os_ << std::format(fmt, std::forward<Args>(args)...) << " {\n";
		return {*this, "};\n\n"};
	}

	/**
	 * Generalized K&R block: "<sig> {\n" ... "}\n". The opener is taken
	 * verbatim (no "if (" / "for (" wrapper) so callers can use this for
	 * any statement-style block (e.g. opening the body of an `if (cond)`
	 * predicate produced by another emitter). Use ifStmt/forLoop when the
	 * "if (" / "for (" wrapping is wanted.
	 */
	template <class... Args>
	[[nodiscard]] auto kr(std::format_string<Args...> fmt, Args &&...args) -> Scope
	{
		emitIndent();
		os_ << std::format(fmt, std::forward<Args>(args)...) << " {\n";
		return {*this, "}\n"};
	}

	/**
	 * No-emit indent scope: bumps level_ on entry, restores on exit.
	 * Use for dangling-if/-else/-for bodies (no braces) and any place we
	 * just need a one-line indented body without `{}`. Emits nothing on
	 * entry or exit --- only the level_ change is visible to subsequent
	 * line() calls.
	 */
	[[nodiscard]] auto bump() -> Scope { return {*this, ""}; }

	/**
	 * Emit a single line at level+1 --- i.e., the bumped equivalent of
	 * line(). Useful for one-statement dangling-if/-else/-for bodies
	 * that don't warrant a `{ auto _ = bump(); line(...); }` block.
	 */
	template <class... Args>
	auto indented(std::format_string<Args...> fmt, Args &&...args) -> CodeBuilder &
	{
		auto _ = bump();
		return line(fmt, std::forward<Args>(args)...);
	}

private:
	void emitIndent()
	{
		for (int i = 0; i < level_; ++i)
			os_ << indentChar_;
	}

	std::ostream &os_;
	char indentChar_{};
	int level_{};
};

#endif /* KATER_CODE_BUILDER_HPP */
