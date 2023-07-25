#
# Check for FLEX.
#
# This macro verifies that flex is installed.  If successful, then
# 1) $LEX is set to "flex" (to emulate lex calls)
# 2) BISON is set to bison
#
# If unsuccessful the NOBUILD_KATER macro is set and the
# ax_build_kater variable is set to no

AC_DEFUN([AC_PROG_FLEX],
[
  if test "x$ax_build_kater" = "x"; then
    ax_build_kater='yes'
  fi

  AC_PROG_LEX(noyywrap)
  if test "$LEX" != "flex"; then
    AC_MSG_WARN([flex not found. kater will not be built.])
    AC_DEFINE(NOBUILD_KATER, [], [whether to build kater])
    ax_build_kater='no'
  else
    AC_SUBST(FLEX,[flex],[location of flex])
  fi
])
