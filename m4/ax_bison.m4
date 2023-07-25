#
# Check for BISON
#
# This macro verifies that Bison is installed.  If successful, then
# 1) YACC is set to bison -y (to emulate YACC calls)
# 2) BISON is set to bison
#
# If unsuccessful the NOBUILD_KATER macro is set and the
# ax_build_kater variable is set to no

AC_DEFUN([AC_PROG_BISON],
[
  if test "x$ax_build_kater" = "x"; then
    ax_build_kater='yes'
  fi

  AC_PROG_YACC
  if test "$YACC" != "bison -y"; then
    AC_MSG_WARN([bison not found. kater will not be built.])
    AC_DEFINE(NOBUILD_KATER, [], [whether to build kater])
    ax_build_kater='no'
  else
    AC_SUBST(BISON, [bison], [location of bison])
  fi
])
