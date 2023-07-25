AC_INIT([kater], [0.1], [michalis@mpi-sws.org])

m4_include([m4/ax_flex.m4])
m4_include([m4/ax_bison.m4])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])

AM_SILENT_RULES([yes])

AC_PROG_CXX
AC_PROG_FLEX
AC_PROG_BISON

AM_CONDITIONAL([kater], [test "x$ax_build_kater" = "xyes"])

AC_PATH_PROG([CPPLINT], [clang-tidy], [true])
AS_IF([test "x$CPPLINT" = xtrue],
    [AC_MSG_WARN([clang-tidy not found. Lint checks disabled.])])

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile src/Makefile])
AC_OUTPUT
