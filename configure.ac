# autoconf version 2.68 and automake version 1.11 seem to be the latest
# versions that can be used with Travis right now.
AC_PREREQ([2.68])
AC_INIT([ods],
	[0.1.0],
	[coder@kyleisom.net],
	[ods],
	[https://github.com/kisom/ods/])
AM_INIT_AUTOMAKE([1.11 foreign])

AC_CONFIG_SRCDIR([src/ch01ex01.cc])
AC_CONFIG_FILES([Makefile src/Makefile data/Makefile])
AC_CONFIG_MACRO_DIR([m4])

PKG_PROG_PKG_CONFIG
AC_CHECK_HEADERS

LT_INIT
AC_PROG_CXX
AC_PROG_INSTALL
AC_PROG_CC_C_O

AC_OUTPUT
