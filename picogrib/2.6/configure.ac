dnl Process this file with autoconf to produce a configure script.

AC_INIT(picogrib, 2.6, [dcesari@arpa.emr.it])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([configure.ac])
AM_INIT_AUTOMAKE([foreign])

AC_LANG([C])
AC_CHECK_FUNCS([getopt_long])
AC_C_BIGENDIAN
dnl AC_C_BIGENDIAN ([]
dnl 	       AC_DEFINE([LITTLE], [1], [Define as 1 if machine little endian])
dnl 	       )

AC_PROG_F77([pgf90 ifort gfortran g77 f77])
AC_F77_WRAPPERS

AC_CHECK_LIB([m],[cos])
AM_PROG_LIBTOOL

pkgdocdir='${datadir}/doc/${PACKAGE}'
AC_SUBST(pkgdocdir)

AC_CONFIG_FILES([
Makefile
])
AC_OUTPUT
