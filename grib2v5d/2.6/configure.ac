dnl Process this file with autoconf to produce a configure script.
AC_INIT(grib2v5d, 2.6, [dcesari@arpa.emr.it])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([configure.ac])
AM_INIT_AUTOMAKE([foreign])

AC_LANG([C])
AC_C_BIGENDIAN

AC_PROG_FC([gfortran pgf90 ifort g95 xlf90 f90], [Fortran 90])
F77=$FC
FFLAGS=$FCFLAGS
AC_PROG_F77([gfortran pgf77 ifort g95 xlf77 f77])
AC_FC_WRAPPERS()
AC_F77_WRAPPERS

AC_LANG_PUSH(Fortran)
dnl check if hibu library is available, otherwise disable sounding
dnl support, only for ARPA-SIMC
AC_CHECK_LIB([hibu], [ngetpoint], , AC_MSG_RESULT([sounding code not compiled]), )

dnl check if mega library is available, otherwise compile a simple source
HAVE_LIBMEGA="yes"
AC_CHECK_LIB([mega], [jeladata5], , 
		     unset HAVE_LIBMEGA
		     , )
if test "x$HAVE_LIBMEGA" == "xyes"; then
   AC_MSG_RESULT([Using mega library])
else
   AC_MSG_RESULT([Will compile mega library functions from source])
fi
AM_CONDITIONAL(NEEDMEGA, test "x$HAVE_LIBMEGA" != "xyes")

AC_SEARCH_LIBS([gribex], [picogrib emos], ,
			 AC_MSG_ERROR([gribex library not found]), )

dnl check if grib_util library is available, otherwise compile a simple source
HAVE_LIBGRIB_UTIL="yes"
AC_CHECK_LIB([grib_util], [getinfo], , 
			  unset HAVE_LIBGRIB_UTIL
		     , )
if test "x$HAVE_LIBGRIB_UTIL" == "xyes"; then
   AC_MSG_RESULT([Using grib_util library])
else
   AC_MSG_RESULT([Will compile grib_util library functions from source])
fi
AM_CONDITIONAL(NEEDGRIB_UTIL, test "x$HAVE_LIBGRIB_UTIL" != "xyes")

dnl AC_CHECK_LIB([grib_util], [getinfo], ,
dnl			  AC_MSG_ERROR([grib_util library not found]), )

HAVE_LIBV5D="yes"
AC_CHECK_LIB([v5d], [v5dcreate], ,
		    unset HAVE_LIBV5D
		    , )
if test "x$HAVE_LIBV5D" == "xyes"; then
   AC_MSG_RESULT([Using vis5d+ library])
else
   AC_MSG_RESULT([Will compile vis5d library functions from source])
fi
AM_CONDITIONAL(NEEDVIS5D, test "x$HAVE_LIBV5D" != "xyes")
AC_LANG_POP

pkgdocdir='${datadir}/doc/${PACKAGE}'
AC_SUBST(pkgdocdir)

AC_CONFIG_FILES([
Makefile
src/Makefile
doc/Makefile
examples/Makefile
])

AC_OUTPUT
