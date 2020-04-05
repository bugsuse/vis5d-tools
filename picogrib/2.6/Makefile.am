
bin_PROGRAMS = dumpgrib partgrib showgrib gribex

lib_LTLIBRARIES = libpicogrib.la
libpicogrib_la_SOURCES = setpar.c griblib.c pbutils.c

dumpgrib_LDADD = libpicogrib.la
partgrib_LDADD = libpicogrib.la
showgrib_LDADD = libpicogrib.la
gribex_LDADD = libpicogrib.la

include_HEADERS = constant.h griblib.h struct.h

dist_pkgdoc_DATA = license.txt licenza.txt picogrib-en.txt picogrib-it.txt \
 README.txt

EXTRA_DIST = picogrib.spec
