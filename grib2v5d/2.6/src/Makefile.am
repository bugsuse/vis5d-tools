
bin_PROGRAMS = grib2v5d
AM_FCFLAGS =

grib2v5d_SOURCES = grib2v5d.F90 maketopo.c sound.f

if NEEDVIS5D
  grib2v5d_SOURCES += binio.c v5d.c
endif
if NEEDMEGA
  grib2v5d_SOURCES += mega/eladata.f
endif
if NEEDGRIB_UTIL
  grib2v5d_SOURCES += grib_util/c2agrid.f grib_util/getdata.f \
   grib_util/findgribest.f grib_util/getinfoest.f grib_util/getinfovep.f \
   grib_util/findgrib.f grib_util/getinfo.f
  AM_FCFLAGS += -Igrib_util
endif

noinst_HEADERS = v5d.incf binio.h v5df.h v5d.h vis5d.h grib_util/getinfo.inc

mostlyclean-compile:
	-rm -f *.$(OBJEXT) *.mod
