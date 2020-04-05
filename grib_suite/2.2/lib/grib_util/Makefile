#FC = f77
# On Sun systems the following should be uncommented if the library
# has to be linked by f90 programs

include ../../config/make.site

getinfoobj = getinfo.o getinfovep.o getinfoest.o

# Comment the following if you do not have f90
#f90objects = c2agrid.o
f90objects =
objects = findgrib.o  getdata.o findgribest.o c2agrid.o $(getinfoobj) $(f90objects)

$(libdir)/libgrib_util.a : $(objects)
	$(AR) $(libdir)/libgrib_util.a $(objects) 

$(getinfoobj): getinfo.inc


clean : 
	rm -f $(libdir)/libgrib_util.a $(objects)

allclean : 
	rm -f $(libdir)/grib_util.a $(objects)

dist :
	tar cvf $(libdir)/libgrib_util.tar  *.f *.c *.f90 Makefile

install : $(libdir)/libgrib_util.a
	install $(libdir)/libgrib_util.a $(DESTDIR)/lib



#.SUFFIXES: .o .f90
#
#.f90.o:
#	$F90 -c $(FFLAGS) $<

