libdir = ${HOME}/yangl/tools/grib_util/2.2/lib
LDFLAGS = -L$(libdir) -lpicogrib -lm

OBJECTS = griblib.o pbutils.o setpar.o dummy.o
exo = dumpgrib.o partgrib.o showgrib.o
exx = ${exo:.o=}
exs = ${exo:.o=.c}

default:
	@echo "Type one of the following:"
	@echo "   make irix"
	@echo "   make alpha"
	@echo "   make cray"
	@echo "   make dec"
	@echo "   make freebsd"
	@echo "   make hp"
	@echo "   make ibm"
	@echo "   make linux"
	@echo "   make stellar"
	@echo "   make sunos"
	@echo "   make clean"


irix:
	make examples \
	"CC = cc" \
	"CFLAGS = -O -c -DUNDERSCORE" \
	"F77 = f77" \
	"F90 = f90" \
	"FFLAGS = -O -c -cpp -DCLARG" \
	"CLINK = cc" \
	"FLINK = f77" \
	"F90LINK = f90 " \
	"ARFLAGS = ruv" \
	"RANLIB = true"

alpha:
	make examples \
	"CC = cc" \
	"CFLAGS = -g -c -traditional -DLITTLE -DUNDERSCORE" \
	"F77 = f77" \
	"F90 = f90" \
	"FFLAGS = -g -c -cpp -DCLARG" \
	"FLINK = f77 -nofor_main" \
	"F90LINK = f90 " \
	"CLINK = cc " \
	"ARFLAGS = ruv" \
	"RANLIB = true"

cray:
	make examples \
	"CC = cc" \
	"CFLAGS = -O" \
	"F77 = cf77" \
	"F90 = f90" \
	"FFLAGS = -O -cpp" \
	"CLINK = cc" \
	"FLINK = cf77" \
	"F90LINK = f90 " \
	"ARFLAGS = ruv" \
	"RANLIB = true"

dec:
	make examples \
	"CC = cc" \
	"CFLAGS = -c -O -Ddec -DLITTLE -DUNDERSCORE" \
	"F77 = f77" \
	"F90 = f90" \
	"FFLAGS = -c -O -cpp -DCLARG" \
	"CLINK = cc" \
	"FLINK = f77" \
	"F90LINK = f90 " \
	"ARFLAGS = ruv" \
	"RANLIB = ranlib"

freebsd:
	make examples \
	"CC = gcc" \
	"CFLAGS += -pipe -O2 -c -DUNDERSCORE -DLITTLE" \
	"F77 = f77" \
	"F90 = f90" \
	"FFLAGS = -c -O -cpp -DCLARG" \
	"CLINK = gcc" \
	"FLINK = f77 -L./ -lf2c" \
	"F90LINK = f90 " \
	"ARFLAGS = ruv" \
	"RANLIB = ranlib"

hp:
	make examples \
	"CC = cc" \
	"CFLAGS = -c -Aa -O -D_HPUX_SOURCE -Dhp" \
	"F77 = f77" \
	"F90 = f90" \
	"FFLAGS = -c -O -cpp -DCLARG" \
	"CLINK = cc" \
	"FLINK = f77" \
	"F90LINK = f90 " \
	"ARFLAGS = ruv" \
	"RANLIB = true"

ibm:
	make examples \
	"CC = cc" \
	"CFLAGS = -c -O -w -Dibm" \
	"F77 = xlf" \
	"F90 = xlf90" \
	"FFLAGS = -c -O -w -cpp -DCLARG" \
	"CLINK = cc" \
	"FLINK = xlf" \
	"F90LINK = xlf90 " \
	"ARFLAGS = ruv" \
	"RANLIB = true"

linux:
	make examples \
	"CC = icc" \
	"CFLAGS = -c -O -DLITTLE -DUNDERSCORE" \
	"F77 = ifort" \
	"F90 = ifort" \
	"FFLAGS = -c -O -cpp -DCLARG" \
	"CLINK = icc" \
	"FLINK = ifort" \
	"F90LINK = ifort " \
	"ARFLAGS = ruv" \
	"RANLIB = ranlib"

stellar:
	make examples \
	"CC = cc" \
	"CFLAGS = -O -c -DUNDERSCORE" \
	"F77 = f77" \
	"F90 = f90" \
	"FFLAGS = -O -c -cpp -DCLARG" \
	"CLINK = cc" \
	"FLINK = f77" \
	"F90LINK = f90 " \
	"ARFLAGS = ruv" \
	"RANLIB = true"

sunos:
	make examples \
	"CC = cc" \
	"CFLAGS = -O -c -Xa -DLITTLE -Dsun -DUNDERSCORE" \
	"F77 = f77" \
	"F90 = f90" \
	"FFLAGS = -c -O -Dsun -cpp -DCLARG" \
	"CLINK = cc" \
	"FLINK = f77 -L./ -lF77" \
	"F90LINK = f90 " \
	"ARFLAGS = ruv" \
	"RANLIB = true"

examples: $(exx)

$(exx): $(exo) $(libdir)/libpicogrib.a
	$(CC) -o $@ $@.o $(LDFLAGS)

$(libdir)/libpicogrib.a : $(OBJECTS)
	ar -r $(libdir)/libpicogrib.a $(OBJECTS) 

clean :
	rm -f *~ $(OBJECTS) $(exo)

allclean : clean
	rm -f $(libdir)/libpicogrib.a dumpgrib partgrib showgrib

dist :
	tar cvf $(libdir)/picogrib.tar licenza.txt README.txt GRIBITA.* *.c *.h Makefile

