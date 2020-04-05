# Makefile for Vis5D utilities

# Vis5D version 5.0 

# Vis5D system for visualizing five dimensional gridded data sets
# Copyright (C) 1990-1997 Bill Hibbard, Brian Paul, Dave Santek,
# and Andre Battaiola.
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 1, or (at your option)
# any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


default:
	@echo "Type one of the following:"
	@echo "   make irix4"
	@echo "   make irix5"
	@echo "   make irix6.0.1"
	@echo "   make irix6"
	@echo "   make alpha"
	@echo "   make dec"
	@echo "   make freebsd"
	@echo "   make hp"
	@echo "   make ibm"
	@echo "   make linux-ppc"
	@echo "   make linux"
	@echo "   make stellar"
	@echo "   make sunos4"
	@echo "   make sunos5"
	@echo "   make clean"


# For debugging on SGI:
debug:
	make targets ../listfonts ../fromxwd \
	"CC = ${CC}" \
	"CFLAGS = -g -c -I../src -DUNDERSCORE" \
	"F77 = ${F77}" \
	"FFLAGS = -g -c -I../src" \
	"CLINK = ${CC}" \
	"FLINK = ${F77}" \
	"ARFLAGS = ruv" \
	"RANLIB = true"

irix4:
	make targets ../listfonts ../fromxwd\
	"CC = ${CC}" \
	"CFLAGS = -O -c -I../src -DUNDERSCORE" \
	"F77 = ${F77}" \
	"FFLAGS = -O -c -I../src" \
	"CLINK = ${CC}" \
	"FLINK = ${F77}" \
	"ARFLAGS = ruv" \
	"RANLIB = true"

irix5:
	make targets ../listfonts ../fromxwd\
	"CC = ${CC}" \
	"CFLAGS = -c -O -I../src -DUNDERSCORE" \
	"F77 = ${F77}" \
	"FFLAGS = -c -O -I../src" \
	"CLINK = ${CC}" \
	"FLINK = ${F77}" \
	"ARFLAGS = ruv" \
	"RANLIB = true"

irix6.0.1:
	make targets ../fromxwd \
	"CC = ${CC}" \
	"CFLAGS = -c -O -woff all -I../src -DUNDERSCORE" \
	"F77 = ${F77}" \
	"FFLAGS = -c -O -I../src" \
	"CLINK = ${CC}" \
	"FLINK = ${F77}" \
	"ARFLAGS = ruv" \
	"RANLIB = true"

irix6:
	make targets ../fromxwd \
	"CC = ${CC}" \
	"CFLAGS = -c -32 -O2 -woff all -I../src -DUNDERSCORE" \
	"F77 = ${F77}" \
	"FFLAGS = -c -32 -O2 -I../src" \
	"CLINK = ${CC} -32" \
	"FLINK = ${F77} -32" \
	"ARFLAGS = ruv" \
	"RANLIB = true"

irix6-mips4:
	make targets ../fromxwd \
	"CC = ${CC}" \
	"CFLAGS = -c -n32 -O2 -woff all -I../src -DUNDERSCORE" \
	"F77 = ${F77}" \
	"FFLAGS = -c -n32 -O2 -I../src" \
	"CLINK = ${CC} -n32" \
	"FLINK = ${F77} -n32" \
	"ARFLAGS = ruv" \
	"RANLIB = true"

alpha:
	make targets \
	"CC = ${CC}" \
	"CFLAGS = -O -c -I../src -traditional -DLITTLE -DUNDERSCORE" \
	"F77 = ${F77}" \
	"FFLAGS = -O -c -I../src" \
	"FLINK = ${F77} -nofor_main" \
	"CLINK = ${CC} " \
	"ARFLAGS = ruv" \
	"RANLIB = true"

cray:
	make ctargets \
	"CC = ${CC}" \
	"CFLAGS = -O -c -I../src -DUNDERSCORE" \
	"F77 = ${F77}" \
	"FFLAGS = -O -c -I../src" \
	"CLINK = ${CC}" \
	"FLINK = ${F77}" \
	"ARFLAGS = ruv" \
	"RANLIB = true"

dec:
	make targets \
	"CC = ${CC}" \
	"CFLAGS = -c -O -Ddec -I../src -DLITTLE -DUNDERSCORE" \
	"F77 = ${F77}" \
	"FFLAGS = -c -O" \
	"CLINK = ${CC}" \
	"FLINK = ${F77}" \
	"ARFLAGS = ruv" \
	"RANLIB = ranlib"

freebsd:
	make ctargets \
	"CC = gcc" \
	"CFLAGS += -pipe -O2 -c -DUNDERSCORE -DLITTLE -I../src" \
	"F77 = ${F77}" \
	"FFLAGS = -c -O " \
	"CLINK = gcc" \
	"FLINK = ${F77} -L./ -lf2c" \
	"ARFLAGS = ruv" \
	"RANLIB = ranlib"

hp:
	make targets \
	"CC = ${CC}" \
	"CFLAGS = -c -Aa -O -D_HPUX_SOURCE -Dhp -I../src" \
	"F77 = ${F77}" \
	"FFLAGS = -c -O" \
	"CLINK = ${CC}" \
	"FLINK = ${F77}" \
	"ARFLAGS = ruv" \
	"RANLIB = true"

ibm:
	make targets \
	"CC = ${CC}" \
	"CFLAGS = -c -O -w -Dibm -I../src" \
	"F77 = xlf" \
	"FFLAGS = -c -O -w" \
	"CLINK = ${CC}" \
	"FLINK = xlf" \
	"ARFLAGS = ruv" \
	"RANLIB = true"

linux-ppc:
	make ctargets \
	"CC = ${CC}" \
	"CFLAGS = -c -pipe -O2 -DLITTLE -DUNDERSCORE -I../src" \
	"F77 = ${F77}" \
	"FFLAGS = -c -O" \
	"CLINK = ${CC}" \
	"FLINK = ${F77}" \
	"ARFLAGS = ruv" \
	"RANLIB = ranlib"

linux:
	make ctargets \
	"CC = ${CC}" \
	"CFLAGS = -c -pipe -O2 -DLITTLE -DUNDERSCORE -I../src" \
	"F77 = ${F77}" \
	"FFLAGS = -c -O" \
	"CLINK = ${CC}" \
	"FLINK = ${F77}" \
	"ARFLAGS = ruv" \
	"RANLIB = ranlib"


stellar:
	make targets \
	"CC = ${CC}" \
	"CFLAGS = -O -c -I../src -DUNDERSCORE" \
	"F77 = ${F77}" \
	"FFLAGS = -O -c" \
	"CLINK = ${CC}" \
	"FLINK = ${F77}" \
	"ARFLAGS = ruv" \
	"RANLIB = true"

sunos4: sun_weirdness
	make targets \
	"CC = ${CC}" \
	"CFLAGS = -O -c -I../src -Dsun -DUNDERSCORE" \
	"F77 = ${F77}" \
	"FFLAGS = -c -O -Dsun" \
	"CLINK = ${CC} -L./ -lF77" \
	"FLINK = ${F77} -L./ -lF77" \
	"ARFLAGS = ruv" \
	"RANLIB = ranlib"

sunos5:
	make targets \
	"CC = ${CC}" \
	"CFLAGS = -O -c -Xa -I../src -Dsun -DUNDERSCORE" \
	"F77 = ${F77}" \
	"FFLAGS = -c -O -Dsun" \
	"CLINK = ${CC}" \
	"FLINK = ${F77} -L./ -lF77" \
	"ARFLAGS = ruv" \
	"RANLIB = true"

sunos5-gcc:
	make targets \
	"CC = gcc" \
	"CFLAGS = -c -O3 -I../src -Dsun -DUNDERSCORE" \
	"F77 = ${F77}" \
	"FFLAGS = -c -O -Dsun" \
	"CLINK = ${CC}" \
	"FLINK = ${F77} -L./ -lF77" \
	"ARFLAGS = ruv" \
	"RANLIB = true"

sunos5-ultra:
	make targets \
	"CC = ${CC}" \
	"CFLAGS = -c -fast -O -xtarget=ultra -xarch=v8plusa -I../src -Dsun -DUNDERSCORE" \
	"F77 = ${F77}" \
	"FFLAGS = -c -O -Dsun" \
	"CLINK = ${CC}" \
	"FLINK = ${F77} -L./ -lF77" \
	"ARFLAGS = ruv" \
	"RANLIB = true"


clean:
	-rm *.o *~ libmain.a

really_clean:
	-rm $(CPROGS) $(FPROGS) *.o *~


######################################################################

V5D_OBJS = ../src/v5d.o ../src/binio.o


CPROGS =  ../v5dinfo ../v5dstats ../comp_to_v5d ../v5dedit ../v5dappend \
	../topoinfo

FPROGS =  ../igg3d ../igu3d ../gg3d ../gr3d_to_v5d


targets: $(CPROGS) $(FPROGS)

ctargets: $(CPROGS)




### v5dinfo: info about a .v5d file

../v5dinfo: v5dinfo.o $(V5D_OBJS)
	$(CLINK) v5dinfo.o $(V5D_OBJS) -lm -o $@

v5dinfo.o: v5dinfo.c ../src/v5d.h
	$(CC) $(CFLAGS) v5dinfo.c



### v5dstats: stats about a .v5d file

../v5dstats: v5dstats.o $(V5D_OBJS)
	$(CLINK) v5dstats.o $(V5D_OBJS) -lm -o $@

v5dstats.o: v5dstats.c ../src/v5d.h
	$(CC) $(CFLAGS) v5dstats.c



### v5dedit: edit a .v5d file header

../v5dedit: v5dedit.o $(V5D_OBJS)
	$(CLINK) v5dedit.o $(V5D_OBJS) -lm -o $@

v5dedit.o: v5dedit.c ../src/v5d.h
	$(CC) $(CFLAGS) v5dedit.c



### v5dappend: append a number .v5d files onto a target file

../v5dappend: v5dappend.o $(V5D_OBJS)
	$(CLINK) v5dappend.o $(V5D_OBJS) -lm -o $@

v5dappend.o: v5dappend.c ../src/v5d.h
	$(CC) $(CFLAGS) v5dappend.c



### igg3d: McIDAS grid utility

../igg3d: igg3d.o libmain.a
	$(FLINK) igg3d.o libmain.a -o $@ $(LFLAGS)

igg3d.o: igg3d.f
	$(F77) $(FFLAGS) igg3d.f



### igu3d: McIDAS grid utility

../igu3d: igu3d.o libmain.a
	$(FLINK) igu3d.o libmain.a -o $@ $(LFLAGS)

igu3d.o: igu3d.f
	$(F77) $(FFLAGS) igu3d.f



### gg3d: McIDAS grid utility

../gg3d: gg3d.o libmain.a
	$(FLINK) gg3d.o libmain.a -o $@ $(LFLAGS)

gg3d.o: gg3d.f
	$(F77) $(FFLAGS) gg3d.f



### comp_to_v5d: convert an old Vis5D COMP* file to a .v5d file

../comp_to_v5d: comp_to_v5d.o $(V5D_OBJS)
	$(CLINK) comp_to_v5d.o $(V5D_OBJS) -lm -o $@

comp_to_v5d.o: comp_to_v5d.c ../src/v5d.h
	$(CC) $(CFLAGS) comp_to_v5d.c



### gr3d_to_v5d

../gr3d_to_v5d: gr3d_to_v5d.o get5d.o $(V5D_OBJS) libmain.a
	$(FLINK) gr3d_to_v5d.o get5d.o $(V5D_OBJS) libmain.a -lm -o $@

gr3d_to_v5d.o: gr3d_to_v5d.f
	$(F77) $(FFLAGS) gr3d_to_v5d.f

get5d.o: get5d.f
	$(F77) $(FFLAGS) get5d.f



### listfonts - for listing fonts available on the system (SGI only)

../listfonts: listfonts.o
	$(CLINK) listfonts.o -lfm_s -lgl_s -o $@

listfonts.o: listfonts.c
	$(CC) $(CFLAGS) listfonts.c



### fromxwd - to convert .xwd files to SGI .rgb format

../fromxwd: fromxwd.o
	$(CLINK) fromxwd.o -limage -o $@

fromxwd.o: fromxwd.c
	$(CC) $(CFLAGS) fromxwd.c


### topoinfo: info about a .topo file

../topoinfo: topoinfo.o $(V5D_OBJS)
	$(CLINK) topoinfo.o $(V5D_OBJS) -lm -o $@

topoinfo.o: topoinfo.c
	$(CC) $(CFLAGS) topoinfo.c



### Common files

../src/v5d.o: ../src/v5d.c ../src/v5d.h
	cd ../src ; \
	$(CC) $(CFLAGS) v5d.c

../src/binio.o: ../src/binio.c
	cd ../src ; \
	$(CC) $(CFLAGS) binio.c



### McIDAS library: libmain.a

LIB_OBJS = doubt.o igmk3d.o kludge.o main.o sdest.o topo.o

libmain.a: $(LIB_OBJS)
	ar $(ARFLAGS) libmain.a $(LIB_OBJS)
	$(RANLIB) libmain.a

doubt.o: doubt.f
	$(F77) $(FFLAGS) doubt.f

igmk3d.o: igmk3d.f
	$(F77) $(FFLAGS) igmk3d.f

kludge.o: kludge.f
	$(F77) $(FFLAGS) kludge.f

main.o: main.c
	$(CC) $(CFLAGS) main.c

sdest.o: sdest.f
	$(F77) $(FFLAGS) sdest.f

topo.o: topo.f
	$(F77) $(FFLAGS) topo.f


### Sun: symbolic link to libF77.a needed
sun_weirdness: libF77.a

libF77.a: /usr/lang/SC1.0/libF77.a
	ln -s /usr/lang/SC1.0/libF77.a .



##### TEST PROGRAMS #####

### v5dmake: make a sample .v5d file

v5dmake: v5dmake.o v5d.o binio.o
	$(CLINK) v5dmake.o v5d.o binio.o -o $@

v5dmake.o: v5dmake.c ../src/v5d.h
	$(CC) $(CFLAGS) v5dmake.c


### FORTRAN output

test: test.o binio.o v5d.o
	$(FLINK) test.o binio.o v5d.o -o $@

test.o: test.f
	$(F77) $(FFLAGS) test.f

