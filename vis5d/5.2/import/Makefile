# Makefile for v5dimport

# Vis5D version 4.2

# Vis5D system for visualizing five dimensional gridded data sets
# Copyright (C) 1990 - 1996  Bill Hibbard, Brian Paul, Dave Santek,
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
	@echo "Enter one of:"
	@echo "  make dec-alpha"
	@echo "  make cray"
	@echo "  make dec"
	@echo "  make freebsd"
	@echo "  make hp"
	@echo "  make ibm"
	@echo "  make irix4"
	@echo "  make irix5"
	@echo "  make irix6.0.1"
	@echo "  make irix6"
	@echo "  make irix6-mips4"
	@echo "  make irix5-nomcidas"
	@echo "  make linux-ppc"
	@echo "  make linux"
	@echo "  make sunos4"
	@echo "  make sunos5"


# SGI debug:
debug:
	make targets \
	"CC = ${CC}" \
	"CFLAGS = -c -g -ansiposix -fullwarn -float -DIMPORT -DUNDERSCORE -DMCIDAS" \
	"F77 = ${F77}" \
	"FFLAGS = -c -O2" \
	"LINK = ${F77}" \
	"LFLAGS = -L../lui5/" \
	"LIBS = -lm -llui -lX11 -lfm -lGL -lGLU   ../src/libmcidas5.a" \
	"EXTRA_OBJS = kltwin.o"

#	"LIBS = -lm -llui -lX11 ../src/libmcidas5.a -lmalloc_cv" \

dec-alpha:
	make targets \
	"CC = ${CC}" \
	"CFLAGS = -c -O -std1 -DLITTLE -DUNDERSCORE -DIMPORT" \
	"F77 = ${F77}" \
	"FFLAGS = -c -O" \
	"LINK = ${F77} -nofor_main" \
	"LFLAGS = -L../lui5/" \
	"LIBS = -lm -llui -lX11  " \
	"EXTRA_OBJS = "

cray:
	make targets \
	"CC = ${CC} -DEPA -I/usr/local/include" \
	"CFLAGS = -c -g  -DIMPORT" \
	"F77 = cf77" \
	"FFLAGS = -c -g" \
	"LINK = ${CC}" \
	"LFLAGS = -L/usr/local/lib -L../lui5/" \
	"LIBS = -lnetcdf -lm -llui -lX11  " \
	"EXTRA_OBJS = alpha.o areal.o julian2std.o model.o mm4.o mm4date2std.o netcdf2julian.o radm.o radm_emis.o rom.o uam.o"

freebsd:
	make targets \
	"CC = gcc" \
	"CFLAGS += -c -O2 -pipe -DLITTLE -DUNDERSCORE -I/usr/X11R6/include" \
	"F77 = ${F77}" \
	"FFLAGS = -c -O" \
	"LINK = ${F77}" \
	"LFLAGS = -L./ -L../lui5/ -L/usr/local/lib" \
	"LIBS = -lf2c -lm -llui -lX11 -L/usr/X11R6/lib" \
	"EXTRA_OBJS = "

hp:
	make targets \
	"CC = ${CC}" \
	"CFLAGS = -c -O -Aa  -DIMPORT -D_HPUX_SOURCE -I/usr/include/X11R5" \
	"F77 = ${F77}" \
	"FFLAGS = -c -O" \
	"LINK = ${CC}" \
	"LFLAGS = -L/usr/lib/X11R5 -L../lui5" \
	"LIBS = -llui -lm -lX11 " \
	"EXTRA_OBJS = "

ibm:
	make targets \
	"CC = ${CC} -DIMPORT " \
	"CFLAGS = -c -O" \
	"F77 = xlf" \
	"FFLAGS = -c -O -w" \
	"LINK = xlf" \
	"LFLAGS = -L../lui5/" \
	"LIBS = -lm -llui -lX11 " \
	"EXTRA_OBJS = "

irix4:
	make targets \
	"CC = ${CC} -DIMPORT " \
	"CFLAGS = -c -O2 -ansi -DUNDERSCORE" \
	"F77 = ${F77}" \
	"FFLAGS = -c -O2" \
	"LINK = ${F77}" \
	"LFLAGS = -L../lui5/" \
	"LIBS = -lm -llui -lX11 " \
	"EXTRA_OBJS = "

irix5:
	make targets \
	"CC = ${CC}" \
	"CFLAGS = -c -O2 -ansiposix -fullwarn  -DIMPORT -DUNDERSCORE -DMCIDAS" \
	"F77 = ${F77}" \
	"FFLAGS = -c -O2" \
	"LINK = ${F77}" \
	"LFLAGS = -L../lui5/" \
	"LIBS = -lm -llui -lX11 ../src/libmcidas5.a " \
	"EXTRA_OBJS = kltwin.o"

irix5-opengl:
	make targets \
	"CC = ${CC}" \
	"CFLAGS = -c -O2 -ansiposix -fullwarn  -float -DIMPORT -DUNDERSCORE -DMCIDAS" \
	"F77 = ${F77}" \
	"FFLAGS = -c -O2" \
	"LINK = ${F77}" \
	"LFLAGS = -L../lui5/" \
	"LIBS = -lm -llui -lX11 -lfm -lGL -lGLU  ../src/libmcidas5.a" \
	"EXTRA_OBJS = kltwin.o"

irix5-nomcidas:
	make targets \
	"CC = ${CC}" \
	"CFLAGS = -c -O2 -ansiposix -DUNDERSCORE -DIMPORT " \
	"F77 = ${F77}" \
	"FFLAGS = -c -O2" \
	"LINK = ${CC}" \
	"LFLAGS = -L../lui5/" \
	"LIBS = -lm -llui -lX11  " \
	"EXTRA_OBJS = "

irix6.0.1:
	make targets \
	"CC = ${CC}" \
	"CFLAGS = -c -ansiposix -float -O2 -woff all -DUNDERSCORE -DIMPORT " \
	"F77 = ${F77}" \
	"FFLAGS = -c -O2" \
	"LINK = ${F77}" \
	"LFLAGS = -L../lui5/" \
	"LIBS = -lm -llui -lX11 -lfm -lGL -lGLU   " \
	"EXTRA_OBJS = "

irix6:
	make targets \
	"CC = ${CC}" \
	"CFLAGS = -c -32 -ansiposix -float -O2 -woff all -DUNDERSCORE -DOPENGL " \
	"F77 = ${F77}" \
	"FFLAGS = -c -32 -O2" \
	"LINK = ${F77}" \
	"LFLAGS = -32 -L../lui5/" \
	"LIBS = -lm -llui -lX11 -lfm -lGL -lGLU  ../src/libmcidas5.a" \
	"EXTRA_OBJS = "


irix6-mips4:
	make targets \
	"CC = ${CC}" \
	"CFLAGS = -c -n32 -mips4 -ansiposix -float -O2 -woff all -DUNDERSCORE -DIMPORT " \
	"F77 = ${F77}" \
	"FFLAGS = -c -n32 -mips4 -O2" \
	"LINK = ${F77}" \
	"LFLAGS = -n32 -L../lui5/" \
	"LIBS = -lm -llui -lX11 -lfm -lGL -lGLU " \
	"EXTRA_OBJS = "

linux-ppc:
	make targets \
	"CC = ${CC}" \
	"CFLAGS = -c -pipe -O2 -mcpu=604 -DIMPORT "\
	"F77 = echo" \
	"FFLAGS = " \
	"LINK = ${CC}" \
	"LFLAGS = -L../lui5/ -L/usr/X11/lib -L/usr/X11R6/lib" \
	"LIBS = -lmoto -lm  -llui -lX11  " \
	"EXTRA_OBJS = "

linux:
	make targets \
	"CC = ${CC}" \
	"CFLAGS = -c -pipe -O2 -DLITTLE -DIMPORT " \
	"F77 = echo" \
	"FFLAGS = " \
	"LINK = ${CC}" \
	"LFLAGS = -L../lui5/ -L/usr/X11/lib -L/usr/X11R6/lib" \
	"LIBS = -lm -llui -lX11  " \
	"EXTRA_OBJS = "

sunos4: sun_weirdness
	make targets \
	"CC = acc" \
	"CFLAGS = -c -O -DUNDERSCORE -DIMPORT " \
	"F77 = ${F77}" \
	"FFLAGS = -c -O" \
	"LINK = ${F77}" \
	"LFLAGS = -L./ -L../lui5/" \
	"LIBS = -lF77 -lm -llui -lX11  " \
	"EXTRA_OBJS = "

sunos5:
	make targets \
	"CC = ${CC}" \
	"CFLAGS = -c -O -Xa -DUNDERSCORE  -DIMPORT -I/usr/openwin/include" \
	"F77 = ${F77}" \
	"FFLAGS = -c -O" \
	"LINK = ${F77}" \
	"LFLAGS = -L./ -L../lui5/ -L/usr/openwin/lib -R/usr/openwin/lib -R/opt/SUNWspro/SC3.0.1/lib" \
	"LIBS = -lF77 -lm -llui -lX11  " \
	"EXTRA_OBJS = "

sunos5-gcc:
	make targets \
	"CC = gcc" \
	"CFLAGS = -c -O3 -DUNDERSCORE -I/usr/openwin/include" \
	"F77 = ${F77}" \
	"FFLAGS = -c -O" \
	"LINK = ${F77}" \
	"LFLAGS = -L./ -L../lui5/ -L/usr/openwin/lib -R/usr/openwin/lib -R/opt/SUNWspro/SC3.0.1/lib" \
	"LIBS = -lF77 -lm -llui -lX11" \
	"EXTRA_OBJS = "

sunos5-ultra:
	make targets \
	"CC = ${CC}" \
	"CFLAGS = -c -fast -O -xtarget=ultra -xarch=v8plusa  -DIMPORT -DUNDERSCORE -I/usr/openwin/include" \
	"F77 = ${F77}" \
	"FFLAGS = -c -O" \
	"LINK = ${F77}" \
	"LFLAGS = -L./ -L../lui5/ -L/usr/openwin/lib -R/usr/openwin/lib -R/opt/SUNWspro/SC3.0.1/lib" \
	"LIBS = -lF77 -lm -llui -lX11  " \
	"EXTRA_OBJS = "


### Sun: symbolic link to libF77.a needed
sun_weirdness: libF77.a

libF77.a: /usr/lang/SC1.0/libF77.a
	ln -s /usr/lang/SC1.0/libF77.a .


clean:
	-rm *.o *~


TAR_FILES = import/README import/Makefile import/*.[chf] \
	lui5/Makefile lui5/*.[ch] src/v5d.[ch] src/binio.[ch] \
	latlon.dat
tar:
	cd .. ; \
	tar -cvLf import.tar $(TAR_FILES) ; \
	compress import.tar ; \
	mv import.tar.Z import


######################################################################

# Location of v5d.[ch] and binio.[ch]:
#V5D = ../../vis5d-4.2/brian
V5D = ../src


# How to make a .o file from a .c file:
.c.o:
	$(CC) $(CFLAGS) -I$(V5D) -I../lui5 $<



OBJECTS = analyze.o binio.o file.o grid.o gui.o main.o misc.o \
	output.o proj.o projlist.o read_epa.o read_gr3d.o read_grid.o \
	read_grads.o read_uwvis.o read_v5d.o resample.o select.o \
	tokenize.o topo.o ui.o  v5d.o $(EXTRA_OBJS)   memory.o

targets: ../v5dimport


../v5dimport: $(OBJECTS) ../lui5/liblui.a
	$(LINK) $(LFLAGS) $(OBJECTS) $(LIBS) -o $@


../lui5/liblui.a:
	@echo "Recompile LUI"

analyze.o: analyze.c analyze.h grid.h misc.h proj.h $(V5D)/v5d.h

binio.o: $(V5D)/binio.c
	$(CC) $(CFLAGS) -I$(V5D) $(V5D)/binio.c

file.o: file.c $(V5D)/binio.h file.h read_epa.h read_gr3d.h read_grid.h \
	read_grads.h read_uwvis.h read_v5d.h

grads_misc.o: grads_misc.f
	$(F77) $(FFLAGS) grads_misc.f

grid.o: grid.c grid.h projlist.h

gui.o: gui.c ../lui5/lui.h analyze.h file.h grid.h misc.h output.h proj.h \
	projlist.h $(V5D)/v5d.h

kltwin.o: kltwin.f
	$(F77) $(FFLAGS) $(OPT) kltwin.f

main.o: main.c analyze.h file.h grid.h gui.h ui.h $(V5D)/v5d.h

output.o: output.c file.h grid.h projlist.h resample.h $(V5D)/v5d.h

proj.o: proj.c grid.h $(V5D)/v5d.h

projlist.o: projlist.c grid.h projlist.h $(V5D)/v5d.h

read_epa.o: read_epa.c grid.h model.h proj.h $(V5D)/v5d.h

read_gr3d.o: read_gr3d.c $(V5D)/binio.h grid.h proj.h $(V5D)/v5d.h

read_grid.o: read_grid.c $(V5D)/binio.h file.h grid.h proj.h $(V5D)/v5d.h

read_grads.o: read_grads.c $(V5D)/binio.h grid.h proj.h $(V5D)/v5d.h

read_uwvis.o: read_uwvis.c $(V5D)/v5d.h

read_v5d.o: read_v5d.c $(V5D)/v5d.h file.h grid.h proj.h projlist.h

resample.o: resample.c grid.h proj.h resample.h topo.h $(V5D)/v5d.h

select.o: select.c select.h grid.h

table.o: table.c grid.h

tokenize.o: tokenize.c

topo.o: topo.c

ui.o: ui.c grid.h tokenize.h $(V5D)/v5d.h

v5d.o: $(V5D)/v5d.c $(V5D)/v5d.h
	$(CC) $(CFLAGS) -I$(V5D) $(V5D)/v5d.c


#### EPA-provided code

alpha.o: alpha.c
	$(CC) $(CFLAGS) alpha.c

areal.o: areal.c
	$(CC) $(CFLAGS) areal.c

julian2std.o: julian2std.c
	$(CC) $(CFLAGS) julian2std.c

model.o: model.c model.h
	$(CC) $(CFLAGS) model.c

mm4.o: mm4.c
	$(CC) $(CFLAGS) mm4.c

mm4date2std.o: mm4date2std.c
	$(CC) $(CFLAGS) mm4date2std.c

netcdf2julian.o: netcdf2julian.c
	$(CC) $(CFLAGS) netcdf2julian.c

radm.o: radm.c
	$(CC) $(CFLAGS) radm.c

radm_emis.o: radm_emis.c
	$(CC) $(CFLAGS) radm_emis.c

rom.o: rom.c
	$(CC) $(CFLAGS) rom.c

uam.o: uam.c
	$(CC) $(CFLAGS) uam.c
