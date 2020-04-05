# makefile for vis5d

# $Id: Makefile,v 1.2 1997/05/02 17:56:30 johan Exp $

# Vis5D version 5.2 

# Vis5D system for visualizing five dimensional gridded data sets
# Copyright (C) 1990-1997  Bill Hibbard, Brian Paul, Dave Santek,
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

# WLH 29 Sept 98 - 'pipe' references added

VPATH = RCS

TCL_INCLUDE =
TCL_LIB =


### Tcl support:  uncomment and edit to enable
#TCL_INCLUDE = -DTCL -I/usr/local/include
#TCL_LIB = -L/usr/local/lib -ltcl

### The -I option specifies where to find tcl.h
### The -L option specifies where to find libtcl.a


default:
	@echo "To make vis5d type one of the following:"
	@echo "***Note*** In order to compile with Mcidas on an SGI add"
	@echo "           '-mcidas' after the make name. (Ex. make irix5-opengl-mcidas)"
	@echo "   make irix4"
	@echo "   make irix5"
	@echo "   make irix5-opengl"
	@echo "   make irix5-mesa"
	@echo "   make irix6-x"
	@echo "   make irix6.0.1"
	@echo "   make irix6"
	@echo "   make irix6-n32-mips3"
	@echo "   make irix6-64-mips4"
	@echo "   make irix6-64-mips4-mesa"
	@echo "   make alpha-denali"
	@echo "   make alpha-mesa"
	@echo "   make dec-opengl"
	@echo "   make dec-mesa"
	@echo "   make freebsd-mesa"
	@echo "   make hp-pex"
	@echo "   make hp10-pex"
	@echo "   make hp-opengl"
	@echo "   make hp-mesa"
	@echo "   make ibm-opengl"
	@echo "   make ibm-mesa"
	@echo "   make linux-opengl"
	@echo "   make linux-threads-mesa"
	@echo "   make linux-mesa"
	@echo "   make linux-ppc-mesa"
	@echo "   make sunos4-mesa"
	@echo "   make sunos5-mesa"
	@echo "   make sunos5-opengl"
	@echo "   make sunos5-ultra"
	@echo "   make sunos5i-mesa"
	@echo "   make clean"

# For debugging on SGI:
debug:
	pmake target  \
	"CC = ${CC}" \
	"CFLAGS = -c -g -fullwarn -DMCIDAS_SIDECAR -DLG -DSINGLE_TASK -DOPENGL -DUNDERSCORE -DMCIDAS" \
	"F77 = ${F77}" \
	"FFLAGS =  -c" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o kltwin.o xdump.o" \
	"LINK = ${F77}" \
	"LIBS = ../lui5/liblui2.a -lGL -lGLU -lX11 -lXext -lm ./libmcidas5.a"
	echo >lastmake "$@"

debug_i:
	pmake target  \
	"CC = ${CC}" \
	"CFLAGS =-c -g -fullwarn -DSINGLE_TASK -DOPENGL -DUNDERSCORE -DMCIDAS -I/usr/local/include" \
	"F77 = ${F77}"\
	"FFLAGS = -c O2" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o kltwin.o xdump.o" \
	"LINK = ${F77}" \
	"LFLAGS = -L../lui5/" \
	"LIBS = ../lui5/liblui2.a -lfm -lGL -lGLU -lX11 -lXext -lm ./libmcidas5.a -llui lmalloc_cv" \

	echo >lastmake "$@"

t:
	pmake target  \
	"CC = ${CC}" \
	"CFLAGS = -c -g -fullwarn -DLG -DSINGLE_TASK -DOPENGL -DUNDERSCORE -I/usr/local/include" \
	"F77 = ${F77}" \
	"FFLAGS = -c" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o xdump.o" \
	"LINK = ${F77}" \
	"TCL_INCLUDE = -DTCL -I/usr/local/include" \
	"TCL_LIB = -L/usr/local/lib -ltcl" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui2.a -lGL -lGLU -lX11 -lXext -lm "
	echo >lastmake "$@"

irix6.0.1:
	pmake target  \
	"CC = ${CC}" \
	"CFLAGS = -c -O3 -DOPENGL -DUNDERSCORE" \
	"F77 = ${F77}" \
	"FFLAGS = -c" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o xdump.o" \
	"LINK = ${F77}" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui2.a -lGL -lGLU -lX11 -lm"
	echo >lastmake "$@"

irix6-x-tcl:
	pmake target  \
	"CC = ${CC}" \
	"CFLAGS = -c -32 -DOPENGL -DUNDERSCORE -I../Mesa/include" \
	"F77 = ${F77}" \
	"FFLAGS = -c" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o xdump.o" \
	"LINK = ${F77}" \
	"TCL_INCLUDE = -DTCL -I/usr/local/include" \
	"TCL_LIB = -L/usr/local/lib -ltcl" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui2.a -L../Mesa/lib -lMesaGL -lMesaGLU -lX11 -lXext -lm "
	echo >lastmake "$@"

irix6-x:
	pmake target  \
	"CC = ${CC}" \
	"CFLAGS = -c -O2 -32 -DOPENGL -DUNDERSCORE -I../Mesa/include" \
	"F77 = ${F77}" \
	"FFLAGS = -c -32" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o xdump.o" \
	"LINK = ${F77} -32" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui2.a -L../Mesa/lib -lMesaGL -lMesaGLU -lX11 -lXext -lm"
	echo >lastmake "$@"

irix6:
	pmake target  \
	"CC = ${CC}" \
	"CFLAGS = -c -O2 -32 -DOPENGL -DUNDERSCORE" \
	"F77 = ${F77}" \
	"FFLAGS = -c -32" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o xdump.o" \
	"LINK = ${F77} -32" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui2.a -lGL -lGLU -lX11 -lm"
	echo >lastmake "$@"

irix6-nof77:
	pmake target  \
	"CC = ${CC}" \
	"CFLAGS = -c -O2 -32 -DOPENGL -DUNDERSCORE" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o xdump.o" \
	"LINK = ${F77} -32" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui2.a -lGL -lGLU -lX11 -lm"
	echo >lastmake "$@"

irix6-64-mips4-mesa:
	pmake target  \
	"CC = ${CC}" \
	"CFLAGS = -c -64 -O2 -mips4 -DOPENGL -DUNDERSCORE -I../Mesa/include" \
	"F77 = ${F77}" \
	"FFLAGS = -c -64 -mips4" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o xdump.o" \
	"LINK = ${F77} -64 -mips4" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui2.a -L../Mesa/lib -lMesaGL -lMesaGLU -lX11 -lXext -lm"
	echo >lastmake "$@"

irix6-64-mips4:
	pmake target  \
	"CC = ${CC}" \
	"CFLAGS = -c -64 -O2 -mips4 -DOPENGL -DUNDERSCORE " \
	"F77 = ${F77}" \
	"FFLAGS = -c -32 -mips4" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o xdump.o" \
	"LINK = ${F77} -64 -mips4" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui2.a -lGL -lGLU -lX11 -lm "
	echo >lastmake "$@"

irix6-n32-mips3:
	pmake target  \
	"CC = ${CC}" \
	"CFLAGS = -c -n32 -O2 -mips3 -DOPENGL -DUNDERSCORE " \
	"F77 = ${F77}" \
	"FFLAGS = -c -n32 -mips3" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o xdump.o" \
	"LINK = ${F77} -n32 -mips3" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui2.a -lGL -lGLU -lX11 -lm"
	echo >lastmake "$@"

irix5-opengl:
	pmake target \
	"CC = ${CC}" \
	"CFLAGS = -c -O2 -fullwarn -DOPENGL -DUNDERSCORE " \
	"F77 = ${F77}" \
	"FFLAGS = -c" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o xdump.o" \
	"LINK = ${F77}" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui2.a -lGL -lGLU -lX11 -lm -lmpc "
	echo >lastmake "$@"

irix5-mesa:
	pmake target \
	"CC = ${CC}" \
	"CFLAGS = -c -g -fullwarn -DOPENGL -DUNDERSCORE -I../Mesa/include" \
	"F77 = ${F77}" \
	"FFLAGS = -c" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o xdump.o" \
	"LINK = ${F77}" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui2.a -L../Mesa/lib -lMesaGL -lMesaGLU -lX11 -lXext -lm -lmpc -lfpe"
	echo >lastmake "$@"








irix4:
	pmake target  \
	"CC = ${CC}" \
	"CFLAGS = -c -O2 -ansi -DSGI_GL -DUNDERSCORE" \
	"F77 = ${F77}" \
	"FFLAGS = -c" \
	"AUXOBJS = gl_to_ppm.o graphics.gl.o" \
	"LINK = ${F77}" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui2.a -lfm -lgl_s -lm -lmpc"
	echo >lastmake "$@"

irix5:
	pmake target  \
	"CC = ${CC}" \
	"CFLAGS = -c -O2 -fullwarn -DLG -DSGI_GL -DUNDERSCORE -DMCIDAS -I/usr/local/include" \
	"F77 = ${F77}" \
	"FFLAGS = -c" \
	"AUXOBJS = gl_to_ppm.o graphics.gl.o kltwin.o" \
	"LINK = ${F77}" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui2.a -lfm -lgl_s -lm -lmpc ./libmcidas5.a"
	echo >lastmake "$@"

irix6.0.1-mcidas:
	pmake target  \
	"CC = ${CC}" \
	"CFLAGS = -c -O3 -DOPENGL -DMCIDAS -DUNDERSCORE" \
	"F77 = ${F77}" \
	"FFLAGS = -c" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o kltwin.o xdump.o" \
	"LINK = ${F77}" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui2.a -lGL -lGLU -lX11 -lm ./libmcidas5.a"
	echo >lastmake "$@"

irix6-x-tcl-mcidas:
	pmake target  \
	"CC = ${CC}" \
	"CFLAGS = -c -32 -O2 -DMCIDAS -DOPENGL -DUNDERSCORE -I../Mesa/include" \
	"F77 = ${F77}" \
	"FFLAGS = -c" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o xdump.o kltwin.o" \
	"LINK = ${F77}" \
	"TCL_INCLUDE = -DTCL -I/usr/local/include" \
	"TCL_LIB = -L/usr/local/lib -ltcl" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui2.a -L../Mesa/lib -lMesaGL -lMesaGLU -lX11 -lXext -lm ./libmcidas5.a "
	echo >lastmake "$@"

irix6-x-mcidas:
	pmake target  \
	"CC = ${CC}" \
	"CFLAGS = -c -32 -O2 -DOPENGL -DMCIDAS -DMCIDAS_SIDECAR -DUNDERSCORE -I../Mesa/include" \
	"F77 = ${F77}" \
	"FFLAGS = -c -32" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o kltwin.o xdump.o" \
	"LINK = ${F77} -32" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui2.a -L../Mesa/lib -lMesaGL -lMesaGLU -lX11 -lXext -lm ./libmcidas5.a"
	echo >lastmake "$@"

irix6-mcidas:
	pmake target  \
	"CC = ${CC}" \
	"CFLAGS = -c -32 -O2 -DOPENGL -DMCIDAS -DMCIDAS_SIDECAR -DUNDERSCORE" \
	"F77 = ${F77}" \
	"FFLAGS = -c -32" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o kltwin.o xdump.o" \
	"LINK = ${F77} -32" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui2.a -lGL -lGLU -lX11 -lm ./libmcidas5.a"
	echo >lastmake "$@"

irix6-64-mips4-mesa-mcidas:
	pmake target  \
	"CC = ${CC}" \
	"CFLAGS = -c -64 -O2 -mips4 -DOPENGL -DNEW_MCIDAS -DMCIDAS -DMCIDAS_SIDECAR -DUNDERSCORE -I../Mesa/include" \
	"F77 = ${F77}" \
	"FFLAGS = -c -64 -mips4" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o xdump.o" \
	"LINK = ${F77} -64 -mips4" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui2.a -L../Mesa/lib -lMesaGL -lMesaGLU -lX11 -lXext -lm ./libmcidas-64-mips4.a"
	echo >lastmake "$@"

irix6-64-mips4-mcidas:
	pmake target  \
	"CC = ${CC}" \
	"CFLAGS = -c -64 -O2 -mips4 -DOPENGL -DUNDERSCORE -DMCIDAS_SIDECAR -DNEW_MCIDAS -DMCIDAS" \
	"F77 = ${F77}" \
	"FFLAGS = -c -32 -mips4" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o xdump.o" \
	"LINK = ${F77} -64 -mips4" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui2.a -lGL -lGLU -lX11 -lm ./libmcidas-64-mips4.a"
	echo >lastmake "$@"

irix6-n32-mips3-mcidas:
	pmake target  \
	"CC = ${CC}" \
	"CFLAGS = -c -n32 -O2 -mips3 -DMCIDAS_SIDECAR -DNEW_MCIDAS -DOPENGL -DUNDERSCORE -DMCIDAS" \
	"F77 = ${F77}" \
	"FFLAGS = -c -n32 -mips3" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o xdump.o" \
	"LINK = ${F77} -n32 -mips3" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui2.a -lGL -lGLU -lX11 -lm ./libmcidas-n32-mips3.a"
	echo >lastmake "$@"

irix5-opengl-mcidas:
	pmake target2  \
	"CC = ${CC}" \
	"CFLAGS = -c -g -fullwarn -DLG -DOPENGL -DUNDERSCORE -DMCIDAS" \
	"F77 = ${F77}" \
	"FFLAGS = -c" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o kltwin.o xdump.o" \
	"LINK = ${F77}" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui2.a -lGL -lGLU -lX11 -lm -lmpc ./libmcidas5.a"
	pmake target \
	"CC = ${CC}" \
	"CFLAGS = -c -O2 -fullwarn -DOPENGL -DUNDERSCORE -DMCIDAS" \
	"F77 = ${F77}" \
	"FFLAGS = -c" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o kltwin.o xdump.o" \
	"LINK = ${F77}" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui2.a -lGL -lGLU -lX11 -lm -lmpc ./libmcidas5.a"
	echo >lastmake "$@"

irix5-mesa-mcidas:
	pmake target \
	"CC = ${CC}" \
	"CFLAGS = -c -g -fullwarn -DOPENGL -DUNDERSCORE -DMCIDAS -I../Mesa/include" \
	"F77 = ${F77}" \
	"FFLAGS = -c" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o kltwin.o xdump.o" \
	"LINK = ${F77}" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui2.a -L../Mesa/lib -lMesaGL -lMesaGLU -lX11 -lXext -lm -lmpc ./libmcidas5.a -lfpe"
	echo >lastmake "$@"

alpha-mesa:
	$(MAKE) target  \
	"CC = ${CC}" \
	"CFLAGS = -c -O -Ddec -w -DSINGLE_TASK -DOPENGL -DLITTLE -DUNDERSCORE -I../Mesa/include" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o xdump.o" \
	"LINK = ${F77}" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui2.a -L../Mesa/lib -lMesaGL -lMesaGLU -lX11 -lXext -lm"

#obsolete
alpha-denali:
	$(MAKE) target  \
	"CC = ${CC}" \
	"CFLAGS = -c -O -taso -traditional -DUNDERSCORE -DDENALI -DSINGLE_TASK -DLITTLE -I/usr/kits/NPGL" \
	"AUXOBJS = gl_to_ppm.o graphics.denali.o" \
	"LINK = ${F77} -non_shared" \
	"LIBS =  ../lui5/liblui2.a -lnpgl -lKPCglext -lvll -lX11 -lKPCext -lm -ldnet_stub"

dec-opengl:
	$(MAKE) target  \
	"CC = ${CC}" \
	"CFLAGS = -c -g -Ddec -w -DSINGLE_TASK -DOPENGL -DLITTLE -DUNDERSCORE -I/usr/include/GL" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o xdump.o" \
	"LINK = ${F77}" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui2.a -L/usr/shlib/ -lGL -lGLU -lX11 -lXext -lm"

dec-mesa:
	$(MAKE) target  \
	"CC = gcc" \
	"CFLAGS = -c -O -Ddec -w -DSINGLE_TASK -DOPENGL -DLITTLE -DUNDERSCORE -I../Mesa/include" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o xdump.o" \
	"LINK = gcc" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui2.a -L../Mesa/lib -lMesaGL -lMesaGLU -lX11 -lXext -lm"

freebsd-mesa:
	$(MAKE) target \
	"CC = ${CC}" \
	"CFLAGS += -c -O -DSINGLE_TASK -DOPENGL -DUNDERSCORE -DLITTLE -I../Mesa/include -I/usr/X11R6/include" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o xdump.o" \
	"LINK = ${F77}" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui.a -L../Mesa/lib -L/usr/X11R6/lib -lMesaGL -lMesaGLU -lm -lX11 -lXext"

hp-pex:
	$(MAKE) target  \
	"CC = ${CC}" \
	"CFLAGS = -c -O -Aa -D_HPUX_SOURCE -Dhp -I/usr/include/X11R5 -I/usr/lib/PEX5/utilities -DSINGLE_TASK -DPEX" \
	"AUXOBJS = graphics.pex.o xdump.o" \
	"LINK = ${F77}" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui2.a /usr/lib/PEX5/utilities/pexut.a -L/usr/lib/X11R5 -lXext -lPEX5 -lX11 -lm"

hp10-pex:
	$(MAKE) target  \
	"CC = ${CC}" \
	"CFLAGS = -Ae -c -Dhp -I/opt/graphics/PEX5/include/X11R6 \
	    -I/opt/graphics/PEX5/include/X11R5 \
	    -I/usr/include/X11R6 -I/usr/include/X11R5 \
	    -I/opt/graphics/PEX5/utilities -DSINGLE_TASK \
	    -DPEX" \
	"OPT = +O3" \
	"AUXOBJS = graphics.pex.o xdump.o" \
	"LINK = ${F77} +O3" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui2.a /var/opt/PEX5/pexut.a -L/usr/lib/X11R6 \
	    -L/usr/lib/X11R5 -L/opt/graphics/PEX5/lib -lXext -lPEX5 -lX11 -lm"

hp-opengl:
	$(MAKE) target \
	"CC = ${CC}" \
	"CFLAGS = -Ae -c -Dhp \
	-I/opt/graphics/OpenGL/include \
	-DSINGLE_TASK \
	-DOPENGL" \
	"OPT = +O3" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o xdump.o" \
	"LINK = ${F77} +O3" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui2.a \
	-L/opt/graphics/OpenGL/lib -lXext -lGLU -lGL -lX11 -lm"

hp-mesa:
	$(MAKE) target2	\
	"CC = ${CC}" \
	"CFLAGS = -c -Aa -w -g -D_HPUX_SOURCE -Dhp -I/usr/include/X11R5 -DSINGLE_TASK -DOPENGL -I../Mesa/include" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o xdump.o" \
	"LINK = ${F77}" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui2.a -L../Mesa/lib -lMesaGL -lMesaGLU -lm -L/usr/lib/X11R5 -lXext -lX11"
	$(MAKE) target  \
	"CC = ${CC}" \
	"CFLAGS = -c -O -Aa -w -D_HPUX_SOURCE -Dhp -I/usr/include/X11R5 -DSINGLE_TASK -DOPENGL -I../Mesa/include" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o xdump.o" \
	"LINK = ${F77}" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui2.a -L../Mesa/lib -lMesaGL -lMesaGLU -lm -L/usr/lib/X11R5 -lXext -lX11"

ibm-opengl:
	$(MAKE) target  \
	"CC = ${CC}" \
	"CFLAGS = -c -O -Dibm -DSINGLE_TASK -DOPENGL" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o xdump.o" \
	"LINK = ${F77}" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui2.a -lGL -lGLU -lX11 -lXext -lm"

ibm-mesa:
	$(MAKE) target  \
	"CC = ${CC}" \
	"CFLAGS = -c -O -Dibm -DSINGLE_TASK -DOPENGL -I../Mesa/include" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o xdump.o" \
	"LINK = ${F77}" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui2.a -L../Mesa/lib -lMesaGL -lMesaGLU -lX11 -lXext -lm"

linux-ppc-mesa:
	$(MAKE) target  \
	"CC = ${CC}"\
	"CFLAGS = -c -O2 -mcpu=604 -w -DUNDERSCORE -DOPENGL -DSINGLE_TASK -I../Mesa/include" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o xdump.o" \
	"LINK = ${CC} "\
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui2.a -L../Mesa/lib -lMesaGL -lMesaGLU -lm -L/usr/X11/lib -L/usr/X11R6/lib -lX11 -lXext"

linux-opengl:
	$(MAKE) target  \
	"CC = ${CC}" \
	"CFLAGS = -c -pipe -O2 -w -DUNDERSCORE -DOPENGL -DSINGLE_TASK -DLITTLE" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o xdump.o" \
	"LINK = ${CC} -pipe -O2" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf  ../lui5/liblui2.a -L/usr/X11R6/lib -lGL -lGLU -lm -lX11 -lXext"


linux-threads-mesa:
	$(MAKE) target \
	"CC = ${CC}" \
	"CFLAGS = -c -O6 -march=pentium -mpentium -w -DLITTLE -DLTHREADS -DUNDERSCORE -DOPENGL -DSINGLE_TASK -I../Mesa/include" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o xdump.o" \
	"LINK = ${CC}" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui.a -L../Mesa/lib -lMesaGL -lMesaGLU -lm -L/usr/X11R6/lib -lX11 -lXext -lpthread"

#
#
# May have to remove the -O2 on some linux machines
#
# May also need to at -ldl to CFLAGS on some linux machines
linux-mesa:
	$(MAKE) target  \
	"CC = ${CC}" \
	"CFLAGS = -c -O2 -w -DLITTLE -DUNDERSCORE -DOPENGL -DSINGLE_TASK -I../Mesa/include" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o xdump.o" \
	"LINK = ${CC}" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui2.a -L../Mesa/lib -lMesaGL -lMesaGLU -lm -L/usr/X11/lib -L/usr/X11R6/lib -lX11 -lXext"

sunos4-mesa:
	$(MAKE) target  \
	"CC = ${CC}" \
	"CFLAGS = -c -O -DSINGLE_TASK -DOPENGL -DUNDERSCORE -I../Mesa/include" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o xdump.o" \
	"LINK = ${F77}" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf ../lui5/liblui2.a -L../Mesa/lib -lMesaGL -lMesaGLU -lm -lX11 -lXext"

sunos5-mesa:
	$(MAKE) target  \
	"CC = ${CC}" \
	"CFLAGS = -O -Xa -c -I/usr/openwin/include -DSUN_OGL_NO_VERTEX_MACROS -I../Mesa/include -DSINGLE_TASK -DOPENGL -DUNDERSCORE" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o xdump.o" \
	"LINK = ${F77}" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf -L/usr/openwin/lib -R/usr/openwin/lib ../lui5/liblui2.a -L../Mesa/lib -lMesaGL -lMesaGLU -lm -lX11 -lXext -lsocket"

sunos5-opengl:
	$(MAKE) target  \
	"CC = ${CC}" \
	"CFLAGS = -O -Xa -c -I/usr/openwin/include -DNO_BORDERS -DSINGLE_TASK -DOPENGL -DUNDERSCORE" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o xdump.o" \
	"LINK = ${F77}" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf -L/usr/openwin/lib -R/usr/openwin/lib ../lui5/liblui2.a -lGL -lGLU -lm -lX11 -lXext -lsocket"

sunos5-opengl-gcc:
	$(MAKE) target \
	"CFLAGS = -c -O3 -I/usr/openwin/include -DNO_BORDERS -DSINGLE_TASK -DOPENGL -DUNDERSCORE" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o xdump.o" \
	"LINK = ${CC}"
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf -L/usr/openwin/lib -R/usr/openwin/lib ../lui5/liblui.a -lGL -lGLU -lX11 -lXext -lXmu -lXi -lsocket"

sunos5-opengl-ultra:
	$(MAKE) target  \
	"CC = ${CC}" \
	"CFLAGS = -g -xtarget=ultra -xarch=v8plusa -c -I/usr/openwin/include -DNO_BORDERS -DSINGLE_TASK -DOPENGL -DUNDERSCORE" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o xdump.o" \
	"LINK = ${F77}" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf -L/usr/openwin/lib -R/usr/openwin/lib ../lui5/liblui2.a -lGL -lGLU -lm -lX11 -lXext -lsocket"

sunos5i-mesa:
	$(MAKE) target  \
	"CC = ${CC}" \
	"CFLAGS = -O -Xa -c -I/usr/openwin/include -DSUN_OGL_NO_VERTEX_MACROS -I../Mesa/include -DSINGLE_TASK -DOPENGL -DLITTLE -DUNDERSCORE" \
	"AUXOBJS = gl_to_ppm.o graphics.ogl.o xdump.o" \
	"LINK = ${CC}" \
	"LIBS = ${NETCDF}/lib/libnetcdf.a -lz -ldl -lhdf5 -lhdf5_hl -lnetcdf -L/usr/openwin/lib -R/usr/openwin/lib ../lui5/liblui2.a -L../Mesa/lib -lMesaGL -lMesaGLU -lm -lX11 -lXext -lsocket"


clean:
	-rm *.o *~


######################################################################

target2: dofirst

target: ../vis5d

API_OBJS = api.o analysis.o anim.o box.o chrono.o compute.o contour.o \
	groupchrono.o globals.o graphics.all.o grid.o image.o imemory.o map.o matrix.o \
	linterp.o memory.o misc.o mwmborder.o pipe.o proj.o queue.o render.o rgb.o \
	record.o save.o socketio.o stream.o sounding.o sync.o tclsave.o textplot.o topo.o \
	traj.o user_data.o volume.o vtmcP.o work.o

OBJECTS_I = analyze_i.o binio.o file_i.o grid_i.o   \
	main_i.o misc_i.o output_i.o proj_i.o projlist_i.o read_epa_i.o \
	read_gr3d_i.o read_grid_i.o read_grads_i.o read_uwvis_i.o read_v5d_i.o \
	resample_i.o select_i.o tokenize_i.o topo_i.o ui_i.o v5d.o memory_i.o \
	imain.o igui.o iapi.o file.o irregular_v5d.o


GUI_OBJS = cursor.o displaywidget.o gui.o gui_i.o labels.o script.o slice.o \
	soundingGUI.o rgbsliders.o isocolor.o uvwwidget.o

OTHER_API =  analysis.o anim.o api.o box.o chrono.o compute.o contour.o \
	groupchrono.o globals.o graphics.all.o grid.o image.o imemory.o map.o matrix.o \
	memory.o misc.o mwmborder.o pipe.o proj.o queue.o record.o render.o rgb.o \
	save.o socketio.o stream.o sounding.o sync.o tclsave.o textplot.o topo.o \
	traj.o user_data.o volume.o vtmcP.o work.o

OBJS = $(API_OBJS) $(GUI_OBJS) $(AUXOBJS) $(OBJECTS_I)


dofirst: api.o uvwwidget.o

../vis5d: $(OBJS) main.o ../lui5/liblui2.a
	$(LINK) main.o $(OBJS) $(TCL_LIB) $(LIBS) -o $@



.c.o:
	$(CC) $(CFLAGS) $(TCL_INCLUDE) $<


SOURCES = analysis.c anim.c api.c binio.c box.c chrono.c compute.c  \
	groupchrono.c contour.c cursor.c globals.c graphics.all.c graphics.gl.c \
	gl_to_ppm.c graphics.ogl.c graphics.pex.c grid.c gui.c image.c imemory.c isocolor.c \
	labels.c linterp.c main.c map.c matrix.c memory.c misc.c mwmborder.c \
	pipe.c proj.c queue.c record.o render.c rgb.c rgbsliders.c save.c script.c \
	slice.c socketio.c sounding.c soundingGUI.c stream.c sync.c \
	tclsave.c textplot.c topo.c traj.c user_data.c v5d.c volume.c vtmcP.c work.c xdump.c \
        irregular_v5d.c


##dep:
##	makedepend -fdepend -Y $(SOURCES)


# The following are dependencies generated by running makedepend:

analysis.o: analysis.h globals.h api.h matrix.h sync.h v5d.h vis5d.h grid.h
analysis.o: memory.h proj.h queue.h socketio.h
anim.o: globals.h api.h matrix.h sync.h v5d.h vis5d.h graphics.h
api.o: analysis.h globals.h api.h matrix.h sync.h v5d.h vis5d.h anim.h box.h
api.o: chrono.h compute.h graphics.h grid.h image.h memory.h map.h misc.h proj.h
api.o: groupchrono.h queue.h render.h save.h traj.h topo.h volume.h work.h
api.o: ../lui5/browser.h
binio.o: binio.h
box.o: api.h box.h globals.h matrix.h sync.h v5d.h vis5d.h graphics.h proj.h
box.o: render.h
chrono.o: api.h globals.h chrono.h vis5d.h v5d.h
compute.o: api.h globals.h matrix.h sync.h v5d.h vis5d.h grid.h memory.h
contour.o: memory.h globals.h api.h matrix.h sync.h v5d.h vis5d.h
cursor.o: api.h gui.h ../lui5/lui.h ../lui5/browser.h ../lui5/list.h
cursor.o: ../lui5/scrollbar.h ../lui5/field.h ../lui5/newbutton.h
cursor.o: ../lui5/button.h ../lui5/buttonmatrix.h ../lui5/colorbar.h
cursor.o: ../lui5/destroy.h ../lui5/dialog.h ../lui5/label.h ../lui5/layout.h
cursor.o: ../lui5/newlabel.h ../lui5/newslider.h ../lui5/popup.h
cursor.o: ../lui5/radio.h matrix.h vis5d.h v5d.h
displaywidget.o: displaywidget.c ../lui5/lui.h analyze_i.h file_i.h grid_i.h misc_i.h output_i.h proj_i.h
displaywidget.o: projlist_i.h v5d.h api.h gui.h
globals.o: globals.h api.h matrix.h sync.h v5d.h vis5d.h memory.h graphics.h
gl_to_ppm.o: api.h graphics.h v5d.h
graphics.all.o: globals.h api.h matrix.h sync.h v5d.h vis5d.h
graphics.gl.o: graphics.h globals.h api.h matrix.h sync.h v5d.h vis5d.h
graphics.gl.o: misc.h
graphics.ogl.o: globals.h api.h matrix.h sync.h v5d.h vis5d.h graphics.h
graphics.ogl.o: misc.h mwmborder.h xdump.h
graphics.pex.o: graphics.h globals.h api.h matrix.h sync.h v5d.h vis5d.h
groupchrono.o: api.h globals.h groupchrono.h vis5d.h v5d.h
grid.o: api.h binio.h grid.h globals.h matrix.h sync.h v5d.h vis5d.h memory.h
gui.o: ../lui5/lui.h ../lui5/browser.h ../lui5/list.h ../lui5/scrollbar.h
gui.o: ../lui5/field.h ../lui5/newbutton.h ../lui5/button.h
gui.o: ../lui5/buttonmatrix.h ../lui5/colorbar.h ../lui5/destroy.h
gui.o: ../lui5/dialog.h ../lui5/label.h ../lui5/layout.h ../lui5/newlabel.h
gui.o: ../lui5/newslider.h ../lui5/popup.h ../lui5/radio.h
gui.o: analysis.h globals.h api.h matrix.h main_i.h sync.h v5d.h vis5d.h cursor.h
gui.o: graphics.h gui.h labels.h slice.h script.h soundingGUI.h tclsave.h rgbsliders.h
gui.o: displaywidget.h isocolor.h uvwwidget.h gui_i.h
image.o: globals.h api.h matrix.h sync.h v5d.h vis5d.h graphics.h rgb.h
imemory.o: imemory.h v5d.h irregular_v5d.h iapi.h api.h
irregular_v5d.o: irregular_v5d.h
labels.o: api.h labels.h gui.h ../lui5/lui.h ../lui5/browser.h ../lui5/list.h
labels.o: ../lui5/scrollbar.h ../lui5/field.h ../lui5/newbutton.h
labels.o: ../lui5/button.h ../lui5/buttonmatrix.h ../lui5/colorbar.h
labels.o: ../lui5/destroy.h ../lui5/dialog.h ../lui5/label.h ../lui5/layout.h
labels.o: ../lui5/newlabel.h ../lui5/newslider.h ../lui5/popup.h
labels.o: ../lui5/radio.h matrix.h vis5d.h v5d.h
main.o: api.h gui.h ../lui5/lui.h ../lui5/browser.h ../lui5/list.h
main.o: ../lui5/scrollbar.h ../lui5/field.h ../lui5/newbutton.h
main.o: ../lui5/button.h ../lui5/buttonmatrix.h ../lui5/colorbar.h
main.o: ../lui5/destroy.h ../lui5/dialog.h ../lui5/label.h ../lui5/layout.h
main.o: ../lui5/newlabel.h ../lui5/newslider.h ../lui5/popup.h
main.o: ../lui5/radio.h matrix.h pipe.h vis5d.h v5d.h script.h
map.o: binio.h graphics.h globals.h api.h matrix.h sync.h v5d.h vis5d.h map.h
map.o: proj.h topo.h user_data.h v5d.h
matrix.o: matrix.h
memory.o: globals.h api.h matrix.h sync.h v5d.h vis5d.h memory.h misc.h
misc.o: api.h globals.h matrix.h sync.h v5d.h vis5d.h memory.h misc.h proj.h
proj.o: globals.h api.h matrix.h sync.h v5d.h vis5d.h proj.h box.h
queue.o: analysis.h globals.h api.h matrix.h sync.h v5d.h vis5d.h queue.h
record.o: record.h v5d.h api.h
render.o: anim.h globals.h api.h matrix.h sync.h v5d.h vis5d.h box.h
render.o: graphics.h grid.h labels.h map.h memory.h misc.h proj.h queue.h
render.o: topo.h volume.h
rgb.o: rgb.h v5d.h
save.o: api.h globals.h matrix.h sync.h v5d.h vis5d.h grid.h memory.h misc.h
save.o: topo.h labels.h
script.o: api.h globals.h matrix.h sync.h v5d.h vis5d.h graphics.h gui.h
script.o: ../lui5/lui.h ../lui5/browser.h ../lui5/list.h ../lui5/scrollbar.h
script.o: ../lui5/field.h ../lui5/newbutton.h ../lui5/button.h
script.o: ../lui5/buttonmatrix.h ../lui5/colorbar.h ../lui5/destroy.h
script.o: ../lui5/dialog.h ../lui5/label.h ../lui5/layout.h
script.o: ../lui5/newlabel.h ../lui5/newslider.h ../lui5/popup.h
script.o: ../lui5/radio.h
slice.o: api.h gui.h ../lui5/lui.h ../lui5/browser.h ../lui5/list.h
slice.o: ../lui5/scrollbar.h ../lui5/field.h ../lui5/newbutton.h
slice.o: ../lui5/button.h ../lui5/buttonmatrix.h ../lui5/colorbar.h
slice.o: ../lui5/destroy.h ../lui5/dialog.h ../lui5/label.h ../lui5/layout.h
slice.o: ../lui5/newlabel.h ../lui5/newslider.h ../lui5/popup.h
slice.o: ../lui5/radio.h matrix.h vis5d.h v5d.h
sounding.o: globals.h graphics.h grid.h memory.h proj.h gui.h sounding.h
sounding.o: soundingGUI.h vis5d.h v5d.h
soundingGUI.o: globals.h gui.h sounding.h soundingGUI.h vis5d.h v5d.h
stream.o: memory.h globals.h api.h matrix.h sync.h v5d.h vis5d.h proj.h
sync.o: sync.h v5d.h
tclsave.o: globals.h api.h matrix.h sync.h v5d.h vis5d.h graphics.h gui.h
tclsave.o: ../lui5/lui.h ../lui5/browser.h ../lui5/list.h ../lui5/scrollbar.h
tclsave.o: ../lui5/field.h ../lui5/newbutton.h ../lui5/button.h
tclsave.o: ../lui5/buttonmatrix.h ../lui5/colorbar.h ../lui5/destroy.h
tclsave.o: ../lui5/dialog.h ../lui5/label.h ../lui5/layout.h
tclsave.o: ../lui5/newlabel.h ../lui5/newslider.h ../lui5/popup.h
tclsave.o: ../lui5/radio.h v5d.h
textplot.o: memory.h globals.h api.h matrix.h sync.h v5d.h vis5d.h
topo.o: binio.h globals.h api.h matrix.h sync.h v5d.h vis5d.h graphics.h
topo.o: image.h memory.h proj.h topo.h user_data.h v5d.h
traj.o: globals.h api.h matrix.h sync.h v5d.h vis5d.h grid.h proj.h
v5d.o: binio.h v5d.h vis5d.h
volume.o: globals.h api.h matrix.h sync.h v5d.h vis5d.h graphics.h grid.h
volume.o: memory.h proj.h volume.h
vtmcP.o: etableP.h memory.h globals.h api.h matrix.h sync.h v5d.h vis5d.h
work.o: analysis.h globals.h api.h matrix.h sync.h v5d.h vis5d.h contour.h
work.o: grid.h memory.h misc.h proj.h queue.h stream.h traj.h vtmcP.h work.h
rgbsliders.o: ../lui5/lui.h ../lui5/browser.h ../lui5/list.h
rgbsliders.o: ../lui5/scrollbar.h ../lui5/field.h ../lui5/newbutton.h
rgbsliders.o: ../lui5/button.h ../lui5/buttonmatrix.h ../lui5/colorbar.h
rgbsliders.o: ../lui5/destroy.h ../lui5/dialog.h ../lui5/label.h
rgbsliders.o: ../lui5/layout.h ../lui5/newlabel.h ../lui5/newslider.h
rgbsliders.o: ../lui5/popup.h ../lui5/radio.h api.h
rgbsliders.o: graphics.h globals.h matrix.h sync.h v5d.h vis5d.h gui.h
rgbsliders.o: mwmborder.h rgbsliders.h
isocolor.o: ../lui5/lui.h ../lui5/browser.h ../lui5/list.h
isocolor.o: ../lui5/scrollbar.h ../lui5/field.h ../lui5/newbutton.h
isocolor.o: ../lui5/button.h ../lui5/buttonmatrix.h ../lui5/colorbar.h
isocolor.o: ../lui5/destroy.h ../lui5/dialog.h ../lui5/label.h
isocolor.o: ../lui5/layout.h ../lui5/newlabel.h ../lui5/newslider.h
isocolor.o: ../lui5/popup.h ../lui5/radio.h api.h gui.h
isocolor.o: matrix.h vis5d.h v5d.h graphics.h globals.h sync.h isocolor.h
isocolor.o: mwmborder.h
linterp.o: globals.h linterp.h v5d.h
user_data.o: map.h topo.h globals.h v5d.h
imain.o: imain.h igui.h iapi.h file.h
igui.o: imain.h igui.h iapi.h file.h api.h globals.h
iapi.o: imain.h igui.h iapi.h file.h api.h globals.h
file.o: imain.h igui.h iapi.h file.h api.h globals.h

analyze_i.o: analyze_i.c analyze_i.h grid_i.h misc_i.h proj_i.h v5d.h

binio.o: binio.c
	$(CC) $(CFLAGS) binio.c
file_i.o: file_i.c binio.h file_i.h read_epa_i.h read_gr3d_i.h read_grid_i.h \
        read_grads_i.h read_uwvis_i.h read_v5d_i.h
grads_misc_i.o: grads_misc.f
	$(F77) $(FFLAGS) grads_misc.f
grid_i.o: grid_i.c grid_i.h projlist_i.h
gui_i.o: gui_i.c ../lui5/lui.h analyze_i.h file_i.h grid_i.h misc_i.h output_i.h proj_i.h \
	projlist_i.h v5d.h api.h gui.h
kltwin_i.o: kltwin.f
	$(F77) $(FFLAGS) $(OPT) kltwin.f
main_i.o: main_i.c analyze_i.h file_i.h grid_i.h gui_i.h ui_i.h v5d.h
output_i.o: output_i.c file_i.h grid_i.h projlist_i.h resample_i.h v5d.h api.h gui.h
proj_i.o: proj_i.c grid_i.h v5d.h
projlist_i.o: projlist_i.c grid_i.h projlist_i.h v5d.h
read_epa_i.o: read_epa_i.c grid_i.h model_i.h proj_i.h v5d.h
read_gr3d_i.o: read_gr3d_i.c binio.h grid_i.h proj_i.h v5d.h
read_grid_i.o: read_grid_i.c binio.h file_i.h grid_i.h proj_i.h v5d.h
read_grads_i.o: read_grads_i.c binio.h grid_i.h proj_i.h v5d.h
read_uwvis_i.o: read_uwvis_i.c v5d.h
read_v5d_i.o: read_v5d_i.c v5d.h file_i.h grid_i.h proj_i.h projlist_i.h
resample_i.o: resample_i.c grid_i.h proj_i.h resample_i.h topo_i.h v5d.h
select_i.o: select_i.c select_i.h grid_i.h
table_i.o: table_i.c grid_i.h
tokenize_i.o: tokenize_i.c
topo_i.o: topo_i.c
ui_i.o: ui_i.c grid_i.h tokenize_i.h v5d.h
v5d.o: v5d.c v5d.h
	$(CC) $(CFLAGS) v5d.c


#### EPA-provided code

alpha_i.o: alpha_i.c
	$(CC) $(CFLAGS) alpha_i.c

areal_i.o: areal_i.c
	$(CC) $(CFLAGS) areal_i.c

julian2std_i.o: julian2std_i.c
	$(CC) $(CFLAGS) julian2std_i.c

model_i.o: model_i.c model_i.h
	$(CC) $(CFLAGS) model_i.c

mm4_i.o: mm4_i.c
	$(CC) $(CFLAGS) mm4_i.c

mm4date2std_i.o: mm4date2std_i.c
	$(CC) $(CFLAGS) mm4date2std_i.c

netcdf2julian_i.o: netcdf2julian_i.c
	$(CC) $(CFLAGS) netcdf2julian_i.c

radm_i.o: radm_i.c
	$(CC) $(CFLAGS) radm_i.c

radm_emis_i.o: radm_emis_i.c
	$(CC) $(CFLAGS) radm_emis_i.c

rom_i.o: rom_i.c
	$(CC) $(CFLAGS) rom_i.c

uam_i.o: uam_i.c
	$(CC) $(CFLAGS) uam_i.c
