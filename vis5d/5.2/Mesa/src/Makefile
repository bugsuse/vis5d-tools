# $Id: Makefile,v 1.12 1997/12/29 23:43:55 brianp Exp brianp $

# Mesa 3-D graphics library
# Version:  2.6
# Copyright (C) 1995-1997  Brian Paul
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Library General Public License for more details.
#
# You should have received a copy of the GNU Library General Public
# License along with this library; if not, write to the Free
# Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


# Makefile for core library


# $Log: Makefile,v $
# Revision 1.12  1997/12/29 23:43:55  brianp
# added fxmesa6.c file
#
# Revision 1.11  1997/12/07 17:31:35  brianp
# added fxmesa4.c and fxmesa5.c for v0.21 fxmesa driver
#
# Revision 1.10  1997/11/07 03:38:31  brianp
# added rule for assembling .S files
#
# Revision 1.9  1997/11/02 20:22:30  brianp
# added ASM_SOURCES stuff
#
# Revision 1.8  1997/10/25 01:23:52  brianp
# now passing major, minor version numbers to lib maker
#
# Revision 1.7  1997/08/23 18:42:09  brianp
# split api.c into api1.c and api2.c, added hash.c
#
# Revision 1.6  1997/05/27 03:04:04  brianp
# replaced glx.c with glxapi.c fakeglx.c realglx.c
#
# Revision 1.5  1997/05/09 02:41:47  brianp
# added mmath.c
#
# Revision 1.4  1997/04/02 03:52:39  brianp
# many object files added/removed due to vertex buffer reorganization
#
# Revision 1.3  1997/02/24 20:05:54  brianp
# updated version to 2.2
#
# Revision 1.2  1996/09/27 01:33:24  brianp
# removed fortran.c
#
# Revision 1.1  1996/09/13 01:37:56  brianp
# Initial revision
#



##### MACROS #####

VPATH = RCS

INCDIR = ../include
LIBDIR = ../lib

CORE_SOURCES = accum.c alpha.c alphabuf.c api1.c api2.c attrib.c bitmap.c \
	blend.c clip.c colortab.c context.c copypix.c depth.c \
	dlist.c drawpix.c enable.c eval.c feedback.c fog.c \
	get.c hash.c image.c light.c lines.c logic.c masking.c matrix.c \
	misc.c mmath.c pb.c pixel.c points.c pointers.c polygon.c \
	quads.c rastpos.c readpix.c rect.c scissor.c shade.c span.c \
	stencil.c teximage.c texobj.c texstate.c texture.c triangle.c \
	varray.c winpos.c vb.c vbfill.c vbrender.c vbxform.c xform.c \
	mthreads.c

DRIVER_SOURCES = glxapi.c fakeglx.c realglx.c xfonts.c \
	xmesa1.c xmesa2.c xmesa3.c xmesa4.c \
	osmesa.c svgamesa.c \
	fxmesa1.c fxmesa2.c fxmesa3.c fxmesa4.c fxmesa5.c fxmesa6.c

ASM_SOURCES = 


OBJECTS = $(ASM_SOURCES:.S=.o) $(CORE_SOURCES:.c=.o) $(DRIVER_SOURCES:.c=.o)



##### RULES #####

.c.o:
	$(CC) -c -I$(INCDIR) $(CFLAGS) $<

.S.o:
	$(CC) -c $(CFLAGS) $<



##### TARGETS #####

default:
	@echo "Specify a target configuration"

clean:
	-rm *.o *~

targets: $(LIBDIR)/$(GL_LIB)

# Make the library
$(LIBDIR)/$(GL_LIB): $(OBJECTS)
	$(MAKELIB) $(GL_LIB) $(MAJOR) $(MINOR) $(OBJECTS)
	mv $(GL_LIB)* $(LIBDIR)


include ../Make-config

include depend



#
# Run 'make dep' to update the dependencies if you change what's included
# by any source file.
# 
dep: $(CORE_SOURCES) $(DRIVER_SOURCES)
	makedepend -fdepend -Y -I../include $(CORE_SOURCES) $(DRIVER_SOURCES)
