# Makefile for external functions.

# This makefile must be called from the externf script only.

# The following variables are defined in 'externf'
#	EXTFUNC		the program to make
#	EXTLIBS		any libraries needed for linking
#	CFLAGS		C compiler flags
#	FFLAGS		FORTRAN compiler flags
#	LFLAGS		linker flags


# Where to find src/ and util/ directories:
VIS5D_HOME = ../


OBJS = extmain.o socketio.o
LIBS = $(VIS5D_HOME)util/libmain.a -lm $(EXTLIBS)


# Link
$(EXTFUNC): $(EXTFUNC).o $(OBJS)
	f77 $(LFLAGS) $(EXTFUNC).o $(OBJS) $(LIBS) -o $@ $(LFLAGS)


# Compile the function
$(EXTFUNC).o: $(EXTFUNC).f
	f77 $(FFLAGS) $(EXTFUNC).f


# Compile other needed objects
extmain.o: $(VIS5D_HOME)src/extmain.c
	cc $(CFLAGS) $(VIS5D_HOME)src/extmain.c -o extmain.o

socketio.o: $(VIS5D_HOME)src/socketio.c
	cc $(CFLAGS) $(VIS5D_HOME)src/socketio.c -o socketio.o
