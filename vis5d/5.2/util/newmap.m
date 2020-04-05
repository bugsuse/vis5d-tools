# Makefile for newmap program


# Usage:
#   make -f newmap.m



CC = cc
# Add -DLITTLE to CFLAGS if compiling for DEC!
CFLAGS = -c -g
F77 = f77
FFLAGS = -c -g
LFLAGS =
LIBS =


OBJECTS = newmap.o mapfunc.o binio.o


newmap: $(OBJECTS)
	$(F77) $(LFLAGS) $(OBJECTS) $(LIBS) -o $@


newmap.o: newmap.c ../src/binaryio.h
	$(CC) $(CFLAGS) -I../src newmap.c

mapfunc.o: mapfunc.f
	$(F77) $(FFLAGS) mapfunc.f

binaryio.o: ../src/binaryio.c
	$(CC) $(CFLAGS) ../src/binaryio.c -o $@
