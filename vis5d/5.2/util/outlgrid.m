# Makefile for newmap program


CFLAGS = -c -g -cckr
FFLAGS = -c -g
LFLAGS =
LIBS =

OBJECTS = outlgrid.o binio.o


outlgrid: $(OBJECTS)
	$(CC) $(LFLAGS) $(OBJECTS) $(LIBS) -o $@


outlgrid.o: outlgrid.c ../src/binio.h
	$(CC) $(CFLAGS) -I../src outlgrid.c

binio.o: ../src/binio.c
	$(CC) $(CFLAGS) ../src/binio.c -o $@
