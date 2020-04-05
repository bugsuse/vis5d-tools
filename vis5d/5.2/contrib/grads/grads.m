# makefile for grads_2_vis5d.f conversion program

# The input to the conversion program is your 5-D grid format.
# The ouput is a v5d file.


# By default, the name of the conversion program is 'foo_to_v5d'.  You should
# probably use a better name.  Assign that name to PROGRAM here:

PROGRAM = grads_2_vis5d


# If DEC, add -DLITTLE to CFLAGS
# If SGI, add -cckr to CFLAGS
CFLAGS = -c -g -cckr -DUNDERSCORE
FFLAGS = -c -g
CC = cc
F77 = f77
LIBS = -lm


OBJECTS = $(PROGRAM).o binio.o v5d.o


$(PROGRAM): $(OBJECTS)
	$(F77) $(OBJECTS) $(LIBS) -o $@

$(PROGRAM).o: $(PROGRAM).f
	$(F77) $(FFLAGS) -I../../src $(PROGRAM).f

binio.o:  ../../src/binio.c
	$(CC) $(CFLAGS) ../../src/binio.c -o binio.o

v5d.o:  ../../src/v5d.c
	$(CC) $(CFLAGS) ../../src/v5d.c -o v5d.o

