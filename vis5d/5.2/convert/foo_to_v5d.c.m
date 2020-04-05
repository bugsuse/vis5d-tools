# makefile for foo_to_v5d.c conversion program

# The input to the conversion program is your 5-D grid format.
# The ouput is a v5d file.


# By default, the name of the conversion program is 'foo_to_v5d'.  You should
# probably use a better name.  Assign that name to PROGRAM here:

PROGRAM = foo_to_v5d


# If DEC, add -DLITTLE to CFLAGS
# If DEC or Linux, (Little-endian), add -DLITTLE to CFLAGS
CFLAGS = -c -g
CC = cc
LIBS = -lm


OBJECTS = $(PROGRAM).o binio.o v5d.o


../$(PROGRAM): $(OBJECTS)
	$(CC) $(OBJECTS) $(LIBS) -o $@

$(PROGRAM).o: $(PROGRAM).c
	$(CC) $(CFLAGS) -I../src $(PROGRAM).c

binio.o:  ../src/binio.c
	$(CC) $(CFLAGS) ../src/binio.c -o binio.o

v5d.o:  ../src/v5d.c
	$(CC) $(CFLAGS) ../src/v5d.c -o v5d.o

