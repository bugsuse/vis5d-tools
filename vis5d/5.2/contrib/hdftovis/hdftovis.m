# makefile for FORTRAN hdftovis.F conversion program

# This makefile is setup for IRIX (SGI)
# See the other Vis5D makefiles for compiler flags you may need for other
# operating systems


CC = cc
CFLAGS = -c -cckr -DUNDERSCORE
F77 = f77
FFLAGS = -g -c
LIBS = -lm libdf.a

OBJS = hdftovis.o args.o compal.o ../../util/libmain.a


# note:  replace /usr2/people/butler with directory where libdf.a is

hdftovis: $(OBJS)
	$(F77) $(OBJS) $(LIBS) -o $@

hdftovis.o: hdftovis.F
	$(F77) $(FFLAGS) hdftovis.F 

args.o: args.c
	$(CC) $(CFLAGS) args.c

compal.o: compal.c
	$(CC) $(CFLAGS) compal.c
