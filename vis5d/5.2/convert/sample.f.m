# makefile for FORTRAN sample.f conversion program

FFLAGS = -g -c
LIBS = -lm ../util/libmain.a

sample: sample.o
	f77 sample.o $(LIBS) -o $@

sample.o: sample.f
	f77 $(FFLAGS) sample.f
