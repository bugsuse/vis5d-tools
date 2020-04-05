# Makefile for vis_to_v5d


default:
	@echo "To make vis_to_v5d ype one of the following:"
	@echo "   make sgi		for SGI systems"
	@echo "   make ibm		for IBM RS/6000 systems"
	@echo "   make cray		for CRAY 2, YMP systems"
	@echo "   make clean		to remove .o files"
	@echo "If your system is not on this list you must edit the makefile"


sgi:
	make targets \
	"CC = cc" \
	"CFLAGS = -c -O -cckr -I$(INC) -DUNDERSCORE"

ibm:
	make targets \
	"CC = cc" \
	"CFLAGS = -c -O -w -Dibm -I$(INC)"

cray:
	make targets \
	"CC = scc" \
	"CFLAGS = -c -O3 -I$(INC)"

# For other systems, add lines similar to those above.


clean:
	-rm *.o *~


######################################################################

INC = ../../src
V5D_OBJS = ../../src/v5d.o ../../src/binio.o


targets: vis_to_v5d


vis_to_v5d: vis_to_v5d.o $(V5D_OBJS)
	$(CC) vis_to_v5d.o $(V5D_OBJS) -lm -o $@

vis_to_v5d.o: vis_to_v5d.c $(INC)/v5d.h $(INC)/binio.h
	$(CC) $(CFLAGS) vis_to_v5d.c

