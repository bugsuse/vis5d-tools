# generic makefile
# SR is directory with source files
# OB is directory for object files
# include files are assumed to be in SR, that is -I$(SR)
#
SR = ./src/
OB = ./obj/

GRIB5D=	$(OB)grib5d.o $(OB)assign5.o $(OB)fetch5.o $(OB)hrzintp5.o \
	$(OB)locate5.o $(OB)open5d.o $(OB)sort5.o $(OB)unpack5.o \
	$(OB)vfetch5.o $(OB)vhrzint5.o $(OB)vsort5.o \
	$(OB)vrtintp5.o $(OB)vvrtint5.o $(OB)windcon.o $(OB)write5d.o \
	$(OB)bword.o $(OB)gsdate.o $(OB)w3fb00.o $(OB)w3fc02.o \
	$(OB)exbits.o $(OB)dyofyr.o $(OB)lscale.o $(OB)x9ie32.o \
	$(OB)gribinfo.o $(OB)gridproj.o $(OB)w3ft01.o $(OB)set_sign.o

grib5d:	$(GRIB5D)
	cc -o grib5d $(GRIB5D) -lc -lm

# Compile source 

$(OB)assign5.o:	$(SR)assign5.c $(SR)want.h $(SR)winds.h $(SR)grid5d.h
	cc -c -g 		$(SR)assign5.c -I$(SR)
	mv assign5.o	$(OB)assign5.o

$(OB)bword.o:		$(SR)bword.c
	cc -c -g 		$(SR)bword.c
	mv bword.o	$(OB)bword.o

$(OB)dyofyr.o:		$(SR)dyofyr.c
	cc -c -g 		$(SR)dyofyr.c
	mv dyofyr.o	$(OB)dyofyr.o

$(OB)exbits.o:		$(SR)exbits.c
	cc -c -g 		$(SR)exbits.c
	mv exbits.o	$(OB)exbits.o

$(OB)fetch5.o:		$(SR)fetch5.c $(SR)want.h $(SR)buffer.h
	cc -c -g 		$(SR)fetch5.c -I$(SR)
	mv fetch5.o	$(OB)fetch5.o

$(OB)gribinfo.o:	$(SR)gribinfo.c $(SR)winds.h $(SR)buffer.h \
				$(SR)gribinfo.h
	cc -c -g 		$(SR)gribinfo.c -I$(SR)
	mv gribinfo.o	$(OB)gribinfo.o

$(OB)grib5d.o:	$(SR)grib5d.c $(SR)want.h $(SR)buffer.h $(SR)gribinfo.h \
			$(SR)ktypes.h $(SR)grid5d.h $(SR)winds.h
	cc -c -g 		$(SR)grib5d.c -I$(SR)
	mv grib5d.o	$(OB)grib5d.o

$(OB)gridproj.o:	$(SR)gridproj.c $(SR)ktypes.h $(SR)buffer.h \
				$(SR)gribinfo.h
	cc -c -g 		$(SR)gridproj.c -I$(SR)
	mv gridproj.o	$(OB)gridproj.o

$(OB)gsdate.o:		$(SR)gsdate.c
	cc -c -g 		$(SR)gsdate.c
	mv gsdate.o	$(OB)gsdate.o

$(OB)hrzintp5.o:	$(SR)hrzintp5.c $(SR)want.h $(SR)buffer.h \
				$(SR)gribinfo.h $(SR)grid5d.h
	cc -c -g 		$(SR)hrzintp5.c -I$(SR)
	mv hrzintp5.o	$(OB)hrzintp5.o

$(OB)locate5.o:		$(SR)locate5.c $(SR)want.h $(SR)buffer.h
	cc -c -g 		$(SR)locate5.c -I$(SR)
	mv locate5.o	$(OB)locate5.o

$(OB)lscale.o:		$(SR)lscale.c
	cc -c -g 		$(SR)lscale.c
	mv lscale.o	$(OB)lscale.o

$(OB)open5d.o:		$(SR)open5d.c $(SR)want.h
	cc -c -g 		$(SR)open5d.c -I$(SR)
	mv open5d.o	$(OB)open5d.o

$(OB)set_sign.o:	$(SR)set_sign.c
	cc -c -g 		$(SR)set_sign.c
	mv set_sign.o	$(OB)set_sign.o

$(OB)sort5.o:		$(SR)sort5.c $(SR)want.h
	cc -c -g 		$(SR)sort5.c -I$(SR)
	mv sort5.o	$(OB)sort5.o

$(OB)unpack5.o:		$(SR)unpack5.c $(SR)buffer.h $(SR)gribinfo.h
	cc -c -g 		$(SR)unpack5.c -I$(SR)
	mv unpack5.o	$(OB)unpack5.o

$(OB)vfetch5.o:		$(SR)vfetch5.c $(SR)want.h $(SR)buffer.h \
				$(SR)gribinfo.h $(SR)winds.h
	cc -c -g 		$(SR)vfetch5.c -I$(SR)
	mv vfetch5.o	$(OB)vfetch5.o

$(OB)vhrzint5.o:	$(SR)vhrzint5.c $(SR)want.h $(SR)buffer.h \
				$(SR)gribinfo.h $(SR)grid5d.h
	cc -c -g 		$(SR)vhrzint5.c -I$(SR)
	mv vhrzint5.o	$(OB)vhrzint5.o

$(OB)vsort5.o:		$(SR)vsort5.c $(SR)want.h $(SR)winds.h
	cc -c -g 		$(SR)vsort5.c -I$(SR)
	mv vsort5.o	$(OB)vsort5.o

$(OB)vrtintp5.o:	$(SR)vrtintp5.c $(SR)want.h $(SR)grid5d.h
	cc -c -g 		$(SR)vrtintp5.c -I$(SR)
	mv vrtintp5.o	$(OB)vrtintp5.o

$(OB)vvrtint5.o:	$(SR)vvrtint5.c $(SR)want.h $(SR)grid5d.h
	cc -c -g 		$(SR)vvrtint5.c -I$(SR)
	mv vvrtint5.o	$(OB)vvrtint5.o

$(OB)windcon.o:		$(SR)windcon.c $(SR)want.h $(SR)winds.h $(SR)buffer.h
	cc -c -g 		$(SR)windcon.c -I$(SR)
	mv windcon.o	$(OB)windcon.o

$(OB)write5d.o:		$(SR)write5d.c $(SR)want.h $(SR)winds.h $(SR)grid5d.h
	cc -c -g 		$(SR)write5d.c -I$(SR)
	mv write5d.o	$(OB)write5d.o

$(OB)w3fb00.o:		$(SR)w3fb00.c
	cc -c -g 		$(SR)w3fb00.c
	mv w3fb00.o	$(OB)w3fb00.o

$(OB)w3fc02.o:		$(SR)w3fc02.c
	cc -c -g 		$(SR)w3fc02.c
	mv w3fc02.o	$(OB)w3fc02.o

$(OB)w3ft01.o:		$(SR)w3ft01.c
	cc -c -g 		$(SR)w3ft01.c
	mv w3ft01.o	$(OB)w3ft01.o

$(OB)x9ie32.o:		$(SR)x9ie32.c
	cc -c -g 		$(SR)x9ie32.c
	mv x9ie32.o	$(OB)x9ie32.o
