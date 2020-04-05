#	librerie = hibu.a mega.a grib_util.a

all :
	(cd grib_util; make);(cd hibu; make);(cd mega ; make)

install :
	(cd grib_util; make install);(cd hibu; make install);(cd mega ; make install)

clean :
	(cd grib_util; make clean ); (cd hibu; make clean);(cd mega ; make clean )

dist :
	tar cvf all.tar licenza.txt Makefile grib_util hibu mega 
