include ../config/make.site

all  : cong menogrib ged gribbo displgrib

cong : cong.o comuni.o
	$(FC) $(FFOPTS) -o cong cong.o comuni.o  -L$(LIBDIR) -lgrib_util -lhibu -lemos
#	$(EMOSLIB)
#	$(gribexhome)/gribex.a


cong.o : cong.f
	$(FC)  $(FFOPTS) -c cong.f

menogrib : menogrib.o
	$(FC)  $(FFOPTS) -o menogrib menogrib.o -L$(LIBDIR) -lemos
#	$(EMOSLIB)
#	 $(gribexhome)/gribex.a

menogrib.o : menogrib.f
	$(FC)  $(FFOPTS) -c menogrib.f


ged : ged.o comuni.o 
	$(FC)  $(FFOPTS) -o ged ged.o comuni.o -L$(LIBDIR) -lhibu -lgrib_util -lemos
#		$(EMOSLIB)
#		$(gribexhome)/gribex.a


comuni.o : comuni.f  comuni.h
	$(FC)  $(FFOPTS) -c comuni.f

ged.o : ged.f
	$(FC)  $(FFOPTS) -c ged.f

gribbo : gribbo.o
	$(FC)  $(FFOPTS) -o gribbo gribbo.o -L$(LIBDIR) -lhibu -lemos
#		$(EMOSLIB)
#		$(gribexhome)/gribex.a


gribbo.o : gribbo.f
	$(FC)  $(FFOPTS) -c gribbo.f


displgrib : displgrib.o
	$(FC)  $(FFOPTS) -o displgrib displgrib.o -L$(LIBDIR) -lemos
#	$(EMOSLIB)
#	$(gribexhome)/gribex.a


displgrib.o : displgrib.f
	$(FC)  $(FFOPTS) -c displgrib.f


clean :
		rm -f cong cong.o menogrib menogrib.o ged.o ged ;
		rm -f displgrib displgrib.o gribbo gribbo.o comuni.o ;

install: cong menogrib ged displgrib gribbo
		$(INSTALLCMD) cong $(DESTDIR)
		$(INSTALLCMD) menogrib $(DESTDIR)
		$(INSTALLCMD) ged $(DESTDIR)
		$(INSTALLCMD) displgrib $(DESTDIR)
		$(INSTALLCMD) gribbo $(DESTDIR)


