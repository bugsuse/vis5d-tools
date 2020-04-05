include config/make.site

all  : esempio1 esempio2 esempio3 

esempio1 : esempio1.o
	$(FC) $(FFOPTS) -o esempio1 esempio1.o  -L$(LIBDIR) -lgrib_util -lhibu -lemos
esempio2 : esempio2.o
	$(FC) $(FFOPTS) -o esempio2 esempio2.o  -L$(LIBDIR) -lgrib_util -lhibu -lemos
esempio3 : esempio3.o
	$(FC) $(FFOPTS) -o esempio3 esempio3.o  -L$(LIBDIR) -lgrib_util -lhibu -lemos


install: esempio1 esempio2 esempio3
		$(INSTALLCMD) esempio1 $(DESTDIR)
		$(INSTALLCMD) esempio2 $(DESTDIR)
		$(INSTALLCMD) esempio3 $(DESTDIR)

clean:
		rm -rf esempio1.o esempio2.o esempio3.o
		rm -rf esempio1 esempio2 esempio3

