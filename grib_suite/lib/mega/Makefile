include ../../config/make.site

objects = eladata.o

$(libdir)/libmega.a : $(objects)
	$(AR) $(libdir)/libmega.a $(objects) 

clean : 
	rm $(libdir)/libmega.a $(objects)

dist :
	tar cvf $(libdir)/mega.tar  *.f *.c Makefile

install : $(libdir)/libmega.a
	 install $(libdir)/libmega.a $(DESTDIR)/lib
