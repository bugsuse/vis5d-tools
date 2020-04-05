include ../../config/make.site

objects = conv_rout_fault.o ltlwd.o rltlwd.o tlld.o \
         conv_rout_new.o ngetpoint.o rot_grib_lambo.o \
         findlimit.o rfindlimit.o rtlld.o destagfault.o \
	 grid_from_grib.o $(EXTRAOBJ_HIBU)

$(libdir)/libhibu.a : $(objects)
	$(AR) $(libdir)/libhibu.a $(objects) 

clean : 
	rm $(libdir)/libhibu.a $(objects)

dist :
	tar cvf $(libdir)/hibu.tar  *.f *.c Makefile

install : $(libdir)/libhibu.a
	install $(libdir)/libhibu.a $(DESTDIR)/lib
