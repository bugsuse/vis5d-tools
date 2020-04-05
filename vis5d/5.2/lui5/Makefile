# Makefile for lui (Little User Interface?) library

# Version 5 (lui5) is same as LUI 4 but cleaned up for ANSI C


# Original info:
#	"@(#)Makefile	1.2 Stellar 89/03/29"
#			Copyright 1989 by
#			Stellar Computer Inc.
#			All Rights Reserved
#	
#	This software comprises unpublished confidential information of
#	Stellar Computer Inc. and may not be used, copied or made
#	available to anyone, except in accordance with the license
#	under which it is furnished.



default:
	@echo "To make liblui.a, type one of the following:"
	@echo "   make alpha"
	@echo "   make cray"
	@echo "   make dec"
	@echo "   make hp"
	@echo "   make ibm"
	@echo "   make irix4"
	@echo "   make irix5"
	@echo "   make irix6.0.1"
	@echo "   make irix6"
	@echo "   make linux"
	@echo "   make linux-ppc"
	@echo "   make stellar"
	@echo "   make sunos4"
	@echo "   make sunos5"
	@echo "If your system is not listed you'll have to edit the Makefile"



debug:
	pmake target \
	"CC = ${CC} -g -n32" \
	"CFLAGS = -xansi -fullwarn -g" \
	"RANLIB = true"


alpha:
	$(MAKE) target \
	"CC = ${CC}" \
	"CFLAGS = -g -traditional -DDENALI" \
	"RANLIB = true"

cray:
	$(MAKE) target \
	"CC = ${CC}" \
	"CFLAGS = -g" \
	"RANLIB = true"

dec:
	$(MAKE) target \
	"CC = ${CC}" \
	"CFLAGS = -g -Ddec" \
	"RANLIB = ranlib"

freebsd:
	$(MAKE) target \
	"CC = ${CC}" \
	"CFLAGS += -g -pipe -I/usr/X11R6/include" \
	"RANLIB = ranlib"

hp:
	$(MAKE) target \
	"CC = ${CC}" \
	"CFLAGS = -Aa -g -D_HPUX_SOURCE -Dhp -I/usr/include/X11R5" \
	"RANLIB = true"

ibm:
	$(MAKE) target \
	"CC = ${CC}" \
	"CFLAGS = -g -Dibm" \
	"RANLIB = true"

irix4:
	pmake target \
	"CC = ${CC} -g" \
	"CFLAGS = -ansiposix -O" \
	"RANLIB = true"

irix5:
	pmake target \
	"CC = ${CC} -g" \
	"CFLAGS = -xansi" \
	"RANLIB = true"

irix6:
	pmake target \
	"CC = ${CC} " \
	"CFLAGS = -32 -xansi -O" \
	"RANLIB = true"

irix6-mips4:
	pmake target \
	"CC = ${CC}" \
	"CFLAGS = -n32 -xansi -O" \
	"RANLIB = true"

irix6.0.1:
	pmake target \
	"CC = ${CC}" \
	"CFLAGS = -xansi -woff all -O" \
	"RANLIB = true"

linux-ppc:
	$(MAKE) target \
	"CC = ${CC}" \
	"CFLAGS = -pipe -g -mcpu=604"\  
	"RANLIB = ranlib"       

linux:
	$(MAKE) target \
	"CC = ${CC}" \
	"CFLAGS = -pipe -g" \
	"RANLIB = ranlib"

sunos4:
	$(MAKE) target \
	"CC = ${CC}" \
	"CFLAGS = -g" \
	"RANLIB = ranlib"

sunos4-gcc:
	$(MAKE) target \
	"CC = ${CC}" \
	"CFLAGS = -g" \
	"RANLIB = ranlib"

sunos5:
	$(MAKE) target \
	"CC = ${CC}" \
	"CFLAGS = -g -Xa -I/usr/openwin/include" \
	"RANLIB = true"

sunos5-gcc:
	$(MAKE) target \
	"CC = ${CC}" \
	"CFLAGS = -g" \
	"RANLIB = ranlib"

sunos5-ultra:
	$(MAKE) target \
	"CC = ${CC}" \
	"CFLAGS = -fast -O -xtarget=ultra -xarch=v8plusa -I/usr/openwin/include" \
	"RANLIB = true"


clean:
	-rm *.o *~ liblui.a liblui2.a


######################################################################


target: liblui.a liblui2.a


HDRS =  		\
	browser.h	\
	newbrowser.h	\
	button.h	\
	buttonmatrix.h	\
	dial.h		\
	dialog.h	\
	field.h		\
	label.h		\
	layout.h	\
	list.h		\
	newlist.h	\
	newbutton.h	\
	newlabel.h	\
	newslider.h	\
	pixmaps.h	\
	popup.h		\
	radio.h		\
	scrollbar.h	\
	lui.h	

OBJS = 			\
	browser.o	\
	newbrowser.o	\
	button.o	\
	buttonmatrix.o	\
	colorbar.o	\
	destroy.o	\
	event.o		\
	field.o		\
	label.o		\
	layout.o	\
	list.o		\
	newlist.o	\
	newbutton.o	\
	newlabel.o	\
	newslider.o	\
	pixmaps.o	\
	radio.o		\
	scrollbar.o	\
	lui.o		

OBJS2 =                  \
	browser.o	\
	browser2.o	\
	newbrowser.o	\
	button.o 	\
	buttonmatrix.o	\
	colorbar.o	\
	destroy.o	\
	event.o		\
	field.o		\
	label.o		\
	layout.o	\
	list.o		\
	newlist.o	\
	newbutton.o	\
	newlabel.o	\
	newslider.o	\
	pixmaps.o	\
	radio.o		\
	scrollbar.o	\
	lui.o


liblui.a: $(OBJS)
	ar ruv liblui.a $(OBJS)
	$(RANLIB) liblui.a

liblui2.a: $(OBJS2)
	ar ruv liblui2.a $(OBJS2)
	$(RANLIB) liblui2.a
	
.c.o:
	$(CC) -c $(CFLAGS) $<


browser.o: list.h field.h newbutton.h 

newbrowser.o: newlist.h field.h newbutton.h


#############################################################

test: test.o liblui.a
	cc test.o liblui.a -lX11 -o $@


test.o: test.c
	cc -c -g -cckr test.c



testlui.o: testlui.c
	cc -c -g -cckr testlui.c

testlui: testlui.o
	cc testlui.o liblui.a -lX11 -o $@


bm: bm.o
	cc bm.o liblui.a -lX11 -o $@

bm.o: bm.c
	cc -c -g -cckr bm.c
