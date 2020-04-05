# Uncomment the following if you have standard Vis5d
VIS5D = ${HOME}/tools/vis5d-5.2/
CFLAGS += -DSTDVIS5D -I$(VIS5D)/src
LDFLAGS += $(VIS5D)/src/binio.o -lm

# Uncomment the following if you have Vis5d+
#LDFLAGS += -lv5d

# Uncomment the following if you have a little endian machine (i386, Alpha)
CFLAGS += -DLITTLE

readglobe: readglobe.o writetopo.o

clean:
	rm -f *.o

dist:
	cd ..; tar cvf readglobe.tar readglobe/*.TXT readglobe/README readglobe/Makefile readglobe/*.c 
