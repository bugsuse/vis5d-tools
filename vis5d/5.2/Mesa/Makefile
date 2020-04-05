# Top-level makefile for Mesa

# Mesa 3-D graphics library
# Version:  2.6
# Copyright (C) 1995-1997  Brian Paul
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Library General Public License for more details.
#
# You should have received a copy of the GNU Library General Public
# License along with this library; if not, write to the Free
# Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


# $Id: Makefile,v 1.47 1998/02/12 03:37:58 brianp Exp $

# $Log: Makefile,v $
# Revision 1.47  1998/02/12 03:37:58  brianp
# added missing demos/Makefiles to tar file list
#
# Revision 1.46  1998/02/05 00:50:59  brianp
# added mtdemos to DEMO_FILES list
#
# Revision 1.45  1998/02/05 00:36:19  brianp
# added John Stone's configs which support threads
#
# Revision 1.44  1998/02/04 00:08:08  brianp
# added cygnus-linux config from Stephane Rehel
#
# Revision 1.43  1998/01/30 01:53:24  brianp
# fixed-up the realclean target (Stephane Rehel)
#
# Revision 1.42  1998/01/17 15:12:18  brianp
# added WIN32 directory to tar file
#
# Revision 1.41  1998/01/06 01:36:01  brianp
# renamed demos/ to xdemos/ and renamed gdemos/ to demos/
# updated BeOS config
# added missing VMS files to tar file list
#
# Revision 1.40  1997/12/29 23:58:16  brianp
# separate HPUX 9.x and 10.x configs
#
# Revision 1.39  1997/12/09 03:02:08  brianp
# minor tar file change
#
# Revision 1.38  1997/12/07 17:38:42  brianp
# updated version to 2.6, added freebsd-386 and linux-386-quake-glide configs
#
# Revision 1.37  1997/12/01 01:12:30  brianp
# added freebsd-386 config (Daniel J. O'Connor)
#
# Revision 1.36  1997/11/20 02:21:29  brianp
# added more files to tar list
#
# Revision 1.35  1997/11/14 03:03:29  brianp
# added a few more tar files
#
# Revision 1.34  1997/10/30 05:55:22  brianp
# added linux-386 (Linux w/ Intel x86 assembly language)
#
# Revision 1.33  1997/10/16 23:30:13  brianp
# added sunos5-ultra-sl from Scott Hofmann
#
# Revision 1.32  1997/10/16 23:29:05  brianp
# bunch of changes for Mesa 2.5, added GLUT
#
# Revision 1.31  1997/08/24 17:26:57  brianp
# changed order of commands for openstep target (Pete French)
#


SHELL = /bin/sh


# To add a new configuration for your system add it to the list below
# then update the Make-config file.



default:
	@echo "Type one of the following:"
	@echo "  make aix                  for IBM RS/6000 with AIX"
	@echo "  make aix-sl               for IBM RS/6000, make shared libs"
	@echo "  make amiwin               for Amiga with SAS/C and AmiWin"
	@echo "  make amix                 for Amiga 3000 UX  SVR4 v2.1 systems"
	@echo "  make beos                 for BeOS with PR or later"
	@echo "  make beos-glide           for BeOS with PR or later, 3Dfx Glide driver"
	@echo "  make bsdos                for BSD/OS from BSDI using GCC"
	@echo "  make cygnus               for Win95/NT using Cygnus-Win32"
	@echo "  make cygnus-linux         for Win95/NT using Cygnus-Win32 under Linux"
	@echo "  make dgux                 for Data General"
	@echo "  make freebsd              for FreeBSD systems with GCC"
	@echo "  make freebsd-386          for FreeBSD systems with GCC, w/ Intel assembly"
	@echo "  make gcc                  for a generic system with GCC"
	@echo "  make hpux9                for HP systems with HPUX 9.x"
	@echo "  make hpux9-sl             for HP systems with HPUX 9.x, make shared libs"
	@echo "  make hpux9-gcc            for HP systems with HPUX 9.x using GCC"
	@echo "  make hpux9-gcc-sl         for HP systems with HPUX 9.x, GCC, make shared libs"
	@echo "  make hpux10               for HP systems with HPUX 10.x"
	@echo "  make hpux10-gcc           for HP systems with HPUX 10.x using GCC"
	@echo "  make irix4                for SGI systems with IRIX 4.x"
	@echo "  make irix5                for SGI systems with IRIX 5.x"
	@echo "  make irix5-dso            for SGI systems with IRIX 5.x, make DSOs"
	@echo "  make irix6-32             for SGI systems with IRIX 6.x, make 32-bit libs"
	@echo "  make irix6-32-dso         for SGI systems with IRIX 6.x, make 32-bit DSOs"
	@echo "  make irix6-n32            for SGI systems with IRIX 6.x, make n32-bit libs"
	@echo "  make irix6-n32-dso        for SGI systems with IRIX 6.x, make n32-bit DSOs"
	@echo "  make irix6-64             for SGI systems with IRIX 6.x, make 64-bit libs"
	@echo "  make irix6-64-dso         for SGI systems with IRIX 6.x, make 64-bit DSOs"

	@echo "  make linux                for Linux systems with GCC"
	@echo "  make linux-elf            for Linux systems, make ELF shared libs"
	@echo "  make linux-386            for Linux w/ Intel assembly"
	@echo "  make linux-386-elf        for Linux w/ Intel assembly, make ELF shared libs"
	@echo "  make linux-386-pthread    for Linux w/ Intel assembly and linuxthreads"
	@echo "  make linux-alpha          for Linux on Alpha systems"
	@echo "  make linux-alpha-elf      for Linux on Alpha systems, make ELF shared libs"
	@echo "  make linux-ppc            for Linux on PowerPC systems"
	@echo "  make linux-glide          for Linux w/ 3Dfx Glide driver"
	@echo "  make linux-386-glide      for Linux w/ 3Dfx Glide driver, Intel assembly"
	@echo "  make linux-386-quake-glide for Linux w/ 3Dfx for GLQuake"
	@echo "  make lynxos               for LynxOS systems with GCC"
	@echo "  make mswindows            for Microsoft Windows"
	@echo "  make macintosh            for Macintosh"
	@echo "  make machten-2.2          for Macs w/ MachTen 2.2 (68k w/ FPU)"
	@echo "  make machten-4.0          for Macs w/ MachTen 4.0.1 or newer with GNU make"
	@echo "  make mklinux              for Linux on Power Macintosh"
	@echo "  make netbsd               for NetBSD 1.0 systems with GCC"
	@echo "  make next                 for NeXT systems with NEXTSTEP 3.3"
	@echo "  make next-x86-x11         for NeXT on Intel x86 with X11"
	@echo "  make next-x11             for NeXT with X11"
	@echo "  make openbsd              for OpenBSD systems"
	@echo "  make openstep             for NeXT systems with OPENSTEP 4.0"
	@echo "  make os2                  for OS/2 with XFree86"
	@echo "  make osf1                 for DEC Alpha systems with OSF/1"
	@echo "  make osf1-sl              for DEC Alpha systems with OSF/1, make shared libs"
	@echo "  make qnx                  for QNX V4 systems with Watcom compiler"
	@echo "  make sco                  for SCO Unix systems with ODT"
	@echo "  make solaris-x86          for PCs with Solaris"
	@echo "  make solaris-x86-gcc      for PCs with Solaris using GCC"
#	@echo "  make solaris-gcc          for Solaris 2 systems with GCC"
	@echo "  make sunos4               for Suns with SunOS 4.x"
	@echo "  make sunos4-sl            for Suns with SunOS 4.x, make shared libs"
	@echo "  make sunos4-gcc           for Suns with SunOS 4.x and GCC"
	@echo "  make sunos4-gcc-sl        for Suns with SunOS 4.x, GCC, make shared libs"
	@echo "  make sunos5               for Suns with SunOS 5.x"
	@echo "  make sunos5-sl            for Suns with SunOS 5.x, make shared libs"
	@echo "  make sunos5-ultra         for Sun UltraSPARCs with SunOS 5.x"
	@echo "  make sunos5-ultra-sl      for Sun UltraSPARCs with SunOS 5.x, make shared libs"
	@echo "  make sunos5-thread        for Suns with SunOS 5.x, using Solaris threads"
	@echo "  make sunos5-pthread       for Suns with SunOS 5.[56] using POSIX threads"
	@echo "  make sunos5-gcc           for Suns with SunOS 5.x and GCC"
	@echo "  make sunos5-gcc-sl        for Suns with SunOS 5.x, GCC, make shared libs"
	@echo "  make sunos5-gcc-thread    for Suns with SunOS 5.x and GCC, using Solaris threads"
	@echo "  make sunos5-gcc-pthread   for Suns with SunOS 5.[56] and GCC, using POSIX threads"
	@echo "  make sunos5-x11r6-gcc-sl  for Suns with X11R6, GCC, make shared libs"
	@echo "  make sunSolaris-CC        for Solaris using C++ compiler"
	@echo "  make ultrix-gcc           for DEC systems with Ultrix and GCC"
	@echo "  make unicos               for Cray C90 (and other?) systems"
	@echo "  make unixware             for PCs running UnixWare"
	@echo "  make unixware-shared      for PCs running UnixWare, shared libs"
	@echo "  make vistra               for Stardent Vistra systems"
	@echo "  make clean                remove .o files"
	@echo "  make realclean            remove .o, library and executable files"



aix aix-sl amix bsdos debug dgux freebsd freebsd-386 gcc \
hpux9 hpux9-gcc hpux9-sl hpux9-gcc-sl hpux10 hpux10-gcc \
irix4 irix5 irix5-dso irix6-32 irix6-n32 irix6-64 \
irix6-32-dso irix6-n32-dso irix6-64-dso \
linux linux-debug linux-elf linux-glide linux-386-glide linux-386-quake-glide \
linux-386 linux-386-elf linux-alpha linux-alpha-elf linux-ppc \
linux-386-pthread \
lynxos machten-2.2 machten-4.0 \
mklinux netbsd next-x86-x11 next-x11 osf1 osf1-sl openbsd qnx sco \
solaris-x86 solaris-x86-gcc sunSolaris-CC \
sunos4 sunos4-sl sunos4-gcc sunos4-gcc-sl \
sunos5 sunos5-sl sunos5-ultra sunos5-ultra-sl sunos5-gcc sunos5-gcc-sl \
sunos5-thread sunos5-pthread sunos5-gcc-thread sunos5-gcc-pthread \
sunos5-x11r6-gcc-sl ultrix-gcc unicos unixware vistra:
	-mkdir lib
	touch src/depend
	touch src-glu/depend
	cd src ; $(MAKE) $@
	cd src-glu ; $(MAKE) $@

amiwin:
	mklib.amiwin

beos beos-glide:
	-mkdir lib
	touch src/depend
	touch src-glu/depend
	cd src ; $(MAKE) -f Makefile.BeOS $@
	cd src-glu ; $(MAKE) -f Makefile.BeOS $@

cygnus cygnus-linux:
	-mkdir lib
	touch src/depend
	touch src-glu/depend
	cd src ; $(MAKE) $@
	cd src-glu ; $(MAKE) $@

macintosh:
	@echo "See the README file for Macintosh intallation information"

mswindows:
	@echo "See the windows/README file for installation information"

next:
	-mkdir lib
	cd src ; $(MAKE) -f Makefile.NeXT $@
	cd src-glu ; $(MAKE) -f Makefile $@
	cd NeXT ; $(MAKE) -f Makefile.NeXT $@

openstep:
	-mkdir lib
	cd src ; $(MAKE) -f Makefile.OpenStep $@
	cd src-glu ; $(MAKE) -f Makefile.OpenStep $@
	cd OpenStep; $(MAKE)

os2:
	@echo "See the README.OS2 file for instructions"



# Remove .o files, emacs backup files, etc.
clean:
	-rm -f include/*~
	-rm -f include/GL/*~
	-rm -f src/*.o src/*~ src/*.a
	-rm -f src-glu/*.o src-glu/*~ src-glu/*.a
	-rm -f demos/*.o


MESA_HOME = Mesa-2.6
MESA_NAME = Mesa-2.6

TAR_FILES =	\
	$(MESA_HOME)/README*			\
	$(MESA_HOME)/IAFA-PACKAGE		\
	$(MESA_HOME)/LICENSE			\
	$(MESA_HOME)/VERSIONS			\
	$(MESA_HOME)/RELNOTES			\
	$(MESA_HOME)/FUTURE			\
	$(MESA_HOME)/Makefile*			\
	$(MESA_HOME)/Make-config		\
	$(MESA_HOME)/mklib.*			\
	$(MESA_HOME)/*.BAT			\
	$(MESA_HOME)/Imakefile			\
	$(MESA_HOME)/descrip.mms		\
	$(MESA_HOME)/mms-config			\
	$(MESA_HOME)/xlib.opt			\
	$(MESA_HOME)/STARTUP.MK			\
	$(MESA_HOME)/mesawin32.mak		\
	$(MESA_HOME)/Names.win			\
	$(MESA_HOME)/include/*.h		\
	$(MESA_HOME)/include/GL/*.h		\
	$(MESA_HOME)/src/Makefile*		\
	$(MESA_HOME)/src/Imakefile		\
	$(MESA_HOME)/src/descrip.mms		\
	$(MESA_HOME)/src/mms_depend		\
	$(MESA_HOME)/src/DEPEND.DOS		\
	$(MESA_HOME)/src/*.def			\
	$(MESA_HOME)/src/*.mak			\
	$(MESA_HOME)/src/*.rc			\
	$(MESA_HOME)/src/*.rsp			\
	$(MESA_HOME)/src/depend			\
	$(MESA_HOME)/src/*.[chS]		\
	$(MESA_HOME)/src-glu/README[12]		\
	$(MESA_HOME)/src-glu/Makefile*		\
	$(MESA_HOME)/src-glu/Imakefile		\
	$(MESA_HOME)/src-glu/descrip.mms	\
	$(MESA_HOME)/src-glu/mms_depend		\
	$(MESA_HOME)/src-glu/*.def		\
	$(MESA_HOME)/src-glu/*.rsp		\
	$(MESA_HOME)/src-glu/depend		\
	$(MESA_HOME)/src-glu/*.[ch]		\
	$(MESA_HOME)/NeXT			\
	$(MESA_HOME)/BeOS			\
	$(MESA_HOME)/OpenStep			\
	$(MESA_HOME)/WIN32



tar:
	cd .. ; \
	tar -cvf $(MESA_NAME).tar $(TAR_FILES) ; \
	gzip $(MESA_NAME).tar ; \
	mv $(MESA_NAME).tar.gz $(DIRECTORY)

