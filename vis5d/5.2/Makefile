# makefile for Vis5D version 5.2 

# Vis5D system for visualizing five dimensional gridded data sets
# Copyright (C) 1990 - 1997 Bill Hibbard, Brian Paul, Dave Santek,
# and Andre Battaiola.
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 1, or (at your option)
# any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


# Notes on IRIX compiler flags
#    IRIX 5.x only supports 32-bit architecture, -mips2 option not used by
#        Vis5D but could squeeze out a little extra performance.
#    IRIX 6.x supports 32 and 64-bit architectures, works only on R4000 or
#        later.  32-bit addressing is enough for Vis5D (-n32 mode).
#        Use -mips3 on all R4x00 variants.  Use -mips4 on R8000, R5000
#        and R10000.


default:
	@echo "Type one of the following:"
	@echo "***Note*** In order to compile with Mcidas on an SGI add"
	@echo "           '-mcidas' after the make name. (Ex. make irix5-opengl-mcidas)"
#	@echo "	make irix4		for SGIs with IRIX 4.x"
#	@echo "	make irix5		for SGIs with IRIX 5.x using IRIS GL"
	@echo "	make irix5-opengl	for SGIs with IRIX 5.x using OpenGL"
	@echo "	make irix5-x		for SGIs with IRIX 5.x using X"
	@echo "	make irix6-x		for SGIs with IRIX 6.x using X"
	@echo "	make irix6.0.1		for SGIs with R8000 and IRIX 6.0.1"
	@echo "	make irix6		for SGIs with IRIX 6.x"
	@echo "	make irix6-n32-mips3	for SGIs with IRIX 6.x and R4K or higher, 32bit objs"
	@echo "	make irix6-64-mips4	for SGIs with IRIX 6.x and R5K or higher, 64bit objs"
	@echo "	make irix6-64-mips4-x	for SGIs with IRIX 6.x and R5K or higher and x, 64bit objs"
#	@echo "	make alpha-denali	for Alpha with Denali graphics"
	@echo "	make alpha-x		for Alpha with X"
	@echo "	make dec-opengl		for DEC workstations with OpenGL"
	@echo "	make freebsd-x		for FreeBSD with X"
	@echo "	make hp-pex		for HP with PEX"
	@echo "	make hp10-pex		for HP with PEX and HP-UX 10.x"
	@echo "	make hp-opengl		for HP with OpenGL"
	@echo "	make hp-x		for HP with X"
	@echo "	make ibm-opengl		for IBM RS/6000 with OpenGL"
	@echo "	make ibm-x		for IBM RS/6000 with X"
	@echo "	make linux-ppc		for Linux on the PowerPC"
	@echo "	make linux-opengl	for Linux 1.2.x with OpenGL"
	@echo "	make linux-x		for Linux 1.2.x with X"
	@echo "	make linux-threads-x	for Linux with multiple threads and X"
#	@echo "	make sunos4-x		for SunOS 4.x with X"
	@echo "	make sunos5-x		for SunOS 5.x with X"
	@echo "	make sunos5-opengl	for SunOS 5.x with OpenGL"
	@echo "	make sunos5-ultra	for SunOS 5.x with OpenGL on an Ultra"
	@echo "	make sunos5i-x		for Solaris on Intel"
	@echo "	make clean		to remove unneeded files when finished"


# For debugging on SGI:
debug:
	cd lui5 ; $(MAKE) debug
	cd src ; $(MAKE) debug
	cd util ; $(MAKE) debug
	cd userfuncs ; ./externf_all

irix4:
	cd lui5 ; $(MAKE) irix4
	cd src ; $(MAKE) irix4
	cd util ; $(MAKE) irix4
	cd userfuncs ; ./externf_all
	cd import ; $(MAKE) irix4

irix5:
	cd lui5 ; $(MAKE) irix5
	cd src ; $(MAKE) irix5
	cd util ; $(MAKE) irix5
	cd userfuncs ; ./externf_all
	cd import ; $(MAKE) irix5

irix6.0.1:
	cd lui5 ; $(MAKE) irix6.0.1
	cd src ; $(MAKE) irix6.0.1
	cd util ; $(MAKE) irix6.0.1
	cd import ; $(MAKE) irix6.0.1
	cd userfuncs ; ./externf_all

irix6-x:
	cd lui5 ; $(MAKE) irix6.0.1
	cd Mesa ; $(MAKE) irix6-32
	cd src ; $(MAKE) irix6-x
	cd util ; $(MAKE) irix6.0.1
	cd import ; $(MAKE) irix6.0.1
	cd userfuncs ; ./externf_all

irix6:
	cd lui5 ; $(MAKE) irix6
	cd src ; $(MAKE) irix6
	cd util ; $(MAKE) irix6
	cd import ; $(MAKE) irix6
	cd userfuncs ; ./externf_all

irix6-n32-mips3:
	cd lui5 ; $(MAKE) irix6-n32-mips3
	cd src ; $(MAKE) irix6-n32-mips3
	cd util ; $(MAKE) irix6-n32-mips3
	cd import ; $(MAKE) irix6-n32-mips3
	cd userfuncs ; ./externf_all

irix6-64-mips4:
	cd lui5 ; $(MAKE) irix6-64-mips4
	cd src ; $(MAKE) irix6-64-mips4
	cd util ; $(MAKE) irix6-64-mips4
	cd import ; $(MAKE) irix6-64-mips4
	cd userfuncs ; ./externf_all

irix6-64-mips4-x:
	cd lui5 ; $(MAKE) irix6-64-mips4
	cd Mesa ; $(MAKE) irix6-64
	cd src ; $(MAKE) irix6-64-mips4-mesa
	cd util ; $(MAKE) irix6-64-mips4
	cd import ; $(MAKE) irix6-64-mips4
	cd userfuncs ; ./externf_all


irix5-opengl:
	cd lui5 ; $(MAKE) irix5
	cd src ; $(MAKE) irix5-opengl
	cd util ; $(MAKE) irix5
	cd userfuncs ; ./externf_all
	cd import ; $(MAKE) irix5-opengl

irix5-x:
	cd lui5 ; $(MAKE) irix5
	cd Mesa ; $(MAKE) irix5
	cd src ; $(MAKE) irix5-mesa
	cd util ; $(MAKE) irix5
	cd userfuncs ; ./externf_all
	cd import ; $(MAKE) irix5


irix6.0.1-mcidas:
	cd lui5 ; $(MAKE) irix6.0.1
	cd src ; $(MAKE) irix6.0.1-mcidas
	cd util ; $(MAKE) irix6.0.1
	cd import ; $(MAKE) irix6.0.1-mcidas
	cd userfuncs ; ./externf_all

irix6-x-mcidas:
	cd lui5 ; $(MAKE) irix6.0.1
	cd Mesa ; $(MAKE) irix6-32
	cd src ; $(MAKE) irix6-x-mcidas
	cd util ; $(MAKE) irix6.0.1
	cd import ; $(MAKE) irix6.0.1-mcidas
	cd userfuncs ; ./externf_all

irix6-mcidas:
	cd lui5 ; $(MAKE) irix6
	cd src ; $(MAKE) irix6-mcidas
	cd util ; $(MAKE) irix6
	cd import ; $(MAKE) irix6-mcidas
	cd userfuncs ; ./externf_all

irix6-n32-mips3-mcidas:
	cd lui5 ; $(MAKE) irix6-n32-mips3
	cd src ; $(MAKE) irix6-n32-mips3-mcidas
	cd util ; $(MAKE) irix6-n32-mips3
	cd import ; $(MAKE) irix6-n32-mips3-mcidas
	cd userfuncs ; ./externf_all

irix6-64-mips4-mcidas:
	cd lui5 ; $(MAKE) irix6-64-mips4
	cd src ; $(MAKE) irix6-64-mips4-mcidas
	cd util ; $(MAKE) irix6-64-mips4
	cd import ; $(MAKE) irix6-64-mips4-mcidas
	cd userfuncs ; ./externf_all

irix6-64-mips4-x-mcidas:
	cd lui5 ; $(MAKE) irix6-64-mips4
	cd Mesa ; $(MAKE) irix6-64
	cd src ; $(MAKE) irix6-64-mips4-mesa-mcidas
	cd util ; $(MAKE) irix6-64-mips4
	cd import ; $(MAKE) irix6-64-mips4-mcidas
	cd userfuncs ; ./externf_all


irix5-opengl-mcidas:
	cd lui5 ; $(MAKE) irix5
	cd src ; $(MAKE) irix5-opengl-mcidas
	cd util ; $(MAKE) irix5
	cd userfuncs ; ./externf_all
	cd import ; $(MAKE) irix5-opengl-mcidas

irix5-x-mcidas:
	cd lui5 ; $(MAKE) irix5
	cd Mesa ; $(MAKE) irix5
	cd src ; $(MAKE) irix5-mesa-mcidas
	cd util ; $(MAKE) irix5
	cd userfuncs ; ./externf_all
	cd import ; $(MAKE) irix5-mcidas

alpha-denali:
	cd lui5 ; $(MAKE) alpha
	cd src ; $(MAKE) alpha-denali
	cd util ; $(MAKE) alpha
	cd userfuncs ; ./externf_all
	cd import ; $(MAKE) dec-alpha

alpha-x:
	cd lui5 ; $(MAKE) alpha
	cd Mesa ; $(MAKE) osf1
	cd src ; $(MAKE) alpha-mesa
	cd util ; $(MAKE) alpha
	cd userfuncs ; ./externf_all
	cd import ; $(MAKE) dec-alpha

dec-opengl:
	cd lui5 ; $(MAKE) alpha
	cd src ; $(MAKE) dec-opengl
	cd util ; $(MAKE) alpha
	cd userfuncs ; ./externf_all
	cd import ; $(MAKE) dec-alpha

freebsd-x:
	cd lui5 ; $(MAKE) freebsd
	cd Mesa ; $(MAKE) freebsd
	cd src ; $(MAKE) freebsd-mesa
	cd util ; $(MAKE) freebsd
	cd import ; $(MAKE) freebsd

ibm-opengl:
	cd lui5 ; $(MAKE) ibm
	cd src ; $(MAKE) ibm-opengl
	cd util ; $(MAKE) ibm
	cd userfuncs ; ./externf_all
	cd import ; $(MAKE) ibm

ibm-x:
	cd lui5 ; $(MAKE) ibm
	cd Mesa ; $(MAKE) aix
	cd src ; $(MAKE) ibm-mesa
	cd util ; $(MAKE) ibm
	cd userfuncs ; ./externf_all
	cd import ; $(MAKE) ibm

hp-pex:
	cd lui5 ; $(MAKE) hp
	cd src ; $(MAKE) hp-pex
	cd util ; $(MAKE) hp
	cd userfuncs ; ./externf_all
	cd import ; $(MAKE) hp

hp10-pex:
	cd lui5 ; $(MAKE) hp
	cd src ; $(MAKE) hp10-pex
	cd util ; $(MAKE) hp
	cd userfuncs ; ./externf_all
	cd import ; $(MAKE) hp

hp-opengl:
	cd lui5 ; $(MAKE) hp
	cd src ; $(MAKE) hp-opengl
	cd util ; $(MAKE) hp
	cd userfuncs ; ./externf_all
	cd import ; $(MAKE) hp

hp-x:
	cd lui5 ; $(MAKE) hp
	cd Mesa ; $(MAKE) hpux9
	cd src ; $(MAKE) hp-mesa
	cd util ; $(MAKE) hp
	cd userfuncs ; ./externf_all
	cd import ; $(MAKE) hp

linux-ppc:
	cd lui5 ; $(MAKE) linux-ppc
	cd Mesa ; $(MAKE) linux-ppc
	cd src ; $(MAKE) linux-ppc-mesa
	cd util ; $(MAKE) linux-ppc
	cd import ; $(MAKE) linux-ppc

linux-opengl:
	cd lui5 ; $(MAKE) linux
	cd src ; $(MAKE) linux-opengl
	cd util ; $(MAKE) linux
	cd import ; $(MAKE) linux

linux-threads-x:
	cd lui5 ; $(MAKE) linux-threads
	cd Mesa ; $(MAKE) linux-386-pthread
	cd src ; $(MAKE) linux-threads-mesa
	cd util ; $(MAKE) linux-threads
	cd import ; $(MAKE) linux-threads

linux-x:
	cd lui5 ; $(MAKE) linux
	cd Mesa ; $(MAKE) linux
	cd src ; $(MAKE) linux-mesa
	cd util ; $(MAKE) linux
	cd import ; $(MAKE) linux

sunos4-x:
	cd lui5 ; $(MAKE) sunos4
	cd Mesa ; $(MAKE) sunos4
	cd src ; $(MAKE) sunos4-mesa
	cd util ; $(MAKE) sunos4
	cd userfuncs ; ./externf_all
	cd import ; $(MAKE) sunos4

sunos5-x:
	cd lui5 ; $(MAKE) sunos5
	cd Mesa ; $(MAKE) sunos5
	cd src ; $(MAKE) sunos5-mesa
	cd util ; $(MAKE) sunos5
	cd userfuncs ; ./externf_all
	cd import ; $(MAKE) sunos5

sunos5-opengl:
	cd lui5 ; $(MAKE) sunos5
	cd src ; $(MAKE) sunos5-opengl
	cd util ; $(MAKE) sunos5
	cd userfuncs ; ./externf_all
	cd import ; $(MAKE) sunos5

sunos5-opengl-gcc:
	cd lui5 ; $(MAKE) sunos5-gcc
	cd src ; $(MAKE) sunos5-opengl-gcc
	cd util ; $(MAKE) sunos5-gcc
	cd userfuncs ; ./externf_all
	cd import ; $(MAKE) sunos5-gcc

sunos5-ultra:
	cd lui5 ; $(MAKE) sunos5-ultra
	cd src ; $(MAKE) sunos5-opengl-ultra
	cd util ; $(MAKE) sunos5-ultra
	cd userfuncs ; ./externf_all
	cd import ; $(MAKE) sunos5-ultra

sunos5i-x:
	cd lui5 ; $(MAKE) sunos5
	cd Mesa ; $(MAKE) sunos5
	cd src ; $(MAKE) sunos5i-mesa
	cd util ; $(MAKE) sunos5i
	cd userfuncs ; ./externf_all
	cd import ; $(MAKE) sunos5i

foo:
	cd lui5 ; $(MAKE) foo
	cd src ; $(MAKE) foo
	cd util ; $(MAKE) foo
	cd userfuncs ; ./externf_all


######################################################################

#
# Miscellaneous makes:
#

VDIR = vis5d-5.2


# Make the Vis5D source tar file

TAR_FILES = \
	$(VDIR)/README			\
	$(VDIR)/README.ps		\
	$(VDIR)/NOTICE			\
	$(VDIR)/PORTING			\
	$(VDIR)/Makefile		\
	$(VDIR)/listfonts		\
	$(VDIR)/*.tcl			\
	$(VDIR)/lui5/Makefile		\
	$(VDIR)/lui5/*.[ch]		\
	$(VDIR)/src/Makefile		\
	$(VDIR)/src/*.[chf]		\
	$(VDIR)/src/*[45].a		\
	$(VDIR)/util/Makefile		\
	$(VDIR)/util/*.[chfm]		\
	$(VDIR)/convert/*.[cfhm]	\
	$(VDIR)/userfuncs/README	\
	$(VDIR)/userfuncs/externf	\
	$(VDIR)/userfuncs/externf.m	\
	$(VDIR)/userfuncs/externf_all	\
	$(VDIR)/userfuncs/*.f		\
	$(VDIR)/Mesa/README		\
	$(VDIR)/Mesa/Makefile		\
	$(VDIR)/Mesa/Make-config	\
        $(VDIR)/Mesa/mklib*.*		\
	$(VDIR)/Mesa/src/*.[chS]	\
	$(VDIR)/Mesa/src/depend		\
	$(VDIR)/Mesa/src/Makefile	\
	$(VDIR)/Mesa/src-glu/*.[chS]	\
	$(VDIR)/Mesa/src-glu/depend	\
	$(VDIR)/Mesa/src-glu/Makefile	\
	$(VDIR)/Mesa/include		\
	$(VDIR)/import/README		\
	$(VDIR)/import/Makefile		\
	$(VDIR)/import/*.[chf]		\
	$(VDIR)/contrib

MCIDAS_TAR_FILES = \
	$(VDIR)/src/libmcidas*.a        

USER_DATA_TAR_FILES = \
	$(VDIR)/user_data/*


#
# add to TAR_FILE to make merged file for shows / demos:
#
#	$(VDIR)/vis5d			\
#	$(VDIR)/vis5d_opengl		\
#	$(VDIR)/EARTH.TOPO		\
#	$(VDIR)/OUTLUSAM		\
#	$(VDIR)/OUTLSUPW		\
#	$(VDIR)/SCHL.v5d		\
#	$(VDIR)/LAMPS.v5d		\
#

# v5dimport source files:
V5DIMPORT_FILES = \
	$(VDIR)/import/Makefile		\
	$(VDIR)/import/README		\
	$(VDIR)/import/*.[chf]


tar:
	cd src ; \
	cd ../.. ; \
	tar -cvf vis5d-5.2.tar $(TAR_FILES) ; \
	compress vis5d-5.2.tar ; \
	mv vis5d-5.2.tar.Z $(VDIR)

import_tar:
	cd .. ; \
	tar -cvf v5dimport.tar $(V5DIMPORT_FILES) ; \
	compress v5dimport.tar ; \
	mv v5dimport.tar.Z $(VDIR)

libmcidas_tar:
	cd src ; \
	cd ../.. ; \
	tar -cvdLf vis5d-libmcidas.tar $(MCIDAS_TAR_FILES) ; \
	compress vis5d-libmcidas.tar ; \
	mv vis5d-libmcidas.tar.Z $(VDIR)

user_data_tar:
	cd src ; \
	cd ../.. ; \
	tar -cvdLf vis5d-userdata.tar $(USER_DATA_TAR_FILES) ; \
	compress vis5d-userdata.tar ; \
	mv vis5d-userdata.tar.Z $(VDIR)

# Make the Vis5D Data tar file

DATA_FILES = \
	OUTLUSAM	\
	OUTLSUPW	\
	EARTH.TOPO	\
	SCHL.v5d	\
	LAMPS.v5d
datatar:
	tar -cvdLf vis5d-data.tar $(DATA_FILES) ; \
	compress vis5d-data.tar

IREG_DATA_FILES = \
	irregular_data/*.nc \
	irregular_data/*.metar \
	irregular_data/*.profiler \
	irregular_data/ngm_99111612.v5d

irregular_datatar:
	tar -cvdLf vis5d-irregular-data.tar $(IREG_DATA_FILES) ; \
	compress vis5d-irregular-data.tar


# Make tar file of executables for certain systems

BINARIES = \
	$(VDIR)/comp_to_v5d	\
	$(VDIR)/gr3d_to_v5d	\
	$(VDIR)/gg3d		\
	$(VDIR)/igg3d		\
	$(VDIR)/igu3d		\
	$(VDIR)/vis5d		\
	$(VDIR)/v5dinfo		\
	$(VDIR)/v5dstats	\
	$(VDIR)/v5dedit		\
	$(VDIR)/v5dappend	\
	$(VDIR)/v5dimport


# Make tar file of SGI IRIX 5.x binaries:
irix5_bin_tar:
	cd .. ; \
	tar -cvf vis5d.irix5.bin.tar $(BINARIES) $(VDIR)/listfonts ; \
	compress vis5d.irix5.bin.tar ; \
	mv vis5d.irix5.bin.tar.Z $(VDIR)


# Make tar file of IBM AIX binaries:
ibm_bin_tar:
	cd .. ; \
	tar -cvf vis5d.ibm.bin.tar $(BINARIES) ; \
	compress vis5d.ibm.bin.tar ; \
	mv vis5d.ibm.bin.tar.Z $(VDIR)


# Make tar file of HP HP-UX binaries:
hp_bin_tar:
	cd .. ; \
	tar -cvf vis5d.hp.bin.tar $(BINARIES) ; \
	compress vis5d.hp.bin.tar ; \
	mv vis5d.hp.bin.tar.Z $(VDIR)


# Make tar file of Alpha OSF/1 binaries:
alpha_bin_tar:
	cd .. ; \
	tar -cvf vis5d.alpha.bin.tar $(BINARIES) ; \
	compress vis5d.alpha.bin.tar ; \
	mv vis5d.alpha.bin.tar.Z $(VDIR)


# Make tar file of SunOS 5.x binaries:
sunos5_bin_tar:
	cd .. ; \
	tar -cvf vis5d.sunos5.bin.tar $(BINARIES) ; \
	compress vis5d.sunos5.bin.tar ; \
	mv vis5d.sunos5.bin.tar.Z $(VDIR)


# Make tar file of PC Linux binaries:
linux_bin_tar:
	cd .. ; \
	tar -cvf vis5d.linux.bin.tar $(BINARIES) ; \
	compress vis5d.linux.bin.tar ; \
	mv vis5d.linux.bin.tar.Z $(VDIR)



# Remove all .o files and Emacs backup files:
clean:
	cd lui5 ; $(MAKE) clean
	cd src ; $(MAKE) clean
	cd util ; $(MAKE) clean
	cd import ; $(MAKE) clean
	-rm userfuncs/*.o


# Remove everything that can be reproduced with make
really_clean:
	cd lui5 ; $(MAKE) clean
	cd src ; $(MAKE) clean
	cd util ; $(MAKE) clean
	cd Mesa ; $(MAKE) clean
	-rm -rf Mesa/lib
	-rm lui5/liblui.a util/libmain.a
	-rm $(BINARIES) listfonts


# Make backup of vis5d-5.2 directory tree
SAVE_DIR = /usr6/save/vis5d-5.2
save:
	cp Makefile		$(SAVE_DIR)
	cp lui5/Makefile	$(SAVE_DIR)/lui5
	cp lui5/*.[ch]		$(SAVE_DIR)/lui5
	cp src/Makefile		$(SAVE_DIR)/src
	cp src/*.[ch]		$(SAVE_DIR)/src
	cp util/Makefile	$(SAVE_DIR)/util
	cp util/*.[chfF]	$(SAVE_DIR)/util
#	cp import/*.[chf]	$(SAVE_DIR)/import
	cp Mesa/LICENSE		$(SAVE_DIR)/Mesa
	cp Mesa/Make*		$(SAVE_DIR)/Mesa
	cp Mesa/README		$(SAVE_DIR)/Mesa
	cp Mesa/mklib*		$(SAVE_DIR)/Mesa
	cp -r Mesa/include	$(SAVE_DIR)/Mesa
	cp Mesa/src/Makefile	$(SAVE_DIR)/Mesa/src
	cp Mesa/src/depend	$(SAVE_DIR)/Mesa/src
	cp Mesa/src/*.[ch]		$(SAVE_DIR)/Mesa/src
	cp Mesa/src-glu/Makefile	$(SAVE_DIR)/Mesa/src-glu
	cp Mesa/src-glu/depend		$(SAVE_DIR)/Mesa/src-glu
	cp Mesa/src-glu/*.[ch]		$(SAVE_DIR)/Mesa/src-glu
	cp import/Makefile	$(SAVE_DIR)/import
	cp import/*.[ch]	$(SAVE_DIR)/import

