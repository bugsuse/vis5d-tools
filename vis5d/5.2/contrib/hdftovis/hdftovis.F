C             to vis5d compressed files.  
c
c A modification of COMP5D
c
c Karen A. Butler, New Technology INC./ES44, Marshall Space Flight Center,AL
C
C ? NAME
C ?    hdftovis - create a compressed grid file
C ?
C ? SYNOPSIS
C ?    hdftovis (hdf file)  (compressed file)
C ?
C ?
C ? EXAMPLE
c ?    hdftovis Geosim.hdf CompGeosim
c
c The following HDF calls are needed in the program which
c generates the HDF file:
c
c DFSDsetdims - sets the dimensions (grid sizes): example: 20 by 20 by 15
c DFSDsetdimstrs - sets the labels for the dimensions: example x,y,z or
c                   lat,long,hgt.
c                   note: a call is needed for each dimension
c DFSDsetdatastrs - sets the labels for each of the parameters: example temp
c                   note: a call is needed for each parameter
c DFSDputdata or DFSDadddata - writes the actual grid or sds(scientific dataset)
c
C
      SUBROUTINE MAIN0
      include "../../util/vis5d.h"
      character*50 CFILE,CPP,hdfile
C
      COMMON/JTIME/NTIMES,NPARMS,MR,MC,ML,
     *XLATN,XLONW,XHGTT,XLATIN,XLONIN,XHGTIN,
     *JDAY(NTIME),JTIME(NTIME),JPARM(NPARM)
      common /hdf/ishape(3),irank,hdfile,cfile,label(3),ipos(3),idi(3)
      integer myacnt,argcnt
      character*50 mycarg
      character*50 argstring
      character*8 label
C
C
      myacnt=argcnt()
      call gethdf
      MRMCML=MR*MC*ML
      CALL LDEST('MRMCML ',MRMCML)
      CALL COMPAL(MRMCML)
      RETURN
      END

C
      subroutine gethdf  
      integer DFSDgetdims,DFSDgetdata,DFSDgetdatastrs,dfsdgetdimscales
      integer DFSDgetdimstrs
      integer DFSDnumber
      include "../../util/vis5d.h"
      common /hdf/ishape(3),irank,hdfile,cfile,label(3),ipos(3),idi(3)
      character*50 hdfile,cfile,cpp
      integer myacnt,argcnt
c
      character*50 format,coor,lunit(3),datalab
      COMMON/JTIME/NTIMES,NPARMS,MR,MC,ML,
     *   XLATN,XLONW,XHGTT,XLATIN,XLONIN,XHGTIN,
     *   JDAY(NTIME),JTIME(NTIME),JPARM(NPARM)
C
      character*8 label
      real tmp(500) 
      character*50 argstring
C LATITUDE, LONGITUDE AND HEIGHT BOUNDS FOR SPATIAL GRID
      DATA XLATS/20.0/,XLATN/50.0/
      DATA XLONE/70.0/,XLONW/120.0/
      DATA XHGTB/0.0/,XHGTT/15.0/
C

C
        myacnt=argcnt()
	if(myacnt.lt.3) then
	  write(6,*) ' usage:   hdftovis <hdf file> <comp file>'
	  write(6,*) ' '
	  write(6,*) '    where:'
	  write(6,*) ' '
	  write(6,*) '     <hdf file> is hdf input filename'
	  write(6,*) '     <comp file> is vis5d compressed output filename'
	  write(6,*) ' '
	  write(6,*) ' '
	  stop
         endif

        hdfile = argstring(1)
        cfile = argstring(2)
C
      iret=DFSDgetdims(hdfile,irank,ishape,3)
      if(iret.ne.0) then
	write(6,*) ' problem opening file: ',hdfile
	stop
      endif

      ipos(1)=1
      ipos(2)=2
      ipos(3)=3
       
      iret=DFSDgetdimstrs(1,label(1),lunit(1),format)
      if(label(1).eq.' ') then
	  write(6,*) ' warning: no 1st dimension given'     
	  label(1)='x'
      endif
      iret=DFSDgetdimstrs(2,label(2),lunit(2),format)
      if(label(2).eq.' ') then
          write(6,*) ' warning: no 2nd dimension given'     
	  label(2)='y'
      endif
      iret=DFSDgetdimstrs(3,label(3),lunit(3),format)
      if(label(3).eq.' ') then
          write(6,*) ' warning: no 3rd dimension given'     
	  label(3)='z'
      endif

      do 10 i=1,3
        if(label(i).eq.'x'.or.label(i).eq.'lon'
     1		    .or.label(i).eq.'X'.or.label(i).eq.'LON'
     2              .or.label(i).eq.'Lon') then
		   mc=ishape(i)
		   ipos(1)=i
		   idi(i)=mc
		   iret=dfsdgetdimscale(i,mc,tmp)
		   if(iret.eq.0) then
c			if(tmp(mc).gt.tmp(1))then
			 xlonw=tmp(mc)
			 xlone=tmp(1)
c                        endif
		   else
		      write(6,*) ' no lon bounds given'
                   endif
	  elseif(label(i).eq.'y'.or.label(i).eq.'lat'.or.
     1		  label(i).eq.'Y'.or.label(i).eq.'LAT'
     2              .or.label(i).eq.'Lat') then
		   mr=ishape(i)
		   ipos(2)=i
		   idi(i)=mr
		   iret=dfsdgetdimscale(i,mr,tmp)
		   if(iret.eq.0) then
c			if(tmp(mr).gt.tmp(1))then
			 xlats=tmp(1)
			 xlatn=tmp(mr)
c                        endif
		   else
		      write(6,*) ' no lat bounds given'
                   endif
	  elseif(label(i).eq.'z'.or.label(i).eq.'hgt'.or.
     1		    label(i).eq.'Z'.or.label(i).eq.'HGT'
     2              .or.label(i).eq.'Hgt') then
		   ml=ishape(i)
		   ipos(3)=i
		   idi(i)=ml
		   iret=dfsdgetdimscale(i,ml,tmp)
		   if(iret.eq.0) then
c			if(tmp(mr).gt.tmp(1))then
			 xhgtb=tmp(1)
			 xhgtt=tmp(ml)
c                        endif
		   else
		      write(6,*) ' no hgt bounds given'
                   endif
          endif
10      continue
       
      nvars=nparms

      if(xlatn.eq.0) xlatn=50.0
      if(xlonw.eq.0) xlonw=120.0
      if(xhgtt.eq.0) xhgtt=15.0

      XLATIN=(XLATN-XLATS)/(mr-1)
      XLONIN=(XLONW-XLONE)/(mc-1)
      XHGTIN=(XHGTT-XHGTB)/(ml-1)

      return 

      end
C
C
C
      SUBROUTINE COMPRS(GRIDK,GRIDL,IGRID)
      include "../../util/vis5d.h"
C     Parameters
      integer DFSDgetdims,DFSDgetdata,DFSDgetdatastrs
      integer DFSDgetdimstrs
      integer DFSDnumber,DFSDrestart
      REAL*4 GRIDK(*),GRIDL(*)
      INTEGER*1 IGRID(*)

C     Common Blocks
      COMMON/JTIME/NTIMES,NPARMS,MR,MC,ML,
     *  XLATN,XLONW,XHGTT,XLATIN,XLONIN,XHGTIN,
     *  JDAY(NTIME),JTIME(NTIME),JPARM(NPARM)

C     Local Variables
c hdf flag
c
      common /hdf/ishape(3),irank,hdfile,cfile,label(3),ipos(3),idi(3)
      character*50 CFILE,CPP,CKWP,hdfile
      REAL*4 GA(1000), GB(1000)
      REAL*4 HEIGHT(1000)
      REAL*4 VALMIN(NPARM), VALMAX(NPARM)
      REAL*4 NORTHLAT(NTIME), WESTLON(NTIME)
      INTEGER MRMCML
      integer ipos(3),idi(3)
      character*50 format,coor,parm,datalab
      INTEGER*4 ID
      character*8 label
      REAL*4 GMIN, GMAX


      MRMCML=MR*MC*ML

C     Initialize VALMIN and VALMAX arrays
      DO 5 I=1,NPARM
         VALMIN(I) = 1.0E35
         VALMAX(I) = -1.0E35
 5    CONTINUE

      call getparm(gridl)   
      iret=DFSDrestart(hdfile)
      num=DFSDnumber(hdfile)
      ntimes=(num/nparms)

C     Compute size of header in 4-byte words
      NHEAD = 8 + ML + 2*NPARMS + 2*NPARMS + 4*NTIME
C     Compute size of grid data in 4-byte words
      NWORD = (MRMCML+3)/4

      IWORD=1+NHEAD

C     LOOP FOR TIMES
      DO 60 IT=1,NTIMES
	 jtime(it)=it 
C        LOOP FOR PARAMS
         DO 50 IP=1,NPARMS

            iret=DFSDgetdata(hdfile,3,ishape,gridl)
            call switch (gridl,gridk,mr,mc,ml,ipos,idi)
            CALL COMPRESS_GRID( GA, GB, GMIN, GMAX, GRIDK, IGRID )
            IF (GMIN .LT. VALMIN(IP))  VALMIN(IP) = GMIN
            IF (GMAX .GT. VALMAX(IP))  VALMAX(IP) = GMAX

C   put in fake number for grids ( not necessary since hdf files)

            iifile = 1
	    iigrid = 1

c   write out data

            CALL LWO( CFILE, IWORD, 1, IIFILE )
            IWORD = IWORD + 1
            CALL LWO( CFILE, IWORD, 1, IIGRID )
            IWORD = IWORD + 1

c           Write GA, GB arrays and compressed data
            CALL LWO( CFILE, IWORD, ML, GA )
            IWORD = IWORD + ML
            CALL LWO( CFILE, IWORD, ML, GB )
            IWORD = IWORD + ML
            CALL LWO( CFILE, IWORD, NWORD, IGRID )
            IWORD = IWORD + NWORD

 50      CONTINUE

 60   CONTINUE

C     Fill in HEIGHT array
      DO 70 I=1,ML
         HEIGHT(ML-I+1) = XHGTT - XHGTIN * (I-1)
 70   CONTINUE

C     Fill in NORTHLAT and WESTLON arrays
      DO 80 I=1,NTIMES
         NORTHLAT(I) = XLATN
         WESTLON(I) = XLONW
 80   CONTINUE

C     Now write the file header
c     write 0x80808083 ID indicating compression by layer w/ McIDAS numbers
      ID = '80808083'X
      CALL LWO(CFILE, 0, 1, ID)
      N = NTIME
      CALL LWO(CFILE, 1, 1, N)
      CALL LWO(CFILE, 2, 1, NPARMS)
      CALL LWO(CFILE, 3, 1, NTIMES)
      CALL LWO(CFILE, 4, 1, MR)
      CALL LWO(CFILE, 5, 1, MC)
      CALL LWO(CFILE, 6, 1, ML)
      CALL LWO(CFILE, 7, 1, XLATIN)
      CALL LWO(CFILE, 8, 1, XLONIN)
      IWORD = 9
      CALL LWO(CFILE, IWORD, ML, HEIGHT)
      IWORD = IWORD + ML
      N = 0
      DO 100, I=1,NPARMS
         CALL LWO(CFILE, IWORD, 1, JPARM(I))
         CALL LWO(CFILE, IWORD+1, 1, N )
         IWORD = IWORD + 2
 100  CONTINUE
      CALL LWO(CFILE, IWORD, NPARMS, VALMIN)
      IWORD = IWORD + NPARMS
      CALL LWO(CFILE, IWORD, NPARMS, VALMAX)
      IWORD = IWORD + NPARMS
      CALL LWO(CFILE, IWORD, NTIME, JTIME)
      IWORD = IWORD + NTIME
      CALL LWO(CFILE, IWORD, NTIME, JDAY)
      IWORD = IWORD + NTIME
      CALL LWO(CFILE, IWORD, NTIME, NORTHLAT)
      IWORD = IWORD + NTIME
      CALL LWO(CFILE, IWORD, NTIME, WESTLON)
      IWORD = IWORD + NTIME

C     All done!

      RETURN
      END
C


      SUBROUTINE COMPRESS_GRID( GA, GB, GRIDMIN, GRIDMAX, DATA, COMP )
C     Compress a 3-D grid from floats to 1-byte integers
C     Input:  DATA - 3-D array (MR,MC,ML) of grid data
C     Output:
C              GRIDMIN, GRIDMAX - smallest and largest values in DATA
C              COMP - 1-BYTE compressed data
C              GA, GB - decompression values

C     Parameters
      REAL*4 DATA(*)
      REAL*4 GRIDMIN, GRIDMAX
      REAL*4 GA(*)
      REAL*4 GB(*)
      INTEGER*1 COMP(*)

C     Common Blocks
      include "../../util/vis5d.h"
      COMMON/JTIME/NTIMES,NPARMS,MR,MC,ML,
     *  XLATN,XLONW,XHGTT,XLATIN,XLONIN,XHGTIN,
     *  JDAY(NTIME),JTIME(NTIME),JPARM(NPARM)

C     Local Variables
      REAL MIN, MAX
      REAL G, DELT, IVAL, DVAL, MVAL
      REAL D(1000), M(1000), LEVMIN(1000), LEVMAX(1000)

C     Constants
      DATA BIGVALUE / 1.0E35 /
      DATA SMALLVALUE / -1.0E35 /

      GRIDMIN = BIGVALUE
      GRIDMAX = SMALLVALUE
      J = 1
      DO 10 LEV=1,ML
C        Find min and max of level
         MAX=-1000000000.0
         MIN=1000000000.0
         DO 20 k=1,MR*MC
            G = DATA(J)
            IF (G .LT. 1.E30 .AND. G .LT. MIN)  MIN = G
            IF (G .LT. 1.E30 .AND. G .GT. MAX)  MAX = G
            J = J + 1
 20      CONTINUE
         IF (MIN .LT. GRIDMIN) GRIDMIN = MIN
         IF (MAX .GT. GRIDMAX) GRIDMAX = MAX
         LEVMIN(LEV) = MIN
         LEVMAX(LEV) = MAX
 10   CONTINUE

C     If the grid minimum is within delt of 0.0, fudge all values
C     within delt of 0.0 to delt, and recalculate mins and maxes
      DELT = (GRIDMAX-GRIDMIN) / 100000.0
      IF ( ABS(GRIDMIN) .LT. DELT ) THEN
         DO 30 J = 1,MR*MC*ML
            IF (DATA(J) .LT. 1.E30 .AND. DATA(j) .LT. DELT) THEN
               DATA(J) = DELT
            ENDIF
 30      CONTINUE
C        Recalculate min and max for each layer and whole grid
         GRIDMIN = DELT
         DO 40 LEV=1,ML
            IF (ABS(LEVMIN(LEV)) .LT. DELT) LEVMIN(LEV) = DELT
            IF (ABS(LEVMAX(LEV)) .LT. DELT) LEVMAX(LEV) = DELT
 40      CONTINUE
      ENDIF

C     Find d[lev] and dmax = MAX( d(1), d(2), ... d(ML)
      DMAX = 0.0
      DO 50 LEV=1,ML
         IF (LEVMIN(LEV) .EQ. BIGVALUE
     +       .AND. LEVMAX(LEV) .EQ. SMALLVALUE) THEN
            D(LEV) = 0.0
         ELSE
            D(LEV) = LEVMAX(LEV)-LEVMIN(LEV)
         ENDIF
         IF (D(LEV) .GT. DMAX) DMAX = D(LEV)
 50   CONTINUE

      IF (DMAX .EQ. 0.0) THEN
         IF (GRIDMIN .EQ. GRIDMAX) THEN
C           whole grid is of same value
            IVAL = 1.0
         ELSE
C           every layer is of a single value
            IVAL = GRIDMAX-GRIDMIN
         ENDIF
      ELSE
         IVAL = DMAX / 253
      ENDIF
      MVAL = GRIDMIN

      DO 60 LEV=1,ML
         M(LEV) = MVAL + IVAL * JINT( (LEVMIN(LEV)-MVAL) / IVAL )
 60   CONTINUE

C     Now compress the data to values in [-128,126] 127=MISSING
      J = 0
      DO 70 LEV=1,ML
         DO 80 I=1,MR*MC
            IF (DATA(J) .GE. 1.E30) THEN
               COMP(J) = 127
            ELSE
               COMP(J) = JINT( (DATA(J)-M(LEV))/IVAL ) - 128
            ENDIF
            J = J + 1
 80      CONTINUE
 70   CONTINUE

C     compute ga, gb values for decompressing in VIS-5D
      DO 90 LEV=1,ML
         GA(LEV) = 1.0 / IVAL
         GB(LEV) = -128 - M(LEV) / IVAL
 90   CONTINUE

      RETURN
      END
      subroutine  switch (gridl,gridk,mr,mc,ml,ipos,id)
      dimension id(3),ipos(3)
      dimension gridk(mr,mc,ml),gridl(id(1),id(2),id(3))
      do 100 i=1,mc
	 i1=i
      do 100 j=1,mr
	 j1=j
      do 100 k=1,ml
	 k1=k
	 if(ipos(1).eq.1) j1=i
	 if(ipos(1).eq.3) k1=i
	 if(ipos(2).eq.2) i1=j
	 if(ipos(2).eq.3) k1=j
	 if(ipos(3).eq.1) j1=k
	 if(ipos(3).eq.2) i1=k
	 gridk(j,i,k)=gridl(j1,i1,k1)
100   continue

      return
      end

      subroutine getparm(gridl)
      include "../../util/vis5d.h"

      common /hdf/ishape(3),irank,hdfile,cfile,label(3),ipos(3),idi(3)
      integer DFSDgetdims,DFSDgetdata,DFSDgetdatastrs
      character*50 format,coor,datalab,tmpparm

      COMMON/JTIME/NTIMES,NPARMS,MR,MC,ML,
     *XLATN,XLONW,XHGTT,XLATIN,XLONIN,XHGTIN,
     *JDAY(NTIME),JTIME(NTIME),JPARM(NPARM)

      character*4 jparm,tmpparm2
      character*8 label
      equivalence (tmpparm2,tmpparm)
      character*50 CFILE,CPP,hdfile

      dimension gridl(*)

      nparms=0

      do 100 ip=1,NPARM

      iret=DFSDgetdata(hdfile,3,ishape,gridl)
       
      if(iret.ne.0) then
	if(nparms.eq.0) nparms=1
	return
      endif

      iret=DFSDgetdatastrs(tmpparm,datalab,format,coor)
       
       
      if(iret.ne.0) then
	write(6,*) ' error in getparm reading hdf datastrs'
	nparms=1
	return
      endif

      if(ip.gt.1)then
      do 150 j=1,nparms
	if (tmpparm2.eq.jparm(j)) then
	    return
        elseif (tmpparm2.eq.' ') then
	    if(nparms.eq.0) nparms=1
	    return
        endif
150   continue
      endif

      jparm(ip)=tmpparm2
      nparms=nparms+1

100   continue
      return
      end
