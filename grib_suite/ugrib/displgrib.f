      PROGRAM DISPLGRIB

comstart displgrib
cidx: Program to DISPLAY a GRIB file.
c
c Visualizza in forma estesa il contenuto di un grib file
c ed i primi venti dati con qualche minimo dato statistico.
c
c	    usage:
c	           displgrib <nomefile>
comend


C
C**** DISPLGRIB - Program to DISPLAY a GRIB file.
C
C
C     Method.
C     -------
C
C           Prints sections 0, 1, 2, 3 and 4 of GRIB message.
C
C     Externals.
C     ----------
C
C           GRIBEX
C           GRPRS0
C           GRPRS1
C           GRPRS2
C           GRPRS3
C           GRPRS4
C           PBOPEN
C           PBGRIB
C           PBCLOSE
C           SETPAR
C
C     Reference.
C     ----------
C
C           WMO Manual on Codes for GRIB definition.
C           WMO Publication No. 9, Volume B, for grid catalogue numbers.
C
C     Comments.
C     ---------
C
C           GRIBEX provides a number of packing/unpacking options.
C           See documentation in routine GRIBEX for details.
C
C     Author.
C     -------
C
C           Paolo Patruno    20/02/96   at S.M.R.
C
C     Modifications.
C     --------------
C
c           Paolo Patruno   11/10/99 at SMR
c           aumentata dimensione array
c
c
C
C     -----------------------------------------------------------------
C
C     Arrays are dimensioned to accommodate T213/N160 data volumes.
C
      PARAMETER (JPACK=400000)
C
C     Array for integer parameters from section 0 of GRIB message.
C
      DIMENSION ISEC0(2)
C
C     Array for integer parameters from section 1 of GRIB message.
C
      DIMENSION ISEC1(53+100)
C
C     Array for integer parameters from section 2 of GRIB message.
C
      DIMENSION ISEC2(512)
C
C     Array for integer parameters from section 3 of GRIB message.
C
      DIMENSION ISEC3(2)
C
C     Array for integer parameters from section 4 of GRIB message.
C
      DIMENSION ISEC4(42)
C
C     Array for real parameters from section 2 of GRIB message.
C
      DIMENSION ZSEC2(200)
C
C     Array for real parameters from section 3 of GRIB message.
C
      DIMENSION ZSEC3(2)
C
C     Array for real parameters from section 4 of GRIB message.
C     This is the binary data section and the array to hold
C     the unpacked data may need to be 4 times as long as that
C     for the packed data.
C
      DIMENSION ZSEC4(JPACK*4)
      DIMENSION IZSEC4(JPACK*4)
      EQUIVALENCE (ZSEC4,IZSEC4)
C
C     Array to read in packed data.
C
      DIMENSION INBUFF(JPACK)
C
C     GRIBEX routine has a number of different options.
C
      CHARACTER*1 YOPER
C
      CHARACTER*80 grib

      DATA RMIS/-1.5e21/
      DATA IMIS/32767/
C
C     Clear error counter.
C
      NUMERR = 0

	KPR=0	!DEBUG PRINT SWITCH
	CALL SETPAR (KBIT,KNEG,KPR)
C
C     Lengths of INBUFF and PSEC4
C
      ILENB = JPACK
      IPUNP = JPACK * 4
      KINLEN= JPACK*(KBIT/8)

	isec3(2)=imis		!missing value
	zsec3(2)=rmis		!missing value

	call getarg(1,grib)
	if (grib.eq.' ')then
            write(*,*)' displgrib version 1.2'
	    write(*,*)'usage:'
	    write(*,*)'displgrib <nomefile>'
	    write(*,*)' '
            goto 300
        end if

	call pbopen(kunit,grib,'r',kret)
	if(kret.ne.0)goto 300

   50 CONTINUE

	call pbgrib(kunit,inbuff,kinlen,isiz,kret)
	if (kret.eq.-1)goto 310
	if (kret.ne.0)goto 300

c	print*,isiz,' this is the lenght of grib read'

C     'D' function to unpack entire GRIB message.
C
      YOPER = 'D'
      WRITE (*,9000)
      WRITE (*,9001) YOPER
C
      IERR = 1
      CALL GRIBEX (ISEC0,ISEC1,ISEC2,ZSEC2,ISEC3,ZSEC3,ISEC4,
     C             ZSEC4,IPUNP,INBUFF,ILENB,IWORD,YOPER,IERR)
C
C     Check return code.
c
      WRITE (*,9004) IERR
      IF (IERR.EQ.-6) WRITE (*,*) ' GRDEMO : Pseudo-grib data found.'
      IF (IERR.GT.0)
     C   THEN
             NUMERR = NUMERR + 1
             GO TO 50
         ENDIF
C
C     Print section 0 , 1 , 2 and 3 (if present) and 4.
C     Section 1 is the product definition section.
C     Section 2 is the grid definition section.
C     Section 3 is the bit-map section.
C     Section 4 is the data section.
C
      CALL GRPRS0 (ISEC0)
      CALL GRPRS1 (ISEC0,ISEC1)
C
      IF (ISEC1(5).EQ.0.OR.ISEC1(5).EQ.64)
     C   THEN
             WRITE (*,9000)
             WRITE (*,*) ' GRDEMO : No section 2 in GRIB message.'
         ELSE
             CALL GRPRS2 (ISEC0,ISEC2,ZSEC2)
         ENDIF
C
      IF (ISEC1(5).EQ.0.OR.ISEC1(5).EQ.128)
     C   THEN
             WRITE (*,9000)
             WRITE (*,*) ' GRDEMO : No section 3 in GRIB message.'
         ELSE
             CALL GRPRS3 (ISEC0,ISEC3,ZSEC3)
         ENDIF

	if (isec4(5).eq.0)then

C
      CALL GRPRS4 (ISEC0,ISEC4,ZSEC4)
C
	  write (*,*)' last value=',zsec4(isec4(1)),isec4(1)

	  rmin=+1.5e21
	  rmax=-1.4e21
	  ngood=0
	  do i=1,isec4(1)
	    if(zsec4(i).ne.rmis) then
		rmin=min(rmin,zsec4(i))
		rmax=max(rmax,zsec4(i))
		ngood=ngood+1
	    end if
	  end do
	  write (*,*)' min value=',rmin,' max value=',rmax,' buoni=',ngood

	else

	write (*,*)' i primi venti valori'
	do ia=1,200
		write(*,*)izsec4(ia)
	end do

	  write (*,*)' last value=',izsec4(isec4(1)),isec4(1)

	  imin=32767
		write(*,*)imin
	  imax=kneg
	  ngood=0
	  do i=1,isec4(1)
	    if(izsec4(i).ne.imis) then
		imin=min(imin,izsec4(i))
		imax=max(imax,izsec4(i))
		ngood=ngood+1
	    end if
	  end do
	  write (*,*)' min value=',imin,' max value=',imax,' buoni=',ngood

	end if

	GO TO 50
C
C
  300 STOP 'Read error'
  310 call pbclose (kunit,kret)
      WRITE (*,9000)
      WRITE (*,9000)
      WRITE (*,9002) NUMERR
      STOP 'end of file'
C
 9000 FORMAT (1H )
 9001 FORMAT (1H ,'DISPLGRIB : Function code = ',A)
 9002 FORMAT (1H ,'DISPLGRIB : Number of decoding errors = ',I9)
 9004 FORMAT (1H ,'DISPLGRIB : GRIBEX return code = ',I4)
C
      END


