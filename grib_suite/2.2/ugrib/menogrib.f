      PROGRAM MENOGRIB
C
C     Arrays are dimensioned to accommodate T213/N160 data volumes.
C

	parameter (imaxvarcumul128=18)
	parameter (imaxvarcumul1=9)
	parameter (imaxvarcumul2=9)

      PARAMETER (JPACK=400000,IPUMP=JPACK*4)
      DIMENSION ISEC0(2)
      DIMENSION ISEC1(32)
      DIMENSION ISEC2(384)
      DIMENSION ISEC3(2)
      DIMENSION ISEC4(42)
      DIMENSION ZSEC2(96)
      DIMENSION ZSEC3(2)
C
C     Array for real parameters from section 4 of GRIB message.
C     This is the binary data section and the array to hold
C     the unpacked data may need to be 4 times as long as that
C     for the packed data.
C
      DIMENSION ZSEC4(IPUMP)
      DIMENSION stack(IPUMP)
C
C     Array to read in packed data.
C
      DIMENSION grib(JPACK)

	dimension ivarcumul128(imaxvarcumul128)
	dimension ivarcumul1(imaxvarcumul1)
	dimension ivarcumul2(imaxvarcumul2)

	data ivarcumul128/142,143,144,145,146,147,176,177,178,179
     $       ,180,181,182,195,196,197,205,228/
	data ivarcumul1/59,61,62,63,64,78,79,90,99/
	data ivarcumul2/59,61,62,63,64,78,79,90,99/

	character*80 file

	DATA IMSVAL,RMSVAL/32767,-1.5E21/	!missing values
C
C     GRIBEX routine has a number of different options.
C
      CHARACTER*1 YOPER


	character*80 fileout,filein,fileoutfau,fileinfau,FIL
	DATA fileout,filein/'NEWGRIB.GRB','GRIB.GRB'/
	DATA fileoutfau,fileinfau/'NEWMAGGETGRIBFAU.DAT',
     $       'MAGGETGRIBFAU.DAT'/


      call getenv('MENOGRIBIN',fil)
	  if (fil.ne.' ') then
	  i = index(fil,' ')-1
          FILEIN=fil(:i)
          write(*,*)FILEIN
      endif
  
      call getenv('MENOGRIBOUT',fil)
	  if (fil.ne.' ') then
	  i = index(fil,' ')-1
          FILEOUT=fil(:i)
          write(*,*)FILEOUT
      endif
       call getenv('MENOGRIBFAUIN',fil)
	  if (fil.ne.' ') then
	  i = index(fil,' ')-1
          FILEINFAU=fil(:i)
          write(*,*)FILEINFAU
      endif
       call getenv('MENOGRIBFAUOUT',fil)
	  if (fil.ne.' ') then
	  i = index(fil,' ')-1
          FILEOUTFAU=fil(:i)
          write(*,*)FILEOUTFAU
      endif
 
C
C     Clear error counter and init other.
C
	NUMERR = 0

	call grsvck(0)		!disable check of parameters

	KPR=0	!DEBUG PRINT SWITCH
	CALL SETPAR (KBIT,KNEG,KPR) !number of bit in computer word
	kinlen=jpack*kbit/8

	isec3(2)=IMSVAL		!missing value
	zsec3(2)=RMSVAL		!missing value

15	print*,'per CUMULARE  inserire il numero di grib da cumulare'
	print*,'per SOTTRARRE inserire un numero negativo'
	print*,'nessuna operazione -> 0   help -> ?'
	read(05,*,err=16)iope
	iope=max(iope,-1)
c				open file
	open (unit=9,file=fileinfau,status='old'
     $       ,iostat=iofau,err=22)
	read (9,'(a80)')file
	open (unit=10,file=fileoutfau,status='unknown')
	write(10,'(a80)')file

22	continue
	call pbopen (iug1,filein,'r',kret)
	if(kret.ne.0)then
		print *,'codice errore pbopen read',kret
		stop 'fatal error'
	end if

	call pbopen (iug2,fileout,'w',kret)
	if(kret.ne.0)then
 		print *,'codice errore pbopen write',kret
		stop 'fatal error'
	end if

50	CONTINUE

	if(iofau.eq.0)read(09,*,iostat=iofau)ifau
	if(ifau.ne.0)then
		ifaunew=1
		write (10,*)ifaunew
		stop 'errore >> dati mancanti'
c		itoto=itoto+1
c		goto 50
	end if		

	call pbgrib(iug1,grib,kinlen,koutlen,kret)
	if(kret.eq.-1)stop 'end of file'
	if(kret.eq.-2)stop 'Read error'
	if(kret.ne. 0)stop 'fatal error pbgrib'
	print*,'ho letto un grib'

	if(iope.eq.0)goto 100

C     'D' function to unpack entire GRIB message.

	YOPER = 'D'
	write(06,*)'GRIBEX  YOPER = D'
	CALL GRIBEX (ISEC0,ISEC1,ISEC2,ZSEC2,ISEC3,ZSEC3,ISEC4,
     C             ZSEC4,IPUMP,grib,JPACK,IWORD,YOPER,IERR)
	WRITE (*,9003) IERR
C
C     Check return code.
c
      IF (IERR.GT.0)
     C   THEN
             NUMERR = NUMERR + 1
	     ifaunew=1  
           GO TO 50
         ENDIF

C     Section 1 is the product definition section.
C     Section 2 is the grid definition section.
C     Section 3 is the bit-map section.
C     Section 4 is the data section.
C
         if (iope.gt.0)then
           if(itoto.ge.iope)then
             print*,' azzero la memoria'
             itoto=0
             do i=1,isec4(1)
               STACK(I)=0
             end do
           end if
         end if

         if (isec1(1).eq.1)then
           do i=1,imaxvarcumul1
             if (isec1(6).eq.ivarcumul1(i))goto 110
           end do
         end if
         if (isec1(1).eq.2)then
           do i=1,imaxvarcumul2
             if (isec1(6).eq.ivarcumul2(i))goto 110
           end do
         end if
         if (isec1(1).eq.128)then
           do i=1,imaxvarcumul128
             if (isec1(6).eq.ivarcumul128(i))goto 110
           end do
         end if

       
	goto 100

110	continue
	itoto=itoto+1
	if (itoto.eq.1.and.iope.gt.0)inizio=isec1(16)
	if (itoto.eq.1.and.iope.lt.0)inizio=0
	if(iope.lt.0)print*,'  >>> il grib',itoto,' viene sottratto'
	if(iope.gt.0)print*,'  >>> il grib',itoto,' viene sommato'

	DO I=1,ISEC4(1)
	  if (ZSEC4(I).eq.rmsval.or.STACK(I).eq.rmsval)then
		diff=rmsval
	  else
		DIFF=ZSEC4(I)+(STACK(I)*float(sign(1,iope)))
	  end if
	  if(iope.lt.0)then
		STACK(I)=ZSEC4(I)
	  else
		STACK(I)=diff
	  end if
	  ZSEC4(I)=DIFF
	END DO

c	if(itoto.ne.iope.and.iope.gt.0)goto 50
	if(itoto.lt.abs(iope))goto 50

	write(10,*)ifaunew
	ifaunew=0
	if(ifaunew.ne.0)goto 50

	ISEC4(2)=24		!massima precisione (visto che non so)
	if(iope.gt.0)then
		isec1(16)=inizio
	else if (iope.lt.0)then

          if (isec1(18).eq.4)then
            isec1(16)=inizio
            inizio=isec1(17)
          else
            isec1(17)=isec1(16)
            isec1(16)=inizio
            isec1(18)=4
            inizio=isec1(17)
          end if

	end if

	YOPER = 'C'
	write(06,*)'GRIBEX  YOPER = C'
	CALL GRIBEX (ISEC0,ISEC1,ISEC2,ZSEC2,ISEC3,ZSEC3,ISEC4,
     C             ZSEC4,IPUMP,grib,JPACK,IWORD,YOPER,IERR)
	WRITE (*,9004) IERR
	koutlen=isec0(1)

100	CONTINUE

	write(10,*)ifau

	call pbwrite(iug2,grib,koutlen,kret)
	if(kret.lt.0)then
		print *,'codice errore pbwrite ',kret
		stop 'fatal error'
	end if
	print*,'ho scritto un grib'
      GO TO 50
C
  200 CONTINUE
C
	WRITE (*,9002) NUMERR
	call pbclose(iug1,kret)
	if(kret.ne.0)print *,'codice errore pbclose read ',kret
	call pbclose(iug2,kret)
	if(kret.ne.0)print *,'codice errore pbclose write',kret
C
      STOP
C
 9002 FORMAT (1H ,'PROGRAM MENOGRIB: Number of decoding errors = ',I9)
 9003 FORMAT (1H ,'DECODIFICA GRIB : GRIBEX return code = ',I4)
 9004 FORMAT (1H ,'CODIFICA   GRIB : GRIBEX return code = ',I4)
C

16	continue

	print*,'gestite variabili tabella 2 versione 1'
	print*,' '
	do ilj=1,imaxvarcumul1
	 print *,ivarcumul1(ilj)
	end do
	print*,' '
	print*,'gestite variabili tabella 2 versione 128 ECMWF'
	print*,' '
	do ilj=1,imaxvarcumul128
	 print *,ivarcumul128(ilj)
	end do
	print*,' '
	print*,'gestite variabili tabella 2 versione 2 DWD'
	print*,' '
	do ilj=1,imaxvarcumul2
	 print *,ivarcumul2(ilj)
	end do

	goto 15

      END


