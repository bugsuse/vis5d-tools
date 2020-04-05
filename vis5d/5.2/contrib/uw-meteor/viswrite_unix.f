      SUBROUTINE VISWRITE(NVIS,ARRAY,SCRATCH,LL,MM,NN,FILENM
     &                 ,XLATN,XLONW,XLATIN,XLONIN,XHGTT,XHGTIN
     &                 ,IYR,IMO,IDY,IHR,IMN,ISC)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Driving routine for writing VIS format files
C
C INPUT:
C        NVIS    - Number of variables to be put into vis file
C        ARRAY   - 3-Dimensional Array to be filled below in READROUTINE
C                  ARRAY is dimensioned (MM,LL,NN) where
C                  ARRAY(1,1,1) is the North-West-Bottom point in the array
C        SCRATCH - 3-dimensional Scratch array dimensioned (MM,LL,NN)
C        LL      - Number of points in the East-West   Direction
C        MM      - Number of points in the North-South Direction
C        NN      - Number of points in the Vertical    Direction
C        FILENM  - Prefix to the VIS format output files
C        XLATN   - Latitude of Northernmost point (degrees, positive North)
C        XLONW   - Longitude of Westernmost point (degrees, [-179 to +180]
C                                              Positive East, Negative West)
C        XLATIN  - Latitude increment (degrees)
C        XLONIN  - Longitude increment (degrees)
C        XHGTT   - Height of Top most point (meters)
C        XHGTIN  - Height increment (meters)
C        IYR     - Year (thousands; i.e. 1993)
C        IMO     - Month (1==Jan, 12==Dec)
C        IDY     - Day of month (1-31)
C        IHR     - Hour (0-23)
C        IMN     - Minute (0-59)
C        ISC     - Seconds (0-59)
C
C OUTPUT:
C        A File Containing the VIS info for all variables for this time.
C        The file name will consist of a prefix (FILENM) to which the 
C        Julian Day/Julian Hour are appended
C        For Example, if FILENM is 'VISFILE', the Julian Day is 92123, and
C        the Julian time is 120000, then the file will be called:
C 
C                  VISFILE.92123.120000
C
C
C NOTE: 
C        The user must supply a routine to fill in the ARRAY with each
C        variable being written out.  A dummy routine (READROUTINE) is
C        currently called.
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      DIMENSION ARRAY(MM,LL,NN),SCRATCH(MM,LL,NN)
      CHARACTER*42 VISFILL,VISFILP
      character*(*) FILENM
      character*42 prefix1
      character*40 cfname,cfname1,ofname
      character*8 VARNAME
      INTEGER*4 IVTAB(64)
C
C Define number of points in each direction after averaging/cutting
C
C      LL= Number or Longitude Points
C      MM= Number of Latitude Points
C      NN= Number of points in vertical
C
C If not on stellar, open file to write info to
C     print*,'call makefn'
      iyear=1000*mod(iyr,100)+julday(imo,idy,iyr)
      itime=10000*IHR
     &       +100*IMN
     &           +ISC
      print107,iyear
107   format('iyear= ',i11.5)
      print108,itime
108   format('itime= ',i11.6)
      CALL MAKEFN(FILENM,'AUTO',IYEAR,ITIME,' ','A',VISFILL,VISFILP)
      print*,'OPENING NEW FORMATTED VISFILE: ',VISFILP
      OPEN(18,file=VISFILP,form='FORMATTED')
      WRITE(18,801) NVIS
      WRITE(18,801) LL
      WRITE(18,801) MM
      WRITE(18,801) NN
801   FORMAT(I4)
C
C Initialize 64 word mcidas-style header to zeros - some locations are not
C used, and are just filled in with zeros.
      DO I=1,64 
        IVTAB(I)=0
      ENDDO
C
C Set 64 word integer header array
C
C Total number of points in grid
      IVTAB(1)=LL*MM*NN
C Number of Latitude points
      IVTAB(2)=MM
C Number of Longitude points
      IVTAB(3)=LL
C Number of heights
      IVTAB(4)=NN
C Latitude,longitude in degrees * 10000
      IVTAB(22)=4
C North Latitude * 10000
      IVTAB(23)=NINT(XLATN*10000.)
C West Longitude * 10000
      xlonw1=-xlonw
      if(xlonw1.lt.0)xlonw1=360.+xlonw1
      IVTAB(24)=NINT(XLONW1*10000.)
C Latitude Increment * 10000
      IVTAB(25)=NINT(XLATIN*10000.)
C Longitude Increment * 10000
      IVTAB(26)=NINT(XLONIN*10000.)
C Heights in meters
      IVTAB(31)=1
C Top Height in meters
      IVTAB(32)=NINT(XHGTT)
C Height Increment in meters
      IVTAB(33)=NINT(XHGTIN)
C Day in YYDDD
      IVTAB(6)=JULDAY(IMO,IDY,IYR)
      MTIME=ISC+100*IMN+10000*IHR
C Time in HHMMSS
      IVTAB(7)=MTIME
C 
C 
      DO 1000 IPLT=1,NVIS
C**********************************************************************
C
C The user must supply his own routine to fill ARRAY with data and
C VARNAME with a maximum of 8 character string describing the variable
C
C
        CALL READROUTINE(IPLT,ARRAY,LL,MM,NN,VARNAME)
C
C**********************************************************************
        PRINT*,'FIELD :',VARNAME,':'
C
C Write variable name to file 
C
      WRITE(18,802) VARNAME
802   FORMAT(A8)
C
C Call routine to write formatted "vis file"
C
      CALL FORMWRT(ARRAY,LL,MM,NN,IVTAB,SCRATCH)
1000  CONTINUE 
      close(18)
      RETURN
      END
C
      SUBROUTINE FORMWRT(ARRAY,LL,MM,NN,IVTAB,SCRATCH)
      DIMENSION ARRAY(MM,LL,NN),SCRATCH(MM,LL,NN)
      DIMENSION IVTAB(64)
      CALL VIOREC(18,IVTAB,64,21,SCRATCH)
      CALL VFOREC(18,ARRAY(1,1,1),LL*MM*NN,18,SCRATCH,'LIN')
      RETURN
      END
      SUBROUTINE READROUTINE(IPLT,ARRAY,LL,MM,NN,VARNAME)
      DIMENSION ARRAY(MM,LL,NN)
      CHARACTER*(*) VARNAME
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C 
C     SAMPLE DATA ARRAY FILL ROUTINE
C      
C     INPUT:
C            IPLT    - Variable number
C            LL      - Number of Longitude Points     1 = West
C            MM      - Number of Latitude Points      1 = North
C            NN      - Number of Height Points        1 = Bottom
C
C     OUTPUT:
C            ARRAY   - Floating Point array dimensioned (MM,LL,NN)
C                      which contains the values of this variable
C            VARNAME - character*8 variable name
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IF(IPLT.EQ.1) THEN
        DO 101 J=1,MM
          DO 102 I=1,LL
            DO 103 K=1,NN
c             ARRAY(J,I,K)=UVAL(J,I,K)
              ARRAY(J,I,K)=float(k)
103         CONTINUE
102       CONTINUE
101     CONTINUE
        VARNAME='U-COMP'
      ELSEIF(IPLT.EQ.2) THEN
        DO 104 J=1,MM
          DO 105 I=1,LL
            DO 106 K=1,NN
c             ARRAY(J,I,K)=VVAL(J,I,K)
              ARRAY(J,I,K)=float(k)
106         CONTINUE
105       CONTINUE
104     CONTINUE
        VARNAME='V-COMP'
      ENDIF
      RETURN
      END
 
      SUBROUTINE MAKEFN(PREFIX,AUTOMAN,IYEAR,ITIME,TUNIT,TYPE
     &                 ,LOCAL,PERM)
      CHARACTER*(*) PREFIX,AUTOMAN,TYPE,LOCAL,PERM,TUNIT
      CHARACTER*12 CT,ANS*1
      INTEGER ICT
C
      IF(AUTOMAN.EQ.'AUTO') THEN
         IF(TUNIT(1:1).EQ.' ')
     +           WRITE(CT,'(I5.5,''.'',I6.6)')IYEAR,ITIME
         ICT=0
         DO NC=1,12 
            IF(CT(NC:NC).EQ.' ')ICT=NC
         ENDDO
         IBLANK=INDEX(PREFIX,' ')
         if(iblank.eq.0)iblank=len(prefix)+1
         PERM=PREFIX(1:IBLANK-1)//CT(ICT+1:12)
         LOCAL=PERM
      ELSEIF(AUTOMAN.EQ.'MAN')THEN
         PERM=PREFIX
         LOCAL=PERM
      ENDIF
C
      RETURN
      END
C
C##############################################################
C
C This is the version with the *fixed* accuracy bug.
C P.P.11.18.91
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine vfinit
      character*1 vc, vcscr(0:63)
      common/vform/vc(0:63)
      data vcscr/'0','1','2','3','4','5','6','7','8','9'
     +          ,'A','B','C','D','E','F','G','H','I','J'
     +          ,'K','L','M','N','O','P','Q','R','S','T'
     +          ,'U','V','W','X','Y','Z','a','b','c','d'
     +          ,'e','f','g','h','i','j','k','l','m','n'
     +          ,'o','p','q','r','s','t','u','v','w','x'
     +          ,'y','z','{','|'/

      do 10 n=0,63
         vc(n)=vcscr(n)
  10  continue

      return
      end
c---------------------------------------------------------
      subroutine vctran(iun1,iun2,type)
      character*78 line,type*(*)

      read(iun1,'(a78)') line
      write(iun2,'(a78)') line
      
      if(type.eq.'C') then
         read(line,'(i8)') n
         nlines=n/78+1
      elseif(type.eq.'I'.or.type.eq.'F') then
         read(line,'(2i8)')n,nbits
         nlines=(n*nbits/6-1)/78+1
      endif
      
      do 10 nl=1,nlines
         read(iun1,'(a78)') line
         write(iun2,'(a78)') line
 10   continue
      
      return
      end
c--------------------------------------------------------
      subroutine vcorec(iunit,a,n)
      character*1 a(1)

      write(iunit,11)n
 11   format(i8)

      do 10 nc=1,n,78
         ne=min(n,nc+78)
         write(iunit,'(78a1)') (a(i),i=nc,ne)
 10   continue
      
      return
      end
c--------------------------------------------------------
      subroutine vcirec(iunit,a,n)
      character*1 a(1)

      read(iunit,11)nn
 11   format(i8)
      
      if(nn.ne.n) then
         print*,' Character count mismatch on vcirec record '
         print*,' Characters on record - ',nn
         print*,' Characters expected  - ',n
         stop 'vcirec'
      endif

      do 10 nc=1,n,78
         ne=min(n,nc+78)
         read(iunit,'(78a1)') (a(i),i=nc,ne)
 10   continue
      
      return
      end
c--------------------------------------------------------
      subroutine vforec(iunit,a,n,nbits,scr,type)
      double precision  bias, fact
      dimension a(*),scr(*)
      character*(*) type
      character*1 vc
      common/vform/vc(0:63)
c
      if(vc(0).ne.'0') call vfinit
c
c        log scaling assumes range of +/- 10e10
c
      call cscale(a,n,amin,amax)
      bias=dble(-amin+1.e-20)
      call cfact(bias,amax,nbits,fact)
      sbias=sngl(bias)
      sfact=sngl(fact)
      
      write(iunit,10)n,nbits,bias,fact
 10   format(2i8,2e20.10)
      call vwrt(iunit,a,n,bias,fact,nbits,scr,type)

      return
      end
c--------------------------------------------------------
      subroutine vwrt(iunit,a,n,bias,fact,nbits,scr,type)
      double precision  bias, fact
      character*(*) type
      character*1 vc
      common/vform/vc(0:63)
      dimension a(n),scr(n)
      character line*80,form*5

      if(type.eq.'LIN') then
         do 10 i=1,n
            scr(i)=sngl( ( dble(a(i))+bias ) *fact )
 10      continue
      elseif(type.eq.'LOG') then
         scfct=2.**(nbits-1)
         do 11 i=1,n
            scr(i)=(sign(1.,a(i))*(log10(max(1.e-10,abs(a(i))))+10.)
     +           /20.+1.)*scfct
 11      continue
      endif

      nvalline=(78*6)/nbits
      nchs=nbits/6
      do 20 i=1,n,nvalline
         ic=0
         do 30 ii=i,i+nvalline-1
            if(ii.gt.n) go to 31
            isval=int(scr(ii))
            do 40 iii=1,nchs
               iscr=intand(intrshft(isval,6*(nchs-iii)),63)
               ic=ic+1
               line(ic:ic)=vc(iscr)
 40         continue
 30      continue
 31      continue
         write(form,100) ic
         write(iunit,form) line(1:ic)
 20   continue                 

 100  format('(a',i2,')')

      return
      end
c--------------------------------------------------------
      subroutine vfirec(iunit,a,n,type)
      character*1 vc
      character*(*) type
      common/vform/vc(0:63)
      character line*80, cs*1
      dimension a(*)

      if(vc(0).ne.'0') call vfinit

      ich0=ichar('0')
      ich9=ichar('9')
      ichcz=ichar('Z')
      ichlz=ichar('z')
      ichca=ichar('A')
      ichla=ichar('a')
      
      read(iunit,10)nn,nbits,bias,fact
 10   format(2i8,2e20.10)
      if(nn.ne.n) then
         print*,' Word count mismatch on vfirec record '
         print*,' Words on record - ',nn
         print*,' Words expected  - ',n
         stop 'vfirec'
      endif

      nvalline=(78*6)/nbits
      nchs=nbits/6
      do 20 i=1,n,nvalline
         read(iunit,'(a78)') line
         ic=0
         do 30 ii=i,i+nvalline-1
            isval=0
            if(ii.gt.n) go to 20
            do 40 iii=1,nchs
               ic=ic+1
               cs=line(ic:ic)
               ics=ichar(cs)
               if(ics.le.ich9)then
                  nc=ics-ich0
               elseif(ics.le.ichcz) then
                  nc=ics-ichca+10
               else
                  nc=ics-ichla+36
               endif
               isval=intor(intlshft(nc,6*(nchs-iii)),isval)
 40         continue
            a(ii)=isval
 30      continue
 20   continue

      facti=1./fact
      if(type.eq.'LIN') then
         do 48 i=1,n
            a(i)=a(i)*facti-bias
 48      continue
      elseif(type.eq.'LOG') then
         scfct=2.**(nbits-1)
         do 55 i=1,n
            a(i)=sign(1.,a(i)-scfct)
     +           *(10.**(abs(20.*(a(i)/scfct-1.))-10.))
 55      continue
      endif

      return
      end
c--------------------------------------------------------
      subroutine cscale(a,n,amin,amax)
      dimension a(n)

      amin=1.e30
      amax=-1.e30
      do 10 nn=1,n
         amin=min(amin,a(nn))
         amax=max(amax,a(nn))
 10   continue

      return
      end
c---------------------------------------------------------      
      subroutine cfact(bias,amax,nbits,fact)
      double precision  bias, bignum, fact, tnum

      bignum=dble(2**nbits-1)
      tnum=bias+dble(amax)
      fact=bignum/(tnum+1.d-20)

      return
      end
      
      subroutine viorec(iunit,ia,n,nbits,scr)
      dimension ia(*),scr(*)
      character*1 vc
      common/vform/vc(0:63)
      
      if(vc(0).ne.'0') call vfinit


      call cscalei(ia,n,iamin,iamax)
      bias=-iamin
      fact=1.
      if((iamax+bias).gt.(2**nbits-1)) then
        print*,'!! Warning from viorec !! - truncation will occur !!'
        print*,'   Maximum- ',iamax,'  bias- ',bias,'  nbits-',nbits
      endif

      write(iunit,10)n,nbits,bias,fact
 10   format(2i8,2e20.10)
      call vwrti(iunit,ia,n,bias,fact,nbits,scr)

      return
      end
c--------------------------------------------------------
      subroutine vwrti(iunit,ia,n,bias,fact,nbits,iscr)
      character*1 vc
      common/vform/vc(0:63)
      dimension ia(n),iscr(n)
      character line*80,form*5

      do 10 i=1,n
         iscr(i)=(ia(i)+bias)*fact+.001
 10   continue

      nchs=(nbits+5)/6
      nvalline=78/nchs
      do 20 i=1,n,nvalline
         ic=0
         do 30 ii=i,i+nvalline-1
            if(ii.gt.n) go to 31
            isval=iscr(ii)
            do 40 iii=1,nchs
               iiscr=intand(intrshft(isval,6*(nchs-iii)),63)
               ic=ic+1
               line(ic:ic)=vc(iiscr)
 40         continue
 30      continue
 31      continue
         write(form,100) ic
         write(iunit,form) line(1:ic)
 20   continue

 100  format('(a',i2,')')

      return
      end
c--------------------------------------------------------
      subroutine viirec(iunit,ia,n)
      character*1 vc
      common/vform/vc(0:63)
      character line*80, cs*1
      dimension ia(*)
      data ich0/48/,ich9/57/,ichcz/90/,ichca/65/,ichla/97/
      ich0=ichar('0')
      ich9=ichar('9')
      ichcz=ichar('Z')
      ichlz=ichar('z')
      ichca=ichar('A')
      ichla=ichar('a')
      
      if(vc(0).ne.'0') call vfinit

      read(iunit,10,end=12)nn,nbits,bias,fact
 10   format(2i8,2e20.10)
      go to 15
 12   continue
      n=-1
      return
 15   continue
      if(nn.ne.n) then
         print*,' Word count mismatch on viirec record '
         print*,' Words on record - ',nn
         print*,' Words expected  - ',n
         stop 'viirec'
      endif

      nchs=(nbits+5)/6
      nvalline=78/nchs
      do 20 i=1,n,nvalline
         read(iunit,'(a78)') line
         ic=0
         do 30 ii=i,i+nvalline-1
            isval=0
            if(ii.gt.n) go to 20
            do 40 iii=1,nchs
               ic=ic+1
               cs=line(ic:ic)
               ics=ichar(cs)
               if(ics.le.ich9)then
                  nc=ics-ich0
               elseif(ics.le.ichcz) then
                  nc=ics-ichca+10
               else
                  nc=ics-ichla+36
               endif
               isval=intor(intlshft(nc,6*(nchs-iii)),isval)
 40         continue
            ia(ii)=isval
 30      continue
 20   continue

      facti=1./fact
      do 48 i=1,n
         ia(i)=ia(i)*facti-bias
 48   continue

      return
      end
c--------------------------------------------------------
      subroutine cscalei(ia,n,iamin,iamax)
      dimension ia(n)

      iamin= 1000000000
      iamax=-1000000000
      do 10 nn=1,n
         iamin=min(iamin,ia(nn))
         iamax=max(iamax,ia(nn))
 10   continue

      return
      end
      FUNCTION INTRSHFT(IWORD,NSHFT)
C
C       This function shifts IWORD to the right NSHFT bits.
C
      INTRSHFT=ISHFT(IWORD,-NSHFT)
      RETURN
      END
      FUNCTION INTLSHFT(IWORD,NSHFT)
C
C       This function shifts IWORD to the left NSHFT bits in a
C         circular manner.
C
      INTLSHFT=ISHFT(IWORD,NSHFT)
      RETURN
      END
      FUNCTION INTAND(IWORD1,IWORD2)
C
C       This function performs a bit-by-bit AND between IWORD1 and
C         IWORD2.
C
      INTAND=IAND(IWORD1,IWORD2)
      RETURN
      END
      FUNCTION INTOR(IWORD1,IWORD2)
C
C       This function performs a bit-by-bit OR between IWORD1 and
C         IWORD2.
C
      INTOR=IOR(IWORD1,IWORD2)
      RETURN
      END
      FUNCTION JULDAY(IMONTH,IDAY,IYEAR)
C
C     COMPUTE THE JULIAN DAY FROM A NORMAL DATE
C
      JULDAY= IDAY
     +      +MIN(1,MAX(0,IMONTH-1))*31
     +      +MIN(1,MAX(0,IMONTH-2))*(28+(1-MIN(1,MOD(IYEAR,4))))
     +      +MIN(1,MAX(0,IMONTH-3))*31
     +      +MIN(1,MAX(0,IMONTH-4))*30
     +      +MIN(1,MAX(0,IMONTH-5))*31
     +      +MIN(1,MAX(0,IMONTH-6))*30
     +      +MIN(1,MAX(0,IMONTH-7))*31
     +      +MIN(1,MAX(0,IMONTH-8))*31
     +      +MIN(1,MAX(0,IMONTH-9))*30
     +      +MIN(1,MAX(0,IMONTH-10))*31
     +      +MIN(1,MAX(0,IMONTH-11))*30
     +      +MIN(1,MAX(0,IMONTH-12))*31
      RETURN
      END
