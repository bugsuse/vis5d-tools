      PROGRAM GRIBBO
Comstart gribbo
Cidx **** GRIBBO - Program to convert fron AFA to a GRIB file.
C
C
c $Date: 2002/09/25 16:44:19 $    $Revision: 1.14 $
c $Id: gribbo.f,v 1.14 2002/09/25 16:44:19 patruno Exp $
c
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
C           Paolo Patruno    5/12/96   at S.M.R.
C
C     Modifications.
C     --------------
C
c     4/12/98
c     Changed predefined area for operational grib from 2 (father) to 4
c     and from 3 (son) to 5.
c
c     11/12/98
c     Fixed bug in computation of maximum value
c
c     8/4/1999
c     changed matrix dimension and added dimension error check 
C
c     12/4/1999
c     added stereographic projection (drt=5)
c     modificato isec2(19) impostato a 8 solo se irg (drt) =10
c     aggiunta proiezione lambert
c
c     23/11/1999
c     Y2K compliant
c
c
C     -----------------------------------------------------------------
Comend
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
      DIMENSION ISEC1(100)
C
C     Array for integer parameters from section 2 of GRIB message.
C
      DIMENSION ISEC2(384)
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
      DIMENSION ZSEC2(96)
C
C     Array for real parameters from section 3 of GRIB message.
C
      DIMENSION ZSEC3(2)
C
C     Array for real parameters from section 4 of GRIB message.
C     This is the binary data section and the array to hold
C     the unpacked data may need to be 2 times as long as that
C     for the packed data.
C
      DIMENSION field(JPACK*2)		!stagghered
      DIMENSION ifield(JPACK*2)		!stagghered
      EQUIVALENCE (FIELD,IFIELD)
      DIMENSION fieldw(JPACK*2*2)	!regular
C
C     Array to write out packed data.
C
      DIMENSION INBUFF(JPACK)
C
C     GRIBEX routine has a number of different options.
C
      CHARACTER*1 YOPER
C
      CHARACTER*80 filein,gribout,par

	character code*80,cija*3

	integer mesi(12),ntf,nrm
	character*4 eqexpid,expid
	equivalence (isec1(41),eqexpid)
	data mesi/31,28,31,30,31,30,31,31,30,31,30,31/

	dimension idata(5),isca(4),ilev(3),ivar(6)
	logical corn,sezione2,sezione3,verbose,comp
	data corn,numaver,verbose,comp/.false.,1,.false.,.false./
        data expid,nrm,ntf /" ",0,0/

C
C     Clear error counter.
C
      NUMERR = 0

	KPR=0   !DEBUG PRINT SWITCH
        call grsdbg(kpr)
        call grsvck(0)                  !disable check of parameters
  
	KPR=0	!DEBUG PRINT SWITCH
	CALL SETPAR (KBIT,KNEG,KPR)
C
C     Lengths of INBUFF and PSEC4
C
      ILENB = JPACK
      KLEN  = JPACK * 2
      KINLEN= JPACK*(KBIT/8)


	igenproc=0
        iemiss=-1
	iarea=255
	ibitm=0
	call getarg(1,filein)
	if (filein.eq.' '.or.filein(1:1).eq.'-')then
	  write(*,*)'		GRIBBO'
          write(*,*)
     +         '$Id: gribbo.f,v 1.14 2002/09/25 16:44:19 patruno Exp $'
          write (*,*) ' '
	  write(*,*)'usage:'
	  write(*,*)'gribbo <filein> <gribout>',
     +     ' [-piii] [-ejjj] [-akkk] [-blll] [-iaaaa] ',
     +     '[-mnrm] [-nntf] [-c] [-comp] [-v]'
	  write(*,*)' '
	  write(*,*)'general conversion from AFA to GRIB'
	  write(*,*)'<filein>  file input'
	  write(*,*)'<gribout> file output'
	  write(*,*)'-piii     forcing iii generating process'
	  write(*,*)'-ejjj     forcing jjj emission centre'
	  write(*,*)'-akkk     forcing kkk grid definition'
	  write(*,*)'-blll     forcing lll predetermined bit map'
	  write(*,*)'-iaaaa    version number or experiment identifier'
     +     ,'(x is special case)'
	  write(*,*)'-mnrm     ensamble: forecast number (isec1(42))'
	  write(*,*)'-nntf     ensamble: total number of', 
     +         'forecast (isec1(43))'
	  write(*,*)'-c        remove border line (cornice)'
	  write(*,*)'-comp     data compress'
	  write(*,*)'-v        set verbose mode'
          goto 300
        end if
	call getarg(2,gribout)
	if (gribout.eq.' '.or.gribout(1:1).eq.'-')then
	  write(*,*)'usage:'
	  write(*,*)'gribbo <filein> <gribout>',
     +     ' [-piii] [-ejjj] [-akkk] [-blll] [-iaaaa] ',
     +     '[-mnrm] [-nntf] [-c] [-comp] [-v]'
	  write(*,*)' '
          goto 300
        end if

	do i=3,IARGC()
	call getarg(i,par)
	  if (par.ne.' ')then
		if(	par(1:2).eq.'-p'.or.
     $            par(1:2).eq.'-P')then
			read (par(3:5),'(i3)',err=301)igenproc
		else if(par(1:2).eq.'-e'.or.
     $                       par(1:2).eq.'-E')then
			read (par(3:5),'(i3)',err=307)iemiss
		else if(par(1:2).eq.'-a'.or.
     $                       par(1:2).eq.'-A')then
			read (par(3:5),'(i3)',err=302)iarea
		else if(par(1:2).eq.'-b'.or.
     $                       par(1:2).eq.'-B')then
			read (par(3:5),'(i3)',err=303)ibitm
		else if(par(1:2).eq.'-i'.or.
     $                       par(1:2).eq.'-I')then
			read (par(3:6),'(a4)',err=303)expid
		else if(par(1:2).eq.'-m'.or.
     $                       par(1:2).eq.'-M')then
			read (par(3:5),'(i3)',err=303)nrm
		else if(par(1:2).eq.'-n'.or.
     $                       par(1:2).eq.'-N')then
			read (par(3:5),'(i3)',err=303)ntf
		else if(par.eq.'-c'.or.
     $                       par.eq.'-C')then
			corn=.true.
		else if(par.eq.'-v'.or.
     $                       par.eq.'-V')then
			verbose=.true.
		else if(par.eq.'-comp'.or.
     $                       par.eq.'-COMP')then
			comp=.true.
		else
		  write(*,*)'error in parameter ',par
		  write(*,*)'usage:'
		  write(*,*)'gribbo <filein> <gribout>',
     $   ' [-piii] [-ejjj] [-akkk] [-blll] [-iaaaa] [-c] [-comp] [-v]'
		  write(*,*)' '
	          goto 306
		end if
          end if
	end do

	if(corn)print*,'opzione di decorniciamento attiva'
	if(igenproc.ne.0)print*,'processo generatore forzato a ',igenproc

	open (unit=10,file=filein,status='old',err=304)

	call pbopen(kunit,gribout,'w',kret)
	if(kret.ne.0)goto 305

   50 CONTINUE

	tph0d=0.
	tlm0d=0.
	rot=0.
	iareatmp=255
	ibitmtmp=0
	sezione2=.true.

	read (10,'(a)',end=310)code
	read (10,*,err=330)idata
	read (10,*,err=330)isca
	read (10,*,err=330)ilev
	read (10,*,err=330)ivar
	read (10,*,err=330)irg
	if (irg.eq.3)read (10,*,err=330)ipcf,lov,alat1,alat2
	if (irg.eq.5)read (10,*,err=330)ipcf,lov
	if (irg.eq.10)read (10,*,err=330)tph0d,tlm0d,rot
	read (10,*,err=330)rlat0,rlatn,dj,nplat
	read (10,*,err=330)rlon0,rlonn,di,nplon
	read (10,*,err=330)nval,ija,itipdat,rmis,imis,igrid

	write (*,'(a)')code(:10)
	if (verbose)then
	  write(*,*)'idata'
	  write(*,*)idata
	  write(*,*)'isca'
	  write(*,*)isca
	  write(*,*)'ilev'
	  write(*,*)ilev
	  write(*,*)'ivar'
	  write(*,*)ivar
	  write(*,*)'irg'
	  write(*,*)irg
	  if (irg.eq.10) write (*,*)'tph0d,tlm0d,rot'
	  if (irg.eq.10) write (*,*)tph0d,tlm0d,rot
	  if (irg.eq.5) write (*,*)'ipcf,lov'
	  if (irg.eq.5) write (*,*)ipcf,lov
	  write(*,*)'rlat0,rlatn,dj,nplat'
	  write(*,*)rlat0,rlatn,dj,nplat
	  write(*,*)'rlon0,rlonn,di,nplon'
	  write(*,*)rlon0,rlonn,di,nplon
	  write(*,*)'nval,ija,itipdat,rmis,imis,igrid'
	  write(*,*)nval,ija,itipdat,rmis,imis,igrid
	end if

        if (nval.gt.jpack*2)goto 308

	if(itipdat.eq.0)then
		read (10,*,err=330)(field(i),i=1,nval)
	else
		read (10,*,err=330)(ifield(i),i=1,nval)
	end if

	if (corn)then
	if(igrid.eq.1)then
		i_hv=0		!	punti H
	else if (igrid.eq.2)then
		i_hv=1		!	punti V
	else
		write (*,*)'errore >>> scornicio solo grid tipo 1 e 2'
		NUMERR = NUMERR + 1
		GO TO 50
	end if

	  call cornice(
     $       rlon0,rlonn
     $       ,rlat0,rlatn
     $       ,nplon,nplat,nval
     $       ,rlon0n,rlonnn
     $       ,rlat0n,rlatnn
     $       ,nplonn,nplatn,nvaln,i_hv
     $       ,field,ier)


	  rlon0=rlon0n
	  rlonn=rlonnn
	  rlat0=rlat0n
	  rlatn=rlatnn
	  nplon=nplonn
	  nplat=nplatn
	  nval=nvaln
	end if

c				converto i bit di scansione

	if(verbose) write (cija,'(i3.3)',err=100)ija

	nija=0
	do i=1,3
	 	if (cija(i:i).ne.'0'.and.cija(i:i).ne.'1')goto 100
		if(cija(i:i).eq.'1')nija=nija+2**(4-i+4)
	end do
	goto 200

100	continue

	nija=ija

200	continue

c	!!!!!!!!!!!!!		sezione 1		!!!!!!!!!!!!!!


	isec1( 1)=ivar(2)		!version code table 2
	isec1( 2)=ivar(1)		!identification of centre


c Impostazione centro di emissione, Marghe e Davide, 29/5/98
        if (iemiss.ne.-1) then
          isec1(2)=iemiss
        endif

	if (igenproc.ne.0)then
		isec1(3)=igenproc
	else

	  jj=len(code)
	  do ii=jj,1,-1
		if(code(ii:ii).ne.' ')i=ii
	  end do
	  do ii=1,jj
		if(code(ii:ii).ne.' ')j=ii
	  end do
	  code=code(i:j)
cd	  print*,'code= >>>>',code,'<<<<'
          read(code,'(i3)',iostat=io)isec1(3)
	  if(io.ne.0.or.isec1(3).gt.255)then
	    ISEC1( 3)=1
	  end if

c          print *,i,j,code(3:3)

	  if(j-i.eq.2.and.code(3:3).eq.'O')then	!operativo

C       N.B.
C
c       i_hv=0          punti H
c       i_hv=1          punti V
c
C       E` OBBLIGATORIO MANTENERE LA REGOLA
C       Ibitm < 100 PUNTI H
C       Ibitm > 100 PUNTI V
C               PER IMPEGNO PRESO PER LA DISTRIBUZIONE

            read (code(1:2),'(i2)',iostat=io)iope
            if(io.eq.0)then     !padre
              igp=(mod(iope,50)+1)*2
              
              if (mod(iope,50).ge.20)then
                ibitmtmp=4      !aree per risoluzione 20/10 
              else if (iope.ge.60)then
                ibitmtmp=6	!aree per padre risoluzione 40 con analisi ruotate
              else
                ibitmtmp=2      !aree per risoluzione 40/20
              end if
              
              if (i_hv.ne.0)then
                ibitmtmp=ibitmtmp+100
              end if
              
              if (mod(iope,50).ge.20)then
                iareatmp=6      ! cambio risoluzione
              else if (iope.ge.60)then
                iareatmp=8	!aree per padre risoluzione 40 con analisi ruotate
              else
                iareatmp=4
c	        iareatmp=2  !modificata area operativa
              end if
              
              if(iope.lt.50)then !figlio
                igp  =igp  +1
                ibitmtmp=ibitmtmp+1
                iareatmp=iareatmp+1
              end if

              isec1(2)=200      !centro emissione 200
              isec1(3)=igp
              sezione2=.false.
              igrid=0           !forzo la non introduzione di dati mancanti
            end if
          end if
        end if

	isec1( 6)=ivar(3)		!variabile convertita tabella 001 WMO
	isec1( 7)=ilev(1)
	isec1( 8)=ilev(2)
	isec1( 9)=ilev(3)		!altro livello
	isec1(10)=mod((idata(1)-1),100)+1
	isec1(11)=idata(2)
	isec1(12)=idata(3)
	isec1(13)=idata(4)
	isec1(14)=idata(5)
	isec1(15)=isca(1)		!unit of time (hour)
	isec1(16)=isca(2)		!scadenza/time of accumulation	P1
	isec1(17)=isca(3)		!0       /scadenza		P2
	isec1(18)=isca(4)		!code table 5
	isec1(19)=numaver		!number in average
	isec1(20)=0
	isec1(21)=(idata(1)-1)/100+1	!secoli
	isec1(22)=0			!decimal scale factor
	isec1(23)=0
	isec1(24)=0


c       elaborazione della estensione grib

	if (expid.ne.' ') then

          eqexpid=expid

	  isec1(22)=98          ! sotto centro di emissione(cosi' e'come ECMWF)
	  isec1(24)=1		! local use
	  isec1(37)=1		! definizione degli stati menbri da 192 a 255 
                                ! ma non funziona 
	  isec1(38)=1		! classe operativa
          if (isca(2).eq.0) then
	    isec1(39)=2
	  else
	    isec1(39)=9
	  end if
	  isec1(40)=1025        ! archivio giornaliero
	  isec1(42)=0
	  isec1(43)=0


	  if (ntf.gt.1)then
c           ensemble forecast data
            isec1(37) = 1
            isec1(38) = 2
            if(nrm.eq.0)then
               isec1(39) = 10
            else
               isec1(39) = 11
            endif
            isec1(40) = 1035
            isec1(42) = nrm
            isec1(43) = ntf
	  end if

	  if (expid.eq."x")then
c           definizione del codice esperimento sulla base della data
            
            jday = 0
            ia = 1980
            do while(ia .lt. idata(1))
	      leap =0
	      if(mod(ia,400).eq.0 .OR.
     $             (mod(ia,4).eq.0 .AND. mod(ia,100).ne.0))leap=1
	      jday = jday + 365 + leap
	      ia=ia+1
	    enddo
	    mesi(2) = 28
	    if(mod(ia,400).eq.0 .OR.
     $           (mod(ia,4).eq.0 .AND. mod(ia,100).ne.0))mesi(2)=29
	    do im=1,idata(2)-1
	      jday = jday + mesi(im)
	    enddo
	    jday = jday + idata(3)
	    
	    ia = 3
	    do while(jday.ge.25)
	      eqexpid(ia+1:ia+1)=char(mod(jday,25)+97)
	      jday=jday/25
	      ia = ia - 1
	    enddo
	  end if
	end if


c	!!!!!!!!!!		sezione 2	!!!!!!!!!!!!

        if (irg.eq.10. and. 
     +    tph0d.eq.0. .and. tlm0d.eq.0. .and. rot.eq.0.) then
c         finto caso ruotato, lo riporto al caso normale lat long
          irg=0
        end if

	if(irg.eq.0.or.irg.eq.10)then
	  isec2(6)=128
	  isec2(7)=rlatn*1000.
	  isec2(8)=rlonn*1000.
	  isec2(9) =nint(abs((rlonn-rlon0)/(nplon-1)*1000.))
	  isec2(10)=nint(abs((rlatn-rlat0)/(nplat-1)*1000.))
	  isec2(15)=0
	  isec2(16)=0
        end if

	if(irg.eq.0)then
	  isec2( 1)=0	!latitude/longitude grid
	  isec2(13)=0
	  isec2(14)=0
	  zsec2(1)=0.

	else if (irg.eq.10)then

	  alo1=0.
	  ala1=-90.

          if (tph0d.eq.0. .and. tlm0d.eq.0.) then
            ALO2=alo1
            ALA2=ala1
	  else
            ctph0=cosd(tph0d)
            stph0=sind(tph0d)
            call RTLLD(ALO1,ALA1,TLM0D,CTPH0,STPH0,ALO2,ALA2)    
          endif

cD	  print*,alo2,ala2,rot

	  isec2( 1)=10	!rotated latitude/longitude grid
	  isec2(13)=ALA2*1000.
	  isec2(14)=ALO2*1000.
	  zsec2(1)=rot

	else if (irg.eq.3)then
	  isec2( 1)=3	!stereographic grid
	  isec2(6)=128
	  isec2( 7)=lov

	  isec2( 8)=0
	  isec2( 9)=di
	  isec2(10)=dj
	  isec2(13)=ipcf
	  isec2(14)=alat1
	  isec2(15)=alat2
	  isec2(16)=0

	  isec2(20)=-90000
	  isec2(21)=0
	  isec2(22)=0

	else if (irg.eq.5)then

	  isec2( 1)=5	!stereographic grid
	  isec2( 6)=0
	  isec2( 7)=lov
	  isec2( 8)=0
	  isec2( 9)=di
	  isec2(10)=dj
	  isec2(13)=ipcf
	  isec2(14)=0
	  isec2(15)=0
	  isec2(16)=0

	else if (irg.lt.0)then

	  iareatmp=abs(irg)
	  sezione2=.false.

	else 
		write(*,*)'fatal error >>Data representation type unknown'
		NUMERR = NUMERR + 1
		GO TO 50
	end if

	if (iarea.ne.255)iareatmp=iarea
	isec1(4)=iareatmp		!grid definizione (sezione 2)

	if (sezione2)then

c              parte comune a tutte le proiezioni
	  isec2(2)=nplon
	  isec2(3)=nplat
	  isec2(4)=rlat0*1000.
	  isec2(5)=rlon0*1000.
	  isec2(11)=nija
	  isec2(12)=0
	  isec2(17)=0
	  isec2(18)=0


c	  if(igrid.eq.0)then    ! 12/4/99
	  if(irg.ne.10)then
		  isec2(19)=0
	  else
		  isec2(19)=8
	  end if
	end if


c	!!!!!!!!!!		sezione 3		!!!!!

	  if (ibitm.ne.0)ibitmtmp=ibitm
	  isec3(1)=ibitmtmp	!0=bit map included
	  isec3(2)=imis		!integer missing value
	  zsec3(2)=rmis		!real missing value

	  if (ibitmtmp.eq.0)then
		  sezione3=.false.
	  else
		  sezione3=.true.
	  end if

c	!!!!!!!!!!		sezione 4		!!!!!!!!!!!!

	if(igrid.eq.0)then
		  klen = nval
		  isec4(1)=nval		!numero valori vettore
	else
		  klen = nplat*nplon
		  isec4(1)=nplat*nplon	!numero valori grigliato regolare
	end if


      nbit=24      ! nbit di default

c				cerca nbit per codifica

	  rmin=rmis
	  rmax=rmis
	  do i=1,nval
		  if(itipdat.eq.0)then
			if(field(i).ne.rmis)then
			  if(rmin.eq.rmis)rmin=field(i)
C			  if(rmin.eq.rmis)rmax=field(i) ! bug until ver 1.04
			  if(rmax.eq.rmis)rmax=field(i)
			  rmin=min(field(i),rmin)
			  rmax=max(field(i),rmax)
			else
			  sezione3=.true.
			end if
		  else
			if(ifield(i).ne.imis)then
			  if(rmin.eq.rmis)rmin=float(ifield(i))
C			  if(rmin.eq.rmis)rmax=float(ifield(i)) Marone?
			  if(rmax.eq.rmis)rmax=float(ifield(i))
			  rmin=min(float(ifield(i)),rmin)
			  rmax=max(float(ifield(i)),rmax)
			else
			  sezione3=.true.
			end if
		  end if
	  end do
CC	end if

	if (rmax.eq.rmis)then
		write(*,*)'fatal error >>tutti i dati sono mancanti'
CC		isec4(1)=-1	!dati mancanti
		NUMERR = NUMERR + 1
		GO TO 50
	end if

      if (comp) then
	csig=0.

	if(ivar(2).eq.1)then
	  csig=cifre(ivar(3),ilev)	!conv. tabella variabili
	end if

	if (csig.eq.0.)then
		nbit=24
cd		print*,'attenzione nbit di default'
	else
	  delta=real(int((rmax-rmin)*real(csig)))+1.	! +1 per approssimazione
	  nbit=alog(delta+1.)/alog(2.)			! log(d+1)/log(2)
	  NBITO=NBIT
	  if(rmax-rmin.eq.0.)goto 34

33	  ZS = (RMAX-RMIN) / (2**(NBIT+1)-1)
	  IF (ZS.NE.0.0) ZS = ALOG(ZS) / ALOG(2.) + 2.
	  ISCALE = MIN (INT(ZS),INT(ZS+SIGN(1.,ZS)))

	  RMAX1=RMIN+2.**real(NBIT)*2.**real(ISCALE)
	  delta=real(int((rmax1-rmin)*real(csig)))+1.	! +1 per approssimazione
	  nbit=alog(delta+1.)/alog(2.)			! log(d+1)/log(2)

	  IF (NBIT.GT.NBITO)THEN
c		  print *,'CAMBIO NBIT ',NBITO,NBIT
		  NBITO=NBIT
		  GOTO 33
	  END IF
	end if
34	NBIT=MIN(MAX(NBIT,NBITO),24)

	if (verbose)then
	  write(*,*)' iv ',ivar(3),' lev ',ilev(2),' max,min ',rmax,rmin
	  write(*,*)' delta ',delta,' nbit ',nbit,
     $         ' nmaxb ',2.**float(nbit)-1.
	  write(*,*)'************** nbit=',nbit,' ***************'
	end if

      end if


	isec4(2)=nbit		!numero di bit used for each packed value
	isec4(3)=0		!grid point data
	isec4(4)=0		!simple packing
	if (itipdat.eq.0)then
		isec4(5)=0	!floating point data
	else
		isec4(5)=32	!integer  point data
	end if
	isec4(6)=0
	isec4(7)=0
	isec4(8)=0
	isec4(9)=0
	isec4(10)=0
	isec4(11)=0
	isec4(12)=0
	isec4(13)=0
	isec4(14)=0
	isec4(15)=0


	if (igrid.eq.0)then
	    call copyvet(FIELD,FIELDw,nval)
	else if (igrid.eq.1)then			!punti h
	  if (itipdat.eq.0)then
	    call copyvet_hfault(FIELD,FIELDw,nplon,nplat,rmis)
	    sezione3=.true.
	  else
	    call copyvet_hfault(IFIELD,FIELDw,nplon,nplat,imis)
	    sezione3=.true.
	  end if
	else if(igrid.eq.2)then
	  if (itipdat.eq.0)then
	    call copyvet_vfault(FIELD,FIELDw,nplon,nplat,rmis)
	    sezione3=.true.
	  else
	    call copyvet_vfault(IFIELD,FIELDw,nplon,nplat,imis)
	    sezione3=.true.
	  end if
	end if


c	!!!!!		gestione sezioni		!!!!

	ipres=0				!section 2 and 3 omitted
	if (sezione2)ipres=ipres+128
	if (sezione3)ipres=ipres+64

	isec1( 5)=ipres

      if (verbose.and.sezione2)WRITE (*,*) 'sezione 2 presente'
      if (verbose.and.sezione3)WRITE (*,*) 'sezione 3 presente'


c	!!!!!!!!!!		finito		!!!!!!!!!!!!

C
C     'C' function to pack entire GRIB message.
C
      YOPER = 'C'

      if (verbose)WRITE (*,9001) YOPER
C
      IERR = 1

      CALL GRIBEX (ISEC0,ISEC1,ISEC2,ZSEC2,ISEC3,ZSEC3,ISEC4,
     + fieldw,KLEN,INBUFF,ILENB,IWORD,YOPER,IERR)

C
C     Check return code.
c
	if (verbose)WRITE (*,9004) IERR
	IF (IERR.GT.0)
     C   THEN
             NUMERR = NUMERR + 1
             GO TO 50
	ENDIF

	if (.not.verbose)goto 565

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
C
c      CALL GRPRS4 (ISEC0,ISEC4,fieldw)

565	continue

	call pbwrite(kunit,inbuff,isec0(1),kret)
	if (kret.eq.-1)goto 320

	GO TO 50
C
C
        
 300    continue
	write (*,*)'input or output file not specified'
	call exit (1)

 301    continue
	write(*,*)'-piii error iii generating process'
	call exit (2)

 307    continue
	write(*,*)'-ejjj error jjj emission centre'
	call exit (2)

 302    continue
	write(*,*)'-akkk error kkk grid definition'
	call exit (3)

 303    continue
	write(*,*)'-blll error lll predetermined bit map'
	call exit (4)

 304    continue
	write(*,*)' error opening file input'
	call exit (5)

 305    continue
	write(*,*)' error opening file output'
	call exit (6)

 306    continue
	write(*,*)' sintax error'
	call exit (7)

 308    continue
	write(*,*)' grid too large. I do not have memory'
	call exit (7)

  330	call pbclose (kunit,kret)
	close (unit=10)
	WRITE (*,*)'Fatal Read error'
	call exit (8)	

  320	call pbclose (kunit,kret)
	close (unit=10)
	WRITE (*,*)'Fatal Write error'
	call exit (9)	

  310	call pbclose (kunit,kret)
	close (unit=10)
	WRITE (*,*)'end of file'
	WRITE (*,9000)
	WRITE (*,9002) NUMERR
	call exit (0)

C
 9000 FORMAT (1H )
 9001 FORMAT (1H ,'GRIBBO : Function code = ',A)
 9002 FORMAT (1H ,'GRIBBO : Number of coding errors = ',I9)
 9004 FORMAT (1H ,'GRIBBO : GRIBEX return code = ',I4)
C
      END




	subroutine cornice(
     $     rlon0HIROT,rlonnHIROT
     $     ,rlat0HIROT,rlatnHIROT
     $     ,IMHV,JMHV,IMJM
     $     ,rlon0HIROTn,rlonnHIROTn
     $     ,rlat0HIROTn,rlatnHIROTn
     $     ,IMHVn,JMHVn,IMJMn,i_hv
     $     ,FIELD,ier)

comstart cornice
c	i_hv=0		punti H
c	i_hv=1		punti V
comend

	real field(imjm)

	iii=0

	it=imhv*4+5
	if=imhv*jmhv-imhv*4
	do i=it,if			!tolgo 4 righe in vertivale

	icol=mod(i,imhv)
	if(icol.ge.0.and.icol.le.4)goto 10
	if(icol.ge.imhv-3.and.icol.le.imhv)goto 10
	
	if(mod(i,2).eq.i_hv)goto 10	!elabora punti H o punti V (pari o di.)
	ii=(i+1)/2			!calcola vecchio indice K
	iii=iii+1			!incrementa nuovo indice K

c	print*,icol,ii,iii
	field(iii)=field(ii)

10	continue
	end do
c					!nuovi parametri area
	imhvn=imhv-8
	jmhvn=jmhv-8
	imjmn=iii
c	imjmn=((imhv+1)/2)*jmhv-jmhv/2

	rlon0HIROTn=
     $       rlon0hirot+(rlonnhirot-rlon0hirot)/real((imhv-1)/4)
	rlonnHIROTn=
     $       rlonnhirot-(rlonnhirot-rlon0hirot)/real((imhv-1)/4)
	rlat0HIROTn=
     $       rlat0hirot+(rlatnhirot-rlat0hirot)/real((jmhv-1)/4)
	rlatnHIROTn=
     $       rlatnhirot-(rlatnhirot-rlat0hirot)/real((jmhv-1)/4)

c	print*,imhv,imhvn,jmhv,jmhvn,imjm,imjmn

	return
	end


	subroutine copyvet_hfault(FIELDh,FIELD1,IMhv,JMhv,fault)
	dimension field1(imhv,jmhv),fieldh(*)

	do j=1,jmhv
	  do i=1,imhv
	    if(mod(i+(j-1)*imhv,2).eq.0)then
		field1(i,j)=fault
	    else
		field1(i,j)=fieldh((i+(j-1)*imhv)/2+1)
	    end if
	  end do
	end do

	return
	end

	subroutine copyvet_vfault(FIELDh,FIELD1,IMhv,JMhv,fault)
	dimension field1(imhv,jmhv),fieldh(*)

	do j=1,jmhv
	  do i=1,imhv
	    if(mod(i+(j-1)*imhv,2).ne.0)then
		field1(i,j)=fault
	    else
cpat1
cpat1		field1(i,j)=fieldh((i+(j-1)*imhv)/2+1)		!17-6-96
		field1(i,j)=fieldh((i+(j-1)*imhv)/2)
cpat1
	    end if
	  end do
	end do

	return
	end

	subroutine copyvet(FIELDh,FIELD1,IMJM)
	dimension field1(imjm),fieldh(*)

	do i=1,imjm
		field1(i)=fieldh(i)
	end do

	return
	end


	FUNCTION Cifre(IV,ILEV)

	dimension csig(255),ilev(3)

c		   1   2   3   4   5   6   7   8   9   10  
	data csig/0.1,0.1,0.1,0.0,0.0,0.1,1.0,1.0,0.0,0.0,	!0
     $       10.,10.,10.,10.,10.,10.,10.,10.,1e-4,0., !1
     $       0.0,0.0,0.0,0.0,10.,0.1,1.0,0.0,0.0,0.0, !2
     $       1.0,10.,10.,10.,0.0,0.0,0.0,0.0,1e3,1e3, !3
     $       1e7,1e7,1e7,1e7,0.0,0.0,1.0,10.,10.,10., !4
     $       1e5,1.0,1e5,10.,1e4,1e4,10.,0.0,1e2,1.0, !5
     $       10.,10.,10.,10.,10.,1e3,1e2,0.0,0.0,0.0, !6
     $       0.2,1.0,1.0,1.0,1.0,10.,0.0,0.0,0.0,0.0, !7
     $       1e2,1e2,1e3,1.0,10.,10.,1.0,1e7,1e4,0.0, !8
     $       1e2,1e3,1.0,10.,10.,10.,1e3,0.0,0.0,0.0, !9
     $       10.,0.0,0.0,1.0,0.0,0.0,1.0,0.0,1.0,0.0, !10
     $       10.,10.,10.,10.,10.,10.,10.,0.0,0.0,0.0, !11
     $       10.,10.,10., 0., 0., 0., 0., 0., 0., !12
     $       126*0./


C			DA DEFINIRE VORTICITA' POTENZIALE (129)
C			ORA 24 BIT

	csigg=csig(iv)

	if(ilev(1).eq.100.and.ilev(2).lt.500)then
				  !umidita` specifica e rapporto di mescolanza
		if(iv.eq.51.or.iv.eq.53)csigg=1.e7
	end if

	cifre=csigg

	RETURN
	END


