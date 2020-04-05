c    <utilita' grib, software di utilita' per  grib>
c    Copyright (C) 2000  <Paolo Patruno SMR ARPA>

c    $Date: 2002/10/08 17:29:16 $    $Revision: 1.21 $
c    $Id: cong.f,v 1.21 2002/10/08 17:29:16 patruno Exp $

c    Questo programma ÅË software  libero; ÅË lecito ridistribuirlo e/o
c    modificarlo secondo i  termini della Licenza Pubblica Generica SMR 
c    come pubblicata  da ARPA SMR ; riferirsi alla versione 1
c    della licenza o (a scelta) ad una versione successiva.

c    Questo programma ÅË distribuito  nella speranza che sia utile,  ma
c    SENZA  ALCUNA GARANZIA;  senza  neppure la  garanzia  implicita di
c    COMMERCIABILITA' o di APPLICABILITÅ¿ PER UN PARTICOLARE SCOPO.  Si
c    veda la Licenza Pubblica Generica SMR per avere maggiori dettagli.

c    Ognuno dovrebbe avere ricevuto una copia della Licenza Pubblica
c    Generica SMR insieme a questo programma; in caso contrario, la si
c    puÅÚ ottenere da Agenzia Regionale Prevenzione e Ambiente (ARPA)
c    Servizio Meteorologico Regionale (SMR), Viale Silvani 6, 40122
c    Bologna, Italia
c    http://www.smr.arpa.emr.it


      PROGRAM cong


comstart cong
c
cidx Programma per la modifica di alcuni semplici parametri del grib
c
cvedi displgrib
cvedi ged
c
c $Date: 2002/10/08 17:29:16 $    $Revision: 1.21 $
c $Id: cong.f,v 1.21 2002/10/08 17:29:16 patruno Exp $
c
c usage:
c
c cong <nomefilein> <nomefileout> [-piii] [-ejjj] [-akkk] [-blll]
c      [-step] [-cfm] [-c2a] [-a2c] [-uv2h] [-destag] [-rnnn] [-v]
c
c <filein>  file input
c <gribout> file output
c -piii     forcing iii generating process
c -ejjj     forcing jjj emission centre
c -akkk     forcing kkk grid definition
c -blll     forcing lll predetermined bit map
c -step     recalculate step grid from extreme
c -cfm      set component flag to m (0/1)
c -c2a      positioning u,v on H grid when C grid
c -a2c      repositioning of u,v from H grid when C grid
c -uv2h     interpolate U/V point variables on H points
c           to use sometimes in conjunction with -c2a
c -destag   destag grid
c -rnnn     rigriglia il campo mediando su nnnXnnn punti
c -comp     comprimi i dati ricalcolando il numero di bit significativi
c -/+"setkey <sintassi comando ged>"  esegue il comando setkey del GED (grib editor)
c                        ed elabora solo i grib corrispndenti a tale chiave 
c                        escludendoli o includendoli                        
c               [data:d1,d2,d3] [ora:o1,o2] [scadenza:s1,s2,s3,s4]
c               [livello:l1,l2,l3] [variabile:v1,v2,v3]
c  Imposta la chiave per la selezione dei grib.
c  Tutti i parametri sono opzionali e le virgole possono essere sostituite
c  da spazi.
c  Tutti i campi non definiti assumono valore "giolli". I campi a sinistra
c  dei due punti possono essere abbreviati fino ad essere rappresentati
c  con un solo carattere; i campi a destra dei due punti sono definiti come
c  segue:
c
c        d1      giorno          )       
c        d2      mese            )
c        d3      anno                    )  emissione
c        o1      ora             )
c        o2      minuti          )
c        s1      indicator of unit of time range (table 4)
c        s2      periodo di tempo 1
c        s3      periodo di tempo 2
c        s4      time range indicator            (table 5)
c        l1      indicator of type of level      (table 3)
c        l2      height, pressure etc. of levels
c        l3      height, pressure etc. of levels
c        v1      identification of originating/generating
c                                         centre (table 0)
c        v2      table 2 version number
c        v3      parameter                       (table 2)
c
c                valore negativo assume significato di "giolli"
c
c        si ppossono inserire commenti facendoli precedere dai
c        caratteri speciali # e !
c
c    Vengono eseguiti prima i test di esclusione (-) (se presenti) 
c    e poi quelli di inclusione (+) (se presenti).
c
c
c -f<nome file>  legge le opzione della funzione setkey dal file
c           specificato
c -afa      set formatted output AFA standard (ascii)
c -v        set verbose mode
c
comend
C
C**** CONG - Program to modify a GRIB file.
C
c   -----------------------------------------------------------------
C
C     Arrays are dimensioned to accommodate T213/N160 data volumes.
C
      PARAMETER (JPACK=250000)
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
      PARAMETER (maxline=200)
      CHARACTER*80 gribin,gribout,par,riga(maxline),rigatmp
     +  ,pa(5)*40,stat*40,setfile*80

c     variabili per local model
      parameter (nvgrc=2)

      dimension ivargu(nvgrc),ivargv(nvgrc)

      data ivargu/33,124/
      data ivargv/34,125/
c     fine variabili per local model


      DATA RMIS/-1.5e21/
      DATA IMIS/32767/
      data kunitin,kunitout/50,51/

      logical verbose,step,c2a,a2c,primo,uv2h,destag
     +  ,key,comp,fset,gribbo,inc,escl,piu,meno,afa

      igenproc=-1
      iemiss=-1
      iarea=-1
      ibitm=-1
      ibitc=-1
      verbose=.false.
      step=.false.
      c2a=.false.
      a2c=.false.
      primo=.true.
      uv2h=.false.
      destag=.false.
      key=.false.
      comp=.false.
      fset=.false.
      afa=.false.
      nl=0
      nrig=1
      gribbo=.false.


      call grsvck (0)
c     call grsdbg (1)
      KPR=0                     !DEBUG PRINT SWITCH
      CALL SETPAR (KBIT,KNEG,KPR)

C
C     Clear error counter.
C
      NUMERR = 0

C
C     Lengths of INBUFF and PSEC4
C
      ILENB = JPACK
      IPUNP = JPACK * 4
      KINLEN= JPACK*(KBIT/8)

      isec3(2)=imis		!missing value
      zsec3(2)=rmis		!missing value

      call getarg(1,gribin)
      call getarg(2,gribout)

      write (*,*) '$Id: cong.f,v 1.21 2002/10/08 17:29:16 patruno Exp $'

      if (gribin.eq.' '.or.gribout.eq.' ')then
        write(*,*)'usage:'
        write(*,*)'cong <nomefilein> <nomefileout>',
     +    ' [-piii] [-ejjj] [-akkk] [-blll] [-step]',
     +    ' [-c2a] [-a2c] [-uv2h] [-destag] [-rnnn] [-comp]',
     +    ' [-/+setkey] [-v] [-f<file>] [-afa]'
        write(*,*)' '
        write(*,*)'<filein>  file input'
        write(*,*)'<gribout> file output'
        write(*,*)'-piii     forcing iii generating process'
        write(*,*)'-ejjj     forcing jjj emission centre'
        write(*,*)'-akkk     forcing kkk grid definition'
        write(*,*)'-blll     forcing lll predetermined bit map'
        write(*,*)'-step     recalculate step grid from extreme'
        write(*,*)'-cfm      set component flag to m (0/1)'
        write(*,*)'-c2a      positioning u,v on H grid when C grid'
        write(*,*)'-a2c      repositioning of u,v from H grid when'
     +    ,' C grid'
        write(*,*)'-uv2h     interpolate U/V point variables on'
     +    ,' H points'
        write(*,*)'          to use sometimes in conjunction with -c2a'
        write(*,*)'-destag   destag grid'
        write(*,*)'-rnnn     rigriglia il campo mediando su nnnXnnn '
     +    ,'punti'
        write(*,*)'-comp     comprimi i dati ricalcolando i bit'
     +    ,' significativi'
        write(*,*)'-/+setkey <sintassi comando ged>'
     +    ,' esegue il comando setkey del GED'
        write(*,*)'-v        set verbose mode'
        write(*,*)'-f<nome file>  legge le opzioni della funzione'
     +    ,' setkey dal file specificato'
        write(*,*)'-afa      set formatted output AFA standard (ascii)'
        write(*,*)' '
        goto 300
      end if


      do i=3,IARGC()
	call getarg(i,par)
        if (par.ne.' ')then
          if(par(1:).eq.'-step'.or.
     +      par(1:).eq.'-STEP')then
            step=.true.
            gribbo=.true.
          else if(par(1:).eq.'-c2a'.or.
     +        par(1:).eq.'-C2A')then
            c2a=.true.
            gribbo=.true.
          else if(par(1:).eq.'-a2c'.or.
     +        par(1:).eq.'-A2C')then
            a2c=.true.
            gribbo=.true.
          else if(par(1:).eq.'-afa'.or.
     +        par(1:).eq.'-AFA')then
            afa=.true.
            gribbo=.true.
          else if(par(1:).eq.'-uv2h'.or.
     +        par(1:).eq.'-UV2H')then
            uv2h=.true.
            gribbo=.true.
          else if(par(1:).eq.'-v'.or.
     +        par(1:).eq.'-V')then
            verbose=.true.
          else if(par(1:).eq.'-destag')then
            destag=.true.
            gribbo=.true.
          else if(par(1:).eq.'-comp')then
            comp=.true.
            gribbo=.true.
          else if(par(2:7).eq.'setkey')then
            key=.true.
            nl=nl+1
            riga(nl)=par
          else if(par(1:2).eq.'-f')then
            key=.true.
            fset=.true.
            setfile=par(3:)
          else if(par(1:2).eq.'-p'.or.
     +        par(1:2).eq.'-P')then
            read (par(3:5),'(i3)',err=301)igenproc
            gribbo=.true.
          else if(par(1:2).eq.'-e'.or.
     +        par(1:2).eq.'-E')then
            read (par(3:5),'(i3)',err=307)iemiss
            gribbo=.true.
          else if(par(1:2).eq.'-a'.or.
     +        par(1:2).eq.'-A')then
            read (par(3:5),'(i3)',err=302)iarea
            gribbo=.true.
          else if(par(1:2).eq.'-b'.or.
     +        par(1:2).eq.'-B')then
            read (par(3:5),'(i3)',err=303)ibitm
            gribbo=.true.
          else if(par(1:3).eq.'-cf'.or.
     +        par(1:2).eq.'-CF')then
            read (par(4:4),'(i1)',err=303)ibitc
            gribbo=.true.
          else if(par(1:2).eq.'-r'.or.
     +        par(1:2).eq.'-R')then
            read (par(3:5),'(i3)',err=303)nrig
            gribbo=.true.
          else
            write(*,*)'error in parameter ',par
            write(*,*)'usage:'
            write(*,*)'cong <filein> <gribout>',
     +        ' [-piii] [-ejjj] [-akkk] [-blll] [-step] [-cfm]',
     +        ' [-c2a] [-a2c] [-uv2h] [-destag] [-comp]' ,
     +        ' [-/+setkey] [-v] [-f<file>] [-afa]'
            write(*,*)' '
            call exit (4)
          end if
        end if
      end do
      
      call pbopen(kunitin,gribin,'r',kret)
      if(kret.ne.0)goto 300

      if (afa)then
c			file ascii afa
	open (unit=kunitout,file=gribout,
     +    status='unknown',FORM='FORMATTED'
     +    ,iostat=kret)
      else
        call pbopen(kunitout,gribout,'w',kret)
      end if
      if(kret.ne.0)goto 300

      if (key.and.fset)then
        open (unit=33,file=setfile,status='old',err=304)
 811    read (33,fmt='(a80)',end=812,err=812)riga(nl+1)
        if (riga(nl+1).eq.' ')goto 811
        nl=nl+1
        if (nl.eq.maxline+1)goto 305
        if(verbose)write (*,*)'opzioni setkey. riga=',nl
     +    ,' opz=',riga(nl)
        goto 811
 812    close (unit=33)
      end if

 50   CONTINUE

      call pbgrib(kunitin,inbuff,kinlen,isiz,kret)
      if (kret.eq.-1)goto 310
      if (kret.ne.0)goto 300
      
c	print*,isiz,' this is the lenght of grib read'
      
C     'D' function to unpack entire GRIB message.
C
      YOPER = 'D'
      if (verbose)then
        WRITE (*,9000)
        WRITE (*,9001) YOPER
      end if
C
      IERR = 1
      CALL GRIBEX (ISEC0,ISEC1,ISEC2,ZSEC2,ISEC3,ZSEC3,ISEC4,
     C             ZSEC4,IPUNP,INBUFF,ILENB,IWORD,YOPER,IERR)
C
C     Check return code.
c
      if (verbose)then
        WRITE (*,9004) IERR
      end if
      
      IF (IERR.EQ.-6) WRITE (*,*) ' GRDEMO : Pseudo-grib data found.'
      IF (IERR.GT.0)
     C  THEN
        NUMERR = NUMERR + 1
        GO TO 50
      ENDIF
      ngribread=ngribread+1

      if (.not.verbose)goto 30
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
     C  THEN
        WRITE (*,9000)
        WRITE (*,*) ' GRDEMO : No section 2 in GRIB message.'
      ELSE
        CALL GRPRS2 (ISEC0,ISEC2,ZSEC2)
      ENDIF
C
      IF (ISEC1(5).EQ.0.OR.ISEC1(5).EQ.128)
     C  THEN
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
 30   continue

      if(iemiss.ge.0) isec1(2)=iemiss
      if(igenproc.ge.0)isec1(3)=igenproc
      if(iarea.ge.0)  isec1(4)=iarea
      if(ibitm.ge.0)  isec3(1)=ibitm
      if(ibitc.ge.0.and.(isec2(1).lt.50.or.isec2(1).gt.80))
     +  isec2(19)=ibitc

      if (nrig.GT.1) call rigriglia
     $     (nrig,isec2,isec4(5),isec3,zsec3,isec4,zsec4,izsec4)

      if(step)then
        isec2(6)=128
        isec2(9)=abs(nint((isec2(5)-isec2(8))/float(isec2(2)-1)))
        isec2(10)=abs(nint((isec2(4)-isec2(7))/float(isec2(3)-1)))
      end if
        
      if(c2a)then

        if (primo)then

          do i=1,nvgrc

            if (isec1(6).eq.ivargu(i).or.isec1(6).eq.ivargv(i))then
              goto 50
            endif
              
          end do

          PRINT*,'TROVATO GRID PUNTI H memorizzo e ricomincio'
          primo=.false.
            
          la4=isec2(4)
          lo5=isec2(5)
          la7=isec2(7)
          lo8=isec2(8)
            
          call pbseek (kunitin,0,0,kret)
            
          if (kret.lt.0)then
            print*,'errore pbseek'
            goto 300
          end if
            
          goto 50
          
        else
          do i=1,nvgrc
            if(isec1(7).ne.105)then
              if (isec1(6).eq.ivargu(i))then
c                  idlon=nint(float(isec2(8)-isec2(5))/float(isec2(2)-1)/2.)
c                  isec2(8)=isec2(8)-idlon
c                  isec2(5)=isec2(5)-idlon
                isec2(5)=lo5
                isec2(8)=lo8
              else if (isec1(6).eq.ivargv(i))then
c                  idlat=nint(float(isec2(7)-isec2(4))/float(isec2(3)-1)/2.)
c                  isec2(7)=isec2(7)-idlat
c                  isec2(4)=isec2(4)-idlat
                isec2(4)=la4
                isec2(7)=la7
              end if
            end if
          end do
        end if
      end if



c       applico i principi di selezione

      if (key)then

        meno=.false.
        piu=.false.
        escl=.false.
        inc=.false.

        do il=1,nl

          rigatmp=riga(il)
c         elaborazione chiave di ricerca
          call spezza(rigatmp,stat,ier)
          if(ier.ne.0)goto 5
          if  (stat(1:1).ne.'-'.and.stat(1:1).ne.'+')then
            print*, 'errore: usare il segno - o + prima di setkey per',
     +        ' escludere o selezionare'
            stop
          end if 

          np=0
        
 20       call spezza(rigatmp,pa(np+1),ier)
          if(ier.ne.0)goto 10
          np=np+1
          if (np.gt.5)then
            print *,'errore numero di parametri in setkey'
            stop
          end if
          goto 20
        
 10       continue
          
          if(verbose)print *,'parametri di setkey: ',(pa(i),i=1,np)
        
          call setkey (pa,0,ier)         !reset key
          call setkey (pa,np,ier)
          if (ier.ne.0)then
            print *,'errore sintassi in setkey'
            stop
          end if

          call testkeys (inbuff,isec0(1),ier)

          if (stat(1:1).eq.'-') then
            meno=.true.
            if (ier.eq.0) then
              if (verbose)write(*,*)'campo da escludere'
              escl=.true.
            end if
          end if

          if(stat(1:1).eq.'+')then
            piu=.true.
            if(ier.eq.0)then
              if(verbose)write(*,*)'campo da includere'
              inc=.true.
            end if
          end if

 5      end do  
        if (meno.and.escl)goto 50
        if (piu.and..not.inc)goto 50

      end if


      if(a2c)then
        do i=1,nvgrc
          if(isec1(7).ne.105)then
            if (isec1(6).eq.ivargu(i))then
              idlon=nint(float(isec2(8)-isec2(5))/
     +          float(isec2(2)-1)/2.)
              isec2(8)=isec2(8)+idlon
              isec2(5)=isec2(5)+idlon
            else if (isec1(6).eq.ivargv(i))then
              idlat=nint(float(isec2(7)-isec2(4))/
     +          float(isec2(3)-1)/2.)
              isec2(7)=isec2(7)+idlat
              isec2(4)=isec2(4)+idlat
            end if
          end if
        end do
      end if


      if(uv2h)then
         do i=1,nvgrc
            if(isec1(7).ne.105)then
               if (isec1(6).eq.ivargu(i))then
                  CALL c2agrid(isec2(2),isec2(3),1,zsec4,zsec4,kret)
                  if (kret.ne.0) goto 306
               else if (isec1(6).eq.ivargv(i))then
                  CALL c2agrid(isec2(2),isec2(3),2,zsec4,zsec4,kret)
                  if (kret.ne.0) goto 306
               end if
            end if
         end do
      end if
      

      if (destag)then
        if (isec4(5).eq.0)then
          write (*,*) 'destag'
          call destagFAULT(zsec4,isec2(2),isec2(3),rmis)
        else
          call destagFAULT(zsec4,isec2(2),isec2(3),imis)
        end if
      end if

      if (afa)then

        call w_ascii_str
     +    (kunitout,gribin,isec1,isec2,zsec2,isec3,zsec3,isec4,
     +    null,zsec4,ier)

      else
        
        if (gribbo)then
          if (comp)then
            call bitsig (zsec4,isec4(1),rmis,isec1(2),isec1(1),isec1(6)
     +        ,isec1(7),isec1(8),isec1(9),nbit)
            if(nbit.ne.0)isec4(2)=min(isec4(2),nbit)
            if (verbose)then
              print*,'************** nbit=',isec4(2),' ***************'
            end if
          end if

          YOPER = 'C'
          if (verbose)then
            WRITE (*,9001) YOPER
          end if
          
          IERR = 1
          CALL GRIBEX (ISEC0,ISEC1,ISEC2,ZSEC2,ISEC3,ZSEC3,ISEC4,
     C      ZSEC4,IPUNP,INBUFF,ILENB,IWORD,YOPER,IERR)
C
C     Check return code.
c
        end if
        
        if (verbose)then
          WRITE (*,9004) IERR
        else
          if (ierr.gt.0)then
            WRITE (*,9004) IERR
            NUMERRc = NUMERRc + 1
          end if
        end if
      end if

      if (ierr.le.0)then
        ngribwrite=ngribwrite+1
        if (.not.afa) call pbwrite (kunitout,inbuff,isec0(1),kret)
      end if

      GO TO 50
C
C
 300  STOP 'Read error'
 310  call pbclose (kunitin,kret)
      if (afa)then
         close(kunitout)
      else
         call pbclose (kunitout,kret)
      endif

      WRITE (*,9000)
      WRITE (*,9005)ngribread,ngribwrite
      WRITE (*,9002) NUMERR
      WRITE (*,9003) NUMERRc
      STOP 'end of file'
C
 9000 FORMAT (1H )
 9001 FORMAT (1H ,'CONG : Function code = ',A)
 9002 FORMAT (1H ,'CONG : Number of decoding errors = ',I9)
 9003 FORMAT (1H ,'CONG : Number of coding errors = ',I9)
 9004 FORMAT (1H ,'CONG : GRIBEX return code = ',I4)
 9005 FORMAT (1H ,'CONG : Number of grib: read = ',I9,' write=',i9)
C
 301  continue
      write(*,*)'-piii error iii generating process'
      call exit (2)
        
 307  continue
      write(*,*)'-ejjj error jjj emission centre'
      call exit (2)
        
 302  continue
      write(*,*)'-akkk error kkk grid definition'
      call exit (3)
        
 303  continue
      write(*,*)'-blll error lll predetermined bit map'
      call exit (4)

 304  continue
      write(*,*)'-f<file> error opening file ',setfile
      call exit (5)

 305  continue
      write(*,*)'-f<file> error reading file: too many lines'
      call exit (5)

 306  continue
      write(*,*)"errore in c2agrid numero= ",kret
      call exit (6)

      END


      subroutine bitsig (field,imjm,fault,iv1,iv2,iv3,
     +  ilev1,ilev2,ilev3,nbit)
c     nbit		numero di bit used for each packed value

      dimension field(imjm)

      call CONVwmo(IV1,iv2,iv3,ilev1,ilev2,ilev3,csig) !conversione tabella variabili

c				cerca nbit per codifica
      nbit=0

      if (csig.eq.0.)then
        print*,'attenzione: nbit di default senza '
     +    ,'ulteriore compressione'
      else

        do i=1,imjm
          if(field(i).ne.fault)then
            rmin=field(i)
            rmax=field(i)
            goto 10
          end if
        end do
      
 10     do i=1,imjm
          if(field(i).ne.fault)then
            rmin=min(field(i),rmin)
            rmax=max(field(i),rmax)
          end if
        end do

        delta=real(int((rmax-rmin)*real(csig)))+1. ! +1 per approssimazione
        nbit=alog(delta+1.)/alog(2.) ! log(d+1)/log(2)
        NBITO=NBIT
        if(rmax-rmin.eq.0.)goto 34
        
 33     ZS = (RMAX-RMIN) / (2**(NBIT+1)-1)
        IF (ZS.NE.0.0) ZS = ALOG(ZS) / ALOG(2.) + 2.
        ISCALE = MIN (INT(ZS),INT(ZS+SIGN(1.,ZS)))
        
        RMAX1=RMIN+2.**real(NBIT)*2.**real(ISCALE)
        delta=real(int((rmax1-rmin)*real(csig)))+1. ! +1 per approssimazione
        nbit=alog(delta+1.)/alog(2.) ! log(d+1)/log(2)
        
        IF (NBIT.GT.NBITO)THEN
c		  print *,'CAMBIO NBIT ',NBITO,NBIT
          NBITO=NBIT
          GOTO 33
        END IF
 34     NBIT=MIN(MAX(NBIT,NBITO),24)
      end if
      
c      print *,'iv',iv,' lev',ilevel,' max',rmax,' min',rmin
c      print *,'delta',delta,' nbit',nbit,' nmaxb',2.**float(nbit)-1.
c      print*,'************** nbit=',nbit,' ***************'

      return
      end


      subroutine CONVwmo(iv1,iv2,iv3,ilev1,ilev2,ilev3,csigg)
      
      dimension ivar(255),RM(255),RQ(255),csig(255)
      
c		   1   2   3   4   5   6   7   8   9   10  
      data csig/0.01,0.01,0.1,0.0,0.0,0.1,1.0,1.0,0.0,0.0, !0
     +  10.,10.,10.,10.,10.,10.,10.,10.,1e-4,0., !1
     +  0.0,0.0,0.0,0.0,10.,0.1,1.0,0.0,0.0,0.0, !2
     +  1.0,10.,10.,10.,0.0,0.0,0.0,0.0,1e3,1e3, !3
     +  1e7,1e7,1e7,1e7,0.0,0.0,1.0,10.,10.,10., !4
     +  1e5,1.0,1e5,10.,1e4,1e4,10.,0.0,1e2,1.0, !5
     +  10.,10.,10.,10.,10.,1e3,1e2,0.0,0.0,0.0, !6
     +  0.2,0.2,0.2,0.2,0.2,10.,0.0,0.0,0.0,0.0, !7
     +  1e2,1e2,1e3,1.0,10.,10.,1.0,1e7,1e4,0.0, !8
     +  1e2,1e3,1.0,10.,10.,10.,1e3,0.0,0.0,0.0, !9
     +  10.,0.0,0.0,1.0,0.0,0.0,1.0,0.0,1.0,0.0, !10
     +  10.,10.,10.,10.,10.,10.,10.,0.0,0.0,0.0, !11
     +  10.,10.,10., 0., 0., 0., 0., 0., 0.,10., !12
     +  125*0./

C			DA DEFINIRE VORTICITA' POTENZIALE (129)
C			ORA 24 BIT

      csigg=0.

      if(iv2.gt.127)return

      csigg=csig(iv3)
      
      itop=500                  !pressure level
      if (ilev1.eq.107.or.ilev1.eq.119) itop=5000 !sigma o eta level circa perche' non ho ptop
      
      if((ilev1.eq.100.or.ilev1.eq.107.or.ilev1.eq.119)
     +  .and.ilev2.lt.itop)then !umidita` specifica e rapporto di mescolanza
        if(iv3.eq.51.or.iv3.eq.53) csigg=1.e7
      end if
      
      RETURN
      END


	subroutine w_ascii_str
     $     (nun,dsname,isez1,isez2,rsez2,isez3,rsez3,isez4
     $     ,iout,rout,ier)
c
c	scrivi in formato ascii a valori adiacenti sull'unita' nun aperta col
c	FORTRAN formatted (steam_lf).
c
c	Il primo ritorno e' fatto se il tipo della proiezione non e' 0 o 10
c	o se manca la sez 2.
c
c	Aggiunto dopo rmd e imd uni 0 per compatibilita' SMR
c
	character*(*)	dsname
	integer		nun,imd
	integer		isez1(*),isez2(*),isez3(*),isez4(*),iout(*)
	real		rmd,rsez2(*),rsez3(*),rout(*)
	character*80	outv
	character*30    tmp
c
	rmd=rsez3(2)
	imd=isez3(2)
	if (isez3(1).eq.0)igrid=0
	if (isez3(1).gt.0)igrid=1
	if (isez3(1).gt.100)igrid=2
c
c	record 0 - dataset name
c
	write(nun,'(a)',err=9000) dsname
c
c	record 1 - Data di emissione
c
	write(nun,'(i4.4 ,4i3.2)',err=9000)
     +		(isez1(21)-1)*100+isez1(10),(isez1(k),k=11,14)
c
c	record 2 - Scadenza
c
	write(nun,'(i3.3,i6.5,2i4.3)',err=9000)
     +		(isez1(k),k=15,18)
c
c	record 3 - Livello
c
	write(nun,'(i3.3,i6.5,i6.3)',err=9000)
     +		(isez1(k),k=7,9)
c
c	record 4 - Variabile estesa
c		   Centro emittente/tabella/parametro/0/0/0
c	oppure se Centro emittente=98 (ECMWF) e flag di local usage
c	non e' zero, al posto dei tre zeri sono riportati
c	i contenuti delle seguenti tre parole del vettore di sez 1
c	restituito da GRIBEX (vedere le specifiche)
c
c	isez1(37)	specifica dell'uso ECMWF
c	isez1(39)	tipo del dato
c	isez1(42)	progressivo
c
	if (isez1(2).ne.98.or.isez1(24).eq.0) then
	  write(nun,'(i3.3,5i4.3)',err=9000)
     +		isez1(2),isez1(1),isez1(6),0,0,0
	else
	  write(nun,'(i3.3,5i4.3)',err=9000)
     +		isez1(2),isez1(1),isez1(6),isez1(37),isez1(39),isez1(42)
	endif
c
c	Caratteristiche di area
c
	if (iand(isez1(5),128).eq.0) then
	  nulo=0
	  nvla=0
	  rlam1=0.
	  rlom1=0.
	  rlam2=0.
 	  rlom2=0.
	  plon=0.
	  plat=0.
	  isez2(1)=-isez1(4)	  
	  isez2(11)=0
	else 
	  if (isez2(1).ne.3) then
	    nulo=isez2(2)
	    nvla=isez2(3)
	    rlam1=float(isez2(4))/1000.
	    rlom1=float(isez2(5))/1000.
	    rlam2=float(isez2(7))/1000.
 	    rlom2=float(isez2(8))/1000.
	    plon=float(isez2(9))/1000.
	    plat=float(isez2(10))/1000.
	  else
	    nulo=isez2(2)
	    nvla=isez2(3)
	    rlam1=float(isez2(4))/1000.
	    rlom1=float(isez2(5))/1000.
	    rlam2=0.
 	    rlom2=0.
	  endif
	endif
c
	if (isez2(1).ne.10) then
c
c	caso aree geografiche pure e altre casistiche
c
	  write (nun,'(i3)',err=9000) isez2(1)
	  if (isez2(1).eq.3) then
             write (nun,'(i4,3f9.3)') isez2(13),float(isez2(7))/1000.,
     $            float(isez2(14))/1000.,float(isez2(15))/1000.
	    write (nun,'(2f9.3,2i9)',err=9000) rlam1,rlam2,isez2(9),nvla
	    write (nun,'(2f9.3,2i9)',err=9000) rlom1,rlom2,isez2(10),nulo
	  else if(isez2(1).ge.0)then
	    write (nun,'(3f9.3,i9)',err=9000) rlam1,rlam2,plat,nvla
	    write (nun,'(3f9.3,i9)',err=9000) rlom1,rlom2,plon,nulo
	  endif
	else
c
c	caso aree ruotate
c
	  polat=float(isez2(13))/1000.
	  polon=float(isez2(14))/1000.
	  call rot_grib_LAMBO (polon,polat,TLM0D,TPH0D)
	  stph0=sind(tph0d)
	  ctph0=cosd(tph0d)
	  write (nun,'(i3)') isez2(1)
	  WRITE (nun,'(3f9.3)')TPH0D,TLM0D,rsez2(1)
cc	  call RTLLD(rlom1,rlam1,tlm0d,ctph0,stph0,alom1,alam1)
cc	  call RTLLD(rlom2,rlam2,tlm0d,ctph0,stph0,alom2,alam2)
cc	  write (nun,'(3f9.3,i9,2f9.2)',err=9000)
cc   +		rlam1,rlam2,plat,nvla,alam1,alam2
cc	  write (nun,'(3f9.3,i9,2f9.2)',err=9000)
cc   +		rlom1,rlom2,plon,nulo,alom1,alom2
	  write (nun,'(3f9.3,i9)',err=9000)
     +		rlam1,rlam2,plat,nvla
	  write (nun,'(3f9.3,i9)',err=9000)
     +		rlom1,rlom2,plon,nulo
	endif
c
c	loop di scrittura delle cinquine considerando il tipo di valori
c	e riordinando i valori.
c
c
c	record di controllo
c
	if (isez4(1).lt.0) then
	  write (nun,*,err=9000) 0,0,rmd,imd,0
	else
c	  k='80'x
	  k=128
	  outv=' '
	  do j=1,3
	    outv(j+1:j+1)='0'
	    if (iand(isez2(11),k).ne.0) outv(j+1:j+1)='1'
	    k=k/2
	  enddo
	  write (nun,*,err=9000) isez4(1),outv(:5),isez4(5),rmd,imd,igrid

	  
	  if (isez4(5).ne.0) then
	  	write (nun,*) (iout(j),j=1,isez4(1))
	  else
		write (nun,*) (rout(j),j=1,isez4(1))
	  end if



c	  outv=' '
c	  l=1
c	  do j=1,isez4(1)
c	    if (isez4(5).ne.0) then
c	      write (outv(l:l+15),*,iostat=ker) iout(j)
c	    else
c		write (tmp,*)rout(j)
c		do k=1,30
c		  if(tmp(31-k:31-k).ne.' ')k1=31-k
c		  if(index(tmp,'E').eq.0)then
ccd			print*,'reale'
c		    if(tmp(k:k).ne.' '.and.
c	1	    tmp(k:k).ne.'0')k2=k
c		  else
ccd			print*,'esponenziale'
c		    if(tmp(k:k).ne.' ')k2=k
c		  end if
c		end do
c		inc=k2-k1+1
ccd		print*,tmp(k1:k2)
ccd		print*,k1,k2
c	        write (outv(l:l+inc),'(1x,a)',iostat=ker) tmp(k1:k2)
c	    endif
c	    if (mod(j,3).eq.0) then
c	      write(nun,'(a)',err=9000) outv(:l+inc)
c	      l=1
c	      outv=' '
c	    else
c	      l=l+inc+1
c	    endif
c	  enddo
c	  if (outv.ne.' ') write(nun,'(a)',err=9000) outv
	endif



c
c	finito in gloria
c
cpat
	ier=0
cpat
	return
c
c	finito malamente
c
cpat
9000	ier=1
	return
cpat
	end

      SUBROUTINE rigriglia(nrig,isec2,eint,isec3,zsec3,isec4,zsec4,
     $     izsec4)
      INTEGER nrig,isec2(*),eint,isec3(*),isec4(*),izsec4(*)
      REAL zsec3(*),zsec4(*)

      INTEGER i,j,k,l,nmed,io,im
      REAL med,xst,yst

      IF (nrig .LE.1) RETURN

      IF (isec2(1).NE.0.AND.isec2(1).NE.4.AND.isec2(1).NE.10) THEN
         PRINT*,'Attenzione, rigrigliatura impossibile con drt ',
     $        isec2(1)
         RETURN
      ENDIF

      IF (MOD(isec2(11),64).GE.32) THEN
         PRINT*,'Attenzione, rigrigliatura impossibile con scanning ',
     $        'mode ',isec2(11)
         RETURN
      ENDIF

      IF (eint.EQ.0) THEN
C Dati reali
         im=0
         DO j=1,isec2(3),nrig
            DO i=1,isec2(2),nrig
               med=0.
               nmed=0
               im=im+1
C Faccio la media su nrigxnrig punti
               DO l=j,MIN(j+nrig-1,isec2(3))
                  DO k=i,MIN(i+nrig-1,isec2(2))
                     io=(l-1)*isec2(2)+k
                     IF (zsec4(io).NE.zsec3(2)) THEN
                        med=med+zsec4(io)
                        nmed=nmed+1
                     ENDIF
                  ENDDO
               ENDDO
C im < io, garantito, posso sporcare
               IF (nmed.GT.0) THEN
                  zsec4(im)=med/nmed
               ELSE
                  zsec4(im)=zsec3(2)
               ENDIF
            ENDDO
         ENDDO
      ELSE
C Dati interi
         im=0
         DO j=1,isec2(3),nrig
            DO i=1,isec2(2),nrig
               med=0.
               nmed=0
               im=im+1
C Faccio la media su nrigxnrig punti
               DO l=j,MIN(j+nrig-1,isec2(3))
                  DO k=i,MIN(i+nrig-1,isec2(2))
                     io=(l-1)*isec2(2)+k
                     IF (izsec4(io).NE.isec3(2)) THEN
                        med=med+izsec4(io)
                        nmed=nmed+1
                     ENDIF
                  ENDDO
               ENDDO
               IF (nmed.GT.0) THEN
                  izsec4(im)=med/nmed
               ELSE
                  izsec4(im)=isec3(2)
               ENDIF
            ENDDO
         ENDDO
      ENDIF
C Correggo la sezione 2
      PRINT*,'Prima',(isec2(i),i=2,5),(isec2(i),i=7,8)
C Calcolo il vecchio passo di griglia con segno
      xst=(isec2(8)-isec2(5))/float(isec2(2)-1)
      yst=(isec2(7)-isec2(4))/float(isec2(3)-1)
C Sposto le coordinate del primo punto
      isec2(5)=isec2(5)+(nrig-1)*0.5*xst
      isec2(4)=isec2(4)+(nrig-1)*0.5*yst
C Ricalcolo il numero di punti di griglia
      isec2(2)=(isec2(2)+nrig-1)/nrig
      isec2(3)=(isec2(3)+nrig-1)/nrig
C Ricalcolo il nuovo passo di griglia con segno
      xst=xst*nrig
      yst=yst*nrig
C Ricalcolo le coordinate dell'ultimo punto
      isec2(8)=isec2(5)+xst*(isec2(2)-1)
      isec2(7)=isec2(4)+yst*(isec2(3)-1)
C Aggiorno il passo di griglia se richiesto
      IF (isec2(6).EQ.128) THEN
         isec2(9)=abs(xst)
         isec2(10)=abs(yst)
      ENDIF
C Correggo la sezione 4
C Nuovo numero di punti totali
      isec4(1)=isec2(2)*isec2(3)
      IF (isec4(1).NE.im) THEN
         PRINT*,'Errore interno grave in rigrigliatura ',isec4(1),im
         STOP
      ENDIF
      PRINT*,'Dopo',(isec2(i),i=2,5),(isec2(i),i=7,8)
      END










