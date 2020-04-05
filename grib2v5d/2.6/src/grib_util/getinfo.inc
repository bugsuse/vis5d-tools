
c    <grib_util, software di utilita' per  grib, API layer>
c    Copyright (C) 2000  <Paolo Patruno SMR ARPA>
c
c    $Date: 2006-08-24 12:15:57 +0200(gio, 24 ago 2006) $    $Revision: 99 $
c    $Id: getinfo.inc 99 2006-08-24 10:15:57Z cesari $

c    Questo programma ÅË software  libero; ÅË lecito ridistribuirlo e/o
c    modificarlo secondo i  termini della Licenza Pubblica Generica SMR
c    come pubblicata  da ARPA SMR ; riferirsi alla versione 1
c    della licenza o (a scelta) ad una versione successiva.

c    Questo programma ÅË distribuito  nella speranza che sia utile,  ma
c    SENZA  ALCUNA GARANZIA;  senza  neppure la  garanzia  implicita di
c    COMMERCIABILITA' o di APPLICABILITA' PER UN PARTICOLARE SCOPO.  Si
c    veda la Licenza Pubblica Generica SMR per avere maggiori dettagli.

c    Ognuno dovrebbe avere ricevuto una copia della Licenza Pubblica
c    Generica SMR insieme a questo programma; in caso contrario, la si
c    puÅÚ ottenere da Agenzia Regionale Prevenzione e Ambiente (ARPA)
c    Servizio Meteorologico Regionale (SMR), Viale Silvani 6, 40122
c    Bologna, Italia
c    http://www.smr.arpa.emr.it


      KPR=0	!DEBUG PRINT SWITCH
      CALL SETPAR (KBIT,KNEG,KPR) !number of bit in computer word

      kinlen=idimg*kbit/8

      if (iug.eq.-1)goto 100
      call pbgrib(iug,grib,kinlen,koutlen,ier)

      if(ier.eq.-1)then
cd		print *,'termino per end of file'
      	return
      else if(ier.ne.0)then
      	print*, 'errore lettura grib (pbgrib)',ier	!error
      	return
      end if

100   continue

C     'I' function to unpack SECTION 0,1,2 only.
      ier=1
      CALL GRIBEX (ISEC0,ISEC1,ISEC2,ZSEC2,ISEC3
     +	,ZSEC3,ISEC4
     +	,ZSEC4,IPUNP,grib,idimg,IWORD,'I',IER)
C
C     Check return code.
c
CD	WRITE (*,9004) IER
      IF (IER.EQ.-6) WRITE (*,*) ' GRDEMO : Pseudo-grib data found.'
      IF (IER.GT.0)then
      	print*, 'errore degribbing (gribex)',ier
      	return
      end if

      ija=isec2(11)

      data(1)=isec1(12)
      data(2)=isec1(11)
      data(3)=isec1(10)+(isec1(21)-1)*100

      ora(1)=isec1(13)
      ora(2)=isec1(14)

      var(1)=isec1(2)
      var(2)=isec1(1)
      var(3)=isec1(6)

      level(1)=isec1(7)
      level(2)=isec1(8)
      level(3)=isec1(9)

      scad(1)=isec1(15)
      scad(2)=isec1(16)
      scad(3)=isec1(17)
      scad(4)=isec1(18)

      if(isec1(5).lt.128)then
      	ier=8000		!sezione 2 omessa
      	return
      end if

      idrt=isec2(1)

      if(idrt.eq.0.or.idrt.eq.4)then
      	alorot=0.		! parametri da elaborare da grib
      	alarot=0.
      	rot=0
      else if(idrt.eq.10.or.idrt.eq.14)then		
      	ALAROT=FLOAT(ISEC2(13))/1000.
      	ALOROT=FLOAT(ISEC2(14))/1000.
      	rot=zsec2(1)
      else if (idrt .gt. 14) then
      	print*,'error data representation type code table 6 unsupported'
      	ier=7000
      	return
      end if

      if (idrt .eq. 0 .or. idrt .eq. 1 .or. idrt .eq. 4 .or.
     +  idrt .eq. 10 .or. idrt .eq. 14) THEN
        if (ISEC2(6).EQ.0) THEN
          di=FLOAT(ABS(isec2(8)-isec2(5)))/(FLOAT(isec2(2)-1)*1000.)
          dj=FLOAT(ABS(isec2(7)-isec2(4)))/(FLOAT(isec2(3)-1)*1000.)
        else
          di=ISEC2(9)/1000.	!parametri elaborati da grib
          dj=ISEC2(10)/1000.
        endif
        ala1=isec2(4)/1000.
        alo1=isec2(5)/1000.
        ala2=isec2(7)/1000.
        alo2=isec2(8)/1000.
      endif
      ni=ISEC2(2)	!parametri elaborati da grib
      nj=ISEC2(3)

