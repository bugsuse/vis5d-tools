
c    <grib_util software di utilita' per  grib>
c    Copyright (C) 2000  <Paolo Patruno SMR ARPA>

c    $Date: 2003-02-11 19:32:58 +0100(mar, 11 feb 2003) $    $Revision: 58 $
c    $Id: findgrib.f 58 2003-02-11 18:32:58Z patruno $

c    Questo programma è software  libero; è lecito ridistribuirlo e/o
c    modificarlo secondo i  termini della Licenza Pubblica Generica SMR
c    come pubblicata  da ARPA SMR ; riferirsi alla versione 1
c    della licenza o (a scelta) ad una versione successiva.

c    Questo programma è distribuito  nella speranza che sia utile,  ma
c    SENZA  ALCUNA GARANZIA;  senza  neppure la  garanzia  implicita di
c    COMMERCIABILITA' o di APPLICABILITÀ PER UN PARTICOLARE SCOPO.  Si
c    veda la Licenza Pubblica Generica SMR per avere maggiori dettagli.

c    Ognuno dovrebbe avere ricevuto una copia della Licenza Pubblica
c    Generica SMR insieme a questo programma; in caso contrario, la si
c    può ottenere da Agenzia Regionale Prevenzione e Ambiente (ARPA)
c    Servizio Meteorologico Regionale (SMR), Viale Silvani 6, 40122
c    Bologna, Italia
c    http://www.smr.arpa.emr.it


      subroutine findgrib(iug,grib,idimg
     +	,data,ora,scad,level,var,ier)

COMSTART FINDGRIB
c	subroutine findgrib(iug,grib,idimg
c	1	,data,ora,scad,level,var,ier)
c
c
c	Ricerca all'interno di un file aperto con PBOPEN i grib
c	specificati dalla chiave data,ora,scad,level,var.
c
c	input:
c
c	iug		I	unita` restituita da PBOPEN
c	data(1)		I	giorno		)	
c	data(2)		I	mese		)
c	data(3)		I	anno			)  emissione
c	ora(1)		I	ora		)
c	ora(2)		I	minuti		)
c	scad(1)		I	indicator of unit of time range	(table 4)
c	scad(2)		I	periodo di tempo 1
c	scad(3)		I	periodo di tempo 2
c				(nel caso sia definita solo p1 o p2
c				viene testato solo il max tra p1 e p2)
c	scad(4)		I	time range indicator 		(table 5)
c	level(1)	I	indicator of type of level	(table 3)
c	level(2)	I	height, pressure etc. of levels
c	level(3)	I	height, pressure etc. of levels
c	var(1)		I	identification of originating/generating
c				 centre  (table 0)
c	var(2)		I	table 2 version number
c	var(3)		I	parameter			(table 2)
c
c	output:
c
c	grib(idimg)	I	buffer di debosito del grib estratto	
c	ier		I	codice errore
c				=0 tutto o.k.
c				=-1 grib not found
c				altri > vedi errori 	getinfo
comend

        dimension	grib(idimg)
      integer level(3),var(3),scad(4),data(3),ora(2)
      integer	levelg(3),varg(3),scadg(4),datag(3),orag(2)

      ier=0

      igiro=0
20    continue

      call getinfo (iug,grib,idimg,datag,orag,scadg,levelg,varg,
     +	alat1,alat2,alon1,alon2,ny,nx,
     +	dy,dx,idrt,alarot,alorot,rot,ija,ier)

      if (ier.eq.-1)goto 99
      if (ier.ne.0)return

c      write(*,*)'found  lev',levelg(2),' var',varg(3)
c     1 ,' scad',scadg(3),datag,orag

      do i=1,3
        if (data(i).lt.0)goto 101
        if (data(i).ne.datag(i))goto 20
101   end do		
      do i=1,2
        if (ora(i).lt.0)goto 102
        if (ora(i).ne.orag(i))goto 20
102   end do	

      if (scad(1).ge.0.and.scad(1).ne.scadg(1))goto 20
      if (scad(2).lt.0.or.scad(3).lt.0)then
      	isc=max(scad(2),scad(3))
      	iscg=max(scadg(2),scadg(3))
      	if (isc.ge.0.and.isc.ne.iscg)goto 20
      else
      	if (scad(2).ne.scadg(2))goto 20
      	if (scad(3).ne.scadg(3))goto 20
      end if
      if (scad(4).ge.0.and.scad(4).ne.scadg(4))goto 20

      do i=1,3
        if (level(i).lt.0)goto 104
        if (level(i).ne.levelg(i))goto 20
104   end do
      do i=1,3
        if (var(i).lt.0)goto 105
        if (var(i).ne.varg(i))goto 20
105   end do

      igiro=0
      goto 30

99    if(igiro.eq.0)then
       print*,'rewind'
         call pbseek(iug,0,0,kret)
         igiro=1
         goto 20
      else
cd        write(*,*)'grib not found'
cd        write(*,*)'scad',scad
cd        write(*,*)'level',level
cd        write(*,*)'var',var
cc        ier=1
      goto 30
      end if

30    return

      end


