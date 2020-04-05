c    <grib_util, software di utilita' per  grib, API layer>
c    Copyright (C) 2000  <Paolo Patruno SMR ARPA>
c
c    $Date: 2002/10/08 17:59:42 $    $Revision: 1.8 $
c    $Id: getinfo.f,v 1.8 2002/10/08 17:59:42 patruno Exp $

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


      subroutine getinfo (iug,grib,idimg,data,ora,scad,level,var,
     +	ala1,ala2,alo1,alo2,nj,ni,dj,di,idrt
     +	,alarot,alorot,rot,ija,ier)

COMSTART GETINFO
C
C	subroutine getinfo (iug,grib,idimg,data,ora,scad,level,var,
C	1	ala1,ala2,alo1,alo2,nj,ni,dj,di,idrt
C	1	,alarot,alorot,rot,ija,ier)
C
c
c	Restituisce informazioni rispetto ad un grib.
c	Se fornita unita` di lettura provvede anche alla lettura (sequenziale)
c	di un grib.
c	N.B.
c	Gestisce solo grib con data rappresentation type 0 oppure 10.
C
c	input:
c
c	iug		I	unita` restituita da PBOPEN se si vuole
c					una lettura sequenziale da file
c				=-1 se il grib viene fornito in input
c	grib(idimg)	I	buffer contenente il grib se iug=-1
c
c	output:
c
c	grib(idimg)	I	buffer contenente il grib se iug<>0
c	data(1)		I	giorno		)	
c	data(2)		I	mese		)
c	data(3)		I	anno			)  emissione
c	ora(1)		I	ora		)
c	ora(2)		I	minuti		)
c	scad(1)		I	indicator of unit of time range	(table 4)
c	scad(2)		I	periodo di tempo 1
c	scad(3)		I	periodo di tempo 2
c	scad(4)		I	time range indicator 		(table 5)
c	level(1)	I	indicator of type of level	(table 3)
c	level(2)	I	height, pressure etc. of levels
c	level(3)	I	height, pressure etc. of levels
c	var(1)		I	identification of originating/generating
c				 centre  (table 0)
c	var(2)		I	table 2 version number
c	var(3)		I	parameter			(table 2)
c	ala1	I	latitudine del primo punto di vet
c	ala2 	I	latitudine dell'ultimo punto di vet
c	alo1 	I	longitudine del primo punto di vet
c	alo2 	I	longitudine dell'ultimo punto di vet
c	nj	I	numero di punti lungo un meridiano
c	ni	I	numero di punti lungo un parallelo
c	dj 	I	passo di griglia in latitudine
c	di 	I	passo di griglia in longitudine
c	idrt	I	data representation type	(table 6)
c	alarot alorot rot	R
c		latitudine longitudine e rotazione del polo di rotazione
c		espresso in gradi.decimi......
c	ija	I	indicatore del modo di scansione
c			numero decimale che imposta correttamente i bit
c			della variabile (es. ija=128)
c		bit 1	0	scansione dei punti nella direzione +i
c			1	scansione dei punti nella direzione -i
c		bit 2	0	scansione dei punti nella direzione -j
c			1	scansione dei punti nella direzione +j
c		bit 3	0	i punti adiacenti nella direzione i
c				sono consecutivi
c		bit 3	1	i punti adiacenti nella direzione j
c				sono consecutivi
c
c	ier		I	codice errore
c				=0 tutto o.k.
c				=-1 end of file
c				=7000 errore data representation type
c					code table 6
c				=8000 errore sezione 2 omessa
c					non vengono restituiti i parametri
c					descrittori dell'area
c				altri > vedi errori pbgrib/gribex
COMEND

      parameter (ipump=1)
cc
      DIMENSION ISEC0(2)
      DIMENSION ISEC1(32+100)
      DIMENSION ISEC2(384)
      DIMENSION ZSEC2(310)
      DIMENSION ISEC3(2)
      DIMENSION ZSEC3(2)
      DIMENSION ISEC4(42)
      DIMENSION ZSEC4(IPUMP)
      dimension grib(idimg)
cc
      real northlat,sudlat
      real latinc
      real westlon,estlon
      real loninc

      integer data(3),ora(2),level(3),scad(4),var(3)

      data irecl/120/


      include 'getinfo.inc'

      return
      end

