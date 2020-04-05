
c    <grib_util software di utilita' per  grib>
c    Copyright (C) 2000  <Paolo Patruno SMR ARPA>

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


      subroutine getdata (grib,idimg,imd,rmd,values,idimv,ibm,ier)

COMSTART GETDATA
C	subroutine getdata (grib,idimg,imd,rmd,values,idimv,ibm,ier)
C
C	Restituisce i dati della sezione 3 e 4 di un grib fornito
c	in input
C
c	input:
c
c	grib(idimg)	I	buffer contenente il grib
c       idimg           I       dimensione massima di idimg (NB riscritta in out)
C	imd		I	valore intero segnalazione manca dato
c	rmd		R	valore reale segnalazione manca dato
c
c
c	output:
c
c	values(idimv)	R	vettore dei dati del grib
c       idimv           I       dimensione reale dei dati letti in values
c	ibm		I	bit map predefinita (=0 inclusa o non utlizzata)
c	ier		I	codice errore
c				=0 tutto o.k.
c				>0 codice errore degribbing (gribex)
c
c  19/4/99 inizializzata variabile ibm altrimenti non corretta specialmente
c          per compilazioni non -static
c
COMEND

      DIMENSION ISEC0(2)
      DIMENSION ISEC1(32+100)
      DIMENSION ISEC2(384)
      DIMENSION ZSEC2(310)
      DIMENSION ISEC3(2)
      DIMENSION ZSEC3(2)
      DIMENSION ISEC4(42)

      dimension grib(idimg),values(idimv)

      isec3(2)=imd
      zsec3(2)=rmd

c	<<<<<<<	 start	>>>>>>>>
      ker=1
      isec3(1)=0                ! se sezione 3 omessa non inizializzata!

      CALL GRIBEX (isec0,isec1,isec2,zsec2,isec3,zsec3,isec4,
     +	values,idimv,grib,idimg,idummy,'D',ker)

      if(ker.gt.0)then
          print*,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          print*,'	>>>>>>>>>>  errore decodifica',ker
          print*,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          idimv=0
          ier=ker
          return
      end if
      ier=0
      idimv=isec4(1)
      ibm=isec3(1)
c	type*,'		>>>>>>>>>>  decodifica ok '

      return
      end
