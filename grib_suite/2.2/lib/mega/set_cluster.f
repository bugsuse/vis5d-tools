c Copyright (C) 2000 

c Questo programma è software libero; è lecito ridistribuirlo e/o
c modificarlo secondo i termini della Licenza Pubblica Generica SMR come
c pubblicata da ARPA SMR ; o la versione 1 della licenza o (a scelta)
c una versione successiva.

c Questo programma è distribuito nella speranza che sia utile, ma SENZA
c ALCUNA GARANZIA; senza neppure la garanzia implicita di
c COMMERCIABILITÀ o di APPLICABILITÀ PER UN PARTICOLARE SCOPO. Si veda
c la Licenza Pubblica Generica SMR per avere maggiori dettagli.

c Ognuno dovrebbe avere ricevuto una copia della Licenza Pubblica
c Generica SMR insieme a questo programma; in caso contrario, la si può
c ottenere da Agenzia Regionale Prevenzione e Ambiente (ARPA) Servizio
c Meteorologico Regionale (SMR), Viale Silvani 6, 40122 Bologna, Italia






	SUBROUTINE SET_CLUSTER (K,LAT,LON,HSTAZ,IMESE,ICLUSTER,IER)

COMSTART SET_CLUSTER 
C SUBROUTINE SET_CLUSTER (K,LAT,LON,HSTAZ,IMESE,ICLUSTER,IER)
C
C	Questa routine definisce, dato un vettore di stazioni, il cluster di 
C	appartenenza di ogni stazione. I cluster sono stati definiti sulla base 
C	della climatologia indentificata da Nanni.
C
C	Vengono definiti cluster diversi a seconda del mese relativo.
C
C===============================================================================
C	Sempre valido
C
C	CLUSTER = 0 		Fuori dai limiti regionali 
C				(lat. min.= 43.4, lat. max.= 45.1,
C				 lon. min.=  9.1, lon. max.= 12.5,
C				 definiti in centesimali)
C===============================================================================
C	OTTOBRE-APRILE (Inverno)
C
C	CLUSTER = 0 		Stazione di montagna HSTAZ > 500. metri
C	CLUSTER = 1		Stazione di pianura
C===============================================================================
C	AGOSTO-SETTEMBRE 
C
C	CLUSTER = 0		Se Lat.< 44.7 e Lon < 10.5
C	CLUSTER = 1		Se Lat.> 44.7
C	CLUSTER = 2		Se Lat.< 44.7 e 10.5 < Lon. < 11.5
C	CLUSTER = 3		Se Lat.< 44.7 e Lon. > 11.5
C===============================================================================
C	MAGGIO-LUGLIO
C
C	CLUSTER = 0 		Stazioni di montagna HSTAZ > 500. metri
C	CLUSTER = 1		Se Lon. < 10.2 		
C	CLUSTER = 2		Se 10.2 < Lon. < 11.2
C	CLUSTER = 3		Se Lon. > 11.2
C===============================================================================
C
C	dove Lat. e Lon. sono  le coordinate delle stazioni in gradi 
C	centesimali e HSTAZ le altezze delle stazioni
C
C===============================================================================
C	INPUT:
C
C	K		I*4	Numero stazioni da suddividere in cluster
C	LAT(K)	     Real*4	Vettore latitudine stazioni espresse in 
C				gradi centesimali
C	LON(K)	     Real*4	Vettore longitudine stazioni espresse in 
C				gradi centesimali
C	HSTAZ(K)     Real*4	Vettore altezze stazioni espresse in metri
C	IMESE		I*4	Mese relativo alla suddivisione in cluster
C
C===============================================================================
C	OUTPUT:
C
C	ICLUSTER(K)	I*4	Vettore contenente il cluster relativo ad ogni 
C				stazione
C	IER		I*4	IER=0 (non utilizzato)
C
COMEND
CC**********************************************************************CC
CC**********************************************************************CC
CC									CC
CC		SERVIZIO METEOROLOGICO REGIONE EMILA ROMAGNA		CC
CC				E.R.S.A.				CC
CC									CC
CC	PAOLO PATRUNO				PIER PAOLO AALBERONI	CC
CC									CC
CC	BOLOGNA 1992							CC
CC**********************************************************************CC
CC**********************************************************************CC

	REAL LAT(K),LON(K),hstaz(k)
	INTEGER ICLUSTER(K)

	DATA ALATMIN,ALATMAX,ALONMIN,ALONMAX/	!limiti dell'area Emilia R.
	1	43.667,45.167,9.167,12.833/

	IER=0

	DO I=1,K
	  IF(LAT(I).GT.ALATMAX.OR.LAT(I).LT.ALATMIN.OR.
	1	LON(I).GT.ALONMAX.OR.LON(I).LT.ALONMIN)THEN
		ICLUSTER(I)=0
		GOTO 10
	  ENDIF
	  IF(IMESE.GE.10.OR.IMESE.LE.4)THEN	!OTTOBRE-APRILE
	    IF(HSTAZ(I).GT.500.)THEN
		ICLUSTER(I)=0
	    ELSE
		ICLUSTER(I)=1
	    END IF
	  ELSE IF(IMESE.GE.8)THEN			!AGOSTO-SETTEMBRE
	    IF(LAT(I).GT.44.7)THEN
		ICLUSTER(I)=1
	    ELSEIF(LON(I).GT.10.5.and.LON(I).LT.11.5)THEN
	 	ICLUSTER(I)=2
	    ELSEIF(LON(I).GT.11.5)THEN
	 	ICLUSTER(I)=3
	    ELSE
		ICLUSTER(I)=0
	    END IF
	  ELSEIF(IMESE.GE.5)THEN			! MAGGIO-GIUGNO-LUGLIO
	    IF(HSTAZ(I).LT.500.)THEN
	      IF(LON(I).LT.10.2)THEN
		ICLUSTER(I)=1
	      ELSEIF(LON(I).LT.11.2)THEN
		ICLUSTER(I)=2
	      ELSE
		ICLUSTER(I)=3
	      ENDIF
	    ELSE
	      ICLUSTER(I)=0
	    ENDIF
	  ENDIF
10	ENDDO
	RETURN
	END
