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





	SUBROUTINE QUACONSPAtem (K,IVAL,aLAT,aLON,hhpoz,data,ora,
	1	ISOGLIA,GRADMAX,IN,IB,FLAG,IER)

comstart QUACONSPATEM
C
C	SUBROUTINE QUACONSPAtem (K,IVAL,aLAT,aLON,hhpoz,data,ora,
C	1	ISOGLIA,GRADMAX,IN,IB,FLAG,IER)
c
C	Questa subroutine effettua un controllo spaziale dei dati  di 
C	temperatura su stazioni che devono essere il piu` possibile 
C	equidistribuite nello spazio. 
C
C	Funziona solamente per stazioni all'interno del territorio regionale 
C	(lat. min.= 43.4, lat. max.= 45.1, lon. min.= 9.1, lon. max.= 12.5),
C	e la cui altezza sia superiore a -100 metri.
C	Nel caso venga trovata anche una sola stazione che non soddisfa queste 
C	richieste il controllo non viene effettuato e la flag di errore (IER) 
C	viene posta a -1.
C
C	Per ogni stazione viene analizzata l'anomalia dal clima, definito dalla
C	routine CLIMA_TEMP_MAX_MIN utilizzata anche per il controllo 
C	climatologico.
C
C	Dopo che e` stata calcolata l'anomalia per ogni stazione viene chiamata
C	la routine QUACONSPA (vedi documentazione propria).
C
C	Nella versione attuale e` consentito al massimo il controllo di 
C	2000 stazioni, se il numero di stazioni da testare e` superiore la flag
C	di errore viene posta a 1.
C
C===============================================================================
C	INPUT:
C
C	K		I*4	Numero di stazioni da testare.
C	IVAL(K)	     	I*4	Valori misurati nelle stazioni espressi in C/10.
C	ALAT(K)      REAL*4	Latitudine delle stazioni in centesimale.
C	ALON(K)      REAL*4	Longitudine delle stazioni in centesimale.
C	HHPOZ(K)     REAL*4	Altezza della stazione in metri.
C	DATA(3)	        I*4	Data relativa alle osservazioni (gg/mm/aa).
C	ORA(2)		I*4	Ora relativa alle osservazioni (hh/mm).
C	ISOGLIA		I*4	Tolleranza tra due stazioni espressa in C/10.
C	GRADMAX      REAL*4	Gradiente orrizzontale massimo consentito 
C				espresso in C/10/Km.
C
C===============================================================================
C	OUTPUT :
C
C	IN		I*4	Numero di stazioni errate.
C	IB		I*4	Numero di stazioni buone.
C	FLAG(K)	       BYTE	Vettore di flag associate ai dati da testare.
C	IER		I*4	condizione di errore della routine:
c				-80 errore allocazione virtual memory
c				-3 due o piu` stazioni con le stesse coordinate
c				   la coppia di stazioni non viene esaminata
c				-2 solo due stazioni, non si puo` triangolare
C				-1 Anagrafica stazione errata
C					fuori dalla regione o
C					altezza sbagliata
C				   non viene controllata nessuna stazione.
C				0  Nessuna errore dalla routine, tutto OK.
C				1   Numero di stazioni da testare > di 2000.
c				
c			errori dovuti alla routine CLIM_TEM_NORM
c				ier=100 errore allocazione unita` per lettura
c				ier=100+iostat  (codice errore apertura file)
c
c			errori dovuti alla routine CLIM_TEMP_MAX_MIN
c				ier=200 errore allocazione unita` per lettura
c				ier=200+iostat  (codice errore apertura file)
COMEND

CC**********************************************************************CC
CC**********************************************************************CC
CC									CC
CC		SERVIZIO METEOROLOGICO REGIONE EMILA ROMAGNA		CC
CC				E.R.S.A.				CC
CC									CC
CC									CC
CC	PAOLO PATRUNO				PIERPAOLO ALBERONI	CC
CC									CC
CC	BOLOGNA 1992							CC
CC**********************************************************************CC
CC**********************************************************************CC

	PARAMETER DATMAX=2000
	dimension iano(DATMAX),ival(k),alat(k),alon(k),hhpoz(k)
	INTEGER*4 STAZ,DATA(3),ORA(2)
	BYTE FLAG(k)
	LOGICAL J_C_E,vf

	DATA ALATMIN,ALATMAX,ALONMIN,ALONMAX/	!limiti dell'area Emilia R.
	1	43.667,45.167,9.167,12.833/

	ier=0
	IF (K.GT.DATMAX)THEN
		IER=1
		GOTO 23
	END IF

C-------------------------------------------------------------------------------
C	CONTROLLO SE DENTRO LIMITI REGIONALI
C-------------------------------------------------------------------------------

	do i=1,k

	  IF (HHPOZ(i).LT.-100.)THEN
		IER=-1
		GOTO 23
	  END IF
	  IF (ALON(i).LT.ALONMIN.OR.ALON(i).GT.ALONMAX.OR.
	1	ALAT(i).LT.ALATMIN.OR.ALAT(i).GT.ALATMAX)THEN
			IER=-1
			GOTO 23
	  ENDIF

	  hhp=hhpoz(i)
	  IF (HHP.LT.0)HHP=0.

	  CALL CLIMA_TEMP_MAX_MIN
	1	(ALAT(i),ALON(i),HHP,
	1	DATA(2),DATA(1),ORA(1),clim,
	1	SIGMAX,SIGMIN,SIGMED,IER)
	  if(ier.ne.0)goto 23
	  IF(J_C_E(IVAL(I)).AND.vf(FLAG(I)))THEN
	    iano(I)=ival(I)-nint(clim*10.)
	  ELSE
	    IANO(I)=32767
	  END IF

d	  type*,'dati',ival(I),nint(clim*10),iano(I)
d	type*,'call quaconspa',k,iano(i),alat(i),alon(i),gradmax,flag(i)
	end do

C-------------------------------------------------------------------------------
C	TEMPERATURA IN CAPANNINA
C-------------------------------------------------------------------------------

	call QUACONSPA (K,Iano,aLAT,aLON,ISOGLIA,GRADMAX,IN,IB,FLAG,IER)

23	RETURN
	END
