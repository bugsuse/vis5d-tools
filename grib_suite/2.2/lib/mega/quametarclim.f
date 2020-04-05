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





	SUBROUTINE QUAMETARCLIM(FLAG,IER)

COMSTART QUAMETARCLIM
C	SUBROUTINE QUAMETARCLIM(FLAG,IER)
c						modificato 12/04/94
c	controllo di qualita` di tipo climatologico sui dati metar.
c	OGNI VARIABILE SYNOP VIENE CONFRONTATA CON DEI LIMITI DI VALIDITA`
C	entro i quali deve essere compresa.

c	FLAG(23)	BYTE	incrementato di tre se verificato errore
c				per le 23 variabili metar
c	IER		I*4	condizione di errore
C			IER=1	tipo messaggio errato

c	COMMON /RRW/RRW	I*2	RRW(29) per passare i dati metar come dichiarato
c				in met$inc:metar.inc

C	Per indirizzare il vettore FLAG alla variabile interessata si puo`
c	utilizzare, includendo nel proprio programma, il file
c	megainc$dir:quametar.inc ed utilizzare come indici del vettore le
c	variabili che hanno lo stesso nome della variabile (meteodata)
c	preceduto da 'IN'. Ad esempio:
c	per la temperatura        FLAG(INFFF)

cI principi generali su cui sono basati questi controlli di qualita`
csono stati ricavati con alcuni adattamenti da:
c
c    1) W.W.W. PLANNING REPORT N.26  'QUALITY CONTROL PROCEDURES 
c       FOR METEOROLOGICAL DATA'
c
c    2) technical note  E.C.M.W.F.- operations department- 
c       'quality control checks applied to observational data'
c
c    3) AERONAUTICA MILITARE ITALIANA REGOLAMENTO DI SERVIZIO N.7
c       'MESSAGGI METEOROLOGICI' MET RS 7  FM 15-V METAR
c
c    4) W.M.O. UNESCO 1970  MAPPE DI TEMPERATURA MEDIA E PRECIPITAZIONE
c       'ATLANTE CLIMATICO DI EUROPA'
c
c     FLAG    NOME VAR.	  SOGLIA MIN.  SOGLIA MAX.        NOTE

C	1	DDD		0	360	vento variabile 370
C	2	FFF		0	199
C	3	FM		10	199
C	4	VVVV		0	10000
C	5	WW	codice esistente e corrispondenza numero-char
C	6	NS(1)		1	9
C	7	NS(2)		3	8
C	8	NS(3)		5	8
C	9	NS(4)		1	8
C	10	CC(1)		TIPO NUBE ESISTENTE
C	11	CC(2)		TIPO NUBE ESISTENTE
C	12	CC(3)		TIPO NUBE ESISTENTE
C	13	CC(4)		TIPO NUBE ESISTENTE
C	14	HHH(1)		25	30000
C	15	HHH(2)		25	30000
C	16	HHH(3)		25	30000
C	17	HHH(4)		25	30000
C	18	TT		-29	49
C	19	TD		-29	49
C	20	PH		960	1060
COMEND
	COMMON /RRW/RRW

	DIMENSION LIMINF(23),LIMSUP(23)

	INCLUDE 'MET$INC:METAR.INC'

	INTEGER*4 STAZ,DATA(3),ORA(2),TIPO
	BYTE FLAG(23)
	BYTE RR(58)
	INTEGER*2 RRW(29)
	EQUIVALENCE (RRW,RR)

C	INCLUDE 'MEGAINC$DIR:QUAMETAR.INC/NOLIST'
	LOGICAL FLA
	PARAMETER NUMNU=14
	CHARACTER*2 NUBI(NUMNU)
	DATA NUBI/'CI','CC','CS','AC','AS','NS','SC','ST','CU','CB',
	1	'CA','VO','K ','//'/

	PARAMETER NUMTE=99
	CHARACTER*6 TEMPRS(NUMTE)
	DATA TEMPRS/
	1	'  ','  ','  ','FU','HZ','HZ','SA','PO','  ',
	1	'BR','MIFG','MIFG','  ','  ','  ','  ','TS','SQ','FC',
	2	'REDZ','RERA','RESN','RERASN','REFZRA','RESH','RESNSH',
	2	'REGR','  ','RETS',
	3	'SA','SA','SA','XXSA','XXSA','XXSA','DRSN','DRSN',
	3	'BLSN','BLSN',
	4	'BCFG','BCFG','FG','FG','FG','FG','FG','FG','FZFG','FZFG',
	5	'DZ','DZ','DZ','DZ','XXDZ','XXDZ','FZDZ','XXFZDZ',
	5	'RA','RA',
	6	'RA','RA','RA','RA','XXRA','XXRA','FZRA','XXFZRA',
	6	'RASN','XXRASN',
	7	'SN','SN','SN','SN','XXSN','XXSN','  ','SG','  ','PE',
	8	'RASH','XXSH','XXSH','RASN','XXRASN','SNSH','XXSN',
	8	'GR','GR','GR',
	9	'XXGR','RA','XXRA','GR','XXGR','TS','TSGR','XXTS',
	9	'TSSA','XXTSGR'/

C	soglia inferiore per tutte le variabili

	LIMINF(1)=0
	LIMINF(2)=0
	LIMINF(3)=10
	LIMINF(4)=0
	LIMINF(5)=4
	LIMINF(6)=4
	LIMINF(7)=4
	LIMINF(8)=4
	LIMINF(9)=1
	LIMINF(10)=3				!mod. 23/04/94
	LIMINF(11)=5				!mod. 23/04/94
	LIMINF(12)=1
	LIMINF(13)=0
	LIMINF(14)=0
	LIMINF(15)=0
	LIMINF(16)=0 
	LIMINF(17)=25   
	LIMINF(18)=25 
	LIMINF(19)=25 
	LIMINF(20)=25 
	LIMINF(21)=-29
	LIMINF(22)=-29
	LIMINF(23)=960

C	soglia superiore per tutte le variabili
	
	LIMSUP(1)=360
	LIMSUP(2)=199
	LIMSUP(3)=199
	LIMSUP(4)=10000
	LIMSUP(5)=99
	LIMSUP(6)=99
	LIMSUP(7)=99
	LIMSUP(8)=99
	LIMSUP(9)=9
	LIMSUP(10)=8
	LIMSUP(11)=8
	LIMSUP(12)=8
	LIMSUP(13)=99
	LIMSUP(14)=99
	LIMSUP(15)=99
	LIMSUP(16)=99
	LIMSUP(17)=30000
	LIMSUP(18)=30000
	LIMSUP(19)=30000
	LIMSUP(20)=30000
	LIMSUP(21)=49
	LIMSUP(22)=49
	LIMSUP(23)=1060

	ier=0

	CALL GETHEA (STAZ,DATA,ORA,TIPO,RRW)

	IF (TIPO.NE.10)THEN
		IER=1
		RETURN
	END IF

C	limite per la direzione vento se il vento e` variabile
	IF (RRW(7).EQ.370)LIMSUP(1)=370

24	CONTINUE	! INIZIO CONTROLLO

	DO NVAR=7,10

	  IF (RRW(NVAR).LT.LIMINF(NVAR-6))FLAG(NVAR-6)=FLAG(NVAR-6)+3
	  IF (RRW(NVAR).GT.LIMSUP(NVAR-6))FLAG(NVAR-6)=FLAG(NVAR-6)+3
		
	END DO

C	tempo presente		!iww error, inserted iiww 23/04/94

	NVAR=11
	CALL NCARINT(WW,1,2,IERRO)
	IF(IERRO.NE.0)THEN
1111		iIWW=32767
		FLAG(NVAR-6)=FLAG(NVAR-6)+3
		GOTO 2222
	END IF

	READ(WW,'(I2)',ERR=1111)iIWW

	IF (TEMPRS(iIWW).EQ.'  ')THEN
		FLAG(NVAR-6)=FLAG(NVAR-6)+3
		GOTO 2222
	END IF
	IF (TEMPRS(iIWW).NE.WW(3:8))THEN
		FLAG(NVAR-6)=FLAG(NVAR-6)+3
		GOTO 2222
	END IF

2222	CONTINUE


c	continuo con le restanti variabili
	DO NVAR=15,18

	  IF (RRW(NVAR).LT.LIMINF(NVAR-6))FLAG(NVAR-6)=FLAG(NVAR-6)+3
	  IF (RRW(NVAR).GT.LIMSUP(NVAR-6))FLAG(NVAR-6)=FLAG(NVAR-6)+3
		
	END DO

c	nuvolosita`
	DO NVAR=19,21

	FLA=.TRUE.
	DO NU=1,NUMNU
		IF (CC(NVAR-18).EQ.NUBI(NU))FLA=.FALSE.
	END DO		
		IF (FLA)FLAG(NVAR-6)=FLAG(NVAR-6)+3
	END DO

	NVAR=22		!quarto gruppo    solo CB
	IF (CC(NVAR-18).NE.'CB'.AND.CC(NVAR-18).NE.'  ')
	1	FLAG(NVAR-6)=FLAG(NVAR-6)+3

c	continuo con le restanti variabili
	DO NVAR=23,29

	  IF (RRW(NVAR).LT.LIMINF(NVAR-6))FLAG(NVAR-6)=FLAG(NVAR-6)+3
	  IF (RRW(NVAR).GT.LIMSUP(NVAR-6))FLAG(NVAR-6)=FLAG(NVAR-6)+3
		
	END DO

	RETURN
	END
