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





	SUBROUTINE QUAMETARCLIM_new(FLAG,IER)

COMSTART QUAMETARCLIM
C	SUBROUTINE QUAMETARCLIM_new(FLAG,IER)
c						modificato 12/04/94
c						modificato 10/02/97
c	controllo di qualita` di tipo climatologico sui dati metar.
c			versione dal 1/7/93
c	OGNI VARIABILE SYNOP VIENE CONFRONTATA CON DEI LIMITI DI VALIDITA`
C	entro i quali deve essere compresa.
c
c	FLAG(23)	BYTE	incrementato di tre se verificato errore
c				per le 23 variabili metar
c	IER		I*4	condizione di errore
C			IER=1	tipo messaggio errato
c
c	COMMON /RRW/RRW	I*2	RRW(29) per passare i dati metar come dichiarato
c				in met$inc:metar.inc
c
C	Per indirizzare il vettore FLAG alla variabile interessata si puo`
c	utilizzare, includendo nel proprio programma, il file
c	megainc$dir:quametar.inc ed utilizzare come indici del vettore le
c	variabili che hanno lo stesso nome della variabile (meteodata)
c	preceduto da 'IN'. Ad esempio:
c	per la temperatura        FLAG(INFFF)
c
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
c
C	1	DDD		0	360	vento variabile 370, calma 380
C	2	FFF		0	199
C	3	FM		10	199
C	4	VVVV		0	10000
C	5	WW	codice esistente 
C	6	NS(1)		1	9
C	7	NS(2)		2	8	n.b.limite inf dovrebbe essere 3
C	8	NS(3)		5	8
C	9	NS(4)		1	8
C	10	CC(1)		TIPO NUBE ESISTENTE
C	11	CC(2)		TIPO NUBE ESISTENTE
C	12	CC(3)		TIPO NUBE ESISTENTE
C	13	CC(4)		TIPO NUBE ESISTENTE e presente (CB,CU)
C	14	HHH(1)		25	30000
C	15	HHH(2)		25	30000
C	16	HHH(3)		25	30000
C	17	HHH(4)		25	30000
C	18	TT		-29	49
C	19	TD		-29	49
C	20	PH		960	1060
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c						N.B.
c
c DAL I/7/93 LA CODIFICA DEL REPORT METAR E` STATA MODIFICATA SECONDO LE
c DIRETTIVE W.M.O..
c
c SONO QUINDI STATE APPORTATE LE NECESSARIE MODIFICHE AL SOFHTWARE DI
c ARCHIVIAZIONE: 
c
c	5/7/93	adapting to new WMO METAR coding:
c		- GGggZ handled
c		- KT, KMH, MPS handled (all values converted to knots)
c		  (370 in ddd means variable direction, 380 calm if fff=0)
c		- dddVddd group ignored
c		- Dv in VVVVDv ignored
c		- VxVxVxVxDv ignored
c		- Runway visibility groups ignored
c		- w'w'(ww) recorded as found (maximum 8 bytes)
c		- Cloud groups : Cloud type id recorded only if CB or TCU
c				 are present (TCU is reported as CU).
c				 Otherwise no data is recorded.
c				 SCT is reported as 2 oktas
c				 BKN as 6 oktas
c				 OVC as 8 oktas
c				 SKC means no clouds
c				 No change in cloud height handling
c				 
c VVhshshs is reported as 9 oktas an hshshs as given
c		- QPhPhPhPh handled
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  anno 1997    attenzione aggiunta gestione :
c               - Cloud groups :
c                                FEW is reported as 1 oktas
c
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
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
	LOGICAL FLA,C_E
	PARAMETER NUMNU=6
	PARAMETER NUMNU4=2
	CHARACTER*2 NUBI(NUMNU),NUBI4(NUMNU4)
	DATA NUBI/'  ','CU','CB','CA','VO','K '/
	DATA NUBI4/'CU','CB'/

	PARAMETER NUMTE=33
	CHARACTER*2 TEMPRS(NUMTE)

			!MODIFICA  10-02-97
C	DATA TEMPRS/
C	1	 'VC'
C	1	,'MI','BC','DR','BL','SH','TS','FZ'
C	1	,'DZ','RA','SN','SG','IC','PE','GR','GS','BR','FG','FU'
C	2	,'VA','DU','SA','HZ'
C	1	,'PO','SQ','FC','SS','DS'
C	1	,'  ','  ','  ','  '/

	DATA TEMPRS/
	1	 'VC'
	1	,'MI','BC','PR','TS','BL','SH','DR','FZ'
	1	,'DZ','RA','SN','SG','IC','PE','GR','GS'
	1	,'BR','FG','FU','UP'
	2	,'VA','DU','SA','HZ','PY'
	1	,'PO','SQ','FC','SS','DS','FC'
	1	,'  '/


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
	LIMINF(10)=2
	LIMINF(11)=5
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

C	limite per la direzione vento se il vento e` variabile o calma
	IF (RRW(7).EQ.370)LIMSUP(1)=370
	IF (RRW(7).EQ.380)LIMSUP(1)=380

	IF(RRW(15).EQ.9)THEN
		LIMINF(17)=0000			!da fare
		LIMSUP(17)=30000
	END IF

24	CONTINUE	! INIZIO CONTROLLO

	DO NVAR=7,10

	  IF (RRW(NVAR).LT.LIMINF(NVAR-6))FLAG(NVAR-6)=FLAG(NVAR-6)+3
	  IF (RRW(NVAR).GT.LIMSUP(NVAR-6))FLAG(NVAR-6)=FLAG(NVAR-6)+3
		
	END DO

C	tempo presente

	NVAR=11
	ipin=1

	if(ww(1:1).eq.char('FF'X))GOTO 2222	! nil

	IF(WW(1:1).EQ.'-'.OR.WW(1:1).EQ.'+')ipin=ipin+1
3333	do nu=1,numte
	  if(ww(ipin:ipin+1).eq.temprs(nu))then
		ipin=ipin+2
		if(ipin.ge.8)goto 2222
		goto 3333
	  end if
	end do

cd	print *,'trovata differenza<',ww(ipin:ipin+1),'>'
cd	print*,'incremento flag',flag(nvar-6)
	FLAG(NVAR-6)=FLAG(NVAR-6)+3

2222	CONTINUE

c	continuo con le restanti variabili
	DO NVAR=15,18

	  IF (.NOT.C_E(RRW(NVAR)))GOTO 36		!coperture
	  IF (RRW(NVAR).LT.LIMINF(NVAR-6))FLAG(NVAR-6)=FLAG(NVAR-6)+3
	  IF (RRW(NVAR).GT.LIMSUP(NVAR-6))FLAG(NVAR-6)=FLAG(NVAR-6)+3
		
36	END DO

c	nuvolosita`	gruppi 1,2,3
	DO NVAR=19,21

	  if(CC(NVAR-18)(1:1).eq.char('FF'X))GOTO 37	! nil
	  FLA=.TRUE.
	  DO NU=1,NUMNU
		IF (CC(NVAR-18).EQ.NUBI(NU))FLA=.FALSE.
	  END DO		
	  IF (FLA)FLAG(NVAR-6)=FLAG(NVAR-6)+3
37	END DO


c	nuvolosita`	gruppo 4

	NVAR=22

	FLA=.TRUE.
	DO NU=1,NUMNU4
		IF (CC(NVAR-18).EQ.NUBI4(NU))FLA=.FALSE.
	END DO		
	IF (FLA)FLAG(NVAR-6)=FLAG(NVAR-6)+3


c	continuo con le restanti variabili


	DO NVAR=23,26

	  IF (.NOT.C_E(RRW(NVAR)))GOTO 38		!altezza nubi
	  IF (RRW(NVAR).LT.LIMINF(NVAR-6))FLAG(NVAR-6)=FLAG(NVAR-6)+3
	  IF (RRW(NVAR).GT.LIMSUP(NVAR-6))FLAG(NVAR-6)=FLAG(NVAR-6)+3
		
38	END DO

	DO NVAR=27,29

	  IF (RRW(NVAR).LT.LIMINF(NVAR-6))FLAG(NVAR-6)=FLAG(NVAR-6)+3
	  IF (RRW(NVAR).GT.LIMSUP(NVAR-6))FLAG(NVAR-6)=FLAG(NVAR-6)+3
		
	END DO

	RETURN
	END
