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





	SUBROUTINE QUASYNOPCLIM(FLAG,IER)
COMSTART QUASYNOPCLIM
C	SUBROUTINE QUASYNOPCLIM(FLAG,IER)
C							modificato 12/04/94
c	controllo di qualita` di tipo climatologico sui dati synop.
c	OGNI VARIABILE SYNOP VIENE CONFRONTATA CON DEI LIMITI DI VALIDITA`
C	entro i quali deve essere compresa. Per la maggioranza delle variabili
c	i limiti sono fissi e sempre validi; per alcune altre dipendono 
c	dalla posizione geografica (pressione a livello stazione e
c	geopotenziale) e dalle variazioni stagionali (radiazione solare).
c
c	FLAG(43)	BYTE	incrementato di tre se verificato errore
c				per le 43 variabili synop
c	IER		I*4	condizione di errore
C			IER=1	tipo messaggio errato
c			IER=-1	non trovo i dati anagrafici della stazione
c				e quindi faccio controlli piu` blandi.

c	COMMON /RRW/RRW	I*2	RRW(50) per passare i dati synop come dichiarato
c				in met$inc:synop.inc

C	Per indirizzare il vettore FLAG alla variabile interessata si puo`
c	utilizzare, includendo nel proprio programma, il file
c	megainc$dir:quasynop.inc ed utilizzare come indici del; vettore le
c	variabili che hanno lo stesso nome della variabile (meteodata)
c	preceduto da 'IN'. Ad esempio:
c	per la temperatura        FLAG(INTATA)

c	I principi generali su cui sono basati questi controlli di qualita`
c	sono stati ricavati con alcuni adattamenti da:
c
c    1) W.W.W. PLANNING REPORT N.26  'QUALITY CONTROL PROCEDURES 
c       FOR METEOROLOGICAL DATA'
c
c    2) technical note  E.C.M.W.F.- operations department- 
c       'quality control checks applied to observational data'
c
c    3) AERONAUTICA MILITARE ITALIANA REGOLAMENTO DI SERVIZIO N.5 
c       'MESSAGGI METEOROLOGICI' MET RS 5  FM 12-VIII SYNOP
c
c    4) W.M.O. UNESCO 1970  MAPPE DI TEMPERATURA MEDIA E PRECIPITAZIONE
c       'ATLANTE CLIMATICO DI EUROPA'

c     FLAG    NOME VAR.	  SOGLIA MIN.  SOGLIA MAX.        NOTE

C	1	HH		0	2500
C	2	VIS		0	9999
C	3	NTOT		0	9
C	4	DD		0	360	vento variabile 999
C	5	FF		0	199
C	6	TATA		-290	490
C	7	TDTD		-290	490	
C	8	UMID		20	100
C	9	PRES		9600	10600
C	10	GEOP		-650	6000  (500-1800) (2150-3450) (4700-6000)
C	11	P0P0		4500	10600	naca-1330  naca+670
C	12	CART		0	8
C	13	TEND		0	60
C	14	PRECI		0	9890
C	15	TR		6	12
C	16	TEMPRS		0	99
C	17	PEMPAS		0	99
C	18	NH		0	9
C	19	CL		0	9
C	20	CM		0	9
C	21	CH		0	9
C	22	TETE		-290	490
C	23	TW		-10	490
C	24	PWA		0	99
C	25	HWA		0	2000
C	26	PW		0	99
C	26	RADSOL		0	* (funzione del giorno,ora,latitudine)
C	27	HW		0	2000
C	28	E		0	9
C	29	TMINS		-290	490
C	30	EG		0	9
C	31	SSS		0	9960  zero in estate in pianura nel sud
C	32	NSS		0	9
C	33-35	NSS		0	8
C	36-39	CSS		0	9
C	40-43	HHSS		0	22500

C	N.B.
c	naca viene calcolata in funzione dell'altezza ed e` l'atmosfera standard
c	i limiti del geopotenziale sono diversi a seconda del livello standard
c	di riferimento.
c	per il calcolo della radiazione globale vedi RADMAX.
c	L'altezza della neve deve essere 0 nei mesi da Maggio a Settembre
c	nelle stazioni sotto i 500m. al di sotto del 48 pararallelo.
COMEND
	COMMON /RRW/RRW
	COMMON /ESTRASTAZ/SEZ2		!per avere SISO e VENT

	DIMENSION LIMINF(43),LIMSUP(43)

	INCLUDE 'MET$INC:STAZIONI.INC'	!per common estrastaz
C	INCLUDE 'MET$INC:SYNOP.INC'

	INTEGER*4 STAZ,DATA(3),ORA(2),TIPO
	BYTE FLAG(43)
	INTEGER*2 RRW(50)
	CHARACTER*24 ANOME

C	INCLUDE 'MEGAINC$DIR:QUASYNOP.INC/NOLIST'

C	soglia inferiore per tutte le variabili

	LIMINF(1)=0
	LIMINF(2)=0
	LIMINF(3)=0
	LIMINF(4)=0
	LIMINF(5)=0
	LIMINF(6)=-290
	LIMINF(7)=-290
	LIMINF(8)= 20
	LIMINF(9)= 9600
	LIMINF(10)=-650
	LIMINF(11)=4500  
	LIMINF(12)=0
	LIMINF(13)=0 
	LIMINF(14)=0   
	LIMINF(15)=6 
	LIMINF(16)=0 
	LIMINF(17)=0 
	LIMINF(18)=0
	LIMINF(19)=0
	LIMINF(20)=0
	LIMINF(21)=0
	LIMINF(22)=-290
	LIMINF(23)=-10
	LIMINF(24)=0 
	LIMINF(25)=0   
	LIMINF(26)=0 
	LIMINF(27)=0   
	LIMINF(28)=0
	LIMINF(29)=-290
	LIMINF(30)=0
	LIMINF(31)=0  
	LIMINF(32)=0
	LIMINF(33)=0
	LIMINF(34)=0
	LIMINF(35)=0
	LIMINF(36)=0
	LIMINF(37)=0
	LIMINF(38)=0
	LIMINF(39)=0
	LIMINF(40)=0    
	LIMINF(41)=0    
	LIMINF(42)=0    
	LIMINF(43)=0 
	
C	soglia superiore per tutte le variabili
	
	LIMSUP(1)=2500
	LIMSUP(2)=9999
	LIMSUP(3)=9
	LIMSUP(4)=360
	LIMSUP(5)=199
	LIMSUP(6)= 490
	LIMSUP(7)= 490
	LIMSUP(8)=100
	LIMSUP(9)=10600
	LIMSUP(10)=6000
	LIMSUP(11)=10600
	LIMSUP(12)=8
	LIMSUP(13)=60
	LIMSUP(14)=9890
	LIMSUP(15)=12
	LIMSUP(16)=99
	LIMSUP(17)=99
	LIMSUP(18)=9
	LIMSUP(19)=9
	LIMSUP(20)=9
	LIMSUP(21)=9
	LIMSUP(22)= 490
	LIMSUP(23)=490
	LIMSUP(24)=99
	LIMSUP(25)=2000
	LIMSUP(26)=99
	LIMSUP(27)=2000
	LIMSUP(28)=9
	LIMSUP(29)= 490
	LIMSUP(30)=9
	LIMSUP(31)=9960
	LIMSUP(32)=9
	LIMSUP(33)=8
	LIMSUP(34)=8
	LIMSUP(35)=8
	LIMSUP(36)=9
	LIMSUP(37)=9
	LIMSUP(38)=9
	LIMSUP(39)=9
	LIMSUP(40)=22500
	LIMSUP(41)=22500
	LIMSUP(42)=22500
	LIMSUP(43)=22500

	ier=0

	CALL GETHEA (STAZ,DATA,ORA,TIPO,RRW)

	IF (TIPO.NE.1)THEN
		IER=1
		RETURN
	END IF

C	leggo i dati anagrafici solo se non sono gia` stati letti

	call GETSTAZ1(staz,ANOME,HHSLM,HHPOZ,ALAT,ALON,IER)
C	
C	Serve ad associare ad una stazione i relativi parametri anagrafici
C	Letti nell' archivio meteodata.
C
C	Input	:	STAZ	= I*4	Codice W.M.O. stazione
C
C	Output	:	ANOME   = C*24	Nome della stazione
C			HHSLM	= R*4	Altezza stazione S.L.M.(m.)
C			HHPOZ	= R*4   Altezza del pozzetto (m.)
C			ALAT	= R*4	Latitudine ( Gradi,primi)
C			ALON	= R*4	Longitudine (Gradi,primi)

c	type*,'estrastaz ier=',ier

C	limite per la direzione vento se il vento e` variabile
	IF (RRW(10).EQ.999)LIMSUP(4)=999

c	limiti per pressione al livello della stazione
	IF (HHPOZ.LT.-100.)HHPOZ=HHSLM	!se non c'e` altezza pozzetto uso
					!altezza stazione
	IF (HHPOZ.LT.-100.)THEN
		IER=-1
		GOTO 22
	END IF

C		!ATMOSFERA NACA 1/10 mb
	PRESNACA=10*EXP(5.2568*LOG(1.-(HHPOZ/44308.))+6.9209)	!H< 10769m.
c	PRESNACA=10*EXP(7.1450487-(HHPOZ/6381.6))		!H> 10769m.
c	TYPE*,'PRESNACA=',PRESNACA
	LIMINF(11)=PRESNACA-1330
	LIMSUP(11)=PRESNACA+670

C	limiti per lo spessore neve

	IF (ABS(ALAT).GT.48.OR.HHPOZ.GT.500)GOTO 22
C	solo per stazioni nel sud Europa e sotto i 500m.

	do imes=5,9				!da Maggio a Settembre
		if (data(2).eq.imes)then	!solo per i mesi estivi
			LIMSUP(31)=0
		end if
	end do


22	CONTINUE

	IF (WMO.NE.STAZ)THEN	!controllo che i dati nel common
				!siano della mia stazione
		IER=-1
		GOTO 24
	END IF

	IF(VENT.GE.0)GOTO 23	! se non c'e` la radiazione solare

	IF (ALAT.LT.-90.OR.ALON.LT.-360)THEN
		IER=-1
		GOTO 23
	END IF

	IF (HHPOZ.LT.0)HHPOZ=0.

	CALL RADMAX(DATA(1),DATA(2),DATA(3),0,0,ORA(1),ORA(2),
	1		ALAT,ALON,HHPOZ,VALMAX,VALMIN,ier)

	LIMINF(26)=NINT(VALMIN)			!con cielo coperto
	LIMSUP(26)=NINT(VALMAX)			!con cielo sereno

23	CONTINUE
c	limiti per geopotenziale

	IF      (SISO.EQ.850)THEN
		LIMINF(10)=500
		LIMSUP(10)=1800
	ELSE IF (SISO.EQ.700)THEN
		LIMINF(10)=2150
		LIMSUP(10)=3450
	ELSE IF	(SISO.EQ.500)THEN
		LIMINF(10)=4700
		LIMSUP(10)=6000
	END IF
	
24	CONTINUE	!FINE CASI PARTICOLRI INIZIO CONTROLLO

	DO NVAR=7,49

	  IF (RRW(NVAR).LT.LIMINF(NVAR-6))FLAG(NVAR-6)=FLAG(NVAR-6)+3

	  IF (RRW(NVAR).GT.LIMSUP(NVAR-6))FLAG(NVAR-6)=FLAG(NVAR-6)+3
		
	END DO

	RETURN
	END
