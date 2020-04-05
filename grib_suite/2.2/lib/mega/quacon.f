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





CC
C	PARAMETER MAX=50
C
C	INTEGER PARA,MESE
C	DIMENSION ISTAZ(MAX),XXX(MAX)
C
CC	stazioni da da cui estrarre i valori synottici
C	DATA ISTAZ/16084,16131,16143,16144,16137,16139,16140,16138,
C	1	16147,16141,16146,16148,16149,16134,16124/
C	DATA I/15/
C
C
C	TYPE*,'PARAMETRO'
C	ACCEPT*,PARA
C	TYPE*,'MESE'
C	ACCEPT*,MESE
C
C	I=1
C2	TYPE*,'STAZIONE',ISTAZ(I)
C	
C	TYPE*,'VALORE'
C	ACCEPT*,XXX(I)
C	IF(XXX(I).EQ.99)GOTO1
C
C	I=I+1
C	GOTO 2
C
C
C
C1	I=I-1
C123	CALL QUACON(PARA,MESE,ISTAZ,XXX,I,IER)
C
C	TYPE*,'IER=',IER
C	END
C

COMSTART QUACON
C	SUBROUTINE QUACON(PARA,MESE,ISTAZ,XXX,NUMSTAZ,GOODYN,IER)
C
C********************************************************************
c Questa routine effettua un controllo di qualita' sui dati di temperatura
c massima minima e media giornaliera sull'area dell'Emilia Romagna
c di tipo:
c climatologico: il dato deve essere comparabile a meno di 3*sigma
c                con un valore climatologico approssimato calcolato
c                in funzione del mese, della latitudine,longitudine e
c                altezza della stazione.
c areale       : il dato deve essere comparabile a meno di 3*sigma
c                con un valore climatologico approssimato calcolato
c                in funzione del mese, della latitudine,longitudine e
c                altezza della stazione piu' lo scarto medio di tutte
c                le altre stazioni dalla loro climatologia.
c Il dato viene considerato errato se tutte le due ipotesi non sono
c verificate. Il dato errato quindi deve discostare di molto dal suo
c valore piu` probabile climatologico ed in piu` le altre stazioni
c dell'area non devono essere concordi mediamente con lo scarto dalla
c climatologia.
c Documentazione rispetto alla funzione per ricostruire la climatologia
c piu' essere ricavata riferendosi all'analisi della temperatura.
c I valori di sigma mensili sono stati calcolati mediando i valori interpolati
c sulle 15 stazioni presenti sul territorio da un grigliato regolare (vedi
c analisi di temperatura) 
c 
C	INGRESSO
C	PARA	INTEGER	*4	
C		PARAMETRO DA CONTROLLARE (1=MAX, 2=MIN, 3=MED)
c
C	MESE	INTEGER*4
C		MESE
c
C	ISTAZ(NUMSTAZ)	INTEGER*4
C	 	CODICE WMO STAZIONI
c
C	XXX(NUMSTAZ)	REAL
C		VALORI DI TEMPERATURA MAX/MIN/MED
C
C
C	NUMSTAZ	INTEGER*4
C		NUMERO DI STAZIONI   (MAX=50)
C
C	IN USCITA
C
C	GOODYN(NUMSTAZ)	LOGICAL
C		TABELLA LOGICA DEI DATI ESAMINATI (VERO DOVE DATO VALIDO)
C
C	IER=1	scelta parametro errata
C	IER=2	errore apertura files
C	IER=3	errore nel mese
C	IER=4	le coordinate delle stazioni escono dalla finestra ammessa
C		o i dati della stazione non sono stati trovati(manca altezza)
C		anche per record locked da altro utente.
C	IER=5	numero di stazioni troppo grande
C
C	COMMON /CONTRO/CONTRO		CONTRO(15) CONTIENE L'UNITA` UTILIZZATA
C					PER LA LETTURA DA FILE (DEFAULT=75)
COMEND
	SUBROUTINE QUACON(PARA,MESE,ISTAZ,XXX,NUMSTAZ,GOODYN,IER)

CC**********************************************************************CC
CC**********************************************************************CC
CC									CC
CC		SERVIZIO METEOROLOGICO REGIONE EMILA ROMAGNA		CC
CC				E.R.S.A.				CC
CC									CC
CC		    UFFICIO INFORMAZIONI E PREVISIONI			CC
CC									CC
CC	PAOLO PATRUNO							CC
CC		 				POMI LUCA		CC
CC	SELVINI ANDREA							CC
CC						BATTAGLIA FABRIZIO	CC
CC									CC
CC									CC
CC	BOLOGNA 1990							CC
CC**********************************************************************CC
CC**********************************************************************CC

	PARAMETER MAX=50

	INTEGER *4 ISTAZ(MAX),NSTAZ
C	INTEGER *4 JSTAZ(MAX),NGOOD
	REAL RLAT(MAX),RLON(MAX),TREGA(12,3)
C	REAL LAG(MAX),LONG(MAX)
	character*24 ANOME

	DIMENSION ITOP(MAX),FGPS(MAX),XXX(MAX)
C	DIMENSION ITOPG(MAX),CLI(MAX),XXG(MAX)

	INTEGER MESE,PARA

	LOGICAL GOODYN(MAX)

	DATA ALATMIN,ALATMAX,ALONMIN,ALONMAX/	!limiti dell'area Emilia R.
	1	43.5,45.5,9.,13./


C	 TABELLA DI 3*SIGMA PER MAX MIN E MED 
	DATA TREGA/11.5,11.8,11.8,11.8,10.8,11.5,9.9,10.0,11.0,11.0,12.3,11.2
	1	  ,12.4,11.0,11.5,9.8,9.1,9.2,9.0,8.4,9.6,9.6,12.0,11.0,
	1	   10.7,9.8,11.2,9.4,8.6,9.3,8.4,8.4,9.2,9.2,11.0,9.9/

	INTEGER CONTRO(25)
	COMMON /CONTRO/CONTRO
	EQUIVALENCE (CONTRO(15),IUN)
	DATA IUN/75/

C	FUNZIONE PER RICOSTRUIRE LA CLIMATOLOGIA (SOLO FUNZIONE DELLA
C	LATITUDINE LONGITUDINE E ALTEZZA
	FF(SP,PP,QP,TP,XCX1,XCX2,XCX3)=SP+PP*XCX1+QP*XCX2+TP*XCX3

	CONV=ACOS(-1.)/180.

	IER=0

	IF(NUMSTAZ.GT.MAX)THEN
	IER=5
	RETURN
	END IF

	IF(MESE.LT.1.OR.MESE.GT.12)THEN
	IER=3
	RETURN
	END IF

C	RLON(NUMSTAZ)	REAL
C		LONGITUDINI CENTESIMALI DELLE STAZIONI

C	RLAT(NUMSTAZ)	REAL
C		LATITUDINI     "          "      "

C	ITOP(NUMSTAZ)	INTEGER*4
C		ALTEZZE     DELLE STAZIONI


C
C	LEGGO I DATI DELLA STAZIONE 
C

	DO I=1,NUMSTAZ

	CALL GETSTAZ1(ISTAZ(I),ANOME,HSLM,HPOZ,ALAT,ALON,IER)
C	Serve ad associare ad una stazione i relativi parametri anagrafici
C	Letti nell' archivio meteodata.
C
C	Input	:	ISTAZ	= I*4	Codice W.M.O. stazione
C
C	Output	:	ANOME   = C*24	Nome della stazione
C			HSLM	= R*4	Altezza stazione S.L.M.(m.)
C			HPOZ	= R*4   Altezza del pozzetto (m.)
C			ALAT	= R*4	Latitudine ( Gradi,primi)
C			ALON	= R*4	Longitudine (Gradi,primi)
C			IER	= I*4	0=O.K.; >0=errore fatale; 
c					<0=segnalazione errore triviale
c	IER=0	tutto O.K.
c	IER=1	errore apertura file archivio stazioni o errore lettura record
c	IER=-1	errore accesso record perche` il record risulta locked da
c		un altro utente (attendere e riprovare)
c	IER=-2	record non trovato

	IF (IER.GT.0)GOTO 123
	IF (IER.LT.0)GOTO 9999

	IF (ALON.LT.-360.OR.ALAT.LT.-90)GOTO 9999
	RLON(I)=SESS_TO_CENT(ALON)
	RLAT(I)=SESS_TO_CENT(ALAT)
	ITOP(I)=INT(HSLM)
	IF (ITOP(I).LE.-999)ITOP(I)=INT(HPOZ)
	IF (ITOP(I).LE.-999)GOTO 9999

	END DO

	GOTO (1,2,3) PARA
	IER=1
	RETURN

1	OPEN(UNIT=IUN,FILE='ANALISITEMPERATURA$DIR:'//
	1	'TABELLAMASSIMA.DAT',STATUS='OLD',ERR=123)
	GOTO 23


2	OPEN(UNIT=IUN,FILE='ANALISITEMPERATURA$DIR:'//
	1	'TABELLAMINIMA.DAT',STATUS='OLD',ERR=123)
	GOTO 23



3	OPEN(UNIT=IUN,FILE='ANALISITEMPERATURA$DIR:'//
	1	'TABELLAMEDIA.DAT',STATUS='OLD',ERR=123)
	GOTO 23

123	IER=2
	RETURN

C	legge i coefficienti della funzione climatologica
23	READ(IUN,131)M,A2,B2,C2,D2
131	FORMAT(1X,I2,30X,4F10.6)
	IF(M.NE.MESE)GO TO 23

	CLOSE (UNIT=IUN)

C   *******     FINE     LETTURA   *************************************

c B:  calcolo del FIRST guess(climatologia) nei punti stazione

	do k=1,NUMSTAZ

	IF(RLON(K).LT.ALONMIN.OR.RLAT(K).LT.ALATMIN.
	1	OR.RLON(K).GT.ALONMAX.OR.RLAT(K).GT.ALATMAX)THEN
9999	IER=4
	RETURN
	END IF

	fgps(k)=FF(A2,B2,C2,D2,rlon(k),float(itop(k)),RLAT(K))
	end do


CC********************************************************
C
C   CONTROLLO DI QUALITA' DEI DATI :
C   VENGONO ESCLUSE QUELLE STAZIONI CHE HANNO UN VALORE
C   DI TEMPERATURA ERRATO SECONDO CERTI CRITERI 
C   DEFINITI NELLA SUBROUTINE QUALITCONTROL
C   SE NUMSTAZ E' IL NUMERO DELLE STAZIONI DI PARTENZA
C   NGOOD E' IL NUMERO DELLE STAZIONI DI ARRIVO

	CALL QUALITCONT(TREGA(MESE,PARA),GOODYN,
	1	NUMSTAZ,RLAT,RLON,ITOP, XXX,ISTAZ,FGPS,
C	1	NGOOD,  LAG, LONG,ITOPG,XXG,JSTAZ,CLI,
	2	MAX)

C	TYPE*,'TREGA=',TREGA(MESE,PARA) ,' NUMSTAZ=',NUMSTAZ
C
C	DO I=1,NUMSTAZ
C	TYPE*,' TEMP=',XXX(I),' STAZ=',ISTAZ(I),' FIRST=',FGPS(I)
C	TYPE*,'RLAT=',RLAT(I),' RLON=(I)',RLON(I),' ITOP=',ITOP(I)
C	END DO
C
C	TYPE*,GOODYN
C
	END



	SUBROUTINE QUALITCONT(TREGA,GOODYN,
	1	 NSTAZ,  RLAT,RLON,ITOP, XXX,ISTAZ,FGPS,
C	1	,NGOOD,  LAG, LONG,ITOPG,XXG,JSTAZ,CLI,
	2	MAX)

	INTEGER *4 ISTAZ(MAX),NSTAZ
C	INTEGER *4 JSTAZ(MAX),NGOOD
	REAL RLAT(MAX),RLON(MAX)
C	REAL LAG(MAX),LONG(MAX)

	DIMENSION ITOP(MAX),FGPS(MAX),XXX(MAX)
C	DIMENSION ITOPG(MAX),CLI(MAX),XXG(MAX)

	LOGICAL GOODYN(MAX)

	DO K=1,NSTAZ
	GOODYN(K)=.FALSE.
	END DO

	DO 123 L=1,NSTAZ

C  CONTROLLO DEI DATI: SE TMIN  O TMAX O TMEDIA >3000 NON
C  CONSIDERA LA STAZIONE
	IF(XXX(L).GT.3000.)GO TO 123

C	        SE ABS(TMIN-FGPS O TMAX-FGPS O TMEDIA-FGPS) >3 VOLTE
C  		LA DEVIAZIONE STANDARD GAMMAME NON CONSIDERO IL DATO

		IF(ABS(XXX(L)-FGPS(L)).LT.TREGA) GOODYN(L)=.TRUE.

123	CONTINUE


C	CONTROLLO SPAZIALE

	DO K=1,NSTAZ

	IF(XXX(K).GT.50.)GO TO 345

	SOMMASCAR=0.
	NGOOD=0

	DO J=1,NSTAZ

	IF(XXX(J).GT.50.OR.K.EQ.J)GO TO 543
		NGOOD=NGOOD+1
		SOMMASCAR=SOMMASCAR+XXX(J)-FGPS(J)

543	END DO

	IF (NGOOD.EQ.0) GO TO 345
	SCARMED=SOMMASCAR/FLOAT(NGOOD)

C	E' STATO CALCOLATO LO SCARTO MEDIO

	IF(ABS(FGPS(K)+SCARMED-XXX(K)).LT.TREGA) GOODYN(K)=.TRUE.

345	END DO	

	RETURN
	END
