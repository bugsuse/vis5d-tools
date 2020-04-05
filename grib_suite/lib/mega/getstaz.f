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

	SUBROUTINE GETSTAZ1(IWMO,ANOME,HHSLM,HHPOZ,ALAT,ALON,IER)

COMSTART GETSTAZ1
C	SUBROUTINE GETSTAZ1(IWMO,ANOME,HHSLM,HHPOZ,ALAT,ALON,IER)
C	
C	Serve ad associare ad una stazione i relativi parametri anagrafici
C	Letti nell' archivio meteodata.
C	Se il record risulta allocato da un altro utente ne attende la
c	liberazione.
C
C	Input	:	IWMO	= I*4	Codice W.M.O. stazione
C
C	Output	:	ANOME   = C*24	Nome della stazione
C			HHSLM	= R*4	Altezza stazione S.L.M.(m.)
C			HHPOZ	= R*4   Altezza del pozzetto (m.)
C			ALAT	= R*4	Latitudine ( Gradi,primi)
C			ALON	= R*4	Longitudine (Gradi,primi)
C			IER	= I*4	0=O.K.; >0=errore fatale; 
c					<0=segnalazione errore triviale
C
C	Chiamate 		: nessuna
C	COMMON	Utilizzati	: /CONTRO/	CONTRO(12) CONTIENE IL NUMERO DI
C						UNITA` DA UTILIZZARE
C				  /ESTRASTAZ/	SEZ2 CON TUTTI I DATI ANAGRAFICI
C						DELLA STAZIONE CORRETTI SOLO SE
C						SONO STATI TROVATI E LETTI
C
C	Torna con HHSLM,HHPOZ,ALAT,ALON = -9999.9 se non recupera la stazione
c	IER=-2	record non trovato
c	IER=0	tutto O.K.
c	IER=1	errore apertura file archivio stazioni o errore lettura record
COMEND

	LOGICAL ASTAT
	INCLUDE '($FORIOSDEF)'
	INCLUDE 'MET$INC:STAZIONI.INC'
	CHARACTER*24 ANOME

	INTEGER CONTRO(25)
	COMMON /CONTRO/CONTRO
	COMMON /ESTRASTAZ/SEZ2
	EQUIVALENCE (CONTRO(12),IUNI)
	DATA IUNI/72/
	LOGICAL PRIMO
	COMMON /PRIMO/PRIMO
	DATA PRIMO/.TRUE./

	IER=0
C
C	ESTRAGGO I DATI ANAGRAFICI DELLA STAZIONE " ISTAZ "
C
	IF (WMO.EQ.IWMO)GOTO 666	!ho appena estratto questa stazione !!

	IF (.NOT.PRIMO)GOTO 222
	INQUIRE (FILE='meteostaz$dir:STAZIONI.DAT',OPENED=ASTAT,
	1	NUMBER=IUN)

	IF(ASTAT)THEN
		IUNI=IUN
	ELSE
		OPEN (UNIT=IUNI,NAME='meteostaz$dir:STAZIONI.DAT',
	1	ORGANIZATION='INDEXED',
	1	ACCESS='KEYED',READONLY,
     	1	TYPE='OLD',SHARED,ERR=9998)
	END IF
	PRIMO=.FALSE.

222	CONTINUE
C
C	Leggo il nome della stazione, l'altezza, la Latitudine, la Longitudine
C	dal file dell' anagrafica delle stazioni 
C

	READ (UNIT=IUNI,IOSTAT=IOS,KEY=IWMO,KEYID=0)SEZ2


	IF(IOS.EQ.FOR$IOS_SPERECLOC)THEN
C		file locked da altro utente
		CALL LIB$WAIT(2.)
		GOTO 222
	ELSE IF(IOS.EQ.FOR$IOS_ATTACCNON)THEN
		IER=-2			!il record non esiste
		GOTO 9999
	ELSE IF (IOS.NE.0)THEN
		IER=1			!altri errori
		GOTO 9999
	END IF

c	tutto O.K.
	UNLOCK IUNI

666	ANOME=NOME
	HHSLM=FLOAT(HSLM)
	HHPOZ=FLOAT(HPOZ)
	ALAT=FLOAT(LATI)/100.
	ALON=FLOAT(LONG)/100.
	RETURN

9998	CONTINUE
	IER=1		!errore apertura file archivio

9999	CONTINUE
	ANOME=' '
	HHSLM=-9999.9
	HHPOZ=-9999.9
	ALAT=-9999.9
	ALON=-9999.9
	RETURN
	END



	SUBROUTINE GETSTAZ2(IWMO,ANOME,HHSLM,HHPOZ,ALAT,ALON,IER)

comstart GETSTAZ2
c	SUBROUTINE GETSTAZ2(IWMO,ANOME,HHSLM,HHPOZ,ALAT,ALON,IER)
C	
C	Serve ad estrarre i  parametri anagrafici delle stazioni
C	presenti nell' archivio meteodata.
c	La ricerca viene effettuata in modo sequenziale restituendo ad
c	ogni chiamata una stazione, dalla prima all'ultima.
C	Se il record risulta allocato da un altro utente ne attende la
c	liberazione.
C
C	Output	:	IWMO	= I*4	Codice W.M.O. stazione
c			ANOME   = C*24	Nome della stazione
C			HHSLM	= R*4	Altezza stazione S.L.M.(m.)
C			HHPOZ	= R*4   Altezza del pozzetto (m.)
C			ALAT	= R*4	Latitudine ( Gradi,primi)
C			ALON	= R*4	Longitudine (Gradi,primi)
C			IER	= I*4	0=O.K.; >0=errore fatale; 
c					<0=segnalazione errore triviale
C
C	Chiamate 		: nessuna
C	COMMON	Utilizzati	: /CONTRO/	CONTRO(12) CONTIENE IL NUMERO DI
C						UNITA` DA UTILIZZARE
C				  /ESTRASTAZ/	SEZ2 CON TUTTI I DATI ANAGRAFICI
C						DELLA STAZIONE CORRETTI SOLO SE
C						SONO STATI TROVATI E LETTI
C
C	Torna con HHSLM,HHPOZ,ALAT,ALON = -9999.9 se non recupera la stazione
c	IER=0	tutto O.K.
c	IER=1	errore apertura file archivio stazioni 
C	IER=2	fine file
C	IER=3	altri errori su lettura record
COMEND

	LOGICAL ASTAT
	INCLUDE '($FORIOSDEF)'
	INCLUDE 'MET$INC:STAZIONI.INC'
	CHARACTER*24 ANOME
	CHARACTER NOMEDEV*80

	INTEGER CONTRO(25)
	COMMON /CONTRO/CONTRO
	COMMON /ESTRASTAZ/SEZ2
	EQUIVALENCE (CONTRO(12),IUNI)
	DATA IUNI/72/
	LOGICAL PRIMO
	COMMON /PRIMO/PRIMO
	DATA PRIMO/.TRUE./

	IER=0
C
C	ESTRAGGO I DATI ANAGRAFICI DELLA STAZIONE " ISTAZ "
C
	IF (.NOT.PRIMO)GOTO 222

	INQUIRE (FILE='meteostaz$dir:STAZIONI.DAT',OPENED=ASTAT,
	1	NUMBER=IUN)


	IF(ASTAT)THEN
		IUNI=IUN
	ELSE
		OPEN (UNIT=IUNI,NAME='meteostaz$dir:STAZIONI.DAT',
	1	ORGANIZATION='INDEXED',form='unformatted',
	1	ACCESS='SEQUENTIAL',READONLY,
     	1	TYPE='OLD',SHARED,ERR=9998)
	END IF
	PRIMO=.FALSE.

222	CONTINUE
C
C	Leggo il nome della stazione, l'altezza, la Latitudine, la Longitudine
C	dal file dell' anagrafica delle stazioni 
C

	READ (UNIT=IUNI,IOSTAT=IOS,END=9997)SEZ2

	IF(IOS.EQ.FOR$IOS_SPERECLOC)THEN
C		file locked da altro utente
		CALL LIB$WAIT(2.)
		GOTO 222
	ELSE IF (IOS.NE.0)THEN
		IER=3			!altri errori
		GOTO 9999
	END IF


c	tutto O.K.
	UNLOCK IUNI


666	ANOME=NOME
	IWMO=WMO
	HHSLM=FLOAT(HSLM)
	HHPOZ=FLOAT(HPOZ)
	ALAT=FLOAT(LATI)/100.
	ALON=FLOAT(LONG)/100.
	RETURN

9997	CONTINUE
	IER=2
	GO TO 9999
9998	CONTINUE
	IER=1		!errore apertura file archivio
	GO TO 9999

9999	CONTINUE
	IWMO=-99999
	ANOME=' '
	HHSLM=-9999.9
	HHPOZ=-9999.9
	ALAT=-9999.9
	ALON=-9999.9
	RETURN
	END




	SUBROUTINE GETSTAZ3(IWMO,ANOME,HHSLM,HHPOZ,ALAT,ALON,IER)

COMSTART GETSTAZ3
C	SUBROUTINE GETSTAZ3(IWMO,ANOME,HHSLM,HHPOZ,ALAT,ALON,IER)
C	
C	Serve ad associare ad una stazione i relativi parametri anagrafici
C	Letti nell' archivio meteodata.
c	La ricerca viene effettuata restituendo la stazione richiesta
c	o quella appena superiore trovata.
C	Se il record risulta allocato da un altro utente ne attende la
c	liberazione.
C
C	Input	:	IWMO	= I*4	Codice W.M.O. stazione
C
C	Output	:	IWMO	= I*4	Codice W.M.O. stazione estratta
c			ANOME   = C*24	Nome della stazione
C			HHSLM	= R*4	Altezza stazione S.L.M.(m.)
C			HHPOZ	= R*4   Altezza del pozzetto (m.)
C			ALAT	= R*4	Latitudine ( Gradi,primi)
C			ALON	= R*4	Longitudine (Gradi,primi)
C			IER	= I*4	0=O.K.; >0=errore fatale; 
c					<0=segnalazione errore triviale
C
C	Chiamate 		: nessuna
C	COMMON	Utilizzati	: /CONTRO/	CONTRO(12) CONTIENE IL NUMERO DI
C						UNITA` DA UTILIZZARE
C				  /ESTRASTAZ/	SEZ2 CON TUTTI I DATI ANAGRAFICI
C						DELLA STAZIONE CORRETTI SOLO SE
C						SONO STATI TROVATI E LETTI
C
C	Torna con HHSLM,HHPOZ,ALAT,ALON = -9999.9 se non recupera la stazione
c	IER=-2	record non trovato
c	IER=0	tutto O.K.
c	IER=1	errore apertura file archivio stazioni o errore lettura record
COMEND

	LOGICAL ASTAT
	INCLUDE '($FORIOSDEF)'
	INCLUDE 'MET$INC:STAZIONI.INC'
	CHARACTER*24 ANOME

	INTEGER CONTRO(25)
	COMMON /CONTRO/CONTRO
	COMMON /ESTRASTAZ/SEZ2
	EQUIVALENCE (CONTRO(12),IUNI)
	DATA IUNI/72/
	LOGICAL PRIMO
	COMMON /PRIMO/PRIMO
	DATA PRIMO/.TRUE./

	IER=0
C
C	ESTRAGGO I DATI ANAGRAFICI DELLA STAZIONE " ISTAZ "
C
	IF (WMO.EQ.IWMO)GOTO 666	!ho appena estratto questa stazione !!

	IF (.NOT.PRIMO)GOTO 222
	INQUIRE (FILE='meteostaz$dir:STAZIONI.DAT',OPENED=ASTAT,
	1	NUMBER=IUN)

	IF(ASTAT)THEN
		IUNI=IUN
	ELSE
		OPEN (UNIT=IUNI,NAME='meteostaz$dir:STAZIONI.DAT',
	1	ORGANIZATION='INDEXED',
	1	ACCESS='KEYED',READONLY,
     	1	TYPE='OLD',SHARED,ERR=9998)
	END IF
	PRIMO=.FALSE.

222	CONTINUE
C
C	Leggo il nome della stazione, l'altezza, la Latitudine, la Longitudine
C	dal file dell' anagrafica delle stazioni 
C

	READ (UNIT=IUNI,IOSTAT=IOS,KEYGE=IWMO,KEYID=0)SEZ2


	IF(IOS.EQ.FOR$IOS_SPERECLOC)THEN
C		file locked da altro utente
		CALL LIB$WAIT(2.)
		GOTO 222
	ELSE IF(IOS.EQ.FOR$IOS_ATTACCNON)THEN
		IER=-2			!il record non esiste
		GOTO 9999
	ELSE IF (IOS.NE.0)THEN
		IER=1			!altri errori
		GOTO 9999
	END IF

c	tutto O.K.
	UNLOCK IUNI

666	IWMO=WMO
	ANOME=NOME
	HHSLM=FLOAT(HSLM)
	HHPOZ=FLOAT(HPOZ)
	ALAT=FLOAT(LATI)/100.
	ALON=FLOAT(LONG)/100.
	RETURN

9998	CONTINUE
	IER=1		!errore apertura file archivio

9999	CONTINUE
	IWMO=-99999
	ANOME=' '
	HHSLM=-9999.9
	HHPOZ=-9999.9
	ALAT=-9999.9
	ALON=-9999.9
	RETURN
	END



	SUBROUTINE GETSTAZ4(IWMO,ANOME,HHSLM,HHPOZ,ALAT,ALON,IER)

comstart GETSTAZ4
c	SUBROUTINE GETSTAZ4(IWMO,ANOME,HHSLM,HHPOZ,ALAT,ALON,IER)
C	
C	Serve ad associare ad una stazione i relativi parametri anagrafici
C	Letti nell' archivio meteodata.
c	Esegue un primo accesso a chiave dopo di che, se la stazione richiesta
c	e` superiore a quella precedentemente estratta, scorre il file delle
c	stazioni in modo sequenziale fino a reperire quella richiesta.
c	Se si richiede un nuovo accesso con
c	un numero di codice piu` piccolo viene ripetuto un nuovo accesso a
c	chiave.
C
C	ESEGUE UN PRIMO ACCESSO A CHIAVE E POI PROSEGUE SEQUENZIALE
C	utile solo per scandire le stazioni in maniera ordinata
C
C	Input	:	IWMO	= I*4	Codice W.M.O. stazione
C
C	Output	:	ANOME   = C*24	Nome della stazione
C			HHSLM	= R*4	Altezza stazione S.L.M.(m.)
C			HHPOZ	= R*4   Altezza del pozzetto (m.)
C			ALAT	= R*4	Latitudine ( Gradi,primi)
C			ALON	= R*4	Longitudine (Gradi,primi)
C			IER	= I*4	0=O.K.; >0=errore fatale; 
c					<0=segnalazione errore triviale
C
C	Chiamate 		: GETSTAZ1,GETSTAZ2
C
C	Torna con HHSLM,HHPOZ,ALAT,ALON = -9999.9 se non recupera la stazione
c	IER=-2	record non trovato
c	IER=0	tutto O.K.
c	IER=1	errore apertura file archivio stazioni o errore lettura record
C	IER=2	fine file
C	IER=3	altri errori su lettura record
comend

	CHARACTER*24 ANOME
	DATA IWMOE/99999/

	IF(IWMO.LE.IWMOE)THEN
	CALL GETSTAZ1(IWMO,ANOME,HHSLM,HHPOZ,ALAT,ALON,IER)	!a chiave
	IWMOE=IWMO
	ELSE
10	CALL GETSTAZ2(IWMOE,ANOME,HHSLM,HHPOZ,ALAT,ALON,IER)	!sequenziale
	END IF

	IF (IER.NE.0)RETURN
	
	IF(IWMOE.GT.IWMO)THEN
		IER=-2			! Non trovo i dati anagrafici
		ANOME=' '
		HHSLM=-9999.9
		HHPOZ=-9999.9
		ALAT=-9999.9
		ALON=-9999.9
		RETURN
	END IF
	IF(IWMOE.LT.IWMO)GOTO 10	! devo andare avanti nel file

c	trovati dati anagrafici
	RETURN
	END


	SUBROUTINE GETSTAZ5(ALAMX,ALAMN,ALOMX,ALOMN,HMX,HMN,
	1	   IWMO1,IWMO2,
	1	IWMO,ANOME,HHSLM,HHPOZ,ALAT,ALON,IER)

COMSTART GETSTAZ5
C	SUBROUTINE GETSTAZ5(ALAMX,ALAMN,ALOMX,ALOMN,HMX,HMN,
C	1	   IWMO1,IWMO2,
C	1	IWMO,ANOME,HHSLM,HHPOZ,ALAT,ALON,IER)
C
C	
C	Serve ad estrarre dall'archivio meteodata le stazioni ed i 
C	relativi parametri anagrafici letti dall' archivio meteodata
C	contenute nei limiti definiti lat. lon. alt.
C
C	Input	:	ALATMX	= R*4	Latitudine max finestra
C			ALATMN	= R*4	Latitudine min finestra
C			ALONMX	= R*4	Longitudine max finestra	
C			ALONMN	= R*4	Longitudine min finestra
C			HMX	= R*4	Altezza max finestra
C			HMN	= R*4	Altezza min finestra
C			IWMO1	= I*4	Codice W.M.O. stazione min finestra
C			IWMO2	= I*4	Codice W.M.O. stazione max finestra
C			IWMO    = I*4	inizializzato ad un valore inferiore
C					ad IWMO1 per il primo accesso
C
C	Output	:	IWMO	= I*4	Codice W.M.O. stazione
C			ANOME   = C*24	Nome della stazione
C			HHSLM	= R*4	Altezza stazione S.L.M.(m.)
C			HHPOZ	= R*4   Altezza del pozzetto (m.)
C			ALAT	= R*4	Latitudine ( Gradi,primi)
C			ALON	= R*4	Longitudine (Gradi,primi)
C			IER	= I*4	0=O.K.; >0=errore fatale; 
c					<0=segnalazione errore triviale
C
C	Chiamate 		: nessuna
C	COMMON	Utilizzati	: /CONTRO/	CONTRO(12) CONTIENE IL NUMERO DI
C						UNITA` DA UTILIZZARE
C				  /ESTRASTAZ/	SEZ2 CON TUTTI I DATI ANAGRAFICI
C						DELLA STAZIONE CORRETTI SOLO SE
C						SONO STATI TROVATI E LETTI
C
c	IER=-2	record non trovato
c	IER=0	tutto O.K.
c	IER=1	errore apertura file archivio stazioni 
C	IER=2	fine file
C	IER=3	altri errori su lettura record
COMEND
	CHARACTER*24 ANOME

C
C	ESTRAGGO I DATI ANAGRAFICI DELLA STAZIONE " IWMO "
C
	IF (IWMO.LT.IWMO1)THEN
		IWMO=IWMO1
		CALL GETSTAZ3(IWMO,ANOME,HHSLM,HHPOZ,ALAT,ALON,IER)
		GOTO 20
	END IF

10	CALL GETSTAZ2(IWMO,ANOME,HHSLM,HHPOZ,ALAT,ALON,IER)	!sequenziale

20	CONTINUE
	IF (IER.NE.0)RETURN
	
	IF(IWMO.GT.IWMO2)THEN
		IER=2			! ho superato i limiti
		IWMO=-99999
		ANOME=' '
		HHSLM=-9999.9
		HHPOZ=-9999.9
		ALAT=-9999.9
		ALON=-9999.9
		RETURN
	END IF

c	trovati dati anagrafici
	IF     (ALAT.GE.ALAMN.AND.ALAT.LE.ALAMX.AND.
	1	ALON.GE.ALOMN.AND.ALON.LE.ALOMX.AND.
	1	HHSLM.GE.HMN.AND.HHSLM.LE.HMX)RETURN

	GO TO 10
	
	END



C	LOGICAL ORA(0:9,0:23)
C	CHARACTER*24 ANOME
C	READ (05,*)IWMO
C
C	CALL GETSTAZ6(IWMO,ANOME,HHSLM,HHPOZ,ALAT,ALON,ORA,IER)
C	PRINT *,IER
C	DO IT=0,23
C		PRINT *,IT,(ORA(IM,IT),IM=0,9)
C	END DO
C	END

        SUBROUTINE GETSTAZ6(IWMO,ANOME,HHSLM,HHPOZ,ALAT,ALON,ORA,IER)
COMSTART GETSTAZ6
C       SUBROUTINE GETSTAZ6(IWMO,ANOME,HHSLM,HHPOZ,ALAT,ALON,ORA,IER)
C
C	Serve ad estrarre i  parametri anagrafici delle stazioni
C	presenti nell' archivio meteodata.
C	Se il record risulta allocato da un altro utente ne attende la
c	liberazione.
C
C	Output	:	IWMO	= I*4	Codice W.M.O. stazione
c			ANOME   = C*24	Nome della stazione
C			HHSLM	= R*4	Altezza stazione S.L.M.(m.)
C			HHPOZ	= R*4   Altezza del pozzetto (m.)
C			ALAT	= R*4	Latitudine ( Gradi,primi)
C			ALON	= R*4	Longitudine (Gradi,primi)
C			ORA(10,24) logical  .TRUE. se messaggio compilato
c					per i 10 messaggi meteodata nelle 24 ore
C			IER	= I*4	0=O.K.; >0=errore fatale; 
c					<0=segnalazione errore triviale
C
C	Chiamate 		: nessuna
C	COMMON	Utilizzati	: /CONTRO/	CONTRO(12) CONTIENE IL NUMERO DI
C						UNITA` DA UTILIZZARE
C				  /ESTRASTAZ/	SEZ2 CON TUTTI I DATI ANAGRAFICI
C						DELLA STAZIONE CORRETTI SOLO SE
C						SONO STATI TROVATI E LETTI
C
C	Torna con HHSLM,HHPOZ,ALAT,ALON = -9999.9 se non recupera la stazione
c	IER=0	tutto O.K.
c	IER=1	errore apertura file archivio stazioni 
C	IER=2	fine file
C	IER=3	altri errori su lettura record
COMEND

	LOGICAL ASTAT
	INCLUDE '($FORIOSDEF)'
	INCLUDE 'MET$INC:STAZIONI.INC'
	CHARACTER*24 ANOME

	LOGICAL ORA(10,0:23)
	BYTE BUF(92)
	INTEGER*4	MASKR(10)
	EQUIVALENCE (BUF,SEZ2)
	EQUIVALENCE	(BUF(53),MASKR)

	INTEGER CONTRO(25)
	COMMON /CONTRO/CONTRO
	COMMON /ESTRASTAZ/SEZ2
	EQUIVALENCE (CONTRO(12),IUNI)
	DATA IUNI/72/
	LOGICAL PRIMO,BITR
	COMMON /PRIMO/PRIMO
	DATA PRIMO/.TRUE./

	IER=0
C
C	ESTRAGGO I DATI ANAGRAFICI DELLA STAZIONE " ISTAZ "
C
	IF (WMO.EQ.IWMO)GOTO 666	!ho appena estratto questa stazione !!

	IF (.NOT.PRIMO)GOTO 222
	INQUIRE (FILE='meteostaz$dir:STAZIONI.DAT',OPENED=ASTAT,
	1	NUMBER=IUN)

	IF(ASTAT)THEN
		IUNI=IUN
	ELSE
		OPEN (UNIT=IUNI,NAME='meteostaz$dir:STAZIONI.DAT',
	1	ORGANIZATION='INDEXED',
	1	ACCESS='KEYED',READONLY,
     	1	TYPE='OLD',SHARED,ERR=9998)
	END IF
	PRIMO=.FALSE.

222	CONTINUE
C
C	Leggo il nome della stazione, l'altezza, la Latitudine, la Longitudine
C	dal file dell' anagrafica delle stazioni 
C

	READ (UNIT=IUNI,IOSTAT=IOS,KEY=IWMO,KEYID=0) BUF

	IF(IOS.EQ.FOR$IOS_SPERECLOC)THEN
C		file locked da altro utente
		CALL LIB$WAIT(2.)
		GOTO 222
	ELSE IF(IOS.EQ.FOR$IOS_ATTACCNON)THEN
		IER=-2			!il record non esiste
		GOTO 9999
	ELSE IF (IOS.NE.0)THEN
		IER=1			!altri errori
		GOTO 9999
	END IF

c	tutto O.K.
	UNLOCK IUNI

666	ANOME=NOME
	HHSLM=FLOAT(HSLM)
	HHPOZ=FLOAT(HPOZ)
	ALAT=FLOAT(LATI)/100.
	ALON=FLOAT(LONG)/100.

	DO M=1,10
	  DO IT=0,23
		ORA(M,IT)=BITR(MASKR(M),IT)
	  END DO
	END DO

	RETURN

9998	CONTINUE
	IER=1		!errore apertura file archivio

9999	CONTINUE
	ANOME=' '
	HHSLM=-9999.9
	HHPOZ=-9999.9
	ALAT=-9999.9
	ALON=-9999.9
	RETURN
	END
C
C_______
        SUBROUTINE GETSTAZ7(IWMO,ANOME,HHSLM,HHPOZ,ALAT,ALON,ORA,IER)

COMSTART GETSTAZ7
C       SUBROUTINE GETSTAZ7(IWMO,ANOME,HHSLM,HHPOZ,ALAT,ALON,ORA,IER)
C
C	Serve ad estrarre i  parametri anagrafici delle stazioni
C	presenti nell' archivio meteodata.
c	La ricerca viene effettuata in modo sequenziale restituendo ad
c	ogni chiamata una stazione, dalla prima all'ultima.
C	Se il record risulta allocato da un altro utente ne attende la
c	liberazione.
C
C	Output	:	IWMO	= I*4	Codice W.M.O. stazione
c			ANOME   = C*24	Nome della stazione
C			HHSLM	= R*4	Altezza stazione S.L.M.(m.)
C			HHPOZ	= R*4   Altezza del pozzetto (m.)
C			ALAT	= R*4	Latitudine ( Gradi,primi)
C			ALON	= R*4	Longitudine (Gradi,primi)
C			ORA(10,24) logical  .TRUE. se messaggio compilato
c					per i 10 messaggi meteodata nelle 24 ore
C			IER	= I*4	0=O.K.; >0=errore fatale; 
c					<0=segnalazione errore triviale
C
C	Chiamate 		: nessuna
C	COMMON	Utilizzati	: /CONTRO/	CONTRO(12) CONTIENE IL NUMERO DI
C						UNITA` DA UTILIZZARE
C				  /ESTRASTAZ/	SEZ2 CON TUTTI I DATI ANAGRAFICI
C						DELLA STAZIONE CORRETTI SOLO SE
C						SONO STATI TROVATI E LETTI
C
C	Torna con HHSLM,HHPOZ,ALAT,ALON = -9999.9 se non recupera la stazione
c	IER=0	tutto O.K.
c	IER=1	errore apertura file archivio stazioni 
C	IER=2	fine file
C	IER=3	altri errori su lettura record
COMEND
	LOGICAL ASTAT
	INCLUDE '($FORIOSDEF)'
	INCLUDE 'MET$INC:STAZIONI.INC'
	CHARACTER*24 ANOME
	CHARACTER NOMEDEV*80

	BYTE  BUFF(92)
	LOGICAL  ORA(10,0:23)
	INTEGER*4   MASKR(10)
	EQUIVALENCE   (BUFF,SEZ2)
	EQUIVALENCE   (BUFF(53),MASKR)
	LOGICAL BITR
C
	INTEGER CONTRO(25)
	COMMON /CONTRO/CONTRO
	COMMON /ESTRASTAZ/SEZ2
	EQUIVALENCE (CONTRO(12),IUNI)
	DATA IUNI/72/
	LOGICAL PRIMO
	COMMON /PRIMO/PRIMO
	DATA PRIMO/.TRUE./

	IER=0
C
C	ESTRAGGO I DATI ANAGRAFICI DELLA STAZIONE " ISTAZ "
C
	IF (.NOT.PRIMO) GOTO 222

	INQUIRE (FILE='meteostaz$dir:STAZIONI.DAT',OPENED=ASTAT,
	1	NUMBER=IUN)


	IF(ASTAT)THEN
		IUNI=IUN
	ELSE
		OPEN (UNIT=IUNI,NAME='meteostaz$dir:STAZIONI.DAT',
	1	ORGANIZATION='INDEXED',form='unformatted',
	1	ACCESS='SEQUENTIAL',READONLY,
     	1	TYPE='OLD',SHARED,ERR=9998)
	END IF
	PRIMO=.FALSE.

222	CONTINUE
C
C	Leggo il nome della stazione, l'altezza, la Latitudine, la Longitudine
C	dal file dell' anagrafica delle stazioni 
C

	READ (UNIT=IUNI,IOSTAT=IOS,END=9997) BUFF

	IF(IOS.EQ.FOR$IOS_SPERECLOC)THEN
C		file locked da altro utente
		CALL LIB$WAIT(2.)
		GOTO 222
	ELSE IF (IOS.NE.0)THEN
		IER=3			!altri errori
		GOTO 9999
	END IF


c	tutto O.K.
	UNLOCK IUNI


666	ANOME=NOME
	IWMO=WMO
	HHSLM=FLOAT(HSLM)
	HHPOZ=FLOAT(HPOZ)
	ALAT=FLOAT(LATI)/100.
	ALON=FLOAT(LONG)/100.

	DO M=1,10
	  DO IT=0,23
		ORA(M,IT)=BITR(MASKR(M),IT)
	  ENDDO
	ENDDO

	RETURN
9997	CONTINUE
	IER=2
	GO TO 9999
9998	CONTINUE
	IER=1		!errore apertura file archivio
	GO TO 9999

9999	CONTINUE
	IWMO=-99999
	ANOME=' '
	HHSLM=-9999.9
	HHPOZ=-9999.9
	ALAT=-9999.9
	ALON=-9999.9
	RETURN
	END
C

	SUBROUTINE GETSTAZ1NG(IWMO,ANOME,HHSLM,HHPOZ,ALAT,ALON,IER)

COMSTART GETSTAZ1NG
C	SUBROUTINE GETSTAZ1NG(IWMO,ANOME,HHSLM,HHPOZ,ALAT,ALON,IER)
C	
C	Serve ad associare ad una stazione i relativi parametri anagrafici
C	Letti nell' archivio meteodata.
C	Se il record risulta allocato da un altro utente restituisce il
c	controllo al programma chiamante comunicando la condizione.
C
C	Input	:	IWMO	= I*4	Codice W.M.O. stazione
C
C	Output	:	ANOME   = C*24	Nome della stazione
C			HHSLM	= R*4	Altezza stazione S.L.M.(m.)
C			HHPOZ	= R*4   Altezza del pozzetto (m.)
C			ALAT	= R*4	Latitudine ( Gradi,primi)
C			ALON	= R*4	Longitudine (Gradi,primi)
C			IER	= I*4	0=O.K.; >0=errore fatale; 
c					<0=segnalazione errore triviale
C
C	Chiamate 		: nessuna
C	COMMON	Utilizzati	: /CONTRO/	CONTRO(12) CONTIENE IL NUMERO DI
C						UNITA` DA UTILIZZARE
C				  /ESTRASTAZ/	SEZ2 CON TUTTI I DATI ANAGRAFICI
C						DELLA STAZIONE CORRETTI SOLO SE
C						SONO STATI TROVATI E LETTI
C
C	Torna con HHSLM,HHPOZ,ALAT,ALON = -9999.9 se non recupera la stazione
c	IER=0	tutto O.K.
c	IER=1	errore apertura file archivio stazioni o errore lettura record
c	IER=-1	errore accesso record perche` il record risulta locked da
c		un altro utente (attendere e riprovare)
c	IER=-2	record non trovato
COMEND

	LOGICAL ASTAT
	INCLUDE '($FORIOSDEF)'
	INCLUDE 'MET$INC:STAZIONI.INC'
	CHARACTER*24 ANOME

	INTEGER CONTRO(25)
	COMMON /CONTRO/CONTRO
	COMMON /ESTRASTAZ/SEZ2
	EQUIVALENCE (CONTRO(12),IUNI)
	DATA IUNI/72/
	LOGICAL PRIMO
	COMMON /PRIMO/PRIMO
	DATA PRIMO/.TRUE./

	IER=0
C
C	ESTRAGGO I DATI ANAGRAFICI DELLA STAZIONE " ISTAZ "
C
	IF (WMO.EQ.IWMO)GOTO 666	!ho appena estratto questa stazione !!

	IF (.NOT.PRIMO)GOTO 222
	INQUIRE (FILE='meteostaz$dir:STAZIONI.DAT',OPENED=ASTAT,
	1	NUMBER=IUN)


	IF(ASTAT)THEN
		IUNI=IUN
	ELSE
		OPEN (UNIT=IUNI,NAME='meteostaz$dir:STAZIONI.DAT',
	1	ORGANIZATION='INDEXED',
	1	ACCESS='KEYED',READONLY,
     	1	TYPE='OLD',SHARED,ERR=9998)
	END IF
	PRIMO=.FALSE.

222	CONTINUE
C
C	Leggo il nome della stazione, l'altezza, la Latitudine, la Longitudine
C	dal file dell' anagrafica delle stazioni 
C

	READ (UNIT=IUNI,IOSTAT=IOS,KEY=IWMO,KEYID=0)SEZ2

	IF(IOS.EQ.FOR$IOS_SPERECLOC)THEN
		IER=-1			!file locked da altro utente
		GOTO 9999
	ELSE IF(IOS.EQ.FOR$IOS_ATTACCNON)THEN
		IER=-2			!il record non esiste
		GOTO 9999
	ELSE IF (IOS.NE.0)THEN
		IER=1			!altri errori
		GOTO 9999
	END IF

c	tutto O.K.
	UNLOCK IUNI

666	ANOME=NOME
	HHSLM=FLOAT(HSLM)
	HHPOZ=FLOAT(HPOZ)
	ALAT=FLOAT(LATI)/100.
	ALON=FLOAT(LONG)/100.
	RETURN

9998	CONTINUE
	IER=1		!errore apertura file archivio

9999	CONTINUE
	ANOME=' '
	HHSLM=-9999.9
	HHPOZ=-9999.9
	ALAT=-9999.9
	ALON=-9999.9
	RETURN
	END



	SUBROUTINE GETSTAZ2NG(IWMO,ANOME,HHSLM,HHPOZ,ALAT,ALON,IER)

comstart GETSTAZ2NG
C	SUBROUTINE GETSTAZ2NG(IWMO,ANOME,HHSLM,HHPOZ,ALAT,ALON,IER)
C	
C	Serve ad associare ad una stazione i relativi parametri anagrafici
C	Letti nell' archivio meteodata.
c	La ricerca viene effettuata in modo sequenziale restituendo ad
c	ogni chiamata una stazione, dalla prima all'ultima.
C	Se il record risulta allocato da un altro utente restituisce il
c	controllo al programma chiamante comunicando la condizione.
C
C	Output	:	IWMO	= I*4	Codice W.M.O. stazione
c			ANOME   = C*24	Nome della stazione
C			HHSLM	= R*4	Altezza stazione S.L.M.(m.)
C			HHPOZ	= R*4   Altezza del pozzetto (m.)
C			ALAT	= R*4	Latitudine ( Gradi,primi)
C			ALON	= R*4	Longitudine (Gradi,primi)
C			IER	= I*4	0=O.K.; >0=errore fatale; 
c					<0=segnalazione errore triviale
C
C	Chiamate 		: nessuna
C	COMMON	Utilizzati	: /CONTRO/	CONTRO(12) CONTIENE IL NUMERO DI
C						UNITA` DA UTILIZZARE
C				  /ESTRASTAZ/	SEZ2 CON TUTTI I DATI ANAGRAFICI
C						DELLA STAZIONE CORRETTI SOLO SE
C						SONO STATI TROVATI E LETTI
C
C	Torna con HHSLM,HHPOZ,ALAT,ALON = -9999.9 se non recupera la stazione
c	IER=-1	errore accesso record perche` il record risulta locked da
c		un altro utente (attendere e riprovare)
c	IER=0	tutto O.K.
c	IER=1	errore apertura file archivio stazioni 
C	IER=2	fine file
C	IER=3	altri errori su lettura record
COMEND

	LOGICAL ASTAT
	INCLUDE '($FORIOSDEF)'
	INCLUDE 'MET$INC:STAZIONI.INC'
	CHARACTER*24 ANOME
	CHARACTER NOMEDEV*80

	INTEGER CONTRO(25)
	COMMON /CONTRO/CONTRO
	COMMON /ESTRASTAZ/SEZ2
	EQUIVALENCE (CONTRO(12),IUNI)
	DATA IUNI/72/
	LOGICAL PRIMO
	COMMON /PRIMO/PRIMO
	DATA PRIMO/.TRUE./

	IER=0
C
C	ESTRAGGO I DATI ANAGRAFICI DELLA STAZIONE " ISTAZ "
C
	IF (.NOT.PRIMO)GOTO 222

	INQUIRE (FILE='meteostaz$dir:STAZIONI.DAT',OPENED=ASTAT,
	1	NUMBER=IUN)


	IF(ASTAT)THEN
		IUNI=IUN
	ELSE
		OPEN (UNIT=IUNI,NAME='meteostaz$dir:STAZIONI.DAT',
	1	ORGANIZATION='INDEXED',form='unformatted',
	1	ACCESS='SEQUENTIAL',READONLY,
     	1	TYPE='OLD',SHARED,ERR=9998)
	END IF
	PRIMO=.FALSE.

222	CONTINUE
C
C	Leggo il nome della stazione, l'altezza, la Latitudine, la Longitudine
C	dal file dell' anagrafica delle stazioni 
C

	READ (UNIT=IUNI,IOSTAT=IOS,END=9997)SEZ2

	IF(IOS.EQ.FOR$IOS_SPERECLOC)THEN
		IER=-1			!file locked da altro utente
		GOTO 9999
	ELSE IF (IOS.NE.0)THEN
		IER=3			!altri errori
		GOTO 9999
	END IF


c	tutto O.K.
	UNLOCK IUNI

666	ANOME=NOME
	IWMO=WMO
	HHSLM=FLOAT(HSLM)
	HHPOZ=FLOAT(HPOZ)
	ALAT=FLOAT(LATI)/100.
	ALON=FLOAT(LONG)/100.
	RETURN

9997	CONTINUE
	IER=2
	GO TO 9999
9998	CONTINUE
	IER=1		!errore apertura file archivio
	GO TO 9999

9999	CONTINUE
	IWMO=-99999
	ANOME=' '
	HHSLM=-9999.9
	HHPOZ=-9999.9
	ALAT=-9999.9
	ALON=-9999.9
	RETURN
	END



D	SUBROUTINE GETSTAZ1MG(IWMO,ANOME,HHSLM,HHPOZ,ALAT,ALON,IER)
DC	
DC	Serve ad associare ad una stazione i relativi parametri anagrafici
DC	Letti nell' archivio meteodata.
DC
DC	Input	:	IWMO	= I*4	Codice W.M.O. stazione
DC
DC	Output	:	ANOME    = C*24	Nome della stazione
DC			HHSLM	= R*4	Altezza stazione S.L.M.(m.)
DC			HHPOZ	= R*4   Altezza del pozzetto (m.)
DC			ALAT	= R*4	Latitudine ( Gradi,primi)
DC			ALON	= R*4	Longitudine (Gradi,primi)
DC			IER	= I*4	0=O.K.; >0=errore fatale; 
DC					<0=segnalazione errore triviale
DC
DC	Chiamate 		: GETX
DC	COMMON	Utilizzati	: /CONTRO/	CONTRO(12) CONTIENE IL NUMERO DI
DC						UNITA` DA UTILIZZARE
DC				  /ESTRASTAZ/	SEZ2 CON TUTTI I DATI ANAGRAFICI
DC						DELLA STAZIONE CORRETTI SOLO SE
DC						SONO STATI TROVATI E LETTI
DC
DC	Torna con HHSLM,HHPOZ,ALAT,ALON = -9999.9 se non recupera la stazione
Dc	IER=-2	record non trovato
Dc	IER=0	tutto O.K.
Dc	IER=1	errore apertura file archivio stazioni o errore lettura record
Dc	IER=2	errore I/O dal file
D
D	LOGICAL ASTAT
D	INCLUDE 'MET$INC:STAZIONI.INC'
D	CHARACTER ANOME*24,CHAR*1
D
D	INTEGER CONTRO(25)
D	COMMON /CONTRO/CONTRO
D	COMMON /ESTRASTAZ/SEZ2
D	EQUIVALENCE (CONTRO(12),IUNI)
D	DATA IUNI/72/
D	INTEGER*2 IUN2
D	LOGICAL PRIMO
D	COMMON /PRIMO/PRIMO
D	DATA PRIMO/.TRUE./
D
D	IER=0
DC
DC	ESTRAGGO I DATI ANAGRAFICI DELLA STAZIONE " IWMO "
DC
D	IF (WMO.EQ.IWMO)GOTO 666	!ho appena estratto questa stazione !!
D
D	IF (.NOT.PRIMO)GOTO 222
D	INQUIRE (FILE='meteostaz$dir:STAZIONI.DAT',OPENED=ASTAT,
D	1	NUMBER=IUN)
D
D
D	IF(ASTAT)THEN
D		IUNI=IUN
D	ELSE
D		OPEN (UNIT=IUNI,NAME='meteostaz$dir:STAZIONI.DAT',
D	1	ORGANIZATION='INDEXED',
D	1	ACCESS='KEYED',READONLY,
D     	1	TYPE='OLD',SHARED,ERR=9995)
D	END IF
D	PRIMO=.FALSE.
D
D222	CONTINUE
DC
DC	Leggo il nome della stazione, l'altezza, la Latitudine, la Longitudine
DC	dal file dell' anagrafica delle stazioni 
DC
D	
D	IUN2=IUNI
D	CALL GETX (IUN2,0,1,CHAR,IWMO,265,SEZ2,&9996,&9997)
D
Dc	tutto O.K.
D
D666	ANOME=NOME
D	HHSLM=FLOAT(HSLM)
D	HHPOZ=FLOAT(HPOZ)
D	ALAT=FLOAT(LATI)/100.
D	ALON=FLOAT(LONG)/100.
D	RETURN
D
D9995	CONTINUE	!errore apertura file archivio
D	IER=1	
D	GO TO 9999
D9996	CONTINUE	!errore stazione inesistente/record non trovato
D	IER=-2
D	GO TO 9999
D9997	CONTINUE	!errore input/output da file
D	IER=2
D	GO TO 9999
D9999	CONTINUE
D	ANOME=' '
D	HHSLM=-9999.9
D	HHPOZ=-9999.9
D	ALAT=-9999.9
D	ALON=-9999.9
D	RETURN
D	END
D
D
D
D	SUBROUTINE GETSTAZ2MG(IWMO,ANOME,HHSLM,HHPOZ,ALAT,ALON,IER)
D
DC	
DC	Serve ad estrarre dall'archivio meteodata le stazioni ed i 
DC	relativi parametri anagrafici letti dall' archivio meteodata
DC
DC	Output	:	IWMO	= I*4	Codice W.M.O. stazione
DC			ANOME   = C*24	Nome della stazione
DC			HHSLM	= R*4	Altezza stazione S.L.M.(m.)
DC			HHPOZ	= R*4   Altezza del pozzetto (m.)
DC			ALAT	= R*4	Latitudine ( Gradi,primi)
DC			ALON	= R*4	Longitudine (Gradi,primi)
DC			IER	= I*4	0=O.K.; >0=errore fatale; 
Dc					<0=segnalazione errore triviale
DC
DC	Chiamate 		: nessuna
DC	COMMON	Utilizzati	: /CONTRO/	CONTRO(12) CONTIENE IL NUMERO DI
DC						UNITA` DA UTILIZZARE
DC				  /ESTRASTAZ/	SEZ2 CON TUTTI I DATI ANAGRAFICI
DC						DELLA STAZIONE CORRETTI SOLO SE
DC						SONO STATI TROVATI E LETTI
DC
Dc	IER=0	tutto O.K.
Dc	IER=1	errore apertura file archivio stazioni 
DC	IER=2	fine file
DC	IER=3	altri errori su lettura record
D
D	LOGICAL ASTAT
D	INCLUDE 'MET$INC:STAZIONI.INC'
D	CHARACTER*24 ANOME
D
D	INTEGER CONTRO(25)
D	INTEGER*2 IUN2,kid
D	COMMON /CONTRO/CONTRO
D	COMMON /ESTRASTAZ/SEZ2
D	EQUIVALENCE (CONTRO(12),IUNI)
D	DATA IUNI/72/
D	LOGICAL PRIMO
D	COMMON /PRIMO/PRIMO
D	DATA PRIMO/.TRUE./
D
D	IER=0
DC
DC	ESTRAGGO I DATI ANAGRAFICI DELLA STAZIONE " IWMO "
DC
D	IF (.NOT.PRIMO)GOTO 222
D
D	INQUIRE (FILE='meteostaz$dir:STAZIONI.DAT',OPENED=ASTAT,
D	1	NUMBER=IUN)
D
D	IF(ASTAT)THEN
D		IUNI=IUN
D	ELSE
D		OPEN (UNIT=IUNI,NAME='meteostaz$dir:STAZIONI.DAT',
D	1	ORGANIZATION='INDEXED',form='unformatted',
D	1	ACCESS='SEQUENTIAL',READONLY,
D     	1	TYPE='OLD',SHARED,ERR=9995)
D	END IF
D	PRIMO=.FALSE.
D
D222	CONTINUE
DC
DC	Leggo il nome della stazione, l'altezza, la Latitudine, la Longitudine
DC	dal file dell' anagrafica delle stazioni 
DC
D	IUN2=IUNI
D	kid=-1
D100	CALL GETX (IUN2,kid,1,CHAR,00000,52,SEZ2,&9996,&9997)
D
Dc	tutto O.K.
D
D666	ANOME=NOME
D	IWMO=WMO
D	HHSLM=FLOAT(HSLM)
D	HHPOZ=FLOAT(HPOZ)
D	ALAT=FLOAT(LATI)/100.
D	ALON=FLOAT(LONG)/100.
D
D9995	CONTINUE	!errore apertura file archivio
D	IER=1	
D	GO TO 9999
D9996	CONTINUE	!ERRORE FINE FILE
D	IER=2
D	GO TO 9999
D9997	CONTINUE	!errore input/output da file
D	IER=3
D	GO TO 9999
D9999	CONTINUE
D	IWMO=-99999
D	ANOME=' '
D	HHSLM=-9999.9
D	HHPOZ=-9999.9
D	ALAT=-9999.9
D	ALON=-9999.9
D
D	RETURN
D	END
D
