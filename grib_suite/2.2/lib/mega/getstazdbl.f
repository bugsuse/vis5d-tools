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





	SUBROUTINE GETSTAZDBL(IWMO,ANOME,HHPOZ,ALAT,ALON,COD,IER)
	CALL GETSTAZDBL1(IWMO,ANOME,HHPOZ,ALAT,ALON,COD,IER)
	RETURN
	END

	SUBROUTINE GETSTAZDBL1(IWMO,ANOME,HHPOZ,ALAT,ALON,COD,IER)
COMSTART GETSTAZDBL1
C	SUBROUTINE GETSTAZDBL1(IWMO,ANOME,HHPOZ,ALAT,ALON,COD,IER)
C	
C	Serve ad associare ad una stazione i relativi parametri anagrafici
C	Letti nell' archivio MATREP
C	Se il record risulta allocato da un altro utente ne attende la
c	liberazione.
C
C	Input	:	IWMO	= I*4	Codice W.M.O. stazione
C
C	Output	:	ANOME   = C*24	Nome della stazione
C			HHPOZ	= R*4   Altezza del pozzetto (m.)
C			ALAT	= R*4	Latitudine ( Gradi,primi)
C			ALON	= R*4	Longitudine (Gradi,primi)
c			COD(18)	= I*4	codici sensori
C			IER	= I*4	0=O.K.; >0=errore fatale; 
c					<0=segnalazione errore triviale
C
C	Chiamate 		: nessuna
C	COMMON	Utilizzati	: /CONTRO/	CONTRO(17) CONTIENE IL NUMERO DI
C						UNITA` DA UTILIZZARE
C
C	Torna con HHPOZ,ALAT,ALON = -9999.9 se non recupera la stazione
c	IER=-2	record non trovato
c	IER=0	tutto O.K.
c	IER=1	errore apertura file archivio stazioni o errore lettura record
COMEND

	LOGICAL ASTAT
	INCLUDE '($FORIOSDEF)'

	CHARACTER*24 ANOME

	INTEGER CONTRO(25)
	COMMON /CONTRO/CONTRO
	EQUIVALENCE (CONTRO(17),IUNI)
	DATA IUNI/77/
	LOGICAL PRIMO
	COMMON /PRIMO/PRIMO
	DATA PRIMO/.TRUE./

	CHARACTER CWMO*6,NOME*24
	INTEGER WMO,HPOZ,COD(18),CODD(18)
	REAL LATI,LONG

	IER=0
C
C	ESTRAGGO I DATI ANAGRAFICI DELLA STAZIONE " ISTAZ "
C
	IF (WMO.EQ.IWMO)GOTO 666	!ho appena estratto questa stazione !!

	IF (.NOT.PRIMO)GOTO 222
	INQUIRE (FILE='mtr$dir:STAZDBL.DAT',OPENED=ASTAT,
	1	NUMBER=IUN)

	IF(ASTAT)THEN
		IUNI=IUN
	ELSE
		OPEN (UNIT=IUNI,NAME='mtr$dir:STAZDBL.DAT',
	1	ORGANIZATION='INDEXED',
	1	ACCESS='KEYED',READONLY,
     	1	TYPE='OLD',SHARED,FORM='FORMATTED',ERR=9998)
	END IF
	PRIMO=.FALSE.

222	CONTINUE
	WRITE (CWMO,'(I6.6)')IWMO
C
C	Leggo il nome della stazione, l'altezza, la Latitudine, la Longitudine
C	dal file dell' anagrafica delle stazioni 
C
	READ (UNIT=IUNI,FMT=33,IOSTAT=IOS,
	1	KEY=CWMO)WMO,IR,LATI,LONG,HPOZ,
	1	NOME,CODD
33	FORMAT(I6,I1,2F5.2,I4,A24,18I3)
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
	HHPOZ=FLOAT(HPOZ)
	ALAT=LATI
	ALON=LONG
	DO I=1,18
		COD(I)=CODD(I)
	END DO
	RETURN

9998	CONTINUE
	IER=1		!errore apertura file archivio

9999	CONTINUE
	ANOME=' '
	HHPOZ=-999.
	ALAT=-999.
	ALON=-999.
	DO I=1,18
		COD(I)=-1
	END DO
	RETURN
	END


	SUBROUTINE GETSTAZDBL2(IWMO,ANOME,HHPOZ,ALAT,ALON,COD,IER)

COMSTART GETSTAZDBL2
C	SUBROUTINE GETSTAZDBL2(IWMO,ANOME,HHPOZ,ALAT,ALON,COD,IER)
C	
C	Serve ad associare ad una stazione i relativi parametri anagrafici
C	Letti nell' archivio MATREP
c	La ricerca viene effettuata in modo sequenziale restituendo
c	ad ogni chiamata una stazione, dalla prima all'ultima.
C	Se il record risulta allocato da un altro utente ne attende la
c	liberazione.
C
C	Input	:	IWMO	= I*4	Codice W.M.O. stazione
C
C	Output	:	ANOME   = C*24	Nome della stazione
C			HHPOZ	= R*4   Altezza del pozzetto (m.)
C			ALAT	= R*4	Latitudine ( Gradi,primi)
C			ALON	= R*4	Longitudine (Gradi,primi)
c			COD(18)	= I*4	codici sensori
C			IER	= I*4	0=O.K.; >0=errore fatale; 
c					<0=segnalazione errore triviale
C
C	Chiamate 		: nessuna
C	COMMON	Utilizzati	: /CONTRO/	CONTRO(17) CONTIENE IL NUMERO DI
C						UNITA` DA UTILIZZARE
C
C	Torna con HHPOZ,ALAT,ALON = -9999.9 se non recupera la stazione
c	IER=0	tutto O.K.
c	IER=1	errore apertura file archivio stazioni
c	IER=2	fine file
c	IER=3	altri errori su lettura record
COMEND

	LOGICAL ASTAT
	INCLUDE '($FORIOSDEF)'

	CHARACTER*24 ANOME

	INTEGER CONTRO(25)
	COMMON /CONTRO/CONTRO
	EQUIVALENCE (CONTRO(17),IUNI)
	DATA IUNI/77/
	LOGICAL PRIMO
	COMMON /PRIMO/PRIMO
	DATA PRIMO/.TRUE./

	CHARACTER CWMO*6,NOME*24
	INTEGER WMO,HPOZ,COD(18),CODD(18)
	REAL LATI,LONG

	IER=0
C
C	ESTRAGGO I DATI ANAGRAFICI DELLA STAZIONE " ISTAZ "
C
	IF (.NOT.PRIMO)GOTO 222
	INQUIRE (FILE='mtr$dir:STAZDBL.DAT',OPENED=ASTAT,
	1	NUMBER=IUN)

	IF(ASTAT)THEN
		IUNI=IUN
	ELSE
		OPEN (UNIT=IUNI,NAME='mtr$dir:STAZDBL.DAT',
	1	ORGANIZATION='INDEXED',
	1	ACCESS='SEQUENTIAL',READONLY,
     	1	TYPE='OLD',SHARED,FORM='FORMATTED',ERR=9998)
	END IF
	PRIMO=.FALSE.

222	CONTINUE
C
C	Leggo il nome della stazione, l'altezza, la Latitudine, la Longitudine
C	dal file dell' anagrafica delle stazioni 
C
	READ (UNIT=IUNI,FMT=33,IOSTAT=IOS,END=9997)
	1	WMO,IR,LATI,LONG,HPOZ,
	1	NOME,CODD
33	FORMAT(I6,I1,2F5.2,I4,A24,18I3)
	IF(IOS.EQ.FOR$IOS_SPERECLOC)THEN
C		file locked da altro utente
		CALL LIB$WAIT(2.)
		GOTO 222
	ELSE IF (IOS.NE.0)THEN
		IER=1			!altri errori
		GOTO 9999
	END IF

c	tutto O.K.
	UNLOCK IUNI

666	ANOME=NOME
	IWMO=WMO
	HHPOZ=FLOAT(HPOZ)
	ALAT=LATI
	ALON=LONG
	DO I=1,18
		COD(I)=CODD(I)
	END DO
	RETURN

9997	CONTINUE
	IER=2
	GOTO 9999

9998	CONTINUE
	IER=1		!errore apertura file archivio

9999	CONTINUE
	ANOME=' '
	IWMO=-99999
	HHPOZ=-999.
	ALAT=-999.
	ALON=-999.
	DO I=1,18
		COD(I)=-1
	END DO
	RETURN
	END




	SUBROUTINE GETSTAZDBL3(IWMO,ANOME,HHPOZ,ALAT,ALON,COD,IER)

COMSTART GETSTAZDBL3
C	SUBROUTINE GETSTAZDBL3(IWMO,ANOME,HHPOZ,ALAT,ALON,COD,IER)
C	
C	Serve ad associare ad una stazione i relativi parametri anagrafici
C	Letti nell' archivio MATREP
C	La ricerca viene effettuata restituendo la stazione richiesta
c	o quella appena superiore trovata.
C	Se il record risulta allocato da un altro utente ne attende la
c	liberazione.
C
C	Input	:	IWMO	= I*4	Codice W.M.O. stazione
C
C	Output	:	ANOME   = C*24	Nome della stazione
C			HHPOZ	= R*4   Altezza del pozzetto (m.)
C			ALAT	= R*4	Latitudine ( Gradi,primi)
C			ALON	= R*4	Longitudine (Gradi,primi)
c			COD(18)	= I*4	codici sensori
C			IER	= I*4	0=O.K.; >0=errore fatale; 
c					<0=segnalazione errore triviale
C
C	Chiamate 		: nessuna
C	COMMON	Utilizzati	: /CONTRO/	CONTRO(17) CONTIENE IL NUMERO DI
C						UNITA` DA UTILIZZARE
C
C	Torna con HHPOZ,ALAT,ALON = -9999.9 se non recupera la stazione
c	IER=-2	record non trovato
c	IER=0	tutto O.K.
c	IER=1	errore apertura file archivio stazioni o errore lettura record
COMEND

	LOGICAL ASTAT
	INCLUDE '($FORIOSDEF)'

	CHARACTER*24 ANOME

	INTEGER CONTRO(25)
	COMMON /CONTRO/CONTRO
	EQUIVALENCE (CONTRO(17),IUNI)
	DATA IUNI/77/
	LOGICAL PRIMO
	COMMON /PRIMO/PRIMO
	DATA PRIMO/.TRUE./

	CHARACTER CWMO*6,NOME*24
	INTEGER WMO,HPOZ,COD(18),CODD(18)
	REAL LATI,LONG

	IER=0
C
C	ESTRAGGO I DATI ANAGRAFICI DELLA STAZIONE " ISTAZ "
C
	IF (WMO.EQ.IWMO)GOTO 666	!ho appena estratto questa stazione !!

	IF (.NOT.PRIMO)GOTO 222
	INQUIRE (FILE='mtr$dir:STAZDBL.DAT',OPENED=ASTAT,
	1	NUMBER=IUN)

	IF(ASTAT)THEN
		IUNI=IUN
	ELSE
		OPEN (UNIT=IUNI,NAME='mtr$dir:STAZDBL.DAT',
	1	ORGANIZATION='INDEXED',
	1	ACCESS='KEYED',READONLY,
     	1	TYPE='OLD',SHARED,FORM='FORMATTED',ERR=9998)
	END IF
	PRIMO=.FALSE.

222	CONTINUE
	WRITE (CWMO,'(I6.6)')IWMO
C
C	Leggo il nome della stazione, l'altezza, la Latitudine, la Longitudine
C	dal file dell' anagrafica delle stazioni 
C
	READ (UNIT=IUNI,FMT=33,IOSTAT=IOS,
	1	KEYGE=CWMO)WMO,IR,LATI,LONG,HPOZ,
	1	NOME,CODD
33	FORMAT(I6,I1,2F5.2,I4,A24,18I3)
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
	IWMO=WMO
	HHPOZ=FLOAT(HPOZ)
	ALAT=LATI
	ALON=LONG
	DO I=1,18
		COD(I)=CODD(I)
	END DO
	RETURN

9998	CONTINUE
	IER=1		!errore apertura file archivio

9999	CONTINUE
	ANOME=' '
	IWMO=-99999
	HHPOZ=-999.
	ALAT=-999.
	ALON=-999.
	DO I=1,18
		COD(I)=-1
	END DO
	RETURN
	END



	SUBROUTINE GETSTAZDBL4(IWMO,ANOME,HHPOZ,ALAT,ALON,COD,IER)

COMSTART GETSTAZDBL4
C	SUBROUTINE GETSTAZDBL4(IWMO,ANOME,HHPOZ,ALAT,ALON,COD,IER)
C	
C	Serve ad associare ad una stazione i relativi parametri anagrafici
C	Letti nell' archivio MATREP
C	Se il record risulta allocato da un altro utente ne attende la
c	liberazione.
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
C
C	Input	:	IWMO	= I*4	Codice W.M.O. stazione
C
C	Output	:	ANOME   = C*24	Nome della stazione
C			HHPOZ	= R*4   Altezza del pozzetto (m.)
C			ALAT	= R*4	Latitudine ( Gradi,primi)
C			ALON	= R*4	Longitudine (Gradi,primi)
c			COD(18)	= I*4	codici sensori
C			IER	= I*4	0=O.K.; >0=errore fatale; 
c					<0=segnalazione errore triviale
C
C	COMMON	Utilizzati	: /CONTRO/	CONTRO(17) CONTIENE IL NUMERO DI
C						UNITA` DA UTILIZZARE
C
C	Chiamate 		: GETSTAZDBL,GETSTAZDBL2
C
C	Torna con HHSLM,HHPOZ,ALAT,ALON = -9999.9 se non recupera la stazione
c	IER=-2	record non trovato
c	IER=0	tutto O.K.
c	IER=1	errore apertura file archivio stazioni o errore lettura record
C	IER=2	fine file
C	IER=3	altri errori su lettura record
comend


	CHARACTER*24 ANOME
	INTEGER COD(18)
	DATA IWMOE/99999/

	IF(IWMO.LE.IWMOE)THEN
	CALL GETSTAZDBL(IWMO,ANOME,HHPOZ,ALAT,ALON,COD,IER)	!a chiave
	IWMOE=IWMO
	ELSE
10	CALL GETSTAZDBL2(IWMOE,ANOME,HHPOZ,ALAT,ALON,COD,IER)	!sequenziale
	END IF

	IF (IER.NE.0)RETURN

	IF(IWMOE.GT.IWMO)THEN
		IER=-2			! Non trovo i dati anagrafici
		ANOME=' '
		IWMO=-99999
		HHPOZ=-999.
		ALAT=-999.
		ALON=-999.
		DO I=1,18
			COD(I)=-1
		END DO
		RETURN
	END IF
	IF(IWMOE.LT.IWMO)GOTO 10	! devo andare avanti nel file

c	trovati dati anagrafici
	RETURN

	END






	SUBROUTINE GETSTAZDBL5(ALAMX,ALAMN,ALOMX,ALOMN,HMX,HMN,
	1	   IWMO1,IWMO2,
	1	IWMO,ANOME,HHPOZ,ALAT,ALON,COD,IER)

COMSTART GETSTAZDBL5
C	SUBROUTINE GETSTAZDBL5(ALAMX,ALAMN,ALOMX,ALOMN,HMX,HMN,
C	1	   IWMO1,IWMO2,
C	1	IWMO,ANOME,HHPOZ,ALAT,ALON,COD,IER)
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
C			HHPOZ	= R*4   Altezza del pozzetto (m.)
C			ALAT	= R*4	Latitudine ( Gradi,primi)
C			ALON	= R*4	Longitudine (Gradi,primi)
c			COD(18)	= I*4	codici sensori
C			IER	= I*4	0=O.K.; >0=errore fatale; 
c					<0=segnalazione errore triviale
C
C	COMMON	Utilizzati	: /CONTRO/	CONTRO(17) CONTIENE IL NUMERO DI
C						UNITA` DA UTILIZZARE
C
C	Chiamate 		: GETSTAZDBL2,GETSTAZDBL3
C
c	IER=-2	record non trovato
c	IER=0	tutto O.K.
c	IER=1	errore apertura file archivio stazioni 
C	IER=2	fine file
C	IER=3	altri errori su lettura record
COMEND
	CHARACTER*24 ANOME
	INTEGER COD(18)

C
C	ESTRAGGO I DATI ANAGRAFICI DELLA STAZIONE " IWMO "
C
	IF (IWMO.LT.IWMO1)THEN
		IWMO=IWMO1
		CALL GETSTAZDBL3
	1	(IWMO,ANOME,HHPOZ,ALAT,ALON,COD,IER)
		GOTO 20
	END IF

10	CALL GETSTAZDBL2
	1	(IWMO,ANOME,HHPOZ,ALAT,ALON,COD,IER)	!sequenziale

20	CONTINUE
	IF (IER.NE.0)RETURN
	
	IF(IWMO.GT.IWMO2)THEN
		IER=2			! ho superato i limiti
		ANOME=' '
		IWMO=-99999
		HHPOZ=-999.
		ALAT=-999.
		ALON=-999.
		DO I=1,18
			COD(I)=-1
		END DO
		RETURN
	END IF

c	trovati dati anagrafici
	IF     (ALAT.GE.ALAMN.AND.ALAT.LE.ALAMX.AND.
	1	ALON.GE.ALOMN.AND.ALON.LE.ALOMX.AND.
	1	HHPOZ.GE.HMN.AND.HHPOZ.LE.HMX)RETURN

	GO TO 10
	
	END
