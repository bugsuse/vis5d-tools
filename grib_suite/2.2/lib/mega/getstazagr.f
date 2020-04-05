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





	SUBROUTINE GETSTAZAGR(IWMO,ANOME,HHPOZ,ALAT,ALON,IER)

COMSTART GETSTAZAGR
C	SUBROUTINE GETSTAZAGR(IWMO,ANOME,HHPOZ,ALAT,ALON,IER)
C	
C	Serve ad associare ad una stazione i relativi parametri anagrafici
C	Letti nell' archivio.
C	Se il record risulta allocato da un altro utente ne attende la
c	liberazione.
C
C	Input	:	IWMO	= I*4	Codice W.M.O. stazione
C
C	Output	:	ANOME   = C*24	Nome della stazione
C			HHPOZ	= R*4   Altezza del pozzetto (m.)
C			ALAT	= R*4	Latitudine ( Gradi,primi)
C			ALON	= R*4	Longitudine (Gradi,primi)
C			IER	= I*4	0=O.K.; >0=errore fatale; 
c					<0=segnalazione errore triviale
C
C	Chiamate 		: nessuna
C	COMMON	Utilizzati	: /CONTRO/	CONTRO(18) CONTIENE IL NUMERO DI
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
	EQUIVALENCE (CONTRO(18),IUNI)
	DATA IUNI/78/
	LOGICAL PRIMO
	DATA PRIMO/.TRUE./

	CHARACTER NOME*24
	INTEGER WMO,HPOZ

	IER=0
C
C	ESTRAGGO I DATI ANAGRAFICI DELLA STAZIONE " ISTAZ "
C
	IF (WMO.EQ.IWMO)GOTO 666	!ho appena estratto questa stazione !!

	IF (.NOT.PRIMO)GOTO 222
	INQUIRE (FILE='METEO$DIR:ANAGSAL.DAT',OPENED=ASTAT,
	1	NUMBER=IUN)

	IF(ASTAT)THEN
		IUNI=IUN
	ELSE
		OPEN (UNIT=IUNI,NAME='METEO$DIR:ANAGSAL.DAT',
	1	ORGANIZATION='INDEXED',
	1	ACCESS='KEYED',READONLY,
     	1	TYPE='OLD',SHARED,FORM='UNFORMATTED',
	1	RECORDTYPE='FIXED',ERR=9998)
	END IF
	PRIMO=.FALSE.

222	CONTINUE
C
C	Leggo il nome della stazione, l'altezza, la Latitudine, la Longitudine
C	dal file dell' anagrafica delle stazioni 
C
	READ (UNIT=IUNI,IOSTAT=IOS,
	1	KEY=IWMO)WMO,NOME,LATI,LONG,HPOZ
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
	ALAT=FLOAT(LATI)/100.
	ALON=FLOAT(LONG)/100.
	RETURN

9998	CONTINUE
	IER=1		!errore apertura file archivio

9999	CONTINUE
	ANOME=' '
	HHPOZ=-999.
	ALAT=-999.
	ALON=-999.
	RETURN
	END
