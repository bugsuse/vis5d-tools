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





	INTEGER FUNCTION N_GETSDSX(FILAC,CONTRO,PRIMO,ISTAZL,DATAL,ORAL,
	1	ISTAZU,DATAU,ORAU,STAZE,DATAE,ORAE,BUFFER,NREP)

COMSTART N_GETSDSX
C	INTEGER FUNCTION N_GETSDSX(FILAC,CONTRO,PRIMO,STAZL,DATAL,ORAL,
C	1	STAZU,DATAU,ORAU,STAZ,DATA,ORA,BUF,NREP)
c
c	Retrieval dal METEODATAbase in sequenza dei records relativi ai reports 
c	compresi nella finestra datal/oral - datau/orau e con sottochiave
c	stazl - stazu. Ogni chiamata rende un report soddisfacente ai limiti
c	impostati. La ricerca viene effettuata contemporaneamente su NREP
c	archivi specificati:   come nome in FILAC,come unita` in CONTRO, 
c	come eventualita` di apertura in PRIMO.
c	Per ogni data quindi vengono restituiti i dati in sequenza per stazione
c	indipendentemente dall'archivio di provenienza. Nel caso esistano dati
c	sincroni della stessa stazione viene restituito un unico report secondo 
c	la priorita` stabilita in FILAC.
c
c	filac(NREP) i/p	 c*100	Nome del file da usare per il retrieval del
c				record.
c				Usato solo se NUN non ha nessun file 
c				aperto su di se', in tal caso causa apertura
c				automatica, o se primo=.true., in tal caso
c				causa chiusura e riapertura automatica.
c				Se il nome non specifica il device e/o
c				directory il default e' METEO$DIR, se il nome
c				non specifica l'estensione in default e' .IND
c				Ad es. filac='SYNOP' causa l'apertura di
c				METEO$DIR:SYNOP.IND .
c 
c	contro(NREP)	i/p	i*4	Unita' da utilizzare nel retrieval.
c				Se nessun file e' aperto su di essa viene
c				automaticamente aperto filac.
c
c	primo(NREP)	i/p 	l*1	Variabile logica per segnalare primo ingresso
c				(da inizializzare a .TRUE., viene
c				automaticamente	posta a .FALSE. all'interno
c				della routine).
c
c	stazl	i/p	i*4	Limite inferiore codice WMO di ricerca per
c				stazione.
c
c	datal(3)/oral(2) i/p i*4 Limite inferiore di ricerca per cronologia
c				(giorno,mese,anno,ore e minuti).
c
c	stazu	i/p	i*4	Limite superiore codice WMO di ricerca per
c				stazione.
c
c	datau(3)/orau(2) i/p i*4 Limite superiore di ricerca per cronologia
c				(giorno,mese,anno,ore e minuti).
c
c	staz	o/p	i*4	Codice WMO della stazione del report
c				reso in buf se n_getsds=0.
c
c	data(3)	o/p	i*4	Vettore contenente rispettivamente
c				giorno,mese ed anno a quattro cifre
c				del report reso in buf se n_getsds=0.
c
c	ora(2)	o/p	i*4	Vettore con ore e minuti del report
c				reso in buf se n_getsds=0.
c
c	buf(n)  o/p	i*2	buffer che conterra' il report letto 
c				dal database se n_getsds=0.
c				(Deve essere dimensionato sulla base del
c				tipo report con n <= 1000).
c
c	n_getsdsx o/p	i*4	= 0 tutto ok - il buffer contiene
c				il primo report in sequenza ricercato
c				= 1 fine ricerca
c				=-1 errore di i/o su file
c				=-2 limiti di ricerca errati
c	nrep	i/p	i*4	numero dei tipi di report su cui fare la
c				ricerca contemporanea.(deve essere <= 20)
c
c	COMMON utilizzati       /ULTIMO/
c	SUBROUTINE utilizzate	JELADATA5,JELADATA6
c	FUNCTION utilizzate	N_GETSDS
COMEND

C	variabili per l'estrazione da meteodata

	PARAMETER ULTIMTIM=2000000000,ULTIMSTAZ=99999
	INTEGER BUFFER(1),BUFFERX(500,20),ITIME(20),FINE
	LOGICAL PRIMO(NREP),FIRST
	INTEGER*4 CONTRO(NREP),ISTAZL,DATAL(3),ORAL(2),
	1	ISTAZU,DATAU(3),ORAU(2),DATAE(3),ORAE(2),ISTAZE(20),
	1	STAZE
	CHARACTER*100 FILAC(NREP)
	COMMON /ULTIMO/JJ,FINE,ITIME,ISTAZE

	FIRST=.TRUE.	!PRIMA VOLTA

	DO I=1,NREP
	  IF(.NOT.PRIMO(I))FIRST=.FALSE.
	END DO

	IF (FIRST)THEN
c	inizializzazione date piu` vecchie per tipo report
	DO I=1,NREP
		ITIME(I)=ULTIMTIM
		ISTAZE(I)=ULTIMSTAZ
	END DO
	FINE=0			!contatore dei file finiti

	END IF


C	ciclo di estrazione dati per i tipi di messaggio
	DO J=1,NREP

	IF (FIRST)THEN
	  GO TO 10			!prima volta leggo tutto
	ELSE
C					!ho gia` letto qualche cosa
	  IF (J.NE.JJ)GOTO 4		!devo rileggere solo dal piu` vecchio
	END IF

10	CONTINUE
C	TYPE*,'LEGGO CON J=',J
C	TYPE*,CONTRO(J),PRIMO(J)

c	*************   per data stazione  **************
	N_GETSDSX=N_GETSDS(FILAC(J),CONTRO(J),PRIMO(J),ISTAZL,DATAL,ORAL,
	1	ISTAZU,DATAU,ORAU,ISTAZE(J),DATAE,ORAE,BUFFERX(1,J))
C	TYPE*,'FINE LETTURA',N_GETSDSX

		IF (N_GETSDSX)1,2,3	!SE <0 ERRORE DI I/O ESCO
					!SE 0  MESSAGGIO TROVATO PROCEDO
					!SE >0 FINE RICERCA

1	CONTINUE
C	TYPE*,'ERRORE GETSSD'
	RETURN

3	FINE=FINE+1		!incremento contatore file finiti
C	TYPE *,'X1 FILE FINITI=',FINE,'  J=',J
	ITIME(J)=ULTIMTIM
	ISTAZE(J)=ULTIMSTAZ
	IF (FINE.LT.NREP)GOTO 4	!ho ancora dei file da scorrere
	N_GETSDSX=1		!termina
	RETURN


2	CONTINUE	!report trovato
C	CALL GETHEA (ISTAZE(J),DATAE,ORAE,TIPOE,BUFFERX(1,J))
	STAZE=ISTAZE(J)
c	TYPE *,'REPORT TROVATO',ISTAZE(j),DATAE,ORAE

	CALL JELADATA5(DATAE(1),DATAE(2),DATAE(3),ORAE(1),ORAE(2),ITIME(J))

4	END DO

	IPRIMTIME=ULTIMTIM
	IPRIMSTAZE=ULTIMSTAZ
	DO J=1,NREP

c	*************   per data stazione  **************

		IF(ITIME(J).LT.IPRIMTIME)THEN
			IPRIMTIME=ITIME(J)		!la precede per data
			IPRIMSTAZE=ISTAZE(J)
			JJ=J			!O.K. sei la prima
		ELSE IF(ITIME(J).EQ.IPRIMTIME.AND.ISTAZE(J).LT.IPRIMSTAZE)THEN
			IPRIMTIME=ITIME(J)	!uguale data la precede per stazione
			IPRIMSTAZE=ISTAZE(J)
			JJ=J			!O.K. sei la prima
		ELSE IF (ITIME(J).EQ.IPRIMTIME.AND.ISTAZE(J).EQ.IPRIMSTAZE.
	1		 AND.IPRIMTIME.LT.ULTIMTIM)THEN

c	************* fine per data stazione  **************

C	data e stazione uguale --AVANZO CON UNA

c	*************   per data stazione  **************
	N_GETSDSX=N_GETSDS(FILAC(J),CONTRO(J),PRIMO(J),ISTAZL,DATAL,ORAL,
	1	ISTAZU,DATAU,ORAU,ISTAZE(J),DATAE,ORAE,BUFFERX(1,J))

		IF (N_GETSDSX)1,7,6	!SE <0 ERRORE DI I/O ESCO
					!SE 0  MESSAGGIO TROVATO PROCEDO
					!SE >0 FINE RICERCA
		END IF
		GOTO 5

7		CONTINUE	!report trovato
C		CALL GETHEA (ISTAZE(J),DATAE,ORAE,TIPOE,BUFFERX(1,J))
		STAZE=ISTAZE(J)
c		TYPE *,'REPORT TROVATO',ISTAZE(j),DATAE,ORAE

		CALL JELADATA5(DATAE(1),DATAE(2),DATAE(3),
	1			ORAE(1),ORAE(2),ITIME(J))
		GOTO 5

6		FINE=FINE+1		!incremento contatore file finiti
C		TYPE *,'X2 FILE FINITI=',FINE,'  J=',J
		ITIME(J)=ULTIMTIM
		ISTAZE(J)=ULTIMSTAZ
		IF (FINE.LT.NREP)GOTO 5	!ho ancora dei file da scorrere
		N_GETSDSX=1		!termina
		RETURN

5	END DO


C	ho trovato i dati da restituire
	inquire (unit=CONTRO(JJ),recl=lrec)

c	meteo2 !!!!!!!!!!!!!!!!!!!!
	lrec=lrec/4
c	meteo2 !!!!!!!!!!!!!!!!!!!!

	DO J=1,LREC
		BUFFER(J)=BUFFERX(J,JJ)
	END DO
	CALL JELADATA6(DATAE(1),DATAE(2),DATAE(3),
	1		ORAE(1),ORAE(2),ITIME(JJ))
	STAZE=ISTAZE(JJ)

	N_GETSDSX=0
	RETURN

	END
