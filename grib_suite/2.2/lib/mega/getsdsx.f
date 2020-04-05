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






	SUBROUTINE GETSDSX(PRIMO,CONTRO,ISTAZL,DATAL,ORAL,ISTAZU,
	1		DATAU,ORAU,TIPO,BUFFER,NREP,ERRORE)

COMSTART GETSDSX
C	SUBROUTINE GETSDSX(PRIMO,CONTRO,ISTAZL,DATAL,ORAL,ISTAZU,
C	1		DATAU,ORAU,TIPO,BUFFER,NREP,ERRORE)
C
C	routine sorpassata   vedi N_GETSDSX
COMEND

C	variabili per l'estrazione da meteodata

	PARAMETER ULTIMTIM=400000000,ULTIMSTAZ=99999
	INTEGER BUFFER(1),BUFFERX(500,20),ITIME(20),FINE
	LOGICAL PRIMO(NREP),FIRST
	INTEGER*4 CONTRO(1),ERRORE,TIPO(NREP),ISTAZL,DATAL(3),ORAL(2),
	1	ISTAZU,DATAU(3),ORAU(2),DATAE(3),ORAE(2),ISTAZE(20),TIPOE
	INTEGER LREC,IOLT,LRECV(25),IOLTV(25)
	COMMON /ULTIMO/JJ,FINE,ITIME,ISTAZE,LRECV,IOLTV
	COMMON /DBR002/LREC,IOLT


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
C	TYPE*,TIPO(J),CONTRO(J),PRIMO(J)

	LREC=LRECV(J)
	IOLT=IOLTV(J)
c	*************   per data stazione  **************
	CALL GETSDS(PRIMO(J),CONTRO(J),ISTAZL,DATAL,ORAL,ISTAZU,
	1	DATAU,ORAU,TIPO(J),BUFFERX(1,J),ERRORE)
C	TYPE*,'FINE LETTURA',ERRORE

	LRECV(J)=LREC
	IOLTV(J)=IOLT
		IF (ERRORE)1,2,3	!SE <0 ERRORE DI I/O ESCO
					!SE 0  MESSAGGIO TROVATO PROCEDO
					!SE >0 FINE RICERCA

1	TYPE*,'ERRORE GETSSD'
	RETURN

3	FINE=FINE+1		!incremento contatore file finiti
C	TYPE *,'X1 FILE FINITI=',FINE,'  J=',J
	ITIME(J)=ULTIMTIM
	ISTAZE(J)=ULTIMSTAZ
	IF (FINE.LT.NREP)GOTO 4	!ho ancora dei file da scorrere
	ERRORE=1		!termina
	RETURN


2	CONTINUE	!report trovato
	CALL GETHEA (ISTAZE(J),DATAE,ORAE,TIPOE,BUFFERX(1,J))
c	TYPE *,'REPORT TROVATO',ISTAZE(j),DATAE,ORAE,TIPOE

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

		LREC=LRECV(J)
		IOLT=IOLTV(J)

c	*************   per data stazione  **************
		CALL GETSDS(PRIMO(J),CONTRO(J),ISTAZL,DATAL,ORAL,ISTAZU,
	1	DATAU,ORAU,TIPO(J),BUFFERX(1,J),ERRORE)

		LRECV(J)=LREC
		IOLTV(J)=IOLT

		IF (ERRORE)1,7,6	!SE <0 ERRORE DI I/O ESCO
					!SE 0  MESSAGGIO TROVATO PROCEDO
					!SE >0 FINE RICERCA
		END IF
5	END DO


C	ho trovato i dati da restituire

	DO J=1,LRECV(JJ)/4
		BUFFER(J)=BUFFERX(J,JJ)
	END DO
	ERRORE=0
	RETURN

7	CONTINUE	!report trovato
	CALL GETHEA (ISTAZE(J),DATAE,ORAE,TIPOE,BUFFERX(1,J))
c	TYPE *,'REPORT TROVATO',ISTAZE(j),DATAE,ORAE,TIPOE

	CALL JELADATA5(DATAE(1),DATAE(2),DATAE(3),ORAE(1),ORAE(2),ITIME(J))

	GOTO 5

6	FINE=FINE+1		!incremento contatore file finiti
C	TYPE *,'X2 FILE FINITI=',FINE,'  J=',J
	ITIME(J)=ULTIMTIM
	ISTAZE(J)=ULTIMSTAZ
	IF (FINE.LT.NREP)GOTO 5	!ho ancora dei file da scorrere
	ERRORE=1		!termina
	RETURN

	END

