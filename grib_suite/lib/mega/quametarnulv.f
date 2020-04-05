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





	SUBROUTINE QUAMETARNULV(FLAG,IATT,IER)
COMSTART QUAMETARNULV
C	SUBROUTINE QUAMETARNULV(FLAG,IATT,IER)

c	Routine per annullare i valori ritenuti errati in quanto hanno
c	superato nel contatore di errore quella che viene ritenuta la
c	soglia di accettabilita` del dato.
c	IATT e` questa soglia e il dato viene restituito mancante (32767)
c	se FLAG >= IATT.
c
C	INPUT:
c	FLAG(23)	BYTE	contatore di errore associato alle
c				23 variabili METAR
C	IATT		I*4	solglia di attendibilita` richiesta per dato
c				considerato errato (solitamente 2)
C	OUTPUT:
c	FLAG(23)	BYTE	azzerato se inizialmente uguale o superiore
c				a IATT per le 23 variabili metar
c	IER		I*4	condizione di errore
c			IER=0	tutto O.K.
C			IER=1	tipo messaggio errato

c	COMMON /RRW/RRW	
C	RRW(29) 	I*2	per passare i dati synop come dichiarato
c				in met$inc:metar.inc
COMEND
	COMMON /RRW/RRW

	BYTE FLAG(23)
	INTEGER*2 RRW(29)
	INTEGER STAZ,DATA(3),ORA(2),TIPO

	ier=0

	CALL GETHEA (STAZ,DATA,ORA,TIPO,RRW)

	IF (TIPO.NE.10)THEN
		IER=1
		RETURN
	END IF

	DO NVAR=7,29
	  IF (FLAG(NVAR-6).GE.IATT)THEN
		FLAG(NVAR-6)=0
		RRW(NVAR)=32767
	  END IF		
	END DO
	DO NVAR=15,18			!	copertura nubi
	  IF (FLAG(NVAR-6).GE.IATT)THEN
		FLAG(NVAR-6)=0
		RRW(NVAR)=0
	  END IF		
	END DO

	RETURN
	END
