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





	SUBROUTINE QUALOCALNULV(FLAG,IATT,IER)
COMSTART QUALOCALNULV
C	SUBROUTINE QUALOCALNULV(FLAG,IATT,IER)

c	Routine per annullare i valori ritenuti errati in quanto hanno
c	superato nel contatore di errore quella che viene ritenuta la
c	soglia di accettabilita` del dato.
c	IATT e` questa soglia e il dato viene restituito mancante (32767)
c	se FLAG >= IATT.
c
C	INPUT:
c	FLAG(46)	BYTE	contatore di errore associato alle
c				46 variabili locali
C	IATT		I*4	solglia di attendibilita` richiesta per dato
c				considerato errato (solitamente 2)
C	OUTPUT:
c	FLAG(46)	BYTE	azzerato se inizialmente uguale o superiore
c				a IATT per le 46 variabili locali
c	IER		I*4	condizione di errore
c			IER=0	tutto O.K.
C			IER=1	tipo messaggio errato

c	COMMON /RRW/RRW
C	RRW(89)		I*2	RRW(89) per passare i dati local come dichiarato
c				in met$inc:local.inc
COMEND
	COMMON /RRW/RRW

	BYTE FLAG(46)
	INTEGER*2 RRW(89)
	INTEGER STAZ,DATA(2),ORA(2),TIPO

	ier=0

	CALL GETHEA (STAZ,DATA,ORA,TIPO,RRW)

	IF (TIPO.NE.11)THEN
		IER=1
		RETURN
	END IF

	DO NVAR=7,52
	  IF (FLAG(NVAR-6).GE.IATT)THEN
		FLAG(NVAR-6)=0
		RRW(NVAR)=32767
	  END IF		
	END DO

	RETURN
	END
