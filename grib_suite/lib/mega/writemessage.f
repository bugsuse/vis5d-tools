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





	SUBROUTINE WRITEMESSAGE (FILENAME,MESSAGE)

COMSTART WRITEMESSAGE
C	SUBROUTINE WRITEMESSAGE (FILENAME,MESSAGE)
C
C	permette di scrivere un messaggio in append in un file di log
c	gestisce il numero di unita` usato (default=71)che viene lasciato
c	aperto dopo la scrittura. se l'unita` e` gia` utilizzata prova
c	con le unita` successive fino alla prima libera che pero` in questo
c	caso viene subito richiusa e ne viene notificato l'uso nel file.

c	input:
c	FILENAME	CHARACTER*(*)	NOME FILE DI LOG
C	MESSAGE		CHARACTER*(*)	MESSAGGIO DA SCRIVERE

C	CALL		NESSUNA
C	COMMON		/CONTRO/	A CONTRO(11) VIENE ASSEGNATA L'UNITA`
C					DA UTILIZZARE
COMEND

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


	CHARACTER*(*) FILENAME,MESSAGE
	LOGICAL ASTAT
	INTEGER CONTRO(25)
	COMMON /CONTRO/CONTRO
	EQUIVALENCE (CONTRO(11),IUNI)
	DATA IUNI/71/			!UNITA` DI DEFAULT DA UTILIZZARE
	LOGICAL FLAG

	FLAG=.FALSE.
	N=LEN(FILENAME)
	I=LEN(MESSAGE)
	INQUIRE (FILE=FILENAME(:N),OPENED=ASTAT,NUMBER=IUN)

	IF (.NOT.ASTAT)THEN
C	se non e` gia` aperto
		IUN=IUNI
3		INQUIRE (UNIT=IUN,OPENED=ASTAT)
		IF (ASTAT)THEN
c	se il numero di unita` e` gia usato
			IUN=IUN+1
			FLAG=.TRUE.
			GO TO 3
		ENDIF
		OPEN (UNIT=IUN,FILE=FILENAME(:N),TYPE='UNKNOWN',
	1	ACCESS='APPEND',RECL=130)
	ENDIF
	IF (FLAG)WRITE(IUN,4)IUN
4	FORMAT ('*WRITEMESSAGE* <WARNING>',
	1	' UNITA` ASSEGNATA GIA` OCCUPATA. USO LA N.',I3)
	WRITE (IUN,2)MESSAGE(:I)
2	FORMAT(A<I>)
	IF (FLAG)CLOSE(UNIT=IUN)
	RETURN
	END
