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





	SUBROUTINE NCARINT(S,INIZ,LUN,IER)

COMSTART NCARINT
C	SUBROUTINE NCARINT(S,INIZ,LUN,IER)
C
C	VERIFICA CHE IL CONTENUTO DI UNA STRINGA DI CARATTERI
C	SIA SOLO NUMERICO.
C	sono validi solo (1;2;3;4;5;6;7;8;9;0)
C
C	IN:
C	S	CHAR*		STRINGA DA ESAMINARE
C	INIZ	I*4		POSIZIONE INIZIALE DA ESAMINARE
C	LUN	I*4		NUMERO CARATTERI DA ESAMINARE
C
C	OUT:
C	IER	I*4		=0 TUTTO O.K.
C				=1 SONO PRESENTI CARATTERI NON NUMERICI
COMEND
	CHARACTER S*(*)

	IER=0
	DO I=INIZ,INIZ+LUN-1
	  IF(ICHAR(S(I:I)).LT.'30'X.OR.ICHAR(S(I:I)).GT.'39'X)THEN

			IER=1

	  END IF
	END DO
	RETURN
	END


