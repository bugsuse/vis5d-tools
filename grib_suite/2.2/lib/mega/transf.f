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





COMSTART TRANSF_X/Y
C	funzioni utili alle routine PLOTSIMBOLI e PLOTSIMBOLIDISSPLA
c	************************************************************
C
C	FUNCTION TRANSFX(X,X0,SCALA,NRIF)
C	per il calcolo della coordinata assoluta x
C
C	FUNCTION TRANSFY(Y,Y0,SCALA,NRIF)
C	per il calcolo della coordinata assoluta y
COMEND

	FUNCTION TRANSFX(X,X0,SCALA,NRIF)
C	per il calcolo della coordinata assoluta x

	IF (NRIF.GT.0)THEN
	  TRANSFX=X0+X*SCALA/1000.
	ELSE IF (NRIF.LT.0)THEN
	     TRANSFX=X0+X*SCALA/1000.-SCALA
	ELSE
	        TRANSFX=X0+X*SCALA/1000.-SCALA/2.
	END IF

	RETURN
	END

	FUNCTION TRANSFY(Y,Y0,SCALA,NRIF)
C	per il calcolo della coordinata assoluta y

	IF (NRIF.GT.0)THEN
	  TRANSFY=Y0+Y*SCALA/1000.
	ELSE IF (NRIF.LT.0)THEN
	     TRANSFY=Y0+Y*SCALA/1000.-SCALA
	ELSE
	        TRANSFY=Y0+Y*SCALA/1000.-SCALA/2.
	END IF

	RETURN
	END

