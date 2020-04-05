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

	SUBROUTINE MASSIMO(VET,K,RMAXIM)

COMSTART MASSIMO
C	SUBROUTINE MASSIMO(VET,K,RMAXIM)
c
c	ricava il valore massimo dal vettore vet dimensionato a k
c	rmaxim= valore massimo
c	INPUT:
c
c	VET(K)	R*4	dati da analizzare
c	K	I*4	numero di dati
c	RMAXIM	R*4	valore massimo
COMEND

	DIMENSION VET(K)

	RMAXIM=-1.E37
	DO I=1,K
		RMAXIM=AMAX1(RMAXIM,VET(I))
	END DO
	RETURN
	END

	SUBROUTINE MINIMO(VET,K,RMINIM)

COMSTART MINIMO
C	SUBROUTINE MINIMO(VET,K,RMINIM)
c
c	ricava il valore minimo dal vettore vet dimensionato a k
c	rminim= valore massimo
c
c	INPUT:
c	VET(K)	R*4	dati da analizzare
c	K	I*4	numero di dati
c	RMINIM	R*4	valore minimo
COMEND

	DIMENSION VET(K)

	RMINIM=1.E38
	DO I=1,K
		RMINIM=AMIN1(RMINIM,VET(I))
	END DO
	RETURN
	END


