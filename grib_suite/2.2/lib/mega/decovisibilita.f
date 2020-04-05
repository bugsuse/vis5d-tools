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






	SUBROUTINE DECOVISIBILITA(K,VIS)

COMSTART DECOVISIBILITA
C	SUBROUTINE DECOVISIBILITA(K,VIS)
C
C	decodifica la visibilita` secondo la tabella di decodifica
C	SYNOP 
C	N.B. la visibilita` <100m e` posta uguale a zero.
C
C	INPUT:
C
C	K	I*4	codice della visibilita` da decodificare
C
C	OUTPUT:
C
C	VIS	I*4	visibilita` espressa in metri
COMEND


	INTEGER*4 TVV(11),K,VIS
	DATA TVV/9999,2*0,2,5,10,20,40,100,200,9999/

	VIS=0
	IF (K.GE.1.AND.K.LE.50) THEN
		VIS=K
	ELSE    IF (K.GE.51.AND.K.LE.55) THEN
			VIS=32767
		ELSE    IF (K.GE.56.AND.K.LE.80) THEN
				VIS=10*(K-50)
			ELSE    IF (K.GE.81.AND.K.LE.88) THEN
					VIS=50*(K-80)+300
				ELSE    IF (K.GT.0) THEN
		      			VIS=TVV(K-88)
					ENDIF
	RETURN
	END	


