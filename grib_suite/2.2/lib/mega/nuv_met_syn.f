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





	SUBROUTINE NUV_MET_SYN(CC,ICC,IER)
CONSTART NUV_MET_SYN
C	SUBROUTINE NUV_MET_SYN(CC,ICC,IER)
C
C	CONVERTE IL TIPO NUBI METAR 'CC' IN UN NUMERO DA ZEROA NOVE
C	SECONDO LA CODIFICA SYNOP SEZIONE 333
C	N.B.
C	IL DATO MANCANTE VIENE RESTITUITO CON VALORE 99
C	IL DATO BARRATO VIENE RESTITUITO CON VALORE 32767
C
C	IN:
C	CC	C*2	TIPO NUBI METAR
C
C	OUT:
C	ICC	I*2	TIPO NUBI SECONDO CODIFICA SYNOP SEZIONE 333
C	IER	I*4	CONDIZIONE DI ERRORE (NON UTILIZZATA)
COMEND

	CHARACTER*2 CC,VETCC(11)
	INTEGER*2 ICC
	DATA VETCC/'CI','CC','CS','AC','AS','NS',
	1	'SC','ST','CU','CB','//'/

	IER=0
	ICC=99

	DO I=1,11
		IF(CC.EQ.VETCC(I))ICC=I-1
	END DO

	IF(ICC.EQ.10)ICC=32767
	RETURN
	END	
