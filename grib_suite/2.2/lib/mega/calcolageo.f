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





	FUNCTION CALCOLAGEO(PO,T,HSTAZ,PRIF)

COMSTART CALCOLAGEO
C	FUNCTION CALCOLAGEO(PO,T,HSTAZ,PRIF)
C	Calcola il geopotenziale ad un livello barico standard
c	data la pressione, temperatura e altezza di un altro livello.
c
c	INPUT:
C	PO	R*4	pressione in mb.
c	T	R*4	temperatura in gradi centigradi
C	HSTAZ	R*4	altezza in metri
c	PRIF	R*4	livello barico di riferimento in mb.
c
COMEND

	RTM=T+273.16-0.3*((1500.-HSTAZ)/100.)

	RDH=67.422*RTM*LOG10(PO/PRIF)

	CALCOLAGEO=HSTAZ+RDH

	RETURN
	END
