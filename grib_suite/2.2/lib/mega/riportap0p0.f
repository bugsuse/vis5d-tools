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





	FUNCTION RIPORTAP0P0(P,T,HHPOZ)	

COMSTART RIPORTAP0P0
C	FUNCTION RIPORTAP0P0(P,T,HHPOZ)	
C	
C	Riporta la pressione al livello STAZIONE partendo dalla
c	pressione al livello del mare secondo le specifiche
c	dell'areonautica militare date la temperatura l'altezza e
c	la pressione della stazione.
c
c	INPUT:
C
C	P		R*4	pressione in m.b.
C	T		R*4	temperatura in gradi Celsius
C	HHPOZ		R*4	altezza stazione in metri
C
C	OUTPUT:
C	
c	RIPORTAPRES	R*4	pressione riportata s.l.m. in mb.
COMEND

C******************************************************************C
C                          RIDUZIONE S.L.M.			   C
C******************************************************************C

	P0=P/10.**(HHPOZ/(18429.1+67.53*T+0.20559*HHPOZ))

C	stessa cosa scritta in maniera diversa come da ITAV
C	TM=T+0.003*HHPOZ 		!Temperatura media dello strato
C	P0=P*10.**(HHPOZ/(18429.1+67.53*TM+0.003*HHPOZ))

	RIPORTAP0P0=P0			!Pressione s.l.m.

	RETURN
	END
