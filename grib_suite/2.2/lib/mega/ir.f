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





	SUBROUTINE IR(AAI,MMI,GGI,ORAI,AAE,MME,GGE,ORAE,MINE,INR)
COMSTART IR
C	SUBROUTINE IR(AAI,MMI,GGI,ORAI,AAE,MME,GGE,ORAE,MINE,INR)

C	calcola un indice incrementato di uno ogni ora possibile
C	synottica o metar

C	INPUT:
C	AAI
C	MMI
C	GGI
C	ORAI	I*4	e` la data da cui inizia il computo dell`indice
C	AAE
C	MME
C	GGE
C	ORAE
C	MINE	I*4	e` la data in cui calcolare l'indice

C	OUTPUT:
C	INR e` l'indice in uscita
COMEND
	INTEGER*4 AAI,MMI,GGI,ORAI,AAE,MME,GGE,ORAE,MINE,INR,DATA,INC,
	1	DATA1,DATA2

	DATA1=	NDAYS(GGE,MME,AAE)
	DATA2=	NDAYS(GGI,MMI,AAI)

	DATA=DATA1-DATA2
	IF (MINE-20) 10,20,30
10	INC=0
	GOTO 40
20	INC=1
	GOTO40
30	INC=2

40	INR=(DATA*72+ORAE*3+INC)-(ORAI*3)+1
	
	RETURN
	END

