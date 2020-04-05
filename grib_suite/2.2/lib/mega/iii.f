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






	SUBROUTINE CLIMA_INSOLAZIONE(ALAT,GG,MM,AA,TR,TS)

	IDATE=NDAYS(GG,MM,AA)-NDAYS(1,1,AA)	!giorno dell'anno
	DJ=360*IDATE/365.25
	DELTA=ASIND(.3978*SIND(DJ-80.2+1.92*SIND(DJ-2.8)))
	S0=ACOSD(-TAND(DELTA)*TAND(ALAT))/7.5	! DURA
	TR=12.-S0/2.				! SORGE
	TS=12.+S0/2.				! TRAMONTA
	RETURN
	END
