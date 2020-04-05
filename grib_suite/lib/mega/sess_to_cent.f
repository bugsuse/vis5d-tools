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






	FUNCTION SESS_TO_CENT(SESSAG)
C	SESSAG: gg.ppss
	SESS=ABS(SESSAG)
	INTERO=INT(SESS)
	PRIMSEC=SESS-float(INTERO)
	PRIMI=INT((PRIMSEC+1.e-6)*100.)
	SECONDI=float(int(((PRIMSEC+1.e-6)*100.-PRIMI)*1000.))/10.
	SESS_TO_CENT=SIGN((INTERO+PRIMI/60.+SECONDI/3600.),SESSAG)
	RETURN
	END

	FUNCTION CENT_TO_SESS(CENT)
	CEN=ABS(CENT)
	INTERO=INT(CEN)
	DECIMALI=CEN-INTERO
	PRIMSEC=DECIMALI*3600.
	PRIMI=INT(PRIMSEC/60.)
	SECONDI=PRIMSEC-PRIMI*60.
	CENT_TO_SESS=SIGN((INTERO+PRIMI*0.01+SECONDI*0.0001),CENT)
C	CENT_TO_SESS: gg.ppss
	RETURN
	END
