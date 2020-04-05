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





	subroutine COD_VIS(vis,ivis)
COMSTART COD_VIS
C	subroutine COD_VIS(vis,ivis)

c	Per codificare la visibilita` come da specifiche codice synop.
c	accetta in ingresso la visibilita` in ettometri come in archivio
c	meteodata. 

c	IN:
c	VIS	I*2	visibilita` espressa in Hm. (tra 0 e 700 o 9999)
c		(visibilita` inferiore a 100 m. = 0 Hm      codice =0;
c		 visibilita` superiore a 50 Km. = 9999 Hm.  codice =99)

c	OUT:
c	IVIS	I*2	codice synop corrispondente 
c		( -9 valore in ingresso errato)

c	by Mantovani Marina  1989
COMEND

	integer*2 vis,ivis

	iVIS=-9
	if(VIS.gt.9999.or.vis.lt.0)RETURN

	if(vis.eq.0) then
		ivis=0
		return
		endif
	if(vis.gt.0.and.vis.le.50) then
		ivis=vis
		return
		endif
	if(vis.ge.60.and.vis.le.300) then
		ivis=(vis/10)+50	
		return
		endif
	if(vis.ge.350.and.vis.le.700) then
		ivis=((vis-300)/50)+80
		return
		endif
	if(vis.eq.9999) then
		ivis=99
		return
		endif
	end
