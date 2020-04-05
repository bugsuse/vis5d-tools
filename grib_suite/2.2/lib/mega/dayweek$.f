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






	subroutine dayweek$(igiorno,imese,ianno,dayw)
COMSTART DAYWEEK
C	subroutine dayweek$(igiorno,imese,ianno,dayw)
c
c	Restituisce data una data il giorno della settimana
c	(in parola e in italiano)
C	
C	INPUT:
c
C	IGIORNO		I*4	giorno
C	IMESE		I*4	mese
C	IANNO		I*4	anno
c
c	OUTPUT:
c
c	DAYW		CHAR*10	giorno della settimana in italiano
COMEND
	character*3 mesi(12)
	character*11  datday
	CHARACTER*10 GIORNI(7),dayw
	integer*4 dd
	integer daynum

	DATA GIORNI/'LUNEDI ','MARTEDI','MERCOLEDI',
	1	    'GIOVEDI','VENERDI','SABATO    ','DOMENICA'/
	data mesi/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
	1	'SEP','OCT','NOV','DEC'/

	IF(IANNO.LT.100)THEN
		IA=IANNO+1900
	ELSE
		IA=IANNO
	END IF

	WRITE(DATday,112)IGIORNO,MESI(IMESE),IA
112	FORMAT(I2.2,'-',A3,'-',I4.4)

	CALL SYS$BINTIM(DATDAY,DD)
	CALL LIB$DAY_OF_WEEK(DD,DAYNUM)

	dayw=giorni(daynum)

	return

	end
