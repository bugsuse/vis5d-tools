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






	SUBROUTINE CLIMA_TEMP_MAX_MIN
	1	(ALAT,ALON,HHSLM,HHPOZ,IMESE,TMAX,TMIN)
C	IER=4	le coordinate delle stazioni escono dalla finestra ammessa
C		o i dati della stazione non sono stati trovati(manca altezza)


	DIMENSION A2(2,12),B2(2,12),C2(2,12),D2(2,12)

C	REAL TREGA(12,3)
CC	 TABELLA DI 3*SIGMA PER MAX MIN E MED 
C	DATA TREGA/11.5,11.8,11.8,11.8,10.8,11.5,9.9,10.0,11.0,11.0,12.3,11.2,
C	1	  12.4,11.0,11.5,9.8,9.1,9.2,9.0,8.4,9.6,9.6,12.0,11.0,
C	1	   10.7,9.8,11.2,9.4,8.6,9.3,8.4,8.4,9.2,9.2,11.0,9.9/


	DATA ALATMIN,ALATMAX,ALONMIN,ALONMAX/	!limiti dell'area Emilia R.
	1	43.5,45.5,9.,13./


C	FUNZIONE PER RICOSTRUIRE LA CLIMATOLOGIA (SOLO FUNZIONE DELLA
C	LATITUDINE LONGITUDINE E ALTEZZA
	FF(SP,PP,QP,TP,XCX1,XCX2,XCX3)=SP+PP*XCX1+QP*XCX2+TP*XCX3

	IF (ALON.LT.-360.OR.ALAT.LT.-90)GOTO 9999
	ALON=SESS_TO_CENT(ALON)
	ALAT=SESS_TO_CENT(ALAT)
	ITOP=INT(HSLM)
	IF (ITOP.LE.-999)ITOP=INT(HPOZ)
	IF (ITOP.LE.-999)GOTO 9999



C	legge i coefficienti della funzione climatologica
GETLUN
	OPEN(UNIT=IUN,FILE='ANALISITEMPERATURA$DIR:'//
	1	'TABELLAMASSIMA.DAT',STATUS='OLD',IOSTAT=IOS,
	1	ERR=123)

23	DO I=1,12
	  READ(IUN,131)A2(1,I),B2(1,I),C2(1,I),D2(1,I)
	ENDDO
131	FORMAT(33X,4F10.6)
	CLOSE (UNIT=IUN)

	OPEN(UNIT=IUN,FILE='ANALISITEMPERATURA$DIR:'//
	1	'TABELLAMINIMA.DAT',STATUS='OLD',IOSTAT=IOS,
	1	ERR=123)

	DO I=1,12
	  READ(IUN,131)A2(2,I),B2(2,I),C2(2,I),D2(2,I)
	ENDDO
	CLOSE (UNIT=IUN)

C	OPEN(UNIT=IUN,FILE='ANALISITEMPERATURA$DIR:'//
C	1	'TABELLAMEDIA.DAT',STATUS='OLD',IOSTAT=IOS,
C	1	ERR=123)



	TMAX=FF(A2(1,IMESE),B2(1,IMESE),C2(1,IMESE)
	1	,D2(1,IMESE),ALON,float(itop),ALAT)
	TMIN=FF(A2(2,IMESE),B2(2,IMESE),C2(2,IMESE)
	1	,D2(2,IMESE),ALON,float(itop),ALAT)

	RETURN
123	IER=IOS+100
	RETURN
9999	IER=4
	RETURN
	END



