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





CC**********************************************************************CC
CC**********************************************************************CC
CC									CC
CC		SERVIZIO METEOROLOGICO REGIONE EMILA ROMAGNA		CC
CC				E.R.S.A.				CC
CC									CC
CC		    UFFICIO INFORMAZIONI E PREVISIONI			CC
CC									CC
CC	PAOLO PATRUNO							CC
CC		 				POMI LUCA		CC
CC	SELVINI ANDREA							CC
CC						BATTAGLIA FABRIZIO	CC
CC									CC
CC									CC
CC	BOLOGNA 1990							CC
CC**********************************************************************CC
CC**********************************************************************CC
	integer function ieladata7 (ig,im,ia)
COMSTART IELADATA7
C	INTEGER FUNCTION IELADATA7 (IG,IM,IA)
c	Calcola la data Giuliana corrispondente 
c
C	variabile integer*4
C	IN:
C	ig 	I*4	Giorno del mese 
C	im 	I*4	Mese
C	ia 	I*4	Anno 
c
C	OUT
C	ieladata7  I*4	Data giuliana
comend
	integer*4 ig,im,ia

	n0	= ndays(01,01,ia)
	nx	= ndays(ig,im,ia)
	ieladata7 = (nx-n0) + 1
	return
	end

	SUBROUTINE IELADATA6(IIDAY,IIMONTH,IIYEAR,IIHOUR,IIMIN,
	1			IMINUTI)

COMSTART IELADATA6
C	SUBROUTINE IELADATA6(IIDAY,IIMONTH,IIYEAR,IIHOUR,IIMIN,
C	1			IMINUTI)
c	Calcola la data e l'ora corrispondente a IMINUTI dopo il
c	1/1/1
c
C	variabile integer*4
C	IN:
C	IMINUTI		I*4	MINUTI AD INIZIARE DALLE ORE 00 DEL 1/1/1
c
c	variabili integer*2
C	OUT:
C	IDAY,IMONTH,IYEAR,	I*2
C	IHOUR,IMIN		GIORNO MESE ANNO ORE MINUTI
comend

	integer*2 IIDAY,IIMONTH,IIYEAR,IIHOUR,IIMIN

	CALL JELADATA6(IDAY,IMONTH,IYEAR,IHOUR,IMIN,
	1			IMINUTI)
	IIDAY=IDAY
	IIMONTH=IMONTH
	IIYEAR=IYEAR
	IIHOUR=IHOUR
	IIMIN=IMIN

	RETURN
	END


	SUBROUTINE JELADATA6(IDAY,IMONTH,IYEAR,IHOUR,IMIN,
	1			IMINUTI)

comstart JELADATA6
c	SUBROUTINE JELADATA6(IDAY,IMONTH,IYEAR,IHOUR,IMIN,
c	1			IMINUTI)
c
c	Calcola la data e l'ora corrispondente a IMINUTI dopo il
c	1/1/1
C
c	variabili integer*4
C	IN:
C	IMINUTI		I*4	MINUTI AD INIZIARE DALLE ORE 00 DEL 1/1/1
c
C	OUT:
C	IDAY,IMONTH,IYEAR,  I*4
C	IHOUR,IMIN		GIORNO MESE ANNO ORE MINUTI
COMEND


C	IGIORNO=IMINUTI/(60*24)
C	IHOUR=(IMINUTI-IGIORNO*(60*24))/60
C	IMIN=IMINUTI-IGIORNO*(60*24)-IHOUR*60

	IMIN=MOD(IMINUTI,60)
	IHOUR=MOD(IMINUTI,(60*24))/60
c	IMINU=IMINUTI-998779680
	iminu=iminuti
	IGIORNO=IMINU/(60*24)
	IF (MOD(IMINU,(60*24)).LT.0)IGIORNO=IGIORNO-1

	CALL NDYIN(IGIORNO,IDAY,IMONTH,IYEAR)

	RETURN
	END



	SUBROUTINE IELADATA5(IIDAY,IIMONTH,IIYEAR,IIHOUR,IIMIN,
	1			IMINUTI)

COMSTART IELADATA5
C	SUBROUTINE IELADATA5(IIDAY,IIMONTH,IIYEAR,IIHOUR,IIMIN,
C	1			IMINUTI)
c
c	Calcola i minuti trascorsi tra il 1/1/1 e la data fornita
c
c	variabili integer*2
C	IN:
C	IIDAY,IIMONTH,IIYEAR,	I*2
C	IIHOUR,IIMIN		GIORNO MESE ANNO ORE MINUTI
C
C	variabile integer*4
C	OUT:
C	IMINUTI		I*4	MINUTI AD INIZIARE DALLE ORE 00 DEL 1/1/1
COMEND

	integer*2 IIDAY,IIMONTH,IIYEAR,IIHOUR,IIMIN
	IDAY=IIDAY
	IMONTH=IIMONTH
	IYEAR=IIYEAR
	IHOUR=IIHOUR
	IMIN=IIMIN
	CALL JELADATA5(IDAY,IMONTH,IYEAR,IHOUR,IMIN,
	1			IMINUTI)
	RETURN
	END


	SUBROUTINE JELADATA5(IDAY,IMONTH,IYEAR,IHOUR,IMIN,
	1			IMINUTI)

comstart JELADATA5
c	SUBROUTINE JELADATA5(IDAY,IMONTH,IYEAR,IHOUR,IMIN,
c	1			IMINUTI)
c
c	Calcola i minuti trascorsi tra il 1/1/1 e la data fornita
c
c	variabili integer*4
C	IN:
C	IDAY,IMONTH,IYEAR,  I*4
C	IHOUR,IMIN		GIORNO MESE ANNO ORE MINUTI
c
C	OUT:
C	IMINUTI		I*4	MINUTI AD INIZIARE DALLE ORE 00 DEL 1/1/1
COMEND

c	IMINUTI=NDAYS(IDAY,IMONTH,IYEAR)*24*60+(IHOUR*60)+IMIN+
c	1	998779680
	IMINUTI=NDAYS(IDAY,IMONTH,IYEAR)*24*60+(IHOUR*60)+IMIN

	RETURN
	END


	SUBROUTINE IELADATA3(DATAIN,DATAOUT,INC)

COMSTART IELADATA3
C	SUBROUTINE IELADATA3(DATAIN,DATAOUT,INC)
C
C	Incrementa (o decrementa) una data del numero di giorni contenuto
C	in INC.
C
C	vettori anno mese giorno integer*2
C	INPUT:
C	DATAIN(3)	I*2	anno mese e giorno da incrementare
C	INC		I*4	incremento in numero di giorni
C
C	OUTPUT:
C	DATAOUT(3)	I*2	anno mese e giorno incrementati
COMEND

	INTEGER*2 DATAIN(3),DATAOUT(3)
	CALL IELADATA1(DATAIN(3) ,DATAIN(2) ,DATAIN(1),
	1	       DATAOUT(3),DATAOUT(2),DATAOUT(1),INC)

	RETURN
	END

	SUBROUTINE JELADATA3(DATAIN,DATAOUT,INC)

COMSTART JELADATA3
C	SUBROUTINE JELADATA3(DATAIN,DATAOUT,INC)
C
C	Incrementa (o decrementa) una data del numero di giorni contenuto
C	in INC.
C
C	vettori anno mese giorno integer*4
C	INPUT:
C	DATAIN(3)	I*4	anno mese e giorno da incrementare
C	INC		I*4	incremento in numero di giorni
C
C	OUTPUT:
C	DATAOUT(3)	I*4	anno mese e giorno incrementati
COMEND

	INTEGER*4 DATAIN(3),DATAOUT(3)
	CALL JELADATA1(DATAIN(3) ,DATAIN(2) ,DATAIN(1),
	1	       DATAOUT(3),DATAOUT(2),DATAOUT(1),INC)

	RETURN
	END


	SUBROUTINE IELADATA2(DATAIN,DATAOUT,INC)

COMSTART IELADATA2
C	SUBROUTINE IELADATA2(DATAIN,DATAOUT,INC)
C
C	Incrementa (o decrementa) una data del numero di giorni contenuto
C	in INC.
C
C	vettori giorno mese anno  integer*2
C	INPUT:
C	DATAIN(3)	I*2	giorno mese e anno da incrementare
C	INC		I*4	incremento in numero di giorni
C
C	OUTPUT:
C	DATAOUT(3)	I*2	giorno mese e anno incrementati
COMEND


	INTEGER*2 DATAIN(3),DATAOUT(3)
	CALL IELADATA1(DATAIN(1) ,DATAIN(2) ,DATAIN(3),
	1	       DATAOUT(1),DATAOUT(2),DATAOUT(3),INC)

	RETURN
	END

	SUBROUTINE JELADATA2(DATAIN,DATAOUT,INC)

COMSTART JELADATA2
c	SUBROUTINE JELADATA2(DATAIN,DATAOUT,INC)
C	Incrementa (o decrementa) una data del numero di giorni contenuto
C	in INC.
C
C	vettori giorno mese anno  integer*4
C	INPUT:
C	DATAIN(3)	I*4	giorno mese e anno da incrementare
C	INC		I*4	incremento in numero di giorni
C
C	OUTPUT:
C	DATAOUT(3)	I*4	giorno mese e anno incrementati
COMEND

	INTEGER*4 DATAIN(3),DATAOUT(3)
	CALL JELADATA1(DATAIN(1) ,DATAIN(2) ,DATAIN(3),
	1	       DATAOUT(1),DATAOUT(2),DATAOUT(3),INC)

	RETURN
	END

	SUBROUTINE IELADATA1(IIDAY1,IIMONTH1,IIYEAR1,
	1		    IIDAY2,IIMONTH2,IIYEAR2,INC)

COMSTART IELADATA1
C	SUBROUTINE IELADATA1(IIDAY1,IIMONTH1,IIYEAR1,
C	1		    IIDAY2,IIMONTH2,IIYEAR2,INC)
C
C	Incrementa (o decrementa) una data del numero di giorni contenuto
C	in INC.
C
c	variabili integer*2
C	INPUT:
c	IIDAY1		I*2	giorno da incrementare
c	IIMONTH1	I*2	mese da incrementare
c	IIYEAR1		I*2	anno da incrementare
C	INC		I*4	incremento in numero di giorni
C
C	OUTPUT:
c	IIDAY2		I*2	giorno incrementato
c	IIMONTH2	I*2	mese incrementato
c	IIYEAR2		I*2	anno incrementato
COMEND

	INTEGER*2 IIDAY1,IIMONTH1,IIYEAR1,
	1		    IIDAY2,IIMONTH2,IIYEAR2

	IDAY1=IIDAY1
	IMONTH1=IIMONTH1
	IYEAR1=IIYEAR1

	CALL      JELADATA1(IDAY1,IMONTH1,IYEAR1,
	1		    IDAY2,IMONTH2,IYEAR2,INC)

	IIDAY2=IDAY2
	IIMONTH2=IMONTH2
	IIYEAR2=IYEAR2

	RETURN
	END



	SUBROUTINE JELADATA1(IDAY1,IMONTH1,IYEAR1,
	1		     IDAY2,IMONTH2,IYEAR2,INC)

COMSTART JELADATA1
c	SUBROUTINE JELADATA1(IDAY1,IMONTH1,IYEAR1,
c	1		     IDAY2,IMONTH2,IYEAR2,INC)
c
C	Incrementa (o decrementa) una data del numero di giorni contenuto
C	in INC.
C
c	variabili integer*4
C	INPUT:
c	IIDAY1		I*4	giorno da incrementare
c	IIMONTH1	I*4	mese da incrementare
c	IIYEAR1		I*4	anno da incrementare
C	INC		I*4	incremento in numero di giorni
C
C	OUTPUT:
c	IIDAY2		I*4	giorno incrementato
c	IIMONTH2	I*4	mese incrementato
c	IIYEAR2		I*4	anno incrementato
COMEND

	IGIORNO=NDAYS(IDAY1,IMONTH1,IYEAR1)+INC
	CALL NDYIN(IGIORNO,IDAY2,IMONTH2,IYEAR2)

	RETURN
	END

	SUBROUTINE ICONTROLDATA(IDAY1,IMONTH1,IYEAR1,IER)

COMSTART ICONTROLDATA
C	SUBROUTINE ICONTROLDATA(IDAY1,IMONTH1,IYEAR1,IER)
C
C	controlla che la data sia una data fattibile
C	variabili integer*2
C
C	INPUT:
C	IDAY1		I*2	giorno da controllare
C	IMONTH1		I*2	mese da controllare
C	IYEAR1		I*2	anno da controllare
C
C	OUTPUT:
C	IER		I*4

c	IER=1		errore giorno
c	IER=2		errore mese
c	IER=3		errore anno
c	IER=-1		data del futuro
COMEND

	INTEGER*2 IDAY1,IMONTH1,IYEAR1

	IDAY=IDAY1
	IMONTH=IMONTH1
	IYEAR=IYEAR1

	CALL jCONTROLDATA(IDAY,IMONTH,IYEAR,IER)
	RETURN
	END


	SUBROUTINE JCONTROLDATA(IDAY,IMONTH,IYEAR,IER)

COMSTART JCONTROLDATA
C	SUBROUTINE JCONTROLDATA(IDAY,IMONTH,IYEAR,IER)
C
C	controlla che la data sia una data fattibile
C	variabili integer*4
C
C	INPUT:
C	IDAY1		I*4	giorno da controllare
C	IMONTH1		I*4	mese da controllare
C	IYEAR1		I*4	anno da controllare
C
C	OUTPUT:
C	IER		I*4

c	IER=1		errore giorno
c	IER=2		errore mese
c	IER=3		errore anno
c	IER=-1		data del futuro
COMEND

	DIMENSION IGIORNIMESE(12)
	DATA IGIORNIMESE/31,28,31,30,31,30,31,31,30,31,30,31/

	IGIORNIMESE(2)=28
	IF (MOD(IYEAR,4).EQ.0)IGIORNIMESE(2)=29
	IER=0
	call idate (imonth1,iday1,iyear1)
	iyear2=mod(iyear,100)

	if(iyear2.gt.iyear1)ier=-1
	if(iyear2.eq.iyear1)then
	  if(imonth.gt.imonth1)ier=-1
	  if(imonth.eq.imonth1.and.iday.gt.iday1)ier=-1
	end if

	IF(IYEAR.LT.1.OR.IYEAR.GT.2050)THEN
		IER=3
	ELSE	IF(IMONTH.LT.1.OR.IMONTH.GT.12)THEN
		IER=2
		ELSE	IF(IDAY.LT.1.OR.IDAY.GT.IGIORNIMESE(IMONTH))THEN
		IER=1
	END IF

	RETURN
	END


	SUBROUTINE ICONTROLORA(IORA,IMIN,IER)
COMSTART ICONTROLORA
C	SUBROUTINE ICONTROLORA(IORA,IMIN,IER)
C	CONTROLLA LA FATTIBILITA` DI UNA ORA E MINUTI.	
C
C	INPUT:
C	IORA	I*2	ORA DA CONTROLLARE
C	IMIN	I*2	MINUTI DA CONTROLLARE
C
C	OUTPUT:
C	IER	I*4	CONDIZIONE DI ERRORE
C			IER=0	TUTTO O.K.
C			IER=1	ORA O MINUTI ERRATI
C
COMEND
	INTEGER*2 IORA,IMIN

	JORA=IORA
	JMIN=IMIN
	CALL JCONTROLORA(JORA,JMIN,IER)
	RETURN
	END

	SUBROUTINE JCONTROLORA(JORA,JMIN,IER)
COMSTART JCONTROLORA
C	SUBROUTINE JCONTROLORA(IORA,IMIN,IER)
C	CONTROLLA LA FATTIBILITA` DI UNA ORA E MINUTI.	
C
C	INPUT:
C	IORA	I*4	ORA DA CONTROLLARE
C	IMIN	I*4	MINUTI DA CONTROLLARE
C
C	OUTPUT:
C	IER	I*4	CONDIZIONE DI ERRORE
C			IER=0	TUTTO O.K.
C			IER=1	ORA O MINUTI ERRATI
C
COMEND

	IER=0
	IF(JORA.LT.0.OR.JORA.GT.23)IER=1
	IF(JMIN.LT.0.OR.JMIN.GT.59)IER=1
	RETURN 
	END


	SUBROUTINE NDYIN(NDAYS,IGG,IMM,IAA)

COMSTART NDYIN
c	SUBROUTINE NDYIN(NDAYS,IGG,IMM,IAA)
C	restituisce la data fornendo in input il numero di
c	giorni dal 1/1/1
C				ATTENZIONE
C		non tiene conto del riaggiustamento dell'anno non 
c		bisestile ogni 100 anni
c
c
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c nota bene                   E' SICURO !!!
c	un anno e' bisestile se divisibile per 4
c       un anno rimane bisestile se divisibile per 400
c       un anno NON e' bisestile se divisibile per 100
c
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
c
C	INPUT:
C	NDAYS	I*4	numero giorni dal 1/1/1
c
c	OUTPUT:
C	IGG	I*4	giorno
C	IMM	I*4	mese
C	IAA	I*4	anno
comend

	DIMENSION IANNO(13),IANNO_B(13)
	DATA IANNO/0,31,59,90,120,151,181,212,243,273,304,334,365/
	DATA IANNO_B/0,31,60,91,121,152,182,213,244,274,305,335,366/

	NDAYY=MOD(NDAYS,(366+365*3))	!	resto di gruppi di 4 anni
	IAA=(NDAYS/(366+365*3))*4+(NDAYY/365)+1	
	IF(((NDAYY)/365).EQ.4)IAA=IAA-1
	NDAY=MOD(NDAYY,365)+1
	IF((NDAYY/365).EQ.4)NDAY=NDAY+365

	IRES=MOD(IAA,4)
	IF(IRES.NE.0) GOTO 99

	DO J=1,12
	  IF(NDAY.GT.IANNO_B(J).AND.NDAY.LE.IANNO_B(J+1)) THEN
	    IMM=J
	  ENDIF
	ENDDO
	IGG=NDAY-IANNO_B(IMM)
	GO TO 999

99	DO J=1,12
	  IF(NDAY.GT.IANNO(J).AND.NDAY.LE.IANNO(J+1)) THEN
	    IMM=J
	  ENDIF
	ENDDO
	IGG=NDAY-IANNO(IMM)
999	RETURN
	END

	FUNCTION NDAYS(IGG,IMM,IAA)
COMSTART NDAYS
C	FUNCTION NDAYS(IGG,IMM,IAA)
C	restituisce  il numero di giorni dal 1/1/1
c	fornendo in input la data
C				ATTENZIONE
C		non tiene conto del riaggiustamento dell'anno non 
c		bisestile ogni 100 anni
c
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c nota bene                   E' SICURO !!!
c	un anno e' bisestile se divisibile per 4
c       un anno rimane bisestile se divisibile per 400
c       un anno NON e' bisestile se divisibile per 100
c
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
c
c	INPUT:
C	IGG	I*4	giorno
C	IMM	I*4	mese
C	IAA	I*4	anno
C
C	OUTPUT:
C	NDAYS	I*4	numero giorni dal 1/1/1
comend

	DIMENSION IANNO(12),IANNO_B(12)
	DATA IANNO/0,31,59,90,120,151,181,212,243,273,304,334/
	DATA IANNO_B/0,31,60,91,121,152,182,213,244,274,305,335/

	NDAY=0
	IBIS=MOD(IAA,4)
	IF(IBIS.EQ.0) THEN
		NDAY=IGG+IANNO_B(IMM)
	ELSE
		NDAY=IGG+IANNO(IMM)
	ENDIF

	NDAYS=(NDAY-1)+(365*(IAA-1))+((IAA-1)/4)  ! -((iaa-1)/100) VERO OGNI 100

	RETURN
	END	
