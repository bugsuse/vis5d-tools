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


      SUBROUTINE JELADATA6(IDAY,IMONTH,IYEAR,IHOUR,IMIN,
     +			IMINUTI)

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


      SUBROUTINE JELADATA5(IDAY,IMONTH,IYEAR,IHOUR,IMIN,
     +			IMINUTI)

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

99    DO J=1,12
        IF(NDAY.GT.IANNO(J).AND.NDAY.LE.IANNO(J+1)) THEN
          IMM=J
        ENDIF
      ENDDO
      IGG=NDAY-IANNO(IMM)
999   RETURN
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
