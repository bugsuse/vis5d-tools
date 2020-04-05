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

	SUBROUTINE MAXIMU(IMINUTI,IVET,I1,I2,MAXI,J)

COMSTART MAXIMU
C	SUBROUTINE MAXIMU(IMINUTI,IVET,I1,I2,MAXI,J)
C	calcola il valore massimo del vettore ivet dall'indice I1 
C	all'indice I2 restituendo il numero di valori validi trovati in J
C	solo nel caso che l'intervallo non sia superiore a INTMAX
c	(default=2 ore) altrimenti MAXI=32767 ; J=-1
C
C	INPUT:
C
C	IMINUTI(I2)	I*4	vettore dei tempi dei dati	
C	IVET(I2)	I*4	vettore dei dati (dato mancante =32767)
C	I1		I*4	indice del vettore IVET da cui iniziare la
c				valutazione
C	I2		I*4	indice del vettore IVET in cui finire la
c				valutazione
C
C	OUTPUT:
C
C	MAXI		R*4	valore massimo trovato
c				se :
C				intervallo tra i dati >INTMAX MAXI=32767
C				dati elaborati        =0      MAXI=32767
C	J		I*4	numero di dati effettivi su cui e` stato
c				valutato il valore massimo.
c				se :
C				intervallo tra i dati >INTMAX j=-1
COMEND

	INTEGER*4 IVET(I2),IMINUTI(I2)
	REAL MAXI
	LOGICAL J_C_E
	COMMON /QUAINTERSPL/INTMAX,GRADMAX,MINSTEP
	call mieopzioni
	J=0
	JJ=I1
	MAXI=-32767
	DO I=I1,I2
		IF(.not.j_c_e(IVET(I)))THEN
			IF (IMINUTI(I)-IMINUTI(JJ).GT.INTMAX)THEN
				J=-1
				GOTO 110
			END IF
			GO TO 100
		END IF
		J=J+1
		JJ=I
		MAXI=MAX(FLOAT(IVET(I)),MAXI)
100	END DO
110	IF(J.LE.0)MAXI=32767
	RETURN
	END


	SUBROUTINE MINIMU(IMINUTI,IVET,I1,I2,MINI,J)
COMSTART MINIMU
C	SUBROUTINE MINIMU(IMINUTI,IVET,I1,I2,MINI,J)
C	calcola il valore minimo del vettore ivet dall'indice I1 
C	all'indice I2 restituendo il numero di valori validi trovati in J
C	solo nel caso che l'intervallo non sia superiore a INTMAX
c	(default=2 ore) altrimenti MAXI=32767 ; J=-1
C
C	INPUT:
C
C	IMINUTI(I2)	I*4	vettore dei tempi dei dati	
C	IVET(I2)	I*4	vettore dei dati (dato mancante =32767)
C	I1		I*4	indice del vettore IVET da cui iniziare la
c				valutazione
C	I2		I*4	indice del vettore IVET in cui finire la
c				valutazione
C
C	OUTPUT:
C
C	MINI		R*4	valore minimo trovato
c				se :
C				intervallo tra i dati >INTMAX MAXI=32767
C				dati elaborati        =0      MAXI=32767
C	J		I*4	numero di dati effettivi su cui e` stato
c				valutato il valore minimo.
c				se :
C				intervallo tra i dati >INTMAX j=-1
COMEND

	INTEGER*4 IVET(I2),IMINUTI(I2)
	REAL MINI
	LOGICAL J_C_E
	COMMON /QUAINTERSPL/INTMAX,GRADMAX,MINSTEP
	call mieopzioni
	J=0
	JJ=I1
	MINI=32767
	DO I=I1,I2
		IF(.not.j_c_e(IVET(I)))THEN
			IF (IMINUTI(I)-IMINUTI(JJ).GT.INTMAX)THEN
				J=-1
				GOTO 110
			END IF
			GO TO 100
		END IF
		J=J+1
		JJ=I
		MINI=MIN(FLOAT(IVET(I)),MINI)
100	END DO
110	IF(J.LE.0)MINI=32767
	RETURN
	END

	SUBROUTINE MEDIU(IMINUTI,IVET,I1,I2,MEDI,J)

COMSTART MEDIU
C	SUBROUTINE MEDIU(IMINUTI,IVET,I1,I2,MEDI,J)
C	calcola il valore medio del vettore ivet dall'indice I1 
C	all'indice I2 restituendo il numero di valori validi trovati in J
C	solo nel caso che l'intervallo non sia superiore a INTMAX
c	(default=2 ore) altrimenti MEDI=32767 ; J=-1.
c	L'algoritmo calcola l'area sottesa dalla spezzata
c	ottenuta per interpolazione lineare dei dati dividendola poi
c	per l'intervallo di tempo tra il primo e l'ultimo dato.
C
C	INPUT:
C
C	IMINUTI(I2)	I*4	vettore dei tempi dei dati	
C	IVET(I2)	I*4	vettore dei dati (dato mancante =32767)
C	I1		I*4	indice del vettore IVET da cui iniziare la
c				valutazione
C	I2		I*4	indice del vettore IVET in cui finire la
c				valutazione
C
C	OUTPUT:
C
C	MEDI		R*4	valore medio trovato
c				se :
C				intervallo tra i dati >INTMAX MEDI=32767
C				dati elaborati        =0      MEDI=32767
C	J		I*4	numero di dati effettivi su cui e` stato
c				valutato il valore medio.
c				se :
C				intervallo tra i dati >INTMAX j=-1
COMEND

	INTEGER*4 IVET(I2),IMINUTI(I2)
	REAL MEDI
	LOGICAL J_C_E
	COMMON /QUAINTERSPL/INTMAX,GRADMAX,MINSTEP
	call mieopzioni
	J=0
	JJ=I1
	MEDI=0
	DO I=I1,I2
		IF(.not.j_c_e(IVET(I)))THEN
			IF (IMINUTI(I)-IMINUTI(JJ).GT.INTMAX)THEN
				J=-1
				GOTO 110
			END IF
			GO TO 100
		END IF
		J=J+1
		IF(J.EQ.1)N1=I
		IF(J.GT.1)THEN
		  MEDI=MEDI+
	1	  (IVET(JJ)+IVET(I))*FLOAT(IMINUTI(I)-IMINUTI(JJ))/2.
		  JJ=I
		  N2=I
		END IF
100	END DO
	MEDI=MEDI/FLOAT(IMINUTI(N1)-IMINUTI(N2))
110	IF(J.LE.0)MEDI=32767
	RETURN
	END

