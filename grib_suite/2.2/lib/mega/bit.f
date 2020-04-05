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





C	PARAMETER NBYTE=1,NBIT=8*NBYTE
C
C	CHARACTER*1 BITMAP(NBIT)
C
C	LOGICAL BITR,BIT
C	BYTE BUF(NBYTE)
C	DATA BIT/.TRUE./
C
C	TYPE*,'INTRODUCI VALORE INIZIALE'
C	READ (5,*)BUF
C
C1	TYPE*,'ICOD='
C	ACCEPT*,ICOD
C	IF (ICOD.GT.NBIT-1)GOTO 2
C
C	CALL BITW(BUF,BIT,ICOD)
C	GOTO 1
C
C2	DO I=0,NBIT-1
C		BITMAP(I+1)='0'
C		IF (BITR(BUF,I))BITMAP(I+1)='1'
C	END DO
C
C	TYPE*,BITMAP
C	STOP
C	END
C
	SUBROUTINE BITW(BUF,BIT,ICOD)

COMSTART BITW
C	SUBROUTINE BITW(BUF,BIT,ICOD)
C	Setta alla condizione della variabile BIT l'ICODicesimo bit
c	dell'area di memoria BUF.
c	INPUT:
C
C	BUF(*)	BYTE	area di memoria da modificare
c	BIT	LOGICAL	se e` true setta il bit a 1
c	ICOD	I*4	posizione del Bit da modificare
c
c	output:
C	BUF(*)	BYTE	area di memoria modificata
COMEND

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

	BYTE BUF(*)
	LOGICAL BIT

	CALL MVBITSB(BIT,0,1,BUF,ICOD)

	RETURN
	END

	FUNCTION BITR(BUF,ICOD)
COMSTART BITR
c	FUNCTION BITR(BUF,ICOD)
C	Restituisce la condizione dell'ICODicesimo bit
c	dell'area di memoria BUF.
c	INPUT:
C
c	ICOD	I*4	posizione del Bit da testare
C	BUF(*)	BYTE	area di memoria da testare
c
c	output:
c	BITR	LOGICAL	se e` true il bit e` pari a 1
COMEND

	BYTE BUF(*)
	LOGICAL BITR

	CALL MVBITSB(BUF,ICOD,1,BITR,0)
	RETURN
	END

	SUBROUTINE MVBITSB(M,I,LEN,N,J)
	dimension m(*),n(*)
	ii=i/32+1
	ib=mod(i,32)
	jj=j/32+1
	jb=mod(j,32)

	CALL MVBITS(M(ii),Ib,LEN,N(jj),Jb)
	RETURN
	END
