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





	SUBROUTINE QUAMETARCONS(FLAG,IER)
COMSTART QUAMETARCONS
C	SUBROUTINE QUAMETARCONS(FLAG,IER)
c	controllo di qualita` sulla consistenza internain un report metar.
c	OGNI VARIABILE metar VIENE CONFRONTATA CON le altre variabili presenti
c	nel report; ogni accoppiamento deve veder soddisfatte alcune condizioni
c	fisiche o inerenti la cifratura.
c
c	FLAG(23)	BYTE	incrementato di uno per ogni accoppiamento
c				tra le 23 variabili metar che ha segnalato
c				un errore
c	IER		I*4	condizione di errore
C			IER=1	tipo messaggio errato

c	COMMON /RRW/RRW
c	RRW(29)		I*2	RRW(29) per passare i dati synop come dichiarato
c				in met$inc:metar.inc
c	COMMON /CODER/CODER	
c	CODER(3)	BYTE	BIT MAP per la segnalazione dell'esito
c				di ogni specifico confronto. In questi otto
c				byte ogni bit e` associato a un particolare
c				accoppiamento tra variabili. Il bit settato
c				a 0 significa che non c'e` stata nessuna
c				segnalazione di errore. Il bit settato
c				a 1 significa che quell'accoppiamento di
c				variabili ha segnalato errore. La posizione
c				del bit da testare e` di seguito descritta.
c				Si consiglia di utilizzare la routine
c				BITR appositamente scritta (vedi propria
c				documentazione).

C	Per indirizzare il vettore FLAG alla variabile interessata si puo`
c	utilizzare, includendo nel proprio programma, il file
c	megainc$dir:quametar.inc ed utilizzare come indici del vettore le
c	variabili che hanno lo stesso nome della variabile (meteodata)
c	preceduto da 'IN'. Ad esempio:
c	per la temperatura        FLAG(INTT)
c
c	versione aprile  1997		corretto iww in iiww (per include metar)
c
cI principi generali su cui sono basati questi controlli di qualita`
csono stati ricavati con alcuni adattamenti da:
c
c    1) W.W.W. PLANNING REPORT N.26  'QUALITY CONTROL PROCEDURES 
c       FOR METEOROLOGICAL DATA'
c
c    2) technical note  E.C.M.W.F.- operations department- 
c       'quality control checks applied to observational data'
c
c    3) AERONAUTICA MILITARE ITALIANA REGOLAMENTO DI SERVIZIO N.7
c       'MESSAGGI METEOROLOGICI' MET RS 7  FM 15-V METAR
c
cPOSIZIONE	VARIABILE	VARIABILI
cNELLA BIT MAP	DA CONTROLLARE	DI CONFRONTO
c
c	1	TT		iiww 
c	2	iiww		TT
c	3	TT		TD
c	4	TT		TD iiww 
c	5	TD		TT 
c	6	TD		iiww TT 
c	7	DDD		FFF 
c	8	FFF		DDD 
c	9	FF		iiww 
c	10	iiww		FF 
c	11	FM		FFF 
c	12	FFF		FM 
c	13	VVVV		iiww 
c	14	VVVV		NTOT 
c	15	iiww		VVVV 
c	16	iiww		TT TD
c	17	iiww		NS(1)
c	18	NS(1)		iiww
c	19	CC(4)		CC(1,2,3)
c	20	CC(1,2,3)	CC(4)
c
c
cVARIABILE	CONFRONTO	CONDIZIONE DI ERRORE
c
cTT		WW		TT<-2		WW=20,21,25 			PRECIPITAZIONI LIQUIDE ORA PRECEDENTE
c				TT<-2		WW=50-55,58-65,80-82,91,92 	PRECIPITAZIONI LIQUIDE 
c				TT>6 O TT<-7	WW=23,24 			PIOVIGGINE O PIOGGIA MISTA A NEVE O CONGELENTESI
c											ORA PRECEDENTE
c				TT>6 O TT<-7	WW=56,57,66,67,68,69,83,84	PIOVIGGINE O PIOGGIA CONGELANTESI O
c											PIOGGIA O ROVESCIO MISTO A NEVE
c				TT>6		WW=22,26			NEVE ORA PRECEDENTE
c				TT>6		WW=70-79,85-88			NEVE
c				TT>4		WW=48,49			NEBBIA CHE FORMA UNO STRATO DI GHIACCIO
c				TT>4		WW=36-39			SCACCIANEVE
c				TT<4		WW=30-35			TEMPESTA DI SABBIA
c				TT<-7		WW=17,91-99			TEMPORALE
c				TT<-7		WW=27,89,90			GRANDINE
c
c		TD		TT<TD	
c
c		TD-WW		TT>TD+5	WW=40,41,50-97,99
c				TT>TD+1	WW=42-49
c
cTD		TT		TT<TD
c
c		WW-TT		TT>TD+5	WW=40,41,50-97,99
c				TT>TD+1	WW=42-49
c
cDDD		FFF		C'E` DDD E NON C'E` FFF
c				DDD= 0		FFF<>0
c				DDD<>0		FFF= 0
c				DDD=370		FFF> 6
c
cFFF		DDD		C'E` FFF E NON C'E` DDD
c				FFF<>0		DDD= 0
c				FFF= 0		DDD<>0
c				FFF>6		DDD=999
c
c		WW		FFF<10		WW=09,30-39
c
c		FM		FM<FFF+10
c
cFM		FFF		FM<FFF+10
c
c
cVVVV		WW		WW=05,29,40	VVVV<1000
c				WW=41-49,	VVVV>1000
c				WW=74,75	VVVV>2000
c				WW=10		VVVV>=9999
c
c		NS(1)		VVVV=>1000	NS(1)=9
c
c
cWW		NS(1)		WW=14-17,	NS(1)=MANCANTE
c				50-75,77-99
c				WW=43,45,	NS(1)<>9
c				47,49
c				WW=4-29	NS(1)=9
c				40,42,44,46,48
c
c		VVVV		WW=		VVVV<1000
c				05,29,40
c				WW=41-49,	VVVV>1000
c				WW=74,75	VVVV>2000
c				WW=10		VVVV>=9999
c
c		TT		TT<-2		WW=20,21,25 			PRECIPITAZIONI LIQUIDE ORA PRECEDENTE
c				TT<-2		WW=50-55,58-65,80-82,91,92 	PRECIPITAZIONI LIQUIDE 
c				TT>6 O TT<-7	WW=23,24 			PIOVIGGINE O PIOGGIA MISTA A NEVE O CONGELENTESI
c											ORA PRECEDENTE
c				TT>6 O TT<-7	WW=56,57,66,67,68,69,83,84	PIOVIGGINE O PIOGGIA CONGELANTESI O
c											PIOGGIA O ROVESCIO MISTO A NEVE
c				TT>6		WW=22,26			NEVE ORA PRECEDENTE
c				TT>6		WW=70-79,85-88			NEVE
c				TT>4		WW=48,49			NEBBIA CHE FORMA UNO STRATO DI GHIACCIO
c				TT>4		WW=36-39			SCACCIANEVE
c				TT<4		WW=30-35			TEMPESTA DI SABBIA
c				TT<-7		WW=17,91-99			TEMPORALE
c				TT<-7		WW=27,89,90			GRANDINE
c
c		TT-TD      	WW=40,41,	(TT-TD)>5
c				50,97,99
c				WW=42-49,	(TT-TD)>1
c
c		FFF		WW=09,30-39	FFF<10
c
cNS(1)		WW		NS(1)=MANCANTE	WW=14-17,
c				     		50-75,77-99
c				NS(1)<>9	WW=43,45,47,49
c				NS(1)=9		WW=4-29,40,42,
c						44,46,48
c
c		
c		CC(1)		NS(1)<>9	CC(1)=MANCANTE
c				NS(1)=9		CC(1)=PRESENTE
c
cHHH(1)		VVVV		HHH(1)<100	VVVV>=9999
c
c		CC(1)		HHH(1)<2500	CC(1)=CC,CI,CS
c
c		NS(1)		HHH(1)<2500	NS(1)=MANCANTE
c
cCC(4)		CC(1,2,3)	CC(4)='CB'	CC(1,2,3)='CB'
c
cCC(1,2,3)	CC(4)		CC(1,2,3)='CB'	CC(4)='CB'
COMEND
	COMMON /RRW/RRW
	COMMON /CODER/CODER

	INCLUDE 'MET$INC:METAR.INC'

	INTEGER*4 STAZ,DATA(3),ORA(2),TIPO
	BYTE FLAG(23)
	BYTE RR(58)
	INTEGER*2 RRW(29)
	EQUIVALENCE (RRW,RR)
	LOGICAL C_E,CH_C_E,C_E_N

	CHARACTER CB*2
	DATA CB/'CB'/
	PARAMETER NBYTE=3
	LOGICAL BIT
	BYTE CODER(NBYTE)
	DATA BIT/.TRUE./

	INCLUDE 'MEGAINC$DIR:QUAMETAR.INC/NOLIST'

C	temperatura
	PARAMETER D1A=22,D1B=10,D1C=16,D1D=6,D1E=6,D1F=13,D1G=3,D1H=8
	INTEGER COD1A(D1A),COD1B(D1B),COD1C(D1C),COD1D(D1D),COD1E(D1E),
	1	COD1F(D1F),COD1G(D1G),COD1H(D1H)
	DATA COD1A/20,21,25,50,51,52,53,54,55,58,59,60,61,62,63,64,	!
	1	65,80,81,82,91,92/					!22
	DATA COD1B/23,24,56,57,66,67,68,69,83,84/			!10
	DATA COD1C/22,26,70,71,72,73,74,75,76,77,78,79,85,86,87,88/	!16
	DATA COD1D/48,49,36,37,38,39/					!6
	DATA COD1E/30,31,32,33,34,35/					!6
	DATA COD1F/17,91,92,93,94,95,96,97,98,99,27,89,90/		!13
	DATA COD1G/40,41,99/						!2
	DATA COD1H/42,43,44,45,46,47,48,49/				!8

c	vento
	PARAMETER D2A=11
	INTEGER COD2A(D2A)
	DATA COD2A/09,30,31,32,33,34,35,36,37,38,39/			!11

c	visibilita`
	PARAMETER D3A=26,D3B=9,D3C=2
	INTEGER COD3A(D3A),COD3B(D3B),COD3C(D3C)
	DATA COD3A/05,06,07,08,09,10,11,12,13,14,15,16,17,
	1	   18,19,20,21,22,23,24,25,26,27,28,29,40/		!26
	DATA COD3B/41,42,43,44,45,46,47,48,49/				!9
	DATA COD3C/74,75/						!2


C	tempo presente
	PARAMETER D6A=4,D6B=4,D6C=26,D6D=9,D6E=2,D6F=30,D6G=3,
	1	D6H=8
	INTEGER COD6A(D6A),COD6B(D6B),COD6C(D6C),COD6D(D6D),COD6E(D6E),
	1	COD6F(D6F),COD6G(D6G),COD6H(D6H)

	DATA COD6A/14,15,16,17/					        !4
	DATA COD6B/43,45,47,49/						!4
	DATA COD6C/05,06,07,08,09,10,11,12,13,14,15,16,17,
	1	   18,19,20,21,22,23,24,25,26,27,28,29,40/		!26
	DATA COD6D/41,42,43,44,45,46,47,48,49/				!9
	DATA COD6E/74,75/						!2
	DATA COD6F/05,06,07,08,09,10,11,12,13,14,15,16,17,
	1	   18,19,20,21,22,23,24,25,26,27,28,29,40,		!
	1	   42,44,46,48/						!30
	DATA COD6G/40,41,99/						!3
	DATA COD6H/42,43,44,45,46,47,48,49/				!8
c	DATA COD6I/40,42,44,46,48/					!5

C	temperatura di rugiada

	PARAMETER D8A=3,D8B=8
	INTEGER COD8A(D8A),COD8B(D8B)
	DATA COD8A/40,41,99/						!2
	DATA COD8B/42,43,44,45,46,47,48,49/				!8

	IER=0
	CALL GETHEA (STAZ,DATA,ORA,TIPO,RRW)

	IF (TIPO.NE.10)THEN
		IER=1
		RETURN
	END IF

	CALL NCARINT(WW,1,2,IERRO)
	IF(IERRO.NE.0)THEN
1111		iiww=32767
		GOTO 2222
	END IF

	READ(WW,'(I2)',ERR=1111)iiww

2222	CONTINUE


C	<<<<<<<<<<<<<<<<<<<<<<< CONTROLLO  TEMPERATURA >>>>>>>

C	************ ERRORE TT iiww 

	IF (.NOT.C_E(iiww).OR..NOT.C_E(TT))GOTO 20

	DO I=1,D1A
	IF(TT.LT.-2.AND.COD1A(I).EQ.iiww)GOTO 2
	END DO

	DO I=1,D1B
	IF((TT.GT.6.OR.TT.LT.-7).AND.COD1B(I).EQ.iiww)GOTO 2
	END DO

	DO I=1,D1C
		IF(TT.GT.6.AND.COD1C(I).EQ.iiww)GOTO 2
	END DO

	DO I=1,D1D
		IF(TT.GT.4.AND.COD1D(I).EQ.iiww)GOTO 2
	END DO

	DO I=1,D1E
		IF(TT.LT.4.AND.COD1E(I).EQ.iiww)GOTO 2
	END DO

	DO I=1,D1F
		IF(TT.LT.-7.AND.COD1F(I).EQ.iiww)GOTO 2
	END DO

C	FLAG(INTT)=FLAG(INTT)-1
	GOTO 20

2	FLAG(INTT)=FLAG(INTT)+1
	CALL BITW(CODER,BIT,1)

C	************ ERRORE iiww TT !!!!!!!!!!!!!

	FLAG(INWW)=FLAG(INWW)+1
	CALL BITW(CODER,BIT,2)

20	CONTINUE

C	************ ERRORE TT TD

	IF (.NOT.C_E(TT).OR..NOT.C_E(TD))GOTO 30

	IF (TT.LT.TD)GOTO 3
	

C	FLAG(INTT)=FLAG(INTT)-1
	GOTO 30

3	FLAG(INTT)=FLAG(INTT)+1
	CALL BITW(CODER,BIT,3)

30	CONTINUE

C	************ ERRORE TT TD iiww ***********

	IF (.NOT.C_E(iiww).OR.
	1	.NOT.C_E(TD).OR..NOT.C_E(TT))GOTO 10000

	DO I=1,D1G
		IF (TT.GT.TD+5.AND.iiww.EQ.COD1G(I))GOTO 1000
	END DO

	IF (TT.GT.TD+5.AND.iiww.GE.50.AND.iiww.LE.97)GOTO 1000

	DO I=1,D1H
		IF (TT.GT.TD+1.AND.iiww.EQ.COD1H(I))GOTO 1000
	END DO

C	FLAG(INTT)=FLAG(INTT)-1
	GOTO 10000

1000	FLAG(INTT)=FLAG(INTT)+1
	CALL BITW(CODER,BIT,4)

10000	CONTINUE


C	<<<<<<<<<<<<< ERRORE TEMPERATURA DI RUGIADA >>>>>>>>>>>

C	************ ERRORE TD TT 

	IF (.NOT.C_E(TT).OR..NOT.C_E(TD))GOTO 50

	IF (TT.LT.TD)GOTO 5

C	FLAG(INTD)=FLAG(INTD)-1
	GOTO 50

5	FLAG(INTD)=FLAG(INTD)+1
	CALL BITW(CODER,BIT,5)

50	CONTINUE

C	************* ERRORE TD iiww TT **********

	IF (.NOT.C_E(iiww).OR.
	1	.NOT.C_E(TD).OR..NOT.C_E(TT))GOTO 60

	DO I=1,D8A
		IF (TT.GT.TD+5.AND.iiww.EQ.COD8A(I))GOTO 6
	END DO

	IF (TT.GT.TD+5.AND.iiww.GE.50.AND.iiww.LE.97)GOTO 6

	DO I=1,D8B
		IF (TT.GT.TD+1.AND.iiww.EQ.COD8B(I))GOTO 6
	END DO

C	FLAG(INTD)=FLAG(INTD)-1
	GOTO 60

6	FLAG(INTD)=FLAG(INTD)+1
	CALL BITW(CODER,BIT,6)

60	CONTINUE

C	<<<<<<<<<<<<<<<<<<<< controllo DDD >>>>>>>>>>>>>>>>>

C	*****   DDD  FFF ********

	IF (.NOT.C_E(DDD))GOTO 70

	IF (.NOT.C_E(FFF))GOTO 7
	IF (DDD.EQ.0.AND.FFF.NE.0.OR.DDD.NE.0.AND.FFF.EQ.0)GOTO 7
	IF (DDD.EQ.370.AND.FFF.GT.6)GOTO 7
	GOTO 70

7	FLAG(INDDD)=FLAG(INDDD)+1
	CALL BITW(CODER,BIT,7)

70	CONTINUE

C	<<<<<<<<<<<<  CONTROLLO FFF >>>>>>>>>>>>>>

C	************   FFF DDD ********

	IF (.NOT.C_E(FFF))GOTO 80

	IF (.NOT.C_E(DDD))GOTO 8
	IF (DDD.EQ.0.AND.FFF.NE.0.OR.DDD.NE.0.AND.FFF.EQ.0)GOTO 8
	IF (DDD.EQ.370.AND.FFF.GT.6)GOTO 8

	GOTO 80

8	FLAG(INFFF)=FLAG(INFFF)+1
	CALL BITW(CODER,BIT,8)

80	CONTINUE


C	************ FF iiww **********

	IF (.NOT.C_E(DDD).OR..NOT.C_E(iiww))GOTO 90

	DO I=1,D2A
		IF(COD2A(I).EQ.iiww.AND.FFF.LT.10)GOTO 9
	END DO

	GOTO 90

9	FLAG(INFFF)=FLAG(INFFF)+1
	CALL BITW(CODER,BIT,9)

C	************ iiww FF **********

	FLAG(INWW)=FLAG(INWW)+1
	CALL BITW(CODER,BIT,10)

90	CONTINUE


C	<<<<<<<<<<<<<<<<<<  FORZA VENTO MASSIMO >>>>>>>>>>>>>>>>

C	************ FM FFF **********

	IF (.NOT.C_E(FM).OR..NOT.C_E(FFF)) GOTO 100
	IF (FM.LT.FFF+10)GOTO  10

10	FLAG(INFM)=FLAG(INFM)+1
	CALL BITW(CODER,BIT,11)

C	************ FFF FM **********

	FLAG(INFFF)=FLAG(INFFF)+1
	CALL BITW(CODER,BIT,12)

100	CONTINUE


C	<<<<<<<<<<<<<<<<<<   VISIBILITA`   >>>>>>>>>>>>>>>>>>>>>>

C	*******  VVVV iiww ********

	IF (.NOT.C_E(VVVV).OR..NOT.C_E(iiww))GOTO 130

	DO I=1,D3A
		IF(iiww.EQ.COD3A(I).AND.VVVV.LT.1000)GOTO 13
	END DO

	DO I=1,D3B
		IF(iiww.EQ.COD3B(I).AND.VVVV.GT.1000)GOTO 13
	END DO

	DO I=1,D3C
		IF(iiww.EQ.COD3C(I).AND.VVVV.GT.2000)GOTO 13
	END DO

	IF (iiww.EQ.10.AND.VVVV.GE.9999)GOTO 13

	GOTO 130

13	FLAG(INVVVV)=FLAG(INVVVV)+1
	CALL BITW(CODER,BIT,13)

130	CONTINUE

C	*******  VVVV   NTOT  ********

	IF (.NOT.C_E_n(NS(1)).OR..NOT.C_E(VVVV))GOTO 11000

	IF(NS(1).EQ.9.AND.VVVV.GE.1000)GOTO 1100

	GOTO 11000

1100	FLAG(INVVVV)=FLAG(INVVVV)+1
	CALL BITW(CODER,BIT,14)

11000	CONTINUE


c	<<<<<<<<<<<<<<<<<<<<<<<<<  TEMPO PRESENTE  >>>>>>>>>>>>>>

C	*******  iiww NS(1) ********

	IF (.NOT.C_E(iiww))GOTO 140

	DO I=1,D6A
		IF (iiww.EQ.COD6A(I).AND..NOT.C_E_n(NS(1)))GOTO 1400
	END DO

	IF (((iiww.GE.50.AND.iiww.LE.75).OR.(iiww.GE.77.AND.iiww.LE.99))
	1	.AND..NOT.C_E_n(NS(1)))GOTO 1400

	IF(.NOT.C_E_n(NS(1)))GOTO 140

	DO I=1,D6B
		IF (iiww.EQ.COD6B(I).AND.NS(1).NE.9)GOTO 1400
	END DO
	DO I=1,D6F
		IF (iiww.EQ.COD6F(I).AND.NS(1).EQ.9)GOTO 1400
	END DO

	GOTO 140

1400	FLAG(INWW)=FLAG(INWW)+1
	CALL BITW(CODER,BIT,17)

C	*******  NS(1) iiww ********

	FLAG(INNS(1))=FLAG(INNS(1))+1
	CALL BITW(CODER,BIT,18)

140	CONTINUE
C	*******  iiww VVVV ********

	IF (.NOT.C_E(VVVV).OR..NOT.C_E(iiww))GOTO 180

	DO I=1,D6C
		IF(iiww.EQ.COD6C(I).AND.VVVV.LT.1000)GOTO 18
	END DO

	DO I=1,D6D
		IF(iiww.EQ.COD6D(I).AND.VVVV.GT.1000)GOTO 18
	END DO

	DO I=1,D6E
		IF(iiww.EQ.COD6E(I).AND.VVVV.GT.2000)GOTO 18
	END DO

	IF (iiww.EQ.10.AND.VVVV.GE.9999)GOTO 18

	GOTO 180

18	FLAG(INWW)=FLAG(INWW)+1
	CALL BITW(CODER,BIT,15)


180	CONTINUE

C	*******  iiww TT TD ********

	IF (.NOT.C_E(iiww).OR..NOT.C_E(TT).OR..NOT.C_E(TD))GOTO 210
	DO I=1,D6G
		IF(TT.GT.TD+5.AND.iiww.EQ.COD6G(I))GOTO 21
	END DO
	IF (TT.GT.TD+5.AND.iiww.GE.50.AND.iiww.LE.97)GOTO 21

	DO I=1,D6H
		IF(iiww.EQ.COD6H(I).AND.TT-TD.GT.1)GOTO 21
	END DO

	GOTO 210

21	FLAG(INWW)=FLAG(INWW)+1
	CALL BITW(CODER,BIT,16)


210	CONTINUE

C	<<<<<<<<<<<<<<<<<<<<< CONTROLLO NUBI >>>>>>>>>>>>>>>

C	*******  CC(4)  CC(1,2,3)  ********

340	CONTINUE

	DO I=1,3					
		IF (CC(I).EQ.CB.AND.CH_C_E(CC(4)))THEN
			II=I
			GOTO 35
		END IF
	END DO	

	GOTO 350

35	FLAG(INCC(4))=FLAG(INCC(4))+1
	CALL BITW(CODER,BIT,19)

C	*******  CC(4)  CC(1,2,3)  ********

	FLAG(INCC(II))=FLAG(INCC(II))+1
	CALL BITW(CODER,BIT,20)


350	CONTINUE

	END


	function C_E_n(i2)
	integer*2 i2
	logical c_e_n,c_e

	c_e_n=c_e(i2)
	if(i2.eq.0)c_e_n=.false.

	return
	end
