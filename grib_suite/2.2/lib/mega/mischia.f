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

	SUBROUTINE MISCHIA 
	INTEGER IWMO,DATA(3),ORA(2),TIPO
	INTEGER*2 LCN(4),LTN(4),LOCIT(3)
	CHARACTER*4 LNCHH(3),ANOME*24
	LOGICAL C_E,CH_C_E
	PARAMETER T0=273.2

	COMMON /RRW/RRW
	COMMON /WWR/WWR

	INCLUDE 'MEGAINC$DIR:METEODATA.INC'
	INCLUDE 'MEGAINC$DIR:LOSYMET.INC'

	EQUIVALENCE (RR(125),LNCHH)	!nubi locali

COMSTART MISCHIA
C	SUBROUTINE MISCHIA 
c	commento provvisorio !!!!
C
C	COMMON /RRW/RRW
C	COMMON /WWR/WWR
C
C	ITMAXP		C/10	I*4
C	IORATMAXP	min	I*4
C	ITMINP		C/10	I*4
C	IORATMINP	min	I*4
C	ITMEDP		C/10	I*4
C	IORATMEDP	min	I*4
C	ITIST		C/10	I*4
C	ITDIST		C/10	I*4
C	IUMAX		0/00	I*4
C	IORAUMAX	min	I*4
C	IUMIN 		0/00	I*4
C	IORAUMIN 	min	I*4
C	IUMED 		0/00	I*4
C	IORAUMED 	min	I*4
C	IPREC 		mm/10	I*4
C	IORAPREC 	imin	I*4
C	IFFIST 		nodi	I*4
C	IDDIST 		gradi	I*4
C	IFFMAX 		nodi	I*4
C	IORAFFMAX 	min	I*4
C	IFFMED 		nodi	I*4
C	IORAFFMED 	min	I*4
C	IVISI 		metri	I*4
C	IPRESSLM 	mb/10	I*4
C	ITPRES 			I*4
C	ITPAS 			I*4
C	IORAITPAS 	min	I*4
C	NTOTA 		1/8	I*4
C	NLIV(4) 	1/8	I*4
C	TLIV(4) 		CHAR*2
C	IRAD		cal/cm2	I*4
C	IORARAD		min	I*4
COMEND

C	inizializza le variabili
	DO J=1,156
		WWR(J)=127
	END DO


	CALL GETHEA(IWMO,DATA,ORA,TIPO,RRW)

	DO J=1,12
		WWR(J)=RR(J)	!ricopio data ora stazione e tipo messaggio
	END DO


	IF (TIPO.NE.01)GOTO 200

C	i dati estratti corrispondono al messaggio synottico

C	cerco TMAX

	IF(.NOT.C_E(TETE))GOTO 10
	IF(ORA(1).EQ.18.AND.ORA(2).EQ.0)THEN
		ITMAXP=TETE
		IORATMAXP=12*60
	END IF

C	cerco TMIN
	IF(ORA(1).EQ.06.AND.ORA(2).EQ.0)THEN
		ITMINP=TETE
		IORATMINP=12*60
	END IF

10	CONTINUE
	IF(.NOT.C_E(TATA))GOTO 20
C	temperatura istantanea
	ITIST=TATA

20	CONTINUE
	IF(.NOT.C_E(TDTD))GOTO 30
C	temperatura di rugiada
	ITDIST=TDTD
	goto 40
30	IF(C_E(UMID).AND.C_E(TATA))THEN
C	se c'e` l'umidita` percentuale e la temperatura viene calcolata la TD
	ITDIST=NINT((TRUG(FLOAT(UMID),FLOAT(TATA)/10.+T0)-T0)*10.)
	END IF

40	CONTINUE
	IF(.NOT.C_E(PRECI))GOTO 50
C	precipitazioni

	IPREC=PRECI

	IF(C_E(TR))THEN
		IORAPREC=TR*60
	ELSE
		IORAPREC=6*60
		IF(MOD(ORA(1)+6,12).EQ.0)IORAPREC=12*60
	END IF

50	CONTINUE

	IF(.NOT.C_E(FF).OR..NOT.C_E(DD))GOTO 60
C	vento istantaneo	!ricordarsi inserire caso vento variabile
	IFFIST=FF
	IDDIST=DD

60	CONTINUE
	IF(.NOT.C_E(VISI))GOTO 70
C	visibilita`
C	vivibilita` superiori a 10000m vengono ridotte a 10000m
	IF (VIS.GT.100)THEN
		IVISI=10000
	ELSE
		IVISI=VIS*100.
	ENDIF

70	CONTINUE
	IF(.NOT.C_E(PRES))GOTO 75
C	pressione slm
	IPRESSLM=PRES

75	CONTINUE
	IF(.NOT.C_E(P0P0).OR..NOT.C_E(TATA))GOTO 80
	CALL GETSTAZ1(IWMO,ANOME,HHSLM,HHPOZ,ALAT,ALON,IER)
	IF (IER.EQ.0.AND.HHPOZ.GE.0)IPRESSLM=NINT(10.*
	1	RIPORTAPRES(FLOAT(P0P0)/10.,FLOAT(TATA)/10.,HHPOZ))

80	CONTINUE
	IF(.NOT.C_E(TEMPRS))GOTO 90
C	tempo presente  
	ITPRES=TEMPRS

90	CONTINUE
	IF(.NOT.C_E(TEMPAS))GOTO 100
C	tempo passato
	ITPAS=TEMPAS
	IORAITPAS=3*60
	IF(MOD(ORA(1),6).EQ.0)IORAITPAS=6*60

100	CONTINUE

	IF(.NOT.C_E(NTOT))GOTO 110
C	copertura totale
	IF(NTOT.LE.8)NTOTA=NTOT

110	CONTINUE
C	cerco la nuvolosita` nei gruppi regionali
		
	NUVTP=0
	DO IN=1,4
		IF (NSS(IN).LT.9.)NUVTP=NUVTP+NSS(IN)
	END DO

C	NUVTP e` la somma delle nuvolosita` parziali
	IF (NUVTP.GE.NTOT)THEN
C	ci sono tutti i gruppi 8 regionali
C	decodifica dei gruppi 8 regionali riportandoli come da metar e ordinati
		CALL DECONUBI(NSS,CSS,NLIV,TLIV)

	ELSE
C	cerco il tipo delle nubi nel gruppo 8 nazionale
C	decodificandola tipo metar e scrivendola nel vettore
		CALL NUVO (CL,CM,CH,TLIV(1),TLIV(3),TLIV(4))
	ENDIF


	IF(.NOT.C_E(RADSOL))GOTO 120
C	radiazione solare
	IRAD=RADSOL
	IORARAD=ORA(1)*60
	IF(IORARAD.EQ.0)IORARAD=24*60

120	CONTINUE


	GO TO 1



200	IF (TIPO.NE.11)GOTO 300
C	i dati estratti sono di tipo locali

	IF(.NOT.C_E(LVMXT1))GOTO 310
C	temperatura
C	valore massimo nell'ora
	ITMAXP=LVMXT1
	IORATMAXP=60

310	CONTINUE
	IF(.NOT.C_E(LVMNT1))GOTO 320
C	valore  minimo nell'ora
	ITMINP=LVMNT1
	IORATMINP=60

320	CONTINUE
	IF(.NOT.C_E(LMEDT1))GOTO 330
c	valore medio nell'ora
	ITMEDP=LMEDT1
	IORATMEDP=60

330	CONTINUE
c	valore istantaneo
	IF(C_E(LISTT1)) ITIST=LISTT1

	IF(.NOT.C_E(LISTUM).OR..NOT.C_E(LISTT1))GOTO 340
C	temperatura di rugiada
	ITDIST=NINT((TRUG(FLOAT(LISTUM),FLOAT(LISTT1)/10.+T0)-T0)*10.)

340	CONTINUE
	IF(.NOT.C_E(LVMXUM))GOTO 350
C	umidita`
	IUMAX=LVMXUM
	IORAUMAX=60

350	CONTINUE
	IF(.NOT.C_E(LVMNUM))GOTO 360
	IUMIN=LVMNUM
	IORAUMIN=60

360	CONTINUE
	IF(.NOT.C_E(LMEDUM))GOTO 370
	IUMED=LMEDUM
	IORAUMED=60

370	CONTINUE

c	IF(L1__IR.EQ.'4')GOTO 380	!OMESSO dati RRR non disponibili S S
c	IF(L1__IR.EQ.'3')THEN		!OMESSO precipitazione =0	 O Y
c		IPREC=0			!				 L N
c		IORAPREC=60		!				 O O
c		GOTO 380		!				   P
c	END IF

	IF(.NOT.C_E(LPRECI))GOTO 380
C	precipitazioni
	IPREC=LPRECI
	IORAPREC=60


380	CONTINUE

C	vento istantaneo
	IF(C_E(LMMV10))	IFFIST=NINT(FLOAT(LMMV10)*0.194) !converte dm/s in nodi
	IF(C_E(LAMV10)) IDDIST=LAMV10
	IF(IFFIST.EQ.0) IDDIST=0

	IF(.NOT.C_E(LVMXVE))GOTO 390
C	vento massimo
	IFFMAX=NINT(FLOAT(LVMXVE)*0.194)
	IORAFFMAX=60

390	CONTINUE
	IF(.NOT.C_E(LMVV60))GOTO 400
c	vento medio
	IFFMED=NINT(FLOAT(LMVV60)*0.194)
	IORAFFMED=60

400	CONTINUE
C	visibilita`
	CALL NCARINT(L2__VV,1,2,IER)
	IF (IER.NE.0)GOTO 410
	READ (L2__VV,'(I2)',ERR=410)IVISIB
	CALL DECOVISIBILITA(IVISIB,IVISIBI)

C	visibilita` superiori a 10000m vengono limitate a 10000m
	IF(IVISIBI.LT.10000)THEN
	  IF(IVISIBI.GT.100)THEN
		IVISI=10000
	  ELSE
		IVISI=IVISIBI*100
	  END IF
	END IF

410	CONTINUE
C	pressione
	IF(.NOT.C_E(LISTPR).OR..NOT.C_E(LISTT1))GOTO 411

	CALL GETSTAZ1(IWMO,ANOME,HHSLM,HHPOZ,ALAT,ALON,IER)
	IF (IER.EQ.0.AND.HHPOZ.GE.0)IPRESSLM=NINT(10.*
	1	RIPORTAPRES(FLOAT(LISTPR)/10.,FLOAT(LISTT1)/10.,HHPOZ))

411	CONTINUE
C	tempo presente e passato
	IF(L1__IX.EQ.'3'.OR .L1__IX.EQ.'6')GOTO 415
	IF(L1__IX.NE.'2'.AND.L1__IX.NE.'5')GOTO 412
C				niente da segnalare
	ITPRES=00
	ITPAS =00
	IORAITPAS=1*60
	IF(MOD(ORA(1),3).EQ.0)IORAITPAS=3*60
	IF(MOD(ORA(1),6).EQ.0)IORAITPAS=6*60
	GOTO 415

412	CONTINUE
C	tempo presente
	CALL NCARINT(L4WWWW,1,2,IER)
	IF (IER.NE.0)GOTO 413
	READ (L4WWWW,'(I2)',ERR=413)ITPRES
413	CONTINUE

C	tempo passato
	CALL NCARINT(L4WWWW,3,2,IER)
	IF (IER.NE.0)GOTO 415
	READ (L4WWWW,'(2X,I2)',ERR=415)ITPAS
	IORAITPAS=1*60
	IF(MOD(ORA(1),3).EQ.0)IORAITPAS=3*60
	IF(MOD(ORA(1),6).EQ.0)IORAITPAS=6*60

415	CONTINUE

C	PARTE RELATIVA ALLA NUVOLOSITA`

C	nuvolosita` totale
	CALL NCARINT(L1___N,1,1,IER)
	IF (IER.NE.0)GOTO 420
	READ (L1___N,'(I1)',ERR=420)NUVTP
	IF(NUVTP.LE.8)NTOTA=NUVTP
420	CONTINUE

C	nuvolosita` gruppi 8 sezione 333
C	inizializzazione variabili

	DO JI=1,4
		LCN(JI)=32767
		LTN(JI)=32767
	END DO

	DO JI=1,3
		LOCIT(JI)=32767
	END DO


c	trasformazione dai character
C	gruppo 8 sezione 333
	DO IN=1,3
		CALL NCARINT(LNCHH(IN),1,2,IER)
		IF (IER.NE.0)GOTO 430
		READ(LNCHH(IN),'(I1,I1,2X)',ERR=430)LCN(IN),LTN(IN)
	END DO

430	CONTINUE

C	gruppo 8 sezione 1
	DO IN=2,4
		CALL NCARINT(L4NCCC(IN:IN),1,1,IER)
		IF (IER.NE.0)GOTO 440
		READ (L4NCCC(IN:IN),'(I1)',ERR=440)LOCIT(IN-1)
440	END DO

c	elaborazione come da synop
C	cerco la nuvolosita` nei gruppi regionali
		
	NUVTP=0
	DO IN=1,3
		IF (LCN(IN).LT.9.)NUVTP=NUVTP+LCN(IN)
	END DO

C	NUVTP e` la somma delle nuvolosita` parziali
	IF (NUVTP.GE.NTOTA)THEN
C	ci sono tutti i gruppi 8 regionali
C	decodifica dei gruppi 8 regionali riportandoli come da metar e ordinati
		CALL DECONUBI(LCN,LTN,NLIV,TLIV)
	ELSE
C	cerco il tipo delle nubi nel gruppo 8 nazionale
C	decodificandola tipo metar e scrivendola nel vettore
		CALL NUVO (LOCIT(1),LOCIT(2),LOCIT(3),
	1	TLIV(1),TLIV(3),TLIV(4))
		
	ENDIF


	IF(.NOT.C_E(LINTRS))GOTO 450
C	RADIAZIONE SOLARE
	IRAD=LINTRS
	IORARAD=60

450	CONTINUE


	GO TO 1


300	IF (TIPO.NE.10)GOTO 1000
C	i dati estratti corrispondono al messaggio metar


c	temperature
	IF(C_E(TT)) ITIST=TT*10
	IF(C_E(TD)) ITDIST=TD*10

c	vento
	IF(C_E(FFF)) IFFIST=FFF
	IF(C_E(DDD)) IDDIST=DDD

	IF(C_E(FM)) IFFMAX=FM

C	visibilita`
	IF(C_E(VVVV)) IVISI=VVVV

C	pressione
	IF(C_E(PH)) IPRESSLM=PH*10	

	IF (.NOT.CH_C_E(WW))GOTO 205
C	tempo presente
	READ(WW(1:2),'(I2)',ERR=210)ITPRESTMP
	ITPRES=ITPRESTMP

205	IF(CC(1).EQ.'CA'.AND.CC(2).EQ.'VO')ITPRES=00

210	CONTINUE
C	cerco la nuvolosita` e la decodifico

	CALL NUBI(NS,CC,NTOTA,NLIV,TLIV)

	GO TO 1


1000	type*,'errore tipo messaggio   *non previsto*'

1	return

	end




	SUBROUTINE DECONUBI (CNI,TNI,CNU,TNU)

COMSTART DECONUBI
C	SUBROUTINE DECONUBI (CNI,TNI,CNU,TNU)
C	subroutine per la conversione dei gruppi 8 del synop regionale
C	in coperture tipo metar ordinate per livello
C	nel caso in cui i campi siano vuoti ritornano coperture =0

C	IN:
C	CNI(4)	INTEGER*2	sono le 4  coperture
C	TNI(4)	INTEGER*2	i 4 tipi di copertura

C	OUT:
C	CNU(4)	INTEGER*4	le cperture in ordine di altezza
C	TNU(4)	CHARACTER*2	i tipi di copertura decodificati tipo metar
COMEND

	CHARACTER*2 TNU(4),L(10),M(10),H(10)
	INTEGER*2 CNI(4),TNI(4)
	integer*4 cnu(4)
	DATA L/'  ','  ','  ','  ','  ','NS','SC','ST','CU','CB'/
	DATA M/'  ','  ','  ','AC','AS','  ','  ','  ','  ','  '/
	DATA H/'CI','CC','CS','  ','  ','  ','  ','  ','  ','  '/

	CNU(1)=0
	CNU(2)=0
	CNU(3)=0
	CNU(4)=0

	TNU(1)='  '
	TNU(2)='  '
	TNU(3)='  '
	TNU(4)='  '

	DO K=1,4
		IF (CNI(K).LT.10.AND.TNI(K).LT.10)THEN
			IF (TNU(1).EQ.'  ')THEN
				TNU(1)=L(TNI(K)+1)
				IF (TNU(1).NE.'  ') CNU(1)=CNI(K)
			ELSE
				IF (TNU(2).EQ.'  ')THEN
					TNU(2)=L(TNI(K)+1)
					IF (TNU(2).NE.'  ') CNU(2)=CNI(K)
				END IF
			END IF
			IF (TNU(3).EQ.'  ')THEN
				TNU(3)=M(TNI(K)+1)
				IF (TNU(3).NE.'  ') CNU(3)=CNI(K)
			ENDIF
			IF (TNU(4).EQ.'  ')THEN
				TNU(4)=H(TNI(K)+1)
				IF (TNU(4).NE.'  ') CNU(4)=CNI(K)
			ENDIF
		END IF
	END DO

	RETURN
	END			


	SUBROUTINE NUVO(CL,CM,CH,CCL,CCM,CCH)
COMSTART NUVO
C	SUBROUTINE NUVO(CL,CM,CH,CCL,CCM,CCH)
C
C	converte il codice delle nubi del synottico gruppo 8 del nazionale
C	con la codifica metar

C	IN:
C	CL	INTEGER*2	codice delle nubi del synoP gruppo 8 nazionale
C	CM	INTEGER*2	codice delle nubi del synoP gruppo 8 nazionale
C	CH	INTEGER*2	codice delle nubi del synoP gruppo 8 nazionale

C	OUT:
C	CCL	CHARACTER*2	codifica metar NUBI BASSE
C	CCM	CHARACTER*2	codifica metar NUBI MEDIE
C	CCH	CHARACTER*2	codifica metar NUBI ALTE
COMEND

	INTEGER*2 CL,CM,CH
	CHARACTER*2 L(5),M(2),H(3),CCL,CCM,CCH
	DATA L/'CU','SC','NS','ST','CB'/
	DATA M/'AS','AC'/
	DATA H/'CI','CS','CC'/

	IF(CL.LT.10)THEN
		IF(CL.EQ.0)CCL='  '
		IF(CL.LE.3.AND.CL.GT.0)CCL=L(1)
		IF(CL.EQ.4.OR.CL.EQ.5.OR.CL.EQ.8)CCL=L(2)
		IF(CL.EQ.6)CCL=L(3)
		IF(CL.EQ.7)CCL=L(4)
		IF(CL.EQ.9)CCL=L(5)
	ELSE
		CCL='  '
	END IF

	IF(CM.LT.10)THEN
		IF(CM.EQ.0)CCM='  '
		IF(CM.LE.2.AND.CM.GT.0)CCM=M(1)
		IF(CM.GT.2)CCM=M(2)
	ELSE
		CCM='  '
	END IF

	IF(CH.LT.10)THEN
		IF(CH.EQ.0)CCH='  '
		IF(CH.LE.4.AND.CH.GT.0)CCH=H(1)
		IF(CH.EQ.5.OR.CH.EQ.6.OR.CH.EQ.7.OR.CH.EQ.8)CCH=H(2)
		IF(CH.EQ.9)CCH=H(3)
	ELSE
		CCH='  '
	END IF
	RETURN
	END

	SUBROUTINE NUBI (NS,CC,NU,NB,TB)
COMSTART NUBI
C	SUBROUTINE NUBI (NS,CC,NU,NB,TB)
C	converte le nubi del METAR ordinandole per livello e calcolando
C	la copertura totale

C	IN:
C	NS(4)	INTEGER*2	nuvolosita` per singolo tipo
c	CC(4)	CHARACTER*2	tipo nube


c	OUT:
C	NU	INTEGER*4	nuvolosita` totale
c	NB(4)	INTEGER*4	nuvolosita` per singolo tipo
C	TB(4)	CHARACTER*2	tipo nube
COMEND
	
	INTEGER*2 NS(4)
	integer*4 NU,NB(4),NA(4),NM(4),NUBM,NUB,NUM,NUA
	DIMENSION ALFA(9)
	CHARACTER*2 GB(10),CC(4),TB(4),TM(4),TA(4)
	DATA ALFA/1.,.875,.75,.625,.5,.375,.25,0.125,0./
	DATA GB/'CI','CC','CS','AC','AS','NS','SC','ST','CU','CB'/

	LB=0
	LM=0
	LA=0

	NU=0
	DO H=1,4
		NB(H)=0
		NM(H)=0
		NA(H)=0
		TB(H)='  '
		TM(H)='  '
		TA(H)='  '
	END DO

C	verifica se il metar riporta CAVOK e nel caso pone le coperture=0
	IF((CC(1).EQ.'CA').AND.(CC(2).EQ.'VO')) GOTO 24
	
	IF(NS(1).GT.9)GOTO24		!SE E` MAGGIORE DI 9 !PONI TUTTO A 0
	IF (NS(1).EQ.9)THEN		!SE E` UGUALE A 9 NU=32767
		NU=32767
		DO H=1,4
			NB(H)=32767
		END DO
		GO TO 24
	ENDIF

	DO J=1,4
		IF (NS(J).LE.8)THEN
			JJ=1
			DO WHILE (CC(J).NE.GB(JJ).AND.JJ.LE.10)
				JJ=JJ+1
			END DO
			GO TO (20,20,20,21,21,22,22,22,22,22,23)JJ
20				LA=LA+1
				NA(LA)=NS(J)
				TA(LA)=CC(J)
			GO TO 23
21				LM=LM+1
				NM(LM)=NS(J)
				TM(LM)=CC(J)
			GO TO 23
22				LB=LB+1
				NB(LB)=NS(J)
				TB(LB)=CC(J)
			GO TO 23
		ENDIF
23	END DO
	NUB=0
	NUM=0
	NUA=0

C	somma le coperture separatamente per ogni livello
	IF (LB.GT.0)NUB=NUVCOM(LB,NB)
	IF (LM.GT.0)NUM=NUVCOM(LM,NM)
	IF (LA.GT.0)NUA=NUVCOM(LA,NA)
C	calcola la copertura per le nubi basse e medie
	NUBM=NUB+NINT(FLOAT(NUM)*(ALFA(NUB+1)))
C	calcola la copertura totale
	NU=NUBM+NINT(FLOAT(NUA)*(ALFA(NUBM+1)))
	NB(3)=NM(1)
	NB(4)=NA(1)
	TB(3)=TM(1)
	TB(4)=TA(1)

24	RETURN
	END

	FUNCTION NUVCOM(LL,NN)
C	somma le coperture allo stesso livello forzandola a 8 se e` maggiore
	INTEGER*4 NN(4),IS

	IS=0
	DO JJ=1,LL
		IS=IS+NN(JJ)
	END DO
	IF(IS.GT.8) IS=8
	NUVCOM=IS
	RETURN 
	END

