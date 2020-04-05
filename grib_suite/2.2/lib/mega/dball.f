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





C
c	gestire tempo presente nei metar 1/7/93		!DA FARE
c	fare controllo consistenza nei metar 1/7/93	!DA FARE
c	inserire controllo climatico E.R. synop e metar	!DA FARE
c	inserire controllo climatico E.R. synop e metar	!DA FARE
C
C
C	testare integer overflow		!DA FARE O SUPERFLUO
C	scrivere vf				!FATTO
C	e finire di inserirla nei metar		!FATTO

	function N_GETMULTI(FILAC,CONTRO,PRIMO,ISTAZL,DATAL,ORAL,
	1	ISTAZU,DATAU,ORAU,STAZE,DATAE,ORAE,rep,NREP)

COMSTART N_GETMULTI
C	function N_GETMULTI(FILAC,CONTRO,PRIMO,ISTAZL,DATAL,ORAL,
C	1	ISTAZU,DATAU,ORAU,STAZE,DATAE,ORAE,rep,NREP)
C	
c	Retrieval dal METEODATAbase in sequenza dei records relativi ai reports 
c	compresi nella finestra datal/oral - datau/orau e con sottochiave
c	stazl - stazu. Ogni chiamata rende un report soddisfacente ai limiti
c	impostati.
c	la ricerca viene fatta contemporaneamente su NREP archivi contenenti
c	differenti tipi di report. Essi possono essere synop metar o locali.
c	viene restituito un unico report all'interno di un'ora dando priorita`
c	secondo l'ordine di FILAC. Il tracciato record e` unico e bisogna far
c	riferimento a megainc$dir:dball.inc ; se si effettua l'include di questo
c	file bisogna passare il nome di variabile wwr al posto di rep.
c	La function effettua anche un controllo di qualita` climatico e di
c	consistenza per i report synop e metar.
c
c	filac(nrep) i/p	c*100	Nome del file da usare per il retrieval del
c				record.
c				Usato solo se NUN non ha nessun file 
c				aperto su di se', in tal caso causa apertura
c				automatica, o se primo=.true., in tal caso
c				causa chiusura e riapertura automatica.
c				Se il nome non specifica il device e/o
c				directory il default e' METEO$DIR, se il nome
c				non specifica l'estensione in default e' .IND
c				Ad es. filac='SYNOP' causa l'apertura di
c				METEO$DIR:SYNOP.IND .
c 
c	contro(nrep)	i/p	i*4	Unita' da utilizzare nel retrieval.
c				Se nessun file e' aperto su di essa viene
c				automaticamente aperto filac.
c
c	primo(nrep)	i/p 	l*1	Variabile logica per segnalare primo ingresso
c				(da inizializzare a .TRUE., viene
c				automaticamente	posta a .FALSE. all'interno
c				della routine).
c
c	istazl	i/p	i*4	Limite inferiore codice WMO di ricerca per
c				stazione.
c
c	datal(3)/oral(2) i/p i*4 Limite inferiore di ricerca per cronologia
c				(giorno,mese,anno,ore e minuti).
c
c	istazu	i/p	i*4	Limite superiore codice WMO di ricerca per
c				stazione.
c
c	datau(3)/orau(2) i/p i*4 Limite superiore di ricerca per cronologia
c				(giorno,mese,anno,ore e minuti).
c
c	staze	o/p	i*4	Codice WMO della stazione del report
c				reso in buf se n_getsds=0.
c
c	datae(3)	o/p	i*4	Vettore contenente rispettivamente
c				giorno,mese ed anno a quattro cifre
c				del report reso in buf se n_getsds=0.
c
c	orae(2)	o/p	i*4	Vettore con ore e minuti del report
c				reso in buf se n_getsds=0.
c
c	rep(n)  o/p	i*2	buffer che conterra' il report letto 
c				dal database se n_getsds=0.
c				(Deve essere dimensionato sulla base 
c				del file megainc$dir:dball.inc).
c
c	nrep	i/p	i*4	numero di report su cui si vuole fare la 
c				ricerca contemporanea.
c
c	n_getmulti o/p	i*4	= 0 tutto ok - il buffer contiene
c				il primo report in sequenza ricercato
c				= 1 fine ricerca
c				=-1 errore di i/o su file
c				=-2 limiti di ricerca errati
c				=-9 stazione senza anagrafica
c				=10 tipo report non corretto
c	
c
c		common con limiti geografici di estrazione
c	common /n_getmulti/ALATMIN,ALATMAX,ALONMIN,ALONMAX
c
c				dati sessagesimali
c
c	11/01/91
c	11/04/94	revised
COMEND

	common /n_getmulti/ALATMIN,ALATMAX,ALONMIN,ALONMAX
	DATA ALATMIN,ALATMAX,ALONMIN,ALONMAX/	!limiti dell'area Emilia R.
	1	-90.0,90.0,-360.0,360.0/

	PARAMETER NVAR=46

	include 'MEGAINC$DIR:METEODATA.inc'
	include 'MEGAINC$DIR:DBALL.INC'
	BYTE WWRMUL(LREC*4,5)
	byte rep(lrec*4)

	COMMON /WWR/WWR
	COMMON /RRW/RRW
C	COMMON /CODER/CODER

	BYTE FLAG(NVAR)
c	data flag/NVAR*0/
c	data iatt/2/

C	PARAMETER NBYTE=8,NBIT=8*NBYTE
C	CHARACTER*1 BITMAP(NBIT)
C	LOGICAL BITR
C	BYTE CODER(NBYTE)

	LOGICAL PRIMO(NREP)
	CHARACTER*100 FILAC(NREP),anome*24
	INTEGER*4 CONTRO(NREP),ERRORE,DATAL(3),ORAL(2),
	1	DATAU(3),ORAU(2),DATAE(3),ORAE(2),STAZE,TIPOE

	data iminmetar/48648960/	!	1/7/93

	n_getmulti=0
	do i=1,nrep
		if (.not.primo(i))goto 10
	end do
	mem=0

10	ERRORE=N_GETSSDX(FILAC,CONTRO,PRIMO,ISTAZL,DATAL,ORAL,ISTAZU,
	1		DATAU,ORAU,STAZE,DATAE,ORAE,RRW,NREP)

		IF (ERRORE)1,2,3	!SE <0 ERRORE DI I/O ESCO
					!SE 0  MESSAGGIO TROVATO PROCEDO
					!SE >0 FINE RICERCA
2	CONTINUE

	CALL GETHEA(STAZE,DATAE,ORAE,TIPOE,RRW)

ccccccccc
c			controllo area geografica
	call getstaz1(staze,anome,hhstaz,hhpoz,alat,alon,ier)
	if(ier.ne.0)call getstazagr(staze,anome,hhpoz,
	1	alat,alon,ier)
d	type*,alat,alon,hhpoz

	if(ier.ne.0)then
	  TYPE*,'errore estrazione anagrafica',ier,' staz=',staze
	  n_getmulti=-9
	else
	  if (alon.lt.alonmin.or.alon.gt.alonmax.or.
	1	alat.lt.alatmin.or.alat.gt.alatmax)goto 10
	end if

ccccccccc

	DO JK=1,NVAR
		FLAG(JK)=0
	END DO

C	SALTA ai vari tipi di elaborazione secondo il tipo di messaggio estratto
	IF (TIPOE.EQ.01)GOTO 101
	IF (TIPOE.EQ.10)GOTO 110
	IF (TIPOE.EQ.11)GOTO 111
	IF (TIPOE.GE.12.AND.TIPOE.LE.14)GOTO 112
	n_getmulti=10
	RETURN

101	continue
	CALL QUASYNOPCLIM(FLAG,IER)
	IF(IER.NE.0)TYPE*,'**** ERRORE QUASYNOPLIM',ier
	CALL QUASYNOPCONS(FLAG,IER)
	IF(IER.NE.0)TYPE*,'**** ERRORE QUASYNOPCONS',ier
c	CALL QUASYNOPNULV(FLAG,IATT,IER)
c	IF(IER.NE.0)TYPE*,'**** ERRORE QUASYNOP',ier
	GOTO 5

110	continue

	call jeladata5(datae(1),datae(2),datae(3),orae(1),orae(2),imimet)
	if (imimet.lt.iminmetar)then	!solo metar vecchi
	  CALL QUAMETARCLIM(FLAG,IER)
	  IF(IER.NE.0)TYPE*,'**** ERRORE QUAMETARCLIM',ier
	  CALL QUAMETARCONS(FLAG,IER)
	  IF(IER.NE.0)TYPE*,'**** ERRORE QUAMETARCONS',ier
	else
	  call antimaronemet(ier)			!sistema nubi FEW
	  CALL QUAMETARCLIM_new(FLAG,IER)
	  IF(IER.NE.0)TYPE*,'**** ERRORE QUAMETARCLIM_NEW',ier
	  CALL QUAMETARCONS_new(FLAG,IER)
	  IF(IER.NE.0)TYPE*,'**** ERRORE QUAMETARCONS_NEW',ier
	end if
c	CALL QUAMETARNULV(FLAG,IATT,IER)
c	IF(IER.NE.0)TYPE*,'**** ERRORE QUAMETAR',ier
	GOTO 5

111	continue
	CALL QUALOCALCLIM(FLAG,IER)
	IF(IER.NE.0)TYPE*,'**** ERRORE QUALOCALCLIM',ier
c	CALL QUALOCALNULV(FLAG,IATT,IER)
c	IF(IER.NE.0)TYPE*,'**** ERRORE QUALOCAL',ier
	GOTO 5

112	continue
	CALL QUAAGMETCLIM(FLAG,IER)
	IF(IER.NE.0)TYPE*,'**** ERRORE QUAAGMETCLIM',ier
c	CALL QUAAGMETNULV(FLAG,IATT,IER)
c	IF(IER.NE.0)TYPE*,'**** ERRORE QUAAGMET',ier
	GOTO 5


5	CONTINUE
	CALL MISCHIONE(FLAG)

C	inizializzo prima data
	IF (MEM.EQ.0)THEN
	  CALL JELADATA5(DATAE(1),DATAE(2),DATAE(3),
	1	ORAE(1),ORAE(2),IMIN1)
	  ISTAZOLD=STAZE
	END IF

	CALL JELADATA5(DATAE(1),DATAE(2),DATAE(3),
	1	ORAE(1),ORAE(2),IMIN2)

	DIF=IMIN2-IMIN1+MOD(IMIN1-30,60)
	IF(DIF.LT.60.AND.STAZE.EQ.ISTAZOLD)then
c	  vettorizzo il dato
	  MEM=MEM+1
	  IF(MEM.GT.5)MEM=5
	  DO I=1,LREC*4
		WWRMUL(I,MEM)=WWR(I)
	  END DO

	  GOTO 10
	END IF

3	CONTINUE
	if(errore.ne.0.and.mem.eq.0)then
		n_getmulti=1
		RETURN
	end if

C	vedo se c'e` un sinottico
d	DO I=1,MEM
d	  CALL GETHEA(STAZE,DATAE,ORAE,TIPOE,WWRMUL(1,I))
d	  IF (TIPOE.EQ.1)THEN
d	    MEM=I
d	    GOTO 30
d	  END IF
d	END DO

c	altrimenti stampo il piu` vicino alle 00
	IOR=60
	DO I=1,MEM
	  CALL GETHEA(STAZE,DATAE,ORAE,TIPOE,WWRMUL(1,I))
	  IORX=ORAE(2)
	  IF (IORX.GT.30)IORX=60-ORAE(2)
	  IF (IORX.LE.IOR)THEN
	    MEM=I
	    IOR=IORX
	  END IF
	END DO

30	CONTINUE
C	HO TROVATO QUALE SCRIVERE
	
	CALL GETHEA(STAZE,DATAE,ORAE,TIPOE,WWRMUL(1,MEM))

	IF (ORAE(2).GT.30)ORAE(1)=ORAE(1)+1
	ORAE(2)=0

	IF (ORAE(1).EQ.24)THEN
		CALL JELADATA2(DATAE,DATAE,1)
		ORAE(1)=0	
	END IF

	CALL NEWKEYFOR (TIPOE,DATAE,ORAE,STAZE,WWRMUL(1,MEM))

	DO I=1,LREC*4
		REP(I)=WWRMUL(I,MEM)
	END DO

	if(errore.ne.0)then
		mem=0
		istazold=0
		RETURN
	end if

C	MEMORIZZO COME PRIMO DATO ESTRATTO

	CALL GETHEA(STAZE,DATAE,ORAE,TIPOE,RRW)
	CALL JELADATA5(DATAE(1),DATAE(2),DATAE(3),
	1	ORAE(1),ORAE(2),IMIN1)
	ISTAZOLD=STAZE

	CALL GETHEA(STAZE,DATAE,ORAE,TIPOE,WWRMUL(1,MEM))  !per output

	MEM=1
	DO I=1,LREC*4
		WWRMUL(I,MEM)=WWR(I)
	END DO


	RETURN

1	n_getmulti=errore
	RETURN

	END

	SUBROUTINE NEWKEYFOR (TIPO,DATA,ORA,STAZ,REC)
COMSTART NEWKEYFOR
C	SUBROUTINE NEWKEYFOR (TIPO,DATA,ORA,STAZ,REC)
C
C	FORMATTA UN RECORD TIPO KEY DI PUNTAMENTO PER UN REPORT 
C
C	TIPO	I*4	NUMERO DEL TIPO REPORT
C	DATA(3)	I*4	VETTORE CON GG MM AA UNO PER ELEMENTO
C	ORA(2)	I*4	VETTORE CON HH MM UNO PER ELEMENTO
C	STAZ	I*4	NUMERO DELLA STAZIONE METEOROLOGICA
C	NN1	---	NON USATO
C	NN2	---	NON USATO
C	REC	CHAR	BUFFER DEL RECORD CHE VERRA' FORMATTATO DALLA ROUTINE
C			(MINIMO 10 BYTES)
COMEND
	INTEGER*4 TIPO,DATA(3),ORA(2),STAZ,POINT
	BYTE REC(*)
	CHARACTER CTRANS*18
C
C	CONVERTI IN ASCII TUTTI I VALORI INTERI DI CHIAVE
C
	WRITE (CTRANS,'(6I2.2,I6.6)') TIPO,DATA(3),DATA(2),DATA(1),ORA,STAZ
	READ (CTRANS,'(9Z2)') (REC(I),I=2,10)
C
C	COPIA SU ZONA DI CHIAVE SECONDARIA
C
C	REC(11)=REC(2)
C	REC(12)=REC(8)
C	REC(13)=REC(9)
C	REC(14)=REC(10)
C	REC(15)=REC(3)
C	REC(16)=REC(4)
C	REC(17)=REC(5)
C	REC(18)=REC(6)
C	REC(19)=REC(7)
	RETURN
	END

	SUBROUTINE MISCHIONE(FLAG)

COMSTART MISCHIONE
C	SUBROUTINE MISCHIONE
C
C	Normalizza i messaggi in archivio meteodata di tipo synop metar e
c	locali su un nuovo e unico tracciato record.
c	Questa routine comunica tramite due common, uno in input e uno in
c	output. Il contenuto dei common viene definito includendo nel main
c	program le seguenti istruzioni
C
C	INCLUDE 'MEGAINC$DIR:METEODATA.INC'
C	INCLUDE 'MEGAINC$DIR:DBALL.INC'
C	COMMON /RRW/RRW			! in input
C	COMMON /WWR/WWR			! in output
C
C	I report letti dall'archivio meteodata dovranno essere quindi
c	memorizzati nel buffer RRW definito nell'include.
C
COMEND
	INTEGER IWMO,DATA(3),ORA(2),TIPO
	INTEGER*2 LOCIT(3)
	CHARACTER*4 ANOME*24
	LOGICAL C_E,CH_C_E,VF
	PARAMETER T0=273.2

	PARAMETER NVAR=46
	BYTE FLAG(NVAR)

	COMMON /RRW/RRW
	COMMON /WWR/WWR

	INCLUDE 'MEGAINC$DIR:METEODATA.INC'
	INCLUDE 'MEGAINC$DIR:DBALL.INC'

	INCLUDE 'MEGAINC$DIR:QUASYNOP.INC'
	INCLUDE 'MEGAINC$DIR:QUAMETAR.INC'
	INCLUDE 'MEGAINC$DIR:QUALOCAL.INC'
	INCLUDE 'MEGAINC$DIR:QUAAGMET.INC'
	INCLUDE 'MEGAINC$DIR:QUADBALL.INC'

	data iminmetar/48648960/	!	1/7/93

C	inizializza le variabili
	DO J=13,lrec*4,2
		WWR(J)=255
	END DO
	DO J=14,lrec*4,2
		WWR(J)=127
	END DO
	DO J=1,NV
		FLAG1(J)=127
		FLAG2(J)=127
		FLAG3(J)=127
	END DO

	CALL GETHEA(IWMO,DATA,ORA,TIPO,RRW)

	DO J=1,12
		WWR(J)=RR(J)	!ricopio data ora stazione e tipo messaggio
	END DO

	IF (TIPO.NE.01)GOTO 200

C	i dati estratti corrispondono al messaggio synottico

10	CONTINUE
	IF(.NOT.C_E(TATA))GOTO 20
C	temperatura istantanea
	ITIST=TATA
	FLAG1(INITIST)=FLAG(INTATA)

20	CONTINUE
	IF(.NOT.C_E(TETE))GOTO 21
C	temperatura minima o massima
	ITMINMAX12=TETE
	FLAG1(INITMINMAX12)=FLAG(INTETE)

21	CONTINUE

	IF(.NOT.C_E(TDTD))GOTO 30
C	temperatura di rugiada
	ITDIST=TDTD
	FLAG1(INITDIST)=FLAG(INTDTD)
	goto 220

30	IF(	     C_E(UMID)
	1	.AND.C_E(TATA)
	1	.and.vf(FLAG(INTATA))
	1	.and.vf(FLAG(INUMID)))THEN
C	se c'e` l'umidita` percentuale e la temperatura viene calcolata la TD
	ITDIST=NINT((TRUG(FLOAT(UMID),FLOAT(TATA)/10.+T0)-T0)*10.)
	FLAG1(INITDIST)=FLAG(INTATA)+FLAG(INUMID)
	END IF

220	CONTINUE
	IF(.NOT.C_E(UMID))GOTO 330
C	umidita` relativa
	IUIST=UMID
	FLAG1(INIUIST)=FLAG(INUMID)
	goto 40

330	IF(	     C_E(TDTD)
	1	.AND.C_E(TATA)
	1	.and.vf(FLAG(INTATA))
	1	.and.vf(FLAG(INTDTD)))THEN
C	se c'e` la td e la temperatura viene calcolata la U
	IUIST=FR(FLOAT(TATA)/10.+T0,FLOAT(TDTD)/10.+T0)
	FLAG1(INIUIST)=FLAG(INTATA)+FLAG(INTDTD)
	END IF

40	CONTINUE
	IF(.NOT.C_E(PRECI))GOTO 50
C	precipitazioni

	IF(	     C_E(TR)
	1	.and.vf(flag(intr)))THEN
		IORAPREC=TR*60
	ELSE
		ioraprec=0
		IF(MOD(ORA(1),12).EQ.0)IORAPREC=6*60
		IF(MOD(ORA(1)+6,12).EQ.0)IORAPREC=12*60
	END IF

	ik=0
	IF(IORAPREC.EQ.   60)IK=5
	IF(IORAPREC.EQ. 2*60)IK=4
	IF(IORAPREC.EQ. 6*60)IK=3
	IF(IORAPREC.EQ.12*60)IK=2
	IF(IORAPREC.EQ.24*60)IK=1

	if(ik.ne.0)then
	  IPREC(IK)=PRECI
	  FLAG1(INIPREC(IK))=FLAG(INPRECI)
	end if

50	CONTINUE

	IF(.NOT.C_E(FF).OR..NOT.C_E(DD))GOTO 60
C	vento istantaneo	! ricordarsi caso vento variabile(999)
	IFFIST=FF
	IDDIST=DD
	FLAG1(INIFFIST)=FLAG(INFF)
	FLAG1(INiDDIST)=FLAG(INDD)

60	CONTINUE
	IF(	    .NOT.C_E(VIS)
	1	.or..not.vf(flag(invis)))GOTO 70
C	visibilita`
C	vivibilita` superiori a 10000m vengono ridotte a 10000m
	IF (VIS.GT.100)THEN
		IVISI=10000
	ELSE
		IVISI=VIS*100.
	ENDIF
	FLAG1(INIVISI)=FLAG(INVIS)

70	CONTINUE
	IF(.NOT.C_E(PRES))GOTO 75
C	pressione slm
	IPRESSLM=PRES
	FLAG1(INIPRESSLM)=FLAG(INPRES)
	GOTO 770

75	CONTINUE
	IF(	    .NOT.C_E(P0P0)
	1	.OR..NOT.C_E(TATA)
	1	.or..not.vf(FLAG(INP0P0))
	1	.or..not.vf(FLAG(INTATA)))GOTO 770
	CALL GETSTAZ1(IWMO,ANOME,HHSLM,HHPOZ,ALAT,ALON,IER)
	IF (IER.EQ.0.AND.HHPOZ.GE.0.AND.HHPOZ.LE.500)then
		IPRESSLM=NINT(10.*
	1	RIPORTAPRES(FLOAT(P0P0)/10.,FLOAT(TATA)/10.,HHPOZ))
		FLAG1(INIPRESSLM)=FLAG(INP0P0)+FLAG(INTATA)
	end if

770	CONTINUE
	IF(.NOT.C_E(P0P0))GOTO 775
C	pressione slm
	IPRESHSTAZ=P0P0
	FLAG1(INIPRESHSTAZ)=FLAG(INP0P0)
	GOTO 80

775	CONTINUE
	IF(         .NOT.C_E(PRES)
	1	.OR..NOT.C_E(TATA)
	1	.or..not.vf(FLAG(INPRES))
	1	.or..not.vf(FLAG(INTATA)))GOTO 80
	CALL GETSTAZ1(IWMO,ANOME,HHSLM,HHPOZ,ALAT,ALON,IER)
	IF (IER.EQ.0.AND.HHPOZ.GE.0.AND.HHPOZ.LE.500)IPRESHSTAZ=
	1	NINT(10.*
	1	RIPORTAP0P0(FLOAT(PRES)/10.,FLOAT(TATA)/10.,HHPOZ))
	FLAG1(INIPRESHSTAZ)=FLAG(INPRES)+FLAG(INTATA)

80	CONTINUE
	IF(.NOT.C_E(TEMPRS))GOTO 90
C	tempo presente  
	ITPRES=TEMPRS
	FLAG1(INITPRES)=FLAG(INTEMPRS)

90	CONTINUE
	IF(.NOT.C_E(TEMPAS))GOTO 100
C	tempo passato
	ITPAS=TEMPAS
	FLAG1(INITPAS)=FLAG(INTEMPAS)

100	CONTINUE
	IF(.NOT.C_E(CART))GOTO 102
C	caratteristica
	ICART=CART
	FLAG1(INICART)=FLAG(INCART)

102	CONTINUE
	IF(.NOT.C_E(TEND))GOTO 105
C	tendenza
	ITEND=TEND
	FLAG1(INITEND)=FLAG(INTEND)

105	CONTINUE
	IF(.NOT.C_E(NTOT))GOTO 110
C	copertura totale
	IF(NTOT.LE.9)NTOTA=NTOT
	FLAG1(INNTOTA)=FLAG(INNTOT)

110	CONTINUE
C	cerco il tipo delle nubi nel gruppo 8 nazionale
C	decodificandola tipo metar e scrivendola nel vettore

	CALL NUVOMA(CL,CM,CH,ITLIV(1),ITLIV(2),ITLIV(3))

	NLIVB=NH			!!
	IF (NTOT.EQ.0)THEN
		NLIVB=0
		ITLIV(1)=99
		ITLIV(2)=99
		ITLIV(3)=99
	END IF

C	IF(NH.EQ.0)NLIVB=NH
C	IF (C_E(CL).AND.CL.NE.0)NLIVB=NH

	FLAG1(INITLIV(1))=FLAG(INCL)
	FLAG1(INITLIV(2))=FLAG(INCM)
	FLAG1(INITLIV(3))=FLAG(INCH)
	FLAG1(INNLIVB)=FLAG(INNH)

120	CONTINUE

	GO TO 1

200	IF (TIPO.NE.11)GOTO 300
CC	i dati estratti sono di tipo locali

c	valore istantaneo temperatura
	IF(C_E(LISTT1))THEN
	  ITIST=LISTT1
	  FLAG1(INITIST)=FLAG(INLISTT1)
	END IF

c	valore medio temperatura
	IF(C_E(LMEDT1))THEN
	  ITMED=LMEDT1
	  FLAG1(INITMED)=FLAG(INLMEDT1)
	END IF

c	valore minimo temperatura
	IF(C_E(LVMNT1))THEN
	  ITMIN=LVMNT1
	  FLAG1(INITMIN)=FLAG(INLVMNT1)
	END IF

c	valore massimo temperatura
	IF(C_E(LVMXT1))THEN
	  ITMAX=LVMXT1
	  FLAG1(INITMAX)=FLAG(INLVMXT1)
	END IF

C	valore istantaneo umidita
	IF(C_E(LISTUM))THEN
	  IUIST = LISTUM
	  FLAG1(INIUIST)=FLAG(INLISTUM)
	END IF

C	valore medio umidita
	IF(C_E(LMEDUM))THEN
	  IUMED = LMEDUM
	  FLAG1(INIUMED)=FLAG(INLMEDUM)
	END IF

C	valore minimo umidita
	IF(C_E(LVMNUM))THEN
	  IUMIN = LVMNUM
	  FLAG1(INIUMIN)=FLAG(INLVMNUM)
	END IF

C	valore massimo umidita
	IF(C_E(LVMXUM))THEN
	  IUMAX = LVMXUM
	  FLAG1(INIUMAX)=FLAG(INLVMXUM)
	END IF

	IF(         .NOT.C_E(LISTUM)
	1	.OR..NOT.C_E(LISTT1)
	1	.or..not.vf(FLAG(INLISTUM))
	1	.or..not.vf(FLAG(INLISTT1)))GOTO 340
C	temperatura di rugiada
	ITDIST=NINT((TRUG(FLOAT(LISTUM),FLOAT(LISTT1)/10.+T0)-T0)*10.)
	FLAG1(INITDIST)=FLAG(INLISTUM)+FLAG(INLISTT1)

340	CONTINUE

c	IF(L1__IR.EQ.'4')GOTO 380	!OMESSO dati RRR non disponibili S S
c	IF(L1__IR.EQ.'3')THEN		!OMESSO precipitazione =0	 O Y
c		IPREC=0			!				 L N
c		IORAPREC=60		!				 O O
c		GOTO 380		!				   P
c	END IF

	IF(.NOT.C_E(LPRECI))GOTO 380
C	precipitazioni
	IPREC(5)=LPRECI
	FLAG1(INIPREC(5))=FLAG(INLPRECI)

380	CONTINUE

C	vento istantaneo
	IF(C_E(LMMV10))THEN
	  IFFIST=NINT(FLOAT(LMMV10)*0.194) !converte dm/s in nodi
	  FLAG1(INIFFIST)=FLAG(INLMMV10)
	END IF
	IF(C_E(LAMV10))THEN
	  IDDIST=LAMV10
	  IF(IFFIST.EQ.0) IDDIST=0
	  FLAG1(INIDDIST)=FLAG(INLAMV10)
	END IF

C	vento medio 
	IF(C_E(LMVV60))THEN
	  IFFMED=NINT(FLOAT(LMVV60)*0.194) !converte dm/s in nodi
	  FLAG1(INIFFMED)=FLAG(INLMVV60)
	END IF

C	vento massimo
	IF(C_E(LVMXVE))THEN
	  IFFMAX=NINT(FLOAT(LVMXVE)*0.194) !converte dm/s in nodi
	  FLAG1(INIFFMAX)=FLAG(INLVMXVE)
	END IF


401	CONTINUE
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
	FLAG1(INIVISI)=0		!non testato

410	CONTINUE
C	pressione
	IF(.NOT.C_E(LISTPR))goto 411

	IPRESHSTAZ=LISTPR
	FLAG1(INIPRESHSTAZ)=FLAG(INLISTPR)


	IF(.NOT.C_E(LISTT1)
	1	.or..not.vf(FLAG(INLISTPR))
	1	.or..not.vf(FLAG(INLISTT1)))goto 411

	CALL GETSTAZ1(IWMO,ANOME,HHSLM,HHPOZ,ALAT,ALON,IER)
	IF (IER.EQ.0.AND.HHPOZ.GE.0.AND.HHPOZ.LE.500)then
		IPRESSLM=NINT(10.*
	1	RIPORTAPRES(FLOAT(LISTPR)/10.,FLOAT(LISTT1)/10.,HHPOZ))
		FLAG1(INIPRESSLM)=FLAG(INLISTPR)+FLAG(INLISTT1)
	end if

411	CONTINUE
C	tempo presente e passato

	IF(L1__IX.EQ.'3'.OR .L1__IX.EQ.'6')GOTO 415
	IF(L1__IX.NE.'2'.AND.L1__IX.NE.'5')GOTO 412
C				niente da segnalare
	ITPRES=00
	ITPAS =00
	FLAG1(INITPRES)=0
	FLAG1(INITPAS)=0
	GOTO 415

412	CONTINUE
C	tempo presente
	CALL NCARINT(L4WWWW,1,2,IER)
	IF (IER.NE.0)GOTO 413
	READ (L4WWWW,'(I2)',ERR=413)ITPRES
	FLAG1(INITPRES)=0
413	CONTINUE

C	tempo passato
	CALL NCARINT(L4WWWW,3,2,IER)
	IF (IER.NE.0)GOTO 415
	READ (L4WWWW,'(2X,I2)',ERR=415)ITPAS
	FLAG1(INITPAS)=0

415	CONTINUE

C	PARTE RELATIVA ALLA NUVOLOSITA`
C	NTOTA,NLIVB,ITLIV(3)

C	nuvolosita` totale
	CALL NCARINT(L1___N,1,1,IER)
	IF (IER.NE.0)GOTO 420
	READ (L1___N,'(I1)',ERR=420)NTOTA
	FLAG1(INNTOTA)=0

420	CONTINUE

C	nuvolosita` gruppi 8 sezione 333
C	inizializzazione variabili

	DO JI=1,3
		LOCIT(JI)=32767
	END DO

c	trasformazione dai character
C	gruppo 8 sezione 1
	DO IN=2,4
		CALL NCARINT(L4NCCC(IN:IN),1,1,IER)
		IF (IER.NE.0)GOTO 440
		READ (L4NCCC(IN:IN),'(I1)',ERR=440)LOCIT(IN-1)
	FLAG1(INITLIV(IN-1))=0
440	END DO

C	cerco il tipo delle nubi nel gruppo 8 nazionale
C	decodificandola tipo metar e scrivendola nel vettore
		CALL NUVOMA (LOCIT(1),LOCIT(2),LOCIT(3),
	1	ITLIV(1),ITLIV(2),ITLIV(3))

C	nuvolosita` basse e medie
	CALL NCARINT(L4NCCC,1,1,IER)
	IF (IER.NE.0)GOTO 450
	READ (L4NCCC,'(I1)',ERR=450)NLIVB
	FLAG1(INNLIVB)=0
450	CONTINUE

	IF (NTOTA.EQ.0)THEN
		NLIVB=0
		ITLIV(1)=99
		ITLIV(2)=99
		ITLIV(3)=99
	END IF

	GO TO 1


300	IF (TIPO.LT.12.OR.TIPO.GT.14)GOTO 400
CC	i dati estratti sono di tipo AGRO

c	valore istantaneo temperatura
	IF(C_E(T180))THEN
	  ITIST=T180
	  FLAG1(INITIST)=FLAG(INT180)
	END IF

c	valore medio temperatura
	IF(C_E(T180ME))THEN
	  ITMED=T180ME
	  FLAG1(INITMED)=FLAG(INT180ME)
	END IF

c	valore minimo temperatura
	IF(C_E(T180MN))THEN
	  ITMIN=T180MN
	  FLAG1(INITMIN)=FLAG(INT180MN)
	END IF

c	valore massimo temperatura
	IF(C_E(T180MX))THEN
	  ITMAX=T180MX
	  FLAG1(INITMAX)=FLAG(INT180MX)
	END IF

C	VALORE UMIDITA ISTANTANEA
	IF(C_E(UM180))THEN
	  IUIST = UM180
	  FLAG1(INIUIST)=FLAG(INUM180)
	END IF

	IF(	    .NOT.C_E(UM180)
	1	.OR..NOT.C_E(T180)
	1	.or..not.vf(FLAG(INUM180))
	1	.or..not.vf(FLAG(INT180)))GOTO 540
C	temperatura di rugiada
	ITDIST=NINT((TRUG(FLOAT(UM180),FLOAT(T180)/10.+T0)-T0)*10.)
	FLAG1(INITDIST)=FLAG(INUM180)+FLAG(INT180)

540	CONTINUE


C	VALORE UMIDITA MEDIA
	IF(C_E(UM180ME))THEN
	  IUMED = UM180ME
	  FLAG1(INIUMED)=FLAG(INUM180ME)
	END IF

C	VALORE UMIDITA MINIMA
	IF(C_E(UM180MN))THEN
	  IUMIN = UM180MN
	  FLAG1(INIUMIN)=FLAG(INUM180MN)
	END IF

C	VALORE UMIDITA MASSIMA
	IF(C_E(UM180MX))THEN
	  IUMAX = UM180MX
	  FLAG1(INIUMAX)=FLAG(INUM180MX)
	END IF

	IF(.NOT.C_E(PREC))GOTO 580
C	precipitazioni

	IF(TIPO.EQ.13)THEN
		IK=5
	ELSE
		IK=4
	END IF

	IPREC(IK)=PREC
	FLAG1(INIPREC(IK))=FLAG(INPREC)

580	CONTINUE

C	vento istantaneo
	IF(C_E(V10))THEN
	  IFFIST=NINT(FLOAT(V10)*0.194) !converte dm/s in nodi
	  FLAG1(INIFFIST)=FLAG(INV10)
	END IF

C	vento medio
	IF(C_E(V60))THEN
	  IFFMED=NINT(FLOAT(V60)*0.194) !converte dm/s in nodi
	  FLAG1(INIFFMED)=FLAG(INV60)
	END IF

	GO TO 1


400	IF (TIPO.NE.10)GOTO 1000
C	i dati estratti corrispondono al messaggio metar

c	temperature
	IF(C_E(TT))THEN
	  ITIST=min(TT*10,32767)
	  FLAG1(INITIST)=FLAG(INTT)
	END IF
	IF(C_E(TD))THEN
	  ITDIST=min(TD*10,32767)
	  FLAG1(INITDIST)=FLAG(INTD)
	END IF
	IF(C_E(TT).AND.C_E(TD)
	1	.and.vf(FLAG(INTT))
	1	.and.vf(FLAG(INTD)))THEN
	  IUIST=FR(FLOAT(TT)+T0,FLOAT(TD)+T0)
	  FLAG1(INIUIST)=FLAG(INTT)+FLAG(INTD)
	END IF
c	vento
	IF(C_E(FFF))THEN
	  IFFIST=FFF
	  FLAG1(INIFFIST)=FLAG(INFFF)
	END IF
	IF(C_E(DDD))THEN
	  IDDIST=DDD
	  IF(IDDIST.EQ.370)IDDIST=999		!VARIABILE
	  IF(IDDIST.EQ.380)IDDIST=0		!calma
	  FLAG1(INIDDIST)=FLAG(INDDD)
	END IF

C	vento massimo
	IF(C_E(FM))THEN
	  IFFMAX=FM
	  FLAG1(INIFFMAX)=FLAG(INFM)
	END IF

C	visibilita`
	IF(C_E(VVVV))THEN
	  IVISI=VVVV
	  FLAG1(INIVISI)=FLAG(INVVVV)
	END IF

C	pressione
	IF(.NOT.C_E(PH).OR..NOT.C_E(TT)
	1	.OR..NOT.vf(FLAG(INPH))
	1	.OR..NOT.vf(FLAG(INTT)))GOTO 800
	CALL GETSTAZ1(IWMO,ANOME,HHSLM,HHPOZ,ALAT,ALON,IER)
	IF (IER.EQ.0.AND.HHPOZ.GE.0.AND.HHPOZ.LE.500)IPRESHSTAZ=
	1	NINT(10.*
	1	RIPORTAP0P0(FLOAT(PH),FLOAT(TT),HHPOZ))
	FLAG1(INIPRESHSTAZ)=FLAG(INPH)+FLAG(INTT)

800	CONTINUE

	IF(C_E(PH))THEN
	  IPRESSLM=min(PH,3276)*10	
	  FLAG1(INIPRESSLM)=FLAG(INPH)
	END IF

	call jeladata5(data(1),data(2),data(3),ora(1),ora(2),imin)
	if (imin.gt.iminmetar)goto 230

c		versione metar fino al 1/7/93

	IF (.NOT.CH_C_E(WW))GOTO 205
C	tempo presente
	READ(WW(1:2),'(I2)',ERR=210)ITPRESTMP
	ITPRES=ITPRESTMP
	FLAG1(INITPRES)=FLAG(INWW)

205	IF(CC(1).EQ.'CA'.AND.CC(2).EQ.'VO')THEN
	  ITPRES=00
	  FLAG1(INITPRES)=FLAG(INWW)
	END IF

210	CONTINUE
C	cerco la nuvolosita` e la decodifico

	CALL NUBIMA(NS,CC,NTOTA,NLIVB,ITLIV)

	FLAG1(INNTOTA)=FLAG(INNS(1))
	FLAG1(INNLIVB)=FLAG(INNS(1))
	IFT=FLAG(INCC(1))+FLAG(INCC(2))+FLAG(INCC(3))+FLAG(INCC(4))
	FLAG1(INITLIV(1))=IFT
	FLAG1(INITLIV(2))=IFT
	FLAG1(INITLIV(3))=IFT
	goto 250

230	continue

c		versione metar dal 1/7/93

C	tempo presente

C	se c'e` cavok non deve esserci ww
c	e gli do priorita`

	IF(CC(1).EQ.'CA'.AND.CC(2).EQ.'VO')THEN
	  ITPRES=00
	  FLAG1(INITPRES)=FLAG(INWW)
	ELSE

	  ITPRES=NTPRmet_to_syn(ww,ns(1))	!conversione tempo presente
	  FLAG1(INITPRES)=FLAG(INWW)

	END IF


C	cerco la nuvolosita` e la decodifico

	CALL NUBIMA_new(NS,CC,hhh,NTOTA,NLIVB,ITLIV)

	FLAG1(INNTOTA)=FLAG(INNS(1))
	FLAG1(INNLIVB)=FLAG(INNS(1))
	IFT=FLAG(INCC(1))+FLAG(INCC(2))+FLAG(INCC(3))+FLAG(INCC(4))
	FLAG1(INITLIV(1))=IFT
	FLAG1(INITLIV(2))=IFT
	FLAG1(INITLIV(3))=IFT

250	continue	
	GO TO 1

1000	type*,'errore tipo messaggio   *non previsto*'

1	return

	end


	SUBROUTINE NUVOMA(CL,CM,CH,CCL,CCM,CCH)
COMSTART NUVOMA
C	SUBROUTINE NUVOMA(CL,CM,CH,CCL,CCM,CCH)
C
C	converte il codice delle nubi del synottico gruppo 8 del nazionale
C	con la codifica metar

C	IN:

C	CL	INTEGER*2	codice delle nubi del synoP gruppo 8 nazionale
C	CM	INTEGER*2	codice delle nubi del synoP gruppo 8 nazionale
C	CH	INTEGER*2	codice delle nubi del synoP gruppo 8 nazionale

C	OUT:
c
C	CCL	INTEGER*2	codifica metar NUBI BASSE
C	CCM	INTEGER*2	codifica metar NUBI MEDIE
C	CCH	INTEGER*2	codifica metar NUBI ALTE
COMEND
C	CHARACTER*2 L(5),M(2),H(3),CCL,CCM,CCH
C	DATA L/'CU','SC','NS','ST','CB'/
C	DATA M/'AS','AC'/
C	DATA H/'CI','CS','CC'/

	INTEGER*2 CL,CM,CH
	INTEGER*2 L(5),M(2),H(3),CCL,CCM,CCH
	DATA L/8,6,5,7,9/
	DATA M/4,3/
	DATA H/0,2,1/

	IF(CL.LT.10)THEN
		IF(CL.EQ.0)CCL=99
		IF(CL.LE.3.AND.CL.GT.0)CCL=L(1)
		IF(CL.EQ.4.OR.CL.EQ.5.OR.CL.EQ.8)CCL=L(2)
		IF(CL.EQ.6)CCL=L(3)
		IF(CL.EQ.7)CCL=L(4)
		IF(CL.EQ.9)CCL=L(5)
	ELSE
		CCL=32767
	END IF

	IF(CM.LT.10)THEN
		IF(CM.EQ.0)CCM=99
		IF(CM.LE.2.AND.CM.GT.0)CCM=M(1)
		IF(CM.GT.2)CCM=M(2)
	ELSE
		CCM=32767
	END IF

	IF(CH.LT.10)THEN
		IF(CH.EQ.0)CCH=99
		IF(CH.LE.4.AND.CH.GT.0)CCH=H(1)
		IF(CH.EQ.5.OR.CH.EQ.6.OR.CH.EQ.7.OR.CH.EQ.8)CCH=H(2)
		IF(CH.EQ.9)CCH=H(3)
	ELSE
		CCH=32767
	END IF
	RETURN
	END

	SUBROUTINE NUBIMA (NS,CC,NU,NBST,ITLIV)
COMSTART NUBIMA
C	SUBROUTINE NUBIMA (NS,CC,NU,NBST,ITLIV)
c
C	converte le nubi del METAR ordinandole per livello e calcolando
C	la copertura totale
c
C	IN:
c
C	NS(4)	INTEGER*2	nuvolosita` per singolo tipo
c	CC(4)	CHARACTER*2	tipo nube
c
c	OUT:
c
C	NU	INTEGER*2	nuvolosita` totale
c	NBST	INTEGER*2	nuvolosita` NUBI BASSE
C	ITLIV(3)	INTEGER*2	tipo nube
COMEND
	
	INTEGER*2 NS(4),NU,NBST,ITLIV(3)
	integer*4 NB(4),NA(4),NM(4)
	INTEGER ITB(4),ITM(4),ITA(4),IT
	DIMENSION ALFA(9)
	CHARACTER*2 GB(10),CC(4),TB(4),TM(4),TA(4)
	DATA ALFA/1.,.875,.75,.625,.5,.375,.25,0.125,0./
	DATA GB/'CI','CC','CS','AC','AS','NS','SC','ST','CU','CB'/

	LB=0
	LM=0
	LA=0

	NU=0
	NBST=0
	DO IH=1,3
		ITLIV(IH)=99
	END DO

C	verifica se il metar riporta CAVOK e nel caso pone le coperture=0
	IF((CC(1).EQ.'CA').AND.(CC(2).EQ.'VO')) GOTO 24
	
	IF (NS(1).EQ.9)then
	  NU=9
	  goto 55			!SE E` UGUALE A 9 NU=9
	end if

	do j=1,4
		if(ns(j).eq.0)then
			NU=32767
			goto 55		!dato errato (controllo di qualita`)
		end if
	end do
	goto 66

55	NBST=32767			!pongo a dato mancante
	DO IH=1,3
		ITLIV(IH)=32767
	END DO
	GO TO 24

66	continue
	IF(NS(1).GT.9)GOTO24		!SE E` MAGGIORE DI 9 !PONI TUTTO A 0

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
	NBST=NUB
	IF (NBST.EQ.0)NBST=NUM		!!

	ITV=-1
	DO I=1,LB
	  CALL NUV_MET_SYN(TB(I),IT,IER)
	  IF(IT.GT.ITV)ITV=IT
	END DO	
	IF(ITV.GE.0)ITLIV(1)=ITV

	ITV=-1
	DO I=1,LM
	  CALL NUV_MET_SYN(TM(I),IT,IER)
	  IF(IT.GT.ITV)ITV=IT
	END DO	
	IF(ITV.GE.0)ITLIV(2)=ITV

	ITV=-1
	DO I=1,La
	  CALL NUV_MET_SYN(TA(I),IT,IER)
	  IF(IT.GT.ITV)ITV=IT
	END DO	
	IF(ITV.GE.0)ITLIV(3)=ITV

24	CONTINUE
	RETURN
	END

	LOGICAL FUNCTION VF(FLAG)
	BYTE FLAG
	COMMON /FLAGSOGLIA/IATT	
	DATA IATT/3/

	IF(FLAG.GE.IATT)THEN
		VF=.FALSE.
	ELSE
		VF=.TRUE.
	END IF
	RETURN
	END

C	FUNCTION NUVCOM(LL,NN)
CC	somma le coperture allo stesso livello forzandola a 8 se e` maggiore
C	INTEGER*4 NN(4),IS
C
C	IS=0
C	DO JJ=1,LL
C		IS=IS+NN(JJ)
C	END DO
C	IF(IS.GT.8) IS=8
C	NUVCOM=IS
C	RETURN 
C	END

	SUBROUTINE NUBIMA_new (NS,CC,hhh,NU,NBST,ITLIV)
COMSTART NUBIMA
C	SUBROUTINE NUBIMA_new (NS,CC,hhh,NU,NBST,ITLIV)
c
C	converte le nubi del METAR ordinandole per livello e calcolando
C	la copertura totale
c	per versione metar dopo il 1/7/93
c
C	IN:
c
C	NS(4)	INTEGER*2	nuvolosita` per singolo tipo
C	HHH(4)	INTEGER*2	altezza nube per singolo tipo
c	CC(4)	CHARACTER*2	tipo nube
c
c	OUT:
c
C	NU	INTEGER*2	nuvolosita` totale
c	NBST	INTEGER*2	nuvolosita` NUBI BASSE
C	ITLIV(3)	INTEGER*2	tipo nube (solo CU o CB)
COMEND
	
	INTEGER*2 NS(4),hhh(4),NU,NBST,ITLIV(3)
	integer*4 NB(4),NA(4),NM(4)
	INTEGER ITB(4),ITM(4),ITA(4),IT
	DIMENSION ALFA(9)
	CHARACTER*2 CC(4)
	DATA ALFA/1.,.875,.75,.625,.5,.375,.25,0.125,0./

	LB=0
	LM=0
	LA=0

	NU=0
	NBST=0
	DO IH=1,3
		ITLIV(IH)=99
	END DO

C	verifica se il metar riporta CAVOK e nel caso pone le coperture=0
	IF((CC(1).EQ.'CA').AND.(CC(2).EQ.'VO')) GOTO 24
	
	IF (NS(1).EQ.9)then
	  NU=9
	  goto 55			!SE E` UGUALE A 9 NU=9
	end if

	do j=1,4
		if(ns(j).eq.0)then
			NU=32767
			goto 55		!dato errato (controllo di qualita`)
		end if
	end do
	goto 66

55	NBST=32767			!pongo a dato mancante
	DO IH=1,3
		ITLIV(IH)=32767
	END DO
	GO TO 24

66	continue
	IF(NS(1).GT.9)GOTO24		!SE E` MAGGIORE DI 9 !PONI TUTTO A 0

	DO J=1,4
		IF (NS(J).LE.8)THEN

			if(hhh(j).gt.30000)goto 23
			if(hhh(j).gt.6000) goto 20
			if(hhh(j).gt.3000) goto 21
					   goto 22

20				LA=LA+1
				NA(LA)=NS(J)
			GO TO 23
21				LM=LM+1
				NM(LM)=NS(J)
			GO TO 23
22				LB=LB+1
				NB(LB)=NS(J)
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
	NBST=NUB
	IF (NBST.EQ.0)NBST=NUM		!!

					! non mi danno piu` il tipo nubi
	if(nub.gt.0)itliv(1)=32767
	if(num.gt.0)itliv(2)=32767
	if(nua.gt.0)itliv(3)=32767

	ITV=-1
	do j=1,4
	  IF (NS(J).gt.8)goto 25
	    CALL NUV_MET_SYN(cc(j),IT,IER)
	    if(ier.ne.0)goto 25
						! salvo che in questi rari casi
	    IF(IT.GT.ITV.and.it.ge.8.and.it.le.9)ITV=IT		!scarto i 99
25	END DO	
	IF(ITV.GE.0)ITLIV(1)=ITV		! solo per le nubi basse

24	CONTINUE
	RETURN
	END


	subroutine antimaronemet(ier)

	COMMON /RRW/RRW

	INTEGER*2 imis
	CHARACTER*2 cmis

	include 'megainc$dir:meteodata.inc'

	equivalence (cmis,imis)
	data imis/32767/

	ier=0
	do i=1,4
		if(ns(i).eq.2)ns(i)=3
	end do	
	if(ww(1:3).eq.'FEW')then
	  do i=4,2,-1
	    ns(i)=ns(i-1)
	    cc(i)=cc(i-1)
	    hhh(i)=hhh(i-1)
	  end do
	  ns(1)=1
	  read (ww(4:6),'(i3)',err=99)hhh(1)
	  hhh(1)=hhh(1)*30			!converte in metri
	  cc(1)=ww(7:8)
	  do i=1,7,2
		ww(i:i+1)=cmis		!tempo presente mancante
	  end do
	end if

	goto 10
	
99	hhh(1)=32767
	cc(1)=cmis			!setto bit a 1 per character
	do i=1,7,2
		ww(i:i+1)=cmis		!tempo presente mancante
	end do

10	return
	end

	function	NTPRmet_to_syn(ww,ns)


COMSTART nTPRmet_to_syn
C	function	NTPRmet_to_syn(ww,ns)
c
c	Restituisce il valore del tempo presente corrispettivo del synop
c	espresso con il codice ww del synop stesso.
c
c	integer function nTPRmet_to_syn :  in input richiede il tempo presente
c	del metar ww e la copertura nuvolosa
c	del primo livello ns(1)  -
c
c	input:
c	character	ww*8		tempo presente metar
c	integer*2	ns		copertura ns(1) metar
c
c	se non c'e` ww allora non ho niente da segnalare (=0)
c	se riesco a convertire > bene!
c	altrimenti mancante (=32767)
c
c	il cielo invisibile viene codificato con VV??? che il meteodata
c	riporta nel gruppo nubi con ns(1)=9 e hs appropriato
c
c
c	prima versione   13-02-96				Andrea Selvini
c       seconda versione 25-10-96				Sala Operativa
c       terza versione    3-02-97				Paolo Patruno
c       4ta versione      7-03-97				Paolo Patruno
c       5ta versione      25-03-97				Patruno Grazzini
c								Selvini
c
c Tabella di conversione 
c
c cod. syn         cod. met           Copertura (ns)
c
c  10		   BR
c  12		   MIFG
c  17		   TS
c  18		   SQ
c  19	 	   FC
c  41		   BCFG
c  44		   FG 			<9
c  45		   FG			=9
c  48		   FZFG			<9
c  49		   FZFG			=9
c  51		   -DZ
c  53		   DZ
c  55		   +DZ
c  56		   -FZDZ
c  57		   FZDZ
c  57		   +FZDZ
c  58	  	   -DZRA
c  58		   -RADZ
c  59		   DZRA
c  59		   RADZ
c  59	 	   +DZRA
c  59		   +RADZ
c  61		   -RA
c  63		   RA
c  65		   +RA
c  66		   -FZRA
c  66		   -RAFZ
c  67		   FZRA
c  67		   RAFZ
c  67		   +FZRA
c  67		   +RAFZ
c  68		   -DZSN
c  68		   -SNDZ
c  68              -RASN
c  68		   -SNRA
c  69		   DZSN
c  69		   SNDZ
c  69		   +DZSN
c  69		   +SNDZ
c  69		   RASN
c  69		   SNRA
c  69		   +RASN
c  69		   +SNRA
c  71		   -SN
c  73		   SN
c  75		   +SN
c  76		   IC
c  77		   SG
c  79		   PE
c  80		   -SHRA
c  81		   SHRA
c  82		   +SHRA
c  83		   -SHRASN
c  83		   -SHSNRA
c  84		   SHRASN
c  84		   SHSNRA
c  84		   +SHRASN
c  84		   +SHSNRA
c  85		   -SHSN
c  86		   SHSN
c  86		   +SHSN
c  87		   -SHGS
c  87		   -SHGSRA
c  87		   -SHGSRASN
c  87		   -SHGSSNRA
c  87		   -SHSG
c  87		   -SHSGRA
c  87		   -SHSGRASN
c  87		   -SHSGSNRA
c  88		   SHGS
c  88		   SHGSRA
c  88		   SHGSRASN
c  88		   SHGSSNRA
c  88		   +SHGS
c  88		   +SHGSRA
c  88		   +SHGSRASN
c  88		   +SHGSSNRA
c  88		   SHSG
c  88		   SHSGRA
c  88		   SHSGRASN
c  88		   SHSGSNRA
c  88		   +SHSG
c  88		   +SHSGRA
c  88		   +SHSGRASN
c  88		   +SHSGSNRA
c  89		   -SHGR
c  89              -SHGRRA
c  89              -SHGRRASN
c  89              -SHGRSNRA
c  90		   SHGR
c  90              SHGRRA
c  90              SHGRRASN
c  90              SHGRSNRA
c  90              +SHGR
c  90              +SHGRRA
c  90              +SHGRRASN
c  90              +SHGRSNRA
c  95		   -TSRA
c  95		   -TSSN
c  95		   -TSRASN
c  95		   -TSSNRA
c  95		   TSRA
c  95              TSSN
c  95              TSRASN
c  95              TSSNRA
c  96		   -TSGR
c  96		   TSGR
c  96		   -TSSG
c  96		   TSSG
c  96		   -TSGS
c  96		   TSGS
c  97   	   +TSRA
c  97              +TSSN
c  97              +TSRASN
c  97              +TSSNRA	
c  98		   TSSS
c  98		   TSDS
c  99		   +TSGR
c  99		   +TSSG
c  99		   +TSGS
COMEND

	parameter (nww=121)
	character	ww*8
	integer*2	ns

	logical ch_c_e

	integer*2	convi(nww) 
	1	/0,10,12,17,18,19,41,44,45,48,
	1	49,51,53,55,56,57,57,58,58,59,
	1	59,59,59,61,63,65,66,66,67,67,
	1	67,67,68,68,68,68,69,69,69,69,
	1	69,69,69,69,71,73,75,76,77,79,
	1	80,81,82,83,83,84,84,84,84,85,
	1	86,86,87,87,87,87,87,87,87,87,
	1	88,88,88,88,88,88,88,88,88,88,
	1	88,88,88,88,88,88,89,89,89,89,
	1	90,90,90,90,90,90,90,90,95,95,
	1	95,95,95,95,95,95,96,96,96,96,
	1	96,96,97,97,97,97,98,98,99,99,
	1	99/
c Tabella di conversione 

	character*8 convs(nww) /
	1 ' ','BR','MIFG','TS','SQ','FC','BCFG','FG','FG','FZFG',

	1 'FZFG','-DZ','DZ','+DZ','-FZDZ','FZDZ','+FZDZ','-DZRA',
	1	'-RADZ','DZRA',

	1 'RADZ','+DZRA','+RADZ','-RA','RA','+RA','-FZRA','-RAFZ',
	1	'FZRA','RAFZ',

	1 '+FZRA','+RAFZ','-DZSN','-SNDZ','-RASN','-SNRA',
	1	'DZSN','SNDZ','+DZSN','+SNDZ',

	1 'RASN','SNRA','+RASN','+SNRA','-SN',
	1	'SN','+SN','IC','SG','PE',

	1 '-SHRA','SHRA','+SHRA','-SHRASN','-SHSNRA',
	1	'SHRASN','SHSNRA','+SHRASN','+SHSNRA','-SHSN',

	1 'SHSN','+SHSN','-SHGS','-SHGSRA','-SHGSRASN','-SHGSSNRA',
	1	'-SHSG','-SHSGRA','-SHSGRASN','-SHSGSNRA',

	1 'SHGS','SHGSRA','SHGSRASN','SHGSSNRA','+SHGS',
	1	'+SHGSRA','+SHGSRASN','+SHGSSNRA','SHSG','SHSGRA',

	1 'SHSGRASN','SHSGSNRA','+SHSG','+SHSGRA','+SHSGRASN',
	1 	'+SHSGSNRA','-SHGR','-SHGRRA','-SHGRRASN','-SHGRSNRA',

	1 'SHGR','SHGRRA','SHGRRASN','SHGRSNRA','+SHGR','+SHGRRA',
	1	'+SHGRRASN','+SHGRSNRA','-TSRA','-TSSN',

	1 '-TSRASN','-TSSNRA','TSRA','TSSN','TSRASN','TSSNRA','-TSGR',
	1	'TSGR','-TSSG','TSSG',

	1 '-TSGS','TSGS','+TSRA','+TSSN','+TSRASN','+TSSNRA',
	1	'TSSS','TSDS','+TSGR','+TSSG',

	1 '+TSGS' /
	
c.......inizializzo il ww presente a ww presente mancante
	nTPRmet_to_syn=32767

	if (.not.ch_c_e(ww))then
		nTPRmet_to_syn=0
		return		
	end if

	do i=1,nww
		if(ww.eq.convs(i))then
			k=i
			goto 10
		end if

	end do

	return

10	continue

	if(ww.eq.'FG'.or.ww.eq.'FZFG')then
		if (ns.eq.9) k=k+1
	end if

	nTPRmet_to_syn=convi(k)
	
	return 
	end 
