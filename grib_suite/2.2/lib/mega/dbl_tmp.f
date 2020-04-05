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







	include 'megainc$dir:dbl.inc'
	parameter nrep=4

	character*100 filac(nrep)
	integer contro(nrep),datal(3)oral(2),datau(3),orau(2),
	1	datae(3),orae(2)
	logical primo(nrep)

	data iatt/2/
	data datal,oral,datau,orau/01,06,92,00,00,15,06,92,00,00/


	TYPE *,' INSERIRE LA FINESTRA DA UTILIZZARE (" "=estremi geo)'
	read (05,'(A8)')area
	IF (area .ne. ' ')GOTO 10
	JPROJ = 0
	TYPE*,' LATITUDINE MINIMA '
	ACCEPT *,RLTMN
	TYPE*,' LATITUDINE MASSIMA '
	ACCEPT *,RLTMX
	TYPE*,' LONGITUDINE MINIMA '
	ACCEPT *,RLNMN
	TYPE*,' LONGITUDINE MASSIMA '
	ACCEPT *,RLNMX
10	CONTINUE

11	TYPE*,'DATA,ORA INIZIALE'
	READ (05,*)DATA,ORA
	call jcontroldata(data(1),data(2),data(3),ier)
	if (ier.ne.0)goto 11
	call jcontrolora(ora(1),ora(2),ier)
	if (ier.ne.0)goto 11
	CALL JELADATA5(DATA(1),DATA(2),DATA(3),ORA(1),ORA(2),MIN_INI)

12	TYPE*,'DATA,ORA FINALE'
	READ (05,*)DATAF,ORAF
	call jcontroldata(dataF(1),dataF(2),dataF(3),ier)
	if (ier.ne.0)goto 12
	call jcontrolora(oraF(1),oraF(2),ier)
	if (ier.ne.0)goto 12
	CALL JELADATA5(DATAF(1),DATAF(2),DATAF(3),ORAF(1),ORAF(2),MIN_FIN)

	CALL SETI(12,12)

	CALL SET_CON_VEL_COMMON('OPT_GRAF_DBL.NAML',IER)
	IF(IER.NE.0)TYPE*,'IER=',IER

	CALL PLOT_GEO_FLASH(AREA,JPROJ,RLTMN,RLTMX,RLNMN,RLNMX
	1	,IFRAME,NFRAMES,IER)

	ier=N_GETMULTI(FILAC,CONTRO,PRIMO,ISTAZL,DATAL,ORAL,
	1	ISTAZU,DATAU,ORAU,STAZE,DATAE,ORAE,wwr,NREP,iatt)
	IF (IER.NE.0)GOTO 88

C=====================================================
	IF (STAZE.NE.ISTAZ(MAX(NSTAZ,1)))NSTAZ=NSTAZ+1

	ISTAZ(NSTAZ)=STAZE

	CALL JELADATA5(DATAE(1),DATAE(2),DATAE(3),
	1	ORAE(1),ORAE(2),IMINUTI)
	IM=(IMINUTI-MIN_INI)/60+1
	IMIN(NSTAZ,IM)=IMINUTI	

C====================================================

99	STOP
	END


	function N_GETMULTI(FILAC,CONTRO,PRIMO,ISTAZL,DATAL,ORAL,
	1	ISTAZU,DATAU,ORAU,STAZE,DATAE,ORAE,rep,NREP,iatt)

C	function N_GETMULTI(FILAC,CONTRO,PRIMO,ISTAZL,DATAL,ORAL,
C	1	ISTAZU,DATAU,ORAU,STAZE,DATAE,ORAE,rep,NREP,iatt)
C	
c	Retrieval dal METEODATAbase in sequenza dei records relativi ai reports 
c	compresi nella finestra datal/oral - datau/orau e con sottochiave
c	stazl - stazu. Ogni chiamata rende un report soddisfacente ai limiti
c	impostati.
c	la ricerca viene fatta contemporaneamente su NREP archivi contenenti
c	differenti tipi di report. Essi possono essere synop metar o locali o
c	agrmet.
c	viene restituito un unico report all'interno di un'ora dando priorita`
c	secondo l'ordine di FILAC. Il tracciato record e` unico e bisogna far
c	riferimento a megainc$dir:dbl.inc ; se si effettua l'include di questo
c	file bisogna passare il nome di variabile wwr al posto di rep.
c	La function effettua anche un controllo di qualita` climatico e di
c	consistenza per i report synop e metar e climatico per locali e agrmet.
c	Bisogna quindi passare la soglia IATT (vedi documentazione controllo 
c	di qualita`).
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
c				del file megainc$dir:dbl.inc).
c
c	nrep	i/p	i*4	numero di report su cui si vuole fare la 
c				ricerca contemporanea.
c
c	iatt	i/p	i*4	soglia di attendibilita` per controllo qualita`
c				(vedi documentazione controllo di qualita`).
c
c	n_getmulti o/p	i*4	= 0 tutto ok - il buffer contiene
c				il primo report in sequenza ricercato
c				= 1 fine ricerca
c				=-1 errore di i/o su file
c				=-2 limiti di ricerca errati
c				=10 tipo report non corretto
c	01/01/91
	PARAMETER NVAR=43

	include 'MEGAINC$DIR:METEODATA.inc'
	include 'MEGAINC$DIR:DBL.INC'
	BYTE WWRMUL(LREC*4,5)
	byte rep(lrec*4)

	COMMON /WWR/WWR
	COMMON /RRW/RRW
C	COMMON /CODER/CODER

	BYTE FLAG(NVAR)
	data flag/NVAR*0/

C	PARAMETER NBYTE=8,NBIT=8*NBYTE
C	CHARACTER*1 BITMAP(NBIT)
C	LOGICAL BITR
C	BYTE CODER(NBYTE)

	LOGICAL PRIMO(NREP)
	CHARACTER*100 FILAC(NREP)
	INTEGER*4 CONTRO(NREP),ERRORE,DATAL(3),ORAL(2),
	1	DATAU(3),ORAU(2),DATAE(3),ORAE(2),STAZE,TIPOE

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

	DO JK=1,NVAR
		FLAG(JK)=0
	END DO

C	SALTA ai vari tipi di elaborazione secondo il tipo di messaggio estratto
	IF (TIPOE.EQ.01)GOTO 101
	IF (TIPOE.EQ.10)GOTO 110
	IF (TIPOE.EQ.11)GOTO 111
	IF (TIPOE.EQ.12)GOTO 112
	n_getmulti=10
	RETURN

101	continue
	CALL QUASYNOPCLIM(FLAG,IER)
c	IF(IER.NE.0)TYPE*,'**** ERRORE QUASYNOPLIM',staze
	CALL QUASYNOPNULV(FLAG,IATT,IER)
c	IF(IER.NE.0)TYPE*,'**** ERRORE QUASYNOP',staze
	CALL QUASYNOPCONS(FLAG,IER)
c	IF(IER.NE.0)TYPE*,'**** ERRORE QUASYNOPCONS',staze
	CALL QUASYNOPNULV(FLAG,IATT,IER)
c	IF(IER.NE.0)TYPE*,'**** ERRORE QUASYNOP',staze
	GOTO 5

110	continue
	CALL QUAMETARCLIM(FLAG,IER)
c	IF(IER.NE.0)TYPE*,'**** ERRORE QUAMETARLIM',staze
	CALL QUAMETARNULV(FLAG,IATT,IER)
c	IF(IER.NE.0)TYPE*,'**** ERRORE QUAMETAR',staze
	CALL QUAMETARCONS(FLAG,IER)
c	IF(IER.NE.0)TYPE*,'**** ERRORE QUAMETARCONS',staze
	CALL QUAMETARNULV(FLAG,IATT,IER)
c	IF(IER.NE.0)TYPE*,'**** ERRORE QUAMETAR',staze
	GOTO 5

111	continue
	CALL QUALOCALCLIM(FLAG,IER)
c	IF(IER.NE.0)TYPE*,'**** ERRORE QUAMETARLIM',staze
	CALL QUALOCALNULV(FLAG,IATT,IER)
c	IF(IER.NE.0)TYPE*,'**** ERRORE QUAMETAR',staze
C	CALL QUALOCALCONS(FLAG,IER)
c	IF(IER.NE.0)TYPE*,'**** ERRORE QUAMETARCONS',staze
C	CALL QUALOCALNULV(FLAG,IATT,IER)
c	IF(IER.NE.0)TYPE*,'**** ERRORE QUAMETAR',staze
	GOTO 5

112	continue
	CALL QUAAGMETCLIM(FLAG,IER)
c	IF(IER.NE.0)TYPE*,'**** ERRORE QUAMETARLIM',staze
	CALL QUAAGMETNULV(FLAG,IATT,IER)
c	IF(IER.NE.0)TYPE*,'**** ERRORE QUAMETAR',staze
C	CALL QUALOCALCONS(FLAG,IER)
c	IF(IER.NE.0)TYPE*,'**** ERRORE QUAMETARCONS',staze
C	CALL QUALOCALNULV(FLAG,IATT,IER)
c	IF(IER.NE.0)TYPE*,'**** ERRORE QUAMETAR',staze
	GOTO 5

5	CONTINUE
	CALL MISCHIONE

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
		n_getmulti=-1
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
	MEM=1
	DO I=1,LREC*4
		WWRMUL(I,MEM)=WWR(I)
	END DO

	CALL GETHEA(STAZE,DATAE,ORAE,TIPOE,WWRMUL(1,MEM))  !per output

	RETURN

1	n_getmulti=-1
	RETURN

	END

	SUBROUTINE NEWKEYFOR (TIPO,DATA,ORA,STAZ,REC)
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

	SUBROUTINE MISCHIONE

C	SUBROUTINE MISCHIONE
C
C	Normalizza i messaggi in archivio meteodata di tipo synop metar e
c	locali su un nuovo e unico tracciato record.
c	Questa routine comunica tramite due common, uno in input e uno in
c	output. Il contenuto dei common viene definito includendo nel main
c	program le seguenti istruzioni
C
C	INCLUDE 'MEGAINC$DIR:METEODATA.INC'
C	INCLUDE 'MEGAINC$DIR:DBL.INC'
C	COMMON /RRW/RRW			! in input
C	COMMON /WWR/WWR			! in output
C
C	I report letti dall'archivio meteodata dovranno essere quindi
c	memorizzati nel buffer RRW definito nell'include.
C
	INTEGER IWMO,DATA(3),ORA(2),TIPO
	INTEGER*2 LOCIT(3)
	CHARACTER*4 ANOME*24
	LOGICAL C_E,CH_C_E
	PARAMETER T0=273.2

	COMMON /RRW/RRW
	COMMON /WWR/WWR

	INCLUDE 'MEGAINC$DIR:METEODATA.INC'
	INCLUDE 'MEGAINC$DIR:DBL.INC'

C	inizializza le variabili
	DO J=13,lrec*4,2
		WWR(J)=255
	END DO
	DO J=14,lrec*4,2
		WWR(J)=127
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

20	CONTINUE
	IF(.NOT.C_E(TDTD))GOTO 30
C	temperatura di rugiada
	ITDIST=TDTD
	goto 220

30	IF(C_E(UMID).AND.C_E(TATA))THEN
C	se c'e` l'umidita` percentuale e la temperatura viene calcolata la TD
	ITDIST=NINT((TRUG(FLOAT(UMID),FLOAT(TATA)/10.+T0)-T0)*10.)
	END IF

220	CONTINUE
	IF(.NOT.C_E(UMID))GOTO 330
C	umidita` relativa
	IUIST=UMID
	goto 40

330	IF(C_E(TDTD).AND.C_E(TATA))THEN
C	se c'e` la td e la temperatura viene calcolata la U
	IUIST=FR(FLOAT(TATA)/10.+T0,FLOAT(TDTD)/10.+T0)
	END IF

40	CONTINUE
	IF(.NOT.C_E(PRECI))GOTO 50
C	precipitazioni

	IF(C_E(TR))THEN
		IORAPREC=TR*60
	ELSE
		IORAPREC=6*60
		IF(MOD(ORA(1)+6,12).EQ.0)IORAPREC=12*60
	END IF

	ik=0
	IF(IORAPREC.EQ.6*60)IK=3
	IF(IORAPREC.EQ.12*60)IK=2

	if(ik.ne.0)IPREC(IK)=PRECI

50	CONTINUE

	IF(.NOT.C_E(FF).OR..NOT.C_E(DD))GOTO 60
C	vento istantaneo	! ricordarsi caso vento variabile(999)
	IFFIST=FF
	IDDIST=DD

60	CONTINUE
	IF(.NOT.C_E(VIS))GOTO 70
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
	GOTO 770

75	CONTINUE
	IF(.NOT.C_E(P0P0).OR..NOT.C_E(TATA))GOTO 770
	CALL GETSTAZ1(IWMO,ANOME,HHSLM,HHPOZ,ALAT,ALON,IER)
	IF (IER.EQ.0.AND.HHPOZ.GE.0.AND.HHPOZ.LE.500)IPRESSLM=
	1	NINT(10.*
	1	RIPORTAPRES(FLOAT(P0P0)/10.,FLOAT(TATA)/10.,HHPOZ))

770	CONTINUE
	IF(.NOT.C_E(P0P0))GOTO 775
C	pressione slm
	IPRESHSTAZ=P0P0
	GOTO 80

775	CONTINUE
	IF(.NOT.C_E(PRES).OR..NOT.C_E(TATA))GOTO 80
	CALL GETSTAZ1(IWMO,ANOME,HHSLM,HHPOZ,ALAT,ALON,IER)
	IF (IER.EQ.0.AND.HHPOZ.GE.0.AND.HHPOZ.LE.500)IPRESHSTAZ=
	1	NINT(10.*
	1	RIPORTAP0P0(FLOAT(PRES)/10.,FLOAT(TATA)/10.,HHPOZ))


80	CONTINUE
	IF(.NOT.C_E(TEMPRS))GOTO 90
C	tempo presente  
	ITPRES=TEMPRS

90	CONTINUE
	IF(.NOT.C_E(TEMPAS))GOTO 100
C	tempo passato
	ITPAS=TEMPAS

100	CONTINUE
	IF(.NOT.C_E(CART))GOTO 102
C	caratteristica
	ICART=CART

102	CONTINUE
	IF(.NOT.C_E(TEND))GOTO 105
C	tendenza
	ITEND=TEND

105	CONTINUE
	IF(.NOT.C_E(NTOT))GOTO 110
C	copertura totale
	IF(NTOT.LE.9)NTOTA=NTOT

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

120	CONTINUE

	GO TO 1


200	IF (TIPO.NE.11)GOTO 300
CC	i dati estratti sono di tipo locali

c	valore istantaneo temperatura
	IF(C_E(LISTT1)) ITIST=LISTT1

C	VALORE UMIDITA
	IF(C_E(LISTUM)) IUIST = LISTUM

	IF(.NOT.C_E(LISTUM).OR..NOT.C_E(LISTT1))GOTO 340
C	temperatura di rugiada
	ITDIST=NINT((TRUG(FLOAT(LISTUM),FLOAT(LISTT1)/10.+T0)-T0)*10.)

340	CONTINUE

c	IF(L1__IR.EQ.'4')GOTO 380	!OMESSO dati RRR non disponibili S S
c	IF(L1__IR.EQ.'3')THEN		!OMESSO precipitazione =0	 O Y
c		IPREC=0			!				 L N
c		IORAPREC=60		!				 O O
c		GOTO 380		!				   P
c	END IF

	IF(.NOT.C_E(LPRECI))GOTO 380
C	precipitazioni
	IPREC(4)=LPRECI

380	CONTINUE

C	vento istantaneo
	IF(C_E(LMMV10))	IFFIST=NINT(FLOAT(LMMV10)*0.194) !converte dm/s in nodi
	IF(C_E(LAMV10)) IDDIST=LAMV10
	IF(IFFIST.EQ.0) IDDIST=0

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
	IF(.NOT.C_E(LISTPR))GOTO 411

	IPRESHSTAZ=LISTPR

	IF(.NOT.C_E(LISTT1))GOTO 411

	CALL GETSTAZ1(IWMO,ANOME,HHSLM,HHPOZ,ALAT,ALON,IER)
	IF (IER.EQ.0.AND.HHPOZ.GE.0.AND.HHPOZ.LE.500)IPRESSLM=NINT(10.*
	1	RIPORTAPRES(FLOAT(LISTPR)/10.,FLOAT(LISTT1)/10.,HHPOZ))

411	CONTINUE
C	tempo presente e passato
	IF(L1__IX.EQ.'3'.OR .L1__IX.EQ.'6')GOTO 415
	IF(L1__IX.NE.'2'.AND.L1__IX.NE.'5')GOTO 412
C				niente da segnalare
	ITPRES=00
	ITPAS =00
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

415	CONTINUE

C	PARTE RELATIVA ALLA NUVOLOSITA`
C	NTOTA,NLIVB,ITLIV(3)

C	nuvolosita` totale
	CALL NCARINT(L1___N,1,1,IER)
	IF (IER.NE.0)GOTO 420
	READ (L1___N,'(I1)',ERR=420)NTOTA
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
440	END DO

C	cerco il tipo delle nubi nel gruppo 8 nazionale
C	decodificandola tipo metar e scrivendola nel vettore
		CALL NUVOMA (LOCIT(1),LOCIT(2),LOCIT(3),
	1	ITLIV(1),ITLIV(2),ITLIV(3))

C	nuvolosita` basse e medie
	CALL NCARINT(L4NCCC,1,1,IER)
	IF (IER.NE.0)GOTO 450
	READ (L4NCCC,'(I1)',ERR=450)NLIVB
450	CONTINUE

	IF (NTOTA.EQ.0)THEN
		NLIVB=0
		ITLIV(1)=99
		ITLIV(2)=99
		ITLIV(3)=99
	END IF

	GO TO 1

300	IF (TIPO.NE.10)GOTO 400
C	i dati estratti corrispondono al messaggio metar

c	temperature
	IF(C_E(TT)) ITIST=TT*10
	IF(C_E(TD)) ITDIST=TD*10
	IF(C_E(TT).AND.C_E(TD))IUIST=FR(FLOAT(TT)+T0,FLOAT(TD)+T0)

c	vento
	IF(C_E(FFF)) IFFIST=FFF
	IF(C_E(DDD)) IDDIST=DDD
	IF(IDDIST.EQ.370)IDDIST=999		!VARIABILE

C	visibilita`
	IF(C_E(VVVV)) IVISI=VVVV

C	pressione

	IF(.NOT.C_E(PH).OR..NOT.C_E(TT))GOTO 800
	CALL GETSTAZ1(IWMO,ANOME,HHSLM,HHPOZ,ALAT,ALON,IER)
	IF (IER.EQ.0.AND.HHPOZ.GE.0.AND.HHPOZ.LE.500)IPRESHSTAZ=
	1	NINT(10.*
	1	RIPORTAP0P0(FLOAT(PH),FLOAT(TT),HHPOZ))

800	CONTINUE

	IF(C_E(PH)) IPRESSLM=PH*10	

	IF (.NOT.CH_C_E(WW))GOTO 205
C	tempo presente
	READ(WW(1:2),'(I2)',ERR=210)ITPRESTMP
	ITPRES=ITPRESTMP

205	IF(CC(1).EQ.'CA'.AND.CC(2).EQ.'VO')ITPRES=00

210	CONTINUE
C	cerco la nuvolosita` e la decodifico

	CALL NUBIMA(NS,CC,NTOTA,NLIVB,ITLIV)

	GO TO 1


	IF (TIPO.NE.12)GOTO 1000

	INTEGER*2 T180,T50,UM,BAGNA,PREC,RAD,V10,EVAP,
	1	  T180MN,T180MX,T180ME,
	1	  T50MN,T50MX,T50ME,
	1	  UMMN,UMMX,UMME,V60

C	i dati estratti corrispondono al messaggio agrometeo

10	CONTINUE
	IF(.NOT.C_E(T180))GOTO 20
C	temperatura istantanea
	ITIST=T180

20	CONTINUE
	IF(C_E(UM).AND.C_E(T180))THEN
C	se c'e` l'umidita` percentuale e la temperatura viene calcolata la TD
	ITDIST=NINT((TRUG(FLOAT(UM),FLOAT(T180)/10.+T0)-T0)*10.)
	END IF

220	CONTINUE
	IF(.NOT.C_E(UM))GOTO 330
C	umidita` relativa
	IUIST=UM
	goto 40

330	CONTINUE
	IF(.NOT.C_E(PREC))GOTO 50
C	precipitazioni

	IPREC(4)=PREC

50	CONTINUE

	IF(.NOT.C_E(V10))GOTO 60
C	vento istantaneo
	IFFIST=V10

120	CONTINUE

	GO TO 1


1000	type*,'errore tipo messaggio   *non previsto*'

1	return

	end


	SUBROUTINE NUVOMA(CL,CM,CH,CCL,CCM,CCH)
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
	DO I=1,LH
	  CALL NUV_MET_SYN(TA(I),IT,IER)
	  IF(IT.GT.ITV)ITV=IT
	END DO	
	IF(ITV.GE.0)ITLIV(3)=ITV

24	CONTINUE
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
