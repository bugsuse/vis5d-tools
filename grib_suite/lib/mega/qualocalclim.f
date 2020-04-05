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





	SUBROUTINE QUALOCALCLIM(FLAG,IER)
COMSTART QUALOCALCLIM
c	FLAG(46)	BYTE	incrementato di uno se verificato errore
c				per le 43 variabili del record locali
c				su solglie strette, oppure increntato di 3
c				su soglie climatiche larghe.
c	IER		I*4	condizione di errore
C			IER=1	tipo messaggio errato
c			IER=-1	non trovo i dati anagrafici della stazione
c				e quindi faccio controlli piu` blandi oppure
C				le coordinate delle stazioni escono dalla 
c				finestra ammessa (regione Emilia Romagna)
c			IER=-2	errore climaumidita` o climatemperatura
c				o radmax, faccio i relativi controlli
c				piu` blandi
c			ier=100 troppe unita` logiche aperte
C			IER>100	codice iostat fortran+100
c
c	COMMON /RRW/RRW	I*2	RRW(89) per passare dati locali come dichiarato
c				in loc$inc:locali.inc
c
C	Per indirizzare il vettore FLAG alla variabile interessata si puo`
c	utilizzare, includendo nel proprio programma, il file
c	megainc$dir:qualocal.inc ed utilizzare come indici del; vettore le
c	variabili che hanno lo stesso nome della variabile (meteodata)
c	preceduto da 'IN'. Ad esempio:
c	per la temperatura        FLAG(INLISTT1)
COMEND
	COMMON /RRW/RRW

	DIMENSION LIMINF(46),LIMSUP(46)

C	INCLUDE 'MET$INC:LOCALI.INC'

	INTEGER*4 STAZ,DATA(3),ORA(2),TIPO
	BYTE FLAG(46)
	INTEGER*2 RRW(89)
	CHARACTER*24 ANOME
	LOGICAL C_E

C	INCLUDE 'MEGAINC$DIR:QUALOCAL.INC/NOLIST'

	DIMENSION PMED(12),PSTD(12)
	DATA PMED  
	1	/1017.8,1015.3,1015.4,1013.2,1014.1,1014.5,1014.4,
	1	 1014.6,1016.9,1018.5,1016.8,1018.6/
	DATA PSTD
	1	/   9.1,   9.7,   8.2,   6.7,   5.4,   4.7,   3.8,
	1	    4.2,   5.1,   7.5,   8.6,  10.5/
	DATA QS,USTD/2.5,15./
	DATA ALATMIN,ALATMAX,ALONMIN,ALONMAX/	!limiti dell'area Emilia R.
	1	43.4,45.1,9.1,12.5/

	ier=0

	CALL GETHEA (STAZ,DATA,ORA,TIPO,RRW)

	IF (TIPO.NE.11)THEN
		IER=1
		RETURN
	END IF

C	soglia inferiore per tutte le variabili
C	soglia superiore per tutte le variabili


C-------------------------------------------------------------------------------
C	ORARI MASSIMI E MINIMI A MEZZANOTTE
C-------------------------------------------------------------------------------

	IF(ORA(1).EQ.0)then
		ISUP=24*60+ORA(2)
		IINF=24*60-60
	ELSE
	        ISUP=ORA(1)*60+ORA(2)
	        IINF=ORA(1)*60-60
	END IF	


        liminf( 1)=     0          !LMVV60
        limsup( 1)=   500          !LMVV60
        liminf( 2)=     0          !LMMV10
        limsup( 2)=   500          !LMMV10
        liminf( 3)=     0          !LAMV10
        limsup( 3)=   360          !LAMV10
        liminf( 4)=     0          !LVMNVE
        limsup( 4)=   500          !LVMNVE
        liminf( 5)=     0          !LVMXVE
        limsup( 5)=   500          !LVMXVE
        liminf( 6)=    IINF        !LHMNVE
        limsup( 6)=    ISUP        !LHMNVE
        liminf( 7)=    IINF        !LHMXVE
        limsup( 7)=    ISUP        !LHMXVE
        liminf( 8)=     0          !LDIPVE
        limsup( 8)=     7          !LDIPVE
        liminf( 9)=  -300          !LISTT1
        limsup( 9)=   500          !LISTT1
        liminf(10)=  -300          !LMEDT1
        limsup(10)=   500          !LMEDT1
        liminf(11)=  -300          !LVMNT1
        limsup(11)=   500          !LVMNT1
        liminf(12)=  -300          !LVMXT1
        limsup(12)=   500          !LVMXT1
        liminf(13)=   IINF         !LHMNT1
        limsup(13)=   ISUP         !LHMNT1
        liminf(14)=   IINF         !LHMXT1
        limsup(14)=   ISUP         !LHMXT1
        liminf(15)=  -300          !LISTT2
        limsup(15)=   500          !LISTT2
        liminf(16)=  -300          !LMEDT2
        limsup(16)=   500          !LMEDT2
        liminf(17)=  -300          !LVMNT2
        limsup(17)=   500          !LVMNT2
        liminf(18)=  -300          !LVMXT2
        limsup(18)=   500          !LVMXT2
        liminf(19)=   IINF         !LHMNT2
        limsup(19)=   ISUP         !LHMNT2
        liminf(20)=   IINF         !LHMXT2
        limsup(20)=   ISUP         !LHMXT2
        liminf(21)=  -300          !LISTTS
        limsup(21)=   500          !LISTTS
        liminf(22)=  -300          !LMEDTS
        limsup(22)=   500          !LMEDTS
        liminf(23)=  -300          !LVMNTS
        limsup(23)=   500          !LVMNTS
        liminf(24)=  -300          !LVMXTS
        limsup(24)=   500          !LVMXTS
        liminf(25)=   IINF         !LHMNTS
        limsup(25)=   ISUP         !LHMNTS
        liminf(26)=   IINF         !LHMXTS
        limsup(26)=   ISUP         !LHMXTS
        liminf(27)=    30          !LISTUM
        limsup(27)=   100          !LISTUM
        liminf(28)=    30          !LMEDUM
        limsup(28)=   100          !LMEDUM
        liminf(29)=    30          !LVMNUM
        limsup(29)=   100          !LVMNUM
        liminf(30)=    30          !LVMXUM
        limsup(30)=   100          !LVMXUM
        liminf(31)=   IINF         !LHMNUM
        limsup(31)=   ISUP         !LHMNUM
        liminf(32)=   IINF         !LHMXUM
        limsup(32)=   ISUP         !LHMXUM
        liminf(33)=  8800          !LISTPR
        limsup(33)= 10400          !LISTPR
        liminf(34)=  8800          !LMEDPR
        limsup(34)= 10400          !LMEDPR
        liminf(35)=  8800          !LVMNPR
        limsup(35)= 10400          !LVMNPR
        liminf(36)=  8800          !LVMXPR
        limsup(36)= 10400          !LVMXPR
        liminf(37)=    IINF        !LHMNPR
        limsup(37)=    ISUP        !LHMNPR
        liminf(38)=    IINF        !LHMXPR
        limsup(38)=    ISUP        !LHMXPR
        liminf(39)=     0          !LISTRS
        limsup(39)=    20          !LISTRS
        liminf(40)=     0          !LINTRS
        limsup(40)=  2280          !LINTRS
        liminf(41)=     0          !LVMNRS
        limsup(41)=    20          !LVMNRS
        liminf(42)=     0          !LVMXRS
        limsup(42)=    20          !LVMXRS
        liminf(43)=    IINF        !LHMNRS
        limsup(43)=    ISUP        !LHMNRS
        liminf(44)=    IINF        !LHMXRS
        limsup(44)=    ISUP        !LHMXRS
        liminf(45)=     0          !LINSOL
        limsup(45)=  1440          !LINSOL
        liminf(46)=     0          !LPRECI
        limsup(46)=  4092          !LPRECI

C				limiti climatici veramente estremi!
	DO NVAR=7,52
	  IF(C_E(RRW(NVAR)))THEN
	    IF (RRW(NVAR).LT.LIMINF(NVAR-6))FLAG(NVAR-6)=FLAG(NVAR-6)+2
	    IF (RRW(NVAR).GT.LIMSUP(NVAR-6))FLAG(NVAR-6)=FLAG(NVAR-6)+2
	  ENDIF	
	END DO

C	leggo i dati anagrafici solo se non sono gia` stati letti

	call GETSTAZ1(staz,ANOME,HHSLM,HHPOZ,ALAT,ALON,IER)
C	
C	Serve ad associare ad una stazione i relativi parametri anagrafici
C	Letti nell' archivio meteodata.
C
C	Input	:	STAZ	= I*4	Codice W.M.O. stazione
C
C	Output	:	ANOME   = C*24	Nome della stazione
C			HHSLM	= R*4	Altezza stazione S.L.M.(m.)
C			HHPOZ	= R*4   Altezza del pozzetto (m.)
C			ALAT	= R*4	Latitudine ( Gradi,primi)
C			ALON	= R*4	Longitudine (Gradi,primi)

D	type*,'estrastaz ier=',ier

c	limiti per pressione al livello della stazione
	IF (HHPOZ.LT.-100.)HHPOZ=HHSLM	!se non c'e` altezza pozzetto uso
					!altezza stazione
	IF (HHPOZ.LT.-100.)THEN
		IER=-1
		GOTO 23
	END IF

C-------------------------------------------------------------------------------
C	CONTROLLO SE DENTRO LIMITI REGIONALI
C-------------------------------------------------------------------------------
	IF (ALON.LT.ALONMIN.OR.ALON.GT.ALONMAX.OR.
	1	ALAT.LT.ALATMIN.OR.ALAT.GT.ALATMAX)THEN
			IER=-1
			GOTO 23
	ENDIF

	alat=sess_to_cent(alat)
	alon=sess_to_cent(alon)

C-------------------------------------------------------------------------------
C	UMIDITA
C-------------------------------------------------------------------------------

	CALL CLIMA_UMIDITA(DATA(2),ORA(1),val1,val2,IER)
	if(ier.ne.0)then
		ier=-2
		goto 25
	end if

	IF(VAL2.LE.USTD)THEN
		LIMINF(27)=50
		LIMINF(28)=50
		LIMINF(29)=50
	END IF
	IF(VAL1.LE.USTD)THEN
		LIMINF(27)=80
		LIMINF(28)=80
		LIMINF(29)=80
	END IF
	IF(100-VAL1.LE.USTD)THEN
		LIMSUP(27)=80
		LIMSUP(28)=80
		LIMSUP(29)=80
	END IF

25	continue
	IF (HHPOZ.LT.0)HHPOZ=0.

C-------------------------------------------------------------------------------
C	RADIAZIONE
C-------------------------------------------------------------------------------
	
	IORAI=ORA(1)-1
	IF(IORAI.LT.0)IORAI=23
	CALL RADMAX(DATA(1),DATA(2),DATA(3),IORAI,0,ORA(1),ORA(2),
	1		cent_to_sess(ALAT),cent_to_sess(ALON),
	1		HHPOZ,VALMAX,VALMIN,ier)
	if (ier.ne.0)then
		ier=-2
		goto 26
	end if

	LIMINF(40)=NINT(VALMIN)			!con cielo coperto
	LIMSUP(40)=NINT(VALMAX)			!con cielo sereno

26	continue
C-------------------------------------------------------------------------------
C	INSOLAZIONE
C-------------------------------------------------------------------------------
	call CLIMA_INSOLAZIONE(ALAT,data(1),data(2),data(3),
	1	ORA(1),ora(2),INS)
	limsup(45)=ins

C-------------------------------------------------------------------------------
C	TEMPERATURA IN CAPANNINA
C-------------------------------------------------------------------------------
	CALL CLIMA_TEMP_MAX_MIN
	1	(ALAT,ALON,HHPOZ,DATA(2),DATA(1),ORA(1),VAL,
	1	SIGMAX,SIGMIN,SIGMED,IER)
	if(ier.ne.0)then
		ier=-2
		goto 27
	end if
	
	LIMINF(9) = NINT((VAL-QS*SIGMED)*10)
	LIMINF(10)= NINT((VAL-QS*SIGMED)*10)
	LIMINF(11)= NINT((VAL-QS*SIGMIN)*10)
	LIMINF(12)= NINT((VAL-QS*SIGMAX)*10)
	
	LIMSUP(9) = NINT((VAL+QS*SIGMED)*10)
	LIMSUP(10)= NINT((VAL+QS*SIGMED)*10)
	LIMSUP(11)= NINT((VAL+QS*SIGMIN)*10)
	LIMSUP(12)= NINT((VAL+QS*SIGMAX)*10)

27	continue
C-------------------------------------------------------------------------------
C	PRESSIONE
C-------------------------------------------------------------------------------
	PMEDHSTAZ=RIPORTAP0P0(PMED(DATA(2)),VAL,HHPOZ)	!clima alla staz.

	IPMEDNOWI=NINT((PMEDHSTAZ-PSTD(DATA(2))*QS)*10)	!inferiore
	IPMEDNOWS=NINT((PMEDHSTAZ+PSTD(DATA(2))*QS)*10)	!superiore

	LIMINF(33)=IPMEDNOWI
	LIMINF(34)=IPMEDNOWI
	LIMINF(35)=IPMEDNOWI
	LIMINF(36)=IPMEDNOWI

	LIMSUP(33)=IPMEDNOWS
	LIMSUP(34)=IPMEDNOWS
	LIMSUP(35)=IPMEDNOWS
	LIMSUP(36)=IPMEDNOWS

23	CONTINUE	!FINE CASI PARTICOLRI INIZIO CONTROLLO

	DO NVAR=7,52
	  IF(C_E(RRW(NVAR)))THEN
	    IF (RRW(NVAR).LT.LIMINF(NVAR-6))then
	FLAG(NVAR-6)=FLAG(NVAR-6)+1
D	type *,rrw(nvar),LIMINF(NVAR-6),val
	endif
	    IF (RRW(NVAR).GT.LIMSUP(NVAR-6))then
	FLAG(NVAR-6)=FLAG(NVAR-6)+1
D	type *,rrw(nvar),LIMsup(NVAR-6)
	endif
	  ENDIF	
	END DO

	RETURN
	END

C-------------------------------------------------------------------------------
C	SUBROUTINE CLIMA_UMIDITA(IMESEP,IORAP,val1,val2,IER)
C-------------------------------------------------------------------------------
	SUBROUTINE CLIMA_UMIDITA(IMESEP,IORAP,val1,val2,IER)

COMSTART CLIMA_UMIDITA
C	SUBROUTINE CLIMA_UMIDITA(IMESEP,IORAP,val1,val2,IER)
C
C	Questa routine fornisce la probabilita` con cui si puo` verificare
c	una umidita` compresa in un intervallo definito. Questa probabilita`
c	e` stata ricavata dal data base orario di umidita` media oraria di
c	S.Pietro Capofiume (Alberoni-Patruno SMR 1992).
c	Per un dato mese e una data ora vengono restituite due frequenze
c	espresse in percentuale: una e` la frequenza delle umidita` al di
c	sotto del 50% e l'altra delle umidita` ad di sotto del 80%.
c
c	input:
c
c	IMESEP		I*4	mese
C	IORAP		I*4	ora (0/23) in UTM
C
C	output:
C
C	VAL1		R*4	probabilita` percentuale di umidita` inferiore
c				al 80%
C	VAL2		R*4	probabilita` percentuale di umidita` inferiore
c				al 50%
C	IER		i*4	condizione di errore
c				ier=0 tutto O.K.
c				ier=100 errole allocazione unita` per lettura
c				ier=100+iostat  (codice errore apertura file)
COMEND

	include '($FORIOSDEF)'

	DIMENSION SOTTO_80(12,0:23)		! Inverno notte, Estate notte
	DIMENSION SOTTO_50(12,0:23)		! Inverno giorno
	DATA IUN /0/

	IER=0

C-------------------------------------------------------------------------------
C
C	lettura dati 
C
C-------------------------------------------------------------------------------

	if (Iun.GT.0) GOTO 50		!hai gia` letto, non ripeterti!!

	call lib$get_lun(Iun)		!cerca unita`
	    if (Iun.lt.0)THEN
	    IER=100
	    return
	ENDIF

c	aperura del file che contiene le frequenze percentuali
c	viene testato se il file risulta locked ritentando l'apertura per
c	dieci volte a distanza di due secondi.

	NUMRIP=0
7	OPEN (UNIT=iun,FILE='megadati$dir:TABELLA_UMIDITA_COMPLETA.DAT',
	1	STATUS='OLD',FORM='FORMATTED',READONLY,IOSTAT=IOS)

	IF(IOS.EQ.0)go to 9

	IF(IOS.EQ.FOR$IOS_OPEFAI.AND.NUMRIP.LT.10)THEN
		call lib$wait(2.)
		NUMRIP=NUMRIP+1
		go to 7
	ELSE
		ier=ios+100
		iun=0
		return
	END IF

9	READ(IUN,FMT='()')
	DO IORA=0,23
	  READ(IUN,1000)(SOTTO_80(IMESE,IORA),IMESE=1,12)
	ENDDO
	READ(IUN,FMT='()')
	DO IORA=0,23
	  READ(IUN,1000)(SOTTO_50(IMESE,IORA),IMESE=1,12)
	ENDDO
1000	FORMAT(12F6.1)
	CLOSE(UNIT=IUN)
	CALL LIB$FREE_LUN(IUN)

50	VAL1=SOTTO_80(IMESEP,IORAP)
	VAL2=SOTTO_50(IMESEP,IORAP)

	RETURN
	END

C-------------------------------------------------------------------------------
C	SUBROUTINE CLIMA_INSOLAZIONE(ALAT,IGG,IMM,IAA,IORA,IMIN,INS)
C-------------------------------------------------------------------------------
	SUBROUTINE CLIMA_INSOLAZIONE(ALAT,IGG,IMM,IAA,IORA,IMIN,INS)

COMSTART CLIMA_INSOLAZIONE
C	SUBROUTINE CLIMA_INSOLAZIONE(ALAT,IGG,IMM,IAA,IORA,IMIN,INS)
C
C	Restituisce il numero di minuti massimo possibile con insolazione 
c	ad iniziare dalla mezzanotte fino ad un'ora specificata, 
c	data la latitudine e una data. 
c	In particolare alle ore 00 viene fornito il totale giornaliero.
C
C	input:
c
C	ALAT	R*4	latitudine in centesimale
C	IGG	I*4	giorno 
C	IMM	I*4	mese
C	IAA	I*4	anno
C	IORA	I*4	ora a cui termina la sommatoria dalla mezzanotte (UTM)
C	IMIN	I*4	minuti a cui termina la sommatoria dalla mezzanotte
c
C	output:
c
C	INS	I*4	tempo di insolazione massimo possibile espresso in
c			minuti
COMEND
	IDATE=NDAYS(IGG,IMM,IAA)-NDAYS(1,1,IAA)	!giorno dell'anno
	DJ=360*IDATE/365.25
	DELTA=ASIND(.3978*SIND(DJ-80.2+1.92*SIND(DJ-2.8)))
	S0=ACOSD(-TAND(DELTA)*TAND(ALAT))/7.5	! DURA
	TR=12.-S0/2.				! SORGE
	TS=12.+S0/2.				! TRAMONTA

	TR=TR-.25		!anticipo di 1/4 di ora il sorgere
	TS=TS+.25		!posticipo di 1/4 di ora il tramonto

	ORA=IORA+(FLOAT(IMIN)/60.)

	if(ora.eq.0)THEN
	  ora=24.		!a mezzanotte c'e` il totale giornaliero	
	ELSE
	  ORA=ORA+1.
	ENDIF

	RINS=MAX(0.,(ORA-TR))

	RINS=MIN(RINS,(TS-TR))

	INS=NINT(RINS*60.)
	
	RETURN
	END

C-------------------------------------------------------------------------------
C	SUBROUTINE CLIMA_TEM_NORM(IMESE,IORA,CLIMNORM,ier)
C
C	Questa routine restituisce un valore che segue l'andamento giornaliero
c	della temperatura per ogni ora della giornata. L'andamento e` 
c	differente per ogni mese ed e` stato ottenuta dal data base della
c	stazione di S.Pietro Capofiume utilizzando una serie di Fourier
c	troncata alla seconda armonica. Gli algoritmi di calcolo sono
c	dell'IMSL per dati discreti.
c	Da verifiche effettuate la validita` della funzione puo` essere
c	estesa ancora con buoni risultati al resto della Pianura della
c	Regione Emilia Romagna.
c
c	input:
c	
c	IMESE		I*4	mese dell'anno 
C	IORA		I*4	ora del giorno (0/23)
C
C	output:
C
C	CLIMNORM	R*4	valore della funzione che ricostruisce
c				l'andamento giornaliero della temperatura
C
C	IER		i*4	condizione di errore
c				ier=0 tutto O.K.
c				ier=100 errore allocazione unita` per lettura
c				ier=100+iostat  (codice errore apertura file)
c
C-------------------------------------------------------------------------------
	SUBROUTINE CLIMA_TEM_NORM(IMESE,IORA,CLIMNORM,ier)

	include '($FORIOSDEF)'

	PARAMETER N=24,NN=N*12
	DIMENSION COEFF(n,12),SEQ(n),WFFTR(63)
	DATA IUN,IMESEOLD/0,0/
	DATA COEFF/NN*0./

	ier=0

	if (Iun.GT.0) GOTO 50	!legge i termini una sola volta

c				non ha mai letto <cerca unita`>
	call lib$get_lun(Iun)
	  if (Iun.lt.0)THEN
	    IER=100
	    return
	ENDIF

c	aperura del file che contiene i termini della serie di Fourier
c	viene testato se il file risulta locked ritentando l'apertura per
c	dieci volte a distanza di due secondi.

	NUMRIP=0
7	OPEN (UNIT=iun,FILE='megadati$dir:C_FOURIER_16144_TEM.DAT',
	1	STATUS='OLD',FORM='FORMATTED',READONLY,IOSTAT=IOS)

	IF(IOS.EQ.0)go to 9

	IF(IOS.EQ.FOR$IOS_OPEFAI.AND.NUMRIP.LT.10)THEN
		call lib$wait(2.)
		NUMRIP=NUMRIP+1
		go to 7
	ELSE
		ier=ios+100
		iun=0
		return
	END IF

9	  DO I=1,N
	    READ(IUN,1001)(COEFF(I,IMESE1),IMESE1=1,12)
	  ENDDO
1001	  FORMAT(12F5.2)

	CALL FFTRI(N,WFFTR)		!alloca memoria per IMSL

	CLOSE (IUN)
	CALL LIB$FREE_LUN(IUN)		!chiudi e libera unita`

50	CONTINUE

c	se necessario calcola i valori nei nodi
	IF(IMESEOLD.NE.IMESE)CALL F2TRB(N,COEFF(1,IMESE),SEQ,WFFTR)
	IMESEOLD=IMESE		!serve per un'altra volta

	CLIMNORM=SEQ(IORA+1)/FLOAT(N)		! SEQ sono i valori nei nodi*N

	RETURN
	END

C-------------------------------------------------------------------------------
C	CLIMA TEMPERATURA
C-------------------------------------------------------------------------------

	SUBROUTINE CLIMA_TEMP_MAX_MIN
	1	(ALAT,ALON,HH,IMESE,IGIORNO,IORA,VAL
	1	,SIGMAX,SIGMIN,SIGMED,IER)

COMSTART CLIMA_TEMP_MAX_MIN
c	SUBROUTINE CLIMA_TEMP_MAX_MIN
c	1	(ALAT,ALON,HH,IMESE,IGIORNO,IORA,VAL
c	1	,SIGMAX,SIGMIN,SIGMED,IER)
c
c	Restituisce un valore climatico di temperatura valido nella
c	Regione Emilia Romagna per una data latitudine, longitudine e altezza
c	per una specifica data e ora del giorno. E` stata utilizzata la 
c	funzione ricavata nello studio della analisi dellatemperatura max
c	min e med in Emilia Romagna di Carlo Cacciamani (SMR) per ottenere
c	i valori di temperatura max e min medi mensili sul territorio regionale
c	I valori relativi a due mesi sono stati interpolati linearmente sulla
c	data specificata in modo da eliminare discontinuita`. E` stata 
c	utilizzata una serie di Fourier con due armoniche ricavata dai dati
c	orari di S. Pietro Capofiume (anni 85-91) che ricostruisce il ciclo
c	giornaliero per ciascun mese dell'anno per ottenere un valore climatico
c	per ciscuna ora del giorno (Alberoni-Patruno).
c
c	input:
c
c	ALAT		R*4	latitudine in centesimale
c	ALON		R*4	longitudine in centesimale
c	HH		R*4	altezza in metri
c	IMESE		I*4	mese
c	IGIORNO		I*4	giorno 
C	IORA		I*4	ora (00/23) (UTM)
C
C	output:
C
C	VAL		R*4	temperatura climatica in gradi centigradi
c				per il mese, giorno, ora e luogo specificato
C	SIGMAX		R*4	sigma mensile per temperatura max giornaliera
C	SIGMIN		R*4	sigma mensile per temperatura min giornaliera
C	SIGMED		R*4	sigma mensile per temperatura med giornaliera
C
C	IER		i*4	condizione di errore
c				ier=0 tutto O.K.
c
c			errori dovuti alla routine CLIMA_TEM_NORM
c				ier=100 errore allocazione unita` per lettura
c				ier=100+iostat  (codice errore apertura file)
c
c			errori dovuti alla routine CLIMA_TEMP_MAX_MIN
c				ier=200 errore allocazione unita` per lettura
c				ier=200+iostat  (codice errore apertura file)
COMEND

	include '($FORIOSDEF)'

	DIMENSION A2(2,12),B2(2,12),C2(2,12),D2(2,12)

	REAL TREGA(12,3)
C	 TABELLA DI 3*SIGMA PER MAX MIN E MED 
	DATA TREGA/11.5,11.8,11.8,11.8,10.8,11.5,9.9,10.0,11.0,11.0,12.3,11.2,
	1	  12.4,11.0,11.5,9.8,9.1,9.2,9.0,8.4,9.6,9.6,12.0,11.0,
	1	   10.7,9.8,11.2,9.4,8.6,9.3,8.4,8.4,9.2,9.2,11.0,9.9/

C	FUNZIONE PER RICOSTRUIRE LA CLIMATOLOGIA (SOLO FUNZIONE DELLA
C	LATITUDINE LONGITUDINE E ALTEZZA
	FF(SP,PP,QP,TP,XCX1,XCX2,XCX3)=SP+PP*XCX1+QP*XCX2+TP*XCX3

	IER=0

	SIGMAX=TREGA(IMESE,1)/3. !sigma mensile per temperatura max giornaliera
	SIGMIN=TREGA(IMESE,2)/3. !sigma mensile per temperatura min giornaliera
	SIGMED=TREGA(IMESE,3)/3. !sigma mensile per temperatura med giornaliera

c	vedi se e` la prima volta
	if (Iun.GT.0) GOTO 50
c	non ha mai letto
C	legge i coefficienti della funzione climatologica

	call lib$get_lun(Iun)
	  if (Iun.lt.0)THEN
	    IER=200
	    return
	ENDIF

c	aperura del file che contiene i termini della climatologia
c	viene testato se il file risulta locked ritentando l'apertura per
c	dieci volte a distanza di due secondi.

	NUMRIP=0
7	OPEN(UNIT=IUN,FILE='MEGADATI$DIR:'//
	1	'TABELLAMASSIMA.DAT',STATUS='OLD',IOSTAT=IOS,
	1	READONLY)
	IF(IOS.EQ.0)go to 23

	IF(IOS.EQ.FOR$IOS_OPEFAI.AND.NUMRIP.LT.10)THEN
		call lib$wait(2.)
		NUMRIP=NUMRIP+1
		go to 7
	ELSE
		ier=ios+200
		iun=0
		return
	END IF


23	DO I=1,12
	  READ(IUN,131)A2(1,I),B2(1,I),C2(1,I),D2(1,I)
	ENDDO
131	FORMAT(33X,4F10.6)
	CLOSE (UNIT=IUN)

	NUMRIP=0

8	OPEN(UNIT=IUN,FILE='MEGADATI$DIR:'//
	1	'TABELLAMINIMA.DAT',STATUS='OLD',IOSTAT=IOS,
	1	READONLY)

	IF(IOS.EQ.0)go to 24

	IF(IOS.EQ.FOR$IOS_OPEFAI.AND.NUMRIP.LT.10)THEN
		call lib$wait(2.)
		NUMRIP=NUMRIP+1
		go to 8
	ELSE
		ier=ios+200
		iun=0
		return
	END IF


24	DO I=1,12
	  READ(IUN,131)A2(2,I),B2(2,I),C2(2,I),D2(2,I)
D	  TYPE *,A2(2,I),B2(2,I),C2(2,I),D2(2,I),I
	ENDDO
C	OPEN(UNIT=IUN,FILE='MEGADATI$DIR:'//
C	1	'TABELLAMEDIA.DAT',STATUS='OLD',IOSTAT=IOS,
C	1	READONLY,ERR=123)

	CLOSE (UNIT=IUN)
	CALL LIB$FREE_LUN(IUN)	!chiudi e libera unita`

50	CONTINUE		! qui ho gia` letto

c	interpolazione lineare tra due mesi per non avere discontinuita`

	IF(IGIORNO.GT.15)THEN
	  IMESE2=IMESE+1
	  IF(IMESE.EQ.12)IMESE2=1
	  IDAY=IGIORNO-15
	ELSE
	  IMESE2=IMESE-1
	  IF(IMESE.EQ.1)IMESE2=12
	  IDAY=15-IGIORNO
	ENDIF

c	si calcola le temperature massime e minime giornaliere climatiche
c	nei due mesi la dui data data e` a cavallo

	TMAX1=FF(A2(1,IMESE),B2(1,IMESE),C2(1,IMESE)
	1	,D2(1,IMESE),ALON,HH,ALAT)
	TMIN1=FF(A2(2,IMESE),B2(2,IMESE),C2(2,IMESE)
	1	,D2(2,IMESE),ALON,HH,ALAT)
	TMAX2=FF(A2(1,IMESE2),B2(1,IMESE2),C2(1,IMESE2)
	1	,D2(1,IMESE2),ALON,HH,ALAT)
	TMIN2=FF(A2(2,IMESE2),B2(2,IMESE2),C2(2,IMESE2)
	1	,D2(2,IMESE2),ALON,HH,ALAT)

c	interpola linearmente
	TMAX=(TMAX2-TMAX1)/30.*FLOAT(IDAY)+TMAX1
	TMIN=(TMIN2-TMIN1)/30.*FLOAT(IDAY)+TMIN1


C-------------------------------------------------------------------------------
C	CALCOLO CLIMA MEDIO
C-------------------------------------------------------------------------------

c	estraggo il valore tra zero e uno che caratterizza il ciclo giornaliero
	CALL CLIMA_TEM_NORM(IMESE,IORA,CLIMNORM,ier)
	if(ier.ne.0)RETURN

c	mi calcolo la temperatura climatica a quell'ora
	VAL=CLIMNORM*(TMAX-TMIN)+TMIN

	RETURN
	END
