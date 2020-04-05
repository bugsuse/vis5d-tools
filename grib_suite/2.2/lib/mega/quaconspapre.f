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





	SUBROUTINE QUACONSPAPRE (K,IVAL,LAT,LON,HSTAZ,
	1	DISTMAX,DIFHMAX,ISOGLIA,PERC,
	1	IN,IB,FLAG,IER)

comstart QUACONSPAPRE
c
C	SUBROUTINE QUACONSPAPRE (K,IVAL,LAT,LON,HSTAZ,
C	1	DISTMAX,DIFHMAX,ISOGLIA,PERC,
C	1	IN,IB,FLAG,IER)
c
C	Questa subroutine effettua un controllo spaziale dei dati di
c	precipitazione, anche oraria, distribuiti su stazioni che devono 
C	essere il piu` possibili equidistribuite nello spazio. 
C
C	Vengono costruiti con una routine NCAR dei triangoli
C	che uniscono tre stazioni adiacenti in maniera che essi siano il
C	piu` possibile equilateri. Questo fa in maniera che da una stazione
C	partano vari raggi (da un minimo di 3 a n) che rappresentano
C	al tempo stesso i vari orientamenti nello spazio e le stazioni 
C	possibilmente piu` vicine. 
C
C	Vengono quindi estratti tutti i vertici dei triangoli appartenenti 
C	a una stazione, ordinati per poi togliere i valori doppi. 
C	I dati corrispondenti a questi vertici vengono utilizzati 
C	per controllare il dato al centro di questa poligonale.
c
C	Le stazioni appartenenti poligonale vengono utilizzate solo se: 
C
C	a) controllo sull'altezza della stazione
C		-10.< HSTAZ < 4000.
C	dove HSTAZ (espresso in metri) è l'atezza della stazione
C
C	b) la distanza tra la stazione in esame e qualsiasi altra stazione 
C	della poligonale è inferiore a DISTMAX
C
C	c) la differenza in altezza tra la stazione in esame e qualsiasi
C	altra stazione della poligonale è inferiore a DIFHMAX
C
c	d) il dato e` considerato presente dalla funzione j_c_e
c
c	e) la flag associata non ha superato la soglia stabilita nella function vf
c
C===============================================================================
C
C	CONDIZIONE DI ERRORE
C
C	La flag e` incrementata di 1 per la stazione in esame se si
c	verificato entrambe le seguenti condizioni:
C
C	a) la poligonale è composta da almeno 3 stazioni + la stazione in esame
C
C	b) la stazione in esame differisce da tutte le stazioni della 
C	poligonale di piu` di   
C
C		| val1 - val2 | > ISOGLIA+PERC*(MAX(val1,val2))
C
C	dove ISOGLIA e PERC sono valori in input e val1 e` il valore di 
C	precipitazione nella stazione in esame, val2 il dato
C	della stazione in uso della poligonale.
C
c	La flag viene decrementata solo se la stazione in esame appartiene  ad 
c	una poligonale con almeno 3 stazioni.
c	N.B.
C	(l'incremento e` effettuato solo sulla stazione anomala)
C	(stazione con precipitazione = prec maggiore di 0.)
c
C	Il controllo e` piu` scadente sulle stazioni che appartengono
C	al poligono che racchiude tutte le stazioni in quanto il controllo
C	su di esse viene effettuato su un angolo inferiore ai 180 gradi.
C	Esse potrebbero eventualmente essere escluse dal controllo.
C
C===============================================================================
C	INPUT:
C
C	K	       I*4	Numero di stazioni da testare.
C	IVAL(K)	       I*4	valori misurati nelle stazioni.
C	LAT(K)	    REAL*4	Latitudine delle stazioni in centesimali.
C	LON(K)	    REAL*4	Longitudine delle stazioni in centesimali.
C	HSTAZ(K)    REAL*4	Altezze delle stazioni in metri.
C	DISTMAX     REAL*4	Distanza massima tra due stazioni perche`
c				queste possano essere utilizzate all'interno
c				della stessa poligonale di controllo di dati;
C				è espressa in Km.
C	DIFHMAX	    REAL*4	Differenza massima consentita tra le altezze 
C				di due stazioni perchè queste possano essere 
C				utilizzate all'interno della stessa poligonale
C				di controllo dei dati; è espressa in metri.
C	ISOGLIA        I*4	Differenza sempre accettata tra stazioni
C				espressa nella stessa unita` di misura di IVAL
C	PERC	    REAL*4	PERCENTUALE DELLA PRECIPITAZIONE MASSIMA
C				TRA DUE STAZIONI CHE SI STANNO CONFRONTANDO
C				DA SOMMARE A ISOGLIA
C
C===============================================================================
C	OUTPUT :
C
C	IN	I*4		numero di stazioni errate
C	IB	I*4		numero di stazioni buone
C	FLAG(K)	byte		flag associata al dato da testare
C	IER	I*4		condizione di errore della routine
C				0=tutto O.K.  (non utilizzata)
C===============================================================================
c
c	per liberare la virtual memory utilizzata ad ogni chiamata
c	dalla routine bisogna chiamare la routine con k=1.
c			
C===============================================================================
c
c		opzioni:
c
C                   REP  FLAG INDICATING THE USE OF THE SAME DATA, BUT
C                        A NEW EXECUTION.  THE DEFAULT VALUE IS SET TO
C                        OFF.
C
C                        TO TURN ON:    CALL CONOP1(6HREP=ON)
C
C                        TO TURN OFF:   CALL CONOP1(7HREP=OFF)
C
C                        NOTE: IF REP=ON, THE SAME DATA AND TRIANGULA-
C                        TION  ARE  TO  BE  USED BUT IT IS ASSUMED THE
C                        USER HAS CHANGED CONTOUR VALUES OR RESOLUTION
C                        FOR THIS RUN.  SCRATCH ARRAYS WK AND IWK MUST
C                        REMAIN UNCHANGED.C                   TFR  FLAG TO ADVANCE FRAME  BEFORE  TRIANGULATION.
C                        THE DEFAULT VALUE IS TFR=ON.
C
C                        IF PROGRAM SET:   CALL CONOP1(6HTFR=ON)
C
C                        TO TURN OFF:      CALL CONOP1(7HTFR=OFF)
C
C                        NOTE: TRIANGLES ARE PLOTTED  AFTER  THE  CON-
C                        TOURING  IS COMPLETED.  IF THE USER WISHED TO
C                        SEE THE TRIANGLES  OVER  THE  CONTOURS,  TURN
C                        THIS SWITCH OFF.
comend


CC**********************************************************************CC
CC**********************************************************************CC
CC									CC
CC		SERVIZIO METEOROLOGICO REGIONE EMILA ROMAGNA		CC
CC				E.R.S.A.				CC
CC									CC
CC									CC
CC	PAOLO PATRUNO				PIER PAOLO ALBERONI	CC
CC									CC
CC	BOLOGNA 1992							CC
CC**********************************************************************CC
CC**********************************************************************CC
      COMMON /CONRA1/ CL(30)     ,NCL        ,OLDZ       ,PV(210)    ,
     1                FINC       ,HI         ,FLO
      COMMON /CONRA2/ REPEAT     ,EXTRAP     ,PER        ,MESS       ,
     1                ISCALE     ,LOOK       ,PLDVLS     ,GRD        ,
     2                CINC       ,CHILO      ,CON        ,LABON      ,
     3                PMIMX      ,SCALE      ,FRADV      ,EXTRI      ,
     4                BPSIZ      ,LISTOP
      COMMON /CONRA3/ IREC
      COMMON /CONRA4/ NCP        ,NCPSZ
      COMMON /CONRA5/ NIT        ,ITIPV
      COMMON /CONRA6/ XST        ,YST        ,XED        ,YED        ,
     1                STPSZ      ,IGRAD      ,IG         ,XRG        ,
     2                YRG        ,BORD       ,PXST       ,PYST       ,
     3                PXED       ,PYED       ,ITICK
      COMMON /CONRA7/ TITLE      ,ISTRNG(32) ,ICNT       ,ITLSIZ
      COMMON /CONRA8/ IHIGH      ,INMAJ      ,INLAB      ,INDAT      ,
     1                FORM(5)    ,LEN        ,IFMT       ,LEND       ,
     2                IFMTD      ,ISIZEP     ,INMIN
      COMMON /CONRA9/ ICOORD(500),NP         ,MXXY       ,TR         ,
     1                BR         ,TL         ,BL         ,CONV       ,
     2                XN         ,YN         ,ITLL       ,IBLL       ,
     3                ITRL       ,IBRL       ,XC         ,YC         ,
     4                ITLOC(210) ,JX         ,JY         ,ILOC       ,
     5                ISHFCT     ,XO         ,YO         ,IOC        ,NC
      COMMON /CONR10/ NT         ,NL         ,NTNL       ,JWIPT      ,
     1                JWIWL      ,JWIWP      ,JWIPL      ,IPR        ,
     2                ITPV
      COMMON /CONR11/ NREP       ,NCRT       ,ISIZEL     ,NDASH(5)   ,
     1                MINGAP     ,IDASH(5)   ,ISIZEM     ,EDASH(5)   ,
     2                TENS
      COMMON /CONR12/ IXMAX      ,IYMAX      ,XMAX       ,YMAX
       LOGICAL         REPEAT     ,EXTRAP     ,PER        ,MESS       ,
     1                LOOK       ,PLDVLS     ,GRD        ,LABON      ,
     2                PMIMX      ,FRADV      ,EXTRI      ,CINC       ,
     3                TITLE      ,LISTOP     ,CHILO      ,CON

	REAL LAT(K),LON(K),HSTAZ(K)
	INTEGER IVAL(K)
	BYTE FLAG(K)
	data ko/0/

	IER=0
	IF ((K.LE.1.or.k.gt.ko).and.ko.ne.0) THEN
C	  type*,'libero memoria'
	  call lib$free_vm ((6*KO-15)*4,IPT)	!libera virtual memory
	  call lib$free_vm ((6*KO)*4,IPL)
	  call lib$free_vm ((18*KO)*4,IWL)
	  call lib$free_vm (KO*4,IWP)
	  call lib$free_vm (KO*4,IWKK)
	  call lib$free_vm (KO*4,IX)
	  call lib$free_vm (KO*4,IY)
	  ko=0

	  if(k.le.1)RETURN
	END IF
	IF(K.le.2)THEN
	  IER=1
	  RETURN
	ENDIF
	IF (REPEAT) GOTO 30			!verifica se e` una ripetizione
	if (k.le.ko)goto 30			!ho gia` sufficiente memoria
C						!prendo virtual memory
C	type*,'prendo memoria'
	j=lib$get_vm((6*K-15)*4,IPT)
	if (j.ne.1)IER=-80
	j=lib$get_vm((6*K)*4,IPL)
	if (j.ne.1)IER=-80
	j=lib$get_vm((18*K)*4,IWL)
	if (j.ne.1)IER=-80
	j=lib$get_vm(K*4,IWP)
	if (j.ne.1)IER=-80
	j=lib$get_vm(K*4,IWKK)
	if (j.ne.1)IER=-80
	j=lib$get_vm(K*4,IX)
	if (j.ne.1)IER=-80
	j=lib$get_vm(K*4,IY)
	if (j.ne.1)IER=-80
	IF(IER.NE.0)RETURN
	KO=K

C	TYPE *,'INIZIO CONTROLLO DATI'
30	CONTINUE
	CALL QUASPAZprec (K,IVAL,LAT,LON,HSTAZ,
	1	DISTMAX,DIFHMAX,isoglia,perc,
	1	%VAL(IPT),%VAL(IPL),%VAL(IWL),%VAL(IWP),
	1	%VAL(IWKK),IN,IB,FLAG,%VAL(IX),%VAL(IY),IER)

	RETURN
	END

	SUBROUTINE QUASPAZprec (K,IVAL,LAT,LON,HSTAZ,
	1	DISTMAX,DIFHMAX,isoglia,perc,
	1	IPT,IPL,IWL,IWP,
	1	WKK,IN,IB,FLAG,X,Y,IER)

      COMMON /CONRA1/ CL(30)     ,NCL        ,OLDZ       ,PV(210)    ,
     1                FINC       ,HI         ,FLO
      COMMON /CONRA2/ REPEAT     ,EXTRAP     ,PER        ,MESS       ,
     1                ISCALE     ,LOOK       ,PLDVLS     ,GRD        ,
     2                CINC       ,CHILO      ,CON        ,LABON      ,
     3                PMIMX      ,SCALE      ,FRADV      ,EXTRI      ,
     4                BPSIZ      ,LISTOP
      COMMON /CONRA3/ IREC
      COMMON /CONRA4/ NCP        ,NCPSZ
      COMMON /CONRA5/ NIT        ,ITIPV
      COMMON /CONRA6/ XST        ,YST        ,XED        ,YED        ,
     1                STPSZ      ,IGRAD      ,IG         ,XRG        ,
     2                YRG        ,BORD       ,PXST       ,PYST       ,
     3                PXED       ,PYED       ,ITICK
      COMMON /CONRA7/ TITLE      ,ISTRNG(32) ,ICNT       ,ITLSIZ
      COMMON /CONRA8/ IHIGH      ,INMAJ      ,INLAB      ,INDAT      ,
     1                FORM(5)    ,LEN        ,IFMT       ,LEND       ,
     2                IFMTD      ,ISIZEP     ,INMIN
      COMMON /CONRA9/ ICOORD(500),NP         ,MXXY       ,TR         ,
     1                BR         ,TL         ,BL         ,CONV       ,
     2                XN         ,YN         ,ITLL       ,IBLL       ,
     3                ITRL       ,IBRL       ,XC         ,YC         ,
     4                ITLOC(210) ,JX         ,JY         ,ILOC       ,
     5                ISHFCT     ,XO         ,YO         ,IOC        ,NC
      COMMON /CONR10/ NT         ,NL         ,NTNL       ,JWIPT      ,
     1                JWIWL      ,JWIWP      ,JWIPL      ,IPR        ,
     2                ITPV
      COMMON /CONR11/ NREP       ,NCRT       ,ISIZEL     ,NDASH(5)   ,
     1                MINGAP     ,IDASH(5)   ,ISIZEM     ,EDASH(5)   ,
     2                TENS
      COMMON /CONR12/ IXMAX      ,IYMAX      ,XMAX       ,YMAX
       LOGICAL         REPEAT     ,EXTRAP     ,PER        ,MESS       ,
     1                LOOK       ,PLDVLS     ,GRD        ,LABON      ,
     2                PMIMX      ,FRADV      ,EXTRI      ,CINC       ,
     3                TITLE      ,LISTOP     ,CHILO      ,CON

	REAL LAT(K),LON(K),HSTAZ(K),X(K),Y(K)
D	DIMENSION XX(2000),YY(2000),A(2000)
	DIMENSION IPT(6*K-15),IPL(6*K),IWL(18*K),IWP(K),WKK(K),
	1	IVERT(100),IVAL(K)
	BYTE FLAG(K)
	LOGICAL j_c_e,vf

	IB=0
	IN=0
	IF (REPEAT) GOTO 30			!verifica se e` una ripetizione

C	calcola i triangoli (NCAR)
C	K,X,Y numero punti e coordinate punti.
C	NT    numero triangoli costruiti
C	IPT   vettore contente i vertici dei triangoli costruiti
C	NL    numero segmenti del poligono-bordo
C	IPL   vettore contenente i vertici del poligono
C	IWL,IWP,WKK area di lavoro

	DOJ=1,K
	  X(J)=COSD(LON(J))*(90.-LAT(J))/90.
	  Y(J)=SIND(LON(J))*(90.-LAT(J))/90.
C	 TYPE *,J,X(J),Y(J),LON(J),LAT(J)
	ENDDO

	NIT=0

	CALL CONTNG (K,X,Y,NT,IPT,NL,IPL,IWL,IWP,WKK)

30	CONTINUE

D	DO JK=1,K
D		A(JK)=IVAL(JK)/10.
D		CALL SUPCON (LAT(JK),LON(JK),XX(JK),YY(JK))
D	END DO
D
D	IF (LOOK)CALL CONTLK(XX,YY,K,IPT) 	! plotta triangoli
D	IF (PLDVLS)CALL CONPDV(XX,YY,A,K)	! plotta dati in input


C	cicla con tutte le stazioni
	DO ISTAZ=1,K
	  IF(.NOT.J_C_E(IVAL(ISTAZ)).OR..not.vf(FLAG(ISTAZ)))GOTO 557
	  IF (HSTAZ(ISTAZ).LT.-10.OR.HSTAZ(ISTAZ).GT.4000)GOTO 557

C	ITROV e` il numero di triangoli in cui e` presente il dato
	  ITROV=0
C		cicla per tutti i triangoli
		DO IT=1,NT
C			se la stazione considerata e` in prima posizione
C			merorizza gli altri due vertici
			IF(IPT(3*IT-2).EQ.ISTAZ)THEN
			ITROV=ITROV+1
			IVERT(2*ITROV)=IPT(3*IT)
			IVERT(2*ITROV-1)=IPT(3*IT-1)
			GOTO 888
			END IF

C			se la stazione considerata e` in seconda posizione
C			merorizza gli altri due vertici
			IF(IPT(3*IT-1).EQ.ISTAZ)THEN
			ITROV=ITROV+1
			IVERT(2*ITROV)=IPT(3*IT)
			IVERT(2*ITROV-1)=IPT(3*IT-2)
			GOTO 888
			END IF

C			se la stazione considerata e` in terza posizione
C			merorizza gli altri due vertici
			IF(IPT(3*IT).EQ.ISTAZ)THEN
			ITROV=ITROV+1
			IVERT(2*ITROV)=IPT(3*IT-1)
			IVERT(2*ITROV-1)=IPT(3*IT-2)
			GOTO 888
			END IF
888		END DO

C	ITROV ora diviene il numero di vertici nell'intorno
C	della stazione trovati
	  ITROV=ITROV*2

C	TYPE*,'NUMERO VERTICI',ITROV
C	TYPE*,'VERTICI TROVATI = ',IVERT

C	ordina i vettori dei vertici secondo valori decrescenti
	  DO I=1,ITROV-1
	    DO KK=I+1,ITROV
	      IF(IVERT(I).LT.IVERT(KK))THEN
		IC=IVERT(KK)
		IVERT(KK)=IVERT(I)
		IVERT(I)=IC
	      ENDIF
	    END DO
	  END DO

C	TYPE*,'NUMERO VERTICI',ITROV
C	TYPE*,'VERTICI ORDINATI = ',IVERT

C	toglie i valori doppi dal vettore dei vertici
	  IV=1
	  DO KK=2,ITROV
C	TYPE *,IVAL(KK),' DA KK'
	    IF(IVERT(IV).NE.IVERT(KK))THEN
		IV=IV+1
		IVERT(IV)=IVERT(KK)
	    ENDIF		
	  END DO
	  IF (IV.GT.ITROV)IV=ITROV

c	TYPE*,'NUMERO VERTICI',IV
c	TYPE*,'VERTICI PULITI = ',IVERT

C	inizia il controllo sulla stazione 
c	inizia con idati della stazioni in esame al centro della poligonale

c	TYPE*,'STAZIONE  ',ISTAZ
	  STAZ_S=0.
	  STAZ_N=0.
C	MED=IVAL(ISTAZ)
C	SUMQ=IVAL(ISTAZ)**2
	  IF(IVAL(ISTAZ).NE.0)THEN
	    STAZ_S=STAZ_S+1
	  ELSE
	    STAZ_N=STAZ_N+1
	  ENDIF

c	inizia con i dati delle stazioni intorno appartenente alla poligonale
	  IOK=0				! num. staz. concordanti
	  DO I=1, IV
	    IF(.NOT.J_C_E(IVAL(IVERT(I))).OR..not.vf(FLAG(IVERT(I))))
	1	  GOTO 10		!dato mancante
	    IF(HSTAZ(IVERT(I)).LT.-10.OR.HSTAZ(IVERT(I)).GT.4000)GOTO 10
					! anagrafica stazione non idonea
C	distanza tra le due stazioni
	    CALL DISTANZA (LAT(ISTAZ),LON(ISTAZ),
	1	LAT(IVERT(I)),LON(IVERT(I)),DIST)

	    IF (DIST.EQ.0.)THEN
	      TYPE*,'ERRORE VALUTAZIONE STAZIONE= ',ISTAZ
	      TYPE*,'DISTANZA CALCOLATA =0.'
	      GOTO 10
	    ELSE IF (DIST.GT.DISTMAX)THEN
	      GOTO 10			!stazioni troppo distanti
	    ELSE 
	      IDIFH=ABS(HSTAZ(ISTAZ)-HSTAZ(IVERT(I)))
	      IF(IDIFH.GT.DIFHMAX)GOTO 10
	    END IF

	    IF(IVAL(IVERT(I)).NE.0)THEN
	      STAZ_S=STAZ_S+1			!stazione bagnata
	    ELSE
	      STAZ_N=STAZ_N+1			!stazione asciutta
	    ENDIF

C	  MED=MED+IVAL(IVERT(I))		!sommatoria
C	  SUMQ=SUMQ+IVAL(IVERT(I))**2		!sommatoria dei quadrati

	    IDIFF=ABS(IVAL(ISTAZ)-IVAL(IVERT(I)))
	    IM=MAX(IVAL(ISTAZ),IVAL(IVERT(I)))
	    IF((ISOGLIA+PERC*IM).GE.IDIFF)IOK=IOK+1
10	  END DO

	  IF (STAZ_N+STAZ_S.LT.4)	GOTO 557	!poche stazioni

c	  IF(STAZ_N.GE.3.AND.STAZ_S.EQ.1.AND.IVAL(ISTAZ).NE.0)GOTO 556	!una ba.
c	  IF(STAZ_S.GE.3.AND.STAZ_N.EQ.1.AND.IVAL(ISTAZ).EQ.0)GOTO 556	!una as.
c
C	  IF(MED.NE.0)THEN
C	    RMED=FLOAT(MED)/(STAZ_S+STAZ_N)
C	    SIGMA=SQRT((SUMQ-RMED**2*(STAZ_S+STAZ_N))/((STAZ_S+STAZ_N)
C	1	*(STAZ_S+STAZ_N-1)))
C
C	    IF(ABS(IVAL(ISTAZ)-RMED).GT.20.AND.
C	1	ABS(IVAL(ISTAZ)-RMED).GT.
C	1	(2.5+RMED*.01)*SIGMA)GOTO 556
C	  ENDIF
c
C	    IF(STAZ_N.NE.0.AND.STAZ_S.NE.0)GOTO 557 !eclude i cluster non omogei

	  IF(IOK.EQ.0)GOTO 556	!non ha trovato stazioni simpatiche

	  FLAG(ISTAZ)=FLAG(ISTAZ)-1 !qui rimangono solo stazioni corrette
	  IB=IB+1
	  GOTO 557

556	  CONTINUE
C	dato considerato errato
d	TYPE *,'DATO CONSIDERATO ERRATO'
d	TYPE *,'STAZIONE ',ISTAZ,'   VALORE ',IVAL(ISTAZ)
d	TYPE *,'  LAT ',LAT(ISTAZ),'  LONG ',LON(ISTAZ)
D	TYPE *,'MEDIA, SIGMA',RMED,(2.5+RMED*.01)*SIGMA,SIGMA
c	setta la flag del dato errato in vettore flag
	  FLAG(ISTAZ)=FLAG(ISTAZ)+1
	  IN=IN+1

557	END DO
	RETURN
	END
