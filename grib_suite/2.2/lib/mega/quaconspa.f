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






	SUBROUTINE QUACONSPA (K,IVAL,LAT,LON,ISOGLIA,GRADMAX,
	1	IN,IB,FLAG,IER)

comstart QUACONSPA
C	SUBROUTINE QUACONSPA (K,IVAL,LAT,LON,ISOGLIA,GRADMAX,
C	1	IN,IB,FLAG,IER)
c
C	Questa subroutine effettua un controllo spaziale dei dati distribuiti
C	su stazioni che devono essere il piu` possibili equidistribuite
C	nello spazio. Vengono costruiti con una routine NCAR dei triangoli
C	che uniscono tre stazioni adiacenti in maniera che essi siano il
C	piu` possibile equilateri. Questo fa in maniera che da una stazione
C	partano vari raggi (da un minimo di 2 a n) che rappresentano
C	al tempo stesso i vari orientamenti nello spazio e le stazioni 
C	possibilmente piu` vicine. Vengono quindi estratti tutti i vertici
C	dei triangoli appartenenti a una stazione, ordinati per poi
C	togliere i valori doppi e considerati per calcolare i vari gradienti
C	nelle varie direzioni. SOLO SE TUTTI QUESTI GRADIENTI VERIFICANO
C	LA CONDIZIONE DI ERRORE E SONO TUTTI DELLO STESSO SEGNO IL DATO VERRA`
C	CONSIDERATO ERRATO
C
C	Il controllo e` piu` scadente sulle stazioni che appartengono
C	al poligono che racchiude tutte le stazioni in quanto il controllo
C	su di esse viene effettuato su un angolo inferiore ai 180 gradi.
C
C	Esse potrebbero eventualmente essere escluse dal controllo, opzione 
C	attualmente non in uso.
C
C	Le coordinate delle stazioni vengono calcolate partendo dalle 
C	coordinate geografiche espresse in gradi centesimali tramite le 
C	funzioni sotto riportate:
C
C	   X(J)=COSD(LON(J))*(90.-LAT(J))/90.
C	   Y(J)=SIND(LON(J))*(90.-LAT(J))/90.
C
C	Queste funzioni lavorano ragionevolmente bene solo nell`emisfero nord .
C	L'origine delle coordinate e` il polo l'asse X e` il meridiano di 
C	Greenwich mentre l'asse Y e` il meridiano a 90 E.
C
C===============================================================================
C
C	CONDIZIONE DI ERRORE
C
C	Il dato della stazione in esame e` considerato errato se il vettore dei 
C	dati contiene almeno tre stazioni con dati presenti e se sono 
C	verificate entrambe le condizioni:
C	
C	a) Tutte le stazioni attorno alla stazione in esame hanno, con essa, 
C	un gradiente superiore a 
C
C		ISOGLIA/DIST + GRADMAX
C
C	dove ISOGLIA e GRADMAX sono parametri passati in input e DIST e` la 
C	distanza sulla terra tra la stazione in esame e la stazione sulla 
C	poligonale che viene utilizzata.
C
C	b) I gradienti sono tutti dello stesso segno.
C	La stazione e` un massimo o un minimo.
C
C	N.B.
C	Se una stazione risulta circondata da dati mancanti nessun controllo 
C	viene fatto su di essa e la flag rimane inalterata.
c
c	I dati mancanti sono definiti dalla function J_C_E per i dati o
c	dalla flag associata con la function VF (vedi:
c	COMMON /FLAGSOGLIA/IATT	
c	DATA IATT/3/				)
c
c 23/9/1998
c
c modificato l'algoritmo:
c e' stata definita una soglia per la distanza, oltre la quale le stazioni
c vengono considerate scorrelate e la coppia di stazioni non viene 
c considerata per il controllo, al pari di un dato mancante.
c
c il parametro di soglia espresso in Km. viene definito tramite un common
c per mantenere la comptibilita' con le versioni precedenti.
c
c	COMMON/QUACONSPA/DISCOL
c
c DISCOL assume valore di default=100
c
c
C===============================================================================
C	INPUT
C
C	K		I*4	Numero di stazioni da testare.
C	IVAL(K)	     	I*4	Valori misurati nelle stazioni.
C	LAT(K)	     Real*4	Lat. delle stazioni in centesimali.
C	LON(K)	     Real*4	Lon. delle stazioni in centesimali.
C	ISOGLIA		I*4	Differenza orrizzontale minima tra le stazioni,
C				espressa in unita` di misura di IVAL;
C				vedi le condizioni di errore per ulteriori 
C				chiarimenti.
C	GRADMAX      Real*4	Gradiente massimo orrizontale della grandezza 
C				considerata, espresso in unita` di misura di 
C				IVAL/Km.
C
C===============================================================================
C	OUTPUT
C
C	IN		I*4	Numero di stazioni errate.
C	IB		I*4	Numero di stazioni buone.
C	FLAG(K)	       BYTE	Vettore di flag associate ai dati da testare.
C	IER		I*4	condizione di errore della routine:
c				-80 errore allocazione virtual memory
c				-3 due o piu` stazioni con le stesse coordinate
c				   la coppia di stazioni non viene esaminata
c				-2 solo due stazioni, non si puo` triangolare
C				0  Nessuna errore dalla routine, tutto OK.
c				
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
C
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
c
CC                   REP  FLAG INDICATING THE USE OF THE SAME DATA, BUT
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
C                        REMAIN UNCHANGED.
C                   TFR  FLAG TO ADVANCE FRAME  BEFORE  TRIANGULATION.
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

	REAL LAT(K),LON(K)
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

	  if (k.le.1)RETURN
	END IF
	IF(K.le.2)THEN
	  IER=-2
	  RETURN
	ENDIF
	IF (REPEAT) GOTO 30			!verifica se e` una ripetizione
	if(k.le.ko) goto 30			!ho gia` abbastanza memoria
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

	CALL QUASPA (K,IVAL,LAT,LON,ISOGLIA,GRADMAX,
	1	%VAL(IPT),%VAL(IPL),%VAL(IWL),%VAL(IWP),
	1	%VAL(IWKK),IN,IB,FLAG,%VAL(IX),%VAL(IY),IER)

	RETURN
	END

	SUBROUTINE QUASPA (K,IVAL,LAT,LON,ISOGLIA,GRADMAX,
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

C	DIMENSION XX(2000),YY(2000),A(2000)
	REAL LAT(K),LON(K),X(K),Y(K)
	DIMENSION IPT(6*K-15),IPL(6*K),IWL(18*K),IWP(K),WKK(K),
	1	IVERT(100),IVAL(K)
	BYTE FLAG(K)
	LOGICAL j_c_e,vf

C 23/9/1998
	COMMON/QUACONSPA/DISCOL
	DATA DISCOL/100./

	IER=0
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

	DO J=1,K
	   X(J)=COSD(LON(J))*(90.-LAT(J))/90.
	   Y(J)=SIND(LON(J))*(90.-LAT(J))/90.
C	 TYPE *,J,X(J),Y(J),LON(J),LAT(J)
	ENDDO

	NIT=0

	CALL CONTNG (K,X,Y,NT,IPT,NL,IPL,IWL,IWP,WKK)

30	CONTINUE

C	DO JK=1,K
C		A(JK)=IVAL(JK)
C		CALL SUPCON (LAT(JK),LON(JK),XX(JK),YY(JK))
C	END DO

C	IF (LOOK)CALL CONTLK(XX,YY,K,IPT) 	! plotta triangoli
C	IF (PLDVLS)CALL CONPDV(XX,YY,A,K)	! plotta dati in input

C	cicla con tutte le stazioni
	DO ISTAZ=1,K
	  IF(.NOT.J_C_E(IVAL(ISTAZ)).OR..not.vf(FLAG(ISTAZ)))GOTO 557
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
888	  END DO

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
	      IV=IV+1			!	solo se e` presente il dato
	      IVERT(IV)=IVERT(KK)
	    ENDIF		
	  END DO
	  IF (IV.GT.ITROV)IV=ITROV

C	TYPE*,'NUMERO VERTICI puliti',IV
c	TYPE*,'VERTICI PULITI = ',IVERT

C	inizia il controllo sulla stazione testando i gradienti
C	TYPE*,'STAZIONE  ',ISTAZ
	  ISEGNO=0
	  IVB=0
	  DO I=1, IV
C	TYPE *,IVAL(ivert(I)),' DA I'
	  IF(.NOT.J_C_E(IVAL(ivert(I))).OR..not.vf(FLAG(IVERT(I))))GOTO 556
C	distanza tra le due stazioni
	    CALL DISTANZA (LAT(ISTAZ),LON(ISTAZ),
	1	LAT(IVERT(I)),LON(IVERT(I)),DIST)
	    IF (DIST.EQ.0.)THEN
	      IER=-3
C	      TYPE*,'ERRORE VALUTAZIONE STAZIONE= ',ISTAZ
C	      TYPE*,'DISTANZA CALCOLATA =0.'
	      GOTO 556
	    END IF
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C	    modifica 23/9/1998
c           se la distanza supera xx, stazioni scorrelate - salta -

	    IF (DIST.GT.DISCOL)GOTO 556
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	    IVB=IVB+1
C	differenza tra i dati delle due stazioni
	    DELTA=FLOAT(IVAL(IVERT(I)))-FLOAT(IVAL(ISTAZ))
C	valore del gradiente nella direzione delle due stazioni
	    GRAD=DELTA/DIST
	    GRADSOGLIA=FLOAT(ISOGLIA)/DIST
C	se il gradiente e` piu` piccolo di gradmax la stazione e`
C	considerata buona
	    IF(ABS(GRAD).LT.GRADSOGLIA+GRADMAX)GOTO 555	!cerca dato 
							!vicino coerente
							!esco dal ciclo

C	se il gradiente cattivo e` negativo decrementa il contatore di segni
	    ISEGNO=ISEGNO-1
C	se il gradiente cattivo e` positivo incrementa il contatore di segni
	    IF (GRAD.GT.0.)ISEGNO=ISEGNO+2
556	  END DO

	IF(IVB.EQ.0) GOTO 557		!NESSUN CONTROLLO

C	se tutti i gradienti cattivi sono dello stesso segno dato errato
	  IF (ABS(ISEGNO).EQ.IVB)THEN
	    FLAG(ISTAZ)=FLAG(ISTAZ)+1
	    IN=IN+1
C	dato considerato errato
C	TYPE *,'DATO CONSIDERATO ERRATO'
C	TYPE *,'STAZIONE ',ISTAZ,'   VALORE ',IVAL(ISTAZ)
C	TYPE *,'  LAT ',LAT(ISTAZ),'  LONG ',LON(ISTAZ)
C	scrive dato errato in vettore dati errati
	  ELSE
555	    FLAG(ISTAZ)=FLAG(ISTAZ)-1
	    IB=IB+1
	  END IF

557	END DO
	RETURN
	END
