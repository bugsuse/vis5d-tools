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





	SUBROUTINE selstaz (K,LAT,LON,distmax,IFLAG,IER)

COMSTART SELSTAZ
C	SUBROUTINE SELSTAZ (K,LAT,LON,distmax,IFLAG,IER)
C	
C	SELEZIONE DELLE STAZIONI TROPPO VICINE TRA LORO
C
C	DOCUMENTAZIONE DA FARE
COMEND

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
	INTEGER IFLAG(K)

	IER=0
	IF (K.LE.1) THEN
	  type*,'libero memoria'
	  call lib$free_vm ((6*KO-15)*4,IPT)	!libera virtual memory
	  call lib$free_vm ((6*KO)*4,IPL)
	  call lib$free_vm ((18*KO)*4,IWL)
	  call lib$free_vm (KO*4,IWP)
	  call lib$free_vm (KO*4,IWKK)
	  call lib$free_vm (KO*4,IX)
	  call lib$free_vm (KO*4,IY)

	  RETURN
	END IF
	IF(K.EQ.2)THEN
	  IER=1
	  RETURN
	ENDIF
	IF (REPEAT) GOTO 30			!verifica se e` una ripetizione
C						!prendo virtual memory
	type*,'prendo memoria'
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

30	CONTINUE
	CALL selsta (K,LAT,LON,distmax,
	1	%VAL(IPT),%VAL(IPL),%VAL(IWL),%VAL(IWP),
	1	%VAL(IWKK),IFLAG,%VAL(IX),%VAL(IY),IER)

	RETURN
	END

	SUBROUTINE selsta (K,LAT,LON,distmax,
	1	IPT,IPL,IWL,IWP,
	1	WKK,IFLAG,X,Y,IER)

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

	REAL LAT(K),LON(K),X(K),Y(K)
	DIMENSION IPT(6*K-15),IPL(6*K),IWL(18*K),IWP(K),WKK(K),
	1	IVERT(100),IFLAG(K)
	LOGICAL j_c_e

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
D	  type *,J,x(j),Y(J),LON(J),LAT(J)
	ENDDO
   
	NIT=0

	CALL CONTNG (K,X,Y,NT,IPT,NL,IPL,IWL,IWP,WKK)

30	CONTINUE

C	cicla con tutte le stazioni
	DO ISTAZ=1,K
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
	IF(IVERT(IV).NE.IVERT(KK))THEN
		IV=IV+1
		IVERT(IV)=IVERT(KK)
	ENDIF		
	END DO
	IF (IV.GT.ITROV)IV=ITROV

C	TYPE*,'NUMERO VERTICI',IV
C	TYPE*,'VERTICI PULITI = ',IVERT

C	iniazia il controllo sulla stazione testando i gradienti
C	TYPE*,'STAZIONE  ',ISTAZ
	GRAD=0
	IF=0
	DO I=1, IV
C	distanza tra le due stazioni
	CALL DISTANZA (LAT(ISTAZ),LON(ISTAZ),
	1	LAT(IVERT(I)),LON(IVERT(I)),DIST)
	IF (DIST.EQ.0.)THEN
		TYPE*,'ERRORE VALUTAZIONE STAZIONE= ',ISTAZ
		TYPE*,'DISTANZA CALCOLATA =0.'
		IF=1
	END IF
C	distmax la stazione e` forse da eliminare ??
	IF(DIST.LT.distmax)IF=1	
	GRAD=GRAD+DIST
556	END DO

	DMED=GRAD/FLOAT(IV)
	IF (IF.GT.0)THEN
C	scrive dato errato in vettore dati errati
	  IFLAG(ISTAZ)=DMED*-1.
	ELSE
	  IFLAG(ISTAZ)=DMED
	END IF

557	END DO

C	cicla con tutte le stazioni
	DO ISTAZ=1,K
	if(iflag(istaz).gt.0)goto 559

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
			GOTO 889
			END IF

C			se la stazione considerata e` in seconda posizione
C			merorizza gli altri due vertici
			IF(IPT(3*IT-1).EQ.ISTAZ)THEN
			ITROV=ITROV+1
			IVERT(2*ITROV)=IPT(3*IT)
			IVERT(2*ITROV-1)=IPT(3*IT-2)
			GOTO 889
			END IF

C			se la stazione considerata e` in terza posizione
C			merorizza gli altri due vertici
			IF(IPT(3*IT).EQ.ISTAZ)THEN
			ITROV=ITROV+1
			IVERT(2*ITROV)=IPT(3*IT-1)
			IVERT(2*ITROV-1)=IPT(3*IT-2)
			GOTO 889
			END IF
889		END DO

C	ITROV ora diviene il numero di vertici nell'intorno
C	della stazione trovati
	ITROV=ITROV*2

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

C	toglie i valori doppi dal vettore dei vertici
	IV=1
	DO KK=2,ITROV
	IF(IVERT(IV).NE.IVERT(KK))THEN
		IV=IV+1
		IVERT(IV)=IVERT(KK)
	ENDIF		
	END DO
	IF (IV.GT.ITROV)IV=ITROV

C	iniazia il controllo sulla stazione testando i gradienti
C	TYPE*,'STAZIONE  ',ISTAZ
	DO I=1, IV
C	distanza tra le due stazioni
	CALL DISTANZA (LAT(ISTAZ),LON(ISTAZ),
	1	LAT(IVERT(I)),LON(IVERT(I)),DIST)
C	distmax la stazione e` forse da eliminare ??
	IF(DIST.LT.distmax.and.abs(iflag(istaz)).
	1	le.abs(iflag(ivert(i)))) iflag(istaz)=0

558	END DO
559	END DO

	RETURN
	END
