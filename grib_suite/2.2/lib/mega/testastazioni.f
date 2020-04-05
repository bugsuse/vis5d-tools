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





	SUBROUTINE TESTASTAZIONI (K,X,Y,VAL,LAT,LON,GRADMAX)

comstart TESTASTAZIONI
c	SUBROUTINE TESTASTAZIONI (K,X,Y,VAL,LAT,LON,GRADMAX)
c
C	questa subroutine effettua un controllo spaziale dei dati distribuiti
C	su stazioni che devono essere il piu` possibili equidistribuite
C	nello spazio. Vengono costruiti con un a routine NCAR dei triangoli
C	che uniscono tre stazioni adiacenti in maniera che essi siano il
C	piu` possibile equilateri. Qesto fa in maniera che da una stazione
C	partano vari raggi (da un minimo di 4 a n) che rappresentano
C	al tempo stesso i vari orientamenti nello spazio e le stazioni 
C	possibilmente piu` vicine. Vengono quindi estratti tutti i vertici
C	dei triangoli appartenenti a una stazione, ordinati per poi
C	togliere i valori doppi e considerati per calcolare i vari gradienti
C	nelle varie direzioni. SOLO SE TUTTI QUESTI GRADIENTI RISULTERANNO
C	SUPERIORI AL GRADMAX E TUTTI DELLO STESSO SEGNO IL DATO VERRA`
C	CONSIDERATO ERRATO
C	( e tolto dai vettori dei dati e delle coordinate NCAR).
C	Il controllo e` piu` scadente sulle stazioni che appartengono
C	al poligono che racchiude tutte le stazioni in quanto il controllo
C	su di esse viene effettuato su un angolo inferiore ai 180 gradi.
C	Essse potrebbero eventualmente essere escluse dal controllo.
C	Le coordinate x,y devo essre calcolate dopo avere  chiamato
C	una supmap appropriata in modo da identificare la superficie terrestre
C	il piu` possibile con un piano (proiezione conforme di Lambert
C	per le nostre latitudini) chiamando poi la supcon.

C	PARAMETRI IN INGRESSO
C
C	K	INTEGER		numero di stazioni da testare
C	X	REAL(K)		coordinate NCAR lungo x
C	Y	REAL(K)		coordinate NCAR lungo y
C	VAL	REAL(K)		valori misurati nelle stazioni
C	LAT	REAL(K)		latitudine delle stazioni in centesimali
C	LON	REAL(K)		longitudine delle stazioni in centesimali
C	GRADMAX REAL		variazione considerata massima/Km della
C				grandezza considerata
C
C	PARAMETRI IN USCITA
C
C	K	INTEGER		numero di stazioni buone
C	X	REAL(K)		coordinate NCAR lungo x delle staz. buone
C	Y	REAL(K)		coordinate NCAR lungo y delle staz. buone
C	VAL	REAL(K)		valori misurati nelle stazioni buone
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

	PARAMETER N=500		!numero stazioni massimo
	REAL VAL(K),LAT(K),LON(K),X(K),Y(K)
	DIMENSION IPT(6*N-15),IPL(6*N),IWL(18*N),IWP(N),WKK(N),
	1	IVERT(100),ISERR(N)

	TYPE *,'INIZIO CONTROLLO DATI'

C	calcola i triangoli (NCAR)
C	K,X,Y numero punti e coordinate punti.
C	NT    numero triangoli costruiti
C	IPT   vettore contente i vertici dei triangoli costruiti
C	NL    numero segmenti del poligono-bordo
C	IPL   vettore contenente i vertici del poligono
C	IWL,IWP,WKK area di lavoro

	CALL CONTNG (K,X,Y,NT,IPT,NL,IPL,IWL,IWP,WKK)

C	INERR e` il numero di stazioni errate trovate
	INERR=0

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
	ISEGNO=0
	DO I=1,IV
C	distanza tra le due stazioni
	CALL DISTANZA (LAT(ISTAZ),LON(ISTAZ),
	1	LAT(IVERT(I)),LON(IVERT(I)),DIST)
	IF (DIST.EQ.0.)THEN
		TYPE*,'ERRORE VALUTAZIONE STAZIONE= ',ISTAZ
		TYPE*,'DISTANZA CALCOLATA =0.'
		GOTO 555
	END IF
C	differenza tra i dati delle due stazioni
	DELTA=VAL(IVERT(I))-VAL(ISTAZ)
C	valore del gradiente nella direzione delle due stazioni
	GRAD=DELTA/DIST
C	se il gradiente e` piu` piccolo di gradmax la stazione e`
C	considerata buona
	IF(ABS(GRAD).LT.GRADMAX)GOTO 555	!cerca dato vicino coerente
C	se il gradiente cattivo e` negativo decrementa il contatore di segni
	ISEGNO=ISEGNO-1
C	se il gradiente cattivo e` positivo incrementa il contatore di segni
	IF (GRAD.GT.0.)ISEGNO=ISEGNO+2
	END DO
C	se tutti i gradienti cattivi sono dello stesso segno dato errato
	IF (ABS(ISEGNO).EQ.IV)THEN
C	dato considerato errato
	TYPE *,'DATO CONSIDERATO ERRATO'
	TYPE *,'STAZIONE ',ISTAZ,'   VALORE ',VAL(ISTAZ)
	TYPE *,'  LAT ',LAT(ISTAZ),'  LONG ',LON(ISTAZ)
C	incrementa contatore dati errati
	INERR=INERR+1
C	scrive dato errato in vettore dati errati
	ISERR(INERR)=ISTAZ
	END IF

555	END DO

C	*******************************************************************
C	Questa parte si puo` escludere ed emettere dalla subroutine
C	i parametri INERR (numero stazioni errate) e ISERR (stazioni errate
C	dimensionato a N (paramiter nelle dichiarative))

C	toglie i valori errati
	DO I=1,INERR
		VAL(ISERR(I))=VAL(K)
		X(ISERR(I))=X(K)
		Y(ISERR(I))=Y(K)
		K=K-1
	END DO
C	*******************************************************************

	TYPE *,'FINE CONTROLLO'
	TYPE *,'TROVATE ',INERR,' STAZIONI ERRATE'
	type *,'TROVATE ',K,' STAZIONI BUONE'

5000	RETURN
	END

	SUBROUTINE DISTANZA(RLAT1,RLONG1,RLAT2,RLONG2,DIST)
COMSTART DISTANZA
C	SUBROUTINE DISTANZA(RLAT1,RLONG1,RLAT2,RLONG2,DIST)
C
C	********************************************************
C	CALCOLA LE DISTANZE 
C	TRA DUE PUNTI DI LATITUDINE E LONGITUDINE CONUSCIUTA
C	N.B.  LA DISTANZA E` IN Km.
C	      Valida solo per distanze piccole in quanto
C	      approssima la corda con l'arco della circonferenza
C	********************************************************
C	parametri in ingresso
C	
C	RLAT1		REAL	latitudine  del punto 1  centesimale
C	RLONG1		REAL	longitudine del punto 1  centesimale
C	RLAT2		REAL	latitudine  del punto 2  centesimale
C	RLONG2		REAL	longitudine del punto 2  centesimale
C
C	parametri in uscita
C
C	DIST		REAL	distanza tra i punti 1 e 2.
C	********************************************************
COMEND

	DATA RAMED/6371.229315/		!raggio medio della terra in Km.
	DATA RPAI/3.1415927/		!PI GRECO

	RLAT=RLAT1-RLAT2		!differenza di latitudine
	RLONG=RLONG1-RLONG2		!differenza di longitudine

	PLAT=(RLAT*RPAI)/180.		!trasforma l'angolo in radianti
	Y=PLAT*RAMED			!distanza N-S tra i punti

C	raggio della circonferenza alla latitudine media tra i due punti
	RAD1=RAMED*COS(((RLAT1+RLAT2)/2.)*RPAI/180.)
	X=(RPAI*RLONG*RAD1)/180.	!distanza E-W tra i punti

	DIST=(X**2+Y**2)**0.5		!distanza tra i punti

C	TYPE *,'X=',X,' Y=',Y,'  DISTANZA= ',DIST

	RETURN
	END
