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







	SUBROUTINE TABSPL(DATI,N,N1,N2,MX,MN,MD,IN,IER)

COMEND TABSPL
C	SUBROUTINE TABSPL(DATI,N,N1,N2,MX,MN,MD,IN,IER)
C
C	Questa ruotine valuta il valore massimo, minimo, medio di un insieme
C	di dati utilizzando un interpolazione "spline" dei dati.
C	Fa un controllo sui dati in maniera temporale controllando
C	che il dato considerato non sia un massimo o un minimo e che
C	contemporaneamente non differisca dai valori intorno piu` di
C	0.1 gradi C per minuto.
C	L'indice del vettore dati e` l'asse dei tempi che vengono 
C	ricalcolati dalla funzione IMINUTI.
C	
C	DATI(N)	: 	I*2	VETTORE CONTENENTE TUTTI I DATI
C	N1	:	I*4	INDICE DELL' ELEMENTO DA CUI SI INIZIA 
C				L'INTERPOLAZIONE
C	N2	:	I*4	INDICE DELL' ELEMENTO CON CUI SI FINISCE
C				L'INTERPOLAZIONE
C	MX	:	REAL	VALORE MASSIMO STIMATO
C	MN	:	REAL	VALORE MINIMO STIMATO
C	MD	:	REAL	VALORE MEDIO STIMATO
C	IN	:	I*4	NUMERI DI DATI BUONI TROVATI
C	IER	:	I*4	INDICATORE DI ERRORE
C
C	IER =  0    : 	TUTTO O.K.
C	IER =  1    :   L'indice N2 e' > di N o e' > di 100	
C	IER =  2    :	Nessun dato buono nell' intervallo N1-N2 oppure
C			fra N1 e N2 esiste un buco fra i dati superiore a 2 ore
C	IER =  3    :   Ci sono piu` di tre dati considerati errati
COMEND

	PARAMETER NMAX=100	!numero massimo di dati in ingresso
	INTEGER*2 DATI(N)
	REAL X(NMAX),Y(NMAX),S(NMAX),MATSPL(NMAX,4),CA(NMAX),
	1	CB(NMAX),CC(NMAX),CD(NMAX),MX,MN,MD,OMX,OMN
c	CHARACTER*130 MESSAGE		!messaggio file.log
c	COMMON /MESSAGE/MESSAGE

C  Test che controlla la dimensione del vettore con l'intervallo da interpolare
	IER=1
	IF(N2.GT.N.OR.N.GT.NMAX) RETURN

C	Riempo i vettori da interpolare X e Y scartando i valori superiori a 
C	3000 ,verificando che l'intervallo tra loro non sia superiore a 2 h.	
C	Faccio un controllo sui dati in maniera temporale controllando
C	che il dato considerato non sia un massimo o un minimo e che
C	contemporaneamente non differisca dai valori intorno piu` di
C	0.1 gradi C per minuto.

	IN=0		! NUMERO DEI DATI BUONI 
	IB=0		! NUMERO DEI DATI ERRATI
	IER=2		! condizione di errore in caso di intervallo >2h
	XX=IMINUTI(N1)	! inizializza i minuti iniziali

	DO I=N1,N2
	IF(ABS(DATI(I)).GT.3000)THEN
C		se il dato e` assente controlla l'intervallo col dato precedente
		IF (IN.GT.0)XX=X(IN)
		IF ((IMINUTI(I)-XX).GT.120)THEN
		  IN=-1
		  RETURN
		END IF
	ELSE
C	se il dato e` presente
	Y(IN+1)=DATI(I)
	X(IN+1)=IMINUTI(I)
	 IF (IN.GT.1)THEN
C	  controlla i gradienti
	  GRAD1=(Y(IN)-Y(IN-1))/(X(IN)-X(IN-1))
	  GRAD2=(Y(IN)-Y(IN+1))/(X(IN)-X(IN+1))
C	  se sono entrambi superiori a 0.1 C/minuto
	  IF (ABS(GRAD1).GT.1.AND.ABS(GRAD2).GT.1)THEN
C	   se sono di senso opposto
	   IF ((GRAD1*GRAD2).LT.0.)THEN
C	    incrementa contatore errori e ricopre il dato errato
	    TYPE *,'TROVATO DATO ERRATO temp=',Y(IN)/10.,'  ore',X(IN)/60.

C	scrittura file.log
c
c	CALL WRITEMESSAGE ('MEGA$DIR:TABULO.LOG',MESSAGE)
c
	    IB=IB+1
	    Y(IN)=DATI(I)
	    X(IN)=IMINUTI(I)
	    GOTO 123
	   END IF
	  END IF
	 END IF
C	 se il dato e` considerato buono
	 IN=IN+1

123	END IF
	END DO

C	se ci sono piu` di tre dati considerati errati 
C	termina in condizione di errore
	IER=3
	IF(IB.GE.3) RETURN

C	Calcolo spline
	CALL CUBSPL(X,Y,S,IN,1,MATSPL)
	CALL CUBCOEFF(X,Y,IN,S,CA,CB,CC,CD)

C	inizializzo i valori max e min
	MX=-500.
	MN=500.
	OMX=0.
	OMN=0.
	MD=0.

C	ricalcolo i valori a intervalli di 5 minuti
	DO K=X(1),X(IN),5
	XVAL=FLOAT(K)
	CALL CUBVAL(CA,CB,CC,CD,X,IN,XVAL,YVAL,DVAL,DDVAL,IER)

C	Cerco il massimo e il minimo dei dati stimati
C	il valore massimo e' contenuto in 	:	MX
C	il valore minimo e' contenuto in 	:	MN
C	l'ora del valore massimo e' contenuta in:	OMX
C	l'ora del valore minimo e' contenuta in :	OMN
C	tutte queste grandezze sono REAL*4

	IF(YVAL.GT.MX)THEN
	MX=YVAL
	OMX=XVAL
	END IF

	IF(YVAL.LT.MN)THEN
	MN=YVAL
	OMN=XVAL
	END IF

	MD=MD+YVAL	
	END DO

C	Calcolo la media stimata
	MD=(MD/((X(IN)-X(1))/5.))/10.
	MX=MX/10.
	MN=MN/10.

	IER=0
	RETURN
	END

