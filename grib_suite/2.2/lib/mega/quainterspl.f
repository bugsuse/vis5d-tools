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






	SUBROUTINE QUAINTERSPL (IMINUTI,DATI,N1,N2,
	1	IN,RMX,RMN,RMD,OMX,OMN,IER)

COMSTART QUAINTERSPL
C	SUBROUTINE QUAINTERSPL (IMINUTI,DATI,N1,N2,
C	1	IN,RMX,RMN,RMD,OMX,OMN,IER)
C
C	Fa un controllo sui dati in maniera temporale controllando
C	che non ci siano intervalli tra i dati superiori a INTMAX.
C	Valuta il valore massimo minimo e medio stimandoli tramiteuna spline.
C	L'asse dei tempi che viene espresso in minuti e` comtenuto in IMINUTI
C	
C	IN:
C	IMINUTI(N2) :	I*4	CONTIENE LA COORDIMATA TEMPO IN MINUTI
C	DATI(N2): 	I*4	VETTORE CONTENENTE TUTTI I DATI
C	N1	:	I*4	INDICE DELL' ELEMENTO DA CUI SI INIZIA 
C				L'INTERPOLAZIONE
C	N2	:	I*4	INDICE DELL' ELEMENTO CON CUI SI FINISCE
C				L'INTERPOLAZIONE
C	OUT:
C	IN	:	I*4	NUMERI DI DATI BUONI TROVATI
C	RMX	:	REAL	VALORE MASSIMO STIMATO
C	RMN	:	REAL	VALORE MINIMO STIMATO
C	RMD	:	REAL	VALORE MEDIO STIMATO
C	OMX	:	I*4	MINUTO IN CUI SI E` VERIFICATA LA MASSIMA
C	OMN	:	I*4	MINUTO IN CUI SI E` VERIFICATA LA MINIMA
C	IER	:	I*4	INDICATORE DI ERRORE
C
C	IER =  0    : 	TUTTO O.K.
C	IER =  1    :   L'indice N1 e' > di N2 o e' > di 100	
C	IER =  2    :	Nessun dato buono nell' intervallo N1-N2 oppure
C			fra N1 e N2 esiste un buco fra i dati superiore a 2 ore
COMEND

	PARAMETER NMAX=100	!numero massimo di dati in ingresso
	INTEGER*4 IMINUTI(N2),DATI(N2),OMX,OMN
	REAL X(NMAX),Y(NMAX)

	COMMON /QUAINTERSPL/INTMAX,GRADMAX,MINSTEP

C  Test che controlla la dimensione del vettore con l'intervallo da interpolare
	IER=1
	IF(N1.GT.N2.OR.N2.GT.NMAX) RETURN

C	inizializzazione variabile
	INTMAX=120
c	per variare i valori di default
	CALL MIEOPZIONI

C	Riempo i vettori da interpolare X e Y scartando i valori superiori a 
C	3000 ,verificando che l'intervallo tra loro non sia superiore a 2 h.	

	IER=2		! condizione di errore in caso di intervallo >XMINUTI
	IX0=IMINUTI(N1) ! inizializza i minuti iniziali assoluti
	IXX=IMINUTI(N1)-IX0	! inizializza i minuti iniziali relativi

	IN=0

	DO I=N1,N2
	 IF(ABS(DATI(I)).LT.3000)THEN
C	   se il dato e` presente
	   IN=IN+1
	   Y(IN)=DATI(I)
	   X(IN)=FLOAT(IMINUTI(I)-IX0)
	 END IF
C	  controlla l'intervallo col dato precedente
	  IF (IN.GT.0)IXX=NINT(X(IN))
	  IF ((IMINUTI(I)-IX0-IXX).GT.INTMAX)THEN
	    IN=-1
	    RETURN
	  END IF
	END DO

	call interspl(x,y,IN,RMX,RMN,RMD,OMX,OMN)

	OMX=OMX+IX0
	OMN=OMN+IX0

	IER=0
	return
	end


	SUBROUTINE interspl(x,y,IN,RMX,RMN,RMD,OMX,OMN)

	PARAMETER NMAX=100	!numero massimo di dati in ingresso
	INTEGER*4 OMX,OMN
	REAL X(NMAX),Y(NMAX),RMX,RMN,RMD
	DIMENSION BREAK(2*NMAX),CSCOEF(4,2*NMAX)	!per spline imsl
	COMMON /QUAINTERSPL/INTMAX,GRADMAX,MINSTEP

C	Questa ruotine valuta il valore massimo, minimo, medio di un insieme
C	di dati utilizzando un interpolazione "spline" dei dati.

C	IN:
C	X(IN)	:	REAL	ASSE DEI TEMPI
C	Y(IN)	:	REAL	DATI
C	IN	:	NUMERO DI DATI

C	OUT:
C	RMX	:	REAL	VALORE MASSIMO STIMATO
C	RMN	:	REAL	VALORE MINIMO STIMATO
C	RMD	:	REAL	VALORE MEDIO STIMATO
C	OMX	:	I*4	MINUTO IN CUI SI E` VERIFICATA LA MASSIMA
C	OMN	:	I*4	MINUTO IN CUI SI E` VERIFICATA LA MINIMA

C	inizializzazione variabili
	MINSTEP=5
c	per variare i valori di default
	CALL MIEOPZIONI

C	Calcolo spline

	CALL CSCON(IN,X,Y,IBREAK,BREAK,CSCOEF)

C	inizializzo i valori max e min
	RMX=-500.
	RMN=500.
	OMX=0
	OMN=0
	RMD=0.

C	ricalcolo i valori a intervalli di 5 minuti
	DO K=X(1),X(IN),MINSTEP
	XVAL=FLOAT(K)

	YVAL=CSVAL(XVAL,IBREAK,BREAK,CSCOEF)

C	Cerco il massimo e il minimo dei dati stimati
C	il valore massimo e' contenuto in 	:	RMX
C	il valore minimo e' contenuto in 	:	RMN
C	l'ora del valore massimo e' contenuta in:	OMX
C	l'ora del valore minimo e' contenuta in :	OMN
C	tutte queste grandezze sono REAL*4

	IF(YVAL.GT.RMX)THEN
	RMX=YVAL
	OMX=XVAL
	END IF

	IF(YVAL.LT.RMN)THEN
	RMN=YVAL
	OMN=XVAL
	END IF

	RMD=RMD+YVAL	
	END DO

C	Calcolo la media stimata
	RMD=RMD/((X(IN)-X(1))/FLOAT(MINSTEP))

	RETURN
	END

C	SUBROUTINE MIEOPZIONI
C	COMMON /QUAINTERSPL/INTMAX,GRADMAX,MINSTEP
C
C	RETURN
C	END
