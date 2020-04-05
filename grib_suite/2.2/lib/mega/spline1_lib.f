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





	SUBROUTINE CUBSPL(X,Y,S,N,IEND,A)
COMSTART CUBSPL
C	SUBROUTINE CUBSPL(X,Y,S,N,IEND,A)
C
C  Questa routine calcola la matrice per trovare i coefficienti
C  di una spline cubica attraverso i dati. 
C  Il sistema e' risolto per ottenere i valori delle derivate
C  seconde. I parametri sono:
C   X,Y    :	vettori x e y da fittare
C   S      :	vettore delle derivate seconde nei punti
C   N	   :    numero di elementi 	
C   IEND   :	tipo di condizione al contorno da usare per S(1) e S(N)
C   IEND=1     	S(1)=S(N)=0.
C   IEND=2     	S(1)=S(2),  S(N)=S(N-1)
C   IEND=3     	finale cubico 
C   A      :   	matrice dei coefficienti e dei termini noti per trovare S
COMEND

	DIMENSION X(N),Y(N),S(N),A(N,4)

C   CALCOLA PER N-2 COLONNE

	NM2=N-2
	NM1=N-1
	DX1=X(2)-X(1)
	DY1=(Y(2)-Y(1))/DX1*6.
	DO 10 I=1,NM2
	   DX2=X(I+2)-X(I+1)
	   DY2=(Y(I+2)-Y(I+1))/DX2*6.
	   A(I,1)=DX1
	   A(I,2)=2.*(DX1+DX2)
	   A(I,3)=DX2
	   A(I,4)=DY2-DY1
	   DX1=DX2
	   DY1=DY2
10	 CONTINUE
C AGGIUSTA LA PRIMA E ULTIMA COLONNA PER LE APPROPRIATE CONDIZIONI
C  AGLI ESTREMI

	GO TO (20,50,80),IEND
C  SE IEND =1  NON VIENE FATTO NESSUN CAMBIAMENTO
20	GO TO 100

C PER IEND=2 S(1)=S(2)   S(N)=S(N-1)   PARABOLICO
50	A(1,2)=A(1,2)+X(2)-X(1)
	A(NM2,2)=A(NM2,2)+X(N)-X(NM1)
	GO TO 100 	 

C PER IEND=3  FINE CUBICA  S(1) E S(N) SONO ESTRAPOLATI
80	DX1=X(2)-X(1)
	DX2=X(3)-X(2)
	A(1,2)=(DX1+DX2)*(DX1+2.*DX2)/DX2
	A(1,3)=(DX2*DX2-DX1*DX1)/DX2
	DXN2=X(NM1)-X(NM2)
	DXN1=X(N)-X(NM1)
	A(NM2,1)=(DXN2*DXN2-DXN1*DXN1)/DXN2
	A(NM2,2)=(DXN1+DXN2)*(DXN1+2.*DXN2)/DXN2
	GO TO 100

C  SOLUZIONE DEL SISTEMA TRIDIAGONALE    RIDUZIONE
100	DO 110 I=2,NM2
	A(I,2)=A(I,2)-A(I,1)/A(I-1,2)*A(I-1,3)
	A(I,4)=A(I,4)-A(I,1)/A(I-1,2)*A(I-1,4)
110	CONTINUE
C  TORNIAMO DIETRO A SOSTITUIRE
	A(NM2,4)=A(NM2,4)/A(NM2,2)
	DO 120 I=2,NM2
	J=NM1-I
	A(J,4)=(A(J,4)-A(J,3)*A(J+1,4))/A(J,2)
120	CONTINUE
C  INTRODUZIONE DEI VALORI DENTRO IL VETTORE S
	DO 130 I=1,NM2 
	S(I+1)=A(I,4)
130	CONTINUE
	GO TO (150,160,170),IEND
150	S(1)=0.
	S(N)=0.
	RETURN
160	S(1)=S(2)
	S(N)=S(N-1)
	RETURN
170	S(1)=((DX1+DX2)*S(2)+DX1*S(3))/DX2
	S(N)=((DXN2+DXN1)*S(NM1)-DXN1*S(NM2))/DXN2
	RETURN
	END

	SUBROUTINE CUBCOEFF(X,Y,N,S,A,B,C,D)
COMSTART CUBCOEFF
C	SUBROUTINE CUBCOEFF(X,Y,N,S,A,B,C,D)
C	Questa subroutine calcola i coefficienti delle cubiche in ogni
C	intervallo [Xi,....Xi+1]
C	X	: vettore contenente le coordinate x dei nodi della spline 
C	Y	:     "       "       "    "       y  "    "    "      "
C	N	: numero di elementi.
C	S	: vettore delle derivate seconde nei nodi calcolati con CUBSPL
C	A,B,C,D	: vettori dei coeff. della spline nei nodi.
COMEND
	DIMENSION Y(N),X(N),S(N),A(N),B(N),C(N),D(N)

C	*** CALCOLO I COEFFICIENTI A, B,C,D DELLA SPLINE NEGLI INTERVALLI
C	    [Xi+1,..,Xi], 

	DO K=1,N-1
	H1=X(K+1)-X(K)
	A(K)=(S(K+1)-S(K))/(6.*H1)
	B(K)=S(K)/2.
	C(K)=(Y(K+1)-Y(K))/H1-(2.*H1*S(K)+H1*S(K+1))/6.
	D(K)=Y(K)
	END DO
	RETURN
	END


	SUBROUTINE CUBVAL(A,B,C,D,X,N,XVAL,YVAL,DVAL,DDVAL,IER)
COMSTART CUBVAL
C	SUBROUTINE CUBVAL(A,B,C,D,X,N,XVAL,YVAL,DVAL,DDVAL,IER)
C	Routine che calcola il valore stimato dalla spline in un punto
C	dati i coeff. della spline , le coordinate x dei nodi e la coordinata x
C	del punto che si vuol stimare.
C	XVAL	: coordinata x del valore da stimare
C	YVAL	: valore stimato
C	DVAL	: derivata prima stimata in XVAL
C	DDVAL	:    "     seconda   "    "  "
C	
C	Tutti gli altri parametri sono specificati nelle routine 
C			CUBVAL E CUBCOEFF
COMEND

	DIMENSION A(N),B(N),C(N),D(N),X(N)

	FX(AA,BB,CC,DD,XX)=(AA*(XX)**3)+(BB*(XX)**2)+(CC*(XX))+DD
	DFX(AA,BB,CC,XX)=(3*AA*(XX)**2)+(2*BB*(XX))+CC
	DDFX(AA,BB,XX)=(6*AA*(XX)+2*BB)
	DDDFX(AA,BB,XX)=(6*AA)

	IER=0
	IF(XVAL.GT.X(N).OR.XVAL.LT.X(1))GO TO 200
	DO L=1,(N-1)
	IF(XVAL.GE.X(L).AND.XVAL.LE.X(L+1))THEN
	DX=(XVAL-X(L))
	YVAL=FX(A(L),B(L),C(L),D(L),DX)
	DVAL=DFX(A(L),B(L),C(L),DX)
	DDVAL=DDFX(A(L),B(L),DX)
	GO TO 100
	END IF
	END DO
100	RETURN
200	IER=1
	END


	FUNCTION CUBAREA(N,X,A,B,C,D)
COMSTART CUBAREA
C	FUNCTION CUBAREA(N,X,A,B,C,D)
C
C	FUNZIONE CHE CALCOLA L'AREA COMPRESA FRA LA SPLINE E L'ASSE  X
C	Tutti gli elementi sono specificati nella routine CUBVAL.
COMEND

	DIMENSION X(N),A(N),B(N),C(N),D(N)
	CUBAREA=0.
	DO I=1,N-1
	DX=X(I+1)-X(I)
	AA=A(I)/4*DX**4
	BB=B(I)/3*DX**3
	CC=C(I)/2*DX**2
	DD=D(I)*DX
	CUBAREA=CUBAREA+AA+BB+CC+DD
	END DO
	RETURN
	END
