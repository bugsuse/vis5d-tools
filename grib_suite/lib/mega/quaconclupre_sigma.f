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





	SUBROUTINE QUACONCLUPRE_sigma(ICLUS,IDATI,FLAG,NUM,IER)
COMSTART QUACONCLUPRE_sigma
C	SUBROUTINE QUACONCLUPRE(ICLUS,IDATI,FLAG,NUM,IER)
C
C	UTILIZZARE LA ROUTINE QUACONCLUPRE
C
COMEND
C	CONTROLLO DI QUALITA` PER LA PRECIPITAZIONE CUMULATA IN 24 ORE
C	UTILIZZANDO UNA SUDDIVISIONE IN CLUSTER DELLE STAZIONI.
C	PER SETTARE I RELATIVI CLUSTER UTILIZZARE LA ROUTINE SET_CLUSTER.
C
C
C	input:
C	ICLUS(num)	i*4	cluster per ogni stazione
C				i cluster sono numerati da 1 a 5.
c				se cluster=0 la stazione non e` da considerare
c
C	IDATI(num)	i*4	valori di precipitazione cumulata in 24 ore
C				su ogni stazione espressa in mm/10
c
C	FLAG(num)	byte	vettore delle flag di qualita` del dato
C
C	NUM		I*4	numero delle stazioni da controllare
C
C	IER		I*4	condizione di errore 
C				0=tutto O.K.  (non utilizzata)
C	la flag e` incrementata di 1 se:
C	in un cluster con piu` di 3 stazioni 
C	1) c'e stata pioggia solo in una stazione
C	2) non e` piovuto solo in una stazione
C	3) se c'e` precipitazione e il dato della stazione scarta dalla
C	   media del cluster : piu` di due millimetri e piu` di 2.5 il sigma
c	   della precipitazione in quel cluster.
C
c	la flag viene decrementata solo nelle stazioni appartenenti ad un
c	cluster con piu` di 3 stazioni.
c	N.B.
C	(l'incremento e` effettuato solo sulla stazione anomala)
C	(stazione con precipitazione = prec maggiore di 0.)


	PARAMETER NCLUSTER=5	!numero massimo di cluster
	INTEGER IDATI(NUM),ICLUS(NUM),IP_S(NCLUSTER),IP_N(NCLUSTER)
	DIMENSION RMED(NCLUSTER),SIGMA(NCLUSTER)
	BYTE FLAG(NUM)
	LOGICAL J_C_E,F_S(NCLUSTER),F_N(NCLUSTER)

	ier=0

	DO I=1,NCLUSTER		!NUMERO MAX CLUSTER
	  F_S(I)=.FALSE.	!flag segnalazione errore in cluster asciutto
	  F_N(I)=.FALSE.	!flag segnalazione errore in cluster bagnato

	  STAZ_S=0.		!numero stazioni bagnate
	  STAZ_N=0.		!numero stazioni asciutte
	  MED=0			!sommatoria prec
	  SUMQ=0		!sommatoria dei quadrati prec

	  DO IS=1,NUM
	    IF(.NOT.J_C_E(IDATI(IS)))GOTO 10	!dato mancante
	    IF(ICLUS(IS).EQ.I)THEN
	      IF(IDATI(IS).NE.0)THEN
	        STAZ_S=STAZ_S+1
	        IP_S(I)=IS		!indice vettore dato forse errato
	      ELSE
	        STAZ_N=STAZ_N+1
	        IP_N(I)=IS		!indice vettore dato forse errato
	      ENDIF
	      MED=MED+IDATI(IS)
	      SUMQ=SUMQ+IDATI(IS)**2
10	      CONTINUE
	    ENDIF
	  ENDDO
	  IF(STAZ_N.GE.3.AND.STAZ_S.EQ.1)F_S(I)=.TRUE.	!errore su bagnata
	  IF(STAZ_S.GE.3.AND.STAZ_N.EQ.1)F_N(I)=.TRUE.	!errore su asciutta
	  IF((STAZ_S+STAZ_N).GE.4)THEN
	    RMED(I)=FLOAT(MED)/(STAZ_S+STAZ_N)
	    SIGMA(I)=SQRT(SUMQ-RMED(I)**2*(STAZ_S+STAZ_N))/(STAZ_S+STAZ_N)
	  ELSE
	    Rmed(i)=-1.			!non ho abbastanza stazioni
	  ENDIF
	ENDDO

	DO IS=1,NUM
	  IF(.NOT.J_C_E(IDATI(IS)))GOTO 12	!dato mancante
	  IC=ICLUS(IS)
	  IF(IC.EQ.0)GOTO 12		!stazione da non considerare
	  RM=RMED(IC)
	  IF(F_S(IC).AND.IP_S(IC).EQ.IS)THEN
	    FLAG(IS)=FLAG(IS)+1		!stazione bagnata errata
	  ELSEIF(F_N(IC).AND.IP_N(IC).EQ.IS)THEN
	    FLAG(IS)=FLAG(IS)+1		!stazione asciutta errata
	  ELSEIF(RM.NE.-1..AND.
	1	abs(IDATI(IS)-RM).GT.20..AND.	!confronto con la media
	1	abs(IDATI(IS)-RM).GT.(2.5+RM*.01)*SIGMA(IC))THEN
	    FLAG(IS)=FLAG(IS)+1
	  ELSEIF(RM.NE.-1.)THEN
	    FLAG(IS)=FLAG(IS)-1		!stazione corretta
	  ENDIF
12	ENDDO
	RETURN
	END
