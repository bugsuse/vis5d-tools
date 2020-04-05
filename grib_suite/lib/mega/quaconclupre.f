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





	SUBROUTINE QUACONCLUPRE(ICLUS,IDATI,FLAG,NUM,ISOGLIA,PERC,IER)

COMSTART QUACONCLUPRE
C	SUBROUTINE QUACONCLUPRE(ICLUS,IDATI,FLAG,NUM,ISOGLIA,PERC,IER)
C
C	Questa subroutine controlla la consistenza della precipitazione 
C	per stazioni che appartengano a cluster percedentementi definiti.
C	
C	Se il cluster di una stazione viene definito zero la stazione 
C	non e` controllata.
C
C###############################################################################
C	NOTA BENE SOLO PER L'EMILIA ROMAGNA
C
C	CONTROLLO DI QUALITA` PER LA PRECIPITAZIONE CUMULATA IN 24 ORE
C	PER SETTARE I RELATIVI CLUSTER UTILIZZARE LA ROUTINE SET_CLUSTER.
C###############################################################################
C
C===============================================================================
C	input:
C
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
C	ISOGLIA		I*4	differenza sempre accettata tra stazioni
C				espressa in mm/10
C
C	PERC		R*4	PERCENTUALE DELLA PRECIPITAZIONE MASSIMA
C				TRA DUE STAZIONI CHE SI STANNO CONFRONTANDO
C				DA SOMMARE A ISOGLIA
C
C===============================================================================
C	output :
C
C	FLAG(num)	byte	vettore delle flag di qualita` del dato
C
C	IER		I*4	condizione di errore 
C				0=tutto O.K.  (non utilizzata)
C
C===============================================================================
C	CONDIZIONE DI ERRORE
C
C	La flag e` incrementata di 1 se sono verificate le seguenti condizioni:
C
C	a) Il cluster deve contenere piu` di 3 stazioni valide
c	   il dato valido viene definito dalla function j_c_e (dato mncante)
c	   e dalla function vf (testa se flag ha superato soglia di
c	   attendibilita`)
C
C	b) la stazione in esame differisce da tutte le altre dello stesso
C	cluster di piu` di   
C
C		| val1 - val2 | > ISOGLIA+PERC*(MAX(val1,val2))
C
C	dove ISOGLIA e PERC sono valori in input e val1 e` il valore di 
C	precipitazione nella stazione in esame, val2 il dato
C	di una stazione dello stesso cluster.
C
c	La flag viene decrementata solo nelle stazioni appartenenti ad un
c	cluster con piu` di 3 stazioni.
c	N.B.
C	(l'incremento e` effettuato solo sulla stazione anomala)
C	(stazione con precipitazione = prec maggiore di 0.)
COMEND
CC**********************************************************************CC
CC**********************************************************************CC
CC									CC
CC		SERVIZIO METEOROLOGICO REGIONE EMILA ROMAGNA		CC
CC				E.R.S.A.				CC
CC									CC
CC									CC
CC	PAOLO PATRUNO			   PIER PAOLO ALBERONI		CC
CC									CC
CC	BOLOGNA 1992							CC
CC**********************************************************************CC
CC**********************************************************************CC

	INTEGER IDATI(NUM),ICLUS(NUM)
	BYTE FLAG(NUM)
	LOGICAL J_C_E,F_S,vf

	ier=0


	DO IS=1,NUM
	  NSTAZ=0		!numero stazioni asciutte
	  F_S=.FALSE.		!
	  IF(.NOT.J_C_E(IDATI(IS)).OR.ICLUS(IS).EQ.0.or.
	1	.not.vf(flag(is)))GOTO 10			!dato mancante
	  DO ISS=1,NUM
	    IF(ICLUS(IS).NE.ICLUS(ISS).OR.IS.EQ.ISS)GOTO 9
	      IF(.NOT.J_C_E(IDATI(ISS)).or..not.vf(flag(iss)))GOTO 9 !dato mancante
	      NSTAZ=NSTAZ+1
	      IDIF=ABS(IDATI(IS)-IDATI(ISS))
	      IMAX=MAX(IDATI(IS),IDATI(ISS))
	      IF(IDIF.lT.ISOGLIA+PERC*IMAX)F_S=.TRUE.
9	  END DO

	  IF(NSTAZ.LT.3)GOTO 10
	  IF(F_S)THEN
	    FLAG(IS)=FLAG(IS)-1		!stazione corretta
	  ELSE
	    FLAG(IS)=FLAG(IS)+1
	  ENDIF
10	ENDDO
	RETURN
	END
