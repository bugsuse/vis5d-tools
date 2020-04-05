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





c	PROGRAM PROVA
c	PARAMETER N2=10
c
c	DIMENSION IMINUTI(N2),IDATI(N2)
c	BYTE FLAG(N2)
c
C	inizializzazione variabili
c	DATA INTMAX/150/,GRADMAX/.5/,RJUMP/1.2/
c
c	DO I=1,N2
c		IMINUTI(I)=I*60
c		ACCEPT*,IDATI(I)
c	END DO
c	CALL QUACONTEM (IMINUTI,IDATI,FLAG,INTMAX,RJUMP,GRADMAX,
c	1	N2,IN,IB,IER)
c	TYPE*,'IER=',IER,'IN=',IN,'IB=',IB
c
c	DO I=1,N2
c		TYPE*,FLAG(I),IDATI(I)
c		IDATI(I)=32767
c	END DO
c
c	CALL QUAINTERSPL (IMINUTI,IDATI,1,N2,
c	1	IN,RMX,RMN,RMD,IOMX,IOMN,IER)
c
c	type*,'*****************************************************'
c	type*,'in=',in,'  rmx=',rmx,'  rmn=',rmn,'  rmd=',rmd
c	type*,iomx,iomn,ier
c
c
c	STOP 
c	END
c

	SUBROUTINE QUACONTEM (IMINUTI,DATI,FLAG,INTMAX,RJUMP,GRADMAX,
	1	N2,NB,NE,IER)

COMSTART QUACONTEM
C	SUBROUTINE QUACONTEM (IMINUTI,DATI,FLAG,INTMAX,RJUMP,GRADMAX,
C				N2,NB,NE,IER)
C
c Esegue un test temporale sui dati.
c 
c L'asse dei tempi che viene espresso in minuti e` contenuto in IMINUTI.
c Per effettuare il controllo i dati non devono essere distanziati nel tempo
c piu` di INTMAX nel qual caso la serie viene spezzata in serie piu` piccole.
c Gli estremi delle serie non potranno essere controllati con il test dei 
c massimi e minimi
c Non vengono trattati i dati > 32767 o con flag >=iatt.
c (vedi :	COMMON /FLAGSOGLIA/IATT	
c		DATA IATT/3/	
c	nella function vf(flag)		)
c
c 1) test di variazione assoluta nel tempo
c Fa un controllo sui dati in maniera temporale controllando che la variazione
c assoluta tra due dati non superi RJUMP. Nel caso vengono settati tutti e due i
c dati errati e ricomincia col controllare il dato successivo. Se il test
c viene superato la flag non viene alterata altrimenti la flag viene 
c incrementata di una unita`
c 
c 2) test massimi e minimi	
c Solo se viene superato il primo test viene controllato che il dato considerato
c non sia un massimo o un minimo e che contemporaneamente non differisca dai
c valori intorno piu` di GRADMAX  per minuto. Se il test
c viene superato la flag viene decrementata altrimenti la flag viene 
c incrementata di una unita`. Gli estremi delle serie non potranno essere 
c controllati con il test dei massimi e minimi e la flag non verra` alterata.
c
C===============================================================================
c	INPUT:
c
c	IMINUTI(N2) :	I*4	CONTIENE LA COORDIMATA TEMPO (IN MINUTI)
c	DATI(N2): 	I*4	VETTORE CONTENENTE TUTTI I DATI
c	FLAG(N2)	BYTE	VETTORE DELLE FLAG ASSOCIATE AI DATI
C	INTMAX		I*4	DISTANZA MASSIMA TEMPORALE TRA I DATI
C	RJUMP		R*4	VARIAZIONE TEMPORALE ASSOLUTA ACCETTATA
C				ESPRESSA IN 
C			(UNITA` DI MIS. DATI/UNITA` MIS. COORDINATA TEMPO)
C	GRADMAX		R*4	VARIAZIONE TEMPORALE PER MASSIMI E MINIMI 
C				ACCETTATA ESPRESSA IN 
C			(UNITA` DI MIS. DATI/UNITA` MIS. COORDINATA TEMPO)
c	N2	:	I*4	DIMENSIONE DEI VETTORI: IMINUTI(N2),
C							DATI(N2),
C							FLAG(N2).
C				INDICE DELL' ELEMENTO CON CUI SI FINISCE
c				IL CONTROLLO
C
C===============================================================================
C	OUTPUT:
C
c	FLAG(N2) :    BYTE	FLAG ASSOCIATE AI DATI
C	NB	:	I*4	NUMERI DI DATI A CUI LA FLAG E` STATA
C				DECREMENTATA
C	NE	:	I*4	NUMERO DI DATI A CUI LA FLAG E` STATA
C				INCREMENTATA (DI UNA UNITA`)
C	IER	:	I*4	INDICATORE DI ERRORE
C
C	IER =  0    : 	TUTTO O.K.
C	IER =  1    :   L'indice  N2 < 1
C	IER =  2    :	Nessun dato buono nell' intervallo  
C	IER = -1    :   esiste un buco fra i dati superiore a INTMAX
C	IER = -2    :   Sono state incrementate piu` di tre flag
C
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


	INTEGER*4 IMINUTI(N2),DATI(N2)
	BYTE FLAG(N2)
	logical j_c_e,vf

C  Test che controlla la dimensione del vettore con l'intervallo da controllare
	IER=1
	IF(N2.LT.1) RETURN

	NB=0		! NUMERO DEI DATI BUONI 	totale
	NE=0		! NUMERO DEI DATI ERRATI	totale
	IER=0	
	IN=0		!numero di dati buoni 		parziale
	IND1=1
	IND2=1		!indici dei dati da analizzare
	IND3=1
	IND_B=0		! indice dato maxmin da decrementare

	DO I=1,N2
d		type*,'   i=',i,'   in=',in
d		type*,'ind1=',ind1,' ind2=',ind2,' ind3=', ind3
C	scarto i valori mancanti e gia flaggati errati

	IF(j_c_e(DATI(I)).AND.vf(FLAG(I)))THEN
C	se il dato e` presente
	 IND1=IND2			!dato gia` controllato
	 IND2=IND3			!dato da controllare
	 IND3=I				!ultimo dato buono
c
c		type *,'------------------------------'
c		type*,'   i=',i,'   in=',in
c		type*,'ind1=',ind1,' ind2=',ind2,' ind3=', ind3

C	verifico che l'intervallo tra loro non sia superiore a intmax	
C	controlla l'intervallo col dato precedente buono
	 IF ((IMINUTI(I)-IMINUTI(IND2)).GT.INTMAX)THEN

C	  IN=-1
C	  NE=-1
C	  RETURN

	  IER=-1	! condizione di errore in caso di intervallo >INTMAX
	  IN=1		! NUMERO DEI DATI BUONI 
	  IND1=I
	  IND2=I
	  IND3=I
	  IND_B = 0
	  GOTO 123

	END IF
C	Faccio un controllo sui dati in maniera temporale controllando
c	che la variazione assoluta tra due dati non superi rjump.
c	Nel caso li setto tutti e due errati e ricomincio col
c	controllare il dato successivo.

	 IF (IN.GE.1)THEN		!ci sono due dati da controllare
	  GRAD2=FLOAT((DATI(IND2)-DATI(IND3)))/
	1	FLOAT((IMINUTI(IND2)-IMINUTI(IND3)))
	  IF(abs(grad2).GE.RJUMP)THEN
C	    incrementa contatore errori e ricopre il dato errato
	    NE=NE+2
	    FLAG(IND2)=FLAG(IND2)+1
	    FLAG(IND3)=FLAG(IND3)+1
	    IN=1		! NUMERO DEI DATI BUONI 
	    IND1=I
	    IND2=I
	    IND3=I
	    IND_B=-I	! Il dato di inizio serie e' stato segnalato errato
	    GOTO 123
	  END IF
	 END IF


C	Faccio un controllo sui dati in maniera temporale controllando
C	che il dato considerato non sia un massimo o un minimo e che
C	contemporaneamente non differisca dai valori intorno piu` di
C	GRADMAX  per minuto.

	 IF (IN.GT.1)THEN		!ci sono tre dati da controllare

	  if(IND_B.gt.0)then		! decremento flag maxmin
	    NB=NB+1
	    FLAG(IND_B)=FLAG(IND_B)-1	!dato buono
	  endif
C	  controlla i gradienti
	  GRAD1=FLOAT((DATI(IND2)-DATI(IND1)))/
	1	FLOAT((IMINUTI(IND2)-IMINUTI(IND1)))
C	  GRAD2=FLOAT((DATI(IND2)-DATI(IND3)))/
C	1	FLOAT((IMINUTI(IND2)-IMINUTI(IND3)))

C	  se sono entrambi superiori a gradmax
	  IF (ABS(GRAD1).GT.GRADMAX.AND.ABS(GRAD2).GT.GRADMAX)THEN
C	   se sono di segno opposto
	   IF ((sign(1.,GRAD1)*sign(1.,GRAD2)).LT.0.)THEN
C	    incrementa contatore errori e ricopre il dato errato
	    NE=NE+1
	    FLAG(IND2)=FLAG(IND2)+1
	    IND2=IND1
c	perde un turno 
c	se il primo dato della tripletta era errato non devo decrementare
c	la flag di quello centrale
c	se il primo dato e' buono mi comporto come inizio serie per evitare
c	di ridecrementare il primo
	    if(ind_b . gt. 0) IND_B = 0
	    GOTO 123
	   END IF
	  END IF
c	  type *,ind2

c	Verico che la tripletta di dati non inizi con un dato errato
c	se a fine serie decremento la flag del penultimo dato
c	altrimenti memorizzo l'indice del dato in ind_b e continuo
c	il loop per testare il dato seguente
c	
	  if(ind1.ne.-ind_b)then
	    if(ind3 .eq. n2)then
	        NB=NB+1
		FLAG(IND2)=FLAG(IND2)-1	!dato buono
	    else
		ind_B=ind2
	    endif
	  end if

	 END IF
C	 se il dato c'e` e non e` errato	(non si sa se e` buono)
	 IN=IN+1
	END IF
123	END DO

C	se ci sono piu` di tre dati considerati errati 
C	termina in condizione di errore
c	uguale se non ne ha trovato neanche uno buono

	IF(NE.GE.3) IER=-2
	IF(NB.EQ.0) IER= 2

	RETURN
	END
