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





	SUBROUTINE QUAAGMETCLIM(FLAG,IER)
COMSTART QUAAGMETCLIM
c	FLAG(21)	BYTE	incrementato di uno se verificato errore
c				per le 21 variabili agrometeo con soglie
c				climatiche strette, oppure incrementato di
c				tre su soglie larghe.
c	IER		I*4	condizione di errore
C			IER=1	tipo messaggio errato
c				(ossia tipo<>12,13,14)
c			IER=-1	non trovo i dati anagrafici della stazione
c				e quindi faccio controlli piu` blandi.
C			IER=-2  errore climaumidita` o climatemperatura
c				relativo controllo piu` blando
c			ier=100 troppe unita` logiche aperte
C			IER>100	codice iostat fortran+100

c	n.b.
c	tipo report deve essere 12 (biorarie ora solare), 13 (orarie ora Z),
c	oppure 14 (biorarie ora Z)
c
c	COMMON /RRW/RRW	I*2	RRW(28) per passare i dati synop come dichiarato
c				in loc$inc:agrmet_120.inc

C	Per indirizzare il vettore FLAG alla variabile interessata si puo`
c	utilizzare, includendo nel proprio programma, il file
c	megainc$dir:quaagmet.inc ed utilizzare come indici del; vettore le
c	variabili che hanno lo stesso nome della variabile (meteodata)
c	preceduto da 'IN'. Ad esempio:
c	per la temperatura        FLAG(INT180)
COMEND
	COMMON /RRW/RRW

	DIMENSION LIMINF(21),LIMSUP(21)

C	INCLUDE 'LOC$INC:AGRMET_120.INC'

	INTEGER*4 STAZ,DATA(3),ORA(2),TIPO
	BYTE FLAG(21)
	INTEGER*2 RRW(28)
	CHARACTER*24 ANOME
	LOGICAL C_E

C	INCLUDE 'MEGAINC$DIR:QUALOCAL.INC/NOLIST'

	DIMENSION PMED(12),PSTD(12)
	DATA PMED  
	1	/1017.8,1015.3,1015.4,1013.2,1014.1,1014.5,1014.4,
	1	 1014.6,1016.9,1018.5,1016.8,1018.6/
	DATA PSTD
	1	/   9.1,   9.7,   8.2,   6.7,   5.4,   4.7,   3.8,
	1	    4.2,   5.1,   7.5,   8.6,  10.5/
	DATA QS,USTD/2.,15./
	DATA ALATMIN,ALATMAX,ALONMIN,ALONMAX/	!limiti dell'area Emilia R.
	1	43.5,45.5,9.,13./


	ier=0

	CALL GETHEA (STAZ,DATA,ORA,TIPO,RRW)

	IF (TIPO.lt.12.or.tipo.gt.14)THEN
		IER=1
		RETURN
	END IF

	if(tipo.eq.12)then	
c				da ora solare a ora Z
	call jeladata5(data(1),data(2),data(3),ora(1),ora(2),iminuti)
	iminuti=iminuti-60
	call jeladata6(data(1),data(2),data(3),ora(1),ora(2),iminuti)
	end if

C	soglia inferiore per tutte le variabili
C	soglia superiore per tutte le variabili


C	DICHIARAZIONE TRACCIATO RECORD AGRMET_120
C	RICHIEDE UNA DICHIARAZIONE : BYTE RR(N) N>=56
C
c	integer*2 T180,T180ME,T180MN,T180MX,
c	1	  UM180,UM180ME,UM180MN,UM180MX,
c	1	  PREC,BAGNA,RAD,V10,V60,
c	1	  T100,T100ME,T100MN,T100MX,
c	1	  UM100,UM100ME,UM100MN,UM100MX

        liminf( 1)=  -300          !TEMPERATURA
        limsup( 1)=   500          !
        liminf( 2)=  -300          !TEMPERATURA
        limsup( 2)=   500          !
        liminf( 3)=  -300          !TEMPERATURA
        limsup( 3)=   500          !
        liminf( 4)=  -300          !TEMPERATURA
        limsup( 4)=   500          !
        liminf( 5)=    30          !UMIDITA`
        limsup( 5)=   100          !
        liminf( 6)=    30          !UMIDITA`
        limsup( 6)=   100          !
        liminf( 7)=    30          !UMIDITA`
        limsup( 7)=   100          !
        liminf( 8)=    30          !UMIDITA`
        limsup( 8)=   100          !
        liminf( 9)=     0          !PRECI		????
        limsup( 9)=  4092          !
        liminf(10)=     0          !BAGNATURA
	if(tipo.eq.13)then
	        limsup(10)=   60   !
	else
	        limsup(10)=   120  !
	end if
        liminf(11)=     0          !RADIAZIONE
	if(tipo.eq.13)then
	        limsup(11)=  2280  !
	else
	        limsup(11)=  4560  !
	end if
        liminf(12)=     0          !VENTO FF
        limsup(12)=   500          !
        liminf(13)=     0          !VENTO FF
        limsup(13)=   500          !
        liminf(14)=  -300          !TEMPERATURA
        limsup(14)=   500          !
        liminf(15)=  -300          !TEMPERATURA
        limsup(15)=   500          !
        liminf(16)=  -300          !TEMPERATURA
        limsup(16)=   500          !
        liminf(17)=  -300          !TEMPERATURA
        limsup(17)=   500          !
        liminf(18)=    30          !UMIDITA`
        limsup(18)=   100          !
        liminf(19)=    30          !UMIDITA`
        limsup(19)=   100          !
        liminf(20)=    30          !UMIDITA`
        limsup(20)=   100          !
        liminf(21)=    30          !UMIDITA`
        limsup(21)=   100          !

C				limiti climatici veramente estremi!
	DO NVAR=7,27
	  IF(C_E(RRW(NVAR)))THEN
	    IF (RRW(NVAR).LT.LIMINF(NVAR-6))FLAG(NVAR-6)=FLAG(NVAR-6)+2
	    IF (RRW(NVAR).GT.LIMSUP(NVAR-6))FLAG(NVAR-6)=FLAG(NVAR-6)+2
	  ENDIF	
	END DO

C	leggo i dati anagrafici solo se non sono gia` stati letti

	call GETSTAZAGR(staz,ANOME,HHPOZ,ALAT,ALON,IER)
C	
C	Serve ad associare ad una stazione i relativi parametri anagrafici
C	Letti nell' archivio meteodata.
C
C	Input	:	STAZ	= I*4	Codice W.M.O. stazione
C
C	Output	:	ANOME   = C*24	Nome della stazione
C			HHPOZ	= R*4   Altezza del pozzetto (m.)
C			ALAT	= R*4	Latitudine ( Gradi,primi)
C			ALON	= R*4	Longitudine (Gradi,primi)

c	type*,'getstazagr ier=',ier

c	limiti per pressione al livello della stazione

	IF (HHPOZ.LT.-100.)THEN
		IER=-1
		GOTO 23
	END IF

C-------------------------------------------------------------------------------
C	CONTROLLO SE DENTRO LIMITI REGIONALI
C-------------------------------------------------------------------------------
	IF (ALON.LT.ALONMIN.OR.ALON.GT.ALONMAX.OR.
	1	ALAT.LT.ALATMIN.OR.ALAT.GT.ALATMAX)THEN
			IER=-1
			GOTO 23
	ENDIF

	ALAT=SESS_TO_CENT(ALAT)		!tutte le routines in centesimali
	ALON=SESS_TO_CENT(ALON)

C-------------------------------------------------------------------------------
C	UMIDITA
C-------------------------------------------------------------------------------

	CALL CLIMA_UMIDITA(DATA(2),ORA(1),val11,val12,IER)
	IF(IER.NE.0)THEN
		IER=-2
		goto 25
	end if

	if (tipo.eq.13)then
		val21=val11
		val22=val12
	else
		IORAI=ORA(1)-1
		IF(IORAI.eq.-1)IORAI=23
		CALL CLIMA_UMIDITA(DATA(2),iorai,val21,val22,IER)
		IF(IER.NE.0)THEN
			IER=-2
			goto 25
		end if
	end if

	val1=(val11+val21)/2.
	val2=(val12+val22)/2.	

	IF(VAL12.LE.USTD)THEN
		LIMINF( 5)=50
		LIMINF(18)=50
	END IF
	IF(VAL11.LE.USTD)THEN
		LIMINF( 5)=80
		LIMINF(18)=80
	END IF
	IF(100-VAL11.LE.USTD)THEN
		LIMSUP( 5)=80
		LIMSUP(18)=80
	END IF


	IF(VAL2.LE.USTD)THEN
		LIMINF( 6)=50
		LIMINF( 7)=50
		LIMINF( 8)=50

		LIMINF(19)=50
		LIMINF(20)=50
		LIMINF(21)=50
	END IF
	IF(VAL1.LE.USTD)THEN
		LIMINF( 6)=80
		LIMINF( 7)=80
		LIMINF( 8)=80

		LIMINF(19)=80
		LIMINF(20)=80
		LIMINF(21)=80
	END IF
	IF(100-VAL1.LE.USTD)THEN
		LIMSUP( 6)=80
		LIMSUP( 7)=80
		LIMSUP( 8)=80

		LIMSUP(19)=80
		LIMSUP(20)=80
		LIMSUP(21)=80
	END IF
25	continue

	IF (HHPOZ.LT.0)HHPOZ=0.

C-------------------------------------------------------------------------------
C	RADIAZIONE
C-------------------------------------------------------------------------------

	if(tipo.eq.13)then	
		IORAI=ORA(1)-1
	else
		IORAI=ORA(1)-2
	end if

	IF(IORAI.eq.-1)IORAI=23
	IF(IORAI.eq.-2)IORAI=22

	CALL RADMAX(DATA(1),DATA(2),DATA(3),IORAI,0,ORA(1),ORA(2),
	1		cent_to_sess(ALAT),cent_to_sess(ALON),
	1		HHPOZ,VALMAX,VALMIN,ier)

	LIMINF(11)=NINT(VALMIN*10.)		!con cielo coperto
	LIMSUP(11)=NINT(VALMAX*10.)		!con cielo sereno

C-------------------------------------------------------------------------------
C	TEMPERATURA IN CAPANNINA
C-------------------------------------------------------------------------------
	CALL CLIMA_TEMP_MAX_MIN
	1	(ALAT,ALON,HHPOZ,DATA(2),DATA(1),ORA(1),VAL1,
	1	SIGMAX,SIGMIN,SIGMED,IER)
	IF(IER.NE.0)THEN
		IER=-2
		goto 23
	end if

d	TYPE*,ALAT,ALON,HHPOZ,DATA(2),DATA(1),ORA(1),VAL1,
d	1	SIGMAX,SIGMIN,SIGMED,IER

	if(tipo.eq.13)then
		val2=val1
	else
		IORAI=ORA(1)-1
		IF(IORAI.eq.-1)IORAI=23
		CALL CLIMA_TEMP_MAX_MIN
	1		(ALAT,ALON,HHPOZ,DATA(2),DATA(1),iorai,VAL2,
	1		SIGMAX,SIGMIN,SIGMED,IER)
		IF(IER.NE.0)THEN
			IER=-2
			goto 23
		end if
	end if

d	TYPE*,ALAT,ALON,HHPOZ,DATA(2),DATA(1),ORA(1),VAL2,
d	1	SIGMAX,SIGMIN,SIGMED,IER

	val=(val1+val2)/2.
	
	LIMINF( 1)= NINT((VAL1-QS*SIGMED)*10.)
	LIMINF( 2)= NINT((VAL-QS*SIGMED)*10.)
	LIMINF( 3)= NINT((VAL-QS*SIGMIN)*10.)
	LIMINF( 4)= NINT((VAL-QS*SIGMAX)*10.)
	
	LIMINF(14)= NINT((VAL1-QS*SIGMED)*10.)
	LIMINF(15)= NINT((VAL-QS*SIGMED)*10.)
	LIMINF(16)= NINT((VAL-QS*SIGMIN)*10.)
	LIMINF(17)= NINT((VAL-QS*SIGMAX)*10.)
	
	LIMSUP( 1)= NINT((VAL1+QS*SIGMED)*10.)
	LIMSUP( 2)= NINT((VAL+QS*SIGMED)*10.)
	LIMSUP( 3)= NINT((VAL+QS*SIGMIN)*10.)
	LIMSUP( 4)= NINT((VAL+QS*SIGMAX)*10.)

	LIMSUP(14)= NINT((VAL1+QS*SIGMED)*10.)
	LIMSUP(15)= NINT((VAL+QS*SIGMED)*10.)
	LIMSUP(16)= NINT((VAL+QS*SIGMIN)*10.)
	LIMSUP(17)= NINT((VAL+QS*SIGMAX)*10.)

23	CONTINUE	!FINE CASI PARTICOLARI INIZIO CONTROLLO

	DO NVAR=7,27
	  IF(C_E(RRW(NVAR)))THEN
	    IF (RRW(NVAR).LT.LIMINF(NVAR-6))FLAG(NVAR-6)=FLAG(NVAR-6)+1
	    IF (RRW(NVAR).GT.LIMSUP(NVAR-6))FLAG(NVAR-6)=FLAG(NVAR-6)+1
	  ENDIF	
	END DO

	RETURN
	END
