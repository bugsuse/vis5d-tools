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





      INTEGER FUNCTION M_GETSS(FILAC, IUN, PRIMO, ISTAZL,
     *                   ISTAZU, STAZE, DATA, ORA, BUFFER)
COMSTART M_GETSS
C      INTEGER FUNCTION M_GETSS(FILAC, IUN, PRIMO, ISTAZL,
C     *                   ISTAZU, STAZE, DATA, ORA, BUFFER)
C
C   Estrae TEMPA e TEMPB, fissata DATA e ORA, per le stazioni comprese 
C   nei limiti ISTAZL, ISTAZU.
C   ad ogni chiamata ritorna in BUFFER i dati relativi ad una stazione,
C   in ordine crescente entro i limiti imposti.
c   Se per una o piu` staz. consecutive manca un report (filac(1) o filac(2))
c   riporta il report della prima staz. successiva in cui e' presente.
C   Se alla chiamata precedente i due report erano fasati si avanzera` su
c   tutti e due i tipi altrimenti solo con quello con stazione piu` 
c   piccola.
C 
c	input:
c	FILAC(2)	CHAR*100	nomi file archivio per tipo1 e tipo2
c	IUN(2)		I*4		unita` logiche per tipo1 e tipo2
c	PRIMO(2)	LOGICAL		true per primo accesso per tipo1 e tipo2
c	ISTAZL		I*4		stazione inferiore da estrarre
c	ISTAZU		I*4		stazione superiore da estrarre
c	DATA(3)		I*4		data (gg,mm,aa) da estrarre
c	ORA(2)		I*4		ora (hh,mm) da estrarre
c
c	output:
c	STAZE(2)	I*4		stazioni estratte 
c					corrispondenti a buffer
c	BUFFER(230,2)	I*2	dati in output per tipo1 e tipo2
c	M_GETSS		I*4	condizione di errore come getssdds
COMEND
      PARAMETER ( ULTIMTIM=2000000000, ULTIMSTAZ=99999 )
C
      INTEGER*2  BUFFER(230,2)
      INTEGER ITIME(20), FINE
      INTEGER IUN(2), ISTAZL, ISTAZU, ISTAZE(20), STAZE(2),
     *        DATA(3), ORA(2), DATAL(3), ORAL(2), DATAU(3), ORAU(2),
     *        IDATA(3), IORA(2)
      CHARACTER*100 FILAC(2)
      LOGICAL PRIMO(2),FIRST
C
      COMMON /ULTIMO/JJ,FINE,ITIME,ISTAZE
C
      DO I=1,3
        DATAL(I)=DATA(I)
        DATAU(I)=DATA(I)
      ENDDO
      ORAL(1)=ORA(1)
      ORAU(1)=ORA(1)
      ORAL(2)=ORA(2)
      ORAU(2)=ORA(2)
      JSTART =1 
      NREP   =2
C
      FIRST=.TRUE.      !PRIMA VOLTA
       DO I=1,NREP
        IF(.NOT.PRIMO(I))FIRST=.FALSE.
      END DO
C
C Inizializzazione date piu` vecchie per tipo report
C
      IF (FIRST)THEN
        DO I=1,NREP
          ISTAZE(I)=ULTIMSTAZ
        END DO
        FINE=0                  !contatore dei file finiti
      END IF
C
      IF(ISTAZE(1).GT.ISTAZE(2)) THEN
        JSTART=2
      ELSEIF(ISTAZE(1).LT.ISTAZE(2)) THEN  
        NREP=1
      ENDIF
C
C Ciclo di estrazione dati per i tipi di messaggio
C
      DO 100 J=JSTART,NREP
C
        M_GETSS=N_GETSDS(FILAC(J),IUN(J),PRIMO(J),ISTAZL,DATAL,ORAL,
     *              ISTAZU,DATAU,ORAU,ISTAZE(J),IDATA,IORA,BUFFER(1,J))
C
        STAZE(J)=ISTAZE(J)

        IF (M_GETSS)1,2,3         !SE <0 ERRORE DI I/O ESCO
                                  !SE 0  MESSAGGIO TROVATO PROCEDO
                                  !SE >0 FINE RICERCA
 1    RETURN

 3    FINE=FINE+1                 !incremento contatore file finiti
c      ITIME(J)=ULTIMTIM
      ISTAZE(J)=ULTIMSTAZ
      IF (FINE.LT.NREP)GOTO 100     !ho ancora dei file da scorrere
      M_GETSS=1                   !termina
      RETURN

 2    CONTINUE                    !report trovato
C
 100  CONTINUE
C
      IPRIMSTAZE=ULTIMSTAZ
      M_GETSS=0
C
      RETURN
      END
