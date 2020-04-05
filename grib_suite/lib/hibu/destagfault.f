
c    <HIBU, software di utilita' per  grid>
c    Copyright (C) 2000  <Paolo Patruno SMR ARPA>
c
c    $Date: 2000/03/27 17:04:09 $    $Revision: 1.2 $
c    $Id: destagfault.f,v 1.2 2000/03/27 17:04:09 lambo Exp $

c    Questo programma ÅË software  libero; ÅË lecito ridistribuirlo e/o
c    modificarlo secondo i  termini della Licenza Pubblica Generica SMR 
c    come pubblicata  da ARPA SMR ; riferirsi alla versione 1
c    della licenza o (a scelta) ad una versione successiva.

c    Questo programma ÅË distribuito  nella speranza che sia utile,  ma
c    SENZA  ALCUNA GARANZIA;  senza  neppure la  garanzia  implicita di
c    COMMERCIABILITA' o di APPLICABILITA' PER UN PARTICOLARE SCOPO.  Si
c    veda la Licenza Pubblica Generica SMR per avere maggiori dettagli.

c    Ognuno dovrebbe avere ricevuto una copia della Licenza Pubblica
c    Generica SMR insieme a questo programma; in caso contrario, la si
c    puÅÚ ottenere da Agenzia Regionale Prevenzione e Ambiente (ARPA)
c    Servizio Meteorologico Regionale (SMR), Viale Silvani 6, 40122
c    Bologna, Italia
c    http://www.smr.arpa.emr.it


      subroutine destagFAULT(field,i1,j1,FAULT)
COMSTART DESTAGFAULT
C	subroutine destagFAULT(field,i1,j1,FAULT)
C
C	opera un riempimento di una matrice staggherata nei punti H o V
c	tramite una interpolazione bilineare nei punti in cui risulti
c	assegnato il valore FAULT.
c
c       Questa versione non interpola sul dato mancante se anche
c       solo uno dei dati da cui deve interpolare risulta mancante.
c
C       Vengono elaborati anche i bordi e gli angoli.
c
COMEND
      DIMENSION FIELD(I1,J1)


      DO I=2,I1-1
C				BORDO

        IF(FIELD(I,1).EQ.FAULT)THEN
          IF(FIELD(I-1,1).NE.FAULT.AND.FIELD(I+1,1).NE.FAULT)
     +     FIELD(I,1)=(FIELD(I-1,1)+FIELD(I+1,1))*0.5

        END IF
        
        IF(FIELD(I,J1).EQ.FAULT)THEN
          IF(FIELD(I-1,J1).NE.FAULT.AND.FIELD(I+1,J1).NE.FAULT)
     +     FIELD(I,J1)=(FIELD(I-1,J1)+FIELD(I+1,J1))*0.5
        END IF
        
        DO J=2,J1-1
C				INTERNO
          IF(FIELD(I,J).EQ.FAULT)THEN
            
            IF(FIELD(I-1,J).NE.FAULT.AND.
     +       FIELD(I+1,J).NE.FAULT.AND.
     +       FIELD(I,J-1).NE.FAULT.AND.
     +       FIELD(I,J+1).NE.FAULT)
     +       FIELD(I,J)=(FIELD(I-1,J)+FIELD(I+1,J)
     +       +FIELD(I,J-1)+FIELD(I,J+1))*0.25
          END IF
        END DO
      END DO
      
      DO J=2,J1-1
C				BORDO
        IF(FIELD(1,J).EQ.FAULT)THEN
          IF(FIELD(1,J-1).NE.FAULT.AND.FIELD(1,J+1).NE.FAULT)
     +     FIELD(1,J)=(FIELD(1,J-1)+FIELD(1,J+1))*0.5
        END IF
        IF(FIELD(I1,J).EQ.FAULT)THEN
          IF (FIELD(I1,J-1).NE.FAULT.AND.FIELD(I1,J+1).NE.FAULT)
     +     FIELD(I1,J)=(FIELD(I1,J-1)+FIELD(I1,J+1))*0.5
        END IF
      END DO
c
c	corner 
c
      IF(FIELD(1,1).EQ.FAULT)THEN
        IF(FIELD(1,2).NE.FAULT.AND.FIELD(2,1).NE.FAULT)
     +   FIELD(1,1)=(FIELD(1,2)+FIELD(2,1))*0.5
      END IF
      IF(FIELD(1,J1).EQ.FAULT)THEN
        IF(FIELD(2,J1).NE.FAULT.AND.FIELD(1,J1-1).NE.FAULT)
     +   FIELD(1,J1)=(FIELD(2,J1)+FIELD(1,J1-1))*0.5
      END IF
      IF(FIELD(I1,J1).EQ.FAULT)THEN
        IF(FIELD(I1-1,J1).NE.FAULT.AND.FIELD(I1,J1-1).NE.FAULT)
     +   FIELD(I1,J1)=(FIELD(I1-1,J1)+FIELD(I1,J1-1))*0.5
      END IF
      IF(FIELD(I1,1).EQ.FAULT)THEN
        IF(FIELD(I1,2).NE.FAULT.AND.FIELD(I1-1,1).NE.FAULT)
     +   FIELD(I1,1)=(FIELD(I1,2)+FIELD(I1-1,1))*0.5
      END IF
        
c      print *,'corner', FIELD(1,1),FIELD(i1,j1)
      RETURN
      END
      






