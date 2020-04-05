
c    <HIBU, software di utilita' per  grid>
c    Copyright (C) 2000  <Paolo Patruno SMR ARPA>
c
c    $Date: 2000/04/11 10:55:02 $    $Revision: 1.4 $
c    $Id: conv_rout_new.f,v 1.4 2000/04/11 10:55:02 lambo Exp $

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

C=======================================================================
C
	SUBROUTINE  REGLH_E(AIN,AOUT,IM,JM,IMHV,JMHV) 
COMSTART REGLH_E
C	SUBROUTINE  REGLH_E(AIN,AOUT,IM,JM,IMHV,JMHV) 
C
C	INPUT:
C	AIN(IM,JM)	REAL	MATRICE ENTRANTE
C	IM		INTEGER	
C	JM		INTEGER	
C	IMHV		INTEGER	
C	JMHV		INTEGER	
C
C	OUTPUT:
C	AOUT(imhv,jmhv)
C
C=======================================================================
c
c	Spacchetta la matrice entrante nell'indice j: ricordiamo
c	che sulla griglia E-Arakawa staggered:
c
c      	         h   u,v   h   u,v   h   ...  u,v   h
c	j=jmt
c	        u,v   h   u,v   h   u,v  ...   h   u,v
c	        ...  ...  ...  ...  ...  ...  ...  ...
c	j
c	        ...  ...  ...  ...  ...  ...  ...  ...
c	        u,v   h   u,v   h   u,v  ...   h   u,v
c	j=2
c                h   u,v   h   u,v   h   ...  u,v   h
c	        u,v   h   u,v   h   u,v  ...   h   u,v
c	j=1
c                h   u,v   h   u,v   h   ...  u,v   h
c
c
c	       i=1   i=2  i=3  i=4  i=5   i       i=imt
c
c
c
c	La matrice AIN e' compattata nell'indice j; la matrice AOUT 
C	e' scompattata, ed ha infatti una dimensione j doppia di AIN.
c
c	N.B.: AOUT(i,j) ha un valore di h per ogni (i,j).
c
c-----------------------------------------------------------------------
COMEND
C	INCLUDE 'HIBIN$DIR:HIBUGRID.INC'
c
	DIMENSION AIN(IM,JM),AOUT(imhv,jmhv)
c-----------------------------------------------------------------------
c
c	controllo
c
C	TYPE *,'IM,JM,IMHV,JMHV',IM,JM,IMHV,JMHV
c-----------------------------------------------------------------------
c
c	Calcolo
c
        DO 10 J=1,JM
        DO 10 I=1,IM
c
C	south boundary
c
	   IF(J.EQ.1) THEN
	      IF(MOD(I,2).EQ.0) THEN
	         AOUT(I,J)=.5*(AIN(I-1,J)+AIN(I+1,J))
	      ELSE
	         AOUT(I,J)=AIN(I,J)
	      ENDIF
C
C	north boundary
C
	   ELSE IF(J.EQ.JM) THEN
	      JJ=2*J-1
	      IF(MOD(I,2).EQ.0)  THEN
	         AOUT(I,JJ)=.5*(AIN(I-1,J)+AIN(I+1,J))
	      ELSE
	         AOUT(I,JJ)=AIN(I,J)
	      ENDIF
C
C	west boundary
C
	   ELSE IF(I.EQ.1) THEN
	      JJ=2*J-1
	      JJJ=JJ-1
	      AOUT(I,JJJ)=.5*(AIN(I,J-1)+AIN(I,J))
	      AOUT(I,JJ)=AIN(I,J)
	      IF(J.EQ.JM-1) AOUT(I,JJ+1)=.5*(AIN(I,J)+AIN(I,J+1))
C
C	east boundary
C
	   ELSE IF(I.EQ.IM) THEN
	      JJ=2*J-1
	      JJJ=JJ-1
	      AOUT(I,JJJ)=.5*(AIN(I,J-1)+AIN(I,J))
	      AOUT(I,JJ)=AIN(I,J)
	      IF(J.EQ.JM-1) AOUT(I,JJ+1)=.5*(AIN(I,J)+AIN(I,J+1))
C
C	inside domain
C
	   ELSE
	      JJ=2*J-1
	      IF(MOD(I,2).EQ.0)   THEN
	         JJJ=JJ+1
	         AOUT(I,JJ)=.25*(AIN(I-1,J)+AIN(I+1,J)+AIN(I,J-1)
	1	   +AIN(I,J))
	         AOUT(I,JJJ)=AIN(I,J)
                 IF(J.EQ.2) AOUT(I,J)=AIN(I,J-1) 
              ELSE
	         JJJ=JJ+1
	         AOUT(I,JJ)=AIN(I,J)
	         AOUT(I,JJJ)=.25*(AIN(I-1,J)+AIN(I+1,J)+AIN(I,J+1)
	1	   +AIN(I,J))
	         IF (J.EQ.2)  THEN
        	    AOUT(I,J)=.25*(AIN(I,J)+AIN(I-1,J-1)+
	1				AIN(I,J-1)+AIN(I+1,J-1))
	         ENDIF    
	      ENDIF
	   END IF
 10     CONTINUE
C-----------------------------------------------------------------------
        RETURN
        END		!end of subroutine REGLH
C-----------------------------------------------------------------------

C=======================================================================
C
	SUBROUTINE REGLV_E(AIN,AOUT,IM,JM,IMHV,JMHV) 

COMSTART REGLV_E
C
C	SUBROUTINE REGLV_E(AIN,AOUT,IM,JM,IMHV,JMHV) 
C
C	INPUT:
C	AIN(IM,JM)	REAL	MATRICE ENTRANTE
C	IM		INTEGER	
C	JM		INTEGER	
C	IMHV		INTEGER	
C	JMHV		INTEGER	
C
C	OUTPUT:
C	AOUT(imhv,jmhv)
C
C=======================================================================
c
c	E' l' analogo di REGLH per un campo vettoriale: vedi REGLH per
c	i commenti. La differenza sostanziale e' negli indici i,j che
c	caratterizzano i punti u,v nella griglia E-staggered, che sono
c	diversi, (Iuv=Ih+1;Juv=Jh+1), da quelli che individuano i punti 
c	h.
c
c-----------------------------------------------------------------------
COMEND
C	INCLUDE 'HIBIN$DIR:HIBUGRID.INC'
c
	DIMENSION AIN(IM,JM),AOUT(IMHV,JMHV)
c-----------------------------------------------------------------------
c
c	calcolo
c
        DO 10 J=1,JM
        DO 10 I=1,IM
c
C	south boundary
c
	   IF (J.EQ.1) THEN
	      IF(MOD(I,2).NE.0) THEN
	         IF(I.EQ.1) THEN
	            AOUT(I,J)=.5*(AIN(I,J)+AIN(I+1,J))
	         ELSE 
	            IF(I.EQ.IM) THEN
	               AOUT(I,J)=.5*(AIN(I-1,J)+AIN(I,J))
	            ELSE
	               AOUT(I,J)=.5*(AIN(I-1,J)+AIN(I+1,J))
	            END IF 
	         END IF 
	      ELSE
	         AOUT(I,J)=AIN(I,J)
	      END IF
C
C	north boundary
C
	   ELSE IF(J.EQ.JM) THEN
	      JJ=2*J-1
	      IF(MOD(I,2).NE.0) THEN
	         IF(I.EQ.1) THEN
	            AOUT(I,J)=.5*(AIN(I,J-1)+AIN(I+1,J))
	         ELSE 
	            IF(I.EQ.IM) THEN
	               AOUT(I,J)=.5*(AIN(I-1,J)+AIN(I,J-1))
	            ELSE
	               AOUT(I,JJ)=.5*(AIN(I-1,J)+AIN(I+1,J))
	            END IF
	         END IF
	      ELSE
	         AOUT(I,JJ)=AIN(I,J)
	   END IF
C
C	west boundary 
C
	   ELSE IF (I.EQ.1) THEN
	      JJ=2*J-1
              JJJ=JJ-1
	      AOUT(I,JJ)=.5*(AIN(I,J-1)+AIN(I,J))
	      AOUT(I,JJJ)=AIN(I,J)
	      IF(J.EQ.JM-1) AOUT(I,JJ+1)=AIN(I,J)
C
C	east boundary
C
	   ELSE IF (I.EQ.IM) THEN
	      JJ=2*J-1
	      JJJ=JJ-1
	      AOUT(I,JJ)=.5*(AIN(I,J-1)+AIN(I,J))
	      AOUT(I,JJJ)=AIN(I,J)
	      IF(J.EQ.JM-1) AOUT(I,JJ+1)=AIN(I,J)
C
C	inside domain
C
	   ELSE
	      JJ=2*J-1
	      IF(MOD(I,2).NE.0) THEN
	         JJJ=JJ+1
	         AOUT(I,JJ)=.25*(AIN(I-1,J)+AIN(I+1,J)+
	1				AIN(I,J-1)+AIN(I,J))
        	 AOUT(I,JJJ)=AIN(I,J)
        	 IF (J.EQ.2) AOUT(I,J)=AIN(I,J-1) 
              ELSE
                 JJJ=JJ+1
                 AOUT(I,JJ)=AIN(I,J)
        	 AOUT(I,JJJ)=.25*(AIN(I-1,J)+AIN(I+1,J)+
	1				AIN(I,J+1)+AIN(I,J))
                 IF (J.EQ.2) THEN
                    AOUT(I,J)=.25*(AIN(I,J)+AIN(I-1,J-1)+
	1				AIN(I,J-1)+AIN(I+1,J-1))
                 ENDIF    
              ENDIF
	   END IF
 10     CONTINUE
C-----------------------------------------------------------------------
        RETURN
        END		!end of subroutine REGLV
C-----------------------------------------------------------------------

C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
C **************************************************************        
      SUBROUTINE RLTLW(ALMD,APHD,TPUS,TPVS,TLM0D,DTR,CTPH0,STPH0        
     &                ,PUS,PVS)                                         
C **************************************************************        
      RELM=(ALMD-TLM0D)*DTR                                             
      SRLM=SIN(RELM)                                                    
      CRLM=COS(RELM)                                                    
C                                                                       
      APH=APHD*DTR                                                      
      SPH=SIN(APH)                                                      
      CPH=COS(APH)                                                      
C                                                                       
      CC=CPH*CRLM                                                       
      TPH=ASIN(CTPH0*SPH-STPH0*CC)                                     	
C                                                                       
      RCTPH=1./COS(TPH)                                                 
      CRAY=STPH0*SRLM*RCTPH                                             
      DRAY=(CTPH0*CPH+STPH0*SPH*CRLM)*RCTPH                             
      DC=DRAY*DRAY+CRAY*CRAY                                            
      PUS=(DRAY*TPUS+CRAY*TPVS)/DC                                      
      PVS=(DRAY*TPVS-CRAY*TPUS)/DC                                      
C                                                                       
      RETURN                                                            
      END                                                               
C-----------------------------------------------------------------------
      SUBROUTINE BILIN(A00,A10,A01,A11,PA,QA,PQA,BLA)
      BLA=A00+PA*(A10-A00)+QA*(A01-A00)+PQA*(A00-A10-A01+A11)
      RETURN
      END         


C=======================================================================
C
	SUBROUTINE  REGvet_H(AIN,AOUT,IMJM,IMHV,JMHV) 
COMSTART REGVET_H
C	SUBROUTINE  REGvet_H(AIN,AOUT,IMJM,IMHV,JMHV) 
C
C	INPUT:
C	AIN(IMJM)	REAL	MATRICE ENTRANTE
C	IMJM		INTEGER	
C	IMHV		INTEGER	
C	JMHV		INTEGER	
C
C	OUTPUT:
C	AOUT(imhv,jmhv)
CC=======================================================================
c
c	Spacchetta la matrice entrante nell'indice j: ricordiamo
c	che sulla griglia E-Arakawa staggered:
c
c      	         h   u,v   h   u,v   h   ...  u,v   h
c	j=jmt
c	        u,v   h   u,v   h   u,v  ...   h   u,v
c	        ...  ...  ...  ...  ...  ...  ...  ...
c	j
c	        ...  ...  ...  ...  ...  ...  ...  ...
c	        u,v   h   u,v   h   u,v  ...   h   u,v
c	j=2
c                h   u,v   h   u,v   h   ...  u,v   h
c
c
c	        u,v   h   u,v   h   u,v  ...   h   u,v
c	j=1
c                h   u,v   h   u,v   h   ...  u,v   h
c
c
c	       i=1   i=2  i=3  i=4  i=5   i       i=imt
c
c-----------------------------------------------------------------------
COMEND
C	INCLUDE 'HIBIN$DIR:HIBUGRID.INC'
c
	DIMENSION AIN(IMJM),AOUT(IMhv,JMhv)
c-----------------------------------------------------------------------
c
c	controllo
c
C	TYPE *,'IMJM,IMHV,JMHV',IMJM,IMHV,JMHV
c-----------------------------------------------------------------------
c
c	Calcolo

	DO 10 J=1,JMhv
	 DO 10 I=1,IMhv	

C	print*,'i,j',i,j
	 rk=float(i+(j-1)*imhv+1)/2.
	 k=rk
	 if((rk-float(k)).eq.0.)then

C	print*,'centrato!'
c		centrato!
		aout(i,j)=ain(k)

	 else if (j.eq.1.or.j.eq.jmhv)then

C	print*,'nord/sud boundary!'
C		south boundary
C		north boundary
C		print*,'k1,k2',k,k+1
		aout(i,j)=.5*(ain(k)+ain(k+1))

	 else if (i.eq.1.or.i.eq.imhv)then
		
C	print*,'est/ovest boundary!'
C		west boundary
C		east boundary
		k1=(i+(j)*imhv+1)/2
		k2=(i+(j-2)*imhv+1)/2
C		print*,'k1,k2',k1,k2
		aout(i,j)=.5*(ain(k1)+ain(k2))

	 else

C	print*,'inside domain!'
C		inside domain
		k1=(i+(j  )*imhv+1)/2
		k2=(i+(j-2)*imhv+1)/2

		k3=(i+(j-1)*imhv  )/2
		k4=(i+(j-1)*imhv+2)/2

C		print*,'k1,k2,k3,k4',k1,k2,k3,k4
		aout(i,j)=.25*(ain(k1)+ain(k2)+ain(k3)+ain(k4))
	 end if

10	continue
C-----------------------------------------------------------------------
        RETURN
        END		!end of subroutine REGvet_H
C-----------------------------------------------------------------------
C=======================================================================
C
	SUBROUTINE REGvet_V(AIN,AOUT,IMJM,IMHV,JMHV) 
COMSTART REGVET_V
C	SUBROUTINE REGvet_V(AIN,AOUT,IMJM,IMHV,JMHV) 
C
C	INPUT:
C	AIN(IM,JM)	REAL	MATRICE ENTRANTE
C	IM		INTEGER	
C	JM		INTEGER	
C	IMHV		INTEGER	
C	JMHV		INTEGER	
C
C	OUTPUT:
C	AOUT(imhv,jmhv)
C
C=======================================================================
c
c	E' l' analogo di REGLH per un campo vettoriale: vedi REGLH per
c	i commenti. La differenza sostanziale e' negli indici i,j che
c	caratterizzano i punti u,v nella griglia E-staggered, che sono
c	diversi, (Iuv=Ih+1;Juv=Jh+1), da quelli che individuano i punti 
c	h.
c
c-----------------------------------------------------------------------
COMEND
C	INCLUDE 'HIBIN$DIR:HIBUGRID.INC'
c
	DIMENSION AIN(IMJM),AOUT(IMhv,JMhv)
c-----------------------------------------------------------------------
c
c	Calcolo

	DO 10 J=1,JMhv
	 DO 10 I=1,IMhv	

C	print*,'i,j',i,j
	 rk=float(i+(j-1)*imhv)/2.
	 k=rk
	 if((rk-float(k)).eq.0.)then

C	print*,'centrato!'
c		centrato!
		aout(i,j)=ain(k)

	 else if (i.eq.1.   and.j.eq.1   )then
		k1=(i+(j-1)*imhv+1)/2
		k2=(i+j*imhv)/2
C		print*,'k1,k2',k1,k2
		aout(i,j)=.5*(ain(k1)+ain(k2))

	 else if (i.eq.imhv.and.j.eq.jmhv)then
		k1=(i+(j-1)*imhv-1)/2
		k2=(i+(j-2)*imhv)/2
C		print*,'k1,k2',k1,k2
		aout(i,j)=.5*(ain(k1)+ain(k2))

	 else if (i.eq.1.   and.j.eq.jmhv)then
		k1=(i+(j-1)*imhv+1)/2
		k2=(i+(j-2)*imhv)  /2
C		print*,'k1,k2',k1,k2
		aout(i,j)=.5*(ain(k1)+ain(k2))

	 else if (i.eq.imhv.and.j.eq.1   )then
		k1=(i+(j-1)*imhv-1)/2
		k2=(i+(j)*imhv)/2
C		print*,'k1,k2',k1,k2
		aout(i,j)=.5*(ain(k1)+ain(k2))

	 else if (j.eq.1.or.j.eq.jmhv)then

C	print*,'nord/sud boundary!'
C		south boundary
C		north boundary
C		print*,'k1,k2',k,k+1
		k1=(i+(j-1)*imhv-1)/2
		k2=(i+(j-1)*imhv+1)/2
		aout(i,j)=.5*(ain(k)+ain(k+1))

	 else if (i.eq.1.or.i.eq.imhv)then
		
C	print*,'est/ovest boundary!'
C		west boundary
C		east boundary
		k1=(i+(j  )*imhv)/2
		k2=(i+(j-2)*imhv)/2
C		print*,'k1,k2',k1,k2
		aout(i,j)=.5*(ain(k1)+ain(k2))

	 else

C	print*,'inside domain!'
C		inside domain
		k1=(i+(j  )*imhv)/2
		k2=(i+(j-2)*imhv)/2

		k3=(i+(j-1)*imhv-1)/2
		k4=(i+(j-1)*imhv+1)/2

C		print*,'k1,k2,k3,k4',k1,k2,k3,k4
		aout(i,j)=.25*(ain(k1)+ain(k2)+ain(k3)+ain(k4))
	 end if

10	continue
C-----------------------------------------------------------------------
        RETURN
        END		!end of subroutine REGvet_v
