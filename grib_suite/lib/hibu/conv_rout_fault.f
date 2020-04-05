
c    <HIBU, software di utilita' per  grid>
c    Copyright (C) 2000  <Paolo Patruno SMR ARPA>
c
c    $Date: 2000/03/27 17:04:08 $    $Revision: 1.2 $
c    $Id: conv_rout_fault.f,v 1.2 2000/03/27 17:04:08 lambo Exp $

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
	SUBROUTINE  REGvet_H_F(AIN,AOUT,IMJM,IMHV,JMHV,fault) 
COMSTART REGVET_H_F
C	SUBROUTINE  REGvet_H_F(AIN,AOUT,IMJM,IMHV,JMHV,fault) 
C
C	INPUT:
C	AIN(IMJM)	REAL	MATRICE ENTRANTE
C	IMJM		INTEGER	
C	IMHV		INTEGER	
C	JMHV		INTEGER	
c	fault		real	dato mancate
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

	   aout(i,j)=fault
C	   print*,'i,j',i,j
	   rk=float(i+(j-1)*imhv+1)/2.
	   k=rk
	   if((rk-float(k)).eq.0.)then
C		print*,'centrato!'
c		centrato!
		aout(i,j)=ain(k)
	   else if (j.eq.1.or.j.eq.jmhv)then
C		print*,'nord/sud boundary!'
C		south boundary
C		north boundary
C		print*,'k1,k2',k,k+1

		if(ain(k).eq.fault.or.ain(k+1).eq.fault)goto 10
		aout(i,j)=.5*(ain(k)+ain(k+1))

	   else if (i.eq.1.or.i.eq.imhv)then
		
C		print*,'est/ovest boundary!'
C		west boundary
C		east boundary
		k1=(i+(j)*imhv+1)/2
		k2=(i+(j-2)*imhv+1)/2
C		print*,'k1,k2',k1,k2

		if(ain(k1).eq.fault.or.ain(k2).eq.fault)goto 10
		aout(i,j)=.5*(ain(k1)+ain(k2))

	   else

C		print*,'inside domain!'
C		inside domain
		k1=(i+(j  )*imhv+1)/2
		k2=(i+(j-2)*imhv+1)/2

		k3=(i+(j-1)*imhv  )/2
		k4=(i+(j-1)*imhv+2)/2

C		print*,'k1,k2,k3,k4',k1,k2,k3,k4

		if(ain(k1).eq.fault.or.ain(k2).eq.fault.or.
	1	ain(k3).eq.fault.or.ain(k4).eq.fault)goto 10

		aout(i,j)=.25*(ain(k1)+ain(k2)+ain(k3)+ain(k4))
	   end if

10	continue
C-----------------------------------------------------------------------
        RETURN
        END		!end of subroutine REGvet_H
C-----------------------------------------------------------------------
C=======================================================================
C
	SUBROUTINE REGvet_V_F(AIN,AOUT,IMJM,IMHV,JMHV,fault) 
COMSTART REGVET_V_F
C	SUBROUTINE REGvet_V_F(AIN,AOUT,IMJM,IMHV,JMHV,fault) 
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

	  aout(i,j)=fault
C	  print*,'i,j',i,j
	  rk=float(i+(j-1)*imhv)/2.
	  k=rk
	  if((rk-float(k)).eq.0.)then

C		print*,'centrato!'
c		centrato!
		aout(i,j)=ain(k)

	  else if (i.eq.1.   and.j.eq.1   )then
		k1=(i+(j-1)*imhv+1)/2
		k2=(i+j*imhv)/2
C		print*,'k1,k2',k1,k2
		if(ain(k1).eq.fault.or.ain(k2).eq.fault)goto 10
		aout(i,j)=.5*(ain(k1)+ain(k2))

	  else if (i.eq.imhv.and.j.eq.jmhv)then
		k1=(i+(j-1)*imhv-1)/2
		k2=(i+(j-2)*imhv)/2
C		print*,'k1,k2',k1,k2
		if(ain(k1).eq.fault.or.ain(k2).eq.fault)goto 10
		aout(i,j)=.5*(ain(k1)+ain(k2))

	  else if (i.eq.1.   and.j.eq.jmhv)then
		k1=(i+(j-1)*imhv+1)/2
		k2=(i+(j-2)*imhv)  /2
C		print*,'k1,k2',k1,k2
		if(ain(k1).eq.fault.or.ain(k2).eq.fault)goto 10
		aout(i,j)=.5*(ain(k1)+ain(k2))

	  else if (i.eq.imhv.and.j.eq.1   )then
		k1=(i+(j-1)*imhv-1)/2
		k2=(i+(j)*imhv)/2
C		print*,'k1,k2',k1,k2
		if(ain(k1).eq.fault.or.ain(k2).eq.fault)goto 10
		aout(i,j)=.5*(ain(k1)+ain(k2))

	  else if (j.eq.1.or.j.eq.jmhv)then

C		print*,'nord/sud boundary!'
C		south boundary
C		north boundary
C		print*,'k1,k2',k,k+1
		k1=(i+(j-1)*imhv-1)/2		!cosa me ne faccio
		k2=(i+(j-1)*imhv+1)/2		!cosa me ne faccio

		if(ain(k).eq.fault.or.ain(k+1).eq.fault)goto 10
		aout(i,j)=.5*(ain(k)+ain(k+1))

	  else if (i.eq.1.or.i.eq.imhv)then
		
C		print*,'est/ovest boundary!'
C		west boundary
C		east boundary
		k1=(i+(j  )*imhv)/2
		k2=(i+(j-2)*imhv)/2
C		print*,'k1,k2',k1,k2
		if(ain(k1).eq.fault.or.ain(k2).eq.fault)goto 10
		aout(i,j)=.5*(ain(k1)+ain(k2))

	  else

C		print*,'inside domain!'
C		inside domain
		k1=(i+(j  )*imhv)/2
		k2=(i+(j-2)*imhv)/2
		k3=(i+(j-1)*imhv-1)/2
		k4=(i+(j-1)*imhv+1)/2

C		print*,'k1,k2,k3,k4',k1,k2,k3,k4

		if(ain(k1).eq.fault.or.ain(k2).eq.fault.or.
	1	ain(k3).eq.fault.or.ain(k4).eq.fault)goto 10

		aout(i,j)=.25*(ain(k1)+ain(k2)+ain(k3)+ain(k4))
	  end if

10	continue
C-----------------------------------------------------------------------
        RETURN
        END		!end of subroutine REGvet_v
