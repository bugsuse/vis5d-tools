
c    <HIBU, software di utilita' per  grid>
c    Copyright (C) 2000  <Paolo Patruno SMR ARPA>
c
c    $Date: 2000/03/27 17:04:09 $    $Revision: 1.3 $
c    $Id: rot_grib_lambo.f,v 1.3 2000/03/27 17:04:09 lambo Exp $

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

C **************************************************************
      SUBROUTINE rot_grib_LAMBO(ALO2,ALA2,TLM0D,TPH0D)    
C **************************************************************

comstart rot_grib_LAMBO
cidx converte i parametri di rotazione espressi nel grib a quelli necessari in lambo
cvedi tlld rtlld ltlwd rltlwd 
c
c      SUBROUTINE rot_grib_LAMBO(ALO2,ALA2,TLM0D,TPH0D)    
c
c Converte i parametri di rotazione come da specifiche grib a quelli necessari
c nella libreria di utilita' di lambo. Il grib infatti esprime i parametri di 
c rotazione in riferimento al Polo Nord mentre lambo utilizza l'intersezione
c del meridiano di greenwich con l'equatore. Il terzo parametro di rotazione
c presente nel grib non viene considerato e deve essere pari a zero.
c
c input:
c  alo2      real       longitudine del Polo Nord del sistema ruotato, nel 
c                       sistema geografico come da specifiche grib 
c                       (rapp. decimale)
c                       
C  ala2      real       latitudine del Polo Nord del sistema 
c                       ruotato nel sistema geografico come da specifiche 
c                       grib (rapp. decimale)
c
c
c output:
c  TLM0D      real      longitudine del punto intercetto del meridiano di 
c                       Greenwich con l'equatore del sistema ruotato, nel 
c                       sistema geografico (rapp. decimale)
c                       
C  TPH0      real       latitudine del punto intercetto del 
c                       meridiano di Greenwich con l'equatore del sistema 
c                       ruotato nel sistema geografico (rapp. decimale)
c
comend

c	ALA2=ASIND(-cosd(TPH0D))	!da risolvere come soluzione particolare
c	alo1=0.
c	ala1=-90

	tph0D=acosd(-sind(ala2))
	TLM0D=alo2


c	ALA2=ASIND(sind(TPH0D)*b+Cosd(TPH0D)*c)	!da risolvere come soluzione
c						!generica
c
c	STPH=SIND(ALA1)       ! = -1
c	CTPH=COSD(ALA1)       ! =  0
c	CTLM=COSD(ALO1)       ! =  1
c	STLM=SIND(ALO1)       ! =  0              
c	CPH =COSD(ALA2)                        
C                                          
c	TLM0D=alo2-ASIND(STLM*CTPH/CPH)      
c
c	d=sind(ala2)
c	b=CTPH*CTLM		!=0
c	c=STPH			!=-1
c
c	t1=(b-sqrt(b*b-d*d+c*c))/(c+d)
c	t2=(b+sqrt(b*b-d*d+c*c))/(c+d)
c
c	tph0D1=2*atan2d((b-sqrt(b*b-d*d+c*c)),(c+d))
c	tph0D2=2*atan2d((b+sqrt(b*b-d*d+c*c)),(c+d))
C             
      RETURN  
      END     
