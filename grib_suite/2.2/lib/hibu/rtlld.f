
c    <HIBU, software di utilita' per  grid>
c    Copyright (C) 2000  <SMR ARPA>
c
c    $Date: 2000/03/27 17:04:09 $    $Revision: 1.3 $
c    $Id: rtlld.f,v 1.3 2000/03/27 17:04:09 lambo Exp $

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

c	antirotazione di coordinate

C **************************************************************
      SUBROUTINE RTLLD(ALO1,ALA1,TLM0D,CTPH0,STPH0,ALO2,ALA2)    
C **************************************************************

comstart RTLLD
c
c SUBROUTINE RTLLD (ALO1,ALA1,TLM0D,CTPH0,STPH0,ALO2,ALA2)
c
cidx trasforma le coordinate dal sistema ruotato al sistema geografico.
cvedi TLLD LTLWD RLTLWD rot_grib_LAMBO
c Trasforma le coordinate lat lon ruotate, tipiche dei modelli meteo LAMBO e
c LOKAL, in coordinate nel sistema geografico.
c
c input:
c
c  ALO1       real     longitudine nel sistema ruotato (rapp. decimale)
c  ALA1       real     latitudine nel sistema ruotato (rapp. decimale)
c  TLM0D      real      longitudine del punto intercetto del meridiano di 
c                       Greenwich con l'equatore del sistema ruotato, nel 
c                       sistema geografico (rapp. decimale)
c                       
C  CTPH0      real      coseno della latitudine del punto intercetto del 
c                       meridiano di Greenwich con l'equatore del sistema 
c                       ruotato nel sistema geografico
c  STPH0      real      seno della latitudine del punto intercetto del 
c                       meridiano di Greenwich con l'equatore del sistema 
c                       ruotato nel sistema geografico
c
c output: 
c
c  ALO2       real      longitudine sistema geografico (rapp. decimale)
c  ALA2       real      latitudine sistema geografico (rapp. decimale)
c
comend

      STPH=SIND(ALA1)                     
      CTPH=COSD(ALA1)                     
      CTLM=COSD(ALO1)                      
      STLM=SIND(ALO1)                      
C                                          
      ALA2=ASIND(STPH0*CTPH*CTLM+CTPH0*STPH)
      CPH=COSD(ALA2)                        
C                                          
      ALO2=TLM0D+ASIND(STLM*CTPH/CPH)      
C             
      RETURN  
      END     
