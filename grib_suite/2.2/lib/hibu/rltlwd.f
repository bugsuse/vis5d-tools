
c    <HIBU, software di utilita' per  grid>
c    Copyright (C) 2000  <SMR ARPA>
c
c    $Date: 2000/03/27 17:04:09 $    $Revision: 1.3 $
c    $Id: rltlwd.f,v 1.3 2000/03/27 17:04:09 lambo Exp $

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
      SUBROUTINE RLTLWD(ALMD,APHD,TPUS,TPVS,TLM0D,CTPH0,STPH0    
     &                ,PUS,PVS)                                 
C **************************************************************
c Routine per antiruotare il vento; usa funzioni goniom. in gradi.
c
c almd,aphd:	coordinate vere (NON ruotate) del punto
c tpus,tpvs:	componenti ruotate del vento
c	output:
c pus,pvs:	componenti vere del vento


comstart RLTLWD
c
c SUBROUTINE RLTLWD(ALMD,APHD,TPUS,TPVS,TLM0D,CTPH0,STPH0,PUS,PVS)
c
cidx trasforma le componenti del vento dal sistema ruotato al sistema geografico
cvedi LTLWD TLLD RTLLD  rot_grib_LAMBO
c
c Trasforma le componeti u,v espresse nel sistema ruotato tipico dei modelli 
c meteo LAMBO e LOKAL a componenti u,v espresse nel sistema geografico
c
c input:
c
c  ALMD       real      longitudine sistema geografico (rapp. decimale)
c  APHD       real      latitudine sistema geografico (rapp. decimale)
c
c  tpus       real      componente u nel sistema ruotato
c  tpvs       real      componente v nel sistema ruotato
c
c  TLM0D      real      longitudine del punto intercetto del meridiano di 
c                       Greenwich con l'equatore del sistema ruotato, nel 
c                       sistema geografico
c                       
C  CTPH0      real      coseno della latitudine del punto intercetto del 
c                       meridiano di Greenwich con l'equatore del sistema 
c                       ruotato nel sistema geografico (rapp. decimale)
c  STPH0      real      seno della latitudine del punto intercetto del 
c                       meridiano di Greenwich con l'equatore del sistema 
c                       ruotato nel sistema geografico
c
c output: 
c
c  pus          real     componente u nel sistema geografico
c  pvs          real     componente v nel sistema geografico
c
comend


      RELM=(ALMD-TLM0D)                                         
      SRLM=SIND(RELM)                                           
      CRLM=COSD(RELM)                                           
C                                                               
      SPH=SIND(APHD)                                              
      CPH=COSD(APHD)                                              
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
