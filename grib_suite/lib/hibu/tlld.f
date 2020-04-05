
c    <HIBU, software di utilita' per  grid>
c    Copyright (C) 2000  <SMR ARPA>
c
c    $Date: 2000/03/27 17:04:10 $    $Revision: 1.3 $
c    $Id: tlld.f,v 1.3 2000/03/27 17:04:10 lambo Exp $

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

c	rotazione di coordinate

      SUBROUTINE TLLD(ALO1,ALA1,TLM0D,CTPH0,STPH0,ALO2,ALA2)

comstart TLLD
c
c TLLD (ALO1,ALA1,TLM0D,CTPH0,STPH0,ALO2,ALA2)
c
cidx trasforma le coordinate geografiche in coordinate nel sistema ruotato
cvedi RTLLD LTLWD RLTLWD rot_grib_LAMBO
c
c Trasforma le coordinate geografiche lat lon in coordinate nel 
c sistema ruotato tipico dei modelli meteo LAMBO e LOKAL.
c
cinput:
c
c  ALO1       real      longitudine sistema geografico (rapp. decimale)
c  ALA1       real      latitudine sistema geografico (rapp. decimale)
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
c  ALO2       real     longitudine nel sistema ruotato (rapp. decimale)
c  ALA2       real     latitudine nel sistema ruotato (rapp. decimale)
c
comend

C-----------------------------------------------------------------------01700000
	RELM=(ALO1-TLM0D)
	SRLM=SIND(RELM)           
	CRLM=COSD(RELM)           
C                              
	SPH=SIND(ALA1)             
	CPH=COSD(ALA1)             
C                              
	CC=CPH*CRLM              
	ANUM=CPH*SRLM            
	DENOM=CTPH0*CC+STPH0*SPH 
C-----------------------------------------------------------------------01820000
                   ALO2=ATAN2D(ANUM,DENOM)       
                   ALA2=ASIND(CTPH0*SPH-STPH0*CC)
C-----------------------------------------------------------------------01850000
	RETURN             
	END
