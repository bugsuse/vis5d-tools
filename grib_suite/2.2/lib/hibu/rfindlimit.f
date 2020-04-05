
c    <HIBU, software di utilita' per  grid>
c    Copyright (C) 2000  <Paolo Patruno SMR ARPA>
c
c    $Date: 2000/03/27 17:04:09 $    $Revision: 1.2 $
c    $Id: rfindlimit.f,v 1.2 2000/03/27 17:04:09 lambo Exp $

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

	SUBROUTINE RFINDLIMIT(ALOMINHIROT,ALOMAXHIROT
	1	      ,ALAMINHIROT,ALAMAXHIROT
	1	      ,NLON,NLAT	
	1	      ,TLM0D,TPH0D	
	1	      ,alomin
	1	      ,alomax
	1	      ,alamin
	1	      ,alamax)

C	INPUT PARAMETERS:
C		HIBU_AREA ........valori minimi e massimi in lon/lat
C				  geografiche dell'area
C
C	OUTPUT PARAMETERS:
C		ALOMINHIROT.......coordinate ruotate
C		TLM0D,TPH0D
C


      	STPH0=SIND(TPH0D)
      	CTPH0=COSD(TPH0D)
	DX=(ALOMAX-ALOMIN)/(NLON-1)
	DY=(ALAMAX-ALAMIN)/(NLAT-1)

	ALOMAXHIROT=-999.	
	ALOMINHIROT= 999.
	ALAMAXHIROT=-999.	
	ALAMINHIROT= 999.

	DO 55 I=1,NLON
	  J=1
	  ELON=(ALOMIN+(I-1)*DX)
	  ELAT=(ALAMIN+(J-1)*DY)
	  CALL TLLD(ELON,ELAT,TLM0D,CTPH0,STPH0,TLOND,TLATD)      
	  ALOMINHIROT=MIN(TLOND,ALOMINHIROT)
	  ALOMAXHIROT=MAX(TLOND,ALOMAXHIROT)
    	  ALAMINHIROT=MIN(TLATD,ALAMINHIROT)
       	  ALAMAXHIROT=MAX(TLATD,ALAMAXHIROT)
55	CONTINUE
	DO 56 J=1,NLAT
	  I=1
	  ELON=(ALOMIN+(I-1)*DX)
	  ELAT=(ALAMIN+(J-1)*DY)
	  CALL TLLD(ELON,ELAT,TLM0D,CTPH0,STPH0,TLOND,TLATD)      
	  ALOMINHIROT=MIN(TLOND,ALOMINHIROT)
	  ALOMAXHIROT=MAX(TLOND,ALOMAXHIROT)
    	  ALAMINHIROT=MIN(TLATD,ALAMINHIROT)
       	  ALAMAXHIROT=MAX(TLATD,ALAMAXHIROT)
56	CONTINUE

	DO 57 I=1,NLON
	  J=NLAT
	  ELON=(ALOMIN+(I-1)*DX)
	  ELAT=(ALAMIN+(J-1)*DY)
	  CALL TLLD(ELON,ELAT,TLM0D,CTPH0,STPH0,TLOND,TLATD)      
	  ALOMINHIROT=MIN(TLOND,ALOMINHIROT)
	  ALOMAXHIROT=MAX(TLOND,ALOMAXHIROT)
    	  ALAMINHIROT=MIN(TLATD,ALAMINHIROT)
       	  ALAMAXHIROT=MAX(TLATD,ALAMAXHIROT)
57	CONTINUE
	DO 58 J=1,NLAT
	  I=NLON
	  ELON=(ALOMIN+(I-1)*DX)
	  ELAT=(ALAMIN+(J-1)*DY)
	  CALL TLLD(ELON,ELAT,TLM0D,CTPH0,STPH0,TLOND,TLATD)      
	  ALOMINHIROT=MIN(TLOND,ALOMINHIROT)
	  ALOMAXHIROT=MAX(TLOND,ALOMAXHIROT)
    	  ALAMINHIROT=MIN(TLATD,ALAMINHIROT)
       	  ALAMAXHIROT=MAX(TLATD,ALAMAXHIROT)
58	CONTINUE

	RETURN
	END
