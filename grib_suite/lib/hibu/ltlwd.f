
c    <HIBU, software di utilita' per  grid>
c    Copyright (C) 2000  <SMR ARPA>
c
c    $Date: 2000/03/27 17:04:09 $    $Revision: 1.3 $
c    $Id: ltlwd.f,v 1.3 2000/03/27 17:04:09 lambo Exp $

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

C     *************************************************************
C     *                                                           *
C     *  LL TO TLL TRANSFORMATION OF VELOCITY                     *
C     *                                                           *
C     *  PROGRAMER: Z.JANJIC, YUGOSLAV FED. HYDROMET. INST.,      *
C     *             BEOGRAD                                       *
C     *                                                           *
C     *************************************************************
C	LAST MODIFICATION: ON 25 December 1992 BY Paolo Patruno at SMR
C **************************************************************
c Routine per ruotare il vento; usa funzioni goniom. in gradi.
c
c almd,aphd:	coordinate vere (NON ruotate) del punto
c u,v:		componenti vere del vento
c	output:
c tu,tv:	componenti ruotate del vento

	SUBROUTINE LTLWD(ALMD,APHD,u,v,TLM0D,CTPH0,STPH0,tu,tv)


comstart LTLWD
c
c SUBROUTINE LTLWD(ALMD,APHD,u,v,TLM0D,CTPH0,STPH0,tu,tv)
c
cidx trasforma le componenti del vento dal sistema geografico al sistema ruotato
cvedi RLTLWD TLLD RTLLD  rot_grib_LAMBO
c
c Trasforma le componeti u,v espresse nel sistema geografico
c a componenti u,v espresse nel sistema ruotato tipico dei modelli 
c meteo LAMBO e LOKAL.
c
c input:
c
c  ALMD       real      longitudine sistema geografico (rapp. decimale)
c  APHD       real      latitudine sistema geografico (rapp. decimale)
c
c  u          real     componente u nel sistema geografico
c  v          real     componente v nel sistema geografico
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
c  tu         real     componente u nel sistema ruotato
c  tv         real     componente v nel sistema ruotato
c
comend

	call LTLW1(ALMD,APHD,TLM0D,CTPH0,STPH0,DRAY,CRAY)
	call LTLW2(U,V,TU,TV,DRAY,CRAY)

	return
	end

      SUBROUTINE LTLW1(ALMD,APHD,TLM0D,CTPH0,STPH0
     2                 ,DRAY,CRAY)
C
      RELM=(ALMD-TLM0D)
      SRLM=SIND(RELM)
      CRLM=COSD(RELM)
C
      SPH=SIND(APHD)
      CPH=COSD(APHD)
C
      CC=CPH*CRLM
C
      TPH=ASIN(CTPH0*SPH-STPH0*CC)
C
      RCTPH=1./COS(TPH)
C
      CRAY=STPH0*SRLM*RCTPH
      DRAY=(CTPH0*CPH+STPH0*SPH*CRLM)*RCTPH
C
	RETURN
      END


      SUBROUTINE LTLW2(PUS,PVS
     2                 ,TPUS,TPVS,DRAY,CRAY)

      TPUS=DRAY*PUS-CRAY*PVS
      TPVS=CRAY*PUS+DRAY*PVS
C
      RETURN
      END

