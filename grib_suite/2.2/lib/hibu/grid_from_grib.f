c    <HIBU, software di utilita' per  grid>
c    Copyright (C) 2000  <Paolo Patruno SMR ARPA>
c
c    $Date: 2000/04/11 10:16:19 $    $Revision: 1.2 $
c    $Id: grid_from_grib.f,v 1.2 2000/04/11 10:16:19 lambo Exp $

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

      subroutine grid_from_grib(u,idimu,rmis,alorot,alarot,
     +     idrt,ibm,
     +     tlm0d,tph0d,igrid,ier)

comstart grid_from_grib
cidx Deduce il tipo di grigliato dalle informazioni del grib e convenzioni SMR
cvedi rot_grib_lambo
c
c      subroutine grid_from_grib(u,idimu,rmis,alorot,alarot,
c     +     idrt,ibm,
c     +     tlm0d,tph0d,igrid,ier)
c
cDeduce il tipo di grigliato dalle informazioni del grib e convenzioni SMR
c
c     input:
c     u        real(idimu)   vettore dei dati da grib
c     idimu    int           dimensione vettore
c     rmis     real          valore dato mancante
c     alorot   real     longitudine del Polo Nord del sistema ruotato, nel
c                       sistema geografico come da specifiche grib
c                       (rapp. decimale)
c         
c     alarot   real     latitudine del Polo Nord del sistema
c                       ruotato nel sistema geografico come da specifiche
c                       grib (rapp. decimale)
c
c     idrt     int      data representation type        (table 6)
c     ibm      int      bit map predefinita (=0 inclusa o non utlizzata)
c
c     output:
c     tlm0d    real     longitudine del punto intercetto del meridiano di
c                       Greenwich con l'equatore del sistema ruotato, nel
c                       sistema geografico (rapp. decimale)
c
c     tph0d    real     latitudine del punto intercetto del
c                       meridiano di Greenwich con l'equatore del sistema
c                       ruotato nel sistema geografico (rapp. decimale)
c
c     igrid    int      indicatore del tipo di grigliato
c                                1       grigliato E staggherato punti H
c                                2       grigliato E staggherato punti V
c                                0       grigliato regolare (destaggherato)
c                                -1      grigliato E staggherato punti H
c                                        con valori mancanti
c                                -2      grigliato E staggherato punti V
c                                        con valori mancanti
c
c     ier      int      error code 
c                             ier=0       OK
c                             ier=1       idrt non gestita 
c
c
comend

c
c     Variabili in input
c
      real*4 rmis,alorot,alarot
      integer*4 idrt,ibm,igrid
c
c     Variabili in output
c
      real*4 tlm0d,tph0d
c
      real*4 u(idimu)


c--------------------- rotazione ----------------

      ier=0

      if(idrt.eq.10)then
         call rot_grib_LAMBO(alorot,alarot,TLM0D,TPH0D)    
      else if (idrt.eq.0)then
         tlm0d=0.
         tph0d=0.
      else
         ier=1
         print*, 'errore data rep type'
         goto 99
      end if
      
      if(ibm.eq.0)then
         if(u(1).eq.rmis.and.u(2).ne.rmis)then
            igrid=-2
         else if(u(2).eq.rmis.and.u(1).ne.rmis)then
            igrid=-1
         else
            igrid=0
         end if
      else
         if(ibm.gt.100)then
            igrid=2
         else
            igrid=1
         end if
      end if
      
 99   return
      end
