/*
c    <picogrib software di decodifica grib>
c    Copyright (C) 2000  <SMR ARPA>
c
c    $Date: 2007-02-23 13:07:07 +0100 (ven, 23 feb 2007) $    $Revision: 61 $
c    $Id: constant.h 61 2007-02-23 12:07:07Z cesari $

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
*/
#define TRUE  -1
#define FALSE  0

/* Codes of information groups to be extracted by Degrib and printed
   with Print_grib */
#define GRIB_LENGTH   'L'    /* Only section lengths */
#define GRIB_MOREINFO 'J'    /* Only section 0, 1 and 2 and integer 4 */
#define GRIB_INFO     'I'    /* Only section 0, 1 and 2 */
#define GRIB_DECODE   'D'    /* Complete decoding */

/* Status codes returned by non-void griblib functions */
#define NORMAL_STATUS  0
#define ABORT_STATUS   (-1)
#define EOF_STATUS		(-2)

#define NO_BIT_VALUE_DEFAULT   1234567890

#define FO_SET "ogvpyYmdhMutTciVlLsNG%"
#define FO_CONG "%.3g/%.4Y%.2m%.2d%.2h/%.3c/v_%v,%p,l_%V,%l,%L.grb"
#define MAXFORMLEN 12
