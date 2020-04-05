/*
c    <picogrib software di decodifica grib>
c    Copyright (C) 2000  <SMR ARPA>
c
c    $Date: 2000/03/28 15:28:55 $    $Revision: 1.2 $
c    $Id: constant.h,v 1.2 2000/03/28 15:28:55 lambo Exp $

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
#define GRIB_INFO     'I'    /* Only section 0, 1 and 2 */
#define GRIB_DECODE   'D'    /* Complete decoding */

/* Status codes returned by non-void griblib functions */
#define NORMAL_STATUS  0
#define ABORT_STATUS   (-1)
#define EOF_STATUS		(-2)

#define NO_BIT_VALUE_DEFAULT   1234567890

