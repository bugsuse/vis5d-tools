/*
c    <picogrib software di decodifica grib>
c    Copyright (C) 2000  <SMR ARPA>
c      $Date: 2005-11-24 13:43:01 +0100 (gio, 24 nov 2005) $    $Revision: 50 $
c      $Id: setpar.c 50 2005-11-24 12:43:01Z cvs $

c    by Davide Cesari and Paolo Patruno
c    ARPA SMR 
c    mailto:p.patruno@smr.arpa.emr.it
c    mailto:dinamici@smr.arpa.emr.it

c    Questo programma � software  libero; � lecito ridistribuirlo e/o
c    modificarlo secondo i  termini della Licenza Pubblica Generica SMR 
c    come pubblicata  da ARPA SMR ; riferirsi alla versione 1
c    della licenza o (a scelta) ad una versione successiva.

c    Questo programma � distribuito  nella speranza che sia utile,  ma
c    SENZA  ALCUNA GARANZIA;  senza  neppure la  garanzia  implicita di
c    COMMERCIABILITA' o di APPLICABILIT� PER UN PARTICOLARE SCOPO.  Si
c    veda la Licenza Pubblica Generica SMR per avere maggiori dettagli.

c    Ognuno dovrebbe avere ricevuto una copia della Licenza Pubblica
c    Generica SMR insieme a questo programma; in caso contrario, la si
c    pu� ottenere da Agenzia Regionale Prevenzione e Ambiente (ARPA)
c    Servizio Meteorologico Regionale (SMR), Viale Silvani 6, 40122
c    Bologna, Italia
c    http://www.smr.arpa.emr.it
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "struct.h"

void F77_FUNC(setpar,SETPAR) (gribint* kbit, gribint* kneg,gribint* kpr)
{*kbit=sizeof(gribfloat)*8;
 *kneg=0xffffffff;
}
