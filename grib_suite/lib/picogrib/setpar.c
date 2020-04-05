/*
c    <picogrib software di decodifica grib>
c    Copyright (C) 2000  <SMR ARPA>
c      $Date: 2000/04/28 12:54:33 $    $Revision: 1.4 $
c      $Id: setpar.c,v 1.4 2000/04/28 12:54:33 lambo Exp $

c    by Davide Cesari and Paolo Patruno
c    ARPA SMR 
c    mailto:p.patruno@smr.arpa.emr.it
c    mailto:dinamici@smr.arpa.emr.it

c    Questo programma è software  libero; è lecito ridistribuirlo e/o
c    modificarlo secondo i  termini della Licenza Pubblica Generica SMR 
c    come pubblicata  da ARPA SMR ; riferirsi alla versione 1
c    della licenza o (a scelta) ad una versione successiva.

c    Questo programma è distribuito  nella speranza che sia utile,  ma
c    SENZA  ALCUNA GARANZIA;  senza  neppure la  garanzia  implicita di
c    COMMERCIABILITA' o di APPLICABILITÀ PER UN PARTICOLARE SCOPO.  Si
c    veda la Licenza Pubblica Generica SMR per avere maggiori dettagli.

c    Ognuno dovrebbe avere ricevuto una copia della Licenza Pubblica
c    Generica SMR insieme a questo programma; in caso contrario, la si
c    può ottenere da Agenzia Regionale Prevenzione e Ambiente (ARPA)
c    Servizio Meteorologico Regionale (SMR), Viale Silvani 6, 40122
c    Bologna, Italia
c    http://www.smr.arpa.emr.it
*/

#include "struct.h"

#ifdef UNDERSCORE
void setpar_
#else
#  ifdef _CRAY
void SETPAR
#  else
void setpar
#  endif
#endif
(gribint* kbit, gribint* kneg,gribint* kpr)
{*kbit=sizeof(gribfloat)*8;
 *kneg=0xffffffff;
}
