/*
c    <picogrib software di decodifica grib>
c    Copyright (C) 2000  <SMR ARPA e Picodata SRL>
c
c    $Date: 2005-11-24 13:43:01 +0100 (gio, 24 nov 2005) $    $Revision: 50 $
c    $Id: showgrib.c 50 2005-11-24 12:43:01Z cvs $

c    Originally written by Marco Mazzola and Massimo Bider
c    Picodata SRL mailto:m.bider@picodata.it

c    Revised by Davide Cesari and Paolo Patruno
c    ARPA SMR 
c    mailto:p.patruno@smr.arpa.emr.it
c    mailto:dinamici@smr.arpa.emr.it

c    Questo programma ÅË software  libero; ÅË lecito ridistribuirlo e/o
c    modificarlo secondo i  termini della Licenza Pubblica Generica SMR 
c    come pubblicata  da ARPA SMR ; riferirsi alla versione 1
c    della licenza o (a scelta) ad una versione successiva.

c    Questo programma ÅË distribuito  nella speranza che sia utile,  ma
c    SENZA  ALCUNA GARANZIA;  senza  neppure la  garanzia  implicita di
c    COMMERCIABILITA' o di APPLICABILITÅ¿ PER UN PARTICOLARE SCOPO.  Si
c    veda la Licenza Pubblica Generica SMR per avere maggiori dettagli.

c    Ognuno dovrebbe avere ricevuto una copia della Licenza Pubblica
c    Generica SMR insieme a questo programma; in caso contrario, la si
c    puÅÚ ottenere da Agenzia Regionale Prevenzione e Ambiente (ARPA)
c    Servizio Meteorologico Regionale (SMR), Viale Silvani 6, 40122
c    Bologna, Italia
c    http://www.smr.arpa.emr.it
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "constant.h"
#include "struct.h"
#include "griblib.h"


main(argc,argv)
int argc;
char *argv[];
{

  FILE *grib_file;
  struct grib_type  grib_msg;
  unsigned char     **grib_buffer;
  unsigned char     *middle_pointer;
  unsigned char     *saved_pointer;
  gribfloat             *no_bit_value;
  long int          status;
  long int          ignore_bitmap;
  long int          show_lat_long;


  grib_buffer = &middle_pointer;

  if ((argc < 2) || (argc > 4))
  {
    printf("Usage:  SHOWGRIB  gribfile  [display-flag [no-bit-value]]\n");
    printf("Example SHOWGRIB  grib.dat         y          9999999\n");
    return ABORT_STATUS;
  }
  if ((grib_file = fopen(argv[1],"rb")) == 0)
  {
    printf("File not found\n");
    return ABORT_STATUS;
  }
  show_lat_long = FALSE;
  if (argc > 2)
  {
    if ( (argv[2][0] == 'y') || (argv[2][0] == 'Y') )
    {
      show_lat_long = TRUE;
      ignore_bitmap = FALSE;
    }
  }
  no_bit_value = 0;
  if (argc == 4)
  {
    no_bit_value = (gribfloat * ) malloc(sizeof(gribfloat));
    *no_bit_value = (gribfloat) atof(argv[3]);
  }
  while( (status = load_grib_file(grib_file, grib_buffer)) == NORMAL_STATUS)
  {
    saved_pointer = *grib_buffer;
    if (no_bit_value == 0)
    {
      gribfloat inu;

      inu = (gribfloat) NO_BIT_VALUE_DEFAULT;
      status = degrib(GRIB_DECODE, grib_buffer, ignore_bitmap,
		      &grib_msg, &inu);
    }
    else
      status = degrib(GRIB_DECODE, grib_buffer, ignore_bitmap,
		      &grib_msg, no_bit_value);
    summarize_grib(grib_msg,show_lat_long,no_bit_value);
    free(saved_pointer);
    if (grib_msg.section1.forecasts_list != 0)
      free(grib_msg.section1.forecasts_list);
    if (grib_msg.section3.bitmap != 0)
      free(grib_msg.section3.bitmap);
    if (grib_msg.section4.binary_data != 0)
      free(grib_msg.section4.binary_data);
    if (status != NORMAL_STATUS) return status;
  }
  fclose(grib_file);
  if (no_bit_value != 0) free(no_bit_value);

  return NORMAL_STATUS;
}
