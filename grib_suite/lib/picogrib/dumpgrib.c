/*
c    <picogrib software di decodifica grib>
c    Copyright (C) 2000  <SMR ARPA e Picodata SRL>
c      $Date: 2000/05/09 12:49:49 $    $Revision: 1.6 $
c      $Id: dumpgrib.c,v 1.6 2000/05/09 12:49:49 lambo Exp $

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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "constant.h"
#include "struct.h"
#include "griblib.h"


main(argc, argv)
int argc;
char *argv[];
{

  FILE *grib_file;
  struct grib_type  grib_msg;
  unsigned char     **grib_buffer;
  unsigned char     *middle_pointer;
  unsigned char     *saved_pointer;
  gribfloat		    no_bit_value;
  long int	    status;
  long int	    ignore_bitmap;


  grib_buffer = &middle_pointer;

  if ((argc < 2) || (argc > 3))
  {
    printf("Usage:  DUMPGRIB  gribfile  [no-bit-value]\n");
    printf("Example DUMPGRIB  grib.dat     9999999\n");
    return ABORT_STATUS;
  }
  if ((grib_file = fopen(argv[1],"rb")) == 0)
  {
    printf("File not found\n");
    return ABORT_STATUS;
  }
  if (argc == 3)
  {
    no_bit_value = (gribfloat) atof(argv[2]);
    ignore_bitmap = FALSE;
  }
  else
  {
    no_bit_value = (gribfloat) 0.;
    ignore_bitmap = TRUE;
  }
  while( (status = load_grib_file(grib_file, grib_buffer)) == NORMAL_STATUS)
  {
    saved_pointer = *grib_buffer;
    status = degrib(GRIB_DECODE, grib_buffer, ignore_bitmap,
                    &grib_msg, &no_bit_value);
    free(saved_pointer);
    print_grib(GRIB_INFO, grib_msg);
    print_grib(GRIB_DECODE, grib_msg);
    if (grib_msg.section1.forecasts_list != 0)
      free(grib_msg.section1.forecasts_list);
    if (grib_msg.section3.bitmap != 0)
      free(grib_msg.section3.bitmap);
    if (grib_msg.section4.binary_data != 0)
      free(grib_msg.section4.binary_data);
    if (status != NORMAL_STATUS) return status;
  }
  fclose(grib_file);

  return NORMAL_STATUS;
}
