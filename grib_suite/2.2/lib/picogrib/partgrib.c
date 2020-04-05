/*
c    <picogrib software di decodifica grib>
c    Copyright (C) 2000  <SMR ARPA e Picodata SRL>
c
c    $Date: 2000/05/09 12:49:50 $    $Revision: 1.4 $
c    $Id: partgrib.c,v 1.4 2000/05/09 12:49:50 lambo Exp $

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
  float		    no_bit_value;
  long int	    status;
  long int	    ignore_bitmap;


  grib_buffer = &middle_pointer;

  if (argc !=2)
  {
    printf("Usage:  PARTGRIB  gribfile\n");
    printf("Example PARTGRIB  grib.dat\n");
    return ABORT_STATUS;
  }
  if ((grib_file = fopen(argv[1],"rb")) == 0)
  {
    printf("File not found\n");
    return ABORT_STATUS;
  }
  ignore_bitmap = TRUE;
  while( (status = load_grib_file(grib_file, grib_buffer)) == NORMAL_STATUS)
  {

    saved_pointer = *grib_buffer;
    status = degrib(GRIB_INFO, grib_buffer, ignore_bitmap,
                    &grib_msg, &no_bit_value);
    status = save_grib_file(saved_pointer,
			    grib_msg.section0.total_length, grib_msg);
    free(saved_pointer);

    if (status != NORMAL_STATUS) return status;

  }
  fclose(grib_file);

  return NORMAL_STATUS;
}
