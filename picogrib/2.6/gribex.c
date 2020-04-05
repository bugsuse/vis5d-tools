/*
c    <picogrib software di decodifica grib>
c    Copyright (C) 2000  <SMR ARPA e Picodata SRL>
c
c    $Date: 2005-11-24 13:43:01 +0100 (gio, 24 nov 2005) $    $Revision: 50 $
c    $Id: gribex.c 50 2005-11-24 12:43:01Z cvs $

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


 main(argc, argv)
int argc;
char *argv[];
{

  struct grib_type  grib_msg;
  gribfloat		    no_bit_value;
  unsigned char     **grib_buffer;
  unsigned char     *middle_pointer;
  unsigned char     *saved_pointer;
  FILE		    *grib_file;
  long int	    status;
  long int	    ignore_bitmap;
  long int ksec0[2];
  long int ksec1[310];
  long int ksec2[100];
  long int ksec3[2];
  long int ksec4[60];
  gribfloat psec2[100];
  gribfloat psec3[2];
  gribfloat psec4[4000];
  long int  klenp;
  char kgrib[5500];
  long int  kleng;
  long int  kword;
  char hoper;
  long int  kret;
  int	i;


  grib_buffer = &middle_pointer;

  if (argc !=2)
  {
    printf("Usage:  GRIBEX  gribfile\n");
    printf("Example GRIBEX  grib.dat\n");
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
    status = degrib(GRIB_LENGTH, grib_buffer, ignore_bitmap,
                    &grib_msg, &no_bit_value);
    kleng = grib_msg.section0.total_length;
    hoper = 'D';
    psec3[1] = 999999.0;
    memcpy(kgrib,saved_pointer,(size_t) kleng);

#ifdef _CRAY
    F77_FUNC(gribex,GRIBEX)(ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4,psec4,&klenp,
			    kgrib,&kleng,&kword,&hoper,1,&kret);
#else
    F77_FUNC(gribex,GRIBEX)(ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4,psec4,&klenp,
			    kgrib,&kleng,&kword,&hoper,&kret,1);
#endif

    free(saved_pointer);

    for (i=0;i<=1;i++)
      printf("ksec0[%d] = %15ld \n",i,ksec0[i]);
    for (i=0;i<=52;i++)
      printf("ksec1[%d] = %15ld \n",i,ksec1[i]);
    for (i=0;i<=22;i++)
      printf("ksec2[%d] = %15ld \n",i,ksec2[i]);
    for (i=0;i<=1;i++)
      printf("ksec3[%d] = %15ld \n",i,ksec3[i]);
    for (i=0;i<=10;i++)
      printf("ksec4[%d] = %15ld \n",i,ksec4[i]);
    for (i=0;i<=1;i++)
      printf("psec2[%d] = %15f \n",i,psec2[i]);
    for (i=0;i<=1;i++)
      printf("psec3[%d] = %15f \n",i,psec3[i]);
    for (i=0;i<=9;i++)
      printf("psec4[%d] = %15f \n",i,psec4[i]);
    printf("klenp = %15ld \n",klenp);
    printf("kleng = %15ld \n",kleng);
    printf("kret = %15ld \n",kret);

    if (status != NORMAL_STATUS) return status;

  }
  fclose(grib_file);

  return NORMAL_STATUS;
}
