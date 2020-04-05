/*
c    <picogrib software di decodifica grib>
c    Copyright (C) 2000  <SMR ARPA e Picodata SRL>
c
c    $Date: 2005-11-24 15:28:03 +0100 (gio, 24 nov 2005) $    $Revision: 53 $
c    $Id: partgrib.c 53 2005-11-24 14:28:03Z cvs $

c    Originally written by Marco Mazzola and Massimo Bider
c    Picodata SRL mailto:m.bider@picodata.it

c    Revised by Davide Cesari and Paolo Patruno
c    ARPA SMR 
c    mailto:ppatruno@smr.arpa.emr.it
c    mailto:dcesari@smr.arpa.emr.it

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
c    http://www.arpa.emr.it/sim
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#ifdef HAVE_GETOPT_LONG
#include <getopt.h>
#else
#include <unistd.h>
#endif
#include <sys/stat.h>
#include <errno.h>
#include "constant.h"
#include "struct.h"
#include "griblib.h"

#define ST_TRAD 0
#define ST_FORM 1

char file_name[1024]="";
int parents=0, append=0;
struct partgrib_filename *first_file=NULL;

void usage(char *prog) {
  printf("PICOGRIB: http://picogrib.sf.net/\n");
  printf("%s: spezza il contenuto di ciascun grib file in tanti file,\n"
 "uno per ogni record grib.\n\n",prog);
  printf("Uso: %s [opzioni] grib1 [grib2 ...]\n",prog);
  printf("opzioni:\n");
  printf("[-h|--help]\t\thelp dettagliato\n");
  printf("[-d|--dir <basedir>]\tmette i file di uscita in <dir>\n");
  printf("[-p|--parents]\t\tcrea tutte le sottodirectory necessarie (come mkdir -p)\n");
  printf("[-t|--traditional]\tusa il formato tradizionale di partgrib (default)\n");
  printf("[-f|--format <format>]\tusa il formato <format> per creare i nomi file\n");
  printf("[-c|--cong]\t\tusa un formato congeniale a cong\n");
  printf("[-n|--name]\t\tstampa su stdout i nomi dei file in uscita ma non li scrive realmente\n");
  printf("[-o|--one]\t\telabora solo il primo record grib di ogni file\n");
  printf("[-a|--append]\t\taccoda ai file gia` esistenti creati dalla corrente\n\t\t\tesecuzione di %s, se ripetuta, accoda a tutti i\n\t\t\tfile gia` esistenti\n",prog);
  printf("grib1 [grib2 ...]\tfile grib in ingresso\n");
}


int main (int argc, char **argv) {
  int c, date=0, genproc=0, style=ST_TRAD, name=0, one=0, len_file_name;
  char *dir=NULL, *bar, *format=NULL, *fo_cong=FO_CONG;
/*   int digit_optind = 0; */
  FILE *grib_file;
  struct grib_type grib_msg;
  unsigned char *grib_buffer, *save_ptr;
  unsigned char *section2_ptr;
  int grib_counter=0;
  gribfloat no_bit_value;
  long int ignore_bitmap=TRUE, status;

  while (1) {
/*     int this_option_optind = optind ? optind : 1; */
#ifdef HAVE_GETOPT_LONG
    int option_index = 0;
    static struct option long_options[] = {
      {"help", no_argument, NULL, 'h'},
      {"dir", required_argument, NULL, 'd'},
      {"parents", no_argument, NULL, 'p'},
      {"format", required_argument, NULL, 'f'},
      {"traditional", no_argument, NULL, 't'},
      {"cong", no_argument, NULL, 'c'},
      {"name", no_argument, NULL, 'n'},
      {"one", no_argument, NULL, 'o'},
      {"append", no_argument, NULL, 'a'},
      {NULL, 0, NULL, 0}
    };
#endif

#ifdef HAVE_GETOPT_LONG
    if ((c = getopt_long (argc, argv, "hd:pf:tcnoa", long_options, &option_index)) 
	== -1) break;
#else
    if ((c = getopt (argc, argv, "hd:pf:tca")) == -1) break;
#endif
    switch (c) {
    case 'h':
      usage(argv[0]);
      format_usage(argv[0]);
      return 0;
      break;
    case 'd':
      dir = optarg;
      break;
    case 'p':
      parents = 1;
      break;
    case 'f':
      format = optarg;
      style = ST_FORM;
      break;
    case 't':
      style = ST_TRAD;
      break;
    case 'c':
      format = fo_cong;
      style = ST_FORM;
      break;
    case 'a':
      append++;
      break;
    case 'n':
      name = 1;
      break;
    case 'o':
      one = 1;
      break;
    default:
      usage(argv[0]);
      return 1;
    }
  }

  if (optind >= argc) {
    usage(argv[0]);
    return 1;
  }

  if (style != ST_TRAD && dir != NULL) {
    strcat(file_name, dir); /* strncat(... sizeof(file_name)-strlen(dir)); */
    if (file_name[strlen(file_name)-1] != '/' &&
	strlen(file_name) < sizeof(file_name) -1) strcat(file_name, "/"); 

    if (parents) { /* Create all directories one at a time */
      bar = file_name;
      if (file_name[0] == '/') bar++;
      while ((bar=index(bar,'/')) != NULL) {
	*bar='\0';
	if (mkdir(file_name, 0775)) {
	  if (errno != EEXIST) {
	    perror(file_name);
	    return 2;
	  }
	}
	*bar='/';
	bar++;
      }
    }
  }


  for (; optind < argc; optind++) { /* Loop over input files */
    if ((grib_file = fopen(argv[optind],"rb")) == NULL) {
      perror(argv[optind]);
      return 2;
    }
    /* Loop over grib records */
    while((status = load_grib_file(grib_file, &grib_buffer)) == NORMAL_STATUS) {
      save_ptr = grib_buffer;
      grib_counter++;
      status = degrib(GRIB_INFO, &grib_buffer, ignore_bitmap,
		      &grib_msg, &no_bit_value);
      if (style == ST_TRAD) { /* Old behavior */
	status = save_grib_file(save_ptr,
				grib_msg.section0.total_length, grib_msg);
      } else { /* New behavior - format */
        section2_ptr = save_ptr + 8 + grib_msg.section1.section_length;
	len_file_name = strlen(file_name); /* Remember end of directory */
	grib_cformat(grib_counter,section2_ptr,
		     &grib_msg, format, file_name, sizeof(file_name));

	if (name) { /* Print file name */
	  printf("%s\n",file_name);
	} else { /* Write file */
	  status = save_grib_file_form(save_ptr, &grib_msg, len_file_name);
	}
	file_name[len_file_name] = '\0'; /* Leave only base directory name */
      }

      free(save_ptr);
      if (status != NORMAL_STATUS) return status;
      if (one) break;
    }
    fclose(grib_file);
  }

  return NORMAL_STATUS;
}


gribint save_grib_file_form(
unsigned char *grib_buffer,  /* buffer where the GRIB message is */
struct grib_type *grib_msg,  /* structure of read GRIB data */
int len_file_name)           /* length of base directory name */
{
  int do_append=0, size;
  FILE *grib_file;
  size_t written_bytes;
  char *bar, *oopt[2]={"w","a"};
  struct partgrib_filename **file;

  size = grib_msg->section0.total_length;
  if (parents) { /* Create all directories from format one at a time */
    bar = file_name+len_file_name;
/*       if (file_name[0] == '/') bar++; */
    while ((bar=index(bar,'/')) != NULL) {
      *bar='\0';
      if (mkdir(file_name, 0775)) {
	if (errno != EEXIST) {
	  perror(file_name);
	  return 2;
	}
      }
      *bar='/';
      bar++;
    }
  }
  if (append == 1) { /* is it an already-created file ? */
    for (file=&first_file; *file != NULL; file=&((*file)->next)) {
      if (!strcmp((*file)->name, file_name)) {
	do_append = 1; /* yes! */
	break;
      }
    }
    if (! do_append) { /* no, add it to the list! */
      if ((*file=malloc(sizeof(struct partgrib_filename))) != NULL) {
	(*file)->next = NULL;
	if (((*file)->name=malloc(strlen(file_name+1))) != NULL) {
	  strcpy((*file)->name, file_name);
	}
      }
    }
  } else if (append > 1) {
    do_append = 1;
  }

  if ((grib_file = fopen(file_name,oopt[do_append])) == NULL) {
    perror(file_name);
    return 2;
  }

  if ((written_bytes = fwrite(grib_buffer, sizeof(unsigned char), size, grib_file))
      != size) {
    perror(file_name);
    return 2;
  }
  fclose(grib_file);

  return NORMAL_STATUS;
}
