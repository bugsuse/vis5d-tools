/*
c    <picogrib software di decodifica grib>
c    Copyright (C) 2000  <SMR ARPA>
c      $Date: 2007-02-23 13:01:52 +0100 (ven, 23 feb 2007) $    $Revision: 59 $
c      $Id: pbutils.c 59 2007-02-23 12:01:52Z cesari $

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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "constant.h"
#include "struct.h"

#define MAXOPFIL 128
#define MAXFNAMELEN 1024

/* definizione per i c obsoleti */
#ifndef off_t
#define off_t long int
#endif

FILE* fpopen[MAXOPFIL];
gribint first = 1;

#ifdef _CRAY
void F77_FUNC(pbopen,PBOPEN) (gribint* unit, char* name, long l1,
			      char* mode, long l2, gribint* iret)
#else
void F77_FUNC(pbopen,PBOPEN) (gribint* unit, char* name, char* mode, 
			      gribint* iret, long l1, long l2)
#endif
{char localname[MAXFNAMELEN], * p;
 gribint i;
 if (first) /* The first time nullify all the pointers */
   {for (i=0; i<MAXOPFIL; i++) fpopen[i] = NULL;
   first = 0;}

 strncpy( localname, name, (size_t) l1); /* MAXFNAMELEN - 1);*/
 localname[l1] = '\0';
 p = localname;
 for (p = localname; *p != '\0' && *p != ' '; p++); /*Trim blanks*/
   *p = '\0';

 for (i=0; fpopen[i] != NULL && i<MAXOPFIL; i++); /*look for a free file pointer*/
 if (i<MAXOPFIL) /*a free pointer/unit is available*/
   {if ((fpopen[i] = fopen(localname, "r")) != NULL)
     {*unit = i;
     *iret = 0;}
   else
     {*iret = 1;}
   }
 else /*no file pointer/units available*/
   {*iret = 1;}
}


void F77_FUNC(pbclose,PBCLOSE) (gribint* unit, gribint* iret)
{fclose(fpopen[*unit]);
 fpopen[*unit] = NULL; /*make available the unit of the closed file*/
}


void F77_FUNC(pbseek,PBSEEK) (gribint* unit, gribint* offset, gribint* whence,
			     gribint* iret)
{off_t local_offset = (off_t) *offset;
 if (*whence == 2) local_offset = - abs(local_offset); /* From end of file*/
 *iret = fseek(fpopen[*unit], local_offset, *whence);
 if(*iret != 0)
   {if ( ! feof(fpopen[*unit]))
     {*iret = -2;} /*Error*/
   else
     {*iret = -1;} /*End of file*/
   clearerr(fpopen[*unit]);
   return;
   }
 *iret = (gribint) ftell(fpopen[*unit]); /*Offset from start of file*/
}


void F77_FUNC(pbtell,PBTELL) (gribint* unit, gribint* iret)
{
  if ((*iret = ftell(fpopen[*unit])) < 0) *iret = -2;
}


void F77_FUNC(pbgrib,PBGRIB) (gribint* unit, unsigned char* grib, 
		gribint* kinlen, gribint* koutlen, gribint* iret)
{unsigned char* tempbuff;
 gribint i,kbit,kneg,kpr;
 if (load_grib_file(fpopen[*unit], &tempbuff) == NORMAL_STATUS)
   {*koutlen = 0;
   for (i=4;i<=6;i++) /*Compute length of message*/
     *koutlen = (*koutlen << 8) + *(tempbuff + i);


   F77_FUNC(setpar,SETPAR) (&kbit, &kneg, &kpr);

   if (*koutlen <= *kinlen*(kbit >> 3))
     {memcpy(grib, tempbuff, *koutlen); /*Copy data to provided buffer*/
     free(tempbuff); /*Deallocate buffer allocated in load_grib_file*/
     *iret = 0;}
   else
     {fprintf(stderr,
	      "Error: buffer size of %d bytes too small for grib, %d bytes.\n",
	      *kinlen,*koutlen);
     free(tempbuff); /*Deallocate buffer allocated in load_grib_file*/
     *iret = -3;}
   }
 else
   {*iret = -1;}
}


void F77_FUNC(pbwrite,PBWRITE) (gribint* unit, unsigned char* grib, 
		gribint* kinlen, gribint* iret)
{
  if ((*iret = fwrite(grib, sizeof(char), *kinlen, fpopen[*unit])*sizeof(char))
      != (sizeof(char) * *kinlen)) *iret = -1;
}
