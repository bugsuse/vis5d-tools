/* read_grads.c */
/* Vis5D version 5.2 */

/*
Vis5D system for visualizing five dimensional gridded data sets
Copyright (C) 1990 - 2000 Bill Hibbard, Johan Kellum, Brian Paul,
Dave Santek, and Andre Battaiola.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/


/*
 * Functions for reading GRADS files
 */


#include <ctype.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "binio.h"
#include "file_i.h"
#include "grid_i.h"
#include "misc_i.h"
#include "proj_i.h"
#include "projlist_i.h"
#include "tokenize_i.h"
#include "../src/v5d.h"



#define MAX(A,B)  ( (A) > (B) ? (A) : (B) )


/* Max chars on input line in CTRL file: */
#define MAXLINE 100



/*
 * Given a time/date string in GrADS format, return it as a date (YYDDD)
 * and time (HHMMSS).
 */
static int parse_time( char *str, int *date, int *time )
{
   int hh, mm, year, day, leap;
   int k;

   /* Extract hours and minutes */
   if (str[2]==':') {
      /* extract hh:mmZ */
      if (!isdigit(str[0]) || !isdigit(str[1])) {
         return 0;
      }
      hh = (str[0] - '0') * 10 + str[1] - '0';
      if (!isdigit(str[3]) || !isdigit(str[4])) {
         return 0;
      }
      mm = (str[3] - '0') * 10 + str[4] - '0';
      if (str[5]!='Z' && str[5]!='z') {
         return 1;
      }
      k = 6;
   }
   else if (str[2]=='Z' || str[2]=='z') {
      /* extract hhZ */
      if (!isdigit(str[0]) || !isdigit(str[1])) {
         return 0;
      }
      hh = (str[0] - '0') * 10 + str[1] - '0';
      mm = 0;
      k = 3;
   }
   else if (str[1]==':') {
      /* extract h:mmZ */
      if (!isdigit(str[0])) {
         return 0;
      }
      hh = str[0] - '0';
      if (!isdigit(str[2]) || !isdigit(str[3])) {
         return 0;
      }
      mm = (str[2] - '0') * 10 + str[3] - '0';
      if (str[4]!='Z' && str[4]!='z') {
         return 0;
      }
      k = 5;
   }
   else if (str[1]=='Z') {
      /* extract hZ */
      if (!isdigit(str[0])) {
         return 0;
      }
      hh = str[0] - '0';
      mm = 0;
      k = 2;
   }
   else {
      hh = mm = 0;
      k = 0;
   }

   *time = 100 * (100 * hh + mm);

   /* Extract day */
   if (isdigit(str[k])) {
      /* day */
      if (isdigit(str[k+1])) {
         day = (str[k] - '0') * 10 + str[k+1] - '0';
         k += 2;
      }
      else {
         day = str[k] - '0';
         k += 1;
      }
   }
   else {
      day = 0;
   }

   /* Extract year from str[k+3] .. str[k+(4 or 6)] */
   if (isdigit(str[k+3]) && isdigit(str[k+4])) {
      if (isdigit(str[k+5]) && isdigit(str[k+6])) {
         year = (str[k+5] - '0') * 10 + str[k+6] - '0';
      }
      else {
         year = (str[k+3] - '0') * 10 + str[k+4] - '0';
      }
   }
   else {
      return 0;
   }

   /* check if leap year */
   leap = ( (year % 4) == 0);

   /* Extract month to increment days */
   if (strncmp(str+k,"jan",3)==0 || strncmp(str+k,"JAN",3)==0) {
      day += 0;
   }
   else if (strncmp(str+k,"feb",3)==0 || strncmp(str+k,"FEB",3)==0) {
      day += 31;
   }
   else if (strncmp(str+k,"mar",3)==0 || strncmp(str+k,"MAR",3)==0) {
      day += 59 + leap;
   }
   else if (strncmp(str+k,"apr",3)==0 || strncmp(str+k,"APR",3)==0) {
      day += 90 + leap;
   }
   else if (strncmp(str+k,"may",3)==0 || strncmp(str+k,"MAY",3)==0) {
      day += 120 + leap;
   }
   else if (strncmp(str+k,"jun",3)==0 || strncmp(str+k,"JUN",3)==0) {
      day += 151 + leap;
   }
   else if (strncmp(str+k,"jul",3)==0 || strncmp(str+k,"JUL",3)==0) {
      day += 181 + leap;
   }
   else if (strncmp(str+k,"aug",3)==0 || strncmp(str+k,"AUG",3)==0) {
      day += 211 + leap;
   }
   else if (strncmp(str+k,"sep",3)==0 || strncmp(str+k,"SEP",3)==0) {
      day += 242 + leap;
   }
   else if (strncmp(str+k,"oct",3)==0 || strncmp(str+k,"OCT",3)==0) {
      day += 272 + leap;
   }
   else if (strncmp(str+k,"nov",3)==0 || strncmp(str+k,"NOV",3)==0) {
      day += 303 + leap;
   }
   else if (strncmp(str+k,"dec",3)==0 || strncmp(str+k,"DEC",3)==0) {
      day += 333 + leap;
   }
   else {
      return 0;
   }

   *date = 1000 * year + day;

   return 1;
}



/*
 * Given a time increment string in GrADS format, return the increment
 * in days and seconds.
 */
static int parse_time_inc( char *inc, int *days, int *seconds )
{
   int i, k;

   i = inc[0] - '0';
   if (isdigit(inc[1])) {
      i = i * 10 + inc[1] - '0';
      k = 2;
   }
   else {
      k = 1;
   }

   if ((inc[k]=='M' && inc[k+1]=='N') || (inc[k]=='m' && inc[k+1]=='n')) {
      *days = 0;
      *seconds = i * 60;
   }
   else if ((inc[k]=='H' && inc[k+1]=='R') || (inc[k]=='h' && inc[k+1]=='r')) {
      *days = 0;
      *seconds = i * 60 * 60;
   }
   else if ((inc[k]=='D' && inc[k+1]=='Y') || (inc[k]=='d' && inc[k+1]=='y')) {
      *days = i;
      *seconds = 0;
   }
   else if ((inc[k]=='M' && inc[k+1]=='O') || (inc[k]=='m' && inc[k+1]=='o')) {
      *days = 30 * i;
      *seconds = 60 * 60 * 10;
   }
   else if ((inc[k]=='Y' && inc[k+1]=='R') || (inc[k]=='y' && inc[k+1]=='r')) {
      *days = 365 * i;
      *seconds = 0;
   }
   else {
      *days = 0;
      *seconds = 0;
      return 0;
   }

   return 1;
}



static void flip_layer( float *data, int rows, int cols, float missing_value )
{
   int i, j;
   float temp[MAXROWS*MAXCOLUMNS];

#define DATA(R,C)  data[R*cols+C]
#define TEMP(R,C)  temp[C*rows+rows-R-1]

   for (i=0;i<rows;i++) {
      for (j=0;j<cols;j++) {

         if (DATA(i,j)==missing_value) {
            TEMP(i,j) = MISSING;
         }
         else {
            TEMP(i,j) = DATA(i,j);
         }
      }
   }

   memcpy( data, temp, rows*cols*sizeof(float) );

#undef DATA
#undef TEMP
}




/*
 * Scan the named file, createing a grid_info struct for each grid.  Store
 * the grid_info structs in the grid data base.
 * Input:  name - name of grads file.
 *         db - the grid data base
 * Return:  number of grids found.
 */
int get_grads_info( char *name, struct grid_db *db )
{
   FILE *f;
   char **token;
   int ntokens;
   int time, var;
   int grids = 0;
   struct grid_info *info;
   int pos = 0;
   float args[1000];
   struct projection *proj;
   struct vcs *vcs[MAXVARS];

   /* GrADS file info: */
   char dset_name[1000];
   float missing_value;
   int byteswapped = 0;
   int nr, nc, nl[MAXLEVELS], maxnl;
   int vertical;
   float westbound, northbound, bottombound;
   float rowinc, colinc, levinc;
   float height[MAXLEVELS];
   char varname[MAXVARS][100];
   int timestamp[MAXTIMES], datestamp[MAXTIMES];
   int numtimes, numvars;


   f = fopen( name, "r" );
   if (!f) {
      printf("Error: couldn't open %s\n", name );
      return 0;
   }

   while (1) {
      char line[MAXLINE];
      /* read a line of the control file */
      if (!fgets( line, MAXLINE, f ))
         break;

      /* break line into tokens */
      token = tokenize( line, &ntokens );

      if (ntokens==0) continue;

      if (strcmp(token[0],"DSET")==0 || strcmp(token[0],"dset")==0) {
         if (ntokens<2) {
            printf("Error: missing filename argument on DSET line\n");
         }
         else {
            if (token[1][0]=='^') {
               /* skip the ^ (current directory) character */
               strcpy( dset_name, token[1]+1 );
            }
            else {
               strcpy( dset_name, token[1] );
            }
         }
      }
      else if (strcmp(token[0],"TITLE")==0 || strcmp(token[0],"title")==0) {
         /* ignore the title line */
      }
      else if (strcmp(token[0],"UNDEF")==0 || strcmp(token[0],"undef")==0) {
         missing_value = atof(token[1]);
      }
      else if (strcmp(token[0],"BYTESWAPPED")==0
               || strcmp(token[0],"byteswapped")==0) {
         byteswapped = 1;
      }
      else if (strcmp(token[0],"FORMAT")==0) {
         if (strcmp(token[1],"SEQUENTIAL")==0 ||
             strcmp(token[1],"sequential")==0) {
               /* this is the only format currently supported; */
               /* also note: FORMAT keyword has been replaced by OPTIONS */
         }
         else {
            printf("Warning: FORMAT not fully supported\n");
            printf("         only SEQUENTIAL format is allowed.\n");
         }
      }
      else if (strcmp(token[0],"OPTIONS")==0) {
         if (strcmp(token[1],"SEQUENTIAL")==0 ||
             strcmp(token[1],"sequential")==0) {
               /* this is the only option currently supported */
         }
         else {
            printf("Warning: OPTIONS not fully supported\n");
            printf("         only SEQUENTIAL option is allowed.\n");
         }
      }
      else if (strcmp(token[0],"FILEHEADER")==0 || strcmp(token[0],"fileheader")==0) {
         if (ntokens<2) {
            printf("Error: missing position argument on FILEHEADER line\n");
         }
         pos = atoi( token[1] );
      }
      else if (strcmp(token[0],"XDEF")==0 || strcmp(token[0],"xdef")==0) {
         if (ntokens<4) {
            printf("Error: missing arguments to XDEF line\n");
         }
         else {
            nc = atoi( token[1] );
            if (strcmp(token[2],"LINEAR")==0 || strcmp(token[2],"LINEAR")==0) {
               westbound = -atof( token[3] );
               colinc = atof( token[4] );
            }
            else if (strcmp(token[2],"LEVELS")==0
                     || strcmp(token[2],"LEVELS")==0) {
               printf("Warning: XDEF LEVELS not fully supported\n");
               westbound = -atof( token[3] );
               colinc = fabs( atof(token[3]) - atof(token[4]) );
            }
         }
      }
      else if (strcmp(token[0],"YDEF")==0 || strcmp(token[0],"ydef")==0) {
         if (ntokens<4) {
            printf("Error: missing arguments to YDEF line\n");
         }
         else {
            nr = atoi( token[1] );
            if (strcmp(token[2],"LINEAR")==0 || strcmp(token[2],"LINEAR")==0) {
               float southbound = atof( token[3] );
               rowinc = atof( token[4] );
               northbound = southbound + rowinc * (nr-1);
            }
            else if (strncmp(token[2],"GAUSR",5)==0
                     || strncmp(token[2],"gausr",5)==0) {
               printf("Warning: YDEF GAUSRnn not supported\n");
            }
         }
      }
      else if (strcmp(token[0],"ZDEF")==0 || strcmp(token[0],"zdef")==0) {
         if (ntokens<3) {
            printf("Error: missing arguments to ZDEF line\n");
         }
         else {
            float pressure[MAXLEVELS];
            int i;
            maxnl = atoi( token[1] );
            if (strcmp(token[2],"LINEAR")==0 || strcmp(token[2],"linear")==0) {
               vertical = VERT_EQUAL_KM;
               bottombound = atof( token[3] );
               levinc = atof( token[4] );
               for (i=0;i<maxnl;i++) {
                  pressure[i] = bottombound + i * levinc;
               }
            }
            else if (strcmp(token[2],"LEVELS")==0
                     || strcmp(token[2],"levels")==0) {
               vertical = VERT_UNEQUAL_KM;
               for (i=0;i<maxnl && i+3<ntokens;i++) {
                  pressure[i] = atof( token[3+i] );
               }
               while (i<maxnl) {
                  fscanf( f, "%f", &pressure[i] );
                  i++;
               }
            }
 
            /* convert pressures to heights */
            {
               float p_bot, p_top, zinc;
               float Po=1012.15;
               float H=7.2;
               p_bot = pressure[0];
               p_top = pressure[maxnl-1];
               height[0] = MAX( H*log(Po/p_bot), 1.0 );
               height[maxnl-1] = H*log(Po/p_top);
               if (maxnl<=1) {
                  zinc = 0.0;
               }
               else {
                  zinc = (height[maxnl-1] - height[0]) / (maxnl-1);
               }
               for (i=0;i<maxnl;i++) {
                  height[i] = H * log( Po/pressure[i] );
               }
            }
         }
      }
      else if (strcmp(token[0],"TDEF")==0 || strcmp(token[0],"tdef")==0) {
         if (ntokens!=5) {
            printf("Error: missing arguments to TDEF line\n");
         }
         else {
            int date0, time0, days, seconds;
            int i, it, id, ii;

            numtimes = atoi( token[1] );
            /* token[2]=="LINEAR" */

            if (!parse_time( token[3], &date0, &time0 )) {
               printf("Error reading grads header, bad time: %s\n", token[3] );
            }
            if (!parse_time_inc( token[4], &days, &seconds )) {
               printf("Error reading grads header, bad time increment: %s\n",
                      token[4] );
            }

            id = v5dYYDDDtoDays(date0);
            it = v5dHHMMSStoSeconds(time0);
            for (i=0;i<numtimes;i++) {
               timestamp[i] = v5dSecondsToHHMMSS(it);
               datestamp[i] = v5dDaysToYYDDD(id);
               it = it + seconds;
               ii = it / 86400;
               it = it - 86400 * ii;
               id = id + days + ii;
            }
         }
      }
      else if (strcmp(token[0],"VARS")==0 || strcmp(token[0],"vars")==0) {
         /* TODO: variables */
         if (ntokens!=2) {
            printf("Error: wrong number of arguments to VARS line\n");
         }
         else {
            char **vartok;
            int i, ntok;

            numvars = atoi( token[1] );

            for (i=0;i<numvars;i++) {
               fgets( line, MAXLINE, f );
               vartok = tokenize( line, &ntok );
               strcpy( varname[i], vartok[0] );   /* var name */
               nl[i] = atoi( vartok[1] );         /* number of levels */
               if (nl[i]==0)  nl[i] = 1;
               /* vartok[2] = units */
               /* vartok[3] = text description */
               free_tokens( vartok );
            }
         }
      }
      else if (strcmp(token[0],"ENDVARS")==0 || strcmp(token[0],"ENDVARS")==0){
         /* ignore */
      }
      else {
         printf("Warning: unknown token: %s\n",token[0] );
      }

   } /*while*/


   /*
    * Generate grid_info structs
    */

   /* same projection for all grids */
   args[0] = northbound;
   args[1] = westbound;
   args[2] = rowinc;
   args[3] = colinc;
   proj = new_projection( db, PROJ_LINEAR, nr, nc, args );

   /* Potentially different vcs for each grid because number of levels */
   /* can vary per variable. */
   for (var=0;var<numvars;var++) {
      if (vertical==VERT_EQUAL_KM) {
         args[0] = bottombound;
         args[1] = levinc;
      }
      else {
         int i;
         for (i=0;i<maxnl;i++) {
            args[i] = height[i];
         }
      }
      vcs[var] = new_vcs( db, vertical, nl[var], 0, args );
   }

   for (time=0;time<numtimes;time++) {

      for (var=0;var<numvars;var++) {

         info = alloc_grid_info();
         info->FileName = str_dup( dset_name );
         info->Format = FILE_GRADS;
         info->TimeStep = time;
         info->VarNum = var;
         info->Position = pos;         /* pos. of data in binary grid file */
         pos += nr * nc * nl[var] * 4;

         info->Nr = nr;
         info->Nc = nc;
         info->Nl = nl[var];

         info->DateStamp = datestamp[time];
         info->TimeStamp = timestamp[time];
         info->VarName = str_dup( varname[var] );

         info->Proj = proj;
         info->Vcs = vcs[var];

         info->MissingValue = missing_value;
         info->byteswapped = byteswapped;

         append_grid( info, db );
         grids++;

      }

   }

   return grids;
}




/*
 * Get the grid data described by g.
 * Input:  g - pointer to a grid_info structure.  It tells us which grid
 *             in which file is needed.
 * Return:  pointer to grid data (can be free()'d when finished)
 *          OR return NULL if there's an error.
 */
float *get_grads_data( struct grid_info *g )
{
   int f, n, nread;
   float *data;

   f = open( g->FileName, O_RDONLY );
   if (f<0) {
      printf("Error: couldn't open %s\n", g->FileName );
      return NULL;
   }

   if (lseek( f, g->Position, SEEK_SET )!=g->Position) {
      printf("Error: couldn't get  GrADS data for time %d, var %s\n",
             g->TimeStep, g->VarName );
      close(f);
      return NULL;
   }

   n = g->Nr * g->Nc * g->Nl;
   data = (float *) malloc( n * sizeof(float) );

   nread = read_float4_array( f, data, n );
   if (nread<n) {
      printf("Error: couldn't read GrADS data for time %d, var %s\n",
             g->TimeStep, g->VarName );
      free( data );
      close(f);
      return NULL;
   }
   else {
      int i;
      if (g->byteswapped) {
        flip4((const unsigned int *) data, (unsigned int*) data, nread);
      }
      /* flip data */
      for (i=0;i<g->Nl;i++) {
         flip_layer( data + i * g->Nr * g->Nc, g->Nr, g->Nc, g->MissingValue );
      }
   }
   close(f);

   return data;
}

