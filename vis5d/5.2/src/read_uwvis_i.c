/* read_uwvis.c */
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
 * Functions for reading UW NMS model VIS files.
 */


#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "binio.h"
#include "file_i.h"
#include "grid_i.h"
#include "misc_i.h"
#include "proj_i.h"
#include "projlist_i.h"
#include "../src/v5d.h"




/**********************************************************************/
/*****                      VIS file I/O                          *****/
/**********************************************************************/


static char vcscr[64] = {
                '0','1','2','3','4','5','6','7','8','9'
               ,'A','B','C','D','E','F','G','H','I','J'
               ,'K','L','M','N','O','P','Q','R','S','T'
               ,'U','V','W','X','Y','Z','a','b','c','d'
               ,'e','f','g','h','i','j','k','l','m','n'
               ,'o','p','q','r','s','t','u','v','w','x'
               ,'y','z','{','|'
};

static char inv_vcscr[256];


static void init_visreader( void )
{
   int i;

   /* init inv_vcscr array */
   for (i=0;i<64;i++) {
      inv_vcscr[ vcscr[i] ] = i;
   }
}



static int *read_int_block( FILE *f, int *len )
{
   int words, bits, ch, val, i, j, k, count, bytes;
   float a, b;
   char line[80];
   int *buffer;

   /* read number of words, addfac, multfac */
   fscanf( f, "%d %d %f %f", &words, &bits, &a, &b);
   fgetc( f );  /* skip \n */

   buffer = (int *) malloc( words * sizeof(int) );
   bytes = (bits+5) / 6;

   count = 0;
   while (count<words) {

      /* read a line */
      fgets( line, 80, f );

      /* convert line of chars to integers */
      for (i=k=0;i<78/bytes && count<words;i++) {
         for (j=val=0;j<bytes;j++) {
            ch = inv_vcscr[line[k++]];
            val = (val << 6) | ch;
         }
/*         buffer[count++] = val * (int) b + (int) a;*/
         buffer[count++] = val * (int) b - (int) a;
      }

   }

   *len = words;
   return buffer;
}




static float *read_float_block( FILE *f, int *len )
{
   int words, bits, ch, val, i, j, k, count, bytes;
   float a, b;
   char line[80];
   float *buffer;

   /* read number of words, bits, addfac, multfac */
   fscanf( f, "%d %d %f %f", &words, &bits, &a, &b);
   fgetc( f );  /* skip \n */

   buffer = (float *) malloc( words * sizeof(float) );
   bytes = (bits+5) / 6;

   count = 0;
   while (count<words) {

      /* read a line */
      fgets( line, 80, f );

      /* convert line of chars to floats */
      for (i=k=0;i<78/bytes && count<words;i++) {
         for (j=val=0;j<bytes;j++) {
            ch = inv_vcscr[line[k++]];
            val = (val << 6) | ch;
         }
         buffer[count++] = (float) val / b - a;
      }

   }

   *len = words;
   return buffer;
}



static void skip_float_block( FILE *f )
{
   int words, bits, i, count, bytes;
   float a, b;
   char line[80];

   /* read number of words, bits, addfac, multfac */
   fscanf( f, "%d %d %f %f", &words, &bits, &a, &b);
   fgetc( f );  /* skip \n */

   bytes = (bits+5) / 6;

   count = 0;
   while (count<words) {

      /* read a line */
      fgets( line, 80, f );

      /* convert line of chars to floats */
      for (i=0;i<78/bytes && count<words;i++) {
         count++;
      }

   }

}




/**********************************************************************/
/**********************************************************************/


/*
 * Scan the named file, createing a grid_info struct for each grid.  Store
 * the grid_info structs in the grid data base.
 * Input:  name - name of UW VIS file.
 *         db - the grid data base
 * Return:  number of grids found.
 */
int get_uwvis_info( char *name, struct grid_db *db )
{
   static int init_flag = 0;
   FILE *f;
   int grids = 0;
   int var, numvars, nr, nc, nl;
   float height[MAXLEVELS];
   char ch;
   int i, vcs;

   if (init_flag==0) {
      init_visreader();
      init_flag = 1;
   }


   /* Open the file */
   f = fopen( name, "r" );
   if (!f) {
      return 0;
   }

   fscanf( f, "%d", &numvars );
   if (numvars>MAXVARS) {
      printf("ERROR: %s contains too many variables, limit is %d\n",
             name, MAXVARS );
   }

   /* grid size */
   fscanf( f, "%d", &nc );
   fscanf( f, "%d", &nr );
   fscanf( f, "%d", &nl );
   (void) getc(f);  /* get '\n' */

   /* this is tricky:
    * We look at the next character:
    * if it's a letter then
    *     we read the variable name and extract the height info from header
    * else
    *     read the height values for each grid level (variable delta Z).
    * endif
    */
   ch = getc(f);
   ungetc(ch, f);

   if (isalpha(ch)) {
      vcs = 1;
   }
   else {
      /* Read the height (in meters) of each grid level */
      for (i=0;i<nl;i++) {
         fscanf( f, "%8f", &height[i] );
         height[i] /= 1000.0;    /* convert from meters to km */
      }
      (void) getc(f);  /* get '\n' */
      vcs = 2;
   }


   for (var=0;var<numvars;var++) {
      int *header, header_size;
      char varname[100];
      struct grid_info *info;
      float args[100];

      /* read variable name */
      fgets( varname, 40, f );
      for (i=7;i>=0 && varname[i]==' ';i--) {
         varname[i] = '\0';
      }
      varname[8] = 0;

      /* read data header */
      header = read_int_block( f, &header_size );


      /*
       * Allocate grid info struct and initialize it.
       */
      info = alloc_grid_info();

      info->FileName = str_dup( name );
      info->Format = FILE_UWVIS;
      info->Position = ftell(f);    /* save position of data in file */

      info->Nr = nr;
      info->Nc = nc;
      info->Nl = nl;

      info->DateStamp = header[5];
      info->TimeStamp = header[6];
      info->VarName = str_dup( varname );

      args[0] = (float) header[22] / 10000.0;
      args[1] = (float) header[23] / 10000.0;
      args[2] = (float) header[24] / 10000.0;
      args[3] = (float) header[25] / 10000.0;
      info->Proj = new_projection( db, PROJ_LINEAR, nr, nc, args );

      if (vcs==1) {
         /* equally spaced km */
         float tophgt = (float) header[31] / 1000.0;
         float hgtinc = (float) header[32] / 1000.0;
         args[0] = tophgt - hgtinc*(nl-1);
         args[1] = hgtinc;
      }
      else {
         /* vcs==2 */
         /* unequally spaced km */
         memcpy( args, height, sizeof(float)*nl );
      }
      info->Vcs = new_vcs( db, vcs, nl, 0, args );

      /*
       * done with this grid
       */
      append_grid( info, db );
      grids++;

      free( header );

#ifdef LEAVEOUT
      {
         float *data;
         int data_size;
         /* read and skip data */
         data = read_float_block( f, &data_size );
         free( data );
      }
#else
      skip_float_block( f );
#endif
   }

   fclose(f);

   return grids;
}




/*
 * Get the grid data described by g.
 * Input:  g - pointer to a grid_info structure.  It tells us which grid
 *             in which file is needed.
 * Return:  pointer to grid data (can be free()'d when finished)
 *          OR return NULL if there's an error.
 */
float *get_uwvis_data( struct grid_info *g )
{
   float *data;
   int count;
   FILE *f;

   /* open file, seek to grid position */
   f = fopen( g->FileName, "r" );
   if (!f) return NULL;

   fseek( f, g->Position, SEEK_SET );

   data = read_float_block( f, &count );

   fclose(f);

   return data;
}

