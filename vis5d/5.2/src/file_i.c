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

/* file.c */


/*
 * Interface to the file format-specific functions.  I.E. we only call
 * the functions in read_gr3d, read_grads, read_*, via the functions in
 * this file.
 */



#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "binio.h"
#include "read_epa_i.h"
#include "read_gr3d_i.h"
#include "read_grid_i.h"
#include "read_grads_i.h"
#include "read_uwvis_i.h"
#include "read_v5d_i.h"
#include "grid_i.h"



/*
 * Determine the format of the named file.
 * Input:  a path/file name
 * Return:  one of FILE_UNKNOWN, FILE_GR3D, FILE_EPA, etc.
 */
static int determine_file_format( char *name )
{
   FILE *f;
   unsigned char head[200];
   int n;

   /* Open the file */
   f = fopen( name, "r" );
   if (!f) {
      return FILE_UNKNOWN;
   }

   /* Read first 200 bytes of file */
   n = fread( head, 1, 200, f );
   fclose(f);
   if (n!=200) {
      return FILE_UNKNOWN;
   }


   /*
    * If filename is 8 characters long and starts with "GR3D" then
    * this is probably a McIDAS GR3D file...
    */
   {
      char *p = strrchr( name, '/' );
      if (p) {
         p++;
      }
      else {
         p = name;
      }
      if (strlen(p)==8 && strncmp(p,"GR3D",4)==0) {
         return FILE_GR3D;
      }
   }


   /*
    * If filename is 8 characters long and starts with "GRID" then
    * this is probably a McIDAS GRID file...
    */
   {
      char *p = strrchr( name, '/' );
      if (p) {
         p++;
      }
      else {
         p = name;
      }
      if (strlen(p)==8 && strncmp(p,"GRID",4)==0) {
         return FILE_GRID;
      }
   }


   /*
    * An EPA MM4 file has one of two specific 8-character strings starting
    * at the 17th byte of the file...
    */
   if (   memcmp( head+16, "MMOUT   ", 8 )==0
       || memcmp( head+16, "ZIGGY   ", 8 )==0) {
      return FILE_EPA;
   }


   /*
    * An EPA RADM file has one of 4 specific 16-character strings starting
    * at byte 144 of the file...
    */
#ifdef JUNK
   {
      int i;
      for (i=144;i<160;i++) {
         printf("%d %c\n", i, head[i] );
      }
   }
#endif
   if (   memcmp(head+144,"AX    69NSPEC   ",16)==0
       || memcmp(head+144,"AX    35NSPEC   ",16)==0
       || memcmp(head+144,"15IMAX    35NSPE",16)==0
       || memcmp(head+144," 6IMAX    35NSPE",16)==0) {
      return FILE_EPA;
   }


   /*
    * A Vis5D .v5d file starts with "V5D\n"
    */
   if (memcmp(head,"V5D\n",4)==0) {
      return FILE_V5D;
   }


   /*
    * An old comp5d file starts with 0x808080.  It can be treated as a
    * v5d file.
    */
   if (head[0]==0x80 && head[1]==0x80 && head[2]==0x80) {
      return FILE_V5D;
   }


   /*
    * A GrADS control file starts with "DSET"
    */
   if (memcmp(head,"DSET",4)==0) {
      return FILE_GRADS;
   }


   /*
    * A UW NMS VIS file starts with an ascii line indicating number of
    * variables.
    */
   {
      int pos, n;
      /* skip spaces */
      for (pos=0; head[pos]==' '; pos++);
      n = 0;
      while (head[pos]>='0' && head[pos]<='9') {
         n = n * 10 + head[pos]-'0';
         pos++;
      }
      if (n>0 && n<100) {   /* test for a reasonable number */
         return FILE_UWVIS;
      }
   }


   /*** ADD NEW FORMATS HERE ***/


   return FILE_UNKNOWN;
}




/*
 * Get information about all grids in the named file.
 * Input:  name - name of file to scan
 *         db - grid data base to put grid info into
 */
void get_file_info( char *name, struct grid_db *db )
{
   FILE *f;
   int kind;

   /* See if the file exists and is readable */
   f = fopen( name, "r" );
   if (!f) {
      printf("Warning:  couldn't open %s\n", name );
      return;
   }
   fclose(f);


   /* Now do format-specific part */
   kind = determine_file_format(name);

   switch (kind) {
      case FILE_GR3D:
         get_gr3d_info( name, db );
         break;
      case FILE_GRID:
         get_grid_info( name, db );
         break;
      case FILE_EPA:
         get_epa_info( name, db );
         break;
      case FILE_V5D:
         get_v5d_info( name, db );
         break;
      case FILE_GRADS:
         get_grads_info( name, db );
         break;
      case FILE_UWVIS:
         get_uwvis_info( name, db );
         break;

      /*** ADD NEW FORMATS HERE ***/

      case FILE_UNKNOWN:
      default:
         printf("Warning:  %s is of unknown type\n", name );
         break;
   }
}



/*
 * Retreive actual grid data from a file.
 * Input:  g - pointer to grid_info struct which describes what we want
 * Return:  pointer to a buffer of floating point data
 */
float *get_file_data( struct grid_info *g )
{
   /* Read the grid data from the file */
   switch (g->Format) {
      case FILE_GR3D:
         return get_gr3d_data( g );
      case FILE_GRID:
         return get_grid_data( g );
      case FILE_EPA:
         return get_epa_data( g );
      case FILE_V5D:
         return get_v5d_data( g );
      case FILE_GRADS:
         return get_grads_data( g );
      case FILE_UWVIS:
         return get_uwvis_data( g );
         
      /*** ADD NEW FORMATS HERE ***/


      default:
         printf("Internal error in get_grid_data()\n");
         return NULL;
   }
}

