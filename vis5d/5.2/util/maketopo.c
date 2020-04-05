/* maketopo.c */

/* Vis5D version 4.3 */

/*
Vis5d program for visualizing five dimensional gridded data sets
Copyright (C) 1990-1997  Bill Hibbard, Brian Paul, Dave Santek,
and Andre Battaiola.

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




/* Utility to make new topography files for VIS-5D.

  compile with:  cc maketopo.c -o maketopo


  Basically, a topography is just a 2-dimensional array of elevations values.
  To make your own topography file for VIS-5D, follow these steps:

  1. Set the ROWS and COLUMNS to indicate how large the topography matrix is.

  2. Set the WEST, EAST, NORTH, and SOUTH values to indicate what region
     of earth your topography spans.  These numbers are in degrees of
     latitude ( + is north, - is south ) and degrees of longitude
     ( + is west, - is east ).

  3. Specify a FILENAME for the topography file.

  4. Put your topography values into the Topo matrix.  Each value is a
     floating point value in meters above sea level.  Positive values are
     above sea level and negative values are below.  All values must be
     in the interval [-8100 , 8100].

     Row [0] corresponds to the north edge of the topography.
     Row [ROWS-1] corresponds to the south edge.
     Column [0] corresponds to the west edge.
     Column [COLUMNS-1] corresponds to the east edge.

  5. [optional]
     Put land/water flags into the Water matrix.  The Water matrix is
     a 2-D array the same size as the Topo matrix.  If Water[row][column]
     is non-zero, then the topography will be blue at that location,
     otherwise the topography is considered to be land.

     The purpose of the Water matrix is to allow you to have water at
     elevations above sea level and have land at elevations below sea level.

*/




#include <stdio.h>
#include "binio.h"


/*** Set these values: ***/

#define ROWS     100          /* These sample values are for a topography   */
#define COLUMNS  200          /* of 100 rows by 200 columns spanning the    */
#define WEST     180.0        /* region from 180 degrees west longitude to  */
#define EAST    -180.0        /* -180 degrees east longitude and from       */
#define NORTH     90.0        /* 90 degrees north latitude to               */
#define SOUTH    -90.0        /* -90 degrees south latitude. (entire earth) */

#define FILENAME "sample.topo"   /* use the .topo suffix for consistency */


float Topo[ROWS][COLUMNS];
char  Water[ROWS][COLUMNS];



main()
{
   int r, c;

   /*** Insert your code here to read your data into the Topo matrix ***/

   /* FOR EXAMPLE:
   FILE *f;
   float z;

   if (f=fopen("mydata","r")) {
      for (r=0;r<ROWS;r++) {
         for (c=0;c<COLUMNS;c++) {
            fscanf( f, "%f", &z );
            Topo[r][c] = z;
         }
      }
      fclose(f);
   }
   else {
      printf("unable to open file\n");
      exit(1);
   }
   */


   /*** Optionally set water flags here: ***/

   /* this example sets the Water flag for all elevations <= sea level */
   for (r=0;r<ROWS;r++) {
      for (c=0;c<COLUMNS;c++) {
         if (Topo[r][c] <= 0.0) {
            Water[r][c] = 1;
         }
         else {
            Water[r][c] = 0;
         }
      }
   }


   /*** Now write the file ***/

   if (write_topo( FILENAME )) {
      printf("done\n");
   }
   else {
      printf("error writing topo\n");
   }
   exit(0);
}




/**********************************************************************/
/***        No changes beyond this point should be necessary.       ***/
/**********************************************************************/

/* NOTES:
   1. int is assumed to be 4-byte signed.
   2. short int is assumed to be 2-byte signed.
   3. the topo_header must be 64 bytes long!
   4. The topo file contains a 64-byte header followed by the 2-D array
      of encoded elevation and water flag values in row-major order.
*/


struct topo_header {
   char id[40];        /* id string "TOPO2" and extra space */
   float westlon;      /* West longitude in degrees */
   float eastlon;      /* East longitude in degrees */
   float northlat;     /* North latitude in degrees */
   float southlat;     /* South latitude in degrees */
   int rows;           /* number of rows */
   int columns;        /* number of columns */
};




/** write the topofile and return 1 for success, 0 for error **/
write_topo( filename )
char filename[];
{
   int f;
   struct topo_header header;
   short int val[ROWS][COLUMNS];
   short int z;
   int r, c;
   mode_t mask = 0666;

   strcpy(header.id, "TOPO2");  /* TOPO = int values, TOPO2 = float values */
   header.westlon = WEST;
   header.eastlon = EAST;
   header.northlat = NORTH;
   header.southlat = SOUTH;
   header.rows = ROWS;
   header.columns = COLUMNS;

   /* Convert each elevation to a short integer, multiply it by two and */
   /* add 1 if the Water flag is set */
   for (r=0;r<ROWS;r++) {
      for (c=0;c<COLUMNS;c++) {
         z = (short int) Topo[r][c];
         if (Water[r][c]) {
            val[r][c] = (z * 2) + 1;
         }
         else {
            val[r][c] = z * 2;
         }
      }
   }

   f = open( filename, O_WRONLY|O_CREAT, mask );
   if (f==-1) {
      /* error */
      return 0;
   }
   else {
      /* Write the header */
      write_bytes( f, header.id, 40 );
      write_float4( f, header.westlon );
      write_float4( f, header.eastlon );
      write_float4( f, header.northlat );
      write_float4( f, header.southlat );
      write_int4( f, header.rows );
      write_int4( f, header.columns );
      /* write the topo data */
      write_int2_array( f, val, ROWS*COLUMNS );
      close(f);
      return 1;
   }
}

