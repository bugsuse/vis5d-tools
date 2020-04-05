/* maketopo.c */

/* Vis5D version 4.3 */

/*
Vis5d program for visualizing five dimensional gridded data sets
Copyright (C) 1990-1997  Bill Hibbard, Brian Paul, Dave Santek,
and Andre Battaiola.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.\

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include "../config.h"
#include <stdio.h>

#ifdef HAVE_LIBV5D
#include <vis5d+/binio.h>
#else
#include "binio.h"
#endif

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
int F77_FUNC(write_topo,WRITE_TOPO)
(char filename[],
 float *west, float *east, float *north, float *south,
 int *rows, int *columns,
 float Topo[]) /*[rows][columns];*/
 /*char  Water[];[rows][columns];, Water )*/
{
  int f;
  struct topo_header header;
  short int *val;
  short int z;
  int rc, rowscols;
  mode_t mask = 0666;
  char* Water = NULL;

  rowscols = *rows * *columns;
  val = (short int*) malloc(rowscols*sizeof(short int));
  if (val == NULL) return 0;

  strcpy(header.id, "TOPO2");  /* TOPO = int values, TOPO2 = float values */
  header.westlon = *west;
  header.eastlon = *east;
  header.northlat = *north;
  header.southlat = *south;
  header.rows = *rows;
  header.columns = *columns;

  /* Convert each elevation to a short integer, multiply it by two and */
  /* add 1 if the Water flag is set */
  for (rc=0;rc<rowscols;rc++) {
    z = (short int) Topo[rc];
    if (Water != NULL) {
      if (Water[rc]) {
	val[rc] = (z * 2) + 1;
      }
      else {
	val[rc] = z * 2;
      }
    }
    else {
      if (Topo[rc] <= 0.0001) {
	val[rc] = (z * 2) + 1;
      }
      else {
	val[rc] = z * 2;
      }
    }
  }

  f = open( filename, O_WRONLY|O_CREAT, mask );
  if (f==-1) {
    free(val);
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
    write_int2_array( f, val, rowscols );
    close(f);
    free(val);
    return 1;
  }
}

