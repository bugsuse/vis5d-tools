/* topoinfo.c */

/*
vis5d program for visualizing five dimensional gridded data sets
Copyright (C) 1990, 1991, 1992  Bill Hibbard, Brian Paul, Dave Santek,
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


/*
 * Print information about a Vis5D topo file.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "binio.h"




main( argc, argv )
int argc;
char *argv[];
{
   char id[40];
   float west, east, north, south;
   int rows, columns;
   mode_t mask;
   int fd;
   short *data;
   int i;
   float min, max;
   int water;

   if (argc==1) {
      printf("Usage:\n");
      printf("   topoinfo topofile\n");
      exit(0);
   }

   fd = open( argv[1], O_RDONLY );
   if (fd==-1) {
      printf("Error: couldn't open %s for reading\n", argv[1] );
      return 0;
   }

   /* read the header */
   read_bytes( fd, id, 40 );
   if (strcmp(id,"TOPO2")==0) {
      /* floating point values */
      read_float4( fd, &west );
      read_float4( fd, &east );
      read_float4( fd, &north );
      read_float4( fd, &south );
   }
   else {
      /* integer values */
      read_int4( fd, &i );   west = i / 100.0;
      read_int4( fd, &i );   east = i / 100.0;
      read_int4( fd, &i );   north = i / 100.0;
      read_int4( fd, &i );   south = i / 100.0;
   }
   read_int4( fd, &rows );
   read_int4( fd, &columns );

   data = (short *) malloc( rows * columns * sizeof(short) );
   if (!data) {
      printf("malloc failed!\n");
      exit(0);
   }

   read_int2_array( fd, data, rows*columns );

   close(fd);

   min = 1.0e30;
   max = -min;
   water = 0;
   for (i=0;i<rows*columns;i++) {
      float height;
      if (data[i]&1) {
         water++;
      }
      height = data[i] / 2;
      if (height<min)  min = height;
      if (height>max)  max = height;
   }

   printf("Topo file: %s\n", argv[1] );
   printf("North bound: %g deg\n", north );
   printf("South bound: %g deg\n", south );
   printf("West bound: %g deg\n", west );
   printf("East bound: %g deg\n", east );
   printf("Rows: %d\n", rows );
   printf("Columns: %d\n", columns);

   printf("min height: %g meters\n", min);
   printf("max height: %g meters\n", max);

   printf("%% water flags: %d\n",  100 * water / (rows*columns) );
}

