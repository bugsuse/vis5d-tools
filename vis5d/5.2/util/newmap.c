/* newmap.c */

/* Vis5D version 4.2 */

/*
Vis5d program for visualizing five dimensional gridded data sets
Copyright (C) 1990-1995 Bill Hibbard, Brian Paul, Dave Santek,
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
 * Utility to transform the vertices in a McIDAS map file with an arbitrary
 * function.
 *
 * Edit the mapfunc.f file so that NEWLAT and NEWLON are computed as you
 * want from the OLDLAT and OLDLON values.
 *
 * Compile "newmap.c" and "mapfunc.f" to produce "newmap" using the newmap.m
 * makefile.
 */




#include <stdio.h>
#include "binio.h"


struct polyline {
        int        minlat;
        int        maxlat;
        int        minlon;
        int        maxlon;
        int        start;
        int        len;
};


#define MAXLINES 100000
struct polyline MapLine[MAXLINES];
int NumLines = 0;



struct vertex {
        int        lat, lon;
};


#define MAXVERTS 100000
struct vertex VertexList[MAXVERTS];
int NumVertices = 0;


#ifdef UNDERSCORE
extern int transform_();
#else
extern int transform();
#endif


main( argc, argv )
int argc;
char *argv[];
{
   int flag;

   if (argc!=3 && argc!=4) {
      printf("Usage:\n");
      printf("    newmap oldmap newmap [-p]\n");
      printf("-p option prints old and new coordinates as they're computed\n");
      exit(0);
   }

   read_mapfile( argv[1] );

   if (argc==4 && strcmp(argv[3],"-p")==0)
        flag = 1;
   else
        flag = 0;
   transform_map( flag );

   write_mapfile( argv[2] );

   exit(0);
}




read_mapfile( filename )
char *filename;
{
   int f;
   int i, minstart;

   f = open( filename, O_RDONLY );
   if (!f) {
      printf("Error: Unable to open %s for reading\n", filename );
      exit(0);
   }

   /* read number of polylines */
   read_int4( f, &NumLines );

   /* read polyline list */
   if (read_int4_array( f, MapLine, NumLines*6 ) < NumLines*6) {
      printf("Error: bad map file\n");
      exit(0);
   }

   minstart = 100000;
   for (i=0;i<NumLines;i++) {
      if (MapLine[i].start<minstart)
        minstart = MapLine[i].start;
   }

   /* read vertex list */
   NumVertices = read_int4_array( f, VertexList, 2*MAXVERTS ) / 2;

   close(f);
}



transform_map( pflag )
int pflag;
{
   int i, n = 0;
   
   if (pflag)
      printf("Vertex   OldLat, OldLon   ->   NewLat,  NewLon\n");

   /* call external FORTRAN subroutine to transform vertices */
   for (i=0;i<NumVertices;i++) {
      float oldlat, oldlon, newlat, newlon;
      if ( (unsigned long) VertexList[i].lat != 0x80808080 ) {
         oldlat = (float) VertexList[i].lat / 10000.0;
         oldlon = (float) VertexList[i].lon / 10000.0;
#ifdef UNDERSCORE
         transform_( &oldlat, &oldlon, &newlat, &newlon );
#else
         transform( &oldlat, &oldlon, &newlat, &newlon );
#endif
         n++;
         if (pflag)
            printf("%5d: %8.4f, %8.4f -> %8.4f, %8.4f\n", n,
                 oldlat, oldlon, newlat, newlon );
         VertexList[i].lat = (int) (newlat * 10000.0);
         VertexList[i].lon = (int) (newlon * 10000.0);
      }
   }

   /* update the min/max lat/lon values in the polyline list */
   for (i=0;i<NumLines;i++) {
      int minlat, maxlat, minlon, maxlon;
      int j, p;

      /* convert mapline[].start from a file word position to an */
      /* index into the VertexList[] array */
      p = (MapLine[i].start - 1 - NumLines*6) / 2;

      minlat = maxlat = VertexList[p].lat;
      minlon = maxlon = VertexList[p].lon;

/*
      printf("old values: %d %d %d %d\n",
             MapLine[i].minlat, MapLine[i].maxlat,
             MapLine[i].minlon, MapLine[i].maxlon );
*/

      for (j=1;j<MapLine[i].len/2;j++) {
         int lat, lon;
         lat = VertexList[p+j].lat;
         lon = VertexList[p+j].lon;
         if (lat<minlat)
           minlat = lat;
         if (lat>maxlat)
           maxlat = lat;
         if (lon<minlon)
           minlon = lon;
         if (lon>maxlon)
           maxlon = lon;
      }

      MapLine[i].minlat = minlat;
      MapLine[i].maxlat = maxlat;
      MapLine[i].minlon = minlon;
      MapLine[i].maxlon = maxlon;

/*
      printf("new values: %d %d %d %d\n",
             MapLine[i].minlat, MapLine[i].maxlat,
             MapLine[i].minlon, MapLine[i].maxlon );
*/
   }

}



write_mapfile( filename )
char *filename;
{
   int f;

   f = open( filename, O_WRONLY | O_CREAT );
   if (!f) {
      printf("Error:  unable to open %s for writing\n", filename );
      exit(0);
   }

   write_int4( f, NumLines );

   if (write_int4_array( f, MapLine, 6*NumLines ) < 6*NumLines) {
      printf("Error:  bad file write (disk full?)\n");
      exit(0);
   }

   if (write_int4_array( f, VertexList, 2*NumVertices ) < 2*NumVertices) {
      printf("Error:  bad file write (disk full?)\n");
      exit(0);
   }

   close( f );
}
