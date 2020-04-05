/* makemap.c */

/* Vis5D version 4.3 */

/*
vis5d program for visualizing five dimensional gridded data sets
Copyright (C) 1990-1997 Bill Hibbard, Brian Paul, Dave Santek,
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
 * Utility program to make your own map outline files.
 *
 * A map outline file consists of a number of polylines.  A polyline
 * is a sequence of connected line segments.  The end points (vertices)
 * of the lines are defined in degrees of latitude and longitude.
 *
 * Compile with:   make -f makemap.m
 * 
 */




#include <stdio.h>
#include "binio.h"




main( argc, argv )
int argc;
char *argv[];
{
   /****** Replace this example code with your own! *****/

   /* this example writes a mapfile which spells out "ABC" like this:

                                               |
                                               |    L
         2*       7*-*8      12*-----*11       +50  a
         / \       |  \        |               |    t
        /   \      |   \       |               |    i
      1*-----*3   6*----*9     |               +35  t
       |     |     |    |      |               |    u
       |     |     |    |      |               |    d
      0*     *4   5*----*10  13*-----*14       +20  e
                                               |
  -----+-----+-----+----+------+-----+---------+---
      130   120   110  100     90    80        |0
                    Longitude                  |


   The asterisks are the vertices.  The polylines are defined by:
      0,1,2,3,4
      1,3
      5,6,7,8,9,10,5
      6,9
      11,12,13,14

   Below is the code to make this map.  All you have to do is call
   these functions:

       vertex( lat, lon )   - to specify the next vertex in lat/lon
       end_line()           - to signal the end of a polyline
       done( filename )     - to write the named map file


   If you use this example unchanged then you can view the map with:
       vis5d LAMPS.v5d -map util/OUTLABC
   */

   /* Letter A */
   vertex( 20.0, 130.0 );  /* vertex 0 */
   vertex( 35.0, 130.0 );  /* vertex 1 */
   vertex( 50.0, 125.0 );  /* vertex 2 */
   vertex( 35.0, 120.0 );  /* vertex 3 */
   vertex( 20.0, 120.0 );  /* vertex 4 */
   end_line();
   vertex( 35.0, 130.0 );  /* vertex 1 */
   vertex( 35.0, 120.0 );  /* vertex 3 */
   end_line();

   /* Letter B */
   vertex( 20.0, 110.0 );  /* vertex 5 */
   vertex( 35.0, 110.0 );
   vertex( 50.0, 110.0 );
   vertex( 50.0, 104.0 );
   vertex( 35.0, 100.0 );
   vertex( 20.0, 100.0 );  /* vertex 10 */
   vertex( 20.0, 110.0 );  /* vertex 5 */
   end_line();
   vertex( 35.0, 110.0 );
   vertex( 35.0, 100.0 );
   end_line();

   /* Letter C */
   vertex( 50.0, 80.0 );
   vertex( 50.0, 90.0 );
   vertex( 20.0, 90.0 );
   vertex( 20.0, 80.0 );
   end_line();

   /* Write the file! */
   done( "OUTLABC" );

   exit(0);
}




/**********************************************************************/
/***        No changes beyond this point should be necessary.       ***/
/**********************************************************************/



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
        int       lat, lon;
};


#define MAXVERTS 100000
struct vertex VertexList[MAXVERTS];
int NumVertices = 0;


initialize()
{
   NumLines = NumVertices = 0;
   MapLine[0].minlat =  10000000;
   MapLine[0].maxlat = -10000000;
   MapLine[0].minlon =  10000000;
   MapLine[0].maxlon = -10000000;
   MapLine[0].start = 0;
   MapLine[0].len = 0;
}



vertex( lat, lon )
float lat, lon;
{
   static int init_flag = 1;
   if (init_flag) {
      initialize();
      init_flag = 0;
   }

   if (NumVertices>=MAXVERTS) {
      printf("Out of space for map vertices!\n");
      exit(1);
   }
   else {     
      int ilat = (int) (lat * 10000.0);
      int ilon = (int) (lon * 10000.0);
      VertexList[NumVertices].lat = ilat;
      VertexList[NumVertices].lon = ilon;
      NumVertices++;
      MapLine[NumLines].len++;
      if (ilat > MapLine[NumLines].maxlat)   MapLine[NumLines].maxlat = ilat;
      if (ilat < MapLine[NumLines].minlat)   MapLine[NumLines].minlat = ilat;
      if (ilon > MapLine[NumLines].maxlon)   MapLine[NumLines].maxlon = ilon;
      if (ilon < MapLine[NumLines].minlon)   MapLine[NumLines].minlon = ilon;
   }
}



end_line()
{
   if (NumVertices==0 || MapLine[NumLines].len==0) {
      printf("Error:  must call vertex() before end_line()!\n");
      exit(1);
   }

   MapLine[NumLines].len *= 2;
   NumLines++;
   if (NumLines>=MAXLINES) {
      printf("Error: out of space for lines!\n");
      exit(1);
   }
   MapLine[NumLines].minlat =  10000000;
   MapLine[NumLines].maxlat = -10000000;
   MapLine[NumLines].minlon =  10000000;
   MapLine[NumLines].maxlon = -10000000;
   MapLine[NumLines].start = NumVertices;
   MapLine[NumLines].len = 0;
}



done( filename )
char *filename;
{
   int f;
   mode_t mask;
   int i;

   /* Convert MapLine[].start values from index into VertexList to
    * absolute file positions (in 4-byte units).
    */
   for (i=0;i<NumLines;i++) {
      int index, pos;
      index = MapLine[i].start;
      pos = 6*NumLines + index*2 + 1;
      MapLine[i].start = pos;
   }


   mask = 0666;
   f = open( filename, O_WRONLY | O_CREAT | O_TRUNC, mask );
   if (f<0) {
      printf("Error:  unable to open %s for writing\n", filename );
      exit(0);
   }

   write_int4( f, NumLines );

   if (write_int4_array( f, (int *) MapLine, 6*NumLines ) < 6*NumLines) {
      printf("Error:  bad file write (disk full?)\n");
      exit(0);
   }

   if (write_int4_array( f, (int *) VertexList, 2*NumVertices ) < 2*NumVertices) {
      printf("Error:  bad file write (disk full?)\n");
      exit(0);
   }

   close( f );
}


