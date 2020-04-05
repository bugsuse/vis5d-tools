/* topo.c */
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
 * Earth topography
 */



#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "binio.h"



static float Topo_westlon, Topo_eastlon, Topo_northlat, Topo_southlat;
static int Topo_rows, Topo_cols;
static short *TopoData;



/*** load_topo ********************************************************
   Read a topography file and initialize Topo and TopoData.
   Input:  filename - name of topo file.
   Return:  0 if error, otherwise non-zero for success.
**********************************************************************/
int load_topo( char filename[] )
{
   static int already_loaded = 0;
   int f;
   char id[40];

   if (!already_loaded) {

      f = open( filename, O_RDONLY );
      if (f<0) {
/*         printf("Topo file %s not found\n", filename );*/
         return 0;
      }

      /* Read file header */
      read_bytes( f, id, 40 );
      if (strcmp(id,"TOPO")==0) {
         /* OLD STYLE: bounds given as ints */
         int k;
         read_int4( f, &k );   Topo_westlon = k / 100.0;
         read_int4( f, &k );   Topo_eastlon = k / 100.0;
         read_int4( f, &k );   Topo_northlat = k / 100.0;
         read_int4( f, &k );   Topo_southlat = k / 100.0;
      }
      else if (strcmp(id,"TOPO2")==0) {
         /* NEW STYLE: bounds given as floats */
         read_float4( f, &Topo_westlon );
         read_float4( f, &Topo_eastlon );
         read_float4( f, &Topo_northlat );
         read_float4( f, &Topo_southlat );
      }
      else {
         printf("%s is not a TOPO file\n", filename);
         close(f);
         return 0;
      }
      read_int4( f, &Topo_rows );
      read_int4( f, &Topo_cols );


      /* allocate storage for the topo values */
      TopoData = (short *) malloc( Topo_rows*Topo_cols*sizeof(short) );
      if (!TopoData) {
         close(f);
         return 0;
      }

      /* read the topo values */
      read_int2_array( f, TopoData, Topo_rows*Topo_cols );

      /* all done */
      close(f);

      already_loaded = 1;
   }

   return 1;
}



static int LatSamples = 1;
static int LonSamples = 1;



/*
 * Before calling the elevation function below, one should first call
 * this function to indicate approximately how many degrees of latitude
 * and longitude are between adjacent samplings (i.e. calls to elevation()).
 * With this hint we can determine how many samples of the topography to
 * take then average together in the elevation() function.
 * Input:  latres - latitude resolution
 *         lonres - longitude resolution
 */
void set_topo_sampling( float latres, float lonres )
{
   LatSamples = (int) (latres / ((Topo_northlat-Topo_southlat) / (Topo_rows-1)));
   LonSamples = (int) (lonres / ((Topo_westlon-Topo_eastlon) / (Topo_cols-1)));
   if (LatSamples<=0)  LatSamples = 1;
   if (LonSamples<=0)  LonSamples = 1;
}



/* TODO:  sampling */


/*** elevation ********************************************************
   Return the elevation of the topography at location (lat,lon) and a
   flag indicating water or land.
   Input:  lat, lon - location in degrees
           water - pointer to integer
   Output:  water - set to 1 if water, 0 if land.
   Returned:  elevation in meters at (lat,lon) or 0 if error.
**********************************************************************/
float elevation_i( float lat, float lon, int *water )
{
   float fr, fc;
   int rowa, cola, rowb, colb;
   float hgt;
   int count, r, c;
   int val, watcount;

   if (!TopoData || lon<Topo_eastlon || lon>Topo_westlon
       || lat<Topo_southlat || lat>Topo_northlat) {
      if (water)
         *water = 0;
      return 0.0;
   }


   /* Return elevation at (lat,lon) by sampling LatSamples x LonSamples */
   /* values centered at that location. */

   /* calculate range of rows */
   fr = Topo_rows * (lat - Topo_northlat) / (Topo_southlat - Topo_northlat);
   rowa = (int) fr - LatSamples/2;
   rowb = rowa+LatSamples;
   if (rowa<0)
      rowa = 0;
   if (rowb>=Topo_rows)
      rowb = Topo_rows-1;

   /* calculate range of columns */
   fc = Topo_cols * (lon-Topo_westlon) / (Topo_eastlon - Topo_westlon);
   cola = (int) fc - LonSamples/2;
   colb = cola+LonSamples;
   if (cola<0)
      cola = 0;
   if (colb>=Topo_cols)
      colb = Topo_cols-1;

   /* find average height in sample area */
   hgt = 0.0;
   count = watcount = 0;
   for (r=rowa;r<=rowb;r++) {
      for (c=cola;c<=colb;c++) {
         val = TopoData[r*Topo_cols+c];
         if (val&1)
            watcount++;
         hgt += (float) (val / 2);
         count++;
      }
   }
   hgt = hgt / (float) count;

   /* calculate water flag */
   if (water) {
      if (watcount>count/2)
        *water = 1;
      else
        *water = 0;
   }

   return hgt;
}



/*
 * Deallocate memory used for storing topography.
 */
void free_topo_i( void )
{
   if (TopoData) {
      free( TopoData );
      TopoData = NULL;
   }
}



