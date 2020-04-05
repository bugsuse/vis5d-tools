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



#include <math.h>
#include "globals.h"
#include "graphics.h"




#define DEFAULT_CURVE    1.4
#define DEFAULT_BIAS     1.0
#define DEFAULT_ALPHAPOW  2.0

/* These values must match the ones in lui/colorbar.h!!!! */
#define CURVE    0
#define BIAS     1
#define ALPHAPOW 2
#define ALPHAVAL 3
#define DRAWFLAG 4
#define MINALPHA 5
#define MAXALPHA 6



/*
 * Reset the RGB and/or Alpha curve parameters to their defaults.
 */
int vis5d_colorbar_init_params( float params[], int rgb_flag, int alpha_flag )
{
   if (rgb_flag) {
      params[CURVE] = DEFAULT_CURVE;
      params[BIAS]  = DEFAULT_BIAS;
   }
   if (alpha_flag) {
      params[ALPHAPOW] = DEFAULT_ALPHAPOW;
   }
   return 0;
}



/*
 * Recompute the RGB and/or Alpha table entries from their parameters.
 */
int vis5d_colorbar_recompute( unsigned int table[], int size, float params[],
                              int rgb_flag, int alpha_flag )
{
   float curve, rfact;
   int i;

   rfact = 0.5 * params[BIAS];
   curve = params[CURVE];

   if (alpha_flag) {
      if (params[ALPHAVAL]==-1) {
         params[MINALPHA] = 255;
         params[MAXALPHA] = 0;
      }
      else {
         params[MINALPHA] = params[ALPHAVAL];
         params[MAXALPHA] = params[ALPHAVAL];
      }
   }


   /* NOTE size-1 because last entry is used for missing data */
   for (i=0;i<size-1;i++) {
      int r,g,b,a;
      float s;

      /* compute s in [0,1] */
      s = (float) i / (float) (size-1);

      if (rgb_flag) {
         float t = curve * (s - rfact);   /* t in [curve*-0.5,curve*0.5) */
         r = 128.0 + 127.0 * atan( 7.0*t ) / 1.57;
         g = 128.0 + 127.0 * (2 * exp(-7*t*t) - 1);
         b = 128.0 + 127.0 * atan( -7.0*t ) / 1.57;
      }
      else {
         /* use current RGB */
         r = UNPACK_RED(   table[i] );
         g = UNPACK_GREEN( table[i] );
         b = UNPACK_BLUE(  table[i] );
      }

      if (alpha_flag) {
         if (params[ALPHAVAL]==-1) {
            /* Init alpha curve */
            a = 255.0 * pow( s, params[ALPHAPOW] );
         }
         else {
            /* Init constant alpha */
            a = params[ALPHAVAL];
         }
         if (a<params[MINALPHA])  params[MINALPHA] = a;
         if (a>params[MAXALPHA])  params[MAXALPHA] = a;
      }
      else {
         /* don't change alpha */
         a = UNPACK_ALPHA( table[i] );
      }

      /* store new packed color */
      table[i] = PACK_COLOR( r, g, b, a );
   }

   table[size-1] = PACK_COLOR( 0, 0, 0, 0 );
   return 0;
}




int vis5d_colorbar_set_alpha( float params[], float alpha )
{
   params[ALPHAVAL] = alpha;
   if (alpha<0) {
      params[MINALPHA] = 0;
      params[MAXALPHA] = 255;
      params[ALPHAPOW] = DEFAULT_ALPHAPOW;
   }
   else {
      params[MINALPHA] = alpha;
      params[MAXALPHA] = alpha;
      params[ALPHAVAL] = alpha;
   }
   return 0;
}


