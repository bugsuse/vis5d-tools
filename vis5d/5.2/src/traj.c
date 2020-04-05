/* traj.c */

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
#include <stdlib.h>
#include "globals.h"
#include "grid.h"
#include "proj.h"


#define TRUNC(X)  ( (float) (int) (X) )






/*
 * Initialize the trajectory tracing module.
 * Return:  1 = ok, 0 = error
 */
int init_traj( Context ctx )
{
   int i, j;
   float lata, lona, latb, lonb, latc, lonc;
   float d, us, vs;
   float lat0, lon0, lat1, lon1, lat2, lon2;
   float midrow, midcol;
   int var = 0;
   
   /* the index of any wind variable */
   if (ctx->dpy_ctx->TrajU>=0)  var = ctx->dpy_ctx->TrajU;
   else if (ctx->dpy_ctx->TrajV>=0)  var= ctx->dpy_ctx->TrajV;
   else if (ctx->dpy_ctx->TrajW>=0)  var= ctx->dpy_ctx->TrajW;


   /* Compute initial trajectory step and length values */
   switch (ctx->Projection) {
      case PROJ_GENERIC:
         ctx->TrajStep = 1.0;
         ctx->TrajLength = 1.0;
         break;
      default:
         /* TODO: verify this is ok: (seems to work) */
         /* This is tricky:  compute distance, in meters, from left to */
         /* right edge of domain.  Do it in two steps to prevent "wrap- */
         /* around" when difference in longitudes > 180 degrees. */
         midrow = (float) ctx->Nr / 2.0;
         midcol = (float) ctx->Nc / 2.0;
         rowcol_to_latlon( ctx, -1, -1, midrow, 0.0, &lat0, &lon0 );
         rowcol_to_latlon( ctx, -1, -1, midrow, midcol, &lat1, &lon1 );
         rowcol_to_latlon( ctx, -1, -1, midrow, (float) (ctx->Nc-1), &lat2, &lon2 );
         d = earth_distance( lat0, lon0, lat1, lon1 )
           + earth_distance( lat1, lon1, lat2, lon2 );
         ctx->TrajStep = TRUNC( (d / 100.0) / 25.0 );
         /* the above was: (float) ctx->Elapsed[1] / 2.0;*/
         ctx->TrajLength = 5.0 * ctx->TrajStep;
         /* the above was: (float) ctx->Elapsed[1]; */
         break;
   }

   /* These values are set by the user through the trajectory widget */
   /* They are just multipliers of the internal values TrajStep and */
   /* TrajLength: */
   ctx->dpy_ctx->UserTrajStep = ctx->dpy_ctx->UserTrajLength = 1.0;

   /*
    * Compute m/s to boxes/s scaling factors for U and V components
    */
   switch (ctx->Projection) {
      case PROJ_GENERIC:
         /* for a generic projection, we assume U, and V velocities are in */
         /* X per second, where X is the same units used for NorthBound, */
         /* WestBound, ctx->RowInc, and ctx->ColInc */
         us = 1.0 / ctx->ColInc;
         vs = -1.0 / ctx->RowInc;
         for (i=0;i<ctx->Nr;i++) {
            for (j=0;j<ctx->Nc;j++) {
               ctx->Uscale[i][j] = us;
               ctx->Vscale[i][j] = vs;
            }
         }
         break;
      case PROJ_LINEAR:
      case PROJ_LAMBERT:
      case PROJ_STEREO:
      case PROJ_ROTATED:
      case PROJ_CYLINDRICAL:
      case PROJ_SPHERICAL:
      case PROJ_MERCATOR:
         for (i=0;i<ctx->Nr;i++) {
            for (j=0;j<ctx->Nc;j++) {

               float ii = (float) i;
               float jj = (float) j;

               /* Compute U scale */
               if (j==0) {
                  rowcol_to_latlon( ctx, 0, var, ii, jj,     &lata, &lona );
                  rowcol_to_latlon( ctx, 0, var, ii, jj+1.0, &latb, &lonb );
                  /* WLH 3-21-97 */
                  if (lata > 89.9) lata = 89.9;
                  if (lata < -89.9) lata = -89.9;
                  if (latb > 89.9) latb = 89.9;
                  if (latb < -89.9) latb = -89.9;
                  d = earth_distance( lata, lona, latb, lonb );
               }
               else if (j==ctx->Nc-1) {
                  rowcol_to_latlon( ctx, 0, var, ii, jj-1.0, &lata, &lona );
                  rowcol_to_latlon( ctx, 0, var, ii, jj,     &latb, &lonb );
                  /* WLH 3-21-97 */
                  if (lata > 89.9) lata = 89.9;
                  if (lata < -89.9) lata = -89.9;
                  if (latb > 89.9) latb = 89.9;
                  if (latb < -89.9) latb = -89.9;
                  d = earth_distance( lata, lona, latb, lonb );
               }
               else {
                  rowcol_to_latlon( ctx, 0, var, ii, jj-1.0, &lata, &lona );
                  rowcol_to_latlon( ctx, 0, var, ii, jj,     &latb, &lonb );
                  rowcol_to_latlon( ctx, 0, var, ii, jj+1.0, &latc, &lonc );
                  /* WLH 3-21-97 */
                  if (lata > 89.9) lata = 89.9;
                  if (lata < -89.9) lata = -89.9;
                  if (latb > 89.9) latb = 89.9;
                  if (latb < -89.9) latb = -89.9;
                  if (latc > 89.9) latc = 89.9;
                  if (latc < -89.9) latc = -89.9;
                  d = (earth_distance( lata,lona, latb,lonb)
                     + earth_distance( latb,lonb, latc,lonc)) / 2.0;
               }
               ctx->Uscale[i][j] = 1.0 / d;

               /* Compute V scale */
               if (i==0) {
                  rowcol_to_latlon( ctx, 0, ctx->dpy_ctx->TrajV, ii,     jj, &lata, &lona );
                  rowcol_to_latlon( ctx, 0, ctx->dpy_ctx->TrajV, ii+1.0, jj, &latb, &lonb );
                  /* WLH 3-21-97 */
                  if (lata > 89.9) lata = 89.9;
                  if (lata < -89.9) lata = -89.9;
                  if (latb > 89.9) latb = 89.9;
                  if (latb < -89.9) latb = -89.9;
                  d = earth_distance( lata, lona, latb, lonb );
               }
               else if (j==ctx->Nc-1) {
                  rowcol_to_latlon( ctx, 0, ctx->dpy_ctx->TrajV, ii-1.0, jj, &lata, &lona );
                  rowcol_to_latlon( ctx, 0, ctx->dpy_ctx->TrajV, ii,     jj, &latb, &lonb );
                  /* WLH 3-21-97 */
                  if (lata > 89.9) lata = 89.9;
                  if (lata < -89.9) lata = -89.9;
                  if (latb > 89.9) latb = 89.9;
                  if (latb < -89.9) latb = -89.9;
                  d = earth_distance( lata, lona, latb, lonb );
               }
               else {
                  rowcol_to_latlon( ctx, 0, ctx->dpy_ctx->TrajV, ii-1.0, jj, &lata, &lona );
                  rowcol_to_latlon( ctx, 0, ctx->dpy_ctx->TrajV, ii,     jj, &latb, &lonb );
                  rowcol_to_latlon( ctx, 0, ctx->dpy_ctx->TrajV, ii+1.0, jj, &latc, &lonc );
                  /* WLH 3-21-97 */
                  if (lata > 89.9) lata = 89.9;
                  if (lata < -89.9) lata = -89.9;
                  if (latb > 89.9) latb = 89.9;
                  if (latb < -89.9) latb = -89.9;
                  if (latc > 89.9) latc = 89.9;
                  if (latc < -89.9) latc = -89.9;
                  d = (earth_distance( lata,lona, latb,lonb)
                     + earth_distance( latb,lonb, latc,lonc)) / 2.0;
               }
               ctx->Vscale[i][j] = -1.0 / d;     /* Note negative!!! */

            }
         }
         break;
      default:
         printf("Error in init_traj: Projection=%d\n", ctx->Projection);
         return 0;
   }

   /*
    * Compute m/s to boxes/s scaling factors for W component
    */
   switch (ctx->VerticalSystem) {
      case VERT_GENERIC:
         for (i=0;i<ctx->MaxNl;i++) {
            ctx->Wscale[i] = 1.0 / ctx->LevInc;
         }
         break;
      case VERT_EQUAL_KM:
         for (i=0;i<ctx->MaxNl;i++) {
            ctx->Wscale[i] = 1.0 / (ctx->LevInc * 1000.0);
         }
         break;
      case VERT_NONEQUAL_MB:
      case VERT_NONEQUAL_KM:
         for (i=0;i<ctx->MaxNl;i++) {
            if (i==0) {
               float hgt1 = gridlevel_to_height( ctx, 1.0 );
               float hgt0 = gridlevel_to_height( ctx, 0.0 );
               float diff = hgt1-hgt0;
               if (fabs(diff) < 0.000001) diff = 0.000001; 
               ctx->Wscale[i] = 1.0 / (diff * 1000.0);
/*               ctx->Wscale[i] = 1.0 / ((hgt1-hgt0) * 1000.0); */
            }
            else if (i==ctx->MaxNl-1) {
               float hgt1 = gridlevel_to_height( ctx, (float)(ctx->MaxNl-1) );
               float hgt0 = gridlevel_to_height( ctx, (float)(ctx->MaxNl-2) );
               float diff = hgt1-hgt0;
               if (fabs(diff) < 0.000001) diff = 0.000001; 
               ctx->Wscale[i] = 1.0 / (diff * 1000.0);
/*               ctx->Wscale[i] = 1.0 / ((hgt1-hgt0) * 1000.0); */
            }
            else {
               float a, b;
               float hgt2 = gridlevel_to_height( ctx, (float)(i+1) );
               float hgt1 = gridlevel_to_height( ctx, (float) i );
               float hgt0 = gridlevel_to_height( ctx, (float)(i-1) );
               float diffa = hgt1-hgt0;
               float diffb = hgt2-hgt1;
               if (fabs(diffa) < 0.000001) diffa = 0.000001; 
               if (fabs(diffb) < 0.000001) diffb = 0.000001; 
               a = 1.0 / (diffa * 1000.0);
               b = 1.0 / (diffb * 1000.0);
/*
               float a = 1.0 / ((hgt1-hgt0) * 1000.0);
               float b = 1.0 / ((hgt2-hgt1) * 1000.0);
*/
               ctx->Wscale[i] = (a+b) / 2.0;
            }
         }
         break;
      default:
         printf("Error in init_traj: ctx->VerticalSystem=%d\n", ctx->VerticalSystem);
         return 0;
   }
   return 1;
}



/*
 * Return U, V, and W for a discrete position in time and space.
 * Return:  1 = ok, 0 = missing data.
 */
static int get_discrete_uvw( Context ctx, int time, int r, int c, int l,
                             float *uptr, float *vptr, float *wptr )
{
   float u = get_grid_value( ctx, time, ctx->dpy_ctx->TrajU, r, c, l );
   float v = get_grid_value( ctx, time, ctx->dpy_ctx->TrajV, r, c, l );
   float w = get_grid_value( ctx, time, ctx->dpy_ctx->TrajW, r, c, l );

   if (IS_MISSING(u) || IS_MISSING(v) || IS_MISSING(w)) {
      /* missing */
      return 0;
   }
   else {
      /* return u,v,w in boxes/sec */
      *uptr = u * ctx->Uscale[r][c];
      *vptr = v * ctx->Vscale[r][c];
      *wptr = w * ctx->Wscale[l];
      return 1;
   }
}

static int get_discrete_uv( Context ctx, int time, int r, int c, int l,
                             float *uptr, float *vptr, float *wptr ) 
{   
   float u = get_grid_value( ctx, time, ctx->dpy_ctx->TrajU, r, c, l ); 
   float v = get_grid_value( ctx, time, ctx->dpy_ctx->TrajV, r, c, l ); 
    
   if (IS_MISSING(u) || IS_MISSING(v)) {
      /* missing */
      return 0;
   }
   else {  
      /* return u,v,w in boxes/sec */
      *uptr = u * ctx->Uscale[r][c];
      *vptr = v * ctx->Vscale[r][c];
      *wptr = 0;
      return 1;
   }
}   

int init_trajPRIME( Display_Context dtx )
{
   int i, j;
   float lata, lona, latb, lonb, latc, lonc;
   float d, us, vs;
   float lat0, lon0, lat1, lon1, lat2, lon2;
   float midrow, midcol;
   int var = 0;


   dtx->UserTrajStep = dtx->UserTrajLength = 1.0;

   switch (dtx->Projection) {
      case PROJ_GENERIC:
         dtx->TrajStep = 1.0;
         dtx->TrajLength = 1.0;
         break;
      default:
         /* TODO: verify this is ok: (seems to work) */
         /* This is tricky:  compute distance, in meters, from left to */
         /* right edge of domain.  Do it in two steps to prevent "wrap- */
         /* around" when difference in longitudes > 180 degrees. */
         midrow = (float) dtx->Nr / 2.0;
         midcol = (float) dtx->Nc / 2.0;
         rowcolPRIME_to_latlon( dtx, -1, -1, midrow, 0.0, &lat0, &lon0 );
         rowcolPRIME_to_latlon( dtx, -1, -1, midrow, midcol, &lat1, &lon1 );
         rowcolPRIME_to_latlon( dtx, -1, -1, midrow, (float) (dtx->Nc-1), &lat2, &lon2 );
         d = earth_distance( lat0, lon0, lat1, lon1 )
           + earth_distance( lat1, lon1, lat2, lon2 );
         dtx->TrajStep = TRUNC( (d / 100.0) / 25.0 );
         /* the above was: (float) dtx->Elapsed[1] / 2.0;*/
         dtx->TrajLength = 5.0 * dtx->TrajStep;
         /* the above was: (float) dtx->Elapsed[1]; */
         break;
   }

   
   /*
    * Compute m/s to boxes/s scaling factors for U and V components
    */
   switch (dtx->Projection) {
      case PROJ_GENERIC:
         /* for a generic projection, we assume U, and V velocities are in */
         /* X per second, where X is the same units used for NorthBound, */
         /* WestBound, dtx->RowInc, and dtx->ColInc */
         us = 1.0 / dtx->ColInc;
         vs = -1.0 / dtx->RowInc;
         for (i=0;i<dtx->Nr;i++) {
            for (j=0;j<dtx->Nc;j++) {
               dtx->Uscale[i][j] = us;
               dtx->Vscale[i][j] = vs;
            }
         }
         break;
      case PROJ_LINEAR:
      case PROJ_LAMBERT:
      case PROJ_STEREO:
      case PROJ_ROTATED:
      case PROJ_CYLINDRICAL:
      case PROJ_SPHERICAL:
      case PROJ_MERCATOR:
         for (i=0;i<dtx->Nr;i++) {
            for (j=0;j<dtx->Nc;j++) {

               float ii = (float) i;
               float jj = (float) j;

               /* Compute U scale */
               if (j==0) {
                  rowcolPRIME_to_latlon( dtx, 0, var, ii, jj,     &lata, &lona );
                  rowcolPRIME_to_latlon( dtx, 0, var, ii, jj+1.0, &latb, &lonb );
                  /* WLH 3-21-97 */
                  if (lata > 89.9) lata = 89.9;
                  if (lata < -89.9) lata = -89.9;
                  if (latb > 89.9) latb = 89.9;
                  if (latb < -89.9) latb = -89.9;
                  d = earth_distance( lata, lona, latb, lonb );
               }
               else if (j==dtx->Nc-1) {
                  rowcolPRIME_to_latlon( dtx, 0, var, ii, jj-1.0, &lata, &lona );
                  rowcolPRIME_to_latlon( dtx, 0, var, ii, jj,     &latb, &lonb );
                  /* WLH 3-21-97 */
                  if (lata > 89.9) lata = 89.9;
                  if (lata < -89.9) lata = -89.9;
                  if (latb > 89.9) latb = 89.9;
                  if (latb < -89.9) latb = -89.9;
                  d = earth_distance( lata, lona, latb, lonb );
               }
               else {
                  rowcolPRIME_to_latlon( dtx, 0, var, ii, jj-1.0, &lata, &lona );
                  rowcolPRIME_to_latlon( dtx, 0, var, ii, jj,     &latb, &lonb );
                  rowcolPRIME_to_latlon( dtx, 0, var, ii, jj+1.0, &latc, &lonc );
                  /* WLH 3-21-97 */
                  if (lata > 89.9) lata = 89.9;
                  if (lata < -89.9) lata = -89.9;
                  if (latb > 89.9) latb = 89.9;
                  if (latb < -89.9) latb = -89.9;
                  if (latc > 89.9) latc = 89.9;
                  if (latc < -89.9) latc = -89.9;
                  d = (earth_distance( lata,lona, latb,lonb)
                     + earth_distance( latb,lonb, latc,lonc)) / 2.0;
               }
               dtx->Uscale[i][j] = 1.0 / d;

               /* Compute V scale */
               if (i==0) {
                  rowcolPRIME_to_latlon( dtx, 0, dtx->TrajV, ii,     jj, &lata, &lona );
                  rowcolPRIME_to_latlon( dtx, 0, dtx->TrajV, ii+1.0, jj, &latb, &lonb );
                  /* WLH 3-21-97 */
                  if (lata > 89.9) lata = 89.9;
                  if (lata < -89.9) lata = -89.9;
                  if (latb > 89.9) latb = 89.9;
                  if (latb < -89.9) latb = -89.9;
                  d = earth_distance( lata, lona, latb, lonb );
               }
               else if (j==dtx->Nc-1) {
                  rowcolPRIME_to_latlon( dtx, 0, dtx->TrajV, ii-1.0, jj, &lata, &lona );
                  rowcolPRIME_to_latlon( dtx, 0, dtx->TrajV, ii,     jj, &latb, &lonb );
                  /* WLH 3-21-97 */
                  if (lata > 89.9) lata = 89.9;
                  if (lata < -89.9) lata = -89.9;
                  if (latb > 89.9) latb = 89.9;
                  if (latb < -89.9) latb = -89.9;
                  d = earth_distance( lata, lona, latb, lonb );
               }
               else {
                  rowcolPRIME_to_latlon( dtx, 0, dtx->TrajV, ii-1.0, jj, &lata, &lona );
                  rowcolPRIME_to_latlon( dtx, 0, dtx->TrajV, ii,     jj, &latb, &lonb );
                  rowcolPRIME_to_latlon( dtx, 0, dtx->TrajV, ii+1.0, jj, &latc, &lonc );
                  /* WLH 3-21-97 */
                  if (lata > 89.9) lata = 89.9;
                  if (lata < -89.9) lata = -89.9;
                  if (latb > 89.9) latb = 89.9;
                  if (latb < -89.9) latb = -89.9;
                  if (latc > 89.9) latc = 89.9;
                  if (latc < -89.9) latc = -89.9;
                  d = (earth_distance( lata,lona, latb,lonb)
                     + earth_distance( latb,lonb, latc,lonc)) / 2.0;
               }
               dtx->Vscale[i][j] = -1.0 / d;     /* Note negative!!! */

            }
         }
         break;
      default:
         printf("Error in init_trajPRIME: Projection=%d\n", dtx->Projection);
         return 0;
   }

   /*
    * Compute m/s to boxes/s scaling factors for W component
    */
   switch (dtx->VerticalSystem) {
      case VERT_GENERIC:
         for (i=0;i<dtx->MaxNl;i++) {
            dtx->Wscale[i] = 1.0 / dtx->LevInc;
         }
         break;
      case VERT_EQUAL_KM:
         for (i=0;i<dtx->MaxNl;i++) {
            dtx->Wscale[i] = 1.0 / (dtx->LevInc * 1000.0);
         }
         break;
      case VERT_NONEQUAL_MB:
      case VERT_NONEQUAL_KM:
         for (i=0;i<dtx->MaxNl;i++) {
            if (i==0) {
               float hgt1 = gridlevelPRIME_to_height( dtx, 1.0 );
               float hgt0 = gridlevelPRIME_to_height( dtx, 0.0 );
               float diff = hgt1-hgt0;
               if (fabs(diff) < 0.000001) diff = 0.000001;
               dtx->Wscale[i] = 1.0 / (diff * 1000.0);
/*               dtx->Wscale[i] = 1.0 / ((hgt1-hgt0) * 1000.0); */
            }
            else if (i==dtx->MaxNl-1) {
               float hgt1 = gridlevelPRIME_to_height( dtx, (float)(dtx->MaxNl-1) );
               float hgt0 = gridlevelPRIME_to_height( dtx, (float)(dtx->MaxNl-2) );
               float diff = hgt1-hgt0;
               if (fabs(diff) < 0.000001) diff = 0.000001;
               dtx->Wscale[i] = 1.0 / (diff * 1000.0);
/*               dtx->Wscale[i] = 1.0 / ((hgt1-hgt0) * 1000.0); */
            }
            else {
               float a, b;
               float hgt2 = gridlevelPRIME_to_height( dtx, (float)(i+1) );
               float hgt1 = gridlevelPRIME_to_height( dtx, (float) i );
               float hgt0 = gridlevelPRIME_to_height( dtx, (float)(i-1) );
               float diffa = hgt1-hgt0;
               float diffb = hgt2-hgt1;
               if (fabs(diffa) < 0.000001) diffa = 0.000001;
               if (fabs(diffb) < 0.000001) diffb = 0.000001;
               a = 1.0 / (diffa * 1000.0);
               b = 1.0 / (diffb * 1000.0);
/*
               float a = 1.0 / ((hgt1-hgt0) * 1000.0);
               float b = 1.0 / ((hgt2-hgt1) * 1000.0);
*/
               dtx->Wscale[i] = (a+b) / 2.0;
            }
         }
         break;
      default:
         printf("Error in init_trajPRIME: dtx->VerticalSystem=%d\n", dtx->VerticalSystem);
         return 0;
   }
   return 1;
}


/*
 * Given two timesteps and a (row,col,lev) position perform a 4-D
 * interpolation of neighboring values to get the U,V, and W wind
 * vector components in units of grid boxes/second.
 * Input:  t0, t1 - the two timesteps
 *         at, bt - timestep weights  (at+bt=1.0)
 *         row, col, lev - the grid position
 * Output:  u, v, w - the interpolated and scaled wind vector values
 * Return:  0 = missing data was found, 1 = valid values
 */
static int get_uvw( Context ctx, int t0, int t1,
                    float at, float bt,
                    float row, float col, float lev,
                    float *u, float *v, float *w, float sl )
{
   int i, j, k;
   float bra, ara, bca, aca, bla, ala, aa, ba, ab, bb;
   float aaaa,abaa,baaa,bbaa,aaba,abba,baba,bbba,
         aaab,abab,baab,bbab,aabb,abbb,babb,bbbb;
   float ullll, ullhl, ulhll, ulhhl, uhlll, uhlhl, uhhll, uhhhl,
         ulllh, ullhh, ulhlh, ulhhh, uhllh, uhlhh, uhhlh, uhhhh;
   float vllll, vllhl, vlhll, vlhhl, vhlll, vhlhl, vhhll, vhhhl,
         vlllh, vllhh, vlhlh, vlhhh, vhllh, vhlhh, vhhlh, vhhhh;
   float wllll, wllhl, wlhll, wlhhl, whlll, whlhl, whhll, whhhl,
         wlllh, wllhh, wlhlh, wlhhh, whllh, whlhh, whhlh, whhhh;

   /* calculate weights for interpolating in box */
   i = (int) row;
   j = (int) col;
   k = (int) lev;

   bra = row-i;
   ara = 1.0-bra;
   bca = col-j;
   aca = 1.0-bca;
   bla = lev-k;
   ala = 1.0-bla;

   aa = ala*at;
   ba = bla*at;
   ab = ala*bt;
   bb = bla*bt;

   /* get 16 weights for 4-d box */
   /* note 4 a/b's are in order of j,i,k,time */
   aaaa = aca*ara*aa;
   abaa = aca*bra*aa;
   baaa = bca*ara*aa;
   bbaa = bca*bra*aa;
   aaba = aca*ara*ba;
   abba = aca*bra*ba;
   baba = bca*ara*ba;
   bbba = bca*bra*ba;

   aaab = aca*ara*ab;
   abab = aca*bra*ab;
   baab = bca*ara*ab;
   bbab = bca*bra*ab;
   aabb = aca*ara*bb;
   abbb = aca*bra*bb;
   babb = bca*ara*bb;
   bbbb = bca*bra*bb;

   /* get u,v,w grid values at 16 corners of 4-d box */
   /* note 4 l/h's are in order of j,i,k,time */
   /* return 0 if any missing data is found */
   if (!sl){
      if (!get_discrete_uvw( ctx, t0, i,j,k,       &ullll, &vllll, &wllll )) return 0;
      if (!get_discrete_uvw( ctx, t0, i,j,k+1,     &ullhl, &vllhl, &wllhl )) return 0;
      if (!get_discrete_uvw( ctx, t0, i+1,j,k,     &ulhll, &vlhll, &wlhll )) return 0;
      if (!get_discrete_uvw( ctx, t0, i+1,j,k+1,   &ulhhl, &vlhhl, &wlhhl )) return 0;
      if (!get_discrete_uvw( ctx, t0, i,j+1,k,     &uhlll, &vhlll, &whlll )) return 0;
      if (!get_discrete_uvw( ctx, t0, i,j+1,k+1,   &uhlhl, &vhlhl, &whlhl )) return 0;
      if (!get_discrete_uvw( ctx, t0, i+1,j+1,k,   &uhhll, &vhhll, &whhll )) return 0;
      if (!get_discrete_uvw( ctx, t0, i+1,j+1,k+1, &uhhhl, &vhhhl, &whhhl )) return 0;
      if (!get_discrete_uvw( ctx, t1, i,j,k,       &ulllh, &vlllh, &wlllh )) return 0;
      if (!get_discrete_uvw( ctx, t1, i,j,k+1,     &ullhh, &vllhh, &wllhh )) return 0;
      if (!get_discrete_uvw( ctx, t1, i+1,j,k,     &ulhlh, &vlhlh, &wlhlh )) return 0;
      if (!get_discrete_uvw( ctx, t1, i+1,j,k+1,   &ulhhh, &vlhhh, &wlhhh )) return 0;
      if (!get_discrete_uvw( ctx, t1, i,j+1,k,     &uhllh, &vhllh, &whllh )) return 0;
      if (!get_discrete_uvw( ctx, t1, i,j+1,k+1,   &uhlhh, &vhlhh, &whlhh )) return 0;
      if (!get_discrete_uvw( ctx, t1, i+1,j+1,k,   &uhhlh, &vhhlh, &whhlh )) return 0;
      if (!get_discrete_uvw( ctx, t1, i+1,j+1,k+1, &uhhhh, &vhhhh, &whhhh )) return 0;

      /* interpolate to get wind vector <U,V,W> */
      *u = aaaa*ullll+aaab*ulllh+aaba*ullhl+aabb*ullhh+
           abaa*ulhll+abab*ulhlh+abba*ulhhl+abbb*ulhhh+
           baaa*uhlll+baab*uhllh+baba*uhlhl+babb*uhlhh+
           bbaa*uhhll+bbab*uhhlh+bbba*uhhhl+bbbb*uhhhh;
      *v = aaaa*vllll+aaab*vlllh+aaba*vllhl+aabb*vllhh+
           abaa*vlhll+abab*vlhlh+abba*vlhhl+abbb*vlhhh+
           baaa*vhlll+baab*vhllh+baba*vhlhl+babb*vhlhh+
           bbaa*vhhll+bbab*vhhlh+bbba*vhhhl+bbbb*vhhhh;
      *w = aaaa*wllll+aaab*wlllh+aaba*wllhl+aabb*wllhh+
           abaa*wlhll+abab*wlhlh+abba*wlhhl+abbb*wlhhh+
           baaa*whlll+baab*whllh+baba*whlhl+babb*whlhh+
           bbaa*whhll+bbab*whhlh+bbba*whhhl+bbbb*whhhh;
   }
   else{
      /* only one level */
      if (!get_discrete_uv( ctx, t0, i,j,k,       &ullll, &vllll, &wllll )) return 0;
      if (!get_discrete_uv( ctx, t0, i,j,k,     &ullhl, &vllhl, &wllhl )) return 0;
      if (!get_discrete_uv( ctx, t0, i+1,j,k,     &ulhll, &vlhll, &wlhll )) return 0;
      if (!get_discrete_uv( ctx, t0, i+1,j,k,   &ulhhl, &vlhhl, &wlhhl )) return 0;
      if (!get_discrete_uv( ctx, t0, i,j+1,k,     &uhlll, &vhlll, &whlll )) return 0;
      if (!get_discrete_uv( ctx, t0, i,j+1,k,   &uhlhl, &vhlhl, &whlhl )) return 0;
      if (!get_discrete_uv( ctx, t0, i+1,j+1,k,   &uhhll, &vhhll, &whhll )) return 0;
      if (!get_discrete_uv( ctx, t0, i+1,j+1,k, &uhhhl, &vhhhl, &whhhl )) return 0;
      if (!get_discrete_uv( ctx, t1, i,j,k,       &ulllh, &vlllh, &wlllh )) return 0;
      if (!get_discrete_uv( ctx, t1, i,j,k,     &ullhh, &vllhh, &wllhh )) return 0;
      if (!get_discrete_uv( ctx, t1, i+1,j,k,     &ulhlh, &vlhlh, &wlhlh )) return 0;
      if (!get_discrete_uv( ctx, t1, i+1,j,k,   &ulhhh, &vlhhh, &wlhhh )) return 0;
      if (!get_discrete_uv( ctx, t1, i,j+1,k,     &uhllh, &vhllh, &whllh )) return 0;
      if (!get_discrete_uv( ctx, t1, i,j+1,k,   &uhlhh, &vhlhh, &whlhh )) return 0;
      if (!get_discrete_uv( ctx, t1, i+1,j+1,k,   &uhhlh, &vhhlh, &whhlh )) return 0;
      if (!get_discrete_uv( ctx, t1, i+1,j+1,k, &uhhhh, &vhhhh, &whhhh )) return 0;

      /* interpolate to get wind vector <U,V,W> */
      *u = aaaa*ullll+aaab*ulllh+aaba*ullhl+aabb*ullhh+
           abaa*ulhll+abab*ulhlh+abba*ulhhl+abbb*ulhhh+
           baaa*uhlll+baab*uhllh+baba*uhlhl+babb*uhlhh+
           bbaa*uhhll+bbab*uhhlh+bbba*uhhhl+bbbb*uhhhh;
      *v = aaaa*vllll+aaab*vlllh+aaba*vllhl+aabb*vllhh+
           abaa*vlhll+abab*vlhlh+abba*vlhhl+abbb*vlhhh+
           baaa*vhlll+baab*vhllh+baba*vhlhl+babb*vhlhh+
           bbaa*vhhll+bbab*vhhlh+bbba*vhhhl+bbbb*vhhhh;
      *w = 0;
   }

   return 1;
}



/*
 * Trace a wind trajectory.
 * Input:  row0, col0, lev0 - initial location
 *         time0 - initial timestep
 *         step - integration step size in seconds
 *         max - size of vr,vc,vl,vt arrays
 * Output:  vr, vc, vl - array of trajectory vertices
 *          vt - array of corresponding trajectory vertex times in seconds
 *               elapsed since first time step.
 * Return:  number of elements in vr,vc,vl,vt
 */
int trace( Context ctx, float row0, float col0, float lev0,
           int time0, int step, int max,
           float vr[], float vc[], float vl[], int vt[] )
{
   float row, col, lev;
   int time;   /* in seconds since first time step */
   int itime;  /* time step corresponding to 'time' */
   int i, vcount, singlelevel;
   float maxr, maxc, maxl, minl;

   /* grid bounds */
   maxr = (float) (ctx->Nr-1);
   maxc = (float) (ctx->Nc-1);
   maxl = (float) (ctx->Nl[ctx->dpy_ctx->TrajU]-1);
   minl = (float) ctx->LowLev[ctx->dpy_ctx->TrajU];

   /* allow trajs to work if u&v in one level only */
   if (maxl == 0 && lev0 == minl){
      singlelevel = 1;
   }
   else{
      singlelevel = 0;
   }

   vcount = max;

   /* TRACE TRAJECTORY BACKWARD (put vertices into array in backward order) */
   row = row0;
   col = col0;
   lev = lev0;
   time = ctx->Elapsed[time0];
   itime = time0;

   while (1) {
      float at, bt, u, v, w;

      /* test if current position is out of bounds */
      if (row<0.0 || row>maxr || col<0.0 || col>maxc || lev<0.0 || lev>maxl || lev<minl)
         break;

      /* record point */
      vcount--;
      vr[vcount] = row;
      vc[vcount] = col;
      vl[vcount] = lev;
      vt[vcount] = time;
      if (vcount==0)
         break;

      /* test if time is out of bounds */
      if (time<0)
         break;

      /* get u,v,w vector at current location and time */
      if (itime+1==ctx->NumTimes) {
         if (!get_uvw( ctx, itime, itime, 1.0, 0.0, row, col, lev,  &u, &v, &w, singlelevel ))
            break;
      }
      else {
         if (ctx->Elapsed[itime]==ctx->Elapsed[itime+1]) {
            /* this should never happen unless the file is screwy */
            break;
         }
         at = (float) (ctx->Elapsed[itime+1] - time)
            / (float) (ctx->Elapsed[itime+1]-ctx->Elapsed[itime]);
         bt = 1.0 - at;
         if (!get_uvw( ctx, itime, itime+1, at, bt, row, col, lev,  &u, &v, &w, singlelevel ))
            break;
      }


      /* update position and time */
      col -= u * (float) step;  /* pos' = pos + velocity * time */
      row -= v * (float) step;
      lev -= w * (float) step;
      time -= step;

      if (time<ctx->Elapsed[itime])
         itime--;
   }

   /* MOVE VERTICES FROM END TO BEGINNING OF ARRAY */
   i = 0;
   while (vcount<max) {
      vr[i] = vr[vcount];
      vc[i] = vc[vcount];
      vl[i] = vl[vcount];
      vt[i] = vt[vcount];
      vcount++;
      i++;
   }
   vcount = i;

   /* TRACE TRAJECTORY FORWARD */
   row = row0;
   col = col0;
   lev = lev0;
   time = ctx->Elapsed[time0];
   itime = time0;

   while (1) {
      float at, bt, u, v, w;

      /* test if position is out of bounds */
      if (row<0.0 || row>maxr || col<0.0 || col>maxc || lev<0.0 || lev>maxl || lev<minl)
         break;

      /* record point */
      vr[vcount] = row;
      vc[vcount] = col;
      vl[vcount] = lev;
      vt[vcount] = time;
      vcount++;
      if (vcount>=max)
         break;

      /* test if time is out of bounds */
      if (time>=ctx->Elapsed[ctx->NumTimes-1])
         break;

      /* get u,v,w vector at current location and time */
      if (ctx->Elapsed[itime]==ctx->Elapsed[itime+1]) {
         /* this should never happen unless the file is screwy */
         break;
      }
      at = (float) (ctx->Elapsed[itime+1] - time)
         / (float) (ctx->Elapsed[itime+1]-ctx->Elapsed[itime]);
      bt = 1.0 - at;
      if (!get_uvw( ctx, itime, itime+1, at, bt, row, col, lev,  &u, &v, &w, singlelevel ))
         break;

      /* update position and time */
      col += u * (float) step;  /* pos' = pos + velocity * time */
      row += v * (float) step;
      lev += w * (float) step;
      time += step;

      if (time>ctx->Elapsed[itime+1])
         itime++;
   }

   /* print the vertex list */

   /*for (i=0;i<vcount;i++) {
      printf("%3d: %6.3f %6.3f %6.3f %d\n", i, vr[i], vc[i], vl[i], vt[i] );
   }*/

   if (vcount > max){
      vcount = max;
   }
   return vcount;
}




#define MAX 5000
#define EPS 0.0000000001
#define WSCALE 150.0
#define RADIUS 10.0



/*
 * Convert a trajectory path to a ribbon.
 */
int to_ribbon( int num, float xt[], float yt[], float zt[],
               int itint[], float xn[], float yn[], float zn[] )
{
   int i, it, ip, il, ih;
   int iuu, imm, idd, iu, id;
   float ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz, ex, ey, ez;
   float cc, dd, ee;
   float scale;
   float xr[MAX], yr[MAX], zr[MAX];
   int itinr[MAX];

   scale = 2.0/WSCALE;

   for (i=0;i<num;i++) {
      xr[i] = xt[i];
      yr[i] = yt[i];
      zr[i] = zt[i];
      itinr[i] = itint[i];
   }

   /* DO 300 ITRAJ=ITRLOW,ITRHI */
   /* IL = LSTART(ITRAJ) */
   /* IH = LEND(ITRAJ) */
   /* CALCULATE UNFILTERED RIBBON ORIENTATIONS */
   il = 0;
   ih = num;
   for (i=il;i<ih;i++) {
      /* DO 10 I=IL,IH */
      it = 2*i - 0;
      ip = it+1;

      /* SHORT TRAJECTORY CASE */
      if (ih-il <= 2) {
         xt[it] = xr[i];
         yt[it] = yr[i];
         zt[it] = zr[i];
         xn[it] = 1.0;
         yn[it] = 0.0;
         zn[it] = 0.0;
         itint[it] = 0;
         xt[ip] = xr[i];
         yt[ip] = yr[i];
         zt[ip] = zr[i];
         xn[ip] = 1.0;
         yn[ip] = 0.0;
         zn[ip] = 0.0;
         itint[ip] = itinr[i];

         /* go to 10 */
      }
      else {
         /* LONG TRAJECTORY CASE */
         /* RAW CROSS RIBBON VECTORS */
         iuu = i+1;
         imm = i;
         idd = i-1;
         if (i == il) {
            iuu = i+2;
            imm = i+1;
            idd = i;
         }
         if (i == ih-1) {
            iuu = i;
            imm = i-1;
            idd = i-2;
         }
         ax = xr[iuu]-xr[imm];
         ay = yr[iuu]-yr[imm];
         az = zr[iuu]-zr[imm];
         bx = xr[imm]-xr[idd];
         by = yr[imm]-yr[idd];
         bz = zr[imm]-zr[idd];
         cx = ay*bz-az*by;
         cy = az*bx-ax*bz;
         cz = ax*by-ay*bx;
         cc = sqrt(cx*cx+cy*cy+cz*cz);
         if (cc < EPS)
           cc = EPS;
         cc = 1.0/cc;
         xn[ip] = cx*cc;
         yn[ip] = cy*cc;
         zn[ip] = cz*cc;
         /* ALONG RIBBON VECTOR */
         iu = i+1;
         id = i-1;
         if (i == il) id = i;
         if (i == ih-1) iu = i;
         dx = xr[iu]-xr[id];
         dy = yr[iu]-yr[id];
         dz = zr[iu]-zr[id];
         dd = sqrt(dx*dx+dy*dy+dz*dz);
         if (dd < EPS)
           dd = EPS;
         dd = 1.0/dd;
         xn[it] = dx*dd;
         yn[it] = dy*dd;
         zn[it] = dz*dd;
      }
   }
   /* 10    CONTINUE */

   if (ih-il < 2)   /* <= 2 ??? */
      return 0;

   /* PROPOGATE CROSS RIBBON VECTOR */
   cx = 0.0;
   cy = 0.0;
   cz = 0.0;

   /* DO 20 I=IL,IH */
   for (i=il;i<ih;i++) {
      it = 2*i-0;
      ip = it+1;
      /* ADD (OR SUBTRACT) RAW CROSS RIBBON FROM RUNNING SUM */
      ax=xn[ip];
      ay=yn[ip];
      az=zn[ip];
      if (ax*cx+ay*cy+az*cz > 0.0) {
         cx=cx+ax;
         cy=cy+ay;
         cz=cz+az;
      }
      else {
         cx=cx-ax;
         cy=cy-ay;
         cz=cz-az;
      }

      /* GET COMPONENT OF (CROSS RIBBON) NORMAL TO ALONG RIBBON */
      dx=xn[it];
      dy=yn[it];
      dz=zn[it];
      dd=cx*dx+cy*dy+cz*dz;
      cx=cx-dd*dx;
      cy=cy-dd*dy;
      cz=cz-dd*dz;
      /* SCALE CROSS RIBBON TO RADIUS LENGTH */
      cc = sqrt(cx*cx+cy*cy+cz*cz);
      if (cc < EPS)
        cc = EPS;
      cc = RADIUS/cc;
      cx=cx*cc;
      cy=cy*cc;
      cz=cz*cc;
      xt[it]=cx;
      yt[it]=cy;
      zt[it]=cz;
   }
   /* 20    CONTINUE; */

   /* NOW DO RIBBONS */
   /* DO 40 I=IL,IH */
   for (i=il;i<ih;i++) {
      it = 2*i - 0;
      ip = it+1;
      cx=xt[it];
      cy=yt[it];
      cz=zt[it];
      dx=xn[it];
      dy=yn[it];
      dz=zn[it];

      cc = scale/RADIUS;
      cx=cx*cc;
      cy=cy*cc;
      cz=cz*cc;
      /* CALCULATE NORMAL TO RIBBON */
      ex = cy*dz - cz*dy;
      ey = cz*dx - cx*dz;
      ez = cx*dy - cy*dx;
      ee = sqrt(ex*ex+ey*ey+ez*ez);
      if (ee < EPS) ee = EPS;
      ee = 1.0/ee;
      ex=ex*ee;
      ey=ey*ee;
      ez=ez*ee;
      /* MAKE RIBBON POINTS */
      xt[it]=xr[i] - cx;
      yt[it]=yr[i] - cy;
      zt[it]=zr[i] - cz;
      xn[it]=ex;
      yn[it]=ey;
      zn[it]=ez;
      itint[it]=0;
      xt[ip]=xr[i] + cx;
      yt[ip]=yr[i] + cy;
      zt[ip]=zr[i] + cz;
      xn[ip]=ex;
      yn[ip]=ey;
      zn[ip]=ez;
      itint[ip]=itinr[i];
   }
   /* 40    CONTINUE */

   return num*2;
}
