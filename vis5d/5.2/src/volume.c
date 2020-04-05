/* volume.c */

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
 * Volume rendering.  Requires alpha blending capability.  This is
 * supported in on SGI hardware with VGX or higher graphics.  Otherwise,
 * it's done in software on some other systems.
 *
 * Volume rendering is also supported on everthing running OpenGL!
 */


#include <math.h>
#include <stdlib.h>
#include "globals.h"
#include "graphics.h"
#include "grid.h"
#include "memory.h"
#include "proj.h"
#include "volume.h"
#ifdef OPENGL
#  include <GL/gl.h>
#endif
#if defined(SGI_GL) || defined(DENALI)
#  include <gl/gl.h>
#endif



#define ABS(A)  ( (A) < 0 ? -(A) : (A) )


/* Direction of slices: */
#define BOTTOM_TO_TOP  0
#define TOP_TO_BOTTOM  1
#define EAST_TO_WEST   2
#define WEST_TO_EAST   3
#define NORTH_TO_SOUTH 4
#define SOUTH_TO_NORTH 5

#define LUT_SIZE 255



struct volume {
   int     dir;         /* Direction of slices */
   int     valid;       /* Valid flag */
   int     slices;      /* Number of slices */
   int     rows, cols;  /* Size of each slice */
   float   *vertex;     /* slice vertices stored as: */
                        /*    vertex[slices][rows][cols][3] */
   uint_1  *index;      /* color table index in [0,255] */
   int oldnr, oldnc, oldnl; /* this is to know how much to dealloc */
};



/*
 * Allocate a new volume structure and return a pointer to it.  If we
 * can't do volume rendering return NULL.
 * Input:  nr, nc, nl - dimensions of largest volume we'll encounter.
 * Return:  pointer to volume struct or NULL
 */
struct volume *alloc_volume( Context ctx, int nr, int nc, int nl )
{
   long alphabits;
   int volren = 0;
   struct volume *v = NULL;

   if (ctx->dpy_ctx->Projection == PROJ_CYLINDRICAL ||
       ctx->dpy_ctx->Projection == PROJ_SPHERICAL){
      ctx->dpy_ctx->VolRender = 0;
      return 0;
   }
   if (nl <= 1){
      ctx->dpy_ctx->VolRender = 0;
      return 0; /* no volume variables */
   }

#if defined (SGI_GL) || defined (DENALI)
   alphabits = getgdesc( GD_BLEND ); 
   if (alphabits==0) {
      ctx->dpy_ctx->VolRender = 0; 
      /* no alpha support means no volume rendering */
      return NULL;
   }
   volren = 1;
#endif

#if defined(OPENGL)
   volren = 1;
#endif

/* MJK 12.15.98 */
#ifdef PEX
   volren = 1;
#endif

   
   if (volren) {
      v = (struct volume *) malloc( sizeof(struct volume) );
      /* initialize the volume struct */
      v->valid = 0;
      /* No matter which way we slice it, we need the same size arrays for */
      /* storing the vertices and colors. */
      v->vertex = (float *) allocate( ctx, nl*nr*nc*3*sizeof(float) );
      v->index = (uint_1 *) allocate( ctx, nl*nr*nc*sizeof(uint_1) );
      if (!v->vertex || !v->index) {
         printf("WARNING:  insufficient memory for volume rendering (%d bytes needed)\n",
                nl * nr * nc * (3*sizeof(float)+sizeof(uint_1)) );
         ctx->dpy_ctx->VolRender = 0; 
         return NULL;
      }
      v->oldnr = nr;
      v->oldnc = nc;
      v->oldnl = nl;
/* MJK 12.15.98 */
#ifdef PEX 
      v->dir = -1;
#endif

   }
   if (v){
      ctx->dpy_ctx->VolRender = 1;
   }
   else{
      ctx->dpy_ctx->VolRender = 0;
   }
   return v;
}

int free_volume( Context ctx)
{
   deallocate( ctx, ctx->Volume->vertex,
     ctx->Volume->oldnl*ctx->Volume->oldnr*ctx->Volume->oldnc*3*sizeof(float));
   deallocate( ctx, ctx->Volume->index,
    ctx->Volume->oldnl*ctx->Volume->oldnr*ctx->Volume->oldnc*sizeof(uint_1));
   free(ctx->Volume);
   ctx->Volume = NULL;
}



/*
 * Compute a volume rendering of the given grid.
 * Input:  data - 3-D data grid.
 *         time, var - time step and variable
 *         nr, nc, nl - size of 3-D grid.
 *         min, max - min and max data value in data grid.
 *         dir - direction to do slicing
 *         v - pointer to a volume struct with the vertex and index
 *             fields pointing to sufficiently large buffers.
 * Output:  v - volume struct describing the computed volume.
 */
static int compute_volume( Context ctx, float data[],
                           int time, int var,
                           int nr, int nc, int nl, int lowlev,
                           float min, float max,
                           int dir,
                           struct volume *v )
{
   Display_Context dtx;
   float zs[MAXLEVELS];
   register int ir, ic, il, i, j;
   register float x, y, dx, dy;
   register float dscale, val;
   register int ilnrnc, icnr;           /* to help speed up array indexing */

   dtx = ctx->dpy_ctx;
   /* compute graphics Z for each grid level */
   for (il=0; il<nl; il++) {
     zs[il] = gridlevel_to_z(ctx, time, var, (float) (il + lowlev));
   }

   /* compute some useful values */
   dx = (dtx->Xmax-dtx->Xmin) / (nc-1);
   dy = (dtx->Ymax-dtx->Ymin) / (nr-1);
   dscale = (float) (LUT_SIZE-1) / (max-min);

   v->dir = dir;

   switch (dir) {
      case BOTTOM_TO_TOP:
         /* compute slices from bottom to top */
         v->slices = nl;
         v->rows = nr;
         v->cols = nc;

         i = 0;  /* index into vertex array */
         j = 0;  /* index into index array */
         for (il=0; il<nl; il++) {
            y = dtx->Ymax;
            ilnrnc = il * nr * nc;
            for (ir=0;ir<nr;ir++) {
               x = dtx->Xmin;
               for (ic=0;ic<nc;ic++) {
                  /* compute vertex */
                  v->vertex[i++] = x;
                  v->vertex[i++] = y;
                  v->vertex[i++] = zs[il];

                  /* compute color table index */
                  val = data[ ilnrnc + ic * nr + ir ];
                  if (IS_MISSING(val) ||
                      val < min || val > max)
                    v->index[j++] = 255;
                  else
                    v->index[j++] = (uint_1) (int) ((val-min) * dscale);

                  x += dx;
               }
               y -= dy;
            }
         }
         break;

      case TOP_TO_BOTTOM:
         /* compute slices from top to bottom */
         v->slices = nl;
         v->rows = nr;
         v->cols = nc;

         i = 0;  /* index into vertex array */
         j = 0;  /* index into index array */
         for (il=nl-1; il>=0; il--) {
            y = dtx->Ymax;
            ilnrnc = il * nr * nc;
            for (ir=0;ir<nr;ir++) {
               x = dtx->Xmin;
               for (ic=0;ic<nc;ic++) {
                  /* compute vertex */
                  v->vertex[i++] = x;
                  v->vertex[i++] = y;
                  v->vertex[i++] = zs[il];

                  /* compute color table index */
                  val = data[ ilnrnc + ic * nr + ir ];
                  if (IS_MISSING(val) ||
                      val < min || val > max)
                    v->index[j++] = 255;
                  else
                    v->index[j++] = (uint_1) (int) ((val-min) * dscale);

                  x += dx;
               }
               y -= dy;
            }
         }
         break;

      case WEST_TO_EAST:
         /* compute slices from west to east */
         v->slices = nc;
         v->rows = nl;
         v->cols = nr;

         i = 0;  /* index into vertex array */
         j = 0;  /* index into index array */
         x = dtx->Xmin;
         for (ic=0; ic<nc; ic++) {
            icnr = ic * nr;
            for (il=nl-1;il>=0;il--) {
               y = dtx->Ymin;
               ilnrnc = il * nr * nc + icnr;
               for (ir=nr-1;ir>=0;ir--) {
                  /* compute vertex */
                  v->vertex[i++] = x;
                  v->vertex[i++] = y;
                  v->vertex[i++] = zs[il];

                  /* compute color table index */
                  val = data[ ilnrnc + ir ];
                  if (IS_MISSING(val) ||
                      val < min || val > max)
                    v->index[j++] = 255;
                  else
                    v->index[j++] = (uint_1) (int) ((val-min) * dscale);

                  y += dy;
               }
            }
            x += dx;
         }
         break;

      case EAST_TO_WEST:
         /* compute slices from east to west */
         v->slices = nc;
         v->rows = nl;
         v->cols = nr;

         i = 0;  /* index into vertex array */
         j = 0;  /* index into index array */
         x = dtx->Xmax;
         for (ic=nc-1; ic>=0; ic--) {
            icnr = ic*nr;
            for (il=nl-1;il>=0;il--) {
               y = dtx->Ymin;
               ilnrnc = il * nr * nc + icnr;
               for (ir=nr-1;ir>=0;ir--) {
                  /* compute vertex */
                  v->vertex[i++] = x;
                  v->vertex[i++] = y;
                  v->vertex[i++] = zs[il];

                  /* compute color table index */
                  val = data[ ilnrnc + ir ];
                  if (IS_MISSING(val) ||
                      val < min || val > max)
                    v->index[j++] = 255;
                  else
                    v->index[j++] = (uint_1) (int) ((val-min) * dscale);

                  y += dy;
               }
            }
            x -= dx;
         }
         break;

      case NORTH_TO_SOUTH:
         /* compute slices from north to south */
         v->slices = nr;
         v->rows = nl;
         v->cols = nc;

         i = 0;  /* index into vertex array */
         j = 0;  /* index into index array */
         y = dtx->Ymax;
         for (ir=0; ir<nr; ir++) {
            for (il=nl-1;il>=0;il--) {
               x = dtx->Xmin;
               ilnrnc = il * nr * nc;
               for (ic=0;ic<nc;ic++) {
                  /* compute vertex */
                  v->vertex[i++] = x;
                  v->vertex[i++] = y;
                  v->vertex[i++] = zs[il];

                  /* compute color table index */
                  val = data[ ilnrnc + ic*nr + ir ];
                  if (IS_MISSING(val) ||
                      val < min || val > max)
                    v->index[j++] = 255;
                  else
                    v->index[j++] = (uint_1) (int) ((val-min) * dscale);

                  x += dx;
               }
            }
            y -= dy;
         }
         break;

      case SOUTH_TO_NORTH:
         /* compute slices from south to north */
         v->slices = nr;
         v->rows = nl;
         v->cols = nc;

         i = 0;  /* index into vertex array */
         j = 0;  /* index into index array */
         y = dtx->Ymin;
         for (ir=nr-1; ir>=0; ir--) {
            for (il=nl-1;il>=0;il--) {
               x = dtx->Xmin;
               ilnrnc = il * nr * nc;
               for (ic=0;ic<nc;ic++) {
                  /* compute vertex */
                  v->vertex[i++] = x;
                  v->vertex[i++] = y;
                  v->vertex[i++] = zs[il];

                  /* compute color table index */
                  val = data[ ilnrnc + ic*nr + ir ];
                  if (IS_MISSING(val) ||
                      val < min || val > max)
                    v->index[j++] = 255;
                  else
                    v->index[j++] = (uint_1) (int) ((val-min) * dscale);

                  x += dx;
               }
            }
            y += dy;
         }
         break;

      default:
         printf("Error in compute_volume!\n");

   } /* switch */

   v->valid = 1;
   return 1;
}

static int compute_volumePRIME( Context ctx, float data[],
                           int time, int var,
                           int nr, int nc, int nl, int lowlev,
                           float min, float max,
                           int dir,
                           struct volume *v )
{
   Display_Context dtx;
   float zs[MAXLEVELS];
   register int ir, ic, il, i, j;
   register float x, y, dx, dy;
   register float dscale, val;
   register int ilnrnc, icnr;           /* to help speed up array indexing */
   float s1,s2,s3,s4,s5,s6,s7,s8;
   float grow, gcol, glev;
   float row, col, lev;
   int gr0,gr1,gc0,gc1,gl0,gl1;
   float ger, gec, gel;

   dtx = ctx->dpy_ctx;
   /* compute graphics Z for each grid level */
   for (il=0; il<nl; il++) {
     zs[il] = gridlevelPRIME_to_zPRIME(dtx, time, var, (float) (il + lowlev));
   }

   /* compute some useful values */
   dx = (dtx->Xmax-dtx->Xmin) / (nc-1);
   dy = (dtx->Ymax-dtx->Ymin) / (nr-1);
   dscale = (float) (LUT_SIZE-1) / (max-min);

   v->dir = dir;

   switch (dir) {
      case BOTTOM_TO_TOP:
         /* compute slices from bottom to top */
         v->slices = nl;
         v->rows = nr;
         v->cols = nc;

         i = 0;  /* index into vertex array */
         j = 0;  /* index into index array */
         for (il=0; il<nl; il++) {
            y = dtx->Ymax;
            ilnrnc = il * nr * nc;
            for (ir=0;ir<nr;ir++) {
               x = dtx->Xmin;
               for (ic=0;ic<nc;ic++) {
                  /* compute vertex */
                  v->vertex[i++] = x;
                  v->vertex[i++] = y;
                  v->vertex[i++] = zs[il];

                  /* compute color table index */
                  row = ir;
                  col = ic;
                  lev = il;
                  gridPRIME_to_grid( ctx, time, var, 1, &row, &col, &lev,
                                      &grow, &gcol, &glev);
                  if ( grow < 0 || gcol < 0 || glev < 0 ||
                    grow >= ctx->Nr || gcol >= ctx->Nc || glev >= ctx->Nl[var]){
                     v->index[j++] = 255;
                  }
                  else{
                     gr0= (int) grow;
                     gr1= gr0+1;
                     if (gr0 == ctx->Nr-1){
                        gr1 = gr0;
                     }
                     gc0 = (int) gcol;
                     gc1 = gc0 + 1;
                     if (gc0 == ctx->Nc-1){
                        gc1 = gc0;
                     }
                     gl0 = (int) glev;
                     gl1 = gl0+1;
                     if (gl0 == ctx->Nl[var]-1){
                        gl1 = gl0;
                     }

                     ger = grow - (float) gr0; /* in [0,1) */
                     gec = gcol - (float) gc0; /* in [0,1) */
                     gel = glev - (float) gl0; /* in [0,1) */

                     if (ger==0.0){
                        gr1 = gr0;
                     }
                     if (gec==0.0){
                        gc1 = gc0;
                     }
                     if (gel==0.0){
                        gl1 = gl0;
                     }

                     s1 = data[(gl0*ctx->Nc+gc0)*ctx->Nr+gr0];
                     s2 = data[(gl0*ctx->Nc+gc0)*ctx->Nr+gr1];
                     s3 = data[(gl0*ctx->Nc+gc1)*ctx->Nr+gr0];
                     s4 = data[(gl0*ctx->Nc+gc1)*ctx->Nr+gr1];
                     s5 = data[(gl1*ctx->Nc+gc0)*ctx->Nr+gr0];
                     s6 = data[(gl1*ctx->Nc+gc0)*ctx->Nr+gr1];
                     s7 = data[(gl1*ctx->Nc+gc1)*ctx->Nr+gr0];
                     s8 = data[(gl1*ctx->Nc+gc1)*ctx->Nr+gr1];

                     if (IS_MISSING(s1) || IS_MISSING(s2) ||
                         IS_MISSING(s3) || IS_MISSING(s4) ||
                         IS_MISSING(s5) || IS_MISSING(s6) ||
                         IS_MISSING(s7) || IS_MISSING(s8)){
                        v->index[j++] = 255;
                     }
                     else{
                        val = ( s1 * (1.0-ger) * (1.0-gec)
                             + s2 * ger       * (1.0-gec)
                             + s3 * (1.0-ger) * gec
                             + s4 * ger       * gec        ) * (1.0-gel)
                          +  ( s5 * (1.0-ger) * (1.0-gec)
                             + s6 * ger       * (1.0-gec)
                             + s7 * (1.0-ger) * gec
                             + s8 * ger       * gec        ) * gel;
                        if (val < min || val > max){
                           v->index[j++] = 255;
                        }
                        else{
                           v->index[j++] = (uint_1) (int) ((val-min) * dscale);
                        }
                     }
                  }
                  x += dx;
               }
               y -= dy;
            }
         }
         break;

      case TOP_TO_BOTTOM:
         /* compute slices from top to bottom */
         v->slices = nl;
         v->rows = nr;
         v->cols = nc;

         i = 0;  /* index into vertex array */
         j = 0;  /* index into index array */
         for (il=nl-1; il>=0; il--) {
            y = dtx->Ymax;
            ilnrnc = il * nr * nc;
            for (ir=0;ir<nr;ir++) {
               x = dtx->Xmin;
               for (ic=0;ic<nc;ic++) {
                  /* compute vertex */
                  v->vertex[i++] = x;
                  v->vertex[i++] = y;
                  v->vertex[i++] = zs[il];

                  /* compute color table index */
                  row = ir;
                  col = ic;
                  lev = il;
                  gridPRIME_to_grid( ctx, time, var, 1, &row, &col, &lev,
                                      &grow, &gcol, &glev);
                  if ( grow < 0 || gcol < 0 || glev < 0 ||
                    grow >= ctx->Nr || gcol >= ctx->Nc || glev >= ctx->Nl[var]){
                     v->index[j++] = 255;
                  }
                  else{
                     gr0= (int) grow;
                     gr1= gr0+1;
                     if (gr0 == ctx->Nr-1){
                        gr1 = gr0;
                     }
                     gc0 = (int) gcol;
                     gc1 = gc0 + 1;
                     if (gc0 == ctx->Nc-1){
                        gc1 = gc0;
                     }
                     gl0 = (int) glev;
                     gl1 = gl0+1;
                     if (gl0 == ctx->Nl[var]-1){
                        gl1 = gl0;
                     }

                     ger = grow - (float) gr0; /* in [0,1) */
                     gec = gcol - (float) gc0; /* in [0,1) */
                     gel = glev - (float) gl0; /* in [0,1) */

                     if (ger==0.0){
                        gr1 = gr0;
                     }
                     if (gec==0.0){
                        gc1 = gc0;
                     }
                     if (gel==0.0){
                        gl1 = gl0;
                     }

                     s1 = data[(gl0*ctx->Nc+gc0)*ctx->Nr+gr0];
                     s2 = data[(gl0*ctx->Nc+gc0)*ctx->Nr+gr1];
                     s3 = data[(gl0*ctx->Nc+gc1)*ctx->Nr+gr0];
                     s4 = data[(gl0*ctx->Nc+gc1)*ctx->Nr+gr1];
                     s5 = data[(gl1*ctx->Nc+gc0)*ctx->Nr+gr0];
                     s6 = data[(gl1*ctx->Nc+gc0)*ctx->Nr+gr1];
                     s7 = data[(gl1*ctx->Nc+gc1)*ctx->Nr+gr0];
                     s8 = data[(gl1*ctx->Nc+gc1)*ctx->Nr+gr1];

                     if (IS_MISSING(s1) || IS_MISSING(s2) ||
                         IS_MISSING(s3) || IS_MISSING(s4) ||
                         IS_MISSING(s5) || IS_MISSING(s6) ||
                         IS_MISSING(s7) || IS_MISSING(s8)){
                        v->index[j++] = 255;
                     }
                     else{
                        val = ( s1 * (1.0-ger) * (1.0-gec)
                             + s2 * ger       * (1.0-gec)
                             + s3 * (1.0-ger) * gec
                             + s4 * ger       * gec        ) * (1.0-gel)
                          +  ( s5 * (1.0-ger) * (1.0-gec)
                             + s6 * ger       * (1.0-gec)
                             + s7 * (1.0-ger) * gec
                             + s8 * ger       * gec        ) * gel;
                        if (val < min || val > max){
                           v->index[j++] = 255;
                        }
                        else{
                           v->index[j++] = (uint_1) (int) ((val-min) * dscale);
                        }
                     }
                  }
                  x += dx;
               }
               y -= dy;
            }
         }
         break;

      case WEST_TO_EAST:
         /* compute slices from west to east */
         v->slices = nc;
         v->rows = nl;
         v->cols = nr;

         i = 0;  /* index into vertex array */
         j = 0;  /* index into index array */
         x = dtx->Xmin;
         for (ic=0; ic<nc; ic++) {
            icnr = ic * nr;
            for (il=nl-1;il>=0;il--) {
               y = dtx->Ymin;
               ilnrnc = il * nr * nc + icnr;
               for (ir=nr-1;ir>=0;ir--) {
                  /* compute vertex */
                  v->vertex[i++] = x;
                  v->vertex[i++] = y;
                  v->vertex[i++] = zs[il];

                  /* compute color table index */

                  row = ir;
                  col = ic;
                  lev = il;
                  gridPRIME_to_grid( ctx, time, var, 1, &row, &col, &lev,
                                      &grow, &gcol, &glev);
                  if ( grow < 0 || gcol < 0 || glev < 0 ||
                    grow >= ctx->Nr || gcol >= ctx->Nc || glev >= ctx->Nl[var]){
                     v->index[j++] = 255;
                  }
                  else{
                     gr0= (int) grow;
                     gr1= gr0+1;
                     if (gr0 == ctx->Nr-1){
                        gr1 = gr0;
                     }
                     gc0 = (int) gcol;
                     gc1 = gc0 + 1;
                     if (gc0 == ctx->Nc-1){
                        gc1 = gc0;
                     }
                     gl0 = (int) glev;
                     gl1 = gl0+1;
                     if (gl0 == ctx->Nl[var]-1){
                        gl1 = gl0;
                     }

                     ger = grow - (float) gr0; /* in [0,1) */
                     gec = gcol - (float) gc0; /* in [0,1) */
                     gel = glev - (float) gl0; /* in [0,1) */

                     if (ger==0.0){
                        gr1 = gr0;
                     }
                     if (gec==0.0){
                        gc1 = gc0;
                     }
                     if (gel==0.0){
                        gl1 = gl0;
                     }

                     s1 = data[(gl0*ctx->Nc+gc0)*ctx->Nr+gr0];
                     s2 = data[(gl0*ctx->Nc+gc0)*ctx->Nr+gr1];
                     s3 = data[(gl0*ctx->Nc+gc1)*ctx->Nr+gr0];
                     s4 = data[(gl0*ctx->Nc+gc1)*ctx->Nr+gr1];
                     s5 = data[(gl1*ctx->Nc+gc0)*ctx->Nr+gr0];
                     s6 = data[(gl1*ctx->Nc+gc0)*ctx->Nr+gr1];
                     s7 = data[(gl1*ctx->Nc+gc1)*ctx->Nr+gr0];
                     s8 = data[(gl1*ctx->Nc+gc1)*ctx->Nr+gr1];

                     if (IS_MISSING(s1) || IS_MISSING(s2) ||
                         IS_MISSING(s3) || IS_MISSING(s4) ||
                         IS_MISSING(s5) || IS_MISSING(s6) ||
                         IS_MISSING(s7) || IS_MISSING(s8)){
                        v->index[j++] = 255;
                     }
                     else{
                        val = ( s1 * (1.0-ger) * (1.0-gec)
                             + s2 * ger       * (1.0-gec)
                             + s3 * (1.0-ger) * gec
                             + s4 * ger       * gec        ) * (1.0-gel)
                          +  ( s5 * (1.0-ger) * (1.0-gec)
                             + s6 * ger       * (1.0-gec)
                             + s7 * (1.0-ger) * gec
                             + s8 * ger       * gec        ) * gel;
                        if (val < min || val > max){
                           v->index[j++] = 255;
                        }
                        else{
                           v->index[j++] = (uint_1) (int) ((val-min) * dscale);
                        }
                     }
                  }
                  y += dy;
               }
            }
            x += dx;
         }
         break;

      case EAST_TO_WEST:
         /* compute slices from east to west */
         v->slices = nc;
         v->rows = nl;
         v->cols = nr;

         i = 0;  /* index into vertex array */
         j = 0;  /* index into index array */
         x = dtx->Xmax;
         for (ic=nc-1; ic>=0; ic--) {
            icnr = ic*nr;
            for (il=nl-1;il>=0;il--) {
               y = dtx->Ymin;
               ilnrnc = il * nr * nc + icnr;
               for (ir=nr-1;ir>=0;ir--) {
                  /* compute vertex */
                  v->vertex[i++] = x;
                  v->vertex[i++] = y;
                  v->vertex[i++] = zs[il];

                  /* compute color table index */
                  row = ir;
                  col = ic;
                  lev = il;
                  gridPRIME_to_grid( ctx, time, var, 1, &row, &col, &lev,
                                      &grow, &gcol, &glev);
                  if ( grow < 0 || gcol < 0 || glev < 0 ||
                    grow >= ctx->Nr || gcol >= ctx->Nc || glev >= ctx->Nl[var]){
                     v->index[j++] = 255;
                  }
                  else{
                     gr0= (int) grow;
                     gr1= gr0+1;
                     if (gr0 == ctx->Nr-1){
                        gr1 = gr0;
                     }
                     gc0 = (int) gcol;
                     gc1 = gc0 + 1;
                     if (gc0 == ctx->Nc-1){
                        gc1 = gc0;
                     }
                     gl0 = (int) glev;
                     gl1 = gl0+1;
                     if (gl0 == ctx->Nl[var]-1){
                        gl1 = gl0;
                     }

                     ger = grow - (float) gr0; /* in [0,1) */
                     gec = gcol - (float) gc0; /* in [0,1) */
                     gel = glev - (float) gl0; /* in [0,1) */

                     if (ger==0.0){
                        gr1 = gr0;
                     }
                     if (gec==0.0){
                        gc1 = gc0;
                     }
                     if (gel==0.0){
                        gl1 = gl0;
                     }

                     s1 = data[(gl0*ctx->Nc+gc0)*ctx->Nr+gr0];
                     s2 = data[(gl0*ctx->Nc+gc0)*ctx->Nr+gr1];
                     s3 = data[(gl0*ctx->Nc+gc1)*ctx->Nr+gr0];
                     s4 = data[(gl0*ctx->Nc+gc1)*ctx->Nr+gr1];
                     s5 = data[(gl1*ctx->Nc+gc0)*ctx->Nr+gr0];
                     s6 = data[(gl1*ctx->Nc+gc0)*ctx->Nr+gr1];
                     s7 = data[(gl1*ctx->Nc+gc1)*ctx->Nr+gr0];
                     s8 = data[(gl1*ctx->Nc+gc1)*ctx->Nr+gr1];

                     if (IS_MISSING(s1) || IS_MISSING(s2) ||
                         IS_MISSING(s3) || IS_MISSING(s4) ||
                         IS_MISSING(s5) || IS_MISSING(s6) ||
                         IS_MISSING(s7) || IS_MISSING(s8)){
                        v->index[j++] = 255;
                     }
                     else{
                        val = ( s1 * (1.0-ger) * (1.0-gec)
                             + s2 * ger       * (1.0-gec)
                             + s3 * (1.0-ger) * gec
                             + s4 * ger       * gec        ) * (1.0-gel)
                          +  ( s5 * (1.0-ger) * (1.0-gec)
                             + s6 * ger       * (1.0-gec)
                             + s7 * (1.0-ger) * gec
                             + s8 * ger       * gec        ) * gel;
                        if (val < min || val > max){
                           v->index[j++] = 255;
                        }
                        else{
                           v->index[j++] = (uint_1) (int) ((val-min) * dscale);
                        }
                     }
                  }
                  y += dy;
               }
            }
            x -= dx;
         }
         break;

      case NORTH_TO_SOUTH:
         /* compute slices from north to south */
         v->slices = nr;
         v->rows = nl;
         v->cols = nc;

         i = 0;  /* index into vertex array */
         j = 0;  /* index into index array */
         y = dtx->Ymax;
         for (ir=0; ir<nr; ir++) {
            for (il=nl-1;il>=0;il--) {
               x = dtx->Xmin;
               ilnrnc = il * nr * nc;
               for (ic=0;ic<nc;ic++) {
                  /* compute vertex */
                  v->vertex[i++] = x;
                  v->vertex[i++] = y;
                  v->vertex[i++] = zs[il];

                  /* compute color table index */
                  row = ir;
                  col = ic;
                  lev = il;
                  gridPRIME_to_grid( ctx, time, var, 1, &row, &col, &lev,
                                      &grow, &gcol, &glev);
                  if ( grow < 0 || gcol < 0 || glev < 0 ||
                    grow >= ctx->Nr || gcol >= ctx->Nc || glev >= ctx->Nl[var]){
                     v->index[j++] = 255;
                  }
                  else{
                     gr0= (int) grow;
                     gr1= gr0+1;
                     if (gr0 == ctx->Nr-1){
                        gr1 = gr0;
                     }
                     gc0 = (int) gcol;
                     gc1 = gc0 + 1;
                     if (gc0 == ctx->Nc-1){
                        gc1 = gc0;
                     }
                     gl0 = (int) glev;
                     gl1 = gl0+1;
                     if (gl0 == ctx->Nl[var]-1){
                        gl1 = gl0;
                     }

                     ger = grow - (float) gr0; /* in [0,1) */
                     gec = gcol - (float) gc0; /* in [0,1) */
                     gel = glev - (float) gl0; /* in [0,1) */

                     if (ger==0.0){
                        gr1 = gr0;
                     }
                     if (gec==0.0){
                        gc1 = gc0;
                     }
                     if (gel==0.0){
                        gl1 = gl0;
                     }

                     s1 = data[(gl0*ctx->Nc+gc0)*ctx->Nr+gr0];
                     s2 = data[(gl0*ctx->Nc+gc0)*ctx->Nr+gr1];
                     s3 = data[(gl0*ctx->Nc+gc1)*ctx->Nr+gr0];
                     s4 = data[(gl0*ctx->Nc+gc1)*ctx->Nr+gr1];
                     s5 = data[(gl1*ctx->Nc+gc0)*ctx->Nr+gr0];
                     s6 = data[(gl1*ctx->Nc+gc0)*ctx->Nr+gr1];
                     s7 = data[(gl1*ctx->Nc+gc1)*ctx->Nr+gr0];
                     s8 = data[(gl1*ctx->Nc+gc1)*ctx->Nr+gr1];

                     if (IS_MISSING(s1) || IS_MISSING(s2) ||
                         IS_MISSING(s3) || IS_MISSING(s4) ||
                         IS_MISSING(s5) || IS_MISSING(s6) ||
                         IS_MISSING(s7) || IS_MISSING(s8)){
                        v->index[j++] = 255;
                     }
                     else{
                        val = ( s1 * (1.0-ger) * (1.0-gec)
                             + s2 * ger       * (1.0-gec)
                             + s3 * (1.0-ger) * gec
                             + s4 * ger       * gec        ) * (1.0-gel)
                          +  ( s5 * (1.0-ger) * (1.0-gec)
                             + s6 * ger       * (1.0-gec)
                             + s7 * (1.0-ger) * gec
                             + s8 * ger       * gec        ) * gel;
                        if (val < min || val > max){
                           v->index[j++] = 255;
                        }
                        else{
                           v->index[j++] = (uint_1) (int) ((val-min) * dscale);
                        }
                     }
                  }
                  x += dx;
               }
            }
            y -= dy;
         }
         break;

      case SOUTH_TO_NORTH:
         /* compute slices from south to north */
         v->slices = nr;
         v->rows = nl;
         v->cols = nc;

         i = 0;  /* index into vertex array */
         j = 0;  /* index into index array */
         y = dtx->Ymin;
         for (ir=nr-1; ir>=0; ir--) {
            for (il=nl-1;il>=0;il--) {
               x = dtx->Xmin;
               ilnrnc = il * nr * nc;
               for (ic=0;ic<nc;ic++) {
                  /* compute vertex */
                  v->vertex[i++] = x;
                  v->vertex[i++] = y;
                  v->vertex[i++] = zs[il];

                  /* compute color table index */
                  row = ir;
                  col = ic;
                  lev = il;
                  gridPRIME_to_grid( ctx, time, var, 1, &row, &col, &lev,
                                      &grow, &gcol, &glev);
                  if ( grow < 0 || gcol < 0 || glev < 0 ||
                    grow >= ctx->Nr || gcol >= ctx->Nc || glev >= ctx->Nl[var]){
                     v->index[j++] = 255;
                  }
                  else{
                     gr0= (int) grow;
                     gr1= gr0+1;
                     if (gr0 == ctx->Nr-1){
                        gr1 = gr0;
                     }
                     gc0 = (int) gcol;
                     gc1 = gc0 + 1;
                     if (gc0 == ctx->Nc-1){
                        gc1 = gc0;
                     }
                     gl0 = (int) glev;
                     gl1 = gl0+1;
                     if (gl0 == ctx->Nl[var]-1){
                        gl1 = gl0;
                     }

                     ger = grow - (float) gr0; /* in [0,1) */
                     gec = gcol - (float) gc0; /* in [0,1) */
                     gel = glev - (float) gl0; /* in [0,1) */

                     if (ger==0.0){
                        gr1 = gr0;
                     }
                     if (gec==0.0){
                        gc1 = gc0;
                     }
                     if (gel==0.0){
                        gl1 = gl0;
                     }

                     s1 = data[(gl0*ctx->Nc+gc0)*ctx->Nr+gr0];
                     s2 = data[(gl0*ctx->Nc+gc0)*ctx->Nr+gr1];
                     s3 = data[(gl0*ctx->Nc+gc1)*ctx->Nr+gr0];
                     s4 = data[(gl0*ctx->Nc+gc1)*ctx->Nr+gr1];
                     s5 = data[(gl1*ctx->Nc+gc0)*ctx->Nr+gr0];
                     s6 = data[(gl1*ctx->Nc+gc0)*ctx->Nr+gr1];
                     s7 = data[(gl1*ctx->Nc+gc1)*ctx->Nr+gr0];
                     s8 = data[(gl1*ctx->Nc+gc1)*ctx->Nr+gr1];

                     if (IS_MISSING(s1) || IS_MISSING(s2) ||
                         IS_MISSING(s3) || IS_MISSING(s4) ||
                         IS_MISSING(s5) || IS_MISSING(s6) ||
                         IS_MISSING(s7) || IS_MISSING(s8)){
                        v->index[j++] = 255;
                     }
                     else{
                        val = ( s1 * (1.0-ger) * (1.0-gec)
                             + s2 * ger       * (1.0-gec)
                             + s3 * (1.0-ger) * gec
                             + s4 * ger       * gec        ) * (1.0-gel)
                          +  ( s5 * (1.0-ger) * (1.0-gec)
                             + s6 * ger       * (1.0-gec)
                             + s7 * (1.0-ger) * gec
                             + s8 * ger       * gec        ) * gel;
                        if (val < min || val > max){
                           v->index[j++] = 255;
                        }
                        else{
                           v->index[j++] = (uint_1) (int) ((val-min) * dscale);
                        }
                     }
                  }
                  x += dx;
               }
            }
            y += dy;
         }
         break;

      default:
         printf("Error in compute_volumePRIME!\n");

   } /* switch */

   v->valid = 1;
   return 1;
}



/*
 * Render the volume described by the given volume struct.
 * Return:  1 = ok
 *          0 = bad volume struct.
 */
static int render_volume( Context ctx,
                          struct volume *v, unsigned int ctable[] )
{
   register int rows, cols, slices, i, j, s;
   register uint_1 *cp0, *cp1;
   register float *vp0, *vp1;

   if (!v || !v->slices)
      return 0;

#if defined (SGI_GL) || defined (DENALI)
   lmcolor( LMC_COLOR );             /* no shading */
   blendfunction( BF_SA, BF_MSA );   /* enable alpha blending */
#endif
#ifdef OPENGL
   glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
   glEnable( GL_BLEND );
#endif

   /* put rows, cols, slices values into registers */
   rows = v->rows-1;  /* number of quad strips = number of data rows - 1 */
   cols = v->cols;
   slices = v->slices;
   /* setup color and vertex pointers */
   cp0 = v->index;
   cp1 = cp0 + cols;
   vp0 = v->vertex;
   vp1 = vp0 + cols * 3;   /* 3 floats per vertex */

/* MJK 12.15.98 */
#ifdef PEX

   rows++;
   j = rows * cols;

   for (s = 0; s < slices; s++)
   {
      draw_volume_quadmesh (rows, cols, vp0, cp0, ctable);
      vp0 += j * 3;
      cp0 += j;
   }
   return 1;
#endif


   /* loop over slices */
   for (s=0;s<slices;s++) {

      /* draw 'rows' quadrilateral strips */
      for (i=0;i<rows;i++) {
#if defined(SGI_GL) || defined(DENALI)
         bgnqstrip();
         for (j=0;j<cols;j++) {
            cpack( ctable[*cp0++] );
            v3f( vp0 );
            vp0 += 3;
            cpack( ctable[*cp1++] );
            v3f( vp1 );
            vp1 += 3;
         }
         endqstrip();
#endif
#ifdef OPENGL
         glBegin( GL_QUAD_STRIP );
         for (j=0;j<cols;j++) {
            glColor4ubv( (GLubyte *) &ctable[*cp0++] );
            glVertex3fv( vp0 );
            vp0 += 3;
            glColor4ubv( (GLubyte *) &ctable[*cp1++] );
            glVertex3fv( vp1 );
            vp1 += 3;
         }
         glEnd();
#endif
      }

      /* skip a row after each slice */
      cp0 += cols;
      vp0 += cols*3;
      cp1 += cols;
      vp1 += cols*3;
   }

#if defined(SGI_GL) || defined(DENALI)
   blendfunction( BF_ONE, BF_ZERO );  /* disable alpha blending */
#endif
#ifdef OPENGL
   glDisable( GL_BLEND );
#endif
   return 1;
}


/* MJK 12.15.98 */
#ifdef PEX
void draw_volume( Context ctx, int it, int ip, unsigned int *ctable )
{
   float *data;
   static int prev_it[VIS5D_MAX_CONTEXTS];
   static int prev_ip[VIS5D_MAX_CONTEXTS];
   static int do_once = 1;
   int dir;
   float x, y, z, ax, ay, az;
   float xyz[3], xy[3][2], xy0[2];
   Display_Context dtx;

   dtx = ctx->dpy_ctx;
   if (do_once){
      int yo;
      for (yo=0; yo<VIS5D_MAX_CONTEXTS; yo++){
         prev_it[yo] = -1;
         prev_ip[yo] = -1;
      }
      do_once = 0;
   }


   xyz[0] = xyz[1] = xyz[2] = 0.0;
   project (xyz, &xy0[0], &xy0[1]);

   xyz[0] = 1.0, xyz[1] = xyz[2] = 0.0;
   project (xyz, &xy[0][0], &xy[0][1]);
   xy[0][0] -= xy0[0], xy[0][1] -= xy0[1];

   xyz[1] = 1.0, xyz[0] = xyz[2] = 0.0;
   project (xyz, &xy[1][0], &xy[1][1]);
   xy[1][0] -= xy0[0], xy[1][1] -= xy0[1];

   xyz[2] = 1.0, xyz[0] = xyz[1] = 0.0;
   project (xyz, &xy[2][0], &xy[2][1]);
   xy[2][0] -= xy0[0], xy[2][1] -= xy0[1];

   ax = (xy[0][0] * xy[0][0]) + (xy[0][1] * xy[0][1]);
   ay = (xy[1][0] * xy[1][0]) + (xy[1][1] * xy[1][1]);
   az = (xy[2][0] * xy[2][0]) + (xy[2][1] * xy[2][1]);

   if ((ax <= ay) && (ax <= az))
   {
      x = (xy[1][1] * xy[2][0]) - (xy[1][0] * xy[2][1]);
      dir = (x > 0.0) ? WEST_TO_EAST : EAST_TO_WEST;
   }
   else if ((ay <= ax) && (ay <= az))
   {
      x = (xy[2][1] * xy[0][0]) - (xy[2][0] * xy[0][1]);
      dir = (x > 0.0) ? SOUTH_TO_NORTH : NORTH_TO_SOUTH;
   }
   else
   {
      x = (xy[0][1] * xy[1][0]) - (xy[0][0] * xy[1][1]);
      dir = (x > 0.0) ? BOTTOM_TO_TOP : TOP_TO_BOTTOM;
   }

   /* If this is a new time step or variable then invalidate old volumes */
   if (it!=prev_it[ctx->context_index] || ip!=prev_ip[ctx->context_index]) {
      ctx->Volume->valid = 0;
      prev_it[ctx->context_index] = it;
      prev_ip[ctx->context_index] = ip;
   }

   /* Determine if we have to compute a set of slices for the direction. */
   if (ctx->Volume->dir!=dir || ctx->Volume->valid==0) {
      data = get_grid( ctx, it, ip );
      if (data) {
         if (ctx->GridSameAsGridPRIME){
            compute_volume( ctx, data, it, ip, ctx->Nr, ctx->Nc, ctx->Nl[ip],
                            ctx->LowLev[ip], ctx->MinVal[ip], ctx->MaxVal[ip],
                            dir, ctx->Volume );
         }
         else{
            compute_volumePRIME( ctx, data, it, ip, dtx->Nr, dtx->Nc, dtx->Nl,
                            dtx->LowLev, ctx->MinVal[ip], ctx->MaxVal[ip],
                            dir, ctx->Volume );
         }
         release_grid( ctx, it, ip, data );
      }
   }

   render_volume( ctx, ctx->Volume, ctable );
}
#else

/*
 * Draw a volumetric rendering of the grid for timestep it and variable ip.
 * Input: it - timestep
 *        ip - variable
 */
void draw_volume( Context ctx, int it, int ip, unsigned int *ctable )
{
   float *data;
   static int prev_it[VIS5D_MAX_CONTEXTS];
   static int prev_ip[VIS5D_MAX_CONTEXTS];
   static int do_once = 1;
   int dir;
   float x, y, z, ax, ay, az;
   MATRIX ctm, proj;
   Display_Context dtx;

   dtx = ctx->dpy_ctx;
   if (do_once){
      int yo;
      for (yo=0; yo<VIS5D_MAX_CONTEXTS; yo++){
         prev_it[yo] = -1;
         prev_ip[yo] = -1;
      }
      do_once = 0;
   }

   /* Get 3rd column values from transformation matrix */
#if defined (SGI_GL) || defined (DENALI)
   /* Compute orientation of 3-D box with respect to current matrices */
   /* with no assumptions about the location of the camera.  This was */
   /* done for the CAVE. */
   mmode( MPROJECTION );
   getmatrix( proj );
   mmode( MVIEWING );
   getmatrix( ctm );
#endif
#if defined(OPENGL)
   glGetFloatv( GL_PROJECTION_MATRIX, (GLfloat *) proj );
   glGetFloatv( GL_MODELVIEW_MATRIX, (GLfloat *) ctm );
#endif

   /* compute third column values in the product of ctm*proj */
   x = ctm[0][0]*proj[0][2] + ctm[0][1]*proj[1][2]
     + ctm[0][2]*proj[2][2] + ctm[0][3]*proj[3][2];
   y = ctm[1][0]*proj[0][2] + ctm[1][1]*proj[1][2]
     + ctm[1][2]*proj[2][2] + ctm[1][3]*proj[3][2];
   z = ctm[2][0]*proj[0][2] + ctm[2][1]*proj[1][2]
     + ctm[2][2]*proj[2][2] + ctm[2][3]*proj[3][2];

   /* examine values to determine how to draw slices */
   ax = ABS(x);
   ay = ABS(y);
   az = ABS(z);
   if (ax>=ay && ax>=az) {
      /* draw x-axis slices */
      dir = (x<0.0) ? WEST_TO_EAST : EAST_TO_WEST;
   }
   else if (ay>=ax && ay>=az) {
      /* draw y-axis slices */
      dir = (y<0.0) ? SOUTH_TO_NORTH : NORTH_TO_SOUTH;
   }
   else {
      /* draw z-axis slices */
      dir = (z<0.0) ? BOTTOM_TO_TOP : TOP_TO_BOTTOM;
   }

   /* If this is a new time step or variable then invalidate old volumes */
   if (it!=prev_it[ctx->context_index] || ip!=prev_ip[ctx->context_index]) {
      ctx->Volume->valid = 0;
      prev_it[ctx->context_index] = it;
      prev_ip[ctx->context_index] = ip;
   }

   /* Determine if we have to compute a set of slices for the direction. */
   if (ctx->Volume->dir!=dir || ctx->Volume->valid==0) {
      data = get_grid( ctx, it, ip );
      if (data) {
         if (ctx->GridSameAsGridPRIME){
            compute_volume( ctx, data, it, ip, ctx->Nr, ctx->Nc, ctx->Nl[ip],
                            ctx->LowLev[ip], ctx->MinVal[ip], ctx->MaxVal[ip],
                            dir, ctx->Volume );
         }
         else{
            compute_volumePRIME( ctx, data, it, ip, dtx->Nr, dtx->Nc, dtx->Nl,
                            dtx->LowLev, ctx->MinVal[ip], ctx->MaxVal[ip],
                            dir, ctx->Volume );
         }
         release_grid( ctx, it, ip, data );
      }
   }

   render_volume( ctx, ctx->Volume, ctable );
}
#endif
