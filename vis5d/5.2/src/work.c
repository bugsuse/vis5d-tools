/*work.c */

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

#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef sgi
#  include <sys/types.h>
#  include <sys/prctl.h>
#endif
#include "analysis.h"
#include "api.h"
#include "contour.h"
#include "globals.h"
#include "graphics.h"
#include "grid.h"
#include "imemory.h"
#include "memory.h"
#include "misc.h"
#include "proj.h"
#include "queue.h"
#include "record.h"
#include "render.h"
#include "stream.h"
#include "sync.h"
#include "textplot.h"
#include "topo.h"
#include "traj.h"
#include "vtmcP.h"
#include "work.h"

/* MJK 12.04.98 */
#include "linterp.h"
#include "map.h"


#define DEG2RAD(d)  ((d)*3.14159/180.0)
#define MIN(A,B)  ( (A) < (B) ? (A) : (B) )
#define MAX(A,B)  ( (A) > (B) ? (A) : (B) )


/* Maximum number of vertices... */
#ifdef BIG_GFX
/* WLH 31 Oct 98
#  define MAX_ISO_VERTS 1200000
*/
/* WLH 31 Oct 98 */
#  define MAX_ISO_VERTS 2400000    /* in an isosurface */
#else
#  define MAX_ISO_VERTS 650000     /* in an isosurface */
#endif


#define MAX_CONT_VERTS (MAXROWS*MAXCOLUMNS)   /* in a contour line slice */
#define MAX_WIND_VERTS (4*MAXROWS*MAXCOLUMNS) /* in a wind vector slice */
#define MAX_TRAJ_VERTS 5000                   /* in a wind trajectory */




/*
 * Compute the color table indexes for a colored isosurface.
 * Input:  ctx - the context
 *         time - which timestep
 *         isovar - the isosurface variable
 *         cvowner - the vis5d_ctx index the colorvar belongs to
 *         colorvar - the coloring variable
 */
static void color_isosurface( Context ctx, int time, int isovar, int cvowner, int colorvar )
{
   uint_1 *color_indexes;
   int i, n;
   float vscale = 1.0 / VERTEX_SCALE;
   float min, valscale;
   Context cvctx;
   Display_Context dtx;
   int cvctxtime;

   dtx = ctx->dpy_ctx;
   cvctx = ctx->dpy_ctx->ctxpointerarray[return_ctx_index_pos(ctx->dpy_ctx, cvowner)];
   if (!ctx->SameIsoColorVarOwner[isovar]){
      /* time = dtx time */
      cvctxtime = dtx->TimeStep[time].ownerstimestep[return_ctx_index_pos(dtx,
                                                     cvowner)];
   }
   else{
      /* time = ctx time */
      cvctxtime = time;
   }
   /* Free old color indexes if any */
   wait_write_lock( &ctx->SurfTable[isovar][time].lock );
   if (ctx->SurfTable[isovar][time].colors) {
      deallocate( ctx, ctx->SurfTable[isovar][time].colors,
                  ctx->SurfTable[isovar][time].numverts*sizeof(uint_1) );
      ctx->SurfTable[isovar][time].colors = NULL;
   }
   done_write_lock( &ctx->SurfTable[isovar][time].lock );

   if (colorvar!=-1) {
      /* Allocate storage for new color indexes */
      n = ctx->SurfTable[isovar][time].numverts;
      color_indexes = allocate( ctx, n*sizeof(uint_1) );
      if (!color_indexes) {
         return;
      }

      min = cvctx->MinVal[colorvar];

      /* MJK 12.04.98 */
      valscale = 254.0 / (cvctx->MaxVal[colorvar] - cvctx->MinVal[colorvar]);

      if (!check_for_valid_time(cvctx, time)){
         for (i=0;i<n;i++) {
            color_indexes[i] = 255;
         }
      }
      else{
         /* Compute color indexes */
         for (i=0;i<n;i++) {
            float x, y, z;
            float row, col, lev;
            float val;
            int index;

            x = ctx->SurfTable[isovar][time].verts[i*3+0] * vscale;
            y = ctx->SurfTable[isovar][time].verts[i*3+1] * vscale;
            z = ctx->SurfTable[isovar][time].verts[i*3+2] * vscale;

            xyzPRIME_to_grid( cvctx, time, colorvar, x, y, z, &row, &col, &lev );
               

            if (cvctx->Nl[colorvar]==1) {
               lev = 0.0;
            }
            val = interpolate_grid_value( cvctx, cvctxtime, colorvar, row, col, lev );
            if (IS_MISSING(val) ||
                val < cvctx->MinVal[colorvar] ||
                val > cvctx->MaxVal[colorvar]) {
               color_indexes[i] = 255;
            }
            else {
               /* MJK 12.04.98 */
               int index = (val - min) * valscale;
               color_indexes[i] = (index < 0) ? 0 : (index > 254) ? 254 : index;
            }
         }
      }
   }
   else {
      color_indexes = NULL;
   }

   /* save results */
   wait_write_lock( &ctx->SurfTable[isovar][time].lock );
   ctx->SurfTable[isovar][time].colors = color_indexes;
   ctx->SurfTable[isovar][time].colorvar = colorvar;
   ctx->SurfTable[isovar][time].cvowner = cvowner;
   done_write_lock( &ctx->SurfTable[isovar][time].lock );
}



/*
 * Calculate an isosurface and store it.
 * Input:  time - the time step.
 *         var - which variable.
 *         iso_level - level to construct contour at.
 *         colorvar - which variable to color with or -1
 *         threadnum - thread ID
 * Output:  resulting poly-triangle strip is saved in SurfTable.
 */
static void calc_isosurface( Context ctx, int time, int var,
                             float iso_level, int cvowner, int colorvar, int threadnum )
{
   /* marching cubes parameters */
   float arx=1.0, ary=1.0, arz=1.0;
   float *vc,  *vr,  *vl;
   float *vc2, *vr2, *vl2;
   float *nx,  *ny,  *nz;
   int *vpts;
   int numverts, numindexes, ipoly, itri;
   /* other vars */
   float *grid;
   int ctxtime;
   int_2 *cverts;
   int_1 *cnorms;
   Display_Context dtx;
#ifdef BIG_GFX
   uint_4 *index;
#else
   uint_2 *index;
#endif

   dtx = ctx->dpy_ctx;
   if (ctx->SameIsoColorVarOwner[var]){ 
      ctxtime = time;
   }
   else{
      ctxtime = dtx->TimeStep[time].ownerstimestep[return_ctx_index_pos(dtx,
                                                   ctx->context_index)];
   }
   if (!ctx->SurfTable[var][time].valid ||
       ctx->SurfTable[var][time].isolevel != iso_level) {

      /* compute the isosurface */

      grid = get_grid( ctx, ctxtime, var );  /* get pointer to grid data */
      if (!grid) return;

      vc = (float *) malloc(sizeof(float)*MAX_ISO_VERTS);
      vr = (float *) malloc(sizeof(float)*MAX_ISO_VERTS);
      vl = (float *) malloc(sizeof(float)*MAX_ISO_VERTS);
      vc2 = (float *) malloc(sizeof(float)*MAX_ISO_VERTS);
      vr2 = (float *) malloc(sizeof(float)*MAX_ISO_VERTS);
      vl2 = (float *) malloc(sizeof(float)*MAX_ISO_VERTS);
      nx = (float *) malloc(sizeof(float)*MAX_ISO_VERTS);
      ny = (float *) malloc(sizeof(float)*MAX_ISO_VERTS);
      nz = (float *) malloc(sizeof(float)*MAX_ISO_VERTS);
      vpts = (int *) malloc(sizeof(int)*2*MAX_ISO_VERTS);
      if ( !vc || !vr || !vl || !vc2 || !vr2 || !vl2 || !nx || !ny || !nz || !vpts ){
         printf(" You do not have enough memory to create isosurfaces.\n");
         if (vc){
            free(vc);
         }
         if (vr){
            free(vr);
         }
         if (vl){
            free(vl);
         }
         if (vc2){
            free(vc2);
         }
         if (vr2){
            free(vr2);
         }
         if (vl2){
            free(vl2);
         }
         if (nx){
            free(nx);
         }
         if (ny){
            free(ny);
         }
         if (nz){
            free(nz);
         }
         if (vpts){
            free(vpts);
         }
         release_grid( ctx, ctxtime, var, grid );
         return;
      }
      /* Pass number of levels of parameter. main_march is not changed */
      main_march( ctx,  grid, ctx->Nc, ctx->Nr, ctx->Nl[var], ctx->LowLev[var],
              iso_level, arx, ary, arz, MAX_ISO_VERTS,
              vc,vr,vl, nx,ny,nz, 2*MAX_ISO_VERTS, vpts,
              &numverts, &numindexes, &ipoly, &itri );

      release_grid( ctx, ctxtime, var, grid );

      recent( ctx, ISOSURF, var );

      if (numindexes>MAX_ISO_VERTS)
         numindexes = MAX_ISO_VERTS;

      /*************************** Compress data ***************************/

      if (numverts>0 && numindexes>0) {
         int vbytes, nbytes, bytes, i;

         /* allocate memory to store compressed vertices */
         vbytes = 3*numverts*sizeof(int_2);
         cverts = (int_2 *) allocate_type( ctx, vbytes, CVX_TYPE );
         /* convert (r,c,l) coords to compressed (x,y,z) */
         if (ctx->GridSameAsGridPRIME){
            gridPRIME_to_compXYZPRIME( dtx, time, var, numverts, vr,vc,vl, (void*) cverts );
         }
         else{
            grid_to_gridPRIME( ctx, time, var, numverts,vr,vc,vl,vr2, vc2, vl2);
            gridPRIME_to_compXYZPRIME( ctx->dpy_ctx, time, var, numverts,
                                       vr2, vc2, vl2, (void*) cverts );
         }
         /* allocate memory to store compressed normals */
         nbytes = 3*numverts*sizeof(int_1);
         cnorms = (int_1 *) allocate_type( ctx, nbytes, CNX_TYPE );
         /* cvt normals from floats in [-1,1] to 1-byte ints in [-125,125] */

         if (ctx->GridSameAsGridPRIME){
            project_normals( ctx, numverts, vr,vc,vl, nx,ny,nz, (void*) cnorms );
         }
         else{
            project_normalsPRIME( dtx, numverts, vr2, vc2, vl2, nx,ny,nz, (void*) cnorms );
         }

         /* allocate memory to store index array */
#ifdef BIG_GFX
         bytes = numindexes * sizeof(uint_4);
         index = (uint_4 *) allocate_type( ctx, bytes, PTS_TYPE );
         memcpy( index, vpts, numindexes * sizeof(uint_4) );
#else
         bytes = numindexes * sizeof(uint_2);
         index = (uint_2 *) allocate_type( ctx, bytes, PTS_TYPE );
         /* convert 4-byte vpts[] elements to 2-byte index[] elements */
         for (i=0;i<numindexes;i++)
            index[i] = (uint_2) vpts[i];
#endif

      }
      else {
         cverts = NULL;
         cnorms = NULL;
         index = NULL;
         numverts = numindexes = 0;
      }

      /******************** Store the new surface ************************/

      wait_write_lock( &ctx->SurfTable[var][time].lock );

      /* deallocate existing surface, if any */
      free_isosurface( ctx, time, var );

      /* add surface to table */
      ctx->SurfTable[var][time].isolevel = iso_level;
      ctx->SurfTable[var][time].numverts = numverts;
      ctx->SurfTable[var][time].verts = cverts;
      ctx->SurfTable[var][time].norms = cnorms;
      ctx->SurfTable[var][time].numindex = numindexes;
      ctx->SurfTable[var][time].index = index;
      ctx->SurfTable[var][time].valid = 1;

      done_write_lock( &ctx->SurfTable[var][time].lock );
      /* BUG FIX MJK 8.6.98
         These free statments were previously outside of this
         large if statment 
      */
      free(vc);
      free(vr);
      free(vl);
      free(vc2);
      free(vr2);
      free(vl2);
      free(nx);
      free(ny);
      free(nz);
      free(vpts);

   }

   if (colorvar!=-1 || (ctx->SurfTable[var][time].cvowner != cvowner) ||
                       (ctx->SurfTable[var][time].colorvar!=colorvar &&
                        ctx->SurfTable[var][time].cvowner ==cvowner)) {
      color_isosurface( ctx, time, var, cvowner, colorvar );
   }

/******* YO this dosn't make sense
   if (colorvar!=-1 || ctx->SurfTable[var][time].colorvar!=colorvar) {
      color_isosurface( ctx, time, var, cvowner, colorvar );
   }
*******/

   if((ctx->SameIsoColorVarOwner[var] && time==ctx->CurTime) ||
      (!ctx->SameIsoColorVarOwner[var] && time==ctx->dpy_ctx->CurTime)){
      ctx->dpy_ctx->Redraw = 1;
   }
}

/* MJK 12.04.98 begin */
static int fit_vecs_to_topo (Context ctx, int num, int max,
                                  float *vr, float *vc, float *vl)
{

    int         i, j, k, n_bytes, n_out, n_new;
    float       *vr_out, *vc_out, *vl_out, *vr_new, *vc_new, *vl_new;
    float       xyz[2][3], *xyz_new;
    float       xmin, ymin, xmax, ymax, xfac, yfac, xtmp, ytmp;
    Display_Context     dtx = ctx->dpy_ctx;


    if (!dtx->TopoFlag) return num;
    if (dtx->TopoVertex == NULL) return num;
    if (num <= 0) return 0;

    xyz_new = allocate (ctx, (dtx->Nr * dtx->Nc * 3 * 3));
    if (xyz_new == NULL) return 0;

    n_bytes = max * sizeof (float);
    vr_out  = allocate (ctx, n_bytes);
    vc_out  = allocate (ctx, n_bytes);
    vl_out  = allocate (ctx, n_bytes);
    if ((vr_out == NULL) || (vc_out == NULL) || (vl_out == NULL))
    {
        if (vr_out != NULL) deallocate (ctx, vr_out, -1);
        if (vc_out != NULL) deallocate (ctx, vc_out, -1);
        if (vl_out != NULL) deallocate (ctx, vl_out, -1);
        deallocate (ctx, xyz_new, -1);
        return 0;
    }


    xmin = dtx->Xmin;
    xmax = dtx->Xmax;
    xfac = (xmax - xmin) / ((float) (dtx->Nc - 1));
    ymin = dtx->Ymin;
    ymax = dtx->Ymax;
    yfac = (ymax - ymin) / ((float) (dtx->Nr - 1));

    n_out = 0;
    for (i = 0; i < num; i += 2)
    {
        xyz[0][0] = (vc[i] * xfac) + xmin;
        xyz[0][1] = ymax - (vr[i] * yfac);
        xyz[1][0] = (vc[i+1] * xfac) + xmin;
        xyz[1][1] = ymax - (vr[i+1] * yfac);
        n_new = bend_line_to_fit_topo (dtx, (float *) xyz, 2, xyz_new);

        if ((n_out+(n_new*2)-1) >= max) break;

        for (j = 0; j < n_new; j++)
        {
            xyz_new[j*3+0] = (xyz_new[j*3+0] - xmin) / xfac;
            xyz_new[j*3+1] = (ymax - xyz_new[j*3+1]) / yfac;
            xyzPRIME_to_gridPRIME (dtx, -1, -1, 0.0, 0.0, xyz_new[j*3+2],
                         &xtmp, &ytmp, &xyz_new[j*3+2]);
        }

        k = 0;
        for (j = 1; j < n_new; k = j++)
        {
            vc_out[n_out] = xyz_new[k*3+0];
            vr_out[n_out] = xyz_new[k*3+1];
            vl_out[n_out] = xyz_new[k*3+2];
            n_out++;

            vc_out[n_out] = xyz_new[j*3+0];
            vr_out[n_out] = xyz_new[j*3+1];
            vl_out[n_out] = xyz_new[j*3+2];
            n_out++;
        }
    }

    if (n_out > 0)
    {
        memcpy (vr, vr_out, n_bytes);
        memcpy (vc, vc_out, n_bytes);
        memcpy (vl, vl_out, n_bytes);
    }

    deallocate (ctx, vr_out, -1);
    deallocate (ctx, vc_out, -1);
    deallocate (ctx, vl_out, -1);
    deallocate (ctx, xyz_new, -1);


    return n_out;
}
/* MJK 12.04.98 end */

/* MJK 12.04.98 begin */
/*
 * Extract data to be displayed at the surface (topography).
 * Input:  ctx - the context
 *         time - which timestep
 *         var - the variable
 *         colmajor - 1 = column major, 0 = row major.
 * Returned:  pointer to 2D array of floats
 */
static float *extract_sfc_slice (Context ctx, int time, int var,
                                    int nrows, int ncols,
                                    float *grid, int colmajor)
{

    int         ir, ic, di, dinc, ti, tr, tc;
    float       *slice_data, x, y, z, grow, gcol, glev, gbot, gtop;
    double      rr, cc, dr, dc;
    Display_Context     dtx = ctx->dpy_ctx;
    float thelat, thelon, thehgt;
    int nothing;

    slice_data = (float *) allocate_type (ctx, nrows * ncols * sizeof (float),
                                          HSLICE_TYPE);
    if (slice_data == NULL) return NULL;

    gbot = ctx->LowLev[var];
    gtop = ctx->Nl[var] + ctx->LowLev[var] - 1;

    dr   = ((float) (dtx->qrows - 1)) / ((float) (nrows - 1));
    dc   = ((float) (dtx->qcols - 1)) / ((float) (ncols - 1));
    rr   = 0.0;
    di   = 0;
    dinc = (colmajor) ? nrows : 1;
    if (ctx->GridSameAsGridPRIME){
       for (ir = 0; ir < nrows; ir++, rr += dr)
       {
           tr = rr + 0.5;
           cc = 0.0;
           if (colmajor) di = ir;
           for (ic = 0; ic < ncols; ic++, di += dinc, cc += dc)
           {
               tc = cc + 0.5;
               ti = (tr * dtx->qcols) + tc;
               x  = dtx->TopoVertex[ti*3+0];
               y  = dtx->TopoVertex[ti*3+1];
               z  = dtx->TopoVertex[ti*3+2];
               xyz_to_grid (ctx, time, var, x, y, z, &grow, &gcol, &glev);

               if (ctx->Nl[var] == 1) glev = gbot;
               if ((glev < gbot) || (glev > gtop))
               {
                   slice_data[di] = MISSING;
               }
               else
               {
                   slice_data[di] = interpolate_grid_value (ctx, time, var,
                                                            grow, gcol, glev);
               }
           }
       }
    }
    else{
       for (ir = 0; ir < nrows; ir++){
          for (ic = 0; ic < ncols; ic++){
             di = ir*ctx->Nc+ic;
             rowcol_to_latlon( ctx, time, var, ir, ic, &thelat, &thelon);
             thehgt = elevation( dtx, thelat, thelon, &nothing);
             geo_to_grid(ctx, time, var, 1, &thelat, &thelon, &thehgt,
                              &grow, &gcol, &glev);

             if (glev < 0 || glev > ctx->Nl[var]-1){
                slice_data[di] = MISSING;
             }
             else{
                if (ctx->Nl[var] == 1) glev = gbot;
                if ((glev < gbot) || (glev > gtop))
                {
                    slice_data[di] = MISSING;
                }
                else
                {
                    slice_data[di] = interpolate_grid_value (ctx, time, var,
                                                             (float)ir, (float)ic, glev);
                }
             }
 
          }
       }
    }
    return slice_data;
}
/* MJK 12.04.98 end */



/*
 * Extract a horizontal slice from a 3-D grid.
 * Input:  grid - pointer to nr * nc * nl 3-D grid of data
 *         nr, nc, nl - size of 3-D grid.
 *         level - position in [0,numlev-1] to extract slice from.
 *         colmajor - 1 = column major, 0 = row major.
 * Returned:  pointer to nr x nc array of floats
 */
static float* extract_hslice( Context ctx, float *grid, int var,
                              int nr, int nc, int nl, int lowlev,
                              float level, int colmajor )
{
   float *slice;

   /* allocate buffer to put 2-D slice of data */
   slice = (float *) allocate_type( ctx, nr * nc * sizeof(float), HSLICE_TYPE );
   if (!slice) {
      return NULL;
   }

   /* extract the 2-D array from 3-D grid */
   if (ctx->Nl[var]==1) {
      /*
       * Special case:  the 3-D grid is really 2-D
       */
      float g1;
      int i, j;

      if (colmajor) {
         for (j=0; j<nc; j++) {
            for (i=0; i<nr; i++) {
               g1 = grid[j*nr+i];
               if (IS_MISSING(g1)) {
                  slice[j*nr+i] = MISSING;
               }
               else {
                  slice[j*nr+i] = g1;
               }
            }
         }
      }
      else {
         /* change from column-major to row-major order */
         for (i=0; i<nr; i++) {
            for (j=0; j<nc; j++) {
               g1 = grid[j*nr+i];
               if (IS_MISSING(g1)) {
                  slice[i*nc+j] = MISSING;
               }
               else {
                  slice[i*nc+j] = g1;
               }
            }
         }
      }
   }
   else {
      /*
       * extract (and interpolate) 2-D slice from 3-D grid
       */
      int upper, lower, above, below;
      float a, b, g1, g2;
      int i, j;

/* WLH 15 Oct 98
      level -= lowlev;
      if (level < 0 || level > nl-1) {
        for (i=0; i<nr*nc; i++) slice[i] = MISSING;
        return slice;
      }
      lower = (int) level;
      upper = lower + 1;
      if (upper>nl-1)
         upper = nl - 1;
      a = level - (float) lower;
      b = 1.0 - a;
*/
      /* WLH 15 Oct 98 */
      level -= ctx->LowLev[var];
      if (level < 0 || level > ctx->Nl[var]-1) {
        for (i=0; i<nr*nc; i++) slice[i] = MISSING;
        return slice;
      }
      lower = (int) level;
      upper = lower + 1;
      if (upper>ctx->Nl[var]-1)
         upper = ctx->Nl[var] - 1;
      a = level - (float) lower;
      b = 1.0 - a;


      if (a==0.0) {
         /* If the location of the slice exactly corresponds to a discrete */
         /* grid level, don't interpolate with next higher level! */
         upper = lower;
      }

#ifdef LEVELTYPES 
      /* Correct if logaritmic interpolation required */
#include "logfrac.h"
      printf("linfrac=%f   ", a);
      CalcLinLogFrac(lower, a, b);
      printf("logfrac=%f\n", a);
#endif

      below = lower * nr * nc;
      above = upper * nr * nc;

      /* interpolate between layers */
      if (colmajor) {
         for (j=0; j<nc; j++) {
            for (i=0; i<nr; i++) {
               g1 = grid[above+j*nr+i];
               g2 = grid[below+j*nr+i];
               if (IS_MISSING(g1) || IS_MISSING(g2)) {
                  slice[j*nr+i] = MISSING;
               }
               else {
                  slice[j*nr+i] = a * g1 + b * g2; 
               }
            }
         }
      }
      else {
         /* change from column-major to row-major order */
         for (i=0; i<nr; i++) {
            for (j=0; j<nc; j++) {
               g1 = grid[above+j*nr+i];
               g2 = grid[below+j*nr+i];
               if (IS_MISSING(g1) || IS_MISSING(g2)) {
                  slice[i*nc+j] = MISSING;
               }
               else {
                  slice[i*nc+j] = a * g1 + b * g2;
               }
            }
         }
      }
   }

   return slice;
}

static float* extract_hslicePRIME( Context ctx, float *grid, int time, int var,
                              int nr, int nc, int nl, int lowlev,
                              float level, int colmajor )
{
   float *slice;
   float lat, lon, row, col, lev;
   float s1,s2,s3,s4,s5,s6,s7,s8;
   float grow, gcol, glev;
   int gr0,gr1,gc0,gc1,gl0,gl1;
   float ger, gec, gel;
   int cont = 1;
   Display_Context dtx;

   dtx = ctx->dpy_ctx;
   /* allocate buffer to put 2-D slice of data */
   slice = (float *) allocate_type( ctx, nr * nc * sizeof(float), HSLICE_TYPE );
   if (!slice) {
      return NULL;
   }

   /* extract the 2-D array from 3-D grid */
   if (ctx->Nl[var]==1) {
      /*
       * Special case:  the 3-D grid is really 2-D
       */
      float g1;
      int i, j;

      if (colmajor) {
         for (j=0; j<nc; j++) {
            for (i=0; i<nr; i++) {
               row = i;
               col = j;
               rowcolPRIME_to_latlon(dtx, 0, 0, row, col, &lat, &lon);
               latlon_to_rowcol(ctx, 0, 0, lat, lon, &grow, &gcol);
               if ( grow < 0 || gcol < 0 ||
                    grow >= ctx->Nr || gcol >= ctx->Nc ){
                  slice[j*nr+i] = MISSING;
                  cont = 0;
               }
               if (cont){
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
                  ger = grow - (float) gr0; /* in [0,1) */
                  gec = gcol - (float) gc0; /* in [0,1) */
                  if (ger==0.0){
                     gr1 = gr0;
                  }
                  if (gec==0.0){
                     gc1 = gc0;
                  }
                  s1 = grid[gc0*ctx->Nr+gr0];
                  s2 = grid[gc0*ctx->Nr+gr1];
                  s3 = grid[gc1*ctx->Nr+gr0];
                  s4 = grid[gc1*ctx->Nr+gr1];
                  if (IS_MISSING(s1) || IS_MISSING(s2) ||
                      IS_MISSING(s3) || IS_MISSING(s4)){
                     slice[j*nr+i] = MISSING;
                  }
                  else{
                     slice[j*nr+i] = ( s1 * (1.0-ger) * (1.0-gec)
                                     + s2 * ger       * (1.0-gec)
                                     + s3 * (1.0-ger) * gec
                                     + s4 * ger       * gec        );
                  }
               }
               cont = 1;
            }
         }
      }
      else {
         /* change from column-major to row-major order */
         for (i=0; i<nr; i++) {
            for (j=0; j<nc; j++) {
               row = i;
               col = j;
               rowcolPRIME_to_latlon(dtx, 0, 0, row, col, &lat, &lon);
               latlon_to_rowcol(ctx, 0, 0, lat, lon, &grow, &gcol);
               if ( grow < 0 || gcol < 0 ||
                    grow >= ctx->Nr || gcol >= ctx->Nc ){
                  slice[i*nc+j] = MISSING;
                  cont = 0;
               }
               if (cont){
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
                  ger = grow - (float) gr0; /* in [0,1) */
                  gec = gcol - (float) gc0; /* in [0,1) */
                  if (ger==0.0){
                     gr1 = gr0;
                  }
                  if (gec==0.0){
                     gc1 = gc0;
                  }
                  s1 = grid[gc0*ctx->Nr+gr0];
                  s2 = grid[gc0*ctx->Nr+gr1];
                  s3 = grid[gc1*ctx->Nr+gr0];
                  s4 = grid[gc1*ctx->Nr+gr1];
                  if (IS_MISSING(s1) || IS_MISSING(s2) ||
                      IS_MISSING(s3) || IS_MISSING(s4)){
                     slice[i*nc+j] = MISSING;
                  }
                  else{
                     slice[i*nc+j] = ( s1 * (1.0-ger) * (1.0-gec)
                                     + s2 * ger       * (1.0-gec)
                                     + s3 * (1.0-ger) * gec
                                     + s4 * ger       * gec        );
                  }
               }
               cont = 1;
            }
         }
      }
   }
   else {
      /*
       * extract (and interpolate) 2-D slice from 3-D grid
       */
      int upper, lower, above, below;
      float a, b, g1, g2;
      int i, j;

      /* WLH 6-30-95 */
      level -= lowlev;
      if (level < 0 || level > nl-1) {
        for (i=0; i<nr*nc; i++) slice[i] = MISSING;
        return slice;
      }

      lower = (int) level;
      upper = lower + 1;
      if (upper>nl-1)
         upper = nl - 1;
      a = level - (float) lower;
      b = 1.0 - a;

      if (a==0.0) {
         /* If the location of the slice exactly corresponds to a discrete */
         /* grid level, don't interpolate with next higher level! */
         upper = lower;
      }

#ifdef LEVELTYPES
      /* Correct if logaritmic interpolation required */
#include "logfrac.h"
      printf("linfrac=%f   ", a);
      CalcLinLogFrac(lower, a, b);
      printf("logfrac=%f\n", a);
#endif

      below = lower * nr * nc;
      above = upper * nr * nc;

      /* interpolate between layers */
      if (colmajor) {
         for (j=0; j<nc; j++) {
            for (i=0; i<nr; i++) {
               row = i;
               col = j;
               lev = upper;
               gridPRIME_to_grid( ctx, time, var, 1, &row, &col, &lev, 
                                   &grow, &gcol, &glev);
               if ( grow < 0 || gcol < 0 || glev < 0 ||
                 grow >= ctx->Nr || gcol >= ctx->Nc || glev >= ctx->Nl[var]){
                  slice[j*nr+i] = MISSING;
                  cont = 0;
               }
               if (cont){
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

                  s1 = grid[(gl0*ctx->Nc+gc0)*ctx->Nr+gr0];
                  s2 = grid[(gl0*ctx->Nc+gc0)*ctx->Nr+gr1];
                  s3 = grid[(gl0*ctx->Nc+gc1)*ctx->Nr+gr0];
                  s4 = grid[(gl0*ctx->Nc+gc1)*ctx->Nr+gr1];
                  s5 = grid[(gl1*ctx->Nc+gc0)*ctx->Nr+gr0];
                  s6 = grid[(gl1*ctx->Nc+gc0)*ctx->Nr+gr1];
                  s7 = grid[(gl1*ctx->Nc+gc1)*ctx->Nr+gr0];
                  s8 = grid[(gl1*ctx->Nc+gc1)*ctx->Nr+gr1];

                  if (IS_MISSING(s1) || IS_MISSING(s2) ||
                      IS_MISSING(s3) || IS_MISSING(s4) ||
                      IS_MISSING(s5) || IS_MISSING(s6) ||
                      IS_MISSING(s7) || IS_MISSING(s8)){
                     g1 = MISSING;
                  }
                  else{
                     g1 = ( s1 * (1.0-ger) * (1.0-gec)
                          + s2 * ger       * (1.0-gec)
                          + s3 * (1.0-ger) * gec
                          + s4 * ger       * gec        ) * (1.0-gel)
                       +  ( s5 * (1.0-ger) * (1.0-gec)
                          + s6 * ger       * (1.0-gec)
                          + s7 * (1.0-ger) * gec
                          + s8 * ger       * gec        ) * gel;
                  }
               }
               /*get g2 */
               if (cont){
                  row = i;
                  col = j;
                  lev = lower;
                  gridPRIME_to_grid( ctx, time, var, 1, &row, &col, &lev, &grow, &gcol, &glev);
                  if ( grow < 0 || gcol < 0 || glev < 0 ||
                       grow >= ctx->Nr || gcol >= ctx->Nc || glev >= ctx->Nl[var]){
                     slice[j*nr+i] = MISSING;
                     cont = 0;
                  }
                  if (cont){
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
                     s1 = grid[(gl0*ctx->Nc+gc0)*ctx->Nr+gr0];
                     s2 = grid[(gl0*ctx->Nc+gc0)*ctx->Nr+gr1];
                     s3 = grid[(gl0*ctx->Nc+gc1)*ctx->Nr+gr0];
                     s4 = grid[(gl0*ctx->Nc+gc1)*ctx->Nr+gr1];
                     s5 = grid[(gl1*ctx->Nc+gc0)*ctx->Nr+gr0];
                     s6 = grid[(gl1*ctx->Nc+gc0)*ctx->Nr+gr1];
                     s7 = grid[(gl1*ctx->Nc+gc1)*ctx->Nr+gr0];
                     s8 = grid[(gl1*ctx->Nc+gc1)*ctx->Nr+gr1];

                     if (IS_MISSING(s1) || IS_MISSING(s2) ||
                         IS_MISSING(s3) || IS_MISSING(s4) ||
                         IS_MISSING(s5) || IS_MISSING(s6) ||
                         IS_MISSING(s7) || IS_MISSING(s8)){
                        g2 = MISSING;
                     }
                     else{
                        g2 = ( s1 * (1.0-ger) * (1.0-gec)
                             + s2 * ger       * (1.0-gec)
                             + s3 * (1.0-ger) * gec
                             + s4 * ger       * gec        ) * (1.0-gel)
                          +  ( s5 * (1.0-ger) * (1.0-gec)
                             + s6 * ger       * (1.0-gec)
                             + s7 * (1.0-ger) * gec
                             + s8 * ger       * gec        ) * gel;
                     }
                  }
               }
               if (cont && !IS_MISSING(g1) && !IS_MISSING(g2)){
                  slice[j*nr+i] = a * g1 + b * g2;
               }
               else{
                  slice[j*nr+i] = MISSING;
               }
               cont = 1;
            }
            cont = 1;
         }
      }
      else {
         /* change from column-major to row-major order */
         for (i=0; i<nr; i++) {
            for (j=0; j<nc; j++) {
               row = i;
               col = j;
               lev = upper;
               gridPRIME_to_grid( ctx, time, var, 1, &row, &col, &lev,
                                   &grow, &gcol, &glev);
               if ( grow < 0 || gcol < 0 || glev < 0 ||
                 grow >= ctx->Nr || gcol >= ctx->Nc || glev >= ctx->Nl[var]){
                  slice[i*nc+j] = MISSING;
                  cont = 0;
               }
               if (cont){
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

                  s1 = grid[(gl0*ctx->Nc+gc0)*ctx->Nr+gr0];
                  s2 = grid[(gl0*ctx->Nc+gc0)*ctx->Nr+gr1];
                  s3 = grid[(gl0*ctx->Nc+gc1)*ctx->Nr+gr0];
                  s4 = grid[(gl0*ctx->Nc+gc1)*ctx->Nr+gr1];
                  s5 = grid[(gl1*ctx->Nc+gc0)*ctx->Nr+gr0];
                  s6 = grid[(gl1*ctx->Nc+gc0)*ctx->Nr+gr1];
                  s7 = grid[(gl1*ctx->Nc+gc1)*ctx->Nr+gr0];
                  s8 = grid[(gl1*ctx->Nc+gc1)*ctx->Nr+gr1];

                  if (IS_MISSING(s1) || IS_MISSING(s2) ||
                      IS_MISSING(s3) || IS_MISSING(s4) ||
                      IS_MISSING(s5) || IS_MISSING(s6) ||
                      IS_MISSING(s7) || IS_MISSING(s8)){
                     g1 = MISSING;
                  }
                  else{
                     g1 = ( s1 * (1.0-ger) * (1.0-gec)
                          + s2 * ger       * (1.0-gec)
                          + s3 * (1.0-ger) * gec
                          + s4 * ger       * gec        ) * (1.0-gel)
                       +  ( s5 * (1.0-ger) * (1.0-gec)
                          + s6 * ger       * (1.0-gec)
                          + s7 * (1.0-ger) * gec
                          + s8 * ger       * gec        ) * gel;
                  }
               }
               /*get g2 */
               if (cont){
                  row = i;
                  col = j;
                  lev = lower;
                  gridPRIME_to_grid( ctx, time, var, 1, &row, &col, &lev, &grow, &gcol, &glev);
                  if ( grow < 0 || gcol < 0 || glev < 0 ||
                       grow >= ctx->Nr || gcol >= ctx->Nc || glev >= ctx->Nl[var]){
                     slice[i*nc+j] = MISSING;
                     cont = 0;
                  }
                  if (cont){
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

                     s1 = grid[(gl0*ctx->Nc+gc0)*ctx->Nr+gr0];
                     s2 = grid[(gl0*ctx->Nc+gc0)*ctx->Nr+gr1];
                     s3 = grid[(gl0*ctx->Nc+gc1)*ctx->Nr+gr0];
                     s4 = grid[(gl0*ctx->Nc+gc1)*ctx->Nr+gr1];
                     s5 = grid[(gl1*ctx->Nc+gc0)*ctx->Nr+gr0];
                     s6 = grid[(gl1*ctx->Nc+gc0)*ctx->Nr+gr1];
                     s7 = grid[(gl1*ctx->Nc+gc1)*ctx->Nr+gr0];
                     s8 = grid[(gl1*ctx->Nc+gc1)*ctx->Nr+gr1];

                     if (IS_MISSING(s1) || IS_MISSING(s2) ||
                         IS_MISSING(s3) || IS_MISSING(s4) ||
                         IS_MISSING(s5) || IS_MISSING(s6) ||
                         IS_MISSING(s7) || IS_MISSING(s8)){
                        g2 = MISSING;
                     }
                     else{
                        g2 = ( s1 * (1.0-ger) * (1.0-gec)
                             + s2 * ger       * (1.0-gec)
                             + s3 * (1.0-ger) * gec
                             + s4 * ger       * gec        ) * (1.0-gel)
                          +  ( s5 * (1.0-ger) * (1.0-gec)
                             + s6 * ger       * (1.0-gec)
                             + s7 * (1.0-ger) * gec
                             + s8 * ger       * gec        ) * gel;
                     }
                  }
               }
               if (cont && !IS_MISSING(g1) && !IS_MISSING(g2)){
                  slice[i*nc+j] = a * g1 + b * g2;
               }
               else{
                  slice[i*nc+j] = MISSING;
               }
               cont = 1;
            }
         }
      }
   }
   return slice;
}



/*** extract_vslice ***************************************************
   Extract a vertical slice from a 3-D grid.
   Input:  grid - pointer to Nl x ctx->Nr * ctx->Nc float array.
           r1,c1,r2,c2 - position in [0,ctx->Nr-1],[0,ctx->Nc-1] to extract slice.
           cols, rows - size of slice returned.
           colmajor - 1 = column major, 0 = row major.
   Returned:  pointer to ctx->Nr x ctx->Nc array (slice) of floats
              or NULL if error
**********************************************************************/
static float *extract_vslice( Context ctx, float *grid,
                              float r1, float c1, float r2, float c2,
                              int rows, int cols,
                              int colmajor )
{
   float gx,gy, stepx, stepy;
   int ic, ir, i, j, iii;
   float g1,g2,g3,g4, ei,ej;
   float *slice;

   /* allocate slice array */
   slice = (float *) allocate_type( ctx, rows * cols * sizeof(float), VSLICE_TYPE );
   if (!slice)
      return NULL;

   /* initialize gx and gy- the real grid indices */
   gx = c1;
   gy = r1;

   /* calculate step sizes for iterating from c1 to c2 and r1 to r2 */
   stepx = (c2-c1) / (float) (cols-1);
   stepy = (r2-r1) / (float) (cols-1);

   if (colmajor) {
      for (ic=0; ic<cols; ic++) {
         /* convert real gx,gy to integer i,j for array indexing */
         i = (int) gx;
         j = (int) gy;
         if (i > ctx->Nc-2) {
            i = ctx->Nc-2;
         }
         if (j > ctx->Nr-2) {
            j = ctx->Nr-2;
         }
         /* calculate error terms */
         ei = gx - (float) i;   /* in [0,1) */
         ej = gy - (float) j;   /* in [0,1) */
         for (ir=0; ir<rows; ir++) {
            iii = ir * ctx->Nr * ctx->Nc;
            /* fetch grid data */
            g1 = grid[iii + i*ctx->Nr + j];          /*  g1 -- g2    +-- j */
            g3 = grid[iii + i*ctx->Nr + j+1];        /*  |      |    |     */
            if (ei!=0.0) {                           /*  g3 -- g4    i     */
               g2 = grid[iii + (i+1)*ctx->Nr + j];
               g4 = grid[iii + (i+1)*ctx->Nr + j+1];
            }
            else {
               g2 = g4 = 0.0;
            }
            if (IS_MISSING(g1) || IS_MISSING(g2)
                || IS_MISSING(g3) || IS_MISSING(g4)) {
               slice[ ic*rows + rows - ir - 1 ] = MISSING;
            }
            else {
               slice[ ic*rows + rows - ir - 1 ] = g1 * (1.0-ei) * (1.0-ej)
                                                + g2 *      ei  * (1.0-ej)
                                                + g3 * (1.0-ei) *      ej
                                                + g4 *      ei  *      ej;
            }
         }
         /* increment grid indices */
         gx += stepx;
         gy += stepy;
      }
   }
   else {
      for (ic=0; ic<cols; ic++) {
         /* convert real gx,gy to integer i,j for array indexing */
         i = (int) gx;
         j = (int) gy;
         if (i > ctx->Nc-2) {
            i = ctx->Nc-2;
         }
         if (j > ctx->Nr-2) {
            j = ctx->Nr-2;
         }
         /* calculate error terms */
         ei = gx - (float) i;   /* in [0,1) */
         ej = gy - (float) j;   /* in [0,1) */
         for (ir=0; ir<rows; ir++) {
            iii = ir * ctx->Nr * ctx->Nc;
            /* fetch grid data */
            g1 = grid[iii + i*ctx->Nr + j];            /* g1 -- g2    +-- j */
            g3 = grid[iii + i*ctx->Nr + j+1];          /* |      |    |     */
            if (ei!=0.0) {                             /* g3 -- g4    i     */
               g2 = grid[iii + (i+1)*ctx->Nr + j];
               g4 = grid[iii + (i+1)*ctx->Nr + j+1];
            }
            else {
               g2 = g4 = 0.0;
            }
            if (IS_MISSING(g1) || IS_MISSING(g2)
                || IS_MISSING(g3) || IS_MISSING(g4)) {
               slice[ ir*cols + ic ] = MISSING;
            }
            else {
               slice[ ir*cols + ic ] = g1 * (1.0-ei) * (1.0-ej)
                                     + g2 *      ei  * (1.0-ej)
                                     + g3 * (1.0-ei) *      ej
                                     + g4 *      ei  *      ej;
            }
         }
         /* increment grid indices */
         gx += stepx;
         gy += stepy;
      }
   }

   return slice;
}


/*** extract_vslicePRIME ***************************************************
   Extract a vertical slice from a virtual 3-D grid.
   Input:  grid - pointer to Nl x ctx->Nr * ctx->Nc float array.
           r1,c1,r2,c2 - position in [0,dtx->Nr-1],[0,dtx->Nc-1] to extract slice.
           cols, rows - size of slice returned.
           colmajor - 1 = column major, 0 = row major.
   Returned:  pointer to ctx->Nr x ctx->Nc array (slice) of floats
              or NULL if error
**********************************************************************/
static float *extract_vslicePRIME( Context ctx, float *grid, int time, int var, 
                              float r1, float c1, float r2, float c2,
                              int rows, int cols,
                              int colmajor )
{
   float gx,gy, stepx, stepy;
   int ic, ir, i, j, iii;
   float g1,g2,g3,g4, ei,ej;
   float *slice;
   float row, col, lev;
   float s1,s2,s3,s4,s5,s6,s7,s8;
   float grow, gcol, glev;
   int gr0,gr1,gc0,gc1,gl0,gl1;
   float ger, gec, gel;
   int cont = 1;
   Display_Context dtx;

   dtx = ctx->dpy_ctx;
   /* allocate slice array */
   slice = (float *) allocate_type( ctx, rows * cols * sizeof(float), VSLICE_TYPE );
   if (!slice)
      return NULL;

   /* initialize gx and gy- the real grid indices */
   gx = c1;
   gy = r1;

   /* calculate step sizes for iterating from c1 to c2 and r1 to r2 */
   stepx = (c2-c1) / (float) (cols-1);
   stepy = (r2-r1) / (float) (cols-1);

   if (colmajor) {
      for (ic=0; ic<cols; ic++) {
         /* convert real gx,gy to integer i,j for array indexing */
         i = (int) gx;
         j = (int) gy;
         if (i >= dtx->Nc-1) {
            i = dtx->Nc-1;
         }
         if (j >= dtx->Nr-1) {
            j = dtx->Nr-1;
         }
         /* calculate error terms */
         ei = gx - (float) i;   /* in [0,1) */
         ej = gy - (float) j;   /* in [0,1) */
         for (ir=0; ir<rows; ir++) {
            iii = ir * dtx->Nr * dtx->Nc;
            row = j;
            col = i;
            lev = ir;
            gridPRIME_to_grid( ctx, time, var, 1, &row, &col, &lev, &grow, &gcol, &glev);
            g1 = interpolate_grid_value( ctx, time, var, grow, gcol, glev);

           /* now fing g3 value */
            row = j+1;
            col = i;
            lev = ir;

            gridPRIME_to_grid( ctx, time, var, 1, &row, &col, &lev, &grow, &gcol, &glev);
            g3 = interpolate_grid_value( ctx, time, var, grow, gcol, glev);

            if (ei!=0.0) {
               row = j;
               col = i+1;
               lev = ir;
               gridPRIME_to_grid( ctx, time, var, 1, &row, &col, &lev, &grow, &gcol, &glev);
               g2 = interpolate_grid_value( ctx, time, var, grow, gcol, glev);

               /* now get g4 value */
               row = j+1;
               col = i+1;
               lev = ir;
               gridPRIME_to_grid( ctx, time, var, 1, &row, &col, &lev, &grow, &gcol, &glev);
               g4 = interpolate_grid_value( ctx, time, var, grow, gcol, glev);
            }   
            else {
               g2 = g4 = 0.0;
            }
            if (IS_MISSING(g1) || IS_MISSING(g2)
                || IS_MISSING(g3) || IS_MISSING(g4)) {
               slice[ ic*rows + rows - ir - 1 ] = MISSING;
            }
            else {
               slice[ ic*rows + rows - ir - 1 ] = g1 * (1.0-ei) * (1.0-ej)
                                                + g2 *      ei  * (1.0-ej)
                                                + g3 * (1.0-ei) *      ej
                                                + g4 *      ei  *      ej;
            }
         }
         /* increment grid indices */
         gx += stepx;
         gy += stepy;
      }
   }
   else {
      for (ic=0; ic<cols; ic++) {
         /* convert real gx,gy to integer i,j for array indexing */
         i = (int) gx;
         j = (int) gy;
         if (i >= dtx->Nc-1) {
            i = dtx->Nc-1;
         }
         if (j >= dtx->Nr-1) {
            j = dtx->Nr-1;
         }
         /* calculate error terms */
         ei = gx - (float) i;   /* in [0,1) */
         ej = gy - (float) j;   /* in [0,1) */
         for (ir=0; ir<rows; ir++) {
            iii = ir * dtx->Nr * dtx->Nc;

            row = j;
            col = i;
            lev = ir;
            gridPRIME_to_grid( ctx, time, var, 1, &row, &col, &lev, &grow, &gcol, &glev);
            g1 = interpolate_grid_value( ctx, time, var, grow, gcol, glev);

           /* now fing g3 value */
            row = j+1;
            col = i;
            lev = ir;

            gridPRIME_to_grid( ctx, time, var, 1, &row, &col, &lev, &grow, &gcol, &glev);
            g3 = interpolate_grid_value( ctx, time, var, grow, gcol, glev);

            if (ei!=0.0 && cont) {
               row = j;
               col = i+1;
               lev = ir;
               gridPRIME_to_grid( ctx, time, var, 1, &row, &col, &lev, &grow, &gcol, &glev);
               g2 = interpolate_grid_value( ctx, time, var, grow, gcol, glev);

               /* now get g4 value */
               row = j+1;
               col = i+1;
               lev = ir;
               gridPRIME_to_grid( ctx, time, var, 1, &row, &col, &lev, &grow, &gcol, &glev);
               g4 = interpolate_grid_value( ctx, time, var, grow, gcol, glev);
            }
            else {
               g2 = g4 = 0.0;
            }
            if (IS_MISSING(g1) || IS_MISSING(g2)
                || IS_MISSING(g3) || IS_MISSING(g4)) {
              slice[ ir*cols + ic ] = MISSING; 
            }
            else {
               slice[ ir*cols + ic ] =          g1 * (1.0-ei) * (1.0-ej)
                                                + g2 *      ei  * (1.0-ej)
                                                + g3 * (1.0-ei) *      ej
                                                + g4 *      ei  *      ej;
            }
         }
         /* increment grid indices */
         gx += stepx;
         gy += stepy;
      }
   }

   return slice;
}




/*
 * Generate a list of vertices for a drawing a horizontal bounding rectangle
 * around horizontal contour line slices or horizontal wind vector slices.
 * Input:  time, var - the timestep and variable
 *         levelPRIME - the display grid level in [0,Nl-1]
 *         curved - if non-zero, generate vertices for a curved box.
 * Output:  boxverts - pointer to array of vertices we've allocated.
 * Return:  number of vertices in the vertex list.
 */

static int make_horizontal_rectangle( Context ctx, int time, int var,
                                      int curved, float levelPRIME,
                                      float **boxverts )
{
   int i, n;
   float *v;

   n = 0;

   if (curved==0) {
      v = (float *) allocate_type( ctx, 5 * 3 * sizeof(float), MHRECT_TYPE );
      if (v) {
         n = 5;
         v[0*3+0] = 0.0;
         v[0*3+1] = 0.0;
         v[0*3+2] = levelPRIME;
         v[1*3+0] = 0.0;
         v[1*3+1] = (float) (ctx->dpy_ctx->Nc-1);
         v[1*3+2] = levelPRIME;
         v[2*3+0] = (float) (ctx->dpy_ctx->Nr-1);
         v[2*3+1] = (float) (ctx->dpy_ctx->Nc-1);
         v[2*3+2] = levelPRIME;
         v[3*3+0] = (float) (ctx->dpy_ctx->Nr-1);
         v[3*3+1] = 0.0;
         v[3*3+2] = levelPRIME;
         v[4*3+0] = 0.0;
         v[4*3+1] = 0.0;
         v[4*3+2] = levelPRIME;
      }
   }
   else {
      /* curved box */
      v = (float *) allocate_type( ctx, (2*ctx->dpy_ctx->Nr + 2*ctx->dpy_ctx->Nc - 3)
                                   * 3 * sizeof(float),
                                   MHRECT_TYPE );
      if (v) {
         /* north edge */
         for (i=0;i<ctx->dpy_ctx->Nc;i++) {
            v[n++] = 0.0;
            v[n++] = i;
            v[n++] = levelPRIME;
         }
         /* east edge */
         for (i=1;i<ctx->dpy_ctx->Nr;i++) {
            v[n++] = i;
            v[n++] = ctx->dpy_ctx->Nc-1;
            v[n++] = levelPRIME;
         }
         /* south edge */
         for (i=ctx->dpy_ctx->Nc-2;i>=0;i--) {
            v[n++] = ctx->dpy_ctx->Nr-1;
            v[n++] = i;
            v[n++] = levelPRIME;
         }
         /* west edge */
         for (i=ctx->dpy_ctx->Nr-2;i>=0;i--) {
            v[n++] = i;
            v[n++] = 0;
            v[n++] = levelPRIME;
         }
         n /= 3;
         assert( n == 2*ctx->dpy_ctx->Nr + 2*ctx->dpy_ctx->Nc - 3 );
      }
   }

   /* convert vertices from grid to graphics coords */
   for (i=0;i<n;i++) {
      float r = v[i*3+0];
      float c = v[i*3+1];
      float l = v[i*3+2];
      gridPRIME_to_xyzPRIME( ctx->dpy_ctx, time, var, 1, &r, &c, &l,
                   &v[i*3+0], &v[i*3+1], &v[i*3+2] );
   }

   *boxverts = v;
   return n;
}




static int make_vertical_rectangle( Context ctx, int time, int var,
                                    int curved,
                                    float r1, float c1, float r2, float c2,
                                    int segs, float **boxverts )
{
   int i, n;
   float *v;

   n = 0;

   if (curved==0) {
      v = (float *) allocate_type( ctx, 5 * 3 * sizeof(float), MVRECT_TYPE );
      if (v) {
         n = 5;
         v[0*3+0] = r1;
         v[0*3+1] = c1;
         v[0*3+2] = ctx->dpy_ctx->LowLev;
         v[1*3+0] = r1;
         v[1*3+1] = c1;
         v[1*3+2] = ctx->dpy_ctx->Nl-1 + ctx->dpy_ctx->LowLev;
         v[2*3+0] = r2;
         v[2*3+1] = c2;
         v[2*3+2] = ctx->dpy_ctx->Nl-1 + ctx->dpy_ctx->LowLev;
         v[3*3+0] = r2;
         v[3*3+1] = c2;
         v[3*3+2] = ctx->dpy_ctx->LowLev; 
         v[4*3+0] = r1;
         v[4*3+1] = c1;
         v[4*3+2] = ctx->dpy_ctx->LowLev; 
/*
         v[0*3+0] = r1;
         v[0*3+1] = c1;
         v[0*3+2] = gridlevel_to_gridlevelPRIME(ctx, ctx->LowLev[var]);
         v[1*3+0] = r1;
         v[1*3+1] = c1;
         v[1*3+2] = gridlevel_to_gridlevelPRIME(ctx, ctx->Nl[var])  +
                    gridlevel_to_gridlevelPRIME(ctx, ctx->LowLev[var]);
         v[2*3+0] = r2;
         v[2*3+1] = c2;
         v[2*3+2] = gridlevel_to_gridlevelPRIME(ctx, ctx->Nl[var])  +
                    gridlevel_to_gridlevelPRIME(ctx, ctx->LowLev[var]);
         v[3*3+0] = r2;
         v[3*3+1] = c2;
         v[3*3+2] = gridlevel_to_gridlevelPRIME(ctx, ctx->LowLev[var]); 
         v[4*3+0] = r1;
         v[4*3+1] = c1;
         v[4*3+2] = gridlevel_to_gridlevelPRIME(ctx, ctx->LowLev[var]); 
*/
      }
   }
   else {
      /* curved box */
      v = (float *) allocate_type( ctx, (2*ctx->dpy_ctx->Nl + 2*segs - 3)
                                   * 3 * sizeof(float),
                                   MVRECT_TYPE );
      if (v) {
         float r, c, dr, dc;
         dr = (r2-r1) / (float) (segs-1);
         dc = (c2-c1) / (float) (segs-1);
         /* top */
         r = r1;
         c = c1;
         for (i=0;i<segs;i++) {
            v[n++] = r;
            v[n++] = c;
            v[n++] = ctx->dpy_ctx->Nl-1 + ctx->dpy_ctx->LowLev;
            r += dr;
            c += dc;
         }
         /* right */
         for (i=ctx->dpy_ctx->Nl-2;i>=0;i--) {
            v[n++] = r2;
            v[n++] = c2;
            v[n++] = i + ctx->dpy_ctx->LowLev;
         }
         /* bottom */
         r = r2-dr;
         c = c2-dc;
         for (i=segs-2;i>=0;i--) {
            v[n++] = r;
            v[n++] = c;
            v[n++] = ctx->dpy_ctx->LowLev;
            r -= dr;
            c -= dc;
         }
         /* left */
         for (i=1;i<ctx->dpy_ctx->Nl;i++) {
            v[n++] = r1;
            v[n++] = c1;
            v[n++] = i + ctx->dpy_ctx->LowLev;
         }
         n /= 3;
         assert( n == 2*ctx->dpy_ctx->Nl + 2*segs - 3 );
      }
   }

   /* convert vertices from grid to graphics coords */
   for (i=0;i<n;i++) {
      float r = v[i*3+0];
      float c = v[i*3+1];
      float l = v[i*3+2];
      gridPRIME_to_xyzPRIME( ctx->dpy_ctx, time, var, 1, &r, &c, &l,
                   &v[i*3+0], &v[i*3+1], &v[i*3+2] );
   }

   *boxverts = v;
   return n;
}


static void calc_textplot( Irregular_Context itx, int time, int threadnum )
{
   Display_Context dtx;
   struct textplot *tp = &itx->TextPlotTable[time];
   int_2 verts;
   int numverts;
   int bytes, i, j, k;
   int_2 *cverts;
   float *lat, *lon, *alt;
   float *xs, *ys, *zs;
   float *vx, *vy, *vz;
   double *numdata = NULL;
   char *chardata = NULL;
   int numv;
   uint_1 *tempcols;
   uint_1 *colors;
   int ploton[MAXRECS];
   int numtouse;
 
   dtx = itx->dpy_ctx;

   /***************************/
   /* malloc temporary memory */
   /***************************/
   lat = (float *) malloc(sizeof(float)*itx->NumRecs[time]);
   lon = (float *) malloc(sizeof(float)*itx->NumRecs[time]);
   alt = (float *) malloc(sizeof(float)*itx->NumRecs[time]);

   xs= (float *) malloc(sizeof(float)*itx->NumRecs[time]);
   ys= (float *) malloc(sizeof(float)*itx->NumRecs[time]);
   zs= (float *) malloc(sizeof(float)*itx->NumRecs[time]);

   vx = (float *) malloc(sizeof(float)*MAX_TEXT_PLOT_VERTS);
   vy = (float *) malloc(sizeof(float)*MAX_TEXT_PLOT_VERTS);
   vz = (float *) malloc(sizeof(float)*MAX_TEXT_PLOT_VERTS);

   tempcols = NULL;
   if (itx->TextPlotColorStatus[itx->TextPlotVar] == VIS5D_ON){
      tempcols = (uint_1 *) malloc(sizeof(uint_1)*MAX_TEXT_PLOT_VERTS);
   }

   /**********************************************/   
   /* Check to make sure there was enough memory */
   /**********************************************/
   if (!lat || !lon || !alt ||
       !xs  || !ys  || !zs  ||
       !vx  || !vy  || !vz ){
      printf("not enough memory in calc_textpot\n");
      exit(0);
   }
   if (itx->TextPlotColorStatus[itx->TextPlotVar] == VIS5D_ON &&
       !tempcols){
       printf("nnot enough memory in calc_textpot\n");
      exit(0);
   }

   /*********************/
   /* get location data */
   /*********************/
   get_record_locations(itx, time, lat, lon, alt);


   
   /************************************************/
   /* convert geo location data to xyz coordinates */
   /************************************************/
   geo_to_xyzPRIME( dtx, 0, 0, itx->NumRecs[time],
                     lat, lon, alt, xs, ys, zs);

   space_plots(itx, time, ploton, xs, ys, zs, &numtouse);

   if (itx->VarType[itx->TextPlotVar] == NUMERICAL_VAR_1D){
      numdata = (double *) malloc(sizeof(double)*numtouse);
   }
   else if (itx->VarType[itx->TextPlotVar] == CHARACTER_VAR){
      chardata = (char *) malloc(sizeof(char)*numtouse*
                          itx->CharVarLength[itx->TextPlotVar]);
   }
   else{
      printf("Error in creating textplot\n");
   }

   /********************************/
   /* get the record num/char data */
   /********************************/
   if (itx->VarType[itx->TextPlotVar] == NUMERICAL_VAR_1D){
      get_some_record_numerical_data( itx, time, itx->TextPlotVar, ploton, numdata);
   }
   else if (itx->VarType[itx->TextPlotVar] == CHARACTER_VAR){
      get_some_record_char_data( itx, time, itx->TextPlotVar, ploton, chardata);
   }
   else{
      printf("Error in creating textplot\n");
   }

   /**************************************/
   /* create the textplots in xyz coords */
   /**************************************/
   if (itx->VarType[itx->TextPlotVar] == NUMERICAL_VAR_1D){
      if (itx->TextPlotColorStatus[itx->TextPlotVar] == VIS5D_ON){
         create_color_num_textplot( itx, time, xs, ys, zs, numdata, ploton,
                                    vx, vy, vz, &numv, tempcols);
      }
      else{
         create_num_textplot( itx, time, xs, ys, zs, numdata, ploton, vx, vy, vz, &numv);
      }
   }
   else if (itx->VarType[itx->TextPlotVar] == CHARACTER_VAR){
      create_letter_textplot( itx, time, xs, ys, zs, chardata, ploton, itx->TextPlotVar,
                              vx, vy, vz, &numv);
   }
   else{
      printf("Error in creating textplot\n");
   }

   /**********************************************/
   /* get vis5d memory to store verts and colors */
   /**********************************************/
   if (numv){
      int c;
      bytes = 3*numv*sizeof(int_2);
      cverts = (int_2 *) i_allocate_type( itx, bytes, CVX1H_TYPE );
      if (itx->TextPlotColorStatus[itx->TextPlotVar] == VIS5D_ON){
         colors = (uint_1 *) i_allocate(itx, numv/2*sizeof(uint_1) );
         for (c = 0; c < numv/2; c++){
            colors[c] = tempcols[c];
         }
      }
      xyz_to_compXYZ(dtx, numv, vx, vy, vz, (void*)cverts);
   }



   /*******************************************/
   /* store the new textplot verts and colors */
   /*******************************************/
   wait_write_lock( &tp->lock);

   /* deallocate existing texplot, if any */
   free_textplot( itx, time);

   /* add textplot to table */
   tp->numverts = numv;
   tp->verts = cverts;
   tp->valid = 1;
   tp->spacing = itx->TextPlotSpacing;
   tp->fontx = itx->TextPlotFontX;
   tp->fonty = itx->TextPlotFontY;
   tp->fontspace = itx->TextPlotFontSpace;
   if (itx->TextPlotColorStatus[itx->TextPlotVar] == VIS5D_ON){
      tp->colors = colors;
   }
   else{
      tp->colors = NULL;
   }

   done_write_lock( &tp->lock);



   /*************************/   
   /* free temporary memory */
   /*************************/
   free(lat);
   free(lon);
   free(alt);
   free(xs);
   free(ys);
   free(zs);
   free(vx);
   free(vy);
   free(vz);
   if (numdata){
      free(numdata);
      numdata = NULL;
   }
   if (chardata){
      free(chardata);
      chardata = NULL;
   }
   if (tempcols){
      free(tempcols);
      tempcols = NULL;
   }

   if (time==itx->dpy_ctx->CurTime) {
      itx->dpy_ctx->Redraw = 1;
   }
}


/*** calc_hslice ******************************************************
   Calculate a horizontal contour line slice and store it.
   Input:  time - the time step.
           var - which variable.
           interval - interval between lines.
           low, high - range of values to contour.
           levelPRIME - position of slice in [0..dtx->Nl-1].
           threadnum - thread ID
   Output:  resulting poly-triangle strip is saved in HSliceTable.
**********************************************************************/
static void calc_hslice( Context ctx, int time, int var,
                         float interval, float low, float high, float levelPRIME,
                         int threadnum )
{
   struct hslice *slice = &ctx->HSliceTable[var][time];
   float *vr1, *vc1, *vr2, *vc2, *vr3, *vc3, *vl;
   float *grid, *slicedata;
   int num1, num2, num3, maxnum, bytes, i;
   float base;
   int_2 *cverts1, *cverts2, *cverts3;
   float *boxverts;
   int numboxverts;
   Display_Context dtx;
   int contour_ok;
   int max_cont_verts;

   dtx = ctx->dpy_ctx;
   /* MJK 12.04.98 */
   if ((ctx->Nl[var]==1) && (!ctx->DisplaySfcHSlice[var])) {
      wait_write_lock( &slice->lock );
      if (slice->valid && !ctx->dpy_ctx->CurvedBox && slice->interval==interval
          && slice->lowlimit==low && slice->highlimit==high) {
         /* special case: just translate existing slice! */
         float z = gridlevelPRIME_to_zPRIME( dtx, time, var, levelPRIME );
         int_2 compz = (int_2) (z * VERTEX_SCALE);
         int_2 *zptr;
         int n;
         /* translate verts1 */
         n = slice->num1;
         zptr = slice->verts1 + 2;
         for (i=0;i<n;i++) {
            *zptr = compz;
            zptr += 3;
         }
         /* translate verts2 */
         n = slice->num2;
         zptr = slice->verts2 + 2;
         for (i=0;i<n;i++) {
            *zptr = compz;
            zptr += 3;
         }
         /* translate verts3 */
         n = slice->num3;
         zptr = slice->verts3 + 2;
         for (i=0;i<n;i++) {
            *zptr = compz;
            zptr += 3;
         }
         /* update the bounding rectangle */
         numboxverts = make_horizontal_rectangle( ctx, time, var,
                                                  ctx->dpy_ctx->CurvedBox,
                                                  levelPRIME, &boxverts );
         slice->numboxverts = numboxverts;
         slice->boxverts = boxverts;
         slice->level = levelPRIME;
         recent( ctx, HSLICE, var );
         done_write_lock( &slice->lock );
         return;
      }
      done_write_lock( &slice->lock );
   }



   /* get the 3-D grid */
   grid = get_grid( ctx, time, var );
   if (!grid)
      return;

   /* extract the 2-D slice from the 3-D grid */
   /* MJK 12.04.98 */
   if (ctx->DisplaySfcHSlice[var]){
      slicedata = extract_sfc_slice (ctx, time, var, dtx->Nr, dtx->Nc, grid, 1);
   }
   else if (ctx->GridSameAsGridPRIME){
      slicedata = extract_hslice( ctx, grid, var, dtx->Nr, dtx->Nc, dtx->Nl,
                               dtx->LowLev, levelPRIME, 1 );
   }
   else{
      slicedata = extract_hslicePRIME( ctx, grid, time, var, dtx->Nr, dtx->Nc, dtx->Nl,
                               dtx->LowLev, levelPRIME, 1 );
   }
   
   if (!slicedata)
      return;

   /* compute an upper bound on the number of vertices that contour() 
      can return: */
   max_cont_verts = 4 * (dtx->Nr-1) * (dtx->Nc-1)
       * fabs((high-low)/interval) + .5;
   if (max_cont_verts > MAX_CONT_VERTS)
       max_cont_verts = MAX_CONT_VERTS;

   vr1 = (float *) malloc(sizeof(float)*max_cont_verts);
   vc1 = (float *) malloc(sizeof(float)*max_cont_verts);
   vr2 = (float *) malloc(sizeof(float)*max_cont_verts/2);
   vc2 = (float *) malloc(sizeof(float)*max_cont_verts/2);
   vr3 = (float *) malloc(sizeof(float)*max_cont_verts/2);
   vc3 = (float *) malloc(sizeof(float)*max_cont_verts/2);
   vl  = (float *) malloc(sizeof(float)*max_cont_verts);

   if (!vr1 || !vc1 || !vr2 || !vc2 || !vr3 || !vc3 || !vl){
      printf(" You do not have enough memory to create hslices.\n");
      if (vr1){
         free(vr1);
      }
      if (vc1){
         free(vc1);
      }
      if (vr2){
         free(vr2);
      }
      if (vc2){
         free(vc2);
      }
      if (vc3){
         free(vc3);
      }
      if (vl){
         free(vl);
      }
      if (vr3){
         free(vr3);
      }
      deallocate( ctx, slicedata, -1 );
      release_grid( ctx, time, var, grid );
      return;
   }
 
   if (low==ctx->MinVal[var])
     base = 0.0;
   else
     base = low;

   /* call contouring routine */
   contour_ok =
     contour( ctx, slicedata, dtx->Nr, dtx->Nc, interval, low, high, base,
             vr1, vc1, max_cont_verts, &num1,
             vr2, vc2, max_cont_verts/2, &num2,
             vr3, vc3, max_cont_verts/2, &num3);

   /* done with grid and slice */
   deallocate( ctx, slicedata, -1 );
   release_grid( ctx, time, var, grid );

   if (!contour_ok) {
     free(vr1);free(vc1);free(vr2);free(vc2);free(vr3);free(vc3);free(vl);
     return;
   }

   /* generate level coordinates array */
   if (num1>num2 && num1>num3) {
      maxnum = num1;
   }
   else if (num2>num1 && num2>num3) {
      maxnum = num2;
   }
   else {
      maxnum = num3;
   }
   for (i=0;i<maxnum;i++) {
      vl[i] = levelPRIME;
   }

   /*
    * Transform vertices from grid coordinates in [0,Nr][0,Nc][0,Nl] to
    * compressed graphics coordinates in [-10000,10000]^3.
    */


   /* MJK 12.04.98 */
   if (ctx->DisplaySfcHSlice[var]){
      num1 = fit_vecs_to_topo (ctx, num1, max_cont_verts, vr1, vc1, vl);
   }


   if (num1) {
      bytes = 3*num1*sizeof(int_2);
      cverts1 = (int_2 *) allocate_type( ctx, bytes, CVX1H_TYPE );
      if (cverts1) {
         gridPRIME_to_compXYZPRIME( dtx, time, var, num1, vr1, vc1, vl, (void*)cverts1 );
      }
      else {
         num1 = 0;
      }
   }
   else {
      cverts1 = NULL;
   }

   /* MJK 12.04.98 */
   if (ctx->DisplaySfcHSlice[var]){
      num2 = fit_vecs_to_topo (ctx, num2, max_cont_verts/2, vr2, vc2, vl);
   }


   if (num2) {
      bytes = 3*num2*sizeof(int_2);
      cverts2 = (int_2 *) allocate_type( ctx, bytes, CVX2H_TYPE );
      if (cverts2) {
         gridPRIME_to_compXYZPRIME( dtx, time, var, num2, vr2, vc2, vl, (void*)cverts2 );
      }
      else {
         num2 = 0;
      }
   }
   else {
      cverts2 = NULL;
   }

   /* MJK 12.04.98 */
   if (ctx->DisplaySfcHSlice[var]){
      num3 = fit_vecs_to_topo (ctx, num3, max_cont_verts/2, vr3, vc3, vl);
   }




   if (num3) {
      bytes = 3*num3*sizeof(int_2);
      cverts3 = (int_2 *) allocate_type( ctx, bytes, CVX3H_TYPE );
      if (cverts3) {
         gridPRIME_to_compXYZPRIME( dtx, time, var, num3, vr3, vc3, vl, (void*)cverts3 );
      }
      else {
         num3 = 0;
      }
   }
   else {
      cverts3 = NULL;
   }

   /*
    * Generate bounding rectangle.
    */
   numboxverts = make_horizontal_rectangle( ctx, time, var, ctx->dpy_ctx->CurvedBox,
                                            levelPRIME, &boxverts );


   /************************ Store the new slice ************************/

   recent( ctx, HSLICE, var );

   wait_write_lock( &slice->lock );

   /* deallocate existing slice, if any */
   free_hslice( ctx, time, var );

   /* store new slice */
   slice->interval = interval;
   slice->lowlimit = low;
   slice->highlimit = high;
   slice->level = levelPRIME;
   slice->num1 = num1;
   slice->verts1 = cverts1;
   slice->num2 = num2;
   slice->verts2 = cverts2;
   slice->num3 = num3;
   slice->verts3 = cverts3;
   slice->boxverts = boxverts;
   slice->numboxverts = numboxverts;
   slice->valid = 1;

   done_write_lock( &slice->lock );

   if (time==ctx->dpy_ctx->CurTime) {
      ctx->dpy_ctx->Redraw = 1;
   }
   free(vr1);
   free(vc1);
   free(vr2);
   free(vc2);
   free(vr3);
   free(vc3);
   free(vl);
}




/*** calc_vslice ******************************************************
   Calculate a vertical contour line slice and store it.
   Input:  time - the time step.
           var - which variable.
           interval - interval between lines.
           low, high - range of values to contour
           r1, c1 - position of 1st corner in [0..ctx->Nr-1],[0..ctx->Nc-1]
           r2, c2 - position of 1st corner in [0..ctx->Nr-1],[0..ctx->Nc-1]
           threadnum - thread ID
   Output:  resulting poly-triangle strip is saved in VSliceTable.
**********************************************************************/
static void calc_vslice( Context ctx, int time, int var,
                         float interval, float low, float high,
                         float r1, float c1, float r2, float c2,
                         int threadnum )
{
   float *vr1, *vc1, *vl1, *vr2, *vc2, *vl2, *vr3, *vc3, *vl3;
   float *grid;
   float *slice;
   int cols, rows;
   int yo;
   int i;
   int num1, num2, num3, bytes;
   float dr, dc, r, base;
   int_2 *cverts1, *cverts2, *cverts3;
   float *boxverts;
   int numboxverts;
   Display_Context dtx;
   int ctxnl, ctxll;
   int contour_ok;
   int max_cont_verts;

   /* WLH 15 Oct 98 */
   float ctxlow;

   dtx = ctx->dpy_ctx;
   /* get the 3-D grid */
   grid = get_grid( ctx, time, var );
   if (!grid)
      return;

   /* extract the 2-D slice from the 3-D grid */
   rows = dtx->Nl;
   cols = (dtx->Nr>dtx->Nc) ? dtx->Nr : dtx->Nc;  /* kludge */
   if (ctx->GridSameAsGridPRIME){

      /* WLH 15 Oct 98 */
      rows = ctx->Nl[var];

      slice = extract_vslice( ctx, grid, r1,c1,r2,c2, rows, cols, 1 );
   }
   else{
      slice = extract_vslicePRIME( ctx, grid, time, var,
                                   r1,c1,r2,c2, rows, cols, 1 );
   }
   
   if (!slice)
      return;

   /* compute an upper bound on the number of vertices that contour() 
      can return: */
   max_cont_verts = 4 * (rows-1) * (cols-1) * fabs((high-low)/interval) + .5;
   if (max_cont_verts > MAX_CONT_VERTS)
       max_cont_verts = MAX_CONT_VERTS;

   vr1 = (float *) malloc(sizeof(float)*max_cont_verts);
   vc1 = (float *) malloc(sizeof(float)*max_cont_verts);
   vl1 = (float *) malloc(sizeof(float)*max_cont_verts);
   vr2 = (float *) malloc(sizeof(float)*max_cont_verts/2);
   vc2 = (float *) malloc(sizeof(float)*max_cont_verts/2);
   vl2 = (float *) malloc(sizeof(float)*max_cont_verts/2);
   vr3 = (float *) malloc(sizeof(float)*max_cont_verts/2);
   vc3 = (float *) malloc(sizeof(float)*max_cont_verts/2);
   vl3 = (float *) malloc(sizeof(float)*max_cont_verts/2);
   if (!vr1 || !vc1 || !vl1 || !vr2 || !vc2 || !vl2 || !vr3 || !vc3 || !vl3){
      printf(" You do not have enough memory to create vslices.\n");
      if (vr1){
         free(vr1);
      }
      if (vc1){
         free(vc1);
      }
      if (vl1){
         free(vl1);
      }
      if (vr2){
         free(vr2);
      }
      if (vc2){
         free(vr3);
      }
      if (vl2){
         free(vl2);
      }
      if (vc3){
         free(vc3);
      }
      if (vl3){
         free(vl3);
      }
      if (vr3){
         free(vr3);
      }
      deallocate( ctx, slice, -1 );
      release_grid( ctx, time, var, grid );
      return;
   }


   if (low==ctx->MinVal[var])
     base = 0.0;
   else
     base = low;
   /* call contouring routine */
   contour_ok =
       contour( ctx, slice, rows, cols, interval, low, high, base,
                vr1, vc1, max_cont_verts, &num1,
                vr2, vc2, max_cont_verts/2, &num2,
                vr3, vc3, max_cont_verts/2, &num3);

   deallocate( ctx, slice, -1 );
   release_grid( ctx, time, var, grid );

   if (!contour_ok) {
       free(vr1); free(vc1); free(vr2); free(vc2); free(vr3); free(vc3);
       free(vl1); free(vl2); free(vl3);
       return;
   }

   /*
    * Convert 2-D coordinates from [0,rows-1][0,cols-1] to 3-D coords
    * in [0,rows-1][0,cols-1][0,Nl-1].
    */

   dr = r2-r1;
   dc = c2-c1;
   ctxnl = gridlevel_to_gridlevelPRIME( ctx, ctx->Nl[var]);
   ctxll = gridlevel_to_gridlevelPRIME( ctx, ctx->LowLev[var]);

   /* WLH 15 Oct 98 */
   if (ctx->GridSameAsGridPRIME){
     ctxlow = ctx->LowLev[var];
   }
   else {
     ctxlow = dtx->LowLev;
   }

   for (i=0;i<num1;i++) {
      r = vc1[i] / (float) (cols-1);   /* r in [0,1] */

/* WLH 15 Oct 98
      vl1[i] = (float) (dtx->Nl-1+dtx->LowLev) - vr1[i];
*/
      /* WLH 15 Oct 98 */
      vl1[i] = (rows - 1 + ctxlow) - vr1[i];

      vr1[i] = r1 + r * dr;
      vc1[i] = c1 + r * dc;
   }
   for (i=0;i<num2;i++) {
      r = vc2[i] / (float) (cols-1);   /* r in [0,1] */

/* WLH 15 Oct 98
      vl2[i] = (float) (dtx->Nl-1+dtx->LowLev) - vr2[i];
*/
      /* WLH 15 Oct 98 */
      vl2[i] = (rows - 1 + ctxlow) - vr2[i];

      vr2[i] = r1 + r * dr;
      vc2[i] = c1 + r * dc;
   }
   for (i=0;i<num3;i++) {
      r = vc3[i] / (float) (cols-1);   /* r in [0,1] */

/* WLH 15 Oct 98
      vl3[i] = (float) (dtx->Nl-1+dtx->LowLev) - vr3[i];
*/
      /* WLH 15 Oct 98 */
      vl3[i] = (rows - 1 + ctxlow) - vr3[i];

      vr3[i] = r1 + r * dr;
      vc3[i] = c1 + r * dc;
   }

   recent( ctx, VSLICE, var );

   if (num1) {
      bytes = 3*num1*sizeof(int_2);
      cverts1 = (int_2 *) allocate_type( ctx, bytes, CVX1V_TYPE );
      if (cverts1) {
         gridPRIME_to_compXYZPRIME( dtx, time, var, num1, vr1, vc1, vl1,
                          (void*) cverts1 );
      }
      else {
         num1 = 0;
      }
   }
   else {
      cverts1 = NULL;
   }

   if (num2) {
      bytes = 3*num2*sizeof(int_2);
      cverts2 = (int_2 *) allocate_type( ctx, bytes, CVX2V_TYPE );
      if (cverts2) {
         gridPRIME_to_compXYZPRIME( dtx, time, var, num2, vr2, vc2, vl2,
                          (void*) cverts2 );
      }
      else {
         num2 = 0;
      }
   }
   else {
      cverts2 = NULL;
   }

   if (num3) {
      bytes = 3*num3*sizeof(int_2);
      cverts3 = (int_2 *) allocate_type( ctx, bytes, CVZ3V_TYPE );
      if (cverts3) {
         gridPRIME_to_compXYZPRIME( dtx, time, var, num3, vr3, vc3, vl3,
                          (void*) cverts3 );
      }
      else {
         num3 = 0;
      }
   }
   else {
      cverts3 = NULL;
   }

   /*
    * Generate bounding rectangle.
    */
   numboxverts = make_vertical_rectangle( ctx, time, var, ctx->dpy_ctx->CurvedBox,
                                          r1, c1, r2, c2, cols, &boxverts );


   /************************ Store the new slice ************************/

   wait_write_lock( &ctx->VSliceTable[var][time].lock );

   /* deallocate existing slice, if any */
   free_vslice( ctx, time, var );

   /* store new slice */
   ctx->VSliceTable[var][time].interval = interval;
   ctx->VSliceTable[var][time].lowlimit = low;
   ctx->VSliceTable[var][time].highlimit = high;
   ctx->VSliceTable[var][time].r1 = r1;
   ctx->VSliceTable[var][time].c1 = c1;
   ctx->VSliceTable[var][time].r2 = r2;
   ctx->VSliceTable[var][time].c2 = c2;
   ctx->VSliceTable[var][time].num1 = num1;
   ctx->VSliceTable[var][time].verts1 = cverts1;
   ctx->VSliceTable[var][time].num2 = num2;
   ctx->VSliceTable[var][time].verts2 = cverts2;
   ctx->VSliceTable[var][time].num3 = num3;
   ctx->VSliceTable[var][time].verts3 = cverts3;
   ctx->VSliceTable[var][time].boxverts = boxverts;
   ctx->VSliceTable[var][time].numboxverts = numboxverts;
   ctx->VSliceTable[var][time].valid = 1;

   done_write_lock( &ctx->VSliceTable[var][time].lock );

   if (time==ctx->dpy_ctx->CurTime) {
      ctx->dpy_ctx->Redraw = 1;
   }
   free(vr1);
   free(vc1);
   free(vl1);
   free(vr2);
   free(vc2);
   free(vl2);
   free(vr3);
   free(vc3);
   free(vl3);
}




/*** calc_chslice *****************************************************
   Calculate a horizontal colored slice and store it.
   Input:  time - the time step.
           var - which variable.
           level - position of slice in [0..Nl-1].
           threadnum - thread ID
   Output:  resulting data values are saved in CHSliceTable.
**********************************************************************/
static void calc_chslice( Context ctx, int time, int var,
                          float level, int threadnum )
{
   struct chslice *slice = &ctx->CHSliceTable[var][time];
   float *vr, *vc, *vl;
   int_2 *cverts;
   float *grid, *slicedata, scale;
   uint_1 *indexes;
   int vbytes, ibytes;
   int slice_rows, slice_cols;
   float density = 1.0;  /* Make this a parameter someday */
   Display_Context dtx;

   dtx = ctx->dpy_ctx;
   if (ctx->Nl[var]==1) {
      wait_write_lock( &slice->lock );
      if (slice->valid && !ctx->dpy_ctx->CurvedBox) {
         /* special case: just translate existing slice! */
         float z = gridlevelPRIME_to_zPRIME( dtx, time, var, level );
         int_2 compz = (int_2) (z * VERTEX_SCALE);
         int nrnc = dtx->Nr * dtx->Nc;
         int_2 *zptr = slice->verts + 2;
         int i;
         for (i=0;i<nrnc;i++) {
            *zptr = compz;
            zptr += 3;
         }
         slice->level = level;
         recent( ctx, CHSLICE, var );
         done_write_lock( &slice->lock );
         return;
      }
      done_write_lock( &slice->lock );
   }


   /* get the 3-D grid */
   grid = get_grid( ctx, time, var );
   if (!grid)
      return;

   /* extract the 2-D array from 3-D grid */
   if (ctx->GridSameAsGridPRIME){
      slicedata = extract_hslice( ctx, grid, var, dtx->Nr, dtx->Nc, dtx->Nl,
                               dtx->LowLev, level, 0 );
   }
   else{
      slicedata = extract_hslicePRIME( ctx, grid, time, var, dtx->Nr, dtx->Nc, dtx->Nl,
                               dtx->LowLev, level, 0 );
   }
   if (!slicedata)
      return;

   /* compute size of colored slice */
   slice_rows = dtx->Nr * density;
   slice_cols = dtx->Nc * density;

   /* allocate space for vertices and color indexes */
   vbytes = slice_rows * slice_cols * sizeof(int_2) * 3;
   cverts = (int_2 *) allocate_type( ctx, vbytes, VXH_TYPE );
   ibytes = slice_rows * slice_cols * sizeof(uint_1);
   indexes = (uint_1 *) allocate_type( ctx, ibytes, INDEXESH_TYPE );
   if (!cverts || !indexes) {
      if (cverts) deallocate(ctx, cverts, vbytes );
      if (indexes) deallocate(ctx, indexes, ibytes );
      return;
   }

   vr = (float *) malloc( MAXROWS*MAXCOLUMNS*sizeof(float));
   vc = (float *) malloc( MAXROWS*MAXCOLUMNS*sizeof(float));
   vl = (float *) malloc( MAXROWS*MAXCOLUMNS*sizeof(float));
   if (!vr || !vc || !vl){
      printf(" You do not have enough memory to create chslices.\n");
      if (vr){
         free(vr);
      }
      if (vc){
         free(vc);
      }
      if (vl){
         free(vl);
      }
      release_grid( ctx, time, var, grid );
      deallocate( ctx, slicedata, -1 );
      return;
   }


   /* compute graphics coords vertices */
   if (density==1.0) {
      int i, j, n = 0;
      for (i=0;i<slice_rows;i++) {
         for (j=0;j<slice_cols;j++) {
            vr[n] = (float) i;
            vc[n] = (float) j;
            vl[n] = level;
            n++;
         }
      }
   }
   else {
      float rowscale = (float) (dtx->Nr-1) / (float) (slice_rows-1);
      float colscale = (float) (dtx->Nc-1) / (float) (slice_cols-1);
      int i, j, n = 0;
      for (i=0;i<slice_rows;i++) {
         int src_row = i * rowscale;
         for (j=0;j<slice_cols;j++) {
            int src_col = j * colscale;
            vr[n] = (float) src_row;
            vc[n] = (float) src_col;
            vl[n] = level;
            n++;
         }
      }
   }
   gridPRIME_to_compXYZPRIME( dtx, time, var, slice_rows*slice_cols, vr, vc, vl,
                    (void*) cverts );


   /* scale data values to [0,254] with missing = 255 */
   if (ctx->MinVal[var]==ctx->MaxVal[var])
     scale = 0.0;
   else
     scale = 254.0 / (ctx->MaxVal[var]-ctx->MinVal[var]);
   if (density==1.0) {
      /* simple calculation */
      float minval = ctx->MinVal[var];
      int i;
      for (i=0;i<slice_rows*slice_cols;i++) {
         if (IS_MISSING(slicedata[i]) ||
             slicedata[i] < minval ||
             slicedata[i] > ctx->MaxVal[var])
            indexes[i] = 255;
         else{
            /* MJK 12.04.98 */
            int index = (slicedata[i]-minval) * scale;
            indexes[i] = (index < 0) ? 0 : (index > 254) ? 254 : index;
         }
      }
   }
   else {
      /* resampling needed */
      int row, col;
      float minval = ctx->MinVal[var];
      float rowscale = (float) (dtx->Nr-1) / (float) (slice_rows-1);
      float colscale = (float) (dtx->Nc-1) / (float) (slice_cols-1);
      int i = 0;
      for (row=0; row<slice_rows; row++) {
         int src_row = row * rowscale;
         for (col=0; col<slice_cols; col++) {
            int src_col = col * colscale;
            float val = slicedata[ src_row * dtx->Nc + src_col ];
            if (IS_MISSING(val) ||
                val < ctx->MinVal[var] ||
                val > ctx->MaxVal[var])
              indexes[i] = 255;
            else{
               /* MJK 12.04.98 */
               int index = (val-minval) * scale;
               indexes[i] = (index < 0) ? 0 : (index > 254) ? 254 : index;
            }
            i++;
         }
      }
   }


   /* done with the 3-D grid and 2-D slice */
   release_grid( ctx, time, var, grid );
   deallocate( ctx, slicedata, -1 );


   /**************************** Store ********************************/
   recent( ctx, CHSLICE, var );

   wait_write_lock( &slice->lock );

   /* deallocate existing slice, if any */
   free_chslice( ctx, time, var );

   /* store new slice */
   slice->level = level;
   slice->rows = slice_rows;
   slice->columns = slice_cols;
   slice->verts = cverts;
   slice->color_indexes = indexes;
   slice->valid = 1;

   done_write_lock( &slice->lock );

   if (time==ctx->dpy_ctx->CurTime) {
      ctx->dpy_ctx->Redraw = 1;
   }
   free(vr);
   free(vc);
   free(vl);
}




/*** calc_cvslice ******************************************************
   Calculate a vertical colored slice and store it.
   Input:  time - the time step.
           var - which variable.
           r1, c1 - position of 1st corner in [0..ctx->Nr-1],[0..ctx->Nc-1]
           r2, c2 - position of 1st corner in [0..ctx->Nr-1],[0..ctx->Nc-1]
           threadnum - thread ID
   Output:  resulting poly-triangle strip is saved in CVSliceTable.
**********************************************************************/
static void calc_cvslice( Context ctx, int time, int var,
                          float r1, float c1, float r2, float c2,
                          int threadnum )
{
   float *vr, *vc, *vl;
   float *grid, *slice;
   int_2 *cverts;
   uint_1 *indexes;
   int cols, rows, i, j, n;
   float scale, r, c, dr, dc;
   float mr, mc, ml;
   float x, y, z;
   int yo, vbytes, ibytes;
   Display_Context dtx;

   /* WLH 15 Oct 8 */
   float ctxlow;

   dtx = ctx->dpy_ctx;
   /* size of quadmesh: */
   rows = dtx->Nl;
   cols = MAX(dtx->Nr,dtx->Nc);

   /* get the 3-D grid */
   grid = get_grid( ctx, time, var );
   if (!grid)
      return;

   /* extract the 2-D slice from the 3-D grid */
   if (ctx->GridSameAsGridPRIME){

      /* WLH 15 Oct 98 */
      rows = ctx->Nl[var];

      slice = extract_vslice( ctx, grid, r1,c1,r2,c2, rows, cols, 0 );
   }
   else{
      slice = extract_vslicePRIME( ctx, grid, time, var,
                                   r1,c1,r2,c2, rows, cols, 0 );
   }
   
   /* allocate space for vertices and color indexes */
   vbytes = rows * cols * sizeof(int_2) * 3;
   ibytes = rows*cols*sizeof(uint_1);
   cverts = (int_2 *) allocate_type( ctx, vbytes, VXV_TYPE );
   indexes = (uint_1 *) allocate_type( ctx, ibytes, INDEXESV_TYPE );
   if (!cverts || !indexes) {
      if (cverts) deallocate(ctx, cverts, vbytes );
      if (indexes) deallocate(ctx, indexes, ibytes );
      return;
   }

#if MAXROWS>MAXCOLUMNS
   vr = (float *) malloc( MAXROWS*MAXLEVELS*sizeof(float));
   vc = (float *) malloc( MAXROWS*MAXLEVELS*sizeof(float));
   vl = (float *) malloc( MAXROWS*MAXLEVELS*sizeof(float));
#else
   vr = (float *) malloc( MAXCOLUMNS*MAXLEVELS*sizeof(float));
   vc = (float *) malloc( MAXCOLUMNS*MAXLEVELS*sizeof(float));
   vl = (float *) malloc( MAXCOLUMNS*MAXLEVELS*sizeof(float));
#endif
   if (!vr || !vc || !vl){
      printf(" You do not have enough memory to create cvslices.\n");
      if (vr){
         free(vr);
      }
      if (vc){
         free(vc);
      }
      if (vl){
         free(vl);
      }
      release_grid( ctx, time, var, grid );
      deallocate( ctx, slice, -1 );
      return;
   }

   /* WLH 15 Oct 98 */
   if (ctx->GridSameAsGridPRIME){
     ctxlow = ctx->LowLev[var];
   }
   else {
     ctxlow = dtx->LowLev;
   }

   /* compute the vertices */
   n = 0;
   dr = (r2-r1) / (cols-1);
   dc = (c2-c1) / (cols-1);
   for (i=0;i<rows;i++) {
      r = r1;
      c = c1;
      for (j=0;j<cols;j++) {
         vr[n] = r;
         vc[n] = c;
/* WLH 15 Oct 98
         vl[n] = i+dtx->LowLev;
*/
         /* WLH 15 Oct 98 */
         vl[n] = i + ctxlow;

         r += dr;
         c += dc;
         n++;
      }
   }
   gridPRIME_to_compXYZPRIME( dtx, time, var, rows*cols, vr, vc, vl, (void*) cverts );

   /* scale grid values to [0,254] with missing = 255 */
   if (ctx->MinVal[var]==ctx->MaxVal[var])
      scale = 0.0;
   else
      scale = 254.0 / (ctx->MaxVal[var]-ctx->MinVal[var]);
   for (i=0;i<rows*cols;i++) {
      if (IS_MISSING(slice[i]) ||
          slice[i] < ctx->MinVal[var] ||
          slice[i] > ctx->MaxVal[var])
         indexes[i] = 255;
      else{
         /* MJK 12.04.98 */
         int index = (slice[i]-ctx->MinVal[var]) * scale;
         indexes[i] = (index < 0) ? 0 : (index > 254) ? 254 : index;
      }
   }


   /* done with the 3-D grid and 2-D slice */
   release_grid( ctx, time, var, grid );
   deallocate( ctx, slice, -1 );

   /* make the tick mark at midpoint of top edge */
   mr = (r1+r2)/2.0;
   mc = (c1+c2)/2.0;
   ml = (float) (dtx->Nl-1+dtx->LowLev);
   gridPRIME_to_xyzPRIME( dtx, time, var, 1, &mr, &mc, &ml, &x, &y, &z );

   ctx->CVSliceTable[var][time].mark[0][0] = x;
   ctx->CVSliceTable[var][time].mark[0][1] = y;
   ctx->CVSliceTable[var][time].mark[0][2] = z+0.02;
   ctx->CVSliceTable[var][time].mark[1][0] = x;
   ctx->CVSliceTable[var][time].mark[1][1] = y;
   ctx->CVSliceTable[var][time].mark[1][2] = z-0.02;


   /************************* Store **********************************/
   recent( ctx, CVSLICE, var );

   wait_write_lock( &ctx->CVSliceTable[var][time].lock );

   /* deallocate existing slice, if any */
   free_cvslice( ctx, time, var );

   /* store new slice */
   ctx->CVSliceTable[var][time].r1 = r1;
   ctx->CVSliceTable[var][time].c1 = c1;
   ctx->CVSliceTable[var][time].r2 = r2;
   ctx->CVSliceTable[var][time].c2 = c2;
   ctx->CVSliceTable[var][time].rows = rows;
   ctx->CVSliceTable[var][time].columns = cols;
   ctx->CVSliceTable[var][time].color_indexes = indexes;
   ctx->CVSliceTable[var][time].verts = cverts;
   ctx->CVSliceTable[var][time].valid = 1;

   done_write_lock( &ctx->CVSliceTable[var][time].lock );

   if (time==ctx->dpy_ctx->CurTime) {
      ctx->dpy_ctx->Redraw = 1;
   }
   free(vr);
   free(vc);
   free(vl);
}



/*
 * Macros for calc_hwindslice and calc_vwindslice:
 */

#define CROSS( c, a, b )  { c[0] =  a[1]*b[2]-a[2]*b[1]; \
                            c[1] = -a[0]*b[2]+a[2]*b[0]; \
                            c[2] =  a[0]*b[1]-a[1]*b[0]; \
                          }

#define MAGNITUDE( a )    sqrt( a[0]*a[0] + a[1]*a[1] + a[2]*a[2] )

#define NORMALIZE( v ) { float mag = 1.0/sqrt(v[0]*v[0]+v[1]*v[1]+v[2]*v[2]); \
                         v[0] *= mag; \
                         v[1] *= mag; \
                         v[2] *= mag; \
                       }

#define SCALE_VEC( v, d )  {  v[0] = v[0] / (d); \
                              v[1] = v[1] / (d); \
                              v[2] = v[2] / (d); \
                           }


#define BARB_INC 6.0

/* MJK 12.04.98 begin */
static int get_cross_vec (float *res, float *dir, float *up)
{

    float       fudge[3];


    CROSS (res, dir, up);

    if (MAGNITUDE (res) != 0.0) return 1;

/*
 *  We get to this point if the wind vector is perpendicular to the slice
 *  plane.  Although not common, this _can_ happen -- especially if one of
 *  the wind components (probably W) is missing.  This hunk of code is a
 *  bit of a kludge.
 */

    if (dir[0] != 0.0)
    {
        fudge[0] = dir[0] * 0.99999;
        fudge[1] = sqrt ((dir[0] * dir[0]) - (fudge[0] * fudge[0]));
        fudge[2] = 0.0;
    }
    else if (dir[1] != 0.0)
    {
        fudge[1] = dir[1] * 0.99999;
        fudge[0] = sqrt ((dir[1] * dir[1]) - (fudge[1] * fudge[1]));
        fudge[2] = 0.0;
    }
    else
    {
        fudge[2] = dir[2] * 0.99999;
        fudge[1] = sqrt ((dir[2] * dir[2]) - (fudge[2] * fudge[2]));
        fudge[0] = 0.0;
    }

    CROSS (res, fudge, up);


    return 0;
}
/* MJK 12.04.98 end */

/* draw line disjoint segments for wind barb */
static void make_barb(Display_Context dtx, float u, float v, float w, float *dir,
                      float *up, float row, float col, float level,
                      float *vr, float *vc, float *vl, int drow, int *vco)
{
  float a[3];
  float kts, len, alen, size;
  float tr, tc, tl;
  float lat, lon, midrow, midcol, sign;
  int i, ikts, ntri, nlong, nshort;
  int vcount;

  vcount = *vco;
  size = drow;
  kts = sqrt (u * u + v * v + w * w) * (3600.0 / 1853.248);

  sign = -1.0;
  if (dtx->Projection != PROJ_GENERIC) {
    midrow = (float) dtx->Nr / 2.0;
    midcol = (float) dtx->Nc / 2.0;
    rowcolPRIME_to_latlon( dtx, -1, -1, midrow, midcol, &lat, &lon );
    sign = (lat >= 0.0) ? -1.0 : 1.0;
  }

  if (kts < 1.0) {
    /* make a cross for calm */

    vr[vcount] = row + size / BARB_INC;
    vc[vcount] = col;
    vl[vcount] = level;
    vcount++;

    vr[vcount] = row - size / BARB_INC;
    vc[vcount] = col;
    vl[vcount] = level;
    vcount++;

    vr[vcount] = row;
    vc[vcount] = col + size / BARB_INC;
    vl[vcount] = level;
    vcount++;

    vr[vcount] = row;
    vc[vcount] = col - size / BARB_INC;
    vl[vcount] = level;
    vcount++;

    vr[vcount] = row;
    vc[vcount] = col;
    vl[vcount] = level + size / BARB_INC;
    vcount++;

    vr[vcount] = row;
    vc[vcount] = col;
    vl[vcount] = level - size / BARB_INC;
    vcount++;

  }
  else {
    len = MAGNITUDE( dir );
    SCALE_VEC( dir, len / size );

    /* make the base line */
    vr[vcount] = row;
    vc[vcount] = col;
    vl[vcount] = level;
    vcount++;
    vr[vcount] = tr = row - dir[0];
    vc[vcount] = tc = col - dir[1];
    vl[vcount] = tl = level - dir[2];
    vcount++;

    /* MJK 12.04.98 */
    get_cross_vec (a, dir, up);



    alen = sign * BARB_INC * MAGNITUDE( a ) / size;
    a[0] = a[0] / alen;
    a[1] = a[1] / alen;
    a[2] = a[2] / alen;

    SCALE_VEC( dir, BARB_INC );
    tr -= dir[0];
    tc -= dir[1];
    tl -= dir[2];

    /* compute numbers of triangles, long & short fletches */
    ikts = (int) kts + 2;
    ntri = ikts / 50;
    ikts = ikts % 50;
    nlong = ikts / 10;
    ikts = ikts % 10;
    nshort = ikts / 5;

    for (i=0; i<ntri; i++) {
      /* riser */
      vr[vcount] = tr;
      vc[vcount] = tc;
      vl[vcount] = tl;
      vcount++;

      vr[vcount] = tr + a[0];
      vc[vcount] = tc + a[1];
      vl[vcount] = tl + a[2];
      vcount++;

      /* cross piece inside triangle */
      vr[vcount] = tr;
      vc[vcount] = tc;
      vl[vcount] = tl;
      vcount++;

      vr[vcount] = tr + 0.5 * (dir[0] + a[0]);
      vc[vcount] = tc + 0.5 * (dir[1] + a[1]);
      vl[vcount] = tl + 0.5 * (dir[2] + a[2]);
      vcount++;

      /* hypotenuse */
      vr[vcount] = tr + a[0];
      vc[vcount] = tc + a[1];
      vl[vcount] = tl + a[2];
      vcount++;

      tr += dir[0];
      tc += dir[1];
      tl += dir[2];

      vr[vcount] = tr;
      vc[vcount] = tc;
      vl[vcount] = tl;
      vcount++;

      /* extend base line for first triangle */
      if (i == 0) {
        vr[vcount] = tr;
        vc[vcount] = tc;
        vl[vcount] = tl;
        vcount++;

        vr[vcount] = tr - dir[0];
        vc[vcount] = tc - dir[1];
        vl[vcount] = tl - dir[2];
        vcount++;
      }
    }

    for (i=0; i<nlong; i++) {
      vr[vcount] = tr + a[0];
      vc[vcount] = tc + a[1];
      vl[vcount] = tl + a[2];
      vcount++;

      tr += dir[0];
      tc += dir[1];
      tl += dir[2];

      vr[vcount] = tr;
      vc[vcount] = tc;
      vl[vcount] = tl;
      vcount++;
    }

    for (i=0; i<nshort; i++) {
      vr[vcount] = tr + 0.5 * (dir[0] + a[0]);
      vc[vcount] = tc + 0.5 * (dir[1] + a[1]);
      vl[vcount] = tl + 0.5 * (dir[2] + a[2]);
      vcount++;

      tr += dir[0];
      tc += dir[1];
      tl += dir[2];

      vr[vcount] = tr;
      vc[vcount] = tc;
      vl[vcount] = tl;
      vcount++;
    }

  }

  *vco = vcount;
}



/*
 * Compute vectors in a horizontal wind slice.
 * Input:  displaytime - which display timestep
 *         ws - which wind slice [0,WINDSLICES-1]
 *         level - vis5d_ctx  level of slice in [0,Nl-1]
 *         scale - user scaling factor  (1.0 is typical)
 *         density - user density factor  1.0, 0.5, 0.25, etc.
 *         threadnum - which thread
 */
static void calc_hwindslice( Display_Context dtx, int displaytime, int ws,
                             float level, float scale, float density,
                             int threadnum )
{
   Context ctx;
   float *grid, *ugrid, *vgrid, *wgrid;
   float mpcol, mprow, mplev;   /* mp = meters per */
   float midlat, midlon;
   int row, col, drow, dcol, vcount;
   float *vr, *vc, *vl;
   int_2 *cverts;
   int uvar, vvar, wvar;
   float *boxverts;
   int numboxverts;
   int  sameUVW;
   float ctxlevel;
   int time;
   float arrowr[4], arrowc[4], arrowl[4];
   float arrowx[4], arrowy[4], arrowz[4];
   float boxy, px, py, pz;


   uvar = dtx->Uvar[ws];
   vvar = dtx->Vvar[ws];
   wvar = dtx->Wvar[ws];

   ctx = dtx->ctxpointerarray[return_ctx_index_pos(dtx, dtx->Uvarowner[ws])];
   if (!ctx){
      printf("error in getting rctx in calc_hwindslice\n");
   }


   /* MJK 12.04.98 */
   if (ctx->dpy_ctx->DisplaySfcHWind[ws]) wvar = -1;


   ctxlevel = gridlevelPRIME_to_gridlevel( ctx, level);
   time = dtx->TimeStep[displaytime].ownerstimestep[return_ctx_index_pos(dtx, ctx->context_index)];
   if (displaytime > 0){
      int timebefore;
      timebefore = dtx->TimeStep[displaytime-1].ownerstimestep[
                   return_ctx_index_pos(dtx, ctx->context_index)];
      if (time == timebefore){
         return;
      }
   }
   if (uvar<0 || vvar<0) {
      /* no wind variables specified */
      return;
   }
   /* Get U, V, W 2-D grids */
   grid = get_grid( ctx, time, uvar );
   if (!grid) return;


   /* MJK 12.04.98 */
   if (ctx->dpy_ctx->DisplaySfcHWind[ws]){
      ugrid = extract_sfc_slice (ctx, time, uvar, ctx->Nr, ctx->Nc, grid, 0);
   }
   else{
      ugrid = extract_hslice( ctx, grid, uvar, ctx->Nr, ctx->Nc, ctx->Nl[uvar],
                              ctx->LowLev[uvar], ctxlevel, 0 );
   }


   release_grid( ctx, time, uvar, grid );


   grid = get_grid( ctx, time, vvar );
   if (!grid) return;



   /* MJK 12.04.98 */
   if (ctx->dpy_ctx->DisplaySfcHWind[ws]){
      vgrid = extract_sfc_slice (ctx, time, vvar, ctx->Nr, ctx->Nc, grid, 0);
   }
   else{
      vgrid = extract_hslice( ctx, grid, vvar, ctx->Nr, ctx->Nc, ctx->Nl[vvar],
                           ctx->LowLev[vvar], ctxlevel, 0 );
   }




   release_grid( ctx, time, vvar, grid );


   if (wvar>-1) {
      grid = get_grid( ctx, time, wvar );
      if (!grid) return;
      wgrid = extract_hslice( ctx, grid, wvar, ctx->Nr, ctx->Nc, ctx->Nl[wvar],
                              ctx->LowLev[wvar], ctxlevel, 0 );
      release_grid( ctx, time, wvar, grid );
   }

   vr = (float *) malloc(sizeof(float)*MAX_WIND_VERTS);
   vc = (float *) malloc(sizeof(float)*MAX_WIND_VERTS);
   vl = (float *) malloc(sizeof(float)*MAX_WIND_VERTS);
   if (!vr || !vc || !vl){
      printf(" You do not have enough memory to create hwinds.\n");
      if (vr){
         free(vr);
      }
      if (vc){
         free(vc);
      }
      if (vl){
         free(vl);
      }
      deallocate( ctx, ugrid, -1 );
      deallocate( ctx, vgrid, -1 );
      if (wvar>-1){
        deallocate( ctx, wgrid, -1 );
      }
      return;
   }

   /* Density: */
   if (density>1.0 || density<0.0)
     density = 1.0;
   drow = (int) (1.0 / density);
   dcol = (int) (1.0 / density);


   /* calculate vector vertices */
   vcount = 0;


   boxy = dtx->Xmax-dtx->Xmin;
   for (row=0; row<ctx->Nr && vcount+40<MAX_WIND_VERTS; row+=drow) {
      for (col=0; col<ctx->Nc && vcount+40<MAX_WIND_VERTS; col+=dcol) {
         float u, v, w;
         float a[3], dir[3], up[3], len, alen;
         float pr, pc, pl;
         float tr, tc, tl;

         /* get <U,V,W> vector */
         u = ugrid[row*ctx->Nc+col];
         v = vgrid[row*ctx->Nc+col];
         w = (wvar>-1) ? wgrid[row*ctx->Nc+col] : 0.0;
         if (IS_MISSING(u) || IS_MISSING(v) || IS_MISSING(w)) {
            /* if u, v, or w is missing, draw no vector */
         }
         else {
            float newlen, oldlen, factor;

            oldlen = sqrt (u * u + v * v + w * w);

            /* make wind arrows parallel to trajectories */
            dir[0] = v * ctx->Vscale[row][col];
            dir[1] = u * ctx->Uscale[row][col];
            dir[2] = w * ctx->Wscale[(int) ctxlevel];

            len = MAGNITUDE( dir );
            arrowr[0] = (float) row;
            arrowc[0] = (float) col;
            arrowl[0] = ctxlevel;

            arrowr[1] = (float) row + dir[0];
            arrowc[1] = (float) col + dir[1];
            arrowl[1] = ctxlevel + dir[2];

            /* get xyz arrow */
            gridPRIME_to_xyzPRIME( dtx, time, uvar, 2, arrowr, arrowc,
                                    arrowl, arrowx, arrowy, arrowz);

            dir[0] = arrowx[1] - arrowx[0];
            dir[1] = arrowy[1] - arrowy[0];
            dir[2] = arrowz[1] - arrowz[0];

            newlen = MAGNITUDE( dir );
            factor = (newlen > 0.0000001) ? oldlen / newlen :
                                            oldlen / 0.0000001;

            /* multiply vector by factor in graphics space */
            factor = (factor / 25.0) * (boxy * 0.03) * scale;

            dir[0] *= factor;
            dir[1] *= factor;
            dir[2] *= factor;

            len = MAGNITUDE( dir );

            if ((dir[2]>len/2.0 || dir[2]<-len/2.0) && dtx->WindBarbs == 0) {
              up[0] = 1.0;  up[1] = 0.0;  up[2] = 0.0;
            }
            else {
              up[0] = 0.0;  up[1] = 0.0;  up[2] = 1.0;
            }

            arrowx[1] = arrowx[0] + dir[0];
            arrowy[1] = arrowy[0] + dir[1];
            arrowz[1] = arrowz[0] + dir[2];

            /* create arrow head in graphics space too!.. */
            /* this gets rid of demented arrow head sizes */
            /* when ratio of row, cols, levs is wacky*/
            CROSS( a, dir, up );
            alen = MAGNITUDE( a );

/* MJK 3.25.99 */
            if (alen * len * 0.1 == 0.0){
               a[0] = 0.0;
               a[1] = 0.0;
               a[2] = 0.0;
            }
            else{
               a[0] = a[0] / alen * len * 0.1;
               a[1] = a[1] / alen * len * 0.1;
               a[2] = a[2] / alen * len * 0.1;
            }

            px = arrowx[1] - dir[0] * 0.2;
            py = arrowy[1] - dir[1] * 0.2;
            pz = arrowz[1] - dir[2] * 0.2;

            arrowx[2] = px + a[0];
            arrowy[2] = py + a[1];
            arrowz[2] = pz + a[2];
            arrowx[3] = px - a[0];
            arrowy[3] = py - a[1];
            arrowz[3] = pz - a[2];

            xyzPRIME_to_gridPRIME( dtx, time, uvar, arrowx[0], arrowy[0],
                                       arrowz[0], &arrowr[0], &arrowc[0], &arrowl[0]);
            xyzPRIME_to_gridPRIME( dtx, time, uvar, arrowx[1], arrowy[1],
                                       arrowz[1], &arrowr[1], &arrowc[1], &arrowl[1]);
            xyzPRIME_to_gridPRIME( dtx, time, uvar, arrowx[2], arrowy[2],
                                       arrowz[2], &arrowr[2], &arrowc[2], &arrowl[2]);
            xyzPRIME_to_gridPRIME( dtx, time, uvar, arrowx[3], arrowy[3],
                                       arrowz[3], &arrowr[3], &arrowc[3], &arrowl[3]);

            if (dtx->WindBarbs == 0) {
               if (len>=0.001) {
                  if (ctx->dpy_ctx->DisplaySfcHWind[ws]){
                     vr[vcount] = arrowr[0];
                     vc[vcount] = arrowc[0];
                     vl[vcount] = arrowl[0];

                     vr[vcount+1] = arrowr[1];
                     vc[vcount+1] = arrowc[1];
                     vl[vcount+1] = arrowl[1];

                     vr[vcount+2] = vr[vcount+1];
                     vc[vcount+2] = vc[vcount+1];
                     vl[vcount+2] = vl[vcount+1];

                     vr[vcount+3] = arrowr[2];
                     vc[vcount+3] = arrowc[2];
                     vl[vcount+3] = arrowl[2];

                     vr[vcount+4] = vr[vcount+1];
                     vc[vcount+4] = vc[vcount+1];
                     vl[vcount+4] = vl[vcount+1];
                     
                     vr[vcount+5] = arrowr[3];
                     vc[vcount+5] = arrowc[3];
                     vl[vcount+5] = arrowl[3];
                     vcount +=6;
                  }
                  else{
                     vr[vcount] = arrowr[0];
                     vc[vcount] = arrowc[0];
                     vl[vcount] = arrowl[0];

                     vr[vcount+1] = arrowr[1];
                     vc[vcount+1] = arrowc[1];
                     vl[vcount+1] = arrowl[1];

                     vr[vcount+2] = arrowr[2];
                     vc[vcount+2] = arrowc[2];
                     vl[vcount+2] = arrowl[2];

                     vr[vcount+3] = arrowr[3];
                     vc[vcount+3] = arrowc[3];
                     vl[vcount+3] = arrowl[3];

                     vcount+=4;
                  }
                  /* Used to debug R8000 -O2/O3 bug:
                  if (vcount==12) {
                     int ii;
                     printf("\n");
                     for (ii=0;ii<12;ii++) {
                        printf("%g %g %g\n", vr[ii], vc[ii], vl[ii] );
                     }
                     printf("  %g %g %g  %g %g %g\n", pr, pc, pl, a[0], a[1], a[2] );
                  }*/
               }
            }
            else{
               dir[0] = arrowr[1] - arrowr[0];
               dir[1] = arrowc[1] - arrowc[0];
               dir[2] = arrowl[1] - arrowl[0];

               len = MAGNITUDE( dir );

               up[0] = 0.0;  up[1] = 0.0;  up[2] = 1.0;

               make_barb(dtx, u, v, w, dir, up, (float) row, (float) col, ctxlevel,
                       vr, vc, vl, drow, &vcount);
            }
         } /* end if U, V & W not MISSING */
      } /* end for (col=... */
   } /* end for (row=... */

   /* deallocate 2-D grids */
   deallocate( ctx, ugrid, -1 );
   deallocate( ctx, vgrid, -1 );
   if (wvar>-1){
     deallocate( ctx, wgrid, -1 );
   }
   /*
    * Bounding rectangle
    */
   numboxverts = make_horizontal_rectangle( ctx, time, uvar, dtx->CurvedBox,
                                            level, &boxverts );

   /************************ Compress ********************************/


   /* MJK 12.04.98 */
   if (ctx->dpy_ctx->DisplaySfcHWind[ws]){
      vcount = fit_vecs_to_topo (ctx, vcount, MAX_WIND_VERTS, vr, vc, vl);
   }

 
   if (vcount>0) {
      int bytes = 3*vcount*sizeof(int_2);
      cverts = (int_2 *) allocate_type( ctx, bytes, WINDXH_TYPE );
      if (!cverts) {
         deallocate( ctx, cverts, bytes);
         cverts = NULL;
         vcount = 0;
      }
      else{
         gridPRIME_to_compXYZPRIME( dtx, time, uvar, vcount, vr, vc, vl, (void*)cverts );
      }
   }
   else {
      vcount = 0;
      cverts = NULL;
   }

   /************************* Store wind slice ***********************/
   recent( ctx, HWIND, ws );
   wait_write_lock( &dtx->HWindTable[ws][time].lock );

   /* deallocate existing slice, if any */
   free_hwind( dtx, time, ws );

   /* store new slice */
   dtx->HWindTable[ws][time].uvar = dtx->Uvar[ws];
   dtx->HWindTable[ws][time].vvar = dtx->Vvar[ws];
   dtx->HWindTable[ws][time].wvar = dtx->Wvar[ws];
   dtx->HWindTable[ws][time].uvarowner = dtx->Uvarowner[ws];
   dtx->HWindTable[ws][time].vvarowner = dtx->Vvarowner[ws];
   dtx->HWindTable[ws][time].wvarowner = dtx->Wvarowner[ws];
   dtx->HWindTable[ws][time].level = level;
   dtx->HWindTable[ws][time].density = density;
   dtx->HWindTable[ws][time].scale = scale;
   dtx->HWindTable[ws][time].nvectors = vcount;  /* 4 vertices / vector */
   dtx->HWindTable[ws][time].verts = cverts;
   dtx->HWindTable[ws][time].boxverts = boxverts;
   dtx->HWindTable[ws][time].numboxverts = numboxverts;
   dtx->HWindTable[ws][time].valid = 1;
   dtx->HWindTable[ws][time].barbs = dtx->WindBarbs;
   dtx->HWindTable[ws][time].uvarowner = ctx->context_index; 
   done_write_lock( &dtx->HWindTable[ws][time].lock );

   if (time==ctx->CurTime) {
      dtx->Redraw = 1;
   }
   free(vr);
   free(vc);
   free(vl);
}



static void calc_hwindslicePRIME( Display_Context dtx, int displaytime, int ws,
                             float level, float scale, float density,
                             int threadnum )
{
   Context ctx;
   float *grid, *ugrid, *vgrid, *wgrid;
   float mpcol, mprow, mplev;   /* mp = meters per */
   float midlat, midlon;
   int row, col, drow, dcol, vcount;
   float *vr, *vc, *vl;   
   int_2 *cverts;
   int uvar, vvar, wvar;
   float *boxverts;
   int numboxverts;
   int  sameUVW;
   float ctxlevel;
   int time;
   float jlat[2], jlon[2], jhgt[2];
   float jx[2], jy[2], jz[2];
   float arrowr[4], arrowc[4], arrowl[4];
   float arrowx[4], arrowy[4], arrowz[4];
   float boxy, px, py, pz;

   uvar = dtx->Uvar[ws];
   vvar = dtx->Vvar[ws];
   wvar = dtx->Wvar[ws];

   ctx = dtx->ctxpointerarray[return_ctx_index_pos(dtx, dtx->Uvarowner[ws])];
   if (!ctx){
      printf("error in getting rctx in calc_hwindslice\n");
   }

   /* MJK 12.04.98 */
   if (ctx->dpy_ctx->DisplaySfcHWind[ws]) wvar = -1;


   ctxlevel = gridlevelPRIME_to_gridlevel( ctx, level);
   time = dtx->TimeStep[displaytime].ownerstimestep[return_ctx_index_pos(dtx, ctx->context_index)];
   if (displaytime > 0){
      int timebefore;
      timebefore = dtx->TimeStep[displaytime-1].ownerstimestep[
                   return_ctx_index_pos(dtx, ctx->context_index)];
      if (time == timebefore){
         return;
      }
   }
   if (uvar<0 || vvar<0) {
      /* no wind variables specified */
      return;
   }

   /* Get U, V, W 2-D grids */
   grid = get_grid( ctx, time, uvar );
   if (!grid) return;


   /* MJK 12.04.98 */
   if (ctx->dpy_ctx->DisplaySfcHWind[ws]){
      ugrid = extract_sfc_slice (ctx, time, uvar, ctx->Nr, ctx->Nc, grid, 0);
   }
   else{
      ugrid = extract_hslice( ctx, grid, uvar, ctx->Nr, ctx->Nc, ctx->Nl[uvar],
                              ctx->LowLev[uvar], ctxlevel, 0 );
   }

   release_grid( ctx, time, uvar, grid );


   grid = get_grid( ctx, time, vvar );
   if (!grid) return;


   /* MJK 12.04.98 */
   if (ctx->dpy_ctx->DisplaySfcHWind[ws]){
      vgrid = extract_sfc_slice (ctx, time, vvar, ctx->Nr, ctx->Nc, grid, 0);
   }
   else{
      vgrid = extract_hslice( ctx, grid, vvar, ctx->Nr, ctx->Nc, ctx->Nl[vvar],
                           ctx->LowLev[vvar], ctxlevel, 0 );
   }


   release_grid( ctx, time, vvar, grid );
   if (wvar>-1) {
      grid = get_grid( ctx, time, wvar );
      if (!grid) return;
      wgrid = extract_hslice( ctx, grid, wvar, ctx->Nr, ctx->Nc, ctx->Nl[wvar],
                              ctx->LowLev[wvar], ctxlevel, 0 );
      release_grid( ctx, time, wvar, grid );
   }

   vr = (float *) malloc(sizeof(float)*MAX_WIND_VERTS);
   vc = (float *) malloc(sizeof(float)*MAX_WIND_VERTS);
   vl = (float *) malloc(sizeof(float)*MAX_WIND_VERTS);
   if (!vr || !vc || !vl){
      printf(" You do not have enough memory to create hwinds.\n");
      if (vr){
         free(vr);
      }
      if (vc){
         free(vc);
      }
      if (vl){
         free(vl);
      }
      deallocate( ctx, ugrid, -1 );
      deallocate( ctx, vgrid, -1 );
      if (wvar>-1){
        deallocate( ctx, wgrid, -1 );
      }
      return;
   }

   /* Density: */
   if (density>1.0 || density<0.0)
     density = 1.0;
   drow = (int) (1.0 / density);
   dcol = (int) (1.0 / density);


   /* calculate vector vertices */
   vcount = 0;

   boxy = dtx->Xmax-dtx->Xmin;
   for (row=0; row<ctx->Nr && vcount+40<MAX_WIND_VERTS; row+=drow) {
      for (col=0; col<ctx->Nc && vcount+40<MAX_WIND_VERTS; col+=dcol) {
         float u, v, w;
         float a[3], dir[3], up[3], len, alen;
         float pr, pc, pl;
         float tr, tc, tl;
         float br[2], bc[2], bl[2], cr[2], cc[2], cl[2];
         float dr[2], dc[2], dl[2], zr[2], zc[2], zl[2], aa[3];

         /* get <U,V,W> vector */
         u = ugrid[row*ctx->Nc+col];
         v = vgrid[row*ctx->Nc+col];
         w = (wvar>-1) ? wgrid[row*ctx->Nc+col] : 0.0;
         if (IS_MISSING(u) || IS_MISSING(v) || IS_MISSING(w)) {
            /* if u, v, or w is missing, draw no vector */
         }
         else {
            float newlen, oldlen, factor;

            oldlen = sqrt (u * u + v * v + w * w);

            /* make wind arrows parallel to trajectories */
            dir[0] = v * ctx->Vscale[row][col];
            dir[1] = u * ctx->Uscale[row][col];
            dir[2] = w * ctx->Wscale[(int) ctxlevel];

            len = MAGNITUDE( dir );
            arrowr[0] = (float) row;
            arrowc[0] = (float) col;
            arrowl[0] = ctxlevel;

            arrowr[1] = (float) row + dir[0];
            arrowc[1] = (float) col + dir[1];
            arrowl[1] = ctxlevel + dir[2];

            /* get xyz arrow */
            grid_to_xyz( ctx, time, uvar, 2, arrowr, arrowc,
                                    arrowl, arrowx, arrowy, arrowz);

            dir[0] = arrowx[1] - arrowx[0];
            dir[1] = arrowy[1] - arrowy[0];
            dir[2] = arrowz[1] - arrowz[0];

            newlen = MAGNITUDE( dir );
            factor = (newlen > 0.0000001) ? oldlen / newlen :
                                            oldlen / 0.0000001;

            /* multiply vector by factor in graphics space */
            factor = (factor / 25.0) * (boxy * 0.03) * scale;

            dir[0] *= factor;
            dir[1] *= factor;
            dir[2] *= factor;

            len = MAGNITUDE( dir );

            /* get small unit vector! */
            arrowx[1] = arrowx[0] + (dir[0]/(len*100));
            arrowy[1] = arrowy[0] + (dir[1]/(len*100));
            arrowz[1] = arrowz[0] + (dir[2]/(len*100));

            xyz_to_geo( ctx, time, uvar, arrowx[0], arrowy[0],
                           arrowz[0], &jlat[0], &jlon[0], &jhgt[0]);
            xyz_to_geo( ctx, time, uvar, arrowx[1], arrowy[1],
                           arrowz[1], &jlat[1], &jlon[1], &jhgt[1]);

            geo_to_xyzPRIME(dtx, displaytime, uvar, 2, jlat, jlon,
                                 jhgt, jx, jy, jz);

            /* now get vector in PRIME space */
            arrowx[0] = jx[0];
            arrowy[0] = jy[0];
            arrowz[0] = jz[0];

            arrowx[1] = jx[0] + 100*len*(jx[1] - jx[0]);
            arrowy[1] = jy[0] + 100*len*(jy[1] - jy[0]);
            arrowz[1] = jz[0] + 100*len*(jz[1] - jz[0]);

            dir[0] = arrowx[1] - arrowx[0];
            dir[1] = arrowy[1] - arrowy[0];
            dir[2] = arrowz[1] - arrowz[0];

            len = MAGNITUDE( dir );

            if ((dir[2]>len/2.0 || dir[2]<-len/2.0) && dtx->WindBarbs == 0) {
              up[0] = 1.0;  up[1] = 0.0;  up[2] = 0.0;
            }
            else {
              up[0] = 0.0;  up[1] = 0.0;  up[2] = 1.0;
            }

            /* create arrow head in graphics space too!.. */
            /* this gets rid of demented arrow head sizes */
            /* when ratio of row, cols, levs is wacky*/
            CROSS( a, dir, up );
            alen = MAGNITUDE( a );

/* MJK 3.25.99 */            
            if (alen * len * 0.1 == 0.0){            
               a[0] = 0.0;            
               a[1] = 0.0;            
               a[2] = 0.0;            
            }            
            else{            
               a[0] = a[0] / alen * len * 0.1;
               a[1] = a[1] / alen * len * 0.1;
               a[2] = a[2] / alen * len * 0.1;
            }

            px = arrowx[1] - dir[0] * 0.2;
            py = arrowy[1] - dir[1] * 0.2;
            pz = arrowz[1] - dir[2] * 0.2;

            arrowx[2] = px + a[0];
            arrowy[2] = py + a[1];
            arrowz[2] = pz + a[2];
            arrowx[3] = px - a[0];
            arrowy[3] = py - a[1];
            arrowz[3] = pz - a[2];

            xyzPRIME_to_gridPRIME( dtx, time, uvar, arrowx[0], arrowy[0],
                                       arrowz[0], &arrowr[0], &arrowc[0], &arrowl[0]);
            xyzPRIME_to_gridPRIME( dtx, time, uvar, arrowx[1], arrowy[1],
                                       arrowz[1], &arrowr[1], &arrowc[1], &arrowl[1]);
            xyzPRIME_to_gridPRIME( dtx, time, uvar, arrowx[2], arrowy[2],
                                       arrowz[2], &arrowr[2], &arrowc[2], &arrowl[2]);
            xyzPRIME_to_gridPRIME( dtx, time, uvar, arrowx[3], arrowy[3],
                                       arrowz[3], &arrowr[3], &arrowc[3], &arrowl[3]);

            if (dtx->WindBarbs == 0) {
               if (len>=0.001) {
                  if (ctx->dpy_ctx->DisplaySfcHWind[ws]){
                     vr[vcount] = arrowr[0];
                     vc[vcount] = arrowc[0];
                     vl[vcount] = arrowl[0];

                     vr[vcount+1] = arrowr[1];
                     vc[vcount+1] = arrowc[1];
                     vl[vcount+1] = arrowl[1];

                     vr[vcount+2] = vr[vcount+1];
                     vc[vcount+2] = vc[vcount+1];
                     vl[vcount+2] = vl[vcount+1];
                     
                     vr[vcount+3] = arrowr[2];
                     vc[vcount+3] = arrowc[2];
                     vl[vcount+3] = arrowl[2];
                     
                     vr[vcount+4] = vr[vcount+1];
                     vc[vcount+4] = vc[vcount+1];
                     vl[vcount+4] = vl[vcount+1];
                                          
                     vr[vcount+5] = arrowr[3];
                     vc[vcount+5] = arrowc[3];
                     vl[vcount+5] = arrowl[3];
                     vcount +=6;
                  }
                  else{
                     vr[vcount] = arrowr[0];
                     vc[vcount] = arrowc[0];
                     vl[vcount] = arrowl[0];

                     vr[vcount+1] = arrowr[1];
                     vc[vcount+1] = arrowc[1];
                     vl[vcount+1] = arrowl[1];

                     vr[vcount+2] = arrowr[2];
                     vc[vcount+2] = arrowc[2];
                     vl[vcount+2] = arrowl[2];

                     vr[vcount+3] = arrowr[3];
                     vc[vcount+3] = arrowc[3];
                     vl[vcount+3] = arrowl[3];

                     vcount+=4;
                  }
                  /* Used to debug R8000 -O2/O3 bug:
                  if (vcount==12) {
                     int ii;
                     printf("\n");
                     for (ii=0;ii<12;ii++) {
                        printf("%g %g %g\n", vr[ii], vc[ii], vl[ii] );
                     }
                     printf("  %g %g %g  %g %g %g\n", pr, pc, pl, a[0], a[1], a[2] );
                  }*/
               }
            }
            else{
               dir[0] = arrowr[1] - arrowr[0];
               dir[1] = arrowc[1] - arrowc[0];
               dir[2] = arrowl[1] - arrowl[0];

               len = MAGNITUDE( dir );

               up[0] = 0.0;  up[1] = 0.0;  up[2] = 1.0;

               make_barb(dtx, u, v, w, dir, up, (float) row, (float) col, ctxlevel,
                       vr, vc, vl, drow, &vcount);
            }
         } /* end if U, V & W not MISSING */
      } /* end for (col=... */
   } /* end for (row=... */

   /* deallocate 2-D grids */
   deallocate( ctx, ugrid, -1 );
   deallocate( ctx, vgrid, -1 );
   if (wvar>-1){
     deallocate( ctx, wgrid, -1 );
   }
   /*
    * Bounding rectangle
    */
   numboxverts = make_horizontal_rectangle( ctx, time, uvar, dtx->CurvedBox,
                                            level, &boxverts );

   /************************ Compress ********************************/

   /* MJK 12.09.98 */
   if (ctx->dpy_ctx->DisplaySfcHWind[ws]){
      vcount = fit_vecs_to_topo (ctx, vcount, MAX_WIND_VERTS, vr, vc, vl);
   }


   if (vcount>0) {
      int bytes = 3*vcount*sizeof(int_2);
      cverts = (int_2 *) allocate_type( ctx, bytes, WINDXH_TYPE );
      if (!cverts) {
         deallocate( ctx, cverts, bytes);
         cverts = NULL;
         vcount = 0;
      }
      else{
         gridPRIME_to_compXYZPRIME( dtx, time, uvar, vcount, vr, vc, vl, (void*)cverts );
      }
   }
   else {
      vcount = 0;
      cverts = NULL;
   }

   /************************* Store wind slice ***********************/
   recent( ctx, HWIND, ws );
   wait_write_lock( &dtx->HWindTable[ws][time].lock );

   /* deallocate existing slice, if any */
   free_hwind( dtx, time, ws );

   /* store new slice */
   dtx->HWindTable[ws][time].uvar = dtx->Uvar[ws];
   dtx->HWindTable[ws][time].vvar = dtx->Vvar[ws];
   dtx->HWindTable[ws][time].wvar = dtx->Wvar[ws];
   dtx->HWindTable[ws][time].uvarowner = dtx->Uvarowner[ws];
   dtx->HWindTable[ws][time].vvarowner = dtx->Vvarowner[ws];
   dtx->HWindTable[ws][time].wvarowner = dtx->Wvarowner[ws];
   dtx->HWindTable[ws][time].level = level;
   dtx->HWindTable[ws][time].density = density;
   dtx->HWindTable[ws][time].scale = scale;
   dtx->HWindTable[ws][time].nvectors = vcount;  /* 4 vertices / vector */
   dtx->HWindTable[ws][time].verts = cverts;
   dtx->HWindTable[ws][time].boxverts = boxverts;
   dtx->HWindTable[ws][time].numboxverts = numboxverts;
   dtx->HWindTable[ws][time].valid = 1;
   dtx->HWindTable[ws][time].barbs = dtx->WindBarbs;
   dtx->HWindTable[ws][time].uvarowner = ctx->context_index;
   done_write_lock( &dtx->HWindTable[ws][time].lock );

   if (time==ctx->CurTime) {
      dtx->Redraw = 1;
   }
   free(vr);
   free(vc);
   free(vl);
}


static void calc_vclip( Display_Context dtx, int num, float r1,
                        float c1, float r2, float c2)
{
   int i, n;
   float *v;
 
   n = 0;
   if (dtx->CurvedBox==0){
      v = (float *) malloc(5 * 3 * sizeof(float));
      if (!v){
         printf("error in calc_vclip\n");
         exit(1);
      }
      n = 5;
      v[0*3+0] = r1;
      v[0*3+1] = c1;
      v[0*3+2] = dtx->LowLev;
      v[1*3+0] = r1;
      v[1*3+1] = c1;
      v[1*3+2] = dtx->Nl-1 + dtx->LowLev;
      v[2*3+0] = r2;
      v[2*3+1] = c2;
      v[2*3+2] = dtx->Nl-1 + dtx->LowLev;
      v[3*3+0] = r2;
      v[3*3+1] = c2;
      v[3*3+2] = dtx->LowLev;
      v[4*3+0] = r1;
      v[4*3+1] = c1;
      v[4*3+2] = dtx->LowLev;
   }
   else{
      float r, c, dr, dc;
      v = (float *) malloc((2*dtx->Nl + 2*dtx->Nc - 3)* 3 * sizeof(float));
      if (!v){
         printf("error in calc_vclip\n");
         exit(1);
      }
      dr = (r2-r1) / (float) (dtx->Nc-1);
      dc = (c2-c1) / (float) (dtx->Nc-1);
      /* top */
      r = r1;
      c = c1;
      for (i=0;i<dtx->Nc;i++) {
         v[n++] = r;
         v[n++] = c;
         v[n++] = dtx->Nl-1 + dtx->LowLev;
         r += dr;
         c += dc;
      }
      /* right */
      for (i=dtx->Nl-2;i>=0;i--) {
         v[n++] = r2;
         v[n++] = c2;
         v[n++] = i + dtx->LowLev;
      }
      /* bottom */
      r = r2-dr;
      c = c2-dc;
      for (i=dtx->Nc-2;i>=0;i--) {
         v[n++] = r;
         v[n++] = c;
         v[n++] = dtx->LowLev;
         r -= dr;
         c -= dc;
      }
      /* left */
      for (i=1;i<dtx->Nl;i++) {
         v[n++] = r1;
         v[n++] = c1;
         v[n++] = i + dtx->LowLev;
      }
      n /= 3;
      assert( n == 2*dtx->Nl + 2*dtx->Nc - 3 );
   }
   /* convert vertices from grid to graphics coords */
   for (i=0;i<n;i++) {
      float r = v[i*3+0];
      float c = v[i*3+1];
      float l = v[i*3+2];
      gridPRIME_to_xyzPRIME( dtx, 0, 0, 1, &r, &c, &l,
                   &v[i*3+0], &v[i*3+1], &v[i*3+2] );
   }
   if (dtx->VClipTable[num].boxverts){
      free( dtx->VClipTable[num].boxverts );
      dtx->VClipTable[num].boxverts = NULL;
   }
   dtx->VClipTable[num].boxverts = v;
   dtx->VClipTable[num].numboxverts = n;
} 
          
static void calc_hclip( Display_Context dtx, int num, float level)
{
   int i, n;
   float *v;

   n = 0;
   if (dtx->CurvedBox==0){
      v = (float *) malloc(5 * 3 * sizeof(float));
      if (!v){
         printf("error in calc_vclip\n");
         exit(1);
      }
      n = 5;
      v[0*3+0] = 0.0;
      v[0*3+1] = 0.0;
      v[0*3+2] = level;
      v[1*3+0] = 0.0;
      v[1*3+1] = (float) (dtx->Nc-1);
      v[1*3+2] = level;
      v[2*3+0] = (float) (dtx->Nr-1);
      v[2*3+1] = (float) (dtx->Nc-1);
      v[2*3+2] = level;
      v[3*3+0] = (float) (dtx->Nr-1);
      v[3*3+1] = 0.0;
      v[3*3+2] = level;
      v[4*3+0] = 0.0;
      v[4*3+1] = 0.0;
      v[4*3+2] = level;
   }
   else{
      v = (float *) malloc((2*dtx->Nr + 2*dtx->Nc - 3)
                                   * 3 * sizeof(float));
      if (!v){
         printf("error in calc_vclip\n");
         exit(1);
      }       
      /* north edge */
      for (i=0;i<dtx->Nc;i++) {
         v[n++] = 0.0;
         v[n++] = i;
         v[n++] = level;
      }
      /* east edge */
      for (i=1;i<dtx->Nr;i++) {
         v[n++] = i;
         v[n++] = dtx->Nc-1;
         v[n++] = level;
      }
      /* south edge */
      for (i=dtx->Nc-2;i>=0;i--) {
         v[n++] = dtx->Nr-1;
         v[n++] = i;
         v[n++] = level;
      }
      /* west edge */
      for (i=dtx->Nr-2;i>=0;i--) {
         v[n++] = i;
         v[n++] = 0;
         v[n++] = level;
      }
      n /= 3;
      assert( n == 2*dtx->Nr + 2*dtx->Nc - 3 );
   }
   /* convert vertices from grid to graphics coords */
   for (i=0;i<n;i++) {
      float r = v[i*3+0];
      float c = v[i*3+1];
      float l = v[i*3+2];
      gridPRIME_to_xyzPRIME( dtx, 0, 0, 1, &r, &c, &l,
                   &v[i*3+0], &v[i*3+1], &v[i*3+2] );
   }
   if (dtx->HClipTable[num].boxverts){
      free(dtx->HClipTable[num].boxverts);
      dtx->HClipTable[num].boxverts = NULL;
   }
   dtx->HClipTable[num].boxverts = v;
   dtx->HClipTable[num].numboxverts = n;
}



/*
 * Compute vectors in a vertical wind slice.
 * Input:  displaytime - which display timestep
 *         ws - which wind slice [0,WINDSLICES-1]
 *         r1, c1 - display row, column of left end of slice
 *         r2, c2 - display row, column of right end of slice
 *         scale - user scaling factor  (1.0 is typical)
 *         density - user density factor  1.0, 0.5, 0.25, etc.
 *         threadnum - which thread
 */
static void calc_vwindslice( Display_Context dtx, int displaytime, int ws,
                             float r1, float c1, float r2, float c2,
                             float scale, float density,
                             int threadnum )
{
   Context ctx;
   float *grid,  *ugrid, *vgrid, *wgrid;
   int row, col, rows, cols, drow;
   float *vr, *vc, *vl;
   float midlat, midlon, mprow, mpcol, mplev;
   int vcount;
   int_2 *cverts;
   int uvar, vvar, wvar, uvarowner, vvarowner, wvarowner;
   float dr, dc;
   int numboxverts;
   float *boxverts;
   float up[3];
   int  sameUVW;
   int time;
   float level, leveljunk;
   float arrowr[4], arrowc[4], arrowl[4];
   float arrowx[4], arrowy[4], arrowz[4];
   float boxy, px, py, pz;


   /* Determine which variables to use for U,V,W */
   uvar = dtx->Uvar[ws];
   vvar = dtx->Vvar[ws];
   wvar = dtx->Wvar[ws];

   ctx = dtx->ctxpointerarray[return_ctx_index_pos(dtx, dtx->Uvarowner[ws])];
   if (!ctx){
      printf("error in getting ctx in calc_vwindslice\n");
   }

   time = dtx->TimeStep[displaytime].ownerstimestep[return_ctx_index_pos(dtx, ctx->context_index)];   if (displaytime > 0){
      int timebefore;
      timebefore = dtx->TimeStep[displaytime-1].ownerstimestep[
                   return_ctx_index_pos(dtx, ctx->context_index)];
      if (time == timebefore){
         return;
      }
   }

   if (uvar<0 || vvar<0) {
      /* no wind variables specified */
      return;
   }


   /*  YO if (dtx->LowLev[uvar] != dtx->LowLev[vvar] ||
       (wvar>-1 && ctx->LowLev[uvar] != ctx->LowLev[wvar])) {
      * wind low levels must match *
      return;
   }*/

   /* Density: */
   if (density>1.0 || density<0.0)
     density = 1.0;

   /* size of 2-D slice */
   rows = ctx->Nl[uvar];
   cols = MAX(ctx->Nr,ctx->Nc) * density;

   /* WLH 15 Oct 98 */
   if (rows <= 1 || cols <= 1) return;

   /* get u, v, w grid slices */
   grid = get_grid( ctx, time, uvar );
   if (!grid) return;
   ugrid = extract_vslice( ctx, grid, r1,c1, r2,c2, rows, cols, 0 );
   release_grid( ctx, time, uvar, grid );

   grid = get_grid( ctx, time, vvar );
   if (!grid) return;
   vgrid = extract_vslice( ctx, grid, r1,c1, r2,c2, rows, cols, 0 );
   release_grid( ctx, time, vvar, grid );

   if (wvar>-1) {
      grid = get_grid( ctx, time, wvar );
      if (!grid) return;
      wgrid = extract_vslice( ctx, grid, r1,c1, r2,c2, rows, cols, 0 );
      release_grid( ctx, time, wvar, grid );
   }

   vr = (float *) malloc(sizeof(float)*MAX_WIND_VERTS);
   vc = (float *) malloc(sizeof(float)*MAX_WIND_VERTS);
   vl = (float *) malloc(sizeof(float)*MAX_WIND_VERTS);
   if (!vr || !vc || !vl){
      printf(" You do not have enough memory to create vwinds.\n");
      if (vr){
         free(vr);
      }
      if (vc){
         free(vc);
      }
      if (vl){
         free(vl);
      }
      deallocate( ctx, ugrid, -1 );
      deallocate( ctx, vgrid, -1 );
      if (wvar>-1){
        deallocate( ctx, wgrid, -1 );
      }
      return;
   }

   drow = (int) (1.0 / density);     /* in slice coords */
   dr = (r2-r1) / (float) (cols-1);  /* delta row and column in */
   dc = (c2-c1) / (float) (cols-1);  /* 3-d grid coords */

   /* "upward" vector */
   up[0] = dc;  up[1] = dr;  up[2] = 0.0;
   NORMALIZE( up );

   /* Compute vectors */
   vcount = 0;
   boxy = dtx->Xmax-dtx->Xmin;
   for (row=0; row<rows && vcount+40<MAX_WIND_VERTS; row+=drow) {
      float gr = (float) r1;
      float gc = (float) c1;
      float gl = (float) (row + ctx->LowLev[uvar]);
      for (col=0; col<cols && vcount+40<MAX_WIND_VERTS; col++) {
         float u, v, w;
         float dir[3];  /* wind vector direction */
         float a[3];  /* up & across vectors */
         float pr, pc, pl, len, alen;
         float tr, tc, tl;

         u = ugrid[row*cols+col];
         v = vgrid[row*cols+col];
         w = (wvar>-1) ? wgrid[row*cols+col] : 0.0;
         if (IS_MISSING(u) || IS_MISSING(v) || IS_MISSING(w)) {
            /* if u, v, or w is missing, draw no vector */
         }
         else {
            float newlen, oldlen, factor;

            oldlen = sqrt (u * u + v * v + w * w);

            /* make wind arrows parallel to trajectories */
            dir[0] = v * ctx->Vscale[(int) gr][(int) gc];
            dir[1] = u * ctx->Uscale[(int) gr][(int) gc];
            dir[2] = w * ctx->Wscale[(int) gl];

            len = MAGNITUDE( dir );
            arrowr[0] = gr;
            arrowc[0] = gc;
            arrowl[0] = gl;

            arrowr[1] = gr + dir[0];
            arrowc[1] = gc + dir[1];
            arrowl[1] = gl + dir[2];

            /* get xyz arrow */
            gridPRIME_to_xyzPRIME( dtx, time, uvar, 2, arrowr, arrowc,
                                    arrowl, arrowx, arrowy, arrowz);

            dir[0] = arrowx[1] - arrowx[0];
            dir[1] = arrowy[1] - arrowy[0];
            dir[2] = arrowz[1] - arrowz[0];

            newlen = MAGNITUDE( dir );
            factor = (newlen > 0.0000001) ? oldlen / newlen :
                                            oldlen / 0.0000001;

            /* multiply vector by factor in graphics space */
            factor = (factor / 25.0) * (boxy * 0.03) * scale;

            dir[0] *= factor;
            dir[1] *= factor;
            dir[2] *= factor;

            len = MAGNITUDE( dir );

            if ((dir[1]>len/2.0 || dir[1]<-len/2.0) && dtx->WindBarbs == 0) {
              up[0] = 1.0;  up[1] = 0.0;  up[2] = 0.0;
            }
            else{
              up[0] = 0.0;  up[1] = 1.0;  up[2] = 0.0;
            }

            arrowx[1] = arrowx[0] + dir[0];
            arrowy[1] = arrowy[0] + dir[1];
            arrowz[1] = arrowz[0] + dir[2];

            /* create arrow head in graphics space too!.. */
            /* this gets rid of demented arrow head sizes */
            /* when ratio of row, cols, levs is wacky*/
            CROSS( a, dir, up );
            alen = MAGNITUDE( a );

/* MJK 3.25.99 */            
            if (alen * len * 0.1 == 0.0){            
               a[0] = 0.0;            
               a[1] = 0.0;            
               a[2] = 0.0;            
            }            
            else{            
               a[0] = a[0] / alen * len * 0.1;
               a[1] = a[1] / alen * len * 0.1;
               a[2] = a[2] / alen * len * 0.1;
            }

            px = arrowx[1] - dir[0] * 0.2;
            py = arrowy[1] - dir[1] * 0.2;
            pz = arrowz[1] - dir[2] * 0.2;

            arrowx[2] = px + a[0];
            arrowy[2] = py + a[1];
            arrowz[2] = pz + a[2];
            arrowx[3] = px - a[0];
            arrowy[3] = py - a[1];
            arrowz[3] = pz - a[2];

            xyzPRIME_to_gridPRIME( dtx, time, uvar, arrowx[0], arrowy[0],
                                       arrowz[0], &arrowr[0], &arrowc[0], &arrowl[0]);
            xyzPRIME_to_gridPRIME( dtx, time, uvar, arrowx[1], arrowy[1],
                                       arrowz[1], &arrowr[1], &arrowc[1], &arrowl[1]);
            xyzPRIME_to_gridPRIME( dtx, time, uvar, arrowx[2], arrowy[2],
                                       arrowz[2], &arrowr[2], &arrowc[2], &arrowl[2]);
            xyzPRIME_to_gridPRIME( dtx, time, uvar, arrowx[3], arrowy[3],
                                       arrowz[3], &arrowr[3], &arrowc[3], &arrowl[3]);

            if (dtx->WindBarbs == 0) {
               if (len>=0.001) {
                  vr[vcount] = arrowr[0];
                  vc[vcount] = arrowc[0];
                  vl[vcount] = arrowl[0];

                  vr[vcount+1] = arrowr[1];
                  vc[vcount+1] = arrowc[1];
                  vl[vcount+1] = arrowl[1];

                  vr[vcount+2] = arrowr[2];
                  vc[vcount+2] = arrowc[2];
                  vl[vcount+2] = arrowl[2];

                  vr[vcount+3] = arrowr[3];
                  vc[vcount+3] = arrowc[3];
                  vl[vcount+3] = arrowl[3];

                  vcount+=4;
                  /* Used to debug R8000 -O2/O3 bug:
                  if (vcount==12) {
                     int ii;
                     printf("\n");
                     for (ii=0;ii<12;ii++) {
                        printf("%g %g %g\n", vr[ii], vc[ii], vl[ii] );
                     }
                     printf("  %g %g %g  %g %g %g\n", pr, pc, pl, a[0], a[1], a[2] );
                  }*/
               }
            }
            else{
               dir[0] = arrowr[1] - arrowr[0];
               dir[1] = arrowc[1] - arrowc[0];
               dir[2] = arrowl[1] - arrowl[0];

               len = MAGNITUDE( dir );


               make_barb(dtx, u, v, w, dir, up, arrowr[0], arrowc[0], arrowl[0],
                       vr, vc, vl, drow, &vcount);
            }
         } /* end if U, V & W not MISSING */
         gr += dr;
         gc += dc;
      } /* end for (col=... */
   } /* end for (row=... */

   /* deallocate 2-D slices */
   deallocate( ctx, ugrid, -1 );
   deallocate( ctx, vgrid, -1 );
   if (wvar>-1){
     deallocate( ctx, wgrid, -1 );
   }

   /*
    * Bounding rectangle
    */
   numboxverts = make_vertical_rectangle( ctx, time, uvar, dtx->CurvedBox,
                                          r1, c1, r2, c2, cols, &boxverts );

   /*************************** Compress *******************************/

   if (vcount>0) {
      int bytes = 3*vcount*sizeof(int_2);
      cverts = (int_2 *) allocate_type( ctx, bytes, WINDXV_TYPE );
      if (!cverts) {
         deallocate( ctx, cverts, bytes);
         cverts = NULL;
         vcount = 0;
      }
      else {
         gridPRIME_to_compXYZPRIME( dtx, time, uvar, vcount, vr, vc, vl, (void*)cverts );
      }
   }
   else {
      vcount = 0;
      cverts= NULL;
   }

   /************************ Store wind slice ***************************/
   recent( ctx, VWIND, ws );

   wait_write_lock( &dtx->VWindTable[ws][time].lock );

   /* deallocate existing slice, if any */
   free_vwind( dtx, time, ws );

   /* store new slice */
   dtx->VWindTable[ws][time].uvar = dtx->Uvar[ws];
   dtx->VWindTable[ws][time].vvar = dtx->Vvar[ws];
   dtx->VWindTable[ws][time].wvar = dtx->Wvar[ws];
   dtx->VWindTable[ws][time].uvarowner = dtx->Uvarowner[ws];
   dtx->VWindTable[ws][time].vvarowner = dtx->Vvarowner[ws];
   dtx->VWindTable[ws][time].wvarowner = dtx->Wvarowner[ws];
   dtx->VWindTable[ws][time].r1 = r1;
   dtx->VWindTable[ws][time].c1 = c1;
   dtx->VWindTable[ws][time].r2 = r2;
   dtx->VWindTable[ws][time].c2 = c2;
   dtx->VWindTable[ws][time].density = density;
   dtx->VWindTable[ws][time].scale = scale;
   dtx->VWindTable[ws][time].nvectors = vcount;   /* 4 vertices / vector */
   dtx->VWindTable[ws][time].verts = cverts;
   dtx->VWindTable[ws][time].numboxverts = numboxverts;
   dtx->VWindTable[ws][time].boxverts = boxverts;
   dtx->VWindTable[ws][time].valid = 1;
   dtx->VWindTable[ws][time].barbs = dtx->WindBarbs;
   dtx->VWindTable[ws][time].uvarowner = ctx->context_index;
   done_write_lock( &dtx->VWindTable[ws][time].lock );

   if (time==ctx->CurTime) {
      dtx->Redraw = 1;
   }
   free(vr);
   free(vc);
   free(vl);
}



static void calc_vwindslicePRIME( Display_Context dtx, int displaytime, int ws,
                             float r1, float c1, float r2, float c2,
                             float scale, float density,
                             int threadnum )
{
   Context ctx;
   float *grid,  *ugrid, *vgrid, *wgrid;
   int row, col, rows, cols, drow;
   float *vr, *vc, *vl;
   float midlat, midlon, mprow, mpcol, mplev;
   int vcount;
   int_2 *cverts;
   int uvar, vvar, wvar, uvarowner, vvarowner, wvarowner;
   float dr, dc;
   int numboxverts;
   float *boxverts;
   float up[3];
   int  sameUVW;
   int time;
   float jlat[2], jlon[2], jhgt[2];
   float jx[2], jy[2], jz[2];
   float arrowr[4], arrowc[4], arrowl[4];
   float arrowx[4], arrowy[4], arrowz[4];
   float boxy, px, py, pz;



   /* Determine which variables to use for U,V,W */
   uvar = dtx->Uvar[ws];
   vvar = dtx->Vvar[ws];
   wvar = dtx->Wvar[ws];

   ctx = dtx->ctxpointerarray[return_ctx_index_pos(dtx, dtx->Uvarowner[ws])];
   if (!ctx){
      printf("error in getting ctx in calc_vwindslice\n");
   }

   time = dtx->TimeStep[displaytime].ownerstimestep[return_ctx_index_pos(dtx, ctx->context_index)];   if (displaytime > 0){
      int timebefore;
      timebefore = dtx->TimeStep[displaytime-1].ownerstimestep[
                   return_ctx_index_pos(dtx, ctx->context_index)];
      if (time == timebefore){
         return;
      }
   }

   if (uvar<0 || vvar<0) {
      /* no wind variables specified */
      return;
   }


   /*  YO if (dtx->LowLev[uvar] != dtx->LowLev[vvar] ||
       (wvar>-1 && ctx->LowLev[uvar] != ctx->LowLev[wvar])) {
      * wind low levels must match *
      return;
   }*/

   /* Density: */
   if (density>1.0 || density<0.0)
     density = 1.0;

   /* size of 2-D slice */
   rows = dtx->Nl;
   cols = MAX(dtx->Nr,dtx->Nc) * density;

   /* get u, v, w grid slices */
   grid = get_grid( ctx, time, uvar );
   if (!grid) return;
   ugrid = extract_vslicePRIME( ctx, grid, time, uvar, r1,c1, r2,c2, rows, cols, 0 );
   release_grid( ctx, time, uvar, grid );

   grid = get_grid( ctx, time, vvar );
   if (!grid) return;
   vgrid = extract_vslicePRIME( ctx, grid, time, vvar, r1,c1, r2,c2, rows, cols, 0 );
   release_grid( ctx, time, vvar, grid );

   if (wvar>-1) {
      grid = get_grid( ctx, time, wvar );
      if (!grid) return;
      wgrid = extract_vslicePRIME( ctx, grid, time, wvar, r1,c1, r2,c2, rows, cols, 0 );
      release_grid( ctx, time, wvar, grid );
   }

   vr = (float *) malloc(sizeof(float)*MAX_WIND_VERTS);
   vc = (float *) malloc(sizeof(float)*MAX_WIND_VERTS);
   vl = (float *) malloc(sizeof(float)*MAX_WIND_VERTS);
   if (!vr || !vc || !vl){
      printf(" You do not have enough memory to create vwinds.\n");
      if (vr){
         free(vr);
      }
      if (vc){
         free(vc);
      }
      if (vl){
         free(vl);
      }
      deallocate( ctx, ugrid, -1 );
      deallocate( ctx, vgrid, -1 );
      if (wvar>-1){
        deallocate( ctx, wgrid, -1 );
      }
      return;
   }

   drow = (int) (1.0 / density);     /* in slice coords */
   dr = (r2-r1) / (float) (cols-1);  /* delta row and column in */
   dc = (c2-c1) / (float) (cols-1);  /* 3-d grid coords */

   /* "upward" vector */
   up[0] = dc;  up[1] = dr;  up[2] = 0.0;
   NORMALIZE( up );

   /* Compute vectors */
   vcount = 0;
   boxy = dtx->Xmax-dtx->Xmin;
   for (row=0; row<rows && vcount+40<MAX_WIND_VERTS; row+=drow) {
      float gr = (float) r1;
      float gc = (float) c1;
      float gl = (float) (row + dtx->LowLev);
      for (col=0; col<cols && vcount+40<MAX_WIND_VERTS; col++) {
         float u, v, w;
         float dir[3];  /* wind vector direction */
         float a[3];  /* up & across vectors */
         float pr, pc, pl, len, alen;
         float tr, tc, tl;
         float br[2], bc[2], bl[2], cr[2], cc[2], cl[2];
         float qr[2], qc[2], ql[2], zr[2], zc[2], zl[2], aa[3];

         u = ugrid[row*cols+col];
         v = vgrid[row*cols+col];
         w = (wvar>-1) ? wgrid[row*cols+col] : 0.0;
         if (IS_MISSING(u) || IS_MISSING(v) || IS_MISSING(w)) {
            /* if u, v, or w is missing, draw no vector */
         }
         else {
           float newlen, oldlen, factor;
           float grC, gcC, glC;

           oldlen = sqrt (u * u + v * v + w * w);

           /* make wind arrows parallel to trajectories */
           gridPRIME_to_grid(ctx, displaytime, uvar, 1, &gr, &gc, &gl,
                             &grC, &gcC, &glC);
           if ((int) (grC) < 0.0 || (int) (grC) > ctx->Nr ||
               (int) (gcC) < 0.0 || (int) (gcC) > ctx->Nc ||
               (int) (glC) < 0.0 || (int) (glC) > ctx->Nl[uvar]){
              /* don't draw it then ! */
           }
           else{
             dir[0] = v * ctx->Vscale[(int) grC][(int) gcC];
             dir[1] = u * ctx->Uscale[(int) grC][(int) gcC];
             dir[2] = w * ctx->Wscale[(int) glC];

             len = MAGNITUDE( dir );
             arrowr[0] = gr;
             arrowc[0] = gc;
             arrowl[0] = gl;

             arrowr[1] = gr + dir[0];
             arrowc[1] = gc + dir[1];
             arrowl[1] = gl + dir[2];

             /* get xyz arrow */
             grid_to_xyz( ctx, time, uvar, 2, arrowr, arrowc,
                                    arrowl, arrowx, arrowy, arrowz);

             dir[0] = arrowx[1] - arrowx[0];
             dir[1] = arrowy[1] - arrowy[0];
             dir[2] = arrowz[1] - arrowz[0];

             newlen = MAGNITUDE( dir );
             factor = (newlen > 0.0000001) ? oldlen / newlen :
                                             oldlen / 0.0000001;

             /* multiply vector by factor in graphics space */
             factor = (factor / 25.0) * (boxy * 0.03) * scale;

             dir[0] *= factor;
             dir[1] *= factor;
             dir[2] *= factor;

             len = MAGNITUDE( dir );

             /* get small unit vector! */
             arrowx[1] = arrowx[0] + (dir[0]/(len*100));
             arrowy[1] = arrowy[0] + (dir[1]/(len*100));
             arrowz[1] = arrowz[0] + (dir[2]/(len*100));

             xyz_to_geo( ctx, time, uvar, arrowx[0], arrowy[0],
                            arrowz[0], &jlat[0], &jlon[0], &jhgt[0]);
             xyz_to_geo( ctx, time, uvar, arrowx[1], arrowy[1],
                            arrowz[1], &jlat[1], &jlon[1], &jhgt[1]);

             geo_to_xyzPRIME(dtx, displaytime, uvar, 2, jlat, jlon,
                                  jhgt, jx, jy, jz);

             /* now get vector in PRIME space */
             arrowx[0] = jx[0];
             arrowy[0] = jy[0];
             arrowz[0] = jz[0];

             arrowx[1] = jx[0] + 100*len*(jx[1] - jx[0]);
             arrowy[1] = jy[0] + 100*len*(jy[1] - jy[0]);
             arrowz[1] = jz[0] + 100*len*(jz[1] - jz[0]);

             dir[0] = arrowx[1] - arrowx[0];
             dir[1] = arrowy[1] - arrowy[0];
             dir[2] = arrowz[1] - arrowz[0];

             len = MAGNITUDE( dir );

             if ((dir[1]>len/2.0 || dir[1]<-len/2.0) && dtx->WindBarbs == 0) {
               up[0] = 1.0;  up[1] = 0.0;  up[2] = 0.0;
             }
             else{
               up[0] = 0.0;  up[1] = 1.0;  up[2] = 0.0;
             }

             /* create arrow head in graphics space too!.. */
             /* this gets rid of demented arrow head sizes */
             /* when ratio of row, cols, levs is wacky*/
             CROSS( a, dir, up );
             alen = MAGNITUDE( a );

/* MJK 3.25.99 */            
             if (alen * len * 0.1 == 0.0){            
                a[0] = 0.0;            
                a[1] = 0.0;            
                a[2] = 0.0;            
             }            
             else{            
                a[0] = a[0] / alen * len * 0.1;
                a[1] = a[1] / alen * len * 0.1;
                a[2] = a[2] / alen * len * 0.1;
             }

             px = arrowx[1] - dir[0] * 0.2;
             py = arrowy[1] - dir[1] * 0.2;
             pz = arrowz[1] - dir[2] * 0.2;

             arrowx[2] = px + a[0];
             arrowy[2] = py + a[1];
             arrowz[2] = pz + a[2];
             arrowx[3] = px - a[0];
             arrowy[3] = py - a[1];
             arrowz[3] = pz - a[2];

             xyzPRIME_to_gridPRIME( dtx, time, uvar, arrowx[0], arrowy[0],
                                        arrowz[0], &arrowr[0], &arrowc[0], &arrowl[0]);
             xyzPRIME_to_gridPRIME( dtx, time, uvar, arrowx[1], arrowy[1],
                                        arrowz[1], &arrowr[1], &arrowc[1], &arrowl[1]);
             xyzPRIME_to_gridPRIME( dtx, time, uvar, arrowx[2], arrowy[2],
                                        arrowz[2], &arrowr[2], &arrowc[2], &arrowl[2]);
             xyzPRIME_to_gridPRIME( dtx, time, uvar, arrowx[3], arrowy[3],
                                        arrowz[3], &arrowr[3], &arrowc[3], &arrowl[3]);

             if (dtx->WindBarbs == 0) {
                if (len>=0.001) {
                   vr[vcount] = arrowr[0];
                   vc[vcount] = arrowc[0];
                   vl[vcount] = arrowl[0];

                   vr[vcount+1] = arrowr[1];
                   vc[vcount+1] = arrowc[1];
                   vl[vcount+1] = arrowl[1];

                   vr[vcount+2] = arrowr[2];
                   vc[vcount+2] = arrowc[2];
                   vl[vcount+2] = arrowl[2];

                   vr[vcount+3] = arrowr[3];
                   vc[vcount+3] = arrowc[3];
                   vl[vcount+3] = arrowl[3];

                   vcount+=4;
                   /* Used to debug R8000 -O2/O3 bug:
                   if (vcount==12) {
                      int ii;
                      printf("\n");
                      for (ii=0;ii<12;ii++) {
                         printf("%g %g %g\n", vr[ii], vc[ii], vl[ii] );
                      }
                      printf("  %g %g %g  %g %g %g\n", pr, pc, pl, a[0], a[1], a[2] );
                   }*/
                }
             }
             else{
                dir[0] = arrowr[1] - arrowr[0];
                dir[1] = arrowc[1] - arrowc[0];
                dir[2] = arrowl[1] - arrowl[0];

                len = MAGNITUDE( dir );


               make_barb(dtx, u, v, w, dir, up, arrowr[0], arrowc[0], arrowl[0],
                        vr, vc, vl, drow, &vcount);
             }
           }
         } /* end if U, V & W not MISSING */
         gr += dr;
         gc += dc;
       }  /*for col*/
   } /* for row */

   /* deallocate 2-D slices */
   deallocate( ctx, ugrid, -1 );
   deallocate( ctx, vgrid, -1 );
   if (wvar>-1){
     deallocate( ctx, wgrid, -1 );
   }

   /*
    * Bounding rectangle
    */
   numboxverts = make_vertical_rectangle( ctx, time, uvar, dtx->CurvedBox,
                                          r1, c1, r2, c2, cols, &boxverts );

   /*************************** Compress *******************************/

   if (vcount>0) {
      int bytes = 3*vcount*sizeof(int_2);
      cverts = (int_2 *) allocate_type( ctx, bytes, WINDXV_TYPE );
      if (!cverts) {
         deallocate( ctx, cverts, bytes);
         cverts = NULL;
         vcount = 0;
      }
      else {
         gridPRIME_to_compXYZPRIME( dtx, time, uvar, vcount, vr, vc, vl, (void*)cverts );
      }
   }
   else {
      vcount = 0;
      cverts= NULL;
   }

   /************************ Store wind slice ***************************/
   recent( ctx, VWIND, ws );

   wait_write_lock( &dtx->VWindTable[ws][time].lock );

   /* deallocate existing slice, if any */
   free_vwind( dtx, time, ws );

   /* store new slice */
   dtx->VWindTable[ws][time].uvar = dtx->Uvar[ws];
   dtx->VWindTable[ws][time].vvar = dtx->Vvar[ws];
   dtx->VWindTable[ws][time].wvar = dtx->Wvar[ws];
   dtx->VWindTable[ws][time].uvarowner = dtx->Uvarowner[ws];
   dtx->VWindTable[ws][time].vvarowner = dtx->Vvarowner[ws];
   dtx->VWindTable[ws][time].wvarowner = dtx->Wvarowner[ws];
   dtx->VWindTable[ws][time].r1 = r1;
   dtx->VWindTable[ws][time].c1 = c1;
   dtx->VWindTable[ws][time].r2 = r2;
   dtx->VWindTable[ws][time].c2 = c2;
   dtx->VWindTable[ws][time].density = density;
   dtx->VWindTable[ws][time].scale = scale;
   dtx->VWindTable[ws][time].nvectors = vcount;   /* 4 vertices / vector */
   dtx->VWindTable[ws][time].verts = cverts;
   dtx->VWindTable[ws][time].numboxverts = numboxverts;
   dtx->VWindTable[ws][time].boxverts = boxverts;
   dtx->VWindTable[ws][time].valid = 1;
   dtx->VWindTable[ws][time].barbs = dtx->WindBarbs;
   dtx->VWindTable[ws][time].uvarowner = ctx->context_index;
   done_write_lock( &dtx->VWindTable[ws][time].lock );

   if (time==ctx->CurTime) {
      dtx->Redraw = 1;
   }
   free(vr);
   free(vc);
   free(vl);
}





/*
 * Compute streamlines on a horizontal slice.
 * Input:  displaytime - which timestep
 *         ws - which wind slice [0,WINDSLICES-1]
 *         level - grid level of slice in [0,Nl-1]
 *         density - user density factor  1.0, 0.5, 0.25, etc.
 *         threadnum - which thread
 */
static void calc_hstreamslice(Display_Context dtx, int displaytime, int ws,
                              float level, float density, int threadnum )
{
   Context ctx;
   float *grid, *ugrid, *vgrid;
   float mpcol, mprow, mplev;   /* mp = meters per */
   float colscale, rowscale, levscale;
   float midlat, midlon;
   int time;
   int i, row, col, drow, dcol, num;
   int nr, nc, nl, ir, ic, il;
   float deltatime;
   float *vr, *vc, *vl;   
   int_2 *cverts;
   int uvar, vvar;
   float *boxverts;
   int numboxverts;
   float ctxlevel;


   /* Determine which variables to use for U,V,W */
   uvar = dtx->Uvar[ws];
   vvar = dtx->Vvar[ws];


   ctx = dtx->ctxpointerarray[return_ctx_index_pos(dtx, dtx->Uvarowner[ws])];
   if (!ctx){
      printf("error in getting ctx in calc_hstreamslice\n");
   }

   ctxlevel = gridlevelPRIME_to_gridlevel( ctx, level);
   time = dtx->TimeStep[displaytime].ownerstimestep[return_ctx_index_pos(dtx, ctx->context_index)];
   if (displaytime > 0){
      int timebefore;
      timebefore = dtx->TimeStep[displaytime-1].ownerstimestep[
                   return_ctx_index_pos(dtx, ctx->context_index)];
      if (time == timebefore){
         return;
      }
   }

   if (uvar<0 || vvar<0) {
      /* no wind variables specified */
      return;
   }

   nr = ctx->Nr;
   nc = ctx->Nc;

   /* Get U, V 2-D grids */
   grid = get_grid( ctx, time, uvar );
   if (!grid) return;


   /* MJK 12.04.98 */
   if (ctx->dpy_ctx->DisplaySfcHStream[ws]){
      ugrid = extract_sfc_slice (ctx, time, uvar, ctx->Nr, ctx->Nc, grid, 0);
   }
   else{
      ugrid = extract_hslice( ctx, grid, uvar, ctx->Nr, ctx->Nc, ctx->Nl[uvar],
                              ctx->LowLev[uvar], level, 0 );
   }

   release_grid( ctx, time, uvar, grid );

   grid = get_grid( ctx, time, vvar );
   if (!grid) return;


   /* MJK 12.04.98 */   
   if (ctx->dpy_ctx->DisplaySfcHStream[ws]){   
      vgrid = extract_sfc_slice (ctx, time, vvar, ctx->Nr, ctx->Nc, grid, 0);
   }
   else{
      vgrid = extract_hslice( ctx, grid, vvar, ctx->Nr, ctx->Nc, ctx->Nl[vvar],
                              ctx->LowLev[vvar], level, 0 );
   }

   release_grid( ctx, time, vvar, grid );

   vr = (float *) malloc(sizeof(float)*MAX_WIND_VERTS);
   vc = (float *) malloc(sizeof(float)*MAX_WIND_VERTS);
   vl = (float *) malloc(sizeof(float)*MAX_WIND_VERTS);
   if (!vr || !vc || !vl){
      printf(" You do not have enough memory to create hstreams.\n");
      if (vr){
         free(vr);
      }
      if (vc){
         free(vc);
      }
      if (vl){
         free(vl);
      }
      deallocate( ctx, ugrid, -1 );
      deallocate( ctx, vgrid, -1 );
      return;
   }

   for (ir=0; ir<nr; ir++) {
     for (ic=0; ic<nc; ic++) {
       ugrid[ir * nc + ic] *= ctx->Uscale[ir][ic];
       vgrid[ir * nc + ic] *= ctx->Vscale[ir][ic];
     }
   }

   /* call contouring routine */
   stream( ctx, ugrid, vgrid, nr, nc, density,
            vr, vc, MAX_WIND_VERTS, &num);

   for(i=0; i<num; i++) vl[i] = ctxlevel;

   /* deallocate 2-D grids */
   deallocate( ctx, ugrid, -1 );
   deallocate( ctx, vgrid, -1 );

   /*
    * Bounding rectangle
    */
   numboxverts = make_horizontal_rectangle( ctx, time, uvar, dtx->CurvedBox,
                                            level, &boxverts );

   /************************ Compress ********************************/
   /*
    * Transform vertices from grid coordinates in [0,Nr][0,Nc][0,Nl] to
    * compressed graphics coordinates in [-10000,10000]^3.
    */

   /* MJK 12.04.98 */
   if (ctx->dpy_ctx->DisplaySfcHStream[ws]){
      num = fit_vecs_to_topo (ctx, num, MAX_WIND_VERTS, vr, vc, vl);
   }


   if (num > 0) {
      int bytes = 3 * num * sizeof(int_2);
      cverts = (int_2 *) allocate_type( ctx, bytes, STREAM1_TYPE );
      if (cverts) {
         gridPRIME_to_compXYZPRIME( dtx, time, uvar, num, vr, vc, vl, (void*)cverts );
      }
      else {
         deallocate( ctx, cverts, bytes);
         num = 0;
         cverts = NULL;
      }
   }
   else {
      num = 0;
      cverts = NULL;
   }

   /************************* Store wind slice ***********************/
   recent( ctx, HSTREAM, ws );

   wait_write_lock( &dtx->HStreamTable[ws][time].lock );

   /* deallocate existing slice, if any */

   free_hstream( dtx, time, ws );

   /* store new slice */
   dtx->HStreamTable[ws][time].uvar = dtx->Uvar[ws];
   dtx->HStreamTable[ws][time].vvar = dtx->Vvar[ws];
   dtx->HStreamTable[ws][time].uvarowner = dtx->Uvarowner[ws];
   dtx->HStreamTable[ws][time].vvarowner = dtx->Vvarowner[ws];
   dtx->HStreamTable[ws][time].level = level;
   dtx->HStreamTable[ws][time].density = density;
   dtx->HStreamTable[ws][time].nlines = num;
   dtx->HStreamTable[ws][time].verts = cverts;
   dtx->HStreamTable[ws][time].boxverts = boxverts;
   dtx->HStreamTable[ws][time].numboxverts = numboxverts;
   dtx->HStreamTable[ws][time].valid = 1;
   dtx->HStreamTable[ws][time].uvarowner = ctx->context_index;

   done_write_lock( &dtx->HStreamTable[ws][time].lock );

   if (time==ctx->CurTime) {
      dtx->Redraw = 1;
   }
   free(vr);
   free(vc);
   free(vl);
}

static void calc_hstreamslicePRIME(Display_Context dtx, int displaytime, int ws,
                              float level, float density, int threadnum )
{
   Context ctx;
   float *grid, *ugrid, *vgrid;
   float mpcol, mprow, mplev;   /* mp = meters per */
   float colscale, rowscale, levscale;
   float midlat, midlon;
   int time;
   int i, row, col, drow, dcol, num;
   int nr, nc, nl, ir, ic, il;
   float deltatime;
   float *vr, *vc, *vl;         
   float *vr2, *vc2, *vl2;
   int_2 *cverts;
   int uvar, vvar;
   float *boxverts;
   int numboxverts;
   float ctxlevel;


   /* Determine which variables to use for U,V,W */
   uvar = dtx->Uvar[ws];
   vvar = dtx->Vvar[ws];


   ctx = dtx->ctxpointerarray[return_ctx_index_pos(dtx, dtx->Uvarowner[ws])];
   if (!ctx){
      printf("error in getting ctx in calc_hstreamslice\n");
   }

   ctxlevel = gridlevelPRIME_to_gridlevel( ctx, level);
   time = dtx->TimeStep[displaytime].ownerstimestep[return_ctx_index_pos(dtx, ctx->context_index)];
   if (displaytime > 0){
      int timebefore;
      timebefore = dtx->TimeStep[displaytime-1].ownerstimestep[
                   return_ctx_index_pos(dtx, ctx->context_index)];
      if (time == timebefore){
         return;
      }
   }

   if (uvar<0 || vvar<0) {
      /* no wind variables specified */
      return;
   }

   nr = ctx->Nr;
   nc = ctx->Nc;

   /* Get U, V 2-D grids */
   grid = get_grid( ctx, time, uvar );
   if (!grid) return;


   /* MJK 12.04.98 */
   if (ctx->dpy_ctx->DisplaySfcHStream[ws]){
      ugrid = extract_sfc_slice (ctx, time, uvar, ctx->Nr, ctx->Nc, grid, 0);
   }
   else{
      ugrid = extract_hslice( ctx, grid, uvar, ctx->Nr, ctx->Nc, ctx->Nl[uvar],
                              ctx->LowLev[uvar], ctxlevel, 0 );
   }


   release_grid( ctx, time, uvar, grid );

   grid = get_grid( ctx, time, vvar );
   if (!grid) return;


   /* MJK 12.04.98 */
   if (ctx->dpy_ctx->DisplaySfcHStream[ws]){
      vgrid = extract_sfc_slice (ctx, time, vvar, ctx->Nr, ctx->Nc, grid, 0);
   }
   else{
      vgrid = extract_hslice( ctx, grid, vvar, ctx->Nr, ctx->Nc, ctx->Nl[vvar],
                              ctx->LowLev[vvar], ctxlevel, 0 );
   }

   release_grid( ctx, time, vvar, grid );

   vr = (float *) malloc(sizeof(float)*MAX_WIND_VERTS);
   vc = (float *) malloc(sizeof(float)*MAX_WIND_VERTS);
   vl = (float *) malloc(sizeof(float)*MAX_WIND_VERTS);
   vr2 = (float *) malloc(sizeof(float)*MAX_WIND_VERTS);
   vc2 = (float *) malloc(sizeof(float)*MAX_WIND_VERTS);
   vl2 = (float *) malloc(sizeof(float)*MAX_WIND_VERTS);
   if (!vr || !vc || !vl || !vr2 || !vc2 || !vl2){
      printf(" You do not have enough memory to create hstreams.\n");
      if (vr){
         free(vr);
      }
      if (vc){
         free(vc);
      }
      if (vl){
         free(vl);
      }
      if (vr2){
         free(vr2);
      }
      if (vc2){
         free(vc2);
      }
      if (vl2){
         free(vl2);
      }
      deallocate( ctx, ugrid, -1 );
      deallocate( ctx, vgrid, -1 );
      return;
   }


   for (ir=0; ir<nr; ir++) {
     for (ic=0; ic<nc; ic++) {
       ugrid[ir * nc + ic] *= ctx->Uscale[ir][ic];
       vgrid[ir * nc + ic] *= ctx->Vscale[ir][ic];
     }
   }

   /* call contouring routine */
   stream( ctx, ugrid, vgrid, nr, nc, density,
            vr, vc, MAX_WIND_VERTS, &num);

   for(i=0; i<num; i++) vl[i] = ctxlevel;

   /* deallocate 2-D grids */
   deallocate( ctx, ugrid, -1 );
   deallocate( ctx, vgrid, -1 );

   /*
    * Bounding rectangle
    */
   numboxverts = make_horizontal_rectangle( ctx, time, uvar, dtx->CurvedBox,
                                            level, &boxverts );

   /************************ Compress ********************************/
   /*
    * Transform vertices from grid coordinates in [0,Nr][0,Nc][0,Nl] to
    * compressed graphics coordinates in [-10000,10000]^3.
    */


   /* MJK 12.04.98 */
   if (ctx->dpy_ctx->DisplaySfcHStream[ws]){
      num = fit_vecs_to_topo (ctx, num, MAX_WIND_VERTS, vr, vc, vl);
   }

   if (num > 0) {
      int bytes = 3 * num * sizeof(int_2);
      cverts = (int_2 *) allocate_type( ctx, bytes, STREAM1_TYPE );
      if (cverts) {
         grid_to_gridPRIME( ctx, time, uvar, num,vr,vc,vl,vr2, vc2, vl2);
         gridPRIME_to_compXYZPRIME( dtx, time, uvar, num, vr2, vc2, vl2, (void*)cverts );
      }
      else {
         deallocate( ctx, cverts, bytes);
         num = 0;
         cverts = NULL;
      }
   }
   else {
      num = 0;
      cverts = NULL;
   }

   /************************* Store wind slice ***********************/
   recent( ctx, HSTREAM, ws );

   wait_write_lock( &dtx->HStreamTable[ws][time].lock );

   /* deallocate existing slice, if any */

   free_hstream( dtx, time, ws );

   /* store new slice */
   dtx->HStreamTable[ws][time].uvar = dtx->Uvar[ws];
   dtx->HStreamTable[ws][time].vvar = dtx->Vvar[ws];
   dtx->HStreamTable[ws][time].uvarowner = dtx->Uvarowner[ws];
   dtx->HStreamTable[ws][time].vvarowner = dtx->Vvarowner[ws];
   dtx->HStreamTable[ws][time].level = level;
   dtx->HStreamTable[ws][time].density = density;
   dtx->HStreamTable[ws][time].nlines = num;
   dtx->HStreamTable[ws][time].verts = cverts;
   dtx->HStreamTable[ws][time].boxverts = boxverts;
   dtx->HStreamTable[ws][time].numboxverts = numboxverts;
   dtx->HStreamTable[ws][time].valid = 1;
   dtx->HStreamTable[ws][time].uvarowner = ctx->context_index;

   done_write_lock( &dtx->HStreamTable[ws][time].lock );

   if (time==ctx->CurTime) {
      dtx->Redraw = 1;
   }
   free(vr);
   free(vc);
   free(vl);
   free(vr2);
   free(vc2);
   free(vl2);

}



/*
 * Compute streamlines on a vertical slice.
 * Input:  displaytime - which timestep
 *         ws - which wind slice [0,WINDSLICES-1]
 *         r1, c1 - row, column of left end of slice
 *         r2, c2 - row, column of right end of slice
 *         density - user density factor  1.0, 0.5, 0.25, etc.
 *         threadnum - which thread
 */
static void calc_vstreamslice( Display_Context dtx, int displaytime, int ws,
                               float r1, float c1, float r2, float c2,
                               float density, int threadnum )
{
   Context ctx;
   int  time;
   float *grid, *ugrid, *vgrid, *wgrid;
   float mpcol, mprow, mplev;   /* mp = meters per */
   float colscale, rowscale, levscale;
   float midlat, midlon;
   int i, row, col, drow, dcol, num;
   int ir, ic, rows, cols;
   float deltatime;
   float *vr, *vc, *vl;
   int_2 *cverts;
   int uvar, vvar, wvar;
   float *boxverts;
   int numboxverts;
   float c, r, cc, rr, lensq;
   float level, leveljunk;


   /* Determine which variables to use for U,V,W */
   uvar = dtx->Uvar[ws];
   vvar = dtx->Vvar[ws];
   wvar = dtx->Wvar[ws];

   ctx = dtx->ctxpointerarray[return_ctx_index_pos(dtx, dtx->Uvarowner[ws])];
   if (!ctx){
      printf("error in getting ctx in calc_vwindslice\n");
   }

   time = dtx->TimeStep[displaytime].ownerstimestep[return_ctx_index_pos(dtx, ctx->context_index)];
   if (displaytime > 0){
      int timebefore;
      timebefore = dtx->TimeStep[displaytime-1].ownerstimestep[
                   return_ctx_index_pos(dtx, ctx->context_index)];
      if (time == timebefore){
         return;
      }
   }


   if (uvar<0 || vvar<0 || wvar<0) {
      /* no wind variables specified */
      return;
   }

   /*if (ctx->LowLev[uvar] != ctx->LowLev[vvar] ||
       ctx->LowLev[uvar] != ctx->LowLev[wvar]) {
      * wind low levels must match *
      return;
   }*/

   /* size of 2-D slice */
   rows = ctx->Nl[uvar];
   cols = MAX(ctx->Nr,ctx->Nc);

   /* WLH 15 Oct 98 */
   if (rows <= 1 || cols <= 1) return;

   /* Get 2-D grids */
   grid = get_grid( ctx, time, uvar );
   if (!grid) return;
   ugrid = extract_vslice( ctx, grid, r1,c1, r2,c2, rows, cols, 0 );
   release_grid( ctx, time, uvar, grid );

   grid = get_grid( ctx, time, vvar );
   if (!grid) return;
   vgrid = extract_vslice( ctx, grid, r1,c1, r2,c2, rows, cols, 0 );
   release_grid( ctx, time, vvar, grid );

   grid = get_grid( ctx, time, wvar );
   if (!grid) return;
   wgrid = extract_vslice( ctx, grid, r1,c1, r2,c2, rows, cols, 0 );
   release_grid( ctx, time, wvar, grid );

   vr = (float *) malloc(sizeof(float)*MAX_WIND_VERTS);
   vc = (float *) malloc(sizeof(float)*MAX_WIND_VERTS);
   vl = (float *) malloc(sizeof(float)*MAX_WIND_VERTS);
   if (!vr || !vc || !vl){
      printf(" You do not have enough memory to create vstreams.\n");
      if (vr){
         free(vr);
      }
      if (vc){
         free(vc);
      }
      if (vl){
         free(vl);
      }
      deallocate( ctx, ugrid, -1 );
      deallocate( ctx, vgrid, -1 );
      deallocate( ctx, wgrid, -1 );
      return;
   }

/* rows in extracted slice bottom to top */
/* cols in extracted slice (c1,c1) to (c2,c2) */
   rr = (r2-r1) / (float) (cols - 1);
   cc = (c2-c1) / (float) (cols - 1);
   for (ic=0; ic<cols; ic++) {
     int ircur, iccur;
     ircur = r1 + ic * rr;
     iccur = c1 + ic * cc;
     for (ir=0; ir<rows; ir++) {
       ugrid[ir * cols + ic] *= ctx->Uscale[ircur][iccur];
       vgrid[ir * cols + ic] *= ctx->Vscale[ircur][iccur];
       wgrid[ir * cols + ic] *= ctx->Wscale[ir + ctx->LowLev[uvar]];
     }
   }

   r = r2 - r1;
   c = c2 - c1;
   lensq = r * r + c * c;
   if (lensq > 0.0000001) {
     r = r / lensq;
     c = c / lensq;
   }
   r = r * cols;
   c = c * cols;

   for (ir=0; ir<rows; ir++) {
     for (ic=0; ic<cols; ic++) {
       /* U+ east, V+ north, W+ up */
       /* (U, V) is projected onto slice plane and becomes ugrid */
       ugrid[ir * cols + ic] =
         c * ugrid[ir * cols + ic] + r * vgrid[ir * cols + ic];
       /* W takes the role of vgrid, except for the flip in row ordering
          i.e., rows increase north to south, but new rows increase
          bottom to top, SO, change sign of W and call it vgrid */
       vgrid[ir * cols + ic] = wgrid[ir * cols + ic];
     }
   }


   /* call contouring routine */
   stream( ctx, ugrid, vgrid, rows, cols, density,
            vr, vc, MAX_WIND_VERTS, &num);


   /* transform vr, vc to vr, vc, vl */
   for(i=0; i<num; i++) {
     vl[i] = ctx->LowLev[uvar] + vr[i];
     vr[i] = r1 + vc[i] * rr;
     vc[i] = c1 + vc[i] * cc;
   }

   /* deallocate 2-D grids */
   deallocate( ctx, ugrid, -1 );
   deallocate( ctx, vgrid, -1 );
   deallocate( ctx, wgrid, -1 );

   /*
    * Bounding rectangle
    */
   numboxverts = make_vertical_rectangle( ctx, time, uvar, dtx->CurvedBox,
                                          r1, c1, r2, c2, cols, &boxverts );

   /************************ Compress ********************************/
   /*
    * Transform vertices from grid coordinates in [0,Nr][0,Nc][0,Nl] to
    * compressed graphics coordinates in [-10000,10000]^3.
    */

   if (num > 0) {
      int bytes = 3 * num * sizeof(int_2);
      cverts = (int_2 *) allocate_type( ctx, bytes, STREAM1_TYPE );
      if (cverts) {
         gridPRIME_to_compXYZPRIME( dtx, time, uvar, num, vr, vc, vl, (void*)cverts );
      }
      else {
         deallocate( ctx, cverts, bytes);
         num = 0;
         cverts = NULL;
      }
   }
   else {
      num = 0;
      cverts = NULL;
   }

   /************************* Store wind slice ***********************/
   recent( ctx, VSTREAM, ws );

   wait_write_lock( &dtx->VStreamTable[ws][time].lock );

   /* deallocate existing slice, if any */
   free_vstream( dtx, time, ws );

   /* store new slice */
   dtx->VStreamTable[ws][time].uvar = dtx->Uvar[ws];
   dtx->VStreamTable[ws][time].vvar = dtx->Vvar[ws];
   dtx->VStreamTable[ws][time].wvar = dtx->Wvar[ws];
   dtx->VStreamTable[ws][time].uvarowner = dtx->Uvarowner[ws];
   dtx->VStreamTable[ws][time].vvarowner = dtx->Vvarowner[ws];
   dtx->VStreamTable[ws][time].wvarowner = dtx->Wvarowner[ws];
   dtx->VStreamTable[ws][time].r1 = r1;
   dtx->VStreamTable[ws][time].c1 = c1;
   dtx->VStreamTable[ws][time].r2 = r2;
   dtx->VStreamTable[ws][time].c2 = c2;
   dtx->VStreamTable[ws][time].density = density;
   dtx->VStreamTable[ws][time].nlines = num;
   dtx->VStreamTable[ws][time].verts = cverts;
   dtx->VStreamTable[ws][time].boxverts = boxverts;
   dtx->VStreamTable[ws][time].numboxverts = numboxverts;
   dtx->VStreamTable[ws][time].valid = 1;
   dtx->VStreamTable[ws][time].uvarowner = ctx->context_index;

   done_write_lock( &dtx->VStreamTable[ws][time].lock );

   if (time==ctx->CurTime) {
      dtx->Redraw = 1;
   }
   free(vr);
   free(vc);
   free(vl);
}

static void calc_vstreamslicePRIME( Display_Context dtx, int displaytime, int ws,
                               float r1, float c1, float r2, float c2,
                               float density, int threadnum )
{
   Context ctx;
   int  time;
   float *grid, *ugrid, *vgrid, *wgrid;
   float mpcol, mprow, mplev;   /* mp = meters per */
   float colscale, rowscale, levscale;
   float midlat, midlon;
   int i, row, col, drow, dcol, num;
   int ir, ic, rows, cols;
   float deltatime;
   float *vr, *vc, *vl;      
   int_2 *cverts;
   int uvar, vvar, wvar;
   float *boxverts;
   int numboxverts;
   float u, v, w, c, r, ccc, rrr, lensq;
   float len, dir[3], br[2], bc[2], bl[2], cr[2], cc[2], cl[2],
         aa[3], zr[2], zc[2], zl[2], gr, gc, gl, grC, gcC, glC;


   /* Determine which variables to use for U,V,W */
   uvar = dtx->Uvar[ws];
   vvar = dtx->Vvar[ws];
   wvar = dtx->Wvar[ws];

   ctx = dtx->ctxpointerarray[return_ctx_index_pos(dtx, dtx->Uvarowner[ws])];
   if (!ctx){
      printf("error in getting ctx in calc_vwindslice\n");
   }

   time = dtx->TimeStep[displaytime].ownerstimestep[return_ctx_index_pos(dtx, ctx->context_index)];   if (displaytime > 0){
      int timebefore;
      timebefore = dtx->TimeStep[displaytime-1].ownerstimestep[
                   return_ctx_index_pos(dtx, ctx->context_index)];
      if (time == timebefore){
         return;
      }
   }


   if (uvar<0 || vvar<0 || wvar<0) {
      /* no wind variables specified */
      return;
   }


   /* size of 2-D slice */
   rows = dtx->Nl;
   cols = MAX(dtx->Nr,dtx->Nc);

   /* Get 2-D grids */
   grid = get_grid( ctx, time, uvar );
   if (!grid) return;
   ugrid = extract_vslicePRIME( ctx, grid, time, uvar, r1,c1, r2,c2, rows, cols, 0 );
   release_grid( ctx, time, uvar, grid );

   grid = get_grid( ctx, time, vvar );
   if (!grid) return;
   vgrid = extract_vslicePRIME( ctx, grid, time, vvar, r1,c1, r2,c2, rows, cols, 0 );
   release_grid( ctx, time, vvar, grid );

   grid = get_grid( ctx, time, wvar );
   if (!grid) return;
   wgrid = extract_vslicePRIME( ctx, grid, time, wvar, r1,c1, r2,c2, rows, cols, 0 );
   release_grid( ctx, time, wvar, grid );

   vr = (float *) malloc(sizeof(float)*MAX_WIND_VERTS);
   vc = (float *) malloc(sizeof(float)*MAX_WIND_VERTS);
   vl = (float *) malloc(sizeof(float)*MAX_WIND_VERTS);
   if (!vr || !vc || !vl){
      printf(" You do not have enough memory to create vstreams.\n");
      if (vr){
         free(vr);
      }
      if (vc){
         free(vc);
      }
      if (vl){
         free(vl);
      }
      deallocate( ctx, ugrid, -1 );
      deallocate( ctx, vgrid, -1 );
      deallocate( ctx, wgrid, -1 );
      return;
   }

   rrr = (r2-r1) / (float) (cols - 1);
   ccc = (c2-c1) / (float) (cols - 1);
   for (ic=0; ic<cols; ic++) {
      int ircur, iccur;
      ircur = r1 + ic * rrr;
      iccur = c1 + ic * ccc;
      for (ir=0; ir<rows; ir++) {
         gr = (float) ircur;
         gc = (float) iccur;
         gl = (float) (ir + dtx->LowLev);

         u = ugrid[ir * cols + ic];
         v = vgrid[ir * cols + ic];
         w = wgrid[ir * cols + ic];
         if (IS_MISSING(u) || IS_MISSING(v) ||
             IS_MISSING(w)){
            ugrid[ir * cols + ic] = MISSING;
            vgrid[ir * cols + ic] = MISSING;
            wgrid[ir * cols + ic] = MISSING;
         }
         else{

            gridPRIME_to_grid(ctx, displaytime, uvar, 1, &gr, &gc, &gl,
                                &grC, &gcC, &glC);
            if ((int) (grC) < 0 || (int) (grC) > ctx->Nr ||
                (int) (gcC) < 0 || (int) (gcC) > ctx->Nc ||
                (int) (glC) < 0 || (int) (glC) > ctx->Nl[uvar]){
               ugrid[ir * cols + ic] = MISSING;
               vgrid[ir * cols + ic] = MISSING;
               wgrid[ir * cols + ic] = MISSING;
            }
            else{
               dir[0] = v * ctx->Vscale[(int) grC][(int) gcC];
               dir[1] = u * ctx->Uscale[(int) grC][(int) gcC];
               dir[2] = w * ctx->Wscale[(int) glC];

               len = MAGNITUDE( dir );    /* len's units are grid boxes */

               br[0] = grC;
               bc[0] = gcC;
               bl[0] = glC;
       
               br[1] = grC + (dir[0]/(len*100));
               bc[1] = gcC + (dir[1]/(len*100));
               bl[1] = glC + (dir[2]/(len*100));

               /* convert two points of small unit vector into  */
               /* gridPRIME coordinates */
               grid_to_gridPRIME(ctx, time, uvar, 2, br, bc, bl, cr, cc, cl);
               if (cr[0] < 0.0 || cr[0] > (float) (dtx->Nr) ||
                   cc[0] < 0.0 || cc[0] > (float) (dtx->Nc) ||
                   cl[0] < 0.0 || cl[0] > (float) (dtx->Nl) ||
                   cr[1] < 0.0 || cr[1] > (float) (dtx->Nr) ||
                   cc[1] < 0.0 || cc[1] > (float) (dtx->Nc) ||
                   cl[1] < 0.0 || cl[1] > (float) (dtx->Nl)){
                   ugrid[ir * cols + ic] = MISSING;
                   vgrid[ir * cols + ic] = MISSING;
                   wgrid[ir * cols + ic] = MISSING;
               }
               else{
                  zr[0] = cr[0];
                  zc[0] = cc[0];
                  zl[0] = cl[0];

                  zr[1] = cr[0] + 100*len*(cr[1]-cr[0]);
                  zc[1] = cc[0] + 100*len*(cc[1]-cc[0]);
                  zl[1] = cl[0] + 100*len*(cl[1]-cl[0]);

                  aa[0] = zr[1] - zr[0];
                  aa[1] = zc[1] - zc[0];
                  aa[2] = zl[1] - zl[0];

                  ugrid[ir * cols + ic] = aa[1];
                  vgrid[ir * cols + ic] = aa[0];
                  wgrid[ir * cols + ic] = aa[2];

               }
            }
         }
      }
   }

   c = c2 - c1;
   r = r2 - r1;
   lensq = r * r + c * c;
   if (lensq > 0.0000001) {
     r = r / lensq;
     c = c / lensq;
   }
   r = r * cols;
   c = c * cols;

   for (ir=0; ir<rows; ir++) {
     for (ic=0; ic<cols; ic++) {
       /* U+ east, V+ north, W+ up */
       /* (U, V) is projected onto slice plane and becomes ugrid */
       if ( !IS_MISSING(ugrid[ir * cols + ic]) || 
            !IS_MISSING(vgrid[ir * cols + ic])){
          ugrid[ir * cols + ic] =
            c * ugrid[ir * cols + ic] + r * vgrid[ir * cols + ic];
          /* W takes the role of vgrid, except for the flip in row ordering
             i.e., rows increase north to south, but new rows increase
             bottom to top, SO, change sign of W and call it vgrid */
          vgrid[ir * cols + ic] = wgrid[ir * cols + ic];
/*printf("ugrid[ir(%d) * cols(%d) + ic(%d)] = %f\n", ir, cols, ic, ugrid[ir * cols + ic]);
printf("vgrid[ir(%d) * cols(%d) + ic(%d)] = %f\n", ir, cols, ic, vgrid[ir * cols + ic]); */

        }
     }
   }

   /* call contouring routine */
   stream( ctx, ugrid, vgrid, rows, cols, density,
            vr, vc, MAX_WIND_VERTS, &num);


   /* transform vr, vc to vr, vc, vl */
   for(i=0; i<num; i++) {
     vl[i] = dtx->LowLev + vr[i];
     vr[i] = r1 + vc[i] * rrr;
     vc[i] = c1 + vc[i] * ccc;
   }

   /* deallocate 2-D grids */
   deallocate( ctx, ugrid, -1 );
   deallocate( ctx, vgrid, -1 );
   deallocate( ctx, wgrid, -1 );

   /*
    * Bounding rectangle
    */
   numboxverts = make_vertical_rectangle( ctx, time, uvar, dtx->CurvedBox,
                                          r1, c1, r2, c2, cols, &boxverts );

   /************************ Compress ********************************/
   /*
    * Transform vertices from grid coordinates in [0,Nr][0,Nc][0,Nl] to
    * compressed graphics coordinates in [-10000,10000]^3.
    */

   if (num > 0) {
      int bytes = 3 * num * sizeof(int_2);
      cverts = (int_2 *) allocate_type( ctx, bytes, STREAM1_TYPE );
      if (cverts) {
         gridPRIME_to_compXYZPRIME( dtx, time, uvar, num, vr, vc, vl, (void*)cverts );
      }
      else {
         deallocate( ctx, cverts, bytes);
         num = 0;
         cverts = NULL;
      }
   }
   else {
      num = 0;
      cverts = NULL;
   }

   /************************* Store wind slice ***********************/
   recent( ctx, VSTREAM, ws );

   wait_write_lock( &dtx->VStreamTable[ws][time].lock );

   /* deallocate existing slice, if any */
   free_vstream( dtx, time, ws );

   /* store new slice */
   dtx->VStreamTable[ws][time].uvar = dtx->Uvar[ws];
   dtx->VStreamTable[ws][time].vvar = dtx->Vvar[ws];
   dtx->VStreamTable[ws][time].wvar = dtx->Wvar[ws];
   dtx->VStreamTable[ws][time].uvarowner = dtx->Uvarowner[ws];
   dtx->VStreamTable[ws][time].vvarowner = dtx->Vvarowner[ws];
   dtx->VStreamTable[ws][time].wvarowner = dtx->Wvarowner[ws];
   dtx->VStreamTable[ws][time].r1 = r1;
   dtx->VStreamTable[ws][time].c1 = c1;
   dtx->VStreamTable[ws][time].r2 = r2;
   dtx->VStreamTable[ws][time].c2 = c2;
   dtx->VStreamTable[ws][time].density = density;
   dtx->VStreamTable[ws][time].nlines = num;
   dtx->VStreamTable[ws][time].verts = cverts;
   dtx->VStreamTable[ws][time].boxverts = boxverts;
   dtx->VStreamTable[ws][time].numboxverts = numboxverts;
   dtx->VStreamTable[ws][time].valid = 1;
   dtx->VStreamTable[ws][time].uvarowner = ctx->context_index;

   done_write_lock( &dtx->VStreamTable[ws][time].lock );

   if (time==ctx->CurTime) {
      dtx->Redraw = 1;
   }
   free(vr);
   free(vc);
   free(vl);
}



/*
 * Compute the color table indexes for a colored trajectory.
 * Input:  ctx - the context where the traju var is located
 *         t - the trajectory struct
 *         cvowner - the coloring variable vis5d_ctx index
 *         colorvar - the coloring variable
 */
static void color_traj( Context ctx, struct traj *t, int cvowner, int colorvar )
{
   Context cvctx;
   Display_Context dtx;
   uint_1 *color_indexes;
   int i, n;
   float vscale = 1.0 / VERTEX_SCALE;
   float min, valscale;
   int time;
   int cvtime;

   dtx = ctx->dpy_ctx;

   if (ctx->context_index != dtx->TrajUowner){
      return;
   }

   cvctx = dtx->ctxpointerarray[return_ctx_index_pos(dtx, cvowner)];
   if (!cvctx){
      printf("error in getting cvctx in color_traj\n");
   }


   /* Free old color indexes if any */
   wait_write_lock( &t->lock );
   if (t->colors) {
      deallocate( ctx, t->colors, t->length*sizeof(uint_1) );
   }
   t->colors = NULL;
   t->colorvar = -1;
   done_write_lock( &t->lock );

   if (colorvar!=-1) {
      /* Allocate storage for new color indexes */
      n = t->length;
      color_indexes = allocate( ctx, n*sizeof(uint_1) );
      if (!color_indexes) {
         return;
      }

      min = cvctx->MinVal[colorvar];
      valscale = 1.0F / (cvctx->MaxVal[colorvar] - cvctx->MinVal[colorvar]);

      /* Compute color indexes */
      time = 0;
      for (i=0;i<n;i++) {
         float x, y, z;
         float row, col, lev;
         float val;
         int index;

         x = t->verts[i*3+0] * vscale;
         y = t->verts[i*3+1] * vscale;
         z = t->verts[i*3+2] * vscale;

         cvtime = return_ctx_time(ctx->dpy_ctx, cvctx->context_index, time);
         if (cvctx->GridSameAsGridPRIME){
            xyzPRIME_to_gridPRIME( dtx, 0, dtx->TrajU, x, y, z, &row, &col, &lev );
         }
         else{
            xyzPRIME_to_grid( cvctx, cvtime, dtx->TrajU, x, y, z, &row, &col, &lev );
         }
            
         /* find the timestep which corresponds to this traj pos */
         while (t->start[time]<i && time<dtx->NumTimes-1) {
            time++;
         }
         cvtime = return_ctx_time(ctx->dpy_ctx, cvctx->context_index, time);
         val = interpolate_grid_value( cvctx, cvtime, colorvar, row, col, lev );

         if (IS_MISSING(val) ||
             val < cvctx->MinVal[colorvar] ||
             val > cvctx->MaxVal[colorvar]) {
            color_indexes[i] = 255;
         }
         else {
            int index = (val - min) * valscale * 254.0F;
            color_indexes[i] = (uint_1) index;
         }
      }
   }
   else {
      color_indexes = NULL;
   }

   /* save results */
   wait_write_lock( &t->lock );
   t->colors = color_indexes;
   t->colorvar = colorvar;
   t->colorvarowner = cvowner;
   done_write_lock( &t->lock );
}



/*
 * Recolor a set of trajectores to the variable specified by
 * TrajColorVar[traj_set].
 */
static void recolor_traj_set( Display_Context dtx, int traj_set )
{
   Context uvarctx;
   int i;

   for (i=0;i<dtx->NumTraj;i++) {
      struct traj *t = dtx->TrajTable[i];
      if (t->group==traj_set) {
         if (t->colorvar != dtx->TrajColorVar[traj_set]) {
            uvarctx = dtx->ctxpointerarray[
                      return_ctx_index_pos(dtx, dtx->TrajUowner)];
            color_traj( uvarctx, t, dtx->TrajColorVarOwner[traj_set],
                                    dtx->TrajColorVar[traj_set] );
         }
      }
   }
}



/*
 * Compute a wind trajectory.
 * Input:  row, col, lev - start position in grid coords.
 *         time - start time in [0..NumTimes-1].
 *         id - trajectory id (group) number
 *         ribbon - 1 = make ribbon traj, 0 = make line segment traj
 *         step_mult - integration step size multiplier (default 1.0)
 *         len_mult - trajectory length multiplier (default 1.0)
 *  Output:  list of coordinates and times are saved in TrajTable.
 */
static void calc_traj( Display_Context dtx, float row, float col, float lev,
                       int dtime, int id, int ribbon,
                       float step_mult, float len_mult,
                       int cvowner, int colorvar )
{
   Context ctx;
   int len, i;
   float r,c,l;
   float *vr, *vc, *vl, *vx, *vy, *vz, *nx, *ny, *nz;
   int *tt;
   int vbytes, nbytes;
   int_2 *cverts;
   int_1 *cnorms;
   struct traj *t;
   int time;

   ctx = dtx->ctxpointerarray[return_ctx_index_pos(dtx, dtx->TrajUowner)];
   if (!ctx){
      printf("error in getting ctx in calc_traj\n");
   }
   time = dtx->TimeStep[dtime].ownerstimestep[return_ctx_index_pos(dtx, dtx->TrajUowner)];
   if (dtx->NumTraj>=MAXTRAJ) {
      /* out of trajectory space */
      return;
   }

   if (Debug){
      printf("calc_traj( %f %f %f %d %d )\n", row, col, lev, time, id );
   }

   vr = (float *) malloc(sizeof(float)*MAX_TRAJ_VERTS);
   vc = (float *) malloc(sizeof(float)*MAX_TRAJ_VERTS);
   vl = (float *) malloc(sizeof(float)*MAX_TRAJ_VERTS);

/* WLH 20 Oct 98
   vx = (float *) malloc(sizeof(float)*MAX_TRAJ_VERTS);
   vy = (float *) malloc(sizeof(float)*MAX_TRAJ_VERTS);
   vz = (float *) malloc(sizeof(float)*MAX_TRAJ_VERTS);
   nx = (float *) malloc(sizeof(float)*MAX_TRAJ_VERTS);
   ny = (float *) malloc(sizeof(float)*MAX_TRAJ_VERTS);
   nz = (float *) malloc(sizeof(float)*MAX_TRAJ_VERTS);
   tt = (int *) malloc(sizeof(int)*MAX_TRAJ_VERTS);
*/
   /* WLH 20 Oct 98 */
   if (ribbon) {
     vx = (float *) malloc(2 * sizeof(float)*MAX_TRAJ_VERTS);
     vy = (float *) malloc(2 * sizeof(float)*MAX_TRAJ_VERTS);
     vz = (float *) malloc(2 * sizeof(float)*MAX_TRAJ_VERTS);
     nx = (float *) malloc(2 * sizeof(float)*MAX_TRAJ_VERTS);
     ny = (float *) malloc(2 * sizeof(float)*MAX_TRAJ_VERTS);
     nz = (float *) malloc(2 * sizeof(float)*MAX_TRAJ_VERTS);
     tt = (int *) malloc(2 * sizeof(int)*MAX_TRAJ_VERTS);
   }
   else {
     vx = (float *) malloc(sizeof(float)*MAX_TRAJ_VERTS);
     vy = (float *) malloc(sizeof(float)*MAX_TRAJ_VERTS);
     vz = (float *) malloc(sizeof(float)*MAX_TRAJ_VERTS);
     nx = (float *) malloc(sizeof(float)*MAX_TRAJ_VERTS);
     ny = (float *) malloc(sizeof(float)*MAX_TRAJ_VERTS);
     nz = (float *) malloc(sizeof(float)*MAX_TRAJ_VERTS);
     tt = (int *) malloc(sizeof(int)*MAX_TRAJ_VERTS);
   }
  
   if (!vr || !vc || !vl || !vx || !vy || !vz || !nx || !ny || !nz || !tt){
      printf(" You do not have enough memory to create trajectories.\n");
      if (vr){
         free(vr);
      }
      if (vc){
         free(vc);
      }
      if (vl){
         free(vl);
      }
      if (vx){
         free(vx);
      }
      if (vy){
         free(vy);
      }
      if (vz){
         free(vz);
      }
      if (nx){
         free(nx);
      }
      if (ny){
         free(ny);
      }
      if (nz){
         free(nz);
      }
      if (tt){
         free(tt);
      }
      return;
   }
 
   if (ctx->GridSameAsGridPRIME){
      len = trace( ctx, row, col, lev, time, (int) (step_mult * ctx->TrajStep),
                  MAX_TRAJ_VERTS, vr, vc, vl, tt );
   }
   else{
      vis5d_gridPRIME_to_grid( ctx->context_index, dtime, ctx->dpy_ctx->TrajU,
                               row, col, lev, &r, &c, &l);
      len = trace( ctx, r, c, l, time, (int) (step_mult * ctx->TrajStep),
                  MAX_TRAJ_VERTS, vr, vc, vl, tt );
   }

 

   if (len==0){
      free(vr);
      free(vc);
      free(vl);
      free(vx);
      free(vy);
      free(vz);
      free(nx);
      free(ny);
      free(nz);
      free(tt);
      return;
   }

   /* convert coords from grid to graphics */
   if (ctx->GridSameAsGridPRIME){
      gridPRIME_to_xyzPRIME( dtx, time, dtx->TrajU, len, vr, vc, vl,  vx, vy, vz );
   }
   else{
      grid_to_xyzPRIME(ctx, time, dtx->TrajU, len, vr, vc, vl,  vx, vy, vz );
   }  

   if (ribbon) {
      /* convert to ribbon, calc normals */
      len = to_ribbon( len, vx,vy,vz, tt,  nx,ny,nz );
   }
   if (len==0){
      free(vr);
      free(vc);
      free(vl);
      free(vx);
      free(vy);
      free(vz);
      free(nx);
      free(ny);
      free(nz);
      free(tt);
      return;
   }


   /****************************** Compress ***************************/
   vbytes = 3 * len * sizeof(int_2);
   cverts = (int_2 *) allocate_type( ctx, vbytes, TRAJX_TYPE );
   if (cverts) {
      /* convert floats to int_2 */
      for (i=0;i<len;i++) {
         cverts[i*3+0] = (int_2) (vx[i] * VERTEX_SCALE);
         cverts[i*3+1] = (int_2) (vy[i] * VERTEX_SCALE);
         cverts[i*3+2] = (int_2) (vz[i] * VERTEX_SCALE);
      }
   }
   else {
      len = 0;
   }

   if (ribbon & len>0) {
      nbytes = 3 * len * sizeof(int_1);
      cnorms = (int_1 *) allocate_type( ctx, nbytes, TRAJXR_TYPE );
      if (!cnorms) {
         deallocate( ctx, cverts, vbytes );
         cverts = NULL;
         len = 0;
      }
      else {
         /* compress normals to 1-byte ints */
         for (i=0;i<len;i++) {
            cnorms[i*3+0] = (int_1) (int) (-nx[i] * NORMAL_SCALE);
            cnorms[i*3+1] = (int_1) (int) ( ny[i] * NORMAL_SCALE);
            cnorms[i*3+2] = (int_1) (int) ( nz[i] * NORMAL_SCALE);
         }
      }
   }
   else {
      cnorms = NULL;
   }

   /***************************** Store ******************************/

   t = allocate( ctx, sizeof(struct traj) );
   if (!t) {
      free(vr);
      free(vc);
      free(vl);
      free(vx);
      free(vy);
      free(vz);
      free(nx);
      free(ny);
      free(nz);
      free(tt);
      return;
   }
   t->ctx_owner = ctx->context_index; 
   t->lock = 0;
   t->row = row;
   t->col = col;
   t->lev = lev;
   t->timestep = time;
   t->stepmult = step_mult;
   t->lengthmult = len_mult;
   t->length = len;
   t->verts = cverts;
   t->norms = cnorms;
   t->start = (uint_2 *) allocate_type( ctx,
                                 ctx->NumTimes * sizeof(uint_2), START_TYPE );
   t->len = (uint_2 *) allocate_type( ctx,
                                 ctx->NumTimes * sizeof(uint_2), LEN_TYPE );

   /* calculate start and len array values */
   if (len>0) {
      int tlen = (int) (len_mult * ctx->TrajLength);
      for (i=0;i<ctx->NumTimes;i++) {
         int t0, t1, j, startj;

         t0 = ctx->Elapsed[i] - tlen;
         t1 = ctx->Elapsed[i];

         /* find segment of traj extending in time from [t0,t1] */
         j = 0;
/* WLH 20 Oct 98
         while (tt[j]<t0 && j<len)
*/
         /* WLH 20 Oct 98 */
         while (j<len && tt[j]<t0)

           j++;
         if (j>=len) {
            t->start[i] = 0;
            t->len[i] = 0;
         }
         else {
            t->start[i] = startj = j;
            while (tt[j]<=t1 && j<len)
              j++;
            if (j-startj<=1)
              t->len[i] = 0;
            else
              t->len[i] = j - startj;
         }
      }
   }

   t->group = id;
   t->kind = ribbon;
   t->colors = NULL;
   if (colorvar>=0) {
      color_traj( ctx, t, cvowner, colorvar );
   }
   else {
      t->colorvar = -1;
   }
   t->lock = 0;

   LOCK_ON( TrajLock );
   dtx->TrajTable[dtx->NumTraj] = t;
   dtx->NumTraj++;
   LOCK_OFF( TrajLock );

   recent( ctx, TRAJ, id );

   dtx->Redraw = 2;
   free(vr);
   free(vc);
   free(vl);
   free(vx);
   free(vy);
   free(vz);
   free(nx);
   free(ny);
   free(nz);
   free(tt);

}



/*
 * Recolor the topography for the given timestep
 */
/* time = dtx time! */
static void recolor_topography( Context ctx, int time )
{
   int colorvar = ctx->dpy_ctx->TopoColorVar;
   int ctxtime;
   Display_Context dtx;

   dtx = ctx->dpy_ctx;
   ctxtime = dtx->TimeStep[time].ownerstimestep[return_ctx_index_pos(dtx,
                                 ctx->context_index)];
   if (colorvar==-1) {
      /* use default coloring by height */
      LOCK_ON( TrajLock );
      if (ctx->dpy_ctx->TopoIndexes[time]) {
         /* free the vertex color indexes */
         /* YO 10-5-97 potential old stuff
         int bytes = ctx->qrows * ctx->qcols * sizeof(uint_1);
         deallocate( ctx, ctx->TopoIndexes[time], bytes );
         ctx->TopoIndexes[time] = NULL; */

         free(ctx->dpy_ctx->TopoIndexes[time]);
         ctx->dpy_ctx->TopoIndexes[time] = NULL;

      }
      LOCK_OFF( TrajLock );
   }
   else if (ctx->context_index == dtx->TopoColorVarOwner){
      /** if (ctx->Nl[colorvar]==1) **/  
      /* Compute color table indexes for each topography vertex */
      /* for this time step. */
      uint_1 *indexes;
      int bytes;
      int trows, tcols, trow, tcol;
      float bias, scale;
      float *grid;


      /* MJK 12.04.98 */
      scale = 254.0 / (ctx->MaxVal[colorvar] - ctx->MinVal[colorvar]);


      bias = ctx->MinVal[colorvar];

      /* allocate storage for vertex colors */
      /* YO 10-5-97 potential old stuff
      bytes = ctx->qrows * ctx->qcols * sizeof(uint_1);
      indexes = allocate( ctx, bytes ); */

      if (ctx->dpy_ctx->TopoIndexes[time]){
         free(ctx->dpy_ctx->TopoIndexes[time]);
         ctx->dpy_ctx->TopoIndexes[time] = NULL;
      }
      indexes = malloc( ctx->dpy_ctx->qrows * ctx->dpy_ctx->qcols * sizeof(uint_1) );
      if (!indexes){
         printf("You do not have enough memory to color topography.\n");
         return;
      } 

      grid = get_grid( ctx, ctxtime, colorvar );

      trows = ctx->dpy_ctx->qrows;
      tcols = ctx->dpy_ctx->qcols;
      for (trow=0;trow<trows;trow++) {
         for (tcol=0;tcol<tcols;tcol++) {
            float grow, gcol, glev;
            float x, y, z;
            int n;

            n = trow * tcols + tcol;
            x = ctx->dpy_ctx->TopoVertex[n*3+0];
            y = ctx->dpy_ctx->TopoVertex[n*3+1];
            z = ctx->dpy_ctx->TopoVertex[n*3+2];
            xyzPRIME_to_grid( ctx, ctxtime, colorvar, x, y, z,
                         &grow, &gcol, &glev );

            if ( (int) grow < 0 || (int) gcol < 0 ||
                 grow < 0 || grow > ctx->Nr-1 &&
                 gcol < 0 || gcol > ctx->Nc-1 &&
                 glev < 0 || glev > ctx->Nl[colorvar]-1){
               indexes[n] = 255;
            }
            else{
               float value;
               value = interpolate_grid_value(ctx, ctxtime, dtx->TopoColorVar,
                                              grow, gcol, glev);
 
               if (IS_MISSING(value) ||
                   value < ctx->MinVal[colorvar] ||
                   value > ctx->MaxVal[colorvar]){
                  indexes[n] = 255;
               }
               else{
                  /* MJK 12.04.98 */
                  int index = (value - bias) * scale;
                  indexes[n] = (index < 0) ? 0 : (index > 254) ? 254 : index;
               }
            }
         }
      }

      release_grid( ctx, ctxtime, colorvar, grid );

      /* save results */
      LOCK_ON( TrajLock );
      if (ctx->dpy_ctx->TopoIndexes[time]) {
         /* deallocate( ctx, ctx->TopoIndexes[time], bytes ); */
         free( ctx->dpy_ctx->TopoIndexes[time]);
      }
      ctx->dpy_ctx->TopoIndexes[time] = indexes;
      LOCK_OFF( TrajLock );
   }

   if (time==dtx->CurTime) {
      ctx->dpy_ctx->Redraw = 1;
   }
}




/*
 * Call once this to initialize this module.
 */
int init_work( void )
{
   return 0;
}


/*
 * Call this when ready to exit vis5d.
 */
int terminate_work( void )
{
   return 0;
}



/*
 * Return 1 if there are any background tasks pending, else return 0.
 */
int any_work_pending( void )
{
   int size, waiters;

   get_queue_info( &size, &waiters );
   if (size==0 && waiters==NumThreads-1) {
      return 0;
   }
   else {
      return 1;
   }
}



/*
 * Take one job off the queue and do it.
 * Input:  threadnum - thread number
 * Return:  1 = one job done, 0 = time to exit
 */
int do_one_task( int threadnum )
{
   int type;
   int i1, i2, i3;
   float f1, f2, f3, f4, f5;
   float r1, r2, c1, c2, l;
   int time, var, colorvar;
   Context ctx;
   Irregular_Context itx;
   float level2;

   get_qentry( &ctx, &itx, &type, &i1, &i2, &i3, &f1, &f2, &f3, &f4, &f5 );
   if (Debug) printf("got entry: %d\n", type );
   time = i1;
   var = i2;
   if (Debug)  printf("\ntask type=%d\n", type );
   switch (type) {
      case TASK_ISOSURFACE:
         /* compute an isosurface. */
         colorvar = i3;
         if (ctx->SameIsoColorVarOwner[var]){ 
            /* time = ctx time */
            calc_isosurface( ctx, time, var, ctx->IsoLevel[var],
                          ctx->IsoColorVarOwner[var], ctx->IsoColorVar[var], threadnum );
         }
         else{
            Display_Context dtx;
            int dtime, ctime, t;
            dtx = ctx->dpy_ctx;
            for(t=0; t < dtx->NumTimes; t++){
               ctime = dtx->TimeStep[t].ownerstimestep[return_ctx_index_pos(dtx, 
                                                       ctx->context_index)];
               if ( ctime==time){
                  calc_isosurface( ctx, t, var, ctx->IsoLevel[var],
                                   ctx->IsoColorVarOwner[var],
                                   ctx->IsoColorVar[var], threadnum );
               }
            }
         }
         break;
      case TASK_TEXT_PLOT:
         calc_textplot( itx, time, threadnum );
         break;
      case TASK_HSLICE:
         /* calculate a horizontal contour line slice. */
         calc_hslice( ctx, time, var, ctx->HSliceInterval[var],
                      ctx->HSliceLowLimit[var], ctx->HSliceHighLimit[var],
                      ctx->HSliceLevel[var], threadnum );
         break;
      case TASK_VSLICE:
         /* calculate a horizontal contour line slice. */
         calc_vslice( ctx, time, var, ctx->VSliceInterval[var],
                      ctx->VSliceLowLimit[var], ctx->VSliceHighLimit[var],
                      ctx->VSliceR1[var], ctx->VSliceC1[var],
                      ctx->VSliceR2[var], ctx->VSliceC2[var], threadnum );
         break;
      case TASK_CHSLICE:
         /* calculate a horizontal colored slice */
         calc_chslice( ctx, time, var, ctx->CHSliceLevel[var], threadnum );
         break;
      case TASK_CVSLICE:
         /* calculate a vertical colored slice */
         calc_cvslice( ctx, time, var, ctx->CVSliceR1[var], ctx->CVSliceC1[var],
                       ctx->CVSliceR2[var], ctx->CVSliceC2[var], threadnum);
         break;
      case TASK_HWIND:
         /* calculate a horizontal wind slice */
         if (ctx->GridSameAsGridPRIME && ctx->context_index == ctx->dpy_ctx->Uvarowner[var]){
            calc_hwindslice( ctx->dpy_ctx, time, var, ctx->dpy_ctx->HWindLevel[var],
                             ctx->dpy_ctx->HWindScale[var],
                             ctx->dpy_ctx->HWindDensity[var], threadnum );
         }
         else if(ctx->context_index == ctx->dpy_ctx->Uvarowner[var]){
            calc_hwindslicePRIME( ctx->dpy_ctx, time, var, ctx->dpy_ctx->HWindLevel[var],
                             ctx->dpy_ctx->HWindScale[var],
                             ctx->dpy_ctx->HWindDensity[var], threadnum );
         }

         break;
      case TASK_VWIND:
         /* calculate a vertical wind slice */
         if (ctx->GridSameAsGridPRIME && ctx->context_index == ctx->dpy_ctx->Uvarowner[var]){
            calc_vwindslice( ctx->dpy_ctx, time, var, ctx->dpy_ctx->VWindR1[var],
                             ctx->dpy_ctx->VWindC1[var],
                             ctx->dpy_ctx->VWindR2[var], ctx->dpy_ctx->VWindC2[var],
                             ctx->dpy_ctx->VWindScale[var],
                             ctx->dpy_ctx->VWindDensity[var], threadnum );
         }
         else if(ctx->context_index == ctx->dpy_ctx->Uvarowner[var]){
            calc_vwindslicePRIME( ctx->dpy_ctx, time, var, ctx->dpy_ctx->VWindR1[var],
                             ctx->dpy_ctx->VWindC1[var],
                             ctx->dpy_ctx->VWindR2[var], ctx->dpy_ctx->VWindC2[var],
                             ctx->dpy_ctx->VWindScale[var],
                             ctx->dpy_ctx->VWindDensity[var], threadnum );
         }

         break;
      case TASK_HSTREAM:
         /* calculate a horizontal streamline slice */
         if (ctx->GridSameAsGridPRIME && ctx->context_index == ctx->dpy_ctx->Uvarowner[var]){
            calc_hstreamslice( ctx->dpy_ctx, time, var, ctx->dpy_ctx->HStreamLevel[var],
                               ctx->dpy_ctx->HStreamDensity[var], threadnum );
         }
         else{
            calc_hstreamslicePRIME(ctx->dpy_ctx, time, var, ctx->dpy_ctx->HStreamLevel[var],
                               ctx->dpy_ctx->HStreamDensity[var], threadnum );
         }
         break;
      case TASK_VSTREAM:
         /* calculate a vertical streamline slice */
         if (ctx->GridSameAsGridPRIME && ctx->context_index == ctx->dpy_ctx->Uvarowner[var]){
            calc_vstreamslice( ctx->dpy_ctx, time, var,
                               ctx->dpy_ctx->VStreamR1[var], ctx->dpy_ctx->VStreamC1[var],
                               ctx->dpy_ctx->VStreamR2[var], ctx->dpy_ctx->VStreamC2[var],
                               ctx->dpy_ctx->VStreamDensity[var], threadnum );
         }
         else{
            calc_vstreamslicePRIME( ctx->dpy_ctx, time, var,
                               ctx->dpy_ctx->VStreamR1[var], ctx->dpy_ctx->VStreamC1[var],
                               ctx->dpy_ctx->VStreamR2[var], ctx->dpy_ctx->VStreamC2[var],
                               ctx->dpy_ctx->VStreamDensity[var], threadnum );
         }
         break;
      case TASK_HCLIP:
         /* calculate the box for a hclip plane */
         calc_hclip( ctx->dpy_ctx, time, ctx->dpy_ctx->HClipTable[time].level);
         break;
      case TASK_VCLIP:
         /* calculate the box for a vclip plane */
         calc_vclip( ctx->dpy_ctx, time, ctx->dpy_ctx->VClipTable[time].r1,
                     ctx->dpy_ctx->VClipTable[time].c1, 
                     ctx->dpy_ctx->VClipTable[time].r2,
                     ctx->dpy_ctx->VClipTable[time].c2);
         break;
      case TASK_TRAJ:
         /* calculate a wind trajectory */
         calc_traj( ctx->dpy_ctx, f1, f2, f3, time, i2, i3, f4, f5,
                    ctx->dpy_ctx->TrajColorVarOwner[i2],ctx->dpy_ctx->TrajColorVar[i2] );
         break;
      case TASK_TRAJ_RECOLOR:
         recolor_traj_set( ctx->dpy_ctx, i1 );
         break;
      case TASK_TOPO_RECOLOR:
         recolor_topography( ctx, time );
         break;
      case TASK_EXT_FUNC:
         calc_ext_func( ctx, time, var, threadnum );
         break;
      case TASK_QUIT:
         if (Debug) {
            printf("TASK_QUIT\n");
            return 0;
         }
         break;
      case TASK_NULL:
         break;
      default:
         printf("VIS-5D INTERNAL ERROR:  Undefined task code!!\n");
   } /*switch*/

   return 1;
}



/*
 * Multiple instances of this function execute in parallel on the SGI
 * and Stellar.  The function takes entries out of the queue and calls
 * the appropriate function to do the work of computing isosurfaces,
 * contour slices, trajectories, etc.
 * Input:  threadnum - integer thread ID in [1..NumThreads-1]
 */
void work( void *threadnum )
{
#ifdef sgi
   /* make this process die if the parent dies */
   prctl( PR_TERMCHILD );
#endif

   while (do_one_task( (int) threadnum ))
     ;
}
