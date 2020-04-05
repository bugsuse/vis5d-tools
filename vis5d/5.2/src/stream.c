/* stream.c */

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

/* 2-D streamline making function */


#include <math.h>
#include <string.h>
#include "memory.h"
#include "globals.h"
#include "proj.h"


#define GU( R, C )           ( ugrid[ (R) * nc + (C) ] )
#define GV( R, C )           ( vgrid[ (R) * nc + (C) ] )
#define MARKARROW( R, C )    ( markarrow[ (C) * nrstart + (R) ] )
#define MARKSTART( R, C )    ( markstart[ (C) * nrstart + (R) ] )
#define MARKEND( R, C )      ( markend[ (C) * nrend + (R) ] )

#define START2ROW( IRLOW )     ( ((float) nr-1.0) * ((float) (IRLOW)+0.5) / (float) nrstart )
#define START2COL( ICLOW )     ( ((float) nc-1.0) * ((float) (ICLOW)+0.5) / (float) ncstart )
#define ROW2ARROW( ROW )       ( (int) (nrarrow * (ROW) / ((float) nr-1.0) ) )
#define COL2ARROW( COL )       ( (int) (ncarrow * (COL) / ((float) nc-1.0) ) )
#define ROW2START( ROW )       ( (int) (nrstart * (ROW) / ((float) nr-1.0) ) )
#define COL2START( COL )       ( (int) (ncstart * (COL) / ((float) nc-1.0) ) )
#define ROW2END( ROW )        ( (int) (nrend * (ROW) / ((float) nr-1.0) ) )
#define COL2END( COL )        ( (int) (ncend * (COL) / ((float) nc-1.0) ) )

#define LENGTH 0.02



/*
 * trace one stream line for 2-D u and v arrays.
 * Note that the input arrays ugrid & vgrid should be in
 * column-major (FORTRAN) order.
 *
 * Input:  ugrid, vgrid - the 2-D wind arrays.
 *         nr, nc - size of 2-D array in rows and columns.
 *         vr, vc - arrays to put streamline vertices
 *         dir - direction 1.0=forward, -1.0=backward
 *         maxv - size of vx, vy arrays
 *         numv - pointer to int to return number of vertices in vx, vy
 *         markarrow - mark array for arrows
 *         markstart - mark array for starting streamlines
 *         markend - mark array for ending streamlines
 *         nrarrow, ncarrow - size of markarrow
 *         nrstart, ncstart - size of markstart
 *         nrend, ncend - size of markend
 *         row, col - start location for streamline
 *         step - step size for streamline
 *         rowlength, collength - scale constants for arrows
 *         irend, icend - location of most recent end box
 * Return:  1 = ok
 *          0 = no more memory for streamlines
 */
int stream_trace( Context ctx, float ugrid[], float vgrid[], int nr, int nc,
                  float dir, float vr[], float vc[], int maxv, int *numv,
                  char *markarrow, char *markstart, char *markend,
                  int nrarrow, int ncarrow, int nrstart, int ncstart,
                  int nrend, int ncend, float row, float col, float step,
                  float rowlength, float collength, int irend, int icend)
{
  int irstart, icstart, ir, ic, ire, ice;
  int ira, ica, irs, ics;
  int nend, num;
  float prevrow, prevcol;
  float a, c, ac, ad, bc, bd;
  float u, v;

  num = *numv;
  nend = 0;

  while (1) {
    float ubd, ubc, uad, uac, vbd, vbc, vad, vac;
    /* interpolate velocity at row, col */
    ir = row;
    ic = col;
    a = row - ir;
    c = col - ic;
    ac = a*c;
    ad = a*(1.0-c);
    bc = (1.0-a)*c;
    bd = (1.0-a)*(1.0-c);

    ubd = GU(ir, ic);
    ubc = GU(ir, ic+1);
    uad = GU(ir+1, ic);
    uac = GU(ir+1, ic+1);
    vbd = GV(ir, ic);
    vbc = GV(ir, ic+1);
    vad = GV(ir+1, ic);
    vac = GV(ir+1, ic+1);

    /* terminate stream if missing data */
    if (IS_MISSING(ubd) || IS_MISSING(ubc) ||
        IS_MISSING(uad) || IS_MISSING(uac) ||
        IS_MISSING(vbd) || IS_MISSING(vbc) ||
        IS_MISSING(vad) || IS_MISSING(vac)) break;
    u = bd * ubd + bc * ubc +
        ad * uad + ac * uac;
    v = bd * vbd + bc * vbc +
        ad * vad + ac * vac;
/* WLH 5 July 96
    u = bd * GU(ir, ic) + bc * GU(ir, ic+1) +
        ad * GU(ir+1, ic) + ac * GU(ir+1, ic+1);
    v = bd * GV(ir, ic) + bc * GV(ir, ic+1) +
        ad * GV(ir+1, ic) + ac * GV(ir+1, ic+1);
*/

    /* scale velocity */
/* WLH 7-3-96
    u = step * ctx->dpy_ctx->Uscale[ir][ic] * u;
    v = step * ctx->dpy_ctx->Vscale[ir][ic] * v;
*/

    u = step * u;
    v = step * v;
    /* test for too many line segments */
    if (num > maxv-2) {
      deallocate( ctx, markarrow, nrstart * ncstart * sizeof(char) );
      deallocate( ctx, markstart, nrstart * ncstart * sizeof(char) );
      deallocate( ctx, markend, nrend * ncend * sizeof(char) );
      *numv = num;
      return 0;
    }

    /* propogate streamline */
    prevrow = row;
    prevcol = col;
    row += dir * v;
    col += dir * u;
    /* terminate stream if out of grid */
    if (row < 0 || col < 0 || row >= nr-1 || col >= nc-1) {
      break;
    }

    ire = ROW2END(row);
    ice = COL2END(col);

    /* terminate stream if enters marked end box */
    if (ire != irend || ice != icend) {
      irend = ire;
      icend = ice;
      if (irend < 0 || irend >= nrend || icend < 0 || icend >= ncend) {
        printf("bad 2:  irend = %d  icend = %d\n", irend, icend);
      }
      if (MARKEND(irend, icend) == 1) {
        break;
      }
      MARKEND(irend, icend) = 1;
      nend = 0;
    }

    /* terminate stream if too many steps in one end box */
    nend++;
    if (nend > 100) {
      break;
    }

    /* make line segment */
    vr[num] = prevrow;
    vc[num++] = prevcol;
    vr[num] = row;
    vc[num++] = col;
    /* mark start box */
    irs = ROW2START(row);
    ics = COL2START(col);
    if (irs < 0 || irs >= nrstart || ics < 0 || ics >= ncstart) {
      printf("bad 3:  irs = %d  ics = %d\n", irs, ics);
    }
    if (MARKSTART(irs, ics) == 0) {
      MARKSTART(irs, ics) = 1;
    }

    /* check for need to draw arrow head */
    ira = ROW2ARROW(row);
    ica = COL2ARROW(col);
    if (MARKARROW(ira, ica) == 0) {
      double rv, cv, v;
      /* test for too many line segments */
      if (num > maxv-4) {
        deallocate( ctx, markarrow, nrstart * ncstart * sizeof(char) );
        deallocate( ctx, markstart, nrstart * ncstart * sizeof(char) );
        deallocate( ctx, markend, nrend * ncend * sizeof(char) );
        *numv = num;
        return 0;
      }
      MARKARROW(ira, ica) = 1;
      rv = dir * (row - prevrow);
      cv = dir * (col - prevcol);
      v = sqrt(rv*rv + cv*cv);
      if (v > 0.000000001) {
        rv = rv / v;
        cv = cv / v;
      }
      vr[num] = row;
      vc[num++] = col;
      vr[num] = row - (rv + cv) * rowlength;
      vc[num++] = col + (rv - cv) * collength;
      vr[num] = row;
      vc[num++] = col;
      vr[num] = row + (cv - rv) * rowlength;
      vc[num++] = col - (cv + rv) * collength;

    }

  } /* end while (forward) */

  *numv = num;
  return 1;
}



/*
 * Compute stream lines for 2-D u and v arrays.
 * Note that the input arrays ugrid & vgrid should be in
 * row-major order.
 *
 * Input:  ugrid, vgrid - the 2-D wind arrays.
 *         nr, nc - size of 2-D array in rows and columns.
 *         density - relative density of streamlines.
 *         vr, vc - arrays to put streamline vertices
 *         maxv - size of vx, vy arrays
 *         numv - pointer to int to return number of vertices in vx, vy
 * Return:  1 = ok
 *          0 = error  (out of memory)
 */
int stream( Context ctx, float ugrid[], float vgrid[], int nr, int nc,
             float density, float vr[], float vc[],  int maxv, int *numv)
{
  int irstart, icstart, irend, icend, ir, ic, ire, ice;
  int ira, ica, irs, ics;
  int nrarrow, ncarrow, nrstart, ncstart, nrend, ncend;
  int nend, num;
  char *markarrow, *markstart, *markend;
  float row, col, prevrow, prevcol;
  float a, c, ac, ad, bc, bd;
  float u, v, step, rowlength, collength;
  float dir;


  /* initialize vertex counts */
  num = 0;

  /* density calculations */
  if (density < 0.5) density = 0.5;
  if (density > 2.0) density = 2.0;

  nrarrow = 15.0001 * density;
  ncarrow = 15.0001 * density;
  nrstart = 15.0001 * density;
  ncstart = 15.0001 * density;
  nrend = 4 * nrstart;
  ncend = 4 * ncstart;

  rowlength = LENGTH * nr / density;
  collength = LENGTH * nc / density;

  /* WLH - use shorter step for higher density */
  step = ctx->TrajStep / density;

  /* allocate mark arrays */
  markarrow = (char *) allocate( ctx, nrstart * ncstart * sizeof(char) );
  if (!markarrow) return 0;
  markstart = (char *) allocate( ctx, nrstart * ncstart * sizeof(char) );
  if (!markstart) return 0;
  markend = (char *) allocate( ctx, nrend * ncend * sizeof(char) );
  if (!markend) return 0;

  /* initialize mark array to zeros */
  memset( markstart, 0, nrstart * ncstart * sizeof(char) );
  memset( markend, 0, nrend * ncend * sizeof(char) );
  /* only draw arrows in every ninth box */
  memset( markarrow, 1, nrstart * ncstart * sizeof(char) );
  for (ir = 1; ir<nrarrow; ir+=3) {
    for (ic = 1; ic<ncarrow; ic+=3) {
      MARKARROW(ir, ic) = 0;
    }
  }

  /* iterate over start boxes */
  for (icstart=0; icstart<ncstart; icstart++) {
    for (irstart=0; irstart<nrstart; irstart++) {
      if (MARKSTART(irstart, icstart) == 0) {
        MARKSTART(irstart, icstart) = 1;

        /* trace streamline forward */
        row = START2ROW(irstart);
        col = START2COL(icstart);

        irend = ROW2END(row);
        icend = COL2END(col);
        if (irend < 0 || irend >= nrend || icend < 0 || icend >= ncend) {
          printf("bad 1:  irend = %d  icend = %d\n", irend, icend);
        }
        MARKEND(irend, icend) = 1;

        dir = 1.0;
        if (stream_trace( ctx, ugrid, vgrid, nr, nc, dir, vr, vc, maxv, &num,
                          markarrow, markstart, markend, nrarrow, ncarrow,
                          nrstart, ncstart, nrend, ncend, row, col, step,
                          rowlength, collength, irend, icend) == 0) {
          *numv = num;
          return 1;
        }

        /* now trace streamline backward */
        row = START2ROW(irstart);
        col = START2COL(icstart);

        irend = ROW2END(row);
        icend = COL2END(col);
        if (irend < 0 || irend >= nrend || icend < 0 || icend >= ncend) {
          printf("bad 3:  irend = %d  icend = %d\n", irend, icend);
        }
        MARKEND(irend, icend) = 1;

        dir = -1.0;
        if (stream_trace( ctx, ugrid, vgrid, nr, nc, dir, vr, vc, maxv, &num,
                          markarrow, markstart, markend, nrarrow, ncarrow,
                          nrstart, ncstart, nrend, ncend, row, col, step,
                          rowlength, collength, irend, icend) == 0) {
          *numv = num;
          return 1;
        }

      } /* end if */
    } /* end for */
  } /* end for */

  /* deallocate mark arrays */
  deallocate( ctx, markarrow, nrstart * ncstart * sizeof(char) );
  deallocate( ctx, markstart, nrstart * ncstart * sizeof(char) );
  deallocate( ctx, markend, nrend * ncend * sizeof(char) );

  *numv = num;

  return 1;
}

