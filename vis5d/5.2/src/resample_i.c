/* resample.c */
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
How resampling is done:

For horizontal resampling we need to know which data points to grab from
the input grid so we can compute the weighted average to store in the output
grid.  The SampRow[][] and SampCol[] arrays tell us this.  Pseudocode:

    FOR each row, r, in output grid DO
       FOR each column, c, in output grid DO
          source_row = SampRow[r][c]
          source_col = SampCol[r][c]

          value = weighted average of input_grid[source_row][source_col]
                                  and input_grid[source_row][source_col+1]
                                  and input_grid[source_row+1][source_col]
                                  and input_grid[source_row+1][source_col+1]

          output_grid[r][c] = value;

       ENDFOR
    ENDFOR

The sample locations are the integer parts of SampRow[][] and SampCol[][].
The sample weights are the fractional parts of SampRow[][] and SampCol[][].


Vertical resampling is done similarly.  However, we can't just store a
1-D table of sample locations.  We have to use a 3-D table because the
vertical resampling can vary across the grid when using a Sigma coordinate
systems (i.e. relative to elevation of topography).


The resampler struct contains the resampling tables corresponding to an
input projection and VCS and output projection and VCS.  Since all the
input grids often have the same projection and VCS we often only need one
resampler.

*/




#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "grid_i.h"
#include "memory_i.h"
#include "proj_i.h"
#include "resample_i.h"
#include "topo_i.h"
#include "../src/v5d.h"



#define TOPO_FILE "EARTH.TOPO"

#define ABS(X)   ( (X) < 0.0 ? -(X) : (X) )






/*
 * Compute all the information needed to resample a grid from the form
 * specifed by g to the grid specified by outrows, outcols, etc.  This is
 * basically a setup step prior to beginning resampling.  It potentially
 * does a lot of work which shouldn't be done "on the fly" while resampling.
 */
static void init_resampler( struct resampler *r, int outnl )
{
   int p;
#define SAMPROW(R,C)  r->SampRow[ (C) + (R) * r->outC ]
#define SAMPCOL(R,C)  r->SampCol[ (C) + (R) * r->outC ]
#define SAMPLEV(R,C,L)  r->SampLev[ (C) + ((R) + (L) * r->inR) * r->inC ]

   assert( r );

   printf("init_resampler...\n");

   r->inR = r->inproj->Nr;
   r->inC = r->inproj->Nc;
   r->inL = r->invcs->Nl;
   r->outR = r->outproj->Nr;
   r->outC = r->outproj->Nc;
   r->outL = outnl;

   if (r->inproj->Kind==PROJ_EPA) {
      r->Guard = 1;
   }
   else {
      r->Guard = 0;
   }

   /* just a test... */
   if (r->outL!=r->outvcs->Nl) {
      printf("different Nl values!\n");
   }


   if (r->invcs != r->outvcs) {
      /* Vertical resampling is needed */
      int i, j, k;

      r->DoVertical = 1;
      r->SampLev = (float *) MALLOC(r->inR * r->inC * r->outL * sizeof(float));

      if (load_topo(TOPO_FILE)) {
         /* Specify topo resampling by looking at distance in lat/lon */
         /* between two grid points near the center of domain. */
         float lat1, lat2, lon1, lon2;
         rowcol_to_latlon_i( (float) r->inR/2, (float) r->inC/2,
                           &lat1, &lon1, r->inproj );
         rowcol_to_latlon_i( (float) r->inR/2+1, (float) r->inC/2+1,
                           &lat2, &lon2, r->inproj );
         set_topo_sampling( ABS(lat2-lat1), ABS(lon2-lon1) );
      }
      else {
         printf("Note: topography file %s not found\n", TOPO_FILE);
      }

      /* Compute locations of vertical samples */
      for (i=0; i<r->inR; i++) {
         for (j=0; j<r->inC; j++) {
            float lat, lon, topo_elev;
            int k1;

            /* Get the elevation of the topo at the lat/lon corresponding */
            /* to this row/column. */
            rowcol_to_latlon_i( (float) i, (float) j, &lat, &lon, r->inproj );
            topo_elev = elevation_i( lat, lon, NULL) / 1000.0;

            /* Special case of input grid being 2-D to make sure the data */
            /* shows up in the output grid and not missed when resampling. */
            if (r->invcs->Nl==1) {
               float height1, level1;
               level_to_height( 0.0, &height1, r->invcs, topo_elev );
               if (height_to_level( height1, &level1, r->outvcs, topo_elev )) {
                  k1 = (int) level1;
               }
               else {
                  k1 = -1;
               }
            }
            else {
               k1 = -1;
            }

            for (k=0;k<r->outL;k++) {
               if (k==k1) {
                  /* special case, see above */
                  SAMPLEV(i,j,k) = 0.0;
               }
               else {
                  float height, level, kk;
                  /* convert level k to a height in output vert coord sys */
                  kk = (float) (k + r->outvcs->LowLev);
                  level_to_height( kk, &height, r->outvcs, topo_elev );
                  /* convert the height to a level in input vert coord sys */
                  if (height_to_level( height, &level, r->invcs, topo_elev )) {
                     SAMPLEV(i,j,k) = level;
                  }
                  else {
                     /* out of bounds */
                     SAMPLEV(i,j,k) = -1.0;
                  }
                  assert( r->inproj->Nr > 0 );
                  p = (j) + ((i) + (k) * r->inR) * r->inC;
                  assert( p < r->inR * r->inC * r->outL );
               }
            }
         }
      }
   }
   else {
      r->DoVertical = 0;
   }

   if (r->inproj != r->outproj) {
      float lat, lon, row, col;
      int i, j;

      r->DoHorizontal = 1;
      r->SampRow = (float *) MALLOC( r->outR * r->outC * sizeof(float) );
      r->SampCol = (float *) MALLOC( r->outR * r->outC * sizeof(float) );

      /* Compute location of horizontal samples */
      for (i=0;i<r->outR;i++) {
         for (j=0;j<r->outC;j++) {
            /* (row,col) -> (lat,lon) in output projection */
            rowcol_to_latlon_i( (float) i, (float) j, &lat, &lon, r->outproj );
            /* (lat,lon) -> (row,col) in input projection */
            if (latlon_to_rowcol_i( lat, lon, &row, &col, r->inproj )) {
               SAMPROW(i,j) = row;
               SAMPCOL(i,j) = col;
            }
            else {
               SAMPROW(i,j) = -1.0;
               SAMPCOL(i,j) = -1.0;
            }
         }
      }
   }
   else {
      r->DoHorizontal = 0;
   }

   printf("Done  (vert=%d, horiz=%d)\n", r->DoVertical, r->DoHorizontal);
#undef SAMPROW
#undef SAMPCOL
#undef SAMPLEV
}



/* List of resamplers */
#define MAX_RESAMPLERS 1000
static struct resampler *ResamplerList[MAX_RESAMPLERS];
static int NumResamplers = 0;


/*
 * Given an input and output projection/VCS returns a resampler struct
 * which will do the needed resampling.
 */
struct resampler *get_resampler( struct projection *inproj,
                                 struct vcs *invcs,
                                 struct projection *outproj,
                                 struct vcs *outvcs,
                                 int outnl )
{
   int i;

   assert( inproj );
   assert( invcs );
   assert( outproj );
   assert( outvcs );

   /* See if the same resampler is already defined */
   for (i=0;i<NumResamplers;i++) {
      if (   ResamplerList[i]->inproj==inproj
          && ResamplerList[i]->invcs==invcs
          && ResamplerList[i]->outproj==outproj
          && ResamplerList[i]->outvcs==outvcs
          && ResamplerList[i]->outL==outnl) {
         /* Found identical resampler in list */
         return ResamplerList[i];
      }
   }

   /* didn't find resampler in list, make new one */
   if (NumResamplers<MAX_RESAMPLERS) {
      struct resampler *r;
      r = (struct resampler *) MALLOC( sizeof(struct resampler) );
      r->inproj = inproj;
      r->invcs = invcs;
      r->outproj = outproj;
      r->outvcs = outvcs;
      init_resampler( r, outnl );
      ResamplerList[NumResamplers] = r;
      NumResamplers++;
      return r;
   }
   else {
      assert( NumResamplers < MAX_RESAMPLERS );
      return NULL;
   }
}



/*
 * Deallocate all resamplers
 */
void free_resamplers( void )
{
   int i;

   for (i=0;i<NumResamplers;i++) {
      if (ResamplerList[i]->DoVertical) {
         free( ResamplerList[i]->SampLev );
      }
      if (ResamplerList[i]->DoHorizontal) {
         free( ResamplerList[i]->SampRow );
         free( ResamplerList[i]->SampCol );
      }
      free( ResamplerList[i] );
   }

   NumResamplers = 0;
}



static void compare( int n, float *a, float *b )
{
   int i, same;
   same = 0;
   for (i=0;i<n;i++) {
      if (a[i]==b[i]) {
         same++;
      }
   }
   printf("%d of %d values are same\n", same, n );
}



/*
 * Resample a 3-D grid to a new vertical coordinate system.
 * Input:  r - pointer to a resampler struct
 *         indata - input grid data
 *         outdata - pointer to buffer to store output grid
 * Output:  outdata - contains resampled grid data
 */
void resample_vertical( struct resampler *r, float *indata, float *outdata )
{
   int i, j, k;

   assert( r );
   assert( indata );
   assert( outdata );

   assert( r->invcs != r->outvcs );

#define INDATA(R,C,L)    indata[  ((L) * r->inC + (C)) * r->inR + (R) ]
#define OUTDATA(R,C,L)   outdata[ ((L) * r->inC + (C)) * r->inR + (R) ]
#define SAMPLEV(R,C,L)   r->SampLev[ (C) + ((R) + (L) * r->inR) * r->inC ]

   for (i=0; i<r->inR; i++) {      /* was outR */
      for (j=0; j<r->inC; j++) {   /* was outC */
         for (k=0; k<r->outL; k++) {
            int level;
            float weight;
            float val;

            level = (int) SAMPLEV( i, j, k );
            weight = SAMPLEV( i, j, k ) - (float) level;

            if (level>=0 && level<r->inL) {
               /* compute weighted average of levels 'level' and 'level+1' */
               if (weight==0.0) {
                  val = OUTDATA(i,j,k) = INDATA(i,j,level);
               }
               else {
                  float v1 = INDATA(i,j,level);
                  float v2 = INDATA(i,j,level+1);
#ifdef DEBUG
                  assert( level+1 < r->inL || weight==0.0 );
#endif
                  if (IS_MISSING(v1) || IS_MISSING(v2)) {
                     val = OUTDATA(i, j, k) = MISSING;
                  }
                  else {
                     val = OUTDATA(i, j, k) = v1 * (1.0F-weight) + v2 * weight;
                  }
               }
#ifdef DEBUG
               assert( val==val );
#endif
            }
            else {
               /* out of bounds */
               OUTDATA(i, j, k) = MISSING;
            }

         }
      }
   }

#ifdef DEBUG
   for (k=0;k<r->inL;k++) {
      compare( r->inR * r->inC, &INDATA(0,0,k), &OUTDATA(0,0,k) );
   }
#endif

#undef INDATA
#undef OUTDATA
#undef SAMPLEV
}



/*
 * Resample a 3-D grid to a new projection.
 * Input:  r - pointer to a resampler struct
 *         indata - input data
 *         outdata - pointer to buffer to store new data
 * Output:  outdata - contains the resampled data
 */
void resample_horizontal( struct resampler *r, float *indata, float *outdata )
{
#define INDATA( R, C, L )    indata[  ((L) * r->inC + (C)) * r->inR + (R) ]
#define OUTDATA( R, C, L )   outdata[ ((L) * r->outC + (C)) * r->outR + (R) ]
#define SAMPROW(R,C)   SampRow[ (C) + (R) * r->outC ]
#define SAMPCOL(R,C)   SampCol[ (C) + (R) * r->outC ]
   int i, j, k;
   int miss;
   int minrow, maxrow, mincol, maxcol;

   assert( r );
   assert( indata );
   assert( outdata );
   assert( r->inproj != r->outproj );

   miss = 0;

   /* bounds of valid grid points of input grid */
   minrow = r->Guard;
   maxrow = r->inR - 1 - r->Guard;
   mincol = r->Guard;
   maxcol = r->inC - 1 - r->Guard;


   for (i=0;i<r->outR;i++) {
      for (j=0;j<r->outC;j++) {
         int row, col;
         float alpha, beta;

         row = (int) r->SAMPROW( i, j );
         col = (int) r->SAMPCOL( i, j );
         alpha = r->SAMPROW( i, j ) - (float) row;
         beta  = r->SAMPCOL( i, j ) - (float) col;

         if (row>=minrow && col>=mincol && row<=maxrow && col<=maxcol) {
            /* get weighted average of four neighbors around (row,col) */
            /* in the input data */
            for (k=0;k<r->outL;k++) {
               float v00, v01, v10, v11;
         
               int dr = (row != maxrow);  /* tricky! */
               int dc = (col != maxcol);
               v00 = INDATA( row,    col,    k );
               v01 = INDATA( row,    col+dc, k );
               v10 = INDATA( row+dr, col,    k );
               v11 = INDATA( row+dr, col+dc, k );
               if (   IS_MISSING(v00) || IS_MISSING(v01)
                   || IS_MISSING(v10) || IS_MISSING(v11)) {
                  OUTDATA( i, j, k ) = MISSING;
                  miss++;
               }
               else {
                  float t0 = v00 * (1.0-beta) + v01 * beta;
                  float t1 = v10 * (1.0-beta) + v11 * beta;
                  float value;
                  value = OUTDATA( i, j, k ) = t0 * (1.0-alpha) + t1 * alpha;
               }
            }
         }
         else {
            /* (row,col) is outside domain of input grid */
            for (k=0; k<r->outL; k++) {
               OUTDATA( i, j, k ) = MISSING;
               miss++;
            }
         }

      }
   }

#undef INDATA
#undef OUTDATA
#undef SAMPROW
#undef SAMPCOL
}

