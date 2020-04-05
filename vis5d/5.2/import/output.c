/* output.c */



/*
 * Make the output file.
 */


#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "analyze.h"
#include "file.h"
#include "grid.h"
#include "memory.h"
#include "misc.h"
#include "proj.h"
#include "projlist.h"
#include "resample.h"
#include "v5d.h"


extern int Debug;



/*
 * Write a grid of missing values.
 * Input:  v5d - the v5d file handle
 *         time, var - which timestep and variable
 *         nr, nc, nl - size of 3-D grid
 */
static void write_missing_grid( v5dstruct *v5d, int time, int var, 
                                int nr, int nc, int nl )
{
   static float *missing = NULL;
   static int gridsize = 0;
   int i;

   if (nr*nc*nl > gridsize) {
      /* allocate and initialize a larger grid of missing values */
      if (missing)   FREE(missing, 1);
      gridsize = nr*nc*nl;
      missing = (float *) MALLOC( gridsize * sizeof(float) );
      for (i=0;i<gridsize;i++) {
         missing[i] = MISSING;
      }
   }
   v5dWriteGrid( v5d, time, var, missing );
}




/*
 * Combine a number of grid arrays by averaging.  Missing values are handled
 * gracefully.
 * Input:  numvalues - how many elements in each grid (usually Nr*Nc*Nl)
 *         numgrids - how many arrays to merge
 *         grids - array of pointers to grids
 * Output:  result - array of averaged values.
 */
void average_values( int numvalues, int numgrids,
                     float *grids[], float *result )
{
   int i, j;

   for (i=0;i<numvalues;i++) {
      float sum = 0.0;
      int count = 0;
      for (j=0;j<numgrids;j++) {
         float value = grids[j][i];
         if (!IS_MISSING(value)) {
            sum += value;
            count++;
         }
      }
      if (count>0) {
         result[i] = sum / (float) count;
      }
      else {
         result[i] = MISSING;
      }
   }
}



/*
 * Like above but don't average, use first non-MISSING value.
 */
void merge_values( int numvalues, int numgrids, float *grids[], float *result )
{
   int i, j;

   for (i=0;i<numvalues;i++) {
      float sum = 0.0;
      for (j=0;j<numgrids;j++) {
         float value = grids[j][i];
         if (!IS_MISSING(value)) {
            result[i] = value;
            break;
         }
      }
      if (j==numgrids) {
         /* have to use missing */
         result[i] = MISSING;
      }
   }
}




#ifdef LEAVEOUT
/*
 * Sort an array of grids which have the same timestep and variable number.
 * When there is more than one input grid available for a particular time-
 * step and variable we need to determine which of those grid(s) to choose
 * for the output file.  This function attempts to sort these grids into
 * an order which places the most desirable grids near the front of the list.
 *
 * Input:  numgrids - how many grids in the array
 *         glist - an array of pointers to grid_info structs
 */
static void sort_similar_grids( int numgrids, struct grid_info *glist[] )
{
   int i, j;

   /* bubble sort is good enough */
   for (i=0;i<numgrids-1;i++) {
      for (j=i+1;j<numgrids;j++) {
         int swap = 0;
         /* compare projection pointers */
         if (glist[i]->Proj > glist[j]->Proj) {
            swap = 1;
         }
         else if (glist[i]->Proj == glist[j]->Proj) {
            /* compare Vcs pointers */
            if (glist[i]->Vcs > glist[j]->Vcs) {
               swap = 1;
            }
            else if (glist[i]->Vcs == glist[j]->Vcs) {
               /* compare grid rows */
               if (glist[i]->Nr < glist[j]->Nr) {
                  swap = 1;
               }
               else if (glist[i]->Nr == glist[j]->Nr) {
                  /* compare grid columns */
                  if (glist[i]->Nc < glist[j]->Nc) {
                     swap = 1;
                  }
               }
            }
         }
         if (swap) {
            struct grid_info *gtemp = glist[i];
            glist[i] = glist[j];
            glist[j] = gtemp;
         }
      }
   }
}
#endif



/*
 * Combine a number of 2-D grids with the same map projection to form a
 * 3-D grid.
 */
static float *combine_2d_grids( struct grid_db *db, int numgrids,
                                struct grid_info *glist[],
                                struct projection **proj,
                                struct vcs **vcs )
{
   float *height;
   int i, j, n, m;
   int nr, nc, nl;
   float *grid;
   int equally_spaced;

   assert( numgrids>1 );

   height = (float *) MALLOC( numgrids * sizeof(float) );

#ifdef LEAVEOUT
   /* Determine how many grids to combine */
   for (i=0; i<numgrids; i++) {
      if (glist[i]->Proj==glist[0]->Proj && glist[i]->Vcs->Nl==1) {
         height[i] = glist[i]->Vcs->Args[0];
      }
      else {
         break;
      }
   }
   n = i;
#else
   for (i=0; i<numgrids; i++) {
      assert( glist[i]->Vcs->Nl == 1 );
      height[i] = glist[i]->Vcs->Args[0];
   }
   n = numgrids;
#endif

   /* we now have an array of [n] height values for the 2-D grids to combine */

   /* sort 2-D grids by height coordinate */
   for (i=0;i<n-1;i++) {
      for (j=i+1;j<n;j++) {
         if (height[i]>height[j]) {
            /* swap */
            float htemp;
            struct grid_info *gtemp;
            htemp = height[i];
            height[i] = height[j];
            height[j] = htemp;
            gtemp = glist[i];
            glist[i] = glist[j];
            glist[j] = gtemp;
         }
      }
   }

   /* remove duplicate heights, which is possible */
   m = 0;
   for (i=0;i<n;i++) {
      if (m==0 || height[i]!=height[m-1]) {
         /* save this height */
         height[m] = height[i];
         glist[m] = glist[i];
         m++;
      }
   }
   n = m;


   /* check for equally-spaced levels */
   {
      float delta = height[1] - height[0];
      equally_spaced = 1;
      for (i=2;i<n;i++) {
         if (height[i]-height[i-1]!=delta) {
            equally_spaced = 0;
            break;
         }
      }
   }

   /* Make a new VCS for the stack of grids */
   if (equally_spaced) {
      float args[2];
      args[0] = height[0];
      args[1] = height[1] - height[0];
      if (glist[0]->Vcs->Kind==VERT_GENERIC) {
         *vcs = new_vcs( db, VERT_GENERIC, n, 0, args );
      }
      else {
         *vcs = new_vcs( db, VERT_EQUAL_KM, n, 0, args );
      }
   }
   else {
      *vcs = new_vcs( db, VERT_UNEQUAL_KM, n, 0, height );
   }
   *proj = glist[0]->Proj;

   assert(*vcs);
   assert(*proj);

   /* all done with array of grid heights */
   FREE(height, 2);

   /* Allocate storage for the 3-D grid */
   nr = glist[0]->Proj->Nr;
   nc = glist[0]->Proj->Nc;
   nl = n;
   grid = (float *) MALLOC( nr * nc * nl * sizeof(float) );
   if (!grid) {
      return NULL;
   }

   /* build the 3-D grid */
   for (i=0;i<n;i++) {
      float *layer;
      layer = get_file_data( glist[i] );
      assert(layer);
      memcpy( grid + nr*nc*i, layer, nr*nc*sizeof(float) );
      FREE( layer, 3 );
   }

   return grid;
}




#ifdef LEAVEOUT
/*
 * Get a 3-D grid of data for a particular timestep and variable.  The
 * data will not be resampled.  The grid may be the result of combining
 * "compatible" 1-level (2-D) grids together (such as McIDAS GRIDs).
 * Input:  db - the grid data base
 *         time, var - timestep and variable number
 * Output:  proj - the map projection of the grid
 *          vcs - the VCS of the grid
 * Return:  pointer to 3-D grid of data or NULL if no data available
 */
static float *get_grid_data( struct grid_db *db, int time, int var,
                             struct projection **proj,
                             struct vcs **vcs )
{
#define MAX_GRIDS 100
   struct grid_info *g, *glist[MAX_GRIDS];
   int numgrids;

   /* Count number of selected grids at this time and var */
   g = db->Matrix[time][var];
   numgrids = 0;
   while (g && numgrids<MAX_GRIDS) {
      if (g->SelectBits==ALL_BITS) {
         glist[numgrids] = g;
         numgrids++;
      }
      g = g->Sibling;
   }

   if (numgrids>=MAX_GRIDS) {

   }

   if (numgrids==0) {
      /* Either no data present or it's not selected for output */
      *proj = NULL;
      *vcs = NULL;
      return NULL;
   }
   else if (numgrids==1) {
      /* Common, simple case */
      *proj = glist[0]->Proj;
      *vcs  = glist[0]->Vcs;
      return get_file_data( glist[0] );
   }
   else {
      /* multiple grids! */
      sort_similar_grids( numgrids, glist );

      if (glist[0]->Vcs->Nl > 1) {
         /* just use the first 3-D grid in the list */
         *proj = glist[0]->Proj;
         *vcs  = glist[0]->Vcs;
         return get_file_data( glist[0] );
      }
      else {
         return combine_2d_grids( db, numgrids, glist, proj, vcs );
      }
   }
#undef MAX_GRIDS
}
#endif



#ifdef LEAVEOUT
/*
 * Return a pointer to the 3-D grid of data resampled to the output file's
 * map projection and VCS.
 * Input:  db - the grid data base
 *         time, vvar - which timestep and variable
 *         outproj - output projection
 *         outvcs - output VCS
 *         outnl - number of grid levels wanted
 * Return:  pointer to 3-D grid of values which may be free()'d or
 *          NULL if there's no grid data
 */
static float *get_resampled_data( struct grid_db *db, int time, int var,
                                  struct projection *outproj,
                                  struct vcs *outvcs,
                                  int outnl )
{
   float *outdata;
   struct projection *gridproj;
   struct vcs *gridvcs;
   struct resampler *resamp;

   assert( outnl <= outvcs->Nl );

   /* get grid data in its original size, projection and VCS */
   outdata = get_grid_data( db, time, var, &gridproj, &gridvcs );
   if (!outdata) {
      return NULL;
   }

   resamp = get_resampler( gridproj, gridvcs, outproj, outvcs, outnl );

   if (Debug) {
      printf("Input grid:          ");
      print_min_max( outdata, gridproj->Nr * gridproj->Nc * gridvcs->Nl );
   }

   /* do vertical resampling if needed */
   if (gridvcs!=outvcs) {
      float *indata = outdata;
      outdata = (float *)
            MALLOC( gridproj->Nr * gridproj->Nc * outnl * sizeof(float) );
      resample_vertical( resamp, indata, outdata );
      FREE( indata, 4 );
   }

   if (Debug) {
      printf("After vert resamp:   ");
      print_min_max( outdata, gridproj->Nr * gridproj->Nc * outnl );
   }

   /* do horizontal resampling if needed */
   if (gridproj!=outproj) {
      float *indata = outdata;
      outdata = (float *) MALLOC( outproj->Nr * outproj->Nc * outnl
                                  * sizeof(float) );
      resample_horizontal( resamp, indata, outdata );
      FREE( indata, 5 );
   }

   if (Debug) {
      printf("After horiz resamp:  ");
      print_min_max( outdata, outproj->Nr * outproj->Nc * outnl );
   }

   return outdata;
}
#endif




/*
 * Given a list of grids, combine all sets of 2-D grids with same map
 * projection into new 3-D grids.  Return the new list of grids.
 * Input:  db - the grid data base
 * In/Out:  numgrids - how many grids in the list
 *          glist - list of grids
 *
 * Example:  suppose we have a list of 6 input grids:
 *             1. 6 levels
 *             2. 1 level, map projection #4
 *             3. 1 level, map projection #4
 *             4. 1 level, map projection #5
 *             5. 6 levels
 *             6. 1 level, map projection #4
 *
 *  The list of output grids would be:
 *             1. 6 levels
 *             2. 3 levels, map projection #4  (1,2 and 6 stacked)
 *             3. 1 level, map projection #5
 *             4. 6 levels
 */
static void find_and_combine_2d_grids( struct grid_db *db, int *numgrids,
                                       struct grid_info *glist[] )
{
   struct grid_info *outlist[100];
   int numout;
   int i, j;

   numout = 0;

   for (i=0;i<*numgrids;i++) {
      if (glist[i]) {
         if (glist[i]->Vcs->Nl==1) {
            struct grid_info *gp[100];   /* grids with same projection */
            int ngp;                     /* how many in gp[] array */
            struct grid_info *newgrid;   /* the "stacked" gp[] grids */

            /* make a list of containing this 2-D grid and all others with */
            /* the same horizonal coordinate system */
            gp[0] = glist[i];
            ngp = 1;
            for (j=i+1;j<*numgrids;j++) {
               if (glist[j]
                   && glist[j]->Vcs->Nl==1 && glist[j]->Proj==glist[i]->Proj) {
                  gp[ngp++] = glist[j];
                  glist[j] = NULL;  /* remove */
               }
            }
            glist[i] = NULL;  /* remove */

            /* Construct a new grid_info from a stack of 2-D grids */
            newgrid = alloc_grid_info();
            newgrid->TimeStep=123;
            newgrid->Data = combine_2d_grids( db, ngp, gp,
                                              &newgrid->Proj, &newgrid->Vcs );
            assert(newgrid->TimeStep==123);
            assert(newgrid->Proj);
            assert(newgrid->Vcs);
            newgrid->Nr = newgrid->Proj->Nr;
            newgrid->Nc = newgrid->Proj->Nc;
            newgrid->Nl = newgrid->Vcs->Nl;

            /* add new grid to output list */
            outlist[numout++] = newgrid;
         }
         else {
            /* just add this grid to the output list */
            outlist[numout++] = glist[i];
         }
      }
   }

   /* Return output grid list */
   for (i=0;i<numout;i++) {
      glist[i] = outlist[i];
   }
   *numgrids = numout;
}




/*
 * Given a grid_info, load the 3-D data from its file and resample
 * it to the output projection.
 */
static float *get_resampled_3d_data( struct grid_db *db, struct grid_info *g,
                                     struct projection *outproj,
                                     struct vcs *outvcs,
                                     int outnl )
{
   struct resampler *resamp;
   float *outdata;

   if (outvcs->Nl!=outnl) {
      printf("**** outvcs->Nl != outnl in get_r_3_d\n");
   }
   assert( g );
   assert( g->Nl==g->Vcs->Nl );

   /* get grid data in its original size, projection and VCS */
   if (g->Data) {
      /* Return a copy of grid data unchanged */
      outdata = MALLOC( g->Nr * g->Nc * g->Nl * sizeof(float) );
      if (!outdata) {
         return NULL;
      }
      memcpy( outdata, g->Data, g->Nr * g->Nc * g->Nl * sizeof(float) );
   }
   else {
      outdata = get_file_data( g );
      if (!outdata) {
         return NULL;
      }
   }

   resamp = get_resampler( g->Proj, g->Vcs, outproj, outvcs, outnl );

   if (Debug) {
      printf("Input grid:          ");
      print_min_max( outdata, g->Proj->Nr * g->Proj->Nc * g->Vcs->Nl );
   }

   /* do vertical resampling if needed */
   if (g->Vcs!=outvcs) {
      float *indata = outdata;
      outdata = (float *)
            MALLOC( g->Proj->Nr * g->Proj->Nc * outnl * sizeof(float) );
      resample_vertical( resamp, indata, outdata );
      FREE( indata, 6 );
   }

   if (Debug) {
      printf("After vert resamp:   ");
      print_min_max( outdata, g->Proj->Nr * g->Proj->Nc * outnl );
   }

   /* do horizontal resampling if needed */
   if (g->Proj!=outproj) {
      float *indata = outdata;
      outdata = (float *) MALLOC( outproj->Nr * outproj->Nc * outnl
                                  * sizeof(float) );
      resample_horizontal( resamp, indata, outdata );
      FREE( indata, 7 );
   }

   if (Debug) {
      printf("After horiz resamp:  ");
      print_min_max( outdata, outproj->Nr * outproj->Nc * outnl );
   }

   return outdata;
}



/*
 * For all the grids that belong to timestep 'time' and variable 'var'
 * resample them to the output projection and VCS then merge them into
 * one final 3-D grid.
 */
static float *get_combined_resampled_data( struct grid_db *db,
                                           int time, int var,
                                           struct projection *outproj,
                                           struct vcs *outvcs,
                                           int outnl, int average )
{
#define MAX_GRIDS 100
   float *outdata;
   struct projection *gridproj;
   struct vcs *gridvcs;
   struct resampler *resamp;
   int numgrids;
   struct grid_info *g, *glist[MAX_GRIDS];
   float *gdata[MAX_GRIDS];
   int i, j, n;

   /* just a test */
   if (outvcs->Nl!=outnl) {
      printf("***** outvcs->nl != outnl in get_c_r_d\n");
   }

   /* Make a list of all selected grids for this timestep and variable */
   g = db->Matrix[time][var];
   numgrids = 0;
   while (g && numgrids<MAX_GRIDS) {
      if (g->SelectBits==ALL_BITS) {
         glist[numgrids] = g;
         numgrids++;
      }
      g = g->Sibling;
   }

   if (numgrids==0) {
      /* no grid data available for this timstep and variable */
      return NULL;
   }

   /* Merge/stack 2-D grids into 3-D grid(s) */
   if (numgrids>1) {
      find_and_combine_2d_grids( db, &numgrids, glist );
   }

   /* Resample all the grids to the output proj and VCS */
   n = numgrids;
   numgrids = 0;
   for (i=0;i<n;i++) {
      gdata[numgrids] = get_resampled_3d_data( db, glist[i],
                                               outproj, outvcs, outnl );
      if (gdata[numgrids]) {
         numgrids++;
      }
   }

   if (numgrids==0) {
      return NULL;
   }
   else if (numgrids==1) {
      /* Common case */
      if (glist[0]->Data) {
         free_grid_info( glist[0] );
      }
      return gdata[0];
   }
   else {
      /* Average together the 3-D grids */
      int nrncnl = outproj->Nr * outproj->Nc * outnl;
      float *gout = (float *) MALLOC( nrncnl * sizeof(float) );

      if (average) {
         average_values( nrncnl, numgrids, gdata, gout );
      }
      else {
         /* sort grids by order of decreasing resolution (high-res first) */
         for (i=0;i<numgrids-1;i++) {
            for (j=i+1;j<numgrids;j++) {
               float res1, res2;
               res1 = proj_resolution( glist[i]->Proj );
               res2 = proj_resolution( glist[j]->Proj );
               if (res2<res1) {
                  /* swap */
                  float *ftmp;
                  struct grid_info *gtmp;
                  ftmp = gdata[i];
                  gdata[i] = gdata[j];
                  gdata[j] = ftmp;
                  gtmp = glist[i];
                  glist[i] = glist[j];
                  glist[j] = gtmp;
               }
            }
         }
         /* use values from high-res grid */
         merge_values( nrncnl, numgrids, gdata, gout );
      }

      /* free temporary data grids */
      for (i=0;i<numgrids;i++) {
         FREE( gdata[i], 8 );
         if (glist[i]->Data) {
            free_grid_info( glist[i] );
         }
      }

      return gout;
   }
}





/*
 * Make a v5d file from the table of grids and parameters found in the
 * v5dstruct.
 *
 * Input:  db - the grid database struct describing the grid data layout
 *         v5d - contains parameters to describe output file:
 *                   Nr = number of rows
 *                   Nc = number of columns
 *                   Projection, ProjArgs - map projection
 *                   Vertical, VertArgs - vertical coord system
 *         filename - name of v5d output file.
 *         maxnl - maximum number of grid levels allowed.
 *         average - 0=use higher-res co-located data
 *                   1=average co-located data
 *         compressmode - bytes per datapoint in output file: 1, 2, or 4
 *
 * Notes:  Which variables and timesteps to output is determined by
 * examining the grid table.
 */
void make_output_file( struct grid_db *db, v5dstruct *v5d, char *filename,
                       int maxnl, int average, int compressmode )
{
   int time, var;
   int xlate_time[MAXTIMES], xlate_var[MAXVARS];
   int nl[IMAXVARS], lowlev[IMAXVARS];
   struct projection *output_proj;
   struct vcs *output_vcs, *var_vcs[MAXVARS];
   int numproj, numvcs;
   int i;


#if V5D_VERSION >= 42
   printf("Writing a 4.3 file!\n");
#endif

   /* Save current number of projections and VCSs */
   numproj = db->NumProj;
   numvcs = db->NumVcs;

   /*
    * Finish initializing the v5d struct
    */

   /* Output file's projection and VCS */
   output_proj = new_projection( db, v5d->Projection, v5d->Nr, v5d->Nc,
                                 v5d->ProjArgs );
   output_vcs = new_vcs( db, v5d->VerticalSystem, maxnl, 0, v5d->VertArgs );

   /* Grid levels */
   compute_grid_levels( db, output_vcs, lowlev, nl );
   for (i=0;i<db->NumVars;i++) {
      /* truncate number of levels to fit user specified limit */
      if (lowlev[i]+nl[i]>maxnl) {
         if (nl[i]>maxnl) {
            lowlev[i] = 0;
            nl[i] = maxnl;
         }
         else {
            lowlev[i] = maxnl - nl[i];
         }
      }
   }


   /* Setup variables */
   v5d->NumVars = 0;
   for (var=0;var<db->NumVars;var++) {
      if (db->VarSelected[var]) {
         if (v5d->NumVars<MAXVARS) {
            strncpy( v5d->VarName[v5d->NumVars], db->VarNames[var], 10 );
            if (db->Units[var]) {
               strncpy( v5d->Units[v5d->NumVars], db->Units[var], 19 );
               v5d->Units[v5d->NumVars][19] = 0;
            }
            xlate_var[v5d->NumVars] = var;

            v5d->Nl[v5d->NumVars] = nl[var];
#if V5D_VERSION >= 42
            v5d->LowLev[v5d->NumVars] = lowlev[var];
#else
            /* sanity check */
            assert( lowlev[var] == 0 );
#endif
            v5d->NumVars++;
         }
         else {
            printf("Warning: too many variables, %d is limit,", MAXVARS );
            printf(" ignoring %s\n", db->VarNames[var] );
         }
      }
   }

   /* Setup timesteps */
   v5d->NumTimes = 0;
   for (time=0;time<db->NumTimes;time++) {
      if (db->TimeSelected[time]) {
         if (v5d->NumTimes<MAXTIMES) {
            v5d->DateStamp[v5d->NumTimes] = db->DateStamp[time];
            v5d->TimeStamp[v5d->NumTimes] = db->TimeStamp[time];
            xlate_time[v5d->NumTimes] = time;
            v5d->NumTimes++;
         }
         else {
            printf("Warning: too many time steps, %d is limit,", MAXTIMES);
            printf(" ignoring %05d %06d\n", db->DateStamp[time],
                   db->TimeStamp[time] );
         }
      }
   }


   /* actually need a different VCS for each variable */
   for (var=0;var<v5d->NumVars;var++) {
      var_vcs[var] = new_vcs( db, v5d->VerticalSystem, v5d->Nl[var],
#if V5D_VERSION >= 42
                                 v5d->LowLev[var],
#else
                                 0,
#endif
                                 v5d->VertArgs );
   }
   v5d->CompressMode = compressmode;

   /*
    * Create the v5d file
    */
   if (!v5dCreateFile( filename, v5d )) {
      printf("Error in v5dCreateFile\n");
      return;
   }


   /*
    * Write grid data
    */
   for (time=0;time<v5d->NumTimes;time++) {
      for (var=0;var<v5d->NumVars;var++) {
         int table_time, table_var;
         float *data;

         /* Translate timestep and variable number from v5d struct indexes */
         /* into grid table indexes. */
         table_time = xlate_time[time];
         table_var = xlate_var[var];

         printf("Time: %d  Var: %s\n", time+1, v5d->VarName[var] );

         /* Get the resampled data for this timestep and variable */
         data = get_combined_resampled_data( db, table_time, table_var,
                                 output_proj, var_vcs[var], v5d->Nl[var],
                                 average );
         if (data) {
            v5dWriteGrid( v5d, time, var, data );
            FREE( data, 9 );
         }
         else {
            if (Debug)  printf("missing: %d %d\n", time, var );
            write_missing_grid( v5d, time, var, v5d->Nr, v5d->Nc,v5d->Nl[var]);
         }
      }
   }

   v5dCloseFile( v5d );

   free_resamplers();

   /* free new projection and VCSs made during resampling */
   for (i=db->NumProj-1; i>=numproj; i--) {
      free_projection( db, db->ProjList[i] );
   }
   for (i=db->NumVcs-1; i>=numvcs; i--) {
      free_vcs( db, db->VcsList[i] );
   }
}


