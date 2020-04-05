/* analyze.c */


#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "analyze.h"
#include "grid.h"
#include "misc.h"
#include "proj.h"
#include "projlist.h"
#include "v5d.h"



static struct grid_db *sort_db;


/*
 * Compare the two grids and return -1, 0 or 1 to indicate their relationship
 * as required by the qsort() function used below.
 */
static int compare_grids( const void *a, const void *b )
{
   struct grid_info *grida, *gridb;
   int n, i, j;

   grida = *( (struct grid_info **) a);
   gridb = *( (struct grid_info **) b);

   /* Compare Dates */
   if (v5dYYDDDtoDays(grida->DateStamp) <
       v5dYYDDDtoDays(gridb->DateStamp)) {
      return -1;
   }
   else if (v5dYYDDDtoDays(grida->DateStamp) >
            v5dYYDDDtoDays(gridb->DateStamp)) {
      return 1;
   }

   /* Equal dates, compare times */
   if (v5dHHMMSStoSeconds(grida->TimeStamp) <
       v5dHHMMSStoSeconds(gridb->TimeStamp)) {
      return -1;
   }
   else if (v5dHHMMSStoSeconds(grida->TimeStamp) >
            v5dHHMMSStoSeconds(gridb->TimeStamp)) {
      return 1;
   }

   /* Equal dates and times, compare varnames */
#ifdef LEAVEOUT
   n = strcmp( grida->VarName, gridb->VarName );
   if (n) {
      return n;
   }

   /* Equal varnames, compare rows */
   if (grida->Nr < gridb->Nr) {
      return -1;
   }
   else if (grida->Nr > gridb->Nr) {
      return 1;
   }

   /* Equal rows, compare columns */
   if (grida->Nc < gridb->Nc) {
      return -1;
   }
   else if (grida->Nc > gridb->Nc) {
      return 1;
   }
#else
   /* i = position of grida->VarName in varname list */
   for (i=0; i<sort_db->NumVars; i++) {
      if (strcmp(grida->VarName, sort_db->VarNames[i])==0) {
         break;
      }
   }
   /* j = position of gridb->VarName in varname list */
   for (j=0; j<sort_db->NumVars; j++) {
      if (strcmp(gridb->VarName, sort_db->VarNames[j])==0) {
         break;
      }
   }
   if (i<j) {
      return -1;
   }
   else if (i>j) {
      return 1;
   }
#endif

   /* Default */
   return 0;
}



/*
 * Sort the grid list by time, date, and variable name, respecively.
 */
void sort_grids( struct grid_db *db )
{
   struct grid_info **grid_array;
   struct grid_info *g;
   int i;

   /* make sure each variable name is in the variable list */
   for (g=db->FirstGrid; g; g=g->Next) {
      int inlist = 0;
      for (i=0; i<db->NumVars; i++) {
         if (strcmp( g->VarName, db->VarNames[i])==0) {
            inlist = 1;
            /* grab this units string if we don't already have one */
            if (!db->Units[i] && g->Units) {
               db->Units[i] = str_dup( g->Units );
            }
            break;
         }
      }
      if (!inlist) {
         /* add to list */
         if (db->NumVars<IMAXVARS) {
            db->VarNames[db->NumVars] = str_dup( g->VarName );
            if (g->Units) {
               db->Units[db->NumVars] = str_dup( g->Units );
            }
            db->NumVars++;
         }
         else {
            printf("Error: too many variables, %d is limit,", IMAXVARS );
            printf(" ignoring %s\n", g->VarName );
         }
      }
   }

   /* Special case: */
   if (db->NumGrids<=1) {
      db->Sorted = 1;
      return;
   }

   /*
    * Convert the linked list into an array of pointers, sort the array,
    * then rebuild the linked list.
    */

   /* Allocate an array of pointers to grid_info structs */
   grid_array = (struct grid_info **) malloc( db->NumGrids
                                              * sizeof(struct grid_info *) );

   /* initialize the grid_array */
   g = db->FirstGrid;
   for (i=0;i<db->NumGrids;i++) {
      grid_array[i] = g;
      g = g->Next;
   }

   sort_db = db;

   /* Use quicksort to sort the array */
   qsort( grid_array, db->NumGrids,
          sizeof(struct grid_info *), compare_grids );

   sort_db = NULL;

   /* Now rebuild the linked list so it's in sorted order */
   for (i=0;i<db->NumGrids-1;i++) {
      grid_array[i]->Next = grid_array[i+1];
   }
   db->FirstGrid = grid_array[0];
   db->LastGrid = grid_array[db->NumGrids-1];
   db->LastGrid->Next = NULL;

   /* Deallocate the grid_array */
   free( grid_array );

   db->Sorted = 1;
}



/*
 * Look at all the grids in the list to initialize the NumVars and VarName
 * fields.
 */
static void make_var_list( struct grid_db *db )
{
   struct grid_info *g;
   int i, inlist;

   db->NumVars = 0;

   /* scan over list of file_info structs */
   for (g=db->FirstGrid; g && db->NumVars<IMAXVARS; g=g->Next) {

      /* check if g->VarName is already in list */
      inlist = 0;
      for (i=0; i<db->NumVars; i++) {
         if (strcmp( g->VarName, db->VarNames[i])==0) {
            /* already in list */
            inlist = 1;
            /* grab the units string if don't already have one */
            if (!db->Units[i] && g->Units) {
               db->Units[i] = str_dup( g->Units );
            }
            break;
         }
      }

      if (!inlist) {
         /* add to list */
         db->VarNames[db->NumVars] = str_dup( g->VarName );
         if (g->Units) {
            db->Units[db->NumVars] = str_dup( g->Units );
         }
         db->NumVars++;
      }

   }
}



/*
 * Look at all the grids in the list to initialize the NumTimes, TimeStamp,
 * and DateStamp fields.
 */
static void make_time_list( struct grid_db *db )
{
   struct grid_info *g;

   assert( db->Sorted );

   db->NumTimes = 0;

   for (g=db->FirstGrid; g && db->NumTimes<IMAXTIMES; g=g->Next) {

      if (db->NumTimes==0) {
         db->TimeStamp[db->NumTimes] = g->TimeStamp;
         db->DateStamp[db->NumTimes] = g->DateStamp;
         db->NumTimes = 1;
      }
      else if (   g->TimeStamp != db->TimeStamp[db->NumTimes-1]
               || g->DateStamp != db->DateStamp[db->NumTimes-1]) {
         if (db->NumTimes<IMAXTIMES) {
            db->TimeStamp[db->NumTimes] = g->TimeStamp;
            db->DateStamp[db->NumTimes] = g->DateStamp;
            db->NumTimes++;
         }
         else {
            printf("Error: too many timesteps, %d is limit,", IMAXTIMES );
            printf(" ignoring %05d %06d\n", g->DateStamp, g->TimeStamp );
         }
      }

   }
}



/*
 * From the linked list of grids_info structs, determine number of variables,
 * their names, number of timesteps, build the grid table, etc...
 */
void analyze_grids( struct grid_db *db )
{
   int i, it, iv;
   struct grid_info *g;

   sort_grids( db );

/*   make_var_list( db );*/
   make_time_list( db );

   /* Initialize selection flag arrays */
   for (i=0;i<db->NumVars;i++) {
      db->VarSelected[i] = 0;
   }
   for (i=0;i<db->NumTimes;i++) {
      db->TimeSelected[i] = 0;
   }
   for (i=0;i<db->NumProj;i++) {
      db->ProjSelected[i] = 0;
   }
   for (i=0;i<db->NumVcs;i++) {
      db->VcsSelected[i] = 0;
   }

   for (it=0;it<db->NumTimes;it++) {
      for (iv=0;iv<db->NumVars;iv++) {
         db->Matrix[it][iv] = NULL;
      }
   }

   /*
    * Setup the matrix of pointers to grids.  When finished, table[time][var]
    * will point to the grid_info struct for every timestep and variable.
    */

   g = db->FirstGrid;

   for (it=0;it<db->NumTimes;it++) {
      /* search grid list for correct timestep */

      while (v5dYYDDDtoDays(g->DateStamp) <
             v5dYYDDDtoDays(db->DateStamp[it]) ||
             (v5dYYDDDtoDays(g->DateStamp) ==
              v5dYYDDDtoDays(db->DateStamp[it]) &&
              v5dHHMMSStoSeconds(g->TimeStamp) <
              v5dHHMMSStoSeconds(db->TimeStamp[it]))) {
         g = g->Next;
         if (!g)  {
            assert(g);
         }
      }

      if (g->DateStamp==db->DateStamp[it] && g->TimeStamp==db->TimeStamp[it]) {
         /* Found the timestep!! */

         for (iv=0;iv<db->NumVars;iv++) {
            struct grid_info *g2;

            /* search grid list for correct variable */
            g2 = g;
            while (strcmp(g2->VarName,db->VarNames[iv])!=0
                   && g2->DateStamp==db->DateStamp[it] && g2->TimeStamp==db->TimeStamp[it]) {
               g2 = g2->Next;
               if (!g2) break;
            }

            if (g2 && strcmp(g2->VarName,db->VarNames[iv])==0
                   && g2->DateStamp==db->DateStamp[it] && g2->TimeStamp==db->TimeStamp[it]) {
               struct grid_info *prev;

               /* Found the variable!! */
               db->Matrix[it][iv] = g2;

               /* There might be more grids with the same timestamp and */
               /* variable name so we chain them together... */
               prev = g2;
               while (1) {
                  g2 = g2->Next;
                  if (g2 && strcmp(g2->VarName,db->VarNames[iv])==0
                      && g2->DateStamp==db->DateStamp[it] && g2->TimeStamp==db->TimeStamp[it]){
                     /* another grid w/ same time and variable name */
                     prev->Sibling = g2;
                     prev = g2;
                  }
                  else {
                     break;
                  }
               }
               prev->Sibling = NULL;
               /* Done chaining */

            }
            else {
               /* Didn't find the variable */
               db->Matrix[it][iv] = NULL;
            }
         }

      }
      else {
         /* didn't find the timestep */
         for (iv=0;iv<db->NumVars;iv++) {
            db->Matrix[it][iv] = NULL;
         }
      }
   }

}




static int is_vcs_selected( struct grid_db *db, struct vcs *vcs )
{
   int i;

   for (i=0;i<db->NumVcs;i++) {
      if (db->VcsList[i]==vcs) {
         return db->VcsSelected[i];
      }
   }
   return 0;
}



/*
 * When we create the v5d output file we need to know how many grid levels
 * are to be produced for each variable.  This function tries to determine
 * these values.
 *
 * Input:  db - the grid data base
 * Output:  nl - the array of grid level values, one per variable
 */
void estimate_grid_levels( struct grid_db *db, int nl[] )
{
  int var, time;
  int nvcs, ivcs, index, vcs_indices[IMAXPROJ], unique; /* WLH 7-19-96 */


  for (var=0; var<db->NumVars; var++) {
    nvcs = 0;
    nl[var] = 0;
    if (db->VarSelected[var]) {

      for (time=0; time<db->NumTimes; time++) {
        if (db->TimeSelected[time]) {
          /* Determine Nl for this timestep and variable */
          struct grid_info *g;
          int one_level_count = 0;
          for (g=db->Matrix[time][var]; g; g=g->Sibling) {
            if (is_vcs_selected(db,g->Vcs)) {
              if (g->Vcs->Nl==1) {
                /* another grid w/ one level */
                unique = 1;
                for (ivcs=0; ivcs<nvcs; ivcs++) {
                  index = lookup_vcs(db, g->Vcs);
                  if (vcs_indices[ivcs] == index) {
                    unique = 0;
                    break;
                  }
                }
                if (unique) {
                  vcs_indices[nvcs] = index;
                  nvcs++;
                  one_level_count++;
                }
              }
              else {
                /* check if max so far */
                if (g->Vcs->Nl > nl[var]) {
                  nl[var] = g->Vcs->Nl;
                }
              }
            }
          }
          if (one_level_count>nl[var]) {
            nl[var] = one_level_count;
          }
        }
      }
    }
/*     printf("Nl[%s] = %d\n", db->VarNames[var], nl[var] );*/
  }

}



/*
 * Determine a reasonable default vertical coordinate system for the output
 * file.
 * Input:  db - the grid database
 *         max_out_nl - max grid levels for any VCS returned
 * Output:  vcs - the VCS number
 *          vcsargs - array of arguments.
 */
void find_default_vcs( struct grid_db *db, int max_out_nl, int *vcs,
                       float *vcsargs )
{
   int i;
   int one_level_count, maxnl, maxnl_index;

   one_level_count = 0;
   maxnl = 0;
   maxnl_index = -1;

   for (i=0; i<db->NumVcs; i++) {
      if (db->VcsSelected[i]) {
         if (db->VcsList[i]->Nl==1) {
            one_level_count++;
         }
         else {
            if (db->VcsList[i]->Nl > maxnl) {
               maxnl = db->VcsList[i]->Nl;
               maxnl_index = i;
            }
         }
      }
   }

   if (maxnl > one_level_count) {
      /* use one the VCS with the most levels */
      assert( maxnl_index>=0 );
      if (db->VcsList[maxnl_index]->Kind==VERT_EPA) {
         /* Special case:  v5d files don't directly support this VCS so */
         /* we have to "guestimate" a new one. */
         float height[MAXLEVELS];
         *vcs = VERT_UNEQUAL_KM;
         for (i=0;i<maxnl;i++) {
            level_to_height( (float) i, &height[i],
                             db->VcsList[maxnl_index], 0.0 );
         }
         memcpy( vcsargs, height, MAXVERTARGS*sizeof(float) );
      }
      else {
         *vcs = db->VcsList[maxnl_index]->Kind;
         memcpy( vcsargs, db->VcsList[maxnl_index]->Args,
                 MAXVERTARGS*sizeof(float));
      }
   }
   else {
      /* make a new VCS out of a "stack" of 1-D slices */
      int j;
      int n = 0;
      float height[MAXLEVELS];
      struct vcs *first_selected = NULL;
      for (i=0; i<db->NumVcs; i++) {
         if (db->VcsSelected[i] && db->VcsList[i]->Nl==1 && n<MAXLEVELS) {
            first_selected = db->VcsList[i];
            height[n] = db->VcsList[i]->Args[0];
            n++;
         }
      }
      /* bubble sort the heights */
      for (i=0;i<n-1;i++) {
         for (j=i+1;j<n;j++) {
            if (height[j]<height[i]) {
               /* swap */
               float tmp = height[i];
               height[i] = height[j];
               height[j] = tmp;
            }
         }
      }
      /* return the result */
      if (n==1) {
         assert(first_selected);
         *vcs = first_selected->Kind;
         vcsargs[0] = first_selected->Args[0];
         vcsargs[1] = first_selected->Args[1];
      }
      else {
         *vcs = VERT_UNEQUAL_KM;
         /* copy height values */
         for (i=0;i<n;i++) {
            vcsargs[i] = height[i];
         }
         /* extrapolate remaining height values */
         for (i=n;i<max_out_nl;i++) {
            vcsargs[i] = height[n-1] + (i-n) * (height[n-1]-height[n-2]);
         }
      }
   }

}



/*
 * Compute the lowlev value for each physical variable.  The lowlev value
 * indicates at which grid level the grid data actually begins.  This, in
 * conjunction with the nl-per-variable value circumvents the problem of
 * many layers of missing values.
 *
 * Input:  db - the grid database
 * Output:  lowlev - array [NumVars] of grid level positions
 */
void find_lowlev_values( struct grid_db *db, float lowlev[] )
{
   int var;

   for (var=0;var<db->NumVars;var++) {

      /* scan all selected timesteps' VCS's for height values... */


      lowlev[var] = 0;
   }

}


/* NEW */


/*
 * Find the minimum and maximum vertical coordinates of all selected
 * grids for variable 'var'.
 */
static void find_min_max_heights( struct grid_db *db, int var,
                                  float *min, float *max )
{
   int time;

   *min = 1.0e30;
   *max = -1.0e30;

   for (time=0; time<db->NumTimes; time++) {
      if (db->TimeSelected[time]) {
         struct grid_info *g;
         for (g=db->Matrix[time][var]; g; g=g->Sibling) {
            if (g->SelectBits==ALL_BITS) {
               float bottom, top;
               level_to_height( 0.0, &bottom, g->Vcs, 0.0 );
               level_to_height( (float) (g->Vcs->Nl-1), &top, g->Vcs, 0.0 );
               if (bottom < *min) {
                  *min = bottom;
               }
               if (top > *max) {
                  *max = top;
               }
            }
         }
      }
   }
}



/*
 * When we create the v5d output file we need to know, for each physical
 * variable, how many grid levels are to be produced, and the location of
 * the bottom-most one.  This function tries to determine these values.
 *
 * Input:  db - the grid data base
 * Output:  lowlev - position of lowest grid level, one per variable
 *          nl - the array of grid level values, one per variable
 */
void compute_grid_levels( struct grid_db *db, struct vcs *outvcs,
                          int lowlev[], int nl[] )
{
   int var, time;

   for (var=0; var<db->NumVars; var++) {
      if (db->VarSelected[var]) {

         float min, max;
         float minlevel, maxlevel;
         int iminlev, imaxlev;

         find_min_max_heights( db, var, &min, &max );

         if (height_to_level( min, &minlevel, outvcs, 0.0 )) {
            iminlev = (int) minlevel;
         }
         else {
            iminlev = 0;
         }

         if (height_to_level( max, &maxlevel, outvcs, 0.0 )) {
            imaxlev = (int) (maxlevel+0.99999);
         }
         else {
            imaxlev = outvcs->Nl-1;
         }

#if V5D_VERSION >= 42
         /* Only Vis5D version 4.2 or later support the lowlev scheme */
         lowlev[var] = iminlev;
         nl[var] = imaxlev - iminlev + 1;
#else
         lowlev[var] = 0;
         nl[var] = imaxlev+1;
#endif
      }
      else {
         lowlev[var] = nl[var] = 0;
      }
      printf("%s: lowlev=%d nl=%d\n", db->VarNames[var], lowlev[var], nl[var]);
   }
}




/*
 * Find the min and max lat/lon values for the given projection.
 * Input:  p - a map projection
 * Output:  minlat, maxlat, minlon, maxlon - min and max lat and lon values
 */
static void find_projection_extents( struct projection *p,
                                     float *min_lat, float *max_lat,
                                     float *min_lon, float *max_lon )
{
   /* TODO: this is inefficient: only have to check the perimeter! */
   int i, j;
   float minlat, maxlat, minlon, maxlon;
   float lat, lon;

   minlat = 90.0;
   maxlat = -90.0;
   minlon = 180.0;
   maxlon = -180.0;

   for (i=0;i<p->Nr;i++) {
      for (j=0;j<p->Nc;j++) {
         if (rowcol_to_latlon( (float) i, (float) j, &lat, &lon, p )) {
            if (lat<minlat)  minlat = lat;
            if (lat>maxlat)  maxlat = lat;
            if (lon<minlon)  minlon = lon;
            if (lon>maxlon)  maxlon = lon;
         }
      }
   }

   *min_lat = minlat;
   *max_lat = maxlat;
   *min_lon = minlon;
   *max_lon = maxlon;
}




/*
 * Scan the grid list to determine various output file parameters such as:
 *   1. Rows, columns, max grid levels
 *   2. Map projection
 *   3. Vertical coordinate system
 *
 * Input:  db - the grid_db struct.
 *         v - the v5dstruct to return results through
 *         rowcol_flag - boolean: compute Nr, Nc, Nl?
 *         proj_flag - boolean: compute map projection?
 *         vert_flag - boolean: compute vertical coordinate system?
 * Output:  v - contains computed parameters
 */
void setup_defaults( struct grid_db *db, v5dstruct *v,
                     int rowcol_flag, int proj_flag, int vert_flag )
{
   int i;

   /*** Grid Dimensions ***/
   if (rowcol_flag) {

      /* Get rows and columns from first selected projection */
      for (i=0;i<db->NumProj;i++) {
         if (db->ProjSelected[i]) {
            v->Nr = db->ProjList[i]->Nr;
            v->Nc = db->ProjList[i]->Nc;
            break;
         }
      }

      estimate_grid_levels( db, v->Nl );
   }

   /*** Projection ***/
   if (proj_flag) {
      /* first selected projection */
      for (i=0;i<db->NumProj;i++) {
         if (db->ProjSelected[i]) {
            if (db->ProjList[i]->Kind==PROJ_EPA) {
               /* Special case: v5d files don't directly support this */
               /* projection so we have to "guestimate" a new one. */
               float minlat, maxlat, minlon, maxlon;
               float args[4];
               find_projection_extents( db->ProjList[i],
                                        &minlat, &maxlat, &minlon, &maxlon);
               v->Projection = PROJ_LINEAR;
               args[0] = maxlat;
               args[1] = maxlon;
               args[2] = (maxlat-minlat) / (db->ProjList[i]->Nr-1);
               args[3] = (maxlon-minlon) / (db->ProjList[i]->Nc-1);
               memcpy( v->ProjArgs, args, 4*sizeof(float) );
            }
            else {
               v->Projection = db->ProjList[i]->Kind;
               memcpy( v->ProjArgs, db->ProjList[i]->Args,
                       MAXPROJARGS*sizeof(float));
            }
            break;
         }
      }
   }

   /*** Vert Coord Sys ***/
   if (vert_flag) {
      int maxnl = 0;
      for (i=0;i<db->NumVars;i++) {
         if (v->Nl[i]>maxnl) {
            maxnl = v->Nl[i];
         }
      }
      find_default_vcs( db, maxnl, &v->VerticalSystem, v->VertArgs );
   }
}

