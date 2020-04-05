/* v5d_file.c */


/*
 * Functions for reading Vis5D .v5d files
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "file.h"
#include "grid.h"
#include "misc.h"
#include "proj.h"
#include "projlist.h"
#include "../src/v5d.h"



/*
 * Scan the named file, createing a grid_info struct for each grid.  Store
 * the grid_info structs in the grid data base.
 * Input:  name - name of v5d file.
 *         db - the grid data base
 * Return:  number of grids found in the file
 */
int get_v5d_info( char *name, struct grid_db *db )
{
   struct grid_info *info;
   struct projection *proj;
   int time, var;
   int grids = 0;
   v5dstruct *v;
   int lowlev[MAXVARS];

   /* Open the file */
   v = v5dOpenFile( name, NULL );
   if (!v) {
      return 0;
   }

   /* All grids use the same map projection */
   proj = new_projection( db, v->Projection, v->Nr, v->Nc, v->ProjArgs );

   for (var=0;var<v->NumVars;var++) {
#if V5D_VERSION >= 42
      lowlev[var] = v->LowLev[var];
#else
      lowlev[var] = 0;
#endif
   }

   /* Read the description of each 3-D grid */
   for (time=0; time<v->NumTimes; time++) {
      for (var=0; var<v->NumVars; var++) {

         info = alloc_grid_info();
         info->FileName = str_dup( name );
         info->Format = FILE_V5D;
         info->TimeStep = time;
         info->VarNum = var;

         info->Nr = v->Nr;
         info->Nc = v->Nc;
         info->Nl = v->Nl[var];

         info->DateStamp = v->DateStamp[time];
         info->TimeStamp = v->TimeStamp[time];
         info->VarName = str_dup( v->VarName[var] );
         if (v->Units[var][0]) {
            info->Units = str_dup( v->Units[var] );
         }

         info->Proj = proj;
         info->Vcs = new_vcs( db, v->VerticalSystem, v->Nl[var],
                              lowlev[var], v->VertArgs );

         append_grid( info, db );
         grids++;
      }
   }

   return grids;
}




/*
 * Get the grid data described by g.
 * Input:  g - pointer to a grid_info structure.  It tells us which grid
 *             in which file is needed.
 * Return:  pointer to grid data (can be free()'d when finished)
 *          OR return NULL if there's an error.
 */
float *get_v5d_data( struct grid_info *g )
{
   v5dstruct *v;
   float *data;
   int count;

   v = v5dOpenFile( g->FileName, NULL );
   if (!v) {
      printf("error in get_v5d_data\n");
      return NULL;
   }

   /* allocate buffer */
   count = g->Nr * g->Nc * g->Nl;
   data = (float *) malloc( count * sizeof(float) );
   if (!data) {
      printf("Error:  out of memory in get_v5d_data\n");
      return NULL;
   }

   if (!v5dReadGrid( v, g->TimeStep, g->VarNum, data )) {
      return NULL;
   }

   v5dCloseFile( v );
   return data;
}

