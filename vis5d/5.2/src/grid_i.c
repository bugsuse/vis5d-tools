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

/* grid.c */


/*
 * Functions for manipulating grids and lists of grids.
 */



#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "grid_i.h"
#include "memory_i.h"
#include "projlist_i.h"



/*
 * Allocate a grid_info struct and initialize it.
 */
struct grid_info *alloc_grid_info( void )
{
   struct grid_info *g;

   g = (struct grid_info *) calloc( 1, sizeof(struct grid_info) );
   if (g) {
      /* all fields will be 0 thanks to calloc but here's some special cases */
      g->TimeStep = g->VarNum = g->Position = -1;
      g->Proj = NULL;
      g->Vcs = NULL;
      g->SelectBits = ALL_BITS;
   }
   return g;
}



/*
 * Deallocate a grid_info struct.
 */
void free_grid_info( struct grid_info *info )
{
   if (info->FileName) {
      FREE( info->FileName, 1000 );
   }
   if (info->VarName) {
      FREE( info->VarName, 1001 );
   }
   if (info->Units) {
      FREE( info->Units, 1002 );
   }
   if (info->Data) {
      FREE( info->Data, 1003 );
   }
   /* "erase" data to help with debugging */
   /*memset( info, 0xff, sizeof(struct grid_info) );*/
   FREE( info, 1004 );
}



/*
 * Allocate a grid_list struct and initialize it.
 */
struct grid_db *alloc_grid_db()
{
   struct grid_db *gl;

   /* Allocate w/ every field initialized to zero */
   gl = (struct grid_db *) calloc( sizeof(struct grid_db), 1 );

   return gl;
}


/*
 * Deallocate a grid_db struct.
 */
void free_grid_db( struct grid_db *db )
{
   struct grid_info *g, *next;
   int i;

   /* free grid_info list */
   for (g=db->FirstGrid; g; g=next) {
      next = g->Next;
      free_grid_info( g );
   }

   /* free varname and units strings */
   for (i=0;i<db->NumVars;i++) {
      FREE( db->VarNames[i], 1005 );
      if (db->Units[i]) {
         FREE( db->Units[i], 1005 );
      }
   }

   /* free projections */
   for (i=0;i<db->NumProj;i++) {
      FREE( db->ProjList[i], 1006 );
   }

   /* free VCSs */
   for (i=0;i<db->NumVcs;i++) {
      FREE( db->VcsList[i]->Args, 1007 );
      FREE( db->VcsList[i], 1008 );
   }

   FREE( db, 1008 );
}



/*
 * Append a grid_info struct onto the tail of the list.
 */
void append_grid( struct grid_info *grid, struct grid_db *db )
{
   int i, inlist;

   if (db->LastGrid) {
      db->LastGrid->Next = grid;
      db->LastGrid = grid;
   }
   else {
      db->FirstGrid = db->LastGrid = grid;
   }
   grid->Next = NULL;
   db->NumGrids++;
   db->Sorted = 0;
}



/*
 * Remove a grid_info struct from a list.
 * Return:  1 = success, 0 = error
 */
int remove_grid( struct grid_info *grid, struct grid_db *db )
{
   struct grid_info *pred;
   int found;

   /* Find predecessor to the grid */
   if (db->FirstGrid==grid) {
      /* grid to remove is first in list */
      db->FirstGrid = grid->Next;
      pred = NULL;
      found = 1;
   }
   else {
      for (pred=db->FirstGrid; pred; pred=pred->Next) {
         if (pred->Next==grid) {
            /* found predecessor! */
            pred->Next = grid->Next;
            found = 1;
            break;
         }
      }
   }
   if (db->LastGrid==grid) {
      db->LastGrid = pred;
   }
   
   db->NumGrids--;
   return found;
}



/*
 * Free all the grids and associated data in a grid_db.
 */
void free_all_grids( struct grid_db *db )
{
   struct grid_info *g, *nextg;
   int i, j;

   /* free grids */
   for (g=db->FirstGrid; g; g=nextg) {
      nextg = g->Next;
      free_grid_info( g );
   }
   db->FirstGrid = db->LastGrid = NULL;
   db->NumGrids = 0;

   /* Free map projections */
   for (i=0;i<db->NumProj;i++) {
      FREE( db->ProjList[i]->Args, 1100 );
      FREE( db->ProjList[i], 1101 );
      db->ProjList[i] = NULL;
   }
   db->NumProj = 0;

   /* Free VCSs */
   for (i=0;i<db->NumVcs;i++) {
      FREE( db->VcsList[i]->Args, 1102 );
      FREE( db->VcsList[i], 1103 );
      db->VcsList[i] = NULL;
   }
   db->NumVcs = 0;

   /* Clear pointers in grid matrix */
   for (i=0;i<db->NumTimes;i++) {
      for (j=0;j<db->NumVars;j++) {
         db->Matrix[i][j] = NULL;
      }
   }

   db->NumVars = 0;
   db->NumTimes = 0;

   db->Sorted = 0;
}



/*
 * Print a grid list to stdout.  Just used for debugging.
 */
void print_grid_list( struct grid_db *db )
{
   struct grid_info *g;
   int i = 1;

   printf("  Grid  Date  Time    Variable    Nr  Nc  Nl  Proj#  Vcs#  Filename\n");
   for (g=db->FirstGrid; g; g=g->Next) {
      int projnum = lookup_proj( db, g->Proj );
      int vcsnum = lookup_vcs( db, g->Vcs );
      printf("%c %4d  %05d %06d  %-10s %3d %3d %3d   %3d   %3d   %s\n",
              g->SelectBits==ALL_BITS ? '*': ' ',
              i, g->DateStamp, g->TimeStamp, g->VarName, g->Nr, g->Nc, g->Nl,
              projnum, vcsnum,
              g->FileName );
      i++;
   }

   printf("*=include grid in output file\n");
}




/*
 * "Print" the grid list to generate an array of strings.
 * Return null if no grids in list.
 */
char **sprint_grid_list( struct grid_db *db )
{
   struct grid_info *g;
   int i = 0;
   char **vector;

   if (db->NumGrids==0) {
      return NULL;
   }

   vector = (char **) MALLOC( db->NumGrids * sizeof(char *) );

   /* See gui.h for column headings */
   for (g=db->FirstGrid; g; g=g->Next) {
      int projnum = lookup_proj( db, g->Proj );
      int vcsnum = lookup_vcs( db, g->Vcs );
      vector[i] = (char *) MALLOC( 1000 );
      sprintf( vector[i],
              "%4d  %05d %06d  %-10s%3d %3d %3d   %2d    %2d   %s%c",
              i+1,
              g->DateStamp, g->TimeStamp,
              g->VarName,
              g->Nr, g->Nc, g->Nl,
              projnum, vcsnum,
              g->FileName,
              g->Sibling ? ',' : ' ' );
      i++;
   }

   return vector;
}



/*
 * Find the maximum number of grid levels of all grids in the list.
 * Input:  db - the grid data base
 * Return:  max number of grid levels in list or -1 if empty list
 */
int find_max_levels( struct grid_db *db )
{
   struct grid_info *g;
   int maxnl = -1;

   for (g=db->FirstGrid; g; g=g->Next) {
      if (g->Nl>maxnl) {
         maxnl = g->Nl;
      }
   }

   return maxnl;
}
