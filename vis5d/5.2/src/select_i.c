/* select.c */
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


#include "grid_i.h"


/*
 * Set or clear the selection bits for all grids in a particular table
 * position.
 * Input:  table - the grid table
 *         time - which timestep
 *         var - which variable
 *         bitmask - which bits to change
 *         state - 1 means set the bits, 0 means clear the bits
 */
static void set_cell_selection( struct grid_db *db, int time, int var,
                                int bitmask, int state )
{
   struct grid_info *g;

   g = db->Matrix[time][var];
   while (g) {
      if (state) {
         g->SelectBits |= bitmask;
      }
      else {
         g->SelectBits &= ~bitmask;
      }
      g->NewSelState = 1;
      g = g->Sibling;
   }
}


#ifdef LEAVEOUT
/*
 * Test if any grid in the chain attached to the named cell has the
 * specified selection bits set.
 */
static int is_cell_selected( struct grid_db *db, int time, int var,
                             int bitmask )
{
   struct grid_info *g;

   g = db->Matrix[time][var];
   while (g) {
      if ((g->SelectBits & bitmask) == bitmask) {
         return 1;
      }
      g = g->Sibling;
   }
   return 0;
}
#endif


/*
 * Mark a time step and all associated grids as selected or unselected
 * according to the state.
 * Input:  db - the grid database
 *         time - which timestep (starting at 0)
 *         state - selection status.
 * Return:  1=there was a side-effect, 0=no side-effect
 *          A side-effect occurs when changing the selection status of a
 *          timestep changes the selection status of some variable.
 */
void select_time( struct grid_db *db, int time, int state )
{
   int var;

   db->TimeSelected[time] = state;

   for (var=0;var<db->NumVars;var++) {
      if (db->Matrix[time][var]) {
         set_cell_selection( db, time, var, TIME_BIT, state );
      }
   }
}



/*
 * Mark a variable and all associated grids as selected or unselected
 * according to the state.
 */
void select_variable( struct grid_db *db, int var, int state )
{
   int time;

   db->VarSelected[var] = state;

   for (time=0;time<db->NumTimes;time++) {
      if (db->Matrix[time][var]) {
         set_cell_selection( db, time, var, VAR_BIT, state );
      }
   }
}




/*
 * Mark a map projection and all associated grids as selected or unselected
 * according to the state.
 */
void select_projection( struct grid_db *db, int projnum, int state )
{
   int time, var;
   struct projection *p;

   db->ProjSelected[projnum] = state;

   p = db->ProjList[projnum];

   for (time=0;time<db->NumTimes;time++) {
      for (var=0;var<db->NumVars;var++) {
         struct grid_info *g;

         g = db->Matrix[time][var];
         while (g) {
            if (g->Proj == p) {
               if (state) {
                  g->SelectBits |= PROJ_BIT;
               }
               else {
                  g->SelectBits &= ~PROJ_BIT;
               }
               g->NewSelState = 1;
            }
            g = g->Sibling;
         }
      }
   }
}



/*
 * Mark a vertical coordinate system and all associated grids as selected
 * or unselected according to the state.
 */
void select_vcs( struct grid_db *db, int vcsnum, int state )
{
   int time, var;
   struct vcs *v;

   db->VcsSelected[vcsnum] = state;

   v = db->VcsList[vcsnum];

   for (time=0;time<db->NumTimes;time++) {
      for (var=0;var<db->NumVars;var++) {
         struct grid_info *g;

         g = db->Matrix[time][var];
         while (g) {
            if (g->Vcs == v) {
               if (state) {
                  g->SelectBits |= VCS_BIT;
               }
               else {
                  g->SelectBits &= ~VCS_BIT;
               }
               g->NewSelState = 1;
            }
            g = g->Sibling;
         }
      }
   }
}



/*
 * Mark all grids in the table as selected or unselected according to 
 * bitmask and state.
 */
void select_all( struct grid_db *db, int bitmask, int state )
{
   int time, var, proj, vcs;

   for (time=0;time<db->NumTimes;time++) {
      for (var=0;var<db->NumVars;var++) {
         set_cell_selection( db, time, var, bitmask, state );
      }
   }

   if (bitmask & TIME_BIT) {
      for (time=0;time<db->NumTimes;time++) {
         db->TimeSelected[time] = state;
      }
   }

   if (bitmask & VAR_BIT) {
      for (var=0;var<db->NumVars;var++) {
         db->VarSelected[var] = state;
      }
   }

   if (bitmask & PROJ_BIT) {
      for (proj=0;proj<db->NumProj;proj++) {
         db->ProjSelected[proj] = state;
      }
   }

   if (bitmask & VCS_BIT) {
      for (vcs=0;vcs<db->NumVcs;vcs++) {
         db->VcsSelected[vcs] = state;
      }
   }
}


#ifdef LEAVEOUT
/*
 * Find the greatest number of levels of any grids for the specified variable.
 * Input:  table - which table
 *         var - which variable in [0,NumVars-1]
 * Return:  max number of levels for the named variable
 */
int table_max_nl( db, var )
struct grid_db *db;
int var;
{
   struct grid_info *g;
   int it, maxnl;
   int count, all_2d;

   maxnl = 0;
   count = 0;
   all_2d = 1;

   for (it=0;it<db->NumTimes;it++) {
      g = db->Matrix[it][var];
      while (g) {
         if (g->SelectBits==ALL_BITS) {
            if (g->Nl > maxnl) {
               maxnl = g->Nl;
            }
            if (g->Nl!=1) {
               all_2d = 0;
            }
            count++;
         }
         g = g->Sibling;
      }
   }

   /* If all grids in the chain are really 2-D grids (ala McIDAS) then */
   /* we'll probably stack those 2-D grids to make a 3-D grid. */
   if (all_2d && maxnl==1 && count>1) {
      maxnl = count;
   }

   return maxnl;
}
#endif

