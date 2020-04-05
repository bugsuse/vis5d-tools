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

/* analyze.h */



#ifndef ANALYZE_H
#define ANALYZE_H


#include "grid_i.h"
#include "v5d.h"


extern void analyze_grids( struct grid_db *db );


extern void estimate_grid_levels( struct grid_db *db, int nl[] );


extern void compute_grid_levels( struct grid_db *db, struct vcs *outvcs,
                                 int lowlev[], int nl[] );


extern void find_default_vcs( struct grid_db *db, int max_out_nl,
                              int *vcs, float *vcsargs );


extern void setup_defaults( struct grid_db *db, v5dstruct *v,
                            int rowcol_flag, int proj_flag, int vert_flag );


#endif
