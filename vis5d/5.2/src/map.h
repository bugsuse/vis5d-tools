
/* Vis5D version 5.2 */

/*
Vis5D system for visualizing five dimensional gridded data sets
Copyright (C) 1990 - 2000 Bill Hibbard, Brian Paul, Dave Santek,
and Andre Battaiola.

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


#ifndef MAP_H
#define MAP_H


#include "globals.h"


extern int init_map( Display_Context dtx, char *mapname );

extern int draw_map( Display_Context dtx, int time, int flat );

/* MJK 12.04.98 */
extern float    get_z_off (Display_Context dtx, float zmin, float zmax);

extern int      bend_line_to_fit_surf (float *verts, int ncols, int nrows,
                                       int grid_scheme,
                                       float xmin, float ymin,
                                       float xmax, float ymax, float zoff,
                                       float *xyz_in, int n_in,
                                       float *xyz_out);

extern int      bend_line_to_fit_topo (Display_Context dtx,
                                       float *xyz_in, int n_in,
                                       float *xyz_out);

extern void     bend_map_seg_to_fit_topo (Display_Context dtx);

#endif
