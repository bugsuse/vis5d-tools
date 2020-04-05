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


#ifndef USERDATA_H
#define USERDATA_H

#include "globals.h"
#include "map.h"

extern int user_data_get_header (char file_name[], v5dstruct *v);

extern int user_data_get_grid (v5dstruct *v, int itime, int ivar,
                            float *grid_data);

extern int user_data_get_topo (Display_Context dtx, char topo_name[]);

extern int user_data_get_map (Display_Context dtx, char map_name[]);


#endif

