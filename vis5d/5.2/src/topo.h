
/* Vis5D version 5.2 */

/*
Vis5D system for visualizing five dimensional gridded data sets
Copyright (C) 1990-2000 Bill Hibbard, Brian Paul, Dave Santek,
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


#ifndef TOPO_H
#define TOPO_H


#include "globals.h"

/* MJK 12.02.98 */ 
#define TOPO_BASE_COLOR       (PACK_COLOR (160, 160, 160, 255))


extern float elevation( Display_Context dtx, float lat, float lon, int *water );


extern int init_topo( Display_Context dtx, char toponame[], int textureflag,
                      int hi_res );


extern void free_topo( Display_Context dtx );


extern void init_topo_color_table( unsigned int ct[], int size,
                                   float minght, float maxhgt );


extern void recolor_topo( Display_Context dtx, unsigned int ct[], int size );


extern void draw_topo( Display_Context dtx, int time,
                       int texture_flag, int flat_flag );


#endif

