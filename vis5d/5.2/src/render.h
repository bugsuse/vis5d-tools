/* render.h */

/* VIS-5D version 5.2 */

/*
VIS-5D system for visualizing five dimensional gridded data sets
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


#ifndef RENDER_H
#define RENDER_H


#include "globals.h"



/* MJK 12.02.98 */
extern void float2string (Display_Context dtx, int icoord, float f, char *str);


extern void plot_string( char *str, float startx, float starty, float startz,
                         float base[], float up[], int rjustify );


extern void render_3d_only( Display_Context dtx, int GoTime );

extern void render_2d_only( Display_Context dtx );

extern void render_sounding_only( Display_Context dtx, int pixmapflag );


extern void render_everything( Display_Context dtx, int GoTime );

extern int check_for_valid_time( Context ctx, int dtxcurtime);

#endif

