
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



#ifndef MISC_H
#define MISC_H


#include "globals.h"




extern void die( char *msg );

extern float round( float x );

extern int which( char *file, char *fullpath );

extern int installed( char *program );

extern int return_ctx_index_pos( Display_Context dtx, int index);

extern int return_ctx_time( Display_Context dtx, int index, int time);

extern void new_hslice_pos(Context ctx, float level, float *z, float *hgt );

extern void new_vslice_pos( Context ctx, float r, float c,
                            float *x, float *y, float *lat, float *lon );

extern void new_hwindslice_pos( Display_Context dtx, float level, float *z, float *hgt );

extern void new_vwindslice_pos( Display_Context dtx, float r, float c,
                            float *x, float *y, float *lat, float *lon );

extern void init_graphics_pos( Context ctx, int var );


extern void del_last_traj( Display_Context dtx );

extern void del_traj_group( Display_Context dtx, int g );



extern int free_isosurface( Context ctx, int time, int var );

extern int free_textplot( Irregular_Context itx, int time);

extern int free_hslice( Context ctx, int time, int var );

extern int free_vslice( Context ctx, int time, int var );

extern int free_chslice( Context ctx, int time, int var );

extern int free_cvslice( Context ctx, int time, int var );

extern int free_param_graphics( Context ctx, int var );

extern int free_hwind( Display_Context dtx, int time, int ws );

extern int free_vwind( Display_Context dtx, int time, int ws );

extern int free_hstream( Display_Context dtx, int time, int ws );

extern int free_vstream( Display_Context dtx, int time, int ws );

extern void free_all_graphics( Context ctx );

extern void turn_off_and_free_var_graphics( Context ctx, int var);

extern void turn_off_and_free_all_display_graphics( Display_Context dtx );

extern void recent( Context ctx, int ig, int ip );

extern int deallocate_lru( Context ctx );



#endif

