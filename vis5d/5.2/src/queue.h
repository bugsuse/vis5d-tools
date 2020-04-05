
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


#ifndef QUEUE_H
#define QUEUE_H


#include "globals.h"



#define TASK_NULL           0
#define TASK_ISOSURFACE     1
#define TASK_HSLICE         2
#define TASK_VSLICE         3
#define TASK_CHSLICE        4
#define TASK_CVSLICE        5
#define TASK_HWIND          6
#define TASK_VWIND          7
#define TASK_TRAJ           8
#define TASK_EXT_FUNC       9
#define TASK_HSTREAM       10
#define TASK_VSTREAM       11
#define TASK_TRAJ_RECOLOR  12
#define TASK_TOPO_RECOLOR  13
#define TASK_HCLIP         14
#define TASK_VCLIP         15
#define TASK_TEXT_PLOT     16
#define TASK_QUIT         100


extern void init_queue( void );

extern void terminate_queue( void );

extern void get_queue_info( int *size, int *waiters );

extern void get_qentry( Context *ctx, Irregular_Context *itx,
                        int *type,
                        int *i1, int *i2, int *i3,
                        float *f1, float *f2, float *f3,
                        float *f4, float *f5 );

extern void request_quit( Context ctx );

extern void new_isosurface( Context ctx, int time, int var, int urgent );

extern void request_isosurface( Context ctx, int time, int var, int urgent );

extern void request_hslice( Context ctx, int time, int var, int urgent );

extern void request_vslice( Context ctx, int time, int var, int urgent );

extern void request_chslice( Context ctx, int time, int var, int urgent );

extern void request_cvslice( Context ctx, int time, int var, int urgent );

extern void request_hwindslice( Display_Context dtx, int time, int ws, int urgent );

extern void request_vwindslice( Display_Context dtx, int time, int ws, int urgent );

extern void request_hstreamslice( Display_Context dtx, int time, int ws, int urgent );

extern void request_vstreamslice( Display_Context dtx, int time, int ws, int urgent );

extern void request_hclip( Context ctx, int num);

extern void request_vclip( Context ctx, int num);

extern void request_traj( Display_Context dtx,
                          float row, float col, float lev,
                          int time, int group, int rib,
                          float step, float len );

extern void request_traj_recoloring( Context ctx, int traj_set );


extern void request_ext_func( Context ctx, int time, int var );


extern void request_topo_recoloring( Context ctx );

extern void request_text_plot( Irregular_Context itx, int time, int var, int urgent);
#endif
