
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


#ifndef MEMORY_H
#define MEMORY_H


#include "globals.h"



#define NULL_TYPE 0
#define GRID_TYPE 1
#define IXPLANE_TYPE 2
#define PTFLAG_TYPE 3
#define PTAUX_TYPE 4
#define PCUBE_TYPE 5
#define POLFVERT_TYPE 6
#define NXA_TYPE 7
#define PNX_TYPE 8
#define TRISTRIPE_TYPE 9
#define VETPOL_TYPE 10
#define CVX_TYPE 11
#define CVY_TYPE 12
#define CVZ_TYPE 13
#define CNX_TYPE 14
#define CNY_TYPE 15
#define CNZ_TYPE 16
#define PTS_TYPE 17
#define HSLICE_TYPE 18
#define VSLICE_TYPE 19
#define MHRECT_TYPE 20
#define MVRECT_TYPE 21
#define CVX1H_TYPE 22
#define CVY1H_TYPE 23
#define CVZ1H_TYPE 24
#define CVX2H_TYPE 25
#define CVY2H_TYPE 26
#define CVZ2H_TYPE 27
#define CVX3H_TYPE 28
#define CVY3H_TYPE 29
#define CVZ3H_TYPE 30
#define CVX1V_TYPE 31
#define CVY1V_TYPE 32
#define CVZ1V_TYPE 33
#define CVX2V_TYPE 34
#define CVY2V_TYPE 35
#define CVZ2V_TYPE 36
#define CVX3V_TYPE 37
#define CVY3V_TYPE 38
#define CVZ3V_TYPE 39
#define VXH_TYPE 40
#define VYH_TYPE 41
#define VZH_TYPE 42
#define INDEXESH_TYPE 43
#define VXV_TYPE 44
#define VYV_TYPE 45
#define VZV_TYPE 46
#define INDEXESV_TYPE 47
#define WINDXH_TYPE 48
#define WINDYH_TYPE 49
#define WINDZH_TYPE 50
#define WINDXV_TYPE 51
#define WINDYV_TYPE 52
#define WINDZV_TYPE 53
#define TRAJX_TYPE 54
#define TRAJY_TYPE 55
#define TRAJZ_TYPE 56
#define TRAJXR_TYPE 57
#define TRAJYR_TYPE 58
#define TRAJZR_TYPE 59
#define START_TYPE 60
#define LEN_TYPE 61
#define STREAM1_TYPE 62
#define STREAM2_TYPE 63
#define STREAM3_TYPE 64
#define SOUND_TYPE 65
#define UWIND_TYPE 66
#define VWIND_TYPE 67
#define VERTDATA_TYPE 68

extern int init_memory( Context ctx, int bytes );

extern int init_shared_memory( Context ctx, void *start, int bytes );

extern int reinit_memory( Context ctx );

extern void *allocate( Context ctx, int bytes );

extern void *allocate_type( Context ctx, int bytes, int type );

extern void *pallocate( Context ctx, int bytes );

extern void deallocate( Context ctx, void *addr, int bytes );

extern int mem_available( Context ctx );

extern int mem_used( Display_Context dtx );



#endif
