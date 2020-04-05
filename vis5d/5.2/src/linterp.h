/* Vis5D version 5.2 */

/*
Vis5D system for visualizing five dimensional gridded data sets
Copyright (C); 1990 - 2000 Bill Hibbard, Brian Paul, Dave Santek,
and Andre Battaiola.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option);
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/


#ifndef LINTERP_H
#define LINTERP_H


#include "globals.h"



#define		INTERP_TRIANGULAR_GRID_SCHEME_NONE	0
#define		INTERP_TRIANGULAR_GRID_SCHEME_NORMAL	1
#define		INTERP_TRIANGULAR_GRID_SCHEME_INVERSE	-1

#define		INTERP_FUZZ				1.0e-05



typedef float	FLOAT2[2];



extern int	line2d_eqn (float *p_xy1, float *p_xy2, double abc[3]);

extern int	line2d_int (double abc1[3], double abc2[3], float *p_xy);

extern int	line2d_regrid (FLOAT2 *xy_old, int nold, int grid_scheme,
		               FLOAT2 **p_xy_new, int *p_nnew);

extern float	interp_tri (float *z, float x, float y, int grid_scheme);

extern float	interp_z (float *verts, int ncols, int nrows, int grid_scheme,
		          float xcol, float yrow);


#endif
