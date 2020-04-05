/* read_gr3d.h */
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


/*
 * Functions for reading with McIDAS GR3D files
 */


#ifndef READ_GR3D_H
#define READ_GR3D_H


#include "grid_i.h"



/*
 * Get a description of the contents of a McIDAS GR3D file.
 */
extern int get_gr3d_info( char *name, struct grid_db *db );


/*
 * Get actual grid data.
 */
extern float *get_gr3d_data( struct grid_info *g );



#endif
