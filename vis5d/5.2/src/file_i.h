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

/* file.h */


#ifndef FILE_H
#define FILE_H


#include "grid_i.h"


/* Input File formats: */
#define FILE_UNKNOWN   0
#define FILE_GR3D      1
#define FILE_EPA       2
#define FILE_V5D       3
#define FILE_GRADS     4
#define FILE_UWVIS     5
#define FILE_GRID      6



/*
 * Return info about the named file.
 */
extern void get_file_info( char *name, struct grid_db *db );


extern float *get_file_data( struct grid_info *g );



#endif

