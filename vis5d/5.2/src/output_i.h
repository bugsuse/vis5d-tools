/* output.h */
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


#ifndef OUTPUT_H
#define OUTPUT_H


#include "grid_i.h"
#include "v5d.h"


extern void make_output_file( struct grid_db *db, v5dstruct *v5d,
                              char *filename, int maxnl,
                              int average, int compressmode );

extern int make_output_ctx( struct grid_db *db, v5dstruct *v5d,
                              char *filename, char *ctxname, int maxnl,
                              int average, int compressmode, int mbsi,
                              int mf, int mc );
#endif
