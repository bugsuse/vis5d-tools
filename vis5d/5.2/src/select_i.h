/* select.h */
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


#ifndef SELECT_H
#define SELECT_H


#include "grid_i.h"


extern void select_time( struct grid_db *db, int time, int state );


extern void select_variable( struct grid_db *db, int var, int state );


extern void select_projection( struct grid_db *db, int projnum,
                               int state );

extern void select_vcs( struct grid_db *db, int vcsnum, int state );


extern void select_all( struct grid_db *db, int bitmask, int state );


#endif

