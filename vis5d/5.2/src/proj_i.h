/* proj.h */
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
 * Map projection / coordinate transformation
 */


#ifndef PROJ_i_H
#define PROJ_i_H


extern int rowcol_to_latlon_i( float row, float col,
                             float *lat, float *lon,
                             struct projection *proj );



extern int latlon_to_rowcol_i( float lat, float lon,
                             float *row, float *col,
                             struct projection *proj );



extern float proj_resolution( struct projection *proj );


extern int height_to_level( float height, float *level,
                            struct vcs *vcs, float topo_elev );


extern int level_to_height( float level, float *height,
                            struct vcs *vcs, float topo_elev );


#endif
