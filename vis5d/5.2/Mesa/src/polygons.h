/* polygons.h */

/*
 * Mesa 3-D graphics library
 * Version:  1.2
 * Copyright (C) 1995  Brian Paul  (brianp@ssec.wisc.edu)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */


/*
$Id: polygons.h,v 1.5 1995/12/30 00:58:35 brianp Exp $

$Log: polygons.h,v $
 * Revision 1.5  1995/12/30  00:58:35  brianp
 * removed gl_compute_z and gl_polygon_edge
 *
 * Revision 1.4  1995/10/24  20:52:02  brianp
 * added prototypes for gl_compute_z and gl_polygon_edge
 *
 * Revision 1.3  1995/05/22  20:59:34  brianp
 * Release 1.2
 *
 * Revision 1.2  1995/03/04  19:25:29  brianp
 * 1.1 beta revision
 *
 * Revision 1.1  1995/02/24  14:26:49  brianp
 * Initial revision
 *
 */


#ifndef POLYGONS_H
#define POLYGONS_H


#include "GL/gl.h"


extern void gl_set_polygon_function( void );


#endif

