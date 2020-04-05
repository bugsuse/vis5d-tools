/* draw.h */

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
$Id: draw.h,v 1.9 1996/02/15 16:52:56 brianp Exp $

$Log: draw.h,v $
 * Revision 1.9  1996/02/15  16:52:56  brianp
 * added gl_transform_vb_part2
 *
 * Revision 1.8  1995/12/30  17:15:11  brianp
 * gl_eval_vertex now takes integer colors
 *
 * Revision 1.7  1995/12/20  17:28:15  brianp
 * removed gl_index and gl_color, now they're inlined
 *
 * Revision 1.6  1995/07/25  16:42:22  brianp
 * made changes for using CC.VertexFunc pointer
 *
 * Revision 1.5  1995/05/22  20:59:34  brianp
 * Release 1.2
 *
 * Revision 1.4  1995/03/09  19:05:36  brianp
 * fixed a typo
 *
 * Revision 1.3  1995/03/04  19:25:29  brianp
 * 1.1 beta revision
 *
 * Revision 1.2  1995/03/02  19:12:06  brianp
 * removed const from gl_eval_vertex()'s t argument
 *
 * Revision 1.1  1995/02/24  14:20:46  brianp
 * Initial revision
 *
 */


#ifndef DRAW_H
#define DRAW_H


#include "GL/gl.h"


extern void gl_transform_vb_part2( GLboolean alldone );


extern void gl_execute_vertex( GLfloat x, GLfloat y, GLfloat z, GLfloat w );


extern void gl_save_and_execute_vertex( GLfloat x, GLfloat y,
				        GLfloat z, GLfloat w );


extern void gl_eval_vertex( const GLfloat vertex[4], const GLfloat normal[3],
			    const GLint color[4], GLuint index,
                            const GLfloat texcoord[4] );


extern void gl_rasterpos( const GLfloat v[4] );


extern void gl_begin( GLenum p );


extern void gl_end( void );


#endif
