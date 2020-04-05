/* list.h */

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
$Id: list.h,v 1.20 1996/02/26 15:10:12 brianp Exp $

$Log: list.h,v $
 * Revision 1.20  1996/02/26  15:10:12  brianp
 * use CC.Current.IntColor instead of CC.Current.Color
 *
 * Revision 1.19  1995/12/30  17:17:56  brianp
 * added direct glScale and glTranslate support
 *
 * Revision 1.18  1995/12/19  17:06:44  brianp
 * added gl_save_blendcolor, blendequation, colormaterial, edge_flag, matrixmode
 * removed gl_save_set_boolean, enum, float, int, uint
 *
 * Revision 1.17  1995/12/19  16:42:20  brianp
 * added gl_save_pixeltransfer
 *
 * Revision 1.16  1995/10/29  19:14:19  brianp
 * added glPolygonOffsetEXT display list support
 *
 * Revision 1.15  1995/10/14  17:42:34  brianp
 * added glClipPlane, glLightModel, and texture functions
 *
 * Revision 1.14  1995/10/04  19:35:07  brianp
 * replaced gl_save_normal with gl_save_normal3fv and gl_save_normal3f
 *
 * Revision 1.13  1995/08/31  18:34:16  brianp
 * added display list / glScissor support
 *
 * Revision 1.12  1995/08/01  20:53:35  brianp
 * added glPixelZoom support
 *
 * Revision 1.11  1995/07/25  16:41:54  brianp
 * made changes for using CC.VertexFunc pointer
 *
 * Revision 1.10  1995/06/22  14:27:50  brianp
 * added prototypes for gl_save_stencil* functions
 *
 * Revision 1.9  1995/05/22  20:59:34  brianp
 * Release 1.2
 *
 * Revision 1.8  1995/05/19  13:26:50  brianp
 * added display list support for selection/name stack functions
 *
 * Revision 1.7  1995/05/15  15:24:28  brianp
 * added gl_list_index()
 *
 * Revision 1.6  1995/05/10  18:45:04  brianp
 * added glTexImage support
 *
 * Revision 1.5  1995/04/19  13:48:18  brianp
 * renamed occurances of near and far for SCO x86 Unix
 *
 * Revision 1.4  1995/04/08  15:26:55  brianp
 * add gl_save_shademodel
 *
 * Revision 1.3  1995/03/08  15:10:02  brianp
 * added logicop and clear_index functions
 *
 * Revision 1.2  1995/03/04  19:25:29  brianp
 * 1.1 beta revision
 *
 * Revision 1.1  1995/02/24  14:24:47  brianp
 * Initial revision
 *
 */


#ifndef LIST_H
#define LIST_H


#include "GL/gl.h"



extern void gl_init_lists( void );

extern GLint gl_list_index( void );


extern void gl_save_accum( GLenum op, GLfloat value );

extern void gl_save_alphafunc( GLenum func, GLclampf ref );

extern void gl_save_blendcolor( GLfloat red, GLfloat green,
                                GLfloat blue, GLfloat alpha );

extern void gl_save_blendequation( GLenum mode );

extern void gl_save_blendfunc( GLenum sfactor, GLenum dfactor );

extern void gl_save_begin( GLenum mode );

extern void gl_save_bitmap( GLsizei width, GLsizei height,
			    GLfloat xorig, GLfloat yorig,
			    GLfloat xmove, GLfloat ymove,
			    const GLubyte *bitmap );

extern void gl_save_clear( GLbitfield mask );

extern void gl_save_clearaccum( GLfloat red, GLfloat green,
			        GLfloat blue, GLfloat alpha );

extern void gl_save_clearcolor( GLclampf red, GLclampf green,
			        GLclampf blue, GLclampf alpha );

extern void gl_save_cleardepth( GLclampd depth );

extern void gl_save_clearindex( GLfloat c );

extern void gl_save_clearstencil( GLint s );

extern void gl_save_clipplane( GLenum plane, const GLfloat *equ );

extern void gl_save_color( const GLint c[4] );

extern void gl_save_colormask( GLboolean red, GLboolean green,
			       GLboolean blue, GLboolean alpha );

extern void gl_save_colormaterial( GLenum face, GLenum mode );

extern void gl_save_copypixels( GLint x, GLint y,
				GLsizei width, GLsizei height, GLenum type );

extern void gl_save_cullface( GLenum mode );

extern void gl_save_depthfunc( GLenum func );

extern void gl_save_depthmask( GLboolean mask );

extern void gl_save_depthrange( GLclampd nearval, GLclampd farval );

extern void gl_save_disable( GLenum cap );

extern void gl_save_drawbuffer( GLenum mode );

extern void gl_save_drawpixels( GLsizei width, GLsizei height, GLenum format,
			        GLenum type, GLvoid *pixels );

extern void gl_save_edgeflag( GLboolean flag );

extern void gl_save_enable( GLenum cap );

extern void gl_save_end( void );

extern void gl_save_evalcoord1( GLfloat u );

extern void gl_save_evalcoord2( GLfloat u, GLfloat v );

extern void gl_save_evalmesh1( GLenum mode, GLint i1, GLint i2 );

extern void gl_save_evalmesh2( GLenum mode, GLint i1, GLint i2,
			       GLint j1, GLint j2 );

extern void gl_save_evalpoint1( GLint i );

extern void gl_save_evalpoint2( GLint i, GLint j );

extern void gl_save_fog( GLenum pname, const GLfloat *params );

extern void gl_save_frontface( GLenum mode );

extern void gl_save_hint( GLenum target, GLenum mode );

extern void gl_save_index( GLuint index );

extern void gl_save_indexmask( GLuint mask );

extern void gl_save_initnames( void );

extern void gl_save_light( GLenum light, GLenum pname, const GLfloat *params,
			   GLuint numparams );

extern void gl_save_lightmodel( GLenum pname, const GLfloat *params );

extern void gl_save_linewidth( GLfloat width );

extern void gl_save_linestipple( GLint factor, GLushort pattern );

extern void gl_save_loadmatrix( const GLfloat *m );

extern void gl_save_loadname( GLuint name );

extern void gl_save_logicop( GLenum opcode );

extern void gl_save_map1( GLenum target, GLfloat u1, GLfloat u2, GLint stride,
			  GLint order, const GLfloat *points );

extern void gl_save_map2( GLenum target,
			  GLfloat u1, GLfloat u2, GLint ustride, GLint uorder,
			  GLfloat v1, GLfloat v2, GLint vstride, GLint vorder,
			  const GLfloat *points );

extern void gl_save_mapgrid1( GLint un, GLfloat u1, GLfloat u2 );

extern void gl_save_mapgrid2( GLint un, GLfloat u1, GLfloat u2,
			      GLint vn, GLfloat v1, GLfloat v2 );

extern void gl_save_material( GLenum face, GLenum pname,
			      const GLfloat *params );

extern void gl_save_matrixmode( GLenum mode );

extern void gl_save_multmatrix( const GLfloat *m );

extern void gl_save_normal3fv( const GLfloat n[3] );

extern void gl_save_normal3f( GLfloat nx, GLfloat ny, GLfloat nz );

extern void gl_save_passthrough( GLfloat token );

extern void gl_save_pixelmap( GLenum map, GLint mapsize,
			       const GLfloat *values );

extern void gl_save_pixeltransfer( GLenum pname, GLfloat param );

extern void gl_save_pixelzoom( GLfloat xfactor, GLfloat yfactor );

extern void gl_save_pointsize( GLfloat size );

extern void gl_save_polygonmode( GLenum face, GLenum mode );

extern void gl_save_polygonoffset( GLfloat factor, GLfloat bias );

extern void gl_save_popattrib( void );

extern void gl_save_popmatrix( void );

extern void gl_save_popname( void );

extern void gl_save_pushattrib( GLbitfield mask );

extern void gl_save_pushmatrix( void );

extern void gl_save_pushname( GLuint name );

extern void gl_save_rasterpos( const GLfloat v[4] );

extern void gl_save_readbuffer( GLenum mode );

extern void gl_save_scissor( GLint x, GLint y, GLsizei width, GLsizei height );

extern void gl_save_shademodel( GLenum mode );

extern void gl_save_stencilfunc( GLenum func, GLint ref, GLuint mask );

extern void gl_save_stencilmask( GLuint mask );

extern void gl_save_stencilop( GLenum fail, GLenum zfail, GLenum zpass );

extern void gl_save_texcoord( const GLfloat tc[4] );

extern void gl_save_texenv( GLenum target, GLenum pname,
                            const GLfloat *params );

extern void gl_save_texparameter( GLenum target, GLenum pname,
                                  const GLfloat *params );

extern void gl_save_texgen( GLenum coord, GLenum pname,
                            const GLfloat *params );

extern void gl_save_teximage1d( GLint level, GLint components,
			        GLsizei width, GLint border,
			        const GLubyte *pixels );

extern void gl_save_teximage2d( GLint level, GLint components,
			        GLsizei width, GLsizei height, GLint border,
			        const GLubyte *pixels );

extern void gl_save_scale( GLfloat x, GLfloat y, GLfloat z );

extern void gl_save_translate( GLfloat x, GLfloat y, GLfloat z );

extern void gl_save_vertex( GLfloat x, GLfloat y, GLfloat z, GLfloat w );

extern void gl_save_viewport( GLint x, GLint y, GLsizei width, GLsizei height );


#endif
