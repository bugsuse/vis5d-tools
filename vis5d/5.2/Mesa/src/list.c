/* $Id: list.c,v 1.38 1996/03/18 13:47:06 brianp Exp $ */

/*
 * Mesa 3-D graphics library
 * Version:  1.2
 * Copyright (C) 1995-1996  Brian Paul  (brianp@ssec.wisc.edu)
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
$Log: list.c,v $
 * Revision 1.38  1996/03/18  13:47:06  brianp
 * fixed glHint() display list bug
 *
 * Revision 1.37  1996/03/15  16:24:57  brianp
 * fixed problem with glClearStencil() in display lists
 *
 * Revision 1.36  1996/02/26  15:10:12  brianp
 * use CC.Current.IntColor instead of CC.Current.Color
 *
 * Revision 1.35  1996/02/02  23:22:13  brianp
 * check for valid display list number in glDeleteLists()
 *
 * Revision 1.34  1995/12/30  17:17:56  brianp
 * added direct glScale and glTranslate support
 *
 * Revision 1.33  1995/12/30  00:55:39  brianp
 * compute CC.Current.IntColor for glColor
 *
 * Revision 1.32  1995/12/20  17:28:44  brianp
 * dont' call gl_color or gl_index anymore, inlined their operations
 *
 * Revision 1.31  1995/12/19  17:06:44  brianp
 * added gl_save_blendcolor, blendequation, colormaterial, edge_flag, matrixmode
 * removed gl_save_set_boolean, enum, float, int, uint
 *
 * Revision 1.30  1995/12/19  16:42:20  brianp
 * added gl_save_pixeltransfer
 *
 * Revision 1.29  1995/11/03  17:38:16  brianp
 * removed unused variables, added casts for C++ compilation
 *
 * Revision 1.28  1995/11/02  14:56:31  brianp
 * added several (Node *) casts per Steven Spitz
 *
 * Revision 1.27  1995/10/29  19:14:19  brianp
 * added glPolygonOffsetEXT display list support
 *
 * Revision 1.26  1995/10/14  17:42:12  brianp
 * added glClipPlane, glLightModel, and texture functions
 *
 * Revision 1.25  1995/10/04  19:35:07  brianp
 * replaced gl_save_normal with gl_save_normal3fv and gl_save_normal3f
 *
 * Revision 1.24  1995/08/31  18:34:16  brianp
 * added display list / glScissor support
 *
 * Revision 1.23  1995/08/01  21:51:40  brianp
 * renamed gl_save_pixelZoom as gl_save_pixelzoom
 *
 * Revision 1.22  1995/08/01  20:53:35  brianp
 * added glPixelZoom support
 *
 * Revision 1.21  1995/07/25  16:41:54  brianp
 * made changes for using CC.VertexFunc pointer
 *
 * Revision 1.20  1995/07/24  20:34:16  brianp
 * replaced memset() with MEMSET() and memcpy() with MEMCPY()
 *
 * Revision 1.19  1995/06/22  14:28:20  brianp
 * added glStencilFunc, glStencilMask and glStencilOp support
 *
 * Revision 1.18  1995/06/12  15:52:56  brianp
 * changed includes for copypix.h and drawpix.h
 *
 * Revision 1.17  1995/05/22  21:02:41  brianp
 * Release 1.2
 *
 * Revision 1.16  1995/05/19  13:26:50  brianp
 * added display list support for selection/name stack functions
 *
 * Revision 1.15  1995/05/18  14:46:19  brianp
 * fixed texture deallocation infinite loop bug
 *
 * Revision 1.14  1995/05/17  13:16:26  brianp
 * added more error checking:  GL_INVALID_OPERATION between begin/end
 *
 * Revision 1.13  1995/05/15  16:07:15  brianp
 * implemented shared/nonshared display lists
 *
 * Revision 1.12  1995/05/12  16:57:22  brianp
 * replaced CC.Mode!=0 with INSIDE_BEGIN_END
 *
 * Revision 1.11  1995/05/10  18:44:40  brianp
 * added glTexImage support
 *
 * Revision 1.10  1995/04/19  13:48:18  brianp
 * renamed occurances of near and far for SCO x86 Unix
 *
 * Revision 1.9  1995/04/08  15:26:37  brianp
 * add gl_save_shademodel
 *
 * Revision 1.8  1995/03/13  15:58:23  brianp
 * fixed bitmap bugs per Thorsten Ohl
 *
 * Revision 1.7  1995/03/09  19:06:05  brianp
 * replaced gl_disable calls with gl_enable(GL_FALSE)
 *
 * Revision 1.6  1995/03/08  15:10:02  brianp
 * added logicop and clear_index functions
 *
 * Revision 1.5  1995/03/04  19:29:44  brianp
 * 1.1 beta revision
 *
 * Revision 1.4  1995/03/01  15:23:53  brianp
 * fixed glListBase, glCallList, glCallLists semantics bug
 *
 * Revision 1.3  1995/02/24  17:27:40  brianp
 * *** empty log message ***
 *
 * Revision 1.2  1995/02/24  15:28:00  brianp
 * fixed destroy_list bug
 *
 * Revision 1.1  1995/02/24  14:24:47  brianp
 * Initial revision
 *
 */


/*
 * display lists.
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "accum.h"
#include "attrib.h"
#include "bitmap.h"
#include "clip.h"
#include "context.h"
#include "copypix.h"
#include "draw.h"
#include "drawpix.h"
#include "enable.h"
#include "eval.h"
#include "feedback.h"
#include "fog.h"
#include "light.h"
#include "logic.h"
#include "macros.h"
#include "pixel.h"
#include "texture.h"
#include "vb.h"
#include "xform.h"


/*
public		list building		internal	display list
gl function	function		function	node kind
----------------------------------------------------------------------
glAccum		gl_save_acum		<same>		NODE_ACCUM
glAlphaFunc	gl_save_alphafunc	<same>		NODE_ALPHA_FUNC
glBegin		gl_save_begin		gl_begin	NODE_BEGIN
glBitmap	gl_save_bitmap		gl_bitmap	NODE_BITMAP
glBlendColor    gl_save_blendcolor      <same>          NODE_BLEND_COLOR
glBlendEquation gl_save_blendequation   <same>          NODE_BLEND_EQUATION
glBlendFunc	gl_save_blendfunc	<same>		NODE_BLEND_FUNC
glCallList	gl_save_call		execute_list	NODE_CALL_LIST
glCallLists	gl_save_call		execute_list	NODE_CALL_LIST_OFFSET
glClear		gl_save_clear		gl_clear	NODE_CLEAR
glClearAccum	gl_save_clearaccum	<same>		NODE_CLEAR_ACCUM
glClearColor	gl_save_clearcolor	<same>		NODE_CLEAR_COLOR
glClearDepth	gl_save_cleardepth	<same>		NODE_CLEAR_DEPTH
glClearIndex	gl_save_clearindex	<same>		NODE_CLEAR_INDEX
glClearStencil	gl_save_clearstencil	<same>		NODE_CLEAR_STENCIL
glClipPlane     gl_save_clipplane       gl_clipplane    NODE_CLIP_PLANE
glColor*	gl_save_color		gl_color	NODE_COLOR
glColorMask	gl_save_colormask	<same>		NODE_COLOR_MASK
glColorMaterial	gl_save_colormaterial	<same>		NODE_COLOR_MATERIAL
glCopyPixels	gl_save_copypixels	gl_copypixels	NODE_COPY_PIXELS
glCullFace	gl_save_cullface	<same>		NODE_CULL_FACE
glDepthFunc	gl_save_depthfunc	<same>		NODE_DEPTH_FUNC
glDepthMask	gl_save_depthmask	<same>		NODE_DEPTH_MASK
glDepthRange	gl_save_depthrange	<same>		NODE_DEPTH_RANGE
glDisable	gl_save_disable		gl_enable	NODE_DISABLE
glDrawBuffer	gl_save_drawbuffer	<same>		NODE_DRAW_BUFFER
glDrawPixels	gl_save_drawpixels	gl_drawpixels	NODE_DRAW_PIXELS
glEdgeFlag*	gl_save_edgeflag        <none>          NODE_EDGE_FLAG
glEnable	gl_save_enable		gl_enable	NODE_ENABLE
glEnd		gl_save_end		gl_end		NODE_END
glEvalCoord1f	gl_save_evalcoord1	gl_evalcoord1	NODE_EVALCOORD1
glEvalCoord1d	gl_save_evalcoord1	gl_evalcoord1	NODE_EVALCOORD1
glEvalCoord2f	gl_save_evalcoord2	gl_evalcoord2	NODE_EVALCOORD2
glEvalCoord2d	gl_save_evalcoord2	gl_evalcoord2	NODE_EVALCOORD2
glEvalPoint1	gl_save_evalpoint1	<same>		NODE_EVALPOINT1
glEvalPoint2	gl_save_evalpoint2	<same>		NODE_EVALPOINT2
glEvalMesh1	gl_save_evalmesh1	<same>		NODE_EVALMESH1
glEvalMesh2	gl_save_evalmesh2	<same>		NODE_EVALMESH2
glFog		gl_save_fog		gl_fog		NODE_FOG
glFrontFace	gl_save_frontface	<same>		NODE_FRONT_FACE
glHint		gl_save_hint		<same>		NODE_HINT
glIndex*	gl_save_index		gl_index	NODE_INDEX
glIndexMask	gl_save_indexmask	<same>		NODE_INDEX_MASK
glInitNames	gl_save_initnames	<same>		NODE_INIT_NAMES
glLight*	gl_save_light		gl_light	NODE_LIGHT
glLightModel*	gl_save_lightmodel	gl_lightmodel	NODE_LIGHT_MODEL
glLineStipple	gl_save_linestipple	gl_line_stipple	NODE_LINE_STIPPLE
glLineWidth	gl_save_linewidth	gl_line_width	NODE_LINE_WIDTH
glListBase	gl_save_listbase	<same>		NODE_LIST_BASE
glLoadMatrix	gl_save_loadmatrix	gl_load_matrix	NODE_LOAD_MATRIX
glLoadName	gl_save_loadname	<same>		NODE_LOAD_NAME
glLogicOp	gl_save_logicop		gl_logicop	NODE_LOGIC_OP
glMap1f		gl_save_map1		gl_map1		NODE_MAP1
glMap1d		gl_save_map1		gl_map1		NODE_MAP1
glMap2f		gl_save_map2		gl_map2		NODE_MAP2
glMap2d		gl_save_map2		gl_map2		NODE_MAP2
glMapGrid1f	gl_save_mapgrid1	gl_mapgrid1	NODE_MAPGRID1
glMapGrid1d	gl_save_mapgrid1	gl_mapgrid1	NODE_MAPGRID1
glMapGrid2f	gl_save_mapgrid2	gl_mapgrid2	NODE_MAPGRID2
glMapGrid2d	gl_save_mapgrid2	gl_mapgrid2	NODE_MAPGRID2
glMaterial*	gl_save_material	gl_material	NODE_MATERIAL
glMatrixMode    gl_save_matrixmode      <same>          NODE_MATRIX_MODE
glMultMatrix	gl_save_multmatrix	gl_mult_matrix	NODE_MULT_MATRIX
glNormal*	gl_save_normal		<none>		NODE_NORMAL
glPassThrough	gl_save_passthrough	gl_passthrough	NODE_PASSTHROUGH
glPixelMap	gl_save_pixelmap	gl_pixel_map	NODE_PIXEL_MAP
glPixelTransfer gl_save_pixeltransfer   gl_pixel_transfer NODE_PIXEL_TRANSFER
glPixelZoom	gl_save_pixelzoom	<same>		NODE_PIXEL_ZOOM
glPointSize	gl_save_pointsize	<same>   	NODE_POINTSIZE
glPolygonMode	gl_save_polygonmode	<same>    	NODE_POLYGON_MODE
glPolygonOffset gl_save_polygonoffset   <same>          NODE_POLYGON_OFFSET
glPopAttrib	gl_save_popmatrix	gl_pop_attrib	NODE_POP_ATTRIB
glPopMatrix	gl_save_popmatrix	gl_pop_matrix	NODE_POP_MATRIX
glPopName	gl_save_popname		<same>		NODE_POP_NAME
glPushAttrib	gl_save_pushmatrix	gl_push_attrib	NODE_PUSH_ATTRIB
glPushMatrix	gl_save_pushmatrix	gl_push_matrix	NODE_PUSH_MATRIX
glPushName	gl_save_pushname	<same>		NODE_PUSH_NAME
glRasterPos*	gl_save_rasterpos	gl_rasterpos	NODE_RASTER_POS
glReadBuffer	gl_save_readbuffer	<same>		NODE_READ_BUFFER
glScale*        gl_save_scale           gl_scale        NODE_SCALE
glScissor       gl_save_scissor         <same>          NODE_SCISSOR
glShadeModel	gl_save_shademodel	<same>		NODE_SHADE_MODEL
glStencilFunc	gl_save_stencilfunc	<same>		NODE_STENCIL_FUNC
glStencilMask	gl_save_stencilmask	<same>		NODE_STENCIL_MASK
glStencilOp	gl_save_stencilop	<same>		NODE_STENCIL_OP
glTexCoord*	gl_save_texcoord	<none>		NODE_TEXCOORD
glTexEnv*       gl_save_texenv          gl_texinv       NODE_TEXENV
glTexGen*       gl_save_texgen          gl_texgen       NODE_TEXGEN
glTexParam*     gl_save_texparameter    gl_texparameter NODE_TEXPARAMETER
glTexImage1D	gl_save_teximage1d	gl_teximage1d	NODE_TEXIMAGE1D
glTexImage2D	gl_save_teximage2d	gl_teximage2d	NODE_TEXIMAGE2D
glTranslate*    gl_save_translate       gl_translate    NODE_TRANSLATE
glVertex*	gl_save_vertex		gl_vertex	NODE_VERTEX
glViewport	gl_save_viewport	gl_viewport	NODE_VIEWPORT
*/


/*
Functions which aren't compiled but executed immediately:
	glIsList
	glGenLists
	glDeleteLists
	glEndList
	glFeedbackBuffer
	glSelectBuffer
	glRenderMode
	glReadPixels
	glPixelStore
	glFlush
	glFinish
	glIsEnabled
	glGet*

Functions which cause errors if called while compile a display list:
	glNewList
*/



/* How many nodes to allocate at a time: */
#define BLOCK_SIZE 100


/*
 * Kinds of nodes:  (the fact that these identifiers are assigned consecutive
 * integer values starting at 0 is very important, see NodeSize array usage)
 */
typedef enum {
	NODE_ACCUM,
	NODE_ALPHA_FUNC,
        NODE_BEGIN,
	NODE_BITMAP,
	NODE_BLEND_COLOR,
	NODE_BLEND_EQUATION,
	NODE_BLEND_FUNC,
        NODE_CALL_LIST,
        NODE_CALL_LIST_OFFSET,
	NODE_CLEAR,
	NODE_CLEAR_ACCUM,
	NODE_CLEAR_COLOR,
	NODE_CLEAR_DEPTH,
	NODE_CLEAR_INDEX,
	NODE_CLEAR_STENCIL,
        NODE_CLIP_PLANE,
	NODE_COLOR,
	NODE_COLOR_MASK,
	NODE_COLOR_MATERIAL,
	NODE_COPY_PIXELS,
	NODE_CULL_FACE,
	NODE_DEPTH_FUNC,
	NODE_DEPTH_MASK,
	NODE_DEPTH_RANGE,
	NODE_DISABLE,
	NODE_DRAW_BUFFER,
	NODE_DRAW_PIXELS,
        NODE_EDGE_FLAG,
	NODE_ENABLE,
        NODE_END,
	NODE_EVALCOORD1,
	NODE_EVALCOORD2,
	NODE_EVALMESH1,
	NODE_EVALMESH2,
	NODE_EVALPOINT1,
	NODE_EVALPOINT2,
	NODE_FOG,
	NODE_FRONT_FACE,
	NODE_HINT,
	NODE_INDEX,
	NODE_INDEX_MASK,
	NODE_INIT_NAMES,
	NODE_LIGHT,
	NODE_LIGHT_MODEL,
	NODE_LINE_STIPPLE,
	NODE_LINE_WIDTH,
	NODE_LIST_BASE,
	NODE_LOAD_MATRIX,
	NODE_LOAD_NAME,
	NODE_LOGIC_OP,
	NODE_MAP1,
	NODE_MAP2,
	NODE_MAPGRID1,
	NODE_MAPGRID2,
	NODE_MATERIAL,
	NODE_MATRIX_MODE,
	NODE_MULT_MATRIX,
        NODE_NORMAL,
	NODE_PASSTHROUGH,
	NODE_PIXEL_MAP,
	NODE_PIXEL_TRANSFER,
	NODE_PIXEL_ZOOM,
	NODE_POINTSIZE,
	NODE_POLYGON_MODE,
	NODE_POLYGON_OFFSET,
	NODE_POP_ATTRIB,
	NODE_POP_MATRIX,
	NODE_POP_NAME,
	NODE_PUSH_ATTRIB,
	NODE_PUSH_MATRIX,
	NODE_PUSH_NAME,
	NODE_RASTER_POS,
	NODE_READ_BUFFER,
        NODE_SCALE,
	NODE_SCISSOR,
	NODE_SHADE_MODEL,
	NODE_STENCIL_FUNC,
	NODE_STENCIL_MASK,
	NODE_STENCIL_OP,
	NODE_TEXCOORD,
        NODE_TEXENV,
        NODE_TEXGEN,
        NODE_TEXPARAMETER,
	NODE_TEXIMAGE1D,
	NODE_TEXIMAGE2D,
        NODE_TRANSLATE,
        NODE_VERTEX,
	NODE_VIEWPORT,
	/* The following two are meta nodes */
	NODE_CONTINUE,
	NODE_END_OF_LIST
} Kind;


/*
 * Each command in the display list is stored as a sequence of adjacent
 * nodes in memory.  Each node is the union of a variety of datatypes.
 */
typedef union node {
	Kind		kind;
	GLboolean	b;
	GLbitfield	bf;
	GLshort		s;
	GLushort	us;
	GLint		i;
	GLuint		ui;
	GLenum		e;
	GLfloat		f;
	GLvoid		*data;
	void		*next;	/* If prev node's kind==NODE_CONTINUE */
} Node;



/* Number of nodes of storage needed for each command: */
static GLuint NodeSize[ NODE_CONTINUE ];


/* Used while a display list is under construction: */
static Node *CurrentListPtr;	/* Head of list being compiled */
static GLuint CurrentListNum;	/* Number of the list being compiled */
static Node *CurrentBlock;	/* Pointer to current block of nodes */
static GLuint CurrentPos;	/* Index into current block of nodes */




/**********************************************************************/
/*****                           Private                          *****/
/**********************************************************************/


/*
 * Return a pointer to the first of 'count' empty nodes.
 */
static Node *alloc_nodes( GLuint count )
{
   Node *n;

   if (CurrentPos + count + 2 > BLOCK_SIZE) {
      /* This block is full */
      n = CurrentBlock + CurrentPos;
      n[0].kind = NODE_CONTINUE;
      n[1].next = (Node *) malloc( sizeof(Node) * BLOCK_SIZE );
      /* TODO: check for out of memory */
      CurrentBlock = (Node *) n[1].next;
      CurrentPos = 0;
   }

   n = CurrentBlock + CurrentPos;
   CurrentPos += count;
   return n;
}




/*
 * Destroy all nodes in a display list.
 * Input:  list - list number in [0..]
 */
void destroy_list( GLuint list )
{
   Node *n, *block;
   GLboolean done;

   block = n = CC.ListGroup->List[list];

   done = GL_FALSE;
   while (!done) {
      switch (n[0].kind) {
	 /* special cases first */
	 case NODE_MAP1:
	    gl_free_control_points( n[1].e, (GLfloat *) n[6].data );
	    n += NodeSize[n[0].kind];
	    break;
	 case NODE_MAP2:
	    gl_free_control_points( n[1].e, (GLfloat *) n[10].data );
	    n += NodeSize[n[0].kind];
	    break;
	 case NODE_DRAW_PIXELS:
	    free( n[5].data );
	    n += NodeSize[n[0].kind];
	    break;
	 case NODE_BITMAP:
	    free( n[7].data );
	    n += NodeSize[n[0].kind];
	    break;
	 case NODE_TEXIMAGE1D:
	    {
	       GLint level = n[1].i;
	       if (CC.TextureImage1D[level]==n[5].data) {
		  /* this texture is currently in use, mark as deletable */
		  CC.TextureImage1DDeleteFlag[level] = GL_TRUE;
	       }
	       else {
		  /* this texture is not currently in use, delete it */
		  free( n[5].data );
	       }
	       n += NodeSize[n[0].kind];
	    }
	    break;
	 case NODE_TEXIMAGE2D:
	    {
	       GLint level = n[1].i;
	       if (CC.TextureImage2D[level]==n[6].data) {
		  /* this texture is currently in use, mark as deletable */
		  CC.TextureImage2DDeleteFlag[level] = GL_TRUE;
	       }
	       else {
		  /* this texture is not currently in use, delete it */
		  free( n[6].data );
	       }
	       n += NodeSize[n[0].kind];
	    }
	    break;
	 case NODE_CONTINUE:
	    n = (Node *) n[1].next;
	    free( block );
	    block = n;
	    break;
	 case NODE_END_OF_LIST:
	    free( block );
	    done = GL_TRUE;
	    break;
	 default:
	    /* Most frequent case */
	    n += NodeSize[n[0].kind];
	    break;
      }
   }

   CC.ListGroup->List[list] = NULL;
}



/*
 * Translate the nth element of list from type to GLuint.
 */
static GLuint translate_id( GLsizei n, GLenum type, const GLvoid *list )
{
   GLbyte *bptr;
   GLubyte *ubptr;
   GLshort *sptr;
   GLushort *usptr;
   GLint *iptr;
   GLuint *uiptr;
   GLfloat *fptr;

   switch (type) {
      case GL_BYTE:
         bptr = (GLbyte *) list;
         return (GLuint) *(bptr+n);
      case GL_UNSIGNED_BYTE:
         ubptr = (GLubyte *) list;
         return (GLuint) *(ubptr+n);
      case GL_SHORT:
         sptr = (GLshort *) list;
         return (GLuint) *(sptr+n);
      case GL_UNSIGNED_SHORT:
         usptr = (GLushort *) list;
         return (GLuint) *(usptr+n);
      case GL_INT:
         iptr = (GLint *) list;
         return (GLuint) *(iptr+n);
      case GL_UNSIGNED_INT:
         uiptr = (GLuint *) list;
         return (GLuint) *(uiptr+n);
      case GL_FLOAT:
         fptr = (GLfloat *) list;
         return (GLuint) *(fptr+n);
      case GL_2_BYTES:
         ubptr = ((GLubyte *) list) + 2*n;
         return (GLuint) *ubptr * 256 + (GLuint) *(ubptr+1);
      case GL_3_BYTES:
         ubptr = ((GLubyte *) list) + 3*n;
         return (GLuint) *ubptr * 65536
              + (GLuint) *(ubptr+1) * 256
              + (GLuint) *(ubptr+2);
      case GL_4_BYTES:
         ubptr = ((GLubyte *) list) + 4*n;
         return (GLuint) *ubptr * 16777216
              + (GLuint) *(ubptr+1) * 65536
              + (GLuint) *(ubptr+2) * 256
              + (GLuint) *(ubptr+3);
      default:
         return 0;
   }
}




/**********************************************************************/
/*****                        Public                              *****/
/**********************************************************************/

void gl_init_lists( void )
{
   static int init_flag = 0;

   if (init_flag==0) {
      CurrentListPtr = CurrentBlock = NULL;
      CurrentListNum = 0;

      NodeSize[NODE_ACCUM] = 3;
      NodeSize[NODE_ALPHA_FUNC] = 3;
      NodeSize[NODE_BEGIN] = 2;
      NodeSize[NODE_BITMAP] = 8;
      NodeSize[NODE_BLEND_COLOR] = 5;
      NodeSize[NODE_BLEND_EQUATION] = 2;
      NodeSize[NODE_BLEND_FUNC] = 3;
      NodeSize[NODE_CALL_LIST] = 2;
      NodeSize[NODE_CALL_LIST_OFFSET] = 2;
      NodeSize[NODE_CLEAR] = 2;
      NodeSize[NODE_CLEAR_ACCUM] = 5;
      NodeSize[NODE_CLEAR_COLOR] = 5;
      NodeSize[NODE_CLEAR_DEPTH] = 2;
      NodeSize[NODE_CLEAR_INDEX] = 2;
      NodeSize[NODE_CLEAR_STENCIL] = 2;
      NodeSize[NODE_CLIP_PLANE] = 6;
      NodeSize[NODE_COLOR] = 5;
      NodeSize[NODE_COLOR_MASK] = 5;
      NodeSize[NODE_COLOR_MATERIAL] = 3;
      NodeSize[NODE_COPY_PIXELS] = 6;
      NodeSize[NODE_CULL_FACE] = 2;
      NodeSize[NODE_DEPTH_FUNC] = 2;
      NodeSize[NODE_DEPTH_MASK] = 2;
      NodeSize[NODE_DEPTH_RANGE] = 3;
      NodeSize[NODE_DISABLE] = 2;
      NodeSize[NODE_DRAW_BUFFER] = 2;
      NodeSize[NODE_DRAW_PIXELS] = 6;
      NodeSize[NODE_ENABLE] = 2;
      NodeSize[NODE_EDGE_FLAG] = 2;
      NodeSize[NODE_END] = 1;
      NodeSize[NODE_EVALCOORD1] = 2;
      NodeSize[NODE_EVALCOORD2] = 3;
      NodeSize[NODE_EVALMESH1] = 4;
      NodeSize[NODE_EVALMESH2] = 6;
      NodeSize[NODE_EVALPOINT1] = 2;
      NodeSize[NODE_EVALPOINT2] = 3;
      NodeSize[NODE_FOG] = 6;
      NodeSize[NODE_FRONT_FACE] = 2;
      NodeSize[NODE_HINT] = 3;
      NodeSize[NODE_INDEX] = 2;
      NodeSize[NODE_INDEX_MASK] = 2;
      NodeSize[NODE_INIT_NAMES] = 1;
      NodeSize[NODE_LIGHT] = 7;
      NodeSize[NODE_LIGHT_MODEL] = 6;
      NodeSize[NODE_LINE_STIPPLE] = 3;
      NodeSize[NODE_LINE_WIDTH] = 2;
      NodeSize[NODE_LIST_BASE] = 2;
      NodeSize[NODE_LOAD_MATRIX] = 17;
      NodeSize[NODE_LOAD_NAME] = 2;
      NodeSize[NODE_LOGIC_OP] = 2;
      NodeSize[NODE_MAP1] = 7;
      NodeSize[NODE_MAP2] = 11;
      NodeSize[NODE_MAPGRID1] = 4;
      NodeSize[NODE_MAPGRID2] = 7;
      NodeSize[NODE_MATERIAL] = 7;
      NodeSize[NODE_MATRIX_MODE] = 2;
      NodeSize[NODE_MULT_MATRIX] = 17;
      NodeSize[NODE_NORMAL] = 4;
      NodeSize[NODE_PASSTHROUGH] = 2;
      NodeSize[NODE_PIXEL_MAP] = 4;
      NodeSize[NODE_PIXEL_TRANSFER] = 3;
      NodeSize[NODE_PIXEL_ZOOM] = 3;
      NodeSize[NODE_POINTSIZE] = 2;
      NodeSize[NODE_POLYGON_MODE] = 3;
      NodeSize[NODE_POLYGON_OFFSET] = 3;
      NodeSize[NODE_POP_ATTRIB] = 1;
      NodeSize[NODE_POP_MATRIX] = 1;
      NodeSize[NODE_POP_NAME] = 1;
      NodeSize[NODE_PUSH_ATTRIB] = 2;
      NodeSize[NODE_PUSH_MATRIX] = 1;
      NodeSize[NODE_PUSH_NAME] = 2;
      NodeSize[NODE_RASTER_POS] = 5;
      NodeSize[NODE_READ_BUFFER] = 2;
      NodeSize[NODE_SCALE] = 4;
      NodeSize[NODE_SCISSOR] = 5;
      NodeSize[NODE_STENCIL_FUNC] = 4;
      NodeSize[NODE_STENCIL_MASK] = 2;
      NodeSize[NODE_STENCIL_OP] = 4;
      NodeSize[NODE_SHADE_MODEL] = 2;
      NodeSize[NODE_TEXCOORD] = 5;
      NodeSize[NODE_TEXENV] = 7;
      NodeSize[NODE_TEXGEN] = 7;
      NodeSize[NODE_TEXPARAMETER] = 7;
      NodeSize[NODE_TEXIMAGE1D] = 6;
      NodeSize[NODE_TEXIMAGE2D] = 7;
      NodeSize[NODE_TRANSLATE] = 4;
      NodeSize[NODE_VERTEX] = 5;
      NodeSize[NODE_VIEWPORT] = 5;
   }
   init_flag = 1;
}



/*
 * Return the name of the display list currently being compiled.  This
 * function is only called by glGet().
 */
GLint gl_list_index( void )
{
   return CurrentListNum;
}



/*
 * Display List compilation functions
 */


void gl_save_accum( GLenum op, GLfloat value )
{
   Node *n = alloc_nodes(3);
   if (n) {
      n[0].kind = NODE_ACCUM;
      n[1].e = op;
      n[2].f = value;
   }
}


void gl_save_alphafunc( GLenum func, GLclampf ref )
{
   Node *n = alloc_nodes(3);
   if (n) {
      n[0].kind = NODE_ALPHA_FUNC;
      n[1].e = func;
      n[2].f = (GLfloat) ref;
   }
}


/*
 * Compile a glBegin into current display list.
 */
void gl_save_begin( GLenum mode )
{
   Node *n = alloc_nodes(2);
   if (n) {
      n[0].kind = NODE_BEGIN;
      n[1].e = mode;
   }
}


void gl_save_bitmap( GLsizei width, GLsizei height,
		     GLfloat xorig, GLfloat yorig,
		     GLfloat xmove, GLfloat ymove,
		     const GLubyte *bitmap )
{
   Node *n = alloc_nodes(8);
   if (n) {
      n[0].kind = NODE_BITMAP;
      n[1].i = (GLint) width;
      n[2].i = (GLint) height;
      n[3].f = xorig;
      n[4].f = yorig;
      n[5].f = xmove;
      n[6].f = ymove;
      n[7].data = (void *) bitmap;
   }
}


void gl_save_blendequation( GLenum mode )
{
   Node *n = alloc_nodes(2);
   if (n) {
      n[0].kind = NODE_BLEND_EQUATION;
      n[1].e = mode;
   }
}


void gl_save_blendfunc( GLenum sfactor, GLenum dfactor )
{
   Node *n = alloc_nodes(3);
   if (n) {
      n[0].kind = NODE_BLEND_FUNC;
      n[1].e = sfactor;
      n[2].e = dfactor;
   }
}


void gl_save_blendcolor( GLfloat red, GLfloat green,
                         GLfloat blue, GLfloat alpha )
{
   Node *n = alloc_nodes(5);
   if (n) {
      n[0].kind = NODE_BLEND_COLOR;
      n[1].f = red;
      n[2].f = green;
      n[3].f = blue;
      n[4].f = alpha;
   }
}


void gl_save_clear( GLbitfield mask )
{
   Node *n = alloc_nodes(2);
   if (n) {
      n[0].kind = NODE_CLEAR;
      n[1].bf = mask;
   }
}


void gl_save_clearaccum( GLfloat red, GLfloat green,
			 GLfloat blue, GLfloat alpha )
{
   Node *n = alloc_nodes(5);
   if (n) {
      n[0].kind = NODE_CLEAR_ACCUM;
      n[1].f = red;
      n[2].f = green;
      n[3].f = blue;
      n[4].f = alpha;
   }
}


void gl_save_clearcolor( GLclampf red, GLclampf green,
			 GLclampf blue, GLclampf alpha )
{
   Node *n = alloc_nodes(5);
   if (n) {
      n[0].kind = NODE_CLEAR_COLOR;
      n[1].f = red;
      n[2].f = green;
      n[3].f = blue;
      n[4].f = alpha;
   }
}


void gl_save_cleardepth( GLclampd depth )
{
   Node *n = alloc_nodes(2);
   if (n) {
      n[0].kind = NODE_CLEAR_DEPTH;
      n[1].f = (GLfloat) depth;
   }
}


void gl_save_clearindex( GLfloat c )
{
   Node *n = alloc_nodes(2);
   if (n) {
      n[0].kind = NODE_CLEAR_INDEX;
      n[1].f = c;
   }
}


void gl_save_clearstencil( GLint s )
{
   Node *n = alloc_nodes(2);
   if (n) {
      n[0].kind = NODE_CLEAR_STENCIL;
      n[1].i = s;
   }
}


void gl_save_clipplane( GLenum plane, const GLfloat *equ )
{
   Node *n = alloc_nodes(6);
   if (n) {
      n[0].kind = NODE_CLIP_PLANE;
      n[1].e = plane;
      n[2].f = equ[0];
      n[3].f = equ[1];
      n[4].f = equ[2];
      n[5].f = equ[3];
   }
}



/*
 * Add a color to current display list.
 */
void gl_save_color( const GLint c[4] )
{
   Node *n = alloc_nodes(5);
   if (n) {
      n[0].kind = NODE_COLOR;
      n[1].i = c[0];
      n[2].i = c[1];
      n[3].i = c[2];
      n[4].i = c[3];
   }
}


void gl_save_colormask( GLboolean red, GLboolean green,
			 GLboolean blue, GLboolean alpha )
{
   Node *n = alloc_nodes(5);
   if (n) {
      n[0].kind = NODE_COLOR_MASK;
      n[1].b = red;
      n[2].b = green;
      n[3].b = blue;
      n[4].b = alpha;
   }
}


void gl_save_colormaterial( GLenum face, GLenum mode )
{
   Node *n = alloc_nodes(3);
   if (n) {
      n[0].kind = NODE_COLOR_MATERIAL;
      n[1].e = face;
      n[2].e = mode;
   }
}


void gl_save_copypixels( GLint x, GLint y,
			 GLsizei width, GLsizei height, GLenum type )
{
   Node *n = alloc_nodes(6);
   if (n) {
      n[0].kind = NODE_COPY_PIXELS;
      n[1].i = x;
      n[2].i = y;
      n[3].i = (GLint) width;
      n[4].i = (GLint) height;
      n[5].e = type;
   }

}



void gl_save_cullface( GLenum mode )
{
   Node *n = alloc_nodes(2);
   if (n) {
      n[0].kind = NODE_CULL_FACE;
      n[1].e = mode;
   }
}


void gl_save_depthfunc( GLenum func )
{
   Node *n = alloc_nodes(2);
   if (n) {
      n[0].kind = NODE_DEPTH_FUNC;
      n[1].e = func;
   }
}


void gl_save_depthmask( GLboolean mask )
{
   Node *n = alloc_nodes(2);
   if (n) {
      n[0].kind = NODE_DEPTH_MASK;
      n[1].b = mask;
   }
}


void gl_save_depthrange( GLclampd nearval, GLclampd farval )
{
   Node *n = alloc_nodes(3);
   if (n) {
      n[0].kind = NODE_DEPTH_RANGE;
      n[1].f = (GLfloat) nearval;
      n[2].f = (GLfloat) farval;
   }
}


void gl_save_disable( GLenum cap )
{
   Node *n = alloc_nodes(2);
   if (n) {
      n[0].kind = NODE_DISABLE;
      n[1].e = cap;
   }
}


void gl_save_drawbuffer( GLenum mode )
{
   Node *n = alloc_nodes(2);
   if (n) {
      n[0].kind = NODE_DRAW_BUFFER;
      n[1].e = mode;
   }
}


void gl_save_drawpixels( GLsizei width, GLsizei height, GLenum format,
			 GLenum type, GLvoid *pixels )
{
   Node *n = alloc_nodes(6);
   if (n) {
      n[0].kind = NODE_DRAW_PIXELS;
      n[1].i = (GLint) width;
      n[2].i = (GLint) height;
      n[3].e = format;
      n[4].e = type;
      n[5].data = pixels;
   }
}


void gl_save_edgeflag( GLboolean flag )
{
   Node *n = alloc_nodes(2);
   if (n) {
      n[0].kind = NODE_EDGE_FLAG;
      n[1].b = flag;
   }
}


void gl_save_enable( GLenum cap )
{
   Node *n = alloc_nodes(2);
   if (n) {
      n[0].kind = NODE_ENABLE;
      n[1].e = cap;
   }
}


/*
 * Compile a glEnd into current display list.
 */
void gl_save_end( void )
{
   Node *n = alloc_nodes(1);
   if (n) {
      n[0].kind = NODE_END;
   }
}



void gl_save_evalcoord1( GLfloat u )
{
   Node *n = alloc_nodes(2);
   if (n) {
      n[0].kind = NODE_EVALCOORD1;
      n[1].f = u;
   }
}


void gl_save_evalcoord2( GLfloat u, GLfloat v )
{
   Node *n = alloc_nodes(3);
   if (n) {
      n[0].kind = NODE_EVALCOORD2;
      n[1].f = u;
      n[2].f = v;
   }
}



void gl_save_evalmesh1( GLenum mode, GLint i1, GLint i2 )
{
   Node *n = alloc_nodes(4);
   if (n) {
      n[0].kind = NODE_EVALMESH1;
      n[1].e = mode;
      n[2].i = i1;
      n[3].i = i2;
   }
}



void gl_save_evalmesh2( GLenum mode, GLint i1, GLint i2,
		        GLint j1, GLint j2 )
{
   Node *n = alloc_nodes(6);
   if (n) {
      n[0].kind = NODE_EVALMESH2;
      n[1].e = mode;
      n[2].i = i1;
      n[3].i = i2;
      n[4].i = j1;
      n[5].i = j2;
   }
}


void gl_save_evalpoint1( GLint i )
{
   Node *n = alloc_nodes(2);
   if (n) {
      n[0].kind = NODE_EVALPOINT1;
      n[1].i = i;
   }
}


void gl_save_evalpoint2( GLint i, GLint j )
{
   Node *n = alloc_nodes(3);
   if (n) {
      n[0].kind = NODE_EVALPOINT2;
      n[1].i = i;
      n[2].i = j;
   }
}


void gl_save_fog( GLenum pname, const GLfloat *params )
{
   Node *n = alloc_nodes(6);
   if (n) {
      n[0].kind = NODE_FOG;
      n[1].e = pname;
      n[2].f = params[0];
      n[3].f = params[1];
      n[4].f = params[2];
      n[5].f = params[3];
   }
}


void gl_save_frontface( GLenum mode )
{
   Node *n = alloc_nodes(2);
   if (n) {
      n[0].kind = NODE_FRONT_FACE;
      n[1].e = mode;
   }
}


void gl_save_hint( GLenum target, GLenum mode )
{
   Node *n = alloc_nodes(3);
   if (n) {
      n[0].kind = NODE_HINT;
      n[1].e = target;
      n[2].e = mode;
   }
}


/*
 * Compile a glIndex call into current display list.
 */
void gl_save_index( GLuint index )
{
   Node *n = alloc_nodes(2);
   if (n) {
      n[0].kind = NODE_INDEX;
      n[1].ui = index;
   }
}


void gl_save_indexmask( GLuint mask )
{
   Node *n = alloc_nodes(2);
   if (n) {
      n[0].kind = NODE_INDEX_MASK;
      n[1].ui = mask;
   }
}



void gl_save_initnames( void )
{
   Node *n = alloc_nodes(1);
   if (n) {
      n[0].kind = NODE_INIT_NAMES;
   }
}

      
/*
 * save a glLight* call.
 */
void gl_save_light( GLenum light, GLenum pname, const GLfloat *params,
		    GLuint numparams )
{
   Node *n = alloc_nodes(7);
   if (n) {
      GLuint i;
      n[0].kind = NODE_LIGHT;
      n[1].e = light;
      n[2].e = pname;
      for (i=0;i<numparams;i++) {
	 n[3+i].f = params[i];
      }
   }
}


void gl_save_lightmodel( GLenum pname, const GLfloat *params )
{
   Node *n = alloc_nodes(6);
   if (n) {
      n[0].kind = NODE_LIGHT_MODEL;
      n[1].e = pname;
      n[2].f = params[0];
      n[3].f = params[1];
      n[4].f = params[2];
      n[5].f = params[3];
   }
}



/*
 * Save a glLineStipple call.
 */
void gl_save_linestipple( GLint factor, GLushort pattern )
{
   Node *n = alloc_nodes(3);
   if (n) {
      n[0].kind = NODE_LINE_STIPPLE;
      n[1].i = factor;
      n[2].us = pattern;
   }
}


/*
 * Save a glLineWidth call.
 */
void gl_save_linewidth( GLfloat width )
{
   Node *n = alloc_nodes(2);
   if (n) {
      n[0].kind = NODE_LINE_WIDTH;
      n[1].f = width;
   }
}


/*
 * Save a glListBase call.
 */
void gl_save_listbase( GLuint base )
{
   Node *n = alloc_nodes(2);
   if (n) {
      n[0].kind = NODE_LIST_BASE;
      n[1].ui = base;
   }
}


/*
 * Add a load matrix operation to current display list.
 */
void gl_save_loadmatrix( const GLfloat *m )
{
   Node *n = alloc_nodes(17);
   if (n) {
      GLuint i;
      n[0].kind = NODE_LOAD_MATRIX;
      for (i=0;i<16;i++) {
	 n[1+i].f = m[i];
      }
   }
}


/*
 * Add a glLoadName to the current display list.
 */
void gl_save_loadname( GLuint name )
{
   Node *n = alloc_nodes(2);
   if (n) {
      n[0].kind = NODE_LOAD_NAME;
      n[1].ui = name;
   }
}


/*
 * Save a glLogicOp call.
 */
void gl_save_logicop( GLenum opcode )
{
   Node *n = alloc_nodes(2);
   if (n) {
      n[0].kind = NODE_LOGIC_OP;
      n[1].e = opcode;
   }
}



void gl_save_map1( GLenum target, GLfloat u1, GLfloat u2, GLint stride,
		   GLint order, const GLfloat *points )
{
   Node *n = alloc_nodes(7);
   if (n) {
      n[0].kind = NODE_MAP1;
      n[1].e = target;
      n[2].f = u1;
      n[3].f = u2;
      n[4].i = stride;
      n[5].i = order;
      n[6].data = (void *) points;
   }
}



void gl_save_map2( GLenum target,
		   GLfloat u1, GLfloat u2, GLint ustride, GLint uorder,
		   GLfloat v1, GLfloat v2, GLint vstride, GLint vorder,
		   const GLfloat *points )
{
   Node *n = alloc_nodes(11);
   if (n) {
      n[0].kind = NODE_MAP2;
      n[1].e = target;
      n[2].f = u1;
      n[3].f = u2;
      n[4].f = v1;
      n[5].f = v2;
      n[6].i = ustride;
      n[7].i = vstride;
      n[8].i = uorder;
      n[9].i = vorder;
      n[10].data = (void *) points;
   }
}



void gl_save_mapgrid1( GLint un, GLfloat u1, GLfloat u2 )
{
   Node *n = alloc_nodes(4);
   if (n) {
      n[0].kind = NODE_MAPGRID1;
      n[1].i = un;
      n[2].f = u1;
      n[3].f = u2;
   }
}


void gl_save_mapgrid2( GLint un, GLfloat u1, GLfloat u2,
		       GLint vn, GLfloat v1, GLfloat v2 )
{
   Node *n = alloc_nodes(7);
   if (n) {
      n[0].kind = NODE_MAPGRID2;
      n[1].i = un;
      n[2].f = u1;
      n[3].f = u2;
      n[4].i = vn;
      n[5].f = v1;
      n[6].f = v2;
   }
}



/*
 * Add a glMaterial call to current display list.
 */
void gl_save_material( GLenum face, GLenum pname, const GLfloat *params )
{
   Node *n = alloc_nodes(7);
   if (n) {
      n[0].kind = NODE_MATERIAL;
      n[1].e = face;
      n[2].e = pname;
      n[3].f = params[0];
      n[4].f = params[1];
      n[5].f = params[2];
      n[6].f = params[3];
   }
}


/*
 * Add a glMatrixMode call to current display list.
 */
void gl_save_matrixmode( GLenum mode )
{
   Node *n = alloc_nodes(2);
   if (n) {
      n[0].kind = NODE_MATRIX_MODE;
      n[1].e = mode;
   }
}


/*
 * Add a mult matrix operation to current display list.
 */
void gl_save_multmatrix( const GLfloat *m )
{
   Node *n = alloc_nodes(17);
   if (n) {
      GLuint i;
      n[0].kind = NODE_MULT_MATRIX;
      for (i=0;i<16;i++) {
	 n[1+i].f = m[i];
      }
   }
}



/*
 * Add a normal to current display list.
 */
void gl_save_normal3fv( const GLfloat norm[3] )
{
   Node *n = alloc_nodes(4);
   if (n) {
      n[0].kind = NODE_NORMAL;
      n[1].f = norm[0];
      n[2].f = norm[1];
      n[3].f = norm[2];
   }
}



/*
 * Add a normal to current display list.
 */
void gl_save_normal3f( GLfloat nx, GLfloat ny, GLfloat nz )
{
   Node *n = alloc_nodes(4);
   if (n) {
      n[0].kind = NODE_NORMAL;
      n[1].f = nx;
      n[2].f = ny;
      n[3].f = nz;
   }
}



/*
 * Save a glPixelMap call.
 */
void gl_save_pixelmap( GLenum map, GLint mapsize, const GLfloat *values )
{
   Node *n = alloc_nodes(4);
   if (n) {
      n[0].kind = NODE_PIXEL_MAP;
      n[1].e = map;
      n[2].i = mapsize;
      n[3].data  = (void *) malloc( mapsize * sizeof(GLfloat) );
      MEMCPY( n[3].data, (void *) values, mapsize * sizeof(GLfloat) );
   }
}



/*
 * Save a glPixelTransfer call.
 */
void gl_save_pixeltransfer( GLenum pname, GLfloat param )
{
   Node *n = alloc_nodes(3);
   if (n) {
      n[0].kind = NODE_PIXEL_TRANSFER;
      n[1].e = pname;
      n[2].f = param;
   }
}



/*
 * Save a glPixelZoom call.
 */
void gl_save_pixelzoom( GLfloat xfactor, GLfloat yfactor )
{
   Node *n = alloc_nodes(4);
   if (n) {
      n[0].kind = NODE_PIXEL_ZOOM;
      n[1].f = xfactor;
      n[2].f = yfactor;
   }
}



/*
 * Save a glPointSize call.
 */
void gl_save_pointsize( GLfloat size )
{
   Node *n = alloc_nodes(2);
   if (n) {
      n[0].kind = NODE_POINTSIZE;
      n[1].f = size;
   }
}



/*
 * Save a glPolygonMode call.
 */
void gl_save_polygonmode( GLenum face, GLenum mode )
{
   Node *n = alloc_nodes(3);
   if (n) {
      n[0].kind = NODE_POLYGON_MODE;
      n[1].e = face;
      n[2].e = mode;
   }
}



void gl_save_polygonoffset( GLfloat factor, GLfloat bias )
{
   Node *n = alloc_nodes(3);
   if (n) {
      n[0].kind = NODE_POLYGON_OFFSET;
      n[1].f = factor;
      n[2].f = bias;
   }
}


/*
 * Add a pop attributies operation to current display list.
 */
void gl_save_popattrib( void )
{
   Node *n = alloc_nodes(1);
   if (n) {
      n[0].kind = NODE_POP_ATTRIB;
   }
}


/*
 * Add a pop matrix operation to current display list.
 */
void gl_save_popmatrix( void )
{
   Node *n = alloc_nodes(1);
   if (n) {
      n[0].kind = NODE_POP_MATRIX;
   }
}


/*
 * Add a glPopName to the current display list.
 */
void gl_save_popname( void )
{
   Node *n = alloc_nodes(1);
   if (n) {
      n[0].kind = NODE_POP_NAME;
   }
}



/*
 * Add a push attributies operation to current display list.
 */
void gl_save_pushattrib( GLbitfield mask )
{
   Node *n = alloc_nodes(2);
   if (n) {
      n[0].kind = NODE_PUSH_ATTRIB;
      n[1].bf = mask;
   }
}



/*
 * Add a push matrix operation to current display list.
 */
void gl_save_pushmatrix( void )
{
   Node *n = alloc_nodes(1);
   if (n) {
      n[0].kind = NODE_PUSH_MATRIX;
   }
}


/*
 * Add a glPushName to the current display list.
 */
void gl_save_pushname( GLuint name )
{
   Node *n = alloc_nodes(2);
   if (n) {
      n[0].kind = NODE_PUSH_NAME;
      n[1].ui = name;
   }
}


void gl_save_rasterpos( const GLfloat v[4] )
{
   Node *n = alloc_nodes(5);
   if (n) {
      n[0].kind = NODE_RASTER_POS;
      n[1].f = v[0];
      n[2].f = v[1];
      n[3].f = v[2];
      n[4].f = v[3];
   }
}



void gl_save_passthrough( GLfloat token )
{
   Node *n = alloc_nodes(2);
   if (n) {
      n[0].kind = NODE_PASSTHROUGH;
      n[1].f = token;
   }
}



void gl_save_readbuffer( GLenum mode )
{
   Node *n = alloc_nodes(2);
   if (n) {
      n[0].kind = NODE_READ_BUFFER;
      n[1].e = mode;
   }
}


/*
 * Add a glScale to current display list.
 */
void gl_save_scale( GLfloat x, GLfloat y, GLfloat z )
{
   Node *n = alloc_nodes( 4 );
   if (n) {
      n[0].kind = NODE_SCALE;
      n[1].f = x;
      n[2].f = y;
      n[3].f = z;
   }
}


void gl_save_scissor( GLint x, GLint y, GLsizei width, GLsizei height )
{
   Node *n = alloc_nodes(5);
   if (n) {
      n[0].kind = NODE_SCISSOR;
      n[1].i = x;
      n[2].i = y;
      n[3].i = width;
      n[4].i = height;
   }
}



/*
 * Add a glShadeModel call to current display list.
 */
void gl_save_shademodel( GLenum mode )
{
   Node *n = alloc_nodes(2);
   if (n) {
      n[0].kind = NODE_SHADE_MODEL;
      n[1].e = mode;
   }
}



void gl_save_stencilfunc( GLenum func, GLint ref, GLuint mask )
{
   Node *n = alloc_nodes(4);
   if (n) {
      n[0].kind = NODE_STENCIL_FUNC;
      n[1].e = func;
      n[2].i = ref;
      n[3].ui = mask;
   }
}



void gl_save_stencilmask( GLuint mask )
{
   Node *n = alloc_nodes(2);
   if (n) {
      n[0].kind = NODE_STENCIL_MASK;
      n[1].ui = mask;
   }
}



void gl_save_stencilop( GLenum fail, GLenum zfail, GLenum zpass )
{
   Node *n = alloc_nodes(4);
   if (n) {
      n[0].kind = NODE_STENCIL_OP;
      n[1].e = fail;
      n[2].e = zfail;
      n[3].e = zpass;
   }
}




/*
 * Add a texture coordinate to current display list.
 */
void gl_save_texcoord( const GLfloat tc[4] )
{
   Node *n = alloc_nodes(5);
   if (n) {
      n[0].kind = NODE_TEXCOORD;
      n[1].f = tc[0];
      n[2].f = tc[1];
      n[3].f = tc[2];
      n[4].f = tc[3];
   }
}



void gl_save_texenv( GLenum target, GLenum pname, const GLfloat *params )
{
   Node *n = alloc_nodes(7);
   if (n) {
      n[0].kind = NODE_TEXENV;
      n[1].e = target;
      n[2].e = pname;
      n[3].f = params[0];
      n[4].f = params[1];
      n[5].f = params[2];
      n[6].f = params[3];
   }
}


void gl_save_texgen( GLenum coord, GLenum pname, const GLfloat *params )
{
   Node *n = alloc_nodes(7);
   if (n) {
      n[0].kind = NODE_TEXGEN;
      n[1].e = coord;
      n[2].e = pname;
      n[3].f = params[0];
      n[4].f = params[1];
      n[5].f = params[2];
      n[6].f = params[3];
   }
}


void gl_save_texparameter( GLenum target, GLenum pname, const GLfloat *params )
{
   Node *n = alloc_nodes(7);
   if (n) {
      n[0].kind = NODE_TEXPARAMETER;
      n[1].e = target;
      n[2].e = pname;
      n[3].f = params[0];
      n[4].f = params[1];
      n[5].f = params[2];
      n[6].f = params[3];
   }
}



/*
 * Add a glTexImage1D call to current display list.
 */
void gl_save_teximage1d( GLint level, GLint components,
			 GLsizei width, GLint border,
			 const GLubyte *pixels )
{
   Node *n = alloc_nodes(6);
   if (n) {
      n[0].kind = NODE_TEXIMAGE1D;
      n[1].i = level;
      n[2].i = components;
      n[3].i = (GLint) width;
      n[4].i = border;
      n[5].data = (GLvoid *) pixels;
   }
}



/*
 * Add a glTexImage2D call to current display list.
 */
void gl_save_teximage2d( GLint level, GLint components,
			 GLsizei width, GLsizei height, GLint border,
			 const GLubyte *pixels )
{
   Node *n = alloc_nodes(7);
   if (n) {
      n[0].kind = NODE_TEXIMAGE2D;
      n[1].i = level;
      n[2].i = components;
      n[3].i = (GLint) width;
      n[4].i = (GLint) height;
      n[5].i = border;
      n[6].data = (GLvoid *) pixels;
   }
}



/*
 * Add a glTranslate to current display list.
 */
void gl_save_translate( GLfloat x, GLfloat y, GLfloat z )
{
   Node *n = alloc_nodes( 4 );
   if (n) {
      n[0].kind = NODE_TRANSLATE;
      n[1].f = x;
      n[2].f = y;
      n[3].f = z;
   }
}


/*
 * Add a vertex to current display list.
 */
void gl_save_vertex( GLfloat x, GLfloat y, GLfloat z, GLfloat w )
{
   Node *n = alloc_nodes( 5 );
   if (n) {
      n[0].kind = NODE_VERTEX;
      n[1].f = x;
      n[2].f = y;
      n[3].f = z;
      n[4].f = w;
   }
}



/*
 * Save a glViewport call to display list.
 */
void gl_save_viewport( GLint x, GLint y, GLsizei width, GLsizei height )
{
   Node *n = alloc_nodes( 5 );
   if (n) {
      n[0].kind = NODE_VIEWPORT;
      n[1].i = x;
      n[2].i = y;
      n[3].i = (GLint) width;
      n[4].i = (GLint) height;
   }
}



/*
 * Compile a glCallList call into current display list.
 */
static void gl_save_call( GLuint list )
{
   Node *n = alloc_nodes(2);
   if (n) {
      n[0].kind = NODE_CALL_LIST;
      n[1].ui = list;
   }
}



/*
 * Compile a glCallLists call into current display list.  The difference
 * between this and the above function is that the ListBase value will
 * be added to the display list number before executing.
 */
static void gl_save_call_offset( GLuint list )
{
   Node *n = alloc_nodes(2);
   if (n) {
      n[0].kind = NODE_CALL_LIST_OFFSET;
      n[1].ui = list;
   }
}




/**********************************************************************/
/*                     Display list execution                         */
/**********************************************************************/


/*
 * Execute a display list.  Note that the ListBase offset must have already
 * been added before calling this function.  I.e. the list argument is
 * the absolute list number, not relative to ListBase.
 * Input:  list - list number in [1..]
 */
static void execute_list( GLuint list )
{
   Node *n;
   GLboolean done;
   Kind k;

   if (!glIsList(list))
      return;

   CC.CallDepth++;

   n = CC.ListGroup->List[list-1];
   done = GL_FALSE;
   while (!done) {
      k = n[0].kind;

      switch (k) {
	 /* Frequently called functions: */
         case NODE_VERTEX:
	    gl_execute_vertex( n[1].f, n[2].f, n[3].f, n[4].f );
            break;
         case NODE_NORMAL:
            CC.Current.Normal[0] = n[1].f;
            CC.Current.Normal[1] = n[2].f;
            CC.Current.Normal[2] = n[3].f;
            break;
	 case NODE_COLOR:
            CC.Current.IntColor[0] = n[1].i;
            CC.Current.IntColor[1] = n[2].i;
            CC.Current.IntColor[2] = n[3].i;
            CC.Current.IntColor[3] = n[4].i;
            if (CC.Light.ColorMaterialEnabled) {
               /* Translate this glColor() call into a glMaterial() call */
               GLfloat color[4];
               color[0] = n[1].i / CC.RedScale;
               color[1] = n[2].i / CC.GreenScale;
               color[2] = n[3].i / CC.BlueScale;
               color[3] = n[4].i / CC.AlphaScale;
               gl_material( CC.Light.ColorMaterialFace,
                            CC.Light.ColorMaterialMode, color );
            }
            VB.MonoColor = GL_FALSE;
	    break;
         case NODE_INDEX:
            CC.Current.Index = n[1].ui;
            VB.MonoColor = GL_FALSE;
            break;
         case NODE_BEGIN:
            gl_begin( n[1].e );
            break;
         case NODE_END:
            gl_end();
            break;
	 case NODE_TEXCOORD:
	    CC.Current.TexCoord[0] = n[1].f;
	    CC.Current.TexCoord[1] = n[2].f;
	    CC.Current.TexCoord[2] = n[3].f;
	    CC.Current.TexCoord[3] = n[4].f;
	    break;

	 /* Everything Else: */
         case NODE_ACCUM:
	    gl_accum( n[1].e, n[2].f );
	    break;
         case NODE_ALPHA_FUNC:
	    glAlphaFunc( n[1].e, n[2].f );
	    break;
	 case NODE_BITMAP:
	    gl_bitmap( (GLsizei) n[1].i, (GLsizei) n[2].i,
		       n[3].f, n[4].f,
		       n[5].f, n[6].f,
		       (GLubyte *) n[7].data );
	    break;
	 case NODE_BLEND_COLOR:
	    glBlendColorEXT( n[1].f, n[2].f, n[3].f, n[4].f );
	    break;
	 case NODE_BLEND_EQUATION:
	    glBlendEquationEXT( n[1].e );
	    break;
	 case NODE_BLEND_FUNC:
	    glBlendFunc( n[1].e, n[2].e );
	    break;
         case NODE_CALL_LIST:
	    /* Generated by glCallList(), don't add ListBase */
            if (CC.CallDepth<MAX_LIST_NESTING) {
               execute_list( n[1].ui );
            }
            break;
         case NODE_CALL_LIST_OFFSET:
	    /* Generated by glCallLists() so we must add ListBase */
            if (CC.CallDepth<MAX_LIST_NESTING) {
               execute_list( CC.List.ListBase + n[1].ui );
            }
            break;
	 case NODE_CLEAR:
	    glClear( n[1].bf );
	    break;
	 case NODE_CLEAR_COLOR:
	    glClearColor( n[1].f, n[2].f, n[3].f, n[4].f );
	    break;
	 case NODE_CLEAR_ACCUM:
	    glClearAccum( n[1].f, n[2].f, n[3].f, n[4].f );
	    break;
	 case NODE_CLEAR_DEPTH:
	    glClearDepth( (GLclampd) n[1].f );
	    break;
	 case NODE_CLEAR_INDEX:
	    glClearIndex( n[1].ui );
	    break;
	 case NODE_CLEAR_STENCIL:
	    glClearStencil( n[1].i );
	    break;
         case NODE_CLIP_PLANE:
            {
               GLfloat equ[4];
               equ[0] = n[2].f;
               equ[1] = n[3].f;
               equ[2] = n[4].f;
               equ[3] = n[5].f;
               gl_clipplane( n[1].e, equ );
            }
            break;
	 case NODE_COLOR_MASK:
	    glColorMask( n[1].b, n[2].b, n[3].b, n[4].b );
	    break;
	 case NODE_COLOR_MATERIAL:
	    glColorMaterial( n[1].e, n[2].e );
	    break;
	 case NODE_COPY_PIXELS:
	    gl_copypixels( n[1].i, n[2].i,
			   (GLsizei) n[3].i, (GLsizei) n[4].i, n[5].e );
	    break;
	 case NODE_CULL_FACE:
	    glCullFace( n[1].e );
	    break;
	 case NODE_DEPTH_FUNC:
	    glDepthFunc( n[1].e );
	    break;
	 case NODE_DEPTH_MASK:
	    glDepthMask( n[1].b );
	    break;
	 case NODE_DEPTH_RANGE:
	    glDepthRange( (GLclampd) n[1].f, (GLclampd) n[2].f );
	    break;
	 case NODE_DISABLE:
	    gl_enable( n[1].e, GL_FALSE );
	    break;
	 case NODE_DRAW_BUFFER:
	    glDrawBuffer( n[1].e );
	    break;
	 case NODE_DRAW_PIXELS:
	    gl_drawpixels( (GLsizei) n[1].i, (GLsizei) n[2].i,
			   n[3].e, n[4].e, n[5].data );
	    break;
	 case NODE_EDGE_FLAG:
            CC.Current.EdgeFlag = n[1].e;
            break;
	 case NODE_ENABLE:
	    gl_enable( n[1].e, GL_TRUE );
	    break;
	 case NODE_EVALCOORD1:
	    gl_evalcoord1( n[1].f );
	    break;
	 case NODE_EVALCOORD2:
	    gl_evalcoord2( n[1].f, n[2].f );
	    break;
	 case NODE_EVALMESH1:
	    glEvalMesh1( n[1].e, n[2].i, n[3].i );
	    break;
	 case NODE_EVALMESH2:
	    glEvalMesh2( n[1].e, n[2].i, n[3].i, n[4].i, n[5].i );
	    break;
	 case NODE_EVALPOINT1:
	    glEvalPoint1( n[1].i );
	    break;
	 case NODE_EVALPOINT2:
	    glEvalPoint2( n[1].i, n[2].i );
	    break;
	 case NODE_FOG:
	    {
	       GLfloat p[4];
	       p[0] = n[2].f;
	       p[1] = n[3].f;
	       p[2] = n[4].f;
	       p[3] = n[5].f;
	       gl_fog( n[1].e, p );
	    }
	    break;
	 case NODE_FRONT_FACE:
	    glFrontFace( n[1].e );
	    break;
	 case NODE_HINT:
	    glHint( n[1].e, n[2].e );
	    break;
	 case NODE_INDEX_MASK:
	    glIndexMask( n[1].ui );
	    break;
	 case NODE_INIT_NAMES:
	    glInitNames();
	    break;
         case NODE_LIGHT:
	    {
	       GLfloat p[4];
	       p[0] = n[3].f;
	       p[1] = n[4].f;
	       p[2] = n[5].f;
	       p[3] = n[6].f;
	       gl_light( n[1].e, n[2].e, p );
	    }
	    break;
         case NODE_LIGHT_MODEL:
	    {
	       GLfloat p[4];
	       p[0] = n[2].f;
	       p[1] = n[3].f;
	       p[2] = n[4].f;
	       p[3] = n[5].f;
	       gl_lightmodel( n[1].e, p );
	    }
	    break;
	 case NODE_LINE_STIPPLE:
	    glLineStipple( n[1].i, n[2].us );
	    break;
	 case NODE_LINE_WIDTH:
	    glLineWidth( n[1].f );
	    break;
	 case NODE_LIST_BASE:
	    glListBase( n[1].ui );
	    break;
	 case NODE_LOAD_MATRIX:
	    if (sizeof(Node)==sizeof(GLfloat)) {
	       gl_load_matrix( &n[1].f );
	    }
	    else {
	       GLfloat m[16];
	       GLuint i;
	       for (i=0;i<16;i++) {
		  m[i] = n[1+i].f;
	       }
	       gl_load_matrix( m );
	    }
	    break;
	 case NODE_LOAD_NAME:
	    glLoadName( n[1].ui );
	    break;
	 case NODE_LOGIC_OP:
	    gl_logicop( n[1].e );
	    break;
	 case NODE_MAP1:
	    gl_map1( n[1].e, n[2].f, n[3].f,
		     n[4].i, n[5].i, (GLfloat *) n[6].data );
	    break;
	 case NODE_MAP2:
	    gl_map2( n[1].e,
		     n[2].f, n[3].f,  /* u1, u2 */
		     n[6].i, n[8].i,  /* ustride, uorder */
		     n[4].f, n[5].f,  /* v1, v2 */
		     n[7].i, n[9].i,  /* vstride, vorder */
		     (GLfloat *) n[10].data );
	    break;
	 case NODE_MAPGRID1:
	    gl_mapgrid1( n[1].i, n[2].f, n[3].f );
	    break;
	 case NODE_MAPGRID2:
	    gl_mapgrid2( n[1].i, n[2].f, n[3].f, n[4].i, n[5].f, n[6].f );
	    break;
	 case NODE_MATERIAL:
	    {
	       GLfloat params[4];
	       params[0] = n[3].f;
	       params[1] = n[4].f;
	       params[2] = n[5].f;
	       params[3] = n[6].f;
	       gl_material( n[1].e, n[2].e, params );
	    }
	    break;
         case NODE_MATRIX_MODE:
            glMatrixMode( n[1].e );
            break;
	 case NODE_MULT_MATRIX:
	    if (sizeof(Node)==sizeof(GLfloat)) {
	       gl_mult_matrix( &n[1].f );
	    }
	    else {
	       GLfloat m[16];
	       GLuint i;
	       for (i=0;i<16;i++) {
		  m[i] = n[1+i].f;
	       }
	       gl_mult_matrix( m );
	    }
	    break;
	 case NODE_PASSTHROUGH:
	    gl_passthrough( n[1].f );
	    break;
	 case NODE_PIXEL_MAP:
	    gl_pixel_map( n[1].e, n[2].i, (GLfloat *) n[3].data );
	    break;
	 case NODE_PIXEL_TRANSFER:
	    gl_pixel_transfer( n[1].e, n[2].f );
	    break;
	 case NODE_PIXEL_ZOOM:
	    glPixelZoom( n[1].f, n[2].f );
	    break;
	 case NODE_POINTSIZE:
	    glPointSize( n[1].f );
	    break;
	 case NODE_POLYGON_MODE:
	    glPolygonMode( n[1].e, n[2].e );
	    break;
	 case NODE_POLYGON_OFFSET:
	    glPolygonOffsetEXT( n[1].f, n[2].f );
	    break;
	 case NODE_POP_ATTRIB:
	    gl_pop_attrib();
	    break;
	 case NODE_POP_MATRIX:
	    glPopMatrix();
	    break;
	 case NODE_POP_NAME:
	    glPopName();
	    break;
	 case NODE_PUSH_ATTRIB:
	    gl_push_attrib( n[1].bf );
	    break;
	 case NODE_PUSH_MATRIX:
	    glPushMatrix();
	    break;
	 case NODE_PUSH_NAME:
	    glPushName( n[1].ui );
	    break;
	 case NODE_RASTER_POS:
	    {
	       GLfloat v[4];
	       v[0] = n[1].f;
	       v[1] = n[2].f;
	       v[2] = n[3].f;
	       v[3] = n[4].f;
	       gl_rasterpos( v );
	    }
	    break;
	 case NODE_READ_BUFFER:
	    glReadBuffer( n[1].e );
	    break;
         case NODE_SCALE:
            gl_scale( n[1].f, n[2].f, n[3].f );
            break;
	 case NODE_SCISSOR:
	    glScissor( n[1].i, n[2].i, n[3].i, n[4].i );
	    break;
	 case NODE_SHADE_MODEL:
	    glShadeModel( n[1].e );
	    break;
	 case NODE_STENCIL_FUNC:
	    glStencilFunc( n[1].e, n[2].i, n[3].ui );
	    break;
	 case NODE_STENCIL_MASK:
	    glStencilMask( n[1].ui );
	    break;
	 case NODE_STENCIL_OP:
	    glStencilOp( n[1].e, n[2].e, n[3].e );
	    break;
         case NODE_TEXENV:
            {
               GLfloat params[4];
               params[0] = n[3].f;
               params[1] = n[4].f;
               params[2] = n[5].f;
               params[3] = n[6].f;
               gl_texenv( n[1].e, n[2].e, params );
            }
            break;
         case NODE_TEXGEN:
            {
               GLfloat params[4];
               params[0] = n[3].f;
               params[1] = n[4].f;
               params[2] = n[5].f;
               params[3] = n[6].f;
               gl_texgen( n[1].e, n[2].e, params );
            }
            break;
         case NODE_TEXPARAMETER:
            {
               GLfloat params[4];
               params[0] = n[3].f;
               params[1] = n[4].f;
               params[2] = n[5].f;
               params[3] = n[6].f;
               gl_texparameter( n[1].e, n[2].e, params );
            }
            break;
	 case NODE_TEXIMAGE1D:
	    gl_teximage1d( n[1].i, n[2].i, n[3].i, n[4].i,
                           (GLubyte *) n[5].data );
	    break;
	 case NODE_TEXIMAGE2D:
	    gl_teximage2d( n[1].i, n[2].i, n[3].i, n[4].i, n[5].i,
                           (GLubyte *) n[6].data );
	    break;
         case NODE_TRANSLATE:
            gl_translate( n[1].f, n[2].f, n[3].f );
            break;
	 case NODE_VIEWPORT:
	    gl_viewport( n[1].i, n[2].i, (GLsizei) n[3].i, (GLsizei) n[4].i );
	    break;
	 case NODE_CONTINUE:
	    n = (Node *) n[1].next;
	    break;
	 case NODE_END_OF_LIST:
	    done = GL_TRUE;
	    break;
	 default:
	    printf("Error in execute_list: %d\n", (int) k );
	    gl_error( GL_INVALID_ENUM, "execute_list" );
      }

      /* increment n to point to next compiled command */
      if (k!=NODE_CONTINUE) {
	 n += NodeSize[k];
      }

   }
   CC.CallDepth--;
}



/**********************************************************************/
/*                           GL functions                             */
/**********************************************************************/



/*
 * Test if a display list number is valid.
 */
GLboolean glIsList( GLuint list )
{
   if (list>0 && CC.ListGroup->List[list-1]) {
      return GL_TRUE;
   }
   else {
      return GL_FALSE;
   }
}



/*
 * Delete a sequence of consecutive display lists.
 */
void glDeleteLists( GLuint list, GLsizei range )
{
   GLuint i;

   if (INSIDE_BEGIN_END) {
      gl_error( GL_INVALID_OPERATION, "glDeleteLists" );
      return;
   }
   if (range<0) {
      gl_error( GL_INVALID_VALUE, "glDeleteLists" );
      return;
   }
   for (i=list;i<list+range;i++) {
      if (i<=MAX_DISPLAYLISTS && CC.ListGroup->List[i-1]) {
         destroy_list( i-1 );
         CC.ListGroup->List[i-1] = NULL;
	 CC.ListGroup->Reserved[i-1] = GL_FALSE;
      }
   }
}



/*
 * Return a display list number, n, such that lists n through n+range-1
 * are free.
 */
GLuint glGenLists( GLsizei range )
{
   GLuint i, freecount;

   if (INSIDE_BEGIN_END) {
      gl_error( GL_INVALID_OPERATION, "glGenLists" );
      return 0;
   }
   if (range<=0) {
      gl_error( GL_INVALID_VALUE, "glGenLists" );
      return 0;
   }

   i = 0;
   freecount = 0;
   for (i=0; i<MAX_DISPLAYLISTS; i++ ) {
      if (CC.ListGroup->List[i]==NULL && CC.ListGroup->Reserved[i]==GL_FALSE) {
         freecount++;
         if (freecount==range) {
	    /* we found 'range' consecutive free lists */
	    GLuint k;
	    GLuint n = i-range+2;
	    for (k=n;k<n+range;k++) {
	       CC.ListGroup->Reserved[k-1] = GL_TRUE;
	    }
            return n;
         }
      }
      else {
         freecount = 0;
      }
   }
   return 0;
}



/*
 * Begin a new display list.
 */
void glNewList( GLuint list, GLenum mode )
{
   if (INSIDE_BEGIN_END) {
      gl_error( GL_INVALID_OPERATION, "glNewList" );
      return;
   }
   if (list==0 || list>MAX_DISPLAYLISTS) {
      gl_error( GL_INVALID_VALUE, "glNewList" );
      return;
   }
   if (mode!=GL_COMPILE && mode!=GL_COMPILE_AND_EXECUTE) {
      gl_error( GL_INVALID_ENUM, "glNewList" );
      return;
   }
   if (CurrentListPtr) {
      /* already compiling a display list */
      gl_error( GL_INVALID_OPERATION, "glNewList" );
      return;
   }

   /* Allocate new display list */
   CurrentListNum = list;
   CurrentListPtr = CurrentBlock = (Node *) malloc( sizeof(Node) * BLOCK_SIZE );
   CurrentPos = 0;

   CC.CompileFlag = GL_TRUE;
   if (mode==GL_COMPILE) {
      CC.ExecuteFlag = GL_FALSE;
      CC.VertexFunc = gl_save_vertex;
   }
   else {
      /* Compile and execute */
      CC.ExecuteFlag = GL_TRUE;
      CC.VertexFunc = gl_save_and_execute_vertex;
   }
}



/*
 * End definition of current display list.
 */
void glEndList( void )
{
   Node *n;

   /* Check that a list is under construction */
   if (!CurrentListPtr) {
      gl_error( GL_INVALID_OPERATION, "glEndList" );
      return;
   }

   n = alloc_nodes(1);
   n[0].kind = NODE_END_OF_LIST;

   /* Install the list */
   if (CC.ListGroup->List[CurrentListNum-1]) {
      destroy_list( CurrentListNum-1 );
   }
   CC.ListGroup->List[CurrentListNum-1] = CurrentListPtr;

   CurrentListNum = 0;
   CurrentListPtr = NULL;
   CC.ExecuteFlag = GL_TRUE;
   CC.CompileFlag = GL_FALSE;
   CC.VertexFunc = gl_execute_vertex;
}



void glCallList( GLuint list )
{
   if (CC.CompileFlag) {
      gl_save_call( list );
   }
   if (CC.ExecuteFlag) {
      /* VERY IMPORTANT:  Save the CompileFlag status, turn it off, */
      /* execute the display list, and restore the CompileFlag. */
      GLboolean save_compile_flag;
      save_compile_flag = CC.CompileFlag;
      CC.CompileFlag = GL_FALSE;
      execute_list( list );
      CC.CompileFlag = save_compile_flag;
   }
}



/*
 * Call multiple display lists.
 */
void glCallLists( GLsizei n, GLenum type, const GLvoid *lists )
{
   GLuint i, list;

   if (CC.CompileFlag) {
      for (i=0;i<n;i++) {
	 list = translate_id( i, type, lists );
	 gl_save_call_offset( list );
      }
   }

   if (CC.ExecuteFlag) {
      /* Save the CompileFlag status, turn it off, execute display list, */
      /* and restore the CompileFlag. */
      GLboolean save_compile_flag;
      save_compile_flag = CC.CompileFlag;
      CC.CompileFlag = GL_FALSE;
      for (i=0;i<n;i++) {
	 list = translate_id( i, type, lists );
	 execute_list( CC.List.ListBase + list );
      }
      CC.CompileFlag = save_compile_flag;
   }
}



/*
 * Set the offset added to list numbers in glCallLists.
 */
void glListBase( GLuint base )
{
   if (CC.CompileFlag) {
      gl_save_listbase( base );
   }
   if (CC.ExecuteFlag) {
      if (INSIDE_BEGIN_END) {
	 gl_error( GL_INVALID_OPERATION, "glListBase" );
	 return;
      }
      CC.List.ListBase = base;
   }
}

