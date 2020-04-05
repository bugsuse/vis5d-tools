/* $Id: polygons.c,v 1.52 1996/03/02 21:58:13 brianp Exp $ */

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
$Log: polygons.c,v $
 * Revision 1.52  1996/03/02  21:58:13  brianp
 * only test edge flag for GL_TRIANGLES, GL_QUADS, or GL_POLYGON
 *
 * Revision 1.51  1996/02/12  19:08:45  brianp
 * applied Bill's Feb 8 patches: change eye Z's to clip W's
 *
 * Revision 1.50  1996/02/06  04:14:04  brianp
 * added GL_FRONT_AND_BACK to glCullFace()
 *
 * Revision 1.49  1996/02/06  03:23:54  brianp
 * removed gamma correction code
 *
 * Revision 1.48  1996/02/01  00:55:40  brianp
 * added some DEFARRAY(),UNDEFARRY() macros for Macintosh
 *
 * Revision 1.47  1996/01/30  19:44:05  brianp
 * set U component of texture coords to zero to supress Purify warnings
 *
 * Revision 1.46  1996/01/29  19:06:02  brianp
 * use CC.Orientation in unfilled_polygon()
 *
 * Revision 1.45  1996/01/22  15:34:31  brianp
 * call GL_INTERPOLATE_RGBA() instead of GL_INTERPOLATE_4FIXED()
 *
 * Revision 1.44  1996/01/11  22:51:59  brianp
 * include depth.h added
 *
 * Revision 1.44  1996/01/11  22:51:59  brianp
 * include depth.h added
 *
 * Revision 1.43  1996/01/11  15:48:21  brianp
 * added Y-axis clipping
 * removed obsolete fast_flat/smooth_rgba_z_polygon() functions
 *
 * Revision 1.42  1996/01/02  22:13:40  brianp
 * removed old fixed-point macros
 *
 * Revision 1.41  1995/12/30  17:19:27  brianp
 * use new gl_interp_texcoords function, per Bill Triggs
 *
 * Revision 1.40  1995/12/30  00:59:09  brianp
 * use integer vertex colors instead of floating point
 *
 * Revision 1.38  1995/12/20  15:27:33  brianp
 * VB color indexes changed to GLuint
 *
 * Revision 1.37  1995/12/18  17:26:50  brianp
 * use new GLdepth datatype
 *
 * Revision 1.36  1995/12/14  19:57:44  brianp
 * used new GLfixed type and GL_INTERPOLATE_4FIXED macro for color interp
 *
 * Revision 1.35  1995/12/12  21:48:48  brianp
 * call GL_INTERPOLATE_Z instead of GL_INTERPOLATE_I
 *
 * Revision 1.34  1995/11/30  00:21:55  brianp
 * restored old gl_polygon_edge function
 *
 * Revision 1.33  1995/11/14  21:49:40  brianp
 * optimized polygon rendering setup
 *
 * Revision 1.32  1995/11/04  20:09:07  brianp
 * optimized gl_polygon_edge()
 *
 * Revision 1.31  1995/10/29  19:14:44  brianp
 * added glPolygonOffsetEXT display list support
 *
 * Revision 1.30  1995/10/27  20:30:29  brianp
 * added glPolygonOffsetEXT()
 *
 * Revision 1.29  1995/10/24  20:52:15  brianp
 * renamed COMPUTE_PLANE_Z as gl_compute_z
 * renamed polygon_edge as gl_polygon_edge
 * use new color interpolation based on fixed point arithmetic
 * removed dead code
 *
 * Revision 1.28  1995/10/22  21:20:50  brianp
 * changed 0.5 to 0.5F in COMPUTE_PLANE_Z calls
 *
 * Revision 1.27  1995/10/22  20:29:37  brianp
 * removed dead code
 *
 * Revision 1.26  1995/10/19  15:48:28  brianp
 * added gamma support
 * changed DD.color arguments to GLubytes
 *
 * Revision 1.25  1995/10/17  21:40:04  brianp
 * added fast_flat_rgba_z_polygon() function
 * removed fast_ci/rgb_polygon() functions
 *
 * Revision 1.24  1995/09/20  18:20:39  brianp
 * prototype device driver changes described
 *
 * Revision 1.23  1995/09/13  14:51:05  brianp
 * use CC.NewState convention
 * use DEFARRAY/UNDEFARRAY macros for Mac
 * replaced VB.Vs and VB.Vt with VB.TexCoord
 *
 * Revision 1.22  1995/07/28  21:34:16  brianp
 * support polygons draw in viewport larger than window
 *
 * Revision 1.21  1995/07/15  14:04:50  brianp
 * fixed texture coord interpolation bug
 *
 * Revision 1.20  1995/07/11  17:47:45  brianp
 * round window coords to nearest int, not truncate, in fast_ci|rgba_polygon
 *
 * Revision 1.19  1995/06/20  16:32:40  brianp
 * new depth buffer value computations
 * introduced new triangle rasterizer code, experimental, not used yet
 *
 * Revision 1.18  1995/06/12  15:43:35  brianp
 * changed color arrays to GLubyte
 * new interpolation functions
 *
 * Revision 1.17  1995/06/05  20:27:53  brianp
 * added CC.Polygon.Unfilled stuff
 *
 * Revision 1.16  1995/06/02  13:59:46  brianp
 * added fast_smooth_rgba_z_polygon() function
 *
 * Revision 1.15  1995/05/31  19:34:12  brianp
 * replaced MAX_VERTICES with VB_MAX
 *
 * Revision 1.14  1995/05/22  21:02:41  brianp
 * Release 1.2
 *
 * Revision 1.13  1995/05/12  17:01:05  brianp
 * changed CC.Mode!=0 to INSIDE_BEGIN_END
 *
 * Revision 1.12  1995/04/18  15:48:23  brianp
 * fixed assignment of NULL to function pointers to prevent warnings on Suns
 *
 * Revision 1.11  1995/04/12  15:36:15  brianp
 * updated to use DD.draw_* function pointers
 *
 * Revision 1.10  1995/03/27  20:31:53  brianp
 * new Texture.Enabled scheme
 *
 * Revision 1.9  1995/03/24  19:28:13  brianp
 * introduced VB
 *
 * Revision 1.8  1995/03/07  14:20:59  brianp
 * updated for new XSetForeground/GC scheme
 *
 * Revision 1.7  1995/03/04  19:29:44  brianp
 * 1.1 beta revision
 *
 * Revision 1.6  1995/03/02  19:18:54  brianp
 * new RasterMask logic
 * fixed Vcolor/Vbcolor bug in textured_polygon()
 *
 * Revision 1.5  1995/02/27  22:49:01  brianp
 * modified for PB
 *
 * Revision 1.4  1995/02/27  15:08:17  brianp
 * added Vcolor/Vindex scheme
 *
 * Revision 1.3  1995/02/26  22:58:57  brianp
 * made COMPUTE_PLANE_Z a function w/ FP overflow checking
 *
 * Revision 1.2  1995/02/25  22:07:43  brianp
 * more debugging code
 *
 * Revision 1.1  1995/02/24  14:26:49  brianp
 * Initial revision
 *
 */


#include <string.h>
#include "context.h"
#include "dd.h"
#include "depth.h"
#include "feedback.h"
#include "interp.h"
#include "lines.h"
#include "list.h"
#include "macros.h"
#include "points.h"
#include "span.h"
#include "vb.h"

/*#include "triangle.h"*/


void glCullFace( GLenum mode )
{
   if (CC.CompileFlag) {
      gl_save_cullface( mode );
   }
   if (CC.ExecuteFlag) {
      if (mode!=GL_FRONT && mode!=GL_BACK && mode!=GL_FRONT_AND_BACK) {
	 gl_error( GL_INVALID_ENUM, "glCullFace" );
	 return;
      }
      if (INSIDE_BEGIN_END) {
	 gl_error( GL_INVALID_OPERATION, "glCullFace" );
	 return;
      }
      CC.Polygon.CullFaceMode = mode;
      CC.NewState = GL_TRUE;
   }
}



void glFrontFace( GLenum mode )
{
   if (CC.CompileFlag) {
      gl_save_frontface( mode );
   }
   if (CC.ExecuteFlag) {
      if (INSIDE_BEGIN_END) {
	 gl_error( GL_INVALID_OPERATION, "glFrontFace" );
	 return;
      }
      if (mode!=GL_CW && mode!=GL_CCW) {
	 gl_error( GL_INVALID_ENUM, "glFrontFace" );
	 return;
      }
      CC.Polygon.FrontFace = mode;
   }  
}



void glPolygonMode( GLenum face, GLenum mode )
{
   if (CC.CompileFlag) {
      gl_save_polygonmode( face, mode );
   }
   if (CC.ExecuteFlag) {
      if (INSIDE_BEGIN_END) {
	 gl_error( GL_INVALID_OPERATION, "glPolygonMode" );
	 return;
      }
      if (face!=GL_FRONT && face!=GL_BACK && face!=GL_FRONT_AND_BACK) {
	 gl_error( GL_INVALID_ENUM, "glPolygonMode(face)" );
	 return;
      }
      else if (mode!=GL_POINT && mode!=GL_LINE && mode!=GL_FILL) {
	 gl_error( GL_INVALID_ENUM, "glPolygonMode(mode)" );
	 return;
      }

      if (face==GL_FRONT || face==GL_FRONT_AND_BACK) {
	 CC.Polygon.FrontMode = mode;
      }
      if (face==GL_BACK || face==GL_FRONT_AND_BACK) {
	 CC.Polygon.BackMode = mode;
      }

      /* Compute a handy "shortcut" value: */
      if (CC.Polygon.FrontMode!=GL_FILL || CC.Polygon.BackMode!=GL_FILL) {
	 CC.Polygon.Unfilled = GL_TRUE;
      }
      else {
	 CC.Polygon.Unfilled = GL_FALSE;
      }

      CC.NewState = GL_TRUE;
   }
}



void glPolygonStipple( const GLubyte *mask )
{
   if (CC.CompileFlag) {
      /*gl_save_polygon_stipple( mask );*/
   }
   if (CC.ExecuteFlag) {
      /* TODO:  bit twiddling, unpacking */
      if (INSIDE_BEGIN_END) {
	 gl_error( GL_INVALID_OPERATION, "glPolygonStipple" );
	 return;
      }
      MEMCPY( CC.PolygonStipple, mask, 32*sizeof(GLuint) );
   }
}



void glGetPolygonStipple( GLubyte *mask )
{
   /* TODO */
}



#ifdef GL_EXT_polygon_offset
void glPolygonOffsetEXT( GLfloat factor, GLfloat bias )
{
   if (CC.CompileFlag) {
      gl_save_polygonoffset( factor, bias );
   }
   if (CC.ExecuteFlag) {
      if (INSIDE_BEGIN_END) {
         gl_error( GL_INVALID_OPERATION, "glPolygonOffsetEXT" );
         return;
      }
      CC.Polygon.OffsetFactor = factor;
      CC.Polygon.OffsetBias = bias;
   }
}
#endif



/**********************************************************************/
/*****                    Rasterization                           *****/
/**********************************************************************/


/*
 * Summary of polygon drawing functions:
 *   feedback_polygon - when rendering mode is GL_FEEDBACK
 *   select_polygon - when rendering mode is GL_SELECT
 *   flat_ci_polygon - flat-shaded, any raster ops, RGBA polygon
 *   smooth_ci_polygon - smooth-shaded, any raster ops, CI polygon
 *   flat_rgba_polygon - flat-shaded, any raster ops, RGBA polygon
 *   smooth_rgba_polygon - smooth-shaded, any raster ops, RGBA polygon
 *   textured_polygon - smooth-shaded, textured RGBA polygon
 */


/*
 * All polygon drawing functions have the same arguments:
 * n      - number of vertices
 * vlist  - array of indexes into the vertex list
 * pv     - provoking vertex: which vertex color to use for flat shading.
 */




/*
 * Put polygon in feedback buffer.
 */
static void feedback_polygon( GLuint n, GLuint vlist[], GLuint pv )
{
   GLuint i;

   APPEND_TOKEN( (GLfloat) GL_POLYGON_TOKEN );
   APPEND_TOKEN( (GLfloat) n );

   for (i=0;i<n;i++) {
      GLfloat x, y, z, w;
      GLfloat tc[4];
      GLuint j = vlist[i];
      GLfloat color[4];

      x = VB.Win[j][0] - CC.RasterOffsetX;
      y = VB.Win[j][1] - CC.RasterOffsetY;
      z = VB.Win[j][2];
      w = VB.Clip[j][3];

      /* convert color from integer back to a float in [0,1] */
      color[0] = (GLfloat) VB.Color[j][0] / CC.RedScale;
      color[1] = (GLfloat) VB.Color[j][1] / CC.GreenScale;
      color[2] = (GLfloat) VB.Color[j][2] / CC.BlueScale;
      color[3] = (GLfloat) VB.Color[j][3] / CC.AlphaScale;

      tc[0] = VB.TexCoord[j][0];
      tc[1] = VB.TexCoord[j][1];
      tc[2] = 0.0F;          /* TODO: R, Q components */
      tc[3] = 1.0F;

      gl_feedback_vertex( x, y, z, w, color, (GLfloat) VB.Index[j], tc );
   }
}



/*
 * Put polygon in selection buffer.
 */
static void select_polygon( GLuint n, GLuint vlist[], GLuint pv )
{
   GLuint i;

   CC.HitFlag = GL_TRUE;
   for (i=0;i<n;i++) {
      GLuint j = vlist[i];
      GLfloat wz = VB.Win[j][2];
      if (wz < CC.HitMinZ) {
	 CC.HitMinZ = wz;
      }
      if (wz > CC.HitMaxZ) {
	 CC.HitMaxZ = wz;
      }
   }
}



/*
 * Left and right boundary values for polygon scan conversion
 */
static GLint lx[MAX_HEIGHT], rx[MAX_HEIGHT];	/* integer X bounds */
static GLint li[MAX_HEIGHT], ri[MAX_HEIGHT];	/* Color Index */
static GLfixed lr[MAX_HEIGHT], rr[MAX_HEIGHT];	/* Red */
static GLfixed lg[MAX_HEIGHT], rg[MAX_HEIGHT];	/* Green */
static GLfixed lb[MAX_HEIGHT], rb[MAX_HEIGHT];	/* Blue */
static GLfixed la[MAX_HEIGHT], ra[MAX_HEIGHT];	/* Alpha */




static void flat_ci_polygon( GLuint n, GLuint vlist[], GLuint pv )
{
#define INTERP_Z
#define CLIP_Y

#define SETUP_CODE			\
   GLuint index = VB.Index[pv];		\
   if (!VB.MonoColor) {			\
      /* set the color index */		\
      (*DD.index)( index );		\
   }

#define INNER_CODE							\
   GLdepth zspan[MAX_WIDTH];						\
   GLint i;								\
   for (i=0;i<len;i++) {						\
      zspan[i] = FixedToUns(fz);  fz += fdzdx;				\
   }									\
   gl_write_monoindex_span( len, xmin, y, zspan, index, GL_POLYGON );

#include "polytemp.h"
}



static void smooth_ci_polygon( GLuint n, GLuint vlist[], GLuint pv )
{
#define INTERP_INDEX
#define INTERP_Z
#define CLIP_Y

#define INNER_CODE							\
   GLdepth zspan[MAX_WIDTH];						\
   GLuint index[MAX_WIDTH];						\
   GLint i;								\
   for (i=0;i<len;i++) {						\
      zspan[i] = FixedToUns(fz);   fz += fdzdx;				\
      index[i] = FixedToInt(fi);     fi += fdidx;			\
   }									\
   gl_write_index_span( len, xmin, y, zspan, index, GL_POLYGON );

#include "polytemp.h"
}



static void flat_rgba_polygon( GLuint n, GLuint vlist[], GLuint pv )
{
#define INTERP_Z
#define CLIP_Y

#define SETUP_CODE				\
   if (!VB.MonoColor) {				\
      /* set the color */			\
      GLubyte r = VB.Color[pv][0];		\
      GLubyte g = VB.Color[pv][1];		\
      GLubyte b = VB.Color[pv][2];		\
      GLubyte a = VB.Color[pv][3];		\
      (*DD.color)( r, g, b, a );		\
   }


#define INNER_CODE						\
    GLdepth zspan[MAX_WIDTH];					\
    GLint i;							\
    for (i=0;i<len;i++) {					\
       zspan[i] = FixedToUns(fz);   fz += fdzdx;		\
    }								\
    gl_write_monocolor_span( len, xmin, y, zspan,		\
                             VB.Color[pv][0], VB.Color[pv][1],	\
                             VB.Color[pv][2], VB.Color[pv][3],	\
			     GL_POLYGON );

#include "polytemp.h"
}



static void smooth_rgba_polygon( GLuint n, GLuint vlist[], GLuint pv )
{
#define INTERP_COLOR
#define INTERP_ALPHA
#define INTERP_Z
#define CLIP_Y

#define INNER_CODE						\
   GLdepth zspan[MAX_WIDTH];					\
   GLubyte red[MAX_WIDTH], green[MAX_WIDTH];			\
   GLubyte blue[MAX_WIDTH], alpha[MAX_WIDTH];			\
   GLint i;							\
   for (i=0;i<len;i++) {					\
      zspan[i] = FixedToUns(fz);    fz += fdzdx;		\
      red[i]   = FixedToInt(fr);    fr += fdrdx;		\
      green[i] = FixedToInt(fg);    fg += fdgdx;		\
      blue[i]  = FixedToInt(fb);    fb += fdbdx;		\
      alpha[i] = FixedToInt(fa);    fa += fdadx;		\
   }								\
   gl_write_color_span( len, xmin, y, zspan,			\
			red, green, blue, alpha, GL_POLYGON );

#include "polytemp.h"
}




/* Max number of pixels along a polygon's edge */
#if MAX_WIDTH > MAX_HEIGHT
#  define EDGEMAX MAX_WIDTH
#else
#  define EDGEMAX MAX_HEIGHT
#endif

static GLfloat flx[MAX_HEIGHT], frx[MAX_HEIGHT];/* float X bounds */
static GLfloat ls[MAX_HEIGHT], rs[MAX_HEIGHT];	/* S */
static GLfloat lt[MAX_HEIGHT], rt[MAX_HEIGHT];	/* T */
static GLfloat lu[MAX_HEIGHT], ru[MAX_HEIGHT];	/* R */
static GLfloat lv[MAX_HEIGHT], rv[MAX_HEIGHT];	/* Q */


/*
 * Compute the location of the pixels along the edge of a polygon.
 * Input:  x1,y1, x2,y2 - endpoints of the polygon edge
 * Output:  x, y - array of edge coords
 * Return:  number of elements in x[], and y[]
 *
 * ONLY USED FOR TEXTURED POLYGON FUNCTION AT THIS TIME!
 */
static GLuint gl_polygon_edge( GLfloat x1, GLfloat y1, GLfloat x2, GLfloat y2,
                               GLfloat x[], GLint y[])
{
   GLint ymin, ymax, iy;
   GLfloat slope;
   GLuint i;

   if (y1==y2) {
      /* skip horizontal edges */
      return 0;
   }

   slope = (x2-x1) / (y2-y1);
   i = 0;

   if (y1<y2) {
      ymin = (int) (y1 + 0.5F);
      ymax = (int) (y2 + 0.5F) - 1;
      for (iy=ymin; iy<=ymax; iy++) {
         x[i] = x1 + ((iy+0.5F) - y1) * slope;
         y[i] = iy;
         i++;
      }
   }
   else {
      ymin = (int) (y2 + 0.5F);
      ymax = (int) (y1 + 0.5F) - 1;
      for (iy=ymax; iy>=ymin; iy--) {
         x[i] = x1 + ((iy+0.5F) - y1) * slope;
         y[i] = iy;
         i++;
      }
   }
   return i;
}



/*
 * Evaluate the current polygon's plane equation at (x,y) to get Z (integer).
 */
static GLint gl_compute_z( GLfloat x, GLfloat y )
{
   GLfloat fz;

   fz = (CC.PlaneD - CC.PlaneA*x - CC.PlaneB*y) / CC.PlaneC;

   if (fz<0.0F) {
      return 0;
   }
   else if (fz>1.0F) {
      return MAX_DEPTH;
   }

   return (GLint) (fz * DEPTH_SCALE);
}




/*
 * Render a texture mapped polygon.  This function is basically unchanged
 * from Mesa 1.2.5 while all other polygon rasterizers have been rewritten.
 * Textured polygons will improved eventually...
 */
static void textured_polygon( GLuint n, GLuint vlist[], GLuint pv )
{
   GLint i, j, y;
   GLint ymin, ymax;
   GLfloat lw[MAX_HEIGHT], rw[MAX_HEIGHT];

   /* find min and max of window coordinate Y values */
   {
      GLfloat min = 1.0e10;
      GLfloat max = -1.0e10;
      if (n==3) {
         GLfloat winy;
         winy = VB.Win[vlist[0]][1];
         if (winy > max)  max = winy;
         if (winy < min)  min = winy;
         winy = VB.Win[vlist[1]][1];
         if (winy > max)  max = winy;
         if (winy < min)  min = winy;
         winy = VB.Win[vlist[2]][1];
         if (winy > max)  max = winy;
         if (winy < min)  min = winy;
      }
      else {
         for (i=0;i<n;i++) {
            GLfloat winy = VB.Win[vlist[i]][1];
            if (winy > max)  max = winy;
            if (winy < min)  min = winy;
         }
      }
      ymin = (GLint) min;
      ymin = CLAMP( ymin, 0, MAX_HEIGHT-1 ) ;
      ymax = (GLint) max;
      ymax = CLAMP( ymax, 0, MAX_HEIGHT-1 ) ;
   }

   /* init edge bounds */
   for (y=ymin;y<=ymax;y++) {
      flx[y] = (GLfloat) (MAX_WIDTH+1);
      frx[y] = -1.0;
   }

   /* process edges to compute bounds */
   for (i=0;i<n;i++) {
      GLuint j0, j1, len;
      DEFARRAY( GLfloat, ex, EDGEMAX );  /* x in win coords */
      DEFARRAY( GLint, ey, EDGEMAX );    /* y in win coords */
      DEFARRAY( GLfloat, ew, EDGEMAX );  /* w in clip coords */
      DEFARRAY( GLfloat, es, EDGEMAX );  /* texture s,t,r,q */
      DEFARRAY( GLfloat, et, EDGEMAX );
      DEFARRAY( GLfloat, eu, EDGEMAX );
      DEFARRAY( GLfloat, ev, EDGEMAX );
      GLfixed r0, g0, b0, a0, r1, g1, b1, a1, dr, dg, db, da;


      j0 = (i==0) ? vlist[n-1] : vlist[i-1];
      j1 = vlist[i];

      /* compute edge pixels */
      /* add 0.5 to undo the CC.RasterOffset */
      len = gl_polygon_edge( VB.Win[j0][0]+0.5F, VB.Win[j0][1]+0.5F,
                             VB.Win[j1][0]+0.5F, VB.Win[j1][1]+0.5F,
                             ex, ey );

      /* setup for edge color interpolation */
      if (CC.Light.ShadeModel==GL_FLAT) {
         r0 = r1 = VB.Color[pv][0] << FIXED_SHIFT;
         g0 = g1 = VB.Color[pv][1] << FIXED_SHIFT;
         b0 = b1 = VB.Color[pv][2] << FIXED_SHIFT;
         a0 = a1 = VB.Color[pv][3] << FIXED_SHIFT;
      }
      else {
         r0 = VB.Color[j0][0];      r1 = VB.Color[j1][0];
         g0 = VB.Color[j0][1];      g1 = VB.Color[j1][1];
         b0 = VB.Color[j0][2];      b1 = VB.Color[j1][2];
         a0 = VB.Color[j0][3];      a1 = VB.Color[j1][3];
      }
      if (len>1) {
         GLint n = len-1;
         dr = (r1-r0) / n;
         dg = (g1-g0) / n;
         db = (b1-b0) / n;
         da = (a1-a0) / n;
      }
      else {
         dr = dg = db = da = 0;
      }

      gl_interp_texcoords( len, GL_TRUE, VB.Clip[j0][3], VB.Clip[j1][3],
			   VB.TexCoord[j0][0], VB.TexCoord[j1][0],
                           VB.TexCoord[j0][1], VB.TexCoord[j1][1],
                           VB.TexCoord[j0][2], VB.TexCoord[j1][2],
                           VB.TexCoord[j0][3], VB.TexCoord[j1][3],
			   es, et, eu, ev, ew );

      /* update span bounds */
      for (j=0;j<len;j++) {
	 register GLfloat x = ex[j];
	 register GLint y = ey[j];

	 if (y>=0 && y<MAX_HEIGHT) {
	    /* update left and right span bounds */
	    if (x < flx[y]) {
	       flx[y] = x;
	       lr[y] = r0;
	       lg[y] = g0;
	       lb[y] = b0;
	       la[y] = a0;
	       ls[y] = es[j];
	       lt[y] = et[j];
	       lu[y] = eu[j] = 0.0;  /* just set to zero for now */
	       lv[y] = ev[j];
	       lw[y] = ew[j];
	    }
	    if (x > frx[y]) {
	       frx[y] = x;
	       rr[y] = r0;
	       rg[y] = g0;
	       rb[y] = b0;
	       ra[y] = a0;
	       rs[y] = es[j];
	       rt[y] = et[j];
	       ru[y] = eu[j] = 0.0;  /* just set to zero for now */
	       rv[y] = ev[j];
	       rw[y] = ew[j];
	    }
	 }
         r0 += dr;   g0 += dg;   b0 += db;   a0 += da;
      }
      UNDEFARRAY( ex );
      UNDEFARRAY( ey );
      UNDEFARRAY( ew );
      UNDEFARRAY( es );
      UNDEFARRAY( et );
      UNDEFARRAY( eu );
      UNDEFARRAY( ev );
   }

   /* process spans */
   for (y=ymin;y<=ymax;y++) {
      GLint xmin = (GLint) (flx[y] + 0.5);
      GLint xmax = (GLint) (frx[y] - 0.5);
      GLint len = xmax-xmin+1;
      if (len>0) {
	 GLint z0, z1;
	 GLdepth zspan[MAX_WIDTH];
         /* Allocate arrays dynamically on Mac */
         DEFARRAY(GLubyte,red,MAX_WIDTH);
         DEFARRAY(GLubyte,green,MAX_WIDTH);
         DEFARRAY(GLubyte,blue,MAX_WIDTH);
         DEFARRAY(GLubyte,alpha,MAX_WIDTH);
         DEFARRAY(GLfloat,s,MAX_WIDTH);
         DEFARRAY(GLfloat,t,MAX_WIDTH);
         DEFARRAY(GLfloat,u,MAX_WIDTH);
         DEFARRAY(GLfloat,v,MAX_WIDTH);

	 /* interpolate z, colors and tex coords */
	 z0 = gl_compute_z( flx[y]+0.5F, (GLfloat) y + 0.5F );
	 z1 = gl_compute_z( frx[y]-0.5F, (GLfloat) y + 0.5F );
	 GL_INTERPOLATE_Z( len, z0, z1, zspan );
	 GL_INTERPOLATE_RGBA( len,
                              lr[y], rr[y], red,
                              lg[y], rg[y], green,
                              lb[y], rb[y], blue,
                              la[y], ra[y], alpha );

	 gl_interp_texcoords( len, GL_FALSE,
			      lw[y], rw[y],
			      ls[y], rs[y], 
			      lt[y], rt[y],
			      lu[y], ru[y],
			      lv[y], rv[y],
			      s, t, u, v, NULL );

	 gl_write_texture_span( len, xmin, y, zspan,  s, t,
				red, green, blue, alpha, GL_POLYGON );

         /* Free dynamic arrays on Mac */
         UNDEFARRAY(red);
         UNDEFARRAY(green);
         UNDEFARRAY(blue);
         UNDEFARRAY(alpha);
         UNDEFARRAY(s);
         UNDEFARRAY(t);
         UNDEFARRAY(u);
         UNDEFARRAY(v); 
      }
   }
}




/*
 * Render a polygon whose front or back rendering modes are point or line.
 */
static void unfilled_polygon( GLuint n, GLuint vlist[], GLuint pv )
{
   GLuint i, j, j0, j1;
   GLboolean edge;

   CC.StippleCounter = 0;  /* in case we're drawing polygon outline */

   if (CC.Mode==GL_TRIANGLES || CC.Mode==GL_QUADS || CC.Mode==GL_POLYGON) {
      edge = GL_FALSE;
   }
   else {
      edge = GL_TRUE;
   }

   if (CC.Orientation==0) {
      /* Front facing */
      if (CC.Polygon.FrontMode==GL_POINT) {
	 for (i=0;i<n;i++) {
	    j = vlist[i];
	    if (edge || VB.Edgeflag[j]) {
               VB.Win[j][0] += 1.0F;    /* adjust window coords because */
               VB.Win[j][1] += 1.0F;    /* raster offsets are differnt */
	       (*CC.PointsFunc)( j, j );  /* for points and polygons */
               VB.Win[j][0] -= 1.0F;
               VB.Win[j][1] -= 1.0F;
	    }
	 }
      }
      else if (CC.Polygon.FrontMode==GL_LINE) {
	 for (i=0;i<n;i++) {
	    j0 = (i==0) ? vlist[n-1] : vlist[i-1];
	    j1 = vlist[i];
	    if (edge || VB.Edgeflag[j0]) {
               VB.Win[j0][0] += 1.0F;
               VB.Win[j0][1] += 1.0F;
               VB.Win[j1][0] += 1.0F;
               VB.Win[j1][1] += 1.0F;
	       (*CC.LineFunc)( j0, j1, pv );
               VB.Win[j0][0] -= 1.0F;
               VB.Win[j0][1] -= 1.0F;
               VB.Win[j1][0] -= 1.0F;
               VB.Win[j1][1] -= 1.0F;
	    }
	 }
      }
      else {
	 (*CC.AuxPolygonFunc)( n, vlist, pv );
      }
   }
   else {
      /* Back facing */
      if (CC.Polygon.BackMode==GL_POINT) {
	 for (i=0;i<n;i++) {
	    j = vlist[i];
	    if (edge || VB.Edgeflag[j]) {
               VB.Win[j][0] += 1.0F;    /* adjust window coords because */
               VB.Win[j][1] += 1.0F;    /* raster offsets are differnt */
	       (*CC.PointsFunc)( j, j );  /* for points and polygons */
               VB.Win[j][0] -= 1.0F;
               VB.Win[j][1] -= 1.0F;
	    }
	 }
      }
      else if (CC.Polygon.BackMode==GL_LINE) {
	 for (i=0;i<n;i++) {
	    j0 = (i==0) ? vlist[n-1] : vlist[i-1];
	    j1 = vlist[i];
	    if (edge || VB.Edgeflag[j0]) {
               VB.Win[j0][0] += 1.0F;
               VB.Win[j0][1] += 1.0F;
               VB.Win[j1][0] += 1.0F;
               VB.Win[j1][1] += 1.0F;
	       (*CC.LineFunc)( j0, j1, pv );
               VB.Win[j0][0] -= 1.0F;
               VB.Win[j0][1] -= 1.0F;
               VB.Win[j1][0] -= 1.0F;
               VB.Win[j1][1] -= 1.0F;
	    }
	 }
      }
      else {
	 (*CC.AuxPolygonFunc)( n, vlist, pv );
      }
   }
}



/*
 * Determine which polygon rendering function to use given the current
 * rendering context.
 */
void gl_set_polygon_function( void )
{
   static GLboolean first_time = GL_TRUE;

   if (first_time) {
      /* one-time polygon initialization */
      GLint i;
      for (i=0;i<MAX_HEIGHT;i++) {
         lx[i] = MAX_WIDTH;
         rx[i] = -1;
      }
      first_time = GL_FALSE;
   }

   if (CC.RenderMode==GL_RENDER) {
      CC.PolygonFunc = (*DD.get_polygon_func)();
      if (CC.PolygonFunc) {
         /* Device Driver will draw the polygon */
      }
      else if (CC.Texture.Enabled) {
	 /* textured */
	 CC.PolygonFunc = textured_polygon;
      }
      else {
	 if (CC.Light.ShadeModel==GL_SMOOTH) {
	    /* smooth shaded, no texturing, stippled or some raster ops */
	    CC.PolygonFunc = CC.RGBAflag ? smooth_rgba_polygon : smooth_ci_polygon;
	 }
	 else {
	    /* flat shaded, no texturing, stippled or some raster ops */
	    CC.PolygonFunc = CC.RGBAflag ? flat_rgba_polygon : flat_ci_polygon;
	 }
      }

      /* PolygonMode */
      if (CC.Polygon.Unfilled) {
	 /* front or back are to be rendered as points or lines */
	 if (!CC.PointsFunc) {
	    gl_set_point_function();
	 }
	 if (!CC.LineFunc) {
	    gl_set_line_function();
	 }
	 CC.AuxPolygonFunc = CC.PolygonFunc;
	 CC.PolygonFunc = unfilled_polygon;
      }
   }
   else if (CC.RenderMode==GL_FEEDBACK) {
      CC.PolygonFunc = feedback_polygon;
   }
   else {
      /* GL_SELECT mode */
      CC.PolygonFunc = select_polygon;
   }
}


