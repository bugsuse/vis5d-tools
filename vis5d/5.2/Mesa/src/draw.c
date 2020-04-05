/* $Id: draw.c,v 1.61 1996/03/15 16:30:53 brianp Exp $ */

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
$Log: draw.c,v $
 * Revision 1.61  1996/03/15  16:30:53  brianp
 * don't use MAP_[XYZ] macros in gl_rasterpos() because of RasterOffset[XY]
 *
 * Revision 1.60  1996/02/26  15:06:42  brianp
 * replaced CC.Current.Color with CC.Current.IntColor
 *
 * Revision 1.59  1996/02/15  16:53:46  brianp
 * split xform_vb() into transform_vb_part1() and gl_transform_vb_part2()
 *
 * Revision 1.58  1996/02/14  16:56:50  brianp
 * replaced gl_index_shade() with gl_index_shade_vertices()
 *
 * Revision 1.58  1996/02/14  16:56:50  brianp
 * replaced gl_index_shade() with gl_index_shade_vertices()
 *
 * Revision 1.57  1996/02/13  17:47:35  brianp
 * call gl_color_shade_vertices_fast() for optimized lighting
 *
 * Revision 1.56  1996/02/06  04:13:37  brianp
 * use CC.Polygon.CullBits
 *
 * Revision 1.55  1996/02/06  03:23:54  brianp
 * removed gamma correction code
 *
 * Revision 1.54  1996/02/01  00:57:11  brianp
 * rearranged code for polygons lit with two-sided lighting
 *
 * Revision 1.53  1996/01/29  19:10:28  brianp
 * fixed a bug in using multiple clipping planes
 *
 * Revision 1.52  1996/01/29  19:06:16  brianp
 * set CC.Orientation field for unfilled polygons
 *
 * Revision 1.51  1996/01/22  15:36:35  brianp
 * cleaned up gl_begin() and call PB_INIT() instead of gl_init_pb()
 *
 * Revision 1.50  1996/01/17  19:57:38  brianp
 * fixed a bug involving the VB clip flags and long primitive strips
 *
 * Revision 1.49  1996/01/16  15:06:48  brianp
 * check that polygons have at least 3 vertices per Michael Pichler
 *
 * Revision 1.48  1996/01/07  22:50:03  brianp
 * replaced gl_color_shade() calls with gl_color_shade_vertices()
 *
 * Revision 1.47  1996/01/05  01:23:09  brianp
 * added profiling
 *
 * Revision 1.46  1995/12/30  17:15:11  brianp
 * gl_eval_vertex now takes integer colors
 *
 * Revision 1.45  1995/12/30  00:50:19  brianp
 * now use integer VB.color and ColorShift
 *
 * Revision 1.44  1995/12/21  17:39:22  brianp
 * optimized gl_execute_vertex() function
 *
 * Revision 1.43  1995/12/20  17:28:15  brianp
 * removed gl_index and gl_color, now they're inlined
 *
 * Revision 1.42  1995/12/20  15:26:25  brianp
 * changed VB color indexes to GLuint
 *
 * Revision 1.41  1995/12/19  22:16:42  brianp
 * added CC.RasterOffsetX/Y
 *
 * Revision 1.40  1995/11/22  13:36:18  brianp
 * small optimization to VB.Win[][] computation
 *
 * Revision 1.39  1995/11/03  22:35:08  brianp
 * call new vertex fogging functions
 *
 * Revision 1.38  1995/11/03  17:40:52  brianp
 * removed unused variables
 *
 * Revision 1.37  1995/11/01  21:45:26  brianp
 * use new gl_color_shade_vertices() function
 *
 * Revision 1.36  1995/10/27  21:37:44  brianp
 * implemented glPolygonOffsetEXT
 * optimized computation of CC.Plane[ABCD]
 *
 * Revision 1.35  1995/10/19  15:47:00  brianp
 * added gamma support
 *
 * Revision 1.34  1995/10/17  21:42:55  brianp
 * enabled VB.MonoColor logic
 *
 * Revision 1.33  1995/09/28  19:40:02  brianp
 * replaced ClipFlag[] with Unclipped[]
 *
 * Revision 1.32  1995/09/27  18:33:24  brianp
 * added call to new gl_transform_normals function
 *
 * Revision 1.31  1995/09/26  16:04:55  brianp
 * moved transform_points to xform.c
 *
 * Revision 1.30  1995/09/25  19:23:40  brianp
 * implemented per-vertex glMaterial calls
 *
 * Revision 1.29  1995/09/22  22:12:41  brianp
 * optimized the xform_vb function to unroll loops, use maximum registers
 *
 * Revision 1.28  1995/09/22  16:51:16  brianp
 * fixed Anyclipped typo
 *
 * Revision 1.27  1995/09/22  16:49:42  brianp
 * added VB.AnyClipped logic
 * added conditionals to gl_execute_vertex
 * prototyped VB.MonoColor logic
 *
 * Revision 1.26  1995/09/17  19:31:03  brianp
 * better logic for computing plane eq of polygons, accidental culling fixed
 *
 * Revision 1.25  1995/09/15  18:39:43  brianp
 * complete restructuring to "vectorize" the vertex pipeline
 *
 * Revision 1.24  1995/07/28  21:33:21  brianp
 * code cleanup, adapt gl_index_shade calls for GLfloat result
 *
 * Revision 1.23  1995/06/20  16:29:36  brianp
 * don't scale Z to integer values here
 * introduced new triangle, polygon area code but not used yet
 *
 * Revision 1.22  1995/06/02  18:59:31  brianp
 * added special case for computing plane equation of 3-sided polygon
 *
 * Revision 1.21  1995/06/02  13:55:09  brianp
 * implemented vertex/primitive buffering
 *
 * Revision 1.20  1995/05/26  19:32:02  brianp
 * replace many assignments in gl_color() with macros
 *
 * Revision 1.19  1995/05/22  21:02:41  brianp
 * Release 1.2
 *
 * Revision 1.18  1995/05/22  21:00:36  brianp
 * tried initial vertex buffering
 *
 * Revision 1.17  1995/05/17  13:17:22  brianp
 * changed default CC.Mode value to allow use of real OpenGL headers
 * removed need for CC.MajorMode variable
 *
 * Revision 1.16  1995/05/15  16:07:33  brianp
 * moved StippleCounter init to new place
 *
 * Revision 1.15  1995/05/12  16:29:18  brianp
 * added #define for NULL
 *
 * Revision 1.14  1995/03/27  20:31:26  brianp
 * new Texture.Enabled scheme
 *
 * Revision 1.13  1995/03/24  15:31:23  brianp
 * introduced VB
 *
 * Revision 1.12  1995/03/23  17:10:12  brianp
 * changed render_point() to render_points(n)
 *
 * Revision 1.11  1995/03/09  21:42:09  brianp
 * new ModelViewInv matrix logic
 *
 * Revision 1.10  1995/03/09  20:08:04  brianp
 * introduced TRANSFORM_POINT and TRANSFORM_NORMAL macros
 *
 * Revision 1.9  1995/03/04  19:29:44  brianp
 * 1.1 beta revision
 *
 * Revision 1.8  1995/03/02  19:10:00  brianp
 * fixed a texgen bug
 *
 * Revision 1.7  1995/02/27  22:48:48  brianp
 * modified for PB
 *
 * Revision 1.6  1995/02/27  15:08:04  brianp
 * added Vcolor/Vindex scheme
 *
 * Revision 1.5  1995/02/26  22:58:43  brianp
 * more zero-area polygon work
 *
 * Revision 1.4  1995/02/26  21:59:43  brianp
 * *** empty log message ***
 *
 * Revision 1.3  1995/02/25  22:07:56  brianp
 * relaxed test for zero-area polygons, still not perfect though
 *
 * Revision 1.2  1995/02/24  15:23:36  brianp
 * removed initialization of d from render_point()
 * added RasterColor code to gl_rasterpos()
 *
 * Revision 1.1  1995/02/24  14:20:43  brianp
 * Initial revision
 *
 */


/*
 * Draw points, lines, and polygons.
 */


#ifdef DEBUG
#  include <assert.h>
#endif
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "clip.h"
#include "context.h"
#include "dd.h"
#include "draw.h"
#include "feedback.h"
#include "fog.h"
#include "light.h"
#include "lines.h"
#include "list.h"
#include "macros.h"
#include "pb.h"
#include "points.h"
#include "polygons.h"
#include "texture.h"
#include "vb.h"
#include "xform.h"

/*#include "triangle.h"*/



#ifndef NULL
#  define NULL 0
#endif


#ifdef DEBUG
#  define ASSERT(X)   assert(X)
#else
#  define ASSERT(X)
#endif



/*
 * Check if the global material has to be updated with info that was
 * associated with a vertex via glMaterial.
 */
static void update_material( GLuint i )
{
   if (VB.MaterialMask[i]) {
      if (VB.MaterialMask[i] & FRONT_AMBIENT_BIT) {
         COPY_4V( CC.Light.Material[0].Ambient, VB.Material[i][0].Ambient );
      }
      if (VB.MaterialMask[i] & BACK_AMBIENT_BIT) {
         COPY_4V( CC.Light.Material[1].Ambient, VB.Material[i][1].Ambient );
      }
      if (VB.MaterialMask[i] & FRONT_DIFFUSE_BIT) {
         COPY_4V( CC.Light.Material[0].Diffuse, VB.Material[i][0].Diffuse );
      }
      if (VB.MaterialMask[i] & BACK_DIFFUSE_BIT) {
         COPY_4V( CC.Light.Material[1].Diffuse, VB.Material[i][1].Diffuse );
      }
      if (VB.MaterialMask[i] & FRONT_SPECULAR_BIT) {
         COPY_4V( CC.Light.Material[0].Specular, VB.Material[i][0].Specular );
      }
      if (VB.MaterialMask[i] & BACK_SPECULAR_BIT) {
         COPY_4V( CC.Light.Material[1].Specular, VB.Material[i][1].Specular );
      }
      if (VB.MaterialMask[i] & FRONT_EMISSION_BIT) {
         COPY_4V( CC.Light.Material[0].Emission, VB.Material[i][0].Emission );
      }
      if (VB.MaterialMask[i] & BACK_EMISSION_BIT) {
         COPY_4V( CC.Light.Material[1].Emission, VB.Material[i][1].Emission );
      }
      if (VB.MaterialMask[i] & FRONT_SHININESS_BIT) {
         CC.Light.Material[0].Shininess = VB.Material[i][0].Shininess;
      }
      if (VB.MaterialMask[i] & BACK_SHININESS_BIT) {
         CC.Light.Material[1].Shininess = VB.Material[i][1].Shininess;
      }
      if (VB.MaterialMask[i] & FRONT_INDEXES_BIT) {
         CC.Light.Material[0].AmbientIndex = VB.Material[i][0].AmbientIndex;
         CC.Light.Material[0].DiffuseIndex = VB.Material[i][0].DiffuseIndex;
         CC.Light.Material[0].SpecularIndex = VB.Material[i][0].SpecularIndex;
      }
      if (VB.MaterialMask[i] & BACK_INDEXES_BIT) {
         CC.Light.Material[1].AmbientIndex = VB.Material[i][1].AmbientIndex;
         CC.Light.Material[1].DiffuseIndex = VB.Material[i][1].DiffuseIndex;
         CC.Light.Material[1].SpecularIndex = VB.Material[i][1].SpecularIndex;
      }
      VB.MaterialMask[i] = 0;  /* reset now */
   }
}



/*
 * Render a line segment from VB[v1] to VB[v2] when either one or both
 * endpoints must be clipped.
 */
static void render_clipped_line( GLuint v1, GLuint v2 )
{
   GLfloat d;
   GLfloat ndc_x, ndc_y, ndc_z;
   GLuint provoking_vertex;

   /* which vertex dictates the color when flat shading: */
   provoking_vertex = v2;

   /*
    * Clipping may introduce new vertices.  New vertices will be stored
    * in the vertex buffer arrays starting with location VB.Free.  After
    * we've rendered the line, these extra vertices can be overwritten.
    */
   VB.Free = VB_MAX;

   /* Clip against user clipping planes */
   if (CC.Transform.AnyClip) {
      if (gl_userclip_line( &v1, &v2 )==0)
	return;
   }

   /* Apply projection matrix:  clip = Proj * eye */
   TRANSFORM_POINT( VB.Clip[v1], CC.ProjectionMatrix, VB.Eye[v1] );
   TRANSFORM_POINT( VB.Clip[v2], CC.ProjectionMatrix, VB.Eye[v2] );

   /* Clip against view volume */
   if (gl_viewclip_line( &v1, &v2 )==0)
      return;

   /* Transform from clip coords to ndc:  ndc = clip / W */
   ASSERT( VB.Clip[v1][3] != 0.0 );
   ASSERT( VB.Clip[v2][3] != 0.0 );
   d = 1.0F / VB.Clip[v1][3];
   ndc_x = VB.Clip[v1][0] * d;
   ndc_y = VB.Clip[v1][1] * d;
   ndc_z = VB.Clip[v1][2] * d;

   /* Map ndc coord to window coords. */
   VB.Win[v1][0] = MAP_X( ndc_x );
   VB.Win[v1][1] = MAP_Y( ndc_y );
   VB.Win[v1][2] = MAP_Z( ndc_z );

   /* Transform from clip coords to ndc:  ndc = clip / W */
   d = 1.0F / VB.Clip[v2][3];
   ndc_x = VB.Clip[v2][0] * d;
   ndc_y = VB.Clip[v2][1] * d;
   ndc_z = VB.Clip[v2][2] * d;

   /* Map ndc coord to window coords. */
   VB.Win[v2][0] = MAP_X( ndc_x );
   VB.Win[v2][1] = MAP_Y( ndc_y );
   VB.Win[v2][2] = MAP_Z( ndc_z );

#ifdef PROFILE
   {
      GLdouble t0 = gl_time();
      (*CC.LineFunc)( v1, v2, provoking_vertex );
      CC.LineTime += gl_time() - t0;
      CC.LineCount++;
   }
#else
   (*CC.LineFunc)( v1, v2, provoking_vertex );
#endif
}



#ifdef LEAVEOUT
/*
 * Apply the glPolygonOffsetEXT transformation to the current plane
 * equation.  Basically, we just have to add an offset to the D term.
 */
static void offset_polygon( void )
{
   GLfloat ac, bc, m, offset;
   ac = CC.PlaneA / CC.PlaneC;
   bc = CC.PlaneB / CC.PlaneC;
   if (ac<0.0F)  ac = -ac;
   if (bc<0.0F)  bc = -bc;
   m = MAX2( ac, bc );
/*
   m = sqrt( ac*ac + bc*bc );
*/
   offset = m * CC.Polygon.OffsetFactor + CC.Polygon.OffsetBias;
   CC.PlaneD = CC.PlaneD + CC.PlaneC * offset;
}
#endif


#define OFFSET_POLYGON( A, B, C, D )					\
	{								\
	   GLfloat m, ac = (A)/(C), bc = (B)/(C);			\
	   if (ac<0.0F)  ac = -ac;					\
	   if (bc<0.0F)  bc = -bc;					\
	   m = MAX2( ac, bc );						\
	   D += (C) * (m * CC.Polygon.OffsetFactor + CC.Polygon.OffsetBias); \
	}




/*
 * Render a polygon in which at least one vertex has to be clipped.
 * Input:  n - number of vertices
 *         vlist - list of vertices in the polygon.
 *         odd_flag - if non-zero, reverse the orientation of the polygon
 */
static void render_clipped_polygon( GLuint n, GLuint vlist[], GLuint odd_flag )
{
   GLuint i, j;
   GLuint provoking_vertex;
   GLuint facing;

   /* which vertex dictates the color when flat shading: */
   provoking_vertex = (CC.Mode==GL_POLYGON) ? vlist[0] : vlist[n-1];

   /*
    * Clipping may introduce new vertices.  New vertices will be stored
    * in the vertex buffer arrays starting with location VB.Free.  After
    * we've rendered the polygon, these extra vertices can be overwritten.
    */
   VB.Free = VB_MAX;

   /* Clip against user clipping planes in eye coord space. */
   if (CC.Transform.AnyClip) {
      n = gl_userclip_polygon( n, vlist );
      if (n<3)
	return;
   }

   /* Transform vertices from eye to clip coordinates:  clip = Proj * eye */
   for (i=0;i<n;i++) {
      j = vlist[i];
      TRANSFORM_POINT( VB.Clip[j], CC.ProjectionMatrix, VB.Eye[j] );
   }

   /* Clip against view volume in clip coord space */
   n = gl_viewclip_polygon( n, vlist );
   if (n<3)
     return;

   /* Transform vertices from clip to ndc:  ndc = clip / W */
   for (i=0;i<n;i++) {
      GLfloat d, ndc_x, ndc_y, ndc_z;
      j = vlist[i];
      ASSERT( VB.Clip[j][3] != 0.0 );
      d = 1.0F / VB.Clip[j][3];
      ndc_x = VB.Clip[j][0] * d;
      ndc_y = VB.Clip[j][1] * d;
      ndc_z = VB.Clip[j][2] * d;

      /* Transform ndc coord to a window coord.  Note that window Z values */
      /* are scaled to integer Z buffer values. */
      VB.Win[j][0] = MAP_X( ndc_x );
      VB.Win[j][1] = MAP_Y( ndc_y );
      VB.Win[j][2] = MAP_Z( ndc_z );

#ifdef NEW
      VB.WinX[j] = (GLint) (MAP_X( ndc_x ) * SUB_PIX_SCALE);
      VB.WinY[j] = (GLint) (MAP_Y( ndc_y ) * SUB_PIX_SCALE);
      VB.WinZ[j] = (GLint) (MAP_Z( ndc_z ) * DEPTH_SCALE);
#endif
   }

   /* Compute the plane equation of polygon: ax + by + cz = d */
   if (CC.ComputePlane) {
      GLuint j0 = vlist[0];
      GLuint j1 = vlist[1];
      GLuint j2 = vlist[2];
      GLuint j3 = vlist[ (n==3) ? 0 : 3 ];
      GLfloat ex = VB.Win[j1][0] - VB.Win[j3][0];
      GLfloat ey = VB.Win[j1][1] - VB.Win[j3][1];
      GLfloat ez = VB.Win[j1][2] - VB.Win[j3][2];
      GLfloat fx = VB.Win[j2][0] - VB.Win[j0][0];
      GLfloat fy = VB.Win[j2][1] - VB.Win[j0][1];
      GLfloat fz = VB.Win[j2][2] - VB.Win[j0][2];
      GLfloat a, b, c, d;
      c = ex*fy-ey*fx;
      if (c==0.0F) {
         /* polygon is perpindicular to view plane, don't draw it */
         return;
      }

      /* compute orientation:  0=front, 1=back */
      facing = (c<0.0F) ^ odd_flag ^ (CC.Polygon.FrontFace==GL_CW);

      if ((facing+1) & CC.Polygon.CullBits) {
	 return;   /* culled */
      }
      a = ey*fz-ez*fy;
      b = ez*fx-ex*fz;
      d = a*VB.Win[j0][0] + b*VB.Win[j0][1] + c*VB.Win[j0][2];
      if (CC.Polygon.OffsetEnabled) {
         OFFSET_POLYGON( a, b, c, d );
      }
      CC.PlaneA = a;
      CC.PlaneB = b;
      CC.PlaneC = c;
      CC.PlaneD = d;
      CC.Orientation = facing;
      if (CC.Light.Model.TwoSide) {
         if (facing==1 && CC.Light.Enabled) {
            /* use back color or index */
            VB.Color = VB.Bcolor;
            VB.Index = VB.Bindex;
         }
         else {
            /* use front color or index */
            VB.Color = VB.Fcolor;
            VB.Index = VB.Findex;
         }
      }
   }
   else {
      CC.PlaneA = 0.0F;
      CC.PlaneB = 0.0F;
      CC.PlaneC = 1.0F;
      CC.PlaneD = 0.0F;
      facing = 0;
   }

#ifdef PROFILE
   {
      GLdouble t0 = gl_time();
      (*CC.PolygonFunc)( n, vlist, provoking_vertex );
      CC.PolygonTime += gl_time() - t0;
      CC.PolygonCount++;
   }
#else
   (*CC.PolygonFunc)( n, vlist, provoking_vertex );
#endif
}



/*
 * Render a polygon in which doesn't have to be clipped.
 * Input:  n - number of vertices
 *         vlist - list of vertices in the polygon.
 *         odd_flag - if non-zero, reverse the orientation of the polygon
 */
static void render_polygon( GLuint n, GLuint vlist[], GLuint odd_flag )
{
   GLuint provoking_vertex;
   GLuint facing;

   /* which vertex dictates the color when flat shading: */
   provoking_vertex = (CC.Mode==GL_POLYGON) ? vlist[0] : vlist[n-1];

   /* Compute the plane equation of polygon: ax + by + cz = d */
   if (CC.ComputePlane) {
      GLuint j0 = vlist[0];
      GLuint j1 = vlist[1];
      GLuint j2 = vlist[2];
      GLuint j3 = vlist[ (n==3) ? 0 : 3 ];
      GLfloat ex = VB.Win[j1][0] - VB.Win[j3][0];
      GLfloat ey = VB.Win[j1][1] - VB.Win[j3][1];
      GLfloat ez = VB.Win[j1][2] - VB.Win[j3][2];
      GLfloat fx = VB.Win[j2][0] - VB.Win[j0][0];
      GLfloat fy = VB.Win[j2][1] - VB.Win[j0][1];
      GLfloat fz = VB.Win[j2][2] - VB.Win[j0][2];
      GLfloat a, b, c, d;
      c = ex*fy-ey*fx;
      if (c==0.0F) {
         /* polygon is perpindicular to view plane, don't draw it */
         return;
      }

      /* compute orientation:  0=front, 1=back */
      facing = (c<0.0F) ^ odd_flag ^ (CC.Polygon.FrontFace==GL_CW);

      if ((facing+1) & CC.Polygon.CullBits) {
         return;   /* culled */
      }
      a = ey*fz-ez*fy;
      b = ez*fx-ex*fz;
      d = a*VB.Win[j0][0] + b*VB.Win[j0][1] + c*VB.Win[j0][2];
      if (CC.Polygon.OffsetEnabled) {
         OFFSET_POLYGON( a, b, c, d );
      }
      CC.PlaneA = a;
      CC.PlaneB = b;
      CC.PlaneC = c;
      CC.PlaneD = d;
      CC.Orientation = facing;
      if (CC.Light.Model.TwoSide) {
         if (facing==1 && CC.Light.Enabled) {
            /* use back color or index */
            VB.Color = VB.Bcolor;
            VB.Index = VB.Bindex;
         }
         else {
            /* use front color or index */
            VB.Color = VB.Fcolor;
            VB.Index = VB.Findex;
         }
      }
   }
   else {
      CC.PlaneA = 0.0F;
      CC.PlaneB = 0.0F;
      CC.PlaneC = 1.0F;
      CC.PlaneD = 0.0F;
      facing = 0;
   }

#ifdef PROFILE
   {
      GLdouble t0 = gl_time();
      (*CC.PolygonFunc)( n, vlist, provoking_vertex );
      CC.PolygonTime += gl_time() - t0;
      CC.PolygonCount++;
   }
#else
   (*CC.PolygonFunc)( n, vlist, provoking_vertex );
#endif
}




/*
 * Render an un-clipped triangle.
 */
static void render_triangle( GLuint v0, GLuint v1, GLuint v2, GLuint pv,
                             GLuint odd_flag )
{
   GLuint facing;
   GLuint vlist[3];

   /* Compute the plane equation of polygon: ax + by + cz = d */
   if (CC.ComputePlane) {
      GLfloat ex = VB.Win[v1][0] - VB.Win[v0][0];
      GLfloat ey = VB.Win[v1][1] - VB.Win[v0][1];
      GLfloat ez = VB.Win[v1][2] - VB.Win[v0][2];
      GLfloat fx = VB.Win[v2][0] - VB.Win[v0][0];
      GLfloat fy = VB.Win[v2][1] - VB.Win[v0][1];
      GLfloat fz = VB.Win[v2][2] - VB.Win[v0][2];
      GLfloat a, b, c, d;
      c = ex*fy-ey*fx;
      if (c==0.0F) {
	 /* polygon is perpindicular to view plane, don't draw it */
	 return;
      }

      /* compute orientation:  0=front, 1=back */
      facing = (c<0.0F) ^ odd_flag ^ (CC.Polygon.FrontFace==GL_CW);

      if ((facing+1) & CC.Polygon.CullBits) {
	 return;   /* culled */
      }
      a = ey*fz-ez*fy;
      b = ez*fx-ex*fz;
      d = a*VB.Win[v0][0] + b*VB.Win[v0][1] + c*VB.Win[v0][2];
      if (CC.Polygon.OffsetEnabled) {
         OFFSET_POLYGON( a, b, c, d );
      }
      CC.PlaneA = a;
      CC.PlaneB = b;
      CC.PlaneC = c;
      CC.PlaneD = d;
      CC.Orientation = facing;
      if (CC.Light.Model.TwoSide) {
         if (facing==1 && CC.Light.Enabled) {
            /* use back color or index */
            VB.Color = VB.Bcolor;
            VB.Index = VB.Bindex;
         }
         else {
            /* use front color or index */
            VB.Color = VB.Fcolor;
            VB.Index = VB.Findex;
         }
      }
   }
   else {
      CC.PlaneA = 0.0F;
      CC.PlaneB = 0.0F;
      CC.PlaneC = 1.0F;
      CC.PlaneD = 0.0F;
      facing = 0;
   }

   /* TODO: eventually call a special triangle rasterizer */
   vlist[0] = v0;
   vlist[1] = v1;
   vlist[2] = v2;

#ifdef PROFILE
   {
      GLdouble t0 = gl_time();
      (*CC.PolygonFunc)( 3, vlist, pv );
      CC.PolygonTime += gl_time() - t0;
      CC.PolygonCount++;
   }
#else
   (*CC.PolygonFunc)( 3, vlist, pv );
#endif
}




/*
 * Render an un-clipped quadrilateral.
 */
static void render_quad( GLuint v0, GLuint v1, GLuint v2, GLuint v3,
                         GLuint pv, GLuint odd_flag )
{
   GLuint facing;
   GLuint vlist[4];

   /* Compute the plane equation of polygon: ax + by + cz = d */
   if (CC.ComputePlane) {
      GLfloat ex = VB.Win[v2][0] - VB.Win[v0][0];
      GLfloat ey = VB.Win[v2][1] - VB.Win[v0][1];
      GLfloat ez = VB.Win[v2][2] - VB.Win[v0][2];
      GLfloat fx = VB.Win[v3][0] - VB.Win[v1][0];
      GLfloat fy = VB.Win[v3][1] - VB.Win[v1][1];
      GLfloat fz = VB.Win[v3][2] - VB.Win[v1][2];
      GLfloat a, b, c, d;
      c = ex*fy-ey*fx;
      if (c==0.0F) {
	 /* polygon is perpindicular to view plane, don't draw it */
	 return;
      }
      
      /* compute orientation:  0=front, 1=back */
      facing = (c<0.0F) ^ odd_flag ^ (CC.Polygon.FrontFace==GL_CW);

      if ((facing+1) & CC.Polygon.CullBits) {
	 return;   /* culled */
      }
      a = ey*fz-ez*fy;
      b = ez*fx-ex*fz;
      d = a*VB.Win[v0][0] + b*VB.Win[v0][1] + c*VB.Win[v0][2];
      if (CC.Polygon.OffsetEnabled) {
         OFFSET_POLYGON( a, b, c, d );
      }
      CC.PlaneA = a;
      CC.PlaneB = b;
      CC.PlaneC = c;
      CC.PlaneD = d;
      CC.Orientation = facing;
      if (CC.Light.Model.TwoSide) {
         if (facing==1 && CC.Light.Enabled) {
            /* use back color or index */
            VB.Color = VB.Bcolor;
            VB.Index = VB.Bindex;
         }
         else {
            /* use front color or index */
            VB.Color = VB.Fcolor;
            VB.Index = VB.Findex;
         }
      }
   }
   else {
      CC.PlaneA = 0.0F;
      CC.PlaneB = 0.0F;
      CC.PlaneC = 1.0F;
      CC.PlaneD = 0.0F;
      facing = 0;
   }

   /* TODO: eventually call a triangle rasterizer twice */
   vlist[0] = v0;
   vlist[1] = v1;
   vlist[2] = v2;
   vlist[3] = v3;

#ifdef PROFILE
   {
      GLdouble t0 = gl_time();
      (*CC.PolygonFunc)( 4, vlist, pv );
      CC.PolygonTime += gl_time() - t0;
      CC.PolygonCount++;
   }
#else
   (*CC.PolygonFunc)( 4, vlist, pv );
#endif
}



/*
 * When the vertex buffer is full, we transform/render it.  Sometimes we
 * have to copy the last vertex (or two) to the front of the vertex list
 * to "continue" the primitive.  For example:  line or triangle strips.
 * This function is a helper for that.
 */
static void copy_vertex( GLuint dst, GLuint src )
{
   COPY_3V( VB.Win[dst], VB.Win[src] );
   COPY_4V( VB.Eye[dst], VB.Eye[src] );
   COPY_4V( VB.Fcolor[dst], VB.Fcolor[src] );
   COPY_4V( VB.Bcolor[dst], VB.Bcolor[src] );
   VB.Findex[dst] = VB.Findex[src];
   VB.Bindex[dst] = VB.Bindex[src];
   VB.Edgeflag[dst] = VB.Edgeflag[src];
   COPY_4V( VB.TexCoord[dst], VB.TexCoord[src] );
   VB.Unclipped[dst] = VB.Unclipped[src];
}




/*
 * Either the vertex buffer is full (VB.Count==VB_MAX) or glEnd() has been
 * called.  Render the primitives defined by the vertices and reset the
 * buffer.
 * Input:  alldone - GL_TRUE = caller is glEnd()
 *                   GL_FALSE = calling because buffer is full.
 */
static void render_vb( GLboolean alldone )
{
   GLuint vlist[VB_MAX];

   switch (CC.Mode) {
      case GL_POINTS:
#ifdef PROFILE
         {
            GLdouble t0 = gl_time();
            (*CC.PointsFunc)( 0, VB.Count-1 );
            CC.PointTime += gl_time() - t0;
            CC.PointCount += VB.Count;
         }
#else
         (*CC.PointsFunc)( 0, VB.Count-1 );
#endif
	 VB.Count = 0;
         VB.AnyClipped = GL_FALSE;
	 break;

      case GL_LINES:
         if (VB.AnyClipped) {
            GLuint i;
            for (i=1;i<VB.Count;i+=2) {
               if (VB.Unclipped[i-1] & VB.Unclipped[i]) {
#ifdef PROFILE
                  GLdouble t0 = gl_time();
                  (*CC.LineFunc)( i-1, i, i );
                  CC.LineTime += gl_time() - t0;
                  CC.LineCount++;
#else
                  (*CC.LineFunc)( i-1, i, i );
#endif
               }
               else {
                  render_clipped_line( i-1, i );
               }
               CC.StippleCounter = 0;
            }
         }
         else {
            GLuint i;
            for (i=1;i<VB.Count;i+=2) {
#ifdef PROFILE
               GLdouble t0 = gl_time();
               (*CC.LineFunc)( i-1, i, i );
               CC.LineTime += gl_time() - t0;
               CC.LineCount++;
#else
               (*CC.LineFunc)( i-1, i, i );
#endif
               CC.StippleCounter = 0;
            }
         }
	 VB.Count = 0;
         VB.AnyClipped = GL_FALSE;
	 break;

      case GL_LINE_STRIP:
	 {
            GLuint i;
	    for (i=1;i<VB.Count;i++) {
               if (VB.Unclipped[i-1] & VB.Unclipped[i]) {
#ifdef PROFILE
                  GLdouble t0 = gl_time();
                  (*CC.LineFunc)( i-1, i, i );
                  CC.LineTime += gl_time() - t0;
                  CC.LineCount++;
#else
                  (*CC.LineFunc)( i-1, i, i );
#endif
               }
               else {
                  render_clipped_line( i-1, i );
               }
	    }
         }
         if (!alldone) {
            copy_vertex( 0, VB.Count-1 );  /* copy last vertex to front */
            VB.Count = 1;
            VB.AnyClipped = VB.Unclipped[0] ? GL_FALSE : GL_TRUE;
	 }
         break;

      case GL_LINE_LOOP:
         {
            GLuint i;
            if (VB.Start==0) {
               i = 1;  /* start at 0th vertex */
            }
            else {
               i = 2;  /* skip first vertex, we're saving it until glEnd */
            }
            while (i<VB.Count) {
               if (VB.Unclipped[i-1] & VB.Unclipped[i]) {
#ifdef PROFILE
                  GLdouble t0 = gl_time();
                  (*CC.LineFunc)( i-1, i, i );
                  CC.LineTime += gl_time() - t0;
                  CC.LineCount++;
#else
                  (*CC.LineFunc)( i-1, i, i );
#endif
               }
               else {
                  render_clipped_line( i-1, i );
               }
               i++;
            }
         }
	 if (alldone) {
            if (VB.Unclipped[VB.Count-1] & VB.Unclipped[0]) {
#ifdef PROFILE
               GLdouble t0 = gl_time();
               (*CC.LineFunc)( VB.Count-1, 0, 0 );
               CC.LineTime += gl_time() - t0;
               CC.LineCount++;
#else
               (*CC.LineFunc)( VB.Count-1, 0, 0 );
#endif
            }
            else {
               render_clipped_line( VB.Count-1, 0 );
            }
	 }
	 else {
	    ASSERT(VB.Count==VB_MAX);
	    /* recycle the vertex list */
            copy_vertex( 1, VB_MAX-1 );
	    VB.Count = 2;
            VB.AnyClipped = !VB.Unclipped[0] || !VB.Unclipped[1];
	 }
         break;

      case GL_TRIANGLES:
         {
            GLuint i;
            for (i=2;i<VB.Count;i+=3) {
               if (VB.Unclipped[i-2] & VB.Unclipped[i-1] & VB.Unclipped[i]) {
                  render_triangle( i-2, i-1, i, i, 0 );
               }
               else {
                  vlist[0] = i-2;
                  vlist[1] = i-1;
                  vlist[2] = i-0;
                  render_clipped_polygon( 3, vlist, 0 );
               }
            }
         }
	 VB.Count = 0;
         VB.AnyClipped = GL_FALSE;
	 break;

      case GL_TRIANGLE_STRIP:
         if (VB.AnyClipped) {
            GLuint i;
            for (i=2;i<VB.Count;i++) {
               if (VB.Unclipped[i-2] & VB.Unclipped[i-1] & VB.Unclipped[i]) {
                  render_triangle( i-2, i-1, i, i, i&1 );
               }
               else {
                  vlist[0] = i-2;
                  vlist[1] = i-1;
                  vlist[2] = i-0;
                  render_clipped_polygon( 3, vlist, i&1 );
               }
            }
         }
         else {
            /* no vertices were clipped */
            GLuint i;
            for (i=2;i<VB.Count;i++) {
               /*(*CC.TriangleFunc)( i-2, i-1, i, i );*/
               render_triangle( i-2, i-1, i, i, i&1 );
            }
         }
         if (!alldone) {
            /* get ready for more vertices in this triangle strip */
            copy_vertex( 0, VB_MAX-2 );
            copy_vertex( 1, VB_MAX-1 );
            VB.Count = 2;
            VB.AnyClipped = !VB.Unclipped[0] || !VB.Unclipped[1];
         }
	 break;

      case GL_TRIANGLE_FAN:
         {
            GLuint i;
            for (i=2;i<VB.Count;i++) {
               if (VB.Unclipped[0] & VB.Unclipped[i-1] & VB.Unclipped[i]) {
                  render_triangle( 0, i-1, i, i, 0 );
               }
               else {
                  vlist[0] = 0;
                  vlist[1] = i-1;
                  vlist[2] = i;
                  render_clipped_polygon( 3, vlist, 0 );
               }
            }
         }
         if (!alldone) {
            /* get ready for more vertices in this triangle fan */
            copy_vertex( 1, VB_MAX-1 );
            VB.Count = 2;
            VB.AnyClipped = !VB.Unclipped[0] || !VB.Unclipped[1];
	 }
	 break;

      case GL_QUADS:
         if (VB.AnyClipped) {
            GLuint i;
            for (i=3;i<VB.Count;i+=4) {
               if (  VB.Unclipped[i-3] & VB.Unclipped[i-2]
                   & VB.Unclipped[i-1] & VB.Unclipped[i]) {
                  render_quad( i-3, i-2, i-1, i, i, 0 );
               }
               else {
                  vlist[0] = i-3;
                  vlist[1] = i-2;
                  vlist[2] = i-1;
                  vlist[3] = i-0;
                  render_clipped_polygon( 4, vlist, 0 );
               }
            }
         }
         else {
            /* no vertices were clipped */
            GLuint i;
            for (i=3;i<VB.Count;i+=4) {
               render_quad( i-3, i-2, i-1, i, i, 0 );
            }
         }
	 VB.Count = 0;
         VB.AnyClipped = GL_FALSE;
	 break;

      case GL_QUAD_STRIP:
         {
            GLuint i;
            for (i=3;i<VB.Count;i+=2) {
               if (  VB.Unclipped[i-2] & VB.Unclipped[i-3]
                   & VB.Unclipped[i-1] & VB.Unclipped[i]) {
                  render_quad( i-2, i-3, i-1, i, i, 1 );
               }
               else {
                  vlist[0] = i-2;
                  vlist[1] = i-3;
                  vlist[2] = i-1;
                  vlist[3] = i-0;
                  render_clipped_polygon( 4, vlist, 1 );
               }
            }
         }
         if (!alldone) {
            /* get ready for more vertices in this quad strip */
            copy_vertex( 0, VB_MAX-2 );
            copy_vertex( 1, VB_MAX-1 );
            VB.Count = 2;
            VB.AnyClipped = !VB.Unclipped[0] || !VB.Unclipped[1];
         }
	 break;

      case GL_POLYGON:
         if (VB.Count>2) {
            GLuint i;
            for (i=0;i<VB.Count;i++) {
               vlist[i] = i;
            }
            if (VB.AnyClipped) {
               render_clipped_polygon( VB.Count, vlist, 0 );
            }
            else {
               render_polygon( VB.Count, vlist, 0 );
            }
         }
	 if (!alldone) {
            /* get ready for more vertices just like a triangle fan */
            copy_vertex( 1, VB_MAX-1 );
            VB.Count = 2;
            VB.AnyClipped = !VB.Unclipped[0] || !VB.Unclipped[1];
	 }
	 break;

      default:
         /* should never get here */
	 abort();
   }

   /* Start = first vertex which hasn't been transformed yet */
   VB.Start = VB.Count;
}



/*
 * Part 2 of Vertex Buffer transformation:  compute lighting, clipflags,
 * fog, texture coords, etc.  Then call render_vb()...
 * The function is called either by xform_vb_part1() or by glDrawArraysEXT().
 */
void gl_transform_vb_part2( GLboolean alldone )
{
#ifdef PROFILE
   GLdouble t0 = gl_time();
#endif

   ASSERT( VB.Count>0 );

   /* Lighting */
   if (CC.Light.Enabled) {
      if (CC.RGBAflag) {
         if (VB.MaterialChanges) {
            GLuint i;
            /* NOTE the <= here.  This is needed in case glColor/glMaterial
             * is called after the last glVertex inside a glBegin/glEnd pair.
             */
	    for (i=VB.Start;i<=VB.Count;i++) {
               update_material( i );
	       gl_color_shade_vertices( 1, &VB.Eye[i], &VB.Normal[i],
                                        CC.LightTwoSide,
                                        &VB.Fcolor[i], &VB.Bcolor[i] );
	    }
	 }
         else {
            if (CC.Light.Fast) {
               /* call optimized shader */
               gl_color_shade_vertices_fast( VB.Count-VB.Start,
                                             VB.Eye + VB.Start,
                                             VB.Normal + VB.Start,
                                             CC.LightTwoSide,
                                             VB.Fcolor + VB.Start,
                                             VB.Bcolor + VB.Start );
            }
            else {
               /* call full-featured shader */
               gl_color_shade_vertices( VB.Count-VB.Start,
                                        VB.Eye + VB.Start,
                                        VB.Normal + VB.Start,
                                        CC.LightTwoSide,
                                        VB.Fcolor + VB.Start,
                                        VB.Bcolor + VB.Start );
            }
	 }
      }
      else {
         if (VB.MaterialChanges) {
            GLuint i;
            /* NOTE the <= here.  This is needed in case glColor/glMaterial
             * is called after the last glVertex inside a glBegin/glEnd pair.
             */
            for (i=VB.Start;i<=VB.Count;i++) {
               update_material( i );
               gl_index_shade_vertices( 1, &VB.Eye[i], &VB.Normal[i],
                                        CC.LightTwoSide,
                                        &VB.Findex[i], &VB.Bindex[i] );
            }
         }
         else {
            gl_index_shade_vertices( VB.Count-VB.Start,
                                     VB.Eye + VB.Start, VB.Normal + VB.Start,
                                     CC.LightTwoSide,
                                     VB.Findex + VB.Start, VB.Bindex + VB.Start );
         }
      }
   }

   /* Per-vertex fog */
   if (CC.Fog.Enabled && CC.Hint.Fog!=GL_NICEST) {
      if (CC.RGBAflag) {
         /* Fog RGB colors */
         gl_fog_color_vertices( VB.Count - VB.Start,
                                VB.Eye + VB.Start,
                                VB.Fcolor + VB.Start );
         if (CC.LightTwoSide) {
            gl_fog_color_vertices( VB.Count - VB.Start,
                                   VB.Eye + VB.Start,
                                   VB.Bcolor + VB.Start );
         }
      }
      else {
         /* Fog color indexes */
         gl_fog_index_vertices( VB.Count - VB.Start,
                                VB.Eye + VB.Start,
                                VB.Findex + VB.Start );
         if (CC.LightTwoSide) {
            gl_fog_index_vertices( VB.Count - VB.Start,
                                   VB.Eye + VB.Start,
                                   VB.Bindex + VB.Start );
         }
      }
   }

   /* Compute/transform texture coords */
   if (CC.Texture.Enabled) {
      GLuint i;
      for (i=VB.Start;i<VB.Count;i++) {
         if (CC.Texture.TexGenEnabled) {
            gl_do_texgen( VB.Obj[i], VB.Eye[i], VB.Normal[i], VB.TexCoord[i] );
         }
         if (!CC.IdentityTexMat) {
            /* transform current texture coordinate by texture matrix */
            /* tc = TexMat * TexCoord */
            GLfloat tc[4];
            TRANSFORM_POINT( tc, CC.TextureMatrix, VB.TexCoord[i] );
            COPY_4V( VB.TexCoord[i], tc );
         }
      }
   }

   /* Initialize clip flags */
   MEMSET( VB.Unclipped+VB.Start, GL_TRUE, VB.Count-VB.Start );

   if (CC.Transform.AnyClip) {
      /* Clip against user-defined clip planes */
      GLuint p;
      for (p=0;p<MAX_CLIP_PLANES;p++) {
         if (CC.Transform.ClipEnabled[p]) {
            GLuint i;
            GLfloat a = CC.Transform.ClipEquation[p][0];
            GLfloat b = CC.Transform.ClipEquation[p][1];
            GLfloat c = CC.Transform.ClipEquation[p][2];
            GLfloat d = CC.Transform.ClipEquation[p][3];
            for (i=VB.Start;i<VB.Count;i++) {
               GLfloat dot = VB.Eye[i][0] * a + VB.Eye[i][1] * b
                           + VB.Eye[i][2] * c + VB.Eye[i][3] * d;
               if (dot < 0.0F) {
                  VB.Unclipped[i] = GL_FALSE;
                  VB.AnyClipped = GL_TRUE;
               }
            }
         }
      }
   }

   /* Transform vertices from eye to clip coords */
   /* Even transform clipped vertices because it's usually faster. */
   gl_xform_points_4fv( VB.Count-VB.Start, VB.Clip+VB.Start,
                        CC.ProjectionMatrix, VB.Eye+VB.Start );

   /* Clip vertices against view volume */
   {
      GLuint i = VB.Start, n = VB.Count;
      while (i!=n) {
         /* Unclipped[i] will remain TRUE if vertex is inside view volume */
         if (VB.Clip[i][0] > VB.Clip[i][3] || VB.Clip[i][0] < -VB.Clip[i][3] ||
             VB.Clip[i][1] > VB.Clip[i][3] || VB.Clip[i][1] < -VB.Clip[i][3] ||
             VB.Clip[i][2] > VB.Clip[i][3] || VB.Clip[i][2] < -VB.Clip[i][3] ){
            VB.Unclipped[i] = GL_FALSE;
            VB.AnyClipped = GL_TRUE;
         }
         i++;
      }
   }
      
   /* Map vertices from clip to window coords */
   if (VB.AnyClipped) {
      GLuint i;
      GLfloat sx = CC.Viewport.Sx,  tx = CC.Viewport.Tx + CC.RasterOffsetX;
      GLfloat sy = CC.Viewport.Sy,  ty = CC.Viewport.Ty + CC.RasterOffsetY;
      GLfloat sz = CC.Viewport.Sz,  tz = CC.Viewport.Tz;
      for (i=VB.Start;i<VB.Count;i++) {
         if (VB.Unclipped[i]) {
            GLfloat d;
            ASSERT( VB.Clip[i][3] != 0.0 );
            d = 1.0F / VB.Clip[i][3];
            VB.Win[i][0] = VB.Clip[i][0] * d * sx + tx;
            VB.Win[i][1] = VB.Clip[i][1] * d * sy + ty;
            VB.Win[i][2] = VB.Clip[i][2] * d * sz + tz;
         }
      }
   }
   else {
      /* This is written to unroll with IRIX 5.3 cc */
      GLuint i = VB.Start, n = VB.Count;
      GLfloat sx = CC.Viewport.Sx,  tx = CC.Viewport.Tx + CC.RasterOffsetX;
      GLfloat sy = CC.Viewport.Sy,  ty = CC.Viewport.Ty + CC.RasterOffsetY;
      GLfloat sz = CC.Viewport.Sz,  tz = CC.Viewport.Tz;
      while (i!=n) {
         GLfloat d = 1.0F / VB.Clip[i][3];
         ASSERT( VB.Clip[i][3] != 0.0 );
         VB.Win[i][0] = VB.Clip[i][0] * d * sx + tx;
         VB.Win[i][1] = VB.Clip[i][1] * d * sy + ty;
         VB.Win[i][2] = VB.Clip[i][2] * d * sz + tz;
         i++;
      }
   }

#ifdef PROFILE
   CC.VertexTime += gl_time() - t0;
   CC.VertexCount += VB.Count - VB.Start;
#endif

   /* Render the primitives */
   render_vb( alldone );
}



/*
 * When the Vertex Buffer is full, this function transforms all the
 * vertices and normals then calls xform_vb_part2()...
 */
static void transform_vb_part1( GLboolean alldone )
{
#ifdef PROFILE
   GLdouble t0 = gl_time();
#endif

   ASSERT( VB.Count>0 );


   /* Transform vertexes from object to eye coords */
   gl_xform_points_4fv( VB.Count-VB.Start, VB.Eye+VB.Start,
                        CC.ModelViewMatrix, VB.Obj+VB.Start );

   /* Transform normals from object to eye coords */
   if (CC.NeedNormals) {
      gl_xform_normals_3fv( VB.Count-VB.Start,
                            VB.Normal+VB.Start, CC.ModelViewInv,
                            VB.Normal+VB.Start, CC.Transform.Normalize );
   }

#ifdef PROFILE
   CC.VertexTime += gl_time() - t0;
#endif

   gl_transform_vb_part2( alldone );
}



/*
 * Copy the vertex and associated data into the VB.  When the VB is
 * full, render the primitives.
 */
void gl_execute_vertex( GLfloat x, GLfloat y, GLfloat z, GLfloat w )
{
   GLuint count = VB.Count;   /* copy to local var to encourage optimization */

   ASSIGN_4V( VB.Obj[count], x, y, z, w );
   if (CC.RGBAflag) {
      if (CC.Light.Enabled) {
         /* need normal vector, vertex color is computed from material */
         COPY_3V( VB.Normal[count], CC.Current.Normal );
      }
      else {
         /* not lighting, need vertex color */
         GLint shift = CC.ColorShift;
         VB.Fcolor[count][0] = CC.Current.IntColor[0] << shift;
         VB.Fcolor[count][1] = CC.Current.IntColor[1] << shift;
         VB.Fcolor[count][2] = CC.Current.IntColor[2] << shift;
         VB.Fcolor[count][3] = CC.Current.IntColor[3] << shift;
      }
      if (CC.Texture.Enabled) {
         COPY_4V( VB.TexCoord[count], CC.Current.TexCoord );
      }
   }
   else {
      if (CC.Light.Enabled) {
         /* need normal vector, vertex color index computed from material*/
         COPY_3V( VB.Normal[count], CC.Current.Normal );
      }
      else {
         /* not lighting, new vertex color index */
         VB.Findex[count] = CC.Current.Index;
      }
   }
   VB.Edgeflag[count] = CC.Current.EdgeFlag;

   count++;
   VB.Count = count;
   if (count==VB_MAX) {
      transform_vb_part1( GL_FALSE );
   }
}


/*
 * Save a glVertex call into a display list AND execute it.
 */
void gl_save_and_execute_vertex( GLfloat x, GLfloat y, GLfloat z, GLfloat w )
{
   gl_save_vertex( x, y, z, w );
   gl_execute_vertex( x, y, z, w );
}




/*
 * Process a vertex produced by an evaluator.
 * Input:  vertex - the X,Y,Z,W vertex
 *         normal - normal vector
 *         color - 4 integer color components
 *         index - color index
 *         texcoord - texture coordinate
 */
void gl_eval_vertex( const GLfloat vertex[4], const GLfloat normal[3],
		     const GLint color[4], GLuint index,
                     const GLfloat texcoord[4] )
{
   GLuint count = VB.Count;   /* copy to local var to encourage optimization */
   GLint shift = CC.ColorShift;

   COPY_4V( VB.Obj[count], vertex );
   COPY_3V( VB.Normal[count], normal );
   VB.Fcolor[count][0] = color[0] << shift;
   VB.Fcolor[count][1] = color[1] << shift;
   VB.Fcolor[count][2] = color[2] << shift;
   VB.Fcolor[count][3] = color[3] << shift;
   VB.Findex[count] = index;
   COPY_4V( VB.TexCoord[count], texcoord );
   VB.Edgeflag[count] = CC.Current.EdgeFlag;

   count++;
   VB.Count = count;
   if (count==VB_MAX) {
      transform_vb_part1( GL_FALSE );
   }
}




void gl_rasterpos( const GLfloat v[4] )
{
   GLfloat eye[4], clip[4], ndc[3], d;

   /* transform v to eye coords:  eye = ModelView * v */
   TRANSFORM_POINT( eye, CC.ModelViewMatrix, v );

   /* compute lighting */
   if (CC.Light.Enabled) {
      GLfloat eyenorm[3];
      if (!CC.ModelViewInvValid) {
	 gl_compute_modelview_inverse();
      }
      TRANSFORM_NORMAL( eyenorm[0], eyenorm[1], eyenorm[2], CC.Current.Normal,
                        CC.ModelViewInv );
      CC.ColorShift = 0;  /* Colors only shifted when smooth shading */
      if (CC.RGBAflag) {
         GLfixed color[4];  /* not really fixed point but integers */
         GLfixed bcolor[4]; /* not used, dummy arg */
         gl_color_shade_vertices( 1, &eye, &eyenorm, 0, &color, &bcolor );
         CC.Current.RasterColor[0] = (GLfloat) color[0] / CC.RedScale;
         CC.Current.RasterColor[1] = (GLfloat) color[1] / CC.GreenScale;
         CC.Current.RasterColor[2] = (GLfloat) color[2] / CC.BlueScale;
         CC.Current.RasterColor[3] = (GLfloat) color[3] / CC.AlphaScale;
      }
      else {
         GLuint dummy;
	 gl_index_shade_vertices( 1, &eye, &eyenorm, 0,
                                  &CC.Current.RasterIndex, &dummy );
      }
   }
   else {
      /* copy current color or index */
      if (CC.RGBAflag) {
         CC.Current.RasterColor[0] = CC.Current.IntColor[0] / CC.RedScale;
         CC.Current.RasterColor[1] = CC.Current.IntColor[1] / CC.GreenScale;
         CC.Current.RasterColor[2] = CC.Current.IntColor[2] / CC.BlueScale;
         CC.Current.RasterColor[3] = CC.Current.IntColor[3] / CC.AlphaScale;
      }
      else {
	 CC.Current.RasterIndex = CC.Current.Index;
      }
   }

   /* clip to user clipping planes */
   if (gl_userclip_point(eye)==0) {
      CC.Current.RasterPosValid = GL_FALSE;
      return;
   }

   /* compute raster distance */
   CC.Current.RasterDistance = (GLfloat)
                      sqrt( eye[0]*eye[0] + eye[1]*eye[1] + eye[2]*eye[2] );

   /* apply projection matrix:  clip = Proj * eye */
   TRANSFORM_POINT( clip, CC.ProjectionMatrix, eye );

   /* clip to view volume */
   if (gl_viewclip_point( clip )==0) {
      CC.Current.RasterPosValid = GL_FALSE;
      return;
   }

   /* ndc = clip / W */
   ASSERT( clip[3]!=0.0 );
   d = 1.0F / clip[3];
   ndc[0] = clip[0] * d;
   ndc[1] = clip[1] * d;
   ndc[2] = clip[2] * d;

   CC.Current.RasterPos[0] = ndc[0] * CC.Viewport.Sx + CC.Viewport.Tx;
   CC.Current.RasterPos[1] = ndc[1] * CC.Viewport.Sy + CC.Viewport.Ty;
   CC.Current.RasterPos[2] = ndc[2] * CC.Viewport.Sz + CC.Viewport.Tz;
   CC.Current.RasterPos[3] = clip[3];
   CC.Current.RasterPosValid = GL_TRUE;

   /* FOG??? */

   if (CC.Texture.Enabled) {
      /* TODO: Current.RasterTexCoord */

   }

   if (CC.RenderMode==GL_SELECT) {
      /* TODO: is this correct? */
      CC.HitFlag = GL_TRUE;
      if (CC.Current.RasterPos[2] < CC.HitMinZ) {
	 CC.HitMinZ = CC.Current.RasterPos[2];
      }
      if (CC.Current.RasterPos[2] < CC.HitMaxZ) {
	 CC.HitMaxZ = CC.Current.RasterPos[2];
      }
   }

}



#ifdef PROFILE
static GLdouble begin_time;
#endif


void gl_begin( GLenum p )
{
#ifdef PROFILE
   begin_time = gl_time();
#endif

   if (!CC.ModelViewInvValid) {
      gl_compute_modelview_inverse();
   }
   if (CC.NewState) {
      gl_update_state();
   }

   CC.Mode = p;
   VB.Start = VB.Count = 0;
   VB.AnyClipped = GL_FALSE;

   VB.MonoColor = CC.MonoPixels;
   if (VB.MonoColor) {
      /* All pixels generated are likely to be the same color so have
       * the device driver set the "monocolor" now.
       */
      if (CC.RGBAflag) {
         GLubyte r = CC.Current.IntColor[0];
         GLubyte g = CC.Current.IntColor[1];
         GLubyte b = CC.Current.IntColor[2];
         GLubyte a = CC.Current.IntColor[3];
         (*DD.color)( r, g, b, a );
      }
      else {
         (*DD.index)(CC.Current.Index);
      }
   }

   /*
    * If flat shading, save integer vertex colors
    * else, save fixed-point (scaled) vertex colors
    */
   CC.ColorShift = (CC.Light.ShadeModel==GL_FLAT) ? 0 : FIXED_SHIFT;

   /* By default use front color/index.  Two-sided lighting may override. */
   VB.Color = VB.Fcolor;
   VB.Index = VB.Findex;

   switch (CC.Mode) {
      case GL_POINTS:
	 CC.LightTwoSide = 0;
	 PB_INIT( GL_POINT );
         CC.RasterOffsetX = CC.RasterOffsetY = 0.5F;
	 break;
      case GL_LINES:
      case GL_LINE_STRIP:
      case GL_LINE_LOOP:
	 CC.LightTwoSide = 0;
	 CC.StippleCounter = 0;
	 PB_INIT( GL_LINE );
         CC.RasterOffsetX = CC.RasterOffsetY = 0.5F;
         break;
      case GL_TRIANGLES:
      case GL_TRIANGLE_STRIP:
      case GL_TRIANGLE_FAN:
      case GL_QUADS:
      case GL_QUAD_STRIP:
      case GL_POLYGON:
	 CC.LightTwoSide = (GLuint) CC.Light.Model.TwoSide;
	 PB_INIT( GL_POLYGON );
         CC.RasterOffsetX = CC.RasterOffsetY = -0.5F;
         break;
      default:
	 gl_error( GL_INVALID_ENUM, "glBegin" );
	 CC.Mode = GL_BITMAP;
   }
}



void gl_end( void )
{
   if (CC.Mode==GL_BITMAP) {
      /* glEnd without glBegin */
      gl_error( GL_INVALID_OPERATION, "glEnd" );
      return;
   }

   if (VB.Count>VB.Start) {
      transform_vb_part1( GL_TRUE );
   }
   if (PB.count>0) {
      gl_flush_pb();
   }
   PB.primitive = CC.Mode = GL_BITMAP;  /* Default mode */
   VB.MaterialChanges = GL_FALSE;

#ifdef PROFILE
   CC.BeginEndTime += gl_time() - begin_time;
   CC.BeginEndCount++;
#endif
}




/*
 *
 * Public functions
 *
 */

void glBegin( GLenum p )
{
   if (CC.CompileFlag) {
      gl_save_begin( p );
   }
   if (CC.ExecuteFlag) {
      gl_begin( p );
   }
}



void glEnd( void )
{
   if (CC.CompileFlag) {
      gl_save_end();
   }
   if (CC.ExecuteFlag) {
      gl_end();
   }
}
