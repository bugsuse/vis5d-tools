/* $Id: glu.c,v 1.9 1997/12/09 03:03:32 brianp Exp $ */

/*
 * Mesa 3-D graphics library
 * Version:  2.6
 * Copyright (C) 1995-1996  Brian Paul
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
 * $Log: glu.c,v $
 * Revision 1.9  1997/12/09 03:03:32  brianp
 * changed version to 2.6
 *
 * Revision 1.8  1997/10/04 01:30:20  brianp
 * changed version to 2.5
 *
 * Revision 1.7  1997/08/13 01:25:21  brianp
 * changed version string to 2.4
 *
 * Revision 1.6  1997/07/24 01:28:44  brianp
 * changed precompiled header symbol from PCH to PC_HEADER
 *
 * Revision 1.5  1997/07/13 22:59:11  brianp
 * added const to viewport parameter of gluPickMatrix()
 *
 * Revision 1.4  1997/05/28 02:29:38  brianp
 * added support for precompiled headers (PCH), inserted APIENTRY keyword
 *
 * Revision 1.3  1997/04/12 16:19:02  brianp
 * changed version to 2.3
 *
 * Revision 1.2  1997/03/11 00:58:34  brianp
 * changed version to 2.2
 *
 * Revision 1.1  1996/09/27 01:19:39  brianp
 * Initial revision
 *
 */


#ifdef PC_HEADER
#include "all.h"
#else
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "gluP.h"
#endif


/*
 * Miscellaneous utility functions
 */


#ifndef M_PI
#define M_PI 3.1415926536
#endif
#define EPS 0.00001




void APIENTRY gluLookAt( GLdouble eyex, GLdouble eyey, GLdouble eyez,
                         GLdouble centerx, GLdouble centery, GLdouble centerz,
                         GLdouble upx, GLdouble upy, GLdouble upz )
{
   GLdouble m[16];
   GLdouble x[3], y[3], z[3];
   GLdouble mag;

   /* Make rotation matrix */

   /* Z vector */
   z[0] = eyex - centerx;
   z[1] = eyey - centery;
   z[2] = eyez - centerz;
   mag = sqrt( z[0]*z[0] + z[1]*z[1] + z[2]*z[2] );
   if (mag) {  /* mpichler, 19950515 */
      z[0] /= mag;
      z[1] /= mag;
      z[2] /= mag;
   }

   /* Y vector */
   y[0] = upx;
   y[1] = upy;
   y[2] = upz;

   /* X vector = Y cross Z */
   x[0] =  y[1]*z[2] - y[2]*z[1];
   x[1] = -y[0]*z[2] + y[2]*z[0];
   x[2] =  y[0]*z[1] - y[1]*z[0];

   /* Recompute Y = Z cross X */
   y[0] =  z[1]*x[2] - z[2]*x[1];
   y[1] = -z[0]*x[2] + z[2]*x[0];
   y[2] =  z[0]*x[1] - z[1]*x[0];

   /* mpichler, 19950515 */
   /* cross product gives area of parallelogram, which is < 1.0 for
    * non-perpendicular unit-length vectors; so normalize x, y here
    */

   mag = sqrt( x[0]*x[0] + x[1]*x[1] + x[2]*x[2] );
   if (mag) {
      x[0] /= mag;
      x[1] /= mag;
      x[2] /= mag;
   }

   mag = sqrt( y[0]*y[0] + y[1]*y[1] + y[2]*y[2] );
   if (mag) {
      y[0] /= mag;
      y[1] /= mag;
      y[2] /= mag;
   }

#define M(row,col)  m[col*4+row]
   M(0,0) = x[0];  M(0,1) = x[1];  M(0,2) = x[2];  M(0,3) = 0.0;
   M(1,0) = y[0];  M(1,1) = y[1];  M(1,2) = y[2];  M(1,3) = 0.0;
   M(2,0) = z[0];  M(2,1) = z[1];  M(2,2) = z[2];  M(2,3) = 0.0;
   M(3,0) = 0.0;   M(3,1) = 0.0;   M(3,2) = 0.0;   M(3,3) = 1.0;
#undef M
   glMultMatrixd( m );

   /* Translate Eye to Origin */
   glTranslated( -eyex, -eyey, -eyez );

}



void APIENTRY gluOrtho2D( GLdouble left, GLdouble right,
                          GLdouble bottom, GLdouble top )
{
   glOrtho( left, right, bottom, top, -1.0, 1.0 );
}



void APIENTRY gluPerspective( GLdouble fovy, GLdouble aspect,
                              GLdouble zNear, GLdouble zFar )
{
   GLdouble xmin, xmax, ymin, ymax;

   ymax = zNear * tan( fovy * M_PI / 360.0 );
   ymin = -ymax;

   xmin = ymin * aspect;
   xmax = ymax * aspect;

   glFrustum( xmin, xmax, ymin, ymax, zNear, zFar );
}



void APIENTRY gluPickMatrix( GLdouble x, GLdouble y,
                             GLdouble width, GLdouble height,
                             const GLint viewport[4] )
{
   GLfloat m[16];
   GLfloat sx, sy;
   GLfloat tx, ty;

   sx = viewport[2] / width;
   sy = viewport[3] / height;
   tx = (viewport[2] + 2.0 * (viewport[0] - x)) / width;
   ty = (viewport[3] + 2.0 * (viewport[1] - y)) / height;

#define M(row,col)  m[col*4+row]
   M(0,0) = sx;   M(0,1) = 0.0;  M(0,2) = 0.0;  M(0,3) = tx;
   M(1,0) = 0.0;  M(1,1) = sy;   M(1,2) = 0.0;  M(1,3) = ty;
   M(2,0) = 0.0;  M(2,1) = 0.0;  M(2,2) = 1.0;  M(2,3) = 0.0;
   M(3,0) = 0.0;  M(3,1) = 0.0;  M(3,2) = 0.0;  M(3,3) = 1.0;
#undef M

   glMultMatrixf( m );
}



const GLubyte* APIENTRY gluErrorString( GLenum errorCode )
{
   static char *tess_error[] = {
      "missing gluEndPolygon",
      "missing gluBeginPolygon",
      "misoriented contour",
      "vertex/edge intersection",
      "misoriented or self-intersecting loops",
      "coincident vertices",
      "colinear vertices",
      "intersecting edges",
      "not coplanar contours"
   };
   static char *nurbs_error[] = {
      "spline order un-supported",
      "too few knots",
      "valid knot range is empty",
      "decreasing knot sequence knot",
      "knot multiplicity greater than order of spline",
      "endcurve() must follow bgncurve()",
      "bgncurve() must precede endcurve()",
      "missing or extra geometric data",
      "can't draw pwlcurves",
      "missing bgncurve()",
      "missing bgnsurface()",
      "endtrim() must precede endsurface()",
      "bgnsurface() must precede endsurface()",
      "curve of improper type passed as trim curve",
      "bgnsurface() must precede bgntrim()",
      "endtrim() must follow bgntrim()",
      "bgntrim() must precede endtrim()",
      "invalid or missing trim curve",
      "bgntrim() must precede pwlcurve()",
      "pwlcurve referenced twice",
      "pwlcurve and nurbscurve mixed",
      "improper usage of trim data type",
      "nurbscurve referenced twice",
      "nurbscurve and pwlcurve mixed",
      "nurbssurface referenced twice",
      "invalid property",
      "endsurface() must follow bgnsurface()",
      "misoriented trim curves",
      "intersecting trim curves",
      "UNUSED",
      "unconnected trim curves",
      "unknown knot error",
      "negative vertex count encountered",
      "negative byte-stride encounteed",
      "unknown type descriptor",
      "null control array or knot vector",
      "duplicate point on pwlcurve"
   };

   /* GL Errors */
   if (errorCode==GL_NO_ERROR) {
      return (GLubyte *) "no error";
   }
   else if (errorCode==GL_INVALID_VALUE) {
      return (GLubyte *) "invalid value";
   }
   else if (errorCode==GL_INVALID_ENUM) {
      return (GLubyte *) "invalid enum";
   }
   else if (errorCode==GL_INVALID_OPERATION) {
      return (GLubyte *) "invalid operation";
   }
   else if (errorCode==GL_STACK_OVERFLOW) {
      return (GLubyte *) "stack overflow";
   }
   else if (errorCode==GL_STACK_UNDERFLOW) {
      return (GLubyte *) "stack underflow";
   }
   else if (errorCode==GL_OUT_OF_MEMORY) {
      return (GLubyte *) "out of memory";
   }
   /* GLU Errors */
   else if (errorCode==GLU_NO_ERROR) {
      return (GLubyte *) "no error";
   }
   else if (errorCode==GLU_INVALID_ENUM) {
      return (GLubyte *) "invalid enum";
   }
   else if (errorCode==GLU_INVALID_VALUE) {
      return (GLubyte *) "invalid value";
   }
   else if (errorCode==GLU_OUT_OF_MEMORY) {
      return (GLubyte *) "out of memory";
   }
   else if (errorCode==GLU_INCOMPATIBLE_GL_VERSION) {
      return (GLubyte *) "incompatible GL version";
   }
   else if (errorCode>=GLU_TESS_ERROR1 && errorCode<=GLU_TESS_ERROR9) {
      return (GLubyte *) tess_error[errorCode-GLU_TESS_ERROR1];
   }
   else if (errorCode>=GLU_NURBS_ERROR1 && errorCode<=GLU_NURBS_ERROR37) {
      return (GLubyte *) nurbs_error[errorCode-GLU_NURBS_ERROR1];
   }
   else {
      return NULL;
   }
}



/*
 * New in GLU 1.1
 */

const GLubyte* APIENTRY gluGetString( GLenum name )
{
   static char *extensions = "";
   static char *version = "1.1 Mesa 2.6";

   switch (name) {
      case GLU_EXTENSIONS:
         return (GLubyte *) extensions;
      case GLU_VERSION:
	 return (GLubyte *) version;
      default:
	 return NULL;
   }
}

