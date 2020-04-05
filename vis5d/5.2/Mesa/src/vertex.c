/* $Id: vertex.c,v 1.13 1996/02/26 15:08:40 brianp Exp $ */

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
$Log: vertex.c,v $
 * Revision 1.13  1996/02/26  15:08:40  brianp
 * new implementation of glColor*() for CC.Current.IntColor
 *
 * Revision 1.12  1996/02/23  22:34:33  brianp
 * changed logic for testing CC.Compile and CC.Execute per Jean-Luc Daems
 *
 * Revision 1.11  1996/02/14  15:40:03  brianp
 * replaced ABS with ABSD
 *
 * Revision 1.10  1996/01/16  17:07:28  brianp
 * fixed color shift bug in glColor[34]ub[v] functions
 *
 * Revision 1.9  1995/12/30  01:02:42  brianp
 * compute CC.Current.IntColor inside all glColor* calls
 *
 * Revision 1.8  1995/12/20  17:31:11  brianp
 * removed gl_color and gl_index calls, now inlined their operations
 *
 * Revision 1.7  1995/12/19  17:07:29  brianp
 * call gl_save_edgeflag instead of gl_save_set_boolean
 *
 * Revision 1.6  1995/10/19  15:49:41  brianp
 * check for argument underflow in glNormal3d[v] functions
 *
 * Revision 1.5  1995/10/04  19:35:37  brianp
 * fixed bugs and optimized glNormal calls
 *
 * Revision 1.4  1995/07/25  16:41:54  brianp
 * made changes for using CC.VertexFunc pointer
 *
 * Revision 1.3  1995/05/22  21:02:41  brianp
 * Release 1.2
 *
 * Revision 1.2  1995/03/04  19:29:44  brianp
 * 1.1 beta revision
 *
 * Revision 1.1  1995/02/24  14:28:31  brianp
 * Initial revision
 *
 */


/*
 * glVertex*, glNormal*, glIndex*, and glColor* functions.
 */



#include "context.h"
#include "draw.h"
#include "light.h"
#include "list.h"
#include "macros.h"
#include "vb.h"



/*
 * Vertex
 */


/*** 2 arguments ***/

void glVertex2d( GLdouble x, GLdouble y )
{
   (*CC.VertexFunc)( (GLfloat) x, (GLfloat) y, 0.0F, 1.0F );
}


void glVertex2f( GLfloat x, GLfloat y )
{
   (*CC.VertexFunc)( x, y, 0.0F, 1.0F );
}


void glVertex2i( GLint x, GLint y )
{
   (*CC.VertexFunc)( (GLfloat) x, (GLfloat) y, 0.0F, 1.0F );
}


void glVertex2s( GLshort x, GLshort y )
{
   (*CC.VertexFunc)( (GLfloat) x, (GLfloat) y, 0.0F, 1.0F );
}


/*** 3 arguments ***/

void glVertex3d( GLdouble x, GLdouble y, GLdouble z )
{
   (*CC.VertexFunc)( (GLfloat) x, (GLfloat) y, (GLfloat) z, 1.0F );
}


void glVertex3f( GLfloat x, GLfloat y, GLfloat z )
{
   (*CC.VertexFunc)( x, y, z, 1.0F );
}


void glVertex3i( GLint x, GLint y, GLint z )
{
   (*CC.VertexFunc)( (GLfloat) x, (GLfloat) y, (GLfloat) z, 1.0F );
}


void glVertex3s( GLshort x, GLshort y, GLshort z )
{
   (*CC.VertexFunc)( (GLfloat) x, (GLfloat) y, (GLfloat) z, 1.0F );
}


/*** 4 arguments ***/

void glVertex4d( GLdouble x, GLdouble y, GLdouble z, GLdouble w )
{
   (*CC.VertexFunc)( (GLfloat) x, (GLfloat) y, (GLfloat) z, (GLfloat) w );
}


void glVertex4f( GLfloat x, GLfloat y, GLfloat z, GLfloat w )
{
   (*CC.VertexFunc)( x, y, z, w );
}


void glVertex4i( GLint x, GLint y, GLint z, GLint w )
{
   (*CC.VertexFunc)( (GLfloat) x, (GLfloat) y, (GLfloat) z, (GLfloat) w );
}


void glVertex4s( GLshort x, GLshort y, GLshort z, GLshort w )
{
   (*CC.VertexFunc)( (GLfloat) x, (GLfloat) y, (GLfloat) z, (GLfloat) w );
}


/*** 2 element vector ***/

void glVertex2dv( const GLdouble *v )
{
   (*CC.VertexFunc)( (GLfloat) v[0], (GLfloat) v[1], 0.0F, 1.0F );
}


void glVertex2fv( const GLfloat *v )
{
   (*CC.VertexFunc)( v[0], v[1], 0.0F, 1.0F );
}


void glVertex2iv( const GLint *v )
{
   (*CC.VertexFunc)( (GLfloat) v[0], (GLfloat) v[1], 0.0F, 1.0F );
}


void glVertex2sv( const GLshort *v )
{
   (*CC.VertexFunc)( (GLfloat) v[0], (GLfloat) v[1], 0.0F, 1.0F );
}



/*** 3 element vector ***/

void glVertex3dv( const GLdouble *v )
{
   (*CC.VertexFunc)( (GLfloat) v[0], (GLfloat) v[1], (GLfloat) v[2], 1.0F );
}


void glVertex3fv( const GLfloat *v )
{
   (*CC.VertexFunc)( v[0], v[1], v[2], 1.0F );
}


void glVertex3iv( const GLint *v )
{
   (*CC.VertexFunc)( (GLfloat) v[0], (GLfloat) v[1], (GLfloat) v[2], 1.0F );
}


void glVertex3sv( const GLshort *v )
{
   (*CC.VertexFunc)( (GLfloat) v[0], (GLfloat) v[1], (GLfloat) v[2], 1.0F );
}


/*** 4 element vector ***/

void glVertex4dv( const GLdouble *v )
{
   (*CC.VertexFunc)( (GLfloat) v[0], (GLfloat) v[1],
		     (GLfloat) v[2], (GLfloat) v[3] );
}


void glVertex4fv( const GLfloat *v )
{
   (*CC.VertexFunc)( v[0], v[1], v[2], v[3] );
}


void glVertex4iv( const GLint *v )
{
   (*CC.VertexFunc)( (GLfloat) v[0], (GLfloat) v[1],
		     (GLfloat) v[2], (GLfloat) v[3] );
}


void glVertex4sv( const GLshort *v )
{
   (*CC.VertexFunc)( (GLfloat) v[0], (GLfloat) v[1],
		     (GLfloat) v[2], (GLfloat) v[3] );
}



/*
 * Normal vectors
 */


/*** 3 arguments ***/

void glNormal3b( GLbyte nx, GLbyte ny, GLbyte nz )
{
   if (CC.CompileFlag) {
      GLfloat x, y, z;
      x = BYTE_TO_FLOAT(nx);
      y = BYTE_TO_FLOAT(ny);
      z = BYTE_TO_FLOAT(nz);
      gl_save_normal3f( x, y, z );
      if (!CC.ExecuteFlag) return;
   }
   CC.Current.Normal[0] = BYTE_TO_FLOAT(nz);
   CC.Current.Normal[1] = BYTE_TO_FLOAT(ny);
   CC.Current.Normal[2] = BYTE_TO_FLOAT(nz);
}


void glNormal3d( GLdouble nx, GLdouble ny, GLdouble nz )
{
   GLfloat fx, fy, fz;
   if (ABSD(nx)<0.00001)   fx = 0.0F;   else  fx = nx;
   if (ABSD(ny)<0.00001)   fy = 0.0F;   else  fy = ny;
   if (ABSD(nz)<0.00001)   fz = 0.0F;   else  fz = nz;
   if (CC.CompileFlag) {
      gl_save_normal3f( fx, fy, fz );
      if (!CC.ExecuteFlag) return;
   }
   CC.Current.Normal[0] = fx;
   CC.Current.Normal[1] = fy;
   CC.Current.Normal[2] = fz;
}


void glNormal3f( GLfloat nx, GLfloat ny, GLfloat nz )
{
   if (CC.CompileFlag) {
      gl_save_normal3f( nx, ny, nz );
      if (!CC.ExecuteFlag) return;
   }
   CC.Current.Normal[0] = nx;
   CC.Current.Normal[1] = ny;
   CC.Current.Normal[2] = nz;
}


void glNormal3i( GLint nx, GLint ny, GLint nz )
{
   if (CC.CompileFlag) {
      GLfloat x, y, z;
      x = INT_TO_FLOAT(nx);
      y = INT_TO_FLOAT(ny);
      z = INT_TO_FLOAT(nz);
      gl_save_normal3f( x, y, z );
      if (!CC.ExecuteFlag) return;
   }
   CC.Current.Normal[0] = INT_TO_FLOAT(nx);
   CC.Current.Normal[1] = INT_TO_FLOAT(ny);
   CC.Current.Normal[2] = INT_TO_FLOAT(nz);
}


void glNormal3s( GLshort nx, GLshort ny, GLshort nz )
{
   if (CC.CompileFlag) {
      GLfloat x, y, z;
      x = SHORT_TO_FLOAT(nx);
      y = SHORT_TO_FLOAT(ny);
      z = SHORT_TO_FLOAT(nz);
      gl_save_normal3f( x, y, z );
      if (!CC.ExecuteFlag) return;
   }
   CC.Current.Normal[0] = SHORT_TO_FLOAT(nx);
   CC.Current.Normal[1] = SHORT_TO_FLOAT(ny);
   CC.Current.Normal[2] = SHORT_TO_FLOAT(nz);
}


/*** vector argument ***/

void glNormal3bv( const GLbyte *v )
{
   if (CC.CompileFlag) {
      GLfloat x, y, z;
      x = BYTE_TO_FLOAT(v[0]);
      y = BYTE_TO_FLOAT(v[1]);
      z = BYTE_TO_FLOAT(v[2]);
      gl_save_normal3f( x, y, z );
      if (!CC.ExecuteFlag) return;
   }
   CC.Current.Normal[0] = BYTE_TO_FLOAT(v[0]);
   CC.Current.Normal[1] = BYTE_TO_FLOAT(v[1]);
   CC.Current.Normal[2] = BYTE_TO_FLOAT(v[2]);
}


void glNormal3dv( const GLdouble *v )
{
   GLfloat fx, fy, fz;
   if (ABSD(v[0])<0.00001)   fx = 0.0F;   else  fx = v[0];
   if (ABSD(v[1])<0.00001)   fy = 0.0F;   else  fy = v[1];
   if (ABSD(v[2])<0.00001)   fz = 0.0F;   else  fz = v[2];
   if (CC.CompileFlag) {
      gl_save_normal3f( fx, fy, fz );
      if (!CC.ExecuteFlag) return;
   }
   CC.Current.Normal[0] = fx;
   CC.Current.Normal[1] = fy;
   CC.Current.Normal[2] = fz;
}


void glNormal3fv( const GLfloat *v )
{
   if (CC.CompileFlag) {
      gl_save_normal3fv( v );
      if (!CC.ExecuteFlag) return;
   }
   CC.Current.Normal[0] = v[0];
   CC.Current.Normal[1] = v[1];
   CC.Current.Normal[2] = v[2];
}


void glNormal3iv( const GLint *v )
{
   if (CC.CompileFlag) {
      GLfloat x, y, z;
      x = INT_TO_FLOAT(v[0]);
      y = INT_TO_FLOAT(v[1]);
      z = INT_TO_FLOAT(v[2]);
      gl_save_normal3f( x, y, z );
      if (!CC.ExecuteFlag) return;
   }
   CC.Current.Normal[0] = INT_TO_FLOAT(v[0]);
   CC.Current.Normal[1] = INT_TO_FLOAT(v[1]);
   CC.Current.Normal[2] = INT_TO_FLOAT(v[2]);
}


void glNormal3sv( const GLshort *v )
{
   if (CC.CompileFlag) {
      GLfloat x, y, z;
      x = SHORT_TO_FLOAT(v[0]);
      y = SHORT_TO_FLOAT(v[1]);
      z = SHORT_TO_FLOAT(v[2]);
      gl_save_normal3f( x, y, z );
      if (!CC.ExecuteFlag) return;
   }
   CC.Current.Normal[0] = SHORT_TO_FLOAT(v[0]);
   CC.Current.Normal[1] = SHORT_TO_FLOAT(v[1]);
   CC.Current.Normal[2] = SHORT_TO_FLOAT(v[2]);
}



/*
 * Color Index
 */


void glIndexd( GLdouble c )
{
   if (CC.CompileFlag) {
      gl_save_index( (GLuint) (GLint) c );
      if (!CC.ExecuteFlag) return;
   }
   CC.Current.Index = (GLuint) (GLint) c;
   VB.MonoColor = GL_FALSE;
}


void glIndexf( GLfloat c )
{
   if (CC.CompileFlag) {
      gl_save_index( (GLuint) (GLint) c );
      if (!CC.ExecuteFlag) return;
   }
   CC.Current.Index = (GLuint) (GLint) c;
   VB.MonoColor = GL_FALSE;
}


void glIndexi( GLint c )
{
   if (CC.CompileFlag) {
      gl_save_index( (GLuint) c );
      if (!CC.ExecuteFlag) return;
   }
   CC.Current.Index = (GLuint) c;
   VB.MonoColor = GL_FALSE;
}


void glIndexs( GLshort c )
{
   if (CC.CompileFlag) {
      gl_save_index( (GLuint) c );
      if (!CC.ExecuteFlag) return;
   }
   CC.Current.Index = (GLuint) c;
   VB.MonoColor = GL_FALSE;
}


void glIndexdv( const GLdouble *c )
{
   if (CC.CompileFlag) {
      gl_save_index( (GLuint) (GLint) *c );
      if (!CC.ExecuteFlag) return;
   }
   CC.Current.Index = (GLuint) (GLint) *c;
   VB.MonoColor = GL_FALSE;
}


void glIndexfv( const GLfloat *c )
{
   if (CC.CompileFlag) {
      gl_save_index( (GLuint) (GLint) *c );
      if (!CC.ExecuteFlag) return;
   }
   CC.Current.Index = (GLuint) (GLint) *c;
   VB.MonoColor = GL_FALSE;
}


void glIndexiv( const GLint *c )
{
   if (CC.CompileFlag) {
      gl_save_index( (GLuint) *c );
      if (!CC.ExecuteFlag) return;
   }
   CC.Current.Index = (GLuint) *c;
   VB.MonoColor = GL_FALSE;
}


void glIndexsv( const GLshort *c )
{
   if (CC.CompileFlag) {
      gl_save_index( (GLuint) *c );
      if (!CC.ExecuteFlag) return;
   }
   CC.Current.Index = (GLuint) *c;
   VB.MonoColor = GL_FALSE;
}



/*
 * Color
 */


void glColor3b( GLbyte red, GLbyte green, GLbyte blue )
{
   glColor3f( BYTE_TO_FLOAT(red), BYTE_TO_FLOAT(green), BYTE_TO_FLOAT(blue) );
}


void glColor3d( GLdouble red, GLdouble green, GLdouble blue )
{
   glColor3f( red, green, blue );
}


void glColor3f( GLfloat red, GLfloat green, GLfloat blue )
{
   if (CC.CompileFlag) {
      GLint c[4];
      c[0] = red * CC.RedScale;
      c[1] = green * CC.GreenScale;
      c[2] = blue * CC.BlueScale;
      c[3] = CC.AlphaScale;
      gl_save_color( c );
      if (!CC.ExecuteFlag) return;
   }
   CC.Current.IntColor[0] = red * CC.RedScale;
   CC.Current.IntColor[1] = green * CC.GreenScale;
   CC.Current.IntColor[2] = blue * CC.BlueScale;
   CC.Current.IntColor[3] = CC.AlphaScale;
   if (CC.Light.ColorMaterialEnabled) {
      /* Translate this glColor() call into a glMaterial() call */
      GLfloat color[4];
      ASSIGN_4V( color, red, green, blue, 1.0F );
      gl_material( CC.Light.ColorMaterialFace,
                   CC.Light.ColorMaterialMode, color );
   }
   VB.MonoColor = GL_FALSE;
}


void glColor3i( GLint red, GLint green, GLint blue )
{
   glColor3f( INT_TO_FLOAT(red), INT_TO_FLOAT(green), INT_TO_FLOAT(blue) );
}


void glColor3s( GLshort red, GLshort green, GLshort blue )
{
   glColor3f( SHORT_TO_FLOAT(red), SHORT_TO_FLOAT(green),
              SHORT_TO_FLOAT(blue) );
}


void glColor3ub( GLubyte red, GLubyte green, GLubyte blue )
{
   if (CC.CompileFlag) {
      GLint c[4];
      if (CC.EightBitColor) {
         ASSIGN_4V( c, red, green, blue, (GLint) CC.AlphaScale );
      }
      else {
         c[0] = red * CC.RedScale * (1.0F/255.0F);
         c[1] = green * CC.GreenScale * (1.0F/255.0F);
         c[2] = blue * CC.BlueScale * (1.0F/255.0F);
         c[3] = CC.AlphaScale;
      }
      gl_save_color( c );
      if (!CC.ExecuteFlag) return;
   }
   if (CC.EightBitColor) {
      ASSIGN_4V( CC.Current.IntColor, red, green, blue, (GLint) CC.AlphaScale);
   }
   else {
      CC.Current.IntColor[0] = red * CC.RedScale * (1.0F/255.0F);
      CC.Current.IntColor[1] = green * CC.GreenScale * (1.0F/255.0F);
      CC.Current.IntColor[2] = blue * CC.BlueScale * (1.0F/255.0F);
      CC.Current.IntColor[3] = CC.AlphaScale;
   }
   if (CC.Light.ColorMaterialEnabled) {
      /* Translate this glColor() call into a glMaterial() call */
      GLfloat color[4];
      color[0] = red * (1.0F/255.0F);
      color[1] = green * (1.0F/255.0F);
      color[2] = blue * (1.0F/255.0F);
      color[3] = 1.0F;
      gl_material( CC.Light.ColorMaterialFace,
                   CC.Light.ColorMaterialMode, color );
   }
   VB.MonoColor = GL_FALSE;
}


void glColor3ui( GLuint red, GLuint green, GLuint blue )
{
   glColor3f( UINT_TO_FLOAT(red), UINT_TO_FLOAT(green), UINT_TO_FLOAT(blue) );
}


void glColor3us( GLushort red, GLushort green, GLushort blue )
{
   glColor3f( USHORT_TO_FLOAT(red), USHORT_TO_FLOAT(green),
              USHORT_TO_FLOAT(blue) );
}


void glColor4b( GLbyte red, GLbyte green, GLbyte blue, GLbyte alpha )
{
   glColor4f( BYTE_TO_FLOAT(red), BYTE_TO_FLOAT(green),
              BYTE_TO_FLOAT(blue), BYTE_TO_FLOAT(alpha) );
}


void glColor4d( GLdouble red, GLdouble green, GLdouble blue, GLdouble alpha )
{
   glColor4f( red, green, blue, alpha );
}


void glColor4f( GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha )
{
   if (CC.CompileFlag) {
      GLint c[4];
      c[0] = red * CC.RedScale;
      c[1] = green * CC.GreenScale;
      c[2] = blue * CC.BlueScale;
      c[3] = alpha * CC.AlphaScale;
      gl_save_color( c );
      if (!CC.ExecuteFlag) return;
   }
   CC.Current.IntColor[0] = red * CC.RedScale;
   CC.Current.IntColor[1] = green * CC.GreenScale;
   CC.Current.IntColor[2] = blue * CC.BlueScale;
   CC.Current.IntColor[3] = alpha * CC.AlphaScale;
   if (CC.Light.ColorMaterialEnabled) {
      /* Translate this glColor() call into a glMaterial() call */
      GLfloat color[4];
      ASSIGN_4V( color, red, green, blue, alpha );
      gl_material( CC.Light.ColorMaterialFace,
                   CC.Light.ColorMaterialMode, color );
   }
   VB.MonoColor = GL_FALSE;
}


void glColor4i( GLint red, GLint green, GLint blue, GLint alpha )
{
   glColor4f( INT_TO_FLOAT(red), INT_TO_FLOAT(green),
              INT_TO_FLOAT(blue), INT_TO_FLOAT(alpha) );
}


void glColor4s( GLshort red, GLshort green, GLshort blue, GLshort alpha )
{
   glColor4f( SHORT_TO_FLOAT(red), SHORT_TO_FLOAT(green),
              SHORT_TO_FLOAT(blue), SHORT_TO_FLOAT(alpha) );
}


void glColor4ub( GLubyte red, GLubyte green, GLubyte blue, GLubyte alpha )
{
   if (CC.CompileFlag) {
      GLint c[4];
      if (CC.EightBitColor) {
         ASSIGN_4V( c, red, green, blue, alpha );
      }
      else {
         c[0] = red * CC.RedScale * (1.0F/255.0F);
         c[1] = green * CC.GreenScale * (1.0F/255.0F);
         c[2] = blue * CC.BlueScale * (1.0F/255.0F);
         c[3] = alpha * CC.AlphaScale * (1.0F/255.0F);
      }
      gl_save_color( c );
      if (!CC.ExecuteFlag) return;
   }
   if (CC.EightBitColor) {
      ASSIGN_4V( CC.Current.IntColor, red, green, blue, alpha );
   }
   else {
      CC.Current.IntColor[0] = red * CC.RedScale * (1.0F/255.0F);
      CC.Current.IntColor[1] = green * CC.GreenScale * (1.0F/255.0F);
      CC.Current.IntColor[2] = blue * CC.BlueScale * (1.0F/255.0F);
      CC.Current.IntColor[3] = alpha * CC.AlphaScale * (1.0F/255.0F);
   }
   if (CC.Light.ColorMaterialEnabled) {
      /* Translate this glColor() call into a glMaterial() call */
      GLfloat color[4];
      color[0] = red * (1.0F/255.0F);
      color[1] = green * (1.0F/255.0F);
      color[2] = blue * (1.0F/255.0F);
      color[3] = alpha * (1.0F/255.0F);
      gl_material( CC.Light.ColorMaterialFace,
                   CC.Light.ColorMaterialMode, color );
   }
   VB.MonoColor = GL_FALSE;
}


void glColor4ui( GLuint red, GLuint green, GLuint blue, GLuint alpha )
{
   glColor4f( UINT_TO_FLOAT(red), UINT_TO_FLOAT(green),
              UINT_TO_FLOAT(blue), UINT_TO_FLOAT(alpha) );
}


void glColor4us( GLushort red, GLushort green, GLushort blue, GLushort alpha )
{
   glColor4f( USHORT_TO_FLOAT(red), USHORT_TO_FLOAT(green),
              USHORT_TO_FLOAT(blue), USHORT_TO_FLOAT(alpha) );
}


void glColor3bv( const GLbyte *v )
{
   glColor3f( BYTE_TO_FLOAT(v[0]), BYTE_TO_FLOAT(v[1]), BYTE_TO_FLOAT(v[2]) );
}


void glColor3dv( const GLdouble *v )
{
   glColor3f( v[0], v[1], v[2] );
}


void glColor3fv( const GLfloat *v )
{
   if (CC.CompileFlag) {
      GLint c[4];
      c[0] = v[0] * CC.RedScale;
      c[1] = v[1] * CC.GreenScale;
      c[2] = v[2] * CC.BlueScale;
      c[3] = CC.AlphaScale;
      gl_save_color( c );
      if (!CC.ExecuteFlag) return;
   }
   CC.Current.IntColor[0] = v[0] * CC.RedScale;
   CC.Current.IntColor[1] = v[1] * CC.GreenScale;
   CC.Current.IntColor[2] = v[2] * CC.BlueScale;
   CC.Current.IntColor[3] = CC.AlphaScale;
   if (CC.Light.ColorMaterialEnabled) {
      /* Translate this glColor() call into a glMaterial() call */
      GLfloat color[4];
      ASSIGN_4V( color, v[0], v[1], v[2], 1.0F );
      gl_material( CC.Light.ColorMaterialFace,
                   CC.Light.ColorMaterialMode, color );
   }
   VB.MonoColor = GL_FALSE;
}


void glColor3iv( const GLint *v )
{
   glColor3f( INT_TO_FLOAT(v[0]), INT_TO_FLOAT(v[1]), INT_TO_FLOAT(v[2]) );
}


void glColor3sv( const GLshort *v )
{
   glColor3f( SHORT_TO_FLOAT(v[0]), SHORT_TO_FLOAT(v[1]),
              SHORT_TO_FLOAT(v[2]) );
}


void glColor3ubv( const GLubyte *v )
{
   if (CC.CompileFlag) {
      GLint c[4];
      if (CC.EightBitColor) {
         ASSIGN_4V( c, v[0], v[1], v[2], (GLint) CC.AlphaScale );
      }
      else {
         c[0] = v[0] * CC.RedScale * (1.0F/255.0F);
         c[1] = v[1] * CC.GreenScale * (1.0F/255.0F);
         c[2] = v[2] * CC.BlueScale * (1.0F/255.0F);
         c[3] = CC.AlphaScale;
      }
      gl_save_color( c );
      if (!CC.ExecuteFlag) return;
   }
   if (CC.EightBitColor) {
      ASSIGN_4V( CC.Current.IntColor, v[0], v[1], v[2], (GLint) CC.AlphaScale);
   }
   else {
      CC.Current.IntColor[0] = v[0] * CC.RedScale * (1.0F/255.0F);
      CC.Current.IntColor[1] = v[1] * CC.GreenScale * (1.0F/255.0F);
      CC.Current.IntColor[2] = v[2] * CC.BlueScale * (1.0F/255.0F);
      CC.Current.IntColor[3] = CC.AlphaScale;
   }
   if (CC.Light.ColorMaterialEnabled) {
      /* Translate this glColor() call into a glMaterial() call */
      GLfloat color[4];
      color[0] = v[0] * (1.0F/255.0F);
      color[1] = v[1] * (1.0F/255.0F);
      color[2] = v[2] * (1.0F/255.0F);
      color[3] = 1.0F;
      gl_material( CC.Light.ColorMaterialFace,
                   CC.Light.ColorMaterialMode, color );
   }
   VB.MonoColor = GL_FALSE;
}


void glColor3uiv( const GLuint *v )
{
   glColor3f( UINT_TO_FLOAT(v[0]), UINT_TO_FLOAT(v[1]), UINT_TO_FLOAT(v[2]) );
}


void glColor3usv( const GLushort *v )
{
   glColor3f( USHORT_TO_FLOAT(v[0]), USHORT_TO_FLOAT(v[1]),
              USHORT_TO_FLOAT(v[2]) );
}


void glColor4bv( const GLbyte *v )
{
   glColor4f( BYTE_TO_FLOAT(v[0]), BYTE_TO_FLOAT(v[1]),
              BYTE_TO_FLOAT(v[2]), BYTE_TO_FLOAT(v[3]) );
}


void glColor4dv( const GLdouble *v )
{
   glColor4f( v[0], v[1], v[2], v[3] );
}


void glColor4fv( const GLfloat *v )
{
   if (CC.CompileFlag) {
      GLint c[4];
      c[0] = v[0] * CC.RedScale;
      c[1] = v[1] * CC.GreenScale;
      c[2] = v[2] * CC.BlueScale;
      c[3] = v[3] * CC.AlphaScale;
      gl_save_color( c );
      if (!CC.ExecuteFlag) return;
   }
   CC.Current.IntColor[0] = v[0] * CC.RedScale;
   CC.Current.IntColor[1] = v[1] * CC.GreenScale;
   CC.Current.IntColor[2] = v[2] * CC.BlueScale;
   CC.Current.IntColor[3] = v[3] * CC.AlphaScale;
   if (CC.Light.ColorMaterialEnabled) {
      /* Translate this glColor() call into a glMaterial() call */
      gl_material( CC.Light.ColorMaterialFace,
                   CC.Light.ColorMaterialMode, v );
   }
   VB.MonoColor = GL_FALSE;
}


void glColor4iv( const GLint *v )
{
   glColor4f( INT_TO_FLOAT(v[0]), INT_TO_FLOAT(v[1]),
              INT_TO_FLOAT(v[2]), INT_TO_FLOAT(v[3]) );
}


void glColor4sv( const GLshort *v )
{
   glColor4f( SHORT_TO_FLOAT(v[0]), SHORT_TO_FLOAT(v[1]),
              SHORT_TO_FLOAT(v[2]), SHORT_TO_FLOAT(v[3]) );
}


void glColor4ubv( const GLubyte *v )
{
   if (CC.CompileFlag) {
      GLint c[4];
      if (CC.EightBitColor) {
         COPY_4V( c, v );
      }
      else {
         c[0] = v[0] * CC.RedScale * (1.0F/255.0F);
         c[1] = v[1] * CC.GreenScale * (1.0F/255.0F);
         c[2] = v[2] * CC.BlueScale * (1.0F/255.0F);
         c[3] = v[3] * CC.AlphaScale * (1.0F/255.0F);
      }
      gl_save_color( c );
      if (!CC.ExecuteFlag) return;
   }
   if (CC.EightBitColor) {
      COPY_4V( CC.Current.IntColor, v );
   }
   else {
      CC.Current.IntColor[0] = v[0] * CC.RedScale * (1.0F/255.0F);
      CC.Current.IntColor[1] = v[1] * CC.GreenScale * (1.0F/255.0F);
      CC.Current.IntColor[2] = v[2] * CC.BlueScale * (1.0F/255.0F);
      CC.Current.IntColor[3] = v[3] * CC.AlphaScale * (1.0F/255.0F);
   }
   if (CC.Light.ColorMaterialEnabled) {
      /* Translate this glColor() call into a glMaterial() call */
      GLfloat color[4];
      color[0] = v[0] * (1.0F/255.0F);
      color[1] = v[1] * (1.0F/255.0F);
      color[2] = v[2] * (1.0F/255.0F);
      color[3] = v[3] * (1.0F/255.0F);
      gl_material( CC.Light.ColorMaterialFace,
                   CC.Light.ColorMaterialMode, color );
   }
   VB.MonoColor = GL_FALSE;
}


void glColor4uiv( const GLuint *v )
{
   glColor4f(  UINT_TO_FLOAT(v[0]), UINT_TO_FLOAT(v[1]),
               UINT_TO_FLOAT(v[2]), UINT_TO_FLOAT(v[3]) );
}


void glColor4usv( const GLushort *v )
{
   glColor4f( USHORT_TO_FLOAT(v[0]), USHORT_TO_FLOAT(v[1]),
              USHORT_TO_FLOAT(v[2]), USHORT_TO_FLOAT(v[3]) );
}



/*
 * glRasterPos* functions
 */


void glRasterPos2d( GLdouble x, GLdouble y )
{
   GLfloat v[4];

   v[0] = (GLfloat) x;
   v[1] = (GLfloat) y;
   v[2] = 0.0F;
   v[3] = 1.0F;

   if (CC.CompileFlag) {
      gl_save_rasterpos( v );
   }
   if (CC.ExecuteFlag) {
      gl_rasterpos( v );
   }
}


void glRasterPos2f( GLfloat x, GLfloat y )
{
   GLfloat v[4];

   v[0] = x;
   v[1] = y;
   v[2] = 0.0F;
   v[3] = 1.0F;

   if (CC.CompileFlag) {
      gl_save_rasterpos( v );
   }
   if (CC.ExecuteFlag) {
      gl_rasterpos( v );
   }
}


void glRasterPos2i( GLint x, GLint y )
{
   GLfloat v[4];

   v[0] = (GLfloat) x;
   v[1] = (GLfloat) y;
   v[2] = 0.0F;
   v[3] = 1.0F;

   if (CC.CompileFlag) {
      gl_save_rasterpos( v );
   }
   if (CC.ExecuteFlag) {
      gl_rasterpos( v );
   }
}


void glRasterPos2s( GLshort x, GLshort y )
{
   GLfloat v[4];

   v[0] = (GLfloat) x;
   v[1] = (GLfloat) y;
   v[2] = 0.0F;
   v[3] = 1.0F;

   if (CC.CompileFlag) {
      gl_save_rasterpos( v );
   }
   if (CC.ExecuteFlag) {
      gl_rasterpos( v );
   }
}


/*** 3 arguments ***/

void glRasterPos3d( GLdouble x, GLdouble y, GLdouble z )
{
   GLfloat v[4];

   v[0] = (GLfloat) x;
   v[1] = (GLfloat) y;
   v[2] = (GLfloat) z;
   v[3] = 1.0F;

   if (CC.CompileFlag) {
      gl_save_rasterpos( v );
   }
   if (CC.ExecuteFlag) {
      gl_rasterpos( v );
   }
}


void glRasterPos3f( GLfloat x, GLfloat y, GLfloat z )
{
   GLfloat v[4];

   v[0] = x;
   v[1] = y;
   v[2] = z;
   v[3] = 1.0F;

   if (CC.CompileFlag) {
      gl_save_rasterpos( v );
   }
   if (CC.ExecuteFlag) {
      gl_rasterpos( v );
   }
}


void glRasterPos3i( GLint x, GLint y, GLint z )
{
   GLfloat v[4];

   v[0] = (GLfloat) x;
   v[1] = (GLfloat) y;
   v[2] = (GLfloat) z;
   v[3] = 1.0F;

   if (CC.CompileFlag) {
      gl_save_rasterpos( v );
   }
   if (CC.ExecuteFlag) {
      gl_rasterpos( v );
   }
}


void glRasterPos3s( GLshort x, GLshort y, GLshort z )
{
   GLfloat v[4];

   v[0] = (GLfloat) x;
   v[1] = (GLfloat) y;
   v[2] = (GLfloat) z;
   v[3] = 1.0F;

   if (CC.CompileFlag) {
      gl_save_rasterpos( v );
   }
   if (CC.ExecuteFlag) {
      gl_rasterpos( v );
   }
}


/*** 4 arguments ***/

void glRasterPos4d( GLdouble x, GLdouble y, GLdouble z, GLdouble w )
{
   GLfloat v[4];

   v[0] = (GLfloat) x;
   v[1] = (GLfloat) y;
   v[2] = (GLfloat) z;
   v[3] = (GLfloat) w;

   if (CC.CompileFlag) {
      gl_save_rasterpos( v );
   }
   if (CC.ExecuteFlag) {
      gl_rasterpos( v );
   }
}


void glRasterPos4f( GLfloat x, GLfloat y, GLfloat z, GLfloat w )
{
   GLfloat v[4];

   /* maybe:  GLfloat *v = &x ??? */
   v[0] = x;
   v[1] = y;
   v[2] = z;
   v[3] = w;

   if (CC.CompileFlag) {
      gl_save_rasterpos( v );
   }
   if (CC.ExecuteFlag) {
      gl_rasterpos( v );
   }
}


void glRasterPos4i( GLint x, GLint y, GLint z, GLint w )
{
   GLfloat v[4];

   v[0] = (GLfloat) x;
   v[1] = (GLfloat) y;
   v[2] = (GLfloat) z;
   v[3] = (GLfloat) w;

   if (CC.CompileFlag) {
      gl_save_rasterpos( v );
   }
   if (CC.ExecuteFlag) {
      gl_rasterpos( v );
   }
}


void glRasterPos4s( GLshort x, GLshort y, GLshort z, GLshort w )
{
   GLfloat v[4];

   v[0] = (GLfloat) x;
   v[1] = (GLfloat) y;
   v[2] = (GLfloat) z;
   v[3] = (GLfloat) w;

   if (CC.CompileFlag) {
      gl_save_rasterpos( v );
   }
   if (CC.ExecuteFlag) {
      gl_rasterpos( v );
   }
}


/*** 2 element vector ***/

void glRasterPos2dv( const GLdouble *v )
{
   GLfloat vv[4];

   vv[0] = (GLfloat) v[0];
   vv[1] = (GLfloat) v[1];
   vv[2] = 0.0F;
   vv[3] = 1.0F;

   if (CC.CompileFlag) {
      gl_save_rasterpos( vv );
   }
   if (CC.ExecuteFlag) {
      gl_rasterpos( vv );
   }
}


void glRasterPos2fv( const GLfloat *v )
{
   GLfloat vv[4];

   vv[0] = v[0];
   vv[1] = v[1];
   vv[2] = 0.0F;
   vv[3] = 1.0F;

   if (CC.CompileFlag) {
      gl_save_rasterpos( vv );
   }
   if (CC.ExecuteFlag) {
      gl_rasterpos( vv );
   }
}


void glRasterPos2iv( const GLint *v )
{
   GLfloat vv[4];

   vv[0] = (GLfloat) v[0];
   vv[1] = (GLfloat) v[1];
   vv[2] = 0.0F;
   vv[3] = 1.0F;

   if (CC.CompileFlag) {
      gl_save_rasterpos( vv );
   }
   if (CC.ExecuteFlag) {
      gl_rasterpos( vv );
   }
}


void glRasterPos2sv( const GLshort *v )
{
   GLfloat vv[4];

   vv[0] = (GLfloat) v[0];
   vv[1] = (GLfloat) v[1];
   vv[2] = 0.0F;
   vv[3] = 1.0F;

   if (CC.CompileFlag) {
      gl_save_rasterpos( vv );
   }
   if (CC.ExecuteFlag) {
      gl_rasterpos( vv );
   }
}


/*** 3 element vector ***/

void glRasterPos3dv( const GLdouble *v )
{
   GLfloat vv[4];

   vv[0] = (GLfloat) v[0];
   vv[1] = (GLfloat) v[1];
   vv[2] = (GLfloat) v[2];
   vv[3] = 1.0F;

   if (CC.CompileFlag) {
      gl_save_rasterpos( vv );
   }
   if (CC.ExecuteFlag) {
      gl_rasterpos( vv );
   }
}


void glRasterPos3fv( const GLfloat *v )
{
   GLfloat vv[4];

   vv[0] = (GLfloat) v[0];
   vv[1] = (GLfloat) v[1];
   vv[2] = (GLfloat) v[2];
   vv[3] = 1.0F;

   if (CC.CompileFlag) {
      gl_save_rasterpos( vv );
   }
   if (CC.ExecuteFlag) {
      gl_rasterpos( vv );
   }
}


void glRasterPos3iv( const GLint *v )
{
   GLfloat vv[4];

   vv[0] = (GLfloat) v[0];
   vv[1] = (GLfloat) v[1];
   vv[2] = (GLfloat) v[2];
   vv[3] = 1.0F;

   if (CC.CompileFlag) {
      gl_save_rasterpos( vv );
   }
   if (CC.ExecuteFlag) {
      gl_rasterpos( vv );
   }
}


void glRasterPos3sv( const GLshort *v )
{
   GLfloat vv[4];

   vv[0] = (GLfloat) v[0];
   vv[1] = (GLfloat) v[1];
   vv[2] = (GLfloat) v[2];
   vv[3] = 1.0F;

   if (CC.CompileFlag) {
      gl_save_rasterpos( vv );
   }
   if (CC.ExecuteFlag) {
      gl_rasterpos( vv );
   }
}


/*** 4 element vector ***/

void glRasterPos4dv( const GLdouble *v )
{
   GLfloat vv[4];

   vv[0] = (GLfloat) v[0];
   vv[1] = (GLfloat) v[1];
   vv[2] = (GLfloat) v[2];
   vv[3] = (GLfloat) v[3];

   if (CC.CompileFlag) {
      gl_save_rasterpos( vv );
   }
   if (CC.ExecuteFlag) {
      gl_rasterpos( vv );
   }
}


void glRasterPos4fv( const GLfloat *v )
{
   if (CC.CompileFlag) {
      gl_save_rasterpos( v );
   }
   if (CC.ExecuteFlag) {
      gl_rasterpos( v );
   }
}


void glRasterPos4iv( const GLint *v )
{
   GLfloat vv[4];

   vv[0] = (GLfloat) v[0];
   vv[1] = (GLfloat) v[1];
   vv[2] = (GLfloat) v[2];
   vv[3] = (GLfloat) v[3];

   if (CC.CompileFlag) {
      gl_save_rasterpos( vv );
   }
   if (CC.ExecuteFlag) {
      gl_rasterpos( vv );
   }
}


void glRasterPos4sv( const GLshort *v )
{
   GLfloat vv[4];

   vv[0] = (GLfloat) v[0];
   vv[1] = (GLfloat) v[1];
   vv[2] = (GLfloat) v[2];
   vv[3] = (GLfloat) v[3];

   if (CC.CompileFlag) {
      gl_save_rasterpos( vv );
   }
   if (CC.ExecuteFlag) {
      gl_rasterpos( vv );
   }
}



/*
 *
 * Texture coordinates
 *
 */


void glTexCoord1d( GLdouble s )
{
   glTexCoord4f( (GLfloat) s, 0.0, 0.0, 1.0 );
}

void glTexCoord1f( GLfloat s )
{
   glTexCoord4f( s, 0.0, 0.0, 1.0 );
}

void glTexCoord1i( GLint s )
{
   glTexCoord4f( (GLfloat) s, 0.0, 0.0, 1.0 );
}

void glTexCoord1s( GLshort s )
{
   glTexCoord4f( (GLfloat) s, 0.0, 0.0, 1.0 );
}

void glTexCoord2d( GLdouble s, GLdouble t )
{
   glTexCoord4f( (GLfloat) s, (GLfloat) t, 0.0, 1.0 );
}

void glTexCoord2f( GLfloat s, GLfloat t )
{
   glTexCoord4f( s, t, 0.0, 1.0 );
}

void glTexCoord2i( GLint s, GLint t )
{
   glTexCoord4f( (GLfloat) s, (GLfloat) t, 0.0, 1.0 );
}

void glTexCoord2s( GLshort s, GLshort t )
{
   glTexCoord4f( (GLfloat) s, (GLfloat) t, 0.0, 1.0 );
}

void glTexCoord3d( GLdouble s, GLdouble t, GLdouble r )
{
   glTexCoord4f( (GLfloat) s, (GLfloat) t, (GLfloat) r, 1.0 );
}

void glTexCoord3f( GLfloat s, GLfloat t, GLfloat r )
{
   glTexCoord4f( s, t, r, 1.0 );
}

void glTexCoord3i( GLint s, GLint t, GLint r )
{
   glTexCoord4f( (GLfloat) s, (GLfloat) t, (GLfloat) r, 1.0 );
}

void glTexCoord3s( GLshort s, GLshort t, GLshort r )
{
   glTexCoord4f( (GLfloat) s, (GLfloat) t, (GLfloat) r, 1.0 );
}

void glTexCoord4d( GLdouble s, GLdouble t, GLdouble r, GLdouble q )
{
   glTexCoord4f( (GLfloat) s, (GLfloat) t, (GLfloat) r, (GLfloat) q );
}

void glTexCoord4f( GLfloat s, GLfloat t, GLfloat r, GLfloat q )
{
   if (CC.CompileFlag) {
      GLfloat tc[4];
      tc[0] = s;
      tc[1] = t;
      tc[2] = r;
      tc[3] = q;
      gl_save_texcoord( tc );
      if (!CC.ExecuteFlag) return;
   }
   CC.Current.TexCoord[0] = s;
   CC.Current.TexCoord[1] = t;
   CC.Current.TexCoord[2] = r;
   CC.Current.TexCoord[3] = q;
}

void glTexCoord4i( GLint s, GLint t, GLint r, GLint q )
{
   glTexCoord4f( (GLfloat) s, (GLfloat) t, (GLfloat) r, (GLfloat) q );
}

void glTexCoord4s( GLshort s, GLshort t, GLshort r, GLshort q )
{
   glTexCoord4f( (GLfloat) s, (GLfloat) t, (GLfloat) r, (GLfloat) q );
}

void glTexCoord1dv( const GLdouble *v )
{
   glTexCoord4f( (GLfloat) *v, 0.0, 0.0, 1.0 );
}

void glTexCoord1fv( const GLfloat *v )
{
   glTexCoord4f( *v, 0.0, 0.0, 1.0 );
}

void glTexCoord1iv( const GLint *v )
{
   glTexCoord4f( (GLfloat) *v, 0.0, 0.0, 1.0 );
}

void glTexCoord1sv( const GLshort *v )
{
   glTexCoord4f( (GLfloat) *v, 0.0, 0.0, 1.0 );
}

void glTexCoord2dv( const GLdouble *v )
{
   glTexCoord4f( (GLfloat) v[0], (GLfloat) v[1], 0.0, 1.0 );
}

void glTexCoord2fv( const GLfloat *v )
{
   glTexCoord4f( v[0], v[1], 0.0, 1.0 );
}

void glTexCoord2iv( const GLint *v )
{
   glTexCoord4f( (GLfloat) v[0], (GLfloat) v[1], 0.0, 1.0 );
}

void glTexCoord2sv( const GLshort *v )
{
   glTexCoord4f( (GLfloat) v[0], (GLfloat) v[1], 0.0, 1.0 );
}

void glTexCoord3dv( const GLdouble *v )
{
   glTexCoord4f( (GLfloat) v[0], (GLfloat) v[1], (GLfloat) v[2], 1.0 );
}

void glTexCoord3fv( const GLfloat *v )
{
   glTexCoord4f( v[0], v[1], v[2], 1.0 );
}

void glTexCoord3iv( const GLint *v )
{
   glTexCoord4f( (GLfloat) v[0], (GLfloat) v[1], (GLfloat) v[2], 1.0 );
}

void glTexCoord3sv( const GLshort *v )
{
   glTexCoord4f( (GLfloat) v[0], (GLfloat) v[1], (GLfloat) v[2], 1.0 );
}

void glTexCoord4dv( const GLdouble *v )
{
   glTexCoord4f( (GLfloat) v[0], (GLfloat) v[1],
		 (GLfloat) v[2], (GLfloat) v[3] );
}

void glTexCoord4fv( const GLfloat *v )
{
   glTexCoord4f( v[0], v[1], v[2], v[3] );
}

void glTexCoord4iv( const GLint *v )
{
   glTexCoord4f( (GLfloat) v[0], (GLfloat) v[1],
		 (GLfloat) v[2], (GLfloat) v[3] );
}

void glTexCoord4sv( const GLshort *v )
{
   glTexCoord4f( (GLfloat) v[0], (GLfloat) v[1],
		 (GLfloat) v[2], (GLfloat) v[3] );
}



/*
 *
 * Polygon Edge Flags
 *
 */


void glEdgeFlag( GLboolean flag )
{
   if (CC.ExecuteFlag) {
      CC.Current.EdgeFlag = flag;
      if (!CC.CompileFlag) return;
   }
   gl_save_edgeflag( flag );
}



void glEdgeFlagv( const GLboolean *flag )
{
   if (CC.ExecuteFlag) {
      CC.Current.EdgeFlag = *flag;
      if (!CC.CompileFlag) return;
   }
   gl_save_edgeflag( *flag );
}
