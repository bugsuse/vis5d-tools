/* $Id: fortran.c,v 1.4 1995/11/03 17:38:54 brianp Exp $ */

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
$Log: fortran.c,v $
 * Revision 1.4  1995/11/03  17:38:54  brianp
 * added casts for C++ compilation
 *
 * Revision 1.3  1995/10/21  13:39:51  brianp
 * updated some comments
 *
 * Revision 1.2  1995/10/17  21:33:09  brianp
 * added fglDisable, fglGenLists, fglGetString, fglGetError, fglIsEnabled
 * fglIsList, and  fglRenderMode
 *
 * Revision 1.1  1995/10/12  17:01:40  brianp
 * Initial revision
 *
 */



/*
 * Fortran wrappers for Mesa functions.
 *
 * To implement the Fortran interface we have to be aware of a few
 * Fortran/C calling conventions which vary from system to system.
 *
 * 1. function naming:  If a Fortran program calls fglAccum then the
 *    name of the C wrapper function might have to be fglaccum, fglaccum_,
 *    or FGLACCUM depending on your system's Fortran/C linking convention.
 *
 * 2. argument passing:  the following table shows what Fortran data type
 *    corresponds to each GL data type used in function arguments.
 *    Remember, Fortran passes all arguments by reference while C passes
 *    arguments by value.
 *
 *    C GL<type>         Fortran
 *    -------------      ----------
 *    GLboolean          logical*1
 *    GLbyte             integer*1
 *    GLubyte            integer*1
 *    GLshort            integer*2
 *    GLushort           integer*2
 *    GLint              integer*4
 *    GLuint             integer*4
 *    GLenum             integer*4
 *    GLsizei            integer*4
 *    GLfloat            real*4
 *    GLclampf           real*4
 *    GLdouble           real*8
 *    GLclampd           real*8
 */



#ifdef FBIND


#include <string.h>
#include "GL/gl.h"
#include "macros.h"




/*
 * Mapping of Fortran types to C types:
 */
#define INT1  GLbyte *
#define INT2  GLshort *
#define INT4  GLint *
#define REAL4  GLfloat *
#define REAL8  GLdouble *
#define CHAR8  void *
#define LOGICAL1  GLboolean *
#define CHAR256  char *



/*
 * Three methods of renaming a C function which is to be called from Fortran:
 *  0 = no change in function name
 *  1 = use all lowercase name (AIX, HPUX)
 *  2 = use all lowercase name with underscore suffix (IRIX, SunOS,
 *         OSF/1, Linux)
 *  3 = use all uppercase name (Cray)
 */





#if FBIND==1
#  define fglAccum          fglaccum
#  define fglAlphaFunc      fglalphafunc
#  define fglBegin          fglbegin
#  define fglClear          fglclear
#  define fglClearColor     fglclearcolor
#  define fglClearDepth     fglcleardepth
#  define fglClearStencil   fglclearstencil
#  define fglColor3d        fglcolor3d
#  define fglColor4d        fglcolor4d
#  define fglColor3f        fglcolor3f
#  define fglColor4f        fglcolor4f
#  define fglDisable        fgldisable
#  define fglDrawPixels     fgldrawpixels
#  define fglEnable         fglenable
#  define fglEnd            fglend
#  define fglGenLists       fglgenlists
#  define fglGetString      fglgetstring
#  define fglGetError       fglgeterror
#  define fglIsEnabled      fglisenabled
#  define fglIsList         fglislist
#  define fglLoadIdentity   fglloadidentity
#  define fglLoadMatrixd    fglloadmatrixd
#  define fglLoadMatrixf    fglloadmatrixf
#  define fglMatrixMode     fglmatrixmode
#  define fglMultMatrixd    fglmultmatrixd
#  define fglMultMatrixf    fglmultmatrixf
#  define fglRenderMode     fglrendermode
#  define fglRotated        fglrotated
#  define fglRotatef        fglrotatef
#  define fglScaled         fglscaled
#  define fglScalef         fglscalef
#  define fglTranslated     fgltranslated
#  define fglTranslatef     fgltranslatef
#  define fglVertex2f       fglvertex2f
#  define fglVertex2i       fglvertex2i
#  define fglVertex3f       fglvertex3f
#  define fglVertex4f       fglvertex4f
#  define fglViewport       fglviewport
#elif FBIND==2
#  define fglAccum          fglaccum_
#  define fglAlphaFunc      fglalphafunc_
#  define fglBegin          fglbegin_
#  define fglClear          fglclear_
#  define fglClearColor     fglclearcolor_
#  define fglClearDepth     fglcleardepth_
#  define fglClearStencil   fglclearstencil_
#  define fglColor3d        fglcolor3d_
#  define fglColor4d        fglcolor4d_
#  define fglColor3f        fglcolor3f_
#  define fglColor4f        fglcolor4f_
#  define fglDisable        fgldisable_
#  define fglDrawPixels     fgldrawpixels_
#  define fglEnable         fglenable_
#  define fglEnd            fglend_
#  define fglGenLists       fglgenlists_
#  define fglGetString      fglgetstring_
#  define fglGetError       fglgeterror_
#  define fglIsEnabled      fglisenabled_
#  define fglIsList         fglislist_
#  define fglLoadIdentity   fglloadidentity_
#  define fglLoadMatrixd    fglloadmatrixd_
#  define fglLoadMatrixf    fglloadmatrixf_
#  define fglMatrixMode     fglmatrixmode_
#  define fglMultMatrixd    fglmultmatrixd_
#  define fglMultMatrixf    fglmultmatrixf_
#  define fglRenderMode     fglrendermode_
#  define fglRotated        fglrotated_
#  define fglRotatef        fglrotatef_
#  define fglScaled         fglscaled_
#  define fglScalef         fglscalef_
#  define fglTranslated     fgltranslated_
#  define fglTranslatef     fgltranslatef_
#  define fglVertex2f       fglvertex2f_
#  define fglVertex2i       fglvertex2i_
#  define fglVertex3f       fglvertex3f_
#  define fglVertex4f       fglvertex4f_
#  define fglViewport       fglviewport_
#elif FBIND==3
#  define fglAccum          FGLACCUM
#  define fglAlphaFunc      FGLALPHAFUNC
#  define fglBegin          FGLBEGIN
#endif



void fglAccum ( INT4 op, REAL4 value )
{
   glAccum( (GLenum) *op, *value );
}


void fglAlphaFunc ( INT4 func, REAL4 ref )
{
   glAlphaFunc( (GLenum) *func, *ref );
}


void fglBegin ( INT4 mode )
{
   glBegin( (GLenum) *mode );
}


void fglBitmap ( INT4 width, INT4 height, REAL4 xorig, REAL4 yorig,
                 REAL4 xmove, REAL4 ymove, void *bitmap )
{
   glBitmap( *width, *height, *xorig, *yorig, *xmove, *ymove,
             (GLubyte *) bitmap );
}


void fglBlendColorEXT ( REAL4 red, REAL4 green, REAL4 blue, REAL4 alpha )
{
   glBlendColorEXT( *red, *green, *blue, *alpha );
}


void fglBlendEquationEXT ( INT4 mode )
{
   glBlendEquationEXT( (GLenum) *mode );
}


void fglBlendFunc ( INT4 sfactor, INT4 dfactor )
{
   glBlendFunc( (GLenum) *sfactor, (GLenum) *dfactor );
}


void fglCallList ( INT4 list )
{
   glCallList( *list );
}


void fglCallLists ( INT4 n, INT4 type, void *lists )
{
   glCallLists( *n, (GLenum) *type, lists );
}


void fglClear ( INT4 mask )
{
   glClear( (GLbitfield) *mask );
}


void fglClearAccum ( REAL4 r, REAL4 g, REAL4 b, REAL4 a )
{
   glClearAccum( *r, *g, *b, *a );
}


void fglClearColor ( REAL4 r, REAL4 g, REAL4 b, REAL4 a )
{
   glClearColor( *r, *g, *b, *a );
}


void fglClearDepth ( REAL4 depth )
{
   glClearDepth( *depth );
}


void fglClearStencil ( INT4 s )
{
   glClearStencil( *s );
}


void fglClipPlane ( INT4 plane, void *equation )
{
   glClipPlane( (GLenum) *plane, (GLdouble *) equation );
}


void fglColor3d ( REAL8 r, REAL8 g, REAL8 b )
{
   glColor3d( *r, *g, *b );
}

void fglColor4d ( REAL8 r, REAL8 g, REAL8 b, REAL8 a )
{
   glColor4d( *r, *g, *b, *a );
}


void fglColor3f ( REAL4 r, REAL4 g, REAL4 b )
{
   glColor3f( *r, *g, *b );
}

void fglColor4f ( REAL4 r, REAL4 g, REAL4 b, REAL4 a )
{
   glColor4f( *r, *g, *b, *a );
}


/* MORE */



void fglDisable ( INT4 cap )
{
   glDisable( (GLenum) *cap );
}

void fglDrawPixels ( INT4 width, INT4 height, INT4 format, INT4 type,
                     CHAR8 pixels )
{
   glDrawPixels( *width, *height, (GLenum) *format, (GLenum) *type, pixels );
}


void fglEdgeFlag ( LOGICAL1 flag )
{
   glEdgeFlag( *flag );
}


void fglEdgeFlagv ( CHAR8 flag )
{
   GLboolean *f = (GLboolean *) flag;
   glEdgeFlagv( f );
}


void fglEnable ( INT4 cap )
{
   glEnable( (GLenum) *cap );
}


void fglEnd ( void )
{
   glEnd();
}


GLint fglGenLists ( INT4 range )
{
   return glGenLists( *range );
}


/*
 * Implementing a C function which returns a characters string via a
 * Fortran invokation is tricky.  This solution works on IRIX.
 */
void fglGetString ( char result[], int length, INT4 name )
{
   int i;
   char *str;

   for (i=0;i<length;i++) {
      result[i] = ' ';
   }
   str = (char *) glGetString( (GLenum) *name );
   if (str) {
      int len = strlen(str);
      if (len>0) {
         strncpy( result, str, MIN2(len,length) );
      }
   }
}


GLint fglGetError ( void )
{
   return glGetError();
}


GLboolean fglIsEnabled ( INT4 cap )
{
   return glIsEnabled( (GLenum) *cap );
}


GLboolean fglIsList ( INT4 list )
{
   return glIsList( *list );
}



/* MORE */

void fglLoadIdentity( void )
{
   glLoadIdentity();
}


void fglLoadMatrixd( CHAR8 m )
{
   glLoadMatrixd( (GLdouble *) m );
}


void fglLoadMatrixf( CHAR8 m )
{
   glLoadMatrixf( (GLfloat *) m );
}


void fglMatrixMode( INT4 mode )
{
   glMatrixMode( (GLenum) *mode );
}


void fglMultMatrixd( CHAR8 m )
{
   glMultMatrixd( (GLdouble *) m );
}


void fglMultMatrixf( CHAR8 m )
{
   glMultMatrixf( (GLfloat *) m );
}


/* MORE */


GLint fglRenderMode ( INT4 mode )
{
   return glRenderMode( (GLenum) *mode );
}


void fglRotated ( REAL8 angle, REAL8 ax, REAL8 ay, REAL8 az )
{
   glRotated( *angle, *ax, *ay, *az );
}


void fglRotatef ( REAL4 angle, REAL4 ax, REAL4 ay, REAL4 az )
{
   glRotatef( *angle, *ax, *ay, *az );
}


void fglScaled ( REAL8 sx, REAL8 sy, REAL8 sz )
{
   glScaled( *sx, *sy, *sz );
}


void fglScalef ( REAL4 sx, REAL4 sy, REAL4 sz )
{
   glScalef( *sx, *sy, *sz );
}


void fglTranslated ( REAL8 tx, REAL8 ty, REAL8 tz )
{
   glTranslated( *tx, *ty, *tz );
}


void fglTranslatef ( REAL4 tx, REAL4 ty, REAL4 tz )
{
   glTranslatef( *tx, *ty, *tz );
}




/* MORE */



void fglVertex2f ( REAL4 x, REAL4 y )
{
   glVertex2f( *x, *y );
}

void fglVertex2i ( INT4 x, INT4 y )
{
   glVertex2i( *x, *y );
}

void fglVertex3f ( REAL4 x, REAL4 y, REAL4 z )
{
   glVertex3f( *x, *y, *z );
}

void fglVertex4f ( REAL4 x, REAL4 y, REAL4 z, REAL4 w )
{
   glVertex4f( *x, *y, *z, *w );
}


void fglViewport ( INT4 x, INT4 y, INT4 width, INT4 height )
{
   glViewport( *x, *y, *width, *height );
}


/* MORE */




#else /*ifdef FBIND*/



/* If FBIND is not defined, Fortran bindings aren't made.  Need this
 * dummy function for some picky compilers.
 */
static void dummy()
{
   return;
}



#endif /*ifdef FBIND*/
