/* bresenhm.h */

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
$Id: bresenhm.h,v 1.6 1996/01/11 20:15:26 brianp Exp $

$Log: bresenhm.h,v $
 * Revision 1.6  1996/01/11  20:15:26  brianp
 * renamed macro variables to prevent name collisions
 *
 * Revision 1.5  1995/12/12  21:44:36  brianp
 * added BRESENHAM_Z macro
 *
 * Revision 1.4  1995/06/09  17:45:58  brianp
 * renamed to bresenhm.[ch]
 *
 * Revision 1.3  1995/05/22  20:59:34  brianp
 * Release 1.2
 *
 * Revision 1.2  1995/03/04  19:25:29  brianp
 * 1.1 beta revision
 *
 * Revision 1.1  1995/02/24  14:18:07  brianp
 * Initial revision
 *
 */


/*
 * A macro which executes Bresenham's line drawing algorithm.  The
 * previously defined BRESENHAM_PLOT macro is then used to 'plot' pixels.
 */


#ifndef BRESENHAM_H
#define BRESENHAM_H


#include "GL/gl.h"



/*
 * Bresenham's line algorithm.
 */
#define BRESENHAM( x1, y1, x2, y2 )	\
{					\
   GLint dx, dy, xf, yf, ta, tb, tt, i;	\
   if (x1!=x2 || y1!=y2) {		\
      if (x2>x1) {			\
         dx = x2-x1;			\
         xf = 1;			\
      }					\
      else {				\
         dx = x1-x2;			\
         xf = -1;			\
      }					\
      if (y2>y1) {			\
         dy = y2-y1;			\
         yf = 1;			\
      }					\
      else {				\
         dy = y1-y2;			\
         yf = -1;			\
      }					\
      if (dx>dy) {			\
         ta = dy+dy;			\
         tt = ta-dx;			\
         tb = tt-dx;			\
         for (i=0;i<=dx;i++) {		\
	    BRESENHAM_PLOT( x1, y1 )	\
            x1 += xf;			\
            if (tt<0) {			\
               tt += ta;		\
            }				\
            else {			\
               tt += tb;		\
               y1 += yf;		\
            }				\
         }				\
      }					\
      else {				\
         ta = dx+dx;			\
         tt = ta-dy;			\
         tb = tt-dy;			\
         for (i=0;i<=dy;i++) {		\
	    BRESENHAM_PLOT( x1, y1 )	\
            y1 += yf;			\
            if (tt<0) {			\
               tt += ta;		\
            }				\
            else {			\
               tt += tb;		\
               x1 += xf;		\
	    }				\
         }				\
      }					\
   }					\
}




/*
 * Bresenham's line algorithm with Z interpolation.
 * Z interpolation done with fixed point arithmetic, 8 fraction bits.
 */
#define BRESENHAM_Z( x1, y1, z1, x2, y2, z2 )	\
{						\
   GLint dx, dy, xf, yf, ta, tb, tt, i;		\
   GLint dz;					\
   if (x1!=x2 || y1!=y2) {			\
      z1 = z1 << 8;				\
      z2 = z2 << 8;				\
      if (x2>x1) {				\
         dx = x2-x1;				\
         xf = 1;				\
      }						\
      else {					\
         dx = x1-x2;				\
         xf = -1;				\
      }						\
      if (y2>y1) {				\
         dy = y2-y1;				\
         yf = 1;				\
      }						\
      else {					\
         dy = y1-y2;				\
         yf = -1;				\
      }						\
      if (dx>dy) {				\
         dz = (z2-z1)/dx;			\
         ta = dy+dy;				\
         tt = ta-dx;				\
         tb = tt-dx;				\
         for (i=0;i<=dx;i++) {			\
	    BRESENHAM_PLOT( x1, y1, z1>>8 )	\
            x1 += xf;				\
            if (tt<0) {				\
               tt += ta;			\
            }					\
            else {				\
               tt += tb;			\
               y1 += yf;			\
            }					\
	    z1 += dz;				\
         }					\
      }						\
      else {					\
         dz = (z2-z1)/dy;			\
         ta = dx+dx;				\
         tt = ta-dy;				\
         tb = tt-dy;				\
         for (i=0;i<=dy;i++) {			\
	    BRESENHAM_PLOT( x1, y1, z1>>8 )	\
            y1 += yf;				\
            if (tt<0) {				\
               tt += ta;			\
            }					\
            else {				\
               tt += tb;			\
               x1 += xf;			\
	    }					\
	    z1 += dz;				\
         }					\
      }						\
   }						\
}




extern GLuint gl_bresenham( GLint x1, GLint y1, GLint x2, GLint y2,
			    GLint x[], GLint y[] );


extern GLuint gl_stippled_bresenham( GLint x1, GLint y1, GLint x2, GLint y2,
				     GLint x[], GLint y[], GLubyte mask[] );



#endif
