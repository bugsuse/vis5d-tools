/* interp.c */

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
$Id: interp.c,v 1.17 1996/02/14 15:40:50 brianp Exp $

$Log: interp.c,v $
 * Revision 1.17  1996/02/14  15:40:50  brianp
 * replaced ABS with ABSF, added F suffix to floating point literals
 *
 * Revision 1.16  1996/02/12  19:07:53  brianp
 * applied Bill's Feb 8 patches: change eye Z's to clip W's
 *
 * Revision 1.15  1996/02/01  00:57:45  brianp
 * replaced dummy macro argument 0; with {}
 *
 * Revision 1.14  1996/01/30  19:43:05  brianp
 * added a dummy parameter to two instances of INTERP_TEXCOORD_LOOPS
 * to prevent cpp warnings
 *
 * Revision 1.13  1996/01/24  14:57:59  brianp
 * applied gl_interp_texcoords() patch from Bill Triggs
 *
 * Revision 1.12  1996/01/22  15:38:50  brianp
 * removed gl_interpolate_ub, gl_interpolate_4ub, gl_interpolate_4fixed
 * applied a patch to gl_interp_texcoords(), per Bill Triggs
 *
 * Revision 1.11  1995/12/30  17:16:52  brianp
 * new gl_interp_texcoords function per Bill Triggs
 *
 * Revision 1.10  1995/12/30  00:52:50  brianp
 * added gl_interpolate_4fixed function
 * removed dead code
 *
 * Revision 1.9  1995/12/18  17:29:35  brianp
 * use new GLdepth datatype
 *
 * Revision 1.8  1995/12/12  21:44:59  brianp
 * added gl_interpolate_z() function
 *
 * Revision 1.7  1995/10/23  21:27:54  brianp
 * new GLubyte interpolation using fixed point arithmetic
 *
 * Revision 1.6  1995/06/12  15:43:57  brianp
 * separate GLint and GLubyte interpolation functions
 *
 * Revision 1.5  1995/06/02  13:58:36  brianp
 * faster gl_interpolate(), tried gl_interpolate_rgba()
 *
 * Revision 1.4  1995/05/22  21:02:41  brianp
 * Release 1.2
 *
 * Revision 1.3  1995/03/17  19:15:21  brianp
 * tried to improve gl_interp_texcoords, not much luck
 *
 * Revision 1.2  1995/03/04  19:29:44  brianp
 * 1.1 beta revision
 *
 * Revision 1.1  1995/02/27  22:20:13  brianp
 * Initial revision
 *
 */


#include "context.h"
#include "macros.h"




/*
 * Linear integer interpolation of Z (depth) values:
 * Iterpolate n integer values between z0 and z1 and put into zspan.
 * When finished, zspan[0] = z0, zspan[n-1] = z1, and the rest of
 * zspan[] is filled with interpolated values.
 * We have to be careful to avoid integer overflow!
 */
void gl_interpolate_z( GLint n, GLint z0, GLint z1, GLdepth zspan[] )
{
   GLint i, dz;

   switch (n) {
      case 1:
         zspan[0] = z0;
         return;
      case 2:
         zspan[0] = z0;
         zspan[1] = z1;
         return;
      case 3:
         zspan[0] = z0;
         zspan[1] = (z0 + z1) >> 1;
         zspan[2] = z1;
         return;
      default:
         z0 = z0 << 7;
         z1 = z1 << 7;
         dz = (z1-z0) / (n-1);
         for (i=0;i<n;i++) {
            zspan[i] = z0 >> 7;
            z0 += dz;
         }
         return;
   }
}



/*
 * Linear integer interpolation:
 * Iterpolate n integer values between y0 and y1 and put into yspan.
 * When finished, yspan[0] = y0, yspan[n-1] = y1, and the rest of
 * yspan[] is filled with interpolated values.
 */
void gl_interpolate_i( GLint n, GLint y0, GLint y1, GLint yspan[] )
{
   switch (n) {
      case 1:
         yspan[0] = y0;
	 return;
      case 2:
         yspan[0] = y0;
         yspan[1] = y1;
	 return;
      case 3:
         yspan[0] = y0;
	 yspan[1] = (y0+y1) >> 1;
         yspan[2] = y1;
	 return;
      default:
	 if (y0==y1) {
	    register GLint i;
	    for (i=0;i<n;i++) {
	       yspan[i] = y0;
	    }
	 }
	 else {
	    register GLint i;
	    register GLint dx, dy;
	    register GLint a, b, d;
	    register GLint y;
	    register GLint qa, qb;
   	    dx = n-1;
	    dy = y1 - y0;
	    qa = dy / dx;
	    dy = dy % dx;
	    if (dy<0) {
	       dy = -dy;
	       qb = qa - 1;
	    }
	    else {
	       qb = qa + 1;
	    }
	    a = dy+dy;   d = a-dx;   b = d-dx;
	    y = y0;
	    for (i=0;i<n;i++) {
	       yspan[i] = y;
	       if (d<0) {
		  d += a;
		  y += qa;
	       }
	       else {
		  d += b;
		  y += qb;
	       }
	    }
	 }
   }
}



/*
 * Interpolate RGBA values.
 * Input:  n - number of values to generate
 *         r0, r1 - first and last alpha values
 *         g0, g1 - first and last alpha values
 *         b0, b1 - first and last alpha values
 *         a0, a1 - first and last alpha values
 * Output:  rspan, gspan, bspan, aspan - interpolated color values in
 *          the range [0,CC.RedScale], [0,CC.GreenScale], [0,CC.BlueScale],
 *          and [0,CC.AlphaScale].
 */
void gl_interpolate_rgba( GLint n,
                          GLfixed r0, GLfixed r1, GLubyte rspan[],
                          GLfixed g0, GLfixed g1, GLubyte gspan[],
                          GLfixed b0, GLfixed b1, GLubyte bspan[],
                          GLfixed a0, GLfixed a1, GLubyte aspan[] )
{
   GLint i, m;
   GLfixed dr, dg, db, da;

   switch (n) {
      case 1:
         rspan[0] = FixedToInt(r0);
         gspan[0] = FixedToInt(g0);
         bspan[0] = FixedToInt(b0);
         aspan[0] = FixedToInt(a0);
	 return;
      case 2:
         rspan[0] = FixedToInt(r0);   rspan[1] = FixedToInt(r1);
         gspan[0] = FixedToInt(g0);   gspan[1] = FixedToInt(g1);
         bspan[0] = FixedToInt(b0);   bspan[1] = FixedToInt(b1);
         aspan[0] = FixedToInt(a0);   aspan[1] = FixedToInt(a1);
	 return;
      default:
         m = n-1;
         dr = (r1-r0) / m;
         dg = (g1-g0) / m;
         db = (b1-b0) / m;
         da = (a1-a0) / m;
         for (i=0;i<n;i++) {
            rspan[i] = FixedToInt(r0);    r0 += dr;
            gspan[i] = FixedToInt(g0);    g0 += dg;
            bspan[i] = FixedToInt(b0);    b0 += db;
            aspan[i] = FixedToInt(a0);    a0 += da;
         }
         return;
   }
}




/* Interpolate texture coordinates. There are several cases so we define
 * a macro for convenience. Depth w can be interpolated affinely or
 * perspectively, with or without the explicit depth values being
 * stored, and for each case there is affine, homogeneous perspective,
 * or inhomogeneous perspective interpolation of 2D (and eventually 3D)
 * texture. Eg, one can have an orthographic (in depth) projection of a
 * projective (in texcoords) texture, etc. The below macro handles the
 * current texture interpolation cases.
 */
#define INTERP_TEXCOORD_LOOPS(DEFINE_MU,EVAL_W)				\
{									\
  if (!v) {								\
    /* Simple affine 2D texture */					\
    for (i=0;i<n;i++) {							\
      DEFINE_MU;							\
      s[i] = s0 + mu * delta_s;						\
      t[i] = t0 + mu * delta_t;						\
      EVAL_W								\
    }									\
  } else if (homogeneous) {						\
    /* Homogeneous interpolation of 2D projective texture		\
     * (down polygon edges) */						\
    for (i=0;i<n;i++) {							\
      DEFINE_MU;							\
      s[i] = s0 + mu * delta_s;						\
      t[i] = t0 + mu * delta_t;						\
      v[i] = v0 + mu * delta_v;						\
      EVAL_W								\
    }									\
  } else {								\
    /* Inhomogeneous interpolation of 2D projective texture		\
     * (along lines or polygon spans) */				\
    for (i=0;i<n;i++) {							\
      GLfloat vi;							\
      DEFINE_MU;							\
      vi = v0 + mu * delta_v;						\
      s[i] = (s0 + mu * delta_s)/vi;					\
      t[i] = (t0 + mu * delta_t)/vi;					\
      EVAL_W								\
    }									\
  }									\
}

/*
 * Perform texture coordinate interpolation along a line in window coord-
 * inate space.  Depending on the perspective correction hint we'll either
 * just do simple linear interpolation or interpolation with perspective
 * correction.
 * Input:  n - number of texture coords to produce
 *	   homogeneous  - interpolate linearly in homogeneous texture 
 *			  coordinates (i.e. do not divide s,t,u by v to get
 *			  affine texture space indices).
 *         clipw0, clipw1 - w coordinate of end points in clip coords
 *         s0, s1 - S-component of texture coords at end points
 *         t0, t1 - T-component of texture coords at end points
 *         u0, u1 - U-component of texture coords at end points
 *         v0, v1 - V-component of texture coords at end points
 * Output:  s, t, u, v - resulting arrays of S, T, U, V texture coordinates
 *          w - interpolated W-component of clip coordinates along the line
 *              (note that w can be a NULL pointer if w isn't needed)
 */
void gl_interp_texcoords( GLuint n, GLboolean homogeneous,
			  GLfloat clipw0, GLfloat clipw1,
			  GLfloat s0, GLfloat s1,
			  GLfloat t0, GLfloat t1,
			  GLfloat u0, GLfloat u1,
			  GLfloat v0, GLfloat v1,
			  GLfloat s[], GLfloat t[], GLfloat u[], GLfloat v[],
			  GLfloat w[])
{
   /*
    * This implementation of this function contributed by
    * Bill Triggs  <Bill.Triggs@imag.fr>.  Thanks Bill!
    */

   if (n==1) {
      if (homogeneous) {
         s[0] = s0;
         t[0] = t0;
         v[0] = v0;
      }
      else {
         s[0] = s0/v0;
         t[0] = t0/v0;
      }
      if (w)  w[0] = clipw0;
   }
   else {
      GLfloat delta_clipw = clipw1 - clipw0;
      GLfloat delta_s = s1 - s0;
      GLfloat delta_t = t1 - t0;
      GLfloat delta_v = v1 - v0;
      GLuint i;

      if (!homogeneous && v0==1.0F && v1==1.0F) {
	/* avoid trivial v scalings */
	v = 0;
      }
      if (CC.ProjectionMatrix[14]==0.0F
          || CC.Hint.PerspectiveCorrection==GL_FASTEST
	  || ABSF(delta_clipw)<0.001F*ABSF(clipw0+clipw1)) {
         /* Affine interpolation in depth -- orthographic or 
	    almost frontal perspective projection. */
	if (w) {
	  INTERP_TEXCOORD_LOOPS(GLfloat mu = (GLfloat) i / (n-1),
				w[i] = clipw0 + mu * delta_clipw;);
	}
	else {
	  INTERP_TEXCOORD_LOOPS(GLfloat mu = (GLfloat) i / (n-1), {} );
	}
      }
      else {
	/* Full perspective interpolation in depth (w<=0) */

	 if (clipw0<1e-20) clipw0=1e-20;
	 if (clipw1<1e-20) clipw1=1e-20;

	 if (w) {
	   INTERP_TEXCOORD_LOOPS
	     (GLfloat w0i = clipw0 * i; 
	      GLfloat mu = w0i / (clipw1 * (n-1-i) + w0i),
	      w[i] = clipw0 + mu * delta_clipw;);
	 }
	 else {
	   INTERP_TEXCOORD_LOOPS
	     (GLfloat w0i = clipw0 * i; 
	      GLfloat mu = w0i / (clipw1 * (n-1-i) + w0i), {} );
	 }
       }
    }
}

