/* interp.h */

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
$Id: interp.h,v 1.13 1996/02/12 19:07:53 brianp Exp $

$Log: interp.h,v $
 * Revision 1.13  1996/02/12  19:07:53  brianp
 * applied Bill's Feb 8 patches: change eye Z's to clip W's
 *
 * Revision 1.12  1996/01/22  15:38:11  brianp
 * removed GL_INTERPOLATE_4FIXED, GL_INTERPOLATE_4UB, GL_INTERPOLATE_UB
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
 * Revision 1.8  1995/12/14  19:58:10  brianp
 * added GL_INTERPOLATE_FIXED4 macro
 *
 * Revision 1.7  1995/12/12  21:46:28  brianp
 * new GL_INTERPOLATE_Z macro
 *
 * Revision 1.6  1995/10/23  21:27:54  brianp
 * new GLubyte interpolation using fixed point arithmetic
 *
 * Revision 1.5  1995/06/12  15:43:57  brianp
 * separate GLint and GLubyte interpolation functions
 *
 * Revision 1.4  1995/06/02  13:59:01  brianp
 * faster GL_INTERPOLATE() macro
 *
 * Revision 1.3  1995/05/22  20:59:34  brianp
 * Release 1.2
 *
 * Revision 1.2  1995/03/04  19:25:29  brianp
 * 1.1 beta revision
 *
 * Revision 1.1  1995/02/27  22:20:16  brianp
 * Initial revision
 *
 */


#ifndef INTERP_H
#define INTERP_H


/*
 * Various integer interpolation functions and macros
 */



extern void gl_interpolate_z( GLint n, GLint z0, GLint z1, GLdepth zspan[] );


extern void gl_interpolate_i( GLint n, GLint y0, GLint y1, GLint yspan[] );


extern void gl_interpolate_rgba( GLint n,
                                 GLfixed r0, GLfixed r1, GLubyte rspan[],
                                 GLfixed g0, GLfixed g1, GLubyte gspan[],
                                 GLfixed b0, GLfixed b1, GLubyte bspan[],
                                 GLfixed a0, GLfixed a1, GLubyte aspan[] );


extern void gl_interp_texcoords( GLuint n, GLboolean homogeneous,
                                 GLfloat clipw0, GLfloat clipw1,
                                 GLfloat s0, GLfloat s1,
                                 GLfloat t0, GLfloat t1,
                                 GLfloat u0, GLfloat u1,
                                 GLfloat v0, GLfloat v1,
                                 GLfloat s[], GLfloat t[],
                                 GLfloat u[], GLfloat v[],
                                 GLfloat w[]);



#ifdef DEBUG

#define GL_INTERPOLATE_Z(N,Z0,Z1,ZSPAN)   gl_interpolate_z(N,Z0,Z1,ZSPAN)

#else

#define GL_INTERPOLATE_Z(N,Z0,Z1,ZSPAN)	\
{					\
   GLint zz0, zz1, dzz, ii;		\
   switch (N) {				\
      case 1:				\
	 ZSPAN[0] = Z0;			\
	 break;				\
      case 2:				\
	 ZSPAN[0] = Z0;			\
	 ZSPAN[1] = Z1;			\
	 break;				\
      case 3:				\
	 ZSPAN[0] = Z0;			\
	 ZSPAN[1] = ((Z0)+(Z1)) >> 1;	\
	 ZSPAN[2] = Z1;			\
	 break;				\
      default:				\
         zz0 = (Z0) << 7;		\
	 zz1 = (Z1) << 7;		\
	 dzz = (zz1-zz0) / ((N)-1);	\
	 for (ii=0;ii<(N);ii++) {	\
	    ZSPAN[ii] = zz0 >> 7;	\
	    zz0 += dzz;			\
	 }				\
   }					\
}



#endif  /*DEBUG*/




/*
 * Macro version of gl_interpolate_rgba()
 */
#define GL_INTERPOLATE_RGBA( N, R0,R1,R, G0,G1,G, B0,B1,B, A0,A1,A )	\
	if ((N)<2) {					\
	   R[0] = FixedToInt(R0);			\
	   G[0] = FixedToInt(G0);			\
	   B[0] = FixedToInt(B0);			\
	   A[0] = FixedToInt(A0);			\
	}						\
	else if ((N)==2) {				\
           R[0] = FixedToInt(R0);			\
           R[1] = FixedToInt(R1);			\
	   G[0] = FixedToInt(G0);			\
	   G[1] = FixedToInt(G1);			\
	   B[0] = FixedToInt(B0);			\
	   B[1] = FixedToInt(B1);			\
	   A[0] = FixedToInt(A0);			\
	   A[1] = FixedToInt(A1);			\
	}						\
	else {						\
	   GLfixed r0, dr, g0, dg, b0, db, a0, da;	\
	   GLint ii, nn = (GLint) (N) - 1;		\
	   r0 = R0;   dr = (R1-r0) / nn;		\
	   g0 = G0;   dg = (G1-g0) / nn;		\
	   b0 = B0;   db = (B1-b0) / nn;		\
	   a0 = A0;   da = (A1-a0) / nn;		\
	   for (ii=0;ii<(N);ii++) {			\
	      R[ii] = FixedToInt(r0);	r0+=dr;		\
	      G[ii] = FixedToInt(g0);	g0+=dg;		\
	      B[ii] = FixedToInt(b0);	b0+=db;		\
	      A[ii] = FixedToInt(a0);	a0+=da;		\
	   }						\
	}


#endif  /* INTERP_H */

