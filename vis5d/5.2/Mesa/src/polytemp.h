/* $Id: polytemp.h,v 1.7 1996/01/19 19:17:19 brianp Exp $ */

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
$Log: polytemp.h,v $
 * Revision 1.7  1996/01/19  19:17:19  brianp
 * fixed uninitialized len problem for flat-shaded polygons
 *
 * Revision 1.6  1996/01/19  18:10:09  brianp
 * fixed floating point exception due to float->integer conversion overflow
 *
 * Revision 1.5  1996/01/11  22:52:44  brianp
 * new zptr computation
 *
 * Revision 1.4  1996/01/11  15:47:35  brianp
 * added Y-axis clipping
 *
 * Revision 1.3  1996/01/02  22:13:40  brianp
 * removed old fixed-point macros
 *
 * Revision 1.2  1995/12/30  00:59:40  brianp
 * use integer vertex colors instead of floating point
 * faster computation of z from x and y
 *
 * Revision 1.1  1995/12/21  18:00:57  brianp
 * Initial revision
 *
 */



/*
 * Polygon rasterizer template.
 *
 * By defining certain preprocessor symbols, this general polygon
 * rasterizer is used to generate a number of custom-purpose
 * polygon rasterizers.  See polygons.c and xmesa3.c for examples.
 */


/*
 * Preprocessor symbols:
 *
 * INTERP_COLOR   - if defined, generate RGB channel interpolation code
 * INTERP_ALPHA   - if defined, also generate alpha channel interpolation code
 * INTERP_Z       - if defined, generate Z-buffer code
 * INTERP_INDEX   - if defined, generate Color Index interpolation code
 *
 * SETUP_CODE     - this optional macro is code which will be executed
 *                  at the top of the polygon function; typically used
 *                  to setup the pixel used for mono-colored polygons
 *
 * INNER_CODE     - this is a macro which must be defined before including
 *                  this file.  It's the innermost code of the rasterizer
 *                  which writes pixels to the color buffer.
 */




/*
 * Function prolog required in .c file:
 *
 *   static void your_polygon_name( GLuint n, GLuint vlist[], GLuint pv )
 *
 */
{
   GLint vert;
   GLint y, ymin, ymax;
   GLfloat yf;
   GLfloat dzdx;
   GLfixed fdzdx;
   GLfloat planea = CC.PlaneA;
   GLfloat planeb = CC.PlaneB;
   GLfloat planec = 1.0F / CC.PlaneC;
   GLfloat planed = CC.PlaneD;
   GLdepth *zrow;
   GLint zrowinc = CC.BufferWidth;

   /* Optional, user-supplied setup code: */
#ifdef SETUP_CODE
   SETUP_CODE
#endif

#ifdef INTERP_Z
   /* compute fixed point dz/dx */
   dzdx = -planea * planec;
   if (dzdx<-1.0F)      fdzdx = -MAX_DEPTH;
   else if (dzdx>1.0F)  fdzdx = MAX_DEPTH;
   else                 fdzdx = FloatToFixed( dzdx * DEPTH_SCALE );
#endif

   ymin = MAX_HEIGHT;
   ymax = -1;

   /*** PART 1:  process edges to compute span bounds ***/
   for (vert=0;vert<n;vert++) {
      GLuint j0, j1;
      GLfloat dxdy, b;
      GLint iy0, iy1, idy;
      GLfloat x0, y0, x1, y1;
      GLfixed fx, fdxdy;
      GLfixed r0, dr, g0, dg, b0, db, a0, da;
      GLfixed i0, i1, di;
      GLint len;    /* vertical length of edge */

      /* indexes of the edge vertices */
      j0 = (vert==0) ? vlist[n-1] : vlist[vert-1];
      j1 = vlist[vert];

      x0 = VB.Win[j0][0];   y0 = VB.Win[j0][1];
      x1 = VB.Win[j1][0];   y1 = VB.Win[j1][1];

      iy0 = (GLint) y0;
      iy1 = (GLint) y1;

      if (iy0==iy1) {
	 continue;    /* skip horizontal edges */
      }
      else if (iy0<iy1) {
	 dxdy = (x1-x0) / (y1-y0);
	 b = x0 - dxdy * y0;         /* b = X intercept */
	 len = iy1 - iy0;
#ifdef INTERP_COLOR
	 r0 = VB.Color[j0][0];
	 g0 = VB.Color[j0][1];
	 b0 = VB.Color[j0][2];
#  ifdef INTERP_ALPHA
	 a0 = VB.Color[j0][3];
#  endif
         if (len>1) {
            dr = (VB.Color[j1][0] - r0) / len;
            dg = (VB.Color[j1][1] - g0) / len;
            db = (VB.Color[j1][2] - b0) / len;
#  ifdef INTERP_ALPHA
            da = (VB.Color[j1][3] - a0) / len;
#  endif
         }
#endif
#ifdef INTERP_INDEX
         i0 = IntToFixed( VB.Index[j0] );
         if (len>0) {
            i1 = IntToFixed( VB.Index[j1] );
            di = (i1 - i0) / len;
         }
#endif
      }
      else {
	 /* y0 > y1 */
         GLint tmp;
         tmp = iy0;  iy0 = iy1;  iy1 = tmp;  /* Swap Y coords */
	 dxdy = (x0-x1) / (y0-y1);
	 b = x1 - dxdy * y1;         /* b = X intercept */
	 len = iy1 - iy0;
#ifdef INTERP_COLOR
	 r0 = VB.Color[j1][0];
	 g0 = VB.Color[j1][1];
	 b0 = VB.Color[j1][2];
#  ifdef INTERP_ALPHA
	 a0 = VB.Color[j1][3];
#  endif
         if (len>1) {
            dr = (VB.Color[j0][0] - r0) / len;
            dg = (VB.Color[j0][1] - g0) / len;
            db = (VB.Color[j0][2] - b0) / len;
#  ifdef INTERP_ALPHA
            da = (VB.Color[j0][3] - a0) / len;
#  endif
         }
#endif
#ifdef INTERP_INDEX
         i0 = IntToFixed( VB.Index[j1] );
         if (len>0) {
            i1 = IntToFixed( VB.Index[j0] );
            di = (i1 - i0) / len;
         }
#endif
      }

      iy0++;
      iy1++;

      /* compute span bounds */
      fx = FloatToFixed( iy0 * dxdy + b + 1.0F );
      if (len>1) {
         /* don't compute if <=1 to prevent float->int overflow error */
         fdxdy = FloatToFixed( dxdy );
      }
      else {
         fdxdy = 0;
      }
      for (y=iy0; y<iy1; y++) {
	 int ix = FixedToInt( fx );
	 fx += fdxdy;

#ifdef CLIP_Y
         /* make sure y isn't ouside of array */
         if (y>=0 && y<CC.BufferHeight) {
#endif
	 /* update left and right bounds */
	 if (ix < lx[y]) {
	    lx[y] = ix;
#ifdef INTERP_COLOR
	    lr[y] = r0;	 lg[y] = g0;  lb[y] = b0;
#  ifdef INTERP_ALPHA
	    la[y] = a0;
#  endif
#endif
#ifdef INTERP_INDEX
            li[y] = i0;
#endif
	 }
	 if (ix > rx[y]) {
	    rx[y] = ix;
#ifdef INTERP_COLOR
	    rr[y] = r0;	 rg[y] = g0;  rb[y] = b0;
#  ifdef INTERP_ALPHA
	    ra[y] = a0;
#  endif
#endif
#ifdef INTERP_INDEX
            ri[y] = i0;
#endif
	 }

#ifdef CLIP_Y
         }
#endif

#ifdef INTERP_COLOR
	 r0 += dr;  g0 += dg;  b0 += db;
#  ifdef INTERP_ALPHA
	 a0 += da;
#  endif
#endif
#ifdef INTERP_INDEX
         i0 += di;
#endif
      }

      /* find min and max y */
      if (iy0 < ymin)  ymin = iy0;
      if (iy0 > ymax)  ymax = iy0;
      if (iy1 < ymin)  ymin = iy1;
      if (iy1 > ymax)  ymax = iy1;
   }

#ifdef CLIP_Y
   if (ymin<0) {
      ymin = 0;
   }
   else if (ymax>CC.BufferHeight) {
      ymax = CC.BufferHeight;
   }
#endif

   /*** PART 2:  scan convert ***/
   yf = (GLfloat) ymin;
   zrow = Z_ADDRESS( 0, ymin );
   for (y=ymin;y<ymax;y++,yf+=1.0F) {
      GLint xmin = lx[y];
      GLint xmax = rx[y];
      GLint len = xmax-xmin;

      /* reset left and right bounds for next polygon */
      lx[y] = MAX_WIDTH;
      rx[y] = -1;

      /* render the span w/ optional color interp, depth interp */
      if (len>0) {
         GLint iz0;
         GLfloat z;
         GLfixed fz;
         GLdepth *zptr;
	 GLfixed fr, fdrdx, fg, fdgdx, fb, fdbdx, fa, fdadx;
         GLfixed fi, fdidx;

#ifdef INTERP_Z
         z = (planed - planea*xmin - planeb*yf) * planec;
         if (z<0.0F) {
            fz = IntToFixed( 0 );
         }
         else if (z>1.0F) {
            fz = IntToFixed( MAX_DEPTH );
         }
         else {
            fz = IntToFixed( (GLint) (z * DEPTH_SCALE) );
         }

         zptr = zrow + xmin;
#endif

#ifdef INTERP_COLOR
	 fr = lr[y];
	 fg = lg[y];
	 fb = lb[y];
#  ifdef INTERP_ALPHA
	 fa = la[y];
#  endif
	 if (len>1) {
	    fdrdx = (rr[y]-fr) / (len-1);
	    fdgdx = (rg[y]-fg) / (len-1);
	    fdbdx = (rb[y]-fb) / (len-1);
#  ifdef INTERP_ALPHA
	    fdadx = (ra[y]-fa) / (len-1);
#  endif
         }
#endif
#ifdef INTERP_INDEX
         fi = li[y];
         if (len>1) {
            fdidx = (ri[y]-fi) / (len-1);
         }
#endif

         {
            INNER_CODE
         }

      } /* if len>0 */

#ifdef INTERP_Z
      zrow += zrowinc;
#endif

   } /* for y */

}



#undef INTERP_COLOR
#undef INTERP_ALPHA
#undef INTERP_Z
#undef INTERP_INDEX
#undef SETUP_CODE
#undef INNER_CODE
#undef CLIP_Y

