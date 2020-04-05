/*  matrix.c */

/* Vis5D version 5.2 */

/*
Vis5D system for visualizing five dimensional gridded data sets
Copyright (C) 1990 - 2000 Bill Hibbard, Johan Kellum, Brian Paul,
Dave Santek, and Andre Battaiola.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include <stdio.h>
#include <math.h>
#include <string.h>
#include "matrix.h"



/*** make_matrix ******************************************************
   Make a transformation matrix to perform the given rotation, scale and
   translation.  This function uses the fast matrix post-concatenation
   techniques from Graphics Gems.
**********************************************************************/
void make_matrix( float rotx, float roty, float rotz,
                  float scale, float transx, float transy, float transz,
                  MATRIX mat )
{
   register float  sx, sy, sz, cx, cy, cz, t;
   register int i;
   float deg2rad = 1.0F / 57.2957F;

   /* Get sin and cosine values */
   sx = sin(rotx * deg2rad);
   cx = cos(rotx * deg2rad);
   sy = sin(roty * deg2rad);
   cy = cos(roty * deg2rad);
   sz = sin(rotz * deg2rad);
   cz = cos(rotz * deg2rad);

   /* Start with identity matrix */
   mat[0][0] = 1.0;  mat[0][1] = 0.0;  mat[0][2] = 0.0;  mat[0][3] = 0.0;
   mat[1][0] = 0.0;  mat[1][1] = 1.0;  mat[1][2] = 0.0;  mat[1][3] = 0.0;
   mat[2][0] = 0.0;  mat[2][1] = 0.0;  mat[2][2] = 1.0;  mat[2][3] = 0.0;
   mat[3][0] = 0.0;  mat[3][1] = 0.0;  mat[3][2] = 0.0;  mat[3][3] = 1.0;

   /* Z Rotation */
   for (i=0;i<4;i++) {
      t = mat[i][0];
      mat[i][0] = t*cz - mat[i][1]*sz;
      mat[i][1] = t*sz + mat[i][1]*cz;
   }

   /* X rotation */
   for (i=0;i<4;i++) {
      t = mat[i][1];
      mat[i][1] = t*cx - mat[i][2]*sx;
      mat[i][2] = t*sx + mat[i][2]*cx;
   }

   /* Y Rotation */
   for (i=0;i<4;i++) {
      t = mat[i][0];
      mat[i][0] = mat[i][2]*sy + t*cy;
      mat[i][2] = mat[i][2]*cy - t*sy;
   }

   /* Scale */
   for (i=0;i<3;i++) {
      mat[i][0] *= scale;
      mat[i][1] *= scale;
      mat[i][2] *= scale;
   }

   /* Translation */
   mat[3][0] = transx;
   mat[3][1] = transy;
   mat[3][2] = transz;
}


#define EPS 0.000001

/*** unmake_matrix ******************************************************
   Decompose a transformation matrix into rotation, scale and translation.
**********************************************************************/
void unmake_matrix( float *rotx, float *roty, float *rotz, float *scale,
                    float *transx, float *transy, float *transz,
                    MATRIX  mat )
{
  float  sx, sy, sz, cx, cy, cz;
  int i, j;
  MATRIX nat;

  float scalex, scaley, scalez, scaleinv, cxa, cxb, cxinv;

  /* translation */
  *transx = mat[3][0];
  *transy = mat[3][1];
  *transz = mat[3][2];

  /* scale */
  scalex = scaley = scalez = 0.0;
  for (i=0; i<3; i++) {
    scalex += mat[0][i] * mat[0][i];
    scaley += mat[1][i] * mat[1][i];
    scalez += mat[2][i] * mat[2][i];
  }
  if (fabs(scalex - scaley) > EPS || fabs(scalex - scalez) > EPS) {
    printf("problem1 %f %f %f\n", scalex, scaley, scalez);
  }
  *scale = sqrt((scalex + scaley + scalez)/3.0);
  scaleinv = fabs(*scale) > EPS ? 1.0 / *scale : 1.0 / EPS;

  for (i=0; i<3; i++) {
    for (j=0; j<3; j++) {
      nat[j][i] = scaleinv * mat[j][i];
    }
  }

  /* rotation */
  sx = -nat[2][1];

  cxa = sqrt(nat[2][0]*nat[2][0] + nat[2][2]*nat[2][2]);
  cxb = sqrt(nat[0][1]*nat[0][1] + nat[1][1]*nat[1][1]);

  if (fabs(cxa - cxb) > EPS) {
    printf("problem2 %f %f\n", cxa, cxb);
  }
  /* the sign of cx does not matter;
     it is an ambiguity in 3-D rotations:
     (rotz, rotx, roty) = (180+rotz, 180-rotx, 180+roty) */
  cx = (cxa + cxb) / 2.0;
  if (fabs(cx) > EPS) {
    cxinv = 1.0 / cx;
    sy = nat[2][0] * cxinv;
    cy = nat[2][2] * cxinv;
    sz = nat[0][1] * cxinv;
    cz = nat[1][1] * cxinv;
  }
  else {
    /* if cx == 0 then roty and rotz are ambiguous:
       assume rotx = 0.0 */
    sy = 0.0;
    cy = 1.0;
    sz = nat[0][2];
    cz = nat[1][2];
  }

  *rotx = 57.2957 * atan2(sx, cx);
  *roty = 57.2957 * atan2(sy, cy);
  *rotz = 57.2957 * atan2(sz, cz);
}



/*
 * Compute r = a * b
 */
void mat_mul( MATRIX r, MATRIX a, MATRIX b )
{
   register int i, j, k;
   MATRIX temp;

   for (i=0; i<4; i++) {
      for (j=0; j<4; j++) {
         temp[i][j] = 0.0;
         for (k=0; k<4; k++)
            temp[i][j] += a[i][k] * b[k][j];
      }
   }
   mat_copy(r, temp);
}



/*
 * dest = src
 */
void mat_copy( MATRIX dest, MATRIX src )
{
   memcpy( dest, src, 16*sizeof(float) );
}




static float sub( MATRIX m, int i, int j )
{
   int i1, i2, j1, j2;

   i1 = (i==0) ? 1 : 0;
   i2 = (i==2) ? 1 : 2;
   j1 = (j==0) ? 1 : 0;
   j2 = (j==2) ? 1 : 2;
   return(m[i1][j1] * m[i2][j2] - m[i1][j2] * m[i2][j1]);
}


/*
 * Compute the inverse of a matrix.
 */
void mat_inv( MATRIX inv, MATRIX mat )
{
   int i, j;
   float det;

   mat_copy( inv, mat );
   det = mat[0][0] * sub(mat, 0, 0)
        - mat[1][0] * sub(mat, 1, 0)
        + mat[2][0] * sub(mat, 2, 0);
   for (i=0; i<3; i++) {
      for (j=0; j<3; j++) {
         inv[j][i] = (((i+j)%2) ? -1 : 1) * sub(mat, i, j) / det;
      }
   }
}




/*
 * vout = NORMALIZE(vin)
 */
void vec_norm( float vin[3], float vout[3] )
{
   float len;

   len = sqrt( vin[0]*vin[0] + vin[1]*vin[1] + vin[2]*vin[2] );

   if (len!=0.0) {
      vout[0] = vin[0] / len;
      vout[1] = vin[1] / len;
      vout[2] = vin[2] / len;
   }
   else {
      vout[0] = vout[1] = 0.0;
      vout[2] = 1.0;
   }
}



/*
 * Compute v = v * m
 */
void mat_vecmul( float v[3], MATRIX m )
{
   float xp, yp, zp, wp;
   float vec4[4];
   register float *mp, *vp;
   register int i;

   vec4[0] = v[0];
   vec4[1] = v[1];
   vec4[2] = v[2];
   vec4[3] = 1.0;

   mp = (float *)m;
   vp = (float *)vec4;
   xp = yp = zp = wp = 0.0;
   for (i=0; i<4; i++) {
      xp += *mp++ * *vp;
      yp += *mp++ * *vp;
      zp += *mp++ * *vp;
      wp += *mp++ * *vp;
      vp++;
   }
   printf("wp=%f\n", wp );
   v[0] = xp / wp;
   v[1] = yp / wp;
   v[2] = zp / wp;
}


void mat_vecmul3( float v[3], MATRIX m )
{
   float w[3];

   w[0] = v[0]*m[0][0] + v[1]*m[1][0] + v[2]*m[2][0] + m[3][0];
   w[1] = v[0]*m[0][1] + v[1]*m[1][1] + v[2]*m[2][1] + m[3][1];
   w[2] = v[0]*m[0][2] + v[1]*m[1][2] + v[2]*m[2][2] + m[3][2];

   v[0] = w[0];
   v[1] = w[1];
   v[2] = w[2];
}


void mat_vecmul4( float v[4], MATRIX m )
{
   float w[4];

   w[0] = v[0]*m[0][0] + v[1]*m[1][0] + v[2]*m[2][0] + v[3]*m[3][0];
   w[1] = v[0]*m[0][1] + v[1]*m[1][1] + v[2]*m[2][1] + v[3]*m[3][1];
   w[2] = v[0]*m[0][2] + v[1]*m[1][2] + v[2]*m[2][2] + v[3]*m[3][2];
   w[3] = v[0]*m[0][3] + v[1]*m[1][3] + v[2]*m[2][3] + v[3]*m[3][3];

   v[0] = w[0];
   v[1] = w[1];
   v[2] = w[2];
   v[3] = w[3];
}


void print_matrix( MATRIX mat )
{
   int i,j;

   for (i=0;i<4;i++) {
      for (j=0;j<4;j++)
         printf("%f ", mat[i][j]);
      printf("\n");
   }
}

