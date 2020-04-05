
/* Vis5D version 5.2 */

/*
Vis5D system for visualizing five dimensional gridded data sets
Copyright (C) 1990 - 2000 Bill Hibbard, Brian Paul, Dave Santek,
and Andre Battaiola.

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



#ifndef MATRIX_H
#define MATRIX_H 1



/* Define the type Matrix. */
typedef float MATRIX[4][4];



/*
 * Make a transformation matrix given rotations, scaling, and a translation.
 */
extern void make_matrix( float rotx, float roty, float rotz,
                         float scale,
                         float transx, float transy, float transz, 
                         MATRIX mat );

extern void unmake_matrix( float *rotx, float *roty, float *rotz,
                         float *scale,
                         float *transx, float *transy, float *transz,
                         MATRIX mat );

/*
 * Compute  r = a * b
 */
extern void mat_mul( MATRIX r, MATRIX a, MATRIX b );


/*
 * dest = src
 */
extern void mat_copy( MATRIX dest, MATRIX src );



/*
 * Compute the inverse of a matrix.
 */
void mat_inv( MATRIX inv, MATRIX mat );



/*
 * vout = NORMALIZE(vin)
 */
extern void vec_norm( float vin[3], float vout[3] );



/*
 * Compute v = v * m
 */
extern void mat_vecmul( float v[3], MATRIX m );


extern void mat_vecmul3( float v[3], MATRIX m );


extern void mat_vecmul4( float v[4], MATRIX m );


extern void print_matrix( MATRIX mat );


#endif
