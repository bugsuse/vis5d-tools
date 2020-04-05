/* vtmcP.c */

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
/*
---------|---------|---------|---------|---------|---------|---------|-|
	
	PROGRAM TO CALCULATE MARCHCUBE CUBES

	NEW COMMENTS AND CHANGES: Andre Luiz Battaiola SSEC/INPE

        MODIFIED BY Brian Paul TO BE RE-ENTRANT AND ALLOCATE ALL
           MEMORY DYNAMICALLY.

        BUG FIXES:
	   Checks for invalid polygon when arrays filled.  BEP 2-13-92
-----------------------------------------------------------------------|
*/

#include <string.h>
#include <math.h>
#include <stdio.h>

#ifdef LEVELTYPES
/* This is a kludge! @@
   It was NOT possible to include globals.h here! */ /* API - note */
extern float Height[];
extern int   LevelType;
#  define PRESSURE_LEVELS 100
#  define HEIGHT_LEVELS   105
#endif

#include "etableP.h"
#include "memory.h"


#ifndef TRUE
#define TRUE		( 1 == 1 )
#define FALSE		( 1 == 0 )
#endif

#ifndef	min
#define	min(a,b)	((a < b) ? a : b)
#endif

#ifndef	max
#define	max(a,b)	((a > b) ? a : b)
#endif

#define	mod(a,b)	(a==b ? 0 : a)
#define	rabs(a)		((a >= 0.) ? a : -a)

#define	BIG_NEG		-2e+9
#define	EPS_0		1.0e-5
#define	EPS_1		1.0 - EPS_0
#define	INVALID_VALUE	1.0e30

#define	exist_polygon_in_cube(nc)				\
    (ptFLAG[nc] != 0 & ptFLAG[nc] != 0xFF)


/* API - global to all Vis5D contexts, and DEBUG implies a single work task */

#ifdef DEBUG
static FILE *output;       /* Pointer to the output file */
static void statistics();
#endif





/* ----- Routine to allocate memory space with check */

static void *xalloc( Context ctx, int size, int type )
{
   return (void *) allocate_type( ctx, size, type);
}



/* ----- Routine to deallocate memory space with check */

static void xfree( Context ctx, void *pt )
{
   if (pt != NULL)
     deallocate(ctx, pt, -1);
}





/***********************************************************/
/****                    FLAGS                          ****/
/***********************************************************/


/* ptGRID(x,y,z) -> *( ptGRID + y + x*ydim + z*xdim_x_ydim ) */

#define	value_plane_z0_00    ptAUX[ pcube[ii] ]
#define	value_plane_z0_10    ptAUX[ pcube[ii] + ydim ]
#define	value_plane_z0_01    ptAUX[ pcube[ii] + 1 ]
#define	value_plane_z0_11    ptAUX[ pcube[ii] + ydim + 1 ]
#define	value_plane_z1_00    ptAUX[ pcube[ii] + xdim_x_ydim ]
#define	value_plane_z1_10    ptAUX[ pcube[ii] + ydim + xdim_x_ydim ]
#define	value_plane_z1_01    ptAUX[ pcube[ii] + 1 + xdim_x_ydim ]
#define	value_plane_z1_11    ptAUX[ pcube[ii] + 1 + ydim + xdim_x_ydim ]

#define exist_cube   (cb > -1 && cb < num_cubes)

#define	cZp	cb
#define	cZn	cb
#define	cYp	cb
#define	cYn	cb
#define	cXp	cb
#define	cXn	cb

#define	get_xyz_cube()						\
{   iz = ii / num_cubes_xy;					\
    ix = (int)((ii - (iz * num_cubes_xy)) / num_cubes_y);	\
    iy = ii - (iz * num_cubes_xy) - (ix * num_cubes_y);		\
}

#define	get_cZp()						\
    cb = (iz < (zdim - 1)) ? ii + num_cubes_xy : -1
#define	get_cZn()						\
    cb = (iz > 0) ? ii - num_cubes_xy : -1
#define	get_cXp()						\
    cb = (ix < (xdim - 1)) ? ii + num_cubes_y : -1
#define	get_cXn()						\
    cb = (ix > 0) ? ii - num_cubes_y : -1
#define	get_cYp()						\
    cb = (iy < (ydim - 1)) ? ii + 1 : -1
#define	get_cYn()						\
    cb = (iy > 0) ? ii - 1 : -1

/*
---------|---------|---------|---------|---------|---------|---------|-|
	Routine to Calculate FLAGS used in Marching Cubes Program
	Author: Andre Luiz Battaiola SSEC/INPE
-----------------------------------------------------------------------|
*/


static int flags( float *ptGRID, int xdim, int ydim, int zdim,
                  int ptFLAG[], int ptAUX[], int pcube[], double isovalue )
{
    int	ii, jj, ix, iy, iz, cb, SF, bcase;
    int num_cubes, num_cubes_xy, num_cubes_y;
    int xdim_x_ydim = xdim*ydim;
    int xdim_x_ydim_x_zdim = xdim_x_ydim*zdim;
    int npolygons;

    num_cubes_y  = ydim-1;
    num_cubes_xy = (xdim-1) * num_cubes_y;
    num_cubes = (zdim-1) * num_cubes_xy;

    /*
    *************
    Big Attention
    *************
    pcube must have the dimension "num_cubes+1" because in terms of
    eficiency the best loop to calculate "pcube" will use more one
    component to pcube.
    */

    /* Calculate the Flag Value of each Cube */
    /* In order to simplify the Flags calculations, "pcube" will
       be used to store the number of the first vertex of each cube */
    ii = 0;	pcube[0] = 0;
    for (iz=0; iz<(zdim-1); iz++)
    {	for (ix=0; ix<(xdim-1); ix++)
	{   cb = pcube[ii];
	    for (iy=1; iy<(ydim-1); iy++) /* Vectorized */
		pcube[ii+iy] = cb+iy;
	    ii += ydim-1;
	    pcube[ii] = pcube[ii-1]+2;
	}
	pcube[ii] += ydim;
    }

   /* Vectorized */
    for (ii = 0; ii < xdim_x_ydim_x_zdim; ii++)
	if      (ptGRID[ii] >= INVALID_VALUE) ptAUX[ii] = 0x1001;
	else if (ptGRID[ii] >= isovalue)      ptAUX[ii] = 1;
	else				      ptAUX[ii] = 0;

   /* Vectorized */
    for (ii = 0; ii < num_cubes; ii++)
	ptFLAG[ii] = ((value_plane_z0_00     ) |
		      (value_plane_z0_10 << 1) |
		      (value_plane_z0_01 << 2) |
		      (value_plane_z0_11 << 3) |
		      (value_plane_z1_00 << 4) |
		      (value_plane_z1_10 << 5) |
		      (value_plane_z1_01 << 6) |
		      (value_plane_z1_11 << 7));

    /* After this Point it is not more used pcube */

    /* Analyse Special Cases in FLAG */
    ii = npolygons = 0;
    while ( TRUE )
    {   for (; ii < num_cubes; ii++)
	    if (exist_polygon_in_cube(ii) && ptFLAG[ii] < MAX_FLAG_NUM)
		break;
	if ( ii == num_cubes ) break;

	bcase = pol_edges[ptFLAG[ii]][0];
	if (bcase == 0xE6 || bcase == 0xF9)
	{   get_xyz_cube();
	/* == Z+ == */
	    if      ((ptFLAG[ii] & 0xF0) == 0x90 ||
		     (ptFLAG[ii] & 0xF0) == 0x60) get_cZp();
	/* == Z- == */
	    else if ((ptFLAG[ii] & 0x0F) == 0x09 ||
		     (ptFLAG[ii] & 0x0F) == 0x06) get_cZn();
	/* == Y+ == */
	    else if ((ptFLAG[ii] & 0xCC) == 0x84 ||
		     (ptFLAG[ii] & 0xCC) == 0x48) get_cYp();
	/* == Y- == */
	    else if ((ptFLAG[ii] & 0x33) == 0x21 ||
		     (ptFLAG[ii] & 0x33) == 0x12) get_cYn();
	/* == X+ == */
	    else if ((ptFLAG[ii] & 0xAA) == 0x82 ||
		     (ptFLAG[ii] & 0xAA) == 0x28) get_cXp();
	/* == X- == */
	    else if ((ptFLAG[ii] & 0x55) == 0x41 ||
		     (ptFLAG[ii] & 0x55) == 0x14) get_cXn();
	/* == Map Special Case == */
	    if  (exist_cube && ptFLAG[cb]<316)  /*changed by BEP on 7-20-92*/
	    {   bcase = pol_edges[ptFLAG[cb]][0];
	        if (bcase == 0x06 || bcase == 0x16 ||
		    bcase == 0x19 || bcase == 0x1E ||
		    bcase == 0x3C || bcase == 0x69)
		    ptFLAG[ii] = sp_cases[ptFLAG[ii]];
	    }
	}
	else if (bcase == 0xE9)
	{   get_xyz_cube();
	    if      (ptFLAG[ii] == 0x6B) SF = SF_6B;
	    else if (ptFLAG[ii] == 0x6D) SF = SF_6D;
	    else if (ptFLAG[ii] == 0x79) SF = SF_79;
	    else if (ptFLAG[ii] == 0x97) SF = SF_97;
	    else if (ptFLAG[ii] == 0x9E) SF = SF_9E;
	    else if (ptFLAG[ii] == 0xB6) SF = SF_B6;
	    else if (ptFLAG[ii] == 0xD6) SF = SF_D6;
	    else if (ptFLAG[ii] == 0xE9) SF = SF_E9;
	    for (jj=0; jj<3; jj++)
	    {   if      (case_E9[jj+SF] == Zp) get_cZp();
	        else if (case_E9[jj+SF] == Zn) get_cZn();
	        else if (case_E9[jj+SF] == Yp) get_cYp();
	        else if (case_E9[jj+SF] == Yn) get_cYn();
	        else if (case_E9[jj+SF] == Xp) get_cXp();
	        else if (case_E9[jj+SF] == Xn) get_cXn();
/* changed:
	        if  (exist_cube)
   to: */
	        if  (exist_cube && ptFLAG[cb]<316)
/*changed by BEP on 7-20-92*/
	        {   bcase = pol_edges[ptFLAG[cb]][0];
	            if (bcase == 0x06 || bcase == 0x16 ||
		        bcase == 0x19 || bcase == 0x1E ||
		        bcase == 0x3C || bcase == 0x69)
		    {   ptFLAG[ii] = sp_cases[ptFLAG[ii]] +
				     case_E9[jj+SF+3];
			break;
		    }
	        }
	    }
	}

        /* Calculate the Number of Generated Triangles and Polygons */
     	npolygons  += pol_edges[ptFLAG[ii]][1];
	ii++;
    }

/*    npolygons2 = 2*npolygons;
*/
   return npolygons;
}




/********************************************************/
/***                   MARCHING                      ****/
/********************************************************/


#define	MASK		0x0F

#define	P_F_V(v,t)	Pol_f_Vert[v*9 + t]
#define	V_f_P		Vert_f_Pol

#define	num_polygons_in_cube(nc)	pol_edges[ptFLAG[nc]][1]

#ifdef DEBUG
/* ----- Print Macro Routines - They are acessed only in case of
	 the debug option has been setted
*/
#define	print_new_cube(ncube,caseH)				\
{	if      (caseH == 0x00)	caseA =  0;			\
	else if (caseH == 0x01)	caseA =  1;			\
	else if (caseH == 0x03)	caseA =  2;			\
	else if (caseH == 0x06)	caseA =  3;			\
	else if (caseH == 0x18)	caseA =  4;			\
	else if (caseH == 0x07)	caseA =  5;			\
	else if (caseH == 0x19)	caseA =  6;			\
	else if (caseH == 0x16)	caseA =  7;			\
	else if (caseH == 0x0f)	caseA =  8;			\
	else if (caseH == 0x17)	caseA =  9;			\
	else if (caseH == 0x3c)	caseA = 10;			\
	else if (caseH == 0x1b)	caseA = 11;			\
	else if (caseH == 0x1e)	caseA = 12;			\
	else if (caseH == 0x69)	caseA = 13;			\
	else if (caseH == 0x27)	caseA = 14;			\
	fprintf (output,"\nCube Number:     %d\n",ncube+1);	\
	fprintf (output,					\
	    "Case:            Program = %x, Article = %d\n",	\
			caseH,caseA);				\
}

#define	print_no_polygon()					\
	fprintf (output,"-- no polygons generated --\n")

#define	print_new_polygon( m )					\
	fprintf (output,"Polygon Number:  %d\n",m)

#define	print_vertex( m )					\
	fprintf(output,"%f %f %f\n",VX[m],VY[m],VZ[m])

#endif

/*
---------|---------|---------|---------|---------|---------|---------|---------|
*/

/* v v ----- For one of the axis, this macro calculates the point
	     that the curve will cross the line (one side of  the
	     cube) that join two points of this axis.
	     Example:
	     Suppose: a = x1, b = x2,
		      dx = (x2 - x1) = 1,
		      dn = (node_value_x2 - node_value_x1)
	     so:

	     xcross = ((isovalue - node_value_x1) * dx / dn) + x1  

	     xcross = ((isovalue - node_value_x1) / dn) + x1  
*/
#define calcNode(i1,vi2,vi1)                                    \
{   nodeDiff = vi2 - vi1;					\
    cp = ( ( isovalue - vi1 ) / nodeDiff ) + i1;		\
}
/* ^ ^ ----- end calcNode */ 
    
  /*  
    printf("Old cp = %f  ", (isovalue-node_below)/(node_above-node_below) + gridbox); \
    printf("New cp = %f\n", cp); 				\
  */

#ifdef LEVELTYPES
#define LOGcalcNode(gridbox, node_above, node_below)		\
{   float Pbelow, Pabove, lnPwanted, Pwanted, f;		\
								\
    if (node_above == node_below)				\
       f = 1.0;							\
    else							\
       f = (isovalue - node_below) / (node_above - node_below);	\
								\
    Pbelow = Height[gridbox];					\
    Pabove = Height[gridbox+1];					\
    if (Pbelow == Pabove)					\
       cp = gridbox;						\
    else {							\
       lnPwanted = log(Pbelow) - f*(log(Pbelow) - log(Pabove));	\
       Pwanted = exp(lnPwanted);				\
       cp = (Pbelow - Pwanted) / (Pbelow - Pabove) + gridbox;	\
    }								\
}
/* ^ ^ ----- end LOGcalcNode */ 
#endif

#define	valid_cube(nc)	(ptFLAG[ncube] < MAX_FLAG_NUM)

#define	PlaneX(n,x,y)	ixPlane[n] + x*ydim + y 
#define	PlaneY(n,x,y)	iyPlane[n] + y*xdim + x 
#define	PlaneZ(n,y)	izPlane[n] + y

#define	exist_edge_1	pol_edges[ptFLAG[ncube]][3] & 0x0002
#define	exist_edge_2	pol_edges[ptFLAG[ncube]][3] & 0x0004
#define	exist_edge_3	pol_edges[ptFLAG[ncube]][3] & 0x0008
#define	exist_edge_4	pol_edges[ptFLAG[ncube]][3] & 0x0010
#define	exist_edge_5	pol_edges[ptFLAG[ncube]][3] & 0x0020
#define	exist_edge_6	pol_edges[ptFLAG[ncube]][3] & 0x0040
#define	exist_edge_7	pol_edges[ptFLAG[ncube]][3] & 0x0080
#define	exist_edge_8	pol_edges[ptFLAG[ncube]][3] & 0x0100
#define	exist_edge_9	pol_edges[ptFLAG[ncube]][3] & 0x0200
#define	exist_edge_A	pol_edges[ptFLAG[ncube]][3] & 0x0400
#define	exist_edge_B	pol_edges[ptFLAG[ncube]][3] & 0x0800
#define	exist_edge_C	pol_edges[ptFLAG[ncube]][3] & 0x1000

#define	vnode0		*(pt)
#define	vnode1		*(pt + ydim)
#define	vnode2		*(pt + 1)
#define	vnode3		*(pt + ydim + 1)
#define	vnode4		*(pt + xdim_x_ydim)
#define	vnode5		*(pt + ydim + xdim_x_ydim)
#define	vnode6		*(pt + 1 + xdim_x_ydim)
#define	vnode7		*(pt + 1 + ydim + xdim_x_ydim)

/*
*************
Big Attention
*************
The "ifs" in the "find_vertex" macro was defined supposing that
the cubes are processed first in "y" axes direction, after "x"
axes direction, and finally in "z" direction. If that order is
changed, it's necessary to redefine the "ifs".
Remind that "Fortran" alignes arrays by columns and "C" alignes
by rows.
*/

#define	nvet	nvertex

#define	find_vertex()						\
{    if (exist_edge_1)		/* cube vertex 0-1 */		\
     {   if (iz || iy)  calc_edge[1] = *(PlaneX(bellow,ix,iy));	\
         else							\
	 {   calcNode(ix,vnode1,vnode0);   calc_edge[1] = nvet;	\
 	     VX[nvet] = cp; VY[nvet] = iy; VZ[nvet] = iz;	\
	     nvet++;						\
         }							\
     }								\
     if (exist_edge_2)		/* cube vertex 0-2 */		\
     {   if (iz || ix)  calc_edge[2] = *(PlaneY(bellow,ix,iy));	\
         else							\
	 {   calcNode(iy,vnode2,vnode0);   calc_edge[2] = nvet;	\
             VX[nvet] = ix; VY[nvet] = cp; VZ[nvet] = iz;	\
	     nvet++;						\
	 }							\
     }								\
     if (exist_edge_3)		/* cube vertex 0-4 */		\
     {   if (ix || iy)	calc_edge[3] = *(PlaneZ(rear,iy));	\
         else							\
	 {   calcNode(iz,vnode4,vnode0);   calc_edge[3] = nvet;	\
             VX[nvet] = ix; VY[nvet] = iy; VZ[nvet] = cp;	\
	     nvet++;						\
	 }							\
     }								\
     if (exist_edge_4)		/* cube vertex 1-3 */		\
     {   if (iz)     calc_edge[4] = *(PlaneY(bellow,ix+1,iy));	\
 	     else						\
	 {   calcNode(iy,vnode3,vnode1);   calc_edge[4] = nvet;	\
             VX[nvet] = ix+1; VY[nvet] = cp; VZ[nvet] = iz;	\
	     *(PlaneY(bellow,ix+1,iy)) = nvet;	nvet++;		\
	 }							\
     }								\
     if (exist_edge_5)		/* cube vertex 1-5 */		\
     {   if (iy)	calc_edge[5] = *(PlaneZ(front,iy));	\
         else							\
	 {   calcNode(iz,vnode5,vnode1);   calc_edge[5] = nvet;	\
             VX[nvet] = ix+1; VY[nvet] = iy; VZ[nvet] = cp;	\
	     *(PlaneZ(front,iy)) = nvet;	nvet++;		\
	 }							\
     }								\
     if (exist_edge_6)		/* cube vertex 2-3 */		\
     {   if (iz)   calc_edge[6] = *(PlaneX(bellow,ix,iy+1));	\
         else							\
	 {   calcNode(ix,vnode3,vnode2);   calc_edge[6] = nvet;	\
             VX[nvet] = cp; VY[nvet] = iy+1; VZ[nvet] = iz;	\
	     *(PlaneX(bellow,ix,iy+1)) = nvet;	nvet++;		\
	 }							\
     }								\
     if (exist_edge_7)		/* cube vertex 2-6 */		\
     {   if (ix) 	calc_edge[7] = *(PlaneZ(rear,iy+1));	\
         else							\
	 {   calcNode(iz,vnode6,vnode2);   calc_edge[7] = nvet;	\
             VX[nvet] = ix; VY[nvet] = iy+1; VZ[nvet] = cp;	\
	     *(PlaneZ(rear,iy+1)) = nvet;	nvet++;		\
	 }							\
     }								\
     if (exist_edge_8)		/* cube vertex 3-7 */		\
     {   calcNode(iz,vnode7,vnode3);	   calc_edge[8] = nvet;	\
         VX[nvet] = ix+1;  VY[nvet] = iy+1;  VZ[nvet] = cp;	\
	 *(PlaneZ(front,iy+1)) = nvet;		nvet++;		\
     }								\
     if (exist_edge_9)		/* cube vertex 4-5 */		\
     {   if (iy)	calc_edge[9] = *(PlaneX(above,ix,iy));	\
         else							\
	 {   calcNode(ix,vnode5,vnode4);   calc_edge[9] = nvet;	\
             VX[nvet] = cp; VY[nvet] = iy; VZ[nvet] = iz+1;	\
	     *(PlaneX(above,ix,iy)) = nvet;	nvet++;		\
         }							\
     }								\
     if (exist_edge_A)		/* cube vertex 4-6 */		\
     {   if (ix)       calc_edge[10] = *(PlaneY(above,ix,iy));	\
         else							\
         {   calcNode(iy,vnode6,vnode4);  calc_edge[10] = nvet;	\
             VX[nvet] = ix; VY[nvet] = cp; VZ[nvet] = iz+1;	\
	     *(PlaneY(above,ix,iy)) = nvet;	nvet++;		\
         }							\
     }								\
     if (exist_edge_B)		/* cube vertex 5-7 */		\
     {   calcNode(iy,vnode7,vnode5);	  calc_edge[11] = nvet;	\
         VX[nvet] = ix+1;   VY[nvet] = cp;   VZ[nvet] = iz+1;	\
	 *(PlaneY(above,ix+1,iy)) = nvet;	nvet++;		\
     }								\
     if (exist_edge_C)		/* cube vertex 6-7 */		\
     {   calcNode(ix,vnode7,vnode6);	  calc_edge[12] = nvet;	\
         VX[nvet] = cp;   VY[nvet] = iy+1;   VZ[nvet] = iz+1;	\
	 *(PlaneX(above,ix,iy+1)) = nvet;	nvet++;		\
     }								\
}

#ifdef LEVELTYPES
/* LOGfind_vertex is equal to find_vertex except for the
   macro's called. When interpolation in the Z-coordinate
   must be performed, LOGcalcNode is used. */

#define	LOGfind_vertex()					\
{    if (exist_edge_1)		/* cube vertex 0-1 */		\
     {   if (iz || iy)  calc_edge[1] = *(PlaneX(bellow,ix,iy));	\
         else							\
	 {   calcNode(ix,vnode1,vnode0);   calc_edge[1] = nvet;	\
 	     VX[nvet] = cp; VY[nvet] = iy; VZ[nvet] = iz;	\
	     nvet++;						\
         }							\
     }								\
     if (exist_edge_2)		/* cube vertex 0-2 */		\
     {   if (iz || ix)  calc_edge[2] = *(PlaneY(bellow,ix,iy));	\
         else							\
	 {   calcNode(iy,vnode2,vnode0);   calc_edge[2] = nvet;	\
             VX[nvet] = ix; VY[nvet] = cp; VZ[nvet] = iz;	\
	     nvet++;						\
	 }							\
     }								\
     if (exist_edge_3)		/* cube vertex 0-4 */		\
     {   if (ix || iy)	calc_edge[3] = *(PlaneZ(rear,iy));	\
         else							\
	 {   LOGcalcNode(iz,vnode4,vnode0);   calc_edge[3] = nvet;	\
             VX[nvet] = ix; VY[nvet] = iy; VZ[nvet] = cp;	\
	     nvet++;						\
	 }							\
     }								\
     if (exist_edge_4)		/* cube vertex 1-3 */		\
     {   if (iz)     calc_edge[4] = *(PlaneY(bellow,ix+1,iy));	\
 	     else						\
	 {   calcNode(iy,vnode3,vnode1);   calc_edge[4] = nvet;	\
             VX[nvet] = ix+1; VY[nvet] = cp; VZ[nvet] = iz;	\
	     *(PlaneY(bellow,ix+1,iy)) = nvet;	nvet++;		\
	 }							\
     }								\
     if (exist_edge_5)		/* cube vertex 1-5 */		\
     {   if (iy)	calc_edge[5] = *(PlaneZ(front,iy));	\
         else							\
	 {   LOGcalcNode(iz,vnode5,vnode1);   calc_edge[5] = nvet;	\
             VX[nvet] = ix+1; VY[nvet] = iy; VZ[nvet] = cp;	\
	     *(PlaneZ(front,iy)) = nvet;	nvet++;		\
	 }							\
     }								\
     if (exist_edge_6)		/* cube vertex 2-3 */		\
     {   if (iz)   calc_edge[6] = *(PlaneX(bellow,ix,iy+1));	\
         else							\
	 {   calcNode(ix,vnode3,vnode2);   calc_edge[6] = nvet;	\
             VX[nvet] = cp; VY[nvet] = iy+1; VZ[nvet] = iz;	\
	     *(PlaneX(bellow,ix,iy+1)) = nvet;	nvet++;		\
	 }							\
     }								\
     if (exist_edge_7)		/* cube vertex 2-6 */		\
     {   if (ix) 	calc_edge[7] = *(PlaneZ(rear,iy+1));	\
         else							\
	 {   LOGcalcNode(iz,vnode6,vnode2);   calc_edge[7] = nvet;	\
             VX[nvet] = ix; VY[nvet] = iy+1; VZ[nvet] = cp;	\
	     *(PlaneZ(rear,iy+1)) = nvet;	nvet++;		\
	 }							\
     }								\
     if (exist_edge_8)		/* cube vertex 3-7 */		\
     {   LOGcalcNode(iz,vnode7,vnode3);	   calc_edge[8] = nvet;	\
         VX[nvet] = ix+1;  VY[nvet] = iy+1;  VZ[nvet] = cp;	\
	 *(PlaneZ(front,iy+1)) = nvet;		nvet++;		\
     }								\
     if (exist_edge_9)		/* cube vertex 4-5 */		\
     {   if (iy)	calc_edge[9] = *(PlaneX(above,ix,iy));	\
         else							\
	 {   calcNode(ix,vnode5,vnode4);   calc_edge[9] = nvet;	\
             VX[nvet] = cp; VY[nvet] = iy; VZ[nvet] = iz+1;	\
	     *(PlaneX(above,ix,iy)) = nvet;	nvet++;		\
         }							\
     }								\
     if (exist_edge_A)		/* cube vertex 4-6 */		\
     {   if (ix)       calc_edge[10] = *(PlaneY(above,ix,iy));	\
         else							\
         {   calcNode(iy,vnode6,vnode4);  calc_edge[10] = nvet;	\
             VX[nvet] = ix; VY[nvet] = cp; VZ[nvet] = iz+1;	\
	     *(PlaneY(above,ix,iy)) = nvet;	nvet++;		\
         }							\
     }								\
     if (exist_edge_B)		/* cube vertex 5-7 */		\
     {   calcNode(iy,vnode7,vnode5);	  calc_edge[11] = nvet;	\
         VX[nvet] = ix+1;   VY[nvet] = cp;   VZ[nvet] = iz+1;	\
	 *(PlaneY(above,ix+1,iy)) = nvet;	nvet++;		\
     }								\
     if (exist_edge_C)		/* cube vertex 6-7 */		\
     {   calcNode(ix,vnode7,vnode6);	  calc_edge[12] = nvet;	\
         VX[nvet] = cp;   VY[nvet] = iy+1;   VZ[nvet] = iz+1;	\
	 *(PlaneX(above,ix,iy+1)) = nvet;	nvet++;		\
     }								\
}
#endif

#define	INV_VAL	INVALID_VALUE

#define	find_vertex_invalid_cube(nc)				\
{   ptFLAG[nc] &= 0x1FF;					\
    if (exist_polygon_in_cube(nc)) 				\
    { if (exist_edge_4)		/* cube vertex 1-3 */		\
      {   if (!iz && vnode3 < INV_VAL && vnode1 < INV_VAL)	\
	{     calcNode(iy,vnode3,vnode1);			\
              VX[nvet] = ix+1; VY[nvet] = cp; VZ[nvet] = iz;	\
	      *(PlaneY(bellow,ix+1,iy)) = nvet;	nvet++;		\
	}							\
      }								\
      if (exist_edge_5)		/* cube vertex 1-5 */		\
      {   if (!iy && vnode5 < INV_VAL && vnode1 < INV_VAL)	\
	{     calcNode(iz,vnode5,vnode1);			\
              VX[nvet] = ix+1; VY[nvet] = iy; VZ[nvet] = cp;	\
	      *(PlaneZ(front,iy)) = nvet;	nvet++;		\
	}							\
      }								\
      if (exist_edge_6)		/* cube vertex 2-3 */		\
      {   if (!iz && vnode3 < INV_VAL && vnode2 < INV_VAL)	\
	{     calcNode(ix,vnode3,vnode2);			\
              VX[nvet] = cp; VY[nvet] = iy+1; VZ[nvet] = iz;	\
	      *(PlaneX(bellow,ix,iy+1)) = nvet;	nvet++;		\
	}							\
      }								\
      if (exist_edge_7)		/* cube vertex 2-6 */		\
      {   if (!ix && vnode6 < INV_VAL && vnode2 < INV_VAL)	\
	{     calcNode(iz,vnode6,vnode2);			\
              VX[nvet] = ix; VY[nvet] = iy+1; VZ[nvet] = cp;	\
	      *(PlaneZ(rear,iy+1)) = nvet;	nvet++;		\
	}							\
      }								\
      if (exist_edge_8)		/* cube vertex 3-7 */		\
      {   if (vnode7 < INV_VAL && vnode3 < INV_VAL)		\
          {   calcNode(iz,vnode7,vnode3);			\
              VX[nvet] = ix+1; VY[nvet] = iy+1; VZ[nvet] = cp;	\
	      *(PlaneZ(front,iy+1)) = nvet;	nvet++;		\
	}							\
      }								\
      if (exist_edge_9)		/* cube vertex 4-5 */		\
      {   if (!iy && vnode5 < INV_VAL && vnode4 < INV_VAL)	\
	{     calcNode(ix,vnode5,vnode4);			\
              VX[nvet] = cp; VY[nvet] = iy; VZ[nvet] = iz+1;	\
	      *(PlaneX(above,ix,iy)) = nvet;	nvet++;		\
          }							\
      }								\
      if (exist_edge_A)		/* cube vertex 4-6 */		\
      {   if (!ix && vnode6 < INV_VAL && vnode4 < INV_VAL)	\
          {   calcNode(iy,vnode6,vnode4);			\
              VX[nvet] = ix; VY[nvet] = cp; VZ[nvet] = iz+1;	\
	      *(PlaneY(above,ix,iy)) = nvet;	nvet++;		\
          }							\
      }								\
      if (exist_edge_B)		/* cube vertex 5-7 */		\
      {   if (vnode7 < INV_VAL && vnode5 < INV_VAL)		\
      	{   calcNode(iy,vnode7,vnode5);				\
	      VX[nvet] = ix+1;  VY[nvet] = cp;  VZ[nvet] = iz+1;\
	      *(PlaneY(above,ix+1,iy)) = nvet;	nvet++;		\
	}							\
      }								\
      if (exist_edge_C)		/* cube vertex 6-7 */		\
      {   if (vnode7 < INV_VAL && vnode6 < INV_VAL)		\
	{     calcNode(ix,vnode7,vnode6);			\
	      VX[nvet] = cp;  VY[nvet] = iy+1;  VZ[nvet] = iz+1;\
	      *(PlaneX(above,ix,iy+1)) = nvet;	nvet++;		\
	}							\
      }								\
    }								\
}

#ifdef LEVELTYPES
/* This macro is the same as find_vertex_invalid_cube, except
   for interpolation in the Z-coordinate. This is performed
   logaritmic by calling the macro LOGcalcNode. */
#define	LOGfind_vertex_invalid_cube(nc)				\
{   ptFLAG[nc] &= 0x1FF;					\
    if (exist_polygon_in_cube(nc)) 				\
    { if (exist_edge_4)		/* cube vertex 1-3 */		\
      {   if (!iz && vnode3 < INV_VAL && vnode1 < INV_VAL)	\
	{     calcNode(iy,vnode3,vnode1);			\
              VX[nvet] = ix+1; VY[nvet] = cp; VZ[nvet] = iz;	\
	      *(PlaneY(bellow,ix+1,iy)) = nvet;	nvet++;		\
	}							\
      }								\
      if (exist_edge_5)		/* cube vertex 1-5 */		\
      {   if (!iy && vnode5 < INV_VAL && vnode1 < INV_VAL)	\
	{     LOGcalcNode(iz,vnode5,vnode1);			\
              VX[nvet] = ix+1; VY[nvet] = iy; VZ[nvet] = cp;	\
	      *(PlaneZ(front,iy)) = nvet;	nvet++;		\
	}							\
      }								\
      if (exist_edge_6)		/* cube vertex 2-3 */		\
      {   if (!iz && vnode3 < INV_VAL && vnode2 < INV_VAL)	\
	{     calcNode(ix,vnode3,vnode2);			\
              VX[nvet] = cp; VY[nvet] = iy+1; VZ[nvet] = iz;	\
	      *(PlaneX(bellow,ix,iy+1)) = nvet;	nvet++;		\
	}							\
      }								\
      if (exist_edge_7)		/* cube vertex 2-6 */		\
      {   if (!ix && vnode6 < INV_VAL && vnode2 < INV_VAL)	\
	{     LOGcalcNode(iz,vnode6,vnode2);			\
              VX[nvet] = ix; VY[nvet] = iy+1; VZ[nvet] = cp;	\
	      *(PlaneZ(rear,iy+1)) = nvet;	nvet++;		\
	}							\
      }								\
      if (exist_edge_8)		/* cube vertex 3-7 */		\
      {   if (vnode7 < INV_VAL && vnode3 < INV_VAL)		\
          {   LOGcalcNode(iz,vnode7,vnode3);			\
              VX[nvet] = ix+1; VY[nvet] = iy+1; VZ[nvet] = cp;	\
	      *(PlaneZ(front,iy+1)) = nvet;	nvet++;		\
	}							\
      }								\
      if (exist_edge_9)		/* cube vertex 4-5 */		\
      {   if (!iy && vnode5 < INV_VAL && vnode4 < INV_VAL)	\
	{     calcNode(ix,vnode5,vnode4);			\
              VX[nvet] = cp; VY[nvet] = iy; VZ[nvet] = iz+1;	\
	      *(PlaneX(above,ix,iy)) = nvet;	nvet++;		\
          }							\
      }								\
      if (exist_edge_A)		/* cube vertex 4-6 */		\
      {   if (!ix && vnode6 < INV_VAL && vnode4 < INV_VAL)	\
          {   calcNode(iy,vnode6,vnode4);			\
              VX[nvet] = ix; VY[nvet] = cp; VZ[nvet] = iz+1;	\
	      *(PlaneY(above,ix,iy)) = nvet;	nvet++;		\
          }							\
      }								\
      if (exist_edge_B)		/* cube vertex 5-7 */		\
      {   if (vnode7 < INV_VAL && vnode5 < INV_VAL)		\
      	{   calcNode(iy,vnode7,vnode5);				\
	      VX[nvet] = ix+1;  VY[nvet] = cp;  VZ[nvet] = iz+1;\
	      *(PlaneY(above,ix+1,iy)) = nvet;	nvet++;		\
	}							\
      }								\
      if (exist_edge_C)		/* cube vertex 6-7 */		\
      {   if (vnode7 < INV_VAL && vnode6 < INV_VAL)		\
	{     calcNode(ix,vnode7,vnode6);			\
	      VX[nvet] = cp;  VY[nvet] = iy+1;  VZ[nvet] = iz+1;\
	      *(PlaneX(above,ix,iy+1)) = nvet;	nvet++;		\
	}							\
      }								\
    }								\
}
#endif

#define	num_pol_in_cube(nc)	pol_edges[ptFLAG[nc]][1]
#define	num_edges_per_pol(nc)	pol_edges[ptFLAG[nc]][2]
#define vet_edges_pol(nc)	pol_edges[ptFLAG[nc]][4]

#define	fill_Vert_f_Pol( nc )					\
{   kk  = num_edges_per_pol(nc);				\
    ptl = &vet_edges_pol(nc);					\
    pa  = pvp;							\
    for (ii=0; ii<num_pol_in_cube(nc); ii++)			\
    {   Vert_f_Pol[pa+6] = ve = kk&MASK;    ve+=pa;		\
        for (jj=pa; jj<ve && jj<pa+6; jj++)			\
	    Vert_f_Pol[jj] = *(ptl++);				\
	kk >>= 4;    pa += 7;					\
    }								\
}

#define	update_data_structure( nc )                            	\
{   kk = num_edges_per_pol(nc);					\
    nn = num_pol_in_cube(nc);					\
    for (ii=0; ii<nn; ii++)					\
    {   mm = pvp+(kk&MASK);					\
        for (jj=pvp; jj<mm; jj++)				\
        {   V_f_P[jj] = ve = calc_edge[V_f_P[jj]];		\
            P_F_V(ve,(P_F_V(ve,8))++) = cpl;			\
	}							\
	kk >>= 4;    pvp += 7;    cpl++;			\
    }								\
}

#define	update_data_structure_with_print( nc )                 	\
{   kk = num_edges_per_pol(nc);					\
    nn = num_pol_in_cube(nc);					\
    for (ii=0; ii<nn; ii++)					\
    {   mm = pvp+(kk&MASK);					\
        for (jj=pvp; jj<mm; jj++)				\
        {   V_f_P[jj] = ve = calc_edge[V_f_P[jj]];		\
            P_F_V(ve,(P_F_V(ve,8))++) = cpl;			\
	    print_vertex(ve);					\
	}							\
	kk >>= 4;    pvp += 7;    cpl++;			\
    }								\
}

#define	swap_planes(a,p1,p2)					\
{   caseA = p1;    p1 = p2;    p2 = caseA;    }



static int marching( Context ctx, float ptGRID[], int xdim, int ydim, int zdim,
                     int ptFLAG[], float VX[], float VY[], float VZ[],
                     int NVERTICE, double isovalue, int npolygons,
                     int Pol_f_Vert[], int Vert_f_Pol[] )
{
   int  *ixPlane[2], *iyPlane[2], *izPlane[2];  /* Plane Address Parameters */
   int  ix, iy, iz, caseA, above, bellow, front, rear, mm, nn;
   int  *ptl, ii, jj, kk, ncube, cpl, pvp, pa, ve, calc_edge[13];
   float    cp, *pt;
   double   nodeDiff;
   int xdim_x_ydim = xdim*ydim;
   int nvertex;

    bellow = rear = 0;	above = front = 1;

    /* Initialize the Auxiliar Arrays of Pointers */
    ix = 9 * (npolygons*2 + 50);
    iy = 7 * npolygons;
    ii = ix + iy;
    /*$dir vector */
    for (jj=0; jj<ii; jj++)  Pol_f_Vert[jj] = BIG_NEG;  /* Vectorized */
    /*$dir vector */
    for (jj=8; jj<ix; jj+=9) Pol_f_Vert[jj] = 0;        /* Vectorized */
    /*$dir vector */
    for (jj=6; jj<iy; jj+=7) Vert_f_Pol[jj] = 0;        /* Vectorized */

    /* Allocate the auxiliar edge vectors
    size ixPlane = (xdim - 1) * ydim = xdim_x_ydim - ydim
    size iyPlane = (ydim - 1) * xdim = xdim_x_ydim - xdim
    size izPlane = xdim
    */

    ix = xdim_x_ydim - ydim;
    iy = xdim_x_ydim - xdim;
    iz = ydim;
    ii = 2 * (ix + iy + iz);
    ixPlane[0] = (int *) xalloc( ctx, ii * sizeof(int), IXPLANE_TYPE);
    if (ixPlane[0]==NULL) return -1;
    ixPlane[1] = ixPlane[0] + ix;
    iyPlane[0] = ixPlane[1] + ix;
    iyPlane[1] = iyPlane[0] + iy;
    izPlane[0] = iyPlane[1] + iy;
    izPlane[1] = izPlane[0] + iz;

    /* Calculate the Vertex of the Polygons which edges were
       calculated above */
    pt = ptGRID;  nvertex = ncube = cpl = pvp = 0;

#ifdef DEBUG
        for ( iz = 0; iz < zdim - 1; iz++ )
        {   for ( ix = 0; ix < xdim - 1; ix++ ) 
            {   for ( iy = 0; iy < ydim - 1; iy++ )
		{   print_new_cube(ncube,pol_edges[ptFLAG[ncube]][0]);
		    if (exist_polygon_in_cube(ncube))
		    {   if (nvertex + 12 > NVERTICE) goto end;
		        if (valid_cube(ncube))
		        {   fill_Vert_f_Pol(ncube);
#ifdef LEVELTYPES
		            switch(LevelType) {
			       case PRESSURE_LEVELS:
			          LOGfind_vertex();
			          break;
			       default:
			          find_vertex();
				  break;
			    }
#else
		    	    find_vertex();
#endif
			    update_data_structure_with_print(ncube);
			}
			else
			{
#ifdef LEVELTYPES
		           switch(LevelType) {
		              case PRESSURE_LEVELS:
		                 LOGfind_vertex_invalid_cube(ncube);
			         break;
		              default:
		                 find_vertex_invalid_cube(ncube);
			         break;
                           }
#else
		           find_vertex_invalid_cube(ncube);
#endif
			}
		    }
		    else
			print_no_polygon();
		    ncube++;  pt++;
		}
	        swap_planes(Z,rear,front);  pt++;
	    }
	    swap_planes(XY,bellow,above);  pt += ydim;
	}
#else
        for ( iz = 0; iz < zdim - 1; iz++ )
        {   for ( ix = 0; ix < xdim - 1; ix++ ) 
            {
		for ( iy = 0; iy < ydim - 1; iy++ )
		{   if (exist_polygon_in_cube(ncube))
		    {   if (nvertex + 12 > NVERTICE) goto end;
		        if (valid_cube(ncube))
		        {   fill_Vert_f_Pol(ncube);
#ifdef LEVELTYPES
		            switch(LevelType) {
			       case PRESSURE_LEVELS:
			          LOGfind_vertex();
			          break;
			       default:
			          find_vertex();
				  break;
			    }
#else
		    	    find_vertex();
#endif
			    update_data_structure(ncube);
			}
			else
			{
#ifdef LEVELTYPES
		           switch(LevelType) {
		              case PRESSURE_LEVELS:
		                 LOGfind_vertex_invalid_cube(ncube);
			         break;
		              default:
		                 find_vertex_invalid_cube(ncube);
			         break;
                           }
#else
		           find_vertex_invalid_cube(ncube);
#endif
			}
		    }
		    ncube++; pt++;
		}
	        swap_planes(Z,rear,front);  pt++;
	    }
	    swap_planes(XY,bellow,above);  pt += ydim;
	}
#endif
end:
    xfree ( ctx, (char *) ixPlane[0]);

    return nvertex;
}






/***********************************************************/
/****                  NORMALS                          ****/
/***********************************************************/



#define GV(x,y,z) ptGRID[(int)y + (int)x*ydim + (int)z*xdim_x_ydim]


#define Vertex_k_Pol_i	pVP[k]
#define	V_f_P		Vert_f_Pol

#define	adjust_normal_by_gradiente(v,x,y,z)			\
{   if (VX[v[0]]==VX[v[1]] || VX[v[0]]==VX[v[2]])		\
	ix = (int)VX[v[0]];					\
    else							\
	ix = (int)VX[v[1]];					\
    if (VY[v[0]]==VY[v[1]] || VY[v[0]]==VY[v[2]])		\
	iy = (int)VY[v[0]];					\
    else							\
	iy = (int)VY[v[1]];					\
    if (VZ[v[0]]==VZ[v[1]] || VZ[v[0]]==VZ[v[2]])		\
	iz = (int)VZ[v[0]];					\
    else							\
	iz = (int)VZ[v[1]];					\
    i1 = ix;							\
    if (i1 != ixb) i2 = i1 + 1;					\
    else	     { i2 = i1; i1--; }				\
    if (GV(i2,iy,iz) >= INVALID_VALUE) { i2--; i1--; }		\
    x = GV(i2,iy,iz) - GV(i1,iy,iz);				\
    i1 = iy;							\
    if (i1 != iyb) i2 = i1 + 1;					\
    else	     { i2 = i1; i1--; }				\
    if (GV(ix,i2,iz) >= INVALID_VALUE) { i2--; i1--; }		\
    y = GV(ix,i2,iz) - GV(ix,i1,iz);				\
    i1 = iz;							\
    if (i1 != izb) i2 = i1 + 1;					\
    else	     { i2 = i1; i1--; }				\
    if (GV(ix,iy,i2) >= INVALID_VALUE) { i2--; i1--; }		\
    z = GV(ix,iy,i2) - GV(ix,iy,i1);				\
    a = (x*x + y*y + z*z);					\
    if (a > 0.)  {  x /= a;  y /= a;  z /= a;  }		\
}



static void normals( float ptGRID[], int xdim, int ydim, int zdim,
                     float VX[], float VY[], float VZ[],
                     float NX[], float NY[], float NZ[],
                     int nvertex, int npolygons,
                     float Pnx[], float Pny[], float Pnz[],
                     float NxA[], float NxB[], float NyA[],
                     float NyB[], float NzA[], float NzB[],
                     double arX, double arY, double arZ,
                     int Pol_f_Vert[], int Vert_f_Pol[] )
{
   int	 i, k, iv[3], n;
   int	 i1, i2, ix, iy, iz, ixb, iyb, izb, *pVP;
   int	 max_vert_per_pol, swap_flag;
   float x, y, z, a, minimum_area, len;
   int   xdim_x_ydim = xdim*ydim;

   ixb = xdim-1;  iyb = ydim-1;  izb = zdim-1;

   minimum_area = max(1.e-4,EPS_0);

   /* Calculate maximum number of vertices per polygon */
   k = 6;    n = 7*npolygons;
   while ( TRUE )
   {   for (i=k+7; i<n; i+=7)
	   if (Vert_f_Pol[i] > Vert_f_Pol[k]) break;
       if (i >= n) break;    k = i;
   }
   max_vert_per_pol = Vert_f_Pol[k];

   /* Calculate the Normals vector components for each Polygon */
   pVP = Vert_f_Pol;
   /*$dir vector */
   for ( i=0; i<npolygons; i++) {  /* Vectorized */
      if (pVP[6]>0) {  /* check for valid polygon added by BEP 2-13-92 */
         NxA[i] = VX[pVP[1]] - VX[pVP[0]];
         NyA[i] = VY[pVP[1]] - VY[pVP[0]];
         NzA[i] = VZ[pVP[1]] - VZ[pVP[0]];
      }
      pVP += 7;
   }

   swap_flag = 0;
   for ( k = 2; k < max_vert_per_pol; k++ )
   {
      pVP = Vert_f_Pol;

      if (swap_flag==0) {
         /*$dir no_recurrence */        /* Vectorized */
	 for ( i=0; i<npolygons; i++ ) {
            if (Vertex_k_Pol_i >= 0) {
               NxB[i]  = VX[pVP[k]] - VX[pVP[0]];
	       NyB[i]  = VY[pVP[k]] - VY[pVP[0]];
	       NzB[i]  = VZ[pVP[k]] - VZ[pVP[0]];
	       Pnx[i] = NyA[i]*NzB[i] - NzA[i]*NyB[i]; 
	       Pny[i] = NzA[i]*NxB[i] - NxA[i]*NzB[i]; 
	       Pnz[i] = NxA[i]*NyB[i] - NyA[i]*NxB[i]; 
	       NxA[i] = Pnx[i]*Pnx[i] + Pny[i]*Pny[i] +	Pnz[i]*Pnz[i];
	       if (NxA[i] > minimum_area) {
                  Pnx[i] /= NxA[i];
		  Pny[i] /= NxA[i];
		  Pnz[i] /= NxA[i];
	       }
	    }
	    pVP += 7;
	 }
      }
      else {  /* swap_flag!=0 */
	 /*$dir no_recurrence */        /* Vectorized */
	 for ( i=0; i<npolygons; i++ ) {
            if (Vertex_k_Pol_i >= 0) {
               NxA[i]  = VX[pVP[k]] - VX[pVP[0]];
	       NyA[i]  = VY[pVP[k]] - VY[pVP[0]];
	       NzA[i]  = VZ[pVP[k]] - VZ[pVP[0]];
	       Pnx[i] = NyB[i]*NzA[i] - NzB[i]*NyA[i]; 
	       Pny[i] = NzB[i]*NxA[i] - NxB[i]*NzA[i]; 
	       Pnz[i] = NxB[i]*NyA[i] - NyB[i]*NxA[i]; 
	       NxB[i] = Pnx[i]*Pnx[i] + Pny[i]*Pny[i] +	Pnz[i]*Pnz[i];
	       if (NxB[i] > minimum_area) {
                  Pnx[i] /= NxB[i];
	          Pny[i] /= NxB[i];
	          Pnz[i] /= NxB[i];
	       }
	    }
	    pVP += 7;
         }
      }

       pVP = Vert_f_Pol;
       /* This Loop <CAN'T> be Vectorized */
       for ( i=0; i<npolygons; i++ )
       {   if (Vertex_k_Pol_i >= 0)
	   {   iv[0] = pVP[0];
	       iv[1] = pVP[k-1];
	       iv[2] = pVP[k];
	       if (NxA[i] > minimum_area)
	       {   x = Pnx[i];   y = Pny[i];   z = Pnz[i];   }
	       else
		   adjust_normal_by_gradiente(iv,x,y,z);
	       /* Update the origin vertex */
	       NX[iv[0]] += x;   NY[iv[0]] += y;   NZ[iv[0]] += z;
	       /* Update the vertex that defines the first vector */
	       NX[iv[1]] += x;   NY[iv[1]] += y;   NZ[iv[1]] += z;
	       /* Update the vertex that defines the second vector */
	       NX[iv[2]] += x;   NY[iv[2]] += y;   NZ[iv[2]] += z;
	   }
	   pVP += 7;
       }

       swap_flag = (swap_flag ? 0 : 1 );
   }

   /* Apply Aspect Ratio in the Normals */
   if (arX != 1.0) for (i=0; i<nvertex; i++) NX[i] /= arX;  /* Vectorized */
   if (arY != 1.0) for (i=0; i<nvertex; i++) NY[i] /= arY;  /* Vectorized */
   if (arZ != 1.0) for (i=0; i<nvertex; i++) NZ[i] /= arZ;  /* Vectorized */

   /* Normalize the Normals */
   for ( i=0; i<nvertex; i++ )  /* Vectorized */
   {   len = sqrt(NX[i]*NX[i] + NY[i]*NY[i] + NZ[i]*NZ[i]);
       if (len > EPS_0)
       {   NX[i] /= len;
	   NY[i] /= len;
	   NZ[i] /= len;
       }
   }

}




/**********************************************************/
/****                  TRIANGLES                       ****/
/**********************************************************/



#define	P_f_V		Pol_f_Vert
#define	V_f_P		Vert_f_Pol

#define	swap(a,b)	{   i = a;    a = b;    b = i;    }

#define	find_unselected_pol(p)					\
{   for (p=last_pol; p<npolygons; p++) if (vet_pol[p]) break;	\
    if (p == npolygons) p = -1;					\
    else last_pol = p;						\
}

#define	num_pol_vert(v)  (P_f_V[(v*9)+8])

#define	num_pol(n)	num_pol_vert(Vt[n])

#define	find_vertex_with_pol(v,iv,vt,nv)			\
{   j=num_pol_vert(vt[0]);    v=vt[(iv=0)];			\
    for (i=1; i<nv; i++)					\
	if (j < (k=num_pol_vert(vt[i]))) { v=vt[(iv=i)]; j=k; }	\
}

#define	get_vertices_of_pol(p,vt,nvt)				\
{   nvt = V_f_P[(j=p*7)+6];    vt = V_f_P + j;   }

#define	get_pol_vert(va,vb,np)					\
{  np = -1;							\
   if (va>=0 && vb>=0) {   /* check added by BEP 2-13-92 */	\
     i=va*9;  k=i+P_f_V[i+8];  j=vb*9;  m=j+P_f_V[j+8];		\
     while (i>0 && j>0 && i<k && j<m)				\
     {   if (P_f_V[i] == P_f_V[j] && vet_pol[P_f_V[i]])		\
         {   np=P_f_V[i];    break;   }				\
	 else if (P_f_V[i] < P_f_V[j]) i++;			\
	 else j++;						\
     }								\
   }								\
}

#define	get_ind_vert(vt,nvt,v,iv)				\
    for (iv=0; iv<nvt && vt[iv]!=v; iv++)

#define	update_polygon(p)	vet_pol[p] = FALSE

/* Table of Normal Direction in the Polygon */
static	int	NTAB[] =
{   0,1,2,       1,2,0,       2,0,1,
    0,1,3,2,     1,2,0,3,     2,3,1,0,     3,0,2,1,
    0,1,4,2,3,   1,2,0,3,4,   2,3,1,4,0,   3,4,2,0,1,   4,0,3,1,2,
    0,1,5,2,4,3, 1,2,0,3,5,4, 2,3,1,4,0,5, 3,4,2,5,1,0, 4,5,3,0,2,1,
    5,0,4,1,3,2
};

/* Table of Inverse Direction in the Polygon */
static	int	ITAB[] =
{   0,2,1,       1,0,2,       2,1,0,
    0,3,1,2,     1,0,2,3,     2,1,3,0,     3,2,0,1,
    0,4,1,3,2,   1,0,2,4,3,   2,1,3,0,4,   3,2,4,1,0,   4,3,0,2,1,
    0,5,1,4,2,3, 1,0,2,5,3,4, 2,1,3,0,4,5, 3,2,4,1,5,0, 4,3,5,2,0,1,
    5,4,0,3,1,2
};
    
static	int	STAB[] = { 0, 9, 25, 50 };
/*
---------|---------|---------|---------|---------|---------|---------|-|
	Routine to Grow Poly Triangle Stripe used in Marching
	Cubes Program
	Author: Andre Luiz Battaiola SSEC/INPE
-----------------------------------------------------------------------|
*/

static int poly_triangle_stripe( int vet_pol[], int Tri_Stripe[],
                                 int nvertex, int npolygons,
                                 int Pol_f_Vert[], int Vert_f_Pol[] )
{
   int  i, j, k, m, ii, npol, cpol, *ptT, Nvt,
	vA, vB, ivA, ivB, f_line_conection, iST, last_pol; 
   int  *Vt;

    f_line_conection = FALSE;
    last_pol = 0;
    iST = 0;

    for (i=0; i<npolygons; i++) vet_pol[i] = TRUE;  /* Vectorized */

    while (TRUE)
    {   find_unselected_pol(cpol);
	if (cpol < 0) break;
	update_polygon(cpol);
	get_vertices_of_pol(cpol,Vt,Nvt);

	for (ivA=0; ivA<Nvt; ivA++) 
	{   ivB = mod(ivA+1,Nvt);
            get_pol_vert(Vt[ivA],Vt[ivB],npol);
	    if (npol >= 0) break;
	}
	/* insert polygon alone */
	if (npol < 0)
	{   ptT = NTAB + STAB[Nvt-3];
	    if (iST > 0)
	    {   Tri_Stripe[iST]   = Tri_Stripe[iST-1];    iST++;
	        Tri_Stripe[iST++] = Vt[*ptT];
	    }
            else f_line_conection = 1; /* WLH 3-9-95 added */
	    for (ii=0; ii<min(Nvt,6); ii++)
		Tri_Stripe[iST++] = Vt[*(ptT++)];
	    continue;
        }

	if ((ivB && ivA==(ivB-1)) || (!ivB && ivA==Nvt-1))
	    ptT = ITAB + STAB[Nvt-3] + (ivB+1)*Nvt;
	else
	    ptT = NTAB + STAB[Nvt-3] + (ivB+1)*Nvt;
	if (f_line_conection)
	{   Tri_Stripe[iST]   = Tri_Stripe[iST-1];    iST++;
	    Tri_Stripe[iST++] = Vt[*(ptT-1)];
	    f_line_conection = FALSE;
	}
	for (ii=0; ii<min(Nvt,6); ii++)
	    Tri_Stripe[iST++] = Vt[*(--ptT)];

	vB = Tri_Stripe[iST-1];
	vA = Tri_Stripe[iST-2];
	cpol = npol;

	while (TRUE)
	{   get_vertices_of_pol(cpol,Vt,Nvt);
	    update_polygon(cpol);
	    get_ind_vert(Vt,Nvt,vA,ivA);
	    get_ind_vert(Vt,Nvt,vB,ivB);
	    if ((ivB && ivA==(ivB-1)) || (!ivB && ivA==Nvt-1))
	        ptT = NTAB + STAB[Nvt-3] + ivA*Nvt + 2;
	    else
	        ptT = ITAB + STAB[Nvt-3] + ivA*Nvt + 2;
	    for (ii=2; ii<min(Nvt,6); ii++)
		Tri_Stripe[iST++] = Vt[*(ptT++)];
	     
	    vB = Tri_Stripe[iST-1];
	    vA = Tri_Stripe[iST-2];
	
            get_pol_vert(vA,vB,cpol);
	    if (cpol < 0)
#if defined(PEX)
            {   f_line_conection  = TRUE;
                break;
            }
#else

	    {   vA = Tri_Stripe[iST-3];
		get_pol_vert(vA,vB,cpol);
		if (cpol < 0)
		{   f_line_conection  = TRUE;
		    break;
		}
		else
		{   Tri_Stripe[iST++] = vA;
		    swap(vA,vB);
		}
	    }
#endif
	}
    }

    return iST;
}


/* main_march:
 C-callable entry point.
*/

void main_march( Context ctx, float *ptGRID, int NC, int NR, int NL,
                 int LOWLEV,
                 float GLEV, float ARX, float ARY, float ARZ,
                 int NVERTS, float *VX, float *VY, float *VZ,
                 float *NX,float *NY, float *NZ,
                 int NPTS, int *VPTS, int*IVERT, int *IPTS, int *IPOLY,
                 int *ITRI)
{
   int	 i, NVT;
   int      *ptAUX, *pcube, *vet_pol;
   float    *NxA, *NxB, *NyA, *NyB, *NzA, *NzB, *Pnx, *Pny, *Pnz;
   double   isovalue, arX, arY, arZ;
   int      size_stripe;
   int      *ptFLAG, *Tri_Stripe;
   int      xdim, ydim, zdim, xdim_x_ydim, xdim_x_ydim_x_zdim;
   int      num_cubes, nvertex, npolygons;
   int      NVERTICE;
   int      ix, iy, ii;
   int      *Pol_f_Vert, *Vert_f_Pol;


        isovalue   = (double) GLEV;

        xdim = NC;   ydim = NR;   zdim = NL;
        xdim_x_ydim = xdim * ydim;
        xdim_x_ydim_x_zdim = xdim_x_ydim * zdim;
        num_cubes = (xdim-1) * (ydim-1) * (zdim-1);

	arX = ARX;    arY = ARY;    arZ = ARZ;

        /* ----- Check Grid dimension and Aspect Ratio Parameters */
        if (xdim < 2 || ydim < 2 || zdim < 2 ||
	    rabs(ARX) < EPS_0 || rabs(ARY) < EPS_0 || rabs(ARZ) < EPS_0 ) {
            *IVERT = *IPTS = *IPOLY = *ITRI = 0;
	    return;
	}


        /* ----- Calculate Flags and Number of Polygons */
        ptFLAG = (int *) xalloc( ctx, num_cubes * sizeof(int), PTFLAG_TYPE );
        ptAUX = (int *) xalloc( ctx, xdim_x_ydim_x_zdim * sizeof(int), PTAUX_TYPE );
        pcube = (int *) xalloc( ctx, (num_cubes + 1) * sizeof(int), PCUBE_TYPE );
        if (!ptFLAG || !ptAUX || !pcube) {
            xfree( ctx, ptFLAG );
            xfree( ctx, ptAUX );
            xfree( ctx, pcube );
            *IVERT = *IPTS = *IPOLY = *ITRI = 0;
	    return;
	}

	npolygons = flags( ptGRID, xdim, ydim, zdim, ptFLAG, ptAUX,
                           pcube, isovalue );
        xfree( ctx, pcube );
        xfree( ctx, ptAUX );
	if (npolygons <= 0) {
           xfree( ctx, ptFLAG );
           *IVERT = *IPTS = *IPOLY = *ITRI = 0;
           return;
	}


        /* ----- Find Polygons and Print Statistics */
#ifdef DEBUG
       output = fopen("cubes.out","w");
#endif
       NVERTICE = NVERTS;

       ix = 9 * (npolygons*2 + 50);
       iy = 7 * npolygons;
       ii = ix + iy;
       Pol_f_Vert = (int *) xalloc ( ctx, ii * sizeof(int), POLFVERT_TYPE );
       if (!Pol_f_Vert) {
          xfree( ctx, ptFLAG );
          *IVERT = *IPTS = *IPOLY = *ITRI = 0;
          return;
       }
       Vert_f_Pol = Pol_f_Vert + ix;

       nvertex = marching( ctx, ptGRID, xdim, ydim, zdim, ptFLAG, VX, VY, VZ,
                            NVERTICE, isovalue, npolygons, Pol_f_Vert,
                            Vert_f_Pol );
#ifdef DEBUG
       statistics( isovalue, num_cubes );
       fclose(output);
#endif

        xfree( ctx, ptFLAG );
	if (nvertex <= 0) {
           *IVERT = *IPTS = *IPOLY = *ITRI = 0;
           return;
	}


        /* allocate buffer for normals() */
        NxA = (float *) xalloc( ctx, 6 * npolygons * sizeof(float), NXA_TYPE );
        NxB = NxA + npolygons;
        NyA = NxB + npolygons;
	NyB = NyA + npolygons;
	NzA = NyB + npolygons;
	NzB = NzA + npolygons;
	Pnx = (float *) xalloc( ctx, 3 * npolygons * sizeof(float), PNX_TYPE );
	Pny = Pnx + npolygons;
	Pnz = Pny + npolygons;
        if (!NxA || !Pnx) {
           xfree( ctx, NxA );
           xfree( ctx, Pnx );
           xfree( ctx, Pol_f_Vert );
           *IVERT = *IPTS = *IPOLY = *ITRI = 0;
           return;
	}

        /* ----- Find Normals */
        memset( NX, 0, nvertex*sizeof(int) );
        memset( NY, 0, nvertex*sizeof(int) );
        memset( NZ, 0, nvertex*sizeof(int) );

	normals( ptGRID, xdim, ydim, zdim, VX,VY,VZ,NX,NY,NZ,
                 nvertex, npolygons, Pnx, Pny, Pnz, NxA,NxB, NyA,NyB, NzA,NzB,
                 arX, arY, arZ, Pol_f_Vert, Vert_f_Pol );

        xfree( ctx, NxA );
        xfree( ctx, Pnx );

	/* ----- Map Vertex in Output Vectors */
	/*       Considere the Aspect Ratio in the Vertices Values */
	/*       Observe that AR is considered in Normals Calculat.*/
	/*
	if (rabs(1.0 - arX) > EPS_0)
	    for (i=0; i<nvertex; i++) VX[i] *= arX;
    	if (rabs(1.0 - arY) > EPS_0)
	    for (i=0; i<nvertex; i++) VY[i] *= arY;
	if (rabs(1.0 - arZ) > EPS_0)
	    for (i=0; i<nvertex; i++) VZ[i] *= arZ;
	*/
	if (arX != 1.0)
	    for (i=0; i<nvertex; i++) VX[i] *= arX;  /* Vectorized */
	if (arY != 1.0)
	    for (i=0; i<nvertex; i++) VY[i] *= arY;  /* Vectorized */
        if (LOWLEV != 0.0)
	    for (i=0; i<nvertex; i++) VZ[i] += LOWLEV;  /* Vectorized */
	if (arZ != 1.0)
	    for (i=0; i<nvertex; i++) VZ[i] *= arZ;  /* Vectorized */


        /* ----- Find PolyTriangle Stripe */
        Tri_Stripe = (int *) xalloc( ctx, 6*npolygons * sizeof(int), TRISTRIPE_TYPE );
        vet_pol = (int *) xalloc( ctx, npolygons * sizeof(int), VETPOL_TYPE );
        if (!Tri_Stripe || !vet_pol) {
           xfree( ctx, Tri_Stripe );
           xfree( ctx, vet_pol );
           xfree( ctx, Pol_f_Vert );
           *IVERT = *IPTS = *IPOLY = *ITRI = 0;
           return;
	}


	size_stripe = poly_triangle_stripe( vet_pol, Tri_Stripe, nvertex,
                                        npolygons, Pol_f_Vert, Vert_f_Pol );
	NVT = min(NPTS,size_stripe);
        memcpy( VPTS, Tri_Stripe, NVT*sizeof(int) );

	/* ----- Realese Memory */
        xfree( ctx, Pol_f_Vert);	/* Free Pol_f_Vert, Vert_f_Pol*/
        xfree( ctx, vet_pol );
        xfree( ctx, Tri_Stripe );

	/* ----- Update Output values */
        *IVERT = nvertex;
	*IPTS  = size_stripe;
	*IPOLY = npolygons;
        *ITRI  = 0;

    }
/* ^ ^ ----- end of the main routine */ 






/*
---------|---------|---------|---------|---------|---------|---------|--
	This is the main routine where:
	- it's done a parsing of the input arguments
	- it's called the routine to calculate the Marching Cubes
	- And if necessary is printed the statistics of the
          Marching Cube process
*/ 

void
marchcube_( Context ctx, float GRID[], int *NC, int *NR, int *NL,
            float *GLEV, int *DEBUG, float *ARX, float *ARY, float *ARZ,
            int *NVERTS,
            float VX[], float VY[], float VZ[],
            float NX[], float NY[], float NZ[],
            int *NPTS, int VPTS[], int *IVERT,
            int *IPTS, int *IPOLY, int *ITRI )
{
   main_march( ctx, GRID, *NC, *NR, *NL, 0, *GLEV, *ARX, *ARY, *ARZ,
	       *NVERTS, VX,VY,VZ,NX,NY,NZ,
               *NPTS, VPTS, IVERT,IPTS,IPOLY,ITRI);
}





/*
---------|---------|---------|---------|---------|---------|---------|-|
	Routine to print statistics of the cube.
	That statistics include:
	   - tally[f]	-> number of ocorrunces of the "flag case f"
	   - nvertex	-> number of vertex generated
	   - npolygons	-> number of polygons generated
	   - num_cubes	-> number of cubes in the matrix
*/

#ifdef DEBUG
#define	TOTFLAGS	268

static	void
statistics( double isovalue, int num_cubes )
{
    int		i, total, vt_flags[TOTFLAGS];
    float	nc;

    fprintf( output, "\n== Statistics ==\n" );

    fprintf( output, "Isovalue:           %9.3e\n", isovalue );
    fprintf( output, "Nodes generated:    %7d\n", nvertex );
    fprintf( output, "Polygons generated: %7d\n", npolygons );
    fprintf( output, "Cubes in matrix:    %7d\n", num_cubes );

    nc = num_cubes / 100.0;
    for (i=0; i<TOTFLAGS; i++) vt_flags[i] = 0;
    for (i=0; i < num_cubes; i++) vt_flags[ptFLAG[i]]++;
    for (i=1, total=0; i < TOTFLAGS && i != 255; i++)
	total += vt_flags[i];

    fprintf( output, "\n  case               count percent\n" );
    for ( i = 0; i < TOTFLAGS; i++ )
        if ( vt_flags[i] )
        {   fprintf( output,"  %3d (%02x -> %02x): %7d  (%5.2f%%)\n",
			       i, i, pol_edges[i][0],
	                       vt_flags[i], (float)vt_flags[i]/nc );
	}
    fprintf( output, "  total         : %7d (100.00%%)\n\n",
	     num_cubes);

    fprintf( output, "  total cases #0: %7d (%5.2f%%)\n",
	     total, (float)total / nc );
    return;
  }
/* ^ ^ ----- end of the statistics routine */ 

#endif


