/*  linterp.c  */


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
/* line and interp module */



#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "globals.h"
#include "topo.h"



#define		INTERP_FUZZ			1.0e-05
#define		INTERP_TRIANGULAR_GRID_SCHEME	1



typedef float	FLOAT2[2];





/* 20Aug97  Phil McDonald */
/******************************************************************************
 ******************************************************************************
 *
 *  The following are a bunch of functions for dealing with 2D lines.
 *  They are included so that line segments may be regridded to a
 *  triangular grid, like the one that is produced when the topography
 *  is rendered as a series of polytriangle strips.  This allows the
 *  lines to closely follow the surface of the grid.
 *
 ******************************************************************************
 ******************************************************************************
 */



/*****************************************************************************/

int	line2d_eqn (float *p_xy1, float *p_xy2, double abc[3])

{
    register double	x1, y1, x2, y2, dx, dy, d;

    x1 = p_xy1[0], y1 = p_xy1[1];
    x2 = p_xy2[0], y2 = p_xy2[1];

    dx = x2 - x1, dy = y2 - y1;

    if (dy == 0.0)
    {
        if (dx == 0.0)
        {
            abc[0] = abc[1] = abc[2] = 0.0;
            return 0;
        }
        if (dx > 0.0) dx = -dx;
    }
    else if (dy < 0.0)
    {
        dy = -dy, dx = -dx;
    }
    d = sqrt ((dx * dx) + (dy * dy));
    dx /= d, dy /= d;

    abc[0] =  dy;
    abc[1] = -dx;
    abc[2] = (y1 * dx) - (x1 * dy);


    return 1;
}



/*****************************************************************************/

int	line2d_int (double abc1[3], double abc2[3], float *p_xy)

{

    register double	a1, b1, c1, a2, b2, c2, ab, ba, bc, cb, xx;

    a1 = abc1[0], b1 = abc1[1], c1 = abc1[2];
    a2 = abc2[0], b2 = abc2[1], c2 = abc2[2];

    ab = a1 * b2;
    ba = b1 * a2;

    if (ab == ba)
    {
        p_xy[0] = p_xy[1] = 0.0;
        return 0;
    }

    bc = b1 * c2;
    cb = c1 * b2;

    xx = (bc - cb) / (ab - ba);

    p_xy[0] = xx;
    p_xy[1] = (fabs (b1) > fabs (b2)) ? ((xx * a1) + c1) / (-b1) :
                                        ((xx * a2) + c2) / (-b2);


    return 1;
}



/*****************************************************************************/

static int	line2d_regrid_add_pt (FLOAT2 xy, FLOAT2 *xy_new, int *p_nnew)

{

    int		nnew = *p_nnew;

    xy_new[nnew][0] = xy[0];
    xy_new[nnew][1] = xy[1];
    (*p_nnew)++;


    return *p_nnew;
}



static int	line2d_regrid_find_ints (double abc1[3], double abc2[3],
		                         float xy1, float xy2,
		                         FLOAT2 *xy_new, int *p_nnew)

{

    register int	i, i1, i2;
    register double	x1, x2, dx, dd;
    FLOAT2		xy;


    if (xy1 == xy2) return 0;

    if (xy1 < xy2)
    {
        x1 = xy1, x2 = xy2;
    }
    else
    {
        x1 = xy2, x2 = xy1;
    }

    i1 = (x1 < 0.0) ? x1 : x1 + 1.0;
    i2 = (x2 > 0.0) ? x2 : x2 - 1.0;
    dx = x2 - x1;
    dd = ((double) i1) - x1;

    for (i = i1; i <= i2; i++)
    {
        abc2[2] = i;

        if (line2d_int (abc1, abc2, xy))
        {
            line2d_regrid_add_pt (xy, xy_new, p_nnew);
        }
    }


    return 1;
}



int	line2d_regrid (FLOAT2 *xy_old, int nold, int grid_scheme,
		       FLOAT2 **p_xy_new, int *p_nnew)

{

    int			i, j, ii, jj, nnew, ibeg, ichk;
    float               x, y;
    double              abc1[3], abc2[3];
    FLOAT2              *xy_new, xy;


    *p_nnew   = 0;
    *p_xy_new = NULL;

    nnew   = 0;
    xy_new = (FLOAT2 *) calloc (1000, sizeof (FLOAT2));

    for (j = 0, i = 1; i < nold; j = i++)
    {
        ibeg = nnew;

        xy[0] = xy_old[j][0],
        xy[1] = xy_old[j][1];
        line2d_regrid_add_pt (xy, xy_new, &nnew);

        if (line2d_eqn (xy_old[j], xy_old[i], abc1))
        {
            if (abc1[1] != 0.0)	/* not vert -- find X grid ints */
            {
                abc2[0] = -1.0, abc2[1] = 0.0;

                line2d_regrid_find_ints (abc1, abc2,
                                         xy_old[j][0], xy_old[i][0],
                                         xy_new, &nnew);
            }

            if (abc1[0] != 0.0)	/* not horiz -- find Y grid ints */
            {
                abc2[0] = 0.0, abc2[1] = -1.0;

                line2d_regrid_find_ints (abc1, abc2,
                                         xy_old[j][1], xy_old[i][1],
                                         xy_new, &nnew);
            }

            if (grid_scheme != 0)	/* find diag grid ints */
            {
                if (grid_scheme < 0)	/* find Y = -X grid ints */
                {
                    if (abc1[0] != abc1[1])
                    {
                        abc2[0] = -1.0, abc2[1] = -1.0;

                        x = xy_old[j][0] + xy_old[j][1];
                        y = xy_old[i][0] + xy_old[i][1];

                        line2d_regrid_find_ints (abc1, abc2, x, y,
                                                 xy_new, &nnew);
                    }
                }
                else			/* find Y = X grid ints */
                {
                    if (abc1[0] != -abc1[1])
                    {
                        abc2[0] = -1.0, abc2[1] = 1.0;

                        x = xy_old[j][0] - xy_old[j][1];
                        y = xy_old[i][0] - xy_old[i][1];

                        line2d_regrid_find_ints (abc1, abc2, x, y,
                                                 xy_new, &nnew);
                    }
                }
            }

            xy[0] = xy_old[i][0];
            xy[1] = xy_old[i][1];
            line2d_regrid_add_pt (xy, xy_new, &nnew);


/*
 *  Sort the points (usually by X)
 */

            ichk = (abc1[0] != 1.0) ? 0 : 1;

            if (xy_old[j][ichk] < xy_old[i][ichk])
            {
                for (jj = ibeg; jj < nnew - 1; jj++)
                {
                    for (ii = jj + 1; ii < nnew; ii++)
                    {
                        if (xy_new[jj][ichk] > xy_new[ii][ichk])
                        {
                            x              = xy_new[jj][0];
                            xy_new[jj][0] = xy_new[ii][0];
                            xy_new[ii][0] = x;
                            y              = xy_new[jj][1];
                            xy_new[jj][1] = xy_new[ii][1];
                            xy_new[ii][1] = y;
                        }
                    }
                }
            }
            else
            {
                for (jj = ibeg; jj < nnew - 1; jj++)
                {
                    for (ii = jj + 1; ii < nnew; ii++)
                    {
                        if (xy_new[jj][ichk] < xy_new[ii][ichk])
                        {
                            x              = xy_new[jj][0];
                            xy_new[jj][0] = xy_new[ii][0];
                            xy_new[ii][0] = x;
                            y              = xy_new[jj][1];
                            xy_new[jj][1] = xy_new[ii][1];
                            xy_new[ii][1] = y;
                        }
                    }
                }
            }

            ii = ibeg;
            for (jj = ibeg; jj < nnew; jj++)
            {
                if (xy_new[jj][ichk] == xy_old[j][ichk])
                {
                    xy_new[ii][0] = xy_new[jj][0];
                    xy_new[ii][1] = xy_new[jj][1];
                    break;
                }
            }
            while (++jj < nnew)
            {
                if (xy_new[jj][ichk] != xy_new[ii][ichk])
                {
                    ii++;
                    xy_new[ii][0] = xy_new[jj][0];
                    xy_new[ii][1] = xy_new[jj][1];
                }
                if (xy_new[jj][ichk] == xy_old[i][ichk])
                {
                    ii++;
                    break;
                }
            }

            nnew = ii;
        }
    }

    *p_nnew   = nnew;
    *p_xy_new = xy_new;


    return 1;
}



/*****************************************************************************/




float	interp_tri (float *z, float x, float y, int grid_scheme)

{

    register double	xx, yy, z0, z1, z2, z3, za, zb, zc;


    xx = (x < INTERP_FUZZ) ? 0.0 : (x > (1.0 - INTERP_FUZZ)) ? 1.0 : x;
    yy = (y < INTERP_FUZZ) ? 0.0 : (y > (1.0 - INTERP_FUZZ)) ? 1.0 : y;

    if (xx == 0.0)
    {
        if (yy == 0.0) return z[0];
        if (yy == 1.0) return z[2];
    }
    else if (xx == 1.0)
    {
        if (yy == 0.0) return z[1];
        if (yy == 1.0) return z[3];
    }

    if (grid_scheme > 0)
    {
        z0 = z[0];
        z1 = z[1];
        z2 = z[3];
        z3 = z[2];
    }
    else
    {
        xx = 1.0 - xx;
        z0 = z[1];
        z1 = z[0];
        z2 = z[2];
        z3 = z[3];
    }

    if (yy <= xx)
        za = z0, zb = z1, zc = z2;
    else
        za = z2, zb = z3, zc = z0, xx = 1.0 - xx, yy = 1.0 - yy;

    z0 = za + ((zb - za) * xx);
    z1 = za + ((zc - za) * xx);


    return z0 + ((z1 - z0) * (yy / xx));
}



/* 20Aug97  Phil McDonald */
/*
 *  Return the interpolated vertex Z value for a point at XCOL,YROW
 *  within the provided array of vertices.
 */
float	interp_z (float *verts, int ncols, int nrows, int grid_scheme,
                  float xcol, float yrow)

{

    int		i, ir, irowa, irowb, mxrow, ic, icola, icolb, mxcol, irc;
    float	x, y, z[4];



    if (verts == NULL) return 0.0;


    mxcol = ncols - 1;
    x     = (xcol < 0.0) ? 0.0 : (xcol > (float) mxcol) ? mxcol : xcol;
    icola = icolb = x;
    if (x > (float) icolb) icolb++;
    x -= (float) icola;

    mxrow = nrows - 1;
    y     = (yrow < 0.0) ? 0.0 : (yrow > (float) mxrow) ? mxrow : yrow;
    irowa = irowb = y;
    if (y > (float) irowb) irowb++;
    y -= (float) irowa;

    i = 0;
    for (ir = irowa; ir <= irowb; ir++)
    {
        for (ic = icola; ic <= icolb; ic++)
        {
            irc    = (ir * ncols) + ic;
            z[i++] = verts[(irc*3)+2];
        }
    }

    if (irowa == irowb)
    {
        if (icola == icolb) return z[0];
        z[2] = z[0];
        z[3] = z[1];
    }
    else if (icola == icolb)
    {
        z[2] = z[1];
        z[1] = z[0];
        z[3] = z[2];
    }


    return interp_tri (z, x, y, grid_scheme);
}
