/* newproj.c */

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

#include <assert.h>
#include <math.h>
#include <stdio.h>
#include "globals.h"
#include "proj.h"

#define DEG2RAD (M_PI/180.0)
#define RAD2DEG (180.0/M_PI)

/*
Pete and Greg parameters:
     Pete rotated sphere lat 0, lon 0 -> Earth lat a, lon b
     r = East angle between North half of lon = 0 line on Pete rotated
         sphere and lon = b on Earth

coordinates:
    lat p1, lon g1 on Earth
    lat pr, lon gr on Pete rotated sphere
*/

/* Pete rotated sphere to Earth */
pandg_back(pr, gr, p1, g1, a, b, r)
float pr, gr, *p1, *g1, a, b, r;
{
  float pm, gm;

  pm = asin( cos(pr) * cos (gr) );
  gm = atan2(cos(pr) * sin (gr), -sin(pr) );

  *p1 = asin( sin(a) * sin(pm) - cos(a) * cos(pm) * cos (gm - r) );
  *g1 = b + atan2(cos(pm) * sin (gm - r),
                 sin(a) * cos(pm) * cos (gm - r) + cos(a) * sin(pm) );

  return;
}

/* Earth to Pete rotated sphere */
pandg_for(p1, g1, pr, gr, a, b, r)
float *pr, *gr, p1, g1, a, b, r;
{
  float p, g;

  p = asin( sin(a) * sin(p1) + cos(a) * cos(p1) * cos (g1 - b) );
  g = r + atan2(cos(p1) * sin (g1 - b),
                 sin(a) * cos(p1) * cos (g1 - b) - cos(a) * sin(p1) );

  *pr = asin( -cos(p) * cos (g) );
  *gr = atan2(cos(p) * sin (g), sin(p) );

  return;
}



main( argc, argv )
int argc;
char *argv[];
{
  int i;
  float pp, gp, pp1, gp1;
  float a, b, r;

/* test back transformation */

  pp1 = DEG2RAD * atof(argv[1]);
  gp1 = DEG2RAD * atof(argv[2]);

  a = DEG2RAD * atof(argv[3]);
  b = DEG2RAD * atof(argv[4]);
  r = DEG2RAD * atof(argv[5]);

  printf("pp1 = %f  gp1 = %f  a = %f  b = %f  r = %f\n", pp1, gp1, a, b, r);

  pandg_back(pp1, gp1, &pp, &gp, a, b, r);

  printf("pp = %f  gp = %f\n", pp, gp);

  pp = RAD2DEG * pp;
  gp = RAD2DEG * gp;

  printf("degrees: pp = %f  gp = %f\n", pp, gp);

/* test forward transformation */

/*
  pp = DEG2RAD * atof(argv[1]);
  gp = DEG2RAD * atof(argv[2]);

  a = DEG2RAD * atof(argv[3]);
  b = DEG2RAD * atof(argv[4]);
  r = DEG2RAD * atof(argv[5]);

  printf("pp = %f  gp = %f  a = %f  b = %f  r = %f\n", pp, gp, a, b, r);

  pandg_for(pp, gp, &pp1, &gp1, a, b, r);

  printf("pp1 = %f  gp1 = %f\n", pp1, gp1);

  pp1 = RAD2DEG * pp1;
  gp1 = RAD2DEG * gp1;

  printf("degrees: pp1 = %f  gp1 = %f\n", pp1, gp1);
*/
}

