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


#include <math.h>
#include "api.h"
#include "box.h"
#include "graphics.h"
#include "globals.h"
#include "proj.h"
#include "render.h"



#define ADD_VERTEX( X, Y, Z )  if (dtx->NumBoxVerts<MAX_BOX_VERTS) {       \
                                  dtx->BoxVerts[dtx->NumBoxVerts][0] = X;  \
                                  dtx->BoxVerts[dtx->NumBoxVerts][1] = Y;  \
                                  dtx->BoxVerts[dtx->NumBoxVerts][2] = Z;  \
                                  dtx->NumBoxVerts++;                      \
                                }

#define END_OF_LINE    if (dtx->NumBoxVerts<MAX_BOX_VERTS) {            \
                          dtx->BoxVerts[dtx->NumBoxVerts][0] = -999.0;  \
                          dtx->NumBoxVerts++;                           \
                       }


#define VERT(Z) (dtx->VerticalSystem==VERT_NONEQUAL_MB ? height_to_pressure(Z) : (Z))


/*
 * Compute vertices for the 3-D box.
 * This function must be called AFTER the dtx->Nr,dtx->Nc,Nl variables have
 * been initialized.
 * Input:  prop - code for box proportions:
 *                1 = scale box according to lat, lons.
 *                2 = scale according to ax, ay, az.
 *           ax, ay, az - aspect ratio numbers used if prop==3.
 */
static void make_square_box( Display_Context dtx )
{
   dtx->NumBoxVerts = 0;
   dtx->TickMarks = 0;

   /* bottom rectangle */
   ADD_VERTEX( dtx->Xmin, dtx->Ymax, dtx->Zmin );
   ADD_VERTEX( dtx->Xmax, dtx->Ymax, dtx->Zmin );
   ADD_VERTEX( dtx->Xmax, dtx->Ymin, dtx->Zmin );
   ADD_VERTEX( dtx->Xmin, dtx->Ymin, dtx->Zmin );
   ADD_VERTEX( dtx->Xmin, dtx->Ymax, dtx->Zmin );
   END_OF_LINE;

   /* top rectangle */
   ADD_VERTEX( dtx->Xmin, dtx->Ymax, dtx->Zmax );
   ADD_VERTEX( dtx->Xmax, dtx->Ymax, dtx->Zmax );
   ADD_VERTEX( dtx->Xmax, dtx->Ymin, dtx->Zmax );
   ADD_VERTEX( dtx->Xmin, dtx->Ymin, dtx->Zmax );
   ADD_VERTEX( dtx->Xmin, dtx->Ymax, dtx->Zmax );
   END_OF_LINE;

   /* top-to-bottom lines */
   ADD_VERTEX( dtx->Xmax, dtx->Ymax, dtx->Zmax );
   ADD_VERTEX( dtx->Xmax, dtx->Ymax, dtx->Zmin );
   END_OF_LINE;
   ADD_VERTEX( dtx->Xmax, dtx->Ymin, dtx->Zmax );
   ADD_VERTEX( dtx->Xmax, dtx->Ymin, dtx->Zmin );
   END_OF_LINE;
   ADD_VERTEX( dtx->Xmin, dtx->Ymin, dtx->Zmax );
   ADD_VERTEX( dtx->Xmin, dtx->Ymin, dtx->Zmin );
   END_OF_LINE;
   ADD_VERTEX( dtx->Xmin, dtx->Ymax, dtx->Zmax );
   ADD_VERTEX( dtx->Xmin, dtx->Ymax, dtx->Zmin );
   END_OF_LINE;

   /* arrow */
   ADD_VERTEX(  0.025, dtx->Ymax+0.05, dtx->Zmin );
   ADD_VERTEX(  0.0,   dtx->Ymax+0.1,  dtx->Zmin );
   ADD_VERTEX( -0.025, dtx->Ymax+0.05, dtx->Zmin );
   END_OF_LINE;
   ADD_VERTEX(  0.0,   dtx->Ymax,      dtx->Zmin );
   ADD_VERTEX(  0.0,   dtx->Ymax+0.1,  dtx->Zmin );
   END_OF_LINE;

   /* N */
   ADD_VERTEX( -0.025, dtx->Ymax+0.15, dtx->Zmin );
   ADD_VERTEX( -0.025, dtx->Ymax+0.25, dtx->Zmin );
   ADD_VERTEX(  0.025, dtx->Ymax+0.15, dtx->Zmin );
   ADD_VERTEX(  0.025, dtx->Ymax+0.25, dtx->Zmin );
   END_OF_LINE;

   if (dtx->Projection==PROJ_GENERIC || dtx->Projection==PROJ_LINEAR) {
      /* East tick mark */
      ADD_VERTEX( dtx->Xmax, dtx->Ymin, dtx->Zmin );
      ADD_VERTEX( dtx->Xmax, dtx->Ymin-0.05, dtx->Zmin-0.05);
      END_OF_LINE;
      /* West */
      ADD_VERTEX( dtx->Xmin, dtx->Ymin, dtx->Zmin );
      ADD_VERTEX( dtx->Xmin, dtx->Ymin-0.05, dtx->Zmin-0.05 );
      END_OF_LINE;
      /* North */
      ADD_VERTEX( dtx->Xmin, dtx->Ymax, dtx->Zmin );
      ADD_VERTEX( dtx->Xmin-0.05, dtx->Ymax, dtx->Zmin-0.05 );
      END_OF_LINE;
      /* South */
      ADD_VERTEX( dtx->Xmin, dtx->Ymin, dtx->Zmin );
      ADD_VERTEX( dtx->Xmin-0.05, dtx->Ymin, dtx->Zmin-0.05 );
      END_OF_LINE;
      /* Top */
      ADD_VERTEX( dtx->Xmin, dtx->Ymin, dtx->Zmax );
      ADD_VERTEX( dtx->Xmin-0.05, dtx->Ymin-0.05, dtx->Zmax );
      END_OF_LINE;
      /* Bottom */
      ADD_VERTEX( dtx->Xmin, dtx->Ymin, dtx->Zmin );
      ADD_VERTEX( dtx->Xmin-0.05, dtx->Ymin-0.05, dtx->Zmin );
      END_OF_LINE;
      dtx->TickMarks = 1;
   }
   else {
      dtx->TickMarks = 0;
   }
}




/*
 * Make the box used to bound a cylindrical projection.
 */
static void make_cylinder_box( Display_Context dtx )
{
   float row, col, lev;
   int r, c, l;
   float x, y, z;
   int i;

   dtx->NumBoxVerts = 0;
   dtx->TickMarks = 0;

   /* curved edges */
   for (i=0;i<4;i++) {
      switch (i) {
         case 0:
            /* bottom south edge */
            r = dtx->Nr-1;
            l = 0;
            break;
         case 1:
            /* top south edge */
            r = dtx->Nr-1;
            l = dtx->MaxNl-1;
            break;
         case 2:
            /* top north edge */
            r = 0;
            l = dtx->MaxNl-1;
            break;
         case 3:
            /* bottom north edge */
            r = 0;
            l = 0;
            break;
      }

      for (c=0;c<dtx->Nc;c++) {
         row = (float) r;
         col = (float) c;
         lev = (float) l;
         gridPRIME_to_xyzPRIME( dtx, 0, 0, 1,  &row, &col, &lev, &x, &y, &z );
         /*grid_to_xyz( dtx, 0, dtx->MaxNlVar, 1, &row, &col, &lev, &x, &y, &z ); */
         ADD_VERTEX( x, y, z );
      }
      END_OF_LINE;
   }

   /* top east edge */
   row = 0;
   col = dtx->Nc-1;
   lev = dtx->MaxNl-1;
   gridPRIME_to_xyzPRIME( dtx, 0, 0, 1,  &row, &col, &lev, &x, &y, &z );
   /*grid_to_xyz( dtx, 0, dtx->MaxNlVar, 1, &row, &col, &lev, &x, &y, &z ); */
   ADD_VERTEX( x, y, z );
   row = dtx->Nr-1;
   gridPRIME_to_xyzPRIME( dtx, 0, 0, 1,  &row, &col, &lev, &x, &y, &z );
   /*grid_to_xyz( dtx, 0, dtx->MaxNlVar, 1, &row, &col, &lev, &x, &y, &z ); */
   ADD_VERTEX( x, y, z );
   END_OF_LINE;

   /* bottom east edge */
   row = 0;
   col = dtx->Nc-1;
   lev = 0;
   gridPRIME_to_xyzPRIME( dtx, 0, 0, 1,  &row, &col, &lev, &x, &y, &z );
   ADD_VERTEX( x, y, z );
   row = dtx->Nr-1;
   gridPRIME_to_xyzPRIME( dtx, 0, 0, 1,  &row, &col, &lev, &x, &y, &z );
   ADD_VERTEX( x, y, z );
   END_OF_LINE;

   /* top west edge */
   row = 0;
   col = 0;
   lev = dtx->MaxNl-1;
   gridPRIME_to_xyzPRIME( dtx, 0, 0, 1,  &row, &col, &lev, &x, &y, &z );
   ADD_VERTEX( x, y, z );
   row = dtx->Nr-1;
   gridPRIME_to_xyzPRIME( dtx, 0, 0, 1,  &row, &col, &lev, &x, &y, &z );
   ADD_VERTEX( x, y, z );
   END_OF_LINE;

   /* bottom west edge */
   row = 0;
   col = 0;
   lev = 0;
   gridPRIME_to_xyzPRIME( dtx, 0, 0, 1,  &row, &col, &lev, &x, &y, &z );
   ADD_VERTEX( x, y, z );
   row = dtx->Nr-1;
   gridPRIME_to_xyzPRIME( dtx, 0, 0, 1,  &row, &col, &lev, &x, &y, &z );
   ADD_VERTEX( x, y, z );
   END_OF_LINE;

   /* south-west vertical edge */
   row = dtx->Nr-1;
   col = 0;
   lev = 0;
   gridPRIME_to_xyzPRIME( dtx, 0, 0, 1,  &row, &col, &lev, &x, &y, &z );
   ADD_VERTEX( x, y, z );
   lev = dtx->MaxNl-1;
   gridPRIME_to_xyzPRIME( dtx, 0, 0, 1,  &row, &col, &lev, &x, &y, &z );
   ADD_VERTEX( x, y, z );
   END_OF_LINE;

   /* north-west vertical edge */
   row = 0;
   col = 0;
   lev = 0;
   gridPRIME_to_xyzPRIME( dtx, 0, 0, 1,  &row, &col, &lev, &x, &y, &z );
   ADD_VERTEX( x, y, z );
   lev = dtx->MaxNl-1;
   gridPRIME_to_xyzPRIME( dtx, 0, 0, 1,  &row, &col, &lev, &x, &y, &z );
   ADD_VERTEX( x, y, z );
   END_OF_LINE;

   /* north-east vertical edge */
   row = 0;
   col = dtx->Nc-1;
   lev = 0;
   gridPRIME_to_xyzPRIME( dtx, 0, 0, 1,  &row, &col, &lev, &x, &y, &z );
   ADD_VERTEX( x, y, z );
   lev = dtx->MaxNl-1;
   gridPRIME_to_xyzPRIME( dtx, 0, 0, 1,  &row, &col, &lev, &x, &y, &z );
   ADD_VERTEX( x, y, z );
   END_OF_LINE;

   /* south-east vertical edge */
   row = dtx->Nr-1;
   col = dtx->Nc-1;
   lev = 0;
   gridPRIME_to_xyzPRIME( dtx, 0, 0, 1,  &row, &col, &lev, &x, &y, &z );
   ADD_VERTEX( x, y, z );
   lev = dtx->MaxNl-1;
   gridPRIME_to_xyzPRIME( dtx, 0, 0, 1,  &row, &col, &lev, &x, &y, &z );
   ADD_VERTEX( x, y, z );
   END_OF_LINE;

}



static void make_sphere_box( Display_Context dtx )
{
   int i;
   float lat, lon, hgt;
   float x, y, z;
   float row, col, lev;
   int r, c, l;

   dtx->NumBoxVerts = 0;
   dtx->TickMarks = 0;

   /* Axis */
   lat = 90.0;
   lon = 0.0;
   hgt = 0.0;
   geo_to_xyzPRIME( dtx, 0, 0, 1, &lat, &lon, &hgt, &x, &y, &z );
   ADD_VERTEX( x, y, z );
   lat = -90.0;
   geo_to_xyzPRIME( dtx, 0, 0, 1, &lat, &lon, &hgt, &x, &y, &z );
   ADD_VERTEX( x, y, z );
   END_OF_LINE;

   /* Equator */
   lat = 0.0;
   hgt = 0.0;
   for (i=0;i<=360;i+=10) {
      lon = (float) i;
      geo_to_xyzPRIME( dtx, 0, 0, 1, &lat, &lon, &hgt, &x, &y, &z );
      ADD_VERTEX( x, y, z );
   }
   END_OF_LINE;

   /* curved edges */
   for (i=0;i<4;i++) {
      switch (i) {
         case 0:
            /* bottom south edge */
            r = dtx->Nr-1;
            l = 0;
            break;
         case 1:
            /* top south edge */
            r = dtx->Nr-1;
            l = dtx->MaxNl-1;
            break;
         case 2:
            /* top north edge */
            r = 0;
            l = dtx->MaxNl-1;
            break;
         case 3:
            /* bottom north edge */
            r = 0;
            l = 0;
            break;
      }

      for (c=0;c<dtx->Nc;c++) {
         row = (float) r;
         col = (float) c;
         lev = (float) l;
         gridPRIME_to_xyzPRIME( dtx, 0, 0, 1, &row, &col, &lev, &x, &y, &z );
         ADD_VERTEX( x, y, z );
      }
      END_OF_LINE;
   }

   for (i=0;i<4;i++) {
      switch (i) {
         case 0:
            /* bottom west edge */
            c = 0;
            l = 0;
            break;
         case 1:
            /* top west edge */
            c = 0;
            l = dtx->MaxNl-1;
            break;
         case 2:
            /* top east edge */
            c = dtx->Nc-1;
            l = dtx->MaxNl-1;
            break;
         case 3:
            /* bottom east edge */
            c = dtx->Nc-1;
            l = 0;
            break;
      }

      for (r=0;r<dtx->Nr;r++) {
         row = (float) r;
         col = (float) c;
         lev = (float) l;
         gridPRIME_to_xyzPRIME( dtx, 0, 0, 1, &row, &col, &lev, &x, &y, &z );
         ADD_VERTEX( x, y, z );
      }
      END_OF_LINE;
   }

   /* south-west vertical edge */
   row = dtx->Nr-1;
   col = 0;
   lev = 0;
   gridPRIME_to_xyzPRIME( dtx, 0, 0, 1, &row, &col, &lev, &x, &y, &z );
   ADD_VERTEX( x, y, z );
   lev = dtx->MaxNl-1;
   gridPRIME_to_xyzPRIME( dtx, 0, 0, 1, &row, &col, &lev, &x, &y, &z );
   ADD_VERTEX( x, y, z );
   END_OF_LINE;

   /* north-west vertical edge */
   row = 0;
   col = 0;
   lev = 0;
   gridPRIME_to_xyzPRIME( dtx, 0, 0, 1, &row, &col, &lev, &x, &y, &z );
   ADD_VERTEX( x, y, z );
   lev = dtx->MaxNl-1;
   gridPRIME_to_xyzPRIME( dtx, 0, 0, 1, &row, &col, &lev, &x, &y, &z );
   ADD_VERTEX( x, y, z );
   END_OF_LINE;

   /* north-east vertical edge */
   row = 0;
   col = dtx->Nc-1;
   lev = 0;
   gridPRIME_to_xyzPRIME( dtx, 0, 0, 1, &row, &col, &lev, &x, &y, &z );
   ADD_VERTEX( x, y, z );
   lev = dtx->MaxNl-1;
   gridPRIME_to_xyzPRIME( dtx, 0, 0, 1, &row, &col, &lev, &x, &y, &z );
   ADD_VERTEX( x, y, z );
   END_OF_LINE;

   /* south-east vertical edge */
   row = dtx->Nr-1;
   col = dtx->Nc-1;
   lev = 0;
   gridPRIME_to_xyzPRIME( dtx, 0, 0, 1, &row, &col, &lev, &x, &y, &z );
   ADD_VERTEX( x, y, z );
   lev = dtx->MaxNl-1;
   gridPRIME_to_xyzPRIME( dtx, 0, 0, 1, &row, &col, &lev, &x, &y, &z );
   ADD_VERTEX( x, y, z );
   END_OF_LINE;
}





/*
 * Compute the graphics bounds Xmin, Xmax, Ymin then construct the box
 * graphics.
 */
void make_box( Display_Context dtx, float ax, float ay, float az )
{
   /* First compute using user-provided aspect ratios, if given. */
   if (ax!=0.0 || ay!=0.0 || az!=0.0) {
      if (ax>=ay && ax>=az) {
         ay = ay / ax;
         az = az / ax;
         ax = 1.0;
      }
      else if (ay>=ax && ay>=az) {
         ax = ax / ay;
         az = az / ay;
         ay = 1.0;
      }
      else {
         ax = ax / az;
         ay = ay / az;
         az = 1.0;
      }
      dtx->Xmax = ax;    dtx->Xmin = -dtx->Xmax;
      dtx->Ymax = ay;    dtx->Ymin = -dtx->Ymax;
      dtx->Zmax = az;    dtx->Zmin = -dtx->Zmax;
   }

   /* Compute according to the projection. */
   switch (dtx->Projection) {
      case PROJ_GENERIC:
         dtx->CurvedBox = 0;
         if (ax==0.0 && ay==0.0 && az==0.0) {
            float rdim, cdim, ldim;
            rdim = fabs(dtx->Nr * dtx->RowInc);
            cdim = fabs(dtx->Nc * dtx->ColInc);
            ldim = dtx->TopBound - dtx->BottomBound;
            if (ldim==0.0) {
               ldim = 1.0;
            }
            if (rdim>=cdim && rdim>=ldim) {
               dtx->Xmax = cdim / rdim;
               dtx->Ymax = 1.0;
               dtx->Zmax = ldim / rdim;
            }
            else if (cdim>=rdim && cdim>=ldim) {
               dtx->Xmax = 1.0;
               dtx->Ymax = rdim / cdim;
               dtx->Zmax = ldim / cdim;
            }
            else {
               dtx->Xmax = cdim / ldim;
               dtx->Ymax = rdim / ldim;
               dtx->Zmax = 1.0;
            }
            dtx->Xmin = -dtx->Xmax;
            dtx->Ymin = -dtx->Ymax;
            dtx->Zmin = -dtx->Zmax;
         }
         break;
      case PROJ_LINEAR:
         dtx->CurvedBox = 0;
         if (ax==0.0 && ay==0.0 && az==0.0) {

               /* scale box according to lat and lon */
/*
               float lats, lons, hgt, midlat;

               midlat = (dtx->NorthBound+dtx->SouthBound) / 2.0;
               lats = dtx->NorthBound-dtx->SouthBound;
               if (lats<0.0)  lats = -lats;
               lons = (dtx->WestBound-dtx->EastBound) *
                                         cos( midlat * 3.14159/180.0 );
               if (lons<0.0)  lons = -lons;
               hgt = dtx->TopBound-dtx->BottomBound;
*/

            float width, height, midlat;
            midlat = (dtx->NorthBound+dtx->SouthBound) / 2.0;
            width = (dtx->Nc * dtx->ColInc) * cos( midlat * 3.14159/180.0 );

            height = dtx->Nr * dtx->RowInc;
            if (width>height) {
               dtx->Xmax = 1.0;
               dtx->Ymax = height / width;
               dtx->Zmax = dtx->Ymax / 2.0;
            }
            else {
               dtx->Xmax = width / height;
               dtx->Ymax = 1.0;
               dtx->Zmax = dtx->Xmax / 2.0;
            }
            dtx->Xmin = -dtx->Xmax;
            dtx->Ymin = -dtx->Ymax;
            dtx->Zmin = -dtx->Zmax;
         }
         break;
      case PROJ_ROTATED:
         dtx->CurvedBox = 0;
         if (ax==0.0 && ay==0.0 && az==0.0) {

            /* scale box according to lat and lon */

            float width, height, midlat;
            midlat = (dtx->NorthBound+dtx->SouthBound) / 2.0;
            /*midlon = (dtx->WestBound+dtx->EastBound) / 2.0;*/
            width = (dtx->Nc * dtx->ColInc) * cos( midlat * 3.14159/180.0 );
            height = dtx->Nr * dtx->RowInc;
            if (width>height) {
               dtx->Xmax = 1.0;
               dtx->Ymax = height / width;
               dtx->Zmax = dtx->Ymax / 2.0;
            }
            else {
               dtx->Xmax = width / height;
               dtx->Ymax = 1.0;
               dtx->Zmax = dtx->Xmax / 2.0;
            }
            dtx->Xmin = -dtx->Xmax;
            dtx->Ymin = -dtx->Ymax;
            dtx->Zmin = -dtx->Zmax;
         }
         break;
      case PROJ_MERCATOR:
         dtx->CurvedBox = 0;
         if (ax==0.0 && ay==0.0 && az==0.0) {
            float width, height;
            width = dtx->Nc * dtx->ColIncKm/111.19;
            height = dtx->Nr * dtx->ColIncKm/111.19;   /* only thing different from LINEAR */
            if (width>height) {
               dtx->Xmax = 1.0;
               dtx->Ymax = height / width;
               dtx->Zmax = dtx->Ymax / 2.0;
            }
            else {
               dtx->Xmax = width / height;
               dtx->Ymax = 1.0;
               dtx->Zmax = dtx->Xmax / 2.0;
            }
            dtx->Xmin = -dtx->Xmax;
            dtx->Ymin = -dtx->Ymax;
            dtx->Zmin = -dtx->Zmax;
         }
         break;
      case PROJ_LAMBERT:
         dtx->CurvedBox = 0;
         if (ax==0.0 && ay==0.0 && az==0.0) {
            float width, height;
            width = dtx->Nc * dtx->ColInc;
            height = dtx->Nr * dtx->ColInc;   /* only thing different from LINEAR */
            if (width>height) {
               dtx->Xmax = 1.0;
               dtx->Ymax = height / width;
               dtx->Zmax = dtx->Ymax / 2.0;
            }
            else {
               dtx->Xmax = width / height;
               dtx->Ymax = 1.0;
               dtx->Zmax = dtx->Xmax / 2.0;
            }
            dtx->Xmin = -dtx->Xmax;
            dtx->Ymin = -dtx->Ymax;
            dtx->Zmin = -dtx->Zmax;
         }
         break;
      case PROJ_STEREO:
         dtx->CurvedBox = 0;
         if (ax==0.0 && ay==0.0 && az==0.0) {
            if (dtx->Nc>dtx->Nr) {
               dtx->Xmax = 1.0;
               dtx->Ymax = (float) dtx->Nr / (float) dtx->Nc;
               dtx->Zmax = dtx->Ymax / 2.0;
            }
            else {
               dtx->Xmax = (float) dtx->Nc / (float) dtx->Nr;
               dtx->Ymax = 1.0;
               dtx->Zmax = dtx->Xmax / 2.0;
            }
            dtx->Xmin = -dtx->Xmax;
            dtx->Ymin = -dtx->Ymax;
            dtx->Zmin = -dtx->Zmax;
         }
         break;
      case PROJ_CYLINDRICAL:
         /* Curved box */
         dtx->CurvedBox = 1;
         if (ax==0.0 && ay==0.0 && az==0.0) {
            dtx->Xmin = -1.0;
            dtx->Xmax =  1.0;
            dtx->Ymin = -1.0;
            dtx->Ymax =  1.0;
            dtx->Zmin = -0.125;   /* TODO: compute better values? */
            dtx->Zmax =  0.125;
         }
         else {
            dtx->Xmin = -1.0;
            dtx->Xmax =  1.0;
            dtx->Ymin = -1.0;
            dtx->Ymax =  1.0;
            dtx->Zmin = -0.125 * az / ax;
            dtx->Zmax =  0.125 * az / ax;
         }
         break;
      case PROJ_SPHERICAL:
         dtx->CurvedBox = 1;
         dtx->Xmin = -1.0;
         dtx->Xmax =  1.0;
         dtx->Ymin = -1.0;
         dtx->Ymax =  1.0;
         dtx->Zmin = -1.0;
         dtx->Zmax =  1.0;
         break;
      default:
         printf("Error in setup_box\n");
   }

   /*
    * Construct the 3-D bounding box.
    */
   switch (dtx->Projection) {
      case PROJ_GENERIC:
      case PROJ_LINEAR:
      case PROJ_LAMBERT:
      case PROJ_STEREO:
      case PROJ_MERCATOR:
      case PROJ_ROTATED:
         make_square_box(dtx);
         break;
      case PROJ_CYLINDRICAL:
         make_cylinder_box(dtx);
         break;
      case PROJ_SPHERICAL:
         make_sphere_box(dtx);
         break;
      default:
         printf("Error in setup_box\n");
   }
}



/*
 * Draw the 3-D box.
 * Input:  it - time step.
 */
void draw_box( Display_Context dtx, int it )
{
   /* base and up vectors for text drawn along x,y,z axes. */
   static float bx[3] = { 0.05, 0.0, 0.0 },      ux[3] = { 0.0, 0.05, 0.05 };
   static float by[3] = { -0.035, 0.0, -0.035 },  uy[3] = { 0.0, 0.07, 0.0 };
   static float bz[3] = { -0.035, -0.035, 0.0 }, uz[3] = { 0.0, 0.0, 0.07 };
   float x1, y1, z1, x2, y2, z2;
   char str[100];

   /* set depth cueing & line color */
/* MJK 3.29.99 */
   if (dtx->Reversed){
      set_color( PACK_COLOR(0,0,0,255) );
   }
   else{
      set_color( dtx->BoxColor );
   }


   set_depthcue( dtx->DepthCue );

/* MJK 3.29.99 */
   if (dtx->Reversed){
      draw_multi_lines( dtx->NumBoxVerts, dtx->BoxVerts, PACK_COLOR(0,0,0,255) );
   }
   else{
      draw_multi_lines( dtx->NumBoxVerts, dtx->BoxVerts, dtx->BoxColor );
   }


   if (dtx->TickMarks) {
      /* Draw axis labels. */
      if (dtx->CoordFlag) {
         x1 = 1.0;
         x2 = (float) dtx->Nc;
         y1 = 1.0;
         y2 = (float) dtx->Nr;
         z1 = 1.0;
         z2 = (float) dtx->MaxNl;
      }
      else {
         x1 = dtx->WestBound;
         x2 = dtx->EastBound;
         y1 = dtx->NorthBound;
         y2 = dtx->SouthBound;
#ifdef LEVELTYPES
         z1 = dtx->BottomCoordinate;
         z2 = dtx->TopCoordinate;
#else
         z1 = dtx->BottomBound;
         z2 = dtx->TopBound;
#endif
         z1 = VERT(z1);
         z2 = VERT(z2);
      }

      if (dtx->CursorX - dtx->Xmin > 0.1 || dtx->DisplayCursor==0) {
         /* MJK 12.02.98 */
         float2string (dtx, 0, x1, str);
         plot_string( str, dtx->Xmin-0.02, dtx->Ymin-0.1, dtx->Zmin-0.125, bx, ux, 0 );
      }

      if (dtx->Xmax - dtx->CursorX > 0.1 || dtx->DisplayCursor==0) {
         /* MJK 12.02.98 */
         float2string (dtx, 0, x2, str);
         plot_string( str, dtx->Xmax-0.05, dtx->Ymin-0.1, dtx->Zmin-0.125, bx, ux, 0 );
      }

      if (dtx->Ymax - dtx->CursorY > 0.1 || dtx->DisplayCursor==0) {
         /* MJK 12.02.98 */
         float2string (dtx, 1, y1, str);
         plot_string( str, dtx->Xmin-0.075, dtx->Ymax-0.03, dtx->Zmin-0.075, by, uy, 1 );
      }

      if (dtx->CursorY-dtx->Ymin > 0.1 || dtx->DisplayCursor==0) {
         /* MJK 12.02.98 */
         float2string (dtx, 2, y2, str);
         plot_string( str, dtx->Xmin-0.075, dtx->Ymin-0.02, dtx->Zmin-0.075, by, uy, 1 );
      }

      if (dtx->CursorZ-dtx->Zmin > 0.1 || dtx->DisplayCursor==0) {
         /* MJK 12.02.98 */
         float2string (dtx, 2, z1, str);
         plot_string( str, dtx->Xmin-0.07, dtx->Ymin-0.07, dtx->Zmin+0.005, bz, uz, 1 );
      }

      if (dtx->Zmax-dtx->CursorZ > 0.1 || dtx->DisplayCursor==0) {
         /* MJK 12.02.98 */
         float2string(dtx, 2, z2, str);
         plot_string( str, dtx->Xmin-0.07, dtx->Ymin-0.07, dtx->Zmax+0.005, bz, uz, 1 );
      }
   }

   set_depthcue( 0 );
}

void draw_tick_marks( Display_Context dtx )
{
   float verts[2][3];
   /* base and up vectors for text drawn along x,y,z axes. */
   static float bx[3] = { 0.05, 0.0, 0.0 },      ux[3] = { 0.0, 0.05, 0.05 };
   static float by[3] = { -0.035, 0.0, -0.035 },  uy[3] = { 0.0, 0.07, 0.0 };
   static float bz[3] = { -0.035, -0.035, 0.0 }, uz[3] = { 0.0, 0.0, 0.07 };
   float tick_inc, i, row, col, lev;
   char str[100];
   
   /* set depth cueing & line color */
/* MJK 3.29.99 */
   if (dtx->Reversed){
      set_color( PACK_COLOR(0,0,0,255) );
   }
   else{
      set_color( dtx->BoxColor );
   }


   set_depthcue( dtx->DepthCue );

   /* go in the order.. low south side, low east side, 
      go counter-clock-wise, then high south side, couterclockwise,
      then southeast side, and work coutner-clockwise, so there are
      12 sides to loop through */
     
   dtx->tick_do[0] = 1;
   dtx->tick_type[0] = 1;
   dtx->tick_num[0] = 10;
   if (dtx->tick_do[0]){
      tick_inc = (float)(dtx->Nc)/(float)(dtx->tick_num[0]-1);
      row = dtx->Nr-1;
      lev = 0;
      for (i = tick_inc; i < dtx->Nc; i += tick_inc){
         col = i;
         vis5d_gridPRIME_to_xyzPRIME(dtx->dpy_context_index, 0, 0,
                  row, col, lev, &verts[0][0], &verts[0][1], &verts[0][2]);
         verts[1][0] = verts[0][0];
         verts[1][1] = verts[0][1] - 0.05;
         verts[1][2] = verts[0][2] - 0.062;
         polyline( verts, 2);
         if (dtx->tick_type[0] == 0){
            float lat, lon, hgt;
            vis5d_gridPRIME_to_geo(dtx->dpy_context_index, 0, 0,
                  row, col, lev, &lat, &lon, &hgt);
            /*float2string(lon, str); */
            if (strlen(str) <2){
               plot_string( str, verts[1][0]-.009, verts[1][1]-.05, verts[1][2]-0.062, bx, ux, 0);
            }
            else if (strlen(str) <4){
               plot_string( str, verts[1][0]-.02, verts[1][1]-.05, verts[1][2]-0.062, bx, ux, 0);
            }
            else{
               plot_string( str, verts[1][0]-.05, verts[1][1]-.05, verts[1][2]-0.062, bx, ux, 0);
            }
         }
         else if (dtx->tick_type[0] == 1){
            /* float2string(col, str); */
            if (strlen(str) <2){
               plot_string( str, verts[1][0]-.009, verts[1][1]-.05, verts[1][2]-0.062, bx, ux, 0);
            }
            else if (strlen(str) <4){
               plot_string( str, verts[1][0]-.02, verts[1][1]-.05, verts[1][2]-0.062, bx, ux, 0);
            }
            else{
               plot_string( str, verts[1][0]-.05, verts[1][1]-.05, verts[1][2]-0.062, bx, ux, 0);
            }
         }
      }
   }
}

         
                         





void draw_logo( Display_Context dtx, unsigned int c )
{
   static short vv[7][2] = {{-1,0}, {9, 41}, {11, 41}, {21, 0},
                          {20, 0}, {10, 41}, {0, 0}};
   static short ii[4][2] = { {10,18}, {10,40}, {11,40}, {11,18} };
   static short idot[4][2] = { {10,15}, {10,14}, {11,14}, {12,15} };
   static short ss[24][2] = {{0,37}, {5, 40}, {16,40}, {20, 38},
                           {20, 32}, {16, 30}, {5, 30}, {0,28},
                           {0, 22}, {5, 20}, {15, 20}, {20, 22},
                           {20, 23}, {15, 21}, {5, 21}, {1, 22},
                           {1, 28}, {5, 30}, {16, 30}, {21, 32},
                           {21, 38}, {16, 40}, {5, 40}, {0, 38}};
   static short s5[18][2] = {{0,35}, {5, 40}, {16,40}, {20, 36},
                           {20, 25}, {16, 21}, {0, 21},
                           {0, 0}, {20, 0}, {20,1}, {1, 1},
                           {1, 20}, {16, 20}, {21, 25},
                           {21, 36}, {16, 41}, {5, 41}, {0, 36}};
   static short dd[14][2] = {{0, 0}, {0, 41}, {16, 41}, {21, 36},
                           {21, 5}, {16, 0}, {1, 0}, {1, 1},
                           {16, 1}, {20, 5}, {20, 36}, {16, 40},
                           {1, 40}, {1, 1}};
   short p[30][2];
   int j;
   float factor;

   factor = dtx->LogoSize;
   set_color( c );

   for (j=0; j<7; j++) {
      p[j][0] = (short)((float)(((dtx->WinWidth - (132)/factor)*factor + vv[j][0]))/factor);
      p[j][1] = (short)((float)(((dtx->WinHeight - (50)/factor )*factor + vv[j][1]))/factor);
   }
   polyline2d( p, 7 );

   for (j=0; j<4; j++) {
      p[j][0] = (short)((float)(((dtx->WinWidth - (112)/factor )*factor + ii[j][0]))/factor);
      p[j][1] = (short)((float)(((dtx->WinHeight - (50)/factor )*factor + ii[j][1]))/factor);
   }
   polyline2d( p, 4 );

   for (j=0; j<4; j++) {
      p[j][0] = (short)((float)(((dtx->WinWidth - (112)/factor )*factor + idot[j][0]))/factor);
      p[j][1] = (short)((float)(((dtx->WinHeight - (50)/factor )*factor + idot[j][1]))/factor);
   }
   polyline2d( p, 4 );

   for (j=0; j<24; j++) {
      p[j][0] = (short)((float)(((dtx->WinWidth - (90)/factor )*factor + ss[j][0]))/factor);
      p[j][1] = (short)((float)(((dtx->WinHeight - (50)/factor )*factor + ss[j][1]))/factor);
   }
   polyline2d( p, 24 );

   for (j=0; j<18; j++) {
      p[j][0] = (short)((float)(((dtx->WinWidth - (60)/factor )*factor + s5[j][0]))/factor);
      p[j][1] = (short)((float)(((dtx->WinHeight - (50)/factor )*factor + s5[j][1]))/factor);
   }
   polyline2d( p, 18 );

   for (j=0; j<14; j++) {
      p[j][0] = (short)((float)(((dtx->WinWidth - (30)/factor )*factor + dd[j][0]))/factor);
      p[j][1] = (short)((float)(((dtx->WinHeight - (50)/factor )*factor + dd[j][1]))/factor);
   }
   polyline2d( p, 14 );

}

