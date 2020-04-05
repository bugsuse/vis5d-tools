/* proj.c */
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
 * Map projection / coordinate transformation
 */



/*
 * Map projection parameters:
 *
 *   IF Projection==PROJ_GENERIC THEN
 *          projargs[0] = northbound
 *          projargs[1] = westbound
 *          projargs[2] = row increment
 *          projargs[3] = column increment
 *   ELSE IF Projection==PROJ_LINEAR THEN
 *          projargs[0] = northbound in degrees
 *          projargs[1] = westbound in deg.
 *          projargs[2] = row increment in deg.
 *          projargs[3] = column increment in deg.
 *   ELSE IF Projection==PROJ_LAMBERT THEN
 *          projargs[0] = Lat1
 *          projargs[1] = Lat2
 *          projargs[2] = PoleRow
 *          projargs[3] = PoleColumn
 *          projargs[4] = Central Long.
 *          projargs[5] = Column Increment in km
 *   ELSE IF Projection==PROJ_STEREO THEN
 *          projargs[0] = Central Latitude
 *          projargs[1] = Central Longitude
 *          projargs[2] = Central Row
 *          projargs[3] = Central Column
 *          projargs[4] = Column increment in km
 *   ELSE IF Projection==PROJ_ROTATED THEN
 *          projargs[0] = Latitude on rotated globe of grid row 0
 *          projargs[1] = Longitude on rotated globe of grid column 0
 *          projargs[2] = degs of latitude on rotated globe between grid rows
 *          projargs[3] = degs of longitude on rotated globe between grid cols
 *          projargs[4] = Earth latitude of (0, 0) on rotated globe
 *          projargs[5] = Earth longitude of (0, 0) on rotated globe
 *          projargs[6] = Clockwise rotation of rotated globe in degrees
 *   ELSE IF Projection==PROJ_MERCATOR THEN
 *          projargs[0] = Central Latitude
 *          projargs[1] = Central Longitude
 *          projargs[2] = Row Increment in Kilometers
 *          projargs[3] = Column Increment in Kilometers
 *   ELSE IF Projection==PROJ_EPA THEN
 *          projargs[ (R*nc+C) * 2 + 0 ] = latitude of grid point (R,C)
 *          projargs[ (R*nc+C) * 2 + 1 ] = longitude of grid point (R,C)
 *   ENDIF
 */


/*
 * Vertical coordinate system parameters:
 *
 *   IF VertSystem==VERT_GENERIC THEN
 *          vertargs[0] = Bottom bound
 *          vertargs[1] = level increment
 *   ELSE IF VertSystem==VERT_EQUAL_KM THEN
 *          vertargs[0] = bottom bound in km
 *          vertargs[1] = level increment in km
 *   ELSE IF VertSystem==VERT_UNEQUAL_KM THEN
 *          vertargs[n] = height of grid level n.  n is in [0..Nl-1]
 *   ELSE IF VertSystem==VERT_UNEQUAL_MB THEN
 *          vertargs[n] = height of grid level n.  n is in [0..Nl-1]
 *   ELSE IF VertSystem==VERT_EPA THEN
 *          vertargs[n] = height of grid level n.  n is in [0..Nl-1]
 *   ENDIF
 */



#include <math.h>
#include <stdio.h>
#include "grid_i.h"
#include "proj_i.h"
#include "v5d.h"


#ifndef M_PI
#  define M_PI 3.1415926
#endif


#define DEG2RAD        (M_PI/180.0)
#define RAD2DEG (180.0/M_PI)
#define RADIUS        6371.23




/* Pete rotated sphere to Earth */
static void pandg_back( float *lat, float *lon, float a, float b, float r )
{
  float pr, gr, pm, gm;

  /* NOTE - longitude sign switches - b too! */

  pr = DEG2RAD * *lat;
  gr = -DEG2RAD * *lon;
  pm = asin( cos(pr) * cos (gr) );
  gm = atan2(cos(pr) * sin (gr), -sin(pr) );

  *lat = RAD2DEG * asin( sin(a) * sin(pm) - cos(a) * cos(pm) * cos (gm - r) );
  *lon = -RAD2DEG * (-b + atan2(cos(pm) * sin (gm - r),
                   sin(a) * cos(pm) * cos (gm - r) + cos(a) * sin(pm)));
}


/* Earth to Pete rotated sphere */
static void pandg_for( float *lat, float *lon, float a, float b, float r )
{
  float p1, g1, p, g;

  /* NOTE - longitude sign switches - b too! */

  p1 = DEG2RAD * *lat;
  g1 = -DEG2RAD * *lon;
  p = asin( sin(a) * sin(p1) + cos(a) * cos(p1) * cos (g1 + b) );
  g = r + atan2(cos(p1) * sin (g1 + b),
                sin(a) * cos(p1) * cos (g1 + b) - cos(a) * sin(p1) );

  *lat = RAD2DEG * asin( -cos(p) * cos (g) );
  *lon = -RAD2DEG * atan2(cos(p) * sin (g), sin(p) );
}


/**********************************************************************/
/*****                   Map Projection                           *****/
/**********************************************************************/




/*
 * Convert a (row,column) grid coordinate to a (lat,lon) coordinate
 * according to the projection info.
 * Return:  1 = ok, 0 = error (out of bounds)
 */
int rowcol_to_latlon_i( float row, float col, float *lat, float *lon,
                      struct projection *proj )
{

   switch (proj->Kind) {
      case PROJ_GENERIC:   /* Generic rectilinear */
      case PROJ_LINEAR:   /* lat/lon rectilinear */
      case PROJ_CYLINDRICAL:
      case PROJ_SPHERICAL:
         /* FALL-THROUGH */
#define NorthBound proj->Args[0]
#define WestBound proj->Args[1]
#define RowInc proj->Args[2]
#define ColInc proj->Args[3]
         *lat = NorthBound - row * RowInc;
         *lon = WestBound - col * ColInc;
#undef NorthBound
#undef WestBound
#undef RowInc
#undef ColInc
         break;

      case PROJ_LAMBERT:   /* Lambert conformal */
#define PoleRow proj->Args[2]
#define PoleCol proj->Args[3]
#define CentralLon proj->Args[4]
#define Hemisphere proj->AuxArgs[0]
#define ConeFactor proj->AuxArgs[1]
#define Cone proj->AuxArgs[2]
         {
            float xldif, xedif, xrlon, radius;

            xldif = Hemisphere * (row-PoleRow) / ConeFactor;
            xedif = (PoleCol-col) / ConeFactor;
            if (xldif==0.0 && xedif==0.0)
               xrlon = 0.0;
            else
               xrlon = atan2( xedif, xldif );
            *lon = xrlon / Cone * RAD2DEG + CentralLon;
            if (*lon > 180.0)
               *lon -= 360.0;

            radius = sqrt( xldif*xldif + xedif*xedif );
            if (radius < 0.0001)
               *lat = 90.0 * Hemisphere;   /* +/-90 */
            else
               *lat = Hemisphere
                      * (90.0 - 2.0*atan(exp(log(radius)/Cone))*RAD2DEG);
         }
#undef PoleRow
#undef PoleCol
#undef CentralLon
#undef Hemisphere
#undef ConeFactor
#undef Cone
         break;

      case PROJ_MERCATOR: /* Mercator */
#define CentralLat proj->Args[0]
#define CentralLon proj->Args[1]
#define RowIncKm proj->Args[2]
#define ColIncKm proj->Args[3]
         {
            float YC, alpha, ic, jc;
            YC = RADIUS * log((1.0 + sin(DEG2RAD*CentralLat))/cos(DEG2RAD*CentralLat));
            ic = (proj->Nr-1)/2.0;
            jc = (proj->Nc-1)/2.0;
            alpha = ( (ic-row) * RowIncKm + YC) / RADIUS;
            *lat = 2 * RAD2DEG * atan( exp(alpha) ) - 90.0;
            *lon = CentralLon - RAD2DEG * (col-jc) * ColIncKm / RADIUS;
         }
#undef CentralLat
#undef CentralLon
#undef RowIncKm
#undef ColIncKm
         break;

      case PROJ_STEREO:   /* Polar Stereographic */
#define CentralLat  proj->Args[0]
#define CentralLon  proj->Args[1]
#define CentralRow  proj->Args[2]
#define CentralCol  proj->Args[3]
#define ColInc      proj->Args[4]
         {
            float CosCentralLat = cos( CentralLat * DEG2RAD );
            float SinCentralLat = sin( CentralLat * DEG2RAD );
            float Scale = (2.0 * RADIUS / ColInc);
            float InvScale = 1.0 / Scale;
            float xrow, xcol, rho, c, cc, sc;

            xrow = CentralRow - row;
            xcol = CentralCol - col;
            rho = xrow*xrow + xcol*xcol;
            if (rho<1.0e-20) {
               *lat = CentralLat;
               *lon = CentralLon;
            }
            else {
               rho = sqrt( rho );
               c = 2.0 * atan( rho * InvScale);
               cc = cos(c);
               sc = sin(c);
               *lat = RAD2DEG
                    * asin( cc*SinCentralLat + xrow*sc*CosCentralLat / rho );
               *lon = CentralLon + RAD2DEG * atan2( xcol * sc,
                         (rho * CosCentralLat * cc - xrow * SinCentralLat * sc) );
               if (*lon < -180.0)  *lon += 360.0;
               else if (*lon > 180.0)  *lon -= 360.0;
            }
         }
#undef CentralLat
#undef CentralLon
#undef CentralRow
#undef CentralCol
#undef ColInc
         break;

      case PROJ_ROTATED:   /* Rotated */
#define NorthBound proj->Args[0]
#define SouthBound (proj->Args[0] - proj->Args[2]*(proj->Nr-1))
#define WestBound proj->Args[1]
#define EastBound (proj->Args[1] - proj->Args[3]*(proj->Nc-1))
#define CentralLat proj->Args[4]
#define CentralLon proj->Args[5]
#define Rotation (proj->Args[6] * DEG2RAD)
         *lat = NorthBound - row * (NorthBound-SouthBound) / (float) (proj->Nr-1);
         *lon = WestBound - col * (WestBound-EastBound) / (float) (proj->Nc-1);
         pandg_back(lat, lon, CentralLat, CentralLon, Rotation);
#undef NorthBound
#undef SouthBound
#undef WestBound
#undef EastBound
#undef CentralLat
#undef CentralLon
#undef Rotation
         break;

      case PROJ_EPA:   /* EPA projection */
#define Latitude(ROW,COL)   proj->Args[ ((ROW)*nc+(COL)) * 2 + 0 ]
#define Longitude(ROW,COL)  proj->Args[ ((ROW)*nc+(COL)) * 2 + 1 ]
         {
            /* TODO:  interpolate between grid points */
            int nr, nc;

            nr = proj->Nr;
            nc = proj->Nc;

            *lat = Latitude( (int) row, (int) col );
            *lon = Longitude( (int) row, (int) col );
         }
#undef Latitude
#undef Longitude
         break;

      default:
         printf("Error in rowcol_to_latlon_i: bad projection: %d\n", proj );
         break;
   }

   return 1;
}




/*
 * Find the rectangle in the EPA Lambert conformal grid which contains the
 * given lat/lon coordinate.
 * Input:  lat, lon - coordinate of interest.
 *         proj - the projection
 * Output:  n, m - row, column of upper-left corner of rectangle.
 *          s, t - coefficients for interpolation.
 * Return:  1 = found a containing rectangle
 *          0 = didn't find a rectangle.
 */
static int find_rectangle( float lat, float lon, int *n, int *m,
                           float *s, float *t, struct projection *proj )
{
#define Latitude(ROW,COL)   proj->Args[ ((ROW)*nc+(COL)) * 2 + 0 ]
#define Longitude(ROW,COL)  proj->Args[ ((ROW)*nc+(COL)) * 2 + 1 ]
   int i, j;
   float px, py, dx, dy;
   int nr, nc;

   /* First two elements of projargs indicate no. of rows and columns */
   nr = proj->Nr;
   nc = proj->Nc;

   for (i=0;i<nr-1;i++) {
      for (j=0;j<nc-1;j++) {

         /*
          * Test if (lat,lon) is inside the rectangle by constructing
          * edge vectors, computing the cross product, and testing if
          * the z component is always positive.
          */
         /* top edge */
         px = lon - Longitude(i,j);
         py = lat - Latitude(i,j);
         dx = Longitude(i,j+1) - Longitude(i,j);
         dy = Latitude(i,j+1) - Latitude(i,j);
         if (dx*py-dy*px<0.0) {
            /* outside */
            /*            printf("above\n");*/
            continue;
         }
         /* right edge */
         px = lon - Longitude(i,j+1);
         py = lat - Latitude(i,j+1);
         dx = Longitude(i+1,j+1) - Longitude(i,j+1);
         dy = Latitude(i+1,j+1) - Latitude(i,j+1);
         if (dx*py-dy*px<0.0) {
            /* outside */
            /*printf("right\n");*/
            continue;
         }
         /* bottom edge */
         px = lon - Longitude(i+1,j+1);
         py = lat - Latitude(i+1,j+1);
         dx = Longitude(i+1,j) - Longitude(i+1,j+1);
         dy = Latitude(i+1,j) - Latitude(i+1,j+1);
         if (dx*py-dy*px<0.0) {
            /* outside */
            /*printf("below\n");*/
            continue;
         }
         /* left edge */
         px = lon - Longitude(i+1,j);
         py = lat - Latitude(i+1,j);
         dx = Longitude(i,j) - Longitude(i+1,j);
         dy = Latitude(i,j) - Latitude(i+1,j);
         if (dx*py-dy*px<0.0) {
            /* outside */
            /*            printf("left\n");*/
            continue;
         }

         /* if we get here, all edge tests succeeded, */
         /* therefore, (lat,lon) is inside the rectangle */

         /* return row, column location */
         *n = i;
         *m = j;
         /* compute interpolation coefficients */
         *s = (lat - Latitude(i,j)) / (Latitude(i+1,j) - Latitude(i,j));
         *t = (lon - Longitude(i,j)) / (Longitude(i,j+1) - Longitude(i,j));

         return 1;
      }
   }

   return 0;
#undef Latitude
#undef Longitude
}



/*
 * Convert a lat/lon coordinate to a row/column according to the given
 * projection.
 * Input:  lat, lon - the lat/lon coordinate
 *         proj - which projection
 *         projargs - projection arguments
 *         nr, nc - number of rows, columns in corresponding 3-D grid
 * Output:  row, col - the computed row and column
 * Return:  1 = ok, 0 = error (lat/lon out of domain)
 */
int latlon_to_rowcol_i( float lat, float lon, float *row, float *col,
                      struct projection *proj )
{
   float lat0, lon0;

   switch (proj->Kind) {

      case PROJ_GENERIC:   /* Generic rectilinear */
      case PROJ_LINEAR:   /* lat/lon rectilinear */
      case PROJ_CYLINDRICAL:
      case PROJ_SPHERICAL:
#define NorthBound proj->Args[0]
#define WestBound proj->Args[1]
#define RowInc proj->Args[2]
#define ColInc proj->Args[3]
         *row = (NorthBound - lat) / RowInc;
         *col = (WestBound - lon) / ColInc;
         if (*row<0.0 || *row>(proj->Nr-1) || *col<0.0 || *col>(proj->Nc-1)) {
            return 0;
         }
#undef NorthBound
#undef WestBound
#undef RowInc
#undef ColInc
         break;

      case PROJ_LAMBERT:   /* Lambert conformal */
#define PoleRow proj->Args[2]
#define PoleCol proj->Args[3]
#define CentralLon proj->Args[4]
#define Hemisphere proj->AuxArgs[0]
#define ConeFactor proj->AuxArgs[1]
#define Cone proj->AuxArgs[2]
         {
            float rlon, rlat, r;
            rlon = lon - CentralLon;
            rlon = rlon * Cone * DEG2RAD;

            if (lat<-85.0) {
               /* infinity */
               r = 10000.0;
            }
            else {
               rlat = (90.0 - Hemisphere * lat) * DEG2RAD * 0.5;
               r = ConeFactor * pow( tan(rlat), Cone );
            }
            *row = PoleRow + r * cos(rlon);
            *col = PoleCol - r * sin(rlon);
            if (   *row<0.0 || *row>(proj->Nr-1)
                || *col<0.0 || *col>(proj->Nc-1)) {
               return 0;
            }
         }
#undef PoleRow
#undef PoleCol
#undef CentralLon
#undef Hemisphere
#undef ConeFactor
#undef Cone
         break;

      case PROJ_MERCATOR: /* Mercator */
#define CentralLat proj->Args[0]
#define CentralLon proj->Args[1]
#define RowIncKm proj->Args[2]
#define ColIncKm proj->Args[3]
         {
            float X, Y, ic, jc, YC;
            ic = (proj->Nr-1)/2.0;
            jc = (proj->Nc-1) / 2.0;
            YC = RADIUS * log((1.0 + sin(DEG2RAD*CentralLat))/cos(DEG2RAD*CentralLat));
            X = RADIUS * (lon-CentralLon) / RAD2DEG;
            Y = RADIUS * log( (1+sin(DEG2RAD*lat))/cos(DEG2RAD*lat) );
            *row = ic - (Y - YC)/RowIncKm;
            *col = jc -  X/ColIncKm;
          }
#undef CentralLat
#undef CentralLon
#undef RowIncKm
#undef ColIncKm
          break;

      case PROJ_STEREO:   /* Polar Stereographic */
#define CentralLat  proj->Args[0]
#define CentralLon  proj->Args[1]
#define CentralRow  proj->Args[2]
#define CentralCol  proj->Args[3]
#define ColInc      proj->Args[4]
         {
            float CosCentralLat = cos( CentralLat * DEG2RAD );
            float SinCentralLat = sin( CentralLat * DEG2RAD );
            float Scale = (2.0 * RADIUS / ColInc);
            float rlat, rlon, clon, clat, k;

            rlat = DEG2RAD * lat;
            rlon = DEG2RAD * (CentralLon - lon);
            clon = cos(rlon);
            clat = cos(rlat);
            k = Scale
                / (1.0 + SinCentralLat*sin(rlat) + CosCentralLat*clat*clon);
            *col = CentralCol + k * clat * sin(rlon);
            *row = proj->Nr - CentralRow
               - k * (CosCentralLat * sin(rlat) - SinCentralLat * clat * clon);

            if (   *row<0.0 || *row>(proj->Nr-1)
                || *col<0.0 || *col>(proj->Nc-1)) {
               return 0;
            }
         }
#undef CentralLat
#undef CentralLon
#undef CentralRow
#undef CentralCol
#undef ColInc
#undef Nr
         break;

      case PROJ_ROTATED:
#define NorthBound proj->Args[0]
#define SouthBound (proj->Args[0] - proj->Args[2]*(proj->Nr-1))
#define WestBound proj->Args[1]
#define EastBound (proj->Args[1] - proj->Args[3]*(proj->Nc-1))
#define CentralLat proj->Args[4]
#define CentralLon proj->Args[5]
#define Rotation (proj->Args[6] * DEG2RAD)
         lat0 = lat;
         lon0 = lon;
         pandg_for( &lat0, &lon0, CentralLat, CentralLon, Rotation );
         *col = (WestBound-lon0)/proj->Args[3];
         *row = (NorthBound-lat0) / proj->Args[2];
         if (   *row<0.0 || *row>(proj->Nr-1)
             || *col<0.0 || *col>(proj->Nc-1)) {
            return 0;
         }
#undef NorthBound
#undef SouthBound
#undef WestBound
#undef EastBound
#undef CentralLat
#undef CentralLon
#undef Rotation
         break;

      case PROJ_EPA:   /* EPA projection */
         {
            int r, c;
            float alpha, beta;
            if (find_rectangle( lat, lon, &r, &c, &alpha, &beta, proj )) {
               *row = (float) r + alpha;
               *col = (float) c + beta;
            }
            else {
               return 0;
            }
         }
         break;

      default:
         printf("Error in latlon_to_rowcol_i: bad projection: %d\n", proj );
         break;
   }

   return 1;
}



/*
 * Return a floating point value indicitive of the resolution of a
 * map projection.  Low values indicate high-resolution, high values
 * indicate low-resolution.  Typically, this value is the product of
 * the row and column increments.
 * Input:  proj - the map projection
 * Return:  resolution value
 */
float proj_resolution( struct projection *proj )
{
   float res;

   switch (proj->Kind) {
      case PROJ_GENERIC:   /* Generic rectilinear */
      case PROJ_LINEAR:   /* lat/lon rectilinear */
      case PROJ_CYLINDRICAL:
      case PROJ_SPHERICAL:
#define RowInc proj->Args[2]
#define ColInc proj->Args[3]
         res = RowInc * ColInc;
#undef RowInc
#undef ColInc
         break;

      case PROJ_MERCATOR:
#define RowInc proj->Args[2]/111.75
#define ColInc proj->Args[3]/111.75
         res = RowInc * ColInc;
#undef RowInc
#undef ColInc
         break;

      case PROJ_ROTATED:
#define RowInc proj->Args[2]
#define ColInc proj->Args[3]
         res = RowInc * ColInc;
#undef RowInc
#undef ColInc
         break;

      case PROJ_LAMBERT:   /* Lambert conformal */
      case PROJ_STEREO:   /* Polar Stereographic */
      case PROJ_EPA:   /* EPA projection */
         /* measure distance between a few grid points in center of grid */
         {
            float midrow = proj->Nr / 2.0;
            float midcol = proj->Nc / 2.0;
            float lat0, lon0, lat1, lon1, dlat, dlon;
            rowcol_to_latlon_i( midrow, midcol, &lat0, &lon0, proj );
            rowcol_to_latlon_i( midrow+1.0, midcol+1.0, &lat1, &lon1, proj );
            dlat = lat1-lat0;
            dlon = lon1-lon0;
            res = sqrt( dlat*dlat + dlon*dlon );
         }
         break;

      default:
         printf("Error in proj_resolution: bad projection: %d\n", proj );
         break;
   }

   /* Return absolute value */
   if (res<0.0) {
      return -res;
   }
   else {
      return res;
   }
}




/**********************************************************************/
/*****              Vertical Coordinate System                    *****/
/**********************************************************************/


/*
 * Perform a binary search of the array for the value and return
 * the index as a float.  Return a -1.0 if the value is out of bounds.
 */
static float binary_search( float value, float array[], int size )
{
   int low, high, mid;
   float x;

   if (value==array[0]) {
      return 0.0;
   }
   else if (value<array[0]) {
      return -1.0;
   }
   else if (value>array[size-1]) {
      return -1.0;
   }
   else if (value==array[size-1]) {
      return (float) (size-1);
   }
   else {
      /* do a binary search of array[] for value */
      low = 0;
      high = size-1;

      while (low<=high) {
         mid = (low+high)/2;
         if (value<array[mid])
           high = mid - 1;
         else if (value>array[mid])
           low = mid + 1;
         else
           return (float) mid;
      }

      /* interpolate a value between high and low */
      x = (value-array[high]) / (array[low]-array[high]);
      return high * (1.0-x) + low * x;
   }
}



/*
 * Convert a grid level into a height value.
 * Input:  level - the grid level
 *         vcs - the vertical coordinate system
 *         topo_elev - elevation of topography at the implicit lat/lon
 *                     (only used for EPA projection)
 * Output:  height - the height value
 */
int level_to_height( float level, float *height, struct vcs *vcs,
                     float topo_elev )
{

   switch (vcs->Kind) {
      case VERT_GENERIC:
         *height = vcs->Args[0] + level * vcs->Args[1];
         break;
      case VERT_EQUAL_KM:
         *height = vcs->Args[0] + level * vcs->Args[1];
         break;
      case VERT_UNEQUAL_MB:   /* Unequally spaced mb */
      case VERT_UNEQUAL_KM:   /* Unequally spaced km */
         {
            int ilevel;
            float alpha;
            ilevel = (int) level;
            alpha = level - ilevel;
            *height = vcs->Args[ilevel] * (1.0-alpha)
                      + vcs->Args[ilevel+1] * alpha;
         }
         break;
      case VERT_EPA:
#define Sigma(K)   vcs->Args[(K)]
         {
            float psurf, s, p;
            psurf = 1012.5 * exp( -topo_elev / 7.2 );
            s = Sigma( (int) level );
            p = 100.0 + s * (psurf - 100.0);
            *height = -7.2 * log( p / 1012.5 );
         }
#undef Sigma
         break;
      default:  /* error */
         ;
   }

   return 1;
}



/*
 * Convert a height to a grid level.
 * Input:  height - the input height value
 *         vcs - the vertical coordinate system
 *         topo_elev - elevation of the topograph at the lat/lon of interest
 *         nl - number of levels in input coordinate system grid
 * Output:  level - the output grid level
 * Return:  1 = ok, 0 = error (height outside of domain)
 */
int height_to_level( float height, float *level, struct vcs *vcs,
                     float topo_elev )
{
   float lev;

   switch (vcs->Kind) {
      case VERT_GENERIC:
      case VERT_EQUAL_KM:
         lev = (height - vcs->Args[0]) / vcs->Args[1];
         break;
      case VERT_UNEQUAL_MB:   /* Unequally spaced mb */
      case VERT_UNEQUAL_KM:   /* Unequally spaced km */
         /* lev will be -1 if out of bounds */
         lev = binary_search( height, vcs->Args, vcs->Nl );
         break;
      case VERT_EPA:
#define Sigma(K)   vcs->Args[(K)]
         {
            float hgt[MAXLEVELS];
            int ilev;
            float psurf, s, p;
            /* compute hgt[ilev] */
            psurf = 1012.5 * exp( -topo_elev / 7.2 );
            for (ilev=0;ilev<vcs->Nl;ilev++) {
               s = Sigma( ilev );
               p = 100.0 + s * (psurf - 100.0);
               hgt[ilev] = -7.2 * log( p / 1012.5 );
            }
            /* search hgt[] for height */
            /* lev will be -1 if out of bounds */
            lev = binary_search( height, hgt, vcs->Nl );
#undef Sigma
         }
         break;
      default:  /* error */
         printf("Error in height_to_level\n");
   }

   /* boundary checks: */
   if ((lev < vcs->LowLev) || (lev > (vcs->Nl-1))) {
      return 0;
   }
   else {
      *level = lev;
      return 1;
   }
}


