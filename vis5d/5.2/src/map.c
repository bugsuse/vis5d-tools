/*  map.c  */


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
/* map lines module */



#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "binio.h"
#include "graphics.h"
#include "globals.h"
#include "map.h"
#include "proj.h"
#include "topo.h"
/* MJK 12.02.98 */
#include "user_data.h"

/* MJK 12.04.98 */
#include "linterp.h"


#ifndef SEEK_SET
#  define SEEK_SET  0
#endif



#define FABSF(X)  ( (float) fabs( (double) (X) ) )




#define CLIP_CODE(x,y,code)     code = 0;                                \
                                if (x<dtx->ClipMin0)       code |= 1;    \
                                else if (x>dtx->ClipMax0)  code |= 2;    \
                                if (y<dtx->ClipMin1)       code |= 4;    \
                                else if (y>dtx->ClipMax1)  code |= 8;


/* MJK 12.04.98 begin */
#define         Z_FUDGE_OFFSET  0.01
#define         Z_FUDGE_FACTOR  0.00150

float   get_z_off (Display_Context dtx, float zmin, float zmax)
{

    float       zoff;

    if (zmin > zmax)
    {
        zmin = height_to_zPRIME (dtx, dtx->MinTopoHgt);
        zmax = height_to_zPRIME (dtx, dtx->MaxTopoHgt);
    }

    zoff = sqrt (((dtx->Xmax-dtx->Xmin)*(dtx->Xmax-dtx->Xmin)) +
                 ((dtx->Ymax-dtx->Ymin)*(dtx->Ymax-dtx->Ymin)) +
                 ((zmax-zmin)*(zmax-zmin))) * Z_FUDGE_FACTOR;


    return zoff;
}

/*
 *  Fit the polyline defined by the 3D coordinates XYZ_IN to the 2D
 *  gridded surface defined by the the 3D vertices VERTS.  The polyline
 *  is regridded by adding vertices wherever the polyline crosses the
 *  grid lines of the surface.
 */

int     bend_line_to_fit_surf (float *verts, int ncols, int nrows,
                               int grid_scheme,
                               float xmin, float ymin, float xmax, float ymax,
                               float zoff,
                               float *xyz_in, int n_in, float *xyz_out)
{

    int         i, nnew;
    float       xfac, yfac, xoff, yoff, xend, yend;
    FLOAT2      *xy, *xynew;



    if (verts   == NULL) return 0;
    if (ncols   <=    1) return 0;
    if (nrows   <=    1) return 0;
    if (xyz_in  == NULL) return 0;
    if (n_in    <=    1) return 0;
    if (xyz_out == NULL) return 0;


    xfac = ((float) (ncols - 1)) / (xmax - xmin);
    yfac = ((float) (nrows - 1)) / (ymax - ymin);



    xy = (FLOAT2 *) malloc (n_in * sizeof (FLOAT2));
    if (xy == NULL) return 0;

    for (i = 0; i < n_in; i++)
    {
        xy[i][0] = (xyz_in[(i*3)+0] - xmin)* xfac;
        xy[i][1] = (ymax - xyz_in[(i*3)+1]) * yfac;
    }

    i    = n_in - 1;
    xend = xyz_in[(i*3)+0];
    yend = xyz_in[(i*3)+1];

    line2d_regrid (xy, n_in, grid_scheme, &xynew, &nnew);



    i = 0;
    xyz_out[(i*3)+0] = xyz_in[(i*3)+0];
    xyz_out[(i*3)+1] = xyz_in[(i*3)+1];
    xyz_out[(i*3)+2] = interp_z (verts, ncols, nrows, grid_scheme,
                                 xy[0][0], xy[0][1]) + zoff;

    for (i = 1; i < nnew-1; i++)
    {
        xyz_out[(i*3)+0] = (xynew[i][0] / xfac) + xmin;
        xyz_out[(i*3)+1] = ymax - (xynew[i][1] / yfac);
        xyz_out[(i*3)+2] = interp_z (verts, ncols, nrows, grid_scheme,
                                     xynew[i][0], xynew[i][1]) + zoff;
    }

    xyz_out[(i*3)+0] = xend;
    xyz_out[(i*3)+1] = yend;
    xyz_out[(i*3)+2] = interp_z (verts, ncols, nrows, grid_scheme,
                                 xy[1][0], xy[1][1]) + zoff;


    if (xynew != NULL) free (xynew);
    free (xy);


    return nnew;
}
/*
 *  Fit the polyline defined by the 3D coordinates XYZ_IN to the
 *  topography surface in the specified context.  The polyline is
 *  regridded by adding vertices wherever the polyline crosses the
 *  topo surface grid lines.
 */

int     bend_line_to_fit_topo (Display_Context dtx, float *xyz_in, int n_in,
                               float *xyz_out)
{

    int         nnew;
    float       zoff;



    if (!dtx->TopoFlag) return 0;
    if (dtx->TopoVertex == NULL) return 0;
    if (xyz_in  == NULL) return 0;
    if (n_in    <=    1) return 0;
    if (xyz_out == NULL) return 0;



    zoff = get_z_off (dtx, 1.0, 0.0);


    nnew = bend_line_to_fit_surf (dtx->TopoVertex, dtx->qcols, dtx->qrows,
                                  INTERP_TRIANGULAR_GRID_SCHEME_NORMAL,
                                  dtx->Xmin, dtx->Ymin, dtx->Xmax, dtx->Ymax,
                                  zoff, xyz_in, n_in, xyz_out);


    return nnew;
}



/*
 *  Fit the most recently saved map line segement in the specified
 *  context to the topography of that context.  Vertices are added to
 *  the segment wherever it crosses topo grid lines.
 */

void bend_map_seg_to_fit_topo (Display_Context dtx)
{

    int         i, ibeg, nnew;



    if (!dtx->TopoFlag) return;
    if (dtx->TopoVertex == NULL) return;


    ibeg = dtx->VertCount - 2;
    if (ibeg < 0) return;



    nnew = bend_line_to_fit_topo (dtx, &(dtx->MapVert[ibeg][0]), 2,
                                  &(dtx->MapVert[ibeg][0]));

    dtx->VertCount          = ibeg + nnew;
    dtx->Len[dtx->SegCount] = dtx->VertCount - dtx->Start[dtx->SegCount];



    if (!dtx->CurvedBox)
    {
        for (i = ibeg; i < dtx->VertCount; i++)
        {
            dtx->FlatMapVert[i][0] = dtx->MapVert[i][0];
            dtx->FlatMapVert[i][1] = dtx->MapVert[i][1];
            dtx->FlatMapVert[i][2] = dtx->Zmin;
        }
    }
    else
    {
        float   x, y, z, flon, flat, fhgt, zmin;

        zmin = dtx->Zmin + get_z_off (dtx, 1.0, 0.0);
        for (i = ibeg; i < dtx->VertCount; i++)
        {
            x = dtx->MapVert[i][0];
            y = dtx->MapVert[i][1];
            xyzPRIME_to_geo (dtx, -1, -1, x, y, zmin, &flat, &flon, &fhgt);
            geo_to_xyzPRIME (dtx, -1, -1, 1, &flat, &flon, &dtx->BottomBound,
                             &x, &y, &z);
            dtx->FlatMapVert[i][0] = x;
            dtx->FlatMapVert[i][1] = y;
            dtx->FlatMapVert[i][2] = z;
        }
    }
}

/* MJK 12.04.98 end */







/* MJK 12.02.98 begin */
static int read_user_map( Display_Context dtx, char *mapname )
{
   int iret = 0;

/*
 *  Call the user's function to get the user's map data.
 *  It is not necessary for this function to actually read a file --
 *  it only needs to put the user's map data into the Vis5D
 *  data structure.
 *
 *  An example for this function can be found in user_data.c.
 */

   iret = user_data_get_map (dtx, mapname);


   return iret;
}
/* MJK 12.02.98 end */



/*
 * Clip the line segment defined by (x0,y0) and (x1,y1) against the
 * bounds of ClipMin0,ClipMax0,ClipMin1 and ClipMax1.
 * Input:  x0,y0, x1,y1 - coordinates of endpoints of line
 *         quickclip - value to use for trival rejection
 * Return:  1 = entire or partial segment resulted
 *          0 = entire line is out of bounds
 */
static int clip( Display_Context dtx, float *x0, float *y0, float *x1, float *y1,
                 float quickclip )
{
   int code0, code1;
   float t, dx, dy;

   if (*x0>quickclip || *x0<-quickclip || *y0>quickclip || *y0<-quickclip ||
       *x1>quickclip || *x1<-quickclip || *y1>quickclip || *y1<-quickclip) {
      /* probably a point mapped to infinity */
      return 0;
   }

   dx = *x1 - *x0;
   dy = *y1 - *y0;

   while (1) {

      /* compute codes */
      CLIP_CODE( *x0, *y0, code0 );
      CLIP_CODE( *x1, *y1, code1 );

      /* check if entirely in bounds */
      if (code0==0 && code1==0)
         return 1;

      /* check if entirely out of bounds */
      if (code0 & code1)
         return 0;

      /* eliminate line segments across the whole box */
      if ((code0 & 1 && code1 & 2) ||
          (code0 & 2 && code1 & 1) ||
          (code0 & 4 && code1 & 8) ||
          (code0 & 8 && code1 & 4))
         return 0;

      if (code0 & 1) {  /* clip against ClipMin0 */
         t = (dtx->ClipMin0 - *x1) / dx;
         *x0 = dtx->ClipMin0;
         *y0 = *y1 + t * dy;
      }
      else if (code0 & 2) {  /* clip against ClipMax0 */
         t = (dtx->ClipMax0 - *x1) / dx;
         *x0 = dtx->ClipMax0;
         *y0 = *y1 + t * dy;
      }
      else if (code0 & 4) {  /* clip against ClipMin1 */
         t = (dtx->ClipMin1 - *y1) / dy;
         *y0 = dtx->ClipMin1;
         *x0 = *x1 + t * dx;
      }
      else if (code0 & 8) {  /* clip against ClipMax1 */
         t = (dtx->ClipMax1 - *y1) / dy;
         *y0 = dtx->ClipMax1;
         *x0 = *x1 + t * dx;
      }
      else if (code1 & 1) {  /* clip against ClipMin0 */
         t = (dtx->ClipMin0 - *x0) / dx;
         *x1 = dtx->ClipMin0;
         *y1 = *y0 + t * dy;
      }
      else if (code1 & 2) {  /* clip against ClipMax0 */
         t = (dtx->ClipMax0 - *x0) / dx;
         *x1 = dtx->ClipMax0;
         *y1 = *y0 + t * dy;
      }
      else if (code1 & 4) {  /* clip against ClipMin1 */
         t = (dtx->ClipMin1 - *y0) / dy;
         *y1 = dtx->ClipMin1;
         *x1 = *x0 + t * dx;
      }
      else if (code1 & 8) {  /* clip against ClipMax1 */
         t = (dtx->ClipMax1 - *y0) / dy;
         *y1 = dtx->ClipMax1;
         *x1 = *x0 + t * dx;
      }

   }

}


#define EPS 0.1

/*
 * Given the geographic coordinates of the end points of a map line,
 * convert the coordinates to graphics coordinates, clip against the 3-D
 * box and save the line if it's visible.
 */
static void add_line( Display_Context dtx, float lat0, float lon0, float hgt0,
                      float lat1, float lon1, float hgt1, int *new )
{
   static float height_kludge = 0.0;

   if (height_kludge==0.0) {
      height_kludge = (dtx->TopBound-dtx->BottomBound) / 25.0;
   }

   if (!dtx->CurvedBox) {
      /* Rectangular box, clip in graphics coords. */

      float x0, y0, z0, x1, y1, z1;
      float flat0, flon0, fhgt0, flat1, flon1, fhgt1;

      /* the arguments are really doubles so we must convert to float */
      flat0 = (float) lat0;
      flon0 = (float) lon0;
      fhgt0 = (float) hgt0;
/* MJK 2.17.99
      geo_to_xyzPRIME( dtx, -1, -1, 1, &flat0, &flon0, &fhgt0, &x0, &y0, &z0 );
*/
      geo_to_xyzTOPO( dtx, -1, -1, 1, &flat0, &flon0, &fhgt0, &x0, &y0, &z0 );

      /* WLH 6 Nov 98 - kludge topo for inverted VERT_GENERIC */
      if (dtx->VerticalSystem == VERT_GENERIC &&
          dtx->TopBound < dtx->BottomBound) {
        z0 = dtx->Zmin + fhgt0 / (dtx->BottomBound-dtx->TopBound)
                 * (dtx->Zmax-dtx->Zmin);
      }

/*      z0 -= 0.01; WLH 4-24-95 */

      flat1 = (float) lat1;
      flon1 = (float) lon1;
      fhgt1 = (float) hgt1;
/* MJK 2.17.99
      geo_to_xyzPRIME( dtx, -1, -1, 1, &flat1, &flon1, &fhgt1, &x1, &y1, &z1 );
*/
      geo_to_xyzTOPO( dtx, -1, -1, 1, &flat1, &flon1, &fhgt1, &x1, &y1, &z1 );

      /* WLH 6 Nov 98 - kludge topo for inverted VERT_GENERIC */
      if (dtx->VerticalSystem == VERT_GENERIC &&
          dtx->TopBound < dtx->BottomBound) {
        z1 = dtx->Zmin + fhgt1 / (dtx->BottomBound-dtx->TopBound)
                 * (dtx->Zmax-dtx->Zmin);
      }

      if (dtx->Projection == PROJ_ROTATED) {
        pandg_for(&flat0, &flon0, dtx->CentralLat, dtx->CentralLon, dtx->Rotation);
        pandg_for(&flat1, &flon1, dtx->CentralLat, dtx->CentralLon, dtx->Rotation);
      }
      if (flon0 < -150.0 && flon1 > 150.0 ||
          flon1 < -150.0 && flon0 > 150.0) {
        x0 = 200.0; /* ensure clipping failure */
      }

/* WLH 8-1-95 */
      if (dtx->Projection == PROJ_LAMBERT ||
          dtx->Projection == PROJ_STEREO) {
        float tlat0, tlon0, thgt0, tlat1, tlon1, thgt1;
        xyzPRIME_to_geo(dtx, -1, -1, x0, y0, z0, &tlat0, &tlon0, &thgt0);
        xyzPRIME_to_geo(dtx, -1, -1, x1, y1, z1, &tlat1, &tlon1, &thgt1);
        if (FABSF(flat0 - tlat0) > EPS) {
          x0 = 200.0; /* ensure clipping failure */
        }
        if (FABSF(flon0 - tlon0) > EPS &&
            FABSF(flon0 - tlon0 - 360.0) > EPS &&
            FABSF(flon0 - tlon0 + 360.0) > EPS) {
          x0 = 200.0; /* ensure clipping failure */
        }
        if (FABSF(flat1 - tlat1) > EPS) {
          x0 = 200.0; /* ensure clipping failure */
        }
        if (FABSF(flon1 - tlon1) > EPS &&
            FABSF(flon1 - tlon1 - 360.0) > EPS &&
            FABSF(flon1 - tlon1 + 360.0) > EPS) {
          x0 = 200.0; /* ensure clipping failure */
        }
      }

      /* kludge z values so they're slightly above the topo */
      z0 += 0.015;
      z1 += 0.015;

      if (clip( dtx, &x0, &y0, &x1, &y1, 100.0 )) {
         /* save the line */
         if (*new==0 && dtx->VertCount>0
                              && dtx->MapVert[dtx->VertCount-1][0]==x0
                              && dtx->MapVert[dtx->VertCount-1][1]==y0) {
            /* add to current polyline */
            dtx->MapVert[dtx->VertCount][0] = x1;
            dtx->MapVert[dtx->VertCount][1] = y1;
            dtx->MapVert[dtx->VertCount][2] = z1;
            dtx->FlatMapVert[dtx->VertCount][0] = x1;
            dtx->FlatMapVert[dtx->VertCount][1] = y1;
            dtx->FlatMapVert[dtx->VertCount][2] = dtx->Zmin+0.01;
            dtx->VertCount++;
            dtx->Len[dtx->SegCount]++;
         }
         else {
            /* begin a new polyline */
            *new = 0;
            if (dtx->Len[dtx->SegCount]>0) {
               dtx->SegCount++;
               dtx->Start[dtx->SegCount] = dtx->VertCount;
            }
            dtx->MapVert[dtx->VertCount][0] = x0;
            dtx->MapVert[dtx->VertCount][1] = y0;
            dtx->MapVert[dtx->VertCount][2] = z0;
            dtx->FlatMapVert[dtx->VertCount][0] = x0;
            dtx->FlatMapVert[dtx->VertCount][1] = y0;
            dtx->FlatMapVert[dtx->VertCount][2] = dtx->Zmin+0.01;
            dtx->VertCount++;
            dtx->MapVert[dtx->VertCount][0] = x1;
            dtx->MapVert[dtx->VertCount][1] = y1;
            dtx->MapVert[dtx->VertCount][2] = z1;
            dtx->FlatMapVert[dtx->VertCount][0] = x1;
            dtx->FlatMapVert[dtx->VertCount][1] = y1;
            dtx->FlatMapVert[dtx->VertCount][2] = dtx->Zmin+0.01;
            dtx->VertCount++;
            dtx->Len[dtx->SegCount] = 2;
         }
   
         /* MJK 12.15.98 */
         bend_map_seg_to_fit_topo (dtx);

      }
   }
   else {
      /* Curved box, clip in lat/lon coordinates */

      float x0, y0, z0, x1, y1, z1;
      float flat0, flon0, fhgt0, flat1, flon1, fhgt1;

      /* the arguments are really doubles so we must convert to float */
      flat0 = (float) lat0;
      flon0 = (float) lon0;
      fhgt0 = (float) hgt0 + height_kludge;
      flat1 = (float) lat1;
      flon1 = (float) lon1;
      fhgt1 = (float) hgt1 + height_kludge;

      if (clip( dtx, &flat0, &flon0, &flat1, &flon1, 1000.0 )) {
         /* save the line */
         float fx0, fy0, fz0, fx1, fy1, fz1;

         /* graphics coords for raised map lines */
/* MJK 2.17.99
         geo_to_xyzPRIME( dtx, -1, -1, 1, &flat0, &flon0, &fhgt0, &x0, &y0, &z0 );
         geo_to_xyzPRIME( dtx, -1, -1, 1, &flat1, &flon1, &fhgt1, &x1, &y1, &z1 );

         geo_to_xyzPRIME( dtx, -1, -1, 1, &flat0, &flon0, &dtx->BottomBound,
                     &fx0, &fy0, &fz0 );
         geo_to_xyzPRIME( dtx, -1, -1, 1, &flat1, &flon1, &dtx->BottomBound,
                     &fx1, &fy1, &fz1 );
*/
         geo_to_xyzTOPO( dtx, -1, -1, 1, &flat0, &flon0, &fhgt0, &x0, &y0, &z0 );
         geo_to_xyzTOPO( dtx, -1, -1, 1, &flat1, &flon1, &fhgt1, &x1, &y1, &z1 );

         /* graphics coords for 'flat' map lines */
         geo_to_xyzTOPO( dtx, -1, -1, 1, &flat0, &flon0, &dtx->BottomBound,
                     &fx0, &fy0, &fz0 );
         geo_to_xyzTOPO( dtx, -1, -1, 1, &flat1, &flon1, &dtx->BottomBound,
                     &fx1, &fy1, &fz1 );


         if (*new==0 && dtx->VertCount>0
                              && dtx->MapVert[dtx->VertCount-1][0]==x0
                              && dtx->MapVert[dtx->VertCount-1][1]==y0) {
            /* add to current polyline */
            dtx->MapVert[dtx->VertCount][0] = x1;
            dtx->MapVert[dtx->VertCount][1] = y1;
            dtx->MapVert[dtx->VertCount][2] = z1;
            dtx->FlatMapVert[dtx->VertCount][0] = fx1;
            dtx->FlatMapVert[dtx->VertCount][1] = fy1;
            dtx->FlatMapVert[dtx->VertCount][2] = fz1;
            dtx->VertCount++;
            dtx->Len[dtx->SegCount]++;
         }
         else {
            /* begin a new polyline */
            *new = 0;
            if (dtx->Len[dtx->SegCount]>0) {
               dtx->SegCount++;
               dtx->Start[dtx->SegCount] = dtx->VertCount;
            }
            dtx->MapVert[dtx->VertCount][0] = x0;
            dtx->MapVert[dtx->VertCount][1] = y0;
            dtx->MapVert[dtx->VertCount][2] = z0;
            dtx->FlatMapVert[dtx->VertCount][0] = fx0;
            dtx->FlatMapVert[dtx->VertCount][1] = fy0;
            dtx->FlatMapVert[dtx->VertCount][2] = fz0;
            dtx->VertCount++;
            dtx->MapVert[dtx->VertCount][0] = x1;
            dtx->MapVert[dtx->VertCount][1] = y1;
            dtx->MapVert[dtx->VertCount][2] = z1;
            dtx->FlatMapVert[dtx->VertCount][0] = fx1;
            dtx->FlatMapVert[dtx->VertCount][1] = fy1;
            dtx->FlatMapVert[dtx->VertCount][2] = fz1;
            dtx->VertCount++;
            dtx->Len[dtx->SegCount] = 2;
         }
         /* MJK 12.15.98 */
/* by removing this if projection = cylindrical it
   make's it work 
         bend_map_seg_to_fit_topo (dtx);
*/
      }

   }
}



/*
 * Read a map file and make the graphics.
 * Input:  mapname - name of map file
 * Return:  0 = error, non-zero = success
 */
int init_map( Display_Context dtx, char *mapname )
{
   int mapflag, mapfile;
   int nsegs;
   int *dir, *verts;
   int i, j, offset, length, new;
   float prevlat, prevlon, prevhgt;
   float lat, lon, hgt;
   float start_shift, end_shift, lon_shift;

   /* MJK 12.02.98 begin */
   mapflag = -1;
   if (dtx->UserMapsFlag) {
      mapflag = read_user_map (dtx, mapname);
      if (mapflag != -1) return mapflag;
   }
   /* MJK 12.02.98 end */

   mapfile = open( mapname, O_RDONLY );
   if (mapfile<0) {
      printf("Map file %s not found\n", mapname );
      return 0;
   }

   /* read number of segments (number of polylines) */
   if (!read_int4( mapfile, &nsegs)) {
      close( mapfile );
      return 0;
   }

   /* allocate and read segment directory */
   dir = (int *) malloc( nsegs * 6 * sizeof(int) );
   if (!dir) {
      close( mapfile );
      return 0;
   }
   if (read_int4_array( mapfile, dir, nsegs*6) < nsegs*6) {
      free( dir );
      close( mapfile );
      return 0;
   }

   dtx->SegCount = dtx->VertCount = 0;
   dtx->Start[0] = 0;
   dtx->Len[0] = 0;
   new = 1;

   if (dtx->CurvedBox==0) {
      /* rectangular box, clip in graphics coords */
      dtx->ClipMin0 = dtx->Xmin;
      dtx->ClipMax0 = dtx->Xmax;
      dtx->ClipMin1 = dtx->Ymin;
      dtx->ClipMax1 = dtx->Ymax;
   }
   else {
      /* curved box, clip in lat/lon coords */
      dtx->ClipMin0 = dtx->SouthBound;
      dtx->ClipMax0 = dtx->NorthBound;
      dtx->ClipMin1 = dtx->EastBound;
      dtx->ClipMax1 = dtx->WestBound;
   }

   /*
    * Map vertices are in lat/lon coordinates whose ranges are [-90,90]
    * and [-180,180], respectively.  If the dataset's longitude boundaries
    * are outside the [-180,180] range, we must make multiple passes
    * through the map line list, shifting the longitudes by +/- 360 
    * degrees to make sure we get all the map lines we're supposed to see.
    */
/* WLH 8-1-95 */
   if (dtx->Projection == PROJ_LAMBERT ||
       dtx->Projection == PROJ_STEREO ||
       dtx->Projection == PROJ_MERCATOR) {
     start_shift = -360.0;
     end_shift = 360.1;
   }
   else {
     if (dtx->EastBound<-180.0) start_shift = -360.0;
     else start_shift = 0.0;
     if (dtx->WestBound>180.0) end_shift = 360.0;
     else end_shift = 0.0;
   }

   for (lon_shift=start_shift; lon_shift<=end_shift; lon_shift+=360.0) {

      for (i=0;i<nsegs && dtx->SegCount<MAXMAPSEG;i++) {
         /* WLH 6-9-95  kludge for bad segment in OUTLCOPL */
         if (dir[6*i] == 0 && dir[6*i+1] == 0 &&
             dir[6*i+2] == 0 && dir[6*i+3] == 0) continue;
         /* get list of vertices for ith segment */
         offset = dir[i*6+4] * 4;
         if (lseek(mapfile,offset,SEEK_SET)<0) {
            free( dir );
            close( mapfile);
            return 0;
         }
         length = dir[i*6+5];
         /* read vertices from disk */
         verts = (int *) malloc( length * sizeof(int) );
         if (read_int4_array( mapfile, verts, length) !=length ) {
            free( dir );
            free( verts );
            close( mapfile );
            return 0;
         }

         for (j=0;j<length/2 && dtx->VertCount<MAXMAPVERT;j++) {
            if (j==0) {
               prevlat = (float) verts[j*2] * 0.0001;
               prevlon = (float) verts[j*2+1] * 0.0001 + lon_shift;
               prevhgt = elevation(dtx,prevlat,prevlon,NULL) / 1000.0;
               new = 1;
            }
            else {
               lat = (float) verts[j*2] * 0.0001;
               lon = (float) verts[j*2+1] * 0.0001 + lon_shift;
               hgt = elevation(dtx,lat,lon,NULL) / 1000.0;
               if (hgt!=hgt) {
                  printf("nan hgt!\n");
               }
               add_line( dtx, prevlat, prevlon, prevhgt, lat, lon, hgt, &new );
               prevlat = lat;
               prevlon = lon;
               prevhgt = hgt;
            }
         }
         if (dtx->Len[dtx->SegCount]>0) {
            dtx->SegCount++;
            dtx->Start[dtx->SegCount] = dtx->VertCount;
            dtx->Len[dtx->SegCount] = 0;
         }

         free( verts );
      }
   }

   if (dtx->Len[dtx->SegCount]>0) {
      dtx->SegCount++;
   }

   free( dir );
   close( mapfile );

/*
   printf("done (SegCount=%d, VertCount=%d)\n", dtx->SegCount, dtx->VertCount);
   for (i=0;i<dtx->SegCount;i++) {
      printf("%3d:  %5d %5d\n", i, dtx->Start[i], dtx->Len[i] );
   }
*/
   return 1;
}






/*
 * Draw the map.
 * Input:  time - time step
 *         flat - 1 = draw flat map
 *                  0 = draw raised map
 * Return:  nothing.
 */
int draw_map( Display_Context dtx, int time, int flat )
{
   int i;

   if (flat) {
      /* draw a flat map */
      for (i=0;i<dtx->SegCount;i++) {
         polyline( dtx->FlatMapVert+dtx->Start[i], dtx->Len[i] );
      }
   }
   else {
      /* draw a map with heights */
      for (i=0;i<dtx->SegCount;i++) {
         polyline( dtx->MapVert+dtx->Start[i], dtx->Len[i] );
      }
   }
   return 0;
}


