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
 * Map projection and vertical coordinate system arithmetic.
 * This file contains functions to convert coordinates between the
 * grid, geographic and grapic coordinate systems.
 *
 * It should be straight forward to add new projection types in this file.
 */



#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "globals.h"
#include "proj.h"
#include "box.h"
#include "api.h"


#define ABS(X)  ( (X) < 0 ? -(X) : (X) )

#ifndef M_PI
#  define M_PI 3.14159265
#endif

#ifdef BIG_GFX
#  define MAX_CONV_VERTS 1200000    /* in an isosurface */
#else
#  define MAX_CONV_VERTS 650000     /* in an isosurface */
#endif

#define DEG2RAD    (M_PI/180.0)
#define RAD2DEG    (180.0/M_PI)
#define RADIUS     6371.23


/* for PROJ_SPHERICAL: */
#define SPHERE_SIZE    0.5
#define SPHERE_SCALE   0.125


/* Convert Height to Pressure: */
#define HGT_TO_P( H )   ( ctx->LogScale * exp( H / ctx->LogExp ) )
#define HGT_TO_PPRIME( H ) ( dtx->LogScale * exp( H / dtx->LogExp ) )

/* Convert Pressure to Height: */
#define P_TO_HGT( P )   ( ctx->LogExp * log( P / ctx->LogScale ) )
#define P_TO_HGTPRIME( P )    ( dtx->LogExp * log( P / dtx->LogScale ) )
#define P_TO_HGT_d( P )   ( dtx->LogExp * log( P / dtx->LogScale ) ) 

/* Convert Pressure to graphics Z: */
#define P_TO_Z( P )     (ctx->dpy_ctx->Zmin + (ctx->dpy_ctx->Zmax-ctx->dpy_ctx->Zmin) * \
                        (P - ctx->Pbot) / (ctx->Ptop-ctx->Pbot))
#define P_TO_ZPRIME( P )     (dtx->Zmin + (dtx->Zmax-dtx->Zmin) * \
                             (P - dtx->Pbot) / (dtx->Ptop-dtx->Pbot)) 

/* Convert graphics Z to Pressure: */
#define Z_TO_P( Z )     (ctx->Pbot + (z-ctx->dpy_ctx->Zmin) * \
                        (ctx->Ptop-ctx->Pbot) / (ctx->dpy_ctx->Zmax-ctx->dpy_ctx->Zmin))
#define ZPRIME_TO_PPRIME( Z )   (dtx->Pbot + (z-dtx->Zmin) * \
                        (dtx->Ptop-dtx->Pbot) / (dtx->Zmax-dtx->Zmin))

/*
 * IF Projection==PROJ_GENERIC THEN
 *        NorthBound = physical location of grid row 0, no units
 *        WestBound = physical location of grid column 0, no units
 *        RowInc = phys. location increment between rows, no units
 *        ColInc = phys. location increment between columns, no units
 *        [Units increase to the left and upward]
 * ELSE IF Projection==PROJ_LINEAR THEN
 *        NorthBound = Latitude of grid row 0, in degrees
 *        WestBound = Longitude of grid column 0, in degrees
 *        RowInc = degrees of latitude between grid rows
 *        ColInc = degrees of longitude between grid columns
 *        [Degrees increase to the left and upward]
 * ELSE IF Projection==PROJ_LAMBERT THEN
 *        Lat1, Lat2 = standard latitudes of conical projection
 *        Note:  Lat1 must be >= Lat2
 *               if (Lat1>0 and Lat2>0) then
 *                  northern hemisphere
 *               else if (Lat1<0 and Lat2<0) then
 *                  southern hemisphere
 *               else
 *                  error
 *               endif
 *               if Lat1==Lat2 then
 *                  Polar Stereographic projection
 *               endif
 *        PoleRow, PoleCol = row and column of north/south pole
 *        CentralLon = which longitude is parallel to the columns
 *        ColInc = increment between grid columns in kilometers
 *        Cone = Cone constant
 *        Hemisphere = +1=northern, -1=southern
 *        ConeFactor = A useful quantity
 * ELSE IF Projection==PROJ_STEREO THEN
 *        CentralLat - latitude of center in degrees
 *        CentralLon - longitude of center in degrees
 *        CentralRow - row # of center
 *        CentralCol - column # of center
 *        ColInc - spacing between columns at center in kilometers
 *        CosCentralLat, SinCentralLat;
 *        StereoScale, InvScale;
 * ELSE IF Projection==PROJ_ROTATED THEN
 *        NorthBound = Latitude on rotated globe of grid row 0, in degrees
 *        WestBound = Longitude on rotated globe of grid column 0, in degrees
 *        RowInc = degrees of latitude on rotated globe between grid rows
 *        ColInc = degrees of longitude on rotated globe between grid columns
 *        CentralLat - Earth latitude of (0, 0) on rotated globe, in radians
 *        CentralLon - Earth longitude of (0, 0) on rotated globe, in radians
 *        Rotation = Clockwise rotation of rotated globe in radians
 *        [Degrees increase to the left and upward]
 * ELSE IF Projection==PROJ_CYLINDRICAL THEN
 *        Use same paramenters as for PROJ_LINEAR, plus:
 *        CylinderScale =  A useful value
 * ELSE IF Projection==PROJ_SPHERICAL THEN
 *        Use same parameters as for PROJ_LINEAR, plus:
 * ENDIF
 */




/* Return the sign of x */
static float sign( float x )
{
   if (x<0.0) {
      return -1.0;
   }
   else if (x>0.0) {
      return 1.0;
   }
   else {
      return 0.0;
   }
}




/*
 * Initialize all map projection stuff.
 * Input:  ctx - the vis5d context
 * Return:  1 = success, 0 = failure
 */
int setup_ctx_projection( Context ctx )
{
   float lat1, lat2;
   float *projargs;

   /*
    * Usually, we use the projection info from the v5d file but if
    * ctx->UserProjection>0 then we use the projection parameters from
    * vis5d_init_projection().
    */

   if (ctx->dpy_ctx->UserProjection>=0) {
      projargs = ctx->dpy_ctx->UserProjArgs;
      ctx->Projection = ctx->dpy_ctx->UserProjection;
   }
   else {
      projargs = ctx->G.ProjArgs;
      ctx->Projection = ctx->G.Projection;
   }

   switch (ctx->Projection) {
      case PROJ_GENERIC:
         /* FALL-THROUGH */
      case PROJ_LINEAR:
      case PROJ_CYLINDRICAL:
      case PROJ_SPHERICAL:
         ctx->NorthBound = projargs[0];
         ctx->WestBound  = projargs[1];
         ctx->RowInc     = projargs[2];
         ctx->ColInc     = projargs[3];
         break;
      case PROJ_MERCATOR:
         ctx->CentralLat = projargs[0];
         ctx->CentralLon = projargs[1];
         ctx->RowIncKm = projargs[2];
         ctx->ColIncKm = projargs[3];
         break;
      case PROJ_ROTATED:
         ctx->NorthBound = projargs[0];
         ctx->WestBound  = projargs[1];
         ctx->RowInc     = projargs[2];
         ctx->ColInc     = projargs[3];
         ctx->CentralLat = DEG2RAD * projargs[4];
         ctx->CentralLon = DEG2RAD * projargs[5];
         ctx->Rotation   = DEG2RAD * projargs[6];
         break;
      case PROJ_LAMBERT:
         ctx->Lat1       = projargs[0];
         ctx->Lat2       = projargs[1];
         ctx->PoleRow    = projargs[2];
         ctx->PoleCol    = projargs[3];
         ctx->CentralLon = projargs[4];
         ctx->ColInc     = projargs[5];
         break;
      case PROJ_STEREO:
         ctx->CentralLat = projargs[0];
         ctx->CentralLon = projargs[1];
         ctx->CentralRow = projargs[2];
         ctx->CentralCol = projargs[3];
         ctx->ColInc     = projargs[4];
         break;
      default:
         printf("Error: unknown projection type in grid.c\n");
         return 0;
   }


   /*
    * Precompute useful values for coordinate transformations.
    */
   switch (ctx->Projection) {
      case PROJ_GENERIC:
      case PROJ_LINEAR:
         ctx->SouthBound = ctx->NorthBound - ctx->RowInc * (ctx->Nr-1);
         ctx->EastBound = ctx->WestBound - ctx->ColInc * (ctx->Nc-1);

         break;
      case PROJ_LAMBERT:
         if (ctx->Lat1==ctx->Lat2) {
            /* polar stereographic? */
            if (ctx->Lat1>0.0) {
               lat1 = (90.0 - ctx->Lat1) * DEG2RAD;
            }
            else {
               lat1 = (90.0 + ctx->Lat1) * DEG2RAD;
            }
            ctx->Cone = cos( lat1 );
            ctx->Hemisphere = 1.0;

         }
         else {
            /* general Lambert conformal */
            float a, b;
            if (sign(ctx->Lat1) != sign(ctx->Lat2)) {
               printf("Error: standard latitudes must have the same sign.\n");
               return 0;
            }
            if (ctx->Lat1<ctx->Lat2) {
               printf("Error: Lat1 must be >= ctx->Lat2\n");
               return 0;
            }
            ctx->Hemisphere = 1.0;

            lat1 = (90.0 - ctx->Lat1) * DEG2RAD;
            lat2 = (90.0 - ctx->Lat2) * DEG2RAD;
            a = log(sin(lat1)) - log(sin(lat2));
            b = log( tan(lat1/2.0) ) - log( tan(lat2/2.0) );
            ctx->Cone = a / b;

         }

         /* Cone is in [-1,1] */
         ctx->ConeFactor = RADIUS * sin(lat1)
                          / (ctx->ColInc * ctx->Cone
                             * pow(tan(lat1/2.0), ctx->Cone) );

         break;
      case PROJ_STEREO:
         ctx->CosCentralLat = cos( ctx->CentralLat * DEG2RAD );
         ctx->SinCentralLat = sin( ctx->CentralLat * DEG2RAD );
         ctx->StereoScale = (2.0 * RADIUS / ctx->ColInc);
         ctx->InvScale = 1.0 / ctx->StereoScale;

         break;
      case PROJ_ROTATED:
         ctx->SouthBound = ctx->NorthBound - ctx->RowInc * (ctx->Nr-1);
         ctx->EastBound = ctx->WestBound - ctx->ColInc * (ctx->Nc-1);

         break;
      case PROJ_CYLINDRICAL:
         if (REVERSE_POLES==-1){
            ctx->CylinderScale = 1.0 / (-1.0*(-90.0-ctx->NorthBound));
         }
         else{
            ctx->CylinderScale = 1.0 / (90.0-ctx->SouthBound);
         }
         ctx->SouthBound = ctx->NorthBound - ctx->RowInc * (ctx->Nr-1);
         ctx->EastBound = ctx->WestBound - ctx->ColInc * (ctx->Nc-1);

         break;
      case PROJ_SPHERICAL:
         ctx->SouthBound = ctx->NorthBound - ctx->RowInc * (ctx->Nr-1);
         ctx->EastBound = ctx->WestBound - ctx->ColInc * (ctx->Nc-1);

         break;
      case PROJ_MERCATOR:
         break;
      default:
         printf("Error in set_projection\n");
         return 0;
   }
   /* MJK 12.28.99 */
   if (ctx->Projection != PROJ_GENERIC && ctx->Projection != PROJ_MERCATOR) {
/*
   if (ctx->Projection != PROJ_GENERIC) {
*/
     if (ctx->SouthBound < -90.0) {
       printf("SouthBound less than -90.0\n");
       return 0;
     }
     if (ctx->NorthBound < ctx->SouthBound) {
       printf("NorthBound less than SouthBound\n");
       return 0;
     }
     if (90.0 < ctx->NorthBound) {
       printf("NorthBound greater than 90.0\n");
       return 0;
     }
   }
   return 1;
}

int setup_ctx_dtx_projection(Context ctx )
{
   float lat1, lat2;
   float *projargs;

   /*
    * Usually, we use the projection info from the v5d file but if
    * ctx->UserProjection>0 then we use the projection parameters from
    * vis5d_init_projection().
    */

   if (ctx->dpy_ctx->UserProjection>=0) {
      projargs = ctx->dpy_ctx->UserProjArgs;
      ctx->Projection = ctx->dpy_ctx->UserProjection;
      ctx->dpy_ctx->Projection = ctx->dpy_ctx->UserProjection;
   }
   else {
      projargs = ctx->G.ProjArgs;
      ctx->Projection = ctx->G.Projection;
      ctx->dpy_ctx->Projection = ctx->G.Projection;
   }

   switch (ctx->Projection) {
      case PROJ_GENERIC:
         /* FALL-THROUGH */
      case PROJ_LINEAR:
      case PROJ_CYLINDRICAL:
      case PROJ_SPHERICAL:
         ctx->NorthBound = projargs[0];
         ctx->WestBound  = projargs[1];
         ctx->RowInc     = projargs[2];
         ctx->ColInc     = projargs[3];
         ctx->dpy_ctx->NorthBound = projargs[0];
         ctx->dpy_ctx->WestBound  = projargs[1];
         ctx->dpy_ctx->RowInc     = projargs[2];
         ctx->dpy_ctx->ColInc     = projargs[3];
         break;
      case PROJ_MERCATOR:
         ctx->CentralLat = projargs[0];
         ctx->CentralLon = projargs[1];
         ctx->RowIncKm = projargs[2];
         ctx->ColIncKm = projargs[3];
         ctx->dpy_ctx->CentralLat = projargs[0];
         ctx->dpy_ctx->CentralLon = projargs[1];
         ctx->dpy_ctx->RowIncKm = projargs[2];
         ctx->dpy_ctx->ColIncKm = projargs[3];
         break;
      case PROJ_ROTATED:
         ctx->NorthBound = projargs[0];
         ctx->WestBound  = projargs[1];
         ctx->RowInc     = projargs[2];
         ctx->ColInc     = projargs[3];
         ctx->CentralLat = DEG2RAD * projargs[4];
         ctx->CentralLon = DEG2RAD * projargs[5];
         ctx->Rotation   = DEG2RAD * projargs[6];
         ctx->dpy_ctx->NorthBound = projargs[0];
         ctx->dpy_ctx->WestBound  = projargs[1];
         ctx->dpy_ctx->RowInc     = projargs[2];
         ctx->dpy_ctx->ColInc     = projargs[3];
         ctx->dpy_ctx->CentralLat = DEG2RAD * projargs[4];
         ctx->dpy_ctx->CentralLon = DEG2RAD * projargs[5];
         ctx->dpy_ctx->Rotation   = DEG2RAD * projargs[6];
         break;
      case PROJ_LAMBERT:
         ctx->Lat1       = projargs[0];
         ctx->Lat2       = projargs[1];
         ctx->PoleRow    = projargs[2];
         ctx->PoleCol    = projargs[3];
         ctx->CentralLon = projargs[4];
         ctx->ColInc     = projargs[5];
         ctx->dpy_ctx->Lat1       = projargs[0];
         ctx->dpy_ctx->Lat2       = projargs[1];
         ctx->dpy_ctx->PoleRow    = projargs[2];
         ctx->dpy_ctx->PoleCol    = projargs[3];
         ctx->dpy_ctx->CentralLon = projargs[4];
         ctx->dpy_ctx->ColInc     = projargs[5];
         break;
      case PROJ_STEREO:
         ctx->CentralLat = projargs[0];
         ctx->CentralLon = projargs[1];
         ctx->CentralRow = projargs[2];
         ctx->CentralCol = projargs[3];
         ctx->ColInc     = projargs[4];
         ctx->dpy_ctx->CentralLat = projargs[0];
         ctx->dpy_ctx->CentralLon = projargs[1];
         ctx->dpy_ctx->CentralRow = projargs[2];
         ctx->dpy_ctx->CentralCol = projargs[3];
         ctx->dpy_ctx->ColInc     = projargs[4];
         break;
      default:
         printf("Error: unknown projection type in grid.c\n");
         return 0;
   }


   /*
    * Precompute useful values for coordinate transformations.
    */
   switch (ctx->Projection) {
      case PROJ_GENERIC:
      case PROJ_LINEAR:
         ctx->SouthBound = ctx->NorthBound - ctx->RowInc * (ctx->Nr-1);
         ctx->EastBound = ctx->WestBound - ctx->ColInc * (ctx->Nc-1);

         ctx->dpy_ctx->SouthBound = ctx->NorthBound - ctx->RowInc * (ctx->Nr-1);
         ctx->dpy_ctx->EastBound = ctx->WestBound - ctx->ColInc * (ctx->Nc-1);
         break;
      case PROJ_LAMBERT:
         if (ctx->Lat1==ctx->Lat2) {
            /* polar stereographic? */
            if (ctx->Lat1>0.0) {
               lat1 = (90.0 - ctx->Lat1) * DEG2RAD;
            }
            else {
               lat1 = (90.0 + ctx->Lat1) * DEG2RAD;
            }
            ctx->Cone = cos( lat1 );
            ctx->Hemisphere = 1.0;

            ctx->dpy_ctx->Cone = cos( lat1 );
            ctx->dpy_ctx->Hemisphere = 1.0;
         }
         else {
            /* general Lambert conformal */
            float a, b;
            if (sign(ctx->Lat1) != sign(ctx->Lat2)) {
               printf("Error: standard latitudes must have the same sign.\n");
               return 0;
            }
            if (ctx->Lat1<ctx->Lat2) {
               printf("Error: Lat1 must be >= ctx->Lat2\n");
               return 0;
            }
            ctx->Hemisphere = 1.0;

            ctx->dpy_ctx->Hemisphere = 1.0;
            lat1 = (90.0 - ctx->Lat1) * DEG2RAD;
            lat2 = (90.0 - ctx->Lat2) * DEG2RAD;
            a = log(sin(lat1)) - log(sin(lat2));
            b = log( tan(lat1/2.0) ) - log( tan(lat2/2.0) );
            ctx->Cone = a / b;

            ctx->dpy_ctx->Cone = a / b;
         }

         /* Cone is in [-1,1] */
         ctx->ConeFactor = RADIUS * sin(lat1)
                          / (ctx->ColInc * ctx->Cone
                             * pow(tan(lat1/2.0), ctx->Cone) );

         ctx->dpy_ctx->ConeFactor = RADIUS * sin(lat1)
                          / (ctx->ColInc * ctx->Cone
                             * pow(tan(lat1/2.0), ctx->Cone) );
         break;
      case PROJ_STEREO:
         ctx->CosCentralLat = cos( ctx->CentralLat * DEG2RAD );
         ctx->SinCentralLat = sin( ctx->CentralLat * DEG2RAD );
         ctx->StereoScale = (2.0 * RADIUS / ctx->ColInc);
         ctx->InvScale = 1.0 / ctx->StereoScale;

         ctx->dpy_ctx->CosCentralLat = cos( ctx->CentralLat * DEG2RAD );
         ctx->dpy_ctx->SinCentralLat = sin( ctx->CentralLat * DEG2RAD );
         ctx->dpy_ctx->StereoScale = (2.0 * RADIUS / ctx->ColInc);
         ctx->dpy_ctx->InvScale = 1.0 / ctx->StereoScale;
         break;
      case PROJ_ROTATED:
         ctx->SouthBound = ctx->NorthBound - ctx->RowInc * (ctx->Nr-1);
         ctx->EastBound = ctx->WestBound - ctx->ColInc * (ctx->Nc-1);

         ctx->dpy_ctx->SouthBound = ctx->NorthBound - ctx->RowInc * (ctx->Nr-1);
         ctx->dpy_ctx->EastBound = ctx->WestBound - ctx->ColInc * (ctx->Nc-1);
         break;
      case PROJ_CYLINDRICAL:
         if (REVERSE_POLES==-1){
            ctx->CylinderScale = 1.0 / (-1.0*(-90.0-ctx->NorthBound));
         }
         else{
            ctx->CylinderScale = 1.0 / (90.0-ctx->SouthBound);
         }
         ctx->SouthBound = ctx->NorthBound - ctx->RowInc * (ctx->Nr-1);
         ctx->EastBound = ctx->WestBound - ctx->ColInc * (ctx->Nc-1);

         if (REVERSE_POLES==-1){         
            ctx->dpy_ctx->CylinderScale = 1.0 / (-1.0*(-90.0-ctx->NorthBound));
         }
         else{         
            ctx->dpy_ctx->CylinderScale = 1.0 / (90.0-ctx->SouthBound);
         }         
         ctx->dpy_ctx->SouthBound = ctx->NorthBound - ctx->RowInc * (ctx->Nr-1);
         ctx->dpy_ctx->EastBound = ctx->WestBound - ctx->ColInc * (ctx->Nc-1);
         break;
      case PROJ_SPHERICAL:
         ctx->SouthBound = ctx->NorthBound - ctx->RowInc * (ctx->Nr-1);
         ctx->EastBound = ctx->WestBound - ctx->ColInc * (ctx->Nc-1);

         ctx->dpy_ctx->SouthBound = ctx->NorthBound - ctx->RowInc * (ctx->Nr-1);
         ctx->dpy_ctx->EastBound = ctx->WestBound - ctx->ColInc * (ctx->Nc-1);
         break;
      case PROJ_MERCATOR:
         break;
      default:
         printf("Error in set_projection\n");
         return 0;
   }
   /* MJK 12.28.99 */
   if (ctx->Projection != PROJ_GENERIC && ctx->Projection != PROJ_MERCATOR) {
/*
   if (ctx->Projection != PROJ_GENERIC || ctx->Projection != PROJ_MERCATOR) {
*/
     if (ctx->SouthBound < -90.0) {
       printf("SouthBound less than -90.0\n");
       return 0;
     }
     if (ctx->NorthBound < ctx->SouthBound) {
       printf("NorthBound less than SouthBound\n");
       return 0;
     }
     if (90.0 < ctx->NorthBound) {
       printf("NorthBound greater than 90.0\n");
       return 0;
     }
   }
   ctx->GridSameAsGridPRIME = vis5d_check_dtx_same_as_ctx(ctx->dpy_ctx->dpy_context_index,
                                                          ctx->context_index);

   return 1;
}




/*
 * Initialize the vertical coordinate system.
 * Return:  1 = success, 0 = error
 */
int setup_ctx_vertical_system( Context ctx )
{
   int i;
   float pressure;
   float *vertargs;

   /*
    * Usually, we use the VCS (Vertical Coordinate System) info from the
    * v5d file.  However, if the user VCS variable is set we use the VCS
    * info from vis5d_init_vertical() function.
    */

   if (ctx->dpy_ctx->UserVerticalSystem>=0) {
      /* use user-provided parameters */
      vertargs = ctx->dpy_ctx->UserVertArgs;
      ctx->VerticalSystem = ctx->dpy_ctx->UserVerticalSystem;
   }
   else {
      /* use parameters from v5d file */
      vertargs = ctx->G.VertArgs;
      ctx->VerticalSystem = ctx->G.VerticalSystem;
   }

   switch (ctx->VerticalSystem) {
      case VERT_GENERIC:
         /* FALL-THROUGH */
      case VERT_EQUAL_KM:
         ctx->BottomBound = vertargs[0];
         ctx->LevInc      = vertargs[1];
         ctx->TopBound = ctx->BottomBound + ctx->LevInc * (ctx->MaxNl-1);
         for (i=0;i<ctx->MaxNl;i++) {
            ctx->Height[i] = ctx->BottomBound + i * ctx->LevInc;
         }
         break;
      case VERT_NONEQUAL_MB:
         /* FALL-THROUGH */
      case VERT_NONEQUAL_KM:
         for (i=0;i<ctx->MaxNl;i++) {
            ctx->Height[i] = vertargs[i];
         }
         ctx->BottomBound = ctx->Height[0];
         ctx->TopBound = ctx->Height[ctx->MaxNl-1];
         break;
      default:
         printf("Error in grid.c, unknown vertical coord system\n");
         return 0;
   }

   /* Precompute useful values */
   switch (ctx->VerticalSystem) {
      case VERT_GENERIC:
      case VERT_EQUAL_KM:
         ctx->TopBound = ctx->BottomBound + ctx->LevInc * (ctx->MaxNl-1);
         for (i=0;i<ctx->MaxNl;i++) {
            ctx->Height[i] = ctx->BottomBound + i * ctx->LevInc;
         }
         if (ctx->LogFlag) {
           ctx->Ptop = ctx->LogScale * exp( ctx->TopBound / ctx->LogExp );
           ctx->Pbot = ctx->LogScale * exp( ctx->BottomBound / ctx->LogExp );
         }
         break;
      case VERT_NONEQUAL_KM:
         if (ctx->LogFlag) {
           ctx->Ptop = ctx->LogScale * exp( ctx->Height[ctx->MaxNl-1] / ctx->LogExp );
           ctx->Pbot = ctx->LogScale * exp( ctx->Height[0] / ctx->LogExp );
         }
         break;
      case VERT_NONEQUAL_MB:
         ctx->Ptop = height_to_pressure(ctx->Height[ctx->MaxNl-1]);
         ctx->Pbot = height_to_pressure(ctx->Height[0]);
         /* The Height[] array should be OK at this point */
         break;
      default:
         return 0;
   }
/* WLH 3 Nov 98
   if (ctx->TopBound < ctx->BottomBound) {
       printf("TopBound less than BottomBound\n");
       return 0;
   }
*/
   /* hack for spherical mjk 4-20-98 */
   if (ctx->Projection == PROJ_SPHERICAL &&
       ctx->TopBound == ctx->BottomBound){
      ctx->TopBound = ctx->BottomBound+.01;
   }

   return 1;
}

int setup_ctx_dtx_vertical_system( Context ctx )
{
   int i;
   float pressure;
   float *vertargs;

   /*
    * Usually, we use the VCS (Vertical Coordinate System) info from the
    * v5d file.  However, if the user VCS variable is set we use the VCS
    * info from vis5d_init_vertical() function.
    */

   if (ctx->dpy_ctx->UserVerticalSystem>=0) {
      /* use user-provided parameters */
      vertargs = ctx->dpy_ctx->UserVertArgs;
      ctx->VerticalSystem = ctx->dpy_ctx->UserVerticalSystem;
      ctx->dpy_ctx->VerticalSystem = ctx->dpy_ctx->UserVerticalSystem;
   }
   else {
      /* use parameters from v5d file */
      vertargs = ctx->G.VertArgs;
      ctx->VerticalSystem = ctx->G.VerticalSystem;
      ctx->dpy_ctx->VerticalSystem = ctx->G.VerticalSystem;
   }

   switch (ctx->VerticalSystem) {
      case VERT_GENERIC:
         /* FALL-THROUGH */
      case VERT_EQUAL_KM:
         ctx->BottomBound = vertargs[0];
         ctx->LevInc      = vertargs[1];
         ctx->TopBound = ctx->BottomBound + ctx->LevInc * (ctx->MaxNl-1);
         for (i=0;i<ctx->MaxNl;i++) {
            ctx->Height[i] = ctx->BottomBound + i * ctx->LevInc;
         }

         ctx->dpy_ctx->BottomBound = vertargs[0];
         ctx->dpy_ctx->LevInc      = vertargs[1];
         ctx->dpy_ctx->TopBound = ctx->BottomBound + ctx->LevInc * (ctx->MaxNl-1);
         for (i=0;i<ctx->MaxNl;i++) {
            ctx->dpy_ctx->Height[i] = ctx->BottomBound + i * ctx->LevInc;
         }
         break;
      case VERT_NONEQUAL_MB:
         /* FALL-THROUGH */
      case VERT_NONEQUAL_KM:
         for (i=0;i<ctx->MaxNl;i++) {
            ctx->Height[i] = vertargs[i];
         }
         ctx->BottomBound = ctx->Height[0];
         ctx->TopBound = ctx->Height[ctx->MaxNl-1];
         for (i=0;i<ctx->MaxNl;i++) {
            ctx->dpy_ctx->Height[i] = vertargs[i];
         }

         ctx->dpy_ctx->BottomBound = ctx->Height[0];
         ctx->dpy_ctx->TopBound = ctx->Height[ctx->MaxNl-1];
         break;
      default:
         printf("Error in grid.c, unknown vertical coord system\n");
         return 0;
   }

   /* Precompute useful values */
   switch (ctx->VerticalSystem) {
      case VERT_GENERIC:
      case VERT_EQUAL_KM:
         ctx->TopBound = ctx->BottomBound + ctx->LevInc * (ctx->MaxNl-1);
         ctx->dpy_ctx->TopBound = ctx->BottomBound + ctx->LevInc * (ctx->MaxNl-1);
         for (i=0;i<ctx->MaxNl;i++) {
            ctx->Height[i] = ctx->BottomBound + i * ctx->LevInc;
            ctx->dpy_ctx->Height[i] = ctx->BottomBound + i * ctx->LevInc;
         }
         if (ctx->LogFlag) {
           ctx->Ptop = ctx->LogScale * exp( ctx->TopBound / ctx->LogExp );
           ctx->Pbot = ctx->LogScale * exp( ctx->BottomBound / ctx->LogExp );
           ctx->dpy_ctx->Ptop = ctx->LogScale * exp( ctx->TopBound / ctx->LogExp );
           ctx->dpy_ctx->Pbot = ctx->LogScale * exp( ctx->BottomBound / ctx->LogExp );
         }
         break;
      case VERT_NONEQUAL_KM:
         if (ctx->LogFlag) {
           ctx->Ptop = ctx->LogScale * exp( ctx->Height[ctx->MaxNl-1] / ctx->LogExp );
           ctx->Pbot = ctx->LogScale * exp( ctx->Height[0] / ctx->LogExp );
           ctx->dpy_ctx->Ptop = ctx->LogScale * exp( ctx->Height[ctx->MaxNl-1] / ctx->LogExp );
           ctx->dpy_ctx->Pbot = ctx->LogScale * exp( ctx->Height[0] / ctx->LogExp );
         }
         break;
      case VERT_NONEQUAL_MB:
         ctx->Ptop = height_to_pressure(ctx->Height[ctx->MaxNl-1]);
         ctx->Pbot = height_to_pressure(ctx->Height[0]);
         ctx->dpy_ctx->Ptop = height_to_pressure(ctx->Height[ctx->MaxNl-1]);
         ctx->dpy_ctx->Pbot = height_to_pressure(ctx->Height[0]);
         /* The Height[] array should be OK at this point */
         break;
      default:
         return 0;
   }
/* WLH 3 Nov 98
   if (ctx->TopBound < ctx->BottomBound) {
       printf("TopBound less than BottomBound\n");
       return 0;
   }
*/
   /* hack for spherical mjk 4-20-98 */
   if (ctx->Projection == PROJ_SPHERICAL &&
       ctx->TopBound == ctx->BottomBound){
      ctx->TopBound = ctx->BottomBound+.01;
      ctx->dpy_ctx->TopBound = ctx->dpy_ctx->BottomBound+10;
   }

   ctx->GridSameAsGridPRIME = vis5d_check_dtx_same_as_ctx(ctx->dpy_ctx->dpy_context_index,
                                                          ctx->context_index);
   return 1;
}


/*
 * Return the parameters of the current vertical coordinate system.
 */
void get_projection( Context ctx, int *projection, float *projargs )
{
   if (ctx->dpy_ctx->Projection<0 || ctx->dpy_ctx->UserProjection<0) {
      /* Haven't called setup_projection() yet, return v5d file's projection */
      *projection = ctx->G.Projection;
      memcpy( projargs, ctx->G.ProjArgs, MAXPROJARGS*sizeof(float) );
   }
   else {
      /* Return user projection args */
      *projection = ctx->dpy_ctx->UserProjection;
      memcpy( projargs, ctx->dpy_ctx->UserProjArgs, MAXPROJARGS*sizeof(float) );
   }
}

void get_projection_d( Display_Context dtx, int *projection, float *projargs )
{
   if (dtx->Projection<0 || dtx->UserProjection<0) {
      /* Haven't called setup_projection() yet, return v5d file's projection */
      vis5d_get_dtx_values(dtx->dpy_context_index, &dtx->G);
      *projection = dtx->G.Projection;
      memcpy( projargs, dtx->G.ProjArgs, MAXPROJARGS*sizeof(float) );
   }
   else {
      /* Return user projection args */
      *projection = dtx->UserProjection;
      memcpy( projargs, dtx->UserProjArgs, MAXPROJARGS*sizeof(float) );
   }
}



/*
 * Return the parameters of the current vertical coordinate system.
 */
void get_vertical_system_d( Display_Context dtx, int *vertical, float *vertargs )
{
   int numargs;

   /* determine number of arguments to return */
   if (dtx->Nl<2) {
      numargs = 2;
   }
   else {
      numargs = dtx->Nl;
   }

   *vertical = dtx->VerticalSystem;
   memcpy( vertargs, dtx->G.VertArgs, numargs * sizeof(float) );
}


/*
 * Return the parameters of the current vertical coordinate system.
 */
void get_vertical_system( Context ctx, int *vertical, float *vertargs )
{
   int numargs;

   /* determine number of arguments to return */
   if (ctx->MaxNl<2) {
      numargs = 2;
   }
   else {
      numargs = ctx->MaxNl;
   }

   if (ctx->dpy_ctx->VerticalSystem<0 || ctx->dpy_ctx->UserVerticalSystem<0) {
      /* Haven't called setup_vertical_system() yet, return the v5d files'
       * projection.
       */
      *vertical = ctx->G.VerticalSystem;
      memcpy( vertargs, ctx->G.VertArgs, numargs * sizeof(float) );
   }
   else {
      *vertical = ctx->dpy_ctx->UserVerticalSystem;
      memcpy( vertargs, ctx->dpy_ctx->UserVertArgs, numargs * sizeof(float) );
   }
}



/*
 * Perform a binary search of the array for the value and return
 * the index as a float.
 */
static float binary_search( float value, float array[], int size )
{
   int low, high, mid;
   float x;

   /* MJK 12.15.98 begin */
#  define SEARCH_FUZZ           1e-05
   if (value < (array[0] - SEARCH_FUZZ)){
      return -1.0;
   }
   else if(  size == 1) {
      return 0.0;
   }
   else if (value > (array[size-1] + SEARCH_FUZZ)) {
   /* return something larger than normal */
   /* so that is count's it as missing when needed */
      return (float)(size+1);
   }
   /* MJK 12.15.98 end */
/*

   if (value<array[0]){
      return -1.0;
   }
   else if(  size == 1) {
      return 0.0;
   }
   else if (value>array[size-1]) {
      return (float)(size+1);
   }
*/
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
           return (float) mid;  /* TODO: check this */
      }

      /* interpolate a value between high and low */
      x = (value-array[high]) / (array[low]-array[high]);
      return high * (1.0-x) + low * x;
   }
}




/**********************************************************************/
/*****                Vertical Coordinate Conversion              *****/
/**********************************************************************/


/*
 * Convert a level from grid coordinates to a zvalue in [Zmin,Zmax].
 * Input:  ctx - the vis5d context
 *         time, var - which timestep and variable.
 *         level - the level.
 */
float gridlevel_to_z( Context ctx, int time, int var, float level )
{
   int ilevel;
   float rlevel, p, hgt;

/*
   assert( var>=0 );
   assert( time>=0 );
*/

   if (level<=0.0) {
      return ctx->dpy_ctx->Zmin;
   }
   else if (level>=ctx->MaxNl-1 || ctx->MaxNl == 1) {
      return ctx->dpy_ctx->Zmax;
   }
   else {
      switch (ctx->VerticalSystem) {
         case VERT_GENERIC:
         case VERT_EQUAL_KM:
            if (ctx->LogFlag) {
              hgt = ctx->BottomBound + (ctx->TopBound - ctx->BottomBound) *
                    level / (float) (ctx->MaxNl-1);
              p = HGT_TO_P( hgt );
              return P_TO_Z( p );
            }
            else {
              return ctx->dpy_ctx->Zmin + (ctx->dpy_ctx->Zmax-ctx->dpy_ctx->Zmin) * level
                     / (float) (ctx->MaxNl-1);
            }
         case VERT_NONEQUAL_KM:
            ilevel = (int) level;
            rlevel = level - ilevel;
            hgt = ctx->Height[ilevel] * (1.0-rlevel) + ctx->Height[ilevel+1] * rlevel;
            if (ctx->LogFlag) {
              p = HGT_TO_P( hgt );
              return P_TO_Z( p );
            }
            else {
              return ctx->dpy_ctx->Zmin + (hgt-ctx->BottomBound) /
                (ctx->TopBound-ctx->BottomBound) * (ctx->dpy_ctx->Zmax-
                 ctx->dpy_ctx->Zmin);

            }
         case VERT_NONEQUAL_MB:
            ilevel = (int) level;
            rlevel = level - ilevel;
            hgt = ctx->Height[ilevel] * (1.0-rlevel) + ctx->Height[ilevel+1] * rlevel;
            p = height_to_pressure( hgt );
            return P_TO_Z( p );
         default:
            printf("Error in gridlevel_to_z\n");
      }
   }
   return 0.0;
}


/*
 * Convert a level from grid coordinates to a zvalue in [Zmin,Zmax].
 * Input:  dtx - the vis5d display context
 *         time, var - which timestep and variable.
 *         level - the level.
 */
float gridlevelPRIME_to_zPRIME( Display_Context dtx, int time, int var, float level )
{
   int ilevel;
   float rlevel, p, hgt;

/*
   assert( var>=0 );
   assert( time>=0 );
*/

   if (level<=0.0) {
      return dtx->Zmin;
   }
   else if (level>=dtx->MaxNl-1 || dtx->MaxNl == 1) {
      return dtx->Zmax;
   }
   else {
      switch (dtx->VerticalSystem) {
         case VERT_GENERIC:
         case VERT_EQUAL_KM:
            if (dtx->LogFlag) {
              hgt = dtx->BottomBound + (dtx->TopBound - dtx->BottomBound) *
                    level / (float) (dtx->MaxNl-1);
              p = HGT_TO_PPRIME( hgt );
              return P_TO_ZPRIME( p );
            }
            else {
              return dtx->Zmin + (dtx->Zmax-dtx->Zmin) * level
                     / (float) (dtx->MaxNl-1);
            }
         case VERT_NONEQUAL_KM:
            ilevel = (int) level;
            rlevel = level - ilevel;
            hgt = dtx->Height[ilevel] * (1.0-rlevel) + dtx->Height[ilevel+1] * rlevel;
            if (dtx->LogFlag) {
              p = HGT_TO_PPRIME( hgt );
              return P_TO_ZPRIME( p );
            }
            else {
               float val;
               val = dtx->Zmin + (hgt-dtx->BottomBound)/(dtx->TopBound-dtx->BottomBound)*
                     (dtx->Zmax-dtx->Zmin);
               return val;
            }
         case VERT_NONEQUAL_MB:
            ilevel = (int) level;
            rlevel = level - ilevel;
            hgt = dtx->Height[ilevel] * (1.0-rlevel) + dtx->Height[ilevel+1] * rlevel;
            p = height_to_pressure( hgt );
            return P_TO_ZPRIME( p );
         default:
            printf("Error in gridlevelPRIME_to_zPRIME\n");
      }
   }
   return 0.0;
}


static float zPRIME_to_gridlevPRIME( Display_Context dtx, float z )
{
   float p, hgt;

   if (z>=dtx->Zmax) {
      return (float) (dtx->MaxNl-1);
   }
   else if (z<=dtx->Zmin) {
      return 0.0;
   }
   else {
      switch (dtx->VerticalSystem) {
         case VERT_GENERIC:
         case VERT_EQUAL_KM:
            if (dtx->LogFlag) {
              p = ZPRIME_TO_PPRIME( z );
              hgt = P_TO_HGTPRIME( p );
              return (float) (dtx->MaxNl-1) * (dtx->BottomBound + z) /
                     (dtx->TopBound-dtx->BottomBound);
            }
            else {
              return (float) (dtx->MaxNl-1) * (z-dtx->Zmin)
                     / (dtx->Zmax-dtx->Zmin);
            }
         case VERT_NONEQUAL_KM:
            if (dtx->LogFlag) {
              p = ZPRIME_TO_PPRIME( z );
              hgt = P_TO_HGTPRIME( p );
            }
            else {
              hgt = dtx->BottomBound + (dtx->TopBound-dtx->BottomBound) *
                    (z-dtx->Zmin)/(dtx->Zmax-dtx->Zmin);
            }
            /* do a binary search of dtx->Height[] for hgt */
            return binary_search( hgt, dtx->Height, dtx->MaxNl );
         case VERT_NONEQUAL_MB:
            p = ZPRIME_TO_PPRIME( z );
            hgt = pressure_to_height(p);
            return binary_search( hgt, dtx->Height, dtx->MaxNl );
         default:
            printf("Error in zPRIME_to_gridlevPRIME\n");
      }
   }
   return 0.0;
}



/*
 * Convert a z graphics coordinate from [ctx->Zmax,ctx->Zmin] to a grid level in
 * [0,ctx->Nl-1].
 */
static float z_to_gridlev( Context ctx, float z )
{
   float p, hgt;

   if (z>=ctx->dpy_ctx->Zmax) {
      return (float) (ctx->MaxNl-1);
   }
   else if (z<=ctx->dpy_ctx->Zmin) {
      return 0.0;
   }
   else {
      switch (ctx->VerticalSystem) {
         case VERT_GENERIC:
         case VERT_EQUAL_KM:
            if (ctx->LogFlag) {
              p = Z_TO_P( z );
              hgt = P_TO_HGT( p );
              return (float) (ctx->MaxNl-1) * (ctx->BottomBound + z) /
                     (ctx->TopBound-ctx->BottomBound);
            }
            else {
              return (float) (ctx->MaxNl-1) * (z-ctx->dpy_ctx->Zmin)
                     / (ctx->dpy_ctx->Zmax-ctx->dpy_ctx->Zmin);
            }
         case VERT_NONEQUAL_KM:
            if (ctx->LogFlag) {
              p = Z_TO_P( z );
              hgt = P_TO_HGT( p );
            }
            else {
              hgt = ctx->BottomBound + (ctx->TopBound-ctx->BottomBound) *
                    (z-ctx->dpy_ctx->Zmin)/(ctx->dpy_ctx->Zmax-ctx->dpy_ctx->Zmin);
            }
            /* do a binary search of ctx->Height[] for hgt */
            return binary_search( hgt, ctx->Height, ctx->MaxNl );
         case VERT_NONEQUAL_MB:
            p = Z_TO_P( z );
            hgt = pressure_to_height(p);
            return binary_search( hgt, ctx->Height, ctx->MaxNl );
         default:
            printf("Error in z_to_gridlev\n");
      }
   }
   return 0.0;
}


/*
 * Convert a grid level to a geographic height.
 * Input:  ctx - the vis5d context
 *         level - grid level
 * Return:  height
 *
 **** This same code is used in sounding.c lines 1834 -1864 
 *
*/
float gridlevel_to_height( Context ctx, float level )
{
   int ilevel;
   float rlevel;

   if ( ctx->MaxNl == 1) {
      return ctx->TopBound;
   }
   else {
      switch (ctx->VerticalSystem) {
         case VERT_GENERIC:
         case VERT_EQUAL_KM:
            return ctx->BottomBound + level * ctx->LevInc;
         case VERT_NONEQUAL_MB:
         case VERT_NONEQUAL_KM:
            ilevel = (int) level;
            rlevel = level - ilevel;
            return ctx->Height[ilevel] * (1.0-rlevel) + ctx->Height[ilevel+1] * rlevel;
         default:
            printf("Error in gridlevel_to_height\n");
      }
   }
   return 0.0;
}

float gridlevelPRIME_to_height( Display_Context dtx, float level ) 
{   
   int ilevel; 
   float rlevel; 
    
   if ( dtx->MaxNl == 1) { 
      return dtx->TopBound;
   }
   else {  
      switch (dtx->VerticalSystem) {
         case VERT_GENERIC:
         case VERT_EQUAL_KM:
            return dtx->BottomBound + level * dtx->LevInc;
         case VERT_NONEQUAL_MB:
         case VERT_NONEQUAL_KM:
            ilevel = (int) level;
            rlevel = level - ilevel; 
            return dtx->Height[ilevel] * (1.0-rlevel) + dtx->Height[ilevel+1] * rlevel; 
         default:
            printf("Error in gridlevel_to_height\n");
      }       
   }
   return 0.0;
}   



/*
 * Convert a height from [ctx->BottomBound,ctx->TopBound] to a grid level
 * in [0,ctx->MaxNl-1].
 */
float height_to_gridlev( Context ctx, float hgt )
{
   /* (hgt<=ctx->BottomBound) {
      return 0.0;
   }
   else if (hgt>=ctx->TopBound) {
      return (float) (ctx->MaxNl-1);
   } 
   else {*/
      switch (ctx->VerticalSystem) {
         case VERT_GENERIC:
         case VERT_EQUAL_KM:
            return (hgt-ctx->BottomBound) / ctx->LevInc;
         case VERT_NONEQUAL_MB:
         case VERT_NONEQUAL_KM:
            /* do a binary search of ctx->Height[] for hgt */
            return binary_search( hgt, ctx->Height, ctx->MaxNl );
         default:
            printf("Error in height_to_gridlev\n");
      }
   /*}*/
   return 0.0;
}

float height_to_gridlevPRIME( Display_Context dtx, float hgt )
{
   /* 
   if (hgt<=dtx->BottomBound) {
      return 0.0;
   }
   else if (hgt>=dtx->TopBound) {
      return (float) (dtx->MaxNl-1);
   }
   else { */
      switch (dtx->VerticalSystem) {
         case VERT_GENERIC:
         case VERT_EQUAL_KM:
            return (hgt-dtx->BottomBound) / dtx->LevInc;
         case VERT_NONEQUAL_MB:
         case VERT_NONEQUAL_KM:
            /* do a binary search of dtx->Height[] for hgt */
            return binary_search( hgt, dtx->Height, dtx->MaxNl );
         default:
            printf("Error in height_to_gridlev\n");
      }
   /*}*/
   return 0.0;
}



/*
 * Convert a height in [ctx->BottomBound,ctx->TopBound] to a z coordinate in [ctx->Zmax,ctx->Zmin].
 */
float height_to_z( Context ctx, float hgt )
{
   float p;

   if (hgt>=ctx->TopBound) {
      return ctx->dpy_ctx->Zmax;
   }
   else if (hgt<=ctx->BottomBound) {
      return ctx->dpy_ctx->Zmin;
   }
   else {
      switch (ctx->VerticalSystem) {
         case VERT_GENERIC:
         case VERT_EQUAL_KM:
         case VERT_NONEQUAL_KM:
            if (ctx->LogFlag) {
              p = HGT_TO_P( hgt );
              return P_TO_Z( p );
            }
            else {
              return ctx->dpy_ctx->Zmin + (hgt-ctx->BottomBound)
                     / (ctx->TopBound-ctx->BottomBound)
                     * (ctx->dpy_ctx->Zmax-ctx->dpy_ctx->Zmin);
            }
         case VERT_NONEQUAL_MB:
            p = height_to_pressure( hgt );
            return P_TO_Z( p );
         default:
            printf("Error in height_to_z\n");
      }
   }
   return 0.0;
}

float height_to_zPRIME( Display_Context dtx, float hgt )
{   
   float p;
    
   /* I think this is a good idea, other
      wise all the iso surfaces get smushed and sparkly
      at the top and bottom
   if (hgt>=dtx->TopBound) {
      return dtx->Zmax;
   }
   else if (hgt<=dtx->BottomBound) {
      return dtx->Zmin;
   }
   else { */ 
      switch (dtx->VerticalSystem) {
         case VERT_GENERIC:
         case VERT_EQUAL_KM:
         case VERT_NONEQUAL_KM:
            if (dtx->LogFlag) {
              p = HGT_TO_PPRIME( hgt );
              return P_TO_ZPRIME( p );
            }         
            else {  
              if (dtx->TopBound-dtx->BottomBound != 0.0){
                return dtx->Zmin + (hgt-dtx->BottomBound)
                        / (dtx->TopBound-dtx->BottomBound)
                       * (dtx->Zmax-dtx->Zmin);
              }
              else{
                return 0.0;
              }
            }                
         case VERT_NONEQUAL_MB:
            p = height_to_pressure( hgt );
            return P_TO_ZPRIME( p );
         default:   
            printf("Error in height_to_zPRIME\n");
      }             
   /*}*/
   return 0.0;
}   

/* MJK 2.17.98 begin */
float height_to_zTOPO(Display_Context dtx, float hgt )
{
   float p;

   if (hgt>=dtx->TopBound) {
      return dtx->Zmax;
   }
   else if (hgt<=dtx->BottomBound) {
      return dtx->Zmin;
   }
   else { 
      switch (dtx->VerticalSystem) {
         case VERT_GENERIC:
         case VERT_EQUAL_KM:
         case VERT_NONEQUAL_KM:
            if (dtx->LogFlag) {
              p = HGT_TO_PPRIME( hgt );
              return P_TO_ZPRIME( p );
            }
            else {
              if (dtx->TopBound-dtx->BottomBound != 0.0){
                return dtx->Zmin + (hgt-dtx->BottomBound)
                        / (dtx->TopBound-dtx->BottomBound)
                       * (dtx->Zmax-dtx->Zmin);
              }
              else{
                return 0.0;
              }
            }
         case VERT_NONEQUAL_MB:
            p = height_to_pressure( hgt );
            return P_TO_ZPRIME( p );
         default:
            printf("Error in height_to_zPRIME\n");
      }
   }
   return 0.0;
}
/* MJK 2.17.98 end */


/*
 * Convert a z value to a height coordinate.
 */
static float z_to_height( Context ctx, float z )
{
   float p;

   switch (ctx->VerticalSystem) {
      case VERT_GENERIC:
      case VERT_EQUAL_KM:
      case VERT_NONEQUAL_KM:
         if (ctx->LogFlag) {
           p = Z_TO_P( z );
           return P_TO_HGT( p );
         }
         else {
           return ctx->BottomBound + (z-ctx->dpy_ctx->Zmin) *
                  (ctx->TopBound-ctx->BottomBound) / (ctx->dpy_ctx->Zmax-
                   ctx->dpy_ctx->Zmin);
         }
      case VERT_NONEQUAL_MB:
         p = Z_TO_P( z );
         return pressure_to_height( p );
      default:
         printf("Error in z_to_height\n");
   }
   return 0.0;
}

/*
 * Convert a z value to a height coordinate.
 */
static float zPRIME_to_heightPRIME( Display_Context dtx, float z )
{
   float p;

   switch (dtx->VerticalSystem) {
      case VERT_GENERIC:
      case VERT_EQUAL_KM:
      case VERT_NONEQUAL_KM:
         if (dtx->LogFlag) {
           p = ZPRIME_TO_PPRIME( z );
           return P_TO_HGT_d( p );
         }
         else {
           return dtx->BottomBound + (z-dtx->Zmin) *
                  (dtx->TopBound-dtx->BottomBound) / (dtx->Zmax-dtx->Zmin);
         }
      case VERT_NONEQUAL_MB:
         p = ZPRIME_TO_PPRIME( z );
         return pressure_to_height( p );
      default:
         printf("Error in z_to_height\n");
   }
   return 0.0;
}

float gridlevelPRIME_to_gridlevel( Context ctx, float levelPRIME )
{
   Display_Context dtx;
   float height, level;

   dtx = ctx->dpy_ctx;
   height = gridlevelPRIME_to_height(dtx, levelPRIME);
   level = height_to_gridlev( ctx, height);
   return level;
}

float gridlevel_to_gridlevelPRIME( Context ctx, float level )
{
   Display_Context dtx;
   float height, levelPRIME;

   dtx = ctx->dpy_ctx;
   height = gridlevel_to_height( ctx, level );
   if (height > dtx->TopBound){
      height = dtx->TopBound;
   }
   else if ( height < dtx->BottomBound){
      height = dtx->BottomBound;
   }
   levelPRIME = height_to_gridlevPRIME( dtx, height);
   return levelPRIME;
}
  



/**********************************************************************/
/*****       (x,y,z), (row,col,lev), (lat,lon,hgt) conversion     *****/
/**********************************************************************/


void grid_to_xyzPRIME(Context ctx, int time, int var, int n,
                  float r[], float c[], float l[],
                  float x[], float y[], float z[] )
{
   float rPRIME[MAX_CONV_VERTS], cPRIME[MAX_CONV_VERTS], lPRIME[MAX_CONV_VERTS];
   grid_to_gridPRIME( ctx, time, var, n, r, c, l,
                      rPRIME, cPRIME, lPRIME);
   gridPRIME_to_xyzPRIME(ctx->dpy_ctx, time, var, n, rPRIME,
                         cPRIME, lPRIME, x, y, z);
}


/*
 * Transform an array of (r,c,l) display grid coordinates to (x,y,z) display graphics
 * coordinates.  (r,c,l) should be in [0,Nr-1][0,Nc-1][0,Nl-1] and
 * (x,y,z) will be returned in [Xmin,Xmax][Ymin,Ymax][Zmax,Zmin].
 *
 * Input:  dtx - the display context
 *         time - which timestep
 *         var - which variable
 *         n - number of coordinates to transform
 *         r, c, l - array of grid coords.
 * Output:  x, y, z - array of graphics coords.
 */
void gridPRIME_to_xyzPRIME( Display_Context dtx, int time, int var, int n,
                  float r[], float c[], float l[],
                  float x[], float y[], float z[] )
{
   int i;

   switch (dtx->Projection) {
      case PROJ_GENERIC:
      case PROJ_LINEAR:
      case PROJ_LAMBERT:
      case PROJ_STEREO:
      case PROJ_ROTATED:
      case PROJ_MERCATOR:
         switch (dtx->VerticalSystem) {
            case VERT_GENERIC:
            case VERT_EQUAL_KM:
               /* simplest, fast case */
               {
                  float xs, ys, zs;
                  xs = (dtx->Xmax-dtx->Xmin) / (float) (dtx->Nc-1);
                  ys = (dtx->Ymax-dtx->Ymin) / (float) (dtx->Nr-1);
                  if (dtx->MaxNl > 1) zs = (dtx->Zmax-dtx->Zmin) / (float) (dtx->MaxNl-1);
                  else zs = 0.0;
                  for (i=0;i<n;i++) {
                     x[i] = dtx->Xmin + c[i] * xs;
                     y[i] = dtx->Ymax - r[i] * ys;
                     z[i] = dtx->Zmin + l[i] * zs;
                  }
               }
               break;
            case VERT_NONEQUAL_MB:
            case VERT_NONEQUAL_KM:
               {
                  float xs, ys;
                  xs = (dtx->Xmax-dtx->Xmin) / (float) (dtx->Nc-1);
                  ys = (dtx->Ymax-dtx->Ymin) / (float) (dtx->Nr-1);
                  for (i=0;i<n;i++) {
                     x[i] = dtx->Xmin + c[i] * xs;
                     y[i] = dtx->Ymax - r[i] * ys;
                     z[i] = gridlevelPRIME_to_zPRIME( dtx, time, var, l[i] );
                  }
               }
               break;
         }
         break;
      case PROJ_CYLINDRICAL:
         {
            for (i=0;i<n;i++) {
               float lat, lon, radius;
               lat = dtx->NorthBound - r[i]*(dtx->NorthBound-dtx->SouthBound)/(float) (dtx->Nr-1);
               radius = (REVERSE_POLES*90.0 - lat) * dtx->CylinderScale;
               lon = dtx->WestBound - c[i] * (dtx->WestBound-dtx->EastBound) / (float) (dtx->Nc-1);
               lon = REVERSE_POLES*lon * DEG2RAD;
               x[i] = REVERSE_POLES*radius * cos(lon);
               y[i] = REVERSE_POLES* -radius * sin(lon);
               z[i] = gridlevelPRIME_to_zPRIME( dtx, time, var, l[i] );
            }
         }
         break;
      case PROJ_SPHERICAL:
         for (i=0;i<n;i++) {
            float lat, lon, hgt;
            float clat, slat, clon, slon, d;

            /* convert (r,c,l) to (lat,lon,hgt) */
            lat = dtx->NorthBound - r[i] * (dtx->NorthBound-dtx->SouthBound) / (float) (dtx->Nr-1);
            lon = dtx->WestBound - c[i] * (dtx->WestBound-dtx->EastBound) / (float) (dtx->Nc-1);
            hgt = gridlevelPRIME_to_height( dtx, l[i] );

            /* convert (lat,lon,hgt) to (x,y,z) */
            clat = cos(lat * DEG2RAD);
            clon = cos(lon * DEG2RAD);
            slat = sin(lat * DEG2RAD);
            slon = sin(lon * DEG2RAD);
            d = (hgt-dtx->BottomBound) / (dtx->TopBound-dtx->BottomBound) * SPHERE_SCALE
                + SPHERE_SIZE;
            x[i] = d * clat * clon;
            y[i] = -d * clat * slon;
            z[i] = d * slat;
         }
         break;
      default:
         printf("Error in gridPRIME_to_xyzPRIME\n");
   }

}

/*
 * Transform an array of (r,c,l) grid coordinates to (x,y,z) graphics
 * coordinates.  (r,c,l) should be in [0,Nr-1][0,Nc-1][0,Nl-1] and
 * (x,y,z) will be returned in [Xmin,Xmax][Ymin,Ymax][Zmax,Zmin].
 *
 * Input:  ctx - the vis5d context
 *         time - which timestep
 *         var - which variable
 *         n - number of coordinates to transform
 *         r, c, l - array of grid coords.
 * Output:  x, y, z - array of graphics coords.
 */
void grid_to_xyz( Context ctx, int time, int var, int n,
                  float r[], float c[], float l[],
                  float x[], float y[], float z[] )
{
   int i;

   switch (ctx->Projection) {
      case PROJ_GENERIC:
      case PROJ_LINEAR:
      case PROJ_LAMBERT:
      case PROJ_STEREO:
      case PROJ_ROTATED:
      case PROJ_MERCATOR:
         switch (ctx->VerticalSystem) {
            case VERT_GENERIC:
            case VERT_EQUAL_KM:
               /* simplest, fast case */
               {
                  float xs, ys, zs;
                  xs = (ctx->dpy_ctx->Xmax-ctx->dpy_ctx->Xmin) / (float) (ctx->Nc-1);
                  ys = (ctx->dpy_ctx->Ymax-ctx->dpy_ctx->Ymin) / (float) (ctx->Nr-1);
                  if (ctx->MaxNl > 1) zs = (ctx->dpy_ctx->Zmax-ctx->dpy_ctx->Zmin) /
                     (float) (ctx->MaxNl-1);
                  else zs = 0.0;
                  for (i=0;i<n;i++) {
                     x[i] = ctx->dpy_ctx->Xmin + c[i] * xs;
                     y[i] = ctx->dpy_ctx->Ymax - r[i] * ys;
                     z[i] = ctx->dpy_ctx->Zmin + l[i] * zs;
                  }
               }
               break;
            case VERT_NONEQUAL_MB:
            case VERT_NONEQUAL_KM:
               {
                  float xs, ys;
                  xs = (ctx->dpy_ctx->Xmax-ctx->dpy_ctx->Xmin) / (float) (ctx->Nc-1);
                  ys = (ctx->dpy_ctx->Ymax-ctx->dpy_ctx->Ymin) / (float) (ctx->Nr-1);
                  for (i=0;i<n;i++) {
                     x[i] = ctx->dpy_ctx->Xmin + c[i] * xs;
                     y[i] = ctx->dpy_ctx->Ymax - r[i] * ys;
                     z[i] = gridlevel_to_z( ctx, time, var, l[i] );
                  }
               }
               break;
         }
         break;
      case PROJ_CYLINDRICAL:
         {
            for (i=0;i<n;i++) {
               float lat, lon, radius;
               lat = ctx->NorthBound - r[i]*(ctx->NorthBound-ctx->SouthBound)/(float) (ctx->Nr-1);
               radius = (REVERSE_POLES*90.0 - lat) * ctx->CylinderScale;
               lon = ctx->WestBound - c[i] * (ctx->WestBound-ctx->EastBound) / (float) (ctx->Nc-1);
               lon = REVERSE_POLES*lon * DEG2RAD;
               x[i] = REVERSE_POLES*radius * cos(lon);
               y[i] = REVERSE_POLES*-radius * sin(lon);
               z[i] = gridlevel_to_z( ctx, time, var, l[i] );
            }
         }
         break;
      case PROJ_SPHERICAL:
         for (i=0;i<n;i++) {
            float lat, lon, hgt;
            float clat, slat, clon, slon, d;

            /* convert (r,c,l) to (lat,lon,hgt) */
            lat = ctx->NorthBound - r[i] * (ctx->NorthBound-ctx->SouthBound) / (float) (ctx->Nr-1);
            lon = ctx->WestBound - c[i] * (ctx->WestBound-ctx->EastBound) / (float) (ctx->Nc-1);
            hgt = gridlevel_to_height( ctx, l[i] );

            /* convert (lat,lon,hgt) to (x,y,z) */
            clat = cos(lat * DEG2RAD);
            clon = cos(lon * DEG2RAD);
            slat = sin(lat * DEG2RAD);
            slon = sin(lon * DEG2RAD);
            d = (hgt-ctx->BottomBound) / (ctx->TopBound-ctx->BottomBound) * SPHERE_SCALE
                + SPHERE_SIZE;
            x[i] = d * clat * clon;
            y[i] = -d * clat * slon;
            z[i] = d * slat;
         }
         break;
      default:
         printf("Error in grid_to_xyz\n");
   }

}



/*
 * Transform an array of (r,c,l) grid coordinates to compressed (x,y,z)
 * graphics coordinates.  (r,c,l) should be in [0,Nr-1][0,Nc-1][0,Nl-1]
 * and (x,y,z) will be returned in [+/-VERTEX_SCALE]^3.
 * NOTE: This function must be FAST because it's used whenever an
 * isosurface, slice, or trajectory is made to transform all the coords!!!
 *
 * Input:  ctx - the vis5d context
 *         time - which timestep
 *         var - which variable
 *         n - number of coordinates
 *         r, c, l - array of grid coords.
 * Output:  xyz - array of compressed graphics coords.
 */
void grid_to_compXYZ( Context ctx, int time, int var, int n,
                      float r[], float c[], float l[],
                      int_2 xyz[][3] )
{
   int i;
   float xx, yy, zz;

   switch (ctx->Projection) {
      case PROJ_GENERIC:
      case PROJ_LINEAR:
      case PROJ_LAMBERT:
      case PROJ_STEREO:
      case PROJ_ROTATED:
      case PROJ_MERCATOR:
         switch (ctx->VerticalSystem) {
            case VERT_GENERIC:
            case VERT_EQUAL_KM:
               /* simplest, fast case */
               {
                  float xs, ys, zs, xt, yt, zt;
                  xs = (ctx->dpy_ctx->Xmax-ctx->dpy_ctx->Xmin) /
                       (float) (ctx->Nc-1) * VERTEX_SCALE;
                  ys = (ctx->dpy_ctx->Ymax-ctx->dpy_ctx->Ymin) /
                       (float) (ctx->Nr-1) * VERTEX_SCALE;
                  if (ctx->MaxNl > 1) {
                     zs = (ctx->dpy_ctx->Zmax-ctx->dpy_ctx->Zmin) /
                          (ctx->MaxNl-1) * VERTEX_SCALE;
                  }
                  else {
                     zs = 0.0;
                  }
                  xt = ctx->dpy_ctx->Xmin * VERTEX_SCALE;
                  yt = ctx->dpy_ctx->Ymax * VERTEX_SCALE;
                  zt = ctx->dpy_ctx->Zmin * VERTEX_SCALE;
                  for (i=0;i<n;i++) {
                     xx = (xt + c[i] * xs);
                     yy = (yt - r[i] * ys);
                     zz = (zt + l[i] * zs);
                     if (xx > 32760.0) xx = 32760.0;
                     if (xx < -32760.0) xx = -32760.0;
                     if (yy > 32760.0) yy = 32760.0;
                     if (yy < -32760.0) yy = -32760.0;
                     if (zz > 32760.0) zz = 32760.0;
                     if (zz < -32760.0) zz = -32760.0;
                     xyz[i][0] = xx;
                     xyz[i][1] = yy;
                     xyz[i][2] = zz;
                  }
               }
               break;
            case VERT_NONEQUAL_MB:
            case VERT_NONEQUAL_KM:
               {
                  float xs, ys, zs, xt, yt;
                  xs = (ctx->dpy_ctx->Xmax-ctx->dpy_ctx->Xmin) /
                       (float) (ctx->Nc-1) * VERTEX_SCALE;
                  ys = (ctx->dpy_ctx->Ymax-ctx->dpy_ctx->Ymin) /
                       (float) (ctx->Nr-1) * VERTEX_SCALE;
                  zs = VERTEX_SCALE;
                  xt = ctx->dpy_ctx->Xmin * VERTEX_SCALE;
                  yt = ctx->dpy_ctx->Ymax * VERTEX_SCALE;
                  for (i=0;i<n;i++) {
                     xx = (xt + c[i] * xs);
                     yy = (yt - r[i] * ys);
                     zz = (gridlevel_to_z( ctx, time, var, l[i] ) * zs);
                     if (xx > 32760.0) xx = 32760.0;
                     if (xx < -32760.0) xx = -32760.0;
                     if (yy > 32760.0) yy = 32760.0;
                     if (yy < -32760.0) yy = -32760.0;
                     if (zz > 32760.0) zz = 32760.0;
                     if (zz < -32760.0) zz = -32760.0;
                     xyz[i][0] = xx;
                     xyz[i][1] = yy;
                     xyz[i][2] = zz;
                  }
               }
               break;
         }
         break;
      case PROJ_CYLINDRICAL:
         {
            for (i=0;i<n;i++) {
               float lat, lon, radius;
               float cylx, cyly, cylz;
               lat = ctx->NorthBound - r[i]
                       *(ctx->NorthBound-ctx->SouthBound)/(float) (ctx->Nr-1);
               radius = (REVERSE_POLES*90.0 - lat) * ctx->CylinderScale;
               lon = ctx->WestBound - c[i]
                       * (ctx->WestBound-ctx->EastBound) / (float) (ctx->Nc-1);
               lon = REVERSE_POLES* lon * DEG2RAD;
               cylx = REVERSE_POLES*radius * cos(lon);
               cyly = REVERSE_POLES*-radius * sin(lon);
               cylz = gridlevel_to_z( ctx, time, var, l[i] );
               xx = cylx * VERTEX_SCALE;
               yy = cyly * VERTEX_SCALE;
               zz = cylz * VERTEX_SCALE;
               if (xx > 32760.0) xx = 32760.0;
               if (xx < -32760.0) xx = -32760.0;
               if (yy > 32760.0) yy = 32760.0;
               if (yy < -32760.0) yy = -32760.0;
               if (zz > 32760.0) zz = 32760.0;
               if (zz < -32760.0) zz = -32760.0;
               xyz[i][0] = xx;
               xyz[i][1] = yy;
               xyz[i][2] = zz;
            }
         }
         break;
      case PROJ_SPHERICAL:
         for (i=0;i<n;i++) {
            float lat, lon, hgt;
            float clat, clon, slat, slon, d;

            /* convert (r,c,l) to (lat,lon,hgt) */
            lat = ctx->NorthBound - r[i]
                     * (ctx->NorthBound-ctx->SouthBound) / (float) (ctx->Nr-1);
            lon = ctx->WestBound - c[i]
                     * (ctx->WestBound-ctx->EastBound) / (float) (ctx->Nc-1);
            hgt = gridlevel_to_height( ctx, l[i] );

            /* convert (lat,lon,hgt) to (x,y,z) */
            clat = cos(lat * DEG2RAD);
            clon = cos(lon * DEG2RAD);
            slat = sin(lat * DEG2RAD);
            slon = sin(lon * DEG2RAD);
            d = (hgt-ctx->BottomBound)
               / (ctx->TopBound-ctx->BottomBound) * SPHERE_SCALE + SPHERE_SIZE;
            d *= VERTEX_SCALE;
            xx = d * clat * clon;
            yy = -d * clat * slon;
            zz = d * slat;
            if (xx > 32760.0) xx = 32760.0;
            if (xx < -32760.0) xx = -32760.0;
            if (yy > 32760.0) yy = 32760.0;
            if (yy < -32760.0) yy = -32760.0;
            if (zz > 32760.0) zz = 32760.0;
            if (zz < -32760.0) zz = -32760.0;
            xyz[i][0] = xx;
            xyz[i][1] = yy;
            xyz[i][2] = zz;
         }
         break;
      default:
         printf("Error in grid_to_compXYZ\n");
   }
}
void xyz_to_compXYZ( Display_Context dtx, int n, float x[], float y[],
                     float z[], int_2 xyz[][3] )
{
   int i;
   float xt, yt, zt;
   float xx, yy, zz;

   for (i = 0; i < n; i++){
      xx =  x[i]*VERTEX_SCALE;
      yy =  y[i]*VERTEX_SCALE;
      zz =  z[i]*VERTEX_SCALE;
      if (xx > 32760.0) xx = 32760.0;
      if (xx < -32760.0) xx = -32760.0;
      if (yy > 32760.0) yy = 32760.0;
      if (yy < -32760.0) yy = -32760.0;
      if (zz > 32760.0) zz = 32760.0;
      if (zz < -32760.0) zz = -32760.0;
      xyz[i][0] = xx;
      xyz[i][1] = yy;
      xyz[i][2] = zz;
   }
}
   
      
void gridPRIME_to_compXYZPRIMEcheck(Display_Context dtx, int time, int var, int *N,
                      float r[], float c[], float l[],
                      int_2 xyz[][3] )
{
   int i;
   int v;
   int n;
   float xx, yy, zz;

   n = *N;
   v = 0;
   switch (dtx->Projection) {
      case PROJ_GENERIC:
      case PROJ_LINEAR:
      case PROJ_LAMBERT:
      case PROJ_STEREO:
      case PROJ_ROTATED:
      case PROJ_MERCATOR:
         switch (dtx->VerticalSystem) {
            case VERT_GENERIC:
            case VERT_EQUAL_KM:
               /* simplest, fast case */
               {
                  float xs, ys, zs, xt, yt, zt;
                  xs = (dtx->Xmax-dtx->Xmin) / (float) (dtx->Nc-1) * VERTEX_SCALE;
                  ys = (dtx->Ymax-dtx->Ymin) / (float) (dtx->Nr-1) * VERTEX_SCALE;
                  if (dtx->MaxNl > 1) {
                     zs = (dtx->Zmax-dtx->Zmin) / (dtx->MaxNl-1) * VERTEX_SCALE;
                  }
                  else {
                     zs = 0.0;
                  }
                  xt = dtx->Xmin * VERTEX_SCALE;
                  yt = dtx->Ymax * VERTEX_SCALE;
                  zt = dtx->Zmin * VERTEX_SCALE;
                  for (i=0;i<n;i++) {
                     if(c[i] < 0 || c[i] > dtx->Nc-1 ||
                        r[i] < 0 || r[i] > dtx->Nr-1 ||
                        l[i] < 0 || l[i] > dtx->Nl-1){
                        v++;
                     }
                     xx = xt + c[i] * xs;
                     yy = yt - r[i] * ys;
                     zz = zt + l[i] * zs;
                     if (xx > 32760.0) xx = 32760.0;
                     if (xx < -32760.0) xx = -32760.0;
                     if (yy > 32760.0) yy = 32760.0;
                     if (yy < -32760.0) yy = -32760.0;
                     if (zz > 32760.0) zz = 32760.0;
                     if (zz < -32760.0) zz = -32760.0;
                     xyz[i-v][0] = xx;
                     xyz[i-v][1] = yy;
                     xyz[i-v][2] = zz; 
                  }
                  *N = n-v;
               }
               break;
            case VERT_NONEQUAL_MB:
            case VERT_NONEQUAL_KM:
               {
                  float xs, ys, zs, xt, yt;
                  xs = (dtx->Xmax-dtx->Xmin) / (float) (dtx->Nc-1) * VERTEX_SCALE;
                  ys = (dtx->Ymax-dtx->Ymin) / (float) (dtx->Nr-1) * VERTEX_SCALE;
                  zs = VERTEX_SCALE;
                  xt = dtx->Xmin * VERTEX_SCALE;
                  yt = dtx->Ymax * VERTEX_SCALE;
                  for (i=0;i<n;i++) {
                     if(c[i] < 0 || c[i] > dtx->Nc-1 ||
                        r[i] < 0 || r[i] > dtx->Nr-1 ||
                        l[i] < 0 || l[i] > dtx->Nl-1){
                        v++;
                     }
                     xx = xt + c[i] * xs;
                     yy = yt - r[i] * ys;
                     zz = gridlevelPRIME_to_zPRIME( dtx, time, var, l[i] ) * zs;
                     if (xx > 32760.0) xx = 32760.0;                     
                     if (xx < -32760.0) xx = -32760.0;                     
                     if (yy > 32760.0) yy = 32760.0;                     
                     if (yy < -32760.0) yy = -32760.0;                     
                     if (zz > 32760.0) zz = 32760.0;                     
                     if (zz < -32760.0) zz = -32760.0;                     
                     xyz[i-v][0] = xx; 
                     xyz[i-v][1] = yy;
                     xyz[i-v][2] = zz;
                  }
                  *N = n-v;
               }
               break;
         }
         break;
      case PROJ_CYLINDRICAL:
         {
            for (i=0;i<n;i++) {
               float lat, lon, radius;
               float cylx, cyly, cylz;
               lat = dtx->NorthBound - r[i]
                       *(dtx->NorthBound-dtx->SouthBound)/(float) (dtx->Nr-1);
               radius = (REVERSE_POLES*90.0 - lat) * dtx->CylinderScale;
               lon = dtx->WestBound - c[i]
                       * (dtx->WestBound-dtx->EastBound) / (float) (dtx->Nc-1);
               lon = REVERSE_POLES*lon * DEG2RAD;
               cylx = REVERSE_POLES*radius * cos(lon);
               cyly = REVERSE_POLES*-radius * sin(lon);
               cylz = gridlevelPRIME_to_zPRIME( dtx, time, var, l[i] );
               if(c[i] < 0 || c[i] > dtx->Nc-1 ||
                  r[i] < 0 || r[i] > dtx->Nr-1 ||
                  l[i] < 0 || l[i] > dtx->Nl-1){
                  v++;
               }
               xx = cylx * VERTEX_SCALE;
               yy = cyly * VERTEX_SCALE;
               zz = cylz * VERTEX_SCALE;
               if (xx > 32760.0) xx = 32760.0;
               if (xx < -32760.0) xx = -32760.0;
               if (yy > 32760.0) yy = 32760.0;
               if (yy < -32760.0) yy = -32760.0;
               if (zz > 32760.0) zz = 32760.0;
               if (zz < -32760.0) zz = -32760.0;
               xyz[i-v][0] = xx;
               xyz[i-v][1] = yy;
               xyz[i-v][2] = zz;
            }
            *N = n-v;
         }
         break;
      case PROJ_SPHERICAL:
         for (i=0;i<n;i++) {
            float lat, lon, hgt;
            float clat, clon, slat, slon, d;

            /* convert (r,c,l) to (lat,lon,hgt) */
            lat = dtx->NorthBound - r[i]
                     * (dtx->NorthBound-dtx->SouthBound) / (float) (dtx->Nr-1);
            lon = dtx->WestBound - c[i]
                     * (dtx->WestBound-dtx->EastBound) / (float) (dtx->Nc-1);
            hgt = gridlevelPRIME_to_height( dtx, l[i] );

            /* convert (lat,lon,hgt) to (x,y,z) */
            clat = cos(lat * DEG2RAD);
            clon = cos(lon * DEG2RAD);
            slat = sin(lat * DEG2RAD);
            slon = sin(lon * DEG2RAD);
            d = (hgt-dtx->BottomBound)
               / (dtx->TopBound-dtx->BottomBound) * SPHERE_SCALE + SPHERE_SIZE;
            d *= VERTEX_SCALE;
            if(c[i] < 0 || c[i] > dtx->Nc-1 ||
               r[i] < 0 || r[i] > dtx->Nr-1 ||
               l[i] < 0 || l[i] > dtx->Nl-1){
               v++;
            }
            xx = d * clat * clon;
            yy = -d * clat * slon;
            zz = d * slat;
            if (xx > 32760.0) xx = 32760.0;
            if (xx < -32760.0) xx = -32760.0;
            if (yy > 32760.0) yy = 32760.0;
            if (yy < -32760.0) yy = -32760.0;
            if (zz > 32760.0) zz = 32760.0;
            if (zz < -32760.0) zz = -32760.0;
            xyz[i-v][0] = xx;
            xyz[i-v][1] = yy;
            xyz[i-v][2] = zz;
         }
         *N = n-v;
         break;
      default:
         printf("Error in gridPRIME_to_compXYZPRIME\n");
   }
}



void gridPRIME_to_compXYZPRIME( Display_Context dtx, int time, int var, int n,
                      float r[], float c[], float l[],
                      int_2 xyz[][3] )
{
   int i;
   /* WLH 6 Oct 98 */
   float xx, yy, zz;

   switch (dtx->Projection) {
      case PROJ_GENERIC:
      case PROJ_LINEAR:
      case PROJ_LAMBERT:
      case PROJ_STEREO:
      case PROJ_ROTATED:
      case PROJ_MERCATOR:
         switch (dtx->VerticalSystem) {
            case VERT_GENERIC:
            case VERT_EQUAL_KM:
               /* simplest, fast case */
               {
                  float xs, ys, zs, xt, yt, zt;

                  xs = (dtx->Xmax-dtx->Xmin) / (float) (dtx->Nc-1) * VERTEX_SCALE;
                  ys = (dtx->Ymax-dtx->Ymin) / (float) (dtx->Nr-1) * VERTEX_SCALE;
                  if (dtx->MaxNl > 1) {
                     zs = (dtx->Zmax-dtx->Zmin) / (dtx->MaxNl-1) * VERTEX_SCALE;
                  }
                  else {
                     zs = 0.0;
                  }
                  xt = dtx->Xmin * VERTEX_SCALE;
                  yt = dtx->Ymax * VERTEX_SCALE;
                  zt = dtx->Zmin * VERTEX_SCALE;
                  for (i=0;i<n;i++) {
                     /* WLH 6 Oct 98 */
                     xx = (xt + c[i] * xs);
                     yy = (yt - r[i] * ys);
                     zz = (zt + l[i] * zs);
                     if (xx > 32760.0) xx = 32760.0;
                     if (xx < -32760.0) xx = -32760.0;
                     if (yy > 32760.0) yy = 32760.0;
                     if (yy < -32760.0) yy = -32760.0;
                     if (zz > 32760.0) zz = 32760.0;
                     if (zz < -32760.0) zz = -32760.0;
                     xyz[i][0] = xx;
                     xyz[i][1] = yy;
                     xyz[i][2] = zz;
/* WLH 6 Oct 98
                     xyz[i][0] = (int_2) (xt + c[i] * xs);
                     xyz[i][1] = (int_2) (yt - r[i] * ys);
                     xyz[i][2] = (int_2) (zt + l[i] * zs);
*/
                  }
               }
               break;
            case VERT_NONEQUAL_MB:
            case VERT_NONEQUAL_KM:
               {
                  float xs, ys, zs, xt, yt;
                  xs = (dtx->Xmax-dtx->Xmin) / (float) (dtx->Nc-1) * VERTEX_SCALE;
                  ys = (dtx->Ymax-dtx->Ymin) / (float) (dtx->Nr-1) * VERTEX_SCALE;
                  zs = VERTEX_SCALE;
                  xt = dtx->Xmin * VERTEX_SCALE;
                  yt = dtx->Ymax * VERTEX_SCALE;
                  for (i=0;i<n;i++) {
                     /* WLH 6 Oct 98 */
                     xx = (xt + c[i] * xs);
                     yy = (yt - r[i] * ys);
                     zz = (gridlevelPRIME_to_zPRIME( dtx, time, var, l[i] ) * zs);
                     if (xx > 32760.0) xx = 32760.0;
                     if (xx < -32760.0) xx = -32760.0;
                     if (yy > 32760.0) yy = 32760.0;
                     if (yy < -32760.0) yy = -32760.0;
                     if (zz > 32760.0) zz = 32760.0;
                     if (zz < -32760.0) zz = -32760.0;
                     xyz[i][0] = xx;
                     xyz[i][1] = yy;
                     xyz[i][2] = zz;

/* WLH 6 Oct 98
                     xyz[i][0] = (int_2) (xt + c[i] * xs);
                     xyz[i][1] = (int_2) (yt - r[i] * ys);
                     xyz[i][2] = (int_2)
                                (gridlevelPRIME_to_zPRIME( dtx, time, var, l[i] ) * zs);
*/
                  }
               }
               break;
         }
         break;
      case PROJ_CYLINDRICAL:
         {
            for (i=0;i<n;i++) {
               float lat, lon, radius;
               float cylx, cyly, cylz;
               lat = dtx->NorthBound - r[i]
                       *(dtx->NorthBound-dtx->SouthBound)/(float) (dtx->Nr-1);
               radius = (REVERSE_POLES*90.0 - lat) * dtx->CylinderScale;
               lon = dtx->WestBound - c[i]
                       * (dtx->WestBound-dtx->EastBound) / (float) (dtx->Nc-1);
               lon = REVERSE_POLES*lon * DEG2RAD;
               cylx = REVERSE_POLES*radius * cos(lon);
               cyly = REVERSE_POLES*-radius * sin(lon);
               cylz = gridlevelPRIME_to_zPRIME( dtx, time, var, l[i] );
               xx = cylx * VERTEX_SCALE;
               yy = cyly * VERTEX_SCALE;
               zz = cylz * VERTEX_SCALE;
               if (xx > 32760.0) xx = 32760.0;
               if (xx < -32760.0) xx = -32760.0;
               if (yy > 32760.0) yy = 32760.0;
               if (yy < -32760.0) yy = -32760.0;
               if (zz > 32760.0) zz = 32760.0;
               if (zz < -32760.0) zz = -32760.0;
               xyz[i][0] = xx;
               xyz[i][1] = yy;
               xyz[i][2] = zz;
/*MJK 5 Oct 98
               xyz[i][0] = (int_2) (cylx * VERTEX_SCALE);
               xyz[i][1] = (int_2) (cyly * VERTEX_SCALE);
               xyz[i][2] = (int_2) (cylz * VERTEX_SCALE);
*/
            }
         }
         break;
      case PROJ_SPHERICAL:
         for (i=0;i<n;i++) {
            float lat, lon, hgt;
            float clat, clon, slat, slon, d;

            /* convert (r,c,l) to (lat,lon,hgt) */
            lat = dtx->NorthBound - r[i]
                     * (dtx->NorthBound-dtx->SouthBound) / (float) (dtx->Nr-1);
            lon = dtx->WestBound - c[i]
                     * (dtx->WestBound-dtx->EastBound) / (float) (dtx->Nc-1);
            hgt = gridlevelPRIME_to_height( dtx, l[i] );

            /* convert (lat,lon,hgt) to (x,y,z) */
            clat = cos(lat * DEG2RAD);
            clon = cos(lon * DEG2RAD);
            slat = sin(lat * DEG2RAD);
            slon = sin(lon * DEG2RAD);
            d = (hgt-dtx->BottomBound)
               / (dtx->TopBound-dtx->BottomBound) * SPHERE_SCALE + SPHERE_SIZE;
            d *= VERTEX_SCALE;
            xx = d * clat * clon;
            yy = -d * clat * slon;
            zz = d * slat;
            if (xx > 32760.0) xx = 32760.0;
            if (xx < -32760.0) xx = -32760.0;
            if (yy > 32760.0) yy = 32760.0;
            if (yy < -32760.0) yy = -32760.0;
            if (zz > 32760.0) zz = 32760.0;
            if (zz < -32760.0) zz = -32760.0;
            xyz[i][0] = xx;
            xyz[i][1] = yy;
            xyz[i][2] = zz;
/*MJK 5 Oct 98
            xyz[i][0] = (int_2) (d * clat * clon);
            xyz[i][1] = (int_2) (-d * clat * slon);
            xyz[i][2] = (int_2) (d * slat);
*/
         }
         break;
      default:
         printf("Error in gridPRIME_to_compXYZPRIME\n");
   }
}


/*
 * Transform an array of (lat,lon,hgt) coordinates to (x,y,z) graphics
 * coodinates.
 * Input:  ctx - the vis5d context
 *         time - which timestep
 *         var - which variable
 *         n - number of coordinates
 *         lat - latitude in degrees
 *         lon - longitude in degrees
 *         hgt - height in current vertical units
 * Output:  x, y, z - graphics coordinates.
 */
void geo_to_xyz( Context ctx, int time, int var, int n,
                 float lat[], float lon[], float hgt[],
                 float x[], float y[], float z[] )
{
   float xscale, yscale;
   int i;

   switch (ctx->Projection) {
      case PROJ_GENERIC:
      case PROJ_LINEAR:
         xscale = (ctx->dpy_ctx->Xmax-ctx->dpy_ctx->Xmin) / (ctx->EastBound-ctx->WestBound);
         yscale = (ctx->dpy_ctx->Ymax-ctx->dpy_ctx->Ymin) / (ctx->NorthBound-ctx->SouthBound);
         for (i=0;i<n;i++) {
            x[i] = ctx->dpy_ctx->Xmin + (lon[i]-ctx->WestBound) * xscale;
            y[i] = ctx->dpy_ctx->Ymin + (lat[i]-ctx->SouthBound) * yscale;
            z[i] = height_to_z( ctx, hgt[i] );
         }
         break;
      case PROJ_MERCATOR:
         {
            float row, col, X, Y, ic, jc, YC;
            ic = (ctx->Nr-1)/2.0;
            jc = (ctx->Nc-1) / 2.0;
            YC = RADIUS * log((1.0 + sin(DEG2RAD*ctx->CentralLat))/cos(DEG2RAD*ctx->CentralLat));
            xscale = (ctx->dpy_ctx->Xmax-ctx->dpy_ctx->Xmin) / (float) (ctx->Nc-1);
            yscale = (ctx->dpy_ctx->Ymax-ctx->dpy_ctx->Ymin) / (float) (ctx->Nr-1);
            for (i=0;i<n;i++) {
               X = RADIUS * (lon[i]-ctx->CentralLon) / RAD2DEG;
               Y = RADIUS * log( (1+sin(DEG2RAD*lat[i]))/cos(DEG2RAD*lat[i]) );
               row = ic - (Y - YC)/ctx->RowIncKm;
               col = jc -  X/ctx->ColIncKm;
               x[i] = ctx->dpy_ctx->Xmin + col * xscale;
               y[i] = ctx->dpy_ctx->Ymax - row * yscale;
               z[i] = height_to_z( ctx, hgt[i] );
            }
         }
         break;
      case PROJ_LAMBERT:
         xscale = (ctx->dpy_ctx->Xmax-ctx->dpy_ctx->Xmin) / (float) (ctx->Nc-1);
         yscale = (ctx->dpy_ctx->Ymax-ctx->dpy_ctx->Ymin) / (float) (ctx->Nr-1);
         for (i=0;i<n;i++) {
            float rlon, rlat, r, row, col;
            rlon = lon[i] - ctx->CentralLon;
/*            if (rlon > 180.0)  rlon -= 360.0;*/
            rlon = rlon * ctx->Cone * DEG2RAD;

            if (lat[i]<-85.0) {
               /* infinity */
               r = 10000.0;
            }
            else {
               rlat = (90.0 - ctx->Hemisphere * lat[i]) * DEG2RAD * 0.5;
               r = ctx->ConeFactor * pow( tan(rlat), ctx->Cone );
            }
            row = ctx->PoleRow + r * cos(rlon);
            col = ctx->PoleCol - r * sin(rlon);
            x[i] = ctx->dpy_ctx->Xmin + col * xscale;
            y[i] = ctx->dpy_ctx->Ymax - row * yscale;
            z[i] = height_to_z( ctx, hgt[i] );
         }
         break;
      case PROJ_STEREO:
         xscale = (ctx->dpy_ctx->Xmax-ctx->dpy_ctx->Xmin) / (float) (ctx->Nc-1);
         yscale = (ctx->dpy_ctx->Ymax-ctx->dpy_ctx->Ymin) / (float) (ctx->Nr-1);
         for (i=0;i<n;i++) {
            float rlat, rlon, clon, clat, k, col, row;
            rlat = DEG2RAD * lat[i];
            rlon = DEG2RAD * (ctx->CentralLon - lon[i]);
            clon = cos(rlon);
            clat = cos(rlat);
            k = ctx->StereoScale
                / (1.0 + ctx->SinCentralLat*sin(rlat)
                       + ctx->CosCentralLat*clat*clon);
            col = (ctx->CentralCol-1) + k * clat * sin(rlon);
/* pre-friday:
            row = ctx->Nr - ctx->CentralRow
               - k * (ctx->CosCentralLat * sin(rlat)
                      - ctx->SinCentralLat * clat * clon);
*/
            row = (ctx->CentralRow-1)
               - k * (ctx->CosCentralLat * sin(rlat)
                      - ctx->SinCentralLat * clat * clon);


/*
            if (col<0.0)  col = 0.0;
            if (col>(ctx->Nc-1))  col = ctx->Nc-1;
            if (row<0.0)  row = 0.0;
            if (row>(ctx->Nr-1))  row = ctx->Nr-1;
*/
            x[i] = ctx->dpy_ctx->Xmin + col * xscale;
            y[i] = ctx->dpy_ctx->Ymax - row * yscale;
            z[i] = height_to_z( ctx, hgt[i] );
         }
         break;
      case PROJ_ROTATED:
         xscale = (ctx->dpy_ctx->Xmax-ctx->dpy_ctx->Xmin) / (ctx->EastBound-ctx->WestBound);
         yscale = (ctx->dpy_ctx->Ymax-ctx->dpy_ctx->Ymin) / (ctx->NorthBound-ctx->SouthBound);
         for (i=0;i<n;i++) {
            float lat0, lon0;
            lat0 = lat[i];
            lon0 = lon[i];
            pandg_for(&lat0, &lon0, ctx->CentralLat, ctx->CentralLon,
                      ctx->Rotation);
            x[i] = ctx->dpy_ctx->Xmin + (lon0-ctx->WestBound) * xscale;
            y[i] = ctx->dpy_ctx->Ymin + (lat0-ctx->SouthBound) * yscale;
            z[i] = height_to_z( ctx, hgt[i] );
         }
         break;
      case PROJ_CYLINDRICAL:
         for (i=0;i<n;i++) {
            float longitude, radius;
            radius = (REVERSE_POLES*90.0 - lat[i]) * ctx->CylinderScale;
            longitude = REVERSE_POLES*lon[i] * DEG2RAD;
            x[i] = REVERSE_POLES*radius * cos(longitude);
            y[i] = REVERSE_POLES*-radius * sin(longitude);
            z[i] = height_to_z( ctx, hgt[i] );
         }
         break;
      case PROJ_SPHERICAL:
         for (i=0;i<n;i++) {
            float clat, clon, slat, slon, d;
            clat = cos(lat[i] * DEG2RAD);
            clon = cos(lon[i] * DEG2RAD);
            slat = sin(lat[i] * DEG2RAD);
            slon = sin(lon[i] * DEG2RAD);
            d = (hgt[i]-ctx->BottomBound)
               / (ctx->TopBound-ctx->BottomBound) * SPHERE_SCALE + SPHERE_SIZE;
            x[i] = d * clat * clon;
            y[i] = -d * clat * slon;
            z[i] = d * slat;
         }
         break;
      default:
         printf("Error in geo_to_xyz\n");
   }
}

void geo_to_xyzPRIME( Display_Context dtx, int time, int var, int n,
                 float lat[], float lon[], float hgt[],
                 float x[], float y[], float z[] )
{
   float xscale, yscale;
   int i;

   switch (dtx->Projection) {
      case PROJ_GENERIC:
      case PROJ_LINEAR:
         xscale = (dtx->Xmax-dtx->Xmin) / (dtx->EastBound-dtx->WestBound);
         yscale = (dtx->Ymax-dtx->Ymin) / (dtx->NorthBound-dtx->SouthBound);
         for (i=0;i<n;i++) {
            x[i] = dtx->Xmin + (lon[i]-dtx->WestBound) * xscale;
            y[i] = dtx->Ymin + (lat[i]-dtx->SouthBound) * yscale;
            z[i] = height_to_zPRIME( dtx, hgt[i] );
         }
         break;
      case PROJ_MERCATOR:
         {
            float row, col, X, Y, ic, jc, YC;
            ic = (dtx->Nr-1)/2.0;
            jc = (dtx->Nc-1) / 2.0;
            YC = RADIUS * log((1.0 + sin(DEG2RAD*dtx->CentralLat))/cos(DEG2RAD*dtx->CentralLat));
            xscale = (dtx->Xmax-dtx->Xmin) / (float) (dtx->Nc-1);
            yscale = (dtx->Ymax-dtx->Ymin) / (float) (dtx->Nr-1);
            for (i=0;i<n;i++) {
               X = RADIUS * (lon[i]-dtx->CentralLon) / RAD2DEG;
               Y = RADIUS * log( (1+sin(DEG2RAD*lat[i]))/cos(DEG2RAD*lat[i]) );
               row = ic - (Y - YC)/dtx->RowIncKm;
               col = jc -  X/dtx->ColIncKm;
               x[i] = dtx->Xmin + col * xscale;
               y[i] = dtx->Ymax - row * yscale;
               z[i] = height_to_zPRIME( dtx, hgt[i] );
            }
         }
         break;
      case PROJ_LAMBERT:
         xscale = (dtx->Xmax-dtx->Xmin) / (float) (dtx->Nc-1);
         yscale = (dtx->Ymax-dtx->Ymin) / (float) (dtx->Nr-1);
         for (i=0;i<n;i++) {
            float rlon, rlat, r, row, col;
            rlon = lon[i] - dtx->CentralLon;
/*            if (rlon > 180.0)  rlon -= 360.0;*/
            rlon = rlon * dtx->Cone * DEG2RAD;

            if (lat[i]<-85.0) {
               /* infinity */
               r = 10000.0;
            }
            else {
               rlat = (90.0 - dtx->Hemisphere * lat[i]) * DEG2RAD * 0.5;
               r = dtx->ConeFactor * pow( tan(rlat), dtx->Cone );
            }
            row = dtx->PoleRow + r * cos(rlon);
            col = dtx->PoleCol - r * sin(rlon);
            x[i] = dtx->Xmin + col * xscale;
            y[i] = dtx->Ymax - row * yscale;
            z[i] = height_to_zPRIME( dtx, hgt[i] );
         }
         break;
      case PROJ_STEREO:
         xscale = (dtx->Xmax-dtx->Xmin) / (float) (dtx->Nc-1);
         yscale = (dtx->Ymax-dtx->Ymin) / (float) (dtx->Nr-1);
         for (i=0;i<n;i++) {
            float rlat, rlon, clon, clat, k, col, row;
            rlat = DEG2RAD * lat[i];
            rlon = DEG2RAD * (dtx->CentralLon - lon[i]);
            clon = cos(rlon);
            clat = cos(rlat);
            k = dtx->StereoScale
                / (1.0 + dtx->SinCentralLat*sin(rlat)
                       + dtx->CosCentralLat*clat*clon);
            col = (dtx->CentralCol-1) + k * clat * sin(rlon);
/* pre-friday:
            row = dtx->Nr - dtx->CentralRow
               - k * (dtx->CosCentralLat * sin(rlat)
                      - dtx->SinCentralLat * clat * clon);
*/
            row = (dtx->CentralRow-1)
               - k * (dtx->CosCentralLat * sin(rlat)
                      - dtx->SinCentralLat * clat * clon);


/*
            if (col<0.0)  col = 0.0;
            if (col>(dtx->Nc-1))  col = dtx->Nc-1;
            if (row<0.0)  row = 0.0;
            if (row>(dtx->Nr-1))  row = dtx->Nr-1;
*/
            x[i] = dtx->Xmin + col * xscale;
            y[i] = dtx->Ymax - row * yscale;
            z[i] = height_to_zPRIME( dtx, hgt[i] );
         }
         break;
      case PROJ_ROTATED:
         xscale = (dtx->Xmax-dtx->Xmin) / (dtx->EastBound-dtx->WestBound);
         yscale = (dtx->Ymax-dtx->Ymin) / (dtx->NorthBound-dtx->SouthBound);
         for (i=0;i<n;i++) {
            float lat0, lon0;
            lat0 = lat[i];
            lon0 = lon[i];
            pandg_for(&lat0, &lon0, dtx->CentralLat, dtx->CentralLon,
                      dtx->Rotation);
            x[i] = dtx->Xmin + (lon0-dtx->WestBound) * xscale;
            y[i] = dtx->Ymin + (lat0-dtx->SouthBound) * yscale;
            z[i] = height_to_zPRIME( dtx, hgt[i] );
         }
         break;
      case PROJ_CYLINDRICAL:
         for (i=0;i<n;i++) {
            float longitude, radius;
            radius = (REVERSE_POLES*90.0 - lat[i]) * dtx->CylinderScale;
            longitude = REVERSE_POLES*lon[i] * DEG2RAD;
            x[i] = REVERSE_POLES*radius * cos(longitude);
            y[i] = REVERSE_POLES*-radius * sin(longitude);
            z[i] = height_to_zPRIME( dtx, hgt[i] );
         }
         break;
      case PROJ_SPHERICAL:
         for (i=0;i<n;i++) {
            float clat, clon, slat, slon, d;
            clat = cos(lat[i] * DEG2RAD);
            clon = cos(lon[i] * DEG2RAD);
            slat = sin(lat[i] * DEG2RAD);
            slon = sin(lon[i] * DEG2RAD);
            d = (hgt[i]-dtx->BottomBound)
               / (dtx->TopBound-dtx->BottomBound) * SPHERE_SCALE + SPHERE_SIZE;
            x[i] = d * clat * clon;
            y[i] = -d * clat * slon;
            z[i] = d * slat;
         }
         break;
      default:
         printf("Error in geo_to_xyz\n");
   }
}

/*MJK 2.17.99 begin */
void geo_to_xyzTOPO( Display_Context dtx, int time, int var, int n,
                 float lat[], float lon[], float hgt[],
                 float x[], float y[], float z[] )
{
   float xscale, yscale;
   int i;

   switch (dtx->Projection) {
      case PROJ_GENERIC:
      case PROJ_LINEAR:
         xscale = (dtx->Xmax-dtx->Xmin) / (dtx->EastBound-dtx->WestBound);
         yscale = (dtx->Ymax-dtx->Ymin) / (dtx->NorthBound-dtx->SouthBound);
         for (i=0;i<n;i++) {
            x[i] = dtx->Xmin + (lon[i]-dtx->WestBound) * xscale;
            y[i] = dtx->Ymin + (lat[i]-dtx->SouthBound) * yscale;
            z[i] = height_to_zTOPO( dtx, hgt[i] );
         }
         break;
      case PROJ_MERCATOR:
         {
            float row, col, X, Y, ic, jc, YC;
            ic = (dtx->Nr-1) / 2.0;
            jc = (dtx->Nc-1) / 2.0;
            YC = RADIUS * log((1.0 + sin(DEG2RAD*dtx->CentralLat))/cos(DEG2RAD*dtx->CentralLat));
            xscale = (dtx->Xmax-dtx->Xmin) / (float) (dtx->Nc-1);
            yscale = (dtx->Ymax-dtx->Ymin) / (float) (dtx->Nr-1);
            for (i=0;i<n;i++) {
               X = RADIUS * (lon[i]-dtx->CentralLon) / RAD2DEG;
               Y = RADIUS * log( (1+sin(lat[i]*DEG2RAD))/cos(lat[i]*DEG2RAD) );
               row = ic - (Y - YC)/dtx->RowIncKm;
               col = jc - X/dtx->ColIncKm;
               x[i] = dtx->Xmin + col * xscale;
               y[i] = dtx->Ymax - row * yscale;
               z[i] = height_to_zTOPO( dtx, hgt[i] );
            }
         }
         break;
      case PROJ_LAMBERT:
         xscale = (dtx->Xmax-dtx->Xmin) / (float) (dtx->Nc-1);
         yscale = (dtx->Ymax-dtx->Ymin) / (float) (dtx->Nr-1);
         for (i=0;i<n;i++) {
            float rlon, rlat, r, row, col;
            rlon = lon[i] - dtx->CentralLon;
/*            if (rlon > 180.0)  rlon -= 360.0;*/
            rlon = rlon * dtx->Cone * DEG2RAD;

            if (lat[i]<-85.0) {
               /* infinity */
               r = 10000.0;
            }
            else {
               rlat = (90.0 - dtx->Hemisphere * lat[i]) * DEG2RAD * 0.5;
               r = dtx->ConeFactor * pow( tan(rlat), dtx->Cone );
            }
            row = dtx->PoleRow + r * cos(rlon);
            col = dtx->PoleCol - r * sin(rlon);
            x[i] = dtx->Xmin + col * xscale;
            y[i] = dtx->Ymax - row * yscale;
            z[i] = height_to_zTOPO( dtx, hgt[i] );
         }
         break;
      case PROJ_STEREO:
         xscale = (dtx->Xmax-dtx->Xmin) / (float) (dtx->Nc-1);
         yscale = (dtx->Ymax-dtx->Ymin) / (float) (dtx->Nr-1);
         for (i=0;i<n;i++) {
            float rlat, rlon, clon, clat, k, col, row;
            rlat = DEG2RAD * lat[i];
            rlon = DEG2RAD * (dtx->CentralLon - lon[i]);
            clon = cos(rlon);
            clat = cos(rlat);
            k = dtx->StereoScale
                / (1.0 + dtx->SinCentralLat*sin(rlat)
                       + dtx->CosCentralLat*clat*clon);
            col = (dtx->CentralCol-1) + k * clat * sin(rlon);
/* pre-friday:
            row = dtx->Nr - dtx->CentralRow
               - k * (dtx->CosCentralLat * sin(rlat)
                      - dtx->SinCentralLat * clat * clon);
*/
            row = (dtx->CentralRow-1)
               - k * (dtx->CosCentralLat * sin(rlat)
                      - dtx->SinCentralLat * clat * clon);


/*
            if (col<0.0)  col = 0.0;
            if (col>(dtx->Nc-1))  col = dtx->Nc-1;
            if (row<0.0)  row = 0.0;
            if (row>(dtx->Nr-1))  row = dtx->Nr-1;
*/
            x[i] = dtx->Xmin + col * xscale;
            y[i] = dtx->Ymax - row * yscale;
            z[i] = height_to_zTOPO( dtx, hgt[i] );
         }
         break;
      case PROJ_ROTATED:
         xscale = (dtx->Xmax-dtx->Xmin) / (dtx->EastBound-dtx->WestBound);
         yscale = (dtx->Ymax-dtx->Ymin) / (dtx->NorthBound-dtx->SouthBound);
         for (i=0;i<n;i++) {
            float lat0, lon0;
            lat0 = lat[i];
            lon0 = lon[i];
            pandg_for(&lat0, &lon0, dtx->CentralLat, dtx->CentralLon,
                      dtx->Rotation);
            x[i] = dtx->Xmin + (lon0-dtx->WestBound) * xscale;
            y[i] = dtx->Ymin + (lat0-dtx->SouthBound) * yscale;
            z[i] = height_to_zTOPO( dtx, hgt[i] );
         }
         break;
      case PROJ_CYLINDRICAL:
         for (i=0;i<n;i++) {
            float longitude, radius;
            radius = (REVERSE_POLES*90.0 - lat[i]) * dtx->CylinderScale;
            longitude = REVERSE_POLES*lon[i] * DEG2RAD;
            x[i] = REVERSE_POLES*radius * cos(longitude);
            y[i] = REVERSE_POLES*-radius * sin(longitude);
            z[i] = height_to_zTOPO( dtx, hgt[i] );
         }
         break;
      case PROJ_SPHERICAL:
         for (i=0;i<n;i++) {
            float clat, clon, slat, slon, d;
            clat = cos(lat[i] * DEG2RAD);
            clon = cos(lon[i] * DEG2RAD);
            slat = sin(lat[i] * DEG2RAD);
            slon = sin(lon[i] * DEG2RAD);
            d = (hgt[i]-dtx->BottomBound)
               / (dtx->TopBound-dtx->BottomBound) * SPHERE_SCALE + SPHERE_SIZE;
            x[i] = d * clat * clon;
            y[i] = -d * clat * slon;
            z[i] = d * slat;
         }
         break;
      default:
         printf("Error in geo_to_xyz\n");
   }
}
/*MJK 2.17.99 end */






/*
 * Transform a (row,column) grid coordinate to (lat,lon) geographic coord.
 * Input:  ctx - the vis5d context
 *         time, var - which timestep and variable
 *         row, col - the row and column
 * Output:  lat, lon - latitude and longitude
 */
void rowcol_to_latlon( Context ctx, int time, int var, float row, float col,
                       float *lat, float *lon )
{

   switch (ctx->Projection) {
      case PROJ_GENERIC:
      case PROJ_LINEAR:
      case PROJ_CYLINDRICAL:
      case PROJ_SPHERICAL:
         *lat = ctx->NorthBound - row * (ctx->NorthBound-ctx->SouthBound)
                / (float) (ctx->Nr-1);
         *lon = ctx->WestBound - col * (ctx->WestBound-ctx->EastBound)
                / (float) (ctx->Nc-1);
         break;
      case PROJ_MERCATOR:
         {
            float YC, alpha, ic, jc;
            YC = RADIUS * log((1.0 + sin(DEG2RAD*ctx->CentralLat))/cos(DEG2RAD*ctx->CentralLat));
            ic = (ctx->Nr-1)/2.0;
            jc = (ctx->Nc-1)/2.0;
            alpha = ( (ic-row) * ctx->RowIncKm + YC) / RADIUS;
            *lat = 2 * RAD2DEG * atan( exp(alpha) ) - 90.0;
            *lon = ctx->CentralLon - RAD2DEG * (col-jc) * ctx->ColIncKm / RADIUS;
         }
         break;
      case PROJ_LAMBERT:
         {
            float xldif, xedif, xrlon, radius;

            xldif = ctx->Hemisphere * (row-ctx->PoleRow) / ctx->ConeFactor;
            xedif = (ctx->PoleCol-col) / ctx->ConeFactor;
            if (xldif==0.0 && xedif==0.0)
               xrlon = 0.0;
            else
               xrlon = atan2( xedif, xldif );
            *lon = xrlon / ctx->Cone * RAD2DEG + ctx->CentralLon;
            if (*lon > 180.0)
               *lon -= 360.0;

            radius = sqrt( xldif*xldif + xedif*xedif );
            if (radius < 0.0001)
               *lat = 90.0 * ctx->Hemisphere;   /* +/-90 */
            else
               *lat = ctx->Hemisphere
                      * (90.0 - 2.0*atan(exp(log(radius)/ctx->Cone))*RAD2DEG);
         }
         break;
      case PROJ_STEREO:
         {
            float xrow, xcol, rho, c, cc, sc;
            xrow = ctx->CentralRow - row -1;
            xcol = ctx->CentralCol - col -1;
            rho = xrow*xrow + xcol*xcol;
            if (rho<1.0e-20) {
               *lat = ctx->CentralLat;
               *lon = ctx->CentralLon;
            }
            else {
               rho = sqrt( rho );
               c = 2.0 * atan( rho * ctx->InvScale);
               cc = cos(c);
               sc = sin(c);
               *lat = RAD2DEG
                    * asin( cc*ctx->SinCentralLat
                            + xrow*sc*ctx->CosCentralLat / rho );
               *lon = ctx->CentralLon + RAD2DEG * atan2( xcol * sc,
                         (rho * ctx->CosCentralLat * cc
                      - xrow * ctx->SinCentralLat * sc) );
               if (*lon < -180.0)  *lon += 360.0;
               else if (*lon > 180.0)  *lon -= 360.0;
            }
         }
         break;
      case PROJ_ROTATED:
         *lat = ctx->NorthBound - row
                     * (ctx->NorthBound-ctx->SouthBound) / (float) (ctx->Nr-1);
         *lon = ctx->WestBound - col
                     * (ctx->WestBound-ctx->EastBound) / (float) (ctx->Nc-1);
         pandg_back(lat, lon, ctx->CentralLat, ctx->CentralLon, ctx->Rotation);
         break;
      default:
         printf("Error in rowcol_to_latlon\n");
   }
}

void rowcolPRIME_to_latlon( Display_Context dtx, int time, int var, float row, float col,
                       float *lat, float *lon )
{

   switch (dtx->Projection) {
      case PROJ_GENERIC:
      case PROJ_LINEAR:
      case PROJ_CYLINDRICAL:
      case PROJ_SPHERICAL:
         *lat = dtx->NorthBound - row * (dtx->NorthBound-dtx->SouthBound)
                / (float) (dtx->Nr-1);
         *lon = dtx->WestBound - col * (dtx->WestBound-dtx->EastBound)
                / (float) (dtx->Nc-1);
         break;
      case PROJ_MERCATOR:
         {
            float YC, alpha, ic, jc;
            YC = RADIUS * log((1.0 + sin(DEG2RAD*dtx->CentralLat))/cos(DEG2RAD*dtx->CentralLat));
            ic = (dtx->Nr-1)/2.0;
            jc = (dtx->Nc-1)/2.0;
            alpha = ( (ic-row) * dtx->RowIncKm + YC) / RADIUS;
            *lat = 2 * RAD2DEG * atan( exp(alpha) ) - 90.0;
            *lon = dtx->CentralLon - RAD2DEG * (col-jc) * dtx->ColIncKm / RADIUS;
         }
         break;
      case PROJ_LAMBERT:
         {
            float xldif, xedif, xrlon, radius;

            xldif = dtx->Hemisphere * (row-dtx->PoleRow) / dtx->ConeFactor;
            xedif = (dtx->PoleCol-col) / dtx->ConeFactor;
            if (xldif==0.0 && xedif==0.0)
               xrlon = 0.0;
            else
               xrlon = atan2( xedif, xldif );
            *lon = xrlon / dtx->Cone * RAD2DEG + dtx->CentralLon;
            if (*lon > 180.0)
               *lon -= 360.0;

            radius = sqrt( xldif*xldif + xedif*xedif );
            if (radius < 0.0001)
               *lat = 90.0 * dtx->Hemisphere;   /* +/-90 */
            else
               *lat = dtx->Hemisphere
                      * (90.0 - 2.0*atan(exp(log(radius)/dtx->Cone))*RAD2DEG);
         }
         break;
      case PROJ_STEREO:
         {
            float xrow, xcol, rho, c, cc, sc;
            xrow = dtx->CentralRow - row -1;
            xcol = dtx->CentralCol - col -1;
            rho = xrow*xrow + xcol*xcol;
            if (rho<1.0e-20) {
               *lat = dtx->CentralLat;
               *lon = dtx->CentralLon;
            }
            else {
               rho = sqrt( rho );
               c = 2.0 * atan( rho * dtx->InvScale);
               cc = cos(c);
               sc = sin(c);
               *lat = RAD2DEG
                    * asin( cc*dtx->SinCentralLat
                            + xrow*sc*dtx->CosCentralLat / rho );
               *lon = dtx->CentralLon + RAD2DEG * atan2( xcol * sc,
                         (rho * dtx->CosCentralLat * cc
                      - xrow * dtx->SinCentralLat * sc) );
               if (*lon < -180.0)  *lon += 360.0;
               else if (*lon > 180.0)  *lon -= 360.0;
            }
         }
         break;
      case PROJ_ROTATED:
         *lat = dtx->NorthBound - row
                     * (dtx->NorthBound-dtx->SouthBound) / (float) (dtx->Nr-1);
         *lon = dtx->WestBound - col
                     * (dtx->WestBound-dtx->EastBound) / (float) (dtx->Nc-1);
         pandg_back(lat, lon, dtx->CentralLat, dtx->CentralLon, dtx->Rotation);
         break;
      default:
         printf("Error in rowcolPRIME_to_latlon\n");
   }
}

/*
 * Convert an (x,y,z) display graphics coordinate to an (r,c,l) display grid coordinate.
 * Input:  dtx - the display context
 *         time, var - which timestep, variable
 *         x, y, z - the graphics coordinate
 * Output:  row, col, lev - the corresponding grid coordinate.
 */
void xyzPRIME_to_gridPRIME( Display_Context dtx, int time, int var,
                  float x, float y, float z,
                  float *row, float *col, float *lev )
{
   switch (dtx->Projection) {
      case PROJ_GENERIC:
      case PROJ_LINEAR:
      case PROJ_LAMBERT:
      case PROJ_STEREO:
      case PROJ_ROTATED:
      case PROJ_MERCATOR:
         *col = (x-dtx->Xmin) / (dtx->Xmax-dtx->Xmin) * (float) (dtx->Nc-1);
         *row = (dtx->Ymax-y) / (dtx->Ymax-dtx->Ymin) * (float) (dtx->Nr-1);
         *lev = zPRIME_to_gridlevPRIME( dtx, z );
         break;
      case PROJ_CYLINDRICAL:
         {
            float lat, lon, r;
            r = sqrt( x*x + y*y );
            if (r<0.001) {
               /* pole */
               lat = REVERSE_POLES*90.0;
               lon = 0.0;
            }
            else {
               lat = REVERSE_POLES*(90.0 - r / dtx->CylinderScale);
               lon = REVERSE_POLES* atan2( -y, x ) * RAD2DEG;
               while (lon<dtx->EastBound)  lon += 360.0;
               while (lon>dtx->WestBound)  lon -= 360.0;
            }
            *col = (lon-dtx->WestBound) / (dtx->EastBound-dtx->WestBound) * (float) (dtx->Nc-1);
            *row = (lat-dtx->NorthBound) / (dtx->SouthBound-dtx->NorthBound) * (float) (dtx->Nr-1);
            *lev = zPRIME_to_gridlevPRIME( dtx, z );
         }
         break;
      case PROJ_SPHERICAL:
         {
            float r;
            r = sqrt( x*x + y*y + z*z );
            if (r<0.001) {
               /* degenerate case */
               *col = 0.0;
               *row = 0.0;
               *lev = 0.0;
            }
            else {
               float lat, lon, hgt, rxy;
               lon = atan2( -y, x ) * RAD2DEG;
               rxy = sqrt( x*x + y*y );
               if (rxy<0.001) {
                  /* north or south pole */
                  if (z<0.0) {
                     lat = -90.0;
                  }
                  else {
                     lat = 90.0;
                  }
                  lon = 0.0;
               }
               else {
                  lat = atan( z / rxy ) * RAD2DEG;
               }
               hgt = (r-SPHERE_SIZE) / SPHERE_SCALE * (dtx->TopBound-dtx->BottomBound)
                    + dtx->BottomBound;
               *col = (lon-dtx->WestBound) / (dtx->EastBound-dtx->WestBound) * (float) (dtx->Nc-1);
               *row = (lat-dtx->NorthBound) / (dtx->SouthBound-dtx->NorthBound)
                       * (float) (dtx->Nr-1);
               *lev = height_to_gridlevPRIME( dtx, hgt );
            }
         }
         break;
      default:
         printf("Error in xyzPRIME_to_gridPRIME\n");
   }
}


void xyzPRIME_to_grid( Context ctx, int time, int var,
                  float x, float y, float z,
                  float *row, float *col, float *lev )
{
   float lt, ln, ht, lat[1], lon[1], hgt[1];
   
   xyzPRIME_to_geo( ctx->dpy_ctx, time, var, x, y, z, &lt, &ln, &ht);
   lat[0] = lt;
   lon[0] = ln;
   hgt[0] = ht;
   geo_to_grid( ctx, time, var, 1, lat, lon, hgt, row, col, lev);
}

/*
 * Convert an (x,y,z) graphics coordinate to an (r,c,l) grid coordinate.
 * Input:  ctx - the vis5d context
 *         time, var - which timestep, variable
 *         x, y, z - the graphics coordinate
 * Output:  row, col, lev - the corresponding grid coordinate.
 */
void xyz_to_grid( Context ctx, int time, int var,
                  float x, float y, float z,
                  float *row, float *col, float *lev )
{
   switch (ctx->Projection) {
      case PROJ_GENERIC:
      case PROJ_LINEAR:
      case PROJ_LAMBERT:
      case PROJ_STEREO:
      case PROJ_ROTATED:
      case PROJ_MERCATOR:
         *col = (x-ctx->dpy_ctx->Xmin) / (ctx->dpy_ctx->Xmax-ctx->dpy_ctx->Xmin)
                * (float) (ctx->Nc-1);
         *row = (ctx->dpy_ctx->Ymax-y) / (ctx->dpy_ctx->Ymax-ctx->dpy_ctx->Ymin)
                * (float) (ctx->Nr-1);
         *lev = z_to_gridlev( ctx, z );
         break;
      case PROJ_CYLINDRICAL:
         {
            float lat, lon, r;
            r = sqrt( x*x + y*y );
            if (r<0.001) {
               /* pole */
               lat = REVERSE_POLES*90.0;
               lon = 0.0;
            }
            else {
               lat = REVERSE_POLES*(90.0 - r / ctx->CylinderScale);
               lon = REVERSE_POLES*atan2( -y, x ) * RAD2DEG;
               while (lon<ctx->EastBound)  lon += 360.0;
               while (lon>ctx->WestBound)  lon -= 360.0;
            }
            *col = (lon-ctx->WestBound) / (ctx->EastBound-ctx->WestBound) * (float) (ctx->Nc-1);
            *row = (lat-ctx->NorthBound) / (ctx->SouthBound-ctx->NorthBound) * (float) (ctx->Nr-1);
            *lev = z_to_gridlev( ctx, z );
         }
         break;
      case PROJ_SPHERICAL:
         {
            float r;
            r = sqrt( x*x + y*y + z*z );
            if (r<0.001) {
               /* degenerate case */
               *col = 0.0;
               *row = 0.0;
               *lev = 0.0;
            }
            else {
               float lat, lon, hgt, rxy;
               lon = atan2( -y, x ) * RAD2DEG;
               rxy = sqrt( x*x + y*y );
               if (rxy<0.001) {
                  /* north or south pole */
                  if (z<0.0) {
                     lat = -90.0;
                  }
                  else {
                     lat = 90.0;
                  }
                  lon = 0.0;
               }
               else {
                  lat = atan( z / rxy ) * RAD2DEG;
               }
               hgt = (r-SPHERE_SIZE) / SPHERE_SCALE * (ctx->TopBound-ctx->BottomBound)
                    + ctx->BottomBound;
               *col = (lon-ctx->WestBound) / (ctx->EastBound-ctx->WestBound) * (float) (ctx->Nc-1);
               *row = (lat-ctx->NorthBound) / (ctx->SouthBound-ctx->NorthBound)
                       * (float) (ctx->Nr-1);
               *lev = height_to_gridlev( ctx, hgt );
            }
         }
         break;
      default:
         printf("Error in xyz_to_grid\n");
   }
}

/**********************************************************************************/
/* 5.0 new stuff                                                                  */
/**********************************************************************************/
/* Convert a (lat, lon) earth coordinates to (row, col) grid                      */
/* coordinates.                                                                   */
/* Input: ctx - the vis5d context                                                 */
/*        time, var - which timestep, variable.                                   */
/*        lat, lon - earth or geographic coordinates                              */
/* Output: row, col  - grid or file coordinates                                   */
/**********************************************************************************/
/* Questions- Should I check to see if RowInc, and ColInc are zero, can they be?  */
/*           Put it in the setup somehwere to make sure there are no zeros there  */
/**********************************************************************************/

void latlon_to_rowcol (Context ctx, int time, int var,
                       float lat, float lon,
                       float *row, float *col)
{
   switch (ctx->Projection) {
      case PROJ_GENERIC:
      case PROJ_LINEAR:
      case PROJ_CYLINDRICAL:
      case PROJ_SPHERICAL:
         *row = (ctx->NorthBound - lat)/ctx->RowInc;
         *col = (ctx->WestBound - lon)/ctx->ColInc;
         break;
      case PROJ_MERCATOR:
         {
            float X, Y, ic, jc, YC;
            ic = (ctx->Nr-1)/2.0;
            jc = (ctx->Nc-1) / 2.0;
            YC = RADIUS * log((1.0 + sin(DEG2RAD*ctx->CentralLat))/cos(DEG2RAD*ctx->CentralLat));
            X = RADIUS * (lon-ctx->CentralLon) / RAD2DEG;
            Y = RADIUS * log( (1+sin(DEG2RAD*lat))/cos(DEG2RAD*lat) );
            *row = ic - (Y - YC)/ctx->RowIncKm;
            *col = jc -  X/ctx->ColIncKm;
          }
          break;
      case PROJ_LAMBERT:
         {
            float rlon, rlat, r;
            int x, y, z;

            rlon = lon - ctx->CentralLon;
            rlon = rlon * ctx->Cone * DEG2RAD;

            if (lat < -85.0) {
               /* infinity */
               r = 10000.0;
            }
            else {
               rlat = (90.0 - ctx->Hemisphere * lat) * DEG2RAD * 0.5;
               r = ctx->ConeFactor * pow( tan(rlat), ctx->Cone );
            }
            *row = ctx->PoleRow + r * cos(rlon);
            *col = ctx->PoleCol - r * sin(rlon);
         }
         break;
      case PROJ_STEREO:
         {
            float rlat, rlon, clon, clat, k;
 
            rlat = DEG2RAD * lat;
            rlon = DEG2RAD * (ctx->CentralLon - lon);
            clon = cos(rlon);
            clat = cos(rlat);
            k = ctx->StereoScale
                / (1.0 + ctx->SinCentralLat*sin(rlat)
                       + ctx->CosCentralLat*clat*clon);
            *col = (ctx->CentralCol-1) + k * clat * sin(rlon);
            *row = (ctx->CentralRow-1)
               - k * (ctx->CosCentralLat * sin(rlat)
                      - ctx->SinCentralLat * clat * clon);
         }
         break;
      case PROJ_ROTATED:
         {
            float lat0, lon0;
 
            pandg_for(&lat0, &lon0, ctx->CentralLat, ctx->CentralLon,
                      ctx->Rotation);
            *row = (ctx->NorthBound - lat)/ctx->RowInc;
            *col = (ctx->WestBound - lon)/ctx->ColInc;
         }
         break;
      default:
         printf("Error in latlon_to_rowcol\n");
   }
}


void latlon_to_rowcolPRIME(Display_Context dtx, int time, int var,
                       float lat, float lon,
                       float *row, float *col)
{
   switch (dtx->Projection) {
      case PROJ_GENERIC:
      case PROJ_LINEAR:
      case PROJ_CYLINDRICAL:
      case PROJ_SPHERICAL:
         *row = (dtx->NorthBound - lat)/dtx->RowInc;
         *col = (dtx->WestBound - lon)/dtx->ColInc;
         break;
      case PROJ_MERCATOR:
         {
            float X, Y, ic, jc, YC;
            ic = (dtx->Nr-1)/2.0;
            jc = (dtx->Nc-1) / 2.0;
            YC = RADIUS * log((1.0 + sin(DEG2RAD*dtx->CentralLat))/cos(DEG2RAD*dtx->CentralLat));
            X = RADIUS * (lon-dtx->CentralLon) / RAD2DEG;
            Y = RADIUS * log( (1+sin(DEG2RAD*lat))/cos(DEG2RAD*lat) );
            *row = ic - (Y - YC)/dtx->RowIncKm;
            *col = jc -  X/dtx->ColIncKm;
          }
          break;
      case PROJ_LAMBERT:
         {
            float rlon, rlat, r;
            int x, y, z;

            rlon = lon - dtx->CentralLon;
            rlon = rlon * dtx->Cone * DEG2RAD;

            if (lat < -85.0) {
               /* infinity */
               r = 10000.0;
            }
            else {
               rlat = (90.0 - dtx->Hemisphere * lat) * DEG2RAD * 0.5;
               r = dtx->ConeFactor * pow( tan(rlat), dtx->Cone );
            }
            *row = dtx->PoleRow + r * cos(rlon);
            *col = dtx->PoleCol - r * sin(rlon);
         }
         break;
      case PROJ_STEREO:
         {
            float rlat, rlon, clon, clat, k;

            rlat = DEG2RAD * lat;
            rlon = DEG2RAD * (dtx->CentralLon - lon);
            clon = cos(rlon);
            clat = cos(rlat);
            k = dtx->StereoScale
                / (1.0 + dtx->SinCentralLat*sin(rlat)
                       + dtx->CosCentralLat*clat*clon);
            *col = (dtx->CentralCol-1) + k * clat * sin(rlon);
            *row = (dtx->CentralRow-1)
               - k * (dtx->CosCentralLat * sin(rlat)
                      - dtx->SinCentralLat * clat * clon);
         }
         break;
      case PROJ_ROTATED:
         {
            float lat0, lon0;

            pandg_for(&lat0, &lon0, dtx->CentralLat, dtx->CentralLon,
                      dtx->Rotation);
            *row = (dtx->NorthBound - lat)/dtx->RowInc;
            *col = (dtx->WestBound - lon)/dtx->ColInc;
         }
         break;
      default:
         printf("Error in latlon_to_rowcol\n");
   }
}

/**********************************************************************************/
/* 5.0 new stuff                                                                  */
/**********************************************************************************/
/* Convert a (lat, lon, hgt) earth coordinates to (row, col, lev) grid            */
/* coordinates.                                                                   */
/* Input: ctx - the vis5d context                                                 */
/*        time, var - which timestep, variable.                                   */
/*        lat, lon, hgt - earth or geographic coordinates                         */
/*        n - number of points                                                    */
/* Output: row, col, lev  - grid or file coordinates                              */
/**********************************************************************************/
/* Questions- Should I check to see if RowInc, and ColInc are zero, can they be?  */
/*           Put it in the setup somehwere to make sure there are no zeros there  */
/**********************************************************************************/

void geo_to_grid (Context ctx, int time, int var, int n,
                  float lat[], float lon[], float hgt[],
                  float row[], float col[], float lev[])
{
   int i;

   switch (ctx->Projection) {
      case PROJ_GENERIC:
      case PROJ_LINEAR:
      case PROJ_CYLINDRICAL:
      case PROJ_SPHERICAL:
         for (i=0;i<n;i++) {
            row[i] = (ctx->NorthBound - lat[i])/ctx->RowInc;
            col[i] = (ctx->WestBound - lon[i])/ctx->ColInc;
         }
         break;
      case PROJ_MERCATOR:
         {
            float X, Y, ic, jc, YC;
            ic = (ctx->Nr-1)/2.0;
            jc = (ctx->Nc-1) / 2.0;
            YC = RADIUS * log((1.0 + sin(DEG2RAD*ctx->CentralLat))/cos(DEG2RAD*ctx->CentralLat));
            for (i=0;i<n;i++) {
               X = RADIUS * (lon[i]-ctx->CentralLon) / RAD2DEG;
               Y = RADIUS * log( (1+sin(DEG2RAD*lat[i]))/cos(DEG2RAD*lat[i]) );
               row[i] = ic - (Y - YC)/ctx->RowIncKm;
               col[i] = jc - X/ctx->ColIncKm;
             }
          }
          break;
      case PROJ_LAMBERT:
         {
            float rlon, rlat, r;
            int x, y, z;

            for (i=0;i<n;i++) {
               rlon = lon[i] - ctx->CentralLon;
               rlon = rlon * ctx->Cone * DEG2RAD;

               if (lat[i] < -85.0) {
                  /* infinity */
                  r = 10000.0;
               }
               else {
                  rlat = (90.0 - ctx->Hemisphere * lat[i]) * DEG2RAD * 0.5;
                  r = ctx->ConeFactor * pow( tan(rlat), ctx->Cone );
               }
               row[i] = ctx->PoleRow + r * cos(rlon);
               col[i] = ctx->PoleCol - r * sin(rlon);
            }   
         }
         break;
      case PROJ_STEREO:
         {
            float rlat, rlon, clon, clat, k;
 
            for (i=0;i<n;i++) {
               rlat = DEG2RAD * lat[i];
               rlon = DEG2RAD * (ctx->CentralLon - lon[i]);
               clon = cos(rlon);
               clat = cos(rlat);
               k = ctx->StereoScale
                   / (1.0 + ctx->SinCentralLat*sin(rlat)
                          + ctx->CosCentralLat*clat*clon);
               col[i] = (ctx->CentralCol-1) + k * clat * sin(rlon);
               row[i] = (ctx->CentralRow-1)
                  - k * (ctx->CosCentralLat * sin(rlat)
                         - ctx->SinCentralLat * clat * clon);
            }
         }
         break;
      case PROJ_ROTATED:
         {
            float lat0, lon0;
   
            for (i=0;i<n;i++) {
               lat0 = lat[i];
               lon0 = lon[i];
               pandg_for(&lat0, &lon0, ctx->CentralLat, ctx->CentralLon,
                         ctx->Rotation);
               row[i] = (ctx->NorthBound - lat0)/ctx->RowInc;
               
               col[i] = (ctx->WestBound - lon0)/ctx->ColInc;
            }
         }
         break;
      default:
         printf("Error in geo_to_grid\n");
   }
   for (i=0;i<n;i++) {
      lev[i] = height_to_gridlev( ctx, hgt[i]);
   }
} 
 
void geo_to_gridPRIME (Display_Context dtx, int time, int var, int n,
                  float lat[], float lon[], float hgt[],
                  float row[], float col[], float lev[])
{
   int i;

   switch (dtx->Projection) {
      case PROJ_GENERIC:
      case PROJ_LINEAR:
      case PROJ_CYLINDRICAL:
      case PROJ_SPHERICAL:
         for (i=0;i<n;i++) {
            row[i] = (dtx->NorthBound - lat[i])/dtx->RowInc;
            col[i] = (dtx->WestBound - lon[i])/dtx->ColInc;
         }
         break;
      case PROJ_MERCATOR:
         {
            float X, Y, ic, jc, YC;
            ic = (dtx->Nr-1)/2.0;
            jc = (dtx->Nc-1) / 2.0;
            YC = RADIUS * log((1.0 + sin(DEG2RAD*dtx->CentralLat))/cos(DEG2RAD*dtx->CentralLat));
            for (i=0;i<n;i++) {
               X = RADIUS * (lon[i]-dtx->CentralLon) / RAD2DEG;
               Y = RADIUS * log( (1+sin(DEG2RAD*lat[i]))/cos(DEG2RAD*lat[i]) );
               row[i] = ic - (Y - YC)/dtx->RowIncKm;
               col[i] = jc - X/dtx->ColIncKm;
             }
          }
          break;
      case PROJ_LAMBERT:
         {
            float rlon, rlat, r;
            int x, y, z;

            for (i=0;i<n;i++) {
               rlon = lon[i] - dtx->CentralLon;
               rlon = rlon * dtx->Cone * DEG2RAD;

               if (lat[i] < -85.0) {
                  /* infinity */
                  r = 10000.0;
               }
               else {
                  rlat = (90.0 - dtx->Hemisphere * lat[i]) * DEG2RAD * 0.5;
                  r = dtx->ConeFactor * pow( tan(rlat), dtx->Cone );
               }
               row[i] = dtx->PoleRow + r * cos(rlon);
               col[i] = dtx->PoleCol - r * sin(rlon);
            }
         }
         break;
      case PROJ_STEREO:
         {
            float rlat, rlon, clon, clat, k;

            for (i=0;i<n;i++) {
               rlat = DEG2RAD * lat[i];
               rlon = DEG2RAD * (dtx->CentralLon - lon[i]);
               clon = cos(rlon);
               clat = cos(rlat);
               k = dtx->StereoScale
                   / (1.0 + dtx->SinCentralLat*sin(rlat)
                          + dtx->CosCentralLat*clat*clon);
               col[i] = (dtx->CentralCol-1) + k * clat * sin(rlon);
               row[i] = (dtx->CentralRow-1)
                  - k * (dtx->CosCentralLat * sin(rlat)
                         - dtx->SinCentralLat * clat * clon);
            }
         }
         break;
      case PROJ_ROTATED:
         {
            float lat0, lon0;

            for (i=0;i<n;i++) {
               lat0 = lat[i];
               lon0 = lon[i];
               pandg_for(&lat0, &lon0, dtx->CentralLat, dtx->CentralLon,
                         dtx->Rotation);
               row[i] = (dtx->NorthBound - lat0)/dtx->RowInc;
               col[i] = (dtx->WestBound - lon0)/dtx->ColInc;
            }
         }
         break;
      default:
         printf("Error in geo_to_grid\n");
   }
   for (i=0;i<n;i++) {
      lev[i] = height_to_gridlevPRIME( dtx, hgt[i]);
   }
}


void gridPRIME_to_geo (Display_Context dtx, int time, int var, int n,
                  float row[], float col[], float lev[],
                  float lat[], float lon[], float hgt[])
{
   int i;

   switch (dtx->Projection) {
      case PROJ_GENERIC:
      case PROJ_LINEAR:
      case PROJ_CYLINDRICAL:
      case PROJ_SPHERICAL:
         for (i=0;i<n;i++) {
            lat[i] = dtx->NorthBound - row[i] * dtx->RowInc;
            lon[i] = dtx->WestBound  - col[i] * dtx->ColInc;
         }
         break;
      case PROJ_MERCATOR:
         {
            float YC, alpha, ic, jc;
            YC = RADIUS * log((1.0 + sin(DEG2RAD*dtx->CentralLat))/cos(DEG2RAD*dtx->CentralLat));
            ic = (dtx->Nr-1)/2.0;
            jc = (dtx->Nc-1)/2.0;
            for (i=0;i<n;i++) {
               alpha = ( (ic-row[i]) * dtx->RowIncKm + YC) / RADIUS;
               lat[i] = 2 * RAD2DEG * atan( exp(alpha) ) - 90.0;
               lon[i] = dtx->CentralLon - RAD2DEG * (col[i]-jc) * dtx->ColIncKm / RADIUS;
            }
         }
         break;
      case PROJ_LAMBERT:
         {
            float xldif, xedif, xrlon, radius;

            for (i=0;i<n;i++) {
               xldif = dtx->Hemisphere * (row[i]-dtx->PoleRow) / dtx->ConeFactor;
               xedif = (dtx->PoleCol-col[i]) / dtx->ConeFactor;
               if (xldif==0.0 && xedif==0.0)
                  xrlon = 0.0;
               else
                  xrlon = atan2( xedif, xldif );
               lon[i] = xrlon / dtx->Cone * RAD2DEG + dtx->CentralLon;
               if (lon[i] > 180.0)
                  lon[i] -= 360.0;

               radius = sqrt( xldif*xldif + xedif*xedif );
               if (radius < 0.0001)
                  lat[i] = 90.0 * dtx->Hemisphere;   /* +/-90 */
               else
                  lat[i] = dtx->Hemisphere
                         * (90.0 - 2.0*atan(exp(log(radius)/dtx->Cone))*RAD2DEG);
            }
         }
         break;
      case PROJ_STEREO:
         {
            float xrow, xcol, rho, c, cc, sc;
            
            for (i=0;i<n;i++) {
               xrow = dtx->CentralRow - row[i] - 1;
               xcol = dtx->CentralCol - col[i] - 1;
               rho = xrow*xrow + xcol*xcol;
               if (rho<1.0e-20) {
                  lat[i] = dtx->CentralLat;
                  lon[i] = dtx->CentralLon;
               }
               else {
                  rho = sqrt( rho );
                  c = 2.0 * atan( rho * dtx->InvScale);
                  cc = cos(c);
                  sc = sin(c);
                  lat[i] = RAD2DEG
                       * asin( cc*dtx->SinCentralLat
                               + xrow*sc*dtx->CosCentralLat / rho );
                  lon[i] = dtx->CentralLon + RAD2DEG * atan2( xcol * sc,
                            (rho * dtx->CosCentralLat * cc
                         - xrow * dtx->SinCentralLat * sc) );
                  if (lon[i] < -180.0)  lon[i] += 360.0;
                  else if (lon[i] > 180.0)  lon[i] -= 360.0;

               }
            }
         }
         break;
      case PROJ_ROTATED:
         {
            float la, lo;

            for (i=0;i<n;i++) {
               lat[i] = dtx->NorthBound - row[i]
                           * (dtx->NorthBound-dtx->SouthBound) / (float) (dtx->Nr-1);
               lon[i] = dtx->WestBound - col[i]
                           * (dtx->WestBound-dtx->EastBound) / (float) (dtx->Nc-1);
               la = lat[i];
               lo = lon[i];
               pandg_back(&la, &lo, dtx->CentralLat, dtx->CentralLon, dtx->Rotation);
               lat[i] = la;
               lon[i] = lo;
            }
         }
         break;
      default:
         printf("Error in grid_to_geo\n");
   }
   for (i=0;i<n;i++) {
      hgt[i] = gridlevelPRIME_to_height( dtx, lev[i]);
   }
}


/**********************************************************************************/
/* 5.0 new stuff                                                                  */
/**********************************************************************************/
/* Convert array or  (row, col, lev) grid coordinates to (lat, lon, hgt) earth    */
/* coordinates.                                                                   */
/* Input: ctx - the vis5d context                                                 */
/*        time, var - which timestep, variable.                                   */
/*        n - number of points                                                    */
/*        row, col, lev - file or grid coordinates                                */
/*                        row = [0, Nr-1]                                         */
/*                        col = [0, Nc-1]                                         */
/*                        lev = [0, Nl-1]                                         */
/* Output: lat, lon, hgt - latitude, longitude, and height. Earth coordinates     */
/**********************************************************************************/
/* Questions- What to do if in PROJ_GENERIC?, do you just not let them load up    */
/*            additional grids if there PROJ_GENERIC, becuase how would they be   */
/*            displayed, maybe do a check later to see if they have the same      */
/*            bounds then you can display mulitple PROJ_GENERIC data sets, but    */
/*            ONLY THEN!                                                          */
/**********************************************************************************/

void grid_to_geo (Context ctx, int time, int var, int n,
                  float row[], float col[], float lev[],
                  float lat[], float lon[], float hgt[])
{
   int i;

   switch (ctx->Projection) {
      case PROJ_GENERIC:
      case PROJ_LINEAR:
      case PROJ_CYLINDRICAL:
      case PROJ_SPHERICAL:
         for (i=0;i<n;i++) {
            lat[i] = ctx->NorthBound - row[i] * ctx->RowInc;
            lon[i] = ctx->WestBound  - col[i] * ctx->ColInc;
         }
         break;
      case PROJ_MERCATOR:
         {
            float YC, alpha, ic, jc;
            YC = RADIUS * log((1.0 + sin(DEG2RAD*ctx->CentralLat))/cos(DEG2RAD*ctx->CentralLat));
            ic = (ctx->Nr-1)/2.0;
            jc = (ctx->Nc-1)/2.0;
            for (i=0;i<n;i++) {
               alpha = ( (ic-row[i]) * ctx->RowIncKm + YC) / RADIUS;
               lat[i] = 2 * RAD2DEG * atan( exp(alpha) ) - 90.0;
               lon[i] = ctx->CentralLon - RAD2DEG * (col[i]-jc) * ctx->ColIncKm / RADIUS;
            }
         }
         break;
      case PROJ_LAMBERT:
         {
            float xldif, xedif, xrlon, radius;
 
            for (i=0;i<n;i++) {
               xldif = ctx->Hemisphere * (row[i]-ctx->PoleRow) / ctx->ConeFactor;
               xedif = (ctx->PoleCol-col[i]) / ctx->ConeFactor;
               if (xldif==0.0 && xedif==0.0)
                  xrlon = 0.0;
               else
                  xrlon = atan2( xedif, xldif );
               lon[i] = xrlon / ctx->Cone * RAD2DEG + ctx->CentralLon;
               if (lon[i] > 180.0)
                  lon[i] -= 360.0;

               radius = sqrt( xldif*xldif + xedif*xedif );
               if (radius < 0.0001)
                  lat[i] = 90.0 * ctx->Hemisphere;   /* +/-90 */
               else
                  lat[i] = ctx->Hemisphere
                         * (90.0 - 2.0*atan(exp(log(radius)/ctx->Cone))*RAD2DEG);
            }
         }
         break;
      case PROJ_STEREO:
         {
            float xrow, xcol, rho, c, cc, sc;
            
            for (i=0;i<n;i++) {
               xrow = ctx->CentralRow - row[i] - 1;
               xcol = ctx->CentralCol - col[i] - 1;
               rho = xrow*xrow + xcol*xcol;
               if (rho<1.0e-20) {
                  lat[i] = ctx->CentralLat;
                  lon[i] = ctx->CentralLon;
               }
               else {
                  rho = sqrt( rho );
                  c = 2.0 * atan( rho * ctx->InvScale);
                  cc = cos(c);
                  sc = sin(c);
                  lat[i] = RAD2DEG
                       * asin( cc*ctx->SinCentralLat
                               + xrow*sc*ctx->CosCentralLat / rho );
                  lon[i] = ctx->CentralLon + RAD2DEG * atan2( xcol * sc,
                            (rho * ctx->CosCentralLat * cc
                         - xrow * ctx->SinCentralLat * sc) );
                  if (lon[i] < -180.0)  lon[i] += 360.0;
                  else if (lon[i] > 180.0)  lon[i] -= 360.0; 

               }
            }
         }
         break;
      case PROJ_ROTATED:
         {
            float la, lo;

            for (i=0;i<n;i++) {
               lat[i] = ctx->NorthBound - row[i]
                           * (ctx->NorthBound-ctx->SouthBound) / (float) (ctx->Nr-1);
               lon[i] = ctx->WestBound - col[i]
                           * (ctx->WestBound-ctx->EastBound) / (float) (ctx->Nc-1);
               la = lat[i];
               lo = lon[i];
               pandg_back(&la, &lo, ctx->CentralLat, ctx->CentralLon, ctx->Rotation);
               lat[i] = la;
               lon[i] = lo;
            }
         }
         break;
      default:
         printf("Error in grid_to_geo\n");
   }
   for (i=0;i<n;i++) {
      hgt[i] = gridlevel_to_height( ctx, lev[i]);
   }
}


void xyz_to_geo( Context ctx, int time, int var,
                 float x, float y, float z,
                 float *lat, float *lon, float *hgt )
{
   switch (ctx->Projection) {
      case PROJ_GENERIC:
      case PROJ_LINEAR:
         *lon = ctx->WestBound - (x-ctx->dpy_ctx->Xmin)
              * (ctx->WestBound-ctx->EastBound) / (ctx->dpy_ctx->Xmax-ctx->dpy_ctx->Xmin);
         *lat = ctx->SouthBound + (y-ctx->dpy_ctx->Ymin)
              * (ctx->NorthBound-ctx->SouthBound) / (ctx->dpy_ctx->Ymax-ctx->dpy_ctx->Ymin);
         *hgt = z_to_height( ctx, z );
         break;
      case PROJ_MERCATOR:
         {       
            float col, row;
            float YC, alpha, ic, jc;
            YC = RADIUS * log((1.0 + sin(DEG2RAD*ctx->CentralLat))/cos(DEG2RAD*ctx->CentralLat));
            ic = (ctx->Nr-1)/2.0;
            jc = (ctx->Nc-1)/2.0;
            /* convert x,y to row,col */
            col = (x-ctx->dpy_ctx->Xmin) / (ctx->dpy_ctx->Xmax-ctx->dpy_ctx->Xmin)
                  * (float) (ctx->Nc-1);
            row = (ctx->dpy_ctx->Ymax-y) / (ctx->dpy_ctx->Ymax-ctx->dpy_ctx->Ymin) 
                  * (float) (ctx->Nr-1);
            alpha = ( (ic-row) * ctx->RowIncKm + YC) / RADIUS;
            *lat = 2 * RAD2DEG * atan( exp(alpha) ) - 90.0;
            *lon = ctx->CentralLon - RAD2DEG * (col-jc) * ctx->ColIncKm / RADIUS;
         }
         break;  
      case PROJ_LAMBERT:
         {
            float col, row, xldif, xedif, xrlon, radius;

            /* convert x,y to row,col */
            col = (x-ctx->dpy_ctx->Xmin) / (ctx->dpy_ctx->Xmax-ctx->dpy_ctx->Xmin)
                  * (float) (ctx->Nc-1);
            row = (ctx->dpy_ctx->Ymax-y) / (ctx->dpy_ctx->Ymax-ctx->dpy_ctx->Ymin) 
                  * (float) (ctx->Nr-1);
            /* convert row,col to lat,lon */
            xldif = ctx->Hemisphere * (row-ctx->PoleRow) / ctx->ConeFactor;
            xedif = (ctx->PoleCol-col) / ctx->ConeFactor;
            if (xldif==0.0 && xedif==0.0)
               xrlon = 0.0;
            else
               xrlon = atan2( xedif, xldif );
            *lon = xrlon / ctx->Cone * RAD2DEG + ctx->CentralLon;
#ifdef LEAVEOUT
            /* check if lon is in the undefined wedge-shaped region */
            if (ctx->Lat1==ctx->Lat2) {
               while (*lon > 180.0) {
                  *lon -= 360.0;
               }
               while (*lon <-180.0) {
                  *lon += 360.0;
               }
            }
            else {
               if (*lon > 180.0) {
                  *lon = 180.0;
               }
               if (*lon < -180.0) {
                  *lon = -180.0;
               }
            }
#endif

            radius = sqrt( xldif*xldif + xedif*xedif );
            if (radius < 0.0001)
               *lat = 90.0 * ctx->Hemisphere;   /* +/-90 */
            else
               *lat = ctx->Hemisphere
                      * (90.0 - 2.0*atan(exp(log(radius)/ctx->Cone))*RAD2DEG);

            *hgt = z_to_height( ctx, z );
         }
         break;
      case PROJ_STEREO:
         {
            float row, col, xrow, xcol, rho, c, cc, sc;
            /* convert x,y to row,col */
            col = (x-ctx->dpy_ctx->Xmin) / (ctx->dpy_ctx->Xmax-ctx->dpy_ctx->Xmin) *
                 (float) (ctx->Nc-1);
            row = (ctx->dpy_ctx->Ymax-y) / (ctx->dpy_ctx->Ymax-ctx->dpy_ctx->Ymin) *
                 (float) (ctx->Nr-1);
            /* convert row,col to lat,lon */
            xrow = ctx->CentralRow - row - 1;
            xcol = ctx->CentralCol - col - 1;
            rho = xrow*xrow + xcol*xcol;
            if (rho<1.0e-5) {
               *lat = ctx->CentralLat;
               *lon = ctx->CentralLon;
            }
            else {
               rho = sqrt( rho );
               c = 2.0 * atan( rho * ctx->InvScale);
               cc = cos(c);
               sc = sin(c);
               *lat = RAD2DEG
                   * asin( cc*ctx->SinCentralLat + xrow*sc*ctx->CosCentralLat / rho );
               *lon = ctx->CentralLon + RAD2DEG * atan2( xcol * sc,
                         (rho * ctx->CosCentralLat * cc - xrow * ctx->SinCentralLat * sc) );
               if (*lon < -180.0) {
                  *lon += 360.0;
               }
               else if (*lon > 180.0) {
                  *lon -= 360.0;
               }
            }
            *hgt = z_to_height( ctx, z );
         }
         break;
      case PROJ_ROTATED:
         *lon = ctx->WestBound - (x-ctx->dpy_ctx->Xmin)
              * (ctx->WestBound-ctx->EastBound) / (ctx->dpy_ctx->Xmax-ctx->dpy_ctx->Xmin);
         *lat = ctx->SouthBound + (y-ctx->dpy_ctx->Ymin)
              * (ctx->NorthBound-ctx->SouthBound) / (ctx->dpy_ctx->Ymax-ctx->dpy_ctx->Ymin);
         *hgt = z_to_height( ctx, z );
         pandg_back(lat, lon, ctx->CentralLat, ctx->CentralLon, ctx->Rotation);
         break;
      case PROJ_CYLINDRICAL:
         {
            float r;
            r = sqrt( x*x + y*y );
            if (r<0.001) {
               /* pole */
               *lat = REVERSE_POLES*90.0;
               *lon = 0.0;
            }
            else {
               *lat = REVERSE_POLES*(90.0 - r / ctx->CylinderScale);
               *lon = REVERSE_POLES*atan2( -y, x ) * RAD2DEG;
               if (ctx->WestBound>180.0)
                  while (*lon<ctx->EastBound)  *lon += 360.0;
               if (ctx->EastBound<-180.0)
                  while (*lon>ctx->WestBound)  *lon -= 360.0;
            }
            *hgt = z_to_height( ctx, z );
         }
         break;
      case PROJ_SPHERICAL:
         {
            float r;
            r = sqrt( x*x + y*y + z*z );
            if (r<0.001) {
               /* degenerate case */
               *lat = 0.0;
               *lon = 0.0;
               *hgt = 0.0;
            }
            else {
               *lon = atan2( -y, x ) * RAD2DEG;
               *lat = atan( z / sqrt(x*x+y*y) ) * RAD2DEG;
               /* TODO: will this work with ctx->VertFlag? */
               *hgt = (r-SPHERE_SIZE) / SPHERE_SCALE
                    * (ctx->TopBound-ctx->BottomBound)
                    + ctx->BottomBound;
            }
         }
         break;
      default:
         printf("Error in xyz_to_geo\n");
   }
}

/*
 * Convert an (x,y,z) graphics coordinate to a (lat, lon, hgt) geographic
 * coordinate.
 * Input:  dtx - the displayxvxis5d context
 *         time, var - which timestep, variable.
 *         x, y, z - graphics coordinate
 * Output:  lat, lon, hgt - latitude, longitude, and height
 */
void xyzPRIME_to_geo( Display_Context dtx, int time, int var,
                 float x, float y, float z,
                 float *lat, float *lon, float *hgt )
{
   switch (dtx->Projection) {
      case PROJ_GENERIC:
      case PROJ_LINEAR:
         *lon = dtx->WestBound - (x-dtx->Xmin)
              * (dtx->WestBound-dtx->EastBound) / (dtx->Xmax-dtx->Xmin);
         *lat = dtx->SouthBound + (y-dtx->Ymin)
              * (dtx->NorthBound-dtx->SouthBound) / (dtx->Ymax-dtx->Ymin);
         *hgt = zPRIME_to_heightPRIME( dtx, z );
         break;
      case PROJ_MERCATOR:
         {
            float col, row;
            float YC, alpha, ic, jc;
            YC = RADIUS * log((1.0 + sin(DEG2RAD*dtx->CentralLat))/cos(DEG2RAD*dtx->CentralLat));
            ic = (dtx->Nr-1)/2.0;
            jc = (dtx->Nc-1)/2.0;
            /* convert x,y to row,col */
            col = (x-dtx->Xmin) / (dtx->Xmax-dtx->Xmin)
                  * (float) (dtx->Nc-1);
            row = (dtx->Ymax-y) / (dtx->Ymax-dtx->Ymin)
                  * (float) (dtx->Nr-1);
            alpha = ( (ic-row) * dtx->RowIncKm + YC) / RADIUS;
            *lat = 2 * RAD2DEG * atan( exp(alpha) ) - 90.0;
            *lon = dtx->CentralLon - RAD2DEG * (col-jc) * dtx->ColIncKm / RADIUS;
            *hgt = zPRIME_to_heightPRIME( dtx, z ); /* WLH 26 Jan 2000 */
         }
         break;
      case PROJ_LAMBERT:
         {
            float col, row, xldif, xedif, xrlon, radius;

            /* convert x,y to row,col */
            col = (x-dtx->Xmin) / (dtx->Xmax-dtx->Xmin) * (float) (dtx->Nc-1);
            row = (dtx->Ymax-y) / (dtx->Ymax-dtx->Ymin) * (float) (dtx->Nr-1);
            /* convert row,col to lat,lon */
            xldif = dtx->Hemisphere * (row-dtx->PoleRow) / dtx->ConeFactor;
            xedif = (dtx->PoleCol-col) / dtx->ConeFactor;
            if (xldif==0.0 && xedif==0.0)
               xrlon = 0.0;
            else
               xrlon = atan2( xedif, xldif );
            *lon = xrlon / dtx->Cone * RAD2DEG + dtx->CentralLon;
#ifdef LEAVEOUT
            /* check if lon is in the undefined wedge-shaped region */
            if (dtx->Lat1==dtx->Lat2) {
               while (*lon > 180.0) {
                  *lon -= 360.0;
               }
               while (*lon <-180.0) {
                  *lon += 360.0;
               }
            }
            else {
               if (*lon > 180.0) {
                  *lon = 180.0;
               }
               if (*lon < -180.0) {
                  *lon = -180.0;
               }
            }
#endif

            radius = sqrt( xldif*xldif + xedif*xedif );
            if (radius < 0.0001)
               *lat = 90.0 * dtx->Hemisphere;   /* +/-90 */
            else
               *lat = dtx->Hemisphere
                      * (90.0 - 2.0*atan(exp(log(radius)/dtx->Cone))*RAD2DEG);

            *hgt = zPRIME_to_heightPRIME( dtx, z );
         }
         break;
      case PROJ_STEREO:
         {
            float row, col, xrow, xcol, rho, c, cc, sc;
            /* convert x,y to row,col */
            col = (x-dtx->Xmin) / (dtx->Xmax-dtx->Xmin) * (float) (dtx->Nc-1);
            row = (dtx->Ymax-y) / (dtx->Ymax-dtx->Ymin) * (float) (dtx->Nr-1);
            /* convert row,col to lat,lon */
            xrow = dtx->CentralRow - row - 1;
            xcol = dtx->CentralCol - col - 1;
            rho = xrow*xrow + xcol*xcol;
            if (rho<1.0e-5) {
               *lat = dtx->CentralLat;
               *lon = dtx->CentralLon;
            }
            else {
               rho = sqrt( rho );
               c = 2.0 * atan( rho * dtx->InvScale);
               cc = cos(c);
               sc = sin(c);
               *lat = RAD2DEG
                   * asin( cc*dtx->SinCentralLat + xrow*sc*dtx->CosCentralLat / rho );
               *lon = dtx->CentralLon + RAD2DEG * atan2( xcol * sc,
                         (rho * dtx->CosCentralLat * cc - xrow * dtx->SinCentralLat * sc) );
               if (*lon < -180.0) {
                  *lon += 360.0;
               }
               else if (*lon > 180.0) {
                  *lon -= 360.0;
               }
            }
            *hgt = zPRIME_to_heightPRIME( dtx, z );
         }
         break;
      case PROJ_ROTATED:
         *lon = dtx->WestBound - (x-dtx->Xmin)
              * (dtx->WestBound-dtx->EastBound) / (dtx->Xmax-dtx->Xmin);
         *lat = dtx->SouthBound + (y-dtx->Ymin)
              * (dtx->NorthBound-dtx->SouthBound) / (dtx->Ymax-dtx->Ymin);
         *hgt = zPRIME_to_heightPRIME( dtx, z );
         pandg_back(lat, lon, dtx->CentralLat, dtx->CentralLon, dtx->Rotation);
         break;
      case PROJ_CYLINDRICAL:
         {
            float r;
            r = sqrt( x*x + y*y );
            if (r<0.001) {
               /* pole */
               *lat = REVERSE_POLES*90.0;
               *lon = 0.0;
            }
            else {
               *lat = REVERSE_POLES*(90.0 - r / dtx->CylinderScale);
               *lon = REVERSE_POLES*atan2( -y, x ) * RAD2DEG;
               if (dtx->WestBound>180.0)
                  while (*lon<dtx->EastBound)  *lon += 360.0;
               if (dtx->EastBound<-180.0)
                  while (*lon>dtx->WestBound)  *lon -= 360.0;
            }
            *hgt = zPRIME_to_heightPRIME( dtx, z );
         }
         break;
      case PROJ_SPHERICAL:
         {
            float r;
            r = sqrt( x*x + y*y + z*z );
            if (r<0.001) {
               /* degenerate case */
               *lat = 0.0;
               *lon = 0.0;
               *hgt = 0.0;
            }
            else {
               *lon = atan2( -y, x ) * RAD2DEG;
               *lat = atan( z / sqrt(x*x+y*y) ) * RAD2DEG;
               /* TODO: will this work with dtx->VertFlag? */
               *hgt = (r-SPHERE_SIZE) / SPHERE_SCALE
                    * (dtx->TopBound-dtx->BottomBound)
                    + dtx->BottomBound;
            }
         }
         break;
      default:
         printf("Error in xyz_to_geo\n");
   }
}



/*
 * Use the current projection type to transform an array of normal
 * vectors.  Then compress the normals to signed bytes.
 * Input:  ctx - the vis5d context
 *         n - number of normals in the array
 *         vr, vc, vl - the grid coordinates corresponding to
 *                      each normal vector.
 *         nx, ny, nz - array of normal vectors to transform.
 *         cnorms - array to put transformed, compressed normals into.
 * Output:  nx, ny, nz - transformed normals.
 */
void project_normals( Context ctx, int n, float vr[], float vc[], float vl[],
                      float nx[], float ny[], float nz[], int_1 cnorms[][3] )
{
   int i;
   float deltalon, deltalat;

   switch (ctx->Projection) {
      case PROJ_GENERIC:
      case PROJ_LINEAR:
      case PROJ_LAMBERT:
      case PROJ_STEREO:
      case PROJ_ROTATED:
      case PROJ_MERCATOR:
         /* don't need vr, vc, vl location information, just compress */
         for (i=0;i<n;i++) {   /* fully vectorized */
            cnorms[i][0] = (int_1) (-nx[i] * NORMAL_SCALE);
            cnorms[i][1] = (int_1) ( ny[i] * NORMAL_SCALE);
            cnorms[i][2] = (int_1) (-nz[i] * NORMAL_SCALE);
         }
         break;
      case PROJ_CYLINDRICAL:
         /* Rotate the x and y components of the normal by an angle */
         /* theta which is computed from the normal's column position. */

         deltalon = (ctx->WestBound-ctx->EastBound) / (float) (ctx->Nc-1);
         for (i=0;i<n;i++) {
            float theta, longitude, nnx, nny;

            longitude = ctx->WestBound-vc[i]*deltalon;
            theta = (REVERSE_POLES*90.0-longitude) * DEG2RAD;
            nnx = -nx[i] * cos(theta) - ny[i] * sin(theta);
            nny = -nx[i] * sin(theta) + ny[i] * cos(theta);
            cnorms[i][0] = (int_1) (nnx * NORMAL_SCALE);
            cnorms[i][1] = (int_1) (nny * NORMAL_SCALE);
            cnorms[i][2] = (int_1) (-nz[i] * NORMAL_SCALE);
         }
         break;
      case PROJ_SPHERICAL:
         deltalon = (ctx->WestBound-ctx->EastBound) / (float) (ctx->Nc-1);
         deltalat = (ctx->NorthBound-ctx->SouthBound) / (float) (ctx->Nr-1);
/*
         printf("enter sx, sy, sz\n");
         scanf("%f %f %f", &sx, &sy, &sz );
*/
         for (i=0;i<n;i++) {
            float rho, theta, longitude, latitude;
            float nx2, ny2, nz2, nx3, ny3, nz3, nx4, ny4, nz4;
            longitude = ctx->WestBound-vc[i]*deltalon;
            latitude = ctx->NorthBound-vr[i]*deltalat;

            /* flip axes */
            nx2 = -nz[i];
            ny2 =  nx[i];
            nz2 = -ny[i];
/*
            nx2 = sx*nz[i];
            ny2 = sy*nx[i];
            nz2 = sz*ny[i];
*/

            /* rotate about Y axis by latitude */
            rho = -latitude * DEG2RAD;
            nx3 = nx2 * cos(rho) - nz2 * sin(rho);
            ny3 = ny2;
            nz3 = nx2 * sin(rho) + nz2 * cos(rho);

            /* rotate about Z axis by longitude */
            theta = -longitude * DEG2RAD;
            nx4 = nx3 * cos(theta) - ny3 * sin(theta);
            ny4 = nx3 * sin(theta) + ny3 * cos(theta);
            nz4 = -nz3;

            cnorms[i][0] = (int_1) (nx4 * NORMAL_SCALE);
            cnorms[i][1] = (int_1) (ny4 * NORMAL_SCALE);
            cnorms[i][2] = (int_1) (nz4 * NORMAL_SCALE);
         }
         break;

      default:
         printf("Error in project_normals\n");
   }

}

void project_normalsPRIME( Display_Context dtx,  int n, float vr[], float vc[], float vl[],
                      float nx[], float ny[], float nz[], int_1 cnorms[][3] )
{
   int i;
   float deltalon, deltalat;

   switch (dtx->Projection) {
      case PROJ_GENERIC:
      case PROJ_LINEAR:
      case PROJ_LAMBERT:
      case PROJ_STEREO:
      case PROJ_ROTATED:
      case PROJ_MERCATOR:
         /* don't need vr, vc, vl location information, just compress */
         for (i=0;i<n;i++) {   /* fully vectorized */
            cnorms[i][0] = (int_1) (-nx[i] * NORMAL_SCALE);
            cnorms[i][1] = (int_1) ( ny[i] * NORMAL_SCALE);
            cnorms[i][2] = (int_1) (-nz[i] * NORMAL_SCALE);
         }
         break;
      case PROJ_CYLINDRICAL:
         /* Rotate the x and y components of the normal by an angle */
         /* theta which is computed from the normal's column position. */

         deltalon = (dtx->WestBound-dtx->EastBound) / (float) (dtx->Nc-1);
         for (i=0;i<n;i++) {
            float theta, longitude, nnx, nny;

            longitude = dtx->WestBound-vc[i]*deltalon;
            theta = (REVERSE_POLES*90.0-longitude) * DEG2RAD;
            nnx = -nx[i] * cos(theta) - ny[i] * sin(theta);
            nny = -nx[i] * sin(theta) + ny[i] * cos(theta);
            cnorms[i][0] = (int_1) (nnx * NORMAL_SCALE);
            cnorms[i][1] = (int_1) (nny * NORMAL_SCALE);
            cnorms[i][2] = (int_1) (-nz[i] * NORMAL_SCALE);
         }
         break;
      case PROJ_SPHERICAL:
         deltalon = (dtx->WestBound-dtx->EastBound) / (float) (dtx->Nc-1);
         deltalat = (dtx->NorthBound-dtx->SouthBound) / (float) (dtx->Nr-1);
/*
         printf("enter sx, sy, sz\n");
         scanf("%f %f %f", &sx, &sy, &sz );
*/
         for (i=0;i<n;i++) {
            float rho, theta, longitude, latitude;
            float nx2, ny2, nz2, nx3, ny3, nz3, nx4, ny4, nz4;
            longitude = dtx->WestBound-vc[i]*deltalon;
            latitude = dtx->NorthBound-vr[i]*deltalat;

            /* flip axes */
            nx2 = -nz[i];
            ny2 =  nx[i];
            nz2 = -ny[i];
/*
            nx2 = sx*nz[i];
            ny2 = sy*nx[i];
            nz2 = sz*ny[i];
*/

            /* rotate about Y axis by latitude */
            rho = -latitude * DEG2RAD;
            nx3 = nx2 * cos(rho) - nz2 * sin(rho);
            ny3 = ny2;
            nz3 = nx2 * sin(rho) + nz2 * cos(rho);

            /* rotate about Z axis by longitude */
            theta = -longitude * DEG2RAD;
            nx4 = nx3 * cos(theta) - ny3 * sin(theta);
            ny4 = nx3 * sin(theta) + ny3 * cos(theta);
            nz4 = -nz3;

            cnorms[i][0] = (int_1) (nx4 * NORMAL_SCALE);
            cnorms[i][1] = (int_1) (ny4 * NORMAL_SCALE);
            cnorms[i][2] = (int_1) (nz4 * NORMAL_SCALE);
         }
         break;

      default:
         printf("Error in project_normals\n");
   }

}


/**********************************************************/
/***** (row', col', lev')  (row, col, lev) conversion *****/
/**********************************************************/

void gridPRIME_to_grid( Context ctx, int time, int var, int n,
                        float rPRIME[], float cPRIME[], float lPRIME[],
                        float r[], float c[], float l[] )
{
   float lat[MAX_CONV_VERTS], lon[MAX_CONV_VERTS], hgt[MAX_CONV_VERTS];

   gridPRIME_to_geo(ctx->dpy_ctx, time, var, n, rPRIME, cPRIME, lPRIME,
                    lat, lon, hgt);
   geo_to_grid( ctx, time, var, n, lat, lon, hgt, r, c, l);
}

void grid_to_gridPRIME( Context ctx, int time, int var, int n,
                        float r[], float c[], float l[],
                        float rPRIME[], float cPRIME[], float lPRIME[])
{
   float lat[MAX_CONV_VERTS], lon[MAX_CONV_VERTS], hgt[MAX_CONV_VERTS];

   grid_to_geo(ctx, time, var, n, r, c, l, lat, lon, hgt);
   geo_to_gridPRIME( ctx->dpy_ctx, time, var, n, lat, lon, hgt,
                     rPRIME, cPRIME, lPRIME);
}


/**********************************************************************/
/*****                      Miscellaneous                         *****/
/**********************************************************************/


#define EARTH_RADIUS 6371230.0


/*
 * Compute the distance (in meters) between two points on the earth surface.
 * Input:  lat1, lon1 - first location
 *         lat2, lon2 - second location
 * Return:  distance in meters
 */
float earth_distance( float lat1, float lon1, float lat2, float lon2 )
{
   float xd, yd, zd, d;

   lat1 *= DEG2RAD;
   lon1 *= DEG2RAD;
   lat2 *= DEG2RAD;
   lon2 *= DEG2RAD;

   xd = EARTH_RADIUS * ( cos(lat2)*cos(lon2) - cos(lat1)*cos(lon1) );
   yd = EARTH_RADIUS * ( cos(lat2)*sin(lon2) - cos(lat1)*sin(lon1) );
   zd = EARTH_RADIUS * ( sin(lat2) - sin(lat1) );

   d = sqrt( xd*xd + yd*yd + zd*zd );
   if (d/(2.0*EARTH_RADIUS) < 0.001) {
      /* d << 2R */
      return d;
   }
   else {
      /* return arc length */
      return 2.0 * EARTH_RADIUS * asin( d / (2.0*EARTH_RADIUS) );
   }
}


/*
 * Compute rough lat lon bounds
 * Input:  dtx - the vis5d display context
 * Output: lats, latn, lonw, lone - bounds
 */
void latlon_bounds( Display_Context dtx,
                    float *lats, float *latn, float *lonw, float *lone )
{
  float t, n;

  rowcolPRIME_to_latlon( dtx, 0, 0, 0.0, 0.0, &t, &n );
  *latn = *lats = t;
  *lonw = *lone = n;
  rowcolPRIME_to_latlon( dtx, 0, 0, (float) dtx->Nr - 1.0, 0.0, &t, &n );
  if (t > *latn) *latn = t;
  if (t < *lats) *lats = t;
  if (n > *lonw) *lonw = n;
  if (n < *lone) *lone = n;
  rowcolPRIME_to_latlon( dtx, 0, 0, 0.0, (float) dtx->Nc - 1.0, &t, &n );
  if (t > *latn) *latn = t;
  if (t < *lats) *lats = t;
  if (n > *lonw) *lonw = n;
  if (n < *lone) *lone = n;
  rowcolPRIME_to_latlon( dtx, 0, 0, (float) dtx->Nr - 1.0, (float) dtx->Nc - 1.0, &t, &n );
  if (t > *latn) *latn = t;
  if (t < *lats) *lats = t;
  if (n > *lonw) *lonw = n;
  if (n < *lone) *lone = n;
  return;
}

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
void pandg_back( float *lat, float *lon, float a, float b, float r )
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

  return;
}

/* Earth to Pete rotated sphere */
void pandg_for( float *lat, float *lon, float a, float b, float r )
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

  return;
}

