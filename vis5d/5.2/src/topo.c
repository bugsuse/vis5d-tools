/*  topo.c */

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
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "binio.h"
#include "globals.h"
#include "graphics.h"
#include "image.h"
#include "memory.h"
#include "proj.h"
#include "topo.h"
#include "user_data.h"



/* maximum rows, columns of topography vertices for high and low res. topo */
#define LO_RES_VERTS 5000
#define HI_RES_VERTS 50000





/**********************************************************************/
/***                     Topography file stuff                      ***/
/**********************************************************************/

/*
 * This struct directly maps to the first part of the topo file.
 * NO LONGER USED, just kept around for reference.
 */
struct topo_header {
   char id[40];        /* id string "TOPO2" */
   float westlon;      /* West longitude in degrees */
   float eastlon;      /* East longitude in degrees */
   float northlat;     /* North latitude in degrees */
   float southlat;     /* South latitude in degrees */
   int rows;           /* number of rows */
   int cols;           /* number of columns */
   /* Next is the topo data in the form:  float data[rows][cols] */
};

/* MJK 12.02.98 begin */
int check_face_norm (Display_Context dtx, int_2 *verts)
{
    int         i, j;
    float       xyz[3], xy[3][2], area;

    for (i = 0; i < 3; i++, verts += 3)
    {
        for (j = 0; j < 3; j++) xyz[j] = verts[j] / VERTEX_SCALE;

        project (xyz, &xy[i][0], &xy[i][1]);
    }

    area = 0.0;
    j    = 2;
    for (i = 0; i < 3; i++)
    {
        area += (xy[i][0] - xy[j][0]) * (xy[i][1] + xy[j][1]);
        j     = i;
    }

    return (area < 0.0) ? 1 : (area > 0.0) ? -1 : 0;
}

int make_topo_strips (Display_Context dtx)
{

    int         i, j, n, ir, ic, nr, nc;
    int_2       *verts;
    int_1       *norms;


    nr = dtx->qrows;
    nc = dtx->qcols;

    n = ((nr * nc * 2) * 2) + ((nc * 2) * 2) + ((nr * 2) * 2);

    dtx->TopoStripsVerts = realloc (dtx->TopoStripsVerts,
                                    (n * 3 * sizeof (int_2)));
    dtx->TopoStripsNorms = realloc (dtx->TopoStripsNorms,
                                    (n * 3 * sizeof (int_1)));
    if ((dtx->TopoStripsVerts == NULL) || (dtx->TopoStripsNorms == NULL))
    {
        if (dtx->TopoStripsVerts == NULL)
            free (dtx->TopoStripsVerts), dtx->TopoStripsVerts = NULL;
        if (dtx->TopoStripsNorms == NULL)
            free (dtx->TopoStripsNorms), dtx->TopoStripsNorms = NULL;

        return 0;
    }

    verts = dtx->TopoStripsVerts;
    norms = dtx->TopoStripsNorms;


    j = 0;
    i = nc;
    for (ir = 1; ir < nr; ir++)
    {
        for (ic = 0; ic < nc; ic++, i++, j++)
        {
            verts[0] = dtx->TopoVertex[i*3+0] * VERTEX_SCALE;
            verts[1] = dtx->TopoVertex[i*3+1] * VERTEX_SCALE;
            verts[2] = dtx->TopoVertex[i*3+2] * VERTEX_SCALE;
            norms[0] = dtx->TopoNormal[i*3+0] * NORMAL_SCALE;
            norms[1] = dtx->TopoNormal[i*3+1] * NORMAL_SCALE;
            norms[2] = dtx->TopoNormal[i*3+2] * NORMAL_SCALE;
            verts += 3, norms += 3;
            verts[0] = dtx->TopoVertex[j*3+0] * VERTEX_SCALE;
            verts[1] = dtx->TopoVertex[j*3+1] * VERTEX_SCALE;
            verts[2] = dtx->TopoVertex[j*3+2] * VERTEX_SCALE;
            norms[0] = dtx->TopoNormal[j*3+0] * NORMAL_SCALE;
            norms[1] = dtx->TopoNormal[j*3+1] * NORMAL_SCALE;
            norms[2] = dtx->TopoNormal[j*3+2] * NORMAL_SCALE;
            verts += 3, norms += 3;
        }
    }



    if (dtx->DisplayTopoBase)
    {
        float           z;
        int_2           base_z;
        int_1           norm_0 = 0.0 * NORMAL_SCALE;
        int_1           norm_1 = 1.0 * NORMAL_SCALE;


        if (dtx->TopoBaseLev <= 0.0)
        {
            z = dtx->Zmin -
                (gridlevelPRIME_to_zPRIME (dtx, -1, -1, -dtx->TopoBaseLev) -
                dtx->Zmin);
        }
        else
        {
            z = gridlevelPRIME_to_zPRIME (dtx, -1, -1, dtx->TopoBaseLev);

            norm_1 = -norm_1;
        }
        /* clamp z to keep from overflowing int_2 base_z */
        z = (z < -3.0) ? -3.0 : (z > 3.0) ? 3.0 : z;

        base_z = z * VERTEX_SCALE;


        /* north side */

        i = 0;
        for (ic = 0; ic < nc; ic++, i++)
        {
            verts[0] = dtx->TopoVertex[i*3+0] * VERTEX_SCALE;
            verts[1] = dtx->TopoVertex[i*3+1] * VERTEX_SCALE;
            verts[2] = dtx->TopoVertex[i*3+2] * VERTEX_SCALE;
            norms[0] = norm_0;
            norms[1] = norm_1;
            norms[2] = norm_0;
            verts += 3, norms += 3;
            verts[0] = dtx->TopoVertex[i*3+0] * VERTEX_SCALE;
            verts[1] = dtx->TopoVertex[i*3+1] * VERTEX_SCALE;
            verts[2] = base_z;
            norms[0] = norm_0;
            norms[1] = norm_1;
            norms[2] = norm_0;
            verts += 3, norms += 3;
        }

        /* south side */

        i = (nr * nc) - 1;
        for (ic = 0; ic < nc; ic++, i--)
        {
            verts[0] = dtx->TopoVertex[i*3+0] * VERTEX_SCALE;
            verts[1] = dtx->TopoVertex[i*3+1] * VERTEX_SCALE;
            verts[2] = dtx->TopoVertex[i*3+2] * VERTEX_SCALE;
            norms[0] = norm_0;
            norms[1] = -norm_1;
            norms[2] = norm_0;
            verts += 3, norms += 3;
            verts[0] = dtx->TopoVertex[i*3+0] * VERTEX_SCALE;
            verts[1] = dtx->TopoVertex[i*3+1] * VERTEX_SCALE;
            verts[2] = base_z;
            norms[0] = norm_0;
            norms[1] = -norm_1;
            norms[2] = norm_0;
            verts += 3, norms += 3;
        }

        /* west side */

        i = (nr - 1) * nc;
        for (ir = 0; ir < nr; ir++, i -= nc)
        {
            verts[0] = dtx->TopoVertex[i*3+0] * VERTEX_SCALE;
            verts[1] = dtx->TopoVertex[i*3+1] * VERTEX_SCALE;
            verts[2] = dtx->TopoVertex[i*3+2] * VERTEX_SCALE;
            norms[0] = -norm_1;
            norms[1] = norm_0;
            norms[2] = norm_0;
            verts += 3, norms += 3;
            verts[0] = dtx->TopoVertex[i*3+0] * VERTEX_SCALE;
            verts[1] = dtx->TopoVertex[i*3+1] * VERTEX_SCALE;
            verts[2] = base_z;
            norms[0] = -norm_1;
            norms[1] = norm_0;
            norms[2] = norm_0;
            verts += 3, norms += 3;
        }

        /* east side */

        i = nc - 1;
        for (ir = 0; ir < nr; ir++, i += nc)
        {
            verts[0] = dtx->TopoVertex[i*3+0] * VERTEX_SCALE;
            verts[1] = dtx->TopoVertex[i*3+1] * VERTEX_SCALE;
            verts[2] = dtx->TopoVertex[i*3+2] * VERTEX_SCALE;
            norms[0] = norm_1;
            norms[1] = norm_0;
            norms[2] = norm_0;
            verts += 3, norms += 3;
            verts[0] = dtx->TopoVertex[i*3+0] * VERTEX_SCALE;
            verts[1] = dtx->TopoVertex[i*3+1] * VERTEX_SCALE;
            verts[2] = base_z;
            norms[0] = norm_1;
            norms[1] = norm_0;
            norms[2] = norm_0;
            verts += 3, norms += 3;
        }

        /* bottom */

        i = (nr * nc) - 1;
        j = i - nc;
        for (ir = 1; ir < nr; ir++)
        {
            for (ic = 0; ic < nc; ic++, i--, j--)
            {
                verts[0] = dtx->TopoVertex[i*3+0] * VERTEX_SCALE;
                verts[1] = dtx->TopoVertex[i*3+1] * VERTEX_SCALE;
                verts[2] = base_z;
                norms[0] = norm_0;
                norms[1] = norm_0;
                norms[2] = -norm_1;
                verts += 3, norms += 3;
                verts[0] = dtx->TopoVertex[j*3+0] * VERTEX_SCALE;
                verts[1] = dtx->TopoVertex[j*3+1] * VERTEX_SCALE;
                verts[2] = base_z;
                norms[0] = norm_0;
                norms[1] = norm_0;
                norms[2] = -norm_1;
                verts += 3, norms += 3;
            }
        }
    }
    return 1;
}
/* MJK 12.02.98 end */






/* MJK 12.02.98 begin */
static int read_user_topo( Display_Context dtx, char *toponame )
{
   int iret = 0;

/*
 *  Call the user's function to get the user's topo data.
 *  It is not necessary for this function to actually read a file --
 *  it only needs to put the user's topo data into the Vis5D
 *  data structure.
 *
 *  An example for this function can be found in user_data.c.
 */

   free_topo (dtx);

   iret = user_data_get_topo (dtx, toponame);


   return iret;
}
/* MJK 12.02.98 end */


/*
 * Read a topography file and initialize Topo and TopoData.
 * Input:  filename - name of topo file.
 * Return:  0 if error, otherwise non-zero for success.
 */
static int read_topo( Display_Context dtx, char *filename )
{
   int f;
   int n;
   char id[40];

   f = open( filename, O_RDONLY );
   if (f<0) {
      printf("Topo file %s not found\n", filename );
      return 0;
   }

   /* Read topo file header */
   read_bytes( f, id, 40 );
   read_float4( f, &dtx->Topo_westlon );
   read_float4( f, &dtx->Topo_eastlon );
   read_float4( f, &dtx->Topo_northlat );
   read_float4( f, &dtx->Topo_southlat );
   read_int4( f, &dtx->Topo_rows );
   read_int4( f, &dtx->Topo_cols );

   if (strcmp(id,"TOPO")==0) {
      /* OLD STYLE: bounds given as ints, convert to floats */
      int *p;
      p = (int *) &dtx->Topo_westlon;  dtx->Topo_westlon = (float) *p / 100.0;
      p = (int *) &dtx->Topo_eastlon;  dtx->Topo_eastlon = (float) *p / 100.0;
      p = (int *) &dtx->Topo_northlat; dtx->Topo_northlat = (float) *p / 100.0;
      p = (int *) &dtx->Topo_southlat; dtx->Topo_southlat = (float) *p / 100.0;
   }
   else if (strcmp(id,"TOPO2")==0) {
      /* OK */
   }
   else {
      printf("%s is not a TOPO file\n", filename);
      close(f);
      return 0;
   }

   dtx->TopoData = (short *) malloc(dtx->Topo_rows * dtx->Topo_cols * sizeof(short));

   /* dtx->TopoData = (short *) allocate( dtx, dtx->Topo_rows * dtx->Topo_cols
                                       * sizeof(short) ); */
   if (!dtx->TopoData) {
      close(f);
      return 0;
   }


   n = dtx->Topo_rows * dtx->Topo_cols;
   if (read_int2_array( f, dtx->TopoData, n) < n) {
      free( dtx->TopoData);
      /* deallocate( dtx, dtx->TopoData, n*sizeof(short) ); */
      dtx->TopoData = NULL;
      close(f);
      return 0;
   }

   close(f);
   return 1;
}



/*
 * Free the memory used to store the topography data
 */
void free_topo( dtx )
Display_Context dtx;
{
   if (dtx->TopoData) {
      free( dtx->TopoData);
      /* deallocate( dtx, dtx->TopoData, dtx->Topo_rows*dtx->Topo_cols*sizeof(short) ); */
      dtx->TopoData = NULL;
   }
}




/*
 * Return the elevation of the topography at location (lat,lon) and a
 * flag indicating water or land.
 * Input:  lat, lon - location in degrees
 *         water - pointer to integer
 * Output:  water - set to 1 if water, 0 if land.
 * Returned:  elevation in meters at (lat,lon) or 0 if error.
 */
float elevation( Display_Context dtx, float lat, float lon, int *water )
{
   float fr, fc;
   int rowa, cola, rowb, colb;
   float hgt;
   int count, r, c;
   int val, watcount;

   /* MJK 12.02.98 begin */
   if ((dtx->Topo_cols == dtx->Nc) && (dtx->Topo_rows == dtx->Nr))
   {
       float    x, y, z, hgt;

       if (!dtx->TopoData)
       {
           if (water) *water = 0.0;
           return 0.0;
       }

       hgt = 0.0;
/* MJK 2.17.99
       geo_to_xyzPRIME (dtx, -1, -1, 1, &lat, &lon, &hgt, &x, &y, &z);
*/
       geo_to_xyzTOPO (dtx, -1, -1, 1, &lat, &lon, &hgt, &x, &y, &z);
       xyzPRIME_to_gridPRIME (dtx, -1, -1, x, y, 0.0, &fr, &fc, &hgt);
   }
   else{
      /* make sure longitude is in [-180,180] */
      if (lon>dtx->Topo_westlon) {
         lon -= 360.0;
      }
      if (lon<dtx->Topo_eastlon) {
         lon += 360.0;
      }

      while (lat<-90.0) {
         lat += 180.0;
      }
      while (lat>90.0) {
         lat -= 180.0;
      }

      if (!dtx->TopoData || lon<dtx->Topo_eastlon || lon>dtx->Topo_westlon
          || lat<dtx->Topo_southlat || lat>dtx->Topo_northlat) {
         if (water)
            *water = 0;
         return 0.0;
      }

      fr = (dtx->Topo_rows - 1) * (lat - dtx->Topo_northlat)
              / (dtx->Topo_southlat - dtx->Topo_northlat);
      fc = (dtx->Topo_cols - 1) * (lon - dtx->Topo_westlon)
              / (dtx->Topo_eastlon - dtx->Topo_westlon);
   }
   /* MJK 12.02.98 end */


   /* Return elevation at (lat,lon) by sampling LatSample*LonSample */
   /* values centered at that location. */

   /* calculate range of rows */
   rowa = (int) fr - dtx->LatSample/2;
   rowb = rowa + dtx->LatSample;
   if (rowa<0)
      rowa = 0;
   if (rowb>=dtx->Topo_rows)
      rowb = dtx->Topo_rows - 1;

   /* calculate range of columns */
   cola = (int) fc - dtx->LonSample/2;
   colb = cola + dtx->LonSample;
   if (cola<0)
      cola = 0;
   if (colb>=dtx->Topo_cols)
      colb = dtx->Topo_cols - 1;



   /* MJK 12.15.98 */
#  define       FUZZ            1e-05
    if ((fr - rowa) < FUZZ) rowb = rowa;
    if ((fc - cola) < FUZZ) colb = cola;
#  undef        FUZZ



   /* find average height in sample area */
   hgt = 0.0;
   count = watcount = 0;
   for (r=rowa;r<=rowb;r++) {
      for (c=cola;c<=colb;c++) {
         val = dtx->TopoData[r*dtx->Topo_cols+c];
         if (val&1)
            watcount++;
         hgt += (float) (val / 2);
         count++;
      }
   }
   hgt = hgt / (float) count;

   /* calculate water flag */
   if (water) {
      if (watcount>count/2)
        *water = 1;
      else
        *water = 0;
   }

   return hgt;
}





/**********************************************************************/
/***                   Topography display stuff                     ***/
/**********************************************************************/


#define TRANS(XMAX,XMIN,YMAX,YMIN,XVAL) (YMAX-(YMAX-YMIN)*(XMAX-XVAL)/(XMAX-XMIN))

#define CLAMP(VAL,MIN,MAX)   ( (VAL<MIN) ? MIN : ((VAL>MAX) ? MAX : VAL) )

#define ABS( A )   ( (A) < 0.0 ? -(A) : (A) )





/*
 * Initialize the topography color table.
 * Input:  ct - the color table
 *         size - number of entries in the table
 *         minhgt, maxhgt - the range of height values
 */
void init_topo_color_table( unsigned int ct[], int size,
                            float minhgt, float maxhgt )
{
#define OCEAN
#ifdef OCEAN
   /* Change submitted by Mike McCann to give better under-water */
   /* topography colors. */
   static float red[7]   = { 5.0, 45.0,  20.0, 20.0,  70.0, 165.0, 200.0};
   static float green[7] = {10.0, 50.0, 170.0,170.0, 200.0,  42.0, 200.0};
   static float blue[7]  = {30.0,150.0,  42.0, 42.0,   0.0,  42.0, 200.0};
   static float range[7] = {-5.0, -0.020,-0.015, 0.0,   0.1,  1.0,   2.8};
#else
   static float red[4]   = { 20.0,  70.0, 165.0, 200.0};
   static float green[4] = {170.0, 200.0,  42.0, 200.0};
   static float blue[4]  = { 42.0,   0.0,  42.0, 200.0};
   static float range[4] = {  0.0,   0.1,  1.0,   2.8};
#endif
   int i, j;
   float x0, x1;
   float r, g, b, dr, dg, db;

   /* initialize to all white to start */
   for (i=0;i<size-1;i++) {
      ct[i] = 0xffffffff;
   }
   ct[size-1] = PACK_COLOR( 25, 25, 255, 255 );  /* r=25, g=25, b=255 */


#ifdef OCEAN
   for (i=0;i<6;i++) {
#else
   for (i=0;i<3;i++) {
#endif
      if (minhgt==maxhgt) {
         r = g = b = 0;
         dr = dg = db = 0;
         x0 = x1 = 0;
      }
      else {
         x0 = (range[i] - minhgt)
                / (maxhgt - minhgt) * (float)(size-1);
         x1 = (range[i+1] - minhgt)
                / (maxhgt - minhgt) * (float)(size-1);
         dr = (red[i+1]-red[i]) / (x1-x0);
         dg = (green[i+1]-green[i]) / (x1-x0);
         db = (blue[i+1]-blue[i]) / (x1-x0);
         r = red[i];
         g = green[i];
         b = blue[i];
      }
      for (j=(int) x0; j<(int) x1; j++) {
         if (j>=0 && j<size-1) {
            ct[j] = PACK_COLOR( (int) r, (int) g, (int) b, 0xff );
         }
         r += dr;
         g += dg;
         b += db;
      }
   }
}




/*
 * Generate the topography quadmesh.  This must be called after the
 * grid data set has been loaded.
 * Input:  toponame - name of topography file
 *         textureflag - 1 = use texture mapping, 0 = don't texture map
 *         hi_res - 1=high resolution topography, 0=normal resolution
 * Return:  1 = ok,  0 = error
 */
int init_topo( Display_Context dtx, char *toponame, int textureflag, int hi_res )
{
   double dx, dy;
   float lat, lon;
   float topo_dlat, topo_dlon;
   float *topoheight;
   int i, j;
   int topoflag = -1;
   int qr, qc;
   uint_1 *indexes;



   /* MJK 12.02.98 begin */
   if (dtx->UserTopoFlag) {
      topoflag = read_user_topo( dtx, toponame );
      if (topoflag == 0) return 0;
   }


   if (topoflag == -1) {
      topoflag = read_topo( dtx, toponame );
   }
   /* MJK 12.02.98 end */



   if (!topoflag && !textureflag) {
      return 0;
   }
   
   /* qrows, qcols - size of topography quadmesh in rows and columns */
   if (dtx->Topo_cols==dtx->Nc && dtx->Topo_rows==dtx->Nr) {
      /* use same topography resolution as grid resolution */
      qc = dtx->Topo_cols;
      qr = dtx->Topo_rows;
   }
   else {
      int maxverts = hi_res ? HI_RES_VERTS : LO_RES_VERTS;
      float r = sqrt( (float) maxverts
                      / ((dtx->Xmax - dtx->Xmin)*(dtx->Ymax - dtx->Ymin)) );
      qc = (int) (r * (dtx->Xmax - dtx->Xmin) + 0.5);
      qr = (int) (r * (dtx->Ymax - dtx->Ymin) + 0.5);
   }

   /* allocate space for topography vertex and color arrays */
   if (dtx->TopoVertex){
      free(dtx->TopoVertex);
      free(dtx->TopoNormal);
      free(dtx->TopoTexcoord);
      free(dtx->TopoFlatVertex);
      /* MJK 12.02.98 begin */
      for (i = 0; i <= MAXTIMES; i++)
      {
         if (dtx->TopoIndexes[i] != NULL)
            free (dtx->TopoIndexes[i]), dtx->TopoIndexes[i] = NULL;
      }
      /* MJK 12.02.98 end */


      /*
      free(dtx->TopoIndexes[MAXTIMES]);
      */
   }
   dtx->TopoVertex     = (float *) malloc( qr*qc*3*sizeof(float) );
   dtx->TopoNormal     = (float *) malloc( qr*qc*3*sizeof(float) );
   dtx->TopoTexcoord   = (float *) malloc( qr*qc*2*sizeof(float) );
   dtx->TopoFlatVertex = (float *) malloc( qr*qc*3*sizeof(float) );
   topoheight = (float *) malloc( qr*qc*sizeof(float) );
   /* topoheight = (float *) allocate( dtx, qr*qc*sizeof(float) ); */
   indexes = malloc( qr*qc*1*sizeof(uint_1) );
   dtx->TopoIndexes[MAXTIMES] = indexes;

   /*
    * Compute topography vertices.
    */
   if (dtx->CurvedBox==0) {
      /* Rectangular box:  generate vertices in graphics coords */
      int k;
      float x, y, z;

      /* MJK 12.15.98 */
      double xx, yy, texture_s, texture_t, delta_s, delta_t;

      dx = (dtx->Xmax-dtx->Xmin) / (float) (qc-1);
      dy = (dtx->Ymax-dtx->Ymin) / (float) (qr-1);

      delta_s = 1.0 / (float) (qc-1);
      delta_t = 1.0 / (float) (qr-1);

      /* calculate sampling size */
      if (dtx->Topo_cols==dtx->Nc && dtx->Topo_rows==dtx->Nr) {
         dtx->LatSample = dtx->LonSample = 1;
      }
      else {
         topo_dlat = (dtx->Topo_northlat-dtx->Topo_southlat) / dtx->Topo_rows;
         dtx->LatSample = CLAMP( (int) (2.0*dy/topo_dlat), 2, 20 );
         topo_dlon = (dtx->Topo_westlon-dtx->Topo_eastlon) / dtx->Topo_cols;
         dtx->LonSample = CLAMP( (int) (2.0*dx/topo_dlon), 2, 20 );
      }

      k = 0;
      yy = dtx->Ymax;
      texture_t = 0.0;
      for (i=0; i<qr; i++) {
         xx = dtx->Xmin;
         texture_s = 0.0;
         y = yy;
         for (j=0; j<qc; j++) {
            int water;
            float hgt;

            x = xx;
            xyzPRIME_to_geo( dtx, -1, -1, x, y, 0.0, &lat, &lon, &hgt );
            hgt = elevation( dtx, lat, lon, &water ) / 1000.0;  /* hgt in km */
/* MJK 2.17.99
            z = height_to_zPRIME( dtx, hgt );
*/
            z = height_to_zTOPO( dtx, hgt );

            /* WLH 3 Nov 98 - kludge topo for inverted VERT_GENERIC */
            if (dtx->VerticalSystem == VERT_GENERIC &&
                dtx->TopBound < dtx->BottomBound) {
              z = dtx->Zmin + hgt / (dtx->BottomBound-dtx->TopBound)
                       * (dtx->Zmax-dtx->Zmin);
            }

            z = ABS(dtx->Zmin - z) < 0.01 ? dtx->Zmin+0.01 : z;
            dtx->TopoVertex[k*3+0] = x;
            dtx->TopoVertex[k*3+1] = y;
            dtx->TopoVertex[k*3+2] = z;

            topoheight[k] = hgt;  /* save topo height at this vertex */
            /* if water flag is set, index will be 255 */
            indexes[k] = (water) ? 255 : 0;

            dtx->TopoFlatVertex[k*3+0] = x;
            dtx->TopoFlatVertex[k*3+1] = y;
            dtx->TopoFlatVertex[k*3+2] = dtx->Zmin;

            dtx->TopoTexcoord[k*2+0] = texture_s;
            dtx->TopoTexcoord[k*2+1] = texture_t;

            k++;
            xx += dx;
            texture_s += delta_s;
         }
         yy -= dy;
         texture_t += delta_t;
      }

   }
   else {
      /* Curved box:  generate vertices in geographic coordinates */

      float lat, lon;
      double latlat, lonlon;
      double dlat, dlon;
      int k;
      float texture_s, texture_t, delta_s, delta_t;

      dlat = (dtx->NorthBound - dtx->SouthBound) / (float) (qr-1);
      dlon = (dtx->WestBound - dtx->EastBound) / (float) (qc-1);

      delta_s = 1.0 / (float) (qc-1);
      delta_t = 1.0 / (float) (qr-1);

      k = 0;
      latlat = dtx->NorthBound;
      texture_t = 0.0;
      for (i=0; i<qr; i++) {
         lonlon = dtx->WestBound;
         lat = latlat;
         texture_s = 0.0;
         for (j=0; j<qc; j++) {
            int water;
            float hgt, x, y, z;

            lon = lonlon;
            hgt = elevation( dtx, lat, lon, &water ) / 1000.0;  /* hgt in km */
/* MJK 2.17.99
            geo_to_xyzPRIME( dtx, -1, -1, 1, &lat, &lon, &hgt, &x, &y, &z );
*/
            geo_to_xyzTOPO( dtx, -1, -1, 1, &lat, &lon, &hgt, &x, &y, &z );
            dtx->TopoVertex[k*3+0] = x;
            dtx->TopoVertex[k*3+1] = y;
            dtx->TopoVertex[k*3+2] = z;

            topoheight[k] = hgt;
            /* if water flag is set, index will be 255 */
            indexes[k] = (water) ? 255 : 0;

            hgt = dtx->BottomBound;
/* MJK 2.17.99            
            geo_to_xyzPRIME( dtx, -1, -1, 1, &lat, &lon, &hgt, &x, &y, &z );
*/
            geo_to_xyzTOPO( dtx, -1, -1, 1, &lat, &lon, &hgt, &x, &y, &z );
            dtx->TopoFlatVertex[k*3+0] = x;
            dtx->TopoFlatVertex[k*3+1] = y;
            dtx->TopoFlatVertex[k*3+2] = z;

            dtx->TopoTexcoord[k*2+0] = texture_s;
            dtx->TopoTexcoord[k*2+1] = texture_t;

            k++;
            lonlon -= dlon;
            texture_s += delta_s;
         }
         latlat -= dlat;
         texture_t += delta_t;
      }

   }

   /* Find MinTopoHgt and MaxTopoHgt */
   dtx->MinTopoHgt = 10000.0;
   dtx->MaxTopoHgt = -10000.0;
   for (i=0;i<qr*qc;i++) {
      if (topoheight[i]<dtx->MinTopoHgt) {
         dtx->MinTopoHgt = topoheight[i];
      }
      if (topoheight[i]>dtx->MaxTopoHgt) {
         dtx->MaxTopoHgt = topoheight[i];
      }
   }

   /* Compute topography color table indexes. */
   for (i=0;i<qr*qc;i++) {
      float hgt = topoheight[i];
      if (indexes[i]!=255) {   /* if not water */
         if (dtx->MinTopoHgt==dtx->MaxTopoHgt) {
            indexes[i] = 0;
         }
         else {
            int index;
            index = (int) ( (hgt-dtx->MinTopoHgt)
                           / (dtx->MaxTopoHgt-dtx->MinTopoHgt) * 254.0 );
            indexes[i] = CLAMP( index, 0, 254 );
         }
      }
   }

   /* done with topoheight array */
   free( topoheight );
   /* deallocate( dtx, topoheight, qr*qc*sizeof(float) ); */

   /* compute quadmesh normal vectors */
   {
      float *qnorm;

      qnorm = (float *) malloc( qc * qr * 3 * sizeof(float) );
      /* qnorm = (float *) allocate( dtx, qc * qr * 3 * sizeof(float) ); */

      /* step 1: compute surface normal for each quadrilateral. */
      for (i=0;i<qr-1;i++) {
         for (j=0;j<qc-1;j++) {
            float a[3], b[3];
            int index;

            index = (i*qc+j)*3;
            /* a is the down vector, b is the right vector */
            a[0] = dtx->TopoVertex[index+qc*3+0] - dtx->TopoVertex[index+0];
            a[1] = dtx->TopoVertex[index+qc*3+1] - dtx->TopoVertex[index+1];
            a[2] = dtx->TopoVertex[index+qc*3+2] - dtx->TopoVertex[index+2];
            b[0] = dtx->TopoVertex[index+3+0] - dtx->TopoVertex[index+0];
            b[1] = dtx->TopoVertex[index+3+1] - dtx->TopoVertex[index+1];
            b[2] = dtx->TopoVertex[index+3+2] - dtx->TopoVertex[index+2];
            /* a cross b is the quad's facet normal */
            qnorm[index+0] =  a[1]*b[2]-a[2]*b[1];
            qnorm[index+1] = -a[0]*b[2]+a[2]*b[0];
            qnorm[index+2] =  a[0]*b[1]-a[1]*b[0];
         }
      }

      /* step 2: compute vertex normals by averaging adjacent */
      /* quadrilateral normals. */
      for (i=0;i<qr;i++) {
         for (j=0;j<qc;j++) {
            float n[3], mag;
            int index;

            index = (i*qc+j)*3;

            n[0] = n[1] = n[2] = 0.0;
            /* upper-left quad */
            if (i>0 && j>0) {
               n[0] += qnorm[ index-qc*3-3+0 ];
               n[1] += qnorm[ index-qc*3-3+1 ];
               n[2] += qnorm[ index-qc*3-3+2 ];
            }
            /* upper-right quad */
            if (i>0 && j<qc-1) {
               n[0] += qnorm[ index-qc*3+0 ];
               n[1] += qnorm[ index-qc*3+1 ];
               n[2] += qnorm[ index-qc*3+2 ];
            }
            /* lower-left quad */
            if (i<qr-1 && j>0) {
               n[0] += qnorm[ index-3+0 ];
               n[1] += qnorm[ index-3+1 ];
               n[2] += qnorm[ index-3+2 ];
            }
            /* lower-right quad */
            if (i<qr-1 && j<qc-1) {
               n[0] += qnorm[ index+0 ];
               n[1] += qnorm[ index+1 ];
               n[2] += qnorm[ index+2 ];
            }

            mag = sqrt( n[0]*n[0] + n[1]*n[1] + n[2]*n[2] );
            if (mag>0.0) {
               mag = 1.0 / mag;
               dtx->TopoNormal[index+0] = n[0] * mag;
               dtx->TopoNormal[index+1] = n[1] * mag;
               dtx->TopoNormal[index+2] = n[2] * mag;
            }
         }
      }

      free (qnorm);
      /* deallocate( dtx, qnorm, qc * qr * 3 * sizeof(float) ); */
   }

   dtx->qcols = qc;
   dtx->qrows = qr;

   /* Define the initial quadmesh vertex colors */
   init_topo_color_table( dtx->TopoColorTable[MAXVARS*VIS5D_MAX_CONTEXTS], 256,
                          dtx->MinTopoHgt, dtx->MaxTopoHgt );
   dtx->TopoColorVar = -1;


   /* MJK 12.02.98 */
   make_topo_strips (dtx);

 
   return 1;
   
}




/*
 * Draw the topography.
 * Input:  time - the timestep number
 *         texture_flag - 0=no texture, 1=texture map
 *         flat_flag - 0=draw w/ topo heights, 1=draw flat
 */
void draw_topo( Display_Context dtx, int time, int texture_flag, int flat_flag )
{
   /* MJK 12.02.98 begin */
   int         i, j, n, ir, ic, nr, nc, nr2, nc2;
   int_2       *verts;
   int_1       *norms;
   uint_1      *color;
   /* MJK 12.02.98 end */

   set_color( 0xffffffff );

   if (flat_flag) {
      if (texture_flag) {
         /* flat texture map */
         use_texture( dtx, time );
         texture_quadmeshnorm( dtx->qrows, dtx->qcols,
                               (void*) dtx->TopoFlatVertex,
                               NULL,  
                               (void*) dtx->TopoTexcoord ); 
      }
      else {
         /* draw nothing */
      }
   }
   else {
      if (texture_flag) {
         /* textured topo */
         use_texture( dtx, time );
         texture_quadmeshnorm( dtx->qrows, dtx->qcols,
                               (void*) dtx->TopoVertex,
                               (void*) dtx->TopoNormal,
                               (void*) dtx->TopoTexcoord ); 
      }
      else {
         /* untextured topo */
         uint_1 *indexes;
         unsigned int *color_table;

         if (dtx->TopoColorVar<0) {
            int yo;

            color_table = dtx->TopoColorTable[MAXVARS*VIS5D_MAX_CONTEXTS];
            indexes = dtx->TopoIndexes[MAXTIMES];
         }
         else {
            color_table = dtx->TopoColorTable[ dtx->TopoColorVarOwner * MAXVARS + dtx->TopoColorVar ];
            indexes = dtx->TopoIndexes[time];
            if (!indexes) {
               indexes = dtx->TopoIndexes[MAXTIMES];
            }
         }

         /* MJK 12.02.98 begin */
         if (dtx->TopoStripsVerts == NULL) return;
         if (dtx->TopoStripsNorms == NULL) return;

         verts = dtx->TopoStripsVerts;
         norms = dtx->TopoStripsNorms;
         nr    = dtx->qrows;
         nc    = dtx->qcols;
         nr2   = nr * 2;
         nc2   = nc * 2;

         n     = (nr > nc) ? nr : nc;
         color = (uint_1 *) malloc ((n * 2 * sizeof (uint_1)));
         if (color == NULL) return;


         /* topography */

         j = 0;
         i = nc;
         for (ir = 1; ir < nr; ir++)
         {
             n = 0;
             for (ic = 0; ic < nc; ic++)
             {
                color[n++] = indexes[i++];
                color[n++] = indexes[j++];
             }

             draw_colored_triangle_strip (nc2,
                                          (void *) verts, (void *) norms,
                                          color, color_table, 255);
             verts += nc2 * 3;
             norms += nc2 * 3;
         }
         if (dtx->DisplayTopoBase)
         {
             unsigned int    base_color = TOPO_BASE_COLOR;
             int             norm_dir = 1;

             /* MJK 3.29.99 */
             clipping_off();


             n = (nr > nc) ? nr : nc;
             memset (color, 0, (n * 2 * sizeof (uint_1)));

/* MJK reversed this 2.16.99
             norm_dir = (dtx->TopoBaseLev < 0.0) ? -1 : 1;
*/
/* MJK 3.29.99 don't know why this is here
             norm_dir = (dtx->TopoBaseLev < 0.0) ? -1 : 1; 
*/
             norm_dir = 1;

             /* north side */

             if ((check_face_norm (dtx, verts) * norm_dir > 0))
                 draw_colored_triangle_strip (nc2,
                                              (void *) verts,
                                              (void *) norms,
                                              color, &base_color, 255);
             verts += nc2 * 3;
             norms += nc2 * 3;

             /* south side */

             if ((check_face_norm (dtx, verts) * norm_dir) > 0)
                 draw_colored_triangle_strip (nc2,
                                              (void *) verts,
                                              (void *) norms,
                                              color, &base_color, 255);
             verts += nc2 * 3;
             norms += nc2 * 3;

             /* west side */

             if ((check_face_norm (dtx, verts) * norm_dir) > 0)
                 draw_colored_triangle_strip (nr2,
                                              (void *) verts,
                                              (void *) norms,
                                              color, &base_color, 255);
             verts += nr2 * 3;
             norms += nr2 * 3;

             /* east side */

             if ((check_face_norm (dtx, verts) * norm_dir) > 0)
                 draw_colored_triangle_strip (nr2,
                                              (void *) verts,
                                              (void *) norms,
                                              color, &base_color, 255);
             verts += nr2 * 3;
             norms += nr2 * 3;

             /* bottom */

             if ((check_face_norm (dtx, verts) * norm_dir) > 0)
             {
                 float       r, g, b, a, fac = 0.90;

                 /* color the bottom slightly darker than the sides */
                 r = (((float) UNPACK_RED (base_color)) / 255.0) * fac;
                 g = (((float) UNPACK_GREEN (base_color)) / 255.0) * fac;
                 b = (((float) UNPACK_BLUE (base_color)) / 255.0) * fac;
                 a = ((float) UNPACK_ALPHA (base_color)) / 255.0;
                 base_color = PACK_COLOR ((int) (r * 255.0),
                                          (int) (g * 255.0),
                                          (int) (b * 255.0),
                                          (int) (a * 255.0));

                 for (ir = 1; ir < nr; ir++)
                 {
                     draw_colored_triangle_strip (nc2,
                                                  (void *) verts,
                                                  (void *) norms,
                                                  color, &base_color, 255);
                     verts += nc2 * 3;
                     norms += nc2 * 3;
                 }
             }
             /* MJK 3.29.99 */
             clipping_on();
         }
         free (color);
      }
   }
}
/* MJK 12.02.98 end */

