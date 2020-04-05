
/* Vis5D version 5.2 */

/*
Vis5D system for visualizing five dimensional gridded data sets
Copyright (C) 1990-2000 Bill Hibbard, Brian Paul, Dave Santek,
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


/* Map projections */


#ifndef PROJ_H
#define PROJ_H



extern int setup_ctx_dtx_projection( Context ctx );

extern int setup_ctx_projection( Context ctx );

extern int setup_ctx_dtx_vertical_system( Context ctx );

extern int setup_ctx_vertical_system( Context ctx );

extern void get_projection( Context ctx, int *projection, float *projargs );

extern void get_projection_d(Display_Context dtx, int *projection, float *projargs );

extern void get_vertical_system_d( Display_Context dtx, int *vertical, float *vertargs );

extern void get_vertical_system( Context ctx, int *vertical, float *vertargs );


extern float gridlevelPRIME_to_zPRIME( Display_Context dtx, int time, int var, float level );

extern float gridlevel_to_z( Context ctx, int time, int var, float level );

extern float gridlevelPRIME_to_gridlevel( Context ctx, float levelPRIME );

extern float gridlevel_to_gridlevelPRIME( Context ctx, float level );

extern float gridlevel_to_height( Context ctx, float level );

extern float gridlevelPRIME_to_height( Display_Context dtx, float level );

extern float height_to_gridlev( Context ctx, float hgt );

extern float height_to_gridlevPRIME( Display_Context dtx, float hgt );

extern float gridlevelPRIME_to_height( Display_Context dtx, float level );

extern float height_to_z( Context ctx, float hgt );

extern float height_to_zPRIME( Display_Context dtx, float hgt );

/* MJK 2.17.99 */
extern float height_to_zTOPO( Display_Context dtx, float hgt );

extern void grid_to_xyz( Context ctx, int time, int var, int n,
                         float r[], float c[], float l[],
                         float x[], float y[], float z[] );

extern void gridPRIME_to_grid( Context ctx, int time, int var, int n,
                        float rPRIME[], float cPRIME[], float lPRIME[],
                        float r[], float c[], float l[] );

extern void grid_to_gridPRIME( Context ctx, int time, int var, int n,
                        float r[], float c[], float l[],
                        float rPRIME[], float cPRIME[], float lPRIME[]);

extern void grid_to_xyzPRIME(Context ctx, int time, int var, int n,
                  float r[], float c[], float l[],
                  float x[], float y[], float z[] );

extern void gridPRIME_to_xyzPRIME( Display_Context dtx, int time, int var, int n,
                         float r[], float c[], float l[],
                         float x[], float y[], float z[] );


extern void xyz_to_compXYZ( Display_Context dtx, int n, float x[], float y[],
                     float z[], int_2 xyz[][3] );

extern void gridPRIME_to_compXYZPRIMEcheck( Display_Context, int time, int var, int *N,
                             float r[], float c[], float l[],
                             int_2 xyz[][3] );

extern void gridPRIME_to_compXYZPRIME( Display_Context, int time, int var, int n,
                             float r[], float c[], float l[],
                             int_2 xyz[][3] );

extern void grid_to_compXYZ( Context ctx, int time, int var, int n,
                             float r[], float c[], float l[],
                             int_2 xyz[][3] );


extern void geo_to_xyz( Context ctx, int time, int var, int n,
                        float lat[], float lon[], float hgt[],
                        float x[], float y[], float z[] );

extern void geo_to_xyzPRIME( Display_Context dtx, int time, int var, int n,
                        float lat[], float lon[], float hgt[],
                        float x[], float y[], float z[] );

/* MJK 2.17.99 */
extern void geo_to_xyzTOPO( Display_Context dtx, int time, int var, int n,
                        float lat[], float lon[], float hgt[],
                        float x[], float y[], float z[] );


extern void gridPRIME_to_geo (Display_Context dtx, int time, int var, int n,
                  float row[], float col[], float lev[],
                  float lat[], float lon[], float hgt[]);

extern void rowcol_to_latlon( Context ctx, int time, int var,
                              float row, float col, float *lat, float *lon );

extern void rowcolPRIME_to_latlon( Display_Context dtx, int time, int var,
                              float row, float col, float *lat, float *lon );

extern void xyzPRIME_to_gridPRIME( Display_Context dtx, int time, int var,
                         float x, float y, float z,
                         float *row, float *col, float *lev );

extern void xyzPRIME_to_grid( Context ctx, int time, int var,
                  float x, float y, float z,
                  float *row, float *col, float *lev );

extern void xyz_to_grid( Context ctx, int time, int var,
                         float x, float y, float z,
                         float *row, float *col, float *lev );

extern void latlon_to_rowcol (Context ctx, int time, int var,
                       float lat, float lon,
                       float *row, float *col);

extern void latlon_to_rowcolPRIME(Display_Context dtx, int time, int var,
                       float lat, float lon,
                       float *row, float *col);

extern void geo_to_grid (Context ctx, int time, int var, int n,
                         float lat[], float lon[], float hgt[],
                         float row[], float col[], float lev[]);

extern void grid_to_geo (Context ctx, int time, int var, int n,
                         float row[], float col[], float lev[],
                         float lat[], float lon[], float hgt[]);

extern void geo_to_gridPRIME (Display_Context dtx, int time, int var, int n,
                  float lat[], float lon[], float hgt[],
                  float row[], float col[], float lev[]);


extern void xyz_to_geo( Context ctx, int time, int var,
                        float x, float y, float z,
                        float *lat, float *lon, float *hgt );

extern void xyzPRIME_to_geo( Display_Context dtx, int time, int var,
                        float x, float y, float z,
                        float *lat, float *lon, float *hgt );

extern void project_normalsPRIME( Display_Context dtx, int n,
                             float vr[], float vc[], float vl[],
                             float nx[], float ny[], float nz[],
                             int_1 cnorms[][3] );

extern void project_normals( Context ctx, int n,
                             float vr[], float vc[], float vl[],
                             float nx[], float ny[], float nz[],
                             int_1 cnorms[][3] );


extern float earth_distance( float lat1, float lon1, float lat2, float lon2 );



extern void latlon_bounds( Display_Context dtx,
                          float *lats, float *latn, float *lonw, float *lone );


extern void pandg_back( float *lat, float *lon, float a, float b, float r );


extern void pandg_for( float *lat, float *lon, float a, float b, float r );


#endif
