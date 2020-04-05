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

#ifndef IAPI_H
#define IAPI_H

#include "file.h"
#include "v5d.h"
#include "globals.h"

struct netcdf_format_info{
   int type;

   /**************/
   /* Dimensions */
   /**************/
   char METAR_REPORT_TYPE_LENGTH[100];
   char METAR_CLD_LAYERS[100];

   /*************/
   /* Variables */
   /*************/
   char METAR_REPORT_TYPE[100];
   char TIME[100];
   char LAT[100];
   char LON[100];
   char HGT[100];
   char METAR_CLD_TYPE[100];

   char LEVELDIM[100];
   char LEVELVAR[100];

   /*********/
   /* Other */
   /*********/
   char METAR[100];
   char SPECI[100];
   char REC_NUM[100];
   char LOCATION_FILL[100];
};

typedef struct netcdf_format_info *NetCDF_Format_Info;


    
/******************/
/* Type so far... */
/******************/
extern NetCDF_Format_Info UNIDATA_METAR;
extern NetCDF_Format_Info FSL_METAR;
extern NetCDF_Format_Info FSL_PROFILE;



extern void Initialize_NetCDF_Format_Info( void );

extern int Open_NetCDF( char *filename, int *fid );

extern int Read_NetCDF( char *filename, int *general_type,
                         int *specific_type, int *fid );

extern int Read_netCDF_Var_ID( int fid, char *varname, int *varid);

extern int Read_NetCDF_Fill( int fid, int varid, double *fillvalue);

extern int Close_NetCDF( int fid );

extern int Read_Sounding_NetCDF_Var_Data( int nc_id, int recid, int varid,
                                   int levels, double *data);

extern int Read_NetCDF_Num_of_Levels( NetCDF_Format_Info finfo,
                              int fid, int *numlevels);

extern int Read_NetCDF_Levels( NetCDF_Format_Info finfo,
                              int nc_id, int recid, int numlevels, float *data);

extern int Read_1D_NetCDF_Var_Int_Data( int fileid, int recid, int varid,
                                      int *data);

extern int Read_1D_NetCDF_Var_Double_Data( int fileid, int recid, int varid,
                                      double *data);

extern int Read_1D_NetCDF_Var_Char_Data( int fileid, int recid, int varid,
                                     int len, char *data);

extern int Read_2D_NetCDF_Var_Int_Data( int fileid, int recid, int varid,
                                      int other_dim, int *data);

extern int Read_2D_NetCDF_Var_Double_Data( int fileid, int recid, int varid,
                                      int other_dim, double *data);

extern int Read_2D_NetCDF_Var_Char_Data( int fileid, int recid, int varid,
                                     int len, int other_dim, char *data);


extern int Read_NetCDF_Record_IDs( NetCDF_Format_Info finfo,
                                         int fid, 
                                         int thetime, int *ids);

extern int Read_NetCDF_Times_and_Recs( NetCDF_Format_Info finfo,
                            int fid, int *num_times,
                            int *ts, int *ds, int time[], int nrecs[]);

extern int Read_NetCDF_Vars( NetCDF_Format_Info finfo,
                            int nc_id,  int *num_vars,
                            char varname[MAXVARS][MAX_VAR_LENGTH],
                            int vartype[], int vardim[], int charvarlength[],
                            double varmin[MAXVARS], double varmax[MAXVARS]);

extern int Read_NetCDF_Bounds( NetCDF_Format_Info finfo,
                              int fid, float *west, float *east,
                              float *north, float *south, float *top, float *bottom);

extern int Read_NetCDF_Location( NetCDF_Format_Info finfo,
                                int fileid, int recid, float *lat, float *lon,
                                float *hgt);

#endif

