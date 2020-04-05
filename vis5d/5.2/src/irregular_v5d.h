/* Vis5D version 5.2 */


/*
Vis5D system for visualizing five dimensional gridded data sets
Copyright (C) 1990 - 2000 Bill Hibbard, Brian Paul, Dave Santek,
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


#ifndef IRREGULARV5D_H
#define IRREGULARV5D_H
#include <X11/Xlib.h>
#include "v5d.h"
#include "api.h"

#define MAXFILES           100
#define MAXFILENAMELENGTH  500
#define MAX_VAR_LENGTH     20

/* general types */
#define SURFACE              1
#define SOUNDING             2

/* general File types */
#define NETCDF_SURFACE       1
#define NETCDF_SOUNDING      2

/* specific File types */
#define fsl_netcdf_metar         1
#define fsl_netcdf_bouy          2
#define fsl_netcdf_profile       3
#define unidata_netcdf_metar     4
#define unidata_netcdf_upperair  5

/* variable types, two for now */
#define CHARACTER_VAR        1
#define NUMERICAL_VAR_1D     2
#define NUMERICAL_VAR_2D     3

typedef struct{
   int FileType;
   int Type;
   char FileName[2000];

   int NumRecs[MAXTIMES];
   int NumVars;
   int CharPointer[MAXVARS];
   int SoundingPointer[MAXVARS];
   int NumTimes;
   char VarName[MAXVARS][MAX_VAR_LENGTH];
   int VarType[MAXVARS];
   int CharVarLength[MAXVARS];
   int TimeStamp[MAXTIMES];
   int DateStamp[MAXTIMES];
   int TimeSeconds[MAXTIMES];
   float TopBound, BottomBound;
   float WestBound, EastBound;
   float SouthBound, NorthBound;
   int Levels;

   double VarMin[MAXVARS];
   double VarMax[MAXVARS];

} irregular_v5dstruct;

extern int irregular_v5dReadRecordGeoData( irregular_v5dstruct *iv, int time, int id,
                             float *lat, float *lon, float *alt);

extern int irregular_v5dReadRecord( irregular_v5dstruct *iv, int time, int id, 
                             double *fdata,
                             double *sdata,  char *cdata, float *ldata);

extern void irregular_v5dInitStruct( irregular_v5dstruct *v );

extern irregular_v5dstruct *irregular_v5dNewStruct( void );

extern irregular_v5dstruct irregular_v5dFreeStruct( irregular_v5dstruct *v );

extern irregular_v5dstruct *irregular_v5dOpenFile( char *filename,      
                                            irregular_v5dstruct *iv );

#endif
