/* file.h */
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

#ifndef FILE_H
#define FILE_H

#include "iapi.h"
#include "irregular_v5d.h"
#include "api.h"

#define METAR_TYPE 1

/* assume when reading a netcdf 3d char */
/* that it is really just 2D_VAR becuase the */
/* 3rd netcdf dimension is really just the char*/
/* array */
#define VAR_1D 0
#define VAR_2D 1
#define VAR_3D 2
#define VAR_4D 3

#define CHAR_VAR   0
#define INT_VAR    1
#define FLOAT_VAR  2
#define DOUBLE_VAR 3

struct file_info{
   int index;

   char      FileName[MAXFILENAMELENGTH];
   int       FileStatus;
   int       FileID;
   int       FileType;
    
   int       NumTimes;
   int       DateStamp[MAXTIMES];
   int       TimeStamp[MAXTIMES];
   int       TimeSeconds[MAXTIMES];
   int       TimeSelected[MAXTIMES];
   int       FDB_to_F_timestep[MAXTIMES];

   int       NumVars;
   char      VarName[MAXVARS][MAX_VAR_LENGTH];
   int       VarType[MAXVARS]; 
   int       VarDimensions[MAXVARS];
   int       CharVarLength[MAXVARS];
   int       VarSelected[MAXVARS];
   int       VarID[MAXVARS];
   int       VarFillValue[MAXVARS];

   int       Levels;

   int       NumRecs[MAXTIMES];

   float     TopBound;
   float     BottomBound;
   float     EastBound;
   float     WestBound;
   float     NorthBound;
   float     SouthBound;

   double    VarMin[MAXVARS];
   double    VarMax[MAXVARS];
   
   int       *RecID[MAXTIMES];
   int       RecsPerTime[MAXTIMES];

   struct netcdf_format_info *Finfo;
};

struct file_db{

   int index;
   int FileType;

   int                NumFiles;
   struct file_info   *File[MAXFILES];
   int                WhichFileOpen;

   int                NumTimes;
   int                DateStamp[MAXTIMES];
   int                TimeStamp[MAXTIMES];
   int                TimeSeconds[MAXTIMES];
   int                TimeSelected[MAXTIMES];
   int                TimeN[MAXTIMES];
   int                NumVars;
   char               VarName[MAXVARS][MAX_VAR_LENGTH];
   int                VarType[MAXVARS]; 
   int                VarDimensions[MAXVARS]; 
   int                CharVarLength[MAXVARS];
   int                VarSelected[MAXVARS];

   int                NumRecs[MAXTIMES];

   float              TopBound;
   float              BottomBound;
   float              EastBound;
   float              WestBound;
   float              NorthBound;
   float              SouthBound;
  
   double             VarMin[MAXVARS];
   double             VarMax[MAXVARS];

   int                Levels;
 
   int                *RecIDfileindex[MAXTIMES];
   int                *RecIDfilerecid[MAXTIMES];
};

typedef struct file_info Irreg_FileInfo;
typedef struct file_db *FileDB;

extern int read_fdb_record_geo_data( int index,  int time,
                              int record, float *lat, float *lon, float *hgt);

extern int read_fdb_record( int index, irregular_v5dstruct *iv, int time,
                      int record, 
                      double *fdata, double *sdata, char *cdata, float *ldata);

extern int fdb_initialize( void );

extern FileDB make_new_fdb( void );

extern int add_a_file( FileDB fdb, char *filename );

extern int remove_a_file( FileDB fdb, int file_index);

extern struct file_info *alloc_file_info( void );

extern int load_fdb_into_v5d( int index, irregular_v5dstruct *iv );

#endif

