
/* irregular_v5d.c */

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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "binio.h"
#include "v5d.h"
#include "vis5d.h"
#include "irregular_v5d.h"
#include "file.h"



/********************************************/
/* Initialize an irregular v5d structure    */
/********************************************/
/* Input: v - pointer to irreguar v5dstruct */
/********************************************/
void irregular_v5dInitStruct( irregular_v5dstruct *v )
{
   memset( v, 0, sizeof(irregular_v5dstruct) );
}



/************************************************/
/* Allocate and initialize irregular v5d struct */
/************************************************/
/* Output: return pointer to new irreg struct   */
/************************************************/
irregular_v5dstruct *irregular_v5dNewStruct( void )
{
   irregular_v5dstruct * v;

   v = (irregular_v5dstruct *) malloc( sizeof(v5dstruct) );
   if (v){
      irregular_v5dInitStruct(v);
   }
   return v;
}


/*******************************************/
/* Free memory for irregular v5d struct    */
/*******************************************/
/* Input: v - pointer to irreg v5d struct  */
/*******************************************/
irregular_v5dstruct irregular_v5dFreeStruct( irregular_v5dstruct *v )
{
   free( v );
   v = 0;
}



/***********************************************************/
/* This will read the geographics data for one record      */
/***********************************************************/
/* Input: iv - the irregular v5d struct                    */
/*        time - the time step, from [0..iv->NumTimes]     */
/*        id - the record id, from [0..iv->NumRecs(time)]  */
/* Output: lat, lon, alt - the geographics position of the */
/*                         record.                         */
/***********************************************************/
int irregular_v5dReadRecordGeoData( irregular_v5dstruct *iv, int time, int id,
                             float *lat, float *lon, float *alt)
{
   int digit1, digit2;
   int index;

   /*********************************************************/
   /* check to see if the data is coming from the iimporter */
   /*********************************************************/
   if (strncmp(iv->FileName, "irregularv5dimportfile", 22)==0){
      digit1 = digit2 = 0;
      if (iv->FileName[22] >= '0' && iv->FileName[22] <= '9'){
         if (iv->FileName[23] >= '0' && iv->FileName[22] <= '9'){
            digit1 = (int)(iv->FileName[23] - '0');
            digit2 = (int)(iv->FileName[22] - '0');
            index = digit2*10+digit1;
         }
         else{
            digit1 = (int)(iv->FileName[22] - '0');
            index = digit1;
         }
         read_fdb_record_geo_data( index,  time, id, lat, lon, alt);
         return 1;
      }
      else{
         printf("error in irregular_read_v5d_header\n");
         return 0;
      }
   }
   else{
      printf(" can't handle this right now\n");
      return 0;
   }
   return 1;
}



/************************************************************************/
/* This will load the data for one record                               */
/************************************************************************/
/* Input: iv - the irregular v5d struct                                 */
/*        time - the time step, from [0..iv->NumTimes]                  */
/*        id - the record id, from [0..iv->NumRecs(time)]               */
/* Output: fdata - array has size of [ NumVars * Double ] and contains  */
/*                 data of type NUMERICAL_VAR_1D                        */
/*         sdata - array has size of                                    */
/*                 [ NumSoundingVars *Levels * Double ] and contains    */
/*                 data of type NUMERICAL_VAR_2D if iv->Type = SOUNDING */                      
/*         cdata - array has size of [ TotalNumCharPerRec * Char ] and  */
/*                 contains data of type CHARACTER_VAR                  */
/*         ldata - array has size of [ NumLevels * Float ] and          */
/*                 contains the height data for each level if           */
/*                 iv->Type = SOUNDING                                  */
/************************************************************************/
int irregular_v5dReadRecord( irregular_v5dstruct *iv, int time, int id, 
                             double *fdata,
                             double *sdata,  char *cdata, float *ldata)
{
   int digit1, digit2;
   int index;

   /*********************************************************/
   /* check to see if the data is coming from the iimporter */
   /*********************************************************/
   if (strncmp(iv->FileName, "irregularv5dimportfile", 22)==0){
      digit1 = digit2 = 0;
      if (iv->FileName[22] >= '0' && iv->FileName[22] <= '9'){
         if (iv->FileName[23] >= '0' && iv->FileName[22] <= '9'){
            digit1 = (int)(iv->FileName[23] - '0');
            digit2 = (int)(iv->FileName[22] - '0');
            index = digit2*10+digit1;
         }
         else{
            digit1 = (int)(iv->FileName[22] - '0');
            index = digit1;
         }
         read_fdb_record( index, iv, time, id, fdata, sdata, cdata, ldata);
         return 1;
      }
      else{
         printf("error in irregular_read_v5d_header\n");
         return 0;
      }
   }
   else{
      printf(" can't handle this right now\n");
      return 0;
   }
   return 1;
}

/**********************************************************************/
/* This will read the vital header info into the irregular v5d struct */
/**********************************************************************/
/* Input: filename - file name to read, if the file name is           */
/*        'irregularv5dimportfile' with a number appened after it then*/
/*        it is assumed that the header information is coming from the*/
/*        file data base(file.c, iapi.c igui.c imain.c).              */
/*        iv - pointer to the irregular v5d struct                    */
/* NOTE:                                                              */
/*   The needed header information is such...                         */
/*   iv->Type - Type of data , one of SURFACE or SOUNDING             */
/*   iv->NumTimes - Number of time steps                              */
/*   iv->Levels - Number of levels if SOUNDING data                   */
/*   iv->TimeStamp[MAXTIMES] - Time in HHMMSS for each timestep       */
/*   iv->DayStamp[MAXTIMES] - Day in YYDDD for each timestep          */
/*   iv->NumRecs[MAXTIMES] - Number of records per timestep           */
/*   iv->NumVars - Number of variables                                */
/*   iv->VarName[MAXVARS][MAX_VAR_LENGTH] - variable names            */
/*   iv->VarMin[MAXVARS] - Min value for each variable                */
/*   iv->VarMax[MAXVARS] - Max value for each variable                */
/*   iv->VarType[MAXVARS] - Variable type, one of CHARACTER_VAR,      */
/*                    NUMERICAL_VAR_1D or NUMERICAL_VAR_2D            */
/*   iv->CharVarLength[MAXVARS] - If var type = CHARACTER_VAR then    */
/*                    this is size of chars the var data              */
/*                    will always hold                                */
/*   iv->CharPointer[MAXVARS] - This is an index into an array        */
/*                    describing where the char data starts.          */
/*                    For example, let say iv->NumVars = 10 and       */
/*                    only var 0, 2 and 7 are of VarType CHARACTER_VAR*/
/*                    and iv->CharVarLength[0] = 12                   */
/*                        iv->CharVarLength[2] = 6                    */
/*                        iv->CharVarLength[7] = 9                    */
/*                    (iv->CharVarLength[1,3,4,5,6,8,9] = 0)          */
/*                    then                                            */
/*                    iv->CharPointer[0] = 0                          */
/*                    iv->CharPointer[1] = 0                          */
/*                    iv->CharPointer[2] = 12                         */
/*                    iv->CharPointer[3] = 0                          */
/*                    iv->CharPointer[4] = 0                          */
/*                    iv->CharPointer[5] = 0                          */
/*                    iv->CharPointer[6] = 0                          */
/*                    iv->CharPointer[7] = 18                         */
/*                    iv->CharPointer[8] = 0                          */
/*                    iv->CharPointer[9] = 0                          */
/*   iv->SoundingPointer[MAXVARS] - This is an index into an array    */
/*                    describing                                      */
/*                    where the 2D sounding data starts, similar to   */
/*                    the iv->CharPointer                             */
/*   iv->TopBound, iv->BottomBound,                                   */
/*   iv->WestBound, iv->EastBound,                                    */
/*   iv->NorthBound, iv->SouthBound - these are the bounding regions  */
/*                    needed later to create a grid and map           */
/*                    projection which all of the proj.c funcs willuse*/
/**********************************************************************/
static int irregular_read_v5d_header( char *filename, irregular_v5dstruct *iv )
{
   int digit1, digit2;
   int index;

   strcpy( iv->FileName, filename);

   /*********************************************************/
   /* check to see if the data is coming from the iimporter */
   /*********************************************************/
   if (strncmp( filename, "irregularv5dimportfile", 22)==0){
      digit1 = digit2 = 0;
      if (filename[22] >= '0' && filename[22] <= '9'){
         if (filename[23] >= '0' && filename[22] <= '9'){
            digit1 = (int)(filename[23] - '0');
            digit2 = (int)(filename[22] - '0');
            index = digit2*10+digit1;
         }
         else{
            digit1 = (int)(filename[22] - '0');
            index = digit1;
         }
         load_fdb_into_v5d( index, iv);
      }
      else{
         printf("error in irregular_read_v5d_header\n");
         return 0;
      }
   }
   else{
      printf(" can't handle this right now\n");
      return 0;
   }

   return 1;
}

 

irregular_v5dstruct *irregular_v5dOpenFile( char *filename, 
                                            irregular_v5dstruct *iv )
{

   if (iv){
      irregular_v5dInitStruct( iv );
   }
   else{
      iv = irregular_v5dNewStruct();
      if (!iv){
         return NULL;
      }
   }

   if (irregular_read_v5d_header( filename, iv )){
      return iv;
   }
   else{
      return NULL;
   }
   printf("ok\n");
}

   




