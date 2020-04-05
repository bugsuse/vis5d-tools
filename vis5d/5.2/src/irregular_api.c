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



#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <signal.h>
#include <unistd.h>
#include <X11/Xlib.h>
#ifdef OPENGL
#include "GL/gl.h"
#include "GL/glu.h"
#include "GL/glx.h"
#endif
#ifdef sgi
#  include <sys/types.h>
#  include <sys/prctl.h>
#  include <sys/sysmp.h>
#endif
#ifdef sunos5
#  include <thread.h>
#endif

#include "analysis.h"
#include "anim.h"
#include "api.h"
#include "box.h"
#include "chrono.h"
#include "compute.h"
#include "globals.h"
#include "graphics.h"
#include "groupchrono.h"
#include "grid.h"
#include "record.h"
#include "image.h"
#include "memory.h"
#include "imemory.h"
#include "map.h"
#include "misc.h"
#include "proj.h"
#include "queue.h"
#include "render.h"
#include "save.h"
#include "sounding.h"
#include "soundingGUI.h"
#include "sync.h"
#include "tclsave.h"
#include "traj.h"
#include "topo.h"
#include "volume.h"
#include "work.h"
#include "netcdf.h"
#include "irregular_api.h"
#include "irregular_v5d.h"

/*****************************************************/
/* when reading NetCDF METAR file information        */
/* assume variables and dimensions to be named       */
/* such.  If not then change these define statements */
/*****************************************************/

/**************/
/* Dimensions */
/**************/
#define METAR_REPORT_TYPE_LENGTH          "rep_type_len"
#define METAR_CLOUD_LAYERS                "cloud_layers"

/*************/
/* Variables */
/*************/
#define METAR_REPORT_TYPE                 "rep_type"
#define METAR_LAT                         "lat"
#define METAR_LON                         "lon"
#define METAR_HGT                         "elev"
#define METAR_STATION_NAME                "stn_name"

/*********
/* Other */
/*********/
#define METAR                             "METAR"


int Read_NetCDF_METARS( char *filename, int *num_data_sets )
{
   int nc_id, id1, id2;
   int status;
   size_t len1, len2;
   char *text1;
   
   /* assume only metar data is being read */
   *num_data_sets = 1;

   /********************************************/   
   /* open file and see if it is a netCDF file */
   /********************************************/
   status = nc_open( filename, NC_NOWRITE, &nc_id);
   if(status != NC_NOERR){
      return -1;
   }


   /***********************************/
   /* check report type of first      */
   /* record, to see if it is a METAR */
   /***********************************/
     
   status = nc_inq_dimid( nc_id, METAR_REPORT_TYPE_LENGTH, &id1);
   if (status != NC_NOERR){
      nc_close(nc_id);
      return -1;
   }
   len2 = (size_t)(strlen(METAR));
   status = nc_inq_dimlen( nc_id, id1, &len1);
   if (status != NC_NOERR || len1 != len2){
      nc_close(nc_id);
      return -1;
   }
   status = nc_inq_varid( nc_id, METAR_REPORT_TYPE, &id2);
   if (status != NC_NOERR){
      nc_close(nc_id);
      return -1;
   }
   text1 = (char *) malloc(len2+1);
   {
      size_t temp[] = {0,0};
      int i;
      for (i = 0; i < len2; i++){
         nc_get_var1_text(nc_id, id2, temp, &text1[i]);
         temp[1]++;
      }
   }

   if (strcmp(METAR, text1)==0){
      free(text1);
      nc_close(nc_id);
      return 0;
   }
   else{
      free(text1);
      nc_close(nc_id);
      return -1;
   }
}

int Read_NetCDF_METARS_Header( char *filename, irregular_v5dstruct *ir)
{
   int temp, c, i, j, k, t, *times, tnid, status;
   int offset, numuniquetimes;
   int nc_id, numrecsid;
   int id, year, day, hour, minute, second;
   nc_type type;
   size_t sizea, sized, numrecs;
   int ntimes[MAXTIMES], uniquetimes[MAXTIMES];
   char vname[1000];
   char tdim[1000];
   size_t num_cloud_layers;
   int numdims;
   int dimids[10];
   size_t stringsize;
   int cloud_layer_id;
   int latid, lonid, hgtid;

   status = nc_open( filename, NC_NOWRITE, &nc_id);
   if(status != NC_NOERR){
      return -1;
   }

   /****************/
   /* get filename */
   /****************/
   strncpy(ir->filename, filename, 1000);


   /******************/
   /* get file title */
   /******************/
   status = nc_get_att_text(nc_id, NC_GLOBAL, "title", ir->filetitle);
   if(status != NC_NOERR){
      return -1;
   }

   /*************************/
   /* get number of records */
   /*************************/
   status = nc_inq_dimid(nc_id, "recNum", &numrecsid);
   if(status != NC_NOERR){
      return -1;
   }
   status = nc_inq_dimlen(nc_id, numrecsid, &numrecs);
   if(status != NC_NOERR){
      return -1;
   }

   /****************/
   /* get numtimes */
   /****************/   
   for (i = 0; i < MAXTIMES; i++){
      uniquetimes[i] = -1;
   }

   status = nc_inq_varid( nc_id, "time_nominal", &tnid);
   if(status != NC_NOERR){
      return -1;
   }

   times = (int *) malloc(sizeof(int)*numrecs);
   if (!times){
      return -1;
   }

   status = nc_get_var_int( nc_id, tnid, times);
   if(status != NC_NOERR){   
      free(times);
      return -1;   
   }   
   
   numuniquetimes = 1;
   uniquetimes[0] = times[0];
   ntimes[0] = 1;
   for (i = 1; i < numrecs; i++){
      for (j = 0; j < numuniquetimes; j++){
         if (times[i] == uniquetimes[j]){
            ntimes[j]++;

            break;
         }
      }
      if (j == numuniquetimes){
         uniquetimes[j] = times[i];
         ntimes[j] = 1;
         numuniquetimes++;
      }
   }   
   ir->numtimes = numuniquetimes;

   /* bubble sort unique times */
   for (i = 0; i < numuniquetimes; i++){
      for (j = 0; j < numuniquetimes-1-i; j++){
         if (times[j+1] > times[j]){
            temp = uniquetimes[j];
            uniquetimes[j] = uniquetimes[j+1];
            uniquetimes[j+1] = temp;
            temp = ntimes[j];
            ntimes[j] = ntimes[j+1];
            ntimes[j+1] = temp;
         }
      }
   }
   ir->numrecs = 0;
   for (i = 0; i < ir->numtimes; i++){
      if (ntimes[i] > ir->numrecs){
         ir->numrecs = ntimes[i];
      }
   }
   
   /***********************/
   /* get day/time stamps */
   /***********************/         
   for (i=0; i < ir->numtimes; i++){
      t = times[i];
      day = t/86400;
      t -= 86400 * day;
      if (day > 730){
         day  -= 730;
         year  = (4*day)/1461;
         day   = day - (365*year+(year-1)/4);
         year += 72;
      }
      else{
         year  = day / 365;
         day   = day - (365*year);
      }
      hour = t/3600;
      t -= 3600*hour;
      minute = t/60;
      t -= 60*minute;
      second = t;
      ir->timestamp[i] = 10000*hour+100*minute+second;
      ir->daystamp[i] = 1000*year+day;
   }

   /*********************/
   /* get variable info */
   /*********************/
   status = nc_inq_nvars( nc_id, &ir->numvars);
   if(status != NC_NOERR){
      free(times);
      return -1;
   }

   /* check to make sure lat, lon and hgt are there */ 
   /* and subtract 3 from the total number of variables */
   status = nc_inq_varid( nc_id, METAR_LAT, &latid);
   if(status != NC_NOERR){
      free(times);
      return -1;
   }
   status = nc_inq_varid( nc_id, METAR_LON, &lonid);
   if(status != NC_NOERR){
      free(times);
      return -1;
   }
   status = nc_inq_varid( nc_id, METAR_HGT, &hgtid);
   if(status != NC_NOERR){
      free(times);
      return -1;
   }

/* useless!
   for (i = 0; i < numrecs; i++){
      size_t theindex[3];
      theindex[0] = i;
      theindex[1] = 0;
      for (j = 0; j < 4; j++){
         theindex[2] = j;   
         nc_get_var1_text(nc_id, 20, theindex, &tdim[j]);
      }
      tdim[4] = 0;
   }
*/

 
   offset = 0;


   /* get cloud layers */
   status = nc_inq_dimid(nc_id, METAR_CLOUD_LAYERS, &cloud_layer_id);
   if(status != NC_NOERR){
      printf("no cloud_layers dimension\n");
      free(times);
      return -1;
   }
   status = nc_inq_dimlen(nc_id, cloud_layer_id, &num_cloud_layers);
   if(status != NC_NOERR){
      printf("no cloud_layers dimension\n");
      free(times);
      return -1;
   }
   if (num_cloud_layers < 1 ||
       num_cloud_layers > 9){
      printf("number of cloud layers must be between 1..9\n");
      free(times);
      return -1;
   }
    


   for (i = 0; i < ir->numvars; i++){
      k = i - offset;
      status = nc_inq_varname( nc_id, i, vname);
      if(status != NC_NOERR){
         free(times);
         return -1;
      }
      status = nc_inq_vartype( nc_id, i, &type);
      if(status != NC_NOERR){
         free(times);
         return -1;
      }
      if (strncmp(METAR_LAT, vname, strlen(METAR_LAT)) != 0 &&
         strncmp(METAR_LON, vname, strlen(METAR_LON)) != 0 &&
         strncmp(METAR_HGT, vname, strlen(METAR_HGT)) != 0){
         if (strlen(vname) < MAX_VAR_LENGTH){
            strncpy(ir->VarName[k], vname, MAX_VAR_LENGTH);
            status = nc_inq_varndims( nc_id, i, &numdims);
            if(status != NC_NOERR){
               printf("error getting numdims\n");
               free(times);
               return -1;
            }
            status = nc_inq_vardimid( nc_id, i, dimids);
            if(status != NC_NOERR){
               printf("error getting dimids\n");
               free(times);
               return -1;
            }
            if (type == NC_CHAR){
               if (numdims == 3){
                  /* cloud layers so get max num of layers */
                  /* and create vars to correspond to cl layers */ 
                  ir->VarName[k][strlen(ir->VarName[k])] = '0';
                  for (c = 1; c < num_cloud_layers; c++){
                     strncpy(ir->VarName[k+c], vname, MAX_VAR_LENGTH);
                     ir->VarName[k+c][strlen(ir->VarName[k+c])] = c + '0';
                     offset--;
                  }
                  status = nc_inq_dimlen( nc_id, dimids[2], &stringsize);
                  if(status != NC_NOERR){
                     printf("error getting stringsize\n");
                     free(times);
                     return -1;
                  }
                  for (c = 0; c < num_cloud_layers; c++){
                     ir->CharVarLength[k+c] = stringsize+1;
                     ir->VarType[k] = 1;
                  }   
               }
               else if (numdims == 2){
                  status = nc_inq_dimlen( nc_id, dimids[1], &stringsize);
                  if(status != NC_NOERR){
                     printf("error getting stringsize\n");
                     free(times);
                     return -1;
                  }
                  ir->CharVarLength[k] = stringsize+1;
               }
               else{
                  printf("don't know what to do with four dimensional var\n");
                  free(times);
                  return -1;
               }
               ir->VarType[k] = 1;
               ir->CharVarLength[k] = stringsize+1;
            }   
            else{
               if (numdims == 2){
                  if (dimids[1] != cloud_layer_id){
                     printf("error with var second demension\n");
                     free(times);
                     return -1;
                  }
                  ir->VarName[k][strlen(ir->VarName[k])] = '0';
                  for (c = 1; c < num_cloud_layers; c++){
                     strncpy(ir->VarName[k+c], vname, MAX_VAR_LENGTH);
                     ir->VarName[k+c][strlen(ir->VarName[k+c])] = c + '0';
                     offset--;
                     ir->CharVarLength[k+c] = 0;
                     ir->VarType[k+c] = 0;
                  }
               }
               ir->VarType[k] = 0;
               ir->CharVarLength[k+c] = 0;
            }
         }
         else{
            /* don't include this var becuase */
            /* wont be able to access it by its */
            /* full variable name later         */
            offset++;
         }
      }
      else{
         offset++;
      }
   }
   ir->numvars -= offset;

/*    for (c = 0; c < ir->numvars; c++){
      printf("var = %d name = %s\n", c, ir->VarName[c]);
   }
*/

   free(times);

   /*  get bounds */
   ir->NorthBound = 0.0;
   ir->SouthBound = 0.0;
   ir->WestBound =  0.0;
   ir->EastBound =  0.0;
   ir->BottomBound =0.0;
   ir->TopBound =   0.0;
   
   {
      float *latdata, *londata, *hgtdata;
      
      latdata = (float *) malloc(numrecs * sizeof(float));
      if (!latdata){
         printf("couldn't allocate enough memory\n");
         return -1;
      }
      londata = (float *) malloc(numrecs * sizeof(float));
      if (!londata){
         printf("couldn't allocate enough memory\n");
         free(latdata);
         return -1;
      }
      hgtdata = (float *) malloc(numrecs * sizeof(float));
      if (!hgtdata){
         printf("couldn't allocate enough memory\n");
         free(latdata);         
         free(londata);         
         return -1;
      }
      status = nc_get_var_float( nc_id, latid, latdata);
      if(status != NC_NOERR){
         printf("error getting bounds\n");
         free(latdata);
         free(londata);
         free(hgtdata);
         return -1;
      }
      status = nc_get_var_float( nc_id, lonid, londata);
      if(status != NC_NOERR){
         printf("error getting bounds\n");
         free(latdata);         
         free(londata);         
         free(hgtdata);
         return -1;
      }           
      status = nc_get_var_float( nc_id, hgtid, hgtdata);
      if(status != NC_NOERR){
         printf("error getting bounds\n");
         free(latdata);         
         free(londata);         
         free(hgtdata);
         return -1;
      }
      for (i = 0; i < numrecs; i++){
         if (latdata[i] != -99999.0){
            if (latdata[i] > ir->NorthBound){
               ir->NorthBound = latdata[i];
            }
            else if (latdata[i] < ir->SouthBound){
               ir->SouthBound = latdata[i];
            }
         }
      }
      for (i = 0; i < numrecs; i++){
         if (londata[i] != -99999.0){
            if (londata[i] < ir->EastBound){
               ir->EastBound = londata[i];
            }
            else if (londata[i] > ir->WestBound){
               ir->WestBound = londata[i];
            }
         }
      }
      for (i = 0; i < numrecs; i++){
         if (hgtdata[i] != -99999.0){
            if (hgtdata[i] > ir->TopBound){
               ir->TopBound = hgtdata[i];
            }
            else if (hgtdata[i] < ir->BottomBound){
               ir->BottomBound = hgtdata[i];
            }
         }
      }
      printf("n = %f s = %f w = %f e = %f t = %f b = %f\n", ir->NorthBound, ir->SouthBound, ir->WestBound, ir->EastBound, ir->TopBound, ir->BottomBound);
   }






   return 0;
}








 
