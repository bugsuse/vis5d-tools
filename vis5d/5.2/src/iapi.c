/* iapi.c */

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



/***************************************************************/
/***************************************************************/
/* This file contains a set of functions that will act as an   */
/* interface for reading and obtaining various nessecary info  */
/* about a NetCDF file.                                        */
/* The functions in 'file.c' will use these.                   */
/***************************************************************/
/***************************************************************/



#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "netcdf.h"
#include "iapi.h"
#include "igui.h"

NetCDF_Format_Info UNIDATA_METAR;
NetCDF_Format_Info FSL_METAR;
NetCDF_Format_Info FSL_PROFILE;

/************************/
/* Close a NetCDF file  */
/************************/
/* Input: fid - File ID */
/************************/
int Close_NetCDF( int fid )
{
   int status;

   status = nc_close(fid);
   if (status != NC_NOERR){
      return 0;
   }
   return 1;
}

/*****************************************************/
/* This will initialize the structures which contain */ 
/* the needed information, such as var names, for the*/
/* different NetCDF file types                       */
/* When adding the ability to read more NetCDF files */
/* more formats will have to be added here           */
/*****************************************************/
void Initialize_NetCDF_Format_Info( void )
{
   /************************************************/
   /* go through the list of formats and init them */
   /************************************************/

   /*************/
   /* FSL_METAR */
   /*************/
   FSL_METAR = (NetCDF_Format_Info) calloc( 1, sizeof(struct netcdf_format_info));
   if (!FSL_METAR){
      printf("Error in creating FSL_METAR struct\n");
      exit(0);
   }
   {
      strcpy(FSL_METAR->METAR_REPORT_TYPE_LENGTH, "maxRepLen");
      strcpy(FSL_METAR->METAR_CLD_LAYERS, "maxSkyCover");
      strcpy(FSL_METAR->METAR_REPORT_TYPE, "reportType");
      strcpy(FSL_METAR->TIME, "timeNominal");
      strcpy(FSL_METAR->LAT, "latitude");
      strcpy(FSL_METAR->LON, "longitude");
      strcpy(FSL_METAR->HGT, "elevation");
      strcpy(FSL_METAR->METAR_CLD_TYPE, "skyCover");
      strcpy(FSL_METAR->METAR, "METAR");
      strcpy(FSL_METAR->SPECI, "SPECI");
      strcpy(FSL_METAR->REC_NUM, "recNum");
      strcpy(FSL_METAR->LOCATION_FILL, "_FillValue");
   }

   /*****************/
   /* UNIDATA_METAR */
   /*****************/
   UNIDATA_METAR = (NetCDF_Format_Info) calloc( 1, sizeof(struct netcdf_format_info));
   if (!UNIDATA_METAR){
      printf("Error in creating UNIDATA_METAR struct\n");
      exit(0);
   }
   {
      strcpy(UNIDATA_METAR->METAR_REPORT_TYPE_LENGTH, "rep_type_len");
      strcpy(UNIDATA_METAR->METAR_CLD_LAYERS, "cloud_layers");
      strcpy(UNIDATA_METAR->METAR_REPORT_TYPE, "rep_type");
      strcpy(UNIDATA_METAR->TIME, "time_nominal");
      strcpy(UNIDATA_METAR->LAT, "lat");
      strcpy(UNIDATA_METAR->LON, "lon");
      strcpy(UNIDATA_METAR->HGT, "elev");
      strcpy(UNIDATA_METAR->METAR_CLD_TYPE, "cloud_type");
      strcpy(UNIDATA_METAR->METAR, "METAR");
      strcpy(UNIDATA_METAR->SPECI, "SPECI");
      strcpy(UNIDATA_METAR->REC_NUM, "recNum");
      strcpy(UNIDATA_METAR->LOCATION_FILL, "_FillValue");
   }


   /***************/   
   /* FSL PROFILE */
   /***************/
   FSL_PROFILE = (NetCDF_Format_Info) calloc( 1, sizeof(struct netcdf_format_info));
   if (!FSL_PROFILE){
      printf("Error in creating FSL_PROFILEstruct\n");
      exit(0);
   }
   {
      strcpy(FSL_PROFILE->LEVELDIM, "level");
      strcpy(FSL_PROFILE->LEVELVAR, "levels");
      strcpy(FSL_PROFILE->LAT, "staLat");
      strcpy(FSL_PROFILE->LON, "staLon");
      strcpy(FSL_PROFILE->HGT, "staElev");
      strcpy(FSL_PROFILE->TIME, "timeObs");
      strcpy(FSL_PROFILE->REC_NUM, "recNum");
      strcpy(FSL_PROFILE->LOCATION_FILL, "_FillValue");
   }

}   


/****************************************************/
/* This helper function goes through a list of data */
/* and finds the min and max values, and disregards */
/* any values matching fillvalue                    */
/****************************************************/
/* Input: numdata - the size of the data array      */
/*        data - array containing the number data   */
/*        fillvalue - values which are considered   */ 
/*                    missing                       */
/* Output: min, max - the min and max values        */
/****************************************************/
void get_min_and_max( int numdata, double *data,
                      double fillvalue, double *min, double *max)
{
   int i, j;
   double tmin, tmax;

   /*************************************/   
   /* first get first min and max since */
   /* first data value may be fillvalue */
   /*************************************/
   j = 0;
   do{
     tmin = tmax = data[j];
     j++;
   } while(tmin == fillvalue && j < numdata);

   for (i = j; i < numdata; i++){
      if (data[i] != fillvalue){
         if (data[i] < tmin){
            tmin = data[i];
         }
         else if (data[i] > tmax){
            tmax = data[i];
         }
      }
   }
   *min = tmin;
   *max = tmax;
}


/*************************************************/
/* This opens a NetCDF file and gets it's file ID*/
/*************************************************/
/* Input: filename - the name of the file to open*/
/* Output: fid - NetCDF file ID                  */
/*************************************************/
int Open_NetCDF( char *filename, int *fid )
{
   int status, nc_id;

   /********************************************/
   /* open file and see if it is a netCDF file */
   /********************************************/
   status = nc_open( filename, NC_NOWRITE, &nc_id);
   if(status != NC_NOERR){
      return 0;
   }
   *fid = nc_id;
   return 1;
}


/***************************************************/
/* This opens a NetCDF file and returns the type   */
/* It is fairly general right now and may need to  */
/* be altered when more types need to be           */
/* distinguished                                   */
/***************************************************/
/* Input: filename - the name of the file to open  */
/* Output: general_type - one of NETCDF_SURFACE or */
/*                        NETCDF_SOUNDING          */
/*         specific_type- one of fsl_netcdf_metar  */
/*                        fsl_netcdf_bouy          */
/*                        fsl_netcdf_profile       */
/*                        unidata_netcdf_metar     */
/*                        unidata_netcdf_upperair  */
/*         fid - NetCDF file ID                    */
/***************************************************/
int Read_NetCDF( char *filename, int *general_type, int *specific_type, int *fid )
{
   int id1, id2;
   int status, status2, status3, status4;
   size_t len1, len2;
   int nc_id;
   char *text1;

   *general_type = -1;
   *specific_type = -1;

   /********************************************/
   /* open file and see if it is a netCDF file */
   /********************************************/
   status = nc_open( filename, NC_NOWRITE, &nc_id);
   if(status != NC_NOERR){
      printf("Error: can't open netcdf file\n");
      return 0;
   }
   *fid = nc_id;


   /****************************/   
   /* Try FSL_NETCDF_METAR 1st */
   /****************************/
   status = nc_inq_dimid( nc_id, FSL_METAR->METAR_REPORT_TYPE_LENGTH, &id1);
   if (status == NC_NOERR){
      status = nc_inq_dimlen( nc_id, id1, &len1);
      if (status != NC_NOERR){
         nc_close(nc_id);
         return 0;
      }
      status = nc_inq_varid( nc_id, FSL_METAR->METAR_REPORT_TYPE, &id2);
      if (status != NC_NOERR){
         nc_close(nc_id);
         return 0;
      }
      text1 = (char *) malloc(len1+1);
      memset(text1, 0, len1+1);
      {
         size_t temp[] = {0,0};
         int i;
         for (i = 0; i < len1; i++){
            nc_get_var1_text(nc_id, id2, temp, &text1[i]);
            temp[1]++;
         }
      }

      if (strcmp(FSL_METAR->METAR, text1)==0 ||
          strcmp(FSL_METAR->SPECI, text1)==0){
         free(text1);
         *general_type = NETCDF_SURFACE;
         *specific_type = fsl_netcdf_metar;
         return 1;
      }
      else{
         free(text1);
         nc_close(nc_id);
         return 0;
      }
   }

   /*********************************/   
   /* Try UNIDATA_NETCDF_METAR next */
   /*********************************/
   status = nc_inq_dimid( nc_id, UNIDATA_METAR->METAR_REPORT_TYPE_LENGTH, &id1);
   if (status == NC_NOERR){
      status = nc_inq_dimlen( nc_id, id1, &len1);
      if (status != NC_NOERR){
         nc_close(nc_id);
         return 0;
      }
      status = nc_inq_varid( nc_id, UNIDATA_METAR->METAR_REPORT_TYPE, &id2);
      if (status != NC_NOERR){
         nc_close(nc_id);
         return 0;
      }
      text1 = (char *) malloc(len1+1);
      memset(text1, 0, len1+1);
      {
         size_t temp[] = {0,0};
         int i;
         for (i = 0; i < len1; i++){
            nc_get_var1_text(nc_id, id2, temp, &text1[i]);
            temp[1]++;
         }
      }

      if (strcmp(UNIDATA_METAR->METAR, text1)==0 ||
          strcmp(UNIDATA_METAR->SPECI, text1)==0){
         free(text1);
         *general_type = NETCDF_SURFACE;
         *specific_type = unidata_netcdf_metar;
         return 1;
      }
      else{
         free(text1);
         nc_close(nc_id);
         return 0;
      }
   }


   /**************************************/
   /* now check to see if it's a PROFILE */
   /**************************************/
   /* for now the only way i'm assuming  */
   /* it's a wind profile file, is if its*/
   /* got LEVELVAR as one of it's variables*/
   /* this should change later           */
   /**************************************/
   status = nc_inq_varid( nc_id, FSL_PROFILE->LEVELVAR, &id1);
   status2 = nc_inq_varid( nc_id, FSL_PROFILE->LAT, &id1);
   status3 = nc_inq_varid( nc_id, FSL_PROFILE->LON, &id1);
   status4 = nc_inq_varid( nc_id, FSL_PROFILE->HGT, &id1);
   if (status == NC_NOERR && status2 == NC_NOERR &&
       status3== NC_NOERR && status4 == NC_NOERR){
      *general_type = NETCDF_SOUNDING;
      *specific_type = fsl_netcdf_profile;
      return 1;
   }

   printf("Error: unrecognized file type\n");
   return 0;
}


/***************************************************/
/* This reads a NetCDF location for a given record */
/***************************************************/
/* Input: finfo - the NetCDF format info           */
/*        nc_id - NetCDF file ID                   */
/*        recid - Record number or ID for desired  */
/*                location                         */
/* Output: lat, lon, hgt - Latitude, Longitude and */
/*                         Altitude for record     */
/***************************************************/
int Read_NetCDF_Location( NetCDF_Format_Info finfo,
                          int nc_id, int recid, float *lat, float *lon,
                          float *hgt)
{
   int latid, lonid, hgtid;
   static size_t index[1];
   int status;

   index[0] = recid;
   status = nc_inq_varid( nc_id, finfo->LAT, &latid);
   if(status != NC_NOERR){
      printf("error in int Read_NetCDF_Location\n");
      return 0;
   }
   status = nc_inq_varid( nc_id, finfo->LON, &lonid);
   if(status != NC_NOERR){
      printf("error in int Read_NetCDF_Location\n");
      return 0;
   }
   status = nc_inq_varid( nc_id, finfo->HGT, &hgtid);
   if(status != NC_NOERR){
      printf("error in int Read_NetCDF_Location\n");
      return 0;
   }
   
   status = nc_get_var1_float( nc_id, latid, index, lat);
   if(status != NC_NOERR){
      printf("error in int Read_NetCDF_Location\n");
      return 0;
   }
   status = nc_get_var1_float( nc_id, lonid, index, lon);
   if(status != NC_NOERR){
      printf("error in int Read_NetCDF_Location\n");
      return 0;
   }
   status = nc_get_var1_float( nc_id, hgtid, index, hgt);
   if(status != NC_NOERR){
      printf("error in int Read_NetCDF_Location\n");
      return 0;
   }
   return 1;
}

/****************************************************************/
/* This reads a 1 Dimensional interger value from a NetCDF file */
/****************************************************************/
/* Input: nc_id - NetCDF file ID                                */
/*        recid - Record ID                                     */
/*        varid - Variable ID                                   */
/* Output: data - the integer data                              */
/****************************************************************/
int Read_1D_NetCDF_Var_Int_Data( int nc_id, int recid, int varid,
                                      int *data)
{
   static size_t index[1];
   int status;

   index[0] = recid;
   status = nc_get_var1_int( nc_id, varid, index, data);
   if (status != NC_NOERR){
      printf("error in int int Read_NetCDF_METAR_Var_int_Data\n");
      return 0;
   }
   return 1;
}


/***************************************************/
/* This reads the sounding data from a NetCDF file */
/***************************************************/
/* Input: nc_id - NetCDF file ID                   */
/*        recid - Record ID                        */
/*        varid - Variable ID                      */
/*        levels - the number of sounding levels   */
/* Output: data - sounding data                    */
/***************************************************/
int Read_Sounding_NetCDF_Var_Data( int nc_id, int recid, int varid,
                                   int levels, double *data)
{
   static size_t start[2], end[2];
   int i, status;

   start[0] = recid;
   start[1] = 0;
   end[0] = 1;
   end[1] = levels;
   status = nc_get_vara_double(nc_id, varid, start, end, data);
   if (status != NC_NOERR){
      printf("error in Read_Sounding_NetCDF_Var_Data\n");
      return 0;
   }
   return 1;
}


/****************************************************************/
/* This Reads a 1 Dimensional double value from a NetCDF file   */
/****************************************************************/
/* Input: nc_id - NetCDF file ID                                */
/*        recid - Record ID                                     */
/*        varid - Variable ID                                   */
/* Output: data - the double data                               */
/****************************************************************/
int Read_1D_NetCDF_Var_Double_Data( int nc_id, int recid, int varid,
                                      double *data)
{
   static size_t index[1];
   int status;

   index[0] = recid;
   status = nc_get_var1_double( nc_id, varid, index, data);
   if (status != NC_NOERR){
      printf("error in int int Read_NetCDF_METAR_Var_Double_Data\n");
      return 0;
   }
   return 1;
}

/****************************************************************/
/* This reads a 1 Dimensional string from a NetCDF file         */
/****************************************************************/
/* Input: nc_id - NetCDF file ID                                */
/*        recid - Record ID                                     */
/*        varid - Variable ID                                   */
/*        len - length of character string to be read           */
/* Output: data - char data                                     */
/****************************************************************/
int Read_1D_NetCDF_Var_Char_Data( int nc_id, int recid, int varid, 
                                     int len, char *data)
{
   size_t index[2];
   int i;
   int status;

   index[0] = recid;
   index[1] = 0;
   for (i=0; i<len; i++){
      status = nc_get_var1_text(nc_id, varid, index, &data[i]);
      if(status != NC_NOERR){
         printf("error in int int Read_NetCDF_METAR_Var_Char_Data\n");
         return 0;
      }
      index[1]++;
   }
   return 1;
}

/****************************************************************/
/* This reads a 2 Dimensional interger value froma NetCDF file  */
/****************************************************************/
/* Input: nc_id - NetCDF file ID                                */
/*        recid - Record ID                                     */
/*        varid - Variable ID                                   */
/*        other_dim - second index into the NetCDF data array   */
/* Output: data - the interger data                             */
/****************************************************************/
int Read_2D_NetCDF_Var_Int_Data( int nc_id, int recid, int varid,
                                      int other_dim, int *data)
{
   static size_t index[2];
   int status;

   index[0] = recid;
   index[1] = other_dim;
   status = nc_get_var1_int( nc_id, varid, index, data);
   if (status != NC_NOERR){
      printf("error in int int Read_NetCDF_METAR_Var_Int_Data\n");
      return 0;
   }
   return 1;
}

/****************************************************************/
/* This reads a 2 Dimensional double   value froma NetCDF file  */
/****************************************************************/
/* Input: nc_id - NetCDF file ID                                */
/*        recid - Record ID                                     */
/*        varid - Variable ID                                   */
/*        other_dim - second index into the NetCDF data array   */
/* Output: data - the double   data                             */
/****************************************************************/
int Read_2D_NetCDF_Var_Double_Data( int nc_id, int recid, int varid, 
                                      int other_dim, double *data)
{
   static size_t index[2];
   int status;

   index[0] = recid;
   index[1] = other_dim;
   status = nc_get_var1_double( nc_id, varid, index, data);
   if (status != NC_NOERR){
      printf("error in int int Read_NetCDF_METAR_Var_Double_Data\n");
      return 0;
   }
   return 1;
}

/****************************************************************/
/* This reads a 2 Dimensional string from a NetCDF file         */
/****************************************************************/
/* Input: nc_id - NetCDF file ID                                */
/*        recid - Record ID                                     */
/*        varid - Variable ID                                   */
/*        len - length of character string to be read           */
/*        other_dim - second index into the NetCDF data array   */
/* Output: data - char data                                     */
/****************************************************************/
int Read_2D_NetCDF_Var_Char_Data( int nc_id, int recid, int varid,
                                     int len, int other_dim, char *data)
{
   size_t index[3];
   int i;
   int status;

   index[0] = recid;
   index[1] = other_dim;
   index[2] = 0;
   for (i=0; i<len; i++){
      status = nc_get_var1_text(nc_id, varid, index, &data[i]);
      if(status != NC_NOERR){
         printf("error in int int Read_NetCDF_METAR_Var_Char_Data\n");
         return 0;
      }
      index[2]++;
   }
   return 1;
}
   

 
/************************************************************/
/* Get the NetCDF Variable ID from the NetCDF variable name */
/************************************************************/
/* Input: fid - NetCDF file ID                              */
/*        varname - variable name                           */
/* Ouput: varid - NetCDF variable ID associated with the    */
/*        desired variable name                             */
/************************************************************/
int Read_netCDF_Var_ID( int fid, char *varname, int *varid)
{
   int status;

   status = nc_inq_varid( fid, varname, varid);
   if (status == NC_ENOTVAR){
      *varid = -1;
      return 1;
   }
   else if (status != NC_NOERR){
      printf("error in Read_netCDF_METAR_Var_ID\n");
      return 0;
   }
   return 1;
}


/******************************************************************/
/* Get the fill or Missing values for a certain variable, this is */
/* useful for determining if a value should be disregarded or not */
/******************************************************************/
/* Input: fid - NetCDF file ID                                    */
/*        varid - NetCDF variable ID                              */
/* Output: fillvalue - Fill Value                                 */
/******************************************************************/
int Read_NetCDF_Fill( int fid, int varid, double *fillvalue)
{
   int status;

   status = nc_get_att_double( fid, varid, "_FillValue", fillvalue);
   if (status == NC_ENOTATT){
      *fillvalue = NC_FILL_DOUBLE;
   }
   else if (status != NC_NOERR){
      return 0;
   }
   return 1;
}
      

/*******************************************************************/
/* This will get the NetCDF record ID's for a specific time. These */
/* rec ID's indexed to [0..numrecs(time)] for future calls         */
/*******************************************************************/
/* Input: finfo - the NetCDF format info                           */
/*        nc_id - NetCDF file ID                                   */
/*        thetime - the NetCDF time, in seconds since              */
/*                  1970-01-01 00 UTC                              */
/* Output: ids - an array of rec IDs belonging to thetime          */
/*******************************************************************/
int Read_NetCDF_Record_IDs( NetCDF_Format_Info finfo,
                                  int nc_id, int thetime, int *ids)
{
   int  *times, status, stat_id;
   int tnid, i, idcount, numrecsid;
   size_t numrecs;


   /*************************/
   /* get number of records */
   /*************************/
   status = nc_inq_dimid(nc_id, finfo->REC_NUM, &numrecsid);
   if(status != NC_NOERR){
      return 0;
   }
   status = nc_inq_dimlen(nc_id, numrecsid, &numrecs);
   if(status != NC_NOERR){
      return 0;
   }

   /*************/
   /* get times */
   /*************/
   status = nc_inq_varid( nc_id, finfo->TIME, &tnid);
   if(status != NC_NOERR){
      return 0;
   }

   times = (int *) malloc(sizeof(int)*numrecs);
   if (!times){
      return 0;
   }

   status = nc_get_var_int( nc_id, tnid, times);
   if(status != NC_NOERR){
      free(times);
      return 0;
   }


   /**********************************************/
   /* get the station rec num  matching the time */
   /* assume no duplicates for now               */
   /**********************************************/
   idcount = 0;
   for (i = 0; i < numrecs; i++){
      if (times[i] == thetime){
         ids[idcount] = i;
         idcount++;
      }
   }
   /*
   *rpt = idcount;

   for (i = idcount; i < num_ids; i++){
      ids[i] = -1;
   }
   */
   free(times);
   return 1;
}

/*********************************************************/
/* This will determine how many time steps there are and */
/* how many record for each time step.                   */
/*********************************************************/
/* Input: finfo - the NetCDF format info                 */
/*        nc_id - the NetCDF file ID                     */
/* Output: num_times - the number of time steps for the  */
/*                     file                              */
/*         ts - array of time stamps,size [0..num_times] */
/*              in HHMMSS                                */
/*         ds - array of date stamps,size [0..num_times] */
/*              in YYDDD                                 */
/*         time - array of times, size [ 0..num_times]   */
/*                is seconds since 1970-01-01 00 UTC     */
/*         trecs - array containing number of records    */
/*                 per time step                         */
/*********************************************************/
int Read_NetCDF_Times_and_Recs( NetCDF_Format_Info finfo,
                                      int nc_id, int *num_times, int *ts, int *ds,
                                      int time[], int trecs[])
{
   int numrecsid;
   int nt, ntimes[MAXTIMES], uniquetimes[MAXTIMES];
   int numuniquetimes;
   int status, tnid, *times, temp, i, j, t;
   int id, year, day, hour, minute, second;
   size_t numrecs;
   int nrecs;

   *num_times = 0;

   /*************************/
   /* get number of records */
   /*************************/
   status = nc_inq_dimid(nc_id, finfo->REC_NUM, &numrecsid);
   if(status != NC_NOERR){
      return 0;
   }
   status = nc_inq_dimlen(nc_id, numrecsid, &numrecs);
   if(status != NC_NOERR){
      return 0;
   }


   /****************/
   /* get numtimes */
   /****************/
   for (i = 0; i < MAXTIMES; i++){
      uniquetimes[i] = -1;
   }

   status = nc_inq_varid( nc_id, finfo->TIME, &tnid);
   if(status != NC_NOERR){
      return 0;
   }

   times = (int *) malloc(sizeof(int)*numrecs);
   if (!times){
      return 0;
   }

   status = nc_get_var_int( nc_id, tnid, times);
   if(status != NC_NOERR){
      free(times);
      return 0;
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
   *num_times = numuniquetimes;
   nt = numuniquetimes;

   /***********************************************/
   /* get the largest number of record for a time */
   /***********************************************/
   /*
   nrecs = 0;
   for (i = 0; i < nt; i++){
      if (ntimes[i] > nrecs){
         nrecs = ntimes[i];
      }
   }
   *mrecs = nrecs; 
   */

   /****************************/   
   /* bubble sort unique times */
   /****************************/
   for (i = 0; i < numuniquetimes; i++){
      for (j = 0; j < numuniquetimes-1-i; j++){
         if (uniquetimes[j+1] < uniquetimes[j]){
            temp = uniquetimes[j];
            uniquetimes[j] = uniquetimes[j+1];
            uniquetimes[j+1] = temp;
            temp = ntimes[j];
            ntimes[j] = ntimes[j+1];
            ntimes[j+1] = temp;
         }
      }
   }
   for (i = 0; i < numuniquetimes; i++){
      trecs[i] = ntimes[i];
   } 

   /***********************/
   /* get day/time stamps */
   /***********************/
   for (i=0; i < nt; i++){
      t = uniquetimes[i];
      time[i] = t;
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
      ts[i] = 10000*hour+100*minute+second;
      ds[i] = 1000*year+day;
   }
   free(times);
   return 1;
}

/****************************************************/
/* This will get the number of levels from a NetCDF */
/* file containing 2D or sounding data              */
/****************************************************/
/* Input: finfo - the NetCDF format info            */
/*        nc_id - NetCDF file ID                    */
/* Output: numlevels - the number of levels each    */
/*                     record will have             */
/****************************************************/
int Read_NetCDF_Num_of_Levels( NetCDF_Format_Info finfo,
                              int nc_id, int *numlevels)
{
   int status;
   int levelid;
   size_t nl;

   status = nc_inq_dimid( nc_id, finfo->LEVELDIM, &levelid);
   if(status != NC_NOERR){
      return 0;
   }
   status = nc_inq_dimlen(nc_id, levelid, &nl);
   if(status != NC_NOERR){
      return 0;
   }
   *numlevels = (int) nl;
   return 1;
}

/****************************************************/
/* This will read the height for each of the levels */
/* in a 2D or sounding NetCDF file in meters        */
/****************************************************/
/* Input: finfo - the NetCDF format info            */
/*        nc_id - NetCDF file ID                    */
/*        recid - the record number of ID           */
/*        numlevels - the number of levels to get   */
/*                    the height for                */
/* Output: data - an array containing the height    */
/*                in meters for each level          */
/****************************************************/
int Read_NetCDF_Levels( NetCDF_Format_Info finfo,
                              int nc_id, int recid, int numlevels, float *data)
{
   int status;
   int levelid;
   static size_t tstart[2], tend[2];

   status = nc_inq_varid( nc_id, finfo->LEVELVAR, &levelid);
   if(status != NC_NOERR){
      return 0;
   }

   tstart[0] =  recid;
   tstart[1] =  0;
   tend[0] = 1;
   tend[1] =  numlevels;

   status = nc_get_vara_float(nc_id, levelid, tstart, tend, data);
   if(status != NC_NOERR){
      return 0;
   }
   return 1;
}

/****************************************************/
/* This will look through all the records in a      */
/* NetCDF file and determine the bounding region    */
/* which will be used to determine the vis5dboudries*/
/****************************************************/
/* Input: finfo - the NetCDF format info            */
/*        nc_id - NetCDF file ID                    */
/* Output: west, east, north, south - the horizontal*/
/*                boundries in degrees              */
/*         top, bottom - the vertical boundries in  */
/*                kilometers above sea level        */
/****************************************************/
int Read_NetCDF_Bounds( NetCDF_Format_Info finfo,
                              int nc_id, float *west, float *east,
                              float *north, float *south, float *top, float *bottom)
{
   int i, status, latid, lonid, hgtid, numrecsid;
   size_t numrecs;
   float nb, sb, wb, eb, tb, bb;
   float *latdata, *londata, *hgtdata;
   float fill_value;

   /*************************/
   /* get number of records */
   /*************************/
   status = nc_inq_dimid(nc_id, finfo->REC_NUM, &numrecsid);
   if(status != NC_NOERR){
      return 0;
   }
   status = nc_inq_dimlen(nc_id, numrecsid, &numrecs);
   if(status != NC_NOERR){
      return 0;
   }

   /*****************************/
   /* malloc lat, lon, hgt data */
   /*****************************/
   latdata = (float *) malloc(numrecs * sizeof(float));
   if (!latdata){
      printf("couldn't allocate enough memory\n");
      return 0;
   }
   londata = (float *) malloc(numrecs * sizeof(float));
   if (!londata){
      printf("couldn't allocate enough memory\n");
      free(latdata);
      return 0;
   }
   hgtdata = (float *) malloc(numrecs * sizeof(float));
   if (!hgtdata){
      printf("couldn't allocate enough memory\n");
      free(latdata);
      free(londata);
      return 0;
   }


   status = nc_inq_varid( nc_id, finfo->LAT, &latid);
   if(status != NC_NOERR){
      free(latdata);
      free(londata);
      free(hgtdata);
      return 0;
   }
   status = nc_inq_varid( nc_id, finfo->LON, &lonid);
   if(status != NC_NOERR){
      free(latdata);      
      free(londata);      
      free(hgtdata);      
      return 0;
   }
   status = nc_inq_varid( nc_id, finfo->HGT, &hgtid);
   if(status != NC_NOERR){
      free(latdata);      
      free(londata);      
      free(hgtdata);      
      return 0;
   }

   status = nc_get_att_float( nc_id, latid, finfo->LOCATION_FILL, &fill_value);
   if(status != NC_NOERR){
      free(latdata);      
      free(londata);      
      free(hgtdata);      
      return 0;
   }

   /************************************************/
   /* get the lat, lon, hgt data in one big chunck */
   /************************************************/
   status = nc_get_var_float( nc_id, latid, latdata);
   if(status != NC_NOERR){
      printf("error getting bounds\n");
      free(latdata);
      free(londata);
      free(hgtdata);
      return 0;
   }
   status = nc_get_var_float( nc_id, lonid, londata);
   if(status != NC_NOERR){
      printf("error getting bounds\n");
      free(latdata);
      free(londata);
      free(hgtdata);
      return 0;
   }
   status = nc_get_var_float( nc_id, hgtid, hgtdata);
   if(status != NC_NOERR){
      printf("error getting bounds\n");
      free(latdata);
      free(londata);
      free(hgtdata);
      return 0;
   }

   
   nb = -180.0;
   sb = 180.0;
   eb = -180.0;
   wb = 180.0;
   tb = -10000.0;
   bb =  10000.0;

   for (i = 0; i < numrecs; i++){
      if (latdata[i] != fill_value){
         if (latdata[i] > nb){
            nb = latdata[i];
         }
         if (latdata[i] < sb){
            sb = latdata[i];
         }
      }
   }

   for (i = 0; i < numrecs; i++){
      if (londata[i] != fill_value){
         if (londata[i] > eb){
            eb = londata[i];
         }
         if (londata[i] < wb){
            wb = londata[i];
         }
      }
   }
   for (i = 0; i < numrecs; i++){
      if (hgtdata[i] != fill_value){
         if (hgtdata[i] > tb){
            tb = hgtdata[i];
         }
         if (hgtdata[i] < bb){
            bb = hgtdata[i];
         }
      }
   }
   *west = -wb;
   *east = -eb;
   *north = nb;
   *south = sb;
   *top = tb/1000.0;
   *bottom = bb/1000.0;

   free(latdata);
   free(londata);
   free(hgtdata);

   return 1;
}
   
/****************************************************/
/* This will get information about the variables in */
/* a NetCDF file                                    */
/****************************************************/
/* Input: finfo - the NetCDF format info            */
/*        nc_id - NetCDF file ID                    */
/* Output: numvars - the number of variable         */
/*         varname - an array of the variable names */
/*         vartype - this array of size [0..numvars */
/*                   contains the variable type,    */
/*                   one of...                      */
/*                   CHAR_VAR                       */
/*                   INT_VAR                        */
/*                   FLOAT_VAR                      */
/*                   DOUBLE_VAR                     */
/*         vardim - this array of size [0..numvars] */
/*         contains the dimension of the variable   */
/*                  which is used to determine how  */
/*                  to read it from the NetCDF file */
/*                  later.  One of...               */
/*                   VAR_1D                         */
/*                   VAR_2D                         */
/*                   VAR_3D                         */
/*                   VAR_4D                         */
/*         charvarlength - this array of size       */
/*                [0..num_vars] contains the        */
/*                 length of the char string for    */
/*                 each of the variables.  If the   */
/*                 vartype is CHAR_VAR it will be   */
/*                 > 0 if vartype != CHAR_VAR then  */
/*                 it will be 0 length              */
/*         varmin, varmax - this array of size      */
/*               [0..num_vars] contains the         */
/*               variable min and max for each of   */
/*               the variables if vartype !=CHAR_VAR*/
/****************************************************/
int Read_NetCDF_Vars( NetCDF_Format_Info finfo,
                            int nc_id,  int *num_vars,
                            char varname[MAXVARS][MAX_VAR_LENGTH],
                            int vartype[], int vardim[], int charvarlength[],
                            double varmin[MAXVARS], double varmax[MAXVARS])
{
   int status, numvars, var_offset ;
   int id, numdims, cloud_layer_id;
   int numrecsid;
   int s, i, j, k, c, latid, lonid, hgtid;
   int dimids[10];
   nc_type type;
   size_t numrecs, stringsize, num_cloud_layers;
   char vname[1000];
   int levels;
   double *mmdata;
   double fillvalue;
   static size_t start[2], end[2];
 

   status = nc_inq_nvars( nc_id, &numvars);
   if(status != NC_NOERR){
      return 0;
   }

   /*************************/
   /* get number of records */
   /*************************/
   status = nc_inq_dimid(nc_id, finfo->REC_NUM, &numrecsid);
   if(status != NC_NOERR){
      return 0;
   }
   status = nc_inq_dimlen(nc_id, numrecsid, &numrecs);
   if(status != NC_NOERR){
      return 0;
   }
   /***********************************/
   /* allocate min max data array now */
   /***********************************/
   mmdata = (double *) malloc(numrecs * sizeof(double));
   if (!mmdata){
      printf("couldn't allocate min max data array\n");
      return 0;
   }


   /*****************************************************/
   /* check to make sure lat, lon and hgt are there     */
   /*****************************************************/
   status = nc_inq_varid( nc_id, finfo->LAT, &latid);
   if(status != NC_NOERR){
      printf("Could not find lat var\n");
      return 0;
   }
   status = nc_inq_varid( nc_id, finfo->LON, &lonid);
   if(status != NC_NOERR){
      printf("Could not find lon var\n");
      return 0;
   }
   status = nc_inq_varid( nc_id, finfo->HGT, &hgtid);
   if(status != NC_NOERR){
      printf("Could not find hgt var\n");
      return 0;
   }
   var_offset = 0;


   /******************************/
   /* get cloud layers if needed */
   /******************************/
   if (finfo == FSL_METAR || finfo == UNIDATA_METAR){
      num_cloud_layers = 0;
      cloud_layer_id = -1;
      status = nc_inq_dimid(nc_id, finfo->METAR_CLD_LAYERS, &cloud_layer_id);
      if(status != NC_NOERR){
         printf("no cloud_layers dimension\n");
      }
      else{
         status = nc_inq_dimlen(nc_id, cloud_layer_id, &num_cloud_layers);
         if(status != NC_NOERR){
            printf("no cloud_layers dimension\n");
            return 0;
         }
         if (num_cloud_layers < 1 ||
             num_cloud_layers > 9){
            printf("number of cloud layers must be between 1..9\n");
            return 0;
         }
      }
   }
 
   
   if (finfo == FSL_METAR || finfo == UNIDATA_METAR){
      for (i = 0; i < numvars; i++){
         k = i - var_offset;
         /****************/      
         /* get var name */
         /****************/
         status = nc_inq_varname( nc_id, i, vname);
         if(status != NC_NOERR){
            return 0;
         }
         /****************/      
         /* get var type */
         /****************/      
         status = nc_inq_vartype( nc_id, i, &type);
         if(status != NC_NOERR){
            return 0;
         }
         if (strncmp(finfo->LAT, vname, strlen(finfo->LAT)) != 0 &&
                  strncmp(finfo->LON, vname, strlen(finfo->LON)) != 0 &&
                  strncmp(finfo->HGT, vname, strlen(finfo->HGT)) != 0){
            if (strlen(vname) < MAX_VAR_LENGTH){
               strncpy(varname[k], vname, MAX_VAR_LENGTH);
               /********************************/            
               /* get number of var dimensions */
               /********************************/
               status = nc_inq_varndims( nc_id, i, &numdims);
               if(status != NC_NOERR){
                  printf("error getting numdims\n");
                  return 0;
               }
               /**************************/            
               /* get var dimension id's */
               /**************************/
               status = nc_inq_vardimid( nc_id, i, dimids);
               if(status != NC_NOERR){
                  printf("error getting dimids\n");
                  return 0;
               }
               /************/
               /* char var */
               /************/
               if (type == NC_CHAR){
                  /****************/
                  /* 3 dimensions */
                  /****************/
                  if (numdims == 3){
                     /*************************************/
                     /* Must be cloud layers, create vars */
                     /*************************************/
                     varname[k][strlen(varname[k])] = '0';
                     for (c = 1; c < num_cloud_layers; c++){
                        strncpy(varname[k+c], vname, MAX_VAR_LENGTH);
                        varname[k+c][strlen(varname[k+c])] = c + '0';
                        var_offset--;
                     }
                     status = nc_inq_dimlen( nc_id, dimids[2], &stringsize);
                     if(status != NC_NOERR){
                        printf("error getting stringsize\n");
                        return 0;
                     }
                     for (c = 0; c < num_cloud_layers; c++){
                        charvarlength[k+c] = stringsize;
                        vartype[k+c] = CHAR_VAR;
                        vardim[k+c] = VAR_2D;
                     }
                  }
                  /****************/
                  /* 2 dimensions */
                  /****************/
                  else if (numdims == 2){
                     status = nc_inq_dimlen( nc_id, dimids[1], &stringsize);
                     if(status != NC_NOERR){
                        printf("error getting stringsize\n");
                        return 0;
                     }
                     charvarlength[k] = (int) stringsize;
                     vartype[k] = CHAR_VAR;
                     vardim[k]  = VAR_1D;
                  }
                  /***************************/
                  /* 1, 4 or more dimensions */
                  /***************************/
                  else{
                     printf("don't know what to do with a char %d dimensional var\n", numdims);
                     return 0;
                  }
               }
               /***********/
               /* num var */
               /***********/
               else{
                  if (type == NC_BYTE ||
                      type == NC_SHORT ||
                      type == NC_INT){
                     vartype[k] = INT_VAR;
                  }
                  else if (type == NC_FLOAT){
                     vartype[k] = FLOAT_VAR;
                  }
                  else if (type == NC_DOUBLE){
                     vartype[k] = DOUBLE_VAR;
                  }
                  else{
                     printf("something wrong with the type\n");
                     exit(0);
                  }
                  /******************/
                  /* get fill value */
                  /******************/
                  status = nc_get_att_double( nc_id, i, finfo->LOCATION_FILL, &fillvalue);
                  if(status != NC_NOERR){
                     printf("ccouldn't get fill value\n");
                     return 0;
                  }       
                  /****************/
                  /* 2 dimensions */
                  /****************/
                  if (numdims == 2){
                     /************************/
                     /* must be cloud height */
                     /************************/
                     if (dimids[1] != cloud_layer_id){
                        printf("error with var second dimension\n");
                        return 0;
                     }
                     varname[k][strlen(varname[k])] = '0';
                     vardim[k] = VAR_2D;
                     for (c = 1; c < num_cloud_layers; c++){
                        strncpy(varname[k+c], vname, MAX_VAR_LENGTH);
                        varname[k+c][strlen(varname[k+c])] = c + '0';
                        var_offset--;
                        charvarlength[k+c] = 0;
                        vartype[k+c] = vartype[k];
                        vardim[k+c] = vardim[k];
                     }
                     /*******************/
                     /* get min and max */
                     /*******************/
                     for (c = 0; c < num_cloud_layers; c++){
                        for (s = 0; s < numrecs; s++){
                           start[0] = s;
                           start[1] = 0;
                           nc_get_var1_double( nc_id, i, start, mmdata+s);
                        }
                        get_min_and_max( numrecs, mmdata, fillvalue, &varmin[k+c],
                                         &varmax[k+c]);
                     }   
                  }
                  /****************/
                  /* 1 dimension  */
                  /****************/
                  else if (numdims == 1){ 
                     vardim[k] = VAR_1D;
                     charvarlength[k] = 0;
                     for (s = 0; s < numrecs; s++){
                        start[0] = s;
                        nc_get_var1_double( nc_id, i, start, mmdata+s);
                     }
                     get_min_and_max( numrecs, mmdata, fillvalue, &varmin[k],
                                      &varmax[k]);

                  }
                  /******************************/   
                  /* other number of dimesnions */
                  /******************************/
                  else{
                     printf("don't know what to do with an number %d dimesional var\n", numdims);
                     return 0;
                  }
               }
            }
            /******************************************/
            /* var is too long excluding the variable */
            /******************************************/
            else{
               printf("variable %s is too long, ommitting it\n", vname);
               var_offset++;
            }
         }
         else{
            var_offset++;
         }
      }
      *num_vars = k;
      return 1;
   }
   else if (finfo = FSL_PROFILE){
      for (i = 0; i < numvars; i++){
         k = i - var_offset;
         /****************/
         /* get var name */
         /****************/
         status = nc_inq_varname( nc_id, i, vname);
         if(status != NC_NOERR){
            return 0;
         }
         /****************/
         /* get var type */
         /****************/
         status = nc_inq_vartype( nc_id, i, &type);
         if(status != NC_NOERR){
            return 0;
         }
         if (strncmp(finfo->LAT, vname, strlen(finfo->LAT)) != 0 &&
               strncmp(finfo->LON, vname, strlen(finfo->LON)) != 0 &&
               strncmp(finfo->HGT, vname, strlen(finfo->HGT)) != 0 &&
               strncmp(finfo->LEVELVAR, vname, strlen(finfo->LEVELVAR)) != 0){
            if (strlen(vname) < MAX_VAR_LENGTH){
               strncpy(varname[k], vname, MAX_VAR_LENGTH);
               /********************************/
               /* get number of var dimensions */
               /********************************/
               status = nc_inq_varndims( nc_id, i, &numdims);
               if(status != NC_NOERR){
                  printf("error getting numdims\n");
                  return 0;
               }
               /**************************/
               /* get var dimension id's */
               /**************************/
               status = nc_inq_vardimid( nc_id, i, dimids);
               if(status != NC_NOERR){
                  printf("error getting dimids\n");
                  return 0;
               }
               /************/
               /* char var */
               /************/
               if (type == NC_CHAR){
                  /****************/
                  /* 2 dimensions */
                  /****************/
                  if (numdims == 2){
                     status = nc_inq_dimlen( nc_id, dimids[1], &stringsize);
                     if(status != NC_NOERR){
                        printf("error getting stringsize\n");
                        return 0;
                     }
                     charvarlength[k] = (int) stringsize;
                     vartype[k] = CHAR_VAR;
                     vardim[k]  = VAR_1D;
                  }
                  /******************************/
                  /* 1, 3, 4 or more dimensions */
                  /******************************/
                  else{
                     printf("don't know what to do with a char %d dimensional var\n", numdims);
                     return 0;
                  }
               }
               /***********/
               /* num var */
               /***********/
               else{
                  if (type == NC_BYTE ||
                      type == NC_SHORT ||
                      type == NC_INT){
                     vartype[k] = INT_VAR;
                  }
                  else if (type == NC_FLOAT){
                     vartype[k] = FLOAT_VAR;
                  }
                  else if (type == NC_DOUBLE){
                     vartype[k] = DOUBLE_VAR;
                  }
                  else{
                     printf("something wrong with the type\n");
                     exit(0);
                  }
                  /****************/
                  /* 2 dimensions */
                  /****************/
                  if (numdims == 2){
                     /*******************************/
                     /* must be a sounding data var */
                     /*******************************/
                    vardim[k] = VAR_2D;
                    charvarlength[k] = 0;
                  }
                  /****************/
                  /* 1 dimension  */
                  /****************/
                  else if (numdims == 1){
                    vardim[k] = VAR_1D;
                    charvarlength[k] = 0;
                  }
                  /******************************/
                  /* other number of dimesnions */
                  /******************************/
                  else{
                     printf("don't know what to do with an number %d dimesional var\n", numdims);
                     return 0;
                  }
               }
            }
            /******************************************/
            /* var is too long excluding the variable */
            /******************************************/
            else{
               printf("variable %s is too long, ommitting it\n", vname);
               var_offset++;
            }
         }
         else{
            var_offset++;
         }
      }
      *num_vars = k;
      free(mmdata);
      return 1;
   }
}
