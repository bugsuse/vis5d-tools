/* file.c */
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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "file.h"
#include "igui.h"

#define MAX_FDBS 99

static FileDB *fdb_table = NULL;
static void init_file_db( FileDB fdb);


/***********************************/
/* This will initialize the fdb or */
/* File Data Base                  */
/***********************************/
int fdb_initialize( void )
{
   int i;
  
   fdb_table = malloc( sizeof(FileDB *) * MAX_FDBS );
   for (i = 0; i < MAX_FDBS; i++){
      fdb_table[i] = NULL;
   }

   Initialize_NetCDF_Format_Info();

   return 1;
}

/*****************************************/
/* This will return an index to the next */
/* available fdb from the table          */
/*****************************************/
/* Output: returns the index of next file*/
/*         database or -1 if not one     */
/*****************************************/
int alloc_fdb( void )
{
   int i;
  
   for (i = 0; i < MAX_FDBS; i++){
      if (fdb_table[i] == NULL){
         return i;
      }
   }
   return -1;
}



/*********************************************/
/* This will allocate one fdb structure and  */
/* initialize it.  It will exit if not enough*/
/* memory availabe.                          */
/*********************************************/
/* Ouput: return pointer to a fdb structure  */
/*********************************************/
FileDB new_fdb( void )
{
   FileDB fdb;
   fdb = (FileDB) calloc( 1, sizeof(struct file_db) );
   if (!fdb){
      printf("error in new_fdb\n");
      exit(0);
   }
   else{
      init_file_db( fdb );
   }
   return fdb;
}



/**********************************************/
/* This will return an iniialized and indexed */
/* fdb(file database) ready to use.           */
/**********************************************/
/* Output: return pointer to fdb, this is     */
/*         called from 'imain.c'              */
/**********************************************/
FileDB make_new_fdb( void )
{
   int index;
   FileDB fdb;

   index = alloc_fdb();
   if (index < 0){
      printf("Could not make new fdb\n");
      exit(0);
   }
   fdb = fdb_table[index] = new_fdb();
   fdb->index = index;
   return fdb;
}



/*****************************************************/
/* This will return the desired fdb, or file database*/
/*****************************************************/
/* Input: index - index to desired fdb               */
/* Output: return pointer to fdb                     */
/*****************************************************/
FileDB get_fdb( int index )
{
   if (index<0 || index >= MAX_FDBS){
      printf("error in get_fdb\n");
      exit(0);
   }
   else{
      return fdb_table[index];
   }
}


   
/*************************************************/
/* allocate the file_info structure, exit if not */
/* enough memory                                 */
/*************************************************/
struct file_info *alloc_file_info( void )
{
   struct file_info *yo;

   yo = (struct file_info *) calloc(1, sizeof(struct file_info));
   if (!yo){
      printf(" could not allocate file_info\n");
      exit(0);
   }
   return yo;
}



/*******************************************/
/* Free the file info and free the RecID's */
/*******************************************/
/* Input: finfo - pointer to the file_info */
/*                struct                   */
/*******************************************/
void free_file_info( struct file_info *finfo)
{
   int i;
   for (i = 0; i < finfo->NumTimes; i++){
      free(finfo->RecID[i]);
   }
   free( finfo); 
}



/**********************************/
/* Init certain values of the fdb */
/**********************************/
/* Input: fdb - pointer to fdb    */
/**********************************/
static void init_file_db( FileDB fdb)
{
   fdb->NumFiles = 0;
   fdb->NumTimes = 0;
   fdb->NumVars  = 0;
   fdb->Levels   = 0;
   fdb->WhichFileOpen = -1;
}


/**************************************************/
/* Remove a file from a fdb and do the required   */
/* functions such as re-determining the variables */
/* or getting new boundries                       */
/**************************************************/
/* Input: fdb - pointer to the fdb                */
/*        file_index - file index indicating which*/
/*                     file to be removed         */
/**************************************************/
int remove_a_file( FileDB fdb, int file_index)
{
   struct file_info *f;
   int i, j, k, tt, td, save_it, throw;
   int nrs;

   f = fdb->File[file_index];

   /*****************************************/
   /* get the appropriate time removed also */
   /*****************************************/
   /*******************************************/
   /* first see if it's need, some other file */
   /* might have the same time step           */
   /*******************************************/
   for (i = 0; i < f->NumTimes; i++){
      td = f->DateStamp[i];
      tt = f->TimeStamp[i];
      save_it = 0;
      for (j = 0; j < fdb->NumFiles && !save_it; j++){
         if ( j != file_index ){
            for (k = 0; k < fdb->File[j]->NumTimes && !save_it; k++){
               if ( td == fdb->File[j]->DateStamp[k] &&
                    tt == fdb->File[j]->TimeStamp[k]){
                  save_it = 1;
               }
            }
         }
      }
      /******************************************/
      /* find out which one it is and chuck it! */
      /******************************************/
      throw = -1;
      for (j = 0; j < fdb->NumTimes; j++){
         if (fdb->DateStamp[j] == td &&
             fdb->TimeStamp[j] == tt){
            throw = j;
            j = fdb->NumTimes;
         }
      }
      if (throw == -1){
         /* something majorly wront here */
         exit(0);
      }

      if (!save_it){
         if (throw == fdb->NumTimes-1){
            fdb->DateStamp[throw] = 0;
            fdb->TimeStamp[throw] = 0;
            fdb->TimeSeconds[throw] = 0;
            fdb->NumRecs[throw] = 0;
            fdb->NumTimes--;
         }
         else{
            for (j = throw+1; j < fdb->NumTimes; j++){
               fdb->DateStamp[j-1] = fdb->DateStamp[j];
               fdb->TimeStamp[j-1] = fdb->TimeStamp[j];
               fdb->TimeSeconds[j-1] = fdb->TimeSeconds[j];
               fdb->TimeSelected[j-1] = fdb->TimeSelected[j];
               fdb->NumRecs[j-1] = fdb->NumRecs[j];
            }
            fdb->DateStamp[j-1] = 0;
            fdb->TimeStamp[j-1] = 0;
            fdb->TimeSelected[j-1] = 0;
            fdb->TimeSeconds[j-1] = 0;
            fdb->NumRecs[j-1] = 0;
            fdb->NumTimes--;
         }
      }
      else{
         fdb->NumRecs[throw] -= f->NumRecs[i];
      }
   }
         
   /********************************************/
   /* now get rid of the appropriate variables */
   /********************************************/
   for (i = 0; i < f->NumVars; i++){
      for (j = 0; j < fdb->NumFiles && !save_it; j++){
         if (j != file_index){
            for (k = 0; k < fdb->NumVars && !save_it; k++){
               if (strcmp(f->VarName[i], fdb->VarName[j])==0){  
                  save_it = 1;
               }
            }
         }
      }
      throw = -1;


      if (!save_it){
         for (j=0; j < fdb->NumVars; j++){
            if (strcmp(f->VarName[i], fdb->VarName[j])==0){
               throw = j;
               j = fdb->NumVars;
            }
         }
         if (throw == -1){
            /* something majorly wront here */
            exit(0);
         }
         if (throw == fdb->NumVars-1){
            fdb->VarName[throw][0] = 0;
            fdb->VarSelected[throw] = 0;
            fdb->VarType[throw] = 0;
            fdb->VarDimensions[throw] = 0; 
            fdb->CharVarLength[throw] = 0;
            fdb->NumVars--;
         }
         else{
            for (j = throw+1; j < fdb->NumVars; j++){
               strcpy(fdb->VarName[j-1], fdb->VarName[j]);
               fdb->VarSelected[j-1] = fdb->VarSelected[j];
            }
            fdb->VarName[j-1][0] = 0;
            fdb->VarSelected[j-1] = 0;
            fdb->NumVars--;
         }
      }
   }
 


   /***************************************/
   /* now free it from the file data base */
   /***************************************/
   free_file_info( fdb->File[file_index] );
   fdb->File[file_index] = NULL;
      
   if (file_index == fdb->NumFiles-1){
      fdb->NumFiles--;
   }
   else{
      for (i = file_index+1; i < fdb->NumFiles; i++){
         fdb->File[i-1] = fdb->File[i];
      }
      fdb->File[i-1] = NULL;
      fdb->NumFiles--;
   }
  
   /*************************************/
   /* Get the correct number of Records */
   /*************************************/
   /*
   nrs = 0;
   for (i = 0; i < fdb->NumFiles; i++){
      if (fdb->File[i]->NumRecs > nrs){
         nrs = fdb->File[i]->NumRecs;
      }
   }
   fdb->NumRecs = nrs;
   */

   /************************************/
   /* Get the correct number of Levels */
   /************************************/
   {
      int lnl = 0;
      for (i = 0; i < fdb->NumFiles; i++){
         if (fdb->File[i]->Levels > lnl){
            lnl = fdb->File[i]->Levels;
         }
      }
      fdb->Levels = lnl;
   }


   /***********************************/   
   /* Get the largest bounding region */
   /***********************************/
   if (fdb->NumFiles == 1){
      fdb->NorthBound = fdb->File[0]->NorthBound;
      fdb->SouthBound = fdb->File[0]->SouthBound;
      fdb->EastBound = fdb->File[0]->EastBound;
      fdb->WestBound = fdb->File[0]->WestBound;
      fdb->TopBound = fdb->File[0]->TopBound;
      fdb->BottomBound = fdb->File[0]->BottomBound;
   }
   else{
      float tb, bb, sb, nb, eb, wb;
      tb = fdb->File[0]->TopBound;
      bb = fdb->File[0]->BottomBound;
      eb = fdb->File[0]->EastBound;
      wb = fdb->File[0]->WestBound;
      nb = fdb->File[0]->NorthBound;
      sb = fdb->File[0]->SouthBound;
      for (i = 1; i < fdb->NumFiles; i++){
         if (fdb->File[i]->TopBound > tb){
            tb = fdb->File[i]->TopBound;
         }
         if (fdb->File[i]->BottomBound < bb){
            bb = fdb->File[i]->BottomBound;
         }
         if (fdb->File[i]->WestBound > wb){
            wb = fdb->File[i]->WestBound;
         }
         if (fdb->File[i]->EastBound < eb){
            eb = fdb->File[i]->EastBound;
         }
         if (fdb->File[i]->NorthBound > nb){
            nb = fdb->File[i]->NorthBound;
         }
         if (fdb->File[i]->SouthBound < sb){
            sb = fdb->File[i]->SouthBound;
         }
      }
      fdb->NorthBound = nb;
      fdb->SouthBound = sb;
      fdb->WestBound = wb;
      fdb->EastBound = eb;
      fdb->TopBound = tb;
      fdb->BottomBound = bb;
   }
   return 1;
}

       

/***************************************/
/* Add a file and it's info to the fdb */
/***************************************/
/* Input: fdb - file database to add   */
/*              file too               */
/*        filename - filename of NetCDF*/
/*             to add to fdb           */
/***************************************/
int add_a_file( FileDB fdb, char *filename)
{
   int  num_times;
   int ds[MAXTIMES], ts[MAXTIMES];
   int fileid, i, j, k, s, cur_file;
   struct file_info *F;
   char fln[500];
   int num_recs[MAXTIMES], dont_add_it, num_vars;
   char var_name[MAXVARS][MAX_VAR_LENGTH];
   int var_type[MAXVARS];
   int var_dim[MAXVARS];
   int char_var_length[MAXVARS];
   float topbound, bottombound, eastbound, westbound;
   float northbound, southbound;
   int *recids[MAXTIMES];
   int recs_per_time[MAXTIMES], time[MAXTIMES];
   int general_type, specific_type; 
   NetCDF_Format_Info finfo;
   int levels = 0;
   double varmin[MAXVARS], varmax[MAXVARS];

   strcpy(fln, filename);
   /* make sure not a file already named this */
   for (i = 0; i < fdb->NumFiles; i++){
      if (strcmp(fln, fdb->File[i]->FileName) == 0){
         return 0;
      }
   }
printf("adding file %s\n", filename);
   /************************/
   /* get file type if any */
   /************************/
   if (!Read_NetCDF( filename, &general_type, &specific_type, &fileid)){
      return 0;
   }
   if (fdb->NumFiles==0){
      fdb->FileType = general_type;
   }
   else if ( general_type != fdb->FileType){
      Close_NetCDF( fileid );                               
      return 0;
   }
   if (specific_type == fsl_netcdf_metar){
      finfo = FSL_METAR;
   }
   else if (specific_type == unidata_netcdf_metar){
      finfo = UNIDATA_METAR;
   }
   else if (specific_type == fsl_netcdf_profile){
      finfo = FSL_PROFILE;
   }
   else{
      printf("Dont't know that type\n");
      return 0;
   }


   /*******************************/   
   /* get times and record number */
   /********************************************/
   if (!Read_NetCDF_Times_and_Recs( finfo, 
                        fileid, &num_times, ts, ds, time, num_recs)){
      Close_NetCDF( fileid );
      return 0;
   }

  
 
   /******************/   
   /* get record IDs */
   /******************/
   for (i = 0; i < num_times; i++){
      recids[i] =  (int *) malloc(sizeof(int)*num_recs[i]);
      if (!recids){
         printf("Error could not allocate memory for record ID's\n");
         exit(0);
      }
      memset(recids[i], -1, sizeof(recids[i]));
   }
   for (i = 0; i < num_times; i++){
      if (!Read_NetCDF_Record_IDs( finfo, fileid,  
                    time[i], recids[i])){
         Close_NetCDF( fileid );
         for (j = 0; j < num_times; j++){
            free(recids[j]);
         }
         return 0;
      }
   }

   
   /************/   
   /* get vars */
   /************/
   if (!Read_NetCDF_Vars( finfo, fileid, &num_vars, var_name,
                                var_type, var_dim, char_var_length,
                                varmin, varmax)){
      Close_NetCDF( fileid );
      for (i = 0; i < num_times; i++){
         free(recids[i]);
      }
      return 0;
   }
   

   /************************/
   /* get levels if needed */
   /************************/
   if (general_type == NETCDF_SOUNDING){
      if (!Read_NetCDF_Num_of_Levels( finfo, fileid, &levels)){
         Close_NetCDF( fileid );
         for (i = 0; i < num_times; i++){
            free(recids[i]);
         }
         return 0;
      }
   }


   /**************/   
   /* get bounds */
   /**************/
   if (!Read_NetCDF_Bounds( finfo, fileid, &westbound, &eastbound,
                                  &northbound, &southbound, &topbound, &bottombound)){
      Close_NetCDF( fileid );                               
      for (i = 0; i < num_times; i++){
         free(recids[i]);
      }
      return 0;
   }


   if (!Close_NetCDF( fileid )){
      for (i = 0; i < num_times; i++){
         free(recids[i]);
      }
      return 0;
   }

 
   F = alloc_file_info();
   strcpy(F->FileName, fln);
   for (i = 0; i < num_times; i++){
      F->RecID[i] = (int *) malloc(sizeof(int)*num_recs[i]);
      if (!F->RecID){
         printf("Error in allocating RecID\n");
         return 0;
      }
   }

   F->FileID = fileid;
   F->FileType = specific_type;
   F->Finfo = finfo;
   /**************************/
   /* add vars tp file_info */
   /**************************/
   F->NumVars = num_vars;
   for( i = 0; i < num_vars; i++){
      strcpy(F->VarName[i], var_name[i]);
      F->VarType[i] = var_type[i];
      F->CharVarLength[i] = char_var_length[i];
      F->VarSelected[i] = 1;
      F->VarDimensions[i] = var_dim[i];
      F->VarMin[i] = varmin[i];
      F->VarMax[i] = varmax[i];
   }
    
   /**************/
   /* add levels */
   /**************/
   F->Levels = levels;

 
   /**************************/
   /* add times to file_info */
   /**************************/
   F->NumTimes = num_times;
   for (i = 0; i < num_times; i++){
      F->DateStamp[i] = ds[i];
      F->TimeStamp[i] = ts[i];
      F->TimeSeconds[i] = time[i];
      F->TimeSelected[i] = 1;
      F->NumRecs[i] = num_recs[i];
   }

   /*****************************/
   /* add number of record-info */
   /*****************************/
   /*
   F->NumRecs = num_recs;
   */

   /***************/
   /* add rec ids */
   /***************/
   for (i = 0; i < num_times; i++){
      for(j = 0; j < num_recs[i]; j++){
         F->RecID[i][j] = recids[i][j];
      }
      free(recids[i]);
   }


   /***************************/
   /* add bounds to file_info */
   /***************************/
   F->NorthBound = northbound;
   F->SouthBound = southbound;
   F->EastBound = eastbound;
   F->WestBound = westbound;
   F->TopBound = topbound;
   F->BottomBound = bottombound;


   /*******************************************************/
   /* put time into year, month, day, hour, min, secs for */
   /* my own sanity right now, probably won't need it     */
   /*******************************************************/
   /*
   for (i = 0; i < num_times; i++){
      static int dds[24] = {31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365,
                         31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366};
      int iy, im, id, days, day, mon, ij;
      iy = (int)(ds[i] / 1000);
      id = ds[i] - (iy*1000);
      iy += 1900;
      im = (iy % 4) == 0 ? 12 : 0;
      for (ij=im; ij<im+12; ij++) {
        if (id <= dds[ij]) {
          mon = ij-im;
          if (mon > 0) id = id - dds[ij-1];
          break;
        }
      }
      mon++;
      F->Year[i] = iy;
      F->Month[i] = mon;
      F->Day[i] = id;
      F->Hour[i] = (int)(ts[i]/10000);
      F->Minute[i] = (int)((ts[i] - (F->Hour[i] * 10000)) / 100);
      F->Second[i] = ts[i] - ((int)(ts[i] / 100)*100);
   }
   */


   /**************************************/   
   /* attach file_info to file data base */
   /**************************************/
   fdb->File[fdb->NumFiles] = F;
   fdb->NumFiles++;


   /*******************************************************/
   /* add vars to file data base make sure no duplicates */
   /*******************************************************/
   for (j = 0; j < F->NumVars; j++){
      dont_add_it = 0;
      for (i = 0; i < fdb->NumVars && !dont_add_it; i++){
         if (strcmp(F->VarName[j],fdb->VarName[i])==0){
            dont_add_it = 1;
            if (F->VarMin[j] < fdb->VarMin[i]){
               fdb->VarMin[i] = F->VarMin[j];
            }
            if (F->VarMax[j] > fdb->VarMax[i]){
               fdb->VarMax[i] = F->VarMax[j];
            }
         }
      }
      if (!dont_add_it){
         strcpy(fdb->VarName[fdb->NumVars], F->VarName[j]);
         fdb->VarSelected[fdb->NumVars] = 1; 
         fdb->VarType[fdb->NumVars] = F->VarType[j];
         fdb->VarDimensions[fdb->NumVars] = F->VarDimensions[j];
         fdb->CharVarLength[fdb->NumVars] = F->CharVarLength[j];
         fdb->VarMin[fdb->NumVars] = F->VarMin[j];
         fdb->VarMax[fdb->NumVars] = F->VarMax[j];
         fdb->NumVars++;
      }
   }
               



   /*******************************************************
   /* add times to file data base make sure no duplicates */
   /*******************************************************/
   for (j = 0; j < F->NumTimes; j++){
      int d, t, y, m, dd, hh, mm, ss;
      for (i = 0;   (i < fdb->NumTimes && 
           ((F->DateStamp[j] > fdb->DateStamp[i]) ||
           (F->DateStamp[j] == fdb->DateStamp[i] &&
            F->TimeStamp[j] > fdb->TimeStamp[i]))); i++){
      }
      if (F->DateStamp[j] != fdb->DateStamp[i]  || 
          F->TimeStamp[j] != fdb->TimeStamp[i]){
         for (k = fdb->NumTimes; k > i; k--){
            fdb->DateStamp[k] = fdb->DateStamp[k-1];  
            fdb->TimeStamp[k] = fdb->TimeStamp[k-1];
            fdb->TimeSelected[k] = fdb->TimeSelected[k-1];
            fdb->TimeSeconds[k] = fdb->TimeSeconds[k-1];
         }
         fdb->DateStamp[i] = F->DateStamp[j];
         fdb->TimeStamp[i] = F->TimeStamp[j];
         fdb->TimeSeconds[i] = F->TimeSeconds[j];
         fdb->TimeSelected[i] = F->TimeSelected[j];
         fdb->NumTimes++;
      }
      F->FDB_to_F_timestep[i] = j;
   }

   /*************************************/
   /* add num recs info to data base    */
   /* these have to be redone everytime */
   /* a new file is added so the numrecs*/
   /* dont't become mismatched with the */
   /* appropriate time                  */
   /*************************************/
   for (i = 0; i < fdb->NumTimes; i++){
      fdb->NumRecs[i] = 0;
      for (j = 0; j < fdb->NumFiles; j++){
         for (k = 0; k < fdb->File[j]->NumTimes; k++){
            if (fdb->File[j]->TimeSeconds[k] ==
                                    fdb->TimeSeconds[i]){
               fdb->NumRecs[i] += fdb->File[j]->NumRecs[k];
            }
         }
      }
   }          

   /***********************************************************/
   /* add bounds to file data base picking the largest domain */
   /***********************************************************/
   if (fdb->NumFiles == 1){
      fdb->NorthBound = F->NorthBound;
      fdb->SouthBound = F->SouthBound;
      fdb->EastBound = F->EastBound;
      fdb->WestBound = F->WestBound;
      fdb->TopBound = F->TopBound;
      fdb->BottomBound = F->BottomBound;
   }
   else{
      if (F->NorthBound > fdb->NorthBound){
         fdb->NorthBound = F->NorthBound;
      }
      if (F->SouthBound < fdb->SouthBound){
         fdb->SouthBound = F->SouthBound;
      }
      if (F->WestBound > fdb->WestBound){
         fdb->WestBound = F->WestBound;
      }
      if (F->EastBound < fdb->EastBound){
         fdb->EastBound = F->EastBound;
      }
      if (F->TopBound > fdb->TopBound){
         fdb->TopBound = F->TopBound;
      }
      if (F->BottomBound < fdb->BottomBound){
         fdb->BottomBound = F->BottomBound;
      }
   }

   /**********************/
   /* add levels too fdb */
   /**********************/
   if (F->Levels > fdb->Levels){
      fdb->Levels = F->Levels;
   } 
   return 1;
}

/*********************************************************/
/* This will read the geographics data for one record    */
/*********************************************************/
/* Input: index - index of the fdb, file database to use */
/*        time - time step of desired record             */
/*        record - desired record for location           */
/* Output: lat, lon, hgt - geo postion of record         */
/*********************************************************/
int read_fdb_record_geo_data( int index,  int time,
                              int record, float *lat, float *lon, float *hgt)
{
   FileDB fdb;
   int whichfile, whichfiletime, current_fileid, whichrec;
  
   fdb = get_fdb(index);
   
   whichrec  = fdb->RecIDfilerecid[time][record];
   if (whichrec == -1){
      *lat = MISSING;
      *lon = MISSING;
      *hgt = MISSING;
      return 1;
   }

   whichfile = fdb->RecIDfileindex[time][record];
   whichfiletime = fdb->File[whichfile]->FDB_to_F_timestep[time];

   /***********************************************/
   /* check to see if the file needs to be opened */
   /***********************************************/
   if (fdb->WhichFileOpen != whichfile){
      if (fdb->WhichFileOpen >= 0){
         if (!Close_NetCDF( fdb->File[fdb->WhichFileOpen]->FileID)){
            printf("could not close file\n");
            return 0;
         }
      }
      if (!Open_NetCDF( fdb->File[whichfile]->FileName, &current_fileid)){
         printf("error in read_fdb_records\n");
         return 0;
      }
      fdb->WhichFileOpen = whichfile;
   }
   else{
      current_fileid = fdb->File[fdb->WhichFileOpen]->FileID;
   }

   /*********************/
   /* get location data */
   /*********************/
   if (!Read_NetCDF_Location( fdb->File[whichfile]->Finfo, current_fileid,
        whichrec, lat, lon, hgt)){
      printf("could not read location data\n");
      return 0;
   }
   
   return 1;
}




/****************************************************************/
/* This will read all the data from a record.  It's used by the */
/* irregular_v5d.c calls                                        */
/****************************************************************/
/* Input: index - index into the fdb being used                 */
/*        iv - pointer to the irregular v5d sturct being used   */
/*             the reason why info from the file database is not*/
/*             used is becuase not all times or variable may be */
/*             included from the file database, and the irregular*/
/*             v5d struct contains the desired or chosen vars and*/
/*             times.                                           */
/*        time - the time step for the desired record           */
/*        record - the record to get the data from              */
/*        fdata - array to store the number data in, it will    */
/*                always be of size [0..NumRecs(time)]          */
/*        sdata - array to store the sounding number data in, it*/
/*               will be of size [0..NumLevels * NumSoundingVars]*/
/*               or NULL if not sounding data                   */
/*        cdata - array to store the var string data in, it will*/
/*                be of size [0..VarCharLengh[var]*var], or NULL*/
/*                if no char data                               */
/*        ldata - array to store the level height info it will  */
/*                be size of [0..NumLevels] or NULL if no       */
/*                sounding data                                 */
/****************************************************************/
int read_fdb_record( int index, irregular_v5dstruct *iv, int time,
                      int record, 
                      double *fdata, double *sdata, char *cdata, float *ldata)
{
   int i, k, j, t, n, z;
   FileDB fdb;
   double fillvalue;
   int  whichvarid, fileid, whichfile, whichrec, whichfiletime;
   static int current_fileid = -1;
   static int current_rec_count = 1;

   fdb = get_fdb(index);

   whichrec  = fdb->RecIDfilerecid[time][record];
   if (whichrec == -1){
      for (i = 0; i < iv->NumVars; i++){
         fdata[i] = MISSING;
         cdata[iv->CharPointer[i]] = 0;
         if(iv->Type == SOUNDING){
            for (j = iv->SoundingPointer[i];
                   j < iv->SoundingPointer[i]+iv->Levels;
                   j++){
               sdata[j] = MISSING;
            }
         }
      }
      return 1;
   }

   whichfile = fdb->RecIDfileindex[time][record];
   whichfiletime = fdb->File[whichfile]->FDB_to_F_timestep[time];

   /***********************************************/
   /* check to see if the file needs to be opened */
   /***********************************************/
   if (fdb->WhichFileOpen != whichfile){
      if (fdb->WhichFileOpen >= 0){
         if (!Close_NetCDF( fdb->File[fdb->WhichFileOpen]->FileID)){
            printf("could not close file\n");
            return 0;
         }
      }
      if (!Open_NetCDF( fdb->File[whichfile]->FileName, &current_fileid)){
         printf("error in read_fdb_records\n");
         return 0;
      }
      fdb->WhichFileOpen = whichfile;
   }
   else{
      current_fileid = fdb->File[fdb->WhichFileOpen]->FileID;
   }

   /******************************/
   /* get level info if SOUNDING */
   /******************************/
   if (iv->Type == SOUNDING){
      if (!Read_NetCDF_Levels( fdb->File[whichfile]->Finfo, current_fileid,
            whichrec, iv->Levels, ldata)){
         printf("couldn't get level data \n");
         return 0;
      }
   }

   i = -1;
   for( k = 0; k < fdb->NumVars; k++){
      if (fdb->VarSelected[k]){
         whichvarid = fdb->File[whichfile]->VarID[k];
         fillvalue = fdb->File[whichfile]->VarFillValue[k];
         i++;
         /********************************/
         /* set it missing is varid = -1 */
         /********************************/
         if (whichvarid < 0){
            if (fdb->VarType[k] == CHAR_VAR){
               cdata[iv->CharPointer[i]] = 0;
            }
            else if (iv->Type == SOUNDING &&
                     fdb->VarDimensions[k] == VAR_2D){
               for (z = iv->SoundingPointer[i];
                    z < iv->SoundingPointer[i] + iv->Levels;
                    z++){
                  sdata[z] = MISSING;
               }
            }
            else{
               fdata[i] = MISSING;
            }
         } 
         /*********************/            
         /* get sounding data */
         /*********************/
         else if (iv->Type == SOUNDING && fdb->VarType[k] != CHAR_VAR &&
                  fdb->VarDimensions[k] == VAR_2D){
            if (!Read_Sounding_NetCDF_Var_Data(current_fileid, 
                   whichrec, whichvarid, iv->Levels, 
                   (sdata+iv->SoundingPointer[i]))){
               printf("error in read_fdb_records2.1\n");
               return 0;
            }
         }
         /**********************/         
         /* get 1D float data  */
         /**********************/
         else if ((fdb->VarType[k] == FLOAT_VAR || fdb->VarType[k] == DOUBLE_VAR)
              && fdb->VarDimensions[k] == VAR_1D){
            if (!Read_1D_NetCDF_Var_Double_Data( current_fileid,
                 whichrec, whichvarid, &fdata[i])){
               printf("error in read_fdb_records\n");
               return 0;
            }
         }
         /********************/         
         /* get 1D char data */
         /********************/         
         else if (fdb->VarType[k] == CHAR_VAR && fdb->VarDimensions[k] == VAR_1D){
            if (!Read_1D_NetCDF_Var_Char_Data( current_fileid,
                 whichrec, whichvarid, iv->CharVarLength[i],
                 (cdata+iv->CharPointer[i]))){
               printf("error in read_fdb_records2\n");
            }
         }
         /*********************/
         /* get 2D float data */
         /*********************/
         else if (fdb->VarType[k] == FLOAT_VAR && fdb->VarDimensions[k] == VAR_2D){
            int other_dim;
            other_dim = iv->VarName[i][strlen(iv->VarName[i])-1] - '0';
            if (!Read_2D_NetCDF_Var_Double_Data( current_fileid,
                 whichrec, whichvarid, other_dim, &fdata[i])){
               printf("error in read_fdb_records\n");
               return 0;
            }
         }
         /*********************/         
         /* get 2D char data  */
         /*********************/
         else if (fdb->VarType[k] == CHAR_VAR && fdb->VarDimensions[k] == VAR_2D){
            int other_dim;
            other_dim = iv->VarName[i][strlen(iv->VarName[i])-1] - '0';
            if (!Read_2D_NetCDF_Var_Char_Data( current_fileid,
                 whichrec, whichvarid, iv->CharVarLength[i],
                 other_dim, (cdata+iv->CharPointer[i]))){
               printf("error in read_fdb_records2\n");
            }
         }
         /*******************/
         /* get 1D int data */
         /*******************/
         else if (fdb->VarType[k] == INT_VAR && fdb->VarDimensions[k] == VAR_1D){
            int temp_data;
            if (!Read_1D_NetCDF_Var_Int_Data( current_fileid,
                 whichrec, whichvarid, &temp_data)){
               printf("error in read_fdb_records\n");
               return 0;
            }
            fdata[i] = (double)temp_data;       
         }
         /*******************/        
         /* get 2D int data */
         /*******************/        
         else if (fdb->VarType[k] == INT_VAR && fdb->VarDimensions[k] == VAR_2D){
            int temp_data, other_dim;
            other_dim = iv->VarName[i][strlen(iv->VarName[i])-1] - '0';
            if (!Read_2D_NetCDF_Var_Int_Data( current_fileid,
                 whichrec, whichvarid, other_dim, &temp_data)){
               printf("error in read_fdb_records\n");
               return 0;
            }       
            fdata[i] = (double)temp_data;       
         }       
         else{
            printf("don't know that type, sorry\n");
         }

         /*******************************************/
         /* check to see if need to make it MISSING */
         /*******************************************/ 
         if (fdb->VarType[k] != CHAR_VAR){
            if (fdata[i]== fillvalue){
               fdata[i] = MISSING;
            }
         }
      }
   }

   return 1;
}


 
                             
   
/*************************************************************/
/* This will load all the nessecary info about in the fdb or */
/* file database into the irregular v5d struct               */
/*************************************************************/
/* Input: index - index of the fdb being used                */
/*        iv - pointer to the irregular v5d struct           */
/*************************************************************/
int load_fdb_into_v5d( int index, irregular_v5dstruct *iv )
{
   int sp, cp, i, j, k, t, q, p;
   FileDB fdb;

   fdb = get_fdb(index);

   if (fdb->FileType == NETCDF_SURFACE){
      iv->Type = SURFACE;
   }
   else if (fdb->FileType == NETCDF_SOUNDING){
      iv->Type = SOUNDING;
   }
   else{
      printf("Error in getting iv->Type\n");
      return 0;
   }

   j = 0;
   for (i = 0; i < fdb->NumVars; i++){
      if (fdb->VarSelected[i]){
         j++;
      }
   }
   iv->NumVars = j;

   j = 0;
   for (i = 0; i < fdb->NumTimes; i++){
      if (fdb->TimeSelected[i]){
         j++;
      }
   }
   iv->NumTimes = j;
   iv->Levels = fdb->Levels;

   /**************/
   /* copy times */
   /**************/
   j = 0;
   for (i = 0; i < fdb->NumTimes; i++){
      if (fdb->TimeSelected[i]){
         iv->TimeStamp[j] = fdb->TimeStamp[i];
         iv->DateStamp[j]  = fdb->DateStamp[i];
         iv->TimeSeconds[j] = fdb->TimeSeconds[i];
         iv->NumRecs[j] = fdb->NumRecs[i];
         j++;
      }
   }
   /******************/
   /* copy var names */
   /******************/
   cp = 0;
   sp = 0;
   j = 0;
   for (i = 0; i < fdb->NumVars; i++){
      if (fdb->VarSelected[i]){
         strcpy( iv->VarName[j], fdb->VarName[i]);
         iv->VarMin[j] = fdb->VarMin[i];
         iv->VarMax[j] = fdb->VarMax[i];
         if (fdb->VarType[i] == CHAR_VAR){
            iv->VarType[j] = CHARACTER_VAR;
         }
         else if (fdb->VarDimensions[i] == VAR_2D &&
                  fdb->FileType == NETCDF_SOUNDING){
            iv->VarType[j] = NUMERICAL_VAR_2D;
         }
         else{
            iv->VarType[j] = NUMERICAL_VAR_1D;
         }
         iv->CharVarLength[j] = fdb->CharVarLength[i];

         /**********************/
         /* set up CharPointer */
         /**********************/
         if (iv->VarType[j] == CHARACTER_VAR){
            iv->CharPointer[j] = cp;
            cp += iv->CharVarLength[j];
         }
         else{
            iv->CharPointer[j] = -1;
         }

         /**************************/
         /* set up SoundingPointer */
         /**************************/
         if (iv->VarType[j] == NUMERICAL_VAR_2D){
            iv->SoundingPointer[j] = sp;
            sp += iv->Levels;
         }
         else{
            iv->SoundingPointer[j] = -1;
         }
      
         j++;
      }
   }
   /***************/
   /* copy bounds */
   /***************/
   iv->TopBound = fdb->TopBound;
   iv->BottomBound = fdb->BottomBound;
   iv->WestBound = fdb->WestBound;
   iv->EastBound = fdb->EastBound;
   iv->NorthBound = fdb->NorthBound;
   iv->SouthBound = fdb->SouthBound;


   /************************************************/
   /* create some kind of record list to pass to   */
   /* the v5dstruct for use in calling             */
   /* the RecIDfilerecid is used when calling recs */
   /* for 0 to numrecs it will correspond to the   */
   /* proper file rec id                           */
   /************************************************/
   j = 0;
   for (i = 0; i < fdb->NumTimes; i++){
      if (fdb->TimeSelected[i]){
         fdb->RecIDfileindex[j] = (int *) malloc(sizeof(int)*fdb->NumRecs[i]);
         fdb->RecIDfilerecid[j] = (int *) malloc(sizeof(int)*fdb->NumRecs[i]);
         if (!fdb->RecIDfileindex[j] || !fdb->RecIDfilerecid[j]){
            printf("no memory left\n");
            exit(0);
         }
         j++;
      }
   }

   
   /**************************************************/
   /* this is kind of messy, may be changed later */
   /* but for now, it assigns [0..NumRecs] values */
   /* to the fdb  specifying */
   /* which records in which files to get for a time */
   /**************************************************/ 

   j = 0;
   for (i = 0; i < fdb->NumTimes; i++){
      if (fdb->TimeSelected[i]){
         int rec_count = 0;
         for ( k = 0; k < fdb->NumFiles; k++){
            for (t = 0; t < fdb->File[k]->NumTimes; t++){
               if (fdb->File[k]->TimeSeconds[t] ==
                              fdb->TimeSeconds[i]){
                  for (q = 0; q < fdb->File[k]->NumRecs[t]; q++){
                     fdb->RecIDfileindex[j][rec_count] = k;
                     fdb->RecIDfilerecid[j][rec_count] = 
                          fdb->File[k]->RecID[t][q];
                     rec_count++;
                  }
               }
            }
         }
         /*
         for (p = rec_count; p < fdb->NumRecs; p++){
            fdb->RecIDfileindex[j][p] = -1;
            fdb->RecIDfilerecid[j][p] = -1;
         }
         */
         j++;
      }
   }

   /*************************************************/
   /* This will go through all the File's and get   */
   /* the appropriate var id's for the variables in */
   /* the fdb var list.  This cuts down on time when*/
   /* reading the data                              */
   /*************************************************/
   for (i = 0; i < fdb->NumFiles; i++){
      int curid, vid;
      double fillvalue;
      if (!Open_NetCDF( fdb->File[i]->FileName, &curid)){
         printf("error in getting variable ids\n");
         return 0;
      }
      for (j = 0; j < fdb->NumVars; j++){
         if (fdb->VarDimensions[j] == VAR_1D ||
             (fdb->VarDimensions[j] == VAR_2D &&
              fdb->FileType == NETCDF_SOUNDING)){
            if (!Read_netCDF_Var_ID(curid, fdb->VarName[j], &vid)){
               printf("error2 in getting variable ids\n");
               return 0;
            }
         }
         else if (fdb->VarDimensions[j] == VAR_2D){
            char temp[MAX_VAR_LENGTH];
            strcpy(temp, fdb->VarName[j]);
            temp[strlen(fdb->VarName[j])-1] = 0;
            if (!Read_netCDF_Var_ID(curid, temp, &vid)){
               printf("error3 in getting variable ids\n");
               return 0;
            }
         }
         else{
            printf("error, can't handle var dimension \n");
            return 0;
         }
         fdb->File[i]->VarID[j] = vid;
         /****************************************/
         /* get the fill value too ahead of time */
         /****************************************/
         if (fdb->VarType[j] != CHAR_VAR){
            if (!Read_NetCDF_Fill( curid, vid, &fillvalue)){
               printf("can't get fill value\n");
               return 0;
            }
            fdb->File[i]->VarFillValue[j] = fillvalue;
         }
         else{
            fdb->File[i]->VarFillValue[j] = 0;
         }
      }
   }

   return 1;
}



