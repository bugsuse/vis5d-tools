/* record.c */

/* Vis5D version 5.1 */
 
/*
Vis5D system for visualizing five dimensional gridded data sets
Copyright (C) 1990 - 1997 Bill Hibbard, Johan Kellum, Brian Paul,
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
#include <ctype.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "api.h"
#include "binio.h"
#include "record.h"
#include "globals.h"
#include "memory.h"
#include "imemory.h"
#include "proj.h"
#include "sync.h"
#include "irregular_v5d.h"
 
 
#ifndef M_PI
#  define M_PI 3.14159265
#endif
 
#define DEG2RAD (M_PI/180.0)
#define RAD2DEG (180.0/M_PI)
 


int open_recordfile(Irregular_Context itx, char *filename)
{
   return 1;
}

/*
int init_record_cache( Irregular_Context itx, int maxbytes, float *ratio )
{
   int loc, i, j, k, l, v;
   int maxtimesize = 0;
   int numfloats, numchars, numberofchars;
   ALLOC_LOCK(itx->Mutex);
*/
   
   /*********************************/   
   /* figure out size of one record */
   /*********************************/
/*
   numfloats = 3;
   numchars = 0;
   numberofchars = 0;
   for (i = 0; i < itx->NumVars; i++){
      if (VarType == 0){
         numfloats++;
      }
      else if (VarType == 1){
         numchars += CharVarLength[i];
         numberofchars++;
      }
      else{
         printf("Error in init_record_cache\n");
         return -1;
      }
   }
   recordsize = numfloats * sizeof(float) +
                numchars  * sizeof(char);

   maxtimesize = itx->NumLocations * recordsize;

*/
   /****************************************/
   /* check for the unlikely case          */
   /* where the  time step can't be loaded */
   /****************************************/
/*
   if (maxbytes < maxtimesize){
      printf("Not enough memory to load one timestep\n");
      return -1;
   }
         

   itx->MaxCachedTimes = (int) (maxbytes / maxtimesize);
   if (itx->MaxCachedTimes >= itx->NumTimes){
      * all the records can be cached  *
      itx->MaxCachedTimes = itx->NumTimes;
      *ratio = 1.0;
   }
   else{
      *ratio = ((float) itx->MaxCachedTimes)
                /((float) (itx->NumTimes));
   }
   itx->NumCachedTimes = 0;
   printf("Cache size: %d records\n", itx->MaxCachedTimes );

   * Allocate the itx->IrregularCache array *
   itx->IrregularCache = (struct cache_irreg_rec *) i_allocate( itx, itx->NumCachedTimes *
                                                    sizeof(struct cache_irreg_rec));
   if (!itx->IrregularCache){
      printf("Error: out of memory.  Couldn't allocate cache space.\n");
      return 0;
   }
   for (i=0;i<itx->MaxCachedTimes;i++){
      for (l=0; l < itx->NumLocations;l++){
         itx->IrregularCache[i].Location =
         (struct location_data *) i_allocate( itx, itx->NumLocations*sizeof(struct location_data));
         if (!itx->IrregularCache[i].Location){
            printf("Error: out of memory.  Couldn't allocate cache space.\n");
            return 0;                                                    
         }                                                    
         for (loc = 0; loc < itx->NumLocations; loc++){
            itx->IrregularCache[i].Location[loc].DataType = 
                                  i_allocate( itx, itx->NumVars*sizeof(int));
            if (!itx->IrregularCache[i].Location[loc].DataType){
               printf("Error: out of memory.  Couldn't allocate cache space.\n");
               return 0;
            }
            itx->IrregularCache[i].Location[loc].Value = 
                                  i_allocate( itx, itx->NumVars*sizeof(float));
            if (!itx->IrregularCache[i].Location[loc].Value){
               printf("Error: out of memory.  Couldn't allocate cache space.\n");
               return 0;
            }
            itx->IrregularCache[i].Location[loc].CharData = 
                                  i_allocate( itx, itx->NumVars*sizeof(char *));
            if (!itx->IrregularCache[i].Location[loc].CharData){
               printf("Error: out of memory.  Couldn't allocate cache space.\n");
               return 0;
            }
            for (v=0; v<itx->NumVars; v++){
               itx->IrregularCache[i].Location[loc].DataType[v] = itx->VarType[v];
               if (itx->VarType[v] == 1){
                  itx->IrregularCache[i].Location[loc].Value[v] = NULL;
                  itx->IrregularCache[i].Location[loc].CharData[v] = 
                                        i_allocate( itx, itx->CharVarLength[v]*sizeof(char));
                  if (!itx->IrregularCache[i].Location[loc].CharData[v]){
                     printf("Error: out of memory.  Couldn't allocate cache space.\n");            
                     return 0;            
                  }            
               }
               else{
                  itx->IrregularCache[i].Location[loc].CharData[v] = NULL;
               }
            }
         }
      }
      itx->IrregularCache[i].Locked = 0;
      itx->IrregularCache[i].Timestep = 0;
   }
               
   for (i=0; i<itx->NumTimes; i++){
      itx->IrregularTable[i].CachePos = -1;
      itx->IrregularTable[i].Location = NULL;
   }
   return 1;
}
*/

/*
int get_empty_irreg_cache_pos(Irregular_Context itx)
{
   int g;

   if (itx->NumCachedTimes < itx->MaxCachedTimes){
      g = itx->NumCachedTimes;
      itx->NumCachedTimes++;
   }
   else{
#ifdef RANDOM
      int time;
      g = rand() % itx->MaxCachedTimes;
      while(itx->IrregularCache[g].Locked){
         g++;
         if (g >= itx->MaxCachedTimes){
            g = 0;
         }
      }
#else
      int minage, i, mini;
      minage = itx->CacheClock;
      for (i=0;i<itx->MaxCachedTimes;i++) {
         if (itx->IrregularCache[i].Age<minage && ctx->IrregularCache[i].Locked==0) {
            minage = itx->IrregularCache[i].Age;
            mini = i;
         }
      }
      g = mini;

      time = itx->IrregularCache[g].Timestep;
      itx->IrregularTable[time].Data = NULL;
      itx->IrregularTable[time].CachePos = -1;
#endif
   }

   itx->IrregularCache[g].Locked = 1;
   return g;
}
*/
      
static void *get_records_for_time(Irregular_Context itx, int time)
{
/*
   int p;
   LOCK_ON( itx->Mutex );
   
   if (itx->IrregularTable[time].Location){
      * already in the cache *
      p = itx->IrregularTable[time].CachePos;
      if (p >= 0);
         itx->IrregularTable[time].Locked = 1;
         itx->IrregularTable[time].Age = ctx->CacheClock++;
      }
      LOCK_OFF( itx->Mutex );
      return itx->IrregularTable[time].Location;    
   }
   else{
      * not in the cache *
      int g;
      g = get_empty_irreg_cache_pos(itx);
      if (!irregular_v5dReadRecords( &itx->G, time, itx->IrregularCache[g].Location)){
         printf("Error: unable to read record information\n");
         LOCK_OFF( itx->Mutex );
         return NULL;
      }
      itx->IrregularTable[time].Location = itx->IrregularCache[g].Location;
      itx->IrregularTable[time].CachePos = g;
      itx->IrregularCache.Locked = 1;
      itx->IrregularCache.Timestep = time;
      itx->IrregularCache.Age = itx->CacheClock++;
      LOCK_OFF(itx->Mutex);
      return itx->IrregularTable[time].Location;
   }
*/
}


/*
void i_preload_cache( Irregular_Context itx )
{

   if (itx->NumTimes <= itx->MaxCachedTimes){
      /* All grids will fit in the cache.  Read whole file now. */
      int time;
      printf("Reading all records.\n");
      for (time = 0; time < itx->NumTimes; time++){
         void *d;
         d = get_records_for_time( itx, time);
         if (d) release_records_for_time( itx, time);
      }
   }
}
*/

/*
int initially_open_recordfile( char filename[], irregular_v5dstruct *iv )
{
   char name[1000];
   
   strcpy( name, filename);

   if (!irregular_v5dOpenFile( name, iv)){
      printf("Could not open irregular data file\n");
      return 0;
   }
   return 1;
}
*/

/*
int open_record_file (Irregular_Context itx, char filename[] )
{

   int i;
   int time;
 
   if (!initially_open_recordfile( filename, &itx->G)){
      return 0;
   }

   strncpy(itx->DataFile, filename, 1000);
   strncpy(itx->ContextName, itx->G.filetitle,1000);
   itx->NumTimes = itx->G.numtimes;
   itx->NumLocations = itx->G.numrecs; 
   itx->NumVars = itx->G.numvars;
   for (i = 0; i < itx->NumVars; i++){
      strncpy(itx->VarName[i], itx->G.VarName[i], MAX_VAR_LENGTH);
      itx->VarType[i] = itx->G.VarType[i];
      itx->CharVarLength[i] = itx->G.CharVarLength[i];
   }
   * convert time from HHMMSS to seconds since midnight *
   * convert date from YYDDD to days since Jan, 1900 *
   for (time=0; time < itx->NumTimes; time++){
      itx->TimeStamp[time] = itx->G.timestamp[time];
      itx->DayStamp[time] =  itx->G.daystamp[time];
   }
   itx->NorthBound = itx->G.NorthBound;
   itx->SouthBound = itx->G.SouthBound;
   itx->WestBound = itx->G.WestBound;
   itx->EastBound = itx->G.EastBound; 
   itx->TopBound = itx->G.TopBound;
   itx->BottomBound = itx->G.BottomBound;

   return 1;
}
*/



         
    



/*

Location_Data *get_record(Irregular_Context itx, int time)
{
   int p;

   LOCK_ON(itx->Mutex);

   if (itx->IrregularTable[time].Location){
      * already in the cache *
      p = itx->IrregularTable[time].CachePos;
      if (p>=0){
         itx->IrregularCache[p].Locked = 1;
         itx->IrregularCache[p].Age = itx->CacheClock++;
      }
      LOCK_OFF( itx->Mutex );
      return itx->IrregularTable[time].Location;
   }
   else{
      * not in the cache *
      * assume that all data is cached all ready!!
      int g;
      g = i_get_empty_cache_pos(itx);
      if (!v5dReadIrregularRecord( &itx->I, time,
                                   itx->IrregularCache[g].Location)){
         printf("sorry, can't do that\n");
         LOCK_OFF( itx->Mutex );
         return NULL;
      }
      itx->IrregularTable[time].Location = itx->IrregularCache[g].Location;
      itx->IrregularTable[time].CachePos = g;
      itx->IrregularCache.Locked = 1;
      itx->IrregularCache.Timestep = time;
      itx->IrregularCache.Age = itx->CacheClock++;
      LOCK_OFF( itx->Mutex );
      return itx->IrregularTable[time].Location;
   }
}
*/
                      
