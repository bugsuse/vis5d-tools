/*  grid.c */

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
#include <ctype.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "api.h"
#include "binio.h"
#include "grid.h"
#include "graphics.h"
#include "globals.h"
#include "memory.h"
#include "proj.h"
#include "sync.h"

/* MJK 12.02.98 */
#include "user_data.h"

#include "v5d.h"


#ifndef M_PI
#  define M_PI 3.14159265
#endif

#define DEG2RAD (M_PI/180.0)
#define RAD2DEG (180.0/M_PI)


/* MJK 12.02.98 begin */
static int read_user_header( char filename[], v5dstruct *v )
{
   int iret = 0;

/*
 *  Call the user's function to get the user's header information.
 *  It is not necessary for this function to actually read a file --
 *  it only needs to put the user's header information into the Vis5D
 *  data structure.
 *
 *  An example for this function can be found in user_data.c.
 */

   iret = user_data_get_header (filename, v);


   return iret;
}

static int read_user_grid( v5dstruct *v, int time, int var, void *griddata )
{
   int iret = 0;

/*
 *  Call the user's function to get the user's grid data for the
 *  specified variable and time.
 *  It is not necessary for this function to actually read a file --
 *  it only needs to put the user's grid data in the place provided.
 *
 *  An example for this function can be found in user_data.c.
 */

   iret = user_data_get_grid (v, time, var, griddata);


   return iret;
}

/*
 * Open a user grid file, read the header, and initialize a bunch of
 * global variables.
 * Input:  filename - name of user grid file.
 * Return:  1 = success, 0 = error, -1 = try Vis5D.
 */
int open_userfile( char filename[], v5dstruct *v )
{
   int iret, var;


   if ((iret = read_user_header (filename, v)) != 1) {
      return iret;
   }


   v5dVerifyStruct( v );

   /* compute grid sizes */
   v->SumGridSizes = 0;
   for (var=0;var<v->NumVars;var++) {
      v->GridSize[var] = 8 * v->Nl[var] + v5dSizeofGrid( v, 0, var );
      v->SumGridSizes += v->GridSize[var];
   }


   return 1;
}

/*
 * Read a grid from a user file.
 * Input:  v - pointer to v5dstruct describing the file
 *         time, var - which timestep and variable
 *         griddata - address of where to store grid data.
 * Return:  1 = ok, 0 = error, -1 = try Vis5D.
 */
int read_userfile( v5dstruct *v, int time, int var, void *griddata )
{

   if (time<0 || time>=v->NumTimes) {
      printf("Error in v5dReadCompressedGrid: bad timestep argument (%d)\n",
             time);
      return 0;
   }
   if (var<0 || var>=v->NumVars) {
      printf("Error in v5dReadCompressedGrid: bad var argument (%d)\n",
             var);
      return 0;
   }

   return read_user_grid (v, time, var, griddata);
}



static void *get_compressed_grid( Context ctx, int time, int var,
                                  float **ga, float **gb );

int write_gridfile( Context ctx, char filename[] )
{
   char name[1000];
   int i, time, var, first;
   void *compdata;
   float *ga, *gb;
   v5dstruct *v;

   v = v5dNewStruct();

   v->NumTimes = ctx->NumTimes;
   v->NumVars = ctx->NumVars;
   v->Nr = ctx->Nr;
   v->Nc = ctx->Nc;
   for (var=0;var<ctx->NumVars;var++) {
      v->Nl[var] = ctx->Nl[var];
      v->LowLev[var] = ctx->LowLev[var];
      strncpy( v->VarName[var], ctx->VarName[var], 8);
      strncpy( v->Units[var], ctx->Units[var], 19 );
      v->MinVal[var] = ctx->MinVal[var]; 
      v->MaxVal[var] = ctx->MaxVal[var];
   }
   for (time=0;time<ctx->NumTimes;time++) {
      v->TimeStamp[time] = v5dSecondsToHHMMSS( ctx->TimeStamp[time] );
      v->DateStamp[time] = v5dDaysToYYDDD( ctx->DayStamp[time] );
   }
   v->CompressMode = ctx->CompressMode;

   /* do the projection and vert coord sys */
   v->Projection = ctx->Projection;
   switch (ctx->Projection) {
      case PROJ_GENERIC:
      case PROJ_LINEAR:
      case PROJ_CYLINDRICAL:
      case PROJ_SPHERICAL:
         v->ProjArgs[0] = ctx->NorthBound;
         v->ProjArgs[1] = ctx->WestBound;
         v->ProjArgs[2] = ctx->RowInc;
         v->ProjArgs[3] = ctx->ColInc;
         break;
      case PROJ_MERCATOR:
         v->ProjArgs[0] = ctx->CentralLat;
         v->ProjArgs[1] = ctx->CentralLon;
         v->ProjArgs[2] = ctx->RowIncKm;
         v->ProjArgs[3] = ctx->ColIncKm;
         break;
      case PROJ_ROTATED:
         v->ProjArgs[0] = ctx->NorthBound; 
         v->ProjArgs[1] = ctx->WestBound; 
         v->ProjArgs[2] = ctx->RowInc;   
         v->ProjArgs[3] = ctx->ColInc;  
         v->ProjArgs[4] = ctx->CentralLat/DEG2RAD; 
         v->ProjArgs[5] = ctx->CentralLon/DEG2RAD;
         v->ProjArgs[6] = ctx->Rotation/DEG2RAD; 
         break;
      case PROJ_LAMBERT:
         v->ProjArgs[0] = ctx->Lat1;    
         v->ProjArgs[1] = ctx->Lat2;   
         v->ProjArgs[2] = ctx->PoleRow;  
         v->ProjArgs[3] = ctx->PoleCol; 
         v->ProjArgs[4] = ctx->CentralLon; 
         v->ProjArgs[5] = ctx->ColInc;    
         break;
      case PROJ_STEREO:
         v->ProjArgs[0] = ctx->CentralLat; 
         v->ProjArgs[1] = ctx->CentralLon;
         v->ProjArgs[2] = ctx->CentralRow;
         v->ProjArgs[3] = ctx->CentralCol;
         v->ProjArgs[4] = ctx->ColInc;    
         break;
      default:
         printf("Error: unknown projection type in grid.c\n");
   }
   v->VerticalSystem = ctx->VerticalSystem;
   switch (ctx->VerticalSystem) {
      case VERT_GENERIC:
      case VERT_EQUAL_KM:
         v->VertArgs[0] = ctx->BottomBound;
         v->VertArgs[1] = ctx->LevInc;
         break;
      case VERT_NONEQUAL_MB:
      case VERT_NONEQUAL_KM:
         for (i=0;i<ctx->MaxNl;i++) {
            v->VertArgs[i] = ctx->Height[i];
         }
         break;
      default:
         printf("Error in grid.c, unknown vertical coord system\n");
   }
            
   v5dCreateFile( filename, v);

   for (time=0;time<ctx->NumTimes;time++) {
      for (var=0;var<ctx->NumVars;var++) {
         printf("Writing grid to file. Time = %d Var = %d\n", time, var);
         compdata = get_compressed_grid( ctx, time, var, &ga, &gb );
         if (!v5dWriteCompressedGrid( v, time, var, ga, gb, compdata )){
            printf("Error in write_gridfile: cannot write compressed grid to file\n");
            exit(0);
         }
      }
   }
   v5dCloseFile( v );

     
   v5dFreeStruct( v );
   return 1;
}
   
/*
 * Open a compressed grid file, read the header, and initialize a bunch of
 * global variables.
 * Input:  filename - name of compressed grid file.
 * Return:  1 = success, 0 = error.
 */
int open_gridfile( Context ctx, char filename[] )
{
   char name[1000];
   int ok, i, time, var, first;

   /* MJK 12.02.98 begin */
   ok = -1;
   if (ctx->UserDataFlag) {
      ok = open_userfile (filename, &ctx->G);
      if (ok == 0) return 0;
   }
   if (ok == -1) {
      if (!initially_open_gridfile( filename, &ctx->G)) {
         return 0;
      }
   }
   /* MJK 12.02.98 end */


   /* Initalize parameter type table */
   for (i=0;i<MAXVARS;i++) {
      ctx->VarType[i] = 0;
   }

   /* Copy header info from G to global variables */
   ctx->NumTimes = ctx->G.NumTimes;
   ctx->NumVars = ctx->G.NumVars;
   ctx->Nr = ctx->G.Nr;
   ctx->Nc = ctx->G.Nc;
   ctx->MaxNl = 0;
   for (var=0;var<ctx->NumVars;var++) {
      ctx->Nl[var] = ctx->G.Nl[var];
      ctx->LowLev[var] = ctx->G.LowLev[var];
      if (ctx->Nl[var]+ctx->LowLev[var]>ctx->MaxNl) {
         ctx->MaxNl = ctx->Nl[var]+ctx->LowLev[var];
         ctx->MaxNlVar = var;
      }
      strncpy( ctx->VarName[var], ctx->G.VarName[var], 8 );
      strncpy( ctx->Units[var], ctx->G.Units[var], 19 );
      ctx->MinVal[var] = ctx->G.MinVal[var];
      ctx->MaxVal[var] = ctx->G.MaxVal[var];
      ctx->VarType[var] = VIS5D_REGULAR;
      ctx->CloneTable[var] = var;
   }

   /* MJK 4-13-98 
   if (ctx->NumVars == 1 && ctx->MaxNl == 1){
      ctx->MaxNl = 2;
      ctx->Nl[0] = 2;
   }
   end MJK*/

   /* Check that grid isn't too big */
   if (ctx->NumTimes>MAXTIMES) {
      printf("Error: Too many time steps (%d) limit is %d\n", ctx->NumTimes,
             MAXTIMES );
      return 0;
   }
   if (ctx->NumVars>MAXVARS) {
      printf("Error: Too many variables (%d) limit is %d\n", ctx->NumVars,
             MAXVARS );
      return 0;
   }
   if (ctx->Nr>MAXROWS) {
      printf("Error: Number of grid rows (%d) too large, %d is limit.\n",
             ctx->Nr,MAXROWS);
      printf("Edit src/v5d.h and increase MAXROWS\n");
      return 0;
   }
   if (ctx->Nc>MAXCOLUMNS) {
      printf("Error: Number of grid columns (%d) too large, %d is limit.\n",
             ctx->Nc, MAXCOLUMNS );
      printf("Edit src/v5d.h and increase MAXCOLUMNS\n");
      return 0;
   }
   if (ctx->MaxNl>MAXLEVELS) {
      printf("Error: Number of grid levels (%d) too large, %d is limit.\n",
             ctx->Nl, MAXLEVELS );
      printf("Edit src/v5d.h and increase MAXLEVELS\n");
      return 0;
   }

   /* convert time from HHMMSS to seconds since midnight */
   /* convert date from YYDDD to days since Jan, 1900 */
   for (time=0;time<ctx->NumTimes;time++) {
      ctx->TimeStamp[time] = v5dHHMMSStoSeconds( ctx->G.TimeStamp[time] );
      ctx->DayStamp[time] = v5dYYDDDtoDays( ctx->G.DateStamp[time] );
   }

   ctx->CompressMode = ctx->G.CompressMode;

   /* calculate elapsed time (in seconds) for each time since initial time */
   first = ctx->DayStamp[0]* 24*60*60 + ctx->TimeStamp[0];
   for (time=0;time<ctx->NumTimes;time++) {
      ctx->Elapsed[time] = ctx->DayStamp[time] * 24*60*60
                         + ctx->TimeStamp[time] - first;
   }

   return 1;
}



int free_grid_cache( Context ctx )
{
   int it, iv;

   for (it=0; it<MAXTIMES; it++){
      for (iv=0; iv<MAXVARS; iv++){
         if (ctx->Ga[it][iv]){
            deallocate(ctx, ctx->Ga[it][iv], -1);
            ctx->Ga[it][iv] = NULL;
         }
         if (ctx->Gb[it][iv]){
            deallocate( ctx, ctx->Gb[it][iv], -1);
            ctx->Gb[it][iv] = NULL;
         }
      }
   }
}


/*
 * Initialize the grid caching system.  This *must* be called *after*
 * the data file's header information has been read.
 * Input:  maxbytes - maximum number of bytes to use for caching grids.
 * Return:  1 = ok, 0 = error (out of memory).
 */
int init_grid_cache( Context ctx, int maxbytes, float *ratio )
{
   int i, it, iv;
   int maxnl, gridsize;

   free_grid_cache( ctx );

   /* First allocate space for ga/gb compression values */
   for (it=0;it<ctx->NumTimes;it++) {
      for (iv=0;iv<ctx->NumVars;iv++) {
         ctx->Ga[it][iv] = (float *) allocate( ctx, ctx->Nl[iv] * sizeof(float) );
         ctx->Gb[it][iv] = (float *) allocate( ctx, ctx->Nl[iv] * sizeof(float) );
      }
   }

   ALLOC_LOCK( ctx->Mutex );   /* Allocate the mutex lock */

   /* Determine the maximum number of grids to cache given the maximum */
   /* number of bytes of memory to use. */
   maxnl = 0;
   for (iv=0; iv<ctx->NumVars; iv++) {
      if (ctx->Nl[iv] > maxnl) {
         maxnl = ctx->Nl[iv];
      }
   }
   gridsize = ctx->Nr * ctx->Nc * maxnl * ctx->CompressMode;
   ctx->MaxCachedGrids = (int) (maxbytes / gridsize);

   if (ctx->MaxCachedGrids >= ctx->NumTimes*ctx->NumVars) {
      /* the whole file can be cached */
      ctx->MaxCachedGrids = ctx->NumTimes*ctx->NumVars;
      *ratio = 1.0;
   }
   else {
      *ratio = ((float) ctx->MaxCachedGrids)
             / ((float) (ctx->NumTimes*ctx->NumVars));
   }

   ctx->NumCachedGrids = 0;

   printf("Cache size: %d grids\n", ctx->MaxCachedGrids );
  
   if (ctx->MaxCachedGrids != ctx->NumTimes*ctx->NumVars){
      int needed;
      needed = (((gridsize * ctx->NumTimes * ctx->NumVars)
               * 5 / 2) / (1024*1024));
      needed = (int) ( (float) needed * 1.25) + 2;
      printf(" Hint... To run Vis5D more efficiently try setting %s to '-mbs %d'\n",
               ctx->DataFile, needed);
   }
      
   /* Allocate the ctx->GridCache array */
   ctx->GridCache = (struct cache_rec *) allocate( ctx, ctx->MaxCachedGrids
                                                * sizeof(struct cache_rec) );
   if (!ctx->GridCache) {
      printf("Error: out of memory.  Couldn't allocate cache table.\n");
      return 0;
   }

   ctx->CacheClock = 1;

   /* Initialize tables */
   for (i=0;i<ctx->MaxCachedGrids;i++) {
      ctx->GridCache[i].Data = (void *) allocate( ctx, gridsize );
      if (!ctx->GridCache[i].Data) {
         printf("Error: out of memory.  Couldn't allocate cache space.\n");
         return 0;
      }
      ctx->GridCache[i].Locked = 0;
      ctx->GridCache[i].Timestep = 0;
      ctx->GridCache[i].Var = 0;
   }
   for (it=0;it<ctx->NumTimes;it++) {
      for (iv=0;iv<MAXVARS;iv++) {
         ctx->GridTable[it][iv].CachePos = -1;
         ctx->GridTable[it][iv].Data = NULL;
      }
   }
   return 1;
}

/*
 * Open a compressed grid file and read the header;
 * if lowecase name fails, try uppercase.
 * Input: filename -name of compressed grid file                 
 *        v - pointer to struct to hold return values
 * Return: 1 = success, 0 = error
 */
int initially_open_gridfile( char filename[], v5dstruct *v )
{
   char name[1000];
   int i;
 
   strcpy( name, filename);
  
   /* Open the v5d file */
   if (!v5dOpenFile( name, v)) {
      /* try uppercase name */
      for ( i = strlen(name)-1; i >= 0; i--) {
         if (name[i] == '/')
            break;
         else if (islower(name[i]))
            name [i] -= 'a'-'A';
      }
      if (!v5dOpenFile( name, v)) {
         printf("Error: datafile %s not found \n", filename);
         return 0;
      }
      else {
         /* success, change filename to uppercase */
         strcpy (filename, name);
      }
   }
   return 1;
}
 

/*
 * Open a compressed grid file,, read the header, and then close the grid file.
 * Input: filename - name of compressed grid file.
 *        v - pointer to struct to hold return values
 * Return: 1 = success, 0 = error.
 */
int query_gridfile( char filename[], v5dstruct *v )
{
   if (!initially_open_gridfile( filename, v)) {
      return 0;
   }
   v5dCloseFile( v );
   return 1;
}




/*
 * Return an index into the ctx->GridCache array which corresponds to an empty
 * position.  If the cache is full, we'll discard something.
 */
int get_empty_cache_pos( Context ctx )
{
   int g;

   /* find g */
   if (ctx->NumCachedGrids<ctx->MaxCachedGrids) {
      /* There's an unused position. */
      g = ctx->NumCachedGrids;
      ctx->NumCachedGrids++;
   }
   else {
      int time, var;
#ifdef RANDOM
      /* pick a cache position at random for replacement */
      g = rand() % ctx->MaxCachedGrids;
      while (ctx->GridCache[g].Locked) {
         g++;
         if (g>=ctx->MaxCachedGrids)
            g = 0;
      }
      printf("Random discard %d\n", g );
#else
      /* LRU */
      int minage, i, mini;
      minage = ctx->CacheClock;
      for (i=0;i<ctx->MaxCachedGrids;i++) {
         if (ctx->GridCache[i].Age<minage && ctx->GridCache[i].Locked==0) {
            minage = ctx->GridCache[i].Age;
            mini = i;
         }
      }
      g = mini;

      /* remove references to data being discarded */
      time = ctx->GridCache[g].Timestep;
      var = ctx->GridCache[g].Var;
      ctx->GridTable[time][var].Data = NULL;
      ctx->GridTable[time][var].CachePos = -1;
#endif
   }

   ctx->GridCache[g].Locked = 1;
   return g;
}




/*** get_compressed_grid **********************************************
   Return a pointer to the compressed data for a 3-D grid.
   Input: time, var - time and variable of grid wanted.
          ga, gb - pointer to pointer to float.
   Output: ga, gb - array of values to use for decompressing.
   Return:  pointer to 1, 2 or 4-byte data values
**********************************************************************/
static void *get_compressed_grid( Context ctx, int time, int var,
                                  float **ga, float **gb )
{
  int p, ok;

  var = ctx->CloneTable[var];

  LOCK_ON( ctx->Mutex );

  if (ctx->GridTable[time][var].Data) {
    /* already in the cache */
    p = ctx->GridTable[time][var].CachePos;
    if (p>=0) {
      ctx->GridCache[p].Locked = 1;
      ctx->GridCache[p].Age = ctx->CacheClock++;
    }
    LOCK_OFF( ctx->Mutex );
    *ga = ctx->Ga[time][var];
    *gb = ctx->Gb[time][var];
    return ctx->GridTable[time][var].Data;
  }
  else {
    /* not in the cache */
    int g;
    g = get_empty_cache_pos(ctx);

    /*printf("Reading grid into pos %d\n", g );*/



   ok = -1;
   if (ctx->UserDataFlag) {
      ok = read_userfile (&ctx->G, time, var, ctx->GridCache[g].Data);
   }
   if (ok == -1) {
      ok = v5dReadCompressedGrid( &ctx->G, time, var,
                                   ctx->Ga[time][var], ctx->Gb[time][var],
                                   ctx->GridCache[g].Data );
   }
   /* MJK 12.02.98 end */



/* MJK 3.3.99 */
   if (!ok){
      printf("Error: unable to read grid (time=%d, var=%d)\n",
             time, var );
      LOCK_OFF( ctx->Mutex );
      return NULL;
    }

    ctx->GridTable[time][var].Data = ctx->GridCache[g].Data;
    ctx->GridTable[time][var].CachePos = g;
    ctx->GridCache[g].Locked = 1;
    ctx->GridCache[g].Timestep = time;
    ctx->GridCache[g].Var = var;
    ctx->GridCache[g].Age = ctx->CacheClock++;

    LOCK_OFF( ctx->Mutex );
    *ga = ctx->Ga[time][var];
    *gb = ctx->Gb[time][var];
    return ctx->GridTable[time][var].Data;
  }
}




/*** release_compressed_grid ******************************************
   Release a compressed grid.
   Input:  time, var - the timestep and variable of grid to release.
**********************************************************************/
void release_compressed_grid( Context ctx, int time, int var )
{
   int p;

   /* just unlock */
   LOCK_ON( ctx->Mutex );
   p = ctx->GridTable[time][var].CachePos;
   if (p>=0) {
      ctx->GridCache[ p ].Locked = 0;
   }
   LOCK_OFF( ctx->Mutex );
}



/*
 * Load some or all of the grid data into main memory.
 */
void preload_cache( Context ctx )
{
   if (ctx->NumTimes*ctx->NumVars <= ctx->MaxCachedGrids) {
      /* All grids will fit in the cache.  Read whole file now. */
      int time, var;
      printf("Reading all grids.\n");
      for (time=0;time<ctx->NumTimes;time++) {
         for (var=0;var<ctx->NumVars;var++) {
            float *ga, *gb;
            void *d;
            d = get_compressed_grid( ctx, time, var, &ga, &gb );
            if (d) release_compressed_grid( ctx, time, var );
         }
      }
   }
}



/*** get_grid *********************************************************
   Return a pointer to the uncompressed data for a 3-D grid.
   Input:  time, var - time and variable of grid wanted.
   Return:  pointer to float data
**********************************************************************/
float *get_grid( Context ctx, int time, int var )
{
   float *data, *ga, *gb;
   void *compdata;
   int nrncnl;

   var = ctx->CloneTable[var];
   nrncnl = ctx->Nr * ctx->Nc * ctx->Nl[var];
   data = (float *) allocate_type( ctx, nrncnl*sizeof(float), GRID_TYPE );
   if (!data) {
      return NULL;
   }

   compdata = get_compressed_grid( ctx, time, var, &ga, &gb );
   if (compdata) {
     v5dDecompressGrid( ctx->Nr, ctx->Nc, ctx->Nl[var], ctx->CompressMode,
                        compdata, ga, gb, data );
     release_compressed_grid( ctx, time, var );
   }

   return data;
}

/* time = fromctx time */
/* var = fromctx var */
float *get_grid2( Context toctx, Context fromctx, int time, int var, int numlevs )
{
   float *data, *ga, *gb, fdata;
   void *compdata;
   int nrncnl, nr, nc, nl;
   float yonrr, yoncc, yonll, nnr, nnc, nnl, lat, lon, hgt;

   var = fromctx->CloneTable[var];
   nrncnl = toctx->Nr * toctx->Nc * numlevs;
   data = (float *) allocate_type( toctx, nrncnl*sizeof(float), GRID_TYPE );
   if (!data) {
      return NULL;
   }

   for (nr = 0; nr < toctx->Nr; nr++){
      for (nc = 0; nc < toctx->Nc; nc++){
         for (nl = 0; nl < numlevs; nl++){
             yonrr = (float) (nr);
             yoncc = (float) (nc);
             yonll = (float) (nl);
             grid_to_geo(toctx, 0, 0, 1, &yonrr, &yoncc, &yonll,
                         &lat, &lon, &hgt);
             geo_to_grid(fromctx, time, var, 1, &lat,
                         &lon, &hgt, &nnr, &nnc, &nnl);
             fdata = interpolate_grid_value( fromctx, time, var,
                         nnr, nnc, nnl);
             data[nr+toctx->Nr*(nc+toctx->Nc*nl)] = fdata;
         }
      }
   }
   return data;
}




/*** put_grid *********************************************************
   Store a new 3-D grid of data.
   Input:  time, var - time and variable of the new grid
           griddata - the new grid data
   Return:  1=ok, 0=error
**********************************************************************/
int put_grid( Context ctx, int time, int var, float *griddata )
{
   if (ctx->VarType[var] == VIS5D_REGULAR ||
       ctx->VarType[var] == VIS5D_PUT) {
     return install_new_grid( ctx, time, var, griddata, ctx->Nl[var],
                              ctx->LowLev[var]);
   }
   else {
     return 0;
   }
}



/*** release_grid *****************************************************
   Release an uncompressed grid.
   Input:  time, var - the timestep and parameter of the grid.
           data - the grid data returned by get_grid().
**********************************************************************/
void release_grid( Context ctx, int time, int var, float *data )
{
   assert( time>=0 && time<ctx->NumTimes );
   assert( var>=0 && var<ctx->NumVars );

   deallocate( ctx, data, ctx->Nr*ctx->Nc*ctx->Nl[var]*sizeof(float) );
}


void release_grid2( Context ctx, int time, int var, int nl, float *data )
{
   deallocate( ctx, data, ctx->Nr*ctx->Nc*nl*sizeof(float) );
}


/*
 * Get a discrete grid value.
 * Input:  time - which timestep
 *         var - which variable
 *         row, col, lev - location
 * Return:  grid value or MISSING if missing.
 */
float get_grid_value( Context ctx, int time, int var,
                      int row, int col, int lev )
{
   void *data;
   float *gavec, *gbvec;
   float value;

   /* WLH 6-30-95 */
   lev -= ctx->LowLev[var];
   if (lev < 0 || lev >= ctx->Nl[var]) return MISSING;

   var = ctx->CloneTable[var];
   data = get_compressed_grid( ctx, time, var, &gavec, &gbvec );
   if (!data) return MISSING;

   if (ctx->CompressMode == 1) {
      V5Dubyte *data1 = (V5Dubyte *) data;
      V5Dubyte c1 = data1[ (lev * ctx->Nc + col) * ctx->Nr + row ];
      if (c1==255) {
         value = MISSING;
      }
      else {
         value = (float) (int) c1 * gavec[lev] + gbvec[lev];
      }
   }
   else if (ctx->CompressMode == 2) {
      V5Dushort *data2 = (V5Dushort *) data;
      V5Dushort c2 = data2[ (lev * ctx->Nc + col) * ctx->Nr + row ];
      if (c2==65535) {
         value = MISSING;
      }
      else {
         value = (float) (int) c2 * gavec[lev] + gbvec[lev];
      }
   }
   else {
      float *data4 = (float *) data;
      value = data4[ (lev * ctx->Nc + col) * ctx->Nr + row ];
   }

   release_compressed_grid( ctx, time, var );

   return value;
}



/*
 * Return a grid value at an arbitrary grid position.  Values will be
 * interpolated between neighboring values.
 * Input:  time - timestep in [0..NumTimes-1]
 *         var - variable in [0..NumVars-1]
 *         row, col, lev - location in [0..Nr-1],[0..Nc-1],[0..Nl[var]-1]
 * Return:  data value or MISSING if missing.
 */
float interpolate_grid_value( Context ctx, int time, int var,
                              float row, float col, float lev )
{
   void *data;
   int i0, j0, k0, i1, j1, k1;
   float d0,d1,d2,d3,d4,d5,d6,d7, d;
   float ei, ej, ek;
   float *gavec, *gbvec, ga, gb;

   /* WLH 6-30-95 */
   lev -= ctx->LowLev[var];
   if (lev < 0 || lev >= ctx->Nl[var] ||
       col < 0 || col >= ctx->Nc ||
       row < 0 || row >= ctx->Nr){
      return MISSING;
   }

   var = ctx->CloneTable[var];

   data = get_compressed_grid( ctx, time, var, &gavec, &gbvec );
   if (!data) return MISSING;

   i0 = (int) row;  i1 = i0 + 1;
   j0 = (int) col;  j1 = j0 + 1;
   k0 = (int) lev;  k1 = k0 + 1;
   
   /* check for grid boundaries */
   if (i0==ctx->Nr-1) {
      i1 = i0;
   }
   if (j0==ctx->Nc-1) {
      j1 = j0;
   }
   if (k0==ctx->Nl[var]-1) {
      k1 = k0;
   }

   /* error terms */
   ei = row - (float) i0;
   ej = col - (float) j0;
   ek = lev - (float) k0;

   /* check if exactly on a row, column, or level */
   if (ei==0.0)  i1 = i0;
   if (ej==0.0)  j1 = j0;
   if (ek==0.0)  k1 = k0;

   if (ctx->CompressMode == 1) {
      /* get eight values at corners of a cube around (r,c,l) */
      V5Dubyte c0,c1,c2,c3,c4,c5,c6,c7;
      V5Dubyte *data1 = (V5Dubyte *) data;

      c0 = data1[ (k0 * ctx->Nc + j0) * ctx->Nr + i0 ];   /* d0 @ (i0,j0,k0) */
      c1 = data1[ (k0 * ctx->Nc + j0) * ctx->Nr + i1 ];   /* d1 @ (i1,j0,k0) */
      c2 = data1[ (k0 * ctx->Nc + j1) * ctx->Nr + i0 ];   /* d2 @ (i0,j1,k0) */
      c3 = data1[ (k0 * ctx->Nc + j1) * ctx->Nr + i1 ];   /* d3 @ (i1,j1,k0) */
      c4 = data1[ (k1 * ctx->Nc + j0) * ctx->Nr + i0 ];   /* d4 @ (i0,j0,k1) */
      c5 = data1[ (k1 * ctx->Nc + j0) * ctx->Nr + i1 ];   /* d5 @ (i1,j0,k1) */
      c6 = data1[ (k1 * ctx->Nc + j1) * ctx->Nr + i0 ];   /* d6 @ (i0,j1,k1) */
      c7 = data1[ (k1 * ctx->Nc + j1) * ctx->Nr + i1 ];   /* d7 @ (i1,j1,k1) */

      release_compressed_grid( ctx, time, var );

      /* check for missing data */
      if (c0==255 || c1==255 || c2==255 || c3==255 ||
          c4==255 || c5==255 || c6==255 || c7==255) {
         return MISSING;
      }

      /* decompress the values */
      ga = gavec[k0];
      gb = gbvec[k0];
      d0 = (float) (int) c0 * ga + gb;
      d1 = (float) (int) c1 * ga + gb;
      d2 = (float) (int) c2 * ga + gb;
      d3 = (float) (int) c3 * ga + gb;

      ga = gavec[k1];
      gb = gbvec[k1];
      d4 = (float) (int) c4 * ga + gb;
      d5 = (float) (int) c5 * ga + gb;
      d6 = (float) (int) c6 * ga + gb;
      d7 = (float) (int) c7 * ga + gb;
   }
   else if (ctx->CompressMode == 2) {
      V5Dushort c0,c1,c2,c3,c4,c5,c6,c7;
      V5Dushort *data2 = (V5Dushort *) data;

      /* get eight values at corners of a cube around (r,c,l) */
      c0 = data2[ (k0 * ctx->Nc + j0) * ctx->Nr + i0 ];   /* d0 @ (i0,j0,k0) */
      c1 = data2[ (k0 * ctx->Nc + j0) * ctx->Nr + i1 ];   /* d1 @ (i1,j0,k0) */
      c2 = data2[ (k0 * ctx->Nc + j1) * ctx->Nr + i0 ];   /* d2 @ (i0,j1,k0) */
      c3 = data2[ (k0 * ctx->Nc + j1) * ctx->Nr + i1 ];   /* d3 @ (i1,j1,k0) */
      c4 = data2[ (k1 * ctx->Nc + j0) * ctx->Nr + i0 ];   /* d4 @ (i0,j0,k1) */
      c5 = data2[ (k1 * ctx->Nc + j0) * ctx->Nr + i1 ];   /* d5 @ (i1,j0,k1) */
      c6 = data2[ (k1 * ctx->Nc + j1) * ctx->Nr + i0 ];   /* d6 @ (i0,j1,k1) */
      c7 = data2[ (k1 * ctx->Nc + j1) * ctx->Nr + i1 ];   /* d7 @ (i1,j1,k1) */

      release_compressed_grid( ctx, time, var );

      /* check for missing data */
      if (c0==65535 || c1==65535 || c2==65535 || c3==65535 ||
          c4==65535 || c5==65535 || c6==65535 || c7==65535) {
         return MISSING;
      }

      /* decompress the values */
      ga = gavec[k0];
      gb = gbvec[k0];
      d0 = (float) (int) c0 * ga + gb;
      d1 = (float) (int) c1 * ga + gb;
      d2 = (float) (int) c2 * ga + gb;
      d3 = (float) (int) c3 * ga + gb;

      ga = gavec[k1];
      gb = gbvec[k1];
      d4 = (float) (int) c4 * ga + gb;
      d5 = (float) (int) c5 * ga + gb;
      d6 = (float) (int) c6 * ga + gb;
      d7 = (float) (int) c7 * ga + gb;
   }
   else {
      float *data4 = (float *) data;

      /* get eight values at corners of a cube around (r,c,l) */
      d0 = data4[ (k0 * ctx->Nc + j0) * ctx->Nr + i0 ];   /* d0 @ (i0,j0,k0) */
      d1 = data4[ (k0 * ctx->Nc + j0) * ctx->Nr + i1 ];   /* d1 @ (i1,j0,k0) */
      d2 = data4[ (k0 * ctx->Nc + j1) * ctx->Nr + i0 ];   /* d2 @ (i0,j1,k0) */
      d3 = data4[ (k0 * ctx->Nc + j1) * ctx->Nr + i1 ];   /* d3 @ (i1,j1,k0) */
      d4 = data4[ (k1 * ctx->Nc + j0) * ctx->Nr + i0 ];   /* d4 @ (i0,j0,k1) */
      d5 = data4[ (k1 * ctx->Nc + j0) * ctx->Nr + i1 ];   /* d5 @ (i1,j0,k1) */
      d6 = data4[ (k1 * ctx->Nc + j1) * ctx->Nr + i0 ];   /* d6 @ (i0,j1,k1) */
      d7 = data4[ (k1 * ctx->Nc + j1) * ctx->Nr + i1 ];   /* d7 @ (i1,j1,k1) */

      release_compressed_grid( ctx, time, var );

      /* check for missing data */
      if (IS_MISSING(d0) || IS_MISSING(d1) ||
          IS_MISSING(d2) || IS_MISSING(d3) ||
          IS_MISSING(d4) || IS_MISSING(d5) ||
          IS_MISSING(d6) || IS_MISSING(d7)) {
         return MISSING;
      }

   }

   /* compute weighted value */
   d = ( d0 * (1.0-ei) * (1.0-ej)
       + d1 * ei       * (1.0-ej)
       + d2 * (1.0-ei) * ej
       + d3 * ei       * ej       ) * (1.0-ek)
     + ( d4 * (1.0-ei) * (1.0-ej)
       + d5 * ei       * (1.0-ej)
       + d6 * (1.0-ei) * ej
       + d7 * ei       * ej       ) * ek;

   return d;
}




/*** allocate_clone_variable *****************************************
   Allocate a new variable which is to be a clone of an existing one.
   This function takes care of all the grid data structure housekeeping.
   Input:  name - name of the new variable.
           var_to_clone - number of the existing variable which we will
                          clone (in [0..ctx->NumVars-1]).
   Return: -1 = error
           otherwise, return the number of the new variable.
**********************************************************************/
int allocate_clone_variable( Context ctx, char name[], int var_to_clone )
{
   int newvar;

   for (newvar=0;newvar<MAXVARS;newvar++) {
      if (ctx->VarType[newvar]==0)
         break;
   }
   if (newvar==MAXVARS) {
      /* no space for a new variable */
      return -1;
   }

   ctx->VarType[newvar] = VIS5D_CLONE;
   ctx->CloneTable[newvar] = var_to_clone;
   ctx->NumVars++;

   /* copy variable/parameter name */
   strncpy( ctx->VarName[newvar], name, 8 );

   ctx->Nl[newvar] = ctx->Nl[var_to_clone];
   ctx->LowLev[newvar] = ctx->LowLev[var_to_clone];

   /* copy min, max values */
   ctx->MinVal[newvar] = ctx->MinVal[var_to_clone];
   ctx->MaxVal[newvar] = ctx->MaxVal[var_to_clone];

   /* MJK 12.14.98 */
   strcpy (ctx->Units[newvar], ctx->Units[var_to_clone]);

   return newvar;
}



/*
 * Initialize ctx->MinVal and ctx->MaxVal for install_new_grid.
 * Input:  newvar - index of variable to initialize
 */
void min_max_init( Context ctx, int newvar )
{
   ctx->MinVal[newvar] = MISSING;
   ctx->MaxVal[newvar] = -MISSING;
}



/*
 * Allocate a new variable which is computed by an external Fortran program.
 * Input: name - name of the new variable.
 * Return: -1 = error
 *         otherwise, return the number of the new variable.
 */
int allocate_extfunc_variable( Context ctx, char name[] )
{
   int newvar;

   for (newvar=0;newvar<MAXVARS;newvar++) {
      if (ctx->VarType[newvar]==0)
         break;
   }
   if (newvar==MAXVARS) {
      /* no space for a new variable */
      return -1;
   }

   ctx->VarType[newvar] = VIS5D_EXT_FUNC;
   ctx->CloneTable[newvar] = newvar;
   ctx->NumVars++;

   strncpy( ctx->VarName[newvar], name, 8 );
   min_max_init(ctx, newvar);

   return newvar;
}



/*
 * Allocate a new variable which is computed as a function of the other
 * variables by a simple typed-in expression.
 * Input:  name - the name of the new variable
 * Return:  -1 = error (MAXVARS exceeded)
 *          otherwise, return the number of the new variable.
 */
int allocate_computed_variable( Context ctx, char *name )
{
   int newvar;

   for (newvar=0;newvar<MAXVARS;newvar++) {
      if (ctx->VarType[newvar]==0)
         break;
   }
   if (newvar==MAXVARS) {
      /* no space for a new variable */
      return -1;
   }

   ctx->VarType[newvar] = VIS5D_EXPRESSION;
   ctx->CloneTable[newvar] = newvar;
   ctx->NumVars++;

   strncpy( ctx->VarName[newvar], name, 8 );
   min_max_init(ctx, newvar);

   return newvar;
}



/*
 * Allocate a new variable which is initialized to MISSING to be
 * filled in by calls to vis5d_put_grid
 * Input:  name - the name of the new variable
 * Return:  -1 = error (MAXVARS exceeded)
 *          otherwise, return the number of the new variable.
 */
int allocate_new_variable( Context ctx, char *name, int nl, int lowlev )
{
   int newvar, time, gridsize, i;
   float *griddata;

   for (newvar=0;newvar<MAXVARS;newvar++) {
      if (ctx->VarType[newvar]==0)
         break;
   }
   if (newvar==MAXVARS) {
      /* no space for a new variable */
      return -1;
   }

   ctx->VarType[newvar] = VIS5D_PUT;
   ctx->CloneTable[newvar] = newvar;
   ctx->NumVars++;
   ctx->Nl[newvar] = nl;
   ctx->LowLev[newvar] = lowlev;

   strncpy( ctx->VarName[newvar], name, 8 );
   min_max_init(ctx, newvar);

   gridsize = ctx->Nr * ctx->Nc * nl * 4;
   griddata = (float *) allocate ( ctx, gridsize );
   for (i=0; i<gridsize; i++) griddata[i] = MISSING;
   for (time=0; time<ctx->NumTimes; time++) {
     put_grid( ctx, time, newvar, griddata );
   }
   deallocate( ctx, griddata, gridsize );

   return newvar;
}



/*** deallocate_variable **********************************************
   deallocate the last variable made with one of the 2 previous
   functions.  This should only be used in case of an error when
   cloning/computing a new variable.
**********************************************************************/
int deallocate_variable( Context ctx, int var )
{
   /* This function isn't completely implemented nor is it used yet */
   assert( var == ctx->NumVars-1 );

   ctx->VarType[var] = 0;

   ctx->NumVars--;
   return 0;
}




/*** install_new_grid *************************************************
   When new grids are computed by an external analysis function, we
   call this function to install the data into Vis5D's globals data
   structures.
   Input:  it, var - times step and variable number of the grid which
                    we're installing.
           griddata - array of grid data
           nl - number of levels in new grid (we know Nr and Nc)
           lowlev - lowest level in new grid
   Return:  1=ok, 0=error
**********************************************************************/
int install_new_grid( Context ctx, int time, int var,
                      float *griddata, int nl, int lowlev)
{
   float min, max;

   ctx->Nl[var] = nl;
   ctx->LowLev[var] = lowlev;

   if (!ctx->GridTable[time][var].Data) {
      int bytes = ctx->Nr * ctx->Nc * nl * ctx->CompressMode;
      ctx->GridTable[time][var].Data = (void *) allocate( ctx, bytes );
      if (ctx->Ga[time][var]){
         deallocate( ctx, ctx->Ga[time][var], -1);
         ctx->Ga[time][var] = NULL;
      }
      if (ctx->Gb[time][var]){
         deallocate( ctx, ctx->Gb[time][var], -1);
         ctx->Gb[time][var] = NULL;
      }
      ctx->Ga[time][var] = (float *) allocate( ctx, nl * sizeof(float) );
      ctx->Gb[time][var] = (float *) allocate( ctx, nl * sizeof(float) );
      if (!ctx->GridTable[time][var].Data
          || !ctx->Ga[time][var] || !ctx->Gb[time][var]) {
         printf("Out of memory, couldn't save results of external ");
         printf("function computation.\n");
         return 0;
      }
   }
   /* compress the data */
   v5dCompressGrid( ctx->Nr, ctx->Nc, nl, ctx->CompressMode, griddata,
                    ctx->GridTable[time][var].Data,
                    ctx->Ga[time][var], ctx->Gb[time][var], &min, &max );

   ctx->GridTable[time][var].CachePos = -1;

   /* update min and max values */
   if (min<ctx->MinVal[var]) {
      ctx->MinVal[var] = min;
      ctx->RealMinVal[var] = min;
   }
   if (max>ctx->MaxVal[var]) {
      ctx->MaxVal[var] = max;
      ctx->RealMaxVal[var] = max;
   }

   return 1;
}


