/*  igrid.c */

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
#include "igrid.h"
#include "globals.h"
#include "memory.h"
#include "proj.h"
#include "sync.h"
#include "v5d.h"


#ifndef M_PI
#  define M_PI 3.14159265
#endif

#define DEG2RAD (M_PI/180.0)
#define RAD2DEG (180.0/M_PI)




/*
 * Open a record file, read the header, and initialize a bunch of
 * global variables.
 * Input:  filename - name of file 
 * Return:  1 = success, 0 = error.
 */
int open_recordfile( Irregular_Context itx, char filename[] )
{
   char name[1000];
   int i, time, var, first;

   if (!initially_open_recordfile( filename, &itx->G)) {
      return 0;
   }

   /* Initalize parameter type table */
   for (i=0;i<MAXVARS;i++) {
      itx->VarType[i] = 0;
   }

   /* Copy header info from G to global variables */
   itx->NumTimes = itx->G.NumTimes;
   itx->NumVars = itx->G.NumVars;
   itx->Nr = itx->G.Nr;
   itx->Nc = itx->G.Nc;
   itx->MaxNl = 0;
   for (var=0;var<itx->NumVars;var++) {
      itx->Nl[var] = itx->G.Nl[var];
      itx->LowLev[var] = itx->G.LowLev[var];
      if (itx->Nl[var]+itx->LowLev[var]>itx->MaxNl) {
         itx->MaxNl = itx->Nl[var]+itx->LowLev[var];
         itx->MaxNlVar = var;
      }
      strncpy( itx->VarName[var], itx->G.VarName[var], 8 );
      strncpy( itx->Units[var], itx->G.Units[var], 19 );
      itx->MinVal[var] = itx->G.MinVal[var];
      itx->MaxVal[var] = itx->G.MaxVal[var];
      itx->VarType[var] = VIS5D_REGULAR;
      itx->CloneTable[var] = var;
   }

   /* MJK 4-13-98 
   if (itx->NumVars == 1 && itx->MaxNl == 1){
      itx->MaxNl = 2;
      itx->Nl[0] = 2;
   }
   end MJK*/

   /* Check that grid isn't too big */
   if (itx->NumTimes>MAXTIMES) {
      printf("Error: Too many time steps (%d) limit is %d\n", itx->NumTimes,
             MAXTIMES );
      return 0;
   }
   if (itx->NumVars>MAXVARS) {
      printf("Error: Too many variables (%d) limit is %d\n", itx->NumVars,
             MAXVARS );
      return 0;
   }
   if (itx->Nr>MAXROWS) {
      printf("Error: Number of grid rows (%d) too large, %d is limit.\n",
             itx->Nr,MAXROWS);
      printf("Edit src/v5d.h and increase MAXROWS\n");
      return 0;
   }
   if (itx->Nc>MAXCOLUMNS) {
      printf("Error: Number of grid columns (%d) too large, %d is limit.\n",
             itx->Nc, MAXCOLUMNS );
      printf("Edit src/v5d.h and increase MAXCOLUMNS\n");
      return 0;
   }
   if (itx->MaxNl>MAXLEVELS) {
      printf("Error: Number of grid levels (%d) too large, %d is limit.\n",
             itx->Nl, MAXLEVELS );
      printf("Edit src/v5d.h and increase MAXLEVELS\n");
      return 0;
   }

   /* convert time from HHMMSS to seconds since midnight */
   /* convert date from YYDDD to days since Jan, 1900 */
   for (time=0;time<itx->NumTimes;time++) {
      itx->TimeStamp[time] = v5dHHMMSStoSeconds( itx->G.TimeStamp[time] );
      itx->DayStamp[time] = v5dYYDDDtoDays( itx->G.DateStamp[time] );
   }

   itx->CompressMode = itx->G.CompressMode;

   /* calculate elapsed time (in seconds) for each time since initial time */
   first = itx->DayStamp[0]* 24*60*60 + itx->TimeStamp[0];
   for (time=0;time<itx->NumTimes;time++) {
      itx->Elapsed[time] = itx->DayStamp[time] * 24*60*60
                         + itx->TimeStamp[time] - first;
   }

   return 1;
}



int free_grid_cache( Irregular_Context itx )
{
   int it, iv;

   for (it=0; it<MAXTIMES; it++){
      for (iv=0; iv<MAXVARS; iv++){
         if (itx->Ga[it][iv]){
            deallocate(itx, itx->Ga[it][iv], -1);
            itx->Ga[it][iv] = NULL;
         }
         if (itx->Gb[it][iv]){
            deallocate( itx, itx->Gb[it][iv], -1);
            itx->Gb[it][iv] = NULL;
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
int init_grid_cache( Irregular_Context itx, int maxbytes, float *ratio )
{
   int i, it, iv;
   int maxnl, gridsize;

   free_grid_cache( itx );

   /* First allocate space for ga/gb compression values */
   for (it=0;it<itx->NumTimes;it++) {
      for (iv=0;iv<itx->NumVars;iv++) {
         itx->Ga[it][iv] = (float *) allocate( itx, itx->Nl[iv] * sizeof(float) );
         itx->Gb[it][iv] = (float *) allocate( itx, itx->Nl[iv] * sizeof(float) );
      }
   }

   ALLOC_LOCK( itx->Mutex );   /* Allocate the mutex lock */

   /* Determine the maximum number of grids to cache given the maximum */
   /* number of bytes of memory to use. */
   maxnl = 0;
   for (iv=0; iv<itx->NumVars; iv++) {
      if (itx->Nl[iv] > maxnl) {
         maxnl = itx->Nl[iv];
      }
   }
   gridsize = itx->Nr * itx->Nc * maxnl * itx->CompressMode;
   itx->MaxCachedGrids = (int) (maxbytes / gridsize);

   if (itx->MaxCachedGrids >= itx->NumTimes*itx->NumVars) {
      /* the whole file can be cached */
      itx->MaxCachedGrids = itx->NumTimes*itx->NumVars;
      *ratio = 1.0;
   }
   else {
      *ratio = ((float) itx->MaxCachedGrids)
             / ((float) (itx->NumTimes*itx->NumVars));
   }

   itx->NumCachedGrids = 0;

   printf("Cache size: %d grids\n", itx->MaxCachedGrids );
  
   if (itx->MaxCachedGrids != itx->NumTimes*itx->NumVars){
      int needed;
      needed = (((gridsize * itx->NumTimes * itx->NumVars)
               * 5 / 2) / (1024*1024));
      needed = (int) ( (float) needed * 1.25) + 2;
      printf(" Hint... To run Vis5D more efficiently try setting %s to '-mbs %d'\n",
               itx->DataFile, needed);
   }
      
   /* Allocate the itx->GridCache array */
   itx->GridCache = (struct cache_rec *) allocate( itx, itx->MaxCachedGrids
                                                * sizeof(struct cache_rec) );
   if (!itx->GridCache) {
      printf("Error: out of memory.  Couldn't allocate cache table.\n");
      return 0;
   }

   itx->CacheClock = 1;

   /* Initialize tables */
   for (i=0;i<itx->MaxCachedGrids;i++) {
      itx->GridCache[i].Data = (void *) allocate( itx, gridsize );
      if (!itx->GridCache[i].Data) {
         printf("Error: out of memory.  Couldn't allocate cache space.\n");
         return 0;
      }
      itx->GridCache[i].Locked = 0;
      itx->GridCache[i].Timestep = 0;
      itx->GridCache[i].Var = 0;
   }
   for (it=0;it<itx->NumTimes;it++) {
      for (iv=0;iv<MAXVARS;iv++) {
         itx->GridTable[it][iv].CachePos = -1;
         itx->GridTable[it][iv].Data = NULL;
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
 * Return an index into the itx->GridCache array which corresponds to an empty
 * position.  If the cache is full, we'll discard something.
 */
int get_empty_cache_pos( Irregular_Context itx )
{
   int g;

   /* find g */
   if (itx->NumCachedGrids<itx->MaxCachedGrids) {
      /* There's an unused position. */
      g = itx->NumCachedGrids;
      itx->NumCachedGrids++;
   }
   else {
      int time, var;
#ifdef RANDOM
      /* pick a cache position at random for replacement */
      g = rand() % itx->MaxCachedGrids;
      while (itx->GridCache[g].Locked) {
         g++;
         if (g>=itx->MaxCachedGrids)
            g = 0;
      }
      printf("Random discard %d\n", g );
#else
      /* LRU */
      int minage, i, mini;
      minage = itx->CacheClock;
      for (i=0;i<itx->MaxCachedGrids;i++) {
         if (itx->GridCache[i].Age<minage && itx->GridCache[i].Locked==0) {
            minage = itx->GridCache[i].Age;
            mini = i;
         }
      }
      g = mini;

      /* remove references to data being discarded */
      time = itx->GridCache[g].Timestep;
      var = itx->GridCache[g].Var;
      itx->GridTable[time][var].Data = NULL;
      itx->GridTable[time][var].CachePos = -1;
#endif
   }

   itx->GridCache[g].Locked = 1;
   return g;
}




/*** get_compressed_grid **********************************************
   Return a pointer to the compressed data for a 3-D grid.
   Input: time, var - time and variable of grid wanted.
          ga, gb - pointer to pointer to float.
   Output: ga, gb - array of values to use for decompressing.
   Return:  pointer to 1, 2 or 4-byte data values
**********************************************************************/
static void *get_compressed_grid( Irregular_Context itx, int time, int var,
                                  float **ga, float **gb )
{
  int p;

  var = itx->CloneTable[var];

  LOCK_ON( itx->Mutex );

  if (itx->GridTable[time][var].Data) {
    /* already in the cache */
    p = itx->GridTable[time][var].CachePos;
    if (p>=0) {
      itx->GridCache[p].Locked = 1;
      itx->GridCache[p].Age = itx->CacheClock++;
    }
    LOCK_OFF( itx->Mutex );
    *ga = itx->Ga[time][var];
    *gb = itx->Gb[time][var];
    return itx->GridTable[time][var].Data;
  }
  else {
    /* not in the cache */
    int g;
    g = get_empty_cache_pos(itx);

    /*printf("Reading grid into pos %d\n", g );*/
    if (!v5dReadCompressedGrid( &itx->G, time, var,
                                itx->Ga[time][var], itx->Gb[time][var],
                                itx->GridCache[g].Data )) {
      printf("Error: unable to read grid (time=%d, var=%d)\n",
             time, var );
      LOCK_OFF( itx->Mutex );
      return NULL;
    }

    itx->GridTable[time][var].Data = itx->GridCache[g].Data;
    itx->GridTable[time][var].CachePos = g;
    itx->GridCache[g].Locked = 1;
    itx->GridCache[g].Timestep = time;
    itx->GridCache[g].Var = var;
    itx->GridCache[g].Age = itx->CacheClock++;

    LOCK_OFF( itx->Mutex );
    *ga = itx->Ga[time][var];
    *gb = itx->Gb[time][var];
    return itx->GridTable[time][var].Data;
  }
}




/*** release_compressed_grid ******************************************
   Release a compressed grid.
   Input:  time, var - the timestep and variable of grid to release.
**********************************************************************/
void release_compressed_grid( Irregular_Context itx, int time, int var )
{
   int p;

   /* just unlock */
   LOCK_ON( itx->Mutex );
   p = itx->GridTable[time][var].CachePos;
   if (p>=0) {
      itx->GridCache[ p ].Locked = 0;
   }
   LOCK_OFF( itx->Mutex );
}



/*
 * Load some or all of the grid data into main memory.
 */
void preload_cache( Irregular_Context itx )
{
   if (itx->NumTimes*itx->NumVars <= itx->MaxCachedGrids) {
      /* All grids will fit in the cache.  Read whole file now. */
      int time, var;
      printf("Reading all grids.\n");
      for (time=0;time<itx->NumTimes;time++) {
         for (var=0;var<itx->NumVars;var++) {
            float *ga, *gb;
            void *d;
            d = get_compressed_grid( itx, time, var, &ga, &gb );
            if (d) release_compressed_grid( itx, time, var );
         }
      }
   }
}



/*** get_grid *********************************************************
   Return a pointer to the uncompressed data for a 3-D grid.
   Input:  time, var - time and variable of grid wanted.
   Return:  pointer to float data
**********************************************************************/
float *get_grid( Irregular_Context itx, int time, int var )
{
   float *data, *ga, *gb;
   void *compdata;
   int nrncnl;

   var = itx->CloneTable[var];
   nrncnl = itx->Nr * itx->Nc * itx->Nl[var];
   data = (float *) allocate_type( itx, nrncnl*sizeof(float), GRID_TYPE );
   if (!data) {
      return NULL;
   }

   compdata = get_compressed_grid( itx, time, var, &ga, &gb );
   if (compdata) {
     v5dDecompressGrid( itx->Nr, itx->Nc, itx->Nl[var], itx->CompressMode,
                        compdata, ga, gb, data );
     release_compressed_grid( itx, time, var );
   }

   return data;
}

/* time = fromitx time */
/* var = fromitx var */
float *get_grid2( Irregular_Context toitx, Irregular_Context fromitx, int time, int var, int numlevs )
{
   float *data, *ga, *gb, fdata;
   void *compdata;
   int nrncnl, nr, nc, nl;
   float yonrr, yoncc, yonll, nnr, nnc, nnl, lat, lon, hgt;

   var = fromitx->CloneTable[var];
   nrncnl = toitx->Nr * toitx->Nc * numlevs;
   data = (float *) allocate_type( toitx, nrncnl*sizeof(float), GRID_TYPE );
   if (!data) {
      return NULL;
   }

   for (nr = 0; nr < toitx->Nr; nr++){
      for (nc = 0; nc < toitx->Nc; nc++){
         for (nl = 0; nl < numlevs; nl++){
             yonrr = (float) (nr);
             yoncc = (float) (nc);
             yonll = (float) (nl);
             grid_to_geo(toitx, 0, 0, 1, &yonrr, &yoncc, &yonll,
                         &lat, &lon, &hgt);
             geo_to_grid(fromitx, time, var, 1, &lat,
                         &lon, &hgt, &nnr, &nnc, &nnl);
             fdata = interpolate_grid_value( fromitx, time, var,
                         nnr, nnc, nnl);
             data[nr+toitx->Nr*(nc+toitx->Nc*nl)] = fdata;
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
int put_grid( Irregular_Context itx, int time, int var, float *griddata )
{
   if (itx->VarType[var] == VIS5D_REGULAR ||
       itx->VarType[var] == VIS5D_PUT) {
     return install_new_grid( itx, time, var, griddata, itx->Nl[var],
                              itx->LowLev[var]);
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
void release_grid( Irregular_Context itx, int time, int var, float *data )
{
   assert( time>=0 && time<itx->NumTimes );
   assert( var>=0 && var<itx->NumVars );

   deallocate( itx, data, itx->Nr*itx->Nc*itx->Nl[var]*sizeof(float) );
}


void release_grid2( Irregular_Context itx, int time, int var, int nl, float *data )
{
   deallocate( itx, data, itx->Nr*itx->Nc*nl*sizeof(float) );
}


/*
 * Get a discrete grid value.
 * Input:  time - which timestep
 *         var - which variable
 *         row, col, lev - location
 * Return:  grid value or MISSING if missing.
 */
float get_grid_value( Irregular_Context itx, int time, int var,
                      int row, int col, int lev )
{
   void *data;
   float *gavec, *gbvec;
   float value;

   /* WLH 6-30-95 */
   lev -= itx->LowLev[var];
   if (lev < 0 || lev >= itx->Nl[var]) return MISSING;

   var = itx->CloneTable[var];
   data = get_compressed_grid( itx, time, var, &gavec, &gbvec );
   if (!data) return MISSING;

   if (itx->CompressMode == 1) {
      V5Dubyte *data1 = (V5Dubyte *) data;
      V5Dubyte c1 = data1[ (lev * itx->Nc + col) * itx->Nr + row ];
      if (c1==255) {
         value = MISSING;
      }
      else {
         value = (float) (int) c1 * gavec[lev] + gbvec[lev];
      }
   }
   else if (itx->CompressMode == 2) {
      V5Dushort *data2 = (V5Dushort *) data;
      V5Dushort c2 = data2[ (lev * itx->Nc + col) * itx->Nr + row ];
      if (c2==65535) {
         value = MISSING;
      }
      else {
         value = (float) (int) c2 * gavec[lev] + gbvec[lev];
      }
   }
   else {
      float *data4 = (float *) data;
      value = data4[ (lev * itx->Nc + col) * itx->Nr + row ];
   }

   release_compressed_grid( itx, time, var );

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
float interpolate_grid_value( Irregular_Context itx, int time, int var,
                              float row, float col, float lev )
{
   void *data;
   int i0, j0, k0, i1, j1, k1;
   float d0,d1,d2,d3,d4,d5,d6,d7, d;
   float ei, ej, ek;
   float *gavec, *gbvec, ga, gb;

   /* WLH 6-30-95 */
   lev -= itx->LowLev[var];
   if (lev < 0 || lev >= itx->Nl[var] ||
       col < 0 || col >= itx->Nc ||
       row < 0 || row >= itx->Nr){
      return MISSING;
   }

   var = itx->CloneTable[var];

   data = get_compressed_grid( itx, time, var, &gavec, &gbvec );
   if (!data) return MISSING;

   i0 = (int) row;  i1 = i0 + 1;
   j0 = (int) col;  j1 = j0 + 1;
   k0 = (int) lev;  k1 = k0 + 1;
   
   /* check for grid boundaries */
   if (i0==itx->Nr-1) {
      i1 = i0;
   }
   if (j0==itx->Nc-1) {
      j1 = j0;
   }
   if (k0==itx->Nl[var]-1) {
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

   if (itx->CompressMode == 1) {
      /* get eight values at corners of a cube around (r,c,l) */
      V5Dubyte c0,c1,c2,c3,c4,c5,c6,c7;
      V5Dubyte *data1 = (V5Dubyte *) data;

      c0 = data1[ (k0 * itx->Nc + j0) * itx->Nr + i0 ];   /* d0 @ (i0,j0,k0) */
      c1 = data1[ (k0 * itx->Nc + j0) * itx->Nr + i1 ];   /* d1 @ (i1,j0,k0) */
      c2 = data1[ (k0 * itx->Nc + j1) * itx->Nr + i0 ];   /* d2 @ (i0,j1,k0) */
      c3 = data1[ (k0 * itx->Nc + j1) * itx->Nr + i1 ];   /* d3 @ (i1,j1,k0) */
      c4 = data1[ (k1 * itx->Nc + j0) * itx->Nr + i0 ];   /* d4 @ (i0,j0,k1) */
      c5 = data1[ (k1 * itx->Nc + j0) * itx->Nr + i1 ];   /* d5 @ (i1,j0,k1) */
      c6 = data1[ (k1 * itx->Nc + j1) * itx->Nr + i0 ];   /* d6 @ (i0,j1,k1) */
      c7 = data1[ (k1 * itx->Nc + j1) * itx->Nr + i1 ];   /* d7 @ (i1,j1,k1) */

      release_compressed_grid( itx, time, var );

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
   else if (itx->CompressMode == 2) {
      V5Dushort c0,c1,c2,c3,c4,c5,c6,c7;
      V5Dushort *data2 = (V5Dushort *) data;

      /* get eight values at corners of a cube around (r,c,l) */
      c0 = data2[ (k0 * itx->Nc + j0) * itx->Nr + i0 ];   /* d0 @ (i0,j0,k0) */
      c1 = data2[ (k0 * itx->Nc + j0) * itx->Nr + i1 ];   /* d1 @ (i1,j0,k0) */
      c2 = data2[ (k0 * itx->Nc + j1) * itx->Nr + i0 ];   /* d2 @ (i0,j1,k0) */
      c3 = data2[ (k0 * itx->Nc + j1) * itx->Nr + i1 ];   /* d3 @ (i1,j1,k0) */
      c4 = data2[ (k1 * itx->Nc + j0) * itx->Nr + i0 ];   /* d4 @ (i0,j0,k1) */
      c5 = data2[ (k1 * itx->Nc + j0) * itx->Nr + i1 ];   /* d5 @ (i1,j0,k1) */
      c6 = data2[ (k1 * itx->Nc + j1) * itx->Nr + i0 ];   /* d6 @ (i0,j1,k1) */
      c7 = data2[ (k1 * itx->Nc + j1) * itx->Nr + i1 ];   /* d7 @ (i1,j1,k1) */

      release_compressed_grid( itx, time, var );

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
      d0 = data4[ (k0 * itx->Nc + j0) * itx->Nr + i0 ];   /* d0 @ (i0,j0,k0) */
      d1 = data4[ (k0 * itx->Nc + j0) * itx->Nr + i1 ];   /* d1 @ (i1,j0,k0) */
      d2 = data4[ (k0 * itx->Nc + j1) * itx->Nr + i0 ];   /* d2 @ (i0,j1,k0) */
      d3 = data4[ (k0 * itx->Nc + j1) * itx->Nr + i1 ];   /* d3 @ (i1,j1,k0) */
      d4 = data4[ (k1 * itx->Nc + j0) * itx->Nr + i0 ];   /* d4 @ (i0,j0,k1) */
      d5 = data4[ (k1 * itx->Nc + j0) * itx->Nr + i1 ];   /* d5 @ (i1,j0,k1) */
      d6 = data4[ (k1 * itx->Nc + j1) * itx->Nr + i0 ];   /* d6 @ (i0,j1,k1) */
      d7 = data4[ (k1 * itx->Nc + j1) * itx->Nr + i1 ];   /* d7 @ (i1,j1,k1) */

      release_compressed_grid( itx, time, var );

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
                          clone (in [0..itx->NumVars-1]).
   Return: -1 = error
           otherwise, return the number of the new variable.
**********************************************************************/
int allocate_clone_variable( Irregular_Context itx, char name[], int var_to_clone )
{
   int newvar;

   for (newvar=0;newvar<MAXVARS;newvar++) {
      if (itx->VarType[newvar]==0)
         break;
   }
   if (newvar==MAXVARS) {
      /* no space for a new variable */
      return -1;
   }

   itx->VarType[newvar] = VIS5D_CLONE;
   itx->CloneTable[newvar] = var_to_clone;
   itx->NumVars++;

   /* copy variable/parameter name */
   strncpy( itx->VarName[newvar], name, 8 );

   itx->Nl[newvar] = itx->Nl[var_to_clone];
   itx->LowLev[newvar] = itx->LowLev[var_to_clone];

   /* copy min, max values */
   itx->MinVal[newvar] = itx->MinVal[var_to_clone];
   itx->MaxVal[newvar] = itx->MaxVal[var_to_clone];

   return newvar;
}



/*
 * Initialize itx->MinVal and itx->MaxVal for install_new_grid.
 * Input:  newvar - index of variable to initialize
 */
void min_max_init( Irregular_Context itx, int newvar )
{
   itx->MinVal[newvar] = MISSING;
   itx->MaxVal[newvar] = -MISSING;
}



/*
 * Allocate a new variable which is computed by an external Fortran program.
 * Input: name - name of the new variable.
 * Return: -1 = error
 *         otherwise, return the number of the new variable.
 */
int allocate_extfunc_variable( Irregular_Context itx, char name[] )
{
   int newvar;

   for (newvar=0;newvar<MAXVARS;newvar++) {
      if (itx->VarType[newvar]==0)
         break;
   }
   if (newvar==MAXVARS) {
      /* no space for a new variable */
      return -1;
   }

   itx->VarType[newvar] = VIS5D_EXT_FUNC;
   itx->CloneTable[newvar] = newvar;
   itx->NumVars++;

   strncpy( itx->VarName[newvar], name, 8 );
   min_max_init(itx, newvar);

   return newvar;
}



/*
 * Allocate a new variable which is computed as a function of the other
 * variables by a simple typed-in expression.
 * Input:  name - the name of the new variable
 * Return:  -1 = error (MAXVARS exceeded)
 *          otherwise, return the number of the new variable.
 */
int allocate_computed_variable( Irregular_Context itx, char *name )
{
   int newvar;

   for (newvar=0;newvar<MAXVARS;newvar++) {
      if (itx->VarType[newvar]==0)
         break;
   }
   if (newvar==MAXVARS) {
      /* no space for a new variable */
      return -1;
   }

   itx->VarType[newvar] = VIS5D_EXPRESSION;
   itx->CloneTable[newvar] = newvar;
   itx->NumVars++;

   strncpy( itx->VarName[newvar], name, 8 );
   min_max_init(itx, newvar);

   return newvar;
}



/*
 * Allocate a new variable which is initialized to MISSING to be
 * filled in by calls to vis5d_put_grid
 * Input:  name - the name of the new variable
 * Return:  -1 = error (MAXVARS exceeded)
 *          otherwise, return the number of the new variable.
 */
int allocate_new_variable( Irregular_Context itx, char *name, int nl, int lowlev )
{
   int newvar, time, gridsize, i;
   float *griddata;

   for (newvar=0;newvar<MAXVARS;newvar++) {
      if (itx->VarType[newvar]==0)
         break;
   }
   if (newvar==MAXVARS) {
      /* no space for a new variable */
      return -1;
   }

   itx->VarType[newvar] = VIS5D_PUT;
   itx->CloneTable[newvar] = newvar;
   itx->NumVars++;
   itx->Nl[newvar] = nl;
   itx->LowLev[newvar] = lowlev;

   strncpy( itx->VarName[newvar], name, 8 );
   min_max_init(itx, newvar);

   gridsize = itx->Nr * itx->Nc * nl * 4;
   griddata = (float *) allocate ( itx, gridsize );
   for (i=0; i<gridsize; i++) griddata[i] = MISSING;
   for (time=0; time<itx->NumTimes; time++) {
     put_grid( itx, time, newvar, griddata );
   }
   deallocate( itx, griddata, gridsize );

   return newvar;
}



/*** deallocate_variable **********************************************
   deallocate the last variable made with one of the 2 previous
   functions.  This should only be used in case of an error when
   cloning/computing a new variable.
**********************************************************************/
int deallocate_variable( Irregular_Context itx, int var )
{
   /* This function isn't completely implemented nor is it used yet */
   assert( var == itx->NumVars-1 );

   itx->VarType[var] = 0;

   itx->NumVars--;
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
int install_new_grid( Irregular_Context itx, int time, int var,
                      float *griddata, int nl, int lowlev)
{
   float min, max;

   itx->Nl[var] = nl;
   itx->LowLev[var] = lowlev;

   if (!itx->GridTable[time][var].Data) {
      int bytes = itx->Nr * itx->Nc * nl * itx->CompressMode;
      itx->GridTable[time][var].Data = (void *) allocate( itx, bytes );
      if (itx->Ga[time][var]){
         deallocate( itx, itx->Ga[time][var], -1);
         itx->Ga[time][var] = NULL;
      }
      if (itx->Gb[time][var]){
         deallocate( itx, itx->Gb[time][var], -1);
         itx->Gb[time][var] = NULL;
      }
      itx->Ga[time][var] = (float *) allocate( itx, nl * sizeof(float) );
      itx->Gb[time][var] = (float *) allocate( itx, nl * sizeof(float) );
      if (!itx->GridTable[time][var].Data
          || !itx->Ga[time][var] || !itx->Gb[time][var]) {
         printf("Out of memory, couldn't save results of external ");
         printf("function computation.\n");
         return 0;
      }
   }
   /* compress the data */
   v5dCompressGrid( itx->Nr, itx->Nc, nl, itx->CompressMode, griddata,
                    itx->GridTable[time][var].Data,
                    itx->Ga[time][var], itx->Gb[time][var], &min, &max );

   itx->GridTable[time][var].CachePos = -1;

   /* update min and max values */
   if (min<itx->MinVal[var]) {
      itx->MinVal[var] = min;
      itx->RealMinVal[var] = min;
   }
   if (max>itx->MaxVal[var]) {
      itx->MaxVal[var] = max;
      itx->RealMaxVal[var] = max;
   }

   return 1;
}


