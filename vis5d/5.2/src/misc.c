/*  misc.c */

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
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "api.h"
#include "globals.h"
#include "memory.h"
#include "misc.h"
#include "proj.h"
#include "sync.h"
#include "vis5d.h"



/*
 * Print an error message and abort.
 */
void die( char *msg )
{
   printf("Error: %s\n",msg);
   abort();
}




/*
 * C implementation of unix 'which' command:  find the full path to
 * where the specified executable resides.
 * Input:  file - executable file to search for
 * Output:  fullpath - the full path/name for the file
 * Return:  1 = ok, 0 = file not found
 */
int which( char *file, char *fullpath )
{
   char *pathvar, *pos;
   char path[1000];
   int len;
   char s[1000];
   struct stat buf;

   /* get the value of PATH from the user's environment */
   pathvar = getenv( "PATH" );

   /* try each path */
   len = 0;
   for (pos=pathvar;*pos;pos++) {
      if (*pos==':') {

         path[len] = 0;
         /* try it */
         strcpy( s, path );
         strcat( s, "/" );
         strcat( s, file );

         if (stat(s, &buf) == 0) {
            /* found */
            if ((buf.st_mode & S_IEXEC)) {
               /* and executable */
               strcpy( fullpath, s );
               return 1;
            }
         }
         /* go on to next path */
         len = 0;
      }
      else {
         path[len++] = *pos;
      }
   }

   /* if we get here, we didn't find visad-d, try the current directory */
   strcpy( s, "./" );
   strcat( s, file );

   if (stat(s, &buf)==0 && (buf.st_mode & S_IEXEC)) {
      /* found it! */
      strcpy( fullpath, s );
      return 1;
   }

   return 0;
}



/*
 * Check if the named program is installed on the system.
 * Return:  1 = installed, 0 = not installed.
 */
int installed( char *program )
{
   char path[1000];

   if (which(program,path)==0) {
/*
      printf("Can't save/print window image because '%s' not found.\n",
              program);
      printf("Install it from your OS CD/tape.\n");
*/
      return 0;
   }
   else {
      return 1;
   }
}



/*** round ************************************************************
   Round off x to a 'nice' value.
**********************************************************************/
float round( float x )
{
   float base, fudge;
   int temp;

   for (base=1000000.0; fabs(x/base)<1.0 && base>0.000001; base/=10.0);

   fudge = (x>0.0) ? 0.5 : -0.5;
   temp = (int) (x/base + fudge);
   return temp * base;
}

/* Input: index - the index of the vis5d_ctx
 * Output: it will return the position in the Display_Context's
 *         ctxarray of the desired vis5d_ctx index
 */
int return_ctx_index_pos( Display_Context dtx, int index)
{
   int yo, spandex;
   for (yo = 0; yo < dtx->numofctxs; yo++){
      spandex = dtx->ctxarray[yo];
      if (spandex == index){
         return yo;
      }
   }
   /*printf("error in return_ctx_index_pos\n"); */
   return -1;
}

/* Input: index - the index of the vis5d_ctx
 *        time - the timestep of the display_ctx
 * Output: return the timestep associates with the display_ctx's
 *         timestep
 */
int return_ctx_time( Display_Context dtx, int index, int time)
{
   int yo;
   for (yo = 0; yo < dtx->numofctxs; yo++){
      if (index == dtx->TimeStep[time].owners[yo]){
         return dtx->TimeStep[time].ownerstimestep[yo];
      }
   }
   printf("error in return_ctx_time\n");
   return -1;
}

/*
 * When a horizontal slice is moved to a new position (in grid coords),
 * this function should be called to update the position in Z and height.
 * Input:  level - position of slice in grid levels
 *         z - position of slice in Z graphics coords
 *         hgt - position of slice in height coords.
 */
void new_hslice_pos( Context ctx, float level, float *z, float *hgt )
{
   *hgt = gridlevelPRIME_to_height( ctx->dpy_ctx, level);
   *z = height_to_zPRIME(ctx->dpy_ctx, *hgt);
}

void new_hwindslice_pos( Display_Context dtx, float level, float *z, float *hgt )
{
   *z = gridlevelPRIME_to_zPRIME( dtx, -1, -1, level );
   *hgt =gridlevelPRIME_to_height( dtx, level );
}


/*
 * Similar function to new_hslice_pos().
 * NOTE: the graphics and geo coordinates are PRIME
 */
void new_vslice_pos( Context ctx, float r, float c,
                     float *x, float *y, float *lat, float *lon )
{
   float zero = 0.0, z;
   float row, col;

   row = r;  /* this is needed because r and c are really doubles */
   col = c;

   gridPRIME_to_xyzPRIME( ctx->dpy_ctx, -1, -1, 1, &row, &col, &zero, x, y, &z );
   rowcolPRIME_to_latlon( ctx->dpy_ctx, -1, -1, r, c, lat, lon );
}

void new_vwindslice_pos( Display_Context dtx, float r, float c,
                     float *x, float *y, float *lat, float *lon )
{
   float zero = 0.0, z;
   float row, col;

   row = r;  /* this is needed because r and c are really doubles */
   col = c;

   gridPRIME_to_xyzPRIME( dtx, -1, -1, 1, &row, &col, &zero, x, y, &z );
   rowcolPRIME_to_latlon( dtx, -1, -1, r, c, lat, lon );
}



/*
 * Set the initial position, etc. of the graphics for a specified variable.
 */
void init_graphics_pos( Context ctx, int var )
{
   Display_Context dtx;
   float r1, c1, r2, c2, l;

   dtx = ctx->dpy_ctx;
   ctx->IsoLevel[var] = ctx->MinVal[var];

   l =  ((float) dtx->LowLev + (float) (dtx->Nl-1) / 2);

   ctx->HSliceLevel[var] = l;
   new_hslice_pos( ctx, ctx->HSliceLevel[var], &ctx->HSliceZ[var],
                   &ctx->HSliceHgt[var] );
   if (ctx->MinVal[var] > ctx->MaxVal[var]) {
      ctx->HSliceInterval[var] = 0.0;
   }
   else {
      ctx->HSliceInterval[var] = round( (ctx->MaxVal[var] - ctx->MinVal[var])
                                        / 10.0 );
   }

   ctx->HSliceLowLimit[var] = ctx->MinVal[var];
   ctx->HSliceHighLimit[var] = ctx->MaxVal[var];

   ctx->CHSliceLevel[var] = l;
   new_hslice_pos( ctx, ctx->CHSliceLevel[var],
                   &ctx->CHSliceZ[var], &ctx->CHSliceHgt[var] );

   ctx->VSliceR1[var] = (float) (dtx->Nr-1) / 2.0;
   ctx->VSliceC1[var] = 0.0;
   ctx->VSliceR2[var] = (float) (dtx->Nr-1) / 2.0;
   ctx->VSliceC2[var] = (float) (dtx->Nc-1);
   if (ctx->MinVal[var] > ctx->MaxVal[var]) {
      ctx->VSliceInterval[var] = 0.0;
   }
   else {
      ctx->VSliceInterval[var] = round( (ctx->MaxVal[var] - ctx->MinVal[var])
                                        / 10.0 );
   }

   ctx->VSliceLowLimit[var] = ctx->MinVal[var];
   ctx->VSliceHighLimit[var] = ctx->MaxVal[var];
   new_vslice_pos( ctx, ctx->VSliceR1[var], ctx->VSliceC1[var],
                   &ctx->VSliceX1[var], &ctx->VSliceY1[var],
                   &ctx->VSliceLat1[var], &ctx->VSliceLon1[var] );
   new_vslice_pos( ctx, ctx->VSliceR2[var], ctx->VSliceC2[var],
                   &ctx->VSliceX2[var], &ctx->VSliceY2[var],
                   &ctx->VSliceLat2[var], &ctx->VSliceLon2[var] );

   ctx->CVSliceR1[var] = ctx->VSliceR1[var];
   ctx->CVSliceC1[var] = ctx->VSliceC1[var]; 
   ctx->CVSliceR2[var] = ctx->VSliceR2[var];
   ctx->CVSliceC2[var] = ctx->VSliceC2[var];
   new_vslice_pos( ctx, ctx->CVSliceR1[var], ctx->CVSliceC1[var],
                   &ctx->CVSliceX1[var], &ctx->CVSliceY1[var],
                   &ctx->CVSliceLat1[var], &ctx->CVSliceLon1[var] );
   new_vslice_pos( ctx, ctx->CVSliceR2[var], ctx->CVSliceC2[var],
                   &ctx->CVSliceX2[var], &ctx->CVSliceY2[var],
                   &ctx->CVSliceLat2[var], &ctx->CVSliceLon2[var] );
}




/*
 * Free a traj struct and any memory it points to.
 */
static void free_traj( Display_Context dtx, struct traj *t )
{
   int bytes;
   Context trajuctx;

   trajuctx = dtx->ctxpointerarray[return_ctx_index_pos(dtx, dtx->TrajUowner)];

   /* WLH 4 Nov 98 */
   if (trajuctx == NULL) return;

   /* vertices */
   bytes = 3 * t->length * sizeof(int_2);
   deallocate( trajuctx, t->verts, bytes );

   if (t->kind==1) {
      /* ribbon normals */
      bytes = 3 * t->length * sizeof(int_1);
      deallocate( trajuctx, t->norms, bytes );
   }

   if (t->colors) {
      /* colors */
      bytes = t->length * sizeof(uint_1);
      deallocate( trajuctx, t->colors, bytes );
   }

   bytes = trajuctx->NumTimes * sizeof(uint_2);
   deallocate( trajuctx, t->start, bytes );
   deallocate( trajuctx, t->len, bytes );
   deallocate( trajuctx, t, sizeof(struct traj) );
}



/*** del_last_traj ****************************************************
   Delete the most recent trajectory.
**********************************************************************/
void del_last_traj( Display_Context dtx )
{
   LOCK_ON( TrajLock );

   if (dtx->NumTraj) {
      free_traj( dtx, dtx->TrajTable[dtx->NumTraj-1] );
      dtx->TrajTable[dtx->NumTraj-1] = NULL;
      dtx->NumTraj--;
   }

   LOCK_OFF( TrajLock );
}




/*** del_traj_group ***************************************************
   Delete a group of trajectories.
   Input:  g - the trajectory group number in [0..VIS5D_TRAJ_SETS-1].
**********************************************************************/
void del_traj_group( Display_Context dtx, int g )
{
   struct traj **dest, **src;
   int i, j, num;

   LOCK_ON( TrajLock );

   i = 0;
   while (i<dtx->NumTraj) {
      if (dtx->TrajTable[i]->group==g) {
         if (i<MAXTRAJ-1) {
            free_traj( dtx, dtx->TrajTable[i] );
            /* delete this one by shifting remaining trajectories backward */
            for (j=i+1;j<dtx->NumTraj;j++) {
               dtx->TrajTable[j-1] = dtx->TrajTable[j];
            }
         }
         dtx->NumTraj--;
      }
      else {
         i++;
      }
   }

   LOCK_OFF( TrajLock );
}




/*
 * Deallocate an isosurface's memory.
 */
int free_isosurface( Context ctx, int time, int var )
{
   Display_Context dtx;
   int ctime, ftime, t, total;
   
   dtx = ctx->dpy_ctx;
   total = 0;
   if (!ctx->SameIsoColorVarOwner[var]){
      ftime = dtx->TimeStep[time].ownerstimestep[return_ctx_index_pos(dtx,
                                  ctx->context_index)];
      for(t=0; t < dtx->NumTimes; t++){
         ctime = dtx->TimeStep[t].ownerstimestep[return_ctx_index_pos(dtx,
                                                 ctx->context_index)];
         if ( ctime==ftime){
            if (ctx->SurfTable[var][time].valid) {
               int b1, b2, b3, b4;
               /* vertices */
               b1 = ctx->SurfTable[var][time].numverts * sizeof(int_2) * 3;
               if (b1) {
                  deallocate( ctx, ctx->SurfTable[var][time].verts, b1 );
               }
               /* normals */
               b2 = ctx->SurfTable[var][time].numverts * sizeof(int_1) * 3;
               if (b2) {
                  deallocate( ctx, ctx->SurfTable[var][time].norms, b2 );
               }
               /* indices */
#ifdef          BIG_GFX
               b3 = ctx->SurfTable[var][time].numindex * sizeof(uint_4);
#else
               b3 = ctx->SurfTable[var][time].numindex * sizeof(uint_2);
#endif
               if (b3) {
                  deallocate( ctx, ctx->SurfTable[var][time].index, b3 );
               }
               /* colors */
               if (ctx->SurfTable[var][time].colors) {
                  b4 = ctx->SurfTable[var][time].numverts * sizeof(uint_1);
                  deallocate( ctx, ctx->SurfTable[var][time].colors, b4 );
                  ctx->SurfTable[var][time].colors = NULL;
               }
               else {
                  b4 = 0;
               }
               ctx->SurfTable[var][time].valid = 0;
               total += b1 + b2 + b3 + b4;
            }
         }
      }
      return total;
   }
   
   else{
      if (ctx->SurfTable[var][time].valid) {
         int b1, b2, b3, b4;
         /* vertices */
         b1 = ctx->SurfTable[var][time].numverts * sizeof(int_2) * 3;
         if (b1) {
            deallocate( ctx, ctx->SurfTable[var][time].verts, b1 );
         }
         /* normals */
         b2 = ctx->SurfTable[var][time].numverts * sizeof(int_1) * 3;
         if (b2) {
            deallocate( ctx, ctx->SurfTable[var][time].norms, b2 );
         }
         /* indices */
#ifdef    BIG_GFX
         b3 = ctx->SurfTable[var][time].numindex * sizeof(uint_4);
#else
         b3 = ctx->SurfTable[var][time].numindex * sizeof(uint_2);
#endif
         if (b3) {
            deallocate( ctx, ctx->SurfTable[var][time].index, b3 );
         }
         /* colors */
         if (ctx->SurfTable[var][time].colors) {
            b4 = ctx->SurfTable[var][time].numverts * sizeof(uint_1);
            deallocate( ctx, ctx->SurfTable[var][time].colors, b4 );
            ctx->SurfTable[var][time].colors = NULL;
         }
         else {
            b4 = 0;
         }
         ctx->SurfTable[var][time].valid = 0;
         return b1 + b2 + b3 + b4;
      }
      else {
         return 0;
      }
   }
}



int free_textplot( Irregular_Context itx, int time)
{
   if (itx->TextPlotTable[time].valid){
      int b1, b2;
      
      b1 = itx->TextPlotTable[time].numverts * sizeof(int_2) * 3;
      if (b1){
         i_deallocate( itx, itx->TextPlotTable[time].verts, b1);
      }
      if (itx->TextPlotTable[time].colors){
         b2 = itx->TextPlotTable[time].numverts/2 * sizeof(uint_1);
         if (b2){
            i_deallocate( itx, itx->TextPlotTable[time].colors, b2);
         }
      }
      itx->TextPlotTable[time].valid = 0;
      return b1 + b2;
   }
   else{
      return 0;
   }
}
 

int free_hslice( Context ctx, int time, int var )
{
   if (ctx->HSliceTable[var][time].valid) {
      int b1, b2, b3, b4;

      b1 = ctx->HSliceTable[var][time].num1 * sizeof(int_2) * 3;
      if (b1) {
         deallocate( ctx, ctx->HSliceTable[var][time].verts1, b1 );
      }
      b2 = ctx->HSliceTable[var][time].num2 * sizeof(int_2) * 3;
      if (b2) {
         deallocate( ctx, ctx->HSliceTable[var][time].verts2, b2 );
      }
      b3 = ctx->HSliceTable[var][time].num3 * sizeof(int_2) * 3;
      if (b3) {
         deallocate( ctx, ctx->HSliceTable[var][time].verts3, b3 );
      }
      b4 = ctx->HSliceTable[var][time].numboxverts * 3 * sizeof(float);
      if (b4) {
         deallocate( ctx, ctx->HSliceTable[var][time].boxverts, b4 );
      }
      ctx->HSliceTable[var][time].valid = 0;
      return b1 + b2 + b3 + b4;
   }
   else {
      return 0;
   }
}




int free_vslice( Context ctx, int time, int var )
{
   if (ctx->VSliceTable[var][time].valid) {
      int b1, b2, b3, b4;

      if (ctx->VSliceTable[var][time].valid
          && ctx->VSliceTable[var][time].num1) {
         b1 = ctx->VSliceTable[var][time].num1 * sizeof(int_2) * 3;
         deallocate( ctx, ctx->VSliceTable[var][time].verts1, b1 );
      }
      if (ctx->VSliceTable[var][time].valid
          && ctx->VSliceTable[var][time].num2) {
         b2 = ctx->VSliceTable[var][time].num2 * sizeof(int_2) * 3;
         deallocate( ctx, ctx->VSliceTable[var][time].verts2, b2 );
      }
      if (ctx->VSliceTable[var][time].valid
          && ctx->VSliceTable[var][time].num3) {
         b3 = ctx->VSliceTable[var][time].num3 * sizeof(int_2) * 3;
         deallocate( ctx, ctx->VSliceTable[var][time].verts3, b3 );
      }
      b4 = ctx->VSliceTable[var][time].numboxverts * 3 * sizeof(float);
      if (b4) {
         deallocate( ctx, ctx->VSliceTable[var][time].boxverts, b4 );
      }
      ctx->VSliceTable[var][time].valid = 0;
      return b1 + b2 + b3 + b4;
   }
   else {
      return 0;
   }
}



int free_chslice( Context ctx, int time, int var )
{
   if (ctx->CHSliceTable[var][time].valid) {
      int nrnc, b1, b2;
      nrnc = ctx->CHSliceTable[var][time].rows
           * ctx->CHSliceTable[var][time].columns;
      b1 = nrnc * sizeof(uint_1);
      deallocate( ctx, ctx->CHSliceTable[var][time].color_indexes, b1 );
      b2 = 3 * nrnc * sizeof(int_2);
      deallocate( ctx, ctx->CHSliceTable[var][time].verts, b2 );
      ctx->CHSliceTable[var][time].valid = 0;
      return b1 + b2;
   }
   else {
      return 0;
   }
}




int free_cvslice( Context ctx, int time, int var )
{

   if (ctx->CVSliceTable[var][time].valid) {
      int nrnc, b1, b2;
      nrnc = ctx->CVSliceTable[var][time].rows
           * ctx->CVSliceTable[var][time].columns;
      b1 = nrnc * sizeof(uint_1);
      deallocate( ctx, ctx->CVSliceTable[var][time].color_indexes, b1 );
      b2 = 3 * nrnc * sizeof(int_2);
      deallocate( ctx, ctx->CVSliceTable[var][time].verts, b2 );
      ctx->CVSliceTable[var][time].valid = 0;
      return b1 + b2;
   }
   else {
      return 0;
   }
}




/*
 * Free all the graphics of a specified physical variable.
 */
int free_param_graphics( Context ctx, int var )
{
   int time;

   for (time=0;time<ctx->NumTimes;time++) {
      LOCK_ON( GfxLock );
      /* isosurfaces */
      free_isosurface( ctx, time, var );

      /* horizontal contour slices */
      free_hslice( ctx, time, var );

      /* vertical contour slices */
      free_vslice( ctx, time, var );

      /* horizontal colored slices */
      free_chslice( ctx, time, var );

      /* vertical colored slices */
      free_cvslice( ctx, time, var );

      LOCK_OFF( GfxLock );
   }
   return 0;
}





int free_hwind( Display_Context dtx, int time, int ws )
{
   Context uctx;

   uctx = dtx->ctxpointerarray[return_ctx_index_pos(dtx, dtx->Uvarowner[ws])];
   if (dtx->HWindTable[ws][time].valid) {
      int b1, b2;
/* MJK 10.14.98 
      dtx->DisplayHWind[ws] = 0;
*/
      b1 = dtx->HWindTable[ws][time].nvectors * sizeof(int_2) * 3;
/* WLH 4 Nov 98
      if (b1) {
*/
      /* WLH 4 Nov 98 */
      if (b1 && uctx) {
         deallocate( uctx, dtx->HWindTable[ws][time].verts, b1 );
      }
      b2 = dtx->HWindTable[ws][time].numboxverts * 3 * sizeof(float);
/* WLH 4 Nov 98
      if (b2) {
*/
      /* WLH 4 Nov 98 */
      if (b2 && uctx) {
         deallocate( uctx, dtx->HWindTable[ws][time].boxverts, b2 );
      }
      dtx->HWindTable[ws][time].valid = 0;
      return b1 + b2;
   }
   else {
      return 0;
   }
}



int free_vwind( Display_Context dtx, int time, int ws )
{
   Context uctx;

   uctx = dtx->ctxpointerarray[return_ctx_index_pos(dtx, dtx->Uvarowner[ws])];
   if (dtx->VWindTable[ws][time].valid) {
      int b1, b2;
/* MJK 10.14.98       
      dtx->DisplayVWind[ws] = 0;
*/
      b1 = dtx->VWindTable[ws][time].nvectors * sizeof(int_2) * 3;
/* WLH 4 Nov 98
      if (b1) {
*/
      /* WLH 4 Nov 98 */
      if (b1 && uctx) {
         deallocate( uctx, dtx->VWindTable[ws][time].verts, b1 );
      }
      b2 = dtx->VWindTable[ws][time].numboxverts * 3 * sizeof(float);
/* WLH 4 Nov 98
      if (b2) {
*/
      /* WLH 4 Nov 98 */
      if (b2 && uctx) {
         deallocate( uctx, dtx->VWindTable[ws][time].boxverts, b2 );
      }
      dtx->VWindTable[ws][time].valid = 0;
      return b1 + b2;
   }
   else {
      return 0;
   }
}



int free_hstream( Display_Context dtx, int time, int ws )
{
   Context uctx;
   
   uctx = dtx->ctxpointerarray[return_ctx_index_pos(dtx, dtx->Uvarowner[ws])];
   if (dtx->HStreamTable[ws][time].valid) {
      int b1, b2;
/* MJK 10.14.98       
      dtx->DisplayHStream[ws] = 0;
*/
      b1 = dtx->HStreamTable[ws][time].nlines * sizeof(int_2) * 3;
/* WLH 4 Nov 98
      if (b1) {
*/
      /* WLH 4 Nov 98 */
      if (b1 && uctx) {
         deallocate( uctx, dtx->HStreamTable[ws][time].verts, b1 );
      }
      b2 = dtx->HStreamTable[ws][time].numboxverts * 3 * sizeof(float);
/* WLH 4 Nov 98
      if (b2) {
*/
      /* WLH 4 Nov 98 */
      if (b2 && uctx) {
         deallocate( uctx, dtx->HStreamTable[ws][time].boxverts, b2 );
      }
      dtx->HStreamTable[ws][time].valid = 0;
      return b1 + b2;
   }
   else {
      return 0;
   }
}



int free_vstream( Display_Context dtx, int time, int ws )
{

   Context uctx;

   uctx = dtx->ctxpointerarray[return_ctx_index_pos(dtx, dtx->Uvarowner[ws])];
   if (dtx->VStreamTable[ws][time].valid) {
      int b1, b2;
/* MJK 10.14.98       
      dtx->DisplayVStream[ws] = 0;
*/
      b1 = dtx->VStreamTable[ws][time].nlines * sizeof(int_2) * 3;
/* WLH 4 Nov 98
      if (b1) {
*/
      /* WLH 4 Nov 98 */
      if (b1 && uctx) {
         deallocate( uctx, dtx->VStreamTable[ws][time].verts, b1 );
      }
      b2 = dtx->VStreamTable[ws][time].numboxverts * 3 * sizeof(float);
/* WLH 4 Nov 98
      if (b2) {
*/
      /* WLH 4 Nov 98 */
      if (b2 && uctx) {
         deallocate( uctx, dtx->VStreamTable[ws][time].boxverts, b2 );
      }
      dtx->VStreamTable[ws][time].valid = 0;
      return b1 + b2;
   }
   else {
      return 0;
   }
}



void free_all_graphics( Context ctx )
{
   int set, var, time, ws;

   for (var=0;var<ctx->NumVars;var++) {
      free_param_graphics( ctx, var );
   }
   for (time=0;time<ctx->dpy_ctx->NumTimes;time++) {
      /* BUG FIX MJK 8.10.98 */
      /* set ws<VIS5D_WIND_SLICES instead of ws<=VIS5D_WIND_SLICES */
      for (ws=0;ws<VIS5D_WIND_SLICES;ws++) {
         if (ctx->dpy_ctx->Uvarowner[ws] == ctx->context_index){
            free_hwind( ctx->dpy_ctx, time, ws );
            free_vwind( ctx->dpy_ctx, time, ws );
            free_hstream( ctx->dpy_ctx, time, ws );
            free_vstream( ctx->dpy_ctx, time, ws );
         }
      }
   }
   if (ctx->context_index == ctx->dpy_ctx->TrajUowner){
      for (set = 0; set < VIS5D_TRAJ_SETS; set++){
         ctx->dpy_ctx->DisplayTraj[set] = 0;
         vis5d_delete_traj_set(ctx->dpy_ctx->dpy_context_index, set);
      }
   }
}

void turn_off_and_free_var_graphics( Context ctx, int var)
{
   int set, i, time;
   free_param_graphics( ctx, var );

   for (i=0;i<VIS5D_WIND_SLICES;i++) {
      if ((ctx->context_index == ctx->dpy_ctx->Uvarowner[i]) &&
          (var == ctx->dpy_ctx->Uvar[i] ||
           var == ctx->dpy_ctx->Vvar[i] ||
           var == ctx->dpy_ctx->Wvar[i])){
         for (time = 0; time < ctx->dpy_ctx->NumTimes; time++){
            free_hwind( ctx->dpy_ctx, time, i);
            free_vwind( ctx->dpy_ctx, time, i);
            free_hstream( ctx->dpy_ctx, time, i);
            free_vstream( ctx->dpy_ctx, time, i);
         }
      }
   }
   if ((ctx->context_index == ctx->dpy_ctx->TrajUowner) &&
          (var == ctx->dpy_ctx->TrajU ||
           var == ctx->dpy_ctx->TrajV ||
           var == ctx->dpy_ctx->TrajW) ){
      for (set = 0; set < VIS5D_TRAJ_SETS; set++){
         vis5d_delete_traj_set(ctx->dpy_ctx->dpy_context_index, set);
      }
   }
}



/* BUG FIX MJK 8.10.98 */
/* Added this function so when ever dtx map and vert parameters */
/* are changed, graphics will but turned off and deallocated */
void turn_off_and_free_all_display_graphics( Display_Context dtx )
{
   int yo, var, time, set, ws;
   Context ctx;
   
   /* param graphics */
   for (yo = 0; yo < dtx->numofctxs; yo++){
      ctx = dtx->ctxpointerarray[yo];
      for (var=0;var<ctx->NumVars;var++) {
         free_param_graphics( ctx, var );
         vis5d_enable_graphics(ctx->context_index, VIS5D_ISOSURF, var, VIS5D_OFF);
         vis5d_enable_graphics(ctx->context_index, VIS5D_HSLICE, var, VIS5D_OFF);
         vis5d_enable_graphics(ctx->context_index, VIS5D_VSLICE, var, VIS5D_OFF);
         vis5d_enable_graphics(ctx->context_index, VIS5D_CHSLICE, var, VIS5D_OFF);
         vis5d_enable_graphics(ctx->context_index, VIS5D_CVSLICE, var, VIS5D_OFF);
         vis5d_enable_graphics(ctx->context_index, VIS5D_VOLUME, var, VIS5D_OFF);
      }
   }

   /* traj sets */
   for (set = 0; set < VIS5D_TRAJ_SETS; set++){
      vis5d_delete_traj_set(dtx->dpy_context_index, set);
      dtx->DisplayTraj[set] = 0; 
   }
  
   /* U,V,W graphics */
   for (ws=0;ws<VIS5D_WIND_SLICES;ws++) {
      for (time=0;time<dtx->NumTimes;time++) {
         free_hwind( dtx, time, ws );
         free_vwind( dtx, time, ws );
         free_hstream( dtx, time, ws );
         free_vstream( dtx, time, ws );
      }
      dtx->DisplayHWind[ws] = 0;
      dtx->DisplayVWind[ws] = 0;
      dtx->DisplayHStream[ws] = 0;
      dtx->DisplayVStream[ws] = 0;
   }
}
         

static int AccessTime = 1;   /* access time pseudo-clock */



/*** recent ***********************************************************
   Inform the memory manager of the most recently used graphic.
   Input:  ig - the graphic type: ISOSURF, HSLICE, VWIND, TRAJ, etc.
           ip - which parameter, wind slice, traj group, etc.
**********************************************************************/
void recent( Context ctx, int ig, int ip )
{
   switch (ig) {
      case ISOSURF:
         ctx->RecentIsosurf[ip] = AccessTime;
         break;
      case HSLICE:
         ctx->RecentHSlice[ip] = AccessTime;
         break;
      case VSLICE:
         ctx->RecentVSlice[ip] = AccessTime;
         break;
      case CHSLICE:
         ctx->RecentCHSlice[ip] = AccessTime;
         break;
      case CVSLICE:
         ctx->RecentCVSlice[ip] = AccessTime;
         break;
      case HWIND:
         ctx->dpy_ctx->RecentHWind[ip] = AccessTime;
         break;
      case VWIND:
         ctx->dpy_ctx->RecentVWind[ip] = AccessTime;
         break;
      case HSTREAM:
         ctx->dpy_ctx->RecentHStream[ip] = AccessTime;
         break;
      case VSTREAM:
         ctx->dpy_ctx->RecentVStream[ip] = AccessTime;
         break;
      case TRAJ:
         ctx->dpy_ctx->RecentTraj[ip] = AccessTime;
         break;
      default:
         printf("error in recent( %d, %d )\n", ig, ip );
   }
   AccessTime++;
}




/* because (val % limit) doesn't work for negatives */
static int adjust( int val, int limit )
{
  while (val < 0) val += limit;
  while (val >= limit) val -= limit;
  return val;
}




/*
 * Free all graphics for a particular time step.
 * Return:  number of bytes freed or 0 if nothing found to deallocate.
 *
 * NOTE: time is in ctx time
 */
static int free_time( Context ctx, int time )
{

   int var, ws, bytes;
   int yo;
   int ctxindex;
   int spandex;
   int ctime, t, vtime;
   Display_Context dtx;

   dtx = ctx->dpy_ctx;
   bytes = 0;
   LOCK_ON( GfxLock );
  
   for (ws=0;ws<VIS5D_WIND_SLICES;ws++) {
      if (dtx->Uvarowner[ws] == ctx->context_index){
         for(t=0; t < dtx->NumTimes; t++){
            ctime = dtx->TimeStep[t].ownerstimestep[return_ctx_index_pos(dtx,
                                                 ctx->context_index)];
            if (ctime==time){
               bytes += free_hwind( dtx, t, ws);
               bytes += free_vwind( dtx, t, ws);
               bytes += free_hstream( dtx, t, ws);
               bytes += free_vstream( dtx, t, ws);
            }
         }
      }
   }

   vtime = time; /* WLH 26 Jan 2000 */

   for (var=0;var<ctx->NumVars;var++) {
      /* isosurfaces */
      bytes += free_isosurface( ctx, vtime, var );
 
      /* horizontal contour slices */
      bytes += free_hslice( ctx, vtime, var );
 
      /* vertical contour slices */
      bytes += free_vslice( ctx, vtime, var );

      /* horizontal colored slices */
      bytes += free_chslice( ctx, vtime, var );
  
      /* vertical colored slices */
      bytes += free_cvslice( ctx, vtime, var );
   }
   LOCK_OFF( GfxLock );
   return bytes;
}


int i_deallocate_lru( Irregular_Context itx )
{
   /* this has not been implemented yet, since so far there is */
   /* only one graphic displayable for an irregular data context */
   return 0;
}






   
/*** deallocate_lru ***************************************************
   Find and deallocate the least-recently-used graphic.
   Return:  number of bytes freed or 0 if nothing found to deallocate.
**********************************************************************/
int deallocate_lru( Context ctx )
{
   int var, time, t, oldtime, oldig, oldvar, bytes;
   static int first_time=1;
   Display_Context dtx;

   dtx = ctx->dpy_ctx;

   if (first_time) {
      printf("WARNING:  Memory is full, graphics may be discarded\n");
      first_time = 0;
   }

   oldtime = AccessTime;

  if (ctx->VeryLarge) {
    int tp, tm;

    tm = adjust(ctx->CurTime + ctx->NumTimes / 2, ctx->NumTimes);
    tp = adjust(tm + 1, ctx->NumTimes);
    bytes = 0;
    while ((tp != ctx->CurTime || tm != ctx->CurTime) && bytes == 0) {
      if (tp != ctx->CurTime) {
        bytes += free_time( ctx, tp);
        tp = adjust(tp + 1, ctx->NumTimes);
      }
      if (tm != ctx->CurTime) {
        bytes += free_time( ctx,  tm);
        tm = adjust(tm - 1, ctx->NumTimes);
      }
    }
    if (bytes == 0) {
      /*printf("free_lru() failed\n");*/
    }
    return bytes;
  }
  else {
     /* find oldest isosurface */
     for (var=0;var<ctx->NumVars;var++) {
        t = ctx->RecentIsosurf[var];
        if (t>0 && t<oldtime) {
           oldtime = t;
           oldig = ISOSURF;
           oldvar = var;
        }
     }

     /* find oldest horizontal contour slice */
     for (var=0;var<ctx->NumVars;var++) {
        t = ctx->RecentHSlice[var];
        if (t>0 && t<oldtime) {
           oldtime = t;
           oldig = HSLICE;
           oldvar = var;
        }
     }

     /* find oldest vertical contour slice */
     for (var=0;var<ctx->NumVars;var++) {
        t = ctx->RecentVSlice[var];
        if (t>0 && t<oldtime) {
           oldtime = t;
           oldig = VSLICE;
           oldvar = var;
        }
     }

     /* find oldest horizontal colored slice */
     for (var=0;var<ctx->NumVars;var++) {
        t = ctx->RecentCHSlice[var];
        if (t>0 && t<oldtime) {
           oldtime = t;
           oldig = CHSLICE;
           oldvar = var;
        }
     }

     /* find oldest vertical colored slice */
     for (var=0;var<ctx->NumVars;var++) {
        t = ctx->RecentCVSlice[var];
        if (t>0 && t<oldtime) {
           oldtime = t;
           oldig = CVSLICE;
           oldvar = var;
        }
     }

     /* find oldest horizontal wind slice */
     for (var=0;var<VIS5D_WIND_SLICES;var++) {
        if (dtx->Uvarowner[var] == ctx->context_index){
           t = dtx->RecentHWind[var];
           if (t>0 && t<oldtime) {
              oldtime = t;
              oldig = HWIND;
              oldvar = var;
           }
        }
     }

     /* find oldest vertical wind slice */
     for (var=0;var<VIS5D_WIND_SLICES;var++) {
        if (dtx->Uvarowner[var] == ctx->context_index){
           t = dtx->RecentVWind[var];
           if (t>0 && t<oldtime) {
              oldtime = t;
              oldig = VWIND;
              oldvar = var;
           }
        }
     }
     /* find oldest horizontal stream slice */
     for (var=0;var<VIS5D_WIND_SLICES;var++) {
        if (dtx->Uvarowner[var] == ctx->context_index){
           t = dtx->RecentHStream[var];
           if (t>0 && t<oldtime) {
              oldtime = t;
              oldig = HSTREAM;
              oldvar = var;
           }
        }
     }

     /* find oldest vertical stream slice */
     for (var=0;var<VIS5D_WIND_SLICES;var++) {
        if (dtx->Uvarowner[var] == ctx->context_index){
           t = dtx->RecentVStream[var];
           if (t>0 && t<oldtime) {
              oldtime = t;
              oldig = VSTREAM;
              oldvar = var;
           }
        }
     }

     /* find oldest trajectory group */
     for (var=0;var<VIS5D_TRAJ_SETS;var++) {
        if (dtx->TrajUowner == ctx->context_index){
           t = dtx->RecentTraj[var];
           if (t>0 && t<oldtime) {
              oldtime = t;
              oldig = TRAJ;
              oldvar = var;
           }
        }
     }


     if (oldtime<AccessTime) {
        /* found something to deallocate */
        bytes = 0;

        if (oldig==ISOSURF) {
           /* deallocate all 'oldvar' isosurfaces */
           for (time=0;time<ctx->NumTimes;time++) {
              bytes += free_isosurface( ctx, time, oldvar );
              /*printf("[reclaimed %d] isosurf %d %d\n", bytes,oldvar,time);*/
           }
           ctx->RecentIsosurf[oldvar] = 0;
        }
        else if (oldig==HSLICE) {
           /* dealloc all 'oldvar' horizontal contour slices */
           for (time=0;time<ctx->NumTimes;time++) {
              bytes += free_hslice( ctx, time, oldvar );
              /*printf("[reclaimed %d] hslice %d %d\n", bytes,oldvar,time);*/
           }
           ctx->RecentHSlice[oldvar] = 0;
        }
        else if (oldig==VSLICE) {
           /* dealloc all 'oldvar' horizontal contour slices */
           for (time=0;time<ctx->NumTimes;time++) {
              bytes += free_vslice( ctx, time, oldvar );
              /*printf("[reclaimed %d] vslice %d %d\n", bytes,oldvar,time);*/
           }
           ctx->RecentVSlice[oldvar] = 0;
        }
        else if (oldig==CHSLICE) {
           for (time=0;time<ctx->NumTimes;time++) {
              bytes += free_chslice( ctx, time, oldvar );
              /*printf("[reclaimed %d] chslice %d %d\n", bytes,oldvar,time);*/
           }
           ctx->RecentCHSlice[oldvar] = 0;
        }
        else if (oldig==CVSLICE) {
           for (time=0;time<ctx->NumTimes;time++) {
              bytes += free_cvslice( ctx, time, oldvar );
              /*printf("[reclaimed %d] cvslice %d %d\n", bytes,oldvar,time);*/
           }
           ctx->RecentCVSlice[oldvar] = 0;
        }
        else if (oldig==HWIND) {
           for (time=0;time<dtx->NumTimes;time++) {
              bytes += free_hwind( dtx, time, oldvar );
              /*printf("[reclaimed %d] hwind %d\n", bytes*3,time);*/
           }
           dtx->RecentHWind[oldvar] = 0;
        }
        else if (oldig==VWIND) {
           for (time=0;time<dtx->NumTimes;time++) {
              bytes += free_vwind( dtx, time, oldvar );
              /*printf("[reclaimed %d] vwind %d\n", bytes*3,time );*/
           }
           dtx->RecentVWind[oldvar] = 0;
        }
        else if (oldig==HSTREAM) {
           for (time=0;time<dtx->NumTimes;time++) {
              bytes += free_hstream( dtx, time, oldvar );
              /*printf("[reclaimed %d] hstream %d\n", bytes*3,time);*/
           }
           dtx->RecentHStream[oldvar] = 0;
        }
        else if (oldig==VSTREAM) {
           for (time=0;time<dtx->NumTimes;time++) {
              bytes += free_vstream( dtx, time, oldvar );
              /*printf("[reclaimed %d] vstream %d\n", bytes*3,time);*/
           }
           dtx->RecentVStream[oldvar] = 0;
        }
        else if (oldig==TRAJ) {
           del_traj_group( dtx, oldvar );
           bytes = 1;
           dtx->RecentTraj[oldvar] = 0;
        }
        else {
           printf("oldig=%d\n", oldig );
           die("error in deallocate_lru");
        }

        if (bytes==0)
           bytes = 1;
        return bytes;
     }
     else {
        /*printf("free_lru() failed\n");*/
        return 0;
     }
  }
}


