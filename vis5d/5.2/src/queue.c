/* queue.c */

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
#include <unistd.h>
#include "analysis.h"
#include "globals.h"
#include "misc.h"
#include "queue.h"
#include "sync.h"
#include "work.h"


#define QSIZE 3000


struct entry {
   Context ctx;                   /* the vis5d context */
   Irregular_Context itx;         
   int type;                      /* the type of entry */
   int i1, i2, i3;                /* integer arguments */
   float f1, f2, f3, f4, f5;      /* float arguments */
};



static struct entry queue[QSIZE];
static int qsize;
static int qhead, qtail;      /* remove from head, add to tail */
static int qwaiters;
static LOCK qlock;
static SEMAPHORE qnotempty;




void init_queue( void )
{
   ALLOC_LOCK( qlock );
   ALLOC_SEM( qnotempty, 0 );
   qsize = 0;
   qhead = qtail = 0;
   qwaiters = 0;
}



void terminate_queue( void )
{
   FREE_LOCK( qlock );
   FREE_SEM( qnotempty );
}



/*
 * Return number of entries still in the queue and the number of threads
 * waiting for work to do.
 * If size==0 and waiters==NumWorkThreads then we're idle.
 */
void get_queue_info( int *size, int *waiters )
{
   LOCK_ON( qlock );
   *size = qsize;
   *waiters = qwaiters;
   LOCK_OFF( qlock );
}




static void add_qentry( Context ctx, Irregular_Context itx, 
                        int urgent, int type,
                        int i1, int i2, int i3,
                        float f1, float f2, float f3, float f4, float f5 )
{
   int pos, i, found=0;

   LOCK_ON( qlock );
   while (qsize==QSIZE-2) {
      if (Debug)
         printf("QUEUE FULL!!!\n");
      LOCK_OFF( qlock );
/* WLH 6 Nov 98
      sleep(1);
*/
      /* WLH 6 Nov 98 */
      if (NumThreads==1) {
        do_one_task( 0 );
      }
      else {
        sleep(1);  /* wait a second for the queue to empty a bit */
      }

      LOCK_ON( qlock );
   }

   /* check if already in the queue */
   pos = qhead;
   found = 0;

   for (i=0;i<qsize;i++) {
      /* check if request is already in the queue */
      if (ctx && queue[pos].ctx==ctx &&
          type!=TASK_TRAJ &&
          queue[pos].type==type &&     /* only need to test */
          queue[pos].i1==i1 &&         /* the first four fields */
          queue[pos].i2==i2) {
         /* already in queue, cancel it if urgent */
         found = 1;
         if (urgent)
           queue[pos].type = TASK_NULL;
         break;
      }
      else if (itx && queue[pos].ctx==ctx &&
          queue[pos].type==type &&     /* only need to test */
          queue[pos].i1==i1 &&         /* the first four fields */
          queue[pos].i2==i2) {
         /* already in queue, cancel it if urgent */
         found = 1;
         if (urgent)
           queue[pos].type = TASK_NULL;
         break;
      }
      else {
         pos++;
         if (pos==QSIZE)
           pos = 0;
      }
   }

   if (urgent) {
      /* insert at head */
      if (qhead==0)
         qhead = QSIZE-1;
      else
         qhead--;
      pos = qhead;
      qsize++;
      SIGNAL_SEM( qnotempty );
   }
   else if (!found) {
      /* insert at tail */
      pos = qtail;
      qtail++;
      if (qtail==QSIZE)
        qtail = 0;
      qsize++;
      SIGNAL_SEM( qnotempty );
   }

   queue[pos].ctx = ctx;
   queue[pos].itx = itx;
   queue[pos].type = type;
   queue[pos].i1 = i1;
   queue[pos].i2 = i2;
   queue[pos].i3 = i3;
   queue[pos].f1 = f1;
   queue[pos].f2 = f2;
   queue[pos].f3 = f3;
   queue[pos].f4 = f4;
   queue[pos].f5 = f5;

   if (Debug) { 
      if (urgent)
        printf("**URGENT** **URGENT** **URGENT** **URGENT** ");
      printf("ADDED AT POS=%d\n", pos );
   } 

   LOCK_OFF( qlock );
}



/*** get_qentry *******************************************************
   Return an entry from the work queue.  If the queue is empty, this
   fuction blocks.
**********************************************************************/
void get_qentry( Context *ctx, Irregular_Context *itx,
                 int *type,
                 int *i1, int *i2, int *i3,
                 float *f1, float *f2, float *f3, float *f4, float *f5 )
{
   if (Debug) printf("get_qentry\n");

   LOCK_ON( qlock );

   while (qsize==0) {
      qwaiters++;
      LOCK_OFF( qlock );
      WAIT_SEM( qnotempty );
      LOCK_ON( qlock );
      qwaiters--;
   }


   if (qsize>0) {
      /* remove from head */
      *ctx = queue[qhead].ctx;
      *itx = queue[qhead].itx;
      *type = queue[qhead].type;
      *i1 = queue[qhead].i1;
      *i2 = queue[qhead].i2;
      *i3 = queue[qhead].i3;
      *f1 = queue[qhead].f1;
      *f2 = queue[qhead].f2;
      *f3 = queue[qhead].f3;
      *f4 = queue[qhead].f4;
      *f5 = queue[qhead].f5;

      if (Debug) printf("REMOVED FROM POS=%d\n", qhead );

      if (*type!=TASK_QUIT) {
         qhead++;
         if (qhead==QSIZE)
           qhead = 0;

         qsize--;
      }

   }
   else {
      *type = TASK_NULL;
   }
   LOCK_OFF( qlock );
   if (Debug) printf("return\n");
}



/*** request_quit *****************************************************
   This is called when we want to exit the program.  It puts a message
   on the queue that all 'work' threads are to exit.
**********************************************************************/
void request_quit( Context ctx )
{
   add_qentry( ctx, NULL, 1, TASK_QUIT, 0,0,0, 0.0,0.0,0.0,0.0,0.0 );
}



/*
 * Request that an isosurface is to be computed.
 * Input:  ctx - the context
 *         time - the timestep number
 *         var - which variable
 *         urgent - 1 = put request at front of queue, 0 = put at back of queue
 */
void request_isosurface( Context ctx, int time, int var, int urgent )
{
   if (ctx->SurfTable[var][time].valid==0 ||
       ctx->SurfTable[var][time].isolevel!=ctx->IsoLevel[var] ||
       ctx->SurfTable[var][time].colorvar!=ctx->IsoColorVar[var]) {
      add_qentry( ctx, NULL, urgent, TASK_ISOSURFACE, time, var, 0,
                  0.0, 0.0, 0.0, 0.0, 0.0 );
   }
}


void request_hclip( Context ctx, int num)
{
   add_qentry( ctx, NULL, 1, TASK_HCLIP, num, 0, 0, 
                  0.0, 0.0, 0.0, 0.0, 0.0 );
}

void request_vclip( Context ctx, int num)
{
   add_qentry( ctx, NULL, 1, TASK_VCLIP, num, 0, 0,
                  0.0, 0.0, 0.0, 0.0, 0.0 );
}


void request_text_plot( Irregular_Context itx, int time, int var, int urgent)
{
   if (itx->TextPlotTable[time].valid == 0 ||
       itx->TextPlotTable[time].spacing != itx->TextPlotSpacing ||
       itx->TextPlotTable[time].fontx != itx->TextPlotFontX ||
       itx->TextPlotTable[time].fonty != itx->TextPlotFontY ||
       itx->TextPlotTable[time].fontspace != itx->TextPlotFontSpace ){
      add_qentry( NULL, itx, urgent, TASK_TEXT_PLOT, time, var, 
      0, 0.0, 0.0, 0.0, 0.0, 0.0 );
   }
}




/*
 * Request that a horizontal contour line slice is to be computed.
 * Input:  ctx - the context
 *         time - the timestep number
 *         var - which variable
 *         urgent - 1 = put request at front of queue, 0 = put at back of queue
 */
void request_hslice( Context ctx, int time, int var, int urgent )
{
   if (ctx->HSliceTable[var][time].valid==0 ||
       ctx->HSliceTable[var][time].level!=ctx->HSliceLevel[var] ||
       ctx->HSliceTable[var][time].interval!=ctx->HSliceInterval[var] ||
       ctx->HSliceTable[var][time].lowlimit!=ctx->HSliceLowLimit[var] ||
       ctx->HSliceTable[var][time].highlimit!=ctx->HSliceHighLimit[var] ) {
      add_qentry( ctx, NULL, urgent, TASK_HSLICE, time, var, 0,
                 0.0, 0.0, 0.0, 0.0, 0.0 );
   }
}



/*
 * Request that a vertical contour line slice is to be computed.
 * Input:  ctx - the context
 *         time - the timestep number
 *         var - which variable
 *         urgent - 1 = put request at front of queue, 0 = put at back of queue
 */
void request_vslice( Context ctx, int time, int var, int urgent )
{
   if (ctx->VSliceTable[var][time].valid==0 ||
       ctx->VSliceTable[var][time].r1!=ctx->VSliceR1[var] ||
       ctx->VSliceTable[var][time].c1!=ctx->VSliceC1[var] ||
       ctx->VSliceTable[var][time].r2!=ctx->VSliceR2[var] ||
       ctx->VSliceTable[var][time].c2!=ctx->VSliceC2[var] ||
       ctx->VSliceTable[var][time].interval!=ctx->VSliceInterval[var] ||
       ctx->VSliceTable[var][time].lowlimit!=ctx->VSliceLowLimit[var] ||
       ctx->VSliceTable[var][time].highlimit!=ctx->VSliceHighLimit[var] ) {
      add_qentry( ctx, NULL, urgent, TASK_VSLICE, time, var, 0,
                 0.0, 0.0, 0.0, 0.0, 0.0 );
   }
}



/*
 * Request that a horizontal color slice is to be computed.
 * Input:  ctx - the context
 *         time - the timestep number
 *         var - which variable
 *         urgent - 1 = put request at front of queue, 0 = put at back of queue
 */
void request_chslice( Context ctx, int time, int var, int urgent )
{
   if (ctx->CHSliceTable[var][time].valid==0 ||
       ctx->CHSliceTable[var][time].level!=ctx->CHSliceLevel[var] ) {
      add_qentry( ctx, NULL, urgent, TASK_CHSLICE, time, var, 0,
                 0.0, 0.0, 0.0, 0.0, 0.0 );
   }
}



/*
 * Request that a vertical colored slice is to be computed.
 * Input:  ctx - the context
 *         time - the timestep number
 *         var - which variable
 *         urgent - 1 = put request at front of queue, 0 = put at back of queue
 */
void request_cvslice( Context ctx, int time, int var, int urgent )
{
   if (ctx->CVSliceTable[var][time].valid==0 ||
       ctx->CVSliceTable[var][time].r1!=ctx->CVSliceR1[var] ||
       ctx->CVSliceTable[var][time].c1!=ctx->CVSliceC1[var] ||
       ctx->CVSliceTable[var][time].r2!=ctx->CVSliceR2[var] ||
       ctx->CVSliceTable[var][time].c2!=ctx->CVSliceC2[var] ) {
      add_qentry( ctx, NULL, urgent, TASK_CVSLICE, time, var, 0,
                 0.0, 0.0, 0.0, 0.0, 0.0 );
   }
}



/*
 * Request that a horizontal wind vector slice is to be computed.
 * Input:  dtx - the display context
 *         time - the display timestep number
 *         ws - which wind slice
 *         urgent - 1 = put request at front of queue, 0 = put at back of queue
 */
void request_hwindslice( Display_Context dtx, int time, int ws, int urgent )
{
   Context ctx;

   ctx = dtx->ctxpointerarray[return_ctx_index_pos(dtx, dtx->Uvarowner[ws])];
   if (dtx->HWindTable[ws][time].valid==0 ||
       dtx->HWindTable[ws][time].uvar!=dtx->Uvar[ws] ||
       dtx->HWindTable[ws][time].vvar!=dtx->Vvar[ws] ||
       dtx->HWindTable[ws][time].wvar!=dtx->Wvar[ws] ||
       dtx->HWindTable[ws][time].uvarowner!=dtx->Uvarowner[ws] ||
       dtx->HWindTable[ws][time].vvarowner!=dtx->Vvarowner[ws] ||
       dtx->HWindTable[ws][time].wvarowner!=dtx->Wvarowner[ws] ||
       dtx->HWindTable[ws][time].level!=dtx->HWindLevel[ws] ||
       dtx->HWindTable[ws][time].scale!=dtx->HWindScale[ws] ||
       dtx->HWindTable[ws][time].density!=dtx->HWindDensity[ws] ||
       dtx->HWindTable[ws][time].barbs!=dtx->WindBarbs ) {

       /* use the first ctx in the dtx to pass to add_qentry */ 
       add_qentry( ctx, NULL, urgent,
                   TASK_HWIND, time, ws,0 ,
                   0.0, 0.0, 0.0, 0.0, 0.0 );
   }
}



/*
 * Request that a vertical wind vector slice is to be computed.
 * Input:  ctx - the display context
 *         time - the timestep number
 *         ws - which wind slice
 *         urgent - 1 = put request at front of queue, 0 = put at back of queue
 */
void request_vwindslice(Display_Context dtx, int time, int ws, int urgent )
{
   Context ctx;

   ctx = dtx->ctxpointerarray[return_ctx_index_pos(dtx, dtx->Uvarowner[ws])];
   if (dtx->VWindTable[ws][time].valid==0 ||
       dtx->VWindTable[ws][time].uvar!=dtx->Uvar[ws] ||
       dtx->VWindTable[ws][time].vvar!=dtx->Vvar[ws] ||
       dtx->VWindTable[ws][time].wvar!=dtx->Wvar[ws] ||
       dtx->VWindTable[ws][time].uvarowner!=dtx->Uvarowner[ws] ||
       dtx->VWindTable[ws][time].vvarowner!=dtx->Vvarowner[ws] ||
       dtx->VWindTable[ws][time].wvarowner!=dtx->Wvarowner[ws] ||
       dtx->VWindTable[ws][time].r1!=dtx->VWindR1[ws] ||
       dtx->VWindTable[ws][time].c1!=dtx->VWindC1[ws] ||
       dtx->VWindTable[ws][time].r2!=dtx->VWindR2[ws] ||
       dtx->VWindTable[ws][time].c2!=dtx->VWindC2[ws] ||
       dtx->VWindTable[ws][time].scale!=dtx->VWindScale[ws] ||
       dtx->VWindTable[ws][time].density!=dtx->VWindDensity[ws] ||
       dtx->VWindTable[ws][time].barbs!=dtx->WindBarbs ) {
      add_qentry( ctx, NULL,
                  urgent, TASK_VWIND, time, ws, 0,
                 0.0, 0.0, 0.0, 0.0, 0.0 );
   }
}



/*
 * Request that a horizontal streamline slice is to be computed.
 * Input:  ctx - the display context
 *         time - the timestep number
 *         ws - which slice
 *         urgent - 1 = put request at front of queue, 0 = put at back of queue
 */
void request_hstreamslice(Display_Context dtx, int time, int ws, int urgent )
{
   Context ctx;

   ctx = dtx->ctxpointerarray[return_ctx_index_pos(dtx, dtx->Uvarowner[ws])];
   if (dtx->HStreamTable[ws][time].valid==0 ||
       dtx->HStreamTable[ws][time].uvar!=dtx->Uvar[ws] ||
       dtx->HStreamTable[ws][time].vvar!=dtx->Vvar[ws] ||
       dtx->HStreamTable[ws][time].uvarowner!=dtx->Uvarowner[ws] ||
       dtx->HStreamTable[ws][time].vvarowner!=dtx->Vvarowner[ws] ||
       dtx->HStreamTable[ws][time].level!=dtx->HStreamLevel[ws] ||
       dtx->HStreamTable[ws][time].density!=dtx->HStreamDensity[ws] ) {
      add_qentry( ctx, NULL,
                  urgent, TASK_HSTREAM, time, ws, 0,
                  0.0, 0.0, 0.0, 0.0, 0.0 );
   }
}



/*
 * Request that a vertical streamline slice is to be computed.
 * Input:  ctx - the display context
 *         time - the timestep number
 *         ws - which slice
 *         urgent - 1 = put request at front of queue, 0 = put at back of queue
 */
void request_vstreamslice(Display_Context dtx, int time, int ws, int urgent )
{
   Context ctx;

   ctx = dtx->ctxpointerarray[return_ctx_index_pos(dtx, dtx->Uvarowner[ws])];
   if (dtx->VStreamTable[ws][time].valid==0 ||
       dtx->VStreamTable[ws][time].uvar!=dtx->Uvar[ws] ||
       dtx->VStreamTable[ws][time].vvar!=dtx->Vvar[ws] ||
       dtx->VStreamTable[ws][time].wvar!=dtx->Wvar[ws] ||
       dtx->VStreamTable[ws][time].uvarowner!=dtx->Uvarowner[ws] ||
       dtx->VStreamTable[ws][time].vvarowner!=dtx->Vvarowner[ws] ||
       dtx->VStreamTable[ws][time].wvarowner!=dtx->Wvarowner[ws] ||
       dtx->VStreamTable[ws][time].r1!=dtx->VStreamR1[ws] ||
       dtx->VStreamTable[ws][time].c1!=dtx->VStreamC1[ws] ||
       dtx->VStreamTable[ws][time].r2!=dtx->VStreamR2[ws] ||
       dtx->VStreamTable[ws][time].c2!=dtx->VStreamC2[ws] ||
       dtx->VStreamTable[ws][time].density!=dtx->VStreamDensity[ws] ) {
      add_qentry( ctx, NULL,
                  urgent, TASK_VSTREAM, time, ws, 0,
                  0.0, 0.0, 0.0, 0.0, 0.0 );
   }
}




/*** request_traj *****************************************************
   This is called to compute a new trajectory.
   Input:  row, col, lev - start pos in [0..Nr-1],[0..Nc-1],[0..Nl-1].
           time - start time in [0..NumTimes-1].
           group - which traj group in [0..TRAJGROUPS-1].
           rib - non-zero means make ribbon instead of line traj.
           step - integration step multiplier.
           len - trajectory length multiplier.
**********************************************************************/
void request_traj( Display_Context dtx, float row, float col, float lev,
                   int time, int group, int rib,
                   float step, float len )
{
   add_qentry( dtx->ctxpointerarray[0], NULL, 1, TASK_TRAJ,
               time, group, rib, row, col, lev, step, len );
}



/*
 * Request that all trajectories in a group be recolored according to
 * the current TrajColorVar variable.
 */
void request_traj_recoloring( Context ctx, int traj_set )
{
   add_qentry( ctx, NULL, 1, TASK_TRAJ_RECOLOR,
               traj_set, 0, 0, 0.0, 0.0, 0.0, 0.0, 0.0 );
}




/*
 * Put a job into the queue for computing an external function
 */
void request_ext_func( Context ctx, int time, int var )
{
#ifdef SINGLE_TASK
   calc_ext_func( ctx, time, var, 1 );
#else
   add_qentry( ctx, NULL, 0, TASK_EXT_FUNC, time, var, 0,
               0.0, 0.0, 0.0, 0.0, 0.0 );
#endif
}



/*
 * Request that the topography be recolored according to a 2-D variable
 * or according to height.
 */
void request_topo_recoloring( Context ctx )
{
   int time, urgent;

   for (time=0;time<ctx->dpy_ctx->NumTimes;time++) {
      if (time==ctx->dpy_ctx->CurTime) {
         urgent = 1;
      }
      else {
         urgent = 0;
      }
      add_qentry( ctx, NULL, urgent, TASK_TOPO_RECOLOR,
                  time, 0, 0, 0.0, 0.0, 0.0, 0.0, 0.0 );
   }
}


