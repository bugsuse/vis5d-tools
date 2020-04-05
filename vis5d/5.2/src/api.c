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
#include <sys/stat.h>
#include <signal.h>
#include <unistd.h>
#include <X11/Xlib.h>
#include <X11/cursorfont.h>
#ifdef SGI_GL
#  include "gl/gl.h"
#endif
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
#ifdef LTHREADS 
#  include <pthread.h>
#endif


#include "analysis.h"
#include "anim.h"
#include "api.h"
#include "box.h"
#include "chrono.h"
#include "compute.h"
#include "globals.h"
#include "gl_to_ppm.h"
#include "graphics.h"
#include "groupchrono.h"
#include "grid.h"
#include "image.h"
#include "memory.h"
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

/* New 5.2 */
#include "imemory.h"
#include "record.h"



#define MEGA 1024*1024
#define MAX(A,B)  ( (A) > (B) ? (A) : (B) )

#define VERY_LARGE_RATIO 0.1


#define CLAMP( X, MIN, MAX )   ( (X)<(MIN) ? (MIN) : ((X>MAX) ? (MAX) : (X)) )



/*
 * Parallelism stuff:
 */
#define MAX_WORKERS 8
#ifdef sgi
  static int WorkerPID[MAX_WORKERS];
#endif
#ifdef sunos5
  static thread_t WorkerPID[MAX_WORKERS];
#endif
#ifdef LTHREADS 
  static pthread_t WorkerPID[MAX_WORKERS];
#endif


/*
 * CAVE stuff:
 */
#ifdef CAVE
#  include <malloc.h>
#  ifdef OPENGL
#    include "cave_ogl.h"
#  else
#    include "cave.h"
#  endif
#  include "cave5d.h"
   static void *cave_shmem = NULL;  /* Pool shared by all contexts */
#endif

/* proj stuff */
#ifndef M_PI
#  define M_PI 3.14159265
#endif

#ifndef DEG2RAD
#  define DEG2RAD    (M_PI/180.0)
#endif

#ifndef RAD2DEG
#  define RAD2DEG    (180.0/M_PI)
#endif

#ifndef RADIUS
#  define RADIUS     6371.23
#endif


/* initial xformation matrix */
static MATRIX init_ctm = { 1.0, 0.0, 0.0, 0.0,
                           0.0, 1.0, 0.0, 0.0, 
                           0.0, 0.0, 1.0, 0.0,
                           0.0, 0.0, 0.0, 1.0 };

/* WLH 6 Oct 98 */
int noexit = 0; /* if true, don't exit on bad vis5d_open_gridfile */

/*  | MJK 11.19.98 |  */

int off_screen_rendering = 0;

int REVERSE_POLES = 1.0;

int in_the_init_stage = 0;

/* MJK 4.27.99 */
char Vis5dDataPath[1000] = "";
char Vis5dFuncPath[1000] = "";

struct varlink var_link[MAXVARS * MAXTYPES * VIS5D_MAX_CONTEXTS];
struct varlink group_var_link[MAXVARS * MAXTYPES * VIS5D_MAX_CONTEXTS];


/**********************************************************************/
/*****                   CONTEXT HANDLING                         *****/
/**********************************************************************/


static Context *ctx_table = NULL;
static Display_Context *dtx_table = NULL;
static Display_Group *grp_table = NULL;

static int init_display_context( Display_Context dtx);
static void init_display_group (Display_Group grp );
static void initialize_stuff( Context ctx );
int init_var_clrtable( int dindex, int vindex, int var );

/**************************************************/
/*                  New 5.2 stuff                 */
/*  |       |       |          |       |     |    */
/* \|/     \|/     \|/        \|/     \|/   \|/   */
/**************************************************/
static Irregular_Context *itx_table = NULL;
static void init_irregular_context( Irregular_Context itx);
Irregular_Context vis5d_get_itx( int index );
static void init_irregular_context( Irregular_Context itx );
static Irregular_Context new_irregular_context (void );
static void destroy_irregular_context( Irregular_Context itx );
static int add_itx_index_to_dtx( int index, int index_of_itx);
static int remove_itx_index_from_dtx( int index, int index_of_itx);
static void initialize_irregular_stuff( Irregular_Context itx );
/**************************************************/
/* /|\     /|\     /|\        /|\     /|\   /|\   */
/*  |       |       |          |       |     |    */
/*                  New 5.2 stuff                 */
/**************************************************/

void debugstuff(void)
{
   printf("hello, is anybody outthere?\n"); 
}
/*
 * This macro used at the beginning of each function which takes a
 * context index.  It translates the context index to a context pointer
 * and prints an error message if the index is bad.
 */
#define CONTEXT( msg ) \
  Context ctx; \
  if (index<0 || index>=VIS5D_MAX_CONTEXTS || (ctx = ctx_table[index])==NULL) { \
    debugstuff(); \
    printf("bad context in %s\n", msg); \
    return VIS5D_BAD_CONTEXT; \
  }

#define DPY_CONTEXT( msg ) \
  Display_Context dtx; \
  if (index<0 || index>=VIS5D_MAX_DPY_CONTEXTS || (dtx = dtx_table[index])==NULL) { \
    printf("bad display_context in %s\n", msg); \
    debugstuff(); \
    return VIS5D_BAD_CONTEXT; \
  }

#define IRG_CONTEXT( msg ) \
  Irregular_Context itx; \
  if (index<0 || index>=VIS5D_MAX_CONTEXTS || (itx = itx_table[index])==NULL) { \
    debugstuff(); \
    printf("bad irregular context in %s\n", msg); \
    return VIS5D_BAD_CONTEXT; \
  }

Irregular_Context vis5d_get_itx( int index )
{
      if (index<0 || index>=VIS5D_MAX_CONTEXTS) {
      debugstuff();
      printf("bad irregular context in vis5d_get_itx\n");
      return NULL;
   }
   else {
      return itx_table[index];
   }
}

static void init_irregular_context( Irregular_Context itx )
{
   int i;
   
   /* initialize everything to zero for starters */
   memset( itx, 0, sizeof(struct irregular_context) );
   itx->dpy_ctx = NULL;
   itx->TextPlotVar = -1;
   itx->TextPlotSpacing = 1.0;
   itx->TextPlotFontX = 10.0;
   itx->TextPlotFontY = 10.0;
   itx->TextPlotFontSpace = 1.0;
   for (i = 0; i < MAXVARS; i++){
      itx->TextPlotColorStatus[i] = VIS5D_OFF;
   }
}

static Irregular_Context new_irregular_context (void )
{
   Irregular_Context itx;
   itx = (Irregular_Context) calloc( 1, sizeof(struct irregular_context ) );
   if (itx){
      init_irregular_context( itx );
   }
   return itx;
}

static void destroy_irregular_context( Irregular_Context itx )
{
   if (itx->mempool){
      free( itx->mempool );
   }
   free( itx );
}

Context vis5d_get_ctx( int index )
{
   if (index<0 || index>=VIS5D_MAX_CONTEXTS) {
      printf("bad context in vis5d_get_ctx\n");
      return NULL;
   }
   else {
      return ctx_table[index];
   }
}

Display_Context vis5d_get_dtx( int index )
{
   if (index<0 || index>=VIS5D_MAX_DPY_CONTEXTS) {
      printf("bad context in vis5d_get_dtx\n");
      return NULL;
   }
   else {  
      return dtx_table[index];
   }
}   

Display_Group vis5d_get_grp( int index )
{
   if (index<0 || index>=VIS5D_MAX_DPY_CONTEXTS) {
      printf("bad context in vis5d_get_dtx\n");
      return NULL;
   }
   else {
      return grp_table[index];
   }
}

      
/*
 * Initialize the fields of the context to reasonable defaults.
 */
static void init_context( Context ctx )
{
   int i;

   /* initialize everything to zero for starters */
   memset( ctx, 0, sizeof(struct vis5d_context) );


   ctx->UserProjection = -1;
   ctx->UserVerticalSystem = -1;

   ctx->MegaBytes = MBS;
   ctx->Volume = NULL;
   for (i=0;i<MAXVARS;i++) {
      ctx->IsoColorVar[i] = -1;
      ctx->SameIsoColorVarOwner[i] = 1;
   }
   
   ALLOC_SEM( ctx->ExtFuncDoneSem, 0 );


   /* MJK 12.01.98 */
   for (i = 0; i < MAXVARS; i++)
   {
      ctx->HSliceLinkPrev[i][0] = ctx->HSliceLinkPrev[i][1] = -1;
      ctx->HSliceLinkNext[i][0] = ctx->HSliceLinkNext[i][1] = -1;
      ctx->VSliceLinkPrev[i][0] = ctx->VSliceLinkPrev[i][1] = -1;
      ctx->VSliceLinkNext[i][0] = ctx->VSliceLinkNext[i][1] = -1;
      ctx->CHSliceLinkPrev[i][0] = ctx->CHSliceLinkPrev[i][1] = -1;
      ctx->CHSliceLinkNext[i][0] = ctx->CHSliceLinkNext[i][1] = -1;
      ctx->CVSliceLinkPrev[i][0] = ctx->CVSliceLinkPrev[i][1] = -1;
      ctx->CVSliceLinkNext[i][0] = ctx->CVSliceLinkNext[i][1] = -1;
   }
   for (i = 0; i < VIS5D_WIND_SLICES; i++)
   {
      ctx->HWindLinkPrev[i][0] = ctx->HWindLinkPrev[i][1] = -1;
      ctx->HWindLinkNext[i][0] = ctx->HWindLinkNext[i][1] = -1;
      ctx->VWindLinkPrev[i][0] = ctx->VWindLinkPrev[i][1] = -1;
      ctx->VWindLinkNext[i][0] = ctx->VWindLinkNext[i][1] = -1;
      ctx->HStreamLinkPrev[i][0] = ctx->HStreamLinkPrev[i][1] = -1;
      ctx->HStreamLinkNext[i][0] = ctx->HStreamLinkNext[i][1] = -1;
      ctx->VStreamLinkPrev[i][0] = ctx->VStreamLinkPrev[i][1] = -1;
      ctx->VStreamLinkNext[i][0] = ctx->VStreamLinkNext[i][1] = -1;
   }

   /* MJK 12.02.98 */
   ctx->ProbeNumVars = -1;
   ctx->UserDataFlag = 0;
   
}



/* This is a helper function for the next function. */

static void adjust_wind_level_info( Display_Context dtx, int varowner, int var )
{
   Context ctx;

   if (varowner >=0){
      ctx = vis5d_get_ctx(varowner);
      if (var>=0) {
         if (ctx->LowLev[var] + ctx->Nl[var] < ctx->WindNl) {
            dtx->WindNl = ctx->LowLev[var] + ctx->Nl[var];
         }
         if (ctx->LowLev[var] > ctx->WindLow) {
            dtx->WindLow = ctx->LowLev[var];
         }
      }
   }
}


/* Update the WindNl and WindLow values */

static void compute_wind_levels( Display_Context dtx )
{
   int i;
   dtx->WindNl = dtx->MaxNl;
   dtx->WindLow = 0;
   for (i=0;i<VIS5D_WIND_SLICES;i++) {
      adjust_wind_level_info( dtx, dtx->Uvarowner[i], dtx->Uvar[i] );
      adjust_wind_level_info( dtx,  dtx->Uvarowner[i],dtx->Vvar[i] );
      adjust_wind_level_info( dtx, dtx->Uvarowner[i], dtx->Wvar[i] );
   }
   adjust_wind_level_info( dtx, dtx->TrajUowner, dtx->TrajU );
   adjust_wind_level_info( dtx, dtx->TrajUowner, dtx->TrajV );
   adjust_wind_level_info( dtx, dtx->TrajUowner, dtx->TrajW );
}




/*
 * Allocate a new vis5d context, initialize to default values.
 */
static Context new_context( void )
{
   Context ctx;

#ifdef CAVE
   if (cave_shmem) {
      ctx = amalloc( sizeof(struct vis5d_context), cave_shmem );
   }
   else {
      ctx = (Context) calloc( 1, sizeof(struct vis5d_context) );
   }
#else
   ctx = (Context) calloc( 1, sizeof(struct vis5d_context) );
#endif
   if (ctx) {
      init_context( ctx );
   }
   return ctx;
}


/*
 * Allocate a new display context, initialize to default values.
 */
static Display_Context new_display_context( void )
{
   Display_Context dtx;
   /* this calloc call eats up time! */
   dtx = (Display_Context) calloc( 1, sizeof(struct display_context) );
   if (dtx) {
      init_display_context( dtx);
   }
   return dtx;
}

/*
 * Allocate a new display group context, initialize to default values.
 */
static Display_Group new_display_group( void )
{
   int yo;
   Display_Group grp;

   grp = (Display_Group) calloc( 1, sizeof(struct display_group) );
   /*for (yo = 0; yo<VIS5D_MAX_DPY_CONTEXTS; yo++){
      grp->dpyarray[yo] = (Display_Context) calloc(1, sizeof(struct display_context));
   }*/
   if (grp){
      init_display_group(grp);
   }
   return grp;
}

/*
 * Initialize the fields of the context to zeros 
 */
static void init_display_group( Display_Group grp)
{
   int yo;
   memset( grp, 0, sizeof(struct display_group) );
}


/*
 * Initialize the fields of the context to reasonable defaults.
 */
static int init_display_context( Display_Context dtx )
{   
   Window win; 
   int yo;
   static unsigned int nice_color[] = {
      PACK_COLOR( 0xffU, 0xffU, 0x00U, 0xffU ),
      PACK_COLOR( 0xffU, 0xffU, 0xffU, 0xffU ),
      PACK_COLOR( 0xffU, 0x00U, 0xffU, 0xffU ),
      PACK_COLOR( 0x00U, 0xffU, 0xffU, 0xffU ),
      PACK_COLOR( 0xffU, 0x50U, 0x50U, 0xffU ),
      PACK_COLOR( 0x00U, 0xffU, 0x00U, 0xffU ),
      PACK_COLOR( 0x00U, 0x80U, 0xffU, 0xffU ),
      PACK_COLOR( 0x80U, 0x80U, 0xffU, 0xffU ),
      PACK_COLOR( 0x80U, 0xffU, 0x80U, 0xffU ),
      PACK_COLOR( 0xffU, 0x80U, 0x80U, 0xffU ),
      PACK_COLOR( 0xffU, 0x80U, 0x00U, 0xffU ),
      PACK_COLOR( 0xffU, 0x00U, 0x80U, 0xffU ),
      PACK_COLOR( 0x80U, 0xffU, 0x00U, 0xffU ),
      PACK_COLOR( 0x00U, 0xffU, 0x80U, 0xffU ),
      PACK_COLOR( 0x80U, 0x80U, 0xffU, 0xffU ),
      PACK_COLOR( 0x00U, 0x80U, 0xffU, 0xffU ),
      PACK_COLOR( 0xc0U, 0xc0U, 0x40U, 0xffU ),
      PACK_COLOR( 0xc0U, 0x40U, 0xc0U, 0xffU ),
      PACK_COLOR( 0x40U, 0xc0U, 0xc0U, 0xffU ),
      PACK_COLOR( 0x40U, 0x80U, 0xffU, 0xffU ),
      PACK_COLOR( 0x80U, 0x80U, 0xffU, 0xffU ),
      PACK_COLOR( 0xc0U, 0xffU, 0xc0U, 0xffU ),
      PACK_COLOR( 0x40U, 0xc0U, 0xffU, 0xffU ),
      PACK_COLOR( 0x04U, 0xffU, 0xc0U, 0xffU ),
      PACK_COLOR( 0xffU, 0x40U, 0xc0U, 0xffU )  };
   int var, time, i, j, k, w;

 
   /* initialize everything to zero for starters */
   if (!dtx->GfxWindow){
      memset( dtx, 0, sizeof(struct display_context) );
   }
    
   dtx->VolRender = 1; /* until proven otherwise */
   dtx->UserProjection = -1;
   dtx->UserVerticalSystem = -1;
   dtx->LogoSize = 1;
   for (yo=0; yo<12; yo++){
      dtx->tick_do[0] = 0;
   }
   make_3d_window( dtx, "Johan", 0, 0, 1, 1);
   set_current_window( dtx );

   dtx->TopoName[0] = 0;
   XSetWindowBorderWidth( GfxDpy, dtx->GfxWindow, 2);
/*
   XSetWindowBorder( GfxDpy, dtx->GfxWindow,
                     PACK_COLOR(255, 255, 255 ,255)); 
  */  
    
    
   for (i=0;i<VIS5D_TRAJ_SETS;i++) {
      dtx->TrajColorVar[i] = -1;
      dtx->TrajColorVarOwner[i] = -1;
   }
   dtx->DisplayBox = 1;
   dtx->DisplayClock = 1;
   dtx->DisplayCursor = 0;
   dtx->DisplayTraj[0] = 1;
   dtx->HWindColor[0] = PACK_COLOR( 0x00U, 0xffU, 0xffU, 0xffU );
   dtx->HWindColor[1] = PACK_COLOR( 0x00U, 0xffU, 0x00U, 0xffU );
   dtx->VWindColor[0] = PACK_COLOR( 0x00U, 0x88U, 0xffU, 0xffU );
   dtx->VWindColor[1] = PACK_COLOR( 0xffU, 0x00U, 0xffU, 0xffU );
   dtx->HStreamColor[0] = PACK_COLOR( 0xffU, 0x88U, 0x00U, 0xffU );
   dtx->HStreamColor[1] = PACK_COLOR( 0xffU, 0xffU, 0x00U, 0xffU );
   dtx->VStreamColor[0] = PACK_COLOR( 0xffU, 0xffU, 0x00U, 0xffU );
   dtx->VStreamColor[1] = PACK_COLOR( 0xffU, 0x88U, 0x00U, 0xffU );

   dtx->TrajColor[0] = PACK_COLOR( 0xffU, 0xffU, 0xffU, 0xffU );
   dtx->TrajColor[1] = PACK_COLOR( 0xffU, 0xffU, 0x00U, 0xffU );
   dtx->TrajColor[2] = PACK_COLOR( 0xffU, 0x00U, 0xffU, 0xffU );
   dtx->TrajColor[3] = PACK_COLOR( 0x00U, 0xffU, 0x00U, 0xffU );
   dtx->TrajColor[4] = PACK_COLOR( 0x00U, 0x88U, 0xffU, 0xffU );
   dtx->TrajColor[5] = PACK_COLOR( 0xffU, 0x00U, 0x88U, 0xffU );
   dtx->TrajColor[6] = PACK_COLOR( 0xffU, 0x88U, 0x00U, 0xffU );
   dtx->TrajColor[7] = PACK_COLOR( 0x00U, 0x88U, 0x88U, 0xffU );


   /* Button matrix colors */
   k = 0;
   for (i=0; i<MAXVARS*VIS5D_MAX_CONTEXTS; i++) {
      dtx->TextPlotColor[i] = nice_color[k];
      for (j=0; j<5; j++) {
         dtx->Color[i][j] = nice_color[k];
         k++;
         if (k>=sizeof(nice_color)/sizeof(unsigned int)) {
            k = 0;
         }
      }
      /* volume button is always white */
      dtx->Color[i][5] = PACK_COLOR( 0xffU, 0xffU, 0xffU, 0xffU);
   }
   
   dtx->ContnumFlag = 1;
   dtx->CursorColor = &dtx->TrajColor[0];
   dtx->BoxColor = PACK_COLOR(255,255,255,255);
   dtx->BgColor = PACK_COLOR(0,0,0,255);
   dtx->LabelColor = PACK_COLOR(255,255,255,255);
   dtx->LightMapColor = PACK_COLOR(255,255,255,255);
   dtx->DarkMapColor = PACK_COLOR(0,0,0,255);
   
   /* MJK 2.22.99 */
   dtx->ContFontFactorX = 0;
   dtx->ContFontFactorY = 0;

 
   dtx->CurrentVolume = -1; 
   dtx->CurrentVolumeOwner = -1;

   dtx->Ax = dtx->Ay = dtx->Az = 0.0;
   dtx->PointerX = dtx->PointerY = -1;
   dtx->FirstArea = -1;


   /* MJK 12.02.98 */
#  ifndef PEX
   dtx->FontHeight = 20;
#  endif


   dtx->FontName[0] = 0;
   strcpy(dtx->SoundFontName, DEFAULT_SOUNDFONTNAME);
   dtx->LineWidth = 1.0;
   dtx->DepthCue = 1;

   dtx->LegendPosition = VIS5D_BOTTOM;
   dtx->LegendSize = 128;
   dtx->LegendMarginX = 0;
   dtx->LegendMarginY = 0;
   dtx->Zoom = 1.0;

   memcpy( dtx->CTM, init_ctm, 16*sizeof(float) );

   dtx->init_cursor_flag = 1;

   /* MJK 12.02.98 begin */
   dtx->Sound.mainvarstep = 50;
   dtx->Sound.SndMinTemp = 228.0;
   dtx->Sound.SndMaxTemp = 323.0;
   dtx->Sound.tickstatus = 0;
   dtx->Sound.currentX = .69;
   dtx->Sound.currentY = .69;
   dtx->Sound.currentTime =1069;
   dtx->Sound.soundline = NULL;
   dtx->Sound.uwindline = NULL;
   dtx->Sound.vwindline = NULL;
   dtx->Sound.tgrid = NULL;
   dtx->Sound.dgrid = NULL;
   dtx->Sound.ugrid = NULL;
   dtx->Sound.var1grid = NULL;
   dtx->Sound.var2grid = NULL;
   dtx->Sound.var3grid = NULL;
   dtx->Sound.vertdata = NULL;

/* MJK 3.30.99 */
   if (dtx->numofctxs>0){   
      dtx->Sound.PreviousSoundTemp = vis5d_find_var(dtx->ctxarray[0],"T");
      dtx->Sound.PreviousSoundDewpt= vis5d_find_var(dtx->ctxarray[0],"TD");
      dtx->Sound.PreviousSoundUWind= vis5d_find_var(dtx->ctxarray[0],"U");
      dtx->Sound.PreviousSoundVWind= vis5d_find_var(dtx->ctxarray[0],"V");
   }
   else{
      dtx->Sound.PreviousSoundTemp =  -1;
      dtx->Sound.PreviousSoundDewpt=  -1;
      dtx->Sound.PreviousSoundUWind= -1;
      dtx->Sound.PreviousSoundVWind= -1;
   }

   dtx->Sound.PreviousSoundVar1 = -1;
   dtx->Sound.PreviousSoundVar2 = -1;
   dtx->Sound.PreviousSoundVar3 = -1;
   dtx->Sound.sndx = 15;
   dtx->Sound.sndy = 15;
   dtx->init_flag = 1;
   dtx->prev_time = -1;
   dtx->HClipTable[0].highlight = 1;
   dtx->NorthBound = 0.0;
   dtx->SouthBound = 0.0;
   dtx->WestBound  = 0.0;
   dtx->EastBound  = 0.0;
   dtx->RowInc     = 0.0;
   dtx->ColInc     = 0.0;
   {
      struct label *next;
      while (dtx->FirstLabel)
      {
          next = dtx->FirstLabel->next;
          free (dtx->FirstLabel);
          dtx->FirstLabel = next;
      }
   }
   /* MJK 12.02.98 end */


   /* MJK 12.02.98 */
   dtx->UserTopoFlag = dtx->UserMapsFlag = 0;

   /* MJK 12.02.98 */
   dtx->DisplayTopoBase = 0;
   dtx->TopoBaseLev     = 0.0;


   return 0;
}   



static void clear_irregular_context( Irregular_Context itx)
{
   int i;
   
   memset( itx->TextPlotTable, 0, sizeof(itx->TextPlotTable) );
}


/*
 * Deallocate all the resources attached to the context.  This is used
 * prior to loading a new dataset into a context.
 */
static void clear_context( Context ctx )
{
   int i;

   reinit_memory( ctx );
   FREE_LOCK( ctx->Mutex );
   /* clear graphics tables */
   memset( ctx->SurfTable, 0, sizeof(ctx->SurfTable) );
   memset( ctx->HSliceTable, 0, sizeof(ctx->HSliceTable) );
   memset( ctx->VSliceTable, 0, sizeof(ctx->VSliceTable) );
   memset( ctx->CHSliceTable, 0, sizeof(ctx->CHSliceTable) );
   memset( ctx->CVSliceTable, 0, sizeof(ctx->CVSliceTable) );
   memset( ctx->dpy_ctx->HWindTable, 0, sizeof(ctx->dpy_ctx->HWindTable) );
   memset( ctx->dpy_ctx->VWindTable, 0, sizeof(ctx->dpy_ctx->VWindTable) );
   memset( ctx->dpy_ctx->TrajTable, 0, sizeof(ctx->dpy_ctx->TrajTable) );
   ctx->dpy_ctx->NumTraj = 0;

   for (i=0;i<VIS5D_WIND_SLICES;i++) {
      ctx->dpy_ctx->Uvar[i] = ctx->dpy_ctx->Vvar[i] = ctx->dpy_ctx->Wvar[i] = -1;
   }
   ctx->dpy_ctx->TrajU = ctx->dpy_ctx->TrajV = ctx->dpy_ctx->TrajW = -1;

   memset( ctx->ExpressionList, 0, sizeof(ctx->ExpressionList) );

   ctx->dpy_ctx->Zoom = 1.0;
   memcpy( ctx->dpy_ctx->CTM, init_ctm, 16*sizeof(float) );
   ctx->dpy_ctx->CursorColor = &ctx->dpy_ctx->TrajColor[0];
}



/*
 * Deallocate a context and everything it points to.
 */
static void destroy_context( Context ctx )
{
   free_all_graphics( ctx );
   free_grid_cache( ctx );

#ifdef CAVE
   if (cave_shmem) {
      afree( ctx->mempool, cave_shmem );
      afree( ctx, cave_shmem);
   }
   else {
      if (ctx->mempool) {
         free( ctx->mempool );
      }
      free( ctx );
   }
#else
   if (ctx->mempool) {
      free( ctx->mempool );
   }
   free( ctx );
#endif
}

static void destroy_display_context( Display_Context dtx )
{
   free( dtx );
}



/**********************************************************************/
/*****                Context-independent functions               *****/
/**********************************************************************/


/*
 * Do one-time initializations which are indepenent of all contexts.
 */
int vis5d_initialize( int cave_mode )
{
   int i;

   init_sync();
   init_queue();
   init_work();

   ALLOC_LOCK( GfxLock );
   ALLOC_LOCK( TrajLock );

#if defined(sgi) || defined(sunos5)
   for (i=0;i<MAX_WORKERS;i++) {
      WorkerPID[i] = 0;
   }
#endif

#ifndef CAVE
   init_graphics();
#endif

   if (cave_mode) {
#ifdef CAVE
      int size = CAVE_MEMORY_SIZE * 1024 * 1024;
      cave_shmem = CAVEUserSharedMemory( size );
      if (!cave_shmem) {
         printf("Error: CAVEUserSharedMemory(%d) failed!\n", CAVE_MEMORY_SIZE);
         exit(1);
      }
      ctx_table = amalloc( sizeof(Context *) * VIS5D_MAX_CONTEXTS,
                               cave_shmem );
#else
      printf("Error: CAVE support not compiled in!\n");
      exit(1);
#endif
   }
   else {
      ctx_table = malloc( sizeof(Context *) * VIS5D_MAX_CONTEXTS );
      dtx_table = malloc( sizeof(Display_Context*) * VIS5D_MAX_DPY_CONTEXTS );
      grp_table = malloc( sizeof(Display_Group*) * VIS5D_MAX_DPY_CONTEXTS );
      /* New 5.2 */
      itx_table = malloc( sizeof(Irregular_Context *) * VIS5D_MAX_CONTEXTS );
}

   for (i=0;i<VIS5D_MAX_CONTEXTS;i++) {
      ctx_table[i] = NULL;
   }
   for (i=0;i<VIS5D_MAX_DPY_CONTEXTS;i++) {
      dtx_table[i] = NULL;
   }
   for (i=0;i<VIS5D_MAX_DPY_CONTEXTS;i++) {
      grp_table[i] = NULL;
   }
   /* New 5.2 */
   for (i=0;i<VIS5D_MAX_ITX_CONTEXTS;i++) {
      itx_table[i] = NULL;
   }
   DisplayRows = 1;
   DisplayCols = 1; 

   return 0;
}


/*
 * Call this clean up function before exiting.
 */
/* Johan TODO for 5.0 */
int vis5d_terminate( int close_windows )
{
   int i;

   if (close_windows) {
      for (i=0;i<VIS5D_MAX_DPY_CONTEXTS;i++) {
         if (dtx_table[i]) {
            free_graphics( dtx_table[i]);
         }
      }
   }

   FREE_LOCK( GfxLock );
   FREE_LOCK( TrajLock );

   terminate_work();
   terminate_queue();
   term_sync();
   terminate_graphics();

   /* Kill threads */
#ifdef sgi
   for (i=0;i<MAX_WORKERS;i++) {
      if (WorkerPID[i]) {
         kill( WorkerPID[i], SIGKILL );
         WorkerPID[i] = 0;
      }
   }
#endif
#ifdef sunos5
   for (i=0;i<MAX_WORKERS;i++) {
      if (WorkerPID[i]) {
         thr_kill( WorkerPID[i], SIGKILL );
         WorkerPID[i] = 0;
      }
   }
#endif

#ifdef LTHREADS 
   for (i=0;i<MAX_WORKERS;i++) {
      if (WorkerPID[i]) {
         pthread_kill( WorkerPID[i], SIGKILL );
         WorkerPID[i] = 0;
      }
   }
#endif

   return 0;
}


/*
 * Initialize 'nworkers' instances of the queue server tasks.
 */
int vis5d_workers( int nworkers )
{
   if (nworkers>=0 && nworkers<MAX_WORKERS) {
      NumThreads = nworkers + 1;
   }
   return 0;
}



/*
 * On single threaded systems, this function will pull one job off the
 * work queue and do it.  If multiple threads, this is a no-op.
 */
int vis5d_do_work( void )
{
   if (NumThreads==1) {
      int size, waiters;
      get_queue_info( &size, &waiters );
      if (size>0) {
         do_one_task( 0 );
      }
   }
   return 0;
}



/*
 * Check if there is work pending in the job queue.
 * Output:  pending_flag - 0 = queue is empty., 1 = queue is not empty
 */
int vis5d_check_work( int *pending_flag )
{
   if (any_work_pending()) {
      *pending_flag = 1;
   }
   else {
      *pending_flag = 0;
   }
   return 0;
}



/*
 * Block until the job queue is empty.
 */
int vis5d_finish_work( void )
{
   int size, waiters;
   if (NumThreads==1) {
      while (1) {
         get_queue_info( &size, &waiters );
         if (size==0) {
            break;
         }
         else {
            do_one_task( 0 );
         }
      }
   }
   else {
      while (1) {
         get_queue_info( &size, &waiters );
         if (size==0 && waiters==NumThreads-1) {
            break;
         }
      }
   }
   return 0;
}

/* WLH 6 Oct 98 */
/*
 * set noexit flag
 * if true don't exit on bad vis5d_open_gridfile
 */
int vis5d_noexit(int noex) {
  noexit = noex;

  return 0;
}

/****************************************************************/
/* this checks to see if a ctx belongs to a certain display_ctx */
/* Input :  index - display_ctx index                           */
/*          vindex- vis5d ctx index                             */
/* Output:  1 - ctx belongs to display_ctx                      */
/*          0 - ctx does NOT belong to display_ctx              */
/****************************************************************/

/*********************************************/
/******* index = Display_Context index *******/
/*********************************************/

int is_valid_dtx_ctx( int index, int vindex)
{
   int yo, spandex;
   DPY_CONTEXT("is_valid_dtx_ctx")

   for (yo=0; yo < dtx->numofctxs; yo++){
      spandex = dtx->ctxarray[yo];
      if (vindex == spandex){
         return 1;
      }
   }
   return 0;
}



/****************************************************************/
/* this resets the timer which keeps track of the time interval*/
/* between the animation of time sequences*/
/****************************************************************/

/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/

/* not a visible api function so far */

int vis5d_reset_display_timer( int index )
{
   struct timeval tp;
   DPY_CONTEXT("vis5d_reset_display_timer")

   /* Get the current time */
   gettimeofday(&tp, (struct timezone *) 0);
   dtx->lapsed_usec = 0;
   dtx->start_sec = tp.tv_sec;
   dtx->start_usec = tp.tv_usec;
   return 0;
}

void get_timer(int which)
{
   long lapsed_usec;
   static long utimer = 0;
   static long timer = 0;
   struct timeval tp;
   gettimeofday(&tp, (struct timezone *) 0);
   if(which == 1){
      lapsed_usec = ((tp.tv_sec - timer)* 1000000) +
                    tp.tv_usec - utimer;
      printf("elapsed time is %d\n",lapsed_usec);
   }
   else{
      utimer = tp.tv_usec;
      timer = tp.tv_sec;
   }
}


/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/

/* not a visible api function so far */

int vis5d_get_display_timer( int index, long *start_sec, long *start_usec)
{
   DPY_CONTEXT("vis5d_get_display_timer")
   *start_sec = dtx->start_sec;
   *start_usec= dtx->start_usec;
   return 0;
}





/****************************************/
/******** index => DATA CONTEXT *********/
/****************************************/

/* not a visible api function so far */

int vis5d_get_context_name( int index, char *name)
{
   CONTEXT("vis5d_get_context_name")
   strcpy( name, ctx->ContextName);
   return 0;
}

   



/* this makes sure all the same vars in a group have same max's and mins */
/* this is useful in having same range for color maps */
/****************************************/
/******** index => GROUP CONTEXT ********/
/****************************************/

int vis5d_set_grp_var_values( int index )
{
   Display_Group grp;
   Display_Context dtx, bigdtx;
   Context ctx, bigctx;
   char thename[30];
   char othername[30];
   int yo, tor, good;
   float minmin, maxmax, min, max;
   int bigloop, ctxloop, varloop; 
   grp = vis5d_get_grp( index );
   for (bigloop=0; bigloop<grp->numofdpys; bigloop++){
      bigdtx = grp->dpyarray[bigloop];
      for (ctxloop=0; ctxloop<bigdtx->numofctxs; ctxloop++){
         bigctx = vis5d_get_ctx(ctxloop);
         for (varloop = 0; varloop < bigctx->NumVars; varloop++){
            vis5d_get_ctx_var_name(ctxloop, varloop, thename);
            minmin = bigctx->MinVal[varloop];
            maxmax = bigctx->MaxVal[varloop]; 
            /* first get the min and the max of the same vars*/
            for (yo = 0; yo < grp->numofdpys; yo++){
               dtx = grp->dpyarray[yo];
               for (tor=0; tor < dtx->numofctxs; tor++){
                  good = vis5d_find_var(dtx->ctxarray[tor], thename);
                  if (good>-1){
                     ctx = vis5d_get_ctx(dtx->ctxarray[tor]);
                     min = ctx->MinVal[good];
                     max = ctx->MaxVal[good];
                     minmin = min < minmin ? min : minmin;
                     maxmax = max > maxmax ? max : maxmax;
                     tor = dtx->numofctxs+1;
                  }
               }
            }
            for (yo = 0; yo < grp->numofdpys; yo++){
               dtx = grp->dpyarray[yo];
               for (tor=0; tor < dtx->numofctxs; tor++){
                  good = vis5d_find_var(dtx->ctxarray[tor], thename);
                  if (good>-1){
                     vis5d_set_var_range( dtx->ctxarray[tor], good, minmin, maxmax);
                     tor = dtx->numofctxs+1;
                  }
               }
            }
         }
      }
   }

   return 0;
}


               
/* This will set the given display to belong to the group */
/* specified by 'index_of_grp'.  If the group context does */
/* not exist it will be created.  Note:   to make a display */
/* belong to NO group set 'index_of_grp' < 1.               */
/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/
         
int vis5d_set_display_group( int index, int index_of_grp )
{
   int yo, yo2;
   int ontinue = 0;
   Display_Group grp;
   Context ctx;
   Irregular_Context itx;
   DPY_CONTEXT("vis5d_set_display_group")
   
   
   /* remove from previous group if so */
   if (dtx->group_index > 0 && dtx->group_index < 10){
      grp = vis5d_get_grp( dtx->group_index );
      for (yo=0; yo<grp->numofdpys; yo++){
         if (grp->dpyarray[yo] == dtx){
            ontinue = 1;
            yo2 = yo;
         }
      }
      if (ontinue){
         for (yo = yo2; yo < grp->numofdpys-1; yo++){
            grp->dpyarray[yo] = grp->dpyarray[yo+1];
         }
         grp->numofdpys -= 1;
      }
      calculate_group_time_steps(grp);
      /* MJK 11.17.98 */
      vis5d_set_grp_var_values(grp->index);
      vis5d_create_group_links( grp->index );
   }
   if (index_of_grp < 1 || index_of_grp > 9){
      /* set the varmax's and varmin's back to old values */
      for (yo = 0; yo < dtx->numofctxs; yo++){
         int time, vars;
         ctx = vis5d_get_ctx(dtx->ctxarray[yo]);
         for(vars=0; vars < ctx->NumVars; vars++){
            ctx->MinVal[vars] = ctx->RealMinVal[vars];
            ctx->MaxVal[vars] = ctx->RealMaxVal[vars];
            for (time=0;time<MAXTIMES;time++) {
               ctx->SurfTable[vars][time].valid = 0;
               ctx->HSliceTable[vars][time].valid = 0;
               ctx->VSliceTable[vars][time].valid = 0;
               ctx->CHSliceTable[vars][time].valid = 0;
               ctx->CVSliceTable[vars][time].valid = 0;
               ctx->dpy_ctx->Redraw = 1;
            }
         }
      }
      for (yo = 0; yo < dtx->numofitxs; yo++){
         int time;
         itx = vis5d_get_itx(dtx->itxarray[yo]);
         for (time=0;time<MAXTIMES;time++) {
            itx->TextPlotTable[time].valid = 0;
         }
      }

      dtx->group_index = -1;
   }
   else{
      /* set to new group */
      dtx->group_index = index_of_grp;

      /*get/make the group context */
      if ( (grp=grp_table[index_of_grp])==NULL){
         grp = grp_table[index_of_grp] = new_display_group();
         /* MJK 11.17.98 */
         grp->index = index_of_grp;
      }
      else{
         grp = vis5d_get_grp( index_of_grp);
      }

      grp->numofdpys += 1;
      grp->dpyarray[grp->numofdpys-1] = dtx;
      calculate_group_time_steps(grp);
      /* MJK 11.17.98 */
      vis5d_set_grp_var_values(grp->index);
      vis5d_create_group_links( grp->index );
      vis5d_signal_redraw( index, 1);
   }
   return 0;
}

/* WLH 12 Nov 98 */
int vis5d_invalidate_isosurface(int index, int var, int time) {
  CONTEXT("vis5d_invalidate_isosurface");
  ctx->SurfTable[var][time].valid = 0;
  return 0;
}

/* WLH 12 Nov 98 */
int vis5d_invalidate_hslice(int index, int var, int time) {
  CONTEXT("vis5d_invalidate_isosurface");
  ctx->HSliceTable[var][time].valid = 0;
  return 0;
}

/* WLH 12 Nov 98 */
int vis5d_invalidate_vslice(int index, int var, int time) {
  CONTEXT("vis5d_invalidate_isosurface");
  ctx->VSliceTable[var][time].valid = 0;
  return 0;
}

/* WLH 12 Nov 98 */
int vis5d_invalidate_chslice(int index, int var, int time) {
  CONTEXT("vis5d_invalidate_isosurface");
  ctx->CHSliceTable[var][time].valid = 0;
  return 0;
}

/* WLH 12 Nov 98 */
int vis5d_invalidate_cvslice(int index, int var, int time) {
  CONTEXT("vis5d_invalidate_isosurface");
  ctx->CVSliceTable[var][time].valid = 0;
  return 0;
}

int vis5d_invalidate_hwind( int index, int var, int time)
{
  DPY_CONTEXT("vis5d_invalidate_hwind");
   dtx->HWindTable[var][time].valid = 0;
   return 0;
}

int vis5d_invalidate_vwind( int index, int var, int time)
{
  DPY_CONTEXT("vis5d_invalidate_vwind");
   dtx->VWindTable[var][time].valid = 0;
   return 0;
}

int vis5d_invalidate_hstream( int index, int var, int time)
{
  DPY_CONTEXT("vis5d_invalidate_hstream");
   dtx->HStreamTable[var][time].valid = 0;
   return 0;
}

int vis5d_invalidate_vstream( int index, int var, int time)
{
  DPY_CONTEXT("vis5d_invalidate_vstream");
   dtx->VStreamTable[var][time].valid = 0;
   return 0;
}


/* this is a helper function */            
static void ungroup_all_displays( void )
{
   int cnt;
   for (cnt = 0; cnt < VIS5D_MAX_DPY_CONTEXTS; cnt++){
      if (dtx_table[cnt] && dtx_table[cnt]->group_index > 0){
         vis5d_set_display_group(cnt, -1);
      }
   }
}



/* this funtion return the index of the group which the */
/* display belongs to.  It returns -1 if it does not belong */
/* to a group */
/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/

int vis5d_get_display_group( int index, int *grp_index )
{
   DPY_CONTEXT("vis5d_get_display_group")

   if ( dtx->group_index < 1 || dtx->group_index > 9){
      *grp_index = -1; 
   }
   else{
      *grp_index = dtx->group_index;
   }
   return 0;
}
          


            

/* this function is a helper funtion for below */
/* it adds the index of a vis5d data context to an array */
/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/

static int add_ctx_index_to_dtx( int index, int index_of_ctx)
{
   int yo;
   int ontinue;
   Context ctx;
   DPY_CONTEXT("add_ctx_index_to_dtx")

   ontinue = 1;
   for (yo=0; yo<dtx->numofctxs; yo++){
      if (dtx->ctxarray[yo] == index_of_ctx){
         ontinue = 0;
      }
   }
   if (ontinue){
      dtx->numofctxs += 1;
      dtx->ctxarray[dtx->numofctxs-1] = index_of_ctx;
      dtx->ctxpointerarray[dtx->numofctxs-1] = vis5d_get_ctx( index_of_ctx );
   }
   return 0;
}


/* this function is a helper funtion for below */
/* it removes the index of a vis5d data context from */
/* an array */
/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/

static int remove_ctx_index_from_dtx( int index, int index_of_ctx)
{
   int yo, yo2, ontinue;
   DPY_CONTEXT("remove_ctx_index_from_dtx")

   ontinue = 0;
   for (yo=0; yo<dtx->numofctxs; yo++){
      if (dtx->ctxarray[yo] == index_of_ctx){
         ontinue = 1;
         yo2 = yo;
      }
   }
   if (ontinue){
      for (yo = yo2; yo < dtx->numofctxs-1; yo++){
         dtx->ctxarray[yo] = dtx->ctxarray[yo+1];
         dtx->ctxpointerarray[yo] = dtx->ctxpointerarray[yo+1];
      }
      dtx->numofctxs -= 1;
      if (dtx->numofctxs>0){
         calculate_display_time_steps( dtx);
      }
   }
   return 0;
}




/* this will assign a vis5d data context to a display and */
/* set/reset various values */
/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/

int vis5d_assign_display_to_data( int index, int display_index)
{
   int var, yo, current;
   int vindex, proceed;
   Display_Context dtx;
   CONTEXT("vis5d_assign_display_to_data")

   /* always ungroup all displays when ever any */
   /* data context is moved or added, there some strange */
   /* bug that happen if this isn't done */
   ungroup_all_displays();
   dtx = vis5d_get_dtx(display_index);
   proceed = 1;
   if( ctx->dpy_ctx){
      if (ctx->dpy_ctx->CurrentVolumeOwner == ctx->context_index){
         ctx->dpy_ctx->CurrentVolumeOwner = -1;
         ctx->dpy_ctx->CurrentVolume = -1;
      }

      if (ctx->dpy_ctx->numofctxs > 1){
         if (ctx->dpy_ctx->ctxpointerarray[0] != ctx){
            vindex = ctx->dpy_ctx->ctxpointerarray[0]->context_index;
         }
         else{
            vindex = ctx->dpy_ctx->ctxpointerarray[1]->context_index;
         }
         vis5d_set_wind_vars( ctx->dpy_ctx->dpy_context_index,
                               vindex, vis5d_find_var(vindex,"U"),
                               vindex, vis5d_find_var(vindex,"V"),
                               vindex, vis5d_find_var(vindex,"W"),
                               vindex, vis5d_find_var(vindex,"U"),
                               vindex, vis5d_find_var(vindex,"V"),
                               vindex, vis5d_find_var(vindex,"W"),
                               vindex, vis5d_find_var(vindex,"U"),
                               vindex, vis5d_find_var(vindex,"V"),
                               vindex, vis5d_find_var(vindex,"W") );
         vis5d_set_sound_vars( ctx->dpy_ctx->dpy_context_index, 
                               vindex, vis5d_find_var(vindex,"T"),
                                vindex, vis5d_find_var(vindex,"TD"),
                                vindex, vis5d_find_var(vindex,"U"),
                                vindex, vis5d_find_var(vindex,"V"),
                                vindex, -1, vindex, -1, vindex, -1 );
      }
      else{
         vindex = -1;
         vis5d_set_wind_vars( ctx->dpy_ctx->dpy_context_index,
                               vindex, -1,
                               vindex, -1,
                               vindex, -1,
                               vindex, -1,
                               vindex, -1,
                               vindex, -1,
                               vindex, -1,
                               vindex, -1,
                               vindex,  -1); 
         vis5d_set_sound_vars( ctx->dpy_ctx->dpy_context_index, 
                               vindex, -1,
                                vindex, -1,
                                vindex, -1,
                                vindex, -1,
                                vindex, -1, vindex, -1, vindex, -1 );
      }
      remove_ctx_index_from_dtx(ctx->dpy_ctx->dpy_context_index, ctx->context_index);
   }
   ctx->dpy_ctx = dtx;
   add_ctx_index_to_dtx( display_index, index);
   calculate_display_time_steps( dtx);
   if (dtx->numofctxs >1){
      memset( ctx->SurfTable, 0, sizeof(ctx->SurfTable) );
      memset( ctx->HSliceTable, 0, sizeof(ctx->HSliceTable) );
      memset( ctx->VSliceTable, 0, sizeof(ctx->VSliceTable) );
      memset( ctx->CHSliceTable, 0, sizeof(ctx->CHSliceTable) );
      memset( ctx->CVSliceTable, 0, sizeof(ctx->CVSliceTable) );
      memset(  ctx->DisplaySurf, 0, sizeof(ctx->DisplaySurf) );
      memset(  ctx->DisplayHSlice, 0, sizeof(ctx->DisplayHSlice) );
      memset(  ctx->DisplayVSlice, 0, sizeof(ctx->DisplayVSlice) );
      memset(  ctx->DisplayCHSlice, 0, sizeof(ctx->DisplayCHSlice) );
      memset(  ctx->DisplayCVSlice, 0, sizeof(ctx->DisplayCVSlice) );
      ctx->CurTime = 0;
      dtx->CurTime = 0;
      vis5d_signal_redraw( display_index, 1 );
   }
   {
      int uvar1, vvar1, wvar1, uvar2, vvar2, wvar2, tuvar, tvvar, twvar, tvar, tdvar;
      int uowner1, vowner1, wowner1, towner, tdowner;
      int uowner2, vowner2, wowner2, tuowner, tvowner, twowner;
      int vindex;
      vindex = index;

      vis5d_get_wind_vars( dtx->dpy_context_index, &uowner1, &uvar1,
                           &vowner1, &vvar1, &wowner1, &wvar1, 
                           &uowner2, &uvar2, &vowner2, &vvar2,
                           &wowner2, &wvar2, &tuowner, &tuvar,
                           &tvowner, &tvvar, &twowner, &twvar);
      if (uvar1 < 0 && vvar1 < 0 && wvar1 < 0 && uvar2 < 0 &&
      vvar2 < 0 && wvar2 < 0 && tuvar < 0 && tvvar < 0 && twvar < 0){
         vis5d_set_wind_vars( dtx->dpy_context_index, vindex, vis5d_find_var(vindex,"U"),
                                     vindex, vis5d_find_var(vindex,"V"),
                                     vindex, vis5d_find_var(vindex,"W"),
                                     vindex, vis5d_find_var(vindex,"U"),
                                     vindex, vis5d_find_var(vindex,"V"),
                                     vindex, vis5d_find_var(vindex,"W"),
                                     vindex, vis5d_find_var(vindex,"U"),
                                     vindex, vis5d_find_var(vindex,"V"),
                                     vindex, vis5d_find_var(vindex,"W") );
      }
      vis5d_get_sound_vars( dtx->dpy_context_index, &towner, &tvar,
                             &tdowner, &tdvar, &uowner1, &uvar1, &vowner1,
                             &vvar1, &wowner1, &wvar1, &uowner2, &vowner2,
                             &wowner2, &uvar2);
      if (tvar < 0 && tdvar < 0 && uvar1 < 0 && vvar1 < 0){
         vis5d_set_sound_vars( dtx->dpy_context_index, vindex, vis5d_find_var(vindex,"T"),
                                      vindex, vis5d_find_var(vindex,"TD"),
                                      vindex, vis5d_find_var(vindex,"U"),
                                      vindex, vis5d_find_var(vindex,"V"),
                                      vindex, -1, vindex, -1, vindex, -1 );
      }
   }

   for (var=0;var<ctx->NumVars;var++) {
      init_graphics_pos( ctx, var );
   }

  /* this var is important! */
   ctx->GridSameAsGridPRIME = vis5d_check_dtx_same_as_ctx(dtx->dpy_context_index,
                                                          ctx->context_index); 

   /* This if (ctx->meminited){ statment is added becuase 
      vis5d_assign_display_to_data maybe called before the memory
      is init'd and screw things up, such is the case when the func
      vis5d_load_v5dfile is run from the interp. */
   if (ctx->meminited){
      if (!dtx->CurvedBox) {
         if (ctx->Volume){
            free_volume( ctx );
         }
         ctx->Volume = alloc_volume( ctx, dtx->Nr, dtx->Nc, dtx->MaxNl);
      }
      else{
         if (ctx->Volume){
            free_volume( ctx );
         }
         ctx->Volume = NULL;
      }
   }
   
   dtx->do_not_recalc_probe_text_width = 0;      

   return 1;
}

int vis5d_check_dtx_same_as_ctx( int dindex, int vindex )
{
   Display_Context dtx;
   Context ctx;
   int yo;

   dtx = vis5d_get_dtx( dindex );
   ctx = vis5d_get_ctx (vindex );

   if (!dtx || !ctx){
      return 0;
   }
   if (dtx->Nr != ctx->Nr ||
        dtx->Nc != ctx->Nc ||
        dtx->Nl != ctx->MaxNl){
      return 0;
   }

   if (dtx->Projection != ctx->Projection){
      return 0;
   }
   if (dtx->Projection == PROJ_GENERIC ||
            dtx->Projection == PROJ_LINEAR  ||
            dtx->Projection == PROJ_CYLINDRICAL ||
            dtx->Projection == PROJ_SPHERICAL ){
      if ( dtx->NorthBound != ctx->NorthBound || 
           dtx->WestBound != ctx->WestBound ||
           dtx->RowInc != ctx->RowInc ||
           dtx->ColInc != ctx->ColInc){
         return 0;
      }
   }
   else if (dtx->Projection != PROJ_ROTATED){
      if ( dtx->NorthBound != ctx->NorthBound || 
           dtx->WestBound != ctx->WestBound ||
           dtx->RowInc != ctx->RowInc ||
           dtx->ColInc != ctx->ColInc ||
           dtx->CentralLat != ctx->CentralLat ||
           dtx->CentralLon != ctx->CentralLon ||
           dtx->Rotation != ctx->Rotation){
         return 0;
      }
   }
   else if (dtx->Projection == PROJ_LAMBERT){
      if ( dtx->Lat1 != ctx->Lat1 || 
           dtx->Lat2 != ctx->Lat2 ||
           dtx->PoleRow != ctx->PoleRow ||
           dtx->PoleCol != ctx->PoleCol ||
           dtx->CentralLon != ctx->CentralLon ||
           dtx->ColInc != ctx->ColInc){
         return 0;
      }
   }
   else if (dtx->Projection == PROJ_STEREO){
      if ( dtx->CentralLat != ctx->CentralLat || 
           dtx->CentralLon != ctx->CentralLon ||
           dtx->CentralRow != ctx->CentralRow ||
           dtx->CentralCol != ctx->CentralCol ||
           dtx->ColInc != ctx->ColInc){
         return 0;
      }
   }
   else if (dtx->Projection == PROJ_MERCATOR){
      if ( dtx->CentralLat != ctx->CentralLat ||
           dtx->CentralLon != ctx->CentralLon ||
           dtx->RowIncKm != ctx->RowIncKm ||
           dtx->ColIncKm != ctx->ColIncKm){
         return 0;
      }
   }


   /* ok good so far... */

   if (dtx->VerticalSystem != ctx->VerticalSystem){
      return 0;
   }
   if (dtx->VerticalSystem == VERT_GENERIC ||
       dtx->VerticalSystem == VERT_EQUAL_KM){
      if (dtx->BottomBound != ctx->BottomBound || 
          dtx->LevInc != ctx->LevInc ||
          dtx->TopBound != ctx->TopBound || 
          dtx->BottomBound != ctx->BottomBound){
         return 0;         
      }         
   }         
   else if (dtx->VerticalSystem == VERT_NONEQUAL_MB ||
            dtx->VerticalSystem == VERT_NONEQUAL_KM){
      for (yo=0; yo< ctx->MaxNl; yo++){
         if (dtx->Height[yo] != ctx->Height[yo]){
            return 0;
         }
      }
      if (dtx->BottomBound != ctx->BottomBound ||
          dtx->TopBound != ctx->TopBound){
         return 0;
      }
   }

   return 1;
}


/* this will load a v5d file named 'filename' and do */
/* the needed initializations */
int vis5d_load_v5dfile( int dindex, int mbs, char *filename, char *ctxname )
{
   Context ctx;
   int yo, index;
   int volflag;
   int dnumber, na[VIS5D_MAX_CONTEXTS];
   

   index = vis5d_alloc_data_context();
   ctx = ctx_table[index] = new_context();
   init_context( ctx );
   ctx->context_index = index;
   ctx->InsideInit = 1;
/* WLH 7 Oct 98
   strcpy(ctx->ContextName, ctxname);
*/
   ctx->LogFlag = 0;

   vis5d_init_memory( index, mbs);

   if (vis5d_open_gridfile( index, filename, 1 )<0) {
      /* WLH 6 Oct 98 */
      if (noexit) {
         init_context( ctx );
         ctx_table[index] = 0;
         return VIS5D_FAIL;
      }
      vis5d_terminate(1);
      exit(0);
   }

   /* WLH 7 Oct 98 */
   strcpy(ctx->ContextName, ctxname);

   /* New 5.2 */
   vis5d_get_num_of_data_sets_in_display( dindex, &dnumber);
   if (dnumber < 1){
      /* New 5.2 */   
      vis5d_init_display_values ( index, -1, dindex);
      /* don't need these i think!
      if (!setup_ctx_dtx_projection( ctx )) {
         return VIS5D_FAIL;
      }
      if (!setup_ctx_dtx_vertical_system( ctx )) {
         return VIS5D_FAIL;
      }
      vis5d_init_display_values ( index, dindex); */
      init_anim(ctx->dpy_ctx);
   }
   else{
      vis5d_assign_display_to_data( index, dindex);
   }
   /* New 5.2 */   
   if (vis5d_init_data_end( index )<0) {
      /* New 5.2 */   
      printf("Error in vis5d_init_data_end\n");
      vis5d_terminate(1);
      exit(0);
   }
   for (yo=0; yo<ctx->NumVars; yo++){
      init_var_clrtable(dindex, ctx->context_index, yo);
   }
   return ctx->context_index;
}    



/*
 * Allocate a new vis5d context and return its index number or VIS5D_FAIL
 * if the limit on contexts has been hit.
 */
int vis5d_alloc_data_context( void )
{
   int i;

   for (i=0;i<VIS5D_MAX_CONTEXTS;i++) {
      if (ctx_table[i]==NULL) {
         return i;
      }
   }

   return VIS5D_FAIL;
}

int vis5d_alloc_display_context( void )
{
   int i;

   for (i=0;i<VIS5D_MAX_DPY_CONTEXTS;i++) {
      if (dtx_table[i]==NULL) {
         return i;
      }
   }

   return VIS5D_FAIL;
}

/* This creates a new display context, and automatically */
/* picks one that has the desired index */

int vis5d_create_display_context( int index )
{
   Display_Context dtx;

   if ( (dtx = dtx_table[index])!=NULL){
      return -1;
   }
   else{
      /*not one created yet, make it and set everythging to zero */
      dtx = dtx_table[index] = new_display_context();
      dtx->dpy_context_index = index;
   }
   vis5d_reset_display_timer( index );
   return 0;
}

/*
 * Get rid of one context's resources
 */
int vis5d_destroy_data_context( int index )
{
   Context ctx;
   Display_Context dtx;
   int dindex;
   int temp, dewpt, uwind,
       vwind, var1, var2, var3;
   int tempowner, dewptowner, uwindowner,
       vwindowner, var1owner, var2owner, var3owner;
   int uvarowner, vvarowner, wvarowner;
   int u2varowner, v2varowner, w2varowner;
   int trajuowner, trajvowner, trajwowner;
   int uvar, vvar, wvar;
   int u2var, v2var, w2var;
   int traju, trajv, trajw;

   if (ctx_table[index]) {
      ctx = ctx_table[index];
      if(!ctx->dpy_ctx){
         destroy_context( ctx_table[index] );
         ctx_table[index] = NULL;
      }
      else{
         dtx = ctx->dpy_ctx;
         if (dtx->numofctxs >1){
            remove_ctx_index_from_dtx(dtx->dpy_context_index, index);
            if (dtx->ctxarray[0]==index){
               /* New 5.2 */
               vis5d_init_display_values( dtx->dpy_context_index, -1, dtx->ctxarray[0]);
            }
         }
         else{
            vis5d_reset_display_context( dtx->dpy_context_index );
         }
         destroy_context( ctx_table[index] );
         ctx_table[index] = NULL;
         dindex = dtx->dpy_context_index;
         vis5d_get_wind_vars( dindex, &uvarowner, &uvar,
                        &vvarowner,  &vvar, &wvarowner, &wvar,
                        &u2varowner, &u2var, &v2varowner, &v2var,
                        &w2varowner,  &w2var, &trajuowner,
                        &traju, &trajvowner, &trajv, &trajwowner, &trajw );
         if (uvarowner == index) {
           uvarowner = -1;
           uvar = -1;
         }
         if (vvarowner == index) {
           vvarowner = -1;
           vvar = -1;
         }
         if (wvarowner == index) {
           wvarowner = -1;
           wvar = -1;
         }
         if (u2varowner == index) {
           u2varowner = -1;
           u2var = -1;
         }
         if (v2varowner == index) {
           v2varowner = -1;
           v2var = -1;
         }
         if (w2varowner == index) {
           w2varowner = -1;
           w2var = -1;
         }
         if (trajuowner == index) {
           trajuowner = -1;
           traju = -1;
         }
         if (trajvowner == index) {
           trajvowner = -1;
           trajv = -1;
         }
         if (trajwowner == index) {
           trajwowner = -1;
           trajw = -1;
         }
         vis5d_set_wind_vars( dindex, uvarowner, uvar,
                        vvarowner,  vvar, wvarowner, wvar,
                        u2varowner, u2var, v2varowner, v2var,
                        w2varowner,  w2var, trajuowner,
                        traju, trajvowner, trajv, trajwowner, trajw );
         vis5d_get_sound_vars( dindex, &tempowner, &temp,
                                  &dewptowner, &dewpt, &uwindowner, &uwind,
                                  &vwindowner, &vwind, &var1owner, &var1,
                                  &var2owner,  &var2, &var3owner, &var3);
         if (tempowner == index) {
           tempowner = -1;
           temp = -1;
         }
         if (dewptowner == index) {
           dewptowner = -1;
           dewpt = -1;
         }
         if (uwindowner == index) {
           uwindowner = -1;
           uwind = -1;
         }
         if (vwindowner == index) {
           vwindowner = -1;
           vwind = -1;
         }
         if (var1owner == index) {
           var1owner = -1;
           var1 = -1;
         }
         if (var2owner == index) {
           var2owner = -1;
           var2 = -1;
         }
         if (var3owner == index) {
           var3owner = -1;
           var3 = -1;
         }
         vis5d_set_sound_vars( dindex, tempowner, temp,
                                  dewptowner, dewpt, uwindowner, uwind,
                                  vwindowner, vwind, var1owner, var1,
                                  var2owner,  var2, var3owner, var3);

      }
   }
   return 0;
}

/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/
int vis5d_destroy_display_context( int index )
{
   if (dtx_table[index]){
      destroy_display_context( dtx_table[index] );
      dtx_table[index] = NULL;
   }
   return 0;
}

int vis5d_reset_display_context( int index )
{
   Window tempwin;
   DPY_CONTEXT("vis5d_reset_display_context")

   set_current_window(dtx);
   clear_3d_window();
   swap_3d_window();
   XUnmapWindow( GfxDpy, dtx->GfxWindow);
   tempwin = dtx->GfxWindow;
   memset( dtx, 0, sizeof(struct display_context) );
   dtx->GfxWindow = tempwin;
   init_display_context( dtx);
   dtx->dpy_context_index = index;
   return 0;
}

/* helper functions */
void init_var_links( void )
{
   int i;
   for (i = 0; i < MAXVARS * MAXTYPES * VIS5D_MAX_CONTEXTS; i++){
      var_link[i].var = -1;
      var_link[i].vindex = -1;
      var_link[i].type = -1;
      group_var_link[i].var = -1;
      group_var_link[i].vindex = -1;
      group_var_link[i].type = -1;
   }
}



/*
 * Begin initialization stage
 */
int vis5d_init_begin( int index, int dindex )
{
   Context ctx;
   Display_Context dtx;
   static int first_time = 1;

   /*printf("sizeof(vis5d_context)=%d\n", sizeof(struct vis5d_context) );*/

   if (first_time){
      init_var_links();
   }
   if (index == -1){
      dtx = vis5d_get_dtx(dindex);
      if (!dtx){
         dtx = dtx_table[dindex] = new_display_context();
         dtx->dpy_context_index = dindex;
         init_display_context( dtx );
      }
      return 0;
   }
   else{
      if (first_time) {
         int i;
         for (i=0;i<VIS5D_MAX_CONTEXTS;i++) {
            ctx_table[i] = NULL;
         }
         first_time = 0;
         if (REVERSE_POLES != -1.0){
            REVERSE_POLES = 1.0;
         }
      }

      if (ctx_table[index]) {
         destroy_context( ctx_table[index] );
         ctx_table[index] = NULL;
      }

      ctx = ctx_table[index] = new_context();
      init_context( ctx );
      ctx->context_index = index;

      ctx->InsideInit = 1;
      
      /* create a display context too! */
      /* if it's not already created though.. */
      dtx = vis5d_get_dtx(dindex);
      if (!dtx){
         dtx = dtx_table[dindex] = new_display_context();
         dtx->dpy_context_index = dindex;
         init_display_context( dtx );
      }
      return 0;
   }
}


/****************************************/
/******** index => DATA CONTEXT *********/
/****************************************/
int vis5d_get_ctx_display_index(int index, int *display_index)
{
   Context ctx;

   if (index<0 || index>=VIS5D_MAX_CONTEXTS || (ctx = ctx_table[index])==NULL) {

       *display_index = -1;
       return -1;
   }
   *display_index = ctx->dpy_ctx->dpy_context_index;
   return 1;
}


/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/
int vis5d_get_num_of_ctxs_in_display( int index, int *number, int numarray[])
{
   int yo;
   Display_Context dtx;

   if (index<0 || index>=VIS5D_MAX_DPY_CONTEXTS || (dtx = dtx_table[index])==NULL){
      *number = 0;
      return -1;
   }     
   *number = dtx->numofctxs;
   for (yo=0; yo < *number; yo++){
      numarray[yo] = dtx->ctxarray[yo];
   }
   return 0;
}

/****************************************/
/******** index => GROUP CONTEXT ********/
/****************************************/
int vis5d_get_num_of_dtxs_in_group( int index, int *number, int numarray[])
{
   int yo;
   Display_Group grp;
   
   if (index<0 || index>=9 || (grp = grp_table[index])==NULL){
      *number = 0;
      return -1;
   }
   *number = grp->numofdpys;
   for (yo=0; yo < *number; yo++){
      numarray[yo] = grp->dpyarray[yo]->dpy_context_index;
   }
   return 0;
}


/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/
int vis5d_map_3d_window( int index )
{
   DPY_CONTEXT("vis5d_map_3d_window");
   XMapWindow(GfxDpy, dtx->GfxWindow);
   return 0;
}

/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/
int vis5d_unmap_3d_window( int index )
{
   Display_Context dtx;

   dtx = vis5d_get_dtx(index);
   if (dtx){
      if( dtx->GfxWindow ){
         XUnmapWindow(GfxDpy, dtx->GfxWindow);
      }
   }
   return 0;
}



/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/
int vis5d_get_display_window( int index, Window *win)
{
   DPY_CONTEXT("vis5d_get_display_window")
   if (dtx->GfxWindow){
      *win = dtx->GfxWindow;
   }
   else{
      *win = 0;
   }
   return 0;
}


/*
 * Make a 3-D rendering window and attach it to this vis5d context.
 * Input: 
 *         title - character string window title
 *         x, y - position of window relative to upper-left corner of screen
 *         width, height - size of window
 */
int vis5d_init_window( char *title, int x, int y,
                       int width, int height )
{
   if (make_big_window( title, x, y, width, height )) {
      return 0;
   }
   else {
      return VIS5D_FAIL;
   }
}





/***************************************************************************/
/* This get's the needed information from a vis5d data context and returns */
/* it in the form of a v5dstruct                                           */
/***************************************************************************/
/****************************************/
/******** index => DATA CONTEXT *********/
/****************************************/
int vis5d_get_ctx_values( int index, v5dstruct *v5d)
{
   int yo;
   CONTEXT("vis5d_get_ctx_values")

   v5d->Nr = ctx->Nr;
   v5d->Nc = ctx->Nc;
   v5d->Nl[0] = ctx->MaxNl;

   v5d->Projection = ctx->Projection;
   if (ctx->Projection == 0 || ctx->Projection == 1 ||
       ctx->Projection == 20 || ctx->Projection == 21){
      v5d->ProjArgs[0] = ctx->NorthBound;
      v5d->ProjArgs[1] = ctx->WestBound;
      v5d->ProjArgs[2] = ctx->RowInc;
      v5d->ProjArgs[3] = ctx->ColInc;
   }
   else if (ctx->Projection == 2 ){
      v5d->ProjArgs[0] = ctx->Lat1;
      v5d->ProjArgs[1] = ctx->Lat2;
      v5d->ProjArgs[2] = ctx->PoleRow;
      v5d->ProjArgs[3] = ctx->PoleCol;
      v5d->ProjArgs[4] = ctx->CentralLon;
      v5d->ProjArgs[5] = ctx->ColInc;
   }
   else if (ctx->Projection == 3 ){
      v5d->ProjArgs[0] = ctx->CentralLat;
      v5d->ProjArgs[1] = ctx->CentralLon;
      v5d->ProjArgs[2] = ctx->CentralRow;
      v5d->ProjArgs[3] = ctx->CentralCol;
      v5d->ProjArgs[4] = ctx->ColInc;
   }
   else if (ctx->Projection == 4){
      v5d->ProjArgs[0] = ctx->NorthBound;
      v5d->ProjArgs[1] = ctx->WestBound;
      v5d->ProjArgs[2] = ctx->RowInc;
      v5d->ProjArgs[3] = ctx->ColInc;
      v5d->ProjArgs[4] = ctx->CentralLat;
      v5d->ProjArgs[5] = ctx->CentralLon;
      v5d->ProjArgs[6] = ctx->Rotation;
   }
   else if (ctx->Projection == 5){
      v5d->ProjArgs[0] = ctx->CentralLat;
      v5d->ProjArgs[1] = ctx->CentralLon;
      v5d->ProjArgs[2] = ctx->RowIncKm;
      v5d->ProjArgs[3] = ctx->ColIncKm;
   }

   v5d->VerticalSystem = ctx->VerticalSystem;

   if (ctx->VerticalSystem == 0 || ctx->VerticalSystem == 1){
      v5d->VertArgs[0] = ctx->BottomBound;
      v5d->VertArgs[1] = ctx->LevInc;
   }
   else if (ctx->VerticalSystem == 2 || ctx->VerticalSystem == 3){
      v5d->VertArgs[0] = ctx->BottomBound;
      for (yo = 1; yo < MAXLEVELS; yo++){
         v5d->VertArgs[yo] = ctx->Height[yo];
      }
   }

   return 0;
}
   
/***************************************************************************/
/* This gets the display context values from a given vis5d data context    */
/* and returns them in the form of a v5dstruct                             */
/***************************************************************************/
/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/

int vis5d_get_dtx_values( int index,  v5dstruct *v5d)
{
   int yo;
   DPY_CONTEXT("vis5d_get_dtx_values")

   v5d->Nr = dtx->Nr;
   v5d->Nc = dtx->Nc;
   v5d->Nl[0] = dtx->Nl;

   v5d->Projection = dtx->Projection;
   if (dtx->Projection == 0 || dtx->Projection == 1 ||
       dtx->Projection == 20 || dtx->Projection == 21){
      v5d->ProjArgs[0] = dtx->NorthBound;
      v5d->ProjArgs[1] = dtx->WestBound;
      v5d->ProjArgs[2] = dtx->RowInc;
      v5d->ProjArgs[3] = dtx->ColInc;
   }
   else if (dtx->Projection == 2 ){
      v5d->ProjArgs[0] = dtx->Lat1;
      v5d->ProjArgs[1] = dtx->Lat2;
      v5d->ProjArgs[2] = dtx->PoleRow;
      v5d->ProjArgs[3] = dtx->PoleCol;
      v5d->ProjArgs[4] = dtx->CentralLon;
      v5d->ProjArgs[5] = dtx->ColInc;
   }
   else if (dtx->Projection == 3 ){
      v5d->ProjArgs[0] = dtx->CentralLat;
      v5d->ProjArgs[1] = dtx->CentralLon;
      v5d->ProjArgs[2] = dtx->CentralRow;
      v5d->ProjArgs[3] = dtx->CentralCol;
      v5d->ProjArgs[4] = dtx->ColInc;
   }
   else if (dtx->Projection == 4){
      v5d->ProjArgs[0] = dtx->NorthBound;
      v5d->ProjArgs[1] = dtx->WestBound;
      v5d->ProjArgs[2] = dtx->RowInc;
      v5d->ProjArgs[3] = dtx->ColInc;
      v5d->ProjArgs[4] = dtx->CentralLat;
      v5d->ProjArgs[5] = dtx->CentralLon;
      v5d->ProjArgs[6] = dtx->Rotation;
   }
   else if (dtx->Projection == PROJ_MERCATOR){
      v5d->ProjArgs[0] = dtx->CentralLat;
      v5d->ProjArgs[1] = dtx->CentralLon;
      v5d->ProjArgs[2] = dtx->RowIncKm;
      v5d->ProjArgs[3] = dtx->ColIncKm;
   }
   
   v5d->VerticalSystem = dtx->VerticalSystem;

   if (dtx->VerticalSystem == 0 || dtx->VerticalSystem == 1){
      v5d->VertArgs[0] = dtx->BottomBound;
      v5d->VertArgs[1] = dtx->LevInc;
   }
   else if (dtx->VerticalSystem == 2 || dtx->VerticalSystem == 3){
      v5d->VertArgs[0] = dtx->BottomBound;
      for (yo = 1; yo < MAXLEVELS; yo++){
         v5d->VertArgs[yo] = dtx->Height[yo];
      }
   }

   return 0;
}

/* WLH 9 Oct 98 - this function created from some code from
   vis5d_set_dtx_values and other code from vis5d_init_display_values */
int setup_dtx( Display_Context dtx, int index ) 
{
   Context ctx;
   int yo, w;

   make_box( dtx, 0.0, 0.0, 0.0);
   vis5d_load_topo_and_map( index );
   vis5d_set_hclip( index, 0, dtx->MaxNl-1);
   /* back */
   vis5d_set_hclip( index, 1, 0);
   /* top */
   vis5d_set_vclip( index, 0, 0, 0, 0, dtx->Nc-1);
   /* bottom */
   vis5d_set_vclip( index, 1, dtx->Nr-1, 0, dtx->Nr-1, dtx->Nc-1);
   /* left */
   vis5d_set_vclip( index, 2, 0, 0, dtx->Nr-1, 0);
   /* right */
   vis5d_set_vclip( index, 3, 0, dtx->Nc-1, dtx->Nr-1, dtx->Nc-1);


   for (yo = 0; yo < dtx->numofctxs; yo++){
      int var;
      ctx = dtx->ctxpointerarray[yo];
      for (var=0;var<ctx->NumVars;var++) {
         init_graphics_pos( ctx, var );
      }
   }

   /* WLH 9 Oct 98 */
   for (w=0;w<VIS5D_WIND_SLICES;w++) {
      /* hwind slices */
      dtx->HWindLevel[w] = (float) (dtx->WindNl-1) / 2.0;
      new_hwindslice_pos( dtx, dtx->HWindLevel[w],
                   &dtx->HWindZ[w], &dtx->HWindHgt[w] );
      dtx->HWindDensity[w] = 1.0;
      dtx->HWindScale[w] = 1.0;
      /* vwind slices */
      dtx->VWindR1[w] = (float) (dtx->Nr-1) / 2.0;
      dtx->VWindC1[w] = 0.0;
      new_vwindslice_pos( dtx, dtx->VWindR1[w], dtx->VWindC1[w],
                      &dtx->VWindX1[w], &dtx->VWindY1[w],
                      &dtx->VWindLat1[w], &dtx->VWindLon1[w] );
      dtx->VWindR2[w] = (float) (dtx->Nr-1) / 2.0;
      dtx->VWindC2[w] = (float) (dtx->Nc-1);
      new_vwindslice_pos( dtx, dtx->VWindR2[w], dtx->VWindC2[w],
                      &dtx->VWindX2[w], &dtx->VWindY2[w],
                      &dtx->VWindLat2[w], &dtx->VWindLon2[w] );
      dtx->VWindDensity[w] = dtx->VWindScale[w] = 1.0;
      /* hstream slices */
      dtx->HStreamLevel[w] = (float) (dtx->WindNl-1) / 2.0;
      new_hwindslice_pos( dtx, dtx->HStreamLevel[w],
                          &dtx->HStreamZ[w],
                          &dtx->HStreamHgt[w] );
      dtx->HStreamDensity[w] = 1.0;
      /* vstream slices */
      dtx->VStreamR1[w] = (float) (dtx->Nr-1) / 2.0;
      dtx->VStreamC1[w] = 0.0;
      new_vwindslice_pos( dtx, dtx->VStreamR1[w], dtx->VStreamC1[w],
                      &dtx->VStreamX1[w], &dtx->VStreamY1[w],
                      &dtx->VStreamLat1[w], &dtx->VStreamLon1[w] );
      dtx->VStreamR2[w] = (float) (dtx->Nr-1) / 2.0;
      dtx->VStreamC2[w] = (float) (dtx->Nc-1);
      new_vwindslice_pos( dtx, dtx->VStreamR2[w], dtx->VStreamC2[w],
                      &dtx->VStreamX2[w], &dtx->VStreamY2[w],
                      &dtx->VStreamLat2[w], &dtx->VStreamLon2[w] );
      dtx->VStreamDensity[w] = 1.0;
   }

   for (yo = 0; yo < dtx->numofctxs; yo++){
      ctx = dtx->ctxpointerarray[yo];
      ctx->GridSameAsGridPRIME = vis5d_check_dtx_same_as_ctx(dtx->dpy_context_index,
                                                          ctx->context_index);
      if (!dtx->CurvedBox) {
         if (ctx->Volume){
            free_volume( ctx );
         }
         ctx->Volume = alloc_volume( ctx, dtx->Nr, dtx->Nc, dtx->MaxNl);
      }
      else{
         if (ctx->Volume){
            free_volume( ctx);
         }
         ctx->Volume = NULL;
      }
   }
   return 0;
}


/* little helper funtion... found in proj.c too */
/* Return the sign of x */
static float Sign( float x )
{
   if (x<0.0) {
      return -1.0;
   }
   else if (x>0.0) {
      return 1.0;
   }
   else {
      return 0.0;
   }
}


/***************************************************************************/
/* This set's the given display_context to that of the v5dstruct values    */
/***************************************************************************/
/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/
int vis5d_set_dtx_values( int index, v5dstruct *v5d)
{
   int i, yo;
   float lat1, lat2;
   Context ctx;
   DPY_CONTEXT("vis5d_set_dtx_values")

   /* first get rid of all the graphics */
   /* BUG FIX MJK 8.10.98 */
   /* added this function here */ 
   turn_off_and_free_all_display_graphics( dtx );

   dtx->Nr = v5d->Nr;
   dtx->Nc = v5d->Nc;
   dtx->Nl = v5d->Nl[0];
   dtx->MaxNl = dtx->Nl;

   /* BUG FIX MJK 8.10.98 */
   /* added  dtx->VolRen so that when changing to PROJ_CYLINDRICAL */
   /* or PROJ_SPHERICAL on the fly, the new gui will not allow vol rending */
   if (v5d->Projection == PROJ_CYLINDRICAL || v5d->Projection == PROJ_SPHERICAL){
      dtx->VolRender = 0;
   }
  
   dtx->Projection = v5d->Projection;
   if (v5d->Projection == PROJ_GENERIC ||
       v5d->Projection == PROJ_LINEAR  ||
       v5d->Projection == PROJ_CYLINDRICAL ||
       v5d->Projection == PROJ_SPHERICAL){
      dtx->NorthBound = v5d->ProjArgs[0];
      dtx->WestBound = v5d->ProjArgs[1];
      dtx->RowInc = v5d->ProjArgs[2];
      dtx->ColInc = v5d->ProjArgs[3];
      dtx->SouthBound = dtx->NorthBound - dtx->RowInc * (dtx->Nr-1);
      dtx->EastBound = dtx->WestBound - dtx->ColInc * (dtx->Nc-1);
      if (v5d->Projection == PROJ_CYLINDRICAL){
         if (REVERSE_POLES==-1.0){
            dtx->CylinderScale = 1.0 / (-1.0*(-90.0-dtx->NorthBound));
         }
         else{
            dtx->CylinderScale = 1.0 / (90.0-dtx->SouthBound);
         }
      }
   }
   else if (v5d->Projection == PROJ_MERCATOR ){
      dtx->CentralLat = v5d->ProjArgs[0];
      dtx->CentralLon = v5d->ProjArgs[1];
      dtx->RowIncKm = v5d->ProjArgs[2];
      dtx->ColIncKm = v5d->ProjArgs[3];
   }
   else if (v5d->Projection == PROJ_LAMBERT ){
      dtx->Lat1 = v5d->ProjArgs[0]; 
      dtx->Lat2 = v5d->ProjArgs[1];
      dtx->PoleRow = v5d->ProjArgs[2];
      dtx->PoleCol = v5d->ProjArgs[3];
      dtx->CentralLon = v5d->ProjArgs[4];
      dtx->ColInc = v5d->ProjArgs[5];
      if (dtx->Lat1==dtx->Lat2) {
         /* polar stereographic? */
         if (dtx->Lat1>0.0) {
            lat1 = (90.0 - dtx->Lat1) * DEG2RAD;
         }
         else {
            lat1 = (90.0 + dtx->Lat1) * DEG2RAD;
         }
         dtx->Cone = cos( lat1 );
         dtx->Hemisphere = 1.0;
      }
      else {
         /* general Lambert conformal */
         float a, b;
         if (Sign(dtx->Lat1) != Sign(dtx->Lat2)) {
            printf("Error: standard latitudes must have the same sign.\n");
            return 0;
         }
         if (dtx->Lat1<dtx->Lat2) {
            printf("Error: Lat1 must be >= dtx->Lat2\n");
            return 0;
         }
         dtx->Hemisphere = 1.0;
         lat1 = (90.0 - dtx->Lat1) * DEG2RAD;
         lat2 = (90.0 - dtx->Lat2) * DEG2RAD;
         a = log(sin(lat1)) - log(sin(lat2));
         b = log( tan(lat1/2.0) ) - log( tan(lat2/2.0) );
         dtx->Cone = a / b;
      }

      /* Cone is in [-1,1] */
      dtx->ConeFactor = RADIUS * sin(lat1)
                       / (dtx->ColInc * dtx->Cone
                          * pow(tan(lat1/2.0), dtx->Cone) );
   }
   else if (v5d->Projection == PROJ_STEREO ){
      dtx->CentralLat = v5d->ProjArgs[0];
      dtx->CentralLon = v5d->ProjArgs[1];
      dtx->CentralRow = v5d->ProjArgs[2];
      dtx->CentralCol = v5d->ProjArgs[3];
      dtx->ColInc = v5d->ProjArgs[4];
      dtx->CosCentralLat = cos( dtx->CentralLat * DEG2RAD );
      dtx->SinCentralLat = sin( dtx->CentralLat * DEG2RAD );
      dtx->StereoScale = (2.0 * RADIUS / dtx->ColInc);
      dtx->InvScale = 1.0 / dtx->StereoScale;
   }
   else if (v5d->Projection == PROJ_ROTATED){
      dtx->NorthBound = v5d->ProjArgs[0];
      dtx->WestBound = v5d->ProjArgs[1];
      dtx->RowInc = v5d->ProjArgs[2];
      dtx->ColInc = v5d->ProjArgs[3];
      dtx->CentralLat = v5d->ProjArgs[4];
      dtx->CentralLon = v5d->ProjArgs[5];
      dtx->Rotation = v5d->ProjArgs[6];
      dtx->SouthBound = dtx->NorthBound - dtx->RowInc * (dtx->Nr-1);
      dtx->EastBound = dtx->WestBound - dtx->ColInc * (dtx->Nc-1);
   }
   /* MJK 12.28.99 */
   if (dtx->Projection != PROJ_GENERIC && dtx->Projection != PROJ_MERCATOR) {
/*
   if (dtx->Projection != PROJ_GENERIC) {
*/
      if (dtx->SouthBound < -90.0) {
         printf("SouthBound less than -90.0\n");
         /* New 5.2 */
         vis5d_init_display_values(dtx->ctxarray[0], -1, index); 
         printf("setting display to values of ctx-%d\n",dtx->ctxarray[0]); 
         return 0;
      }
/* WLH 3 Nov 98
      if (dtx->NorthBound < dtx->SouthBound) {
         printf("NorthBound less than SouthBound\n");
         vis5d_init_display_values(dtx->ctxarray[0], index);
         printf("setting display to values of ctx-%d\n",dtx->ctxarray[0]);
         return 0;
      }
*/
      if (90.0 < dtx->NorthBound) {
         printf("NorthBound greater than 90.0\n");
         /* New 5.2 */
         vis5d_init_display_values(dtx->ctxarray[0], -1, index);
         printf("setting display to values of ctx-%d\n",dtx->ctxarray[0]);
         return 0;
      }
   }

   dtx->VerticalSystem = v5d->VerticalSystem;

   if (v5d->VerticalSystem == VERT_GENERIC ||
       v5d->VerticalSystem == VERT_EQUAL_KM){
      dtx->BottomBound = v5d->VertArgs[0];
      dtx->LevInc = v5d->VertArgs[1];
      dtx->TopBound = dtx->BottomBound + dtx->LevInc * (dtx->MaxNl-1);
      for (i=0;i<dtx->MaxNl;i++) {
         dtx->Height[i] = dtx->BottomBound + i * dtx->LevInc;
      }
      if (dtx->LogFlag) {
        dtx->Ptop = dtx->LogScale * exp( dtx->TopBound / dtx->LogExp );
        dtx->Pbot = dtx->LogScale * exp( dtx->BottomBound / dtx->LogExp );
      }
   }
   else if (v5d->VerticalSystem == VERT_NONEQUAL_KM ||
            v5d->VerticalSystem == VERT_NONEQUAL_MB){
      dtx->BottomBound = v5d->VertArgs[0];
      for (yo = 0; yo < MAXLEVELS; yo++){
         dtx->Height[yo] = v5d->VertArgs[yo];
      }
      dtx->TopBound = dtx->Height[dtx->MaxNl-1];
      if (v5d->VerticalSystem == VERT_NONEQUAL_KM){
         dtx->Ptop = dtx->LogScale * exp( dtx->Height[dtx->MaxNl-1] / dtx->LogExp );
         dtx->Pbot = dtx->LogScale * exp( dtx->Height[0] / dtx->LogExp );
      }
      else if (v5d->VerticalSystem == VERT_NONEQUAL_MB){
         dtx->Ptop = height_to_pressure(dtx->Height[dtx->MaxNl-1]);
         dtx->Pbot = height_to_pressure(dtx->Height[0]);
      }
   }
/* WLH 3 Nov 98
   if (dtx->TopBound < dtx->BottomBound) {
       printf("TopBound less than BottomBound\n");
       vis5d_init_display_values(dtx->ctxarray[0], index);
       printf("setting display to values of ctx-%d\n",dtx->ctxarray[0]);
       return 0;
   }
*/
   if (dtx->Projection == PROJ_SPHERICAL &&
      dtx->TopBound == dtx->BottomBound){
     dtx->TopBound = dtx->BottomBound + 10;
   }
   /* WLH 9 Oct 98 */
   setup_dtx(dtx, index); 

   return 0;
}
 



/***************************************************************************/
/* This function is used when a vis5d data context is attatched to a       */
/* display which has no values.  It just sets the desired display values   */
/* to that of the default vis5d data context values                        */
/***************************************************************************/
/****************************************/
/******** index => DATA CONTEXT *********/
/****************************************/
/* New 5.2 */
int vis5d_init_display_values ( int index, int iindex, int display )
{
   int x;
   int yo;
   Display_Context dtx;
   Context ctx;
   /* New 5.2 */
   /*  |   |  */
   /* \|/ \|/ */
   /***********/
   Irregular_Context itx;
   int var, time, i, j, k, w;

   if (index != -1 && iindex != -1 ){
      printf("Error in vis5d_init_display_values\n");
      return 0;
   }


   if ( index == -1 && iindex == -1){
      ctx = NULL;
      itx = NULL;
   }
   else if (index >= 0 ){
      if (index<0 || index>=VIS5D_MAX_CONTEXTS || (ctx = ctx_table[index])==NULL) {
         printf("bad context in vis5d_init_display_values\n");
      }
      itx = NULL;
   }   
   else{
      if (iindex<0 || iindex>=VIS5D_MAX_CONTEXTS || (itx = itx_table[iindex])==NULL) {
         printf("bad irregular context in vis5d_init_display_values\n");
         debugstuff();
      }
      ctx = NULL;
   }


   if (display < 0 || display >=VIS5D_MAX_DPY_CONTEXTS){
      printf("error in vis5d_init_display_values\n");
   }
   else if ( (dtx = dtx_table[display])== NULL){
      dtx = dtx_table[display] = new_display_context();
      dtx->dpy_context_index = display;
      dtx->numofctxs = 0;
      dtx->ctxarray[0] = -1;
      dtx->group_index = 0;
   }
   else{
      dtx = vis5d_get_dtx( display );
   }
   if (!ctx && !itx){
      return 0;
   }

   
   vis5d_reset_display_timer(dtx->dpy_context_index);

   if (ctx){
      memset( ctx->SurfTable, 0, sizeof(ctx->SurfTable) );
      memset( ctx->HSliceTable, 0, sizeof(ctx->HSliceTable) );
      memset( ctx->VSliceTable, 0, sizeof(ctx->VSliceTable) );
      memset( ctx->CHSliceTable, 0, sizeof(ctx->CHSliceTable) );
      memset( ctx->CVSliceTable, 0, sizeof(ctx->CVSliceTable) );
      memset(  ctx->DisplaySurf, 0, sizeof(ctx->DisplaySurf) );
      memset(  ctx->DisplayHSlice, 0, sizeof(ctx->DisplayHSlice) );
      memset(  ctx->DisplayVSlice, 0, sizeof(ctx->DisplayVSlice) );
      memset(  ctx->DisplayCHSlice, 0, sizeof(ctx->DisplayCHSlice) );
      memset(  ctx->DisplayCVSlice, 0, sizeof(ctx->DisplayCVSlice) );
      ctx->CurTime = 0; 
      memset( ctx->ExpressionList, 0, sizeof(ctx->ExpressionList) );
   }

   if (itx){
      memset( itx->TextPlotTable, 0, sizeof(itx->TextPlotTable) );
   }

   memset(  dtx->DisplayTraj, 0, sizeof(dtx->DisplayTraj) );
   memset(  dtx->DisplayHWind, 0, sizeof(dtx->DisplayHWind) );
   memset(  dtx->DisplayVWind, 0, sizeof(dtx->DisplayVWind) );
   memset(  dtx->DisplayHStream, 0, sizeof(dtx->DisplayHStream) );
   memset( dtx->HWindTable, 0, sizeof(dtx->HWindTable) );
   memset( dtx->VWindTable, 0, sizeof(dtx->VWindTable) );
   memset( dtx->TrajTable, 0, sizeof(dtx->TrajTable) );
   memset(  dtx->DisplayVStream, 0, sizeof(dtx->DisplayVStream) );
   dtx->CurrentVolume = -1;
   dtx->CurrentVolumeOwner = -1;
   dtx->CurTime = 0;
   dtx->NumTraj = 0;

   /* Set 1 on traj's automatcially on */
   dtx->DisplayTraj[0] = 1;

   for (i=0;i<VIS5D_WIND_SLICES;i++) {
      dtx->Uvar[i] = dtx->Vvar[i] = dtx->Wvar[i] = -1;
   }
   dtx->TrajU = dtx->TrajV = dtx->TrajW = -1;


   dtx->Zoom = 1.0;
   memcpy( dtx->CTM, init_ctm, 16*sizeof(float) );
   dtx->CursorColor = &dtx->TrajColor[0];

   dtx->Redraw = 0;
   if (ctx){
      dtx->Nr = ctx->Nr;
      dtx->Nc = ctx->Nc;
      yo = MAXLEVELS;
      for (x= 0; x < MAXVARS; x++){
         if (ctx->LowLev[x] < yo ){
            yo = ctx->LowLev[x];
         }
      }
      dtx->LowLev = yo;
      yo = 0;
      for (x = 0; x< MAXVARS; x++){
         if (yo < ctx->Nl[x]){
            yo = ctx->Nl[x];
         }
      }
      dtx->MaxNl = yo;
      dtx->Nl = yo;
      dtx->NumTimes = ctx->NumTimes;
      dtx->NumVars = ctx->NumVars;
      dtx->WindNl = ctx->WindNl;
      dtx->WindLow = ctx->WindLow;
      dtx->CurTime = ctx->CurTime;
      dtx->Projection = ctx->Projection;
      dtx->NorthBound = ctx->NorthBound;
      dtx->SouthBound = ctx->SouthBound;
      dtx->WestBound = ctx->WestBound;
      dtx->EastBound = ctx->EastBound;
      dtx->RowInc = ctx->RowInc;
      dtx->ColInc = ctx->ColInc;
      dtx->Lat1 = ctx->Lat1;
      dtx->Lat2 = ctx->Lat2;
      dtx->PoleRow = ctx->PoleRow;
      dtx->PoleCol = ctx->PoleCol;
      dtx->CentralLat = ctx->CentralLat;
      dtx->CentralLon = ctx->CentralLon;
      dtx->CentralRow = ctx->CentralRow;
      dtx->CentralCol = ctx->CentralCol;
      dtx->Rotation = ctx->Rotation;
      dtx->Cone = ctx->Cone;
      dtx->Hemisphere = ctx->Hemisphere;
      dtx->ConeFactor = ctx->ConeFactor;
      dtx->CosCentralLat = ctx->CosCentralLat;
      dtx->SinCentralLat = ctx->SinCentralLat;
      dtx->StereoScale = ctx->StereoScale;
      dtx->InvScale = ctx->InvScale;
      dtx->CylinderScale = ctx->CylinderScale;
      dtx->CentralLat = ctx->CentralLat;
      dtx->CentralLon = ctx->CentralLon;
      dtx->RowIncKm = ctx->RowIncKm;
      dtx->ColIncKm = ctx->ColIncKm;
      dtx->VerticalSystem = ctx->VerticalSystem;
      dtx->LevInc = ctx->LevInc;
      dtx->BottomBound = ctx->BottomBound;
      dtx->TopBound = ctx->TopBound;
      for (yo = 0; yo < MAXLEVELS; yo++){
         dtx->Height[yo] = ctx->Height[yo];
      }
      dtx->LogFlag = ctx->LogFlag;
      dtx->LogScale = ctx->LogScale;
      dtx->LogExp = ctx->LogExp;
      dtx->Ptop = ctx->Ptop;
      dtx->Pbot = ctx->Pbot;
   }
   else{
      dtx->CurTime = 0;
      dtx->NumTimes = itx->NumTimes;
      dtx->Projection = PROJ_LINEAR;
      dtx->VerticalSystem = VERT_EQUAL_KM;
      dtx->NorthBound = itx->NorthBound;
      dtx->SouthBound = itx->SouthBound;
      dtx->WestBound = itx->WestBound;
      dtx->EastBound = itx->EastBound;
      dtx->TopBound = itx->TopBound;
      dtx->BottomBound = itx->BottomBound;
      dtx->Nr = 10;
      dtx->Nc = 10;
      dtx->Nl = 10;
      dtx->RowInc = (dtx->NorthBound - dtx->SouthBound)/dtx->Nr;
      dtx->ColInc = (dtx->WestBound - dtx->EastBound)/dtx->Nc;
      dtx->LevInc = (dtx->TopBound - dtx->BottomBound)/dtx->Nl;
      
      for (yo = 0; yo < MAXLEVELS; yo++){
         dtx->Height[yo] = dtx->BottomBound + dtx->LevInc * (float)(yo);
      }
      dtx->LogFlag = 0;
      dtx->MaxNl = dtx->Nl;
   }

#ifdef OPENGL
   if (ctx){
      memcpy( dtx->ModelMat, ctx->ModelMat, 16*sizeof(float) );
      memcpy( dtx->ProjMat, ctx->ProjMat, 16*sizeof(float) );
   }
#endif
   vis5d_get_dtx_values(dtx->dpy_context_index, &dtx->G);
   if (ctx){
      vis5d_assign_display_to_data( index, display);
   }
   else{
      vis5d_assign_display_to_irregular_data( iindex, display);
   }

   if (dtx->TopoName[0] == 0){
      vis5d_init_topo(display, "EARTH.TOPO", 0); 
   }
   vis5d_init_map(display, dtx->MapName);

   if (ctx){
      if (!setup_ctx_dtx_projection( ctx )) {
         return VIS5D_FAIL;
      }
      if (!setup_ctx_dtx_vertical_system( ctx )) {
         return VIS5D_FAIL;
      }
   }

   if ( (ctx && ctx->ColInc > 0)
        || (itx && dtx->ColInc > 0)){
      if ((dtx->Projection != PROJ_MERCATOR) ||
          (dtx->Projection == PROJ_MERCATOR && dtx->ColIncKm > 0)){
         make_box( dtx, dtx->Ax, dtx->Ay, dtx->Az); 
         vis5d_load_topo_and_map( display );   
      }
   }

   if (ctx){
      compute_wind_levels(dtx); 
      for (var=0;var<VIS5D_WIND_SLICES;var++) {
         for (time=0;time<MAXTIMES;time++) {
            dtx->HWindTable[var][time].valid = 0;
            dtx->VWindTable[var][time].valid = 0;
            dtx->HStreamTable[var][time].valid = 0;
            dtx->VStreamTable[var][time].valid = 0;
         }
      }
      for (w=0;w<VIS5D_WIND_SLICES;w++) {
         /* hwind slices */
         dtx->HWindLevel[w] = (float) (dtx->WindNl-1) / 2.0;
         new_hwindslice_pos( dtx, dtx->HWindLevel[w], 
                      &dtx->HWindZ[w], &dtx->HWindHgt[w] );
         dtx->HWindDensity[w] = 1.0;
         dtx->HWindScale[w] = 1.0;
         /* vwind slices */
         dtx->VWindR1[w] = (float) (dtx->Nr-1) / 2.0;
         dtx->VWindC1[w] = 0.0;
         new_vwindslice_pos( dtx, dtx->VWindR1[w], dtx->VWindC1[w],
                         &dtx->VWindX1[w], &dtx->VWindY1[w],
                         &dtx->VWindLat1[w], &dtx->VWindLon1[w] );
         dtx->VWindR2[w] = (float) (dtx->Nr-1) / 2.0;
         dtx->VWindC2[w] = (float) (dtx->Nc-1);
         new_vwindslice_pos( dtx, dtx->VWindR2[w], dtx->VWindC2[w],
                         &dtx->VWindX2[w], &dtx->VWindY2[w],
                         &dtx->VWindLat2[w], &dtx->VWindLon2[w] );
         dtx->VWindDensity[w] = dtx->VWindScale[w] = 1.0;
         /* hstream slices */
         dtx->HStreamLevel[w] = (float) (dtx->WindNl-1) / 2.0;
         new_hwindslice_pos( dtx, dtx->HStreamLevel[w], 
                             &dtx->HStreamZ[w],
                             &dtx->HStreamHgt[w] );
         dtx->HStreamDensity[w] = 1.0;
         /* vstream slices */
         dtx->VStreamR1[w] = (float) (dtx->Nr-1) / 2.0;
         dtx->VStreamC1[w] = 0.0;
         new_vwindslice_pos( dtx, dtx->VStreamR1[w], dtx->VStreamC1[w],
                         &dtx->VStreamX1[w], &dtx->VStreamY1[w],
                         &dtx->VStreamLat1[w], &dtx->VStreamLon1[w] );
         dtx->VStreamR2[w] = (float) (dtx->Nr-1) / 2.0;
         dtx->VStreamC2[w] = (float) (dtx->Nc-1);
         new_vwindslice_pos( dtx, dtx->VStreamR2[w], dtx->VStreamC2[w],
                         &dtx->VStreamX2[w], &dtx->VStreamY2[w],
                         &dtx->VStreamLat2[w], &dtx->VStreamLon2[w] );
         dtx->VStreamDensity[w] = 1.0;
      }  
   }
   vis5d_graphics_mode(display, VIS5D_JULIAN, VIS5D_ON);

   if (ctx && dtx->RowInc != 0.0 &&  dtx->ColInc != 0.0){
      init_trajPRIME(ctx->dpy_ctx);
   }

   /* initialize cursor position */
   {
      float row, col, lev;

      row = (dtx->Nr-1) / 2.0;
      col = (dtx->Nc-1) / 2.0;
      lev = (dtx->MaxNl-1) / 2.0;

      gridPRIME_to_xyzPRIME( dtx, -1, -1, 1, &row, &col, &lev, &dtx->CursorX,
                   &dtx->CursorY, &dtx->CursorZ );
   }

   /* misc sounding stuff */
   dtx->Sound.mainvarstep = 50;
   dtx->Sound.SndMinTemp = 228.0;
   dtx->Sound.SndMaxTemp = 323.0;
   dtx->Sound.tickstatus = 0;
   dtx->Sound.currentX = .69;
   dtx->Sound.currentY = .69;
   dtx->Sound.currentTime =1069;
   dtx->Sound.soundline = NULL;
   dtx->Sound.uwindline = NULL;
   dtx->Sound.vwindline = NULL;
   dtx->Sound.tgrid = NULL;
   dtx->Sound.dgrid = NULL;
   dtx->Sound.ugrid = NULL;
   dtx->Sound.var1grid = NULL;
   dtx->Sound.var2grid = NULL;
   dtx->Sound.var3grid = NULL;
   dtx->Sound.vertdata = NULL;

   if (ctx){
      dtx->Sound.PreviousSoundTemp = vis5d_find_var(dtx->ctxarray[0],"T");
      dtx->Sound.PreviousSoundDewpt= vis5d_find_var(dtx->ctxarray[0],"TD");
      dtx->Sound.PreviousSoundUWind= vis5d_find_var(dtx->ctxarray[0],"U");
      dtx->Sound.PreviousSoundVWind= vis5d_find_var(dtx->ctxarray[0],"V");
   }
   else{
      dtx->Sound.PreviousSoundTemp = -1;
      dtx->Sound.PreviousSoundDewpt= -1;
      dtx->Sound.PreviousSoundUWind= -1;
      dtx->Sound.PreviousSoundVWind= -1;
   }
   /***********/
   /* /|\ /|\ */
   /*  |   |  */
   /* New 5.2 */
   /***********/

   if (dtx->Xmax != 0.0 && dtx->Xmin != 0.0 &&
       dtx->Ymax != 0.0 && dtx->Ymin != 0.0 &&
       dtx->Zmax != 0.0 && dtx->Zmin != 0.0){
      /* front */
      vis5d_set_hclip( display, 0, dtx->MaxNl-1);
      /* back */
      vis5d_set_hclip( display, 1, 0);
      /* top */
      vis5d_set_vclip( display, 0, 0, 0, 0, dtx->Nc-1);
      /* bottom */
      vis5d_set_vclip( display, 1, dtx->Nr-1, 0, dtx->Nr-1, dtx->Nc-1);
      /* left */
      vis5d_set_vclip( display, 2, 0, 0, dtx->Nr-1, 0);
      /* right */
      vis5d_set_vclip( display, 3, 0, dtx->Nc-1, dtx->Nr-1, dtx->Nc-1);
   }
   return 0;

}
      
/* MJK 12.02.98 begin */
/*
 * Control which variables to display with the probe.
 * Input:  index - context index
 *         numvars - number of variables
 *                   -1 for all variables
 *                   0 for no variables (but position is still displayed)
 *         varlist - list of variable indices
 *                   NULL with numvars = 0 or numvars = -1
 */
int vis5d_set_probe_vars (int index, int numvars, int *varlist)
{
   int  i, ivar;
   CONTEXT("vis5d_set_probe_vars")

   for (i = 0; i < ctx->NumVars; i++) ctx->ProbeVar[i] = 0;

   ctx->ProbeNumVars = numvars;
   if (ctx->ProbeNumVars <= 0) return 0;

   if (varlist == NULL)
   {
      ctx->ProbeNumVars = 0;
   }
   else
   {
      for (i = 0; i < numvars; i++)
      {
         ivar = varlist[i];
         if (ivar >= 0) ctx->ProbeVar[ivar] = i + 1;
      }
   }


   return 0;
}
/* MJK 12.02.98 end */



/*
 * Make a 2-D sounding window and attach it to this vis5d context.
 * Input:  index - the context number
 *         title - character string window title
 *         x, y - position of window relative to upper-left corner of screen
 *         width, height - size of window
 *         scw - parent (sound control window) or null
 */
int vis5d_init_sndwindow( int index, char *title, int x, int y,
                          int width, int height, Window scw, char *wdpy_name )
{
   DPY_CONTEXT("vis5d_make_sndwindow");

   dtx->Sound.soundwin = NULL;
   if (make_soundGFX_window( dtx, title, x, y, width, height, scw, wdpy_name)) {
      return 0;
   }
   else {
      return VIS5D_FAIL;
   }
}

/*
 * Map the 2-D sounding graphics window
 * Input:  index - the context number
 *         show -  1 map the sndwindow, 0 unmap the sndwindow
 *
 */
/* MJK 12.10.98 begin*/
int vis5d_map_sndwindow( int index)
{
   DPY_CONTEXT("vis5d_map_sndwindow");

   XSynchronize(SndDpy, 1);
   if (dtx->Sound.SoundCtrlWindow){
      XMapWindow( SndDpy, dtx->Sound.SoundCtrlWindow);
   }
   XMapWindow( SndDpy, dtx->Sound.soundwin);
   XSynchronize(SndDpy, 0);
   return 0;
}

int vis5d_unmap_sndwindow( int index )
{
   DPY_CONTEXT("vis5d_map_sndwindow");

   if (dtx->Sound.SoundCtrlWindow){
      XUnmapWindow( SndDpy, dtx->Sound.SoundCtrlWindow);
   }
   else{
      XMapWindow( SndDpy, dtx->Sound.soundwin);
   }
   return 0;
}
/* MJK 12.10.98 end */
#ifdef SGI_GL
/*
 * Attach a "pure" IRIS GL window to this vis5d context.
 */
int vis5d_init_gl_window( int index, Display *dpy, Window window, long winid )
{
   CONTEXT("vis5d_init_gl_window");
   use_gl_window( ctx, dpy, window, winid );
   return 0;
}
#endif



#ifdef SGI_GL
/*
 * Attach a "mixed-mode" IRIS GL window to this vis5d context.
 */
int vis5d_init_glx_window( int index, Display *dpy, Window window,
                          GLXconfig *glctx )
{
   CONTEXT("vis5d_init_glx_window");
   use_glx_window( ctx, dpy, window, glctx );
   return 0;
}
#endif



#ifdef OPENGL
/*
 * Attach an OpenGL window and rendering context to this vis5d context.
 */
int vis5d_init_opengl_window( int index, Display *dpy, Window window,
                              GLXContext glctx )
{
   DPY_CONTEXT("vis5d_init_opengl_window");
   return use_opengl_window( dtx, dpy, window, glctx, NULL );
}
#endif



/*
 * Specify the size of memory pool for the context in megabytes.
 */
int vis5d_init_memory( int index, int mbs )
{
   CONTEXT("vis5d_init_memory");
   ctx->MegaBytes = mbs;
   return 0;
}


/* if this function is called then same scale is set */
/* and the vertical plot variables will be ploted all on the same scale */
/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/
int vis5d_init_samescale( int index )
{
   DPY_CONTEXT("vis5d_init_samescale");

   dtx->Sound.samestepflag = 1;
   return 0;
}

/*
 * Specify the aspect ratio of the 3-D box.
 * Input: index - the context number
 *        alon, alat, ahgt - aspect ratios:  latitude:longitude:height
 */
/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/
int vis5d_init_box( int index, float alon, float alat, float ahgt )
{
   DPY_CONTEXT("vis5d_init_box");
   dtx->Ax = alon;
   dtx->Ay = alat;
   dtx->Az = ahgt;
   return 0;
}

/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/
int vis5d_make_box( int index, float x, float y, float z)
{
   DPY_CONTEXT("vis5d_make_box");
   if ( dtx->ColInc > 0 ){
      make_box( dtx, x, y, z);
   }
   if (dtx->Xmax != 0.0 && dtx->Xmin != 0.0 &&
       dtx->Ymax != 0.0 && dtx->Ymin != 0.0 &&
       dtx->Zmax != 0.0 && dtx->Zmin != 0.0){
      /* front */
      vis5d_set_hclip( index, 0, dtx->MaxNl-1);
      /* back */
      vis5d_set_hclip( index, 1, 0);
      /* top */
      vis5d_set_vclip( index, 0, 0, 0, 0, dtx->Nc-1);
      /* bottom */
      vis5d_set_vclip( index, 1, dtx->Nr-1, 0, dtx->Nr-1, dtx->Nc-1);
      /* left */
      vis5d_set_vclip( index, 2, 0, 0, dtx->Nr-1, 0);
      /* right */
      vis5d_set_vclip( index, 3, 0, dtx->Nc-1, dtx->Nr-1, dtx->Nc-1);
   }

   return 0;
}


/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/
int vis5d_init_log( int index, int flag, float scale, float exponent )
{
   int i;
   DPY_CONTEXT("vis5d_init_log");

   dtx->LogFlag = flag;
   dtx->LogScale = scale;
   dtx->LogExp = exponent;
   if(flag){
      switch (dtx->VerticalSystem) {
         case VERT_GENERIC:
         case VERT_EQUAL_KM:
            dtx->TopBound = dtx->BottomBound + dtx->LevInc * (dtx->MaxNl-1);
            for (i=0;i<dtx->Nl;i++) {
               dtx->Height[i] = dtx->BottomBound + i * dtx->LevInc;
            }
            if (dtx->LogFlag) {
              dtx->Ptop = dtx->LogScale * exp( dtx->TopBound / dtx->LogExp );
              dtx->Pbot = dtx->LogScale * exp( dtx->BottomBound / dtx->LogExp );
            }
            break;
         case VERT_NONEQUAL_KM:
            if (dtx->LogFlag) {
              dtx->Ptop = dtx->LogScale * exp( dtx->Height[dtx->MaxNl-1] / dtx->LogExp );
              dtx->Pbot = dtx->LogScale * exp( dtx->Height[0] / dtx->LogExp );
            }
            break;
         case VERT_NONEQUAL_MB:
            dtx->Ptop = height_to_pressure(dtx->Height[dtx->MaxNl-1]);
            dtx->Pbot = height_to_pressure(dtx->Height[0]);
            /* The Height[] array should be OK at this point */
            break;
         default:
            return 0;
      }
   }

   return 1;
}

/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/
int vis5d_get_log( int index, int *flag, float *scale, float *exponent)
{
   DPY_CONTEXT("vis5d_get_log");
   *flag = dtx->LogFlag;
   *scale = dtx->LogScale;
   *exponent = dtx->LogExp;
   return 0;
}


/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/
int vis5d_init_map( int index, char *mapname )
{
   Display_Context dtx;      
   if (index < 0 || index >=VIS5D_MAX_DPY_CONTEXTS){   
      printf("error in vis5d_init_map\n");
      return -1;   
   }   
   else if ((dtx = dtx_table[index])== NULL){   
      dtx = dtx_table[index] = new_display_context();   
      dtx->dpy_context_index = index;   
      dtx->numofctxs = 0;   
      dtx->ctxarray[0] = -1;   
      dtx->group_index = 0;   
   }   
   else{   
      dtx = vis5d_get_dtx( index );   
   }   
   if (mapname){
      strcpy( dtx->MapName, mapname );
   }
   else{
      dtx->MapName[0] = 0;
   }
   return 0;
}

/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/
int vis5d_get_map( int index, char *mapname )
{
   DPY_CONTEXT("vis5d_get_map");
   strcpy(mapname, dtx->MapName);
   return 0;
}

/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/
int vis5d_init_topo_and_map_ctx( int index, char *toponame, int highres_flag )
{
   DPY_CONTEXT("vis5d_init_topo_ctx");
   strcpy( dtx->TopoName, toponame );
   dtx->HiResTopo = highres_flag;
   dtx->TopoFlag = 1;
   dtx->MapFlag = 1; 
   return 0;
}

/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/
int vis5d_init_topo( int index, char *toponame, int highres_flag )
{
   Display_Context dtx;   
   if (index < 0 || index >=VIS5D_MAX_DPY_CONTEXTS){
      printf("error in vis5d_init_topo\n");
      return -1;
   }
   else if ((dtx = dtx_table[index])== NULL){
      dtx = dtx_table[index] = new_display_context();
      dtx->dpy_context_index = index;
      dtx->numofctxs = 0;
      dtx->ctxarray[0] = -1;
      dtx->group_index = 0;
   }
   else{
      dtx = vis5d_get_dtx( index );
   }
   strcpy( dtx->TopoName, toponame );
   dtx->HiResTopo = highres_flag;

   return 0;
}

/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/
int vis5d_get_topo( int index, char *toponame)
{
   DPY_CONTEXT("vis5d_get_topo");
   strcpy( toponame, dtx->TopoName);
   return 0;
}

/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/
/* MJK 4.27.99
int vis5d_init_path( int index, char *pathname )
{
   int len;
   DPY_CONTEXT("vis5d_init_path");

   strcpy( dtx->Path, pathname );
   len = strlen(dtx->Path);
   if (len>0 && dtx->Path[len-1]!='/') {
      strcat( dtx->Path, "/" );
   }

   return 0;
}
*/
int vis5d_init_path( char *pathname )
{
   int len;
   strcpy( Vis5dDataPath, pathname );

   /* Make sure Path ends with a / */
   len = strlen(Vis5dDataPath);
   if (len>0 && Vis5dDataPath[len-1]!='/') {
      strcat( Vis5dDataPath, "/" );
   }

   return 0;
}

/* MJK 12.02.98 */
int vis5d_init_clock( int index, int clock)
{
   DPY_CONTEXT("vis5d_init_clock");
   dtx->CircleClock = clock;
   return 0;
}


/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/
int vis5d_init_texture( int index, char *texturename )
{
   DPY_CONTEXT("vis5d_init_texture");
   strcpy( dtx->TextureName, texturename );
   return 0;
}

/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/
int vis5d_get_texture( int index, char *texturename )
{
   DPY_CONTEXT("vis5d_get_texture")
   strcpy( texturename, dtx->TextureName);
   return 0;
}

/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/
int vis5d_init_firstarea( int index, int area )
{
   DPY_CONTEXT("vis5d_init_firstarea");
   dtx->FirstArea = area;
   return 0;
}

/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/
int vis5d_get_firstarea( int index )
{
   DPY_CONTEXT("vis5d_get_firstarea")
   return dtx->FirstArea;
}

/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/
int vis5d_init_sequence( int index, char *sequencename )
{
   DPY_CONTEXT("vis5d_init_sequence");
   strcpy( dtx->SequenceName, sequencename );
   return 0;
}

/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/
int vis5d_get_sequence( int index, char *sequencename )
{
   DPY_CONTEXT("vis5d_get_sequence");
   strcpy( sequencename, dtx->SequenceName);
   return 0;
}


/*
 * Override the projection from the data file with this one.
 */
/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/
int vis5d_init_projection( int index, int projection, float *projargs )
{
   DPY_CONTEXT("vis5d_init_projection");
   dtx->UserProjection = projection;
   /* MJK 12.10.98 */
   if (dtx->UserProjArgs) free (dtx->UserProjArgs), dtx->UserProjArgs = NULL;

   if (projargs) {
      dtx->UserProjArgs = malloc( MAXPROJARGS*sizeof(float) );
      memcpy( dtx->UserProjArgs, projargs, MAXPROJARGS*sizeof(float) );
   }
   return 0;
}



/*
 * Override the vertical coordinate system  from the data file with this one.
 */
/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/
int vis5d_init_vertical( int index, int vertical, float *vertargs )
{
   DPY_CONTEXT("vis5d_init_vertical");
   dtx->UserVerticalSystem = vertical;
   /* MJK 12.10.98 */    
   if (dtx->UserVertArgs) free (dtx->UserVertArgs), dtx->UserVertArgs = NULL;

   if (vertargs) {
      dtx->UserVertArgs = malloc( MAXVERTARGS*sizeof(float) );
      memcpy( dtx->UserVertArgs, vertargs, MAXVERTARGS*sizeof(float) );
   }
   return 0;
}


static void load_topo_and_map( Display_Context dtx )
{
   char name[1000];

   /*** Load topography ***/
   /* MJK 4.27.99 
   if (dtx->Path[0]) {
   */
   if (Vis5dDataPath[0]) {
   /* MJK 4.27.99    
      strcpy( name, dtx->Path );
   */      
/* SGJ 7/3/00
      strcpy( name, Vis5dDataPath );
      strcat( name, dtx->TopoName );
*/
      /* SGJ 7/3/00: don't prepend path if TopoName is already absolute: */
      if (dtx->TopoName[0] != '/') {
        strcpy( name, Vis5dDataPath );
        strcat( name, dtx->TopoName );
      }
      else {
        strcpy( name, dtx->TopoName );
      }
   }
   else {
      strcpy( name, dtx->TopoName );
   }
   if (name[0]) {
      dtx->TopoFlag = init_topo( dtx, name, dtx->TextureFlag, dtx->HiResTopo );
   }
   else {
      dtx->TopoFlag = 0;
   }

   /*** Load texture, areas, or image sequence ***/
   init_image(dtx);
   if (dtx->TextureName[0]) {
      /* Load an SGI .rgb image as the texture */
      dtx->TextureFlag = read_texture_image( dtx, dtx->TextureName );
   }
#ifdef MCIDAS
   else if (dtx->FirstArea>0) {
      /* Read a sequence of McIDAS areas as textures */
      dtx->TextureFlag =  read_texture_areas( dtx, dtx->FirstArea ); 
   }
#endif
   else if (dtx->SequenceName[0]) {
      /* Read a raw image sequence as textures */
      dtx->TextureFlag = read_texture_sequence( dtx, dtx->SequenceName ); 
   }
   else {
      dtx->TextureFlag = 0;
   }

   /*** Load map ***/
   if (dtx->MapName[0] == 0) {
      /* depending on domain size, pick appropriate default */
      float latn, lats, lonw, lone;
      latlon_bounds(dtx, &lats, &latn, &lonw, &lone);
      if (30.0 < latn && latn < 80.0 &&
          0.0 < lats && lats < 45.0 &&
          80.0 < lonw && lonw < 180.0 &&
          30.0 < lone && lone < 115.0) {
   /* MJK 4.27.99          
         sprintf( name, "%s%s", dtx->Path, USAFILE);
   */      
         sprintf( name, "%s%s", Vis5dDataPath, USAFILE);
      }
      else {
   /* MJK 4.27.99          
         sprintf( name, "%s%s", dtx->Path, WORLDFILE );
   */      
         sprintf( name, "%s%s", Vis5dDataPath, WORLDFILE );
      }
   }
   else {
      /* concatenate path and user-supplied map file name */
      /* MJK 4.27.99                   
      if (dtx->Path[0]) {
         strcpy( name, dtx->Path );
      */
      if (Vis5dDataPath[0]) {
/* SGJ 7/3/00: don't prepend path if MapName is already absolute
         strcpy( name, Vis5dDataPath );
         strcat( name, dtx->MapName );
*/
        /* SGJ 7/3/00: don't prepend path if MapName is already absolute: */
        if (dtx->MapName[0] != '/') {
          strcpy( name, Vis5dDataPath );
          strcat( name, dtx->MapName );
        }
        else {
          strcpy( name, dtx->MapName );
        }
      }
      else {
         strcpy( name, dtx->MapName );
      }
   }
   if (name[0]) {
      dtx->MapFlag = init_map( dtx, name );
   }
   else {
      dtx->MapFlag = 0;
   }

   /* MJK 12.10.98 */
   /*
   free_topo( dtx );
   */
}


/* WLH 26 Oct 98
int vis5d_initialize_stuff(int index) {
   CONTEXT("vis5d_initialize_stuff")
   initialize_stuff(ctx);
   return 0;
}
*/

/* WLH 26 Oct 98 */
int vis5d_initialize_stuff(int index) {
   Context ctx;
   Irregular_Context itx;
   int chowmany, cwhichones[VIS5D_MAX_CONTEXTS], cyo, ind;
   DPY_CONTEXT("vis5d_initialize_stuff")
   vis5d_get_num_of_ctxs_in_display( index, &chowmany, cwhichones);
   for ( cyo = 0; cyo < chowmany; cyo++) {
     ind = cwhichones[cyo];
     if (ind<0 || ind>=VIS5D_MAX_CONTEXTS || (ctx = ctx_table[ind])==NULL) {
       printf("bad context in vis5d_initialize_stuff\n");
       return VIS5D_BAD_CONTEXT;
     }
     initialize_stuff(ctx);
   }
   vis5d_get_num_of_itxs_in_display( index, &chowmany, cwhichones);
   for ( cyo = 0; cyo < chowmany; cyo++) {
     ind = cwhichones[cyo];
     if (ind<0 || ind>=VIS5D_MAX_CONTEXTS || (itx = itx_table[ind])==NULL) {
       printf("bad context in vis5d_initialize_stuff\n");
       return VIS5D_BAD_CONTEXT;
     }
     initialize_irregular_stuff(itx);
   }
   return 0;
}



/*
 * Initialize graphics-related and other global vars AFTER the data
 * set has been loaded.
 */
static void initialize_stuff( Context ctx )
{
   int var, time, i, j, k, w;


   /* Graphics structs */
   for (var=0;var<MAXVARS;var++) {
      for (time=0;time<MAXTIMES;time++) {
         ctx->SurfTable[var][time].valid = 0;
         ctx->HSliceTable[var][time].valid = 0;
         ctx->VSliceTable[var][time].valid = 0;
         ctx->CHSliceTable[var][time].valid = 0;
         ctx->CVSliceTable[var][time].valid = 0;
      }
   }

   

   /* Initialize surfaces, slices */
   for (var=0;var<ctx->NumVars;var++) {
      init_graphics_pos( ctx, var );
   }
}

static void initialize_irregular_stuff( Irregular_Context itx )
{
   int time;

   for (time = 0; time < MAXTIMES;time++) {
      itx->TextPlotTable[time].valid = 0;
   }
}



/*
 * End of initialization stage.
 */
/* New 5.2 */
int vis5d_init_data_end( int index )
{
#ifdef CAVE
   void *pool;
#endif
   int yo;
   int memsize;
   float ratio;
   /* New 5.2 */
   CONTEXT("vis5d_init_data_end");


/*  | MJK 11.19.98 |  */
   if (off_screen_rendering){
      for (yo = 0; yo < ctx->NumVars; yo++){
         init_var_clrtable( ctx->dpy_ctx->dpy_context_index, index, yo);
      }
   }


   /*** Memory setup ***/
   if (ctx->MegaBytes==0) {
#ifdef CAVE
      abort();
#endif
      /* use malloc/free */
      if (!init_memory( ctx, 0 )) {
         return VIS5D_FAIL;
      }
      memsize = 0;
   }
   else {
      /* Use bounded memory management */
      if (ctx->MegaBytes<10) {
         ctx->MegaBytes = 10;
      }
      /* use 80% of MegaBytes */
      memsize = (int) ( (float) ctx->MegaBytes * 0.80 ) * MEGA;
#ifdef CAVE
      pool = amalloc(memsize,cave_shmem);
      if (!pool) {
         printf("Error: couldn't allocate %d bytes from shared CAVE pool\n",
                 memsize );
         return VIS5D_OUT_OF_MEMORY;
      }
      if (!init_shared_memory( ctx, pool, memsize )) {
         return VIS5D_FAIL;
      }
#else
      if (!init_memory( ctx, memsize )) {
         return VIS5D_FAIL;
      }
#endif
   }
   /* Let 2/5 of the memory pool be used for caching grid data. */
   if (memsize==0) {
      /* Grid cache size = 100MB */
      if (!init_grid_cache( ctx, 100*1024*1024, &ratio )) {
         return VIS5D_OUT_OF_MEMORY;
      }
   }
   else {
      if (!init_grid_cache( ctx, memsize * 2 / 5, &ratio )) {
         return VIS5D_OUT_OF_MEMORY;
      }
   }
   /* Read some or all of the grid data into main memory now.  If we */
   /* have enough memory, the whole file will be loaded.  Otherwise, */
   /* an arbitrary set of grids will be loaded. */
   if (ctx->PreloadCache) {
      preload_cache(ctx);
   }
   if (memsize!=0) {
      /* check if there's enough memory left after loading the data set */
      int min_mem = MAX( memsize/3, 3*MEGA );
      if (mem_available(ctx)<min_mem) {
         printf("Not enough memory left for graphics (only %d bytes free)\n",
                mem_available(ctx));
         return VIS5D_OUT_OF_MEMORY;
      }
   }
   ctx->VeryLarge = (ratio < VERY_LARGE_RATIO);
   if (ctx->VeryLarge) printf("Using VeryLarge option - graphics may be slow\n");

   if (ctx->dpy_ctx->numofctxs == 1){
      init_anim(ctx->dpy_ctx);

      /*** setup map proj and vert coord system, make 3-D box */
      if (!setup_ctx_dtx_projection( ctx )) {
         return VIS5D_FAIL;
      }
      if (!setup_ctx_dtx_vertical_system( ctx )) {
         return VIS5D_FAIL;
      }
      init_trajPRIME(ctx->dpy_ctx);
      make_box( ctx->dpy_ctx, ctx->dpy_ctx->Ax, ctx->dpy_ctx->Ay, ctx->dpy_ctx->Az );
      if (!in_the_init_stage){
         load_topo_and_map( ctx->dpy_ctx );
      }
      compute_wind_levels(ctx->dpy_ctx);
   }
   else{
      if (!setup_ctx_projection( ctx )) {
         return VIS5D_FAIL;
      }
      if (!setup_ctx_vertical_system( ctx )) {
         return VIS5D_FAIL;
      }
   }

   /* front */
   vis5d_set_hclip( ctx->dpy_ctx->dpy_context_index, 0,
                    ctx->dpy_ctx->MaxNl-1);
   /* back */    
   vis5d_set_hclip( ctx->dpy_ctx->dpy_context_index, 1,
                    0);
   /* top */    
   vis5d_set_vclip( ctx->dpy_ctx->dpy_context_index, 0,
                    0, 0, 0, ctx->dpy_ctx->Nc-1);
   /* bottom */    
   vis5d_set_vclip( ctx->dpy_ctx->dpy_context_index, 1,
                    ctx->dpy_ctx->Nr-1, 0, ctx->dpy_ctx->Nr-1, ctx->dpy_ctx->Nc-1);
   /* left */    
   vis5d_set_vclip( ctx->dpy_ctx->dpy_context_index, 2,
                    0, 0, ctx->dpy_ctx->Nr-1, 0);
   /* right */    
   vis5d_set_vclip( ctx->dpy_ctx->dpy_context_index, 3,
                    0, ctx->dpy_ctx->Nc-1, ctx->dpy_ctx->Nr-1, ctx->dpy_ctx->Nc-1);


   /*** Miscellaneous ***/
   if (ctx->dpy_ctx->FontName[0]) { 
      /* use non-default font */
      set_3d_font(  ctx->dpy_ctx, ctx->dpy_ctx->FontName, ctx->dpy_ctx->FontHeight );
   }
 
   for (yo=0; yo < ctx->NumVars; yo++){
      ctx->RealMinVal[yo] = ctx->MinVal[yo];
      ctx->RealMaxVal[yo] = ctx->MaxVal[yo];
   } 
   initialize_stuff(ctx);
   if (!init_traj(ctx)) {
      return VIS5D_FAIL;
   }
  
   if (ctx->dpy_ctx->CurvedBox) {
      /* Can't do volume rendering with curved box */
      ctx->Volume = NULL;
   }
   else {
      ctx->Volume = alloc_volume( ctx, ctx->dpy_ctx->Nr, ctx->dpy_ctx->Nc,
                                  ctx->dpy_ctx->MaxNl );
   }

   /*** Create threads ***/
#ifdef sgi
   if (NumThreads>1 && WorkerPID[0]==0) {
      /* Fork off the worker threads if we haven't already */
      if (NumThreads>1)   WorkerPID[0] = sproc( work, PR_SALL, 1 );
      if (NumThreads>2)   WorkerPID[1] = sproc( work, PR_SALL, 2 );
      if (NumThreads>3)   WorkerPID[2] = sproc( work, PR_SALL, 3 );
      if (NumThreads>4)   WorkerPID[3] = sproc( work, PR_SALL, 4 );
      if (NumThreads>5)   WorkerPID[4] = sproc( work, PR_SALL, 5 );
      if (NumThreads>6)   WorkerPID[5] = sproc( work, PR_SALL, 6 );
      if (NumThreads>7)   WorkerPID[6] = sproc( work, PR_SALL, 7 );
      if (NumThreads>8)   WorkerPID[7] = sproc( work, PR_SALL, 8 );
   }
#endif
#ifdef sunos5
   if (Threadsers>1 && WorkerPID[0]==0) {
      if (NumThreads>1)   thr_create( NULL, 0, work, 1, 0, &WorkerPID[0] );
      if (NumThreads>2)   thr_create( NULL, 0, work, 2, 0, &WorkerPID[1] );
      if (NumThreads>3)   thr_create( NULL, 0, work, 3, 0, &WorkerPID[2] );
      if (NumThreads>4)   thr_create( NULL, 0, work, 4, 0, &WorkerPID[3] );
   }
#endif

#ifdef LTHREADS 
   if (Threadsers>1 && WorkerPID[0]==0) {
      if (NumThreads>1)   pthread_create( &WorkerPID[0], NULL, work, 1 );
      if (NumThreads>2)   pthread_create( &WorkerPID[1], NULL, work, 2 );
      if (NumThreads>3)   pthread_create( &WorkerPID[2], NULL, work, 3 );
      if (NumThreads>4)   pthread_create( &WorkerPID[3], NULL, work, 4 );
   }
#endif

   ctx->InsideInit = 0;

   return 0;
}

int vis5d_get_v5dfilename( int index, char *name )
{
   CONTEXT("vis5d_get_v5dfilename")
   strcpy(name, ctx->DataFile);
   return 0;
}


/*
 * Open the named v5d file, read its header info, and then close the file.
 * Does not consume a vis5d context in doing so.
 */
int vis5d_query_gridfile( char *name, v5dstruct *v )
{
   if (query_gridfile( name, v )) {
      return 0;
   }
   else {
      return VIS5D_FAIL;
   }
}


/*
 * Open the named v5d file and read its header.  This should be called
 * before v5d_init_end().  If it's called after vis5d_init_end() the
 * current dataset will be replace by the named one.
 */
int vis5d_open_gridfile( int index, char *name, int read_flag )
{
   float ratio;
   int yo, dindex, dnumber, na[VIS5D_MAX_CONTEXTS];
   Display_Context dtx;
   CONTEXT("vis5d_open_gridfile");

   ctx->PreloadCache = read_flag;
   if (ctx->DataFile[0]) {
      dindex = ctx->dpy_ctx->dpy_context_index;
      dtx = vis5d_get_dtx( dindex );
      /* already have a dataset loaded, replace it with the new one */
      v5dCloseFile( &ctx->G );
      free_all_graphics( ctx );
      init_context( ctx );
      strcpy( ctx->DataFile, name );
      strcpy( ctx->ContextName, name);
      if (open_gridfile( ctx, name )) {
         vis5d_get_num_of_ctxs_in_display( dindex, &dnumber, na);
         if (ctx->context_index == na[0]){
            /* New 5.2 */
            vis5d_init_display_values ( index, -1, dindex);
            if (!setup_ctx_dtx_projection( ctx )) {
               return VIS5D_FAIL;
            }
            if (!setup_ctx_dtx_vertical_system( ctx )) {
               return VIS5D_FAIL;
            }
            /* New 5.2 */            
            vis5d_init_display_values ( index, -1, dindex);

            init_anim(ctx->dpy_ctx);
         }
         else{
            vis5d_assign_display_to_data( index, dindex);
         }

            
         if (ctx->memory_limit==0) {
            /* Grid cache size = 100MB */
            if (!init_grid_cache( ctx, 100*1024*1024, &ratio )) {
               return VIS5D_FAIL;
            }
         }
         else {
            if (!init_grid_cache( ctx, ctx->memory_limit * 2 / 5, &ratio )) {
               return VIS5D_FAIL;
            }
         }
         if (ctx->PreloadCache) {
            preload_cache(ctx);
         }
         if (ctx->memory_limit != 0) {
            /* check if there's enough memory left after loading the data set */
            int min_mem = MAX( ctx->memory_limit/3, 3*MEGA );
            if (mem_available(ctx)<min_mem) {
               printf("Not enough memory left for graphics (only %d bytes free)\n",
                      mem_available(ctx));
               return VIS5D_FAIL;
            }
         }
         ctx->VeryLarge = (ratio < VERY_LARGE_RATIO);
         if (ctx->VeryLarge) printf("Using VeryLarge option - graphics may be slow\n");

         if (ctx->context_index == na[0]){
            init_anim(ctx->dpy_ctx);

            /*** setup map proj and vert coord system */
            if (!setup_ctx_dtx_projection( ctx )) {
               return VIS5D_FAIL;
            }
            if (!setup_ctx_dtx_vertical_system( ctx )) {
               return VIS5D_FAIL;
            }
            init_trajPRIME(ctx->dpy_ctx);
            make_box( ctx->dpy_ctx, ctx->dpy_ctx->Ax, ctx->dpy_ctx->Ay, ctx->dpy_ctx->Az );
            load_topo_and_map( ctx->dpy_ctx );
            compute_wind_levels(ctx->dpy_ctx);
            for (yo = 1; yo < dtx->numofctxs; yo++){
               Context xtc;
               xtc = dtx->ctxpointerarray[yo];
               xtc->GridSameAsGridPRIME = vis5d_check_dtx_same_as_ctx(
                        dtx->dpy_context_index, xtc->context_index); 
               if (!dtx->CurvedBox) {
                  if (xtc->Volume){
                     free_volume( xtc );
                  }
                  xtc->Volume = alloc_volume( xtc, dtx->Nr, dtx->Nc, dtx->MaxNl);
               }
               else{
                  if (xtc->Volume){
                     free_volume( xtc);
                  }
                  xtc->Volume = NULL;
               }
            }
         }      
         else{
            if (!setup_ctx_projection( ctx )) {
               return VIS5D_FAIL;
            }
            if (!setup_ctx_vertical_system( ctx )) {
               return VIS5D_FAIL;
            }
         }
         /* front */
         vis5d_set_hclip( ctx->dpy_ctx->dpy_context_index, 0,
                          ctx->dpy_ctx->MaxNl-1);
         /* back */
         vis5d_set_hclip( ctx->dpy_ctx->dpy_context_index, 1,
                          0);
         /* top */
         vis5d_set_vclip( ctx->dpy_ctx->dpy_context_index, 0,
                          0, 0, 0, ctx->dpy_ctx->Nc-1);
         /* bottom */
         vis5d_set_vclip( ctx->dpy_ctx->dpy_context_index, 1,
                          ctx->dpy_ctx->Nr-1, 0, ctx->dpy_ctx->Nr-1, ctx->dpy_ctx->Nc-1);
         /* left */
         vis5d_set_vclip( ctx->dpy_ctx->dpy_context_index, 2,
                          0, 0, ctx->dpy_ctx->Nr-1, 0);
         /* right */
         vis5d_set_vclip( ctx->dpy_ctx->dpy_context_index, 3,
                          0, ctx->dpy_ctx->Nc-1, ctx->dpy_ctx->Nr-1, ctx->dpy_ctx->Nc-1);


         /*** Miscellaneous ***/
         if (ctx->dpy_ctx->FontName[0]) {
            /* use non-default font */
/*            set_3d_font(  ctx->dpy_ctx, ctx->dpy_ctx->FontName ); */
         }

         for (yo=0; yo < ctx->NumVars; yo++){
            ctx->RealMinVal[yo] = ctx->MinVal[yo];
            ctx->RealMaxVal[yo] = ctx->MaxVal[yo];
         }
         initialize_stuff(ctx);
         if (!init_traj(ctx)) {
            return VIS5D_FAIL;
         }
         if (ctx->dpy_ctx->CurvedBox) {
            /* Can't do volume rendering with curved box */
            ctx->Volume = NULL;
         }
         else { 
            ctx->Volume = alloc_volume( ctx, ctx->dpy_ctx->Nr,
                                     ctx->dpy_ctx->Nc, ctx->dpy_ctx->MaxNl );
         }
      }
      else {
         return VIS5D_FAIL;
      }
   }
   else {
      strcpy( ctx->DataFile, name );
      strcpy( ctx->ContextName, name);
      if (open_gridfile( ctx, name )) {
         return 0;
      }
      else {
         return VIS5D_FAIL;
      }
   }
   return 0;
}

/*** Level Function ***/

int vis5d_get_levels( int index, int var)
{
   CONTEXT("vis5d_get_ctx_levels")
   return ctx->Nl[var];
}  



/*** Time Functions ***/

int vis5d_get_ctx_numtimes( int index, int *numtimes )
{
   CONTEXT("vis5d_get_ctx_numtimes")
    *numtimes = ctx->NumTimes;
    return 0;
}

/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/
int vis5d_get_dtx_numtimes( int index, int *numtimes )
{
   DPY_CONTEXT("vis5d_get_dtx_numtimes")
   *numtimes = dtx->NumTimes;
   return 0;
}

/****************************************/
/******** index => GROUP CONTEXT ********/
/****************************************/
int vis5d_get_grp_numtimes( int index, int *numtimes )
{
   Display_Group grp;
  
   grp = vis5d_get_grp( index );
   *numtimes = grp->NumTimes;
   return 0;
}

/****************************************/
/******** index => DATA CONTEXT *********/
/****************************************/
int vis5d_get_ctx_time_stamp( int index, int timestep, int *day, int *time )
{
  CONTEXT("vis5d_get_ctx_time_stamp")
  if (timestep<0 || timestep>=ctx->NumTimes) {
     return VIS5D_BAD_TIME_STEP;
  }
  else {
     *day = ctx->DayStamp[timestep];
     *time = ctx->TimeStamp[timestep];
     return 0;
  }
}

/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/
int vis5d_get_dtx_time_stamp( int index, int timestep, int *day, int *time)
{
   int dtimeold, stimeold;
   int dtime, stime;
   int yo, spandex;
   DPY_CONTEXT("vis5d_get_dtx_time_stamp")

   if (timestep<0 || timestep>=dtx->NumTimes) {
      return VIS5D_BAD_TIME_STEP;
   }   
   else {
/* MJK 9.22.99 
      if (dtx->numofctxs == 1){
         vis5d_get_ctx_time_stamp(dtx->ctxarray[0], timestep, day, time);
      }
      else{
*/
      *day = dtx->DayStamp[timestep]; 
      *time= dtx->TimeStamp[timestep];
      return 0;
   }
}
   

/****************************************/
/******** index => DATA CONTEXT *********/
/****************************************/
int vis5d_set_ctx_time_stamp( int index, int timestep, int day, int time )
{
  CONTEXT("vis5d_set_ctx_time_stamp")
  if (timestep<0 || timestep>=ctx->NumTimes) {
     return VIS5D_BAD_TIME_STEP;
  }
  else {
     ctx->DayStamp[timestep] = day;
     ctx->TimeStamp[timestep] = time;
     return 0;
  }
}


/****************************************/
/******** index => GROUP CONTEXT ********/
/****************************************/
int vis5d_signal_group_redraw( int index, int time)
{
   Display_Group grp;
   Display_Context dtx;
   int yo, spandex;

   grp = vis5d_get_grp(index);
   for (yo=0; yo < grp->numofdpys; yo++){
      spandex = grp->dpyarray[yo]->dpy_context_index;
      vis5d_signal_redraw(spandex, time);
   }
   return 0;
}   


/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/

int vis5d_set_probe_on_traj( int index, int time)
{
   int endpoint;
   int i, first_set, first_traj_of_set;
   struct traj *t;
   DPY_CONTEXT("vis5d_set_probe_on_traj")


   if (!dtx->DisplayProbe || !dtx->DisplayProbeOnTraj){
      return 0;
   }

   /*** get first set/group that's on ***/
   first_set = -1;
   for (i = 0; i < VIS5D_TRAJ_SETS; i++){
      if (dtx->DisplayTraj[i]){
         first_set = i;
         i = VIS5D_TRAJ_SETS;
      }
   }
   if (first_set == -1){
      return 0;
   }

   /*** get first traj in the first set ***/
   first_traj_of_set = -1;
   for (i = 0; i < dtx->NumTraj; i++){
      t = dtx->TrajTable[i];
      if ( t->group == first_set){
         first_traj_of_set = i;
         i = dtx->NumTraj;
      }
   }
   if (first_traj_of_set == -1){
      return 0;
   }

   if (t->len[time] >= 1 ){
      endpoint = t->start[time]+ t->len[time]-1;
      dtx->CursorX = (float)(t->verts[endpoint*3+0]) / VERTEX_SCALE;
      dtx->CursorY = (float)(t->verts[endpoint*3+1]) / VERTEX_SCALE;
      dtx->CursorZ = (float)(t->verts[endpoint*3+2]) / VERTEX_SCALE;
   }
   return 1;
}

/****************************************/
/******** index => DATA CONTEXT *********/
/****************************************/

/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/
int vis5d_set_dtx_timestep( int index, int time )
{
   int yo, spandex;
   int flo, dindex;
   Context ctx;
   Irregular_Context itx;
   DPY_CONTEXT("vis5d_set_dtx_timestep")

   dtx->CurTime = time;
   vis5d_set_probe_on_traj( index, dtx->CurTime);
   for (yo=0; yo < dtx->numofctxs+dtx->numofitxs; yo++){
      if (dtx->TimeStep[time].ownertype[yo] == REGULAR_TYPE){
         spandex = dtx->TimeStep[time].owners[yo];
         ctx = vis5d_get_ctx( spandex);
         ctx->CurTime = dtx->TimeStep[time].ownerstimestep[yo];
      }
      else if (dtx->TimeStep[time].ownertype[yo] == IRREGULAR_TYPE){
         spandex = dtx->TimeStep[time].owners[yo];
         itx = vis5d_get_itx( spandex);
         itx->CurTime = dtx->TimeStep[time].ownerstimestep[yo];
      }
      else{
         printf("Big Error in vis5d_set_dtx_timestep\n");
      }
   }
   dtx->Redraw = 1;
   return 0;
}

/****************************************/
/******** index => GROUP CONTEXT ********/
/****************************************/
int vis5d_set_grp_timestep( int index, int time)
{
   Display_Group grp;
   int yo, spandex, abc;
   Display_Context dtx;

   grp = vis5d_get_grp(index);
   grp->CurTime = time;
   for (yo=0; yo < grp->numofdpys; yo++){
      spandex = grp->TimeStep[time].owners[yo];
      dtx = vis5d_get_dtx( spandex);
      abc = grp->TimeStep[time].ownerstimestep[yo];
      vis5d_set_dtx_timestep(spandex, abc);
   }
   return 0;
}

/****************************************/
/******** index => DATA CONTEXT *********/
/****************************************/
int vis5d_get_ctx_timestep( int index, int *curtime )
{
   CONTEXT("vis5d_get_ctx_timestep")
   *curtime = ctx->CurTime;
   return 0;
}

int vis5d_get_itx_timestep( int index, int *curtime )
{
   IRG_CONTEXT("vis5d_get_itx_timestep")
   *curtime = itx->CurTime;
   return 0;
}


/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/
int vis5d_get_dtx_timestep( int index, int *curtime )
{
   DPY_CONTEXT("vis5d_get_dtx_timestep")
   *curtime = dtx->CurTime;
   return 0;
}

/****************************************/
/******** index => GROUP CONTEXT ********/
/****************************************/
int vis5d_get_grp_timestep( int index, int *curtime )
{
   Display_Group grp;

   grp = vis5d_get_grp(index);
   *curtime = grp->CurTime;
   return 0;
}


/*** Clipping Functions ***/

/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/
int vis5d_set_hclip( int index, int num, float level)
{
   float maxlev, lowlev;
   DPY_CONTEXT("vis5d_set_hclip")
   
   maxlev = (float) (dtx->LowLev + dtx->Nl - 1);
   lowlev = (float) (dtx->LowLev);

   if (level < lowlev) {
      level = lowlev;
   }
   else if (level > maxlev) {
      level = maxlev;
   }

   dtx->HClipTable[num].level = level;
   dtx->HClipTable[num].eqn[0] = 0;
   dtx->HClipTable[num].eqn[1] = 0;
   dtx->HClipTable[num].eqn[2] = 1;
   dtx->HClipTable[num].eqn[3] = gridlevelPRIME_to_zPRIME(dtx, -1, -1, level);

   if (dtx->ctxpointerarray[0]) {
     request_hclip(dtx->ctxpointerarray[0], num);
   }
   return 0;
}

/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/
int vis5d_get_hclip( int index, int num, float *level)
{
   DPY_CONTEXT("vis5d_get_hclip")

   *level = dtx->HClipTable[num].level;
   return 0;
}

/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/
int vis5d_set_vclip( int index, int num, float r1, float c1,
                     float r2, float c2)
{
   float x1,y1,z1,x2,y2,z2,x3,y3,z3;
   float A[3], B[3], C[3], D, zero;
   DPY_CONTEXT("vis5d_set_vclip")
   
   dtx->VClipTable[num].r1 = r1;
   dtx->VClipTable[num].c1 = c1;
   dtx->VClipTable[num].r2 = r2;
   dtx->VClipTable[num].c2 = c2;

   zero = 0.0;
   gridPRIME_to_xyzPRIME( dtx, -1, -1, 1, &r1, &c1, &zero, &x1, &y1, &z1);
   gridPRIME_to_xyzPRIME( dtx, -1, -1, 1, &r2, &c2, &zero, &x2, &y2, &z2);
   x3 = x2;
   y3 = y2;
   z3 = z2 + 1.0;

   /* get the eqn for the plane */
   /* get the two vectors */
   A[0] = x1-x2;
   A[1] = y1-y2;
   A[2] = z1-z2;
   B[0] = x3-x2;
   B[1] = y3-y2;
   B[2] = z3-z2;
 
   /*get the cross product*/
   C[0] = A[1]*B[2] - A[2]*B[1];
   C[1] = A[2]*B[0] - A[0]*B[2];
   C[2] = A[0]*B[1] - A[1]*B[0];
 
   /*get D*/
   D = -1 * (C[0]*x1 + C[1]*y1 + C[2]*z1 );
 
   dtx->VClipTable[num].eqn[0] = C[0];
   dtx->VClipTable[num].eqn[1] = C[1];
   dtx->VClipTable[num].eqn[2] = C[2];
   dtx->VClipTable[num].eqn[3] = D;
 
   if (dtx->ctxpointerarray[0]) {
     request_vclip(dtx->ctxpointerarray[0], num);
   }
   return 0;
}

/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/
int vis5d_get_vclip( int index, int num, float *r1, float *c1,
                     float *r2, float *c2)
{
   DPY_CONTEXT("vis5d_get_vclip")

   *r1 = dtx->VClipTable[num].r1;
   *c1 = dtx->VClipTable[num].c1;
   *r2 = dtx->VClipTable[num].r2;
   *c2 = dtx->VClipTable[num].c2;
   return 0;
}

int vis5d_set_clip_mode( int index, int clip, int mode )
{
   DPY_CONTEXT("vis5d_set_current_clip")
   if (clip==0 || clip==1){
      dtx->HClipTable[clip].highlight = mode;
   }
   else{
      dtx->VClipTable[clip-2].highlight = mode;
   }
   return 0;
}

int vis5d_get_clip_mode( int index, int clip , int *mode)
{
   DPY_CONTEXT("vis5d_get_current_clip")

   if (clip == 0 || clip == 1){
      *mode = dtx->HClipTable[clip].highlight;
   }
   else{
      *mode = dtx->VClipTable[clip-2].highlight;
   }
   return 0;
} 
     


      
   
/*** Variable Functions ***/

/****************************************/
/******** index => DATA CONTEXT *********/
/****************************************/
int vis5d_get_ctx_numvars( int index, int *numvars )
{
   CONTEXT("vis5d_get_ctx_numvars");
   if (ctx){
      *numvars = ctx->NumVars;
   }
   else{
      *numvars = 0;
   }
   return 0;
}

/*
 * Find the number of the named variable.
 */
/****************************************/
/******** index => DATA CONTEXT *********/
/****************************************/
int vis5d_find_var( int index, char *name )
{
   int i;
   CONTEXT("vis5d_find_var");

   for (i=0;i<ctx->NumVars;i++) {
      if (strcmp(ctx->VarName[i],name)==0) {
         return i;
      }
   }
   return VIS5D_FAIL;
}


/****************************************/
/******** index => DATA CONTEXT *********/
/****************************************/
int vis5d_get_ctx_var_name( int index, int var, char *name )
{
   CONTEXT("vis5d_get_ctx_var_name");

   if (var>=0 && var<ctx->NumVars) {
      strcpy( name, ctx->VarName[var] );
      return 0;
   }
   else {
      return VIS5D_BAD_VAR_NUMBER;
   }
}

/****************************************/
/******** index => DATA CONTEXT *********/
/****************************************/
int vis5d_get_var_units( int index, int var, char *units )
{
   CONTEXT("vis5d_get_var_units");
   if (var>=0 && var<ctx->NumVars) {
      strcpy( units, ctx->Units[var] );
      return 0;
   }
   else {
      return VIS5D_BAD_VAR_NUMBER;
   }
}


/*
 * Return the type of a variable.
 * Input:  index - the context index
 *         var - variable number
 * Output:  type - one of:
 *                     VIS5D_REGULAR - a regular variable
 *                     VIS5D_CLONE - clone of another variable
 *                     VIS5D_EXT_FUNC - computed with external Fortran function
 *                     VIS5D_EXPRESSION - computed with a simple expression
 */
/****************************************/
/******** index => DATA CONTEXT *********/
/****************************************/
int vis5d_get_var_type( int index, int var, int *type )
{
   CONTEXT("vis5d_get_ctx_var_name");
   if (var>=0 && var<ctx->NumVars) {
      *type = ctx->VarType[var];
      return 0;
   }
   else {
      return VIS5D_BAD_VAR_NUMBER;
   }
}



/****************************************/
/******** index => DATA CONTEXT *********/
/****************************************/
int vis5d_get_var_info( int index, int var, void *info )
{
   CONTEXT("vis5d_get_var_info");
   if (var>=0 && var<ctx->NumVars) {
      if (ctx->VarType[var]==VIS5D_CLONE) {
         int *cloneof = (int *) info;
         *cloneof = ctx->CloneTable[var];
      }
      else if (ctx->VarType[var]==VIS5D_EXT_FUNC) {
         char *funcname = (char *) info;
         strcpy( funcname, ctx->VarName[var] );
      }
      else if (ctx->VarType[var]==VIS5D_EXPRESSION) {
         char *funcname = (char *) info;
         strcpy( funcname, ctx->ExpressionList[var] );
      }
      return 0;
   }
   else {
      return VIS5D_BAD_VAR_NUMBER;
   }
}


/****************************************/
/******** index => DATA CONTEXT *********/
/****************************************/
int vis5d_get_ctx_var_range( int index, int var, float *min, float *max )
{
  CONTEXT("vis5d_get_ctx_var_range")
   if (var>=0 && var<ctx->NumVars) {
      *min = ctx->MinVal[var];
      *max = ctx->MaxVal[var];
      return 0;
   }
   else {
      return VIS5D_BAD_VAR_NUMBER;
   }
}


/****************************************/
/******** index => DATA CONTEXT *********/
/****************************************/
int vis5d_set_var_range( int index, int var, float min, float max )
{
   int time;
   CONTEXT("vis5d_set_var_range")
   if (var>=0 && var<ctx->NumVars) {

      /* MJK 12.10.98 */
      if (min != ctx->MinVal[var]){

         ctx->MinVal[var] = min;
         for (time=0;time<MAXTIMES;time++) {
            ctx->SurfTable[var][time].valid = 0;
            ctx->HSliceTable[var][time].valid = 0;
            ctx->VSliceTable[var][time].valid = 0;
            ctx->CHSliceTable[var][time].valid = 0;
            ctx->CVSliceTable[var][time].valid = 0;
            ctx->dpy_ctx->Redraw = 1;
         }
      }

      /* MJK 12.10.98 */
      if (max != ctx->MaxVal[var]){

         ctx->MaxVal[var] = max;
         for (time=0;time<MAXTIMES;time++) {
            ctx->SurfTable[var][time].valid = 0;
            ctx->HSliceTable[var][time].valid = 0;
            ctx->VSliceTable[var][time].valid = 0;
            ctx->CHSliceTable[var][time].valid = 0;
            ctx->CVSliceTable[var][time].valid = 0;
            ctx->dpy_ctx->Redraw = 1;
         }
      }
      return 0;
   }
   else {
      return VIS5D_BAD_VAR_NUMBER;
   }
}


/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/
int vis5d_set_sound_vars( int index, int tempowner, int temp, int dewptowner, int dewpt,
                          int uwindowner,  int uwind, int vwindowner, int vwind,
                          int var1owner, int var1, int var2owner, int var2, 
                          int var3owner, int var3 )
{
   Context ctx;
   DPY_CONTEXT("vis5d_set_sound_vars")
   
   dtx->Sound.SoundTemp = temp ;
   dtx->Sound.SoundDewpt= dewpt;
   dtx->Sound.SoundUWind = uwind ;
   dtx->Sound.SoundVWind = vwind ;
   dtx->Sound.SoundVar1 = var1;
   dtx->Sound.SoundVar2 = var2;
   dtx->Sound.SoundVar3 = var3;

   if (!is_valid_dtx_ctx(index, tempowner)){
      dtx->Sound.SoundTemp = -1;
      dtx->Sound.SoundTempOwner = NULL; 
   }
   else{
      dtx->Sound.SoundTempOwner = vis5d_get_ctx(tempowner) ;
      ctx = vis5d_get_ctx(tempowner);
      if ( (temp >= 0 ) && (ctx->Nl[temp] < 2)){
         dtx->Sound.SoundTemp = -1;
      }
   }
   if (!is_valid_dtx_ctx(index, dewptowner)){
      dtx->Sound.SoundDewpt = -1;
      dtx->Sound.SoundDewptOwner= NULL;
   }
   else{
      dtx->Sound.SoundDewptOwner= vis5d_get_ctx(dewptowner);
      ctx = vis5d_get_ctx(dewptowner);
      if ( (dewpt >= 0 ) && (ctx->Nl[dewpt] < 2)){
         dtx->Sound.SoundDewpt = -1;
      }
   }
   if (!is_valid_dtx_ctx(index, uwindowner)){
      dtx->Sound.SoundUWind = -1;
      dtx->Sound.SoundUWindOwner = NULL; 
   }
   else{
      dtx->Sound.SoundUWindOwner = vis5d_get_ctx(uwindowner) ;
      ctx = vis5d_get_ctx(uwindowner);
      if ( (uwind >= 0) && (ctx->Nl[uwind] < 2)){
         dtx->Sound.SoundUWind = -1;
      }
   }
   if (!is_valid_dtx_ctx(index, vwindowner)){
      dtx->Sound.SoundVWind = -1;
      dtx->Sound.SoundVWindOwner = NULL; 
   }
   else{
      dtx->Sound.SoundVWindOwner = vis5d_get_ctx(vwindowner) ;
      ctx = vis5d_get_ctx(vwindowner);
      if ( (vwind >= 0) && (ctx->Nl[vwind] < 2)){
         dtx->Sound.SoundVWind = -1;
      }
   }
   if (dtx->Sound.SoundVWindOwner != dtx->Sound.SoundUWindOwner){
      dtx->Sound.SoundUWind = -1;
      dtx->Sound.SoundUWind = -1;
   }

   if (!is_valid_dtx_ctx(index, var1owner)){
      dtx->Sound.SoundVar1 = -1;
      dtx->Sound.SoundVar1Owner =  NULL;
   }
   else{
      dtx->Sound.SoundVar1Owner = vis5d_get_ctx(var1owner);
      ctx = vis5d_get_ctx(var1owner);
      if ( (var1 >= 0) && (ctx->Nl[var1] < 2)){
         dtx->Sound.SoundVar1 = -1;
      }
   }
   if(!is_valid_dtx_ctx(index, var2owner)){
      dtx->Sound.SoundVar2 = -1;
      dtx->Sound.SoundVar2Owner = NULL; 
   }
   else{
      dtx->Sound.SoundVar2Owner = vis5d_get_ctx(var2owner);
      ctx = vis5d_get_ctx(var2owner);
      if ( (var2 >= 0) && (ctx->Nl[var2] < 2)){
         dtx->Sound.SoundVar2 = -1;
      }
   }
   if (!is_valid_dtx_ctx(index, var3owner)){
      dtx->Sound.SoundVar3 = -1;
      dtx->Sound.SoundVar3Owner =  NULL;
   }
   else{
      dtx->Sound.SoundVar3Owner = vis5d_get_ctx(var3owner);
      ctx = vis5d_get_ctx(var3owner);
      if ( (var3 >= 0) && (ctx->Nl[var3] < 2)){
         dtx->Sound.SoundVar3 = -1;
      }
   }
   return 0;
}

/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/
int vis5d_get_sound_vars( int index, int *tempowner, int *temp, int *dewptowner, int *dewpt,
                          int *uwindowner,  int *uwind, int *vwindowner, int *vwind,
                          int *var1owner, int *var1, int *var2owner, 
                          int *var2, int *var3owner, int *var3 )
{
   DPY_CONTEXT("vis5d_get_sound_vars")
  
   *temp = dtx->Sound.SoundTemp;
   *dewpt= dtx->Sound.SoundDewpt;
   *uwind = dtx->Sound.SoundUWind;
   *vwind = dtx->Sound.SoundVWind;
   *var1 = dtx->Sound.SoundVar1;
   *var2 = dtx->Sound.SoundVar2;
   *var3 = dtx->Sound.SoundVar3;
   if (*temp > -1 && dtx->Sound.SoundTempOwner){
      *tempowner = dtx->Sound.SoundTempOwner->context_index;
   }
   else{
      *tempowner = -1;
   }
   if( *dewpt>-1 && dtx->Sound.SoundDewptOwner){
      *dewptowner= dtx->Sound.SoundDewptOwner->context_index;
   }
   else{
      *dewptowner = -1;
   }
   if( *uwind>-1 && dtx->Sound.SoundUWindOwner){
      *uwindowner = dtx->Sound.SoundUWindOwner->context_index;
   }
   else{
      *uwindowner = -1;
   }
   if( *vwind>-1 && dtx->Sound.SoundVWindOwner){
      *vwindowner = dtx->Sound.SoundVWindOwner->context_index;
   }
   else{
      *vwindowner = -1;
   }
   if( *var1>-1 && dtx->Sound.SoundVar1Owner){
      *var1owner = dtx->Sound.SoundVar1Owner->context_index;
   }
   else{
      *var1owner = -1;
   }
   if( *var2>-1 && dtx->Sound.SoundVar2Owner){
      *var2owner = dtx->Sound.SoundVar2Owner->context_index;
   }
   else{
      *var2owner = -1;
   }
   if( *var3>-1 && dtx->Sound.SoundVar3Owner){
      *var3owner = dtx->Sound.SoundVar3Owner->context_index;
   }
   else{
      *var3owner = -1;
   }

   return 0;
}

/****************************************/
/******* index => DISPLAY CONTEXT *******/
/****************************************/
int vis5d_set_wind_vars( int index,
                         int u1varowner, int u1, int v1varowner, int v1,
                         int w1varowner, int w1, int u2varowner, int u2,
                         int v2varowner, int v2, int w2varowner, int w2,
                         int utrajowner, int utraj, int vtrajowner, int vtraj,
                         int wtrajowner, int wtraj )
{
   int time, ws;
   Context ctx;
   DPY_CONTEXT("vis5d_set_wind_vars")

   if (is_valid_dtx_ctx(index, u1varowner)){
      dtx->Uvarowner[0] = u1varowner;
      dtx->Uvar[0] = u1;
   }
   else{
      dtx->Uvarowner[0] = -1;
      dtx->Uvar[0] = -1;
   }

   if (is_valid_dtx_ctx(index, v1varowner)){
      dtx->Vvarowner[0] = v1varowner;
      dtx->Vvar[0] = v1;
   }
   else{
      dtx->Vvarowner[0] = -1;
      dtx->Vvar[0] = -1;
   }

   if (is_valid_dtx_ctx(index, w1varowner)){
      dtx->Wvarowner[0] = w1varowner;
      dtx->Wvar[0] = w1;
   }
   else{
      dtx->Wvarowner[0] = -1;
      dtx->Wvar[0] = -1;
   }
   if (is_valid_dtx_ctx(index, u2varowner)){
      dtx->Uvarowner[1] = u2varowner;
      dtx->Uvar[1] = u2; 
   }
   else{
      dtx->Uvarowner[1] = -1;
      dtx->Uvar[1] = -1;
   }
   if (is_valid_dtx_ctx(index, v2varowner)){
      dtx->Vvarowner[1] = v2varowner;
      dtx->Vvar[1] = v2;
   }
   else{
      dtx->Vvarowner[1] = -1;
      dtx->Vvar[1] = -1;
   }

   if (is_valid_dtx_ctx(index, w2varowner)){
      dtx->Wvarowner[1] = w2varowner;
      dtx->Wvar[1] = w2;
   }
   else{
      dtx->Wvarowner[1] = -1;
      dtx->Wvar[1] = -1;
   }

   if (is_valid_dtx_ctx(index, utrajowner)){
      dtx->TrajUowner = utrajowner;
      dtx->TrajU = utraj;
   }
   else{
      dtx->TrajUowner = -1;
      dtx->TrajU = -1;
   }

   if (is_valid_dtx_ctx(index, vtrajowner)){
      dtx->TrajVowner = vtrajowner;
      dtx->TrajV = vtraj;
   }
   else{
      dtx->TrajVowner = -1;
      dtx->TrajV = -1;
   }

   if (is_valid_dtx_ctx(index, wtrajowner)){
      dtx->TrajWowner = wtrajowner;
      dtx->TrajW = wtraj;
   }
   else{
      dtx->TrajWowner = -1;
      dtx->TrajW = -1;
   }
   if (dtx->TrajWowner >= 0){
      if (dtx->TrajWowner != dtx->TrajVowner ||
          dtx->TrajWowner != dtx->TrajUowner ||
          dtx->TrajVowner != dtx->TrajUowner){
         dtx->TrajUowner = -1;
         dtx->TrajU = -1;
         dtx->TrajVowner = -1;
         dtx->TrajV = -1;
         dtx->TrajWowner = -1;
         dtx->TrajW = -1;
      }
   }
   else if (dtx->TrajVowner != dtx->TrajUowner){
      dtx->TrajUowner = -1;
      dtx->TrajU = -1;
      dtx->TrajVowner = -1;
      dtx->TrajV = -1;
   }

   if (dtx->Wvarowner[0] >= 0){
      if (dtx->Wvarowner[0] != dtx->Vvarowner[0] ||
          dtx->Wvarowner[0] != dtx->Uvarowner[0] ||
          dtx->Vvarowner[0] != dtx->Uvarowner[0]){
         dtx->Uvarowner[0] = -1;
         dtx->Uvar[0] = -1;
         dtx->Vvarowner[0] = -1;
         dtx->Vvar[0] = -1;
         dtx->Wvarowner[0] = -1;
         dtx->Wvar[0] = -1;
      }
   }
   else if (dtx->Vvarowner[0] != dtx->Uvarowner[0]){
      dtx->Uvarowner[0] = -1;
      dtx->Uvar[0] = -1;
      dtx->Vvarowner[0] = -1;
      dtx->Vvar[0] = -1;
   }


   if (dtx->Wvarowner[1] >= 0){
      if (dtx->Wvarowner[1] != dtx->Vvarowner[1] ||
          dtx->Wvarowner[1] != dtx->Uvarowner[1] ||
          dtx->Vvarowner[1] != dtx->Uvarowner[1]){
         dtx->Uvarowner[1] = -1;
         dtx->Uvar[1] = -1;
         dtx->Vvarowner[1] = -1;
         dtx->Vvar[1] = -1;
         dtx->Wvarowner[1] = -1;
         dtx->Wvar[1] = -1;
      }
   }
   else if (dtx->Vvarowner[1] != dtx->Uvarowner[1]){
      dtx->Uvarowner[1] = -1;
      dtx->Uvar[1] = -1;
      dtx->Vvarowner[1] = -1;
      dtx->Vvar[1] = -1;
   }
   


   compute_wind_levels( dtx );
   for (ws=0; ws<VIS5D_WIND_SLICES; ws++) {
      for (time=0; time<dtx->NumTimes; time++) {
         if (dtx->Uvarowner[ws] >= 0){
            ctx = vis5d_get_ctx(dtx->Uvarowner[ws]);
            if (dtx->HWindTable[ws][time].valid) {
               request_hwindslice( dtx, time, ws, 0 );
            }
            if (dtx->VWindTable[ws][time].valid) {
               request_vwindslice( dtx, time, ws, 0 );
            }
            if (dtx->HStreamTable[ws][time].valid) {
               request_hstreamslice( dtx, time, ws, 0 );
            }
            if (dtx->VStreamTable[ws][time].valid) {
               request_vstreamslice( dtx, time, ws, 0 );
            }
         }
      }
   }
   return 0;
}

int vis5d_get_wind_vars( int index,
                         int *u1owner, int *u1, int *v1owner, int *v1,
                         int *w1owner, int *w1, int *u2owner, int *u2,
                         int *v2owner, int *v2, int *w2owner, int *w2,
                         int *tuowner, int *utraj, int *tvowner, int *vtraj,
                         int *twowner, int *wtraj )
{
   DPY_CONTEXT("vis5d_get_wind_vars")
   *u1owner = dtx->Uvarowner[0];
   *v1owner = dtx->Vvarowner[0];
   *w1owner = dtx->Wvarowner[0];
   *u2owner = dtx->Uvarowner[1];
   *v2owner = dtx->Vvarowner[1];
   *w2owner = dtx->Wvarowner[1];
   *tuowner = dtx->TrajUowner;
   *tvowner = dtx->TrajVowner;
   *twowner = dtx->TrajWowner;
   *u1 = dtx->Uvar[0];
   *v1 = dtx->Vvar[0];
   *w1 = dtx->Wvar[0];
   *u2 = dtx->Uvar[1];
   *v2 = dtx->Vvar[1];
   *w2 = dtx->Wvar[1];
   *utraj = dtx->TrajU;
   *vtraj = dtx->TrajV;
   *wtraj = dtx->TrajW;

   return 0;
}



/* Reset the position and attributes of all graphics for the given var. */
int vis5d_reset_var_graphics( int index, int newvar )
{
  CONTEXT("vis5d_reset_var_graphics")
  init_graphics_pos(ctx, newvar);
  return 0;
}

/* helper function similar to init_var_colortable in gui.c */
int init_var_clrtable( int dindex, int vindex, int var )
{
   float *p;
   unsigned int *table;

   /* Isosurfaces */
   vis5d_get_color_table_params( dindex, VIS5D_ISOSURF, vindex, var, &p);
   vis5d_get_color_table_address( dindex, VIS5D_ISOSURF, vindex, var, &table );
   vis5d_color_table_init_params( p, 1, 1 );
   vis5d_color_table_set_alpha( p, 255 );
   vis5d_color_table_recompute( table, 256, p, 1, 1 );

   /* CHSlices */
   vis5d_get_color_table_params( dindex, VIS5D_CHSLICE, vindex, var, &p);
   vis5d_get_color_table_address( dindex, VIS5D_CHSLICE, vindex, var, &table );
   vis5d_color_table_init_params( p, 1, 1 );
   vis5d_color_table_set_alpha( p, -2 );
/* MJK 2.8.99
   vis5d_color_table_set_alpha( p, 255 );
*/
   vis5d_color_table_recompute( table, 256, p, 1, 1 );

   /* CVSlices */
   vis5d_get_color_table_params( dindex, VIS5D_CVSLICE, vindex, var, &p);
   vis5d_get_color_table_address( dindex, VIS5D_CVSLICE, vindex, var, &table );
   vis5d_color_table_init_params( p, 1, 1 );
   vis5d_color_table_set_alpha( p, -2 );
/* MJK 2.8.99
   vis5d_color_table_set_alpha( p, 255 );
*/
   vis5d_color_table_recompute( table, 256, p, 1, 1 );

   /* Volumes */
   vis5d_get_color_table_params( dindex, VIS5D_VOLUME, vindex, var, &p);
   vis5d_get_color_table_address( dindex, VIS5D_VOLUME, vindex, var, &table );
   vis5d_color_table_init_params( p, 1, 1 );
   vis5d_color_table_set_alpha( p, -1 );
   vis5d_color_table_recompute( table, 256, p, 1, 1 );

   /* Trajectories */
   vis5d_get_color_table_params( dindex, VIS5D_TRAJ, vindex, var, &p);
   vis5d_get_color_table_address( dindex, VIS5D_TRAJ, vindex, var, &table );
   vis5d_color_table_init_params( p, 1, 1 );
   vis5d_color_table_set_alpha( p, 255 );
   vis5d_color_table_recompute( table, 256, p, 1, 1 );

   /* Topography */
   vis5d_get_color_table_params( dindex, VIS5D_TOPO, vindex, var, &p);
   vis5d_get_color_table_address( dindex, VIS5D_TOPO, vindex, var, &table );
   vis5d_color_table_init_params( p, 1, 1 );
   vis5d_color_table_set_alpha( p, 255 );
   vis5d_color_table_recompute( table, 256, p, 1, 1 );
   
   return 1;
}
int init_irregular_var_clrtable( int dindex, int iindex, int var )
{
   float *p;
   unsigned int *table;

   /* Textplots */
   vis5d_get_color_table_params( dindex, VIS5D_TEXTPLOT, iindex, var, &p);
   vis5d_get_color_table_address( dindex, VIS5D_TEXTPLOT, iindex, var, &table );
   vis5d_color_table_init_params( p, 1, 1 );
   vis5d_color_table_set_alpha( p, -2 );
   vis5d_color_table_set_alpha( p, 255 );
   vis5d_color_table_recompute( table, 256, p, 1, 1 );
   return 0;
}


/*** Grid Functions ***/

int vis5d_get_sizePRIME( int index, int *nr, int *nc, int *nl, int *lowlev,
                         int *windnl, int *windlow )
{
  int i;
  DPY_CONTEXT("vis5d_get_sizePRIME")

  if (nr)        *nr = dtx->Nr;
  if (nc)        *nc = dtx->Nc;
  if (nl)        *nl = dtx->Nl;
  if (lowlev)    *lowlev = dtx->LowLev;
  if (windnl)    *windnl = dtx->WindNl;
  if (windlow)   *windlow = dtx->WindLow;
  return 0;
}

int vis5d_get_size( int index, int *nr, int *nc, int nl[], int lowlev[],
                    int *maxnl, int *maxnlVar, int *windnl, int *windlow )
{
  int i;
  CONTEXT("vis5d_get_size")

  if (nr)        *nr = ctx->Nr;
  if (nc)        *nc = ctx->Nc;
  if (nl)        for (i=0; i<ctx->NumVars; i++)  nl[i] = ctx->Nl[i];
  if (lowlev)    for (i=0; i<ctx->NumVars; i++)  lowlev[i] = ctx->LowLev[i];
  if (maxnl)     *maxnl = ctx->MaxNl;
  if (maxnlVar)  *maxnlVar = ctx->MaxNlVar;
  if (windnl)    *windnl = ctx->WindNl;
  if (windlow)   *windlow = ctx->WindLow;
  return 0;
}


/*
 * Return a 3-D data grid.
 * Input:  index - the context index
 *         time, var - timestep and variable number
 *         data - address to put data, assumed to be large enough.
 */
int vis5d_get_grid( int index, int time, int var, float *data )
{
   float *grid;
   CONTEXT("vis5d_get_grid");

   grid = get_grid( ctx, time, var );
   memcpy( data, grid, ctx->Nr*ctx->Nc*ctx->Nl[var]*sizeof(float) );
   release_grid( ctx, time, var, grid );
   return 0;
}


/*
 * Put new data into a 3-D grid.
 * Input:  index - the context index
 *         time, var - timestep and variable number
 *         data - array of data, assumed to be the right size.
 */
int vis5d_put_grid( int index, int time, int var, float *data )
{
   CONTEXT("vis5d_get_grid");
   if (put_grid( ctx, time, var, data )) {
      return 0;
   }
   else {
      return VIS5D_FAIL;
   }
}


/*
 * Return a variable's grid value at a specific position, using current
 * timestep.
 */
int vis5d_get_grid_value( int index, int var,
                          float row, float column, float level,
                          float *value )
{
   CONTEXT("vis5d_get_grid_value");
   if (var<0 || var>=ctx->NumVars) {
      return VIS5D_BAD_VAR_NUMBER;
   }
   if (row<0.0 || row>(ctx->Nr-1) ||
       column<0.0 || column>(ctx->Nc-1) ||
       level<0.0 || level>(ctx->Nl[var]-1)) {
      /* position is out of bounds */
      return VIS5D_BAD_VALUE;
   }
   *value = interpolate_grid_value( ctx, ctx->CurTime, var,
                                    row, column, level );
   return 0;
}


/*
 * Control the VeryLarge flag.
 * Input:  index - context index
 *         mode - one of VIS5D_ON, VIS5D_OFF, VIS5D_TOGGLE, or VIS5D_GET
 * Return:  VIS5D_ON or VIS5D_OFF, after function is applied
 */
int vis5d_verylarge_mode( int index, int mode )
{
  int *val;
  CONTEXT("vis5d_verylarge_mode");
  val = &ctx->VeryLarge;
  switch (mode) {
    case VIS5D_OFF:
      *val = 0;
      break;
    case VIS5D_ON:
      if (*val == 0) printf("Using VeryLarge option - graphics may be slow\n");
      *val = 1;
      break;
    case VIS5D_TOGGLE:
      *val = *val ? 0 : 1;
      break;
    case VIS5D_GET:
      break;
    default:
      printf("bad mode (%d) in vis5d_verylarge_mode\n", mode);
      return VIS5D_BAD_MODE;
  }
  return *val;
}


/*** Map Projection and VCS Functions ***/

int vis5d_get_ctx_projection( int index, int *projection, float *projargs )
{
   CONTEXT("vis5d_get_ctx_projection");
   get_projection( ctx, projection, projargs );
   return 0;
}

int vis5d_get_dtx_projection( int index, int *projection, float *projargs )
{
   DPY_CONTEXT("vis5d_get_dtx_projection")
   get_projection_d( dtx, projection, projargs );
   return 0;
}

int vis5d_get_ctx_vertical( int index, int *vertical, float *vertargs )
{
   CONTEXT("vis5d_get_ctx_vertical");
   get_vertical_system( ctx, vertical, vertargs );

   /* MJK 12.10.98 */
   if (*vertical == VERT_NONEQUAL_MB)
   {
      int       numargs;

      numargs = (ctx->MaxNl < 2) ? 2 : ctx->MaxNl;
      while (--numargs >= 0)
      {
         if (vertargs[numargs] != -999.99)
             vertargs[numargs] = height_to_pressure (vertargs[numargs]);
      }
   }
   return 0;
}

int vis5d_get_dtx_vertical( int index, int *vertical, float *vertargs )
{
   DPY_CONTEXT("vis5d_get_dtx_vertical");
   get_vertical_system_d( dtx, vertical, vertargs );

   /* MJK 12.10.98 */
   if (*vertical == VERT_NONEQUAL_MB)
   {
      int       numargs;

      numargs = (dtx->MaxNl < 2) ? 2 : dtx->MaxNl;
      while (--numargs >= 0)
      {
         if (vertargs[numargs] != -999.99)
             vertargs[numargs] = height_to_pressure (vertargs[numargs]);
      }
   }
   return 0;
}

int vis5d_get_curved( int index, int *curved )
{
  DPY_CONTEXT("vis5d_get_size")
  *curved = dtx->CurvedBox;
  return 0;
}


int vis5d_set_dtx_projection_and_vertsys( int index, int what, int type, float towhat)
{
   int i, yo;
   float lat1, lat2;
   DPY_CONTEXT("vis5d_set_dtx_projection_and_vertsys")
   
   switch (what){
      case SET_PROJ_Projection: 
         dtx->Projection = (int) towhat;
         break;
      case SET_PROJ_NorthBound:
         dtx->NorthBound = towhat;
         break;
      case SET_PROJ_WestBound:
         dtx->WestBound = towhat;
         break;      
      case SET_PROJ_RowInc:
         dtx->RowInc = towhat;
         break;      
      case SET_PROJ_ColInc:
         dtx->ColInc = towhat;
         break;      
      case SET_PROJ_Lat1:
         dtx->Lat1 = towhat;
         break;      
      case SET_PROJ_Lat2:
         dtx->Lat2 = towhat;
         break;      
      case SET_PROJ_PoleRow:
         dtx->PoleRow = towhat;
         break;      
      case SET_PROJ_PoleCol:
         dtx->PoleCol = towhat;
         break;      
      case SET_PROJ_CentralLat:
         dtx->CentralLat = towhat;
         break;      
      case SET_PROJ_CentralLon:
         dtx->CentralLon = towhat;
         break;      
      case SET_PROJ_CentralRow:
         dtx->CentralRow = towhat;
         break;      
      case SET_PROJ_CentralCol:
         dtx->CentralCol = towhat;
         break;      
      case SET_PROJ_RowIncKm:
         dtx->RowIncKm = towhat;
         break;
      case SET_PROJ_ColIncKm:
         dtx->ColIncKm = towhat;
         break;
      case SET_PROJ_Rotation:
         dtx->Rotation = towhat;
         break;
      case SET_PROJ_CylinderScale:
         dtx->CylinderScale = towhat;
         break;
      case SET_PROJ_VerticalSystem:
         dtx->VerticalSystem = (int) towhat;
         break;
      case SET_PROJ_LevInc:
         dtx->LevInc = towhat;
         break;
      case SET_PROJ_BottomBound:
         dtx->BottomBound = towhat;
         break;
      case SET_PROJ_Height:
         if ((int)(dtx->VerticalSystem)==VERT_NONEQUAL_MB){
            dtx->Height[type] = pressure_to_height(towhat);
         }
         else{
            dtx->Height[type] = towhat;
         }
         break;
      case SET_PROJ_NumRows:
         dtx->Nr = (int) towhat;
         break;
      case SET_PROJ_NumCols:
         dtx->Nc = (int) towhat;
         break;
      case SET_PROJ_NumLevs:
         dtx->Nl = (int) towhat;
         dtx->MaxNl = dtx->Nl;
         break;
      case SET_PROJ_Done:

         /************************************************/
         /* Setting extranious values for map projection */
         /************************************************/
         if (dtx->Projection == PROJ_GENERIC ||
             dtx->Projection == PROJ_LINEAR  ||
             dtx->Projection == PROJ_CYLINDRICAL ||
             dtx->Projection == PROJ_SPHERICAL){
            dtx->SouthBound = dtx->NorthBound - dtx->RowInc * (dtx->Nr-1);
            dtx->EastBound = dtx->WestBound - dtx->ColInc * (dtx->Nc-1);
            if (dtx->Projection == PROJ_CYLINDRICAL){
               if (REVERSE_POLES==-1.0){
                  dtx->CylinderScale = 1.0 / (-1.0*(-90.0-dtx->NorthBound));
               }
               else{
                  dtx->CylinderScale = 1.0 / (90.0-dtx->SouthBound);
               }
            }
         }
         else if (dtx->Projection == PROJ_LAMBERT ){
            if (dtx->Lat1==dtx->Lat2) {
               /* polar stereographic? */
               if (dtx->Lat1>0.0) {
                  lat1 = (90.0 - dtx->Lat1) * DEG2RAD;
               }
               else {
                  lat1 = (90.0 + dtx->Lat1) * DEG2RAD;
               }
               dtx->Cone = cos( lat1 );
               dtx->Hemisphere = 1.0;
            }
            else {
               /* general Lambert conformal */
               float a, b;
               if (Sign(dtx->Lat1) != Sign(dtx->Lat2)) {
                  printf("Error: standard latitudes must have the same sign.\n");
                  /* New 5.2 */
                  vis5d_init_display_values(dtx->ctxarray[0], -1, index); 
                  printf("setting display to values of ctx-%d\n",dtx->ctxarray[0]);
                  return 0;
               }
               if (dtx->Lat1<dtx->Lat2) {
                  printf("Error: Lat1 must be >= dtx->Lat2\n");
                  return 0;
                  /* New 5.2 */                  
                  vis5d_init_display_values(dtx->ctxarray[0], -1, index);          
                  printf("setting display to values of ctx-%d\n",dtx->ctxarray[0]);         
               }
               dtx->Hemisphere = 1.0;
               lat1 = (90.0 - dtx->Lat1) * DEG2RAD;
               lat2 = (90.0 - dtx->Lat2) * DEG2RAD;
               a = log(sin(lat1)) - log(sin(lat2));
               b = log( tan(lat1/2.0) ) - log( tan(lat2/2.0) );
               dtx->Cone = a / b;
            }
            /* Cone is in [-1,1] */
            dtx->ConeFactor = RADIUS * sin(lat1)
                             / (dtx->ColInc * dtx->Cone
                                * pow(tan(lat1/2.0), dtx->Cone) );
         }
         else if (dtx->Projection == PROJ_STEREO ){
            dtx->CosCentralLat = cos( dtx->CentralLat * DEG2RAD );
            dtx->SinCentralLat = sin( dtx->CentralLat * DEG2RAD );
            dtx->StereoScale = (2.0 * RADIUS / dtx->ColInc);
            dtx->InvScale = 1.0 / dtx->StereoScale;
         }
         else if (dtx->Projection == PROJ_ROTATED){
            dtx->SouthBound = dtx->NorthBound - dtx->RowInc * (dtx->Nr-1);
            dtx->EastBound = dtx->WestBound - dtx->ColInc * (dtx->Nc-1);
         }
         /* MJK 12.28.99 */
         if (dtx->Projection != PROJ_GENERIC && dtx->Projection != PROJ_MERCATOR) {
/*
         if (dtx->Projection != PROJ_GENERIC) {
*/
            if (dtx->SouthBound < -90.0) {
               printf("SouthBound less than -90.0\n");
               /* New 5.2 */                  
               vis5d_init_display_values(dtx->ctxarray[0], -1, index);
               printf("setting display to values of ctx-%d\n",dtx->ctxarray[0]);
               return 0;
            }
            if (dtx->NorthBound < dtx->SouthBound) {
               printf("NorthBound less than SouthBound\n");
               /* New 5.2 */                  
               vis5d_init_display_values(dtx->ctxarray[0], -1, index);
               printf("setting display to values of ctx-%d\n",dtx->ctxarray[0]);
               return 0;
            }
            if (90.0 < dtx->NorthBound) {
               printf("NorthBound greater than 90.0\n");
               /* New 5.2 */                  
               vis5d_init_display_values(dtx->ctxarray[0], -1, index);
               printf("setting display to values of ctx-%d\n",dtx->ctxarray[0]);
               return 0;
            }
         }
         /*********************************************/
         /* Setting extranious values for vert system */
         /*********************************************/ 
         if (dtx->VerticalSystem == VERT_GENERIC ||
             dtx->VerticalSystem == VERT_EQUAL_KM){
            dtx->TopBound = dtx->BottomBound + dtx->LevInc * (dtx->MaxNl-1);
            for (i=0;i<dtx->MaxNl;i++) {
               dtx->Height[i] = dtx->BottomBound + i * dtx->LevInc;
            }
            if (dtx->LogFlag) {
              dtx->Ptop = dtx->LogScale * exp( dtx->TopBound / dtx->LogExp );
              dtx->Pbot = dtx->LogScale * exp( dtx->BottomBound / dtx->LogExp );
            }
         }
         else if (dtx->VerticalSystem == VERT_NONEQUAL_KM ||
                  dtx->VerticalSystem == VERT_NONEQUAL_MB){
            dtx->TopBound = dtx->Height[dtx->MaxNl-1];
            if (dtx->VerticalSystem == VERT_NONEQUAL_KM){
               dtx->Ptop = dtx->LogScale * exp( dtx->Height[dtx->MaxNl-1] / dtx->LogExp );
               dtx->Pbot = dtx->LogScale * exp( dtx->Height[0] / dtx->LogExp );
            }
            else if (dtx->VerticalSystem == VERT_NONEQUAL_MB){
               dtx->Ptop = height_to_pressure(dtx->Height[dtx->MaxNl-1]);
               dtx->Pbot = height_to_pressure(dtx->Height[0]);
            }
         }
/* WLH 3 Nov 98
         if (dtx->TopBound < dtx->BottomBound) {
             printf("TopBound less than BottomBound\n");
             vis5d_init_display_values(dtx->ctxarray[0], index);
             printf("setting display to values of ctx-%d\n",dtx->ctxarray[0]);
             return 0;
         }
*/
         if (dtx->Projection == PROJ_SPHERICAL &&
            dtx->TopBound == dtx->BottomBound){
            dtx->TopBound = dtx->BottomBound + 10;
         }
         setup_dtx(dtx, index);
         break;                  
      default:
         printf("somehting is awry in vis5d_set_dtx_projection_and_vertsys\n");
         return 0;
   }

   /* WLH 22 Oct 98 */
   return 0;

}
 








/*** Topography and Map Functions ***/

int vis5d_load_topo_and_map( int index )
{
  DPY_CONTEXT("vis5d_load_topo_and_map")

  load_topo_and_map( dtx);
}

int vis5d_check_topo( int index, int *topoflag )
{
  DPY_CONTEXT("vis5d_check_topo")
  *topoflag = dtx->TopoFlag;
  return 0;
}


int vis5d_check_map( int index, int *mapflag )
{
  DPY_CONTEXT("vis5d_check_map")
  *mapflag = dtx->MapFlag;
  return 0;
}


int vis5d_check_texture( int index, int *textureflag )
{
  DPY_CONTEXT("vis5d_check_texture")
  *textureflag = dtx->TextureFlag;
  return 0;
}


vis5d_get_topo_range( int index, float *MinTopoHgt, float *MaxTopoHgt )
{
  DPY_CONTEXT("vis5d_get_topo_range")
  *MinTopoHgt = dtx->MinTopoHgt;
  *MaxTopoHgt = dtx->MaxTopoHgt;
  return 0;
}


int vis5d_reset_topo_colors( int index )
{
   DPY_CONTEXT("vis5d_reset_topo_colors")

   init_topo_color_table( dtx->TopoColorTable[MAXVARS*VIS5D_MAX_CONTEXTS], 256,
   dtx->MinTopoHgt, dtx->MaxTopoHgt );
   dtx->Redraw = 1;
   return 0;
}



/*
 * Define a texture map.  The texture image is not copied; just the pointer
 * to it is saved.  The format of the texture image is that which is used
 * by the underlying graphics library (usually either arrays of 4-byte or
 * 1-byte color/intensity values.  The texture's width and height should
 * both be powers of two if using OpenGL.
 * Input:  timestep - which timestep this texture is to be shown
 *         width, height - size of texture in pixels
 *         components - how many color components per pixel:
 *                        1 = grayscale
 *                        3 = rgb
 *                        4 = rgba
 *         image - the texture image
 */
int vis5d_texture_image( int index, int timestep, int width, int height,
                         int components, void *image )
{
   DPY_CONTEXT("vis5d_texture");
   define_texture( dtx, timestep, width, height, components, image );
   return 0;
}


int vis5d_set_topo_color_var( int index, int colorvarctx, int colorvar )
{
   DPY_CONTEXT("vis5d_set_topo_color_var");
   if (dtx->TopoColorVarOwner != colorvarctx ||
       (dtx->TopoColorVar != colorvar && 
       dtx->TopoColorVarOwner == colorvarctx )){
      Context ctx;
      dtx->TopoColorVar = colorvar;
      dtx->TopoColorVarOwner = colorvarctx;
      ctx = vis5d_get_ctx(colorvarctx);
      request_topo_recoloring( ctx );
   }
   return 0;
}


int vis5d_get_topo_color_var( int index, int *colorvarctx, int *colorvar )
{
   DPY_CONTEXT("vis5d_get_topo_color_var")
   *colorvarctx = dtx->TopoColorVarOwner;
   *colorvar = dtx->TopoColorVar;
   return 0;
}




/*** Cloning, External Functions, and Expression Functions ***/


/*
 * Make a clone of a variable.
 * Input:  index - the context index
 *         var_to_clone - number of variable to clone
 *         newname - name to give the clone
 * Return:  VIS5D_FAIL if newname already used or the limit on number of
 *              variables has been hit.
 */
int vis5d_make_clone_variable( int index, int var_to_clone, char *newname,
                               int *newvar )
{
   int i, n;
   CONTEXT("vis5d_make_clone_variable")

   if (var_to_clone<0 || var_to_clone>=ctx->NumVars) {
      return VIS5D_BAD_VAR_NUMBER;
   }

   if (strlen(newname) > 8) return VIS5D_FAIL;

   n = vis5d_find_var( index, newname );
   if (n>=0) {
      if (ctx->CloneTable[n]==var_to_clone) {
         /* this clone already made */
         *newvar = n;
         return 0;
      }
      else {
         /* clone's name is already used. */
         return VIS5D_FAIL;
      }
   }

   /* make the clone */
   *newvar = allocate_clone_variable( ctx, newname, var_to_clone );
   if (*newvar<0) {
      return VIS5D_FAIL;
   }
   else {

      /* MJK 12.04.98 */
      vis5d_init_cloned_var_colortables(ctx->dpy_ctx->dpy_context_index, index, *newvar);

      return 0;
   }
}




int vis5d_compute_ext_func( int index, char *funcpathname, int *newvar )
{
  int i, var;
  char newname[1000];
  int recompute_flag;
  char funcname[1000];
  Context ctx;
  DPY_CONTEXT("vis5d_compute_ext_func")

  ctx = dtx->ctxpointerarray[0]; 
  printf("Computing external function %s\n", funcpathname );
  var = -1;
  *newvar = -1;
  recompute_flag = 0;

  /* let funcname = funcpathname without leading directory path */
  {
     char *ptr = strrchr( funcpathname, '/' );
     if (ptr) {
        strcpy( funcname, ptr+1 );
     }
     else {
        strcpy( funcname, funcpathname );
     }
  }

  /* look if this extfunc variable already exists */
  for (i=0;i<ctx->NumVars;i++) {
    if (strcmp(funcname,ctx->VarName[i])==0
        && ctx->VarType[i]==VIS5D_EXT_FUNC) {
      /* this variable already exists so just recompute it */
      var = i;
      recompute_flag = 1;
      break;
    }
  }

  if (recompute_flag==0) {
    /* We're going to make a new variable, make sure the name is unique */
    strcpy( newname, funcname );
    for (i=0;i<ctx->NumVars;i++) {
      if (strcmp(funcname, ctx->VarName[i])==0) {
        strcat( newname, "'" );
      }
    }

    var = allocate_extfunc_variable( ctx, newname );
    if (var==-1) {
      /* unable to make a new variable */
      deallocate_variable( ctx, var );
      return VIS5D_FAIL;
    }
  }

  /* call the external function... */
/*
  strcpy( filename, FUNCTION_PATH );
  strcat( filename, "/" );
  strcat( filename, funcname );
*/

  if (compute_analysis_variable( ctx, var, funcpathname )) {
     init_var_clrtable(dtx->dpy_context_index, ctx->context_index, var);
    /* computation was successful */
    if (recompute_flag) {
      /* delete graphics */
      free_param_graphics( ctx, var );
      /* recalculate currently displayed graphics of var */
      if (ctx->DisplaySurf[var]) {
         for (i=0;i<ctx->NumTimes;i++) {
            request_isosurface( ctx, i, var, i==ctx->CurTime );
         }
      }
      if (ctx->DisplayHSlice[var]) {
         for (i=0;i<ctx->NumTimes;i++) {
            request_hslice( ctx, i, var, i==ctx->CurTime );
         }
      }
      if (ctx->DisplayVSlice[var]) {
         for (i=0;i<ctx->NumTimes;i++) {
            request_vslice( ctx, i, var, i==ctx->CurTime );
         }
      }
      if (ctx->DisplayCHSlice[var]) {
         for (i=0;i<ctx->NumTimes;i++) {
            request_chslice( ctx, i, var, i==ctx->CurTime );
         }
      }
      if (ctx->DisplayCVSlice[var]) {
         for (i=0;i<ctx->NumTimes;i++) {
            request_cvslice( ctx, i, var, i==ctx->CurTime );
         }
      }
      ctx->dpy_ctx->Redraw = 2;
    }
    else {
      *newvar = var;
    }
    compute_wind_levels(ctx->dpy_ctx);
    return 0;
  }
  else {
    /* there was an error */
    if (!recompute_flag) {
      deallocate_variable( ctx, var );
    }
    return VIS5D_FAIL;
  }
}



int vis5d_make_expr_var( int index, char *expression, char *newname,
                         char *mess, int *newvarowner, int *newvar, int *recompute )
{
   int result;
   int expressionowner;
   Context ctx;
   DPY_CONTEXT("vis5d_make_expr_var")

   result = compute_var(dtx, expression, newvarowner, newname, mess, recompute );
   if (result<0) {
      /* error */
      return VIS5D_FAIL;
   }
   else {
      ctx = vis5d_get_ctx( *newvarowner );
      /* should this have same recompute logic as vis5d_compute_ext_func ?? */
      *newvar = result;
      strcpy( ctx->ExpressionList[*newvar], expression );
      turn_off_and_free_var_graphics( ctx, *newvar); 
      vis5d_reset_var_graphics(ctx->context_index, *newvar);
      init_var_clrtable( index, ctx->context_index, *newvar);
      if (dtx->DisplaySound){
         reload_sounding_data( dtx );
         do_pixmap_art ( dtx );
         draw_sounding(dtx , dtx->CurTime);
      }
      return 0;
   }
}



int vis5d_make_new_var( int index, char *newname, int nl, int lowlev,
                        int *newvar )
{
   int result;
   CONTEXT("vis5d_make_new_var")

   result = allocate_new_variable(ctx, newname, nl, lowlev);
   if (result<0) {
      /* error */
      return VIS5D_FAIL;
   }
   else {
      *newvar = result;
      return 0;
   }
}



/*** Rendering Functions ***/

int vis5d_signal_redraw( int index, int count )
/* Signal that a redraw is requested */
{
   DPY_CONTEXT("vis5d_signal_redraw")
   if (dtx->numofctxs + dtx->numofitxs >= 1){
      dtx->Redraw = count;
   }
   return 0;
}


int vis5d_check_redraw( int index, int *redraw )
{
   DPY_CONTEXT("vis5d_check_redraw");
   *redraw = dtx->Redraw;
   return 0;
}

int vis5d_draw_frame( int index, int animflag )
{
   int howmany, whichones[VIS5D_MAX_CONTEXTS];
   int yo,spandex, cthing, ctime;
   DPY_CONTEXT("vis5d_check_redraw");

   vis5d_get_num_of_data_sets_in_display( index, &howmany);
/*
   if (howmany >= 1){
*/
   dtx = vis5d_get_dtx( index );
/* MJK 3.29.99 
   if (dtx->Reversed) {
     vis5d_set_color( index, VIS5D_BACKGROUND, 0, 1.0, 1.0, 1.0, 1.0 );
     vis5d_set_color( index, VIS5D_BOX, 0, 0.0, 0.0, 0.0, 1.0 );
     vis5d_set_color( index, VIS5D_LABEL, 0,  0.0, 0.0, 0.0, 1.0 );
   }
   else {
     vis5d_set_color( index, VIS5D_BACKGROUND, 0,  0.0, 0.0, 0.0, 1.0 );
     vis5d_set_color( index, VIS5D_BOX, 0, 1.0, 1.0, 1.0, 1.0 );
     vis5d_set_color( index, VIS5D_LABEL, 0, 1.0, 1.0, 1.0, 1.0 );
   }
*/

   set_current_window( dtx );
   set_line_width( dtx->LineWidth );
#ifndef CAVE

/* MJK 3.29.99 */
    if (dtx->Reversed){
       clear_color( PACK_COLOR(255,255,255,255) );
    }
    else{
       clear_color( dtx->BgColor );
    }


   clear_3d_window(); 
#endif
/* MJK 3.6.99 */
   if (howmany >= 1){
      render_everything( dtx, animflag );
   }
   dtx->Redraw = 0;
   return 0;
}


int vis5d_draw_3d_only( int index, int animflag )
{
   DPY_CONTEXT("vis5d_draw_3d_only");
   render_3d_only( dtx, animflag );
   dtx->Redraw = 0;
   return 0;
}


int vis5d_draw_2d_only( int index )
{
   DPY_CONTEXT("vis5d_draw_2d_only");
   render_2d_only( dtx );
   dtx->Redraw = 0;
   return 0;
}

int vis5d_draw_sounding_only( int index, int pixmapflag)
{
   DPY_CONTEXT("vis5d_draw_sounding_only");
   render_sounding_only( dtx, pixmapflag);
   render_sounding_only( dtx, pixmapflag);
   dtx->Redraw = 0;
   return 0;
}

int vis5d_swap_frame( int index )
{
   DPY_CONTEXT("vis5d_swap_frame");
   set_current_window( dtx );
   swap_3d_window();
   return 0;
}

int vis5d_invalidate_grp_frames( int index )
{
   Display_Group grp;
   Display_Context dtx;
   int dtxloop;

   grp = vis5d_get_grp(index);
   for (dtxloop = 0; dtxloop<grp->numofdpys; dtxloop++){
      dtx = grp->dpyarray[dtxloop];
      vis5d_invalidate_dtx_frames( dtx->dpy_context_index);
   }
   return 0;
}
   

int vis5d_invalidate_dtx_frames( int index )
{
   int spandex, yo, howmany, whichones[VIS5D_MAX_CONTEXTS];
   Context ctx;
   DPY_CONTEXT("vis5d_invalidate_dtx_frames")

   invalidate_frames( dtx );
   dtx->Redraw = 1;
   return 0;
}





vis5d_set_pointer( int index, int x, int y )
{
  DPY_CONTEXT("vis5d_set_fake_pointer")
  dtx->PointerX = x;
  dtx->PointerY = y;
  return 0;
}



/*
 * Control miscellaneous graphics rendering options.
 * Input:  index - context index
 *         what - one of:  VIS5D_BOX, VIS5D_CLIP, VIS5D_CLOCK, VIS5D_MAP, VIS5D_TOPO,
 *                VIS5D_PERSPECTIVE, VIS5D_CONTOUR_NUMBERS, VIS5D_GRID_COORDS,
 *                VIS5D_PRETTY, VIS5D_INFO, VIS5D_PROBE, VIS5D_SOUND,
 *                VIS5D_CURSOR, VIS5D_TEXTURE, VIS5D_ANIMRECORD, VIS5D_JULIAN,
 *                VIS5D_BARBS, VIS5D_SND_THTA, VIS5D_SND_THTE, VIS5D_SND_W,
 *                VIS5D_SND_TICKS
 *                VIS5D_ALPHAMODE, VIS5D_HIRESTOPO, 
 *                VIS5D_SAMESCALE, VIS5D_PROBE_ON_TRAJ
 *  
 *         mode - one of VIS5D_ON, VIS5D_OFF, VIS5D_TOGGLE, or VIS5D_GET
 * Return:  VIS5D_ON or VIS5D_OFF, after function is applied
 */
int vis5d_graphics_mode( int index, int what, int mode )
{
   int *val;
   DPY_CONTEXT("vis5d_graphics_mode")

  switch(what) {
    case VIS5D_BOX:
      val = &dtx->DisplayBox;
      break;
    case VIS5D_CLOCK:
      val = &dtx->DisplayClock;
      break;
    case VIS5D_MAP:
      val = &dtx->DisplayMap;
      break;
    case VIS5D_TOPO:
      val = &dtx->DisplayTopo;
      break;
    case VIS5D_LEGENDS:
      val = &dtx->DisplayLegends;
      break;
    case VIS5D_PERSPECTIVE:
      val = &dtx->GfxProjection;
      break;
    case VIS5D_CONTOUR_NUMBERS:
      val = &dtx->ContnumFlag;
      break;
    case VIS5D_GRID_COORDS:
      val = &dtx->CoordFlag;
      break;
    case VIS5D_PRETTY:
      val = &dtx->PrettyFlag;
      /* it's up to the graphics library to test for the pretty flag */
      /* and implement whatever's needed. */
#ifdef LEAVEOUT
      if (mode==VIS5D_OFF && dtx->PrettyFlag) {
         set_pretty( 0 );
      }
      else if (mode==VIS5D_ON && !dtx->PrettyFlag) {
         set_pretty( 1 );
      }
#endif
      break;
    case VIS5D_INFO:
      val = &dtx->DisplayInfo;
      break;
    case VIS5D_PROBE:
      val = &dtx->DisplayProbe;
      break;
    case VIS5D_PROBE_ON_TRAJ:
      val = &dtx->DisplayProbeOnTraj;
      break;
    case VIS5D_SOUND:
      val = &dtx->DisplaySound;
      break;
    case VIS5D_CURSOR:
      val = &dtx->DisplayCursor;
      break;
    case VIS5D_CLIP:
      val = &dtx->DisplayClips;
      break;
    case VIS5D_ANIMRECORD:
      val = &dtx->AnimRecord;
      break;
    case VIS5D_TEXTURE:
      val = &dtx->DisplayTexture;
      break;
    case VIS5D_DEPTHCUE:
      val = &dtx->DepthCue;
      break;
    case VIS5D_JULIAN:
      val = &dtx->JulianDate;
      break;
    case VIS5D_BARBS:
      val = &dtx->WindBarbs;
      break;
    case VIS5D_SND_THTA:
      val = &dtx->Sound.thtastatus;
      break;
    case VIS5D_SND_THTE:
      val = &dtx->Sound.thtestatus;
      break;
    case VIS5D_SND_W:
      val = &dtx->Sound.wstatus;
      break;
    case VIS5D_SND_TICKS:
      val = &dtx->Sound.tickstatus;
      break;
    case VIS5D_REVERSE:  /* implemented from gui.c */
      val = &dtx->Reversed;
      break;
    case VIS5D_ALPHAMODE:
      val = &dtx->AlphaBlend;
      break;
    case VIS5D_HIRESTOPO:
      val = &dtx->HiResTopo;
      break;
    case VIS5D_SAMESCALE:
      val = &dtx->Sound.samestepflag;
      break;
    /* MJK 12.10.98 */
    case VIS5D_SND_MIXRAT:
       val = &dtx->Sound.wstatus;
       break;
    case VIS5D_SND_TEMP:
       val = &dtx->Sound.tempstatus;
       break;
    default:
      printf("bad value (%d) in vis5d_graphics_mode(what)\n", what);
      return VIS5D_BAD_CONSTANT;
  }
  switch (mode) {
    case VIS5D_OFF:
      if (*val != 0) {
        dtx->Redraw = 1;
        vis5d_invalidate_dtx_frames(index);
      }
      *val = 0;
      break;
    case VIS5D_ON:
      if (*val != 1) {
        dtx->Redraw = 1;
        vis5d_invalidate_dtx_frames(index);
      }
      *val = 1;
      break;
    case VIS5D_TOGGLE:
      *val = *val ? 0 : 1;
      dtx->Redraw = 1;
      vis5d_invalidate_dtx_frames(index);
      break;
    case VIS5D_GET:
      break;
    default:
      printf("bad mode (%d) in vis5d_graphics_mode\n", mode);
      return VIS5D_BAD_MODE;
  }

  return *val;
}



int vis5d_check_ctx_volume( int index, int *volume )
{
   CONTEXT("vis5d_check_ctx_volume") 
   *volume = ctx->Volume ? 1 : 0;
   return 0;
}

int vis5d_check_dtx_volume( int index, int *volume )
{
   DPY_CONTEXT("vis5d_check_dtx_volume")
   *volume = dtx->VolRender ? 1 : 0;
   return 0;
}




/*
 * Control what "data"-graphics to display.
 * Input:  index - context index
 *         what - one of VIS5D_ISOSURF, VIS5D_HSLICE, VIS5D_VSLICE,
 *                VIS5D_CHSLICE, VIS5D_CVSLICE, VIS5D_VOLUME, VIS5D_TRAJ,
 *                VIS5D_HWIND, VIS5D_VWIND, or VIS5D_HSTREAM
 *         number - variable number, trajectory set or wind slice number
 *                  depending on 'what'.
 *         mode - one of VIS5D_ON, VIS5D_OFF, VIS5D_TOGGLE, or VIS5D_GET
 * Return:  current value after function is applied
 */
int vis5d_enable_graphics( int index, int type, int number, int mode )
{
  int *val;
  CONTEXT("vis5d_enable_graphics");

  switch(type) {
    case VIS5D_ISOSURF:
      val = &ctx->DisplaySurf[number];
      break;
    case VIS5D_HSLICE:
      val = &ctx->DisplayHSlice[number];
      break;
    case VIS5D_VSLICE:
      val = &ctx->DisplayVSlice[number];
      break;
    case VIS5D_CHSLICE:
      val = &ctx->DisplayCHSlice[number];
      break;
    case VIS5D_CVSLICE:
      val = &ctx->DisplayCVSlice[number];
      break;
    case VIS5D_VOLUME:
      switch (mode) {
        case VIS5D_OFF:
          if (number == ctx->dpy_ctx->CurrentVolume &&
              ctx->context_index == ctx->dpy_ctx->CurrentVolumeOwner) {
            ctx->dpy_ctx->CurrentVolume = -1;
            ctx->dpy_ctx->CurrentVolumeOwner = -1;
            ctx->dpy_ctx->Redraw = 1;
            vis5d_invalidate_dtx_frames(index);
          }
          break;
        case VIS5D_ON:
          if (number != ctx->dpy_ctx->CurrentVolume && 
              ctx->context_index != ctx->dpy_ctx->CurrentVolumeOwner) {
            ctx->dpy_ctx->CurrentVolume = number;
            ctx->dpy_ctx->CurrentVolumeOwner = ctx->context_index;
            ctx->dpy_ctx->Redraw = 1;
            vis5d_invalidate_dtx_frames(index);
          }
          break;
        case VIS5D_TOGGLE:
          if (number == ctx->dpy_ctx->CurrentVolume &&
              ctx->context_index == ctx->dpy_ctx->CurrentVolumeOwner) {
            ctx->dpy_ctx->CurrentVolume = -1;
            ctx->dpy_ctx->CurrentVolumeOwner = -1;
          }
          else if (number != ctx->dpy_ctx->CurrentVolume &&
                   ctx->context_index != ctx->dpy_ctx->CurrentVolumeOwner) {
            ctx->dpy_ctx->CurrentVolume = number;
            ctx->dpy_ctx->CurrentVolumeOwner = ctx->context_index;
          }
          ctx->dpy_ctx->Redraw = 1;
          vis5d_invalidate_dtx_frames(index);
          break;
        case VIS5D_GET:
          break;
        default:
          printf("bad mode (%d) in vis5d_enable_graphics\n", mode);
          return VIS5D_BAD_MODE;
      }
      return (number == ctx->dpy_ctx->CurrentVolume);
    case VIS5D_TRAJ:
      val = &ctx->dpy_ctx->DisplayTraj[number];
      break;
    case VIS5D_HWIND:
      val = &ctx->dpy_ctx->DisplayHWind[number];
      break;
    case VIS5D_VWIND:
      val = &ctx->dpy_ctx->DisplayVWind[number];
      break;
    case VIS5D_HSTREAM:
      val = &ctx->dpy_ctx->DisplayHStream[number];
      break;
    case VIS5D_VSTREAM:
      val = &ctx->dpy_ctx->DisplayVStream[number];
      break;
    default:
      return VIS5D_BAD_CONSTANT;
  }
  switch (mode) {
    case VIS5D_OFF:
      if (*val != 0) {
        ctx->dpy_ctx->Redraw = 1;
        vis5d_invalidate_dtx_frames(ctx->dpy_ctx->dpy_context_index);
      }
      *val = 0;
      break;
    case VIS5D_ON:
      if (*val != 1) {
        ctx->dpy_ctx->Redraw = 1;
        vis5d_invalidate_dtx_frames(ctx->dpy_ctx->dpy_context_index);
      }
      *val = 1;
      break;
    case VIS5D_TOGGLE:
      *val = *val ? 0 : 1;
      ctx->dpy_ctx->Redraw = 1;
      vis5d_invalidate_dtx_frames(ctx->dpy_ctx->dpy_context_index);
      break;
    case VIS5D_GET:
      break;
    default:
      printf("bad mode (%d) in vis5d_enable_graphics\n", mode);
      return VIS5D_BAD_MODE;
  }
  /* MJK 12.01.98 */
/*
  if (mode != VIS5D_GET) enable_linked_slices (index, type, number, *val);
*/
 
  return *val;
}


vis5d_get_volume( int index, int *CurrentVolumeOwner, int *CurrentVolume )
{
   DPY_CONTEXT("vis5d_get_volume")
   *CurrentVolumeOwner = dtx->CurrentVolumeOwner;
   *CurrentVolume = dtx->CurrentVolume;
   return 0;
}


vis5d_set_volume( int index, int CurrentVolumeOwner, int CurrentVolume )
{
   DPY_CONTEXT("vis5d_set_volume")

   dtx->CurrentVolumeOwner = CurrentVolumeOwner;
   dtx->CurrentVolume = CurrentVolume;
   return 0;
}



/*
 * Return a pointer to the color of the specified graphics element.
 * Input:  index - the context index
 *         type - one of:  VIS5D_ISOSURF, VIS5D_HSLICE, VIS5D_VSLICE,
 *                VIS5D_CHSLICE, VIS5D_CVSLICE, VIS5D_VOLUME, VIS5D_TRAJ,
 *                VIS5D_HWIND, VIS5D_VWIND, VIS5D_HSTREAM, VIS5D_VSTREAM,
 *                VIS5D_BOX, VIS5D_BACKGROUND or VIS5D_LABEL.
 *         number - variable number, trajectory set number or wind slice
 *                  number depending on 'type'.
 *                  NOTE: the number for Color[] ranges from
 *                        [0...VIS5D_MAX_CONTEXTS*MAXVARS] and
 *                        so for example to get the color of the var
 *                        number 5 from vis5d_ctx number 2 of the 
 *                        VSLICE it would be:
 *                                 &dtx->Color[2*MAXVARS+5][2]
 * 
 * In/Out:  color - pointer to the color
 *
 * NOTE: THIS IS NO LONGER A VISIBLE API FUNCTION.
 */
static int get_graphics_color_address( int index, int type, int number,
                                       unsigned int **color )
{
  DPY_CONTEXT("get_graphics_color_address")
  switch(type) {
    case VIS5D_ISOSURF:
      *color = &dtx->Color[number][0];
      break;
    case VIS5D_HSLICE:
      *color = &dtx->Color[number][1];
      break;
    case VIS5D_VSLICE:
      *color = &dtx->Color[number][2];
      break;
    case VIS5D_CHSLICE:
      *color = &dtx->Color[number][3];
      break;
    case VIS5D_CVSLICE:
      *color = &dtx->Color[number][4];
      break;
    case VIS5D_VOLUME:
      *color = &dtx->Color[number][5];
      break;
    case VIS5D_TRAJ:
      *color = &dtx->TrajColor[number];
      break;
    case VIS5D_HWIND:
      *color = &dtx->HWindColor[number];
      break;
    case VIS5D_VWIND:
      *color = &dtx->VWindColor[number];
      break;
    case VIS5D_HSTREAM:
      *color = &dtx->HStreamColor[number];
      break;
    case VIS5D_VSTREAM:
      *color = &dtx->VStreamColor[number];
      break;
    case VIS5D_BOX:
      *color = &dtx->BoxColor;
      break;
    case VIS5D_LIGHT_MAP:
      *color = &dtx->LightMapColor;
      break;
    case VIS5D_DARK_MAP:
      *color = &dtx->DarkMapColor;
      break;
    case VIS5D_BACKGROUND:
      *color = &dtx->BgColor;
      break;
    case VIS5D_LABEL:
      *color = &dtx->LabelColor;
      break;
    case VIS5D_TEXTPLOT:
      *color = &dtx->TextPlotColor[number];
      break;
    default:
      return VIS5D_BAD_CONSTANT;
  }
  return 0;
}



/*
 * Set the color of Vis5D graphic.
 * Input:  index - the display context index
 *         type - one of:  VIS5D_ISOSURF, VIS5D_HSLICE, VIS5D_VSLICE,
 *                VIS5D_CHSLICE, VIS5D_CVSLICE, VIS5D_VOLUME, VIS5D_TRAJ,
 *                VIS5D_HWIND, VIS5D_VWIND, VIS5D_HSTREAM, VIS5D_VSTREAM,
 *                VIS5D_BOX, VIS5D_BACKGROUND or VIS5D_LABEL.
 *         number - which variable or trajectory set
 *         red, green, blue, alpha - the color specified as 4 floating
 *                                   point values in the interval [0,1].
 */
int vis5d_set_color( int index, int type, int number,
                     float red, float green, float blue, float alpha )
{
   unsigned int *ptr;
   int n;
   int r, g, b, a;

   n = get_graphics_color_address( index, type, number, &ptr );
   if (n) {
      return n;
   }
   r = (int) (red * 255.0);
   g = (int) (green * 255.0);
   b = (int) (blue * 255.0);
   a = (int) (alpha * 255.0);
   *ptr = PACK_COLOR(r,g,b,a);
   return 0;
}



/*
 * Get the color of Vis5D graphic.
 * Input:  index - the display context index
 *         type - the graphic type:  VIS5D_ISOSURF, VIS5D_TRAJ, etc.
 *         number - which variable or trajectory set
 * Output: red, green, blue, alpha - returns the color as 4 floating
 *                                   point values in the interval [0,1].
 */
int vis5d_get_color( int index, int type, int number,
                     float *red, float *green, float *blue, float *alpha )
{
   unsigned int *ptr;
   int n;
   int r, g, b, a;

   n = get_graphics_color_address( index, type, number, &ptr );
   if (n) {
      return n;
   }
   *red   = UNPACK_RED( *ptr )   / 255.0;
   *green = UNPACK_GREEN( *ptr ) / 255.0;
   *blue  = UNPACK_BLUE( *ptr )  / 255.0;
   *alpha = UNPACK_ALPHA( *ptr ) / 255.0;
   return 0;
}



/*
 * Set the color of the 3-D cursor to be equal to that of one of the
 * trajectory sets.
 */
int vis5d_set_cursor_color( int index, int traj_set )
{
  DPY_CONTEXT("vis5d_set_cursor_color")
  dtx->CursorColor = &dtx->TrajColor[traj_set];
  return 0;
}




/*
 * Return the address of a colortable.
 * Input:  index - display context index
 *         type - one of:  VIS5D_CHSLICE, VIS5D_CVSLICE, VIS5D_VOLUME,
 *                or VIS5D_TOPO.
 *         varowner- the vis5d_ctx index where the var belongs to
 *         number - variable number if type isn't VIS5D_TOPO
 * InOut:  colors - pointer to address of colortable
 */
int vis5d_get_color_table_address( int index, int type, int varowner, int number,
                                   unsigned int **colors )
{
  DPY_CONTEXT("vis5d_get_color_table_address")

  switch (type) {
    case VIS5D_ISOSURF:
      *colors = dtx->IsoColors[varowner*MAXVARS+number];
      break;
    case VIS5D_CHSLICE:
      *colors = dtx->CHSliceColors[varowner*MAXVARS+number];
      break;
    case VIS5D_CVSLICE:
      *colors = dtx->CVSliceColors[varowner*MAXVARS+number];
      break;
    case VIS5D_VOLUME:
      *colors = dtx->VolumeColors[varowner*MAXVARS+number];
      break;
    case VIS5D_TOPO:
      if (number<0) {
         *colors = dtx->TopoColorTable[MAXVARS*VIS5D_MAX_CONTEXTS];
      }
      else {
         *colors = dtx->TopoColorTable[varowner*MAXVARS+number];
      }
      break;
    case VIS5D_TRAJ:
      *colors = dtx->TrajColors[varowner*MAXVARS+number];
      break;
    case VIS5D_TEXTPLOT:
      *colors = dtx->TextPlotColors[varowner*MAXVARS+number];
      break;
    default:
      return VIS5D_BAD_CONSTANT;
  }
  return 0;
}



/*
 * Return the parameters which describe a color table curve.
 * Input:  index - context index
 *         graphic - one of:  VIS5D_ISOSURF, VIS5D_CHSLICE, VIS5D_CVSLICE,
 *                   VIS5D_VOLUME, VIS5D_TRAJ or VIS5D_TOPO.
 *         var - variable number if type isn't VIS5D_TOPO
 * InOut:  params - pointer to address of parameter array
 */
int vis5d_get_color_table_params( int index, int graphic, int varowner, int var,
                                  float **params )
{
   DPY_CONTEXT("vis5d_get_color_table_params")

   switch (graphic) {
      case VIS5D_ISOSURF:
         *params = dtx->IsoColorParams[varowner*MAXVARS+var];
         break;
      case VIS5D_CHSLICE:
         *params = dtx->CHSliceParams[varowner*MAXVARS+var];
         break;
      case VIS5D_CVSLICE:
         *params = dtx->CVSliceParams[varowner*MAXVARS+var];
         break;
      case VIS5D_VOLUME:
         *params = dtx->VolumeColorParams[varowner*MAXVARS+var];
         break;
      case VIS5D_TRAJ:
         *params = dtx->TrajColorParams[varowner*MAXVARS+var];
         break;
      case VIS5D_TOPO:
         if (var<0) {
            *params = dtx->TopoColorParams[MAXVARS];
         }
         else {
            *params = dtx->TopoColorParams[varowner*MAXVARS+var];
         }
         break;
      case VIS5D_TEXTPLOT:
        *params = dtx->TextPlotColorParams[varowner*MAXVARS+var];
         break;
      default:
         return VIS5D_FAIL;
   }

   return 0;
}

int vis5d_load_color_table( int index, int graphic, int varowner, int var,
                            int table_size, char *filename)
{
   FILE *f;
   float min, max, val;
   float params[1000];
   int entries;
   int i, j, r[MAX_TABLE], g[MAX_TABLE], b[MAX_TABLE], a[MAX_TABLE];
   unsigned int *table;
   Context ctx;
   DPY_CONTEXT("vis5d_load_color_table")

   if (filename[0]==0) {
      printf("Load aborted\n");
      return -1;
   }

   f = fopen( filename, "r" );
   if (!f) {
      printf("Error: couldn't open %s for reading\n", filename );
      return -1;
   }

   fscanf( f, "%d %f %f %f %f\n", &entries, &min, &max,
           &params[CURVE], &params[BIAS] );
   for (i=0;i<table_size;i++) {
      fscanf( f, "%d %d %d %d\n", &r[i], &g[i], &b[i], &a[i] );
   }
   fclose(f);

   vis5d_get_color_table_address( index, graphic, varowner, var, &table);
   ctx = vis5d_get_ctx(varowner);
   for (i=0;i<entries;i++) {
      /* convert i to a value in [ctx->MinVal[var], ctx->MaxVal[var]] */
      val = ctx->MinVal[var] + (float) i / (float) entries * (ctx->MaxVal[var]-ctx->MinVal[var]);
      /* convert val to j in [0..table_size] */
      j = (int) (table_size * (val-min) / (max-min));
      if (j<0) j = 0;
      else if (j>=table_size) j = table_size-1;

      table[i] = PACK_COLOR( r[j], g[j], b[j], a[j] );
   }
   vis5d_signal_redraw( index, 1 );
   return 0;
}


/*
 * Set the parameters which describe a color table curve, and use them
 * to recompute the color tabel of the color table.
 * Input:  index - context index
 *         graphic - one of:  VIS5D_ISOSURF, VIS5D_CHSLICE, VIS5D_CVSLICE,
 *                   VIS5D_VOLUME, VIS5D_TRAJ or VIS5D_TOPO.
 *         var - variable number if type isn't VIS5D_TOPO
 *         params - address of parameter array
 */
int vis5d_set_color_table_params( int index, int graphic, int varowner, int var, float params[] )
{
   float *p;
   int i;
   unsigned int *table;
   DPY_CONTEXT("vis5d_set_color_table_params")

   switch (graphic) {
      case VIS5D_ISOSURF:
         p = dtx->IsoColorParams[varowner*MAXVARS+var];
         break;
      case VIS5D_CHSLICE:
         p = dtx->CHSliceParams[varowner*MAXVARS+var];
         break;
      case VIS5D_CVSLICE:
         p = dtx->CVSliceParams[varowner*MAXVARS+var];
         break;
      case VIS5D_VOLUME:
         p = dtx->VolumeColorParams[varowner*MAXVARS+var];
         break;
      case VIS5D_TRAJ:
         p = dtx->TrajColorParams[varowner*MAXVARS+var];
         break;
      case VIS5D_TOPO:
         if (var<0) {
            p = dtx->TopoColorParams[MAXVARS];
         }
         else {
            p = dtx->TopoColorParams[varowner*MAXVARS+var];
         }
         break;
      default:
         return VIS5D_FAIL;
   }

   for (i=0;i<7;i++) {
      p[i] = params[i];
   }

   vis5d_get_color_table_address( index, graphic, varowner, var, &table );
   vis5d_color_table_recompute( table, 256, p, 1, 1 );
   return 0;
}




/*
 * Reset the RGB and/or Alpha curve parameters to their defaults.
 */
int vis5d_color_table_init_params( float params[], int rgb_flag, int alpha_flag )
{
   if (rgb_flag) {
      params[CURVE] = DEFAULT_CURVE;
      params[BIAS]  = DEFAULT_BIAS;
   }
   if (alpha_flag) {
      params[ALPHAPOW] = DEFAULT_ALPHAPOW;
   }
   return 0;
}



/*
 * Recompute the RGB and/or Alpha table entries from their parameters.
 */
int vis5d_color_table_recompute( unsigned int table[], int size, float params[],
                              int rgb_flag, int alpha_flag )
{
   float curve, rfact;
   int i;

   rfact = 0.5 * params[BIAS];
   curve = params[CURVE];

   if (alpha_flag) {
      if (params[ALPHAVAL]==-1) {
         params[MINALPHA] = 255;
         params[MAXALPHA] = 0;
      }
      else {
         params[MINALPHA] = params[ALPHAVAL];
         params[MAXALPHA] = params[ALPHAVAL];
      }
   }


   /* NOTE size-1 because last entry is used for missing data */
   for (i=0;i<size-1;i++) {
      int r,g,b,a;
      float s;

      /* compute s in [0,1] */
      s = (float) i / (float) (size-1);

      if (rgb_flag) {
         float t = curve * (s - rfact);   /* t in [curve*-0.5,curve*0.5) */
         r = 128.0 + 127.0 * atan( 7.0*t ) / 1.57;
         g = 128.0 + 127.0 * (2 * exp(-7*t*t) - 1);
         b = 128.0 + 127.0 * atan( -7.0*t ) / 1.57;
      }
      else {
         /* use current RGB */
         r = UNPACK_RED(   table[i] );
         g = UNPACK_GREEN( table[i] );
         b = UNPACK_BLUE(  table[i] );
      }

      if (alpha_flag) {
         if (params[ALPHAVAL]==-1) {
            /* Init alpha curve */
            a = 255.0 * pow( s, params[ALPHAPOW] );
         }
         else {
            /* Init constant alpha */
            a = params[ALPHAVAL];
         }
         if (a<params[MINALPHA])  params[MINALPHA] = a;
         if (a>params[MAXALPHA])  params[MAXALPHA] = a;
      }
      else {
         /* don't change alpha */
         a = UNPACK_ALPHA( table[i] );
      }

      /* store new packed color */
      table[i] = PACK_COLOR( r, g, b, a );
   }

   table[size-1] = PACK_COLOR( 0, 0, 0, 0 );
   return 0;
}




int vis5d_color_table_set_alpha( float params[], float alpha )
{
   params[ALPHAVAL] = alpha;
   if (alpha<0) {
      params[MINALPHA] = 0;
      params[MAXALPHA] = 255;
/* MJK 2.8.99 */
      if (alpha == -2.0){
         params[ALPHAPOW] = 0.0;
         params[ALPHAVAL] = -1.0;
      }
      else{
         params[ALPHAPOW] = DEFAULT_ALPHAPOW;
      }
   }
   else {
      params[MINALPHA] = alpha;
      params[MAXALPHA] = alpha;
      params[ALPHAVAL] = alpha;
   }
   return 0;
}





/*
 * Specify how semi-transparent graphics are to be rendered.
 * Input:  index - the context index
 *         mode - 0 = use screendoor method, 1 = use alpha blending
 */
int vis5d_alpha_mode( int index, int mode )
{
   DPY_CONTEXT("vis5d_alpha_mode");
   transparency_mode( dtx, mode );
   return 0;
}


/*
 * Specify the font to use in the 3-D window.
 * Input:  index - the context number
 *         fontname - the name of the font
 */
int vis5d_font(  int index, char *fontname, int size )
{
   DPY_CONTEXT("vis5d_font");
   set_current_window( dtx );
   set_3d_font( dtx, fontname, size );
   return 0;
}

int vis5d_soundfont( int index, char *fontname )
{
   DPY_CONTEXT("vis5d_soundfont");
   if (fontname[0]==0){
      strcpy( dtx->SoundFontName, DEFAULT_SOUNDFONTNAME);
   }
   else{
      strcpy( dtx->SoundFontName, fontname);
   }
   return 0;
}


int vis5d_get_font( int index, char *fontname, int *size)
{
   DPY_CONTEXT("vis5d_get_font");
   get_3d_font( dtx, fontname, size);

   return 0;
}

/* MJK 2.22.99 */
int vis5d_resize_contour_font( int index, int factorx, int factory)
{
   DPY_CONTEXT("vis5d_resize_contour_font");

   dtx->ContFontFactorX = factorx;
   dtx->ContFontFactorY = factory;
   return 0;
}


/* WLH 8 Oct 98 */
int vis5d_get_font_height( int index, int *height)
{
   DPY_CONTEXT("vis5d_get_font");
   *height = dtx->FontHeight;

   return 0;
}





/*
 * Set the width of lines in 3-D window.
 */
int vis5d_linewidth( int index, float width )
{
   DPY_CONTEXT("vis5d_linewidth");
   dtx->LineWidth = (float) width;
   return 0;
}

int vis5d_get_linewidth( int index, float *width )
{
   DPY_CONTEXT("vis5d_get_linewidth");
   *width = dtx->LineWidth;
   return 0;
}

/*
 * Do graphics library initializations.
 */
int vis5d_gl_setup( int index, long win_id, int width, int height )
{
   CONTEXT("vis5d_gl_init");
   context_init( ctx, win_id, width, height );
   return 0;
}



/*** 3-D View Functions ***/

int vis5d_set_matrix( int index, float ctm[4][4] )
{
   DPY_CONTEXT("vis5d_set_matrix")

   mat_copy(dtx->CTM, ctm);
   dtx->Redraw = 1;
   vis5d_invalidate_dtx_frames(index);
   return 0;
}


int vis5d_get_matrix( int index, float ctm[4][4] )
{
  DPY_CONTEXT("vis5d_get_matrix")
  mat_copy(ctm, dtx->CTM);
  return 0;
}


/*
 * Set the view to either North, South, East, West, Top, or Bottom.
 * Input:  view - one of VIS5D_NORTH, VIS5D_SOUTH, .. VIS5D_BOTTOM
 */
vis5d_set_ortho_view( int index, int view )
{
   MATRIX ctm;
   DPY_CONTEXT("vis5d_ortho_view")

   switch (view) {
      case VIS5D_BOTTOM:
         make_matrix( 0.0, 180.0, 0.0, 1.0, 0.0, 0.0, 0.0, ctm );
         vis5d_set_matrix(index, ctm);
         break;
      case VIS5D_NORTH:
         make_matrix( -90.0, 180.0, 0.0, 1.0, 0.0, 0.0, 0.0, ctm );
         vis5d_set_matrix(index, ctm);
         break;
      case VIS5D_EAST:
         make_matrix( -90.0, -90.0, 0.0, 1.0, 0.0, 0.0, 0.0, ctm );
         vis5d_set_matrix(index, ctm);
         break;
      case VIS5D_TOP:
         make_matrix( 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, ctm );
         vis5d_set_matrix(index, ctm);
         break;
      case VIS5D_SOUTH:
         make_matrix( -90.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, ctm );
         vis5d_set_matrix(index, ctm);
         break;
      case VIS5D_WEST:
         make_matrix( -90.0, 90.0, 0.0, 1.0, 0.0, 0.0, 0.0, ctm );
         vis5d_set_matrix(index, ctm);
      default:
         return VIS5D_BAD_CONSTANT;
   }
   
   dtx->FrntClip = 0.0;
   dtx->Zoom = 1.0;
   vis5d_invalidate_dtx_frames(index);

   return 0;
}



/* Set an arbitrary view rotation, scale and translation */
int vis5d_set_view( int index, float xrot, float yrot, float zrot,
                    float scale, float xtrans, float ytrans, float ztrans )
{
   MATRIX ctm;
   DPY_CONTEXT("vis5d_set_view")

   make_matrix( xrot, yrot, zrot, scale, xtrans, ytrans, ztrans, ctm );
   vis5d_set_matrix(index, ctm);
   vis5d_invalidate_dtx_frames(index);
   return 0;
}




/* retrieves view rotation, scale and translation from current matrix */
int vis5d_get_view( int index, float *xrot, float *yrot, float *zrot,
                    float *scale, float *xtrans, float *ytrans, float *ztrans )
{
   MATRIX ctm;
   DPY_CONTEXT("vis5d_get_view")

   vis5d_get_matrix(index, ctm);
   unmake_matrix( xrot, yrot, zrot, scale, xtrans, ytrans, ztrans, ctm);
   return 0;
}

 


int vis5d_set_camera( int index, int perspec, float front, float zoom )
{
   DPY_CONTEXT("vis5d_set_camera");

   dtx->GfxProjection = perspec;
   if (front<0.0) {
      dtx->FrntClip = 0.0;
   }
   else if (front>=1.0) {
      dtx->FrntClip = 0.99;
   }
   else {
      dtx->FrntClip = front;
   }
   dtx->Zoom = zoom;
   dtx->Redraw = 1;
   return 0;
}


int vis5d_get_camera( int index, int *perspec, float *front, float *zoom )
{
   DPY_CONTEXT("vis5d_get_camera");

   *perspec = dtx->GfxProjection;
   *front = dtx->FrntClip;
   *zoom = dtx->Zoom;
   return 0;
}
 



int vis5d_get_box_bounds( int index, float *xmin, float *xmax,
                          float *ymin, float *ymax, float *zmin, float *zmax )
{
   DPY_CONTEXT("vis5d_get_box_bounds");
   *xmin = dtx->Xmin;
   *xmax = dtx->Xmax;
   *ymin = dtx->Ymin;
   *ymax = dtx->Ymax;
   *zmin = dtx->Zmin;
   *zmax = dtx->Zmax;
   return 0;
}



/*** Isosurface, Slice, and Trajectory Functions ***/


/*
 * Put an isosurface request into the work queue.
 * Input:  index - the context index
 *         time - which timestep
 *         var - which variable
 *         urgent - 1 = put request at head of queue, 0 = put at back of queue
 */
int vis5d_make_isosurface( int index, int time, int var, int urgent )
{
   
   CONTEXT("vis5d_make_iso_surface")

   if (!ctx->VeryLarge || time == ctx->CurTime) {
     request_isosurface( ctx, time, var, urgent );
   }
   return 0;
}



int vis5d_set_isosurface( int index, int var, float isolevel )
{
   CONTEXT("vis5d_set_iso_surface")
   ctx->IsoLevel[var] = isolevel;
   return 0;
}


int vis5d_get_isosurface( int index, int var, float *isolevel )
{
  CONTEXT("vis5d_get_iso_surface")
  *isolevel = ctx->IsoLevel[var];
  return 0;
}


/*
 * Return the number of the variable used to color a particular isosurface.
 */
int vis5d_get_isosurface_color_var( int index, int iso_var, int *cvowner, int *colorvar )
{
   CONTEXT("vis5d_get_isosurface_color_var");
   *colorvar = ctx->IsoColorVar[iso_var];
   *cvowner = ctx->IsoColorVarOwner[iso_var];
   return 0;
}


/*
 * Set the variable used to color a particular isosurface.
 */
int vis5d_set_isosurface_color_var( int index, int iso_var, int cvowner, int colorvar )
{
   int var, time;
   CONTEXT("vis5d_set_isosurface_color_var");
   ctx->IsoColorVar[iso_var] = colorvar;
   ctx->IsoColorVarOwner[iso_var] = cvowner;
   if (index == cvowner){
      if (ctx->SameIsoColorVarOwner[iso_var] == 0){
         for (var=0;var<ctx->NumVars;var++) {
            for (time=0;time<ctx->dpy_ctx->NumTimes;time++) {
               free_isosurface(ctx, time, var);
            }
         }
      }
      ctx->SameIsoColorVarOwner[iso_var] = 1;
   }
   else{
      if (ctx->SameIsoColorVarOwner[iso_var] == 1){
         for (var=0;var<ctx->NumVars;var++) {
            for (time=0;time<ctx->dpy_ctx->NumTimes;time++) {
               free_isosurface(ctx, time, var);
            }
         }
      }
      ctx->SameIsoColorVarOwner[iso_var] = 0;
      ctx->WasSameIsoColorVarOwner[iso_var] = 0;
   }
   for (time=0;time<ctx->NumTimes;time++) {
      vis5d_make_isosurface( index, time, iso_var, time==ctx->CurTime);
   }
   ctx->dpy_ctx->Redraw = 1;
   return 0;
}

/* helper function */
int follow_slice_link (int *p_vindex, int *p_type, int *p_num)
{

   int *p_next_type, *p_next_num, *p_next_vindex;


   if (!vis5d_get_slice_link (*p_vindex, *p_type, *p_num,
                             &p_next_vindex, &p_next_type, &p_next_num)!= 0){
      return 0;
   }

   *p_type = *p_next_type;
   *p_num  = *p_next_num;
   *p_vindex = *p_next_vindex;


   return 1;
}

/* helper function */
int follow_group_graphic_link (int *p_vindex, int *p_type, int *p_num)
{

   int *p_next_type, *p_next_num, *p_next_vindex;


   if (!vis5d_get_group_graphic_link (*p_vindex, *p_type, *p_num,
                             &p_next_vindex, &p_next_type, &p_next_num)!= 0){
      return 0;
   }

   *p_type = *p_next_type;
   *p_num  = *p_next_num;
   *p_vindex = *p_next_vindex;


   return 1;
}


int vis5d_create_group_links( int gindex )
{
   Display_Group grp;
   Display_Context dtx, dtx2;
   Context ctx;
   int cnum;
   int *next_vindex, *next_type, *next_var;
   char name[20], aname[20];
   int yo, lo, var, type, found_one_already;
   int d, c, good, k, i;

   grp = vis5d_get_grp( gindex );

   /************************/   
   /* init group var links */
   /************************/
   for (i = 0; i < MAXVARS * MAXTYPES * VIS5D_MAX_CONTEXTS; i++){
      group_var_link[i].var = -1;
      group_var_link[i].vindex = -1;
      group_var_link[i].type = -1;
   }

   /********************************************************/      
   /* first go through all the linked slices and add those */
   /********************************************************/
   for (yo=0; yo<grp->numofdpys; yo++){
      dtx = grp->dpyarray[yo];
      for (lo=0; lo<dtx->numofctxs; lo++){
         ctx = dtx->ctxpointerarray[lo];
         for ( var = 0; var < ctx->NumVars; var++){
            for (type = VIS5D_HSLICE; type <= VIS5D_CVSLICE; type++){
               if (vis5d_get_slice_link(ctx->context_index, type, var,
                                    &next_vindex, &next_type, &next_var)){
                  vis5d_link_group_graphics( ctx->context_index, type, var,
                                             *next_vindex, *next_type, *next_var);
               }
            }
         }
         for (var = 0; var < VIS5D_WIND_SLICES; var++){
            for (type = VIS5D_HWIND; type <= VIS5D_VSTREAM; type++){
               if (vis5d_get_slice_link(ctx->context_index, type, var,
                                    &next_vindex, &next_type, &next_var)){
                  vis5d_link_group_graphics( ctx->context_index, type, var,
                                             *next_vindex, *next_type, *next_var);
               }
            }
         }
      }
   }

   /********************************************/
   /* now go through and look for similar vars */
   /********************************************/
   for (yo=0; yo<grp->numofdpys; yo++){
      dtx = grp->dpyarray[yo];
      for (lo=0; lo<dtx->numofctxs; lo++){
         ctx = dtx->ctxpointerarray[lo];
         for ( var = 0; var < ctx->NumVars; var++){
            /**********************/
            /* get var name first */
            /**********************/
            vis5d_get_ctx_var_name(ctx->context_index, var, aname);
            for (d = yo+1; d < grp->numofdpys; d++){
               dtx2 = grp->dpyarray[d];
               found_one_already = 0;
               for (c = 0; c < dtx2->numofctxs; c++){
                  cnum = dtx2->ctxarray[c];
                  good = vis5d_find_var(cnum, aname);
                  if (good >= 0){
                     if (!found_one_already){
                        /********************************/
                        /* throw that in the group link */
                        /********************************/
                        for (k = VIS5D_ISOSURF; k <= VIS5D_VOLUME; k++){
                           vis5d_link_group_graphics(ctx->context_index, k, var,
                                                     cnum, k, good);
                        }
                        found_one_already = 1;
                     }
                     else{
                        /***********************************/
                        /* duplicate var name in dpy, make */
                        /* sure this is not linked         */
                        /***********************************/
                        for (k = VIS5D_ISOSURF; k <= VIS5D_VOLUME; k++){
                           vis5d_unlink_group_graphics(cnum, k, good);
                        }
                     }
                  }
               }
            }
         }
      }
   }
   /********************************************************/
   /* now go through and link similar hwinds vwinds etc... */
   /********************************************************/
   dtx = grp->dpyarray[0];
   if (dtx->numofctxs > 0){
      ctx = dtx->ctxpointerarray[0];
      for (yo=1; yo<grp->numofdpys; yo++){
         dtx2 = grp->dpyarray[yo];
         cnum = dtx2->ctxarray[0];
         for (k = VIS5D_HWIND; k <= VIS5D_VSTREAM; k++){
            for (c = 0; c < VIS5D_WIND_SLICES; c++){
               vis5d_link_group_graphics(ctx->context_index, k, c,
                                      cnum, k, c);
            }
         }
      }
   }

   return 1;
}


int vis5d_link_group_graphics (int vindex1, int type1, int number1,
                             int vindex2, int type2, int number2)
{  
   int  numvar, num, nmax;
   int  cur_type, cur_num, end1_type, end1_num, end2_type, end2_num;
   int  *p_type, *p_num, *p_vindex, cur_vindex, end1_vindex, end2_vindex;



   if ((vindex1 == vindex2) && (type1 == type2) && (number1 == number2)) return VIS5D_BAD_VALUE;


   vis5d_get_ctx_numvars (vindex1, &numvar);
   num = 0;


   switch (type1)
   {
      case VIS5D_ISOSURF:
      case VIS5D_HSLICE:
      case VIS5D_CHSLICE:
      case VIS5D_VSLICE:
      case VIS5D_CVSLICE:
      case VIS5D_VOLUME:
         nmax  = numvar;
         num++;
         break;

      case VIS5D_HWIND:
      case VIS5D_HSTREAM:
      case VIS5D_VWIND:
      case VIS5D_VSTREAM:
         nmax  = VIS5D_WIND_SLICES;
         num++;
         break;

      default:
         return VIS5D_BAD_CONSTANT;
   }
   if ((number1 < 0) || (number1 >= nmax)) return VIS5D_BAD_VAR_NUMBER;

   vis5d_get_ctx_numvars (vindex2, &numvar);
   switch (type2)
   {
      case VIS5D_ISOSURF:
      case VIS5D_HSLICE:
      case VIS5D_CHSLICE:
      case VIS5D_VSLICE:
      case VIS5D_CVSLICE:
      case VIS5D_VOLUME:
         nmax  = numvar;
         num++;
         break;

      case VIS5D_HWIND:
      case VIS5D_HSTREAM:
      case VIS5D_VWIND:
      case VIS5D_VSTREAM:
         nmax  = VIS5D_WIND_SLICES; 
         num++;
         break;

      default:
         return VIS5D_BAD_CONSTANT;
   }
   if ((number2 < 0) || (number2 >= nmax)) return VIS5D_BAD_VAR_NUMBER;


   if (num != 2) return VIS5D_BAD_VALUE;



/*
 *  Find the end of the chain containing slice 1.  The end is the
 *  slice just before slice 1.
 */

   end1_type = type1;
   end1_num  = number1;
   end1_vindex = vindex1;
   cur_type  = type1;
   cur_num   = number1;
   cur_vindex = vindex1;
   while (follow_group_graphic_link (&cur_vindex, &cur_type, &cur_num))
   {
      if ((cur_type == type1) && (cur_num == number1) &&
                                (cur_vindex == vindex1)){
         break;
      }
      if ((cur_type == type2) && (cur_num == number2) &&
                                (cur_vindex == vindex2)){
          return 1;
      }

      end1_type = cur_type;
      end1_num  = cur_num;
      end1_vindex = cur_vindex;
   }

/*
 *  Find the end of the chain containing slice 2.  The end is the
 *  slice just before slice 2.
 */

   end2_type = type2;
   end2_num  = number2;
   end2_vindex = vindex2;
   cur_type  = type2;
   cur_num   = number2;
   cur_vindex = vindex2;
   while (follow_group_graphic_link (&cur_vindex, &cur_type, &cur_num))
   {
      if ((cur_type == type2) && (cur_num == number2) &&
                                (cur_vindex == vindex2)){
         break;
      }
      if ((cur_type == type1) && (cur_num == number1) &&
                                (cur_vindex == vindex1)){
          return 1;
      }
      end2_type = cur_type;
      end2_num  = cur_num;
      end2_vindex = cur_vindex;
   }

   vis5d_get_group_graphic_link (end1_vindex, end1_type, end1_num, &p_vindex, &p_type, &p_num);
   *p_type = type2;
   *p_num  = number2;
   *p_vindex = vindex2;

   vis5d_get_group_graphic_link (end2_vindex,  end2_type, end2_num, &p_vindex, &p_type, &p_num);
   *p_type = type1;
   *p_num  = number1;
   *p_vindex = vindex1;

   return 0;
}

int vis5d_unlink_group_graphics (int vindex, int type, int number)
{
   int  numvar, numh, numv, nmax;
   int  cur_vindex, end_vindex, cur_type, cur_num, end_type, end_num;
   int  *p_type, *p_num, *p_vindex;


   vis5d_get_ctx_numvars (vindex, &numvar);

   switch (type)
   {
      case VIS5D_ISOSURF:
      case VIS5D_HSLICE:
      case VIS5D_CHSLICE:
      case VIS5D_VSLICE:
      case VIS5D_CVSLICE:
      case VIS5D_VOLUME:
         nmax = numvar;
         break;

      case VIS5D_HWIND:
      case VIS5D_VWIND:
      case VIS5D_HSTREAM:
      case VIS5D_VSTREAM:
         nmax = VIS5D_WIND_SLICES;
         break;

      default:
         return VIS5D_BAD_CONSTANT;
   }

   if ((number < 0) || (number >= nmax)) return VIS5D_BAD_VAR_NUMBER;

/*
 *  Find the end of the chain containing the specified slice.  The end
 *  is the slice just before the specified slice.
 */

   end_type = type;
   end_num  = number;
   end_vindex = vindex;
   cur_type = type;
   cur_num  = number;
   cur_vindex = vindex;

   while (follow_group_graphic_link (&cur_vindex, &cur_type, &cur_num))
   {
      if ((cur_type == type) && (cur_num == number) &&
                                (cur_vindex == vindex)){
          break;
      }
      end_type = cur_type;
      end_num  = cur_num;
      end_vindex = cur_vindex;
   }

   vis5d_get_group_graphic_link (vindex, type, number, &p_vindex, &p_type, &p_num);
   cur_type = *p_type;
   cur_num  = *p_num;
   cur_vindex = *p_vindex;
   *p_type  = -1;
   *p_num   = -1;
   *p_vindex = -1;

   if ((end_type != type) || (end_num != number) || (end_vindex != vindex))
   {
      vis5d_get_group_graphic_link (end_vindex, end_type, end_num, &p_vindex, &p_type, &p_num);
      if ((cur_type == end_type) && (cur_num == end_num) &&
                                (cur_vindex == end_vindex)){
         cur_type = cur_num = cur_vindex = -1;
      }
      *p_type = cur_type;
      *p_num  = cur_num;
      *p_vindex = cur_vindex;
   }


   return 0;
}

int vis5d_get_group_graphic_link (int index, int type, int num,
                          int **p_next_vindex, int **p_next_type, int **p_next_num)
{

   int *p_type, *p_num, *p_vindex;


   p_type   =  &group_var_link[(MAXTYPES*index+type)*MAXVARS+num].type;
   p_num    =  &group_var_link[(MAXTYPES*index+type)*MAXVARS+num].var;
   p_vindex =  &group_var_link[(MAXTYPES*index+type)*MAXVARS+num].vindex;


   if (p_next_type) *p_next_type = p_type;
   if (p_next_num)  *p_next_num  = p_num;
   if (p_next_vindex) *p_next_vindex = p_vindex;

   if (*p_type == -1) return 0;


   return 1;
}

    




/*
 * Link two slices together.
 * Input:  index - context index
 *         type1 - slice to be linked
 *         number1 - index of variable for type1
 *         type2 - slice to be linked to
 *         number2 - index of variable for type2
 */
int vis5d_link_slices (int vindex1, int type1, int number1,
                       int vindex2, int type2, int number2)
{
   int  numvar, numh, numv, nmax;
   int  cur_type, cur_num, end1_type, end1_num, end2_type, end2_num;
   int  *p_type, *p_num, *p_vindex, cur_vindex, end1_vindex, end2_vindex;



   if ((vindex1 == vindex2) && (type1 == type2) && (number1 == number2)) return VIS5D_BAD_VALUE;


   vis5d_get_ctx_numvars (vindex1, &numvar);
   numh = numv = 0;


   switch (type1)
   {
      case VIS5D_HSLICE:
      case VIS5D_CHSLICE:
         nmax  = numvar;
         numh++;
         break;

      case VIS5D_VSLICE:
      case VIS5D_CVSLICE:
         nmax  = numvar;
         numv++;
         break;

      case VIS5D_HWIND:
      case VIS5D_HSTREAM:
         nmax  = VIS5D_WIND_SLICES;
         numh++;
         break;

      case VIS5D_VWIND:
      case VIS5D_VSTREAM:
         nmax  = VIS5D_WIND_SLICES;
         numv++;
         break;

      default:
         return VIS5D_BAD_CONSTANT;
   }
   if ((number1 < 0) || (number1 >= nmax)) return VIS5D_BAD_VAR_NUMBER;


   vis5d_get_ctx_numvars (vindex2, &numvar);
   switch (type2)
   {
      case VIS5D_HSLICE:
      case VIS5D_CHSLICE:
         nmax  = numvar;
         numh++;
         break;

      case VIS5D_VSLICE:
      case VIS5D_CVSLICE:
         nmax  = numvar;
         numv++;
         break;

      case VIS5D_HWIND:
      case VIS5D_HSTREAM:
         nmax  = VIS5D_WIND_SLICES;
         numh++;
         break;

      case VIS5D_VWIND:
      case VIS5D_VSTREAM:
         nmax  = VIS5D_WIND_SLICES;
         numv++;
         break;

      default:
         return VIS5D_BAD_CONSTANT;
   }
   if ((number2 < 0) || (number2 >= nmax)) return VIS5D_BAD_VAR_NUMBER;


   if ((numh != 2) && (numv != 2)) return VIS5D_BAD_VALUE;



/*
 *  Find the end of the chain containing slice 1.  The end is the
 *  slice just before slice 1.
 */

   end1_type = type1;
   end1_num  = number1;
   end1_vindex = vindex1;
   cur_type  = type1;
   cur_num   = number1;
   cur_vindex = vindex1;
   while (follow_slice_link (&cur_vindex, &cur_type, &cur_num))
   {
      if ((cur_type == type1) && (cur_num == number1) &&
                                (cur_vindex == vindex1)){
         break;
      }
      if ((cur_type == type2) && (cur_num == number2) &&
                                (cur_vindex = vindex2)){
          return 1;
      }

      end1_type = cur_type;
      end1_num  = cur_num;
      end1_vindex = cur_vindex;
   }

/*
 *  Find the end of the chain containing slice 2.  The end is the
 *  slice just before slice 2.
 */

   end2_type = type2;
   end2_num  = number2;
   end2_vindex = vindex2;
   cur_type  = type2;
   cur_num   = number2;
   cur_vindex = vindex2;
   while (follow_slice_link (&cur_vindex, &cur_type, &cur_num))
   {
      if ((cur_type == type2) && (cur_num == number2) &&
                                (cur_vindex == vindex2)){
         break;
      }
      if ((cur_type == type1) && (cur_num == number1) &&
                                (cur_vindex = vindex1)){ 
          return 1;
      }
      end2_type = cur_type;
      end2_num  = cur_num;
      end2_vindex = cur_vindex;
   }

   vis5d_get_slice_link (end1_vindex, end1_type, end1_num, &p_vindex, &p_type, &p_num);
   *p_type = type2;
   *p_num  = number2;
   *p_vindex = vindex2;

   vis5d_get_slice_link (end2_vindex,  end2_type, end2_num, &p_vindex, &p_type, &p_num);
   *p_type = type1;
   *p_num  = number1;
   *p_vindex = vindex1;

   return 0;
}


/*
 * Unlink a slice.
 * Input:  index - context index
 *         type - slice to be unlinked
 *         var - index of variable for type
 */
int vis5d_unlink_slice (int vindex, int type, int number)
{
   int  numvar, numh, numv, nmax;
   int  cur_vindex, end_vindex, cur_type, cur_num, end_type, end_num;
   int  *p_type, *p_num, *p_vindex;


   vis5d_get_ctx_numvars (vindex, &numvar);

   switch (type)
   {
      case VIS5D_HSLICE:
      case VIS5D_VSLICE:
      case VIS5D_CHSLICE:
      case VIS5D_CVSLICE:
         nmax = numvar;
         break;

      case VIS5D_HWIND:
      case VIS5D_VWIND:
      case VIS5D_HSTREAM:
      case VIS5D_VSTREAM:
         nmax = VIS5D_WIND_SLICES;
         break;

      default:
         return VIS5D_BAD_CONSTANT;
   }

   if ((number < 0) || (number >= nmax)) return VIS5D_BAD_VAR_NUMBER;



/*
 *  Find the end of the chain containing the specified slice.  The end
 *  is the slice just before the specified slice.
 */

   end_type = type;
   end_num  = number;
   end_vindex = vindex;
   cur_type = type;
   cur_num  = number;
   cur_vindex = vindex;

   while (follow_slice_link (&cur_vindex, &cur_type, &cur_num))
   {
      if ((cur_type == type) && (cur_num == number) &&
                                (cur_vindex == vindex)){
          break;
      }
      end_type = cur_type;
      end_num  = cur_num;
      end_vindex = cur_vindex;
   }

   vis5d_get_slice_link (vindex, type, number, &p_vindex, &p_type, &p_num);
   cur_type = *p_type;
   cur_num  = *p_num;
   cur_vindex = *p_vindex;
   *p_type  = -1;
   *p_num   = -1;
   *p_vindex = -1;

   if ((end_type != type) || (end_num != number) || (end_vindex != vindex))
   {
      vis5d_get_slice_link (end_vindex, end_type, end_num, &p_vindex, &p_type, &p_num);
      if ((cur_type == end_type) && (cur_num == end_num) &&
                                (cur_vindex == end_vindex)){
         cur_type = cur_num = cur_vindex = -1;
      }
      *p_type = cur_type;
      *p_num  = cur_num;
      *p_vindex = cur_vindex;
   }


   return 0;
}



/*
 *  Find the pointers to the next slice type and number for the specified
 *  slice type and number.
 *
 *  return : -1 bad type
 *            1 slice is not linked
 *            0 slice is linked
 */
int vis5d_get_slice_link (int index, int type, int num,
                          int **p_next_vindex, int **p_next_type, int **p_next_num)
{

   int *p_type, *p_num, *p_vindex;

   p_type   =  &var_link[(MAXTYPES*index+type)*MAXVARS+num].type;
   p_num    =  &var_link[(MAXTYPES*index+type)*MAXVARS+num].var;
   p_vindex =  &var_link[(MAXTYPES*index+type)*MAXVARS+num].vindex;



   if (p_next_type) *p_next_type = p_type;
   if (p_next_num)  *p_next_num  = p_num;
   if (p_next_vindex) *p_next_vindex = p_vindex;

   if (*p_type == -1) return 0;


   return 1;
}



static int new_slice_pos( int index, int type, int num )
/* if type = HSLICE, VSLICE, CHSLICE or CVSLICE then num = variable number
   if type = HWIND, VWIND, HSTREAM or VSTREAM then num = slice number */
{
  Display_Context dtx;
  CONTEXT("vis5d_new_slice_pos")
  dtx = ctx->dpy_ctx;
  switch (type) {
    case HSLICE:
      new_hslice_pos( ctx, ctx->HSliceLevel[num],
                      &ctx->HSliceZ[num], &ctx->HSliceHgt[num] );
      break;
    case CHSLICE:
      new_hslice_pos( ctx, ctx->CHSliceLevel[num],
                      &ctx->CHSliceZ[num], &ctx->CHSliceHgt[num] );
      break;
    case HWIND:
      new_hwindslice_pos( ctx->dpy_ctx, ctx->dpy_ctx->HWindLevel[num],
                      &ctx->dpy_ctx->HWindZ[num], &ctx->dpy_ctx->HWindHgt[num] );
      break;
    case VWIND:
      new_vwindslice_pos( ctx->dpy_ctx, ctx->dpy_ctx->VWindR1[num], ctx->dpy_ctx->VWindC1[num],
                      &ctx->dpy_ctx->VWindX1[num], &ctx->dpy_ctx->VWindY1[num],
                      &ctx->dpy_ctx->VWindLat1[num], &ctx->dpy_ctx->VWindLon1[num] );
      new_vwindslice_pos( ctx->dpy_ctx, ctx->dpy_ctx->VWindR2[num], ctx->dpy_ctx->VWindC2[num],
                      &ctx->dpy_ctx->VWindX2[num], &ctx->dpy_ctx->VWindY2[num],
                      &ctx->dpy_ctx->VWindLat2[num], &ctx->dpy_ctx->VWindLon2[num] );
      break;
    case HSTREAM:
      new_hwindslice_pos( ctx->dpy_ctx, ctx->dpy_ctx->HStreamLevel[num],
                      &ctx->dpy_ctx->HStreamZ[num], &ctx->dpy_ctx->HStreamHgt[num] );
      break;
    case VSTREAM:
      new_vwindslice_pos( ctx->dpy_ctx, ctx->dpy_ctx->VStreamR1[num],
                          ctx->dpy_ctx->VStreamC1[num],
                          &ctx->dpy_ctx->VStreamX1[num], &ctx->dpy_ctx->VStreamY1[num],
                          &ctx->dpy_ctx->VStreamLat1[num], &ctx->dpy_ctx->VStreamLon1[num] );
      new_vwindslice_pos( ctx->dpy_ctx, ctx->dpy_ctx->VStreamR2[num],
                          ctx->dpy_ctx->VStreamC2[num],
                          &ctx->dpy_ctx->VStreamX2[num], &ctx->dpy_ctx->VStreamY2[num],
                          &ctx->dpy_ctx->VStreamLat2[num], &ctx->dpy_ctx->VStreamLon2[num] );
      break;
    case VSLICE:
      new_vslice_pos( ctx, ctx->VSliceR1[num], ctx->VSliceC1[num],
                      &ctx->VSliceX1[num], &ctx->VSliceY1[num],
                      &ctx->VSliceLat1[num], &ctx->VSliceLon1[num] );
      new_vslice_pos( ctx, ctx->VSliceR2[num], ctx->VSliceC2[num],
                      &ctx->VSliceX2[num], &ctx->VSliceY2[num],
                      &ctx->VSliceLat2[num], &ctx->VSliceLon2[num] );
      break;
    case CVSLICE:
      new_vslice_pos( ctx, ctx->CVSliceR1[num], ctx->CVSliceC1[num],
                      &ctx->CVSliceX1[num], &ctx->CVSliceY1[num],
                      &ctx->CVSliceLat1[num], &ctx->CVSliceLon1[num] );
      new_vslice_pos( ctx, ctx->CVSliceR2[num], ctx->CVSliceC2[num],
                      &ctx->CVSliceX2[num], &ctx->CVSliceY2[num],
                      &ctx->CVSliceLat2[num], &ctx->CVSliceLon2[num] );
      break;
    default:
      printf("bad constant (%d) in vis5d_new_slice_pos\n", type);
      break;
  }

  /*
  move_linked_slices (index, type, num);
  */

  return 0;
}



/*
 * Put a horizontal contour line slice request into the work queue.
 * Input:  index - the context index
 *         time - which timestep
 *         var - which variable
 *         urgent - 1 = put request at head of queue, 0 = put at back of queue
 */
int vis5d_make_hslice( int index, int time, int var, int urgent )
{
  CONTEXT("vis5d_make_hslice")

  if (!ctx->VeryLarge || time == ctx->CurTime) {
    request_hslice(ctx, time, var, urgent);
  }

  /* MJK 12.01.98 */
  /*
  make_linked_slices (index, time, VIS5D_HSLICE, var, urgent);
  */

  return 0;
}


int vis5d_set_hslice( int index, int var, float interval,
                      float low, float high, float level )
{
   float maxlev;
   char aname[20];
   int dtxloop, ctxloop;
   CONTEXT("vis5d_set_hslice")
   
   if (var<0 || var>=ctx->NumVars) {
      return VIS5D_BAD_VAR_NUMBER;
   }
   if (ctx->dpy_ctx->Nl==1) {
      maxlev = ctx->dpy_ctx->MaxNl-1;
   }
   else {
      maxlev = ctx->dpy_ctx->MaxNl-1;
   }
   if (level<0.0) {
      level = 0.0;
   }
   else if (level>maxlev) {
      level = maxlev;
   }
   ctx->HSliceInterval[var] = interval;
   ctx->HSliceLowLimit[var] = low;
   ctx->HSliceHighLimit[var] = high;
   ctx->HSliceLevel[var] = level; 
   return new_slice_pos(index, HSLICE, var);
}


int vis5d_get_hslice( int index, int var, float *interval,
                      float *low, float *high, float *level )
{
  CONTEXT("vis5d_get_hslice")
  *interval = ctx->HSliceInterval[var];
  *low = ctx->HSliceLowLimit[var];
  *high = ctx->HSliceHighLimit[var];
  *level = ctx->HSliceLevel[var];
  return 0;
}



/*
 * Put a vertical contour line slice request into the work queue.
 * Input:  index - the context index
 *         time - which timestep
 *         var - which variable
 *         urgent - 1 = put request at head of queue, 0 = put at back of queue
 */
int vis5d_make_vslice( int index, int time, int var, int urgent )
{
  CONTEXT("vis5d_make_vslice")

  if (!ctx->VeryLarge || time == ctx->CurTime) {
    request_vslice(ctx, time, var, urgent);
  }
  /* MJK 12.01.98 */
  /*
  make_linked_slices (index, time, VIS5D_VSLICE, var, urgent);
  */

  return 0;
}


int vis5d_set_vslice( int index, int var, float interval,
                      float low, float high,
                      float row0, float col0, float row1, float col1)
{
   float maxlev;
   char aname[20];
   int dtxloop, ctxloop;
   CONTEXT("vis5d_set_vslice")

   ctx->VSliceInterval[var] = interval;
   ctx->VSliceLowLimit[var] = low;
   ctx->VSliceHighLimit[var] = high;
   ctx->VSliceR1[var] = CLAMP( row0, 0.0, ctx->dpy_ctx->Nr-1 );
   ctx->VSliceC1[var] = CLAMP( col0, 0.0, ctx->dpy_ctx->Nc-1 );
   ctx->VSliceR2[var] = CLAMP( row1, 0.0, ctx->dpy_ctx->Nr-1 );
   ctx->VSliceC2[var] = CLAMP( col1, 0.0, ctx->dpy_ctx->Nc-1 );
   return new_slice_pos(index, VSLICE, var);
}


int vis5d_get_vslice( int index, int var,
                      float *interval, float *low, float *high,
                      float *row0, float *col0, float *row1, float *col1 )
{
  CONTEXT("vis5d_get_vslice")
  *interval = ctx->VSliceInterval[var];
  *low = ctx->VSliceLowLimit[var];
  *high = ctx->VSliceHighLimit[var];
  *row0 = ctx->VSliceR1[var];
  *col0 = ctx->VSliceC1[var];
  *row1 = ctx->VSliceR2[var];
  *col1 = ctx->VSliceC2[var];
  return 0;
}


/*
 * Put a horizontal color slice request into the work queue.
 * Input:  index - the context index
 *         time - which timestep
 *         var - which variable
 *         urgent - 1 = put request at head of queue, 0 = put at back of queue
 */
int vis5d_make_chslice( int index, int time, int var, int urgent )
{
   CONTEXT("vis5d_make_chslice")

   if (!ctx->VeryLarge || time == ctx->CurTime) {
     request_chslice(ctx, time, var, urgent);
   }
   /* MJK 12.01.98 */
   /*
   make_linked_slices (index, time, VIS5D_CHSLICE, var, urgent);
   */

   return 0;
}


int vis5d_set_chslice( int index, int var, float level )
{
   float maxlev;
   char aname[20];
   int dtxloop, ctxloop;
   CONTEXT("vis5d_set_chslice")

   if (var<0 || var>=ctx->NumVars) {
      return VIS5D_BAD_VAR_NUMBER;
   }
   if (ctx->Nl[var]==1) {
      maxlev = ctx->dpy_ctx->MaxNl-1;
   }
   else {
      maxlev = ctx->dpy_ctx->MaxNl -1;
   }
   if (level<0.0) {
      level = 0.0;
   }
   else if (level>maxlev) {
      level = maxlev;
   }
   ctx->CHSliceLevel[var] = level; 
   return new_slice_pos(index, CHSLICE, var);
}


int vis5d_get_chslice( int index, int var, float *level )
{
  CONTEXT("vis5d_get_chslice")
  *level = ctx->CHSliceLevel[var];
  return 0;
}


/*
 * Put a vertical color slice request into the work queue.
 * Input:  index - the context index
 *         time - which timestep
 *         var - which variable
 *         urgent - 1 = put request at head of queue, 0 = put at back of queue
 */
int vis5d_make_cvslice( int index, int time, int var, int urgent )
{
  CONTEXT("vis5d_make_cvslice")

   if (!ctx->VeryLarge || time == ctx->CurTime) {
     request_cvslice(ctx, time, var, urgent);
   }
   /* MJK 12.01.98 */
   /*
   make_linked_slices (index, time, VIS5D_CVSLICE, var, urgent);
   */
   return 0;
}


int vis5d_set_cvslice( int index, int var,
                       float row0, float col0, float row1, float col1 )
{
   float maxlev;
   char aname[20];
   int dtxloop, ctxloop;
   float r, c, l;
   CONTEXT("vis5d_set_cvslice")

   ctx->CVSliceR1[var] = CLAMP( row0, 0.0, ctx->dpy_ctx->Nr-1 );
   ctx->CVSliceC1[var] = CLAMP( col0, 0.0, ctx->dpy_ctx->Nc-1 );
   ctx->CVSliceR2[var] = CLAMP( row1, 0.0, ctx->dpy_ctx->Nr-1 );
   ctx->CVSliceC2[var] = CLAMP( col1, 0.0, ctx->dpy_ctx->Nc-1 );

   return new_slice_pos(index, CVSLICE, var);

}


int vis5d_get_cvslice( int index, int var,
                       float *row0, float *col0, float *row1, float *col1 )
{
   float r, c, l;
   CONTEXT("vis5d_get_cvslice")
   *row0 = ctx->CVSliceR1[var];
   *col0 = ctx->CVSliceC1[var];
   *row1 = ctx->CVSliceR2[var];
   *col1 = ctx->CVSliceC2[var];
   return 0;
}


/*
 * Put a horizontal wind vector slice request into the work queue.
 * Input:  index - the context index
 *         time - which timestep
 *         slice - which wind slice
 *         urgent - 1 = put request at head of queue, 0 = put at back of queue
 */
int vis5d_make_hwindslice( int index, int time, int slice, int urgent )
{
   int vl;
   Context ctx;
   DPY_CONTEXT("vis5d_make_hwindslice")

   if (dtx->Uvarowner[slice] >= 0 && dtx->Vvarowner[slice] >= 0){
      ctx = vis5d_get_ctx(dtx->Uvarowner[slice]);
      vl = vis5d_verylarge_mode( dtx->Uvarowner[slice], VIS5D_GET);
      if (!vl || time ==dtx->CurTime) {
         request_hwindslice( dtx, time, slice, urgent); 
      }
      /* MJK 12.01.98 */
      /*
      make_linked_slices (index, time, VIS5D_HWIND, slice, urgent);
      */
      return 0;
   }

   return 0;
}


int vis5d_set_hwindslice( int index, int ws, float density, float scale,
                          float level )
{
   float maxlev, lowlev;
   int uvar, vvar, wvar;
   int uvarnl, vvarnl, wvarnl;
   int first_ctx_in_dtx;
   DPY_CONTEXT("vis5d_set_hwindslice")


   if (ws<0 || ws>=VIS5D_WIND_SLICES) {
      return VIS5D_BAD_VALUE;
   }
   uvar = dtx->Uvar[ws];
   vvar = dtx->Vvar[ws];
   wvar = dtx->Wvar[ws];


   /* MJK 4.19.99 */
   if (dtx->Uvarowner[ws] >= 0){
      uvarnl = dtx->ctxpointerarray[return_ctx_index_pos(dtx, dtx->Uvarowner[ws])]->Nl[uvar];
   }
   else{
      uvarnl = 0;
   }
   if (dtx->Vvarowner[ws] >= 0){
      vvarnl = dtx->ctxpointerarray[return_ctx_index_pos(dtx, dtx->Vvarowner[ws])]->Nl[vvar];
   }
   else{
      vvarnl = 0;
   }
   if (dtx->Wvarowner[ws] >= 0){
      wvarnl = dtx->ctxpointerarray[return_ctx_index_pos(dtx, dtx->Wvarowner[ws])]->Nl[wvar];
   }
   else{
      wvarnl = 0;
   }


   if (uvarnl == 1 || vvarnl == 1 || wvarnl == 1) {
      maxlev = dtx->MaxNl-1;
      lowlev = 0.0;
   }
   else {
      maxlev = dtx->LowLev + dtx->Nl - 1;
      lowlev = dtx->LowLev;
   }
   if (level < lowlev) {
      level = lowlev;
   }
   else if (level > maxlev) {
      level = maxlev;
   }
   dtx->HWindLevel[ws] = level;
   dtx->HWindDensity[ws] = density;
   dtx->HWindScale[ws] = scale;
   first_ctx_in_dtx = dtx->ctxarray[0];
   return new_slice_pos(first_ctx_in_dtx, HWIND, ws);
}


int vis5d_get_hwindslice( int index, int ws, float *density, float *scale,
                          float *level )
{
  DPY_CONTEXT("vis5d_get_hwindslice")
  *level = dtx->HWindLevel[ws];
  *density = dtx->HWindDensity[ws];
  *scale = dtx->HWindScale[ws];
  return 0;
}


/*
 * Put a vertical wind vector slice request into the work queue.
 * Input:  index - the context index
 *         time - which timestep
 *         slice - which wind slice
 *         urgent - 1 = put request at head of queue, 0 = put at back of queue
 */
int vis5d_make_vwindslice( int index, int time, int slice, int urgent )
{
   int vl;
   Context ctx;
   DPY_CONTEXT("vis5d_make_vwindslice")

   if (dtx->Uvarowner[slice] >= 0 && dtx->Vvarowner[slice] >= 0){
      ctx = vis5d_get_ctx(dtx->Uvarowner[slice]);
      vl = vis5d_verylarge_mode( dtx->Uvarowner[slice], VIS5D_GET);
      if (!vl || time == dtx->CurTime) {
        request_vwindslice( dtx, time, slice, urgent);
      }
      /* MJK 12.01.98 */
      /*
      make_linked_slices (index, time, VIS5D_VWIND, slice, urgent);
      */
      return 0;
   }

   return 0;
}


int vis5d_set_vwindslice( int index, int ws,
                          float density, float scale,
                          float row0, float col0, float row1, float col1 )
{
   int first_ctx_in_dtx;
   DPY_CONTEXT("vis5d_set_vwindslice")

   dtx->VWindDensity[ws] = density;
   dtx->VWindScale[ws] = scale;
   dtx->VWindR1[ws] = CLAMP( row0, 0.0, dtx->Nr-1 );
   dtx->VWindC1[ws] = CLAMP( col0, 0.0, dtx->Nc-1 );
   dtx->VWindR2[ws] = CLAMP( row1, 0.0, dtx->Nr-1 );
   dtx->VWindC2[ws] = CLAMP( col1, 0.0, dtx->Nc-1 );

   first_ctx_in_dtx = dtx->ctxarray[0];

   return new_slice_pos(first_ctx_in_dtx, VWIND, ws);
}


int vis5d_get_vwindslice( int index, int ws, float *density, float *scale,
                          float *row0, float *col0, float *row1, float *col1 )
{
   DPY_CONTEXT("vis5d_get_vwindslice")
   *density = dtx->VWindDensity[ws];
   *scale = dtx->VWindScale[ws];
   *row0 = dtx->VWindR1[ws];
   *col0 = dtx->VWindC1[ws];
   *row1 = dtx->VWindR2[ws];
   *col1 = dtx->VWindC2[ws];
   return 0;
}


/*
 * Put a horizontal wind vector slice request into the work queue.
 * Input:  index - the display context index
 *         time - which timestep
 *         slice - which wind slice
 *         urgent - 1 = put request at head of queue, 0 = put at back of queue
 */
int vis5d_make_hstreamslice( int index, int time, int slice, int urgent )
{
   int vl;
   Context ctx;
   DPY_CONTEXT("vis5d_make_hstreamslice")

   if (dtx->Uvarowner[slice] >= 0 && dtx->Vvarowner[slice] >= 0){
      ctx = vis5d_get_ctx(dtx->Uvarowner[slice]);
      vl = vis5d_verylarge_mode( dtx->Uvarowner[slice], VIS5D_GET);
      if (!vl || time == dtx->CurTime) {
        request_hstreamslice( dtx, time, slice, urgent);
      }
      /* MJK 12.01.98 */
      /*
      make_linked_slices (index, time, VIS5D_HSTREAM, slice, urgent);
      */
      return 0;
   }

   return 0;
}


int vis5d_set_hstreamslice( int index, int ws, float density, float level )
{
   int first_ctx_in_dtx;  

   DPY_CONTEXT("vis5d_set_hstreamslice")

   dtx->HStreamLevel[ws] = level;
   dtx->HStreamDensity[ws] = density;
   first_ctx_in_dtx = dtx->ctxarray[0];

   return new_slice_pos(first_ctx_in_dtx, HSTREAM, ws);
}


int vis5d_get_hstreamslice( int index, int ws, float *density, float *level )
{
   DPY_CONTEXT("vis5d_get_hstreamslice")
   *level = dtx->HStreamLevel[ws];
   *density = dtx->HStreamDensity[ws];
   return 0;
}


/*
 * Put a vertical wind vector slice request into the work queue.
 * Input:  index - the display context index
 *         time - which timestep
 *         slice - which wind slice
 *         urgent - 1 = put request at head of queue, 0 = put at back of queue
 */
int vis5d_make_vstreamslice( int index, int time, int slice, int urgent )
{
   int vl;
   Context ctx;
   DPY_CONTEXT("vis5d_make_vstreamslice")


   if (dtx->Uvarowner[slice] >= 0 && dtx->Vvarowner[slice] >= 0){
      ctx = vis5d_get_ctx(dtx->Uvarowner[slice]);
      vl = vis5d_verylarge_mode( dtx->Uvarowner[slice], VIS5D_GET);
      if (!vl || time == dtx->CurTime) {
        request_vstreamslice( dtx, time, slice, urgent);
      }
      /* MJK 12.01.98 */
      /*
      make_linked_slices (index, time, VIS5D_VSTREAM, slice, urgent);
      */
      return 0;
   }

   return 0;
}


int vis5d_set_vstreamslice( int index, int ws, float density,
                            float row0, float col0, float row1, float col1 )
{
   int first_ctx_in_dtx;

   DPY_CONTEXT("vis5d_set_vstreamslice")

   dtx->VStreamDensity[ws] = density;
   dtx->VStreamR1[ws] = CLAMP( row0, 0.0, dtx->Nr-1 );
   dtx->VStreamC1[ws] = CLAMP( col0, 0.0, dtx->Nc-1 );
   dtx->VStreamR2[ws] = CLAMP( row1, 0.0, dtx->Nr-1 );
   dtx->VStreamC2[ws] = CLAMP( col1, 0.0, dtx->Nc-1 );

   first_ctx_in_dtx = dtx->ctxarray[0];

   return new_slice_pos(first_ctx_in_dtx, VSTREAM, ws);
}


int vis5d_get_vstreamslice( int index, int ws, float *density,
                            float *row0, float *col0, float *row1, float *col1 )
{
   DPY_CONTEXT("vis5d_get_vstreamslice")
   *density = dtx->VStreamDensity[ws];
   *row0 = dtx->VStreamR1[ws];
   *col0 = dtx->VStreamC1[ws];
   *row1 = dtx->VStreamR2[ws];
   *col1 = dtx->VStreamC2[ws];
   return 0;
}



int vis5d_print_traj( int index, int traj_num, float lat[],
                      float lon[], float hgt[], float traj_value[])
{
   struct traj *t;
   float valscale, min,lt, ln, ht;
   int i, endpoint;
   DPY_CONTEXT("vis5d_print_traj")

   t = dtx->TrajTable[traj_num];
   if (t->colorvar != -1){
      Context otherctx;

      otherctx = dtx->ctxpointerarray[return_ctx_index_pos(dtx, t->colorvarowner)];
      valscale = 1.0F / (otherctx->MaxVal[t->colorvar] - otherctx->MinVal[t->colorvar]);
      min = otherctx->MinVal[t->colorvar];
      for (i = 0; i < dtx->NumTimes; i ++){
         if (t->len[i] < 1){
            lat[i] = 0.00;
            lon[i] = 0.00;
            hgt[i] = 0.00;
            traj_value[i] = 0.00;
         }
         else{
            endpoint = t->start[i]+ t->len[i]-1;
            vis5d_xyzPRIME_to_geo( dtx->dpy_context_index, 0, dtx->Uvar[0],
                           (float)(t->verts[endpoint*3+0]) / VERTEX_SCALE,
                           (float)(t->verts[endpoint*3+1]) / VERTEX_SCALE,
                           (float)(t->verts[endpoint*3+2]) / VERTEX_SCALE,
                            &lt, &ln, &ht);
            lat[i] = lt;
            lon[i] = ln;
            hgt[i] = ht;
            traj_value[i] = ((float)((t->colors[endpoint]))/(valscale * 254.0F)) + min;
         }
      }
   }
   else{
      for (i = 0; i < dtx->NumTimes; i ++){
         if (t->len[i] < 1){
            lat[i] = 0.00;
            lon[i] = 0.00;
            hgt[i] = 0.00;
            traj_value[i] = 0.00;
         }
         else{
            endpoint = t->start[i] + t->len[i]-1;
            vis5d_xyzPRIME_to_geo( dtx->dpy_context_index, 0, dtx->Uvar[0],
                              (float)(t->verts[endpoint*3+0]) / VERTEX_SCALE,
                              (float)(t->verts[endpoint*3+1]) / VERTEX_SCALE,
                              (float)(t->verts[endpoint*3+2]) / VERTEX_SCALE,
                               &lt, &ln, &ht);
            lat[i] = lt;
            lon[i] = ln;
            hgt[i] = ht;
            traj_value[i] = 0;
         }
      }
   }
   return 0;
}



/* row, col, lev in virtual grid */
/* time in display time */
int vis5d_make_traj( int index, float row, float col, float lev,
                     int time, int set)
{
   DPY_CONTEXT("vis5d_make_traj")

   if (row < 0.0){
      row = 0.0;
   }
   else if (row > dtx->Nr-1){
      row = (float) dtx->Nr-1;
   }
   if (col < 0.0){
      col = 0.0;
   }
   else if (col > dtx->Nc-1){
      col = (float) dtx->Nc-1;
   }
   if (lev < 0.0){
      lev = 0.0;
   }
   else if (lev > dtx->Nl){
      lev = (float) dtx->Nl;
   }
   request_traj(dtx, row, col, lev, time, set, dtx->RibbonFlag,
                dtx->UserTrajStep, dtx->UserTrajLength);
   return 0;
}


int vis5d_set_traj( int index, float step, float length, int ribbon_flag )
{
   DPY_CONTEXT("vis5d_set_traj")

   dtx->UserTrajStep = step;
   dtx->UserTrajLength = length;
   dtx->RibbonFlag = ribbon_flag;
   return 0;
}


int vis5d_get_traj( int index, float *step, float *length, int *ribbon_flag )
{
  DPY_CONTEXT("vis5d_get_traj")
  *step = dtx->UserTrajStep;
  *length = dtx->UserTrajLength;
  *ribbon_flag = dtx->RibbonFlag;
  return 0;
}


/*
 * Set the variable used to color a trajectory set.
 */
int vis5d_set_trajectory_color_var( int index, int traj_set, int cvowner, int colorvar )
{
   Context ctx;
   DPY_CONTEXT("vis5d_set_trajectory_color_var");

   ctx = vis5d_get_ctx(cvowner);
   if (dtx->TrajColorVar[traj_set] != cvowner || 
       (dtx->TrajColorVar[traj_set] != colorvar &&
       dtx->TrajColorVarOwner[traj_set] == cvowner)) {
      /* recolor existing trajectories in the set */
      dtx->TrajColorVar[traj_set] = colorvar;
      dtx->TrajColorVarOwner[traj_set] = cvowner;
      request_traj_recoloring( ctx, traj_set );
   }
   return 0;
}


/*
 * Return the number of the variable used to color a trajectory set.
 */
int vis5d_get_trajectory_color_var( int index, int traj_set, int *cvowner, int *colorvar )
{
   DPY_CONTEXT("vis5d_get_trajecotry_color_var");
   *colorvar = dtx->TrajColorVar[traj_set];
   *cvowner = dtx->TrajColorVarOwner[traj_set];
   return 0;
}


int vis5d_delete_last_traj( int index )
{
  DPY_CONTEXT("vis5d_del_traj");
  del_last_traj(dtx);
  return 0;
}


int vis5d_delete_traj_set( int index, int set )
{
  DPY_CONTEXT("vis5d_del_traj_set");
  del_traj_group(dtx, set);
  return 0;
}


/*
 * Return number of trajectories in the context.
 */
int vis5d_get_num_traj( int index )
{
   DPY_CONTEXT("vis5d_get_num_traj");
   return dtx->NumTraj;
}


/*
 * Return parameters of an individual trajectory.
 */
int vis5d_get_traj_info( int index, int trajnum,
                         float *row, float *column, float *level,
                         int *timestep, float *step, float *length,
                         int *group, int *ribbon )
{
   struct traj *t;
   DPY_CONTEXT("vis5d_get_traj_info");
   if (trajnum>=dtx->NumTraj) {
      return VIS5D_BAD_VALUE;
   }
   t = dtx->TrajTable[trajnum];

   *row      = t->row;
   *column   = t->col;
   *level    = t->lev;
   *timestep = t->timestep;
   *step     = t->stepmult;
   *length   = t->lengthmult;
   *group    = t->group;
   *ribbon   = t->kind;
   return 0;
}



int vis5d_make_timestep_graphics( int index, int time )
{
   int var, ws;
   int yo, spandex;
   Context ctx;
   DPY_CONTEXT("vis5d_make_timestep_graphics")

   for (yo = 0; yo < dtx->numofctxs; yo++){  
      spandex = dtx->TimeStep[time].owners[yo];
      ctx = vis5d_get_ctx( spandex );
      for (var=0; var<ctx->NumVars; var++) {
         if (ctx->DisplaySurf[var]) request_isosurface(ctx, time, var, 1 );
         if (ctx->DisplayHSlice[var]) request_hslice(ctx, time, var, 1);
         if (ctx->DisplayVSlice[var]) request_vslice(ctx, time, var, 1);
         if (ctx->DisplayCHSlice[var]) request_chslice(ctx, time, var, 1);
         if (ctx->DisplayCVSlice[var]) request_cvslice(ctx, time, var, 1);
      }
   }   
   for (ws=0; ws<VIS5D_WIND_SLICES; ws++) {
      ctx = vis5d_get_ctx(dtx->Uvarowner[ws]);
      if (dtx->DisplayHWind[ws]) request_hwindslice(dtx, time, ws, 1);
      if (dtx->DisplayVWind[ws]) request_vwindslice(dtx, time, ws, 1);
      if (dtx->DisplayHStream[ws]) request_hstreamslice(dtx, time, ws, 1);
      if (dtx->DisplayVStream[ws]) request_vstreamslice(dtx, time, ws, 1);
   }

   return 0;
}




int vis5d_free_graphics( int index )
{
   CONTEXT("vis5d_free_graphics")
   free_all_graphics( ctx ); 
   return 0;
}






/*** Text Label Functions ***/


/*
 * Allocate a new label struct, assign it a unique ID, insert it into
 * the head of the linked list for the context, and disable editing
 * of the previous label (if any).
 */
static struct label *alloc_label( Display_Context dtx )
{
   static int LabelID = 1000;
   struct label *l, *next;

   l = (struct label *) malloc( sizeof(struct label) );
   if (l) {
      l->id = LabelID++;
      if (dtx->FirstLabel && dtx->FirstLabel->state) {
         /* disable editing of prev label */
         if (dtx->FirstLabel->len==0) {
            /* delete zero-length label */
            next = dtx->FirstLabel->next;
            free( dtx->FirstLabel );
            dtx->FirstLabel = next;
         }
         else {
            dtx->FirstLabel->state = 0;
         }
      }
      l->next = dtx->FirstLabel;
      dtx->FirstLabel = l;
   }
   return l;
}


static void compute_label_bounds( Display_Context dtx, struct label *lab )
{
   set_current_window( dtx );
   lab->x1 = lab->x;
   lab->y1 = lab->y + dtx->FontDescent - dtx->FontHeight;
   lab->x2 = lab->x + text_width( lab->text );
   lab->y2 = lab->y + dtx->FontDescent;
}



/*
 * Make a complete text label at position (x,y).
 */
int vis5d_make_label( int index, int x, int y, char *text )
{
   struct label *l;
   DPY_CONTEXT("vis5d_make_label");

   l = alloc_label( dtx );
   if (l) {
      strcpy( l->text, text );
      l->len = strlen( text );
      l->x = x;
      l->y = y;
      l->state = 0;
      compute_label_bounds( dtx, l );
      return 0;
   }
   return VIS5D_OUT_OF_MEMORY;
}


/*
 * Start a new text label at x, y.  Characters will be appended onto it
 * with vis5d_edit_label().
 */
int vis5d_new_label( int index, int x, int y, int *label_id )
{
   struct label *l;
   DPY_CONTEXT("vis5d_new_label");

   l = alloc_label( dtx );
   if (l) {
      l->text[0] = 0;
      l->len = 0;
      l->x = x;
      l->y = y;
      l->state = 1;
      *label_id = l->id;
      compute_label_bounds( dtx, l );
      return 0;
   }
   return VIS5D_OUT_OF_MEMORY;
}



/*
 * Append the character onto the new label.
 * if chr==BACKSPACE, delete last char
 * if chr==RETURN, finish the label
 * See also: vis5d_new_label
 */
int vis5d_edit_label( int index, char chr )
{
   struct label *lab;
   DPY_CONTEXT("vis5d_edit_label");

   /* We can only edit the most recently made label.  It will be at the
    * head of the linked list!
    */

   lab = dtx->FirstLabel;
   if (lab && lab->state==1) {
      if (chr=='\r') {
         /* RETURN key, done editing */
         lab->state = 0;
         if (lab->len==0) {
            /* delete zero-length labels */
            struct label *next = lab->next;
            free( lab );
            dtx->FirstLabel = next;
            return 0;
         }
      }
      else if (chr==8 || chr==127) {
         /* BACKSPACE or DELETE key, delete last character */
         if (lab->len>0) {
            lab->len--;
            lab->text[lab->len] = 0;
         }
      }
      else {
         /* Append the character */
         lab->text[lab->len] = chr;
         lab->len++;
         lab->text[lab->len] = 0;
      }
      compute_label_bounds( dtx, lab );
   }
   return 0;
}



/*
 * Return the ID of the label near point (x,y).
 */
int vis5d_find_label( int index, int *x, int *y, int *label_id )
{
   struct label *lab;
   DPY_CONTEXT("vis5d_find_label");

   for (lab=dtx->FirstLabel; lab; lab=lab->next) {
      if (lab->x1<=*x && *x<=lab->x2 && lab->y1<=*y && *y<=lab->y2) {
         *x = lab->x;
         *y = lab->y;
         *label_id = lab->id;
         return 0;
      }
   }
   return VIS5D_FAIL;
}



/*
 * Move the specified label to (x,y).
 */
int vis5d_move_label( int index, int label_id, int x, int y )
{
   struct label *lab;
   DPY_CONTEXT("vis5d_move_label");

   for (lab=dtx->FirstLabel; lab; lab=lab->next) {
      if (lab->id==label_id) {
         lab->x = x;
         lab->y = y;
         compute_label_bounds( dtx, lab );
         return 0;
      }
   }
   return VIS5D_BAD_VALUE;
}



/*
 * Delete the specified label.
 */
int vis5d_delete_label( int index, int label_id )
{
   struct label *lab, *prev;
   DPY_CONTEXT("vis5d_delete_label");

   prev = NULL;
   for (lab=dtx->FirstLabel; lab; lab=lab->next) {
      if (lab->id == label_id) {
         /* found it */
         if (prev) {
            prev->next = lab->next;
         }
         else {
            dtx->FirstLabel = lab->next;
         }
         free( lab );
         return 0;
      }
      prev = lab;
   }
   return VIS5D_BAD_VALUE;
}


/*
 * Return the nth text label.
 * Input:  index - the display xcontext index
 *         n - which label, starting at 1
 * Output:  x, y - the label position
 *          label - the label text
 * Return:  VIS5D_OK - if n was valid
 *          VIS5D_FAIL - if n is larger than the number of labels
 */
int vis5d_get_label( int index, int n, int *x, int *y, char *label )
{
   int i;
   struct label *lab;
   DPY_CONTEXT("vis5d_get_label");

   if (n<1) {
      return VIS5D_FAIL;
   }

   lab = dtx->FirstLabel;
   for (i=0;i<n-1;i++) {
      if (!lab) {
         return VIS5D_FAIL;
      }
      else {
         lab = lab->next;
      }
   }

   if (!lab) {
      return VIS5D_FAIL;
   }

   /* Return nth label's info */
   *x = lab->x;
   *y = lab->y;
   strcpy( label, lab->text );
   lab = lab->next;
   return 0;
}



/*** 3-D Cursor Functions ***/

int vis5d_set_cursor( int index, float x, float y, float z )
{
   float lat, lon, hgt, row, col, lev;
   int dindex;
   DPY_CONTEXT("vis5d_set_cursor")

   vis5d_xyzPRIME_to_geo(index, 0, 0, x, y, z, &lat, &lon, &hgt);
   dtx->CursorX = x;
   dtx->CursorY = y;
   dtx->CursorZ = z;
   return 0;
}


int vis5d_get_cursor( int index, float *x, float *y, float *z )
{
   DPY_CONTEXT("vis5d_get_cursor")

   *x = dtx->CursorX;
   *y = dtx->CursorY;
   *z = dtx->CursorZ;
   return 0;
}

vis5d_set_logo_size( int index, float size )
{
   DPY_CONTEXT("vis5d_set_logo_size")
   dtx->LogoSize = size;
   dtx->Redraw = 1;
   return 0;
}

/*** Color Legend Function */

int vis5d_set_legends( int index, int position, int size, int marginx, int marginy )
{
  DPY_CONTEXT("vis5d_set_legends")
  if (position != VIS5D_TOP && position != VIS5D_BOTTOM &&
      position != VIS5D_RIGHT && position != VIS5D_LEFT) {
    return VIS5D_BAD_VALUE;
  }
  if (size < 10 || size > 1000) return VIS5D_BAD_VALUE;
  dtx->LegendPosition = position;
  dtx->LegendSize = size;
  dtx->LegendMarginX = marginx;
  dtx->LegendMarginY = marginy;
  return 0;
}

int vis5d_get_legends( int index, int *position, int *size, int *marginx, int *marginy )
{
  DPY_CONTEXT("vis5d_get_legends")
  *position = dtx->LegendPosition;
  *size = dtx->LegendSize;
  *marginx = dtx->LegendMarginX;
  *marginy = dtx->LegendMarginY;

  return 0;
}

/*** 3-D Viewing Window Functions */

int vis5d_get_window( int index, Window *window, int *width, int *height )
{
   DPY_CONTEXT("vis5d_get_window");
   *window = dtx->GfxWindow;
   *width = dtx->WinWidth;
   *height = dtx->WinHeight;
   return 0;
}


int vis5d_resize_BIG_window(  int width, int height )
{
   /* MJK 12.21.98 */
   /* MJK 4.14.99
   if (BigWinFull){
      return 0;
   }
   */
   if (StaticWin){
      width = StaticWinWidth;
      height = StaticWinHeight;
   }
   
   XSynchronize(GfxDpy, 1);
   XResizeWindow(GfxDpy, BigWindow, width, height);
   resize_BIG_window( width, height);
   XSynchronize(GfxDpy, 0);
   return 0;

}

int vis5d_resize_3d_window( int index, int width, int height )
{
   DPY_CONTEXT("vis5d_resize_3d_window");
   XSynchronize(GfxDpy, 1);
   set_current_window( dtx );
   XResizeWindow(GfxDpy, dtx->GfxWindow, width, height);
   resize_3d_window( width, height);
   XSynchronize(GfxDpy, 0);
   return 0;
}

int vis5d_moveresize_BIG_window( int x, int y, int width, int height)
{
   /* MJK 12.21.98 */   
   /* MJK 4.14.99
   if (BigWinFull){
      return 0;
   }
   */
   if (StaticWin){
      width = StaticWinWidth;
      height = StaticWinHeight;
      x = StaticWinXPos;
      y = StaticWinYPos;
   }

   XSynchronize(GfxDpy, 1);
   XMoveResizeWindow(GfxDpy, BigWindow, x, y,
                     width, height);
   resize_BIG_window( width, height);
   XSynchronize(GfxDpy, 0);

   return 0;
}

int vis5d_moveresize_3d_window( int index, int x, int y,
                                int width, int height )
{
   DPY_CONTEXT("vis5d_moveresize_3d_window")
   XSynchronize(GfxDpy, 1);
   set_current_window( dtx );
   XMoveResizeWindow(GfxDpy, dtx->GfxWindow, x, y,
                     width, height);
   resize_3d_window( width, height);
   XSynchronize(GfxDpy, 0);
   return 0;
}
 

int vis5d_get_image_formats( int *formats )
{
   *formats =  save_formats();
   return 0;
}


int vis5d_resize_sounding_window( int index, int width, int height , int x, int y)
{
   DPY_CONTEXT("vis5d_resize_sounding_window")
   resize_snd_window( dtx, width, height, x, y);
   return 0;
}
   
/*
 * Save window image to a file.
 * Input:  
 *         filename - name of image file.
 *         format:  1 = VIS5D_SGI, 2 = VIS5D_GIF, 4 = VIS5D_XWD,
 *                   8 = VIS5D_PS, 16 = VIS5D_COLOR_PS
 */
int vis5d_save_window( char *filename, int format )
{
   Display_Context dtx;
   int i;
   char s[1000];
   struct stat buf;
   FILE *f;
   int use_convert;

   strcpy( s, "./util/convert");
   if (stat(s, &buf)==0 && (buf.st_mode & S_IEXEC)) {
      use_convert = 1;
   }
   else{
      use_convert = 0;
   }


   if (filename[0]==0) {
      /* no filename! */
      return VIS5D_FAIL;
   }
   if (off_screen_rendering && format != VIS5D_PPM){
      printf("Error: when off screen rendering, save format must be VIS5D_PPM\n");
      return VIS5D_FAIL;
   }

   XRaiseWindow( GfxDpy, BigWindow);
 
   /* MJK 11.19.98 */
   vis5d_finish_work();

   for (i = 0; i < DisplayRows*DisplayCols; i++){
      dtx = vis5d_get_dtx(i);
      vis5d_draw_frame(dtx->dpy_context_index, 0);
      vis5d_swap_frame(dtx->dpy_context_index);
      XSync( GfxDpy, 0 );
      vis5d_draw_frame(dtx->dpy_context_index, 0);
      vis5d_swap_frame(dtx->dpy_context_index);
      XSync( GfxDpy, 0 );
   }
       
   if (!off_screen_rendering && (( format == VIS5D_PPM && use_convert) ||
       (format != VIS5D_PPM)) && save_3d_window( filename, format )){
      return 0;
   }
#ifdef OPENGL
   else if (format == VIS5D_PPM){
      int x = 0;
      int y = 0;
      for (i = 0; i < DisplayCols; i++){
         dtx = vis5d_get_dtx(i);
         x += dtx->WinWidth;
      }
      for (i = 0; i < DisplayRows; i++){
         dtx = vis5d_get_dtx(i*DisplayCols);
         y += dtx->WinHeight;
      }
      if (!open_ppm_file( filename, x, y)){
         return VIS5D_FAIL;
      }
      for (i = 0; i < DisplayRows*DisplayCols; i++){
         dtx = vis5d_get_dtx(i);
         if (!add_display_to_ppm_file( dtx, i)){
            return VIS5D_FAIL;
         }
      }
      if (!close_ppm_file()){
         return VIS5D_FAIL;
      }
   }
#endif
   else {
      return VIS5D_FAIL;
   }
}

int vis5d_save_snd_window( int index, char *filename, int format )
{
   int back;
   DPY_CONTEXT("vis5d_save_snd_window")

   back = dtx->DisplaySound;

   dtx->DisplaySound = 1;
   if (filename[0]==0) {
      /* no filename! */
      return VIS5D_FAIL;
   }
   vis5d_map_sndwindow( index );
   vis5d_draw_sounding_only( index, 1);
   vis5d_draw_sounding_only( index, 1);

   if (save_snd_window( dtx, filename, format )){
      return 0;
   }
   else {
      return VIS5D_FAIL;
   }
   dtx->DisplaySound = back;
   if (!dtx->DisplaySound){
      vis5d_unmap_sndwindow( index );
   }
   return 0;
}

int vis5d_save_to_v5dfile( int index, char *filename)
{
   CONTEXT("vis5d_save_to_v5dfile")
   if (filename[0]==0) {
      /* no filename! */
      return VIS5D_FAIL;
   }
   if (write_gridfile( ctx, filename )){
      return 0;
   }
   else {
      return VIS5D_FAIL;
   }
}

vis5d_print_window( void )
{
  print_3d_window();
  return 0;
}

vis5d_print_snd_window( int index )
{
   DPY_CONTEXT("vis5d_print_snd_window")
   set_current_window( dtx );
   print_snd_window( dtx );
   return 0;
}

/*** Functions useful for controlling Vis5D via -pipe */
/* get dispay_index by location in BIG 3-D window */
/* WLH 15 Oct 98 */
vis5d_locate_dtx(Window w, int x, int y, int *display_index)
{
   int width, height, i, j;
   Display_Context dtx;

   *display_index = 0;
   if (DisplayRows == 1 && DisplayCols == 1) {
     return 0;
   }

   dtx = dtx_table[0];
   if (w == BigWindow) {
     width = dtx->WinWidth + 8;
     height = dtx->WinHeight + 8;
   
     i = x / width;
     j = y / height;
     if (i > (DisplayCols - 1)) i = DisplayCols - 1;
     if (j > (DisplayRows - 1)) j = DisplayRows - 1;

     /* assume that display contexts are in raster order */
     *display_index = i + j * DisplayRows;
     if (!dtx_table[*display_index]) {
        *display_index = 0;
     }
   }
   else {
     int k;
     for (k=0; k<DisplayRows*DisplayCols; k++) {
       dtx = dtx_table[k];
       if (dtx) {
         if (w == dtx->GfxWindow) {
           *display_index = k;
           break;
         }
       }
     }
   }
   return 0;
}

/* get context_index by name */
vis5d_name_ctx(char *name, int *context_index)
{
   int i;
   for (i=0; i<VIS5D_MAX_CONTEXTS; i++) {
      Context ctx = ctx_table[i];
      if (ctx && strcmp(name, ctx->ContextName) == 0) {
         *context_index = i;
         return 0;
      }
   }
   *context_index = 0;
   return VIS5D_FAIL;
}


/*** Coordinate Conversion Functions ***/

int vis5d_project( int index, float p[3], float *x, float *y )
{
  DPY_CONTEXT("vis5d_project")
  set_current_window( dtx );
  project( p, x, y );
  return 0;
}


int vis5d_unproject( int index, float cursor_x, float cursor_y,
                     float point[3], float dir[3] )
{
  DPY_CONTEXT("vis5d_unproject")
  set_current_window( dtx );
  unproject( cursor_x, cursor_y, point, dir);
  return 0;
}

int vis5d_xyzPRIME_to_gridPRIME(int index, int time, int var,
                       float x, float y, float z,
                       float *row, float *col, float *lev )
{
  DPY_CONTEXT("vis5d_xyzPRIME_to_gridPRIME")
  xyzPRIME_to_gridPRIME(dtx, time, var, x, y, z, row, col, lev);
  return 0;
}

int vis5d_xyz_to_grid( int index, int time, int var,
                       float x, float y, float z,
                       float *row, float *col, float *lev )
{
  CONTEXT("vis5d_xyz_to_grid")
  xyz_to_grid(ctx, time, var, x, y, z, row, col, lev);
  return 0;
}


int vis5d_gridPRIME_to_xyzPRIME( int index, int time, int var,
                       float row, float col, float lev,
                       float *x, float *y, float *z )
{
  float r[1], c[1], l[1];
  DPY_CONTEXT("vis5d_gridPRIME_to_xyzPRIME")
  r[0] = row;
  c[0] = col;
  l[0] = lev;
  gridPRIME_to_xyzPRIME(dtx, time, var, 1, r, c, l, x, y, z);
  return 0;
}

int vis5d_gridPRIME_to_grid( int index, int time, int var,
                             float rPRIME, float cPRIME, float lPRIME,
                             float *r, float *c, float *l)
{
   float rr[1], cc[1], ll[1];
   CONTEXT("vis5d_gridPRIME_to_grid")
   rr[0] = rPRIME;
   cc[0] = cPRIME;
   ll[0] = lPRIME;
   gridPRIME_to_grid(ctx, time, var, 1, rr, cc, ll, r, c, l);
   return 0;
}

int vis5d_grid_to_gridPRIME( int index, int time, int var,
                             float r, float c, float l,
                             float *rPRIME, float *cPRIME, float *lPRIME)
{
   float rr[1], cc[1], ll[1];
   CONTEXT("vis5d_grid_to_gridPRIME")
   rr[0] = r;
   cc[0] = c;
   ll[0] = l;
   grid_to_gridPRIME( ctx, time, var, 1, rr, cc, ll, rPRIME, cPRIME, lPRIME);
   return 0;
}


int vis5d_grid_to_xyz( int index, int time, int var,
                       float row, float col, float lev,
                       float *x, float *y, float *z )
{
  float r[1], c[1], l[1];
  CONTEXT("vis5d_grid_to_xyz")
  r[0] = row;
  c[0] = col;
  l[0] = lev;
  grid_to_xyz(ctx, time, var, 1, r, c, l, x, y, z);
  return 0;
}


int vis5d_xyzPRIME_to_geo( int index, int time, int var,
                      float x, float y, float z,
                      float *lat, float *lon, float *hgt )
{
  DPY_CONTEXT("vis5d_xyzPRIME_to_geo")
  xyzPRIME_to_geo(dtx, time, var, x, y, z, lat, lon, hgt);
  return 0;
}

int vis5d_geo_to_xyzPRIME( int index, int time, int var,
                      float lat, float lon, float hgt, 
                      float *x, float *y, float *z)
{
   float latt[1], lonn[1], hgtt[1];
   DPY_CONTEXT("vis5d_geo_to_xyzPRIME")
   latt[0] = lat;
   lonn[0] = lon;
   hgtt[0] = hgt;
   geo_to_xyzPRIME(dtx, time, var, 1, latt, lonn, hgtt,
                   x, y, z);

   return 0;
} 

int vis5d_xyz_to_geo( int index, int time, int var,
                      float x, float y, float z,
                      float *lat, float *lon, float *hgt )
{
  CONTEXT("vis5d_xyz_to_grid")
  xyz_to_geo(ctx, time, var, x, y, z, lat, lon, hgt);
  return 0;
}



int vis5d_rowcol_to_latlon( int index, int time, int var,
                            float row, float col,
                            float *lat, float *lon)
{
   CONTEXT("vis5d_rowcol_to_latlon")
   rowcol_to_latlon(ctx, time, var, row, col, lat, lon);
   return 0;
}

int vis5d_rowcolPRIME_to_latlon( int index, int time, int var,
                            float row, float col,
                            float *lat, float *lon)
{
   DPY_CONTEXT("vis5d_rowcolPRIME_to_latlon")
   rowcolPRIME_to_latlon( dtx, time, var, row, col, lat, lon);
   return 0;
}


int vis5d_grid_to_geo ( int index, int time, int var,
                        float row, float col, float lev,
                        float *lat, float *lon, float *hgt)
{
   float ro[1], co[1], le[1];

   CONTEXT("vis5d_grid_to_geo")
   ro[0] = row;
   co[0] = col;
   le[0] = lev;
   grid_to_geo( ctx, time, var, 1, ro, co, le, lat, lon, hgt);
   return 0;
}

int vis5d_gridPRIME_to_geo( int index, int time, int var,
                        float row, float col, float lev,
                        float *lat, float *lon, float *hgt)
{
   float ro[1], co[1], le[1];

   DPY_CONTEXT("vis5d_gridPRIME_to_geo")
   ro[0] = row;
   co[0] = col;
   le[0] = lev;
   gridPRIME_to_geo( dtx, time, var, 1, ro, co, le, lat, lon, hgt);
   return 0;
}

int vis5d_latlon_to_rowcol( int index, int time, int var,
                            float lat, float lon,
                            float *row, float *col)
{
   CONTEXT("vis5d_latlon_to_rowcol")
   latlon_to_rowcol( ctx, time, var, lat, lon, row, col);
   return 0;
}


int vis5d_latlon_to_rowcolPRIME( int index, int time, int var,
                            float lat, float lon,
                            float *row, float *col)
{
   DPY_CONTEXT("vis5d_latlon_to_rowcolPRIME")
   latlon_to_rowcolPRIME( dtx, time, var, lat, lon, row, col);
   return 0;
}


int vis5d_geo_to_grid( int index, int time, int var, 
                       float lat, float lon, float hgt,
                       float *row, float *col, float *lev)
{
   float la[1], lo[1], hg[1];
   CONTEXT("vis5d_geo_to_grid")
   la[0] = lat;
   lo[0] = lon;
   hg[0] = hgt;
   geo_to_grid( ctx, time, var, 1, la, lo, hg, row, col, lev);
   return 0;
}

int vis5d_geo_to_gridPRIME( int index, int time, int var,
                       float lat, float lon, float hgt,
                       float *row, float *col, float *lev)
{
   float la[1], lo[1], hg[1];
   DPY_CONTEXT("vis5d_geo_to_gridPRIME")
   la[0] = lat;
   lo[0] = lon;
   hg[0] = hgt;
   geo_to_gridPRIME( dtx, time, var, 1, la, lo, hg, row, col, lev);
   return 0;
}

int vis5d_gridlevelPRIME_to_height( int index, int time, int var,
                             float lev, float *hgt)
{
   DPY_CONTEXT("vis5d_gridlevelPRIME_to_height")

   *hgt = gridlevelPRIME_to_height( dtx, lev);
   return 0;
}

int vis5d_gridlevel_to_height( int index, int time, int var,
                             float lev, float *hgt)
{
   CONTEXT("vis5d_gridlevel_to_height")
   *hgt = gridlevel_to_height( ctx, lev);
   return 0;
}

int vis5d_height_to_gridlevelPRIME( int index, int time, int var,
                             float hgt, float *lev)
{
   DPY_CONTEXT("vis5d_height_to_gridlevelPRIME")

   *lev = height_to_gridlevPRIME( dtx, hgt);
   return 0;
}

int vis5d_height_to_gridlevel( int index, int time, int var,
                             float hgt, float *lev)
{
   CONTEXT("vis5d_height_to_gridlevel")

   *lev = height_to_gridlev( ctx, hgt);
   return 0;
}

int vis5d_geo_to_xyz( int index, int time, int var,
                      float lat, float lon, float hgt,
                      float *x, float *y, float *z )
{
  float la[1], lo[1], hg[1];
  CONTEXT("vis5d_geo_to_xyz")
  la[0] = lat;
  lo[0] = lon;
  hg[0] = hgt;
  geo_to_xyz(ctx, time, var, 1, la, lo, hg, x, y, z);
  return 0;
}


/*** Save and Restore Functions (obsolete in favor of Tcl save/restore) ***/



#ifdef LEAVEOUT
/*
 * Save graphics/settings.
 * Return:  0 = OK
 *          VIS5D_BAD_VALUE = couldn't open output file
 *          VIS5D_FAIL = error while writing file, disk full?
 */
int vis5d_save( int index, char *filename, int gfx, int traj)
{
   CONTEXT("vis5d_save")
   return tclsave( index, filename, gfx, traj );
}
#endif


/*
 * Restore saved graphics/settings.  This restores an old (version 4.1
 * and earlier) binary .SAVE file.  Obsolete in favor of Tcl-based
 * save and restore.
 * Return:  0 = OK
 *          VIS5D_BAD_VALUE = filename not found
 *          VIS5D_FAIL = couldn't restore (bad file??)
 */
int vis5d_restore( int index, char *filename )
{
   CONTEXT("vis5d_restore")

   return restore( ctx, filename );
}


/* MJK 12.02.98 begin */
/*
 * Set flag to indicate that user-provided functions are to be used
 * to read gridded data.
 * Return:  0 = OK
 */
int vis5d_set_user_data_flag (int index, int user_data)
{
   CONTEXT("vis5d_set_user_data_flag")

   ctx->UserDataFlag = user_data;

   return 0;
}

/*
 * Set flags to indicate that user-provided functions are to be used
 * to read map data or topo data.
 * Return:  0 = OK
 */
int vis5d_set_user_flags (int index, int user_topo, int user_maps)
{
   DPY_CONTEXT("vis5d_set_user_flags")

   dtx->UserTopoFlag = user_topo;
   dtx->UserMapsFlag = user_maps;

   return 0;
}
/* MJK 12.02.98 end */


/* MJK 12.02.98 begin */
int vis5d_set_topo_base (int index, int state, float level)
{
   DPY_CONTEXT("vis5d_set_topo_base")

   dtx->DisplayTopoBase = state;
   dtx->TopoBaseLev     = level;
   if (!in_the_init_stage){
      setup_dtx(dtx, index);
   }
   return 0;
}

int vis5d_get_flatmap_level (int index, float *level)
{

   float row, col;

   DPY_CONTEXT("vis5d_get_flatmap_level")

   if (!dtx->MapFlag) return VIS5D_FAIL;

   vis5d_xyzPRIME_to_gridPRIME (index, 0, 0, 0.0, 0.0, dtx->FlatMapVert[0][2],
                      &row, &col, level);

   return 0;
}



int vis5d_set_flatmap_level (int index, float level)
{

   int i;
   float x, y, z;

   DPY_CONTEXT("vis5d_set_flatmap_level")

   if (!dtx->MapFlag) return VIS5D_FAIL;

   vis5d_gridPRIME_to_xyzPRIME (index, -1, -1, 0.0, 0.0, level, &x, &y, &z);
   for (i = 0; i < dtx->VertCount; i++) dtx->FlatMapVert[i][2] = z;

   return 0;
}
/* MJK 12.02.98 end */


/* MJK 12.04.98 begin */
vis5d_init_cloned_var_colortables( int index, int varowner, int var )
{
    int                  var_type, src_var;
    unsigned int        *ctable, *src_ctable;
    float               *parms;
    DPY_CONTEXT("vis5d_init_cloned_var_colortables");

    vis5d_get_var_type (varowner, var, &var_type);
    if (var_type != VIS5D_CLONE) return 0;


    vis5d_get_var_info (varowner, var, &src_var);

    /* Copy source color tables */

    /* Isosurfaces */
    vis5d_get_color_table_params (index, VIS5D_ISOSURF, varowner, src_var,
                                  &parms);
    vis5d_set_color_table_params (index, VIS5D_ISOSURF, varowner, var, parms);
    vis5d_get_color_table_address (index, VIS5D_ISOSURF, varowner, src_var,
                                   &src_ctable);
    vis5d_get_color_table_address (index, VIS5D_ISOSURF, varowner, var,
                                   &ctable);
    memcpy (ctable, src_ctable, (256 * sizeof (unsigned int)));

    /* CHSlices */
    vis5d_get_color_table_params (index, VIS5D_CHSLICE, varowner, src_var,
                                  &parms);
    vis5d_set_color_table_params (index, VIS5D_CHSLICE, varowner, var, parms);
    vis5d_get_color_table_address (index, VIS5D_CHSLICE, varowner, src_var,
                                   &src_ctable);
    vis5d_get_color_table_address (index, VIS5D_CHSLICE, varowner, var,
                                   &ctable);
    memcpy (ctable, src_ctable, (256 * sizeof (unsigned int)));

    /* CVSlices */
    vis5d_get_color_table_params (index, VIS5D_CVSLICE, varowner, src_var,
                                  &parms);
    vis5d_set_color_table_params (index, VIS5D_CVSLICE, varowner, var, parms);
    vis5d_get_color_table_address (index, VIS5D_CVSLICE, varowner, src_var,
                                   &src_ctable);
    vis5d_get_color_table_address (index, VIS5D_CVSLICE, varowner, var,
                                   &ctable);
    memcpy (ctable, src_ctable, (256 * sizeof (unsigned int)));

    /* Volumes */
    vis5d_get_color_table_params (index, VIS5D_VOLUME, varowner, src_var,
                                  &parms);
    vis5d_set_color_table_params (index, VIS5D_VOLUME, varowner, var, parms);
    vis5d_get_color_table_address (index, VIS5D_VOLUME, varowner, src_var,
                                   &src_ctable);
    vis5d_get_color_table_address (index, VIS5D_VOLUME, varowner, var,
                                   &ctable);
    memcpy (ctable, src_ctable, (256 * sizeof (unsigned int)));

    /* Trajectories */
    vis5d_get_color_table_params (index, VIS5D_TRAJ, varowner, src_var,
                                  &parms);
    vis5d_set_color_table_params (index, VIS5D_TRAJ, varowner, var, parms);
    vis5d_get_color_table_address (index, VIS5D_TRAJ, varowner, src_var,
                                   &src_ctable);
    vis5d_get_color_table_address (index, VIS5D_TRAJ, varowner, var,
                                   &ctable);
    memcpy (ctable, src_ctable, (256 * sizeof (unsigned int)));

    /* Topography */
    vis5d_get_color_table_params (index, VIS5D_TOPO, varowner, src_var,
                                  &parms);
    vis5d_set_color_table_params (index, VIS5D_TOPO, varowner, var, parms);
    vis5d_get_color_table_address (index, VIS5D_TOPO, varowner, src_var,
                                   &src_ctable);
    vis5d_get_color_table_address (index, VIS5D_TOPO, varowner, var,
                                   &ctable);
    memcpy (ctable, src_ctable, (256 * sizeof (unsigned int)));

    return 1;
}
/* MJK 12.04.98 end */



int vis5d_enable_sfc_map( int index, int mode )
{
   char *val;
   DPY_CONTEXT("vis5d_enable_sfc_map")

   if ((!dtx->TopoFlag) || (dtx->TopoVertex == NULL )){
      return 0;
   }
   val = &dtx->DisplaySfcMap;
   switch (mode) {
      case VIS5D_OFF:
         if (*val != 0) {
            dtx->Redraw = 1;
            vis5d_invalidate_dtx_frames(index);
         }
         *val = 0;
         break;
      case VIS5D_ON:
         if (*val != 1) {
            dtx->Redraw = 1;
            vis5d_invalidate_dtx_frames(index);
         }
         *val = 1;
         break;
      case VIS5D_TOGGLE:
         *val = *val ? 0 : 1;
         dtx->Redraw = 1;
         vis5d_invalidate_dtx_frames(index);
         break;
      case VIS5D_GET:
         break;
      default:
         printf("bad mode (%d) in vis5d_enable_sfc_map\n", mode);
         return VIS5D_BAD_MODE;
   }
   return *val;
}



/* MJK 12.04.98 begin */
/*
 * Control what surface "data" graphics to display.
 * Input:  index - context index
 *         what - one of VIS5D_HSLICE, VIS5D_HWIND, VIS5D_HSTREAM, or
 *                
 *         number - variable number, or wind slice number
 *                  depending on 'what'.
 *         mode - one of VIS5D_ON, VIS5D_OFF, VIS5D_TOGGLE, or VIS5D_GET
 * Return:  current value after function is applied
 */
int vis5d_enable_sfc_graphics( int index, int type, int number, int mode )
{
   char *val;
   CONTEXT("vis5d_enable_sfc_graphics")

   if ((!ctx->dpy_ctx->TopoFlag) || (ctx->dpy_ctx->TopoVertex == NULL))
      return 0;

   switch (type) {
      case VIS5D_HSLICE:
         val = &ctx->DisplaySfcHSlice[number];
         break;
      case VIS5D_HWIND:
         val = &ctx->dpy_ctx->DisplaySfcHWind[number];
         break;
      case VIS5D_HSTREAM:
         val = &ctx->dpy_ctx->DisplaySfcHStream[number];
         break;
      default:
         return VIS5D_BAD_CONSTANT;
   }
   switch (mode) {
      case VIS5D_OFF:
         if (*val != 0) {
            ctx->dpy_ctx->Redraw = 1;
            vis5d_invalidate_dtx_frames(ctx->dpy_ctx->dpy_context_index);
         }
         *val = 0;
         break;
      case VIS5D_ON:
         if (*val != 1) {
            ctx->dpy_ctx->Redraw = 1;
            vis5d_invalidate_dtx_frames(ctx->dpy_ctx->dpy_context_index);
         }
         *val = 1;
         break;
      case VIS5D_TOGGLE:
         *val = *val ? 0 : 1;
         ctx->dpy_ctx->Redraw = 1;
         vis5d_invalidate_dtx_frames(ctx->dpy_ctx->dpy_context_index);
         break;
      case VIS5D_GET:
         break;
      default:
         printf("bad mode (%d) in vis5d_enable_sfc_graphics\n", mode);
         return VIS5D_BAD_MODE;
   }

   if (mode != VIS5D_GET)
   {
      int       i, numtimes;

      vis5d_get_ctx_numtimes (index, &numtimes);

      switch (type) {
         case VIS5D_HSLICE:
            for (i = 0; i < numtimes; i++)
               ctx->HSliceTable[number][i].valid = 0;
            break;
         case VIS5D_HWIND:
            for (i = 0; i < numtimes; i++)
               ctx->dpy_ctx->HWindTable[number][i].valid = 0;
            break;
         case VIS5D_HSTREAM:
            for (i = 0; i < numtimes; i++)
               ctx->dpy_ctx->HStreamTable[number][i].valid = 0;
            break;
      }
      /*
      enable_linked_sfc_slices (index, type, number, *val);
      */

   }
   return *val;
}
/* MJK 12.04.98 end */

int set_cursor_type (Display *dpy, Window win, int shape)
{
   Cursor cursor;
  
   cursor = XCreateFontCursor (GfxDpy, shape);
   XDefineCursor (dpy, win, cursor);
   XFreeCursor (dpy, cursor);
   return 1;
}


int vis5d_set_busy_cursor( Display *dpy, Window win, int busy)
{
   Cursor cursor;
   
   if (busy){
      set_cursor_type (dpy, win, XC_watch);
   }
   else{
      set_cursor_type(dpy, win, XC_top_left_arrow);
   }
   return 1;
}

/* MJK 12.10.98 begin*/
/*
 * Get the current view matrix's scale factors.
 * Return:  0 = OK
 */
int     vis5d_get_view_scales (int index,
                               float *scalex, float *scaley, float *scalez)
{

   int          i;
   float        xscale, yscale, zscale;
   MATRIX       xform;

   DPY_CONTEXT("vis5d_get_view_scales")



   vis5d_get_matrix (index, xform);

   xscale = yscale = zscale = 0.0;
   for (i = 0; i < 3; i++)
   {
      xscale += xform[0][i] * xform[0][i];
      yscale += xform[1][i] * xform[1][i];
      zscale += xform[2][i] * xform[2][i];
   }

   *scalex = sqrt (xscale);
   *scaley = sqrt (yscale);
   *scalez = sqrt (zscale);


   return 0;
}



/*
 * Set the current view matrix's scale factors.
 * Return:  0 = OK
 */
int     vis5d_set_view_scales (int index,
                               float scalex, float scaley, float scalez)
{

   float        xscale, yscale, zscale;
   MATRIX       xform, mat;

   DPY_CONTEXT("vis5d_set_view_scales")



   if (scalex == 0.0) scalex = 1.0;
   if (scaley == 0.0) scaley = 1.0;
   if (scalez == 0.0) scalez = 1.0;

   vis5d_get_matrix (index, xform);
   vis5d_get_view_scales (index, &xscale, &yscale, &zscale);

   memset (mat, 0, 16*sizeof (float));
   mat[0][0] = (scalex < 0.0) ? -scalex : scalex / xscale;
   mat[1][1] = (scaley < 0.0) ? -scaley : scaley / yscale;
   mat[2][2] = (scalez < 0.0) ? -scalez : scalez / zscale;
   mat[3][3] = 1.0;

   mat_mul (xform, mat, xform);

   vis5d_set_matrix (index, xform);


   return 0;
}



/*
 * Get the amount of exaggeration along the physical (geographic)
 * vertical axis.
 * Return:  0 = OK
 */
int     vis5d_get_vert_exaggeration (int index, float *scalez)
{

   int          nr, nc, proj;
   float        proj_args[MAXPROJARGS];
   float        xscale, yscale, zscale, xmin, ymin, zmin, xmax, ymax, zmax;
   float        hgt_min, hgt_max, rlat, rlon;
   float        xy_km_per_grid, xy_km_per_unit, z_km_per_unit;
   float        xview, yview, zview, xdist, ydist, zdist;

   DPY_CONTEXT("vis5d_get_vert_exaggeration")



   vis5d_get_view_scales (index, &xscale, &yscale, &zscale);

   vis5d_get_box_bounds (index, &xmin, &xmax, &ymin, &ymax, &zmin, &zmax);
   xview = (xmax - xmin) * xscale;
   yview = (ymax - ymin) * yscale;
   zview = (zmax - zmin) * zscale;

   vis5d_get_dtx_projection (index, &proj, proj_args);
   switch (proj)
   {
      case PROJ_LAMBERT:

         xy_km_per_grid = proj_args[5];
         break;

      case PROJ_STEREO:

         xy_km_per_grid = proj_args[4];
         break;

      case PROJ_LINEAR:
      case PROJ_CYLINDRICAL:
      case PROJ_SPHERICAL:
      case PROJ_ROTATED:

         xy_km_per_grid = proj_args[2] * 111.0;
         break;

      default:

         *scalez = -1.0;

         return 0;
   }

   vis5d_get_size (index, &nr, &nc, NULL, NULL, NULL, NULL, NULL, NULL);
   xdist = ((float) nc - 1) * xy_km_per_grid;
   ydist = ((float) nr - 1) * xy_km_per_grid;

   vis5d_xyz_to_geo (index, -1, -1, xmin, ymin, zmin, &rlat, &rlon, &hgt_min);
   vis5d_xyz_to_geo (index, -1, -1, xmax, ymax, zmax, &rlat, &rlon, &hgt_max);
   zdist = hgt_max - hgt_min;

   xy_km_per_unit = sqrt ((xdist * xdist) + (ydist * ydist)) /
                    sqrt ((xview * xview) + (yview * yview));
   z_km_per_unit  = zdist / zview;


   *scalez = xy_km_per_unit / z_km_per_unit;


   return 0;
}



/*
 * Set the amount of exaggeration along the physical (geographic)
 * vertical axis.
 * Return:  0 = OK
 */
int     vis5d_set_vert_exaggeration (int index, float scalez)
{

   float        zscale;

   DPY_CONTEXT("vis5d_set_vert_exaggeration")



   if (scalez > 0.0)
   {
      vis5d_get_vert_exaggeration (index, &zscale);
      scalez /= zscale;
   }

   vis5d_set_view_scales (index, -1.0, -1.0, -scalez);


   return 0;
}

/* from volume.c */
struct volume {
   int     dir;         /* Direction of slices */
   int     valid;       /* Valid flag */
   int     slices;      /* Number of slices */
   int     rows, cols;  /* Size of each slice */
   float   *vertex;     /* slice vertices stored as: */
                        /*    vertex[slices][rows][cols][3] */
   uint_1  *index;      /* color table index in [0,255] */
};



int vis5d_set_all_invalid (int index)
{

   int          var, time;
   CONTEXT("vis5d_set_all_invalid")



   for (var=0;var<MAXVARS;var++) {
      for (time=0;time<MAXTIMES;time++) {
         ctx->SurfTable[var][time].valid = 0;
         ctx->HSliceTable[var][time].valid = 0;
         ctx->VSliceTable[var][time].valid = 0;
         ctx->CHSliceTable[var][time].valid = 0;
         ctx->CVSliceTable[var][time].valid = 0;
      }
   }

   for (var=0;var<VIS5D_WIND_SLICES;var++) {
      for (time=0;time<MAXTIMES;time++) {
         ctx->dpy_ctx->HWindTable[var][time].valid = 0;
         ctx->dpy_ctx->VWindTable[var][time].valid = 0;
         ctx->dpy_ctx->HStreamTable[var][time].valid = 0;
         ctx->dpy_ctx->VStreamTable[var][time].valid = 0;
      }
   }

   if (ctx->Volume != NULL) ctx->Volume->valid = 0;


   return 0;
}

int vis5d_set_all_irregular_invalid( int index )
{
   int time;
   IRG_CONTEXT("vis5d_set_all_irregular_invalid");

   for (time = 0; time < MAXTIMES; time++){
      itx->TextPlotTable[time].valid = 0;
   }
   return 0;
}


/**************************************************/
/*                  New 5.2 stuff                 */
/*  |       |       |          |       |     |    */
/* \|/     \|/     \|/        \|/     \|/   \|/   */
/**************************************************/

static int add_itx_index_to_dtx( int index, int index_of_itx)
{
   int yo;
   int ontinue;
   Irregular_Context itx;
   DPY_CONTEXT("add_itx_index_to_dtx")

   ontinue = 1;
   for (yo=0; yo<dtx->numofitxs; yo++){
      if (dtx->itxarray[yo] == index_of_itx){
         ontinue = 0;
      }
   }
   if (ontinue){
      dtx->numofitxs += 1;
      dtx->itxarray[dtx->numofitxs-1] = index_of_itx;
      dtx->itxpointerarray[dtx->numofitxs-1] = vis5d_get_itx( index_of_itx );
   }
   return 0;
}

static int remove_itx_index_from_dtx( int index, int index_of_itx)
{
   int yo, yo2, ontinue;
   DPY_CONTEXT("remove_itx_index_from_dtx")

   ontinue = 0;
   for (yo=0; yo<dtx->numofitxs; yo++){
      if (dtx->itxarray[yo] == index_of_itx){
         ontinue = 1;
         yo2 = yo;
      }
   }
   if (ontinue){
      for (yo = yo2; yo < dtx->numofitxs-1; yo++){
         dtx->itxarray[yo] = dtx->itxarray[yo+1];
         dtx->itxpointerarray[yo] = dtx->itxpointerarray[yo+1];
      }
      dtx->numofitxs -= 1;
      if (dtx->numofitxs>0){
         calculate_display_time_steps( dtx);
      }
   }
   return 0;
}

int vis5d_assign_display_to_irregular_data( int index, int display_index)
{
   int i, yo;
   Display_Context dtx;
   IRG_CONTEXT("vis5d_assign_display_to_irregular_data")

   ungroup_all_displays();
   dtx = vis5d_get_dtx(display_index);
   if (itx->dpy_ctx){
      remove_itx_index_from_dtx( itx->dpy_ctx->dpy_context_index, itx->context_index);
   }
   itx->dpy_ctx = dtx;
   add_itx_index_to_dtx( display_index, index);
   calculate_display_time_steps( dtx );
   if (dtx->numofitxs > 0){
      itx->CurTime = 0;
      dtx->CurTime = 0;
      vis5d_signal_redraw( display_index, 1 );
   }

   if (dtx->numofitxs > 1){
      memset( itx->TextPlotTable, 0, sizeof(itx->TextPlotTable) );
   }

   return 1;
}

int vis5d_alloc_irregular_data_context( void )
{
   int i;

   for (i=0;i<VIS5D_MAX_CONTEXTS;i++) {
      if (itx_table[i]==NULL) {
         return i;
      }
   }

   return VIS5D_FAIL;
}

int vis5d_load_irregular_v5dfile( int dindex, int mbs, char *filename, char *ctxname ){
   Irregular_Context itx;
   int i, yo, index;
   int dnumber;
   int j, k, l;

   index = vis5d_alloc_irregular_data_context();
   itx = itx_table[index] = new_irregular_context();
   init_irregular_context( itx );
   itx->context_index = index;


   vis5d_init_irregular_memory( index, mbs );

   if (!vis5d_open_recordfile( index, filename, ctxname, 1)){
      if (noexit){
         return VIS5D_FAIL;
      }
      else{
         vis5d_terminate(1);
         exit(0);
      }
   }

   vis5d_get_num_of_data_sets_in_display( dindex, &dnumber);
   if (dnumber < 1){
      vis5d_init_display_values ( -1, index, dindex);
      init_anim( itx->dpy_ctx);
   }
   else{
      vis5d_assign_display_to_irregular_data( index, dindex);
   }
   if (vis5d_init_irregular_data_end( index)<0){
      printf("Error in vis5d_init_irregular_data_end\n");
      vis5d_terminate(1);
      exit(0);
   }
   for (i = 0; i < itx->NumVars; i++){
      init_irregular_var_clrtable(dindex, index, i);
   }
/*
   for (i = 0; i < itx->MaxCachedTimes; i++){
      for (j = 0; j < 10; j++){
         printf("Record = %d\n", j);
         for (k = 0; k < itx->NumVars; k++){
            printf("   VarName = %s", itx->VarName[k]);
            if (itx->VarType[k] == NUMERICAL_VAR_1D){
               if (IS_MISSING(itx->IrregularCache[i].Record[j].Value[k])){
                  printf(" FloatValue = MISSING\n");
               }
               else{
                  printf(" FloatValue = %f\n", itx->IrregularCache[i].Record[j].Value[k]);
               }
            }
            else if (itx->VarType[k] == NUMERICAL_VAR_2D){
               for (l = itx->SoundingPointer[k]; l < itx->SoundingPointer[k]+itx->Levels; l++){
                  if (IS_MISSING(itx->IrregularCache[i].Record[j].SoundingValue[l])){
                     printf(" SoundingValue[%d] = MISSING\n", l-itx->SoundingPointer[k]);
                  }
                  else{
                     printf(" SoundingValue[%d] = %f\n", l-itx->SoundingPointer[k],
                     itx->IrregularCache[i].Record[j].SoundingValue[l]);
                  }
               }
            }
            else{
               printf(" CharValue = ");
               for (l = itx->CharPointer[k]; l < itx->CharPointer[k]+itx->CharVarLength[k]; l++){
                  printf("%c", itx->IrregularCache[i].Record[j].CharData[l]);
               }
               printf("\n");
            }
         }
      }
   }
   */

   return index;
}


int vis5d_get_num_of_itxs_in_display( int index, int *number, int numarray[])
{
   int yo;
   Display_Context dtx;

   if (index<0 || index>=VIS5D_MAX_DPY_CONTEXTS || (dtx = dtx_table[index])==NULL){
      *number = 0;
      return -1;
   }
   *number = dtx->numofitxs;
   for (yo=0; yo < *number; yo++){
      numarray[yo] = dtx->itxarray[yo];
   }
   return 0;
}

int vis5d_get_num_of_data_sets_in_display( int index, int *number)
{
   int yo;
   Display_Context dtx;

   if (index<0 || index>=VIS5D_MAX_DPY_CONTEXTS || (dtx = dtx_table[index])==NULL){
      *number = 0;
      return -1;
   }
   *number = dtx->numofctxs + dtx->numofitxs;
   return 0;
}

int vis5d_init_irregular_memory( int index, int mbs )
{
   IRG_CONTEXT("vis5d_init_irregular_memory");
   itx->MegaBytes = mbs;
   return 0;
}


int vis5d_initialize_irregular_stuff( int index)
{
   Irregular_Context itx;
   int chowmany, cwhichones[VIS5D_MAX_CONTEXTS], cyo, ind;
   DPY_CONTEXT("vis5d_initialize_irregular_stuff")

   vis5d_get_num_of_itxs_in_display( index, &chowmany, cwhichones);
   for ( cyo = 0; cyo < chowmany; cyo++) {
     ind = cwhichones[cyo];
     if (ind<0 || ind>=VIS5D_MAX_CONTEXTS || (itx = itx_table[ind])==NULL) {
       printf("bad context in vis5d_initialize_stuff\n");
       return VIS5D_BAD_CONTEXT;
     }
     initialize_irregular_stuff(itx);
   }

   return 0;
}

int vis5d_init_irregular_data_end( int index )
{
   int i, yo;
   int memsize;
   float ratio;
   IRG_CONTEXT("vis5d_init_irregular_data_end")

   /*** Memory setup ***/
   if (itx->MegaBytes==0) {
      /* use malloc/free */
      if (!init_irregular_memory( itx, 0 )) {
         return VIS5D_FAIL;
      }
      memsize = 0;
   }
   else {
      /* Use bounded memory management */
      if (itx->MegaBytes<10) {
         itx->MegaBytes = 10;
      }
      /* use 80% of MegaBytes */
      memsize = (int) ( (float) itx->MegaBytes * 0.80 ) * MEGA;
      if (!init_irregular_memory( itx, memsize )) {
         return VIS5D_FAIL;
      }
   }
   /* Let 2/5 of the memory pool be used for caching grid data. */
   if (memsize==0) {
      /* Grid cache size = 100MB */
      if (!init_record_cache( itx, 100*1024*1024, &ratio )) {
         return VIS5D_OUT_OF_MEMORY;
      }
   }
   else {
      if (!init_record_cache( itx, memsize * 2 / 5, &ratio )) {
         return VIS5D_OUT_OF_MEMORY;
      }
   }
   /* Read some or all of the grid data into main memory now.  If we */
   /* have enough memory, the whole file will be loaded.  Otherwise, */
   /* an arbitrary set of records will be loaded. */


   if (itx->PreloadCache) {
      preload_irregular_cache(itx);
   }
 
   load_geo_data(itx);
  
   if (memsize!=0) {
      /* check if there's enough memory left after loading the data set */
      int min_mem = MAX( memsize/3, 3*MEGA );
      if (i_mem_available(itx)<min_mem) {
         printf("Not enough memory left for graphics (only %d bytes free)\n",
                i_mem_available(itx));
         return VIS5D_OUT_OF_MEMORY;
      }
   }

   /* There is no Verylarge option now since there is only one type of graphic 

   itx->VeryLarge = (ratio < VERY_LARGE_RATIO);
   if (itx->VeryLarge) printf("Using VeryLarge option - graphics may be slow\n");

   */

   /*** Create threads ***/
#ifdef sgi
   if (NumThreads>1 && WorkerPID[0]==0) {
      /* Fork off the worker threads if we haven't already */
      if (NumThreads>1)   WorkerPID[0] = sproc( work, PR_SALL, 1 );
      if (NumThreads>2)   WorkerPID[1] = sproc( work, PR_SALL, 2 );
      if (NumThreads>3)   WorkerPID[2] = sproc( work, PR_SALL, 3 );
      if (NumThreads>4)   WorkerPID[3] = sproc( work, PR_SALL, 4 );
      if (NumThreads>5)   WorkerPID[4] = sproc( work, PR_SALL, 5 );
      if (NumThreads>6)   WorkerPID[5] = sproc( work, PR_SALL, 6 );
      if (NumThreads>7)   WorkerPID[6] = sproc( work, PR_SALL, 7 );
      if (NumThreads>8)   WorkerPID[7] = sproc( work, PR_SALL, 8 );
   }
#endif
#ifdef sunos5
   if (Threadsers>1 && WorkerPID[0]==0) {
      if (NumThreads>1)   thr_create( NULL, 0, work, 1, 0, &WorkerPID[0] );
      if (NumThreads>2)   thr_create( NULL, 0, work, 2, 0, &WorkerPID[1] );
      if (NumThreads>3)   thr_create( NULL, 0, work, 3, 0, &WorkerPID[2] );
      if (NumThreads>4)   thr_create( NULL, 0, work, 4, 0, &WorkerPID[3] );
   }
#endif

#ifdef LTHREADS
   if (Threadsers>1 && WorkerPID[0]==0) {
      if (NumThreads>1)   pthread_create( &WorkerPID[0], NULL, work, 1 );
      if (NumThreads>2)   pthread_create( &WorkerPID[1], NULL, work, 2 );
      if (NumThreads>3)   pthread_create( &WorkerPID[2], NULL, work, 3 );
      if (NumThreads>4)   pthread_create( &WorkerPID[3], NULL, work, 4 );
   }
#endif
   return 1;
}


int vis5d_open_recordfile( int index, char *name, char *itxname, int read_flag )
{
   IRG_CONTEXT("vis5d_open_recordfile");


   itx->PreloadCache = read_flag;
   if (!open_recordfile( itx, name)){
      return 0;
   }
   strcpy( itx->ItxName, itxname);
   return 1;
}

int vis5d_get_itx_numtimes( int index, int *numtimes )
{
   IRG_CONTEXT("vis5d_get_itx_numtimes")
    *numtimes = itx->NumTimes;
    return 0;
}

int vis5d_get_itx_time_stamp( int index, int timestep, int *day, int *time )
{
  IRG_CONTEXT("vis5d_get_itx_time_stamp")
  if (timestep<0 || timestep>=itx->NumTimes) {
     return VIS5D_BAD_TIME_STEP;
  }
  else {
     *day = itx->DayStamp[timestep];
     *time = itx->TimeStamp[timestep];
     return 0;
  }
}

int vis5d_get_itx_var_name( int index, int var, char *name )
{
   IRG_CONTEXT("vis5d_get_itx_var_name");

   if (var>=0 && var<itx->NumVars) {
      strcpy( name, itx->VarName[var] );
      return 0;
   }
   else {
      return VIS5D_BAD_VAR_NUMBER;
   }
}

int vis5d_get_itx_name( int index, char *name )
{
   IRG_CONTEXT("vis5d_get_itx_file_name");

   strcpy( name, itx->ItxName);
   return 0;
}

int vis5d_get_itx_numvars( int index, int *numvars )
{
   IRG_CONTEXT("vis5d_get_itx_numvars");
   if (itx){
      *numvars = itx->NumVars;
   }
   else{
      *numvars = 0;
   }
   return 0;
}

int vis5d_set_text_plot( int index, int var, float spacing,
                         float fontx, float fonty, float fontspace)
{
   int i;
   IRG_CONTEXT("vis5d_set_text_plot_var");
   
   if (var != itx->TextPlotVar){
      for (i = 0; i < MAXTIMES; i++){
         free_textplot( itx, i);
         itx->TextPlotTable[i].valid = 0;
      }
   }
   itx->TextPlotVar = var;
   itx->TextPlotSpacing = spacing;
   itx->TextPlotFontX = fontx;
   itx->TextPlotFontY = fonty;
   itx->TextPlotFontSpace = fontspace;

   return 0;
}

int vis5d_get_text_plot( int index, int *var, float *spacing,
                         float *fontx, float *fonty, float *fontspace)
{
   IRG_CONTEXT("vis5d_get_text_plot_var");

   *var = itx->TextPlotVar;
   *spacing = itx->TextPlotSpacing;
   *fontx = itx->TextPlotFontX;
   *fonty = itx->TextPlotFontY;
   *fontspace = itx->TextPlotFontSpace;

   return 0;
}

int vis5d_make_text_plot( int index, int time, int urgent)
{
   IRG_CONTEXT("vis5d_make_text_plot");

   if (itx->TextPlotVar >= 0){
      request_text_plot( itx, time, itx->TextPlotVar, urgent);
   }
   return 0;
}

int vis5d_invalidate_text_plot( int index, int time)
{
   IRG_CONTEXT("vis5d_invalidate_text_plot");
   
   free_textplot( itx, time);
   itx->TextPlotTable[time].valid = 0;
   return 0;
}

int vis5d_get_textplot_color_status( int index, int var, int *status)
{
   IRG_CONTEXT("vis5d_get_textplot_color_status");

   *status = itx->TextPlotColorStatus[var];
   return 0;
}

int vis5d_set_textplot_color_status( int index, int var, int status)
{
   int i;
   IRG_CONTEXT("vis5d_set_textplot_color_status");

   if (itx->TextPlotColorStatus[var] != status){
      for (i = 0; i < itx->NumTimes; i++){
         free_textplot( itx, i);
         itx->TextPlotTable[i].valid = 0;
      }
      itx->TextPlotColorStatus[var] = status;
   }
   return 0;
}

int vis5d_get_itx_var_range( int index, int var, float *min, float *max )
{
  IRG_CONTEXT("vis5d_get_itx_var_range")
   if (var>=0 && var<itx->NumVars) {
      *min = itx->MinVal[var];
      *max = itx->MaxVal[var];
      return 0;
   }
   else {
      return VIS5D_BAD_VAR_NUMBER;
   }
}

int vis5d_get_itx_display_index(int index, int *display_index)
{
   Irregular_Context itx;

   if (index<0 || index>=VIS5D_MAX_CONTEXTS || (itx = itx_table[index])==NULL) {

       *display_index = -1;
       return -1;
   }
   *display_index = itx->dpy_ctx->dpy_context_index;
   return 1;
}

int vis5d_enable_irregular_graphics( int index, int type, int mode )
{
   int *val;
   IRG_CONTEXT("vis5d_enable_irregular_graphics");

   switch(type){
      case VIS5D_TEXTPLOT:
         val = &itx->DisplayTextPlot;
         break;
    default:
      return VIS5D_BAD_CONSTANT;
  }
  switch (mode) {
    case VIS5D_OFF:
      if (*val != 0) {
        itx->dpy_ctx->Redraw = 1;
        vis5d_invalidate_dtx_frames(itx->dpy_ctx->dpy_context_index);
      }
      *val = 0;
      break;
    case VIS5D_ON:
      if (*val != 1) {
        itx->dpy_ctx->Redraw = 1;
        vis5d_invalidate_dtx_frames(itx->dpy_ctx->dpy_context_index);
      }
      *val = 1;
      break;
    case VIS5D_TOGGLE:
      *val = *val ? 0 : 1;
      itx->dpy_ctx->Redraw = 1;
      vis5d_invalidate_dtx_frames(itx->dpy_ctx->dpy_context_index);
      break;
    case VIS5D_GET:
      break;
    default:
      printf("bad mode (%d) in vis5d_enable_irregular_graphics\n", mode);
      return VIS5D_BAD_MODE;
  }
  return *val;
}

int vis5d_destroy_irregular_data_context( int index )
{
   Irregular_Context itx;
   Display_Context dtx;
   int dindex;

   if (itx_table[index]){
      itx = itx_table[index];
      if(!itx->dpy_ctx){
         destroy_irregular_context( itx_table[index] );
         itx_table[index] = NULL;
      }
      else{
         dtx = itx->dpy_ctx;
         if (dtx->numofitxs >1){
            remove_itx_index_from_dtx(dtx->dpy_context_index, index);
            if (dtx->ctxarray[0]==index){
               /* New 5.2 */
               vis5d_init_display_values( -1, dtx->itxarray[0], dtx->dpy_context_index);
            }
         }
         else{
            vis5d_reset_display_context( dtx->dpy_context_index );
         }
         destroy_irregular_context( itx_table[index] );
         itx_table[index] = NULL;
      }
   }



   return 0;
}

