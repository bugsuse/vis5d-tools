/*  gui.c */

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
#include <X11/Xlib.h>
#include <X11/keysym.h>
#include <X11/cursorfont.h>
#include <unistd.h>
#include "../lui5/lui.h"
#include "../lui5/pixmaps.h"
#include "analysis.h"
#include "api.h"
#include "cursor.h"
#include "displaywidget.h"
#include "graphics.h"   /*TODO: remove this!!!! */

#include "gui.h"
#include "labels.h"
#include "slice.h"
#include "script.h"
#include "tclsave.h"
#include "main_i.h"
#include "imain.h"
#include "igui.h"

#include "rgbsliders.h"
#include "isocolor.h"
#include "uvwwidget.h"
#include "mwmborder.h"
#include "sounding.h"
#include "soundingGUI.h"

#define MM_NORMAL 1
#define MM_TRAJ   2
#define MM_LABEL  3
#define MM_SLICE  4
#define MM_PROBE  5
#define MM_SOUND  6
#define MM_CLIP   7
#define MM_MAX    69

static GuiContext gtx_table[VIS5D_MAX_CONTEXTS];

/*****************************************************************/
/* New 5.0 stuff that may or may not be relavent later           */
/*****************************************************************/
static int keystatus = 0;
static int currentkey= 0;
static int key1status = 0;
static int key2status = 0;
static int key3status = 0;
static int key4status = 0;
static int key5status = 0;
static int key6status = 0;
static int key7status = 0;
static int key8status = 0;
static int key9status = 0;
static Window keywin = NULL;
static LUI_NEWLABEL *keylabel = NULL;
static LUI_NEWBUTTON *keyexit = NULL;
static int group_event( int gindex, int type, int status );
static int do_one_time[VIS5D_MAX_DPY_CONTEXTS];
static do_once = 1;
int top_margin = 0;
int bottom_margin = 0;
int left_margin = 0;
int right_margin = 0;
char title_string[MAX_TITLES][200];
int title_x_position[MAX_TITLES];
int title_y_position[MAX_TITLES];
char title_font[MAX_TITLES][200];
int number_of_titles = 0;
int redo_the_gui = 0;

/* WLH 11 Nov 98 */
int redo_this_gui[VIS5D_MAX_DPY_CONTEXTS];

char the_gui_dpy_name[1000];



/* MJK 12.04.98 */
int update_linked_slices_sliders (int index, int type, int num);

int set_window_decor (Display *dpy, Window win);

int set_window_decor_all (int index);


/* MJK 12.04.98 begin */
int cb_chvar[VIS5D_MAX_DPY_CONTEXTS];
int cb_chvindex[VIS5D_MAX_DPY_CONTEXTS];
int cb_chvvar[VIS5D_MAX_DPY_CONTEXTS];
int cb_dindex = 0;

/* MJK 12.07.98 */
static int mode_cb( LUI_BUTTON *pb );
static int reverse_cb( LUI_NEWBUTTON *pb );
int hide_colorbar_window( int index );
int hide_chcolorbar_window( int index );
static void map_map_window (int index, int state);
static int tp_close_color_cb( LUI_NEWBUTTON *b);
static int tp_monocolor_cb( LUI_NEWBUTTON *b);
static int tp_multicolor_cb( LUI_NEWBUTTON *b);
int tp_dindex = 0;

/* MJK 12.04.98 begin */
void mod_vpos_slider (int index, LUI_NEWSLIDER *s, int ivar,
                             int is_wind, float lev)
{
   float        xmin, xmax, ymin, ymax, zmin, zmax, hgtmin, hgtmax;
   float        hgt, x, y, z, lat, lon, vertargs[MAXVERTARGS];
   int          itime, vertsys, itop, ibot, nr, nc;
   char         label[20];
   GuiContext   gtx = get_gui_gtx (index);

   if (is_wind)
   {
      int       u1, v1, w1, u2, v2, w2, ut, vt, wt;
      vis5d_get_wind_vars (index, &gtx->u1owner, &u1, &gtx->v1owner, &v1,
                                  &gtx->w1owner, &w1, &gtx->u2owner, &u2,
                                  &gtx->v2owner, &v2, &gtx->w2owner, &w2,
                                  &gtx->tuowner, &ut, &gtx->tvowner, &vt,
                                  &gtx->twowner, &wt);
      ivar = (ivar == 0) ? u1 : u2;
   }
   if (ivar < 0) ivar = 0;

   vis5d_get_dtx_timestep (index, &itime);

   vis5d_gridPRIME_to_xyzPRIME (index, itime, ivar, 0.0, 0.0, lev, &x, &y, &z);
   vis5d_xyzPRIME_to_geo  (index, itime, ivar, x, y, z, &lat, &lon, &hgt);

   vis5d_get_box_bounds (index, &xmin, &xmax, &ymin, &ymax, &zmin, &zmax);
   vis5d_xyzPRIME_to_geo (index, itime, ivar, xmin, ymin, zmin, &lat, &lon, &hgtmin);
   vis5d_xyzPRIME_to_geo (index, itime, ivar, xmax, ymax, zmax, &lat, &lon, &hgtmax);

   label[0] = '\0';
   if (s == gtx->map_pos_slider) strcat (label, "Map ");

   vis5d_get_dtx_vertical (index, &vertsys, vertargs);
   if (vertsys == VERT_NONEQUAL_MB)
   {
      strcat (label, "Level");
      LUI_NewSliderChange (s, label, "mb",
                           height_to_pressure (hgtmax),
                           height_to_pressure (hgtmin),
                           height_to_pressure (hgt));
   }
   else
   {
      strcat (label, "Height");
      LUI_NewSliderChange (s, label, "km", hgtmin, hgtmax, hgt);
   }
}

int check_map_color (int index)
{
   GuiContext   gtx = get_gui_gtx (index);
   int          cur_map;
   float        bg, r, g, b, a;
   int vindex = gtx->array_of_ctxs[0];



   if (!vis5d_graphics_mode (index, VIS5D_MAP, VIS5D_GET)) return 0;

   bg = vis5d_graphics_mode (index, VIS5D_REVERSE, VIS5D_GET);

   cur_map = -1;
   if (vis5d_enable_sfc_map (index, VIS5D_GET)) {
      if (!vis5d_graphics_mode (index, VIS5D_TOPO, VIS5D_GET)) {
         vis5d_get_color (index, VIS5D_DARK_MAP, 0, &r, &g, &b, &a);
         if ((r == bg) && (g == bg) && (b == bg)) cur_map = VIS5D_DARK_MAP;
      }
   }
   else {
      vis5d_get_color (index, VIS5D_LIGHT_MAP, 0, &r, &g, &b, &a);
      if ((r == bg) && (g == bg) && (b == bg)) cur_map = VIS5D_LIGHT_MAP;
   }

   if (cur_map != -1) {
      int       cur_rgb, cur_var;
      float     fg;

      fg = 1.0 - bg;

      vis5d_set_color (index, cur_map, 0, fg, fg, fg, 1.0);
      LUI_ButtonColor (gtx->map_button, fg, fg, fg);

      get_current_rgbsliders (gtx, &cur_rgb, &cur_var);
      if (cur_rgb == cur_map) refresh_rgb_sliders2 (gtx);
   }

   vis5d_signal_redraw (gtx->context_index, 1);

   return 0;
}
/* MJK 12.04.98 end */

GuiContext get_gui_gtx( int index )
{
  GuiContext gtx;
  if ((gtx = gtx_table[index]) == NULL || index<0 || index>=VIS5D_MAX_CONTEXTS) {
    printf("bad gui_context\n");
    return NULL;
  }
  return gtx;
}

GuiContext get_gui_gtx2( int index )
{
  GuiContext gtx;
  if ((gtx = gtx_table[index]) == NULL || index<0 || index>=VIS5D_MAX_CONTEXTS) {
    return NULL;
  }
  return gtx;
}

/*
 * Deallocate a context and everything it points to.
 */
void destroy_gui_context( GuiContext gtx )
{
   if (gtx->fakewin){   
      XDestroyWindow( GuiDpy, gtx->fakewin);
   }   
   if (gtx->CloneWindow){
      XDestroyWindow( GuiDpy, gtx->CloneWindow);
   }
   if (gtx->IsoWindow){   
      XDestroyWindow( GuiDpy, gtx->IsoWindow);
   }      
   if (gtx->HSliceWindow){   
      XDestroyWindow( GuiDpy, gtx->HSliceWindow);
   }      
   if (gtx->VSliceWindow){   
      XDestroyWindow( GuiDpy, gtx->VSliceWindow);
   }      
   if (gtx->WindWindow){
      XDestroyWindow( GuiDpy, gtx->WindWindow);
   }   
   if (gtx->ExprWindow){   
      XDestroyWindow( GuiDpy, gtx->ExprWindow);
   }      
   if (gtx->SaveWindow){   
      XDestroyWindow( GuiDpy, gtx->SaveWindow);
   }      
   if (gtx->SaveFileWindow){   
      XDestroyWindow( GuiDpy, gtx->SaveFileWindow);
   }      
   if (gtx->RestoreWindow){
      XDestroyWindow( GuiDpy, gtx->RestoreWindow);
   }   
   if (gtx->SavePicWindow){
      XDestroyWindow( GuiDpy, gtx->SavePicWindow);
   }      
   if (gtx->TrajWindow){   
      XDestroyWindow( GuiDpy, gtx->TrajWindow);
   }         
   if (gtx->sm1){   
      XDestroyWindow( GuiDpy, gtx->sm1);
   }         
   if (gtx->sm2){
      XDestroyWindow( GuiDpy, gtx->sm2);
   }      
   if (gtx->sm3){
      XDestroyWindow( GuiDpy, gtx->sm3);
   }      
   if (gtx->SoundCtrlWindow){
      XDestroyWindow( GuiDpy, gtx->SoundCtrlWindow);
   }
   if (gtx->VerifyWindow){   
      XDestroyWindow( GuiDpy, gtx->VerifyWindow);
   }         
   if (gtx->AlertWindow){   
      XDestroyWindow( GuiDpy, gtx->AlertWindow);
   }         
   if (gtx->GroupWindow){   
      XDestroyWindow( GuiDpy, gtx->GroupWindow);
   }         
   if (gtx->rgb_sliders_window){
      XDestroyWindow( GuiDpy, gtx->rgb_sliders_window);
   }      
   if (gtx->isocolor_subwin1){
      XDestroyWindow( GuiDpy, gtx->isocolor_subwin1);
   }      
   if (gtx->isocolor_subwin2){
      XDestroyWindow( GuiDpy, gtx->isocolor_subwin2);
   }      
   if (gtx->error_window){   
      XDestroyWindow( GuiDpy, gtx->error_window);
   }         
   if (gtx->isocolor_window){
      XDestroyWindow( GuiDpy, gtx->isocolor_window);
   }
   if (gtx->display_window){   
      XDestroyWindow( GuiDpy, gtx->display_window);
   }
   if (gtx->uvw_window){   
      XDestroyWindow( GuiDpy, gtx->uvw_window);
   }            
   if (gtx->CpWindow){
      XDestroyWindow( GuiDpy, gtx->CpWindow);
   }
   free( gtx );
}


/*
 * Allocate a new vis5d context, initialize to default values.
 */
GuiContext create_gui_context( int index )
{
  GuiContext gtx;
  static int first_time = 1;

  if (first_time) {
    int i;
    for (i=0;i<VIS5D_MAX_CONTEXTS;i++) {
      gtx_table[i] = NULL;
    }
    first_time = 0;
  }

  if (gtx_table[index]) {
    destroy_gui_context( gtx_table[index] );
    gtx_table[index] = NULL;
  }

  gtx = gtx_table[index] = (GuiContext) calloc(1, sizeof(struct gui_context));
  if (gtx) {
    /* init */
    memset( gtx, 0, sizeof(struct gui_context) );
    gtx->context_index = index;
  }
  return gtx;
}



static int cb_graphic[VIS5D_MAX_DPY_CONTEXTS];
static int cb_var[VIS5D_MAX_DPY_CONTEXTS];
static int cb_vindex[VIS5D_MAX_DPY_CONTEXTS];
static int cb_vvar[VIS5D_MAX_DPY_CONTEXTS];

Display *GuiDpy;          /* The X display for the GUI */
int GuiScr;               /* The X screen number for the GUI */
Visual *GuiVisual;        /* The X visual for the GUI */
Colormap GuiColormap;     /* The X colormap for the GUI */
int GuiDepth;             /* The depth of the GUI visual */
Window GuiBigWin;        /* Big Fake Win for seperate dpy */
int GuiBigWinWidth;
int GuiBigWinHeight;
int Current_GuiBigWin_Width;
int Current_GuiBigWin_Height;
int Current_GuiBigWin_x;
int Current_GuiBigWIn_y;
int Being_Resized_From_X_Loop;


#define BW 6
#define BUTTONSIZE 93   /* Width of regular buttons */
#define RADIOSIZE 120   /* Width of radio buttons */

/* depends on window border size: */
#ifdef stellar
#  define SPACE 15
#else
#  define SPACE 40
#endif


/*
 * List of external functions and expressions per context.
 */
static int NumFuncs[VIS5D_MAX_DPY_CONTEXTS];
static int FuncType[VIS5D_MAX_DPY_CONTEXTS][MAX_FUNCS];
static char FuncName[VIS5D_MAX_DPY_CONTEXTS][MAX_FUNCS][1000];
static int FuncOwner[VIS5D_MAX_DPY_CONTEXTS][VIS5D_MAX_CONTEXTS];


static char *Copyright[] = {
   " Vis5D version 5.2  Copyright (C) 1990 - 2000 ",
   "    Bill Hibbard, Johan Kellum, Brian Paul    ",
   "              and Andre Battaiola             "
};

static char *ModeInfo1[] = {
    " Change the Viewing Angle ",
    "                          ",
    "      Mouse Buttons       ",
    "--------------------------",
    " rotate | zoom & | trans- ",
    "  view  |  clip  |  late  "
};

static char *ModeInfo2[] = {
    " Make & View Trajectories ",
    "                          ",
    "       Mouse Buttons      ",
    "--------------------------",
    " rotate |  make  |  move  ",
    "  view  | trajec | cursor "
};

static char *ModeInfo3[] = { 
    " Make & Edit Text Labels  ",
    "                          ",
    "       Mouse Buttons      ",
    "--------------------------",
    "  make  |  move  | delete ",
    "  label |  label | label  "
};

static char *ModeInfo4[] = {
    "  Change Slice Positions  ",
    "                          ",
    "       Mouse Buttons      ",
    "--------------------------",
    " rotate | zoom & | move   ",
    "  view  |  clip  | slice  "
};

static char *ModeInfo5[] = {
    "  Probe Data With Cursor  ",
    "                          ",
    "       Mouse Buttons      ",
    "--------------------------",
    " rotate | zoom & | move   ",
    "  view  |  clip  | cursor "
};

static char *ModeInfo6[] = {
    "  Change Clipping Plane   ",
    "        Positions         ",
    "       Mouse Buttons      ",
    "--------------------------",
    " rotate | toggle |  move  ",
    "  view  | planes | planes ",
};

static char v5dfile[1000];


static int new_function_cb( LUI_NEWBUTTON *pb, int state );

void init_irregular_var_colortable( GuiContext gtx, int varowner, int var )
{
   int index;
   unsigned int *table;
   float *p;

   index = gtx->context_index;

   /* Textplots */
   vis5d_get_color_table_params( index, VIS5D_TEXTPLOT, varowner, var, &p);
   vis5d_get_color_table_address( index, VIS5D_TEXTPLOT, varowner, var, &table );
   vis5d_color_table_init_params( p, 1, 1 );
   vis5d_color_table_set_alpha( p, -2 );
   vis5d_color_table_set_alpha( p, 255 );
   vis5d_color_table_recompute( table, 256, p, 1, 1 );
}

static void init_var_colortable( GuiContext gtx, int varowner, int var )
{
   int index;
   unsigned int *table;
   float *p;
   
   index = gtx->context_index;

   
   /* Isosurfaces */
   vis5d_get_color_table_params( index, VIS5D_ISOSURF, varowner, var, &p);
   vis5d_get_color_table_address( index, VIS5D_ISOSURF, varowner, var, &table );
   vis5d_color_table_init_params( p, 1, 1 );
   vis5d_color_table_set_alpha( p, 255 );
   vis5d_color_table_recompute( table, 256, p, 1, 1 );

   /* CHSlices */
   vis5d_get_color_table_params( index, VIS5D_CHSLICE, varowner, var, &p);
   vis5d_get_color_table_address( index, VIS5D_CHSLICE, varowner, var, &table );
   vis5d_color_table_init_params( p, 1, 1 );
   vis5d_color_table_set_alpha( p, -2 );
/* MJK 2.8.99
   vis5d_color_table_set_alpha( p, 255 );
*/
   vis5d_color_table_recompute( table, 256, p, 1, 1 );

   /* CVSlices */
   vis5d_get_color_table_params( index, VIS5D_CVSLICE, varowner, var, &p);
   vis5d_get_color_table_address( index, VIS5D_CVSLICE, varowner, var, &table );
   vis5d_color_table_init_params( p, 1, 1 );
   vis5d_color_table_set_alpha( p, -2 );
/* MJK 2.8.99
   vis5d_color_table_set_alpha( p, 255 );
*/
   vis5d_color_table_recompute( table, 256, p, 1, 1 );

   /* Volumes */
   vis5d_get_color_table_params( index, VIS5D_VOLUME, varowner, var, &p);
   vis5d_get_color_table_address( index, VIS5D_VOLUME, varowner, var, &table );
   vis5d_color_table_init_params( p, 1, 1 );
   vis5d_color_table_set_alpha( p, -1 );
   vis5d_color_table_recompute( table, 256, p, 1, 1 );

   /* Trajectories */
   vis5d_get_color_table_params( index, VIS5D_TRAJ, varowner, var, &p);
   vis5d_get_color_table_address( index, VIS5D_TRAJ, varowner, var, &table );
   vis5d_color_table_init_params( p, 1, 1 );
   vis5d_color_table_set_alpha( p, 255 );
   vis5d_color_table_recompute( table, 256, p, 1, 1 );

   /* Topography */
   vis5d_get_color_table_params( index, VIS5D_TOPO, varowner, var, &p);
   vis5d_get_color_table_address( index, VIS5D_TOPO, varowner, var, &table );
   vis5d_color_table_init_params( p, 1, 1 );
   vis5d_color_table_set_alpha( p, 255 );
   vis5d_color_table_recompute( table, 256, p, 1, 1 );
}



void init_colortables( int index )
{
   int yo, spandex, howmany, whichones[VIS5D_MAX_CONTEXTS];
   GuiContext gtx = get_gui_gtx(index);
   int var, numvars;
   float *p;
   
   vis5d_get_num_of_ctxs_in_display( index, &howmany, whichones);
   for (yo = 0; yo < howmany; yo++){
      spandex = whichones[yo];
      vis5d_get_ctx_numvars( spandex, &numvars);
      for (var=0;var<numvars; var++){
         init_var_colortable( gtx, spandex, var);
      }
   }

   vis5d_get_num_of_itxs_in_display( index, &howmany, whichones);
   for (yo = 0; yo < howmany; yo++){
      spandex = whichones[yo];
      vis5d_get_itx_numvars( spandex, &numvars);
      for (var=0;var<numvars; var++){
         init_irregular_var_colortable( gtx, spandex, var);
      }
   }

   /* Topography height color table */
   if (howmany >= 1){
      unsigned int *table;

      spandex = whichones[0];
      vis5d_get_color_table_params( index, VIS5D_TOPO, whichones[0], -1, &p);
      vis5d_get_color_table_address( index, VIS5D_TOPO, whichones[0], -1, &table );
      vis5d_color_table_init_params( p, 1, 1 );
      vis5d_color_table_set_alpha( p, 255 );
      vis5d_color_table_recompute( table, 256, p, 1, 1 );
      vis5d_reset_topo_colors( index );
   }
}

void init_some_colortables( int dindex, int vindex )
{
   GuiContext gtx = get_gui_gtx(dindex);
   int var, numvars;

   vis5d_get_ctx_numvars( vindex, &numvars);
   for (var=0;var<numvars; var++){
      init_var_colortable( gtx, vindex, var);
   }
}
  

static Window get_win( int index )
{
   Window winny;

   vis5d_get_display_window( index,&winny );
   return winny;
}

/*******************************************************/
/*** CALL BACK FUNCTIONS FOR VERIFY AND ALERT WINDOW ***/
/*******************************************************/

static int verify_value;
int alrighty_value;


static int ok_cb( LUI_NEWBUTTON *pb )
{
  int index = pb->context_index;
  GuiContext gtx = get_gui_gtx(index);
  verify_value = 1;
  return 0;
}

static int OK_cb( LUI_NEWBUTTON *pb )
{
  int index = pb->context_index;
  GuiContext gtx = get_gui_gtx(index);
  alrighty_value = 1;
  return 0;
}

static int cancel_cb( LUI_NEWBUTTON *pb )
{
  int index = pb->context_index;
  GuiContext gtx = get_gui_gtx(index);
  verify_value = 0;
  return 0;
}




/*** verify ***********************************************************
   Open the verification window with a prompt asking the user to
   specify either an OK or CANCEL.
   Input:  prompt - a string which asks the user what he wants to do.
                    Ex: "Do you want to exit?"
   Returned:  1 = OK selected
              0 = CANCEL selected
**********************************************************************/
int verify( int index, char *prompt )
{
   XEvent pe;
   GuiContext gtx = get_gui_gtx(index);

   LUI_NewLabelChangeText( gtx->VerifyLabel, prompt );

   verify_value = -1;
   XMapWindow( GuiDpy, gtx->VerifyWindow );

   while (verify_value < 0) {
      if (XPending(GuiDpy)) {
         XNextEvent(GuiDpy, &pe);
         LUI_EventDispatch(&pe);
      }
   }
   XUnmapWindow( GuiDpy, gtx->VerifyWindow );
   XSync( GuiDpy, 0 );
   vis5d_signal_redraw(index, 1);
   return(verify_value);
}
int alrighty( int index, char *prompt )
{
   XEvent pe;
   GuiContext gtx = get_gui_gtx(index);

   LUI_NewLabelChangeText( gtx->OKLabel, prompt );

   alrighty_value = -1;
   XMapWindow( GuiDpy, gtx->OKWindow );

   while (alrighty_value < 0) {
      if (XPending(GuiDpy)) {
         XNextEvent(GuiDpy, &pe);
         LUI_EventDispatch(&pe);
      }
   }
   XUnmapWindow( GuiDpy, gtx->OKWindow );
   XSync( GuiDpy, 0 );
   return(alrighty_value);

}

int show_group_color_key(  char *prompt )
{
   Window box1;
   LUI_NEWLABEL *eq1;
   if (keywin == NULL){
   
      keywin = LUI_CreateWindowAt( LUI_RootWindow, 100, 200, 300, 160);
      XMapWindow(GuiDpy, keywin);
      /* keylabel = LUI_NewLabelCreate(keywin, LUI_LEFT, LUI_TOP,
                                    300, 55, "nil" );*/
      box1 = XCreateSimpleWindow(GuiDpy, keywin, 10, 10, 40, 40, 1,
                                 LUI_AllocateColorInt(0,0,0),
                                 LUI_AllocateColorInt(255, 0, 255));
      XMapWindow(GuiDpy, box1);
      eq1 = LUI_NewLabelCreate(keywin, 55,20,20,20,"=1");

      keyexit = LUI_PushButtonCreate(keywin, 230, 60, 60, 30, "Close");

   }
/*   LUI_NewLabelChangeText( keylabel, prompt);*/
   XMapWindow(GuiDpy, keywin);
   return 0;
}    

void set_current_display( int cur )
{
   Current_Display = cur;
}

void get_current_display(  int *cur )
{
   *cur = Current_Display;
}
   




/*** alert ***********************************************************
   Open the alert window with a message and wait until the user clicks
   on "OK".
   Input:  prompt - a message string
**********************************************************************/
int alert( int index, char *prompt )
{
   XEvent pe;
   GuiContext gtx = get_gui_gtx(index);

   if (gtx->AlertLabel == NULL ) return 0;

   LUI_NewLabelChangeText( gtx->AlertLabel, prompt );

   verify_value = -1;
   XMapWindow( GuiDpy, gtx->AlertWindow );

   while (verify_value < 0) {
      if (XPending(GuiDpy)) {
         XNextEvent(GuiDpy, &pe);
         LUI_EventDispatch(&pe);
      }
   }
   XUnmapWindow( GuiDpy, gtx->AlertWindow );
   vis5d_signal_redraw(index, 1);
   return 1;
}

void set_display_border_color( int index, int R, int G, int B)
{
   unsigned long bumber;
   GuiContext gtx = get_gui_gtx(index);

#ifdef NO_BORDERS
      XSetWindowBorderWidth( GfxDpy, get_win(index), 0);
#else
      XSetWindowBorderWidth( GfxDpy, get_win(index), 1);
#endif
   gtx->borderR = R;
   gtx->borderG = G;
   gtx->borderB = B;
   XSync(GfxDpy, 0);
   bumber = SND_AllocateColorInt(R, G, B);
   XSetWindowBorder( GfxDpy, get_win(index), bumber);
}

void get_display_border_color( int index, int *R, int *G, int *B)
{
   GuiContext gtx = get_gui_gtx(index);

   *R = gtx->borderR;
   *G = gtx->borderG;
   *B = gtx->borderB;
}
 
void set_display_matrix( int rows, int cols )
{
      DisplayRows = rows;
      DisplayCols = cols;
}  
void get_display_matrix( int *rows, int *cols )
{  
   *rows = DisplayRows;
   *cols = DisplayCols;
}        


void unmap_all_windows( void )
{
   int  y;
   GuiContext gtx;
 
   for (y = 0; y < VIS5D_MAX_DPY_CONTEXTS; y++){
      vis5d_unmap_3d_window(y);
   }
}

void enable_gui_button_windows( int vindex, int dindex, int type, int num, int mode)
{
   GuiContext gtx = get_gui_gtx(dindex);

   if (type == VIS5D_HWIND){
      if (mode != 0){
         if (num == 0){
            LUI_ButtonSetState(gtx->hwind1BUTTON,1);
         }
         else{
            LUI_ButtonSetState(gtx->hwind2BUTTON,1);
         }
         gtx->cur_hwind = num;
         gtx->cur_hstream = -1;
         gtx->cur_hwindmap = 1;
         gtx->cur_hstreammap = 0;
      }
      else{
         if (num == 0){
            LUI_ButtonSetState(gtx->hwind1BUTTON,0);
         }
         else{
            LUI_ButtonSetState(gtx->hwind2BUTTON,0);
         }
         gtx->cur_hwind = -1;
         gtx->cur_hwindmap = 0;
      }
   }
   else if (type == VIS5D_VWIND){
      if (mode != 0){
         if (num == 0){
            LUI_ButtonSetState(gtx->vwind1BUTTON,1);
         }
         else{
            LUI_ButtonSetState(gtx->vwind2BUTTON,1);
         }
         gtx->cur_vwind = num;
         gtx->cur_vstream = -1;
         gtx->cur_vwindmap = 1;
         gtx->cur_vstreammap = 0;
      }
      else{
         if (num == 0){
            LUI_ButtonSetState(gtx->vwind1BUTTON,0);
         }
         else{
            LUI_ButtonSetState(gtx->vwind2BUTTON,0);
         }
         gtx->cur_vwind = -1;
         gtx->cur_vwindmap = -1;
      }
   }
   else if (type == VIS5D_HSTREAM){
      if (mode != 0){
         LUI_ButtonSetState(gtx->hstreamBUTTON,1);
         gtx->cur_hwind = -1;
         gtx->cur_hstream = num;
         gtx->cur_hwindmap = 0;
         gtx->cur_hstreammap = 1;
      }
      else{
         gtx->cur_hstream = -1;
         gtx->cur_hstreammap = -1;
      }
   }
   else if (type == VIS5D_VSTREAM){
      if (mode != 0){
         LUI_ButtonSetState(gtx->vstreamBUTTON,1);
         gtx->cur_vwind = -1;
         gtx->cur_vstream = num;
         gtx->cur_vwindmap = 0;
         gtx->cur_vstreammap = 1;
      }
      else{
         LUI_ButtonSetState(gtx->vstreamBUTTON,0);
      }
   }
   else{
      if (type == VIS5D_HSLICE){
         if (mode != 0){
            gtx->cur_hslice = num;
            gtx->cur_hslicevindex = vindex;
            gtx->cur_hslicemap = 1;
         }
         else{
            if (gtx->cur_hslice == num && gtx->cur_hslicevindex == vindex){
               gtx->cur_hslice = -1;
               gtx->cur_hslicevindex = -1;
               gtx->cur_hslicemap = 0;
            }
         }
      }
      else if (type == VIS5D_VSLICE){
         if (mode != 0){
            gtx->cur_vslice = num;
            gtx->cur_vslicevindex = vindex;
            gtx->cur_vslicemap = 1;
         }
         else{
            if (gtx->cur_vslice == num &&
                gtx->cur_vslicevindex == vindex){
               gtx->cur_vslice = -1;
               gtx->cur_vslicevindex = -1;
               gtx->cur_vslicemap = 0;
            }
         }
      }
      else if (type == VIS5D_CHSLICE){
         if (mode != 0){
            cb_chvindex[dindex] = vindex;
            cb_chvvar[dindex]   = num;
            cb_chvar[dindex]    = get_button_gtx_index(dindex,
                                         vindex, num);
         }
         else{
            if (cb_chvindex[dindex] == vindex &&
                cb_chvvar[dindex] == num &&
                cb_chvar[dindex] == get_button_gtx_index(dindex,vindex, num)){
               cb_chvindex[dindex] = -1;
               cb_chvvar[dindex]   = -1;
               cb_chvar[dindex]    = -1;
            }
         }
      }
      else if (type == VIS5D_CVSLICE){
         if (mode != 0){
            cb_vindex[dindex] = vindex;
            cb_vvar[dindex]   = num;
            cb_var[dindex]    = get_button_gtx_index(dindex,
                                         vindex, num);
            cb_graphic[dindex]= VIS5D_CVSLICE;
         }
         else{
            if (cb_vindex[dindex] == vindex &&
                cb_vvar[dindex] == num &&
                cb_var[dindex] == get_button_gtx_index(dindex,vindex, num)){
               cb_vindex[dindex] = -1;
               cb_vvar[dindex]   = -1;
               cb_var[dindex]    = -1;
               cb_graphic[dindex]= -1;
            }
         }
      }
      else if (type == VIS5D_ISOSURF){
         if (mode != 0){
            gtx->cur_isosurf = num;
            gtx->cur_isosurfvindex = vindex;
            gtx->cur_isosurfmap = 1;
         }
         else{
            if (gtx->cur_isosurf == num && gtx->cur_isosurfvindex == vindex){
               gtx->cur_isosurf = -1;
               gtx->cur_isosurfvindex = -1;
               gtx->cur_isosurfmap = 0;
            }
         }
      }
      else if (type == VIS5D_VOLUME){
         vis5d_get_volume(dindex,  &gtx->CurrentVolumeOwner, &gtx->CurrentVolume);
         if (mode != 0){
            gtx->CurrentVolume = num;
            gtx->CurrentVolumeOwner = vindex;
            cb_vindex[dindex] = vindex;
            cb_vvar[dindex]   = num;
            cb_var[dindex]    = get_button_gtx_index(dindex,
                                         vindex, num);
            cb_graphic[dindex]= VIS5D_VOLUME;
         }
         else{
            if (gtx->CurrentVolume == num &&
                gtx->CurrentVolumeOwner == vindex){
               gtx->CurrentVolume = -1;
               gtx->CurrentVolumeOwner = -1;
               cb_vindex[dindex] = -1;
               cb_vvar[dindex]   = -1;
               cb_var[dindex]    = -1;
               cb_graphic[dindex]= -1;
            }
         }
      }
   }
}

void enable_and_make_gui_button_graphics( int vindex, int dindex, int type, int num, int mode)
{
   GuiContext gtx = get_gui_gtx(dindex);
   int curtime, numtimes, time, times;

   if (type == VIS5D_HWIND){
      if (mode != 0){
         vis5d_enable_graphics(gtx->array_of_ctxs[0],
                               VIS5D_HWIND, num, VIS5D_ON);
         vis5d_get_dtx_timestep(dindex, &curtime);
         vis5d_get_dtx_numtimes(dindex, &numtimes);
         for (time=0;time<numtimes;time++) {
            vis5d_make_hwindslice(dindex, time, num, time==curtime);
         }
      }
      else{
         vis5d_enable_graphics(gtx->array_of_ctxs[0],
                               VIS5D_HWIND, num, VIS5D_OFF);
      }
      vis5d_invalidate_dtx_frames(dindex);
   }
   else if (type == VIS5D_VWIND){
      if (mode != 0){
         vis5d_enable_graphics(gtx->array_of_ctxs[0],
                               VIS5D_VWIND, num, VIS5D_ON);
         vis5d_get_dtx_timestep(dindex, &curtime);
         vis5d_get_dtx_numtimes(dindex, &numtimes);
         for (time=0;time<numtimes;time++) {
            vis5d_make_vwindslice(dindex, time, num, time==curtime);
         }
      }
      else{
         vis5d_enable_graphics(gtx->array_of_ctxs[0],
                               VIS5D_VWIND, num, VIS5D_OFF);
      }
      vis5d_invalidate_dtx_frames(dindex);
   }
   else if (type == VIS5D_HSTREAM){
      if (mode != 0){
         vis5d_enable_graphics(gtx->array_of_ctxs[0],
                               VIS5D_HSTREAM, num, VIS5D_ON);
         vis5d_get_dtx_timestep(dindex, &curtime);
         vis5d_get_dtx_numtimes(dindex, &numtimes);
         for (time=0;time<numtimes;time++){
            vis5d_make_hstreamslice(dindex, time, num, time==curtime);
         }
      }
      else{
         vis5d_enable_graphics(gtx->array_of_ctxs[0], VIS5D_HSTREAM, num, VIS5D_OFF);
      }
      vis5d_invalidate_dtx_frames(dindex);
   }
   else if (type == VIS5D_VSTREAM){
      if (mode != 0){
         vis5d_enable_graphics(gtx->array_of_ctxs[0],
                               VIS5D_VSTREAM, num, VIS5D_ON);
         vis5d_get_dtx_timestep(dindex, &curtime);
         vis5d_get_dtx_numtimes(dindex, &numtimes);
         for (time=0;time<numtimes;time++){
            vis5d_make_vstreamslice(dindex, time, num, time==curtime);
         }
      }
      else{
         vis5d_enable_graphics(gtx->array_of_ctxs[0],
                               VIS5D_VSTREAM, num, VIS5D_OFF);
      }
      vis5d_invalidate_dtx_frames(dindex);
   }
   else{
      if (type == VIS5D_HSLICE){
         if (mode != 0){
            vis5d_enable_graphics(vindex, VIS5D_HSLICE,
                                  num, VIS5D_ON);
            vis5d_get_ctx_numtimes( vindex, &numtimes );
            vis5d_get_ctx_timestep( vindex,  &curtime);
            for ( times = 0; times < numtimes; times++){
               vis5d_make_hslice( vindex, times, num, times==curtime);
            }
         }
         else{
            vis5d_enable_graphics(vindex, VIS5D_HSLICE, num,
                                  VIS5D_OFF);
         }
      }
      else if (type == VIS5D_VSLICE){
         if (mode != 0){
            vis5d_enable_graphics(vindex, VIS5D_VSLICE, num,
                                  VIS5D_ON);
            vis5d_get_ctx_numtimes( vindex, &numtimes );
            vis5d_get_ctx_timestep( vindex,  &curtime);
            for ( times = 0; times < numtimes; times++){
               vis5d_make_vslice( vindex, times, num, times==curtime);
            }
         }
         else{
            vis5d_enable_graphics(vindex, VIS5D_VSLICE, num,
                                  VIS5D_OFF);
         }
      }
      else if (type == VIS5D_CHSLICE){
         if (mode != 0){
            vis5d_enable_graphics(vindex, VIS5D_CHSLICE, num,
                                  VIS5D_ON);
            vis5d_get_ctx_numtimes( vindex, &numtimes );
            vis5d_get_ctx_timestep( vindex,  &curtime);
            for ( times = 0; times < numtimes; times++){
               vis5d_make_chslice( vindex, times, num, times==curtime);
            }
         }
         else{
            vis5d_enable_graphics(vindex, VIS5D_CHSLICE, num,
                                  VIS5D_OFF);
         }
      }
      else if (type == VIS5D_CVSLICE){
         if (mode != 0){
            vis5d_enable_graphics(vindex, VIS5D_CVSLICE, num,
                                  VIS5D_ON);
            vis5d_get_ctx_numtimes( vindex, &numtimes );
            vis5d_get_ctx_timestep( vindex,  &curtime);
            for ( times = 0; times < numtimes; times++){
               vis5d_make_cvslice( vindex, times, num, times==curtime);
            }
         }
         else{
            vis5d_enable_graphics(vindex, VIS5D_CVSLICE, num,
                                  VIS5D_OFF);
         }
      }
      else if (type == VIS5D_ISOSURF){
         if (mode != 0){
            vis5d_enable_graphics(vindex, VIS5D_ISOSURF, num,
                                  VIS5D_ON);
            vis5d_get_ctx_numtimes( vindex, &numtimes );
            vis5d_get_ctx_timestep( vindex,  &curtime);
            for ( times = 0; times < numtimes; times++){
               vis5d_make_isosurface( vindex, times, num, times==curtime);
            }
         }
         else{
            vis5d_enable_graphics(vindex, VIS5D_ISOSURF, num,
                                  VIS5D_OFF);
         }
      }
      else if (type == VIS5D_VOLUME){
         int cvo, cv;
         vis5d_get_volume(dindex,  &cvo, &cv);
         if (mode != 0){
            if ((cvo != vindex || cv != num) && cvo != -1){
               vis5d_enable_graphics(cvo, VIS5D_VOLUME,
                                     cv, VIS5D_OFF);
            }
            vis5d_enable_graphics(vindex, VIS5D_VOLUME, num,
                                  VIS5D_ON);
            gtx->CurrentVolume = num;
            gtx->CurrentVolumeOwner = vindex;
            vis5d_set_volume( dindex, vindex, num);
         }
         else{
            if (cv == num &&
                cvo == vindex){
               vis5d_set_volume( dindex, -1, -1);
            }
         }
      }
   }
}



/* HERE */

int update_linked_buttons( int vindex, int dindex, int type, int var, int state )
{
   int *next_vindex, *next_type, *next_var;
   int cur_dindex, cur_vindex, cur_type, cur_var;
   int do_once;
   GuiContext gtx = get_gui_gtx(dindex);
 
   do_once = 1;
   cur_vindex = vindex;
   cur_type = type;
   cur_var = var;
   while(1){
      if(gtx->group_index > 0){
         if(!vis5d_get_group_graphic_link(cur_vindex,cur_type,cur_var,
                                          &next_vindex,&next_type,&next_var)){
            return 0;
         }
      }
      else{
         if(!vis5d_get_slice_link(cur_vindex,cur_type,cur_var,
                                  &next_vindex,&next_type,&next_var)){
            return 0;
         }
      }

      if (vindex == cur_vindex && type == cur_type && var == cur_var &&
          do_once == 0){
         return 1;
      }
      else if (vindex == cur_vindex && type == cur_type && var == cur_var &&
          do_once == 1){
         do_once = 0;
      }
      vis5d_get_ctx_display_index( cur_vindex, &cur_dindex);
      enable_gui_button_windows(cur_vindex, cur_dindex, cur_type, cur_var, state);
      enable_and_make_gui_button_graphics(cur_vindex, cur_dindex, cur_type, cur_var, state);
      cur_vindex = *next_vindex;
      cur_type = *next_type;
      cur_var = *next_var; 
   }   
}
   




void map_all_windows( int onlyrecolor )
{
   GuiContext gtx;
   int x, y, yo, wstep, hstep, width, height;
   int Kurrant;
   XWindowAttributes winatts;
   int R,G,B, gindex;
   static GC gc;
   XGCValues vals;

   yo = 0;
   get_current_display( &Kurrant );
   XGetWindowAttributes( GfxDpy, BigWindow, &winatts);
   for (y = 0; y < DisplayRows; y++){
      for (x = 0; x < DisplayCols; x++){
         gtx = get_gui_gtx2(yo);
         if (!gtx){
            /* yo++; */
         }
         else{
            vis5d_get_display_group( yo, &gindex );
            gtx->group_index = gindex;
            width =   winatts.width-(right_margin*DisplayCols)-
                                    (left_margin*DisplayCols);
            height =  winatts.height-(bottom_margin*DisplayRows)-
                                     (top_margin*DisplayRows);
            if (DisplayRows == 1 && DisplayCols == 1){
               wstep = width-8;
               hstep = height-8;
            }
            else{
               wstep = ((width-4) -(DisplayCols*8)) / DisplayCols;
               hstep = ((height-4) -(DisplayRows*8))/ DisplayRows;
            }
            if (!StaticWin){
               wstep = wstep > hstep ? hstep : wstep;
               hstep = hstep > wstep ? wstep : hstep;
            }
            if (DisplayRows == 1 && DisplayCols == 1){
               set_display_border_color(0, 0, 0, 0 );
            }
            else if (gtx->group_index >= 1){
               switch(gtx->group_index){
                  case 1:
                     set_display_border_color( yo, 255, 0, 255);
                     break;
                  case 2:
                     set_display_border_color( yo, 255, 255, 0 );
                     break;
                  case 3:
                     set_display_border_color( yo, 0, 255, 255);
                     break;
                  case 4:
                     set_display_border_color( yo, 255, 180, 50);
                     break;
                  case 5:
                     set_display_border_color( yo, 180, 50, 255);
                     break;
                  case 6:
                     set_display_border_color( yo, 50, 255, 180);
                     break;
                  case 7:
                     set_display_border_color( yo, 255, 50, 180);
                     break;
                  case 8:
                     set_display_border_color( yo, 180, 255, 50);
                     break;
                  case 9:
                     set_display_border_color( yo, 50, 180, 255);
                     break;
                  default: printf("something is awry\n");
               }
            }
            else if (gtx->context_index == Kurrant){
               set_display_border_color(yo, 255, 255, 255);
            }
            else {
               set_display_border_color(yo, 165, 42, 42);
            }
            if (onlyrecolor == 0){
               vis5d_map_3d_window( gtx->context_index );
               vis5d_moveresize_3d_window( gtx->context_index,
                             x * 8 + 4 + x * wstep + (left_margin*(x+1)) +
                                                     (right_margin*x),
                          y * 8 + 4 + y * hstep + (top_margin*(y+1)) +
                                                   (bottom_margin*y),
                           wstep, hstep);
               vis5d_invalidate_dtx_frames(gtx->context_index);
            }
            yo++;
         }
      }
   }
   wstep = wstep + right_margin + left_margin; 
   hstep = hstep + bottom_margin + top_margin; 
   if (DisplayRows == 1 && DisplayCols == 1){
      vis5d_resize_BIG_window((wstep+8)*DisplayCols, (hstep+8)*DisplayRows);
   }
   else if (DisplayRows != 0 && DisplayCols != 0){
      vis5d_resize_BIG_window((wstep+8)*DisplayCols+4, (hstep+8)*DisplayRows+4);
   }
   /* draw titles */
   for (yo=0; yo<number_of_titles; yo++){
      char ring[200];
      int len;
      if (!XLoadFont(GfxDpy, title_font[yo])){
         printf("can't load title font\n");
      }
      else{
         vals.background = LUI_AllocateColorInt(0, 0, 0);
         vals.foreground = LUI_AllocateColorInt(255, 255, 255);
         gc = XCreateGC( GfxDpy, BigWindow, GCForeground | GCBackground, &vals );
         XSetFont(GfxDpy, gc, XLoadFont(GfxDpy, title_font[yo]));
         strcpy(ring,title_string[yo]);
         len = strlen(ring);
         XDrawString(GfxDpy, BigWindow, gc,
                 title_x_position[yo], title_y_position[yo],
                 ring, len);
      
      }
   }
}

         

/**********************************************************************/
/*****                   Widget callback functions                *****/
/**********************************************************************/


#ifdef LEAVEOUT
static int debug_cb( LUI_BUTTON *pb )
{
   int index = pb->context_index;
   GuiContext gtx = get_gui_gtx(index);
/*   Debug = !Debug;*/
   return 0;
}
#endif


/* toggle trajectory ribbon button */
static int ribbon_cb( LUI_NEWBUTTON *pb )
{
   float UserTrajStep, UserTrajLength;
   int dyo, dhowmany, dwhichones[VIS5D_MAX_CONTEXTS];
   int RibbonFlag;
   int index = pb->context_index;
   GuiContext gtx = get_gui_gtx(index);
   vis5d_get_traj(index, &UserTrajStep, &UserTrajLength, &RibbonFlag);
   RibbonFlag = !RibbonFlag;
   if (gtx->group_index > 0){
      vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
      for (dyo = 0; dyo < dhowmany; dyo++){
         vis5d_set_traj(dwhichones[dyo], UserTrajStep, UserTrajLength, RibbonFlag);
      }
   }
   else{
      vis5d_set_traj(index, UserTrajStep, UserTrajLength, RibbonFlag);
   }
   group_event(gtx->group_index, 100, RibbonFlag);
   vis5d_signal_redraw(index, 1);
   return 0;
}


/* delete last trajectory */
static int dellast_cb( LUI_NEWBUTTON *pb )
{
   int index = pb->context_index;
   GuiContext gtx = get_gui_gtx(index);
   vis5d_delete_last_traj(index);
   vis5d_invalidate_dtx_frames(index);
   return 0;
}


/* delete trajectory set */
static int delset_cb( LUI_NEWBUTTON *pb )
{
   char msg[100];
   int index = pb->context_index;
   GuiContext gtx = get_gui_gtx(index);

   sprintf(msg,"Delete trajectory set %d?", gtx->cur_trajset+1);
   if (verify(index, msg)) {
      vis5d_delete_traj_set(index, gtx->cur_trajset);
   }
   vis5d_invalidate_dtx_frames(index);
   return 0;
}



/* called when trajectory step type-in changes */
static int trajstep_cb( LUI_FIELD *field )
{
   float UserTrajStep, UserTrajLength;
   int dyo, dhowmany, dwhichones[VIS5D_MAX_CONTEXTS];
   int RibbonFlag;
   int index = field->context_index;
   GuiContext gtx = get_gui_gtx(index);

   vis5d_get_traj(index, &UserTrajStep, &UserTrajLength, &RibbonFlag);
   UserTrajStep = LUI_FieldGetDouble( gtx->TrajStepField );
   if (UserTrajStep<=0.0) {
      UserTrajStep = 1.0;
      LUI_FieldSetDouble( gtx->TrajStepField, 1.0 );
   }
   if (gtx->group_index > 0){
      vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
      for (dyo = 0; dyo < dhowmany; dyo++){
         vis5d_set_traj(dwhichones[dyo], UserTrajStep, UserTrajLength, RibbonFlag);
      }
   }
   else{
      vis5d_set_traj(index, UserTrajStep, UserTrajLength, RibbonFlag);
   }
   return 0;
}



/* called when trajectory length type-in changes */
static int trajlength_cb( LUI_FIELD *field )
{
   float UserTrajStep, UserTrajLength;
   int dyo, dhowmany, dwhichones[VIS5D_MAX_CONTEXTS];   
   int RibbonFlag;
   int index = field->context_index;
   GuiContext gtx = get_gui_gtx(index);
   vis5d_get_traj(index, &UserTrajStep, &UserTrajLength, &RibbonFlag);
   UserTrajLength = LUI_FieldGetDouble( gtx->TrajLenField );
   if (UserTrajLength<=0.0) {
      UserTrajLength = 1.0;
      LUI_FieldSetDouble( gtx->TrajLenField, 1.0 );
   }
   if (gtx->group_index > 0){
      vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
      for (dyo = 0; dyo < dhowmany; dyo++){
         vis5d_set_traj(dwhichones[dyo], UserTrajStep, UserTrajLength, RibbonFlag);
      }
   }
   else{
      vis5d_set_traj(index, UserTrajStep, UserTrajLength, RibbonFlag);
   }
   return 0;
}



/*
 * Display the expression type-in window with the given expression shown.
 */
static void map_expr_window( int index, char *expr )
{
   GuiContext gtx = get_gui_gtx(index);
   strcpy( gtx->Expression, expr );
   LUI_FieldSetText( gtx->expr_field, expr );
   XMapWindow( GuiDpy, gtx->ExprWindow );
}


/*** add_button_row ***************************************************
   Add a new row of variable buttons to the control panel window.
   Input:  name - name of new variable
           newrow - which row of buttons this is to be.
**********************************************************************/

/* DONT USE THIS ANYMORE BUT KEEP IT HERE FOR OLE TIMES SAKE */
/* make_another_gui() does all of this stuff */

static void add_button_row( int index, char *name, int newrow )
{
   float red[6], green[6], blue[6];
   char *labels[6];
   int j;
   int Nr, Nc, Nl[MAXVARS], LowLev[MAXVARS], MaxNl, MaxNlVar, WindNl, WindLow;
   GuiContext gtx = get_gui_gtx(index); /*index = display index */
   vis5d_get_size(index, &Nr, &Nc, Nl, LowLev, &MaxNl, &MaxNlVar, &WindNl, &WindLow);

   for (j=0; j<gtx->Columns; j++) {
      if (j==5) {
         /* volume button always white */
         red[j] = green[j] = blue[j] = 1.0;
      }
      else {
         float alpha;
         vis5d_get_color( index, j, newrow, &red[j], &green[j], &blue[j],
                          &alpha );
      }
      if (Nl[newrow]>1 || (j==1 || j==3)) {
         labels[j] = name;
      }
      else {
         labels[j] = NULL;
      }
   }
   LUI_ContextIndex(index);
   LUI_ButtonMatrixAddRow( gtx->ButtonMatrix, labels, red, green, blue );
   LUI_ButtonMatrixShowBottom( gtx->ButtonMatrix );
}


/* called when a button in the clone window is selected */
static int do_clone_cb( LUI_NEWBUTTON *b, int state )
{
  int index = b->context_index;
  int vindex = b->indexowner;
  int newvar;
  int numvars;
  GuiContext gtx = get_gui_gtx(index);


  if (b->index==9999) {
     /* Cancel button */
  }
  else if (b->index>=0) {
     /* clone an existing variable */
     int var_to_clone = b->index;
     char newname[100];
     int i;

     /* make name for new variable (original name plus quotes (')) */
     vis5d_get_ctx_var_name( b->indexowner, var_to_clone, newname );
     for (i=0;i<MAXVARS;i++) {
        strcat( newname, "'" );
        if (vis5d_find_var( b->indexowner, newname )<0) {
           break;
        }
     }
     if (i==MAXVARS ||
         vis5d_make_clone_variable(b->indexowner, var_to_clone, newname, &newvar)<0) {
        /* error, unable to make new variable */
     }
     else {
        /* add a row of buttons to the control panel */
        make_another_gui(gtx->context_index, 0);
        hide_widgets( index );
        show_widgets( index );
        /* turn_off_everything(gtx->context_index); */
        vis5d_reset_var_graphics(b->indexowner, newvar);
     }
  }
  else if (b->index<=0) {
    /* compute an external function variable */
    int func = -(b->index+1);
    if (FuncType[index][func]==VIS5D_EXT_FUNC) {
      char funcname[1000];
      if (gtx->funcpath[0]) {
         strcpy( funcname, gtx->funcpath );
      }
      else {
         strcpy( funcname, FUNCTION_PATH );
      }
      strcat( funcname, "/" );
      strcat( funcname, FuncName[index][func] );
      if (vis5d_compute_ext_func( index, funcname, &newvar) < 0 ) {
        /* error */
        char str[1000];
        sprintf(str, "External function \"%s\" failed", funcname );
        alert( index, str );
      }
      else {
        /* success */
        if (newvar >= 0) {
           char newname[10];
           vis5d_get_ctx_var_name( index, newvar, newname );
           make_another_gui(gtx->context_index, 0);
           hide_widgets( index );
           show_widgets( index );
           /* turn_off_everything(gtx->context_index); */
           vis5d_reset_var_graphics(index, newvar);
        }
      }
    }
    else if (FuncType[index][func]==VIS5D_EXPRESSION) {
      /* Recompute a variable defined by an expression */
      int lo, var, flag;
      vis5d_get_ctx_numvars( vindex, &numvars );
      lo = 0;
      flag = -1;
      for (var=0;var<numvars;var++) {
         char varname[10];
         vis5d_get_ctx_var_name( vindex, var, varname );
         while( FuncName[index][func][lo] != '.'){
            lo++;
         }
         FuncName[index][func][lo] = 0;
         if (strcmp(varname, FuncName[index][func])==0) {
            char expression[1000];
            vis5d_get_var_info( vindex, var, (void *) expression );
            map_expr_window( index, expression );
            flag = var;
         }
         FuncName[index][func][lo] = '.';
      }
      if (flag == -1) {
        printf("Error in do_clone_cb, var not found\n");
        return 0;
      }
    }
    else {
      printf("Error in do_clone_cb\n");
    }
  }

  XUnmapWindow( GuiDpy, gtx->CloneWindow );
  /*make_another_gui( gtx->context_index );*/
  vis5d_signal_redraw(index, 1);
  return 0;
}




/*
 * Scan the list of variables for the type VIS5D_EXPRESSION.  Put the
 * names of the expressions into the exprname array.
 * Return:  number of expression variables
 */
static int find_expr_variables( int index, char exprname[][1000] )
{
   int var, numvars, vartype, num;
   int chowmany, cwhichones[VIS5D_MAX_CONTEXTS];
   int cyo;

   num = 0;
   vis5d_get_num_of_ctxs_in_display( index,  &chowmany, cwhichones);
   for (cyo = 0; cyo < chowmany; cyo++){
      int vindex;
      vindex = cwhichones[cyo];
      vis5d_get_ctx_numvars( vindex, &numvars );
      for (var=0;var<numvars;var++) {
         vis5d_get_var_type( vindex, var, &vartype );
         if (vartype==VIS5D_EXPRESSION) {
            vis5d_get_ctx_var_name( vindex, var, exprname[num] );
            FuncOwner[index][num] = vindex;
            num++;
         }
      }
   }
   return num;
}



static void unmap_expr_window( int index )
{
   GuiContext gtx = get_gui_gtx(index);
   XUnmapWindow( GuiDpy, gtx->ExprWindow );
}



/* map the horizontal wind slice control window */
static void map_hwind_window( int index, int ws )
{
   float HWindDensity, HWindScale, HWindLevel;
   char str[1000];
   GuiContext gtx = get_gui_gtx(index);
   int vindex = gtx->array_of_ctxs[0];

   vis5d_get_hwindslice(index, ws, &HWindDensity, &HWindScale, &HWindLevel);

   gtx->cur_hwind = ws;

   /* MJK 12.04.98 begin */
   XUnmapWindow( GuiDpy, gtx->HWindWindow );
   sprintf( str, "HWind %d Scale:", ws+1 );
   LUI_NewLabelChangeText( gtx->hwindscale_label, str );
   LUI_FieldSetDouble( gtx->hwindscale_field, HWindScale );
   LUI_FieldSetDouble( gtx->hwinddensity_field, HWindDensity );

   {
      int       state;

      state = vis5d_enable_sfc_graphics (vindex, VIS5D_HWIND, ws, VIS5D_GET);
      LUI_ButtonSetState (gtx->hwind_sfc_button, state);

      mod_vpos_slider (index, gtx->hwind_pos_slider, ws, 1, HWindLevel);
   }

   XMapWindow( GuiDpy, gtx->HWindWindow );
   /* MJK 12.04.98 end */
}


/* map the vertical wind slice control window */
static void map_vwind_window( int index, int ws )
{
   float VWindDensity, VWindScale;
   float row0, col0, row1, col1;
   char str[1000];
   GuiContext gtx = get_gui_gtx(index);

   vis5d_get_vwindslice(index, ws, &VWindDensity, &VWindScale,
                        &row0, &col0, &row1, &col1);

   gtx->cur_vwind = ws;

   XUnmapWindow( GuiDpy, gtx->WindWindow );
   sprintf( str, "V-Wind %d Scale:", ws+1 );
   LUI_NewLabelChangeText( gtx->windscale_label, str );
   LUI_FieldSetDouble( gtx->windscale_field, VWindScale );
   LUI_FieldSetDouble( gtx->winddensity_field, VWindDensity );

   XMapWindow( GuiDpy, gtx->WindWindow );
}


/* map the horizontal stream slice control window */
static void map_hstream_window( int index, int ws )
{
   float HStreamDensity, HStreamScale, HStreamLevel;
   char str[1000];
   GuiContext gtx = get_gui_gtx(index);
   int vindex = gtx->array_of_ctxs[0];

   vis5d_get_hstreamslice(index, ws, &HStreamDensity, &HStreamLevel);
   HStreamScale = 1.0;

   gtx->cur_hstream = ws;

   /* MJK 12.04.98 begin */
   XUnmapWindow( GuiDpy, gtx->HWindWindow );
   sprintf( str, "HStream  Scale:" );
   LUI_NewLabelChangeText( gtx->hwindscale_label, str );
   LUI_FieldSetDouble( gtx->hwindscale_field, HStreamScale );
   LUI_FieldSetDouble( gtx->hwinddensity_field, HStreamDensity );

   {
      int       state;

      state = vis5d_enable_sfc_graphics (vindex, VIS5D_HSTREAM, ws, VIS5D_GET);
      LUI_ButtonSetState (gtx->hwind_sfc_button, state);

      mod_vpos_slider (index, gtx->hwind_pos_slider, ws, 1, HStreamLevel);
   }

   XMapWindow( GuiDpy, gtx->HWindWindow );
   /* MJK 12.04.98 end */
}


/* map the vertical stream slice control window */
static void map_vstream_window( int index, int ws )
{
   float VStreamDensity, VStreamScale;
   float VStreamR1, VStreamC1, VStreamR2, VStreamC2;
   char str[1000];
   GuiContext gtx = get_gui_gtx(index);

   vis5d_get_vstreamslice(index, ws, &VStreamDensity,
                          &VStreamR1, &VStreamC1, &VStreamR2, &VStreamC2);
   VStreamScale = 1.0;

   gtx->cur_vstream = ws;

   XUnmapWindow( GuiDpy, gtx->WindWindow );
   sprintf( str, "VStream %d Scale:", ws+1 );
   LUI_NewLabelChangeText( gtx->windscale_label, str );
   LUI_FieldSetDouble( gtx->windscale_field, VStreamScale );
   LUI_FieldSetDouble( gtx->winddensity_field, VStreamDensity );

   XMapWindow( GuiDpy, gtx->WindWindow );
}


/* map the isosurface isolevel window */
/* MJK 12.04.98 begin */
static void map_isosurf_window( int dindex, int vindex, int var )
{
   float min, max, isolevel;
   char name[10], units[20];
   GuiContext gtx = get_gui_gtx(dindex);
   int dhowmany, dwhichones[VIS5D_MAX_CONTEXTS];
   int chowmany, cwhichones[VIS5D_MAX_CONTEXTS];
   int times, dyo, cyo, good;
   char aname[30];
   char rap[30];

   if (gtx->group_index > 0){
      vis5d_get_ctx_var_name( vindex, var, aname);
      vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
      for (dyo = 0; dyo < dhowmany; dyo++){
         vis5d_get_num_of_ctxs_in_display( dwhichones[dyo], &chowmany, cwhichones);
         for ( cyo = 0; cyo < chowmany; cyo++){
            good = vis5d_find_var(cwhichones[cyo], aname);
            if (good>-1){
               vis5d_get_ctx_var_range(cwhichones[cyo], good, &min, &max );
               if (min > max ) {
                  min = max = 0.0;
                  isolevel = 0.0;
               }
               else {
                  vis5d_get_isosurface(cwhichones[cyo], good, &isolevel);
               }
               vis5d_get_ctx_var_name (cwhichones[cyo], good, name);
               vis5d_get_var_units (cwhichones[cyo], good, units);
               if (dhowmany > 1){
                  strcpy( rap, return_var_and_index(name, cwhichones[cyo]));
                  LUI_NewSliderChange( gtx_table[dwhichones[dyo]]->IsoSlider,
                                       rap, units,
                                       min, max, isolevel );
               }
               else{
                  LUI_NewSliderChange( gtx_table[dwhichones[dyo]]->IsoSlider,
                                       name, units, min, max, isolevel );
               }

               if(dwhichones[dyo] == dindex){
                  XMapWindow(GuiDpy, gtx->IsoWindow);
               }
            }
         }
      }
   }
   else{
      vis5d_get_ctx_var_range( vindex, var, &min, &max );
      if (min > max ) {
         min = max = 0.0;
         isolevel = 0.0;
      }
      else {
         vis5d_get_isosurface(vindex, var, &isolevel);
      }
      vis5d_get_num_of_ctxs_in_display( dindex, &chowmany, cwhichones);
      vis5d_get_ctx_var_name (vindex, var, name);
      vis5d_get_var_units (vindex, var, units);
      if (dhowmany > 1){
         strcpy( rap, return_var_and_index(name, vindex));
         LUI_NewSliderChange( gtx->IsoSlider,rap, units,
                              min, max, isolevel );
      }
      else{
         LUI_NewSliderChange( gtx->IsoSlider, name, units, min, max, isolevel );
      }

      XMapWindow(GuiDpy, gtx->IsoWindow);
   }
}
/* MJK 12.04.98 end */


/* map the horizontal contour line slice control window */
static void map_hslice_window( int dindex, int vindex, int var )
{
   char str[100];
   float min, max;
   char name[10];
   float HSliceInterval, HSliceLowLimit, HSliceHighLimit, HSliceLevel;
   GuiContext gtx = get_gui_gtx(dindex);

   vis5d_get_ctx_var_range(vindex, var, &min, &max);
   vis5d_get_ctx_var_name(vindex, var, name );
   vis5d_get_hslice(vindex, var, &HSliceInterval,
                    &HSliceLowLimit, &HSliceHighLimit, &HSliceLevel);

   /* MJK 12.04.98 begin */
   {
      char      units[20];
      int       lunits, lname, state;

      vis5d_get_var_units (vindex, var, units);
      if ((lunits = strlen (units)) > 0)
      {
         lname = strlen (name);
         if ((lname+lunits) > 5)
            sprintf (str, "%s H. Slice Int. (%s):", name, units);
         else
            sprintf (str, "%s Hor. Slice Interval (%s):", name, units);
      }
      else
         sprintf (str, "%s Hor. Slice Interval:", name);

      state = vis5d_enable_sfc_graphics (vindex, VIS5D_HSLICE, var, VIS5D_GET);
      LUI_ButtonSetState (gtx->hslice_sfc_button, state);

      mod_vpos_slider (dindex, gtx->hslice_pos_slider, var, 0, HSliceLevel);
   }
   /* MJK 12.04.98 end */

   LUI_NewLabelChangeText( gtx->hslice_label, str );
   if (HSliceLowLimit==min && HSliceHighLimit==max)
     sprintf( str, "%g", HSliceInterval );
   else
     sprintf( str, "%g (%g,%g)", HSliceInterval, HSliceLowLimit, HSliceHighLimit );
   LUI_FieldSetText( gtx->hslice_field, str );

   XMapWindow(GuiDpy,gtx->HSliceWindow);
}



/* map the vertical contour line slice control window */
static void map_vslice_window( int dindex, int vindex, int var )
{
   char str[100];
   float min, max;
   char name[10];
   float VSliceInterval, VSliceLowLimit, VSliceHighLimit;
   float row0, col0, row1, col1;
   GuiContext gtx = get_gui_gtx(dindex);

   vis5d_get_ctx_var_range(vindex, var, &min, &max);
   vis5d_get_ctx_var_name( vindex, var, name );
   vis5d_get_vslice(vindex, var, &VSliceInterval, &VSliceLowLimit,
                    &VSliceHighLimit, &row0, &col0, &row1, &col1);

   /* MJK 12.04.98 begin */
   {
      char      units[20];
      int       lunits, lname;

      vis5d_get_var_units (vindex, var, units);
      if ((lunits = strlen (units)) > 0)
      {
         lname = strlen (name);
         if ((lname+lunits) > 5)
            sprintf (str, "%s V. Slice Int. (%s):", name, units);
         else
            sprintf (str, "%s Ver. Slice Interval (%s):", name, units);
      }
      else
         sprintf (str, "%s Ver. Slice Interval:", name);
   }
   /* MJK 12.04.98 end */
   
   LUI_NewLabelChangeText( gtx->vslice_label, str );
   if (VSliceLowLimit==min && VSliceHighLimit==max)
     sprintf( str, "%g", VSliceInterval );
   else
     sprintf( str, "%g (%g,%g)", VSliceInterval,
             VSliceLowLimit, VSliceHighLimit );
   LUI_FieldSetText( gtx->vslice_field, str );

   XMapWindow(GuiDpy,gtx->VSliceWindow);
}






/* MJK 12.04.98 begin */


static void init_currents(void)
{
   int yo;
   for (yo = 0; yo < VIS5D_MAX_DPY_CONTEXTS; yo++){
      do_one_time[yo] = 1;
      cb_graphic[yo] = -1;
      cb_var[yo] = -1;
      cb_vvar[yo] = -1;
      cb_vindex[yo] = -1;

      cb_chvar[yo] = -1;
      cb_chvvar[yo] = -1;
      cb_chvindex[yo] = -1;
   }
}
static int tp_red_slider_cb( LUI_NEWSLIDER *s, float value )
{
   GuiContext gtx = get_gui_gtx(s->context_index);
   int dindex = gtx->context_index;
   int iindex = gtx->current_text_plot_iindex;
   float r, g, b, a;
   int var;
   float spacing, fontx, fonty, fontspace;

   vis5d_get_text_plot( iindex, &var, &spacing,
                &fontx, &fonty, &fontspace);
   
   vis5d_get_color( dindex, VIS5D_TEXTPLOT, (iindex*MAXVARS)+var,
                    &r, &g, &b, &a);
   r = value;

   vis5d_set_color( dindex, VIS5D_TEXTPLOT, (iindex*MAXVARS)+var,
                    r, g, b, a);

   vis5d_signal_redraw( dindex, 1);

   LUI_ButtonMatrixSetColor( gtx->TextPlotButtonMatrix,
                             var, 0, r, g, b );
   return 0;
}
   
static int tp_green_slider_cb( LUI_NEWSLIDER *s, float value )
{
   GuiContext gtx = get_gui_gtx(s->context_index);
   int dindex = gtx->context_index;
   int iindex = gtx->current_text_plot_iindex;
   float r, g, b, a;
   int var;
   float spacing, fontx, fonty, fontspace;

   vis5d_get_text_plot( iindex, &var, &spacing,
                &fontx, &fonty, &fontspace);
   
   vis5d_get_color( dindex, VIS5D_TEXTPLOT, (iindex*MAXVARS)+var,
                    &r, &g, &b, &a);
   g = value;

   vis5d_set_color( dindex, VIS5D_TEXTPLOT, (iindex*MAXVARS)+var,
                    r, g, b, a);

   vis5d_signal_redraw( dindex, 1);
   LUI_ButtonMatrixSetColor( gtx->TextPlotButtonMatrix,
                             var, 0, r, g, b );
   return 0;
}

static int tp_blue_slider_cb( LUI_NEWSLIDER *s, float value )
{
   GuiContext gtx = get_gui_gtx(s->context_index);
   int dindex = gtx->context_index;
   int iindex = gtx->current_text_plot_iindex;
   float r, g, b, a;
   int var;
   float spacing, fontx, fonty, fontspace;

   vis5d_get_text_plot( iindex, &var, &spacing,
                &fontx, &fonty, &fontspace);
   
   vis5d_get_color( dindex, VIS5D_TEXTPLOT, (iindex*MAXVARS)+var,
                    &r, &g, &b, &a);
   b = value;

   vis5d_set_color( dindex, VIS5D_TEXTPLOT, (iindex*MAXVARS)+var,
                    r, g, b, a);

   vis5d_signal_redraw( dindex, 1);
   LUI_ButtonMatrixSetColor( gtx->TextPlotButtonMatrix,
                             var, 0, r, g, b );
   return 0;
}

static int tp_alpha_slider_cb( LUI_NEWSLIDER *s, float value )
{
   GuiContext gtx = get_gui_gtx(s->context_index);
   int dindex = gtx->context_index;
   int iindex = gtx->current_text_plot_iindex;
   float r, g, b, a;
   int var;
   float spacing, fontx, fonty, fontspace;

   vis5d_get_text_plot( iindex, &var, &spacing,
                &fontx, &fonty, &fontspace);

   vis5d_get_color( dindex, VIS5D_TEXTPLOT, (iindex*MAXVARS)+var,
                    &r, &g, &b, &a);
   a = value;

   vis5d_set_color( dindex, VIS5D_TEXTPLOT, (iindex*MAXVARS)+var,
                    r, g, b, a);

   vis5d_signal_redraw( dindex, 1);
   LUI_ButtonMatrixSetColor( gtx->TextPlotButtonMatrix,
                             var, 0, r, g, b );
   return 0;
}

static int tp_colorbar_callback( LUI_COLORBAR *cb, int action )
{
   GuiContext gtx = get_gui_gtx( cb->context_index);
   float *p;
   int dindex = gtx->context_index;
   int iindex = gtx->current_text_plot_iindex;
   int var;
   float spacing, fontx, fonty, fontspace;
   unsigned int *ctable;


   vis5d_get_text_plot( iindex,
                        &var, &spacing, &fontx,
                        &fonty, &fontspace);

   if (action==LUI_RGB_RESET || action==LUI_ALPHA_RESET
       || action==LUI_RGB_SHAPE || action==LUI_ALPHA_CHANGE ||
          action==LUI_RGB_CHANGE) {
      vis5d_get_color_table_address( dindex, VIS5D_TEXTPLOT,
                                     iindex, var, &ctable);
      vis5d_get_color_table_params( dindex, VIS5D_TEXTPLOT,
                                    iindex, var, &p);
      if (action==LUI_RGB_RESET) {
         vis5d_color_table_init_params( p, 1, 0 );
         vis5d_color_table_recompute( ctable, 256, p, 1, 0 );
      }
      else if (action==LUI_ALPHA_RESET) {
         vis5d_color_table_init_params( p, 0, 1 );
         vis5d_color_table_recompute( ctable, 256, p, 0, 1 );
      }
      else if (action==LUI_RGB_SHAPE) {
         vis5d_color_table_recompute( ctable, 256, p, 1, 0 );
      }
      else if (action==LUI_ALPHA_SHAPE) {
         vis5d_color_table_recompute( ctable, 256, p, 0, 1 );
      }
      else if (action==LUI_ALPHA_CHANGE) {
         vis5d_signal_redraw( dindex, 1);
      }
      LUI_ColorBarRedraw( cb );
      vis5d_signal_redraw(dindex, 1);
   }
   return 0;
}
void make_tp_color_window( GuiContext gtx )
{
   int dindex, iindex;
   int subx, suby;
   Window w;
   LUI_NEWBUTTON *b;
   LUI_NEWSLIDER *s;

   dindex = gtx->context_index;
   iindex = gtx->current_text_plot_iindex;

   if (!gtx->tp_color_window){
      gtx->tp_color_window = LUI_CreateWindowAt( LUI_RootWindow, 10,
                                                 800, 380, 182 );
      w = gtx->tp_color_window;
      gtx->tp_color_label = LUI_NewLabelCreate( w, LUI_LEFT, LUI_TOP, 310, 20,
                                             "default label" );
      b = LUI_PushButtonCreate( w, LUI_NEXT_X, LUI_SAME_Y, 60, 20, "Close" );
      b->index = gtx->context_index;   
      LUI_ButtonCallback( b, tp_close_color_cb );
      
      gtx->tp_monocolor = LUI_PushButtonCreate( w, 5, 30, 90, 20, " MonoColor" );
      gtx->tp_monocolor->index = gtx->context_index;   
      LUI_ButtonCallback( gtx->tp_monocolor, tp_monocolor_cb );

      gtx->tp_multicolor = LUI_PushButtonCreate( w, 5, 70, 90, 20, "MultiColor" );
      gtx->tp_multicolor->index = gtx->context_index;   
      LUI_ButtonCallback( gtx->tp_multicolor, tp_multicolor_cb );

   }
 
   w = gtx->tp_color_window;

   subx = 100;
   suby = 25;

   if (!gtx->tp_color_subwin1){
      gtx->tp_color_subwin1 = LUI_CreateWindowAt( w, subx, suby, 280, 200 );
      /* red slider */
      s = LUI_NewSliderCreate( gtx->tp_color_subwin1, 0, 0, 270 );
      gtx->tp_red_slider = s;
      LUI_NewSliderChange( s, "Red", NULL, 0.0, 1.0, 1.0 );
      LUI_NewSliderCallback( s, tp_red_slider_cb );
      gtx->tp_red_slider->context_index = dindex;

      /* green slider */
      s = LUI_NewSliderCreate( gtx->tp_color_subwin1, 0, LUI_NEXT_Y, 270 );
      gtx->tp_green_slider = s;
      LUI_NewSliderChange( s, "Green", NULL, 0.0, 1.0, 1.0 );
      LUI_NewSliderCallback( s, tp_green_slider_cb );
      gtx->tp_green_slider->context_index = dindex;

      /* blue slider */
      s = LUI_NewSliderCreate( gtx->tp_color_subwin1, 0, LUI_NEXT_Y, 270 );
      gtx->tp_blue_slider = s;
      LUI_NewSliderChange( s, "Blue", NULL, 0.0, 1.0, 1.0 );
      LUI_NewSliderCallback( s, tp_blue_slider_cb );
      gtx->tp_blue_slider->context_index = dindex;

      /* alpha slider */
      s = LUI_NewSliderCreate( gtx->tp_color_subwin1, 0, LUI_NEXT_Y, 270 );
      gtx->tp_alpha_slider = s;
      LUI_NewSliderChange( s, "Opacity", NULL, 0.0, 1.0, 1.0 );
      LUI_NewSliderCallback( s, tp_alpha_slider_cb );
      gtx->tp_alpha_slider->context_index = dindex;
   }
   LUI_FrameWidth(4);
   LUI_ColorBarPacking( PACK_COLOR('R','G','B','A') );

   if (!gtx->tp_color_subwin2){
      gtx->tp_color_subwin2 = LUI_CreateWindowAt( w, subx, suby, 280, 200 );
      gtx->tp_colorbar = LUI_ColorBarCreate( gtx->tp_color_subwin2,
                                                    0, 0, 270, 154 );
      LUI_ColorBarCallback( gtx->tp_colorbar, tp_colorbar_callback );
   }
   gtx->tp_colorbar->context_index = gtx->context_index;
   XMapWindow( GuiDpy, gtx->tp_color_subwin2 );

}

void hide_tp_color_window( GuiContext gtx)
{
   XUnmapWindow(GuiDpy, gtx->tp_color_window);
}

void show_tp_color_window( GuiContext gtx, int var)
{
   float r, g, b, a;
   unsigned int *color;
   int numvars;
   int colorvar;
   int status, row;
   unsigned int *table;
   char varname[40], units[20];
   char label[100];
   float *p;
   float min, max;
   int dindex = gtx->context_index;
   int iindex = gtx->current_text_plot_iindex;
 
   /* unmap window in case it's already displayed */
   XUnmapWindow( GuiDpy, gtx->tp_color_subwin1);
   XUnmapWindow( GuiDpy, gtx->tp_color_subwin2);

   vis5d_get_itx_var_name( iindex, var, varname);
   sprintf( label, "%s textplot color:", varname );

   LUI_NewLabelChangeText( gtx->tp_color_label, label );

   vis5d_get_textplot_color_status( iindex, var, &status );

   p = NULL;
   vis5d_get_itx_var_range(iindex, var, &min, &max);
   if (min != max){
      XMapWindow(GuiDpy, gtx->tp_monocolor->window);
      XMapWindow(GuiDpy, gtx->tp_multicolor->window);
   }
   else{
      XUnmapWindow(GuiDpy, gtx->tp_monocolor->window);
      XUnmapWindow(GuiDpy, gtx->tp_multicolor->window);
   }

   if (status == VIS5D_ON){
      
      LUI_ButtonSetState(gtx->tp_multicolor,1);
      LUI_ButtonSetState(gtx->tp_monocolor,0);
      strcpy( units, " ");
      vis5d_get_color_table_address( dindex, VIS5D_TEXTPLOT,
                        iindex, var, &table );
      vis5d_get_color_table_params( dindex, VIS5D_TEXTPLOT,
                        iindex, var, &p);
      if (p != NULL) {
         LUI_ColorBarChange (gtx->tp_colorbar, varname, units,
                          min, max, table, 255, p);
      }
      XMapWindow( GuiDpy, gtx->tp_color_window );
      XMapWindow( GuiDpy, gtx->tp_color_subwin2 );
   }
   else{
      LUI_ButtonSetState(gtx->tp_multicolor,0);
      LUI_ButtonSetState(gtx->tp_monocolor,1);
      vis5d_get_color( dindex, VIS5D_TEXTPLOT, (iindex*MAXVARS)+var,
                       &r,&g,&b,&a );
      LUI_NewSliderSetValue( gtx->tp_red_slider, r );
      LUI_NewSliderSetValue( gtx->tp_green_slider, g );
      LUI_NewSliderSetValue( gtx->tp_blue_slider, b );
      LUI_NewSliderSetValue( gtx->tp_alpha_slider, a );
      XMapWindow( GuiDpy, gtx->tp_color_window );
      XMapWindow( GuiDpy, gtx->tp_color_subwin1 );
   }


}





static int textplotspacing_cb( LUI_FIELD *field )
{
   GuiContext gtx = get_gui_gtx(tp_dindex);
   float  spacing;
   int var, time, numtimes, curtime;
   int iindex = gtx->current_text_plot_iindex;
   float fontx, fonty, fontspace;

   vis5d_get_itx_timestep(iindex, &curtime);
   vis5d_get_text_plot( iindex,
                        &var, &spacing, &fontx,
                        &fonty, &fontspace);
   spacing = LUI_FieldGetDouble( gtx->TextPlotSpacingField );
   vis5d_set_text_plot( iindex,
                        var, spacing,
                        fontx, fonty, fontspace);

   gtx->current_text_plot_spacing = spacing;
   gtx->current_text_plot_fontx = fontx;
   gtx->current_text_plot_fonty = fonty;
   gtx->current_text_plot_fontspace = fontspace;

   vis5d_get_itx_numtimes( iindex, &numtimes);
   for (time = 0; time < numtimes; time++){
      vis5d_make_text_plot( iindex, time, time==curtime);
   }
   vis5d_signal_redraw(tp_dindex, 1);

   return 0;
}
static int textplotfontx_cb( LUI_FIELD *field )
{
   GuiContext gtx = get_gui_gtx(tp_dindex);
   float spacing;
   int curtime, var, time, numtimes;
   float fontx, fonty, fontspace;

   int iindex = gtx->current_text_plot_iindex;

   vis5d_get_itx_timestep(iindex, &curtime);   
   vis5d_get_text_plot( iindex,
                        &var, &spacing, &fontx,
                        &fonty, &fontspace);
   fontx = LUI_FieldGetDouble( gtx->TextPlotFontX );
   vis5d_set_text_plot( iindex,
                        var,  spacing,
                        fontx, fonty, fontspace);

   gtx->current_text_plot_spacing = spacing;
   gtx->current_text_plot_fontx = fontx;
   gtx->current_text_plot_fonty = fonty;
   gtx->current_text_plot_fontspace = fontspace;


   vis5d_get_itx_numtimes( iindex, &numtimes);
   for (time = 0; time < numtimes; time++){
      vis5d_make_text_plot( iindex, time, time==curtime);
   }
   vis5d_signal_redraw(tp_dindex, 1);

   return 0;
}

static int textplotfonty_cb( LUI_FIELD *field )
{
   GuiContext gtx = get_gui_gtx(tp_dindex);
   float spacing;
   int curtime, var, time, numtimes;
   float fontx, fonty, fontspace;

   int iindex = gtx->current_text_plot_iindex;

   vis5d_get_itx_timestep(iindex, &curtime);   
   vis5d_get_text_plot( iindex,
                        &var, &spacing, &fontx,
                        &fonty, &fontspace);
   fonty = LUI_FieldGetDouble( gtx->TextPlotFontY );
   vis5d_set_text_plot( iindex,
                        var, spacing,
                        fontx, fonty, fontspace);

   gtx->current_text_plot_spacing = spacing;
   gtx->current_text_plot_fontx = fontx;
   gtx->current_text_plot_fonty = fonty;
   gtx->current_text_plot_fontspace = fontspace;


   vis5d_get_itx_numtimes( iindex, &numtimes);
   for (time = 0; time < numtimes; time++){
      vis5d_make_text_plot( iindex, time, time==curtime);
   }
   vis5d_signal_redraw(tp_dindex, 1);
   return 0;
}
static int textplotfontspace_cb( LUI_FIELD *field )
{
   GuiContext gtx = get_gui_gtx(tp_dindex);
   float spacing;
   int curtime, var, time, numtimes;
   float fontx, fonty, fontspace;

   int iindex = gtx->current_text_plot_iindex;

   vis5d_get_itx_timestep(iindex, &curtime);   
   vis5d_get_text_plot( iindex,
                        &var, &spacing, &fontx,
                        &fonty, &fontspace);
   fontspace = LUI_FieldGetDouble( gtx->TextPlotFontSpace );
   vis5d_set_text_plot( iindex,
                        var, spacing,
                        fontx, fonty, fontspace);

   gtx->current_text_plot_spacing = spacing;
   gtx->current_text_plot_fontx = fontx;
   gtx->current_text_plot_fonty = fonty;
   gtx->current_text_plot_fontspace = fontspace;


   vis5d_get_itx_numtimes( iindex, &numtimes);
   for (time = 0; time < numtimes; time++){
      vis5d_make_text_plot( iindex, time, time==curtime);
   }
   vis5d_signal_redraw(tp_dindex, 1);
   return 0;
}

                        
static int fontxup_cb( LUI_NEWBUTTON *b )
{
   GuiContext gtx = get_gui_gtx(tp_dindex);
   int iindex = gtx->current_text_plot_iindex; 
   float spacing, fontx, fonty, fontspace;
   int curtime, var, time, numtimes;
 
   vis5d_get_itx_timestep(iindex, &curtime);   
   vis5d_get_text_plot( iindex,
                        &var, &spacing, &fontx,
                        &fonty, &fontspace);
   fontx += 1.0;
   vis5d_set_text_plot( iindex,
                        var, spacing,
                        fontx, fonty, fontspace);

   gtx->current_text_plot_spacing = spacing;
   gtx->current_text_plot_fontx = fontx;
   gtx->current_text_plot_fonty = fonty;
   gtx->current_text_plot_fontspace = fontspace;

   LUI_FieldSetDouble( gtx->TextPlotFontX, fontx);
   vis5d_get_itx_numtimes( iindex, &numtimes);
   for (time = 0; time < numtimes; time++){
      vis5d_make_text_plot( iindex, time, time==curtime);
   }
   vis5d_signal_redraw(tp_dindex, 1);
   return 0;
}

static int fontxdown_cb( LUI_NEWBUTTON *b )
{
   GuiContext gtx = get_gui_gtx(tp_dindex);
   int iindex = gtx->current_text_plot_iindex;
   float spacing, fontx, fonty, fontspace;
   int curtime, var, time, numtimes;

   vis5d_get_itx_timestep(iindex, &curtime);   
   vis5d_get_text_plot( iindex,
                        &var, &spacing, &fontx,
                        &fonty, &fontspace);
   fontx -= 1.0;
   vis5d_set_text_plot( iindex,
                        var, spacing,
                        fontx, fonty, fontspace);

   gtx->current_text_plot_spacing = spacing;
   gtx->current_text_plot_fontx = fontx;
   gtx->current_text_plot_fonty = fonty;
   gtx->current_text_plot_fontspace = fontspace;

   LUI_FieldSetDouble( gtx->TextPlotFontX, fontx);

   vis5d_get_itx_numtimes( iindex, &numtimes);
   for (time = 0; time < numtimes; time++){
      vis5d_make_text_plot( iindex, time, time==curtime);
   }
   vis5d_signal_redraw(tp_dindex, 1);
   return 0;
}

static int tp_close_cb( LUI_NEWBUTTON *b )
{
   GuiContext gtx = get_gui_gtx(tp_dindex);

   XUnmapWindow( GuiDpy, gtx->TextPlotWindow);
   return 0;
}

static int fontyup_cb( LUI_NEWBUTTON *b )
{
   GuiContext gtx = get_gui_gtx(tp_dindex);
   int iindex = gtx->current_text_plot_iindex;
   float spacing, fontx, fonty, fontspace;
   int curtime, var, time, numtimes;

   vis5d_get_itx_timestep(iindex, &curtime);   
   vis5d_get_text_plot( iindex,
                        &var, &spacing, &fontx,
                        &fonty, &fontspace);
   fonty += 1.0;
   vis5d_set_text_plot( iindex,
                        var, spacing,
                        fontx, fonty, fontspace);

   gtx->current_text_plot_spacing = spacing;
   gtx->current_text_plot_fontx = fontx;
   gtx->current_text_plot_fonty = fonty;
   gtx->current_text_plot_fontspace = fontspace;

   LUI_FieldSetDouble( gtx->TextPlotFontY, fonty);

   vis5d_get_itx_numtimes( iindex, &numtimes);
   for (time = 0; time < numtimes; time++){
      vis5d_make_text_plot( iindex, time, time==curtime);
   }
   vis5d_signal_redraw(tp_dindex, 1);
   return 0;
}

static int fontydown_cb( LUI_NEWBUTTON *b )
{
   GuiContext gtx = get_gui_gtx(tp_dindex);
   int iindex = gtx->current_text_plot_iindex;
   float spacing, fontx, fonty, fontspace;
   int curtime, var, time, numtimes;

   vis5d_get_itx_timestep(iindex, &curtime);   
   vis5d_get_text_plot( iindex,
                        &var, &spacing, &fontx,
                        &fonty, &fontspace);
   fonty -= 1.0;
   vis5d_set_text_plot( iindex,
                        var, spacing,
                        fontx, fonty, fontspace);

   gtx->current_text_plot_spacing = spacing;
   gtx->current_text_plot_fontx = fontx;
   gtx->current_text_plot_fonty = fonty;
   gtx->current_text_plot_fontspace = fontspace;

   LUI_FieldSetDouble( gtx->TextPlotFontY, fonty);

   vis5d_get_itx_numtimes( iindex, &numtimes);
   for (time = 0; time < numtimes; time++){
      vis5d_make_text_plot( iindex, time, curtime==time);
   }
   vis5d_signal_redraw(tp_dindex, 1);
   return 0;
}

static int fontspaceup_cb( LUI_NEWBUTTON *b )
{
   GuiContext gtx = get_gui_gtx(tp_dindex);
   int iindex = gtx->current_text_plot_iindex;
   float spacing, fontx, fonty, fontspace;
   int curtime, var, time, numtimes;

   vis5d_get_itx_timestep(iindex, &curtime);   
   vis5d_get_text_plot( iindex,
                        &var, &spacing, &fontx,
                        &fonty, &fontspace);
   fontspace += 5.0;
   vis5d_set_text_plot( iindex,
                        var, spacing,
                        fontx, fonty, fontspace);

   gtx->current_text_plot_spacing = spacing;
   gtx->current_text_plot_fontx = fontx;
   gtx->current_text_plot_fonty = fonty;
   gtx->current_text_plot_fontspace = fontspace;

   LUI_FieldSetDouble( gtx->TextPlotFontSpace, fontspace);

   vis5d_get_itx_numtimes( iindex, &numtimes);
   for (time = 0; time < numtimes; time++){
      vis5d_make_text_plot( iindex, time, time==curtime);
   }
   vis5d_signal_redraw(tp_dindex, 1);
   return 0;
}

static int fontspacedown_cb( LUI_NEWBUTTON *b )
{
   GuiContext gtx = get_gui_gtx(tp_dindex);
   int iindex = gtx->current_text_plot_iindex;
   float spacing, fontx, fonty, fontspace;
   int curtime, var, time, numtimes;

   vis5d_get_itx_timestep(iindex, &curtime);   
   vis5d_get_text_plot( iindex,
                        &var, &spacing, &fontx,
                        &fonty, &fontspace);
   fontspace -= 5.0;
   vis5d_set_text_plot( iindex,
                        var, spacing,
                        fontx, fonty, fontspace);

   gtx->current_text_plot_spacing = spacing;
   gtx->current_text_plot_fontx = fontx;
   gtx->current_text_plot_fonty = fonty;
   gtx->current_text_plot_fontspace = fontspace;

   LUI_FieldSetDouble( gtx->TextPlotFontSpace, fontspace);

   vis5d_get_itx_numtimes( iindex, &numtimes);
   for (time = 0; time < numtimes; time++){
      vis5d_make_text_plot( iindex, time, time==curtime);
   }
   vis5d_signal_redraw(tp_dindex, 1);
   return 0;
}

 

static int TextPlotMatrix_cb( LUI_BUTTON_MATRIX *bm, int row, int col, int button )
{
   int dindex = bm->context_index;
   GuiContext gtx = get_gui_gtx(dindex);
   int iindex = gtx->current_text_plot_iindex;
   int time, curtime, numtimes;
   int var;
   float spacing;
   float fontx, fonty, fontspace;

   vis5d_get_text_plot( iindex, &var, &spacing,
                        &fontx, &fonty, &fontspace);
   vis5d_get_itx_timestep(iindex, &curtime);   
   if (button == Button1){
      if (gtx->current_text_plot_iindex == iindex &&
      gtx->current_text_plot_var == row){
         vis5d_enable_irregular_graphics(iindex, VIS5D_TEXTPLOT, VIS5D_OFF);
         vis5d_set_text_plot(iindex, -1, spacing, fontx, fonty, fontspace);
         LUI_ButtonMatrixSetState( bm, row, col, 0);
         gtx->current_text_plot_var = -1;
         hide_tp_color_window(gtx);
         return 0;
      }
      else if (gtx->current_text_plot_iindex == iindex){
         if (gtx->current_text_plot_var >= 0){
            LUI_ButtonMatrixSetState( bm, gtx->current_text_plot_var, col, 0);
         }
         LUI_ButtonMatrixSetState( bm, row, col, 1);
         vis5d_enable_irregular_graphics(iindex, VIS5D_TEXTPLOT, VIS5D_ON);
         vis5d_set_text_plot(iindex, row, 
                             gtx->current_text_plot_spacing,
                             gtx->current_text_plot_fontx,
                             gtx->current_text_plot_fonty,
                             gtx->current_text_plot_fontspace);
         gtx->current_text_plot_var = row;
         vis5d_get_itx_numtimes( iindex, &numtimes); 
         for (time = 0; time < numtimes; time++){
            vis5d_make_text_plot( iindex, time, time==curtime);
         }
         vis5d_signal_redraw(dindex, 1);
         hide_tp_color_window(gtx);
         return 0;
      }
   }
   else if (button == Button3){
      show_tp_color_window(gtx, var);
      return 0;
   } 
}

int show_text_plot_window( int dindex, int iindex)
{
   GuiContext gtx = get_gui_gtx(dindex);
   char *labels[1];
   int i;
   float spacing;
   int var;
   LUI_NEWBUTTON *b;
   float red, green, blue, alpha;
   
   tp_dindex = dindex;
   vis5d_get_text_plot(iindex, &var,
                       &gtx->current_text_plot_spacing,
                       &gtx->current_text_plot_fontx,
                       &gtx->current_text_plot_fonty,
                       &gtx->current_text_plot_fontspace);

   if (!gtx->TextPlotWindow){
      gtx->TextPlotWindow = LUI_CreateWindowAt( LUI_RootWindow,
                            200, 200, 400, 300);
      LUI_NewLabelCreate(gtx->TextPlotWindow, LUI_LEFT, LUI_TOP,
                   300, 20, "Choose Variable to Plot" );
      b = LUI_PushButtonCreate(gtx->TextPlotWindow, 220, 15, 50, 20, "Close");
      b->context_index = dindex;
      LUI_ButtonCallback( b, tp_close_cb);
      LUI_NewLabelCreate(gtx->TextPlotWindow, LUI_LEFT, 245, 65, 20, "Spacing:");
      gtx->TextPlotSpacingField = LUI_FieldCreate( gtx->TextPlotWindow,
                               72, 245, 30, 20);  
      LUI_FieldCallback( gtx->TextPlotSpacingField, textplotspacing_cb );
      gtx->TextPlotSpacingField->context_index = dindex;
      
      LUI_NewLabelCreate(gtx->TextPlotWindow, 105, 245, 50, 20, "fontX:");
      LUI_NewLabelCreate(gtx->TextPlotWindow, 105, 270, 50, 20, "fontY:");
      LUI_NewLabelCreate(gtx->TextPlotWindow, 215, 245, 80, 20, "fontSpace:");
      gtx->TextPlotFontX = LUI_FieldCreate( gtx->TextPlotWindow,
                               160, 245, 30, 20);
      gtx->TextPlotFontY = LUI_FieldCreate( gtx->TextPlotWindow,
                               160, 270, 30, 20);
      gtx->TextPlotFontSpace = LUI_FieldCreate( gtx->TextPlotWindow,
                               300, 245, 30, 20);
      gtx->TextPlotFontX->context_index = dindex;
      gtx->TextPlotFontY->context_index = dindex;
      gtx->TextPlotFontSpace->context_index = dindex;

      LUI_FieldCallback( gtx->TextPlotFontX, textplotfontx_cb);
      LUI_FieldCallback( gtx->TextPlotFontY, textplotfonty_cb);
      LUI_FieldCallback( gtx->TextPlotFontSpace, textplotfontspace_cb);

      b = LUI_PushButtonCreate( gtx->TextPlotWindow, 192, 244, 17, 11, "+");
      b->context_index = gtx->context_index;
      LUI_ButtonCallback( b, fontxup_cb);
      b = LUI_PushButtonCreate( gtx->TextPlotWindow, 192, 255, 17, 11, "-");
      b->context_index = gtx->context_index;
      LUI_ButtonCallback( b, fontxdown_cb);
      b = LUI_PushButtonCreate( gtx->TextPlotWindow, 192, 269, 17, 11, "+");
      b->context_index = gtx->context_index;
      LUI_ButtonCallback( b, fontyup_cb);
      b = LUI_PushButtonCreate( gtx->TextPlotWindow, 192, 280, 17, 11, "-");
      b->context_index = gtx->context_index;
      LUI_ButtonCallback( b, fontydown_cb);
      b = LUI_PushButtonCreate( gtx->TextPlotWindow, 332, 244, 17, 11, "+");
      b->context_index = gtx->context_index;
      LUI_ButtonCallback( b, fontspaceup_cb);
      b = LUI_PushButtonCreate( gtx->TextPlotWindow, 332, 255, 17, 11, "-");
      b->context_index = gtx->context_index;
      LUI_ButtonCallback( b, fontspacedown_cb);
      b->context_index = gtx->context_index;
     
      
   }

   LUI_FieldSetDouble( gtx->TextPlotSpacingField, gtx->current_text_plot_spacing);
   LUI_FieldSetDouble( gtx->TextPlotFontX, gtx->current_text_plot_fontx);
   LUI_FieldSetDouble( gtx->TextPlotFontY, gtx->current_text_plot_fonty);
   LUI_FieldSetDouble( gtx->TextPlotFontSpace, gtx->current_text_plot_fontspace);

   if (gtx->TextPlotButtonMatrix){
      LUI_ButtonMatrixDestroy(gtx->TextPlotButtonMatrix);
   }

   gtx->TextPlotButtonMatrix = LUI_ButtonMatrixCreate(gtx->TextPlotWindow,
                                10, 30, 200, 206, 1);
   
   for (i = 0; i < gtx->IrregularNumVars[iindex]; i++){
      labels[0] = gtx->IrregularVarName[i][iindex];
      vis5d_get_color( gtx->context_index, VIS5D_TEXTPLOT, iindex*MAXVARS+i,
                         &red, &green, &blue, &alpha); 
      LUI_ButtonMatrixAddRow( gtx->TextPlotButtonMatrix,
                              labels, &red, &green, &blue);
   }
   LUI_ButtonMatrixCallback( gtx->TextPlotButtonMatrix, TextPlotMatrix_cb);
   gtx->TextPlotButtonMatrix->context_index = dindex;
   gtx->current_text_plot_iindex = iindex;
   if (var >= 0){
      LUI_ButtonMatrixSetState(gtx->TextPlotButtonMatrix, var, 0, 1);
   }
   

   XMapWindow( GuiDpy, gtx->TextPlotWindow);
   XMapWindow( GuiDpy, gtx->TextPlotButtonMatrix->mainwindow);
   gtx->current_text_plot_iindex = iindex;
}
   
   


   

/*
 * Display a color_table widget window.
 * Input:  dindex - display context index
 *         vindex - vis5d context index
 *         graphic - either VIS5D_TOPO, VIS5D_CHSLICE, VIS5D_CVSLICE,
 *                   or VIS5D_VOLUME
 *         var - which variable
 */
int show_colorbar_window( int dindex, int vindex, int graphic, int var )
{
   GuiContext gtx = get_gui_gtx(dindex);
   float min, max;
   unsigned int *colors;
   int vvar;
   char name[20], label[100], units[20];
   float *p;
   XWindowAttributes winatts;
   XWindowAttributes winatts2;

   /* SJ 14 Aug 2000 */
   if (vindex < 0 || var < 0) return 0;

   vvar = get_button_ctx_row (dindex, var);

   switch (graphic)
   {
      case VIS5D_TOPO:
         vvar = 0;
         vis5d_get_topo_range (dindex, &min, &max);
         strcpy (units, "km");
         strcpy (label, "Topography");
         break;

      case VIS5D_CHSLICE:
         strcpy (label, "horiz slice ");
         break;

      case VIS5D_CVSLICE:
         strcpy (label, "vert slice ");
         break;

      case VIS5D_VOLUME:
         strcpy (label, "volume ");
         break;

      default:
         return 0;
   }

   if (graphic != VIS5D_TOPO)
   {
      vis5d_get_ctx_var_range (vindex, vvar, &min, &max);
      vis5d_get_var_units (vindex, vvar, units);
      vis5d_get_ctx_var_name (vindex, vvar, name);
      strcat (label, name);
   }
   vis5d_get_color_table_address (dindex, graphic, vindex, vvar, &colors);
   vis5d_get_color_table_params (dindex, graphic, vindex, vvar, &p);

   if (graphic == VIS5D_CHSLICE)
   {
      float     level;

      vis5d_get_chslice (vindex, vvar, &level);
      XGetWindowAttributes( GuiDpy, gtx->chslice_pos_slider->window, &winatts2);
      XGetWindowAttributes( GuiDpy, gtx->CHSliceWindow, &winatts);

      XMoveResizeWindow(GuiDpy, gtx->chslice_pos_slider->window,
        winatts2.x, winatts.height-38, winatts2.width, winatts2.height);
      mod_vpos_slider (dindex, gtx->chslice_pos_slider, vvar, 0, level);

      XMapWindow (GuiDpy, gtx->CHSliceWindow);

      cb_dindex              = dindex;
      cb_chvindex[cb_dindex] = vindex;
      cb_chvar[cb_dindex]    = var;
      cb_chvvar[cb_dindex]   = vvar;
      LUI_ColorBarSetSize( gtx->CHSliceColorbar, winatts.width,
                           winatts.height - 68);
      LUI_ColorBarChange (gtx->CHSliceColorbar, label, units, min, max,
                          colors, 255, p);

      LUI_ColorBarShow( gtx->CHSliceColorbar );
      return 1;
   }
   XMapWindow (GuiDpy, gtx->ColorbarWindow);


   cb_dindex             = dindex;
   cb_vindex[cb_dindex]  = vindex;
   cb_graphic[cb_dindex] = graphic;
   cb_var[cb_dindex]     = var;
   cb_vvar[cb_dindex]    = vvar;
   XGetWindowAttributes( GuiDpy, gtx->ColorbarWindow, &winatts);

   LUI_ColorBarSetSize( gtx->Colorbar, winatts.width,
                     winatts.height-28);
   LUI_ColorBarChange (gtx->Colorbar, label, units, min, max,
                       colors, 255, p);
   LUI_ColorBarShow( gtx->Colorbar );

   return 1;
}



int hide_colorbar_window( int index )
{
   GuiContext gtx = get_gui_gtx(index);

   XUnmapWindow (GuiDpy, gtx->ColorbarWindow);

   cb_graphic[cb_dindex] = -1;
   cb_var[cb_dindex]     = -1;
   cb_vvar[cb_dindex]     = -1;
   cb_vindex[cb_dindex]  = -1;
}



int hide_chcolorbar_window( int index )
{
   GuiContext gtx = get_gui_gtx(index);

   XUnmapWindow (GuiDpy, gtx->CHSliceWindow);

   cb_chvar[cb_dindex]    = -1;
   cb_chvvar[cb_dindex]    = -1;
   cb_chvindex[cb_dindex] = -1;
}

static int tp_close_color_cb( LUI_NEWBUTTON *b)
{
   GuiContext gtx = get_gui_gtx(b->index);
   XUnmapWindow(GuiDpy, gtx->tp_color_window);
   return 0;
}

static int tp_monocolor_cb( LUI_NEWBUTTON *b)
{
   GuiContext gtx = get_gui_gtx(b->index);
   int iindex = gtx->current_text_plot_iindex;
   int curtime, var, time, numtimes;
   float spacing, fontx, fonty, fontspace;

   vis5d_get_itx_timestep(iindex, &curtime);   
   vis5d_get_text_plot( iindex,
                        &var, &spacing, &fontx,
                        &fonty, &fontspace);
   vis5d_set_textplot_color_status( iindex, var, VIS5D_OFF);
   show_tp_color_window( gtx, var);
   vis5d_get_itx_numtimes( iindex, &numtimes);
   for (time = 0; time < numtimes; time++){
      vis5d_make_text_plot( iindex, time, time==curtime);
   }

}

static int tp_multicolor_cb( LUI_NEWBUTTON *b)
{
   GuiContext gtx = get_gui_gtx(b->index);
   int iindex = gtx->current_text_plot_iindex;
   int curtime, var, time, numtimes;
   float spacing, fontx, fonty, fontspace;

   vis5d_get_itx_timestep(iindex, &curtime);   
   vis5d_get_text_plot( iindex,
                        &var, &spacing, &fontx,
                        &fonty, &fontspace);
   vis5d_set_textplot_color_status( iindex, var, VIS5D_ON);   
   show_tp_color_window( gtx, var);
   vis5d_get_itx_numtimes( iindex, &numtimes);
   for (time = 0; time < numtimes; time++){
      vis5d_make_text_plot( iindex, time, curtime==time);
   }
}

 
static int cb_close_cb (LUI_NEWBUTTON *b)
{
    if (b->index == 0)
        hide_colorbar_window (b->context_index);
    else
        hide_chcolorbar_window (b->context_index);
}


/*
 * Set the alpha value for all color table entries for a colored slice.
 * NOTE: the variable var which is passed is the var of the gui_ctx
 */
void set_slice_alpha( int index, int graphic, int vowner, int var, int alpha )
{
   GuiContext gtx = get_gui_gtx(index);
   unsigned int *colors;
   float *params;
   int vvar;

   vvar = get_button_ctx_row(index, var);

   vis5d_get_color_table_address (index, graphic, vowner, vvar, &colors);
   vis5d_get_color_table_params (index, graphic, vowner, vvar, &params);
   vis5d_color_table_set_alpha (params, alpha);
   vis5d_color_table_recompute (colors, 256, params, 0, 1);

   if (((cb_graphic[cb_dindex] == graphic) && (cb_var[cb_dindex] == var)) ||
       ((graphic == VIS5D_CHSLICE) && (cb_chvar[cb_dindex] == var))) {
      /* Change alpha of displayed colorbar widget */
      char label[100], name[20], units[20];
      float min, max;

      vis5d_get_ctx_var_range (vowner, vvar, &min, &max);
      vis5d_get_ctx_var_name (vowner, vvar, name);
      vis5d_get_var_units (vowner, vvar, units);
      if (graphic==VIS5D_CHSLICE) {
         strcpy (label, "horiz slice ");
      }
      else {
         strcpy (label, "vert slice ");
      }

      if (graphic==VIS5D_CHSLICE) {
         sprintf (label, "horiz slice %s", name);
         LUI_ColorBarChange (gtx->CHSliceColorbar, label, units, min, max,
                             colors, 255, params);
      }
      else if (graphic==VIS5D_CVSLICE) {
         sprintf (label, "vert slice %s", name);
         LUI_ColorBarChange (gtx->Colorbar, label, units, min, max,
                             colors, 255, params);
      }
   }
}

/* MJK 12.07.98 end */






/************************************************************/
/* Create the window save requester.                        */
/************************************************************/
static void make_savepic_window( int index )
{
   GuiContext gtx = get_gui_gtx(index);
   LUI_NEWBUTTON *b;
   char **labels;
   int n, formats;

   vis5d_get_image_formats( &formats );

   gtx->SavePicWindow = LUI_CreateWindowAt( LUI_RootWindow, 50, 250, 290,222 );

   LUI_NewLabelCreate( gtx->SavePicWindow, LUI_LEFT, LUI_TOP, 280, 30,
                       "Save Window Image" );

   /* allocate array of 6 char pointers */
   labels = (char **) malloc( 6 * sizeof(char *) );

   /* make labels for radio button and update SaveFormats array */
   n = 0;
   if (formats & VIS5D_RGB) {
      labels[n] = strdup("SGI .rgb");
      gtx->SaveFormats[n] = VIS5D_RGB;
      n++;
   }
   if (formats & VIS5D_GIF) {
      labels[n] = strdup("GIF");
      gtx->SaveFormats[n] = VIS5D_GIF;
      n++;
   }
   if (formats & VIS5D_PS) {
      labels[n] = strdup("PostScript");
      gtx->SaveFormats[n] = VIS5D_PS;
      n++;
   }
   if (formats & VIS5D_COLOR_PS) {
      labels[n] = strdup("Color PostScript");
      gtx->SaveFormats[n] = VIS5D_COLOR_PS;
      n++;
   }
   if (formats & VIS5D_XWD) {
      labels[n] = strdup("xwd (X Window Dump)");
      gtx->SaveFormats[n] = VIS5D_XWD;
      n++;
   }
   /* MJK 11.19.98 */      
   if (formats & VIS5D_PPM) {
      labels[n] = strdup("24 bit ppm");
      gtx->SaveFormats[n] = VIS5D_PPM;
      n++;    
   }

   gtx->SavePicRadio = LUI_RadioCreate( gtx->SavePicWindow,
                                        LUI_LEFT, LUI_NEXT_Y, 280,
                                        n, labels );

   LUI_NewLabelCreate( gtx->SavePicWindow, LUI_LEFT, LUI_NEXT_Y, 280, 30,
                     "File name: " );

   gtx->SavePicField = LUI_FieldCreate( gtx->SavePicWindow,
                                   LUI_LEFT, LUI_NEXT_Y, 280, 26 );

   b = LUI_PushButtonCreate( gtx->SavePicWindow, 30, LUI_NEXT_Y,
                             100, 26, "Save" );
   LUI_ButtonCallback( b, ok_cb );

   b = LUI_PushButtonCreate( gtx->SavePicWindow, 160, LUI_SAME_Y,
                             100, 26, "Cancel" );
   LUI_ButtonCallback( b, cancel_cb );

   XUnmapWindow( GuiDpy, gtx->SavePicWindow );
}


/*********************************************************
   Input: index - display or gui index
          vindex - vis5d data ctx

   Output: return 1 if the display contains the data set
           vindex, or return 0 if it does not.
***********************************************************/
int is_valid( int index, int vindex)
{
   GuiContext gtx = get_gui_gtx(index);
   int yo, spandex;
 
   for (yo=0; yo < gtx->how_many_regular_contexts; yo++){
      spandex = gtx->array_of_ctxs[yo];
      if (vindex == spandex){
         return 1;
      }
   }
   return 0;
}
/* BUG FIX MJK 8.10.98 */
/* added this function so when adding a display to a group */
/* all the other graphics in that group are turned off also */
/* this is so that all graphics are in sync. I could have serched */
/* though every graphics and mode in the other displays and set the */
/* newly group display to the same but that's too much work for now */
/* maybe do it later */
void group_turn_off_everything( int index )
{
   int dyo, dhowmany, dwhichones[VIS5D_MAX_CONTEXTS];

   vis5d_get_num_of_dtxs_in_group( index, &dhowmany, dwhichones);
   for (dyo = 0; dyo < dhowmany; dyo++){
      turn_off_everything( dwhichones[dyo] );
   }
}

void turn_off_everything( int index )
{
   GuiContext gtx = get_gui_gtx(index);
   int i, j, ws;
   int ctxindex, ctxrow, yo;
   float r, g, b, a;
   
   if (gtx->how_many_regular_contexts>0){
   /* take care of the button matix! */
      for (i=0; i < gtx->total_numvars; i++){
         for (j=0;j<gtx->Columns;j++) {
            int ctxi, what;
            switch (j) {
               case 0:  what = VIS5D_ISOSURF;  break;
               case 1:  what = VIS5D_HSLICE;   break;
               case 2:  what = VIS5D_VSLICE;   break;
               case 3:  what = VIS5D_CHSLICE;  break;
               case 4:  what = VIS5D_CVSLICE;  break;
               case 5:  what = VIS5D_VOLUME;   break;
               default:
                  /* better never happen! */
                  abort();
            }
            ctxrow = get_button_ctx_row(index, i);
            ctxindex = get_button_ctx_index(index, i);
            vis5d_enable_graphics( ctxindex, what, ctxrow, VIS5D_OFF);
            LUI_ButtonMatrixSetState( gtx->ButtonMatrix, i, j, 0 );
         }
      }

      vis5d_set_volume( index, -1, -1 );
      gtx->CurrentVolume = -1;
      gtx->CurrentVolume = -1;
    
      /* get those wind slices too! */
      for (ws=0;ws<VIS5D_WIND_SLICES;ws++) {
         vis5d_enable_graphics(gtx->array_of_ctxs[0], VIS5D_HWIND, ws, VIS5D_OFF);
         LUI_ButtonSetState( gtx->hwind_button[ws], 0 );
         vis5d_enable_graphics(gtx->array_of_ctxs[0], VIS5D_VWIND, ws, VIS5D_OFF);
         LUI_ButtonSetState( gtx->vwind_button[ws], 0 );
      }
      vis5d_enable_graphics(gtx->array_of_ctxs[0], VIS5D_HSTREAM, 0, VIS5D_OFF);
      vis5d_enable_graphics(gtx->array_of_ctxs[0], VIS5D_VSTREAM, 0, VIS5D_OFF);
      LUI_ButtonSetState( gtx->hstream_button[0], 0 );
      LUI_ButtonSetState( gtx->vstream_button[0], 0 );
     
      for (i=0;i<VIS5D_TRAJ_SETS;i++) {
         vis5d_enable_graphics(gtx->tuowner, VIS5D_TRAJ, i, VIS5D_OFF);
         LUI_ButtonState( gtx->TrajButton[i], 0);
      } 

      if (gtx->SoundCtrlWindow){
         XUnmapWindow( GuiDpy, gtx->SoundCtrlWindow );
      }
      LUI_ButtonState( gtx->normalBUTTON, 1 );
      if (gtx->total_ctx_numtimes>1) {
         LUI_ButtonState( gtx->trajectoryBUTTON, 0);
      }
      gtx->cur_isosurf = gtx->cur_isosurfvindex = -1;
      gtx->cur_hslice = gtx->cur_hslicevindex = -1;
      gtx->cur_vslice = gtx->cur_vslicevindex = -1;
      gtx->cur_hwind = gtx->cur_vwind = -1;
      gtx->cur_hstream = gtx->cur_vstream = -1;
      cb_graphic[index] = -1;
      cb_var[index] = -1;
      cb_chvar[index] = -1;
      cb_vvar[index] = -1;
      cb_chvvar[index] = -1;
      

      gtx->cur_isosurfmap = 0;
      gtx->cur_hslicemap = 0;    
      gtx->cur_vslicemap = 0;    
      gtx->cur_hwindmap = 0;    
      gtx->cur_vwindmap = 0;    
      gtx->cur_hstreammap = 0;    
      gtx->cur_vstreammap = 0;    
      
      hide_isocolor_window( gtx );
      hide_rgb_sliders( gtx );
   }
   
   if (gtx->how_many_irregular_contexts>0){
      for (i = 0; i < gtx->how_many_irregular_contexts; i++){
         vis5d_enable_irregular_graphics(gtx->array_of_itxs[i],
                             VIS5D_TEXTPLOT, VIS5D_OFF);
         if (gtx->TextPlotButtonMatrix){
            for (j = 0; j < gtx->IrregularNumVars[gtx->array_of_itxs[i]]; j++){
               LUI_ButtonMatrixSetState(gtx->TextPlotButtonMatrix,
                                        j, 0, 0);
            }
         }
      }
      gtx->current_text_plot_var = -1;
   }

   gtx->topoBUTTON->state = 0;
   gtx->map_button->state = 0;
   gtx->boxBUTTON->state = 1;
   gtx->clockBUTTON->state = 1;
   gtx->gridBUTTON->state = 0;
   gtx->contBUTTON->state = 1;
   gtx->reverseBUTTON->state = 0;
   gtx->perspec_button->state = 0;

   vis5d_graphics_mode(index, VIS5D_TOPO, VIS5D_OFF);
   vis5d_graphics_mode(index, VIS5D_MAP, VIS5D_OFF);
   vis5d_graphics_mode(index, VIS5D_BOX, VIS5D_ON);
   vis5d_graphics_mode(index, VIS5D_CLOCK,VIS5D_ON);
   vis5d_graphics_mode(index, VIS5D_GRID_COORDS,VIS5D_OFF);
   vis5d_graphics_mode(index, VIS5D_CONTOUR_NUMBERS,VIS5D_ON);
   vis5d_graphics_mode(index, VIS5D_REVERSE,VIS5D_OFF);
   vis5d_graphics_mode(index, VIS5D_PERSPECTIVE,VIS5D_OFF);

   gtx->MouseMode = MM_NORMAL;
   LUI_LabelChangeText( gtx->ModeInfoLabel, 6, ModeInfo1 );
   vis5d_graphics_mode(index, VIS5D_CURSOR, VIS5D_OFF);
   vis5d_graphics_mode(index, VIS5D_PROBE, VIS5D_OFF);
   vis5d_graphics_mode(index, VIS5D_SOUND, VIS5D_OFF);
   vis5d_graphics_mode(index, VIS5D_CLIP, VIS5D_OFF);

   LUI_ButtonState( gtx->sliceBUTTON, 0);
   LUI_ButtonState( gtx->labelBUTTON, 0);
   LUI_ButtonState( gtx->probeBUTTON, 0);
   LUI_ButtonState( gtx->soundingBUTTON, 0);
   LUI_ButtonState( gtx->clippingBUTTON, 0);

   hide_widgets(index);
   
}


/* MJK 12.01.98 */
void update_button_matrix( int index )
{
   GuiContext gtx = get_gui_gtx(index);
   int ctxrow, ctxindex, yo, i, j;
   float r, g, b, a;

   for (yo=0; yo < gtx->how_many_regular_contexts; yo++){
      for (i=0; i < gtx->total_numvars; i++){
         for (j=0;j<gtx->Columns-1;j++) {
            int ctxi, what;
            switch (j) {
               case 0:  what = VIS5D_ISOSURF;  break;
               case 1:  what = VIS5D_HSLICE;   break;
               case 2:  what = VIS5D_VSLICE;   break;
               case 3:  what = VIS5D_CHSLICE;  break;
               case 4:  what = VIS5D_CVSLICE;  break;
               default:
                  /* better never happen! */
                  abort();
            }
            ctxrow = get_button_ctx_row(index, i);
            ctxindex = get_button_ctx_index(index, i);
            vis5d_get_color(index, what, ctxindex*MAXVARS+ctxrow,
                            &r, &g, &b, &a );
            LUI_ButtonMatrixSetColor( gtx->ButtonMatrix, i, what, r, g, b );
            if (vis5d_enable_graphics( ctxindex, what, ctxrow, VIS5D_GET )){
               LUI_ButtonMatrixSetState( gtx->ButtonMatrix, i, j, 1 );
            }
            else{
               LUI_ButtonMatrixSetState( gtx->ButtonMatrix, i, j, 0 );
            }
         }
      }
   }
}




/* This will update the button matrix, and if double_check = 1 */
/* then it will set all button sliders and windows to off but  */
/* then check to see if any should be on.                      */
/* This take care of: GUI BUTTON MATRIX, SLIDERS, RGB CONTROLS.*/
/* Call. show_widgets after this.                              */
/* WLH 13 Oct 98
static void update_button_states( int index, int double_check )
*/
/* WLH 13 Oct 98 */
void update_button_states( int index, int double_check )
{
   GuiContext gtx = get_gui_gtx(index);
   int i, j, ws;
   int ctxindex, ctxrow, yo;
   float r, g, b, a;
   int volo, volvar;

   if (gtx->how_many_regular_contexts > 0){
      if (double_check){
         gtx->cur_isosurf = -1;
         gtx->cur_isosurfvindex = -1;
         gtx->cur_isosurfmap = 0;
         gtx->cur_hslice = -1;  
         gtx->cur_hslicevindex = -1;  
         gtx->cur_hslicemap = 0;  
         gtx->cur_vslice = -1;    
         gtx->cur_vslicevindex = -1;    
         gtx->cur_vslicemap = 0;    
         cb_var[index] = -1;      
         cb_vvar[index] = -1;
         cb_vindex[index] = -1;                         
         cb_graphic[index] = -1;
         cb_chvar[index] = -1;
         cb_chvvar[index] = -1;
         cb_chvindex[index] = -1;
      }
    
      vis5d_get_volume( index, &volo, &volvar);
      /* redraw all buttons with restored colors */
      for (yo=0; yo < gtx->how_many_regular_contexts; yo++){
         for (i=0; i < gtx->total_numvars; i++){
            for (j=0;j<gtx->Columns;j++) {
               int ctxi, what;
               switch (j) {
                  case 0:  what = VIS5D_ISOSURF;  break;
                  case 1:  what = VIS5D_HSLICE;   break;
                  case 2:  what = VIS5D_VSLICE;   break;
                  case 3:  what = VIS5D_CHSLICE;  break;
                  case 4:  what = VIS5D_CVSLICE;  break;
                  case 5:  what = VIS5D_VOLUME;   break;
                  default:
                     /* better never happen! */
                     abort();
               }
               ctxrow = get_button_ctx_row(index, i);
               ctxindex = get_button_ctx_index(index, i);
               vis5d_get_color(index, what, ctxindex*MAXVARS+ctxrow,
                               &r, &g, &b, &a );
               LUI_ButtonMatrixSetColor( gtx->ButtonMatrix, i, what, r, g, b );
               if (vis5d_enable_graphics( ctxindex, what, ctxrow, VIS5D_GET ) && (j != 5 ||
                   (j==5 && ctxindex==volo && ctxrow==volvar))){
                  
                  LUI_ButtonMatrixSetState( gtx->ButtonMatrix, i, j, 1 );
                  if (double_check){
                     switch (j) {
                        case 0:
                           gtx->cur_isosurf = ctxrow;
                           gtx->cur_isosurfvindex = ctxindex;
                           gtx->cur_isosurfmap = 1;
                           break;
                        case 1:
                           gtx->cur_hslice = ctxrow;
                           gtx->cur_hslicevindex = ctxindex;
                           gtx->cur_hslicemap = 1;
                           break;
                        case 2:
                           gtx->cur_vslice = ctxrow;  
                           gtx->cur_vslicevindex = ctxindex;  
                           gtx->cur_vslicemap = 1;  
                           break;
                        case 3:
                           cb_chvar[index] = ctxrow;    
                           cb_chvindex[index] = ctxindex;    
                           break;
                        case 4:
                           cb_chvvar[index] = ctxrow;
                           cb_chvar[index] = i;      
                           cb_chvindex[index] = ctxindex;                         
                           break;
                        case 5:
                           cb_var[index] = i;        
                           cb_vvar[index] = ctxrow;
                           cb_vindex[index] = ctxindex;                           
                           cb_graphic[index] = VIS5D_VOLUME;
                           break;
                        default:
                           /* better never happen! */
                           abort();
                     }
                  }
               }
               else{
                  LUI_ButtonMatrixSetState( gtx->ButtonMatrix, i, j, 0 );
               }
            }
         }
      }
      if (double_check){
         gtx->cur_hwind = -1;
         gtx->cur_hwindmap = 0;
         gtx->cur_vwind = -1;
         gtx->cur_vwindmap = 0;
         gtx->cur_hstream = -1;
         gtx->cur_hstreammap = 0;
         gtx->cur_vstream = -1;
         gtx->cur_vstreammap = 0;
         gtx->uvw_map = 0;
         hide_isocolor_window( gtx );
         hide_rgb_sliders( gtx );
      }

      /* update wind slice buttons */
      for (ws=0;ws<VIS5D_WIND_SLICES;ws++) {
         /* color */
         vis5d_get_color( index, VIS5D_HWIND, ws, &r, &g, &b, &a );
         LUI_ButtonColor( gtx->hwind_button[ws], r, g, b );
         vis5d_get_color( index, VIS5D_VWIND, ws, &r, &g, &b, &a );
         LUI_ButtonColor( gtx->vwind_button[ws], r, g, b );
         /* on/off status */
         if (vis5d_enable_graphics(gtx->array_of_ctxs[0], VIS5D_HWIND, ws, VIS5D_GET)) {
            LUI_ButtonSetState( gtx->hwind_button[ws], 1 );
            if (double_check){
               gtx->cur_hwind = ws;
               gtx->cur_hwindmap = 1;
            }
         }
         else {
            LUI_ButtonSetState( gtx->hwind_button[ws], 0 );
         }
         if (vis5d_enable_graphics(gtx->array_of_ctxs[0], VIS5D_VWIND, ws, VIS5D_GET)) {
            LUI_ButtonSetState( gtx->vwind_button[ws], 1 );
            if (double_check){
               gtx->cur_vwind = ws;
               gtx->cur_vwindmap = 1;
            }
         }
         else {
            LUI_ButtonSetState( gtx->vwind_button[ws], 0 );
         }
      }
      ws = 0;
      vis5d_get_color( index, VIS5D_HSTREAM, ws, &r, &g, &b, &a );
      LUI_ButtonColor( gtx->hstream_button[ws], r, g, b );
      vis5d_get_color( index, VIS5D_VSTREAM, ws, &r, &g, &b, &a );
      LUI_ButtonColor( gtx->vstream_button[ws], r, g, b );
      if (vis5d_enable_graphics(gtx->array_of_ctxs[0], VIS5D_HSTREAM, ws, VIS5D_GET)) {
         LUI_ButtonSetState( gtx->hstream_button[ws], 1 );
         if (double_check){      
            gtx->cur_hstream = ws;
            gtx->cur_hstreammap = 1;
         }
      }
      else {
        LUI_ButtonSetState( gtx->hstream_button[ws], 0 );
      }
      if (vis5d_enable_graphics(gtx->array_of_ctxs[0], VIS5D_VSTREAM, ws, VIS5D_GET)) {
        LUI_ButtonSetState( gtx->vstream_button[ws], 1 );
         if (double_check){            
            gtx->cur_vstream = ws;
            gtx->cur_vstreammap = 1;
         }
      }
      else {
        LUI_ButtonSetState( gtx->vstream_button[ws], 0 );
      }
   }
}   


void show_widgets( int index )
{
   GuiContext gtx = get_gui_gtx(index);
   int i, j;
   int spandex;
   int ctxindex, ctxrow, yo, gindex;
   int dhowmany, dwhichones[VIS5D_MAX_CONTEXTS];
   int dyo;


   cb_dindex = index;
   vis5d_get_display_group( index, &gindex );
   ctxindex = gtx->array_of_ctxs[0];
   gtx->group_index = gindex;
   if (gtx->how_many_regular_contexts <1){
      return;
   }

   map_map_window(index, vis5d_graphics_mode (index, VIS5D_MAP, VIS5D_GET));
   if (gtx->cur_isosurf>=0 && gtx->cur_isosurfmap == 1 &&
       is_valid(index, gtx->cur_isosurfvindex)) {
      map_isosurf_window( index, gtx->cur_isosurfvindex, gtx->cur_isosurf );
   }
  
   if (gtx->cur_hslice>=0  && gtx->cur_hslicemap == 1 &&
       is_valid(index, gtx->cur_hslicevindex)) {
      map_hslice_window( index, gtx->cur_hslicevindex, gtx->cur_hslice );
   }

   if (gtx->cur_vslice>=0  && gtx->cur_vslicemap == 1 &&
       is_valid(index, gtx->cur_vslicevindex)) {
      map_vslice_window( index, gtx->cur_vslicevindex, gtx->cur_vslice );
   }

   if (gtx->uvw_map){
      show_uvw_widget( gtx );
   }

   if (cb_graphic[cb_dindex]>=0 && cb_var[cb_dindex]>=0 &&
       is_valid( index, cb_vindex[cb_dindex])) {
      show_colorbar_window( index, cb_vindex[cb_dindex],
                            cb_graphic[cb_dindex], cb_var[cb_dindex] );
   }

   if (cb_chvar[cb_dindex]>=0 &&
       is_valid( index, cb_chvindex[cb_dindex])) {
      show_colorbar_window( index, cb_chvindex[cb_dindex],
                            VIS5D_CHSLICE, cb_chvar[cb_dindex] );
   }

   refresh_isocolor_window( gtx );

   refresh_rgb_sliders2( gtx );

   if (gtx->MouseMode==MM_TRAJ && gtx->TrajWindow){
      XMapWindow( GuiDpy, gtx->TrajWindow );
   }
   if (gtx->group_index >= 1 && gtx->MouseMode==MM_SOUND){
      vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
      for (dyo = 0; dyo < dhowmany; dyo++){
         vis5d_map_sndwindow(dwhichones[dyo]);
         vis5d_draw_sounding_only( dwhichones[dyo], 1);
      }
   }
   else if (gtx->MouseMode==MM_SOUND){
/* MJK 3.9.99 */
      vis5d_map_sndwindow( index );
      vis5d_draw_sounding_only( index, 1);
   }
   /* Go through some of the Cp_window buttons */
   if (gtx->GoTime == 1 || gtx->GoTime == -1){
      gtx->animateBUTTON->state = 1;
   }
   else{
      gtx->animateBUTTON->state = 0;
   }
   gtx->topoBUTTON->state = vis5d_graphics_mode(index, VIS5D_TOPO, VIS5D_GET);
   gtx->map_button->state = vis5d_graphics_mode(index, VIS5D_MAP, VIS5D_GET);
   gtx->boxBUTTON->state = vis5d_graphics_mode(index, VIS5D_BOX, VIS5D_GET);
   gtx->clockBUTTON->state = vis5d_graphics_mode(index, VIS5D_CLOCK,VIS5D_GET);
   gtx->gridBUTTON->state = vis5d_graphics_mode(index, VIS5D_GRID_COORDS,VIS5D_GET);
   gtx->contBUTTON->state = vis5d_graphics_mode(index, VIS5D_CONTOUR_NUMBERS,VIS5D_GET);
   gtx->reverseBUTTON->state = vis5d_graphics_mode(index, VIS5D_REVERSE,VIS5D_GET);
   gtx->perspec_button->state = vis5d_graphics_mode(index, VIS5D_PERSPECTIVE,VIS5D_GET);
   gtx->legendsBUTTON->state = vis5d_graphics_mode(index, VIS5D_LEGENDS, VIS5D_GET);

   /* MJK 12.04.98 begin */
   {
      gtx->animateBUTTON->state = (gtx->GoTime != 0);
      if (gtx->map_button)
      {
         float  r, g, b, a;

         if (vis5d_enable_sfc_map (index, VIS5D_GET))
            vis5d_get_color (index, VIS5D_DARK_MAP, 0, &r, &g, &b, &a);
         else
            vis5d_get_color (index, VIS5D_LIGHT_MAP, 0, &r, &g, &b, &a);
         LUI_ButtonColor (gtx->map_button, r, g, b);
         gtx->map_button->state = vis5d_graphics_mode (index, VIS5D_MAP,
                                                       VIS5D_GET);
      }

      if ((gtx->cur_hwind >= 0) && (gtx->cur_hwindmap == 1))
      {
         XUnmapWindow (GuiDpy, gtx->HWindWindow);
         map_hwind_window (index, gtx->cur_hwind);
      }
      if ((gtx->cur_hstream >= 0) && (gtx->cur_hstreammap == 1))
      {
         XUnmapWindow (GuiDpy, gtx->HWindWindow);
         map_hstream_window (index, gtx->cur_hstream);
      }
      if ((gtx->cur_vwind >= 0) && (gtx->cur_vwindmap == 1))
      {
         XUnmapWindow (GuiDpy, gtx->WindWindow);
         map_vwind_window (index, gtx->cur_vwind);
      }
      if ((gtx->cur_vstream >= 0) && (gtx->cur_vstreammap == 1))
      {
         XUnmapWindow (GuiDpy, gtx->WindWindow);
         map_vstream_window (index, gtx->cur_vstream);
      }

   /* note that the hslice, hwind, and hstream SFC buttons were reset
    * when their windows were mapped.
    */

   }
   {
      float     level;

      vis5d_get_flatmap_level (ctxindex, &level);
      update_vpos_slider (index, VIS5D_MAP, -1, level);
   }
   /* MJK 12.07.98 */
   /*
   update_button_states (index);
   */
   update_snd_widgets (index);
   /* MJK 12.04.98 end */

 }   



void hide_widgets( int index )
{
   GuiContext gtx = get_gui_gtx2(index);
   int  howmany, whichones[VIS5D_MAX_CONTEXTS];

   if (!gtx){
      return;
   }
   vis5d_get_num_of_ctxs_in_display( index, &howmany, whichones);
   if (howmany > 0){
      XUnmapWindow( GuiDpy, gtx->IsoWindow );
      XUnmapWindow( GuiDpy, gtx->HSliceWindow );
      XUnmapWindow( GuiDpy, gtx->VSliceWindow );
      XUnmapWindow( GuiDpy, gtx->WindWindow );
      if (gtx->uvw_window){
         XUnmapWindow( GuiDpy, gtx->uvw_window);
      }

      LUI_ColorBarHide( gtx->Colorbar );

      XUnmapWindow( GuiDpy, gtx->isocolor_window);

      XUnmapWindow( GuiDpy, gtx->rgb_sliders_window );

      XUnmapWindow(GuiDpy, gtx->TrajWindow );

      /* MJK 12.04.98 */
      XUnmapWindow (GuiDpy, gtx->MapWindow);
      XUnmapWindow (GuiDpy, gtx->CHSliceWindow);
      XUnmapWindow (GuiDpy, gtx->HWindWindow);
      XUnmapWindow (GuiDpy, gtx->ColorbarWindow);



      vis5d_unmap_sndwindow( index);
      XUnmapWindow( GuiDpy, gtx->SoundCtrlWindow );
      XUnmapWindow( GuiDpy, gtx->VerifyWindow );
      XUnmapWindow( GuiDpy, gtx->AlertWindow );
      if (gtx->CloneWindow) {
         XUnmapWindow( GuiDpy, gtx->CloneWindow );
      }
      if (gtx->WindWindow) {
         XUnmapWindow( GuiDpy, gtx->WindWindow );
      }
      if (gtx->ExprWindow){
         XUnmapWindow( GuiDpy, gtx->ExprWindow );
      }
   }
   vis5d_get_num_of_itxs_in_display( index, &howmany, whichones);
   if (howmany > 0){
      if (gtx->TextPlotWindow){
         XUnmapWindow(GuiDpy, gtx->TextPlotWindow);
      }
   }
}

/*
 * This is called after we execute a Tcl script, do a RESTORE, or compute
 * a new type-in expression.  It updates the color and on/off state of
 * all GUI buttons.
 */
static void reset_widgets( int index )
{
   int i, j, time, var, ws, numvars;
   GuiContext gtx = get_gui_gtx(index);
   float r, g, b, a;

   vis5d_get_ctx_numvars( index, &numvars );
   if (gtx->cur_isosurf>=0) {
      XUnmapWindow( GuiDpy, gtx->IsoWindow );
      map_isosurf_window( index, gtx->cur_isosurfvindex, gtx->cur_isosurf ); 
   }

   if (gtx->cur_hslice>=0) {
      XUnmapWindow( GuiDpy, gtx->HSliceWindow );
      map_hslice_window( index, gtx->cur_hslicevindex, gtx->cur_hslice ); 
   }

   if (gtx->cur_vslice>=0) {
      XUnmapWindow( GuiDpy, gtx->VSliceWindow );
      map_vslice_window( index, gtx->cur_vslicevindex, gtx->cur_vslice ); 
   }

   if (cb_graphic[cb_dindex]>=0 && cb_var[cb_dindex]>=0) {
      show_colorbar_window( index, cb_vindex[cb_dindex],
                            cb_graphic[cb_dindex], cb_var[cb_dindex] ); 
   }

   if (cb_chvar[cb_dindex]>=0) {
      show_colorbar_window( index, cb_chvindex[cb_dindex], 
                            VIS5D_CHSLICE, cb_chvar[cb_dindex] ); 
   }


   if (gtx->cur_hwind>=0 || gtx->cur_vwind>=0 ||
       gtx->cur_hstream>=0 || gtx->cur_vstream>=0) {
      XUnmapWindow( GuiDpy, gtx->WindWindow );
      if (gtx->cur_hwind>=0) {
        map_hwind_window( index, gtx->cur_hwind );
      }
      else if (gtx->cur_vwind>=0) {
        map_vwind_window( index, gtx->cur_vwind );
      }
      else if (gtx->cur_hstream>=0) {
        map_hstream_window( index, gtx->cur_hstream );
      }
      else {
        map_vstream_window( index, gtx->cur_vstream );
      }
   }

   refresh_isocolor_window( gtx );
   refresh_rgb_sliders( gtx );

   /* redraw all buttons with restored colors */
   for (i=0;i<numvars;i++) {
      for (j=0;j<gtx->Columns;j++) {
         float r, g, b, a;
         int what;
         switch (j) {
            case 0:  what = VIS5D_ISOSURF;  break;
            case 1:  what = VIS5D_HSLICE;   break;
            case 2:  what = VIS5D_VSLICE;   break;
            case 3:  what = VIS5D_CHSLICE;  break;
            case 4:  what = VIS5D_CVSLICE;  break;
            case 5:  what = VIS5D_VOLUME;   break;
            default:
               /* better never happen! */
               abort();
         }
         vis5d_get_color( index, what, i, &r, &g, &b, &a );
         LUI_ButtonMatrixSetColor( gtx->ButtonMatrix, i, what, r, g, b );
         if (vis5d_enable_graphics( index, what, i, VIS5D_GET )) {
            LUI_ButtonMatrixSetState( gtx->ButtonMatrix, i, j, 1 );
         }
         else {
            LUI_ButtonMatrixSetState( gtx->ButtonMatrix, i, j, 0 );
         }
      }
   }

   if (gtx->perspec_button != NULL) {
     int PerspecFlag;
     PerspecFlag = vis5d_graphics_mode(index, VIS5D_PERSPECTIVE, VIS5D_GET);
     LUI_ButtonSetState(gtx->perspec_button, PerspecFlag);
   }

   /* trajectory colors */
   for (i=0;i<VIS5D_TRAJ_SETS;i++) {
      vis5d_get_color( index, VIS5D_TRAJ, i, &r, &g, &b, &a );
      LUI_ButtonSetColor( gtx->TrajButton[i], (int)(r*255.0), (int)(g*255.0),
                         (int)(b*255.0) );
   }

   {
     float UserTrajStep, UserTrajLength;
     int RibbonFlag;
     vis5d_get_traj(index, &UserTrajStep, &UserTrajLength, &RibbonFlag);
     LUI_ButtonSetState( gtx->TrajRibbonButton, RibbonFlag);
     LUI_FieldSetDouble( gtx->TrajStepField, UserTrajStep );
     LUI_FieldSetDouble( gtx->TrajLenField, UserTrajLength );
   }

   /* update wind slice buttons */
   for (ws=0;ws<VIS5D_WIND_SLICES;ws++) {
      /* color */
      vis5d_get_color( index, VIS5D_HWIND, ws, &r, &g, &b, &a );
      LUI_ButtonColor( gtx->hwind_button[ws], r, g, b );
      vis5d_get_color( index, VIS5D_VWIND, ws, &r, &g, &b, &a );
      LUI_ButtonColor( gtx->vwind_button[ws], r, g, b );
      /* on/off status */
      if (vis5d_enable_graphics(index, VIS5D_HWIND, ws, VIS5D_GET)) {
         LUI_ButtonSetState( gtx->hwind_button[ws], 1 );
      }
      else {
         LUI_ButtonSetState( gtx->hwind_button[ws], 0 );
      }
      if (vis5d_enable_graphics(index, VIS5D_VWIND, ws, VIS5D_GET)) {
         LUI_ButtonSetState( gtx->vwind_button[ws], 1 );
         }
      else {
         LUI_ButtonSetState( gtx->vwind_button[ws], 0 );
      }
   }
   /* update stream slice buttons */
   ws = 0;
   vis5d_get_color( index, VIS5D_HSTREAM, ws, &r, &g, &b, &a );
   LUI_ButtonColor( gtx->hstream_button[ws], r, g, b );
   vis5d_get_color( index, VIS5D_VSTREAM, ws, &r, &g, &b, &a );
   LUI_ButtonColor( gtx->vstream_button[ws], r, g, b );
   if (vis5d_enable_graphics(index, VIS5D_HSTREAM, ws, VIS5D_GET)) {
     LUI_ButtonSetState( gtx->hstream_button[ws], 1 );
   }
   else {
     LUI_ButtonSetState( gtx->hstream_button[ws], 0 );
   }
   if (vis5d_enable_graphics(index, VIS5D_VSTREAM, ws, VIS5D_GET)) {
     LUI_ButtonSetState( gtx->vstream_button[ws], 1 );
   }
   else {
     LUI_ButtonSetState( gtx->vstream_button[ws], 0 );
   }
}

/* WLH 11 Nov 98 */
void recompute_graphics( int index, int numtrajs, float *lats, float *lons,
                         float *alts, int *days, int *times, int *gps)
{
  recompute_graphics_var(index, numtrajs, lats, lons, alts, days, times, gps, -1);
}

/*
 * This is called after we execute a Tcl script, do a RESTORE, or compute
 * a new type-in expression.  It recomputes any graphics whose parameters
 * may have been changed by the script.
 */
/* WLH 11 Nov 98
void recompute_graphics( int index, int numtrajs, float *lats, float *lons,
                         float *alts, int *days, int *times, int *gps)
*/
/* WLH 11 Nov 98 */
void recompute_graphics_var( int index, int numtrajs, float *lats, float *lons,
                             float *alts, int *days, int *times, int *gps, int cvar)
{

   GuiContext gtx = get_gui_gtx2(index);
   int dhowmany, dwhichones[VIS5D_MAX_CONTEXTS];
   int chowmany, cwhichones[VIS5D_MAX_CONTEXTS];
   int dnumtimes, cnumtimes, dyo, cyo, yo;
   int curtime, dcurtime, ccurtime, var, ws, time, numvars;


   vis5d_get_num_of_itxs_in_display( index, &chowmany, cwhichones);
   for ( cyo = 0; cyo < chowmany; cyo++){
      vis5d_get_itx_numtimes( cwhichones[cyo], &cnumtimes );
      vis5d_get_itx_timestep(cwhichones[cyo], &curtime);   
      if (vis5d_enable_irregular_graphics(cwhichones[cyo], VIS5D_ISOSURF, VIS5D_GET)){
         for (time=0;time<cnumtimes;time++){
            vis5d_make_text_plot(cwhichones[cyo], time, time==curtime);
         }
      }
   }

   vis5d_get_num_of_ctxs_in_display( index, &chowmany, cwhichones);
   for ( cyo = 0; cyo < chowmany; cyo++){
      vis5d_get_ctx_numvars( cwhichones[cyo], &numvars );
      vis5d_get_dtx_numtimes( index, &dnumtimes );
      vis5d_get_ctx_numtimes( cwhichones[cyo], &cnumtimes );

/* WLH 26 Oct 98
      vis5d_initialize_stuff(cwhichones[cyo]);
*/

      /* WLH 21 Oct 98 - added 'visible' to comment */
      /* update visible isosurfaces and slices */
      for (var=0;var<numvars;var++) {

         /* WLH 11 Nov 98 */
         if (cvar >= 0 && var != cvar) continue;

         /* WLH 21 Oct 98 */
         if (vis5d_enable_graphics(cwhichones[cyo], VIS5D_ISOSURF, var, VIS5D_GET)) {
           for (time=0;time<cnumtimes;time++) {
              vis5d_make_isosurface(cwhichones[cyo], time, var, 0);
           }
         }

         if (vis5d_enable_graphics(cwhichones[cyo], VIS5D_HSLICE, var, VIS5D_GET)) {
            for (time=0;time<cnumtimes;time++) {
               vis5d_make_hslice(cwhichones[cyo], time, var, 0);
            }
         }
         if (vis5d_enable_graphics(cwhichones[cyo], VIS5D_VSLICE, var, VIS5D_GET)) {
            for (time=0;time<cnumtimes;time++) {
               vis5d_make_vslice(cwhichones[cyo], time, var, 0);
            }
         }
         if (vis5d_enable_graphics(cwhichones[cyo], VIS5D_CHSLICE, var, VIS5D_GET)) {
            for (time=0;time<cnumtimes;time++) {
               vis5d_make_chslice(cwhichones[cyo], time, var, 0);
            }
         }
         if (vis5d_enable_graphics(cwhichones[cyo], VIS5D_CVSLICE, var, VIS5D_GET)) {
            for (time=0;time<cnumtimes;time++) {
               vis5d_make_cvslice(cwhichones[cyo], time, var, 0);
            }
         }
      }

      /* WLH 21 Oct 98 */
      /* update hidden isosurfaces and slices */
      for (var=0;var<numvars;var++) {

         /* WLH 11 Nov 98 */
         if (cvar >= 0 && var != cvar) continue;

         if (!vis5d_enable_graphics(cwhichones[cyo], VIS5D_ISOSURF, var, VIS5D_GET)) {
            for (time=0;time<cnumtimes;time++) {
/* WLH 12 Nov 98
              vis5d_make_isosurface(cwhichones[cyo], time, var, 0);
*/
              /* WLH 12 Nov 98 */
              vis5d_invalidate_isosurface(cwhichones[cyo], var, time);

           }
         }
         if (!vis5d_enable_graphics(cwhichones[cyo], VIS5D_HSLICE, var, VIS5D_GET)) {
            for (time=0;time<cnumtimes;time++) {
/* WLH 12 Nov 98
               vis5d_make_hslice(cwhichones[cyo], time, var, 0);
*/
              /* WLH 12 Nov 98 */
              vis5d_invalidate_hslice(cwhichones[cyo], var, time);

            }
         }
         if (!vis5d_enable_graphics(cwhichones[cyo], VIS5D_VSLICE, var, VIS5D_GET)) {
            for (time=0;time<cnumtimes;time++) {
/* WLH 12 Nov 98
               vis5d_make_vslice(cwhichones[cyo], time, var, 0);
*/
              /* WLH 12 Nov 98 */
              vis5d_invalidate_vslice(cwhichones[cyo], var, time);

            }
         }
         if (!vis5d_enable_graphics(cwhichones[cyo], VIS5D_CHSLICE, var, VIS5D_GET)) { 
            for (time=0;time<cnumtimes;time++) {
/* WLH 12 Nov 98
               vis5d_make_chslice(cwhichones[cyo], time, var, 0);
*/
              /* WLH 12 Nov 98 */
              vis5d_invalidate_chslice(cwhichones[cyo], var, time);

            }
         }
         if (!vis5d_enable_graphics(cwhichones[cyo], VIS5D_CVSLICE, var, VIS5D_GET)) {
            for (time=0;time<cnumtimes;time++) {
/* WLH 12 Nov 98
               vis5d_make_cvslice(cwhichones[cyo], time, var, 0);
*/
              /* WLH 12 Nov 98 */
              vis5d_invalidate_cvslice(cwhichones[cyo], var, time);

            }
         }
      }

      /* update wind and stream slices */
      for (ws=0;ws<VIS5D_WIND_SLICES;ws++) {
         if (vis5d_enable_graphics(cwhichones[cyo], VIS5D_HWIND, ws, VIS5D_GET)) {
            for (time=0;time<dnumtimes;time++) {
               vis5d_make_hwindslice(index, time, ws, 0);
            }
         }
         if (vis5d_enable_graphics(cwhichones[cyo], VIS5D_VWIND, ws, VIS5D_GET)) {
            for (time=0;time<dnumtimes;time++) {
               vis5d_make_vwindslice(index, time, ws, 0);
            }
         }
         if (vis5d_enable_graphics(cwhichones[cyo], VIS5D_HSTREAM, ws, VIS5D_GET)) {
            for (time=0;time<dnumtimes;time++) {
               vis5d_make_hstreamslice(index, time, ws, 0);
            }
         }
         if (vis5d_enable_graphics(cwhichones[cyo], VIS5D_VSTREAM, ws, VIS5D_GET)) {
            for (time=0;time<dnumtimes;time++) {
               vis5d_make_hstreamslice(index, time, ws, 0);
            }
         }
      }
      {
/* WLH 21 Oct 98
         int numtrajs, ts, gp, rb;
         float row, col, lev, st, lg;
         numtrajs = vis5d_get_num_traj( index );
         for (yo=0; yo<numtrajs; yo++){
            vis5d_get_traj_info( index, yo, &row, &col, &lev, &ts, &st,
                                 &lg, &gp, &rb);
            vis5d_make_traj( index, row, col, lev, ts, gp);
         }
*/
      }         
   /* WLH 21 Oct 98 */
   } /* for ( cyo = 0; cyo < chowmany; cyo++) */

   /* WLH 21 Oct 98 - this is once per display context,
      not once per data context;
      it needs to delete the old trajectories;
      and it needs to re-create the old trajectories
      according to day, time and earth location rather
      than time step number and grid location */
/* WLH 6 Nov 98
   for (yo = 0; yo < VIS5D_TRAJ_SETS; yo++) {
     vis5d_delete_traj_set(index, yo);
   }
*/

   if (numtrajs > 0) {
     int ts, gp, rb, numtimes, i;
     float row, col, lev, st, lg;
     int *dtxdays, *dtxtimes;

     /* WLH 6 Nov 98 */
     for (yo = 0; yo < VIS5D_TRAJ_SETS; yo++) {
       vis5d_delete_traj_set(index, yo);
     }

     vis5d_get_dtx_numtimes(index, &numtimes);
     dtxdays = malloc(numtimes * sizeof(int));
     dtxtimes = malloc(numtimes * sizeof(int));
     for (i=0; i<numtimes; i++) {
       vis5d_get_dtx_time_stamp(index, i, &dtxdays[i], &dtxtimes[i]);
     }

     for (yo=0; yo<numtrajs; yo++) {
       if (days[yo] < dtxdays[0] ||
           (days[yo] == dtxdays[0] && times[yo] < dtxtimes[0])) {
         continue;
       }
       ts = -1;
       for (i=1; i<numtimes; i++) {
         if (days[yo] < dtxdays[i]) {
           ts = i-1;
           break;
         }
         if (days[yo] == dtxdays[i]) {
           if (times[yo] < dtxtimes[i]) {
             ts = i-1;
             break;
           }
           if (times[yo] == dtxtimes[i]) {
             ts = i;
             break;
           }
         }
       }
       if (ts > 0) {
         vis5d_geo_to_gridPRIME(index, ts, 0, lats[yo], lons[yo],
                                alts[yo], &row, &col, &lev);
         vis5d_make_traj( index, row, col, lev, ts, gps[yo]);
       }
     }

     free(dtxdays);
     free(dtxtimes);

     free(lats);
     free(lons);
     free(alts);
     free(days);
     free(times);
     free(gps);
   }

}

/* WLH 21 Oct 98 */
int save_traj(int index, float **latsp, float **lonsp, float **altsp,
              int **daysp, int **timesp, int **gpsp) {
  int numtrajs, ts, gp, rb, yo;
  float row, col, lev, st, lg;
  float *lats, *lons, *alts;
  int *times, *days, *gps;
  numtrajs = vis5d_get_num_traj( index );
  if (numtrajs < 1) return numtrajs;
  lats = malloc(numtrajs * sizeof(float));
  lons = malloc(numtrajs * sizeof(float));
  alts = malloc(numtrajs * sizeof(float));
  times = malloc(numtrajs * sizeof(int));
  days = malloc(numtrajs * sizeof(int));
  gps = malloc(numtrajs * sizeof(int));
  for (yo=0; yo<numtrajs; yo++) {
    vis5d_get_traj_info( index, yo, &row, &col, &lev,
                         &ts, &st, &lg, &gps[yo], &rb);
    vis5d_get_dtx_time_stamp(index, ts, &days[yo], &times[yo]);
    vis5d_gridPRIME_to_geo(index, ts, 0, row, col, lev,
                           &lats[yo], &lons[yo], &alts[yo]);
  }
  *latsp = lats;
  *lonsp = lons;
  *altsp = alts;
  *daysp = days;
  *timesp = times;
  *gpsp = gps;
  return numtrajs;
}


/* This is called by LUI when it wants to pass us an event */
static int controlpanel_cb( Window window, XEvent *event )
{
  int index, y;
  GuiContext gtx;
  XWindowAttributes winatts;
  XWindowAttributes winatts2;

  switch (event->type) {
  case ConfigureNotify:
    for (index=0; index<VIS5D_MAX_CONTEXTS; index++) {
      if ((gtx = gtx_table[index]) != NULL &&
          event->xconfigure.window==gtx->CpWindow){
        int width, height;
        if (gtx->how_many_irregular_contexts+gtx->how_many_regular_contexts<=0){
           return 0;
        }
        gtx->CpHeight = event->xconfigure.height;
        width = gtx->ButtonMatrixWidth;
        height = gtx->CpHeight - gtx->ButtonMatrixTop;
        if (gtx->how_many_irregular_contexts >= 1){
           XGetWindowAttributes( GuiDpy, gtx->IrregularButtonMatrix->mainwindow,
                                 &winatts);

           XGetWindowAttributes( GuiDpy,
                                 gtx->IrregularButtonMatrix->scrollbar->window,
                                 &winatts2);
           height -= (gtx->IrregularButtonMatrixHeight + 34);
           LUI_NewLabelDestroy(gtx->Irregular_Heading);
           gtx->Irregular_Heading = LUI_NewLabelCreate(
               gtx->CpWindow, 0, gtx->ButtonMatrixTop +height+4, 
               380, 28,
               "      Text          Symbol        Vertical  \n"
               "      Plot           Plot           Plot    ");

           XMoveResizeWindow(GuiDpy, gtx->IrregularButtonMatrix->mainwindow,
                             winatts.x, gtx->Irregular_Heading->y + 28,
                             winatts.width, winatts.height);           
           XMoveResizeWindow(GuiDpy,
                             gtx->IrregularButtonMatrix->scrollbar->window,
                             winatts2.x, gtx->Irregular_Heading->y + 28,
                             winatts.width, winatts.height);

           LUI_ButtonMatrixRedraw(gtx->IrregularButtonMatrix);
        } 
        if (height<50) {
          height = 50;
        }
        if (gtx->how_many_regular_contexts > 0){
           LUI_ButtonMatrixResize( gtx->ButtonMatrix, width, height );
        }
        break;
      }
    }
    break;
  }
  return 0;
}




/****************************************************/
/*** CALL-BACK FUNCTIONS FOR MAIN CONTROL BUTTONS ***/
/****************************************************/


/*** anim_cb **********************************************************
   Animate button call-back function.  Toggle animation on/off.
   Input:  pb - LUI button pointer
**********************************************************************/
static int anim_cb( LUI_NEWBUTTON *pb )
{
   int index = pb->context_index;
   GuiContext gtx = get_gui_gtx(index);

   if (gtx->GoTime){
      gtx->GoTime = 0;
      group_event(gtx->group_index, 1, 0);
   }
   else {
      if (pb->mousebutton == Button3){
         /* reverse animation */
          gtx->GoTime = -1;
          if (gtx->group_index >0){
             group_event(gtx->group_index, 1, 2);
          }
      }
      else{
         /* forward animation */
          gtx->GoTime = 1;
          if (gtx->group_index>0){
             group_event(gtx->group_index, 1, 1);
          }
      }
   }

   vis5d_signal_redraw(gtx->context_index, 1);
   return 0;
}



/*
 * ANIM-REC callback.
 */
static int animrec_cb( LUI_NEWBUTTON *pb )
{
   int index = pb->context_index;
   GuiContext gtx = get_gui_gtx(index);

   if (gtx->GoTime) {
      gtx->GoTime = 0;
      vis5d_graphics_mode( index, VIS5D_ANIMRECORD, VIS5D_OFF );
   }
   else {
      if (pb->mousebutton == Button3) 
         /* reverse animation */
          gtx->GoTime = -1;
      else
         /* forward animation */
          gtx->GoTime = 1;
      vis5d_graphics_mode(index, VIS5D_ANIMRECORD, VIS5D_ON );
   }

   vis5d_signal_redraw(index, 1);
   return 0;
}




/*** step_cb **********************************************************
   Step button call-back function.  Step forward or backward one time
   step or set time to first time step depending on which mouse
   button was pressed.
   Input:  pb - LUI button pointer
**********************************************************************/
static int step_cb( LUI_NEWBUTTON *pb )
{
   int NumTimes;
   int CurTime;
   int index = pb->context_index;
   GuiContext gtx = get_gui_gtx(index);
   int verylarge = 0;
   
   index = gtx->context_index;
   vis5d_get_dtx_numtimes( index, &NumTimes );
   vis5d_get_dtx_timestep( index, &CurTime );
   if (gtx->how_many_regular_contexts > 0){
      verylarge = vis5d_verylarge_mode(gtx->array_of_ctxs[0], VIS5D_GET);
   }

   /* CurTime must be in [0..NumTimes-1] */

   if (gtx->GoTime == 0) {
      if (pb->mousebutton == Button1) {
         group_event(gtx->group_index, 2, 0);
         if (CurTime==NumTimes-1 && gtx->group_index<1){
            CurTime = 0;
         }
         else if (gtx->group_index<1){
            CurTime++;
         }
      }
      else if(pb->mousebutton == Button2) {
         group_event(gtx->group_index, 2, 1);
         if (gtx->group_index<1){
            CurTime = 0;
         }
      }
      else if(pb->mousebutton == Button3) {
         group_event(gtx->group_index, 2, 2);
         if (CurTime==0 && gtx->group_index<1){
            CurTime = NumTimes-1;
         }
         else if(gtx->group_index<1){
            CurTime--;
         }
      }

      if (verylarge) {
         /* make graphics */
         vis5d_make_timestep_graphics(index, CurTime);
         vis5d_finish_work();
      }


      if (gtx->group_index<1){
         vis5d_set_dtx_timestep(index, CurTime);
      }
   }
   return 0;
}



/* callback for quit button */
static int exit_cb( LUI_NEWBUTTON *pb )
{
   int index = pb->context_index;
   if (verify(index, "Do you really want to exit?")) {
      /* send quit message to server */
      /*request_quit( NULL );*/

      /* Destroy all GUI's */
      for (index=0;index<VIS5D_MAX_CONTEXTS;index++) {
         if (gtx_table[index]) {
            /*gui_cleanup( gtx_table[index] );*/
         }
      }

      vis5d_terminate(1);
      exit(0);
   }
   return 0;
}



static int box_cb( LUI_NEWBUTTON *pb )
{
   int index = pb->context_index;
   GuiContext gtx = get_gui_gtx(index);
   /* toggle box rendering */
   if (pb->mousebutton==Button1) {
      vis5d_graphics_mode(index, VIS5D_BOX, VIS5D_TOGGLE);
      group_event(gtx->group_index, 8,
                  vis5d_graphics_mode(pb->context_index, VIS5D_BOX, VIS5D_GET));
      vis5d_invalidate_dtx_frames(index);
   }
   return 0;
}


static int clock_cb( LUI_NEWBUTTON *pb )
{
   int index = pb->context_index;
   GuiContext gtx = get_gui_gtx(index);
   /* toggle clock rendering */
   if (pb->mousebutton==Button1) {
      vis5d_graphics_mode(index, VIS5D_CLOCK, VIS5D_TOGGLE);
      group_event(gtx->group_index, 9,
                  vis5d_graphics_mode(pb->context_index, VIS5D_CLOCK, VIS5D_GET));
      vis5d_invalidate_dtx_frames(index);
   }
   return 0;
}



/*
 * Called when user clicks on SAVE button.
 */
static int save_cb( LUI_NEWBUTTON *pb )
{
   static char savefile[1000] = "";
   int index = pb->context_index;
   GuiContext gtx = get_gui_gtx(index);

   if (pb->mousebutton==Button1 ||
       pb->mousebutton==Button2){
      if (savefile[0]==0) {
         /* set initial save file name */
         sprintf( savefile, "%s.SAVE", v5dfile );
         LUI_FieldSetText( gtx->SaveNameField, savefile );
      }
      XMapWindow( GuiDpy, gtx->SaveWindow );
   }
   else{
      sprintf( savefile, "file.v5d");
      LUI_FieldSetText( gtx->SaveFileNameField, savefile);
      XMapWindow( GuiDpy, gtx->SaveFileWindow );
   }
   return 1;
}


/*
 * Called when user clicks on SAVE's OK button.
 */
static int save_ok_cb( LUI_NEWBUTTON *pb )
{
   char savefile[1000], str[1000];
   int result;
   int index = pb->context_index;
   GuiContext gtx = get_gui_gtx(index);

   LUI_FieldGetText( gtx->SaveNameField, savefile );

   result = tcl_save( index, savefile );

   XUnmapWindow( GuiDpy, gtx->SaveWindow );

   if (result==0) {
      /* printf("Saved: %s\n", savefile ); */
      vis5d_signal_redraw(index, 1);
   }
   else if (result==1) {
      sprintf( str, "Error: Unable to open\n%s for writing", savefile );
      alert( index, str );
   }
   else if (result==2) {
      sprintf( str, "Error while writing %s\nCheck if disk is full.",
              savefile );
      alert( index, str );
   }
   return 0;
}


static int savefile_ok_cb( LUI_NEWBUTTON *pb )
{
   int yo, spandex, howmany, whichones[VIS5D_MAX_CONTEXTS];
   char savefile[1000], str[1000];
   int result;
   int index = pb->context_index;
   GuiContext gtx = get_gui_gtx(index);

   vis5d_get_num_of_ctxs_in_display( index, &howmany, whichones);
   LUI_FieldGetText( gtx->SaveFileNameField, savefile );

   result = vis5d_save_to_v5dfile( whichones[0], savefile );

   XUnmapWindow( GuiDpy, gtx->SaveFileWindow );

   if (result==0) {
      /* printf("Saved: %s\n", savefile ); */
      vis5d_signal_redraw(index, 1);
   }
   else{
      printf("Error in writing v5d file\n");
   }
   return 0;
}

/*
 * Called when user clicks on SAVE's Cancel button.
 */
static int save_cancel_cb( LUI_NEWBUTTON *pb )
{
   int index = pb->context_index;
   GuiContext gtx = get_gui_gtx(index);
   XUnmapWindow( GuiDpy, gtx->SaveWindow );
   return 0;
}  

static int savefile_cancel_cb( LUI_NEWBUTTON *pb )
{
   int index = pb->context_index;
   GuiContext gtx = get_gui_gtx(index);
   XUnmapWindow( GuiDpy, gtx->SaveFileWindow );
   return 0;
}


/*
 * Called when user clicks on RESTORE button.
 */
static int restore_cb( LUI_NEWBUTTON *pb )
{
   static char savefile[1000] = "";
   int index = pb->context_index;
   GuiContext gtx = get_gui_gtx(index);

   if (savefile[0]==0) {
      /* set initial save file name */
      sprintf( savefile, "%s.SAVE", v5dfile );
      LUI_FieldSetText( gtx->RestoreNameField, savefile );
   }
   XMapWindow( GuiDpy, gtx->RestoreWindow );
   return 1;
}



/*
 * Called when user clicks on RESTORE button.
 */
static int restore_ok_cb( LUI_NEWBUTTON *pb )
{
   char savefile[100];
   int res;
   int index = pb->context_index;
   GuiContext gtx = get_gui_gtx(index);
   int var, oldnumvars, numvars;

   /* WLH 21 Oct 98 */
   int numtrajs, *days = NULL, *times = NULL, *gps = NULL;
   float *lats = NULL, *lons = NULL, *alts = NULL;

   LUI_FieldGetText( gtx->RestoreNameField, savefile );
   XUnmapWindow( GuiDpy, gtx->RestoreWindow );

/* WLH 21 Oct 98
   vis5d_get_ctx_numvars( index, &oldnumvars );
*/

   /* WLH 21 Oct 98 */
/* WLH 6 Nov 98
   numtrajs = save_traj(index, &lats, &lons, &alts,
                        &days, &times, &gps);
*/

   /* Determine if restoring a binary save file or Tcl script. */
   {
      FILE *f;
      f = fopen(savefile,"r");
      if (!f) {
         res = VIS5D_BAD_VALUE;
      }
      else {
         char str[10];
         fgets( str, 3, f );
         if (str[0]=='#') {
            /* restore tcl file */
            if (execute_script( index, savefile ) !=0) {
               /* this just automatically create a new
                  gui when a script is run */
               res = 0;
            }
            else {
               res = VIS5D_FAIL;
            }
         }
         else {
            /* restore old-style binary .SAVE file */
            res = vis5d_restore( index, savefile );
         }
      }
   }

   if (res==0) {
      /* may have to add more button rows to control panel now */
      make_another_gui( index, 1 );
      hide_widgets( index );
      show_widgets( index );
      /*      turn_off_everything( index );*/
/* WLH 21 Oct 98
      recompute_graphics(index);
*/
      /* WLH 21 Oct 98 */
/* WLH 6 Nov 98
      recompute_graphics(index, numtrajs, lats, lons, alts, days, times, gps);
*/
      /* WLH 6 Nov 98 */
      recompute_graphics( index, 0, NULL, NULL, NULL, NULL, NULL, NULL);

      vis5d_signal_redraw(index, 1);
   }
   else if (res==VIS5D_BAD_VALUE) {
      char str[1000];

      /* WLH 21 Oct 98 */
      if (lats) free(lats);
      if (lons) free(lons);
      if (alts) free(alts);
      if (days) free(days);
      if (times) free(times);
      if (gps) free(gps);

      sprintf( str, "Error: unable to open\n%s for reading", savefile );
      alert( index, str );
   }
   else if (res==VIS5D_FAIL) {
      char str[1000];

      /* WLH 21 Oct 98 */
      if (lats) free(lats);
      if (lons) free(lons);
      if (alts) free(alts);
      if (days) free(days);
      if (times) free(times);
      if (gps) free(gps);

      sprintf( str,
              "Error while reading %s\nFile may be corrupt or an old version",
              savefile );
      alert( index, str );
   }

   vis5d_invalidate_dtx_frames(index);
   return 0;
}



/*
 * Called when user clicks on RESTORE's Cancel button.
 */
static int restore_cancel_cb( LUI_NEWBUTTON *pb )
{
   int index = pb->context_index;
   GuiContext gtx = get_gui_gtx(index);
   XUnmapWindow( GuiDpy, gtx->RestoreWindow );
   return 0;
}  



/* callback for topography button */
static int topo_cb( LUI_NEWBUTTON *pb )
{
   int index = pb->context_index;
   GuiContext gtx = get_gui_gtx(index);

   if (pb->mousebutton==Button1) {
      /* toggle display of topography */
      vis5d_graphics_mode(pb->context_index, VIS5D_TOPO, VIS5D_TOGGLE);
      group_event(gtx->group_index, 6,
                  vis5d_graphics_mode(pb->context_index, VIS5D_TOPO, VIS5D_GET));

      /* MJK 12.04.98 */
      check_map_color (index);
   }
   else if (pb->mousebutton==Button2 ||
            pb->mousebutton==Button3) {
      /* topo color widget */
      /* BUG FIX MJK 8.10.98 */
      /* got rid of some code here */
      XSync( GuiDpy, 0 );
      show_isocolor_window( gtx, VIS5D_TOPO, 
                            gtx->array_of_ctxs[0], -1, 0 );
   }
   vis5d_invalidate_dtx_frames(index);
   return 0;
}



/* MJK 12.04.98 begin */
/*** hslice_pos_cb ****************************************************
   This is the call-back function for the hslice_pos widget.
**********************************************************************/
static hslice_pos_cb (LUI_NEWSLIDER *s, float hgt)
{
    float       x, y, z, row, col, lev, vertargs[MAXVERTARGS];
    float       interval, low, high, cur_lev;
    int         vertsys, time, curtime, numtimes;
    int         index = s->context_index;
    GuiContext  gtx = get_gui_gtx(index);
    int vindex, dindex;

    vindex = gtx->cur_hslicevindex;
    dindex = index; 
    vis5d_get_dtx_vertical (dindex, &vertsys, vertargs);
    if (vertsys == VERT_NONEQUAL_MB) hgt = pressure_to_height (hgt);

    vis5d_geo_to_xyzPRIME  (dindex, 0, 0, 0.0, 0.0, hgt, &x, &y, &z);
    vis5d_xyzPRIME_to_gridPRIME (dindex, 0, 0, 0.0, 0.0, z, &row, &col, &lev);


    vis5d_get_hslice (vindex, gtx->cur_hslice, &interval, &low, &high, &cur_lev);
    if (vis5d_enable_sfc_graphics (vindex, VIS5D_HSLICE, gtx->cur_hslice,
                                   VIS5D_GET))
    {
        mod_vpos_slider (dindex, gtx->hslice_pos_slider, gtx->cur_hslice,
                         0, cur_lev);
        return 0;
    }
    vis5d_set_hslice (vindex, gtx->cur_hslice, interval, low, high, lev);

    vis5d_get_ctx_timestep (vindex, &curtime);
    vis5d_get_ctx_numtimes (vindex, &numtimes);

    for (time = 0; time < numtimes; time++)
    {
       vis5d_make_hslice (vindex, time, gtx->cur_hslice, time==curtime);
    }
    move_linked_hslices( vindex, dindex, VIS5D_HSLICE, gtx->cur_hslice, lev);
    /* MJK 12.07.98 */
    vis5d_signal_redraw (gtx->context_index, 1);

    return 0;
}



/*** hwind_pos_cb ****************************************************
   This is the call-back function for the hwind_pos widget.
**********************************************************************/
static hwind_pos_cb (LUI_NEWSLIDER *s, float hgt)
{
    float       x, y, z, row, col, lev, vertargs[MAXVERTARGS];
    float       density, scale, cur_lev;
    int         vertsys, time, curtime, numtimes;
    int         index = s->context_index;
    GuiContext  gtx = get_gui_gtx(index);
    int vindex = gtx->array_of_ctxs[0];

    vis5d_get_dtx_timestep (index, &curtime);
    vis5d_get_dtx_numtimes (index, &numtimes);

    vis5d_get_dtx_vertical (index, &vertsys, vertargs);
    if (vertsys == VERT_NONEQUAL_MB) hgt = pressure_to_height (hgt);

    vis5d_geo_to_xyzPRIME  (index, 0, 0, 0.0, 0.0, hgt, &x, &y, &z);
    vis5d_xyzPRIME_to_gridPRIME (index, 0, 0, 0.0, 0.0, z, &row, &col, &lev);

    if (gtx->cur_hwind >= 0)
    {
       vis5d_get_hwindslice (index, gtx->cur_hwind, &density, &scale,
                              &cur_lev);
       if (vis5d_enable_sfc_graphics (vindex, VIS5D_HWIND, gtx->cur_hwind,
                                      VIS5D_GET))
       {
           mod_vpos_slider (index, gtx->hwind_pos_slider, gtx->cur_hwind,
                            0, cur_lev);
           return 0;
       }
       vis5d_set_hwindslice (index, gtx->cur_hwind, density, scale, lev);
       for (time = 0; time < numtimes; time++)
       {
          vis5d_make_hwindslice (index, time, gtx->cur_hwind, time==curtime);
       }
       move_linked_hslices( vindex, index, VIS5D_HWIND, gtx->cur_hwind, lev);
    }

    else
    {
       vis5d_get_hstreamslice (index, gtx->cur_hstream, &density, &cur_lev);
       if (vis5d_enable_sfc_graphics (vindex, VIS5D_HSTREAM, gtx->cur_hstream,
                                      VIS5D_GET))
       {
           mod_vpos_slider (index, gtx->hwind_pos_slider, gtx->cur_hstream,
                            0, cur_lev);
           return 0;
       }
       vis5d_set_hstreamslice (index, gtx->cur_hstream, density, lev);
       for (time = 0; time < numtimes; time++)
       {
          vis5d_make_hstreamslice (index, time, gtx->cur_hstream,
                                   time==curtime);
       }
       move_linked_hslices( vindex, index, VIS5D_HSTREAM, gtx->cur_hstream, lev);
    }
    vis5d_signal_redraw (gtx->context_index, 1);

    return 0;
}



/*** chslice_pos_cb ****************************************************
   This is the call-back function for the chslice_pos widget.
**********************************************************************/
static chslice_pos_cb (LUI_NEWSLIDER *s, float hgt)
{
    float       x, y, z, row, col, lev, vertargs[MAXVERTARGS];
    float       cur_lev;
    int         vertsys, time, curtime, numtimes;
    int         index = s->context_index;
    GuiContext  gtx = get_gui_gtx(index);
    int vindex, dindex;

    dindex = index;
    vindex = cb_chvindex[dindex]; 
    vis5d_get_dtx_vertical (dindex, &vertsys, vertargs);
    if (vertsys == VERT_NONEQUAL_MB) hgt = pressure_to_height (hgt);

    vis5d_geo_to_xyzPRIME  (dindex, 0, 0, 0.0, 0.0, hgt, &x, &y, &z);
    vis5d_xyzPRIME_to_gridPRIME (dindex, 0, 0, 0.0, 0.0, z, &row, &col, &lev);

    vis5d_get_chslice (vindex, cb_chvar[cb_dindex], &cur_lev);
    vis5d_set_chslice (vindex, cb_chvar[cb_dindex], lev);


    vis5d_get_ctx_timestep (vindex, &curtime);
    vis5d_get_ctx_numtimes (vindex, &numtimes);
    for (time = 0; time < numtimes; time++)
    {
       vis5d_make_chslice (vindex, time, cb_chvar[cb_dindex], time==curtime);
    }
    move_linked_hslices( vindex, dindex, VIS5D_CHSLICE, cb_chvar[cb_dindex], lev);

    /* MJK 12.07.98 */
    vis5d_signal_redraw( gtx->context_index, 1);

    return 0;
}



/*** map_pos_cb ****************************************************
   This is the call-back function for the map_pos widget.
**********************************************************************/
static map_pos_cb (LUI_NEWSLIDER *s, float hgt)
{
    float       x, y, z, row, col, lev, vertargs[MAXVERTARGS], cur_lev;
    int         vertsys, state;
    int         index = s->context_index;
    GuiContext  gtx = get_gui_gtx(index);
    int vindex = gtx->array_of_ctxs[0];

    vis5d_get_dtx_vertical (index, &vertsys, vertargs);
    if (vertsys == VERT_NONEQUAL_MB) hgt = pressure_to_height (hgt);

    vis5d_geo_to_xyzPRIME  (index, 0, 0, 0.0, 0.0, hgt, &x, &y, &z);
    vis5d_xyzPRIME_to_gridPRIME (index, 0, 0, 0.0, 0.0, z, &row, &col, &lev);

    vis5d_get_flatmap_level (index, &cur_lev);

    if (vis5d_enable_sfc_map (index, VIS5D_GET))
    {
        mod_vpos_slider (index, gtx->map_pos_slider, 0, 0, cur_lev);

        return 0;
    }

    vis5d_set_flatmap_level (index, lev);

    if (vis5d_graphics_mode (index, VIS5D_MAP, VIS5D_GET))
    {
        vis5d_graphics_mode (index, VIS5D_MAP, VIS5D_ON);
    }
    vis5d_signal_redraw (index, 1);
    return 0;
}

static sfc_hslice_cb (LUI_NEWBUTTON *pb)
{
   int curtime, numtimes, i;
   int index = pb->context_index;
   GuiContext gtx = get_gui_gtx(index);
   GuiContext gtx2;
   int vindex = gtx->cur_hslicevindex;
   int dhowmany, dwhichones[VIS5D_MAX_CONTEXTS];
   int chowmany, cwhichones[VIS5D_MAX_CONTEXTS];
   int state, dyo, cyo, good;
   char aname[30];

   if (gtx->cur_hslice>-1) {
      state = vis5d_enable_sfc_graphics(vindex, VIS5D_HSLICE, gtx->cur_hslice,
                                        VIS5D_GET);
      if (state == 0){
         state = 1;
      }
      else{
         state = 0;
      }   
      if (gtx->group_index > 0){
         vis5d_get_ctx_var_name( vindex, gtx->cur_hslice, aname);
         vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
         for (dyo = 0; dyo < dhowmany; dyo++){
            vis5d_get_num_of_ctxs_in_display( dwhichones[dyo], &chowmany, cwhichones);
            gtx2 = get_gui_gtx(dwhichones[dyo]);
            for ( cyo = 0; cyo < chowmany; cyo++){
               good = vis5d_find_var(cwhichones[cyo], aname);
               if (good>-1){
                  vis5d_enable_sfc_graphics(cwhichones[cyo], VIS5D_HSLICE,
                                        good, state);
                  LUI_ButtonSetState (gtx2->hslice_sfc_button, state);  
                  vis5d_get_ctx_timestep(cwhichones[cyo], &curtime);
                  vis5d_get_ctx_numtimes (cwhichones[cyo], &numtimes);
                  for (i=0;i<numtimes;i++) {
                     vis5d_make_hslice(cwhichones[cyo], i, good, i==curtime);
                  }
                  cyo = chowmany;
               }
            }
         }
      }   
      else{
         vis5d_enable_sfc_graphics (vindex, VIS5D_HSLICE, gtx->cur_hslice,
                                    state);
         LUI_ButtonSetState (gtx->hslice_sfc_button, state);

         vis5d_get_ctx_timestep(vindex, &curtime);
         vis5d_get_ctx_numtimes (vindex, &numtimes);
         for (i=0;i<numtimes;i++) {
            vis5d_make_hslice (vindex, i, gtx->cur_hslice, i==curtime);
         }
      }
   }

   return 0;
}



static sfc_hwind_cb (LUI_NEWBUTTON *pb)
{
   int curtime, numtimes, i;
   int index = pb->context_index;
   GuiContext gtx = get_gui_gtx(index);
   GuiContext gtx2;
   int vindex = gtx->array_of_ctxs[0];
   int dhowmany, dwhichones[VIS5D_MAX_CONTEXTS];
   int chowmany, cwhichones[VIS5D_MAX_CONTEXTS];
   int state, dyo, cyo, good;

   
   if (gtx->cur_hwind>-1) {
      state = vis5d_enable_sfc_graphics (vindex, VIS5D_HWIND, gtx->cur_hwind,
                                 VIS5D_GET);
      if (state == 0){
         state = 1;
      }
      else{
         state = 0;
      }
      if (gtx->group_index > 0){
         vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
         for (dyo = 0; dyo < dhowmany; dyo++){
            gtx2 = get_gui_gtx(dwhichones[dyo]);
            vindex = gtx2->array_of_ctxs[0];
            vis5d_enable_sfc_graphics (vindex, VIS5D_HWIND, gtx->cur_hwind,
                                     state);
            LUI_ButtonSetState (gtx2->hwind_sfc_button, state);
            vis5d_get_dtx_timestep(dwhichones[dyo], &curtime);
            vis5d_get_dtx_numtimes (dwhichones[dyo], &numtimes);
            for (i=0;i<numtimes;i++) {
               vis5d_make_hwindslice(dwhichones[dyo], i, gtx->cur_hwind,
                                     i==curtime);
            }
         }
      }
      else{
         vis5d_enable_sfc_graphics (vindex, VIS5D_HWIND, gtx->cur_hwind,
                                     state);
         LUI_ButtonSetState (gtx->hwind_sfc_button, state);
         vis5d_get_dtx_timestep(index, &curtime);
         vis5d_get_dtx_numtimes (index, &numtimes);
         for (i=0;i<numtimes;i++) {
            vis5d_make_hwindslice (index, i, gtx->cur_hwind, i==curtime);
         }
      }
   }
   else if (gtx->cur_hstream>-1) {
      state = vis5d_enable_sfc_graphics (vindex, VIS5D_HWIND, gtx->cur_hstream,
                                 VIS5D_GET);
      if (state == 0){
         state = 1;
      }
      else{
         state = 0;
      }
      if (gtx->group_index > 0){
         vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
         for (dyo = 0; dyo < dhowmany; dyo++){
            gtx2 = get_gui_gtx(dwhichones[dyo]);
            vindex = gtx2->array_of_ctxs[0];
            vis5d_enable_sfc_graphics (vindex, VIS5D_HSTREAM, gtx->cur_hstream,
                                     state);
            LUI_ButtonSetState (gtx2->hwind_sfc_button, state);
            vis5d_get_dtx_timestep(dwhichones[dyo], &curtime);
            vis5d_get_dtx_numtimes (dwhichones[dyo], &numtimes);
            for (i=0;i<numtimes;i++) {
               vis5d_make_hstreamslice(dwhichones[dyo], i, gtx->cur_hstream,
                                     i==curtime);
            }
         }
      }
      else{
         vis5d_enable_sfc_graphics (vindex, VIS5D_HSTREAM, gtx->cur_hstream,
                                     state);
         LUI_ButtonSetState (gtx->hwind_sfc_button, state);
         vis5d_get_dtx_timestep(index, &curtime);
         vis5d_get_dtx_numtimes (index, &numtimes);
         for (i=0;i<numtimes;i++) {
            vis5d_make_hstreamslice (index, i, gtx->cur_hstream, i==curtime);
         }
      }
   }

   return 0;
}


/* map the map control window */
static void map_map_window (int index, int state)
{
   static int   map_win_open = 0;

   GuiContext gtx = get_gui_gtx(index);

   if (state < 0) state = !map_win_open;

   map_win_open = state;

   if (map_win_open)
   {
      float level;

      vis5d_get_flatmap_level (index, &level);
      mod_vpos_slider (index, gtx->map_pos_slider, 0, 0, level);
      XMapWindow (GuiDpy, gtx->MapWindow);
   }
   else
   {
      XUnmapWindow (GuiDpy, gtx->MapWindow);
   }
}

static int sfc_map_cb (LUI_NEWBUTTON *pb)
{
   int          index = pb->context_index;
   int          sfc_state, which_map, other_map, graphic, var;
   float        r, g, b, a;
   GuiContext   gtx = get_gui_gtx (index);
   GuiContext   gtx2;
   int vindex = gtx->array_of_ctxs[0];
   int dhowmany, dwhichones[VIS5D_MAX_CONTEXTS];
   int chowmany, cwhichones[VIS5D_MAX_CONTEXTS];
   int state, dyo, cyo, good;

   state = vis5d_enable_sfc_map (index, VIS5D_GET);
   if (state == 0){
      state = 1;
   }
   else{
      state = 0;
   }
   if (gtx->group_index > 0){
      vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
      for (dyo = 0; dyo < dhowmany; dyo++){
         gtx2 = get_gui_gtx(dwhichones[dyo]);
         vindex = gtx2->array_of_ctxs[0];
         sfc_state = state;
         which_map = (sfc_state) ? VIS5D_DARK_MAP  : VIS5D_LIGHT_MAP;
         other_map = (sfc_state) ? VIS5D_LIGHT_MAP : VIS5D_DARK_MAP;
         vis5d_enable_sfc_map (index, state);
         LUI_ButtonSetState (gtx2->map_sfc_button, sfc_state);
         vis5d_get_color (index, which_map, 0, &r, &g, &b, &a);
         LUI_ButtonColor (gtx2->map_button, r, g, b);
         check_map_color(dwhichones[dyo]);
      }
   }

   sfc_state = state;
   which_map = (sfc_state) ? VIS5D_DARK_MAP  : VIS5D_LIGHT_MAP;
   other_map = (sfc_state) ? VIS5D_LIGHT_MAP : VIS5D_DARK_MAP;

   vis5d_enable_sfc_map (index, state);
   LUI_ButtonSetState (gtx->map_sfc_button, sfc_state);
   vis5d_get_color (index, which_map, 0, &r, &g, &b, &a);
   LUI_ButtonColor (gtx->map_button, r, g, b);

   if (index == get_current_rgbsliders (gtx, &graphic, &var))
   {
      if (graphic == other_map)
         show_rgb_sliders (gtx, which_map, gtx->array_of_ctxs[0], 0, 0);
   }
   check_map_color (index);
   return 0;
}

static int map_cb (LUI_NEWBUTTON *pb)
{
   int index = pb->context_index;
   GuiContext gtx = get_gui_gtx(index);
   int vindex = gtx->array_of_ctxs[0];
   int dhowmany, dwhichones[VIS5D_MAX_CONTEXTS];
   int chowmany, cwhichones[VIS5D_MAX_CONTEXTS];
   int state, dyo, cyo, good;


   if (pb->mousebutton==Button1) {
      /* toggle display */
      if (pb->state == LUI_BUTTON_IN)
      {
         if (gtx->group_index > 0){
            vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
            for (dyo = 0; dyo < dhowmany; dyo++){
               vis5d_graphics_mode (dwhichones[dyo], VIS5D_MAP, VIS5D_ON);
               check_map_color(dwhichones[dyo]);
            }
         }
         vis5d_graphics_mode (index, VIS5D_MAP, VIS5D_ON);
         map_map_window (index, 1);
      }
      else
      {
         if (gtx->group_index > 0){
            vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
            for (dyo = 0; dyo < dhowmany; dyo++){
               vis5d_graphics_mode (dwhichones[dyo], VIS5D_MAP, VIS5D_OFF);
               check_map_color(dwhichones[dyo]);            
            }
         }
         vis5d_graphics_mode (index, VIS5D_MAP, VIS5D_OFF);
         map_map_window (index, 0);
      }

      check_map_color (index);
   }
   else if (pb->mousebutton==Button2) {
      map_map_window (index, -1);
   }
   else if (pb->mousebutton==Button3) {
      /* set map color */

      if (vis5d_enable_sfc_map (index, VIS5D_GET))
         show_rgb_sliders (gtx, VIS5D_DARK_MAP, gtx->array_of_ctxs[0], 0, 0);
      else
         show_rgb_sliders (gtx, VIS5D_LIGHT_MAP, gtx->array_of_ctxs[0], 0, 0);
   }

   return 0;
}
/* MJK 12.04.98 end */




/* toggle perspective/parallel projection */
static int proj_cb( LUI_NEWBUTTON *pb )
{
   int index = pb->context_index;
   GuiContext gtx = get_gui_gtx(index);
   vis5d_graphics_mode(pb->context_index, VIS5D_PERSPECTIVE, VIS5D_TOGGLE);
   group_event(gtx->group_index, 13,
                  vis5d_graphics_mode(pb->context_index, VIS5D_PERSPECTIVE, VIS5D_GET));
   return 0;
}



/* toggle contour label drawing */
static int contnum_cb( LUI_NEWBUTTON *pb )
{
   int index = pb->context_index;
   GuiContext gtx = get_gui_gtx(index);
   vis5d_graphics_mode(pb->context_index, VIS5D_CONTOUR_NUMBERS, VIS5D_TOGGLE);
   group_event(gtx->group_index, 11,
                  vis5d_graphics_mode(pb->context_index, VIS5D_CONTOUR_NUMBERS, VIS5D_GET));
   return 0;
}



/* toggle between geographic and grid coordinate display */
static int coord_cb( LUI_NEWBUTTON *pb )
{
   int index = pb->context_index;
   GuiContext gtx = get_gui_gtx(index);
   vis5d_graphics_mode(pb->context_index, VIS5D_GRID_COORDS, VIS5D_TOGGLE);
   group_event(gtx->group_index, 10,
                  vis5d_graphics_mode(pb->context_index, VIS5D_GRID_COORDS, VIS5D_GET));
   return 0;
}


/* Called when user clicks on HWind button */
static int hwind_cb( LUI_NEWBUTTON *b, int state )
{
   GuiContext gtx = get_gui_gtx( b->context_index );
   int index = b->context_index; /* index = display_ctx index */
   int slice = b->index;
   int dhowmany, dwhichones[VIS5D_MAX_CONTEXTS];
   int chowmany, cwhichones[VIS5D_MAX_CONTEXTS];
   int times, dyo, cyo, good;


   update_linked_buttons(gtx->array_of_ctxs[0], index, VIS5D_HWIND, slice, state);
   if (b->mousebutton==Button1) {
      /* toggle display on/off */
      if (state==1) {
         int time, numtimes, curtime;
         /* MJK 12.04.98 */
         gtx->cur_hstream = -1;
         gtx->cur_hwindmap = 1;
         gtx->cur_hstreammap = 0;
         gtx->cur_hwind = slice;
 

         map_hwind_window( index, gtx->cur_hwind );
         vis5d_enable_graphics(gtx->array_of_ctxs[0], VIS5D_HWIND, slice, VIS5D_ON);
         vis5d_get_dtx_timestep(index, &curtime);
         vis5d_get_dtx_numtimes(index, &numtimes);
         for (time=0;time<numtimes;time++) {
            vis5d_make_hwindslice(index, time, slice, time==curtime);
         }
      }
      else {
         XUnmapWindow(GuiDpy,gtx->HWindWindow);
         vis5d_enable_graphics(gtx->array_of_ctxs[0], VIS5D_HWIND, slice, VIS5D_OFF);
         gtx->cur_hwind = -1;
         gtx->cur_hwindmap = 0;
      }
   }
   else if (b->mousebutton==Button2) {
      /* show/hide pop-up control window */
      if (gtx->cur_hwind==slice) {
         /* MJK 12.04.98 */
         XUnmapWindow(GuiDpy,gtx->HWindWindow);
         gtx->cur_hwind = -1;
         gtx->cur_hwindmap = 0;
      }
      else {
         gtx->cur_hwind = slice;
         gtx->cur_hwindmap = 1;
         map_hwind_window( index, gtx->cur_hwind );
      }
      /* MJK 12.04.98 */
      gtx->cur_hstream = -1;
      gtx->cur_hstreammap = 0;
    }
   else if (b->mousebutton==Button3) {
      show_rgb_sliders( gtx, VIS5D_HWIND, gtx->array_of_ctxs[0], slice, 0 ); 
   }

   if (gtx->group_index > 0){
      vis5d_invalidate_grp_frames(gtx->group_index);
   }
   else{
      vis5d_invalidate_dtx_frames(index);
   }
   return 0;
}



/* Called when user clicks on VWind button */
static int vwind_cb( LUI_NEWBUTTON *b, int state )
{
   GuiContext gtx = get_gui_gtx( b->context_index );
   int index = b->context_index;
   int slice = b->index;
   int dhowmany, dwhichones[VIS5D_MAX_CONTEXTS];
   int chowmany, cwhichones[VIS5D_MAX_CONTEXTS];
   int times, dyo, cyo, good;

   update_linked_buttons(gtx->array_of_ctxs[0], index, VIS5D_VWIND, slice, state);
   if (b->mousebutton==Button1) {
      /* toggle display on/off */
      if (state==1) {
         int time, numtimes, curtime;
         /* MJK 12.04.98 */
         gtx->cur_vwind = slice;
         gtx->cur_vstream = -1;
         gtx->cur_vwindmap = 1;
         gtx->cur_vstreammap = 0;


         map_vwind_window( index, gtx->cur_vwind );
         vis5d_enable_graphics(gtx->array_of_ctxs[0], VIS5D_VWIND, slice, VIS5D_ON);
         vis5d_get_dtx_timestep(index, &curtime);
         vis5d_get_dtx_numtimes(index, &numtimes);
         for (time=0;time<numtimes;time++) {
            vis5d_make_vwindslice(index, time, slice, time==curtime);
         }
      }
      else {
         XUnmapWindow(GuiDpy,gtx->WindWindow);
         vis5d_enable_graphics(gtx->array_of_ctxs[0], VIS5D_VWIND, slice, VIS5D_OFF);
         gtx->cur_vwind = -1;
         gtx->cur_vwindmap = 0;
      }
   }
   else if (b->mousebutton==Button2) {
      /* show/hide pop-up control window */
      if (gtx->cur_vwind==slice) {
         XUnmapWindow(GuiDpy,gtx->WindWindow);
         gtx->cur_vwind = -1;
         gtx->cur_vwindmap = 0;
      }
      else {
         gtx->cur_vwind = slice;
         map_vwind_window( index, gtx->cur_vwind );
         gtx->cur_vwindmap = 1;
      }
      /* MJK 12.04.98 */
      gtx->cur_vstream = -1;
      gtx->cur_vstreammap = 0;


   }
   else if (b->mousebutton==Button3) {
      show_rgb_sliders( gtx, VIS5D_VWIND, gtx->array_of_ctxs[0], slice, 0 ); 
   }

   if (gtx->group_index > 0){
      vis5d_invalidate_grp_frames(gtx->group_index);
   }
   else{
      vis5d_invalidate_dtx_frames(index);
   }

   return 0;
}



/* Called when user clicks on HStream button */
static int hstream_cb( LUI_NEWBUTTON *b, int state )
{
   GuiContext gtx = get_gui_gtx( b->context_index );
   int index = b->context_index;
   int slice = b->index;
   int dhowmany, dwhichones[VIS5D_MAX_CONTEXTS];
   int chowmany, cwhichones[VIS5D_MAX_CONTEXTS];
   int times, dyo, cyo, good;

   update_linked_buttons(gtx->array_of_ctxs[0], index, VIS5D_HSTREAM, slice, state);
   if (b->mousebutton==Button1) {
      /* toggle display on/off */
      if (state==1) {
         int time, numtimes, curtime;
         /* MJK 12.04.98 */                      
         gtx->cur_hstream = slice;
         gtx->cur_hwind = -1;
         gtx->cur_hstreammap = 1;
         gtx->cur_hwindmap = 0;


         map_hstream_window( index, gtx->cur_hstream );
         vis5d_enable_graphics(gtx->array_of_ctxs[0], VIS5D_HSTREAM, slice, VIS5D_ON);
         vis5d_get_dtx_timestep(index, &curtime);
         vis5d_get_dtx_numtimes(index, &numtimes);
         for (time=0;time<numtimes;time++) {
            vis5d_make_hstreamslice(index, time, slice, time==curtime);
         }
      }
      else {
         /* MJK 12.04.98 */
         XUnmapWindow(GuiDpy,gtx->HWindWindow);


         vis5d_enable_graphics(gtx->array_of_ctxs[0], VIS5D_HSTREAM, slice, VIS5D_OFF);
         gtx->cur_hstream = -1;
         gtx->cur_hstreammap = 0;
      }
   }
   else if (b->mousebutton==Button2) {
      /* show/hide pop-up control window */
      if (gtx->cur_hstream==slice) {
         /* MJK 12.04.98 */
         XUnmapWindow(GuiDpy,gtx->HWindWindow);


         gtx->cur_hstreammap = 0;
         gtx->cur_hstream = -1;
      }
      else {
         gtx->cur_hstream = slice;
         gtx->cur_hstreammap = 1;
         map_hstream_window( index, gtx->cur_hwind );
      }
      /* MJK 12.04.98 */
      gtx->cur_hwind = -1;
      gtx->cur_hwindmap = 0;


   }
   else if (b->mousebutton==Button3) {
      show_rgb_sliders( gtx, VIS5D_HSTREAM, gtx->array_of_ctxs[0], slice, 0 ); 
   }

   if (gtx->group_index > 0){
      vis5d_invalidate_grp_frames(gtx->group_index);
   }
   else{
      vis5d_invalidate_dtx_frames(index);
   }
   return 0;
}


/* Called when user clicks on VStream button */
static int vstream_cb( LUI_NEWBUTTON *b, int state )
{
   GuiContext gtx = get_gui_gtx( b->context_index );
   int index = b->context_index;
   int slice = b->index;
   int dhowmany, dwhichones[VIS5D_MAX_CONTEXTS];
   int chowmany, cwhichones[VIS5D_MAX_CONTEXTS];
   int times, dyo, cyo, good;

   update_linked_buttons(gtx->array_of_ctxs[0], index, VIS5D_VSTREAM, slice, state);
   if (b->mousebutton==Button1) {
      /* toggle display on/off */
      if (state==1) {
         int time, numtimes, curtime;
         /* MJK 12.04.98 */                      
         gtx->cur_vstream = slice;
         gtx->cur_vwind = -1;
         gtx->cur_vstreammap = 1;
         gtx->cur_vwindmap = 0;
         map_vstream_window( index, gtx->cur_vstream );
         vis5d_enable_graphics(gtx->array_of_ctxs[0], VIS5D_VSTREAM, slice, VIS5D_ON);
         vis5d_get_dtx_timestep(index, &curtime);
         vis5d_get_dtx_numtimes(index, &numtimes);
         for (time=0;time<numtimes;time++) {
            vis5d_make_vstreamslice(index, time, slice, time==curtime);
         }
      }
      else {
         XUnmapWindow(GuiDpy,gtx->WindWindow);
         vis5d_enable_graphics(gtx->array_of_ctxs[0], VIS5D_VSTREAM, slice, VIS5D_OFF);
         gtx->cur_vstream = -1;
         gtx->cur_vstreammap = 0;
      }
   }
   else if (b->mousebutton==Button2) {
      /* show/hide pop-up control window */
      if (gtx->cur_vstream==slice) {
         XUnmapWindow(GuiDpy,gtx->WindWindow);
         gtx->cur_vstream = -1;
         gtx->cur_vstreammap = 0;      
      }
      else {
         gtx->cur_vstream = slice;
         map_vstream_window( index, gtx->cur_hwind );
         gtx->cur_vstreammap = 1;      
      }
      /* MJK 12.04.98 */
      gtx->cur_vwind = -1;
      gtx->cur_vwindmap = 0;


   }
   else if (b->mousebutton==Button3) {
      show_rgb_sliders( gtx, VIS5D_VSTREAM, gtx->array_of_ctxs[0], slice, 0 ); 
   }

   if (gtx->group_index > 0){
      vis5d_invalidate_grp_frames(gtx->group_index);
   }
   else{
      vis5d_invalidate_dtx_frames(index);
   }
   return 0;
}


/*** view_cb **********************************************************
   Called when the "TOP", "SOUTH" or "WEST" buttons are selected.
**********************************************************************/
static int view_cb( LUI_NEWBUTTON *b )
{
   int dindex, oindex;
   int index = b->context_index;
   int dyo, dhowmany, dwhichones[VIS5D_MAX_CONTEXTS];
   GuiContext gtx = get_gui_gtx((index/MM_MAX)-1);
   
   dindex = gtx->context_index;
   oindex = index - (dindex * MM_MAX) - (MM_MAX);
   if (b->mousebutton==Button3) {
      if (oindex==1) {
         if (gtx->group_index > 0){
            vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
            for (dyo = 0; dyo < dhowmany; dyo++){
               vis5d_set_ortho_view(dwhichones[dyo], VIS5D_BOTTOM );
               vis5d_invalidate_dtx_frames(dwhichones[dyo]);
            }
         }
         else{
            vis5d_set_ortho_view(dindex, VIS5D_BOTTOM );
         }
      }
      else if (oindex==2) {
         if (gtx->group_index > 0){         
            vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);         
            for (dyo = 0; dyo < dhowmany; dyo++){         
               vis5d_set_ortho_view(dwhichones[dyo], VIS5D_NORTH );
               vis5d_invalidate_dtx_frames(dwhichones[dyo]);
            }         
         }         
         else{         
            vis5d_set_ortho_view(dindex, VIS5D_NORTH );         
         }
      }
      else {
         if (gtx->group_index > 0){                  
            vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
            for (dyo = 0; dyo < dhowmany; dyo++){                  
               vis5d_set_ortho_view(dwhichones[dyo], VIS5D_EAST );
               vis5d_invalidate_dtx_frames(dwhichones[dyo]);
            }                  
         }                  
         else{                  
            vis5d_set_ortho_view(dindex, VIS5D_EAST );                              
         }         
      }         
   }
   else {
      if (oindex==1) {
         if (gtx->group_index > 0){                  
            vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
            for (dyo = 0; dyo < dhowmany; dyo++){                  
               vis5d_set_ortho_view(dwhichones[dyo], VIS5D_TOP );
               vis5d_invalidate_dtx_frames(dwhichones[dyo]);
            }                  
         }                  
         else{                  
            vis5d_set_ortho_view(dindex, VIS5D_TOP );                              
         }         
      }         
      else if (oindex==2) {
         if (gtx->group_index > 0){                  
            vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
            for (dyo = 0; dyo < dhowmany; dyo++){                  
               vis5d_set_ortho_view(dwhichones[dyo], VIS5D_SOUTH );
               vis5d_invalidate_dtx_frames(dwhichones[dyo]);
            }                  
         }                  
         else{                  
            vis5d_set_ortho_view(dindex,VIS5D_SOUTH  );                              
         }         
      }         
      else {
         if (gtx->group_index > 0){                  
            vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
            for (dyo = 0; dyo < dhowmany; dyo++){                  
               vis5d_set_ortho_view(dwhichones[dyo], VIS5D_WEST );
               vis5d_invalidate_dtx_frames(dwhichones[dyo]);
            }                  
         }                  
         else{                  
            vis5d_set_ortho_view(dindex, VIS5D_WEST );                              
         }         
      }         
   }

   return 0;
}




/*** mode_cb ******************************************************
   This function is called when the user clicks on any of the mode
   radio button widgets.
**********************************************************************/
static int mode_cb( LUI_BUTTON *pb )
{
   int index = pb->context_index;
   int mindex, dindex;
   GuiContext gtx = get_gui_gtx((index/MM_MAX-1));
   XWindowAttributes winatts;

   dindex = gtx->context_index;
   mindex = index - (dindex * MM_MAX) - MM_MAX;

   if (gtx->MouseMode==MM_TRAJ)
      XUnmapWindow( GuiDpy, gtx->TrajWindow );

   if (gtx->MouseMode==MM_SOUND){
      vis5d_unmap_sndwindow( dindex);
      XUnmapWindow( GuiDpy, gtx->SoundCtrlWindow );
      group_event(gtx->group_index, 101, 0);
   }
   if (gtx->MouseMode==MM_LABEL){
      if (gtx->group_index > 0){
         group_event(gtx->group_index, 102, 0 );
      }
      else{
         vis5d_edit_label( dindex, '\r' );
      }
   }
   gtx->MouseMode = mindex;

   switch (gtx->MouseMode) {
      case MM_NORMAL:
         LUI_LabelChangeText( gtx->ModeInfoLabel, 6, ModeInfo1 );
         vis5d_graphics_mode(dindex, VIS5D_CURSOR, VIS5D_OFF);
         vis5d_graphics_mode(dindex, VIS5D_PROBE, VIS5D_OFF);
         vis5d_graphics_mode(dindex, VIS5D_SOUND, VIS5D_OFF);
         vis5d_graphics_mode(dindex, VIS5D_CLIP, VIS5D_OFF);
         group_event(gtx->group_index, 14, MM_NORMAL );
         break;
      case MM_TRAJ:
         LUI_LabelChangeText( gtx->ModeInfoLabel, 6, ModeInfo2 );
         XMapWindow( GuiDpy, gtx->TrajWindow );
         vis5d_graphics_mode(dindex, VIS5D_CURSOR, VIS5D_ON);
         vis5d_graphics_mode(dindex, VIS5D_PROBE, VIS5D_OFF);
         vis5d_graphics_mode(dindex, VIS5D_SOUND, VIS5D_OFF);
         vis5d_graphics_mode(dindex, VIS5D_CLIP, VIS5D_OFF);
         group_event(gtx->group_index, 14, MM_TRAJ);
         break;
      case MM_SLICE:
         LUI_LabelChangeText( gtx->ModeInfoLabel, 6, ModeInfo4 );
         vis5d_graphics_mode(dindex, VIS5D_CURSOR, VIS5D_OFF);
         vis5d_graphics_mode(dindex, VIS5D_PROBE, VIS5D_OFF);
         vis5d_graphics_mode(dindex, VIS5D_SOUND, VIS5D_OFF);
         vis5d_graphics_mode(dindex, VIS5D_CLIP, VIS5D_OFF);
         group_event(gtx->group_index, 14, MM_SLICE);
         break;
      case MM_LABEL:
         LUI_LabelChangeText( gtx->ModeInfoLabel, 6, ModeInfo3 );
         vis5d_graphics_mode(dindex, VIS5D_CURSOR, VIS5D_OFF);
         vis5d_graphics_mode(dindex, VIS5D_PROBE, VIS5D_OFF);
         vis5d_graphics_mode(dindex, VIS5D_SOUND, VIS5D_OFF);
         vis5d_graphics_mode(dindex, VIS5D_CLIP, VIS5D_OFF);
         group_event(gtx->group_index, 14, MM_LABEL);
         break;
      case MM_PROBE:
         LUI_LabelChangeText( gtx->ModeInfoLabel, 6, ModeInfo5 );
         vis5d_graphics_mode(dindex, VIS5D_CURSOR, VIS5D_ON);
         vis5d_graphics_mode(dindex, VIS5D_PROBE, VIS5D_ON);
         if (pb->mousebutton == Button2 ||
             pb->mousebutton == Button3){
            vis5d_graphics_mode(dindex, VIS5D_PROBE_ON_TRAJ, VIS5D_ON);
         }
         else{
            vis5d_graphics_mode(dindex, VIS5D_PROBE_ON_TRAJ, VIS5D_OFF);
         }
         vis5d_graphics_mode(dindex, VIS5D_SOUND, VIS5D_OFF);
         vis5d_graphics_mode(dindex, VIS5D_CLIP, VIS5D_OFF);
         group_event(gtx->group_index, 14, MM_PROBE);
         break;
      case MM_SOUND:
         LUI_LabelChangeText( gtx->ModeInfoLabel, 6, ModeInfo5 );
         vis5d_graphics_mode(dindex, VIS5D_CURSOR, VIS5D_ON);
         vis5d_graphics_mode(dindex, VIS5D_PROBE, VIS5D_OFF);
         vis5d_graphics_mode(dindex, VIS5D_SOUND, VIS5D_ON);
         vis5d_graphics_mode(dindex, VIS5D_CLIP, VIS5D_OFF);

         /* MJK 12.04.98 */
          vis5d_map_sndwindow( dindex);


         vis5d_draw_sounding_only( dindex, 1);
         XGetWindowAttributes(GuiDpy, gtx->SoundCtrlWindow, &winatts);
         if (gtx->othersnddpy != 0){
            vis5d_resize_sounding_window( dindex, winatts.width, winatts.height,
                                          winatts.x, winatts.y);
         }
         LUI_BorderWidth(2);
         group_event(gtx->group_index, 14, MM_SOUND);
         break; 
      case MM_CLIP:
         LUI_LabelChangeText( gtx->ModeInfoLabel, 6, ModeInfo6 );
         vis5d_graphics_mode(dindex, VIS5D_CURSOR, VIS5D_OFF);
         vis5d_graphics_mode(dindex, VIS5D_PROBE, VIS5D_OFF);
         vis5d_graphics_mode(dindex, VIS5D_SOUND, VIS5D_OFF);
         vis5d_graphics_mode(dindex, VIS5D_CLIP, VIS5D_ON);
         group_event(gtx->group_index, 14, MM_CLIP);
         break;          
      }
   
   vis5d_invalidate_dtx_frames(dindex);
   return 0;
}



/* trajectory set buttons */
static int traj_cb( LUI_BUTTON *pb )
{
   int set;
   int index = pb->context_index;
   GuiContext gtx = get_gui_gtx(index);

   set = pb->index;

   if (pb->event->xbutton.button==Button1) {
      /*** Left Button -> toggle set on/off ***/
      if (vis5d_enable_graphics(gtx->array_of_ctxs[0], VIS5D_TRAJ, set, VIS5D_TOGGLE)) {
         if (gtx->group_index > 0){
            group_event( gtx->group_index, 15+set,
            vis5d_enable_graphics(gtx->array_of_ctxs[0], VIS5D_TRAJ, set, VIS5D_GET));
         }
         vis5d_set_cursor_color( index, set );
         gtx->cur_trajset = set;
      }
   }
   else if (pb->event->xbutton.button==Button2){
      /* Middle Button -> select set */
      if (pb->state == LUI_BUTTON_OUT)
         pb->state = LUI_BUTTON_IN;
      else
         pb->state = LUI_BUTTON_OUT;
      vis5d_set_cursor_color( index, set );
      gtx->cur_trajset = set;
   }
   else if (pb->event->xbutton.button==Button3) {
      /* Right Button -> color selector */
      if (pb->state == LUI_BUTTON_OUT)
         pb->state = LUI_BUTTON_IN;
      else
         pb->state = LUI_BUTTON_OUT;
      XSync( GuiDpy, 0 );
      show_isocolor_window( gtx, VIS5D_TRAJ,
                            gtx->array_of_ctxs[0], set, 0 ); 
   }

   vis5d_invalidate_dtx_frames(index);
   return 0;
}
/******************************************************************/
/* Input: index - gui index                                       */
/*        row   - the row in the gui var list, in                 */
/*                the range of [0...VIS5D_MAX_CONTEXTS * MAXVARS] */
/*                                                                */
/* Output: the number of the var according to the vis5d_context   */
/******************************************************************/
int get_button_ctx_row( int index, int row)
{
   int yo, counter, numvars, spandex;
   GuiContext gtx = get_gui_gtx(index);

   counter = -1;
   for (yo = 0; yo <gtx->how_many_regular_contexts; yo++){
      spandex = gtx->array_of_ctxs[yo];
      numvars = gtx->RegularNumVars[spandex];
      counter += numvars;
      if (row <= counter){
         return ((row-counter) + (numvars-1));
      }
   }   
   printf("error in get_button_row\n");
   return -1;
}    

/******************************************************************/
/* Input: index - gui index                                       */
/*        row   - the row in the gui var list, in                 */
/*                the range of [0...VIS5D_MAX_CONTEXTS * MAXVARS] */
/*                                                                */
/* Output: the vis5d context_index associated with the var located*/
/*         in that row.                                           */
/******************************************************************/

int get_button_ctx_index(int index, int row)
{
   int yo, counter, numvars, spandex;
   GuiContext gtx = get_gui_gtx(index);
   
   counter = -1;
   for (yo = 0; yo <gtx->how_many_regular_contexts; yo++){
      spandex = gtx->array_of_ctxs[yo];
      numvars = gtx->RegularNumVars[spandex];
      counter += numvars;
      if (row <= counter){
         return spandex;
      }
   }
   printf("something wrong in get_button_ctx_index\n"); 
   return -1;
}
/******************************************************************/
/* Input dindex - the display or gui ctx index                    */
/*       vindex - the vis5d_ctx index                             */
/*       row    - the var number according to the vis5d_ctx       */
/* Output: the correct row number according to the gui_ctx of the */
/*         given vis5d var number                                 */
/******************************************************************/
 
int get_button_gtx_index(int dindex, int vindex, int row)
{
   GuiContext gtx = get_gui_gtx(dindex);
   int counter, yo, numvars, spandex;
 
   counter = 0;   
   for (yo = 0; yo <gtx->how_many_regular_contexts; yo++){
      spandex = gtx->array_of_ctxs[yo];
      numvars = gtx->RegularNumVars[spandex];
      if ( vindex == spandex ){
         return row+counter;
      }
      counter += numvars;
   }
   return -1;
}   
         
int is_ok_button_to_press( int dindex, int vindex, int row)
{
   GuiContext gtx = get_gui_gtx(dindex);
   int chowmany, cwhichones[VIS5D_MAX_CONTEXTS];
   int good,  cyo;
   char thename[50];
   
   if (gtx->group_index > 0){
      vis5d_get_ctx_var_name( vindex, row, thename);
      vis5d_get_num_of_ctxs_in_display( dindex, &chowmany, cwhichones);
      for ( cyo = 0; cyo < chowmany; cyo++){
         good = vis5d_find_var(cwhichones[cyo], thename);
         if (good>-1){
            if (cwhichones[cyo] == vindex){
               /* its the first one in ther so it's A OKAY! */
               return 1;
            }
            else{
               return 0;
            }
         }
      }
      /* this shludn't happen but oh well */
      return 1;
   }
   else{
      return 1;
   }
}
            

   
/**********************************************************/
/*** CALL-BACK FUNCTIONS FOR MATRIX OF GRAPHICS BUTTONS ***/
/**********************************************************/

static int irregular_matrix_cb( LUI_BUTTON_MATRIX *bm, int row,
                                       int col, int button )
{
   int index = bm->context_index;
   GuiContext gtx = get_gui_gtx(index);

   if (col == 0){
      show_text_plot_window( index, gtx->array_of_itxs[row]);
   }

   return 0;
}



/*
 * This function is called whenever a button in the button matrix is
 * clicked on.
 */
static int button_matrix_cb( LUI_BUTTON_MATRIX *bm, int row, int col,
                             int button )
{
   int curtime, numtimes, time;
   int ctxcurtime, ctxnumtimes;
   int CVO, CV;
   int index = bm->context_index;
   GuiContext gtx = get_gui_gtx(index);
   int state = LUI_ButtonMatrixGetState( bm, row, col );
   int vindex, dindex;
   int vrow;
   char name[20];
   int gloop, spandex, grow, i, yo, cbutton;
   int dhowmany, dwhichones[VIS5D_MAX_CONTEXTS];
   int chowmany, cwhichones[VIS5D_MAX_CONTEXTS];
   int times, dyo, cyo, good;
   char aname[20];


   /*index of vis5d_ctx */
   vindex = get_button_ctx_index( index, row);
   gtx->current_ctx = vindex;

   /* variable number of the specified row */
   vrow = get_button_ctx_row( index, row );
 
   /*index of display_ctx */
   dindex = index;

   vis5d_get_dtx_timestep(dindex, &curtime);
   vis5d_get_dtx_numtimes(dindex, &numtimes);
   vis5d_get_ctx_timestep(vindex, &ctxcurtime);
   vis5d_get_ctx_numtimes(vindex, &ctxnumtimes);

      
   /* get all the nessecary info for grouping */


   /* MJK 11.17.98 */
   /* put this code in vis5d_set_display_group */
   /*
   if (gtx->group_index > 0 && col < 6 && button==Button1){
      vis5d_set_grp_var_values( gtx->group_index);
   }
   */
   if (button==Button1) {
      /*** Left Button -> toggle graphic on/off ***/
      if (col<5) {
         if (!is_ok_button_to_press(dindex, vindex, vrow)){
            return 0;
         }
         else{
            LUI_ButtonMatrixSetState( bm, row, col, !state );
         }
      }
      vis5d_signal_redraw(dindex, 2);
      switch (col) {
         case 0:
            /** Iso-Surface **/
            cbutton = vis5d_enable_graphics(vindex, VIS5D_ISOSURF, vrow, VIS5D_GET);
            if (!cbutton){
               vis5d_enable_graphics(vindex, VIS5D_ISOSURF, vrow, VIS5D_ON);
               if (gtx->cur_isosurf>=0) {
                  XUnmapWindow(GuiDpy,gtx->IsoWindow);
               }
               gtx->cur_isosurf = vrow;
               gtx->cur_isosurfvindex = vindex;
               gtx->cur_isosurfmap = 1;
               map_isosurf_window( dindex, vindex, vrow );
               update_linked_buttons( vindex, dindex, VIS5D_ISOSURF, vrow, 1);
            }         
            else{
               update_linked_buttons( vindex, dindex, VIS5D_ISOSURF, vrow, 0);
               XUnmapWindow(GuiDpy,gtx->IsoWindow);
               gtx->cur_isosurf = -1;
               gtx->cur_isosurfvindex = -1;
               gtx->cur_isosurfmap = 0;
               vis5d_enable_graphics(vindex, VIS5D_ISOSURF, vrow, VIS5D_OFF);
            }
            break;
         case 1:
            /** Horizontal Contour Line Slice **/
            XUnmapWindow(GuiDpy,gtx->HSliceWindow);
            cbutton = vis5d_enable_graphics(vindex, VIS5D_HSLICE, vrow, VIS5D_GET);
            if (!cbutton){
               vis5d_enable_graphics(vindex, VIS5D_HSLICE, vrow, VIS5D_ON);
               if (gtx->cur_hslice>=0){
                  XUnmapWindow(GuiDpy,gtx->HSliceWindow);
               }
               gtx->cur_hslice = vrow;
               gtx->cur_hslicevindex = vindex;
               gtx->cur_hslicemap = 1;
               map_hslice_window( dindex, vindex, vrow );
               update_linked_buttons(vindex, dindex, VIS5D_HSLICE, vrow, 1);
               for (time=0;time<ctxnumtimes;time++) {
                  vis5d_make_hslice(vindex, time, vrow, time==ctxcurtime);
               }
            }  
            else{
               update_linked_buttons(vindex, dindex, VIS5D_HSLICE, vrow, 0);
               XUnmapWindow(GuiDpy,gtx->HSliceWindow);
               gtx->cur_hslice = -1;
               gtx->cur_hslicevindex = -1;
               gtx->cur_hslicemap = 0;
               vis5d_enable_graphics(vindex, VIS5D_HSLICE, vrow, VIS5D_OFF);
            }
            break;
         case 2:
            /** Vertical Contour Line Slice **/
            XUnmapWindow(GuiDpy,gtx->VSliceWindow);
            cbutton = vis5d_enable_graphics(vindex, VIS5D_VSLICE, vrow, VIS5D_GET);
            if (!cbutton){
               vis5d_enable_graphics(vindex, VIS5D_VSLICE, vrow, VIS5D_ON);
               if (gtx->cur_vslice>=0) {
                  XUnmapWindow(GuiDpy,gtx->VSliceWindow);
               }
               gtx->cur_vslice = vrow;
               gtx->cur_vslicevindex = vindex;
               gtx->cur_vslicemap = 1;
               map_vslice_window( dindex, vindex, vrow );
               update_linked_buttons(vindex, dindex, VIS5D_VSLICE, vrow, 1);
               for (time=0;time<ctxnumtimes;time++) {
                  vis5d_make_vslice(vindex, time, vrow, time==ctxcurtime);
               }
            }
            else{
               update_linked_buttons(vindex, dindex, VIS5D_VSLICE, vrow, 0);
               XUnmapWindow(GuiDpy,gtx->VSliceWindow);
               gtx->cur_vslice = -1;
               gtx->cur_vslicevindex = -1;
               gtx->cur_vslicemap = 0;
               vis5d_enable_graphics(vindex, VIS5D_VSLICE, vrow, VIS5D_OFF);
            }
            break;
         case 3:
            /** Horizontal Colored Slice **/
            cbutton = vis5d_enable_graphics(vindex, VIS5D_CHSLICE, vrow, VIS5D_GET);
            if (!cbutton){
               vis5d_enable_graphics(vindex, VIS5D_CHSLICE, vrow, VIS5D_ON);
               show_colorbar_window( dindex, vindex, VIS5D_CHSLICE, row );
               update_linked_buttons(vindex, dindex, VIS5D_CHSLICE, vrow, 1);
               for (time=0;time<ctxnumtimes;time++) {
                  vis5d_make_chslice(vindex, time, vrow, time==ctxcurtime);
               }
            }
            else{
               update_linked_buttons(vindex, dindex, VIS5D_CHSLICE, vrow, 0);
               vis5d_enable_graphics(vindex, VIS5D_CHSLICE, vrow, VIS5D_OFF);
               hide_chcolorbar_window( dindex );
            }
            break;
         case 4:
            cbutton = vis5d_enable_graphics(vindex, VIS5D_CVSLICE, vrow, VIS5D_GET);
            if (!cbutton){
               vis5d_enable_graphics(vindex, VIS5D_CVSLICE, vrow, VIS5D_ON);
               show_colorbar_window( dindex, vindex, VIS5D_CVSLICE, row );
               update_linked_buttons(vindex, dindex, VIS5D_CVSLICE, vrow, 1);
               for (time=0;time<ctxnumtimes;time++) {
                  vis5d_make_cvslice(vindex, time, vrow, time==ctxcurtime);
               }
            }
            else{
               update_linked_buttons(vindex, dindex, VIS5D_CVSLICE, vrow, 0);
               vis5d_enable_graphics(vindex, VIS5D_CVSLICE, vrow, VIS5D_OFF);
               hide_colorbar_window( dindex );
            }
            break;
         case 5:
            /** Volume **/
            /** Volume **/
            vis5d_get_volume(dindex,  &CVO, &CV);
            if (vrow==CV && vindex == CVO &&
                CV != -1) {
               /* turn off the volume */
               LUI_ButtonMatrixSetState( gtx->ButtonMatrix, row, 5, 0 );
               if (cb_graphic[cb_dindex]==VIS5D_VOLUME && cb_var[cb_dindex]==vrow) {
                  hide_colorbar_window( dindex );
               }
               update_linked_buttons(CVO, dindex,
                                     VIS5D_VOLUME, CV, 0);
               gtx->CurrentVolume = -1;
               gtx->CurrentVolumeOwner = -1;
            }
            else {
               /* turn on a different volume */
               if (CV>=0) {
                  int cv;
                  /* turn off previous volume button */
                  cv = get_button_gtx_index(dindex, CVO, CV);
                  LUI_ButtonMatrixSetState( gtx->ButtonMatrix, cv, 5, 0 );
                  update_linked_buttons(CVO, dindex, VIS5D_VOLUME,
                                        CV , 0);
               }
               gtx->CurrentVolume = vrow;
               gtx->CurrentVolumeOwner = vindex;
               update_linked_buttons(vindex, dindex, VIS5D_VOLUME, 
                                        vrow , 1);
               show_colorbar_window( dindex, vindex, VIS5D_VOLUME, row );
               LUI_ButtonMatrixSetState( gtx->ButtonMatrix, row, 5, 1 );
            }
            vis5d_set_volume(dindex, gtx->CurrentVolumeOwner, gtx->CurrentVolume);
            break;
      } /*switch*/
      if (gtx->group_index > 0){
         vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
         for (dyo = 0; dyo < dhowmany; dyo++){
            vis5d_invalidate_dtx_frames(dwhichones[dyo]);
         }
      }
      else{
         vis5d_invalidate_dtx_frames(dindex);
      }
   }
   else if (button==Button2) {
      /* Middle Button -> toggle slider/selector window on/off ***/
      switch (col) {
         case 0:
            if (gtx->cur_isosurf>=0)
               XUnmapWindow(GuiDpy,gtx->IsoWindow);
            if (gtx->cur_isosurf == vrow &&
                gtx->cur_isosurfvindex == vindex &&
                gtx->cur_isosurfmap == 1){
               gtx->cur_isosurfmap = 0;
            }
            else {
               gtx->cur_isosurf = vrow;
               gtx->cur_isosurfvindex = vindex;
               gtx->cur_isosurfmap = 1;
               map_isosurf_window( dindex, vindex, gtx->cur_isosurf );
            }
            break;
         case 1:
            if (gtx->cur_hslice>=0)
               XUnmapWindow(GuiDpy,gtx->HSliceWindow);
            if (gtx->cur_hslice==vrow &&
                gtx->cur_hslicevindex == vindex &&
                gtx->cur_hslicemap==1){
               gtx->cur_hslicemap = 0;
            }
            else {
               gtx->cur_hslice = vrow;
               gtx->cur_hslicevindex = vindex;
               gtx->cur_hslicemap = 1;
               map_hslice_window( dindex, vindex, vrow );
            }
            break;
         case 2:
            if (gtx->cur_vslice>=0)
               XUnmapWindow(GuiDpy,gtx->VSliceWindow);
            if (gtx->cur_vslice==vrow &&
                gtx->cur_vslicevindex==vindex &&
                gtx->cur_vslicemap==1){
               gtx->cur_vslicemap = 0;
            }
            else {
               gtx->cur_vslice = vrow;
               gtx->cur_vslicevindex = vindex;
               gtx->cur_vslicemap = 1;
               map_vslice_window( dindex, vindex, vrow );
            }
            break;
         case 3:

            /* MJK 12.04.98 */
            if (cb_chvar[cb_dindex]==vrow) {
               hide_chcolorbar_window( dindex );
            }
            else {
               show_colorbar_window( dindex, vindex, VIS5D_CHSLICE, row );
            }
            break;
         case 4:
            if (cb_graphic[cb_dindex]==VIS5D_CVSLICE && cb_var[cb_dindex]==vrow) {
               hide_colorbar_window( dindex );
            }
            else {
               show_colorbar_window( dindex, vindex, VIS5D_CVSLICE, row );
            }
            break;
         case 5:
            if (cb_graphic[cb_dindex]==VIS5D_VOLUME && cb_var[cb_dindex]==vrow) {
               hide_colorbar_window( dindex );
            }
            else {
               show_colorbar_window( dindex, vindex, VIS5D_VOLUME, row );
            }
            break;
      }
   }
   else if (button==Button3) {
      /* Right Button -> color selector */
      if (col<5) {
         gtx->ColorRow = row;
         gtx->ColorCol = col;
         if (col==0) {
            /* Isosurface color */
            XSync( GuiDpy, 0 );
            show_isocolor_window( gtx, VIS5D_ISOSURF, vindex, row, 0 );
         }
         else if (col==1) {
            /* HSlice color */
            show_rgb_sliders( gtx, VIS5D_HSLICE, vindex, row, 0 );
         }
         else if (col==2) {
            /* VSlice color */
            show_rgb_sliders( gtx, VIS5D_VSLICE, vindex, row, 0 );
         }
         else if (col==3) {
            /* Colored HSlice tickmark color */
            show_rgb_sliders( gtx, VIS5D_CHSLICE, vindex, row, 0 );
         }
         else if (col==4) {
            /* Colored VSlice tickmark color */
            show_rgb_sliders( gtx, VIS5D_CVSLICE, vindex, row, 0 );
         }
      }
   }
   vis5d_signal_redraw(dindex, 2);
   return 1;
}




/****************************************************************/
/*** CALL-BACK FUNCTIONS FOR ISOSURFACE, SLICE WINDOW WIDGETS ***/
/****************************************************************/


/*** isosurface_cb ****************************************************
   This is the call-back function for the isosurface level widget.
**********************************************************************/
static isosurface_cb( LUI_NEWSLIDER *s, float value )
{
   float IsoLevel;
   int index = s->context_index;
   GuiContext gtx = get_gui_gtx(index);
   int dhowmany, dwhichones[VIS5D_MAX_CONTEXTS];
   int chowmany, cwhichones[VIS5D_MAX_CONTEXTS];
   int dyo, cyo, good;
   char aname[20];

   vis5d_get_isosurface(gtx->cur_isosurfvindex, gtx->cur_isosurf, &IsoLevel);
   if (IsoLevel!=value) {
      IsoLevel = value;
   }
   vis5d_get_ctx_var_name( gtx->cur_isosurfvindex, gtx->cur_isosurf, aname);
   if (gtx->group_index > 0){
      group_event(gtx->group_index, 5, 1);
      vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
      for (dyo = 0; dyo < dhowmany; dyo++){
         vis5d_get_num_of_ctxs_in_display( dwhichones[dyo], &chowmany, cwhichones);
         for ( cyo = 0; cyo < chowmany; cyo++){
            good = vis5d_find_var(cwhichones[cyo], aname);
            if (good>-1){
               vis5d_set_isosurface(cwhichones[cyo], good, IsoLevel);
            }
         }
      }
   }
   else{
      vis5d_set_isosurface(gtx->cur_isosurfvindex, gtx->cur_isosurf, IsoLevel);
   }
   return 0;
}


/* called when the "NEWSURF" button is selected */
static newsurf_cb( LUI_NEWBUTTON *pb )
{
   int curtime, numtimes, i;
   int index = pb->context_index;
   GuiContext gtx = get_gui_gtx(index);
   int dhowmany, dwhichones[VIS5D_MAX_CONTEXTS];
   int chowmany, cwhichones[VIS5D_MAX_CONTEXTS];
   int times, dyo, cyo, good;
   char aname[20];

   vis5d_get_ctx_timestep(gtx->cur_isosurfvindex, &curtime);
   vis5d_get_ctx_numtimes( gtx->cur_isosurfvindex, &numtimes );
   if (gtx->cur_isosurf>-1) {
      if (gtx->group_index > 0){
         vis5d_get_ctx_var_name( gtx->cur_isosurfvindex, gtx->cur_isosurf, aname);
         group_event(gtx->group_index, 5, 1);
         vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
         for (dyo = 0; dyo < dhowmany; dyo++){
            vis5d_get_num_of_ctxs_in_display( dwhichones[dyo], &chowmany, cwhichones);
            for ( cyo = 0; cyo < chowmany; cyo++){
               good = vis5d_find_var(cwhichones[cyo], aname);
               if (good>-1){
                  vis5d_get_ctx_numtimes( cwhichones[cyo], &numtimes );
                  vis5d_get_ctx_timestep( cwhichones[cyo],  &curtime);
                  for ( times = 0; times < numtimes; times ++){
                     vis5d_make_isosurface( cwhichones[cyo], times, good, times==curtime);
                  }
                  cyo = chowmany;
               }
            }
         }
      }
      else{
         for (i=0;i<numtimes;i++) {
            vis5d_make_isosurface( gtx->cur_isosurfvindex, i, gtx->cur_isosurf, i==curtime );
         }
         vis5d_signal_redraw(index, 1);
      }
   }
   return 0;
}


static int texture_cb( LUI_NEWBUTTON *b, int state )
{
   int index = b->context_index;
   vis5d_graphics_mode(index, VIS5D_TEXTURE, VIS5D_TOGGLE);
   vis5d_invalidate_dtx_frames(index);
   vis5d_signal_redraw(index, 1);
   return 0;
}



/*
 * This function is called when the user has entered a new value into
 * the horizontal contour line slice interval widget.
 */
static int hslice_cb( LUI_FIELD *field )
{
   char valstr[1000];
   int ctxcurtime, ctxnumtimes, time;
   float minval, maxval;
   float cur_interval, cur_low, cur_high, cur_level;
   int index = field->context_index;
   GuiContext gtx = get_gui_gtx(index);
   int dhowmany, dwhichones[VIS5D_MAX_CONTEXTS];
   int chowmany, cwhichones[VIS5D_MAX_CONTEXTS];
   int dyo, cyo, good;
   char aname[30];

   vis5d_get_ctx_timestep(gtx->cur_hslicevindex, &ctxcurtime);
   vis5d_get_ctx_numtimes(gtx->cur_hslicevindex, &ctxnumtimes);
   vis5d_get_ctx_var_range(gtx->cur_hslicevindex, gtx->cur_hslice, &minval, &maxval);
   vis5d_get_hslice(gtx->cur_hslicevindex, gtx->cur_hslice, &cur_interval, &cur_low,
                    &cur_high, &cur_level);

   LUI_FieldGetText( gtx->hslice_field, valstr );

   if (gtx->cur_hslice>=0) {
      float interval, low, high;
      if ( strchr(valstr,'(') && strchr(valstr,')') && strchr(valstr,',') ) {
         sscanf( valstr,"%f (%f,%f)", &interval, &low, &high );
      }
      else {
         sscanf( valstr,"%f", &interval );
         low = minval;
         high = maxval;
      }
      /* interval = 0 not allowed */
      if (interval==0.0)
        interval = cur_interval;
      /* verify low and high */
      if (low>=high) {
         low = minval;
         high = maxval;
      }
      /* BUG FIX MJK 8.7.98 */
      /* This grouping code was not here before */
      if (gtx->group_index > 0){
         vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
         vis5d_get_ctx_var_name( gtx->cur_hslicevindex, gtx->cur_hslice, aname);
         for (dyo = 0; dyo < dhowmany; dyo++){
            vis5d_get_num_of_ctxs_in_display( dwhichones[dyo], &chowmany, cwhichones);
            for ( cyo = 0; cyo < chowmany; cyo++){
               good = vis5d_find_var(cwhichones[cyo], aname);
               if (good>-1){
                  vis5d_get_hslice(cwhichones[cyo], good, &cur_interval,
                                   &cur_low, &cur_high, &cur_level);
                  vis5d_get_ctx_timestep(cwhichones[cyo], &ctxcurtime);
                  vis5d_get_ctx_numtimes(cwhichones[cyo], &ctxnumtimes);
                  if (cur_interval!=interval || cur_low!=low ||
                                                cur_high!=high) {
                     vis5d_set_hslice(cwhichones[cyo], good,
                                      interval, low, high, cur_level);
                     for (time=0;time<ctxnumtimes;time++) {
                        vis5d_make_hslice(cwhichones[cyo], time, good, time==ctxcurtime);
                     }
                  }
                  vis5d_signal_redraw(dwhichones[dyo], 1); 
               }
            }
         }
      }                     
      else{
         if (cur_interval!=interval || cur_low!=low || cur_high!=high) {
            vis5d_set_hslice(gtx->cur_hslicevindex, gtx->cur_hslice, interval, low, high,
                             cur_level);
            for (time=0;time<ctxnumtimes;time++) {
               vis5d_make_hslice(gtx->cur_hslicevindex, time, gtx->cur_hslice, time==ctxcurtime);
            }
         }
         vis5d_signal_redraw(index, 1); 
      }
   }
   return 0;
}



/*
 * This function is called when the user has entered a new value into
 * the vertical contour line slice interval widget.
 */
static int vslice_cb( LUI_FIELD *field )
{
   char valstr[1000];
   int ctxcurtime, ctxnumtimes, time;
   float minval, maxval;
   float VSliceInterval, VSliceLowLimit, VSliceHighLimit;
   float row0, col0, row1, col1;
   int index = field->context_index;
   GuiContext gtx = get_gui_gtx(index);
   int dhowmany, dwhichones[VIS5D_MAX_CONTEXTS];
   int chowmany, cwhichones[VIS5D_MAX_CONTEXTS];
   int dyo, cyo, good;
   char aname[30];

   vis5d_get_ctx_timestep(gtx->cur_vslicevindex, &ctxcurtime);
   vis5d_get_ctx_numtimes(gtx->cur_vslicevindex, &ctxnumtimes);
   vis5d_get_ctx_var_range(gtx->cur_vslicevindex, gtx->cur_vslice, &minval, &maxval);
   vis5d_get_vslice(gtx->cur_vslicevindex, gtx->cur_vslice, &VSliceInterval, &VSliceLowLimit,
                    &VSliceHighLimit, &row0, &col0, &row1, &col1);

   LUI_FieldGetText( gtx->vslice_field, valstr );

   if (gtx->cur_vslice>=0) {
      float interval, low, high;
      if ( strchr(valstr,'(') && strchr(valstr,')') && strchr(valstr,',') ) {
         sscanf( valstr,"%f (%f,%f)", &interval, &low, &high );
      }
      else {
         sscanf( valstr,"%f", &interval );
         low = minval;
         high = maxval;
      }
      /* interval = 0 not allowed */
      if (interval==0.0)
        interval = VSliceInterval;
      /* verify low and high */
      if (low>=high) {
         low = minval;
         high = maxval;
      }
      /* BUG FIX MJK 8.7.98 */
      /* This grouping code was not here before */
      if (gtx->group_index > 0){
         vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
         vis5d_get_ctx_var_name( gtx->cur_vslicevindex, gtx->cur_vslice, aname);
         for (dyo = 0; dyo < dhowmany; dyo++){
            vis5d_get_num_of_ctxs_in_display( dwhichones[dyo], &chowmany, cwhichones);
            for ( cyo = 0; cyo < chowmany; cyo++){
               good = vis5d_find_var(cwhichones[cyo], aname);
               if (good>-1){
                  vis5d_get_vslice(cwhichones[cyo], good, &VSliceInterval,
                      &VSliceLowLimit, &VSliceHighLimit, &row0, &col0, &row1, &col1);
                  vis5d_get_ctx_timestep(cwhichones[cyo], &ctxcurtime);
                  vis5d_get_ctx_numtimes(cwhichones[cyo], &ctxnumtimes);
                  if (VSliceInterval!=interval || VSliceLowLimit!=low ||
                      VSliceHighLimit!=high) {
                     vis5d_set_vslice(cwhichones[cyo], good, interval, low, high,
                                      row0, col0, row1, col1);
                     for (time=0;time<ctxnumtimes;time++) {
                        vis5d_make_vslice(cwhichones[cyo], time, good, time==ctxcurtime);
                     }
                  }
                  vis5d_signal_redraw(dwhichones[dyo], 1);
               }
            }
         }
      }
      else{
         if (VSliceInterval!=interval || VSliceLowLimit!=low || VSliceHighLimit!=high) {
            vis5d_set_vslice(gtx->cur_vslicevindex, gtx->cur_vslice, interval, low, high,
                             row0, col0, row1, col1);
            for (time=0;time<ctxnumtimes;time++) {
               vis5d_make_vslice(gtx->cur_vslicevindex, time, gtx->cur_vslice, time==ctxcurtime);
            }
         }
         vis5d_signal_redraw(index, 1);
      }
   }
   return 0;
}



/*** windscale_cb *****************************************************
   This function is called when the user has entered a new value into
   the wind scale widget.
**********************************************************************/
/* MJK 12.04.98 begin */

static int windscale_cb( LUI_FIELD *field, char *text )
{
   float f;
   int curtime, numtimes, time;
   float HWindDensity, HWindScale, HWindLevel;
   float VWindDensity, VWindScale, row0, col0, row1, col1;
   int index = field->context_index; /*index=display_ctx index*/
   GuiContext gtx = get_gui_gtx(index);
   int dhowmany, dwhichones[VIS5D_MAX_CONTEXTS];
   int dyo;

   vis5d_get_dtx_timestep(index, &curtime);
   vis5d_get_dtx_numtimes(index, &numtimes);

   f = LUI_FieldGetDouble( field );
   if (f==0.0) {
      f = 1.0;
      LUI_FieldSetDouble( field, f );
   }

   /* BUG FIX MJK 8.7.98 */
   /* This grouping code was not here before */
   if (gtx->cur_hwind>=0) {
      if (gtx->group_index > 0){
         vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
         for (dyo = 0; dyo < dhowmany; dyo++){
            gtx = gtx_table[dwhichones[dyo]];
            if (gtx->cur_hwind>=0) {
               vis5d_get_hwindslice(dwhichones[dyo], gtx->cur_hwind,
                              &HWindDensity, &HWindScale, &HWindLevel);
               if (HWindScale!=f) {
                  HWindScale = f;
                  vis5d_set_hwindslice(dwhichones[dyo], gtx->cur_hwind,
                             HWindDensity, HWindScale, HWindLevel);
                  vis5d_get_dtx_timestep(dwhichones[dyo], &curtime);
                  vis5d_get_dtx_numtimes(dwhichones[dyo], &numtimes);
                  for (time=0;time<numtimes;time++) {
                     vis5d_make_hwindslice(dwhichones[dyo], time,
                                      gtx->cur_hwind, time==curtime);
                  }
                  vis5d_signal_redraw(dwhichones[dyo], 1);
               }
            }
         }
      }
      else{
         vis5d_get_hwindslice(index, gtx->cur_hwind,
                              &HWindDensity, &HWindScale, &HWindLevel);
         if (HWindScale!=f) {
            HWindScale = f;
            vis5d_set_hwindslice(index, gtx->cur_hwind,
                                 HWindDensity, HWindScale, HWindLevel);
            for (time=0;time<numtimes;time++) {
               vis5d_make_hwindslice(index, time, gtx->cur_hwind, time==curtime);
            }
         }
      }
   }
   else if (gtx->cur_vwind>=0) {
      if (gtx->group_index > 0){
         vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
         for (dyo = 0; dyo < dhowmany; dyo++){
            gtx = gtx_table[dwhichones[dyo]];
            if (gtx->cur_vwind>=0) {
               vis5d_get_vwindslice(dwhichones[dyo], gtx->cur_vwind,
                     &VWindDensity, &VWindScale,
                     &row0, &col0, &row1, &col1);
               if (VWindScale!=f) {
                  VWindScale = f;
                  vis5d_set_vwindslice(dwhichones[dyo], gtx->cur_vwind,
                       VWindDensity, VWindScale,
                       row0, col0, row1, col1);
                  vis5d_get_dtx_timestep(dwhichones[dyo], &curtime);
                  vis5d_get_dtx_numtimes(dwhichones[dyo], &numtimes);
                  for (time=0;time<numtimes;time++) {
                     vis5d_make_vwindslice(dwhichones[dyo], time,
                                      gtx->cur_vwind, time==curtime);
                  }
                  vis5d_signal_redraw(dwhichones[dyo], 1);
               }
            }
         }
      }
      else{
         vis5d_get_vwindslice(index, gtx->cur_vwind, &VWindDensity, &VWindScale,
                              &row0, &col0, &row1, &col1);
         if (VWindScale!=f) {
            VWindScale = f;
            vis5d_set_vwindslice(index, gtx->cur_vwind, VWindDensity, VWindScale,
                                 row0, col0, row1, col1);
            for (time=0;time<numtimes;time++) {
               vis5d_make_vwindslice(index, time, gtx->cur_vwind, time==curtime);
            }
         }
      }
   }
   else if (gtx->cur_hstream>=0) {
      f = 1.0;
      LUI_FieldSetDouble( field, f );
   }

   return 0;
}

static int winddensity_cb( LUI_FIELD *field )
{
   float d;
   int curtime, numtimes, time;
   float HWindDensity, HWindScale, HWindLevel;
   float VWindDensity, VWindScale, row0, col0, row1, col1;
   float HStreamDensity, HStreamLevel;
   float VStreamDensity;
   int index = field->context_index; /* index = display_ctx index */
   GuiContext gtx = get_gui_gtx(index);
   int dhowmany, dwhichones[VIS5D_MAX_CONTEXTS];
   int dyo;

   vis5d_get_dtx_timestep(index, &curtime);
   vis5d_get_dtx_numtimes(index, &numtimes);

   d = LUI_FieldGetDouble( field );
   /* BUG FIX MJK 8.7.98 */
   /* This grouping code was not here before */
   if ((field == gtx->hwinddensity_field) && (gtx->cur_hwind >= 0)) {
      if (gtx->group_index > 0){
         vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
         for (dyo = 0; dyo < dhowmany; dyo++){
            gtx = gtx_table[dwhichones[dyo]];
            if (gtx->cur_hwind>=0) {
               if (d<0.0 || d>1.0) {
                  d = 1.0;
                  LUI_FieldSetDouble( field, d );
               }
               vis5d_get_hwindslice(dwhichones[dyo], gtx->cur_hwind,
                           &HWindDensity, &HWindScale, &HWindLevel);
               if (HWindDensity!=d) {
                  HWindDensity = d;
                  vis5d_set_hwindslice(dwhichones[dyo],gtx->cur_hwind,
                              HWindDensity, HWindScale, HWindLevel);
                  vis5d_get_dtx_timestep(dwhichones[dyo], &curtime);
                  vis5d_get_dtx_numtimes(dwhichones[dyo], &numtimes);
                  for (time=0;time<numtimes;time++) {
                     vis5d_make_hwindslice(dwhichones[dyo], time,



                                       gtx->cur_hwind, time==curtime);
                  }
                  vis5d_signal_redraw(dwhichones[dyo], 1);
               }
            }
         }
      }
      else{
         if (d<0.0 || d>1.0) {
            d = 1.0;
            LUI_FieldSetDouble( field, d );
         }
         vis5d_get_hwindslice(index, gtx->cur_hwind,
                              &HWindDensity, &HWindScale, &HWindLevel);
         if (HWindDensity!=d) {
            HWindDensity = d;
            vis5d_set_hwindslice(index, gtx->cur_hwind,
                                 HWindDensity, HWindScale, HWindLevel);
            for (time=0;time<numtimes;time++) {
               vis5d_make_hwindslice(index, time, gtx->cur_hwind, time==curtime);
            }
            vis5d_signal_redraw(index, 1);
         }
      }
   }
   else if ((field == gtx->winddensity_field) && (gtx->cur_vwind >= 0)) {
      if (gtx->group_index > 0){
         vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
         for (dyo = 0; dyo < dhowmany; dyo++){
            gtx = gtx_table[dwhichones[dyo]];
            if (gtx->cur_vwind>=0) {
               if (d<0.0 || d>1.0) {
                  d = 1.0;
                  LUI_FieldSetDouble( field, d );
               }
               vis5d_get_vwindslice(dwhichones[dyo], gtx->cur_vwind, &VWindDensity,
                              &VWindScale, &row0, &col0, &row1, &col1);
               if (VWindDensity!=d) {
                  VWindDensity = d;
                  vis5d_set_vwindslice(dwhichones[dyo], gtx->cur_vwind,
                      VWindDensity, VWindScale, row0, col0, row1, col1);
                  vis5d_get_dtx_timestep(dwhichones[dyo], &curtime);
                  vis5d_get_dtx_numtimes(dwhichones[dyo], &numtimes);
                  for (time=0;time<numtimes;time++) {
                     vis5d_make_vwindslice(dwhichones[dyo], time,
                          gtx->cur_vwind, time==curtime);
                  }
                  vis5d_signal_redraw(dwhichones[dyo], 1);
               }
            }
         }
      }
      else{
         if (d<0.0 || d>1.0) {
            d = 1.0;
            LUI_FieldSetDouble( field, d );
         }
         vis5d_get_vwindslice(index, gtx->cur_vwind, &VWindDensity, &VWindScale,
                              &row0, &col0, &row1, &col1);
         if (VWindDensity!=d) {
            VWindDensity = d;
            vis5d_set_vwindslice(index, gtx->cur_vwind, VWindDensity, VWindScale,
                                 row0, col0, row1, col1);
            for (time=0;time<numtimes;time++) {
               vis5d_make_vwindslice(index, time, gtx->cur_vwind, time==curtime);
            }
            vis5d_signal_redraw(index, 1);
         }
      }
   }
   else if ((field == gtx->hwinddensity_field) && (gtx->cur_hstream >= 0)) {
      if (gtx->group_index > 0){
         vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
         for (dyo = 0; dyo < dhowmany; dyo++){
            gtx = gtx_table[dwhichones[dyo]];
            if (gtx->cur_hstream>=0) {
               if (d < 0.5 || d > 2.0) {
                  if (d < 0.5) d = 0.5;
                  if (d > 2.0) d = 2.0;
                  LUI_FieldSetDouble( field, d );
               }
               vis5d_get_hstreamslice(dwhichones[dyo], gtx->cur_hstream,
               &HStreamDensity, &HStreamLevel);
               if (HStreamDensity!=d) {
                  HStreamDensity = d;
                  vis5d_set_hstreamslice(dwhichones[dyo], gtx->cur_hstream,
                  HStreamDensity, HStreamLevel);
                  vis5d_get_dtx_timestep(dwhichones[dyo], &curtime);
                  vis5d_get_dtx_numtimes(dwhichones[dyo], &numtimes);
                  for (time=0;time<numtimes;time++) {
                     vis5d_make_hstreamslice(dwhichones[dyo], time,
                     gtx->cur_hstream, time==curtime);
                  }
                  vis5d_signal_redraw(dwhichones[dyo], 1);
               }
            }
         }
      }
      else{
         if (d < 0.5 || d > 2.0) {
            if (d < 0.5) d = 0.5;
            if (d > 2.0) d = 2.0;
            LUI_FieldSetDouble( field, d );
         }
         vis5d_get_hstreamslice(index, gtx->cur_hstream, &HStreamDensity, &HStreamLevel);
         if (HStreamDensity!=d) {
            HStreamDensity = d;
            vis5d_set_hstreamslice(index, gtx->cur_hstream, HStreamDensity, HStreamLevel);
            for (time=0;time<numtimes;time++) {
               vis5d_make_hstreamslice(index, time, gtx->cur_hstream, time==curtime);
            }
            vis5d_signal_redraw(index, 1);
         }
      }
   }
   else if ((field == gtx->winddensity_field) && (gtx->cur_vstream >= 0)) {
      if (gtx->group_index > 0){
         vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
         for (dyo = 0; dyo < dhowmany; dyo++){
            gtx = gtx_table[dwhichones[dyo]];
            if (gtx->cur_vstream>=0) {
               if (d < 0.5 || d > 2.0) {
                  if (d < 0.5) d = 0.5;
                  if (d > 2.0) d = 2.0;
                  LUI_FieldSetDouble( field, d );
               }
               vis5d_get_vstreamslice(dwhichones[dyo], gtx->cur_vstream,
               &VStreamDensity, &row0, &col0, &row1, &col1);
               if (VStreamDensity!=d) {
                  VStreamDensity = d;
                  vis5d_set_vstreamslice(dwhichones[dyo], gtx->cur_vstream,
                  VStreamDensity, row0, col0, row1, col1);
                  vis5d_get_dtx_timestep(dwhichones[dyo], &curtime);
                  vis5d_get_dtx_numtimes(dwhichones[dyo], &numtimes);
                  for (time=0;time<numtimes;time++) {
                     vis5d_make_vstreamslice(dwhichones[dyo], time,
                     gtx->cur_vstream, time==curtime);
                  }
                  vis5d_signal_redraw(dwhichones[dyo], 1);
               }
            }
         }
      }
      else{
         if (d < 0.5 || d > 2.0) {
            if (d < 0.5) d = 0.5;
            if (d > 2.0) d = 2.0;
            LUI_FieldSetDouble( field, d );
         }
         vis5d_get_vstreamslice(index, gtx->cur_vstream, &VStreamDensity,
                                &row0, &col0, &row1, &col1);
         if (VStreamDensity!=d) {
            VStreamDensity = d;
            vis5d_set_vstreamslice(index, gtx->cur_vstream, VStreamDensity,
                                   row0, col0, row1, col1);
            for (time=0;time<numtimes;time++) {
               vis5d_make_vstreamslice(index, time, gtx->cur_vstream, time==curtime);
            }
            vis5d_signal_redraw(index, 1);
         }
      }
   }
   return 0;
}



/************************************/
/***  Variable Cloning Functions  ***/
/************************************/


/*
 * Create the "new variable" window.  This function is called when the
 * user clicks on the "NEWVAR" button.  We rebuild the window from scratch
 * each time to get the latest external functions.
 */
static void make_clone_window( int index )
{
   LUI_NEWBUTTON *b;
   int x, y, i, var, num, n;
   char name[1000];
   GuiContext gtx = get_gui_gtx(index);
   int NumVars;
   char RegularVarName[VIS5D_MAX_CONTEXTS*MAXVARS][50];
   int VarType[VIS5D_MAX_CONTEXTS*MAXVARS];
   int yo, spandex, icount = 0;
   char *gabels;

   for (yo=0; yo< gtx->how_many_regular_contexts; yo++){
      spandex = gtx->array_of_ctxs[yo];
      for (i=0;i<gtx->RegularNumVars[spandex];i++) {
         vis5d_get_ctx_var_name( spandex, i, RegularVarName[icount] );
         vis5d_get_var_type( spandex, i, &VarType[icount] );
         icount++;
      }
   }

   if (gtx->CloneWindow) {
      /* destroy current clone window before remaking it */
      LUI_DestroyWindow( gtx->CloneWindow );  /* all widgets destroyed too */
      gtx->CloneWindow = NULL;
   }

   gtx->CloneWindow = LUI_CreateWindow( LUI_RootWindow, 100, 100 );

   /* MJK 12.04.98 */
   set_window_decor (GuiDpy, gtx->CloneWindow);


 
   LUI_BorderWidth( 1 );
   XSync(GuiDpy, 0);

   LUI_NewLabelCreate( gtx->CloneWindow, LUI_LEFT, LUI_TOP, 300, 26,
                       "Select variable to clone:" );

   /* Make buttons for REGULAR and EXT_FUNC variables */
   num = 0;
   icount = 0;
   for (yo=0; yo< gtx->how_many_regular_contexts; yo++){
      spandex = gtx->array_of_ctxs[yo];
      for (i=0;i<gtx->RegularNumVars[spandex];i++) {
         if (VarType[icount]==VIS5D_REGULAR || VarType[icount]==VIS5D_EXT_FUNC) {
            if (num%3==0) {
               x = LUI_LEFT;
               y = LUI_NEXT_Y;
            }
            else {
               x = LUI_NEXT_X;
               y = LUI_SAME_Y;
            }
            if (gtx->how_many_regular_contexts <= 1){
               b = LUI_PushButtonCreate( gtx->CloneWindow, x,y, 80,22, RegularVarName[icount]);
            }
            else{
               int argh;
               gabels = return_var_and_index(RegularVarName[icount], spandex);
               for (argh=0; argh < 10; argh++){
                  RegularVarName[icount][argh] = gabels[argh];
               }
               b = LUI_PushButtonCreate( gtx->CloneWindow, x,y, 80,22, RegularVarName[icount]);
            }
            LUI_ButtonCallback( b, do_clone_cb );
            b->index = i;             /* ctx var */
            b->indexowner = spandex;  /* ctx index */
            b->context_index = index; /* dtx index */
            num++;
         }
         icount++;
      }
   }            

   /* Scan the userfuncs directory for analysis functions */
   if (gtx->funcpath[0]) {
      /* search directory specified by -funcpath <path> */
      n = find_analysis_functions( gtx->funcpath, FuncName[index] );
   }
   else {
      /* search default directory */
      n = find_analysis_functions( FUNCTION_PATH, FuncName[index] );
   }
   NumFuncs[index] = n;
   for (i=0;i<n;i++) {
      FuncType[index][i] = VIS5D_EXT_FUNC;
   }

   /* Look for variables computed by expressions too. */
   n = 0;
   n = find_expr_variables(index, FuncName[index]+NumFuncs[index]);
   for (i=0;i<n;i++) {
      int j = NumFuncs[index]+i;
      FuncType[index][j] = VIS5D_EXPRESSION;
   }
   NumFuncs[index] += n;

   if (NumFuncs[index]>0) {
      int cnt;
      char nmr[20];
      cnt = 0;
      /* Make button pad listing functions we can compute */
      LUI_NewLabelCreate( gtx->CloneWindow, LUI_LEFT, LUI_NEXT_Y, 300, 26,
                          "Or function to compute:" );
      for (i=0;i<NumFuncs[index];i++) {
         if (FuncType[index][i]==VIS5D_EXPRESSION) {
            int got;
            got = 0;
            while(FuncName[index][i][got] != 0){
               got++;
            }
            sprintf(nmr, "%d", FuncOwner[index][cnt]);
            FuncName[index][i][got] = '.';
            FuncName[index][i][got+1] = nmr[0];
            FuncName[index][i][got+2] = nmr[1];
            FuncName[index][i][got+3] = 0;

            /* Put = ... after the name so user knows it's a type-in expr. */
            sprintf(name, "%s = ...", FuncName[index][i] );
            cnt++;
         }
         else {
            strcpy( name, FuncName[index][i] );
         }
         if (i%3==0) {
            x = LUI_LEFT;
            y = LUI_NEXT_Y;
         }
         else {
            x = LUI_NEXT_X;
            y = LUI_SAME_Y;
         }
         b = LUI_PushButtonCreate( gtx->CloneWindow, x, y,  80, 22, name );
         LUI_ButtonCallback( b, do_clone_cb );
         LUI_ButtonIndex( b, -(i+1) );
         b->context_index = gtx->context_index;
         if( FuncType[index][i]==VIS5D_EXPRESSION){
            b->indexowner = FuncOwner[index][cnt-1];
         }
      }
   }

   LUI_NewLabelCreate( gtx->CloneWindow, LUI_LEFT, LUI_NEXT_Y,
                       250, 26, "Or:" );
   b = LUI_PushButtonCreate( gtx->CloneWindow, LUI_LEFT, LUI_NEXT_Y,
                             246, 22, "Make type-in expression..." );
   LUI_ButtonCallback( b, new_function_cb );
   b->context_index = index;
   LUI_NewLabelCreate( gtx->CloneWindow, LUI_LEFT, LUI_NEXT_Y,
                       250, 12, " " );
   b = LUI_PushButtonCreate( gtx->CloneWindow, 80, LUI_NEXT_Y,
                         90, 22, "Cancel" );
   LUI_ButtonCallback( b, do_clone_cb );
   LUI_ButtonIndex( b, 9999 );
   b->context_index = index;

   XMapWindow( GuiDpy, gtx->CloneWindow );

   gtx->CloneWidth = 252;
   gtx->CloneHeight = LUI_LayoutGet( LUI_NEXT_Y );
   LUI_MoveResizeWindow( gtx->CloneWindow, 450,400,
                         gtx->CloneWidth, gtx->CloneHeight );
}


/* called when the NEWVAR button is pressed */
static int newvar_cb( LUI_NEWBUTTON *pb )
{
   int numvars;
   int index = pb->context_index;
   GuiContext gtx = get_gui_gtx(index);

   vis5d_get_ctx_numvars( index, &numvars );

   if (pb->state == LUI_BUTTON_IN) {
      /* unmap clone window */
      XUnmapWindow( GuiDpy, gtx->CloneWindow );
      unmap_expr_window(index);
   }
   else {
      /* check if there's room for another variable */
      if (numvars>=MAXVARS) {
         alert( index, "Cannot make anymore clones." );
         return 0;
      }
      /* display clone window */
      make_clone_window(index);
   }
   return 0;
}





#ifdef LEAVEOUT
static delete_button_row( row )
int row;
{


}
#endif



/*
 * Update the labels on various widgets when the min and max values for
 * a variable may have changed.
 */
void update_var( int index, int newvar )
{
   char str[1000];
   char varname[10];
   float minval, maxval;
   unsigned int *colors;
   GuiContext gtx = get_gui_gtx(index);

   vis5d_get_ctx_var_name( index, newvar, varname );
   vis5d_get_ctx_var_range(index, newvar, &minval, &maxval);
   vis5d_reset_var_graphics(index, newvar);

   if (cb_graphic[cb_dindex]>=0 && cb_var[cb_dindex]>=0) {
      show_colorbar_window( index,cb_vindex[cb_dindex],cb_graphic[cb_dindex],cb_var[cb_dindex] ); 
   }
   if (cb_chvar[cb_dindex] >= 0){
      show_colorbar_window( index,cb_chvindex[cb_dindex],VIS5D_CHSLICE,cb_chvar[cb_dindex]);
   }
}




/*
 * This is called when the "New Function" button is pressed.
 */
static int new_function_cb( LUI_NEWBUTTON *pb, int state )
{
   int index = pb->context_index;
   GuiContext gtx = get_gui_gtx(index);
   XUnmapWindow( GuiDpy, gtx->CloneWindow );
   map_expr_window( index, "" );
   return 0;
}


/*
 * This is called when Enter is pressed in the expression type-in window.
 */
static int expr_cb( LUI_FIELD *field )
{
   int index = field->context_index;
   GuiContext gtx = get_gui_gtx(index);
   LUI_FieldGetText( gtx->expr_field, gtx->Expression );
   return 0;
}




/*
 * This is called when OK is selected on the expression type-in window.
 */
static int expr_ok_cb( LUI_NEWBUTTON *pb )
{
   char expr[1000], newname[100], mess[100];
   int var, recompute, varowner;
   int index = pb->context_index;
   GuiContext gtx = get_gui_gtx(index);

   /* WLH 21 Oct 98 */
   int numtrajs, *days = NULL, *times = NULL, *gps = NULL;
   float *lats = NULL, *lons = NULL, *alts = NULL;
   numtrajs = save_traj(index, &lats, &lons, &alts,
                        &days, &times, &gps);

   LUI_FieldGetText( gtx->expr_field, expr );

   /* evaluate */
   if (vis5d_make_expr_var(index, expr, newname, mess, &varowner, &var, &recompute) < 0 &&
       mess[0] != 0) {
      alert(index, mess);
      var = -1;
   }

   if (var>=0) {
      /* success */
      /* Update the GUI with a new row of buttons if we're not recomputing */
      /* an existing variable. */
      if (!recompute){
         make_another_gui( index, 0 );
      }
      init_var_colortable( gtx, varowner, var);
/* WLH 21 Oct 98
      recompute_graphics( index );
*/
/* WLH 11 Nov 98
      recompute_graphics( index, numtrajs, lats, lons, alts, days, times, gps);
*/
/* WLH 11 Nov 98 */
      recompute_graphics_var( index, numtrajs, lats, lons, alts, days,
                              times, gps, var);

      hide_widgets( index );
      show_widgets( index );
      unmap_expr_window(index);
      
   }
   else {

      /* WLH 21 Oct 98 */
      if (lats) free(lats);
      if (lons) free(lons);
      if (alts) free(alts);
      if (days) free(days);
      if (times) free(times);
      if (gps) free(gps);

      /* error- don't unmap expression window, user probably wants to edit */
   }
   return 0;
}


/*
 * This is called when cancel is selected on the expression type-in window.
 */
static int expr_cancel_cb( LUI_NEWBUTTON *pb )
{
   int index = pb->context_index;
   GuiContext gtx = get_gui_gtx(index);
   unmap_expr_window(index);
   return 0;
}



/* toggle pretty rendering on/off */
static int pretty_cb( LUI_BUTTON *pb )
{
   int PrettyFlag;
   int index = pb->context_index;
   GuiContext gtx = get_gui_gtx(index);

   /* only turn on pretty when not animating */
   if (gtx->GoTime==0) {
      PrettyFlag = vis5d_graphics_mode(index, VIS5D_PRETTY, VIS5D_TOGGLE);
      if (PrettyFlag) vis5d_signal_redraw(index, 1);
   }
   else {
      /* turn pretty off */
      vis5d_graphics_mode(index, VIS5D_PRETTY, VIS5D_OFF);
   }
   return 0;
}



/* toggle reverse video option */
static int reverse_cb( LUI_NEWBUTTON *pb )
{
/* MJK 12.04.98 begin */
   int          index = pb->context_index;
   GuiContext   gtx = get_gui_gtx (index);
   int          light_background;



   light_background = vis5d_graphics_mode (index, VIS5D_REVERSE, VIS5D_TOGGLE);

   group_event (gtx->group_index, 12, light_background);

   check_map_color (index);


   return 0;
}


/* toggle information display */
static void info_cb( int index)
{
   vis5d_graphics_mode(index, VIS5D_INFO, VIS5D_TOGGLE);
   vis5d_invalidate_dtx_frames(index);
}


/* called when SAVE PIC button is selected */
static int savepic_cb( LUI_NEWBUTTON *pb )
{
   XEvent pe;
   char filename[1000];
   int index = pb->context_index;
   GuiContext gtx = get_gui_gtx(index);

   verify_value = -1;
   XMapWindow( GuiDpy, gtx->SavePicWindow );

   while (verify_value < 0) {
      if (XPending(GuiDpy)) {
         XNextEvent(GuiDpy, &pe);
         LUI_EventDispatch(&pe);
      }
   }
   XUnmapWindow( GuiDpy, gtx->SavePicWindow );
   if (verify_value==1 && gtx->MouseMode != MM_SOUND) {
      int n = LUI_RadioGetCurrent( gtx->SavePicRadio );
      int format = gtx->SaveFormats[n];

      /* raise 3-D window and redraw */
      XRaiseWindow( GfxDpy, gtx->mainwin );
      XSync( GfxDpy, 0 );
      vis5d_draw_frame(index, gtx->GoTime);
      vis5d_swap_frame(index);
      XSync( GfxDpy, 0 );
      vis5d_draw_frame(index, gtx->GoTime);
      vis5d_swap_frame(index);
      XSync( GfxDpy, 0 );
      LUI_FieldGetText( gtx->SavePicField, filename );
      vis5d_save_window( filename, format );
      /* raise control panel window */
      XRaiseWindow( GuiDpy, gtx->CpWindow );
   }
   else if (verify_value==1 && gtx->MouseMode ==MM_SOUND) {
      int n = LUI_RadioGetCurrent( gtx->SavePicRadio );
      int format = gtx->SaveFormats[n];

      /* raise 3-D window and redraw */
      XRaiseWindow( GfxDpy, gtx->SoundCtrlWindow );
      XSync( GfxDpy, 0 );
      vis5d_draw_frame(index, gtx->GoTime);
      vis5d_swap_frame(index);
      XSync( GfxDpy, 0 );
      vis5d_draw_frame(index, gtx->GoTime);
      vis5d_swap_frame(index);
      XSync( GfxDpy, 0 );
      LUI_FieldGetText( gtx->SavePicField, filename );
      vis5d_save_snd_window( index, filename, format );
      /* raise control panel window */
      XRaiseWindow( GuiDpy, gtx->CpWindow );
   }
   return 0;
}




/* WLH 1 Oct 98
static void iconify( int index )
*/
void iconify( int index )
{
   GuiContext gtx = get_gui_gtx(index);
   XUnmapWindow( GfxDpy, BigWindow );

   XUnmapWindow( GuiDpy, gtx->CpWindow );
   /* MJK 10.16.98
   XUnmapWindow( GuiDpy, gtx->IsoWindow );
   XUnmapWindow( GuiDpy, gtx->HSliceWindow );
   XUnmapWindow( GuiDpy, gtx->VSliceWindow );
   if (gtx->TrajWindow) {
      XUnmapWindow( GuiDpy, gtx->TrajWindow );
   }
   vis5d_unmap_sndwindow( index);
   */
   /* MJK 12.04.98 */
   /*
   XUnmapWindow( GuiDpy, gtx->SoundCtrlWindow );
   */
   /*
   XUnmapWindow( GuiDpy, gtx->VerifyWindow );
   XUnmapWindow( GuiDpy, gtx->AlertWindow );
   if (gtx->CloneWindow) {
      XUnmapWindow( GuiDpy, gtx->CloneWindow );
   }
   if (gtx->WindWindow) {
      XUnmapWindow( GuiDpy, gtx->WindWindow );
   }
   XUnmapWindow( GuiDpy, gtx->ExprWindow );
   */
   hide_widgets( index );
}


void deiconify( int index )
{
   GuiContext gtx = get_gui_gtx(index);
   XMapWindow( GfxDpy, BigWindow ); 

   XMapWindow( GuiDpy, gtx->CpWindow );
   show_widgets( index );
   /* MJK 10.16.98
   if (gtx->cur_isosurf>-1)  XMapWindow( GuiDpy, gtx->IsoWindow );
   if (gtx->cur_hslice>-1)  XMapWindow( GuiDpy, gtx->HSliceWindow );
   if (gtx->cur_vslice>-1)  XMapWindow( GuiDpy, gtx->VSliceWindow );

   if (gtx->MouseMode==MM_TRAJ && gtx->TrajWindow) {
      XMapWindow( GuiDpy, gtx->TrajWindow );


      vis5d_map_sndwindow( index);
      vis5d_draw_sounding_only( index, 1);

   }
   if (gtx->MouseMode==MM_SOUND && gtx->SoundCtrlWindow) {
    */  /* MJK 12.04.98 */
/*
      vis5d_map_sndwindow( index);
      vis5d_draw_sounding_only( index, 1);
*//*

   }
   if ((gtx->cur_hwind>-1 || gtx->cur_vwind>-1 ||
        gtx->cur_hstream>-1 || gtx->cur_vstream>-1)
       && gtx->WindWindow) {
      XMapRaised( GuiDpy, gtx->WindWindow );

   }
   */
   XSync( GuiDpy, 0 );
   XSync( GfxDpy, 0 );
   vis5d_signal_redraw(index, 2);
}


/*
 * Print window image to PostScript printer.
 */
static void print_cb( int index )
{
   char *printer, msg[1000];
   GuiContext gtx = get_gui_gtx(index);

   printer = getenv( "PRINTER" );
   if (printer) {
      sprintf( msg, "Print to %s?", printer );
   }
   else {
      sprintf( msg, "Print to default printer?" );
   }

   if (verify(index, msg) && gtx->MouseMode != MM_SOUND) {
      vis5d_draw_frame(index, gtx->GoTime);
      vis5d_swap_frame(index);
      vis5d_print_window();
   }
   else if (verify(index, msg) && gtx->MouseMode == MM_SOUND) {
      vis5d_draw_frame(index, gtx->GoTime);
      vis5d_swap_frame(index);
      vis5d_print_snd_window( index );
   }
}



/*
 * Called when "INTERP" button is pressed.
 */
static int interpret_cb( LUI_NEWBUTTON *b )
{
   Display *dpy;
   Window win;
   int whichones[VIS5D_MAX_CONTEXTS], howmany;
   int rows, cols, yo, i;
   char *dpy_name;
   GuiContext gtx = get_gui_gtx(b->context_index);

   /* WLH 21 Oct 98 */
   int numtrajs[VIS5D_MAX_DPY_CONTEXTS], *days[VIS5D_MAX_DPY_CONTEXTS];
   int *times[VIS5D_MAX_DPY_CONTEXTS], *gps[VIS5D_MAX_DPY_CONTEXTS];
   float *lats[VIS5D_MAX_DPY_CONTEXTS], *lons[VIS5D_MAX_DPY_CONTEXTS];
   float *alts[VIS5D_MAX_DPY_CONTEXTS];
   for (i=0; i<VIS5D_MAX_DPY_CONTEXTS; i++) {
     numtrajs[i] = 0;
     days[i] = NULL;
     times[i] = NULL;
     gps[i] = NULL;
     lats[i] = NULL;
     lons[i] = NULL;
     alts[i] = NULL;
   }
   get_display_matrix( &rows, &cols);
   for (i = 0; i < rows*cols; i++) {
     numtrajs[i] = save_traj(i, &lats[i], &lons[i], &alts[i],
                             &days[i], &times[i], &gps[i]);
   }

   /*
    * Try to automatically raise the shell window
    */
   if (getenv("DISPLAY")) {
      dpy_name = getenv("DISPLAY");
      dpy = XOpenDisplay(dpy_name);
      if (dpy) {
         if (getenv("WINDOWID")) {
            win = atoi( getenv("WINDOWID") );
            if (win) {
               XRaiseWindow( dpy, win );
            }
         }
         XCloseDisplay(dpy);
      }
   }

   hide_widgets( Current_Display );
   if (interpret( b->context_index )==-1){
      get_display_matrix( &rows, &cols);
      for (yo = 0; yo < rows*cols; yo++){
         vis5d_create_display_context(yo);
         vis5d_get_num_of_ctxs_in_display( yo, &howmany, whichones);
         if (howmany < 1){
            if (the_gui_dpy_name[0]==0){
               make_nodata_gui( yo, NULL, NULL);
            }
            else{
               make_nodata_gui( yo, the_gui_dpy_name, NULL);
            }

            /* WLH 21 Oct 98 */
            if (lats[yo]) free(lats[yo]);
            if (lons[yo]) free(lons[yo]);
            if (alts[yo]) free(alts[yo]);
            if (days[yo]) free(days[yo]);
            if (times[yo]) free(times[yo]);
            if (gps[yo]) free(gps[yo]);

         }
         else{

            /* WLH 11 Nov 98 */
            if (redo_this_gui[yo]) {

              make_another_gui( yo, 1 );

              /* WLH 26 Oct 98 */
              vis5d_initialize_stuff(yo);

              /* WLH 21 Oct 98 */
              recompute_graphics( yo, numtrajs[yo], lats[yo], lons[yo],
                                  alts[yo], days[yo], times[yo], gps[yo]);

              vis5d_signal_redraw( yo, 1);

            /* WLH 11 Nov 98 */
            }
            else {
              if (lats[yo]) free(lats[yo]);
              if (lons[yo]) free(lons[yo]);
              if (alts[yo]) free(alts[yo]);
              if (days[yo]) free(days[yo]);
              if (times[yo]) free(times[yo]);
              if (gps[yo]) free(gps[yo]);
            }

         }
      }

      /* WLH 21 Oct 98 */
      for (i=rows*cols; i<VIS5D_MAX_DPY_CONTEXTS; i++) {
        if (lats[i]) free(lats[i]);
        if (lons[i]) free(lons[i]);
        if (alts[i]) free(alts[i]);
        if (days[i]) free(days[i]);
        if (times[i]) free(times[i]);
        if (gps[i]) free(gps[i]);
      }

   }
   /* WLH 21 Oct 98 */
   else {
     for (i=0; i<VIS5D_MAX_DPY_CONTEXTS; i++) {
       if (lats[i]) free(lats[i]);
       if (lons[i]) free(lons[i]);
       if (alts[i]) free(alts[i]);
       if (days[i]) free(days[i]);
       if (times[i]) free(times[i]);
       if (gps[i]) free(gps[i]);
     }
   }

   if (gtx->how_many_regular_contexts>0){
      update_button_states( b->context_index, 1);
/* MJK 3.9.99 */
      load_snd_widgets(b->context_index);
/* MJK 3.9.99 */
      read_snd_widgets(b->context_index);
/* MJK 3.9.99 */
      vis5d_draw_sounding_only( b->context_index, 1 );
      hide_widgets( b->context_index );
      show_widgets( b->context_index );
      map_all_windows( 1 );
      map_fake_windows( 1 );
      vis5d_signal_redraw( b->context_index, 1 );
   }
   else{
      int whichones[VIS5D_MAX_CONTEXTS], howmany;
      XUnmapWindow(GuiDpy, gtx_table[Current_Display]->CpWindow);
      Current_Display = -1;
      for (i = 0; i < VIS5D_MAX_DPY_CONTEXTS; i++){
         if (gtx_table[i]){
            vis5d_get_num_of_ctxs_in_display( i, &howmany, whichones);
            if (howmany>0){
               Current_Display = i; 
               break;
            }
         }
      }
      if (Current_Display == -1){
         /* make no_data_gui */
         if (the_gui_dpy_name[0]==0){
            make_nodata_gui( 0, NULL, NULL);
         }
         else{
            make_nodata_gui( 0, the_gui_dpy_name, NULL);
         }
         Current_Display = 0;
         gtx = gtx_table[0];
         XMapWindow(GuiDpy, gtx->CpWindow);
         LUI_MoveResizeWindow( gtx->CpWindow, gtx->cpx, gtx->cpy, CP_WIDTH, gtx->CpHeight );
         map_all_windows(0);
         map_fake_windows(0);
      }
      else{
         gtx = get_gui_gtx( Current_Display );
         XMapWindow(GuiDpy, gtx->CpWindow );
         LUI_MoveResizeWindow( gtx->CpWindow, gtx->cpx, gtx->cpy, CP_WIDTH, gtx->CpHeight );
         update_button_states( gtx->context_index, 1);
         hide_widgets( gtx->context_index );
         show_widgets( gtx->context_index );
         map_all_windows( 1 );
         map_fake_windows( 1 );
         vis5d_signal_redraw( gtx->context_index, 1 );
      }
   }
   return 0;
}



/*
 * Called when "OK" button in script file browser is pressed.
 */
static int run_script_cb( LUI_BROWSER *b, char *file )
{
  return run_script(Current_Display, file);
}

int run_script( int index, char *file )
{
   GuiContext gtx = get_gui_gtx(Current_Display);
   int rows, cols, yo, i;
   int whichones[VIS5D_MAX_CONTEXTS], howmany;
   int whatnow;

   /* WLH 21 Oct 98 */
   int numtrajs[VIS5D_MAX_DPY_CONTEXTS], *days[VIS5D_MAX_DPY_CONTEXTS];
   int *times[VIS5D_MAX_DPY_CONTEXTS], *gps[VIS5D_MAX_DPY_CONTEXTS];
   float *lats[VIS5D_MAX_DPY_CONTEXTS], *lons[VIS5D_MAX_DPY_CONTEXTS];
   float *alts[VIS5D_MAX_DPY_CONTEXTS];
   for (i=0; i<VIS5D_MAX_DPY_CONTEXTS; i++) {
     numtrajs[i] = 0;
     days[i] = NULL;
     times[i] = NULL;
     gps[i] = NULL;
     lats[i] = NULL;
     lons[i] = NULL;
     alts[i] = NULL;
   }
   get_display_matrix( &rows, &cols);
   for (i = 0; i < rows*cols; i++) {
     numtrajs[i] = save_traj(i, &lats[i], &lons[i], &alts[i],
                             &days[i], &times[i], &gps[i]);
   }

   /* MJK 11.19.98 */
   if (!off_screen_rendering){
      hide_widgets( Current_Display );
   }
   whatnow = execute_script( index, file );
   /* MJK 11.19.98 */   
   if (off_screen_rendering){
      return 0;
   }
   if (whatnow == 0){

     /* WLH 21 Oct 98 */
     for (i=0; i<VIS5D_MAX_DPY_CONTEXTS; i++) {
       if (lats[i]) free(lats[i]);
       if (lons[i]) free(lons[i]);
       if (alts[i]) free(alts[i]);
       if (days[i]) free(days[i]);
       if (times[i]) free(times[i]);
       if (gps[i]) free(gps[i]);
     }

      alert( index, "Script returned error status");
   }
   if (whatnow == -1){
      get_display_matrix( &rows, &cols);
      for (yo = 0; yo < rows*cols; yo++){
         vis5d_create_display_context(yo);
         vis5d_get_num_of_ctxs_in_display( yo, &howmany, whichones);
         if (howmany < 1){
            if (the_gui_dpy_name[0]==0){
               make_nodata_gui( yo, NULL, NULL);
            }
            else{
               make_nodata_gui( yo, the_gui_dpy_name, NULL);
            }

            /* WLH 21 Oct 98 */
            if (lats[yo]) free(lats[yo]);
            if (lons[yo]) free(lons[yo]);
            if (alts[yo]) free(alts[yo]);
            if (days[yo]) free(days[yo]);
            if (times[yo]) free(times[yo]);
            if (gps[yo]) free(gps[yo]);

         }
         else{

            /* WLH 11 Nov 98 */
            if (redo_this_gui[yo]) {

              make_another_gui( yo, 1 );

              /* WLH 26 Oct 98 */
              vis5d_initialize_stuff(yo);

              /* WLH 21 Oct 98 */
              recompute_graphics( yo, numtrajs[yo], lats[yo], lons[yo],
                                  alts[yo], days[yo], times[yo], gps[yo]);

              vis5d_signal_redraw( yo, 1);

            /* WLH 11 Nov 98 */
            }
            else {
              if (lats[yo]) free(lats[yo]);
              if (lons[yo]) free(lons[yo]);
              if (alts[yo]) free(alts[yo]);
              if (days[yo]) free(days[yo]);
              if (times[yo]) free(times[yo]);
              if (gps[yo]) free(gps[yo]);
            }

         }
      }

      /* WLH 21 Oct 98 */
      for (i=rows*cols; i<VIS5D_MAX_DPY_CONTEXTS; i++) {
        if (lats[i]) free(lats[i]);
        if (lons[i]) free(lons[i]);
        if (alts[i]) free(alts[i]);
        if (days[i]) free(days[i]);
        if (times[i]) free(times[i]);
        if (gps[i]) free(gps[i]);
      }

   }
 
   if (gtx->how_many_regular_contexts>0){   
      update_button_states( index, 0);
/* MJK 3.9.99 */
      load_snd_widgets(index);
/* MJK 3.9.99 */
      read_snd_widgets(index);
/* MJK 3.9.99 */
      vis5d_draw_sounding_only( index, 1 );
      hide_widgets( index );
      show_widgets( index );
      map_all_windows( 1 );
      map_fake_windows( 1 );
      vis5d_signal_redraw( index, 1 );
   }
   else{   
      XUnmapWindow(GuiDpy, gtx_table[Current_Display]->CpWindow);
      Current_Display = -1;
      for (i = 0; i < VIS5D_MAX_DPY_CONTEXTS; i++){
         if (gtx_table[i]){
            vis5d_get_num_of_ctxs_in_display( i, &howmany, whichones);
            if (howmany>0){
               Current_Display = i; 
               break;  
            }       
         }       
      }       
      if (Current_Display == -1){ 
         /* make no_data_gui */
         if (the_gui_dpy_name[0]==0){
            make_nodata_gui( 0, NULL, NULL);
         }       
         else{   
            make_nodata_gui( 0, the_gui_dpy_name, NULL);
         }       
         Current_Display = 0;
         gtx = gtx_table[0];
         XMapWindow(GuiDpy, gtx->CpWindow);
         LUI_MoveResizeWindow( gtx->CpWindow, gtx->cpx, gtx->cpy, CP_WIDTH, gtx->CpHeight );
         map_all_windows(0);
         map_fake_windows(0);
      }          
      else{   
         gtx = get_gui_gtx( Current_Display );
         XMapWindow(GuiDpy, gtx->CpWindow );
         LUI_MoveResizeWindow( gtx->CpWindow, gtx->cpx, gtx->cpy, CP_WIDTH, gtx->CpHeight );
         update_button_states( gtx->context_index, 1);
         hide_widgets( gtx->context_index );
         show_widgets( gtx->context_index );
         map_all_windows( 1 );
         map_fake_windows( 1 );
         vis5d_signal_redraw( gtx->context_index, 1 );
      }
   }
   return 0;
}

/* This is called after exiting from the interpreter and if there */
/* are new buttons that need to be added */
/* DONT USE THIS ANYMORE BUT KEEP IT HERE FOR OLE TIMES SAKE */
/* make_another_gui() does all of this stuff */
int make_new_buttons_after_interp( int index, int number_of_new_buttons )
{
   GuiContext gtx = get_gui_gtx(index);
   char varname[999];
   int varthing;
   int varnum;
   int yo;
   vis5d_get_ctx_numvars( index, &varnum);
   for (yo = 0; yo < number_of_new_buttons; yo++){
      varthing = varnum - number_of_new_buttons + yo;
      vis5d_get_ctx_var_name( index, varthing, varname ); 
      add_button_row(index, varname, varthing);
      init_var_colortable( gtx, index, varthing );
      vis5d_reset_var_graphics(index, varthing);
   }

   /* WLH 26 Oct 98 */
   vis5d_initialize_stuff(yo);

   /* WLH 21 Oct 98 */
   recompute_graphics( index, 0, NULL, NULL, NULL, NULL, NULL, NULL);

   vis5d_signal_redraw( index, 1 );
   return 0;
}    

/*
 * Called when "SCRIPT.." button is pressed.
 */
static int script_cb( void )
{
   static LUI_BROWSER *b = NULL;

   if (!b) {
      /* create the file browser once */
      LUI_BorderWidth( 2 );
      b = LUI_BrowserCreate( 300, 400 );
      LUI_BrowserCallback( b, run_script_cb );


   /* MJK 12.04.98 */
   set_window_decor (GuiDpy, b->window);

 
   }

   LUI_BrowserActivate( b, NULL );
   return 0;
}

/*
 * Called when the "IMPORT" button is pressed.
 */
static int import_cb( LUI_NEWBUTTON *b )
{
   int index = b->context_index;
   GuiContext gtx = get_gui_gtx(index);

   main_irun( GuiDpy, 0, NULL);
   return 0;
}


static int iimport_cb( LUI_NEWBUTTON *b )
{
   int index = b->context_index;
   GuiContext gtx = get_gui_gtx(index);

   run_ireg_importer( GuiDpy, 0, NULL);
   return 0;
}


/*
 * Called when the "DISPLAY" button is pressed.
 */
static int display_cb( LUI_NEWBUTTON *b )
{
   int index = b->context_index;
   GuiContext gtx = get_gui_gtx(index);

   show_display_widget( gtx );
   return 0;
}

static int uvw_cb( LUI_NEWBUTTON *b )
{
   int index = b->context_index;
   GuiContext gtx = get_gui_gtx(index);

   show_uvw_widget( gtx );
   return 0;
}


static int legend_cb( LUI_NEWBUTTON *b )
{
   int index = b->context_index;
   vis5d_graphics_mode( index, VIS5D_LEGENDS, VIS5D_TOGGLE );
}

/* BUG FIX MJK 8.8.98 */
/* added this function */
int copy_grp_colors( int gindex, int dindex, int vindex, int var, int type)
{
   unsigned int *table, *othertable;
   int dhowmany, dwhichones[VIS5D_MAX_CONTEXTS];
   int chowmany, cwhichones[VIS5D_MAX_CONTEXTS];
   int i, dyo, cyo, good;
   char aname[30];

   if (gindex > 0){
      if (type==VIS5D_TOPO){
         vis5d_get_num_of_dtxs_in_group( gindex, &dhowmany, dwhichones);
         vis5d_get_color_table_address( dindex, type, 0, -1, &table);
         for (dyo = 0; dyo < dhowmany; dyo++){
            vis5d_get_color_table_address( dwhichones[dyo], type, 0, -1,  &othertable);
            for(i=0; i<256; i++){
               othertable[i] = table[i];
            }
            vis5d_signal_redraw( dwhichones[dyo], 1);
         }
      }
      else{
         vis5d_get_num_of_dtxs_in_group( gindex, &dhowmany, dwhichones);
         vis5d_get_ctx_var_name( vindex, var, aname);
         vis5d_get_color_table_address( dindex, type, vindex, var, &table);
         for (dyo = 0; dyo < dhowmany; dyo++){
            vis5d_get_num_of_ctxs_in_display( dwhichones[dyo], &chowmany, cwhichones);
            for ( cyo = 0; cyo < chowmany; cyo++){
               good = vis5d_find_var(cwhichones[cyo], aname);
               if (good>-1 && dwhichones[dyo] != dindex){
                  vis5d_get_color_table_address( dwhichones[dyo], type,
                                   cwhichones[cyo], good, &othertable);
                  for(i=0; i<256; i++){
                     othertable[i] = table[i];
                  }
                  vis5d_signal_redraw( dwhichones[dyo], 1);
               }
            }
         }
      }
   }
   return 0;
}

/*
 * Called when color table in a colorbar widget is modified.
 */
static int colorbar_cb( LUI_COLORBAR *cb, int action )
{
   int index = cb->context_index;
   int vindex;
   float *p;
   GuiContext gtx;
   
   gtx = get_gui_gtx(index);
   cb_dindex = index;
   vindex = cb_vindex[cb_dindex];
   if (cb_graphic[cb_dindex]==VIS5D_TOPO) {
      if (action==LUI_RGB_RESET) {
         /* user changed table with mouse */
         vis5d_reset_topo_colors( index );
         LUI_ColorBarRedraw( cb );
         vis5d_signal_redraw( index, 1 );
         copy_grp_colors( gtx->group_index, index, 0 , 0, VIS5D_TOPO);
      }
      else if (action==LUI_RGB_CHANGE) {
         vis5d_signal_redraw( index, 1 );
         copy_grp_colors( gtx->group_index, index, 0 , 0, VIS5D_TOPO);      
      }
      else {
          /* ignore alpha events, not used */
      }
   }
   /* MJK 12.04.98 begin */
   else if ((cb == gtx->CHSliceColorbar) && (cb_chvar[cb_dindex] >= 0)) {
      vindex = cb_chvindex[cb_dindex];
      if (action==LUI_RGB_RESET) {
         unsigned int *table;
         vis5d_get_color_table_params (index, VIS5D_CHSLICE, vindex,
                                       cb_chvvar[cb_dindex], &p);
         vis5d_get_color_table_address (index, VIS5D_CHSLICE, vindex,
                                        cb_chvvar[cb_dindex], &table );
         vis5d_color_table_init_params( p, 1, 0 );
         vis5d_color_table_recompute( table, 256, p, 1, 0 );
         LUI_ColorBarRedraw( cb );
      }
      else if (action==LUI_ALPHA_RESET) {
         unsigned int *table;
         int opacity;
         vis5d_get_color_table_params(index, VIS5D_CHSLICE, vindex,
                                      cb_chvvar[cb_dindex], &p);
         vis5d_get_color_table_address( index, VIS5D_CHSLICE, vindex,
                                        cb_chvvar[cb_dindex], &table );
         vis5d_color_table_init_params( p, 0, 1 );
         vis5d_color_table_recompute( table, 256, p, 0, 1 );
         LUI_ColorBarRedraw( cb );
      }
      else if (action==LUI_RGB_SHAPE) {
         /* change shape of rgb curves */
         unsigned int *table;
         vis5d_get_color_table_params(index, VIS5D_CHSLICE, vindex,
                                      cb_chvvar[cb_dindex], &p);
         vis5d_get_color_table_address( index, VIS5D_CHSLICE, vindex,
                                        cb_chvvar[cb_dindex], &table );
         vis5d_color_table_recompute( table, 256, p, 1, 0 );
         LUI_ColorBarRedraw( cb );
      }
      else if (action==LUI_ALPHA_SHAPE) {
         /* change shape of alpha curve */
         unsigned int *table;
         vis5d_get_color_table_params(index, VIS5D_CHSLICE, vindex,
                                      cb_chvvar[cb_dindex], &p);
         vis5d_get_color_table_address( index, VIS5D_CHSLICE, vindex,
                                        cb_chvvar[cb_dindex], &table );
         vis5d_color_table_recompute( table, 256, p, 0, 1 );
         LUI_ColorBarRedraw( cb );
      }
      else if (action==LUI_RGB_CHANGE) {
         /* mouse used to draw curve */
      }
      else if (action==LUI_ALPHA_CHANGE) {
      }
      else {
         return 0;
      }
      vis5d_signal_redraw( index, 1 );
      /* BUG FIX MJK 8.8.98 */
      /* added this function */
      copy_grp_colors( gtx->group_index, index, vindex,
                       cb_chvvar[cb_dindex], VIS5D_CHSLICE);
   }
   /* MJK 12.04.98 end */



   else if (cb_graphic[cb_dindex]==VIS5D_CVSLICE) {
      if (action==LUI_RGB_RESET) {
         unsigned int *table;
         vis5d_get_color_table_params(index, VIS5D_CVSLICE, vindex, cb_vvar[cb_dindex], &p);
         vis5d_get_color_table_address( index, VIS5D_CVSLICE, vindex, cb_vvar[cb_dindex], &table );
         vis5d_color_table_init_params( p, 1, 0 );
         vis5d_color_table_recompute( table, 256, p, 1, 0 );
         vis5d_signal_redraw( index, 1 );
         LUI_ColorBarRedraw( cb );
         /* BUG FIX MJK 8.8.98 */
         /* added this function */
         copy_grp_colors( gtx->group_index, index, vindex, cb_vvar[cb_dindex], VIS5D_CVSLICE);
      }
      else if (action==LUI_ALPHA_RESET) {
         unsigned int *table;
         int opacity;
         vis5d_get_color_table_params(index, VIS5D_CVSLICE, vindex, cb_vvar[cb_dindex], &p);
         vis5d_get_color_table_address( index, VIS5D_CVSLICE, vindex, cb_vvar[cb_dindex], &table );
         vis5d_color_table_init_params( p, 0, 1 );
         vis5d_color_table_recompute( table, 256, p, 0, 1 );
         LUI_ColorBarRedraw( cb );
         vis5d_signal_redraw( index, 1 );
         /* BUG FIX MJK 8.8.98 */
         /* added this function */
         copy_grp_colors( gtx->group_index, index, vindex, cb_vvar[cb_dindex], VIS5D_CVSLICE);
      }
      else if (action==LUI_RGB_SHAPE) {
         /* change shape of rgb curves */
         unsigned int *table;
         vis5d_get_color_table_params(index, VIS5D_CVSLICE, vindex, cb_vvar[cb_dindex], &p);
         vis5d_get_color_table_address( index, VIS5D_CVSLICE, vindex, cb_vvar[cb_dindex], &table );
         vis5d_color_table_recompute( table, 256, p, 1, 0 );
         LUI_ColorBarRedraw( cb );
         vis5d_signal_redraw( index, 1 );
         /* BUG FIX MJK 8.8.98 */
         /* added this function */
         copy_grp_colors( gtx->group_index, index, vindex, cb_vvar[cb_dindex], VIS5D_CVSLICE);
      }
      else if (action==LUI_ALPHA_SHAPE) {
         /* change shape of alpha curve */
         unsigned int *table;
         vis5d_get_color_table_params(index, VIS5D_CVSLICE, vindex, cb_vvar[cb_dindex], &p);
         vis5d_get_color_table_address( index, VIS5D_CVSLICE, vindex, cb_vvar[cb_dindex], &table );
         vis5d_color_table_recompute( table, 256, p, 0, 1 );
         LUI_ColorBarRedraw( cb );
         vis5d_signal_redraw( index, 1 );
         /* BUG FIX MJK 8.8.98 */
         /* added this function */
         copy_grp_colors( gtx->group_index, index, vindex, cb_vvar[cb_dindex], VIS5D_CVSLICE);
      }
      else if (action==LUI_RGB_CHANGE) {
         vis5d_signal_redraw( index, 1 );
         /* BUG FIX MJK 8.8.98 */
         /* added this function */
         copy_grp_colors( gtx->group_index, index, vindex, cb_vvar[cb_dindex], VIS5D_CVSLICE);
      }
      else if (action==LUI_ALPHA_CHANGE) {
         vis5d_signal_redraw( index, 1 );
         /* BUG FIX MJK 8.8.98 */
         /* added this function */
         copy_grp_colors( gtx->group_index, index, vindex, cb_vvar[cb_dindex], VIS5D_CVSLICE);
      }
   }
   else if (cb_graphic[cb_dindex]==VIS5D_VOLUME) {
      if (action==LUI_RGB_RESET) {
         unsigned int *table;
         vis5d_get_color_table_params(index, VIS5D_VOLUME, vindex, cb_vvar[cb_dindex], &p);
         vis5d_get_color_table_address( index, VIS5D_VOLUME, vindex, cb_vvar[cb_dindex], &table );
         vis5d_color_table_init_params( p, 1, 0 );
         vis5d_color_table_recompute( table, 256, p, 1, 0);
         vis5d_signal_redraw( index, 1 );
         LUI_ColorBarRedraw( cb );
         /* BUG FIX MJK 8.8.98 */
         /* added this function */
         copy_grp_colors( gtx->group_index, index, vindex, cb_vvar[cb_dindex], VIS5D_VOLUME);
      }
      else if (action==LUI_ALPHA_RESET) {
         unsigned int *table;
         vis5d_get_color_table_params(index, VIS5D_VOLUME, vindex, cb_vvar[cb_dindex], &p);
         vis5d_get_color_table_address( index, VIS5D_VOLUME, vindex, cb_vvar[cb_dindex], &table );
         vis5d_color_table_set_alpha( p, -1 );
         vis5d_color_table_init_params( p, 0, 1 );
         vis5d_color_table_recompute( table, 256, p, 0, 1);
         LUI_ColorBarRedraw( cb );
         vis5d_signal_redraw( index, 1 );
         /* BUG FIX MJK 8.8.98 */
         /* added this function */
         copy_grp_colors( gtx->group_index, index, vindex, cb_vvar[cb_dindex], VIS5D_VOLUME);
      }
      else if (action==LUI_RGB_SHAPE) {
         /* change shape of rgb curves */
         unsigned int *table;
         vis5d_get_color_table_params(index, VIS5D_VOLUME, vindex, cb_vvar[cb_dindex], &p);
         vis5d_get_color_table_address( index, VIS5D_VOLUME, vindex, cb_vvar[cb_dindex], &table );
         vis5d_color_table_recompute( table, 256, p, 1, 0);
         LUI_ColorBarRedraw( cb );
         vis5d_signal_redraw( index, 1 );
         /* BUG FIX MJK 8.8.98 */
         /* added this function */
         copy_grp_colors( gtx->group_index, index, vindex, cb_vvar[cb_dindex], VIS5D_VOLUME);
      }
      else if (action==LUI_ALPHA_SHAPE) {
         /* change shape of alpha curve */
         unsigned int *table;
         vis5d_get_color_table_params(index, VIS5D_VOLUME, vindex, cb_vvar[cb_dindex], &p);
         vis5d_get_color_table_address( index, VIS5D_VOLUME, vindex, cb_vvar[cb_dindex], &table );
         vis5d_color_table_recompute( table, 256, p, 0, 1);
         LUI_ColorBarRedraw( cb );
         vis5d_signal_redraw( index, 1 );
         /* BUG FIX MJK 8.8.98 */
         /* added this function */
         copy_grp_colors( gtx->group_index, index, vindex, cb_vvar[cb_dindex], VIS5D_VOLUME);
      }
      else if (action==LUI_RGB_CHANGE) {
         vis5d_signal_redraw( index, 1 );
         /* BUG FIX MJK 8.8.98 */
         /* added this function */
         copy_grp_colors( gtx->group_index, index, vindex, cb_vvar[cb_dindex], VIS5D_VOLUME);
      }
      else if (action==LUI_ALPHA_CHANGE) {
         /* TODO: update opacity */
         vis5d_signal_redraw( index, 1 );
         /* BUG FIX MJK 8.8.98 */
         /* added this function */
         copy_grp_colors( gtx->group_index, index, vindex, cb_vvar[cb_dindex], VIS5D_VOLUME);
      }

   }

   return 0;
}



/*** These functions are called when the F1, F2, F3, etc. function ***/
/*** keys are called.  You can put whatever you want in them to    ***/
/*** make macros, shortcuts, etc.                                  ***/

static void func1( int index )
{
   GuiContext gtx = get_gui_gtx(index);

   if (gtx->widget_enable) {
      gtx->widget_enable = 0;
      /* raise 3-D window */
      XRaiseWindow( GfxDpy, gtx->mainwin );
   }
   else {
      gtx->widget_enable = 1;
      /* raise control panel window */
      XRaiseWindow( GuiDpy, gtx->CpWindow );
   }
}

static void func2( int index )
{
   /* toggle info display */
   info_cb(index);
   vis5d_invalidate_dtx_frames(index);
}

static void func3( int index )
{
   GuiContext gtx = get_gui_gtx(index);
   gtx->GoTime = (gtx->GoTime == 0) ? 1 : 0; /* toggle animation */
   return;
}

static void func4( int index )
{
}

static void func5( int index )
{
}

static void func6( int index )
{
}

static void func7( int index )
{
}

static void func8( int index )
{
}

static void func9( int index )
{
}
static void reset_all_keys_but( int index )
{
   if (index!=1) key1status = 0;
   if (index!=2) key2status = 0;
   if (index!=3) key3status = 0;
   if (index!=4) key4status = 0;
   if (index!=5) key5status = 0;
   if (index!=6) key6status = 0;
   if (index!=7) key7status = 0;
   if (index!=8) key8status = 0;
   if (index!=9) key9status = 0;
}

void unmap_gtx_CpWindow( int index )
{

   if (gtx_table[index] != NULL){
      XUnmapWindow(GuiDpy, gtx_table[index]->CpWindow);
   }
}

/**********************************************************************/
/******                   Group Handling                          *****/
/**********************************************************************/
/*
    Input gindex - the group index

          type 1 = Animate or GoTime flag
             status 0 = stop animateing GoTime=0
             status 1 = animate forward in time GoTime=1
             status 2 = animate backward in time GoTime=-1

          type 2 = Step time 
             status 0 = step forward
             status 1 = set time to zero
             status 2 = step backward
        
          type 3 = set CurrentVolume
             status # = set CurrentVolume to that var number
          type 4 = set CurrentVolumeOwner
             status # = set CurrentVolOwner to that ctx index number
          
          type 5 = vis5d_signal_redraw
             status # = number of times redraw value
    
          type 6 = topo flag
             status 0 = turn topo & topo button off
             status 1 = turn topo & topo button on
          type 7 = VIS5D_MAP
             status 0 = turn on
             status 1 = turn off
          type 8 = VIS5D_BOX
             status 0 = turn on
             status 1 = turn off
          type 9 = VIS5D_CLOCK
             status 0 = turn on
             status 1 = turn off
          type 10 = VIS5D_GRID_COORDS
             status 0 = turn on
             status 1 = turn off
          type 11 = VIS5D_CONTOUR_NUMBERS
             status 0 = turn on
             status 1 = turn off
          type 12 = VIS5D_REVERSE
             status 0 = turn on
             status 1 = turn off
          type 13 = VIS5D_PERSPECTIVE
             status 0 = turn on
             status 1 = turn off
          type 14 = MouseModes
             status 1 = MM_NORMAL
             status 2 = MM_TRAJ
             status 3 = MM_LABEL 
             status 4 = MM_SLICE
             status 5 = MM_PROBE
         type 15 + VIS5D_TRAJ_SETS-1 = = traj buttons
             status 0 = turn off;
             status 1 = turn on;
         type 100 = TrajRibbonFlag
             status 0 = off;
             status 1 = on;
         type 101 = unmap sounding window
         type 102 = send return if in LABEL mode so
                    as to get rid of that annoying
                    cursor.
*/                  


/* HERE */
static int group_event( int gindex, int type, int status)
{
   int yo;
   int index;
   int time, numtimes;
   int got_one;

   got_one = 0;
   if (gindex > 0){
      for (index=0; index<VIS5D_MAX_DPY_CONTEXTS; index++) {
         if (gtx_table[index] && gtx_table[index]->group_index==gindex) {
            if (type==1){
               if (status==0){
                  if (!got_one){
                     vis5d_get_grp_timestep(gtx_table[index]->group_index, &time);
                     got_one = 1;
                  }
                  else {
                     vis5d_set_grp_timestep(gtx_table[index]->group_index, time);
                                              
                  }
                  gtx_table[index]->GoTime = 0;
               }
               else if (status == 1){
                  gtx_table[index]->GoTime = 1;
               }
               else if (status == 2){
                  gtx_table[index]->GoTime = -1;
               }
            }
            else if (type==2){
               if (!got_one){
                  got_one = 1;
                  vis5d_get_grp_numtimes( gtx_table[index]->group_index, &numtimes );
                  vis5d_get_grp_timestep( gtx_table[index]->group_index, &time );
                  if (status==0){
                     if (time==numtimes-1){
                        time = 0;
                     }
                     else{
                        time++;
                     }
                  }
                  else if (status==1){
                     time = 0;
                  }
                  else if (status==2){
                     if (time==0){
                        time = numtimes-1;
                     }
                     else{
                        time--;
                     }
                  }
                  vis5d_set_grp_timestep(gtx_table[index]->group_index,
                                                  time);
                  vis5d_signal_group_redraw( gtx_table[index]->group_index, 1);
               } 
            }      
            else if (type==3){
               gtx_table[index]->CurrentVolume = status;
            }
            else if (type==4){
               gtx_table[index]->CurrentVolumeOwner = status;
            }
            else if (type==5){
               vis5d_signal_redraw(index, status);
            }
            else if (type==6){
               if(status==0){
                  vis5d_graphics_mode(index, VIS5D_TOPO, VIS5D_OFF);
                  gtx_table[index]->topoBUTTON->state = 0;
               }
               if(status==1){
                  vis5d_graphics_mode(index, VIS5D_TOPO, VIS5D_ON);
                  gtx_table[index]->topoBUTTON->state = 1;
               }
            }
            else if (type==7){
               if(status==0){
                  vis5d_graphics_mode(index, VIS5D_MAP, VIS5D_OFF);
                  gtx_table[index]->map_button->state = 0;
               }
               if(status==1){
                  vis5d_graphics_mode(index, VIS5D_MAP, VIS5D_ON);
                  gtx_table[index]->map_button->state = 1;
               }
            }
            else if (type==8){
               if(status==0){
                  vis5d_graphics_mode(index, VIS5D_BOX, VIS5D_OFF);
                  gtx_table[index]->boxBUTTON->state = 0;
                  vis5d_invalidate_dtx_frames(index);
               }
               if(status==1){
                  vis5d_graphics_mode(index, VIS5D_BOX, VIS5D_ON);
                  gtx_table[index]->boxBUTTON->state = 1;
                  vis5d_invalidate_dtx_frames(index);
               }
            }
            else if (type==9){
               if(status==0){
                  vis5d_graphics_mode(index, VIS5D_CLOCK, VIS5D_OFF);
                  gtx_table[index]->clockBUTTON->state = 0;
                  vis5d_invalidate_dtx_frames(index);
               }
               if(status==1){
                  vis5d_graphics_mode(index, VIS5D_CLOCK, VIS5D_ON);
                  gtx_table[index]->clockBUTTON->state = 1;
                  vis5d_invalidate_dtx_frames(index);
               }
            }
            else if (type==10){
               if(status==0){
                  vis5d_graphics_mode(index, VIS5D_GRID_COORDS, VIS5D_OFF);
                  gtx_table[index]->gridBUTTON->state = 0;
               }
               if(status==1){
                  vis5d_graphics_mode(index, VIS5D_GRID_COORDS, VIS5D_ON);
                  gtx_table[index]->gridBUTTON->state = 1;
               }
            }
            else if (type==11){
               if(status==0){
                  vis5d_graphics_mode(index, VIS5D_CONTOUR_NUMBERS, VIS5D_OFF);
                  gtx_table[index]->contBUTTON->state = 0;
               }
               if(status==1){
                  vis5d_graphics_mode(index, VIS5D_CONTOUR_NUMBERS, VIS5D_ON);
                  gtx_table[index]->contBUTTON->state = 1;
               }
            }
            else if (type==12){
               if(status==0){
                  vis5d_graphics_mode(index, VIS5D_REVERSE, VIS5D_OFF);
                  gtx_table[index]->reverseBUTTON->state = 0;
                  vis5d_signal_redraw(index, 2);
                  vis5d_invalidate_dtx_frames( index );
               }
               if(status==1){
                  vis5d_graphics_mode(index, VIS5D_REVERSE, VIS5D_ON);
                  gtx_table[index]->reverseBUTTON->state = 1;
                  vis5d_signal_redraw(index, 2);
                  vis5d_invalidate_dtx_frames( index );
               }
            }
            else if (type==13){
               if(status==0){
                  vis5d_graphics_mode(index, VIS5D_PERSPECTIVE, VIS5D_OFF);
                  gtx_table[index]->perspec_button->state = 0;
               }
               if(status==1){
                  vis5d_graphics_mode(index, VIS5D_PERSPECTIVE, VIS5D_ON);
                  gtx_table[index]->perspec_button->state = 1;
               }
            }
            else if (type==14){
               if (status==MM_NORMAL){
                  gtx_table[index]->MouseMode = MM_NORMAL;
                  LUI_LabelChangeText(gtx_table[index]->ModeInfoLabel, 6, ModeInfo1 );
                  vis5d_graphics_mode(index, VIS5D_CURSOR, VIS5D_OFF);
                  vis5d_graphics_mode(index, VIS5D_PROBE, VIS5D_OFF);
                  vis5d_graphics_mode(index, VIS5D_SOUND, VIS5D_OFF);
                  vis5d_graphics_mode(index, VIS5D_CLIP, VIS5D_OFF);
                  vis5d_invalidate_dtx_frames(index);
                  LUI_ButtonState(gtx_table[index]->normalBUTTON, 1);
                  LUI_ButtonState(gtx_table[index]->trajectoryBUTTON,0 );
                  LUI_ButtonState(gtx_table[index]->sliceBUTTON, 0);
                  LUI_ButtonState(gtx_table[index]->labelBUTTON, 0);
                  LUI_ButtonState(gtx_table[index]->probeBUTTON, 0);
                  LUI_ButtonState(gtx_table[index]->soundingBUTTON, 0);
                  LUI_ButtonState(gtx_table[index]->clippingBUTTON, 0);
               }
               if (status==MM_TRAJ){ 
                  gtx_table[index]->MouseMode = MM_TRAJ;
                  LUI_LabelChangeText(gtx_table[index]->ModeInfoLabel, 6, ModeInfo2 );
                  vis5d_graphics_mode(index, VIS5D_CURSOR, VIS5D_ON);
                  vis5d_graphics_mode(index, VIS5D_PROBE, VIS5D_OFF);
                  vis5d_graphics_mode(index, VIS5D_CLIP, VIS5D_OFF);
                  vis5d_graphics_mode(index, VIS5D_SOUND, VIS5D_OFF);
                  vis5d_invalidate_dtx_frames(index);
                  LUI_ButtonState(gtx_table[index]->normalBUTTON, 0);
                  LUI_ButtonState(gtx_table[index]->trajectoryBUTTON,1 );
                  LUI_ButtonState(gtx_table[index]->sliceBUTTON,0 );
                  LUI_ButtonState(gtx_table[index]->labelBUTTON,0 );
                  LUI_ButtonState(gtx_table[index]->probeBUTTON,0 );
                  LUI_ButtonState(gtx_table[index]->soundingBUTTON,0 );
                  LUI_ButtonState(gtx_table[index]->clippingBUTTON, 0);
               }
               if (status==MM_SLICE){
                  gtx_table[index]->MouseMode = MM_SLICE;
                  LUI_LabelChangeText(gtx_table[index]->ModeInfoLabel, 6, ModeInfo3 );
                  vis5d_graphics_mode(index, VIS5D_CURSOR, VIS5D_OFF);
                  vis5d_graphics_mode(index, VIS5D_PROBE, VIS5D_OFF);
                  vis5d_graphics_mode(index, VIS5D_SOUND, VIS5D_OFF);
                  vis5d_graphics_mode(index, VIS5D_CLIP, VIS5D_OFF);
                  vis5d_invalidate_dtx_frames(index);
                  LUI_ButtonState(gtx_table[index]->normalBUTTON,0 );
                  LUI_ButtonState(gtx_table[index]->trajectoryBUTTON,0 );
                  LUI_ButtonState(gtx_table[index]->sliceBUTTON,1 );
                  LUI_ButtonState(gtx_table[index]->labelBUTTON,0 );
                  LUI_ButtonState(gtx_table[index]->probeBUTTON,0 );
                  LUI_ButtonState(gtx_table[index]->soundingBUTTON,0 );
                  LUI_ButtonState(gtx_table[index]->clippingBUTTON, 0);
               }
               if (status==MM_LABEL){
                  gtx_table[index]->MouseMode = MM_LABEL;
                  LUI_LabelChangeText(gtx_table[index]->ModeInfoLabel, 6, ModeInfo4 );
                  vis5d_graphics_mode(index, VIS5D_CURSOR, VIS5D_OFF);
                  vis5d_graphics_mode(index, VIS5D_PROBE, VIS5D_OFF);
                  vis5d_graphics_mode(index, VIS5D_SOUND, VIS5D_OFF);
                  vis5d_graphics_mode(index, VIS5D_CLIP, VIS5D_OFF);
                  vis5d_invalidate_dtx_frames(index);
                  LUI_ButtonState(gtx_table[index]->normalBUTTON,0 );
                  LUI_ButtonState(gtx_table[index]->trajectoryBUTTON,0 );
                  LUI_ButtonState(gtx_table[index]->sliceBUTTON,0 );
                  LUI_ButtonState(gtx_table[index]->labelBUTTON,1 );
                  LUI_ButtonState(gtx_table[index]->probeBUTTON,0 );
                  LUI_ButtonState(gtx_table[index]->soundingBUTTON,0 );
                  LUI_ButtonState(gtx_table[index]->clippingBUTTON, 0);
               }
               if (status==MM_PROBE){
                  gtx_table[index]->MouseMode = MM_PROBE;
                  LUI_LabelChangeText(gtx_table[index]->ModeInfoLabel, 6, ModeInfo5 );
                  vis5d_graphics_mode(index, VIS5D_CURSOR, VIS5D_ON);
                  vis5d_graphics_mode(index, VIS5D_PROBE, VIS5D_ON);
                  vis5d_graphics_mode(index, VIS5D_SOUND, VIS5D_OFF);
                  vis5d_graphics_mode(index, VIS5D_CLIP, VIS5D_OFF);
                  vis5d_invalidate_dtx_frames(index);
                  LUI_ButtonState(gtx_table[index]->normalBUTTON,0 );
                  LUI_ButtonState(gtx_table[index]->trajectoryBUTTON,0 );
                  LUI_ButtonState(gtx_table[index]->sliceBUTTON,0 );
                  LUI_ButtonState(gtx_table[index]->labelBUTTON,0 );
                  LUI_ButtonState(gtx_table[index]->probeBUTTON,1 );
                  LUI_ButtonState(gtx_table[index]->clippingBUTTON, 0);
                  LUI_ButtonState(gtx_table[index]->soundingBUTTON,0 );
               }
               if (status==MM_SOUND){
                  XWindowAttributes winatts;
                  gtx_table[index]->MouseMode = MM_SOUND;
                  LUI_LabelChangeText(gtx_table[index]->ModeInfoLabel, 6, ModeInfo5 );
                  vis5d_graphics_mode(index, VIS5D_CURSOR, VIS5D_ON);
                  vis5d_graphics_mode(index, VIS5D_PROBE, VIS5D_OFF);
                  vis5d_graphics_mode(index, VIS5D_SOUND, VIS5D_ON);
                  vis5d_graphics_mode(index, VIS5D_CLIP, VIS5D_OFF);
                  vis5d_invalidate_dtx_frames(index);
                  LUI_ButtonState(gtx_table[index]->normalBUTTON,0 );
                  LUI_ButtonState(gtx_table[index]->trajectoryBUTTON,0 );
                  LUI_ButtonState(gtx_table[index]->sliceBUTTON,0 );
                  LUI_ButtonState(gtx_table[index]->labelBUTTON,0 );
                  LUI_ButtonState(gtx_table[index]->probeBUTTON,0 );
                  LUI_ButtonState(gtx_table[index]->soundingBUTTON,1 );
                  LUI_ButtonState(gtx_table[index]->clippingBUTTON, 0);
                  XMapWindow( GuiDpy, gtx_table[index]->SoundCtrlWindow );
                  vis5d_map_sndwindow( index);
                  vis5d_draw_sounding_only( index, 1);
                  vis5d_draw_sounding_only( index, 1);
                  XGetWindowAttributes( GuiDpy, gtx_table[index]->SoundCtrlWindow, &winatts);
                  if (gtx_table[index]->othersnddpy != 0){
                     vis5d_resize_sounding_window( index, winatts.width, winatts.height,
                                                   winatts.x, winatts.y);
                  }
               }
               if (status==MM_CLIP){
                  gtx_table[index]->MouseMode = MM_CLIP;
                  LUI_LabelChangeText(gtx_table[index]->ModeInfoLabel, 6, ModeInfo6 );
                  vis5d_graphics_mode(index, VIS5D_CURSOR, VIS5D_OFF);
                  vis5d_graphics_mode(index, VIS5D_PROBE, VIS5D_OFF);
                  vis5d_graphics_mode(index, VIS5D_SOUND, VIS5D_OFF);
                  vis5d_graphics_mode(index, VIS5D_CLIP, VIS5D_ON);
                  vis5d_invalidate_dtx_frames(index);
                  LUI_ButtonState(gtx_table[index]->normalBUTTON,0 );
                  LUI_ButtonState(gtx_table[index]->trajectoryBUTTON,0 );
                  LUI_ButtonState(gtx_table[index]->sliceBUTTON,0 );
                  LUI_ButtonState(gtx_table[index]->labelBUTTON,0 );
                  LUI_ButtonState(gtx_table[index]->probeBUTTON,0 );
                  LUI_ButtonState(gtx_table[index]->clippingBUTTON, 1);
                  LUI_ButtonState(gtx_table[index]->soundingBUTTON, 0);
               }
            }
            else if (type >= 15 && type <= 15+VIS5D_TRAJ_SETS-1){
               int trajnum = type -  15;
               LUI_ButtonState( gtx_table[index]->TrajButton[trajnum], status);
               vis5d_enable_graphics(gtx_table[index]->array_of_ctxs[0],
                                     VIS5D_TRAJ, trajnum, status);
            }
            else if (type == 100){
               LUI_ButtonSetState( gtx_table[index]->TrajRibbonButton, status );
            }
            else if (type == 101){
               if (gtx_table[index]->SoundCtrlWindow){
                 XUnmapWindow( GuiDpy, gtx_table[index]->SoundCtrlWindow);
               }
            }
            else if (type == 102){
               if (gtx_table[index]->MouseMode==MM_LABEL){
                  vis5d_edit_label( index, '\r' );
               }
            }
         }
      }
   }
}





/**********************************************************************/
/*****                   Event Handling                           *****/
/**********************************************************************/



/*
 * Handle an input event in the 3-D graphics window.
 * Input:  index - context index
 *         ev - the X event
 */
static int gfx_window_event( int index, XEvent *ev )
{
   int  current_x, current_y;
   float  anglex, angley, scale, transx, transy;
   MATRIX  temp;
   MATRIX  mat;
   int i, r;
   Window window;
   int WinWidth, WinHeight;
   int CurTime;
   float CursorX, CursorY, CursorZ;
   int Uvar, Vvar, Wvar, Uvar2, Vvar2, Wvar2, TrajU, TrajV, TrajW;
   int NumTimes;
   int whichones[VIS5D_MAX_CONTEXTS], howmany;
   int firstctxindex;
   GuiContext gtx;
   static int perspec;
   static float frontclip, zoom;
   int owner[9];
   int dhowmany, dwhichones[VIS5D_MAX_CONTEXTS];
   int chowmany, cwhichones[VIS5D_MAX_CONTEXTS];
   int times, dyo, cyo, good;
   static int oldwinheight = 0;
   static int oldwinwidth = 0;
   static int oldwinx = 0;
   static int oldwiny = 0;
   /* INDEX = display index */

   gtx = get_gui_gtx(index);
   vis5d_get_num_of_ctxs_in_display( index, &howmany, whichones);
   firstctxindex = whichones[0]; 
   vis5d_get_window( index, &window, &WinWidth, &WinHeight );
   vis5d_get_dtx_timestep(index, &CurTime);
   vis5d_get_cursor(index, &CursorX, &CursorY, &CursorZ);
   if (howmany >= 1){
      vis5d_get_wind_vars(index, &owner[0], &Uvar, &owner[1], &Vvar, 
                          &owner[2], &Wvar,
                          &owner[3], &Uvar2, &owner[4],  &Vvar2, 
                          &owner[5], &Wvar2, &owner[6] , &TrajU, 
                          &owner[7], &TrajV, &owner[8], &TrajW);
   }
   vis5d_get_dtx_numtimes( index, &NumTimes );


/* Iconification... */
#ifdef LEAVEOUT
   if (ev->type==UnmapNotify && keystatus == 0 
       && (ev->xany.window==get_win(gtx->context_index) || ev->xany.window==gtx->CpWindow)) {
      /* Go to sleep if iconified */
      iconify(index);
      do {
         if (XPending(GfxDpy))
           XNextEvent(GfxDpy, ev);
         else
           XNextEvent(GuiDpy, ev);
      } while (ev->type!=MapNotify);
      deiconify(index);
   }
#endif


   if (ev->xany.window==gtx->fakewin && keystatus == 0 &&
       ev->type != KeyPress ) {
      /* fudge the mouse coordinates in the event struct */
      if (ev->type==ButtonPress || ev->type==ButtonRelease) {
         ev->xbutton.x = ev->xbutton.x * WinWidth / gtx->fakewidth;
         ev->xbutton.y = ev->xbutton.y * WinHeight / gtx->fakeheight;
      }
      else if (ev->type==MotionNotify) {
         ev->xmotion.x = ev->xmotion.x * WinWidth / gtx->fakewidth;
         ev->xmotion.y = ev->xmotion.y * WinHeight / gtx->fakeheight;
         vis5d_set_pointer(index, ev->xmotion.x, ev->xmotion.y);
         vis5d_signal_redraw(index, 1);
      }
   }

   /* Check for mouse-mode-specific events first */
   if (gtx->MouseMode==MM_SLICE && slice_event(index, *ev, CurTime) 
       && keystatus == 0) {
      vis5d_graphics_mode(index, VIS5D_PRETTY, VIS5D_OFF);
      vis5d_invalidate_dtx_frames(index);
   }
   else if (gtx->MouseMode==MM_CLIP && slice_event(index, *ev, CurTime)
       && keystatus == 0) {
      vis5d_graphics_mode(index, VIS5D_PRETTY, VIS5D_OFF);
      vis5d_invalidate_dtx_frames(index);
   }
   else if (gtx->MouseMode==MM_CLIP && ev->type == ButtonPress &&
            ev->xbutton.button==Button2){
      vis5d_set_clip_mode( gtx->context_index, gtx->cur_clip, 0);
      if (gtx->cur_clip == 5){
         gtx->cur_clip = 0;
      }
      else{
         gtx->cur_clip += 1;
      }
      vis5d_set_clip_mode( gtx->context_index, gtx->cur_clip, 1);
   }
   else if (gtx->MouseMode==MM_LABEL && labeling_event(index, *ev) && keystatus == 0 ) {
      vis5d_graphics_mode(index, VIS5D_PRETTY, VIS5D_OFF);
      vis5d_invalidate_dtx_frames(index);
   }
   else if (gtx->MouseMode==MM_TRAJ && (r=cursor_event(index, *ev)) && keystatus == 0) {
      if (r==2) {
         /* convert cursor position from (x,y,z) to (r,c,l) */
         float cursor_r, cursor_c, cursor_l;
         float cursor_rP, cursor_cP, cursor_lP;
         float lat, lon, hgt;
         int numbtimes;
         vis5d_xyzPRIME_to_gridPRIME( index, CurTime, TrajU, CursorX, CursorY, CursorZ,
                            &cursor_r, &cursor_c, &cursor_l );
         if (gtx->GoTime) {
            if (gtx->group_index > 0){
               vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
               vis5d_gridPRIME_to_geo( index, 0, TrajU, cursor_r, cursor_c, cursor_l,
                                       &lat, &lon, &hgt);
               for (dyo = 0; dyo < dhowmany; dyo++){
                  vis5d_geo_to_gridPRIME( dwhichones[dyo], i, 0, lat, lon, hgt,
                                       &cursor_rP, &cursor_cP, &cursor_lP);
                  vis5d_get_dtx_numtimes( dwhichones[dyo], &numbtimes);
                  for (i=0; i< numbtimes; i++){
                     vis5d_make_traj( dwhichones[dyo], cursor_rP, cursor_cP, cursor_lP,
                                      i, gtx->cur_trajset );
                  }
               }
            }
            else{
               for (i=0;i<NumTimes;i++) {
                  vis5d_make_traj( index, cursor_r, cursor_c, cursor_l,
                                   i, gtx->cur_trajset );
               }
            }
         }
         else {
            /* one traj */
            if (gtx->group_index > 0){
               vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
               vis5d_gridPRIME_to_geo( index, 0, TrajU, cursor_r, cursor_c, cursor_l,
                                          &lat, &lon, &hgt);
               for (dyo = 0; dyo < dhowmany; dyo++){
                  vis5d_geo_to_gridPRIME( dwhichones[dyo], i, 0, lat, lon, hgt,               
                                       &cursor_rP, &cursor_cP, &cursor_lP);                  
                  vis5d_get_dtx_timestep( dwhichones[dyo], &numbtimes);
                  vis5d_make_traj( dwhichones[dyo], cursor_rP, cursor_cP, cursor_lP,
                                   numbtimes, gtx->cur_trajset );
               }
            }
            else{     
               vis5d_make_traj( index, cursor_r, cursor_c, cursor_l,
                                CurTime, gtx->cur_trajset );
            }
         }
      }
      if (gtx->group_index > 0 ){
         vis5d_invalidate_grp_frames( gtx->group_index );
         vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
         for (dyo = 0; dyo < dhowmany; dyo++){
            vis5d_graphics_mode(dwhichones[dyo], VIS5D_PRETTY, VIS5D_OFF);
         }
      }
      else{
         vis5d_graphics_mode(index, VIS5D_PRETTY, VIS5D_OFF);
         vis5d_invalidate_dtx_frames(index);
      }
   }
   else if (gtx->MouseMode==MM_PROBE && cursor_event(index, *ev)==1 && keystatus == 0) {
      if (gtx->group_index > 0 ){
         vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
         for (dyo = 0; dyo < dhowmany; dyo++){
            vis5d_graphics_mode(dwhichones[dyo],VIS5D_PRETTY, VIS5D_OFF);
            vis5d_signal_redraw(dwhichones[dyo], 1);
         }
      }
      else{
         vis5d_graphics_mode(index, VIS5D_PRETTY, VIS5D_OFF);
         vis5d_signal_redraw(index, 1);
      }
   } 
   else if (gtx->MouseMode==MM_SOUND && cursor_event(index, *ev)==1 && keystatus == 0) {
      if (gtx->group_index > 0 ){
         vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
         for (dyo = 0; dyo < dhowmany; dyo++){
            vis5d_graphics_mode(dwhichones[dyo], VIS5D_PRETTY, VIS5D_OFF);
            vis5d_signal_redraw(dwhichones[dyo], 1);
         }
      }
      else{
         vis5d_graphics_mode(index, VIS5D_PRETTY, VIS5D_OFF);
         vis5d_signal_redraw(index, 1); 
      }
   }
   /*** Button press ***/
   else if (ev->type == ButtonPress && keystatus == 0) {
      gtx->start_x = ev->xbutton.x;
      gtx->start_y = ev->xbutton.y;
      vis5d_get_matrix(index, gtx->ctm);
      vis5d_get_camera( index, &perspec, &frontclip, &zoom );

      if (ev->xbutton.button==Button1 && !gtx->p2 && !gtx->p3) {
         gtx->p1 = 1;
      }
      else if (ev->xbutton.button==Button2 && !gtx->p1 && !gtx->p3) {
         gtx->p2 = 1;
      }
      else if (ev->xbutton.button==Button3 && !gtx->p1 && !gtx->p2) {
         gtx->p3 = 1;
      }
      vis5d_graphics_mode(index, VIS5D_PRETTY, VIS5D_OFF);
      vis5d_invalidate_dtx_frames(index);
   } /* if ev->type==ButtonPress */

   /*** Button down and pointer moving ***/
   else if (ev->type == MotionNotify && keystatus == 0) {
      current_x = ev->xmotion.x;
      current_y = ev->xmotion.y;
      if (gtx->p1) {
         /* rotate */
         angley = (current_x-gtx->start_x) * 200.0 / (float) WinWidth;
         anglex = (current_y-gtx->start_y) * 200.0 / (float) WinHeight;
         make_matrix( anglex,angley,0.0, 1.0, 0.0,0.0,0.0, mat );
         mat_mul( temp, gtx->ctm, mat );
         if (gtx->group_index > 0){
            vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
            for (dyo = 0; dyo < dhowmany; dyo++){
               vis5d_set_matrix(dwhichones[dyo], temp);
            }
         }
         else{
            vis5d_set_matrix(index, temp);
         }
      }
      else if (gtx->p2) {
         /* clip */
         float zdelt = (gtx->start_x - current_x) / (float) WinWidth;
         float newfront = frontclip - zdelt;
         float scale = exp( (gtx->start_y-current_y) / (float) WinHeight );
         float newzoom = zoom * scale;
         if (gtx->group_index > 0){
            vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
            for (dyo = 0; dyo < dhowmany; dyo++){
               vis5d_set_camera( dwhichones[dyo], perspec, newfront, newzoom );
            }
         }
         else{
            vis5d_set_camera( index, perspec, newfront, newzoom );
         }
      }
      else if (gtx->p3) {
         /* translate */
         transx = (gtx->start_x - current_x) * -2.0 / (float) WinWidth;
         transy = (gtx->start_y - current_y) * 2.0 / (float) WinHeight;
         make_matrix( 0.0,0.0,0.0, 1.0, transx,transy,0.0, mat );
         mat_mul( temp, gtx->ctm, mat );
         if (gtx->group_index > 0){
            vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
            for (dyo = 0; dyo < dhowmany; dyo++){
               vis5d_set_matrix(dwhichones[dyo], temp);
            }
         }
         else{
            vis5d_set_matrix(index, temp);
         }
      }
   } /* if ev->type==MotionNotify */
      
   /*** Button release ***/
   else if (ev->type == ButtonRelease && keystatus == 0) {
      if (ev->xbutton.button == Button1) {
         gtx->p1 = 0;
      }
      else if (ev->xbutton.button == Button2) {
         gtx->p2 = 0;
      }
      else if (ev->xbutton.button == Button3) {
         gtx->p3 = 0;
      }
      vis5d_signal_redraw(index, 1);
   } /* if ev->type==ButtonRelease */
      
   /*** Key press ***/
   else if (ev->type == KeyPress) {
      KeySym keysym;
      XComposeStatus compose;
      char buff[20];
      XLookupString( &ev->xkey, buff, 1, &keysym, &compose );
      if (buff[0]=='p' || buff[0]=='P') {
         print_cb( index );
      }
      else if (buff[0]=='f' || buff[0]=='F') {
         gtx->AnimRate = gtx->AnimRate - 10;
         if (gtx->AnimRate < 1) gtx->AnimRate = 1;
      }
      else if (buff[0]=='s' || buff[0]=='S') {
         gtx->AnimRate = gtx->AnimRate + 10;
      }
      else if (buff[0]=='l'){
         gtx->size_of_logo = gtx->size_of_logo + 0.2;
         vis5d_set_logo_size( gtx->context_index, gtx->size_of_logo);
      }
      else if (buff[0]=='L'){
         gtx->size_of_logo = gtx->size_of_logo - 0.2; 
         vis5d_set_logo_size( gtx->context_index, gtx->size_of_logo);
      }  
      else if (gtx->MouseMode==MM_CLIP && 
               (buff[0]=='r' || buff[0]=='R')){
         int Nr, Nc, Nl, LowLev, MaxNlVar, WindNl, WindLow;

         vis5d_get_sizePRIME(gtx->context_index, &Nr, &Nc,
          &Nl, &LowLev,  &WindNl, &WindLow);
         vis5d_set_hclip( gtx->context_index, 0, Nl-1);
         vis5d_set_hclip( gtx->context_index, 1, 0);
         vis5d_set_vclip( gtx->context_index, 0, 0, 0, 0, Nc-1);
         vis5d_set_vclip( gtx->context_index, 1, Nr-1, 0, Nr-1, Nc-1);
         vis5d_set_vclip( gtx->context_index, 2, 0, 0, Nr-1, 0);
         vis5d_set_vclip( gtx->context_index, 3, 0, Nc-1, Nr-1, Nc-1);

         vis5d_graphics_mode(gtx->context_index, VIS5D_PRETTY, VIS5D_OFF);
         vis5d_invalidate_dtx_frames(gtx->context_index);
      }
      else if (keysym == XK_F1) func1(index);
      else if (keysym == XK_F2) func2(index);
      else if (keysym == XK_F3) func3(index);
      else if (keysym == XK_F4) func4(index);
      else if (keysym == XK_F5) func5(index);
      else if (keysym == XK_F6) func6(index);
      else if (keysym == XK_F7) func7(index);
      else if (keysym == XK_F8) func8(index);
      else if (keysym == XK_F9) func9(index);
      else if (keysym == XK_1){
         key1status = (key1status==0 && keystatus==0) ? 1 : 0;
         keystatus = key1status;
         currentkey = key1status ? 1 : 0;
         if (key1status == 1) reset_all_keys_but(1);
      } 
      else if (keysym == XK_2){
         key2status = (key2status==0 && keystatus==0) ? 1 : 0;
         keystatus = key2status;  
         currentkey = key2status ? 2 : 0;
         if (key2status == 1) reset_all_keys_but(2);
      }
      else if (keysym == XK_3){
         key3status = (key3status==0 && keystatus==0) ? 1 : 0;
         keystatus = key3status;  
         currentkey = key3status ? 3 : 0;
         if (key3status == 1) reset_all_keys_but(3);
      }
      else if (keysym == XK_4){
         key4status = (key4status==0 && keystatus==0) ? 1 : 0;
         keystatus = key4status;  
         currentkey = key4status ? 4 : 0;
         if (key4status == 1) reset_all_keys_but(4);
      }
      else if (keysym == XK_5){
         key5status = (key5status==0 && keystatus==0) ? 1 : 0;
         keystatus = key5status;  
         currentkey = key5status ?  5: 0;
         if (key5status == 1) reset_all_keys_but(5);
      }
      else if (keysym == XK_6){
         key6status = (key6status==0 && keystatus==0) ? 1 : 0;
         keystatus = key6status;  
         currentkey = key6status ? 6 : 0;
         if (key6status == 1) reset_all_keys_but(6);
      }
      else if (keysym == XK_7){
         key7status = (key7status==0 && keystatus==0) ? 1 : 0; 
         keystatus = key7status;  
         currentkey = key7status ? 7 : 0;
         if (key7status == 1) reset_all_keys_but(7);
      }
      else if (keysym == XK_8){
         key8status = (key8status==0 && keystatus==0) ? 1 : 0;
         keystatus = key8status;  
         currentkey = key8status ? 8 : 0;
         if (key8status == 1) reset_all_keys_but(8);
      }
      else if (keysym == XK_9){
         key9status = (key9status==0 && keystatus==0) ? 1 : 0;
         keystatus = key9status;  
         currentkey = key9status ? 9 : 0;
         if (key9status == 1) reset_all_keys_but(9);
      }
   }

   /*** Window size ***/
   else if (ev->type==ConfigureNotify ) {
      if (ev->xany.window==gtx->fakewin) {
         /* update fake window size */
         gtx->fakewidth = ev->xconfigure.width;
         gtx->fakeheight = ev->xconfigure.height;
      }
      if (ev->xany.window==gtx->SoundCtrlWindow){
         /* update sounding window */
         if (gtx->othersnddpy != 0){
            vis5d_resize_sounding_window( index, ev->xconfigure.width,
                                          ev->xconfigure.height,
                                          ev->xconfigure.x,
                                          ev->xconfigure.y);
         }
         else {
            vis5d_resize_sounding_window( index, ev->xconfigure.width,
                                          ev->xconfigure.height, 0, 95 );
         }
      }
      else if(ev->xany.window==BigWindow && (ev->type==ResizeRequest ||
              ev->type==22)){
         /* 3-D viewing window has changed size */
         if (ev->xconfigure.width != oldwinwidth || 
             ev->xconfigure.height!= oldwinheight ||
             ev->xconfigure.x != oldwinx ||
             ev->xconfigure.y != oldwiny){
            oldwinwidth = ev->xconfigure.width;
            oldwinheight = ev->xconfigure.height;
            oldwinx = ev->xconfigure.x;
            oldwiny = ev->xconfigure.y; 
            map_all_windows( 0 );
            vis5d_invalidate_dtx_frames(index);
         }
      }
      else if(ev->xany.window==GuiBigWin && (ev->type==ResizeRequest ||
              ev->type==22)){
         if (ev->xconfigure.x != 0 && ev->xconfigure.y !=0){
            if (ev->xconfigure.width != oldwinwidth || 
                ev->xconfigure.height!= oldwinheight ||
                ev->xconfigure.x != oldwinx ||
                ev->xconfigure.y != oldwiny){
               oldwinwidth = ev->xconfigure.width;
               oldwinheight = ev->xconfigure.height;
               oldwinx = ev->xconfigure.x;
               oldwiny = ev->xconfigure.y; 
               vis5d_moveresize_BIG_window( oldwinx, oldwiny,
                     oldwinwidth, oldwinheight);
               map_all_windows( 0 );
            }       
         }
         else if (ev->xconfigure.width != oldwinwidth || 
             ev->xconfigure.height!= oldwinheight){
            oldwinwidth = ev->xconfigure.width;
            oldwinheight = ev->xconfigure.height;
            vis5d_resize_BIG_window( oldwinwidth, oldwinheight);
            map_all_windows( 0 );
         }  
      }  
   }

   /*** Window visibility ***/

   /* MJK 12.04.98 begin */
   else if (ev->type==Expose || ev->type==VisibilityNotify
            || ev->type==GraphicsExpose) {
      XEvent    nextev;

       /* while (QLength (ev->xany.display)) WLH 24 May 2000 */
       while (XPending(ev->xany.display))
       {
          XPeekEvent (ev->xany.display, &nextev);
          if (nextev.xany.window != ev->xany.window) break;
          if ((nextev.type != Expose          ) &&
              (nextev.type != VisibilityNotify) &&
              (nextev.type != GraphicsExpose  )) break;
          XNextEvent (ev->xany.display, &nextev);
      }
      if (ev->xany.window == gtx->SoundCtrlWindow) {
         vis5d_draw_sounding_only (index, 1);
      }
      else if (ev->xany.window == gtx->mainwin) {
         vis5d_signal_redraw(index, 1);
      }
   }
   /* MJK 12.04.98 end */


   /*** Flush remaining motion events in event queue ***/
   /* while (QLength(GfxDpy) > 0) { WLH 24 May 2000 */
   while (XPending(GfxDpy) > 0) {
      XEvent ne;
      XPeekEvent(GfxDpy, &ne);
      /* Don't flush if its a button press or release */
      if (ne.type == ButtonPress || ne.type == ButtonRelease ||
          ne.xany.window != get_win(gtx->context_index))
        break;

      /* Get the next event */
      XNextEvent(GfxDpy, &ne);
   }
   if (GfxDpy!=GuiDpy) {
      /* while (QLength(GuiDpy) > 0) { WLH 24 May 2000 */
      while (XPending(GuiDpy) > 0) {
         XEvent ne;
         XPeekEvent(GuiDpy, &ne);
         /* Don't flush if its a button press or release */
         if (ne.type == ButtonPress || ne.type == ButtonRelease ||
             ne.xany.window != gtx->fakewin)
           break;
         /* Get the next event */
         XNextEvent(GuiDpy, &ne);
      }
   }
   return 1;
}



/*
 * Wait or poll for an X event, process the event.
 * Input:  block - 1 = wait for event, 0 = poll for event
 * Return:  0 = there was no input
 *          1 = there was a an input event
 *          2 = there was a LUI or Color Widget event
 */
int get_user_input( int block )
{
   XEvent ev;
   int index;
   int result;
   int Kurrant;

   if (GuiDpy==GfxDpy && block) {
      XNextEvent( GuiDpy, &ev );      /* blocking wait */
   }
   else if (XPending(GuiDpy)) {       /* poll */
      XNextEvent( GuiDpy, &ev );
   }
   else if (XPending(GfxDpy)) {       /* poll */
      XNextEvent( GfxDpy, &ev );
   }
   else {
      return 0;
   }
   /*
    * Determine which context the event belongs to by searching
    * the context list.
    */
   get_current_display( &Kurrant);

   for (index=0; index<VIS5D_MAX_DPY_CONTEXTS; index++) {
      int R, G, B, group;
      int change = keystatus;
      int howmany,whichones[VIS5D_MAX_CONTEXTS], howmany2; 
 
      if (gtx_table[index]) {
         vis5d_get_num_of_ctxs_in_display( index, &howmany, whichones);
         vis5d_get_num_of_itxs_in_display( index, &howmany2, whichones);
         if (ev.type == ButtonPress && (ev.xany.window == get_win(index)
             || ev.xany.window == gtx_table[index]->fakewin)
          && keystatus == 1 && howmany+howmany2 >0){
            turn_off_everything(index);
            if (key1status ==1){
               if (gtx_table[index]->group_index == 1){
                  gtx_table[index]->group_index = -1;
                  vis5d_set_display_group( index, -1);
               }
               else{
                  gtx_table[index]->group_index = 1;
                  vis5d_set_display_group( index, 1);
                  group_turn_off_everything(1);
               }
            }
            else if (key2status ==1){
               if (gtx_table[index]->group_index == 2){
                  gtx_table[index]->group_index = -1;
                  vis5d_set_display_group( index, -1);
               }
               else{
                  gtx_table[index]->group_index = 2;
                  vis5d_set_display_group( index, 2);
                  group_turn_off_everything(2);               
               }
            }
            else if (key3status ==1){
               if (gtx_table[index]->group_index == 3){            
                  gtx_table[index]->group_index = -1;            
                  vis5d_set_display_group( index, -1);            
               }            
               else{            
                  gtx_table[index]->group_index = 3;
                  vis5d_set_display_group( index, 3);
                  group_turn_off_everything(3);               
               }            
            }
            else if (key4status ==1){
               if (gtx_table[index]->group_index == 4){                        
                  gtx_table[index]->group_index = -1;                        
                  vis5d_set_display_group( index, -1);                        
               }                        
               else{                        
                  gtx_table[index]->group_index = 4;            
                  vis5d_set_display_group( index, 4);            
                  group_turn_off_everything(4);               
               }                        
            }
            else if (key5status ==1){
               if (gtx_table[index]->group_index == 5){                        
                  gtx_table[index]->group_index = -1;                        
                  vis5d_set_display_group( index, -1);                        
               }                        
               else{                        
                  gtx_table[index]->group_index = 5;            
                  vis5d_set_display_group( index, 5);            
                  group_turn_off_everything(5);               
               }                        
            }
            else if (key6status ==1){
               if (gtx_table[index]->group_index == 6){                        
                  gtx_table[index]->group_index = -1;                        
                  vis5d_set_display_group( index, -1);                        
               }                        
               else{                        
                  gtx_table[index]->group_index = 6;            
                  vis5d_set_display_group( index, 6);            
                  group_turn_off_everything(6);               
               }                        
            }
            else if (key7status ==1){
               if (gtx_table[index]->group_index == 7){                        
                  gtx_table[index]->group_index = -1;                        
                  vis5d_set_display_group( index, -1);                        
               }                        
               else{                        
                  gtx_table[index]->group_index = 7;            
                  vis5d_set_display_group( index, 7);            
                  group_turn_off_everything(7);               
               }                        
            }
            else if (key8status ==1){
               if (gtx_table[index]->group_index == 8){                        
                  gtx_table[index]->group_index = -1;                        
                  vis5d_set_display_group( index, -1);                        
               }                        
               else{                        
                  gtx_table[index]->group_index = 8;            
                  vis5d_set_display_group( index, 8);            
                  group_turn_off_everything(8);               
               }                        
            }
            else if (key9status ==1){
               if (gtx_table[index]->group_index == 9){                        
                  gtx_table[index]->group_index = -1;                        
                  vis5d_set_display_group( index, -1);                        
               }                        
               else{                        
                  gtx_table[index]->group_index = 9;            
                  vis5d_set_display_group( index, 9);            
                  group_turn_off_everything(9);               
               }                        
            }
            map_all_windows( 1 );
            map_fake_windows( 1 ); 
         }
         if (ev.type == ButtonPress && (ev.xany.window == get_win(index) ||
             ev.xany.window == gtx_table[index]->fakewin)  && keystatus == 0 &&
             howmany+howmany2 > 0 ){
            if(Kurrant != index){
               if( Kurrant >= 0){
                  hide_widgets( Kurrant );
                  get_display_border_color( Kurrant, &R, &G, &B);
                  if (R==255 && G==255 && B==255){
                     set_display_border_color(Kurrant, 165, 42, 42);
                  }
                  XUnmapWindow(GuiDpy, gtx_table[Kurrant]->CpWindow);
               }
               set_current_display( index );
               get_current_display( &Kurrant);
               get_display_border_color(Kurrant, &R, &G, &B);
               if (gtx_table[Kurrant]->how_many_regular_contexts+
                   gtx_table[Kurrant]->how_many_irregular_contexts >=1){
                  LUI_MoveResizeWindow( gtx_table[Kurrant]->CpWindow,
                   gtx_table[Kurrant]->cpx, gtx_table[Kurrant]->cpy,
                   CP_WIDTH, gtx_table[Kurrant]->CpHeight );

                  XMapWindow(GuiDpy, gtx_table[Kurrant]->CpWindow);
                 /* deiconify(Kurrant); */
               }
               if (R==165 && G==42 && B==42 && keystatus == 0){
                  set_display_border_color(Kurrant,255, 255, 255);
               }
               update_button_states( index, 0 );
               show_widgets( index );

            }
         }
      
         if (ev.xany.window == get_win(index)  
             || ev.xany.window == gtx_table[index]->SoundCtrlWindow
#ifdef DENALI
             || ev.xany.window==gtx_table[index]->NPGLwin
#endif
             || ev.xany.window==gtx_table[index]->fakewin
             || ev.xany.window==BigWindow
             || (ev.xany.window==GuiBigWin && ev.type==ConfigureNotify)){
            return gfx_window_event( index, &ev );
         }
      }
   }

   /* probably a LUI event */
   if (ev.type==ButtonPress) {
      /*vis5d_graphics_mode(index, VIS5D_PRETTY, VIS5D_OFF);*/
   }

   for (index=0; index<VIS5D_MAX_DPY_CONTEXTS; index++) {
      if (gtx_table[index]) {
         if (ev.type == ConfigureNotify && (ev.xany.window ==
             gtx_table[index]->ColorbarWindow || ev.xany.window ==
             gtx_table[index]->CHSliceWindow)){
            if (ev.xany.window==gtx_table[index]->CHSliceWindow){
               show_colorbar_window( index, cb_chvindex[index],
                    VIS5D_CHSLICE, cb_chvar[index]);
            }
            else{
               show_colorbar_window(index, cb_vindex[index],
                    cb_graphic[index], cb_var[index]);
            }
         }
      }
   }

   if (keystatus == 0){
      if (ev.type==ButtonPress) {
         LUI_EventDispatch( &ev );
         /* MJK 12.01.98 */
         update_button_matrix( Kurrant );
      }
      else{
         LUI_EventDispatch( &ev );
      }
   }
   return 2;
}




/**********************************************************************/
/*****                  GUI Construction                          *****/
/**********************************************************************/


/*
 * Make the graphics save window.
 */
static void make_save_window( int index )
{
   GuiContext gtx = get_gui_gtx(index);
   LUI_NEWBUTTON *b;
   Window w;

   w = LUI_CreateWindowAt( LUI_RootWindow, 450, 250, 239, 100 );
   gtx->SaveWindow = w;

   LUI_NewLabelCreate( w, LUI_LEFT, LUI_TOP, 280, 30,
                       "Save settings as..." );

   gtx->SaveNameField = LUI_FieldCreate( w, LUI_LEFT, LUI_NEXT_Y, 233, 28 );

   b = LUI_PushButtonCreate( w, LUI_LEFT, LUI_NEXT_Y, 115, 25, "Save" );
   b->context_index = index;
   LUI_ButtonCallback( b, save_ok_cb );
   b = LUI_PushButtonCreate( w, LUI_NEXT_X, LUI_SAME_Y, 115, 25, "Cancel" );
   b->context_index = index;
   LUI_ButtonCallback( b, save_cancel_cb );
}

static void make_savefile_window( int index )
{
   GuiContext gtx = get_gui_gtx(index);
   LUI_NEWBUTTON *b;
   Window w;

   w = LUI_CreateWindowAt( LUI_RootWindow, 450, 250, 239, 100 );
   gtx->SaveFileWindow = w;

   LUI_NewLabelCreate( w, LUI_LEFT, LUI_TOP, 280, 30,
                       "Save to v5d file as..." );

   gtx->SaveFileNameField = LUI_FieldCreate( w, LUI_LEFT, LUI_NEXT_Y, 233, 28 );

   b = LUI_PushButtonCreate( w, LUI_LEFT, LUI_NEXT_Y, 115, 25, "Save" );
   b->context_index = index;
   LUI_ButtonCallback( b, savefile_ok_cb );
   b = LUI_PushButtonCreate( w, LUI_NEXT_X, LUI_SAME_Y, 115, 25, "Cancel" );
   b->context_index = index;
   LUI_ButtonCallback( b, savefile_cancel_cb );
}



/*
 * Make the graphics restore window.
 */
static void make_restore_window( int index )
{
   GuiContext gtx = get_gui_gtx(index);
   LUI_NEWBUTTON *b;
   Window w;

   w = LUI_CreateWindowAt( LUI_RootWindow, 450, 250, 239, 100 );
   gtx->RestoreWindow = w;

   LUI_NewLabelCreate( w, LUI_LEFT, LUI_TOP, 280, 30,
                       "Restore settings from..." );
   gtx->RestoreNameField = LUI_FieldCreate( w, LUI_LEFT, LUI_NEXT_Y, 233, 28 );

   b = LUI_PushButtonCreate( w, LUI_LEFT, LUI_NEXT_Y, 115, 25, "OK" );
   b->context_index = index;
   LUI_ButtonCallback( b, restore_ok_cb );
   b = LUI_PushButtonCreate( w, LUI_NEXT_X, LUI_SAME_Y, 115, 25, "Cancel" );
   b->context_index = index;
   LUI_ButtonCallback( b, restore_cancel_cb );
}



/*
 * Make the pop-up window used for entering/editing expressions.
 */
static void make_expression_window( int index )
{
   LUI_NEWBUTTON *b;
   GuiContext gtx = get_gui_gtx(index);

   gtx->ExprWindow = LUI_CreateWindowAt( LUI_RootWindow, 450, 450,
                                         406, 26*3+3*4 );

   LUI_NewLabelCreate( gtx->ExprWindow, LUI_LEFT, LUI_TOP, 400, 26,
                       "Enter/edit expression:" );
   gtx->expr_field = LUI_FieldCreate( gtx->ExprWindow, LUI_LEFT, LUI_NEXT_Y,
                                      400, 26 );
   LUI_FieldCallback( gtx->expr_field, expr_cb );

   b = LUI_PushButtonCreate( gtx->ExprWindow, LUI_LEFT, LUI_NEXT_Y,
                             100, 26, "OK" );
   b->context_index = gtx->context_index;
   LUI_ButtonCallback( b, expr_ok_cb );
   b = LUI_PushButtonCreate( gtx->ExprWindow, LUI_NEXT_X, LUI_SAME_Y,
                             100, 26, "Cancel" );
   LUI_ButtonCallback( b, expr_cancel_cb );
}




/*
 * Make the verify window
 */
static void make_verify_window( int index )
{
   LUI_NEWBUTTON *b;
   GuiContext gtx = get_gui_gtx(index);

   gtx->VerifyWindow = LUI_CreateWindowAt( LUI_RootWindow, 600, 400, 300, 90 );

   gtx->VerifyLabel = LUI_NewLabelCreate( gtx->VerifyWindow, LUI_LEFT, LUI_TOP,
                                     300, 55, "nil" );

   b = LUI_PushButtonCreate( gtx->VerifyWindow, 33, LUI_NEXT_Y, 100,25, "OK");
   b->context_index = index;
   LUI_ButtonCallback( b, ok_cb );

   b = LUI_PushButtonCreate( gtx->VerifyWindow, 166, LUI_SAME_Y,
                             100,25, "Cancel");
   b->context_index = index;
   LUI_ButtonCallback( b, cancel_cb );
}

static void make_OK_window( int index )
{
   LUI_NEWBUTTON *b;
   GuiContext gtx = get_gui_gtx(index);

   gtx->OKWindow = LUI_CreateWindowAt( LUI_RootWindow, 600, 400, 500, 90 );

   gtx->OKLabel = LUI_NewLabelCreate( gtx->OKWindow, LUI_LEFT, LUI_TOP,
                                     500, 55, "nil" );

   b = LUI_PushButtonCreate( gtx->OKWindow, 33, LUI_NEXT_Y, 100,25, "OK");
   b->context_index = index;
   LUI_ButtonCallback( b, OK_cb );
}



/*
 * Make the alert window.
 */
static void make_alert_window( int index )
{
   LUI_NEWBUTTON *b;
   GuiContext gtx = get_gui_gtx(index);

   gtx->AlertWindow = LUI_CreateWindowAt( LUI_RootWindow, 600, 400, 600, 100 );

   gtx->AlertLabel = LUI_NewLabelCreate( gtx->AlertWindow, LUI_LEFT, LUI_TOP,
                                    600, 60, "nil" );

   b = LUI_PushButtonCreate( gtx->AlertWindow, 100, LUI_NEXT_Y, 100, 25, "OK");
   b->context_index = index;
   LUI_ButtonCallback( b, ok_cb );
}




/*
 * Make the trajectory window.
 */
static void make_trajectory_window( int index )
{
   int x, y, w, h, i;
   char label[1000];
   LUI_NEWBUTTON *b;
   int height;
   float UserTrajStep, UserTrajLength;
   int RibbonFlag;
   GuiContext gtx = get_gui_gtx(index);

   vis5d_get_traj(index, &UserTrajStep, &UserTrajLength, &RibbonFlag);

   if (gtx->TrajWindow){
      for (i=0;i<VIS5D_TRAJ_SETS;i++) {
         float r, g, b, a;
         vis5d_get_color( gtx->context_index, VIS5D_TRAJ, i, &r, &g, &b, &a );
         LUI_ButtonSetColor( gtx->TrajButton[i], (int)(r*255.0), (int)(g*255.0),
                             (int)(b*255.0) );
         if (gtx->tuowner >= 0 && vis5d_enable_graphics(gtx->tuowner, VIS5D_TRAJ, i, VIS5D_GET)) {
            LUI_ButtonState( gtx->TrajButton[i], 1 );
         }
         else{
            LUI_ButtonState( gtx->TrajButton[i], 0);
         }
      }
      LUI_FieldSetDouble( gtx->TrajStepField, UserTrajStep );
      LUI_FieldSetDouble( gtx->TrajLenField, UserTrajLength );
      gtx->TrajRibbonButton->state = RibbonFlag;
   }
   else{
      gtx->TrajWindow = LUI_CreateWindowAt(LUI_RootWindow, 405, 830, 300, 150 );

      /* Create the user trajectories title label. */
      LUI_NewLabelCreate( gtx->TrajWindow, LUI_LEFT, LUI_TOP, 300, 36,
        "   Interactive Wind Trajectories\n LFT-Display, CNT-Select, RT-Color");

      /* Trajectory set buttons */
      LUI_ButtonPadOpen ("gtx->TrajButtons",   /* name */
                    gtx->TrajWindow,           /* parent window */
                    0, 40,       /* origin relative to parent */
                    BW,                   /* border size */
                    0x000000              /* background color */        
                    );
      for (i=0;i<VIS5D_TRAJ_SETS;i++) {
         float r, g, b, a;
         vis5d_get_color( gtx->context_index, VIS5D_TRAJ, i, &r, &g, &b, &a );
         sprintf(label, "Set %d", i+1 );
         gtx->TrajButton[i] = LUI_ButtonCreate( label, LUI_BUTTON_TOGGLE,
                                           i%4, i/4, 72, i, (LUI_FNCP) traj_cb );
         LUI_ButtonSetColor( gtx->TrajButton[i], (int)(r*255.0), (int)(g*255.0),
                             (int)(b*255.0) );
         if (gtx->tuowner >= 0 && vis5d_enable_graphics(gtx->tuowner, VIS5D_TRAJ, i, VIS5D_GET)) {
            LUI_ButtonState( gtx->TrajButton[i], 1 );
         }
         else{
            LUI_ButtonState( gtx->TrajButton[i], 0);
         }
      }
      LUI_ButtonPadClose("gtx->TrajButtons");
      LUI_ButtonPadVisible("gtx->TrajButtons", 1);
      LUI_ButtonPadQuery("gtx->TrajButtons", &x, &y, &w, &h);
      height = y + h;

      /* Traj Step */
      LUI_NewLabelCreate( gtx->TrajWindow, BW, height, 50, 25, "Step:" );
      gtx->TrajStepField = LUI_FieldCreate( gtx->TrajWindow, LUI_NEXT_X, LUI_SAME_Y, 80,25);
      LUI_FieldSetDouble( gtx->TrajStepField, UserTrajStep );
      LUI_FieldCallback( gtx->TrajStepField, trajstep_cb ); 

      /* Traj Length */
      LUI_NewLabelCreate( gtx->TrajWindow, LUI_NEXT_X, LUI_SAME_Y, 65, 25, "Length:" );
      gtx->TrajLenField = LUI_FieldCreate( gtx->TrajWindow, LUI_NEXT_X, LUI_SAME_Y, 80,25 );
      LUI_FieldSetDouble( gtx->TrajLenField, UserTrajLength );
      LUI_FieldCallback( gtx->TrajLenField, trajlength_cb ); 

      gtx->TrajRibbonButton = LUI_CheckButtonCreate( gtx->TrajWindow, LUI_LEFT, LUI_NEXT_Y,
                                  70, 25, "Ribbon");
      LUI_ButtonCallback( gtx->TrajRibbonButton, ribbon_cb );

      /* MJK 12.04.98 */
      LUI_ButtonSetState (gtx->TrajRibbonButton, RibbonFlag);


      b = LUI_PushButtonCreate( gtx->TrajWindow, LUI_NEXT_X, LUI_SAME_Y,
                                110, LUI_SAME_H, "Delete Last" );
      LUI_ButtonCallback( b, dellast_cb );

      b = LUI_PushButtonCreate( gtx->TrajWindow, LUI_NEXT_X, LUI_SAME_Y,
                                105, LUI_SAME_H, "Delete Set" );
      LUI_ButtonCallback( b, delset_cb );

      XUnmapWindow(GuiDpy, gtx->TrajWindow );
   }
}

static int okay_cb(int index, LUI_NEWBUTTON *b, int state )
{
   GuiContext gtx = get_gui_gtx( index );

   XUnmapWindow( GuiDpy, gtx->error_window );
   return 0;
}



void make_error_widget( int index, char *message)
{
   LUI_NEWBUTTON *okay;
   Window w;
   GuiContext gtx = get_gui_gtx( index );


   w = LUI_CreateWindowAt( LUI_RootWindow, 200, 200 ,500, 50);
   
   gtx->error_window = w;

   LUI_BorderWidth(2);

   LUI_NewLabelCreate( w, LUI_LEFT, LUI_TOP, 490, 20, message);
   okay = LUI_PushButtonCreate( w, LUI_LEFT, LUI_NEXT_Y, 66, 20, "OK");
   LUI_ButtonCallback( okay, okay_cb ); 
}

   

void show_error_widget( int index, char *message)
{
   GuiContext gtx = get_gui_gtx( index );

   make_error_widget( index, message);
   XMapWindow( GuiDpy, gtx->error_window);
}



void create_nodata_widgets( int index )
{
   Window win;
   int x,y,w,h, hb, wb;
   GuiContext gtx = get_gui_gtx(index);
 
   LUI_LayoutGutter( 3 );
   LUI_Initialize( "Vis5D", GuiDpy, GuiVisual, GuiDepth, GuiColormap );

   vis5d_get_display_window(index, &win);
   gtx->mainwin = win;
   if (GuiDpy==GfxDpy) {
      LUI_SetMainWindow( win );
   }
#ifdef LG
   gtx->cpy = 32;
#endif
   /*******************************/
   /* Create control panel window */
   /*******************************/
   if (!gtx->CpWindow){
      gtx->CpWindow = LUI_CreateWindowAt( LUI_RootWindow, /* parent */
                           gtx->cpx, gtx->cpy, /* position */ 100, 100 /* temporary size */);
   }
   LUI_EventAdd ( gtx->CpWindow, ExposureMask | ButtonPressMask
                    | StructureNotifyMask , (LUI_FNCP) controlpanel_cb);

   gtx->CpHeight = 0;

   /******************************************************************/
   /* Create the copyright label.                                    */
   /******************************************************************/
   if (gtx->copyrightBUTTON){
      LUI_LabelDestroy( gtx->copyrightBUTTON );
   }
   gtx->copyrightBUTTON = LUI_LabelOpen ( "Copyright",      /* name */
             3, Copyright,           /* text strings */
             gtx->CpWindow,               /* parent window */
             0, 0,                   /* origin relative to parent */
             CP_WIDTH, 50,           /* dimensions */
             BW,                     /* border size */
             NULL                    /* callback function */
             );
   LUI_LabelClose("Copyright");
   LUI_LabelVisible("Copyright", 1);
   LUI_LabelHighlight("Copyright", 0);
   LUI_LabelQuery("Copyright", &x, &y, &w, &h);
   gtx->CpHeight += h;
   /************************************************************/
   /* Create the main button pad.                              */
   /************************************************************/
   hb = 19;
   wb = 90;
   if (gtx->lillypadBUTTON){
      LUI_ButtonPadDestroy( gtx->lillypadBUTTON );
   }
      gtx->lillypadBUTTON = LUI_ButtonPadOpen ("Buttons", /* name */
                       gtx->CpWindow,       /* parent window */
                       0, gtx->CpHeight,    /* origin relative to parent */
                       BW,             /* border size */
                       0x000000        /* background color */
                       );

   LUI_BorderWidth(1);
   if (gtx->animateBUTTON){
      LUI_NewButtonDestroy( gtx->animateBUTTON );
      LUI_NewButtonDestroy( gtx->stepBUTTON );
   }
   gtx->animateBUTTON = LUI_ToggleButtonCreate(gtx->CpWindow, 6, gtx->CpHeight+6,
                        wb, hb, "");
   gtx->stepBUTTON = LUI_PushButtonCreate(gtx->CpWindow, LUI_NEXT_X,
                        LUI_SAME_Y, wb, hb, "");
   LUI_ButtonCallback(gtx->animateBUTTON, NULL );
   LUI_ButtonCallback(gtx->stepBUTTON, NULL);

   if (gtx->newvarBUTTON){
      LUI_NewButtonDestroy( gtx->newvarBUTTON );
   }
   gtx->newvarBUTTON=LUI_PushButtonCreate(gtx->CpWindow, LUI_NEXT_X, LUI_SAME_Y,
                           wb, hb, "");
   LUI_ButtonCallback(gtx->newvarBUTTON, NULL);

   /********************/
   /* This one WORKS!! */
   /********************/
   if (gtx->exitBUTTON){
      LUI_NewButtonDestroy( gtx->exitBUTTON );
   }
   gtx->exitBUTTON = LUI_PushButtonCreate(gtx->CpWindow, LUI_NEXT_X, LUI_SAME_Y,
                          wb, hb, "EXIT");
   LUI_ButtonCallback(gtx->exitBUTTON, exit_cb);
   LUI_ButtonContextIndex(gtx->exitBUTTON, index );


   /** Row 2 **/
   if (gtx->textureBUTTON){
      LUI_NewButtonDestroy( gtx->textureBUTTON );
   }
   gtx->textureBUTTON = LUI_ToggleButtonCreate(gtx->CpWindow,6,
                           LUI_NEXT_Y, wb, hb, "");
   LUI_ButtonCallback(gtx->textureBUTTON, NULL);




   if (gtx->topBUTTON){
      LUI_NewButtonDestroy( gtx->topBUTTON );
      LUI_NewButtonDestroy( gtx->southBUTTON );
      LUI_NewButtonDestroy( gtx->westBUTTON );
   }
   gtx->topBUTTON =LUI_PushButtonCreate(gtx->CpWindow, LUI_NEXT_X, LUI_SAME_Y,
                          wb, hb, "");
   gtx->southBUTTON =LUI_PushButtonCreate(gtx->CpWindow, LUI_NEXT_X, LUI_SAME_Y,
                          wb, hb, "");
   gtx->westBUTTON  =LUI_PushButtonCreate(gtx->CpWindow, LUI_NEXT_X, LUI_SAME_Y,
                          wb, hb, "");
   LUI_ButtonCallback(gtx->topBUTTON, NULL);
   LUI_ButtonCallback(gtx->southBUTTON, NULL);
   LUI_ButtonCallback(gtx->westBUTTON, NULL);


   /** Row 3 **/
   if (gtx->topoBUTTON){
      LUI_NewButtonDestroy( gtx->topoBUTTON );
   }
   gtx->topoBUTTON = LUI_ToggleButtonCreate(gtx->CpWindow,6,
                     LUI_NEXT_Y, wb, hb, "");
   LUI_ButtonCallback(gtx->topoBUTTON, NULL);
   if (gtx->map_button){
      LUI_NewButtonDestroy( gtx->map_button );
   }
   gtx->map_button = LUI_ToggleButtonCreate(gtx->CpWindow, LUI_NEXT_X,
                       LUI_SAME_Y, wb, hb, "");
   LUI_ButtonCallback(gtx->map_button, NULL);
   LUI_ButtonContextIndex(gtx->map_button, index);
   if (gtx->boxBUTTON){
      LUI_NewButtonDestroy( gtx->boxBUTTON );
      LUI_NewButtonDestroy( gtx->clockBUTTON );
   }
   gtx->boxBUTTON = LUI_ToggleButtonCreate(gtx->CpWindow, LUI_NEXT_X,
                          LUI_SAME_Y, wb, hb, "");
   LUI_ButtonCallback(gtx->boxBUTTON, NULL);

   gtx->clockBUTTON=LUI_ToggleButtonCreate(gtx->CpWindow, LUI_NEXT_X,
                          LUI_SAME_Y, wb, hb, "");
   LUI_ButtonCallback(gtx->clockBUTTON, NULL);


   /** Row 4 **/
   if (gtx->saveBUTTON){
      LUI_NewButtonDestroy(gtx->saveBUTTON);
      LUI_NewButtonDestroy(gtx->restoreBUTTON);
      LUI_NewButtonDestroy(gtx->gridBUTTON);
      LUI_NewButtonDestroy(gtx->contBUTTON);
   }
   gtx->saveBUTTON = LUI_PushButtonCreate(gtx->CpWindow, 6,
                        LUI_NEXT_Y, wb, hb, "");
   gtx->restoreBUTTON = LUI_PushButtonCreate(gtx->CpWindow, LUI_NEXT_X,
                        LUI_SAME_Y, wb, hb, "");
   gtx->gridBUTTON = LUI_ToggleButtonCreate(gtx->CpWindow, LUI_NEXT_X,
                        LUI_SAME_Y, wb, hb, "");
   gtx->contBUTTON = LUI_ToggleButtonCreate(gtx->CpWindow, LUI_NEXT_X,
                        LUI_SAME_Y, wb, hb, "");

   LUI_ButtonCallback(gtx->saveBUTTON, NULL); 
   LUI_ButtonCallback(gtx->restoreBUTTON, NULL);
   LUI_ButtonCallback(gtx->gridBUTTON, NULL);
   LUI_ButtonCallback(gtx->contBUTTON,NULL);



   /** Row 5 **/
   if (gtx->animrecBUTTON){
      LUI_NewButtonDestroy(gtx->animrecBUTTON);
   }
   gtx->animrecBUTTON = LUI_ToggleButtonCreate(gtx->CpWindow, 6,
                           LUI_NEXT_Y, wb, hb, "");
   LUI_ButtonCallback(gtx->animrecBUTTON, NULL);
   LUI_ButtonContextIndex(gtx->animrecBUTTON, index );
   if (gtx->reverseBUTTON != NULL){
      LUI_NewButtonDestroy(gtx->reverseBUTTON);
   }
   gtx->reverseBUTTON = LUI_ToggleButtonCreate(gtx->CpWindow, LUI_NEXT_X,
                           LUI_SAME_Y, wb, hb, "");
   LUI_ButtonCallback(gtx->reverseBUTTON,NULL); 

   if (gtx->savepicBUTTON){
      LUI_NewButtonDestroy( gtx->savepicBUTTON );
   }
   gtx->savepicBUTTON=LUI_PushButtonCreate(gtx->CpWindow, LUI_NEXT_X,
                     LUI_SAME_Y, wb, hb, "");
   LUI_ButtonCallback(gtx->savepicBUTTON, NULL);
   LUI_ButtonContextIndex(gtx->savepicBUTTON, index );
   if(gtx->perspec_button){
      LUI_NewButtonDestroy( gtx->perspec_button );
   }
   gtx->perspec_button =  LUI_ToggleButtonCreate(gtx->CpWindow, LUI_NEXT_X,
                        LUI_SAME_Y, wb, hb, "");
   LUI_ButtonCallback(gtx->perspec_button,NULL); 

   /** Row 5 **/
   if(gtx->scriptBUTTON){
      LUI_NewButtonDestroy(gtx->scriptBUTTON);
      LUI_NewButtonDestroy(gtx->interpBUTTON);
      LUI_NewButtonDestroy(gtx->uvwvarsBUTTON);
      LUI_NewButtonDestroy(gtx->legendsBUTTON);
   }
   gtx->scriptBUTTON = LUI_PushButtonCreate(gtx->CpWindow, 6, LUI_NEXT_Y,
                                      wb, hb, "SCRIPT..");
   gtx->interpBUTTON = LUI_PushButtonCreate(gtx->CpWindow, LUI_NEXT_X,
                        LUI_SAME_Y, wb, hb, "INTERP..");
   gtx->uvwvarsBUTTON= LUI_PushButtonCreate(gtx->CpWindow, LUI_NEXT_X,
                        LUI_SAME_Y, wb, hb, "");
   gtx->legendsBUTTON= LUI_ToggleButtonCreate(gtx->CpWindow, LUI_NEXT_X,
                        LUI_SAME_Y, wb, hb, "");
   LUI_ButtonCallback(gtx->scriptBUTTON, script_cb);
   LUI_ButtonCallback(gtx->interpBUTTON, interpret_cb );
   LUI_ButtonCallback(gtx->uvwvarsBUTTON,NULL); 
   LUI_ButtonCallback(gtx->legendsBUTTON,NULL); 



   /** Row 6 **/
   if (gtx->importBUTTON){
      LUI_NewButtonDestroy(gtx->importBUTTON);
      LUI_NewButtonDestroy(gtx->iimportBUTTON);
      LUI_NewButtonDestroy(gtx->displayBUTTON);
      LUI_NewButtonDestroy(gtx->blank1BUTTON);
   }
                                                                               
   gtx->importBUTTON =LUI_PushButtonCreate(gtx->CpWindow, 6, LUI_NEXT_Y,
                                      wb, hb, "IMPORT");
   gtx->iimportBUTTON=LUI_PushButtonCreate(gtx->CpWindow, LUI_NEXT_X,
                        LUI_SAME_Y, wb, hb, "IRG IMPORT");
   gtx->displayBUTTON= LUI_PushButtonCreate(gtx->CpWindow, LUI_NEXT_X,
                        LUI_SAME_Y, wb, hb, "DISPLAY");
   gtx->blank1BUTTON = LUI_PushButtonCreate(gtx->CpWindow, LUI_NEXT_X,
                        LUI_SAME_Y, wb, hb, "");

   LUI_ButtonCallback(gtx->importBUTTON, import_cb);
   LUI_ButtonCallback(gtx->iimportBUTTON, iimport_cb);
   LUI_ButtonCallback(gtx->displayBUTTON, display_cb);
   LUI_ButtonCallback(gtx->blank1BUTTON, NULL);
   LUI_ButtonContextIndex(gtx->importBUTTON, index );
   LUI_ButtonContextIndex(gtx->iimportBUTTON, index );
   LUI_ButtonContextIndex(gtx->displayBUTTON, index );
   LUI_ButtonContextIndex(gtx->blank1BUTTON, index );



   LUI_ButtonPadClose("Buttons");
   LUI_ButtonPadVisible("Buttons", 1);
   LUI_ButtonPadQuery("Buttons", &x, &y, &w, &h);
   gtx->CpHeight += hb*8+8;
   if (!gtx->VerifyWindow){
      make_verify_window(index);
      make_OK_window( index );
      make_alert_window(index);
      make_save_window(index);
      make_savefile_window(index);
      make_restore_window(index);
      make_savepic_window(index);
      make_expression_window(index);
      make_rgb_sliders( gtx );
      make_uvw_widget( gtx );
      make_tp_color_window( gtx );
   }
   /*
   make_verify_window( index );
   make_OK_window( index );
   */
}




/*
 * Create all the LUI widgets.
 * Input:  volflag - if non-zero, add a 6th column for volume rendering.
 */
void create_widgets( int index, int volflag, char *programname )
{
   Window window,sndwindow;
   int WinWidth, WinHeight;
   int NumTimes[1000], NumVars[1000];
   int Nr, Nc, Nl[MAXVARS], LowLev[MAXVARS], MaxNl, MaxNlVar, WindNl, WindLow;
   int Uvar, Vvar, Wvar, Uvar2, Vvar2, Wvar2, TrajU, TrajV, TrajW;
   int topoflag, mapflag, textureflag;
   float MinTopoHgt, MaxTopoHgt;
   int x, y, w, h, i, j, yo, yo2, yo3, bottom;
   LUI_BUTTON *lb;
   GuiContext gtx = get_gui_gtx(index);
   LUI_NEWBUTTON *b;
   int spandex, adder=0;
   int hb, wb, proceed;
   Window win;

   /* MJK 12.04.98 */
   int          geom_ht = gtx->CpHeight;
   int          state;

   /********************************/
   /* get the number of time steps */
   /********************************/
   gtx->total_ctx_numtimes = 0;
   gtx->total_itx_numtimes = 0;
   for (yo = 0; yo < gtx->how_many_regular_contexts; yo ++){
      spandex = gtx->array_of_ctxs[yo];
      vis5d_get_ctx_numtimes( spandex, &i);
      gtx->total_ctx_numtimes += i;
   }
   for (yo = 0; yo < gtx->how_many_irregular_contexts; yo ++){
      spandex = gtx->array_of_itxs[yo];
      vis5d_get_itx_numtimes( spandex, &i);
      gtx->total_itx_numtimes += i;
   }

   vis5d_get_dtx_numtimes( index, &i);
   gtx->total_dtx_numtimes = i;


   /************************************************************/
   /* get the number of variables total and for each vis5d_ctx */   
   /************************************************************/
   adder = 0;
   for (yo = 0; yo < gtx->how_many_regular_contexts; yo ++){
      spandex = gtx->array_of_ctxs[yo];
      vis5d_get_ctx_numvars( spandex, &gtx->RegularNumVars[spandex]);
      adder += gtx->RegularNumVars[spandex];
      gtx->total_numvars = adder;
   }
   for (yo = 0; yo < gtx->how_many_irregular_contexts; yo ++){
      spandex = gtx->array_of_itxs[yo];
      vis5d_get_itx_numvars( spandex, &gtx->IrregularNumVars[spandex]);      
   }

   /*************************************************************************************/
   /* get the number of levels of each vis5d_ctx, in order to see which buttons to make */
   /*************************************************************************************/
   for (yo = 0; yo < gtx->how_many_regular_contexts; yo ++){
      spandex = gtx->array_of_ctxs[yo];
      vis5d_get_size(spandex, &Nr, &Nc, gtx->NumLevels[spandex], LowLev,
                      &MaxNl, &MaxNlVar, &WindNl, &WindLow);
   }
   vis5d_get_wind_vars(index, &gtx->u1owner, &Uvar, &gtx->v1owner,
                          &Vvar, &gtx->w1owner, &Wvar, &gtx->u2owner,
                          &Uvar2, &gtx->v2owner, &Vvar2, &gtx->w2owner,
                          &Wvar2, &gtx->tuowner, &TrajU, &gtx->tvowner,
                          &TrajV, &gtx->twowner, &TrajW); 


   vis5d_check_topo( index, &topoflag );
   vis5d_check_map( index, &mapflag );
   vis5d_check_texture( index, &textureflag );
   vis5d_get_topo_range(index, &MinTopoHgt, &MaxTopoHgt);
   /**************************/
   /* get the variable names */
   /**************************/
   for (yo=0; yo< gtx->how_many_regular_contexts; yo++){
      spandex = gtx->array_of_ctxs[yo];
      for (i=0;i<gtx->RegularNumVars[spandex];i++) {
         vis5d_get_ctx_var_name( spandex, i, gtx->RegularVarName[i][spandex] );
      }
   }

   /*************************************/
   /* get irregular file names and vars */
   /*************************************/
   for (yo=0; yo< gtx->how_many_irregular_contexts; yo++){
      vis5d_get_itx_name(yo, gtx->FileName[yo]);
      spandex = gtx->array_of_itxs[yo];
      for (i=0;i<gtx->IrregularNumVars[spandex];i++){
         vis5d_get_itx_var_name( spandex, i, gtx->IrregularVarName[i][spandex] );
      }
   }

   LUI_LayoutGutter( 3 );
   LUI_Initialize( programname, GuiDpy, GuiVisual, GuiDepth, GuiColormap );


   /* this is omitted asumeing that the BigWindow and LittleWindow stuff works */
   /* vis5d_get_window( index, &window, &WinWidth, &WinHeight ); */


   vis5d_get_display_window(index, &win);
   gtx->mainwin = win;


   if (GuiDpy==GfxDpy) {
      LUI_SetMainWindow( win );
   }
   /*******************************/
   /* Create control panel window */
   /*******************************/
   proceed = 0;

#ifdef LG 
   gtx->cpy = 32;
#endif
   if (!gtx->CpWindow){
      gtx->CpWindow = LUI_CreateWindowAt( LUI_RootWindow, /* parent */
                           gtx->cpx, gtx->cpy, /* position */ 100, 100 /* temporary size */);
      proceed = 1;
   }      
   gtx->CpHeight = 0;
   /******************************************************************/
   /* Create the copyright label.                                    */
   /******************************************************************/
   if (gtx->copyrightBUTTON){
      LUI_LabelDestroy( gtx->copyrightBUTTON );
   }
   gtx->copyrightBUTTON = LUI_LabelOpen ( "Copyright",      /* name */
             3, Copyright,           /* text strings */
             gtx->CpWindow,               /* parent window */
             0, 0,                   /* origin relative to parent */
             CP_WIDTH, 50,           /* dimensions */
             BW,                     /* border size */
             NULL                    /* callback function */
             );
   LUI_LabelClose("Copyright");
   LUI_LabelVisible("Copyright", 1);
   LUI_LabelHighlight("Copyright", 0);
   LUI_LabelQuery("Copyright", &x, &y, &w, &h);
   gtx->CpHeight += h;
   /************************************************************/
   /* Create the main button pad.                              */
   /************************************************************/
   hb = 19;
   wb = 90;
   if (gtx->lillypadBUTTON){
      LUI_ButtonPadDestroy( gtx->lillypadBUTTON );
   } 
      gtx->lillypadBUTTON = LUI_ButtonPadOpen ("Buttons", /* name */
                       gtx->CpWindow,       /* parent window */
                       0, gtx->CpHeight,    /* origin relative to parent */
                       BW,             /* border size */
                       0x000000        /* background color */
                       );

   LUI_BorderWidth(1);
   /** Row 1 **/
   if (gtx->total_dtx_numtimes > 1) {
      if (gtx->animateBUTTON){
         LUI_NewButtonDestroy( gtx->animateBUTTON );
         LUI_NewButtonDestroy( gtx->stepBUTTON ); 
      }
      gtx->animateBUTTON = LUI_ToggleButtonCreate(gtx->CpWindow, 6, gtx->CpHeight+6,
                           wb, hb, "ANIMATE");
      gtx->stepBUTTON = LUI_PushButtonCreate(gtx->CpWindow, LUI_NEXT_X,
                           LUI_SAME_Y, wb, hb, "STEP");
      LUI_ButtonCallback(gtx->animateBUTTON, anim_cb);
      LUI_ButtonCallback(gtx->stepBUTTON, step_cb);
      LUI_ButtonContextIndex(gtx->animateBUTTON, index);
      LUI_ButtonContextIndex(gtx->stepBUTTON,  index);
      gtx->animateBUTTON->special = 1;
   }
   else{
      if (gtx->animateBUTTON){
         LUI_NewButtonDestroy( gtx->animateBUTTON );
         LUI_NewButtonDestroy( gtx->stepBUTTON );
      }
      gtx->animateBUTTON = LUI_ToggleButtonCreate(gtx->CpWindow, 6, gtx->CpHeight+6,
                           wb, hb, "ANIMATE");
      gtx->stepBUTTON = LUI_PushButtonCreate(gtx->CpWindow, LUI_NEXT_X,
                           LUI_SAME_Y, wb, hb, "STEP");
      LUI_ButtonCallback(gtx->animateBUTTON, NULL );
      LUI_ButtonCallback(gtx->stepBUTTON, NULL);
   }
   


   if (gtx->newvarBUTTON){
      LUI_NewButtonDestroy( gtx->newvarBUTTON );
   }
   gtx->newvarBUTTON=LUI_PushButtonCreate(gtx->CpWindow, LUI_NEXT_X, LUI_SAME_Y,
                           wb, hb, "NEW VAR..");
   LUI_ButtonCallback(gtx->newvarBUTTON, newvar_cb); 
   LUI_ButtonContextIndex(gtx->newvarBUTTON, index);


   if (gtx->exitBUTTON){
      LUI_NewButtonDestroy( gtx->exitBUTTON );
   }
   gtx->exitBUTTON = LUI_PushButtonCreate(gtx->CpWindow, LUI_NEXT_X, LUI_SAME_Y,
                          wb, hb, "EXIT");
   LUI_ButtonCallback(gtx->exitBUTTON, exit_cb);
   LUI_ButtonContextIndex(gtx->exitBUTTON, index );


   /** Row 2 **/
   if (textureflag) {
      if (gtx->textureBUTTON){
         LUI_NewButtonDestroy( gtx->textureBUTTON );
      }
      gtx->textureBUTTON = LUI_ToggleButtonCreate(gtx->CpWindow,6,
                              LUI_NEXT_Y, wb, hb, "TEXTURE");
      LUI_ButtonCallback(gtx->textureBUTTON, texture_cb);
      LUI_ButtonContextIndex(gtx->textureBUTTON, index);
      gtx->textureBUTTON->special = 1;
   }
   else {
      if (gtx->textureBUTTON){
         LUI_NewButtonDestroy( gtx->textureBUTTON );
      }
      gtx->textureBUTTON = LUI_ToggleButtonCreate(gtx->CpWindow,6,
                              LUI_NEXT_Y, wb, hb, "");
      LUI_ButtonCallback(gtx->textureBUTTON, NULL);
   }




   if (gtx->topBUTTON){
      LUI_NewButtonDestroy( gtx->topBUTTON );
      LUI_NewButtonDestroy( gtx->southBUTTON );
      LUI_NewButtonDestroy( gtx->westBUTTON );
   }
   gtx->topBUTTON =LUI_PushButtonCreate(gtx->CpWindow, LUI_NEXT_X, LUI_SAME_Y,
                          wb, hb, "TOP");
   gtx->southBUTTON =LUI_PushButtonCreate(gtx->CpWindow, LUI_NEXT_X, LUI_SAME_Y,
                          wb, hb, "SOUTH");
   gtx->westBUTTON  =LUI_PushButtonCreate(gtx->CpWindow, LUI_NEXT_X, LUI_SAME_Y,
                          wb, hb, "WEST");
   LUI_ButtonCallback(gtx->topBUTTON, view_cb);
   LUI_ButtonCallback(gtx->southBUTTON, view_cb);
   LUI_ButtonCallback(gtx->westBUTTON, view_cb);
   gtx->topBUTTON->context_index = ((index+1) * MM_MAX + 1);
   gtx->southBUTTON->context_index = ((index+1) * MM_MAX + 2);
   gtx->westBUTTON->context_index = ((index+1) * MM_MAX + 3);




   /** Row 3 **/
   if (topoflag) {
      if (gtx->topoBUTTON){
         LUI_NewButtonDestroy( gtx->topoBUTTON );
      }
      gtx->topoBUTTON = LUI_ToggleButtonCreate(gtx->CpWindow,6,
                        LUI_NEXT_Y, wb, hb, "TOPO");
      LUI_ButtonCallback(gtx->topoBUTTON, topo_cb);
      LUI_ButtonContextIndex(gtx->topoBUTTON,  index);
   }
   else {
      if (gtx->topoBUTTON){
         LUI_NewButtonDestroy( gtx->topoBUTTON );
      }
      gtx->topoBUTTON = LUI_ToggleButtonCreate(gtx->CpWindow,6,
                        LUI_NEXT_Y, wb, hb, "");
      LUI_ButtonCallback(gtx->topoBUTTON, NULL);
      LUI_ButtonContextIndex(gtx->topoBUTTON, index);
   }



   if (mapflag) {
      if (gtx->map_button){
         LUI_NewButtonDestroy( gtx->map_button );
      }
      gtx->map_button = LUI_ToggleButtonCreate(gtx->CpWindow, LUI_NEXT_X,
                          LUI_SAME_Y, wb, hb, "MAP");
      LUI_ButtonCallback(gtx->map_button, map_cb);
      LUI_ButtonContextIndex(gtx->map_button, index);
   }
   else {
      if (gtx->map_button){
         LUI_NewButtonDestroy( gtx->map_button );
      }
      gtx->map_button = LUI_ToggleButtonCreate(gtx->CpWindow, LUI_NEXT_X,
                          LUI_SAME_Y, wb, hb, "");
      LUI_ButtonCallback(gtx->map_button, NULL);
      LUI_ButtonContextIndex(gtx->map_button, index);
   }



   if (gtx->boxBUTTON){
      LUI_NewButtonDestroy( gtx->boxBUTTON );
      LUI_NewButtonDestroy( gtx->clockBUTTON );
   }
   gtx->boxBUTTON = LUI_ToggleButtonCreate(gtx->CpWindow, LUI_NEXT_X,
                          LUI_SAME_Y, wb, hb, "BOX");
   gtx->boxBUTTON->state = 1;
   LUI_ButtonCallback(gtx->boxBUTTON, box_cb);
   LUI_ButtonContextIndex(gtx->boxBUTTON, index);
  
   gtx->clockBUTTON=LUI_ToggleButtonCreate(gtx->CpWindow, LUI_NEXT_X,
                          LUI_SAME_Y, wb, hb, "CLOCK");
   gtx->clockBUTTON->state = 1;
   LUI_ButtonCallback(gtx->clockBUTTON, clock_cb);
   LUI_ButtonContextIndex(gtx->clockBUTTON, index);




   /** Row 4 **/
   if (gtx->saveBUTTON){
      LUI_NewButtonDestroy(gtx->saveBUTTON);
      LUI_NewButtonDestroy(gtx->restoreBUTTON);
      LUI_NewButtonDestroy(gtx->gridBUTTON);
      LUI_NewButtonDestroy(gtx->contBUTTON);
   }
   gtx->saveBUTTON = LUI_PushButtonCreate(gtx->CpWindow, 6,
                        LUI_NEXT_Y, wb, hb, "SAVE..");
   gtx->restoreBUTTON = LUI_PushButtonCreate(gtx->CpWindow, LUI_NEXT_X,
                        LUI_SAME_Y, wb, hb, "RESTORE");
   gtx->gridBUTTON = LUI_ToggleButtonCreate(gtx->CpWindow, LUI_NEXT_X,
                        LUI_SAME_Y, wb, hb, "GRID #'s");
   gtx->contBUTTON = LUI_ToggleButtonCreate(gtx->CpWindow, LUI_NEXT_X,
                        LUI_SAME_Y, wb, hb, "CONT #'s");
   gtx->contBUTTON->state = 1;

   LUI_ButtonCallback(gtx->saveBUTTON, save_cb);
   LUI_ButtonCallback(gtx->restoreBUTTON, restore_cb);
   LUI_ButtonCallback(gtx->gridBUTTON, coord_cb);
   LUI_ButtonCallback(gtx->contBUTTON, contnum_cb);
   LUI_ButtonContextIndex(gtx->saveBUTTON, index);
   LUI_ButtonContextIndex(gtx->restoreBUTTON, index );
   LUI_ButtonContextIndex(gtx->gridBUTTON, index);
   LUI_ButtonContextIndex(gtx->contBUTTON, index);



   /** Row 5 **/
#if defined(MESA)
   if (gtx->animrecBUTTON){
      LUI_NewButtonDestroy(gtx->animrecBUTTON);
   }
   gtx->animrecBUTTON = LUI_ToggleButtonCreate(gtx->CpWindow, 6,
                           LUI_NEXT_Y, wb, hb, "ANIM");
   LUI_ButtonCallback(gtx->animrecBUTTON, animrec_cb);
   LUI_ButtonContextIndex(gtx->animrecBUTTON, index );
#else
   if (gtx->animrecBUTTON){
      LUI_NewButtonDestroy(gtx->animrecBUTTON);
   }
   gtx->animrecBUTTON = LUI_ToggleButtonCreate(gtx->CpWindow, 6,
                           LUI_NEXT_Y, wb, hb, "");
   LUI_ButtonCallback(gtx->animrecBUTTON, NULL);
   LUI_ButtonContextIndex(gtx->animrecBUTTON, index );
#endif
   if (gtx->reverseBUTTON != NULL){
      LUI_NewButtonDestroy(gtx->reverseBUTTON);
   }
   gtx->reverseBUTTON = LUI_ToggleButtonCreate(gtx->CpWindow, LUI_NEXT_X,
                           LUI_SAME_Y, wb, hb, "REVERSE");
   LUI_ButtonCallback(gtx->reverseBUTTON, reverse_cb);
   LUI_ButtonContextIndex(gtx->reverseBUTTON, index );

   if (save_formats()>0) {
      if (gtx->savepicBUTTON){
         LUI_NewButtonDestroy( gtx->savepicBUTTON );
      }
      gtx->savepicBUTTON=LUI_PushButtonCreate(gtx->CpWindow, LUI_NEXT_X,
                        LUI_SAME_Y, wb, hb, "SAVE PIC");
      LUI_ButtonCallback(gtx->savepicBUTTON, savepic_cb);
      LUI_ButtonContextIndex(gtx->savepicBUTTON, index );
   }
   else {
      if (gtx->savepicBUTTON){
         LUI_NewButtonDestroy( gtx->savepicBUTTON );
      }
      gtx->savepicBUTTON=LUI_PushButtonCreate(gtx->CpWindow, LUI_NEXT_X,
                        LUI_SAME_Y, wb, hb, "");
      LUI_ButtonCallback(gtx->savepicBUTTON, NULL);
      LUI_ButtonContextIndex(gtx->savepicBUTTON, index );
   }
   if (Perspec_available) {
      if(gtx->perspec_button){
         LUI_NewButtonDestroy( gtx->perspec_button );
      }
      gtx->perspec_button =  LUI_ToggleButtonCreate(gtx->CpWindow, LUI_NEXT_X,
                           LUI_SAME_Y, wb, hb, "PERSPEC");
      LUI_ButtonCallback(gtx->perspec_button, proj_cb);
      LUI_ButtonContextIndex(gtx->perspec_button, index);
   }
   else {
      gtx->perspec_button = NULL;
      if(gtx->perspecBUTTON){
         LUI_NewButtonDestroy( gtx->perspecBUTTON );
      }
      gtx->perspecBUTTON = LUI_PushButtonCreate(gtx->CpWindow, LUI_NEXT_X,
                        LUI_SAME_Y, wb, hb, "");
      LUI_ButtonCallback(gtx->perspecBUTTON, NULL);
      LUI_ButtonContextIndex(gtx->perspecBUTTON, index );

   }

   /** Row 5 **/
   if(gtx->scriptBUTTON){
      LUI_NewButtonDestroy(gtx->scriptBUTTON);
      LUI_NewButtonDestroy(gtx->interpBUTTON);
      LUI_NewButtonDestroy(gtx->uvwvarsBUTTON);
      LUI_NewButtonDestroy(gtx->legendsBUTTON);
   }
   gtx->scriptBUTTON = LUI_PushButtonCreate(gtx->CpWindow, 6, LUI_NEXT_Y,
                                      wb, hb, "SCRIPT..");
   gtx->interpBUTTON = LUI_PushButtonCreate(gtx->CpWindow, LUI_NEXT_X,
                        LUI_SAME_Y, wb, hb, "INTERP..");
   gtx->uvwvarsBUTTON= LUI_PushButtonCreate(gtx->CpWindow, LUI_NEXT_X,
                        LUI_SAME_Y, wb, hb, "UVW VARS..");
   gtx->legendsBUTTON= LUI_ToggleButtonCreate(gtx->CpWindow, LUI_NEXT_X,
                        LUI_SAME_Y, wb, hb, "LEGENDS");
   LUI_ButtonCallback(gtx->scriptBUTTON, script_cb);
   LUI_ButtonCallback(gtx->interpBUTTON, interpret_cb );
   LUI_ButtonCallback(gtx->uvwvarsBUTTON, uvw_cb );
   LUI_ButtonCallback(gtx->legendsBUTTON, legend_cb );
   LUI_ButtonContextIndex(gtx->scriptBUTTON, index);
   LUI_ButtonContextIndex(gtx->interpBUTTON, index);
   LUI_ButtonContextIndex(gtx->uvwvarsBUTTON, index);
   LUI_ButtonContextIndex(gtx->legendsBUTTON, index);
   gtx->legendsBUTTON->special = 1;



   /** Row 6 **/
   if (gtx->importBUTTON){
      LUI_NewButtonDestroy(gtx->importBUTTON);
      LUI_NewButtonDestroy(gtx->iimportBUTTON);
      LUI_NewButtonDestroy(gtx->displayBUTTON);
      LUI_NewButtonDestroy(gtx->blank1BUTTON);
   }

   gtx->importBUTTON =LUI_PushButtonCreate(gtx->CpWindow, 6, LUI_NEXT_Y,
                                      wb, hb, "IMPORT");
   gtx->iimportBUTTON=LUI_PushButtonCreate(gtx->CpWindow, LUI_NEXT_X,
                        LUI_SAME_Y, wb, hb, "IRG IMPORT");
   gtx->displayBUTTON= LUI_PushButtonCreate(gtx->CpWindow, LUI_NEXT_X,
                        LUI_SAME_Y, wb, hb, "DISPLAY");
   gtx->blank1BUTTON = LUI_PushButtonCreate(gtx->CpWindow, LUI_NEXT_X,
                        LUI_SAME_Y, wb, hb, "");

   LUI_ButtonCallback(gtx->importBUTTON, import_cb);
   LUI_ButtonCallback(gtx->iimportBUTTON, iimport_cb);
   LUI_ButtonCallback(gtx->displayBUTTON, display_cb);
   LUI_ButtonCallback(gtx->blank1BUTTON, NULL);
   LUI_ButtonContextIndex(gtx->importBUTTON, index );
   LUI_ButtonContextIndex(gtx->iimportBUTTON, index );
   LUI_ButtonContextIndex(gtx->displayBUTTON, index );
   LUI_ButtonContextIndex(gtx->blank1BUTTON, index );



   LUI_ButtonPadClose("Buttons");
   LUI_ButtonPadVisible("Buttons", 1);
   LUI_ButtonPadQuery("Buttons", &x, &y, &w, &h);
   gtx->CpHeight += hb*8+8;


   
   /******************************************************************/
   /* Create the radio buttons.                                    ***/
   /******************************************************************/
   if (gtx->radiopadBUTTON){
      LUI_ButtonDestroy(gtx->normalBUTTON);
      if (gtx->trajectoryBUTTON){
         LUI_ButtonDestroy( gtx->trajectoryBUTTON );
      }
      LUI_ButtonDestroy(gtx->sliceBUTTON);
      LUI_ButtonDestroy(gtx->labelBUTTON);
      LUI_ButtonDestroy(gtx->probeBUTTON);
      LUI_ButtonDestroy(gtx->soundingBUTTON);
      LUI_ButtonPadDestroy( gtx->radiopadBUTTON );
   }
   gtx->radiopadBUTTON = LUI_ButtonPadOpenType( "Modes",  /* name */
            gtx->CpWindow,             /* parent window */
            LUI_BUTTON_RADIO,          /* type */
            0, gtx->CpHeight,          /* position */
            BW,                        /* border size */
            0x000000                   /* background color */
          );
   gtx->normalBUTTON  = LUI_ButtonCreateType("Normal", 0,0, RADIOSIZE, 
                                             (index+1) * MM_MAX + MM_NORMAL, (LUI_FNCP) mode_cb);
   gtx->normalBUTTON->context_index = (index+1) * MM_MAX + MM_NORMAL;
   LUI_ButtonState( gtx->normalBUTTON, 1 ); 

   if (gtx->total_ctx_numtimes>1 && gtx->how_many_regular_contexts > 0) {
      gtx->trajectoryBUTTON = LUI_ButtonCreateType( "Trajectory", 0,1, RADIOSIZE,
                            (index+1) * MM_MAX + MM_TRAJ, (LUI_FNCP) mode_cb );
      gtx->trajectoryBUTTON->context_index = (index+1) * MM_MAX + MM_TRAJ;
   }
   else {
      gtx->trajectoryBUTTON = LUI_ButtonCreateType( " ", 0,1, RADIOSIZE,
                                            (index+1) * MM_MAX + MM_TRAJ,  NULL );
   }

   if (gtx->how_many_regular_contexts > 0) {
      gtx->sliceBUTTON =  LUI_ButtonCreateType("Slice", 0,2, RADIOSIZE,
                            (index+1)*MM_MAX+MM_SLICE, (LUI_FNCP) mode_cb );
   }
   else{
      gtx->sliceBUTTON =  LUI_ButtonCreateType(" ",  0,2, RADIOSIZE,
                            (index+1)*MM_MAX+MM_SLICE, NULL);
   }

   gtx->labelBUTTON =  LUI_ButtonCreateType("Label", 0,3, RADIOSIZE,
                         (index+1)*MM_MAX+MM_LABEL, (LUI_FNCP) mode_cb );
   /* Comment out the next statement to remove "Probe" option. */
   if (gtx->how_many_regular_contexts > 0) {
      gtx->probeBUTTON =   LUI_ButtonCreateType("Probe", 0,4, RADIOSIZE,
                            (index+1)*MM_MAX+MM_PROBE, (LUI_FNCP) mode_cb );
      gtx->soundingBUTTON = LUI_ButtonCreateType("Sounding", 0,5, RADIOSIZE,
                            (index+1)*MM_MAX+MM_SOUND, (LUI_FNCP) mode_cb );
   }
   else{
      gtx->probeBUTTON =   LUI_ButtonCreateType(" ", 0,4, RADIOSIZE,
                            (index+1)*MM_MAX+MM_PROBE, NULL);
      gtx->soundingBUTTON = LUI_ButtonCreateType(" ", 0,5, RADIOSIZE,
                            (index+1)*MM_MAX+MM_SOUND, NULL);
   }

   gtx->clippingBUTTON = LUI_ButtonCreateType("Clipping", 0,6, RADIOSIZE,
                         (index+1)*MM_MAX+MM_CLIP, (LUI_FNCP) mode_cb );

   gtx->sliceBUTTON->context_index = (index+1)*MM_MAX+MM_SLICE;
   gtx->labelBUTTON->context_index = (index+1)*MM_MAX+MM_LABEL;
   gtx->probeBUTTON->context_index = (index+1)*MM_MAX+MM_PROBE;
   gtx->soundingBUTTON->context_index = (index+1)*MM_MAX+MM_SOUND;
   gtx->clippingBUTTON->context_index = (index+1)*MM_MAX+MM_CLIP;

   LUI_ButtonPadClose( "Modes" );
   LUI_ButtonPadVisible( "Modes", 1 );
   LUI_ButtonPadQuery( "Modes", &x, &y, &w, &h );

   if (gtx->ModeInfoLabel){
      LUI_LabelDestroy( gtx->ModeInfoLabel);
   }
   gtx->ModeInfoLabel = LUI_LabelOpen ( "ModeInfo",   /* name */
             6, ModeInfo1,       /* text strings */
             gtx->CpWindow,           /* parent window */
             w, gtx->CpHeight,        /* origin relative to parent */
             CP_WIDTH-w, h,      /* dimensions */
             BW,                 /* border size */
             NULL                /* callback function */
             );
   LUI_LabelClose("ModeInfo");
   LUI_LabelVisible("ModeInfo", 1);
   LUI_LabelHighlight("ModeInfo", 1);
   LUI_LabelQuery("ModeInfo", &x, &y, &w, &h);
   gtx->CpHeight += h;


   if( gtx->hwind1BUTTON){
      LUI_NewButtonDestroy(gtx->hwind1BUTTON);
      LUI_NewButtonDestroy(gtx->vwind1BUTTON);
      LUI_NewButtonDestroy(gtx->hstreamBUTTON);
      LUI_NewButtonDestroy(gtx->hwind2BUTTON);
      LUI_NewButtonDestroy(gtx->vwind2BUTTON);
      LUI_NewButtonDestroy(gtx->vstreamBUTTON);
   }
   if (gtx->colheading){
      LUI_NewLabelDestroy(gtx->colheading);
   }
   if (gtx->ButtonMatrix){
      LUI_ButtonMatrixDestroy(gtx->ButtonMatrix);
   }
   
   /***********************************************************/
   /* Make wind slice buttons                                 */
   /***********************************************************/
   if (gtx->how_many_regular_contexts >= 1) {
      Window w = gtx->CpWindow;
      LUI_NEWBUTTON *b;
      float red, green, blue, alpha;
      int spandex;


      LUI_FrameWidth( 2 );
            
        
      gtx->hwind1BUTTON = LUI_ToggleButtonCreate( w, 3, gtx->CpHeight, 58, 22, "Hwind1" );
      LUI_ButtonCallback(gtx->hwind1BUTTON, hwind_cb );
      LUI_ButtonIndex(gtx->hwind1BUTTON, 0 );
      gtx->hwind1BUTTON->context_index = index;
      vis5d_get_color( gtx->context_index, VIS5D_HWIND, 0, &red, &green, &blue, &alpha );
      LUI_ButtonColor(gtx->hwind1BUTTON, red, green, blue );
      gtx->hwind_button[0] =gtx->hwind1BUTTON;

      gtx->vwind1BUTTON  = LUI_ToggleButtonCreate( w, LUI_NEXT_X, LUI_SAME_Y, 58, 22, "Vwind1");
      LUI_ButtonCallback(gtx->vwind1BUTTON , vwind_cb );
      LUI_ButtonIndex( gtx->vwind1BUTTON, 0 );
      gtx->vwind1BUTTON->context_index = index;
      vis5d_get_color( gtx->context_index, VIS5D_VWIND, 0, &red, &green, &blue, &alpha );
      LUI_ButtonColor( gtx->vwind1BUTTON, red, green, blue );
      gtx->vwind_button[0] = gtx->vwind1BUTTON;

      gtx->hwind2BUTTON = LUI_ToggleButtonCreate( w, LUI_NEXT_X, LUI_SAME_Y, 58, 22, "Hwind2");
      LUI_ButtonCallback( gtx->hwind2BUTTON, hwind_cb );
      LUI_ButtonIndex( gtx->hwind2BUTTON, 1 );
      gtx->hwind2BUTTON->context_index = index;
      vis5d_get_color(gtx->context_index, VIS5D_HWIND, 1, &red, &green, &blue, &alpha );
      LUI_ButtonColor( gtx->hwind2BUTTON, red, green, blue );
      gtx->hwind_button[1] = gtx->hwind2BUTTON;

      gtx->vwind2BUTTON= LUI_ToggleButtonCreate( w, LUI_NEXT_X, LUI_SAME_Y, 58, 22, "Vwind2");
      LUI_ButtonCallback( gtx->vwind2BUTTON, vwind_cb );
      LUI_ButtonIndex( gtx->vwind2BUTTON, 1 );
      gtx->vwind2BUTTON->context_index = index;
      vis5d_get_color(gtx->context_index, VIS5D_VWIND, 1, &red, &green, &blue, &alpha );
      LUI_ButtonColor( gtx->vwind2BUTTON, red, green, blue );
      gtx->vwind_button[1] = gtx->vwind2BUTTON;

      /* MJK 12.04.98 begin*/
      gtx->hstreamBUTTON = LUI_ToggleButtonCreate( w, LUI_NEXT_X, LUI_SAME_Y, 65, 22,"HStream");
      LUI_ButtonCallback( gtx->hstreamBUTTON, hstream_cb );
      LUI_ButtonIndex( gtx->hstreamBUTTON, 0 );
      gtx->hstreamBUTTON->context_index = index;
      vis5d_get_color( gtx->context_index, VIS5D_HSTREAM, 0, &red, &green, &blue, &alpha );
      LUI_ButtonColor( gtx->hstreamBUTTON, red, green, blue );
      gtx->hstream_button[0] = gtx->hstreamBUTTON;
      /* MJK 12.04.98 end */
  
      gtx->vstreamBUTTON = LUI_ToggleButtonCreate( w, LUI_NEXT_X, LUI_SAME_Y, 65, 22,"VStream");
      LUI_ButtonCallback( gtx->vstreamBUTTON, vstream_cb );
      LUI_ButtonIndex( gtx->vstreamBUTTON, 0 );
      gtx->vstreamBUTTON->context_index = index;
      vis5d_get_color( gtx->context_index, VIS5D_VSTREAM, 0, &red, &green, &blue, &alpha );
      LUI_ButtonColor( gtx->vstreamBUTTON, red, green, blue );
      gtx->vstream_button[0] = gtx->vstreamBUTTON;


      /**************************************************/
      /* Column headings                                */
      /**************************************************/

      if (volflag) {
         /* 6 column headings */
         gtx->colheading = LUI_NewLabelCreate( gtx->CpWindow, 0, LUI_NEXT_Y, 380, 28,
                             "           Contour Slice  Colored Slice\n"
                             "  Isosurf  Horiz.  Vert.  Horiz.  Vert.  Volume" );
      }
      else {
         /* 5 column headings */
         gtx->colheading = LUI_NewLabelCreate( gtx->CpWindow, 0, LUI_NEXT_Y, 380, 28,
                             "             Contour Slices     Colored Slices\n"
                             "   Isosurf   Horiz.    Vert.    Horiz.   Vert." );
      }

      /**************************************************/
      /* Button Matrix                                  */
      /**************************************************/
      gtx->Columns = volflag ? 6 : 5;
      gtx->ButtonMatrixTop = LUI_LayoutGet( LUI_NEXT_Y );
      gtx->ButtonMatrixWidth = 378;
      gtx->ButtonMatrixHeight = 172;
      gtx->ButtonMatrix = LUI_ButtonMatrixCreate( gtx->CpWindow, 3, LUI_NEXT_Y,
                               gtx->ButtonMatrixWidth, gtx->ButtonMatrixHeight,
                               gtx->Columns );
      gtx->ButtonMatrix->context_index = gtx->context_index;
      for (yo = 0; yo < gtx->how_many_regular_contexts; yo++){

         spandex  = gtx->array_of_ctxs[yo];
    
         for (i=0; i<gtx->RegularNumVars[spandex]; i++) {
            float red[6], green[6], blue[6], alpha;
            char *labels[6], jam[5];
            int type;
            yo2 = 0;
            if ( gtx->how_many_regular_contexts > 1){
               while (gtx->RegularVarName[i][spandex][yo2] != NULL){
                  yo2++;
               }
               if ((yo2 >= 2 && gtx->RegularVarName[i][spandex][yo2-1] != '.' &&
                                gtx->RegularVarName[i][spandex][yo2-2] != '.' &&
                                gtx->RegularVarName[i][spandex][yo2]   != '.') ||
                                yo2 < 2){
                  gtx->RegularVarName[i][spandex][yo2] = '.';
                  gtx->RegularVarName[i][spandex][yo2+1] = NULL;
                  sprintf( jam,"%d", spandex);
                  strcat( gtx->RegularVarName[i][spandex], jam);
               }
            }
            for (j=0; j<gtx->Columns; j++) {
               if (j==5) {
                  /* volume button always white */
                  red[j] = green[j] = blue[j] = 1.0;
               }
               else {
                  if (j == 0) type = VIS5D_ISOSURF;
                  else if (j == 1) type = VIS5D_HSLICE;
                  else if (j == 2) type = VIS5D_VSLICE;
                  else if (j == 3) type = VIS5D_CHSLICE;
                  else if (j == 4) type = VIS5D_CVSLICE;
                  else if (j == 5) type = VIS5D_VOLUME;
                  vis5d_get_color( gtx->context_index, type, spandex*MAXVARS+i, &red[j], &green[j],
                                   &blue[j], &alpha );
               }
               if (gtx->NumLevels[spandex][i]>1 || (j==1 || j==3)) {
                  labels[j] = gtx->RegularVarName[i][spandex] ;
               }
               else {
                  labels[j] = NULL;
               }
            }
            LUI_ButtonMatrixAddRow( gtx->ButtonMatrix, labels, red, green, blue );
         }
      }
      XMapWindow( LUI_Display, gtx->ButtonMatrix->mainwindow );
      gtx->CpHeight = gtx->ButtonMatrixTop + gtx->ButtonMatrixHeight;

      if (gtx->how_many_regular_contexts>=1){
         /* XMapWindow( LUI_Display, gtx->CpWindow); */
          /* XMoveResizeWindow(GuiDpy, gtx->CpWindow, 10, 32, CP_WIDTH, gtx->CpHeight); */
      }
      else{
          /* XUnmapWindow( LUI_Display, gtx->CpWindow); */
      }

      LUI_ButtonMatrixCallback( gtx->ButtonMatrix, button_matrix_cb );

      /* Finish making the control panel. */
      /* MJK 12.04.98 */
      if (geom_ht > 0) gtx->CpHeight = geom_ht;

      if (proceed){
         LUI_EventAdd ( gtx->CpWindow, ExposureMask | ButtonPressMask
                    | StructureNotifyMask , (LUI_FNCP) controlpanel_cb);
      }

      /* LUI_MoveResizeWindow( gtx->CpWindow, gtx->cpx, gtx->cpy, CP_WIDTH, gtx->CpHeight ); */

      bottom = gtx->CpHeight + 2*SPACE;


      /************************************************************/
      /* Make wind slice density & scale window.                  */
      /************************************************************/

      if (!gtx->WindWindow){
         gtx->WindWindow = LUI_CreateWindowAt(LUI_RootWindow,gtx->cpx,bottom,380,32);

         gtx->windscale_label = LUI_NewLabelCreate(gtx->WindWindow,LUI_LEFT,LUI_TOP,
                                                  130, 26, "nil" );
         gtx->windscale_field = LUI_FieldCreate(gtx->WindWindow,LUI_NEXT_X,
                                                LUI_SAME_Y, 80, LUI_SAME_H );
         LUI_FieldCallback( gtx->windscale_field, windscale_cb );
         gtx->windscale_field->context_index = gtx->context_index;

         gtx->winddensity_label = LUI_NewLabelCreate( gtx->WindWindow,
                                                     LUI_NEXT_X, LUI_SAME_Y,
                                                     70, LUI_SAME_H, "Density:");
         gtx->winddensity_field = LUI_FieldCreate( gtx->WindWindow,
                                                   LUI_NEXT_X, LUI_SAME_Y,
                                                   80, LUI_SAME_H );
         LUI_FieldCallback( gtx->winddensity_field, winddensity_cb );
         gtx->winddensity_field->context_index = gtx->context_index;

         XUnmapWindow( GuiDpy, gtx->WindWindow );

         bottom += /*WindHeight*/ 32 + SPACE;


         /* MJK 12.04.98 begin */
         h = 32 + 40;
         gtx->HWindWindow = LUI_CreateWindowAt (LUI_RootWindow,
                                                gtx->cpx, bottom, 380, h);

         gtx->hwindscale_label = LUI_NewLabelCreate (gtx->HWindWindow,
                                                     LUI_LEFT, LUI_TOP,
                                                     130, 26, "nil");
         gtx->hwindscale_field = LUI_FieldCreate (gtx->HWindWindow,
                                                  LUI_NEXT_X, LUI_SAME_Y,
                                                  80, LUI_SAME_H);
         LUI_FieldCallback (gtx->hwindscale_field, windscale_cb);
         gtx->hwindscale_field->context_index = gtx->context_index;

         gtx->hwinddensity_label = LUI_NewLabelCreate (gtx->HWindWindow,
                                                       LUI_NEXT_X, LUI_SAME_Y,
                                                       70, LUI_SAME_H, "Density:");
         gtx->hwinddensity_field = LUI_FieldCreate (gtx->HWindWindow,
                                                    LUI_NEXT_X, LUI_SAME_Y,
                                                    80, LUI_SAME_H);
         LUI_FieldCallback (gtx->hwinddensity_field, winddensity_cb);
         gtx->hwinddensity_field->context_index = gtx->context_index;

         gtx->hwind_pos_slider = LUI_NewSliderCreate (gtx->HWindWindow,
                                                      2, 32, CP_WIDTH-49);
         LUI_NewSliderCallback (gtx->hwind_pos_slider, hwind_pos_cb);

         /* MJK 12.08.98 */
         gtx->hwind_pos_slider->context_index = gtx->context_index;


         gtx->hwind_sfc_button = LUI_ToggleButtonCreate (gtx->HWindWindow,
                                                         LUI_NEXT_X, LUI_SAME_Y,
                                                         36, 36, "SFC");
         LUI_ButtonCallback (gtx->hwind_sfc_button, sfc_hwind_cb);

         /* MJK 12.08.98 */
         gtx->hwind_sfc_button->context_index = gtx->context_index;

         XUnmapWindow( GuiDpy, gtx->WindWindow );

         bottom += /*WindHeight*/ h + SPACE;
         /* MJK 12.04.98 end */  


         /******************************************************************/
         /* create 3-D iso-level widget(s)                                 */
         /******************************************************************/

         gtx->IsoWindow = LUI_CreateWindow( LUI_RootWindow, gtx->cpx, gtx->cpy );

         gtx->IsoSlider = LUI_NewSliderCreate( gtx->IsoWindow, 2, 2, CP_WIDTH-37 );
         LUI_NewSliderCallback( gtx->IsoSlider, isosurface_cb );
         gtx->IsoSlider->context_index = gtx->context_index;

         gtx->IsosurfWidth = CP_WIDTH;
         gtx->IsosurfHeight = 40;

         b = LUI_PushButtonCreate( gtx->IsoWindow, LUI_NEXT_X, LUI_SAME_Y, 30, 36,
                                   "OK");
         LUI_ButtonCallback( b, newsurf_cb );
         b->context_index = gtx->context_index;

         LUI_MoveResizeWindow( gtx->IsoWindow, gtx->cpx, bottom,
                               gtx->IsosurfWidth, gtx->IsosurfHeight );

         bottom += gtx->IsosurfHeight + SPACE;



         /******************************************************************/
         /* create horizontal contour line interval widget                 */
         /******************************************************************/
         /* MJK 12.04.98 begin */
         h = 32 + 40;
         gtx->HSliceWindow = LUI_CreateWindowAt( LUI_RootWindow, gtx->cpx,
                                                 bottom, 380, h );

         gtx->hslice_label = LUI_NewLabelCreate( gtx->HSliceWindow, LUI_LEFT,
                                                 LUI_TOP, 230, 26, "nil" );
         gtx->hslice_field = LUI_FieldCreate( gtx->HSliceWindow,
                                              LUI_NEXT_X, LUI_SAME_Y,
                                              140, LUI_SAME_H );
         LUI_FieldCallback( gtx->hslice_field, hslice_cb );
         gtx->hslice_field->context_index = gtx->context_index;

         gtx->hslice_pos_slider = LUI_NewSliderCreate (gtx->HSliceWindow,
                                                       2, 32, CP_WIDTH-49);
         LUI_NewSliderCallback (gtx->hslice_pos_slider, hslice_pos_cb);

         gtx->hslice_pos_slider->context_index = gtx->context_index;

         gtx->hslice_sfc_button = LUI_ToggleButtonCreate( gtx->HSliceWindow,
                                                          LUI_NEXT_X, LUI_SAME_Y,
                                                    36, 36, "SFC");
         LUI_ButtonCallback( gtx->hslice_sfc_button, sfc_hslice_cb );
         /* FIRST THING */
         gtx->hslice_field->context_index = gtx->context_index;
         /* MJK 12.08.98 */                                                 
         gtx->hslice_sfc_button->context_index = gtx->context_index;

         bottom += h + SPACE;
         /* MJK 12.04.98 end */
         

         /******************************************************************/
         /* create vertical contour line interval widget                   */
         /******************************************************************/
         gtx->VSliceWindow = LUI_CreateWindowAt( LUI_RootWindow, gtx->cpx,
                                                 bottom, 380, 32 );

         gtx->vslice_label = LUI_NewLabelCreate( gtx->VSliceWindow, LUI_LEFT,
                                                 LUI_TOP, 230, 26, "nil" );
         gtx->vslice_field = LUI_FieldCreate( gtx->VSliceWindow, LUI_NEXT_X,
                                              LUI_SAME_Y, 140, LUI_SAME_H );
         LUI_FieldCallback( gtx->vslice_field, vslice_cb );
         gtx->vslice_field->context_index = gtx->context_index;

         bottom += /*VSliceHeight*/ 32 + SPACE;


         /***************************************************************/
         /* Make the colorbar widget window                             */
         /***************************************************************/
         /* MJK 12.04.98 begin */
         h = 160 + 20 + 8;
         if (bottom+gtx->RGBHeight+SPACE>ScrHeight)
            bottom = ScrHeight - gtx->RGBHeight - SPACE;

         gtx->ColorbarWindow = LUI_CreateWindowAt (LUI_RootWindow, 10, bottom,
                                                   CP_WIDTH, h);

         b = LUI_PushButtonCreate (gtx->ColorbarWindow, CP_WIDTH-60-4, LUI_TOP,
                                   60, 20, "Close");
         LUI_ButtonCallback (b, cb_close_cb);
         LUI_ButtonIndex (b, 0);
         b->context_index = gtx->context_index;

         gtx->Colorbar = LUI_ColorBarCreate (gtx->ColorbarWindow,
                                             0, LUI_NEXT_Y,
                                             CP_WIDTH, 160 );
         LUI_ColorBarCallback( gtx->Colorbar, colorbar_cb );
         LUI_BorderWidth( 2 );
         bottom += gtx->RGBHeight + SPACE;
         gtx->Colorbar->context_index = gtx->context_index;

         /* CHSlice colorbar */
         h = 160 + 40 + 20 + 8;
         if (bottom+gtx->RGBHeight+SPACE>ScrHeight)
            bottom = ScrHeight - gtx->RGBHeight - SPACE;

         gtx->CHSliceWindow = LUI_CreateWindowAt (LUI_RootWindow, 10, bottom,
                                                  CP_WIDTH, h);

         b = LUI_PushButtonCreate (gtx->CHSliceWindow, CP_WIDTH-60-4, LUI_TOP,
                                   60, 20, "Close");
         LUI_ButtonCallback (b, cb_close_cb);
         LUI_ButtonIndex (b, 1);
         b->context_index = gtx->context_index;

         gtx->CHSliceColorbar = LUI_ColorBarCreate (gtx->CHSliceWindow,
                                                    0, LUI_NEXT_Y,
                                                    CP_WIDTH, 160);
         LUI_ColorBarCallback( gtx->CHSliceColorbar, colorbar_cb );
         gtx->CHSliceColorbar->context_index = gtx->context_index;

         LUI_BorderWidth (2);

         gtx->chslice_pos_slider = LUI_NewSliderCreate (gtx->CHSliceWindow,
                                                        2, LUI_NEXT_Y, CP_WIDTH-6);
         /* MJK 12.08.98 */
         LUI_NewSliderCallback (gtx->chslice_pos_slider, chslice_pos_cb);
         gtx->chslice_pos_slider->context_index = gtx->context_index;
         /* MJK 12.04.98 end */ 
      }
      make_trajectory_window(index);
      make_isocolor_window( gtx );
   }


   if (gtx->Irregular_Heading){
      LUI_NewLabelDestroy(gtx->Irregular_Heading);
   }
   if (gtx->IrregularButtonMatrix){
      LUI_ButtonMatrixDestroy(gtx->IrregularButtonMatrix);
   }

   LUI_BorderWidth(2);
   if (gtx->how_many_irregular_contexts >= 1){
      char *labels[3];
      char *tlabels[1];
      float r,g,b;

      gtx->Irregular_Heading = LUI_NewLabelCreate(
               gtx->CpWindow, 0, gtx->CpHeight, 380, 28,
               "      Text          Symbol        Vertical  \n"
               "      Plot           Plot           Plot    ");

      gtx->IrregularButtonMatrixWidth = 378;
      gtx->IrregularButtonMatrixHeight = 50;
      gtx->IrregularColumns = 3;
      gtx->IrregularButtonMatrixTop = LUI_LayoutGet( LUI_NEXT_Y );
      /**********************************/
      /* Create Irregular Button Matrix */
      /**********************************/
      gtx->IrregularButtonMatrix = LUI_ButtonMatrixCreate(
                      gtx->CpWindow, 3, LUI_NEXT_Y, 
                      gtx->IrregularButtonMatrixWidth,
                      gtx->IrregularButtonMatrixHeight,
                      gtx->IrregularColumns);

      gtx->IrregularButtonMatrix->context_index = gtx->context_index;
      for (yo = 0; yo < gtx->how_many_irregular_contexts; yo++){
         int s=0;
         char j[5];
         spandex  = gtx->array_of_itxs[yo];
         if (gtx->how_many_irregular_contexts > 1){
            while (gtx->FileName[yo][s] != NULL){
               s++;
            }
            if ((s >= 2 && gtx->FileName[yo][s-1] != '.' &&
                           gtx->FileName[yo][s-2] != '.' &&
                           gtx->FileName[yo][s]   != '.') ||
                           s < 2){
               gtx->FileName[yo][s] = '.';
               gtx->FileName[yo][s+1] = NULL;
               sprintf( j, "%d", spandex);
               strcat( gtx->FileName[yo], j);
            }
         }
         for (s = 0; s < 3; s++){
            labels[s] = gtx->FileName[yo];
         }
         r = g = b = 1.0;
         LUI_ButtonMatrixAddRow( gtx->IrregularButtonMatrix, labels,
                                 &r, &g, &b);
      }
      XMapWindow( LUI_Display, gtx->IrregularButtonMatrix->mainwindow);
      gtx->CpHeight = gtx->IrregularButtonMatrixTop + 
                      gtx->IrregularButtonMatrixHeight;

      LUI_ButtonMatrixCallback( gtx->IrregularButtonMatrix,
                                irregular_matrix_cb );
      gtx->IrregularButtonMatrix->context_index = gtx->context_index;

      make_tp_color_window( gtx );
   } 

   /***************************************************************/
   /* Make map window                                             */
   /***************************************************************/
   h = 40;
   bottom = LUI_LayoutGet( LUI_NEXT_Y )+450; 
   gtx->MapWindow = LUI_CreateWindowAt(LUI_RootWindow,gtx->cpx,bottom,380,h);

   gtx->map_pos_slider = LUI_NewSliderCreate (gtx->MapWindow,
                                              2, LUI_TOP, CP_WIDTH-49);

   LUI_NewSliderCallback (gtx->map_pos_slider, map_pos_cb);
   gtx->map_pos_slider->context_index = gtx->context_index;

   gtx->map_sfc_button = LUI_ToggleButtonCreate (gtx->MapWindow,
                                                 LUI_NEXT_X, LUI_SAME_Y,
                                                 36, 36, "SFC");
   LUI_ButtonCallback( gtx->map_sfc_button, sfc_map_cb );

   /* MJK 12.08.98 */
   gtx->map_pos_slider->context_index = gtx->context_index;
   gtx->map_sfc_button->context_index = gtx->context_index;


   XUnmapWindow( GuiDpy, gtx->MapWindow );

   if (!gtx->VerifyWindow){
      make_verify_window(index);
      make_OK_window( index );
      make_alert_window(index);
      make_save_window(index);
      make_savefile_window(index);
      make_restore_window(index);
      make_savepic_window(index);
      make_expression_window(index);
      make_rgb_sliders( gtx );
      make_uvw_widget( gtx );
      make_tp_color_window( gtx );
   }

}
 



void set_anim_rate( int index, int rate)
{
   GuiContext gtx = get_gui_gtx(index);
   gtx->AnimRate = rate;
}

/* newgui = 0  use this when exiting from displaywidget.c   */
/*               this keeps the same button matrix settings */
/*             and display mode  and control buttons but    */
/*             adds on extra buttons if another data set was*/
/*             added to the display.  Or use this after a   */
/*             new button, clone, ext function etc..        */
/*                                                          */
/* newgui = 1  use this after running a script, interpreter */
/*             to make sure correct sliders, color curves and*/
/*             windows are mapped                           */
int make_another_gui(int index, int newgui)
{
   GuiContext gtx = get_gui_gtx2(index);
   int yo, spandex, chowmany, cwhichones[VIS5D_MAX_CONTEXTS];
   int ihowmany, iwhichones[VIS5D_MAX_CONTEXTS];
   int Kur, volrender;
   static GC gc;
   XGCValues vals;

   
   /*******************************/
   /* get number of itxs and ctxs */
   /*******************************/
   vis5d_get_num_of_ctxs_in_display( index, &chowmany, cwhichones);
   vis5d_get_num_of_itxs_in_display( index, &ihowmany, iwhichones);

   /********************************/
   /* make gtx if not already made */
   /********************************/
   if (gtx){
      gtx->how_many_regular_contexts = chowmany;
      gtx->how_many_irregular_contexts = ihowmany;
   }
   if (chowmany + ihowmany < 1){
      return 0;
   }
   if (!gtx){
      newgui = 1;
      gtx = create_gui_context(index);
   }
   gtx->othersnddpy = gtx_table[0]->othersnddpy;
   if (newgui){
      gtx->MouseMode = MM_NORMAL;
      vis5d_graphics_mode(index, VIS5D_CURSOR, VIS5D_OFF);
      vis5d_graphics_mode(index, VIS5D_PROBE, VIS5D_OFF);
      vis5d_graphics_mode(index, VIS5D_SOUND, VIS5D_OFF);
      vis5d_graphics_mode(index, VIS5D_CLIP, VIS5D_OFF);
      if (gtx->ModeInfoLabel){
         LUI_LabelChangeText( gtx->ModeInfoLabel, 6, ModeInfo1 );
      }
      gtx->uvw_map = 0;
   }
   vis5d_check_dtx_volume( index, &volrender);
   /* MJK 12.04.98 */
   if(GuiDpy==GfxDpy){
      make_gui( index, "Johan", NULL, NULL, volrender);
   }
   else{
      make_gui( index, "Johan", "Hello", NULL, volrender);
   }
   if (newgui){
      gtx->AnimRate = 100;
      gtx->size_of_logo = 1.0; 
      if (gtx_table[0]->funcpath){
         strcpy(gtx->funcpath, gtx_table[0]->funcpath);
      }
   }
   if (gtx->how_many_regular_contexts >0){
      gtx->othersnddpy = gtx_table[0]->othersnddpy;
      if (gtx->othersnddpy){
         vis5d_init_sndwindow( index, "Skew-T and Vertical Plot Displsy",
                            15, 15, 10, 10, gtx->SoundCtrlWindow, "hello");
      }
      else{
         vis5d_init_sndwindow( index, "Skew-T and Vertical Plot Displsy",
                            15, 15, 10, 10, gtx->SoundCtrlWindow, NULL);
      }
      if (gtx->othersnddpy ){
         vis5d_resize_sounding_window( gtx->context_index, 630, 600,
                                      gtx->cpx, 15);
      }
   }
   get_current_display( &Kur);
   if (Kur == index ){
      XMoveResizeWindow(GuiDpy, gtx->CpWindow,
      10, 32, CP_WIDTH, gtx->CpHeight);
      XMapWindow(GuiDpy, gtx->CpWindow);
   }
   if (newgui==1){
      update_button_states( index, 1);
   }
   else{
      update_button_states( index, 0);
   }
   return 0;
}

int gui_set_display_border_color(int index, int R, int G, int B)
{
#ifndef NO_BORDERS
   GuiContext gtx = get_gui_gtx(index);
   unsigned long bumber;

   XSync(GuiDpy, 0);
   bumber = SND_AllocateColorInt(R, G, B);
   XSetWindowBorder( GuiDpy, gtx->fakewin, bumber);
   return 0;
#endif
}

/* MJK 11.17.98 */
void make_watch_cursor( void )
{
   if (GuiBigWin){
      XDefineCursor(GuiDpy, GuiBigWin, XCreateFontCursor(GuiDpy, XC_watch));
   }
   else{
      XDefineCursor(GuiDpy, BigWindow, XCreateFontCursor(GuiDpy, XC_watch));
   }
}
void unmake_watch_cursor( void )
{
   if (GuiBigWin){
      XUndefineCursor(GuiDpy, GuiBigWin);
   }
   else{
      XUndefineCursor(GuiDpy, BigWindow);
   }
}




int map_fake_windows( int onlyrecolor)
{
   int x, y, yo, wstep, hstep, width, height;
   int Kurrant;
   XWindowAttributes winatts;
   GuiContext gtx;
   int R,G,B;
   int DRs, DCs;
   yo = 0;

   if (!GuiBigWin){
      return 0;
   }
   get_current_display( &Kurrant );
   XGetWindowAttributes( GfxDpy, BigWindow, &winatts);
   get_display_matrix( &DRs, &DCs);
   for (y = 0; y < DRs; y++){
      for (x = 0; x < DCs; x++){
         get_display_border_color( yo, &R, &G, &B);
         width =   winatts.width;
         height =  winatts.height;
         gtx = get_gui_gtx2(yo);
         if (gtx){
            if (DRs == 1 && DCs == 1){
               wstep = width-8;
               hstep = height-8;
            }
            else{
               wstep = ((width-4) -(DCs*8)) / DCs;
               hstep = ((height-4) -(DRs*8))/ DRs;
            }
            wstep = wstep > hstep ? hstep : wstep;
            hstep = hstep > wstep ? wstep : hstep;
            if (DRs == 1 && DCs == 1){
               gui_set_display_border_color(0, 0, 0, 0 );
            }
            else if (gtx->group_index >= 1){
               switch(gtx->group_index){
                  case 1:
                     gui_set_display_border_color( yo, 255, 0, 255);
                     break;
                  case 2:
                     gui_set_display_border_color( yo, 255, 255, 0 );
                     break;
                  case 3:
                     gui_set_display_border_color( yo, 0, 255, 255);
                     break;
                  case 4:
                     gui_set_display_border_color( yo, 255, 180, 50);
                     break;
                  case 5:
                     gui_set_display_border_color( yo, 180, 50, 255);
                     break;
                  case 6:
                     gui_set_display_border_color( yo, 50, 255, 180);
                     break;
                  case 7:
                     gui_set_display_border_color( yo, 255, 50, 180);
                     break;
                  case 8:
                     gui_set_display_border_color( yo, 180, 255, 50);
                     break;
                  case 9:
                     gui_set_display_border_color( yo, 50, 180, 255);
                     break;
                  default: printf("something is awry\n");
               }
            }
            else if (gtx->context_index == Kurrant){
               gui_set_display_border_color(yo, 255, 255, 255);
            }
            else {
               gui_set_display_border_color(yo, 165, 42, 42);
            }
            if (onlyrecolor == 0){
               XMapWindow(GuiDpy, gtx->fakewin);
               XMoveResizeWindow(GuiDpy, gtx->fakewin, x * 8 + 4 + x * wstep,
                             y * 8 + 4 + y * hstep, wstep, hstep);
               gtx->fakewidth = wstep;
               gtx->fakeheight = hstep;
            }
         }
         yo++;
      }
   }
   if (DRs == 1 && DCs == 1){
      XResizeWindow(GuiDpy, GuiBigWin,
      (wstep+8)*DCs, (hstep+8)*DRs);
   }
   else{
      XResizeWindow(GuiDpy, GuiBigWin,
      (wstep+8)*DCs+4, (hstep+8)*DRs+4);
   }
/*
   if (DRs == 1 && DCs == 1){
      XMoveResizeWindow(GuiDpy, GuiBigWin, winatts.x, winatts.y,
      (wstep+8)*DCs, (hstep+8)*DRs);
   }
   else{
      XMoveResizeWindow(GuiDpy, GuiBigWin, winatts.x, winatts.y,
      (wstep+8)*DCs+4, (hstep+8)*DRs+4);
   }
*/
}

int make_gui_BigWin( char *wdpy_name)
{
   XSizeHints hints;
   XSetWindowAttributes attr;
   int attr_flags;
   int fakex, fakey;
   char programname[128];
   int xpos = 410;
   int ypos = 10;
   
   sprintf(programname, "Vis5d Control Panel");
   
   GuiDpy = XOpenDisplay( wdpy_name );
   if (!GuiDpy) {
      printf("Unable to open display %s\n", wdpy_name );
      exit(1);
   }
   GuiScr = DefaultScreen( GuiDpy );
   find_best_visual( GuiDpy, GuiScr, &GuiDepth, &GuiVisual, &GuiColormap );

   /* create fake 3-D window on widget display so user can rotate */
   /* or interact with the 3-D image using the widget display's mouse */

   attr.colormap = GuiColormap;
   attr.event_mask = ExposureMask | PointerMotionMask | KeyPressMask
             | ButtonPressMask | ButtonReleaseMask | StructureNotifyMask;
   attr.background_pixel = BlackPixel( GfxDpy, GfxScr );
   attr.border_pixel = BlackPixel( GfxDpy, GfxScr );
   attr_flags = CWColormap | CWEventMask | CWBackPixel | CWBorderPixel;
   fakex = 410;
   fakey = 10;
   GuiBigWinWidth = DisplayWidth(GuiDpy, DefaultScreen(GuiDpy)) - 420;
   GuiBigWinHeight= GuiBigWinWidth;

   GuiBigWin = XCreateWindow( GuiDpy, RootWindow(GuiDpy, GuiScr),
                                  fakex, fakey, GuiBigWinWidth, GuiBigWinHeight,
                                 0, GuiDepth, InputOutput, GuiVisual,
                                 attr_flags, &attr );
   XSelectInput( GuiDpy, GuiBigWin, ExposureMask | ButtonMotionMask
                | KeyPressMask | KeyReleaseMask | ButtonPressMask | ButtonReleaseMask
                | StructureNotifyMask | SubstructureNotifyMask
                | VisibilityChangeMask );

   hints.x = xpos;
   hints.y = ypos;
   hints.flags = USPosition | PPosition;
   XSetStandardProperties( GuiDpy, GuiBigWin,
                           programname, programname,  /*title, icon*/
                           None,  /*icon_pixmap*/
                           NULL, 0,  /*arguments*/
                           &hints );
   XSelectInput( GuiDpy, GuiBigWin,
                 ButtonPressMask|ButtonReleaseMask|PointerMotionMask
                 |StructureNotifyMask );
   XMapWindow( GuiDpy, GuiBigWin );

  return 0;

}



int make_nodata_gui( int index, char *wdpy_name, char *geom_str)
{

   GuiContext gtx;
   char programname[1000];
   int xpos = 410;
   int ypos = 10;

   gtx = get_gui_gtx2( index );
   if (!gtx){
      gtx = create_gui_context(index);
   }
   gtx->fakewin = 0;
   gtx->cpx = 10;
   gtx->cpy = 10;
   /* MJK 12.14.98 */
   {
      if (geom_str != NULL)
      {
         get_window_geometry (geom_str, NULL, &gtx->CpHeight,
                              &gtx->cpx, &gtx->cpy);
      }
      else if (index > 0)
      {
         GuiContext     gtx0 = get_gui_gtx (0);
         if (gtx0){
            gtx->cpx = gtx0->cpx;
            gtx->cpy = gtx0->cpy;
         }
      }
   }
   
   gtx->p1 = 0;
   gtx->p2 = 0;
   gtx->p3 = 0;
   gtx->context_index = index;
   gtx->how_many_regular_contexts = 0;
   gtx->how_many_irregular_contexts = 0;
   
   sprintf(programname, "Vis5d Control Panel");
   LUI_ContextIndex(index);

   if (wdpy_name==NULL ) {
      /* graphics and widgets on same display */
      GuiDpy = GfxDpy;
      GuiScr = GfxScr;
      GuiVisual = GfxVisual;
      GuiDepth = GfxDepth;
      GuiColormap = GfxColormap;
      vis5d_set_pointer(index, -1, -1);

   }
   else {
      /* open second display for widgets */
      XSizeHints hints;
      XSetWindowAttributes attr;
      int attr_flags;
      int fakex, fakey;

      attr.colormap = GuiColormap;
      attr.event_mask = ExposureMask | PointerMotionMask | KeyPressMask
                | ButtonPressMask | ButtonReleaseMask | StructureNotifyMask;
      attr.background_pixel = BlackPixel( GfxDpy, GfxScr );
      attr.border_pixel = BlackPixel( GfxDpy, GfxScr );
      attr_flags = CWColormap | CWEventMask | CWBackPixel | CWBorderPixel;

      gtx->fakewin = XCreateWindow( GuiDpy, GuiBigWin,
                                     1, 1, 100, 100,
                                    0, GuiDepth, InputOutput, GuiVisual,
                                    attr_flags, &attr );
      hints.x = xpos;
      hints.y = ypos;
      hints.flags = USPosition | PPosition;
      XSetStandardProperties( GuiDpy, gtx->fakewin,
                              programname, programname,  /*title, icon*/
                              None,  /*icon_pixmap*/
                              NULL, 0,  /*arguments*/
                              &hints );
      XSelectInput( GuiDpy, GuiBigWin, ExposureMask | ButtonMotionMask
                   | KeyPressMask | KeyReleaseMask | ButtonPressMask | ButtonReleaseMask
                   | StructureNotifyMask | SubstructureNotifyMask
                   | VisibilityChangeMask );
   }
   create_nodata_widgets( index );
   return 1;
}

static int exit_on_fatal_error( Display* dpy )
{
   vis5d_terminate(1);
   return 0;
}

int make_gui( int index, char *dataset, char *wdpy_name, char *geom_str, int volflag )
{
   int xpos = 410;
   int ypos = 10;
   int soundy;
   int vertical;
   float xxmin, xxmax, yymin, yymax, zzmin, zzmax, junk1, junk2;
   float hgtlow, hgthigh;
   char programname[128];
   float vertargs[MAXLEVELS];
   GuiContext gtx = get_gui_gtx(index);
   XWindowAttributes winatts;
   int cwhichones[VIS5D_MAX_CONTEXTS], chowmany, yo;
   int iwhichones[VIS5D_MAX_CONTEXTS], ihowmany;
   static GC gc;
   XGCValues vals;
   int (*johan)( Display* );


   if(do_once){
      johan = XSetIOErrorHandler(exit_on_fatal_error);   
      init_currents();
      do_once = 0;
   }
   gtx->fakewin = 0;
   gtx->cpx = 10;
   gtx->cpy = 10;
   gtx->RGBWidth=CP_WIDTH;
   gtx->RGBHeight=200;
   {
      if (geom_str != NULL)
      {
         get_window_geometry (geom_str, NULL, &gtx->CpHeight,
                              &gtx->cpx, &gtx->cpy);
      }
      else if (index > 0)
      {
         GuiContext     gtx0 = get_gui_gtx (0);
         if (gtx0){
            gtx->cpx = gtx0->cpx;
            gtx->cpy = gtx0->cpy;
         }
      }
   }

   /************************************************************/
   /* get the number of ctxs/itxs for this display/gui context */
   /************************************************************/
   vis5d_get_num_of_ctxs_in_display( index, &chowmany, cwhichones);
   vis5d_get_num_of_itxs_in_display( index, &ihowmany, iwhichones);
   gtx->how_many_regular_contexts = chowmany;
   gtx->how_many_irregular_contexts = ihowmany;
   for (yo=0; yo<gtx->how_many_regular_contexts; yo++){
      gtx->array_of_ctxs[yo] = cwhichones[yo];
   }
   for (yo=0; yo<gtx->how_many_irregular_contexts; yo++){
      gtx->array_of_itxs[yo] = iwhichones[yo];
   }


   gtx->cur_trajset = 0;
   gtx->widget_enable = 1;
   gtx->p1 = 0;
   gtx->p2 = 0;
   gtx->p3 = 0;
   gtx->context_index = index;

   vis5d_get_dtx_vertical( index, &vertical, vertargs);
   gtx->vertsys = vertical;

   vis5d_get_box_bounds( index, &xxmin, &xxmax, &yymin, &yymax, &zzmin, &zzmax);
   vis5d_xyzPRIME_to_geo( index, 0, 0, 0.0, 0.0, zzmin, &junk1, &junk2, &hgtlow );
   vis5d_xyzPRIME_to_geo( index, 0, 0, 0.0, 0.0, zzmax, &junk1, &junk2, &hgthigh);
   if ( hgthigh < 1.0 && hgtlow < -1.0 ){
      gtx->oceanonly = 1;
   }
   else {
      gtx->oceanonly = 0;
   }

   gtx->size_of_logo = 1.0;
   sprintf(programname, "Vis5d Control Panel");


   /* set Vis5D context for all widget creates */
   LUI_ContextIndex(index);
   
   /* JOHAN added this so if won't go through the hassle of getting */
   /* a display and stuff if it's already there */

   if (wdpy_name==NULL) {
      /* graphics and widgets on same display */
      GuiDpy = GfxDpy;
      GuiScr = GfxScr;
      GuiVisual = GfxVisual;
      GuiDepth = GfxDepth;
      GuiColormap = GfxColormap;
      vis5d_set_pointer(index, -1, -1);

   }
   else {
      /* open second display for widgets */
      XSizeHints hints;
      XSetWindowAttributes attr;
      int attr_flags;
      int fakex, fakey;

      attr.colormap = GuiColormap;
      attr.event_mask = ExposureMask | PointerMotionMask | KeyPressMask
                | ButtonPressMask | ButtonReleaseMask | StructureNotifyMask;
      attr.background_pixel = BlackPixel( GfxDpy, GfxScr );
      attr.border_pixel = BlackPixel( GfxDpy, GfxScr );
      attr_flags = CWColormap | CWEventMask | CWBackPixel | CWBorderPixel;

      gtx->fakewin = XCreateWindow( GuiDpy, GuiBigWin,
                                     1, 1, 100, 100,
                                    0, GuiDepth, InputOutput, GuiVisual,
                                    attr_flags, &attr );
      hints.x = xpos;
      hints.y = ypos;
      hints.flags = USPosition | PPosition;
      XSetStandardProperties( GuiDpy, gtx->fakewin,
                              programname, programname,  /*title, icon*/
                              None,  /*icon_pixmap*/
                              NULL, 0,  /*arguments*/
                              &hints );
      XSelectInput( GuiDpy, GuiBigWin, ExposureMask | ButtonMotionMask
                   | KeyPressMask | KeyReleaseMask | ButtonPressMask | ButtonReleaseMask
                   | StructureNotifyMask | SubstructureNotifyMask
                   | VisibilityChangeMask ); 
   }
   /* Initialize colorbar editor */
   LUI_ColorBarPacking( PACK_COLOR('R','G','B','A') );
   /* make the widgets */
   create_widgets( index, volflag, programname );

   /* Initialize colorbars/colortables */

   if(do_one_time[gtx->context_index]){
      do_one_time[gtx->context_index] = 0;
      init_colortables( gtx->context_index );
   }

   /* XRaiseWindow( GuiDpy, gtx->CpWindow );  */

   XGetWindowAttributes( GuiDpy, gtx->CpWindow, &winatts);

   soundy = DisplayHeight(GuiDpy, DefaultScreen(GuiDpy)) - 620;
   if (soundy < 1) soundy = 1;
   make_sound_window( index, gtx->cpx, soundy, 630, 600);
   if (gtx->othersnddpy ){
      vis5d_resize_sounding_window( gtx->context_index, 630, 600,
                                    gtx->cpx, soundy);
   }
   /* MJK 12.04.98 */
   set_window_decor_all (index);
   XSync (GuiDpy, 0);

 
   return 1;
}


/* MJK 12.04.98 begin */
int get_window_geometry (char *geom_str, int *width, int *height,
                             int *xpos, int *ypos)
{

    int                 geom_mask, x, y;
    unsigned int        w, h;


    if (geom_str          == NULL) return 0;
    if (strlen (geom_str) ==    0) return 0;


    if ((strcmp (geom_str, "full") == 0) || (strcmp (geom_str, "FULL") == 0))
    {
        int             screen;
        Display         *dpy;

        if (!(dpy = XOpenDisplay (NULL)))
        {
            fprintf (stderr, "Unable to open default display\n");
            exit (1);
        }
        screen = DefaultScreen (dpy);

        if (xpos   != NULL) *xpos   = 0;
        if (ypos   != NULL) *ypos   = 0;
        if (width  != NULL) *width  = DisplayWidth (dpy, screen);
        if (height != NULL) *height = DisplayHeight (dpy, screen);

        return 1;
    }


    if ((geom_mask = XParseGeometry (geom_str, &x, &y, &w, &h)) == 0) return -1;

    if (geom_mask &      XValue) if (xpos   != NULL) *xpos   = x;
    if (geom_mask &      YValue) if (ypos   != NULL) *ypos   = y;
    if (geom_mask &  WidthValue) if (width  != NULL) *width  = w;
    if (geom_mask & HeightValue) if (height != NULL) *height = h;


    return 1;
}
int     update_vpos_slider (int index, int type, int var, float level)
{

    GuiContext  gtx = get_gui_gtx (index);

    if (type == VIS5D_HSLICE)
    {
        if (gtx->cur_hslice == var)
            mod_vpos_slider (index, gtx->hslice_pos_slider, var, 0, level);
    }
    else if (type == VIS5D_CHSLICE)
    {
        if (cb_chvar[cb_dindex] == var)
            mod_vpos_slider (index, gtx->chslice_pos_slider, var, 0, level);
    }
    else if (type == VIS5D_HWIND)
    {
        if (gtx->cur_hwind == var)
            mod_vpos_slider (index, gtx->hwind_pos_slider, var, 1, level);
    }
    else if (type == VIS5D_HSTREAM)
    {
        if (gtx->cur_hstream == var)
            mod_vpos_slider (index, gtx->hwind_pos_slider, var, 1, level);
    }
    else if (type == VIS5D_MAP)
    {
        mod_vpos_slider (index, gtx->map_pos_slider, -1, 0, level);
    }
    else
        return 0;


    return 1;
}

int set_window_decor (Display *dpy, Window win)

{
    if (!win) return 0;

    set_mwm_border (dpy, win,
                    MWM_DECOR_BORDER   |
                    MWM_DECOR_TITLE    |
                    MWM_DECOR_RESIZEH  |
                    MWM_DECOR_MINIMIZE |
                    MWM_DECOR_MAXIMIZE);


    return 1;
}


int set_window_decor_all (int index)
{

    GuiContext  gtx = get_gui_gtx (index);

    set_window_decor (GuiDpy, gtx->CpWindow);
    set_window_decor (GuiDpy, gtx->HWindWindow);
    set_window_decor (GuiDpy, gtx->CHSliceWindow);
    set_window_decor (GuiDpy, gtx->MapWindow);
    set_window_decor (GuiDpy, gtx->CloneWindow);
    set_window_decor (GuiDpy, gtx->IsoWindow);
    set_window_decor (GuiDpy, gtx->HSliceWindow);
    set_window_decor (GuiDpy, gtx->VSliceWindow);
    set_window_decor (GuiDpy, gtx->WindWindow);
    set_window_decor (GuiDpy, gtx->TrajWindow);
    set_window_decor (GuiDpy, gtx->ExprWindow);
    set_window_decor (GuiDpy, gtx->VerifyWindow);
    set_window_decor (GuiDpy, gtx->AlertWindow);
    set_window_decor (GuiDpy, gtx->SavePicWindow);
    set_window_decor (GuiDpy, gtx->SaveWindow);
    set_window_decor (GuiDpy, gtx->RestoreWindow);
    set_window_decor (GuiDpy, gtx->isocolor_subwin1);
    set_window_decor (GuiDpy, gtx->isocolor_subwin2);
    set_window_decor (GuiDpy, gtx->isocolor_window);
    set_window_decor (GuiDpy, gtx->rgb_sliders_window);
    set_window_decor (GuiDpy, gtx->uvw_window);
    set_window_decor (GuiDpy, gtx->SoundCtrlWindow);
    set_window_decor (GuiDpy, gtx->fakewin);
    set_window_decor (GuiDpy, gtx->ColorbarWindow);

    set_window_decor (GuiDpy, gtx->display_window);

    XSync (GuiDpy, 0);


    return 1;
}




int set_mouse_mode (int index, int mode)
{

    GuiContext  gtx = get_gui_gtx (index);
    LUI_BUTTON  *button;



    switch (mode) {
        case MM_NORMAL:
           button = gtx->normalBUTTON;
           break;
        case MM_TRAJ:
           button = gtx->trajectoryBUTTON;
           break;
        case MM_SLICE:
           button = gtx->sliceBUTTON;
           break;
        case MM_LABEL:
           button = gtx->labelBUTTON;
           break;
        case MM_PROBE:
           button = gtx->probeBUTTON;
           break;
        case MM_SOUND:
           button = gtx->soundingBUTTON;
           break;
        case MM_CLIP:
           button = gtx->clippingBUTTON;
           break;
        default:
           return 1;
    }

    LUI_ButtonState (gtx->normalBUTTON, 0);
    LUI_ButtonState (gtx->trajectoryBUTTON, 0);
    LUI_ButtonState (gtx->sliceBUTTON, 0);
    LUI_ButtonState (gtx->labelBUTTON, 0);
    LUI_ButtonState (gtx->probeBUTTON, 0);
    LUI_ButtonState (gtx->soundingBUTTON, 0);
    LUI_ButtonState (gtx->clippingBUTTON, 0);
    LUI_ButtonState (button, 1);

    button->context_index = ((index + 1) * MM_MAX) + mode;

    mode_cb (button);


    return 0;
}


int     get_animate (int index, int *state, int *rate, int *dwell)
{

    GuiContext  gtx = get_gui_gtx (index);



    *state = gtx->GoTime;
    *rate  = gtx->AnimRate;
    *dwell = gtx->AnimDwell;


    return 0;
}



int     set_animate (int index, int state, int rate, int dwell)
{

    GuiContext  gtx = get_gui_gtx (index);



    if (rate  != VIS5D_IGNORE) gtx->AnimRate  = (rate  < 0) ? 0 : rate;
    if (dwell != VIS5D_IGNORE) gtx->AnimDwell = (dwell < 0) ? 0 : dwell;

    if (state != VIS5D_IGNORE)
    {
        gtx->GoTime = (state < 0) ? -1 : (state > 0) ? 1 : 0;
    }


    return 0;
}



int     set_reverse_background (int index, int state)
{

    GuiContext  gtx = get_gui_gtx (index);
    LUI_BUTTON  *button;



    if (gtx->reverseBUTTON != NULL)
    {
        if (state == VIS5D_TOGGLE) state = !gtx->reverseBUTTON->state;
        LUI_ButtonSetState (gtx->reverseBUTTON, state);

        reverse_cb (gtx->reverseBUTTON);
    }


    return 0;
}



int     reinit_gui (int index)
{

    int         i, nvar, ntime, flag;
    float       r[6], g[6], b[6], a;
    char        var_name[20], *label[6];
    GuiContext  gtx = get_gui_gtx (index);



    if (gtx == NULL) return 0;
    if (gtx->CpWindow == 0) return 0;


    vis5d_get_ctx_numvars (index, &nvar);
    vis5d_get_dtx_numtimes (index, &ntime);


/*
 *  Hide all of the windows.
 */

    hide_colorbar_window (index);
    hide_chcolorbar_window (index);
    hide_isocolor_window (gtx);
    hide_rgb_sliders (gtx);

    if (gtx->IsoWindow) XUnmapWindow (GuiDpy, gtx->IsoWindow);
    if (gtx->HSliceWindow) XUnmapWindow (GuiDpy, gtx->HSliceWindow);
    if (gtx->VSliceWindow) XUnmapWindow (GuiDpy, gtx->VSliceWindow);
    if (gtx->WindWindow) XUnmapWindow (GuiDpy, gtx->WindWindow);
    if (gtx->ExprWindow) XUnmapWindow (GuiDpy, gtx->ExprWindow);
    if (gtx->RestoreWindow) XUnmapWindow (GuiDpy, gtx->RestoreWindow);
    if (gtx->SaveWindow) XUnmapWindow (GuiDpy, gtx->SaveWindow);
    if (gtx->SavePicWindow) XUnmapWindow (GuiDpy, gtx->SavePicWindow);
    if (gtx->TrajWindow) XUnmapWindow (GuiDpy, gtx->TrajWindow);
    if (gtx->rgb_sliders_window) XUnmapWindow (GuiDpy, gtx->rgb_sliders_window);
    if (gtx->isocolor_window) XUnmapWindow (GuiDpy, gtx->isocolor_window);
    if (gtx->isocolor_subwin1) XUnmapWindow (GuiDpy, gtx->isocolor_subwin1);
    if (gtx->isocolor_subwin2) XUnmapWindow (GuiDpy, gtx->isocolor_subwin2);
    if (gtx->uvw_window) XUnmapWindow (GuiDpy, gtx->uvw_window);
    if (gtx->CloneWindow) XUnmapWindow (GuiDpy, gtx->CloneWindow);
    if (gtx->fakewin) XUnmapWindow (GuiDpy, gtx->fakewin);
    if (gtx->SoundCtrlWindow) XUnmapWindow (GuiDpy, gtx->SoundCtrlWindow);
    if (gtx->HWindWindow) XUnmapWindow (GuiDpy, gtx->HWindWindow);
    if (gtx->CHSliceWindow) XUnmapWindow (GuiDpy, gtx->CHSliceWindow);
    if (gtx->MapWindow) XUnmapWindow (GuiDpy, gtx->MapWindow);

    XSync (GuiDpy, 0);

    gtx->cur_isosurf = -1;
    gtx->cur_hslice  = -1;
    gtx->cur_vslice  = -1;
    gtx->cur_trajset = -1;
    gtx->cur_hwind   = -1;
    gtx->cur_vwind   = -1;
    gtx->cur_hstream = -1;
    gtx->cur_vstream = -1;


/*
 *  Refill the button matrices.
 */

    if (gtx->ButtonMatrix)
    {
        XUnmapWindow (GuiDpy, gtx->ButtonMatrix->mainwindow);
        LUI_ButtonMatrixEmpty (gtx->ButtonMatrix);

        r[5] = g[5] = b[5] = 1.0; /* volviz is always white */
        for (i = 0; i < 6; i++) label[i] = var_name;

        for (i = 0; i < nvar; i++)
        {
            vis5d_get_ctx_var_name (index, i, var_name);

            vis5d_get_color (index, VIS5D_ISOSURF, i, &r[0], &g[0], &b[0], &a);
            vis5d_get_color (index, VIS5D_HSLICE, i, &r[1], &g[1], &b[1], &a);
            vis5d_get_color (index, VIS5D_VSLICE, i, &r[2], &g[2], &b[2], &a);
            vis5d_get_color (index, VIS5D_CHSLICE, i, &r[3], &g[3], &b[3], &a);
            vis5d_get_color (index, VIS5D_CVSLICE, i, &r[4], &g[4], &b[4], &a);

            LUI_ButtonMatrixAddRow (gtx->ButtonMatrix, label, r, g, b);
        }

        XMapWindow (GuiDpy, gtx->ButtonMatrix->mainwindow);
    }

    if (gtx->iso_button_matrix)
    {
        XUnmapWindow (GuiDpy, gtx->iso_button_matrix->mainwindow);
        LUI_ButtonMatrixEmpty (gtx->iso_button_matrix);

        r[0] = g[0] = b[0] = 1.0;
        label[0] = var_name;

        strcpy (var_name, "MonoColor");
        LUI_ButtonMatrixAddRow (gtx->iso_button_matrix, label, r, g, b);
        for (i = 0; i < nvar; i++)
        {
            vis5d_get_ctx_var_name (index, i, var_name);

            LUI_ButtonMatrixAddRow (gtx->iso_button_matrix, label, r, g, b);
        }

        XMapWindow (GuiDpy, gtx->iso_button_matrix->mainwindow);
    }


/*
 *  Finish up.
 */

    set_mouse_mode (index, MM_NORMAL);

    init_colortables (index);

    XSync (GuiDpy, 0);


    return 0;
}




/* MJK 12.04.98 end */

int set_busy_cursor ( int busy)
{
   int i, nr, nc;

   get_display_matrix( &nr, &nc);
   for( i = 0; i < nr*nc; i++){
      if (gtx_table[i]) {
         if (gtx_table[i]->CpWindow) vis5d_set_busy_cursor (GuiDpy, gtx_table[i]->CpWindow,busy);
         if (gtx_table[i]->IsoWindow) vis5d_set_busy_cursor (GuiDpy, gtx_table[i]->IsoWindow,busy);
         if (gtx_table[i]->HSliceWindow) vis5d_set_busy_cursor (GuiDpy, gtx_table[i]->HSliceWindow,busy);
         if (gtx_table[i]->VSliceWindow) vis5d_set_busy_cursor (GuiDpy, gtx_table[i]->VSliceWindow,busy);
         if (gtx_table[i]->WindWindow) vis5d_set_busy_cursor (GuiDpy, gtx_table[i]->WindWindow,busy);
         if (gtx_table[i]->ExprWindow) vis5d_set_busy_cursor (GuiDpy, gtx_table[i]->ExprWindow,busy);
         if (gtx_table[i]->RestoreWindow) vis5d_set_busy_cursor (GuiDpy, gtx_table[i]->RestoreWindow,busy);
         if (gtx_table[i]->SaveWindow) vis5d_set_busy_cursor (GuiDpy, gtx_table[i]->SaveWindow,busy);
         if (gtx_table[i]->SavePicWindow) vis5d_set_busy_cursor (GuiDpy, gtx_table[i]->SavePicWindow,busy);
         if (gtx_table[i]->TrajWindow) vis5d_set_busy_cursor (GuiDpy, gtx_table[i]->TrajWindow,busy);
         if (gtx_table[i]->rgb_sliders_window)
             vis5d_set_busy_cursor (GuiDpy, gtx_table[i]->rgb_sliders_window,busy);
         if (gtx_table[i]->isocolor_window)
             vis5d_set_busy_cursor (GuiDpy, gtx_table[i]->isocolor_window,busy);
         if (gtx_table[i]->isocolor_subwin1)
             vis5d_set_busy_cursor (GuiDpy, gtx_table[i]->isocolor_subwin1,busy);
         if (gtx_table[i]->isocolor_subwin2)
             vis5d_set_busy_cursor (GuiDpy, gtx_table[i]->isocolor_subwin2,busy);
         if (gtx_table[i]->uvw_window) vis5d_set_busy_cursor (GuiDpy, gtx_table[i]->uvw_window,busy);
         if (gtx_table[i]->CloneWindow) vis5d_set_busy_cursor (GuiDpy, gtx_table[i]->CloneWindow,busy);
         if (gtx_table[i]->fakewin) vis5d_set_busy_cursor (GuiDpy, gtx_table[i]->fakewin,busy);
         if (gtx_table[i]->SoundCtrlWindow)
             vis5d_set_busy_cursor (GuiDpy, gtx_table[i]->SoundCtrlWindow,busy);
         if (gtx_table[i]->HWindWindow) vis5d_set_busy_cursor (GuiDpy, gtx_table[i]->HWindWindow,busy);
         if (gtx_table[i]->CHSliceWindow) vis5d_set_busy_cursor (GuiDpy, gtx_table[i]->CHSliceWindow,busy);
         if (gtx_table[i]->MapWindow) vis5d_set_busy_cursor (GuiDpy, gtx_table[i]->MapWindow,busy);
         if (gtx_table[i]->ColorbarWindow)
             vis5d_set_busy_cursor (GuiDpy, gtx_table[i]->ColorbarWindow,busy);
         if (GuiBigWin){
            vis5d_set_busy_cursor (GuiDpy, GuiBigWin, busy);
         }
      }
   }
   if (BigWindow){
      vis5d_set_busy_cursor (GfxDpy, BigWindow, busy);
   }

    return 1;
}

/* MJK 12.04.98 end */
