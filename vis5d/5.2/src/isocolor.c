/*  isocolor.c */
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




/*
 * Isosurface color widget
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <X11/Xlib.h>
#include "../lui5/lui.h"
#include "api.h"
#include "gui.h"
#include "graphics.h"
#include "isocolor.h"
#include "mwmborder.h"
#include "uvwwidget.h"


static int current_dindex = 0;
static int current_graphic[VIS5D_MAX_DPY_CONTEXTS];
static int current_var[VIS5D_MAX_DPY_CONTEXTS];
static int current_vindex[VIS5D_MAX_DPY_CONTEXTS];
static int current_vvar[VIS5D_MAX_DPY_CONTEXTS];

/*static ColorBar isocb;*/
static int curbutton[VIS5D_MAX_DPY_CONTEXTS];
static int curisovrow[VIS5D_MAX_DPY_CONTEXTS];
static int curisovindex[VIS5D_MAX_DPY_CONTEXTS];


/* MJK 12.04.98 */
static int callback( LUI_BUTTON_MATRIX *bm, int row, int col, int button )
{
   float *p;
   int dindex, vindex;
   GuiContext gtx = get_gui_gtx( current_dindex );

   curisovrow[current_dindex] = get_button_ctx_row( current_dindex, row-1);
   curisovindex[current_dindex] = get_button_ctx_index( current_dindex, row-1);
   dindex = current_dindex;

   LUI_ButtonMatrixSetState( bm, curbutton[current_dindex], 0, 0 );
   LUI_ButtonMatrixSetState( bm, row, 0, 1 );
   if (current_graphic[current_dindex]==VIS5D_TOPO) {
      XUnmapWindow( GuiDpy, gtx->isocolor_subwin1 );
      XMapWindow( GuiDpy, gtx->isocolor_subwin2 );
   }
   else {
      /* Isosurface of trajectory color table */
      if (curbutton[current_dindex]==0 && row>0) {
         XUnmapWindow( GuiDpy, gtx->isocolor_subwin1 );
         XMapWindow( GuiDpy, gtx->isocolor_subwin2 );
      }
      else if (curbutton[current_dindex]>0 && row==0) {
         XMapWindow( GuiDpy, gtx->isocolor_subwin1 );
         XUnmapWindow( GuiDpy, gtx->isocolor_subwin2 );
      }
   }

   curbutton[current_dindex] = row;

   if (row>-1 || current_graphic[current_dindex]==VIS5D_TOPO) {
      unsigned int *table;
      char varname[20], units[20];
      float min, max;
      int cvar, cvowner;
      int numtimes, curtime, dtxcurtime, time;


      cvar = curisovrow[current_dindex];
      cvowner = curisovindex[current_dindex];
      vis5d_get_ctx_numtimes( current_vindex[current_dindex], &numtimes );
      vis5d_get_ctx_timestep( current_vindex[current_dindex], &curtime );
      vis5d_get_dtx_timestep( current_dindex, &dtxcurtime);

      if (cvar >= 0)
      {
         vis5d_get_ctx_var_name  (cvowner, cvar, varname);
         vis5d_get_var_units (cvowner, cvar, units);
         vis5d_get_ctx_var_range (cvowner, cvar, &min, &max);
         vis5d_get_color_table_address (dindex,
                                        current_graphic[current_dindex],
                                        cvowner, cvar, &table);
         vis5d_get_color_table_params (dindex,
                                       current_graphic[current_dindex],
                                       cvowner, cvar, &p);
         LUI_ColorBarChange (gtx->iso_colorbar,
                             varname, units, min, max, table, 255, p);
      }

      if (current_graphic[current_dindex]==VIS5D_ISOSURF) {
         vis5d_set_isosurface_color_var( current_vindex[current_dindex],
                             current_vvar[current_dindex], cvowner, cvar );
         /* recompute/recolor the isosurface acording to new coloring var */
         for (time=0;time<numtimes;time++) {
            vis5d_make_isosurface( current_vindex[current_dindex],
                                   time, current_vvar[current_dindex],
                                   time==curtime );
         }
      }
      else if (current_graphic[current_dindex]==VIS5D_TRAJ) {
         /* current_var[current_dindex] is really the trajectory set */
         vis5d_set_trajectory_color_var( dindex, current_var[current_dindex],
                                         cvowner, cvar );
      }
      else if (current_graphic[current_dindex]==VIS5D_TOPO) {
         if (cvar < 0) {
            strcpy (varname, "Elevation");
            strcpy (units, "km");
            vis5d_get_topo_range (dindex, &min, &max);
            vis5d_get_color_table_address (dindex, VIS5D_TOPO,
                                           cvowner, cvar, &table);
            vis5d_get_color_table_params (dindex, VIS5D_TOPO,
                                          cvowner, cvar, &p);
            LUI_ColorBarChange (gtx->iso_colorbar,
                                varname, units, min, max, table, 255, p);
         }
         vis5d_set_topo_color_var(dindex, cvowner,  cvar );
      }
   }
   return 0;
}

static void init_currents(void)
{
   int yo;
   for (yo = 0; yo < VIS5D_MAX_DPY_CONTEXTS; yo++){
      current_graphic[yo] = -1;
      current_var[yo] = -1;
      current_vindex[yo] = -1;
      current_vvar[yo] = -1;
      curbutton[yo] = 0;
      curisovrow[yo] = -1;
      curisovindex[yo] = -1;
   }
}


/*
 * Called when one of the variable buttons in the isosurface color
 * widget is pressed.
 */

#ifdef JOHAN
static int callback( LUI_BUTTON_MATRIX *bm, int row, int col, int button )
{
   float *p;
   int dindex, vindex;
   GuiContext gtx = get_gui_gtx( current_dindex );

   curisovrow[current_dindex] = get_button_ctx_row( current_dindex, row-1); 
   curisovindex[current_dindex] = get_button_ctx_index( current_dindex, row-1); 
   dindex = current_dindex;

   LUI_ButtonMatrixSetState( bm, curbutton[current_dindex], 0, 0 );
   LUI_ButtonMatrixSetState( bm, row, 0, 1 );
   if (current_graphic[current_dindex]==VIS5D_TOPO) {
      XUnmapWindow( GuiDpy, gtx->isocolor_subwin1 );
      XMapWindow( GuiDpy, gtx->isocolor_subwin2 );
   }
   else {
      /* Isosurface of trajectory color table */
      if (curbutton[current_dindex]==0 && row>0) {
         XUnmapWindow( GuiDpy, gtx->isocolor_subwin1 );
         XMapWindow( GuiDpy, gtx->isocolor_subwin2 );
      }
      else if (curbutton[current_dindex]>0 && row==0) {
         XMapWindow( GuiDpy, gtx->isocolor_subwin1 );
         XUnmapWindow( GuiDpy, gtx->isocolor_subwin2 );
      }
   }

   curbutton[current_dindex] = row;

   if (row>-1 || current_graphic[current_dindex]==VIS5D_TOPO) {
      unsigned int *table;
      char varname[20];
      float min, max;
      int cvar, cvowner;
      int numtimes, curtime, dtxcurtime, time;
      

      cvar = curisovrow[current_dindex];
      cvowner = curisovindex[current_dindex]; 
      vis5d_get_ctx_numtimes( current_vindex[current_dindex], &numtimes );
      vis5d_get_ctx_timestep( current_vindex[current_dindex], &curtime );
      vis5d_get_dtx_timestep( current_dindex, &dtxcurtime);
      if (current_graphic[current_dindex]==VIS5D_ISOSURF) {
         if (cvar>=0) {
            vis5d_get_color_table_address( dindex, VIS5D_ISOSURF,
                                           cvowner, cvar, &table );
            vis5d_get_color_table_params(dindex, VIS5D_ISOSURF, cvowner, cvar, &p);
            vis5d_get_ctx_var_name( cvowner, cvar, varname );
            vis5d_get_ctx_var_range( cvowner, cvar, &min, &max );
            LUI_ColorBarChange( gtx->iso_colorbar,
                                varname, min, max, table, 255, p);
         }
         vis5d_set_isosurface_color_var( current_vindex[current_dindex], 
                             current_vvar[current_dindex], cvowner, cvar );
         /* recompute/recolor the isosurface acording to new coloring var */
         for (time=0;time<numtimes;time++) {
            vis5d_make_isosurface( current_vindex[current_dindex],
                                   time, current_vvar[current_dindex],
                                   time==curtime );
         }
      }
      else if (current_graphic[current_dindex]==VIS5D_TRAJ) {
         if (cvar>=0) {
            vis5d_get_color_table_address( dindex, VIS5D_TRAJ,
                                           cvowner, cvar, &table );
            vis5d_get_color_table_params(dindex, VIS5D_TRAJ, cvowner, cvar, &p);
            vis5d_get_ctx_var_name( cvowner, cvar, varname );
            vis5d_get_ctx_var_range( cvowner, cvar, &min, &max );
            LUI_ColorBarChange( gtx->iso_colorbar,
                                varname, min, max, table, 255, p);
         }
         /* current_var[current_dindex] is really the trajectory set */
         vis5d_set_trajectory_color_var( dindex, current_var[current_dindex], cvowner, cvar );
      }
      else if (current_graphic[current_dindex]==VIS5D_TOPO) {
         vis5d_get_color_table_address( dindex, VIS5D_TOPO,
                                        cvowner, cvar, &table );
         vis5d_get_color_table_params(dindex, VIS5D_TOPO, cvowner, cvar, &p);
         if (cvar>=0) {
            vis5d_get_ctx_var_name( cvowner, cvar, varname );
            vis5d_get_ctx_var_range(cvowner, cvar, &min, &max );
         }
         else {
            vis5d_get_topo_range( dindex, &min, &max );
            strcpy( varname, "Elevation" );
         }
         LUI_ColorBarChange( gtx->iso_colorbar,
                                varname, min, max, table, 255, p);
         vis5d_set_topo_color_var(dindex, cvowner,  cvar );
      }
   }
   vis5d_signal_redraw( current_dindex, 2 );
   return 0;
}
#endif




/* called when red slider is changed */
static int red_slider_cb( LUI_NEWSLIDER *s, float value )
{
   GuiContext gtx = get_gui_gtx( current_dindex );
   float r, g, b, a;

   if (current_graphic[current_dindex]==VIS5D_TRAJ){
      vis5d_get_color( current_dindex, current_graphic[current_dindex],
                       current_var[current_dindex], &r,&g,&b,&a );
      r = value;
      vis5d_set_color( current_dindex, current_graphic[current_dindex],  
                   current_var[current_dindex], r,g,b,a );
   }
   else{
     vis5d_get_color( current_dindex, current_graphic[current_dindex], curisovindex[current_dindex]*MAXVARS+
                       current_vvar[current_dindex], &r,&g,&b,&a );
      r = value;
      vis5d_set_color( current_dindex, current_graphic[current_dindex], curisovindex[current_dindex]*MAXVARS+
                       current_vvar[current_dindex], r,g,b,a );
   }

   if (current_graphic[current_dindex]==VIS5D_ISOSURF) {
      LUI_ButtonMatrixSetColor( gtx->ButtonMatrix, current_var[current_dindex], 0, r, g, b );
   }
   else if (current_graphic[current_dindex]==VIS5D_TRAJ) {
      LUI_ButtonSetColor( gtx->TrajButton[current_var[current_dindex]], (int)(r*255),
                          (int)(g*255), (int)(b*255) );
   }
   vis5d_signal_redraw( current_dindex, 1 );
   return 0;
}



/* called when green slider is changed */
static int green_slider_cb( LUI_NEWSLIDER *s, float value )
{
   GuiContext gtx = get_gui_gtx( current_dindex );
   float r, g, b, a;

   if (current_graphic[current_dindex]==VIS5D_TRAJ){
      vis5d_get_color( current_dindex, current_graphic[current_dindex], 
                       current_var[current_dindex], &r,&g,&b,&a );
      g = value;
      vis5d_set_color( current_dindex, current_graphic[current_dindex], 
                   current_var[current_dindex], r,g,b,a );
   }
   else{
     vis5d_get_color( current_dindex, current_graphic[current_dindex], curisovindex[current_dindex]*MAXVARS+
                       current_vvar[current_dindex], &r,&g,&b,&a );
      g = value;
      vis5d_set_color( current_dindex, current_graphic[current_dindex], curisovindex[current_dindex]*MAXVARS+
                       current_vvar[current_dindex], r,g,b,a );
   }

   if (current_graphic[current_dindex]==VIS5D_ISOSURF) {
      LUI_ButtonMatrixSetColor( gtx->ButtonMatrix, current_var[current_dindex], 0, r, g, b );
   }
   else if (current_graphic[current_dindex]==VIS5D_TRAJ) {
      LUI_ButtonSetColor( gtx->TrajButton[current_var[current_dindex]], (int)(r*255),
                          (int)(g*255), (int)(b*255) );
   }
   vis5d_signal_redraw( current_dindex, 1 );
   return 0;
}



/* called when blue slider is changed */
static int blue_slider_cb( LUI_NEWSLIDER *s, float value )
{
   GuiContext gtx = get_gui_gtx( current_dindex );
   float r, g, b, a;

   if (current_graphic[current_dindex]==VIS5D_TRAJ){
      vis5d_get_color( current_dindex, current_graphic[current_dindex], 
                       current_var[current_dindex], &r,&g,&b,&a );
      b = value;
      vis5d_set_color( current_dindex, current_graphic[current_dindex],
                   current_var[current_dindex], r,g,b,a );
   }
   else{
     vis5d_get_color( current_dindex, current_graphic[current_dindex], curisovindex[current_dindex]*MAXVARS+
                       current_vvar[current_dindex], &r,&g,&b,&a );
      b = value;
      vis5d_set_color( current_dindex, current_graphic[current_dindex], curisovindex[current_dindex]*MAXVARS+
                       current_vvar[current_dindex], r,g,b,a );
   }

   if (current_graphic[current_dindex]==VIS5D_ISOSURF) {
      LUI_ButtonMatrixSetColor( gtx->ButtonMatrix, current_var[current_dindex], 0, r, g, b );
   }
   else if (current_graphic[current_dindex]==VIS5D_TRAJ) {
      LUI_ButtonSetColor( gtx->TrajButton[current_var[current_dindex]], (int)(r*255),
                          (int)(g*255), (int)(b*255) );
   }
   vis5d_signal_redraw( current_dindex, 1 );
   return 0;
}



/* called when alpha slider is changed */
static int alpha_slider_cb( LUI_NEWSLIDER *s, float value )
{
   float r, g, b, a;

   if (current_graphic[current_dindex]==VIS5D_TRAJ){
      vis5d_get_color( current_dindex, current_graphic[current_dindex],
                       current_var[current_dindex], &r,&g,&b,&a );
      a = value;
      vis5d_set_color( current_dindex, current_graphic[current_dindex],
                   current_var[current_dindex], r,g,b,a );
   }
   else{
     vis5d_get_color( current_dindex, current_graphic[current_dindex], curisovindex[current_dindex]*MAXVARS+
                       current_vvar[current_dindex], &r,&g,&b,&a );
      a = value;
      vis5d_set_color( current_dindex, current_graphic[current_dindex], curisovindex[current_dindex]*MAXVARS+
                       current_vvar[current_dindex], r,g,b,a );
   }

   vis5d_signal_redraw( current_dindex, 1 );
   return 0;
}



/* Called when Close button is pressed */
static int close_cb( LUI_NEWBUTTON *b, int state )
{
   /* MJK 12.04.98 */
   GuiContext gtx = get_gui_gtx( b->context_index );

   XUnmapWindow( GuiDpy, gtx->isocolor_window );
   current_vindex[current_dindex] =  current_graphic[current_dindex] = current_var[current_dindex] = -1;
   current_vvar[current_dindex] = -1;
   return 0;
}



/*
 * Called when the colorbar's color table is changed.
 */
static int colorbar_callback( LUI_COLORBAR *cb, int action )
{
   GuiContext gtx = get_gui_gtx( current_dindex );
   float *p;
   int  cvowner;

 
   if (action==LUI_RGB_RESET || action==LUI_ALPHA_RESET
       || action==LUI_RGB_SHAPE || action==LUI_ALPHA_CHANGE) {
      if (current_graphic[current_dindex]==VIS5D_ISOSURF) {
         unsigned int *ctable;
         int cvar;

         vis5d_get_isosurface_color_var( current_vindex[current_dindex], 
                             current_vvar[current_dindex], &cvowner, &cvar );
         vis5d_get_color_table_address( current_dindex, VIS5D_ISOSURF,
                                        cvowner, cvar, &ctable );
         vis5d_get_color_table_params( current_dindex, VIS5D_ISOSURF, cvowner, cvar, &p);
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
            vis5d_signal_redraw( current_dindex, 1);
         }
      }
      else if (current_graphic[current_dindex]==VIS5D_TRAJ) {
         unsigned int *ctable;
         int cvar, trajset = current_var[current_dindex];
         vis5d_get_trajectory_color_var( current_dindex, trajset, &cvowner, &cvar );
         vis5d_get_color_table_address( current_dindex, VIS5D_TRAJ,
                                        cvowner, cvar, &ctable );
         vis5d_get_color_table_params( current_dindex, VIS5D_TRAJ, cvowner, cvar, &p);
         if (action==LUI_RGB_RESET) {
            vis5d_color_table_init_params( p, 1, 0 );
            vis5d_color_table_recompute( ctable, 256, p, 1, 0);
         }
         else if (action==LUI_ALPHA_RESET) {
            vis5d_color_table_init_params( p, 0, 1 );
            vis5d_color_table_recompute( ctable, 256, p, 0, 1);
         }
         else if (action==LUI_RGB_SHAPE) {
            vis5d_color_table_recompute( ctable, 256, p, 1, 0);
         }
         else if (action==LUI_ALPHA_SHAPE) {
            vis5d_color_table_recompute( ctable, 256, p, 0, 1);
         }
         else if (action==LUI_ALPHA_CHANGE) {
            vis5d_signal_redraw( current_dindex, 1);
         }
      }
      else if (current_graphic[current_dindex]==VIS5D_TOPO) {
         unsigned int *ctable;
         int cvar;

         vis5d_get_topo_color_var( current_dindex, &cvowner, &cvar );
         if (cvar>=0) {
            vis5d_get_color_table_address( current_dindex, VIS5D_TOPO,
                                           cvowner, cvar, &ctable );
            vis5d_get_color_table_params( current_dindex, VIS5D_TOPO, cvowner, cvar, &p);
         }
         if (action==LUI_RGB_RESET) {
            if (cvar<0) {
               vis5d_reset_topo_colors( current_dindex );
            }
            else {
               vis5d_color_table_init_params( p, 1, 0 );
               vis5d_color_table_recompute( ctable, 256, p, 1, 0);
            }
         }
         else if (action==LUI_ALPHA_RESET) {
            if (cvar<0) {
               vis5d_reset_topo_colors( current_dindex );
            }
            else {
               vis5d_color_table_init_params( p, 0, 1 );
               vis5d_color_table_recompute( ctable, 256, p, 0, 1);
            }
         }
         else if (action==LUI_RGB_SHAPE && cvar>=0) {
            vis5d_color_table_recompute( ctable, 256, p, 1, 0);
         }
         else if (action==LUI_ALPHA_SHAPE && cvar>=0) {
            vis5d_color_table_recompute( ctable, 256, p, 0, 1);
         }
         else if (action==LUI_ALPHA_CHANGE) {
            vis5d_signal_redraw( current_dindex, 1);
         }
      }
      else {
         abort();
      }

      /* Now actually redraw the color curves */
      LUI_ColorBarRedraw( cb );
   }

   /* redraw window in any case */
   vis5d_signal_redraw( current_dindex, 1 );
   return 0;
}



/*
 * Make the isosurface color widget window.
 * This may be called more than once actually.
 */
void make_isocolor_window( GuiContext gtx )
{
   LUI_BUTTON_MATRIX *bm;
   LUI_NEWSLIDER *s;
   LUI_NEWBUTTON *b;
   char varname[20];
   int i;
   float red=1.0, green=1.0, blue=1.0;
   int index = gtx->context_index;
   int subx, suby;
   Window w;
   int yo, spandex;
   char *labels[1];
   static int do_once = 1;
   int done_mainwin = 0;

   if(do_once){
      do_once = 0;
      init_currents();
   }

   current_dindex = gtx->context_index;

   if (!gtx->isocolor_window){
      gtx->isocolor_window = LUI_CreateWindowAt( LUI_RootWindow, 10,
                                                 800, 380, 182 );
      w = gtx->isocolor_window;

      gtx->isocolor_label = LUI_NewLabelCreate( w, LUI_LEFT, LUI_TOP, 310, 20,
                                             "default label" );
      b = LUI_PushButtonCreate( w, LUI_NEXT_X, LUI_SAME_Y, 60, 20, "Close" );
      LUI_ButtonCallback( b, close_cb );
      done_mainwin = 1;
   }

   w = gtx->isocolor_window;

   /* create the button matrix (actually 1-column list) of variable buttons */
   if (gtx->iso_button_matrix){
      LUI_ButtonMatrixDestroy(gtx->iso_button_matrix);
   }
   if (done_mainwin){

      bm = LUI_ButtonMatrixCreate( w, LUI_LEFT, LUI_NEXT_Y, 100, 154, 1);
   }
   else{
      bm = LUI_ButtonMatrixCreate( w, 4, 26, 100, 154, 1);
   }
   gtx->iso_button_matrix = bm;

      labels[0] = "MonoColor";
      LUI_ButtonMatrixAddRow( bm, labels, &red, &green, &blue );
      for (yo=0; yo< gtx->how_many_regular_contexts; yo++){
         spandex = gtx->array_of_ctxs[yo];
         for (i=0;i<gtx->RegularNumVars[spandex];i++) {
            vis5d_get_ctx_var_name( spandex, i, gtx->RegularVarName[i][spandex] );
            if (gtx->how_many_regular_contexts <= 1){
               labels[0] = gtx->RegularVarName[i][spandex];
            }
            else{
               labels[0] = return_var_and_index(gtx->RegularVarName[i][spandex], spandex);
            }
            LUI_ButtonMatrixAddRow( bm, labels,
                                    &red, &green, &blue );
         }
      }

   LUI_ButtonMatrixCallback( bm, callback );
   LUI_ButtonMatrixSetState( bm, curbutton[current_dindex], 0, 1 );

   subx = LUI_LayoutGet( LUI_NEXT_X );
   suby = LUI_LayoutGet( LUI_SAME_Y );

   if (!gtx->isocolor_subwin1){
      gtx->isocolor_subwin1 = LUI_CreateWindowAt( w, subx, suby, 280, 200 );

   /* MJK 12.04.98 begin */
      /* red slider */
      s = LUI_NewSliderCreate( gtx->isocolor_subwin1, 0, 0, 270 );
      gtx->iso_red_slider = s;
      LUI_NewSliderChange( s, "Red", NULL, 0.0, 1.0, 1.0 );
      LUI_NewSliderCallback( s, red_slider_cb );

      /* green slider */
      s = LUI_NewSliderCreate( gtx->isocolor_subwin1, 0, LUI_NEXT_Y, 270 );
      gtx->iso_green_slider = s;
      LUI_NewSliderChange( s, "Green", NULL, 0.0, 1.0, 1.0 );
      LUI_NewSliderCallback( s, green_slider_cb );

      /* blue slider */
      s = LUI_NewSliderCreate( gtx->isocolor_subwin1, 0, LUI_NEXT_Y, 270 );
      gtx->iso_blue_slider = s;
      LUI_NewSliderChange( s, "Blue", NULL, 0.0, 1.0, 1.0 );
      LUI_NewSliderCallback( s, blue_slider_cb );

      /* alpha slider */
      s = LUI_NewSliderCreate( gtx->isocolor_subwin1, 0, LUI_NEXT_Y, 270 );
      gtx->iso_alpha_slider = s;
      LUI_NewSliderChange( s, "Opacity", NULL, 0.0, 1.0, 1.0 );
      LUI_NewSliderCallback( s, alpha_slider_cb );
   /* MJK 12.04.98 end */
   }

   XMapWindow( GuiDpy, gtx->isocolor_subwin1 );

   LUI_FrameWidth(4);
   LUI_ColorBarPacking( PACK_COLOR('R','G','B','A') );

   if (!gtx->isocolor_subwin2){
      gtx->isocolor_subwin2 = LUI_CreateWindowAt( w, subx, suby, 280, 200 );
      gtx->iso_colorbar = LUI_ColorBarCreate( gtx->isocolor_subwin2,
                                                    0, 0, 270, 154 );
      LUI_ColorBarCallback( gtx->iso_colorbar, colorbar_callback );
   }

   XMapWindow( GuiDpy, bm->mainwindow );
}




/* Unmap/hide the isosurface color window */
void hide_isocolor_window( GuiContext gtx )
{
   XUnmapWindow( GuiDpy, gtx->isocolor_window );
   current_vindex[current_dindex] =  current_graphic[current_dindex] = current_var[current_dindex] = -1;
   current_vvar[current_dindex] = -1;
}



/* Map/display the isosurface color window */
/* vindex - this is the vis5d_ctx index of varibale clicked on */
/* over_ride- if 1 then map window regarless */
/* MJK 12.08.98 begin */
void show_isocolor_window( GuiContext gtx, int graphic,
                           int vindex, int var, int over_ride )
{
   float r, g, b, a;
   unsigned int *color;
   int numvars;
   int colorvar;
   int row;
   unsigned int *table;
   float min, max;
   char varname[20], units[20];
   char label[100];
   float *p;
   int cvowner, cv;


   /* unmap window in case it's already displayed */
   XUnmapWindow( GuiDpy, gtx->isocolor_window );

   if ((current_dindex==gtx->context_index) &&
       (current_graphic[current_dindex]==graphic) &&
       (current_var[current_dindex]==var) && over_ride != 1) {
      /* toggle off */
      current_graphic[current_dindex] = current_var[current_dindex] = -1;
      return;
   }

   current_dindex = gtx->context_index;
   current_graphic[current_dindex] = graphic;
   current_var[current_dindex] = var;
   current_vindex[current_dindex] = vindex;
   /* current_vvar[current_dindex] is the var number according to the
      vis5d_ctx it belongs to */
   if (current_graphic[current_dindex]<6){
      current_vvar[current_dindex] = get_button_ctx_row(current_dindex, var);
   }
   else{
      current_vvar[current_dindex] = -1;
   }
   /* set the label string */
   if (graphic==VIS5D_ISOSURF) {
      vis5d_get_ctx_var_name( current_vindex[current_dindex],
                          current_vvar[current_dindex], varname );
      sprintf( label, "%s isosurface color:", varname );
   }
   else if (graphic==VIS5D_TRAJ) {
      sprintf( label, "Trajectory set %d color:", var+1 );
   }
   else if (graphic==VIS5D_TOPO) {
      sprintf( label, "Topography color:" );
   }
   else {
      /* should never get here */
      abort();
   }
   LUI_NewLabelChangeText( gtx->isocolor_label, label );

   /* Set RGBA sliders to appropriate color values */
   if (graphic == VIS5D_ISOSURF || graphic == VIS5D_TRAJ){
      cv = (graphic == VIS5D_ISOSURF) ? current_vvar[current_dindex] :
                                        current_var[current_dindex];
      vis5d_get_color (current_dindex, current_graphic[current_dindex],
                       (current_vindex[current_dindex]*MAXVARS)+cv,
                        &r,&g,&b,&a );
      LUI_NewSliderSetValue( gtx->iso_red_slider, r );
      LUI_NewSliderSetValue( gtx->iso_green_slider, g );
      LUI_NewSliderSetValue( gtx->iso_blue_slider, b );
      LUI_NewSliderSetValue( gtx->iso_alpha_slider, a );
   }


   /* turn off previous button */
   vis5d_get_ctx_numvars( vindex, &numvars );
   for (row=0;row<gtx->total_numvars+1;row++) {
      LUI_ButtonMatrixSetState( gtx->iso_button_matrix, row,
                                0, 0 );
   }

   /* turn on current button */
   /* and load colorbar widget with apropriate color table */
   p = NULL;
   if (current_graphic[current_dindex]==VIS5D_ISOSURF) {
      vis5d_get_isosurface_color_var( vindex, current_vvar[current_dindex],
                                      &cvowner, &colorvar );
      if (colorvar <0){
         curisovindex[current_dindex] = current_vindex[current_dindex];
         curisovrow[current_dindex]   = current_vvar[current_dindex];
      }
   }
   else if (current_graphic[current_dindex]==VIS5D_TRAJ) {
      vis5d_get_trajectory_color_var( vindex, current_var[current_dindex],
                                      &cvowner, &colorvar );
   }
   else if (current_graphic[current_dindex]==VIS5D_TOPO) {
      vis5d_get_topo_color_var( current_dindex, &cvowner,  &colorvar );
      if (colorvar<0) {
         strcpy( varname, "Elevation" );
         strcpy (units, "km");
         vis5d_get_topo_range( current_dindex, &min, &max );
         vis5d_get_color_table_address( current_dindex, VIS5D_TOPO,
                                        cvowner, colorvar, &table );
         vis5d_get_color_table_params(current_dindex, VIS5D_TOPO,
                                      cvowner, colorvar, &p);
      }
   }
   if (colorvar >= 0) {
      vis5d_get_ctx_var_name  (cvowner, colorvar, varname);
      vis5d_get_var_units (cvowner, colorvar, units);
      vis5d_get_ctx_var_range (cvowner, colorvar, &min, &max);
      vis5d_get_color_table_address (current_dindex,
                                     current_graphic[current_dindex],
                                     cvowner, colorvar, &table);
      vis5d_get_color_table_params (current_dindex,
                                    current_graphic[current_dindex],
                                    cvowner, colorvar, &p);
   }
   if (p != NULL) {
      LUI_ColorBarChange (gtx->iso_colorbar,
                          varname, units, min, max, table, 255, p);
   }

   if (current_graphic[current_dindex]==VIS5D_TOPO) {
      LUI_ButtonMatrixChangeLabel( gtx->iso_button_matrix, 0, 0, "Elevation" );
   }
   else {
      LUI_ButtonMatrixChangeLabel( gtx->iso_button_matrix, 0, 0, "MonoColor" );
   }

   LUI_ButtonMatrixSetState( gtx->iso_button_matrix, colorvar+1, 0, 1 );
   curbutton[current_dindex] = get_button_gtx_index(current_dindex,
                                                    cvowner, colorvar);
   curbutton[current_dindex] +=1;
   /* show either color sliders or color widget */
   XUnmapWindow( GuiDpy, gtx->isocolor_subwin1 );
   XUnmapWindow( GuiDpy, gtx->isocolor_subwin2 );
   if (colorvar>=0 || current_graphic[current_dindex]==VIS5D_TOPO) {
      XMapWindow( GuiDpy, gtx->isocolor_subwin2 );
   }
   else {
      XMapWindow( GuiDpy, gtx->isocolor_subwin1 );
   }

   XMapWindow( GuiDpy, gtx->isocolor_window );
}

/* MJK end */

#ifdef JOHAN
void show_isocolor_window( GuiContext gtx, int graphic,
                           int vindex, int var, int over_ride )
{
   float r, g, b, a;
   unsigned int *color;
   int numvars;
   int colorvar;
   int row;
   unsigned int *table;
   float min, max;
   char varname[20];
   char label[100];
   float *p;
   int cvowner;


   /* unmap window in case it's already displayed */
   XUnmapWindow( GuiDpy, gtx->isocolor_window );

   if (current_dindex==gtx->context_index && current_graphic[current_dindex]==graphic
       && current_var[current_dindex]==var && over_ride != 1) {
      /* toggle off */
      current_graphic[current_dindex] = current_var[current_dindex] = -1;
      return;
   }

   current_dindex = gtx->context_index;
   current_graphic[current_dindex] = graphic;
   current_var[current_dindex] = var;
   current_vindex[current_dindex] = vindex;
   /* current_vvar[current_dindex] is the var number according to the vis5d_ctx it belongs to */
   if (current_graphic[current_dindex]<6){
      current_vvar[current_dindex] = get_button_ctx_row(current_dindex, var);
   }
   else{
      current_vvar[current_dindex] = -1;
   }
   /* set the label string */
   if (graphic==VIS5D_ISOSURF) {
      vis5d_get_ctx_var_name( current_vindex[current_dindex], current_vvar[current_dindex], varname );
      sprintf( label, "%s isosurface color:", varname );
   }
   else if (graphic==VIS5D_TRAJ) {
      sprintf( label, "Trajectory set %d color:", var+1 );
   }
   else if (graphic==VIS5D_TOPO) {
      sprintf( label, "Topography color:" );
   }
   else {
      abort();
   }
   LUI_NewLabelChangeText( gtx->isocolor_label, label );

   /* Set RGBA sliders to appropriate color values */
   /* MJK 7.28.98
   ***************************************************
   if (current_graphic[current_dindex]>=0 && current_graphic[current_dindex]<=5){
      vis5d_get_color(current_dindex, current_graphic[current_dindex],
                       current_vindex[current_dindex]*MAXVARS+
                      current_vvar[current_dindex], &r,&g,&b,&a );
   }
   else{
      vis5d_get_color(current_dindex, current_graphic[current_dindex],
                      current_var[current_dindex], &r,&g,&b,&a );
   }

   LUI_NewSliderSetValue( gtx->iso_red_slider, r );
   LUI_NewSliderSetValue( gtx->iso_green_slider, g );
   LUI_NewSliderSetValue( gtx->iso_blue_slider, b );
   LUI_NewSliderSetValue( gtx->iso_alpha_slider, a );
   ****************************************************
   */

   if (graphic == VIS5D_ISOSURF || graphic == VIS5D_TRAJ){
      vis5d_get_color(current_dindex, current_graphic[current_dindex],
                       current_vindex[current_dindex]*MAXVARS+
                      current_vvar[current_dindex], &r,&g,&b,&a );
      LUI_NewSliderSetValue( gtx->iso_red_slider, r );
      LUI_NewSliderSetValue( gtx->iso_green_slider, g );
      LUI_NewSliderSetValue( gtx->iso_blue_slider, b );
      LUI_NewSliderSetValue( gtx->iso_alpha_slider, a );
   }


   /* turn off previous button */
   vis5d_get_ctx_numvars( vindex, &numvars );
   for (row=0;row<gtx->total_numvars+1;row++) {
      LUI_ButtonMatrixSetState( gtx->iso_button_matrix, row,
                                0, 0 );
   }

   /* turn on current button */
   /* and load colorbar widget with apropriate color table */
   if (current_graphic[current_dindex]==VIS5D_ISOSURF) {
      vis5d_get_isosurface_color_var( vindex, current_vvar[current_dindex], &cvowner, &colorvar );
      if (colorvar <0){
         curisovindex[current_dindex] = current_vindex[current_dindex];
         curisovrow[current_dindex]   = current_vvar[current_dindex];
      }
      else {
         vis5d_get_color_table_address( current_dindex, VIS5D_ISOSURF,
                                       cvowner, colorvar, &table );
         vis5d_get_color_table_params(current_dindex, VIS5D_ISOSURF, cvowner, colorvar, &p);
         vis5d_get_ctx_var_name( cvowner, colorvar, varname );
         vis5d_get_ctx_var_range( cvowner, colorvar, &min, &max );
         LUI_ColorBarChange( gtx->iso_colorbar,
                             varname, min, max, table, 255, p);
      }
   }
   else if (current_graphic[current_dindex]==VIS5D_TRAJ) {
      vis5d_get_trajectory_color_var( vindex, current_var[current_dindex], &cvowner, &colorvar );
      if (colorvar>=0) {
         vis5d_get_color_table_address( current_dindex, VIS5D_TRAJ,
                                       cvowner, colorvar, &table );
         vis5d_get_color_table_params(current_dindex, VIS5D_TRAJ, cvowner, colorvar, &p);
         vis5d_get_ctx_var_name( cvowner, colorvar, varname );
         vis5d_get_ctx_var_range( cvowner, colorvar, &min, &max );
         LUI_ColorBarChange( gtx->iso_colorbar,
                             varname, min, max, table, 255, p);
      }
   }
   else if (current_graphic[current_dindex]==VIS5D_TOPO) {
      int colorvarindex;

      vis5d_get_topo_color_var( current_dindex, &cvowner,  &colorvar );
      vis5d_get_color_table_address( current_dindex, VIS5D_TOPO,
                                     cvowner, colorvar, &table );
      vis5d_get_color_table_params(current_dindex, VIS5D_TOPO,
                                   cvowner, colorvar, &p);
      if (colorvar>=0) {
         vis5d_get_ctx_var_name( cvowner, colorvar, varname );
         vis5d_get_ctx_var_range( cvowner, colorvar, &min, &max );
      }
      else {
         vis5d_get_topo_range( current_dindex, &min, &max );
         strcpy( varname, "Elevation" );
      }
      LUI_ColorBarChange( gtx->iso_colorbar,
                          varname, min, max, table, 255, p);
   }
   else {
      /* should never get here */
      abort();
   }

   if (current_graphic[current_dindex]==VIS5D_TOPO) {
      LUI_ButtonMatrixChangeLabel( gtx->iso_button_matrix, 0, 0, "Height" );
   }
   else {
      LUI_ButtonMatrixChangeLabel( gtx->iso_button_matrix, 0, 0, "MonoColor" );
   }

   LUI_ButtonMatrixSetState( gtx->iso_button_matrix, colorvar+1, 0, 1 );
   curbutton[current_dindex] = get_button_gtx_index(current_dindex, cvowner, colorvar);
   curbutton[current_dindex] +=1;
   /* show either color sliders or color widget */
   XUnmapWindow( GuiDpy, gtx->isocolor_subwin1 );
   XUnmapWindow( GuiDpy, gtx->isocolor_subwin2 );
   if (colorvar>=0 || current_graphic[current_dindex]==VIS5D_TOPO) {
      XMapWindow( GuiDpy, gtx->isocolor_subwin2 );
   }
   else {
      XMapWindow( GuiDpy, gtx->isocolor_subwin1 );
   }

   XMapWindow( GuiDpy, gtx->isocolor_window );
}

#endif

/*
 * Call this function to udpate the sliders, color curves to current values.
 * This is usefull after a Vis5D RESTORE operation.
 */
void refresh_isocolor_window( GuiContext gtx )
{
   current_dindex = gtx->context_index;
   if (current_graphic[current_dindex]>=0 &&
       current_var[current_dindex]>=0 && is_valid( current_dindex, current_vindex[current_dindex])) {
      XSync( GuiDpy, 0 );

      show_isocolor_window( gtx, current_graphic[current_dindex],current_vindex[current_dindex],
                             current_var[current_dindex], 1 );
   }
}

