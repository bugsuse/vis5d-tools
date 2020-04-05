/* rgbsliders.c */

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
 * Widget for RGB color sliders.
 */



#include <stdio.h>
#include <string.h>
#include "X11/Xlib.h"
#include "../lui5/lui.h"
#include "api.h"
#include "graphics.h"
#include "gui.h"
#include "mwmborder.h"
#include "rgbsliders.h"


static int current_dindex = 0;
static int current_graphic[VIS5D_MAX_DPY_CONTEXTS];
static int current_var[VIS5D_MAX_DPY_CONTEXTS];
static int current_vindex[VIS5D_MAX_DPY_CONTEXTS];
static int current_vvar[VIS5D_MAX_DPY_CONTEXTS];
static int rgb_bottom[VIS5D_MAX_DPY_CONTEXTS], rgba_bottom[VIS5D_MAX_DPY_CONTEXTS];


static void init_currents(void)
{
   int yo;
   for (yo = 0; yo < VIS5D_MAX_DPY_CONTEXTS; yo++){
      current_graphic[yo] = -1;
      current_var[yo] = -1;
      current_vindex[yo] = -1;
      current_vvar[yo] = -1;
      rgb_bottom[yo] = 0;
      rgba_bottom[yo] = 0;
   }
}



/*
 * Change the color of a GUI button to (r,g,b).
 */
static void update_button_color( GuiContext gtx, float r, float g, float b )
{
   if (current_graphic[current_dindex]==VIS5D_HWIND) {
      LUI_ButtonColor( gtx->hwind_button[current_var[current_dindex]], r, g, b );
   }
   else if (current_graphic[current_dindex]==VIS5D_VWIND) {
      LUI_ButtonColor( gtx->vwind_button[current_var[current_dindex]], r, g, b );
   }
   else if (current_graphic[current_dindex]==VIS5D_HSTREAM) {
      LUI_ButtonColor( gtx->hstream_button[current_var[current_dindex]], r, g, b );
   }
   else if (current_graphic[current_dindex]==VIS5D_VSTREAM) {
      LUI_ButtonColor( gtx->vstream_button[current_var[current_dindex]], r, g, b );
   }
   else if (current_graphic[current_dindex]==VIS5D_HSLICE) {
      LUI_ButtonMatrixSetColor( gtx->ButtonMatrix, current_var[current_dindex], 1, r, g, b );
   }
   else if (current_graphic[current_dindex]==VIS5D_VSLICE) {
      LUI_ButtonMatrixSetColor( gtx->ButtonMatrix, current_var[current_dindex], 2, r, g, b );
   }
   else if (current_graphic[current_dindex]==VIS5D_CHSLICE) {
      LUI_ButtonMatrixSetColor( gtx->ButtonMatrix, current_var[current_dindex], 3, r, g, b );
   }
   else if (current_graphic[current_dindex]==VIS5D_CVSLICE) {
      LUI_ButtonMatrixSetColor( gtx->ButtonMatrix, current_var[current_dindex], 4, r, g, b );
   }

   /* MJK 12.04.98 */
   else if (current_graphic[current_dindex]==VIS5D_LIGHT_MAP) {

      LUI_ButtonColor( gtx->map_button, r, g, b );
   }
   else if (current_graphic[current_dindex]==VIS5D_DARK_MAP) {

      LUI_ButtonColor( gtx->map_button, r, g, b );
   }
   else if (current_graphic[current_dindex]==VIS5D_TRAJ) {
      LUI_ButtonSetColor( gtx->TrajButton[current_var[current_dindex]], (int)(r*255),
                          (int)(g*255), (int)(b*255) );
   }
}


/* called when red slider is changed */
static int red_slider_cb( LUI_NEWSLIDER *s, float value )
{
   GuiContext gtx = get_gui_gtx(current_dindex);
   float r, g, b, a;

   if (current_graphic[current_dindex]>=0 && current_graphic[current_dindex]<=5){
      vis5d_get_color( current_dindex, current_graphic[current_dindex], current_vindex[current_dindex]*MAXVARS+
                       current_vvar[current_dindex], &r,&g,&b,&a );
      r = value;
      vis5d_set_color( current_dindex, current_graphic[current_dindex], current_vindex[current_dindex]*MAXVARS+
                       current_vvar[current_dindex], r,g,b,a );
   }
   else{
      vis5d_get_color( current_dindex, current_graphic[current_dindex], current_var[current_dindex], &r,&g,&b,&a );
      r = value;
      vis5d_set_color( current_dindex, current_graphic[current_dindex], current_var[current_dindex], r,g,b,a );
   }

   vis5d_signal_redraw( current_dindex, 1 );
   update_button_color( gtx, r, g, b );
   return 0;
}



/* called when green slider is changed */
static int green_slider_cb( LUI_NEWSLIDER *s, float value )
{
   GuiContext gtx = get_gui_gtx(current_dindex);
   float r, g, b, a;

   if (current_graphic[current_dindex]>=0 && current_graphic[current_dindex]<=5){
      vis5d_get_color( current_dindex, current_graphic[current_dindex], current_vindex[current_dindex]*MAXVARS+
                       current_vvar[current_dindex], &r,&g,&b,&a );
      g = value;
      vis5d_set_color( current_dindex, current_graphic[current_dindex], current_vindex[current_dindex]*MAXVARS+
                       current_vvar[current_dindex], r,g,b,a );
   }
   else{
      vis5d_get_color( current_dindex, current_graphic[current_dindex], current_var[current_dindex], &r,&g,&b,&a );
      g = value;
      vis5d_set_color( current_dindex, current_graphic[current_dindex], current_var[current_dindex], r,g,b,a );
   }

   vis5d_signal_redraw( current_dindex, 1 );
   update_button_color( gtx, r, g, b );
   return 0;
}



/* called when blue slider is changed */
static int blue_slider_cb( LUI_NEWSLIDER *s, float value )
{
   GuiContext gtx = get_gui_gtx(current_dindex);
   float r, g, b, a;

   if (current_graphic[current_dindex]>=0 && current_graphic[current_dindex]<=5){
      vis5d_get_color( current_dindex, current_graphic[current_dindex], current_vindex[current_dindex]*MAXVARS+
                       current_vvar[current_dindex], &r,&g,&b,&a );
      b = value;
      vis5d_set_color( current_dindex, current_graphic[current_dindex], current_vindex[current_dindex]*MAXVARS+
                       current_vvar[current_dindex], r,g,b,a );
   }
   else{
      vis5d_get_color( current_dindex, current_graphic[current_dindex], current_var[current_dindex], &r,&g,&b,&a );
      b = value;
      vis5d_set_color( current_dindex, current_graphic[current_dindex], current_var[current_dindex], r,g,b,a );
   }

   vis5d_signal_redraw( current_dindex, 1 );
   update_button_color( gtx, r, g, b );
   return 0;
}



/* called when alpha slider is changed */
static int alpha_slider_cb( LUI_NEWSLIDER *s, float value )
{
   GuiContext gtx = get_gui_gtx(current_dindex);
   float r, g, b, a;

   if (current_graphic[current_dindex]>=0 && current_graphic[current_dindex]<=5){   
      vis5d_get_color( current_dindex, current_graphic[current_dindex], current_vindex[current_dindex]*MAXVARS+   
                       current_vvar[current_dindex], &r,&g,&b,&a );   
      a = value;
      vis5d_set_color( current_dindex, current_graphic[current_dindex], current_vindex[current_dindex]*MAXVARS+   
                       current_vvar[current_dindex], r,g,b,a );   
   }   
   else{   
      vis5d_get_color( current_dindex, current_graphic[current_dindex], current_var[current_dindex], &r,&g,&b,&a );   
      b = value;
      vis5d_set_color( current_dindex, current_graphic[current_dindex], current_var[current_dindex], r,g,b,a );   
   }   

   /* change the alpha value in the color lookup table */
   if (current_graphic[current_dindex]==VIS5D_CHSLICE) {
      int ia = (int)(a * 255.0);
      set_slice_alpha( current_dindex, VIS5D_CHSLICE, current_vindex[current_dindex], current_var[current_dindex], ia );
   }
   else if (current_graphic[current_dindex]==VIS5D_CVSLICE) {
      int ia = (int)(a * 255.0);
      set_slice_alpha( current_dindex, VIS5D_CVSLICE, current_vindex[current_dindex], current_var[current_dindex], ia );
   }

   vis5d_signal_redraw( current_dindex, 1 );
   return 0;
}



static int close_cb( LUI_NEWBUTTON *b, int state )
{
   GuiContext gtx = get_gui_gtx( current_dindex );

   XUnmapWindow( GuiDpy, gtx->rgb_sliders_window );
   current_vindex[current_dindex] =  current_graphic[current_dindex] = current_var[current_dindex] = -1;
   current_vvar[current_dindex] = -1; 
   return 0;
}


void make_rgb_sliders( GuiContext gtx )
{
   Window w;
   LUI_NEWSLIDER *s;
   LUI_NEWBUTTON *b;
   static int do_once = 1;


   if(do_once){
      do_once = 0;
      init_currents();
   }

   current_dindex = gtx->context_index;

   /* make the window */
   w = LUI_CreateWindowAt( LUI_RootWindow, 10, 800, 380, 144 );
   set_mwm_border( GuiDpy, w, MWM_DECOR_BORDER | MWM_DECOR_TITLE );

   gtx->rgb_sliders_window = w;

   /* make the label */
   gtx->rgb_sliders_label = LUI_NewLabelCreate( w, LUI_LEFT, LUI_TOP, 311, 20,
                                                "default label" );

   /* make the "Close" button */
   b = LUI_PushButtonCreate( w, LUI_NEXT_X, LUI_SAME_Y, 60, 20, "Close" );
   LUI_ButtonCallback( b, close_cb );

   /* MJK 12.04.98 begin*/
   /* make red slider */
   s = LUI_NewSliderCreate( w, LUI_LEFT, LUI_NEXT_Y, 374 );
   gtx->rgb_red_slider = s;
   LUI_NewSliderChange( s, "Red", NULL, 0.0, 1.0, 0.5 );
   LUI_NewSliderCallback( s, red_slider_cb );

   /* make green slider */
   s = LUI_NewSliderCreate( w, LUI_LEFT, LUI_NEXT_Y, 374 );
   gtx->rgb_green_slider = s;
   LUI_NewSliderChange( s, "Green", NULL, 0.0, 1.0, 0.5 );
   LUI_NewSliderCallback( s, green_slider_cb );

   /* make blue slider */
   s = LUI_NewSliderCreate( w, LUI_LEFT, LUI_NEXT_Y, 374 );
   gtx->rgb_blue_slider = s;
   LUI_NewSliderChange( s, "Blue", NULL, 0.0, 1.0, 0.5 );
   LUI_NewSliderCallback( s, blue_slider_cb );

   rgb_bottom[current_dindex] = LUI_LayoutGet( LUI_NEXT_Y );

   /* make alpha slider */
   s = LUI_NewSliderCreate( w, LUI_LEFT, LUI_NEXT_Y, 374 );
   gtx->rgb_alpha_slider = s;
   LUI_NewSliderChange( s, "Opacity", NULL, 0.0, 1.0, 0.5 );
   LUI_NewSliderCallback( s, alpha_slider_cb );
   /* MJK 12.04.98 end */

   rgba_bottom[current_dindex] = LUI_LayoutGet( LUI_NEXT_Y );
}




/*
 * Hide the RGB sliders window.
 */
void hide_rgb_sliders( GuiContext gtx )
{
    current_vindex[current_dindex] = current_graphic[current_dindex] = current_var[current_dindex] = -1;
   current_vvar[current_dindex] = -1; 
   XUnmapWindow( GuiDpy, gtx->rgb_sliders_window );
}




/*
 * Display the RGB sliders window.
 */
void show_rgb_sliders( GuiContext gtx, int graphic, int vindex, int var, int over_ride )
{
   float r, g, b, a;
   char label[100];

   if (current_dindex==gtx->context_index &&
       current_graphic[current_dindex]==graphic &&
       current_var[current_dindex]==var && current_vindex[current_dindex] == vindex &&
       over_ride != 1) {
      /* toggle the window off */
      XUnmapWindow( GuiDpy, gtx->rgb_sliders_window );
   current_graphic[current_dindex] = current_var[current_dindex] = current_vindex[current_dindex] = -1; 
      return;
   }
   else if (current_dindex>=0 && current_graphic[current_dindex]>=0 && current_var[current_dindex]>=0) {
      /* unmap window with old settings before showing new */
      XUnmapWindow( GuiDpy, gtx->rgb_sliders_window );
   }

   current_dindex = gtx->context_index;
   current_graphic[current_dindex] = graphic;
   current_var[current_dindex] = var;
   current_vindex[current_dindex] = vindex;
   if (current_graphic[current_dindex]<6){
      current_vvar[current_dindex] = get_button_ctx_row(current_dindex, var);
   } 

   if (current_graphic[current_dindex]>=0 && current_graphic[current_dindex]<=5){
      vis5d_get_color( current_dindex, current_graphic[current_dindex], current_vindex[current_dindex]*MAXVARS+
                       current_vvar[current_dindex], &r,&g,&b,&a );
   }
   else{
      vis5d_get_color( current_dindex, current_graphic[current_dindex], current_var[current_dindex], &r,&g,&b,&a );
   }

   if (graphic==VIS5D_LIGHT_MAP) {
      strcpy( label, "Light Map Color:" );
   }
   else if (graphic==VIS5D_DARK_MAP) {
      strcpy( label, "Dark Map Color:" );
   }
   else if (graphic==VIS5D_HWIND) {
      sprintf( label, "Horizontal Wind slice %d color:", var+1 );
   }
   else if (graphic==VIS5D_VWIND) {
      sprintf( label, "Vertical Wind slice %d color:", var+1 );
   }

   /* MJK 12.04.98 */
   else if (graphic==VIS5D_HSTREAM) {
      sprintf( label, "Horizontal Stream slice %d color:", var+1 );
   }
   else if (graphic==VIS5D_VSTREAM) {
      sprintf( label, "Vertical Stream slice %d color:", var+1 );
   }



   else if (graphic==VIS5D_HSLICE) {
      char varname[20];
      vis5d_get_ctx_var_name(current_vindex[current_dindex], current_vvar[current_dindex], varname );
      sprintf( label, "%s Horizontal Contour Slice color:", varname );
   }
   else if (graphic==VIS5D_VSLICE) {
      char varname[20];
      vis5d_get_ctx_var_name( current_vindex[current_dindex], current_vvar[current_dindex], varname );
      sprintf( label, "%s Vertical Contour Slice color:", varname );
   }
   else if (graphic==VIS5D_CHSLICE) {
      char varname[20];
      vis5d_get_ctx_var_name( current_vindex[current_dindex], current_vvar[current_dindex], varname );
      sprintf( label, "%s Horizontal Color Slice tickmark:", varname );
   }
   else if (graphic==VIS5D_CVSLICE) {
      char varname[20];
      vis5d_get_ctx_var_name( current_vindex[current_dindex], current_vvar[current_dindex], varname );
      sprintf( label, "%s Vertical Color Slice tickmark:", varname );
   }

   LUI_NewLabelChangeText( gtx->rgb_sliders_label, label );

   LUI_NewSliderSetValue( gtx->rgb_red_slider, r );
   LUI_NewSliderSetValue( gtx->rgb_green_slider, g );
   LUI_NewSliderSetValue( gtx->rgb_blue_slider, b );
   LUI_NewSliderSetValue( gtx->rgb_alpha_slider, a );

   if (graphic==VIS5D_CHSLICE || graphic==VIS5D_CVSLICE) {
      /* show alpha slider too */
      LUI_ResizeWindow( gtx->rgb_sliders_window, 380, rgba_bottom[current_dindex] );
   }
   else {
      /* don't show alpha slider */
      LUI_ResizeWindow( gtx->rgb_sliders_window, 380, rgb_bottom[current_dindex] );
   }

   XMapWindow( GuiDpy, gtx->rgb_sliders_window );
}


void refresh_rgb_sliders( GuiContext gtx )
{
   current_dindex = gtx->context_index;
   if (current_graphic[current_dindex]>=0 && current_var[current_dindex]>=0 &&
       current_vindex[current_dindex] >= 0) {
      hide_rgb_sliders( gtx );
      show_rgb_sliders( gtx, current_graphic[current_dindex], 
                current_vindex[current_dindex], current_var[current_dindex],  1 ); 
   }
}

void refresh_rgb_sliders2( GuiContext gtx )
{
   current_dindex = gtx->context_index;
   if (current_graphic[current_dindex]>=0 && current_var[current_dindex]>=0 &&
       current_vindex[current_dindex] >= 0 && is_valid(current_dindex, current_vindex[current_dindex])) {
      show_rgb_sliders( gtx, current_graphic[current_dindex],
                current_vindex[current_dindex], current_var[current_dindex],  1 );
   }
}

/* MJK 12.04.98 */

int get_current_rgbsliders (GuiContext gtx, int *p_graphic, int *p_var)
{

   int          index = gtx->context_index;



   *p_graphic = current_graphic[index];
   *p_var     = current_var[index];


   return current_dindex;
}

