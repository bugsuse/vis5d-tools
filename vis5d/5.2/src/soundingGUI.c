/* soundingGUI.c */

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
 * This plots temperature and dewpoint on a skew-t or
 * plots up to three vertical plots of any variable
 */





#include <math.h>
#include "../lui5/lui.h"
#include <stdlib.h>
#include <strings.h>
#include "globals.h"
#include "graphics.h"
#include "grid.h"
#include "memory.h"
#include "proj.h"
#include "gui.h"
#include "sounding.h"
#include "soundingGUI.h"
#ifdef OPENGL
#  include <GL/gl.h>
#endif
#if defined(SGI_GL) || defined(DENALI)
#  include <gl/gl.h>
#endif
#include "uvwwidget.h"
#include "vis5d.h"

void make_lui_stuff( int index );

/* MJK 12.15.98 */
static int tempapply_cb( LUI_NEWBUTTON *b, int state );


static int thtaapply_cb( LUI_NEWBUTTON *b, int state );

static int thteapply_cb( LUI_NEWBUTTON *b, int state );

static int wapply_cb( LUI_NEWBUTTON *b, int state );

static int tickapply_cb(LUI_NEWBUTTON *b, int state );

static int sndapply_cb( LUI_NEWBUTTON *b, int state );



    /**********************************************************************/
    /* This makes all of the fancy gui labels and widgets                 */
    /**********************************************************************/
    /* Input: index - index of context                                    */
    /**********************************************************************/


void make_lui_stuff( int index )
{
   GuiContext gtx = get_gui_gtx(index);
   LUI_NEWBUTTON *b;
   Window w, sm1, sm2, sm3;
   int xover;
   int tempp;
   w = gtx->SoundCtrlWindow;
   

   LUI_BorderWidth(2);
   if ((gtx->vertsys > 0) && (gtx->oceanonly != 1)){
      if (gtx->snd_tempLABEL){
         LUI_NewLabelDestroy(gtx->snd_tempLABEL);
         LUI_FieldDestroy(gtx->snd_temp);
         LUI_NewButtonDestroy(gtx->thta_button);
         LUI_NewLabelDestroy(gtx->var1LABEL);
         LUI_FieldDestroy(gtx->snd_var1);
         LUI_NewLabelDestroy(gtx->snd_dewptLABEL);
         LUI_FieldDestroy(gtx->snd_dewpt);
         LUI_NewButtonDestroy(gtx->thte_button);
         LUI_NewLabelDestroy(gtx->var2LABEL);
         LUI_FieldDestroy(gtx->snd_var2);
         LUI_NewLabelDestroy(gtx->snd_u_windLABEL);
         LUI_FieldDestroy(gtx->snd_u_wind);
         LUI_NewButtonDestroy(gtx->w_button);
         LUI_NewLabelDestroy(gtx->var3LABEL);
         LUI_FieldDestroy(gtx->snd_var3);
         LUI_NewLabelDestroy(gtx->snd_v_windLABEL);
         LUI_FieldDestroy(gtx->snd_v_wind);
         LUI_NewButtonDestroy(gtx->tick_button);
         LUI_NewButtonDestroy(gtx->sndapply);


      }
      gtx->snd_tempLABEL = LUI_NewLabelCreate(w, LUI_LEFT, LUI_TOP, 175, 20, "Temperature Variable:" );
      gtx->snd_temp = LUI_FieldCreate(w, LUI_NEXT_X, LUI_SAME_Y, 80, 20 );
      /* MJK 12.15.98 begin */
      b = LUI_ToggleButtonCreate (w, LUI_NEXT_X, LUI_SAME_Y, 100, 20,
                                  "Temp Grid");
      LUI_ButtonCallback (b, tempapply_cb);
      LUI_ButtonIndex (b, 0);
      LUI_ButtonColor (b, 1.0, 1.0, 1.0);
      gtx->temp_button = b;
      /* MJK 12.15.98 end */

/*
      b = LUI_ToggleButtonCreate(w, LUI_NEXT_X, LUI_SAME_Y,
                                 100, 20, "Theta Lines");
      LUI_ButtonCallback( b, thtaapply_cb);
      LUI_ButtonIndex( b, 0);
      LUI_ButtonColor( b, .8, .196, .196);
      gtx->thta_button = b;
*/
      gtx->var1LABEL = LUI_NewLabelCreate(w, LUI_NEXT_X, LUI_SAME_Y, 175, 20, "Vertical Plot Var 1:");
      gtx->snd_var1 = LUI_FieldCreate(w, LUI_NEXT_X, LUI_SAME_Y, 80, 20 );



      gtx->snd_dewptLABEL = LUI_NewLabelCreate(w, LUI_LEFT, LUI_NEXT_Y, 175, 20, "Dewpoint Variable:" );
      gtx->snd_dewpt = LUI_FieldCreate(w, LUI_NEXT_X, LUI_SAME_Y, 80, 20 );

      /* MJK 12.15.98 begin */
      b = LUI_ToggleButtonCreate (w, LUI_NEXT_X, LUI_SAME_Y, 100, 20,
                                  "Theta Grid");
      LUI_ButtonCallback (b, thtaapply_cb);
      LUI_ButtonIndex (b, 0);
      LUI_ButtonColor (b, 1.0, 1.0, 1.0);
      gtx->thta_button = b;

      /*
      b = LUI_ToggleButtonCreate(w, LUI_NEXT_X, LUI_SAME_Y,
                                 100, 20, "Thete Lines");
      LUI_ButtonCallback( b, thteapply_cb);
      LUI_ButtonIndex( b, 0);
      LUI_ButtonColor( b, .0392, .549, .2157);
      gtx->thte_button = b;
*/

      gtx->var2LABEL = LUI_NewLabelCreate(w, LUI_NEXT_X, LUI_SAME_Y, 175, 20, "Vertical Plot Var 2:");
      gtx->snd_var2 = LUI_FieldCreate(w, LUI_NEXT_X, LUI_SAME_Y, 80, 20 );

 

      gtx->snd_u_windLABEL = LUI_NewLabelCreate(w, LUI_LEFT, LUI_NEXT_Y, 175, 20, "U Wind Variable:" );
      gtx->snd_u_wind = LUI_FieldCreate(w, LUI_NEXT_X, LUI_SAME_Y, 80, 20 );

      /* MJK 12.15.98 begin */      
      b = LUI_ToggleButtonCreate (w, LUI_NEXT_X, LUI_SAME_Y, 100, 20,
                                  "ThetaE Grid");
      LUI_ButtonCallback (b, thteapply_cb);
      LUI_ButtonIndex (b, 0);
      LUI_ButtonColor (b, 1.0, 1.0, 1.0);
      gtx->thte_button = b;

/*
      b = LUI_ToggleButtonCreate(w, LUI_NEXT_X, LUI_SAME_Y,
                                 100, 20, "  W Lines  ");
      LUI_ButtonCallback( b, wapply_cb);
      LUI_ButtonIndex( b, 0);
      LUI_ButtonColor( b, 1, 1, 0);
      gtx->w_button = b;
*/

      gtx->var3LABEL = LUI_NewLabelCreate(w, LUI_NEXT_X, LUI_SAME_Y, 175, 20, "Vertical Plot Var 3:");
      gtx->snd_var3 = LUI_FieldCreate(w, LUI_NEXT_X, LUI_SAME_Y, 80, 20 );

   

      gtx->snd_v_windLABEL = LUI_NewLabelCreate(w, LUI_LEFT, LUI_NEXT_Y, 175, 20, "V Wind Variable:" );
      gtx->snd_v_wind = LUI_FieldCreate(w, LUI_NEXT_X, LUI_SAME_Y, 80, 20 );

      /* MJK 12.15.98 begin */            
      b = LUI_ToggleButtonCreate (w, LUI_NEXT_X, LUI_SAME_Y, 100, 20,
                                  "MixRat Grid");
      LUI_ButtonCallback (b, wapply_cb);
      LUI_ButtonIndex (b, 0);
      LUI_ButtonColor (b, 1.0, 1.0, 1.0);
      gtx->w_button = b;



      /*Link the fields together for TABing */
      LUI_FieldLink( gtx->snd_temp, gtx->snd_dewpt);
      LUI_FieldLink( gtx->snd_dewpt,gtx->snd_u_wind);
      LUI_FieldLink( gtx->snd_u_wind,gtx->snd_v_wind);
      LUI_FieldLink( gtx->snd_v_wind, gtx->snd_var1);
      LUI_FieldLink( gtx->snd_var1, gtx->snd_var2);
      LUI_FieldLink( gtx->snd_var2, gtx->snd_var3);

      b = LUI_ToggleButtonCreate(w, LUI_NEXT_X, LUI_SAME_Y,
                              145, 20, "Show Tick Marks");
      LUI_ButtonCallback( b, tickapply_cb);
      LUI_ButtonIndex( b, 0);
      LUI_ButtonColor( b, 1, 1, 1);
      gtx->tick_button = b;

      gtx->sndapply = LUI_PushButtonCreate(w, LUI_NEXT_X, LUI_SAME_Y, 66, 20, " APPLY");
      LUI_ButtonCallback(gtx->sndapply, sndapply_cb );
   }
   else{
      if (gtx->snd_var2){
         LUI_NewLabelDestroy(gtx->var1LABEL);
         LUI_NewLabelDestroy(gtx->var2LABEL);
         LUI_NewLabelDestroy(gtx->var3LABEL);
         LUI_NewButtonDestroy(gtx->tick_button);
         LUI_NewButtonDestroy(gtx->sndapply);
      }

      gtx->var1LABEL = LUI_NewLabelCreate(w, LUI_LEFT, LUI_TOP, 175, 20, "Vertical Plot Var 1:");
      gtx->snd_var1 = LUI_FieldCreate(w, LUI_NEXT_X, LUI_SAME_Y, 80, 20 );

      gtx->var2LABEL = LUI_NewLabelCreate(w, LUI_LEFT, LUI_NEXT_Y, 175, 20, "Vertical Plot Var 2:");
      gtx->snd_var2 = LUI_FieldCreate(w, LUI_NEXT_X, LUI_SAME_Y, 80, 20 );



      gtx->var3LABEL = LUI_NewLabelCreate(w, LUI_LEFT, LUI_NEXT_Y, 175, 20, "Vertical Plot Var 3:");
      gtx->snd_var3 = LUI_FieldCreate(w, LUI_NEXT_X, LUI_SAME_Y, 80, 20 );

      /*Link the fields together for TABing */
      LUI_FieldLink( gtx->snd_var1, gtx->snd_var2);
      LUI_FieldLink( gtx->snd_var2, gtx->snd_var3);

      b = LUI_ToggleButtonCreate(w, LUI_LEFT, LUI_NEXT_Y,
                              165, 20, "Vertical Plot Ticks");
      LUI_ButtonCallback( b, tickapply_cb);
      LUI_ButtonIndex( b, 0);
      LUI_ButtonColor( b, 1, 1, 1);
      gtx->tick_button = b;

      gtx->sndapply = LUI_PushButtonCreate(w, LUI_NEXT_X, LUI_SAME_Y, 66, 20, " APPLY");
      LUI_ButtonCallback(gtx->sndapply, sndapply_cb );
   }

   if ((gtx->vertsys > 0) && (gtx->oceanonly != 1)){
      xover = 0;
   }
   else {
      xover = 364;
   }
   if (gtx->sm1){
      XDestroyWindow(GuiDpy, gtx->sm1);
      XDestroyWindow(GuiDpy, gtx->sm2);
      XDestroyWindow(GuiDpy, gtx->sm3);
   }

   /* MJK 12.15.98 */
   gtx->sm1 = XCreateSimpleWindow(GuiDpy, gtx->SoundCtrlWindow, 529-xover, 4,
                                  12, 15, 1, LUI_AllocateColorInt(0,0,0),
                                  LUI_AllocateColorInt(255, 255, 0));
   gtx->sm2 = XCreateSimpleWindow(GuiDpy, gtx->SoundCtrlWindow, 529-xover, 27,
                                  12, 15, 1, LUI_AllocateColorInt(0,0,0),
                                  LUI_AllocateColorInt(255, 0, 255));
   gtx->sm3 = XCreateSimpleWindow(GuiDpy, gtx->SoundCtrlWindow, 529-xover, 50,
                                  12, 15, 1, LUI_AllocateColorInt(0,0,0),
                                  LUI_AllocateColorInt(255, 255, 255));
/*

   gtx->sm1 = XCreateSimpleWindow(GuiDpy, gtx->SoundCtrlWindow, 529-xover, 4,
                       12, 15, 1, LUI_AllocateColorInt(0,0,0),
                       LUI_AllocateColorInt(0, 255, 0));
   gtx->sm3 = XCreateSimpleWindow(GuiDpy, gtx->SoundCtrlWindow, 529-xover, 50,
                       12, 15, 1, LUI_AllocateColorInt(0,0,0),
                       LUI_AllocateColorInt(255, 255, 100));
   gtx->sm2 = XCreateSimpleWindow(GuiDpy, gtx->SoundCtrlWindow, 529-xover, 27,
                       12, 15, 1, LUI_AllocateColorInt(0,0,0),
                       LUI_AllocateColorInt(100, 255, 255));
*/
   XMapWindow(GuiDpy, gtx->sm1);
   XMapWindow(GuiDpy, gtx->sm2);
   XMapWindow(GuiDpy, gtx->sm3);

}

    /**********************************************************************/
    /* This makes the main sounding window, where the gui stuff is added  */
    /* and where the ctx->Soundwin is mapped to.                          */
    /**********************************************************************/
    /* Input: index - index of context                                    */
    /*        xpos, ypos - coordinates for where to put the screen        */
    /*        width, height - dimensions for the window                   */
    /*        dontdraw - dont draw after making window                    */
    /**********************************************************************/

void make_sound_window(int index, int xpos, int ypos, int width, int height)
{
   XSetWindowAttributes attr;
   XSizeHints sizehints;
   XGCValues values;
   GuiContext gtx = get_gui_gtx(index);
   int attr_flags;
   Window w, sm1, sm2, sm3;
   XWindowAttributes xa, xa2;
   int xover;

   /* first make the Sounding Control window */
   if (!gtx->SoundCtrlWindow){
      w = LUI_CreateWindowAt( LUI_RootWindow, xpos, ypos, width, height);
      gtx->SoundCtrlWindow = w;

      /* MJK 12.15.98 */
      sizehints.x = xpos;
      sizehints.y = ypos;
/*

      sizehints.x = 50;
      sizehints.y = 50;
*/
      sizehints.width = width;
      sizehints.height= height;
      sizehints.max_height = 5500;
      sizehints.max_width = 5500;
      sizehints.flags = USSize | USPosition;
      XSetStandardProperties( GuiDpy, gtx->SoundCtrlWindow,
                              "Skew-T and Vertical Plot Display",
                              "Skew-T and Vertical Plot Display",
                               None, (char**)NULL, 0, &sizehints);
      XSetNormalHints(GuiDpy, gtx->SoundCtrlWindow, &sizehints);
      XSelectInput( GuiDpy, gtx->SoundCtrlWindow, ExposureMask 
                   | ButtonPressMask | ButtonReleaseMask
                   | StructureNotifyMask
                   | VisibilityChangeMask );

      /* Now add fancy options stuff at top of Sound Window  */
   }
   make_lui_stuff(index);
   if (gtx->how_many_regular_contexts >0){
      load_snd_widgets(index);
      read_snd_widgets( index );
   }
}

/* MJK 12.15.98 begin*/
static int tempapply_cb( LUI_NEWBUTTON *b, int state )
{
   int index = b->context_index;
   GuiContext gtx = get_gui_gtx(index);
   int onoroff;

   vis5d_graphics_mode( index, VIS5D_SND_TEMP, VIS5D_TOGGLE);
   vis5d_draw_sounding_only( index, 1 );
}
/* MJK 12.15.98 end */


static int thtaapply_cb( LUI_NEWBUTTON *b, int state )
{
   int index = b->context_index;
   GuiContext gtx = get_gui_gtx(index);
   int onoroff;

   vis5d_graphics_mode( index, VIS5D_SND_THTA, VIS5D_TOGGLE);
   vis5d_draw_sounding_only( index, 1 );
}

static int thteapply_cb( LUI_NEWBUTTON *b, int state )
{
   int index = b->context_index;
   GuiContext gtx = get_gui_gtx(index);
   int onoroff;

   vis5d_graphics_mode( index, VIS5D_SND_THTE, VIS5D_TOGGLE);
   vis5d_draw_sounding_only( index, 1 );
}

static int wapply_cb( LUI_NEWBUTTON *b, int state )
{
   int index = b->context_index;
   GuiContext gtx = get_gui_gtx(index);
   int onoroff;

   vis5d_graphics_mode( index, VIS5D_SND_W, VIS5D_TOGGLE);
   vis5d_draw_sounding_only( index, 1 );
}

static int tickapply_cb(LUI_NEWBUTTON *b, int state )
{
   int index = b->context_index;
   GuiContext gtx = get_gui_gtx(index);
   int onoroff;

   vis5d_graphics_mode( index, VIS5D_SND_TICKS, VIS5D_TOGGLE);
   vis5d_draw_sounding_only( index, 1 );
}




static int sndapply_cb( LUI_NEWBUTTON *b, int state )
{
   int index = b->context_index;
   GuiContext gtx = get_gui_gtx(index);

   read_snd_widgets( index );
   vis5d_draw_sounding_only( index, 1 );

   return 0;
}
static int invalid_ctx_index( int gindex, int vindex)
{
   GuiContext gtx = get_gui_gtx(gindex);
   int yo, spandex;

   for (yo = 0; yo < gtx->how_many_regular_contexts; yo++){
      spandex = gtx->array_of_ctxs[yo];
      if (spandex == vindex){
         return 0;
      }
   }
   return 1;
}
  
/* MJK 3.9.99 */
void read_snd_widgets( int index )
{
   GuiContext gtx = get_gui_gtx(index);
   char  varname[20];
   int var1, var2, var3, tmperature, uwnd, vwnd, dewpnt;
   int var1owner, var2owner, var3owner, tmpowner;
   int uwndowner, vwndowner, dewpntowner;
   int lp;
   char rap[30];
 
   LUI_FieldGetText( gtx->snd_var1, varname);
   var1owner = return_ctxindex(index,varname);
   if (var1owner < 0 || invalid_ctx_index(index, var1owner)){
      var1 = -1;
   }
   else{
      lp = return_ctxindex(index,varname);
      strcpy(rap, return_ctx_var(varname));
      var1 = vis5d_find_var(lp, rap );
      if (var1 == VIS5D_FAIL){
         var1 = -1;
      }
   }

   LUI_FieldGetText( gtx->snd_var2, varname);
   var2owner = return_ctxindex(index,varname);
   if (var2owner < 0  || invalid_ctx_index(index, var2owner)){
      var2 = -1;
   }
   else {
      lp = return_ctxindex(index,varname);
      strcpy(rap, return_ctx_var(varname));
      var2 = vis5d_find_var(lp, rap ); 
      if (var2 == VIS5D_FAIL || (var2 == var1 && var2owner == var1owner)){
         var2 = -1;
      }
   }

   LUI_FieldGetText( gtx->snd_var3, varname);
   var3owner = return_ctxindex(index,varname);
   if (var3owner < 0  || invalid_ctx_index(index, var3owner)){
      var3 = -1;
   }
   else{
      lp = return_ctxindex(index,varname);
      strcpy(rap, return_ctx_var(varname));
      var3 = vis5d_find_var(lp, rap ); 
      if (var3 == VIS5D_FAIL || (var3 == var2 && var3owner == var2owner)
                             || (var3 == var1 && var3owner == var2owner) ){
         var3 = -1;
      }
   }

   if ((gtx->vertsys != 0) && (gtx->oceanonly != 1)){
      LUI_FieldGetText( gtx->snd_temp, varname );
      tmpowner = return_ctxindex(index,varname);
      if (tmpowner < 0 || invalid_ctx_index(index, tmpowner)){
         tmperature = -1;
      }
      else{
         lp = return_ctxindex(index,varname);      
         strcpy(rap, return_ctx_var(varname));      
         tmperature = vis5d_find_var(lp, rap ); 
         if (tmperature == VIS5D_FAIL){
            tmperature = -1;
         }
      }
      LUI_FieldGetText( gtx->snd_u_wind, varname );
      uwndowner = return_ctxindex(index,varname);
      if (uwndowner < 0 || invalid_ctx_index(index, uwndowner)){
         uwnd = -1;
      }
      else{
         lp = return_ctxindex(index,varname);            
         strcpy(rap, return_ctx_var(varname));            
         uwnd = vis5d_find_var(lp, rap ); 
         if ( uwnd == VIS5D_FAIL){
            uwnd = -1; 
         }
      }
      LUI_FieldGetText( gtx->snd_v_wind, varname );
      vwndowner = return_ctxindex(index,varname);
      if (vwndowner < 0 || invalid_ctx_index(index, vwndowner)){
         vwnd = -1;
      }
      else{
         lp = return_ctxindex(index,varname);            
         strcpy(rap, return_ctx_var(varname));            
         vwnd = vis5d_find_var(lp, rap ); 
         if (vwnd == VIS5D_FAIL){
/* MJK 3.9.99 */
            vwnd = -1;
         }
      }
      LUI_FieldGetText( gtx->snd_dewpt,varname );
      dewpntowner = return_ctxindex(index,varname);
      if (dewpntowner < 0 || invalid_ctx_index(index, dewpntowner)){
         dewpnt = -1;
      }
      else{
         lp = return_ctxindex(index,varname);                     
         strcpy(rap, return_ctx_var(varname));                     
         dewpnt= vis5d_find_var(lp, rap ); 
         if (dewpnt == VIS5D_FAIL){
            dewpnt = -1;
         }
      }
   }
   if ((gtx->vertsys == 0) || (gtx->oceanonly == 1)){
      tmperature = -1;
      dewpnt = -1;
      uwnd = -1;
      vwnd = -1;
      tmpowner = -1;
      dewpntowner = -1;
      uwndowner = -1;
      vwndowner = -1;
   }
   vis5d_set_sound_vars( index, tmpowner, tmperature, dewpntowner, dewpnt,
                         uwndowner, uwnd, vwndowner, vwnd, var1owner, var1,
                         var2owner,  var2, var3owner, var3 );
   load_snd_widgets(index);

}

/* MJK 3.9.99 */
void load_snd_widgets( int index)
{
   GuiContext gtx = get_gui_gtx(index);
   char varname[20];
   int u, v, t, d, var1, var2, var3;
   int uo, vo, to, doo, var1o, var2o, var3o;
   char rap[30];

   vis5d_get_sound_vars( index, &to, &t, &doo, &d, &uo, &u,
                         &vo,  &v, &var1o, &var1, &var2o, &var2, &var3o, &var3);



/* WLH 6 Nov 98
   if (var1 >= 0){
*/
   /* WLH 6 Nov 98 */
   if (var1o >= 0 && var1 >= 0){
      vis5d_get_ctx_var_name( var1o, var1, varname);
   }
   else {
      varname[0] = '\0';
   }
   strcpy( rap, return_var_and_index(varname, var1o));
   LUI_FieldSetText( gtx->snd_var1,  rap);




/* WLH 6 Nov 98
   if (var2 >= 0){
*/
   /* WLH 6 Nov 98 */
   if (var2o >= 0 && var2 >= 0){
      vis5d_get_ctx_var_name( var2o, var2, varname);
   }
   else {
      varname[0] = 0;
   }
   strcpy( rap, return_var_and_index(varname, var2o));
   LUI_FieldSetText( gtx->snd_var2, rap);




/* WLH 6 Nov 98
   if (var3 >= 0){
*/
   /* WLH 6 Nov 98 */
   if (var3o >= 0 && var3 >= 0){
      vis5d_get_ctx_var_name( var3o, var3, varname);
   }
   else {
      varname[0] = 0;
   }
   strcpy( rap, return_var_and_index(varname, var3o));
   LUI_FieldSetText( gtx->snd_var3, rap);


   if((gtx->vertsys != 0) && (gtx->oceanonly != 1)){
/* WLH 6 Nov 98
      if (t >= 0){
*/
      /* WLH 6 Nov 98 */
      if (to >= 0 && t >= 0){
         vis5d_get_ctx_var_name( to, t, varname);
      }
      else {
         varname[0] = 0;
      }
      strcpy( rap, return_var_and_index(varname, to));
      LUI_FieldSetText( gtx->snd_temp, rap );
   

/* WLH 6 Nov 98
      if (d >= 0){
*/
      /* WLH 6 Nov 98 */
      if (doo >= 0 && d >= 0){
         vis5d_get_ctx_var_name( doo, d, varname);
      }
      else {
         varname[0] = 0;
      }
      strcpy( rap, return_var_and_index(varname, doo));   
      LUI_FieldSetText( gtx->snd_dewpt, rap);


/* WLH 6 Nov 98
      if (u >= 0){
*/
      /* WLH 6 Nov 98 */
      if (uo >= 0 && u >= 0){
         vis5d_get_ctx_var_name( uo, u, varname);
      }
      else{
         varname[0] = 0;
      }
      strcpy( rap, return_var_and_index(varname, uo));   
      LUI_FieldSetText( gtx->snd_u_wind, rap); 

/* WLH 6 Nov 98
      if (v >= 0){
*/
      /* WLH 6 Nov 98 */
      if (vo >= 0 && v >= 0){
         vis5d_get_ctx_var_name(vo, v, varname );
      }
      else{
         varname[0] = 0;
      }
      strcpy( rap, return_var_and_index(varname, vo));   
      LUI_FieldSetText( gtx->snd_v_wind, rap);
   }
}


/* MJK 12.07.98 begin */
int update_snd_widgets (int index)
{

    int         state;
    GuiContext  gtx = get_gui_gtx (index);



    load_snd_widgets (index);

    state = vis5d_graphics_mode (index, VIS5D_SND_TEMP, VIS5D_GET);
    LUI_ButtonSetState (gtx->temp_button, state);
    state = vis5d_graphics_mode (index, VIS5D_SND_THTA, VIS5D_GET);
    LUI_ButtonSetState (gtx->thta_button, state);
    state = vis5d_graphics_mode (index, VIS5D_SND_THTE, VIS5D_GET);
    LUI_ButtonSetState (gtx->thte_button, state);
    state = vis5d_graphics_mode (index, VIS5D_SND_MIXRAT, VIS5D_GET);
    LUI_ButtonSetState (gtx->w_button, state);


    return 0;
}
/* MJK 12.07.98 end */

