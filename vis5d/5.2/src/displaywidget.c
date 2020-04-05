/* displaywidget.c */
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
 * This does.... I'm not sure yet. Ask me in a couple of weeks!
 */


#include <stdlib.h>
#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/keysym.h>
#include <unistd.h>
#include <string.h>
#include "../lui5/lui.h"
#include "api.h"
#include "displaywidget.h"
#include "gui.h"
#include "globals.h"
#include "vis5d.h"

#include "misc_i.h"
#include "file_i.h"
#include "gui_i.h"
#include "grid_i.h"
#include "memory_i.h"
#include "output_i.h"
#include "proj_i.h"
#include "projlist_i.h"
#include "select_i.h"
#include "v5d.h"

#include "graphics.h"
#include "memory.h"
#include "proj.h"
#include "gui.h"
#include "sounding.h"

#define VERT(X, Z) (X->VerticalSystem == 3 ? height_to_pressure(Z) : (Z))
#define INVERT(X, Z) (X->VerticalSystem == 3 ? pressure_to_height(Z) : (Z))

#define MAX_PRJ_FIELDS 8 

static v5dstruct *V5Dyo;

static char *current_ctx_name;

Window Dwindow;

static int dpycoords[16][2];
static int current_ctx;
static int browser_where_remove_ctx_is_being_applied;
static int display_where_remove_ctx_is_being_applied;
static LUI_BROWSER *ctx_browser[16];
static LUI_NEWLABEL *ctx_label[17];
static LUI_NEWLABEL *mlabel;
static LUI_NEWBUTTON *b11, *b12, *b13, *b22;
static LUI_NEWBUTTON *b23, *b32, *b33;
static LUI_NEWBUTTON *b34, *b43, *b44;
static LUI_NEWBUTTON *ok;
static LUI_NEWBUTTON *disperse;
static LUI_NEWBUTTON *delete;
static LUI_FIELD  *fvf[16];
static LUI_NEWLABEL  *fvl[16];
static LUI_FIELD  *fdf[16];
static LUI_NEWLABEL  *fdl[16];
static LUI_NEWLABEL  *ful[16];
static LUI_NEWBUTTON *fub[16];
static LUI_NEWBUTTON *opt_but[16];
static LUI_NEWLABEL  *sdp[16];
static LUI_FIELD *lfield, *cfield, *rfield;
static LUI_NEWBUTTON *vrt_button, *prj_button;
static LUI_NEWBUTTON *done_button;
static current_selected_ctx;
static current_selected_ctx_type;
static current_selected_browser_where_ctx_is_in;
static current_selected_display_where_ctx_is_in;
static void highlight_matrix_button( int mbutton);
static void show_current_matrix( void );
static int fub_cb( LUI_NEWBUTTON *b, int state);
static int opt_cb( LUI_NEWBUTTON *b, int state);
static int cfield_cb (LUI_FIELD *f, char *str );
static int lfield_cb (LUI_FIELD *f, char *str );
static int rfield_cb( LUI_FIELD *field, char *str );
static int prj_cb (LUI_NEWBUTTON *b, XEvent *event );
static int vrt_cb (LUI_NEWBUTTON *b, XEvent *event );
static int current_user_input_display;
static int vrt_scrollbar_cb( LUI_SCROLLBAR *sb, float pos );

static Window optwin;
static LUI_FIELD *first_area_field;
static LUI_FIELD *sequence_field;
static LUI_FIELD *alpha_field;
static LUI_FIELD *barbs_field;
static LUI_FIELD *boxx_field;
static LUI_FIELD *boxy_field;
static LUI_FIELD *boxz_field;
static LUI_FIELD *fontname_field;
static LUI_FIELD *fontsize;
static LUI_FIELD *funcpath_field;
static LUI_FIELD *hirestopo_field;
static LUI_FIELD *julian_field;
static LUI_FIELD *animrate_field;
static LUI_FIELD *texture_field;
static LUI_FIELD *toponame_field;
static LUI_FIELD *mapname_field;
static LUI_FIELD *log_flag_field;
static LUI_FIELD *log_scale_field;
static LUI_FIELD *log_exponent_field;
static LUI_FIELD *linewidth_field;
static LUI_FIELD *samescale_field;
static LUI_FIELD *lposition_field;
static LUI_FIELD *lsize_field;
static LUI_FIELD *legendx_field;
static LUI_FIELD *legendy_field;


static LUI_NEWBUTTON *finish_button;


static Window prjwin;
static LUI_RADIO *prj_radio;
static LUI_NEWLABEL *prjlabel[MAX_PRJ_FIELDS];
static LUI_FIELD    *prjfield[MAX_PRJ_FIELDS];
static LUI_NEWBUTTON *close_prj_button;

static LUI_RADIO *vrt_radio;
#define MAX_VRT_FIELDS 4
static LUI_NEWLABEL *vrtlabel[MAX_VRT_FIELDS];
static LUI_FIELD    *vrtfield[MAX_VRT_FIELDS];
static LUI_NEWBUTTON *close_vert_button;
static LUI_SCROLLBAR *vrt_scrollbar;

static void remove_all_browsers( void );
static void load_prj_widgets( v5dstruct *v5d );
static void load_vrt_widgets( v5dstruct *v5d );
static int fvf_cb( LUI_FIELD *field, char *str );
static int fdf_cb( LUI_FIELD *field, char *str );
static int kg;

/* WLH 13 Oct 98 - add this entire function 'destroy_cb' */
static LUI_NEWBUTTON *destroy;
static int destroy_cb( LUI_NEWBUTTON *b, int state )
{
  int Kurrant, i;
  int rows, cols, yo;
  GuiContext gtx, gtxi;

  if (current_selected_ctx >= 0) {

    /* WLH 21 Oct 98 */
    int numtrajs, *days = NULL, *times = NULL, *gps = NULL;
    float *lats = NULL, *lons = NULL, *alts = NULL;
   
    if (current_selected_ctx_type==REGULAR_TYPE){
       numtrajs = save_traj(display_where_remove_ctx_is_being_applied,
                         &lats, &lons, &alts, &days, &times, &gps);
       vis5d_destroy_data_context( current_selected_ctx );
    }
    else{
       vis5d_destroy_irregular_data_context( current_selected_ctx );
    }

    vis5d_create_display_context( display_where_remove_ctx_is_being_applied);
    make_another_gui( display_where_remove_ctx_is_being_applied, 1);
/* WLH 21 Oct 98
    recompute_graphics( display_where_remove_ctx_is_being_applied );
*/

    /* WLH 4 Nov 98 */
    vis5d_initialize_stuff(display_where_remove_ctx_is_being_applied);
    hide_widgets(display_where_remove_ctx_is_being_applied);

    /* WLH 21 Oct 98 */
    recompute_graphics( display_where_remove_ctx_is_being_applied,
                        numtrajs, lats, lons, alts, days, times, gps);

    vis5d_signal_redraw( display_where_remove_ctx_is_being_applied, 1 );
    gtx = get_gui_gtx( display_where_remove_ctx_is_being_applied );
    
    if (gtx->how_many_regular_contexts+gtx->how_many_irregular_contexts < 1){
       int whichones[VIS5D_MAX_CONTEXTS], howmany;
       int whichones2[VIS5D_MAX_CONTEXTS], howmany2;
       XUnmapWindow(GuiDpy, gtx->CpWindow);
       Current_Display = -1;
       for (i = 0; i < VIS5D_MAX_DPY_CONTEXTS; i++){
          gtxi = get_gui_gtx2(i);
          if (gtxi){
             vis5d_get_num_of_ctxs_in_display( i, &howmany, whichones);
             vis5d_get_num_of_itxs_in_display( i, &howmany2, whichones2);
             if (howmany+howmany2>0){
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
       }
       else{
          gtx = get_gui_gtx( Current_Display );
          update_button_states( gtx->context_index, 1);
          vis5d_signal_redraw( gtx->context_index, 1 );
       }
    }
    LUI_ContextBrowserActivate(ctx_browser[browser_where_remove_ctx_is_being_applied]); 
    current_selected_ctx = -1;
    current_selected_display_where_ctx_is_in = -1;
    current_selected_browser_where_ctx_is_in = -1;
    return 1;
  }
}


/*********************************************************************************/
/** this is the call back function when the 'Return to Vis5d' button is pressed **/
/*********************************************************************************/
static int ok_cb( LUI_NEWBUTTON *b, int state )
{
   int yo, rows, cols, Kurrant;
   XWindowAttributes winatts;
   int howmany, whichones[VIS5D_MAX_CONTEXTS];

   remove_all_browsers();
   LUI_NewLabelDestroy(mlabel);
   LUI_NewButtonDestroy(ok);
   LUI_NewButtonDestroy(b11);
   LUI_NewButtonDestroy(b12);
   LUI_NewButtonDestroy(b13);
   LUI_NewButtonDestroy(b22);
   LUI_NewButtonDestroy(b23);
   LUI_NewButtonDestroy(b32);
   LUI_NewButtonDestroy(b33);
   LUI_NewButtonDestroy(b34);
   LUI_NewButtonDestroy(b43);
   LUI_NewButtonDestroy(b44);
   for (yo = 0; yo < 16; yo++){
      if ( fvl[yo] ){
         LUI_NewButtonDestroy(fub[yo]);
         LUI_NewButtonDestroy(opt_but[yo]);
         LUI_NewLabelDestroy(fvl[yo]);
         LUI_NewLabelDestroy(ful[yo]);
         LUI_NewLabelDestroy(fdl[yo]);
         LUI_FieldDestroy(fvf[yo]);
         LUI_FieldDestroy(fdf[yo]);
         LUI_NewLabelDestroy(sdp[yo]);
      }
   }
   XUnmapWindow( GuiDpy, Dwindow ); 
   get_display_matrix( &rows, &cols);

   for (yo = 0; yo < rows*cols; yo++){
      vis5d_create_display_context(yo);
      vis5d_get_num_of_data_sets_in_display( yo, &howmany);
      if (howmany < 1){
         if (the_gui_dpy_name[0]==0){
            make_nodata_gui( yo, NULL, NULL);
         }
         else{
            make_nodata_gui( yo, the_gui_dpy_name, NULL);
         }
      }
      else{
         make_another_gui(yo, 0);
         hide_widgets(yo);
      }
   }
   unmap_all_windows();
   map_all_windows(0);
   map_fake_windows(0);
   get_current_display( &Kurrant );
   vis5d_get_num_of_data_sets_in_display( Kurrant, &howmany);
   if (howmany < 1){
      GuiContext gtx = get_gui_gtx(0);
      XMoveResizeWindow(GuiDpy, gtx->CpWindow,
      10, 32, CP_WIDTH, gtx->CpHeight);
      XMapWindow(GuiDpy, gtx->CpWindow);
   }
   update_button_states(Kurrant, 0);
   hide_widgets( Kurrant );
   show_widgets( Kurrant );
   return 0;
}

/*********************************************************************************/
/** This call back function is called when the user types a context number      **/
/** in the 'FROM Context #' field, it will then  change all the data ctx's that **/
/** belong to the display so that they have the same display values of the      **/
/** specified data ctx                                                          **/
/*********************************************************************************/
static int fvf_cb( LUI_FIELD *field, char *str )
{
   v5dstruct *yoman;
   int num;
  
   yoman = v5dNewStruct();
   num = atoi(str);
   vis5d_get_ctx_values( num, yoman);
   vis5d_set_dtx_values( field->display_index, yoman);
   LUI_FieldSetText( field, "");
   v5dFreeStruct( yoman );
}

/*********************************************************************************/
/** This call back function is called when the user types a context number in the*/
/** 'FROM Display' fiels, it will then change all the data ctx's that belong to **/
/** the display so that they have the same display values as that of the        **/
/** specified display                                                           **/
/*********************************************************************************/
static int fdf_cb( LUI_FIELD *field, char *str )
{
   int num;
   v5dstruct *yoman;

   yoman = v5dNewStruct();
   num = atoi(str);
   vis5d_get_dtx_values(num, yoman);
   vis5d_set_dtx_values(field->display_index, yoman);
   v5dFreeStruct( yoman);
   LUI_FieldSetText( field, "");
}

/*********************************************************************************/
/** This call back function is called when the 'Max Levels' field is changed.   **/
/** this field shows up after the user chooses the 'User Input' button          **/
/*********************************************************************************/
static int lfield_cb( LUI_FIELD *field, char *str )
{
   int nl;

   load_vrt_widgets( V5Dyo );
   nl = atoi(str);
   V5Dyo->Nl[0] = nl;
   return 0;
}


/*********************************************************************************/
/** This call back function is called when the user changes the value of the    **/
/** 'Rows' field                                                                **/
/*********************************************************************************/
static int rfield_cb( LUI_FIELD *field, char *str )
{
   int nr;

   nr = atoi( str );
   if (nr) {
      /* adjust projection to match new rows */
      switch (V5Dyo->Projection) {
         case 0:
         case 1:  /* rectilinear*/
         case 20:
         case 21:
            V5Dyo->ProjArgs[2] *= (float) V5Dyo->Nr / (float) nr;
            break;
         case 2:  /* lambert */
            V5Dyo->ProjArgs[2] *= (float) V5Dyo->Nr / (float) nr;
            break;
         case 3:  /* polar stereographic */
            V5Dyo->ProjArgs[2] *= (float) V5Dyo->Nr / (float) nr;
            break;
         default:
            /* ??? */
            ;
      }
      load_prj_widgets( V5Dyo );
      V5Dyo->Nr = nr;
   }

   return 0;
}

/*********************************************************************************/
/** This call back function is called when the user changes the values of the   **/
/** 'Columns' field                                                             **/
/*********************************************************************************/
static int cfield_cb( LUI_FIELD *field, char *str )
{
   int nc;

   nc = atoi( str );
   if (nc) {
      /* adjust projection to match new rows */
      switch (V5Dyo->Projection) {
         case 0:
         case 1:  /* rectilinear */
         case 20:
         case 21:
            V5Dyo->ProjArgs[3] *= (float) V5Dyo->Nc / (float) nc;
            break;
         case 2:  /* lambert */
            V5Dyo->ProjArgs[3] *= (float) V5Dyo->Nc / (float) nc;
            V5Dyo->ProjArgs[5] *= (float) V5Dyo->Nc / (float) nc;
            break;
         case 3:  /* polar stereographic */
            V5Dyo->ProjArgs[3] *= (float) V5Dyo->Nc / (float) nc;
            V5Dyo->ProjArgs[4] *= (float) V5Dyo->Nc / (float) nc;
            break;
         default:
            /* ??? */
            ;
      }
      load_prj_widgets( V5Dyo );
      V5Dyo->Nc = nc;
   }
   return 0;
}

/*********************************************************************************/
/** This is function is called when ever a list item is clicked on inside of the */
/** browsers.  This controls the clicking and placing of the contexts and the   **/
/** reassignment of display values if they need to be changed                   **/
/*********************************************************************************/

static int window_process( LUI_BROWSER *b, XEvent *event )
{
   /* WLH 13 Oct 98 */
   int num, i, k;
   int numtype;

   char *str;
   char str2[100];
   char numletter[5];
   switch (event->type) {
      case ButtonPress:
             /************************************************************/
             /*** if the user has a list item highlighted and tries to ***/
             /*** highlight another one, it won't let him/her do it    ***/
             /************************************************************/
          if (current_selected_browser_where_ctx_is_in != b->browser_number &&
              b->selected >= 0 && current_selected_ctx >= 0){
              b->selected = -1;
              show_current_matrix();
              current_selected_ctx = -1;
              current_selected_display_where_ctx_is_in = -1;
              current_selected_browser_where_ctx_is_in = -1;
              /* print error here saying, 'only one at a time highlighted */
           }
             /************************************************************/
             /*** this simply selects and highlights the ctx or list item*/
             /************************************************************/
           else if (b->selected >= 0){
               str = b->list->strings[b->selected];
/* WLH 6 Oct 98
               strcpy(str2, str  );
               strcpy(numletter, str2+8);
*/
               /* WLH 6 Oct 98 */
               strncpy(str2, str, 50);
               if (str2[0] == 'I'){
                  numtype = IRREGULAR_TYPE;
                  strncpy(numletter, str2+10, 5);
               }
               else{
                  numtype = REGULAR_TYPE;
                  strncpy(numletter, str2+8, 5);
               }


/* WLH 13 Oct 98
               numletter[1]= '\0';
*/
               
               /* WLH 13 Oct 98 - in case there are more than 10 data contexts */
               k = 4;
               for (i=1; i<5; i++) {
                 if (numletter[i] < '0' || '9' < numletter[i]) {
                   k = i;
                   break;
                 }
               }
               numletter[k]= '\0';

               num = atoi(numletter);
               current_selected_ctx = num; 
               current_selected_ctx_type = numtype;
               current_selected_browser_where_ctx_is_in = b->browser_number; 
               current_selected_display_where_ctx_is_in = b->display_number;
               browser_where_remove_ctx_is_being_applied= b->browser_number;
               display_where_remove_ctx_is_being_applied= b->display_number;
            }
             /************************************************************/
             /*** this deposits the highlighted ctx into the display, if */
             /*** it is the only one then is set's the display values to */
             /*** that of the ctx, if there is already a ctx attatched to*/
             /*** that display, then it will set the dpy_ctx of the new  */
             /*** ctx to the values of the display it is now in          */
             /************************************************************/
            else if (b->selected < 0 && current_selected_ctx >= 0){
               int sven, count, mount;
 
               /*check to see how many contexts belong to this display */
               sven = 0;
               for (count = 0; count < VIS5D_MAX_CONTEXTS; count++){
                  vis5d_get_ctx_display_index( count, &mount);
                  if (mount == b->display_number ){
                     sven++;
                  }
               }
               for (count = 0; count < VIS5D_MAX_CONTEXTS; count++){
                  vis5d_get_itx_display_index( count, &mount);
                  if (mount == b->display_number ){
                     sven++;
                  }
               }

               if (sven == 0){
                  char tname[1000];
                  char mname[1000];
                  /* it's the first one in there! */
                  /* carry over the map and topo file from last display! */
                  vis5d_get_topo(current_selected_display_where_ctx_is_in, tname);
                  vis5d_get_map(current_selected_display_where_ctx_is_in, mname);
                  vis5d_init_topo(b->display_number, tname, 0);
                  vis5d_init_map(b->display_number, mname);
                  if (current_selected_ctx_type ==REGULAR_TYPE){ 
                     vis5d_init_display_values( current_selected_ctx, -1, b->display_number);
                  }
                  else{
                     vis5d_init_display_values( -1, current_selected_ctx, b->display_number);
                  }
                  make_another_gui( b->display_number, 1); 
                  init_colortables( b->display_number );
               }
               else{
                  /*
                  turn_off_everything( current_selected_display_where_ctx_is_in ); 
                  */
                  if (current_selected_ctx_type ==REGULAR_TYPE){
                     vis5d_assign_display_to_data( current_selected_ctx, 
                                                   b->display_number);
                     init_some_colortables( b->display_number, current_selected_ctx);
                  }
                  else{
                     vis5d_assign_display_to_irregular_data( current_selected_ctx,
                                                   b->display_number);
                  }

               }   


               /* now check to see if the vis5d_ctx that was moved was the only one */
               /* in it's previous display, and if so, set that previous display's */
               /* values to zero */
               sven = 0;
               for (count = 0; count < VIS5D_MAX_CONTEXTS; count++){
                  vis5d_get_ctx_display_index( count, &mount);
                  if (mount == current_selected_display_where_ctx_is_in ){
                     sven++;
                  }
               }
               for (count = 0; count < VIS5D_MAX_CONTEXTS; count++){
                  vis5d_get_itx_display_index( count, &mount);
                  if (mount == current_selected_display_where_ctx_is_in ){
                     sven++;
                  }
               }
               if (sven == 0){
                  GuiContext gtx = get_gui_gtx2(current_selected_display_where_ctx_is_in); 
                  int Kurrant;

                  get_current_display( &Kurrant );
                  if (Kurrant==current_selected_display_where_ctx_is_in){
                     set_current_display( b->display_number );
                  }
                  vis5d_reset_display_context( current_selected_display_where_ctx_is_in);
                  unmap_gtx_CpWindow( current_selected_display_where_ctx_is_in);
                  if (gtx){
                     XUnmapWindow(GuiDpy, gtx->SoundCtrlWindow);
                  }
               }
               hide_widgets(current_selected_display_where_ctx_is_in);
               current_selected_ctx = -1;
               LUI_ContextBrowserActivate(ctx_browser[current_selected_browser_where_ctx_is_in]);
               current_selected_display_where_ctx_is_in = -1;
               LUI_ContextBrowserActivate(b); 
          }            
          break;
   }
   return 1;
}


/*********************************************************************************/
/** This will make all 16 browsers lists once                                   **/
/*********************************************************************************/
static void make_all_browsers(void)
{
   int yo, bnumber;

   for (yo = 0; yo < 16; yo++){
      /*vis5d_create_display_context(yo); */
      if (ctx_browser[yo] == NULL){
         bnumber = yo;
         ctx_browser[yo] = LUI_ContextBrowserCreate( Dwindow,dpycoords[bnumber][0],
                                                     dpycoords[bnumber][1], 200, 120, 0);
         /* LUI_BrowserCallback( ctx_browser[bnumber], read_ctx_ok_cb ); */
         ctx_browser[yo]->show = 0;
         LUI_EventAdd2(ctx_browser[bnumber]->list->window,
                       ExposureMask | ButtonPressMask | ButtonReleaseMask |
                       EnterWindowMask | LeaveWindowMask,
                       (LUI_FNCP) window_process, ctx_browser[bnumber] );
         ctx_browser[bnumber]->browser_number = bnumber;
         ctx_label[bnumber] = LUI_NewLabelCreate(Dwindow, dpycoords[bnumber][0]+48,
                                                 dpycoords[bnumber][1]-20,
                                                 180, 20, "          ");
      }
   }
}





/*********************************************************************************/
/* This will make the fields and preceeding text which are found under each     **/
/* browser list.                                                                **/
/*********************************************************************************/
static void make_ctx_browser( int bnumber, Window win, int x, int y, int display )
{

      sdp[bnumber]=LUI_NewLabelCreate(Dwindow, dpycoords[bnumber][0],
                         dpycoords[bnumber][1] + 120, 200, 15,
                         " SET DISPLAY PARAMETERS");

      fvl[bnumber]=LUI_NewLabelCreate(Dwindow, dpycoords[bnumber][0],
                         dpycoords[bnumber][1] + 137, 118, 15,
                         "FROM Context #");


      fvf[bnumber]=LUI_FieldCreate(Dwindow, dpycoords[bnumber][0]+116,
                               dpycoords[bnumber][1] + 135,  83, 20);
      fvf[bnumber]->display_index = display;
      LUI_FieldCallback( fvf[bnumber], fvf_cb );

   
      fdl[bnumber]=LUI_NewLabelCreate(Dwindow, dpycoords[bnumber][0],
                         dpycoords[bnumber][1] + 162, 118, 15,
                         "FROM Display #");
      fdf[bnumber]=LUI_FieldCreate(Dwindow, dpycoords[bnumber][0]+116,
                               dpycoords[bnumber][1] + 158,  83, 20);
      fdf[bnumber]->display_index = display;
      LUI_FieldCallback( fdf[bnumber], fdf_cb);



      ful[bnumber]=LUI_NewLabelCreate(Dwindow, dpycoords[bnumber][0],
                         dpycoords[bnumber][1] +187, 45, 15, 
                         "FROM");
      fub[bnumber]=LUI_PushButtonCreate(Dwindow, dpycoords[bnumber][0]+50,
                           dpycoords[bnumber][1] +182, 100, 20,
                           "User Input");
      LUI_ButtonCallback( fub[bnumber], fub_cb);
      fub[bnumber]->index = display;
 
      opt_but[bnumber] = LUI_PushButtonCreate(Dwindow,
                         dpycoords[bnumber][0] + 132,
                         dpycoords[bnumber][1]-19,
                         67,20,"Options");
      opt_but[bnumber]->index = display;
      LUI_ButtonCallback( opt_but[bnumber], opt_cb);


   ctx_browser[bnumber]->display_number = display;
}

   
/*
 * "Load" the current states of the VCS widgets from the
 * information in the given v5d struct.
 */
static void load_vrt_widgets( v5dstruct *v5d )
{
   int v, i, n;
   int maxnl;
   double size;

   switch (v5d->VerticalSystem) {
      case 0:  /* linear, equally spaced, generic */
         v = 0;
         n = 2;
         LUI_NewLabelChangeText( vrtlabel[0], "Bottom Bound:" );
         LUI_NewLabelChangeText( vrtlabel[1], "Level Increment:" );
         LUI_NewLabelChangeText( vrtlabel[2], "" );
         LUI_NewLabelChangeText( vrtlabel[3], "" );
         LUI_ScrollBarSet( vrt_scrollbar, 100.0, 0.0 );
         break;
      case 1:  /* equal km */
         v = 1;
         n = 2;
         LUI_NewLabelChangeText( vrtlabel[0], "Bottom Bound (km):" );
         LUI_NewLabelChangeText( vrtlabel[1], "Level Increment (km):" );
         LUI_NewLabelChangeText( vrtlabel[2], "" );
         LUI_NewLabelChangeText( vrtlabel[3], "" );
         LUI_ScrollBarSet( vrt_scrollbar, 100.0, 0.0 );
         break;
      case 2:  /* unequal km */
         v = 2;
         n = 4;
         LUI_NewLabelChangeText( vrtlabel[0], "Level 1 Height (km):" );
         LUI_NewLabelChangeText( vrtlabel[1], "Level 2 Height (km):" );
         LUI_NewLabelChangeText( vrtlabel[2], "Level 3 Height (km):" );
         LUI_NewLabelChangeText( vrtlabel[3], "Level 4 Height (km):" );
         maxnl = LUI_FieldGetInt( lfield );
         if (maxnl<=MAX_VRT_FIELDS) {
            size = 100.0;
         }
         else {
            size = 100 * MAX_VRT_FIELDS / maxnl;
         }
         LUI_ScrollBarSet( vrt_scrollbar, size, 0.0 );
         break;
      case 3:  /* unequal mb */
         v = 3;
         n = 4;
         LUI_NewLabelChangeText( vrtlabel[0], "Level 1 Pressure (mb):" );
         LUI_NewLabelChangeText( vrtlabel[1], "Level 2 Pressure (mb):" );
         LUI_NewLabelChangeText( vrtlabel[2], "Level 3 Pressure (mb):" );
         LUI_NewLabelChangeText( vrtlabel[3], "Level 4 Pressure (mb):" );
         maxnl = LUI_FieldGetInt( lfield );
         if (maxnl<=MAX_VRT_FIELDS) {
            size = 100.0;
         }
         else {
            size = 100 * MAX_VRT_FIELDS / maxnl;
         }
         LUI_ScrollBarSet( vrt_scrollbar, size, 0.0 );
         break;
      default:
         /* VCS not initialized yet */
         v = 0;
         n = 0;
   }

   if (LUI_RadioGetCurrent(vrt_radio) != v) {
      LUI_RadioSetCurrent( vrt_radio, v );
   }
   for (i=0;i<n;i++) {
      if (v5d->VertArgs[i] < 1){
         LUI_FieldSetDouble( vrtfield[i], VERT(v5d, v5d->VertArgs[i]) );
      }
      else {
         /* don't use this 
         LUI_FieldSetNotDouble( vrtfield[i], VERT(v5d, v5d->VertArgs[i]) );
         */
         LUI_FieldSetDouble( vrtfield[i], VERT(v5d, v5d->VertArgs[i]) );
      }
   }
   for (i=n;i<MAX_VRT_FIELDS;i++) {
      LUI_FieldSetText( vrtfield[i], "" );
   }
}




/*
 * "Load" the contents of the rows, columns, and max levels fields.
 */
static void Load_gridsize_widgets( v5dstruct *v5d )
{

   LUI_FieldSetInt( rfield, v5d->Nr );
   LUI_FieldSetInt( cfield, v5d->Nc );
   LUI_FieldSetInt( lfield, v5d->Nl[0] );
}



/* Called when user clicks on a map projection radio button. */
static int prj_radio_cb( LUI_RADIO *r, int current )
{
   static float proj_save[30][MAX_PRJ_FIELDS];
   static int init_flag[30] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                               0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
   int i;

   /* save current parameters for this projection */
   if (V5Dyo->Projection>=0) {
      for (i=0;i<MAX_PRJ_FIELDS;i++) {
         proj_save[V5Dyo->Projection][i] = LUI_FieldGetDouble( prjfield[i] );
      }
      init_flag[V5Dyo->Projection] = 1;
   }
   if (current == 5 ){
      V5Dyo->Projection = PROJ_CYLINDRICAL;
      current = PROJ_CYLINDRICAL;
   }
   else if (current == 6){
      V5Dyo->Projection = PROJ_SPHERICAL;
      current = PROJ_SPHERICAL;
   }
   if (current == 7 ){
      V5Dyo->Projection = PROJ_MERCATOR;
      current = PROJ_MERCATOR;
   }
   else{
      V5Dyo->Projection = current;
   }

   /* restore parameters for this projection */
   if (current>=0) {
      for (i=0;i<MAX_PRJ_FIELDS;i++) {
         if (init_flag[current]) {
            V5Dyo->ProjArgs[i] = proj_save[current][i];
         }
         else if ((V5Dyo->Projection == PROJ_CYLINDRICAL ||
                  V5Dyo->Projection == PROJ_SPHERICAL) &&
                  init_flag[1] == 1){
            V5Dyo->ProjArgs[i] = proj_save[1][i];
         }
         else{
            V5Dyo->ProjArgs[i] = 0.0;
         }
      }
   }

   load_prj_widgets( V5Dyo );
   return 0;
}

/* Called when user clicks on a VCS radio button. */
static int vrt_radio_cb( LUI_RADIO *r, int current )
{

   vrt_scrollbar_cb(vrt_scrollbar, 0);
   if ((current==2 || current==3) &&
       (V5Dyo->VerticalSystem!=2 && V5Dyo->VerticalSystem!=3)) {
      /* switching to unequal km */
      if (V5Dyo->VertArgs[2]==0.0) {
         float bottom, delta;
         int i, maxnl;

         /* setup nice defaults */
         bottom = V5Dyo->VertArgs[0];
         delta = V5Dyo->VertArgs[1] - V5Dyo->VertArgs[0];

         maxnl = LUI_FieldGetInt( lfield );

         for (i=2;i<maxnl;i++) {
            V5Dyo->VertArgs[i] = bottom + i * delta;
         }
      }
   }

   V5Dyo->VerticalSystem = current;
   load_vrt_widgets( V5Dyo );
   return 0;
}

static void load_prj_values( v5dstruct *v5d )
{
   int p, i, n;

   switch (v5d->Projection) {
      case PROJ_GENERIC:
         p = 0;
         n = 4;
         break;
      case PROJ_LINEAR:
         p = 1;
         n = 4;
         break;
      case PROJ_LAMBERT:
         p = 2;
         n = 6;
         break;
      case PROJ_STEREO:
         p = 3;
         n = 5;
         break;
      case PROJ_ROTATED:
         p = 4;
         n = 7;
         break;
      case PROJ_CYLINDRICAL:
         p = 5;
         n = 4;
         break;
      case PROJ_SPHERICAL:
         p = 6;
         n = 4;
         break;
      case PROJ_MERCATOR:
         p = 7;
         n = 4;
/* MJK 12.12.99 */
         break;
      default:
         /* Projection not initialized yet */
         p = 0;
         n = 0;
   }
   for (i=0;i<n;i++) {
      if (v5d->ProjArgs[i] < 1.0){
         LUI_FieldSetDouble( prjfield[i], v5d->ProjArgs[i] );
      }
      else
         LUI_FieldSetDouble( prjfield[i], v5d->ProjArgs[i] );
         
/*
         Don't use this MJK 9 - 7 - 98
         LUI_FieldSetNotDouble( prjfield[i], v5d->ProjArgs[i] );
*/
      }
   for (i=n;i<MAX_PRJ_FIELDS;i++) {
      LUI_FieldSetText( prjfield[i], "" );
   }

}



static void load_prj_widgets( v5dstruct *v5d )
{
   int p, i, n;

   switch (v5d->Projection) {
      case PROJ_GENERIC:
         p = 0;
         n = 4;
         LUI_NewLabelChangeText( prjlabel[0], "North Bound:" );
         LUI_NewLabelChangeText( prjlabel[1], "West Bound:" );
         LUI_NewLabelChangeText( prjlabel[2], "Row Increment:" );
         LUI_NewLabelChangeText( prjlabel[3], "Column Increment:" );
         LUI_NewLabelChangeText( prjlabel[4], "" );
         LUI_NewLabelChangeText( prjlabel[5], "" );
         LUI_NewLabelChangeText( prjlabel[6], "" );
         break;
      case PROJ_LINEAR:
         p = 1;
         n = 4;
         LUI_NewLabelChangeText( prjlabel[0], "North Bound (deg):" );
         LUI_NewLabelChangeText( prjlabel[1], "West Bound (deg):" );
         LUI_NewLabelChangeText( prjlabel[2], "Row Increment (deg):" );
         LUI_NewLabelChangeText( prjlabel[3], "Column Increment (deg):" );
         LUI_NewLabelChangeText( prjlabel[4], "" );
         LUI_NewLabelChangeText( prjlabel[5], "" );
         LUI_NewLabelChangeText( prjlabel[6], "" );
         break;
      case PROJ_LAMBERT:
         p = 2;
         n = 6;
         LUI_NewLabelChangeText( prjlabel[0], "Latitude 1:" );
         LUI_NewLabelChangeText( prjlabel[1], "Latitude 2:" );
         LUI_NewLabelChangeText( prjlabel[2], "Pole Row:" );
         LUI_NewLabelChangeText( prjlabel[3], "Pole Column:" );
         LUI_NewLabelChangeText( prjlabel[4], "Central Longitude:" );
         LUI_NewLabelChangeText( prjlabel[5], "Column Increment (km):" );
         LUI_NewLabelChangeText( prjlabel[6], "" );
         break;
      case PROJ_STEREO:
         p = 3;
         n = 5;
         LUI_NewLabelChangeText( prjlabel[0], "Central Latitude:" );
         LUI_NewLabelChangeText( prjlabel[1], "Central Longitude:" );
         LUI_NewLabelChangeText( prjlabel[2], "Central Row:" );
         LUI_NewLabelChangeText( prjlabel[3], "Central Column:" );
         LUI_NewLabelChangeText( prjlabel[4], "Column Increment (km):" );
         LUI_NewLabelChangeText( prjlabel[5], "" );
         LUI_NewLabelChangeText( prjlabel[6], "" );
         break;
      case PROJ_ROTATED:
         p = 4;
         n = 7;
         LUI_NewLabelChangeText( prjlabel[0], "Lat of grid row 0:" );
         LUI_NewLabelChangeText( prjlabel[1], "Lon of grid col 0:" );
         LUI_NewLabelChangeText( prjlabel[2], "Row increment (deg):" );
         LUI_NewLabelChangeText( prjlabel[3], "Col increment (deg):" );
         LUI_NewLabelChangeText( prjlabel[4], "Lat of (0,0):" );
         LUI_NewLabelChangeText( prjlabel[5], "Lon of (0,0):" );
         LUI_NewLabelChangeText( prjlabel[6], "Rotation (deg):" );
         break;
      case PROJ_CYLINDRICAL:
         p = 5;
         n = 4;
         LUI_NewLabelChangeText( prjlabel[0], "North Bound:" );
         LUI_NewLabelChangeText( prjlabel[1], "West Bound:" );
         LUI_NewLabelChangeText( prjlabel[2], "Row Increment:" );
         LUI_NewLabelChangeText( prjlabel[3], "Column Increment:" );
         LUI_NewLabelChangeText( prjlabel[4], "" );
         LUI_NewLabelChangeText( prjlabel[5], "" );
         LUI_NewLabelChangeText( prjlabel[6], "" );
         break;
      case PROJ_SPHERICAL:
         p = 6;
         n = 4;
         LUI_NewLabelChangeText( prjlabel[0], "North Bound:" );
         LUI_NewLabelChangeText( prjlabel[1], "West Bound:" );
         LUI_NewLabelChangeText( prjlabel[2], "Row Increment:" );
         LUI_NewLabelChangeText( prjlabel[3], "Column Increment:" );
         LUI_NewLabelChangeText( prjlabel[4], "" );
         LUI_NewLabelChangeText( prjlabel[5], "" );
         LUI_NewLabelChangeText( prjlabel[6], "" );
         break;
      case PROJ_MERCATOR:
         p = 7;
         n = 4;
         LUI_NewLabelChangeText( prjlabel[0], "Center Lat:" );
         LUI_NewLabelChangeText( prjlabel[1], "Center Lon:" );
         LUI_NewLabelChangeText( prjlabel[2], "Row Inc in Km:" );
         LUI_NewLabelChangeText( prjlabel[3], "Col Inc in Km:" );
         LUI_NewLabelChangeText( prjlabel[4], "" );
         LUI_NewLabelChangeText( prjlabel[5], "" );
         LUI_NewLabelChangeText( prjlabel[6], "" );
         break;
      default:
         /* Projection not initialized yet */
         p = 0;
         n = 0;
   }
   if (LUI_RadioGetCurrent(prj_radio) != p) {
      LUI_RadioSetCurrent( prj_radio, p );
   }
   for (i=0;i<n;i++) {
      LUI_FieldSetDouble( prjfield[i], v5d->ProjArgs[i] );
   }
   for (i=n;i<MAX_PRJ_FIELDS;i++) {
      LUI_FieldSetText( prjfield[i], "" );
   }

}


/* Called when the value of a projection parameter field changes. */
static int prj_field_cb( LUI_FIELD *field, char *str )
{
   int i;

   for (i=0;i<MAX_PRJ_FIELDS;i++) {
      if (field==prjfield[i]) {
         V5Dyo->ProjArgs[i] = atof( str );
         break;
      }
   }
   return 0;
}


/* Called when the value of a VCS parameter field changes. */
static int vrt_field_cb( LUI_FIELD *field, char *str )
{
   int i;
   int maxnl, top;

   if (LUI_RadioGetCurrent( vrt_radio )==2) {
      maxnl = LUI_FieldGetInt( lfield );
      if (maxnl<=MAX_VRT_FIELDS) {
         top = 0;
      }
      else {
         double pos = LUI_ScrollBarGetPos( vrt_scrollbar );
         top = (int) (pos / 100.0 * (maxnl-MAX_VRT_FIELDS));
      }
   }
   else {
      top = 0;
   }

   for (i=0;i<MAX_VRT_FIELDS;i++) {
      if (field==vrtfield[i]) {
         V5Dyo->VertArgs[top+i] = INVERT(V5Dyo, atof( str ));
         break;
      }
   }
   return 0;
}


/*
 * Called when the scrollbar in the vertical coordinate system window
 * is moved.
 */
static int vrt_scrollbar_cb( LUI_SCROLLBAR *sb, float pos )
{
   int maxnl, top, i;

   maxnl = LUI_FieldGetInt( lfield );
   if (maxnl<=MAX_VRT_FIELDS) {
      top = 0;
   }
   else {
      top = (int) (pos / 100.0 * (maxnl-MAX_VRT_FIELDS));
   }

   for (i=0;i<MAX_VRT_FIELDS;i++) {
      char str[100];
      sprintf( str, "Level %d Height (km):", top+i+1 );
      LUI_NewLabelChangeText( vrtlabel[i], str );
      LUI_FieldSetDouble( vrtfield[i], VERT(V5Dyo, V5Dyo->VertArgs[top+i] ));
   }
   return 0;
}



static int finish_cb( LUI_NEWBUTTON *button, XEvent *event )
{
   int dindex;
   GuiContext gtx;
   char name[1000];

   dindex  = current_user_input_display;
   gtx = get_gui_gtx(dindex);
   kg = 0;
   vis5d_init_firstarea( dindex, LUI_FieldGetInt(first_area_field));
   LUI_FieldGetText( sequence_field, name);
   vis5d_init_sequence( dindex, name);
   vis5d_alpha_mode( dindex, LUI_FieldGetInt(alpha_field));
   vis5d_graphics_mode(dindex,  VIS5D_BARBS, LUI_FieldGetInt(barbs_field));
   vis5d_make_box( dindex, (float) (LUI_FieldGetDouble(boxx_field)),
                           (float) (LUI_FieldGetDouble(boxy_field)),
                           (float) (LUI_FieldGetDouble(boxz_field)));
   LUI_FieldGetText( fontname_field, name);
    
   vis5d_font( dindex, name, LUI_FieldGetInt(fontsize));
   LUI_FieldGetText(funcpath_field, gtx->funcpath);
   vis5d_graphics_mode( dindex, VIS5D_JULIAN, LUI_FieldGetInt(julian_field));
   gtx->AnimRate = LUI_FieldGetInt(animrate_field);
   
   LUI_FieldGetText(texture_field, name);
   vis5d_init_texture( dindex, name);
   LUI_FieldGetText(toponame_field, name);
   vis5d_init_topo( dindex, name, 
                            LUI_FieldGetInt(hirestopo_field));
   LUI_FieldGetText(mapname_field, name);
   vis5d_init_map( dindex, name);
   vis5d_init_log( dindex, LUI_FieldGetInt(log_flag_field),
                           (float) (LUI_FieldGetDouble(log_scale_field)),
                           (float) (LUI_FieldGetDouble(log_exponent_field)));
   vis5d_linewidth( dindex, (float)(LUI_FieldGetDouble(linewidth_field)));
   vis5d_graphics_mode(dindex, VIS5D_SAMESCALE, LUI_FieldGetInt(samescale_field));
   vis5d_set_legends( dindex, LUI_FieldGetInt(lposition_field),
                              LUI_FieldGetInt(lsize_field),
                              LUI_FieldGetInt(legendx_field),
                              LUI_FieldGetInt(legendy_field));
   vis5d_load_topo_and_map( dindex ); 
   return 0;
}

static int done_cb( LUI_NEWBUTTON *button, XEvent *event )
{

   /* WLH 21 Oct 98 */
   int numtrajs, *days = NULL, *times = NULL, *gps = NULL;
   float *lats = NULL, *lons = NULL, *alts = NULL;
   numtrajs = save_traj(current_user_input_display,
                        &lats, &lons, &alts, &days, &times, &gps);


   vis5d_set_dtx_values(current_user_input_display, V5Dyo); 

   /* WLH 26 Oct 98 */
   vis5d_initialize_stuff(current_user_input_display);

/* WLH 21 Oct 98
   recompute_graphics( current_user_input_display );
*/
   /* WLH 21 Oct 98 */
   recompute_graphics( current_user_input_display,
                       numtrajs, lats, lons, alts, days, times, gps);

   /*   turn_off_everything(current_user_input_display); */
   /* BUG FIX MJK 8.10.98 */
   /* added hide_widgets and turn_off_everything to this */
   update_button_states( current_user_input_display, 1);
   hide_widgets(current_user_input_display);
   prj_radio_cb(prj_radio, V5Dyo->Projection );
   vrt_radio_cb(vrt_radio, V5Dyo->VerticalSystem);
   v5dFreeStruct(V5Dyo);
   V5Dyo = NULL;
   kg = 0;
}


static int load_opt( int dindex )
{
   char name[300];
   float x1, y1, z1, x2, y2, z2, scale, exponent;
   int size, flag;
   int fa, fb, fc, fd, fe;

   GuiContext gtx = get_gui_gtx(dindex);

   fa = vis5d_get_firstarea( dindex );
   LUI_FieldSetInt( first_area_field, fa);
   vis5d_get_sequence( dindex, name);
   LUI_FieldSetText( sequence_field, name );
   LUI_FieldSetInt( alpha_field, vis5d_graphics_mode(dindex,
                                     VIS5D_ALPHAMODE, VIS5D_GET));
   LUI_FieldSetInt( barbs_field, vis5d_graphics_mode(dindex,
                                     VIS5D_BARBS, VIS5D_GET));
   vis5d_get_box_bounds( dindex, &x1, &x2, &y1, &y2, &z1, &z2);
   {
      /* get aspect ratios */
      if (x2>=y2 && x2>=z2) {
         y2 = y2 / x2;
         z2 = z2 / x2;
         x2 = 1.0;
      }
      else if (y2>=x2 && y2>=z2) {
         x2 = x2 / y2;
         z2 = z2 / y2;
         y2 = 1.0;
      }
      else {
         x2 = x2 / z2;
         y2 = y2 / z2;
         z2 = 1.0;
      }
   }
   LUI_FieldSetDouble( boxx_field, (double) x2);
   LUI_FieldSetDouble( boxy_field, (double) y2);
   LUI_FieldSetDouble( boxz_field, (double) z2);
   size = 0;
   name[0] = 0;
   vis5d_get_font( dindex, name, &size);
   LUI_FieldSetText( fontname_field, name);
   LUI_FieldSetInt( fontsize, size);
   LUI_FieldSetText( funcpath_field, gtx->funcpath);
   LUI_FieldSetInt( hirestopo_field, vis5d_graphics_mode(dindex,
                                     VIS5D_HIRESTOPO, VIS5D_GET));
   LUI_FieldSetInt( julian_field, vis5d_graphics_mode(dindex,
                                     VIS5D_JULIAN, VIS5D_GET));
   LUI_FieldSetInt( animrate_field, gtx->AnimRate);
   name[0] = 0;
   vis5d_get_texture(dindex, name);
   LUI_FieldSetText( texture_field, name);
   name[0] = 0;
   vis5d_get_topo( dindex, name);
   LUI_FieldSetText( toponame_field, name);
   vis5d_get_map( dindex, name);
   LUI_FieldSetText( mapname_field, name);
   vis5d_get_log( dindex, &flag, &scale, &exponent);
   LUI_FieldSetInt( log_flag_field, flag);
   LUI_FieldSetDouble( log_scale_field, (double) scale);
   LUI_FieldSetDouble( log_exponent_field, (double) exponent);
   vis5d_get_linewidth( dindex, &x1);
   LUI_FieldSetDouble( linewidth_field, (double) x1);
   LUI_FieldSetInt( samescale_field, vis5d_graphics_mode(dindex,
                                     VIS5D_SAMESCALE, VIS5D_GET));
   vis5d_get_legends( dindex, &fb, &fc, &fd, &fe);
   LUI_FieldSetInt( lposition_field, fb);
   LUI_FieldSetInt( lsize_field, fc);
   LUI_FieldSetInt( legendx_field, fd);
   LUI_FieldSetInt( legendy_field, fe);
   return 0;
}


                                       
   
      
static int opt_cb( LUI_NEWBUTTON *b, int state)
{
   XEvent event;
   int mat, is_there;

   vis5d_get_ctx_display_index(b->index, &is_there);
   if (is_there < 0){
      return -1;
   }

   kg = 1;
   if ( DisplayRows <= 4 && DisplayCols <= 4){
      mat = (DisplayRows * 10) + DisplayCols;
   }
   else{
      return -1;
   }
   switch(mat){
      case 11:
         XResizeWindow(GuiDpy, Dwindow, 568, 987);
         break;
      case 12:
         XResizeWindow(GuiDpy, Dwindow, 788, 987);
         break;
      case 22:
         XResizeWindow(GuiDpy, Dwindow, 788, 987);
         break;
      case 23:
         XResizeWindow(GuiDpy, Dwindow, 1009, 987);
         break;
      case 32:
         XResizeWindow(GuiDpy, Dwindow, 788, 987);
         break;
      case 33:
         XResizeWindow(GuiDpy, Dwindow, 1009, 987);
         break;
      case 34:
         XResizeWindow(GuiDpy, Dwindow, 1264, 987);
         break;
      case 43:
         XResizeWindow(GuiDpy, Dwindow, 1009, 987);
         break;
      case 44:
         XResizeWindow(GuiDpy, Dwindow, 1264, 987);
         break;
   }

   current_user_input_display = b->index;

   if (optwin == NULL){
      optwin = LUI_CreateWindowAt(Dwindow, 5, 140, 300, 800);
      XMapWindow( GuiDpy, optwin);

      LUI_NewLabelCreate( optwin, LUI_LEFT, LUI_TOP, 175, 26, "first_area number:");
      first_area_field = LUI_FieldCreate( optwin, LUI_NEXT_X, LUI_SAME_Y, 70, 26 );


      LUI_NewLabelCreate( optwin, LUI_LEFT, LUI_NEXT_Y, 175, 26, "sequence file:");
      sequence_field = LUI_FieldCreate( optwin, LUI_NEXT_X, LUI_SAME_Y, 70, 26 );
      

      LUI_NewLabelCreate( optwin, LUI_LEFT, LUI_NEXT_Y, 175, 26, "alpha 0=off 1=on:");
      alpha_field = LUI_FieldCreate( optwin, LUI_NEXT_X, LUI_SAME_Y, 70, 26 );


      LUI_NewLabelCreate( optwin, LUI_LEFT, LUI_NEXT_Y, 175, 26, "barbs 0=off 1=on:");
      barbs_field = LUI_FieldCreate( optwin, LUI_NEXT_X, LUI_SAME_Y, 70, 26 );
      
     
      LUI_NewLabelCreate( optwin, LUI_LEFT, LUI_NEXT_Y, 175, 26, "box X:");
      boxx_field = LUI_FieldCreate( optwin, LUI_NEXT_X, LUI_SAME_Y, 70, 26 );


      LUI_NewLabelCreate( optwin, LUI_LEFT, LUI_NEXT_Y, 175, 26, "box Y:");
      boxy_field = LUI_FieldCreate( optwin, LUI_NEXT_X, LUI_SAME_Y, 70, 26 );


      LUI_NewLabelCreate( optwin, LUI_LEFT, LUI_NEXT_Y, 175, 26, "box Z:");
      boxz_field = LUI_FieldCreate( optwin, LUI_NEXT_X, LUI_SAME_Y, 70, 26 );


      LUI_NewLabelCreate( optwin, LUI_LEFT, LUI_NEXT_Y, 175, 26, "fontname:");
      fontname_field = LUI_FieldCreate( optwin, LUI_NEXT_X, LUI_SAME_Y, 70, 26 );


      LUI_NewLabelCreate( optwin, LUI_LEFT, LUI_NEXT_Y, 175, 26, "fontsize:");
      fontsize = LUI_FieldCreate( optwin, LUI_NEXT_X, LUI_SAME_Y, 70, 26 );


      LUI_NewLabelCreate( optwin, LUI_LEFT, LUI_NEXT_Y, 175, 26, "funcpath:");
      funcpath_field = LUI_FieldCreate( optwin, LUI_NEXT_X, LUI_SAME_Y, 70, 26 );

     
      LUI_NewLabelCreate( optwin, LUI_LEFT, LUI_NEXT_Y, 175, 26, "hirestopo 0=off 1=on:");
      hirestopo_field = LUI_FieldCreate( optwin, LUI_NEXT_X, LUI_SAME_Y, 70, 26 );

      
      LUI_NewLabelCreate( optwin, LUI_LEFT, LUI_NEXT_Y, 175, 26, "julian 0=off 1=on:");
      julian_field = LUI_FieldCreate( optwin, LUI_NEXT_X, LUI_SAME_Y, 70, 26 );


      LUI_NewLabelCreate( optwin, LUI_LEFT, LUI_NEXT_Y, 175, 26, "animrate:");
      animrate_field = LUI_FieldCreate( optwin, LUI_NEXT_X, LUI_SAME_Y, 70, 26 );

      
      LUI_NewLabelCreate( optwin, LUI_LEFT, LUI_NEXT_Y, 175, 26, "texture file:");
      texture_field = LUI_FieldCreate( optwin, LUI_NEXT_X, LUI_SAME_Y, 70, 26 );


      LUI_NewLabelCreate( optwin, LUI_LEFT, LUI_NEXT_Y, 175, 26, "toponame:");
      toponame_field = LUI_FieldCreate( optwin, LUI_NEXT_X, LUI_SAME_Y, 70, 26 );


      LUI_NewLabelCreate( optwin, LUI_LEFT, LUI_NEXT_Y, 175, 26, "mapanme:");
      mapname_field = LUI_FieldCreate( optwin, LUI_NEXT_X, LUI_SAME_Y, 70, 26 );



      LUI_NewLabelCreate( optwin, LUI_LEFT, LUI_NEXT_Y, 175, 26, "log flag 0=off 1=on:");
      log_flag_field = LUI_FieldCreate( optwin, LUI_NEXT_X, LUI_SAME_Y, 70, 26 );


      LUI_NewLabelCreate( optwin, LUI_LEFT, LUI_NEXT_Y, 175, 26, "log scale:");
      log_scale_field = LUI_FieldCreate( optwin, LUI_NEXT_X, LUI_SAME_Y, 70, 26 );


      LUI_NewLabelCreate( optwin, LUI_LEFT, LUI_NEXT_Y, 175, 26, "log exponent:");
      log_exponent_field = LUI_FieldCreate( optwin, LUI_NEXT_X, LUI_SAME_Y, 70, 26 );


      LUI_NewLabelCreate( optwin, LUI_LEFT, LUI_NEXT_Y, 175, 26, "linewidth:");
      linewidth_field = LUI_FieldCreate( optwin, LUI_NEXT_X, LUI_SAME_Y, 70, 26 );


      LUI_NewLabelCreate( optwin, LUI_LEFT, LUI_NEXT_Y, 175, 26, "samescale:");
      samescale_field = LUI_FieldCreate( optwin, LUI_NEXT_X, LUI_SAME_Y, 70, 26 );


      LUI_NewLabelCreate( optwin, LUI_LEFT, LUI_NEXT_Y, 175, 26, "legend position:");
      lposition_field = LUI_FieldCreate( optwin, LUI_NEXT_X, LUI_SAME_Y, 70, 26 );
      LUI_NewLabelCreate( optwin, LUI_LEFT, LUI_NEXT_Y, 175, 26, "top = 28 bottom = 29");
      LUI_NewLabelCreate( optwin, LUI_LEFT, LUI_NEXT_Y, 175, 26, "left = 38 right = 37");

      LUI_NewLabelCreate( optwin, LUI_LEFT, LUI_NEXT_Y, 175, 26, "legend relative x_pos:");
      legendx_field = LUI_FieldCreate( optwin, LUI_NEXT_X, LUI_SAME_Y, 70, 26 );

      LUI_NewLabelCreate( optwin, LUI_LEFT, LUI_NEXT_Y, 175, 26, "legend relative y_pos:");
      legendy_field = LUI_FieldCreate( optwin, LUI_NEXT_X, LUI_SAME_Y, 70, 26 );

      LUI_NewLabelCreate( optwin, LUI_LEFT, LUI_NEXT_Y, 175, 26, "legend size:");
      lsize_field = LUI_FieldCreate( optwin, LUI_NEXT_X, LUI_SAME_Y, 70, 26 );
      

      finish_button = LUI_PushButtonCreate( optwin, LUI_LEFT, LUI_NEXT_Y,
                                                80, 26, "Done" );
      LUI_ButtonCallback ( finish_button, finish_cb);



   }

   XMapWindow( GuiDpy, optwin);
   XSync(GuiDpy, 0);
   load_opt(b->index); 
   LUI_EventLock( optwin );
   while (kg){
      XNextEvent(GuiDpy, &event);
      LUI_EventDispatch(&event);
   }
   LUI_EventLock( 0 );
   XUnmapWindow( GuiDpy, optwin);
   return 0;
}


 
      
   


static int fub_cb( LUI_NEWBUTTON *b, int state)
{

   static char *labels[] = { "Generic",
                             "Cylindrical Equidistant",
                             "Lambert Conformal",
                             "Polar Stereographic",
                             "Rotated",
                             "Cylindrical Projection",
                             "Spherical Projection",
                             "Mercator" };

   static char *labels2[] = { "Linear, Equally spaced, Generic units",
                             "Linear, Equally spaced, Kilometers",
                             "Linear, Unequally spaced, Kilometers",
                             "Pressure, Unequally spaced, Millibars" };

   int i;
   XEvent event;
   int x, y, height;
   int mat;
   
   if ( DisplayRows <= 4 && DisplayCols <= 4){
      mat = (DisplayRows * 10) + DisplayCols;
   }
   else{
      return -1;
   }
   switch(mat){
      case 11:
         XResizeWindow(GuiDpy, Dwindow, 568, 987);
         break;
      case 12:
         XResizeWindow(GuiDpy, Dwindow, 788, 987);
         break;
      case 22:
         XResizeWindow(GuiDpy, Dwindow, 788, 987);
         break;
      case 23:
         XResizeWindow(GuiDpy, Dwindow, 1009, 987);
         break;
      case 32:
         XResizeWindow(GuiDpy, Dwindow, 788, 987);
         break;
      case 33:
         XResizeWindow(GuiDpy, Dwindow, 1009, 987);
         break;
      case 34:
         XResizeWindow(GuiDpy, Dwindow, 1264, 987);
         break;
      case 43:
         XResizeWindow(GuiDpy, Dwindow, 1009, 987);
         break;
      case 44:
         XResizeWindow(GuiDpy, Dwindow, 1264, 987);
         break;
   }


   current_user_input_display = b->index;
   if (V5Dyo != NULL){
      v5dFreeStruct( V5Dyo );
   }
   V5Dyo = v5dNewStruct();
   vis5d_get_dtx_values(b->index, V5Dyo);
   kg = 1;


   if (prjwin == NULL ){

      prjwin = LUI_CreateWindowAt(Dwindow, 5, 200, 300, 800);
         XMapWindow( GuiDpy, prjwin);
 
      LUI_NewLabelCreate( prjwin, LUI_LEFT, LUI_TOP, 100, 26, "Rows:" );
      rfield = LUI_FieldCreate( prjwin, LUI_NEXT_X, LUI_SAME_Y, 70, 26 );
      LUI_FieldCallback( rfield, rfield_cb );
  
      LUI_NewLabelCreate( prjwin, LUI_LEFT, LUI_NEXT_Y, 100, 26, "Columns:");
      cfield = LUI_FieldCreate( prjwin, LUI_NEXT_X, LUI_SAME_Y, 70, 26 );
      LUI_FieldCallback( cfield, cfield_cb );
      LUI_FieldLink( rfield, cfield );

      LUI_NewLabelCreate( prjwin, LUI_LEFT, LUI_NEXT_Y, 100, 26, "Max Levels:");
      lfield = LUI_FieldCreate( prjwin, LUI_NEXT_X, LUI_SAME_Y, 70, 26 );
      LUI_FieldCallback( lfield, lfield_cb );
      LUI_FieldLink( cfield, lfield );
 
      LUI_NewLabelCreate( prjwin, LUI_LEFT, LUI_NEXT_Y, 290,30,
                          "Output File's Map Projection" );
      prj_radio = LUI_RadioCreate( prjwin, LUI_LEFT, LUI_NEXT_Y, 290,
                                    8, labels );
      LUI_RadioCallback( prj_radio, prj_radio_cb );

      for (i=0;i<MAX_PRJ_FIELDS;i++) {
         prjlabel[i] = LUI_NewLabelCreate( prjwin, LUI_LEFT, LUI_NEXT_Y,
                                            200, 26, "Proj Arg:" );
         prjfield[i] = LUI_FieldCreate( prjwin, LUI_NEXT_X, LUI_SAME_Y,
                                         85, 26 );
         LUI_FieldCallback( prjfield[i], prj_field_cb ); 
         if (i>0) {
            LUI_FieldLink( prjfield[i-1], prjfield[i] );
         }
      }
      LUI_NewLabelCreate( prjwin, LUI_LEFT, LUI_NEXT_Y, 290,30,
                          "Output File's Vertical" );
      LUI_NewLabelCreate( prjwin, LUI_LEFT, LUI_NEXT_Y, 290,30,
                          "Coordinate System" );
      vrt_radio = LUI_RadioCreate( prjwin, LUI_LEFT, LUI_NEXT_Y, 290,
                                    4, labels2 );
      LUI_RadioCallback( vrt_radio, vrt_radio_cb );

      for (i=0;i<MAX_VRT_FIELDS;i++) {
         vrtlabel[i] = LUI_NewLabelCreate( prjwin, LUI_LEFT, LUI_NEXT_Y,
                                            200, 26, "Vert Arg:" );
         vrtfield[i] = LUI_FieldCreate( prjwin, LUI_NEXT_X, LUI_SAME_Y,
                                      62, 26 );
         LUI_FieldCallback( vrtfield[i], vrt_field_cb );
         if (i>0) {
            LUI_FieldLink( vrtfield[i-1], vrtfield[i] );
         }
         if (i==0) {
            x = LUI_LayoutGet( LUI_NEXT_X );
            y = LUI_LayoutGet( LUI_SAME_Y );
         }
      }

      close_vert_button = LUI_PushButtonCreate( prjwin, LUI_LEFT, LUI_NEXT_Y,
                                                80, 26, "Done" );
      LUI_ButtonCallback ( close_vert_button, done_cb );

      /* Make the slider for selecting which vertical argument */
      height = 26*MAX_VRT_FIELDS + LUI_LayoutGetGutter()*(MAX_VRT_FIELDS-1);
      vrt_scrollbar = LUI_ScrollBarCreate( prjwin, x, y,
                                            20, height, 1 );
      LUI_ScrollBarCallback( vrt_scrollbar, vrt_scrollbar_cb );
   }

   Load_gridsize_widgets(V5Dyo);
   load_vrt_widgets(V5Dyo);
   load_prj_values(V5Dyo);
   vrt_radio_cb(vrt_radio, V5Dyo->VerticalSystem );
   prj_radio_cb(prj_radio, V5Dyo->Projection );
   XMapWindow( GuiDpy, prjwin);
 
   XSync(GuiDpy, 0);
   LUI_EventLock( prjwin ); 
   while (kg){
      XNextEvent(GuiDpy, &event);
      LUI_EventDispatch(&event);
   } 
   LUI_EventLock( 0 );
   XUnmapWindow( GuiDpy, prjwin);
       
   return 0;

}      



static void show_browser( int bnumber, int display )
{
   char dpyname[20];

   sprintf( dpyname, "Display %d", display);
   make_ctx_browser(bnumber, Dwindow, dpycoords[bnumber][0], dpycoords[bnumber][1],
                    display);
   ctx_browser[bnumber]->show = 1;
   LUI_ContextBrowserActivate( ctx_browser[bnumber]);
   LUI_NewLabelChangeText(ctx_label[bnumber], dpyname);
}

static void remove_all_browsers( void )
{
   int bnumber;

   for (bnumber = 0; bnumber < 16; bnumber++){
      if( ctx_browser[bnumber]->show == 1 ){
         ctx_browser[bnumber]->show = 0;
         LUI_ContextBrowserDeactivate( ctx_browser[bnumber]);
         LUI_NewLabelChangeText(ctx_label[bnumber],"         ");
         LUI_NewButtonDestroy(fub[bnumber]);
         LUI_NewButtonDestroy(opt_but[bnumber]);
         LUI_NewLabelDestroy(fvl[bnumber]);
         LUI_NewLabelDestroy(ful[bnumber]);
         LUI_NewLabelDestroy(fdl[bnumber]);
         LUI_FieldDestroy(fvf[bnumber]);
         LUI_FieldDestroy(fdf[bnumber]);
         LUI_NewLabelDestroy(sdp[bnumber]);
         fub[bnumber] = NULL;
         opt_but[bnumber] = NULL;
         fvl[bnumber] = NULL;
         ful[bnumber] = NULL;
         fdl[bnumber] = NULL;
         fvf[bnumber] = NULL;
         fdf[bnumber] = NULL;
         sdp[bnumber] = NULL;
      }
   }
}

 


static int m11_cb( LUI_NEWBUTTON *b, int state)
{

   highlight_matrix_button( 11 );
   remove_all_browsers();
   show_browser( 0,0 ); 
   set_display_matrix(1,1);
   XResizeWindow(GuiDpy, Dwindow, 568,237);
}

static int m12_cb( LUI_NEWBUTTON *b, int state)
{

   highlight_matrix_button( 12 );
   remove_all_browsers();
   show_browser(0,0);
   show_browser(1,1);
   set_display_matrix(1,2);
   XResizeWindow(GuiDpy, Dwindow, 788,237);
}

static int m13_cb( LUI_NEWBUTTON *b, int state)
{

   highlight_matrix_button( 13 );
   remove_all_browsers();
   show_browser(0,0);
   show_browser(1,1);
   show_browser(2,2);
   set_display_matrix(1,3);
   XResizeWindow(GuiDpy, Dwindow, 1009, 237);
}

static int m22_cb( LUI_NEWBUTTON *b, int state)
{

   highlight_matrix_button( 22 );
   remove_all_browsers();
   show_browser(0,0);
   show_browser(1,1);
   show_browser(4,2);
   show_browser(5,3);
   set_display_matrix(2,2);
   XResizeWindow(GuiDpy, Dwindow, 788,489);
}

static int m23_cb( LUI_NEWBUTTON *b, int state)
{

   highlight_matrix_button( 23 );
   remove_all_browsers();
   show_browser(0,0);
   show_browser(1,1);
   show_browser(2,2);
   show_browser(4,3);
   show_browser(5,4);
   show_browser(6,5);
   set_display_matrix(2,3);
   XResizeWindow(GuiDpy, Dwindow, 1009,489);
}

static int m32_cb( LUI_NEWBUTTON *b, int state)
{

   highlight_matrix_button( 32 );
   remove_all_browsers();
   show_browser(0,0);
   show_browser(1,1);
   show_browser(4,2);
   show_browser(5,3);
   show_browser(8,4);
   show_browser(9,5);
   set_display_matrix(3,2);   
   XResizeWindow(GuiDpy, Dwindow, 788,739);
}

static int m33_cb( LUI_NEWBUTTON *b, int state)
{

   highlight_matrix_button( 33 );
   remove_all_browsers();
   show_browser(0,0);
   show_browser(1,1);
   show_browser(2,2);
   show_browser(4,3);
   show_browser(5,4);
   show_browser(6,5);
   show_browser(8,6);
   show_browser(9,7);
   show_browser(10,8);
   set_display_matrix(3,3);
   XResizeWindow(GuiDpy, Dwindow, 1009, 739);
}

static int m34_cb( LUI_NEWBUTTON *b, int state)
{

   highlight_matrix_button( 34 );
   remove_all_browsers();
   show_browser(0,0);
   show_browser(1,1);
   show_browser(2,2);
   show_browser(3,3);
   show_browser(4,4);
   show_browser(5,5);
   show_browser(6,6);
   show_browser(7,7);
   show_browser(8,8);
   show_browser(9,9);
   show_browser(10,10);
   show_browser(11,11);
   set_display_matrix(3,4);
   XResizeWindow(GuiDpy, Dwindow, 1264,739);
}

static int m43_cb( LUI_NEWBUTTON *b, int state)
{

   highlight_matrix_button( 43 );
   remove_all_browsers();
   show_browser(0,0);
   show_browser(1,1);
   show_browser(2,2);
   show_browser(4,3);
   show_browser(5,4);
   show_browser(6,5);
   show_browser(8,6);
   show_browser(9,7);
   show_browser(10,8);
   show_browser(12,9);
   show_browser(13,10);
   show_browser(14,11);
   set_display_matrix(4,3);
   XResizeWindow(GuiDpy, Dwindow, 1009, 984);
}

static int m44_cb( LUI_NEWBUTTON *b, int state)
{

   highlight_matrix_button( 44 );
   remove_all_browsers();
   show_browser(0,0);
   show_browser(1,1);
   show_browser(2,2);
   show_browser(3,3);
   show_browser(4,4);
   show_browser(5,5);
   show_browser(6,6);
   show_browser(7,7);
   show_browser(8,8);
   show_browser(9,9);
   show_browser(10,10);
   show_browser(11,11);
   show_browser(12,12);
   show_browser(13,13);
   show_browser(14,14);
   show_browser(15,15);
   set_display_matrix(4,4);
   XResizeWindow(GuiDpy, Dwindow, 1264, 984);
}

static void show_current_matrix( void )
{
   int mat;
   LUI_NEWBUTTON *b;

   if ( DisplayRows <= 4 && DisplayCols <= 4){
      mat = (DisplayRows * 10) + DisplayCols;
   }
   else{
      mat = 44; 
   }
   switch (mat) {
      case 11 :
         m11_cb(b,1);
         break;
      case 12 :
         m12_cb(b,1);
         break;
      case 22 :
         m22_cb(b,1);
         break;
      case 23 :
         m23_cb(b,1);
         break;
      case 32 :
         m32_cb(b,1);
         break;
      case 33 :
         m33_cb(b,1);
         break;
      case 34 :
         m34_cb(b,1);
         break;
      case 43 :
         m43_cb(b,1);
         break;
      case 44 :
         m44_cb(b,1);
         break;
      default:
         printf("bad display matric mode in show_current_matrix \n");
   }
}          

static void make_display_widget( GuiContext gtx )
{
   char *labels[3];
   Window w;
   float red[3], green[3], blue[3];
   int mnumber;
   int yo;
  
   current_ctx = gtx->context_index;
   current_selected_ctx = -1;
   current_selected_display_where_ctx_is_in = -1;

   prjwin = NULL;
   optwin = NULL;
 
   if (Dwindow == NULL){
      w = LUI_CreateWindowAt( LUI_RootWindow, 5, 5, 568,237);
      gtx->display_window = w;
      Dwindow = w;
   }
   w = Dwindow;
   LUI_BorderWidth( 2 );

   mlabel = LUI_NewLabelCreate( w, LUI_LEFT, LUI_TOP,200,20,"   Display Matrix");
   b11 = LUI_ToggleButtonCreate( w, LUI_LEFT, LUI_NEXT_Y, 52, 22, "1 x 1");
   LUI_ButtonCallback( b11, m11_cb);
   b12 = LUI_ToggleButtonCreate( w, LUI_NEXT_X, LUI_SAME_Y, 52, 22, "1 x 2");
   LUI_ButtonCallback( b12, m12_cb);
   b13 = LUI_ToggleButtonCreate( w, LUI_NEXT_X, LUI_SAME_Y, 52, 22, "1 x 3");
   LUI_ButtonCallback( b13, m13_cb);
   b22 = LUI_ToggleButtonCreate( w, LUI_NEXT_X, LUI_SAME_Y, 52, 22, "2 x 2");
   LUI_ButtonCallback( b22, m22_cb);
   b23 = LUI_ToggleButtonCreate( w, LUI_NEXT_X, LUI_SAME_Y, 52, 22, "2 x 3");
   LUI_ButtonCallback( b23, m23_cb);
   b32 = LUI_ToggleButtonCreate( w, LUI_LEFT, LUI_NEXT_Y, 52, 22, "3 x 2");
   LUI_ButtonCallback( b32, m32_cb);
   b33 = LUI_ToggleButtonCreate( w, LUI_NEXT_X, LUI_SAME_Y, 52, 22, "3 x 3");
   LUI_ButtonCallback( b33, m33_cb);
   b34 = LUI_ToggleButtonCreate( w, LUI_NEXT_X, LUI_SAME_Y, 52, 22, "3 x 4");
   LUI_ButtonCallback( b34, m34_cb);
   b43 = LUI_ToggleButtonCreate( w, LUI_NEXT_X, LUI_SAME_Y, 52, 22, "4 x 3");
   LUI_ButtonCallback( b43, m43_cb);
   b44 = LUI_ToggleButtonCreate( w, LUI_NEXT_X, LUI_SAME_Y, 52, 22, "4 x 4");
   LUI_ButtonCallback( b44, m44_cb);




   dpycoords[0][0] = 350;
   dpycoords[0][1] = 25;
   dpycoords[1][0] = 570;
   dpycoords[1][1] = 25;
   dpycoords[2][0] = 790;
   dpycoords[2][1] = 25; 
   dpycoords[3][0] = 1010;
   dpycoords[3][1] = 25;
   dpycoords[4][0] = 350;
   dpycoords[4][1] = 275;
   dpycoords[5][0] = 570;
   dpycoords[5][1] = 275;
   dpycoords[6][0] = 790;
   dpycoords[6][1] = 275;
   dpycoords[7][0] = 1010;
   dpycoords[7][1] = 275;
   dpycoords[8][0] = 350;
   dpycoords[8][1] = 525;
   dpycoords[9][0] = 570;
   dpycoords[9][1] = 525;
   dpycoords[10][0] = 790;
   dpycoords[10][1] = 525;
   dpycoords[11][0] = 1010;
   dpycoords[11][1] = 525;
   dpycoords[12][0] = 350; 
   dpycoords[12][1] = 775;
   dpycoords[13][0] = 570;
   dpycoords[13][1] = 775;
   dpycoords[14][0] = 790;
   dpycoords[14][1] = 775;
   dpycoords[15][0] = 1010;
   dpycoords[15][1] = 775;
         
   if (ctx_browser[0] == NULL){
      for (yo = 0; yo < 16; yo++){
         ctx_browser[yo] = NULL;
      }
   }
                                          


   ok = LUI_PushButtonCreate( w,2, 103,135,30, "Return to Vis5D" );
   LUI_ButtonCallback( ok, ok_cb );

   /* WLH 13 Oct 98 */
   destroy = LUI_PushButtonCreate( w,2, 138,135,30, "Remove" );
   LUI_ButtonCallback( destroy, destroy_cb );

   XMapWindow( GuiDpy, w );
   make_all_browsers();
   if ( DisplayRows <= 4 && DisplayCols <= 4){
      mnumber = (DisplayRows * 10) + DisplayCols;
   }
   else{
      mnumber = 44;
   }

   highlight_matrix_button( mnumber );
   show_current_matrix();
   
}

static void highlight_matrix_button( int mbutton)
{
   LUI_ButtonSetState(b11, 0);
   LUI_ButtonSetState(b12, 0);
   LUI_ButtonSetState(b13, 0);
   LUI_ButtonSetState(b22, 0);
   LUI_ButtonSetState(b23, 0);
   LUI_ButtonSetState(b32, 0);
   LUI_ButtonSetState(b33, 0);
   LUI_ButtonSetState(b34, 0);
   LUI_ButtonSetState(b43, 0);
   LUI_ButtonSetState(b44, 0);
   
   switch (mbutton) {
      case 11:
         LUI_ButtonSetState(b11, 1);
         break;
      case 12:
         LUI_ButtonSetState(b12, 1);
         break;
      case 13:
         LUI_ButtonSetState(b13, 1);
         break;
      case 22:
         LUI_ButtonSetState(b22, 1);
         break;
      case 23:
         LUI_ButtonSetState(b23, 1);
         break;
      case 32:
         LUI_ButtonSetState(b32, 1);
         break;
      case 33:
         LUI_ButtonSetState(b33, 1);
         break;
      case 34:
         LUI_ButtonSetState(b34, 1);
         break;
      case 43:
         LUI_ButtonSetState(b43, 1);
         break;
      case 44:
         LUI_ButtonSetState(b44, 1);
         break;
      default:
         printf("bad button in highlight_matrix_button \n");
         
   }
}





void show_display_widget( GuiContext gtx )
{
   int index = gtx->context_index;
   make_display_widget( gtx );

}



