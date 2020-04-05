/* gui.c */
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
#include "../lui5/lui.h"
#include "igui.h"
#include "gui.h"

static Display *dpy;

static Window mainwin;
static Window timewin;
static Window filewin;
static Window varwin;

static LUI_NEWLIST *timelister;
static LUI_NEWLIST *filelister;
static LUI_NEWLIST *varlister;


extern char *ipath;

static LUI_FIELD *MBS_field;
static LUI_NEWLABEL *MBS_label;
static LUI_FIELD *savename_field;
static LUI_NEWLABEL *savename_label;
static LUI_FIELD *YYYY_field;
static LUI_NEWLABEL *YYYY_label;
static LUI_NEWBUTTON *filter_button;
static LUI_FIELD *DDD_field;
static LUI_NEWLABEL *DDD_label;
static LUI_FIELD *HH_field;
static LUI_NEWLABEL *HH_label;
static LUI_FIELD *MM_field;
static LUI_NEWLABEL *MM_label;
static LUI_FIELD *SS_field;
static LUI_NEWLABEL *SS_label;
static LUI_NEWBROWSER *file_browser;
static LUI_NEWBUTTON *read_button;
static LUI_NEWBUTTON *remove_button;
static LUI_NEWBUTTON *exit_button;
static LUI_NEWBUTTON *load_button;

static FileDB FDB; 
static remove_list[MAXFILES];
static char **time_chars;
static char **file_chars;
static char **var_chars;

static int keepongoing;


/*
 * This exits from the irregular import gui 
 * and returns control back to the main     
 * vis5d gui control panel                  
 */
static int exit_cb( void )
{
   keepongoing = 0;
   XUnmapWindow(dpy, mainwin);
   return 0;
}

/*
 * This loads the files, the selected times, and
 * variables into vis5d using the 
 * vis5d_load_irregular_v5dfile command.  It then
 * recreates a new gui to reflect the data set
 */
static int load_cb( void )
{
   int Kurrant, rows, cols, howmany, yo, index;
   char filename[1000];
   char itxname[100];
   int  mbs;


   sprintf( filename, "irregularv5dimportfile%d", FDB->index);
   LUI_FieldGetText(savename_field, itxname);
   if (!itxname[0]){
      alrighty( 0, "You must enter a Datset Name before loading...");
      return 1;
   }
   mbs = LUI_FieldGetInt( MBS_field);
 
   index = vis5d_load_irregular_v5dfile(0,  mbs, filename, itxname);
   keepongoing = 0;
   XUnmapWindow(dpy, mainwin);
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


   return 1;
}


/*********************************************/
/* This is the call back function that takes */
/* the filter fields and selects the only the*/
/* times which match, all other times will   */
/* be un-selected                            */
/*********************************************/
static int filter_cb( void )
{
   int i, year, day, hh, mm, ss;
   char t1[10];
   char t2[10];
   char t3[10];
   char t4[10];
   char t5[10];

   /****************************************************/
   /* make sure the blank fields don't affect anything */
   /****************************************************/
   LUI_FieldGetText(YYYY_field, t1);
   LUI_FieldGetText(DDD_field, t2);
   LUI_FieldGetText(HH_field, t3);
   LUI_FieldGetText(MM_field, t4);
   LUI_FieldGetText(SS_field, t5);

   year = day = hh = mm = ss = -1;

   /******************/
   /* get the fields */
   /******************/
   if (t1[0]){
      year = LUI_FieldGetInt(YYYY_field);
   }
   if (t2[0]){
      day = LUI_FieldGetInt(DDD_field);
   }
   if (t3[0]){
      hh = LUI_FieldGetInt(HH_field);
   }
   if (t4[0]){
      mm = LUI_FieldGetInt(MM_field);
   }
   if (t5[0]){
      ss = LUI_FieldGetInt(SS_field);
   }


   /************************************************/   
   /* go through the list and select the good ones */
   /************************************************/
   for (i = 0; i < FDB->NumTimes; i++){
      int y, d, h, m, s, good;
      good = 1;
      y = (int)(FDB->DateStamp[i]/1000);
      d = FDB->DateStamp[i] - (y*1000);
      h = (int)(FDB->TimeStamp[i] / 10000);
      m = (int)((FDB->TimeStamp[i]-(h*10000))/100);
      s = FDB->TimeStamp[i]-((int)(FDB->TimeStamp[i]/100)*100); 
      
      if (year != -1 && y != year){
         good = 0;
      }
      if (good && day != -1 && d != day){
         good = 0;
      }
      if (good && hh != -1 && h != hh){
         good = 0;      
      }
      if (good && mm != -1 && m != mm){
         good = 0;      
      }
      if (good && ss != -1 && s != ss){
         good = 0;      
      }

      FDB->TimeSelected[i] = good;
      LUI_NEWListSetStatus(timelister, i, good);
   }
   return 1;
}

 


/*****************************************/
/* This loads the list of times from the */
/* file data base FDB and highlights any */
/* times where FDB->TimeSelected = 1     */
/* This should be called when ever a new */
/* file is loaded into the FDB           */
/*****************************************/
void load_times( void  )
{
   int i; 
   
   if (!time_chars){
      time_chars = (char **) malloc(1000*sizeof(char *) );
      for (i = 0; i < 1000; i ++){
         time_chars[i] = (char *) malloc(100* sizeof(char) );
      }
   }
   
   for (i = 0; i < FDB->NumTimes; i++){
      sprintf( time_chars[i], "%7d %06d", 
         1900000+FDB->DateStamp[i], FDB->TimeStamp[i]);
   }
   LUI_NEWListLoad( timelister, FDB->NumTimes, time_chars, 0);
   for (i = 0; i < FDB->NumTimes; i++){
      LUI_NEWListSetStatus(timelister, i, FDB->TimeSelected[i]);
   }
}

void load_files( void )
{
   int i;

   if (!file_chars){
      file_chars = (char **) malloc(1000*sizeof(char *) );
      for (i = 0; i < 1000; i ++){
         file_chars[i] = (char *) malloc(100* sizeof(char) );
      }
   }

   for (i = 0; i < FDB->NumFiles; i++){
      strcpy(file_chars[i], FDB->File[i]->FileName);
   }
   LUI_NEWListLoad( filelister, FDB->NumFiles, file_chars, 0);
   LUI_NEWListSetStatusAll( filelister, 0);

   for (i = 0; i < MAXFILES; i++){
      remove_list[i] = 0;
   }
   
}
   
void load_vars( void  )
{
   int i;
   if (!var_chars){
      var_chars = (char **) malloc(1000*sizeof(char *) );
      for (i = 0; i < 1000; i ++){
         var_chars[i] = (char *) malloc(100* sizeof(char) );
      }
   }

   for (i = 0; i < FDB->NumVars; i++){
      strcpy(var_chars[i], FDB->VarName[i]);
   }
   LUI_NEWListLoad( varlister, FDB->NumVars, var_chars, 0);
   for (i = 0; i < FDB->NumVars; i++){
      LUI_NEWListSetStatus(varlister, i, FDB->VarSelected[i]);
   }
}


/**************/
/* Read files */
/**************/
static int read_ok_cb( LUI_NEWBROWSER *b, char filepath[500][500] )
{
   int i;
   char name[500];
   
   /**************************************/   
   /* Add the file to the file data base */
   /**************************************/
   for (i = 0; i < b->num_selected; i++){
      strcpy( name, filepath[i]);
      if (!add_a_file( FDB, name)){
         printf("Sorry, could not add the file %s\n", filepath[i]);
      }
   }

   /****************************************/
   /* Reload the time widget and update it */
   /****************************************/
   load_times( );

   /**************************************/
   /* Reload the file list and update it */
   /**************************************/
   load_files( );

   /**************************************/
   /*  Reload the var list and update it */
   /**************************************/
   load_vars( );

   return 0;
}

static int file_select_cb( LUI_NEWLIST *list, int entry, int state )
{
   remove_list[entry] = state;
}


static int time_select_cb( LUI_NEWLIST *list, int entry, int state )
{
   FDB->TimeSelected[entry] = state;
   return 0;
}

static int var_select_cb( LUI_NEWLIST *list, int entry, int state )
{
   FDB->VarSelected[entry] = state;
   return 0;
}

static void make_file_browser( void )
{
   file_browser = LUI_NEWBrowserCreate( 400, 400 );
   LUI_NEWBrowserCallback( file_browser, read_ok_cb );
}

/* Called when user clicks on "Read file..." */
static int read_cb( void )
{
   LUI_NEWBrowserActivate( file_browser, ipath );
   return 0;
}

static int remove_cb( void )
{
   int i;
   for (i = FDB->NumFiles-1; i >= 0; i--){
      if (remove_list[i]==1){
         remove_a_file( FDB, i);
      }
   }

   /****************************************/
   /* Reload the time widget and update it */
   /****************************************/
   load_times();
   load_files();
   load_vars(); 
}



static void make_filewin( void )
{
   int i, xo, yo; /* x and y offset to move it around */

   xo = 5;
   yo = -60;

   filewin = LUI_CreateWindowAt( mainwin, 12+xo, 105+yo, 310, 254);
   XSetWindowBackground(LUI_Display, filewin, LUI_Color_darkgray);

   LUI_NewLabelCreate( filewin, 4, 3, 302, 19,
                       "              File List   ");
   filelister = LUI_NEWListCreate( filewin, 4, 25, 301, 200, 0 );
   LUI_NEWListCallback( filelister, file_select_cb );
   XMapWindow( dpy, filewin);
   read_button = LUI_PushButtonCreate( mainwin, 17+xo, 332+yo, 110, 24,
                                       "Load File..." );
   remove_button = LUI_PushButtonCreate( mainwin, 130+xo, 332+yo, 189, 24,
                                       "Remove Selected File/s");
   LUI_ButtonCallback( read_button, read_cb );
   LUI_ButtonCallback( remove_button, remove_cb);

   for (i = 0; i < MAXFILES; i++){
      remove_list[i] = 0;
   }
}

static void make_varwin( void )
{
   int xo, yo; /* x and y offset to move it around */

   xo = -125;
   yo = 7;
  
   varwin = LUI_CreateWindowAt( mainwin, 300+xo, 300+yo, 180, 330);
   LUI_NewLabelCreate( varwin, LUI_LEFT, LUI_TOP, 190, 15,
                       "Select Variables");

   varlister = LUI_NEWListCreate( varwin, LUI_LEFT, LUI_NEXT_Y, 165, 280, 0 );
   LUI_NEWListCallback( varlister, var_select_cb );
   XMapWindow( dpy, varwin );
}


static void make_timewin( void )
{
   int xo, yo; /* x and y offset to move it around */

   xo = -2;
   yo = 205;
   timewin = LUI_CreateWindowAt( mainwin, 12+xo, 105+yo, 150, 300);
   LUI_NewLabelCreate( timewin, LUI_LEFT, LUI_TOP, 190, 20,
                       " Select Times  " );

   LUI_NewLabelCreate( timewin, LUI_LEFT, LUI_NEXT_Y, 190, 9,
                       "YYYYDDD HHMMSS" );
   timelister = LUI_NEWListCreate( timewin, LUI_LEFT, LUI_NEXT_Y, 140, 200, 0 );
   LUI_NEWListCallback( timelister, time_select_cb );
   XMapWindow( dpy, timewin );

   filter_button = LUI_PushButtonCreate( mainwin, 17+xo, 350+yo, 142, 17,
                   "     FILTER");
   LUI_ButtonCallback( filter_button, filter_cb);
 
   YYYY_field = LUI_FieldCreate(mainwin, 10+xo, 368+yo, 40, 20);
   YYYY_label = LUI_NewLabelCreate( mainwin, 12+xo, 388+yo, 40, 15, "YYYY");
   YYYY_field->len_limit = 4;   

   DDD_field = LUI_FieldCreate(mainwin, 52+xo, 368+yo, 33, 20);
   DDD_label = LUI_NewLabelCreate( mainwin, 55+xo, 388+yo, 30, 15, "DDD");
   DDD_field->len_limit = 3;

   HH_field = LUI_FieldCreate(mainwin, 87+xo, 368+yo, 25, 20);
   HH_label = LUI_NewLabelCreate( mainwin, 89+xo, 388+yo, 20, 15, "HH");
   HH_field->len_limit = 2;

   MM_field = LUI_FieldCreate(mainwin, 114+xo, 368+yo, 25, 20);
   MM_label = LUI_NewLabelCreate( mainwin, 116+xo, 388+yo, 20, 15, "MM");
   MM_field->len_limit = 2;

   SS_field = LUI_FieldCreate(mainwin, 141+xo, 368+yo, 25, 20);
   SS_label = LUI_NewLabelCreate( mainwin, 143+xo, 388+yo, 20, 15, "SS");
   SS_field->len_limit = 2;   


   
}
   


/*
 * Make the main window.
 */
static void make_mainwin( void )
{

   mainwin = LUI_CreateWindowAt( LUI_RootWindow, 1,1, 600, 630 );
   XMapWindow( dpy, mainwin );
   LUI_NewLabelCreate( mainwin, LUI_LEFT, LUI_TOP, 590, 30,
           "                     Vis5D Irregular Data Importer  ");

   exit_button = LUI_PushButtonCreate( mainwin, 
                     335, 45, 60, 27, " Exit" );
   LUI_ButtonCallback( exit_button, exit_cb );
   load_button = LUI_PushButtonCreate( mainwin,
                     335, 75, 60, 27, " Load" );
   LUI_ButtonCallback( load_button, load_cb );
   savename_field = LUI_FieldCreate( mainwin, 335, 105, 90, 25);
   savename_label = LUI_NewLabelCreate(mainwin, 332, 130, 100, 20,
                              "Dataset Name");
   savename_field->len_limit = 8;

   MBS_field = LUI_FieldCreate( mainwin, 440, 105, 40, 25);
   MBS_label = LUI_NewLabelCreate(mainwin, 448, 130, 30, 20, "MBS");
   LUI_FieldSetInt(MBS_field, 32); 



   
}


void imake_gui( Display *thedpy )
{

   keepongoing = 1;
   dpy = thedpy;
   LUI_BorderWidth( 2 );
   LUI_Initialize( "v5dIrregImport (5.2)", dpy, NULL, 0, 0);

   make_mainwin();
   make_filewin();
   make_timewin();
   make_varwin();
   make_file_browser();
}

/*
 * Main loop when using the graphical interface.
 */
int igui_loop(FileDB fdb)
{
   XEvent event;

   FDB = fdb;
   while(keepongoing){
      XNextEvent(dpy, &event);
      LUI_EventDispatch(&event);
   } 
/*
   LUI_EventProcess();
*/
   return 0;
}

