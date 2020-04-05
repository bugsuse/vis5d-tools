/* gui.c */


#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "analyze.h"
#include "lui.h"
#include "misc.h"
#include "file.h"
#include "grid.h"
#include "memory.h"
#include "output.h"
#include "proj.h"
#include "projlist.h"
#include "select.h"
#include "v5d.h"
#include "api.h"

#if V5D_VERSION >= 42
#endif

#define VERT(X, Z) (X->VerticalSystem == 3 ? height_to_pressure(Z) : (Z))
#define INVERT(X, Z) (X->VerticalSystem == 3 ? pressure_to_height(Z) : (Z))




extern int Debug;
extern char *path;

/* This struct describes the output file: */
static v5dstruct *V5Dout;

/* Describes the input grids: */
static struct grid_db *DB;




/*
 * Main window and its widgets:
 */
static Display *dpy;
static Window mainwin;
static LUI_LIST *gridlister;
static LUI_NEWLABEL *size_label;
static LUI_FIELD *rows_field, *columns_field;
static LUI_NEWBUTTON *proj_button, *vcs_button;
static LUI_FIELD *levels_field;
static LUI_NEWBUTTON *vert_button;
static LUI_FIELD *outfile_field;



/*
 * Time window and its widgets:
 */
static Window timewin;
static LUI_LIST *timelister;
static LUI_NEWBUTTON *close_time_button;

/*
 * Variable window and its widgets:
 */
static Window varwin;
static LUI_LIST *varlister;
static LUI_NEWBUTTON *close_var_button;

/*
 * Map projection window and widgets:
 */
static Window projwin;
static LUI_RADIO *proj_radio;
#define MAX_PROJ_FIELDS 7
static LUI_NEWLABEL *projlabel[MAX_PROJ_FIELDS];
static LUI_FIELD    *projfield[MAX_PROJ_FIELDS];
static LUI_NEWBUTTON *close_proj_button;

/*
 * Vert. Coord. Sys. window and widgets:
 */
static Window vertwin;
static LUI_RADIO *vert_radio;
#define MAX_VERT_FIELDS 4
static LUI_NEWLABEL *vertlabel[MAX_VERT_FIELDS];
static LUI_FIELD    *vertfield[MAX_VERT_FIELDS];
static LUI_NEWBUTTON *close_vert_button;
static LUI_SCROLLBAR *vert_scrollbar;


/*
 * File browser.
 */
static LUI_BROWSER *file_browser;


/*
 * Projection list window and widgets:
 */
static Window projlistwin;
static LUI_LIST *projlister;
static LUI_NEWBUTTON *close_projlist_button;


/*
 * Vertical coordiante system list window and widgets:
 */
static Window vcslistwin;
static LUI_LIST *vcslister;
static LUI_NEWBUTTON *close_vcslist_button;


/*
 * Options window
 */
static Window optionwin;
static LUI_NEWBUTTON *close_option_button;
static LUI_RADIO *average_radio;
static LUI_RADIO *bytes_radio;



#ifdef JUNK
static int
   proj_radio_cb(),
   vert_radio_cb(),
   estimate_file_size();
   
static void
   load_projection_widgets(),
   load_vertical_widgets(),
   load_time_window(),
   load_var_window(),
   load_gridsize_widgets(),
   load_projlist_window(),
   load_vcslist_window();
#endif





/*
 * "Load" the current states of the map projection widgets from the
 * information in the given v5d struct.
 */
static void load_projection_widgets( v5dstruct *v5d )
{
   int p, i, n;

   switch (v5d->Projection) {
      case PROJ_GENERIC:
         p = 0;
         n = 4;
         LUI_NewLabelChangeText( projlabel[0], "North Bound:" );
         LUI_NewLabelChangeText( projlabel[1], "West Bound:" );
         LUI_NewLabelChangeText( projlabel[2], "Row Increment:" );
         LUI_NewLabelChangeText( projlabel[3], "Column Increment:" );
         LUI_NewLabelChangeText( projlabel[4], "" );
         LUI_NewLabelChangeText( projlabel[5], "" );
         LUI_NewLabelChangeText( projlabel[6], "" );
         break;
      case PROJ_LINEAR:
         p = 1;
         n = 4;
         LUI_NewLabelChangeText( projlabel[0], "North Bound (deg):" );
         LUI_NewLabelChangeText( projlabel[1], "West Bound (deg):" );
         LUI_NewLabelChangeText( projlabel[2], "Row Increment (deg):" );
         LUI_NewLabelChangeText( projlabel[3], "Column Increment (deg):" );
         LUI_NewLabelChangeText( projlabel[4], "" );
         LUI_NewLabelChangeText( projlabel[5], "" );
         LUI_NewLabelChangeText( projlabel[6], "" );
         break;
      case PROJ_LAMBERT:
         p = 2;
         n = 6;
         LUI_NewLabelChangeText( projlabel[0], "Latitude 1:" );
         LUI_NewLabelChangeText( projlabel[1], "Latitude 2:" );
         LUI_NewLabelChangeText( projlabel[2], "Pole Row:" );
         LUI_NewLabelChangeText( projlabel[3], "Pole Column:" );
         LUI_NewLabelChangeText( projlabel[4], "Central Longitude:" );
         LUI_NewLabelChangeText( projlabel[5], "Column Increment (km):" );
         LUI_NewLabelChangeText( projlabel[6], "" );
         break;
      case PROJ_STEREO:
         p = 3;
         n = 5;
         LUI_NewLabelChangeText( projlabel[0], "Central Latitude:" );
         LUI_NewLabelChangeText( projlabel[1], "Central Longitude:" );
         LUI_NewLabelChangeText( projlabel[2], "Central Row:" );
         LUI_NewLabelChangeText( projlabel[3], "Central Column:" );
         LUI_NewLabelChangeText( projlabel[4], "Column Increment (km):" );
         LUI_NewLabelChangeText( projlabel[5], "" );
         LUI_NewLabelChangeText( projlabel[6], "" );
         break;
      case PROJ_ROTATED:
         p = 4;
         n = 7;
         LUI_NewLabelChangeText( projlabel[0], "Lat of grid row 0:" );
         LUI_NewLabelChangeText( projlabel[1], "Lon of grid col 0:" );
         LUI_NewLabelChangeText( projlabel[2], "Row increment (deg):" );
         LUI_NewLabelChangeText( projlabel[3], "Col increment (deg):" );
         LUI_NewLabelChangeText( projlabel[4], "Lat of (0,0):" );
         LUI_NewLabelChangeText( projlabel[5], "Lon of (0,0):" );
         LUI_NewLabelChangeText( projlabel[6], "Rotation (deg):" );
         break;
      default:
         /* Projection not initialized yet */
         p = 0;
         n = 0;
   }
   if (LUI_RadioGetCurrent(proj_radio) != p) {
      LUI_RadioSetCurrent( proj_radio, p );
   }
   for (i=0;i<n;i++) {
      LUI_FieldSetDouble( projfield[i], v5d->ProjArgs[i] );
   }
   for (i=n;i<MAX_PROJ_FIELDS;i++) {
      LUI_FieldSetText( projfield[i], "" );
   }

}



/*
 * "Load" the current states of the VCS widgets from the
 * information in the given v5d struct.
 */
static void load_vertical_widgets( v5dstruct *v5d )
{
   int v, i, n;
   int maxnl;
   double size;

   switch (v5d->VerticalSystem) {
      case 0:  /* linear, equally spaced, generic */
         v = 0;
         n = 2;
         LUI_NewLabelChangeText( vertlabel[0], "Bottom Bound:" );
         LUI_NewLabelChangeText( vertlabel[1], "Level Increment:" );
         LUI_NewLabelChangeText( vertlabel[2], "" );
         LUI_NewLabelChangeText( vertlabel[3], "" );
         LUI_ScrollBarSet( vert_scrollbar, 100.0, 0.0 );
         break;
      case 1:  /* equal km */
         v = 1;
         n = 2;
         LUI_NewLabelChangeText( vertlabel[0], "Bottom Bound (km):" );
         LUI_NewLabelChangeText( vertlabel[1], "Level Increment (km):" );
         LUI_NewLabelChangeText( vertlabel[2], "" );        
         LUI_NewLabelChangeText( vertlabel[3], "" );
         LUI_ScrollBarSet( vert_scrollbar, 100.0, 0.0 );
         break;
      case 2:  /* unequal km */
         v = 2;
         n = 4;
         LUI_NewLabelChangeText( vertlabel[0], "Level 1 Height (km):" );
         LUI_NewLabelChangeText( vertlabel[1], "Level 2 Height (km):" );
         LUI_NewLabelChangeText( vertlabel[2], "Level 3 Height (km):" );
         LUI_NewLabelChangeText( vertlabel[3], "Level 4 Height (km):" );
         maxnl = LUI_FieldGetInt( levels_field );
         if (maxnl<=MAX_VERT_FIELDS) {
            size = 100.0;
         }
         else {
            size = 100 * MAX_VERT_FIELDS / maxnl;
         }
         LUI_ScrollBarSet( vert_scrollbar, size, 0.0 );
         break;
      case 3:  /* unequal mb */
         v = 3;
         n = 4;
         LUI_NewLabelChangeText( vertlabel[0], "Level 1 Pressure (mb):" );
         LUI_NewLabelChangeText( vertlabel[1], "Level 2 Pressure (mb):" );
         LUI_NewLabelChangeText( vertlabel[2], "Level 3 Pressure (mb):" );
         LUI_NewLabelChangeText( vertlabel[3], "Level 4 Pressure (mb):" );
         maxnl = LUI_FieldGetInt( levels_field );
         if (maxnl<=MAX_VERT_FIELDS) {
            size = 100.0;
         }
         else {
            size = 100 * MAX_VERT_FIELDS / maxnl;
         }
         LUI_ScrollBarSet( vert_scrollbar, size, 0.0 );
         break;
      default:
         /* VCS not initialized yet */
         v = 0;
         n = 0;
   }

   if (LUI_RadioGetCurrent(vert_radio) != v) {
      LUI_RadioSetCurrent( vert_radio, v );
   }
   for (i=0;i<n;i++) {
     LUI_FieldSetDouble( vertfield[i], VERT(v5d, v5d->VertArgs[i]) );
   }
   for (i=n;i<MAX_VERT_FIELDS;i++) {
      LUI_FieldSetText( vertfield[i], "" );
   }
}



/*
 * "Load" the contents of the timestep selection window based upon
 * current grid list.
 */
static void load_time_window( void )
{
   int i;
   char **strings;

   /* It's OK if DB->NumTimes==0) */

   /* WLH 26 July 96, NO ITS NOT, since strings not initialized */
   strings = NULL;

   if (DB->NumTimes > 0) {
     strings = (char **) MALLOC( DB->NumTimes * sizeof(char *) );
   }
   for (i=0;i<DB->NumTimes;i++) {
      char str[100];
      sprintf( str, "%3d %05d %06d", i+1, DB->DateStamp[i], DB->TimeStamp[i] );
      strings[i] = str_dup( str );
   }

   LUI_ListLoad( timelister, DB->NumTimes, strings, 1 );

   for (i=0;i<DB->NumTimes;i++) {
      if (DB->TimeSelected[i]) {
         LUI_ListSetStatus( timelister, i, 1 );
      }
      else {
         LUI_ListSetStatus( timelister, i, 0 );
      }
   }
}



/*
 * "Load" the contents of the variable selection window based upon
 * current grid list.
 */
static void load_var_window( void )
{
   int i;
   char **strings;

   /* It's OK if DB->NumVars==0 */

   /* WLH 26 July 96, NO ITS NOT, since strings not initialized */
   strings = NULL;

   /* make array of strings of variable names and physical units */
   if (DB->NumVars > 0) {
     strings = (char **) MALLOC( DB->NumVars * sizeof(char*) );
   }
   for (i=0;i<DB->NumVars;i++) {
      strings[i] = MALLOC(200);
      if (DB->Units[i]) {
         sprintf( strings[i], "%s (%s)", DB->VarNames[i], DB->Units[i] );
      }
      else {
         sprintf( strings[i], "%s", DB->VarNames[i] );
      }
   }

/*   LUI_ListLoad( varlister, DB->NumVars, DB->VarNames, 0 );*/
   LUI_ListLoad( varlister, DB->NumVars, strings, 1 );

   for (i=0;i<DB->NumVars;i++) {
      if (DB->VarSelected[i]) {
         LUI_ListSetStatus( varlister, i, 1 );
      }
      else {
         LUI_ListSetStatus( varlister, i, 0 );
      }
   }
}



/*
 * "Load" the contents of the map projection selection window based upon
 * current grid list.
 */
static void load_projlist_window( void )
{
   char **projections;
   int i;

   projections = sprint_projection_list( DB );
   LUI_ListLoad( projlister, DB->NumProj, projections, 1 );

   for (i=0;i<DB->NumProj;i++) {
      LUI_ListSetStatus( projlister, i, DB->ProjSelected[i] );
   }
}



/*
 * "Load" the contents of the vcs selection window based upon
 * current grid list.
 */
static void load_vcslist_window( void )
{
   char **vcslist;
   int i;

   vcslist = sprint_vcs_list( DB );
   LUI_ListLoad( vcslister, DB->NumVcs, vcslist, 1 );

   for (i=0;i<DB->NumVcs;i++) {
      LUI_ListSetStatus( vcslister, i, DB->VcsSelected[i] );
   }
}



/*
 * "Load" the contents of the rows, columns, and max levels fields.
 */
static void load_gridsize_widgets( v5dstruct *v5d )
{
   int maxnl, i;

   maxnl = 0;
   for (i=0;i<MAXVARS;i++) {
      if (v5d->Nl[i]>maxnl) {
         maxnl = v5d->Nl[i];
      }
   }

   LUI_FieldSetInt( rows_field, v5d->Nr );
   LUI_FieldSetInt( columns_field, v5d->Nc );
   LUI_FieldSetInt( levels_field, maxnl );
}




/*
 * Given the current selection of grids and output grid size, compute the
 * approximate size of the output file.
 */
static int estimate_file_size( void )
{
   int time, var;
   int numtimes = 0, numvars = 0;
   int nr, nc, maxnl;
   int size;
   float mbytes;
   char str[1000];

   for (time=0;time<DB->NumTimes;time++) {
      if (DB->TimeSelected[time]) {
         numtimes++;
      }
   }

   for (var=0;var<DB->NumVars;var++) {
      if (DB->VarSelected[var]) {
         numvars++;
      }
   }

   nr = LUI_FieldGetInt( rows_field );
   nc = LUI_FieldGetInt( columns_field );
   maxnl = LUI_FieldGetInt( levels_field );

   size = numtimes * numvars * nr * nc * maxnl + 50000;

   mbytes = size / 1000000.0;

   sprintf( str, "Approx. size: %.2f MB", mbytes );
   LUI_NewLabelChangeText( size_label, str );

   return size;
}



/**********************************************************************/
/*****                       Callback functions                   *****/
/**********************************************************************/


/* for debugging */
static int debug_cb( void )
{
   static int count=0;
   print_projection_list( DB );
/*   combine_vcs( DB, VERT_EQUAL_KM );*/
   count++;
   if (count>5) {
      /* 5 clicks, make a core file for serious debugging */
      abort();
   }
   return 0;
}


/* Called for "Exit" button */
static int exit_cb( void )
{
   free_grid_db( DB );
   v5dFreeStruct( V5Dout );
   LUI_ListUnload( gridlister );
   LUI_ListUnload( timelister );
   LUI_ListUnload( varlister );
   LUI_ListUnload( projlister );
   LUI_ListUnload( vcslister );
   exit(0);
   return 0;
}



/* Called for "Make" button */
static int make_cb( void )
{
   char filein[1000], filename[1000];
   int maxnl = 0;
   int average, compressmode;

   LUI_FieldGetText( outfile_field, filein );
   LUI_FieldSetText( outfile_field, "");
   if (filein[0]==0) {
      printf("No filename given!\n");
      return 0;
   }
   if (filein[0] == '/' || path == NULL) {
     strcpy(filename, filein);
   }
   else {
     int len;
     strcpy(filename, path);
     /* add a trailing slash to path if there isn't one already */
     len = strlen(filename);
     if (len>0 && filename[len-1]!='/') {
       strcat(filename, "/");
     }
     strcat(filename, filein);
   }

   average = LUI_RadioGetCurrent( bytes_radio );
   switch (LUI_RadioGetCurrent( bytes_radio )) {
      case 0:
         compressmode = 1;
         break;
      case 1:
         compressmode = 2;
         break;
      case 2:
         compressmode = 4;
         break;
   }

   V5Dout->Nr = LUI_FieldGetInt( rows_field );
   V5Dout->Nc = LUI_FieldGetInt( columns_field );
   maxnl = LUI_FieldGetInt( levels_field );
   if (V5Dout->Nr < 2 || V5Dout->Nr>MAXROWS) {
      printf("Error: too few or too many grid rows, range is [2,%d]\n",
             MAXROWS);
      return 0;
   }
   if (V5Dout->Nc < 2 || V5Dout->Nc>MAXCOLUMNS) {
      printf("Error: too few or too many grid columns, range is [2,%d]\n",
             MAXCOLUMNS);
      return 0;
   }
   if (maxnl<1 || maxnl>MAXLEVELS) {
      printf("Error: too few or too many grid levels, range is [1,%d]\n",
             MAXLEVELS);
      return 0;
   }

/*   read_projection_widgets( V5Dout );*/

   printf("Making %s...\n", filename );
   make_output_file( DB, V5Dout, filename, maxnl, average, compressmode );
   printf("Done\n");
   return 0;
}



int start_vis5d( char *filename )
{
   FILE *f;
   char command[1000];
   char line[1000];
   char *sub;

   /* start Vis5D */
   printf("Starting Vis5D...\n");
   strcpy(command, "vis5d ");
   strcat(command, filename);
   f = fopen("vis5d_options", "r" );
   if (f) {
     strcat(command, " ");
     fgets(line, 1000, f);
     sub = strchr(line, '\n');
     if (sub != NULL) sub[0] = 0;
     strcat(command, line);
   }
   strcat(command, " &");
   printf("command:  %s\n", command);
   system(command);
   return 0;
}



/* Called for "Visualize" button */
static int go_cb( void )
{
   char filein[1000], filename[1000];
   int maxnl = 0;
   int average, compressmode;

   LUI_FieldGetText( outfile_field, filein );
   LUI_FieldSetText( outfile_field, "");
   if (filein[0]==0) {
     char *user = getenv("USER");
     if (user == NULL) user = "user";
     strcpy(filein, user);
     strcat(filein, ".v5d");
   }
   if (filein[0] == '/' || path == NULL) {
     strcpy(filename, filein);
   }
   else {
     int len;
     strcpy(filename, path);
     /* add a trailing slash to path if there isn't one already */
     len = strlen(filename);
     if (len>0 && filename[len-1]!='/') {
       strcat(filename, "/");
     }
     strcat(filename, filein);
   }


   average = LUI_RadioGetCurrent( bytes_radio );
   switch (LUI_RadioGetCurrent( bytes_radio )) {
      case 0:
         compressmode = 1;
         break;
      case 1:
         compressmode = 2;
         break;
      case 2:
         compressmode = 4;
         break;
   }

   V5Dout->Nr = LUI_FieldGetInt( rows_field );
   V5Dout->Nc = LUI_FieldGetInt( columns_field );
   maxnl = LUI_FieldGetInt( levels_field );
   if (V5Dout->Nr < 2 || V5Dout->Nr>MAXROWS) {
      printf("Error: too few or too many grid rows, range is [2,%d]\n",
             MAXROWS);
      return 0;
   }
   if (V5Dout->Nc < 2 || V5Dout->Nc>MAXCOLUMNS) {
      printf("Error: too few or too many grid columns, range is [2,%d]\n",
             MAXCOLUMNS);
      return 0;
   }
   if (maxnl<1 || maxnl>MAXLEVELS) {
      printf("Error: too few or too many grid levels, range is [1,%d]\n",
             MAXLEVELS);
      return 0;
   }

/*   read_projection_widgets( V5Dout );*/

   printf("Making %s...\n", filename );
   make_output_file( DB, V5Dout, filename, maxnl, average, compressmode );

   start_vis5d(filename);

   free_grid_db( DB );
   exit(0);
   return 0;
}



/* Called for "Select by time..." button */
static int timestep_cb()
{
   XMapWindow( dpy, timewin );
   XRaiseWindow( dpy, timewin );
   return 0;
}


/* Called when "Select by projection.." button is pressed */
static int projlist_cb( void )
{
   load_projlist_window();
   XMapWindow( dpy, projlistwin );
   XRaiseWindow( dpy, projlistwin );
   return 0;
}


/* Called when "Select by vcs.." button is pressed */
static int vcslist_cb( void )
{
   load_vcslist_window();
   XMapWindow( dpy, vcslistwin );
   XRaiseWindow( dpy, vcslistwin );
   return 0;
}


/* Called for "Select by variable..." button */
static int variable_cb()
{
   XMapWindow( dpy, varwin );
   XRaiseWindow( dpy, varwin );
   return 0;
}



/* Called for "All" button. */
static int all_cb()
{
   /* select all grids */
   select_all( DB, ALL_BITS, 1 );
   LUI_ListSetStatusAll( gridlister, 1 );
   LUI_ListSetStatusAll( varlister, 1 );
   LUI_ListSetStatusAll( timelister, 1 );
   LUI_ListSetStatusAll( projlister, 1 );
   LUI_ListSetStatusAll( vcslister, 1 );
   estimate_file_size();
   return 0;
}


/* Called for "None" button. */
static int none_cb()
{
   /* unselect all grids */
   select_all( DB, ALL_BITS, 0 );
   LUI_ListSetStatusAll( gridlister, 0 );
   LUI_ListSetStatusAll( varlister, 0 );
   LUI_ListSetStatusAll( timelister, 0 );
   LUI_ListSetStatusAll( projlister, 0 );
   LUI_ListSetStatusAll( vcslister, 0 );
   estimate_file_size();
   return 0;
}


/* Called when user changes value of rows field */
static int rows_cb( field, str )
LUI_FIELD *field;
char *str;
{
   int nr;

#ifdef LEAVEOUT
   nr = atoi( str );
   if (nr) {
      /* adjust projection to match new rows */
      switch (V5Dout->Projection) {
         case 0:
         case 1:  /* rectilinear*/
            V5Dout->ProjArgs[2] *= (float) V5Dout->Nr / (float) nr;
            break;
         case 2:  /* lambert */
            V5Dout->ProjArgs[2] *= (float) V5Dout->Nr / (float) nr;
            break;
         case 3:  /* polar stereographic */
            V5Dout->ProjArgs[2] *= (float) V5Dout->Nr / (float) nr;
            break;
         default:
            /* ??? */
            ;
      }
      load_projection_widgets( V5Dout );
      V5Dout->Nr = nr;
   }
#endif

   estimate_file_size();
   return 0;
}


/* Called when user changes value of columns field */
static int columns_cb( LUI_FIELD *field, char *str )
{
   int nc;

#ifdef LEAVEOUT
   nc = atoi( str );
   if (nc) {
      /* adjust projection to match new rows */
      switch (V5Dout->Projection) {
         case 0:
         case 1:  /* rectilinear */
            V5Dout->ProjArgs[3] *= (float) V5Dout->Nc / (float) nc;
            break;
         case 2:  /* lambert */
            V5Dout->ProjArgs[3] *= (float) V5Dout->Nc / (float) nc;
            V5Dout->ProjArgs[5] *= (float) V5Dout->Nc / (float) nc;
            break;
         case 3:  /* polar stereographic */
            V5Dout->ProjArgs[3] *= (float) V5Dout->Nc / (float) nc;
            V5Dout->ProjArgs[4] *= (float) V5Dout->Nc / (float) nc;
            break;
         default:
            /* ??? */
            ;
      }
      load_projection_widgets( V5Dout );
      V5Dout->Nc = nc;
   }
#endif
   estimate_file_size();
   return 0;
}



static int levels_cb( LUI_FIELD *field, char *str )
{
   estimate_file_size();
   load_vertical_widgets( V5Dout );
   return 0;
}



/* Called when user clicks on a map projection radio button. */
static int proj_radio_cb( LUI_RADIO *r, int current )
{
   static float proj_save[20][MAX_PROJ_FIELDS];
   static int init_flag[20] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                               0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
   int i;

   /* save current parameters for this projection */
   if (V5Dout->Projection>=0) {
      for (i=0;i<MAX_PROJ_FIELDS;i++) {
         proj_save[V5Dout->Projection][i] = LUI_FieldGetDouble( projfield[i] );
      }
      init_flag[V5Dout->Projection] = 1;
   }

   V5Dout->Projection = current;

   /* restore parameters for this projection */
   if (current>=0) {
      for (i=0;i<MAX_PROJ_FIELDS;i++) {
         if (init_flag[current]) {
            V5Dout->ProjArgs[i] = proj_save[current][i];
         }
         else {
            V5Dout->ProjArgs[i] = 0.0;
         }
      }
   }

   load_projection_widgets( V5Dout );
   return 0;
}



/* Called when user clicks on "Projection..." button */
static int proj_cb( LUI_NEWBUTTON *button, XEvent *event )
{
   proj_radio_cb( proj_radio, V5Dout->Projection );
   XMapWindow( dpy, projwin );
   XRaiseWindow( dpy, projwin );
   return 0;
}


/* Called when user clicks on a VCS radio button. */
static int vert_radio_cb( LUI_RADIO *r, int current )
{

   if ((current==2 || current==3) &&
       (V5Dout->VerticalSystem!=2 && V5Dout->VerticalSystem!=3)) {
      /* switching to unequal km */
      if (V5Dout->VertArgs[2]==0.0) {
         float bottom, delta;
         int i, maxnl;

         /* setup nice defaults */
         bottom = V5Dout->VertArgs[0]; 
         delta = V5Dout->VertArgs[1] - V5Dout->VertArgs[0];

         maxnl = LUI_FieldGetInt( levels_field );

         for (i=2;i<maxnl;i++) {
            V5Dout->VertArgs[i] = bottom + i * delta;
         }
      }
   }

   V5Dout->VerticalSystem = current;
   load_vertical_widgets( V5Dout );
   return 0;
}



/* Called when user clicks on "Vertical..." button */
static int vert_cb( LUI_NEWBUTTON *button, XEvent *event )
{
   vert_radio_cb( vert_radio, V5Dout->VerticalSystem);
   XMapWindow( dpy, vertwin );
   XRaiseWindow( dpy, vertwin );
   return 0;
}


/* Called when user clicks on "Options..." button */
static int option_cb( void )
{
   XMapWindow( dpy, optionwin );
   return 0;
}


/* Called when user clicks on a "Close" button */
static int close_cb( LUI_NEWBUTTON *button, XEvent *event )
{
   if (button==close_var_button) {
      XUnmapWindow( dpy, varwin );
   }
   else if (button==close_time_button) {
      XUnmapWindow( dpy, timewin );
   }
   else if (button==close_proj_button) {
      XUnmapWindow( dpy, projwin );
   }
   else if (button==close_vert_button) {
      XUnmapWindow( dpy, vertwin );
   }
   else if (button==close_projlist_button) {
      XUnmapWindow( dpy, projlistwin );
   }
   else if (button==close_vcslist_button) {
      XUnmapWindow( dpy, vcslistwin );
   }
   else if (button==close_option_button) {
      XUnmapWindow( dpy, optionwin );
   }
   return 0;
}


/* Called when the selection status of a grid list entry changes. */
static int grid_select_cb( LUI_LIST *list, int entry, int state )
{
   LUI_ListSetStatus( list, entry, !state );

   estimate_file_size();
   return 0;
}



/* Called when the selection status of a variable list entry changes. */
static int var_select_cb( LUI_LIST *list, int entry, int state )
{
   struct grid_info *g;
   int i;

   select_variable( DB, entry, state );

   for (i=0,g=DB->FirstGrid; g; i++,g=g->Next) {
      if (g->NewSelState) {
         LUI_ListSetStatus( gridlister, i, g->SelectBits==ALL_BITS );
         g->NewSelState = 0;
      }
   }

   estimate_file_size();
   return 0;
}



/* Called when the selection status of a variable list entry changes. */
static int time_select_cb( LUI_LIST *list, int entry, int state )
{
   struct grid_info *g;
   int i;

   select_time( DB, entry, state );

   for (i=0,g=DB->FirstGrid; g; i++,g=g->Next) {
      if (g->NewSelState) {
         LUI_ListSetStatus( gridlister, i, g->SelectBits==ALL_BITS );
         g->NewSelState = 0;
      }
   }

   estimate_file_size();
   return 0;
}


/* Called when the selection status of a map projection in the projection
 * list window changes.
 */
static int proj_select_cb( LUI_LIST *list, int entry, int state )
{
   int i;
   struct grid_info *g;

   select_projection( DB, entry, state );

   for (i=0,g=DB->FirstGrid; g; i++,g=g->Next) {
      if (g->NewSelState) {
         LUI_ListSetStatus( gridlister, i, g->SelectBits==ALL_BITS );
         g->NewSelState = 0;
      }
   }

   estimate_file_size();
   return 0;
}



/* Called when the selection status of an entry in the vcs
 * list window changes.
 */
static int vcs_select_cb( LUI_LIST *list, int entry, int state )
{
   int i;
   struct grid_info *g;

   select_vcs( DB, entry, state );

   for (i=0,g=DB->FirstGrid; g; i++,g=g->Next) {
      if (g->NewSelState) {
         LUI_ListSetStatus( gridlister, i, g->SelectBits==ALL_BITS );
         g->NewSelState = 0;
      }
   }

   estimate_file_size();
   return 0;
}



/* Called when the value of a projection parameter field changes. */
static int proj_field_cb( LUI_FIELD *field, char *str )
{
   int i;

   for (i=0;i<MAX_PROJ_FIELDS;i++) {
      if (field==projfield[i]) {
         V5Dout->ProjArgs[i] = atof( str );
         break;
      }
   }
   return 0;
}


/*
 * Called when user clicks on "Guess" button in map projection window.
 * The result is that all selected grids are scanned to determine a "nice"
 * map projection.
 */
static int guess_proj_cb( void )
{
   setup_defaults( DB, V5Dout, 0, 1, 0 );

   load_projection_widgets( V5Dout );
   return 0;
}



/*
 * Called when user clicks on "Guess" button in the VCS window.
 * The result is that all selected grids are scanned to determine a "nice"
 * VCS.
 */
static int guess_vert_cb( void )
{
   setup_defaults( DB, V5Dout, 0, 0, 1 );

   load_vertical_widgets( V5Dout );
   return 0;
}



/* Called when the value of a VCS parameter field changes. */
static int vert_field_cb( LUI_FIELD *field, char *str )
{
   int i;
   int maxnl, top;

   if (LUI_RadioGetCurrent( vert_radio )==2) {
      maxnl = LUI_FieldGetInt( levels_field );
      if (maxnl<=MAX_VERT_FIELDS) {
         top = 0;
      }
      else {
         double pos = LUI_ScrollBarGetPos( vert_scrollbar );
         top = (int) (pos / 100.0 * (maxnl-MAX_VERT_FIELDS));
      }
   }
   else {
      top = 0;
   }

   for (i=0;i<MAX_VERT_FIELDS;i++) {
      if (field==vertfield[i]) {
         V5Dout->VertArgs[top+i] = INVERT(V5Dout, atof( str ));
         break;
      }
   }
   return 0;
}




/*
 * Called when the scrollbar in the vertical coordinate system window
 * is moved.
 */
static int vert_scrollbar_cb( LUI_SCROLLBAR *sb, float pos )
{
   int maxnl, top, i;

   maxnl = LUI_FieldGetInt( levels_field );
   if (maxnl<=MAX_VERT_FIELDS) {
      top = 0;
   }
   else {
      top = (int) (pos / 100.0 * (maxnl-MAX_VERT_FIELDS));
   }

   for (i=0;i<MAX_VERT_FIELDS;i++) {
      char str[100];
      sprintf( str, "Level %d Height (km):", top+i+1 );
      LUI_NewLabelChangeText( vertlabel[i], str );
      LUI_FieldSetDouble( vertfield[i], VERT(V5Dout, V5Dout->VertArgs[top+i] ));
   }
   return 0;
}




/* Called when user clicks on "Read file..." */
static int read_cb( void )
{
   LUI_BrowserActivate( file_browser, path );
   return 0;
}


/* Called when user has selected a file in the browser and clicked on "OK" */
static int read_ok_cb( LUI_BROWSER *browser, char *filepath )
{
   char **gridtext;
   int oldnumgrids;

   printf("Read: %s\n", filepath );

   oldnumgrids = DB->NumGrids;

   get_file_info( filepath, DB );

   analyze_grids( DB );

   select_all( DB, ALL_BITS, 1 );

   if (oldnumgrids==0 && DB->NumGrids>0) {
      /* Now we can setup defaults for the output file */
      setup_defaults( DB, V5Dout, 1,1,1 );
      load_projection_widgets( V5Dout );
      load_vertical_widgets( V5Dout );
      load_gridsize_widgets( V5Dout );
   }

   gridtext = sprint_grid_list( DB );
   LUI_ListLoad( gridlister, DB->NumGrids, gridtext, 1 );
   LUI_ListSetStatusAll( gridlister, 1 );

   load_var_window();
   load_time_window();
   load_projlist_window();
   load_vcslist_window();
   estimate_file_size();
   return 0;
}


static int discard_cb( void )
{
   free_all_grids( DB );
   load_var_window();
   load_time_window();
   load_projlist_window();
   load_vcslist_window();
   LUI_ListUnload( gridlister );
   LUI_ListUnload( timelister );
   LUI_ListUnload( varlister );
   LUI_ListUnload( projlister );
   LUI_ListUnload( vcslister );
/*   LUI_ListLoad( gridlister, 0, NULL );*/
   
   return 0;
}



/**********************************************************************/
/*****                Window and Widget Creation                  *****/
/**********************************************************************/


/*
 * Make the main window.
 */
static void make_mainwin( void )
{
   LUI_NEWBUTTON *read_button;
   LUI_NEWBUTTON *time_button, *var_button;
   LUI_NEWBUTTON *all_button, *none_button;
   LUI_NEWBUTTON *make_button, *go_button, *exit_button, *debug_button;
   LUI_NEWBUTTON *discard_button;
   LUI_NEWBUTTON *option_button;

   mainwin = LUI_CreateWindowAt( LUI_RootWindow, 1,1, 600, 595 );
   XMapWindow( dpy, mainwin );

   LUI_NewLabelCreate( mainwin, LUI_LEFT, LUI_TOP, 590, 30,
           "                    Vis5D Data Importer / Resampler" );

   LUI_NewLabelCreate( mainwin, LUI_LEFT, LUI_NEXT_Y, 590, 15,
   "Grid  YYDDD HHMMSS  Variable   Nr  Nc  Nl  Proj# VCS#  Filename" );

   gridlister = LUI_ListCreate( mainwin, LUI_LEFT, LUI_NEXT_Y, 590, 300, 1 );
   LUI_ListCallback( gridlister, grid_select_cb );

   read_button = LUI_PushButtonCreate( mainwin, LUI_LEFT, LUI_NEXT_Y, 120, 24,
                                       "Read file..." );
   LUI_ButtonCallback( read_button, read_cb );
   discard_button = LUI_PushButtonCreate( mainwin, LUI_NEXT_X, LUI_SAME_Y,
                                          150, 24, "Discard all grids" );
   LUI_ButtonCallback( discard_button, discard_cb );

   /*** Select by Time and Variable buttons ***/
   time_button = LUI_PushButtonCreate( mainwin,
                                       LUI_LEFT, LUI_NEXT_Y, 150, 24,
                                       "Select by time..." );
   LUI_ButtonCallback( time_button, timestep_cb );
   var_button = LUI_PushButtonCreate( mainwin,
                              LUI_NEXT_X, LUI_SAME_Y, 180, LUI_SAME_H,
                              "Select by variable..." );
   LUI_ButtonCallback( var_button, variable_cb );

   proj_button = LUI_PushButtonCreate( mainwin,
                                      LUI_NEXT_X, LUI_SAME_Y, 200, LUI_SAME_H,
                                      "Select by projection..." );
   LUI_ButtonCallback( proj_button, projlist_cb );

   vcs_button = LUI_PushButtonCreate( mainwin,
                                      LUI_LEFT, LUI_NEXT_Y, 140, LUI_SAME_H,
                                      "Select by VCS..." );
   LUI_ButtonCallback( vcs_button, vcslist_cb );

   all_button = LUI_PushButtonCreate( mainwin,
                                      LUI_NEXT_X, LUI_SAME_Y, 100, LUI_SAME_H,
                                      "Select All" );
   LUI_ButtonCallback( all_button, all_cb );
   none_button = LUI_PushButtonCreate( mainwin,
                                      LUI_NEXT_X, LUI_SAME_Y, 110, LUI_SAME_H,
                                      "Select None" );
   LUI_ButtonCallback( none_button, none_cb );


   /*** "Output label" ***/
   LUI_NewLabelCreate( mainwin, LUI_LEFT, LUI_NEXT_Y, 230,15, "" ); /*blank*/
   LUI_NewLabelCreate( mainwin, LUI_LEFT, LUI_NEXT_Y, 230,26,
                       "Output file parameters:" );

   size_label = LUI_NewLabelCreate( mainwin,
                                    LUI_NEXT_X, LUI_SAME_Y, 200, LUI_SAME_H,
                                    "Approx. size: 0 MB" );

   /*** Rows and Columns fields ***/
   LUI_NewLabelCreate( mainwin,
                       LUI_LEFT, LUI_NEXT_Y,
                       50, 26,
                       "Rows:" );
   rows_field = LUI_FieldCreate( mainwin,
                                 LUI_NEXT_X, LUI_SAME_Y, 70, LUI_SAME_H );
   LUI_FieldCallback( rows_field, rows_cb );

   LUI_NewLabelCreate( mainwin,
                       LUI_NEXT_X, LUI_SAME_Y,
                       70, LUI_SAME_H,
                       "Columns:" );
   columns_field = LUI_FieldCreate( mainwin,
                                    LUI_NEXT_X, LUI_SAME_Y, 70, LUI_SAME_H );
   LUI_FieldCallback( columns_field, columns_cb );
   LUI_FieldLink( rows_field, columns_field );

   /*** Grid levels button ***/
   LUI_NewLabelCreate( mainwin,
                       LUI_NEXT_X, LUI_SAME_Y,
                       100, LUI_SAME_H,
                       "Max Levels:" );
   levels_field = LUI_FieldCreate( mainwin,
                                   LUI_NEXT_X, LUI_SAME_Y, 70, LUI_SAME_H );
   LUI_FieldCallback( levels_field, levels_cb );
   LUI_FieldLink( columns_field, levels_field );


   /*** Projection button ***/
   proj_button = LUI_PushButtonCreate( mainwin,
                                       LUI_LEFT, LUI_NEXT_Y, 150, LUI_SAME_H,
                                       "Map Projection...");
   LUI_ButtonCallback( proj_button, proj_cb );

   /*** VCS button ***/
   vert_button = LUI_PushButtonCreate( mainwin,
                                       LUI_NEXT_X, LUI_SAME_Y, 210, LUI_SAME_H,
                                       "Vertical Coord System...");
   LUI_ButtonCallback( vert_button, vert_cb );

   /*** Options window button ***/
   option_button = LUI_PushButtonCreate( mainwin, LUI_NEXT_X, LUI_SAME_Y,
                                         100, 26, "Options..." );
   LUI_ButtonCallback( option_button, option_cb );


   /*** Output filename field and Make and Exit Buttons ***/
   LUI_NewLabelCreate( mainwin, LUI_LEFT, LUI_NEXT_Y, 90, 26,
                       "File name:" );
   outfile_field = LUI_FieldCreate( mainwin,
                                    LUI_NEXT_X, LUI_SAME_Y, 200, LUI_SAME_H );

   make_button = LUI_PushButtonCreate( mainwin,
                     LUI_NEXT_X, LUI_SAME_Y, 60, LUI_SAME_H, "Make" );
   LUI_ButtonCallback( make_button, make_cb );
   go_button = LUI_PushButtonCreate( mainwin,
                     LUI_NEXT_X, LUI_SAME_Y, 100, LUI_SAME_H, "Visualize" );
   LUI_ButtonCallback( go_button, go_cb );
   exit_button = LUI_PushButtonCreate( mainwin,
                      LUI_NEXT_X, LUI_SAME_Y, 60, LUI_SAME_H, "Exit" );
   LUI_ButtonCallback( exit_button, exit_cb );

   if (Debug) {
      debug_button = LUI_PushButtonCreate( mainwin,
                 LUI_NEXT_X, LUI_SAME_Y, LUI_SAME_W, LUI_SAME_H, "debug" );
      LUI_ButtonCallback( debug_button, debug_cb );
   }
}



/*
 * Make the time selection window.
 */
static void make_timewin( void )
{
   timewin = LUI_CreateWindowAt( LUI_RootWindow, 550,0,  200, 300 );

   LUI_NewLabelCreate( timewin, LUI_LEFT, LUI_TOP, 190, 30,
                       "Select grids by timestep:" );
   LUI_NewLabelCreate( timewin, LUI_LEFT, LUI_NEXT_Y, 190, 20,
                       "No. YYDDD HHMMSS" );
   timelister = LUI_ListCreate( timewin, LUI_LEFT, LUI_NEXT_Y, 190, 200, 0 );
   LUI_ListCallback( timelister, time_select_cb );

   close_time_button = LUI_PushButtonCreate( timewin, LUI_LEFT, LUI_NEXT_Y,
                                             80, 26, "Close" );
   LUI_ButtonCallback( close_time_button, close_cb );
}



/*
 * Make the variable selection window.
 */
static void make_varwin( void )
{
   varwin = LUI_CreateWindowAt( LUI_RootWindow, 0, 0, 200, 305 );

   LUI_NewLabelCreate( varwin, LUI_LEFT, LUI_TOP, 190, 30,
              "Select grids by variable:" );
   varlister = LUI_ListCreate( varwin, LUI_LEFT, LUI_NEXT_Y, 190, 230, 0 );
   LUI_ListCallback( varlister, var_select_cb );

   close_var_button = LUI_PushButtonCreate( varwin, LUI_LEFT, LUI_NEXT_Y,
                                             80, 26, "Close" );
   LUI_ButtonCallback( close_var_button, close_cb );

}



/*
 * Make the map projection window.
 */
static void make_projwin( void )
{
   static char *labels[] = { "Generic",
                             "Cylindrical Equidistant",
                             "Lambert Conformal",
                             "Polar Stereographic",
                             "Rotated" };
   LUI_NEWBUTTON *guess_proj_button;
   int i;

   projwin = LUI_CreateWindowAt( LUI_RootWindow, 0, 0, 300, 400 );

   LUI_NewLabelCreate( projwin, LUI_LEFT, LUI_TOP, 290,30,
                       "Output File's Map Projection" );

   proj_radio = LUI_RadioCreate( projwin, LUI_LEFT, LUI_NEXT_Y, 290,
                                 5, labels );
   LUI_RadioCallback( proj_radio, proj_radio_cb );

   for (i=0;i<MAX_PROJ_FIELDS;i++) {
      projlabel[i] = LUI_NewLabelCreate( projwin, LUI_LEFT, LUI_NEXT_Y,
                                         200, 26, "Proj Arg:" );
      projfield[i] = LUI_FieldCreate( projwin, LUI_NEXT_X, LUI_SAME_Y,
                                      85, 26 );
      LUI_FieldCallback( projfield[i], proj_field_cb );
      if (i>0) {
         LUI_FieldLink( projfield[i-1], projfield[i] );
      }
   }

   close_proj_button = LUI_PushButtonCreate( projwin, LUI_LEFT, LUI_NEXT_Y,
                                             80, 26, "Close" );
   LUI_ButtonCallback( close_proj_button, close_cb );
   guess_proj_button = LUI_PushButtonCreate( projwin, LUI_NEXT_X, LUI_SAME_Y,
                                             80, 26, "Guess" );
   LUI_ButtonCallback( guess_proj_button, guess_proj_cb );

}



/*
 * Make the vertical coordinate system window.
 */
static void make_vertwin( void )
{
   static char *labels[] = { "Linear, Equally spaced, Generic units",
                             "Linear, Equally spaced, Kilometers",
                             "Linear, Unequally spaced, Kilometers",
                             "Pressure, Unequally spaced, Millibars" };
   LUI_NEWBUTTON *guess_vert_button;
   int i;
   int x, y, height;

   vertwin = LUI_CreateWindowAt( LUI_RootWindow, 0, 0, 340, 275 );

   LUI_NewLabelCreate( vertwin, LUI_LEFT, LUI_TOP, 330,30,
                       "Output File's Vertical Coordinate System" );

   vert_radio = LUI_RadioCreate( vertwin, LUI_LEFT, LUI_NEXT_Y, 330,
                                 4, labels );
   LUI_RadioCallback( vert_radio, vert_radio_cb );

   for (i=0;i<MAX_VERT_FIELDS;i++) {
      vertlabel[i] = LUI_NewLabelCreate( vertwin, LUI_LEFT, LUI_NEXT_Y,
                                         180, 26, "Vert Arg:" );
      vertfield[i] = LUI_FieldCreate( vertwin, LUI_NEXT_X, LUI_SAME_Y,
                                      110, 26 );
      LUI_FieldCallback( vertfield[i], vert_field_cb );
      if (i>0) {
         LUI_FieldLink( vertfield[i-1], vertfield[i] );
      }
      if (i==0) {
         x = LUI_LayoutGet( LUI_NEXT_X );
         y = LUI_LayoutGet( LUI_SAME_Y );
      }
   }

   close_vert_button = LUI_PushButtonCreate( vertwin, LUI_LEFT, LUI_NEXT_Y,
                                             80, 26, "Close" );
   LUI_ButtonCallback( close_vert_button, close_cb );

   guess_vert_button = LUI_PushButtonCreate( vertwin, LUI_NEXT_X, LUI_SAME_Y,
                                             80, 26, "Guess" );
   LUI_ButtonCallback( guess_vert_button, guess_vert_cb );

   /* Make the slider for selecting which vertical argument */
   height = 26*MAX_VERT_FIELDS + LUI_LayoutGetGutter()*(MAX_VERT_FIELDS-1);
   vert_scrollbar = LUI_ScrollBarCreate( vertwin, x, y,
                                         20, height, 1 );
   LUI_ScrollBarCallback( vert_scrollbar, vert_scrollbar_cb );
}


static void make_file_browser( void )
{
   file_browser = LUI_BrowserCreate( 400, 400 );
   LUI_BrowserCallback( file_browser, read_ok_cb );
}



/*
 * Make the projection list window.
 */
static void make_projlistwin( void )
{
   projlistwin = LUI_CreateWindowAt( LUI_RootWindow, 0, 0, 508,200 );
   LUI_NewLabelCreate( projlistwin, LUI_LEFT, LUI_TOP, 500, 20,
                       "Select grids by map projection" );
   LUI_NewLabelCreate( projlistwin, LUI_LEFT, LUI_NEXT_Y, 500, 20,
                       "No. Type                Nr   Nc   Parameters" );
   projlister = LUI_ListCreate( projlistwin, LUI_LEFT, LUI_NEXT_Y, 500, 100,1 );
   LUI_ListCallback( projlister, proj_select_cb );

   close_projlist_button = LUI_PushButtonCreate( projlistwin, LUI_LEFT,
                                                 LUI_NEXT_Y, 80,26, "Close" );
   LUI_ButtonCallback( close_projlist_button, close_cb );
}



/*
 * Make the vcs list window.
 */
static void make_vcslistwin( void )
{
   vcslistwin = LUI_CreateWindowAt( LUI_RootWindow, 0, 0, 508,200 );
   LUI_NewLabelCreate( vcslistwin, LUI_LEFT, LUI_TOP, 500, 20,
                       "Select grids by vertical coordinate system" );
   LUI_NewLabelCreate( vcslistwin, LUI_LEFT, LUI_NEXT_Y, 500, 20,
                       "No. Type                         Nl   Parameters" );
   vcslister = LUI_ListCreate( vcslistwin, LUI_LEFT, LUI_NEXT_Y, 500, 100, 1 );
   LUI_ListCallback( vcslister, vcs_select_cb );

   close_vcslist_button = LUI_PushButtonCreate( vcslistwin, LUI_LEFT,
                                                 LUI_NEXT_Y, 80,26, "Close" );
   LUI_ButtonCallback( close_vcslist_button, close_cb );
}



/*
 * Make the options window.
 */
static void make_optionwin( void )
{
   static char *labels[] = { "Use higher-res grid",
                             "Averaging" };
   static char *byte_labels[] = { "1 byte (scaled int)",
                                  "2 bytes (scaled int)",
                                  "4 bytes (floating point)" };

   optionwin = LUI_CreateWindowAt( LUI_RootWindow, 0,0, 300, 254 );
   LUI_NewLabelCreate( optionwin, LUI_LEFT, LUI_TOP, 400, 30,
                       "             Options" );

   LUI_NewLabelCreate( optionwin, LUI_LEFT, LUI_NEXT_Y, 380, 20,
                       "Combine co-located data by:" );
   average_radio = LUI_RadioCreate( optionwin, 30, LUI_NEXT_Y,
                                    200, 2, labels );

   LUI_NewLabelCreate( optionwin, LUI_LEFT, LUI_NEXT_Y, 380, 20,
                       "Bytes per grid point:" );
   bytes_radio = LUI_RadioCreate( optionwin, 30, LUI_NEXT_Y,
                                  230, 3, byte_labels );

   /* empty space: */
   LUI_NewLabelCreate( optionwin, LUI_LEFT, LUI_NEXT_Y, 10, 10, " " );

   close_option_button = LUI_PushButtonCreate( optionwin, LUI_LEFT, LUI_NEXT_Y,
                                               80, 26, "Close" );
   LUI_ButtonCallback( close_option_button, close_cb );
}


/*
 * Make the graphical interface.
 */
void make_gui( void )
{
   dpy = XOpenDisplay( NULL );
   LUI_BorderWidth( 2 );
   LUI_Initialize( "v5dimport (4.2)", dpy, NULL, 0, 0 );


   make_mainwin();
   make_timewin();
   make_varwin();
   make_projwin();
   make_vertwin();
   make_projlistwin();
   make_vcslistwin();
   make_file_browser();
   make_optionwin();
}



/*
 * Main loop when using the graphical interface.
 */
int gui_loop( struct grid_db *db, v5dstruct *v5dout )
{
   char **grids;

   DB = db;
   V5Dout = v5dout;

   /* initialize grid list window */
   grids = sprint_grid_list( DB );
   LUI_ListLoad( gridlister, DB->NumGrids, grids, 1 );
   /* select all grids */
   LUI_ListSetStatusAll( gridlister, 1 );

   /* initialize projection widgets window */
   load_time_window();
   load_var_window();
   load_projection_widgets( v5dout );
   load_vertical_widgets( v5dout );
   load_gridsize_widgets( v5dout );

   LUI_EventProcess();
   return 0;
}
