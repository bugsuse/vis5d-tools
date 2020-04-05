/*  main.c */
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

Yo u should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <signal.h>
#ifdef sgi
#  include <sys/sysmp.h>
#endif
#include "api.h"
#include "gui.h"
#include "graphics.h"
#include "vis5d.h"
#include "script.h"
#include "pipe.h"
#include "queue.h"

#ifndef MBS
#  define MBS 0
#endif
#ifndef TOPOFILE
#  define TOPOFILE "EARTH.TOPO"
#endif
#ifndef WORLDFILE
#  define WORLDFILE "OUTLSUPW"
#endif
#ifndef USAFILE
#  define USAFILE "OUTLUSAM"
#endif


#define MAX(A,B)  ( (A) > (B) ? (A) : (B) )
#define CP_WIDTH  384



/*
 * Print list of options.
 */
static void usage( void )
{
#define P printf
   P("Vis5D version 5.2  (MBS=%d)\n", MBS);
   P("Usage:\n");
   P("   vis5d file.v5d [options]\n");
   P("Options:\n");
#if defined(SGI_GL) || defined(DENALI) || defined(OPENGL)
   P("   -alpha\n");
   P("      Use alpha blending instead of screen door transparency.\n");
#endif
#ifdef MCIDAS
   P("   -area N\n");
   P("      Specifies the first McIDAS AREA file in a sequence to\n");
   P("      display in the 3-D box.\n");
#endif
   P("   -barbs\n");
   P("      Display wind barbs rather than vectors.\n");
   P("   -box alon alat ahgt\n");
   P("      Set aspect ratio of the 3-D box (longitude:latitude:height).\n");
   P("      Example:  vis5d LAMPS.v5d -box 4 2 1\n");

   /* MJK 12.02.98 */
   P("   -circle\n");
   P("      Display a circular clock\n");

   /* MJK 12.15.98 */
   P("   -cpgeom WIDTHxHEIGHT+X+Y  (or WIDTHxHEIGHT or +X+Y)\n");
   P("      Specify the geometry (position only) of the control panel\n");
   P("      Since the control panel must be a fixed width, any WIDTH\n");
   P("      specification will be ignored.\n");
   P("   -date\n");
   P("      Display date as dd month yy rather than yyddd on clock.\n");
   P("   -dwell ms\n");
   P("      Set the animation dwelling time when stepping from time=Numtimes \n");
   P("      to time=0\n");
#if defined(SGI_GL) || defined(DENALI)
   P("   -font fontname size\n");
   P("      Set the IRIS GL font and its height used in the viewing window.\n");
   P("      Use the 'listfonts' program to list your system's font names.\n");
   P("      Size of 72 equals one inch (2.54 cm).\n");
   P("      Example:  vis5d LAMPS.v5d -font Helvetica 30\n");
#endif
#if defined(VOGL) || defined(OPENGL)
   P("   -font fontname\n");
   P("      Set the X font used in the viewing window.  Use xlsfonts(1)\n");
   P("      to list your system's fonts.\n");
   P("      Example:  vis5d LAMPS.v5d -font fg-30\n");
   P("   -soundfont fontname\n");
   P("      Set the X font used in the sounding window. Use xlsfonts(1)\n");
   P("      to list your system's fonts.\n");
   P("      Example:  vis5d LAMPS.v5d -soundfont fg-30\n");
#endif

   /* MJK 12.15.98 */
#ifdef PEX
   P("   -font fontname size\n");
   P("      Set the X or PEX font and its height used in the 3D window.\n");
   P("      Since X fonts have explicit heights, size controls the line\n");
   P("      spacing. A size of 0 will restore the line spacing to the\n");
   P("      font's height.  PEX fonts (those having \"PEX\" in their\n");
   P("      names) are scalable.  To restore the default PEX font, use\n");
   P("      \" \" for the name.  A PEX font's default size may be restored\n");
   P("      by specifying a size of 0.\n");
   P("      Example:  vis5d LAMPS.v5d -font helvb24 20\n");
#endif

   P("   -full\n");
   P("      Full-screen window; make the 3-D window as large as possible.\n");
   P("   -funcpath pathname\n");
   P("      Specify directory to search for user Fortran functions.\n");
   P("      Example:  vis5d LAMPS.v5d -funcpath /usr/local/vis5d/userfuncs\n");
   P("   -geometry WIDTHxHEIGHT+X+Y  (or WIDTHxHEIGHT or +X+Y)\n");
   P("      Specify the geometry of the 3-D window\n");
   P("   -hirestopo\n");
   P("      Display high resolution topography.  Recommended only for\n");
   P("      fast graphics systems.\n");
   P("   -legend position size x y\n");
   P("      Set color legend position (1=BOT,2=TOP,3=LFT,4=RT), size, and \n");
   P("      relative x and y position from default, usually 0 and 0.\n");
   P("   -log [log_scale] [log_exponent]\n");
   P("      Specify a logrithmic vertical scale.\n");
   P("      Default log_scale = %f\n", DEFAULT_LOG_SCALE );
   P("      Default log_exponent = %f\n", DEFAULT_LOG_EXP );
   P("   -map mapfile\n");
   P("      Use a different map outline file instead of OUTLSUPW\n");
   P("      Example:  vis5d LAMPS.v5d -map OUTLUSAL\n");
   P("   -nofile\n");
   P("      Run vis5d with out loading a vis5d data set\n");
   P("   -top_margin size\n");
   P("      Specify the size in points for the top margin of the 3d windows\n");
   P("   -bottom_margin size\n");
   P("      Specify the size in points for the bottom margin of the 3d windows\n");
   P("   -left_margin size\n");
   P("      Specify the size in points for the left margin of the 3d windows\n");
   P("   -right_margin size\n");
   P("      Specify the size in points for the right margin of the 3d windows\n");
   P("   -mbs n\n");
   P("      Limit the memory used by vis5d to 'n' megabytes.  When\n");
   P("      the limit is exceeded, the least-recently-viewed graphics\n");
   P("      are deallocated.\n");
#ifdef OPENGL
   P("   -offscreen\n");
   P("       Do off screen rendering, used in conjunction with -script command\n");
#endif
   P("   -path pathname\n");
   P("      Specify directory to search for map and topograpy files.\n");
   P("      Example:  vis5d LAMPS.v5d -path /usr3/data\n");
   P("   -projection p\n");
   P("      Set the map projection.  Default is obtained from the datafile.\n");
   P("      p may be one of:\n");
   P("          generic     - rectilinear grid in generic units\n");
   P("          linear      - rectilinear lat/lon grid (cylindrical equidistant)\n");
   P("          lambert     - Lambert Conformal or Polar Stereographic\n");
   P("          stereo      - Azimuthal Stereographic\n");
   P("          rotated     - rotated rectilinear lat/lon grid\n");
   P("          cylindrical - rectilinear lat/lon grid drawn as a cylinder\n");
   P("          spherical   - rectilinear lat/lon grid drawn as a sphere\n");
   P("      Only the first 3 characters of p are needed.\n");
   P("      You will be prompted for additional parameters.\n");
   P("      Example:   vis5d LAMPS.v5d -projection spherical\n");
   P("   -quickstart\n");
   P("      Don't load any data when starting up vis5d, even if the whole\n");
   P("      file will fit into memory.  Useful when reading files by\n");
   P("      NFS.\n");
   P("   -rate ms\n");
   P("      Set the animation rate.  ms is the miniumum time delay in\n");
   P("      milli-seconds between frames.  Default is 100.\n");
   P("   -reverse_poles\n");
   P("      When using a cylindrical projection this option will cause the\n");
   P("      south pole to be at the center of the disk\n");
   P("   -samescale\n");
   P("      Set the scale for the vertical plot variables to be the same\n");
   P("      for all three variables\n");
   P("   -script filename\n");
   P("      Execute a Tcl script for controlling Vis5D.\n");
#if defined(SGI_GL) || defined(DENALI) || defined(OPENGL)
   P("   -sequence filename\n");
   P("      Specify a sequence of images to texture map over the topography.\n");
   P("   -texture image.rgb\n");
   P("      Specify an image to texture map over the topography.\n");
#endif
   P("   -title x y font \"string\"\n");
   P("       This is used in conjunction with the -_margin option.  This will\n");
   P("       print the string at location (x,y) in the BigWindow.  Any number\n");
   P("       of titles can be created. The strings will not show up if they are not in\n");
   P("       a margin.\n");
   P("   -topo topfile\n");
   P("      Use a different topography file instead of EARTH.TOPO.\n");
   P("      Example:  vis5d LAMPS.v5d -topo MARS.TOPO\n");


   /* MJK 12.02.98 */
   P("   -topobase [lev_value]\n");
   P("      Display a base below the topography.  lev_value is the\n");
   P("      vertical grid level value to which the base will extend.\n");
   P("      Typically, this is a negative value so that the base is below\n");
   P("      the lowest level (0.0).  If lev_value is omitted, 0.0 will be\n");
   P("      used.\n");
   P("      Example:  vis5d LAMPS.v5d -topobase -3.0\n");


   P("   -trajvars uvar vvar [wvar]\n");
   P("      Specify which variables to use for tracing trajectories\n");
   P("      Example:  vis5d dataset -trajvars U-REL V-REL WREL\n");


   /* MJK 12.02.98 */
   P("   -userdata flags\n");
   P("      Use user-provided functions to read data, maps, or topo\n");
   P("      flags is a string that may contain any combination of:\n");
   P("         g or G    - use the function for reading grid data\n");
   P("         m or M    - use the function for reading map data\n");
   P("         t or T    - use the function for reading topo data\n");
   P("      Example:  vis5d dataset -userdata gtM\n");


   P("   -vertical v\n");
   P("      Define the vertical coordinate system.  Default is from datafile.\n");
   P("      v may be one of:\n");
   P("          generic    - equally spaced levels in generic units\n");
   P("          equal      - equally spaced levels in km\n");
   P("          unequal    - unequally spaced levels in km\n");
   P("      Only the first 3 characters of v are needed.\n");
   P("      You will be prompted for additional parameters.\n");
   P("      Example:   vis5d LAMPS.v5d -vertical equal\n");
   P("   -verylarge flag (0 or 1)\n");
   P("      Force VeryLarge flag to be set or cleared.  This controls\n");
   P("      whether graphics generation is sync'd with rendering.\n");
   P("      Default is calculated from ratio of file and memory sizes.\n");
   P("   -wdpy xdisplay\n");
   P("      Make widgets appear on a different display.  Useful in\n");
   P("      combination with -full for making slides and videos.\n");
   P("      Example:  vis5d LAMPS.v5d -full -wdpy pluto:0\n");
   P("   -wide w\n");
   P("      Set width of line segments in pixels (default is 1.0).\n");
   P("   -wind2 uvar vvar [wvar]\n");
   P("      Specify which variables to use when computing the HWIND-2\n");
   P("      and VWIND-2 wind vector graphics.\n");
   P("   -widescreen\n");
   P("      Bring up a 1x3 display matrix\n");
   P("Keyboard functions (when pointing in 3-D window):\n");
   P("   F1 - Raise/lower control panel window.\n");
   P("   F2 - Toggle display of system info.\n");
   P("   P  - Print window image to PostScript printer.\n");
   P("   F  - Faster animation.\n");
   P("   S  - Slower animation.\n");
   P("   l  - reduce the size of Vis5D logo\n");
   P("   L  - increse the size of Vis%D logo\n");

#undef P
}



/*
 * Reset the timer or return milliseconds elapsed since last reset.
 * Input:  index    = display_ctx index
           flag - 0 = reset timer
 *                1 = return elapsed time in ms.
 * Return:  elapsed time in ms.
 */
static int timer( int index, int flag )
{
   struct timeval tp;
   static long start_usec, start_sec, lapsed_usec;

   
   /* Get the current time */
   gettimeofday(&tp, (struct timezone *) 0);

   if (flag) {
      /* Return elapsed time */
      vis5d_get_display_timer( index, &start_sec, &start_usec);
      lapsed_usec = ((tp.tv_sec - start_sec) * 1000000) +
                    tp.tv_usec - start_usec;
      return lapsed_usec / 1000;
   }
   else {
      /* Reset timer */
      vis5d_reset_display_timer( index );
      return 0;
   }
}



/*
 * This is called by main.  It reads user input and updates the 3-D
 * display until the program terminates.
 */
/* static int main_loop( int index ) */
/* WLH 29 Sept 98
static int main_loop( void )
*/
static int main_loop(char *pipe_name)
{
   int block;
   int numtimes, verylarge;
   int yo, howmany, whichones[VIS5D_MAX_CONTEXTS], spandex;
   int ghowmany, gwhichones[VIS5D_MAX_CONTEXTS];
   int dindex, main_redraw;
   int redraw, work, working;
   int DR, DC, stuff;
   static int cursor = 0;
   


   get_display_matrix( &DR, &DC);
   for (dindex = 0; dindex < DR * DC; dindex++) {
      timer(dindex, 0);  /* reset timer */
   }
   while (1){
/* WLH 29 Sept 98 */
      if (pipe_name != NULL) {
        check_pipe(pipe_name);
      }
      get_display_matrix( &DR, &DC);
      /* once around this while loop for each animation step or redraw */
      /* MJK 11.17.98 */

      vis5d_check_work( &work );

      get_queue_info(&working, &stuff);
      if (working && !cursor){
         XSync(GuiDpy, 0);
         set_busy_cursor( 1 );
         cursor = 1;
      }
      else if (!working && cursor){
         XSync(GuiDpy, 0);
         set_busy_cursor( 0 );
         cursor = 0;
      }
      

/* WLH 29 Sept 98
      block = (work == 0);
*/
      block = (work == 0 && pipe_name == NULL);
      for (dindex = 0; dindex < DR * DC; dindex++) {
         GuiContext gtx = get_gui_gtx2(dindex );
         /* Determine if we should block/wait or poll for user input */
         if(gtx){
            vis5d_check_redraw( dindex, &redraw );
            block = block && redraw == 0 && (gtx->GoTime == 0);
         }
      } 

      main_redraw = 0;
      while (1) {
         int input_status;
         for (dindex = 0; dindex < DR * DC; dindex++) {
            GuiContext gtx = get_gui_gtx2(dindex );
            if(gtx){
               /* Determine if we should block/wait or poll for user input */
               vis5d_check_redraw( dindex, &redraw );
               if( gtx ){
                  block = block && redraw == 0 && (gtx->GoTime == 0);

               }
               else{
                  block = block && redraw;
               }
            }
         }

         /* MJK 12.15.98 */
         vis5d_check_work( &work );
         if (work) block = 0;


         input_status = get_user_input(block);
         /* if (input_status != 0) main_redraw = 1;  */
         if (input_status != 2) {  /* ???? WLH 29 Aug 97 */
            break;

         }
      }
      for (dindex = 0; dindex < DR * DC; dindex++) {
         GuiContext gtx = get_gui_gtx2(dindex );
         if (gtx){
            if (gtx->group_index > 0){
               vis5d_get_grp_numtimes( gtx->group_index, &numtimes);
            }
            else {
               vis5d_get_dtx_numtimes( dindex, &numtimes );
            }
            if (main_redraw != 0) vis5d_signal_redraw(dindex, 1);
            /* Do this when not animating: */
            if (gtx->GoTime == 0) {
               vis5d_check_redraw( dindex, &redraw );
               if (redraw) {

                  vis5d_draw_frame( dindex, gtx->GoTime );
                  gtx->needswap = 1;
/*                  vis5d_swap_frame( dindex );  */
                  /* BIG ATTENTION */
                  /* WLH 29 Aug 97: move vis5d_swap_frame calls into another loop?? */
               }
            }
            /* Do this while animating: */
            else { /* gtx->GoTime != 0) */
               if ((timer(dindex, 1)) >= gtx->AnimRate) {
                  int time;
                  vis5d_get_dtx_timestep( dindex, &time );
                  if (gtx->group_index >0){
                     vis5d_get_grp_timestep(gtx->group_index, &time);
                  }
                  if (gtx->GoTime==1) {
                     /* Step forward */
                     time++;
                     if (time >= numtimes) {
                        time = 0;
                     }
                  }
                  else {
                     /* Step backward */
                     time--;
                     if (time<0) {
                        time = numtimes-1;
                     }
                  }
                  if (gtx->group_index >0){
                     vis5d_get_num_of_dtxs_in_group( gtx->group_index, &ghowmany,gwhichones);
                     /* only do the group_redraw once, other wise it will */
                     /* advance the timestep for each group everytime it */
                     /* gets to a different gtx/dtx */
                     if (gtx->context_index == gwhichones[0]){
                        vis5d_set_grp_timestep(gtx->group_index, time);
                        vis5d_signal_group_redraw( gtx->group_index, 1);
                     }
                  }
                  else{
                     vis5d_set_dtx_timestep( dindex, time );
                     vis5d_signal_redraw(dindex, 1);
                  }
                  timer(dindex, 0);  /* reset timer */
                  
                  vis5d_get_num_of_ctxs_in_display( dindex, &howmany, whichones);    
                  for (yo = 0; yo < howmany; yo++){
                     spandex = whichones[yo]; 
                     verylarge = vis5d_verylarge_mode(spandex, VIS5D_GET);
                     if (verylarge) {
                        /* make graphics */
                        vis5d_make_timestep_graphics(spandex, time);
                        vis5d_finish_work();
                     }
                  }
               } /* end if (timer(dindex, 1) >= gtx->AnimRate) */
               vis5d_check_redraw( dindex, &redraw );
               if (redraw) {
                  vis5d_draw_frame( dindex, gtx->GoTime );
                  gtx->needswap = 1;
/*                  vis5d_swap_frame( dindex ); */
                  /* BIG ATTENTION */
                  /* WLH 29 Aug 97: move vis5d_swap_frame calls into another loop?? */
               }
              /* MJK 12.15.98 begin */
               if ((gtx->GoTime != 0) && (gtx->AnimDwell > 0))
               {
                  int   time;

                  vis5d_get_dtx_timestep (dindex, &time);
                  if (((gtx->GoTime > 0) && (time == (numtimes-1))) ||
                      ((gtx->GoTime < 0) && (time == 0)))
                  {
                     timer (dindex, 0);
                     while (timer (dindex, 1) < gtx->AnimDwell);
                  }
               }
              /* MJK 12.15.98 end */


            } /* end if (gtx->GoTime != 0) */
         }/*end if(gtx)*/
         get_queue_info(&working, &stuff);
         if (working && !cursor){
            set_busy_cursor( 1 );
            cursor = 1;
         }
         else if (!working && cursor){
            set_busy_cursor( 0 );
            cursor = 0;
         }
         vis5d_do_work();
      } /* end for (dindex = 0; dindex < DR * DC; dindex++) */

      for (dindex = 0; dindex < DR * DC; dindex++) {
         GuiContext gtx = get_gui_gtx2(dindex );
         if (gtx){
            if (gtx->needswap){
               vis5d_swap_frame( dindex );
               gtx->needswap = 0;
            }
         }
      }
      get_queue_info(&working, &stuff);
      if (working && !cursor){
         cursor = 1;
         set_busy_cursor( 1 );
      }
      else if (!working && cursor){
         cursor = 0;
         set_busy_cursor( 0 );
      }
      vis5d_do_work();     /*if not in parallel mode, do a queued job  */
   }
}

/*
 * Create the 2-D Sounding window
 */
static int make_snd_window(int index, char *title, Window scw,
                            char *wdpy_name )
{
   GuiContext gtx = get_gui_gtx(index);
   int xpos, ypos, height, width, scrwidth, scrheight;
   Display *dpy;
   int scr;
   
   dpy = XOpenDisplay( NULL );
   if (!dpy) {
      printf("Unable to open default display\n");
      exit(1);
   }

   scr = DefaultScreen( dpy );

   scrwidth = DisplayWidth( dpy, scr );
   scrheight = DisplayHeight( dpy, scr );

   height = 10;
   width  = 10;
   /* MJK 12.15.98 */
   xpos = 0;
   ypos = -1;
/*
   xpos = 15;
   ypos = 15;
*/

   return vis5d_init_sndwindow( index, "Skew-T and Vertical Plot Displsy",
                                 xpos, ypos, width, height, scw, wdpy_name);
}



/*
 * Create the 3-D rendering window.
 */
/* MJK 12.07.98 begin */
static int make_gfx_window(  char *title, char *geom_str)
{
   int xpos, ypos, width, height;
   int scrwidth, scrheight;
   Display *dpy;
   int scr;

   dpy = XOpenDisplay( NULL );
   if (!dpy) {
      printf("Unable to open default display\n");
      exit(1);
   }

   scr = DefaultScreen( dpy );

   scrwidth = DisplayWidth( dpy, scr );
   scrheight = DisplayHeight( dpy, scr );


   /* compute best pos and size */
#if defined(VOGL) || defined(MESA)
   /* use smaller screen w/ VOGL and Mesa for faster rendering */
   height = width = (scrwidth - 420) * 2 / 3;
#else
   height = width = scrwidth - 420;
#endif
   xpos = 410;
   ypos = 10;

   if (get_window_geometry (geom_str, &width, &height, &xpos, &ypos) < 0) {
      printf("bad value (%s) for geometry in make_gfx_window()\n", geom_str);
      exit(1);
   }

   if (xpos < 0) xpos = scrwidth - width;
   if (ypos < 0) ypos = scrheight - height;
  
/* MJK 4.14.99 */
   if (StaticWin){
      StaticWinXPos = xpos;
      StaticWinYPos = ypos;
      StaticWinWidth = width;
      StaticWinHeight = height;
   }

   return vis5d_init_window( title, xpos, ypos, width, height );
}
/* MJK 12.07.98 end */



/*
 * Get a floating point value from stdin, don't change the value if
 * a blank line is read.
 */
static void input_float( float *fptr )
{
   char str[1000];

   gets( str );
   if (str[0]!=0 && str[0]!='\n') {
      *fptr = atof( str );
   }
}




/*
 * Prompt the user to enter projection parameters for the specifed projection
 * type.
 * Input:  index - the context index
 *         proj - the projection type, one of PROJ_* symbols
 * Output:  the current and then modified projection parameters
 */
static void prompt_for_projection_args( int index, int proj, float projargs[] )
{
   int current_proj;

   /* get default values */
   vis5d_get_ctx_projection( index, &current_proj, projargs );

   /* Prompt user for projection parameters now */
   printf("Enter projection parameters, press Return for (default)\n");

   switch (proj) {
      case PROJ_GENERIC:
         printf("Generic projection (no units):\n");
         printf("   North Bound (%g): ", projargs[0] );
         input_float( &projargs[0] );
         printf("   West Bound (%g): ", projargs[1] );
         input_float( &projargs[1] );
         printf("   Row Increment (%g): ", projargs[2] );
         input_float( &projargs[2] );
         printf("   Column Increment (%g): ", projargs[3] );
         input_float( &projargs[3] );
         break;
      case PROJ_LINEAR:
         printf("Cylindrical Equidistant projection:\n");
         printf("   North Bound Latitude (degrees) (%g): ", projargs[0] );
         input_float( &projargs[0] );
         printf("   West Bound Longitude (degrees) (%g): ", projargs[1] );
         input_float( &projargs[1] );
         printf("   Row Increment (degrees) (%g): ", projargs[2] );
         input_float( &projargs[2] );
         printf("   Column Increment (degrees) (%g): ", projargs[3] );
         input_float( &projargs[3] );
         break;
      case PROJ_LAMBERT:
         if (current_proj!=PROJ_LAMBERT) {
            int nr, nc;
            vis5d_get_size( index, &nr, &nc, NULL,NULL,NULL,NULL,NULL,NULL );
            projargs[0] = 70.0;
            projargs[1] = 20.0;
            projargs[2] = -nr;
            projargs[3] = nc/2;
            projargs[4] = 90.0;
            projargs[5] = 100.0;
         }
         printf("Lambert Conformal projection:\n");
         printf("   Standard Latitude 1 (degrees) (%g): ", projargs[0] );
         input_float( &projargs[0] );
         printf("   Standard Latitude 2 (degrees) (%g): ", projargs[1] );
         input_float( &projargs[1] );
         printf("   North/South Pole row (%g): ", projargs[2] );
         input_float( &projargs[2] );
         printf("   North/South Pole column (%g): ", projargs[3] );
         input_float( &projargs[3] );
         printf("   Central longitude (degrees) (%g): ", projargs[4] );
         input_float( &projargs[4] );
         printf("   Column increment (km) (%g): ", projargs[5] );
         input_float( &projargs[5] );
         break;
      case PROJ_STEREO:
         if (current_proj!=PROJ_STEREO) {
            /* default is north pole polar stereographic */
            int nr, nc;
            vis5d_get_size( index, &nr, &nc, NULL,NULL,NULL,NULL,NULL,NULL );
            projargs[0] = 90.0;
            projargs[1] = 0.0;
            projargs[2] = nr/2.0;
            projargs[3] = nc/2.0;
            projargs[4] = 500.0;
         }
         printf("Stereographic:\n");
         printf("  Center Latitude (degrees) (%g): ", projargs[0] );
         input_float( &projargs[0] );
         printf("  Center Longitude (degrees) (%g): ", projargs[1] );
         input_float( &projargs[1] );
         printf("  Center Row (%g): ", projargs[2] );
         input_float( &projargs[2] );
         printf("  Center Column (%g): ", projargs[3] );
         input_float( &projargs[3] );
         printf("  Column spacing (km) (%g): ", projargs[4] );
         input_float( &projargs[4] );
         break;
      case PROJ_ROTATED:
         printf("Rotated Equidistant projection:\n");
         printf("  Latitude on rotated globe of grid row 0 (degrees) (%g): ",
                projargs[0] );
         input_float( &projargs[0] );
         printf("  Longitude on rotated globe of grid column 0 (degrees)");
         printf(" (%g): ", projargs[1] );
         input_float( &projargs[1] );
         printf("   Row Increment on rotated globe (degrees) (%g): ",
                projargs[2] );
         input_float( &projargs[2] );
         printf("   Column Increment on rotated globe (degrees) (%g): ",
                projargs[3] );
         input_float( &projargs[3] );
         printf("  Earth Latitude of (0, 0) on rotated globe (degrees) (%g): ",
                projargs[4] );
         input_float( &projargs[4] );
         printf("  Earth Longitude of (0, 0) on rotated globe (degrees) (%g): ",
                projargs[5] );
         input_float( &projargs[5] );
         printf("   Clockwise rotation of rotated globe (degrees) (%g): ",
                projargs[6] );
         input_float( &projargs[6] );
         break;

      case PROJ_CYLINDRICAL:
      case PROJ_SPHERICAL:
         if (proj==PROJ_CYLINDRICAL) {
            printf("Cylindrical projection of rectangular grid:\n");
         }
         else {
            printf("Spherical projection of rectangular grid:\n");
         }
         printf("   North Bound Latitude (degrees) (%g): ", projargs[0] );
         input_float( &projargs[0] );
         printf("   West Bound Longitude (degrees) (%g): ", projargs[1] );
         input_float( &projargs[1] );
         printf("   Row Increment (degrees) (%g): ", projargs[2] );
         input_float( &projargs[2] );
         printf("   Column Increment (degrees) (%g): ", projargs[3] );
         input_float( &projargs[3] );
         break;
      default:
         /* should never allow a bad projection number to get this far! */
         abort();
   }
}



/*
 * Prompt the user to enter parameters for the given vertical coordinate
 * sytem type.
 * Input:  index - the context index number
 *         vcs - the vertical coordinate system number, one of VERT_*
 * Output:  vertargs - the vcs arguments
 */
static void prompt_for_vertical_args( int index, int vcs, float vertargs[] )
{
   int current_vcs;
   int maxnl, i;
   float pressure;

   vis5d_get_size( index, NULL, NULL, NULL, NULL, &maxnl, NULL, NULL, NULL );

   /* get default values */
   vis5d_get_ctx_vertical( index, &current_vcs, vertargs );

   switch (vcs) {
      case VERT_GENERIC:
         printf("Generic linear vertical coordinate system:\n");
         printf("  Bottom Bound (%g): ", vertargs[0] );
         input_float( &vertargs[0] );
         printf("  Level Increment (%g): ", vertargs[1] );
         input_float( &vertargs[1] );
         break;
      case VERT_EQUAL_KM:
         printf("Linear, equally spaced levels in km:\n");
         printf("  Bottom Bound (km) (%g): ", vertargs[0] );
         input_float( &vertargs[0] );
         printf("  Level Increment (km) (%g): ", vertargs[1] );
         input_float( &vertargs[1] );
         break;
      case VERT_NONEQUAL_KM:
         printf("Linear, non-equally spaced levels in km:\n");
         for (i=0;i<maxnl;i++) {
            printf("  Level %d Height (km) (%g): ", i, vertargs[i] );
            input_float( &vertargs[i] );
         }
         break;
      case VERT_NONEQUAL_MB:
         printf("Linear, non-equally spaced levels in mb:\n");
         for (i=0;i<maxnl;i++) {
            printf("  Level %d Pressure (km) (%g): ", i,
                   height_to_pressure(vertargs[i]) );
            input_float( &pressure );
            vertargs[i] = pressure_to_height(pressure);
         }
         break;
      default:
         /* should never get this far with a bad vcs type! */
         abort();
   }
}



main( int argc, char *argv[] )
{
   int i;
   struct rlimit rl;
   int cpus;
   GuiContext gtx;
   char windowname[100];
   char swindowname[100];
   int volflag;
   int index, do_once;
   int workers = 0;
   int filepointer = 0;
   int gopointer = 0;
   int onetime = 1;
   int rows, cols, yo;

   char *v5dfile[VIS5D_MAX_DPY_CONTEXTS];

   /* command line arguments with default values: */
   int first_area[VIS5D_MAX_DPY_CONTEXTS];      /* -area */
   char *sequence[VIS5D_MAX_DPY_CONTEXTS];      /*-sequence */
   int alpha[VIS5D_MAX_DPY_CONTEXTS];           /* -alpha */
   int barbs[VIS5D_MAX_DPY_CONTEXTS];           /* -barbs */
   float boxx[VIS5D_MAX_DPY_CONTEXTS],
         boxy[VIS5D_MAX_DPY_CONTEXTS],
         boxz[VIS5D_MAX_DPY_CONTEXTS];          /* -box */
   char *fontname[VIS5D_MAX_DPY_CONTEXTS];      /* -font */
   char *soundfontname[VIS5D_MAX_DPY_CONTEXTS]; /* -soundfont */
   int fontsize[VIS5D_MAX_DPY_CONTEXTS];
/* MJK 4.27.99
   char *funcpath[VIS5D_MAX_DPY_CONTEXTS];       -funcpath 
*/

   /* MJK 12.04.98 */
   char *gfx_geom_str = NULL;                   /* -geometry */
   char full_geom_str[] = "full";
   char *cp_geom_str = NULL;                    /* -cpgeom */


    
   int geometry = 0;                            /* -geometry */
   int width, height, xpos, ypos;
   int hirestopo[VIS5D_MAX_DPY_CONTEXTS];       /* -hirestopo */
   int widescreen;

   /* MJK 12.02.98 */
   int circleclock[VIS5D_MAX_DPY_CONTEXTS];     /* -circle */

   int julian[VIS5D_MAX_DPY_CONTEXTS];          /* -date */
   char *mapname[VIS5D_MAX_DPY_CONTEXTS];       /* -map */
   int mbs[VIS5D_MAX_DPY_CONTEXTS];             /* -mbs */
/* MJK 4.27.99   
   char *path[VIS5D_MAX_DPY_CONTEXTS];           -path 
*/
   int user_projection[VIS5D_MAX_DPY_CONTEXTS]; /* -projection */
   int preload[VIS5D_MAX_DPY_CONTEXTS];         /* -quick */
   int nofile;                                  /* -nofile */
   int animrate[VIS5D_MAX_DPY_CONTEXTS];        /* -rate */
   int animdwell[VIS5D_MAX_DPY_CONTEXTS];        /*-dwell */
   char *script = NULL;                         /* -script */
   char *texture[VIS5D_MAX_DPY_CONTEXTS];       /* -texture */
   char *toponame[VIS5D_MAX_DPY_CONTEXTS];      /* -topo */
   char traju[VIS5D_MAX_DPY_CONTEXTS][20],
        trajv[VIS5D_MAX_DPY_CONTEXTS][20],
        trajw[VIS5D_MAX_DPY_CONTEXTS][20];      /* -trajvars */
   int user_vertsys[VIS5D_MAX_DPY_CONTEXTS];    /* -vertical */
   int log_vcs[VIS5D_MAX_DPY_CONTEXTS];         /* -log */
   float scale[VIS5D_MAX_DPY_CONTEXTS];
   float exponent[VIS5D_MAX_DPY_CONTEXTS];
   int verylarge[VIS5D_MAX_DPY_CONTEXTS];       /* -verylarge (-1=don't care)*/
   char *wdpy_name = NULL;                      /* -wdpy */
   float linewidth[VIS5D_MAX_DPY_CONTEXTS];     /* -wide */
   char u2[VIS5D_MAX_DPY_CONTEXTS][20],
        v2[VIS5D_MAX_DPY_CONTEXTS][20],
        w2[VIS5D_MAX_DPY_CONTEXTS][20];         /* -wind2 */

   /* MJK 12.02.98 */
   int user_data[VIS5D_MAX_DPY_CONTEXTS];       /* -userdata */
   int user_maps[VIS5D_MAX_DPY_CONTEXTS];
   int user_topo[VIS5D_MAX_DPY_CONTEXTS];


   /* MJK 12.02.98 */
   int topo_base[VIS5D_MAX_DPY_CONTEXTS];       /* -topobase */
   float topo_base_lev[VIS5D_MAX_DPY_CONTEXTS];

   int reverse_poles;

   int samescale[VIS5D_MAX_DPY_CONTEXTS];
   int legend_position[VIS5D_MAX_DPY_CONTEXTS];
   int legend_size[VIS5D_MAX_DPY_CONTEXTS];
   int legendx[VIS5D_MAX_DPY_CONTEXTS];
   int legendy[VIS5D_MAX_DPY_CONTEXTS];
/* WLH 29 Sept 98 */
   char *pipe_name;

   int dindex = 0;

   /* these variables help keep track of new variables during scripting */
   int number_of_vars_before, number_of_vars_after; 

   /* WLH 5 Oct 98 */
   setbuf(stdout,NULL);
   setbuf(stderr,NULL);

   /* MJK 11.19.98 */      
   off_screen_rendering = 0;

   /* If no arguments, print help */
   if (argc==1) {
      usage();
      exit(0);
   }

#ifndef hp
   /* disable core dumps */
   rl.rlim_cur = rl.rlim_max = 0;
   setrlimit( RLIMIT_CORE, &rl );
#endif

#ifdef sgi
   cpus = sysmp( MP_NPROCS );  /* get number of CPUs present */
#  ifdef SINGLE_TASK
   workers = 0;
   vis5d_workers( 0 );  /* for debugging */
#  else
   workers = MAX(1,cpus-1);
#  endif
#else
#ifdef LTHREADS 
   workers = 4;
#else
   /* Non-SGI systems -> no parallelism */
   workers = 0;
#endif
#endif



   for (yo=0; yo < VIS5D_MAX_DPY_CONTEXTS; yo++){
      v5dfile[yo] = NULL;
      first_area[yo] = 0;
      sequence[yo] = NULL;
      alpha[yo] = 0;
      barbs[yo] = 0;
      boxx[yo] = 0.0;
      boxy[yo] = 0.0;
      boxz[yo] = 0.0;
      fontname[yo] = NULL;
      soundfontname[yo] = NULL;
      fontsize[yo] = 0;
      /* MJK 4.27.99
      funcpath[yo] = NULL;
      */
      hirestopo[yo] = 0;

      /* MJK 12.02.98 */
      circleclock[yo] = 0;
      user_data[yo] = 0;
      user_maps[yo] = 0;
      user_topo[yo] = 0;
      julian[yo] = 1;
      mapname[yo] = NULL; 
      mbs[yo] = MBS;
      /* MJK 4.27.99
      path[yo] = NULL;
      */
      user_projection[yo] = -1;
      preload[yo] = 1;
      animrate[yo] = ANIMRATE; 
      animdwell[yo] = VIS5D_IGNORE;
      texture[yo] = NULL;
      toponame[yo] = TOPOFILE;
      user_vertsys[yo] = -1;
      log_vcs[yo] = 0;
      scale[yo] = DEFAULT_LOG_SCALE;
      exponent[yo] = DEFAULT_LOG_EXP;
      verylarge[yo] = -1;
      linewidth[yo] = 1.0;
      samescale[yo] = 0;
      legend_position[yo] = VIS5D_BOTTOM;
      legend_size[yo] = 128;
      legendx[yo] = 0;
      legendy[yo] = 0;
      /* default wind var names */
      strcpy( traju[yo], "U" );   strcpy( trajv[yo], "V" );   strcpy( trajw[yo], "W" );
      strcpy( u2[yo], "U" );      strcpy( v2[yo], "V" );      strcpy( w2[yo], "W" );
   }
   /* WLH 29 Sept 98 */
   pipe_name = NULL;
   nofile = 0;
   widescreen = 0;
   reverse_poles = 1;
/* MJK 4.27.99 */
   Vis5dFuncPath[0] = 0;
   Vis5dDataPath[0] = 0;

   /* Parse command line options */
   for (i=1;i<argc;i++) {
      if (gopointer > 0){
         filepointer = gopointer - 1;
      }
      if (strcmp(argv[i],"-area")==0 && i+1<argc) {
         first_area[filepointer] = atoi(argv[i+1]);
         i++;
      }
#if defined(SGI_GL) || defined(DENALI) || defined(OPENGL) || defined(PEX)
      else if (strcmp(argv[i],"-alpha")==0) {
         /* alpha transparency flag */
         alpha[filepointer] = 1;
      }
#endif
      else if (strcmp(argv[i],"-barbs")==0) {
         /* use wind barbs rather than vectors */
         barbs[filepointer] = 1;
      }
      else if (strcmp(argv[i],"-box")==0 && i+3<argc) {
         boxx[filepointer] = atof(argv[i+1]);
         boxy[filepointer] = atof(argv[i+2]);
         boxz[filepointer] = atof(argv[i+3]);
         i+=3;
      }
#if defined(SGI_GL) || defined(DENALI)
      else if (strcmp(argv[i],"-font")==0 && i+2<argc) {
         fontname[filepointer] = argv[i+1];
         fontsize[filepointer] = atoi( argv[i+2] );
         i+=2;
      }
#endif
#if defined(XFDI) || defined(VOGL) || defined(OPENGL)
      else if (strcmp(argv[i],"-font")==0 && i+1<argc) {
         fontname[filepointer] = argv[i+1];
         i++;
      }
      else if (strcmp(argv[i],"-soundfont")==0 && i+1<argc) {
         soundfontname[filepointer] = argv[i+1];
         i++;
      }
#endif
      /* MJK 12.10.98 */
#  ifdef PEX
      else if (strcmp(argv[i],"-font")==0 && i+2<argc) {
         fontname[filepointer] = argv[i+1];
         fontsize[filepointer] = atoi( argv[i+2] );
         i+=2;
      }
#  endif

      /* MJK 12.04.98  begin */
      else if (strcmp(argv[i],"-full")==0) {
         gfx_geom_str = full_geom_str;

         /* MJK 12.21.98 */
         /* MJK 4.14.99 
         BigWinFull = 1;
         */
         StaticWin = 1;
      }
      else if (strcmp(argv[i],"-funcpath")==0 && i+1<argc) {
         /* MJK 4.27.99
         funcpath[filepointer] = argv[i+1];
         */
         strcpy(Vis5dFuncPath, argv[i+1]);
         i++;
      }
      else if (strcmp(argv[i],"-geometry")==0 && i+1<argc) {
         /* 3-D window geometry */

         /* MJK 4.14.99 */
         StaticWin = 1;

         gfx_geom_str = argv[i+1];
         i++;
      }
      else if (strcmp(argv[i],"-cpgeom")==0 && i+1<argc) {
         /* control panel geometry */
         cp_geom_str = argv[i+1];
         i++;
      }
      /* MJK 12.04.98  end */

      else if (strcmp(argv[i],"-top_margin")==0) {
         top_margin = atoi( argv[i+1] );
         i++;
      }
      else if (strcmp(argv[i],"-bottom_margin")==0) {
         bottom_margin = atoi( argv[i+1] );
         i++;
      }
      else if (strcmp(argv[i],"-left_margin")==0) {
         left_margin = atoi( argv[i+1] );
         i++;
      }
      else if (strcmp(argv[i],"-right_margin")==0) {
         right_margin = atoi( argv[i+1] );
         i++;
      }

      else if (strcmp(argv[i],"-hirestopo")==0) {
         hirestopo[filepointer] = 1;
      }
      else if (strcmp(argv[i],"-widescreen")==0){
         widescreen = 1;
      }

      /* MJK 12.02.98 */
      else if (strcmp(argv[i],"-circle")==0){
         circleclock[filepointer] = 1;
      }

      else if (strcmp(argv[i],"-date")==0) {
         /* use non-Julian date for clock */
         julian[filepointer] = 0;
      }
      else if (strcmp(argv[i],"-map")==0 && i+1<argc) {
         mapname[filepointer] = argv[i+1];
         i++;
      }
      /* MJK 11.19.98 */      
      else if (strcmp(argv[i],"-offscreen")==0) {
         off_screen_rendering = 1;
      }
      else if (strcmp(argv[i],"-mbs")==0 && i+1<argc) {
         mbs[filepointer] = atoi( argv[i+1] );
         i++;
      }
      else if (strcmp(argv[i],"-reverse_poles")==0){
         REVERSE_POLES = -1.0;
      }
      else if (strcmp(argv[i],"-samescale")==0){
         samescale[filepointer] = 1;
      }
      else if (strcmp(argv[i],"-legend")==0 && i+2<argc) {
        int pos;
        pos = atoi( argv[i+1] );
        if (pos == 1) legend_position[filepointer] = VIS5D_BOTTOM;
        else if (pos == 2) legend_position[filepointer] = VIS5D_TOP;
        else if (pos == 3) legend_position[filepointer] = VIS5D_LEFT;
        else if (pos == 4) legend_position[filepointer] = VIS5D_RIGHT;
        legend_size[filepointer] = atoi( argv[i+2] );
        legendx[filepointer] = atoi(argv[i+3]);
        legendy[filepointer] = atoi(argv[i+4]);
        i += 4;
      }
      else if (strcmp(argv[i],"-path")==0 && i+1<argc) {
         /* MJK 4.27.99
         path[filepointer] = argv[i+1];
         */
         strcpy(Vis5dDataPath, argv[i+1]);
         i++;
      }
      /* WLH 29 Sept 98 */
      else if (strcmp(argv[i],"-pipe")==0 && i+1<argc) {
         pipe_name = argv[i+1];
         i++;
      }
      else if (strcmp(argv[i],"-projection")==0 && i+1<argc) {
         /* User-specified map projection */
         if (strncmp(argv[i+1],"gen",3)==0) {
            user_projection[filepointer] = PROJ_GENERIC;
         }
         else if (strncmp(argv[i+1],"lin",3)==0) {
            user_projection[filepointer] = PROJ_LINEAR;
         }
         else if (strncmp(argv[i+1],"lam",3)==0) {
            user_projection[filepointer] = PROJ_LAMBERT;
         }
         else if (strncmp(argv[i+1],"ste",3)==0) {
            user_projection[filepointer] = PROJ_STEREO;
         }
         else if (strncmp(argv[i+1],"rot",3)==0) {
            /* rotated equidistant projection */
            user_projection[filepointer] = PROJ_ROTATED;
         }
         else if (strncmp(argv[i+1],"sph",3)==0) {
            user_projection[filepointer] = PROJ_SPHERICAL;
         }
         else if (strncmp(argv[i+1],"cyl",3)==0) {
            /* cylindrical projection */
            user_projection[filepointer] = PROJ_CYLINDRICAL;
         }
         else {
            printf("Bad projection option: %s\n", argv[i+1] );
         }
         i++;
      }
      else if (strncmp(argv[i],"-quick",6)==0) {
         preload[filepointer] = 0;
      }
      else if (strcmp(argv[i],"-nofile")==0){
         nofile = 1;
      }
      else if (strcmp(argv[i],"-rate")==0 && i+1<argc) {
         animrate[filepointer] = atoi( argv[i+1] );
         if (animrate[filepointer]<1)
           animrate[filepointer] = 1;
         i++;
      }
      else if (strcmp(argv[i],"-dwell")==0 && i+1<argc) {
         animdwell[filepointer] = atoi( argv[i+1] );
         if (animdwell[filepointer]<1)
           animdwell[filepointer] = 1;
         i++;
      }

      else if (strcmp(argv[i],"-sequence")==0 && i+1<argc) {
         sequence[filepointer] = argv[i+1];
         i++;
      }
      else if (strcmp(argv[i],"-texture")==0 && i+1<argc) {
         texture[filepointer] = argv[i+1];
         i++;
      }
      else if (strcmp(argv[i],"-title")==0 && i+1<argc) {
         title_x_position[number_of_titles] = atoi( argv[i+1] );
         title_y_position[number_of_titles] = atoi( argv[i+2] );
         strcpy(title_font[number_of_titles], argv[i+3]);
         strcpy(title_string[number_of_titles], argv[i+4]);
         number_of_titles++;
         i += 4;
      }
      else if (strcmp(argv[i],"-topo")==0 && i+1<argc) {
         toponame[filepointer] = argv[i+1];
         i++;
      }
      else if (strcmp(argv[i],"-trajvars")==0) {
         /* wind trajectory variables */
         if (i+3<argc && argv[i+3][0]!='-') {
            strcpy( traju[filepointer], argv[i+1] );
            strcpy( trajv[filepointer], argv[i+2] );
            strcpy( trajw[filepointer], argv[i+3] );
            i += 3;
         }
         else {
            strcpy( traju[filepointer], argv[i+1] );
            strcpy( trajv[filepointer], argv[i+2] );
            i += 2;
         }
      }
      else if (strncmp(argv[i],"-vert",5)==0 && i+1<argc) {
         if (strncmp( argv[i+1], "gen", 3)==0) {
            user_vertsys[filepointer] = VERT_GENERIC;
         }
         else if (strncmp( argv[i+1], "equ", 3)==0) {
            user_vertsys[filepointer] = VERT_EQUAL_KM;
         }
         else if (strncmp( argv[i+1], "une", 3)==0) {
            user_vertsys[filepointer] = VERT_NONEQUAL_KM;
         }
         else {
            printf("Bad vertical coordinate option: %s\n", argv[i+1] );
         }
         i++;
      }
      else if (strcmp( argv[i], "-log")==0) {
         /* This is after "-vert" because it must override */
         float x, y;
         if (i+2<argc && (x=atof(argv[i+2])) && (y=atof(argv[i+1]))) {
            exponent[filepointer] = x;
            scale[filepointer] = y;
            i += 2;
         }
         else if (i+1<argc && (x=atof(argv[i+1]))) {
            scale[filepointer] = x;
            i++;
         }
         log_vcs[filepointer] = 1;
      }
      else if (strcmp(argv[i],"-script")==0) {
         script = argv[i+1];
         i++;
      }
      else if (strcmp(argv[i],"-verylarge")==0 && i+1<argc) {
        /* set flag for very large data set */
        verylarge[filepointer] = atoi(argv[i+1]);
        i++;
      }
      else if (strcmp(argv[i],"-wdpy")==0 && i+1<argc) {
         /* widget display */
         wdpy_name = argv[i+1];
         i++;
      }
      else if (strcmp(argv[i],"-wide")==0 && i+1<argc) {
         linewidth[filepointer] = atof( argv[i+1] );
         i++;
      }
      else if (strcmp(argv[i],"-wind2")==0) {
         /* secondary wind variables */
         if (i+3<argc && argv[i+3][0]!='-') {
            strcpy( u2[filepointer], argv[i+1] );
            strcpy( v2[filepointer], argv[i+2] );
            strcpy( w2[filepointer], argv[i+3] );
            i += 3;
         }
         else {
            strcpy( u2[filepointer], argv[i+1] );
            strcpy( v2[filepointer], argv[i+2] );
            i += 2;
         }
      }


      /* MJK 12.02.98 */
      else if (strcmp(argv[i],"-userdata")==0 && i+1<argc) {
         if (strchr (argv[i+1], 'g')) user_data[filepointer] = 1;
         if (strchr (argv[i+1], 'G')) user_data[filepointer] = 1;
         if (strchr (argv[i+1], 'm')) user_maps[filepointer] = 1;
         if (strchr (argv[i+1], 'M')) user_maps[filepointer] = 1;
         if (strchr (argv[i+1], 't')) user_topo[filepointer] = 1;
         if (strchr (argv[i+1], 'T')) user_topo[filepointer] = 1;
         i++;
      }

      /* MJK 12.02.98 */
      else if (strcmp(argv[i],"-topobase")==0) {
         topo_base[filepointer]     = 1;

         topo_base_lev[filepointer] = 0.0;
         if (i+1<argc)
         {
            if (sscanf (argv[i+1], "%f", &topo_base_lev[filepointer]) == 1) i++;
         }
      }


      else if (argv[i][0]!='-' ) {
         v5dfile[gopointer] = argv[i];
         gopointer++;
      }
      else {
         printf("unknown option: %s\n", argv[i] );
      }
   }

   /* MJK 11.19.98 */         
#ifdef OPENGL
   if (off_screen_rendering && script == NULL){
      off_screen_rendering = 0;
      printf(" can not do offscreen rendering with out a script to run\n");
   }
#else
   off_screen_rendering = 0;
#endif

   if (!v5dfile[0] && !nofile) {
      printf("Error: to load with no v5d use '-nofile'\n");
      exit(0);
   }

   /* Initialize the Vis5D API */
   vis5d_initialize( 0 /*NO CAVE*/);   /* Do once, independent of contexts */
   vis5d_workers( workers );

   /* WLH 6 Oct 98 */
   vis5d_noexit(nofile);

   /* Initialize context #1 */
   do_once = 1;

   /* MJK 4.27.99 */
   if (Vis5dDataPath[0] == 0 /* MJK 5.25.99 */ && getenv("VIS5D_PATH") ){
      strcpy( Vis5dDataPath, getenv("VIS5D_PATH") );
   }
   if (Vis5dDataPath[0]){
      vis5d_init_path (Vis5dDataPath);
   }

   /**************************/
   /**************************/
   /* do this if NO DATA SET */
   /**************************/
   /**************************/
   if (nofile){
      gopointer = 0;
      index = -1;
      dindex = 0;

      sprintf( windowname, "Vis5D 3-D Display");
      /* MJK 12.02.98 */
      make_gfx_window( windowname, gfx_geom_str );
      vis5d_init_begin( index, dindex );
      set_current_display( dindex );
      vis5d_init_display_values ( index, -1, dindex );
   }
   else{
      for (dindex = 0; dindex < gopointer; dindex++){
         index = vis5d_alloc_data_context();
         if (index<0) {
            printf("Error: couldn't allocate Vis5D context!\n");
            vis5d_terminate(1);
            exit(1);
         }
         if (do_once){
            if (gopointer>1){
               sprintf( windowname, "Vis5D 3-D Display");
            }
            else{
               sprintf( windowname, "Vis5D 3-D Display (%s)", v5dfile[0] );
            }

            /* MJK 12.02.98 */
            make_gfx_window( windowname, gfx_geom_str );


         }
         vis5d_init_begin( index , dindex);
    
         if (fontname[dindex]) {
            vis5d_font( dindex, fontname[dindex], fontsize[dindex]);
         }
         
         if (soundfontname[dindex]) {
            vis5d_soundfont( dindex, soundfontname[dindex]);
         }
         else{
            vis5d_soundfont( dindex, DEFAULT_SOUNDFONTNAME);
         }

         if (do_once){
            set_current_display(dindex);
            do_once = 0;
         }
         
         vis5d_init_memory( index, mbs[dindex] );

         if (samescale[dindex]) {
            vis5d_init_samescale( dindex );
         }


         /* MJK 12.02.98 */
         vis5d_set_user_data_flag (index, user_data[dindex]);
         vis5d_set_user_flags (dindex, user_topo[dindex], user_maps[dindex]);



         /* MJK 12.02.98 */
         in_the_init_stage = 1;
         vis5d_set_topo_base (dindex, topo_base[dindex], topo_base_lev[dindex]);
         in_the_init_stage = 0;
         /* MJK 4.27.99 
         if (path[dindex] == NULL) path[dindex] = getenv ("VIS5D_PATH");
         if (path[dindex] != NULL) vis5d_init_path (dindex, path[dindex]);
         */




         if (vis5d_open_gridfile( index, v5dfile[dindex], preload[dindex] )<0) {
            vis5d_terminate(1);
            exit(0);
         }
         if (first_area[dindex]){
            vis5d_init_firstarea(dindex, first_area[dindex]);
         }

         vis5d_alpha_mode( dindex, alpha[dindex] );
         if (boxx[dindex] && boxy[dindex] && boxz[dindex]) {
            vis5d_init_box( dindex,  boxx[dindex], boxy[dindex], boxz[dindex] );
         }
         vis5d_init_map( dindex, mapname[dindex] );
         /* MJK 4.27.99
         if (path[dindex]) {
            vis5d_init_path( dindex, path[dindex] );
         }
         */
         if (sequence[dindex]){
            vis5d_init_sequence( dindex, sequence[dindex] );
         }
         if (texture[dindex]) {
            vis5d_init_texture( dindex, texture[dindex] );
         }
         vis5d_init_topo_and_map_ctx( dindex, toponame[dindex], hirestopo[dindex]);

         vis5d_init_display_values ( index, -1, dindex );

         in_the_init_stage = 1;

         /* MJK 12.02.98 */
         vis5d_init_clock( dindex, circleclock[dindex]);

         if (user_projection[dindex]>-1) {
            float projargs[100];
            prompt_for_projection_args( dindex, user_projection[dindex], projargs );
            vis5d_init_projection( dindex, user_projection[dindex], projargs );
            vis5d_init_display_values ( index, -1, dindex );
         }
         else if (user_vertsys[dindex]>-1) {
            float vertargs[MAXLEVELS];
            prompt_for_vertical_args( dindex, user_vertsys[dindex], vertargs );
            vis5d_init_vertical( dindex, user_vertsys[dindex], vertargs );
            vis5d_init_display_values ( index, -1, dindex );         
         }


         /*****************/
         /* display stuff */
         /*****************/

         vis5d_linewidth( dindex, linewidth[dindex] );
         if (log_vcs[dindex]) {
            vis5d_init_log( dindex, 1, scale[dindex], exponent[dindex] );
         }
         else{
            vis5d_init_log( dindex, 0, scale[dindex], exponent[dindex] );
         }

         if (vis5d_init_data_end( index )<0) {
            printf("Error in vis5d_init_data_end\n");
            vis5d_terminate(1);
            exit(0);
         }

         /* setup trajectory and wind variables */
         vis5d_set_wind_vars( dindex, index, vis5d_find_var(index,"U"),
                                     index, vis5d_find_var(index,"V"),
                                     index, vis5d_find_var(index,"W"),
                                     index, vis5d_find_var(index,u2[dindex]),
                                     index, vis5d_find_var(index,v2[dindex]),
                                     index, vis5d_find_var(index,w2[dindex]),
                                     index, vis5d_find_var(index,traju[dindex]),
                                     index, vis5d_find_var(index,trajv[dindex]),
                                     index, vis5d_find_var(index,trajw[dindex]) );

         vis5d_set_sound_vars( index, index, vis5d_find_var(index,"T"),
                                      index, vis5d_find_var(index,"TD"),
                                      index, vis5d_find_var(index,"U"),
                                      index, vis5d_find_var(index,"V"),
                                      index, -1, 0, -1, 0, -1 );

         if (verylarge[dindex] == 1) {
            vis5d_verylarge_mode(index, VIS5D_ON);
         }
         else if (verylarge[dindex] == 0) {
            vis5d_verylarge_mode(index, VIS5D_OFF);
         }

         if (barbs[dindex] == 1) vis5d_graphics_mode(dindex, VIS5D_BARBS, VIS5D_ON);
         else vis5d_graphics_mode(dindex, VIS5D_BARBS, VIS5D_OFF);

         if (julian[dindex] == 1) vis5d_graphics_mode(dindex, VIS5D_JULIAN, VIS5D_ON);
         else vis5d_graphics_mode(dindex, VIS5D_JULIAN, VIS5D_OFF);

         vis5d_check_dtx_volume( dindex, &volflag );

         vis5d_set_legends(dindex, legend_position[dindex], legend_size[dindex],
                           legendx[dindex], legendy[dindex]);

         /* MJK 4.5.99 */
         /*
         if (funcpath[dindex]) {
            strcpy(gtx->funcpath, funcpath[dindex]);
         }
         */
         in_the_init_stage = 0;
      }
   }


   switch(gopointer){
      case 0:
         set_display_matrix( 1, 1);
         break;
      case 1:
         set_display_matrix( 1, 1);
         break;
      case 2:
         set_display_matrix( 1, 2);
         break;
      case 3:
         set_display_matrix( 2, 2);
         break;
      case 4:
         set_display_matrix( 2, 2);
         break;
      case 5:
         set_display_matrix( 2, 3);
         break;
      case 6:
         set_display_matrix( 2, 3);
         break;
      case 7:
         set_display_matrix( 3, 3);
         break;
      case 8:
         set_display_matrix( 3, 3);
         break;
      case 9:
         set_display_matrix( 3, 3);
         break;
      case 10:
         set_display_matrix( 3, 4);
         break;
      case 11:
         set_display_matrix( 3, 4);
         break;
      case 12:
         set_display_matrix( 3, 4);
         break;
      case 13:
         set_display_matrix( 4, 4);
         break;
      case 14:
         set_display_matrix( 4, 4);
         break;
      case 15:
         set_display_matrix( 4, 4);
         break;
      case 16:
         set_display_matrix( 4, 4);
         break;
      default:
      printf("somethings wrong in main.c with the filepointer\n");
   }
   if (widescreen){
      set_display_matrix(1,3);
   }

   get_display_matrix( &rows, &cols); 
   gtx = create_gui_context(0);
   /* MJK 11.19.98 */      
   if (off_screen_rendering){
      run_script( 0, script );
      exit(0);
   }

   if (wdpy_name != NULL){
      make_gui_BigWin( wdpy_name);
      strcpy( the_gui_dpy_name, wdpy_name);
   }
   else{
      the_gui_dpy_name[0] = 0;
      GuiBigWin = 0;
   }
   gtx->AnimRate = animrate[0];
   gtx->AnimDwell = animdwell[0];
   gtx->SoundCtrlWindow = 0;
   if (wdpy_name != NULL ){
      gtx->othersnddpy = 1;
   }
   else {
      gtx->othersnddpy = 0;
   }

   if (nofile){
      make_nodata_gui( 0, wdpy_name, cp_geom_str);
   }
   else{


      /* MJK 11.19.98 */
      make_gui( 0, v5dfile[0], wdpy_name, cp_geom_str, volflag );
      LUI_MoveResizeWindow( gtx->CpWindow, gtx->cpx, gtx->cpy, CP_WIDTH, gtx->CpHeight );
      XMapWindow(GuiDpy, gtx->CpWindow);


   }

   XMapWindow(GuiDpy, gtx->CpWindow);
   LUI_MoveResizeWindow( gtx->CpWindow, gtx->cpx, gtx->cpy, CP_WIDTH, gtx->CpHeight );

   if (!nofile){
      sprintf( swindowname," Skew-T and Vertical Plot Display (%s)", v5dfile[0] );
      make_snd_window( 0, swindowname, gtx->SoundCtrlWindow, wdpy_name );
      if (gtx->othersnddpy ){
         vis5d_resize_sounding_window( gtx->context_index, 630, 600,
                                       gtx->cpx, 15);
      } 
   }

   /* MJK 4.5.99 */
/* MJK 4.27.99
   if (funcpath[0]){
      strcpy(gtx->funcpath, funcpath[0]);
   }
*/


   for (yo = 1; yo < rows*cols; yo++){
      vis5d_create_display_context(yo);
      make_another_gui(yo, 1);
      if (yo < gopointer){
         set_animate( yo, VIS5D_IGNORE, animrate[yo], animdwell[yo]);
      }


      /* MJK 4.5.99 */   
/* MJK 4.27.99
      if (funcpath[yo]){
         gtx = get_gui_gtx(yo);
         strcpy(gtx->funcpath, funcpath[yo]);
      }     
*/


   }
   map_all_windows(0);
   map_fake_windows(0);

   /* execute script specified by -script command line option */
   if (script) {
      run_script( 0, script );
   }

   main_loop(pipe_name);

   vis5d_terminate(1);
   return 0;
}


