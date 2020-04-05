/* sounding.c */

/* Vis5D version 5.2 */
/*ha*/
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
#include "vis5d.h"

/* MJK 12.15.98 */
#  include      "topo.h"
#  include      "v5d.h"
#  define SOUND_BARB_SIZE               48
#  define PI                            3.14159265


#define BORDER 65 

#define HEBGBS 0  

#define TICK_DASH_LENGTH 2 

#define PF_TRUECOLOR 0

#define PF_XALLOC    1

#define PF_8BIT      2





Status SND_XAllocColor( Display *dpy, Colormap cmap, int cmap_size,
                        XColor *color );

void SND_Initialize( Display_Context dtx, Display *display,
                     Visual *visual, int depth, Colormap colormap );

GC make_gc(Display_Context dtx, int foregroundR, int foregroundG, int foregroundB,
                  int backgroundR, int backgroundG, int backgroundB, int linewidth);

static void make_soundpixmap( Display_Context dtx);

static void draw_var_stuff( Display_Context dtx, int var, Context varctx);

static void draw_ticks( Display_Context dtx, int var, Context varctx);

static float pres_to_height(float pressure);

static float height_to_pres(float height);

static void draw_box ( Display_Context dtx );

static float svp( float K);

static float mixratio( float K, float pres );

static float thetaE( float pres, float temp);
  
static float get_temp_for_thte(float thte, float pres);

static void draw_wlines( Display_Context dtx);

static void draw_thtelines( Display_Context dtx );

static void draw_thtalines( Display_Context dtx );

static void cut_line_data2( Display_Context dtx, int *x1, int *y1, int *x2, int *y2);

static void precut_line_data (Display_Context dtx, int *x1, int *y1, int x2, int y2);

static void cut_line_data( Display_Context dtx, int x1, int y1, int *x2, int *y2);

static void draw_vert_stuff( Display_Context dtx );

static void draw_millibarlines ( Display_Context dtx );


/* MJK 12.15.98 */
static void draw_templines( Display_Context dtx );
static void make_a_barb( Display_Context dtx, float spd, float dir,
                         float height, int barb_size);




static void drawbarbflag( Display_Context dtx, XPoint flagpoints[], int up, float dir);

static void drawbarbline( Display_Context dtx, int x1, int y1, int x2, int y2,
                           int up, float dir);

static void convert_xy_to_barb(Display_Context dtx, int inx, int iny, float dir,
                                          int *outx, int *outy);

void vardata_to_xy(Display_Context dtx, float alt, float value, float min, float max, int *x, int *y);

void setvarsteps( Display_Context dtx);

void data_to_xy(Display_Context dtx, float alt, float temp, int *x, int *y);

void data_to_y (Display_Context dtx, float alt, int *y);

float grid_level_to_height( Display_Context dtx, float level );

static int extract_sound( Display_Context dtx, float *grid, int var,
                             int nr, int nc, int nl, int lowlev,
                             float row, float col);

static int extract_soundPRIME( Context ctx, int var,
                             int nr, int nc, int nl, int lowlev,
                             float row, float col);

static int extract_wind( Display_Context dtx, float *gridu, float *gridv,
                             int varu, int varv,
                             int nr, int nc, int nl, int lowlev,
                             float row, float col);

static int extract_windPRIME( Context ctx, 
                             int varu, int varv,
                             int nr, int nc, int nl, int lowlev,
                             float row, float col);

static float winterval[34] = { 0.1, 0.2, 0.4, 0.6, 0.8, 1.0, 1.5,
                               2.0, 2.5, 3.0, 4.0, 5.0, 6.0, 7.0, 
                               8.0, 9.0, 10.0, 12.0, 14.0, 16.0, 18.0,
                               20.0, 24.0, 28.0, 32.0, 36.0, 40.0, 44.0,
                               48.0, 52.0, 56.0, 60.0, 68.0, 76.0 };

static int pixelformat;

static unsigned long ctable8[5][9][5];   /* Only for PF_8BIT */

static unsigned long rtable[256], gtable[256], btable[256];  /* PF_TRUECOLOR */




    /**********************************************************************/
    /* This just resizes the window to the given width and height         */
    /**********************************************************************/
    /* Input: dtx- display context index                                  */
    /*        width- the width of the resized window                      */
    /*        height- the height of the resized window                    */
    /**********************************************************************/


void resize_snd_window(Display_Context dtx, int width, int height, int x, int y)
{
   if (!dtx->Sound.soundwin){
      return;
   }
   dtx->Sound.soundwin_width = width;
   dtx->Sound.soundwin_height = height;
   if (x==0 && y == 0 ){
      x = dtx->Sound.sndx;
      y = dtx->Sound.sndy;
   }
   else {
      dtx->Sound.sndx = x;
      dtx->Sound.sndy = y;
   }
   XMoveResizeWindow( SndDpy, dtx->Sound.soundwin, x, y, width, height);
   if (dtx->Sound.SoundCtrlWindow != 0){
      if (dtx->Sound.otherdpy){
         dtx->Sound.sndheight = height -2*BORDER;
      }
      else {
         dtx->Sound.sndheight = height -95 -2*BORDER;
      }
      dtx->Sound.sndwidth =  width - 2 * BORDER;
      do_pixmap_art ( dtx );
      draw_sounding(dtx, dtx->CurTime);
   }
   else {
      dtx->Sound.sndheight = height - 2 * BORDER;
      dtx->Sound.sndwidth =  width - 2 * BORDER;
      do_pixmap_art ( dtx );
      draw_sounding(dtx, dtx->CurTime);
   }
}   




/*
 * Given an RGB color, return the corresponding pixel value.
 * Input;  r, g, b - red, green, and blue in [0,255]
 * Return:  a pixel value
 *
 ***** This is the same code found in lui.c lines 236 - 263 ****
 */
unsigned long SND_AllocateColorInt( int r, int g, int b )
{
   XColor xcol;

   switch (pixelformat) {
      case PF_TRUECOLOR:
         return rtable[r] | gtable[g] | btable[b];
      case PF_8BIT:
         return ctable8[r/52][g/31][b/52];
      case PF_XALLOC:
         xcol.red = r << 8;
         xcol.green = g << 8;
         xcol.blue = b << 8;
         SND_XAllocColor( SndDpy, SndColormap, SndVisual->map_entries,
                          &xcol );
         return xcol.pixel;
      default:
         printf("Error in SND_AllocateColorInt %d\n", pixelformat);
         exit(0);
   }
   return 0;
}


/*
 * A replacement for XAllocColor.  This function should never fail
 * to allocate a color.  When XAllocColor fails we return the nearest
 * matching color.
 *
 ***** This is the same code found in lui.c lines 181 - 232 **** 
 */
Status SND_XAllocColor( Display *dpy, Colormap cmap, int cmap_size,
                        XColor *color )
{
   int p, bestmatch;
   double dist, mindist;  /* 3*2^16^2 exceeds long int precision */
   static XColor *allcolors = NULL;
   XColor *acptr;

#define DISTANCE(r1,g1,b1,r2,g2,b2)  ( ((r2)-(r1)) * ((r2)-(r1)) + \
    ((g2)-(g1)) * ((g2)-(g1)) + ((b2)-(b1)) * ((b2)-(b1)) )

   if (!XAllocColor(dpy, cmap, color)) {
      /* query whole colormap if not yet done */
      if (!allcolors) {
         allcolors = (XColor *) malloc( cmap_size * sizeof(XColor) );
         for (p = 0;  p < cmap_size;  p++)
           allcolors[p].pixel = p;
         XQueryColors (dpy, cmap, allcolors, cmap_size);
      }

      /* find best match */
      bestmatch = -1;
      mindist = 0.0;
      p = cmap_size;
      while (p--) {
         acptr = allcolors + p;
         dist = DISTANCE( (double)color->red, (double)color->green,
                          (double)color->blue, (double)acptr->red,
                          (double)acptr->green, (double)acptr->blue);
         if (bestmatch < 0 || dist < mindist)
           mindist = dist, bestmatch = p;
      }
      color->red   = allcolors[bestmatch].red;
      color->green = allcolors[bestmatch].green;
      color->blue  = allcolors[bestmatch].blue;
      if (!XAllocColor( dpy, cmap, color )) {
         /* this is a real hack but should be good enough */
         color->pixel = bestmatch;
      }
   }
#undef DISTANCE

   return 1;
}


/*
 * Input:  program_name - the label for all window title bars
 *         display - the X display (or NULL)
 *         visual - the X visual (or NULL)
 *         depth - the depth of the visual (or 0)
 *         colormap - the X colormap (or 0)
 */
void SND_Initialize(  Display_Context dtx, Display *display,
                     Visual *visual, int depth, Colormap colormap )
{
   static int initialized = 0;
   XSetWindowAttributes attr;
   XVisualInfo visinfo;
   int yo;

   /* Only do this once */
   if (initialized)
      return;
   else
      initialized = 1;

 
   /* Open the display if one wasn't passed (already opened) */
   if (display) {
      SndDpy  = display;
   }
   else {
      SndDpy = XOpenDisplay(NULL);
      if (!SndDpy) {
         printf("Can't open sound display");
      }
   }

   /* Set defaults */
   SndRootWindow = DefaultRootWindow (SndDpy);
   SndScr        = DefaultScreen ( SndDpy );
   SndScrWidth   = DisplayWidth (SndDpy, SndScr );
   SndScrHeight  = DisplayHeight( SndDpy, SndScr );


   if (visual) {
      SndVisual = visual;
      SndDepth = depth;
      SndColormap = colormap;
   }
   else {
      if (XMatchVisualInfo( SndDpy,SndScr,24,TrueColor,&visinfo )) {
         SndVisual = visinfo.visual;
         SndDepth = 24;
         SndColormap = XCreateColormap( SndDpy, RootWindow(SndDpy, SndScr),
                                         SndVisual, AllocNone );
      }
      else {
         SndVisual = DefaultVisual( SndDpy, SndScr );
         SndDepth = DefaultDepth( SndDpy, SndScr );
         SndColormap = DefaultColormap( SndDpy, SndScr );
      }
   }

   /* Setup color allocation stuff */
   if (SndVisual->class==TrueColor || SndVisual->class==DirectColor) {
      /* Initialize rtable[], gtable[], and btable[] */
      XColor xcol;
      int i;
      xcol.green = 0;
      xcol.blue = 0;
      for (i=0;i<256;i++) {
         xcol.red = i * 0xffff / 0xff;
         XAllocColor( SndDpy, SndColormap, &xcol );
         rtable[i] = xcol.pixel;
      }
      xcol.red = 0;
      xcol.blue = 0;
      for (i=0;i<256;i++) {
         xcol.green = i * 0xffff / 0xff;
         XAllocColor( SndDpy, SndColormap, &xcol );
         gtable[i] = xcol.pixel;
      }
     xcol.red = 0;
      xcol.green = 0;
      for (i=0;i<256;i++) {
         xcol.blue = i * 0xffff / 0xff;
         XAllocColor( SndDpy, SndColormap, &xcol );
         btable[i] = xcol.pixel;
      }
      pixelformat = PF_TRUECOLOR;
   }
   else if (SndVisual->class==PseudoColor) {
      /* Note: the color allocation scheme must be the same as what's used */
      /* in Mesa to allow colormap sharing! */
      int r, g, b;
      for (r=0;r<5;r++) {
         for (g=0;g<9;g++) {
            for (b=0;b<5;b++) {
               XColor xcol;
               xcol.red   = r * 65535 / 4;
               xcol.green = g * 65535 / 8;
               xcol.blue  = b * 65535 / 4;
               SND_XAllocColor( SndDpy, SndColormap,
                                SndVisual->map_entries, &xcol );
               ctable8[r][g][b] = xcol.pixel;
            }
         }
      }
      pixelformat = PF_8BIT;
   }
   else {
      pixelformat = PF_XALLOC;
   }
}




    /**********************************************************************/
    /* This makes a graphics context                                      */
    /**********************************************************************/
    /* Input: dtx - context                                               */
    /*        foregroundR - the red value 1..255 of the foreground        */
    /*        foregroundG - the green value 1..255 of the foreground      */
    /*        foregroundB - the blue value 1..255 of the foreground       */
    /*        backgroundR - the red value 1..255 fo the background        */
    /*        backgroundG - the green value 1..255 of the background      */
    /*        backgroundB - the blue value 1..255 of the background       */
    /*        linewidth - the line width value for the GC                 */
    /*                                                                    */
    /* Output: make_gc- the GC with the above specifications              */
    /**********************************************************************/

GC make_gc(Display_Context dtx, int foregroundR, int foregroundG, int foregroundB,
                  int backgroundR, int backgroundG, int backgroundB, int linewidth)
{
   XGCValues vals;
   
   vals.foreground = SND_AllocateColorInt(foregroundR,
                                   foregroundG, foregroundB);
   vals.background = SND_AllocateColorInt(backgroundR,
                                   backgroundG, backgroundB);
   vals.line_width = linewidth;
   return XCreateGC ( SndDpy, dtx->Sound.soundwin, GCLineWidth |
                      GCForeground | GCBackground, &vals );
}




    /**********************************************************************/
    /* This makes the window that all of the sounding graphics is         */
    /* drawn or mapped to.  The sndheight and sndwidth are the dimentions */
    /* inside this window to which the actual data is drawn to.           */
    /**********************************************************************/
    /* Input: dtx - context                                               */
    /*        title - title for window if there is no parent              */
    /*        xpos, ypos - the x and y position of the window if no parent*/
    /*        width, height - of the window                               */
    /* Output: 1 if it works                                              */
    /**********************************************************************/
    
int make_soundGFX_window( Display_Context dtx, char *title, int xpos, int ypos,
                           int width, int height, Window ctrlwindow, char *wdpy_name)
{
   XSetWindowAttributes attr;
   int attr_flags;
   XWindowAttributes winatts;
   int vertical;
   float vjunk[MAXLEVELS];
   int yo;
 


   dtx->Sound.SoundCtrlWindow = ctrlwindow;
   dtx->Sound.get_vert_data = 1;
   vis5d_get_dtx_vertical(dtx->dpy_context_index, &vertical, vjunk);
   dtx->Sound.vertsys = vertical;
   if( dtx->TopBound < 1.0 && dtx->BottomBound < -1.0 ) {
      dtx->Sound.oceanonly = 1;
   }
   else {
      dtx->Sound.oceanonly = 0;
   }
   SND_Initialize( dtx, SndDpy, SndVisual, SndDepth, SndColormap);
   /* now just set some variables for the first time */
   yo = dtx->ctxarray[0];
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

   dtx->Sound.PreviousSoundTemp = vis5d_find_var(dtx->ctxarray[0],"T");
   dtx->Sound.PreviousSoundDewpt= vis5d_find_var(dtx->ctxarray[0],"TD");
   dtx->Sound.PreviousSoundUWind= vis5d_find_var(dtx->ctxarray[0],"U");
   dtx->Sound.PreviousSoundVWind= vis5d_find_var(dtx->ctxarray[0],"V");
   dtx->Sound.PreviousSoundVar1 = -1;
   dtx->Sound.PreviousSoundVar2 = -1;
   dtx->Sound.PreviousSoundVar3 = -1;
   dtx->Sound.sndx = 15;
   dtx->Sound.sndy = 15;
   vis5d_set_sound_vars( dtx->dpy_context_index, yo, vis5d_find_var(dtx->ctxarray[0],"T"),
                                yo, vis5d_find_var(dtx->ctxarray[0],"TD"),
                                yo, vis5d_find_var(dtx->ctxarray[0],"U"),
                                yo, vis5d_find_var(dtx->ctxarray[0],"V"),
                                yo, -1, yo, -1, yo, -1 );

   attr.event_mask = ExposureMask | ButtonMotionMask | KeyReleaseMask
                           | KeyPressMask | ButtonPressMask | ButtonReleaseMask
                        | StructureNotifyMask | VisibilityChangeMask;
 
   attr.colormap = SndColormap;
   attr.background_pixel = BlackPixel( SndDpy, SndScr );
   attr.border_pixel =BlackPixel( SndDpy, SndScr ); 
   attr_flags = CWColormap | CWEventMask | CWBackPixel | CWBorderPixel;
   if (wdpy_name != NULL) {
      dtx->Sound.otherdpy = 1;
   }
   if (dtx->Sound.soundwin){
      XDestroyWindow(SndDpy, dtx->Sound.soundwin);
   }
   if ((dtx->Sound.SoundCtrlWindow != 0) && (wdpy_name == NULL)) {
      XGetWindowAttributes( SndDpy, dtx->Sound.SoundCtrlWindow, &winatts);

      dtx->Sound.soundwin = XCreateWindow(SndDpy, dtx->Sound.SoundCtrlWindow,
                       0, 95, winatts.width, winatts.height-95+HEBGBS,
                       1, SndDepth, InputOutput,
                       SndVisual, attr_flags, &attr);
      dtx->Sound.soundwin_width = winatts.width;
      dtx->Sound.soundwin_height= winatts.height-95+HEBGBS;
      dtx->Sound.sndheight = winatts.height-95-2*BORDER;
      dtx->Sound.sndwidth = winatts.width-2*BORDER;
   }
   else {
      XSizeHints sizehints;

      dtx->Sound.soundwin = XCreateWindow(SndDpy, RootWindow(SndDpy, SndScr),
                       xpos, ypos, width, height, 1, SndDepth, InputOutput,
                       SndVisual, attr_flags, &attr);
      dtx->Sound.soundwin_width = width;
      dtx->Sound.soundwin_height= height ;
      dtx->Sound.sndheight = height -2*BORDER;
      dtx->Sound.sndwidth  = width - 2*BORDER;
      sizehints.x = 20;
      sizehints.y = 40;
      sizehints.width = 200;
      sizehints.height = 200;
      sizehints.flags = PPosition | PSize; 

      XSetStandardProperties(SndDpy, dtx->Sound.soundwin,
                             "Skew-T and Vertical Plot Display",
                             "Skew-T and Vertical Plot Display",
                             None, (char**)NULL, 0, &sizehints);

   }
   /* MJK 12.15.98 */
   dtx->Sound.vert_gc  = make_gc(dtx, 255, 255, 255, 0, 0, 0, 2);
   dtx->Sound.Tempgc   = make_gc(dtx, 255, 0, 0, 0, 0, 0, 2);
   dtx->Sound.Dewptgc  = make_gc(dtx, 0, 255, 0, 0, 0, 0, 2);
   dtx->Sound.barb_gc  = make_gc(dtx, 0, 255, 255, 0, 0, 0, 2);
   dtx->Sound.barb2_gc = make_gc(dtx, 255, 255, 255, 0, 0, 0, 1);
   dtx->Sound.var1_gc  = make_gc(dtx, 255, 255, 0, 0, 0, 0, 2);
   dtx->Sound.var2_gc  = make_gc(dtx, 255, 0, 255, 0, 0, 0, 2);
   dtx->Sound.var3_gc  = make_gc(dtx, 255, 255, 255, 0, 0, 0, 2);
   dtx->Sound.rect_gc  = make_gc(dtx, 0, 0, 0, 1, 1, 1, 1);
   dtx->Sound.box_gc   = make_gc(dtx, 160, 160, 160, 0, 0, 0, 1);
   yo = height_to_pressure (dtx->BottomBound) + 0.5;
   /* if (yo % 50) yo += 50; WLH 7 June 2000 */
   if (yo < 50) yo += 50;
   dtx->Sound.BotPress = (yo / 50) * 50;
   yo = height_to_pressure (dtx->TopBound);
   if (yo < 50) yo += 50;
   dtx->Sound.TopPress = (yo / 50) * 50;
   dtx->Sound.TopHgt   = pressure_to_height ((float) dtx->Sound.TopPress);
   dtx->Sound.BotHgt   = pressure_to_height ((float) dtx->Sound.BotPress);
   dtx->Sound.DiffHgt  = dtx->Sound.TopHgt - dtx->Sound.BotHgt;
/*

   dtx->Sound.vert_gc = make_gc(dtx, 255, 255, 255, 0, 0, 0, 2);
   dtx->Sound.Tempgc = make_gc(dtx, 255, 255, 255, 0, 0, 0, 2);
   dtx->Sound.Dewptgc= make_gc(dtx, 255, 0, 255, 0, 0, 0, 2);
   dtx->Sound.barb_gc = make_gc(dtx, 0, 0, 255, 0, 0, 0, 2);
   dtx->Sound.barb2_gc= make_gc(dtx, 255,255,255, 0, 0, 0, 1);
   dtx->Sound.var1_gc = make_gc(dtx, 0, 255, 0, 0, 0, 0, 2);
   dtx->Sound.var2_gc = make_gc(dtx, 100, 255, 255, 0, 0, 0, 2);
   dtx->Sound.var3_gc = make_gc(dtx, 255, 255, 100, 0, 0, 0, 2);
   dtx->Sound.rect_gc =  make_gc(dtx, 0, 0, 0, 1, 1, 1, 1);
   dtx->Sound.box_gc =  make_gc(dtx, 169, 169, 169, 0, 0, 0, 1);
*/

   if  (XLoadFont(SndDpy, dtx->SoundFontName)){
      XSetFont(SndDpy, dtx->Sound.var1_gc, XLoadFont(SndDpy, dtx->SoundFontName));
      XSetFont(SndDpy, dtx->Sound.var2_gc, XLoadFont(SndDpy, dtx->SoundFontName));
      XSetFont(SndDpy, dtx->Sound.var3_gc, XLoadFont(SndDpy, dtx->SoundFontName));
   }
   do_pixmap_art(dtx);

   /* MJK 12.15.98 */
    if (dtx->Sound.SoundCtrlWindow != 0){
      XMapWindow(SndDpy, dtx->Sound.soundwin);
   }

   return 1;
}

    /**********************************************************************/
    /* This makes the pixmap to which all the extraneous graphics         */
    /* such as the theta lines, thtate lines, millibar lines, outside box */
    /* are drawn to.                                                      */
    /**********************************************************************/
    /* Input - context                                                    */
    /**********************************************************************/

static void make_soundpixmap( Display_Context dtx)
{
   if (dtx->Sound.soundpix){
      XFreePixmap(SndDpy, dtx->Sound.soundpix);
   } 
   if( dtx->Sound.SoundCtrlWindow ) {
      dtx->Sound.soundpix = XCreatePixmap( SndDpy, dtx->Sound.soundwin,
                                          dtx->Sound.sndwidth+2*BORDER,
                                          dtx->Sound.sndheight+95+2*BORDER,
                                          SndDepth);
      XFillRectangle(SndDpy, dtx->Sound.soundpix, dtx->Sound.rect_gc, 0, 0,
                     dtx->Sound.sndwidth+2*BORDER,
                     dtx->Sound.sndheight + 95 +2*BORDER);
   }
   else {
      dtx->Sound.soundpix = XCreatePixmap( SndDpy, dtx->Sound.soundwin,
                                          dtx->Sound.sndwidth+2*BORDER,
                                          dtx->Sound.sndheight+2*BORDER,
                                          SndDepth);
      XFillRectangle(SndDpy, dtx->Sound.soundpix, dtx->Sound.rect_gc, 0, 0,
                     dtx->Sound.sndwidth+2*BORDER,
                     dtx->Sound.sndheight+2*BORDER);
   }
}

    /**********************************************************************/
    /* This draws all the extraneous gphics to a pixmap if they are wanted*/
    /**********************************************************************/
    /* Input - context                                                    */
    /**********************************************************************/

void do_pixmap_art( Display_Context dtx )
{ 

   setvarsteps( dtx);

   make_soundpixmap( dtx);
   if ( dtx->Sound.wstatus ){
      draw_wlines(dtx);
   }
   if ( dtx->Sound.thtestatus ){
      draw_thtelines(dtx);
   }
   if ( dtx->Sound.thtastatus ){
      draw_thtalines(dtx);
   }

   /* MJK 12.15.98 */
   if ( dtx->Sound.tempstatus ){
      draw_templines(dtx);
   }


   if (dtx->Sound.SoundVar1 >= 0){
      draw_var_stuff(dtx, dtx->Sound.SoundVar1, dtx->Sound.SoundVar1Owner);
      if(dtx->Sound.tickstatus) draw_ticks(dtx, dtx->Sound.SoundVar1,
                                           dtx->Sound.SoundVar1Owner);
   }
   if (dtx->Sound.SoundVar2 >= 0){
      draw_var_stuff(dtx, dtx->Sound.SoundVar2, dtx->Sound.SoundVar2Owner);
      if(dtx->Sound.tickstatus) draw_ticks(dtx, dtx->Sound.SoundVar2,
                                           dtx->Sound.SoundVar2Owner);
   }
   if (dtx->Sound.SoundVar3 >= 0){
      draw_var_stuff(dtx, dtx->Sound.SoundVar3, dtx->Sound.SoundVar3Owner);
      if(dtx->Sound.tickstatus) draw_ticks(dtx, dtx->Sound.SoundVar3,
                                           dtx->Sound.SoundVar3Owner);
   }
   if ((dtx->Sound.vertsys != 0) && (dtx->Sound.oceanonly != 1))  draw_millibarlines(dtx);
   draw_box(dtx);
   if (!dtx->Sound.get_vert_data)  draw_vert_stuff(dtx);
}


    /**********************************************************************/
    /* This draws the numbers and unit for each variable alond the bottom */
    /**********************************************************************/
    /* Input: dtx - context                                               */
    /*        var - the variable whose numbers and unit should be drawn   */
    /**********************************************************************/

static void draw_var_stuff( Display_Context dtx, int var, Context varctx)
{
   int counter= -1;
   float yo, step;
   GC var_gc;
   char num[9];
   int vary = 25;
   int stringnumber;
   char varunit[10] = "";
   int strlength = 0;

   if (var == dtx->Sound.SoundVar1 &&
       varctx == dtx->Sound.SoundVar1Owner){
      var_gc = dtx->Sound.var1_gc;
      vary += 10;
      step = dtx->Sound.var1step;
      strlength = strlen(dtx->Sound.SoundVar1Owner->Units[var]);
   }
   if (var == dtx->Sound.SoundVar2 &&
       varctx == dtx->Sound.SoundVar2Owner){
      var_gc = dtx->Sound.var2_gc;
      vary += 22;
      step = dtx->Sound.var2step;
      strlength = strlen(dtx->Sound.SoundVar2Owner->Units[var]);
   }
   if (var == dtx->Sound.SoundVar3 &&
       varctx == dtx->Sound.SoundVar3Owner){
      var_gc = dtx->Sound.var3_gc;
      vary += 34;
      step = dtx->Sound.var3step;
      strlength = strlen(dtx->Sound.SoundVar3Owner->Units[var]);
   }
   if (dtx->Sound.samestepflag){
      for ( yo = dtx->Sound.samestepmin; yo <= dtx->Sound.samestepmax; yo += step){
         counter ++;
         sprintf(num, "%.1f\n", yo );
         stringnumber = strlen(num)-1;
         if ( (dtx->Sound.mainvarstep * counter)+BORDER < dtx->Sound.sndwidth+BORDER){
            XDrawString( SndDpy, dtx->Sound.soundpix, var_gc,
                         (dtx->Sound.mainvarstep * counter+1)+BORDER-15,
                          dtx->Sound.sndheight+BORDER-HEBGBS+vary,
                          num, stringnumber);
         }
         if(stringnumber > 7){
            yo += step;
            counter ++;
         }
      }
   }
   else{
      for ( yo = varctx->MinVal[var]; yo <= varctx->MaxVal[var]; yo += step){
         counter ++;
         sprintf(num, "%.1f\n", yo );
         stringnumber = strlen(num)-1;
         if ( (dtx->Sound.mainvarstep * counter)+BORDER < dtx->Sound.sndwidth+BORDER){
            XDrawString( SndDpy, dtx->Sound.soundpix, var_gc,
                         (dtx->Sound.mainvarstep * counter+1)+BORDER-15,
                          dtx->Sound.sndheight+BORDER-HEBGBS+vary,
                          num, stringnumber);
         }
         if(stringnumber > 7){
            yo += step;
            counter ++;
         }
      }    
   }
   XDrawString( SndDpy, dtx->Sound.soundpix, var_gc,             
                BORDER - 45, dtx->Sound.sndheight + BORDER-HEBGBS+vary,
                varctx->Units[var], strlength);
}

   /*****************************************************************/
   /* This draws the vertical ticks for the different vars          */
   /*****************************************************************/
   /* Input: dtx- context                                           */
   /*        var - the variable for which the vertical ticks should */
   /*              be drawn                                         */
   /*****************************************************************/

 
static void draw_ticks( Display_Context dtx, int var, Context varctx)
{
   float step, yo;
   int dash_offset;
   int counter;
   int dash_list_length = { TICK_DASH_LENGTH };
   static  char dotted[2] = {4,12};
   char *dash_list[] = {dotted};
   GC var_gc;

   XSetLineAttributes(SndDpy, dtx->Sound.var1_gc, 1, LineOnOffDash, CapButt, JoinRound);
   XSetLineAttributes(SndDpy, dtx->Sound.var2_gc, 1, LineOnOffDash, CapButt, JoinRound);
   XSetLineAttributes(SndDpy, dtx->Sound.var3_gc, 1, LineOnOffDash, CapButt, JoinRound);
   XSetDashes(SndDpy, dtx->Sound.var1_gc, 1, dash_list[0], 2);
   XSetDashes(SndDpy, dtx->Sound.var2_gc, 5, dash_list[0], 2);
   XSetDashes(SndDpy, dtx->Sound.var3_gc, 9, dash_list[0], 2);
   counter = -1;
   if (var == dtx->Sound.SoundVar1 &&
       varctx == dtx->Sound.SoundVar1Owner){
       var_gc = dtx->Sound.var1_gc;
       step = dtx->Sound.var1step;
   }
   if (var == dtx->Sound.SoundVar2 &&
       varctx == dtx->Sound.SoundVar2Owner){
      var_gc = dtx->Sound.var2_gc;
      step = dtx->Sound.var2step;
   }
   if (var == dtx->Sound.SoundVar3 &&
       varctx == dtx->Sound.SoundVar3Owner){
      var_gc = dtx->Sound.var3_gc;
      step = dtx->Sound.var3step;
   } 
   if (dtx->Sound.samestepflag){
      for ( yo = varctx->MinVal[var]; yo < varctx->MaxVal[var]; yo += step){
         counter ++;
         if ( (dtx->Sound.mainvarstep * counter)+BORDER < dtx->Sound.sndwidth+BORDER){
            XDrawLine( SndDpy, dtx->Sound.soundpix, var_gc,
                    (dtx->Sound.mainvarstep * counter)+BORDER, dtx->Sound.sndheight+BORDER-HEBGBS,
                    (dtx->Sound.mainvarstep * counter)+BORDER, BORDER-HEBGBS);
         }
      }
   }
   else {
      for ( yo = varctx->MinVal[var]; yo < varctx->MaxVal[var]; yo += step){
         counter ++;
         if ( (dtx->Sound.mainvarstep * counter)+BORDER < dtx->Sound.sndwidth+BORDER){
            XDrawLine( SndDpy, dtx->Sound.soundpix, var_gc, 
                    (dtx->Sound.mainvarstep * counter)+BORDER, dtx->Sound.sndheight+BORDER-HEBGBS,
                    (dtx->Sound.mainvarstep * counter)+BORDER, BORDER-HEBGBS);
         }
      }
   }
   XSetLineAttributes(SndDpy, dtx->Sound.var1_gc, 2, LineSolid, CapButt, JoinRound);
   XSetLineAttributes(SndDpy, dtx->Sound.var2_gc, 2, LineSolid, CapButt, JoinRound);
   XSetLineAttributes(SndDpy, dtx->Sound.var3_gc, 2, LineSolid, CapButt, JoinRound);

}   

    /**********************************************************************/
    /* This just converts pressure to height using DEFAULT_LOG_EXP        */
    /* and DEFAULT_LOG_EXP                                                */
    /**********************************************************************/
    /* Input: dtx -context                                                */
    /*        pressure - the pressure to convert from                     */
    /**********************************************************************/
   
static float pres_to_height(float pressure)
{
   return  (DEFAULT_LOG_EXP * log( pressure / DEFAULT_LOG_SCALE));
}

    /**********************************************************************/
    /* This just converts height to pressure using DEFAULT_LOG_EXP        */
    /* and DEFAULT_LOG_EXP                                                */
    /**********************************************************************/
    /* Input: dtx -context                                                */
    /*        height - the height to convert from                         */
    /**********************************************************************/

static float height_to_pres(float height)
{
   return (DEFAULT_LOG_SCALE * exp( height / DEFAULT_LOG_EXP));
}


    /**********************************************************************/
    /* This draws the main border box surrounding the sounding data       */
    /**********************************************************************/
    /* Input: dtx -context                                                */
    /**********************************************************************/

static void draw_box ( Display_Context dtx )
{ 
   XDrawLine ( SndDpy, dtx->Sound.soundpix, dtx->Sound.box_gc,
               BORDER, BORDER-5-HEBGBS, dtx->Sound.sndwidth+BORDER, BORDER-5-HEBGBS);

   XDrawLine ( SndDpy, dtx->Sound.soundpix, dtx->Sound.box_gc,
               dtx->Sound.sndwidth+BORDER, BORDER-5-HEBGBS, dtx->Sound.sndwidth+BORDER,
               dtx->Sound.sndheight+BORDER-HEBGBS);

   XDrawLine ( SndDpy, dtx->Sound.soundpix, dtx->Sound.box_gc,
               BORDER, BORDER-5-HEBGBS, BORDER, dtx->Sound.sndheight+BORDER-HEBGBS);
   if((dtx->Sound.vertsys == 0) || (dtx->BottomBound < -1.0)){
      XDrawLine ( SndDpy, dtx->Sound.soundpix, dtx->Sound.box_gc,
               BORDER, dtx->Sound.sndheight+BORDER-HEBGBS,
               dtx->Sound.sndwidth+BORDER, dtx->Sound.sndheight+BORDER-HEBGBS);
   }
}

    /**********************************************************************/
    /* This returns the saturation vapor pressure for a specified         */
    /* temperature in Kelvin                                              */
    /**********************************************************************/
    /* Input: K - temperature in Kelvin                                   */
    /* Output: svp - the saturation vapor pressure                        */
    /**********************************************************************/

static float svp( float K)
{
   float hit;

   hit = exp( (17.2693882*K - 4717.306) / (K - 35.7) );
   return  (6.1078 * hit);
}

    /**********************************************************************/
    /* This returns a mixing ration for a given temperature in Kelvin     */
    /* and a given pressure in millibars                                  */
    /**********************************************************************/
    /* Input: K - temperature in Kelvin                                   */
    /*        pres- pressure in millibars                                 */
    /* Output: mixratio - the mixing ratio                                */
    /**********************************************************************/

static float mixratio( float K, float pres )
{
   return (621.97 * svp(K) / ( pres - svp(K)));
}

    /**********************************************************************/
    /* This will give a thta-e at 1012.5 millibars for a given            */
    /* pressure in millibars and temperature in Kelvin                    */
    /**********************************************************************/
    /* Input: pres - pressure in millibars                                */
    /*        temp - temperature in Kelvin                                */
    /* Output: thetaE - theta-E in Kelvin                                 */
    /**********************************************************************/

static float thetaE( float pres, float temp)
{
   float thta, te;

   thta = temp * pow((1012.5/pres),.286);
   te = thta * exp(2.6518986* mixratio(temp, pres) / temp);
   return te;
}

    /**********************************************************************/
    /* This will approximate what the temperature(K)    for a given       */
    /* pressure (Millibars) and a given thetaE (K)                        */
    /**********************************************************************/
    /* Input: thte - Theta-E in Kelvin                                    */
    /*        pres - pressure in millibars                                */
    /* Output: get_temp_for_thte - the returned temperature in Kelving    */
    /**********************************************************************/

   
static float get_temp_for_thte(float thte, float pres)
{
   float tgnu= 293.16;
   float tgnup, tenu, tenup, cor;
   int x;

   for (x=1; x < 100; x++){
      tgnup = tgnu + 1.0;
      tenu = thetaE( pres, tgnu);
      tenup =thetaE( pres, tgnup);
      cor = (thte - tenu) / (tenup - tenu);
      tgnu = tgnu + cor;
      if (( cor < .01) && ((-1 * cor) < .01)){
         return tgnu;
      }
   }
   return 9999.9;
}




/* MJK 12.15.98 begin */
/*
 *  The sounding background grids have been changed to more closely
 *  resemble those of the sounding plots on NWS AWIPS and WFO-Advanced
 *  systems.
 *  These grids may not be desired by everyone.
 */

#define CLIP_PT1_CLIPPED        1
#define CLIP_PT2_CLIPPED        2
#define CLIP_BOTH_CLIPPED       3
#define CLIP_BOTH_OUT           -1

#define CLIP_MOVE_X(VAL,X,Y)    X = (VAL), Y = ((((VAL) * dy) + a) / dx)
#define CLIP_MOVE_Y(VAL,Y,X)    Y = (VAL), X = ((((VAL) * dx) - a) / dy)



static int clip_line_seg (Display_Context dtx,
                          int *ix1, int *iy1, int *ix2, int *iy2)
{
   int          clip_stat, ixa, iya, ixb, iyb;
   float        x1, y1, x2, y2, dx, dy, a, w, h;

   x1 = *ix1, y1 = *iy1;
   x2 = *ix2, y2 = *iy2;

   dx = x2 - x1;
   dy = y2 - y1;
   a  = (y1 * dx) - (x1 * dy);

   w  = dtx->Sound.sndwidth;
   h  = dtx->Sound.sndheight;

   /* MJK 3.22.99 */
   if (x1 < 0.0 && y1 < 0.0){
      return CLIP_BOTH_OUT;
   }

   if (x1 < 0.0)
   {
      if (x2 < 0.0) return CLIP_BOTH_OUT;
      CLIP_MOVE_X(0.0, x1, y1);
   }
   else if (x1 > w)
   {
      if (x2 > w) return CLIP_BOTH_OUT;
      CLIP_MOVE_X(w, x1, y1);
   }

   if (y1 < 0.0)
   {
      if (y2 < 0.0) return CLIP_BOTH_OUT;
      CLIP_MOVE_Y(0.0, y1, x1);
   }
   else if (y1 > h)
   {
      if (y2 > h) return CLIP_BOTH_OUT;
      CLIP_MOVE_Y(h, y1, x1);
   }

   if (x2 < 0.0)
   {
      CLIP_MOVE_X(0.0, x2, y2);
   }
   else if (x2 > w)
   {
      CLIP_MOVE_X(w, x2, y2);
   }

   if (y2 < 0.0)
   {
      CLIP_MOVE_Y(0.0, y2, x2);
   }
   else if (y2 > h)
   {
      CLIP_MOVE_Y(h, y2, x2);
   }

   ixa = x1 + 0.5, iya = y1 + 0.5;
   ixb = x2 + 0.5, iyb = y2 + 0.5;

   clip_stat = 0;
   if ((ixa != *ix1) || (iya != *iy1)) clip_stat |= CLIP_PT1_CLIPPED;
   if ((ixb != *ix2) || (iyb != *iy2)) clip_stat |= CLIP_PT2_CLIPPED;
   if (clip_stat == CLIP_BOTH_CLIPPED)
   {
      if ((ixa == ixb) && (iya == iyb)) return CLIP_BOTH_OUT;
   }

   *ix1 = ixa;
   *iy1 = iya;
   *ix2 = ixb;
   *iy2 = iyb;

   return clip_stat;
}

static int draw_line_seg (Display_Context dtx, Drawable drawable, GC gc,
                          int ix1, int iy1, int ix2, int iy2)
{
   int  clip_stat, ixa, iya, ixb, iyb;


   ixa = ix1, iya = iy1;
   ixb = ix2, iyb = iy2;

   clip_stat = clip_line_seg (dtx, &ixa, &iya, &ixb, &iyb);
   if (clip_stat != CLIP_BOTH_OUT)
   {
      XDrawLine (SndDpy, drawable, gc,
                 ixa+BORDER, iya+BORDER-HEBGBS, ixb+BORDER, iyb+BORDER-HEBGBS);
   }

   return clip_stat;
}

    /**********************************************************************/
    /* This draws the contstant mixing ratio lines                        */
    /**********************************************************************/
    /* Input dtx - context                                                */
    /**********************************************************************/

static void draw_wlines( Display_Context dtx)
{
#define N_MIXRAT        6

   float        mixrat[N_MIXRAT] = {0.5, 1.0, 2.0, 5.0, 10.0, 20.0};
   float        ptop = 400.0;

   int i, nchr, wow, x, y, oldx, oldy, txtw, txth, txth2, txtx, txty, xx, yy;
   float gotomintemp, p;
   char label[8];
   XFontStruct *font_info;

   x = -9999;
   y = -9999;

   if (XLoadFont(SndDpy, dtx->SoundFontName)) {
      font_info = XLoadQueryFont (SndDpy, dtx->SoundFontName);
      XSetFont(SndDpy, dtx->Sound.box_gc, font_info->fid);
      txth      = font_info->ascent;
      txth2     = txth / 2;
   }
   XSetLineAttributes (SndDpy, dtx->Sound.box_gc, 1, LineOnOffDash,
                       CapRound, JoinRound);

   data_to_y (dtx, pressure_to_height (625.0), &txty);

   for (i = 0; i < N_MIXRAT; i++) {
      wow = 1;
      for( gotomintemp = 373.0; gotomintemp > 173.0; gotomintemp -= .1){
         p = ((621.97 * svp(gotomintemp) +
              (mixrat[i] * svp(gotomintemp)))/mixrat[i]);
         if ((p <= 1012.5) && (wow == 1)){
            wow = 0;
            data_to_xy( dtx, pressure_to_height(p), gotomintemp, &x, &y);
            oldx = x;
            oldy = y;
         }
         if ( p <= ptop) {
            data_to_xy( dtx, pressure_to_height(p), gotomintemp, &x, &y);
            gotomintemp = 100;
         }
      }
      clip_line_seg (dtx, &oldx, &oldy, &x, &y);

      yy = txty + txth;
      xx = oldx;
      if (y != oldy) xx += ((yy - oldy) * (x - oldx) / (y - oldy));
      XDrawLine( SndDpy, dtx->Sound.soundpix, dtx->Sound.box_gc,
                 oldx+BORDER, oldy+BORDER-HEBGBS, xx+BORDER, yy+BORDER-HEBGBS);
      yy = txty - txth;
      xx = oldx;
      if (y != oldy) xx += ((yy - oldy) * (x - oldx) / (y - oldy));
      XDrawLine( SndDpy, dtx->Sound.soundpix, dtx->Sound.box_gc,
                 x+BORDER, y+BORDER-HEBGBS, xx+BORDER, yy+BORDER-HEBGBS);

      if(mixrat[i] < 1.0){
         sprintf(label, "%.1f", mixrat[i]);
      }
      else {
         sprintf(label, "%.0f", mixrat[i]);
      }
      nchr = strlen (label);
      txtw = XTextWidth (font_info, label, nchr) / 2;
      txtx = oldx + ((x - oldx) * (txty - oldy) / (y - oldy));
      XDrawString( SndDpy, dtx->Sound.soundpix, dtx->Sound.box_gc,
                   txtx+BORDER-txtw, txty+BORDER-HEBGBS+txth2,
                   label, nchr);
   }

   XSetLineAttributes (SndDpy, dtx->Sound.box_gc, 1, LineSolid,
                       CapRound, JoinRound);
      
      
#undef N_MIXRAT
}

    /**********************************************************************/
    /* This draws the moist adiabat lines                                 */
    /**********************************************************************/
    /* Input dtx - context                                                */
    /**********************************************************************/

static void draw_thtelines( Display_Context dtx )
{
   float p, step, step2;
   float yo, gotomaxheight;
   int x, y, oldx, oldy;


   XSetLineAttributes (SndDpy, dtx->Sound.box_gc, 1, LineOnOffDash,
                       CapRound, JoinRound);

   step = .5;
   if ((dtx->Sound.sndwidth < 300) || (dtx->Sound.sndheight < 300)){
      step2= 20;
   }
   else if ((dtx->Sound.sndwidth <550) || (dtx->Sound.sndheight < 550)){
      step2= 10;
   }
   else {
      step2= 5;
   }
   for (yo = dtx->Sound.SndMaxTemp-10; yo > dtx->Sound.SndMinTemp; yo -= step2){
      data_to_xy( dtx, 0, yo, &x, &y);
      for (gotomaxheight = 0.0;
      gotomaxheight < dtx->Sound.TopHgt; gotomaxheight += step){
         p = height_to_pres(gotomaxheight);
         oldx = x;
         oldy = y;
         data_to_xy( dtx, gotomaxheight,
                     get_temp_for_thte(thetaE(1012.5, yo), p), &x, &y);

         draw_line_seg (dtx, dtx->Sound.soundpix, dtx->Sound.box_gc,
                        oldx, oldy, x, y);
      }
   }

   XSetLineAttributes (SndDpy, dtx->Sound.box_gc, 1, LineSolid,
                       CapRound, JoinRound);
}

    /**********************************************************************/
    /* This draws the dry adiabat lines                                   */
    /**********************************************************************/
    /* Input dtx - context                                                */
    /**********************************************************************/

static void draw_thtalines( Display_Context dtx )
{
   int yo, x, y, oldx, oldy, txtw, txth, clip_stat, drew_some;
   float mintemp = 123.0;
   float step = 10.0, step2 = 10.0, step3 = 5.0;
   float p,h;
   float gotomintemp;
   char thtastring [8];
   XFontStruct *font_info;

   if (XLoadFont(SndDpy, dtx->SoundFontName)){
      font_info = XLoadQueryFont (SndDpy, dtx->SoundFontName);
      XSetFont(SndDpy, dtx->Sound.box_gc, font_info->fid);
      txtw      = XTextWidth (font_info, "999", 3) / 2;
      txth      = font_info->ascent;
   }

   x     = dtx->Sound.sndwidth / (txtw * 8 / 3);
   y     = (470 - 290) / 5;
   step  = (x <= 0) ? 50 : (y < x) ? 5 : ((y + (x-1)) / x) * 5;

   for (yo = 470; yo >= 240; yo -= step) {
      sprintf(thtastring, "%d", yo);
      data_to_xy( dtx, 0, yo, &x, &y );

      drew_some = 0;
      for (gotomintemp = yo; gotomintemp >  mintemp; gotomintemp -= step3){
         p =  exp((1/.286)*(log((7.23674 * gotomintemp)/yo)));
         h = pressure_to_height( p );
         oldx = x;
         oldy = y;
         data_to_xy(dtx, h, gotomintemp,&x,&y);

         clip_stat = draw_line_seg (dtx, dtx->Sound.soundpix, dtx->Sound.box_gc,
                                    oldx, oldy, x, y);
         if (clip_stat == CLIP_BOTH_OUT) {
            if (drew_some) break;
         }
         else {
            if (clip_stat & CLIP_PT2_CLIPPED) break;
            drew_some = 1;
         }
      }
      x = oldx + ((0 - oldy) * (x - oldx) / (y - oldy));
      y = 0;
      if ((x > 0) && (x < dtx->Sound.sndwidth)) {
         XDrawString (SndDpy, dtx->Sound.soundpix, dtx->Sound.box_gc,
                      x-txtw+BORDER, y-txth+BORDER-HEBGBS, thtastring, 3);
      }
   }
}

    /**********************************************************************/
    /* This draws the temperature lines                                   */
    /**********************************************************************/
    /* Input dtx - context                                                */
    /**********************************************************************/

static void draw_templines( Display_Context dtx )
{
   int yo, yo2, x, y, oldx, oldy, txtw, txth, txth2, nchr;
   float step;
   char tempstring [8];
   XFontStruct *font_info;

   if (XLoadFont(SndDpy, dtx->SoundFontName)){
      font_info = XLoadQueryFont (SndDpy, dtx->SoundFontName);
      XSetFont(SndDpy, dtx->Sound.box_gc, font_info->fid);
      txth      = font_info->ascent;
      txth2     = txth / 2;
   }

   if ((dtx->Sound.sndwidth < 300) || (dtx->Sound.sndheight < 300)){
      step = 20;
   }
   else if ((dtx->Sound.sndwidth <550) || (dtx->Sound.sndheight < 550)){
      step = 10;
   }
   else {
      step = 5;
   }

   for (yo = 50; yo >= -120; yo -= step) {
      yo2 = yo + 273;
      sprintf(tempstring, "%d", yo);
      nchr = strlen (tempstring);

      data_to_xy( dtx, dtx->Sound.BotHgt, yo2, &oldx, &oldy );
      data_to_xy( dtx, dtx->Sound.TopHgt, yo2, &x, &y );

      clip_line_seg (dtx, &oldx, &oldy, &x, &y);
      draw_line_seg (dtx, dtx->Sound.soundpix, dtx->Sound.box_gc,
                     oldx, oldy, x, y);
      if (y == 0) {
         y -= txth * 5 / 2;
         XDrawString( SndDpy, dtx->Sound.soundpix, dtx->Sound.box_gc,
                      x+BORDER, y+BORDER-HEBGBS,
                      tempstring, nchr);
      }
      else if (x == dtx->Sound.sndwidth) {
         XDrawString( SndDpy, dtx->Sound.soundpix, dtx->Sound.box_gc,
                      dtx->Sound.sndwidth+4+BORDER, y+txth2+BORDER-HEBGBS,
                      tempstring, nchr);
      }
   }
}

    /**********************************************************************/
    /* This just draws the millibars.                                     */
    /**********************************************************************/
    /* Input dtx - context                                                */
    /**********************************************************************/

static void draw_millibarlines ( Display_Context dtx )
{
   float press;
   int x, y, xleft, xright, xmid, nchr, txtw, txth2, txts;
   float temp;
   char label[8];
   XFontStruct *font_info;

   xleft  = BORDER;
   xright = BORDER + dtx->Sound.sndwidth;
   xmid   = (xleft + xright) / 2;

   if (XLoadFont(SndDpy, dtx->SoundFontName)) {
      font_info = XLoadQueryFont (SndDpy, dtx->SoundFontName);
      XSetFont(SndDpy, dtx->Sound.box_gc, font_info->fid);
      txth2     = font_info->ascent / 2;
      txts      = XTextWidth (font_info, " ", 1);
   }

   for (press = dtx->Sound.BotPress; press >= dtx->Sound.TopPress;
        press -= 50) {
      data_to_xy( dtx, pressure_to_height (press), 266, &x, &y);
      if ((((int) (press + 0.5)) % 100) == 0) {
         if ( y > 10 ) {
            sprintf(label,"%.0f", press );
            nchr = strlen (label);
            txtw = XTextWidth (font_info, label, nchr) / 2;
            XDrawString( SndDpy, dtx->Sound.soundpix, dtx->Sound.box_gc,
                         xmid-txtw, y+txth2+BORDER-HEBGBS, label, nchr);
            txtw += txts;
            XDrawLine ( SndDpy, dtx->Sound.soundpix, dtx->Sound.box_gc,
                        xleft, y+BORDER-HEBGBS, xmid-txtw, y+BORDER-HEBGBS);

            XDrawLine ( SndDpy, dtx->Sound.soundpix, dtx->Sound.box_gc,
                        xmid+txtw, y+BORDER-HEBGBS, xright, y+BORDER-HEBGBS);
         }
      }
      else {
         XDrawLine ( SndDpy, dtx->Sound.soundpix, dtx->Sound.box_gc,
                     xleft, y+BORDER-HEBGBS, xright, y+BORDER-HEBGBS);
      }
   }
}

#ifdef DONOTUSE




    /**********************************************************************/
    /* This draws the contstant mixing ratio lines                        */
    /**********************************************************************/
    /* Input dtx - context                                                */
    /**********************************************************************/

static void draw_wlines( Display_Context dtx)
{
   GC w_gc = make_gc(dtx, 255, 255, 0, 0, 0, 0, 1);
   int wow, step, x, y, oldx, oldy, yo;
   float gotomintemp, p, wi;
   char shortwst[8], longwst[8];

   x = -9999;
   y = -9999;

   if ((dtx->Sound.sndwidth < 300) || (dtx->Sound.sndheight < 300)){
      step= 4;
   }
   else if ((dtx->Sound.sndwidth <550) || (dtx->Sound.sndheight < 550)){
      step= 3;
   }
   else {
      step= 1;
   }
   XSetLineAttributes(SndDpy, w_gc, 1, LineOnOffDash, CapRound, JoinRound);   
   for (yo = 0; yo < 34; yo += step ){
      wow = 1;
      for( gotomintemp = 373.0; gotomintemp > 173.0; gotomintemp -= .1){
         p = ((621.97 * svp(gotomintemp) + (winterval[yo] * svp(gotomintemp)))/winterval[yo]);
         if ((p <= 1012.5) && (wow == 1)){
            wow = 0;
            data_to_xy( dtx, pres_to_height(p), gotomintemp, &x, &y);
            oldx = x;
            oldy = y;
         }
         if ( p <= 200.0) {
            data_to_xy( dtx, pres_to_height(p), gotomintemp, &x, &y);
            gotomintemp = 100;
         }
      }
      cut_line_data2(dtx, &oldx, &oldy, &x, &y); 
      XDrawLine( SndDpy, dtx->Sound.soundpix, w_gc,
                       oldx+BORDER, oldy+BORDER-HEBGBS,
                       x+BORDER, y+BORDER-HEBGBS);
      w_gc = make_gc(dtx, 255, 255, 0, 0, 0, 0, 1);
      XDrawLine( SndDpy, dtx->Sound.soundpix, w_gc,
                       oldx+BORDER, oldy+BORDER-HEBGBS,
                       oldx+BORDER, oldy+BORDER+12-HEBGBS);
      if (XLoadFont(SndDpy, dtx->SoundFontName)){
            w_gc = make_gc(dtx, 255, 255, 0, 0, 0, 0, 2);
            XSetFont(SndDpy, w_gc, XLoadFont(SndDpy, dtx->SoundFontName));
      }
      if(winterval[yo] < 3.0){
         sprintf(longwst, "%1.1f\n", winterval[yo]);
      XDrawString( SndDpy, dtx->Sound.soundpix, w_gc,
                   oldx+BORDER-7, oldy+BORDER+21-HEBGBS, longwst,3);
      }
      else if(winterval[yo] < 10.0){
         sprintf(longwst, "%d\n", (int) (winterval[yo]));
      XDrawString( SndDpy, dtx->Sound.soundpix, w_gc,
                   oldx+BORDER-2, oldy+BORDER+21-HEBGBS, longwst,1);
      }
      else{
         sprintf(shortwst, "%d\n", (int) (winterval[yo]));
         XDrawString( SndDpy, dtx->Sound.soundpix, w_gc,
                   oldx+BORDER-5, oldy+BORDER+21-HEBGBS, shortwst,2);
      }
      w_gc = make_gc(dtx, 255, 255, 0, 0, 0, 0, 1);
      XSetLineAttributes(SndDpy, w_gc, 1, LineOnOffDash, CapRound, JoinRound);

   }
}          

    /**********************************************************************/
    /* This draws the moist adiabat lines                                 */
    /**********************************************************************/
    /* Input dtx - context                                                */
    /**********************************************************************/

static void draw_thtelines( Display_Context dtx )
{
   GC thte_gc = make_gc(dtx, 10, 140, 55, 0, 0, 0, 2);
   float p, step, step2;
   float yo, gotomaxheight;
   int x, y, oldx, oldy;
   

   step = .5;    
   if ((dtx->Sound.sndwidth < 300) || (dtx->Sound.sndheight < 300)){
      step2= 20;
   }
   else if ((dtx->Sound.sndwidth <550) || (dtx->Sound.sndheight < 550)){
      step2= 10;
   }
   else {
      step2= 5;
   }
   for (yo = dtx->Sound.SndMaxTemp-10; yo > dtx->Sound.SndMinTemp; yo -= step2){
      data_to_xy( dtx, 0, yo, &x, &y);
      for (gotomaxheight = 0.0; 
      gotomaxheight < pres_to_height(200); gotomaxheight += step){
         p = height_to_pres(gotomaxheight);
         oldx = x;
         oldy = y;
         data_to_xy( dtx, gotomaxheight,
                     get_temp_for_thte(thetaE(1012.5, yo), p), &x, &y);
         if ((x <= dtx->Sound.sndwidth) && (y <= dtx->Sound.sndheight)
         && (oldx <= dtx->Sound.sndwidth) && (oldy <= dtx->Sound.sndheight)
         && (x >=0) && (y >= 0) && (oldx >= 0) && (oldy >= 0)){
            XDrawLine( SndDpy, dtx->Sound.soundpix, thte_gc,
                       oldx+BORDER, oldy+BORDER-HEBGBS,
                        x+BORDER, y+BORDER-HEBGBS);
         }
         else if(((oldx >= 0  )&&(oldy >=0 ))&&((x <0) || (y<0))){
            cut_line_data(dtx, oldx, oldy, &x, &y);
            XDrawLine( SndDpy, dtx->Sound.soundpix, thte_gc,
                       oldx+BORDER, oldy+BORDER-HEBGBS,
                        x+BORDER, y+BORDER-HEBGBS);
         }
      }
   }
}

    /**********************************************************************/
    /* This draws the dry adiabat lines                                   */
    /**********************************************************************/
    /* Input dtx - context                                                */
    /**********************************************************************/
 
static void draw_thtalines( Display_Context dtx )
{
   GC thta_gc = make_gc(dtx, 204, 50, 50, 0, 0, 0, 2);
   int yo, yo2, x, y, oldx, oldy, prex, prey;
   float mintemp = 123.0;
   float step = 10.0, step2 = 2.0, step3 = 5.0;
   float p,h;
   float gotomintemp;
   char thtastring [8];
   
   if ((dtx->Sound.sndwidth < 300) || (dtx->Sound.sndheight < 300)){
      step = 20;
      step2= 20;
   }
   else if ((dtx->Sound.sndwidth <550) || (dtx->Sound.sndheight < 550)){
      step = 10;
      step2= 10;
   }
   else {
      step = 10;
      step2= 2;
   }
    
   /* first draw the theta lines along the x axis */
   for (yo = dtx->Sound.SndMaxTemp; yo > dtx->Sound.SndMinTemp; yo -= step2){
      data_to_xy( dtx, 0, yo, &x, &y ); 
      thta_gc = make_gc(dtx, 255, 105, 105, 0, 0, 0, 2);
      if( (((yo - 3) % 10) == 0 ) && yo != dtx->Sound.SndMaxTemp){
         XSetForeground(SndDpy, thta_gc, SND_AllocateColorInt(0,0,0));
         XFillRectangle(SndDpy, dtx->Sound.soundpix, thta_gc,
                        x+BORDER-5, y+BORDER, 22, 12);
         thta_gc = make_gc(dtx, 255, 105, 105, 0, 0, 0, 2);
         if (XLoadFont(SndDpy, dtx->SoundFontName)){
            XSetFont(SndDpy, thta_gc, XLoadFont(SndDpy, dtx->SoundFontName));
         }
         sprintf(thtastring, "%d\n", yo);
         XDrawString( SndDpy, dtx->Sound.soundpix, thta_gc,
                   x+BORDER-5, y+BORDER+10-HEBGBS, thtastring, 3);
      }
      if ( ((yo - 3) % 10) == 0 ){
         thta_gc = make_gc(dtx, 204, 50, 50, 0, 0, 0, 2);
      }
      else {
         thta_gc = make_gc(dtx, 150, 10, 10, 0, 0, 0, 1);
      }   
      if( (((yo - 3) % 10) == 0 ) && yo == dtx->Sound.SndMaxTemp){
         if (XLoadFont(SndDpy, dtx->SoundFontName)){
            thta_gc = make_gc(dtx, 255, 105, 105, 0, 0, 0, 2);
            XSetFont(SndDpy, thta_gc, XLoadFont(SndDpy, dtx->SoundFontName));
         }

         sprintf(thtastring, "%d\n", yo);
         XDrawString( SndDpy, dtx->Sound.soundpix, thta_gc,
                   x+BORDER+4, y+BORDER+10-HEBGBS, thtastring, 3);
         thta_gc = make_gc(dtx, 204, 50, 50, 0, 0, 0, 2);
      }
      for (gotomintemp = yo; gotomintemp >  mintemp; gotomintemp -= step3){
         p =  exp((1/.286)*(log((7.23674 * gotomintemp)/yo)));
         h = pres_to_height( p );
         oldx = x;
         oldy = y;
         data_to_xy(dtx, h, gotomintemp,&x,&y);
         if ((x <= dtx->Sound.sndwidth) && (y <= dtx->Sound.sndheight)
         && (oldx <= dtx->Sound.sndwidth) && (oldy <= dtx->Sound.sndheight)
         && (x >=0) && (y >= 0) && (oldx >= 0) && (oldy >= 0)){
            XDrawLine( SndDpy, dtx->Sound.soundpix, thta_gc,
                       oldx+BORDER, oldy+BORDER-HEBGBS,
                       x+BORDER, y+BORDER-HEBGBS);
         }
         else if(((oldx >= 0  )&&(oldy >=0 ))&&((x <0) || (y<0))){
            cut_line_data(dtx, oldx, oldy, &x, &y);
            XDrawLine( SndDpy, dtx->Sound.soundpix, thta_gc,
                       oldx+BORDER, oldy+BORDER-HEBGBS,
                       x+BORDER, y+BORDER-HEBGBS);
            gotomintemp = mintemp-1; 
         } 
      }
   }
   /* now draw the thteta line up the right y axis */
  for (yo = 483; yo > dtx->Sound.SndMaxTemp; yo -= step2){
      data_to_xy( dtx, h, dtx->Sound.SndMaxTemp , &x, &y);
      if ( ((yo - 3) % 10) == 0 ){
         thta_gc = make_gc(dtx, 204, 50, 50, 0, 0, 0, 2);
      }
      else {
         thta_gc = make_gc(dtx, 150, 10, 10, 0, 0, 0, 1); 
      }
      for (gotomintemp = yo; gotomintemp >  mintemp; gotomintemp -= step3){
         p =  exp((1/.286)*(log((7.23674 * gotomintemp)/yo)));
         h = pres_to_height( p );
         oldx = x;
         oldy = y;
         data_to_xy(dtx, h, gotomintemp,&x,&y);
         prex = oldx;
         prey = oldy;
         if(((oldx>dtx->Sound.sndwidth) || ( oldy > dtx->Sound.sndheight)) &&
            (x <= dtx->Sound.sndwidth) && (y <= dtx->Sound.sndheight) &&
            (x >= 0 ) && (y >= 0 )){
            precut_line_data( dtx, &oldx, &oldy, x, y);
            XDrawLine( SndDpy, dtx->Sound.soundpix, thta_gc,
                       oldx+BORDER, oldy+BORDER-HEBGBS,
                       x+BORDER, y+BORDER-HEBGBS);
            if(((yo - 3) % 10) == 0 ){
               thta_gc = make_gc(dtx, 255, 105, 105, 0, 0, 0, 2);
               if (XLoadFont(SndDpy, dtx->SoundFontName)){
                  XSetFont(SndDpy, thta_gc, XLoadFont(SndDpy, dtx->SoundFontName));
               }
               sprintf(thtastring, "%d", yo);
               XDrawString( SndDpy, dtx->Sound.soundpix, thta_gc,
                         oldx+BORDER+4, oldy+BORDER+7-HEBGBS, thtastring, 3);
               thta_gc = make_gc(dtx, 204, 50, 50, 0, 0, 0, 2);
            }
         }
         if ((x <= dtx->Sound.sndwidth) && (y <= dtx->Sound.sndheight)
         && (oldx <= dtx->Sound.sndwidth) && (oldy <= dtx->Sound.sndheight)
         && (x >=0) && (y >= 0) && (oldx >= 0) && (oldy >= 0)){
            XDrawLine( SndDpy, dtx->Sound.soundpix, thta_gc,
                       oldx+BORDER, oldy+BORDER-HEBGBS,
                       x+BORDER, y+BORDER-HEBGBS);
         }
         else if(yo!= 483 && ((oldx > 0  )&&(oldy >0 ))&&((x <0) || (y<0))){
            cut_line_data(dtx, oldx, oldy, &x, &y);
            XDrawLine( SndDpy, dtx->Sound.soundpix, thta_gc,
                       oldx+BORDER, oldy+BORDER-HEBGBS,
                       x+BORDER, y+BORDER-HEBGBS);
            gotomintemp = mintemp-1;
         }
      }
   }
}
#endif

    /**********************************************************************/
    /* This will take two points, check if one of them is outside the data*/
    /* boundry then chop it off, if it is.  It also chops of lines at the */
    /* 200mb level. And if points x1 and y1 are not even in the data      */
    /* boundry then the line will not be drawn                            */
    /**********************************************************************/
    /* Input dtx - context                                                */
    /*       x1, y1, x2, y2 - points making the line to be chopped        */
    /**********************************************************************/

static void cut_line_data2( Display_Context dtx, int *x1, int *y1, int *x2, int *y2)
{
   float m,b;
   float xone, yone, xtwo, ytwo;
   float x_intercept, y_intercept;
   int p_equals_200mb;
   float bottomline;

   xone = (float) (*x1);
   yone = (float) (*y1);
   xtwo = (float) (*x2);
   ytwo = (float) (*y2);
   p_equals_200mb = *y2;

   /* get equation of line for x1, y1, x2, y2 */
   if ((xone - xtwo) == 0 ){
      m = 0.0;
      b = *y1;
   }
   else {
      m = (yone - ytwo)/(xone - xtwo);
      b = yone - (m * xone);
   }
   bottomline = ((dtx->TopBound-0.0001)*((int) (dtx->Sound.sndheight))/
                 (dtx->TopBound-dtx->BottomBound+0.0001));

   /* see where this line intersects the 1012.5 mb line */
   *x1 = (int) ((bottomline - b)/m);
   *y1 = (int) (bottomline);

   /* see where the line intersects the right line */
   *x2 = dtx->Sound.sndwidth;
   *y2 = (int) ((float) dtx->Sound.sndwidth * m + b);

   /* if it intersects the top line, the chop it off */
   if ((*y2) < p_equals_200mb){
      *x2 = (int) ((p_equals_200mb - b) / m );
      *y2 = p_equals_200mb;
   }
   if ((*y2) < 0){
      *x2 = (int) (-b / m);
      *y2 = 0;
   }
}   

    /**********************************************************************/
    /* When figureing out the starting point for the theta lines in the + */
    /* y direction on the right side of the data boundry this will chop   */
    /* off the given line so it starts at the right side given by y =     */
    /* dtx->Sound.sndwidth                                                */
    /**********************************************************************/
    /* Input dtx - context                                                */
    /*       x1, y1, x2, y2 - points making the line to be chopped        */
    /* Output: x1, y1 - these may be altered in order to start at boudry  */
    /**********************************************************************/
 
static void precut_line_data (Display_Context dtx, int *x1, int *y1, int x2, int y2)
{
   float m,b;
   float line_intercept;
   float  xone, yone, xtwo, ytwo;

   /* get equation of line for x1, y1, x2, y2 */
   xone = (float) *x1;
   yone = (float) *y1;
   xtwo = (float) (x2);
   ytwo = (float) (y2);
   if ((xone - xtwo) == 0 ){
      m = 0.0;
      b = yone;
   }
   else {
      m = (yone - ytwo)/(xone - xtwo);
      b = yone - (m * xone);
   }
   
   /* see where on the line x= sndwidth it intersecpts */
   line_intercept = m * dtx->Sound.sndwidth + b;

   *x1 = dtx->Sound.sndwidth;
   *y1 = (int) (line_intercept); 
}



    /**********************************************************************/
    /* This cuts the given line so that it's end point will be on the     */
    /* left or top side of the data boundry.  It is used in drawing the   */
    /* theta lines.                                                       */
    /**********************************************************************/
    /* Input dtx - context                                                */
    /*       x1, y1, x2, y2 - points making the line to be chopped        */
    /* Output x2, y2 - these points may be altered to end at data boudry  */
    /**********************************************************************/

        
static void cut_line_data( Display_Context dtx, int x1, int y1, int *x2, int *y2)
{
   float m,b;
   float x_intercept, y_intercept;
   float  xone, yone, xtwo, ytwo;
 
   /* get equation of line for x1, y1, x2, y2 */
   xone = (float) x1;
   yone = (float) y1;
   xtwo = (float) (*x2);
   ytwo = (float) (*y2);
   if ((x1 - xtwo) == 0 ){
      m = 0.0;
      b = yone;
   }
   else { 
      m = (yone - ytwo)/(xone - xtwo);
      b = yone - (m * xone);
   }

   /* see where this line intersects the axies */
   y_intercept = b;
   x_intercept = (-1.0 * b)/m;

   if (y_intercept == 0 ){
      *x2 = 0;
      *y2 = 0;
   }
   else if ( y_intercept < 0 ){
      *x2 = (int) (x_intercept);
      *y2 = 0;
   }
   else if ( x_intercept < 0){
      *x2 = 0;
      *y2 = (int) (y_intercept);
   }
}

    /**********************************************************************/
    /* This draws the numbers and ticks up the vertical                   */
    /**********************************************************************/
    /* Input dtx - context                                                */
    /**********************************************************************/

static void draw_vert_stuff( Display_Context dtx )
{
   int yo, y;
   int maxlev;
   int dash_offset;
   int dash_list_length = { TICK_DASH_LENGTH };
   static  char dotted[2] = {4, 12};
   char *dash_list[] = {dotted};
   char num[10];
   int stringnumber;
   double average;

   if (!dtx->Sound.vert_gc){
      dtx->Sound.vert_gc = make_gc(dtx, 255, 255, 255, 0, 0, 0, 2);
   }
   maxlev = (int) (dtx->Sound.vertdata[0]);
   {
      double total=0.0;;
      for (yo=1; yo < maxlev + 1; yo++){
         total += dtx->Sound.vertdata[yo];
      }
      average = total / (double)(maxlev);
   }    
      
   for ( yo=1; yo < maxlev + 1; yo++){
      dtx->Sound.vert_gc = make_gc(dtx, 100, 100, 100, 0, 0, 0, 1);
      XSetLineAttributes(SndDpy, dtx->Sound.vert_gc, 0, LineOnOffDash, CapButt, JoinRound);
      XSetDashes(SndDpy, dtx->Sound.vert_gc, 1, dash_list[0], 2);
      data_to_y( dtx, dtx->Sound.vertdata[yo], &y);
      if(dtx->Sound.tickstatus){ 
         XDrawLine( SndDpy, dtx->Sound.soundpix, dtx->Sound.vert_gc, BORDER, y + BORDER - HEBGBS,
                    dtx->Sound.sndwidth+BORDER, y + BORDER - HEBGBS);
      }
      if (average < .1){
         float numb;
         numb = dtx->Sound.vertdata[yo] * 1000.0;
         sprintf(num, "%.2f\n", numb);
      }
      else{
         sprintf(num, "%.1f\n", dtx->Sound.vertdata[yo]);
      }
      stringnumber = strlen(num)-1;
      dtx->Sound.vert_gc = make_gc(dtx, 255, 255, 255, 0, 0, 0, 1);
      if (XLoadFont(SndDpy, dtx->SoundFontName)){
         XSetFont(SndDpy, dtx->Sound.vert_gc, XLoadFont(SndDpy, dtx->SoundFontName));
      }
      XDrawString( SndDpy, dtx->Sound.soundpix, dtx->Sound.vert_gc,
                   dtx->Sound.sndwidth + BORDER + 25, y + BORDER - HEBGBS + 4,
                   num, stringnumber);
   }
   if (average < .1){
      sprintf(num, "m");
   }
   else{
      sprintf(num, "Km");
   }
   if( dtx->Sound.vertsys != 0 )
      XDrawString( SndDpy, dtx->Sound.soundpix, dtx->Sound.vert_gc,
                 dtx->Sound.sndwidth + BORDER + 25+5,  BORDER - HEBGBS -8,
                 num, 2);
}     




#ifdef DONOTUSE

    /**********************************************************************/
    /* This just draws the millibars.                                     */
    /**********************************************************************/
    /* Input dtx - context                                                */
    /**********************************************************************/
 
static void draw_millibarlines ( Display_Context dtx )
{
   GC millibars_gc = make_gc(dtx, 110, 110, 110, 0, 0, 0, 1);
   float millibars, pressure;
   int yo, x, y, increment, number;
   float temp;
   char nums[8]; 

   increment = 2;
   pressure = 1100.0; 
   for (yo=0; yo < 10; yo++ ){
      pressure -= 50.0 * (float) increment; 
      millibars = pres_to_height( pressure );
      number = pressure;
      sprintf(nums,"%d\n", number );
      data_to_xy( dtx, millibars, 266, &x, &y); 
      if ( y > 10 ) {

        if (millibars_gc) XFreeGC(SndDpy, millibars_gc);

         millibars_gc = make_gc(dtx, 140, 140, 140, 0, 0, 0, 1);
         if (pressure == 1000.0){
            XDrawString( SndDpy, dtx->Sound.soundpix, millibars_gc,
                      (dtx->Sound.sndwidth/2-23)+BORDER, y+2+BORDER-HEBGBS, nums, 4);
         }
         else { 
            XDrawString( SndDpy, dtx->Sound.soundpix, millibars_gc,
                      (dtx->Sound.sndwidth/2-19)+BORDER, y+2+BORDER-HEBGBS, nums, 3);
         }

        if (millibars_gc) XFreeGC(SndDpy, millibars_gc);

         millibars_gc = make_gc(dtx, 110, 110, 110, 0, 0, 0, 1);
         XDrawLine ( SndDpy, dtx->Sound.soundpix, millibars_gc, BORDER, y+BORDER-HEBGBS,
                     (dtx->Sound.sndwidth/2-24)+BORDER, y+BORDER-HEBGBS);
   
         XDrawLine ( SndDpy, dtx->Sound.soundpix, millibars_gc,
                     (dtx->Sound.sndwidth/2+24)+BORDER , y+BORDER-HEBGBS,
                      dtx->Sound.sndwidth+BORDER, y+BORDER-HEBGBS); 
      }
   }

   if (millibars_gc) XFreeGC(SndDpy, millibars_gc);

}
#endif
    /**********************************************************************/
    /* This will draw a barb onto the soundwin                            */
    /**********************************************************************/
    /* Input: dtx - context                                               */
    /*        spd - this is the given speed in meters/second              */
    /*        dir - this is the direction in radians                      */
    /*        height - this is the height at which the barb info is given */
    /**********************************************************************/
 
/* MJK 12.15.98 begin */
static void make_a_barb( Display_Context dtx, float spd, float dir,
                         float height, int barb_size)
{
   int location;
   int mainstemlength = barb_size;
   int fiftycount = 0;
   int tencount = 0;
   int fivecount = 0;
   int calmcount = 0;
   int speed;
   int yo;
   int space = mainstemlength / 8;
   int bigx  = space * 2;
   int x,y;
   XPoint  flagpoints[3];
/* MJK 12.15.98 end */

   data_to_xy( dtx, height, 273, &x, &y);
   /* get the count of how many and what kind of flags */
   spd = spd / .51282;
   speed = (int) spd;
   fiftycount = speed / 50;
   speed -= fiftycount * 50;
   tencount   = speed / 10;
   speed -= tencount * 10;
   fivecount  = speed / 5;
   if ( fiftycount == 0 && tencount == 0 && fivecount == 0 ){
      calmcount = 1;
   }

   /* now contruct a barb giving the x and y and draw that line */
   location = mainstemlength;

   /*draw main stem line */
   if (!calmcount) drawbarbline(dtx, 0, 0, 0, location, y, dir);

   /*draw any 50's if there is any*/
   for (yo = 0; yo < fiftycount; yo++){
      drawbarbline(dtx, 0, location, bigx, location, y, dir);
      location -= space;
      drawbarbline(dtx, 0, location, bigx, location + space, y, dir);
      flagpoints[0].x = 0;
      flagpoints[0].y = location + space;
      flagpoints[1].x = bigx;
      flagpoints[1].y = location + space;
      flagpoints[2].x = 0;
      flagpoints[2].y = location;
      drawbarbflag( dtx, flagpoints, y, dir);
}

   /*draw any 10's if there are any*/
   for (yo = 0; yo < tencount; yo ++){
      if ( yo != 0 ) location -= space;
      drawbarbline(dtx, 0, location, bigx, location+space, y, dir);
   }

   /*draw any 5's if there are any */
   for (yo = 0; yo < fivecount; yo++){
      location -= space;
      drawbarbline(dtx, 0, location, bigx/2, location + space/2, y, dir);
   }

   /*draw the calm little x if it is calm out*/
   if (calmcount == 1){
      drawbarbline(dtx,-4, 0, 4, 0, y, 0);
      drawbarbline(dtx, 0, -4, 0, 4,y, 0);
   }
}

    /**********************************************************************/
    /* This will draw and color in any barb flags                         */
    /**********************************************************************/
    /* Input: dtx - context                                               */
    /*        flagpoints - the three points making up the barb flag       */
    /*        up - the lenght from the center of barb to flag             */
    /*        dir - the number or radians from 0 degrees to turn and draw */
    /*              the barb flag                                         */
    /**********************************************************************/

static void drawbarbflag( Display_Context dtx, XPoint flagpoints[], int up, float dir)
{
   int x, y, x1, x2, x3, y1, y2, y3, xx1, yy1, xx2, xx3, yy2, yy3;;

   x = 0;
   y = up;
   x1 = flagpoints[0].x;
   y1 = flagpoints[0].y;
   x2 = flagpoints[1].x;
   y2 = flagpoints[1].y;
   x3 = flagpoints[2].x;
   y3 = flagpoints[2].y;

   convert_xy_to_barb( dtx, x1, y1, dir, &xx1, &yy1);
   convert_xy_to_barb( dtx, x2, y2, dir, &xx2, &yy2);
   convert_xy_to_barb( dtx, x3, y3, dir, &xx3, &yy3);
   flagpoints[0].x = xx1 + x + BORDER;
   flagpoints[0].y = yy1 + y + BORDER-HEBGBS;
   flagpoints[1].x = xx2 + x + BORDER;
   flagpoints[1].y = yy2 + y + BORDER-HEBGBS;
   flagpoints[2].x = xx3 + x + BORDER;
   flagpoints[2].y = yy3 + y + BORDER-HEBGBS;

   XFillPolygon( SndDpy, dtx->Sound.soundwin, dtx->Sound.barb2_gc,
                 flagpoints, 3, Nonconvex, CoordModeOrigin);
}

    /**********************************************************************/
    /* This will take two points,the radius, and direction and draw a line*/
    /**********************************************************************/
    /* Input dtx - context                                                */
    /*       x1, y1, x2, y2 - points for the line to be drawn             */
    /*       up - radius                                                  */
    /*       dir - degrees from 0 degree to turn                          */
    /**********************************************************************/


static void drawbarbline( Display_Context dtx, int x1, int y1, int x2, int y2,
                           int up, float dir)
{
   int x, y, xx1, yy1, xx2, yy2;

   x = 0;
   y = up;
   convert_xy_to_barb( dtx, x1, y1, dir, &xx1, &yy1);
   convert_xy_to_barb( dtx, x2, y2, dir, &xx2, &yy2);
   XDrawLine( SndDpy, dtx->Sound.soundwin, dtx->Sound.barb_gc, xx1 + x + BORDER,
              yy1 + y + BORDER-HEBGBS, xx2 + x + BORDER,
              yy2 + y + BORDER-HEBGBS);
}

    /**********************************************************************/
    /* This will take a point, convert it to polar coordinates, turn it   */
    /* the given direction, then convert it back to catesian coordinates  */
    /**********************************************************************/
    /* Input: dtx - context                                               */
    /*        inx, iny - the x and y fot the point to be transformed      */
    /*        dir - the number of radians to turn the point to the right  */
    /*        outx, outy - the x and y after transformation               */
    /**********************************************************************/


static void convert_xy_to_barb(Display_Context dtx, int inx, int iny, float dir,
                                          int *outx, int *outy)
{
   float x, y, r, angle;

   x = (float) inx;
   y = (float) iny;

   /* first convert to polar coordinates */
   r = sqrt((x * x) + (y * y));
   if ( x > 0 && y < 0 )
      angle = PI + atan(x / y);
   else if ( x < 0 && y < 0)
      angle = PI + atan(x / y);
   else if (y == 0){
      if ( x < 0 )
         angle = 3 * PI / 2;
      else
         angle = PI / 2;
   }
   else if (x == 0){
      if ( y < 0 )
         angle = PI;
      else
         angle = 0;
   }
   else
      angle = atan(x / y);

   /* now just add on the angle */
   angle +=  (float) dir;

   /* now convert back to x and y */
   *outx = (int) (cos(angle) * r);
   *outy = (int) (sin(angle) * r);
}    

    /**********************************************************************/
    /* This reload the grids for the sounding variables in case the       */
    /* variables change or the time changes this can be used to change them*/
    /**********************************************************************/
    /* Input: dtx - context                                               */
    /**********************************************************************/


int reload_sounding_data( Display_Context dtx )
{
   if ( dtx->Sound.SoundTemp >= 0 ) {
      if (dtx->Sound.tgrid == NULL ) {
         dtx->Sound.tgrid = get_grid( dtx->Sound.SoundTempOwner,
                                      dtx->Sound.SoundTempOwner->CurTime, dtx->Sound.SoundTemp);
      }
      else {
         release_grid( dtx->Sound.SoundTempOwner, dtx->Sound.SoundTempOwner->CurTime,
                       dtx->Sound.SoundTemp, dtx->Sound.tgrid );
         dtx->Sound.tgrid = get_grid( dtx->Sound.SoundTempOwner, dtx->Sound.SoundTempOwner->CurTime,
                                      dtx->Sound.SoundTemp);
      }
   }

   if ( dtx->Sound.SoundDewpt >= 0){
      if (dtx->Sound.dgrid == NULL ) {
         dtx->Sound.dgrid = get_grid( dtx->Sound.SoundDewptOwner, dtx->Sound.SoundDewptOwner->CurTime,
                                      dtx->Sound.SoundDewpt);
      }
      else {
         release_grid( dtx->Sound.SoundDewptOwner, dtx->Sound.SoundDewptOwner->CurTime,
                       dtx->Sound.SoundDewpt, dtx->Sound.dgrid );
         dtx->Sound.dgrid = get_grid( dtx->Sound.SoundDewptOwner, dtx->Sound.SoundDewptOwner->CurTime,
                                      dtx->Sound.SoundDewpt);
      }
   }


   if ( dtx->Sound.SoundUWind >= 0 && dtx->Sound.SoundVWind >=0 ){
      if (dtx->Sound.ugrid == NULL ) {
         dtx->Sound.ugrid = get_grid( dtx->Sound.SoundUWindOwner, dtx->Sound.SoundUWindOwner->CurTime,
                                      dtx->Sound.SoundUWind);
      }
      else {
         release_grid( dtx->Sound.SoundUWindOwner, dtx->Sound.SoundUWindOwner->CurTime,
                       dtx->Sound.SoundUWind, dtx->Sound.ugrid );
         dtx->Sound.ugrid = get_grid( dtx->Sound.SoundUWindOwner, dtx->Sound.SoundUWindOwner->CurTime,
                                      dtx->Sound.SoundUWind);
      }
      if (dtx->Sound.vgrid == NULL ) {
         dtx->Sound.vgrid = get_grid( dtx->Sound.SoundVWindOwner, dtx->Sound.SoundVWindOwner->CurTime,
                                      dtx->Sound.SoundVWind);
      }
      else {
         release_grid( dtx->Sound.SoundVWindOwner, dtx->Sound.SoundVWindOwner->CurTime,
                       dtx->Sound.SoundVWind, dtx->Sound.vgrid );
         dtx->Sound.vgrid = get_grid( dtx->Sound.SoundVWindOwner, dtx->Sound.SoundVWindOwner->CurTime,
                                      dtx->Sound.SoundVWind);
      }
   }


   if ( dtx->Sound.SoundVar1 >= 0){
      if (dtx->Sound.var1grid == NULL ) {
         dtx->Sound.var1grid = get_grid( dtx->Sound.SoundVar1Owner, dtx->Sound.SoundVar1Owner->CurTime,
                                         dtx->Sound.SoundVar1);
      }
      else {
         release_grid( dtx->Sound.SoundVar1Owner, dtx->Sound.SoundVar1Owner->CurTime,
                       dtx->Sound.SoundVar1, dtx->Sound.var1grid );
         dtx->Sound.var1grid = get_grid( dtx->Sound.SoundVar1Owner, dtx->Sound.SoundVar1Owner->CurTime,
                                         dtx->Sound.SoundVar1);
      }
   }


   if ( dtx->Sound.SoundVar2 >= 0){
      if (dtx->Sound.var2grid == NULL ) {
         dtx->Sound.var2grid = get_grid( dtx->Sound.SoundVar2Owner, dtx->Sound.SoundVar2Owner->CurTime,
                                         dtx->Sound.SoundVar2);
      }
      else {
         release_grid( dtx->Sound.SoundVar2Owner, dtx->Sound.SoundVar2Owner->CurTime,
                       dtx->Sound.SoundVar2, dtx->Sound.var2grid );
         dtx->Sound.var2grid = get_grid( dtx->Sound.SoundVar2Owner, dtx->Sound.SoundVar2Owner->CurTime,
                                         dtx->Sound.SoundVar2);
      }
   }


   if ( dtx->Sound.SoundVar3 >= 0){
      if (dtx->Sound.var3grid == NULL ) {
         dtx->Sound.var3grid = get_grid( dtx->Sound.SoundVar3Owner, dtx->Sound.SoundVar3Owner->CurTime,
                                         dtx->Sound.SoundVar3);
      }
      else {
         release_grid( dtx->Sound.SoundVar3Owner, dtx->Sound.SoundVar3Owner->CurTime,
                       dtx->Sound.SoundVar3, dtx->Sound.var3grid );
         dtx->Sound.var3grid = get_grid( dtx->Sound.SoundVar3Owner, dtx->Sound.SoundVar3Owner->CurTime,
                                         dtx->Sound.SoundVar3);
      }
   }
   return 1;
}   



/* MJK 12.15.98 begin */
#define RESET_ELEV              -99999.0

static int draw_sounding_line (Display_Context dtx, GC gc, int x, int y,
                               float alt, float elev)
{

   static int           oldx = -1;
   static int           oldy = -1;
   static float         olda = RESET_ELEV;


   if (alt == RESET_ELEV) {
      oldx = oldy = -1;
      olda = RESET_ELEV;

      return 0;
   }

   if (alt >= elev) {

      if ((alt >= elev) && (olda < elev)) {
         float  frac;

         frac  = ((float) (elev - olda)) / ((float) (alt - olda));
         oldx += (x - oldx) * frac;
         oldy += (y - oldy) * frac;
      }

      draw_line_seg (dtx, dtx->Sound.soundwin, gc, oldx, oldy, x, y);
   }

   oldx = x;
   oldy = y;
   olda = alt;


   return 1;
}


    /**********************************************************************/
    /* This function puts everything together and draws the soundings and */
    /* vertical plot lines for a given time, and cursor location which    */
    /* is found in the context                                            */
    /**********************************************************************/
    /* Input: dtx - context                                               */
    /*        time - time step for whcih the sounding line is drawn       */
    /* Output: returns 1 is all goes well                                 */
    /**********************************************************************/



int draw_sounding( Display_Context dtx, int time )
{
   float windspeed, winddir;
   float row, col, lev;
   float alt, value;
   float frow, fcol;
   int x, y;
   int yo, yo2;
   int icol, irow;

   float        elev, lat, lon, hgt, *temp_save = NULL;
   int          barb_size;
   unsigned int line_width;
   Context      varownerctx, prvownerctx, var2ownerctx, prv2ownerctx;


   /* this will get the heights for the levels*/
   /* and put them in dtx->Sound.vertdata, and the first number in this array */
   /* tells how many levels there are */

   if (dtx->Sound.get_vert_data){
      dtx->Sound.get_vert_data = 0;
      dtx->Sound.vertdata = (float *) malloc(dtx->MaxNl * sizeof(float) + 1);
      if (!dtx->Sound.vertdata) {
         return 0;
      }
      dtx->Sound.vertdata[0] = (float) (dtx->MaxNl);
      for ( yo = 1; yo < dtx->MaxNl + 1; yo++){
         dtx->Sound.vertdata[yo] = grid_level_to_height( dtx, yo-1);
      }
   }
   XSync( SndDpy, 0);

   XCopyArea(SndDpy, dtx->Sound.soundpix, dtx->Sound.soundwin,
             dtx->Sound.box_gc,
             0, 0, dtx->Sound.sndwidth+2*BORDER,
             dtx->Sound.sndheight+95+2*BORDER+(BORDER/2), 0, 0);


   /*                               */
   /* draw the data into the window */
   /*                               */
   xyzPRIME_to_gridPRIME( dtx, dtx->CurTime, 1,
                dtx->CursorX, dtx->CursorY, dtx->CursorZ, &row, &col, &lev );
   irow = (int)(row + 0.5);
   icol = (int)(col + 0.5);
   frow = row - (float)irow;
   fcol = col - (float)icol;

   if ( fabs(frow) < 0.001){
      row = (float)(irow);
   }
   if ( fabs(fcol) < 0.001){
      col = (float)(icol);
   }

   elev = RESET_ELEV;
   if (dtx->TopoData) {
      xyzPRIME_to_geo (dtx, -1, -1, dtx->CursorX, dtx->CursorY, 0.0,
                       &lat, &lon, &hgt);
      elev = elevation (dtx, lat, lon, NULL) / 1000.0;
   }

/* 13May98  Phil McDonald */
   if ((dtx->Sound.PreviousSoundTemp < 0) && (dtx->Sound.SoundTemp >= 0) &&
       (dtx->Sound.tgrid != NULL))
       dtx->Sound.PreviousSoundTemp = dtx->Sound.SoundTemp;
   if ((dtx->Sound.PreviousSoundDewpt < 0) && (dtx->Sound.SoundDewpt >= 0) &&
       (dtx->Sound.dgrid != NULL))
       dtx->Sound.PreviousSoundDewpt = dtx->Sound.SoundDewpt;
   if ((dtx->Sound.PreviousSoundUWind < 0) && (dtx->Sound.SoundUWind >= 0) &&
       (dtx->Sound.ugrid != NULL))
       dtx->Sound.PreviousSoundUWind = dtx->Sound.SoundUWind;
   if ((dtx->Sound.PreviousSoundVWind < 0) && (dtx->Sound.SoundVWind >= 0) &&
       (dtx->Sound.vgrid != NULL))
       dtx->Sound.PreviousSoundVWind = dtx->Sound.SoundVWind;
   if ((dtx->Sound.PreviousSoundVar1 < 0) && (dtx->Sound.SoundVar1 >= 0) &&
       (dtx->Sound.var1grid != NULL))
       dtx->Sound.PreviousSoundVar1 = dtx->Sound.SoundVar1;
   if ((dtx->Sound.PreviousSoundVar2 < 0) && (dtx->Sound.SoundVar2 >= 0) &&
       (dtx->Sound.var2grid != NULL))
       dtx->Sound.PreviousSoundVar2 = dtx->Sound.SoundVar2;
   if ((dtx->Sound.PreviousSoundVar3 < 0) && (dtx->Sound.SoundVar3 >= 0) &&
       (dtx->Sound.var3grid != NULL))
       dtx->Sound.PreviousSoundVar3 = dtx->Sound.SoundVar3;



   /* draw the temperature first if it is given*/

   if (dtx->Sound.SoundTemp >= 0){
      varownerctx = dtx->Sound.SoundTempOwner;
      prvownerctx = dtx->Sound.PreviousSoundTempOwner;

      if ( (dtx->Sound.SoundTemp != dtx->Sound.PreviousSoundTemp) &&
           (varownerctx != prvownerctx) && (dtx->Sound.tgrid != NULL) ){
         release_grid( prvownerctx, time,
                       dtx->Sound.PreviousSoundTemp, dtx->Sound.tgrid );
         dtx->Sound.tgrid = NULL;
      }
      dtx->Sound.PreviousSoundTemp      = dtx->Sound.SoundTemp;
      dtx->Sound.PreviousSoundTempOwner = varownerctx;

      if (dtx->Sound.tgrid == NULL ) {
         dtx->Sound.tgrid = get_grid( varownerctx, time, dtx->Sound.SoundTemp);
         if (dtx->Sound.tgrid == NULL) return 0;
      }

      if (varownerctx->GridSameAsGridPRIME){
         yo =  extract_sound( dtx, dtx->Sound.tgrid, dtx->Sound.SoundTemp,
                              varownerctx->Nr, varownerctx->Nc,
                              varownerctx->Nl[dtx->Sound.SoundTemp],
                              varownerctx->LowLev[dtx->Sound.SoundTemp],
                              row, col );
      }
      else{
         yo = extract_soundPRIME( varownerctx, dtx->Sound.SoundTemp,
                                  varownerctx->Nr, varownerctx->Nc,
                                  varownerctx->Nl[dtx->Sound.SoundTemp],
                                  varownerctx->LowLev[dtx->Sound.SoundTemp],
                                  row, col );
      }

      draw_sounding_line (dtx, dtx->Sound.Tempgc, -1, -1, RESET_ELEV, elev);

      for (yo=0; yo < varownerctx->Nl[dtx->Sound.SoundTemp]; yo++){
         alt   = gridlevel_to_height( varownerctx, yo);
         value = dtx->Sound.soundline[yo];
         data_to_xy(dtx, alt, value, &x, &y);

         draw_sounding_line (dtx, dtx->Sound.Tempgc, x, y, alt, elev);
      }

/* 13Oct97  Phil McDonald       save temp to compare with dewpt */
      temp_save            = dtx->Sound.soundline;
      dtx->Sound.soundline = NULL;

   }
   else {
      if (dtx->Sound.tgrid != NULL ){
         release_grid( dtx->Sound.PreviousSoundTempOwner, time,
                       dtx->Sound.PreviousSoundTemp, dtx->Sound.tgrid );
         dtx->Sound.tgrid = NULL;
      }
   }



   /* now draw the dewpoint line if its there */

   if (dtx->Sound.SoundDewpt >= 0){
      varownerctx = dtx->Sound.SoundDewptOwner;
      prvownerctx = dtx->Sound.PreviousSoundDewptOwner;

      if ( (dtx->Sound.SoundDewpt != dtx->Sound.PreviousSoundDewpt) &&
           (varownerctx != prvownerctx) && (dtx->Sound.dgrid != NULL) ){
         release_grid( prvownerctx, time,
                       dtx->Sound.PreviousSoundDewpt, dtx->Sound.dgrid );
         dtx->Sound.dgrid = NULL;
      }
      dtx->Sound.PreviousSoundDewpt      = dtx->Sound.SoundDewpt;
      dtx->Sound.PreviousSoundDewptOwner = varownerctx;

      if (dtx->Sound.dgrid == NULL ) {
         dtx->Sound.dgrid = get_grid( varownerctx, time, dtx->Sound.SoundDewpt);
         if (dtx->Sound.dgrid == NULL) return 0;
      }

      if (varownerctx->GridSameAsGridPRIME){
         yo =  extract_sound( dtx, dtx->Sound.dgrid, dtx->Sound.SoundDewpt,
                              varownerctx->Nr, varownerctx->Nc,
                              varownerctx->Nl[dtx->Sound.SoundDewpt],
                              varownerctx->LowLev[dtx->Sound.SoundDewpt],
                              row, col );
      }
      else{
         yo = extract_soundPRIME( varownerctx,  dtx->Sound.SoundDewpt,
                                  varownerctx->Nr, varownerctx->Nc,
                                  varownerctx->Nl[dtx->Sound.SoundDewpt],
                                  varownerctx->LowLev[dtx->Sound.SoundDewpt],
                                  row, col );
      }

      draw_sounding_line (dtx, dtx->Sound.Dewptgc, -1, -1, RESET_ELEV, elev);

      for (yo=0; yo < varownerctx->Nl[dtx->Sound.SoundDewpt]; yo++){
         alt   = gridlevel_to_height(varownerctx, yo);
         value = dtx->Sound.soundline[yo];

/* 13Oct97  Phil McDonald       ensure that dewpt doesn't exceed temp */
         if (temp_save != NULL)
         {
            if ((!IS_MISSING(temp_save[yo])) && (!IS_MISSING(value)))
            {
               if (value > temp_save[yo]) value = temp_save[yo];
            }
         }

         data_to_xy(dtx, alt, value, &x, &y);

         draw_sounding_line (dtx, dtx->Sound.Dewptgc, x, y, alt, elev);
      }

   }
   else {
      if (dtx->Sound.dgrid != NULL ){
         release_grid( dtx->Sound.PreviousSoundDewptOwner, time,
                       dtx->Sound.PreviousSoundDewpt, dtx->Sound.dgrid );
         dtx->Sound.dgrid = NULL;
      }
   }

/* 13Oct97  Phil McDonald */
   if (temp_save != NULL) free (temp_save), temp_save = NULL;



   /* now draw all the wind barb stuff */

   if ((dtx->Sound.SoundUWind >= 0) && (dtx->Sound.SoundVWind >= 0 )){
      varownerctx  = dtx->Sound.SoundUWindOwner;
      prvownerctx  = dtx->Sound.PreviousSoundUWindOwner;
      var2ownerctx = dtx->Sound.SoundVWindOwner;
      prv2ownerctx = dtx->Sound.PreviousSoundVWindOwner;

      if ( (dtx->Sound.SoundUWind != dtx->Sound.PreviousSoundUWind) &&
           (varownerctx != prvownerctx) && (dtx->Sound.ugrid != NULL) ){
         release_grid( prvownerctx, time,
                       dtx->Sound.PreviousSoundUWind, dtx->Sound.ugrid );
         dtx->Sound.ugrid = NULL;
      }
      dtx->Sound.PreviousSoundUWind      = dtx->Sound.SoundUWind;
      dtx->Sound.PreviousSoundUWindOwner = varownerctx;

      if ( (dtx->Sound.SoundVWind != dtx->Sound.PreviousSoundVWind) &&
           (var2ownerctx != prv2ownerctx) && (dtx->Sound.vgrid != NULL) ){
         release_grid( prv2ownerctx, time,
                       dtx->Sound.PreviousSoundVWind, dtx->Sound.vgrid );
         dtx->Sound.vgrid = NULL;
      }
      dtx->Sound.PreviousSoundVWind      = dtx->Sound.SoundVWind;
      dtx->Sound.PreviousSoundVWindOwner = var2ownerctx;

      if( dtx->Sound.ugrid == NULL ) {
         dtx->Sound.ugrid = get_grid( varownerctx, time,
                                      dtx->Sound.SoundUWind);
         if (dtx->Sound.ugrid == NULL) return 0;
      }
      if( dtx->Sound.vgrid == NULL ) {
         dtx->Sound.vgrid = get_grid( var2ownerctx, time,
                                      dtx->Sound.SoundVWind);
         if (dtx->Sound.vgrid == NULL) {
            release_grid( varownerctx, time,
                          dtx->Sound.SoundUWind, dtx->Sound.ugrid );
            return 0;
         }
      }

      if (varownerctx->GridSameAsGridPRIME){
         yo = extract_wind( dtx, dtx->Sound.ugrid, dtx->Sound.vgrid,
                            dtx->Sound.SoundUWind, dtx->Sound.SoundVWind,
                            dtx->Nr, dtx->Nc, dtx->Nl,
                            dtx->LowLev, row, col );
      }
      else{
         yo = extract_windPRIME( varownerctx,
                                 dtx->Sound.SoundUWind, dtx->Sound.SoundVWind,
                                 varownerctx->Nr, varownerctx->Nr,
                                 varownerctx->Nl[dtx->Sound.SoundUWind],
                                 varownerctx->LowLev[dtx->Sound.SoundUWind],
                                 row, col);
      }

/* 20Oct97  Phil McDonald       calc a suitable barb size and line width */
      barb_size = 0;
      alt = grid_level_to_height( dtx, 0);
      data_to_xy( dtx, alt, 273, &x, &yo2);
      for (yo=1; yo < varownerctx->Nl[dtx->Sound.SoundUWind]; yo++){
         alt = grid_level_to_height( dtx, yo);
         data_to_xy( dtx, alt, 273, &x, &y);
         barb_size += (yo2 - y);
         yo2 = y;
      }
      barb_size = (barb_size * 5) /
                  ((varownerctx->Nl[dtx->Sound.SoundUWind] - 1) * 3);
      if (barb_size > SOUND_BARB_SIZE) barb_size = SOUND_BARB_SIZE;
      line_width = (barb_size < 40) ? 1 : 2;
      XSetLineAttributes (SndDpy, dtx->Sound.barb_gc, line_width,
                                  LineSolid, CapButt, JoinRound);

      for (yo=0; yo < varownerctx->Nl[dtx->Sound.SoundUWind]; yo++){
         alt = gridlevel_to_height(varownerctx, yo);

         if (alt >= elev) {
            float u, v;

            u = dtx->Sound.uwindline[yo];
            v = dtx->Sound.vwindline[yo];

            windspeed = sqrt((u * u) + (v * v));

            if (windspeed < 300.0) {
               winddir = (PI / 2.0) - atan2 (v, u);
               if (winddir < 0.0) winddir += PI * 2.0;

               make_a_barb (dtx, windspeed, winddir+PI/2, alt, barb_size);
            }
         }
      }
   }
   else {
      if (dtx->Sound.ugrid != NULL ) {
         release_grid( dtx->Sound.PreviousSoundUWindOwner,
                       time, dtx->Sound.PreviousSoundUWind, dtx->Sound.ugrid);
         dtx->Sound.ugrid = NULL;
      }
      if (dtx->Sound.vgrid != NULL ) {
         release_grid( dtx->Sound.PreviousSoundVWindOwner,
                       time, dtx->Sound.PreviousSoundVWind, dtx->Sound.vgrid);
         dtx->Sound.vgrid = NULL;
      }
   }



   /* now draw the different vertical variables */

   if( dtx->Sound.SoundVar1 >= 0){
      varownerctx  = dtx->Sound.SoundVar1Owner;
      prvownerctx  = dtx->Sound.PreviousSoundVar1Owner;

      if ( (dtx->Sound.SoundVar1 != dtx->Sound.PreviousSoundVar1) &&
           (varownerctx != prvownerctx) && (dtx->Sound.var1grid != NULL) ){
         release_grid( prvownerctx, time,
                       dtx->Sound.PreviousSoundVar1, dtx->Sound.var1grid );
         dtx->Sound.var1grid = NULL;
      }
      dtx->Sound.PreviousSoundVar1      = dtx->Sound.SoundVar1;
      dtx->Sound.PreviousSoundVar1Owner = varownerctx;

      if (dtx->Sound.var1grid == NULL ){
         dtx->Sound.var1grid = get_grid( varownerctx, time,
                                         dtx->Sound.SoundVar1);
         if (dtx->Sound.var1grid == NULL) return 0;
      }

      if (varownerctx->GridSameAsGridPRIME){
         yo= extract_sound( dtx, dtx->Sound.var1grid, dtx->Sound.SoundVar1,
                            dtx->Nr, dtx->Nc, dtx->Nl,
                            dtx->LowLev, row, col );
      }
      else{
         yo= extract_soundPRIME(varownerctx, dtx->Sound.SoundVar1,
                                varownerctx->Nr, varownerctx->Nc,
                                varownerctx->Nl[dtx->Sound.SoundVar1],
                                varownerctx->LowLev[dtx->Sound.SoundVar1],
                                row, col );
      }

      if (XLoadFont(SndDpy, dtx->SoundFontName)){
         XSetFont(SndDpy, dtx->Sound.var1_gc,
                  XLoadFont(SndDpy, dtx->SoundFontName));
      }
      draw_sounding_line (dtx, dtx->Sound.var1_gc, -1, -1, RESET_ELEV, elev);

      for (yo=0; yo < varownerctx->Nl[dtx->Sound.SoundVar1]; yo++){
         alt   = gridlevel_to_height(varownerctx, yo);
         value = dtx->Sound.soundline[yo];
         vardata_to_xy(dtx, alt, value,
                       varownerctx->MinVal[dtx->Sound.SoundVar1],
                       varownerctx->MaxVal[dtx->Sound.SoundVar1], &x, &y);

         draw_sounding_line (dtx, dtx->Sound.var1_gc, x, y, alt, elev);
      }

   }
   else {
      if (dtx->Sound.var1grid != NULL ){
         release_grid(dtx->Sound.PreviousSoundVar1Owner, time,
                      dtx->Sound.PreviousSoundVar1, dtx->Sound.var1grid);
         dtx->Sound.var1grid = NULL;
      }
   }



   if( dtx->Sound.SoundVar2 >= 0){
      varownerctx  = dtx->Sound.SoundVar2Owner;
      prvownerctx  = dtx->Sound.PreviousSoundVar2Owner;

      if ( (dtx->Sound.SoundVar2 != dtx->Sound.PreviousSoundVar2) &&
           (varownerctx != prvownerctx) && (dtx->Sound.var2grid != NULL) ){
         release_grid( prvownerctx, time,
                       dtx->Sound.PreviousSoundVar2, dtx->Sound.var2grid );
         dtx->Sound.var2grid = NULL;
      }
      dtx->Sound.PreviousSoundVar2      = dtx->Sound.SoundVar2;
      dtx->Sound.PreviousSoundVar2Owner = varownerctx;

      if (dtx->Sound.var2grid == NULL ){
         dtx->Sound.var2grid = get_grid( varownerctx, time,
                                         dtx->Sound.SoundVar2);
         if (dtx->Sound.var2grid == NULL) return 0;
      }

      if (varownerctx->GridSameAsGridPRIME){
         yo= extract_sound( dtx, dtx->Sound.var2grid, dtx->Sound.SoundVar2,
                            dtx->Nr, dtx->Nc, dtx->Nl,
                            dtx->LowLev, row, col );
      }
      else{
         yo= extract_soundPRIME(varownerctx, dtx->Sound.SoundVar2,
                                varownerctx->Nr, varownerctx->Nc,
                                varownerctx->Nl[dtx->Sound.SoundVar2],
                                varownerctx->LowLev[dtx->Sound.SoundVar2],
                                row, col );
      }

      if (XLoadFont(SndDpy, dtx->SoundFontName)){
         XSetFont(SndDpy, dtx->Sound.var2_gc,
                  XLoadFont(SndDpy, dtx->SoundFontName));
      }
      draw_sounding_line (dtx, dtx->Sound.var2_gc, -1, -1, RESET_ELEV, elev);

      for (yo=0; yo < varownerctx->Nl[dtx->Sound.SoundVar2]; yo++){
         alt   = gridlevel_to_height(varownerctx, yo);
         value = dtx->Sound.soundline[yo];
         vardata_to_xy(dtx, alt, value,
                       varownerctx->MinVal[dtx->Sound.SoundVar2],
                       varownerctx->MaxVal[dtx->Sound.SoundVar2], &x, &y);

         draw_sounding_line (dtx, dtx->Sound.var2_gc, x, y, alt, elev);
      }

   }
   else {
      if (dtx->Sound.var2grid != NULL ){
         release_grid(dtx->Sound.PreviousSoundVar2Owner, time,
                      dtx->Sound.PreviousSoundVar2, dtx->Sound.var2grid);
         dtx->Sound.var2grid = NULL;
      }
   }



   if( dtx->Sound.SoundVar3 >= 0){
      varownerctx  = dtx->Sound.SoundVar3Owner;
      prvownerctx  = dtx->Sound.PreviousSoundVar3Owner;

      if ( (dtx->Sound.SoundVar3 != dtx->Sound.PreviousSoundVar3) &&
           (varownerctx != prvownerctx) && (dtx->Sound.var3grid != NULL) ){
         release_grid( prvownerctx, time,
                       dtx->Sound.PreviousSoundVar3, dtx->Sound.var3grid );
         dtx->Sound.var3grid = NULL;
      }
      dtx->Sound.PreviousSoundVar3      = dtx->Sound.SoundVar3;
      dtx->Sound.PreviousSoundVar3Owner = varownerctx;

      if (dtx->Sound.var3grid == NULL ){
         dtx->Sound.var3grid = get_grid( varownerctx, time,
                                         dtx->Sound.SoundVar3);
         if (dtx->Sound.var3grid == NULL) return 0;
      }

      if (varownerctx->GridSameAsGridPRIME){
         yo= extract_sound( dtx, dtx->Sound.var3grid, dtx->Sound.SoundVar3,
                            dtx->Nr, dtx->Nc, dtx->Nl,
                            dtx->LowLev, row, col );
      }
      else{
         yo= extract_soundPRIME(varownerctx, dtx->Sound.SoundVar3,
                                varownerctx->Nr, varownerctx->Nc,
                                varownerctx->Nl[dtx->Sound.SoundVar3],
                                varownerctx->LowLev[dtx->Sound.SoundVar3],
                                row, col );
      }

      if (XLoadFont(SndDpy, dtx->SoundFontName)){
         XSetFont(SndDpy, dtx->Sound.var3_gc,
                  XLoadFont(SndDpy, dtx->SoundFontName));
      }
      draw_sounding_line (dtx, dtx->Sound.var3_gc, -1, -1, RESET_ELEV, elev);

      for (yo=0; yo < varownerctx->Nl[dtx->Sound.SoundVar3]; yo++){
         alt   = gridlevel_to_height(varownerctx, yo);
         value = dtx->Sound.soundline[yo];
         vardata_to_xy(dtx, alt, value,
                       varownerctx->MinVal[dtx->Sound.SoundVar3],
                       varownerctx->MaxVal[dtx->Sound.SoundVar3], &x, &y);

         draw_sounding_line (dtx, dtx->Sound.var3_gc, x, y, alt, elev);
      }

   }
   else {
      if (dtx->Sound.var3grid != NULL ){
         release_grid(dtx->Sound.PreviousSoundVar3Owner, time,
                      dtx->Sound.PreviousSoundVar3, dtx->Sound.var3grid);
         dtx->Sound.var3grid = NULL;
      }
   }

   return 1;
}

#undef RESET_ELEV


    /**********************************************************************/
    /* This converts vertical plot data to x and y's matching the soundwin*/
    /**********************************************************************/
    /* Input: dtx - context                                               */
    /*        alt - altitude of data point in                             */
    /*        value - value of the data point, to be plotted on the y axis*/
    /*        min - minimum value for the data point                      */
    /*        max - maximum value for the data point                      */
    /* Output: x, y - the coordinates to be plotted on soundwin           */
    /**********************************************************************/



void vardata_to_xy(Display_Context dtx, float alt, float value,
                   float min, float max, int *x, int *y)
{

   if (dtx->Sound.samestepflag){
      min = dtx->Sound.samestepmin;
      max = dtx->Sound.samestepmax;
   }

   *x = dtx->Sound.sndwidth * (value - min) / (max - min);

   data_to_y (dtx, alt, y);
}

#ifdef DONOTUSE



    /**********************************************************************/
    /* This function puts everything together and draws the soundings and */
    /* vertical plot lines for a given time, and cursor location which    */
    /* is found in the context                                            */
    /**********************************************************************/
    /* Input: dtx - context                                               */
    /*        time - time step for whcih the sounding line is drawn       */
    /* Output: returns 1 is all goes well                                 */
    /**********************************************************************/



int draw_sounding( Display_Context dtx, int time )
{
   float windspeed, winddir;
   float row, col, lev;
   float alt, temp, value;
   float frow, fcol;
   int oldx = 9999;
   int oldy = 9999;
   int x = 9999;
   int y = 9999;
   int yo, yo2;
   int icol, irow;
   XGCValues values;
   float spd;
   Font varfont;


   /* this will get the heights for the levels*/
   /* and put them in dtx->Sound.vertdata, and the first number in this array */
   /* tells how many levels there are */

   if (dtx->Sound.get_vert_data){
      dtx->Sound.get_vert_data = 0;
      dtx->Sound.vertdata = (float *) malloc(dtx->MaxNl * sizeof(float) + 1);
      if (!dtx->Sound.vertdata) {
         return NULL;
      }
      dtx->Sound.vertdata[0] = (float) (dtx->MaxNl); 
      for ( yo = 1; yo < dtx->MaxNl + 1; yo++){
         dtx->Sound.vertdata[yo] = grid_level_to_height( dtx, yo-1);
      }
   }
   XSync( SndDpy, 0);

   XCopyArea(SndDpy, dtx->Sound.soundpix, dtx->Sound.soundwin, dtx->Sound.box_gc,
             0, 0, dtx->Sound.sndwidth+2*BORDER,
             dtx->Sound.sndheight+95+2*BORDER+(BORDER/2), 0, 0);


   /*                               */
   /* draw the data into the window */
   /*                               */
   xyzPRIME_to_gridPRIME( dtx, dtx->CurTime, 1,
                dtx->CursorX, dtx->CursorY, dtx->CursorZ, &row, &col, &lev );
   irow = (int)(row + 0.5);
   icol = (int)(col + 0.5);
   frow = row - (float)irow;
   fcol = col - (float)icol;

   if ( fabs(frow) < 0.001){
      row = (float)(irow);
   }
   if ( fabs(fcol) < 0.001){
      col = (float)(icol);
   }
 

   /* draw the temperature first if it is given*/
   if (dtx->Sound.SoundTemp >= 0){
      if ( (dtx->Sound.SoundTemp != dtx->Sound.PreviousSoundTemp) &&
           (dtx->Sound.SoundTempOwner != dtx->Sound.PreviousSoundTempOwner) &&
           dtx->Sound.tgrid != NULL ){
         release_grid( dtx->Sound.PreviousSoundTempOwner, time,
                       dtx->Sound.PreviousSoundTemp, dtx->Sound.tgrid );
         dtx->Sound.tgrid = get_grid( dtx->Sound.SoundTempOwner, time, dtx->Sound.SoundTemp);
      }      
      dtx->Sound.PreviousSoundTemp = dtx->Sound.SoundTemp;
      dtx->Sound.PreviousSoundTempOwner = dtx->Sound.SoundTempOwner;
      if (dtx->Sound.tgrid == NULL ) {
         dtx->Sound.tgrid = get_grid( dtx->Sound.SoundTempOwner, time, dtx->Sound.SoundTemp);
      }
      if (!dtx->Sound.tgrid){
         return NULL;
      }
      if (dtx->Sound.SoundTempOwner->GridSameAsGridPRIME){
         yo =  extract_sound( dtx, dtx->Sound.tgrid, dtx->Sound.SoundTemp,
                              dtx->Sound.SoundTempOwner->Nr, dtx->Sound.SoundTempOwner->Nc,
                              dtx->Sound.SoundTempOwner->Nl[dtx->Sound.SoundTemp],
                              dtx->Sound.SoundTempOwner->LowLev[dtx->Sound.SoundTemp], row, col );
      }
      else{
         yo = extract_soundPRIME( dtx->Sound.SoundTempOwner, dtx->Sound.SoundTemp,
                              dtx->Sound.SoundTempOwner->Nr, dtx->Sound.SoundTempOwner->Nc,
                              dtx->Sound.SoundTempOwner->Nl[dtx->Sound.SoundTemp],
                              dtx->Sound.SoundTempOwner->LowLev[dtx->Sound.SoundTemp], row, col );
      }
      XSetLineAttributes(SndDpy, dtx->Sound.Tempgc, 2, LineSolid, CapRound, JoinRound);
      for (yo=0; yo < dtx->Sound.SoundTempOwner->Nl[dtx->Sound.SoundTemp]; yo++){
         alt = gridlevel_to_height( dtx->Sound.SoundTempOwner, yo);
         temp = dtx->Sound.soundline[yo];
         oldx = x;
         oldy = y;
         data_to_xy(dtx, alt, temp, &x, &y);
         if ( alt < -0.2 ) {
            x = -9999;
         }
         if ((x <= dtx->Sound.sndwidth) && (y <= dtx->Sound.sndheight)
            && (oldx <= dtx->Sound.sndwidth) && (oldy <= dtx->Sound.sndheight)
            && (x >= 0) && (y >= 0) && (oldx >= 0) && (oldy >= 0)){
            XDrawLine(SndDpy, dtx->Sound.soundwin, dtx->Sound.Tempgc, oldx+BORDER,
                      oldy+BORDER-HEBGBS, x +BORDER, y +BORDER-HEBGBS);
         }
      }
   }
   else {
      if (dtx->Sound.tgrid != NULL ){
         release_grid( dtx->Sound.PreviousSoundTempOwner, time, dtx->Sound.PreviousSoundTemp, dtx->Sound.tgrid );
         dtx->Sound.tgrid = NULL;
      }
   }

   /* now draw the dewpoint line if its there */

   x = -1;
   y = -1;
   oldx = -1;
   oldy = -1;
   if (dtx->Sound.SoundDewpt >= 0){
      if ( (dtx->Sound.SoundDewpt != dtx->Sound.PreviousSoundDewpt) &&
           (dtx->Sound.SoundDewptOwner != dtx->Sound.PreviousSoundDewptOwner) &&
           dtx->Sound.dgrid != NULL ){
         release_grid( dtx->Sound.PreviousSoundDewptOwner, time,
                       dtx->Sound.PreviousSoundDewpt, dtx->Sound.dgrid ); 
         dtx->Sound.dgrid = get_grid( dtx->Sound.SoundDewptOwner, time, dtx->Sound.SoundDewpt);
      }
      dtx->Sound.PreviousSoundDewpt = dtx->Sound.SoundDewpt;
      dtx->Sound.PreviousSoundDewptOwner = dtx->Sound.SoundDewptOwner;

      if (dtx->Sound.dgrid == NULL ) {
         dtx->Sound.dgrid = get_grid( dtx->Sound.SoundDewptOwner, time, dtx->Sound.SoundDewpt);
      }
      if (!dtx->Sound.dgrid){
         return NULL;
      }
      if (dtx->Sound.SoundDewptOwner->GridSameAsGridPRIME){
         yo =  extract_sound( dtx, dtx->Sound.dgrid, dtx->Sound.SoundDewpt,
                              dtx->Sound.SoundDewptOwner->Nr, dtx->Sound.SoundDewptOwner->Nc,
                              dtx->Sound.SoundDewptOwner->Nl[dtx->Sound.SoundDewpt],
                              dtx->Sound.SoundDewptOwner->LowLev[dtx->Sound.SoundDewpt], row, col );
      }
      else{
         yo = extract_soundPRIME( dtx->Sound.SoundDewptOwner,  dtx->Sound.SoundDewpt,
                              dtx->Sound.SoundDewptOwner->Nr, dtx->Sound.SoundDewptOwner->Nc,
                              dtx->Sound.SoundDewptOwner->Nl[dtx->Sound.SoundDewpt],
                              dtx->Sound.SoundDewptOwner->LowLev[dtx->Sound.SoundDewpt], row, col );
      }
      XSetLineAttributes(SndDpy, dtx->Sound.Dewptgc, 2, LineSolid, CapRound, JoinRound);
      for (yo=0; yo < dtx->Sound.SoundDewptOwner->Nl[dtx->Sound.SoundDewpt]; yo++){
         alt = gridlevel_to_height(dtx->Sound.SoundDewptOwner, yo);
         temp = dtx->Sound.soundline[yo];
         oldx = x;
         oldy = y;
         data_to_xy(dtx, alt, temp, &x, &y);
         if ( alt < -0.2 ) {
            x = -9999;
         }
         if ((x <= dtx->Sound.sndwidth) && (y <= dtx->Sound.sndheight)
            && (oldx <= dtx->Sound.sndwidth) && (oldy <= dtx->Sound.sndheight)
            && (x >= 0) && (y >= 0) && (oldx >= 0) && (oldy >= 0)){
            XDrawLine(SndDpy, dtx->Sound.soundwin, dtx->Sound.Dewptgc, oldx+BORDER,
                      oldy+BORDER-HEBGBS, x +BORDER, y +BORDER-HEBGBS);
         }
      }
   }
   else {
      if (dtx->Sound.dgrid != NULL ){
         release_grid( dtx->Sound.PreviousSoundDewptOwner, time, dtx->Sound.PreviousSoundDewpt, dtx->Sound.dgrid );
         dtx->Sound.dgrid = NULL;
      }
   }

   /* now draw all the wind barb stuff */
  
   if ((dtx->Sound.SoundUWind >= 0) && (dtx->Sound.SoundVWind >= 0 )){

      if ( (dtx->Sound.SoundUWind != dtx->Sound.PreviousSoundUWind) &&
           (dtx->Sound.SoundUWindOwner != dtx->Sound.PreviousSoundUWindOwner) &&
           dtx->Sound.ugrid != NULL ){
         release_grid( dtx->Sound.PreviousSoundUWindOwner, time,
                       dtx->Sound.PreviousSoundUWind, dtx->Sound.ugrid );
         dtx->Sound.ugrid = get_grid( dtx->Sound.SoundUWindOwner, time, dtx->Sound.SoundUWind);
      }

      if ( (dtx->Sound.SoundVWind != dtx->Sound.PreviousSoundVWind) &&
           (dtx->Sound.SoundVWindOwner != dtx->Sound.PreviousSoundVWindOwner) &&
           dtx->Sound.vgrid != NULL ){
         release_grid( dtx->Sound.PreviousSoundVWindOwner,
                       time, dtx->Sound.PreviousSoundVWind, dtx->Sound.vgrid );
         dtx->Sound.vgrid = get_grid( dtx->Sound.SoundVWindOwner, time, dtx->Sound.SoundVWind);
      }
      dtx->Sound.PreviousSoundVWind = dtx->Sound.SoundVWind;
      dtx->Sound.PreviousSoundUWind = dtx->Sound.SoundUWind;
      dtx->Sound.PreviousSoundVWindOwner = dtx->Sound.SoundVWindOwner;
      dtx->Sound.PreviousSoundUWindOwner = dtx->Sound.SoundUWindOwner;


      if( dtx->Sound.ugrid == NULL ) {
         dtx->Sound.ugrid = get_grid( dtx->Sound.PreviousSoundUWindOwner, time,
                                      dtx->Sound.SoundUWind);
      }
      if( dtx->Sound.vgrid == NULL ) {
         dtx->Sound.vgrid = get_grid( dtx->Sound.PreviousSoundVWindOwner, time,
                                      dtx->Sound.SoundVWind);
      }
      if ((!dtx->Sound.ugrid) || (!dtx->Sound.vgrid)){
         return NULL;
      }
      if (dtx->Sound.SoundUWindOwner->GridSameAsGridPRIME){
         yo = extract_wind( dtx, dtx->Sound.ugrid, dtx->Sound.vgrid, dtx->Sound.SoundUWind,
                            dtx->Sound.SoundVWind, dtx->Nr, dtx->Nc,
                            dtx->Nl,
                            dtx->LowLev, row, col );
      }
      else{
         yo = extract_windPRIME( dtx->Sound.SoundUWindOwner, dtx->Sound.SoundUWind,
                                 dtx->Sound.SoundVWind, dtx->Sound.SoundUWindOwner->Nr,
                                 dtx->Sound.SoundUWindOwner->Nc, 
                                 dtx->Sound.SoundUWindOwner->Nl[dtx->Sound.SoundUWind],
                                 dtx->Sound.SoundUWindOwner->LowLev[dtx->Sound.SoundUWind],
                                 row, col);
      }
      for (yo=0; yo < dtx->Sound.SoundUWindOwner->Nl[dtx->Sound.SoundUWind]; yo++){
         alt = gridlevel_to_height(dtx->Sound.SoundUWindOwner, yo);
         windspeed = sqrt((dtx->Sound.uwindline[yo] * dtx->Sound.uwindline[yo]) +
                          (dtx->Sound.vwindline[yo] * dtx->Sound.vwindline[yo]));
         if (dtx->Sound.uwindline[yo] > 0 && dtx->Sound.vwindline[yo] < 0 )
            winddir = PI + atan(dtx->Sound.uwindline[yo]/dtx->Sound.vwindline[yo]);
         else if (dtx->Sound.uwindline[yo] < 0 && dtx->Sound.vwindline[yo] < 0)
            winddir = PI + atan(dtx->Sound.uwindline[yo]/dtx->Sound.vwindline[yo]);
         else if (dtx->Sound.uwindline[yo] > 0 && dtx->Sound.vwindline[yo] > 0)
            winddir = atan(dtx->Sound.uwindline[yo]/dtx->Sound.vwindline[yo]);
         else if (dtx->Sound.uwindline[yo] < 0 && dtx->Sound.vwindline[yo] > 0)
            winddir = atan(dtx->Sound.uwindline[yo]/dtx->Sound.vwindline[yo]);
         else if ( dtx->Sound.uwindline[yo] == 0 && dtx->Sound.vwindline[yo] > 0)
            winddir = 0;
         else if ( dtx->Sound.uwindline[yo] == 0 && dtx->Sound.vwindline[yo] < 0)
            winddir = PI;
         else if (dtx->Sound.uwindline[yo] > 0 && dtx->Sound.vwindline[yo] == 0)
            winddir = PI / 2;
         else if (dtx->Sound.uwindline[yo] < 0 && dtx->Sound.vwindline[yo] == 0)
            winddir = 3 * PI / 2;
         if(windspeed < 300.0){
            make_a_barb ( dtx, windspeed, winddir+PI/2, alt );
         }
      }
   }
   else {
      if (dtx->Sound.ugrid != NULL ) {
         release_grid( dtx->Sound.PreviousSoundUWindOwner,
                       time, dtx->Sound.PreviousSoundUWind, dtx->Sound.ugrid);
         dtx->Sound.ugrid = NULL;
      }
      if (dtx->Sound.vgrid != NULL ) {
         release_grid( dtx->Sound.PreviousSoundVWindOwner,
                       time, dtx->Sound.PreviousSoundVWind, dtx->Sound.vgrid);
         dtx->Sound.vgrid = NULL;
      }
   }
  
   /* now draw the different vertical variables */
   oldx = -1;
   oldy = -1;
   x    = -1;
   y    = -1;
   if( dtx->Sound.SoundVar1 >= 0){

      if ( (dtx->Sound.SoundVar1Owner != dtx->Sound.PreviousSoundVar1Owner) &&
           (dtx->Sound.SoundVar1 != dtx->Sound.PreviousSoundVar1) &&
           dtx->Sound.var1grid != NULL ){
         release_grid( dtx->Sound.PreviousSoundVar1Owner, time,
                       dtx->Sound.PreviousSoundVar1, dtx->Sound.var1grid );
         dtx->Sound.var1grid = get_grid( dtx->Sound.SoundVar1Owner, time, dtx->Sound.SoundVar1);
      }
      dtx->Sound.PreviousSoundVar1 = dtx->Sound.SoundVar1;
      dtx->Sound.PreviousSoundVar1Owner = dtx->Sound.SoundVar1Owner;

      if (!dtx->Sound.var1_gc){
         dtx->Sound.var1_gc = make_gc(dtx, 0, 255, 0, 0, 0, 0, 2);
         if  (XLoadFont(SndDpy, dtx->SoundFontName)){
            XSetFont(SndDpy, dtx->Sound.var1_gc, XLoadFont(SndDpy, dtx->SoundFontName));
         }
      }
      if (dtx->Sound.var1grid == NULL ){
         dtx->Sound.var1grid = get_grid( dtx->Sound.SoundVar1Owner, time, dtx->Sound.SoundVar1);
      }
      if (!dtx->Sound.var1grid){
         return NULL;
      }
      if (dtx->Sound.SoundVar1Owner->GridSameAsGridPRIME){
         yo= extract_sound( dtx, dtx->Sound.var1grid, dtx->Sound.SoundVar1, dtx->Nr,
                         dtx->Nc, dtx->Nl,
                         dtx->LowLev, row, col );
      }
      else{
         yo= extract_soundPRIME(dtx->Sound.SoundVar1Owner, dtx->Sound.SoundVar1,
                              dtx->Sound.SoundVar1Owner->Nr, dtx->Sound.SoundVar1Owner->Nc,
                              dtx->Sound.SoundVar1Owner->Nl[dtx->Sound.SoundVar1],
                              dtx->Sound.SoundVar1Owner->LowLev[dtx->Sound.SoundVar1], row, col );
      }

      for (yo=0; yo < dtx->Sound.SoundVar1Owner->Nl[dtx->Sound.SoundVar1]; yo++){
         alt = gridlevel_to_height(dtx->Sound.SoundVar1Owner, yo);
         value = dtx->Sound.soundline[yo];
         oldx = x;
         oldy = y;
         vardata_to_xy(dtx, alt, value,
                       dtx->Sound.PreviousSoundVar1Owner->MinVal[dtx->Sound.SoundVar1],
                       dtx->Sound.PreviousSoundVar1Owner->MaxVal[dtx->Sound.SoundVar1], &x, &y);
         if ((x <= dtx->Sound.sndwidth) && (y <= dtx->Sound.sndheight)
            && (oldx <= dtx->Sound.sndwidth) && (oldy <= dtx->Sound.sndheight)
            && (x >= 0) && (y >= 0) && (oldx >= 0) && (oldy >= 0)){
            XDrawLine(SndDpy, dtx->Sound.soundwin, dtx->Sound.var1_gc, oldx+BORDER,
                      oldy+BORDER-HEBGBS, x +BORDER, y +BORDER-HEBGBS);
         }
      }
   }
   else {
      if (dtx->Sound.var1grid != NULL ){
         release_grid(dtx->Sound.PreviousSoundVar1Owner, time,
                      dtx->Sound.PreviousSoundVar1, dtx->Sound.var1grid);
         dtx->Sound.var1grid = NULL;
      }
   }

   oldx = -1;
   oldy = -1;
   x    = -1;
   y    = -1;
   if( dtx->Sound.SoundVar2 >= 0){

      if ( (dtx->Sound.SoundVar2Owner != dtx->Sound.PreviousSoundVar2Owner) &&
           (dtx->Sound.SoundVar2 != dtx->Sound.PreviousSoundVar2) &&
           dtx->Sound.var2grid != NULL ){
         release_grid( dtx->Sound.PreviousSoundVar2Owner, time,
                       dtx->Sound.PreviousSoundVar2, dtx->Sound.var2grid );
         dtx->Sound.var2grid = get_grid( dtx->Sound.SoundVar2Owner, time,
                                         dtx->Sound.SoundVar2);
      }
      dtx->Sound.PreviousSoundVar2 = dtx->Sound.SoundVar2;
      dtx->Sound.PreviousSoundVar2Owner = dtx->Sound.SoundVar2Owner;

      if (!dtx->Sound.var2_gc){
         dtx->Sound.var2_gc = make_gc(dtx, 0, 255, 0, 0, 0, 0, 2);
         if  (XLoadFont(SndDpy, dtx->SoundFontName)){
            XSetFont(SndDpy, dtx->Sound.var2_gc, XLoadFont(SndDpy, dtx->SoundFontName));
         }
      }
      if (dtx->Sound.var2grid == NULL ){
         dtx->Sound.var2grid = get_grid( dtx->Sound.SoundVar2Owner, time, dtx->Sound.SoundVar2);
      }
      if (!dtx->Sound.var2grid){
         return NULL;
      }
      if (dtx->Sound.SoundVar2Owner->GridSameAsGridPRIME){
         yo= extract_sound( dtx, dtx->Sound.var2grid, dtx->Sound.SoundVar2, dtx->Nr,
                            dtx->Nc, dtx->Nl,
                            dtx->LowLev, row, col );
      }
      else{
         yo= extract_soundPRIME(dtx->Sound.SoundVar2Owner, dtx->Sound.SoundVar2,
                              dtx->Sound.SoundVar2Owner->Nr, dtx->Sound.SoundVar2Owner->Nc,
                              dtx->Sound.SoundVar2Owner->Nl[dtx->Sound.SoundVar2],
                              dtx->Sound.SoundVar2Owner->LowLev[dtx->Sound.SoundVar2], row, col );
      }
      for (yo=0; yo < dtx->Sound.SoundVar2Owner->Nl[dtx->Sound.SoundVar2]; yo++){
         alt = gridlevel_to_height(dtx->Sound.SoundVar2Owner, yo);
         value = dtx->Sound.soundline[yo];
         oldx = x;
         oldy = y;
         vardata_to_xy(dtx, alt, value,
                       dtx->Sound.SoundVar2Owner->MinVal[dtx->Sound.SoundVar2],
                       dtx->Sound.SoundVar2Owner->MaxVal[dtx->Sound.SoundVar2], &x, &y);
         if ((x <= dtx->Sound.sndwidth) && (y <= dtx->Sound.sndheight)
            && (oldx <= dtx->Sound.sndwidth) && (oldy <= dtx->Sound.sndheight)
            && (x >= 0) && (y >= 0) && (oldx >= 0) && (oldy >= 0)){
            XDrawLine(SndDpy, dtx->Sound.soundwin, dtx->Sound.var2_gc, oldx+BORDER,
                      oldy+BORDER-HEBGBS, x +BORDER, y +BORDER-HEBGBS);
         }
      }
   }
   else {
      if (dtx->Sound.var2grid != NULL ){
         release_grid(dtx->Sound.PreviousSoundVar2Owner, time,
                      dtx->Sound.PreviousSoundVar2, dtx->Sound.var2grid);
         dtx->Sound.var2grid = NULL;
      }
   }
  
   oldx = -1;
   oldy = -1;
   x    = -1;
   y    = -1;
   if( dtx->Sound.SoundVar3 >= 0){

      if ( (dtx->Sound.SoundVar3Owner != dtx->Sound.PreviousSoundVar3Owner) &&
           (dtx->Sound.SoundVar3 != dtx->Sound.PreviousSoundVar3) &&
           dtx->Sound.var3grid != NULL ){
         release_grid( dtx->Sound.PreviousSoundVar3Owner, time,
                       dtx->Sound.PreviousSoundVar3, dtx->Sound.var3grid );
         dtx->Sound.var3grid = get_grid( dtx->Sound.SoundVar3Owner, time, dtx->Sound.SoundVar3);
      }
      dtx->Sound.PreviousSoundVar3 = dtx->Sound.SoundVar3;
      dtx->Sound.PreviousSoundVar3Owner = dtx->Sound.SoundVar3Owner;

      if (!dtx->Sound.var3_gc){
         dtx->Sound.var3_gc = make_gc(dtx, 0, 255, 0, 0, 0, 0, 2);
         if  (XLoadFont(SndDpy, dtx->SoundFontName)){
            XSetFont(SndDpy, dtx->Sound.var3_gc, XLoadFont(SndDpy, dtx->SoundFontName));
         }
      }
      if (dtx->Sound.var3grid == NULL ){
         dtx->Sound.var3grid = get_grid( dtx->Sound.SoundVar3Owner, time, dtx->Sound.SoundVar3);
      }
      if (!dtx->Sound.var3grid){
         return NULL;
      }
      if (dtx->Sound.SoundVar3Owner->GridSameAsGridPRIME){
         yo= extract_sound( dtx,
                            dtx->Sound.var3grid, dtx->Sound.SoundVar3, dtx->Nr,
                            dtx->Nc, dtx->Nl,
                            dtx->LowLev, row, col );
      }
      else{
         yo= extract_soundPRIME(dtx->Sound.SoundVar3Owner, dtx->Sound.SoundVar3,
                              dtx->Sound.SoundVar3Owner->Nr, dtx->Sound.SoundVar3Owner->Nc,
                              dtx->Sound.SoundVar3Owner->Nl[dtx->Sound.SoundVar3],
                              dtx->Sound.SoundVar3Owner->LowLev[dtx->Sound.SoundVar3], row, col );
      }
      for (yo=0; yo < dtx->Sound.SoundVar3Owner->Nl[dtx->Sound.SoundVar3]; yo++){
         alt = gridlevel_to_height(dtx->Sound.SoundVar3Owner, yo);         
         value = dtx->Sound.soundline[yo];
         oldx = x;
         oldy = y;
         vardata_to_xy(dtx, alt, value,
                       dtx->Sound.SoundVar3Owner->MinVal[dtx->Sound.SoundVar3],
                       dtx->Sound.SoundVar3Owner->MaxVal[dtx->Sound.SoundVar3], &x, &y);
         if ((x <= dtx->Sound.sndwidth) && (y <= dtx->Sound.sndheight)
            && (oldx <= dtx->Sound.sndwidth) && (oldy <= dtx->Sound.sndheight)
            && (x >= 0) && (y >= 0) && (oldx >= 0) && (oldy >= 0)){
            XDrawLine(SndDpy, dtx->Sound.soundwin, dtx->Sound.var3_gc, oldx+BORDER,
                      oldy+BORDER-HEBGBS, x +BORDER, y +BORDER-HEBGBS);
         }
      }
   }
   else {
      if (dtx->Sound.var3grid != NULL ){
         release_grid(dtx->Sound.PreviousSoundVar3Owner, time,
                      dtx->Sound.PreviousSoundVar3, dtx->Sound.var3grid);
         dtx->Sound.var3grid = NULL;
      }
   }

   return 1;
}

    /**********************************************************************/
    /* This converts vertical plot data to x and y's matching the soundwin*/
    /**********************************************************************/
    /* Input: dtx - context                                               */
    /*        alt - altitude of data point in                             */
    /*        value - value of the data point, to be plotted on the y axis*/
    /*        min - minimum value for the data point                      */
    /*        max - maximum value for the data point                      */
    /* Output: x, y - the coordinates to be plotted on soundwin           */
    /**********************************************************************/



void vardata_to_xy(Display_Context dtx, float alt, float value, float min, float max, int *x, int *y)
{

   if (dtx->Sound.samestepflag){
      *x = (int) ((dtx->Sound.sndwidth * (value - dtx->Sound.samestepmin))/(dtx->Sound.samestepmax -
                                           dtx->Sound.samestepmin));
      *y = (int) (dtx->Sound.sndheight - (((alt-dtx->BottomBound)*dtx->Sound.sndheight)/
                                           (dtx->TopBound - dtx->BottomBound))); 
   }
   else {
      *x = (int) ((dtx->Sound.sndwidth * (value - min))/(max - min));
      *y = (int) (dtx->Sound.sndheight - (((alt-dtx->BottomBound)*dtx->Sound.sndheight)/
                                         (dtx->TopBound - dtx->BottomBound)));
   }
}

#endif


    /**********************************************************************/
    /* This will figure out what the interval for the value of the        */
    /* variable is compared to dtx->mainvarstep                           */
    /**********************************************************************/
    /* Input: dtx - context                                               */
    /**********************************************************************/

void setvarsteps( Display_Context dtx)
{

   if (dtx->Sound.samestepflag){
      int min, max, yo;
  
      if (dtx->Sound.SoundVar1 >= 0){
         min = dtx->Sound.SoundVar1Owner->MinVal[dtx->Sound.SoundVar1];
         max = dtx->Sound.SoundVar1Owner->MaxVal[dtx->Sound.SoundVar1];
      }
      if (dtx->Sound.SoundVar2 >= 0){
         if (dtx->Sound.SoundVar2Owner->MinVal[dtx->Sound.SoundVar2] < min){
            min = dtx->Sound.SoundVar2Owner->MinVal[dtx->Sound.SoundVar2];
         }
         if (dtx->Sound.SoundVar2Owner->MaxVal[dtx->Sound.SoundVar2] > max){
            max = dtx->Sound.SoundVar2Owner->MaxVal[dtx->Sound.SoundVar2];
         }
      }
      if (dtx->Sound.SoundVar3 >= 0){
         if (dtx->Sound.SoundVar3Owner->MinVal[dtx->Sound.SoundVar3] < min) {
            min = dtx->Sound.SoundVar3Owner->MinVal[dtx->Sound.SoundVar3];
         }
         if (dtx->Sound.SoundVar3Owner->MaxVal[dtx->Sound.SoundVar3] > max) {
            max = dtx->Sound.SoundVar3Owner->MaxVal[dtx->Sound.SoundVar3];
         }
      }
      if (dtx->Sound.SoundVar1 >= 0){
         dtx->Sound.var1step =(( ((float)(dtx->Sound.mainvarstep)) * (max -
                        min ))/ ((float)(dtx->Sound.sndwidth))) ;
      }
      if (dtx->Sound.SoundVar2 >= 0){
         dtx->Sound.var2step =(( ((float)(dtx->Sound.mainvarstep)) * (max -
                        min))/ ((float)(dtx->Sound.sndwidth))) ;
      }
      if (dtx->Sound.SoundVar3 >= 0){
         dtx->Sound.var3step =(( ((float)(dtx->Sound.mainvarstep)) * (max -
                        min))/ ((float)(dtx->Sound.sndwidth))) ;
      }
      dtx->Sound.samestepmax = max;
      dtx->Sound.samestepmin = min;
   }
   else {
      if (dtx->Sound.SoundVar1 >= 0){
         dtx->Sound.var1step =(( ((float)(dtx->Sound.mainvarstep)) *
                              (dtx->Sound.SoundVar1Owner->MaxVal[dtx->Sound.SoundVar1] -
                        dtx->Sound.SoundVar1Owner->MinVal[dtx->Sound.SoundVar1]))/
                       ((float)(dtx->Sound.sndwidth))) ;
      }
      if (dtx->Sound.SoundVar2 >= 0){
         dtx->Sound.var2step =(( ((float)(dtx->Sound.mainvarstep)) *
                              (dtx->Sound.SoundVar2Owner->MaxVal[dtx->Sound.SoundVar2] -
                        dtx->Sound.SoundVar2Owner->MinVal[dtx->Sound.SoundVar2]))/
                       ((float)(dtx->Sound.sndwidth))) ;
      }
      if (dtx->Sound.SoundVar3 >= 0){
         dtx->Sound.var3step =(( ((float)(dtx->Sound.mainvarstep)) *
                              (dtx->Sound.SoundVar3Owner->MaxVal[dtx->Sound.SoundVar3] -
                        dtx->Sound.SoundVar3Owner->MinVal[dtx->Sound.SoundVar3]))/
                       ((float)(dtx->Sound.sndwidth))) ;
      }
   }
}


/* MJK 12.15.98 begin */

    /**********************************************************************/
    /* This converts skew-t values to the x and y to be plotted           */
    /**********************************************************************/
    /* Input: dtx - context                                               */
    /*        alt - altitude of data point in kilometers                  */
    /*        temp - temperature in Kelvin of data point to be plotter    */
    /* Output: x, y - the coordinates to be plotted on soundwin           */
    /**********************************************************************/

void data_to_xy(Display_Context dtx, float alt, float temp, int *x, int *y)
{
   temp += ((alt + 0.00000069) * 4.638);
   *x = dtx->Sound.sndwidth * (temp - dtx->Sound.SndMinTemp) /
        (dtx->Sound.SndMaxTemp - dtx->Sound.SndMinTemp);
   data_to_y (dtx, alt, y);
}

    /**********************************************************************/
    /* This just converts vertical values into a y coordinate             */
    /**********************************************************************/
    /* Input: dtx - context                                               */
    /*        alt - altitude of the value                                 */
    /* Output: y - the y coordinate to be plotted in soundwin             */
    /**********************************************************************/


void data_to_y (Display_Context dtx, float alt, int *y)
{
   alt += .00000069;
   *y = dtx->Sound.sndheight * (dtx->Sound.TopHgt - alt) / dtx->Sound.DiffHgt;
}

/* MJK 12.15.98 end */

#ifdef DONOTUSE
    /**********************************************************************/
    /* This converts skew-t values to the x and y to be plotted           */
    /**********************************************************************/
    /* Input: dtx - context                                               */
    /*        alt - altitude of data point in kilometers                  */
    /*        temp - temperature in Kelvin of data point to be plotter    */
    /* Output: x, y - the coordinates to be plotted on soundwin           */
    /**********************************************************************/

void data_to_xy(Display_Context dtx, float alt, float temp, int *x, int *y)
{
   alt += .00000069;
   temp += (alt * 4.638);
  *x = (int) ((temp- dtx->Sound.SndMinTemp) * (dtx->Sound.sndwidth/(dtx->Sound.SndMaxTemp - dtx->Sound.SndMinTemp)));
  *y = (int) (dtx->Sound.sndheight - (((alt-dtx->BottomBound)*dtx->Sound.sndheight)/
             (dtx->TopBound - dtx->BottomBound)));

}

    /**********************************************************************/
    /* This just converts vertical values into a y coordinate             */
    /**********************************************************************/
    /* Input: dtx - context                                               */
    /*        alt - altitude of the value                                 */
    /* Output: y - the y coordinate to be plotted in soundwin             */
    /**********************************************************************/


void data_to_y (Display_Context dtx, float alt, int *y)
{
   alt += .00000069;
   *y = (int) (dtx->Sound.sndheight - (((alt-dtx->BottomBound)*
               dtx->Sound.sndheight)/(dtx->TopBound - dtx->BottomBound)));
}

#endif



    /**********************************************************************/
    /* This converts a grid level to a height                             */
    /**********************************************************************/
    /* Input: dtx - context                                               */
    /*        level - grid level                                          */
    /* Output: height                                                     */
    /*************************** NOTE!! ***********************************/
    /*******  This same code is used in proj.c lines 614 - 649  ***********/
    /**********************************************************************/


float grid_level_to_height( Display_Context dtx, float level )
{
   int ilevel;
   float rlevel;

   if (level<=0) {
      return dtx->BottomBound;
   }
   else if (level>=dtx->MaxNl-1 || dtx->MaxNl == 1) {
      return dtx->TopBound;
   }
   else {
      switch (dtx->VerticalSystem) {
         case VERT_GENERIC:
         case VERT_EQUAL_KM:
            return dtx->BottomBound + level * dtx->LevInc;
         case VERT_NONEQUAL_MB:
         case VERT_NONEQUAL_KM:
            ilevel = (int) level;
            rlevel = level - ilevel;
            return dtx->Height[ilevel] * (1.0-rlevel) + dtx->Height[ilevel+1] * rlevel;
         default:
            printf("Error in gridlevel_to_height\n");
      }
   }
   return 0.0;
}

    /**********************************************************************/
    /* This interpolates values in between grid points                    */
    /**********************************************************************/
    /* Input: dtx - context                                               */
    /*        grid - grid array                                           */
    /*        var - variable to extract form grid                         */
    /*        nr, nc, nl - number or rows, columns, and levels for varible*/
    /*        lowlev - the lowest level for the variable                  */
    /*        row, col - row and column where cursor is                   */
    /* Output: return 1 if all goes well                                  */
    /**********************************************************************/

static int extract_sound( Display_Context dtx, float *grid, int var,
                             int nr, int nc, int nl, int lowlev,
                             float row, float col)
{
   float a, b, c, d;
   float I, J, K, L, E, F, Z;
   int  level, lowrow, highrow, leftcol, rightcol;


   /* allocate buffer to put soundline data into */
   if (dtx->Sound.soundline != NULL){
      free(dtx->Sound.soundline);
   }
   dtx->Sound.soundline = (float *) malloc(nl * sizeof(float) );
   if (!dtx->Sound.soundline) {
     return NULL;
   }

   /*
    *  The best way to see the interpolation here is to draw a box
    *  each of the four coners represents a discrete grid point with a certain
    *  given value and the cursor lies someplace in side this box or on the BORDER
    *  of it.  The box has sides: TOP, BOTTOM, LEFT, and RIGHT.   Rows increase
    *  going down, and Cols increase going right.  So
    *  the upper left  corner is (leftcol, lowrow)  with a data value I
    *  the upper right corner is (rightcol,lowrow)  with a data value J
    *  the lower left  corner is (leftcol, highrow) with a data value K
    *  the lower right corner is (rightcol,highrow) with a data value L
    *  q is the point where the cursor col intersects the BOTTOM line
    *  r is the point where the cursor col intersects the TOP line
    *  s is the point on the line between q and r where the cursor lies
    *  E is the interpolated value for the point q
    *  F is the interpolated value for the point r
    *  Then one simply interpolates for what the value of the point s is
    *
    */

   level = lowlev;
   lowrow = (int) row;
   leftcol = (int) col;
   highrow = lowrow + 1;
   rightcol = leftcol + 1;

   if (highrow > nr-1)
      highrow = nr-1;
   if (rightcol > nc-1)
      rightcol = nc-1;

   a = col - leftcol;
   b = 1 - a;
   c = row - lowrow;
   d = 1 - c;

   if ( (row == ((float)lowrow)) && (col == ((float)leftcol)) ){
      for (level = lowlev; level < nl; level++){
         Z = grid[ lowrow + nr * (leftcol + nc * level)];
         if (IS_MISSING(Z)){
            dtx->Sound.soundline[level] = MISSING;
         }
         else{
            dtx->Sound.soundline[level] = Z;
         }
      }
   }
   else {
      for (level = lowlev; level < nl; level++){
         /* get values for the four coners of the (col, row) box */
         I = grid[ lowrow + nr * (leftcol + nc * level)];
         J = grid[ lowrow + nr * (rightcol+ nc * level)];
         K = grid[ highrow+ nr * (leftcol + nc * level)];
         L = grid[ highrow+ nr * (rightcol+ nc * level)];
         if (IS_MISSING(I) || IS_MISSING(J) || IS_MISSING(K) || IS_MISSING(L)) {
            dtx->Sound.soundline[level] = MISSING;
         }
         else {
            E = a * L +  b * K ;
            F = a * J +  b * I ;
            dtx->Sound.soundline[level] = c * E + d * F;
         }
      }
   }
   return 1;
}

static int extract_soundPRIME( Context ctx, int var,
                             int nr, int nc, int nl, int lowlev,
                             float row, float col)
{
   int  level;
   float ROW, COL, LEV, Row, Col, Lev;

   /* allocate buffer to put soundline data into */
   if (ctx->dpy_ctx->Sound.soundline != NULL){
      free(ctx->dpy_ctx->Sound.soundline);
   }
   ctx->dpy_ctx->Sound.soundline = (float *) malloc(nl * sizeof(float) );
   if (!ctx->dpy_ctx->Sound.soundline) {
     return NULL;
   }


   ROW = (float)(row);
   COL = (float)(col);
   LEV = gridlevel_to_gridlevelPRIME( ctx, lowlev);
   for (level = lowlev; level < nl; level++){
      gridPRIME_to_grid(ctx, 0, var, 1, &ROW, &COL, &LEV, &Row, &Col, &Lev);  
      ctx->dpy_ctx->Sound.soundline[level]=interpolate_grid_value( ctx,
                                           0, var, Row, Col, level);
   }
   return 1;
}






    /**********************************************************************/
    /* This interpolates values in between grid points, for wind variables*/
    /**********************************************************************/
    /* Input: dtx - context                                               */
    /*        ugrid, vgrid  - grid arrays                                 */
    /*        varu, varv - variable to extract form grid                  */
    /*        nr, nc, nl - number or rows, columns, and levels for varible*/
    /*        lowlev - the lowest level for the variables                 */
    /*        row, col - row and column where cursor is                   */
    /* Output: return 1 if all goes well                                  */
    /**********************************************************************/

static int extract_wind( Display_Context dtx, float *gridu, float *gridv,
                             int varu, int varv,
                             int nr, int nc, int nl, int lowlev,
                             float row, float col)
{
   float a, b, c, d;
   float I, J, K, L, E, F, Z;
   int  level, lowrow, highrow, leftcol, rightcol;


   /* allocate buffer to put windline data into */


   if (dtx->Sound.uwindline != NULL) free(dtx->Sound.uwindline);
   if (dtx->Sound.vwindline != NULL) free(dtx->Sound.vwindline);

   dtx->Sound.uwindline = (float *) malloc( nl * sizeof(float) );
   dtx->Sound.vwindline = (float *) malloc( nl * sizeof(float) );
   if ((!dtx->Sound.uwindline) || (!dtx->Sound.vwindline)) {

      /* MJK 12.15.98 */
      if (dtx->Sound.uwindline != NULL)
      {
         free (dtx->Sound.uwindline);
         dtx->Sound.uwindline = NULL;
      }
      if (dtx->Sound.vwindline != NULL)
      {
         free (dtx->Sound.vwindline);
         dtx->Sound.vwindline = NULL;
      }


      return NULL;
   }

   level = lowlev;
   lowrow = (int) row;
   leftcol = (int) col;
   highrow = lowrow + 1;
   rightcol = leftcol + 1;

   if (highrow > nr-1)
      highrow = nr-1;
   if (rightcol > nc-1)
      rightcol = nc-1;

   a = col - leftcol;
   b = 1 - a;
   c = row - lowrow;
   d = 1 - c;

   if ( (row == ((float)lowrow)) && (col == ((float)leftcol)) ){
      for (level = lowlev; level < nl; level++){
         Z = gridu[ lowrow + nr * (leftcol + nc * level)];
         if (IS_MISSING(Z)){
            dtx->Sound.uwindline[level] = MISSING;
         }
         else{
            dtx->Sound.uwindline[level] = Z;
         }
      }
      for (level = lowlev; level < nl; level++){
         Z = gridv[ lowrow + nr * (leftcol + nc * level)];
         if (IS_MISSING(Z)){
            dtx->Sound.vwindline[level] = MISSING;
         }
         else{
            dtx->Sound.vwindline[level] = Z;
         }
      }
   return 1;
   }
   else {
      for (level = lowlev; level < nl; level++){
         /* get values for the four coners of the (col, row) box */
         I = gridu[ lowrow + nr * (leftcol + nc * level)];
         J = gridu[ lowrow + nr * (rightcol+ nc * level)];
         K = gridu[ highrow+ nr * (leftcol + nc * level)];
         L = gridu[ highrow+ nr * (rightcol+ nc * level)];
         if (IS_MISSING(I) || IS_MISSING(J) || IS_MISSING(K) || IS_MISSING(L)) {
            dtx->Sound.uwindline[level] = MISSING;
         }
         else {
            E = a * L +  b * K ;
            F = a * J +  b * I ;
            dtx->Sound.uwindline[level] = c * E + d * F;
         }
      }

      for (level = lowlev; level < nl; level++){
         /* get values for the four coners of the (col, row) box */
         I = gridv[ lowrow + nr * (leftcol + nc * level)];
         J = gridv[ lowrow + nr * (rightcol+ nc * level)];
         K = gridv[ highrow+ nr * (leftcol + nc * level)];
         L = gridv[ highrow+ nr * (rightcol+ nc * level)];
         if (IS_MISSING(I) || IS_MISSING(J) || IS_MISSING(K) || IS_MISSING(L)) {
            dtx->Sound.vwindline[level] = MISSING;
         }
         else {
            E = a * L +  b * K ;
            F = a * J +  b * I ;
            dtx->Sound.vwindline[level] = c * E + d * F;
         }
      }
      return 1;
   }
}

static int extract_windPRIME( Context ctx, 
                             int varu, int varv,
                             int nr, int nc, int nl, int lowlev,
                             float row, float col)
{
   int  level;
   float ROW, COL, LEV, Row, Col, Lev;


   /* allocate buffer to put windline data into */


   if (ctx->dpy_ctx->Sound.uwindline != NULL) free(ctx->dpy_ctx->Sound.uwindline);
   if (ctx->dpy_ctx->Sound.vwindline != NULL) free(ctx->dpy_ctx->Sound.vwindline);

   ctx->dpy_ctx->Sound.uwindline = (float *) malloc( nl * sizeof(float) );
   ctx->dpy_ctx->Sound.vwindline = (float *) malloc( nl * sizeof(float) );
   if ((!ctx->dpy_ctx->Sound.uwindline) || (!ctx->dpy_ctx->Sound.vwindline)) {


      /* MJK 12.15.98 */
      if (ctx->dpy_ctx->Sound.uwindline != NULL)
      {
         free (ctx->dpy_ctx->Sound.uwindline);
         ctx->dpy_ctx->Sound.uwindline = NULL;
      }
      if (ctx->dpy_ctx->Sound.vwindline != NULL)
      {
         free (ctx->dpy_ctx->Sound.vwindline);
         ctx->dpy_ctx->Sound.vwindline = NULL;
      }


      return NULL;
   }

   ROW = (float)(row);
   COL = (float)(col);
   LEV = gridlevel_to_gridlevelPRIME( ctx, lowlev);
   for (level = lowlev; level < nl; level++){
      gridPRIME_to_grid(ctx, 0, varu, 1, &ROW, &COL, &LEV, &Row, &Col, &Lev);
      ctx->dpy_ctx->Sound.uwindline[level]=interpolate_grid_value( ctx,
                                           0, varu, Row, Col, level);
   }
   ROW = (float)(row);
   COL = (float)(col);
   LEV = gridlevel_to_gridlevelPRIME( ctx, lowlev);
   for (level = lowlev; level < nl; level++){
      gridPRIME_to_grid(ctx, 0, varv, 1, &ROW, &COL, &LEV, &Row, &Col, &Lev);
      ctx->dpy_ctx->Sound.vwindline[level]=interpolate_grid_value( ctx,
                                           0, varv, Row, Col, level);
   }
   return 1;
}
