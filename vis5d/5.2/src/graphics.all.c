/*  graphics.all.c */

/* Graphics independent of underlying library */
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


#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "globals.h"


/*
 * The following graphics variables have the same value for all vis5d
 * contexts.
 */
Window BigWindow = NULL;
int BigWinWidth = NULL;
int BigWinHeight = NULL;

/* MJK 12.21.98 */
int BigWinFull = 0;

int StaticWin = 0;
int StaticWinXPos = 0;
int StaticWinYPos = 0;
int StaticWinWidth = 0;
int StaticWinHeight = 0;

Display *GfxDpy = NULL;          /* The X display */
int GfxScr = 0;                  /* The X screen number */
Visual *GfxVisual = NULL;        /* The X visual */
Colormap GfxColormap = 0;        /* The X colormap */
int GfxDepth = 0;                /* Depth of the 3-D window */
int ScrWidth=0, ScrHeight=0;     /* screen size */
int HQR_available = 0;           /* High-Quality rendering available? */
int Perspec_available = 0;       /* Perspective rendering available? */
Display *SndDpy = NULL;
int SndScr = 0;
Visual *SndVisual = NULL;
Colormap SndColormap = 0;
int SndDepth = 0;
int SndScrWidth = 0, SndScrHeight = 0;
int SndRootWindow = 0;



extern void init_graphics2( void );




/*
 * Find the best visual the given display/screen supports.  This is a
 * helper function to using some graphics libraries.
 * Input:  dpy - the display
 *         scr - the screen number
 * Output:  depth - the depth of the visual
 *          visual - the visual pointer
 *          cmap - a suitable colormap
 */
int find_best_visual( Display *dpy, int scr, int *depth, Visual **visual,
                      Colormap *cmap )
{
   XVisualInfo visinfo;
   Window root;
   Screen *screen;

   assert( dpy );

   root = RootWindow( dpy, scr );
   screen = DefaultScreenOfDisplay( dpy );

   if (XMatchVisualInfo( dpy, scr, 24, TrueColor, &visinfo)) {
      *depth = 24;
      *visual = visinfo.visual;
      if (*visual==DefaultVisual(dpy,scr) && DefaultDepth(dpy,scr)==24) {
         *cmap = DefaultColormap(dpy,scr);
      }
      else {
         *cmap = XCreateColormap( dpy, root, *visual, AllocNone );
      }
   }
   else if (XMatchVisualInfo( dpy, scr, 8, PseudoColor, &visinfo)) {
      *depth = 8;
      *visual = visinfo.visual;
      if (visinfo.visual==DefaultVisual(dpy,scr) && DefaultDepth(dpy,scr)==8
          && MaxCmapsOfScreen(screen)==1) {
         /* share the root colormap */
         *cmap = DefaultColormap( dpy, scr );
      }
      else {
         *cmap = XCreateColormap( dpy, root, *visual, AllocNone );
      }
   }
   else {
      /* Use the screen's default */
      *depth = DefaultDepth( dpy, scr );
      *visual = DefaultVisual( dpy, scr );
      *cmap = DefaultColormap( dpy, scr );
   }

   if (*depth<8) {
      printf("Error: couldn't get suitable visual!\n");
      exit(1);
   }
   return 1;
}




/*
 * Open the default display, determine screen size, call graphics
 * library-dependent initialization function.  This should only be
 * called once, not per-context.
 */
void init_graphics( void )
{
   /* open the default display */
   GfxDpy = XOpenDisplay( NULL );
   SndDpy = GfxDpy; 
   if (!GfxDpy) {
      printf("Unable to open default display\n");
      exit(1);
   }
  if (!SndDpy) {
      printf("Unable to open sound display\n");
      exit(1);
   }


   GfxScr = DefaultScreen( GfxDpy );
   SndScr = DefaultScreen( SndDpy );


   ScrWidth = DisplayWidth( GfxDpy, GfxScr );
   ScrHeight = DisplayHeight( GfxDpy, GfxScr );
   SndScrWidth = DisplayWidth( SndDpy, SndScr );
   SndScrHeight = DisplayHeight( SndDpy, SndScr );

   /* Do library-specific initializations */
   init_graphics2();

   /* While some graphics libraries don't need this, others do and the
    * X/GUI stuff always does.
    */
   find_best_visual( GfxDpy, GfxScr, &GfxDepth, &GfxVisual, &GfxColormap );
   find_best_visual( SndDpy, SndScr, &SndDepth, &SndVisual, &SndColormap );
}




/*
 * Find a window by name and return its window ID.  Modified to
 * search breadth-first instead of depth-first by Mike Manyin @ GSFC.
 */
Window find_window( Display *dpy, int scr, Window start, char *name )
{
   Status stat;
   int n;
   unsigned int num;
   Window w, root, parent, *children;
   char *title;

   if (XFetchName( dpy, start, &title )==1) {
      if (strcmp( name, title )==0) { 
         /* found it */
         XFree( title );
         return start;
      }
      XFree( title );
   }
 
   /* get list of child windows */
   stat = XQueryTree( dpy, start, &root, &parent, &children, &num );

   if (stat==1) {
      /* search each child window for a match: */
      for (n=num-1; n>=0; n--) {
         if (XFetchName( dpy, start, &title )==1) {
            if (strcmp( name, title )==0) {
               /* found it */
               XFree( title );
               return start;
            }
            XFree( title );
         }
      }

      /* search the descendents of each child for a match: */
      for (n=num-1; n>=0; n--) {
         w = find_window( dpy, scr, children[n], name );
         if (w) {
            XFree( (char *) children );
            return w;
         }
      }

      if ( children != NULL ) {
         XFree( (char *) children );
      }
   }

   /* not found */
   return 0;
}



