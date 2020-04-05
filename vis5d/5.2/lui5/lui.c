#ifdef SCCS
static char sccsid[]="@(#)lui.c        1.2 Stellar 89/03/01";
#endif
/*
                        Copyright 1989 by
                        Stellar Computer Inc.
                        All Rights Reserved
        
        This software comprises unpublished confidential information of
        Stellar Computer Inc. and may not be used, copied or made
        available to anyone, except in accordance with the license
        under which it is furnished.
*/

/*
 * LUI library
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <sys/time.h>
#include <unistd.h>

#define LUI_DECLARE
#include "lui.h"
#include "pixmaps.h"
#include <X11/Xatom.h>


#ifndef O_RDONLY
#  if defined(ibm) || defined(dec)
#    include <fcntl.h>
#  else
#    if defined(dec)
#       include <sys/file.h>
#    else
#       include <sys/fcntl.h>
#    endif
#  endif
#endif


/*
 * Global library variables:
 */
Display *LUI_Display = NULL;
Window LUI_RootWindow;
Visual *LUI_Visual;
XFontStruct *LUI_Font;
Colormap LUI_Colormap;


unsigned long
  LUI_Color_black,
  LUI_Color_md_grey,
  LUI_Color_white,
  LUI_Color_red,
  LUI_Color_green,
  LUI_Color_blue,
  LUI_Color_fore,
  LUI_Color_xor,
  LUI_Color_gray,
  LUI_Color_highgray,
  LUI_Color_darkgray;
GC
  LUI_Gc,
  LUI_GC_black,
  LUI_GC_md_grey,
  LUI_GC_white,
  LUI_GC_red,
  LUI_GC_green,
  LUI_GC_blue,
  LUI_GC_fore,
  LUI_GC_xor,
  LUI_GC_gray,
  LUI_GC_highgray,
  LUI_GC_darkgray,
  LUI_GC_top,                /* for top and left of 3-D highlight */
  LUI_GC_bottom;        /* for bottom and right of 3-D highlight */
int
  LUI_Debug,
  LUI_Screen,
  LUI_Width,
  LUI_Height,
  LUI_Depth,
  LUI_Font_yoff,
  LUI_Font_height,
  LUI_Font_width;

int LUI_Border = 3;

char LUI_ProgramName[128];
Cursor LUI_CursorTable[32];

int
  LUI_DefaultHue = -1,
  LUI_DefaultHue2 = 0,
  LUI_DefaultHue3 = 0;



static Window LUI_DummyWindow;
static Window main_window = 0;


/*
 * Color allocation
 */
#define PF_TRUECOLOR 0
#define PF_XALLOC    1
#define PF_8BIT      2

static int pixelformat;
static unsigned long ctable8[5][9][5];   /* Only for PF_8BIT */
static unsigned long rtable[256], gtable[256], btable[256];  /* PF_TRUECOLOR */




int context_index;

void LUI_ContextIndex(index)
int index;
{
  context_index = index;
}



static void LUI_FatalError( char *errstr, char *extra )
{
    fprintf(stderr, "Fatal LUI Error: %s ", errstr);

    if (extra != NULL)
      fprintf(stderr, "%s\n", extra);
    else 
      fprintf(stderr, "\n");

    if (LUI_Display != NULL) 
      XCloseDisplay(LUI_Display);

    exit(-1);
}



/*
 * Specify the width of the 3-D border to draw inside buttons, scrollbars, etc.
 * Input:  width - border width in pixels
 */
void LUI_BorderWidth( int width )
{
   if (width<1) {
      LUI_Border = 1;
   }
   else {
      LUI_Border = width;
   }
}



/*
 * Specify the width of the 3-D frame to draw inside buttons, scrollbars, etc.
 * Input:  width - border width in pixels
 */
void LUI_FrameWidth( int width )
{
   if (width<1) {
      LUI_Border = 0;
   }
   else {
      LUI_Border = width;
   }
}



/*
 * A replacement for XAllocColor.  This function should never fail
 * to allocate a color.  When XAllocColor fails we return the nearest
 * matching color.
 *
 *  This is the same code found in sounding.c lines 227 - 277
 */
Status LUI_XAllocColor( Display *dpy, Colormap cmap, int cmap_size,
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
         /*printf("code problem in lui.c\n");*/
      }
   }
#undef DISTANCE

   return 1;
}





/*
 * Given an RGB color, return the corresponding pixel value.
 * Input;  r, g, b - red, green, and blue in [0,255]
 * Return:  a pixel value
 *
 * This is the same code found in sounding.c lines 195 - 223
 *
 */
unsigned long LUI_AllocateColorInt( int r, int g, int b )
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
         LUI_XAllocColor( LUI_Display, LUI_Colormap, LUI_Visual->map_entries,
                          &xcol );
         return xcol.pixel;
      default:
         printf("Error in LUI_AllocateColorInt %d\n", pixelformat);
         exit(0);
   }
   return 0;
}




/*
 * Allocate a color specified by red, green, and blue and return a pixel
 * value.  If the color allocation fails, the white pixel will be returned.
 * Input:  red, green, blue - values in [0,1]
 * Return:  a pixel value
 */
unsigned long LUI_AllocateColor( float red, float green, float blue )
{
   return LUI_AllocateColorInt( (int) (red*255.0), (int) (green*255.0),
                                (int) (blue*255.0) );
}





static GC LUI_MakeGC( unsigned long color )
{
    unsigned long gc_mask;
    XGCValues gcv;

    gc_mask          = GCForeground | GCBackground | GCArcMode;
    gcv.foreground   = color;
    gcv.arc_mode     = ArcChord;
    return(XCreateGC(LUI_Display, LUI_DummyWindow, gc_mask, &gcv));
}



/*
 * Allocate our colors and GC's.  This function heavily modified by
 * BEP on Jan 18, 1995.
 */
void LUI_CreateColors( void )
{
   XGCValues gcv;
   unsigned long gc_mask, color;
   static char gray_pattern[] = { 0x1, 0x2 };

   /*** The basic GC ***/
   gc_mask        = GCForeground | GCBackground | GCFont | GCArcMode;
   gcv.font       = LUI_Font->fid;
   gcv.arc_mode   = ArcChord;
   LUI_Gc         = XCreateGC(LUI_Display, LUI_DummyWindow, gc_mask, &gcv);
   LUI_GC_fore    = LUI_Gc;

   /*** Black ***/
   LUI_Color_black = LUI_AllocateColor( 0.0, 0.0, 0.0 );
   gcv.foreground = LUI_Color_black;
   LUI_GC_black = XCreateGC(LUI_Display, LUI_DummyWindow, gc_mask, &gcv);

   /*** White ***/
   LUI_Color_white = LUI_AllocateColor( 1.0, 1.0, 1.0 );
   gcv.foreground = LUI_Color_white;
   LUI_GC_white = XCreateGC(LUI_Display, LUI_DummyWindow, gc_mask, &gcv);

   /*** Gray (white on monochrome) ***/
   LUI_Color_gray = LUI_AllocateColor( 0.6, 0.6, 0.6 );
   gcv.foreground = LUI_Color_gray;
   LUI_GC_gray = XCreateGC(LUI_Display, LUI_DummyWindow, gc_mask, &gcv);

   /*** Highlight Gray (white on monochrome) ***/
   LUI_Color_highgray = LUI_AllocateColor( 0.75, 0.75, 0.75 );
   gcv.foreground = LUI_Color_highgray;
   LUI_GC_highgray = XCreateGC(LUI_Display, LUI_DummyWindow, gc_mask, &gcv);

   /*** Dark Gray (black on monochrome) ***/
   LUI_Color_darkgray = LUI_AllocateColor( 0.3, 0.3, 0.3 );
   gcv.foreground = LUI_Color_darkgray;
   if (LUI_Color_darkgray == LUI_Color_white) {
      LUI_Color_darkgray = LUI_Color_black;
   }
   LUI_GC_darkgray = XCreateGC(LUI_Display, LUI_DummyWindow, gc_mask, &gcv);

   /*** Red (black on monochrome) ***/
   LUI_Color_red = LUI_AllocateColor( 1.0, 0.0, 0.0 );
   if (LUI_Color_red == LUI_Color_white) {
      LUI_Color_red = LUI_Color_black;
   }
   gcv.foreground = LUI_Color_red;
   LUI_GC_red  = XCreateGC(LUI_Display, LUI_DummyWindow, gc_mask, &gcv);
   /*** Green ***/
   LUI_Color_green = LUI_AllocateColor( 0.0, 1.0, 0.0 );
   gcv.foreground = LUI_Color_green;
   LUI_GC_green = XCreateGC( LUI_Display, LUI_DummyWindow, gc_mask, &gcv );

   /*** Blue ***/
   LUI_Color_blue = LUI_AllocateColor( 0.0, 0.5, 1.0 );
   gcv.foreground = LUI_Color_blue;
   LUI_GC_blue = XCreateGC( LUI_Display, LUI_DummyWindow, gc_mask, &gcv );


   /*** Top 3-D GC ***/
   color = LUI_AllocateColor( 0.8, 0.8, 0.8 );
   if (color==LUI_Color_white) {
      /* make a stipple of 50% gray */
      unsigned long mask;
      mask = GCForeground | GCBackground | GCFont| GCFillStyle | GCStipple;
      gcv.foreground = LUI_Color_black;
      gcv.background = LUI_Color_white;
      gcv.fill_style = FillOpaqueStippled;
      gcv.stipple = XCreateBitmapFromData( LUI_Display, LUI_DummyWindow,
                                           gray_pattern, 2, 2 );
      LUI_GC_top  = XCreateGC(LUI_Display, LUI_DummyWindow, mask, &gcv);
   }
   else {
      gcv.foreground = color;
      LUI_GC_top  = XCreateGC(LUI_Display, LUI_DummyWindow, gc_mask, &gcv);
   }

   /*** Bottom 3-D GC ***/
   color = LUI_AllocateColor( 0.35, 0.35, 0.35 );
   if (color==LUI_Color_white) {
      /* make bottom GC black */
      LUI_GC_bottom = LUI_GC_black;
   }
   else {
      gcv.foreground = color;
      LUI_GC_bottom  = XCreateGC(LUI_Display, LUI_DummyWindow, gc_mask, &gcv);
   }

   /* Default button color: */
   LUI_Color_md_grey = LUI_AllocateColor( 0.5, 0.5, 0.5 );
   LUI_GC_md_grey = LUI_MakeGC(LUI_Color_md_grey);

}



/*
 * Initialize LUI.  This has to be the first LUI call.
 * Input:  program_name - the label for all window title bars
 *         display - the X display (or NULL)
 *         visual - the X visual (or NULL)
 *         depth - the depth of the visual (or 0)
 *         colormap - the X colormap (or 0)
 */
void LUI_Initialize( char *program_name, Display *display,
                     Visual *visual, int depth, Colormap colormap )
{
    static int initialized = 0;
    XSetWindowAttributes attr;
    XVisualInfo visinfo;

    /* Only do this once */
    if (initialized)
      return;
    else
      initialized = 1;

    /* this is a hack! */
    LUI_DefaultHue  = 0xffffff;
    LUI_DefaultHue2 = 0xffffff;
    LUI_DefaultHue3 = 0xffffff;


    /* Open the display if one wasn't passed (already opened) */
    if (display) {
       LUI_Display = display;
    }
    else {
       LUI_Display = XOpenDisplay(NULL);
       if (!LUI_Display) {
          LUI_FatalError("Can't open display", XDisplayName(NULL));
       }
    }

    /* Set defaults */
    LUI_RootWindow = DefaultRootWindow(LUI_Display);
    LUI_Screen     = DefaultScreen(LUI_Display);
    LUI_Width      = DisplayWidth (LUI_Display, LUI_Screen);
    LUI_Height     = DisplayHeight(LUI_Display, LUI_Screen);

    if (visual) {
       LUI_Visual = visual;
       LUI_Depth = depth;
       LUI_Colormap = colormap;
    }
    else {
       if (XMatchVisualInfo( LUI_Display,LUI_Screen,24,TrueColor,&visinfo )) {
          LUI_Visual = visinfo.visual;
          LUI_Depth = 24;
          LUI_Colormap = XCreateColormap( LUI_Display, LUI_RootWindow,
                                          LUI_Visual, AllocNone );
       }
       else {
          LUI_Visual = DefaultVisual( LUI_Display, LUI_Screen );
          LUI_Depth = DefaultDepth( LUI_Display, LUI_Screen );
          LUI_Colormap = DefaultColormap( LUI_Display, LUI_Screen );
       }
    }

    /* Setup color allocation stuff */
    if (LUI_Visual->class==TrueColor || LUI_Visual->class==DirectColor) {
       /* Initialize rtable[], gtable[], and btable[] */
       XColor xcol;
       int i;
       xcol.green = 0;
       xcol.blue = 0;
       for (i=0;i<256;i++) {
          xcol.red = i * 0xffff / 0xff;
          XAllocColor( LUI_Display, LUI_Colormap, &xcol );
          rtable[i] = xcol.pixel;
       }
       xcol.red = 0;
       xcol.blue = 0;
       for (i=0;i<256;i++) {
          xcol.green = i * 0xffff / 0xff;
          XAllocColor( LUI_Display, LUI_Colormap, &xcol );
          gtable[i] = xcol.pixel;
       }
       xcol.red = 0;
       xcol.green = 0;
       for (i=0;i<256;i++) {
          xcol.blue = i * 0xffff / 0xff;
          XAllocColor( LUI_Display, LUI_Colormap, &xcol );
          btable[i] = xcol.pixel;
       }
       pixelformat = PF_TRUECOLOR;
    }
    else if (LUI_Visual->class==PseudoColor) {
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
                LUI_XAllocColor( LUI_Display, LUI_Colormap,
                                 LUI_Visual->map_entries, &xcol );
                ctable8[r][g][b] = xcol.pixel;
             }
          }
       }
       pixelformat = PF_8BIT;
    }
    else {
       pixelformat = PF_XALLOC;
    }



    /* Create a dummy window.  This is needed because XCreateGC needs a
       drawable of the type which we'll be using in LUI.  Unfortunately,
       the LUI_RootWindow may not have the visual we want.  For example,
       on the SGI PI, the default root window is pseudo color but we want
       LUI to use TrueColor if it's available.  In this case using RootWindow
       in XCreateGC will make a GC of the wrong type. */
    attr.border_pixel = 0;
    attr.background_pixel = 255;
    attr.colormap = LUI_Colormap;
    LUI_DummyWindow = XCreateWindow( LUI_Display, LUI_RootWindow,
                                     0, 0, 10, 10, 1,
                                     LUI_Depth, InputOutput, LUI_Visual,
                                     CWBorderPixel | CWBackPixel | CWColormap,
                                     &attr );
    XSync( LUI_Display, 0 );
    /* Open the font */
    if ((LUI_Font = XLoadQueryFont(LUI_Display, LUI_FONT)) == NULL) {
        if ((LUI_Font = XLoadQueryFont(LUI_Display, "fixed")) == NULL) 
          LUI_FatalError("Can't open font", LUI_FONT);
    }
    LUI_Font_yoff = LUI_Font->max_bounds.ascent;
    LUI_Font_height = LUI_Font->max_bounds.ascent+LUI_Font->max_bounds.descent;
    LUI_Font_width = LUI_Font->max_bounds.width;

    /* Create some colors */
    LUI_CreateColors();

    /* Set the program name */
    strcpy(LUI_ProgramName, program_name);

    LUI_InitButtonPixmaps();   /* Added 3-15-91 by Brian Paul */
}



void LUI_SetMainWindow( Window w )
{
   main_window = w;
}




/*
 * Set Window Manager properties for the given window.
 * Input:  w - the window
 *         title - the window title/name
 */
static void set_wm_hints( Window w, char *title,
                          int x, int y, int width, int height )
{
#define X11R3
#ifdef X11R3
   XSizeHints size_hints;

   size_hints.flags  = USPosition | USSize;
   size_hints.x      = x;
   size_hints.y      = y;
   size_hints.width  = width;
   size_hints.height = height;

   XSetNormalHints( LUI_Display, w, &size_hints );
   XSetStandardProperties(LUI_Display, w,
                          title, title,
                          None, (char **)NULL, 0, &size_hints);
#else
   /* This is the "right" way to set a window's size & position but it */
   /* doesn't work on all systems!!!!! */
   XTextProperty name;
   XSizeHints size;
   XWMHints *wm;

   name.value = (unsigned char *) title;
   name.encoding = XA_STRING;
   name.format = 8;
   name.nitems = strlen(title);

   size.flags = ((x!=0 && y!=0) ? USPosition : 0)
              | ((width!=0 && height!=0) ? USSize : 0);
   size.x = x;
   size.y = y;
   size.width = width;
   size.height = height;

   wm = XAllocWMHints();
   if (main_window) {
      wm->flags = WindowGroupHint;
      wm->window_group = main_window;
   }
   else {
      wm->flags = 0;
   }

   XSetWMProperties( LUI_Display, w,  /* dpy, win ID */
                     &name, &name,    /* window name, icon name */
                     NULL, 0,         /* argv, argc */
                     &size, wm,       /* size, wm hints */
                     NULL );

   XFree( (char *) wm );
#endif
}





/*
 * set the pixel value (color) of LUI_Gc.
 */
void LUI_SetColor( unsigned long color )
{
    XSetForeground(LUI_Display, LUI_Gc, color );
}



void LUI_Cursor( Window window, int cursor )
{
    static int been_here = 0;

    if (!been_here) {
        been_here = 1;

        /* Create the cursors */
        LUI_CursorTable[LUI_MAIN_CURSOR]     
          = XCreateFontCursor(LUI_Display, XC_top_left_arrow /*XC_hand1*/ );
        LUI_CursorTable[LUI_DIAL_CURSOR]     
          = XCreateFontCursor(LUI_Display, XC_draft_small);
        LUI_CursorTable[LUI_DIAL_CW_CURSOR]  
          = XCreateFontCursor(LUI_Display, XC_exchange);
        LUI_CursorTable[LUI_DIAL_CCW_CURSOR] 
          = XCreateFontCursor(LUI_Display, XC_exchange);
        LUI_CursorTable[LUI_DIAL_RES_CURSOR] 
          = XCreateFontCursor(LUI_Display, XC_circle);
        LUI_CursorTable[LUI_QUESTION_CURSOR] 
          = XCreateFontCursor(LUI_Display, XC_question_arrow);
        LUI_CursorTable[LUI_DOWN_CURSOR]     
          = XCreateFontCursor(LUI_Display, XC_sb_down_arrow);
        LUI_CursorTable[LUI_UP_CURSOR]       
          = XCreateFontCursor(LUI_Display, XC_sb_up_arrow);
        LUI_CursorTable[LUI_UP_DOWN_CURSOR]  
          = XCreateFontCursor(LUI_Display, XC_sb_v_double_arrow);
        LUI_CursorTable[LUI_CLOCK_CURSOR]  
          = XCreateFontCursor(LUI_Display, XC_watch);
        LUI_CursorTable[LUI_PENCIL_CURSOR]  
          = XCreateFontCursor(LUI_Display, XC_pencil);
        LUI_CursorTable[LUI_ARROW_HAND_CURSOR]  
          = XCreateFontCursor(LUI_Display, XC_draft_large);
    }

    XDefineCursor(LUI_Display, window, LUI_CursorTable[cursor]);
}

Window LUI_CreateSndWindowAt( Window parent, int x, int y, int xs, int ys )
{
    XSetWindowAttributes window_attributes;
    Window window;
    int scrwidth, scrheight;
    int border;

    scrwidth = DisplayWidth (LUI_Display, LUI_Screen);
    scrheight = DisplayHeight(LUI_Display, LUI_Screen);

    if (!xs) xs = scrwidth;
    if (!ys) ys = scrheight;

    /* Make sure window isn't off bottom of screen */
    if (y+ys>scrheight) {
       y = scrheight - ys;
    }

    border = 0;

    /* Create the window - sized full screen if xs,xy = 0,0 */
    window_attributes.border_pixel = LUI_Color_black;
    window_attributes.background_pixel = LUI_Color_gray;
    window_attributes.background_pixmap = None;
    window_attributes.colormap = LUI_Colormap;
    window_attributes.event_mask = StructureNotifyMask;
    window = XCreateWindow(LUI_Display,
                           parent,
                           x, y, xs, ys, border,
                           LUI_Depth,  /* Used to be DefaultDepth -Brian */
                           InputOutput,
                           LUI_Visual,  /* Used to be DefaultVisual -Brian */
                           CWBackPixmap | CWColormap | CWBorderPixel
                           | CWBackPixel | CWEventMask,  &window_attributes);
    XSelectInput( LUI_Display, window,
                  ButtonPressMask|ButtonReleaseMask|PointerMotionMask
                  |StructureNotifyMask );

    /* Check for parent - If it is the root window, auto position this one */
    if (parent == DefaultRootWindow(LUI_Display)) {
       set_wm_hints( window, LUI_ProgramName, x, y, xs, ys );
#ifdef JUNK
        size_hints.flags  = USPosition | USSize;
        size_hints.x      = x;
        size_hints.y      = y;
        size_hints.width  = xs;
        size_hints.height = ys;

        XSetNormalHints(LUI_Display, window, &size_hints);
        XSetStandardProperties(LUI_Display, window, LUI_ProgramName, LUI_ProgramName,
                               None, (char **)NULL, 0, &size_hints);
#endif
    }

    /* Set the default cursor */
    LUI_Cursor(window, LUI_MAIN_CURSOR);

    return(window);
}




Window LUI_CreateWindowAt( Window parent, int x, int y, int xs, int ys )
{
    XSetWindowAttributes window_attributes;
/*JUNK    XSizeHints size_hints;
    XWMHints wm_hints;
*/
    Window window;
    int scrwidth, scrheight;
    int border;

    scrwidth = DisplayWidth (LUI_Display, LUI_Screen);
    scrheight = DisplayHeight(LUI_Display, LUI_Screen);

    if (!xs) xs = scrwidth;
    if (!ys) ys = scrheight;

/* MJK 12.04.98 */
/* 22Sep97  Phil McDonald */
   if (x < 0) x = scrwidth - xs;
   if (y < 0) y = scrheight - ys;




    /* Make sure window isn't off bottom of screen */
    if (y+ys>scrheight) {
       y = scrheight - ys;
    }

    border = 0;

    /* Create the window - sized full screen if xs,xy = 0,0 */
    window_attributes.border_pixel = LUI_Color_black;
    window_attributes.background_pixel = LUI_Color_gray;
    window_attributes.background_pixmap = None;
    window_attributes.colormap = LUI_Colormap;
    window = XCreateWindow(LUI_Display,
                           parent,
                           x, y, xs, ys, border,
                           LUI_Depth,  /* Used to be DefaultDepth -Brian */
                           InputOutput,
                           LUI_Visual,  /* Used to be DefaultVisual -Brian */
                           CWBackPixmap | CWColormap | CWBorderPixel
                           | CWBackPixel,  &window_attributes);
    XSync( LUI_Display, 0 );
    LUI_EventAdd(window, StructureNotifyMask, NULL);
    LUI_EventAdd(window, SubstructureNotifyMask, NULL);
    LUI_EventAdd(window, ButtonPressMask , NULL);
    LUI_EventAdd(window, ButtonReleaseMask, NULL); 
    LUI_EventAdd(window, VisibilityChangeMask, NULL);

    /* Check for parent - If it is the root window, auto position this one */
    if (parent == DefaultRootWindow(LUI_Display)) {
       set_wm_hints( window, LUI_ProgramName, x, y, xs, ys );
#ifdef JUNK
        size_hints.flags  = USPosition | USSize;
        size_hints.x      = x;
        size_hints.y      = y;
        size_hints.width  = xs;
        size_hints.height = ys;

        XSetNormalHints(LUI_Display, window, &size_hints);
        XSetStandardProperties(LUI_Display, window, LUI_ProgramName, LUI_ProgramName,
                               None, (char **)NULL, 0, &size_hints);
#endif
    }
    
    /* Set the default cursor */
    LUI_Cursor(window, LUI_MAIN_CURSOR);

    return(window);
}


Window LUI_CreateWindow( Window parent, int xs, int ys )
{
    XSetWindowAttributes window_attributes;
/*JUNK
    XSizeHints size_hints;
    XWMHints wm_hints;
*/
    Window window;

    /* Create the window - sized full screen if xs,xy = 0,0 */
    window_attributes.background_pixmap = None;
    window_attributes.background_pixel = LUI_Color_gray;  /*0x777777;*/
    window_attributes.background_pixmap = None;
    window_attributes.colormap = LUI_Colormap;
    window = XCreateWindow(LUI_Display,
                           parent,
                           0, 0,
                           xs ? xs : DisplayWidth (LUI_Display, LUI_Screen),
                           ys ? ys : DisplayHeight(LUI_Display, LUI_Screen),
                           0,
                           LUI_Depth,   /* Used to be DefaultDepth -Brian */
                           InputOutput,
                           LUI_Visual,  /* Used to be DefaultVisual -Brian */
                           CWBackPixmap| CWColormap | CWBorderPixel
                           | CWBackPixel, &window_attributes);

    /* Check for parent - If it is the root window, auto position this one */
    if (parent == DefaultRootWindow(LUI_Display)) {
    set_wm_hints( window, LUI_ProgramName, 0, 0, xs, ys );
#ifdef JUNK
        size_hints.flags  = /*USPosition | */USSize;
        size_hints.x      = 0;
        size_hints.y      = 0;
        size_hints.width  = xs;
        size_hints.height = ys;

        XSetNormalHints(LUI_Display, window, &size_hints);
        XSetStandardProperties(LUI_Display, window, LUI_ProgramName, LUI_ProgramName,
                               None, (char **)NULL, 0, &size_hints);
#endif
    }

    /* Set the default cursor */
    LUI_Cursor(window, LUI_MAIN_CURSOR);

    return(window);
}


void LUI_DestroyWindow( Window window )
{
   LUI_DestroyWidgetsInWindow( window );
   LUI_EventRemove( window );
   XDestroyWindow(LUI_Display, window);
}



void LUI_MoveResizeWindow( Window window, int x, int y, int xs, int ys )
{

   XSizeHints size_hints;
   int scrheight;
/* MJK 12.04.98 */
/* 22Sep97  Phil McDonald */
   int scrwidth;

   scrwidth  = DisplayWidth(LUI_Display, LUI_Screen);
   scrheight = DisplayHeight(LUI_Display, LUI_Screen);

   if (x < 0) x = scrwidth - xs;
   if (y < 0) y = scrheight - ys;
#ifdef JOHAN
   scrheight = DisplayHeight(LUI_Display, LUI_Screen);
#endif

   /* Make sure window isn't off bottom of screen */
   if (y+ys>scrheight) {
      y = scrheight - ys;
   }
 
   size_hints.flags  = USPosition | USSize;
   size_hints.x      = x;
   size_hints.y      = y;
   size_hints.width  = xs;
   size_hints.height = ys;
    
   XSetNormalHints(LUI_Display, window, &size_hints);
   XSetStandardProperties(LUI_Display, window, LUI_ProgramName,
                          LUI_ProgramName, None, NULL, 0, &size_hints);
   /* try to move & resize by both methods on HP */
   XMoveResizeWindow(LUI_Display, window, x, y, xs, ys);
}


/* MJK 12.04.98 */
/* 16Feb98  Phil McDonald */
void LUI_ResizeWindow( Window window, int xs, int ys )
{
    int x, y;
    unsigned int width, height, bw, depth;
    Window root;

    /* get current window size */
    XGetGeometry( LUI_Display, window, &root, &x, &y, &width, &height,
                  &bw, &depth );

    LUI_MoveResizeWindow( window, x, y, xs, ys );
/*
    XResizeWindow( LUI_Display, window, xs, ys );
*/
}
#ifdef JOHAN
void LUI_ResizeWindow( Window window, int xs, int ys )
{
#ifdef JUNK
    int x, y;
    unsigned int width, height, bw, depth;
    Window root;

    /* get current window size */
    XGetGeometry( LUI_Display, window, &root, &x, &y, &width, &height,
                  &bw, &depth );

    LUI_MoveResizeWindow( window, x, y, xs, ys );
#endif
    XResizeWindow( LUI_Display, window, xs, ys );
}
#endif




/*
 * Deallocate a color as returned by LUI_AllocColor
 */
void LUI_FreeColor( unsigned long pixel )
{
   if (pixelformat==PF_XALLOC) {
      XFreeColors( LUI_Display, LUI_Colormap, &pixel, 1, 0 );
   }
}





unsigned long LUI_MakeColor( int r, int g, int b )
{
   return LUI_AllocateColor( (float) r / 255.0,
                             (float) g / 255.0,
                             (float) b / 255.0 );
}



        
static LUI_DrawBorderAt( Window window, int x, int y, int width, int height,
                         int border_size, int invert )
{
    XPoint pts[4];
    int bs;
    int ox0, ox1, ox2, ox3, oy0, oy1, oy2, oy3;
    int ix0, ix1, ix2, ix3, iy0, iy1, iy2, iy3;
    GC top_gc, bottom_gc;

    if (invert) {
       top_gc = LUI_GC_top;
       bottom_gc = LUI_GC_bottom;
    }
    else {
       top_gc = LUI_GC_bottom;
       bottom_gc = LUI_GC_top;
    }
    bs  = border_size;

    ox0 = x;         oy0 = y;
    ox1 = x + width; oy1 = y;
    ox2 = x + width; oy2 = y + height;
    ox3 = x;         oy3 = y + height;
    ix0 = ox0 + bs;  iy0 = oy0 + bs;
    ix1 = ox1 - bs;  iy1 = oy1 + bs;
    ix2 = ox2 - bs;  iy2 = oy2 - bs;
    ix3 = ox3 + bs;  iy3 = oy3 - bs;

    /* top */
    pts[0].x = ox0; pts[0].y = oy0; pts[1].x = ox1; pts[1].y = oy1;
    pts[2].x = ix1; pts[2].y = iy1; pts[3].x = ix0; pts[3].y = iy0;
    XFillPolygon(LUI_Display, window, top_gc, pts, 4, Convex, CoordModeOrigin);

    /* right */
    pts[0].x = ox1; pts[0].y = oy1; pts[1].x = ox2; pts[1].y = oy2;
    pts[2].x = ix2; pts[2].y = iy2; pts[3].x = ix1; pts[3].y = iy1;
    XFillPolygon(LUI_Display, window, bottom_gc, pts, 4, Convex, CoordModeOrigin);

    /* bottom */
    pts[0].x = ox2; pts[0].y = oy2; pts[1].x = ox3; pts[1].y = oy3;
    pts[2].x = ix3; pts[2].y = iy3; pts[3].x = ix2; pts[3].y = iy2;
    XFillPolygon(LUI_Display, window, bottom_gc, pts, 4, Convex, CoordModeOrigin);
    
    /* left */
    pts[0].x = ox0; pts[0].y = oy0; pts[1].x = ox3; pts[1].y = oy3;
    pts[2].x = ix3; pts[2].y = iy3; pts[3].x = ix0; pts[3].y = iy0;
    XFillPolygon(LUI_Display, window, top_gc, pts, 4, Convex, CoordModeOrigin);
}


void LUI_FrameWindow( Window window, int width, int height, int border_size )
{
    LUI_DrawBorderAt(window, 0, 0, width, height, border_size, 1);
}


void LUI_FrameWindow2( Window  window, int width, int height, int border_size )
{
    LUI_DrawBorderAt(window, 0, 0, width, height, border_size, 0);
}




/*
 * Draw a "3-D" border.
 * Input:  window - which window
 *         x, y - upper-left position
 *         width, height - outside dimensions
 *         thickness - width of border in pixels
 *         raised - 1 = draw as raised border, 0 = draw as lowerd border
 */
void LUI_DrawFrame( Window window, int x, int y, int width, int height,
                    int thickness, int raised )
{
   XPoint pts[4];
   GC top_gc, bottom_gc;

   if (thickness<=0) {
      return;
   }

   if (raised) {
      top_gc = LUI_GC_top;
      bottom_gc = LUI_GC_bottom;
   }
   else {
      top_gc = LUI_GC_bottom;
      bottom_gc = LUI_GC_top;
   }

   /* top */
   XFillRectangle( LUI_Display, window, top_gc, x, y, width, thickness );

   /* left */
   XFillRectangle( LUI_Display, window, top_gc, x, y, thickness, height );

   /* bottom */
   pts[0].x = x + thickness;   pts[0].y = y + height - thickness;
   pts[1].x = x + width;       pts[1].y = y + height - thickness;
   pts[2].x = x + width;       pts[2].y = y + height;
   pts[3].x = x;               pts[3].y = y + height;
   XFillPolygon( LUI_Display, window, bottom_gc,
                 pts, 4, Convex, CoordModeOrigin );

   /* right */
   pts[0].x = x + width - thickness,  pts[0].y = y + thickness;
   pts[1].x = x + width;              pts[1].y = y;
   pts[2].x = x + width;              pts[2].y = y + height;
   pts[3].x = x + width - thickness;  pts[3].y = y + height;
   XFillPolygon( LUI_Display, window, bottom_gc,
                 pts, 4, Convex, CoordModeOrigin );
}



/******************************************************************************/
LUI_DrawFlatBorderAt(window, x, y, width, height, border_size, color)
    Window window;
    int x, y, width, height, border_size;
    unsigned long color;
{
    XPoint pts[4];
    int bs;
    int ox0, ox1, ox2, ox3, oy0, oy1, oy2, oy3;
    int ix0, ix1, ix2, ix3, iy0, iy1, iy2, iy3;

    bs  = border_size;

    ox0 = x;         oy0 = y;
    ox1 = x + width; oy1 = y;
    ox2 = x + width; oy2 = y + height;
    ox3 = x;         oy3 = y + height;
    ix0 = ox0 + bs;  iy0 = oy0 + bs;
    ix1 = ox1 - bs;  iy1 = oy1 + bs;
    ix2 = ox2 - bs;  iy2 = oy2 - bs;
    ix3 = ox3 + bs;  iy3 = oy3 - bs;

    LUI_SetColor(color);

    pts[0].x = ox0; pts[0].y = oy0; pts[1].x = ox1; pts[1].y = oy1;
    pts[2].x = ix1; pts[2].y = iy1; pts[3].x = ix0; pts[3].y = iy0;
    XFillPolygon(LUI_Display, window, LUI_Gc, pts, 4, Convex, CoordModeOrigin);

    pts[0].x = ox1; pts[0].y = oy1; pts[1].x = ox2; pts[1].y = oy2;
    pts[2].x = ix2; pts[2].y = iy2; pts[3].x = ix1; pts[3].y = iy1;
    XFillPolygon(LUI_Display, window, LUI_Gc, pts, 4, Convex, CoordModeOrigin);

    pts[0].x = ox2; pts[0].y = oy2; pts[1].x = ox3; pts[1].y = oy3;
    pts[2].x = ix3; pts[2].y = iy3; pts[3].x = ix2; pts[3].y = iy2;
    XFillPolygon(LUI_Display, window, LUI_Gc, pts, 4, Convex, CoordModeOrigin);
    
    pts[0].x = ox0; pts[0].y = oy0; pts[1].x = ox3; pts[1].y = oy3;
    pts[2].x = ix3; pts[2].y = iy3; pts[3].x = ix0; pts[3].y = iy0;
    XFillPolygon(LUI_Display, window, LUI_Gc, pts, 4, Convex, CoordModeOrigin);
}

/******************************************************************************/
LUI_DrawDropShadow(window, x, y, w, h, d, color)
    Window window;
    int x, y, w, h, d;
    unsigned long color;
{
    XPoint p[6];

    LUI_SetColor(color);

    p[0].x = x + d;  p[0].y = y+h  ;
    p[1].x = x+w  ;  p[1].y = y+h  ;
    p[2].x = x+w  ;  p[2].y = y + d;
    p[3].x = x+w+d;  p[3].y = y + d;
    p[4].x = x+w+d;  p[4].y = y+h+d;
    p[5].x = x + d;  p[5].y = y+h+d;
    XFillPolygon(LUI_Display, window, LUI_Gc, p, 6, Convex, CoordModeOrigin);

#ifdef NOT_YET
    LUI_SetColor(0xffffff);
    XDrawLine(LUI_Display, window, LUI_Gc, x, y, w - d, y);
    XDrawLine(LUI_Display, window, LUI_Gc, x, y, x, h - d);
#endif
}


char **LUI_MakeLabel( char *string )
{
    char **argv, *arg0;

    /* Malloc address for char array */
    if (!(argv = (char **)malloc(sizeof(char **)))) 
      LUI_FatalError("LUI_MakeLabel: can't malloc.\n",NULL);

    /* Malloc string (This fucker bit hard - let's kludge up a big malloc */
    if (!(arg0 = (char *)malloc(128)))
      LUI_FatalError("LUI_MakeLabel: can't malloc.\n",NULL);

    /* Copy string */
    strcpy(arg0, string);

    /* Set address */
    *argv = arg0;

    /* Return pointer */
    return(argv);
}

/******************************************************************************/
#ifdef LEAVEOUT
 Window 
LUI_CreateIcon(name, xpos, ypos)
    char *name;
    int xpos, ypos;
{
    XSetWindowAttributes window_attributes;
    Window window;
    int w, h;

    w = 150;
    h = 50;

    /* Create the icon window */
    window_attributes.border_pixel     = WhitePixel(LUI_Display, LUI_Screen);
    window_attributes.background_pixel = BlackPixel(LUI_Display, LUI_Screen);
    window_attributes.bit_gravity      = CenterGravity;
    window_attributes.background_pixmap = None;
    window_attributes.colormap = LUI_Colormap;
    window = XCreateWindow(LUI_Display,
                           LUI_RootWindow,
                           xpos, ypos,
                           w, h,
                           0,
                           LUI_Depth,
                           InputOutput,
                           LUI_Visual,
                           CWBorderPixel | CWBitGravity | CWBackPixel
                           | CWColormap,  &window_attributes);

    /* Add the label */
    LUI_LabelOpen(name, 1, (char **)LUI_MakeLabel(name), 
              window,
              0, 0, w, h, 10,
              NULL);
    LUI_LabelClose(name);
    LUI_LabelVisible(name, 1);
    LUI_LabelColor(name, LUI_DefaultHue);

    return(window);
}
#endif

/*****************************************************************************/
#ifdef LEAVEOUT
 Window 
LUI_CreatePictureIcon(name, xpos, ypos)
    char *name;
    int xpos, ypos;
{
    XSetWindowAttributes window_attributes;
    Window window;
    Pixmap pixmap, LUI_ReadPixmap();
    int w, h, LUI_PictureIconProcess();
    char buf[128];

    sprintf(buf, "%s.x", name);
    if (!(pixmap = LUI_ReadPixmap(buf, 0, 0, &w, &h))) {
        LUI_CreateIcon(name, xpos, ypos);
        return;
    }

    /* Create the icon window */
    window_attributes.border_pixel = 0;
    window_attributes.background_pixmap = pixmap;
    window_attributes.background_pixel = 255;
    window_attributes.colormap = LUI_Colormap;
    window = XCreateWindow(LUI_Display,
                           LUI_RootWindow,
                           xpos, ypos,
                           w, h,
                           0,
                           LUI_Depth,
                           InputOutput,
                           LUI_Visual,
                           CWBackPixmap | CWColormap | CWBackPixel
                           | CWBorderPixel, &window_attributes);

    LUI_EventAdd(window, ExposureMask, LUI_PictureIconProcess);
    
    return(window);
}
#endif



#ifdef LEAVEOUT
LUI_PictureIconProcess(window, event)
    Window window;
    XEvent *event;
{
    XWindowAttributes wattr;

    XGetWindowAttributes(LUI_Display, window, &wattr);

    LUI_FrameWindow(window, wattr.width, wattr.height, 8);
}
#endif


#ifdef LEAVEOUT
 Pixmap
LUI_ReadPixmap(path, x, y, w, h)
    char *path;
    int *w, *h, x, y;
{
    XImage *ximage;
    Pixmap pixmap;
    int xpad, bytes_per_line, format, offset, fd, width, height, size;
    unsigned long *imagebuf;

    /* Setup for image */
    format         = ZPixmap;
    offset         = 0;
    xpad           = 32;
    bytes_per_line = 0;

    if ((fd = open(path, O_RDONLY)) == -1) {
        fprintf(stderr, "LUI_ReadPixmap(): can't open pixmap %s\n", path);
        return((Pixmap)0);
    }

    /* Read dimensions */
    if (read(fd, &width,  sizeof(width )) != sizeof(width ) ||
        read(fd, &height, sizeof(height)) != sizeof(height)) {
        fprintf(stderr, "LUI_ReadPixmap(): can't read dimensions for pixmap %s\n", path);
        return((Pixmap)0);
    }

    *w = width;
    *h = height;

    /* Read image data */
    size = width * height * sizeof(unsigned long);
    if (!(imagebuf = (unsigned long *) malloc(size))) {
        fprintf(stderr, "LUI_ReadPixmap(): can't allocate imagebuf for pixmap %s\n", path);
        return((Pixmap)0);
    }
        
    if (read(fd, imagebuf, size) != size) {
        fprintf(stderr, "LUI_ReadPixmap(): can't read image data for pixmap %s\n", path);
        return((Pixmap)0);
    }
        
    /* Create an image */
    ximage = XCreateImage(LUI_Display, 
                          LUI_Visual,
                          LUI_Depth, 
                          format,
                          offset,
                          (char *) imagebuf, 
                          width, height,
                          xpad,
                          bytes_per_line);

    /* Create a pixmap */
    pixmap = XCreatePixmap(LUI_Display, 
                           LUI_RootWindow, 
                           width, height, 
                           LUI_Depth);
        
    /* Put the image into the pixmap */
    XPutImage(LUI_Display,
              pixmap,
              LUI_GC_black,
              ximage, x, y, 0, 0, width, height);
    
    free(imagebuf);
    close(fd);

    return(pixmap);
}
#endif



void LUI_RgbShade( unsigned long color, float shade, int *r, int *g, int *b )
{
   int red, green, blue;

    red = (LUI_DefaultHue >> 16) & 0xff;
    green = (LUI_DefaultHue >> 8) & 0xff;
    blue = LUI_DefaultHue & 0xff;

    *r = (int) ( (float) red * shade );
    *g = (int) ( (float) green * shade );
    *b = (int) ( (float) blue * shade );
}



#define MAX_CLOCKS 32

/******************************************************************************/
LUI_Timer(clock, running, seconds)
    int clock;
    int running;
    float *seconds;
{
    struct timeval tp;
    static int times_called[MAX_CLOCKS];
    static float start_milleseconds[MAX_CLOCKS], lapsed_milleseconds;

    /* Error test */
    if (clock < 0 || clock >= MAX_CLOCKS) {
        fprintf(stderr, "LUI_Timer(): bad clock selection %d\n", clock);
        return(0);
    }

    /* Get system time */
    if (gettimeofday(&tp, (struct timezone *)0)) 
      fprintf(stderr, "LUI_Timer(): gettimeofday botch!\n");

    /* Set the timer */
    if (!running) {
        start_milleseconds[clock] = tp.tv_sec * 1000000 + tp.tv_usec;
        times_called[clock] = 0;
    }

    /* Get current time */
    else {
        lapsed_milleseconds = (tp.tv_sec * 1000000 + tp.tv_usec) - start_milleseconds[clock];
        
        *seconds = lapsed_milleseconds / 1000000.0;
    }

    return(times_called[clock]++);
}

/** Originally:
static char RuntimeDir[1000] = "/public/local/lib/stellar/LUI";
*/
/*** Changed by BrianP on 2-22-91 to: ***/
static char RuntimeDir[1000] = "lui";




#ifdef ultrix
/* strdup() is not available on ultrix! */
char *strdup( char *s )
{
   int len = strlen(s);
   char *s2 = (char *) malloc( len + 1 );
   strcpy( s2, s );
   return s2;
}
#endif
