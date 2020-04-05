/*  graphics.gl.c */

/* Graphics functions for SGI IrisGL */
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
 * NOTES:
 *   1. lmcolor( LMC_COLOR ) is the default mode.
 *   2. lighting is normally disabled.
 */


#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "graphics.h"
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <gl/gl.h>
#include <gl/image.h>
#include <gl/device.h>
#include <gl/glws.h>
#include <fmclient.h>
#include "globals.h"
#include "matrix.h"
#include "misc.h"


#ifndef M_PI
#  define M_PI 3.14159265
#endif


/*
 * Private GL variables:
 */
static long zfar;                /* max Z buffer value */

static Matrix Identity = { 1.0, 0.0, 0.0, 0.0,
                           0.0, 1.0, 0.0, 0.0,
                           0.0, 0.0, 1.0, 0.0,
                           0.0, 0.0, 0.0, 1.0 };

static int pretty_flag = 0;


/* Default font and its handle */
#define DEFAULT_FONT "Times-Roman"



#define VIEWOBJ 10


/* setup the default material surface description: */
static float mat1[] = {
   AMBIENT, 0.3, 0.3, 0.3,   /* surface ambient color */
   DIFFUSE, 0.7, 0.7, 0.7,   /* surface diffuse color */
   LMNULL
};

/* setup the light source descriptions: */
static float light1[] = {
   LCOLOR, 0.8, 0.8, 0.8,          /* light color */
   POSITION, 0.0, 0.0, 10.0, 0.0,  /* light location */
   LMNULL
};

static float light2[] = {
   LCOLOR, 0.8, 0.8, 0.8,          /* light color */
   POSITION, 0.0, 0.0, -10.0, 0.0,  /* light location */
   LMNULL
};

/* setup the lighting model description: */
static float lmodel[] = {
   AMBIENT, 0.2, 0.2, 0.2,         /* ambient color */
   LOCALVIEWER, 0.0,               /* infinite viewpoint */
   TWOSIDE, 0,
   LMNULL
};

   /* Accumulation buffer antialiasing */
#define AA_INC  1.0
static float  xoffsets[AA_PASSES] =
    { -AA_INC, 0.0, AA_INC,  -AA_INC, 0.0, AA_INC,  -AA_INC, 0.0, AA_INC };
static float  yoffsets[AA_PASSES] =
    { -AA_INC, -AA_INC, -AA_INC,  0.0, 0.0, 0.0,  AA_INC, AA_INC, AA_INC };

static unsigned short screen1[16] = { /* 75% */
      0x7777, 0xdddd, 0x7777, 0xdddd, 0x7777, 0xdddd, 0x7777, 0xdddd,
      0x7777, 0xdddd, 0x7777, 0xdddd, 0x7777, 0xdddd, 0x7777, 0xdddd
};
static unsigned short screen2[16] = { /* 50% */
      0xaaaa, 0x5555, 0xaaaa, 0x5555, 0xaaaa, 0x5555, 0xaaaa, 0x5555,
      0xaaaa, 0x5555, 0xaaaa, 0x5555, 0xaaaa, 0x5555, 0xaaaa, 0x5555
};
static unsigned short screen3[16] = { /* 25% */
      0x8888, 0x2222, 0x8888, 0x2222, 0x8888, 0x2222, 0x8888, 0x2222,
      0x8888, 0x2222, 0x8888, 0x2222, 0x8888, 0x2222, 0x8888, 0x2222
};




/*
 * Load the font specified by ctx->FontName
 */
static void load_font( Context ctx )
{
   static int init_flag = 1;
   fmfonthandle fnt;
   fmfontinfo info;

   if (init_flag) {
      fminit();   /* Initialize font manager */
      init_flag = 0;
   }

   if (!ctx->gfx.font) {
      if (ctx->FontName[0]==0) {
         /* use default font */
         strcpy( ctx->FontName, DEFAULT_FONT );
         ctx->FontHeight = 20;
      }
      fnt = fmfindfont( ctx->FontName );
      if (!fnt) {
         fprintf(stderr, "Unable to load font %s\n", ctx->FontName );
         exit(1);
      }

      ctx->gfx.font = fmscalefont( fnt, (double) ctx->FontHeight );
      
      if (fmgetfontinfo( ctx->gfx.font, &info)==-1) {
         fprintf(stderr, "Error loading font %s\n", ctx->FontName );
         exit(1);
      }

      ctx->FontHeight = (int) (0.8 * info.ysize);
      ctx->FontDescent = info.yorig;
   }

   fmsetfont( ctx->gfx.font );
}



/*
 * Do SGI IRIS GL specific initializations.  This is called once.
 */
void init_graphics2( void )
{
   if (getgdesc(GD_BITS_ACBUF)>0)
      HQR_available = 1;
   else
      HQR_available = 0;
   Perspec_available = 1;
   fminit();
}




void terminate_graphics( void )
{
   /* nothing */
}


/* Use GL windowing functions? */
#define GL_WINDOW


/*
 * Free the graphics resources attached to a vis5d context.
 */
void free_graphics( Context ctx )
{
#ifdef GL_WINDOW
   winclose( ctx->gfx.winid );
#else
   if (ctx->gfx.gl_ctx) {
      glXDestroyContext( GfxDpy, ctx->gfx.gl_ctx );
      ctx->gfx.gl_ctx = 0;
   }
   if (ctx->GfxWindow) {
      XDestroyWindow( GfxDpy, ctx->GfxWindow );
      ctx->GfxWindow = 0;
   }
#endif
}




/*
 * Specify which context / 3D window is the current one for rendering.
 */
static Context current_ctx = NULL;

#ifdef GL_WINDOW
#  define SET_GFX_CONTEXT( c )                            \
        if (c!=current_ctx) {                             \
           printf("Gfx context switch!\n");               \
           winset( c->gfx.winid );                        \
           fmsetfont( c->gfx.font );                      \
           current_ctx = c;                               \
        }
#else
#  define SET_GFX_CONTEXT( c )                            \
        if (c!=current_ctx) {                             \
           printf("Gfx context switch!\n");               \
           if (GLXwinset( GfxDpy, c->GfxWindow )!=0) {    \
              printf("Error: GLXwinset failed\n");        \
           }                                              \
           fmsetfont( c->gfx.font );                      \
           current_ctx = c;                               \
        }
#endif



/*
 * One-time GL initializations.
 */
static void one_time_init( Context ctx )
{
   lmdef( DEFMATERIAL, 10, 0, mat1 );   /* define material, light, model */
   lmdef( DEFLIGHT, 20, 0, light1 );
   lmdef( DEFLIGHT, 21, 0, light2 );
   lmdef( DEFLMODEL, 30, 0, lmodel );
   lmbind( MATERIAL, 10 );
   lmbind( LIGHT1, 20 );
   lmbind( LIGHT2, 21 );
   lmbind( LMODEL, 30 );

   /* screen door transparency patterns */
   defpattern( 1, 16, screen1 );
   defpattern( 2, 16, screen2 );
   defpattern( 3, 16, screen3 );
   setpattern(0);

   subpixel(TRUE);   /* for faster rendering on VGX */

   ctx->gfx.CurColor = 0xffffffff;
}



/*
  Assume that GL graphics has been initialized;
              the Font Manager has been initialized;
              the graphics is running in the foreground;
              the matrix-mode is MVIEWING;
  We do not assume that a window is open.
*/
void once_for_all_contexts_init(void)
{
   zfar = getgdesc( GD_ZMAX );

   /* Note: definitions of LIGHTs is done in IISS, on per-Frame basis */

   lmdef( DEFMATERIAL, 10, 0, mat1 );
   lmdef( DEFLMODEL, 30, 0, lmodel );
   /* Note: bindings will be handled in IISS, during cell settings */

   /* screen door transparency patterns */
   defpattern( 1, 16, screen1 );
   defpattern( 2, 16, screen2 );
   defpattern( 3, 16, screen3 );

   subpixel(TRUE);   /* for faster rendering on VGX */

} /* once_for_all_contexts_init <<< */



/*
 * Mike Manyin's context-init function
 */
void
context_init(
  Context ctx,
  long win_id,
  int width,
  int height
)
{

   ctx->gfx.winid = win_id;
   ctx->WinWidth = width;
   ctx->WinHeight = height;

/* This may be obsolete since I changed the font setup code:
   ctx->gfx.font = DefaultFont;
   ctx->FontHeight = get_font_height(ctx->gfx.font);
   ctx->FontDescent = get_font_descent(ctx->gfx.font);
*/

   /* don't let the regular graphics initialization be done: */
   ctx->gfx.init_flag = 1;

   /* cursor color: */
   ctx->gfx.CurColor = 0xffffffff;

   /* Set graphics based on the context: */
/*needed?   fmsetfont( ctx->gfx.font );*/

} /* context_init <<< */



/*
 * Create a 3-D rendering window for the given vis5d context.
 */
int make_3d_window( Context ctx, char *title, int xpos, int ypos,
                    int width, int height )
{
#ifdef GL_WINDOW
   static long Black = 0xff000000;
   char title2[100];

   strcpy( title2, title );  /* uniquify the window title */
   strcat( title2, "   " );

   foreground();

   if (width==ScrWidth && height==ScrHeight) {
      noborder();
      prefposition( 0, ScrWidth-1, 0, ScrHeight-1 );
   }
   else {
      prefposition( xpos, xpos+width-1,
                    ScrHeight-ypos-1, ScrHeight-ypos-height );
   }
   /* setup 24-bit, double buffer, z-buffered window */
   ctx->gfx.winid = winopen( title2 );
   winconstraints();
   RGBmode();
   doublebuffer();
   zbuffer( TRUE );
   gconfig();

   /* find X window pointer to the GL window */
   ctx->GfxWindow = find_window( GfxDpy, GfxScr, RootWindow(GfxDpy,GfxScr), title2 );
   if (!ctx->GfxWindow) {
      fprintf(stderr,"Unable to create graphics window\n");
      exit(1);
   }
   wintitle( title );    /* restore window title */

   /* select which X events to receive */
   XSelectInput( GfxDpy, ctx->GfxWindow, ExposureMask | ButtonMotionMask
                | KeyPressMask | ButtonPressMask | ButtonReleaseMask
                | StructureNotifyMask | VisibilityChangeMask );


   /* Set WM hints */
   {
      XSizeHints size;

      size.x = xpos;
      size.y = ypos;
      size.width  = width;
      size.height = height;
      size.max_width = ScrWidth;
      size.max_height = ScrHeight;
      size.flags = USSize | USPosition; /* | PMaxSize */;

      XSetWMSizeHints( GfxDpy, ctx->GfxWindow, &size, XA_WM_SIZE_HINTS );
      XSetWMNormalHints( GfxDpy, ctx->GfxWindow, &size );
   }

   zfunction( ZF_LESS );  /* improves transparency quality */
   zfar = getgdesc( GD_ZMAX );

   czclear( Black, zfar );
   swapbuffers();
   czclear( Black, zfar );

   mmode( MVIEWING );     /* multi-matrix mode */

   {
      long width, height;
      getsize( &width, &height );
      ctx->WinWidth = width;
      ctx->WinHeight = height;
   }
#else
   /* Use GLX interface */
   GLXconfig params[4];
   GLXconfig *config, *c;
   XSetWindowAttributes attr;
   XVisualInfo* vis;
   Window root;
   int attr_flags;

   /* make list of window characteristics needed */
   params[0].buffer = GLX_NORMAL;
   params[0].mode = GLX_RGB;
   params[0].arg = TRUE;
   params[1].buffer = GLX_NORMAL;
   params[1].mode = GLX_DOUBLE;
   params[1].arg = TRUE;
   params[2].buffer = GLX_NORMAL;
   params[2].mode = GLX_ZSIZE;
   params[2].arg = GLX_NOCONFIG;
   params[3].buffer = 0;
   params[3].mode = 0;
   params[3].arg = 0;

   config = GLXgetconfig( GfxDpy, GfxScr, params );
   if (config==NULL) {
      printf("Error: unable to get double-buffered RGB configuration\n");
      return 0;
   }

   XSetErrorHandler(0);  /* get X errors instead of GL errors */

   /* Search the returned config array for visual info */
   vis = NULL;
   for (c=config;c->buffer;c++) {
      if (c->mode==GLX_VISUAL) {
         XVisualInfo template;
         int nret;
         template.visualid = c->arg;
         template.screen = DefaultScreen(GfxDpy);
         vis = XGetVisualInfo(GfxDpy, VisualScreenMask|VisualIDMask,
                                          &template, &nret);
         break;
      }
   }
   if (vis==NULL) {
      printf("Error: unable to obtain db, RGB visual\n");
      return 0;
   }

   /* Set initial window attributes */
   if (DisplayMatrix == 11 && BigWindow != NULL){
      root = RootWindow( GfxDpy, GfxScr ); 
   }
   else {
      root = BigWindow;
   }
   attr.colormap = XCreateColormap( GfxDpy, root,
                                    vis->visual, AllocNone );
   attr.event_mask = ExposureMask | PointerMotionMask | KeyPressMask
                | ButtonPressMask | ButtonReleaseMask | StructureNotifyMask
                | VisibilityChangeMask;

   attr.background_pixel = 0;
   attr.border_pixel = 0;
   attr_flags = CWColormap | CWEventMask | CWBackPixel | CWBorderPixel;

   /* Create the window */
   ctx->GfxWindow = XCreateWindow( GfxDpy, root,
                                  xpos, ypos, width, height, 0,
                                  vis->depth, InputOutput, vis->visual,
                                  attr_flags, &attr );


   GfxVisual = vis->visual;
   GfxDepth = vis->depth;
   GfxColormap = attr.colormap;

   /* Set WM hints */
   {
      XTextProperty name;
      XSizeHints size;
      XWMHints wm;

      name.value = (unsigned char *) title;
      name.encoding = XA_STRING;
      name.format = 8;
      name.nitems = strlen(title);

      size.x = xpos;
      size.y = ypos;
      size.width  = width;
      size.height = height;
      size.flags = USSize | USPosition;

      wm.flags = WindowGroupHint;
      wm.window_group = ctx->GfxWindow;

      XSetWMProperties( GfxDpy, ctx->GfxWindow,  /* dpy, win ID */
                        &name, &name,   /* window name, icon name */
                        NULL, 0,        /* argv, argc */
                        &size, &wm,     /* size, wm hints */
                        NULL            /* class */  );
   }

   /* put the window id back into the configuration array. */
   for (c=config;c->buffer;c++) {
      if ((c->buffer==GLX_NORMAL) && (c->mode==GLX_WINDOW)) {
         c->arg = ctx->GfxWindow;
         break;
      }
   }

   /* Link GL to the X window */
   if (GLXlink( GfxDpy, config )) {
      printf("Error: GLXlink() failed\n");
      return 0;
   }

   /* Map the window */
   XMapWindow( GfxDpy, ctx->GfxWindow );

   /* Make ctx->GfxWindow the current GL window */
   if (GLXwinset( GfxDpy, ctx->GfxWindow )!=0) {
      printf("Error: GLXwinset failed\n");
   }

   zbuffer( TRUE );
   zfunction( ZF_LESS );  /* improves transparency quality */
   zfar = getgdesc( GD_ZMAX );

   mmode( MVIEWING );     /* multi-matrix mode */

   WinWidth = width;
   WinHeight = height;
#endif

   viewport( 0, (Screencoord) ctx->WinWidth-1, 0, (Screencoord) ctx->WinHeight-1);


   lmdef( DEFMATERIAL, 10, 0, mat1 );   /* define material, light, model */
   lmdef( DEFLIGHT, 20, 0, light1 );
   lmdef( DEFLIGHT, 21, 0, light2 );
   lmdef( DEFLMODEL, 30, 0, lmodel );
   lmbind( MATERIAL, 10 );
   lmbind( LIGHT1, 20 );
   lmbind( LIGHT2, 21 );
   lmbind( LMODEL, 30 );

   /* screen door transparency patterns */
   defpattern( 1, 16, screen1 );
   defpattern( 2, 16, screen2 );
   defpattern( 3, 16, screen3 );
   setpattern(0);

   subpixel(TRUE);   /* for faster rendering on VGX */

   ctx->gfx.CurColor = 0xffffffff;

   if (!ctx->gfx.init_flag) {
      one_time_init(ctx);
      ctx->gfx.init_flag = 1;
   }

   return 1;
}



/*
 * Bind a Vis5D context to the given IRIS GL window.
 * Input:  ctx - the vis5d context
 *         winid - the IRIS GL window ID
 *         title - the window's title, needed to find corresponding X window ID
 * Return:  1 = ok, 0 = error.
 */
int use_gl_window( Context ctx, Display *dpy, Window window, long winid )
{
   long w, h;

   assert( dpy );
   assert( window );
   assert( winid>=0 );

   ctx->GfxWindow = window;
   ctx->gfx.winid = winid;

   winset( winid );
   getsize( &w, &h );
   ctx->WinWidth = w;
   ctx->WinHeight = h;

   return 1;
}




/*
 * Attach a "mixed-mode" IRIS GL window to this vis5d context.
 */
int use_glx_window( Context ctx, Display *dpy, Window window, GLXconfig *glctx)
{
   Window root;
   int x, y;
   unsigned int w, h, bw, depth;

   assert( dpy );
   assert( window );
   assert( glctx );

   ctx->GfxWindow = window;
   
   if (GLXwinset( GfxDpy, ctx->GfxWindow )!=0) {
      return 0;
   }

   XGetGeometry( dpy, window, &root, &x, &y, &w, &h, &bw, &depth );
   ctx->WinWidth = w;
   ctx->WinHeight = h;

   return 1;
}



/*
 * Set the current rendering context/window.
 */
void set_current_window( Context ctx )
{
   if (ctx!=current_ctx) {
#ifdef GL_WINDOW
      if (ctx->gfx.winid) {
         if (!ctx->gfx.init_flag) {
            one_time_init(ctx);
            ctx->gfx.init_flag = 1;
         }
         winset( ctx->gfx.winid );
         load_font( ctx );
      }
#else
      if (ctx->GfxWindow) {
         if (!ctx->gfx.init_flag) {
            one_time_init(ctx);
            ctx->gfx.init_flag = 1;
         }
         if (GLXwinset( GfxDpy, ctx->GfxWindow )!=0) {
            printf("Error: GLXwinset failed\n");
         }
         fmsetfont( ctx->gfx.font );
      }
#endif
      current_ctx = ctx;
   }
}




/*
 * Specify the font to use for the current window.  This function must
 * be called after a window has been created.  The default font will be
 * used otherwise.
 */
int set_3d_font( Context ctx, char *name, int size )
{
   strcpy( ctx->FontName, name );
   ctx->FontHeight = size;
   ctx->gfx.font = 0;

   /* font will actually loaded the first time a string is drawn */
}



/*
 * Set the window's background color.
 */
void clear_color( unsigned int bgcolor )
{
   current_ctx->gfx.backcolor = bgcolor;
}


/*
 * Clear the graphics window.  This is called prior to rendering a frame.
 */
void clear_3d_window( void )
{
   long width, height;

   getsize( &width, &height );
   viewport( 0, (Screencoord) width-1, 0, (Screencoord) height-1);
   czclear( current_ctx->gfx.backcolor, zfar );
   current_ctx->WinWidth = width;
   current_ctx->WinHeight = height;
}



/*
 * Called when window size changes.
 */
void resize_3d_window( int width, int height )
{
   current_ctx->WinWidth = width;
   current_ctx->WinHeight = height;
/*   viewport( 0, (Screencoord) WinWidth-1, 0, (Screencoord) WinHeight-1);*/
}



void swap_3d_window( void )
{
   gflush();
   swapbuffers();
}



void set_2d( void )
{
   mmode( MPROJECTION );
   ortho2( -0.5, current_ctx->WinWidth-0.5, -0.5, current_ctx->WinHeight-0.5 );
   mmode( MVIEWING );
   loadmatrix( Identity );
   zfunction( ZF_ALWAYS );
}



/*
 * A "magic" number which describes the default size of the view volume.
 * The view volume extends from -MAGIC to MAGIC along X, Y, and Z.
 */
#define MAGIC 1.5F
#define ZMAGIC 1.8F


/*
 * Distance from eye to center of 3-D box when in perspective mode:
 */
#define EYE_DIST  4.0F


void set_3d( int perspective, float frontclip, float zoom, float *modelmat )
{
   int width = current_ctx->WinWidth;
   int height = current_ctx->WinHeight;

   if (frontclip<0.0F) {
      frontclip = 0.0F;
   }
   else if (frontclip>=1.0F) {
      frontclip = 0.99F;
   }
   else {
      frontclip = frontclip;
   }

   if (perspective) {
      float x, y, near, far;

      near = EYE_DIST - ZMAGIC + (2.0F*MAGIC*frontclip);
      far = EYE_DIST + ZMAGIC;

      if (width>height) {
         x = MAGIC / EYE_DIST * near;
         y = MAGIC / EYE_DIST * near * height / width;
      }
      else {
         x = MAGIC / EYE_DIST * near * width / height;
         y = MAGIC / EYE_DIST * near;
      }

      mmode( MPROJECTION );
      window( -x, x, -y, y, near, far );

      mmode( MVIEWING );
      loadmatrix( Identity );
      translate( 0.0, 0.0, -EYE_DIST );
      scale( zoom, zoom, 1.0 );
      {
         Matrix m;
         m[0][0] = modelmat[0];      m[0][1] = modelmat[1];
         m[0][2] = modelmat[2];      m[0][3] = modelmat[3];
         m[1][0] = modelmat[4];      m[1][1] = modelmat[5];
         m[1][2] = modelmat[6];      m[1][3] = modelmat[7];
         m[2][0] = modelmat[8];      m[2][1] = modelmat[9];
         m[2][2] = modelmat[10];     m[2][3] = modelmat[11];
         m[3][0] = modelmat[12];     m[3][1] = modelmat[13];
         m[3][2] = modelmat[14];     m[3][3] = modelmat[15];
         multmatrix( m );
      }
   }
   else {
      /* orthographic */
      float x, y, near, far;

      if (width>height) {
         x = MAGIC / zoom;
         y = MAGIC / zoom * height / width;
      }
      else {
         x = MAGIC / zoom * width / height;
         y = MAGIC / zoom;
      }
      near = -ZMAGIC + (2.0F*ZMAGIC*frontclip);
      far = ZMAGIC;

      mmode( MPROJECTION );
      ortho( -x, x, -y, y, near, far );

      mmode( MVIEWING );
      {
         Matrix m;
         m[0][0] = modelmat[0];      m[0][1] = modelmat[1];
         m[0][2] = modelmat[2];      m[0][3] = modelmat[3];
         m[1][0] = modelmat[4];      m[1][1] = modelmat[5];
         m[1][2] = modelmat[6];      m[1][3] = modelmat[7];
         m[2][0] = modelmat[8];      m[2][1] = modelmat[9];
         m[2][2] = modelmat[10];     m[2][3] = modelmat[11];
         m[3][0] = modelmat[12];     m[3][1] = modelmat[13];
         m[3][2] = modelmat[14];     m[3][3] = modelmat[15];
         loadmatrix( m );
      }
   }

   mmode( MPROJECTION );
   getmatrix( current_ctx->ProjMat );
   mmode( MVIEWING );
   getmatrix( current_ctx->ModelMat );


   /* make an object, VIEWOBJ, so we can use mapw later */
   makeobj(VIEWOBJ);
   mmode( MPROJECTION );
   loadmatrix( current_ctx->ProjMat );
   mmode( MVIEWING);
   loadmatrix( current_ctx->ModelMat );
   closeobj();


   current_ctx->Zoom = zoom;
   current_ctx->Perspective = perspective;

   zfunction( ZF_LESS );
}



void project( float p[3], float *x, float *y )
{
   float q[4], nx, ny;

   q[0] = p[0];
   q[1] = p[1];
   q[2] = p[2];
   q[3] = 1.0;

   mat_vecmul4( q, current_ctx->ModelMat );
   mat_vecmul4( q, current_ctx->ProjMat );
   nx = q[0] / q[3];   /* divide by w */
   ny = q[1] / q[3];
   *x = (nx+1.0) / 2.0 * current_ctx->WinWidth;
   *y = (1.0-ny) / 2.0 * current_ctx->WinHeight;
}



void unproject( float x, float y, float p[3], float d[3] )
{
   float mag;
   Screencoord sx, sy;
   float a0,a1,a2;
   sx = (Screencoord) x;
   sy = (Screencoord) (current_ctx->WinHeight-y-1);
   mapw( VIEWOBJ, sx, sy, &p[0], &p[1], &p[2], &a0, &a1, &a2 );
   d[0] = a0 - p[0];
   d[1] = a1 - p[1];
   d[2] = a2 - p[2];
   mag = sqrt( d[0]*d[0] + d[1]*d[1] + d[2]*d[2] );
   d[0] /= mag;
   d[1] /= mag;
   d[2] /= mag;
}



void transparency_mode( Context ctx, int mode )
{
   if (getgdesc(GD_BLEND) && mode==1) {
      ctx->AlphaBlend = 1;
   }
   else {
      ctx->AlphaBlend = 0;
   }
}



void set_color( unsigned int c )
{
   current_ctx->gfx.CurColor = c;
   cpack( (unsigned long) c );
}



void set_depthcue( onoff )
int onoff;
{
   short r, g, b;
   if (onoff) {
      r = UNPACK_RED( current_ctx->gfx.CurColor );
      g = UNPACK_GREEN( current_ctx->gfx.CurColor );
      b = UNPACK_BLUE( current_ctx->gfx.CurColor );
      depthcue( TRUE );
      lRGBrange( r/5, g/5, b/5,  r, g, b, 0, zfar );
   }
   else {
      depthcue( FALSE );
   }
}



void set_line_width( double w )
{
   linewidthf( (float) w );
}




void set_pointer( int p )
{
   static unsigned short  hourglass[16] = { 0x1ff0, 0x1ff0, 0x0820, 0x0820,
                                            0x0820, 0x0c60, 0x06c0, 0x0100,
                                            0x0100, 0x06c0, 0x0c60, 0x0820,
                                            0x0820, 0x0820, 0x1ff0, 0x1ff0 };
   if (p) {
      /* make busy cursor */
      curstype( C16X1 );
      defcursor( 1, hourglass );
      setcursor( 1, 0, 0 );
   }
   else {
      /* regulay cursor */
      setcursor( 0, 0, 0 );
   }
}



void set_pretty( int onoff )
{
   /* Disabled for the time being.  It appears the glcompat/acsize/gconfig
    * calls cause some strangeness with single/double buffering on some
    * machines.
    */
#ifdef LEAVEOUT
   if (onoff) {
      /* turn on */
      /*subpixel(TRUE);*/
      glcompat( GLC_OLDPOLYGON, 0 );
      acsize(16);
      gconfig();
      set_pointer(1);
      pretty_flag = 1;
   }
   else {
      /* turn off */
      /*subpixel(FALSE);*/
      glcompat( GLC_OLDPOLYGON, 1 );
      acsize(0);
      gconfig();
      set_pointer(0);
      pretty_flag = 0;
   }
#endif
}



/*** These functions are taken from the SGI GL Programming Guide, ch. 15 ***/
static void subpixwindow( float left, float right, float bottom, float top,
                          float near, float far, float pixdx, float pixdy )
{
   short vleft, vright, vbottom, vtop;
   float xwsize, ywsize, dx, dy;
   int xpixels, ypixels;

   getviewport( &vleft, &vright, &vbottom, &vtop );
   xpixels = vright - vleft + 1;
   ypixels = vtop - vbottom + 1;
   xwsize = right - left;
   ywsize = top - bottom;
   dx = -pixdx * xwsize / xpixels;
   dy = -pixdy * ywsize / ypixels;
   window( left+dx,right+dx, bottom+dy, top+dy, near, far );
}


static void subpixperspective( Angle fovy, float aspect, float near,
                               float far, float pixdx, float pixdy )
{
   float fov2, left, right, bottom, top;
   fov2 = ((fovy*M_PI)/1800)/2.0;
   top = near / (fcos(fov2) / fsin(fov2));
   bottom = -top;
   right = top * aspect;
   left = -right;
   subpixwindow( left, right, bottom, top, near, far, pixdx, pixdy );
}


void start_aa_pass( int n )
{
#ifdef JUNK
#define EYEDIST 3.0
#define FOV 400
   float w, h, front, rear;

   if (pretty_flag) {
      if (n==0) {
         /* clear ACC buffer */
         acbuf( AC_CLEAR, 0.0 );
      }
      if (current_ctx->Perspective) {
         float front, rear;

         front = EYEDIST - current_ctx->Scale * 1.75 * current_ctx->FrontClip;
         if (front<0.01)  front = 0.01;
         rear = EYEDIST + current_ctx->Scale * 1.75;
         subpixperspective( FOV, current_ctx->AspectRatio, front, rear,
                           xoffsets[n]/2.0, yoffsets[n]/2.0 );
         loadmatrix( current_ctx->ModelMat );
      }
      else {
         w = xoffsets[n] / current_ctx->WinWidth;
         h = yoffsets[n] / current_ctx->WinHeight;
         front = current_ctx->Scale * 1.75 * current_ctx->FrontClip;
         rear = -current_ctx->Scale * 1.75;
         ortho( -current_ctx->AspectRatio+w, current_ctx->AspectRatio+w, -1.0+h, 1.0+h, front, rear );
         loadmatrix( current_ctx->ModelMat );
      }
   }
#endif
}




void end_aa_pass( int n )
{
   if (pretty_flag) {
      acbuf( AC_ACCUMULATE, 1.0 );
      if (n==AA_PASSES-1) {
         acbuf( AC_RETURN, 1.0/(float) AA_PASSES );
      }
   }
}



int save_formats( void )
{
   return VIS5D_RGB | VIS5D_GIF | VIS5D_PS | VIS5D_COLOR_PS;
}



int save_3d_window( char filename[], int format )
{
   static char rgb_img[] = "/usr/tmp/Vis5D_image.rgb";
   long xorg, yorg, xsize, ysize, x2, y2;
   char cmd[1000];

   if (!installed("scrsave"))  return 0;

   set_pointer(1);  /* 'busy' pointer */

   /* make window dump with scrsave */
   getorigin( &xorg, &yorg );
   getsize( &xsize, &ysize );
   x2 = xorg + xsize - 1;
   y2 = yorg + ysize - 1;
   sprintf( cmd, "scrsave %s %d %d %d %d", rgb_img, xorg, x2, yorg, y2 );
   printf("Executing: %s\n", cmd );
   system( cmd );

   if (format==VIS5D_RGB) {
      /* just rename file */
      sprintf( cmd, "mv %s %s\n", rgb_img, filename );
      system( cmd );
      /* We now have a SGI .rgb file named "filename" */
   }
   else if (format==VIS5D_GIF) {
      /* Convert .rgb file to gif */
      if (!installed("togif"))  return 0;
      sprintf(cmd,"togif %s %s", rgb_img, filename );
      printf("Executing: %s\n", cmd );
      system(cmd);
      /* delete .rgb file */
      unlink( rgb_img );
   }
   else if (format==VIS5D_PS) {
      /* Convert .rgb file to PS */
      if (!installed("tops"))  return 0;
      sprintf(cmd,"tops %s > %s", rgb_img, filename );
      printf("Executing: %s\n", cmd );
      system(cmd);
      /* delete .rgb file */
      unlink( rgb_img );
   }
   else if (format==VIS5D_COLOR_PS) {
      /* Convert .rgb file to color PS */
      if (!installed("tops"))  return 0;
      sprintf(cmd,"tops %s -rgb > %s", rgb_img, filename );
      printf("Executing: %s\n", cmd );
      system(cmd);
      /* delete .rgb file */
      unlink( rgb_img );
   }
   else {
      printf("Internal error in save_window (%d)\n", format);
   }
   printf("Done writing image file.\n");
   set_pointer(0);
   return 1;
}



int print_3d_window( void )
{
   static char ps_file[] = "/usr/tmp/Vis5D_image.ps";
   char cmd[1000];

   if (!save_3d_window( ps_file, VIS5D_PS ))  return 0;

   /* We now have a PostScript file */

   if (installed("lpr")) {
      /* Send ps_file to default printer */
      sprintf(cmd,"lpr %s\n", ps_file );
      printf("Executing: %s\n", cmd );
      system(cmd);
   }

   /* delete .ps file */
   unlink( ps_file );

   return 1;
}



/*
 * Enable or disable lighting
 */
static void enable_lighting( int state )
{
/*
   if (state) {
      lmbind( MATERIAL, 10 );
      lmbind( LIGHT1, 20 );
      lmbind( LIGHT2, 21 );
      lmbind( LMODEL, 30 );
   }
   else {
      lmbind( MATERIAL, 0 );
   }
*/
}



/*
 * Set the transparency level.
 * Input:  alpha - transparency in [0,255] i.e. [clear,opaque]
 */
static void set_transparency( int alpha )
{
   if (alpha==255) {
      /* disable */
      blendfunction( BF_ONE, BF_ZERO );
      afunction( 0, AF_ALWAYS );
      setpattern(0);
   }
   else {
      /* enable */
      if (current_ctx->AlphaBlend) {
         blendfunction( BF_SA, BF_MSA );
      }
      else {
         int transpattern = 3 - alpha / 64;
         setpattern( transpattern );
      }
   }
}




/**********************************************************************/
/***                         Drawing Functions                      ***/
/**********************************************************************/



#ifdef BIG_GFX
void draw_isosurface( int n, uint_4 *index, int_2 verts[][3], int_1 norms[][3],
                      unsigned int color )
#else
void draw_isosurface( int n, uint_2 *index, int_2 verts[][3], int_1 norms[][3],
                      unsigned int color )
#endif
{
   int i;

   set_transparency( UNPACK_ALPHA(color) );
   enable_lighting(1);

   lmcolor( LMC_AD );
   cpack( color );

   pushmatrix();
   scale( 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE );
   nmode( NNORMALIZE );  /* scale normals to unit length */

   /* Render the indexed triangle strip */
   bgntmesh();
   for (i=0;i<n;i++) {
      float norm[3];
      int j;
      j = index[i];
      norm[0] = norms[j][0];
      norm[1] = norms[j][1];
      norm[2] = norms[j][2];
      n3f( norm );
      v3s( verts[j] );
   }
   endtmesh();

   nmode( NAUTO );
   popmatrix();

   enable_lighting(0);
   set_transparency( 255 );
   lmcolor( LMC_COLOR );
}




void draw_colored_isosurface( int n,
#ifdef BIG_GFX
                              uint_4 *index,
#else
                              uint_2 *index,
#endif
                              int_2 verts[][3],
                              int_1 norms[][3],
                              uint_1 color_indexes[],
                              unsigned int color_table[],
                              int alpha )
{
   int i;

   enable_lighting(1);
   lmcolor( LMC_AD );

   if (alpha==-1) {
      /* variable alpha in the mesh */
      blendfunction( BF_SA, BF_MSA );
      afunction( 5, AF_GREATER );
   }
   else {
      /* constant alpha */
      set_transparency( alpha );
   }

   pushmatrix();
   scale( 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE );
   nmode( NNORMALIZE );  /* scale normals to unit length */

   /* Render the triangle strip */
   bgntmesh();
   for (i=0;i<n;i++) {
      int j = index[i];
      int k = color_indexes[j];
      float norm[3];
      norm[0] = norms[j][0];
      norm[1] = norms[j][1];
      norm[2] = norms[j][2];
      n3f(norm);
      cpack( color_table[k] );
      v3s( verts[j] );
   }
   endtmesh();

   nmode( NAUTO );
   popmatrix();

   enable_lighting(0);
   set_transparency( 255 );
   lmcolor( LMC_COLOR );
}



void draw_triangle_strip( int n, int_2 verts[][3], int_1 norms[][3],
                          unsigned int color )
{
   int i;

   set_transparency( UNPACK_ALPHA(color) );
   enable_lighting(1);

   lmcolor( LMC_AD );
   cpack( color );

   pushmatrix();
   scale( 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE );
   nmode( NNORMALIZE );

   /* Render the triangle strip */
   bgntmesh();
   for (i=0;i<n;i++) {
      float norm[3];
      norm[0] = norms[i][0];
      norm[1] = norms[i][1];
      norm[2] = norms[i][2];
      n3f( norm );
      v3s( verts[i] );
   }
   endtmesh();

   nmode( NAUTO );
   popmatrix();

   enable_lighting(0);
   set_transparency(255);
   lmcolor( LMC_COLOR );
}



/*
 * Used to draw colored ribbon trajectories.
 */
void draw_colored_triangle_strip( int n,
                                  int_2 verts[][3], int_1 norms[][3],
                                  uint_1 color_indexes[],
                                  unsigned int color_table[] )
{
   int i;
/*
   set_transparency( UNPACK_ALPHA(color) );
   enable_lighting(1);
*/
   pushmatrix();
   scale( 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE );
   nmode( NNORMALIZE );

   /* Render the triangle strip */
   bgntmesh();
   for (i=0;i<n;i++) {
/*
      float norm[3];
      norm[0] = norms[i][0];
      norm[1] = norms[i][1];
      norm[2] = norms[i][2];
      n3f( norm );
*/
      cpack( color_table[color_indexes[i]] );
      v3s( verts[i] );
   }
   endtmesh();

/*   nmode( NAUTO );*/
   popmatrix();
/*
   enable_lighting(0);
   set_transparency(255);
*/
}



void draw_color_quadmesh( int rows, int columns, int_2 verts[][3],
                          uint_1 color_indexes[], unsigned int color_table[],
                          int alphavalue )
{
   register int i, j, base1, base2;
   unsigned int color_row1[1000];
   unsigned int color_row2[1000];
   unsigned int *row1ptr, *row2ptr, *tmp;

   /* enable transparency blending */
   if (alphavalue==-1) {
      /* variable alpha in the mesh */
      blendfunction( BF_SA, BF_MSA );
      afunction( 5, AF_GREATER );
   }
   else {
      /* constant alpha */
      set_transparency( alphavalue );
   }

   pushmatrix();
   scale( 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE );

   /* get first row of colors */
   for (j=0;j<columns;j++) {
      color_row1[j] = color_table[color_indexes[j]];
   }
   row1ptr = color_row1;
   row2ptr = color_row2;

   /* render mesh as a sequence of quad strips */
   for (i=0;i<rows-1;i++) {
      base1 = i * columns;
      base2 = (i+1) * columns;
      /* second row of colors */
      for (j=0;j<columns;j++) {
         row2ptr[j] = color_table[color_indexes[base2+j]];
      }
      bgnqstrip();
      for (j=0;j<columns;j++) {
         cpack( row1ptr[j] );
         v3s( verts[base1+j] );
         cpack( row2ptr[j] );
         v3s( verts[base2+j] );
      }
      endqstrip();
      /* swap row1ptr and row2ptr */
      tmp = row1ptr;
      row1ptr = row2ptr;
      row2ptr = tmp;
   }

   popmatrix();

   /* disable transparency */
   set_transparency( 255 );
}



void draw_lit_color_quadmesh( int rows, int columns,
                              float verts[][3],
                              float norms[][3],
                              uint_1 color_indexes[],
                              unsigned int color_table[] )
{
   register int i, j, base1, base2;
   unsigned int color_row1[1000];
   unsigned int color_row2[1000];
   unsigned int *row1ptr, *row2ptr, *tmp;

   /* enable transparency blending */
#ifdef FOO
   if (alphavalue==-1) {
      /* variable alpha in the mesh */
      blendfunction( BF_SA, BF_MSA );
      afunction( 5, AF_GREATER );
   }
   else {
      /* constant alpha */
      set_transparency( alphavalue );
   }
#endif

   enable_lighting(1);
   lmcolor( LMC_AD );  /* cpack modifies material */

   /* get first row of colors */
   for (j=0;j<columns;j++) {
      color_row1[j] = color_table[color_indexes[j]];
   }
   row1ptr = color_row1;
   row2ptr = color_row2;

   /* render mesh as a sequence of quad strips */
   for (i=0;i<rows-1;i++) {
      base1 = i * columns;
      base2 = (i+1) * columns;
      /* second row of colors */
      for (j=0;j<columns;j++) {
         row2ptr[j] = color_table[color_indexes[base2+j]];
      }
      bgnqstrip();
      for (j=0;j<columns;j++) {
         cpack( row1ptr[j] );
         n3f( norms[base1+j] );
         v3f( verts[base1+j] );
         cpack( row2ptr[j] );
         n3f( norms[base2+j] );
         v3f( verts[base2+j] );
      }
      endqstrip();
      /* swap row1ptr and row2ptr */
      tmp = row1ptr;
      row1ptr = row2ptr;
      row2ptr = tmp;
   }

   lmcolor( LMC_COLOR );
   enable_lighting(0);
#ifdef FOO
   /* disable transparency */
   set_transparency( 255 );
#endif
}




void draw_wind_lines( int nvectors, int_2 verts[][3],
                      unsigned int color )
{
   int i, j;

   cpack( color );

   pushmatrix();
   scale( 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE );

   for (i=0;i<nvectors;i++) {
      j = i * 4;
      /* main vector */
      bgnline();
      v3s( verts[j] );
      v3s( verts[j+1] );
      endline();
      /* head vectors */
      bgnline();
      v3s( verts[j+2] );
      v3s( verts[j+1] );
      v3s( verts[j+3] );
      endline();
   }

   popmatrix();
}


void draw_disjoint_lines( int n, int_2 verts[][3], unsigned int color )
{
   int i;

   cpack( color );

   pushmatrix();
   scale( 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE );
   for (i=0;i<n;i+=2 ) {
      bgnline();
      v3s( verts[i] );
      v3s( verts[i+1] );
      endline();
   }
   popmatrix();
}



void draw_polylines( int n, int_2 verts[][3], unsigned int color )
{
   int i;

   cpack( color );

   pushmatrix();
   scale( 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE );
   bgnline();
   for (i=0;i<n;i++ ) {
      v3s( verts[i] );
   }
   endline();
   popmatrix();
}



void draw_colored_polylines( int n, int_2 verts[][3],
                             uint_1 color_indexes[],
                             unsigned int color_table[] )
{
   int i;

   pushmatrix();
   scale( 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE );
   bgnline();
   for (i=0;i<n;i++ ) {
      cpack( color_table[color_indexes[i]] );
      v3s( verts[i] );
   }
   endline();
   popmatrix();
}



void draw_multi_lines( int n, float verts[][3], unsigned int color )
{
   int i;

   cpack( color );

   bgnline();
   for (i=0;i<n;i++ ) {
      if (verts[i][0]==-999.0) {
         /* start new line */
         endline();
         bgnline();
      }
      else {
         v3f( verts[i] );
      }
   }
   endline();
}

void get_zmin( Context ctx, float *zmin)
{
   *zmin = ctx->Zmin;
}


void draw_cursor( Context ctx, int style, float x, float y,
                  float z, unsigned int color )
{
   static int init_flag = 1;
   static Object line_cursor;
   static Object polygon_cursor;
   static Object sounding_cursor;

   if (init_flag) {
      /* do one-time initialization */
      float v[3];

      /* Make Sounding cursor object */
      sounding_cursor = genobj();
      makeobj( sounding_cursor );
      set_line_width(3.0);
      v[0] = v[1] = 0.0; 
      bgnline();
      v[2] = ctx->Zmin;   v3f(v);
      v[2] = ctx->Zmax;   v3f(v);
      endline();
      set_line_width(1.0);
      v[1] = 0.0;
      v[2] = ctx->Zmax;
      bgnline();
      v[0] = -0.05; v3f(v);
      v[0] =  0.05; v3f(v);
      endline();
      v[0] = 0.0;
      bgnline();
      v[1] = -0.05; v3f(v);
      v[1] =  0.05; v3f(v);
      endline();
      closeobj();


      /* Make line-segment cursor object */
      line_cursor = genobj();
      makeobj( line_cursor );
      set_line_width(1.0);
      /* X axis */
      v[1] = v[2] = 0.0;
      bgnline();
      v[0] = -0.05;  v3f(v);
      v[0] =  0.05;  v3f(v);
      endline();
      /* Y axis */
      v[0] = 0.0;
      bgnline();
      v[1] = -0.05;  v3f(v);
      v[1] =  0.05;  v3f(v);
      endline();
      /* Z axis */
      v[1] = 0.0;
      bgnline();
      v[2] = -0.05;  v3f(v);
      v[2] =  0.05;  v3f(v);
      endline();
      closeobj();

      /* Make polygonal cursor object */
      polygon_cursor = genobj();
      makeobj( polygon_cursor );
      set_line_width(1.0);
      /* X axis */
      bgntmesh();
      v[0] = -0.05; v[1] = -0.005; v[2] =  0.005;   v3f(v);
      v[0] = -0.05; v[1] =  0.005; v[2] = -0.005;   v3f(v);
      v[0] =  0.05; v[1] = -0.005; v[2] =  0.005;   v3f(v);
      v[0] =  0.05; v[1] =  0.005; v[2] = -0.005;   v3f(v);
      endtmesh();
      bgntmesh();
      v[0] = -0.05; v[1] = -0.005; v[2] = -0.005;   v3f(v);
      v[0] = -0.05; v[1] =  0.005; v[2] =  0.005;   v3f(v);
      v[0] =  0.05; v[1] = -0.005; v[2] = -0.005;   v3f(v);
      v[0] =  0.05; v[1] =  0.005; v[2] =  0.005;   v3f(v);
      endtmesh();
      /* Y-axis */
      bgntmesh();
      v[0] = -0.005; v[1] = -0.05; v[2] =  0.005;   v3f(v);
      v[0] =  0.005; v[1] = -0.05; v[2] = -0.005;   v3f(v);
      v[0] = -0.005; v[1] =  0.05; v[2] =  0.005;   v3f(v);
      v[0] =  0.005; v[1] =  0.05; v[2] = -0.005;   v3f(v);
      endtmesh();
      bgntmesh();
      v[0] = -0.005; v[1] = -0.05; v[2] = -0.005;   v3f(v);
      v[0] =  0.005; v[1] = -0.05; v[2] =  0.005;   v3f(v);
      v[0] = -0.005; v[1] =  0.05; v[2] = -0.005;   v3f(v);
      v[0] =  0.005; v[1] =  0.05; v[2] =  0.005;   v3f(v);
      endtmesh();
      /* Z-axis */
      bgntmesh();
      v[0] = -0.005; v[1] = -0.005; v[2] =  0.05;   v3f(v);
      v[0] =  0.005; v[1] =  0.005; v[2] =  0.05;   v3f(v);
      v[0] = -0.005; v[1] = -0.005; v[2] = -0.05;   v3f(v);
      v[0] =  0.005; v[1] =  0.005; v[2] = -0.05;   v3f(v);
      endtmesh();
      bgntmesh();
      v[0] = -0.005; v[1] =  0.005; v[2] =  0.05;   v3f(v);
      v[0] =  0.005; v[1] = -0.005; v[2] =  0.05;   v3f(v);
      v[0] = -0.005; v[1] =  0.005; v[2] = -0.05;   v3f(v);
      v[0] =  0.005; v[1] = -0.005; v[2] = -0.05;   v3f(v);
      endtmesh();
      closeobj();

      init_flag = 0;
   }

   cpack( color );

   pushmatrix();
   translate( x, y, z );
   if (style == 1) {
      callobj( polygon_cursor );
   }
   if (style == 2) {
      callobj( sounding_cursor );
   }
   else {
      callobj( line_cursor );
   }
   popmatrix();
}




/***** OLD primitives ****/



void polyline( float vert[][3], int n )
{
   register int i;

/*
   lmcolor( LMC_COLOR );
   cpack( current_ctx->gfx.CurColor );
*/

   bgnline();
   for (i=0;i<n;i++) {
      v3f( vert[i] );
   }
   endline();
}




void disjointpolyline( float vert[][3], int n )
{
   register int i;

   cpack( current_ctx->gfx.CurColor );

   for (i=0;i<n;i+=2) {
      bgnline();
      v3f( vert[i] );
      v3f( vert[i+1] );
      endline();
   }
}




#ifdef JUNK
void quadmeshnorm( float vert[][3], float norm[][3],
                   unsigned int color[], int rows, int cols )
{
   register int i, j, base1, base2;

   lmcolor( LMC_AD );
   set_transparency( UNPACK_ALPHA(current_ctx->gfx.CurColor) );
   enable_lighting(1);

   /* break mesh into strips */
   for (i=0;i<rows-1;i++) {
      base1 = i * cols;
      base2 = (i+1) * cols;
      bgnqstrip();
      for (j=0;j<cols;j++) {
         cpack( color[base1+j] ); 
         n3f( norm[base1+j] );
         v3f( vert[base1+j] );
         cpack( color[base2+j] ); 
         n3f( norm[base2+j] );
         v3f( vert[base2+j] );
      }
      endqstrip();
   }

   enable_lighting( 0 );
   set_transparency( 255 );
   lmcolor( LMC_COLOR );}
}
#endif



void polyline2d( short vert[][2], int n )
{
   short v[2];
   int i;

   cpack( current_ctx->gfx.CurColor );
   bgnline();
   for (i=0;i<n;i++) {
      v[0] = vert[i][0];
      v[1] = current_ctx->WinHeight - vert[i][1];
      v2s( v );
   }
   endline();
}



void draw_text( int xpos, int ypos, char *str )
{
   load_font( current_ctx );
   cpack( current_ctx->gfx.CurColor );
   cmovi( xpos, current_ctx->WinHeight-ypos, 1 );
   fmprstr( str );
}



int text_width( char *str )
{
   load_font( current_ctx );
   return fmgetstrwidth( current_ctx->gfx.font, str );
}



/*
 * Start a new display list object and return its ID or zero if error.
 */
int begin_object( void )
{
   Object obj;

   obj = genobj();
   makeobj( obj );
   return (int) obj;
}


/*
 * End construction of a display list object.
 */
void end_object( void )
{
   closeobj();
}


/*
 * Draw a display list object.
 */
void call_object( int obj )
{
   callobj( (Object) obj );
}


void delete_object( int obj )
{
   delobj( (Object) obj );
}
