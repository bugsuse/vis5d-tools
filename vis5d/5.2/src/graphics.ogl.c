/*  graphics.ogl */

/* Graphics functions for OpenGL */
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
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include "GL/gl.h"
#include "GL/glu.h"
#include "GL/glx.h"
#include "globals.h"
#include "graphics.h"
#include "matrix.h"
#include "misc.h"
#include "mwmborder.h"
#include <sys/stat.h>
#include "xdump.h"


/*
 * Private OpenGL variables:
 */
static int pretty_flag = 0;
#ifndef M_PI
#define M_PI 3.14159265
#endif

#define TMP_XWD "tmp.xwd"
#define TMP_RGB "tmp.rgb"


/* Accumulation buffer antialiasing */
#define AA_INC  1.0
static float  xoffsets[AA_PASSES] =
    { -AA_INC, 0.0, AA_INC,  -AA_INC, 0.0, AA_INC,  -AA_INC, 0.0, AA_INC };
static float  yoffsets[AA_PASSES] =
    { -AA_INC, -AA_INC, -AA_INC,  0.0, 0.0, 0.0,  AA_INC, AA_INC, AA_INC };


/* "Screen door" transparency */
static GLuint stipple[3][32];

static Display_Context current_dtx = NULL;

#define DEFAULT_FONT "10x20"
struct Biggfx{
      GLXContext gl_ctx;
      XFontStruct *font;
      GLuint fontbase;
   };

struct Biggfx biggfx;




static void check_error( char *where )
{
   GLenum error;

   while (1) {
      error = glGetError();
      if (error==GL_NO_ERROR) {
         break;
      }
      printf("OpenGL error near %s: %s\n", where, gluErrorString( error ) );
   }
}



/*
 * Do OpenGL-specific initializations.  This is only called once.
 */
void init_graphics2( void )
{
   int i;

   HQR_available = 1;
   Perspec_available = 1;

   /* Setup stipples for screendoor transparency */
   /* stipple[0] = 25% opaque */
   for (i=0;i<32;i+=2) {
      stipple[0][i+0] = 0x88888888;
      stipple[0][i+1] = 0x22222222;
   }
   /* stipple[1] = 50% opaque */
   for (i=0;i<32;i+=2) {
      stipple[1][i+0] = 0xaaaaaaaa;
      stipple[1][i+1] = 0x55555555;
   }
   /* stipple[2] = 75% opaque */
   for (i=0;i<32;i+=2) {
      stipple[2][i+0] = 0x77777777;
      stipple[2][i+1] = 0xdddddddd;
   }
}



/*
 * Call this before exiting vis5d.
 */
void terminate_graphics( void )
{
}


/*
 * Free the graphics resources attached to a display context.
 */
void free_graphics( Display_Context dtx )
{
   if (dtx->gfx.gl_ctx) {
      glXDestroyContext( GfxDpy, dtx->gfx.gl_ctx );
      dtx->gfx.gl_ctx = 0;
   }
   if (dtx->GfxWindow) {
      XDestroyWindow( GfxDpy, dtx->GfxWindow );
      dtx->GfxWindow = 0;
   }
}



void
context_init(
  Context ctx,
  long win_id,
  int width,
  int height
)
{
   /* nothing for OpenGL */
}


int make_big_window( char *title, int xpos, int ypos, int width, int height)
{
   static int attrib_list[] = {
      GLX_RGBA,
      GLX_RED_SIZE, 1,
      GLX_GREEN_SIZE, 1,
      GLX_BLUE_SIZE, 1,
      GLX_DEPTH_SIZE, 1,
      GLX_DOUBLEBUFFER,
      None };
   Window root;
   XSetWindowAttributes win_attrib;
   XSizeHints sizehints;
   XVisualInfo *visualinfo;
   unsigned long mask;
   Screen *screen = DefaultScreenOfDisplay( GfxDpy );

   root = DefaultRootWindow(GfxDpy);
   /*********************/
   /* Choose the visual */
   /*********************/
   visualinfo = glXChooseVisual( GfxDpy, GfxScr, attrib_list );
   if (!visualinfo) {
      printf("Error: couldn't get RGB, Double-Buffered, Depth-Buffered GLX");
      printf(" visual!\n");
      exit(0);
   }
   /* Create the GL/X context. */
   if (biggfx.gl_ctx){
      glXDestroyContext( GfxDpy, biggfx.gl_ctx);
   }
   biggfx.gl_ctx = glXCreateContext( GfxDpy, visualinfo, NULL, True );
   if (!biggfx.gl_ctx) {
      /* try (indirect context) */
      biggfx.gl_ctx = glXCreateContext( GfxDpy, visualinfo, NULL, False );
      if (!biggfx.gl_ctx) {
        printf("Error: glXCreateContext failed!\n");
        exit(0);
      }
      else {
        printf("Warning: using indirect GL/X context, may be slow\n");
      }
   }



   /*******************/
   /* Make the window */
   /*******************/
   win_attrib.background_pixmap = None;
   win_attrib.background_pixel = 0;
   win_attrib.border_pixel = 0;
   if (MaxCmapsOfScreen(screen)==1
       && visualinfo->depth==DefaultDepth(GfxDpy,GfxScr)
       && visualinfo->visual==DefaultVisual(GfxDpy,GfxScr)) {
      /* Share the root colormap on low-end displays */
      win_attrib.colormap = DefaultColormap( GfxDpy, GfxScr );
   }
   else {
      win_attrib.colormap = XCreateColormap( GfxDpy, root,
                                        visualinfo->visual, AllocNone );
   }
   win_attrib.event_mask = ExposureMask | ButtonMotionMask | KeyReleaseMask
                           | KeyPressMask | ButtonPressMask | ButtonReleaseMask
                        | StructureNotifyMask | VisibilityChangeMask;
   mask = CWBackPixmap | CWBackPixel | CWBorderPixel | CWEventMask
          | CWColormap;  /* | CWOverrideRedirect;*/
   if (!BigWindow){
      BigWindow = XCreateWindow( GfxDpy, root, xpos, ypos, width, height,
                               0, visualinfo->depth, InputOutput,
                               visualinfo->visual,
                               mask, &win_attrib );
      BigWinWidth = width;
      BigWinHeight = height;
      XSelectInput( GfxDpy, BigWindow, ExposureMask | ButtonMotionMask | KeyReleaseMask
                   | KeyPressMask | ButtonPressMask | ButtonReleaseMask
                   | StructureNotifyMask | SubstructureNotifyMask
                   | VisibilityChangeMask );
      sizehints.x = xpos;
      sizehints.y = ypos;
      sizehints.width  = width;
      sizehints.height = height;
      sizehints.flags = USSize | USPosition;
      XSetNormalHints( GfxDpy, BigWindow, &sizehints);
      XSetStandardProperties( GfxDpy, BigWindow, title, title,
                            None, (char **)NULL, 0, &sizehints);
      if (!BigWindow) {
         printf("Error: XCreateWindow failed in making BigWindow!\n");
         exit(0);
      }
   }
   if (width==ScrWidth && height==ScrHeight) {
      /* This is a hack for borderless windows! */
      no_border( GfxDpy, BigWindow );
   }

   /* Bind the GLX context to the window (make this window the current one) */
   if (BigWindow){
      if (!glXMakeCurrent( GfxDpy, BigWindow, biggfx.gl_ctx )) {
         printf("Error: glXMakeCurrent failed!\n");
         exit(0);
      }
   }
/* MJK 11.19.98 */
   if (!off_screen_rendering){
      XMapWindow( GfxDpy, BigWindow);
   }

   if (visualinfo->depth<8) {
      /* This case occurs on 8-bit SGI Indys, etc because in double buffer
       * mode the front and back buffers are only 4 bits deep.
       * We do nothing, the GUI will use the GfxVisual, GfxColormap, etc
       * which was found by find_best_visual previously.
       */
   }
   else {
      /* Reassign GfxVisual, GfxColormap, etc to use what glxChooseVisual
       * gave us.  This is especially important on low-end systems using
       * Mesa because we want to share colormaps, etc to prevent colormap
       * "flashing".
       */
      GfxVisual = visualinfo->visual;
      GfxDepth = visualinfo->depth;
      GfxColormap = win_attrib.colormap;
   }
   /* Setup lighting parameters */
   {
      static GLfloat light0_pos[] = { 0.0, 0.0, 1000.0, 0.0 };
      static GLfloat light1_pos[] = { 0.0, 0.0, -1000.0, 0.0 };
      static GLfloat light_ambient[] = { 0.15, 0.15, 0.15, 1.0 };
      static GLfloat light_diffuse[] = { 0.6, 0.6, 0.6, 1.0 };
      static GLfloat light_specular[] = { 1.0, 1.0, 1.0, 1.0 };
      static GLfloat model_ambient[] = { 0.0, 0.0, 0.0, 1.0 };
      /*static GLfloat mat_specular[] = { 0.5, 0.5, 0.5, 1.0 };*/
      glLightfv( GL_LIGHT0, GL_AMBIENT, light_ambient );
      glLightfv( GL_LIGHT0, GL_DIFFUSE, light_diffuse );
      glLightfv( GL_LIGHT0, GL_SPECULAR, light_specular );
      glLightfv( GL_LIGHT0, GL_POSITION, light0_pos );
      glLightfv( GL_LIGHT1, GL_AMBIENT, light_ambient );
      glLightfv( GL_LIGHT1, GL_DIFFUSE, light_diffuse );
      glLightfv( GL_LIGHT1, GL_SPECULAR, light_specular );
      glLightfv( GL_LIGHT1, GL_POSITION, light1_pos );
      glLightModelfv( GL_LIGHT_MODEL_AMBIENT, model_ambient );
      glLightModeli( GL_LIGHT_MODEL_TWO_SIDE, 0 );
      glLightModeli( GL_LIGHT_MODEL_LOCAL_VIEWER, 0 );
      /*glMaterialfv( GL_FRONT_AND_BACK, GL_SPECULAR, mat_specular );*/
      /*glMaterialf( GL_FRONT_AND_BACK, GL_SHININESS, 100.0 );*/
      glEnable( GL_LIGHT0 );
      glEnable( GL_LIGHT1 );
      glEnable( GL_NORMALIZE );
   }
   /* Setup fog/depthcue parameters */
   {
      static GLfloat fog_color[] = { 0.2, 0.2, 0.2, 1.0 };

      glFogi( GL_FOG_MODE, GL_LINEAR );
      glFogfv( GL_FOG_COLOR, fog_color );
   }
   return 1;
}





int make_3d_window( Display_Context dtx, char *title, int xpos, int ypos,
                    int width, int height )
{
   int mapit;
   static int attrib_list[] = {
      GLX_RGBA,
      GLX_RED_SIZE, 1,
      GLX_GREEN_SIZE, 1,
      GLX_BLUE_SIZE, 1,
   /* GLX_ALPHA_SIZE, 1,   Leave out, some systems have no alpha bitplanes */
      GLX_DEPTH_SIZE, 1,
      GLX_DOUBLEBUFFER,
/*
      GLX_ACCUM_RED_SIZE, 1,
      GLX_ACCUM_GREEN_SIZE, 1,
      GLX_ACCUM_BLUE_SIZE, 1,
      GLX_ACCUM_ALPHA_SIZE, 1,
*/
      None };
   Window root;
   XSetWindowAttributes win_attrib;
   XSizeHints sizehints;
   XVisualInfo *visualinfo;
   unsigned long mask;
   Screen *screen = DefaultScreenOfDisplay( GfxDpy );
   int cur;
   GLXContext prevctx;
   GLXDrawable prevdraw;

   if (!BigWindow){
      printf("no BigWindow \n");
      exit(0);
   }
   root = BigWindow;


   /* MJK 11.19.98 */
   if (off_screen_rendering){
      width = BigWinWidth/DisplayRows;
      height = BigWinHeight/DisplayCols;
   }

   visualinfo = glXChooseVisual( GfxDpy, GfxScr, attrib_list );
   if (!visualinfo) {
      printf("Error: couldn't get RGB, Double-Buffered, Depth-Buffered GLX");
      printf(" visual!\n");
      exit(0);
   }

   /* Create the GL/X context. */
   if (dtx->gfx.gl_ctx){
      glXDestroyContext( GfxDpy, dtx->gfx.gl_ctx);
   }
   dtx->gfx.gl_ctx = glXCreateContext( GfxDpy, visualinfo, NULL, True );
   if (!dtx->gfx.gl_ctx) {
      /* try (indirect context) */
      dtx->gfx.gl_ctx = glXCreateContext( GfxDpy, visualinfo, NULL, False );
      if (!dtx->gfx.gl_ctx) {
        printf("Error: glXCreateContext failed!\n");
        exit(0);
      }
      else {
        printf("Warning: using indirect GL/X context, may be slow\n");
      }
   }
   current_dtx = dtx;


   if (!dtx->GfxWindow){
      /* Make the window */
      win_attrib.background_pixmap = None;
      win_attrib.background_pixel = 0;
      win_attrib.border_pixel = 0;
/******************************************************************************/
/*    Johan don't forget to add KeyReleaseMask to pex and gl graphics!!!!!!!!!   */
/******************************************************************************/
      win_attrib.colormap = GfxColormap;
      win_attrib.event_mask = ExposureMask | ButtonMotionMask | KeyReleaseMask
                              | KeyPressMask | ButtonPressMask | ButtonReleaseMask
                           | StructureNotifyMask | VisibilityChangeMask;
      mask = CWBackPixmap | CWBackPixel | CWBorderPixel | CWEventMask
             | CWColormap;  /* | CWOverrideRedirect;*/
      if (!dtx->GfxWindow){
         dtx->GfxWindow = XCreateWindow( GfxDpy, BigWindow, xpos, ypos, width, height,
                                 0, GfxDepth, InputOutput,
                                  GfxVisual,
                                  mask, &win_attrib );
         XSelectInput( GfxDpy, dtx->GfxWindow, ExposureMask | ButtonMotionMask
                      | KeyPressMask | KeyReleaseMask | ButtonPressMask | ButtonReleaseMask
                      | StructureNotifyMask | SubstructureNotifyMask
                      | VisibilityChangeMask );
         sizehints.x = xpos;
         sizehints.y = ypos;
         sizehints.width  = width;
         sizehints.height = height;
         sizehints.flags = USSize | USPosition;
         XSetNormalHints( GfxDpy, dtx->GfxWindow, &sizehints);
         XSetStandardProperties( GfxDpy, dtx->GfxWindow, "Vis5D 5.2", "Vis5d 5.2",
                               None, (char **)NULL, 0, &sizehints);
      }
   }
   if (!dtx->GfxWindow) {
      printf("Error: XCreateWindow failed in making GfxWindow!\n");
      exit(0);
   }
   /* MJK 11.19.98 */
   if (off_screen_rendering){
      GLXPixmap yomap;
      dtx->GfxPixmap = XCreatePixmap( GfxDpy, dtx->GfxWindow, width, height,visualinfo->depth);
      dtx->WinHeight = height;
      dtx->WinWidth  = width;
      yomap = glXCreateGLXPixmap( GfxDpy, visualinfo, dtx->GfxPixmap);
      glXMakeCurrent( GfxDpy, yomap, dtx->gfx.gl_ctx );
   }


   if (width==ScrWidth && height==ScrHeight) {
      /* This is a hack for borderless windows! */
      no_border( GfxDpy, dtx->GfxWindow );
   }



   prevctx = glXGetCurrentContext();
   prevdraw= glXGetCurrentDrawable();
 
   /* MJK 11.19.98 */
   if (!off_screen_rendering){
      if (dtx->GfxWindow ){
         if (!glXMakeCurrent( GfxDpy, dtx->GfxWindow, dtx->gfx.gl_ctx )) {
            printf("Error: glXMakeCurrent failed!\n");
            exit(0);
         }
      }
   } 
   {
      static GLfloat light0_pos[] = { 0.0, 0.0, 1000.0, 0.0 };
      static GLfloat light1_pos[] = { 0.0, 0.0, -1000.0, 0.0 };
      static GLfloat light_ambient[] = { 0.15, 0.15, 0.15, 1.0 };
      static GLfloat light_diffuse[] = { 0.6, 0.6, 0.6, 1.0 };
      static GLfloat light_specular[] = { 1.0, 1.0, 1.0, 1.0 };
      static GLfloat model_ambient[] = { 0.0, 0.0, 0.0, 1.0 };
      /*static GLfloat mat_specular[] = { 0.5, 0.5, 0.5, 1.0 };*/

      glLightfv( GL_LIGHT0, GL_AMBIENT, light_ambient );
      glLightfv( GL_LIGHT0, GL_DIFFUSE, light_diffuse );
      glLightfv( GL_LIGHT0, GL_SPECULAR, light_specular );
      glLightfv( GL_LIGHT0, GL_POSITION, light0_pos );
      glLightfv( GL_LIGHT1, GL_AMBIENT, light_ambient );
      glLightfv( GL_LIGHT1, GL_DIFFUSE, light_diffuse );
      glLightfv( GL_LIGHT1, GL_SPECULAR, light_specular );
      glLightfv( GL_LIGHT1, GL_POSITION, light1_pos );
      glLightModelfv( GL_LIGHT_MODEL_AMBIENT, model_ambient );
      glLightModeli( GL_LIGHT_MODEL_TWO_SIDE, 0 );
      glLightModeli( GL_LIGHT_MODEL_LOCAL_VIEWER, 0 );
      /*glMaterialfv( GL_FRONT_AND_BACK, GL_SPECULAR, mat_specular );*/
      /*glMaterialf( GL_FRONT_AND_BACK, GL_SHININESS, 100.0 );*/

      glEnable( GL_LIGHT0 );
      glEnable( GL_LIGHT1 );
      glEnable( GL_NORMALIZE );
   }
   /* Setup fog/depthcue parameters */
   {
      static GLfloat fog_color[] = { 0.2, 0.2, 0.2, 1.0 };

      glFogi( GL_FOG_MODE, GL_LINEAR );
      glFogfv( GL_FOG_COLOR, fog_color );
   }

   dtx->WinWidth = width;
   dtx->WinHeight = height;

   glViewport( 0, 0, dtx->WinWidth, dtx->WinHeight ); 

   if (dtx->FontName[0]==0) {
      strcpy( dtx->FontName, DEFAULT_FONT );
   }
   dtx->gfx.font = XLoadQueryFont( GfxDpy, dtx->FontName );
   if (!dtx->gfx.font) {
      fprintf( stderr, "Unable to load font: %s\n", dtx->FontName );
      return 0;
   }
   dtx->gfx.fontbase = glGenLists( dtx->gfx.font->max_char_or_byte2 );
   glXUseXFont( dtx->gfx.font->fid, 0,
               dtx->gfx.font->max_char_or_byte2, dtx->gfx.fontbase );
   dtx->FontHeight = dtx->gfx.font->ascent + dtx->gfx.font->descent;
   dtx->FontDescent = dtx->gfx.font->descent;
   if (prevctx && prevdraw){
      if (!glXMakeCurrent( GfxDpy, prevdraw, prevctx )) {
         printf("Error: glXMakeCurrent failed!\n");
         exit(0);
      }
   }
      

   return 1;
}



/*
 * Bind the given OpenGL window to a Vis5D context.
 * Return:  1 = ok, 0 = error.
 */
int use_opengl_window( Display_Context dtx, Display *dpy, Window window,
                       GLXContext glctx, XFontStruct *xfont )
{

   if (dpy!=GfxDpy) {
      GfxDpy = dpy;
      GfxScr = DefaultScreen( GfxDpy );

      ScrWidth = DisplayWidth( GfxDpy, GfxScr );
      ScrHeight = DisplayHeight( GfxDpy, GfxScr );

      /* While some graphics libraries don't need this, others do and the
       * X/GUI stuff always does.
       */
      find_best_visual( GfxDpy, GfxScr, &GfxDepth, &GfxVisual, &GfxColormap );
   }

   dtx->gfx.gl_ctx = glctx;
   dtx->GfxWindow = window;


   /* Bind the GLX context to the window (make this window the current one) */
   glXMakeCurrent( GfxDpy, dtx->GfxWindow, dtx->gfx.gl_ctx );

   /* Setup the font */
   if (xfont) {
      dtx->gfx.font = xfont;
   }
   else {
      /* load font now */
      if (dtx->FontName[0]==0) {
        strcpy( dtx->FontName, DEFAULT_FONT );
      }
      dtx->gfx.font = XLoadQueryFont( GfxDpy, dtx->FontName );
      if (!dtx->gfx.font) {
        fprintf( stderr, "Unable to load font: %s\n", dtx->FontName );
        return 0;
      }
   }
   dtx->gfx.fontbase = glGenLists( dtx->gfx.font->max_char_or_byte2 );
   glXUseXFont( dtx->gfx.font->fid, 0,
               dtx->gfx.font->max_char_or_byte2, dtx->gfx.fontbase );
   dtx->FontHeight = dtx->gfx.font->ascent + dtx->gfx.font->descent;
   dtx->FontDescent = dtx->gfx.font->descent;

   return 1;
}




/*
 * Specify which display_context / 3D window is the current one for rendering.
 */

/*
#define SET_GFX_DISPLAY_CONTEXT( c )                           \       
   if (c!=current_dtx) {                                       \       
      glXMakeCurrent( GfxDpy, c->GfxWindow, c->gfx.gl_ctx );   \
      current_dtx = c;                                         \       
   }
    
*/

/*
 * Set the current rendering context/window.
 */
void set_current_window( Display_Context dtx )
{
   if (dtx!=current_dtx) {
      /* MJK 11.19.98 */
      if (dtx->GfxPixmap){
         if (off_screen_rendering){
            glXMakeCurrent( GfxDpy, dtx->GfxPixmap, dtx->gfx.gl_ctx );
         }
      }
      else if (dtx->GfxWindow) {
         glXMakeCurrent( GfxDpy, dtx->GfxWindow, dtx->gfx.gl_ctx );
       }
      current_dtx = dtx;
   }
}

void finish_rendering( void )
{
   /* nothing */
   /* used for PEX */
}


/*
 * Specify the font to use in the 3-D window.  Must be called before
 * the window is created.
 */
int set_3d_font(  Display_Context dtx, char *name, int size )
{
   strcpy( dtx->FontName, name );
   if (dtx->FontName[0]==0) {
      strcpy( dtx->FontName, DEFAULT_FONT );
   }
   glXMakeCurrent( GfxDpy, dtx->GfxWindow, dtx->gfx.gl_ctx);
   if (dtx->gfx.font && dtx->gfx.fontbase && dtx->gfx.font->max_char_or_byte2){
      glDeleteLists(dtx->gfx.fontbase, dtx->gfx.font->max_char_or_byte2);
   } 
   dtx->gfx.font = XLoadQueryFont( GfxDpy, dtx->FontName );
   if (!dtx->gfx.font) {
      fprintf( stderr, "Unable to load font: %s\n", dtx->FontName );
      return 0;
   }
   dtx->gfx.fontbase = glGenLists( dtx->gfx.font->max_char_or_byte2 );
   glXUseXFont( dtx->gfx.font->fid, 0,
               dtx->gfx.font->max_char_or_byte2, dtx->gfx.fontbase );
   dtx->FontHeight = dtx->gfx.font->ascent + dtx->gfx.font->descent;
   dtx->FontDescent = dtx->gfx.font->descent;

   return 0;
}

int get_3d_font( Display_Context dtx, char *name, int *size)
{
   strcpy( name, dtx->FontName);
   return 0;
}



/*
 * Set the window's background color.
 */
void clear_color( unsigned int bgcolor )
{
   GLfloat r, g, b, a;

   r = UNPACK_RED( bgcolor) / 255.0;
   g = UNPACK_GREEN( bgcolor ) / 255.0;
   b = UNPACK_BLUE( bgcolor ) / 255.0;
   a = UNPACK_ALPHA( bgcolor ) / 255.0;
   glClearColor( r, g, b, a );
}



/*
 * Clear the graphics window.  This is called prior to rendering a frame.
 */
void clear_3d_window( void )
{
   glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );
}



/*
 * Called when window size changes.
 */
void resize_3d_window( int width, int height )
{
   current_dtx->WinWidth = width;
   current_dtx->WinHeight = height;
   glViewport( 0, 0, width, height );
}

void resize_BIG_window( int width, int height )
{
   glXMakeCurrent( GfxDpy, BigWindow, biggfx.gl_ctx);
   glViewport( 0, 0, width, height );
   glXMakeCurrent( GfxDpy, current_dtx->GfxWindow, current_dtx->gfx.gl_ctx );
}
   

void swap_3d_window( void )
{
   check_error("swap");
   /* MJK 11.19.98 */
   if (!off_screen_rendering){
      glXSwapBuffers( GfxDpy, current_dtx->GfxWindow );
   }
   else{
      glXSwapBuffers( GfxDpy, current_dtx->GfxPixmap );
   }
}



void set_2d( void )
{
   glMatrixMode( GL_PROJECTION );
   glLoadIdentity();
   glOrtho( 0.0, (GLfloat) current_dtx->WinWidth,
           0.0, (GLfloat) current_dtx->WinHeight, -1.0, 1.0 );
   glMatrixMode( GL_MODELVIEW );
   glLoadIdentity();
   glDisable( GL_DEPTH_TEST );
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


void clipping_on( void )
{
   if (!current_dtx->CurvedBox){
      glEnable(GL_CLIP_PLANE0);
      glEnable(GL_CLIP_PLANE1); 
      glEnable(GL_CLIP_PLANE2);
      glEnable(GL_CLIP_PLANE3);
      glEnable(GL_CLIP_PLANE4);
      glEnable(GL_CLIP_PLANE5);  
      glFinish(); 
   }
}

void clipping_off( void )
{
   if (!current_dtx->CurvedBox){
      glDisable(GL_CLIP_PLANE0);
      glDisable(GL_CLIP_PLANE1);
      glDisable(GL_CLIP_PLANE2);
      glDisable(GL_CLIP_PLANE3);
      glDisable(GL_CLIP_PLANE4);
      glDisable(GL_CLIP_PLANE5); 
   }
}


void set_3d( int perspective, float frontclip, float zoom, float *modelmat)
{
   int width = current_dtx->WinWidth;
   int height = current_dtx->WinHeight;
   GLdouble eqnleft[4];
   GLdouble eqnright[4];
   GLdouble eqntop[4]; 
   GLdouble eqnbottom[4];
   GLdouble eqnback[4];
   GLdouble eqnfront[4];

   eqntop[0] = -1 * current_dtx->VClipTable[0].eqn[0];
   eqntop[1] = -1 * current_dtx->VClipTable[0].eqn[1];
   eqntop[2] = -1 * current_dtx->VClipTable[0].eqn[2];
   eqntop[3] = -1 * current_dtx->VClipTable[0].eqn[3] + 0.01;
   eqnbottom[0] = current_dtx->VClipTable[1].eqn[0];
   eqnbottom[1] = current_dtx->VClipTable[1].eqn[1];
   eqnbottom[2] =current_dtx->VClipTable[1].eqn[2];
   eqnbottom[3] =current_dtx->VClipTable[1].eqn[3] + 0.01;
   eqnleft[0] = current_dtx->VClipTable[2].eqn[0];
   eqnleft[1] = current_dtx->VClipTable[2].eqn[1];
   eqnleft[2] = current_dtx->VClipTable[2].eqn[2];
   eqnleft[3] = current_dtx->VClipTable[2].eqn[3] + 0.01;
   eqnright[0] = -1 * current_dtx->VClipTable[3].eqn[0];
   eqnright[1] = -1 * current_dtx->VClipTable[3].eqn[1];
   eqnright[2] = -1 * current_dtx->VClipTable[3].eqn[2];
   eqnright[3] = -1 * current_dtx->VClipTable[3].eqn[3] + 0.01;

   eqnfront[0] = -1 *current_dtx->HClipTable[0].eqn[0];
   eqnfront[1] = -1 *current_dtx->HClipTable[0].eqn[1];
   eqnfront[2] = -1 *current_dtx->HClipTable[0].eqn[2];
   eqnfront[3] = current_dtx->HClipTable[0].eqn[3] + 0.01;
   eqnback[0] = current_dtx->HClipTable[1].eqn[0];
   eqnback[1] =current_dtx->HClipTable[1].eqn[1];
   eqnback[2] =current_dtx->HClipTable[1].eqn[2];
   eqnback[3] = -1 * current_dtx->HClipTable[1].eqn[3] + 0.01;


   check_error("set_3d");
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

      glMatrixMode( GL_PROJECTION );
      glLoadIdentity();
      glFrustum( -x, x, -y, y, near, far );

      glMatrixMode( GL_MODELVIEW );
      glLoadIdentity();
      glTranslatef( 0.0, 0.0, -EYE_DIST );
      glScalef( zoom, zoom, 1.0 );
      glMultMatrixf( modelmat );
      glClipPlane(GL_CLIP_PLANE0, eqnleft);
      glClipPlane(GL_CLIP_PLANE1, eqnright);
      glClipPlane(GL_CLIP_PLANE2, eqntop);
      glClipPlane(GL_CLIP_PLANE3, eqnbottom);
      glClipPlane(GL_CLIP_PLANE4, eqnback);
      glClipPlane(GL_CLIP_PLANE5, eqnfront);

      glFogf( GL_FOG_START, EYE_DIST - ZMAGIC );
      glFogf( GL_FOG_END, far );
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
      near = 2.0F*ZMAGIC*frontclip;
      far = 2.0*ZMAGIC;

      glMatrixMode( GL_PROJECTION );
      glLoadIdentity();
      glOrtho( -x, x, -y, y, near, far );

      glMatrixMode( GL_MODELVIEW );
      glTranslatef( 0.0, 0.0, -ZMAGIC );
      glMultMatrixf( modelmat );
      glClipPlane(GL_CLIP_PLANE0, eqnleft);
      glClipPlane(GL_CLIP_PLANE1, eqnright);
      glClipPlane(GL_CLIP_PLANE2, eqntop);
      glClipPlane(GL_CLIP_PLANE3, eqnbottom);
      glClipPlane(GL_CLIP_PLANE4, eqnback);
      glClipPlane(GL_CLIP_PLANE5, eqnfront);

      glFogf( GL_FOG_START, 0.0 );
      glFogf( GL_FOG_END, far );
   }
   glEnable( GL_DEPTH_TEST );
   glGetDoublev( GL_MODELVIEW_MATRIX, current_dtx->ModelMat );
   glGetDoublev( GL_PROJECTION_MATRIX, current_dtx->ProjMat );
   current_dtx->Perspective = perspective;
   check_error("end set_3d");
   glViewport(0, 0,width,height);
}

void project( float p[3], float *x, float *y )
{
   GLint viewport[4];
   GLdouble winx, winy, winz;
   float *m;
   int i;

   /*glGetIntegerv( GL_VIEWPORT, viewport );*/
   viewport[0] = 0;
   viewport[1] = 0;
   viewport[2] = current_dtx->WinWidth;
   viewport[3] = current_dtx->WinHeight;

   gluProject( (GLdouble) p[0], (GLdouble) p[1], (GLdouble) p[2],
              current_dtx->ModelMat, current_dtx->ProjMat, viewport,
              &winx, &winy, &winz );

   *x = winx;
   *y = current_dtx->WinHeight - winy;
}



void unproject( float x, float y, float p[3], float d[3] )
{
   GLint viewport[4];
   GLdouble x0, y0, z0;
   GLdouble x1, y1, z1;
   GLdouble len;
   float *m;
   int i;

   /*glGetIntegerv( GL_VIEWPORT, viewport );*/
   viewport[0] = 0;
   viewport[1] = 0;
   viewport[2] = current_dtx->WinWidth;
   viewport[3] = current_dtx->WinHeight;

   /* Unproject */
   if (!gluUnProject( (GLdouble) x, (GLdouble) (current_dtx->WinHeight-y),
                    (GLdouble) 0.0,
                    current_dtx->ModelMat, current_dtx->ProjMat, viewport,
                    &x0, &y0, &z0 )) {
      printf("unproject1 failed\n");
   }
   if (!gluUnProject( (GLdouble) x, (GLdouble) (current_dtx->WinHeight-y),
                    (GLdouble) 1.0,
                    current_dtx->ModelMat, current_dtx->ProjMat, viewport,
                    &x1, &y1, &z1 )) {
      printf("unproject2 failed\n");
   }

   p[0] = x0;
   p[1] = y0;
   p[2] = z0;
   d[0] = x1-x0;
   d[1] = y1-y0;
   d[2] = z1-z0;
   len = sqrt( d[0]*d[0] + d[1]*d[1] + d[2]*d[2] );
   d[0] /= len;
   d[1] /= len;
   d[2] /= len;
}



void transparency_mode( Display_Context dtx, int mode )
{
   if (mode==1) {
      dtx->AlphaBlend = 1;
   }
   else {
      dtx->AlphaBlend = 0;
   }
}



void set_color( unsigned int c )
{
   GLfloat material_color[4];

   material_color[0] = UNPACK_RED( c )   / 255.0;
   material_color[1] = UNPACK_GREEN( c ) / 255.0;
   material_color[2] = UNPACK_BLUE( c )  / 255.0;
   material_color[3] = UNPACK_ALPHA( c ) / 255.0;
   glMaterialfv( GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, material_color );
   glColor4ubv( (GLubyte *) &c );
}



void set_depthcue( int onoff )
{
   if (onoff) {
      glEnable( GL_FOG );
   }
   else {
      glDisable( GL_FOG );
   }
}



void set_line_width( double w )
{
   glLineWidth( (GLfloat) w );
}




void set_pointer( int p )
{
   if (p) {
      /* make busy cursor */
      XDefineCursor( GfxDpy, current_dtx->GfxWindow,
                   XCreateFontCursor(GfxDpy,XC_watch) );
   }
   else {
      XDefineCursor( GfxDpy, current_dtx->GfxWindow,
                   XCreateFontCursor(GfxDpy,XC_top_left_arrow) );
   }
}


void set_pretty( int onoff )
{
   if (onoff) {
      /* turn on */
      set_pointer(1);
      pretty_flag = 1;
   }
   else {
      /* turn off */
      set_pointer(0);
      pretty_flag = 0;
   }
}



/*** These functions are taken from the OpenGL Programming Guide, ch. 10 ***/
static void accFrustum( GLdouble left, GLdouble right,
                        GLdouble bottom, GLdouble top,
                        GLdouble near, GLdouble far,
                        GLdouble pixdx, GLdouble pixdy,
                        GLdouble eyedx, GLdouble eyedy,
                        GLdouble focus )
{
   GLdouble xwsize, ywsize; 
   GLdouble dx, dy;
   GLint viewport[4];

   glGetIntegerv (GL_VIEWPORT, viewport);
       
   xwsize = right - left;
   ywsize = top - bottom;
       
   dx = -(pixdx*xwsize/(GLdouble) viewport[2] + eyedx*near/focus);
   dy = -(pixdy*ywsize/(GLdouble) viewport[3] + eyedy*near/focus);
       
   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
   glFrustum (left + dx, right + dx, bottom + dy, top + dy, near, far);
   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();
   glTranslatef(-eyedx, -eyedy, 0.0);
}


static void accPerspective( GLdouble fovy, GLdouble aspect,
                            GLdouble near, GLdouble far,
                            GLdouble pixdx, GLdouble pixdy,
                            GLdouble eyedx, GLdouble eyedy, GLdouble focus )
{
   GLdouble fov2,left,right,bottom,top;

   fov2 = ((fovy*M_PI) / 180.0) / 2.0;

   top = near / (cos(fov2) / sin(fov2));
   bottom = -top;

   right = top * aspect;
   left = -right;

   accFrustum( left, right, bottom, top, near, far,
              pixdx, pixdy, eyedx, eyedy, focus );
}




void start_aa_pass( int n )
{
#define EYEDIST 3.0
#define FOV 400
#ifdef JUNK
   float w, h, front, rear;
   if (pretty_flag) {
      if (n==0) {
        /* clear ACC buffer */
        glClear( GL_ACCUM_BUFFER_BIT );
      }
      if (current_dtx->Perspective) {
        float front, rear;

        front = EYEDIST - current_dtx->Scale * 1.75 * current_dtx->FrntClip;
        if (front<0.01)  front = 0.01;
        rear = EYEDIST + current_dtx->Scale * 1.75;
        accPerspective( FOV, current_dtx->AspectRatio, front, rear, 
                      xoffsets[n]/2.0, yoffsets[n]/2.0,
                      0.0, 0.0, 1.0 );
      }
      else {
        w = xoffsets[n] / current_dtx->WinWidth;
        h = yoffsets[n] / current_dtx->WinHeight;
        front = current_dtx->Scale * 1.75 * current_dtx->FrntClip;
        rear = -current_dtx->Scale * 1.75;
        accFrustum( -current_dtx->AspectRatio, current_dtx->AspectRatio,
                     -1.0, 1.0, front, rear, w, h,
                   0.0, 0.0, 0.0 );
      }
   }
#endif
}




void end_aa_pass( int n )
{
   if (pretty_flag) {
      glAccum( GL_ACCUM, 1.0/AA_PASSES );
      if (n==AA_PASSES-1) {
        glAccum( GL_RETURN, 1.0 );
      }
   }
}



/*** save_formats *****************************************************
   Return a set of flag bits indicating what image formats we can
   save as.
**********************************************************************/
int save_formats( void )
{
   int formats;
   char s[1000];
   struct stat buf;
   FILE *f;

   formats = VIS5D_XWD;

   strcpy( s, "./util/convert");
   if (stat(s, &buf)==0 && (buf.st_mode & S_IEXEC)) {
      /* found ImageMagick convert program so use it!! */
      formats |= VIS5D_PPM;
      formats |= VIS5D_GIF;
      formats |= VIS5D_PS;
      formats |= VIS5D_COLOR_PS;
      formats |= VIS5D_RGB;
      formats |= VIS5D_TGA;
   }
   else{
      formats |= VIS5D_PPM;

      if (installed("xpr"))  formats |= VIS5D_PS;

      if (installed("fromxwd")) {
         formats |= VIS5D_RGB;
      }
      else {
         /* the rest of the image formats depend on fromxwd */
         return formats;
      }

      if (installed("togif"))  formats |= VIS5D_GIF;
      if (installed("tops"))  formats |= VIS5D_COLOR_PS;
   }

   return formats;
}



int save_3d_window( char *filename, int format )
{
   char xwdname[100];
   char cmd[1000];
   char s[1000];
   struct stat buf;
   FILE *f;
   int use_convert;

   set_pointer(1);


   XRaiseWindow( GfxDpy, BigWindow);
   XSync( GfxDpy, 0 );

   strcpy( s, "./util/convert");
   if (stat(s, &buf)==0 && (buf.st_mode & S_IEXEC)) {
      /* found convert, use it! */
      printf("Using ImageMagick convert program\n");
      use_convert = 1;
   }
   else{
      use_convert = 0;
   }

   if (format==VIS5D_XWD) {
      strcpy( xwdname, filename );
   }
   else {
      strcpy( xwdname, TMP_XWD );
   }

   /* Make an X window dump file (.xwd) */
   f = fopen(xwdname,"w");
   if (!f) {
      printf("Error unable to open %s for writing\n", filename);
      set_pointer(0);
      return 0;
   }

   Window_Dump( GfxDpy, GfxScr, BigWindow, f );
   fclose(f);

   if (use_convert && format != VIS5D_XWD){
      if (format==VIS5D_RGB){
         sprintf( cmd, "./util/convert %s sgi:%s", xwdname, filename );
         printf("Executing: %s\n", cmd );
         system (cmd);
         unlink( xwdname );
      }
      if (format==VIS5D_GIF){
         sprintf( cmd, "./util/convert %s gif:%s", xwdname, filename );
         printf("Executing: %s\n", cmd );
         system (cmd);
         unlink( xwdname );
      }
      if (format==VIS5D_PS || format == VIS5D_COLOR_PS){
         sprintf( cmd, "./util/convert %s ps:%s", xwdname, filename );
         printf("Executing: %s\n", cmd );
         system (cmd);
         unlink( xwdname );
      }
      if (format==VIS5D_PPM){
         sprintf( cmd, "./util/convert %s ppm:%s", xwdname, filename );
         printf("Executing: %s\n", cmd );
         system (cmd);
         unlink( xwdname );
      }
      if (format==VIS5D_TGA){
         sprintf( cmd, "./util/convert %s tga:%s", xwdname, filename );
         printf("Executing: %s\n", cmd );
         system (cmd);
         unlink( xwdname );
      }
   }
   else{
      if (format==VIS5D_RGB) {
         sprintf( cmd, "fromxwd %s %s", xwdname, filename );
         printf("Executing: %s\n", cmd );
         system( cmd );
         unlink( xwdname );
      }
      else if (format==VIS5D_GIF) {
         /* convert xwd to rgb */
         sprintf( cmd, "fromxwd %s %s", xwdname, TMP_RGB );
         printf("Executing: %s\n", cmd );
         system( cmd );
         /* convert rgb to gif */
         sprintf( cmd, "togif %s %s", TMP_RGB, filename );
         printf("Executing: %s\n", cmd );
         system( cmd );
         unlink( xwdname );
         unlink( TMP_RGB );
      }
      else if (format==VIS5D_PS) {
         sprintf( cmd, "xpr -device ps -gray 4 %s >%s", xwdname, filename );
         printf("Executing: %s\n", cmd );
         system( cmd );
         unlink( xwdname );
      }
      else if (format==VIS5D_COLOR_PS) {
         /* convert xwd to rgb */
         sprintf( cmd, "fromxwd %s %s", xwdname, TMP_RGB );
         printf("Executing: %s\n", cmd );
         system( cmd );
         /* convert rgb to color PS */
         sprintf(cmd,"tops %s -rgb > %s", TMP_RGB, filename );
         printf("Executing: %s\n", cmd );
         system( cmd );
         unlink( xwdname );
         unlink( TMP_RGB );
      }
   }

   printf("Done writing image file.\n");
   set_pointer(0);
   return 1;
}

int save_snd_window(Display_Context dtx, char *filename, int format )
{
   char xwdname[100];
   char cmd[1000];
   char s[1000];
   struct stat buf;
   FILE *f;
   int use_convert;

   set_pointer(1);

   XRaiseWindow(GfxDpy, dtx->Sound.SoundCtrlWindow);
   XSync( GfxDpy, 0 );
   vis5d_draw_frame(dtx->dpy_context_index, 0);
   vis5d_swap_frame(dtx->dpy_context_index);
   XSync( GfxDpy, 0 );
   vis5d_draw_frame(dtx->dpy_context_index, 0);
   vis5d_swap_frame(dtx->dpy_context_index);
   XSync( GfxDpy, 0 );

   strcpy( s, "./util/convert");
   if (stat(s, &buf)==0 && (buf.st_mode & S_IEXEC)) {
      /* found convert, use it! */
      printf("Using ImageMagick convert program\n");
      use_convert = 1;
   }
   else{
      use_convert = 0;
   }

   if (format==VIS5D_XWD) {
      strcpy( xwdname, filename );
   }
   else {
      strcpy( xwdname, TMP_XWD );
   }

   /* Make an X window dump file (.xwd) */
   f = fopen(xwdname,"w");
   if (!f) {
      printf("Error unable to open %s for writing\n", filename);
      set_pointer(0);
      return 0;
   }
   if (dtx->Sound.soundwin){
      Window_Dump( GfxDpy, GfxScr,  dtx->Sound.soundwin, f );
      fclose(f);
   }
   else{
      return 0;
   }

   if (use_convert && format != VIS5D_XWD){
      if (format==VIS5D_RGB){
         sprintf( cmd, "./util/convert %s sgi:%s", xwdname, filename );
         printf("Executing: %s\n", cmd );
         system (cmd);
         unlink( xwdname );
      }
      if (format==VIS5D_GIF){
         sprintf( cmd, "./util/convert %s gif:%s", xwdname, filename );
         printf("Executing: %s\n", cmd );
         system (cmd);
         unlink( xwdname );
      }
      if (format==VIS5D_PS || format == VIS5D_COLOR_PS){
         sprintf( cmd, "./util/convert %s ps:%s", xwdname, filename );
         printf("Executing: %s\n", cmd );
         system (cmd);
         unlink( xwdname );
      }
      if (format==VIS5D_PPM){
         sprintf( cmd, "./util/convert %s ppm:%s", xwdname, filename );
         printf("Executing: %s\n", cmd );
         system (cmd);
         unlink( xwdname );
      }
      if (format==VIS5D_TGA){
         sprintf( cmd, "./util/convert %s tga:%s", xwdname, filename );
         printf("Executing: %s\n", cmd );
         system (cmd);
         unlink( xwdname );
      }
   }
   else{
      if (format==VIS5D_RGB) {
         sprintf( cmd, "fromxwd %s %s", xwdname, filename );
         printf("Executing: %s\n", cmd );
         system( cmd );
         unlink( xwdname );
      }
      else if (format==VIS5D_GIF) {
         /* convert xwd to rgb */
         sprintf( cmd, "fromxwd %s %s", xwdname, TMP_RGB );
         printf("Executing: %s\n", cmd );
         system( cmd );
         /* convert rgb to gif */
         sprintf( cmd, "togif %s %s", TMP_RGB, filename );
         printf("Executing: %s\n", cmd );
         system( cmd );
         unlink( xwdname );
         unlink( TMP_RGB );
      }
      else if (format==VIS5D_PS) {
         sprintf( cmd, "xpr -device ps -gray 4 %s >%s", xwdname, filename );
         printf("Executing: %s\n", cmd );
         system( cmd );
         unlink( xwdname );
      }
      else if (format==VIS5D_COLOR_PS) {
         /* convert xwd to rgb */
         sprintf( cmd, "fromxwd %s %s", xwdname, TMP_RGB );
         printf("Executing: %s\n", cmd );
         system( cmd );
         /* convert rgb to color PS */
         sprintf(cmd,"tops %s -rgb > %s", TMP_RGB, filename );
         printf("Executing: %s\n", cmd );
         system( cmd );
         unlink( xwdname );
         unlink( TMP_RGB );
      }
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

int print_snd_window( Display_Context dtx )
{
   static char ps_file[] = "/usr/tmp/Vis5D_image.ps";
   char cmd[1000];

   if (!save_snd_window( dtx, ps_file, VIS5D_PS ))  return 0;

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


static void set_transparency( int alpha )
{
   if (alpha==255) {
      /* disable */
      glDisable( GL_BLEND );
      glDisable( GL_POLYGON_STIPPLE );
   }
   else {
      /* enable */
      if (current_dtx->AlphaBlend) {
        glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
        glEnable( GL_BLEND );
      }
      else {
         int s = alpha / 64;    /* s is in [0,3] */
         if (s<3) {
            glPolygonStipple( (GLubyte *) stipple[s] );
            glEnable( GL_POLYGON_STIPPLE );
         }
      }
   }
}


/**********************************************************************/
/***                       Drawing Functions                        ***/
/**********************************************************************/


void draw_isosurface( int n,
#ifdef BIG_GFX
                      uint_4 *index,
#else
                      uint_2 *index,
#endif
                      int_2 verts[][3],
                      int_1 norms[][3],
                      unsigned int color )
{
   int i;
   GLfloat mat_color[4];

   mat_color[0] = UNPACK_RED( color )   / 255.0;
   mat_color[1] = UNPACK_GREEN( color ) / 255.0;
   mat_color[2] = UNPACK_BLUE( color )  / 255.0;
   mat_color[3] = UNPACK_ALPHA( color ) / 255.0;
   glMaterialfv( GL_FRONT, GL_AMBIENT_AND_DIFFUSE, mat_color );

   set_transparency( UNPACK_ALPHA(color) );
   glEnable( GL_LIGHTING );

   glPushMatrix();
   glScalef( 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE );

   /* Render the triangle strip */
   glBegin( GL_TRIANGLE_STRIP );
   for (i=0;i<n;i++) {
      int j = index[i];
      glNormal3bv( (GLbyte *) norms[j] );
      glVertex3sv( verts[j] );
   }
   glEnd();

   glPopMatrix();

   set_transparency(255);
   glDisable( GL_LIGHTING );
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

   glColorMaterial( GL_FRONT, GL_AMBIENT_AND_DIFFUSE );
   glEnable( GL_COLOR_MATERIAL );
   glEnable( GL_LIGHTING );

   glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
   glEnable( GL_BLEND ); 
   glAlphaFunc( GL_GREATER, 0.05 );
   glEnable( GL_ALPHA_TEST ); 

   set_transparency( alpha );

   glPushMatrix();
   glScalef( 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE );

   /* Render the triangle strip */
   glBegin( GL_TRIANGLE_STRIP );
   for (i=0;i<n;i++) {
      int j = index[i];
      unsigned int k = color_indexes[j];
      glColor4ubv( (GLubyte *) &color_table[k] );
      glNormal3bv( (GLbyte *) norms[j] );
      glVertex3sv( verts[j] );
   }
   glEnd();

   glPopMatrix();

   glDisable( GL_LIGHTING );
   glDisable( GL_COLOR_MATERIAL );
   glDisable( GL_BLEND );
   glDisable( GL_POLYGON_STIPPLE );
   glDisable( GL_ALPHA_TEST );
}




void draw_triangle_strip( int n, int_2 verts[][3], int_1 norms[][3],
                          unsigned int color )
{
   int i;
   GLfloat material_color[4];

   material_color[0] = UNPACK_RED( color )   / 255.0;
   material_color[1] = UNPACK_GREEN( color ) / 255.0;
   material_color[2] = UNPACK_BLUE( color )  / 255.0;
   material_color[3] = UNPACK_ALPHA( color ) / 255.0;
   glMaterialfv( GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, material_color );

   set_transparency( UNPACK_ALPHA(color) );
   glEnable( GL_LIGHTING );

   glPushMatrix();
   glScalef( 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE );

   /* Render the triangle strip */
   glBegin( GL_TRIANGLE_STRIP );
   for (i=0;i<n;i++) {
      glNormal3bv( (GLbyte *) norms[i] );
      glVertex3sv( verts[i] );
   }
   glEnd();

   glPopMatrix();

   glDisable( GL_LIGHTING );
   set_transparency(255);
}



void draw_colored_triangle_strip( int n,
                                  int_2 verts[][3], int_1 norms[][3],
                                  uint_1 color_indexes[],
                                  unsigned int color_table[], int alpha )
{
   int i;

   glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
   glEnable( GL_BLEND );
   glAlphaFunc( GL_GREATER, 0.05 ); 
   glEnable( GL_ALPHA_TEST );
   /* MJK 12.04.98 begin */
   glShadeModel(GL_SMOOTH);
   glEnable( GL_LIGHTING );
   glColorMaterial( GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE );
   glEnable( GL_COLOR_MATERIAL );
   /* MJK 12.04.98 end */

 
   if (alpha==-1) {
      glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
      glEnable( GL_BLEND );
      glAlphaFunc( GL_GREATER, 0.05 );
      glEnable( GL_ALPHA_TEST );
   }
   else {
      /* constant alpha */
      set_transparency( alpha );
   }

   glPushMatrix();
   glScalef( 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE );

   /* Render the triangle strip */
   glBegin( GL_TRIANGLE_STRIP );
   for (i=0;i<n;i++) {
      glColor4ubv( (GLubyte *) &color_table[color_indexes[i]] );
      /* MJK 12.4.98 */
      glNormal3bv( norms[i]);

      glVertex3sv( verts[i] );
   }
   glEnd();

   glPopMatrix();

   glDisable( GL_BLEND );
   glDisable( GL_POLYGON_STIPPLE );
   glDisable( GL_ALPHA_TEST );
   /* MJK 12.04.98 begin */
   glDisable( GL_LIGHTING );
   glDisable(GL_COLOR_MATERIAL); 
   /* MJK 12.04.98 end */
}




void draw_color_quadmesh( int rows, int columns, int_2 verts[][3],
                          uint_1 color_indexes[], unsigned int color_table[],
                          int alphavalue )
{
/*#define TEXTURE_SLICE*/
#ifdef TEXTURE_SLICE

   register int i, j, base1, base2;

   glTexImage1D( GL_TEXTURE_1D, 0, 4, 256, 0, GL_RGBA, GL_UNSIGNED_BYTE,
                 color_table );
   glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE );

   glTexParameteri( GL_TEXTURE_1D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
   glTexParameteri( GL_TEXTURE_1D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );

   glEnable( GL_TEXTURE_1D );
   glMatrixMode( GL_TEXTURE );
   glLoadIdentity();
   glScalef( 1.0/255.0, 1.0/255.0, 1.0/255.0 );

   glMatrixMode( GL_MODELVIEW );
   glPushMatrix();
   glScalef( 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE );

   glColor4f( 1.0, 1.0, 1.0, 1.0 );

   /* render mesh as a sequence of quad strips */
   for (i=0;i<rows-1;i++) {
      base1 = i * columns;
      base2 = (i+1) * columns;
      glBegin( GL_QUAD_STRIP );
      for (j=0;j<columns;j++) {
         glTexCoord1i( (GLint) color_indexes[base1+j] );
        glVertex3sv( verts[base1+j] );
         glTexCoord1i( (GLint) color_indexes[base2+j] );
        glVertex3sv( verts[base2+j] );
      }
      glEnd();
   }

   glDisable( GL_TEXTURE_1D );
   glPopMatrix();

#else
   register int i, j, base1, base2;
   unsigned int color_row1[1000];
   unsigned int color_row2[1000];
   unsigned int *row1ptr, *row2ptr, *tmp;

   if (alphavalue==-1) {
      /* variable alpha in the mesh */
      glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
      glEnable( GL_BLEND );
      glAlphaFunc( GL_GREATER, 0.05 );
      glEnable( GL_ALPHA_TEST );
   }
   else {
      /* constant alpha */
      set_transparency( alphavalue );
   }

   glPushMatrix();
   glScalef( 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE );

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
      glBegin( GL_QUAD_STRIP );
      for (j=0;j<columns;j++) {
        glColor4ubv( (GLubyte *) &row1ptr[j] );
        glVertex3sv( verts[base1+j] );
        glColor4ubv( (GLubyte *) &row2ptr[j] );
        glVertex3sv( verts[base2+j] );
      }
      glEnd();
      /* swap row1ptr and row2ptr */
      tmp = row1ptr;
      row1ptr = row2ptr;
      row2ptr = tmp;
   }

   glPopMatrix();

   glDisable( GL_BLEND );
   glDisable( GL_POLYGON_STIPPLE );
   glDisable( GL_ALPHA_TEST );
#endif
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

#ifdef FOO
   if (alphavalue==-1) {
      /* variable alpha in the mesh */
      glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
      glEnable( GL_BLEND );
      glAlphaFunc( GL_GREATER, 0.05 );
      glEnable( GL_ALPHA_TEST );
   }
   else {
      /* constant alpha */
      set_transparency( alphavalue );
   }
#endif

   glEnable( GL_LIGHTING );
   glColorMaterial( GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE );
   glEnable( GL_COLOR_MATERIAL );

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
      glBegin( GL_QUAD_STRIP );
      for (j=0;j<columns;j++) {
        glColor4ubv( (GLubyte *) &row1ptr[j] );
        glNormal3fv( norms[base1+j] );
        glVertex3fv( verts[base1+j] );
        glColor4ubv( (GLubyte *) &row2ptr[j] );
         glNormal3fv( norms[base2+j] );
        glVertex3fv( verts[base2+j] );
      }
      glEnd();
      /* swap row1ptr and row2ptr */
      tmp = row1ptr;
      row1ptr = row2ptr;
      row2ptr = tmp;
   }

   glDisable( GL_LIGHTING );
   glDisable( GL_COLOR_MATERIAL );
/*FOO
   glDisable( GL_BLEND );
   glDisable( GL_POLYGON_STIPPLE );
   glDisable( GL_ALPHA_TEST );
*/
}



void draw_wind_lines( int nvectors, int_2 verts[][3], unsigned int color )
{
   int i, j;

   glShadeModel( GL_FLAT );
   glDisable( GL_DITHER );
   glColor4ubv( (GLubyte *) &color );

   glPushMatrix();
   glScalef( 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE );

   glBegin( GL_LINES );
   for (i=0;i<nvectors;i++) {
      j = i * 4;
      /* main vector */
      glVertex3sv( verts[j] );
      glVertex3sv( verts[j+1] );
      /* head vectors */
      glVertex3sv( verts[j+1] );
      glVertex3sv( verts[j+2] );
      glVertex3sv( verts[j+1] );
      glVertex3sv( verts[j+3] );
   }
   glEnd();

   glShadeModel( GL_SMOOTH );
   glEnable( GL_DITHER );
   glPopMatrix();
}


void draw_disjoint_lines( int n, int_2 verts[][3], unsigned int color )
{
   int i;
 
   glColor4ubv( (GLubyte *) &color );
   glPushMatrix();
   glScalef( 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE );
   glShadeModel( GL_FLAT );
   glDisable( GL_DITHER );
   glBegin( GL_LINES );
   for (i=0;i<n;i+=2 ) {
      glVertex3sv( verts[i] );
      glVertex3sv( verts[i+1] );
   }
   glEnd();
   glShadeModel( GL_SMOOTH );
   glEnable( GL_DITHER );
   glPopMatrix();
}


void draw_colored_disjoint_lines( int n, int_2 verts[][3],
                                    uint_1 color_indexes[],
                                    unsigned int color_table[] )
{
   int i;

   glPushMatrix();
   glScalef( 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE );
   glBegin( GL_LINES );
   for (i=0;i<n;i+=2 ) {
      glColor4ubv( (GLubyte *) &color_table[color_indexes[i/2]] );
      glVertex3sv( verts[i] );
      glVertex3sv( verts[i+1] );
   }
   glEnd();
   glPopMatrix();
}



void draw_polylines( int n, int_2 verts[][3], unsigned int color )
{
   int i;

   glColor4ubv( (GLubyte *) &color );
   glShadeModel( GL_FLAT );
   glDisable( GL_DITHER );

   glPushMatrix();
   glScalef( 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE );
   glBegin( GL_LINE_STRIP );
   for (i=0;i<n;i++ ) {
      glVertex3sv( verts[i] );
   }
   glEnd();
   glPopMatrix();
   glShadeModel( GL_SMOOTH );
   glEnable( GL_DITHER );
}



void draw_colored_polylines( int n, int_2 verts[][3],
                             uint_1 color_indexes[],
                             unsigned int color_table[] )
{
   int i;

   glPushMatrix();
   glScalef( 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE, 1.0/VERTEX_SCALE );
   glBegin( GL_LINE_STRIP );
   for (i=0;i<n;i++ ) {
      glColor4ubv( (GLubyte *) &color_table[color_indexes[i]] );
      glVertex3sv( verts[i] );
   }
   glEnd();
   glPopMatrix();
}




void draw_multi_lines( int n, float verts[][3], unsigned int color )
{
   int i;

   glColor4ubv( (GLubyte *) &color );

   glBegin( GL_LINE_STRIP );
   for (i=0;i<n;i++ ) {
      if (verts[i][0]==-999.0) {
         /* start new line */
         glEnd();
         glBegin( GL_LINE_STRIP );
      }
      else {
         glVertex3fv( verts[i] );
      }
   }
   glEnd();
}

 


void draw_cursor( Display_Context dtx, int style, float x, float y, float z, unsigned int color )
/*** Style 0: Line Cursor 
     Style 1: Polygon Cursor
     Style 2: Sounding Cursor
***/


{
   static GLuint line_cursor;
   static GLuint polygon_cursor;
   static GLuint sounding_cursor;
   if (dtx->init_cursor_flag) {
    /* do one-time initialization */

  
    /* Make Sounding_cursor vertical line */
      sounding_cursor = glGenLists(1);
      glNewList( sounding_cursor, GL_COMPILE );
      glLineWidth(3.0); 
      glBegin( GL_LINES );
      glVertex3f( 0.0, 0.0, dtx->Zmin);
      glVertex3f( 0.0, 0.0, dtx->Zmax );
      glEnd();
      glLineWidth(1.0);
      glBegin( GL_LINES );
      glVertex3f( -0.05, 0.0, dtx->Zmax);
      glVertex3f(  0.05, 0.0, dtx->Zmax);
      glVertex3f( 0.0, -0.05, dtx->Zmax);
      glVertex3f( 0.0,  0.05, dtx->Zmax);
      glEnd();
      glEndList();
 

    /* Make line-segment cursor object */
      line_cursor = glGenLists(1);
      glNewList( line_cursor, GL_COMPILE );
      glBegin( GL_LINES );
      glVertex3f( -0.05, 0.0, 0.0 );
      glVertex3f(  0.05, 0.0, 0.0 );
      glVertex3f( 0.0, -0.05, 0.0 );
      glVertex3f( 0.0,  0.05, 0.0 );
      glVertex3f( 0.0, 0.0, -0.05 );
      glVertex3f( 0.0, 0.0,  0.05 );
      glEnd();
      glEndList();

   /* Makt polygona cursor object */
      polygon_cursor = glGenLists(1);
      glNewList( polygon_cursor, GL_COMPILE );
      glBegin( GL_QUADS );
      /* X axis */
      glVertex3f( -0.05, -0.005,  0.005 );
      glVertex3f( -0.05,  0.005, -0.005 );
      glVertex3f(  0.05,  0.005, -0.005 );
      glVertex3f(  0.05, -0.005,  0.005 );
      glVertex3f( -0.05, -0.005, -0.005 );
      glVertex3f( -0.05,  0.005,  0.005 );
      glVertex3f(  0.05,  0.005,  0.005 );
      glVertex3f(  0.05, -0.005, -0.005 );
      /* Y-axis */
      glVertex3f( -0.005, -0.05,  0.005 );
      glVertex3f(  0.005, -0.05, -0.005 );
      glVertex3f(  0.005,  0.05, -0.005 );
      glVertex3f( -0.005,  0.05,  0.005 );
      glVertex3f( -0.005, -0.05, -0.005 );
      glVertex3f(  0.005, -0.05,  0.005 );
      glVertex3f(  0.005,  0.05,  0.005 );
      glVertex3f( -0.005,  0.05, -0.005 );
      /* Z-axis */
      glVertex3f( -0.005, -0.005,  0.05 );
       glVertex3f(  0.005,  0.005,  0.05 );
      glVertex3f(  0.005,  0.005, -0.05 );
      glVertex3f( -0.005, -0.005, -0.05 );
      glVertex3f( -0.005,  0.005,  0.05 );
      glVertex3f(  0.005, -0.005,  0.05 );
      glVertex3f(  0.005, -0.005, -0.05 );
      glVertex3f( -0.005,  0.005, -0.05 );
      glEnd();
      glEndList();

     dtx->init_cursor_flag =  0;
   }

   glColor4ubv( (GLubyte *) &color );
   glPushMatrix();
   if (style == 2)  z = 0 ; 
   glTranslatef( x, y, z  );
   if (style == 1) {
      glCallList( polygon_cursor );
   }
   if (style == 2) {
      glCallList( sounding_cursor ); 
   }
   else {
      glCallList( line_cursor );
   }
   glPopMatrix();
}




/**** OLD primitives ***/



void polyline( float vert[][3], int n )
{
   register int i;

   glBegin( GL_LINE_STRIP );
   for (i=0;i<n;i++) {
      glVertex3fv( vert[i] );
   }
   glEnd();
}




void disjointpolyline( float vert[][3], int n )
{
   register int i;

   glShadeModel( GL_FLAT );   /* faster */
   glDisable( GL_DITHER );
   glBegin( GL_LINES );
   for (i=0;i<n;i+=2) {
      glVertex3fv( vert[i] );
      glVertex3fv( vert[i+1] );
   }
   glEnd();
   glShadeModel( GL_SMOOTH );
   glEnable( GL_DITHER );
}




#ifdef JUNK
void quadmeshnorm( float vert[][3], float norm[][3], unsigned int color[],
                   int rows, int cols )
{
   register int i, j, base1, base2;

   glEnable( GL_LIGHTING );
   glColorMaterial( GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE );
   glEnable( GL_COLOR_MATERIAL );
   /* break mesh into strips */
   for (i=0;i<rows-1;i++) {
      base1 = i * cols;
      base2 = (i+1) * cols;
      glBegin( GL_QUAD_STRIP );
      for (j=0;j<cols;j++) {
        glColor4ubv( (GLubyte *) &color[base1+j] ); 
        glNormal3fv( norm[base1+j] );
        glVertex3fv( vert[base1+j] );
        glColor4ubv( (GLubyte *) &color[base2+j] ); 
        glNormal3fv( norm[base2+j] );
        glVertex3fv( vert[base2+j] );
      }
      glEnd();
   }

   glDisable( GL_COLOR_MATERIAL );
   glDisable( GL_LIGHTING );
}
#endif




void polyline2d( short vert[][2], int n )
{
   int i;

   glShadeModel( GL_FLAT );
   glDisable( GL_DITHER );
   glBegin( GL_LINE_STRIP );
   for (i=0;i<n;i++) {
      glVertex2i( vert[i][0], current_dtx->WinHeight-vert[i][1] );
   }
   glEnd();
   glShadeModel( GL_SMOOTH );
   glEnable( GL_DITHER );
}


void draw_text( int xpos, int ypos, char *str )
{
   int len = strlen(str);

   glRasterPos2i( xpos, current_dtx->WinHeight-ypos );
   glListBase( current_dtx->gfx.fontbase );
   glCallLists( len, GL_UNSIGNED_BYTE, str );
}



int text_width( char *str )
{
   int dir, ascent, descent;
   XCharStruct overall;

   XTextExtents( current_dtx->gfx.font, str, strlen(str),
                 &dir, &ascent, &descent, &overall);
   return overall.width;
}



/*
 * Start a new display list object and return its ID or zero if error.
 */
int begin_object( void )
{
   GLuint obj;

   obj = glGenLists( 1 );
   if (obj>0) {
      glNewList( obj, GL_COMPILE_AND_EXECUTE );
   }
   return (int) obj;
}


/*
 * End construction of a display list object.
 */
void end_object( void )
{
   glEndList();
}


/*
 * Draw a display list object.
 */
void call_object( int obj )
{
   glCallList( (GLuint) obj );
}


/*
 * Delete a display list object.
 */
void delete_object( int objnum )
{
   glDeleteLists( (GLuint) objnum, 1 );
}



#ifdef MESA

/*
 * Return a copy of Mesa's back buffer image.
 */
XImage *mesa_read_image( void )
{
   unsigned int w = current_dtx->WinWidth;
   unsigned int h = current_dtx->WinHeight;
   XImage *backimage;

   XMesaGetBackBuffer( XMesaGetCurrentBuffer(), NULL, &backimage );
   return XSubImage( backimage, 0, 0, w, h );
}


/*
 * Write an XImage into the Mesa window.
 */
void mesa_draw_image( XImage *image )
{
   XImage *backimage;
   unsigned int n;

   XMesaGetBackBuffer( XMesaGetCurrentBuffer(), NULL, &backimage );
   n = backimage->bytes_per_line * backimage->height;
   memcpy( backimage->data, image->data, n );
}


#endif /*ifdef MESA*/
