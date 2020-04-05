/* Vis5D version 5.2 */

/*
Vis5D system for visualizing five dimensional gridded data sets
Copyright (C) 1990-2000  Bill Hibbard, Brian Paul, Dave Santek,
and Andre Battaiola.

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
 * In the Makefile one of the following symbols MUST be defined in CFLAGS
 * to specify the graphics system/library:
 *
 *        Symbol     System                        graphics.c
 * ------------------------------------------------------------
 *        XFDI       Stardent/Stellar           graphics.xfdi.c
 *        SGI_GL     Silicon Graphics GL        graphics.gl.c
 *        IBM_GL     IBM GL                     graphics.ibmgl.c
 *        VOGL       VOGL using X driver        graphics.vogl.c
 *        DENALI     Kubota/Alpha Denali        graphics.denali.c
 *        PEX        HP (others?) PEX           graphics.pex.c
 *        OPENGL     OpenGL on any system       graphics.ogl.c
 */



/* This module does 3-D rendering in terms of GL, XFDI, VOGL, etc.
   The 3-D coordinate system is left-handed:

      +Y|                 +X is right
        |  / +Z           +Y is up
        | /               +Z is away
        |/
        +------ +X

   where coordinates to 3-D drawing functions should be in [-1,1]^3.

   The 2-D coordinate system is in terms of pixels with (0,0) in the
   upper-left corner of the window:

      +-------- +X
      |
      |
      |
      | +Y



   The basic order of events is:

   init_graphics();
   make_window();
   while (1) {
      clear_window();
      set_3d();
      draw 3-D stuff
      set_2d();
      draw 2-D stuff;
      update_window();
   };

*/



#ifndef GRAPHICS_H
#define GRAPHICS_H



#include <X11/Xlib.h>
#ifdef SGI_GL
#  include <gl/glws.h>
#endif
#include "globals.h"
#include "matrix.h"



/*
 * Antialiasing passes:
 */
#ifdef XFDI
#  define AA_PASSES 9
#endif
#ifdef SGI_GL
#  define AA_PASSES 9
#endif
#ifdef OPENGL
#  define AA_PASSES 9
#endif
#ifdef DENALI
#  define AA_PASSES 9
#endif
#ifndef AA_PASSES
#  define AA_PASSES 1
#endif


/*
 * Byte order of packed colors:  (colors are stored in 4-byte unsigned ints)
 */
#ifdef XFDI
#  define PACK_COLOR(R,G,B,A)   ( (R)<<16 | (G)<<8 | (B) )
#  define UNPACK_RED(X)         ( ( (X) >> 16 ) & 0xff )
#  define UNPACK_GREEN(X)       ( ( (X) >> 8 ) & 0xff )
#  define UNPACK_BLUE(X)        ( (X) & 0xff )
#  define UNPACK_ALPHA(X)       0
#endif
#if defined(SGI_GL) || defined(IBM_GL) || defined (DENALI) || defined(PEX)
#  define PACK_COLOR(R,G,B,A)   ( (A)<<24 | (B)<<16 | (G)<<8 | (R) )
#  define UNPACK_RED(X)         ( (X) & 0xff )
#  define UNPACK_GREEN(X)       ( ( (X) >> 8 ) & 0xff )
#  define UNPACK_BLUE(X)        ( ( (X) >> 16 ) & 0xff )
#  define UNPACK_ALPHA(X)       ( ( (X) >> 24 ) & 0xff )
#endif
#if defined(VOGL)
#  define PACK_COLOR(R,G,B,A)   ( (A)<<24 | (R)<<16 | (G)<<8 | (B) )
#  define UNPACK_RED(X)         ( ( (X) >> 16 ) & 0xff )
#  define UNPACK_GREEN(X)       ( ( (X) >> 8 ) & 0xff )
#  define UNPACK_BLUE(X)        ( (X) & 0xff )
#  define UNPACK_ALPHA(X)       ( ( (X) >> 24 ) & 0xff )
#endif
#if defined(OPENGL)
#  ifdef LITTLE
#    define PACK_COLOR(R,G,B,A)   ( (A)<<24 | (B)<<16 | (G)<<8 | (R) )
#    define UNPACK_RED(X)         ( (X) & 0xff )
#    define UNPACK_GREEN(X)       ( ( (X) >> 8 ) & 0xff )
#    define UNPACK_BLUE(X)        ( ( (X) >> 16 ) & 0xff )
#    define UNPACK_ALPHA(X)       ( ( (X) >> 24 ) & 0xff )
#  else
#    define PACK_COLOR(R,G,B,A)   ( (R)<<24 | (G)<<16 | (B)<<8 | (A) )
#    define UNPACK_RED(X)         ( ( (X) >> 24 ) & 0xff )
#    define UNPACK_GREEN(X)       ( ( (X) >> 16 ) & 0xff )
#    define UNPACK_BLUE(X)        ( ( (X) >> 8 ) & 0xff )
#    define UNPACK_ALPHA(X)       ( (X) & 0xff )
#  endif
#endif


#include <X11/Xlib.h>


/*
 * Graphics variables which have the same value for all vis5d contexts:
 */
extern Window BigWindow;              /* The main big window */
extern BigWinWidth;
extern BigWinHeight;

/* MJK 12.21.98 */
extern BigWinFull;                    /* 1 = full size window */

extern int StaticWin;
extern int StaticWinXPos;
extern int StaticWinYPos;
extern int StaticWinWidth;
extern int StaticWinHeight;

extern Display *GfxDpy;               /* The X display */
extern int GfxScr;                    /* The X screen number */
extern Visual *GfxVisual;             /* The X visual */
extern Colormap GfxColormap;          /* The X colormap */
extern int GfxDepth;                  /* Depth of the 3-D window */
extern int ScrWidth, ScrHeight;       /* screen size */
extern int HQR_available;             /* High-Quality rendering available? */
extern int Perspec_available;         /* Perspective rendering available? */
extern Display *SndDpy;
extern int SndScr;
extern Visual *SndVisual;
extern Colormap SndColormap;
extern int SndDepth;
extern int SndScrWidth, SndScrHeight;
extern int SndRootWindow; 



/**********************************************************************/
/*****    Functions in graphics.all.c  (library independent)      *****/
/**********************************************************************/


/*
 * Find a window by name, return its ID.
 */
extern Window find_window( Display *dpy, int scr, Window start,
                           char *name );



/*
 * Find the best visual the display supports and initialize the global
 * X/graphics variables.  This isn't needed by all graphics libraries.
 */
extern int find_best_visual( Display *dpy, int scr, int *depth,
                             Visual **visual, Colormap *cmap );



/*
 * Open the default display, determine screen size, call graphics
 * library-dependent initialization function.  This should only be
 * called once, not per-context.
 */
extern void init_graphics( void );





/**********************************************************************/
/*****    Functions in graphics.*.c  (library dependent)          *****/
/**********************************************************************/



#ifdef SGI_GL

extern int use_gl_window( Context ctx, Display *dpy, Window win, long winid );


extern int use_glx_window( Context ctx, Display *dpy,
                           Window window, GLXconfig *glctx );

#endif


#ifdef OPENGL
extern int use_opengl_window( Display_Context dtx, Display *dpy, Window window,
                              GLXContext glctx, XFontStruct *xfont );
#endif



/*
 * Do library-depedent initializations.  This is only called from
 * init_graphics().
 */
extern void init_graphics2( void );



/*
 * Call this before exiting vis5d.
 */
extern void terminate_graphics( void );



/*
 * Call this to free the graphics resources attached to a vis5d context.
 */
extern void free_graphics( Display_Context dtx );



/*
 * Specify the font to use in the 3-D window.
 * Must be called before make_3d_window to take effect.
 */
/* MJK 12.10.98 */
extern int set_3d_font(  Display_Context dtx, char *name, int size );

extern int get_3d_font(  Display_Context dtx, char *name, int *size);


/*
 * Contributed by Michael Manyin.
 */
extern void context_init( Context ctx, long win_id, int width, int height );



/*
 * Make a 3-D graphics window.
 * Input:  ctx - the vis5d context
 *         title - window title string
 *         xpos,ypos - position in pixel from upper-left corner
 *         width, height - width and height of window in pixels
 * Return:  1 = success, 0 = error
 */
extern int make_3d_window( Display_Context dtx,
                           char *title,
                           int xpos, int ypos,
                           int width, int height );

extern int make_big_window( char *title, int xpos, int ypos,
                            int width, int height);



/*
 * Return a set of flag bits indicating what image formats we can write.
 */
extern int save_formats( void );



/*
 * Set the current rendering context/window.
 */
extern void set_current_window( Display_Context dtx );



/*
 * Save the current image in the 3-D window to the named file.
 * Input:  filename - image filename
 *         format - VIS5D_RGB = SGI .rgb
 *                  VIS5D_GIF = .GIF
 *                  VIS5D_XWD = .xwd   (X window dump)
 *                  VIS5D_PS = black & white PostScript
 *                  VIS5D_COLOR_PS = color PostScript
 * Return:  1 = ok, 0 = error
 */
extern int save_3d_window( char filename[], int format );

extern int save_snd_window( Display_Context dtx, char filename[], int format );

extern void finish_rendering( void );


/*
 * Print the window image to the default PostScript printer.
 * Return:  1 = ok, 0 = error
 */
extern int print_3d_window( void );

extern int print_snd_window( Display_Context dtx );


/*
 * Specify the background color for the window.
 */
extern void clear_color( unsigned int bgcolor );



/*
 * Clear the graphics window.  This is called prior to rendering a frame.
 */
extern void clear_3d_window( void );


/*
 * Called when window size changes.  Viewport gets updated.
 */
extern void resize_3d_window( int width, int height );

/*
 * Called when window size of the BIG window changes. 
 */
extern void resize_BIG_window( int width, int height );


/*
 * Display contents of back buffer.
 */
extern void swap_3d_window( void );



/*
 * Begin 2-D rendering.  All coordinates given to the 2-D drawing routines
 * are in window pixel coordinates with (0,0) in the upper-left corner.
 */
extern void set_2d( void );



/*
 * This is to be called prior to any 3-D rendering calls.
 * Input:  perspective - 0 = othrographic, 1 = perspective
 *         frontclip - position of front clipping plane, 0=all visible,
 *                     1=all clipped
 *         zoom - zoom factor, 1.0 = normal
 *         modelmat - rotation/translation matrix
 */
extern void
set_3d( int perspective, float frontclip, float zoom, float *modelmat);

extern void clipping_on( void );
extern void clipping_off( void );


/*
 * Use current transformation and viewing information to project a
 * point p from 3-D graphics coordinates to 2-D window coordinates
 * in [0,WinWidth-1]x[0,WinHeight-1].
 * Input:  p - 3-D point in graphics space
 * Output:  x, y - 2-D point in normalized window coordinates.
 */
extern void project( float p[3], float *x, float *y );



/*
 * Given a 2-D window coordinate in [0,WinWidth-1]x[0,WinHeight-1],
 * return the parametric equation of a line in 3-D such that the
 * projection of the line from 3-D to 2-D is a point at the window
 * coordinate.
 * Input:  x, y - window coordinate.
 * Output:  p, d - parametric equation of line:  l = p + t*d
 *                 NOTE, d will have unit length
 */
extern void unproject( float x, float y, float p[3], float d[3] );



/*
 * Specify how to render semi-transparent surfaces.
 * Input:  mode - 0 = use "screendoor" (stipple) technique
 *                1 = use alpha blending
 */
extern void transparency_mode( Display_Context dtx, int mode );



/*
 * Set the current drawing color.
 */
extern void set_color( unsigned int color );




/*
 * Set the line depth cueing flag on or off.
 */
extern void set_depthcue( int onoff );



/*
 * Set the width of lines.
 * Input:  w - width in pixels.
 */
extern void set_line_width( double w );



/*
 * Set pretty rendering mode on or off.
 * Input: onoff - 1 = on, 0 = off.
 */
extern void set_pretty( int onoff );



/*
 * Start antialiasing pass n, where n is in [0..AA_PASSES-1].
 */
extern void start_aa_pass( int n );



/*
 * End an antialiasing pass n, where n is in [0..AA_PASSES-1].
 */
extern void end_aa_pass( int n );



/**********************************************************************/
/***                        Drawing Functions                       ***/
/**********************************************************************/


/*
 * Render a compressed isosurface.
 * Input:  n - number of indexes
 *         index - array of [n] indexes into verts[] and norms[]
 *         verts - array of scaled integer vertices
 *         norms - array of scaled integer normals
 *         color - the isosurface color
 */
#ifdef BIG_GFX
extern void draw_isosurface( int n, uint_4 *index,
                             int_2 verts[][3], int_1 norms[][3],
                             unsigned int color );
#else
extern void draw_isosurface( int n, uint_2 *index,
                             int_2 verts[][3], int_1 norms[][3],
                             unsigned int color );
#endif


/*
 * Render a compressed, COLORED, isosurface.
 * Input:  n - number of indexes
 *         index - array of [n] indexes into verts[] and norms[]
 *         verts - array of scaled integer vertices
 *         norms - array of scaled integer normals
 *         color_indexes - array of indexes into the color table
 *         color_table - array [256] of 4-byte packed colors
 *         alphavalue - -1=variable, 0..255=constant
 */
extern void draw_colored_isosurface( int n,
#ifdef BIG_GFX
                                     uint_4 *index,
#else
                                     uint_2 *index,
#endif
                                     int_2 verts[][3],
                                     int_1 norms[][3],
                                     uint_1 color_indexes[],
                                     unsigned int color_table[],
                                     int alphavalue );



/*
 * Draw a lit triangle strip.
 * Input:  n - number of vertices
 *         verts - array [n][3] of vertices
 *         norms = array [n][3] of normals
 *         color - the color
 */
extern void draw_triangle_strip( int n,
                                 int_2 verts[][3], int_1 norms[][3],
                                 unsigned int color );


/*
 * Draw a lit triangle strip with per-vertex colors.
 */
extern void draw_colored_triangle_strip( int n,
                                         int_2 verts[][3], int_1 norms[][3],
                                         uint_1 color_indexes[],
                                         unsigned int *color_table, int alpha );


/*
 * Draw a per-vertex colored quadrilateral mesh.
 * Used to draw colored slices.
 * Input:  rows, columns - size of 2-D mesh (vertices)
 *         verts - array [rows*columns][3] of scaled 1-byte int vertices
 *         color_indexes - array [rows*columns] of color indexes
 *         color_table - array of colors indexed by color_indexes
 *         alphavalue - the alpha value: 0..255 = constant, -1 = variable
 */
extern void draw_color_quadmesh( int rows, int columns,
                                 int_2 verts[][3],
                                 uint_1 color_indexes[],
                                 unsigned int color_table[],
                                 int alphavalue );



/*
 * Draw a lighted, per-vertex colored quadrilateral mesh.
 * Used to draw colored topography.
 * Input:  rows, columns - size of 2-D mesh (vertices)
 *         verts - array [rows*columns][3] of floating point vertices
 *         norms - array [rows*columns][3] of floating point normals
 *         color_indexes - array [rows*columns] of color indexes
 *         color_table - array of colors indexed by color_indexes
 */
extern void draw_lit_color_quadmesh( int rows, int columns,
                                     float verts[][3],
                                     float norms[][3],
                                     uint_1 color_indexes[],
                                     unsigned int color_table[] );



/*
 * Draw wind vector line segments.
 * Input:  nvectors - number of wind vectors
 *         verts - array [nvectors*4][3] of scaled 2-byte integers
 *         color - the color
 */
extern void draw_wind_lines( int nvectors,
                             int_2 verts[][3],
                             unsigned int color );



/*
 * Draw a set of disjoint lines whose vertices are scaled 2-byte ints.
 * Input:  n - number of vertices, not lines
 *         verts - array [][3] of int_2 vertices
 *         color - the color
 */
extern void draw_disjoint_lines( int n, int_2 verts[][3],
                                 unsigned int color );





extern void draw_colored_disjoint_lines( int n, int_2 verts[][3],
                                    uint_1 color_indexes[],
                                    unsigned int color_table[] );
 
/*
 * Draw a polyline whose vertices are scaled 2-byte ints.
 * Input:  n - number of vertices, not lines
 *         verts - array [][3] of int_2 vertices
 *         color - the color
 */
extern void draw_polylines( int n, int_2 verts[][3],
                            unsigned int color );


/*
 * Draw a polyline as above but colored per-vertex accordint to a value
 * in a color table.
 */
extern void draw_colored_polylines( int n, int_2 verts[][3],
                                    uint_1 color_indexes[],
                                    unsigned int color_table[] );


/*
 * Render a number of polylines.  When the X component of a vertex is -999.0
 * we start a new line.
 * Input:  n - number of vertices
 *         verts - array [n][3] of floating point vertices
 *         color - line color
 */
extern void draw_multi_lines( int n, float verts[][3],
                              unsigned int color );




/*
 * Draw a 3-D cursor at location (x,y,z).
 * Input:  style - 0=lines, 1=polygons
 *         x, y, z - position
 *         color - cursor color
 */
extern void draw_cursor( Display_Context dtx, int style,
                         float x, float y, float z,
                         unsigned int color );





/*********OLD FUNCTIONS **********/


/*
 * Draw a quadrilateral mesh with normals and vertex colors.
 */
extern void quadmeshnorm( float vert[][3], float norm[][3],
                          unsigned int color[], int rows, int cols );



/*
 * Draw a 3-D poly line.
 */
extern void polyline( float vert[][3], int n );



/*
 * Draw a series of disjoint 3-D lines.
 */
extern void disjointpolyline( float vert[][3], int n );





/*
 * Draw a 2-D poly line.  Coordinates are in pixels with the origin
 * in the upper-left corner of the window.  NOTE:  vertices are shorts.
 */
extern void polyline2d( short vert[][2], int n );



/*
 * Draw a text string at a position.
 */
extern void draw_text( int xpos, int ypos, char *str );



/*
 * Return the width of a text string in pixels.
 */
extern int text_width( char *str );




/*
 * Display list functions:
 */
extern int begin_object( void );

extern void end_object( void );

extern void call_object( int objnum );

extern void delete_object( int objnum );

#endif

