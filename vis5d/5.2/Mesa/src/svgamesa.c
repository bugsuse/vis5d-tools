/* $Id: svgamesa.c,v 1.6 1997/07/24 01:45:24 brianp Exp $ */

/*
 * Mesa 3-D graphics library
 * Version:  2.4
 * Copyright (C) 1995-1997  Brian Paul
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */


/*
 * $Log: svgamesa.c,v $
 * Revision 1.6  1997/07/24 01:45:24  brianp
 * added doubleBuffer flag to SVGAMesaCreateContext()
 *
 * Revision 1.5  1997/07/24 01:21:56  brianp
 * changed precompiled header symbol from PCH to PC_HEADER
 *
 * Revision 1.4  1997/05/28 03:26:29  brianp
 * added precompiled header (PCH) support
 *
 * Revision 1.3  1997/05/26 21:15:37  brianp
 * now pass red/green/blue/alpha bits to gl_create_visual()
 *
 * Revision 1.2  1996/10/15 00:23:51  brianp
 * cleaned up code to work with Mesa 2.0
 *
 * Revision 1.1  1996/09/13 01:38:16  brianp
 * Initial revision
 *
 */


/*
 * Linux SVGA/Mesa interface.
 *
 * This interface is not finished!  Still have to implement pixel
 * reading functions and double buffering.  Then, look into accelerated
 * line and polygon rendering.  And, clean up a bunch of other stuff.
 * Any volunteers?
 */


#ifdef SVGA


#ifdef PC_HEADER
#include "all.h"
#else
#include <stdio.h>
#include <stdlib.h>
#include <vga.h>
#include "GL/svgamesa.h"
#include "context.h"
#include "matrix.h"
#include "types.h"
#endif


struct svgamesa_context {
   GLcontext *gl_ctx;		/* the core Mesa context */
   GLvisual *gl_vis;		/* describes the color buffer */
   GLframebuffer *gl_buffer;	/* the ancillary buffers */
   GLuint index;		/* current color index */
   GLint red, green, blue;	/* current rgb color */
   GLint width, height;		/* size of color buffer */
   GLint depth;			/* bits per pixel (8,16,24 or 32) */
};


static SVGAMesaContext SVGAMesa = NULL;    /* the current context */



/*
 * Convert Mesa window Y coordinate to VGA screen Y coordinate:
 */
#define FLIP(Y)  (SVGAMesa->height-(Y)-1)



/**********************************************************************/
/*****                 Miscellaneous functions                    *****/
/**********************************************************************/


static void get_buffer_size( GLcontext *ctx, GLuint *width, GLuint *height )
{
   *width = SVGAMesa->width = vga_getxdim();
   *height = SVGAMesa->height = vga_getydim();
}


/* Set current color index */
static void set_index( GLcontext *ctx, GLuint index )
{
   SVGAMesa->index = index;
   vga_setcolor( index );
}


/* Set current drawing color */
static void set_color( GLcontext *ctx,
                       GLubyte red, GLubyte green,
                       GLubyte blue, GLubyte alpha )
{
   SVGAMesa->red = red;
   SVGAMesa->green = green;
   SVGAMesa->blue = blue;
   vga_setrgbcolor( red, green, blue );
}


static void clear_index( GLcontext *ctx, GLuint index )
{
   /* TODO: Implements glClearIndex() */
}


static void clear_color( GLcontext *ctx,
                         GLubyte red, GLubyte green,
                         GLubyte blue, GLubyte alpha )
{
   /* TODO: Implements glClearColor() */
}


static void clear( GLcontext *ctx,
                   GLboolean all,
                   GLint x, GLint y, GLint width, GLint height )
{
   vga_clear();
}


static GLboolean set_buffer( GLcontext *ctx,
                             GLenum mode )
{
   /* TODO: implement double buffering and use this function to select */
   /* between front and back buffers. */
   return GL_TRUE;
}




/**********************************************************************/
/*****            Write spans of pixels                           *****/
/**********************************************************************/


static void write_index_span( GLcontext *ctx,
                              GLuint n, GLint x, GLint y,
                              const GLuint index[],
                              const GLubyte mask[] )
{
   int i;
   y = FLIP(y);
   for (i=0;i<n;i++,x++) {
      if (mask[i]) {
         vga_setcolor( index[i] );
         vga_drawpixel( x, y );
      }
   }
}



static void write_monoindex_span( GLcontext *ctx,
                                  GLuint n, GLint x, GLint y,
                                  const GLubyte mask[] )
{
   int i;
   y = FLIP(y);
   /* use current color index */
   vga_setcolor( SVGAMesa->index );
   for (i=0;i<n;i++,x++) {
      if (mask[i]) {
         vga_drawpixel( x, y );
      }
   }
}



static void write_color_span( GLcontext *ctx,
                              GLuint n, GLint x, GLint y,
                              const GLubyte red[], const GLubyte green[],
                              const GLubyte blue[], const GLubyte alpha[],
                              const GLubyte mask[] )
{
   int i;
   y=FLIP(y);
   if (mask) {
      /* draw some pixels */
      for (i=0; i<n; i++, x++) {
         if (mask[i]) {
            vga_setrgbcolor( red[i], green[i], blue[i] );
            vga_drawpixel( x, y );
         }
      }
   }
   else {
      /* draw all pixels */
      for (i=0; i<n; i++, x++) {
         vga_setrgbcolor( red[i], green[i], blue[i] );
         vga_drawpixel( x, y );
      }
   }
}



static void write_monocolor_span( GLcontext *ctx,
                                  GLuint n, GLint x, GLint y,
                                  const GLubyte mask[])
{
   int i;
   y=FLIP(y);
   /* use current rgb color */
   vga_setrgbcolor( SVGAMesa->red, SVGAMesa->green, SVGAMesa->blue );
   for (i=0; i<n; i++, x++) {
      if (mask[i]) {
         vga_drawpixel( x, y );
      }
   }
}



/**********************************************************************/
/*****                 Read spans of pixels                       *****/
/**********************************************************************/


static void read_index_span( GLcontext *ctx,
                             GLuint n, GLint x, GLint y, GLuint index[])
{
   int i;
   y = FLIP(y);
   for (i=0; i<n; i++,x++) {
      index[i] = vga_getpixel( x, y );
   }
}



static void read_color_span( GLcontext *ctx,
                             GLuint n, GLint x, GLint y,
                             GLubyte red[], GLubyte green[],
                             GLubyte blue[], GLubyte alpha[] )
{
   int i;
   for (i=0; i<n; i++, x++) {
      /* TODO */
   }
}



/**********************************************************************/
/*****                  Write arrays of pixels                    *****/
/**********************************************************************/


static void write_index_pixels( GLcontext *ctx,
                                GLuint n, const GLint x[], const GLint y[],
                                const GLuint index[], const GLubyte mask[] )
{
   int i;
   for (i=0; i<n; i++) {
      if (mask[i]) {
         vga_setcolor( index[i] );
         vga_drawpixel( x[i], FLIP(y[i]) );
      }
   }
}



static void write_monoindex_pixels( GLcontext *ctx,
                                    GLuint n,
                                    const GLint x[], const GLint y[],
                                    const GLubyte mask[] )
{
   int i;
   /* use current color index */
   vga_setcolor( SVGAMesa->index );
   for (i=0; i<n; i++) {
      if (mask[i]) {
         vga_drawpixel( x[i], FLIP(y[i]) );
      }
   }
}



static void write_color_pixels( GLcontext *ctx,
                                GLuint n, const GLint x[], const GLint y[],
                                const GLubyte r[], const GLubyte g[],
                                const GLubyte b[], const GLubyte a[],
                                const GLubyte mask[] )
{
   int i;
   for (i=0; i<n; i++) {
      if (mask[i]) {
         vga_setrgbcolor( r[i], g[i], b[i] );
         vga_drawpixel( x[i], FLIP(y[i]) );
      }
   }
}



static void write_monocolor_pixels( GLcontext *ctx,
                                    GLuint n,
                                    const GLint x[], const GLint y[],
                                    const GLubyte mask[] )
{
   int i;
   /* use current rgb color */
   vga_setrgbcolor( SVGAMesa->red, SVGAMesa->green, SVGAMesa->blue );
   for (i=0; i<n; i++) {
      if (mask[i]) {
         vga_drawpixel( x[i], FLIP(y[i]) );
      }
   }
}




/**********************************************************************/
/*****                   Read arrays of pixels                    *****/
/**********************************************************************/

/* Read an array of color index pixels. */
static void read_index_pixels( GLcontext *ctx,
                               GLuint n, const GLint x[], const GLint y[],
                               GLuint index[], const GLubyte mask[] )
{
   int i;
   for (i=0; i<n; i++,x++) {
      index[i] = vga_getpixel( x[i], FLIP(y[i]) );
   }
}



static void read_color_pixels( GLcontext *ctx,
                               GLuint n, const GLint x[], const GLint y[],
                               GLubyte red[], GLubyte green[],
                               GLubyte blue[], GLubyte alpha[],
                               const GLubyte mask[] )
{
   /* TODO */
}



static void svgamesa_setup_DD_pointers( GLcontext *ctx )
{
   /* Initialize all the pointers in the DD struct.  Do this whenever */
   /* a new context is made current or we change buffers via set_buffer! */

   ctx->Driver.UpdateState = svgamesa_setup_DD_pointers;

   ctx->Driver.ClearIndex = clear_index;
   ctx->Driver.ClearColor = clear_color;
   ctx->Driver.Clear = clear;

   ctx->Driver.Index = set_index;
   ctx->Driver.Color = set_color;

   ctx->Driver.SetBuffer = set_buffer;
   ctx->Driver.GetBufferSize = get_buffer_size;

   ctx->Driver.PointsFunc = NULL;
   ctx->Driver.LineFunc = NULL;
   ctx->Driver.TriangleFunc = NULL;

   /* Pixel/span writing functions: */
   /* TODO: use different funcs for 8, 16, 32-bit depths */
   ctx->Driver.WriteColorSpan       = write_color_span;
   ctx->Driver.WriteMonocolorSpan   = write_monocolor_span;
   ctx->Driver.WriteColorPixels     = write_color_pixels;
   ctx->Driver.WriteMonocolorPixels = write_monocolor_pixels;
   ctx->Driver.WriteIndexSpan       = write_index_span;
   ctx->Driver.WriteMonoindexSpan   = write_monoindex_span;
   ctx->Driver.WriteIndexPixels     = write_index_pixels;
   ctx->Driver.WriteMonoindexPixels = write_monoindex_pixels;

   /* Pixel/span reading functions: */
   /* TODO: use different funcs for 8, 16, 32-bit depths */
   ctx->Driver.ReadIndexSpan = read_index_span;
   ctx->Driver.ReadColorSpan = read_color_span;
   ctx->Driver.ReadIndexPixels = read_index_pixels;
   ctx->Driver.ReadColorPixels = read_color_pixels;
}



/*
 * Create a new VGA/Mesa context and return a handle to it.
 */
SVGAMesaContext SVGAMesaCreateContext( GLboolean doubleBuffer )
{
   SVGAMesaContext ctx;
   GLboolean rgb_flag;
   GLfloat redscale, greenscale, bluescale, alphascale;
   GLboolean alpha_flag = GL_FALSE;
   int colors;
   GLint index_bits;
   GLint redbits, greenbits, bluebits, alphabits;

   /* determine if we're in RGB or color index mode */
   colors = vga_getcolors();
   if (colors==32768) {
      rgb_flag = GL_TRUE;
      redscale = greenscale = bluescale = alphascale = 255.0;
      redbits = greenbits = bluebits = 8;
      alphabits = 0;
      index_bits = 0;
   }
   else if (colors==256) {
      rgb_flag = GL_FALSE;
      redscale = greenscale = bluescale = alphascale = 0.0;
      redbits = greenbits = bluebits = alphabits = 0;
      index_bits = 8;
   }
   else {
      printf(">16 bit color not implemented yet!\n");
      return NULL;
   }

   ctx = (SVGAMesaContext) calloc( 1, sizeof(struct svgamesa_context) );
   if (!ctx) {
      return NULL;
   }

   ctx->gl_vis = gl_create_visual( rgb_flag,
                                   alpha_flag,
                                   doubleBuffer,
                                   16,   /* depth_size */
                                   8,    /* stencil_size */
                                   16,   /* accum_size */
                                   index_bits,
                                   redscale,
                                   greenscale,
                                   bluescale,
                                   alphascale,
                                   redbits, greenbits,
                                   bluebits, alphabits);

   ctx->gl_ctx = gl_create_context( ctx->gl_vis,
                                    NULL,  /* share list context */
                                    (void *) ctx
                                  );

   ctx->gl_buffer = gl_create_framebuffer( ctx->gl_vis );

   ctx->index = 1;
   ctx->red = ctx->green = ctx->blue = 255;

   ctx->width = ctx->height = 0;  /* temporary until first "make-current" */

   return ctx;
}




/*
 * Destroy the given VGA/Mesa context.
 */
void SVGAMesaDestroyContext( SVGAMesaContext ctx )
{
   if (ctx) {
      gl_destroy_visual( ctx->gl_vis );
      gl_destroy_context( ctx->gl_ctx );
      gl_destroy_framebuffer( ctx->gl_buffer );
      free( ctx );
      if (ctx==SVGAMesa) {
         SVGAMesa = NULL;
      }
   }
}



/*
 * Make the specified VGA/Mesa context the current one.
 */
void SVGAMesaMakeCurrent( SVGAMesaContext ctx )
{
   SVGAMesa = ctx;
   gl_make_current( ctx->gl_ctx, ctx->gl_buffer );
   svgamesa_setup_DD_pointers( ctx->gl_ctx );

   if (ctx->width==0 || ctx->height==0) {
      /* setup initial viewport */
      ctx->width = vga_getxdim();
      ctx->height = vga_getydim();
      gl_Viewport( ctx->gl_ctx, 0, 0, ctx->width, ctx->height );
   }
}



/*
 * Return a handle to the current VGA/Mesa context.
 */
SVGAMesaContext SVGAMesaGetCurrentContext( void )
{
   return SVGAMesa;
}


/*
 * Swap front/back buffers for current context if double buffered.
 */
void SVGAMesaSwapBuffers( void )
{
   if (SVGAMesa->gl_vis->DBflag) {
      vga_flip();
   }
}


#else

/*
 * Need this to provide at least one external definition when SVGA is
 * not defined on the compiler command line.
 */

int gl_svga_dummy_function(void)
{
   return 0;
}

#endif  /*SVGA*/

