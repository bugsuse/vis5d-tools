/* amesa.c */

/*
 * Mesa 3-D graphics library
 * Version:  1.2
 * Copyright (C) 1995  Brian Paul  (brianp@ssec.wisc.edu)
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
 * Amiga/Mesa interface.
 */


/*
$Id: amesa.c,v 1.5 1995/06/12 15:36:31 brianp Exp $

$Log: amesa.c,v $
 * Revision 1.5  1995/06/12  15:36:31  brianp
 * changed color arrays to GLubyte
 * use 8.3 filenames
 *
 * Revision 1.4  1995/05/31  19:33:50  brianp
 * removed references to MAX_VERTICES
 *
 * Revision 1.3  1995/05/22  21:02:41  brianp
 * Release 1.2
 *
 * Revision 1.2  1995/03/04  19:29:44  brianp
 * 1.1 beta revision
 *
 * Revision 1.1  1995/02/24  14:15:49  brianp
 * Initial revision
 *
 */


/* TODO: THIS INTERFACE PROBABLY DOESN'T WORK AS IT IS VERY OUT OF DATE */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <graphics/rastport.h>
#include <hardware/blit.h>
#include <clib/graphics_protos.h>
#include "bresenhm.h"
#include "context.h"
#include "dd.h"
#include "GL/amesa.h"
#include "polygons.h"
#include "xform.h"


#define MAX_POLYGON 300


struct amesa_context {
        struct gl_context *gl_ctx;	/* the main library context */

	GLboolean rgb_flag;	/* IF rgb_flag==GL_TRUE THEN
					window is on a HAM screen
					front_rp points to window's rastport
					back_rp is NULL
					rp == front_rp
					rgb_buffer is the back buffer
					pixel = current packed RGBA value
				   ELSE
					window is on a regular, CI screen
					front_rp points to window's rastport
					back_rp points to back buffer or NULL
					rp = front_rp or back_rp
					rgb_buffer = NULL
					pixel = current color index
				   ENDIF
				*/

	GLboolean db_flag;		/* double buffering flag */

	struct Window *window;		/* the Intuition window */
        struct RastPort *front_rp;	/* front rastport */
        struct RastPort *back_rp;	/* back rastport (NULL if SB or RGB) */
	struct RastPort *rp;		/* current rastport */

	GLenum drawbuffer;
	GLenum readbuffer;

	GLuint width, height;		/* drawable area */
	GLuint depth;			/* bits per pixel */

	GLuint pixel;			/* current color index or RGBA value */
	GLuint *rgb_buffer;		/* back buffer when in RGBA mode */

	GLint left, bottom;		/* offsets due to window border */
};



static AMesaContext Current = NULL;


/*
 *
 * Private functions
 *
 */


/*
 * Create a new rastport to use as a back buffer.
 * Input:  width, height - size in pixels
 *         depth - number of bitplanes
 */
static struct RastPort *make_rastport( int width, int height, int depth )
{
   struct RastPort *rp;
   struct BitMap *bm;
   int i;

   /* add error checking! */
   bm = (struct BitMap *) malloc( sizeof(struct BitMap) );
   InitBitMap( bm, depth, width, height );
   for (i=0;i<depth;i++) {
      bm->Planes[i] = AllocRaster( width, height );
   }
   rp = (struct RastPort *) malloc( sizeof(struct RastPort) );
   InitRastPort( rp );
   rp->BitMap = bm;

   return rp;

}


/*
 * Deallocate a rastport.
 */
static void destroy_rastport( struct RastPort *rp )
{
   int i, depth;

   if (rp) {
      depth = rp->BitMap->Depth;
      for (i=0;i<depth;i++) {
         FreeRaster( rp->BitMap->Planes[i], 
	 	     rp->BitMap->BytesPerRow*8,
		     rp->BitMap->Rows );
      }
      free( rp->BitMap );
      free( rp );
   }
}



/*
 * Construct a temporary raster for use by the given rasterport.
 * Temp rasters are used for polygon drawing.
 */
static void make_temp_raster( struct RastPort *rp )
{
   unsigned long width, height;
   PLANEPTR p;
   struct TmpRas *tmpras;
   struct AreaInfo *areainfo;
   UWORD *pattern;
   APTR vbuffer;

   width = rp->BitMap->BytesPerRow*8;
   height = rp->BitMap->Rows;

   /* allocate structures */
   p = AllocRaster( width, height );
   tmpras = (struct TmpRas *) malloc( sizeof(struct TmpRas) );
   areainfo = (struct AreaInfo *) malloc( sizeof(struct AreaInfo) );
   pattern = (UWORD *) malloc( sizeof(UWORD) );
   *pattern = 0xffffffff;
   vbuffer = (APTR) malloc( MAX_POLYGON * 5 * sizeof(WORD) );

   /* initialize */
   InitTmpRas( tmpras, p, ((width+15)/16)*height );
   InitArea( areainfo, vbuffer, MAX_POLYGON );

   /* bind to rastport */
   rp->AreaPtrn = pattern;
   rp->TmpRas = tmpras;
   rp->AreaInfo = areainfo;
   rp->AreaPtSz = 0;
}




/*
 * Destroy a temp raster.
 */
static void destroy_temp_raster( struct RastPort *rp )
{
   /* bitmap */
   if (rp->TmpRas) {
      FreeRaster( rp->TmpRas->RasPtr,
		  rp->BitMap->BytesPerRow*8,
		  rp->BitMap->Rows );
      free( rp->TmpRas );
      rp->TmpRas = NULL;
   }

   if (rp->AreaInfo) {
      free( rp->AreaInfo->VctrTbl );
      free( rp->AreaInfo );
      rp->AreaInfo = NULL;
   }

   if (rp->AreaPtrn) {
      free( rp->AreaPtrn );
      rp->AreaPtrn = NULL;
   }
}



/*
 * Write a pixel to a bitmap which is in HAM mode.
 */
static void ham_plot( struct BitMap *bm, int x, int y, int pixel )
{
   int p, pmask;
   int offset;
   unsigned char *ptr, mask, bit;

   offset = y * (int) bm->BytesPerRow + (x >> 3);
   bit = 128 >> (x & 7);
   mask = 0xff ^ bit;

   pmask = 1;
   for (p=0;p<6;p++) {
      ptr = bm->Planes[p] + offset;
      if (pixel & pmask) {
	 /* set bit */
	 *ptr = *ptr | bit;
      }
      else {
	 /* clear bit */
	 *ptr = *ptr & mask;
      }
      pmask = pmask << 1;
   }
}


/*
 * Write a row of pixels to a 6-plane bitmap.
 */
static void ham_row( struct BitMap *bm, int y, unsigned char pixel[] )
{
   int byte, nbytes, i, offset;

   nbytes = bm->BytesPerRow;
   offset = y * nbytes;
   i = 0;

   for (byte=0;byte<nbytes;byte++) {
      register UBYTE bitmask;
      UBYTE pixcache0, pixcache1, pixcache2, pixcache3, pixcache4, pixcache5;

      pixcache0 = pixcache1 = pixcache2 = 0;
      pixcache3 = pixcache4 = pixcache5 = 0;

      for (bitmask=128; bitmask; bitmask = bitmask >> 1) {
	 register UBYTE pixel_i = pixel[i];

	 if (pixel_i &  1) pixcache0 |= bitmask;
	 if (pixel_i &  2) pixcache1 |= bitmask;
	 if (pixel_i &  4) pixcache2 |= bitmask;
	 if (pixel_i &  8) pixcache3 |= bitmask;
	 if (pixel_i & 16) pixcache4 |= bitmask;
	 if (pixel_i & 32) pixcache5 |= bitmask;

	 i++;
      }

      bm->Planes[0][offset] = pixcache0;
      bm->Planes[1][offset] = pixcache1;
      bm->Planes[2][offset] = pixcache2;
      bm->Planes[3][offset] = pixcache3;
      bm->Planes[4][offset] = pixcache4;
      bm->Planes[5][offset] = pixcache5;

      offset++;
   }

}



/*
 * Convert and copy pixels from 32-bit RGBA format to 6-bit HAM.
 * Input:  width, height - size of buffer
 *         rgba - pointer to width*height 32-bit RGBA pixels
 *         hamrp - HAM mode rasterport
 */
static void rgba_to_ham( GLuint width, GLuint height,
			 GLuint *rgba,
			 struct RastPort *hamrp )
{
   GLuint x, y;
   GLint pr, pg, pb;
   GLint pixel, r, g, b;
   GLint dr, dg, db;
   struct BitMap *bm;
   unsigned char pixel_buf[1280];

/*   SetRast( hamrp, 0 );*/

   bm = hamrp->BitMap;

   for (y=0;y<height;y++) {
      pr = pg = pb = 0;
      for (x=0;x<width;x++) {

	 /* extract 4-bit r, g, b values */
         pixel = rgba[y*width+x];
         r = (pixel & 0x0000f0) >> 4;
	 g = (pixel & 0x00f000) >> 12;
         b = (pixel & 0xf00000) >> 20;

         dr = r-pr;  if (dr<0) dr = -dr;
	 dg = g-pg;  if (dg<0) dg = -dg;
	 db = b-pb;  if (db<0) db = -db;

	 /* modify the component which is most different from prev pixel */
	 if (dr>=dg && dr>=db) {
	    pr = r;
	    pixel_buf[x] = 32+r;
	 }
	 else if (dg>=dr && dg>=db) {
	    pg = g;
	    pixel_buf[x] = 48+g;
	 }
	 else {
	    pb = b;
	    pixel_buf[x] = 16+b;
	 }

      }
      ham_row( bm, y, pixel_buf );
   }
}



/*
 * Allocate all auxillary buffers as needed.  This function is used
 * for initial setup and when window resizes occur.
 */
static void alloc_aux_buffers( AMesaContext c )
{
   if (c->rgb_flag) {
      /* color buffer */

      if (c->rgb_buffer) {
	 free( c->rgb_buffer);
      }
      c->rgb_buffer = (GLuint *)
	                malloc( c->width * c->height * sizeof(GLuint) );
      if (!c->rgb_buffer) {
	 /* unable to open true-color buffer */
	 printf("Not enough memory for RGB buffer\n");
      }
      c->rp = c->front_rp;
   }
   else {
      /* index buffer */

      if (c->db_flag) {
         /* deallocate current back buffer, if any */
	 if (c->back_rp) {
	    destroy_temp_raster( c->back_rp );
	    destroy_rastport( c->back_rp );
	 }
	 /* allocate new back buffer */
         c->back_rp = make_rastport( c->window->Width, c->window->Height,
				     c->depth );
	 if (c->back_rp) {
	    make_temp_raster( c->back_rp );
	    c->rp = c->back_rp;
	 }
	 else {
	    c->rp = c->front_rp;
	 }
      }
      else {
	 c->rp = c->front_rp;
      }
   }

   /* deallocte front temp raster, if any */
   destroy_temp_raster( c->front_rp);
   /* allocate new front temp raster */
   make_temp_raster( c->front_rp );
}



/*
 *
 * Public functions
 *
 */



/*
 * Given an Intuition window pointer, create a new AMesaContext
 * Notes:  If rgb_flag is true, db_flag must also be true.
 *
 * Input:  window - Intuition window to render into.
 *         db_flag - GL_TRUE = double buffered, GL_FALSE = single buffered
 *         rgb_flag - GL_TRUE = RGB window, GL_FALSE = Color Index window
 * Return:  a AMesaContext or NULL if error.
 */
AMesaContext AMesaCreateContext( struct Window *window,
				 GLboolean db_flag,
				 GLboolean rgb_flag )
{
   AMesaContext c;

   /* allocate amesa_struct */
   c = (AMesaContext) malloc( sizeof(struct amesa_context) );
   if (!c) {
      return NULL;
   }

   /* initialize the context */
   c->gl_ctx = gl_new_context();
   c->rgb_flag = rgb_flag;
   c->db_flag = db_flag;
   c->window = window;
   c->front_rp = window->RPort;
   c->back_rp = NULL;
   c->rp = NULL;
   c->width = window->Width - window->BorderLeft - window->BorderRight;
   c->height = window->Height - window->BorderTop - window->BorderBottom;
   c->left = window->BorderLeft;
   c->bottom = window->Height - window->BorderBottom - 1;
   c->rgb_buffer = NULL;

   if (rgb_flag) {
      /* RGBA color buffers */
      c->gl_ctx->RGBAflag = GL_TRUE;
      c->depth = 32;
      alloc_aux_buffers( c );
      c->pixel = 0;
   }
   else {
      /* CI color buffers */
      c->gl_ctx->RGBAflag = GL_FALSE;
      c->depth = window->RPort->BitMap->Depth;
      alloc_aux_buffers( c );
      c->pixel = 1;
   }

   if (db_flag) {
      c->gl_ctx->Color.DrawBuffer = GL_BACK;
   }
   else {
      c->gl_ctx->Color.DrawBuffer = GL_FRONT;
   }


/*
   printf("win width = %d\n", (int) window->Width );
   printf("win height = %d\n", (int) window->Height );
   printf("left = %d\n", (int) window->BorderLeft );
   printf("rightt = %d\n", (int) window->BorderRight );
   printf("top = %d\n", (int) window->BorderTop );
   printf("bottom = %d\n", (int) window->BorderBottom );
*/

   c->gl_ctx->BufferWidth = c->width;
   c->gl_ctx->BufferHeight = c->height;

   return (AMesaContext) c;
}



void AMesaDestroyContext( AMesaContext c )
{
   gl_destroy_context( c->gl_ctx );
   if (c->rgb_flag) {
      free( c->rgb_buffer );
   }
   else {
      if (c->back_rp) {
         destroy_temp_raster( c->back_rp );
         destroy_rastport( c->back_rp );
      }
   }
   destroy_temp_raster( c->front_rp );

   free( (void *) c );
}



int AMesaMakeCurrent( AMesaContext ctx )
{
   AMesaContextc = ctx;

   gl_set_context( ctx->gl_ctx );
   Current = ctx;
   if (Current->gl_ctx->Viewport.Width==0) {
      /* initialize viewport to window size */
      gl_viewport( 0, 0, Current->width, Current->height );
   }

}



AMesaContext AMesaGetCurrentContext( void )
{
   return Current;
}



void AMesaSwapBuffers( void )
{
   if (Current->rgb_flag) {
      rgba_to_ham( Current->width, Current->height,
		   Current->rgb_buffer,
		   Current->front_rp );
   }
   else {
      if (Current->back_rp) {
         unsigned long minterm = 0xc0;
	 int x = Current->window->BorderLeft;
	 int y = Current->window->BorderTop;
         ClipBlit( Current->back_rp, x, y,   /* from */
	           Current->front_rp, x, y,  /* to */
		   Current->width, Current->height,  /* size */
		   minterm );
      }
   }
}


/*
 *
 * Device driver functions.
 *
 */



/****************************************/
/*           Miscellaneous              */
/****************************************/


/*
 * Finish any pending rendering operations, then return.
 */
void dd_flush( void )
{
   if (Current->rgb_flag && !Current->db_flag) {
      /* copy RGB buffer to ham window */
      rgba_to_ham( Current->width, Current->height,
		   Current->rgb_buffer,
		   Current->front_rp );
   }

}



/*
 * Return information about current color buffer.
 * Output:  mode - 0 = CI, 1 = RGBA
 *          depth - In Color Index mode, return bits/pixel
 *                - In RGBA mode, return bits/component
 */
void dd_buffer_info( GLuint *width, GLuint *height,
		     GLuint *mode, GLuint *depth )
{
   struct Window *w;
   GLint new_width, new_height;

   /* get new window dimensions */
   w = Current->window;
   new_width  = w->Width  - w->BorderLeft - w->BorderRight;
   new_height = w->Height - w->BorderTop  - w->BorderBottom;

   if (new_width!=Current->width || new_height!=Current->height) {
      Current->width = new_width;
      Current->height = new_height;
      Current->left = w->BorderLeft;
      Current->bottom = w->Height - w->BorderBottom - 1;
      alloc_aux_buffers( Current );
   }

   *width = Current->width;
   *height = Current->height;

   if (Current->rgb_flag) {
      *mode = 1;
      *depth = 8;
   }
   else {
      *mode = 0;
      *depth = Current->depth;
   }
}



GLenum dd_read_buffer( GLenum mode )
{
   /*return Current->readbuffer;*/
   return mode;
}



GLenum dd_draw_buffer( GLenum mode )
{
   /*return Current->drawbuffer;*/
   return mode;
}



/****************************************/
/*       Simple Render Functions        */
/****************************************/

/*
 * Set current color index.
 */
void dd_index( GLuint index )
{
   Current->pixel = index;
   SetAPen( Current->rp, (unsigned long) index );
}


/* Set the index mode bitplane mask. */
void dd_index_mask( GLuint mask )
{
   Current->rp->Mask = (UBYTE) mask;
}


/*
 * Set current RGBA color.
 */
void dd_color( const GLfloat color[4] )
{
   GLuint ir, ig, ib, ia;

   ir = (GLuint) (color[0] * 255.0);
   ig = (GLuint) (color[1] * 255.0);
   ib = (GLuint) (color[2] * 255.0);
   ia = (GLuint) (color[3] * 255.0);

   Current->pixel = (ia << 24) | (ib << 16) | (ig << 8) | ir;
}


void dd_color_mask( GLboolean rmask, GLboolean gmask,
		    GLboolean bmask, GLboolean amask )
{
   /* TODO */
}



/*
 * Write a pixel of the current color.
 */
void dd_draw_pixel( GLint x, GLint y )
{
   y = Current->bottom - y;
   x = Current->left + x;

   if (Current->rgb_flag) {
      Current->rgb_buffer[ y * Current->width + x ] = Current->pixel;
   }
   else {
      WritePixel( Current->rp, (long) x, (long) y );
   }
}



/*
 * Draw a line segment using current color.
 */
void dd_draw_line( GLint x0, GLint y0, GLint x1, GLint y1 )
{
   x0 = Current->left + x0;
   y0 = Current->bottom - y0;
   x1 = Current->left + x1;
   y1 = Current->bottom - y1;

   if (Current->rgb_flag) {
      /* use bresenham's alg. to draw line using Current->pixel */
#define BRESENHAM_PLOT( X, Y )		\
		Current->rgb_buffer[Y*Current->width+X] = Current->pixel;
      BRESENHAM( x0, y0, x1, y1 );
   }
   else {
      Move( Current->rp, (long) x0, (long) y0 );
      Draw( Current->rp, (long) x1, (long) y1 );
   }
}



/*
 * Draw a filled polygon of a single color.  If there is hardware/OS support
 * for polygon drawing use that here.  Otherwise, call a function in
 * polygon.c to do the drawing.
 */
void dd_draw_polygon( GLuint n, GLint x[], GLint y[] )
{
   if (Current->rgb_flag) {
      gl_polygon( n, x, y );
   }
   else {
      GLuint i;
      AreaMove( Current->rp, (long) (Current->left + x[0]),
			     (long) (Current->bottom - y[0]) );
      for (i=1;i<n;i++) {
         AreaDraw( Current->rp, (long) (Current->left + x[i]),
				(long) (Current->bottom - y[i]) );
      }
      AreaEnd( Current->rp );
   }
}



/*
 * Fill a rectangular window region with the current color.
 * Input:  x, y - pixel coordinate of lower-left corner of rectagle
 *         width, height - size of rectangle to fill.
 */
void dd_draw_rectangle( GLint x, GLint y, GLint width, GLint height )
{
   if (Current->rgb_flag) {
      GLint i, j;
      GLint y0, y1;
      y0 = Current->bottom - y - height + 1;
      y1 = Current->bottom - y;
      for (j=y0;j<=y1;j++) {
         register GLuint *p = Current->rgb_buffer + j * Current->width + x;
         for (i=0;i<width;i++) {
	    *p++ = Current->pixel;
         }
      }
   }
   else {
      UBYTE mask_save;
      mask_save = Current->rp->Mask;
      Current->rp->Mask = 0xff;
      RectFill( Current->rp,
	        Current->left+x,         Current->bottom-y-height+1,
	        Current->left+x+width-1, Current->bottom-y );
      Current->rp->Mask = mask_save;
   }
}



/****************************************/
/*        Span Rendering Functions      */
/****************************************/


/* Write a horizontal span of color-index pixels with a boolean mask. */
void dd_write_index_span( GLuint n, GLint x, GLint y,
			  const GLuint index[], const GLubyte mask[] )
{
   GLuint i;

   x = Current->left + x;
   y = Current->bottom - y;

   for (i=0;i<n;i++,x++) {
      if (mask[i]) {
	 SetAPen( Current->rp, (unsigned long) index[i] );
	 WritePixel( Current->rp, (long) x, (long) y );
      }
   }
}



void dd_write_monoindex_span( GLuint n, GLint x, GLint y,
			      GLuint index, const GLubyte mask[] )
{
   GLuint i;

   x = Current->left + x;
   y = Current->bottom - y;

   SetAPen( Current->rp, (unsigned long) index );

   for (i=0;i<n;i++,x++) {
      if (mask[i]) {
	 WritePixel( Current->rp, (long) x, (long) y );
      }
   }
}



/* Read a horizontal span of color-index pixels. */
void dd_read_index_span( GLuint n, GLint x, GLint y, GLuint index[] )
{
   GLuint i;

   x = Current->left + x;
   y = Current->bottom - y;

   for (i=0;i<n;i++,x++) {
      index[i] = (GLuint) ReadPixel( Current->rp, (long) x, (long) y );
   }
}



/*
 * Write a span of RGBA pixels with a mask.
 */
void dd_write_color_span( GLuint n, GLint x, GLint y,
			  const GLubyte red[], const GLubyte green[],
			  const GLubyte blue[], const GLubyte alpha[],
			  const GLubyte mask[] )
{
   GLuint i, *ptr;

   x = Current->left + x;
   y = Current->bottom - y;

   ptr = Current->rgb_buffer + y * Current->width + x;
   for (i=0;i<n;i++) {
      if (mask[i]) {
         *ptr = (alpha[i] << 24) | (blue[i] << 16) | (green[i] << 8) | red[i];
      }
      ptr++;
   }
}



void dd_write_monocolor_span( GLuint n, GLint x, GLint y,
			      GLubyte red, GLubyte green,
			      GLubyte blue, GLubyte alpha,
			      const GLubyte mask[] )
{
   GLuint i, *ptr, p;

   x = Current->left + x;
   y = Current->bottom - y;

   p = (alpha << 24) | (blue << 16) | (green << 8) | red;

   ptr = Current->rgb_buffer + y * Current->width + x;
   for (i=0;i<n;i++) {
      if (mask[i]) {
	 *ptr = p;
      }
      ptr++;
   }
}



/* Read a horizontal span of color pixels. */
void dd_read_color_span( GLuint n, GLint x, GLint y,
			 GLubyte red[], GLubyte green[],
		         GLubyte blue[], GLubyte alpha[] )
{
   GLuint i, *ptr, p;
   GLint d255 = 1.0 / 255.0;

   x = Current->left + x;
   y = Current->bottom - y;

   ptr = Current->rgb_buffer + y * Current->width + x;
   for (i=0;i<n;i++) {
      p = *ptr++;
      red[i]   =   p      & 0xff;
      green[i] = ((p>>8)  & 0xff) << 8;
      blue[i]  = ((p>>16) & 0xff) << 16;
      alpha[i] = ((p>>24) & 0xff) << 24;
   }
}

