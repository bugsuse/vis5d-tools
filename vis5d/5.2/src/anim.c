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

/* anim.c */


/*
 * Stored frame animation.
 */

#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "globals.h"
#include "graphics.h"
#ifdef OPENGL
#  include <GL/gl.h>
#endif
#ifdef SGI_GL
#include <gl/gl.h>
#endif
#ifdef DENALI
#include <gl/gl.h>
#endif
#ifdef MESA
  extern XImage *mesa_read_image( void );
  extern void mesa_draw_image( XImage *img );
#endif



/*
 * Call this once during initialization.
 */
void init_anim( Display_Context dtx )
{
   int i;

   for (i=0;i<MAXTIMES;i++) {
      dtx->cache[i] = NULL;
   }
   dtx->all_invalid = 1;
}




/*
 * Invalidate all the frames in the cache.  This should be called when
 * the viewpoint changes, the selected graphics changes, or the window
 * size changes.
 */
void invalidate_frames( Display_Context dtx )
{
   int i;

   if (!dtx->all_invalid) {

      for (i=0;i<MAXTIMES;i++) {
         if (dtx->cache[i]) {
            XDestroyImage( dtx->cache[i] );
            dtx->cache[i] = NULL;
         }
      }
      dtx->all_invalid = 1;
   }
}


/*
 * This function is called after a frame has been rendered.  If the
 * cached frame is invalid we'll copy the newly rendered frame into the
 * cache.
 * Input:  timestep - a timestep in [0,NumTimes-1]
 * Return:  1 = the cache was loaded with a new image
 *          0 = the cache was unchanged
 */
int save_frame( Display_Context dtx, int timestep )
{
   if (!dtx->cache[timestep]) {
      printf("saving frame %d\n", timestep );
#ifdef MESA
      dtx->cache[timestep] = mesa_read_image();
#endif
      dtx->all_invalid = 0;
      return 1;
   }
   else {
      return 0;
   }
}


/*
 * This function returns a frame from the cache and displays it.
 * Input:  timestep - a timestep in [0,NumTimes-1]
 * Return:  1 = there was a cached frame and it's now displayed
 *          0 = there was no cached frame to display, we must render it
 */
int get_frame( Display_Context dtx, int timestep )
{
   if (dtx->cache[timestep]) {
      printf("drawing cached frame %d...\n", timestep );
#ifdef MESA
      mesa_draw_image( dtx->cache[timestep] );
#endif
      return 1;
   }
   else {
      return 0;
   }
}

