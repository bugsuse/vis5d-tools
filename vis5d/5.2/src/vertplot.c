/* Vis5D version 5.1 */

/*
Vis5D system for visualizing five dimensional gridded data sets
Copyright (C) 1990-1997 Bill Hibbard, Brian Paul, Dave Santek,
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
 * Sounding rendering.  . . .
 */


#include <math.h>
#include "../lui5/lui.h"
#include <stdlib.h>
#include "globals.h"
#include "graphics.h"
#include "grid.h"
#include "memory.h"
#include "proj.h"
#include "gui.h"
#include "sounding.h"
#ifdef OPENGL
#  include <GL/gl.h>
#endif
#if defined(SGI_GL) || defined(DENALI)
#  include <gl/gl.h>
#endif
#include "vis5d.h"


/*
 * Vertical Plot.  . . . . . 
 */

void make_vertplot_Ctrl_Window( int index, int width, int height)
{
   Window root = DefaultRootWindow(SndDpy);
   XSetWindowAttributes attr;
   XSizeHints sizehints;
   XGCValues values;
   GuiContext gtx = get_gui_gtx(index);
   int attr_flags;
   Window w;

   find_best_visual(SndDpy, SndScr, &SndDepth, &SndVisual, &SndColormap);
   ttr.colormap = SndColormap;
   attr.event_mask = ExposureMask | PointerMotionMask | KeyPressMask
             | ButtonPressMask | ButtonReleaseMask | StructureNotifyMask;
   attr.background_pixel = LUI_AllocateColor(.6, .6, .6);
   attr.border_pixel = 0;
   attr_flags = CWColormap | CWEventMask | CWBackPixel | CWBorderPixel;

   gtx->VertPlotCtrlWindow = XCreateWindow( SndDpy, RootWindow(SndDpy, SndScr),
                                           400, 614, 384, 400, 1, SndDepth, InputOutput,
                                           SndVisual, attr_flags, &attr);

  w = gtx->VertPlotCtrlWindow;
   sizehints.x = 50;
   sizehints.y = 50;
   sizehints.width = width;
   sizehints.height= height;
   sizehints.max_height = 5500;
   sizehints.max_width = 5500;
   sizehints.flags = USSize | USPosition;
   XSetStandardProperties( SndDpy, gtx->VertPlotCtrlWindow,
                           "Vertical Plot",
                           "Vertical Plot",
                            None, (char**)NULL, 0, &sizehints);
   XSetNormalHints(SndDpy, gtx->VertPlotCtrlWindow, &sizehints);
   XSelectInput( SndDpy, gtx->VertPlotCtrlWindow, ExposureMask
                | ButtonPressMask | ButtonReleaseMask
                | StructureNotifyMask
                | VisibilityChangeMask );
}
 


