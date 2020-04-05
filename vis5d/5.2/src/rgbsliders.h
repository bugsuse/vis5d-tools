
/* Vis5D version 5.2 */

/*
Vis5D system for visualizing five dimensional gridded data sets
Copyright (C) 1990 - 2000  Bill Hibbard, Brian Paul, Dave Santek,
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



#ifndef RGBSLIDERS_H
#define RGBSLIDERS_H


#include "gui.h"


extern void make_rgb_sliders( GuiContext gtx );


extern void hide_rgb_sliders( GuiContext gtx );


extern void show_rgb_sliders( GuiContext gtx, int graphic, int vindex, int var, int over_ride );

extern void refresh_rgb_sliders( GuiContext gtx );

extern void refresh_rgb_sliders2( GuiContext gtx );

extern int get_current_rgbsliders (GuiContext gtx, int *p_graphic, int *p_var);

#endif
