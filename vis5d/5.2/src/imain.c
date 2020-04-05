/* imain.c */
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
#include <stdlib.h>
#include <string.h>
#include <X11/Xlib.h>
#include <X11/keysym.h>
#include "igui.h"
#include "../lui5/lui.h"

char *ipath = NULL;   /* -path */

int run_ireg_importer( Display *guidpy, int argc, char *argv[] )
{
   FileDB fdb;
   int index;
   static int do_once = 1;

   if (do_once){
      fdb_initialize();
      do_once = 0;
   }

   fdb = make_new_fdb();
   imake_gui(guidpy);
   igui_loop(fdb);

   return 0;

}

