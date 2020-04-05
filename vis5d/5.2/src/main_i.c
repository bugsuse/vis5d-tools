/* main.c */
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



#include <stdio.h>
#include <string.h>
#include <X11/Xlib.h>
#include <X11/keysym.h>
#include "analyze_i.h"
#include "file_i.h"
#include "grid_i.h"
#include "gui_i.h"
#include "select_i.h"
#include "ui_i.h"
#include "../src/v5d.h"


int Debug_i;           /* -debug */
char *path = NULL;   /* -path */


int main_irun( Display *guidpy, int argc, char *argv[] )
{
   struct grid_db *db;
   v5dstruct *v5dout;
   int i;
   int text_ui = 0;

   db = alloc_grid_db();
   v5dout = v5dNewStruct();

   Debug_i = 0;

   /* Read initial input files */
   if (argc>1) {
      for (i=1;i<argc;i++) {
         if (strcmp(argv[i],"-t")==0) {
            text_ui = 1;
         }
         else if (strcmp(argv[i],"-debug")==0) {
            Debug_i = 1;
         }
         else if (strcmp(argv[i],"-path")==0) {
            path = argv[i+1];
            i++;
         }
         else {
            get_file_info( argv[i], db );
         }
      }
   }


   analyze_grids( db );
   select_all( db, ALL_BITS, 1 );

   setup_defaults( db, v5dout, 1, 1, 1 );

   if (text_ui) {
      /* Text-based user interface */
      ui_loop( db, v5dout );
   }
   else {
      /* Graphical user interface */
      make_gui_i(guidpy);
      gui_loop( db, v5dout );
   }

   return 0;
}

