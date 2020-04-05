/* main.c */



#include <stdio.h>
#include <string.h>
#include "analyze.h"
#include "file.h"
#include "grid.h"
#include "gui.h"
#include "select.h"
#include "ui.h"
#include "../src/v5d.h"


int Debug;           /* -debug */
char *path = NULL;   /* -path */


main( int argc, char *argv[] )
{
   struct grid_db *db;
   v5dstruct *v5dout;
   int i;
   int text_ui = 0;

   db = alloc_grid_db();
   v5dout = v5dNewStruct();

   Debug = 0;

   /* Read initial input files */
   if (argc>1) {
      for (i=1;i<argc;i++) {
         if (strcmp(argv[i],"-t")==0) {
            text_ui = 1;
         }
         else if (strcmp(argv[i],"-debug")==0) {
            Debug = 1;
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
      make_gui();
      gui_loop( db, v5dout );
   }

   return 0;
}

