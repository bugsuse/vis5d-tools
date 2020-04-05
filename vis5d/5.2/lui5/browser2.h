/* browser.h */


/*
 * File browser widget.  This widget is created in an indepedent window.
 */


#ifndef LUI_NEWBROWSER_H
#define LUI_NEWBROWSER_H


#include "newlist.h"
#include "list.h"
#include "field.h"
#include "newbutton.h"

#define MAX_NUM_SELECTED 1000
typedef struct lui_newbrowser {
   int window;

   int width, height;

   char current[2000];                 /* Current directory path */

   LUI_LIST *list;                     /* List of files to click on */
   LUI_FIELD *field;                   /* Current path/filename */
   LUI_NEWBUTTON *parent_button;       /* The "Parent" button */
   LUI_NEWBUTTON *root_button;         /* The "Root" button */
   LUI_NEWBUTTON *home_button;         /* The "Home" button */
   LUI_NEWBUTTON *cancel_button;       /* The "Cancel" button */
   LUI_NEWBUTTON *ok_button;           /* The "OK" button */

   int selected[MAX_NUM_SELECTED];                /* which list entry currently selected */
   int cur_selected;
   int num_selected;

   int (*callback)( struct lui_browser *browser, char filepaths[500][500] );
   int context_index;                  /* for example, Vis5D context */

   int display_number;
   int browser_number;
   int show;                           /* 0 = hide 1 = show */
} LUI_NEWBROWSER;



extern LUI_NEWBROWSER *LUI_NEWBrowserCreate( int width, int height );

extern LUI_NEWBROWSER *LUI_NEWContextBrowserCreate( Window win, int x, int y,
                                              int width, int height, int display );

extern void LUI_NEWBrowserCallback( LUI_NEWBROWSER *browser,
                                 int (*callback)() );

extern int LUI_NEWContextBrowserActivate( LUI_NEWBROWSER *browser );

extern int LUI_NEWContextBrowserDeactivate( LUI_NEWBROWSER *browser );

extern int LUI_NEWBrowserActivate( LUI_NEWBROWSER *browser, char *path );

/* MJK 12.04.98 */
/* 24Nov97  Phil McDonald */
  extern void LUI_NEWBrowserDestroy (LUI_NEWBROWSER *bp);
#endif

