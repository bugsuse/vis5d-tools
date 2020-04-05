/* browser.h */


/*
 * File browser widget.  This widget is created in an indepedent window.
 */


#ifndef LUI_BROWSER_H
#define LUI_BROWSER_H


#include "list.h"
#include "field.h"
#include "newbutton.h"


#define MAX_ENTRIES 1000
#define MAX_CTX_ENTRIES 100

typedef struct lui_browser {
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

   int selected;                /* which list entry currently selected */

   int (*callback)( struct lui_browser *browser, char *filepath );
   int (*callback2)( struct lui_browser *browser, char filepaths[500][500] );
   int context_index;                  /* for example, Vis5D context */

   int display_number;
   int browser_number;
   int show;                           /* 0 = hide 1 = show */
} LUI_BROWSER;



extern LUI_BROWSER *LUI_BrowserCreate( int width, int height );

extern LUI_BROWSER *LUI_ContextBrowserCreate( Window win, int x, int y,
                                              int width, int height, int display );

extern void LUI_BrowserCallback( LUI_BROWSER *browser,
                                 int (*callback)() );

extern int LUI_ContextBrowserActivate( LUI_BROWSER *browser );

extern int LUI_ContextBrowserDeactivate( LUI_BROWSER *browser );

extern int LUI_BrowserActivate( LUI_BROWSER *browser, char *path );

/* MJK 12.04.98 */
/* 24Nov97  Phil McDonald */
  extern void LUI_BrowserDestroy (LUI_BROWSER *bp);
#endif

