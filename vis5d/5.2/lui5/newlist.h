/* list.h */


#ifndef LUI_NEWLIST_H
#define LUI_NEWLIST_H


#include "scrollbar.h"


typedef struct lui_newlist {
   Window window;        /* main window ID */
   int x, y;             /* position of list widget w.r.t. parent */
   int width, height;    /* size of main window */

   LUI_SCROLLBAR *vsb;   /* the vertical scrollbar */
   LUI_SCROLLBAR *hsb;   /* the horizontal scrollbar */

   int free_flag;        /* free() strings when finished? */
   char **strings;       /* pointer to array of pointers to strings */
   int *select_flags;    /* array of "is selected" flags */
   int numstrings;       /* number of strings in above arrays */

   int rows;             /* how many rows of text are viewable */
   int columns;          /* how many columns (pixels) are viewable */
   int topstring;        /* which string is at top of window */
   int leftoffset;       /* horizontal scroll offset in pixels */

   int button_down;

   int maxwidth;         /* width of longest string in pixels */

   int (*callback)( struct lui_newlist *, int , int );
                         /* (un)select callback function */
   int context_index;    /* for example, Vis5D context */

   int display_number;

   void *userdata;
} LUI_NEWLIST;



extern void LUI_NEWListLoad( LUI_NEWLIST *list, int num, char **strings,
                                 int free_flag );


extern void LUI_NEWListUnload( LUI_NEWLIST *list );


extern int LUI_NEWListGetStatus( LUI_NEWLIST *list, int entry );


extern void LUI_NEWListSetStatus( LUI_NEWLIST *list, int entry, int status );


extern void LUI_NEWListSetStatusAll( LUI_NEWLIST *list, int status );


extern void LUI_NEWListCallback( LUI_NEWLIST *list, int (*callback)() );


extern LUI_NEWLIST *LUI_NEWListCreate( Window parent, int x, int y,
                                 int width, int height, int hscroll );


extern void LUI_NEWListDestroy( LUI_NEWLIST *list );



#endif

