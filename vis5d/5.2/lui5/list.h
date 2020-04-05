/* list.h */


#ifndef LUI_LIST_H
#define LUI_LIST_H


#include "scrollbar.h"

#define FONT_HEIGHT  (LUI_Font_height+0)
#define SLIDER_WIDTH 20
#define MIN2(A,B)   ( (A) < (B) ? (A) : (B) )

typedef struct lui_list {
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

   int maxwidth;         /* width of longest string in pixels */

   int (*callback)( struct lui_list *, int , int );
                         /* (un)select callback function */
   int context_index;    /* for example, Vis5D context */

   int display_number;

   void *userdata;
} LUI_LIST;



extern void LUI_ListLoad( LUI_LIST *list, int num, char **strings,
                                 int free_flag );


extern void LUI_ListUnload( LUI_LIST *list );


extern int LUI_ListGetStatus( LUI_LIST *list, int entry );


extern void LUI_ListSetStatus( LUI_LIST *list, int entry, int status );


extern void LUI_ListSetStatusAll( LUI_LIST *list, int status );


extern void LUI_ListCallback( LUI_LIST *list, int (*callback)() );


extern LUI_LIST *LUI_ListCreate( Window parent, int x, int y,
                                 int width, int height, int hscroll );


extern void LUI_ListDestroy( LUI_LIST *list );



#endif

