/* scrollbar.h */


#ifndef LUI_SCROLLBAR_H
#define LUI_SCROLLBAR_H




typedef struct lui_scrollbar {
   Window window;        /* The X window ID */
   int x, y;             /* postion of window with respect to parent */
   int width, height;    /* interior size of window */
   int border;           /* border width */

   int orientation;      /* 1=vertical, 0 = horizontal */
   float size;           /* size of scroll bar as percent of container size */
   float position;       /* position of scroll bar as a percent */

   int startx, starty;   /* position of mouse when button first pressed */
   float startpos;       /* position value when mouse button first pressed */
   int delta;            /* pixels between top of know and mouse pos """ */
   int dragging;         /* drag flag */

   int (*callback)( struct lui_scrollbar *, float );
                         /* callback function when scrollbar knob is moved */
   int context_index;    /* for example, Vis5D context */

   void *userdata;       /* pointer to user data */
} LUI_SCROLLBAR;





extern LUI_SCROLLBAR *LUI_ScrollBarCreate( Window parent,
                int x, int y, int width, int height, int orientation );


extern void LUI_ScrollBarSet( LUI_SCROLLBAR *scrollbar, float size,
                                     float position );


extern void LUI_ScrollBarSetPos( LUI_SCROLLBAR *scrollbar, float position );


extern float LUI_ScrollBarGetPos( LUI_SCROLLBAR *scrollbar );


extern void LUI_ScrollBarCallback( LUI_SCROLLBAR *scrollbar,
                                  int (* callback)( LUI_SCROLLBAR *, float ) );


extern void LUI_ScrollBarData( LUI_SCROLLBAR *scrollbar, void *data );


extern void LUI_ScrollBarDestroy( LUI_SCROLLBAR *sb );


extern void LUI_ScrollBarResize( LUI_SCROLLBAR *sb, int width, int height );


#endif


