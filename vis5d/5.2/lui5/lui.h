/*                @(#)lui.h        1.3 Stellar 89/03/22        */
/*
                        Copyright 1989 by
                        Stellar Computer Inc.
                        All Rights Reserved
        
        This software comprises unpublished confidential information of
        Stellar Computer Inc. and may not be used, copied or made
        available to anyone, except in accordance with the license
        under which it is furnished.
*/
/*
 * LUI header
 */

#ifndef _LUI_HEADER
#define _LUI_HEADER 1

/* X include files */
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>

typedef int ((*LUI_FNCP)( /*...*/ ));


/* Widget include files */
#include "browser.h"
#include "newbrowser.h"
#include "button.h"
#include "buttonmatrix.h"
#include "colorbar.h"
#include "destroy.h"
/*#include "dial.h"*/
#include "dialog.h"
#include "field.h"
#include "label.h"
#include "layout.h"
#include "list.h"
#include "newbutton.h"
#include "newlabel.h"
#include "newslider.h"
#include "popup.h"
#include "radio.h"
#include "scrollbar.h"




#define LUI_FONT  "8x13"
#define LUI_MAIN_CURSOR     0  /* Default Cursor */
#define LUI_DIAL_CURSOR     1
#define LUI_DIAL_CW_CURSOR  2
#define LUI_DIAL_CCW_CURSOR 3
#define LUI_DIAL_RES_CURSOR 4
#define LUI_QUESTION_CURSOR 5
#define LUI_DOWN_CURSOR     6
#define LUI_UP_CURSOR       7
#define LUI_UP_DOWN_CURSOR  8
#define LUI_CLOCK_CURSOR    9
#define LUI_PENCIL_CURSOR  10
#define LUI_ARROW_HAND_CURSOR  11

/* For degrees <--> radians conversion */
#define DERAD(x) ((x) * 0.017453292519943295)
#define RADDE(x) ((x) * 57.29577951308232300)

#define LUI_POPUP_MENU_SIZE(M) (sizeof(M) / 4)


extern Display *LUI_Display;
extern Window LUI_RootWindow;
extern Visual *LUI_Visual;
extern XFontStruct *LUI_Font;
extern Colormap LUI_Colormap;

extern unsigned long
  LUI_Color_black,
  LUI_Color_md_grey,
  LUI_Color_white,
  LUI_Color_red,
  LUI_Color_green,
  LUI_Color_blue,
  LUI_Color_fore,
  LUI_Color_xor,
  LUI_Color_gray,
  LUI_Color_highgray,
  LUI_Color_darkgray;

extern GC
  LUI_Gc,
  LUI_GC_black,
  LUI_GC_md_grey,
  LUI_GC_white,
  LUI_GC_red,
  LUI_GC_green,
  LUI_GC_blue,
  LUI_GC_fore,
  LUI_GC_xor,
  LUI_GC_gray,
  LUI_GC_highgray,
  LUI_GC_darkgray,
  LUI_GC_top,              /* for top and left of 3-D highlight */
  LUI_GC_bottom;           /* for bottom and right of 3-D highlight */

extern int
  LUI_Debug,
  LUI_Screen,
  LUI_Width,
  LUI_Height,
  LUI_Depth,
  LUI_Font_yoff,
  LUI_Font_height,
  LUI_Font_width;

extern int LUI_Border;

extern int
  LUI_DefaultHue,
  LUI_DefaultHue2,
  LUI_DefaultHue3;

extern int context_index;

extern char LUI_ProgramName[];
extern Cursor LUI_CursorTable[];



#ifdef ultrix
  extern char *strdup();
#endif

extern void LUI_Initialize( char *program_name, Display *display,
                                   Visual *visual, int depth,
                                   Colormap colormap );

extern void LUI_BorderWidth( int width );
extern void LUI_FrameWidth( int width );

extern void LUI_SetMainWindow( Window w );

extern Window LUI_CreateWindowAt( Window parent, int x, int y,
                                         int xs, int ys );

extern Window LUI_CreateSndWindowAt( Window parent, int x, int y,
                                         int xs, int ys );

extern Window LUI_CreateWindow( Window parent, int xs, int ys );

extern void LUI_DestroyWindow( Window window );

extern unsigned long LUI_AllocateColorInt( int r, int g, int b );

extern unsigned long LUI_AllocateColor( float red, float green, float blue );

extern unsigned long LUI_MakeColor( int r, int g, int b );

extern void LUI_FreeColor( unsigned long pixel );

extern void LUI_RgbShade( unsigned long color, float shade,
                                 int *r, int *g, int *b );

extern void LUI_SetColor( unsigned long color );

extern Status LUI_XAllocColor( Display *dpy, Colormap cmap,
                                      int cmap_size, XColor *color );

extern void LUI_FrameWindow( Window window, int width, int height,
                                    int border_size );

extern void LUI_FrameWindow2( Window window, int width, int height,
                                     int border_size );

extern void LUI_DrawFrame( Window window, int x, int y,
                                  int width, int height,
                                  int thickness, int raised );

extern void LUI_MoveResizeWindow( Window window,
                                         int x, int y, int xs, int ys );

extern void LUI_ResizeWindow( Window window, int xs, int ys );

extern void LUI_Cursor( Window window, int cursor );

extern void LUI_EventAdd( Window window, unsigned long mask,
                                 LUI_FNCP func );

extern void LUI_EventAdd2( Window window, unsigned long mask,
                                  LUI_FNCP func, void *widget );

extern void LUI_EventProcess( void );

extern int LUI_EventDispatch( XEvent *event );

extern void LUI_EventRemove( Window window );

extern void delete_event( Window window );

extern void LUI_ContextIndex( int index);

extern void LUI_DestroyWidgetsInWindow( Window window );

extern char **LUI_MakeLabel( char *string );

extern void LUI_EventLock( Window window );


#endif  /*_LUI_HEADER*/


