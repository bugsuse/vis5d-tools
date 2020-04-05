/*                @(#)button.h        1.2 Stellar 89/03/22        */
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
 * LUI Button header
 */

#ifndef _LUI_BUTTON_HEADER
#define _LUI_BUTTON_HEADER 1



#define LUI_BUTTON_YS             30  /*32 default*/
#define LUI_BUTTON_SMALL_XS       90
#define LUI_BUTTON_LARGE_XS       180
#define LUI_BUTTON_PIXMAP_SM_IN   "LUI_Button.PixmapSmallIn"
#define LUI_BUTTON_PIXMAP_SM_OUT  "LUI_Button.PixmapSmallOut"
#define LUI_BUTTON_PIXMAP_LG_IN   "LUI_Button.PixmapLargeIn"
#define LUI_BUTTON_PIXMAP_LG_OUT  "LUI_Button.PixmapLargeOut"

#define LUI_BUTTON_RADIO_PIXMAP_IN  "LUI_Button.Radio.In"
#define LUI_BUTTON_RADIO_PIXMAP_OUT "LUI_Button.Radio.Out"

#define LUI_BUTTON_OPTION_PIXMAP_IN "LUI_Button.Option.In"
#define LUI_BUTTON_OPTION_PIXMAP_OUT "LUI_Button.Option.Out"

#define LUI_BUTTON_SMALL       0
#define LUI_BUTTON_LARGE       1

#define LUI_BUTTON_OUT         0
#define LUI_BUTTON_IN          1

#define LUI_BUTTON_CONFIG     -1   /* Application changes label and redraws   */
#define LUI_BUTTON_TOGGLE      0   /* Toggle switch: on/off/on, etc           */
#define LUI_BUTTON_FUNCTION    1   /* Stays on until another function picked  */
#define LUI_BUTTON_ONESHOT     2   /* Immediately pops out after selecting    */
#define LUI_BUTTON_NODRAW      3   /* Button not drawn, state doesn't change  */

#define LUI_BUTTON_RADIO      10
#define LUI_BUTTON_OPTION     11

typedef struct lui_button {
    char           label[64];
    int            group;
    int            state;
    int            active;
    Window         window;
    int            type;
    int            x, y;
    int            width, height;
    int            index;
    int            indexowner;
    int            mousebutton;
    unsigned long  hicolor; /* BEP 2-13-92 */
    int            (*func)( struct lui_button *btn );
    int context_index;                   /* for example, Vis5D context */
    Pixmap         pixmap_out, pixmap_in;
    XEvent        *event;
    struct lui_button_pad *button_pad;
    struct lui_button *np;
} LUI_BUTTON;

typedef struct lui_button_pad {
    char           name[64];
    Window         window;
    LUI_BUTTON    *button_head;
    int            type;
    int            x, y;
    int            width, height;
    int            border_size;
    int            visible, hiding;
    Pixmap         pixmap_out, pixmap_in;
    int            hue;
    unsigned long  background_color;
    struct lui_button_pad *np;
} LUI_BUTTON_PAD;



extern LUI_BUTTON_PAD *LUI_ButtonPadOpen( char *name, Window parent,
                                            int x, int y, int border_size,
                                            unsigned long background_color );

extern LUI_BUTTON_PAD *LUI_ButtonPadOpenType( char *name, Window parent,
                                       int type, int x, int y, int border_size,
                                       unsigned long background_color );

extern LUI_BUTTON *LUI_ButtonCreate( char *name, int type,
                                            int x, int y, int size,
                                            int index, LUI_FNCP func );

extern LUI_BUTTON *LUI_ButtonCreateType( char *name, int x, int y,
                                      int size, int index, LUI_FNCP func );

extern void LUI_ButtonPadClose( char *name );

extern void LUI_ButtonPadVisible( char *name, int on_off );

extern void LUI_ButtonPadQuery( char *name,
                                       int *x, int *y,
                                       int *width, int *height );

extern int LUI_ButtonSetColor( LUI_BUTTON *button,
                                      int r, int g, int b );


extern int LUI_ButtonState( LUI_BUTTON *button, int state );

extern int LUI_SetButtonState( LUI_BUTTON *button, int state );

extern void LUI_ButtonDestroy( LUI_BUTTON *bp );

extern void LUI_ButtonPadDestroy( LUI_BUTTON_PAD *bp );

#endif /*_LUI_BUTTON_HEADER*/

