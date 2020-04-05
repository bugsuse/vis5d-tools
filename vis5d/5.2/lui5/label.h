/*                @(#)label.h        1.1 Stellar 89/02/09        */
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
 * LUI Label header
 */

#ifndef _LUI_LABEL_HEADER
#define _LUI_LABEL_HEADER 1


#define LUI_LABEL_MAX_ARGSIZE  128

#define LUI_LABEL_COL_TEXT     0
#define LUI_LABEL_COL_LEFT     2
#define LUI_LABEL_COL_TOP      3
#define LUI_LABEL_COL_RIGHT    4
#define LUI_LABEL_COL_BOTTOM   5
#define LUI_LABEL_COL_FRONT    6
#define LUI_LABEL_COL_HILITE1  7
#define LUI_LABEL_COL_HILITE2  8

typedef struct lui_label {
    char           name[64];
    Window         window;
    int            image, image_on;
    unsigned long  background_color;
    unsigned long  color[16];
    XEvent        *event;
    int            x, y;
    int            width, height;
    int            argc;
    char         **argv;
    int            argsize[LUI_LABEL_MAX_ARGSIZE];
    int            drop_shadow;
    int            border_size;
    int            visible, hiding;
    XFontStruct   *font;
    int            font_yoff;
    int         (*func)( struct lui_label * );
/*    LUI_FNCP      func;*/
    int         (*draw_func)( struct lui_label *, int, int, int, int );
/*    LUI_FNCP      draw_func;*/
   int context_index;                   /* for example, Vis5D context */
    int            text_drop_shadow;
    char          *data;
    char          *owner;
    struct lui_label *np;
} LUI_LABEL;




extern LUI_LABEL *LUI_LabelOpen( char *name, int argc,
                                        char **argv, Window parent,
                                        int x, int y, int width, int height,
                                        int border_size, LUI_FNCP func );

extern void LUI_LabelClose( char *name );

extern int LUI_LabelVisible( char *name, int on );

extern void LUI_LabelHighlight( char *name, int on );

extern void LUI_LabelHighlight2( LUI_LABEL *label, int on );

extern void LUI_LabelHighlightText( LUI_LABEL *label, int on );

extern int LUI_LabelDropShadow( char *name, int on );

extern int LUI_LabelSetDrawFunc( LUI_LABEL *label, LUI_FNCP func );

extern int LUI_LabelQuery( char *name, int *x, int *y,
                                  int *width, int *height );

extern void LUI_LabelChangeText( LUI_LABEL *label, int argc,
                                        char **argv );

extern void LUI_LabelColor( char *name, int hue );

extern LUI_LABEL *LUI_LabelLookupWindow( Window window );

extern void LUI_LabelDestroy( LUI_LABEL *bp );

#endif /*_LUI_LABEL_HEADER*/


