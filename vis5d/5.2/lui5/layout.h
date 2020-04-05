/* layout.h */


#ifndef LUI_LAYOUT_H
#define LUI_LAYOUT_H



#define LUI_NEXT_X  -1000
#define LUI_NEXT_Y  -1001
#define LUI_SAME_X  -1002
#define LUI_SAME_Y  -1003
#define LUI_SAME_W  -1004
#define LUI_SAME_H  -1005
#define LUI_LEFT    -1006
#define LUI_TOP     -1007



extern void LUI_LayoutGutter( int g );


extern int LUI_LayoutGetGutter( void );


extern int LUI_LayoutGet( int param );


extern void LUI_LayoutCheck( int *x, int *y, int *width, int *height );


#endif

