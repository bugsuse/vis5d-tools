/* destroy.h */


#ifndef DESTROY_H
#define DESTROY_H



extern void LUI_UnlinkWidgetFromWindow( Window window, void *widget );


extern void LUI_AddWidgetToWindow( Window window, void *widget,
                                          LUI_FNCP destroyfunc );


#endif

