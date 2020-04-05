/* pixmaps.h */



/* This module replaces the runtime pixmaps required by LUI with a
   compile-time equivalent.

   Written by:  Brian Paul  on 3-15-91
*/




#include "X11/Xlib.h"


extern Pixmap LUI_PixmapOptionIn, LUI_PixmapOptionOut,
              LUI_PixmapRadioIn, LUI_PixmapRadioOut;




extern void LUI_InitButtonPixmaps( void );



