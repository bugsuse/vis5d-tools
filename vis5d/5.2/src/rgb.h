
/* Vis5D version 5.2 */

/*
Vis5D system for visualizing five dimensional gridded data sets
Copyright (C) 1990 - 2000 Bill Hibbard, Brian Paul, Dave Santek,
and Andre Battaiola.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/


/*
 * Code to read SGI .rgb image files.  Borrowed from the SGI OpenGL TK
 * source code.
 */



#ifndef RGB_H
#define RGB_H



typedef struct _TK_RGBImageRec {
    int sizeX, sizeY;
    unsigned char *data;
} RGB_IMAGE;


extern RGB_IMAGE *ReadRGB(char *fileName);


extern void FreeRGB( RGB_IMAGE *image );


#endif

