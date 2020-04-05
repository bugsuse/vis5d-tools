/* colorbar.h */

/* Vis5D version 4.1 */

/*
Vis5D system for visualizing five dimensional gridded data sets
Copyright (C) 1990 - 1995  Bill Hibbard, Brian Paul, Dave Santek,
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


#ifndef LUI_COLORBAR_H
#define LUI_COLORBAR_H



/* Result codes for callback function: */
#define LUI_RGB_CHANGE      1     /* mouse click */
#define LUI_ALPHA_CHANGE    2     /* mouse click */
#define LUI_RGB_RESET       4     /* pressed 'r' */
#define LUI_ALPHA_RESET     8     /* pressed 'R' */
#define LUI_RGB_SHAPE      16     /* pressed arrow key */
#define LUI_ALPHA_SHAPE    32     /* pressed arrow key */




typedef struct lui_colorbar {
   Window parent;
   Window window;
   int x, y;                    /* position */
   int width, height;           /* size */
   int framewidth;
   int wedge_y;                 /* top coord of color wedge */
   int marker_y;                /* top coord of marker arrow */
   int label_y;                 /* y coord of text labels */

   int (*callback)( struct lui_colorbar *, int );  /* user callback func */

/*JUNK
   Window parent;
   int mapped;
*/

   unsigned int *table;
   int table_size;

   char label[40];              /* text label at bottom */
   float minval, maxval;        /* min and max data values */
   int markerpos;               /* position of marker as index into table */
#ifdef JUNK
   float curve, bias;           /* values for arrow key feature */

   int alpha;                   /* either -1 or a constant alpha value */
   float alphapow;              /* power for computing alpha curve */
#endif
   int minalpha, maxalpha;      /* min and max alpha values in table */

   float *params; /*NEW*/

   int helpflag;                /* if nonzero, print help messages */

   int context_index;

/* MJK 12.04.98 */
/* 24Sep97  Phil McDonald */
   char	units[20];
} LUI_COLORBAR;




extern void LUI_ColorBarPacking( unsigned int color_packing );


extern LUI_COLORBAR *LUI_ColorBarCreate( Window parent,
                                         int x, int y, int width, int height );

extern void LUI_ColorBarCallback( LUI_COLORBAR *cb,
                                  int (*callback)( LUI_COLORBAR *, int ) );


extern void LUI_ColorBarDestroy( LUI_COLORBAR *cb );


/* MJK 12.04.98 */
/* 24Sep97  Phil McDonald	added units */
extern void LUI_ColorBarChange( LUI_COLORBAR *cb, char *label, char *units,
                                float min, float max, unsigned int *table,
                                int table_size, float params[]);
#ifdef JOHAN
extern void LUI_ColorBarChange( LUI_COLORBAR *cb,
                                char *label, float min, float max,
                                unsigned int *table, int table_size,
                                float params[] );
#endif
/*
                                int init_rgb, int init_alpha, int alphaval );
*/

extern void LUI_ColorBarShow( LUI_COLORBAR *cb );


extern void LUI_ColorBarHide( LUI_COLORBAR *cb );


extern void LUI_ColorBarSetAlpha( unsigned int *table, int size, int alpha );


extern int LUI_ColorBarEvent( XEvent *event, LUI_COLORBAR **cb );


extern void LUI_ColorBarRedraw( LUI_COLORBAR *cb );


extern int LUI_ColorBarOpacity( LUI_COLORBAR *cb );

/* MJK 4.15.99 */
extern void LUI_ColorBarSetSize( LUI_COLORBAR *cb, int width, int height );

#endif
