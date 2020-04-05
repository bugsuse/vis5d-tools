/* colorbar.c */

/* Vis5D version 4.2 */

/*
Vis5d program for visualizing five dimensional gridded data sets
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



/*
 * RGB color bar (aka wedge, aka widget) editor.  This is used to inter-
 * actively edit an RGB color table.  The color table is an array of N
 * 4-byte color values.  The way in which red, green, blue, and alpha
 * values (in 0..255) are packed into 4-byte color entries is determined
 * by the color_packing parameter to ColorBarInit.
 *
 * Usage:
 *  1. call ColorBarInit() once to setup the X display, visual, colormap, etc.
 *
 *  2. call ColorBarCreate() once for each ColorBar window you need.  If
 *     you're only going to display 1 ColorBar at a time, only call this once.
 *
 *  3. call ColorBarShow() to display a ColorBar
 *
 */


   

#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lui.h"
#include "X11/keysym.h"

#define DEFAULT_CURVE    1.4
#define DEFAULT_BIAS     1.0
#define DEFAULT_ALPHAPOW  2.0



#define WEDGE_HEIGHT 12
#define MARKER_HEIGHT 10



/* How RGBA values are packed into a 4-byte integer: */
static int rshift, gshift, bshift, ashift;

#define PACK_COLOR( R, G, B, A ) ( ((R)<<rshift) | ((G)<<gshift) | \
                                   ((B)<<bshift) | ((A)<<ashift) )

#define UNPACK_RED( COLOR )     (( (COLOR)>>rshift) & 0xff )
#define UNPACK_GREEN( COLOR )   (( (COLOR)>>gshift) & 0xff )
#define UNPACK_BLUE( COLOR )    (( (COLOR)>>bshift) & 0xff )
#define UNPACK_ALPHA( COLOR )   (( (COLOR)>>ashift) & 0xff )


#define CURVE    0
#define BIAS     1
#define ALPHAPOW 2
#define ALPHAVAL 3
#define DRAWFLAG 4



/* "Clipboard" variables */
#define MAX_TABLE 1000
static unsigned int ClipColors[MAX_TABLE];
static float ClipComp, ClipRot;
static float ClipAlphaPow;
static unsigned int ClipAlpha;



/* Help messages */
#define HELP_LINES 9
static char help_strings[HELP_LINES][100] = {
   "Up/Down Arrows - change curvature",
   "Left/Rt Arrows - move curves left/right",
   "Shift+Arrows - change transparency curve",
   "R - reset colors",
   "Shift+R - reset transparency",
   "C - copy color to clipboard",
   "P - paste colors from clipboard",
   "S - save colors to file",
   "L - load colors from file"
};



void LUI_ColorBarPacking( unsigned int color_packing )
{
   int i;

   /* color packing */
   for (i=0;i<4;i++) {
      switch (color_packing&0xff) {
         case 'R':   rshift = i * 8;   break;
         case 'G':   gshift = i * 8;   break;
         case 'B':   bshift = i * 8;   break;
         case 'A':   ashift = i * 8;   break;
         default:
            /* This is a prgrammer's error: */
            printf("Error: back color_packing argument to LUI_ColorPacking\n");
            exit(0);
      }
      color_packing = color_packing >> 8;
   }

}



/*
 * Display the given colorbar 
 */
void LUI_ColorBarShow( LUI_COLORBAR *cb )
{
   XMapWindow( LUI_Display, cb->window );
}


void LUI_ColorBarHide( LUI_COLORBAR *cb )
{
   XUnmapWindow( LUI_Display, cb->window );
}

#ifdef LEAVEOUT
/*
 * Hide the given ColorBar.
 */
void ColorBarHide( LUI_COLORBAR *cb )
{
   if (!cb) {
      /* Hide all ColorBars */
      ColorBar c;
      int i;
      for (c=first_cb; c; c=c->next) {
         if (c->parent==0) {
            XUnmapWindow( dpy, c->window );
            c->mapped = 0;
         }
      }
   }
   else {
      XUnmapWindow( dpy, cb->window );
      cb->mapped = 0;
   }
}
#endif



/*
 * Copy current colors to the "Clipboard".
 */
static void copy_colors( LUI_COLORBAR *cb )
{
   memcpy( ClipColors, cb->table, cb->table_size*sizeof(unsigned int) );
   ClipComp = cb->params[CURVE];
   ClipRot = cb->params[BIAS];
   ClipAlphaPow = cb->params[ALPHAPOW];
   ClipAlpha = cb->params[ALPHAVAL];
}



/*
 * Paste values from "Clipboard" into given ColorBar.
 */
static void paste_colors( LUI_COLORBAR *cb )
{
   memcpy( cb->table, ClipColors, cb->table_size*sizeof(unsigned int) );
   cb->params[CURVE] = ClipComp;
   cb->params[BIAS] = ClipRot;
   cb->params[ALPHAPOW] = ClipAlphaPow;
   cb->params[ALPHAVAL] = ClipAlpha;
}



/*
 * Save colors to a file.
 *
 * File format is ASCII:
 *    <table_size> <minval> <maxval> <curve> <bias>
 *    <r> <g> <b> <a>         - n lines of rgba values as integers in [0,255]
 *    .....
 *    <r> <g> <b> <a>
 */
static void save_colors( LUI_COLORBAR *cb )
{
   char filename[1000];
   FILE *f;
   int i;

   printf("Enter filename to save colors to: ");
   gets( filename );
   if (filename[0]==0) {
      printf("Save aborted\n");
      return;
   }

   f = fopen( filename, "w" );
   if (!f) {
      printf("Error: couldn't open %s for writing\n", filename );
      return;
   }

   fprintf( f, "%d %f %f %f %f\n", cb->table_size, cb->minval, cb->maxval,
            cb->params[CURVE], cb->params[BIAS] );
   for (i=0;i<cb->table_size;i++) {
      int r, g, b, a;

      r = UNPACK_RED( cb->table[i] );
      g = UNPACK_GREEN( cb->table[i] );
      b = UNPACK_BLUE( cb->table[i] );
      a = UNPACK_ALPHA( cb->table[i] );
   
      fprintf( f, "%d %d %d %d\n", r, g, b, a );
   }
   fclose(f);
   printf("Done\n");
}



/*
 * Load colors from a file.
 */
static void load_colors( LUI_COLORBAR *cb )
{
   char filename[1000];
   FILE *f;
   float min, max;
   int i, j, r[MAX_TABLE], g[MAX_TABLE], b[MAX_TABLE], a[MAX_TABLE];
   float val;
   int entries;

   printf("Enter filename to load colors from: ");
   gets( filename );
   if (filename[0]==0) {
      printf("Load aborted\n");
      return;
   }

   f = fopen( filename, "r" );
   if (!f) {
      printf("Error: couldn't open %s for reading\n", filename );
      return;
   }

   fscanf( f, "%d %f %f %f %f\n", &entries, &min, &max,
           &cb->params[CURVE], &cb->params[BIAS] );
   for (i=0;i<cb->table_size;i++) {
      fscanf( f, "%d %d %d %d\n", &r[i], &g[i], &b[i], &a[i] );
   }
   fclose(f);

   /* now map colors from range specified in file to the range in */
   /* the color widget */

   for (i=0;i<entries;i++) {
      /* convert i to a value in [cb->minval,cb->maxval] */
      val = cb->minval + (float) i / (float) entries * (cb->maxval-cb->minval);
      /* convert val to j in [0..cb->table_size] */
      j = (int) (cb->table_size * (val-min) / (max-min));
      if (j<0) j = 0;
      else if (j>=cb->table_size) j = cb->table_size-1;

      cb->table[i] = PACK_COLOR( r[j], g[j], b[j], a[j] );
   }

   printf("Done\n");
}




/*
 * Convert window X coordinate to color table index.
 */
static int x_to_index( LUI_COLORBAR *cb, int x )
{
   float xscale;
   int index;
   xscale = (float) cb->table_size / (float) (cb->width-cb->framewidth*2);
   index = (x-cb->framewidth) * xscale;
   if (index<0) {
      index = 0;
   }
   else if (index>=cb->table_size) {
      index = cb->table_size-1;
   }
   return index;
}


/*
 * Convert color table index to window X coordinate.
 */
static int index_to_x( LUI_COLORBAR *cb, int index )
{
   float xscale;
   int x;

   xscale = (float) (cb->width-cb->framewidth*2) / (float) (cb->table_size-1);
   x = cb->framewidth + index * xscale;
   if (x>=cb->width-cb->framewidth) {
      x = cb->width-cb->framewidth - 1;
   }
   return x;
}


/*
 * Convert a color intensity to a window Y coordinate.
 */
static int intensity_to_y( LUI_COLORBAR *cb, int intensity )
{
   float yscale;
   int y;
   yscale = (float) (cb->wedge_y-cb->framewidth) / 255.0;
   y = cb->wedge_y - intensity * yscale;
   if (y<0) {
      y = 0;
   }
   else if (y>=cb->wedge_y) {
      y = cb->wedge_y - 1;
   }
   return y;
}


/*
 * Convert a window Y coordinate to a color intensity.
 */
static int y_to_intensity( LUI_COLORBAR *cb, int y )
{
   float yscale;
   int intensity;
   yscale = 255.0 / (float) (cb->wedge_y-cb->framewidth);
   intensity = (cb->wedge_y - y ) * yscale;
   if (intensity<0) {
      intensity = 0;
   }
   else if (intensity>255) {
      intensity = 255;
   }
   return intensity;
}




/*** redraw_range *****************************************************
   Redraw part of a Color Widget.
   Input:  c - which Color Widget
           a,b - range of entries of table to redraw.
**********************************************************************/
static void redraw_range( LUI_COLORBAR *cb, int a, int b )
{
   Window win;
   int i;
   int x,y, px,py;
   int x1, y1, x2, y2;

   win = cb->window;

   if (a<0)  a = 0;
   if (b>=cb->table_size)  b = cb->table_size-1;

   /* calc region to update */
   x1 = index_to_x( cb, a );
   x2 = index_to_x( cb, b);

   y1 = intensity_to_y( cb, 255 );
   y2 = intensity_to_y( cb, 0 ); 

   /* erase region */
   XFillRectangle( LUI_Display, win, LUI_GC_black,
                   x1,y1, x2-x1+1, y2-y1+1 );

   /* redraw region of entries in interval [a,b] */
   if (a>0) a--;
   if (b<cb->table_size-1)  b++;

   /* draw red levels */
   for (i=a;i<=b;i++) {
      x = index_to_x( cb, i );
      y = intensity_to_y( cb, UNPACK_RED(cb->table[i]) );
      if (i!=a)
         XDrawLine( LUI_Display, win, LUI_GC_red, px, py, x, y );
      px = x;  py = y;
   }

   /* draw green levels */
   for (i=a;i<=b;i++) {
      x = index_to_x( cb, i );
      y = intensity_to_y( cb, UNPACK_GREEN(cb->table[i]) );
      if (i!=a)
         XDrawLine( LUI_Display, win, LUI_GC_green, px,py, x,y );
      px = x;  py = y;
   }

   /* draw blue levels */
   for (i=a;i<=b;i++) {
      x = index_to_x( cb, i );
      y = intensity_to_y( cb, UNPACK_BLUE(cb->table[i]) );
      if (i!=a)
         XDrawLine( LUI_Display, win, LUI_GC_blue, px,py, x,y );
      px = x;  py = y;
   }

   /* draw alpha levels */
   if (cb->minalpha<255 || cb->maxalpha<255) {
      for (i=a;i<=b;i++) {
         x = index_to_x( cb, i );
         y = intensity_to_y( cb, UNPACK_ALPHA(cb->table[i]) );
         if (i!=a)
            XDrawLine( LUI_Display, win, LUI_GC_white, px,py, x,y );
         px = x;  py = y;
      }
   }

   /* draw the color bar */
   for (x=x1;x<=x2;x++) {
      int r, g, b;
      unsigned int color;
      i = x_to_index( cb, x );
      color = cb->table[i];
      r = UNPACK_RED( color );
      g = UNPACK_GREEN( color );
      b = UNPACK_BLUE( color );
      XSetForeground( LUI_Display, LUI_Gc, LUI_AllocateColorInt(r,g,b) );
      XDrawLine( LUI_Display, win, LUI_Gc, x, cb->wedge_y,
                 x, cb->wedge_y + WEDGE_HEIGHT );
   }

   if (cb->helpflag) {
      /* print help messages */
      for (i=0;i<HELP_LINES;i++) {
         XDrawString( LUI_Display, win, LUI_GC_white,
                      cb->framewidth+10,
                      cb->framewidth+12+i*(LUI_Font_height+1),
                      help_strings[i], strlen(help_strings[i]) );
      }
   }
}



/* MJK 12.04.98 */
/* 24Sep97  Phil McDonald	added units, fmt */
static void redraw_marker( LUI_COLORBAR *cb )
{
   Window win;
   int x, y0, y1;
   char str[50];
   int dir,ascent, descent;
   XCharStruct overall;
   int xpos;
   float val;
   float	range;
   int		ndigits;
   char		fmt[20] = "";
   char		fmt2[30] = "";

   win = cb->window;

   y0 = cb->marker_y;
   y1 = cb->height - cb->framewidth - 1;
   XFillRectangle( LUI_Display, win, LUI_GC_black,
                   cb->framewidth, y0,
                   cb->width-cb->framewidth*2, y1-y0+1 );

   /* draw marker below color wedge at ctable[pos] */
   x = index_to_x( cb, cb->markerpos );
   XDrawLine( LUI_Display, win, LUI_GC_white,
              x, cb->marker_y, x, cb->marker_y+MARKER_HEIGHT );
   XDrawLine( LUI_Display, win, LUI_GC_white,
              x, cb->marker_y, x-3, cb->marker_y+6 );
   XDrawLine( LUI_Display, win, LUI_GC_white,
              x, cb->marker_y, x+3, cb->marker_y+6 );

   range = (cb->maxval > cb->minval) ? cb->maxval - cb->minval :
                                       cb->minval - cb->maxval;
   if (range == 0.0) range = 1.0;
   ndigits = (2.5 - log10 (range));
   if (ndigits < 0) ndigits = 0;
   if (ndigits > 9) ndigits = 9;
   sprintf (fmt, "%%.%df", ndigits);

   /* draw min value */
   sprintf( str, fmt, cb->minval );
   XDrawString( LUI_Display, win, LUI_GC_white,
                cb->framewidth+2, cb->label_y, str, strlen(str) );

   /* draw variable name and marker value */
   val = cb->minval + (cb->maxval-cb->minval)
                  * ( (float) cb->markerpos / (float) (cb->table_size-1));
   sprintf (fmt2, "%%s = %s", fmt);
   sprintf (str, fmt2, cb->label, val);
   if (strlen (cb->units) > 0)
   {
      strcat (str, " ");
      strcat (str, cb->units);
   }
   XTextExtents( LUI_Font, str, strlen(str), &dir,&ascent,&descent,&overall );
   xpos = (cb->width - overall.width) / 2;
   XDrawString( LUI_Display, win, LUI_GC_white,
                xpos, cb->label_y, str, strlen(str) );

   /* draw max value */
   sprintf( str, fmt, cb->maxval );
   XTextExtents( LUI_Font, str, strlen(str), &dir,&ascent,&descent,&overall );
   xpos = cb->width - overall.width - cb->framewidth - 2;
   XDrawString( LUI_Display, win, LUI_GC_white,
                xpos, cb->label_y, str, strlen(str) );

   /* draw new compression & rotation factors */
/*  This can be un-commented if you want to see these values superimposed
   over the color curves.
   fprintf(stderr,"redraw_marker(): Curve=%.1f\n",cb->curve);
   sprintf(str,"comp=%.1f",cb->curve);
   XDrawString( LUI_Display, win, LUI_GC_white, 15, 75, str, strlen(str) );
   fprintf(stderr,"redraw_marker(): Bias =%.3f\n\n",cb->params[BIAS]);
   sprintf(str,"rot =%.3f",cb->params[BIAS]);
   XDrawString( LUI_Display, win, LUI_GC_white, 15, 90, str, strlen(str) );
*/

   LUI_DrawFrame( win, 1, 1, cb->width-2, cb->height-2, cb->framewidth-1, 1 );
   XDrawRectangle( LUI_Display, win, LUI_GC_black,
                   0, 0, cb->width-1, cb->height-1 );
}
#ifdef JOHAN
static void redraw_marker( LUI_COLORBAR *cb )
{
   Window win;
   int x, y0, y1;
   char str[50];
   int dir,ascent, descent;
   XCharStruct overall;
   int xpos;
   float val;

   win = cb->window;

   y0 = cb->marker_y;
   y1 = cb->height - cb->framewidth - 1;
   XFillRectangle( LUI_Display, win, LUI_GC_black,
                   cb->framewidth, y0,
                   cb->width-cb->framewidth*2, y1-y0+1 );

   /* draw marker below color wedge at ctable[pos] */
   x = index_to_x( cb, cb->markerpos );
   XDrawLine( LUI_Display, win, LUI_GC_white,
              x, cb->marker_y, x, cb->marker_y+MARKER_HEIGHT );
   XDrawLine( LUI_Display, win, LUI_GC_white,
              x, cb->marker_y, x-3, cb->marker_y+6 );
   XDrawLine( LUI_Display, win, LUI_GC_white,
              x, cb->marker_y, x+3, cb->marker_y+6 );

   /* draw min value */
   sprintf( str, "%.2f", cb->minval );
   XDrawString( LUI_Display, win, LUI_GC_white,
                cb->framewidth+2, cb->label_y, str, strlen(str) );

   /* draw variable name and marker value */
   val = cb->minval + (cb->maxval-cb->minval)
                  * ( (float) cb->markerpos / (float) (cb->table_size-1));
   sprintf(str,"%s= %.2f", cb->label, val );
   XTextExtents( LUI_Font, str, strlen(str), &dir,&ascent,&descent,&overall );
   xpos = (cb->width - overall.width) / 2;
   XDrawString( LUI_Display, win, LUI_GC_white,
                xpos, cb->label_y, str, strlen(str) );

   /* draw max value */
   sprintf( str, "%.2f", cb->maxval );
   XTextExtents( LUI_Font, str, strlen(str), &dir,&ascent,&descent,&overall );
   xpos = cb->width - overall.width - cb->framewidth - 2;
   XDrawString( LUI_Display, win, LUI_GC_white,
                xpos, cb->label_y, str, strlen(str) );

   /* draw new compression & rotation factors */
/*  This can be un-commented if you want to see these values superimposed
   over the color curves.
   fprintf(stderr,"redraw_marker(): Curve=%.1f\n",cb->curve);
   sprintf(str,"comp=%.1f",cb->curve);
   XDrawString( LUI_Display, win, LUI_GC_white, 15, 75, str, strlen(str) );
   fprintf(stderr,"redraw_marker(): Bias =%.3f\n\n",cb->params[BIAS]);
   sprintf(str,"rot =%.3f",cb->params[BIAS]);
   XDrawString( LUI_Display, win, LUI_GC_white, 15, 90, str, strlen(str) );
*/

   LUI_DrawFrame( win, 1, 1, cb->width-2, cb->height-2, cb->framewidth-1, 1 );
   XDrawRectangle( LUI_Display, win, LUI_GC_black,
                   0, 0, cb->width-1, cb->height-1 );
}
#endif



/*
 * Redraw a ColorBar window.
 * Input:  cb - the ColorBar object
 */
void LUI_ColorBarRedraw( LUI_COLORBAR *cb )
{
   Window win, root;
   int x, y;
   unsigned int width, height, bw, depth;

   win = cb->window;

   redraw_range( cb, 0, cb->table_size-1 );
   redraw_marker( cb );

   LUI_DrawFrame( win, 1, 1, cb->width-2, cb->height-2, cb->framewidth-1, 1 );
   XDrawRectangle( LUI_Display, win, LUI_GC_black,
                   0, 0, cb->width-1, cb->height-1 );

}




/*
 * Called to set the size or resize a colorbar widget.
 */
/* MJK 4.15.99 */
void LUI_ColorBarSetSize( LUI_COLORBAR *cb, int width, int height )
{
   XResizeWindow(LUI_Display, cb->window, width, height);
   cb->width = width;
   cb->height = height;
   cb->label_y = cb->height - 5;  /* 4? */
   cb->marker_y = cb->label_y + 1 - MARKER_HEIGHT - LUI_Font_height;
   cb->wedge_y = cb->marker_y - WEDGE_HEIGHT;
}




#define ANY_MODIFIER (ShiftMask|ControlMask|Mod1Mask)


/*
 * Given an XEvent, process it if it pertains to a ColorBar, else do
 * nothing with it.
 * Input:  event - the X event
 * Output:  whichcb - which colorbar this event pertained to.
 * Return: 1 - if color table was changed.
 *         2 - if reset key was pressed
 *         3 - if <shift>+reset key was pressed
 *         0 - if no change
 */
static int colorbar_process( LUI_COLORBAR *cb, XEvent *event )
{
   static int p1 = 0, p2 = 0, p3 = 0, p4 = 0; /* red, green, blue, alpha */
   static int pentry;
   int i, modify, entry, result;
   static int move_marker;

   result = modify = 0;

   if (event->type==KeyPress) {
      char keybuf[50];
      KeySym key;
      XComposeStatus compose;
      int count;
      count = XLookupString( &event->xkey, keybuf, 50, &key, &compose );
      if (count==1) {
         if (keybuf[0]=='r') {
            /* Reset RGB */
            cb->params[DRAWFLAG] = 0.0;
            result = LUI_RGB_RESET;
         }
         else if (keybuf[0]=='R') {
            /* Reset alpha */
            result = LUI_ALPHA_RESET;
            cb->params[DRAWFLAG] = 0.0;
         }
         else if (keybuf[0]=='c' || keybuf[0]=='C') {
            /* Copy current colors to clipboard */
            copy_colors( cb );
         }
         else if (keybuf[0]=='p' || keybuf[0]=='P') {
            /* Paste clipboard colors to current color widget */
            paste_colors( cb );
            LUI_ColorBarRedraw( cb );
            result = LUI_RGB_CHANGE | LUI_ALPHA_CHANGE;
            cb->params[DRAWFLAG] = 0.0;
            result = LUI_RGB_CHANGE | LUI_ALPHA_CHANGE;
         }

/* WLH 7-18-96 */
         else if (keybuf[0]=='s' || keybuf[0]=='S') {
            /* save colors to a file */
            save_colors( cb );
         }
         else if (keybuf[0]=='l' || keybuf[0]=='L') {
            /* load colors from a file */
            load_colors( cb );
            LUI_ColorBarRedraw( cb );
            return 1;
         }

         else {
            /* if unused key, toggle help display */
            cb->helpflag = !cb->helpflag;
            LUI_ColorBarRedraw( cb );
         }
      }
      else if (key == XK_Left) {
         /* rotate left */
         cb->params[BIAS] -= 0.03/cb->params[CURVE];
         result = LUI_RGB_SHAPE;
         cb->params[DRAWFLAG] = 0.0;
      }
      else if (key == XK_Right) {
         /* rotate right */
         cb->params[BIAS] += 0.03/cb->params[CURVE];
         result = LUI_RGB_SHAPE;
         cb->params[DRAWFLAG] = 0.0;
      }
      else if (key == XK_Up) {
         /* expand color map */
         if (event->xkey.state & ANY_MODIFIER) {
            cb->params[ALPHAPOW] -= 0.1;
            if (cb->params[ALPHAPOW]<0.0)
              cb->params[ALPHAPOW] = 0.0;
            result = LUI_ALPHA_SHAPE;
            cb->params[DRAWFLAG] = 0.0;
         }
         else {
            cb->params[CURVE] -= 0.1;
            result = LUI_RGB_SHAPE;
            cb->params[DRAWFLAG] = 0.0;
         }
      }
      else if (key == XK_Down) {
         /* compress color map */
         if (event->xkey.state & ANY_MODIFIER) {
            cb->params[ALPHAPOW] += 0.1;
            result = LUI_ALPHA_SHAPE;
            cb->params[DRAWFLAG] = 0.0;
         }
         else {
            cb->params[CURVE] += 0.1;
            result = LUI_RGB_SHAPE;
            cb->params[DRAWFLAG] = 0.0;
         }
      }
   }
   else if (event->type==Expose && event->xexpose.count==0) {
      LUI_ColorBarRedraw( cb );
      result = 0;
   }
   else if (event->type==ConfigureNotify) {
/* MJK 4.15.99 */
      LUI_ColorBarSetSize( cb, event->xconfigure.width, event->xconfigure.height );
      result = 0;
   }
   else if (event->type==ButtonPress ) {
      if (event->xbutton.y<cb->wedge_y) {
         /* change color function */
         move_marker = 0;
      }
      else {
         /* change marker position */
         move_marker = 1;
      }
      /* determine which curve to modify */
      if (event->xbutton.state&ANY_MODIFIER) {
         p4 = 1;
      }
      else {
         if (event->xbutton.button==Button1)  p1 = 1;
         if (event->xbutton.button==Button2)  p2 = 1;
         if (event->xbutton.button==Button3)  p3 = 1;
      }
      pentry = x_to_index( cb, event->xbutton.x );
      modify = 1;
   }
   else if (event->type==ButtonRelease) {
      if (p1 || p2 || p3) {
         result = LUI_RGB_CHANGE;
      }
      else {
         result = LUI_ALPHA_CHANGE;
      }
      if (event->xbutton.button==Button1)  p1 = 0;
      if (event->xbutton.button==Button2)  p2 = 0;
      if (event->xbutton.button==Button3)  p3 = 0;
      p4 = 0;
   }
   else if (event->type==MotionNotify) {
      /* Flush extra MotionNotify events */
      while (QLength(LUI_Display)>0) {
         XEvent next;
         XPeekEvent(LUI_Display, &next);
         if (next.type!=MotionNotify)
            break;
         XNextEvent(LUI_Display, event);
      }
      modify = 1;
   }


   /* Modify one or more of the color curves */

   if (modify && (p1 || p2 || p3 || p4)) {
      /* calculate which entry in color table to change */
      entry = x_to_index( cb, event->xbutton.x );
      /* update */
      if (move_marker) {
         /* changing marker position */
         cb->markerpos = entry;
         redraw_marker( cb );
      }
      else {
         /* changing color graph */
         int a, b, value;

         value = y_to_intensity( cb, event->xbutton.y );

         if (pentry<=entry) {
            a = pentry;
            b = entry;
         }
         else {
            a = entry;
            b = pentry;
         }

         /* update entries from 'pentry' to 'entry' */
         for (i=a; i<=b; i++) {
            int red, green, blue, alpha;
            red = UNPACK_RED(cb->table[i]);
            green = UNPACK_GREEN(cb->table[i]);
            blue = UNPACK_BLUE(cb->table[i]);
            alpha = UNPACK_ALPHA(cb->table[i]);
            if (p1) {
               /* modify red */
               red = value;
            }
            if (p2) {
               /* modify green */
               green = value;
            }
            if (p3) {
               /* modify blue */
               blue = value;
            }
            if (p4) {
               /* modify alpha */
               alpha = value;
            }
            /* change the color table entry */
            cb->table[i] = PACK_COLOR(red,green,blue,alpha);
         } /* for */

         /* redraw the color curves */
         if (pentry<entry)
           redraw_range( cb, pentry-1, entry+1 );
         else
           redraw_range( cb, entry-1, pentry+1 );

         pentry = entry;

         if (p4) {
            /* update min,max alpha values */
            cb->minalpha = 256;
            cb->maxalpha = 0;
            for (i=0;i<cb->table_size;i++) {
               int a = UNPACK_ALPHA( cb->table[i] );
               if (a<cb->minalpha)  cb->minalpha = a;
               if (a>cb->maxalpha)  cb->maxalpha = a;
            }
         }

         if (p4) {
            result = LUI_ALPHA_CHANGE;
         }
         else {
            result = LUI_RGB_CHANGE;
         }
         cb->params[DRAWFLAG] = 1.0;
      }
   } /*modify*/


   if (result!=0 && cb->callback) {
      (*cb->callback)(cb, result);
   }

   return result;
}




/*
 * Return the "opacity" of a colorbar.  
 * Input:  cb - the colorbar in question
 * Return:  0..255 = the constant opacity of the color table entries
 *          -1     = variable opacity in the table
 */
int LUI_ColorBarOpacity( LUI_COLORBAR *cb )
{
   if (cb->minalpha==cb->maxalpha) {
      return cb->minalpha;
   }
   else {
      return -1;
   }
}



/*
 * Create a ColorBar widget (i.e. window).
 * Input:  parent - the parent X window, 0 indicates root window
 *         x, y - position with respect to parent
 *         width, height - size of widget window
 */
LUI_COLORBAR *LUI_ColorBarCreate( Window parent, int x, int y,
                                  int width, int height )
{
   LUI_COLORBAR *cb;
   int scr;
   Window win;
   unsigned long mask;
   XSetWindowAttributes attr;
   XSizeHints hints;
   int scrheight;

   LUI_LayoutCheck( &x, &y, &width, &height );

   /* allocate struct */
   cb = (LUI_COLORBAR *) calloc( 1, sizeof(LUI_COLORBAR) );
   if (!cb) {
      return NULL;
   }

   /* make sure window isn't off bottom of screen */
   if (!parent && y+height > LUI_Height) {
      y = LUI_Height - height;
   }

   /*** Create the X window ***/
   if (parent) {
      cb->window = LUI_CreateWindowAt( parent, x, y, width, height );
      XMapWindow( LUI_Display, cb->window );
   }
   else {
      parent = LUI_RootWindow;
      cb->window = LUI_CreateWindowAt( parent, x, y, width, height );
   }

   /* init fields */
   cb->x = x;
   cb->y = y;
/*
   cb->width = width;
   cb->height = height;
*/
   cb->parent = parent;

   LUI_EventAdd2( cb->window, 
                  ExposureMask | ButtonMotionMask | KeyPressMask
                  | ButtonPressMask | ButtonReleaseMask | StructureNotifyMask,
                  (LUI_FNCP) colorbar_process, cb );

   cb->framewidth = LUI_Border + 1;
/* MJK 4.15.99 */      
   LUI_ColorBarSetSize( cb, width, height );

   return cb;
}



void LUI_ColorBarCallback( LUI_COLORBAR *cb,
                           int (*callback)( LUI_COLORBAR *, int ) )
{
   assert(cb);
   cb->callback = callback;
}



void LUI_ColorBarDestroy( LUI_COLORBAR *cb )
{
   XDestroyWindow( LUI_Display, cb->window );
   free(cb);
}




/*
 * Change the colortable displayed in a LUI_COLORBAR widget.  If the
 * color table pointer is a new one.  If init_rgb or init_alpha is non-
 * zero, the colortable will be initialized.
 * Input:  cb - the LUI_COLORBAR widget
 *         label - the text label
 *         min, max - minimum and maximum data values
 *         table - address of color look-up table array
 *         table_size - number of entries in the table.
 *         init_rgb - if nonzero, initialize RGB components of table
 *         init_alpha - if nonzero, initialize alpha component of table
 *         alphaval - positive, constant alpha value or -1 for variable alpha
 */
/* MJK 12.04.98 */
/* 24Sep97  Phil McDonald	added units */
void LUI_ColorBarChange( LUI_COLORBAR *cb, char *label, char *units,
                         float min, float max,
                         unsigned int *table, int table_size, float params[])
{
   int i;
   int found;

   assert( cb );
   assert( table_size > 0 );

   strcpy( cb->label, label );
   cb->table = table;
   cb->table_size = table_size;
   cb->minval = min;
   cb->maxval = max;

   cb->params = params;

   if (units == NULL)
      strcpy (cb->units, "");
   else
      strcpy (cb->units, units);

   LUI_ColorBarRedraw( cb );
}
#ifdef JOHAN
void LUI_ColorBarChange( LUI_COLORBAR *cb, char *label, float min, float max,
                         unsigned int *table, int table_size,
                         float params[] )
{
   int i;
   int found;

   assert( cb );
   assert( table_size > 0 );

   strcpy( cb->label, label );
   cb->table = table;
   cb->table_size = table_size;
   cb->minval = min;
   cb->maxval = max;

   cb->params = params;

   LUI_ColorBarRedraw( cb );
}
#endif
