/* buttonmatrix.c */


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <X11/keysym.h>
#include "lui.h"



#define MIN(A,B)  ( (A) < (B) ? (A) : (B) )


/*
 * NOTES:
 *   Each button in the matrix can have a different color.  This unique
 *     color is only used when the button is turned on.  The X color for
 *     the button is only allocated while the button is on to minimize
 *     colormap allocations.
 */




static void draw_button( LUI_BUTTON_MATRIX *bm, int item, int row, int col )
{
   int x, y;
   int width, height;

   width = bm->bwidth;
   height = bm->bheight;
   x = col * width;
   y = row * height;

   if (bm->labels[item][col]) {
      int len;

/* MJK 12.04.98 */
/* 03Feb98  Phil McDonald       use white on dark or blue-dominated colors */
      GC	text_gc;
      text_gc = ((bm->color[item][col][0] < 0.6) &&
                 (bm->color[item][col][1] < 0.6)) ? LUI_GC_white : LUI_GC_black;

      LUI_DrawFrame( bm->mainwindow, x, y, width-1, height-1, 1,
                     !bm->state[item][col] );

      len = MIN( strlen(bm->labels[item][col]), bm->maxchars );

      if (bm->state[item][col]) {
         /* button is "on" */
         XSetForeground( LUI_Display, LUI_Gc, bm->pixel[item][col] );
         XFillRectangle( LUI_Display, bm->mainwindow, LUI_Gc,
                         x+1, y+1, width-3, height-3 );

         XDrawString( LUI_Display, bm->mainwindow, text_gc,
                     x+2, y+LUI_Font_yoff+1,
                     bm->labels[item][col], len );
/* MJK 12.04.98 */
/*         XDrawString( LUI_Display, bm->mainwindow, LUI_GC_black,
                     x+2, y+LUI_Font_yoff+1,
                     bm->labels[item][col], len );
*/

      }
      else {
         /* button is "off" */
         XFillRectangle( LUI_Display, bm->mainwindow, LUI_GC_gray,
                        x+1, y+1, width-3, height-3 );
         XDrawString( LUI_Display, bm->mainwindow, LUI_GC_black,
                     x+2, y+LUI_Font_yoff+1,
                     bm->labels[item][col], len );
      }
   }
   else {
      /* empty button */
      XFillRectangle( LUI_Display, bm->mainwindow, LUI_GC_gray,
                      x, y, width-1, height-1 );
   }

   XDrawLine( LUI_Display, bm->mainwindow, LUI_GC_black,
              x, y+height-1, x+width-1, y+height-1 );
   XDrawLine( LUI_Display, bm->mainwindow, LUI_GC_black,
              x+width-1, y, x+width-1, y+height-1 );

}



static void draw_matrix( LUI_BUTTON_MATRIX *bm )
{
   int i, j;

   for (i=0;i<bm->viewrows;i++) {
      for (j=0;j<bm->columns;j++) {
         int item = i + bm->toprow;
         if (item<bm->rows) {
            draw_button( bm, item, i, j );
         }
      }
   }
}



static void set_scrollbar( LUI_BUTTON_MATRIX *bm )
{
   double size, pos;

   if (bm->rows <= bm->viewrows) {
      size = 100.0;
      pos = 0.0;
   }
   else {
      size = 100.0 * (double) bm->viewrows / (double) bm->rows;
      pos = 100.0 * bm->toprow / (double) (bm->rows - bm->viewrows);
   }
   LUI_ScrollBarSet( bm->scrollbar, size, pos );
}

void LUI_ButtonMatrixRedraw( LUI_BUTTON_MATRIX *bm)
{
   draw_matrix(bm);
   set_scrollbar(bm);
}



/*
 * Convert a window (x,y) coordinate into a button (row,column).
 */
static int xy_to_rowcol( LUI_BUTTON_MATRIX *bm, int x, int y,
                         int *row, int *col )
{
   *row = y / bm->bheight + bm->toprow;
   *col = x / bm->bwidth;
}



static int matrix_event( LUI_BUTTON_MATRIX *bm, XEvent *event )
{
   switch (event->type) {
      case Expose:
         draw_matrix( bm );
         break;

      case ButtonPress:
         if (bm->callback) {
            int row, col;
            xy_to_rowcol( bm, event->xbutton.x, event->xbutton.y, &row, &col );
/*
            int row = event->xbutton.y / bm->bheight + bm->toprow;
            int col = event->xbutton.x / bm->bwidth;
*/
            if (row < bm->viewrows+bm->toprow) {
               if (bm->labels[row][col]) {
                  (*bm->callback)( bm, row, col, event->xbutton.button );
               }
            }
         }
         break;

      case KeyPress:
         /* Scroll button matrix if too many to see all at once */
         if (bm->rows > bm->viewrows) {
            KeySym keysym;
            XComposeStatus compose;
            char buffer[100];

            XLookupString( &event->xkey, buffer, 100, &keysym, &compose );

            if (keysym==XK_Up && bm->toprow>0) {
               bm->toprow--;
               draw_matrix( bm );
               set_scrollbar( bm );
            }
            else if (keysym==XK_Down && bm->toprow < bm->rows - bm->viewrows) {
               bm->toprow++;
               draw_matrix( bm );
               set_scrollbar( bm );
            }
         }
         break;

      default:
         printf("Error in matrix_event: unexpected event\n");
   }
   return 1;
}



static int sb_event( LUI_SCROLLBAR *sb, float pos )
{
   LUI_BUTTON_MATRIX *bm;
   int oldtop;

   bm = sb->userdata;

   oldtop = bm->toprow;

   bm->toprow = (int) (pos * (bm->rows - bm->viewrows) / 100.0);

   if (oldtop!=bm->toprow) {
      draw_matrix( bm );
   }
   return 1;
}



void LUI_ButtonMatrixDestroy( bm )
LUI_BUTTON_MATRIX *bm;
{
   int i, j;

   /* free the color allocations */
   for (i=0;i<bm->rows;i++) {
      for (j=0;j<bm->columns;j++) {
         if (bm->pixel[i][j]!=0) {
            LUI_FreeColor( bm->pixel[i][j] );
         }

/* MJK 12.04.98 */
/* 24Nov97  Phil McDonald */
         if (bm->labels[i][j]) free (bm->labels[i][j]);

      }
   }

   LUI_ScrollBarDestroy( bm->scrollbar );
   LUI_EventRemove( bm->mainwindow );
   XDestroyWindow( LUI_Display, bm->mainwindow );
   free( bm );
}



LUI_BUTTON_MATRIX *LUI_ButtonMatrixCreate( Window parent, int x, int y,
                                           int width, int height,
                                           int columns )
{
   LUI_BUTTON_MATRIX *bm;
   int xx, yy;

   LUI_LayoutCheck( &x, &y, &width, &height );

   /* allocate struct w/ all fields zeroed */
   bm = (LUI_BUTTON_MATRIX *) calloc( 1, sizeof(LUI_BUTTON_MATRIX) );

   bm->scrollbar = LUI_ScrollBarCreate( parent, x, y, 16, height, 1 );
   LUI_UnlinkWidgetFromWindow( parent, bm->scrollbar );
   bm->scrollbar->userdata = bm;
   LUI_ScrollBarCallback( bm->scrollbar, sb_event );

   bm->mainwindow = XCreateSimpleWindow( LUI_Display, parent,
                                     x+18, y, width-18, height-2,
                                     1, LUI_Color_black, LUI_Color_gray );

   {
      int nx = x+18, ny = y, nw = width-18, nh = height-2;
      LUI_LayoutCheck( &nx, &ny, &nw, &nh );
   }

   bm->x = x+18;
   bm->y = y;
   bm->width = width - 18;
   bm->height = height;
   bm->rows = 0;
   bm->columns = columns;

   bm->toprow = 0;
   bm->bwidth = bm->width / bm->columns;
   bm->bheight = LUI_Font_height + 4;
   bm->viewrows = height / bm->bheight;

   {
      static char str[] = "WWWWWWWWWWWWWW";
      int dir, ascent, descent;
      XCharStruct overall;
      bm->maxchars = 0;
      do {
         bm->maxchars++;
         XTextExtents( LUI_Font, str, bm->maxchars+1, &dir, &ascent,
                       &descent, &overall );

      } while (overall.width < bm->bwidth-2);
   }
        

   bm->callback = NULL;

   LUI_AddWidgetToWindow( parent, bm, (LUI_FNCP) LUI_ButtonMatrixDestroy );

   LUI_EventAdd2( bm->mainwindow, 
                  ExposureMask | ButtonPressMask | KeyPressMask,
                  (LUI_FNCP) matrix_event, bm );

   return bm;
}



/*
 * Add a new row of buttons to the button matrix.
 * Input:  bm - which button matrix
 *         labels - button labels for the row
 *         reds, greens, blues - colors for each button in [0,1]
 */
void LUI_ButtonMatrixAddRow( LUI_BUTTON_MATRIX *bm, char *labels[],
                             float *reds, float *greens, float *blues )
{
   int i;

   if (bm->rows==MAX_BM_ROWS) {
      printf("Error in LUI_ButtonMatrixAddRow: too many rows\n");
      return;
   }

   for (i=0;i<bm->columns;i++) {
      if (labels[i]) {
         bm->labels[bm->rows][i] = strdup( labels[i] );
      }
      else {
         bm->labels[bm->rows][i] = NULL;
      }
      bm->color[bm->rows][i][0] = reds[i];
      bm->color[bm->rows][i][1] = greens[i];
      bm->color[bm->rows][i][2] = blues[i];
      bm->pixel[bm->rows][i] = 0;
      bm->state[bm->rows][i] = 0;
   }

   bm->rows++;

   set_scrollbar( bm );

   draw_matrix( bm );
}



void LUI_ButtonMatrixChangeLabel( LUI_BUTTON_MATRIX *bm,
                                  int row, int column, char *label )
{
   if (bm->labels[row][column]) {
      free( bm->labels[row][column] );
   }
   bm->labels[row][column] = strdup( label );
   draw_matrix( bm );
}


void LUI_ButtonMatrixCallback( LUI_BUTTON_MATRIX *bm, int (*callback)() )
{
   bm->callback = callback;
   bm->context_index = context_index;
}



void LUI_ButtonMatrixSetState( LUI_BUTTON_MATRIX *bm, int row, int col,
                               int state )
{
   /* always free the button's color */
   if (bm->pixel[row][col]>0) {
      LUI_FreeColor( bm->pixel[row][col] );
   }
   bm->pixel[row][col] = 0;

   bm->state[row][col] = state;
   if (state) {
      /* Allocate the button's color */
      float r = bm->color[row][col][0];
      float g = bm->color[row][col][1];
      float b = bm->color[row][col][2];
      bm->pixel[row][col] = LUI_AllocateColor( r, g, b );
   }

   draw_button( bm, row, row-bm->toprow, col );
}



int LUI_ButtonMatrixGetState( LUI_BUTTON_MATRIX *bm, int row, int col )
{
   return bm->state[row][col];
}



/*
 * Set the color of a matrix's button.
 */
void LUI_ButtonMatrixSetColor( LUI_BUTTON_MATRIX *bm, int row, int col,
                               double red, double green, double blue )
{
   bm->color[row][col][0] = red;
   bm->color[row][col][1] = green;
   bm->color[row][col][2] = blue;
   if (bm->state[row][col]) {
      /* force redraw with new color */
      LUI_ButtonMatrixSetState( bm, row, col, 1 );
   }
}



/*
 * Scroll the button matrix so the last row is visible.
 */
void LUI_ButtonMatrixShowBottom( bm )
LUI_BUTTON_MATRIX *bm;
{
   if (bm->viewrows < bm->rows) {
      bm->toprow = bm->rows - bm->viewrows;
      set_scrollbar(bm);
      draw_matrix(bm);
   }
}


/*
 * Change the size of a button matrix window.
 */
void LUI_ButtonMatrixResize( bm, width, height )
LUI_BUTTON_MATRIX *bm;
int width, height;
{
   XResizeWindow( LUI_Display, bm->mainwindow, width-18, height );

   bm->width = width - 10;
   bm->height = height;
   bm->bwidth = bm->width / bm->columns;
   bm->viewrows = height / bm->bheight;

   if (bm->rows <= bm->viewrows) {
      bm->toprow = 0;
   }
   else if (bm->toprow > bm->rows - bm->viewrows) {
      bm->toprow = bm->rows - bm->viewrows;
   }

   LUI_ScrollBarResize( bm->scrollbar, 16, height );
   set_scrollbar( bm );
   draw_matrix( bm );
}



/*
 * Delete all the rows in a button matrix, i.e. empty it.
 */
void LUI_ButtonMatrixEmpty( bm )
LUI_BUTTON_MATRIX *bm;
{
   bm->rows = 0;
   bm->toprow = 0;
   set_scrollbar( bm );
   draw_matrix( bm );
}



