/* radio.c */


/*
 * Radio button widget.  Exactly one of a set of buttons can be selected
 * at once.
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lui.h"


#define LMARGIN 20



static void draw_radio( LUI_RADIO *r )
{
   int x, y, x1, x2, width, height;
   int i;

   /* background area */
   x = LUI_Border;
   y = LUI_Border;
   width = r->width - 2 * LUI_Border;
   height = r->height - 2 * LUI_Border;
   XFillRectangle( LUI_Display, r->window, LUI_GC_gray,
                      x, y, width, height );

   /* draw labels */
   for (i=0;i<r->numbuttons;i++) {
      x = LMARGIN;
      y = LUI_Border + 1 + i * (LUI_Font_height+4) + LUI_Font_yoff;

      XDrawString( LUI_Display, r->window, LUI_GC_black,
                   x, y, r->labels[i], strlen(r->labels[i]) );

      x1 = LUI_Border + 2;
      x2 = r->width - 2*LUI_Border + 1;
      y = LUI_Border + i * (LUI_Font_height+4) - 2;
      if (i>0) {
         XDrawLine( LUI_Display, r->window, LUI_GC_black, x1, y, x2, y );
      }
      XDrawRectangle( LUI_Display, r->window, LUI_GC_black,
                      x1, y+3, 11, 11 );

      if (r->current==i) {
         XFillRectangle( LUI_Display, r->window, LUI_GC_red,
                        x1+2, y+5, 8, 8 );

      }
   }

   LUI_DrawFrame( r->window, 0, 0, r->width, r->height,
                  LUI_Border, 1 );
}



static int radio_process( LUI_RADIO *r, XEvent *event )
{
   switch (event->type) {
      case Expose:
         draw_radio( r );
         break;

      case ButtonPress:
         if (event->xbutton.button==Button1) {
            int y = event->xbutton.y;
            int row = (y - LUI_Border - 1) / (LUI_Font_height+4);
            if (row>=0 && row<r->numbuttons) {
               r->current = row;
               draw_radio( r );
               if (r->callback) {
                  (*r->callback) ( r, row );
               }
            }
         }
         break;

      default:
         printf("Error in radio_process: unexpected event\n");
   }
   return 1;
}




/*
 * Create a new radio button widget.
 * Input:  parent - parent window
 *         x, y - position w.r.t parent
 *         width - width in pixels, height is automatic
 *         numbuttons - how many buttons
 *         labels - the button labels
 */
LUI_RADIO *LUI_RadioCreate( Window parent, int x, int y, int width,
                            int numbuttons, char **labels )
{
   LUI_RADIO *r;
   int height;

   height = (LUI_Font_height + 4) * numbuttons + 2 * LUI_Border + 4;

   LUI_LayoutCheck( &x, &y, &width, &height );

   r = (LUI_RADIO *) malloc( sizeof(LUI_RADIO) );
   if (!r) {
      return NULL;
   }

   r->window = XCreateSimpleWindow( LUI_Display, parent,
                                    x, y, width-2, height-2,
                                    1, LUI_Color_black, LUI_Color_gray );

   LUI_EventAdd2( r->window, 
                 ExposureMask | ButtonPressMask,
                  (LUI_FNCP) radio_process, r );

   XMapWindow( LUI_Display, r->window );

   r->x = x;
   r->y = y;
   r->width = width - 2;
   r->height = height - 2;

   r->numbuttons = numbuttons;
   r->labels = labels;
   r->current = 0;
   r->callback = NULL;

/* MJK 12.04.98 */
/* 24Nov07  Phil McDonald */
   LUI_AddWidgetToWindow( parent, r, (LUI_FNCP) LUI_RadioDestroy );

   return r;
}



/*
 * Specify the callback function for a radio button set.
 * The callback function should be declared as:
 *            int callback( LUI_RADIO *r, int selection )
 *
 */
void LUI_RadioCallback( LUI_RADIO *r, int (*callback)( LUI_RADIO *, int) )
{
   r->callback = callback;
   r->context_index = context_index;
}


void LUI_RadioSetCurrent( LUI_RADIO *r, int current )
{
   if (r && current>=0 && current<r->numbuttons) {
      r->current = current;
      draw_radio( r );
   }
}


int LUI_RadioGetCurrent( LUI_RADIO *r )
{
   return r->current;
}


/*
 * Destroy a button. 
 */
void LUI_RadioDestroy( LUI_RADIO *r )
{
   LUI_EventRemove( r->window );
   XDestroyWindow( LUI_Display, r->window );
   free( r );
}

