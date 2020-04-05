/* scrollbar.c */


/*
 * Scrollbar widget
 */



#include <stdio.h>
#include <stdlib.h>
#include "lui.h"
#include "scrollbar.h"





static void compute_knob( LUI_SCROLLBAR *sb, int *x, int *y,
                          int *width, int *height )
{
   /* compute knob position */
   if (sb->orientation==1) {
      /* vertical */
      *x = sb->border + 1;
      *width = sb->width - 2*sb->border - 2;
      *height = (sb->height - 2*sb->border - 2) * sb->size / 100.0;
      if (*height<10) {
         *height = 10;
      }
      *y = sb->border + 1 + (sb->height - 2*sb->border - 2 - *height)
            * sb->position / 100.0;
   }
   else {
      /* horizontal */
      *y = sb->border + 1;
      *height = sb->height - 2*sb->border - 2;
      *width = (sb->width - 2*sb->border - 2) * sb->size / 100.0;
      if (*width<10) {
         *width = 10;
      }
      *x = sb->border + 1 + (sb->width - 2*sb->border - 2 - *width)
             * sb->position / 100.0;
   }
}


static void draw( LUI_SCROLLBAR *sb )
{
   int x, y, width, height;

   compute_knob( sb, &x, &y, &width, &height );


   /* draw background */
   XFillRectangle( LUI_Display, sb->window, LUI_GC_gray,
                   sb->border, sb->border, sb->width-2*sb->border,
                   sb->height-2*sb->border );

   /* draw knob */
/*
   XFillRectangle( LUI_Display, sb->window, LUI_GC_black,
                   x, y, width, height );
*/

   LUI_DrawFrame( sb->window, x, y, width, height, 3, 1 );
   XDrawRectangle( LUI_Display, sb->window, LUI_GC_black,
                   x, y, width-1, height-1 );

#ifdef EXPERIMENTAL
   if (sb->orientation==1) {
      int w = sb->width - 2*sb->border;
      int y2 = y + height;
      XFillRectangle( LUI_Display, sb->window, LUI_GC_gray,
                      sb->border, sb->border, w, y-sb->border );
      XFillRectangle( LUI_Display, sb->window, LUI_GC_black,
                      x, y, w, height );
      XFillRectangle( LUI_Display, sb->window, LUI_GC_gray,
                      sb->border, y2, w, sb->height-y2-sb->border );
   }
#endif

   /* draw frame */
   LUI_DrawFrame( sb->window, 0, 0, sb->width, sb->height,
                  sb->border, 0 );
}



/* called when button is pressed */
static void press( LUI_SCROLLBAR *sb, int newx, int newy )
{
   int x, y, width, height;
   float jump;

   sb->dragging = 0;

   jump = 100.0 / ((100.0 / sb->size) - 1);
/*   jump = 100.0 / ((99.0 / sb->size) - 0);*/


   compute_knob( sb, &x, &y, &width, &height );
   if (sb->orientation==1) {
      /* vertical */
      if (newy < y) {
         /* jump up */
         sb->position -= jump;
         if (sb->position<0.0) {
            sb->position = 0.0;
         }
         draw( sb );
         if (sb->callback) {
            (*sb->callback)( sb, sb->position );
         }
      }
      else if (newy>=y+height) {
         /* jump down */
         sb->position += jump;
         if (sb->position>100.0) {
            sb->position = 100.0;
         }
         draw( sb );
         if (sb->callback) {
            (*sb->callback)( sb, sb->position );
         }
      }
      else if (sb->size<100.0) {
         sb->startx = newx;
         sb->starty = newy;
         sb->startpos = sb->position;
         sb->delta = newy - y;
         sb->dragging = 1;
      }
   }
   else {
      /* horizontal */
      if (newx < x) {
         /* jump left */
         sb->position -= jump;
         if (sb->position<0.0) {
            sb->position = 0.0;
         }
         draw( sb );
         if (sb->callback) {
            (*sb->callback)( sb, sb->position );
         }
      }
      else if (newx>=x+width) {
         /* jump right */
         sb->position += jump;
         if (sb->position>100.0) {
            sb->position = 100.0;
         }
         draw( sb );
         if (sb->callback) {
            (*sb->callback)( sb, sb->position );
         }
      }
      else if (sb->size<100.0) {
         sb->startx = newx;
         sb->starty = newy;
         sb->startpos = sb->position;
         sb->delta = newx - x;
         sb->dragging = 1;
      }
   }
}



/* called when mouse is dragged */
static void drag( LUI_SCROLLBAR *sb, int newx, int newy )
{
   float newpos;
   int x, y, width, height;

   if (sb->dragging) {
      compute_knob( sb, &x, &y, &width, &height );

      if (sb->orientation==1) {
         /* vertical */
         newy = newy - sb->border - 2 - sb->delta;

         newpos = (float) newy / (sb->height - 2*sb->border - 2 - height) * 100.0;
   /*
         newpos = 100.0 * (float) (newy - sb->border-2)
                     / (float) (sb->height - 2*sb->border - 4);
   */
         if (newpos<0.0) {
            sb->position = 0.0;
         }
         else if (newpos>100.0) {
            sb->position = 100.0;
         }
         else {
            sb->position = newpos;
         }
      }
      else {
         /* horizontal */
         newx = newx - sb->border - 2 - sb->delta;

         newpos = (float) newx / (sb->width - 2*sb->border - 2 - width) * 100.0;
         if (newpos<0.0) {
            sb->position = 0.0;
         }
         else if (newpos>100.0) {
            sb->position = 100.0;
         }
         else {
            sb->position = newpos;
         }
      }
      draw( sb );
      if (sb->callback) {
         (*sb->callback)( sb, sb->position );
      }
   }
}




/*
 * When an X event occurs in the scrollbar window, this function will
 * be called.
 * Input:  scrollbar - which scrollbar widget
 *         event - the X event
 */
static int process( LUI_SCROLLBAR *scrollbar, XEvent *event )
{
   int x, y;

   switch (event->type) {
      case Expose:
         draw( scrollbar );
         break;

      case ButtonPress:
         press( scrollbar, event->xbutton.x, event->xbutton.y );
         break;

      case MotionNotify:
         /* dragging slider */
         {
            Display *dpy = event->xany.display;
            XEvent next;
            x = event->xmotion.x;
            y = event->xmotion.y;
            while (XPending(dpy)) {
               XPeekEvent(dpy,&next);
               if (next.type==MotionNotify) {
                  x = next.xmotion.x;
                  y = next.xmotion.y;
                  XNextEvent(dpy,&next);  /* discard the event */
               }
               else {
                  break;
               }
            }
         }
         drag( scrollbar, x, y );
         break;

      case ButtonRelease:
         scrollbar->dragging = 0;
         break;

      default:
         printf("Error in scrollbar process:  unexpected event (%d)\n",
                (int) event->type );
   }
   return 1;
}



/*
 * Create a new scroll bar widget.
 * Input:  parent - parent window ID
 *         x, y - position of scroll bar w.r.t. parent
 *         width, height - outside dimensions of scrollbar
 *         orientation - 1 = vertical, 0 = horizontal
 */
LUI_SCROLLBAR * LUI_ScrollBarCreate( Window parent, int x, int y,
                                     int width, int height,
                                     int orientation )
{
   LUI_SCROLLBAR *scrollbar;

   LUI_LayoutCheck( &x, &y, &width, &height );

   scrollbar = (LUI_SCROLLBAR *) malloc( sizeof(LUI_SCROLLBAR) );
   if (!scrollbar) {
      return NULL;
   }

   scrollbar->x = x;
   scrollbar->y = y;
   scrollbar->width = width /*- 2*/;
   scrollbar->height = height /*- 2*/;
   scrollbar->border = LUI_Border;
   scrollbar->window = XCreateSimpleWindow( LUI_Display, parent,
                                       x, y, width/*-2*/, height/*-2*/,
                                       1, LUI_Color_black, LUI_Color_gray );
   LUI_EventAdd2( scrollbar->window,
                  ExposureMask | ButtonPressMask | ButtonReleaseMask
                  | ButtonMotionMask,
                  (LUI_FNCP) process, scrollbar );
   XMapWindow( LUI_Display, scrollbar->window );

   scrollbar->orientation = orientation;
   scrollbar->size = 100.0;
   scrollbar->position = 0.0;

   scrollbar->callback = NULL;
   scrollbar->userdata = NULL;

   return scrollbar;
}



/*
 * Change the size and position of the scrollbar and redraw it.
 * Input:  scrollbar - the scrollbar
 *         size - size of knob as a percent in [0,100]
 *         position - position of knob as percent in [0,100]
 */
void LUI_ScrollBarSet( LUI_SCROLLBAR *scrollbar, float size, float position )
{
   scrollbar->size = size;
   scrollbar->position = position;
   draw( scrollbar );
}



/*
 * Change the position of the scrollbar and redraw it.
 * Input:  scrollbar - the scrollbar
 *         position - position of knob as percent in [0,100]
 */
void LUI_ScrollBarSetPos( LUI_SCROLLBAR *scrollbar, float position )
{
   scrollbar->position = position;
   draw( scrollbar );
}



/*
 * Return the current position of the named scrollbar.
 */
float LUI_ScrollBarGetPos( LUI_SCROLLBAR *scrollbar )
{
   return scrollbar->position;
}



/*
 * Register the function to call when the scrollbar is moved.
 * Input:  scrollbar - which scrollbar
 *         callback - the callback function.  It should be declared as:
 *                       int callback( LUI_SCROLLBAR *sb, float position )
 *                    The position will be in [0,100]
 */
void LUI_ScrollBarCallback( LUI_SCROLLBAR *scrollbar,
                            int (* callback)( LUI_SCROLLBAR *, float ) )
{
   scrollbar->callback = callback;
   scrollbar->context_index = context_index;
}


/*
 * Attach a pointer to userdata to the scrollbar.
 */
void LUI_ScrollBarData( LUI_SCROLLBAR *scrollbar, void *data )
{
   scrollbar->userdata = data;
}



/*
 * Destroy a scrollbar.
 */
void LUI_ScrollBarDestroy( LUI_SCROLLBAR *sb )
{
   LUI_EventRemove( sb->window );
   XDestroyWindow( LUI_Display, sb->window );
   free( sb );
}



void LUI_ScrollBarResize( LUI_SCROLLBAR *sb, int width, int height )
{
   XResizeWindow( LUI_Display, sb->window, width, height );
   sb->width = width;
   sb->height = height;
}
