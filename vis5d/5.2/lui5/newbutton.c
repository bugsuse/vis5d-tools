/* newbutton.c */


/*
 * New push/toggle button widget
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lui.h"


#define LMARGIN 2



static void draw_button( LUI_NEWBUTTON *b )
{
   int x, y, texty, width, height;
   GC backgc;

   if (b->highlight) {
      backgc = LUI_GC_highgray;
   }
   else {
      backgc = LUI_GC_gray;
   }


   /* background area */
   x = b->framewidth;
   y = b->framewidth;
   width = b->width - 2 * b->framewidth;
   height = b->height - 2 * b->framewidth;

   /* center label vertically */
   texty = b->framewidth + (b->height - 2*b->framewidth - LUI_Font_height) / 2
           + LUI_Font_yoff;

   if (b->toggle==2) {
      /* a toggle button with a red light */
      int hgt = height - 8;
      /* draw on/off light indicator next to label */
      XFillRectangle( LUI_Display, b->window, backgc, x, y, width, height );
      if (b->state==1) {
         /* lit */
         XFillRectangle( LUI_Display, b->window, LUI_GC_red,
                         x+4, y+4, height/2-3, height-8 );
      }
      XDrawRectangle( LUI_Display, b->window, LUI_GC_black,
                      x+2, y+2, height/2, height-5 );
      XDrawString( LUI_Display, b->window, LUI_GC_black,
                   x+LMARGIN+4+height/2, texty, b->label, strlen(b->label) );
      LUI_DrawFrame( b->window, 0, 0, b->width, b->height,
                     b->framewidth, 1 );
   }
   else {
      if (b->state==0) {
         /* off */
         XFillRectangle( LUI_Display, b->window, backgc, x, y, width, height );
         XDrawString( LUI_Display, b->window, LUI_GC_black,
                      x+LMARGIN, texty, b->label, strlen(b->label) );
      }
      else {
         /* on */
         XSetForeground( LUI_Display, LUI_Gc, b->color );
         XFillRectangle( LUI_Display, b->window, LUI_Gc,
                         x, y, width, height );

/* MJK 12.04.98 */
/* 03Feb98  Phil McDonald	use white on dark or blue-dominated colors */
         if ((b->red < 0.6) && (b->green < 0.6)) {
#ifdef JOHAN
         if (b->red+b->green+b->blue < 0.6) {
#endif

            /* white text */
            XDrawString( LUI_Display, b->window, LUI_GC_white,
                        x+LMARGIN, texty, b->label, strlen(b->label) );
         }
         else {
            /* black text */
            XDrawString( LUI_Display, b->window, LUI_GC_black,
                        x+LMARGIN, texty, b->label, strlen(b->label) );
         }
      }
      LUI_DrawFrame( b->window, 0, 0, b->width, b->height,
                     b->framewidth, !b->state );
   }

}



static int button_process( LUI_NEWBUTTON *b, XEvent *event )
{
   int x, y;


   switch (event->type) {
      case Expose:
         draw_button( b );
         break;

      case ButtonPress:
         b->mousebutton = event->xbutton.button;
         if (event->xbutton.button==Button1 ||
             (b->special==1 && event->xbutton.button==Button3)) {
            if (b->toggle) {
               b->state = !b->state;
               if (b->callback) {
                  (*b->callback)( b, b->state );
               }
            }
            else {
               b->state = 1;
            }
            draw_button( b );
         }
         else if (b->callback) {
            /* other button */
            (*b->callback)( b, b->state );
         }

         break;

      case ButtonRelease:
         b->mousebutton = event->xbutton.button;
         if (event->xbutton.button==Button1 ||
             (b->special==1 && event->xbutton.button==Button3)) {
            if (b->toggle) {
               /* nothing */
            }
            else {
               /* push button now released -> activate */
               b->state = 0;
               draw_button( b );
               x = event->xbutton.x;
               y = event->xbutton.y;
               if (b->callback && x>=0 && x<=b->width && y>=0 && y<=b->height) {
                  (*b->callback)( b, b->state );
               }
            }
         }
         break;

      case EnterNotify:
         b->highlight = 1;
         draw_button( b );
         break;

      case LeaveNotify:
         b->highlight = 0;
         draw_button( b );
         break;

      default:
         printf("Error in button_process: unexpected event\n");
   }
   return 1;
}


/*
 * Destroy a button. 
 */
void LUI_NewButtonDestroy( LUI_NEWBUTTON *button )
{
   LUI_EventRemove( button->window );
   XDestroyWindow( LUI_Display, button->window );
   button->window = NULL;
   free( button->label );
   button->label = NULL;
   free( button );
   button = NULL;
}


/*
 * Create a push or toggle button.
 * Input:  toggle - 0=push button, 1=toggle button, 2=lighted toggle button
 */
static LUI_NEWBUTTON *new_button( Window parent, int x, int y,
                                  int width, int height, char *label,
                                  int toggle )
{
   LUI_NEWBUTTON *b;

   LUI_LayoutCheck( &x, &y, &width, &height );

   b = (LUI_NEWBUTTON *) malloc( sizeof(LUI_NEWBUTTON) );
   if (!b) {
      return NULL;
   }

   b->window = XCreateSimpleWindow( LUI_Display, parent,
                                    x, y, width-2, height-2,
                                    1, LUI_Color_black, LUI_Color_gray );

   LUI_EventAdd2( b->window, 
                  ExposureMask | ButtonPressMask | ButtonReleaseMask |
                  EnterWindowMask | LeaveWindowMask,
                  (LUI_FNCP) button_process, b );

   XMapWindow( LUI_Display, b->window );

   b->x = x;
   b->y = y;
   b->width = width - 2;
   b->height = height - 2;
   b->framewidth = LUI_Border;

   b->toggle = toggle;
   b->label = strdup( label );
   b->state = 0;
   b->highlight = 0;
   b->color = LUI_Color_white /*darkgray*/;  /*black;*/
/* MJK 12.04.98 */
/* 03Feb98  Phil McDonald	default button color is white */
   b->red = b->green = b->blue = 1.0;
#ifdef JOHAN
   b->red = b->green = b->blue = 0.3;
#endif
   b->callback = NULL;

   LUI_AddWidgetToWindow( parent, b, (LUI_FNCP) LUI_NewButtonDestroy );
   return b;
}



LUI_NEWBUTTON *LUI_PushButtonCreate( Window parent, int x, int y,
                                     int width, int height, char *label )
{
   return new_button( parent, x, y, width, height, label, 0 );
}



LUI_NEWBUTTON *LUI_ToggleButtonCreate( Window parent, int x, int y,
                                       int width, int height, char *label )
{
   return new_button( parent, x, y, width, height, label, 1 );
}



LUI_NEWBUTTON *LUI_CheckButtonCreate( Window parent, int x, int y,
                                      int width, int height, char *label )
{
   return new_button( parent, x, y, width, height, label, 2 );
}


/*
 * Set the "on" color for a button.
 */
void LUI_ButtonColor( LUI_NEWBUTTON *b, double red, double green, double blue )
{
   if (b->color!=LUI_Color_white/*darkgray*/) {
      LUI_FreeColor( b->color );
/*      XFreeColors( LUI_Display, LUI_Colormap, &b->color, 1, 0 );*/
   }
   b->red = red;
   b->green = green;
   b->blue =blue;
   b->color = LUI_AllocateColor( red, green, blue );
   if (b->state) {
      draw_button(b);
   }
}



/*
 * Set the state of a toggle button.  0=off, 1=on
 */
void LUI_ButtonSetState( LUI_NEWBUTTON *b, int state )
{
   if (b->toggle) {
      b->state = state;
      draw_button( b );
   }
}



/*
 * Get the state of a button.
 * Return:  1 = on, 0 = off
 */
int LUI_ButtonGetState( LUI_NEWBUTTON *b )
{
   return b->state;
}


/*
 * Specify the callback function for a button.
 * The callback function should be declared as:
 *            int callback( LUI_NEWBUTTON *b, int state )
 *
 */
void LUI_ButtonCallback( LUI_NEWBUTTON *b,
                         int (*callback)( LUI_NEWBUTTON *, int) )
{
   b->callback = callback;
   b->context_index = context_index;
}



/*
 * Store a user specified index value in a button.
 */
void LUI_ButtonIndex( LUI_NEWBUTTON *b, int index )
{
   b->index = index;
}

void LUI_ButtonContextIndex( LUI_NEWBUTTON *b, int index )
{
   b->context_index = index;
}


