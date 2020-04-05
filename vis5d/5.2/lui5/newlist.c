/* list.c */


/*
 * List Widget - a scrollable window of text strings in which one or more
 * can be selected by clicking on them with the mouse.
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lui.h"
#include "list.h"
#include "newlist.h"



/*
 * Redraw a single entry in the list
 */
static void draw_entry( LUI_NEWLIST *list, int entry )
{
   char *str;
   int len;
   GC back_gc, text_gc;
   int row;
   int x, y;

   row = entry - list->topstring;
   if (row>=0 && row<list->rows) {

      x = LUI_Border;
      y = LUI_Border + row * FONT_HEIGHT;

      if (entry < list->numstrings) {
         if (list->select_flags[entry]) {
            /* draw as highlighted */
            back_gc = LUI_GC_black;
            text_gc = LUI_GC_white;
         }
         else {
            /* draw normal */
            back_gc = LUI_GC_gray;
            text_gc = LUI_GC_black;
         }

         XFillRectangle( LUI_Display, list->window, back_gc,
                         x, y, list->width-2*LUI_Border, FONT_HEIGHT );

         /* Don't worry about clipping text, X will take care of that */
         str = list->strings[ entry ];
         len = strlen( str );
         XDrawString( LUI_Display, list->window, text_gc,
                      x-list->leftoffset, y+LUI_Font_yoff, str, len );
      }
      else {
         /* just redraw background */
         XFillRectangle( LUI_Display, list->window, LUI_GC_gray,
                         x, y, list->width-2*LUI_Border, FONT_HEIGHT );
      }
   }
}


/*
 * Redraw whole list.
 */
static void draw_list( LUI_NEWLIST *list )
{
   int i;

   for (i=0;i<list->rows;i++) {
      draw_entry( list, list->topstring+i );
   }

   /* draw the frame */
   LUI_DrawFrame( list->window, 0, 0, list->width, list->height, LUI_Border,1);
}




/*
 * Load an array of strings into the list widget.  Then, redraw the widget.
 * Input:  list - the list widget;
 *         num - number of strings
 *         strings - array of pointers to character arrays (like main's argv)
 *         free_flag - 1=free strings when finished, 0=never free strings
 */
void LUI_NEWListLoad( LUI_NEWLIST *list, int num, char **strings, int free_flag )
{
   float size;
   int i;

   if (!list) {
      printf("Error in LUI_NEWListLoad:  NULL list argument\n");
      return;
   }

   /* deallocate old list */
   if (list->free_flag && list->strings) {
      for (i=0;i<list->numstrings;i++) {
         free( list->strings[i] );
      }
      free( list->strings );
   }
   if (list->select_flags) {
      free( list->select_flags );
      list->select_flags = NULL;
   }


   /* store new strings */
   list->free_flag = free_flag;
   list->strings = strings;
   list->numstrings = num;
   if (num>0) {
      list->select_flags = (int *) calloc( num, sizeof(int) );
   }
   list->topstring = 0;

   /* find width, in pixels, of longest line */
   list->maxwidth = 0;
   for (i=0;i<num;i++) {
      int dir, ascent, descent;
      XCharStruct overall;
      XTextExtents( LUI_Font, strings[i], strlen(strings[i]),
                    &dir, &ascent, &descent, &overall );
      if (overall.width > list->maxwidth) {
         list->maxwidth = overall.width;
      }
   }

   draw_list( list );

   /* vertical scrollbar */
   if (num==0) {
      size = 100.0;
   }
   else {
      size = 100.0 * (float) list->rows / (float) num;
      if (size>100.0) {
         size = 100.0;
      }
   }
   LUI_ScrollBarSet( list->vsb, size, 0.0 );

   /* horizontal scrollbar */
   if (list->hsb) {
      if (list->maxwidth==0) {
         size = 100.0;
      }
      else {
         size = 100.0 * (float) list->columns / (float) list->maxwidth;
         if (size>100.0) {
            size = 100.0;
         }
      }
      LUI_ScrollBarSet( list->hsb, size, 0.0 );
   }
}



/*
 * Deallocate the strings in a list widget.  The strings *must* have been
 * originally been allocated with malloc()!
 */
void LUI_NEWListUnload( LUI_NEWLIST *list )
{
   if (list->free_flag && list->strings) {
      int i;
      for (i=0;i<list->numstrings;i++) {
         free( list->strings[i] );
      }
      free( list->strings );
      free( list->select_flags );
      list->strings = NULL;
      list->select_flags = NULL;
      list->numstrings = 0;

      draw_list( list );
      LUI_ScrollBarSet( list->vsb, 100.0, 0.0 );
   }
}





/*
 * Return the selection status of an entry in the list.
 * Input:  list - which list widget
 *         entry - which entry, 0 being the first
 * Return:  1 = selected, 0 = unselected.
 */
int LUI_NEWListGetStatus( LUI_NEWLIST *list, int entry )
{
   if (entry>=0 && entry<list->numstrings) {
      return list->select_flags[entry];
   }
   else {
      return 0;
   }
}



/*
 * Set the selection status of an entry in the list.
 * Input:  list - which list widget
 *         entry - which entry, 0 being the first
 *         status - 1 = selected, 0 = unselected
 */
void LUI_NEWListSetStatus( LUI_NEWLIST *list, int entry, int status )
{
   if (entry>=0 && entry<list->numstrings) {
      list->select_flags[entry] = status;
      if (entry>=list->topstring && entry<list->topstring+list->rows) {
         draw_entry( list, entry );
      }
   }
}



/*
 * Set the selection status of all entries in the list.
 * Input:  list - which list widget
 *         status - 1 = selected, 0 = unselected
 */
void LUI_NEWListSetStatusAll( LUI_NEWLIST *list, int status )
{
   int i;

   for (i=0;i<list->numstrings;i++) {
      list->select_flags[i] = status;
      if (i>=list->topstring && i<list->topstring+list->rows) {
         draw_entry( list, i );
      }
   }
}



/*
 * Register a callback function to call when the user (un)selects an
 * entry in the list.
 * Input:  list - which list
 *         callback - the callback function.  It should be declared as:
 *                      int callback( LUI_NEWLIST *list, int entry, int state )
 */
void LUI_NEWListCallback( LUI_NEWLIST *list, int (*callback)() )
{
   list->callback = callback;
   list->context_index = context_index;
}



/*
 * When an X event occurs in the list window, this function will be called.
 * Input:  list - which list widget
 *         event - the X event
 */
static int list_process( LUI_NEWLIST *list, XEvent *event )
{
   static int prev_entry = -1;
   static int first_entry = -1;

   XSynchronize(LUI_Display, 1);
   switch (event->type) {
      case Expose:
         draw_list( list );
         break;

      case ButtonPress:
         {
            /* main window */
            int row, i;
            row = (event->xbutton.y - LUI_Border) / FONT_HEIGHT;
            i = list->topstring + row;
            if (i<list->numstrings) {
               list->button_down = 1;
               /* toggle status */
               list->select_flags[i] = !list->select_flags[i];
               prev_entry = i;
               first_entry = i;
               draw_entry( list, i );
               XSync( LUI_Display, 0 );
               if (list->callback) {
                  (*list->callback)( list, i, list->select_flags[i] );
               }
            }
         }
         break;

      case MotionNotify:
         {
            /* main window */
            int row, i;
            XEvent ev;
            row = (event->xmotion.y - LUI_Border) / FONT_HEIGHT;
            if ((row <0 || row >= list->rows) && list->button_down){
               while( list->button_down && (row >= list->rows || row < 0)){

                  /* move it first */
                  if (row<0 && list->topstring-1 >= 0){
                     list->topstring--;
                     list->select_flags[list->topstring] = list->select_flags[first_entry];
                     if (list->callback) {
                        (*list->callback)( list, list->topstring,
                                             list->select_flags[list->topstring]);
                     }
                     draw_list(list);
                     LUI_ScrollBarSetPos( list->vsb,
                        100.0 * list->topstring / (list->numstrings-list->rows) );
                     }
                  else if (row>=list->rows && list->topstring < list->numstrings-list->rows){
                     list->topstring++;
                     list->select_flags[list->topstring+list->rows-1] =
                                          list->select_flags[first_entry];
                     if (list->callback) {
                        (*list->callback)( list, list->topstring+list->rows-1,
                                           list->select_flags[list->topstring+list->rows-1]);
                     }
                     draw_list(list);                  
                     LUI_ScrollBarSetPos( list->vsb,                  
                        100.0 * list->topstring / (list->numstrings-list->rows) );
                  }

                  /*do some checking */
                  if (XPending(LUI_Display)) {
                     XNextEvent(LUI_Display, &ev);
                     if (ev.type == ButtonRelease || ev.type == LeaveNotify){
                        list->button_down = 0;
                     }
                     else if (ev.type == MotionNotify){
                        row = (ev.xmotion.y - LUI_Border) / FONT_HEIGHT;
                     }
                  }
               }                  
            }
            else if (row>=0 && row<list->rows) {
               i = list->topstring + row;
               if (i>=0 && i<list->numstrings && i!=prev_entry) {
                  list->select_flags[i] = list->select_flags[first_entry];
                  draw_entry( list, i );
                  if (list->callback) {
                     (*list->callback)( list, i, list->select_flags[i] );
                  }
                  prev_entry = i;
               }
            }
/*
                  int j;
                  if (i>first_entry){
                     for (j=first_entry+1; j<=i; j++){

                        list->select_flags[j] = list->select_flags[first_entry];
                        draw_entry( list, j );
                        if (list->callback) { 
                           (*list->callback)( list, j, list->select_flags[j] );
                        }
                     }
                  }
                  else {
                     for (j=first_entry-1;j>=i;j--) {
                        list->select_flags[j] = list->select_flags[first_entry];
                        draw_entry( list, j );
                        if (list->callback) {
                           (*list->callback)( list, j, list->select_flags[j] );
                        }
                     }
                  }
                  prev_entry = i;
               }
            }
*/

         }
 
         break;

      case ButtonRelease:
         prev_entry = -1;
         list->button_down = 0;
         break;

      case EnterNotify:
         break;
      case LeaveNotify:
         break;

      default:
         printf("Error in list_process:  unexpected event (%d)\n",
                (int) event->type );
   }
   return 1;
}



/*
 * Callback for the vertical slider, when it moves this function is called.
 */
static int vscroll_cb( LUI_SCROLLBAR *sb, float pos )
{
   int newtop;
   LUI_NEWLIST *list;

   list = (LUI_NEWLIST *) sb->userdata;

   newtop = (list->numstrings - list->rows) * pos / 100.0;
   if (newtop != list->topstring) {
      list->topstring = newtop;
      draw_list( list );
   }
   return 1;
}


/*
 * Callback for the horizontal slider, when it moves this function is called.
 */
static int hscroll_cb( LUI_SCROLLBAR *sb, float pos )
{
   int newleft;
   LUI_NEWLIST *list;

   list = (LUI_NEWLIST *) sb->userdata;

   newleft = (list->maxwidth - list->columns) * pos / 100.0;
   if (newleft != list->leftoffset) {
      list->leftoffset = newleft;
      draw_list( list );
   }
   return 1;
}



/*
 * Create a new list widget.
 * Input:  parent - parent window ID
 *         x, y, location of list widget with respect to parent in pixels
 *         width, height - outside dimensions of list widget in pixels
 *                         including the scroll bar.
 *         hscroll - include a horizontal scrollbar?
 * Return:  LUI_NEWLIST pointer or NULL if error.
 */
LUI_NEWLIST *LUI_NEWListCreate( Window parent, int x, int y, int width, int height,
                          int hscroll )
{
   LUI_NEWLIST *list;

   LUI_LayoutCheck( &x, &y, &width, &height );

   list = (LUI_NEWLIST *) malloc( sizeof(LUI_NEWLIST) );
   if (!list) {
      return NULL;
   }

   width -= (SLIDER_WIDTH + 2 * LUI_Border);
   if (hscroll) {
      height -= (SLIDER_WIDTH + 2 * LUI_Border);
   }

   list->x = x;
   list->y = y;
   list->width = width;
   list->height = height - 2;
   list->window = XCreateSimpleWindow( LUI_Display, parent,
                                       x, y, width, height-2,
                                       1, LUI_Color_black, LUI_Color_gray );
   LUI_EventAdd2( list->window, 
                  ExposureMask | ButtonPressMask | ButtonMotionMask
                  | ButtonReleaseMask,
                  (LUI_FNCP) list_process, list );
   XMapWindow( LUI_Display, list->window );

   list->vsb = LUI_ScrollBarCreate( parent, x + width + 2*LUI_Border, y,
                                    SLIDER_WIDTH, height-2, 1 );
   LUI_ScrollBarData( list->vsb, list );
   LUI_ScrollBarCallback( list->vsb, vscroll_cb );

   if (hscroll) {
      list->hsb = LUI_ScrollBarCreate( parent, x, y + height + 2*LUI_Border,
                                       width, 20, 0 );
      LUI_ScrollBarData( list->hsb, list );
      LUI_ScrollBarCallback( list->hsb, hscroll_cb );
   }
   else {
      list->hsb = NULL;
   }
   list->leftoffset = 0;

   list->strings = NULL;
   list->select_flags = NULL;
   list->numstrings = 0;
   list->rows = (list->height-2*LUI_Border) / FONT_HEIGHT;
   list->columns = list->width-2*LUI_Border;
   list->topstring = 0;

   return list;
}



/*
 * Destroy the given list widget.
 */
void LUI_NEWListDestroy( LUI_NEWLIST *list )
{
   if (!list) {
      printf("Error in LUI_NEWListDestroy:  NULL list argument\n");
      return;
   }

   if (list->select_flags) {
      free( list->select_flags );
   }
   if (list->window) {
      LUI_EventRemove( list->window );
      XDestroyWindow( LUI_Display, list->window );
   }
   free( list );
}



