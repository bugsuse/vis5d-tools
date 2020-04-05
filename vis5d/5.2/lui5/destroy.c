/* destroy.c */


/*
 * Maintain a list of widgets inside each window.  When a window is
 * destroyed, automatically call widget destroy functions.
 */


#include <stdio.h>
#include <stdlib.h>
#include <X11/Xlib.h>



struct window_list {
   Window window;
   struct widget_node *head;
   struct window_list *next;
};


struct widget_node {
   void *widget;
   void (*destroy)( void *);
   struct widget_node *next;
};



static struct window_list *List = NULL;


/*
 * Associate a widget to a window so it can be automatically destoyed
 * when the function below is called.
 * Input:  window - the X window ID
 *         widget - pointer to the LUI widget
 *         destroyfunc - pointer to function which will destroy the widget
 */
void LUI_AddWidgetToWindow( Window window, void *widget,
                            void (*destroyfunc)(void *) )
{
   struct window_list *win;
   struct widget_node *wdgt;

   /* allocate a widget_node and initialize it */
   wdgt = (struct widget_node *) malloc( sizeof(struct widget_node) );
   wdgt->widget = widget;
   wdgt->destroy = destroyfunc;

   /* search to see if window is already in list */
   for (win=List;win;win=win->next) {
      if (win->window==window) {
         /* add to head of list */
         wdgt->next = win->head;
         win->head = wdgt;
         return;
      }
   }

   /* window not found! */
   win = (struct window_list *) malloc( sizeof(struct window_list) );
   win->window = window;
   win->next = List;
   List = win;
   win->head = wdgt;
   wdgt->next = NULL;
}




/*
 * Remove a widget from the list attached to the given window but don't
 * destroy the widget!  This function is needed for widgets with child
 * widgets.  See LUI_ButtonMatrixCreate() for example.
 */
void LUI_UnlinkWidgetFromWindow( Window window, void *widget )
{
#ifdef LEAVEOUT
   struct window_list *win;
   struct widget_node *wdgt, *prev;

   return;

   for (win=List;win;win=win->next) {
      if (win->window==window) {
         /* search widget node list */
         prev = NULL;
         for (wdgt=win->head; wdgt; wdgt=wdgt->next) {
            if (wdgt->widget==widget) {
               /* found it! */
               if (prev) {
                  prev->next = wdgt->next;
               }
               else {
                  win->head = wdgt->next;
               }
               free( wdgt );
               return;
            }
            prev = wdgt;
         }
      }
   }
   /* not found */
#endif
}




/*
 * Destroy all the widgets attached to the given window.
 */
void LUI_DestroyWidgetsInWindow( Window window )
{
   struct window_list *win, *prev;

   prev = NULL;
   for (win=List;win;win=win->next) {
      if (win->window==window) {
         /* Found the window!  Free all widgets in the list */
         struct widget_node *wdgt, *next;
         for (wdgt=win->head; wdgt; wdgt=next) {
            next = wdgt->next;
            (*wdgt->destroy)(wdgt->widget);
            free(wdgt);
         }
         /* Free the window_list node */
         if (prev) {
            prev->next = win->next;
         }
         else {
            List = win->next;
         }
         free( win );
         return;
      }
      else {
         prev = win;
      }
   }
   /* window not found! do nothing */
}
