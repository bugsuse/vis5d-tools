/* newlabel.c */


/*
 * A new text label widget
 */


#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "lui.h"





static void draw_label( LUI_NEWLABEL *l )
{
   int i, x, y;
   char *str, *ch;
   int len;

   /* draw background */
   XFillRectangle( LUI_Display, l->window, LUI_GC_gray,
                   0, 0, l->width, l->height );

   x = LUI_Border;
   y = LUI_Border + (l->height - 2*LUI_Border - l->lines * LUI_Font_height) / 2
       + LUI_Font_yoff;

   /* draw text strings */
   str = l->label;
   for (i=0;i<l->lines;i++) {
      ch = str;
      len = 0;
      while (*ch!='\n' && *ch!=0) {
         len++;
         ch++;
      }
      XDrawString( LUI_Display, l->window, LUI_GC_black, x, y, str, len );
      y += LUI_Font_height;
      str = ch+1;
   }

}



static int label_process( LUI_NEWLABEL *l, XEvent *event )
{
   switch (event->type) {
      case Expose:
         draw_label( l );
         break;
      default:
         printf("Error in label_process: unexpected event\n");
   }
   return 1;
}



/*
 * Compute how many lines of text are in the label.
 */
static void compute_lines( LUI_NEWLABEL *l )
{
   char *ch;

   l->lines = 0;
   for (ch=l->label;*ch;ch++) {
      if (*ch=='\n')
         l->lines++;
   }
   l->lines++;
}



void LUI_NewLabelDestroy( LUI_NEWLABEL *l )
{
   LUI_EventRemove( l->window );
   XDestroyWindow( LUI_Display, l->window );
   free( l->label );
   free( l );
}



/*
 * Create a new label.  Multi-line text labels can be specified by
 * inserting newline '\n' characters in the label string.
 * Input:  parent - parent window ID
 *         x, y - position of lable with respect to parent.
 *         width, height - size of label window
 *         label - the character string label
 */
LUI_NEWLABEL *LUI_NewLabelCreate( Window parent, int x, int y,
                                  int width, int height, char *label )
{
   LUI_NEWLABEL *l;

   LUI_LayoutCheck( &x, &y, &width, &height );

   l = (LUI_NEWLABEL *) malloc( sizeof(LUI_NEWLABEL) );
   if (!l) {
      return NULL;
   }

   /* Create the X window, the size-2 takes into effect of the 1-pixel */
   /* black border surrounding the window */
   l->window = XCreateSimpleWindow( LUI_Display, parent,
                                    x, y, width, height,
                                    0, LUI_Color_black, LUI_Color_gray );

   LUI_EventAdd2( l->window, ExposureMask, (LUI_FNCP) label_process, l );

   XMapWindow( LUI_Display, l->window );

   l->x = x;
   l->y = y;
   l->width = width;
   l->height = height;

   l->label = strdup( label );
   compute_lines( l );

   LUI_AddWidgetToWindow( parent, l, (LUI_FNCP) LUI_NewLabelDestroy );
   return l;
}


void LUI_NewLabelRefresh( LUI_NEWLABEL *l )
{
   draw_label( l );
}

void LUI_NewLabelChangeText( LUI_NEWLABEL *l, char *label )
{
   if (l->label) {
      free( l->label );
   }
   l->label = strdup( label );
   compute_lines( l );
   draw_label( l );
}


