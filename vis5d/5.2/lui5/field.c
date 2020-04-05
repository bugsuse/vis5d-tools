/* field.c */


/*
 * Field Widget - a single line type-in text field.
 */


#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <X11/keysym.h>
#include "lui.h"
#include "field.h"



#define LMARGIN 2


static void copy_string( char *dst, char *src, int max )
{
   int i;

   for (i=0; i<max && (dst[i]=src[i]); i++)
     ;
}



/*
 * Adjust the field's scroll field according to the field width, length of
 * text and cursor position.
 */
static void compute_scroll( LUI_FIELD *field )
{
   if (strlen(field->text) <= field->columns ||
       strlen(field->text) >= field->len_limit) {
      /* whole string is visible, don't scroll */
      field->scroll = 0;
   }
   else {
      /* scroll so cursor is visible */
      if (field->curpos < field->scroll) {
         /* left */
         field->scroll = field->curpos;
      }
      else if (field->curpos-field->scroll > field->columns) {
         /* right */
         field->scroll = field->curpos - field->columns;
      }
      else {
         /* no change */
      }
   }

   if (field->scroll==field->curpos && field->scroll>0) {
      field->scroll -= 1;
   }
}




void draw_field( LUI_FIELD *field )
{
   int x = LUI_Border;
   int y = LUI_Border;
   int texty;

/*
   XFillRectangle( LUI_Display, field->window, LUI_GC_gray,
             x, y, field->width-2*LUI_Border, field->height-2*LUI_Border );
*/
   XFillRectangle( LUI_Display, field->window, LUI_GC_white,
             x, y, field->width-2*LUI_Border, field->height-2*LUI_Border );

   texty = LUI_Border + (field->height - 2*LUI_Border - LUI_Font_height) / 2
           + LUI_Font_yoff;

   assert( field->scroll <= strlen(field->text) );
   XDrawString( LUI_Display, field->window, LUI_GC_black,
                x+LMARGIN, texty,
                field->text + field->scroll,
                strlen(field->text) - field->scroll );

   LUI_DrawFrame( field->window, 0,0, field->width, field->height,
                  LUI_Border, 0 );

   if (field->editing) {
      /* draw cursor */
      int dir, ascent, descent, width, height;
      XCharStruct overall;

      XTextExtents( LUI_Font,
                    field->text + field->scroll,     /* the text */
                    field->curpos - field->scroll,   /* text length */
                    &dir, &ascent, &descent, &overall);
      width = overall.width;
      height = ascent + descent;
      x = LUI_Border + LMARGIN + width;
      y = texty - LUI_Font_yoff - 1;
      XDrawLine( LUI_Display, field->window, LUI_GC_black,
                 x, y, x, y+height+2 );
      XDrawLine( LUI_Display, field->window, LUI_GC_black,
                 x+1, y, x+1, y+height+2 );
      XDrawLine( LUI_Display, field->window, LUI_GC_black,
                 x-1, y, x+2, y );
      XDrawLine( LUI_Display, field->window, LUI_GC_black,
                 x-1, y+height+2, x+2, y+height+2 );
   }
}



/*
 * Set the current text of a field.
 */
void LUI_FieldSetText( LUI_FIELD *field, char *text )
{
   copy_string( field->text, text, MAX_FIELD );
   field->text[MAX_FIELD-1] = 0;
   field->curpos = strlen( field->text );
   compute_scroll( field );
   draw_field( field );
}


/*
 * Set the current text of a field as a double.
 */
void LUI_FieldSetDouble( LUI_FIELD *field, double x )
{
   sprintf( field->text, "%g", x );
   field->curpos = strlen( field->text );
   compute_scroll( field );
   draw_field( field );
}

void LUI_FieldSetNotDouble( LUI_FIELD *field, double x )
{
   sprintf( field->text, "%.3f", x );
   field->curpos = strlen( field->text );
   compute_scroll( field );
   draw_field( field );
}



/*
 * Set the current text of a field as an integer.
 */
void LUI_FieldSetInt( LUI_FIELD *field, int i )
{
   sprintf( field->text, "%d", i );
   field->curpos = strlen( field->text );
   compute_scroll( field );
   draw_field( field );
}



/*
 * Return the current contests of a field as a string.
 */
void LUI_FieldGetText( LUI_FIELD *field, char *text )
{
   copy_string( text, field->text, MAX_FIELD );
}



/*
 * Return the current contents of a field as a double.
 */
double LUI_FieldGetDouble( LUI_FIELD *field )
{
   return atof( field->text );
}



/*
 * Return the current contents of a field as an integer.
 */
int LUI_FieldGetInt( LUI_FIELD *field )
{
   return atoi( field->text );
}



/*
 * Specify the callback for a field widget.  It will be called whenever
 * the field's text has been modified.
 * Input:  field - which field widget
 *         callback - pointer to callback function declared as:
 *                      int callback( LUI_FIELD *field, char *text )
 */
void LUI_FieldCallback( LUI_FIELD *field, int (* callback)() )
{
   field->callback = callback;
   field->context_index = context_index;
}


static void keypress( LUI_FIELD *field, XEvent *event )
{
   KeySym keysym;
   XComposeStatus compose;
   char buffer[100];
   int len, count;

   if (!field->editing)
      return;

   count = XLookupString( &event->xkey, buffer, 100, &keysym, &compose );

   len = strlen( field->text );


   if (keysym==XK_Left) {
      if (field->curpos>0) {
         field->curpos--;
      }
      compute_scroll( field );
   }
   else if (keysym==XK_Right) {
      if (field->curpos<len) {
         field->curpos++;
      }
      compute_scroll( field );
   }
   else if (keysym==XK_BackSpace) {
      if (field->curpos>0) {
         char *ch = field->text + field->curpos - 1;
         while (*ch) {
            *ch = *(ch+1);
            ch++;
         }
         field->curpos--;
         field->modified = 1;
         if (field->scroll>0) {
            field->scroll--;
         }
/*         compute_scroll( field );*/
      }
   }
   else if (keysym==XK_Delete) {
      if (field->curpos < len) {
         char *ch = field->text + field->curpos;
         while (*ch) {
            *ch = *(ch+1);
            ch++;
         }
         field->modified = 1;
         compute_scroll( field );
      }
   }
   else if (keysym==XK_Return) {
/*      field->editing = 0;*/
      field->modified = 0;
      if (field->callback) {
         (*field->callback)( field, field->text );
      }
   }
   else if (keysym==XK_Tab) {
      if (field->warp_to) {
         field->editing = 0;
         field->modified = 0;
         if (field->callback) {
            (*field->callback)( field, field->text );
         }
         XWarpPointer( LUI_Display, None, field->warp_to->window,
                       0,0, 0,0, event->xkey.x, event->xkey.y );
      }
   }
   else if (count==1 && len < field->len_limit) {
      /* insert */
      int i;
/*
      char *ch = field->text + len;
      for (i=0;i<=len-field->curpos;i++) {
         *(ch+1) = *ch;
         ch--;
      }
      *ch = buffer[0];
*/
      for (i=len; i>=field->curpos; i--) {
         field->text[i+1] = field->text[i];
      }
      field->text[field->curpos] = buffer[0];

      field->curpos++;
      field->modified = 1;
      compute_scroll( field );
   }

   draw_field( field );
}


/*
 * This is called when the mouse button is clicked in the field.  Use
 * the value of x (in pixels) to position the cursor.
 */
static void click( LUI_FIELD *field, int x )
{
   int len, pos;

   len = strlen( field->text );

   field->curpos = len;
   for (pos=0;pos<=len;pos++) {
      int dir, ascent, descent, width;
      XCharStruct overall;

      XTextExtents( LUI_Font,
                    field->text + field->scroll, pos,
                    &dir, &ascent, &descent, &overall);
      width = overall.width;
      if (LUI_Border + LMARGIN + width > x) {
         if (pos>0)
           field->curpos = field->scroll + pos - 1;
         else 
           field->curpos = field->scroll + 0;
         break;
      }
   }

   field->editing = 1;
   field->modified = 0;
   draw_field( field );
}



static int field_process( LUI_FIELD *field, XEvent *event )
{

   switch (event->type) {
      case KeyPress:
         keypress( field, event );
         break;

      case ButtonPress:
         click( field, event->xbutton.x );
         break;

      case EnterNotify:
         field->editing = 1;
         field->modified = 0;
         draw_field( field );
         break;

      case LeaveNotify:
         field->editing = 0;
         draw_field( field );
         if (field->modified && field->callback) {
            (*field->callback)( field, field->text );
         }
         field->modified = 0;
         break;

      case Expose:
         draw_field( field );
         break;

      default:
         ;
   }
   return 1;
}



/*
 * Destroy a type-in field.
 */
void LUI_FieldDestroy( LUI_FIELD *field )
{
   LUI_EventRemove( field->window );
   XDestroyWindow( LUI_Display, field->window );
   free( field );
}



LUI_FIELD *LUI_FieldCreate( Window parent, int x, int y,
                            int width, int height )
{
   LUI_FIELD *field;

   LUI_LayoutCheck( &x, &y, &width, &height );

   field = (LUI_FIELD *) malloc( sizeof(LUI_FIELD) );
   if (!field) {
      return NULL;
   }

   field->window = XCreateSimpleWindow( LUI_Display, parent,
                                        x, y, width-2, height-2,
                                        1, LUI_Color_black, LUI_Color_gray );

   LUI_EventAdd2( field->window, 
                  KeyPressMask | ExposureMask | ButtonPressMask
                  | EnterWindowMask | LeaveWindowMask,
                  (LUI_FNCP) field_process, field );

   XMapWindow( LUI_Display, field->window );

   field->x = x;
   field->y = y;
   field->width = width-2;
   field->height = height-2;

   field->text[0] = 0;
   field->curpos = 0;
   field->columns = (width-2) / LUI_Font_width - 1;
   field->scroll = 0;
   field->editing = 0;
   field->modified = 0;
   field->callback = NULL;
   field->warp_to = NULL;
   field->len_limit = 10000;
   LUI_AddWidgetToWindow( parent, field, (LUI_FNCP) LUI_FieldDestroy );
   return field;
}



/*
 * Link two field widgets together so that pressing TAB in one moves the
 * input focus to the next field.
 */
void LUI_FieldLink( LUI_FIELD *from, LUI_FIELD *to )
{
   from->warp_to = to;
}



