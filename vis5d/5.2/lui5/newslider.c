/* newslider.c */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lui.h"

/* MJK 12.04.98 */
/* 22Sep97  Phil McDonald */
#  include <math.h>


#define X_MARGIN 10
#define Y_MARGIN 4

#define SLIDER_BAR_LEN(s)  ( s->width - 2 * X_MARGIN )


#define LUI_SLIDER_INACTIVE        0
#define LUI_SLIDER_ACTIVE          1

#define LUI_SLIDER_INT             0
#define LUI_SLIDER_FLOAT           1




/* MJK 12.04.98 */
/* 22Sep97  Phil McDonald */
static int make_string( LUI_NEWSLIDER *slider, float value, char *fmt,
                        char *retstr )
{
    if (slider->type == LUI_SLIDER_FLOAT) {
        char tmp[64], *c, *d;

         if (strlen (fmt) > 0)
         {
            sprintf (retstr, fmt, value);
            return strlen(retstr);
         }
#ifdef JOHAN
static int make_string( LUI_NEWSLIDER *slider, float value, char *retstr )
{
    if (slider->type == LUI_SLIDER_FLOAT) {
        char tmp[64], *c, *d;
#endif

        /* changed by BP on 10-15-91 to a format based upon value */
        if (value>=0.01 || value<=-0.01)
           sprintf(tmp, "%7.2f", value);
        else
           sprintf(tmp, "%7.5f", value);
        for (c = tmp; *c == ' '; ++c);

        /* remove trailing 0's - WLH 10-21-91 */
          while((d = strrchr(c, '0')) == (c + strlen(c) - 1) &&
                 *(c + strlen(c) - 2) != '.') *d = 0;
        sprintf(retstr, "%s", c);
    }
    else
      sprintf(retstr, "%d", (int)(value+0.5));

    return(strlen(retstr));
}




static void draw_slider( LUI_NEWSLIDER *s )
{
   XPoint pts[4];
   int x, y, w, h;
   int xc, yc, size;
   char caption[128], valstr[128];

   /* Fill the background and draw border */
   XFillRectangle( LUI_Display, s->window, 
#ifdef OLD
                   /*s->hilite ? LUI_GC_white : */ LUI_GC_gray,
#endif
                   s->hilite ? LUI_GC_highgray : LUI_GC_gray,
                   0, 0, s->width, s->height );

   LUI_DrawFrame( s->window, 0, 0, s->width, s->height,
                  LUI_Border, 1 );


   /* Draw slider bar groove */
   x = X_MARGIN + 6;
   y = Y_MARGIN + LUI_Font_height - 1;
   w = s->width - 2 * X_MARGIN - 12;
   h = 6;

   /* tiny end triangles */
   XFillRectangle(LUI_Display, s->window, LUI_GC_white, x-6, y, x+12, 2*h);
   XFillRectangle(LUI_Display, s->window, LUI_GC_md_grey, x+w, y, 6, 2*h);

   pts[0].x = x-6;    pts[0].y = y;
   pts[1].x = x+w+6;  pts[1].y = y;
   pts[2].x = x+w;    pts[2].y = y+h;
   pts[3].x = x;      pts[3].y = y+h;
   XFillPolygon(LUI_Display, s->window,
                LUI_GC_bottom, pts, 4, Convex, CoordModeOrigin);
   XDrawLines(LUI_Display, s->window, 
              LUI_GC_black, pts, 4, CoordModeOrigin);

   y += h;
   pts[0].x = x;     pts[0].y = y;
   pts[1].x = x+w;   pts[1].y = y;
   pts[2].x = x+w+6; pts[2].y = y+h;
   pts[3].x = x-6;  pts[3].y = y+h;
   XFillPolygon(LUI_Display, s->window, /*sl_grey*/
                LUI_GC_top, pts, 4, Convex, CoordModeOrigin);
   XDrawLines(LUI_Display, s->window, 
              LUI_GC_black, pts, 4, CoordModeOrigin);

   XDrawRectangle(LUI_Display, s->window, LUI_GC_black,
                  x-6, y-6, w+12, 2*h);

   /* Draw slider marker */
   if (s->high==s->low) {
      x = X_MARGIN;
   }
   else {
      x = X_MARGIN + (SLIDER_BAR_LEN(s)) * (s->value-s->low)
          / (s->high-s->low);
   }

   y = Y_MARGIN + LUI_Font_height - 1;
   w = 6;
   h = 6;

   /* changed to red tick by BEP 4-30-93 */
   pts[0].x = x;   pts[0].y = y;
   pts[1].x = x+w; pts[1].y = y+h;
   pts[2].x = x;   pts[2].y = y+h*2;
   pts[3].x = x-w; pts[3].y = y+h;
   XFillPolygon(LUI_Display, s->window, 
                LUI_GC_black, pts, 4, Convex, CoordModeOrigin);

   pts[0].x = x;      pts[0].y = y+3;
   pts[1].x = x+w-3;  pts[1].y = y+h;
   pts[2].x = x;      pts[2].y = y+h*2-3;
   pts[3].x = x-w+3;  pts[3].y = y+h;
   XFillPolygon(LUI_Display, s->window, 
                LUI_GC_red, pts, 4, Convex, CoordModeOrigin);


   yc = Y_MARGIN + LUI_Font_yoff - 1;

/* MJK 12.04.98 */
/* 22Sep97  Phil McDonald */
   {
   char		fmt[20] = "";
   float	range;
   int		ndigits;

   range = (s->high > s->low) ? s->high - s->low : s->low - s->high;
   if (range == 0.0) range = 1.0;
   ndigits = (2.5 - log10 (range));
   if (ndigits < 0) ndigits = 0;
   if (ndigits > 9) ndigits = 9;
   sprintf (fmt, "%%.%df", ndigits);

   /* Label & current value */
   make_string( s, s->value, fmt, valstr );

   sprintf(caption, "%s = %s", s->label, valstr);

   if (s->units != NULL && strlen (s->units) > 0)
   {
      strcat (caption, " ");
      strcat (caption, s->units);
   }

   size = strlen(caption);
   xc = s->width / 2 - XTextWidth(LUI_Font, caption, size) / 2.0 + 0.5;
   XDrawString(LUI_Display, s->window, LUI_GC_black, xc, yc, caption, size);

   /* Low labels */
   size = make_string( s, s->low, fmt, valstr );
   xc = X_MARGIN - 3;
   XDrawString(LUI_Display, s->window, LUI_GC_black, xc, yc, valstr, size);

   /* High label */
   size = make_string( s, s->high, fmt, valstr );
   xc = X_MARGIN + SLIDER_BAR_LEN(s) - XTextWidth(LUI_Font, valstr, size) + 3;
   XDrawString(LUI_Display, s->window, LUI_GC_black, xc, yc, valstr, size);
   }
#ifdef JOHAN
   /* Label & current value */
   make_string( s, s->value, valstr );
   sprintf(caption, "%s = %s", s->label, valstr);
   size = strlen(caption);
   xc = s->width / 2 - XTextWidth(LUI_Font, caption, size) / 2.0 + 0.5;
   XDrawString(LUI_Display, s->window, LUI_GC_black, xc, yc, caption, size);

   /* Low labels */
   size = make_string( s, s->low, valstr );
   xc = X_MARGIN - 3;
   XDrawString(LUI_Display, s->window, LUI_GC_black, xc, yc, valstr, size);

   /* High label */
   size = make_string( s, s->high, valstr );
   xc = X_MARGIN + SLIDER_BAR_LEN(s) - XTextWidth(LUI_Font, valstr, size) + 3;
   XDrawString(LUI_Display, s->window, LUI_GC_black, xc, yc, valstr, size);
#endif
}




static int slider_process( LUI_NEWSLIDER *s, XEvent *event )
{

   switch (event->type) {
      case EnterNotify:
         s->hilite = 1;
         draw_slider( s );
         break;
        
      case LeaveNotify:
         s->hilite = 0;
         draw_slider( s );
         break;

      case Expose:
         draw_slider( s );
         break;

      case ButtonPress:
         s->state = LUI_SLIDER_ACTIVE;
         s->button = event->xbutton.button; /* WLH added 12-12-91 */
        /* fall through to .... */

      case MotionNotify:
         if (s->state == LUI_SLIDER_ACTIVE) {
            float x_in_pixels; /* inside slide bar range */

            if (event->type==ButtonPress) {
               x_in_pixels = event->xbutton.x - X_MARGIN;
            }
            else {
               x_in_pixels = event->xmotion.x - X_MARGIN;
            }
            
            /* clamp value: */
            if (x_in_pixels<0) {
               x_in_pixels = 0;
            }
            else if (x_in_pixels > SLIDER_BAR_LEN(s)) {
               x_in_pixels = SLIDER_BAR_LEN(s);
            }
            
            /* map from slider pixels to application range: */
            s->value = s->low + (s->high-s->low)  *  
                                 x_in_pixels/(SLIDER_BAR_LEN(s));

            if (s->type == LUI_SLIDER_INT) {
               s->value = (int)(s->value + 0.5);
            }

            if (s->old_value != s->value) {
               if (s->callback) {
                  (*(s->callback))(s, s->value);
               }
               draw_slider( s );
            }

            s->old_value = s->value;
         }
         break;
        
      case ButtonRelease:
         s->state = LUI_SLIDER_INACTIVE;
/*         s->hilite = 0;*/

         /* This might cause problems: Added to allow an application to */
         /* only do stuff on a button release                           */

         if (s->callback) {
            (*(s->callback))(s, s->value );
         }

         draw_slider( s );
         break;
   }

   /* Flush remaining motion events in event queue */
   while (QLength(LUI_Display) > 0) {
      XEvent ne;

      XPeekEvent(LUI_Display, &ne);

      /* Don't flush if its a button press or release */
      if (ne.type != MotionNotify) 
        break;

      /* Get the next event */
      XNextEvent(LUI_Display, &ne);
   }

   return 0;
}



void LUI_NewSliderDestroy( LUI_NEWSLIDER *s )
{
   XDestroyWindow( LUI_Display, s->window );
   if (s->label) {
      free( s->label );
   }

/* MJK 12.04.98 */
/* 24Nov97  Phil McDonald */
   if (s->units) free (s->units);

   free( s );
}


LUI_NEWSLIDER *LUI_NewSliderCreate( Window parent, int x, int y, int width )
{
    LUI_NEWSLIDER *s;
    int height;

    height = 36;

    LUI_LayoutCheck( &x, &y, &width, &height );

    s = (LUI_NEWSLIDER *) malloc( sizeof(LUI_NEWSLIDER) );
    if (!s) {
       return NULL;
    }
    
    /* Create the slider window */
    s->window = XCreateSimpleWindow( LUI_Display, parent,
                                    x, y, width-2, height-2,
                                    1, LUI_Color_black, LUI_Color_gray );

    LUI_EventAdd2( s->window, 
                   ExposureMask | ButtonPressMask | ButtonReleaseMask
                   | ButtonMotionMask | EnterWindowMask | LeaveWindowMask,
                  (LUI_FNCP) slider_process, s );

    XMapWindow(LUI_Display, s->window);

    /* Initialize slider */
    s->label = strdup("");
    s->x = x;
    s->y = y;
    s->width = width - 2;
    s->height = height - 2;
    s->state      = LUI_SLIDER_INACTIVE;
    s->type       = LUI_SLIDER_FLOAT;
    s->low = 0.0;
    s->high = 1.0;
    s->value = 0.5;
    s->old_value = 0.5;
    s->index = 0;
    s->callback = NULL;
    s->context_index = context_index;
    s->hilite = 0;

/* MJK 12.04.98 */
/* 22Sep97  Phil McDonald */
    s->units = strdup("");

    LUI_AddWidgetToWindow( parent, s, (LUI_FNCP) LUI_NewSliderDestroy );

    return s;
}



void LUI_NewSliderCallback( LUI_NEWSLIDER *s,
                            int (*callback)( LUI_NEWSLIDER *, float ) )
{
   s->callback = callback;
}


void LUI_NewSliderSetLabel( LUI_NEWSLIDER *s, char *label )
{
   if (s->label) {
      free(s->label);
   }
   s->label = strdup( label );
}


void LUI_NewSliderSetRange( LUI_NEWSLIDER *s, float min, float max )
{
   s->low = min;
   s->high = max;
   draw_slider( s );
}



void LUI_NewSliderSetValue( LUI_NEWSLIDER *s, float value )
{
   s->value = value;
   draw_slider( s );
}



/* MJK 12.04.98 */
/* 22Sep97  Phil McDonald */
void LUI_NewSliderSetUnits( LUI_NEWSLIDER *s, char *units )
{
   if (s->units) {
      free(s->units);
   }
   if (units != NULL){
      s->units = strdup( units );
   }
   else{
      s->units = NULL;
   }

}



void LUI_NewSliderChange( LUI_NEWSLIDER *s, char *label, char *units,
                          float min, float max, float value)
{
   LUI_NewSliderSetLabel( s, label );
   LUI_NewSliderSetUnits( s, units );
   LUI_NewSliderSetRange( s, min, max );

/* 18Feb98  Phil McDonald */
   if (value < min) value = min;
   if (value > max) value = max;

   LUI_NewSliderSetValue( s, value );
}
#ifdef JOHAN
void LUI_NewSliderChange( LUI_NEWSLIDER *s, char *label,
                          float min, float max, float value )
{
   LUI_NewSliderSetLabel( s, label );
   LUI_NewSliderSetRange( s, min, max );
   LUI_NewSliderSetValue( s, value );
}
#endif




