/* layout.c */



/*
 * Automatic widget layout helper.
 */


#include "layout.h"

static int LastX, LastY;
static int LastWidth, LastHeight;

static int Gutter = 5;



/*
 * Set gutter (space between widgets) in pixels.
 */
void LUI_LayoutGutter( int g )
{
   Gutter = g;
}


int LUI_LayoutGetGutter( void )
{
   return Gutter;
}



/*
 * Return the value associated with the given layout symbol.
 */
int LUI_LayoutGet( int param )
{
   switch (param) {
      case LUI_NEXT_X:
         return LastX + LastWidth + Gutter;
      case LUI_NEXT_Y:
         return LastY + LastHeight + Gutter;
      case LUI_SAME_X:
         return LastX;
      case LUI_SAME_Y:
         return LastY;
      case LUI_SAME_W:
         return LastWidth;
      case LUI_SAME_H:
         return LastHeight;
      case LUI_LEFT:
         return Gutter;
      case LUI_TOP:
         return Gutter;
      default:
         return 0;
   }
}




/*
 * Check if user position/size arguments specify auto placement.  If so,
 * compute real arguments.
 */
void LUI_LayoutCheck( int *x, int *y, int *width, int *height )
{

   if (*x==LUI_NEXT_X) {
      *x = LastX + LastWidth + Gutter;
   }
   else if (*x==LUI_SAME_X) {
      *x = LastX;
   }
   else if (*x==LUI_LEFT) {
      *x = Gutter;
   }

   if (*y==LUI_NEXT_Y) {
      *y = LastY + LastHeight + Gutter;
   }
   else if (*y==LUI_SAME_Y) {
      *y = LastY;
   }
   else if (*y==LUI_TOP) {
      *y = Gutter;
   }

   if (*width==LUI_SAME_W) {
      *width = LastWidth;
   }

   if (*height==LUI_SAME_H) {
      *height = LastHeight;
   }

   LastX = *x;
   LastY = *y;
   LastWidth = *width;
   LastHeight = *height;

}
