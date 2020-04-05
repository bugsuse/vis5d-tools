/*  labels.c */

/* Vis5D version 5.2 */

/*
Vis5D system for visualizing five dimensional gridded data sets
Copyright (C) 1990 - 2000 Bill Hibbard, Johan Kellum, Brian Paul,
Dave Santek, and Andre Battaiola.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include <string.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "api.h"
#include "labels.h"
#include "gui.h"





int labeling_event( index, event )
int index;
XEvent event;
{
   static int fudge_x=0, fudge_y=0, id=0, lastbutton=0;

   GuiContext gtx = get_gui_gtx(index);

   if (event.type==ButtonPress && event.xbutton.button==Button1) {
      /* user want to make a new label */
      vis5d_new_label( index, event.xbutton.x, event.xbutton.y, &id );
      lastbutton = Button1;
   }
   else if (event.type==ButtonPress && event.xbutton.button==Button2) {
      /* user wants to move a label. */
      int x = event.xbutton.x;
      int y = event.xbutton.y;
      if (vis5d_find_label( index, &x, &y, &id)==0) {
         /* difference between label position and mouse position: */
         fudge_x = x - event.xbutton.x;
         fudge_y = y - event.xbutton.y;
      }
      else {
         /* didn't click on a label */
         id = 0;
      }
      lastbutton = Button2;
   }
   else if (event.type==MotionNotify && lastbutton==Button2 && id>0) {
      /* user is dragging a label. */
      vis5d_move_label( index, id, event.xbutton.x + fudge_x,
                        event.xbutton.y + fudge_y );
   }
   else if (event.type==ButtonPress && event.xbutton.button==Button3) {
      /* user wants to delete a label. */
      int x = event.xbutton.x;
      int y = event.xbutton.y;
      if (vis5d_find_label( index, &x, &y, &id)==0) {
         vis5d_delete_label( index, id );
      }
      lastbutton = Button3;
   }
   else if (event.type==KeyPress) {
      /* a key has been pressed */
      char buff[10];
      KeySym key;
      XComposeStatus compose;
      if (XLookupString( &event.xkey, buff, 10, &key, &compose )==1) {
         vis5d_edit_label( index, buff[0] );
         return 1;
      }
      else {
         /* function or special key pressed */
         return 0;
      }
   }
   else {
      return 0;
   }

   return 1;
}

