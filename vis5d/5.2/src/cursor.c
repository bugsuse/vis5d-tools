/*  cursor.c */
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


#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/keysym.h>
#include <math.h>
#include "api.h"
#include "gui.h"

#define DOT( a, b )       (a[0]*b[0] + a[1]*b[1] + a[2]*b[2])

#define EPS 0.000001
  

/*
 * Verify that the cursor is in bounds.  If not, put it in bounds.
 */
void check_cursor_in_bounds( int index) /* DISPLAY INDEX */
{
  float row, col, lev;
  float x, y, z;
  int CoordFlag;
  int Nr, Nc, Nl, LowLev, MaxNlVar, WindNl, WindLow;

  vis5d_get_cursor(index, &x, &y, &z);

  vis5d_xyzPRIME_to_gridPRIME(index, -1, -1, x, y, z, &row, &col, &lev);

  CoordFlag = vis5d_graphics_mode(index, VIS5D_GRID_COORDS, VIS5D_GET);

  vis5d_get_sizePRIME(index, &Nr, &Nc, &Nl, &LowLev,   &WindNl, &WindLow);

  /* if cursor position is being printed in grid coordinates, snap */
  /* cursor location to discrete grid positions */
  if (CoordFlag==1) {
    row = (float) (int) (row+0.5);
    col = (float) (int) (col+0.5);
    lev = (float) (int) (lev+0.5);
  }

  if (row<0.0)  row = 0.0;
  else if (row>Nr-1)  row = Nr-1;
  if (col<0.0)  col = 0.0;
  else if (col>Nc-1)  col = Nc-1;
  if (lev<0.0)  lev = 0.0;
  else if (lev>Nl-1)  lev = Nl-1;

  vis5d_gridPRIME_to_xyzPRIME(index, -1, -1, row, col, lev, &x, &y, &z);

  vis5d_set_cursor(index, x, y, z);
  return;
}




/*** cursor_event ********************************************************
   This function receives an input event and tries to use it to update
   the cursor's position.
   Input: event - a copy of the XEvent.
   Returned:  0 = event not used.
              1 = cursor moved.
              2 = middle button pressed.
*************************************************************************/
int cursor_event( int index, XEvent event )
{
   GuiContext gtx = get_gui_gtx(index);
   int dyo, dhowmany, dwhichones[VIS5D_MAX_CONTEXTS];
   float lat, lon, hgt;
   static int pressed3 = 0;
   int result;           /* value returned */
   static int sounding = 0;
   static float zcursor;
   result = 0;

   if (event.type==ButtonPress && event.xbutton.button==Button3) {
      /* Right mouse button has been pressed. */
      pressed3 = 1;
      result = 1;
      sounding = vis5d_graphics_mode(index, VIS5D_SOUND, VIS5D_GET);
      if (sounding)  {
         float cx,cy,s[3], ds[3], a[3], da[3];
         float dsds, dads, dada, sds, ads, sda, ada, t; 
         cx = (float) event.xbutton.x;
         cy = (float) event.xbutton.y;
     
         /* unproject (cx,cy) to line in 3-D  */
          vis5d_unproject(index, cx, cy, a, da);
          vis5d_get_cursor(index, &s[0], &s[1], &s[2]);
          ds[0] = 0;
          ds[1] = 0;
          ds[2] = 1;  
        /* Find the closest point on line s, sa to line a, da */
          dsds = DOT(ds, ds);
          dads = DOT(da, ds);
          dada = DOT(da, da); 
          sds  = DOT(s,  ds);
          ads  = DOT(a,  ds);
          sda  = DOT(s,  da);
          ada  = DOT(a,  da);
          if ((dada*-dsds)-(-dads*dads)==0.000) zcursor = 0;
          else{  
             t = ((dada*(sds-ads))-(dads*(sda-ada)))/((dada*-dsds)-(-dads*dads)); 
             zcursor = s[2] + t*ds[2];
          } 
      } 
       
   }
   else  if (event.type==ButtonPress && event.xbutton.button==Button2) {
      /* middle button pressed */
      result = 2;
   }
   else if (event.type==ButtonRelease && event.xbutton.button==Button3) {
      /* Right mouse button has been released. */
      pressed3 = 0;
      result = 1;
      sounding = 0;
   }

   if ( (event.type==MotionNotify || event.type==ButtonPress) && pressed3 ) {
      float cx, cy, s[3], ds[3], a[3], da[3], dc[3], d;
      float t, x, y, z,tzmax, tzmin, zmin, zmax;
     


      sounding = vis5d_graphics_mode(index, VIS5D_SOUND, VIS5D_GET);
      if (sounding) {
         float dads, ads, sds;
         int Nr, Nc, Nl[MAXVARS], LowLev[MAXVARS], MaxNl, MaxNlVar, WindNl, WindLow;         
         float xmin, xmax, ymin, ymax, zmin, zmax;

         cx = (float) event.xbutton.x;
         cy = (float) event.xbutton.y;
         vis5d_unproject(index, cx, cy, a, da);
      
         /* intersect line a, da with plane z = zcursor  */
         vis5d_get_box_bounds( index, &xmin, &xmax, &ymin,
                               &ymax, &zmin, &zmax);
         if (zcursor < zmin) zcursor = zmin;
         if (zcursor > zmax) zcursor = zmax; 
         if (da[2] == 0.0000) t= 10.0;
         else  t = (zcursor-a[2])/da[2]; 
         x = a[0] + da[0]*t;
         y = a[1] + da[1]*t;
         z = a[2] + da[2]*t;

         if (gtx->group_index > 0){
            vis5d_xyzPRIME_to_geo(index, 0, 0, x, y, z, &lat, &lon, &hgt);
            vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
            for (dyo = 0; dyo < dhowmany; dyo++){
               vis5d_geo_to_xyzPRIME( dwhichones[dyo], 0, 0, lat, lon, hgt,
                                      &x, &y, &z);
               vis5d_set_cursor(dwhichones[dyo], x, y, z);
               check_cursor_in_bounds(dwhichones[dyo]);
            }
         }
         else{ 
            vis5d_set_cursor(index, x, y, z);
            check_cursor_in_bounds(index);
         }
         result = 1;
      }

      
      else{ 
         /* move 3-D cursor */
 
         cx = (float) event.xbutton.x;
         cy = (float) event.xbutton.y;
   
         /* unproject (cx,cy) to line in 3-D */
         vis5d_unproject(index, cx, cy, a, da);
         /* vector from a to cursor pos */
         vis5d_get_cursor(index, &x, &y, &z);
         dc[0] = x - a[0];
         dc[1] = y - a[1];
         dc[2] = z - a[2];
   
         /* find component of dc on da (da already has length 1) */
         d = DOT( dc, da );
   
         /* calculate position of cursor along 'a' */
         x = a[0] + da[0]*d;
         y = a[1] + da[1]*d;
         z = a[2] + da[2]*d;
         if (gtx->group_index > 0){
            vis5d_xyzPRIME_to_geo(index, 0, 0, x, y, z, &lat, &lon, &hgt);
            vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
            for (dyo = 0; dyo < dhowmany; dyo++){
               vis5d_geo_to_xyzPRIME( dwhichones[dyo], 0, 0, lat, lon, hgt,
                                      &x, &y, &z);
               vis5d_set_cursor(dwhichones[dyo], x, y, z);
               check_cursor_in_bounds(dwhichones[dyo]);
            }
         }
         else{
            vis5d_set_cursor(index, x, y, z);
            check_cursor_in_bounds(index);
         }
         result = 1;
      }      
   }

   if (event.type==KeyPress) {
      char buff[10];
      KeySym key;
      XComposeStatus compose;
      int shift_flag;
      float x, y, z;
      float row, col, lev;
      float step;

      XLookupString( &event.xkey, buff, 10, &key, &compose );
      shift_flag = event.xkey.state & ShiftMask;

      vis5d_get_cursor(index, &x, &y, &z);
      vis5d_xyzPRIME_to_gridPRIME( index, 0, 0, x, y, z, &row, &col, &lev );

      if (vis5d_graphics_mode(index, VIS5D_GRID_COORDS, VIS5D_GET)) {
         /* move by whole grid units */
         step = 1.0;
      }
      else {
         /* move by 1/10 grid units */
         step = 0.1;
      }

      switch (key) {
         case XK_Left:
            col -= step;
            result = 1;
            break;
         case XK_Right:
            col += step;
            result = 1;
            break;
         case XK_Up:
            if (shift_flag) {
               lev += step;
            }
            else {
               row -= step;
            }
            result = 1;
            break;
         case XK_Down:
            if (shift_flag) {
               lev -= step;
            }
            else {
               row += step;
            }
            result = 1;
            break;
         default:
            result = 0;
      }

      if (result==1) {
         vis5d_gridPRIME_to_xyzPRIME( index, 0, 0, row, col, lev, &x, &y, &z );
         if (gtx->group_index > 0){
            vis5d_xyzPRIME_to_geo(index, 0, 0, x, y, z, &lat, &lon, &hgt);
            vis5d_get_num_of_dtxs_in_group( gtx->group_index, &dhowmany, dwhichones);
            for (dyo = 0; dyo < dhowmany; dyo++){
               vis5d_geo_to_xyzPRIME( dwhichones[dyo], 0, 0, lat, lon, hgt,
                                      &x, &y, &z);
               vis5d_set_cursor(dwhichones[dyo], x, y, z);
               check_cursor_in_bounds(dwhichones[dyo]);
            }
         }
         else{
            vis5d_set_cursor(index, x, y, z);
            check_cursor_in_bounds(index);
         }
      }
   }

   return result;
}
