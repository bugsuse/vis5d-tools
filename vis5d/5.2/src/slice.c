/*  slice.c */

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

/* Functions for selecting & moving 2-D slices. */


#include <stdio.h>
#include <math.h>
#include "api.h"
#include "gui.h"
#include "globals.h"


/* useful macros: */

#define CROSS( c, a, b )  { c[0] =  a[1]*b[2]-a[2]*b[1]; \
                            c[1] = -a[0]*b[2]+a[2]*b[0]; \
                            c[2] =  a[0]*b[1]-a[1]*b[0]; \
                          }

#define MAGNITUDE( a )    sqrt( a[0]*a[0] + a[1]*a[1] + a[2]*a[2] )

#define CLAMP(VAL,MIN,MAX)   ( (VAL<MIN) ? MIN : ((VAL>MAX) ? MAX : VAL) )


#define DISTANCE( x1, y1, x2, y2 )   sqrt( (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) )



/* for horizontal slices:
     corner 0 = North East
     corner 1 = South East
     corner 2 = South West
     corner 3 = North West
*/
static void get_col_from_lon( int dindex, float lon, int type, float *col);
static void get_row_from_lat(int dindex, float lat, int type, float *row);





/*
 * Calculate the distance from the cursor to the nearest corner of
 * the given hslice.
 * Input:  curx, cury - cursor coords in pixels
 *         time, var - which timestep and variable
 *         level - position of slice in grid coords [0..Nl-1].
 * Output:  corner - number of nearest corner in [0..3]
 * Returned:  distance from cursor to corner.
 */
static float distance_to_hslice( int index, int curx, int cury,
                                 int time, int var, float level, int *corner )
{
   float cx, cy, px, py;
   float dist, neardist, p[3];
   float r, c;
   float flevel = (float) level;
   int i;
   int Nr, Nc, Nl, LowLev, MaxNlVar, WindNl, WindLow;
   /** INDEX = display index **/
 
   vis5d_get_sizePRIME(index, &Nr, &Nc, &Nl, &LowLev,  &WindNl, &WindLow);

   neardist = 1000000.0;  /* any large value */

   cx = (float) curx;
   cy = (float) cury;

   /* Find distance to each corner of the slice */
   for (i=0;i<4;i++) {
      switch (i) {
         case 0:  r = 0.0;  c = (float) (Nc-1);  break;
         case 1:  r = (float) (Nr-1);  c = (float) (Nc-1);  break;
         case 2:  r = (float) (Nr-1);  c = 0.0;  break;
         case 3:  r = 0.0;  c = 0.0;  break;
      }
      vis5d_gridPRIME_to_xyzPRIME(index, time, var, r, c,
                                  flevel, &p[0], &p[1], &p[2] );
      vis5d_project(index, p, &px, &py );

      dist = (px-cx)*(px-cx) + (py-cy)*(py-cy);
      if (dist<=neardist) {
         neardist = dist;
         *corner = i;
      }
   }

   return neardist;
}

static float distance_to_hclip( int index, int curx, int cury,
                                int num, float level, int *corner)
{
   float cx, cy, px, py;
   float dist, neardist, p[3];
   float r, c;
   int i;
   int Nr, Nc, Nl, LowLev, MaxNlVar, WindNl, WindLow;

   vis5d_get_sizePRIME(index, &Nr, &Nc, &Nl, &LowLev,  &WindNl, &WindLow);
   neardist = 1000000.0;  /* any large value */

   cx = (float) curx;
   cy = (float) cury;

   /* Find distance to each corner of the plane */
   for (i=0;i<4;i++) {
      switch (i) {
         case 0:  r = 0.0;  c = (float) (Nc-1);  break;
         case 1:  r = (float) (Nr-1);  c = (float) (Nc-1);  break;
         case 2:  r = (float) (Nr-1);  c = 0.0;  break;
         case 3:  r = 0.0;  c = 0.0;  break;
      }
      vis5d_gridPRIME_to_xyzPRIME(index, 0, 0, r, c,
                                  level, &p[0], &p[1], &p[2] );
      vis5d_project(index, p, &px, &py );

      dist = (px-cx)*(px-cx) + (py-cy)*(py-cy);
      if (dist<=neardist) {
         neardist = dist;
         *corner = i;
      }
   }

   return neardist;
}
   
static float distance_to_vclip( int index, int curx, int cury, int num,
                                float r1, float c1, float r2, float c2, int *corner)
{
   float cx, cy, px, py;   
   float dist, neardist, p[3];   
   float l, r, c;   
   int curved;
   int i;   
   int Nr, Nc, Nl, LowLev, MaxNlVar, WindNl, WindLow;

   vis5d_get_sizePRIME(index, &Nr, &Nc, &Nl, &LowLev,  &WindNl, &WindLow);
   neardist = 1000000.0;  /* any large value */

   cx = (float) curx;
   cy = (float) cury;

   /* find nearest corner */
   for (i=0;i<4;i++) {
      switch (i) {
          case 0:  r = r1;  c = c1;  l = (float) (Nl-1+LowLev);  break;
          case 1:  r = r2;  c = c2;  l = (float) (Nl-1+LowLev);  break;
          case 2:  r = r1;  c = c1;  l = (float) LowLev;  break;
          case 3:  r = r2;  c = c2;  l = (float) LowLev;  break;
      }
      /* convert r,c,l to graphics coord p */
      vis5d_gridPRIME_to_xyzPRIME(index, 0,0, r, c, l, &p[0], &p[1], &p[2] );
      vis5d_project(index, p, &px, &py );

      dist = (px-cx)*(px-cx) + (py-cy)*(py-cy);
      if (dist<neardist) {
         neardist = dist;
         *corner = i;
      }
   }

   vis5d_get_curved(index, &curved);
   if (curved==0) {
      /* try to find a closer top or bottom edge midpoint */
      for (i=4;i<6;i++) {
         c = ( c1 + c2 ) / 2.0;
         r = ( r1 + r2 ) / 2.0;
         if (i==4) {
            l = (float) (Nl-1+LowLev);
         }
         else {
            l = (float) LowLev;
         }
         vis5d_gridPRIME_to_xyzPRIME(index, 0,0, r, c, l, &p[0], &p[1], &p[2] );
         vis5d_project(index, p, &px, &py );
         dist = (px-cx)*(px-cx) + (py-cy)*(py-cy);
         if (dist<neardist) {
            neardist = dist;
            *corner = i;
         }
      }
   }

   return neardist;
}


/*
 * Calculate the distance from the cursor to the nearest corner of
 * the given vslice.
 * Input:  curx, cuy - cursor coords in pixels
 *         time, var - which timestep and variable
 *         r1,c1,r2,c2 - position of slice in grid coords
 * Output:  corner - number of nearest corner or edge in 0..5
 * Returned:  distance from cursor to corner.
 */
static float distance_to_vslice( int index, int curx, int cury,
                                 int time, int var,
                                 float r1, float c1, float r2, float c2,
                                 int *corner )
{
   float cx, cy;
   float px, py;
   float r, c, l;
   float dist, neardist, p[3];
   int i;
   int curved;
   int Nr, Nc, Nl, LowLev, MaxNlVar, WindNl, WindLow;
   /** INDEX = display index **/
 
   vis5d_get_sizePRIME(index, &Nr, &Nc, &Nl, &LowLev,  &WindNl, &WindLow);

   neardist = 1000000.0;  /* any large value */

   cx = (float) curx;
   cy = (float) cury;

   /* find nearest corner */
   for (i=0;i<4;i++) {
      switch (i) {
          case 0:  r = r1;  c = c1;  l = (float) (Nl-1+LowLev);  break;
          case 1:  r = r2;  c = c2;  l = (float) (Nl-1+LowLev);  break;
          case 2:  r = r1;  c = c1;  l = (float) LowLev;  break;
          case 3:  r = r2;  c = c2;  l = (float) LowLev;  break;
      }
      /* convert r,c,l to graphics coord p */
      vis5d_gridPRIME_to_xyzPRIME(index, time, var, r, c, l, &p[0], &p[1], &p[2] );
      vis5d_project(index, p, &px, &py );

      dist = (px-cx)*(px-cx) + (py-cy)*(py-cy);
      if (dist<neardist) {
         neardist = dist;
         *corner = i;
      }
   }

   vis5d_get_curved(index, &curved);
   if (curved==0) {
      /* try to find a closer top or bottom edge midpoint */
      for (i=4;i<6;i++) {
         c = ( c1 + c2 ) / 2.0;
         r = ( r1 + r2 ) / 2.0;
         if (i==4) {
            l = (float) (Nl-1+LowLev);
         }
         else {
            l = (float) LowLev;
         }
         vis5d_gridPRIME_to_xyzPRIME(index, time, var, r, c, l, &p[0], &p[1], &p[2] );
         vis5d_project(index, p, &px, &py );
         dist = (px-cx)*(px-cx) + (py-cy)*(py-cy);
         if (dist<neardist) {
            neardist = dist;
            *corner = i;
         }
      }
   }

   return neardist;
}

int update_linked_vpos_sliders( int vindex, int dindex, int type, int num, float level)
{
   int *next_vindex, *next_type, *next_var;
   int cur_dindex, cur_vindex, cur_type, cur_var;
   GuiContext gtx = get_gui_gtx(dindex);

   cur_vindex = vindex;
   cur_type = type;
   cur_var = num;

   /**************************************/
   /* loop through all the linked slices */
   /**************************************/
   while(1){
      /**********************************************/
      /* check which slice it is and exit if needed */
      /**********************************************/
      if (gtx->group_index > 0){
         if(!vis5d_get_group_graphic_link(cur_vindex,cur_type,cur_var,
                                          &next_vindex,&next_type,&next_var)){
            return 0;
         }
      }
      else{
         if(!vis5d_get_slice_link(cur_vindex,cur_type,cur_var,
                                  &next_vindex,&next_type,&next_var)){
            return 0;
         }
      }

         
      cur_vindex = *next_vindex;
      cur_type = *next_type;
      cur_var = *next_var;
      if (vindex == cur_vindex && type == cur_type && num == cur_var){
         return 1;
      }
      vis5d_get_ctx_display_index( cur_vindex, &cur_dindex);
      gtx = get_gui_gtx( cur_dindex );
      
      if (type == VIS5D_HSLICE){
         if (gtx->cur_hslice == num){
            mod_vpos_slider( cur_dindex, gtx->hslice_pos_slider, num, 0, level);
         }
      }
      if (type == VIS5D_CHSLICE ){
         if (cb_chvar[cur_dindex] == num){
            mod_vpos_slider( cur_dindex, gtx->chslice_pos_slider, num, 0, level);
         }
      }
      if (type == VIS5D_HWIND){
         if (gtx->cur_hwind == num){
            mod_vpos_slider( cur_dindex, gtx->hwind_pos_slider, num, 1, level);
         }
      }
      if (type == VIS5D_HSTREAM){
         if (gtx->cur_hstream == num){
            mod_vpos_slider( cur_dindex, gtx->hwind_pos_slider, num, 1, level);
         }
      }
   }
   return 1;
}


      




int move_linked_hslices( int vindex, int dindex, int type, int num, float level)
{
   int *next_vindex, *next_type, *next_var;
   int cur_dindex, cur_vindex, cur_type, cur_var;
   int time, curtime, numtimes;
   float interval, low, high, density, scale;
   float hgt, lev, l;
   GuiContext gtx = get_gui_gtx(dindex);


   cur_vindex = vindex;
   cur_type = type;
   cur_var = num;

   vis5d_gridlevelPRIME_to_height( dindex, 0, num, level, &hgt);

   /**************************************/
   /* loop through all the linked slices */
   /**************************************/
   while(1){
      /**********************************************/
      /* check which slice it is and exit if needed */
      /**********************************************/
      if(gtx->group_index > 0){
         if(!vis5d_get_group_graphic_link(cur_vindex,cur_type,cur_var,
                                  &next_vindex,&next_type,&next_var)){
            return 0;
         }
      }
      else{
         if(!vis5d_get_slice_link(cur_vindex,cur_type,cur_var,
                                  &next_vindex,&next_type,&next_var)){
            return 0;
         }
      }

      cur_vindex = *next_vindex;
      cur_type = *next_type;
      cur_var = *next_var;
      if (vindex == cur_vindex && type == cur_type && num == cur_var){
         return 1;
      }
      vis5d_get_ctx_display_index( cur_vindex, &cur_dindex);
      vis5d_height_to_gridlevelPRIME( cur_dindex, 0, cur_var, hgt, &lev);

      /****************************/
      /* get, set, and make slice */
      /****************************/
      if (cur_type == VIS5D_HSLICE){
         vis5d_get_hslice(cur_vindex, cur_var, &interval, &low, &high, &l);
         vis5d_set_hslice(cur_vindex, cur_var, interval, low, high, lev);
         vis5d_get_ctx_numtimes(cur_vindex, &numtimes);
         vis5d_get_ctx_timestep(cur_vindex, &curtime);
         for (time = 0; time < numtimes; time++){
            vis5d_make_hslice( cur_vindex, time, cur_var, time==curtime);
         }
      }
      else if (cur_type == VIS5D_CHSLICE){
         vis5d_set_chslice(cur_vindex, cur_var, lev);
         vis5d_get_ctx_numtimes(cur_vindex, &numtimes);
         vis5d_get_ctx_timestep(cur_vindex, &curtime);
         for (time = 0; time < numtimes; time++){
            vis5d_make_chslice( cur_vindex, time, cur_var, time==curtime);
         }
      }
      else if (cur_type == VIS5D_HWIND){
         vis5d_get_hwindslice(cur_dindex, cur_var, &density, &scale, &l);
         vis5d_set_hwindslice(cur_dindex, cur_var, density, scale, lev);
         vis5d_get_dtx_numtimes(cur_dindex, &numtimes);
         vis5d_get_dtx_timestep(cur_dindex, &curtime);
         for (time = 0; time < numtimes; time++){
            vis5d_make_hwindslice( cur_dindex, time, cur_var, time==curtime);
         }
      }
      else if (cur_type == VIS5D_HSTREAM){
         vis5d_get_hstreamslice(cur_dindex, cur_var, &density, &l);
         vis5d_set_hstreamslice(cur_dindex, cur_var, density, lev);
         vis5d_get_dtx_numtimes(cur_dindex, &numtimes);
         vis5d_get_dtx_timestep(cur_dindex, &curtime);
         for (time = 0; time < numtimes; time++){
            vis5d_make_hstreamslice( cur_dindex, time, cur_var, time==curtime);
         }
      }
   }
   return 1;
}




       
      

      
int move_linked_vslices( int vindex, int dindex, int type, int num, int corner,
                         float row0, float col0, float row1, float col1)
{
   int *next_vindex, *next_type, *next_var;
   int cur_dindex, cur_vindex, cur_type, cur_var;
   int Nr, Nc, Nl, Nr0, Nc0, Nl0, LowLev, WindNl, WindLow;
   int time, curtime, numtimes;
   float interval, low, high, density, scale;
   float lat1, lat0, lon1, lon0;
   float r0, r1, c0, c1;
   GuiContext gtx = get_gui_gtx(dindex);

   cur_vindex = vindex;
   cur_type = type;
   cur_var = num;

   vis5d_rowcolPRIME_to_latlon( dindex, 0, 0, row0, col0, &lat0, &lon0);
   vis5d_rowcolPRIME_to_latlon( dindex, 0, 0, row1, col1, &lat1, &lon1);
   vis5d_get_sizePRIME( dindex, &Nr0, &Nc0, &Nl0, &LowLev,  &WindNl, &WindLow);

   /**************************************/
   /* loop through all the linked slices */
   /**************************************/
   while(1){
      /**********************************************/
      /* check which slice it is and exit if needed */
      /**********************************************/
      if (gtx->group_index > 0){
         if(!vis5d_get_group_graphic_link(cur_vindex,cur_type,cur_var,
                                          &next_vindex,&next_type,&next_var)){
            return 0;
         }
      }
      else{
         if(!vis5d_get_slice_link(cur_vindex,cur_type,cur_var,
                                  &next_vindex,&next_type,&next_var)){
            return 0;
         }
      }

      cur_vindex = *next_vindex;
      cur_type = *next_type;
      cur_var = *next_var;

      if (vindex == cur_vindex && type == cur_type && num == cur_var){
         return 1;
      }

      vis5d_get_ctx_display_index( cur_vindex, &cur_dindex);
      vis5d_get_sizePRIME( cur_dindex, &Nr, &Nc, &Nl, &LowLev,  &WindNl, &WindLow);

      /**************************************/      
      /* get the appropriate r0, c0, r1, c1 */
      /**************************************/
      if (cur_type == VIS5D_VSLICE){
         vis5d_get_vslice(cur_vindex, cur_var, &interval, &low, &high,
                          &r0, &c0, &r1, &c1);
      }
      else if (cur_type == VIS5D_CVSLICE){
         vis5d_get_cvslice(cur_vindex, cur_var,
                          &r0, &c0, &r1, &c1);
      }
      else if (cur_type == VIS5D_VWIND){
         vis5d_get_vwindslice(cur_dindex, cur_var, &density, &scale,
                          &r0, &c0, &r1, &c1);
      }
      else if (cur_type == VIS5D_VSTREAM){
         vis5d_get_vstreamslice(cur_dindex, cur_var, &density, 
                          &r0, &c0, &r1, &c1);
      }


      /************************************************/
      /* get appropriate row and col from lat and lon */
      /************************************************/  
      if (corner==0 || corner==2){
         if (row0 == 0){
            get_col_from_lon( cur_dindex, lon0, 0, &c0);
            r0 = 0.0;
         }
         else if (col0 == 0){
            get_row_from_lat( cur_dindex, lat0, 0, &r0);
            c0 = 0.0;
         }
         else if (row0 == Nr0-1){
            get_col_from_lon( cur_dindex, lon0, 1,  &c0);
            r0 = (float)(Nr-1);
         }
         else if(col0 == Nc0-1){
            get_row_from_lat( cur_dindex, lat0, 1, &r0);
            c0 = (float)(Nc-1);
         }
      }
      else if(corner==1 || corner==3){
         if (row1 == 0){
            get_col_from_lon( cur_dindex, lon1, 0, &c1);
            r1 = 0.0;
         }
         else if (col1 == 0){
            get_row_from_lat( cur_dindex, lat1, 0, &r1);
            c1 = 0.0;
         }
         else if (row1 == Nr0-1){
            get_col_from_lon( cur_dindex, lon1, 1,  &c1);
            r1 = (float)(Nr-1);
         }
         else if(col1 == Nc0-1){
            get_row_from_lat( cur_dindex, lat1, 1, &r1);
            c1 = (float)(Nc-1);
         }
      }
      else if(corner==4 || corner==5){
         if (row0 == 0){
            get_col_from_lon( cur_dindex, lon0, 0, &c0);
            r0 = 0.0;
         }
         else if (col0 == 0){
            get_row_from_lat( cur_dindex, lat0, 0, &r0);
            c0 = 0.0;
         }
         else if (row0 == Nr0-1){
            get_col_from_lon( cur_dindex, lon0, 1,  &c0);
            r0 = (float)(Nr-1);
         }
         else if(col0 == Nc0-1){
            get_row_from_lat( cur_dindex, lat0, 1, &r0);
            c0 = (float)(Nc-1);
         }
         if (row1 == 0){
            get_col_from_lon( cur_dindex, lon1, 0, &c1);
            r1 = 0.0;
         }
         else if (col1 == 0){
            get_row_from_lat( cur_dindex, lat1, 0, &r1);
            c1 = 0.0;
         }
         else if (row1 == Nr0-1){
            get_col_from_lon( cur_dindex, lon1, 1,  &c1);
            r1 = (float)(Nr-1);
         }
         else if(col1 == Nc0-1){
            get_row_from_lat( cur_dindex, lat1, 1, &r1);
            c1 = (float)(Nc-1);
         }
      }
      else{
         printf("error in linked slice moving\n");
         return 0;
      }


      /*******************************/      
      /* now set and make the slices */
      /*******************************/
      if (cur_type == VIS5D_VSLICE){
         vis5d_set_vslice(cur_vindex, cur_var, interval, low, high,
                          r0, c0, r1, c1);
         vis5d_get_ctx_numtimes(cur_vindex, &numtimes);
         vis5d_get_ctx_timestep( cur_vindex, &curtime);
         for (time = 0; time < numtimes; time++){
            vis5d_make_vslice(cur_vindex, time, cur_var, time==curtime);
         }
      }
      else if (cur_type == VIS5D_CVSLICE){
         vis5d_set_cvslice(cur_vindex, cur_var,
                          r0, c0, r1, c1);
         vis5d_get_ctx_numtimes(cur_vindex, &numtimes);
         vis5d_get_ctx_timestep( cur_vindex, &curtime);
         for (time = 0; time < numtimes; time++){
            vis5d_make_cvslice(cur_vindex, time, cur_var, time==curtime);
         }
      }
      else if (cur_type == VIS5D_VWIND){
         vis5d_set_vwindslice(cur_dindex, cur_var, density, scale,
                          r0, c0, r1, c1);
         vis5d_get_dtx_numtimes(cur_dindex, &numtimes);
         vis5d_get_dtx_timestep(cur_dindex, &curtime);
         for (time = 0; time < numtimes; time++){
            vis5d_make_vwindslice(cur_dindex, time, cur_var, time==curtime);
         }
      }
      else if (cur_type == VIS5D_VSTREAM){
         vis5d_set_vstreamslice(cur_dindex, cur_var, density,
                          r0, c0, r1, c1);
         vis5d_get_dtx_numtimes(cur_dindex, &numtimes);
         vis5d_get_dtx_timestep(cur_dindex, &curtime);
         for (time = 0; time < numtimes; time++){
            vis5d_make_vstreamslice(cur_dindex, time, cur_var, time==curtime);
         }
      }
   }
   return 1;
}






/*
 * Given a cursor position, find the slice to be selected for moving.
 * Input:  curx,cury - the cursor position in pixels (0,0) = upper-left
 *         time - which timestep
 *         contype   - pointer to int
 *         contour   - pointer to int
 *         corner    - pointer to int
 * Output:  contype - either HSLICE, VSLICE, CHSLICE, or CVSLICE.
 *          contour - the number of the nearest contour (which variable)
 *          corner - the number of the nearest corner in [0..3] or
 *                   if a vslice edge midpoint has been selected 4, or 5
 * Returned:  1 - a contour has been identified.
 *            0 - a contour was not identified.
 */
static int find_nearest_slice( int index, int curx, int cury, int time,
                               int *contype, int *contour, int *corner, int *thecontext )
{
   float dist, neardist;
   int var, corn, w;
   int nearctx, yo;
   float interval, low, high, level;
   float row0, col0, row1, col1;
   float density, scale;
   int Uvar, Vvar, Wvar, Uvar2, Vvar2, Wvar2, TrajU, TrajV, TrajW;
   int NumVars;
   int Nr, Nc, Nl[MAXVARS], LowLev[MAXVARS], MaxNl, MaxNlVar, WindNl, WindLow;
   int howmany, whichones[VIS5D_MAX_CONTEXTS];
   int owner[9];
   /** INDEX = display index **/

   vis5d_get_num_of_ctxs_in_display( index, &howmany, whichones);
   neardist = 15.0 * 15.0;  /* 15 pixel radius */
   *contype = *contour = *corner = *thecontext = -1;
   nearctx = -1;
   for (yo = 0; yo < howmany; yo ++){
      int vindex = whichones[yo];

      vis5d_get_ctx_numvars( vindex, &NumVars );
      vis5d_get_size(vindex, &Nr, &Nc, Nl, LowLev, &MaxNl,  &MaxNlVar, &WindNl, &WindLow);

      /* try horizontal contour slices */
      for (var=0;var<NumVars;var++) {
         if (vis5d_enable_graphics(vindex, VIS5D_HSLICE, var, VIS5D_GET)) {
            vis5d_get_hslice(vindex, var, &interval, &low, &high, &level);
            dist = distance_to_hslice(index, curx, cury, time, var, level, &corn );
            if (dist<neardist) {
               *thecontext = vindex;
               neardist = dist;
               *contype = VIS5D_HSLICE;
               *contour = var;
               *corner = corn;
            }
         }
      }

      /* try horizontal color slices */
      for (var=0;var<NumVars;var++) {
         if (vis5d_enable_graphics(vindex, VIS5D_CHSLICE, var, VIS5D_GET)) {
            vis5d_get_chslice(vindex, var, &level);
            dist = distance_to_hslice(index, curx, cury, time, var, level, &corn );
            if (dist<neardist) {
               *thecontext = vindex;
               neardist = dist;
               *contype = VIS5D_CHSLICE;
               *contour = var;
               *corner = corn;
            }
         }
      }

      /* try vertical contour slices */
      for (var=0;var<NumVars;var++) {
         if (vis5d_enable_graphics(vindex, VIS5D_VSLICE, var, VIS5D_GET)) {
            vis5d_get_vslice(vindex, var, &interval,
                                  &low, &high, &row0, &col0, &row1, &col1);
            dist = distance_to_vslice(index, curx, cury,  time, var,
                                      row0, col0, row1, col1, &corn );
            if (dist<neardist) {
               neardist = dist;
               *thecontext = vindex;
               *contype = VIS5D_VSLICE;
               *contour = var;
               *corner = corn;
            }
         }
      }

      /* try vertical colored slices */
      for (var=0;var<NumVars;var++) {
         if (vis5d_enable_graphics(vindex, VIS5D_CVSLICE, var, VIS5D_GET)) {
            vis5d_get_cvslice(vindex, var, &row0, &col0, &row1, &col1);
            dist = distance_to_vslice(index, curx, cury,  time, var,
                                      row0, col0, row1, col1, &corn );
            if (dist<neardist) {
               neardist = dist;
               *contype = VIS5D_CVSLICE;
               *thecontext = vindex;
               *contour = var;
               *corner = corn;
            }
         }
      }

      vis5d_get_wind_vars(index, &owner[0],&Uvar, &owner[1], &Vvar, &owner[2], &Wvar,
                          &owner[3], &Uvar2, &owner[4], &Vvar2, &owner[5], &Wvar2,
                          &owner[6], &TrajU, &owner[7], &TrajV, &owner[8], &TrajW);

      /* try horizontal wind slices */
      for (w=0;w<VIS5D_WIND_SLICES;w++) {
         int wner = w == 0 ? owner[0] : owner[3];
         if (wner == vindex && vis5d_enable_graphics(vindex, VIS5D_HWIND, w, VIS5D_GET)) {
            int uvar = w == 0 ? Uvar : Uvar2;
            vis5d_get_hwindslice(index, w, &density, &scale, &level);
            dist = distance_to_hslice(index, curx, cury, time, uvar, level, &corn );
            if (dist<neardist) {
               neardist = dist;
               *contype = VIS5D_HWIND;
               *thecontext = vindex;
               *contour = w;
               *corner = corn;
            }
         }
      }

      /* try vertical wind slice */
      for (w=0;w<VIS5D_WIND_SLICES;w++) {
         int wner = w == 0 ? owner[0] : owner[3];
         if (wner == vindex && vis5d_enable_graphics(vindex, VIS5D_VWIND, w, VIS5D_GET)) {
            int uvar = w == 0 ? Uvar : Uvar2;
            vis5d_get_vwindslice(index, w, &density, &scale, &row0, &col0, &row1, &col1);
            dist = distance_to_vslice(index, curx, cury, time, uvar,
                                      row0, col0, row1, col1, &corn );
            if (dist<neardist) {
               neardist = dist;
               *contype = VIS5D_VWIND;
               *contour = w;
               *thecontext = vindex;
               *corner = corn;
            }
         }
      }

      /* try horizontal stream slices */
      for (w=0;w<VIS5D_WIND_SLICES;w++) {
         int wner = w == 0 ? owner[0] : owner[3];
         if (wner == vindex && vis5d_enable_graphics(vindex, VIS5D_HSTREAM, w, VIS5D_GET)) {
            int uvar = w == 0 ? Uvar : Uvar2;
            vis5d_get_hstreamslice(index, w, &density, &level);
            dist = distance_to_hslice(index, curx, cury, time, uvar, level, &corn );
            if (dist<neardist) {
               neardist = dist;
               *contype = VIS5D_HSTREAM;
               *contour = w;
               *thecontext = vindex;
               *corner = corn;
            }
         }
      }

      /* try vertical stream slice */
      for (w=0;w<VIS5D_WIND_SLICES;w++) {
         int wner = w == 0 ? owner[0] : owner[3];         
         if (wner == vindex && vis5d_enable_graphics(vindex, VIS5D_VSTREAM, w, VIS5D_GET)) {
            int uvar = w == 0 ? Uvar : Uvar2;
            vis5d_get_vstreamslice(index, w, &density, &row0, &col0, &row1, &col1);
            dist = distance_to_vslice(index, curx, cury, time, uvar,
                                      row0, col0, row1, col1, &corn );
            if (dist<neardist) {
               neardist = dist;
               *contype = VIS5D_VSTREAM;
               *thecontext = vindex;
               *contour = w;
               *corner = corn;
            }
         }
      }
   }
   /** ALL DONE!  contype, contour, corner have the results */
   if (*contype!=-1) {
      return 1;
   }
   else
      return 0;
}

static int find_nearest_clip_slice( int index, int curx, int cury,
                                    int *planetype, int *planenum, int *corner)
{
   float dist, neardist;
   int i, corn, current;
   float level, r1, c1, r2, c2;

   neardist = 15.0 * 15.0;  /* 15 pixel radius */
   *planetype = *planenum = *corner = -1;

   /*try horizontal clipping planes*/
   for (i=0; i < 2; i++){
      vis5d_get_hclip( index, i, &level);
      vis5d_get_clip_mode( index, i, &current);
      if (current==1){
         dist = distance_to_hclip( index, curx, cury, i, level, &corn);
         if (dist < neardist){
            neardist = dist;
            *planetype = 0;
            *planenum = i;
            *corner = corn;
         }
      }
   }
   for (i=0; i < 4; i++){
      vis5d_get_vclip( index, i, &r1, &c1, &r2, &c2);
      vis5d_get_clip_mode( index, i+2, &current);
      if (current==1){
         dist = distance_to_vclip( index, curx, cury, i, r1, c1, r2, c2, &corn);
         if (dist < neardist){
            neardist = dist;
            *planetype = 1;
            *planenum = i;
            *corner = corn;
         }
      }
   }
   if (*planetype != -1){
      return 1;
   }
   else{
      return 0;
   }
}

/*** point_nearest_line ***********************************************
   Given two lines 'a' and 'b' find the point along 'a' which is
   closest to 'b'.  The result is in the form of a parameter 't' along
   line 'a'.
   Input:  a, da - point on 'a' and direction of 'a'
           b, db - point on 'b' and direction of 'b'
   Return:  t, such that a+t*da = the nearest point.
**********************************************************************/
static float point_nearest_line( float a[3], float da[3],
                                 float b[3], float db[3] )
{
   float dc[3], mc;
   float m[3][3], det, t;

   /* find point on 'a' closest to 'b' */
   CROSS( dc, da, db );   /* dc = da X db */
   mc = MAGNITUDE( dc );
   if (mc<=0.0001) {
      /* a is nearly parallel to b */
      return 0.0;
   }
   else {
      m[0][0] = b[0]-a[0];  m[0][1] = b[1]-a[1];  m[0][2] = b[2]-a[2];
      m[1][0] = db[0];      m[1][1] = db[1];      m[1][2] = db[2];
      m[2][0] = dc[0];      m[2][1] = dc[1];      m[2][2] = dc[2];

      det = m[0][0] * (m[1][1]*m[2][2]-m[1][2]*m[2][1])
          - m[0][1] * (m[1][0]*m[2][2]-m[1][2]*m[2][0])
           + m[0][2] * (m[1][0]*m[2][1]-m[1][1]*m[2][0]);

      t = det / (mc*mc);
      return t;
   }
}



/*** move_hslice ******************************************************
   Move a horizontal slice according to the cursor position.
   Input:  curx, cury - cursor position in pixels
           level - pointer to current slice level
           corner - which corner is 'grabbed'
   Output:  level - modified to change position.
   Returned:  0 = no movement
              1 = slice was moved.
**********************************************************************/
static int move_hslice( int index, int curx, int cury,
                        int time, int var,
                        float *level, int corner )
{
   float cx,cy;
   float a[3], da[3];
   float b[3], db[3];
   float t;
   float topl, botl, aa[3];
   float r, c;
   float maxlevel, minlevel;
   int Nr, Nc, Nl, LowLev, MaxNlVar, WindNl, WindLow;
   int vNr, vNc, vNl[MAXVARS], vLowLev[MAXVARS], vMaxNl, vMaxNlVar, vWindNl, vWindLow;

   /** INDEX = vis5d_context index **/ 
   int dindex;

   vis5d_get_ctx_display_index( index, &dindex);
   vis5d_get_sizePRIME(dindex, &Nr, &Nc, &Nl, &LowLev,  &WindNl, &WindLow);
   vis5d_get_size( index, &vNr, &vNc, vNl, vLowLev, &vMaxNl, &vMaxNlVar, &vWindNl, &vWindLow);
   if (vNl[var]==1) {
      /* Special case: hslice of a 2-D grid can be at any level*/
      maxlevel = (float) (Nl-1);
      minlevel = 0.0;
   }
   else {
      maxlevel = (float) (Nl-1+LowLev);
      minlevel = (float) LowLev;
   }
   /* "unproject" cursor position to a line 'b' in 3-D graphics coordinates */
   cx = (float) curx;
   cy = (float) cury;
   vis5d_unproject( dindex, cx, cy, b, db );
   /* let 'a'+'da' be the line corresponding to the edge of the box
    * along which the slice is being dragged.
    */
   switch (corner) {
      case 0:  r = 0.0;  c = (float) (Nc-1);  break;
      case 1:  r = (float) (Nr-1);  c = (float) (Nc-1);  break;
      case 2:  r = (float) (Nr-1);  c = 0.0;  break;
      case 3:  r = 0.0;  c = 0.0;  break;
   }
   topl = maxlevel;
   botl = minlevel;
   vis5d_gridPRIME_to_xyzPRIME(dindex, time, var, r, c, botl, &a[0], &a[1], &a[2] );
   vis5d_gridPRIME_to_xyzPRIME(dindex, time, var, r, c, topl, &aa[0], &aa[1], &aa[2] );
   da[0] = aa[0] - a[0];
   da[1] = aa[1] - a[1];
   da[2] = aa[2] - a[2];


   /* find point on 'a' closest to 'b' */
   t = point_nearest_line( a, da, b, db );
   if (t==0.0) {
      return 0;
   }
   else {
      float x, y, z, row, col;
      /* convert "closest" z value back to a grid level */
      x = a[0] + t * da[0];
      y = a[1] + t * da[1];
      z = a[2] + t * da[2];
      vis5d_xyzPRIME_to_gridPRIME(dindex, time, var, x, y, z, &row, &col, level );
      return 1;
   }

}

static int move_hclip( int index, int num, int curx, int cury,
                       float  *level, int corner)
{
   int Nr, Nc, Nl, LowLev, MaxNlVar, WindNl, WindLow;
   float cx,cy;
   float a[3], da[3];
   float b[3], db[3];
   float t;
   float topl, botl, aa[3];
   float r, c;
   float maxlevel, minlevel;

   vis5d_get_sizePRIME(index, &Nr, &Nc, &Nl, &LowLev,  &WindNl, &WindLow);

   maxlevel = (float) (Nl-1+LowLev);
   minlevel = (float) LowLev;

   /* "unproject" cursor position to a line 'b' in 3-D graphics coordinates */
   cx = (float) curx;
   cy = (float) cury;
   vis5d_unproject( index, cx, cy, b, db );
   /* let 'a'+'da' be the line corresponding to the edge of the box
    * along which the slice is being dragged.
    */
   switch (corner) {
      case 0:  r = 0.0;  c = (float) (Nc-1);  break;
      case 1:  r = (float) (Nr-1);  c = (float) (Nc-1);  break;
      case 2:  r = (float) (Nr-1);  c = 0.0;  break;
      case 3:  r = 0.0;  c = 0.0;  break;
   }
   topl = maxlevel;
   botl = minlevel;
   vis5d_gridPRIME_to_xyzPRIME(index, 0,0, r, c, botl, &a[0], &a[1], &a[2] );
   vis5d_gridPRIME_to_xyzPRIME(index, 0,0, r, c, topl, &aa[0], &aa[1], &aa[2] );
   da[0] = aa[0] - a[0];
   da[1] = aa[1] - a[1];
   da[2] = aa[2] - a[2];
      
   /* find point on 'a' closest to 'b' */
   t = point_nearest_line( a, da, b, db );
   if (t==0.0) {
      return 0;
   }
   else {
      float x, y, z, row, col;
      /* convert "closest" z value back to a grid level */
      x = a[0] + t * da[0];
      y = a[1] + t * da[1];
      z = a[2] + t * da[2];
      vis5d_xyzPRIME_to_gridPRIME(index, 0,0, x, y, z, &row, &col, level );
      return 1;
   }
}

static int move_vclip( int index, int num, int curx, float cury, float *r1, float *c1,
                       float *r2, float *c2, int  corner)
{
   float cx, cy;
   float b[3], db[3];
   float p[3], px, py;
   float nr, nc;
   int Nr, Nc, Nl, LowLev, MaxNlVar, WindNl, WindLow;

   vis5d_get_sizePRIME(index, &Nr, &Nc, &Nl, &LowLev,  &WindNl, &WindLow);

   nr = (float) (Nr-1);
   nc = (float) (Nc-1);

   cx = (float) curx;
   cy = (float) cury;
   vis5d_unproject( index, cx, cy, b, db );

   if (corner<4) {
      /** dragging slice by a corner **/
      float dist, neardist, neart, t, l;
      float newcol, newrow;
      int edge, nearedge;

      if (corner<2) {
         l = (float) (Nl-1+LowLev);
      }
      else {
         l = (float) LowLev;
      }

      /* find the point on one of the top or bottom box edges which */
      /* is closest to the cursor */
      neardist = 10000.0;
      for (edge=0;edge<4;edge++) {
         float r0, c0, r1, c1, a[3], da[3], aa[3];

         switch (edge) {
            case 0:
               /* north */
               r0 = 0.0;  c0 = 0.0;
               r1 = 0.0;  c1 = (float) (Nc-1);
               break;
            case 1:
               /* east */
               r0 = 0.0;  c0 = (float) (Nc-1);
               r1 = (float) (Nr-1);  c1 = c0;
               break;
            case 2:
               /* south */
               r0 = (float) (Nr-1);  c0 = 0.0;
               r1 = (float) (Nr-1);  c1 = (float) (Nc-1);
               break;
            case 3:
               /* west */
               r0 = 0.0;  c0 = 0.0;
               r1 = (float) (Nr-1);  c1 = 0.0;
               break;
         }
         vis5d_gridPRIME_to_xyzPRIME( index, 0,0, r0, c0, l, &a[0], &a[1], &a[2] );
         vis5d_gridPRIME_to_xyzPRIME( index, 0,0, r1, c1, l, &aa[0], &aa[1], &aa[2] );

         da[0] = aa[0] - a[0];
         da[1] = aa[1] - a[1];
         da[2] = aa[2] - a[2];
         t = point_nearest_line( a, da, b, db );
         p[0] = a[0] + da[0]*t;
         p[1] = a[1] + da[1]*t;
         p[2] = a[2] + da[2]*t;
         vis5d_project( index, p, &px, &py );
         dist = sqrt( (px-cx)*(px-cx) + (py-cy)*(py-cy) );
         if (dist==0.0)  return 0;
         if (dist<neardist) {
            neardist = dist;
            nearedge = edge;
            neart = CLAMP( t, 0.0, 1.0 );
         }
      }
      /* compute new row and column for corner of the slice */
      switch (nearedge) {
         case 0:
            /* north */
            newcol = nc*neart;
            newrow = 0.0;
            break;
         case 1:
            /* east */
            newcol = nc;
            newrow = nr*neart;
            break;
         case 2:
            /* south */
            newcol = nc*neart;
            newrow = nr;
            break;
         case 3:
            /* west */
            newcol = 0.0;
            newrow = nr*neart;
            break;
      }

      if (corner%2==0) {
         /* make sure the corners won't be too close */
         if (DISTANCE( *r2, *c2, newrow, newcol) > 2.0) {
            *r1 = newrow;
            *c1 = newcol;
         }
      }
      else {
         /* make sure the corners won't be too close */
         if (DISTANCE( *r1, *c1, newrow, newcol) > 2.0) {
            *r2 = newrow;
            *c2 = newcol;
         }
      }
      return 1;

   }
   else {
      /** dragging slice by midpoint of edge **/

      float prow, pcol, plev, drow, dcol;
      float newr1, newc1, newr2, newc2;
      float x, y, t;


      if (corner==4) {
         /* find intersection 'p', of 'b' with top plane of box */
         vis5d_gridPRIME_to_xyzPRIME(index, 0,0, 1.0, 1.0, (float) (Nl-1+LowLev),
                           &x, &y, &t);
         t = (t - b[2]) / db[2];
      }
      else {
         /* find intersection 'p', of 'b' with bottom plane of box */
         vis5d_gridPRIME_to_xyzPRIME(index, 0,0, 1.0, 1.0, (float) LowLev, &x, &y, &t);
         t = (t - b[2]) / db[2];
      }
      p[0] = b[0] + t*db[0];
      p[1] = b[1] + t*db[1];
      p[2] = b[2] + t*db[2];

      /* convert p to row and columns */
      vis5d_xyzPRIME_to_gridPRIME( index, 0,0, p[0], p[1], p[2], &prow, &pcol, &plev );

      /* let d = 2-D vector parallel to current slice */
      drow = *r2 - *r1;
      dcol = *c2 - *c1;

      if (drow==0.0) {
         /* east/west slice */
         newc1 = *c1;
         newc2 = *c2;
         newr1 = newr2 = prow;
      }
      else if (dcol==0.0) {
         /* north/south slice */
         newr1 = *r1;
         newr2 = *r2;
         newc1 = newc2 = pcol;
      }
      else {
         /* diagonal slice */
         /* recompute r1,c1, r2,c2 such that they fall on the line */
         /* defined by point (prow,pcol) and direction (drow,dcol) */
         t = -prow / drow;
         newr1 = 0.0;
         newc1 = pcol + dcol*t;

         t = (nr-prow) / drow;
         newr2 = Nr-1.0;
         newc2 = pcol + dcol*t;
         /* trim */
         if (newc1<0.0) {
            t = -pcol / dcol;
            newr1 = prow + drow*t;
            newc1 = 0.0;
         }
         if (newc1>(float)(Nc-1)) {
            t = (nc-pcol) / dcol;
            newr1 = prow + drow*t;
            newc1 = (float) (Nc-1);
         }
         if (newc2<0.0) {
            t = -pcol / dcol;
            newr2 = prow + drow*t;
            newc2 = 0.0;
         }
         if (newc2>(float)(Nc-1)) {
            t = (nc-pcol) / dcol;
            newr2 = prow + drow*t;
            newc2 = (float) (Nc-1);
         }
      }

      newc1 = CLAMP( newc1, 0.0, nc );
      newc2 = CLAMP( newc2, 0.0, nc );
      newr1 = CLAMP( newr1, 0.0, nr );
      newr2 = CLAMP( newr2, 0.0, nr );

#ifdef LEAVEOUT
      if ((newc1==0.0 && newc2==0.0) || (newc1==nc && newc2==nc)) {
         if (newr1<nr/2.0)
           newr1 = 0.0;
         else
           newr1 = nr;
         newr2 = nr - newr1;
      }
      if ((newr1==0.0 && newr2==0.0) || (newr1==nc && newr2==nc)) {
         if (newc1<nc/2.0)
           newc1 = 0.0;
         else
           newc1 = nc;
         newc2 = nc - newc1;
      }
#endif
      /* make sure the corners won't be too close */
      if (DISTANCE( newr1, newc1, newr2, newc2 ) > 2.0) {
         if (drow < 0.0){
            *r1 = newr2;
            *c1 = newc2;
            *r2 = newr1;
            *c2 = newc1;
         }
         else{
            *r1 = newr1;
            *c1 = newc1;
            *r2 = newr2;
            *c2 = newc2;
         }
      }
      return 1;
   }
}

/* this will get the top/bottom col for a given lon */
/* type = 0     Top/North edge of box */
/* type = 1     Bottom/South edge of box */
static void get_col_from_lon( int dindex, float lon, int type, float *col)
{
   float projargs[100];
   int proj;
   float longitude, longitude1, longitude2, junk;
 
   *col = 0.0;
   vis5d_get_dtx_projection( dindex, &proj, projargs);
   if (proj == PROJ_GENERIC || proj == PROJ_LINEAR ||
       proj == PROJ_CYLINDRICAL || proj == PROJ_SPHERICAL){
      int Nr, Nc, Nl, LowLev, MaxNlVar, WindNl, WindLow;
      
      vis5d_get_sizePRIME(dindex, &Nr, &Nc, &Nl,
                          &LowLev,  &WindNl, &WindLow);

      vis5d_rowcolPRIME_to_latlon( dindex, 0, 0, 0.0, 0.0,
                                   &junk, &longitude1);
      vis5d_rowcolPRIME_to_latlon( dindex, 0, 0, 0.0, Nc-1,
                                   &junk, &longitude2);
      *col = ((((float)(Nc-1))/
             (longitude2-longitude1))*(lon-longitude1));
   }
   else{
      float num, left, right, countdown, done, mpoint, edge;
      int Nr, Nc, Nl, LowLev, MaxNlVar, WindNl, WindLow;
      vis5d_get_sizePRIME(dindex, &Nr, &Nc, &Nl,
                       &LowLev,  &WindNl, &WindLow);
      /* do it iteratively!! */
      /* north edge */
      left = 0.0;
      right = (float)(Nc-1);
      countdown = 20;
      done = 0;
      if (type==0){
         edge = 0.0;
      }
      else{
         edge = (float)(Nr-1);
      } 
      while (!done && countdown >= 0){
         mpoint = (right + left)/2.0;
         *col = mpoint;
         vis5d_rowcolPRIME_to_latlon( dindex, 0, 0, edge,
                            mpoint, &junk, &longitude);
         if (fabs(lon-longitude) < 0.001){
            done = 1;
         }
         else if (longitude > lon){
            countdown--;
            left = mpoint;
         }
         else{
            countdown--;
            right = mpoint;
         }
      }
   }
}
         
/* type = 0 west edge of box */
/* type = 1 east side of box */                                          
static void get_row_from_lat(int dindex, float lat, int type, float *row)
{
   float projargs[100];
   int proj;
   float latitude, latitude1, latitude2, junk;

   *row = 0.0;
   vis5d_get_dtx_projection( dindex, &proj, projargs);
   if (proj == PROJ_GENERIC || proj == PROJ_LINEAR ||
       proj == PROJ_CYLINDRICAL || proj == PROJ_SPHERICAL){
      int Nr, Nc, Nl, LowLev, MaxNlVar, WindNl, WindLow;

      vis5d_get_sizePRIME(dindex, &Nr, &Nc, &Nl,
                          &LowLev,  &WindNl, &WindLow);

      vis5d_rowcolPRIME_to_latlon( dindex, 0, 0, 0.0, 0.0,
                                   &latitude1, &junk);
      vis5d_rowcolPRIME_to_latlon( dindex, 0, 0, Nr-1, 0.0,
                                   &latitude2, &junk);
      *row = ((((float)(Nr-1))/
             (latitude2-latitude1))*(lat-latitude1));
   }
   else{
      float num, top, bottom, countdown, done, mpoint, edge;
      int Nr, Nc, Nl, LowLev, MaxNlVar, WindNl, WindLow;
      vis5d_get_sizePRIME(dindex, &Nr, &Nc, &Nl,
                       &LowLev,  &WindNl, &WindLow);
      /* do it iteratively!! */
      top = 0.0;
      bottom = (float)(Nr-1);
      countdown = 20;
      done = 0;
      if (type==0){
         edge = 0.0;
      }
      else{
         edge = (float)(Nc-1);
      }
      while (!done && countdown >= 0){
         mpoint = (top+bottom)/2.0;
         *row = mpoint;
         vis5d_rowcolPRIME_to_latlon( dindex, 0, 0, mpoint,
                            edge, &latitude, &junk);
         if (fabs(lat-latitude) < 0.001){
            done = 1;
         }
         else if (latitude > lat ){
            countdown--;
            top = mpoint;
         }
         else{
            countdown--;
            bottom = mpoint;
         }
      }
   }
}





 
/*
 * Move a vertical slice according to the cursor position.
 * Input:  curx, cury - cursor position in pixels
 *         time, var - which timestep and variable.
 *         r1,c1,r2,c2 - pointers to current slice position values
 *         corner - which corner is 'grabbed'
 * Output:  r1,c1,r2,c2 - possibly modified slice position.
 * Returned:  0 = no movement
 *            1 = slice was moved.
 */
static int move_vslice( int index, int curx, int cury,
                        int time, int var,
                        float *r1, float *c1, float *r2, float *c2,
                        int corner )
{
   float cx, cy;
   float b[3], db[3];
   float p[3], px, py;
   float nr, nc;
   int Nr, Nc, Nl, LowLev, MaxNlVar, WindNl, WindLow;
   int vNr, vNc, vNl[MAXVARS], vLowLev[MAXVARS], vMaxNl, vMaxNlVar, vWindNl, vWindLow;

   /** INDEX = v5d_context index **/
   int dindex;

   vis5d_get_ctx_display_index( index, &dindex); 
   vis5d_get_sizePRIME(dindex, &Nr, &Nc, &Nl, &LowLev,  &WindNl, &WindLow);
   vis5d_get_size( index, &vNr, &vNc, vNl, vLowLev, &vMaxNl, &vMaxNlVar, &vWindNl, &vWindLow);

   nr = (float) (Nr-1);
   nc = (float) (Nc-1);

   cx = (float) curx;
   cy = (float) cury;
   vis5d_unproject( dindex, cx, cy, b, db );

   if (corner<4) {
      /** dragging slice by a corner **/
      float dist, neardist, neart, t, l;
      float newcol, newrow;
      int edge, nearedge;

      if (corner<2) {
         l = (float) (Nl-1+LowLev);
      }
      else {
         l = (float) LowLev;
      }

      /* find the point on one of the top or bottom box edges which */
      /* is closest to the cursor */
      neardist = 10000.0;
      for (edge=0;edge<4;edge++) {
         float r0, c0, r1, c1, a[3], da[3], aa[3];

         switch (edge) {
            case 0:
               /* north */
               r0 = 0.0;  c0 = 0.0;
               r1 = 0.0;  c1 = (float) (Nc-1);
               break;
            case 1:
               /* east */
               r0 = 0.0;  c0 = (float) (Nc-1);
               r1 = (float) (Nr-1);  c1 = c0;
               break;
            case 2:
               /* south */
               r0 = (float) (Nr-1);  c0 = 0.0;
               r1 = (float) (Nr-1);  c1 = (float) (Nc-1);
               break;
            case 3:
               /* west */
               r0 = 0.0;  c0 = 0.0;
               r1 = (float) (Nr-1);  c1 = 0.0;
               break;
         }
         vis5d_gridPRIME_to_xyzPRIME( dindex, time, var, r0, c0, l, &a[0], &a[1], &a[2] );
         vis5d_gridPRIME_to_xyzPRIME( dindex, time, var, r1, c1, l, &aa[0], &aa[1], &aa[2] );

         da[0] = aa[0] - a[0];
         da[1] = aa[1] - a[1];
         da[2] = aa[2] - a[2];
         t = point_nearest_line( a, da, b, db );
         p[0] = a[0] + da[0]*t;
         p[1] = a[1] + da[1]*t;
         p[2] = a[2] + da[2]*t;
         vis5d_project( dindex, p, &px, &py );
         dist = sqrt( (px-cx)*(px-cx) + (py-cy)*(py-cy) );
         if (dist==0.0)  return 0;
         if (dist<neardist) {
            neardist = dist;
            nearedge = edge;
            neart = CLAMP( t, 0.0, 1.0 );
         }
      }

      /* compute new row and column for corner of the slice */
      switch (nearedge) {
         case 0:
            /* north */
            newcol = nc*neart;
            newrow = 0.0;
            break;
         case 1:
            /* east */
            newcol = nc;
            newrow = nr*neart;
            break;
         case 2:
            /* south */
            newcol = nc*neart;
            newrow = nr;
            break;
         case 3:
            /* west */
            newcol = 0.0;
            newrow = nr*neart;
            break;
      }

      if (corner%2==0) {
         /* make sure the corners won't be too close */
         if (DISTANCE( *r2, *c2, newrow, newcol) > 2.0) {
            *r1 = newrow;
            *c1 = newcol;
         }
      }
      else {
         /* make sure the corners won't be too close */
         if (DISTANCE( *r1, *c1, newrow, newcol) > 2.0) {
            *r2 = newrow;
            *c2 = newcol;
         }
      }
      return 1;

   }
   else {
      /** dragging slice by midpoint of edge **/

      float prow, pcol, plev, drow, dcol;
      float newr1, newc1, newr2, newc2;
      float x, y, t;


      if (corner==4) {
         /* find intersection 'p', of 'b' with top plane of box */
         vis5d_gridPRIME_to_xyzPRIME(dindex, time, var, 1.0, 1.0, (float) (Nl-1+LowLev),
                           &x, &y, &t);
         t = (t - b[2]) / db[2];
      }
      else {
         /* find intersection 'p', of 'b' with bottom plane of box */
         vis5d_gridPRIME_to_xyzPRIME(dindex, time, var, 1.0, 1.0, (float) LowLev, &x, &y, &t);
         t = (t - b[2]) / db[2];
      }
      p[0] = b[0] + t*db[0];
      p[1] = b[1] + t*db[1];
      p[2] = b[2] + t*db[2];

      /* convert p to row and columns */
      vis5d_xyzPRIME_to_gridPRIME( dindex, time, var, p[0], p[1], p[2], &prow, &pcol, &plev );

      /* let d = 2-D vector parallel to current slice */
      drow = *r2 - *r1;
      dcol = *c2 - *c1;

      if (drow==0.0) {
         /* east/west slice */
         newc1 = *c1;
         newc2 = *c2;
         newr1 = newr2 = prow;
      }
      else if (dcol==0.0) {
         /* north/south slice */
         newr1 = *r1;
         newr2 = *r2;
         newc1 = newc2 = pcol;
      }
      else {
         /* diagonal slice */
         /* recompute r1,c1, r2,c2 such that they fall on the line */
         /* defined by point (prow,pcol) and direction (drow,dcol) */
         t = -prow / drow;
         newr1 = 0.0;
         newc1 = pcol + dcol*t;

         t = (nr-prow) / drow;
         newr2 = Nr-1.0;
         newc2 = pcol + dcol*t;

         /* trim */
         if (newc1<0.0) {
            t = -pcol / dcol;
            newr1 = prow + drow*t;
            newc1 = 0.0;
         }
         if (newc1>(float)(Nc-1)) {
            t = (nc-pcol) / dcol;
            newr1 = prow + drow*t;
            newc1 = (float) (Nc-1);
         }
         if (newc2<0.0) {
            t = -pcol / dcol;
            newr2 = prow + drow*t;
            newc2 = 0.0;
         }
         if (newc2>(float)(Nc-1)) {
            t = (nc-pcol) / dcol;
            newr2 = prow + drow*t;
            newc2 = (float) (Nc-1);
         }

         /* MJK 12.15.98 begin */
         drow = *r1 - *r2;
         if (drow < 0.0) drow = -drow;
         dcol = *c1 - *c2;
         if (dcol < 0.0) dcol = -dcol;
         if (drow >= dcol)
         {
             if (((*r1 < *r2) && (newr1 > newr2)) ||
                 ((*r1 > *r2) && (newr1 < newr2))) drow = -1.0;
         }
         else
         {
             if (((*c1 < *c2) && (newc1 > newc2)) ||
                 ((*c1 > *c2) && (newc1 < newc2))) drow = -1.0;
         }
         if (drow < 0.0)
         {
             x = newr1, newr1 = newr2, newr2 = x;
             y = newc1, newc1 = newc2, newc2 = y;
         }
         /* MJK 12.15.98 end */
      }

      newc1 = CLAMP( newc1, 0.0, nc );
      newc2 = CLAMP( newc2, 0.0, nc );
      newr1 = CLAMP( newr1, 0.0, nr );
      newr2 = CLAMP( newr2, 0.0, nr );
#ifdef LEAVEOUT
      if ((newc1==0.0 && newc2==0.0) || (newc1==nc && newc2==nc)) {
         if (newr1<nr/2.0)
           newr1 = 0.0;
         else
           newr1 = nr;
         newr2 = nr - newr1;
      }
      if ((newr1==0.0 && newr2==0.0) || (newr1==nc && newr2==nc)) {
         if (newc1<nc/2.0)
           newc1 = 0.0;
         else
           newc1 = nc;
         newc2 = nc - newc1;
      }
#endif
      /* make sure the corners won't be too close */
      if (DISTANCE( newr1, newc1, newr2, newc2 ) > 2.0) {
         *r1 = newr1;
         *c1 = newc1;
         *r2 = newr2;
         *c2 = newc2;
      }
      return 1;
   }
}



/*** move_slice ****************************************************
   Move the position of a contour.  This function is called to
   respond to a user action.
   Input:  curx, cury - the cursor position in pixels (0,0) = upper-left.
           dindex     - display context index
           vindex     - vis5d context index
           contype    - the type of contour to move (HSLICE, CVSLICE,etc)
           time, var  - which display timestep and ctx variable 
           corner     - the corner of the contour to use as reference.
           recalc     - non-zero if slice should be recalculated now.
**********************************************************************/
static void move_slice( int dindex, int vindex, int curx, int cury,
                        int contype, int time, int var, int corner,
                        int recalc , int grp_index)
{
   GuiContext gtx = get_gui_gtx(dindex);
   float interval0, low0, high0, level;
   float interval, low, high;
   float row0, col0, row1, col1;
   float density, scale;
   float lat0, lon0, lat1, lon1, hgt;
   float r0, c0, c1, r1;
   float junk, junk1, junk2, junk3, lev;
   int Uvar, Vvar, Wvar, Uvar2, Vvar2, Wvar2, TrajU, TrajV, TrajW;
   int Uvarowner, Vvarowner, Wvarowner, Uvar2owner, Vvar2owner, Wvar2owner;
   int TrajUowner, TrajVowner, TrajWowner;
   int it, numtimes, dpynumtimes, curtime, dpycurtime;
   int dhowmany, dwhichones[VIS5D_MAX_CONTEXTS];
   int chowmany, cwhichones[VIS5D_MAX_CONTEXTS];
   int dyo, cyo, good;
   char aname[20];

   vis5d_get_ctx_numtimes( vindex, &numtimes );
   vis5d_get_dtx_numtimes( dindex, &dpynumtimes );
   vis5d_get_dtx_timestep( dindex, &dpycurtime );
   vis5d_get_ctx_timestep( vindex, &curtime);


   if (contype==VIS5D_HSLICE) {
      vis5d_get_hslice(vindex, var, &interval, &low, &high, &level);
      if (move_hslice( vindex, curx, cury, time, var, &level, corner ) && recalc) {
         vis5d_set_hslice(vindex, var, interval, low, high, level);
         
         /* MJK 12.15.98 */
         update_vpos_slider (vindex, contype, var, level);


         for (it=0;it<numtimes;it++) {
            vis5d_make_hslice(vindex, it, var, it==curtime);
         }
         update_linked_vpos_sliders( vindex, dindex, contype, var, level);
         move_linked_hslices( vindex, dindex, VIS5D_HSLICE, var, level);
      }
   }
   else if (contype==VIS5D_CHSLICE) {
      vis5d_get_chslice(vindex, var, &level);
      if (move_hslice( vindex, curx, cury, time, var, &level, corner ) && recalc) {
         vis5d_set_chslice(vindex, var, level);

         /* MJK 12.15.98 */
         update_vpos_slider (vindex, contype, var, level);


         for (it=0;it<numtimes;it++) {
            vis5d_make_chslice(vindex, it, var, it==curtime);
         }
         update_linked_vpos_sliders( vindex, dindex, contype, var, level);
         move_linked_hslices( vindex, dindex, VIS5D_CHSLICE, var, level);
      }
   }
   else if (contype==VIS5D_VSLICE) {
      vis5d_get_vslice(vindex, var, &interval, &low, &high, &row0, &col0, &row1, &col1);
      if (move_vslice( vindex, curx, cury, time, var, &row0, &col0, &row1, &col1, corner)
          && recalc) {
         vis5d_set_vslice(vindex, var, interval, low, high, row0, col0, row1, col1);
         for (it=0;it<numtimes;it++) {
            vis5d_make_vslice(vindex, it, var, it==curtime);
         }
         move_linked_vslices( vindex, dindex, VIS5D_VSLICE, var, corner,
                              row0, col0, row1, col1);
      }         
   }
   else if (contype==VIS5D_CVSLICE) {
      vis5d_get_cvslice(vindex, var, &row0, &col0, &row1, &col1);
      if (move_vslice( vindex, curx, cury, time, var, &row0, &col0, &row1, &col1, corner)
          && recalc) {
         vis5d_set_cvslice(vindex, var, row0, col0, row1, col1);
         for (it=0;it<numtimes;it++) {
            vis5d_make_cvslice(vindex, it, var, it==curtime);
         }
         move_linked_vslices( vindex, dindex, VIS5D_CVSLICE, var, corner,
                              row0, col0, row1, col1);
      }
   }
   else if (contype==VIS5D_HWIND) {
      int uvar;
      vis5d_get_wind_vars(dindex, &Uvarowner, &Uvar, &Vvarowner, &Vvar, &Wvarowner, &Wvar,
                          &Uvar2owner, &Uvar2, &Vvar2owner, &Vvar2, &Wvar2owner, 
                          &Wvar2, &TrajUowner, &TrajU, &TrajVowner, &TrajV, &TrajWowner, &TrajW);

      uvar = var == 0 ? Uvar : Uvar2;
      vis5d_get_hwindslice(dindex, var, &density, &scale, &level);
      if (move_hslice( vindex, curx, cury, time, uvar, &level, corner) && recalc) {
         vis5d_set_hwindslice(dindex, var, density, scale, level);

         /* MJK 12.15.98 */
         update_vpos_slider (vindex, contype, var, level);

         for (it=0;it<dpynumtimes;it++) {
            vis5d_make_hwindslice(dindex, it, var, it==dpycurtime);
         }
         update_linked_vpos_sliders( vindex, dindex, contype, var, level);
         move_linked_hslices( vindex, dindex, VIS5D_HWIND, var, level);
      }
   }
   else if (contype==VIS5D_VWIND) {
      int uvar;
      vis5d_get_wind_vars(dindex, &Uvarowner, &Uvar, &Vvarowner, &Vvar, &Wvarowner, &Wvar,
                          &Uvar2owner, &Uvar2, &Vvar2owner, &Vvar2, &Wvar2owner, 
                          &Wvar2, &TrajUowner, &TrajU, &TrajVowner, &TrajV, &TrajWowner, &TrajW);

      uvar = var == 0 ? Uvar : Uvar2;
      vis5d_get_vwindslice(dindex, var, &density, &scale, &row0, &col0, &row1, &col1);
      if (move_vslice( vindex, curx, cury, time, uvar,
                       &row0, &col0, &row1, &col1, corner) && recalc) {
         vis5d_set_vwindslice(dindex, var, density, scale, row0, col0, row1, col1);
         for (it=0;it<dpynumtimes;it++) {
            vis5d_make_vwindslice(dindex, it, var, it==dpycurtime);
         }
         move_linked_vslices( vindex, dindex, VIS5D_VWIND, var, corner,        
                              row0, col0, row1, col1);   
      }
   }
   else if (contype==VIS5D_HSTREAM) {
      int uvar;
      vis5d_get_wind_vars(dindex, &Uvarowner, &Uvar, &Vvarowner, &Vvar, &Wvarowner, &Wvar,
                          &Uvar2owner, &Uvar2, &Vvar2owner, &Vvar2, &Wvar2owner, 
                          &Wvar2, &TrajUowner, &TrajU, &TrajVowner, &TrajV, &TrajWowner, &TrajW);

      uvar = var == 0 ? Uvar : Uvar2;
      vis5d_get_hstreamslice(dindex, var, &density, &level);
      if (move_hslice( vindex, curx, cury, time, uvar, &level, corner) && recalc) {
         vis5d_set_hstreamslice(dindex, var, density, level);

         /* MJK 12.15.98 */         
         update_vpos_slider (vindex, contype, var, level);         

         for (it=0;it<dpynumtimes;it++) {
            vis5d_make_hstreamslice(dindex, it, var, it==dpycurtime);
         }
         update_linked_vpos_sliders( vindex, dindex, contype, var, level);
         move_linked_hslices( vindex, dindex, VIS5D_HWIND, var, level);
      }
   }
   else if (contype==VIS5D_VSTREAM) {
      int uvar;
      vis5d_get_wind_vars(dindex, &Uvarowner, &Uvar, &Vvarowner, &Vvar, &Wvarowner, &Wvar,
                          &Uvar2owner, &Uvar2, &Vvar2owner, &Vvar2, &Wvar2owner, 
                          &Wvar2, &TrajUowner, &TrajU, &TrajVowner, &TrajV, &TrajWowner, &TrajW);

      uvar = var == 0 ? Uvar : Uvar2;
      vis5d_get_vstreamslice(dindex, var, &density, &row0, &col0, &row1, &col1);
      if (move_vslice( vindex, curx, cury, time, uvar,
                       &row0, &col0, &row1, &col1, corner) && recalc) {
         vis5d_set_vstreamslice(dindex, var, density, row0, col0, row1, col1);
         for (it=0;it<dpynumtimes;it++) {
            vis5d_make_vstreamslice(dindex, it, var, it==dpycurtime);
         }
         move_linked_vslices( vindex, dindex, VIS5D_VSTREAM, var, corner,                   
                              row0, col0, row1, col1);
      }
   } 
   if (grp_index >0){
      vis5d_invalidate_grp_frames(grp_index);
   }
   else{
      vis5d_invalidate_dtx_frames(dindex);
   }
}

static void move_plane( int index, int curx, int cury, 
                        int planetype, int planenum, int corner)
{
   float level, r1, c1, r2, c2;
   int current;
   /* horiz plane */
   if (planetype==0){
      vis5d_get_hclip( index, planenum, &level);
      vis5d_get_clip_mode( index, planenum, &current);
      if (move_hclip( index, planenum, curx, cury, &level, corner) && current){
         vis5d_set_hclip( index, planenum, level);
      }
   }
   else if (planetype==1){
      vis5d_get_vclip( index, planenum, &r1, &c1, &r2, &c2);
      vis5d_get_clip_mode( index, planenum+2, &current);
      if (move_vclip( index, planenum, curx, cury, &r1, &c1, &r2, &c2, corner) && current){
         vis5d_set_vclip( index, planenum, r1, c1, r2, c2);
      }
   }
   vis5d_invalidate_dtx_frames(index);
}           


/*
 * This function is called whenever an input event occurs in the
 * main viewing window while in 'slice moving mode'.
 * Input:  event - the X event
 *         time - current timestep
 * Return:  1 = event was used to move a slice
 *          0 = event was not used
 */
int slice_event( int index, XEvent event, int time )
{
   int result;           /* value returned */
   static int flag;      /* has a contour been selected? */
   static int contype;    /* the type of contour selected */
   static int contour;    /* the contour selectd */
   static int corner;     /* the contour corner selected */
   static int otherindex = -1; /* the index of the vis5d_ctx */
   static int planetype;
   static int planenum;
   int clipmode;
   int recalc;

   GuiContext gtx = get_gui_gtx(index);
   /* INDEX = display index */

   /* MJK 12.15.98 begin */
   static float prev_r1, prev_c1, prev_r2, prev_c2;


   if ((flag) && (contype == VIS5D_VSLICE))
   {
      float     cur_r1, cur_c1, cur_r2, cur_c2, x1, x2, x3;

      vis5d_get_vslice (otherindex, contour, &x1, &x2, &x3,
                        &cur_r1, &cur_c1, &cur_r2, &cur_c2);
      if ((cur_r1 != prev_r1) || (cur_c1 != prev_c1) ||
          (cur_r2 != prev_r2) || (cur_c2 != prev_c2))
      {
         corner = (corner == 0) ? 1 : (corner == 1) ? 0 : corner;
      }
   }
   /* MJK 12.15.98 end */



   result = 0;
   clipmode = vis5d_graphics_mode(index, VIS5D_CLIP, VIS5D_GET);

   if (event.type==ButtonPress && event.xbutton.button==Button3) {
      /* The right mouse button has been pressed. */
      /* Determine which slice is closest to the current cursor position. */
      if (clipmode){
         flag = find_nearest_clip_slice( index, event.xbutton.x, event.xbutton.y,
                                         &planetype, &planenum, &corner);
         otherindex = 1;
      }
      else{
         flag = find_nearest_slice(index, event.xbutton.x, event.xbutton.y, time,
                                    &contype, &contour, &corner, &otherindex );
      }
      if (otherindex == -1){
         flag = -1;
         result = -1;
      }
      else {
         result = 1;
      }
   }

   if ( (event.type==MotionNotify || event.type==ButtonPress)
       /*&& event.xbutton.button==Button3*/ && flag==1) {
      /* The cursor has been moved while the right mouse button */
      /* is pressed.  Update the position of the slice. */
#ifdef SINGLE_TASK
      /* If we don't have parallelism, recompute only if not animating */
      recalc = (gtx->GoTime==0) ? 1 : 0;
#else
      /* If we have parallelism, recompute whether animating or not */
      recalc = 1;
#endif

      if (clipmode){
         move_plane( index, event.xbutton.x, event.xbutton.y,
                     planetype, planenum, corner);
      }
      else{
         move_slice( index, otherindex, event.xbutton.x, event.xbutton.y,
                  contype, time, contour, corner, recalc, gtx->group_index );
      }

      /* MJK 12.15.98 begin */
      if (contype == VIS5D_VSLICE)
      {
         float  x1, x2, x3;

         vis5d_get_vslice (otherindex, contour, &x1, &x2, &x3,
                           &prev_r1, &prev_c1, &prev_r2, &prev_c2);
      }
      /* MJK 12.15.98 end */


      result = 1;
   }

   if (event.type==ButtonRelease && event.xbutton.button==Button3 &&
       otherindex >= 0) {
      /* The right mouse button has been released. */
      if (clipmode){
         move_plane( index, event.xbutton.x, event.xbutton.y,
                     planetype, planenum, corner);
      }
      else{
         move_slice( index, otherindex, event.xbutton.x, event.xbutton.y,
                     contype, time, contour, corner, 1, gtx->group_index );
      }
      flag = 0;
      result = 1;
   }

   return result;
}


