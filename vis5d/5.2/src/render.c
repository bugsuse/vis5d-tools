/* render.c */


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

#include <assert.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "anim.h"
#include "api.h"
#include "box.h"
#include "matrix.h"
#include "globals.h"
#include "graphics.h"
#include "grid.h"
#include "labels.h"
#include "map.h"
#include "memory.h"
#include "misc.h"
#include "proj.h"
#include "queue.h"
#include "sounding.h"
#include "sync.h"
#include "topo.h"
#include "vis5d.h"
#include "volume.h"
#include "v5d.h"



#define MAX(A,B)  ( (A) > (B) ? (A) : (B) )
#define MIN(A,B)  ( (A) < (B) ? (A) : (B) )

#define ABS(X)  ( (X) < 0.0f ? -(X) : (X) )


#define TICK_SIZE 0.05
#define CLOCK_SEGMENTS  36
#define VERT(Z) (ctx->VerticalSystem==VERT_NONEQUAL_MB ? height_to_pressure(Z) : (Z))
#define VERTPRIME(Z) (dtx->VerticalSystem==VERT_NONEQUAL_MB ? height_to_pressure(Z) : (Z))


/* Vertical spacing between rows of text: (in pixels) */
#define VSPACE 1


/*** float2string *****************************************************
   Convert a float into an ascii string.
**********************************************************************/
/* MJK 12.01.98 */
/* old now
void float2string( float f, char *str )
{
   if (f==0.0 || fabs(f)<0.01)
      strcpy( str, "0.0" );
   else if (f>=100.0 || f<=-100.0)
      sprintf(str, "%d", (int) f );
   else
      sprintf(str, "%4.2f", f );
}
*/

/* MJK 12.01.98 */
void float2string (Display_Context dtx, int icoord, float f, char *str)
{
   float        fmin, fmax, frng;

   if (fabs (f) < 0.01) f = 0.0;

   if (dtx->CoordFlag) {
      switch (icoord) {
         case 0:
            fmin = 1.0;
            fmax = dtx->Nc;
            break;
         case 1:
            fmin = 1.0;
            fmax = dtx->Nr;
            break;
         case 2:
            fmin = 1.0;
            fmax = dtx->MaxNl;
            break;
      }
   }
   else {
      switch (icoord) {
         case 0:
            fmin = dtx->WestBound;
            fmax = dtx->EastBound;
            break;
         case 1:
            fmin = dtx->SouthBound;
            fmax = dtx->NorthBound;
            break;
         case 2:
            fmin = VERTPRIME(dtx->BottomBound);
            fmax = VERTPRIME(dtx->TopBound);
            break;
      }
   }
   frng = (fmin < fmax) ? fmax - fmin : fmin - fmax;

   if (frng < 500.0)
      sprintf (str, "%.2f", f);
   else
      sprintf (str, "%.0f", f);
}

/*** check_view_side *****************************************************
   Determine if the plane of a clockwise series of points faces the camera.
   
   return:      -1      plane faces away from the camera
                 0      plane includes the camera
                 1      plane faces the camera
**********************************************************************/
int check_view_side (Context ctx, int type, int num)
{

    int         iside;
    float       xyz[3][3], xy[3][2], area;



    switch (type)
    {
/* Need to work on non-vertical slices */
        case VSLICE:
            xyz[0][0] = ctx->VSliceX2[num];
            xyz[0][1] = ctx->VSliceY2[num];
            xyz[0][2] = ctx->dpy_ctx->Zmin;
            xyz[1][0] = ctx->VSliceX1[num];
            xyz[1][1] = ctx->VSliceY1[num];
            xyz[1][2] = ctx->dpy_ctx->Zmin;
            xyz[2][0] = ctx->VSliceX1[num];
            xyz[2][1] = ctx->VSliceY1[num];
            xyz[2][2] = ctx->dpy_ctx->Zmax;
            break;

        default:
            return 0;
    }


    project (&xyz[0][0], &xy[0][0], &xy[0][1]);
    project (&xyz[1][0], &xy[1][0], &xy[1][1]);
    project (&xyz[2][0], &xy[2][0], &xy[2][1]);

    area = ((xy[0][0] - xy[2][0]) * (xy[0][1] + xy[2][1])) +
           ((xy[1][0] - xy[0][0]) * (xy[1][1] + xy[0][1])) +
           ((xy[2][0] - xy[1][0]) * (xy[2][1] + xy[1][1]));

    iside = (area > 0.0) ? -1 : (area < 0.0) ? 1 : 0;


    return iside;
}


int flip_vslice_end_for_end (Context ctx, int time, int var)
{
   float        x;

   x = ctx->VSliceR1[var];
   ctx->VSliceR1[var] = ctx->VSliceR2[var];
   ctx->VSliceR2[var] = x;
   x = ctx->VSliceC1[var];
   ctx->VSliceC1[var] = ctx->VSliceC2[var];
   ctx->VSliceC2[var] = x;
   x = ctx->VSliceX1[var];
   ctx->VSliceX1[var] = ctx->VSliceX2[var];
   ctx->VSliceX2[var] = x;
   x = ctx->VSliceY1[var];
   ctx->VSliceY1[var] = ctx->VSliceY2[var];
   ctx->VSliceY2[var] = x;
   x = ctx->VSliceLat1[var];
   ctx->VSliceLat1[var] = ctx->VSliceLat2[var];
   ctx->VSliceLat2[var] = x;
   x = ctx->VSliceLon1[var];
   ctx->VSliceLon1[var] = ctx->VSliceLon2[var];
   ctx->VSliceLon2[var] = x;

   request_vslice (ctx, time, var, (time == ctx->CurTime));
   return 0;
}




/* same code as in uvwwidget.c */
char *return_var_plus_index( char *varname, int index )
{
   int yo;
/* WLH 20 Oct 98
   char whole[40];
*/
   /* WLH 20 Oct 98 */
   char* whole;

   char num[40];
   int length;

   /* WLH 20 Oct 98 */
   whole = (char *) malloc(40);

   if (index <0 || varname[0] == 0){
      whole[0] = 0;
      return whole;
   }
   for (yo = 0; yo < 17; yo++){
      if (varname[yo] == '\0' || varname[yo] == ' '){
         yo -=1;
         whole[yo+1] = '.';
         sprintf(num, "%d\n", index );
         if (index > 99 && yo < 15){
            whole[yo+2] = num[0];
            whole[yo+3] = num[1];
            whole[yo+4] = num[2];
            whole[yo+5] = '\0';
            return whole;
         }
         else if ( index > 9 && yo < 16){
            whole[yo+2] = num[0];
            whole[yo+3] = num[1];
            whole[yo+4] = '\0';
            return whole;
         }
         else{
            whole[yo+2] = num[0];
            whole[yo+3] = '\0';
            return whole;
         }
      }
      whole[yo] = varname[yo];
   }
   whole[yo] = '\0';
   return whole;
}


/*** plot_string  ******************************************************
   Plot (draw) a string in 3-D.  At this time, only strings of digits,
   periods, and dashes are implemented.
   Input: f - the string to plot.
          startx, y, z - the point in 3-D to start at.
          base - vector indicating text baseline.
          up - vector indicating upright direction for text.
          rjustify - non-zero value indicates right justify the text.
**********************************************************************/
void plot_string( char *str, float startx, float starty, float startz,
                  float base[], float up[], int rjustify )
{
   static float zero[] = { 0,0, 0,.8, .4,.8, .4,0, 0,0 },
      one[] = { 0,0, 0,.8 },
      two[] = { .4,0, 0,0, 0,.4, .4,.4, .4,.8, 0,.8 },
      three[] = { 0,0, .4,0, .4,.4, 0,.4, .4,.4, .4,.8, 0,.8 },
      four[] = { 0,.8, 0,.4, .4,.4, .4,.8, .4,0 },
      five[] = { 0,0, .4,0, .4,.4, 0,.4, 0,.8, .4,.8 },
      six[] = { .4,.8, 0,.8, 0,0, .4,0, .4,.4, 0,.4 },
      seven[] = { 0,.7, 0,.8, .4,.8, .4,0 },
      eight[] = { 0,0, 0,.8, .4,.8, .4,0, 0,0, 0,.4, .4,.4 },
      nine[] = { .4,.4, 0,.4, 0,.8, .4,.8, .4,0 },
      dash[] = { 0,.4, .4,.4 },
      dot[] = { 0,0, 0,.1, .1,.1, .1,0, 0,0 };

   static float *index[12] = { zero, one, two, three, four, five, six,
                               seven, eight, nine, dash, dot };

   static float width[12] = { 0.6, 0.2, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6,
      0.6, 0.6, 0.6, 0.3 };
   static int verts[12] = { 5, 2, 6, 7, 5, 6, 6, 4, 7, 5, 2, 5 };

   float *temp, plot[100][3];
   float cx, cy, cz;
   int i, j, k, len;

   cx = startx;  cy = starty;  cz = startz;
   len = strlen(str);

   if (rjustify) {
      /* draw right justified text */
      for (i=len-1; i>=0; i--) {
         if (str[i]=='-')
            k = 10;
         else if (str[i]=='.')
            k = 11;
         else if (str[i]>='0' && str[i]<='9')
            k = str[i] - '0';
         else
            continue;
         /* calculate position for this char */
         cx += width[k]*base[0];
         cy += width[k]*base[1];
         cz += width[k]*base[2];
         /* make the vertex array for this character */
         temp = index[k];
         for (j=0; j<verts[k]; j++) {
            float x, y;
            x = *temp++;
            y = *temp++;
            plot[j][0] = cx - x*base[0] + y*up[0];
            plot[j][1] = cy - x*base[1] + y*up[1];
            plot[j][2] = cz - x*base[2] + y*up[2];
         }
         polyline( plot, verts[k] );
      }

   }
   else {
      /* draw left justified text */
      for (i=0; i<len; i++) {
         if (str[i]=='-')
            k = 10;
         else if (str[i]=='.')
            k = 11;
         else if (str[i]>='0' && str[i]<='9')
            k = str[i] - '0';
         else
            continue;
         /* make the vertex array for this character */
         temp = index[k];
         for (j=0; j<verts[k]; j++) {
            float x, y;
            x = *temp++;
            y = *temp++;
            plot[j][0] = cx + x*base[0] + y*up[0];
            plot[j][1] = cy + x*base[1] + y*up[1];
            plot[j][2] = cz + x*base[2] + y*up[2];
         }
         polyline( plot, verts[k] );
         /* calculate position for next char */
         cx += width[k]*base[0];
         cy += width[k]*base[1];
         cz += width[k]*base[2];
      }
   }

}

int check_for_valid_time( Context ctx, int dtxcurtime)
{
   Display_Context dtx;
   int dtime, stime, ldtime, lstime;
   int ctxcurtime, yo, ctime, spandex;
   int ctxdtime, ctxstime;
   dtx = ctx->dpy_ctx;
   
   if (dtx->numofctxs == 1){
      return 1;
   }
   if (ctx->NumTimes == 1){
      return 1;
   }
   ctxcurtime = 
   ldtime = lstime = 0;
   for( yo = 0; yo < dtx->numofctxs; yo ++){
      spandex = dtx->TimeStep[dtxcurtime].owners[yo];
      ctime = dtx->TimeStep[dtxcurtime].ownerstimestep[yo];
      vis5d_get_ctx_time_stamp( spandex, ctime, &dtime, &stime);
      if (spandex == ctx->context_index){
         ctxcurtime = ctime;
         ctxdtime = dtime;
         ctxstime = stime;
      }
      else if (dtime > ldtime || (dtime == ldtime && stime > lstime)){
         ldtime = dtime;
         lstime = stime;
      }
   }
   if (ctxcurtime == 0 && (ctxdtime > ldtime || 
       (ctxdtime == ldtime && ctxstime > lstime))){
       return 0;
   }
   else if (ctxcurtime == ctx->NumTimes-1 && (ctxdtime < ldtime || 
       (ctxdtime == ldtime && ctxstime < lstime))){
       return 0;
   }
   else{
      return 1;
   }
}

/*
 * Draw the tick mark for a horizontal slice.
 * Input:  level - grid level
 *         z - graphics z coord
 *         height - geographic height coord
 */
static void draw_horizontal_slice_tick( Display_Context dtx, float level,
                                        float z, float height )
{
   float v[2][3];
   static float base[3] = { 0.035, -0.035, 0.0 };
   static float up[3] = { 0.0, 0.0, 0.07 };
   char str[1000];

   /* vertices for tick mark */
   v[0][0] = dtx->Xmax;
   v[0][1] = dtx->Ymin;
   v[0][2] = z;
   v[1][0] = dtx->Xmax + 0.05;
   v[1][1] = dtx->Ymin - 0.05;
   v[1][2] = z;
   polyline( v, 2 );

   /* the label */
   if (dtx->CoordFlag) {
      /* MJK 12.01.98 */
      float2string(dtx, 2,  level+1.0, str );
   }
   else {
      /* MJK 12.01.98 */
      float2string(dtx, 2, VERTPRIME(height), str );
   }
   plot_string( str, dtx->Xmax+0.07, dtx->Ymin-0.07, z, base,up, 0 );
}




/*
 * Draw a tick mark for a vertical slice.
 * Input:  row, col - position in grid coords
 *         x, y - position in graphics coords
 *         lat,lon - position in geographic coords
 */
static void draw_vertical_slice_tick( Display_Context dtx, float row, float col,
                                      float x, float y, float lat, float lon )
{
   float v[2][3];
   int cc, rr;
   /* base and up vectors for drawing 3-D text */
   static float b2[3] = { 0.05, 0.0, 0.0 }, u2[3] = { 0.0, 0.05, 0.0 };
   static float b3[3] = { -0.05, 0.0, 0.0 }, u3[3] = { 0.0, 0.05, 0.0 };
   char str[1000];

   cc = (int) (col);
   rr = (int) (row);
   
   if (cc <= 0) {
      /* draw on top-west edge */
      v[0][0] = x;
      v[0][1] = y;
      v[0][2] = dtx->Zmax;
      v[1][0] = x-0.05;
      v[1][1] = y;
      v[1][2] = dtx->Zmax;
      polyline( v, 2 );
      if (dtx->CoordFlag) {
         /* MJK 12.01.98 */
         float2string(dtx, 1, row+1, str );
      }
      else {
         /* MJK 12.01.98 */      
         float2string(dtx, 1, lat, str );
      }
      plot_string( str, x-0.07, y, dtx->Zmax, b3, u3, 1 );
   }
   else if (cc >= dtx->Nc-1) {
      /* draw on top-east edge */
      v[0][0] = x;
      v[0][1] = y;
      v[0][2] = dtx->Zmax;
      v[1][0] = x+0.05;
      v[1][1] = y;
      v[1][2] = dtx->Zmax;
      polyline( v, 2 );
      if (dtx->CoordFlag) {
         /* MJK 12.01.98 */      
         float2string(dtx, 1, row+1, str );
      }
      else {
         /* MJK 12.01.98 */      
         float2string(dtx, 1, lat, str );
      }
      plot_string( str, x+0.07, y, dtx->Zmax, b2, u2, 0 );
   }
   else if (rr <=0) {
      /* draw on top-north edge */
      v[0][0] = x;
      v[0][1] = y;
      v[0][2] = dtx->Zmax;
      v[1][0] = x;
      v[1][1] = y+0.05;
      v[1][2] = dtx->Zmax;
      polyline( v, 2 );
      if (dtx->CoordFlag) {
         /* MJK 12.01.98 */               
         float2string(dtx, 0, col+1.0, str );
      }
      else {
         /* MJK 12.01.98 */               
         float2string(dtx, 0, lon, str );
      }
      plot_string( str, x-0.07, y+0.07, dtx->Zmax, b2,u2, 0 );
   }
   else {
      /* draw on top-south edge */
      v[0][0] = x;
      v[0][1] = y;
      v[0][2] = dtx->Zmax;
      v[1][0] = x;
      v[1][1] = y-0.05;
      v[1][2] = dtx->Zmax;
      polyline( v, 2 );
      if (dtx->CoordFlag) {
         /* MJK 12.01.98 */                        
         float2string(dtx, 0, col+1.0, str );
      }
      else {
         /* MJK 12.01.98 */                        
         float2string(dtx, 0, lon, str );
      }
      plot_string( str, x-0.07, y-0.12, dtx->Zmax, b2,u2, 0 );
   }
}


/*
 * Print the current cursor position.
 */
static void print_cursor_position( Display_Context dtx, int it )
{
   static float bx[3] = { 0.05, 0.0, 0.0 },      ux[3] = { 0.0, 0.05, 0.05 };
   static float by[3] = { -0.035, 0.0, -0.035 },  uy[3] = { 0.0, 0.07, 0.0 };
   static float bz[3] = { -0.035, -0.035, 0.0 }, uz[3] = { 0.0, 0.0, 0.07 };
   float v[6][3];
   float x, y, z;
   char str[100];

   /* MJK 12.01.98 */
   float lat, lon, hgt, row, col, lev;
   int ix;
   char fmt[] = {"%s: %9.3f %s  "};

   /* MJK 12.01.98 begin*/
   if ((dtx->DisplayProbe) || (dtx->DisplaySound)){
      /* MJK 3.29.99 */
      if (dtx->Reversed){
         set_color (PACK_COLOR(0,0,0,255));
      }
      else{
         set_color (dtx->BoxColor);
      }
   }
   else{
      set_color( *dtx->CursorColor );
   }
   /* end MJK 12.01.98 */

   if (dtx->Projection==PROJ_LINEAR || dtx->Projection==PROJ_GENERIC) {
      /* Rectangular box:  put labels along edge of box in 3-D */

      set_depthcue( dtx->DepthCue );

      /* draw tick marks */
      v[0][0] = v[1][0] = dtx->CursorX;
      v[0][1] = dtx->Ymin;  v[1][1] = dtx->Ymin-0.05;
      v[0][2] = dtx->Zmin;  v[1][2] = dtx->Zmin-0.05;
      v[2][0] = dtx->Xmin;  v[3][0] = dtx->Xmin-0.05;
      v[2][1] = v[3][1] = dtx->CursorY;
      v[2][2] = dtx->Zmin;  v[3][2] = dtx->Zmin-0.05;
      v[4][0] = dtx->Xmin;  v[5][0] = dtx->Xmin-0.05;
      v[4][1] = dtx->Ymin;  v[5][1] = dtx->Ymin-0.05;
      v[4][2] = v[5][2] = dtx->CursorZ;
      if (dtx->DisplaySound) {
         v[5][0] = 0;
         v[5][1] = 0;
         v[4][0] = 0;
         v[4][1] = 0;
      } 
      disjointpolyline( v, 6 );

      /* draw position labels */
      if (dtx->CoordFlag) {
         /* display cursor position in grid coordinates */
         xyzPRIME_to_gridPRIME( dtx, it, -1,
                      dtx->CursorX, dtx->CursorY, dtx->CursorZ, &y, &x, &z );
         x += 1.0;
         y += 1.0;
         z += 1.0;
      }
      else {
         /* display cursor position in geographic coordinates */
         xyzPRIME_to_geo( dtx, it, -1,
                     dtx->CursorX, dtx->CursorY, dtx->CursorZ, &y, &x, &z );
         z = VERTPRIME(z);
      }

      /* MJK 12.01.98 */                        
      float2string(dtx, 0, x, str );
      plot_string( str, dtx->CursorX-0.04, dtx->Ymin-0.1,
                   dtx->Zmin-0.125, bx, ux, 0 );
      float2string(dtx, 1, y, str );
      plot_string( str, dtx->Xmin-0.075, dtx->CursorY-0.02,
                   dtx->Zmin-0.075, by, uy, 1 );
      float2string(dtx, 2, z, str );


      if (!dtx->DisplaySound)
      plot_string( str, dtx->Xmin-0.07, dtx->Ymin-0.07,
                   dtx->CursorZ+0.005, bz, uz, 1 );

      set_depthcue( 0 );
   }


   /* MJK 12.01.98 */
   ix = dtx->Nr;
   if (dtx->Nc > ix) ix = dtx->Nc;
   if (dtx->MaxNl > ix) ix = dtx->MaxNl;
   x = ix;

   v[0][0] = dtx->Xmin, v[0][1] = dtx->Ymin, v[0][2] = dtx->Zmin;
   v[1][0] = dtx->Xmax, v[1][1] = dtx->Ymax, v[1][2] = dtx->Zmax;
   v[2][0] = dtx->Xmin, v[2][1] = dtx->Ymax, v[2][2] = dtx->Zmax;
   v[3][0] = dtx->Xmax, v[3][1] = dtx->Ymin, v[3][2] = dtx->Zmax;
   for (ix = 0; ix < 4; ix++)
   {
      xyzPRIME_to_geo (dtx, it, -1, v[ix][0], v[ix][1], v[ix][2],
                       &lat, &lon, &hgt);
      if (lat < 0.0) lat = -lat;
      if (lon < 0.0) lon = -lon;
      hgt = VERTPRIME(hgt);
      if (lat > x) x = lat;
      if (lon > x) x = lon;
      if (hgt > x) x = hgt;
   }

   sprintf (str, "%.3f", x);
   ix = strchr (str, '.') - str;
   if (ix < 1) ix = 1;
   fmt[5] = (ix + 4) + '0';
   sprintf (str, fmt, "XXX", x, "xx");
   ix = dtx->WinWidth - (text_width (str));

   if (dtx->CoordFlag) {
      /* display cursor position in grid coordinates */
      xyzPRIME_to_gridPRIME (dtx, it, -1,
                             dtx->CursorX, dtx->CursorY, dtx->CursorZ,
                             &row, &col, &lev);

      sprintf( str, "Row: %g", row+1.0 );
      draw_text( ix, (dtx->FontHeight+VSPACE), str );

      sprintf( str, "Col: %g", col+1.0 );
      draw_text( ix, 2*(dtx->FontHeight+VSPACE), str );

      if (!dtx->DisplaySound)
      {
         sprintf( str, "Lev: %g", lev+1.0 );
         draw_text( ix, 3*(dtx->FontHeight+VSPACE), str );
      }

   }
   else {
      /* display cursor position in geographic coordinates */
      xyzPRIME_to_geo (dtx, it, -1, dtx->CursorX, dtx->CursorY, dtx->CursorZ,
                       &lat, &lon, &hgt);
      if (dtx->Projection == PROJ_GENERIC)
      {
         sprintf (str, fmt, "Row", lat, "");
         draw_text( ix, (dtx->FontHeight+VSPACE), str );

         sprintf (str, fmt, "Col", lon, "");
         draw_text( ix, 2*(dtx->FontHeight+VSPACE), str );
      }
      else
      {
         char hemi[] = {"X"};

         /* hemispheres are "Wisconsin-centric" ;-) */
         hemi[0] = 'N';
         if (lat < 0.0) lat = -lat, hemi[0] = 'S';
         sprintf (str, fmt, "Lat", lat, hemi);
         draw_text( ix, (dtx->FontHeight+VSPACE), str );

         hemi[0] = 'W';
         if (lon < 0.0) lon = -lon, hemi[0] = 'E';
         sprintf (str, fmt, "Lon", lon, hemi);
         draw_text( ix, 2*(dtx->FontHeight+VSPACE), str );
      }

      if (!dtx->DisplaySound)
      {
         if (dtx->VerticalSystem == VERT_NONEQUAL_MB)
            sprintf (str, fmt, "Hgt", VERTPRIME(hgt), "mb");
         else
            sprintf (str, fmt, "Hgt", hgt, "km");
         draw_text( ix, 3*(dtx->FontHeight+VSPACE), str );
      }
   }
   /* end MJK 12.01.98 */
}



/*
 * Examine the entries of a color lookup table to determine the alpha value.
 * If the alpha is the same for all entries return that value.  If the
 * alpha varies between entries return -1.
 * Input:  color_table - array of color values
 *         size - number of entries in table
 * Return:  -1 or constant alpha value
 */
static int get_alpha( unsigned int color_table[], int size )
{
   int alpha, i;

   /* Check for variable vs. constant alpha */
   alpha = UNPACK_ALPHA( color_table[0] );
   for (i=0;i<size;i++) {
      if (UNPACK_ALPHA(color_table[i])!=alpha) {
         return -1;
      }
   }
   return alpha;
}




/*
 * Render all isosurfaces selected for display.
 * Input:  ctx - the context
 *         time -  dtx time!! 
 *         tf - transparency flag: 1=only draw opaque surfaces
 *                                 0=only draw transparent surfaces
 */
static void render_isosurfaces( Context ctx, int dtxtime, int ctxtime, int tf, int animflag )
{
   int var, alpha, lock;
   Display_Context dtx;
   int time, colorvar, cvowner;

   dtx = ctx->dpy_ctx;
   for (var=0;var<ctx->NumVars;var++) {
      if (ctx->SameIsoColorVarOwner[var] ||
          ctx->IsoColorVar[var] < 0){
         time = ctxtime;
      }
      else{
         time = dtxtime;
      }
      if (ctx->DisplaySurf[var] && ctx->SurfTable[var][time].valid) {
         if (animflag) {
            lock = cond_read_lock( &ctx->SurfTable[var][time].lock );
         }
         else {
            wait_read_lock( &ctx->SurfTable[var][time].lock );
            lock = 1;
         }
         if (lock) {
            recent( ctx, ISOSURF, var );
            colorvar = ctx->SurfTable[var][time].colorvar;
            cvowner = ctx->SurfTable[var][time].cvowner;
            /* Determine alpha for surface:  -1=variable, 0..255=constant */
            if (ctx->SurfTable[var][time].colors) {
               alpha = UNPACK_ALPHA( dtx->Color[ctx->context_index*MAXVARS+
                                     var][ISOSURF] ); 
               /* alpha = get_alpha( ctx->IsoColors[colorvar], 255 ); WLH 16 Aug 97 */
            }
            else {
               alpha = UNPACK_ALPHA( dtx->Color[ctx->context_index*MAXVARS+
                                     var][ISOSURF] );
            }
            if ( (tf && alpha==255) || (tf==0 && alpha<255) ) {
               if (ctx->SurfTable[var][time].colors) {
                  draw_colored_isosurface(
                                   ctx->SurfTable[var][time].numindex,
                                   ctx->SurfTable[var][time].index,
                                   (void *) ctx->SurfTable[var][time].verts,
                                   (void *) ctx->SurfTable[var][time].norms,
                                   (void *) ctx->SurfTable[var][time].colors,
                                   dtx->IsoColors[cvowner*MAXVARS+colorvar],
                                   alpha );
               }
               else {
                  draw_isosurface( ctx->SurfTable[var][time].numindex,
                                   ctx->SurfTable[var][time].index,
                                   (void *) ctx->SurfTable[var][time].verts,
                                   (void *) ctx->SurfTable[var][time].norms,
                                   dtx->Color[ctx->context_index*MAXVARS+var][0] );



               
               }
            }
            done_read_lock( &ctx->SurfTable[var][time].lock );
         }
      }
   }
}


static void render_hclips( Display_Context dtx, int animflag)
{
   int i, lock;
   for (i = 0; i < 2; i++){
      if (dtx->HClipTable[i].highlight == 1){
         set_color( PACK_COLOR(100,25,240,255));
         set_line_width(4);
      }
      else{
         set_color( PACK_COLOR(50,200,75,255));
         set_line_width(1);
      }
      polyline( (void *) dtx->HClipTable[i].boxverts,
                      dtx->HClipTable[i].numboxverts);
      /* MJK 3.29.99 */
      if (dtx->Reversed){
         set_color( PACK_COLOR(0,0,0,255) );
      }
      else{
         set_color( dtx->BoxColor );
      }


      set_line_width(dtx->LineWidth);
   }
}

static void render_vclips( Display_Context dtx, int animflag)
{
   int i, lock;   
   for (i = 0; i < 4; i++){
      if (dtx->VClipTable[i].highlight == 1){
         set_color( PACK_COLOR(100,25,240,255));
         set_line_width(4);
      }
      else{
         set_color( PACK_COLOR(50,200,75,255));
         set_line_width(1);
      }
      polyline( (void *) dtx->VClipTable[i].boxverts,
                         dtx->VClipTable[i].numboxverts);
      if (dtx->VClipTable[i].highlight == 1){
         float vert[4][3];
         float zbot, ztop, x1, x2, y1, y2;
         float llev, hlev;
         llev = (float) dtx->LowLev;
         hlev = (float) (dtx->Nl-1+dtx->LowLev);
         gridPRIME_to_xyzPRIME( dtx, 0, 0, 1, &dtx->VClipTable[i].r1,
                                &dtx->VClipTable[i].c1, &llev,
                                &x1, &y1, &zbot);
         gridPRIME_to_xyzPRIME( dtx, 0, 0, 1, &dtx->VClipTable[i].r2,
                                &dtx->VClipTable[i].c2, &hlev,
                                &x2, &y2, &ztop);
         vert[0][0] = vert[1][0] = vert[2][0] = vert[3][0] = (x1 + x2)*0.5;
         vert[0][1] = vert[1][1] = vert[2][1] = vert[3][1] = (y1 +y2)*0.5;
         vert[0][2] = ztop+TICK_SIZE;
         vert[1][2] = ztop;
         vert[2][2] = zbot;
         vert[3][2] = zbot-TICK_SIZE;
         set_line_width(5);
         disjointpolyline( vert, 4 );
      }   
/* MJK 3.29.99 */
   if (dtx->Reversed){
      set_color( PACK_COLOR(0,0,0,255) );
   }
   else{
      set_color( dtx->BoxColor );
   }


      set_line_width(dtx->LineWidth);
   }
}


static void render_textplots( Irregular_Context itx, int time)
{
   int var;
   float a, b, c, d;
   static do_once = 0;

   vis5d_get_text_plot(itx->context_index, &var, &a, &b, &c, &d);
   if (itx->DisplayTextPlot && itx->TextPlotTable[time].valid){
      if (itx->TextPlotTable[time].colors){
         draw_colored_disjoint_lines(itx->TextPlotTable[time].numverts,
                               (void *) itx->TextPlotTable[time].verts,
                               (void *) itx->TextPlotTable[time].colors,
                               itx->dpy_ctx->TextPlotColors[itx->context_index*
                               MAXVARS+var]);
      }
      else{
         draw_disjoint_lines( itx->TextPlotTable[time].numverts,
                              (void *) itx->TextPlotTable[time].verts,
                              itx->dpy_ctx->TextPlotColor[itx->context_index*
                             MAXVARS+var]);
      }
   }
}

/*
 * Render all horizontal contour slices selected for display.
 * Input:  ctx - the context
 *         time - the time step
 *         labels - draw labels flag.
 */
static void render_hslices( Context ctx, int time, int labels, int animflag )
{
   int var, lock;

   for (var=0;var<ctx->NumVars;var++) {
      if (ctx->DisplayHSlice[var] && ctx->HSliceTable[var][time].valid) {
         if (animflag) {
            lock = cond_read_lock(&ctx->HSliceTable[var][time].lock);
         }
         else {
            wait_read_lock(&ctx->HSliceTable[var][time].lock);
            lock = 1;
         }
         if (lock) {
            recent( ctx, HSLICE, var );

            /* draw main contour lines */
            draw_disjoint_lines( ctx->HSliceTable[var][time].num1,
                                 (void *) ctx->HSliceTable[var][time].verts1,
                                 ctx->dpy_ctx->Color[ctx->context_index*MAXVARS+var][HSLICE] );
            
            if (labels) {
               /* draw contour labels */
               draw_disjoint_lines( ctx->HSliceTable[var][time].num3,
                                    (void *)ctx->HSliceTable[var][time].verts3,
                                    ctx->dpy_ctx->Color[ctx->context_index*MAXVARS
                                    +var][HSLICE] );
            }
            else {
               /* draw hidden contour lines */
               draw_disjoint_lines( ctx->HSliceTable[var][time].num2,
                                    (void *)ctx->HSliceTable[var][time].verts2,
                                    ctx->dpy_ctx->Color[ctx->context_index*MAXVARS+
                                    var][HSLICE] );
            }

            /* draw the bounding box */
            /* MJK 12.01.98 */
            if (!ctx->DisplaySfcHSlice[var]){
               polyline( (void *) ctx->HSliceTable[var][time].boxverts,
                              ctx->HSliceTable[var][time].numboxverts );
            }

            done_read_lock( &ctx->HSliceTable[var][time].lock );
         }

         /* draw position label */
         /* MJK 12.01.98 */
         if (!ctx->DisplaySfcHSlice[var]){
            if (ctx->dpy_ctx->DisplayBox && !ctx->dpy_ctx->CurvedBox) {
               float l, z, h;
               l =  ctx->HSliceLevel[var];
               z = height_to_zPRIME( ctx->dpy_ctx, ctx->HSliceHgt[var]);   
               clipping_off();
               draw_horizontal_slice_tick( ctx->dpy_ctx,l, z, ctx->HSliceHgt[var]);
               clipping_on();
            }
         }
      }
   }
}




/*
 * Render all vertical contour slices selected for display.
 * Input:  ctx - the context
 *         time - the time step
 *         labels - draw labels flag.
 */
static void render_vslices( Context ctx, int time, int labels, int animflag )
{
   int var, lock;
   float r1, r2, c1, c2, l, x1, x2,y1,y2,z1,z2,hgt1,hgt2;
   float r1p, r2p, c1p, c2p, lp;

   for (var=0;var<ctx->NumVars;var++) {
      if (ctx->DisplayVSlice[var] && ctx->VSliceTable[var][time].valid) {

         /* MJK 12.01.98 */
         if (labels)
         {
            if (check_view_side (ctx, VSLICE, var) < 0)
            {
               flip_vslice_end_for_end (ctx, time, var);
            }
         }


         if (animflag) {
            lock = cond_read_lock(&ctx->VSliceTable[var][time].lock);
         }
         else {
            wait_read_lock(&ctx->VSliceTable[var][time].lock);
            lock = 1;
         }
         if (lock) {
            recent( ctx, VSLICE, var );

            /* draw main contour lines */
            draw_disjoint_lines( ctx->VSliceTable[var][time].num1,
                                 (void*) ctx->VSliceTable[var][time].verts1,
                                 ctx->dpy_ctx->Color[ctx->context_index*MAXVARS+
                                 var][VSLICE] );

            if (labels) {
               /* draw contour labels */
               draw_disjoint_lines( ctx->VSliceTable[var][time].num3,
                                    (void*) ctx->VSliceTable[var][time].verts3,
                                    ctx->dpy_ctx->Color[ctx->context_index*MAXVARS+
                                    var][VSLICE] );
            }
            else {
               /* draw hidden contour lines */
               draw_disjoint_lines( ctx->VSliceTable[var][time].num2,
                                    (void*) ctx->VSliceTable[var][time].verts2,
                                    ctx->dpy_ctx->Color[ctx->context_index*MAXVARS+
                                    var][VSLICE] );
            }

            /* draw the bounding box */
            polyline( (void *) ctx->VSliceTable[var][time].boxverts,
                           ctx->VSliceTable[var][time].numboxverts );

            done_read_lock( &ctx->VSliceTable[var][time].lock );
         }

         if (ctx->dpy_ctx->DisplayBox && !ctx->dpy_ctx->CurvedBox) {
            /* draw position labels */
            float vert[4][3];
            float zbot, ztop;
            zbot = gridlevelPRIME_to_zPRIME(ctx->dpy_ctx, time, var, (float) ctx->dpy_ctx->LowLev);
            ztop = gridlevelPRIME_to_zPRIME(ctx->dpy_ctx, time, var,
                                  (float) (ctx->dpy_ctx->Nl-1+ctx->dpy_ctx->LowLev));
            set_color( ctx->dpy_ctx->Color[ctx->context_index*MAXVARS+var][VSLICE] );
            r1p = ctx->VSliceR1[var];
            c1p = ctx->VSliceC1[var];
            r2p = ctx->VSliceR2[var];
            c2p = ctx->VSliceC2[var];
            lp= 0.0;
            gridPRIME_to_xyzPRIME( ctx->dpy_ctx, time, var, 1, &r1p, &c1p, &lp, &x1, &y1, &z1);
            gridPRIME_to_xyzPRIME( ctx->dpy_ctx, time, var, 1, &r2p, &c2p, &lp, &x2, &y2, &z2);
            clipping_off();
            draw_vertical_slice_tick(ctx->dpy_ctx, r1p, c1p, x1, y1, 
                                     ctx->VSliceLat1[var], ctx->VSliceLon1[var] );
            draw_vertical_slice_tick(ctx->dpy_ctx, r2p, c2p, x2, y2,
                                     ctx->VSliceLat2[var],ctx->VSliceLon2[var] );
/* 
            draw_vertical_slice_tick( ctx->dpy_ctx, ctx->VSliceR1[var],
                                      ctx->VSliceC1[var],
                                      ctx->VSliceX1[var],
                                      ctx->VSliceY1[var],
                                      ctx->VSliceLat1[var],
                                      ctx->VSliceLon1[var] );
            draw_vertical_slice_tick( ctx->dpy_ctx, ctx->VSliceR2[var],
                                      ctx->VSliceC2[var],
                                      ctx->VSliceX2[var],
                                      ctx->VSliceY2[var],
                                      ctx->VSliceLat2[var],
                                      ctx->VSliceLon2[var] );
*/
            /* draw small markers at midpoint of top and bottom edges */
            vert[0][0] = vert[1][0] = vert[2][0] = vert[3][0] = (x1 + x2)*0.5;
/*
                              (ctx->VSliceX1[var] + ctx->VSliceX2[var]) * 0.5;
*/
            vert[0][1] = vert[1][1] = vert[2][1] = vert[3][1] = (y1 +y2)*0.5;
/*
                              (ctx->VSliceY1[var] + ctx->VSliceY2[var]) * 0.5;
*/
            vert[0][2] = ztop+TICK_SIZE;
            vert[1][2] = ztop;
            vert[2][2] = zbot;
            vert[3][2] = zbot-TICK_SIZE;
            set_line_width(5); /* WLH 3-5-96 */
            disjointpolyline( vert, 4 );
            set_line_width(ctx->dpy_ctx->LineWidth); /* WLH 3-5-96 */
            clipping_on();
         }
      }
   }
}





/*
 * Render all horizontal colored slices selected for display.
 * Input:  ctx - the context
 *         time - the timestep
 *         tf - transparency flag: 1=only draw opaque slices
 *                                 0=only draw transparent slices
 *         animflag - 1=animating, 0=not animating
 */
static void render_chslices( Context ctx, int time, int tf, int animflag )
{
   int var, alpha, lock;
   Display_Context dtx;

   dtx = ctx->dpy_ctx;
   for (var=0;var<ctx->NumVars;var++) {
      if (ctx->DisplayCHSlice[var]) {
         if (ctx->CHSliceTable[var][time].valid) {
            if (animflag) {
               lock = cond_read_lock( &ctx->CHSliceTable[var][time].lock );
            }
            else {
               wait_read_lock( &ctx->CHSliceTable[var][time].lock );
               lock = 1;
            }
            if (lock) {
               recent( ctx, CHSLICE, var );

               alpha = get_alpha( dtx->CHSliceColors[ctx->context_index*MAXVARS+var],
                                  255 );

               if ( (tf && alpha==255) || (tf==0 && alpha<255) ) {
                  draw_color_quadmesh( ctx->CHSliceTable[var][time].rows,
                                    ctx->CHSliceTable[var][time].columns,
                                    (void *)ctx->CHSliceTable[var][time].verts,
                                    ctx->CHSliceTable[var][time].color_indexes,
                                    dtx->CHSliceColors[ctx->context_index*MAXVARS+var],
                                     -1 );
                                 /* ctx->CHSliceColors[var], alpha ); WLH 15 Aug 97 */
               }

               done_read_lock( &ctx->CHSliceTable[var][time].lock );
            }

            /* draw position label */
            if (tf && dtx->DisplayBox && !dtx->CurvedBox) {
               set_color( dtx->Color[ctx->context_index*MAXVARS+var][CHSLICE] );
               clipping_off();
               draw_horizontal_slice_tick( dtx, ctx->CHSliceLevel[var],
                                           ctx->CHSliceZ[var],
                                           ctx->CHSliceHgt[var] );
               clipping_on();
            }
         }
      }
   }
}




/*
 * Render all vertical colored slices selected for display.
 * Input:  ctx - the context
 *         time - the timestep
 *         tf - transparency flag: 1=only draw opaque slices
 *                                 0=only draw transparent slices
 */
static void render_cvslices( Context ctx, int time, int tf, int animflag )
{
   int var, alpha, lock;
   float r1, r2, c1, c2, l, x1, x2,y1,y2,z1,z2,hgt1,hgt2;
   float r1p, r2p, c1p, c2p, lp;
   Display_Context dtx;

   dtx = ctx->dpy_ctx;
   for (var=0;var<ctx->NumVars;var++) {
      if (ctx->DisplayCVSlice[var] && ctx->CVSliceTable[var][time].valid) {
         if (animflag) {
            lock = cond_read_lock(&ctx->CVSliceTable[var][time].lock);
         }
         else {
            wait_read_lock(&ctx->CVSliceTable[var][time].lock);
            lock = 1;
         }
         if (lock) {
            recent( ctx, CVSLICE, var );

            alpha = get_alpha( dtx->CVSliceColors[ctx->context_index*MAXVARS+var], 255 );
            if ( (tf && alpha==255) || (tf==0 && alpha<255) ) {
               draw_color_quadmesh( ctx->CVSliceTable[var][time].rows,
                                    ctx->CVSliceTable[var][time].columns,
                                    (void *)ctx->CVSliceTable[var][time].verts,
                                    ctx->CVSliceTable[var][time].color_indexes,
                                    dtx->CVSliceColors[ctx->context_index*MAXVARS+var],
                                    -1 );
                                 /* ctx->CVSliceColors[var], alpha ); WLH 15 Aug 97 */
            }
            done_read_lock( &ctx->CVSliceTable[var][time].lock );
         }

         if (tf & dtx->DisplayBox && !dtx->CurvedBox) {
            /* draw position labels */
            float zbot, ztop;
            float vert[4][3];
            zbot = gridlevelPRIME_to_zPRIME(ctx->dpy_ctx, time, var, (float) ctx->dpy_ctx->LowLev);
            ztop = gridlevelPRIME_to_zPRIME(ctx->dpy_ctx, time, var,
                                  (float) (ctx->dpy_ctx->Nl-1+ctx->dpy_ctx->LowLev));
            set_color( dtx->Color[ctx->context_index*MAXVARS+var][CVSLICE] );
            r1p = ctx->CVSliceR1[var];
            c1p = ctx->CVSliceC1[var];
            r2p = ctx->CVSliceR2[var];
            c2p = ctx->CVSliceC2[var];
            lp= 0.0;
            gridPRIME_to_xyzPRIME( ctx->dpy_ctx, time, var, 1, &r1p, &c1p, &lp, &x1, &y1, &z1);
            gridPRIME_to_xyzPRIME( ctx->dpy_ctx, time, var, 1, &r2p, &c2p, &lp, &x2, &y2, &z2);

            clipping_off();
            draw_vertical_slice_tick(ctx->dpy_ctx, r1p, c1p, x1, y1,
                                     ctx->CVSliceLat1[var], ctx->CVSliceLon1[var] );
            draw_vertical_slice_tick(ctx->dpy_ctx, r2p, c2p, x2, y2,
                                     ctx->CVSliceLat2[var],ctx->CVSliceLon2[var] );
/*
            draw_vertical_slice_tick( dtx, ctx->CVSliceR1[var],
                                      ctx->CVSliceC1[var],
                                      ctx->CVSliceX1[var],
                                      ctx->CVSliceY1[var],
                                      ctx->CVSliceLat1[var],
                                      ctx->CVSliceLon1[var] );
            draw_vertical_slice_tick( dtx, ctx->CVSliceR2[var],
                                      ctx->CVSliceC2[var],
                                      ctx->CVSliceX2[var],
                                      ctx->CVSliceY2[var],
                                      ctx->CVSliceLat2[var],
                                      ctx->CVSliceLon2[var] );
*/

            /* draw small markers at midpoint of top and bottom edges */
            vert[0][0] = vert[1][0] = vert[2][0] = vert[3][0] = (x1 + x2)*0.5;
            vert[0][1] = vert[1][1] = vert[2][1] = vert[3][1] = (y1 +y2)*0.5;
/*

            vert[0][0] = vert[1][0] = vert[2][0] = vert[3][0] =
                           (ctx->CVSliceX1[var] + ctx->CVSliceX2[var]) * 0.5;
            vert[0][1] = vert[1][1] = vert[2][1] = vert[3][1] =
                           (ctx->CVSliceY1[var] + ctx->CVSliceY2[var]) * 0.5;
*/
            vert[0][2] = ztop+TICK_SIZE;
            vert[1][2] = ztop;
            vert[2][2] = zbot;
            vert[3][2] = zbot-TICK_SIZE;
            set_line_width(5); /* WLH 3-5-96 */
            disjointpolyline( vert, 4 );
            set_line_width(dtx->LineWidth); /* WLH 3-5-96 */
            clipping_on();
         }
      }
   }
}




/*
 * Render all horizontal wind vector slices which are selected for display.
 * Input:  ctx - the context
 *         time - the timestep
 */
static void render_hwind_slices( Context ctx, int time, int animflag )
{
   int w, lock;
   Display_Context dtx;

   dtx = ctx->dpy_ctx;
   for (w=0;w<VIS5D_WIND_SLICES;w++) {
      if (dtx->DisplayHWind[w] && dtx->HWindTable[w][time].valid &&
          ctx->context_index == dtx->Uvarowner[w]) {
         if (animflag) {
            lock = cond_read_lock( &dtx->HWindTable[w][time].lock );
         }
         else {
            wait_read_lock( &dtx->HWindTable[w][time].lock );
            lock = 1;
         }
         /* MJK 12.01.98 */
         set_color( dtx->HWindColor[w] );
         if (ctx->dpy_ctx->DisplaySfcHWind[w])
         {
            if (lock) {
               recent( ctx, HWIND, w );

               draw_disjoint_lines( dtx->HWindTable[w][time].nvectors,
                                    (void *) dtx->HWindTable[w][time].verts,
                                    dtx->HWindColor[w] );

               done_read_lock( &dtx->HWindTable[w][time].lock );
            }
         }
         
         else
         {
            if (lock) {
               recent( ctx, HWIND, w );

               /* draw the bounding box */
               polyline( (void *) dtx->HWindTable[w][time].boxverts,
                         dtx->HWindTable[w][time].numboxverts );

               /* draw wind vectors */
               if (dtx->HWindTable[w][time].barbs) {
                 draw_disjoint_lines( dtx->HWindTable[w][time].nvectors,
                                      (void *) dtx->HWindTable[w][time].verts,
                                      dtx->HWindColor[w] );
               }
               else {
                 draw_wind_lines( dtx->HWindTable[w][time].nvectors / 4,
                                  (void *) dtx->HWindTable[w][time].verts,
                                  dtx->HWindColor[w] );
               }

               done_read_lock( &dtx->HWindTable[w][time].lock );
            }

            /* draw position label */
            if (dtx->DisplayBox && !dtx->CurvedBox) {
               clipping_off();
               draw_horizontal_slice_tick( dtx, dtx->HWindLevel[w],
                                           dtx->HWindZ[w], dtx->HWindHgt[w]);
               clipping_on();
            }
         }
      /* end MJK 12.01.98 */
      }
   }

}



/*
 * Render all vertical wind vector slices which are selected for display.
 * Input:  ctx - the context
 *         time - the timestep
 */
static void render_vwind_slices( Context ctx, int time, int animflag )
{
   int w, lock;
   Display_Context dtx;
   float r1, r2, c1, c2, l, x1, x2,y1,y2,z1,z2,hgt1,hgt2;
   float r1p, r2p, c1p, c2p, lp;

   dtx = ctx->dpy_ctx;
   for (w=0;w<VIS5D_WIND_SLICES;w++) {
      if (dtx->DisplayVWind[w] && dtx->VWindTable[w][time].valid &&
          ctx->context_index == dtx->Uvarowner[w]) {
         if (animflag) {
            lock = cond_read_lock(&dtx->VWindTable[w][time].lock);
         }
         else {
            wait_read_lock(&dtx->VWindTable[w][time].lock);
            lock = 1;
         }
         if (lock) {
            ctx = dtx->ctxpointerarray[0];
            recent( ctx, VWIND, w );

            /* draw the bounding box */
            set_color( dtx->VWindColor[w] );
            polyline( (void *) dtx->VWindTable[w][time].boxverts,
                      dtx->VWindTable[w][time].numboxverts );
            /* draw wind vectors */
            if (dtx->VWindTable[w][time].barbs) {
              draw_disjoint_lines( dtx->VWindTable[w][time].nvectors,
                                   (void *) dtx->VWindTable[w][time].verts,
                                   dtx->VWindColor[w] );
            }
            else {
              draw_wind_lines( dtx->VWindTable[w][time].nvectors / 4,
                               (void *) dtx->VWindTable[w][time].verts,
                               dtx->VWindColor[w] );
            }

            done_read_lock( &dtx->VWindTable[w][time].lock );
         }

         if (dtx->DisplayBox && !dtx->CurvedBox) {
            /* position labels */
            float zbot, ztop;
            float vert[4][3];
            zbot = gridlevelPRIME_to_zPRIME(dtx, time, dtx->Uvar[w],
                                  (float) dtx->LowLev);
            ztop = gridlevelPRIME_to_zPRIME(dtx, time, dtx->Uvar[w],
                                  (float) (dtx->Nl
                                           +dtx->LowLev));
            clipping_off();
            r1p = dtx->VWindR1[w];
            c1p = dtx->VWindC1[w];
            r2p = dtx->VWindR2[w];
            c2p = dtx->VWindC2[w];
            lp= 0.0;
            gridPRIME_to_xyzPRIME( dtx, time, dtx->Uvar[w], 1, &r1p, &c1p, &lp, &x1, &y1, &z1);
            gridPRIME_to_xyzPRIME( dtx, time, dtx->Uvar[w], 1, &r2p, &c2p, &lp, &x2, &y2, &z2);

            draw_vertical_slice_tick( dtx, r1p, c1p, x1, y1, 
                                      dtx->VWindLat1[w], dtx->VWindLon1[w] );
            draw_vertical_slice_tick( dtx, r2p, c2p, x2, y2,
                                      dtx->VWindLat2[w], dtx->VWindLon2[w] );
            /* draw small markers at midpoint of top and bottom edges */
            vert[0][0] = vert[1][0] = vert[2][0] = vert[3][0] =
                                  (dtx->VWindX1[w] + dtx->VWindX2[w]) * 0.5;
            vert[0][1] = vert[1][1] = vert[2][1] = vert[3][1] =
                                  (dtx->VWindY1[w] + dtx->VWindY2[w]) * 0.5;
            vert[0][2] = ztop+TICK_SIZE;
            vert[1][2] = ztop;
            vert[2][2] = zbot;
            vert[3][2] = zbot-TICK_SIZE;
            set_line_width(5); /* WLH 3-5-96 */
            disjointpolyline( vert, 4 );
            set_line_width(dtx->LineWidth); /* WLH 3-5-96 */
            clipping_on();
         }
      }
   }
}



/*
 * Render all horizontal stream vector slices which are selected for display.
 * Input:  ctx - the context
 *         time - the timestep
 */
static void render_hstream_slices( Context ctx, int time, int animflag )
{
   int w, lock;
   Display_Context dtx;

   dtx = ctx->dpy_ctx;
   for (w=0;w<VIS5D_WIND_SLICES;w++) {
      if (dtx->DisplayHStream[w] && dtx->HStreamTable[w][time].valid &&
          ctx->context_index == dtx->Uvarowner[w]) {
         if (animflag) {
            lock = cond_read_lock(&dtx->HStreamTable[w][time].lock);
         }
         else {
            wait_read_lock(&dtx->HStreamTable[w][time].lock);
            lock = 1;
         }
         if (lock) {
            ctx = dtx->ctxpointerarray[0];
            recent( ctx, HSTREAM, w );

            /* draw the bounding box */
            set_color( dtx->HStreamColor[w] );
            /* MJK 12.02.92 */
            if (!ctx->dpy_ctx->DisplaySfcHStream[w]){
               polyline( (void *) dtx->HStreamTable[w][time].boxverts,
                         dtx->HStreamTable[w][time].numboxverts );
            }


            /* draw main contour lines */
            draw_disjoint_lines( dtx->HStreamTable[w][time].nlines,
                                 (void *) dtx->HStreamTable[w][time].verts,
                                 dtx->HStreamColor[w] );
            
            done_read_lock( &dtx->HStreamTable[w][time].lock );
         }

         /* draw position label */
         /* MJK 12.02.92 */
         if (!ctx->dpy_ctx->DisplaySfcHStream[w]){
            if (dtx->DisplayBox && !dtx->CurvedBox) {
               clipping_off();
               draw_horizontal_slice_tick( dtx, dtx->HStreamLevel[w],
                                           dtx->HStreamZ[w], dtx->HStreamHgt[w]);
               clipping_on();
            }
         }
      }
   }
}



/*
 * Render all vertical stream vector slices which are selected for display.
 * Input:  ctx - the context
 *         time - the timestep
 */
static void render_vstream_slices( Context ctx, int time, int animflag )
{
   int w, lock;
   Display_Context dtx;

   dtx = ctx->dpy_ctx;
   for (w=0;w<VIS5D_WIND_SLICES;w++) {
      if (dtx->DisplayVStream[w] && dtx->VStreamTable[w][time].valid &&
          ctx->context_index == dtx->Uvarowner[w]) {
         if (animflag) {
            lock = cond_read_lock(&dtx->VStreamTable[w][time].lock);
         }
         else {
            wait_read_lock(&dtx->VStreamTable[w][time].lock);
            lock = 1;
         }
         if (lock) {
         ctx = dtx->ctxpointerarray[0];
         recent( ctx, VSTREAM, w );

            /* draw the bounding box */
            set_color( dtx->VStreamColor[w] );
            polyline( (void *) dtx->VStreamTable[w][time].boxverts,
                      dtx->VStreamTable[w][time].numboxverts );


            /* draw main contour lines */
            draw_disjoint_lines( dtx->VStreamTable[w][time].nlines,
                                 (void *) dtx->VStreamTable[w][time].verts,
                                 dtx->VStreamColor[w] );

            done_read_lock( &dtx->VStreamTable[w][time].lock );
         }

         if (dtx->DisplayBox && !dtx->CurvedBox) {
            /* position labels */
            float zbot, ztop;
            float vert[4][3];
            zbot = gridlevelPRIME_to_zPRIME(dtx, time, dtx->Uvar[w],
                                  (float) dtx->LowLev);
            ztop = gridlevelPRIME_to_zPRIME(dtx, time, dtx->Uvar[w],
                                  (float) (dtx->Nl
                                           +dtx->LowLev));
            clipping_off();
            draw_vertical_slice_tick( dtx, dtx->VStreamR1[w], dtx->VStreamC1[w],
                                      dtx->VStreamX1[w], dtx->VStreamY1[w],
                                      dtx->VStreamLat1[w], dtx->VStreamLon1[w] );
            draw_vertical_slice_tick( dtx, dtx->VStreamR2[w], dtx->VStreamC2[w],
                                      dtx->VStreamX2[w], dtx->VStreamY2[w],
                                      dtx->VStreamLat2[w], dtx->VStreamLon2[w] );
            /* draw small markers at midpoint of top and bottom edges */
            vert[0][0] = vert[1][0] = vert[2][0] = vert[3][0] =
                                  (dtx->VStreamX1[w] + dtx->VStreamX2[w]) * 0.5;
            vert[0][1] = vert[1][1] = vert[2][1] = vert[3][1] =
                                  (dtx->VStreamY1[w] + dtx->VStreamY2[w]) * 0.5;
            vert[0][2] = ztop+TICK_SIZE;
            vert[1][2] = ztop;
            vert[2][2] = zbot;
            vert[3][2] = zbot-TICK_SIZE;
            set_line_width(5); /* WLH 3-5-96 */
            disjointpolyline( vert, 4 );
            set_line_width(dtx->LineWidth); /* WLH 3-5-96 */
            clipping_on();
         }
      }
   }
}




static void render_trajectories( Context ctx, int it, int tf )
{
   int alpha, i, len, start;
   Display_Context dtx;

   dtx = ctx->dpy_ctx;
   for (i=0;i<dtx->NumTraj;i++) {
      struct traj *t = dtx->TrajTable[i];

      if (t->ctx_owner==ctx->context_index
          && dtx->DisplayTraj[t->group]
           && cond_read_lock(&t->lock)){

         assert( t->lock==1 );

         recent( ctx, TRAJ, t->group );
         alpha = UNPACK_ALPHA( dtx->TrajColor[t->group]);
         if ( (tf && alpha==255) || (tf==0 && alpha<255) ) {
            start = t->start[it];
            len = t->len[it];
            if (start!=0xffff && len>0) {
               if (t->kind==0) {
                  /* draw as line segments */
                  int colorvar = t->colorvar;
                  if (colorvar>=0) {
                     /* draw colored trajectory */
                     draw_colored_polylines( len,
                                     (void *) (t->verts + start*3),
                                     (void*)(t->colors + start),
                                     dtx->TrajColors[t->colorvarowner*MAXVARS+
                                     colorvar]);
                  }
                  else {
                     /* monocolored */
                     draw_polylines( len,
                                     (void *) (t->verts + start*3),
                                     dtx->TrajColor[t->group] );
                  }
               }
               else {
                  /* draw as triangle strip */
                  int colorvar = t->colorvar;
                  if (colorvar>=0) {
                     /* draw colored triangles */
                     draw_colored_triangle_strip( len,
                                       (void*)(t->verts + start*3),
                                       (void*)(t->norms + start*3),
                                       (void*)(t->colors + start),
                                       dtx->TrajColors[t->colorvarowner*MAXVARS+
                                       colorvar], alpha );
                  }
                  else {
                     /* monocolor */
                     draw_triangle_strip( len,
                                       (void*)(t->verts + start*3),
                                       (void*)(t->norms + start*3),
                                       dtx->TrajColor[t->group] );
                  }
               }
            }
         }
         done_read_lock( &t->lock );
      }
   }
}




/*
 * Draw the clock in the upper-left corner of the 3-D window.
 * Input:  ctx - the vis5d context
 *         c - the color to use.
 */
/* MJK 12.02.98 begin */
static void draw_clock( Display_Context dtx, unsigned int c )
{
   static char day[7][20] = {"Sunday", "Monday", "Tuesday", "Wednesday",
                             "Thursday", "Friday", "Saturday" };
   static char month[12][4] = {"Jan", "Feb", "Mar", "Apr", "May", "Jun",
                               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
   static int dds[24] = {31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365,
                         31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366};

   static float twopi = 2.0 * 3.141592;
   short pp[CLOCK_SEGMENTS+1][2];
   float ang, delta;
   float clk_size, clk_margin, clk_radius, clk_center_x, clk_center_y;
   char str[12];
   int i, time_str_width;
   int yo, stime, stimeold, dtime, dtimeold, spandex;

   clk_size     = 4*(dtx->FontHeight+VSPACE);
   clk_margin   = clk_size / 16.0;
   clk_radius   = (clk_size / 2.0) - clk_margin;
   clk_center_x = clk_size / 2.0;
   clk_center_y = clk_size / 2.0;

   /* Draw the clock. */
   if (dtx->NumTimes)
      ang = twopi * (float) dtx->CurTime / (float) dtx->NumTimes;
   else
      ang = 0.0;

   pp[0][1] = clk_center_y;
   pp[0][0] = clk_center_x;
   pp[1][1] = clk_center_y - (clk_radius * cos(ang));
   pp[1][0] = clk_center_x + (clk_radius * sin(ang));
   pp[2][1] = pp[1][1] + 1;
   pp[2][0] = pp[1][0] + 1;
   pp[3][1] = pp[0][1] + 1;
   pp[3][0] = pp[0][0] + 1;
   pp[4][1] = pp[0][1] - 1;
   pp[4][0] = pp[0][0] + 1;
   pp[5][1] = pp[1][1] - 1;
   pp[5][0] = pp[1][0] + 1;

   set_color( c );
   polyline2d( pp, 6 );

   if (dtx->CircleClock){
      /* Draw a circle around the clock. */
      delta = twopi / ((float) CLOCK_SEGMENTS);
      ang   = 0.0;
      for (i = 0; i < CLOCK_SEGMENTS; i++)
      {
         pp[i][0] = clk_center_x + (clk_radius * sin (ang)) + 0.5;
         pp[i][1] = clk_center_y - (clk_radius * cos (ang)) + 0.5;
         ang += delta;
      }
      pp[i][0] = pp[0][0];
      pp[i][1] = pp[0][1];
      polyline2d (pp, CLOCK_SEGMENTS+1);
   }



   clk_size += clk_margin;

   dtimeold = -1;
   stimeold = -1;
   vis5d_get_dtx_time_stamp( dtx->dpy_context_index,
                             dtx->CurTime,
                            &dtime, &stime);
   dtimeold = dtime;
   stimeold = stime;
   i = stimeold;
   sprintf( str, "%02d:%02d:%02d", i/3600, (i/60)%60, i%60 );
   draw_text( clk_size, dtx->FontHeight+VSPACE, str );
   time_str_width = text_width (str);

   if (dtx->JulianDate) {
     sprintf( str, "%05d", v5dDaysToYYDDD( dtimeold ));
   }
   else {
     int iy, im, id, days, day, mon;
     days = dtimeold;

     /* next two lines from from v5dDaysToYYDDD */
     iy = (4 * days) / 1461;
     id = days - (365 * iy + (iy - 1) / 4);
     if (iy > 99) iy -= 100; /* MJK 5.11.99 */

     im = (iy % 4) == 0 ? 12 : 0;
     for (i=im; i<im+12; i++) {
       if (id <= dds[i]) {
         mon = i-im;
         if (mon > 0) id = id - dds[i-1];
         break;
       }
     }
     sprintf(str, "%02d %s %02d", id, month[mon], iy);
   }
   draw_text( clk_size, 2*(dtx->FontHeight+VSPACE), str );

   sprintf( str, "%d of %d", dtx->CurTime+1, dtx->NumTimes );
   draw_text( clk_size, 3*(dtx->FontHeight+VSPACE), str );

   if (dtx->NumTimes == 1 ||
       ((dtx->Elapsed[dtx->NumTimes-1] - dtx->Elapsed[0])
         / (dtx->NumTimes - 1)) < 48*3600 ) {
     /* Print day of week */
     draw_text( clk_size, 4*(dtx->FontHeight+VSPACE),
                day[ (dtimeold+0) % 7 ] );
   }
   if (dtx->group_index > 0){
      sprintf( str, "   Group %d", dtx->group_index);
      draw_text( (clk_size + time_str_width), (dtx->FontHeight+VSPACE), str );
   }
}
/* MJK 12.02.98 end */



/*
 * Render all the 2-D text labels.
 */
static void render_text_labels( Display_Context dtx )
{
   struct label *lab;

  for (lab=dtx->FirstLabel; lab; lab=lab->next) {
    draw_text( lab->x, lab->y, lab->text );
    if (lab->state) {
       /* being edited -> draw cursor */
       short verts[4][2];
       verts[0][0] = lab->x2;     verts[0][1] = lab->y1;
       verts[1][0] = lab->x2;     verts[1][1] = lab->y2;
       verts[2][0] = lab->x2+1;   verts[2][1] = lab->y2;
       verts[3][0] = lab->x2+1;   verts[3][1] = lab->y1;
       polyline2d( verts, 4 );
    }
  }
}




static void draw_fake_pointer( Display_Context dtx )
{
   short pp[8][2];

   pp[0][0] = dtx->PointerX;       pp[0][1] = dtx->PointerY;
   pp[1][0] = dtx->PointerX+15;    pp[1][1] = dtx->PointerY+5;
   pp[2][0] = dtx->PointerX+5;     pp[2][1] = dtx->PointerY+15;
   pp[3][0] = dtx->PointerX;       pp[3][1] = dtx->PointerY;
   pp[4][0] = dtx->PointerX+20;    pp[4][1] = dtx->PointerY+20;

   polyline2d( pp, 5 );

}



/*
 * Print status info at bottom of window.
 */
static void print_info( Display_Context dtx )
{
   char str[1000];
   int m, size, waiters;

   m = mem_used( dtx );
   get_queue_info( &size, &waiters );
   if (m>=0)
      sprintf(str, "Pending: %d   Memory Used: %d", size, m );
   else
      sprintf(str, "Pending: %d", size );

   draw_text( 10, dtx->WinHeight - dtx->FontHeight, str );
}



/*
 * Print the numeric value of each variable at the probe's current location.
 */
/* MJK 12.02.98 begin */
static void draw_probe( Display_Context dtx )
{
   float val;
   char str[1000];
   int y, var;
   int x;
   int yo;
   float rr,cc,ll;

   /* find widest parameter name, but only once */
   if (!dtx->do_not_recalc_probe_text_width){
      x = -1;
      for (yo = 0; yo < dtx->numofctxs; yo++){
         for (var=0;var<dtx->ctxpointerarray[yo]->NumVars; var++) {
            int w = text_width( dtx->ctxpointerarray[yo]->VarName[var] );
            int l = strlen( dtx->ctxpointerarray[yo]->VarName[var] );
            if (w < 1) w = 11 * l;
            if (w>x)
               x = w;
         }
      }
      if (dtx->numofctxs >0){
         x += 25;
      }
      dtx->do_not_recalc_probe_text_width = 1;
      dtx->probe_text_width = x;
   }
   x = dtx->probe_text_width;

   /* Draw from bottom of window upward */
   y = dtx->WinHeight - dtx->FontHeight;
   for (yo = 0; yo < dtx->numofctxs; yo++){
      Context ctx;
      int ipvar, npvar, lpvar;
      ctx = dtx->ctxpointerarray[yo];
      if (ctx->ProbeNumVars >= 0) {
         npvar = ctx->ProbeNumVars;
         lpvar = 1;
      }
      else {
         npvar = ctx->NumVars - 1;
         lpvar = 0;
      }
      for (ipvar = npvar; ipvar >= lpvar; ipvar--) {
         var = ipvar;
         if (ctx->ProbeNumVars > 0) {
            for (var=ctx->NumVars-1;var>=0;var--) {
               if (ctx->ProbeVar[var] == ipvar) break;
            }
         }
         if (var >= 0) {
            float r, c, l;
            xyzPRIME_to_grid( ctx, ctx->CurTime, var,
                         dtx->CursorX, dtx->CursorY, dtx->CursorZ, &rr, &cc, &ll );
            xyzPRIME_to_gridPRIME( ctx->dpy_ctx, dtx->CurTime, var, dtx->CursorX,
                                   dtx->CursorY, dtx->CursorZ, &r, &c, &l);
            if (ll < ctx->LowLev[var] || ll > ctx->Nl[var]-1 + ctx->LowLev[var] ||
                rr < 0 || rr > ctx->Nr-1 || cc < 0 || cc > ctx->Nc-1 ||
                !check_for_valid_time(ctx, dtx->CurTime)) {
               val = MISSING;
            }
            else if (dtx->CoordFlag==1) {
               /* discrete grid position */
               int row = (int) (r+0.01);
               int col = (int) (c+0.01);
               int lev = (int) (l+0.01);
               if (ctx->GridSameAsGridPRIME){
                  val = get_grid_value( ctx, ctx->CurTime, var, row, col, lev );
               }
               else{
                  vis5d_gridPRIME_to_grid(ctx->context_index, ctx->CurTime, var,
                                          (float)(row), (float)(col), (float)(lev),
                                           &rr, &cc, &ll);
                  if (ll < ctx->LowLev[var] || ll > ctx->Nl[var]-1 + ctx->LowLev[var] ||
                      rr < 0 || rr > ctx->Nr-1 || cc < 0 || cc > ctx->Nc-1) {
                     val = MISSING;
                  }
                  else{
                     val = get_grid_value( ctx, ctx->CurTime, var, rr, cc, ll);
                  }
               }
            }
            else {
               if (ctx->GridSameAsGridPRIME){
                  val = interpolate_grid_value( ctx, ctx->CurTime, var, r, c, l );
               }
               else{
                  val = interpolate_grid_value( ctx, ctx->CurTime, var, rr, cc, ll );
               }
            }
            sprintf( str, "%-4s", ctx->VarName[var] );


            if (dtx->numofctxs > 1){
               char yodle[40];
               strcpy(yodle, return_var_plus_index(str, ctx->context_index));
               draw_text( 10, y, yodle );
            }
            else{
               draw_text( 10, y, str);
            }
            if (IS_MISSING(val))
              sprintf( str, " = MISSING" );
            else
              sprintf( str, " = %.3g %s", val, ctx->Units[var] );
            draw_text( x+10, y, str );
            y -= (dtx->FontHeight+VSPACE);
         }
      }
   }
}
/* MJK 12.02.98 end */


/*
#define LEGEND_SPACE   20
#define LEGEND_WIDTH   25
#define LEGEND_HEIGHT 128
*/
#define TICK_LENGTH     4
#define NUM_TICKS       5


/*
 * Draw a color legend.
 * Input: var    = parameter index the color slice belongs to for which to
 *                draw the legend,
 *        type  = CHSLICE or CVSLICE,
 *        xleft = x-position for left side of color bar,
 *        ybot  = y-position for bottom of color bar.
 * Return: width of bar + numbers drawn, or height of bar + label.
 */
int draw_legend( Context ctx, int varowner, int var, int type, int xleft, int ybot )
{
   int   y, yoffset,
         width,
         lutindex,
         tickspacing,
         textwidth;
   int tick;
   short cline[2][2];
   uint  *lut;
   char  scrap[100], format[20];
   int legend_space, legend_width, legend_height;
   float label;
   Display_Context dtx;
   Context colorctx;

   /* MJK 12.02.98 */
   int bg_r, bg_g, bg_b, bg_a, fg_r, fg_g, fg_b, fg_a;

   dtx = ctx->dpy_ctx;
   colorctx = dtx->ctxpointerarray[return_ctx_index_pos(dtx, varowner)];

   legend_height = dtx->LegendSize;
   legend_width = (25 * dtx->LegendSize) / 128;
   legend_space = (20 * dtx->LegendSize) / 128;

   switch(type) {
      case VIS5D_ISOSURF:
         lut = dtx->IsoColors[varowner*MAXVARS+var];
         break;
      case VIS5D_CHSLICE:
         lut = dtx->CHSliceColors[varowner*MAXVARS+var];
         break;
      case VIS5D_CVSLICE:
         lut = dtx->CVSliceColors[varowner*MAXVARS+var];
         break;
      case VIS5D_TRAJ:
         lut = dtx->TrajColors[varowner*MAXVARS+var];
         break;
      case VIS5D_VOLUME:
         lut = dtx->VolumeColors[varowner*MAXVARS+var];
         break;
      case VIS5D_TOPO:
         lut = dtx->TopoColorTable[varowner*MAXVARS+var];
         break;
      default:
         /* this should never happen */
         abort();
   }

   /* These line values never change */
   cline[0][0] = xleft;
   cline[1][0] = xleft + legend_width;

   /* Draw the colors */

   /* MJK 12.02.98 begin */

   bg_r = UNPACK_RED(dtx->BgColor);
   bg_g = UNPACK_GREEN(dtx->BgColor);
   bg_b = UNPACK_BLUE(dtx->BgColor);

   for (y = 0; y<legend_height; y++) {
      lutindex = (255 * y)/legend_height;
      cline[0][1] = cline[1][1] = ybot - y;

      fg_r = UNPACK_RED(lut[lutindex]);
      fg_g = UNPACK_GREEN(lut[lutindex]);
      fg_b = UNPACK_BLUE(lut[lutindex]);
      fg_a = UNPACK_ALPHA(lut[lutindex]);
      bg_a = 255 - fg_a;
      fg_r = ((fg_r * fg_a) + (bg_r * bg_a)) / 255;
      fg_g = ((fg_g * fg_a) + (bg_g * bg_a)) / 255;
      fg_b = ((fg_b * fg_a) + (bg_b * bg_a)) / 255;

      set_color (PACK_COLOR(fg_r, fg_g, fg_b, 255));
      polyline2d(cline, 2);

   }

   /* Draw a box around the legend colorbar */
      /* MJK 3.29.99 */
   if (dtx->Reversed) {
      set_color(PACK_COLOR( 0, 0, 0, 255));
   }
   else{
      set_color( dtx->BoxColor );
   }
   cline[0][0] = cline[1][0] = xleft;
   cline[0][1] = ybot;
   cline[1][1] = ybot - legend_height + 1;
   polyline2d(cline, 2);
   cline[0][0] = cline[1][0] = xleft + legend_width;
   polyline2d(cline, 2);
   cline[0][0] = xleft;
   cline[1][0] = xleft + legend_width;
   cline[0][1] = cline[1][1] = ybot;
   polyline2d(cline, 2);
   cline[0][1] = cline[1][1] = ybot - legend_height + 1;
   polyline2d(cline, 2);

   /* MJK 12.02.98 end */


   /* Determine largest value physical variable can have */
   label = ABS(colorctx->MaxVal[var]);
   if (ABS(colorctx->MinVal[var]) > label)
      label = ABS(colorctx->MinVal[var]);

   /* Create 'pretty' formatting string */
   sprintf(scrap, "% .0f", label); 
   sprintf(format, "%% %d.2f", strlen(scrap)+3);

   /* Draw values and tick marks on the right hand side of the legend */
   textwidth = 0;

   tickspacing = 4*legend_height/(dtx->FontHeight);

   /* Make sure we have a tick at the top of the legend @@ */

   cline[0][0] += TICK_LENGTH + legend_width;
      /* MJK 3.29.99 */
   if (dtx->Reversed) {
      set_color(PACK_COLOR( 0, 0, 0, 255));
   }
   else{
      set_color( dtx->BoxColor );
   }
XSync( GfxDpy, 0 );
   for (tick=0;tick<NUM_TICKS;tick++) {
      int ticky, texty;
      float value;

      ticky = ybot - tick * (legend_height-1) / (NUM_TICKS-1);
      texty = ybot - tick * (legend_height-dtx->FontHeight+dtx->FontDescent)
              / (NUM_TICKS-1);
      value = colorctx->MinVal[var] + (colorctx->MaxVal[var]-colorctx->MinVal[var])*tick/4.0;

      cline[0][1] = cline[1][1] = ticky;
      polyline2d(cline, 2);
      
      sprintf(scrap, format, value);
      draw_text( xleft + legend_width + TICK_LENGTH + 2, texty, scrap );
XSync( GfxDpy, 0 );
      if (text_width(scrap) > textwidth)
         textwidth = text_width(scrap);
   }

   /* Print name of physical variable above legend */
   if (colorctx->Units[var][0]) {
      sprintf( scrap, "%s (%s)", colorctx->VarName[var], colorctx->Units[var] );
      draw_text( xleft, ybot - legend_height - dtx->FontDescent-2, scrap );
   }
   else {
      draw_text( xleft, ybot - legend_height - dtx->FontDescent-2,
                 colorctx->VarName[var]);
   }

   if (dtx->LegendPosition == VIS5D_BOTTOM ||
       dtx->LegendPosition == VIS5D_TOP) {
     return legend_width + TICK_LENGTH + 5 + textwidth + legend_space;
   }
   else {
     return legend_height + 5 + dtx->FontHeight + legend_space;
   }
}


/*
 * Draws color legends of activated color slices.
 * Since the space in the 3-D window is restricted only one row of legends
 * is drawn in the bottom of the 3-D window. The order of drawing is:
 * first the legends of the horizontal slices for parameter 0..NumVars-1
 * and then the vertical slices for parameter 0..NumVars-1.
 */
static void draw_color_legends( Display_Context dtx )
{
   int var, set;
   int left;      /* Left x position of current legend */
   int bottom;    /* Bottom y position of current legend */ 
   int inc, vert;
   int vindex, dindex;
   int ctxnum, cvar, cvowner;
   Context ctx;

   
   dindex = dtx->dpy_context_index;
   if (dtx->LegendPosition == VIS5D_BOTTOM) {
     left = 50+dtx->LegendMarginX;
     bottom  = dtx->WinHeight - 20 + dtx->LegendMarginY;
     vert = 0;
   }
   else if (dtx->LegendPosition == VIS5D_TOP) {
     left = 200+dtx->LegendMarginX;
     bottom  = dtx->LegendSize + 5 + dtx->FontHeight + 25 + dtx->LegendMarginY;
     vert = 0;
   }
   else if (dtx->LegendPosition == VIS5D_RIGHT) {
     left = dtx->WinWidth - ((35 * dtx->LegendSize) / 128) - 5 * dtx->FontHeight+dtx->LegendMarginX;
     bottom  = dtx->LegendSize + 5 + dtx->FontHeight + 50 + dtx->LegendMarginY;
     vert = 1;
   }
   else if (dtx->LegendPosition == VIS5D_LEFT) {
     left = 20+dtx->LegendMarginX;
     bottom  = dtx->LegendSize + 5 + dtx->FontHeight + 100 + dtx->LegendMarginY;
     vert = 1;
   }
   else {
     printf("draw_color_legends: bad LegendPosition\n");
     return;
   }

   /* Isosurface color tables */
   for (ctxnum=0; ctxnum<dtx->numofctxs; ctxnum++){
      ctx = dtx->ctxpointerarray[ctxnum];
      for (var=0; var<ctx->NumVars; var++) {
         cvar = ctx->IsoColorVar[var];
         cvowner = ctx->IsoColorVarOwner[var];
         if (ctx->DisplaySurf[var] && cvar>=0 ){
            /* Draw legend at position (xstart, ystart) = upper left corner */
            inc = draw_legend( ctx, cvowner, cvar, VIS5D_ISOSURF, left, bottom ); 
            if (vert) {
              bottom += inc;
              if (bottom > dtx->WinHeight - 50) return;
            }
            else {
              left += inc;
              if (left > dtx->WinWidth - 150) return;
            }
         }
      }
   }

   /* Find activated horizontal color slices */
   for (ctxnum=0; ctxnum<dtx->numofctxs; ctxnum++){
      ctx = dtx->ctxpointerarray[ctxnum];
      for (var=0; var<ctx->NumVars; var++) {
         if (ctx->DisplayCHSlice[var]) {
            /* Draw legend at position (xstart, ystart) = upper left corner */
            vindex = ctx->context_index;
            inc = draw_legend( ctx, vindex, var, VIS5D_CHSLICE, left, bottom ); 
            if (vert) {
              bottom += inc;
              if (bottom > dtx->WinHeight - 50) return;
            }
            else {
              left += inc;
              if (left > dtx->WinWidth - 150) return;
            }
         }
      }
   }

   /* Find activated vertical color slices */
   for (ctxnum=0; ctxnum<dtx->numofctxs; ctxnum++){
      ctx = dtx->ctxpointerarray[ctxnum];
      for (var=0; var<ctx->NumVars; var++) {
         if (ctx->DisplayCVSlice[var]) {
            /* Draw legend at position (xstart, ystart) = upper left corner */
            vindex = ctx->context_index;
            inc = draw_legend( ctx, vindex, var, VIS5D_CVSLICE, left, bottom ); 
            if (vert) {
              bottom += inc;
              if (bottom > dtx->WinHeight - 50) return;
            }
            else {
              left += inc;
              if (left > dtx->WinWidth - 150) return;
            }
         }
      }
   }

   /* Volume */
   if (dtx->CurrentVolume>=0  ) {
      /* Draw legend at position (xstart, ystart) = upper left corner */
      inc = draw_legend( ctx, dtx->CurrentVolumeOwner,
                         dtx->CurrentVolume, VIS5D_VOLUME, left, bottom ); 
      if (vert) {
        bottom += inc;
        if (bottom > dtx->WinHeight - 50) return;
      }
      else {
        left += inc;
        if (left > dtx->WinWidth - 150) return;
      }
   }

   /* Trajectory color tables */
   for (set=0; set<VIS5D_TRAJ_SETS; set++) {
      int cvar = dtx->TrajColorVar[set];
      int cvowner = dtx->TrajColorVarOwner[set];
      if (dtx->DisplayTraj[set] && cvar>=0 ){ 
         /* Draw legend at position (xstart, ystart) = upper left corner */
         inc = draw_legend( ctx, cvowner, cvar, VIS5D_TRAJ, left, bottom ); 
         if (vert) {
           bottom += inc;
           if (bottom > dtx->WinHeight - 50) return;
         }
         else {
           left += inc;
           if (left > dtx->WinWidth - 150) return;
         }
      }
   }

   /* Topo color table */
   if (dtx->TopoColorVar>=0 && dtx->DisplayTopo) {
      int cvar = dtx->TopoColorVar;
      int cvowner = dtx->TopoColorVarOwner;
      /* Draw legend at position (xstart, ystart) = upper left corner */
      inc = draw_legend( ctx, cvowner, cvar, VIS5D_TOPO, left, bottom ); 
      if (vert) {
        bottom += inc;
        if (bottom > dtx->WinHeight - 50) return;
      }
      else {
        left += inc;
        if (left > dtx->WinWidth - 150) return;
      }
   }

}




/*
 * Draw anything the user wants in 3D.
 * Drawing bounds are (Xmin,Ymin,Zmin) - (Xmax,Ymax,Zmax)
 */
static void draw_user_3d_graphics( Display_Context dtx )
{
}



/*
 * Draw anything the user wants in 2D.
 * Drawing bounds are (0,0) - (Width-1, Height-1), origin in upper-left corner.
 */
static void draw_user_2d_graphics( Display_Context dtx )
{

}




/*
 * Only draw the 3-D elements of the scene.  No matrix, viewport, etc
 * operations are done here.  This function is useful for the CAVE
 * since it controls the viewing parameters.
 * Input:  ctx - the context
 *         animflag - 1=animating, 0=not animating
 */
void render_3d_only( Display_Context dtx, int animflag )
{
   int yo, spandex, ctime, labels, i;
   Context ctx;
   Irregular_Context itx;

   if (animflag){
      labels = !dtx->ContnumFlag;
   }
   else{
      labels = dtx->ContnumFlag;
   }

   /* Loop over antialiasing passes */
   for (i=0; i < (dtx->PrettyFlag ? AA_PASSES : 1); i++) {

      start_aa_pass(i);

      /*** Draw 3-D lines ***/

      clipping_off();
      /* MJK 12.02.98 begin */
      if (dtx->DisplayCursor)
      {
         if (dtx->DisplayProbe)
         {

         /* MJK 3.29.99 */
            if (dtx->Reversed){
               draw_cursor (dtx, 0, dtx->CursorX, dtx->CursorY, dtx->CursorZ,
                            PACK_COLOR(0,0,0,255));
            }
            else{
               draw_cursor (dtx, 0, dtx->CursorX, dtx->CursorY, dtx->CursorZ,
                            dtx->BoxColor);
            }



         }
         else if (dtx->DisplaySound)
         {
         /* MJK 3.29.99 */
            if (dtx->Reversed){
               draw_cursor (dtx, 2, dtx->CursorX, dtx->CursorY, 0,
                            PACK_COLOR(0,0,0,255));
            }
            else{
               draw_cursor (dtx, 2, dtx->CursorX, dtx->CursorY, 0,
                            dtx->BoxColor);
            }

         }
         else
         {
            draw_cursor (dtx, dtx->RibbonFlag,
                         dtx->CursorX, dtx->CursorY, dtx->CursorZ,
                         *dtx->CursorColor);
         }

         if (dtx->DisplayBox)
         {
            print_cursor_position (dtx, dtx->CurTime);
         }
      }
      /* MJK 12.02.98 end */

      clipping_on();

      for (yo = 0; yo < dtx->numofitxs; yo++){
         spandex = dtx->TimeStep[dtx->CurTime].owners[yo];
         itx = dtx->itxpointerarray[yo];
         render_textplots( itx, itx->CurTime);
      }

      for (yo= 0; yo < dtx->numofctxs; yo++){
         spandex = dtx->TimeStep[dtx->CurTime].owners[yo];
         ctx = dtx->ctxpointerarray[yo];
         if (check_for_valid_time(ctx, dtx->CurTime)){
            render_hslices( ctx, ctx->CurTime, labels, animflag );
         }
      }

      for (yo= 0; yo < dtx->numofctxs; yo++){
         spandex = dtx->TimeStep[dtx->CurTime].owners[yo];
         ctx = dtx->ctxpointerarray[yo];
         if (check_for_valid_time(ctx, dtx->CurTime)){
            render_vslices( ctx, ctx->CurTime, labels, animflag );
         }   
      }

      for (yo= 0; yo < dtx->numofctxs; yo++){
         spandex = dtx->TimeStep[dtx->CurTime].owners[yo];
         ctx = dtx->ctxpointerarray[yo];
         if (check_for_valid_time(ctx, dtx->CurTime)){
            render_hwind_slices( ctx, ctx->CurTime, animflag );
         }   
      }

      for (yo= 0; yo < dtx->numofctxs; yo++){
         spandex = dtx->TimeStep[dtx->CurTime].owners[yo];
         ctx = dtx->ctxpointerarray[yo];
         if (check_for_valid_time(ctx, dtx->CurTime)){         
            render_vwind_slices( ctx, ctx->CurTime, animflag );
         }   
      }
      
      for (yo= 0; yo < dtx->numofctxs; yo++){
         spandex = dtx->TimeStep[dtx->CurTime].owners[yo];
         ctx = dtx->ctxpointerarray[yo]; 
         if (check_for_valid_time(ctx, dtx->CurTime)){         
            render_hstream_slices( ctx, ctx->CurTime, animflag );
         }   
      }
      
      for (yo= 0; yo < dtx->numofctxs; yo++){
         spandex = dtx->TimeStep[dtx->CurTime].owners[yo];
         ctx = dtx->ctxpointerarray[yo]; 
         if (check_for_valid_time(ctx, dtx->CurTime)){         
            render_vstream_slices( ctx, ctx->CurTime, animflag );
         }   
      }


      /* draw user graphics */
      draw_user_3d_graphics( dtx );



      /*** Draw opaque 3-D graphics ***/
      /* MJK 12.02.98 begin */
      set_depthcue( dtx->DepthCue );
      if (dtx->TopoFlag && dtx->DisplayTopo) {
         set_depthcue(0);
         draw_topo( dtx, dtx->CurTime, dtx->DisplayTexture, 0 );
      }
      else if (dtx->DisplayTexture) {
         /* just draw flat textured image */
         set_depthcue(0);
         draw_topo( dtx, dtx->CurTime, 1, 1 );
      }

      if (dtx->MapFlag && dtx->DisplayMap) {
         if (dtx->DisplaySfcMap) {
            set_color( dtx->DarkMapColor );
            draw_map( dtx, dtx->CurTime, 0 );
         }
         else {
            set_color( dtx->LightMapColor );
            draw_map( dtx, dtx->CurTime, 1 );
         }


      /* MJK 3.29.99 */
         if (dtx->Reversed){
            set_color( PACK_COLOR(0,0,0,255) );
         }
         else{
            set_color( dtx->BoxColor );
         }

      }
      set_depthcue(0);
      /* MJK 12.02.98 end */


      for (yo= 0; yo < dtx->numofctxs; yo++){      
         spandex = dtx->TimeStep[dtx->CurTime].owners[yo];      
         ctx = dtx->ctxpointerarray[yo];       
         if (check_for_valid_time(ctx, dtx->CurTime)){
            render_trajectories( ctx, ctx->CurTime, 1);
         }
      }     

      for (yo= 0; yo < dtx->numofctxs; yo++){
         spandex = dtx->TimeStep[dtx->CurTime].owners[yo];
         ctx = dtx->ctxpointerarray[yo];
         if (check_for_valid_time(ctx, dtx->CurTime)){
            render_isosurfaces( ctx, dtx->CurTime, ctx->CurTime, 1, animflag );
         }      
      }

      for (yo= 0; yo < dtx->numofctxs; yo++){
         spandex = dtx->TimeStep[dtx->CurTime].owners[yo];
         ctx = dtx->ctxpointerarray[yo];
         if (check_for_valid_time(ctx, dtx->CurTime)){
            render_chslices( ctx, ctx->CurTime, 1, animflag );
         }      
      }

      for (yo= 0; yo < dtx->numofctxs; yo++){
         spandex = dtx->TimeStep[dtx->CurTime].owners[yo];
         ctx = dtx->ctxpointerarray[yo];
         if (check_for_valid_time(ctx, dtx->CurTime)){
            render_cvslices( ctx, ctx->CurTime, 1, animflag );
         }      
      }



      /*** Draw transparent 3-D objects ***/



      for (yo= 0; yo < dtx->numofctxs; yo++){
         spandex = dtx->TimeStep[dtx->CurTime].owners[yo];
         ctx = dtx->ctxpointerarray[yo];
         if (check_for_valid_time(ctx, dtx->CurTime)){            
            render_trajectories( ctx, ctx->CurTime, 0);
         }            
      }

      for (yo= 0; yo < dtx->numofctxs; yo++){
         spandex = dtx->TimeStep[dtx->CurTime].owners[yo];
         ctx = dtx->ctxpointerarray[yo];
         if (check_for_valid_time(ctx, dtx->CurTime)){            
            render_isosurfaces( ctx, dtx->CurTime, ctx->CurTime, 0, animflag );
         }            
      }

      for (yo= 0; yo < dtx->numofctxs; yo++){
         spandex = dtx->TimeStep[dtx->CurTime].owners[yo];
         ctx = dtx->ctxpointerarray[yo];
         if (check_for_valid_time(ctx, dtx->CurTime)){            
            render_chslices( ctx, ctx->CurTime, 0, animflag );
         }            
      }

      for (yo= 0; yo < dtx->numofctxs; yo++){
         spandex = dtx->TimeStep[dtx->CurTime].owners[yo];
         ctx = dtx->ctxpointerarray[yo];
         if (check_for_valid_time(ctx, dtx->CurTime)){            
            render_cvslices( ctx, ctx->CurTime, 0, animflag );
         }            
      }

      if (dtx->CurrentVolume!=-1){
         ctx = dtx->ctxpointerarray[return_ctx_index_pos(dtx,
                                    dtx->CurrentVolumeOwner)];
         
         if (check_for_valid_time(ctx, dtx->CurTime)){                     
            draw_volume( ctx, ctx->CurTime, dtx->CurrentVolume,
                      dtx->VolumeColors[ctx->context_index*MAXVARS+
                       dtx->CurrentVolume] );
         }
      }

      end_aa_pass(i);

   } /* aa passes */
}



/*
 * Only draw the 2-D elements of the scene.  No matrix, viewport, etc
 * operations are done here.
 * Input:  ctx - the context
 */
void render_2d_only( Display_Context dtx )
{

   if (dtx->DisplayClock) {
   /* MJK 3.29.99 */
      if (dtx->Reversed){
         draw_clock( dtx, PACK_COLOR(0,0,0,255) );
         draw_logo( dtx, PACK_COLOR(0,0,0,255) );
      }
      else{
         draw_clock( dtx, dtx->BoxColor );
         draw_logo( dtx, dtx->BoxColor );
      }
   }
   if (dtx->DisplayInfo) {
      print_info(dtx);
   }
   if (dtx->DisplayProbe) {
      draw_probe(dtx);
   }
   if (dtx->DisplayCursor && dtx->DisplayBox) {
      print_cursor_position( dtx, dtx->CurTime );
   }

   if (dtx->PointerX>=0 && dtx->PointerY>=0){
      draw_fake_pointer(dtx);  /* for remote widget mode */
   }

   /* MJK 3.29.99 */
   if (dtx->Reversed){
      set_color( PACK_COLOR(0,0,0,255) );
   }
   else{
      set_color( dtx->LabelColor );
   }


   render_text_labels(dtx);

   /* Draw color map legends of color slices (as much as fit in window) */
   if (dtx->DisplayLegends ) {
      draw_color_legends(dtx);
   }

   draw_user_2d_graphics( dtx );
}

/*
 *
 * draw everything that belongs inside of
 * the Sounding Graphics Window
 *
 *
 */
void render_sounding_only( Display_Context dtx, int pixmapflag )
{  
   if (dtx->DisplaySound) {
      if ( pixmapflag == 1 ){
         do_pixmap_art( dtx );
         /* MJK 12.02.98 */
         /*
         draw_sounding(dtx, dtx->CurTime);
         */
      }
      if ((dtx->CursorX != dtx->Sound.currentX || 
           dtx->CursorY != dtx->Sound.currentY) ||
           (dtx->CurTime != dtx->Sound.currentTime) ||
           /* MJK 12.02.98 */ (pixmapflag)){
         if (dtx->CurTime != dtx->Sound.currentTime){
            reload_sounding_data( dtx );
         }
         draw_sounding(dtx, dtx->CurTime); 
         dtx->Sound.currentX = dtx->CursorX;
         dtx->Sound.currentY = dtx->CursorY;
         dtx->Sound.currentTime = dtx->CurTime;
      } 
   }
}



/*
 * Redraw everything in the 3-D window but don't display it yet.  Call
 * swap_3d_window() to do that.
 * Input:  ctx - the vis5d context
 *         animflag - 1=animating, 0=notanimating
 * Return:  0 = ok, -1 = error.
 */
void render_everything( Display_Context dtx, int animflag )
{
 static int num = 0;   

   if (get_frame(dtx, dtx->CurTime)) {
      return;
   }

   /* MJK 12.02.98 */
   clear_color (dtx->BgColor);


   /*** Draw 3-D Objects ***/
   set_3d( dtx->GfxProjection, dtx->FrntClip,
           dtx->Zoom, (float*) dtx->CTM);

/* WLH 3 July 2000 */
   if (dtx->DisplayBox){
      int i;
      for (i=0; i < (dtx->PrettyFlag ? AA_PASSES : 1); i++) {
         start_aa_pass(i);
         draw_box(dtx, dtx->CurTime);
         /* draw_tick_marks( dtx ); */
         end_aa_pass(i);
      }
   }

   clipping_on();
   render_3d_only( dtx, animflag );

   /*** Draw box now, but first disable clipping planes ***/
   clipping_off();

/* WLH 3 July 2000
   if (dtx->DisplayBox){
      int i;
      for (i=0; i < (dtx->PrettyFlag ? AA_PASSES : 1); i++) {
         start_aa_pass(i);
         draw_box(dtx, dtx->CurTime);
         draw_tick_marks( dtx );
         end_aa_pass(i);
      }
   }
*/
   if (dtx->DisplayClips){
      render_vclips( dtx, animflag );
      render_hclips( dtx, animflag );
   }


   /*** Draw 2-D objects ***/
   set_2d();
   render_2d_only( dtx );

   /*** Draw Sounding ***/
   render_sounding_only( dtx , 0); 

   if (dtx->AnimRecord) {
      save_frame( dtx, dtx->CurTime ); 
   }
   finish_rendering();
}

