/* fsl.c */

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

#include "api.h"
#include "globals.h"
#include "fsl.h"
#include "queue.h"
#include "proj.h"
#include "graphics.h"
#include "work.h"



/* 28May98  Phil McDonald */
/*
 *  Follow a slice link to the next slice in the chain.
 */
int follow_slice_link (int index, int *p_type, int *p_num)
{

   int *p_next_type, *p_next_num;


   if (vis5d_get_slice_link (index, *p_type, *p_num, &p_next_type, &p_next_num)
      != 0) return 0;

   *p_type = *p_next_type;
   *p_num  = *p_next_num;


   return 1;
}

/*
 *  Note that new_slice_pos (who calls move_linked_slices) uses
 *  the shorter slice names "SLICE", rather than "VIS5D_SLICE".
 */
int move_linked_slices (int index, int type, int num)
{
   static int   start_type = -1;
   static int   start_num = -1;

   int          cur_type, cur_num;
   float        level, row1, col1, row2, col2;
   float        xint, xlow, xhigh, xden, xscale, xlev, xr1, xc1, xr2, xc2;


   if (start_type != -1) return 0;

   switch (type)
   {
      case HSLICE:
         type = VIS5D_HSLICE;
         vis5d_get_hslice (index, num, &xint, &xlow, &xhigh, &level);
         break;

      case CHSLICE:
         type = VIS5D_CHSLICE;
         vis5d_get_chslice (index, num, &level);
         break;

      case HWIND:
         type = VIS5D_HWIND;
         vis5d_get_hwindslice (index, num, &xden, &xscale, &level);
         break;

      case HSTREAM:
         type = VIS5D_HSTREAM;
         vis5d_get_hstreamslice (index, num, &xden, &level);
         break;

      case VSLICE:
         type = VIS5D_VSLICE;
         vis5d_get_vslice (index, num, &xint, &xlow, &xhigh,
                           &row1, &col1, &row2, &col2);
         break;

      case CVSLICE:
         type = VIS5D_CVSLICE;
         vis5d_get_cvslice (index, num, &row1, &col1, &row2, &col2);
         break;

      case VWIND:
         type = VIS5D_VWIND;
         vis5d_get_vwindslice (index, num, &xden, &xscale,
                               &row1, &col1, &row2, &col2);
         break;

      case VSTREAM:
         type = VIS5D_VSTREAM;
         vis5d_get_vstreamslice (index, num, &xden,
                                 &row1, &col1, &row2, &col2);
         break;

      default:
         return 0;
   }

   start_type = type;
   start_num  = num;

   cur_type = type;
   cur_num  = num;
   while (follow_slice_link (index, &cur_type, &cur_num))
   {
      if ((cur_type == start_type) && (cur_num == start_num)) break;

      switch (cur_type)
      {
         case VIS5D_HSLICE:
            vis5d_get_hslice (index, cur_num, &xint, &xlow, &xhigh, &xlev);
            vis5d_set_hslice (index, cur_num, xint, xlow, xhigh, level);
            break;

         case VIS5D_CHSLICE:
            vis5d_set_chslice (index, cur_num, level);
            break;

         case VIS5D_HWIND:
            vis5d_get_hwindslice (index, cur_num, &xden, &xscale, &xlev);
            vis5d_set_hwindslice (index, cur_num, xden, xscale, level);
            break;

         case VIS5D_HSTREAM:
            vis5d_get_hstreamslice (index, cur_num, &xden, &xlev);
            vis5d_set_hstreamslice (index, cur_num, xden, level);
            break;

         case VIS5D_VSLICE:
            vis5d_get_vslice (index, cur_num, &xint, &xlow, &xhigh,
                              &xr1, &xc1, &xr2, &xc2);
            vis5d_set_vslice (index, cur_num, xint, xlow, xhigh,
                              row1, col1, row2, col2);
            break;

         case VIS5D_CVSLICE:
            vis5d_set_cvslice (index, cur_num, row1, col1, row2, col2);
            break;

         case VIS5D_VWIND:
            vis5d_get_vwindslice (index, cur_num, &xden, &xscale,
                                  &xr1, &xc1, &xr2, &xc2);
            vis5d_set_vwindslice (index, cur_num, xden, xscale,
                                  row1, col1, row2, col2);
            break;

         case VIS5D_VSTREAM:
            vis5d_get_vstreamslice (index, cur_num, &xden,
                                    &xr1, &xc1, &xr2, &xc2);
            vis5d_set_vstreamslice (index, cur_num, xden,
                                    row1, col1, row2, col2);
            break;
      }

   }

   start_type = -1;
   start_num  = -1;


   return 0;
}



int make_linked_slices (int index, int time, int type, int num, int urgent)
{
   static int   start_type = -1;
   static int   start_num = -1;

   int          cur_type, cur_num;
   int          qsize1, qsize2, qwaiters;


   if (start_type != -1) return 0;

   start_type = type;
   start_num  = num;

   cur_type = type;
   cur_num  = num;
   while (follow_slice_link (index, &cur_type, &cur_num))
   {
      if ((cur_type == start_type) && (cur_num == start_num)) break;

      get_queue_info (&qsize1, &qwaiters);

      switch (cur_type)
      {
         case VIS5D_HSLICE:
            vis5d_make_hslice (index, time, cur_num, urgent);
            break;
         case VIS5D_VSLICE:
            vis5d_make_vslice (index, time, cur_num, urgent);
            break;
         case VIS5D_CHSLICE:
            vis5d_make_chslice (index, time, cur_num, urgent);
            break;
         case VIS5D_CVSLICE:
            vis5d_make_cvslice (index, time, cur_num, urgent);
            break;
         case VIS5D_HWIND:
            vis5d_make_hwindslice (index, time, cur_num, urgent);
            break;
         case VIS5D_VWIND:
            vis5d_make_vwindslice (index, time, cur_num, urgent);
            break;
         case VIS5D_HSTREAM:
            vis5d_make_hstreamslice (index, time, cur_num, urgent);
            break;
         case VIS5D_VSTREAM:
            vis5d_make_vstreamslice (index, time, cur_num, urgent);
            break;
      }

      if (NumThreads == 1)
      {
         get_queue_info (&qsize2, &qwaiters);
         if (qsize2 > qsize1) do_one_task (0);
      }
   }

   start_type = -1;
   start_num  = -1;


   return 0;
}




/* 01Jun98  Phil McDonald */
int enable_linked_slices (int index, int type, int num, int mode)
{
   static int   start_type = -1;
   static int   start_num = -1;

   int          cur_type, cur_num;


   if (start_type != -1) return 0;

   start_type = type;
   start_num  = num;

   cur_type = type;
   cur_num  = num;
   while (follow_slice_link (index, &cur_type, &cur_num))
   {
      if ((cur_type == start_type) && (cur_num == start_num)) break;

      vis5d_enable_graphics (index, cur_type, cur_num, mode);
   }

   start_type = -1;
   start_num  = -1;

/*   if (vis5d_get_slice_link (index, type, num, NULL, NULL) == 0)
      show_widgets (index); */


   return 1;
}





/* 02Jun98  Phil McDonald */
int enable_linked_sfc_slices (int index, int type, int num, int mode)
{
   static int   start_type = -1;
   static int   start_num = -1;

   int          cur_type, cur_num;


   if (start_type != -1) return 0;

   start_type = type;
   start_num  = num;

   cur_type = type;
   cur_num  = num;
   while (follow_slice_link (index, &cur_type, &cur_num))
   {
      if ((cur_type == start_type) && (cur_num == start_num)) break;

      vis5d_enable_sfc_graphics (index, cur_type, cur_num, mode); 

   }

   start_type = -1;
   start_num  = -1;

/*   if (vis5d_get_slice_link (index, type, num, NULL, NULL) == 0)
      show_widgets (index);
*/

   return 1;
}


/* 22Oct98  Phil McDonald */
int sync_linked_slices (int index, int type, int num)
{
   static int   end_type = -1;
   static int   end_num = -1;

   int          cur_type, cur_num;


   if (end_type == -1)
   {
      cur_type = type;
      cur_num  = num;
      while (1)
      {
         end_type = cur_type;
         end_num  = cur_num;

         if (!follow_slice_link (index, &cur_type, &cur_num)) break;
         if ((cur_type == type) && (cur_num == num)) break;
      }
   }

   if ((type != end_type) || (num != end_num)) return 0;

   end_type = -1;
   end_num  = -1;


   return 1;
}



/* 01Jul98  Phil McDonald */
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

/* 01Jul98  Phil McDonald */
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

   move_linked_slices (ctx->context_index, VSLICE, var);
   make_linked_slices (ctx->context_index, time, VIS5D_VSLICE, var,
                       (time == ctx->CurTime));
}






