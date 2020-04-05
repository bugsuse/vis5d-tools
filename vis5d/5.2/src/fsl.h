/* Vis5D version 5.2 */

/*
Vis5D system for visualizing five dimensional gridded data sets
Copyright (C) 1990 - 2000 Bill Hibbard, Brian Paul, Dave Santek,
and Andre Battaiola.

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

#ifndef FSL_H 
#define FSL_H

/* MJK 12.01.98 */

int follow_slice_link (int index, int *p_type, int *p_num);

int move_linked_slices (int index, int type, int num);

int make_linked_slices (int index, int time, int type, int num, int urgent);

int enable_linked_slices (int index, int type, int num, int mode);

int enable_linked_sfc_slices (int index, int type, int num, int mode);

int sync_linked_slices (int index, int type, int num);

int check_view_side (Context ctx, int type, int num);

int flip_vslice_end_for_end (Context ctx, int time, int var);



#endif

