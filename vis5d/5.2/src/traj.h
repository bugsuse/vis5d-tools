/* Vis5D version 5.2 */

/*
Vis5D system for visualizing five dimensional gridded data sets
Copyright (C) 1990-2000 Bill Hibbard, Brian Paul, Dave Santek,
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


/* trajectory tracing module */



#ifndef TRAJ_H
#define TRAJ_H



extern int init_traj( Context ctx );

extern int init_trajPRIME( Display_Context dtx);

extern int trace( Context ctx, float row0, float col0, float lev0,
                  int time0, int step, int max,
                  float vr[], float vc[], float vl[], int vt[] );


extern int to_ribbon( int num, float xt[], float yt[], float zt[],
                      int itint[], float xn[], float yn[], float zn[] );


#endif
