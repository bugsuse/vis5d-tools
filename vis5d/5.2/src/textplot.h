/* textplot.h */
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




#ifndef TEXTPLOT_H
#define TEXTPLOT_H

#define MAX_TEXT_PLOT_VERTS 100000

extern int create_num_textplot( Irregular_Context itx, int time, float xs[],
                      float ys[], float zs[], double *numdata, int ploton[],
                      float vx[], 
                      float vy[], float vz[], int *numv);

extern int create_letter_textplot( Irregular_Context itx, int time, float xs[],
                      float ys[], float zs[], char *chardata, int ploton[], int var,
                      float vx[],
                      float vy[], float vz[], int *numv);

extern int create_color_num_textplot( Irregular_Context itx, int time, float xs[],
                     float ys[], float zs[], double *numdata,
                     int ploton[], float vx[],
                     float vy[], float vz[], int *numv,
                     uint_1 *color_indexes);

extern void space_plots( Irregular_Context itx, int time,       
                     int ploton[], float xs[],
                     float ys[], float zs[], int *numtouse);

#endif

