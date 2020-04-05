/* textplot.c */
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
#include <math.h>
#include <stdio.h>
#include "memory.h"
#include "textplot.h"
#include "globals.h"
#include "api.h"

#define MAX_SYMBOLS 400 
#define MAX_SYMBOL_VERTS 35 
#define DISTANCE( x1, y1, x2, y2 )   sqrt( (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) )

static float textcoordx[MAX_SYMBOLS][MAX_SYMBOL_VERTS];
static float textcoordy[MAX_SYMBOLS][MAX_SYMBOL_VERTS];

static init_text(void)
{
   int i, j, k;

   for( i = 0; i < MAX_SYMBOLS; i++){
      for (j = 0; j < MAX_SYMBOL_VERTS; j++){
         textcoordx[i][j] = 27.0;
         textcoordy[i][j] = 27.0;
      }
   }
   
   textcoordx[0][0]=-0.3;
   textcoordy[0][0]=1.0;
   textcoordx[0][1]=-0.8;
   textcoordy[0][1]=0.25;

   textcoordx[0][2]=-0.8;
   textcoordy[0][2]=0.25;
   textcoordx[0][3]=-0.8;
   textcoordy[0][3]=-0.25;

   textcoordx[0][4]=-0.8;
   textcoordy[0][4]=-0.25;
   textcoordx[0][5]=-0.3;
   textcoordy[0][5]=-1.0;

   textcoordx[0][6]=-0.3;
   textcoordy[0][6]=-1.0;
   textcoordx[0][7]=0.3;
   textcoordy[0][7]=-1.0;

   textcoordx[0][8]=0.3;
   textcoordy[0][8]=-1.0;
   textcoordx[0][9]=0.8;
   textcoordy[0][9]=-0.25;

   textcoordx[0][10]=0.8;
   textcoordy[0][10]=-0.25;
   textcoordx[0][11]=0.8;
   textcoordy[0][11]=0.25;

   textcoordx[0][12]=0.8;
   textcoordy[0][12]=0.25;
   textcoordx[0][13]=0.3;
   textcoordy[0][13]=1.0;

   textcoordx[0][14]=0.3;
   textcoordy[0][14]=1.0;
   textcoordx[0][15]=-0.3;
   textcoordy[0][15]=1.0;


   /* 1 */
   textcoordx[1][0]=-0.2;
   textcoordy[1][0]=0.6;
   textcoordx[1][1]=0.0;
   textcoordy[1][1]=1.0;

   textcoordx[1][2]=0.0;
   textcoordy[1][2]=1.0;
   textcoordx[1][3]=0.0;
   textcoordy[1][3]=-1.0;

   textcoordx[1][4]=-0.2;
   textcoordy[1][4]=-1.0;
   textcoordx[1][5]=0.2;
   textcoordy[1][5]=-1.0;

   /* 2 */
   textcoordx[2][0]=-0.8;
   textcoordy[2][0]=0.6;
   textcoordx[2][1]=-0.3;
   textcoordy[2][1]=1.0;

   textcoordx[2][2]=-0.3;
   textcoordy[2][2]=1.0;
   textcoordx[2][3]=0.3;
   textcoordy[2][3]=1.0;

   textcoordx[2][4]=0.3;
   textcoordy[2][4]=1.0;
   textcoordx[2][5]=0.8;
   textcoordy[2][5]=0.6;

   textcoordx[2][6]=0.8;
   textcoordy[2][6]=0.6;
   textcoordx[2][7]=-0.8;
   textcoordy[2][7]=-1.0;

   textcoordx[2][8]=-0.8;
   textcoordy[2][8]=-1.0;
   textcoordx[2][9]=0.8;
   textcoordy[2][9]=-1.0;


   /* 3 */
   textcoordx[3][0]=-0.8;
   textcoordy[3][0]=1.0;
   textcoordx[3][1]=0.8;
   textcoordy[3][1]=1.0;
   
   textcoordx[3][2]=0.8;
   textcoordy[3][2]=1.0;
   textcoordx[3][3]=-0.8;
   textcoordy[3][3]=0.0;

   textcoordx[3][4]=-0.8;
   textcoordy[3][4]=0.0;
   textcoordx[3][5]=0.0;
   textcoordy[3][5]=0.1;
   
   textcoordx[3][6]=0.0;
   textcoordy[3][6]=0.1;
   textcoordx[3][7]=0.4;
   textcoordy[3][7]=0.1;

   textcoordx[3][8]=0.4;
   textcoordy[3][8]=0.1;
   textcoordx[3][9]=0.8;
   textcoordy[3][9]=-0.2;
   
   textcoordx[3][10]=0.8;
   textcoordy[3][10]=-0.2;
   textcoordx[3][11]=0.8;
   textcoordy[3][11]=-0.4;

   textcoordx[3][12]=0.8;
   textcoordy[3][12]=-0.4;
   textcoordx[3][13]=0.3;
   textcoordy[3][13]=-1.0;
   
   textcoordx[3][14]=0.3;
   textcoordy[3][14]=-1.0;
   textcoordx[3][15]=-0.3;
   textcoordy[3][15]=-1.0;

   textcoordx[3][16]=-0.3;
   textcoordy[3][16]=-1.0;
   textcoordx[3][17]=-0.8;
   textcoordy[3][17]=-0.7;

   /* 4 */
   textcoordx[4][0]=-0.8;
   textcoordy[4][0]=1.0;
   textcoordx[4][1]=-0.8;
   textcoordy[4][1]=0.0;

   textcoordx[4][2]=-0.8;
   textcoordy[4][2]=0.0;
   textcoordx[4][3]=0.8;
   textcoordy[4][3]=0.0;

   textcoordx[4][4]=0.2;
   textcoordy[4][4]=0.8;
   textcoordx[4][5]=0.2;
   textcoordy[4][5]=-1.0;


   /* 5 */
   textcoordx[5][0]=0.8;
   textcoordy[5][0]=1.0;
   textcoordx[5][1]=-0.8;
   textcoordy[5][1]=1.0;

   textcoordx[5][2]=-0.8;
   textcoordy[5][2]=1.0;
   textcoordx[5][3]=-0.8;
   textcoordy[5][3]=0.0;

   textcoordx[5][4]=-0.8;
   textcoordy[5][4]=0.0;
   textcoordx[5][5]=0.2;
   textcoordy[5][5]=0.1;

   textcoordx[5][6]=0.2;
   textcoordy[5][6]=0.1;
   textcoordx[5][7]=0.8;
   textcoordy[5][7]=-0.1;

   textcoordx[5][8]=0.8;
   textcoordy[5][8]=-0.1;
   textcoordx[5][9]=0.7;
   textcoordy[5][9]=-0.7;

   textcoordx[5][10]=0.7;
   textcoordy[5][10]=-0.7;
   textcoordx[5][11]=0.0;
   textcoordy[5][11]=-1.0;

   textcoordx[5][12]=0.0;
   textcoordy[5][12]=-1.0;
   textcoordx[5][13]=-0.7;
   textcoordy[5][13]=-0.7;

   textcoordx[5][14]=-0.7;
   textcoordy[5][14]=-0.7;
   textcoordx[5][15]=-0.8;
   textcoordy[5][15]=-0.35;

   /* 6 */
   textcoordx[6][0]=0.7;
   textcoordy[6][0]=1.0;
   textcoordx[6][1]=-0.8;
   textcoordy[6][1]=-0.2;

   textcoordx[6][2]=-0.8;
   textcoordy[6][2]=-0.2;
   textcoordx[6][3]=-0.8;
   textcoordy[6][3]=-0.6;

   textcoordx[6][4]=-0.8;
   textcoordy[6][4]=-0.6;
   textcoordx[6][5]=-0.4;
   textcoordy[6][5]=-1.0;

   textcoordx[6][6]=-0.4;
   textcoordy[6][6]=-1.0;
   textcoordx[6][7]=0.4;
   textcoordy[6][7]=-1.0;

   textcoordx[6][8]=0.4;
   textcoordy[6][8]=-1.0;
   textcoordx[6][9]=0.8;
   textcoordy[6][9]=-0.6;

   textcoordx[6][10]=0.8;
   textcoordy[6][10]=-0.6;
   textcoordx[6][11]=0.8;
   textcoordy[6][11]=-0.2;

   textcoordx[6][12]=0.8;
   textcoordy[6][12]=-0.2;
   textcoordx[6][13]=0.4;
   textcoordy[6][13]=0.2;

   textcoordx[6][14]=0.4;
   textcoordy[6][14]=0.2;
   textcoordx[6][15]=-0.4;
   textcoordy[6][15]=0.2;

   textcoordx[6][16]=-0.4;
   textcoordy[6][16]=0.2;
   textcoordx[6][17]=-0.8;
   textcoordy[6][17]=-0.2;

   /* 7 */
   textcoordx[7][0]=-0.8;
   textcoordy[7][0]=1.0;
   textcoordx[7][1]=0.8;
   textcoordy[7][1]=1.0;

   textcoordx[7][2]=0.8;
   textcoordy[7][2]=1.0;
   textcoordx[7][3]=-0.7;
   textcoordy[7][3]=-1.0;

   /* 8 */
   textcoordx[8][0]=-0.3;
   textcoordy[8][0]=1.0;
   textcoordx[8][1]=-0.8;
   textcoordy[8][1]=0.75;

   textcoordx[8][2]=-0.8;
   textcoordy[8][2]=0.75;
   textcoordx[8][3]=-0.8;
   textcoordy[8][3]=0.25;

   textcoordx[8][4]=-0.8;
   textcoordy[8][4]=0.25;
   textcoordx[8][5]=-0.3;
   textcoordy[8][5]=0.0;

   textcoordx[8][6]=-0.3;
   textcoordy[8][6]=0.0;
   textcoordx[8][7]=0.3;
   textcoordy[8][7]=0.0;

   textcoordx[8][8]=0.3;
   textcoordy[8][8]=0.0;
   textcoordx[8][9]=0.8;
   textcoordy[8][9]=0.25;

   textcoordx[8][10]=0.8;
   textcoordy[8][10]=0.25;
   textcoordx[8][11]=0.8;
   textcoordy[8][11]=0.75;

   textcoordx[8][12]=0.8;
   textcoordy[8][12]=0.75;
   textcoordx[8][13]=0.3;
   textcoordy[8][13]=1.0;

   textcoordx[8][14]=0.3;
   textcoordy[8][14]=1.0;
   textcoordx[8][15]=-0.3;
   textcoordy[8][15]=1.0;

   textcoordx[8][16]=-0.3;
   textcoordy[8][16]=0.0;
   textcoordx[8][17]=-0.8;
   textcoordy[8][17]=-0.25;

   textcoordx[8][18]=-0.8;
   textcoordy[8][18]=-0.25;
   textcoordx[8][19]=-0.8;
   textcoordy[8][19]=-0.75;

   textcoordx[8][20]=-0.8;
   textcoordy[8][20]=-0.75;
   textcoordx[8][21]=-0.3;
   textcoordy[8][21]=-1.0;

   textcoordx[8][22]=-0.3;
   textcoordy[8][22]=-1.0;
   textcoordx[8][23]=0.3;
   textcoordy[8][23]=-1.0;

   textcoordx[8][24]=0.3;
   textcoordy[8][24]=-1.0;
   textcoordx[8][25]=0.8;
   textcoordy[8][25]=-0.75;

   textcoordx[8][26]=0.8;
   textcoordy[8][26]=-0.75;
   textcoordx[8][27]=0.8;
   textcoordy[8][27]=-0.25;

   textcoordx[8][28]=0.8;
   textcoordy[8][28]=-0.25;
   textcoordx[8][29]=0.3;
   textcoordy[8][29]=0.0;

   /* 9 */
   textcoordx[9][0]=-0.8;
   textcoordy[9][0]=-1.0;
   textcoordx[9][1]=0.8;
   textcoordy[9][1]=0.25;

   textcoordx[9][2]=0.8;
   textcoordy[9][2]=0.25;
   textcoordx[9][3]=0.8;
   textcoordy[9][3]=0.75;

   textcoordx[9][4]=0.8;
   textcoordy[9][4]=0.75;
   textcoordx[9][5]=0.3;
   textcoordy[9][5]=1.0;

   textcoordx[9][6]=0.3;
   textcoordy[9][6]=1.0;
   textcoordx[9][7]=-0.3;
   textcoordy[9][7]=1.0;

   textcoordx[9][8]=-0.3;
   textcoordy[9][8]=1.0;
   textcoordx[9][9]=-0.8;
   textcoordy[9][9]=0.75;

   textcoordx[9][10]=-0.8;
   textcoordy[9][10]=0.75;
   textcoordx[9][11]=-0.8;
   textcoordy[9][11]=0.25;

   textcoordx[9][12]=-0.8;
   textcoordy[9][12]=0.25;
   textcoordx[9][13]=-0.3;
   textcoordy[9][13]=0.0;

   textcoordx[9][14]=-0.3;
   textcoordy[9][14]=0.0;
   textcoordx[9][15]=0.3;
   textcoordy[9][15]=0.0;

   textcoordx[9][16]=0.3;
   textcoordy[9][16]=0.0;
   textcoordx[9][17]=0.8;
   textcoordy[9][17]=0.25;

   /* . */
   textcoordx['.'][0]=-0.3;
   textcoordy['.'][0]=-0.7;
   textcoordx['.'][1]=0.3;
   textcoordy['.'][1]=-0.7;

   textcoordx['.'][2]=0.3;
   textcoordy['.'][2]=-0.7;
   textcoordx['.'][3]=0.3;
   textcoordy['.'][3]=-1.0;

   textcoordx['.'][4]=0.3;
   textcoordy['.'][4]=-1.0;
   textcoordx['.'][5]=-0.3;
   textcoordy['.'][5]=-1.0;

   textcoordx['.'][6]=-0.3;
   textcoordy['.'][6]=-1.0;
   textcoordx['.'][7]=-0.3;
   textcoordy['.'][7]=-0.7;
    
   textcoordx['.'][8]=-0.3;
   textcoordy['.'][8]=-0.7;
   textcoordx['.'][9]=0.3;
   textcoordy['.'][9]=-1.0;
       
   textcoordx['.'][10]=0.3;
   textcoordy['.'][10]=-0.7;
   textcoordx['.'][11]=-0.3;
   textcoordy['.'][11]=-1.0;
    
   /* - */
   textcoordx['-'][0]=-0.7;
   textcoordy['-'][0]=0.0;
   textcoordx['-'][1]=0.7;
   textcoordy['-'][1]=0.0;


   /* A */
   textcoordx['A'][0]=-0.8;
   textcoordy['A'][0]=-1.0;
   textcoordx['A'][1]=0.0;
   textcoordy['A'][1]=1.0;

   textcoordx['A'][2]=0.0;
   textcoordy['A'][2]=1.0;
   textcoordx['A'][3]=0.8;
   textcoordy['A'][3]=-1.0;

   textcoordx['A'][4]=-0.4;
   textcoordy['A'][4]=0.0;
   textcoordx['A'][5]=0.4;
   textcoordy['A'][5]=0.0;


   /* B */
   textcoordx['B'][0]=-0.8;
   textcoordy['B'][0]=1.0;
   textcoordx['B'][1]=-0.3;
   textcoordy['B'][1]=1.0;

   textcoordx['B'][2]=-0.3;
   textcoordy['B'][2]=1.0;
   textcoordx['B'][3]=0.8;
   textcoordy['B'][3]=0.8;

   textcoordx['B'][4]=0.8;
   textcoordy['B'][4]=0.8;
   textcoordx['B'][5]=0.8;
   textcoordy['B'][5]=0.2;

   textcoordx['B'][6]=0.8;
   textcoordy['B'][6]=0.2;
   textcoordx['B'][7]=-0.8;
   textcoordy['B'][7]=0.0;

   textcoordx['B'][8]=-0.8;
   textcoordy['B'][8]=0.0;
   textcoordx['B'][9]=0.8;
   textcoordy['B'][9]=-0.2;

   textcoordx['B'][10]=0.8;
   textcoordy['B'][10]=-0.2;
   textcoordx['B'][11]=0.8;
   textcoordy['B'][11]=-0.8;

   textcoordx['B'][12]=0.8;
   textcoordy['B'][12]=-0.8;
   textcoordx['B'][13]=-0.3;
   textcoordy['B'][13]=-1.0;

   textcoordx['B'][14]=-0.3;
   textcoordy['B'][14]=-1.0;
   textcoordx['B'][15]=-0.8;
   textcoordy['B'][15]=-1.0;

   textcoordx['B'][16]=-0.8;
   textcoordy['B'][16]=-1.0;
   textcoordx['B'][17]=-0.8;
   textcoordy['B'][17]=1.0;


   /* C */
   textcoordx['C'][0]=0.8;
   textcoordy['C'][0]=0.5;
   textcoordx['C'][1]=0.8;
   textcoordy['C'][1]=0.6;

   textcoordx['C'][2]=0.8;
   textcoordy['C'][2]=0.6;
   textcoordx['C'][3]=0.5;
   textcoordy['C'][3]=1.0;

   textcoordx['C'][4]=0.5;
   textcoordy['C'][4]=1.0;
   textcoordx['C'][5]=-0.5;
   textcoordy['C'][5]=1.0;

   textcoordx['C'][6]=-0.5;
   textcoordy['C'][6]=1.0;
   textcoordx['C'][7]=-0.8;
   textcoordy['C'][7]=0.6;

   textcoordx['C'][8]=-0.8;
   textcoordy['C'][8]=0.6;
   textcoordx['C'][9]=-0.8;
   textcoordy['C'][9]=-0.6;

   textcoordx['C'][10]=-0.8;
   textcoordy['C'][10]=-0.6;
   textcoordx['C'][11]=-0.5;
   textcoordy['C'][11]=-1.0;

   textcoordx['C'][12]=-0.5;
   textcoordy['C'][12]=-1.0;
   textcoordx['C'][13]=0.5;
   textcoordy['C'][13]=-1.0;

   textcoordx['C'][14]=0.5;
   textcoordy['C'][14]=-1.0;
   textcoordx['C'][15]=0.8;
   textcoordy['C'][15]=-0.6;

   textcoordx['C'][16]=0.8;
   textcoordy['C'][16]=-0.6;
   textcoordx['C'][17]=0.8;
   textcoordy['C'][17]=-0.6;

   /* D */
   textcoordx['D'][0]=-0.8;
   textcoordy['D'][0]=1.0;
   textcoordx['D'][1]=0.0;
   textcoordy['D'][1]=1.0;

   textcoordx['D'][2]=0.0;
   textcoordy['D'][2]=1.0;
   textcoordx['D'][3]=0.8;
   textcoordy['D'][3]=0.7;

   textcoordx['D'][4]=0.8;
   textcoordy['D'][4]=0.7;
   textcoordx['D'][5]=0.8;
   textcoordy['D'][5]=-0.7;

   textcoordx['D'][6]=0.8;
   textcoordy['D'][6]=-0.7;
   textcoordx['D'][7]=0.0;
   textcoordy['D'][7]=-1.0;

   textcoordx['D'][8]=0.0;
   textcoordy['D'][8]=-1.0;
   textcoordx['D'][9]=-0.8;
   textcoordy['D'][9]=-1.0;

   textcoordx['D'][10]=-0.8;
   textcoordy['D'][10]=-1.0;
   textcoordx['D'][11]=-0.8;
   textcoordy['D'][11]=1.0;


   /* E */
   textcoordx['E'][0]=-0.8;
   textcoordy['E'][0]=1.0;
   textcoordx['E'][1]=0.8;
   textcoordy['E'][1]=1.0;

   textcoordx['E'][2]=-0.8;
   textcoordy['E'][2]=0.0;
   textcoordx['E'][3]=0.5;
   textcoordy['E'][3]=0.0;

   textcoordx['E'][4]=-0.8;
   textcoordy['E'][4]=-1.0;
   textcoordx['E'][5]=0.8;
   textcoordy['E'][5]=-1.0;

   textcoordx['E'][6]=-0.8;
   textcoordy['E'][6]=1.0;
   textcoordx['E'][7]=-0.8;
   textcoordy['E'][7]=-1.0;


   /* F */
   textcoordx['F'][0]=-0.8;
   textcoordy['F'][0]=1.0;
   textcoordx['F'][1]=0.8;
   textcoordy['F'][1]=1.0;

   textcoordx['F'][2]=-0.8;
   textcoordy['F'][2]=0.1;
   textcoordx['F'][3]=0.6;
   textcoordy['F'][3]=0.1;

   textcoordx['F'][4]=-0.8;
   textcoordy['F'][4]=1.0;
   textcoordx['F'][5]=-0.8;
   textcoordy['F'][5]=-1.0;

   /* G */
   textcoordx['G'][0]=0.8;
   textcoordy['G'][0]=0.3;
   textcoordx['G'][1]=0.8;
   textcoordy['G'][1]=0.6;

   textcoordx['G'][2]=0.8;
   textcoordy['G'][2]=0.6;
   textcoordx['G'][3]=0.4;
   textcoordy['G'][3]=1.0;

   textcoordx['G'][4]=0.4;
   textcoordy['G'][4]=1.0;
   textcoordx['G'][5]=-0.4;
   textcoordy['G'][5]=1.0;

   textcoordx['G'][6]=-0.4;
   textcoordy['G'][6]=1.0;
   textcoordx['G'][7]=-0.8;
   textcoordy['G'][7]=0.6;

   textcoordx['G'][8]=-0.8;
   textcoordy['G'][8]=0.6;
   textcoordx['G'][9]=-0.8;
   textcoordy['G'][9]=-0.6;

   textcoordx['G'][10]=-0.8;
   textcoordy['G'][10]=-0.6;
   textcoordx['G'][11]=-0.4;
   textcoordy['G'][11]=-1.0;

   textcoordx['G'][12]=-0.4;
   textcoordy['G'][12]=-1.0;
   textcoordx['G'][13]=0.4;
   textcoordy['G'][13]=-1.0;

   textcoordx['G'][14]=0.4;
   textcoordy['G'][14]=-1.0;
   textcoordx['G'][15]=0.8;
   textcoordy['G'][15]=-0.3;

   textcoordx['G'][16]=0.8;
   textcoordy['G'][16]=-0.3;
   textcoordx['G'][17]=0.2;
   textcoordy['G'][17]=-0.3;


   /* H */
   textcoordx['H'][0]=-0.8;
   textcoordy['H'][0]=1.0;
   textcoordx['H'][1]=-0.8;
   textcoordy['H'][1]=-1.0;

   textcoordx['H'][2]=0.8;
   textcoordy['H'][2]=1.0;
   textcoordx['H'][3]=0.8;
   textcoordy['H'][3]=-1.0;

   textcoordx['H'][4]=-0.8;
   textcoordy['H'][4]=0.0;
   textcoordx['H'][5]=0.8;
   textcoordy['H'][5]=0.0;

   /* I */
   textcoordx['I'][0]=-0.4;
   textcoordy['I'][0]=1.0;
   textcoordx['I'][1]=0.4;
   textcoordy['I'][1]=1.0;

   textcoordx['I'][2]=-0.4;
   textcoordy['I'][2]=-1.0;
   textcoordx['I'][3]=0.4;
   textcoordy['I'][3]=-1.0;

   textcoordx['I'][4]=0.0;
   textcoordy['I'][4]=1.0;
   textcoordx['I'][5]=0.0;
   textcoordy['I'][5]=-1.0;

   /* J */
   textcoordx['J'][0]=-0.2;
   textcoordy['J'][0]=1.0;
   textcoordx['J'][1]=0.8;
   textcoordy['J'][1]=1.0;

   textcoordx['J'][2]=0.3;
   textcoordy['J'][2]=1.0;
   textcoordx['J'][3]=0.3;
   textcoordy['J'][3]=-0.6;

   textcoordx['J'][4]=0.3;
   textcoordy['J'][4]=-0.6;
   textcoordx['J'][5]=-0.1;
   textcoordy['J'][5]=-1.0;

   textcoordx['J'][6]=-0.1;
   textcoordy['J'][6]=-1.0;
   textcoordx['J'][7]=-0.4;
   textcoordy['J'][7]=-1.0;

   textcoordx['J'][8]=-0.4;
   textcoordy['J'][8]=-1.0;
   textcoordx['J'][9]=-0.8;
   textcoordy['J'][9]=-0.6;

   textcoordx['J'][10]=-0.8;
   textcoordy['J'][10]=-0.6;
   textcoordx['J'][11]=-0.8;
   textcoordy['J'][11]=-0.4;

   /* K */
   textcoordx['K'][0]=-0.8;
   textcoordy['K'][0]=1.0;
   textcoordx['K'][1]=-0.8;
   textcoordy['K'][1]=-1.0;

   textcoordx['K'][2]=-0.8;
   textcoordy['K'][2]=-0.2;
   textcoordx['K'][3]=0.8;
   textcoordy['K'][3]=1.0;

   textcoordx['K'][4]=-0.4;
   textcoordy['K'][4]=0.1;
   textcoordx['K'][5]=0.8;
   textcoordy['K'][5]=-1.0;


   /* L */
   textcoordx['L'][0]=-0.8;
   textcoordy['L'][0]=1.0;
   textcoordx['L'][1]=-0.8;
   textcoordy['L'][1]=-1.0;

   textcoordx['L'][2]=-0.8;
   textcoordy['L'][2]=-1.0;
   textcoordx['L'][3]=0.8;
   textcoordy['L'][3]=-1.0;


   /* M */
   textcoordx['M'][0]=-0.8;
   textcoordy['M'][0]=-1.0;
   textcoordx['M'][1]=-0.8;
   textcoordy['M'][1]=1.0;

   textcoordx['M'][2]=-0.8;
   textcoordy['M'][2]=1.0;
   textcoordx['M'][3]=0.0;
   textcoordy['M'][3]=0.0;

   textcoordx['M'][4]=0.0;
   textcoordy['M'][4]=0.0;
   textcoordx['M'][5]=0.8;
   textcoordy['M'][5]=1.0;

   textcoordx['M'][6]=0.8;
   textcoordy['M'][6]=1.0;
   textcoordx['M'][7]=0.8;
   textcoordy['M'][7]=-1.0;


   /* N */
   textcoordx['N'][0]=-0.8;
   textcoordy['N'][0]=-1.0;
   textcoordx['N'][1]=-0.8;
   textcoordy['N'][1]=1.0;

   textcoordx['N'][2]=-0.8;
   textcoordy['N'][2]=1.0;
   textcoordx['N'][3]=0.8;
   textcoordy['N'][3]=-1.0;

   textcoordx['N'][4]=0.8;
   textcoordy['N'][4]=1.0;
   textcoordx['N'][5]=0.8;
   textcoordy['N'][5]=-1.0;


   /* O */
   textcoordx['O'][0]=-0.4;
   textcoordy['O'][0]=1.0;
   textcoordx['O'][1]=-0.8;
   textcoordy['O'][1]=0.4;

   textcoordx['O'][2]=-0.8;
   textcoordy['O'][2]=0.4;
   textcoordx['O'][3]=-0.8;
   textcoordy['O'][3]=-0.4;

   textcoordx['O'][4]=-0.8;
   textcoordy['O'][4]=-0.4;
   textcoordx['O'][5]=-0.4;
   textcoordy['O'][5]=-1.0;

   textcoordx['O'][6]=-0.4;
   textcoordy['O'][6]=-1.0;
   textcoordx['O'][7]=0.4;
   textcoordy['O'][7]=-1.0;

   textcoordx['O'][8]=0.4;
   textcoordy['O'][8]=-1.0;
   textcoordx['O'][9]=0.8;
   textcoordy['O'][9]=-0.4;

   textcoordx['O'][10]=0.8;
   textcoordy['O'][10]=-0.4;
   textcoordx['O'][11]=0.8;
   textcoordy['O'][11]=0.4;

   textcoordx['O'][12]=0.8;
   textcoordy['O'][12]=0.4;
   textcoordx['O'][13]=0.4;
   textcoordy['O'][13]=1.0;

   textcoordx['O'][14]=0.4;
   textcoordy['O'][14]=1.0;
   textcoordx['O'][15]=-0.4;
   textcoordy['O'][15]=1.0;


   /* P */
   textcoordx['P'][0]=-0.8;
   textcoordy['P'][0]=1.0;
   textcoordx['P'][1]=-0.8;
   textcoordy['P'][1]=-1.0;

   textcoordx['P'][2]=-0.8;
   textcoordy['P'][2]=0.0;
   textcoordx['P'][3]=0.5;
   textcoordy['P'][3]=0.0;

   textcoordx['P'][4]=0.5;
   textcoordy['P'][4]=0.0;
   textcoordx['P'][5]=0.8;
   textcoordy['P'][5]=0.3;

   textcoordx['P'][6]=0.8;
   textcoordy['P'][6]=0.3;
   textcoordx['P'][7]=0.8;
   textcoordy['P'][7]=0.7;

   textcoordx['P'][8]=0.8;
   textcoordy['P'][8]=0.7;
   textcoordx['P'][9]=0.5;
   textcoordy['P'][9]=1.0;

   textcoordx['P'][10]=0.5;
   textcoordy['P'][10]=1.0;
   textcoordx['P'][11]=-0.8;
   textcoordy['P'][11]=1.0;


   /* Q */
   textcoordx['Q'][0]=-0.4;
   textcoordy['Q'][0]=1.0;
   textcoordx['Q'][1]=-0.8;
   textcoordy['Q'][1]=0.4;
    
   textcoordx['Q'][2]=-0.8;
   textcoordy['Q'][2]=0.4;
   textcoordx['Q'][3]=-0.8;
   textcoordy['Q'][3]=-0.4;
    
   textcoordx['Q'][4]=-0.8;
   textcoordy['Q'][4]=-0.4;
   textcoordx['Q'][5]=-0.4;
   textcoordy['Q'][5]=-1.0;
    
   textcoordx['Q'][6]=-0.4;
   textcoordy['Q'][6]=-1.0;
   textcoordx['Q'][7]=0.4;
   textcoordy['Q'][7]=-1.0;
    
   textcoordx['Q'][8]=0.4;
   textcoordy['Q'][8]=-1.0;
   textcoordx['Q'][9]=0.8;
   textcoordy['Q'][9]=-0.4;
    
   textcoordx['Q'][10]=0.8;
   textcoordy['Q'][10]=-0.4;
   textcoordx['Q'][11]=0.8;
   textcoordy['Q'][11]=0.4;
    
   textcoordx['Q'][12]=0.8;
   textcoordy['Q'][12]=0.4;
   textcoordx['Q'][13]=0.4;
   textcoordy['Q'][13]=1.0;
    
   textcoordx['Q'][14]=0.4;
   textcoordy['Q'][14]=1.0;
   textcoordx['Q'][15]=-0.4;
   textcoordy['Q'][15]=1.0;

   textcoordx['Q'][16]=0.2;   
   textcoordy['Q'][16]=-0.5;   
   textcoordx['Q'][17]=0.8;   
   textcoordy['Q'][17]=-0.9;   


   /* R */
   textcoordx['R'][0]=-0.8;
   textcoordy['R'][0]=1.0;
   textcoordx['R'][1]=-0.8;
   textcoordy['R'][1]=-1.0;
    
   textcoordx['R'][2]=-0.8;
   textcoordy['R'][2]=0.0;
   textcoordx['R'][3]=0.5;
   textcoordy['R'][3]=0.0;
    
   textcoordx['R'][4]=0.5;
   textcoordy['R'][4]=0.0;
   textcoordx['R'][5]=0.8;
   textcoordy['R'][5]=0.3;
    
   textcoordx['R'][6]=0.8;
   textcoordy['R'][6]=0.3;
   textcoordx['R'][7]=0.8;
   textcoordy['R'][7]=0.7;
    
   textcoordx['R'][8]=0.8;
   textcoordy['R'][8]=0.7;
   textcoordx['R'][9]=0.5;
   textcoordy['R'][9]=1.0;
    
   textcoordx['R'][10]=0.5;
   textcoordy['R'][10]=1.0;
   textcoordx['R'][11]=-0.8;
   textcoordy['R'][11]=1.0;
    
   textcoordx['R'][12]=0.5;
   textcoordy['R'][12]=0.0;
   textcoordx['R'][13]=0.8;
   textcoordy['R'][13]=-1.0;


   /* S */
   textcoordx['S'][0]=0.8;
   textcoordy['S'][0]=0.6;
   textcoordx['S'][1]=0.4;
   textcoordy['S'][1]=1.0;

   textcoordx['S'][2]=0.4;
   textcoordy['S'][2]=1.0;
   textcoordx['S'][3]=-0.4;
   textcoordy['S'][3]=1.0;

   textcoordx['S'][20]=-0.4;
   textcoordy['S'][20]=1.0;
   textcoordx['S'][21]=-0.8;
   textcoordy['S'][21]=0.6;

   textcoordx['S'][4]=-0.8;
   textcoordy['S'][4]=0.6;
   textcoordx['S'][5]=-0.8;
   textcoordy['S'][5]=0.4;

   textcoordx['S'][6]=-0.8;
   textcoordy['S'][6]=0.4;
   textcoordx['S'][7]=-0.4;
   textcoordy['S'][7]=0.0;

   textcoordx['S'][8]=-0.4;
   textcoordy['S'][8]=0.0;
   textcoordx['S'][9]=0.4;
   textcoordy['S'][9]=0.0;

   textcoordx['S'][10]=0.4;
   textcoordy['S'][10]=0.0;
   textcoordx['S'][11]=0.8;
   textcoordy['S'][11]=-0.4;

   textcoordx['S'][12]=0.8;
   textcoordy['S'][12]=-0.4;
   textcoordx['S'][13]=0.8;
   textcoordy['S'][13]=-0.6;

   textcoordx['S'][14]=0.8;
   textcoordy['S'][14]=-0.6;
   textcoordx['S'][15]=0.4;
   textcoordy['S'][15]=-1.0;

   textcoordx['S'][16]=0.4;
   textcoordy['S'][16]=-1.0;
   textcoordx['S'][17]=-0.4;
   textcoordy['S'][17]=-1.0;

   textcoordx['S'][18]=-0.4;
   textcoordy['S'][18]=-1.0;
   textcoordx['S'][19]=-0.8;
   textcoordy['S'][19]=-0.6;


   /* T */
   textcoordx['T'][0]=-0.8;
   textcoordy['T'][0]=1.0;
   textcoordx['T'][1]=0.8;
   textcoordy['T'][1]=1.0;

   textcoordx['T'][2]=0.0;
   textcoordy['T'][2]=1.0;
   textcoordx['T'][3]=0.0;
   textcoordy['T'][3]=-1.0;


   /* U */
   textcoordx['U'][0]=-0.8;
   textcoordy['U'][0]=1.0;
   textcoordx['U'][1]=-0.8;
   textcoordy['U'][1]=-0.7;

   textcoordx['U'][2]=-0.8;
   textcoordy['U'][2]=-0.7;
   textcoordx['U'][3]=-0.5;
   textcoordy['U'][3]=-1.0;

   textcoordx['U'][4]=-0.5;
   textcoordy['U'][4]=-1.0;
   textcoordx['U'][5]=0.5;
   textcoordy['U'][5]=-1.0;

   textcoordx['U'][6]=0.5;
   textcoordy['U'][6]=-1.0;
   textcoordx['U'][7]=0.8;
   textcoordy['U'][7]=-0.7;

   textcoordx['U'][8]=0.8;
   textcoordy['U'][8]=-0.7;
   textcoordx['U'][9]=0.8;
   textcoordy['U'][9]=1.0;


   /* V */
   textcoordx['V'][0]=-0.8;
   textcoordy['V'][0]=1.0;
   textcoordx['V'][1]=0.0;
   textcoordy['V'][1]=-1.0;

   textcoordx['V'][2]=0.0;
   textcoordy['V'][2]=-1.0;
   textcoordx['V'][3]=0.8;
   textcoordy['V'][3]=1.0;


   /* W */
   textcoordx['W'][0]=-0.8;
   textcoordy['W'][0]=1.0;
   textcoordx['W'][1]=-0.8;
   textcoordy['W'][1]=-1.0;

   textcoordx['W'][2]=-0.8;
   textcoordy['W'][2]=-1.0;
   textcoordx['W'][3]=0.0;
   textcoordy['W'][3]=0.0;

   textcoordx['W'][4]=0.0;
   textcoordy['W'][4]=0.0;
   textcoordx['W'][5]=0.8;
   textcoordy['W'][5]=-1.0;

   textcoordx['W'][6]=0.8;
   textcoordy['W'][6]=-1.0;
   textcoordx['W'][7]=0.8;
   textcoordy['W'][7]=1.0;

    
   /* X */
   textcoordx['X'][0]=-0.8;
   textcoordy['X'][0]=1.0;
   textcoordx['X'][1]=0.8;
   textcoordy['X'][1]=-1.0;

   textcoordx['X'][2]=0.8;
   textcoordy['X'][2]=1.0;
   textcoordx['X'][3]=-0.8;
   textcoordy['X'][3]=-1.0;


   /* Y */
   textcoordx['Y'][0]=-0.8;
   textcoordy['Y'][0]=1.0;
   textcoordx['Y'][1]=0.0;
   textcoordy['Y'][1]=0.1;

   textcoordx['Y'][2]=0.0;
   textcoordy['Y'][2]=0.1;
   textcoordx['Y'][3]=0.8;
   textcoordy['Y'][3]=1.0;

   textcoordx['Y'][4]=0.0;
   textcoordy['Y'][4]=0.1;
   textcoordx['Y'][5]=0.0;
   textcoordy['Y'][5]=-1.0;


   /* Z phew!!!.... */
   textcoordx['Z'][0]=-0.8;
   textcoordy['Z'][0]=1.0;
   textcoordx['Z'][1]=0.8;
   textcoordy['Z'][1]=1.0;

   textcoordx['Z'][2]=-0.8;
   textcoordy['Z'][2]=-1.0;
   textcoordx['Z'][3]=0.8;
   textcoordy['Z'][3]=1.0;

   textcoordx['Z'][4]=-0.8;
   textcoordy['Z'][4]=-1.0;
   textcoordx['Z'][5]=0.8;
   textcoordy['Z'][5]=-1.0;


}




static int make_digit( char digit, float x, float y, float dx, 
                       float dy, float vx[], float vy[])
{
   int num = 0;
   static do_once = 1;
   int n;

   if (do_once){
      init_text();
      do_once = 0;
   }
   if (digit >= '0'  &&  digit <= '9'){
      n = digit - '0';
   }
   else{
      n = (int)digit;
   }
   while(textcoordx[n][num] <= 1.0){
      vx[num] = x + (dx*textcoordx[n][num]);
      vy[num] = y + (dy*0.9*textcoordy[n][num]);
      num++;
      vx[num] = x + (dx*textcoordx[n][num]);
      vy[num] = y + (dy*0.9*textcoordy[n][num]);
      num++;
   }
   return num;
}

static void do_digits( double x, char str[],
                       int *decimal, int *sign)
{
   register int k, k1, k2, k3;
   int len;

   len = strlen(str);

   k = (int) x;
   if (x < 0){
      k = -k;
      x = -x;
      *sign = 1;
   }
   else{
      *sign = 0;
   }

   k1 = ( (int) (x * 10.0) ) % 10;
   k2 = ( (int) (x * 100.0) ) % 10;
   k3 = ( (int) (x * 1000.0) ) % 10;

   if (k>=100) {
      str[len-4] = 0;
      *decimal = 0;
   }
   else if (k>=10) {
      *decimal = 1;
      str[len-1] = 0;
      if (k2 == 0){
         str[len-2] = 0;
         if (k1 == 0){
            str[len-4] = 0;
            *decimal = 0;
         }
      }
   }
   else{
      *decimal = 1;
      if (k3==0){
         str[len-1] = 0;
         if (k2==0){
            str[len-2] = 0;
            if (k1==0){
               str[len-4] = 0;
               *decimal = 0;
            }
         }
      }
   }
}
      
 


int create_num_textplot( Irregular_Context itx, int time, float xs[],
                     float ys[], float zs[], double *numdata, int ploton[],
                     float vx[],
                     float vy[], float vz[], int *numv)
{
   int v, i, j, vcount;
   char label_str[40];
   int label_len;
   float x, y, z;
   int morev;
   float sizex, sizey, sizespace;
   int decimal, sign;
   int count = 0;

   sizex = 0.002 * itx->TextPlotFontX;
   sizey = 0.002 * 0.8 * itx->TextPlotFontY;
   sizespace = .0005 * itx->TextPlotFontSpace;
      
   vcount = 0;
   for (i = 0; i < itx->NumRecs[time]; i++){
      if (ploton[i]==1){
         if (!IS_MISSING(numdata[count])){
            sprintf( label_str, "%.3f", numdata[count]);
            do_digits( numdata[count], label_str, &decimal, &sign);
            label_len = strlen(label_str);

            x = xs[i] - ((sizex * label_len)+(sizespace * (label_len-1)))/2.0; 
            y = ys[i];


            if (sign){
               x += sizex/4.0; 
            }
            if (decimal){
               x += sizex/4.0;
            }

            for (j = 0; j < label_len; j++){
               if (label_str[j] == '.' ||
                   label_str[j] == '-'){
                  x -= sizex/4.0;
                  morev = make_digit( label_str[j], x, y,
                                   sizex/4.0, sizey,
                                   vx+vcount, vy+vcount);
                  x -= sizex/4.0;
               }
               else{
                  morev = make_digit( label_str[j], x, y,
                                   sizex/2.0, sizey,
                                   vx+vcount, vy+vcount);
               }
               x += sizex + sizespace;
               for (v = vcount; v < vcount+morev; v++){
                  vz[v] = zs[i];
               }
               vcount += morev;
               if (vcount >= MAX_TEXT_PLOT_VERTS){
                  printf("Error in create_num_textplot\n");
                  return -1;
               }
            }
         }
         count++;
      }
   }


   *numv = vcount;
   return 0;
}

int create_color_num_textplot( Irregular_Context itx, int time, float xs[],
                     float ys[], float zs[], double *numdata,
                     int ploton[], float vx[],
                     float vy[], float vz[], int *numv,
                     uint_1 *color_indexes)
{
   int v, i, j, vcount;
   char label_str[40];
   int label_len;
   float x, y, z;
   int morev;
   float sizex, sizey, sizespace;
   int decimal, sign;
   float valscale, min, max;
   int var, index;
   int pcount = 0; 

   sizex = 0.002 * itx->TextPlotFontX;
   sizey = 0.002 * 0.8 * itx->TextPlotFontY;
   sizespace = .0005 * itx->TextPlotFontSpace;
   var = itx->TextPlotVar;

   vcount = 0;
   min = itx->MinVal[var];
   max = itx->MaxVal[var];
   valscale = 254.0 / (max - min);

   for (i = 0; i < itx->NumRecs[time]; i++){
      if (ploton[i]){
         if (!IS_MISSING(numdata[pcount])){
            sprintf( label_str, "%.3f", numdata[pcount]);
            do_digits( numdata[pcount], label_str, &decimal, &sign);
            label_len = strlen(label_str);
    
            x = xs[i] - ((sizex * label_len)+(sizespace * (label_len-1)))/2.0;
            y = ys[i];
    
            if (sign){
               x += sizex/4.0;
            }
            if (decimal){
               x += sizex/4.0;
            }
    
            for (j = 0; j < label_len; j++){
               if (label_str[j] == '.' ||
                   label_str[j] == '-'){
                  x -= sizex/4.0;
                  morev = make_digit( label_str[j], x, y,
                                   sizex/4.0, sizey,
                                   vx+vcount, vy+vcount);
                  x -= sizex/4.0;
               }
               else{
                  morev = make_digit( label_str[j], x, y,
                                   sizex/2.0, sizey,
                                   vx+vcount, vy+vcount);
               }
               x += sizex + sizespace;
               index = (numdata[pcount] - min) * valscale;
               index = (index < 0) ? 0 : (index > 254) ? 254 : index;
               for (v = vcount; v < vcount+morev; v+=2){
                  vz[v] = zs[i];
                  vz[v+1] = zs[i];
                  color_indexes[v/2] = index;
               }
               vcount += morev;
               if (vcount >= MAX_TEXT_PLOT_VERTS){
                  printf("Error in create_num_textplot\n");
                  return -1;
               }
            }
         }
         pcount++;
      }
   }
   *numv = vcount;
   return 0;
}

int create_letter_textplot( Irregular_Context itx, int time, float xs[],
                     float ys[], float zs[], char *chardata, int ploton[], int var,
                     float vx[],
                     float vy[], float vz[], int *numv) 
{                    
   int v, i, j, vcount;
   char label_str[40];
   int label_len;
   float x, y, z;
   int morev;
   float sizex, sizey, sizespace;
   int decimal, sign;
   char tempword[1000];
   int l, ccount;

   sizex = 0.002 * itx->TextPlotFontX;
   sizey = 0.002 * 0.8 * itx->TextPlotFontY;
   sizespace = 0.0005 * itx->TextPlotFontSpace;
 
   vcount = 0;
   ccount = 0;
   if (chardata[0] == 0){
      *numv = 0;
       return 0;
   }
   for (i = 0; i < itx->NumRecs[time]; i++){
      if (ploton[i]){
         for (l = 0; l < itx->CharVarLength[var]; l++){
            tempword[l] = chardata[ccount+l];
         }  
         tempword[l] = 0;
         label_len = strlen(tempword);
         x = xs[i] - ((sizex * label_len)+(sizespace * (label_len-1)))/2.0;
         y = ys[i];
 
         for (j = 0; j < label_len; j++){
            morev = make_digit( tempword[j], x, y,
                             sizex/2.0, sizey,
                             vx+vcount, vy+vcount);
            x += sizex+ sizespace;
            for (v = vcount; v < vcount+morev; v++){
               vz[v] = zs[i];
            }  
            vcount += morev;
            if (vcount >= MAX_TEXT_PLOT_VERTS){
               printf("Error in create_letter_textplot\n");
               return -1;
            }  
         }
         ccount += itx->CharVarLength[var];
      }
   }  
 
   *numv = vcount;
   return 0;
}  

     
void space_plots( Irregular_Context itx, int time, 
                     int ploton[], float xs[],
                     float ys[], float zs[], int *numtouse)
{
   float xlist[MAXRECS];
   int xrecord[MAXRECS];
   int numx;
   int temp[MAXRECS];
   int numtemps;
   float thres;
   int i, j, k, ntu;
   float dist;

   for (i = 0; i < itx->NumRecs[time]; i++){
      ploton[i] = 1;
   }
   ntu = itx->NumRecs[time];

   if (itx->TextPlotSpacing == 0.0){
      *numtouse = 0;
      return;
   }
   /***************************/   
   /* set first record to one */
   /***************************/
   xlist[0] = xs[0];
   xrecord[0] = 0;
   numx = 1;
   

   thres = 0.1 * itx->TextPlotSpacing;
   for (i = 1; i < itx->NumRecs[time]; i++){
      numtemps = 0;
      /******************************************/
      /* go through records that are ok to plot */
      /******************************************/
      for ( j = 0; j < numx; j++){
         /* check if within threshold */
         if (xs[i]-xlist[j] < thres){
            temp[numtemps] = xrecord[j];
            numtemps++;
         }
      }
      for (j = 0; j < numtemps && ploton[i]; j++){
         dist = DISTANCE(xs[i], ys[i], xs[temp[j]], ys[temp[j]]);
         if (dist < thres){
            ploton[i] = 0;
            ntu--;

         }
      }
      if (ploton[i]){
         for (j = 0; j < numx; j++){
            if (xs[i] < xlist[j]){
               for (k = numx; k > j; k--){
                  xlist[k] = xlist[k-1];
                  xrecord[k] = xrecord[k-1];
               }
               break;
            }
         }
         xlist[j] = xs[i];
         xrecord[j] = i;
         numx++;
      }
   }
   *numtouse = ntu;
}
 






          





























   

