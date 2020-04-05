/* contour.c */
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

/* 2-D contouring function */


#define XY


#include <stdio.h>
#include <string.h>
#include "memory.h"
#include "globals.h"



#define ABS( X )            ( (X) < 0 ? -(X) : (X) )
#define MIN2( X, Y )        ( (X) < (Y) ? (X) : (Y) )
#define MAX2( X, Y )        ( (X) > (Y) ? (X) : (Y) )
#define MIN4( W, X, Y, Z )  ( MIN2( MIN2(W,X), MIN2(Y,Z) ) )
#define MAX4( W, X, Y, Z )  ( MAX2( MAX2(W,X), MAX2(Y,Z) ) )

/* NINT(x) = if x > 0 then INT(x+0.5) else INT(x-0.5) endif */
#define NINT( X )           ( (X) >= 0 ? (int) ((X) + 0.5) : (int) ((X)-0.5) )



#define G( R, C )           ( g[ (C) * nr + (R) ] )
#define MARK( R, C )        ( mark[ (C) * nr + (R) ] )



/*
 * Size of temporary vertex arrays:  It would be more efficient memory-
 * wise to dynamically allocate the vx,vy,ipnt arrays to size maxv1+maxv2
 * but it would also be slower.
 */
#define MAXTEMP (MAXROWS*MAXCOLUMNS)


/* all returned vertices will be scaled to this region: */
#ifdef XY
#define XMIN 0.0
#define XMAX (nr-1)
#define YMIN 0.0
#define YMAX (nc-1)
#else
#define XMIN (-1.0)
#define XMAX  (1.0)
#define YMIN (-1.0)
#define YMAX  (1.0)
#endif




/* MJK 12.10.98 begin */
/*
 * Plot a digit for a contour label in a vector font format.
 * Input:  digit - digit character
 *         row, col, hgt - position and size of digit.
 *         vx, vy - arrays to put vertices into.
 * Output:  vx, vy - the vertices of the label's digits.
 * Return:  number of vertices put into vx,vy.
 */
static int plot_digit (char digit, float row, float col, float hgt,
                       float vx[], float vy[] )
{
   static int lb[91] = { 0,
      105,102,80,20,02,05,27,87,105,85,103,3,1,5,87,105,102,80,
      60,7,0,87,105,102,80,70,52,54,52,30,20,2,5,27,104,57,50,100,0,
      100,107,67,62,40,20,2,5,27,80,102,105,87,27,5,2,20,30,52,57,
      107,100,4,105,102,80,70,52,55,37,27,5,2,20,30,52,55,77,87,105,
      27, 5,2,20,80,102,105,87,77,55,50 };
   static int lt[12] = { 0,
      1,10,15,22,35,40,49,60,63,80,91 };

   float        rs, cs, hl, he;
   int          i, ib, ie, llin, llel;
   int          num = 0;

   if ((digit < '0') || (digit > '9')) return 0;

   i  = digit - '0';
   ib = lt[i+1];
   ie = lt[i+2]-1;
   for (i=ib;i<=ie;i++) {
      llin = lb[i]/10;
      llel = lb[i]-llin*10;
      hl = hgt*llin;
      he = hgt*llel;
      if (i != ib) {
         vx[num] = rs;
         vy[num] = cs;
         num++;
         vx[num] = row-hl;
         vy[num] = col-he;
         num++;
      }
      rs = row-hl;
      cs = col-he;
   }


   return num;
}


static int plot_digit_wierd (char digit, float row, float col, float hgt, float h, 
                       float vx[], float vy[] )
{
   static int lb[91] = { 0,
      105,102,80,20,02,05,27,87,105,85,103,3,1,5,87,105,102,80,
      60,7,0,87,105,102,80,70,52,54,52,30,20,2,5,27,104,57,50,100,0,
      100,107,67,62,40,20,2,5,27,80,102,105,87,27,5,2,20,30,52,57,
      107,100,4,105,102,80,70,52,55,37,27,5,2,20,30,52,55,77,87,105,
      27, 5,2,20,80,102,105,87,77,55,50 };
   static int lt[12] = { 0,
      1,10,15,22,35,40,49,60,63,80,91 };

   float        rs, cs, hl, he;
   int          i, ib, ie, llin, llel;
   int          num = 0;

   if ((digit < '0') || (digit > '9')) return 0;

   i  = digit - '0';
   ib = lt[i+1];
   ie = lt[i+2]-1;
   for (i=ib;i<=ie;i++) {
      llin = lb[i]/10;
      llel = lb[i]-llin*10;
      hl = hgt*llin;
      he = h*llel;
      if (i != ib) {
         vx[num] = rs;
         vy[num] = cs;
         num++;
         vx[num] = row-hl;
         vy[num] = col-he;
         num++;
      }
      rs = row-hl;
      cs = col-he;
   }


   return num;
}

/*
 * Plot a contour label in a vector font format.
 *     Note: ROWS = XK TO XM, COLS = YK TO YM
 * Input:  lbl_str - string containing the contour label
 *         xk, yk, xm, ym - bounds for the label.
 *         vx, vy - arrays to put vertices into.
 * Output:  vx, vy - the vertices of the label's digits.
 * Return:  number of vertices put into vx,vy.
 */
static int plot_label (char *lbl_str, float xk, float yk, float xm, float ym,
                       float vx[], float vy[] )
{
   float        xmk, ymk, hgt, h, width, row, col, gap;
   int          i, len, idot;
   int          num = 0;

   if (lbl_str == NULL) return 0;
   if ((len = strlen (lbl_str)) == 0) return 0;

   width = len;
   idot  = -1;
   for (i = 0; i < len; i++)
   {
      if (lbl_str[i] == '.')
      {
         idot = i;
         width -= 0.5;
         break;
      }
   }
   if (lbl_str[0] == '-') width -= 0.5;


   if (xm < xk) col = xm, xm = xk, xk = col;
   if (ym < yk) row = ym, ym = yk, yk = row;
   xmk = xm - xk;
   ymk = ym - yk;

   hgt  = xmk/1.2;
   h    = ymk/(width+0.2);
   if (h < hgt) hgt=h;
   hgt *= 0.8; /* make the label slightly smaller than its box */
   h    = hgt/10.0;
   gap  = hgt * 0.2; /* gap between characters */

   row = xm - (0.5 * (xmk - hgt));
   col = ym - (0.5 * (ymk - (width * hgt))) - gap;

   i = len;
   if (idot >= 0)
   {
      while (--i > idot)
      {
         num += plot_digit (lbl_str[i], row, col, h, (vx+num), (vy+num));
         col -= hgt;
      }

      /* plot decimal cross */
      col += gap;
      vx[num] = row-0.1*hgt;
      vy[num] = col-0.2*hgt;
      num++;
      vx[num] = row-0.2*hgt;
      vy[num] = col-0.3*hgt;
      num++;
      vx[num] = row-0.2*hgt;
      vy[num] = col-0.2*hgt;
      num++;
      vx[num] = row-0.1*hgt;
      vy[num] = col-0.3*hgt;
      num++;
      col -= gap;
      /* half space for column of decimal cross */
      col -= 0.5*hgt;
   }

   while (--i >= 0)
   {
      if (lbl_str[i] != '-')
      {
         num += plot_digit (lbl_str[i], row, col, h, (vx+num), (vy+num));
         col -= hgt;
      }
      else
      {
         /* PLOT MINUS SIGN */
         vx[num] = row-0.5*hgt;
         vy[num] = col-0.4*hgt;
         num++;
         vx[num] = row-0.5*hgt;
         vy[num] = col;
         num++;
      }
   }

   return num;
}

static int plot_label_wierd (char *lbl_str, float xk, float yk, float xm, float ym,
                       float vx[], float vy[] )
{
   float        hr, hc, xmk, ymk, hgt, h, width, row, col, gap;
   int          i, len, idot;
   int          num = 0;

   if (lbl_str == NULL) return 0;
   if ((len = strlen (lbl_str)) == 0) return 0;

   width = len;
   idot  = -1;
   for (i = 0; i < len; i++)
   {
      if (lbl_str[i] == '.')
      {
         idot = i;
         width -= 0.5;
         break;
      }
   }
   if (lbl_str[0] == '-') width -= 0.5;


   if (xm < xk) col = xm, xm = xk, xk = col;
   if (ym < yk) row = ym, ym = yk, yk = row;
   xmk = xm - xk;
   ymk = ym - yk;

   hgt  = xmk/1.2;
   h    = ymk/(width+0.2);
   hgt *= 0.8; /* make the label slightly smaller than its box */
   hc    = h/10.0;
   hr    = hgt/10.0;
   gap  = h * 0.2; /* gap between characters */

   row = xm - (0.5 * (xmk - hgt));
   col = ym - (0.5 * (ymk - (width * h))) - gap;
  

   i = len;
   if (idot >= 0)
   {
      while (--i > idot)
      {
         num += plot_digit_wierd (lbl_str[i], row, col, hr, hc, (vx+num), (vy+num));
         col -= hgt;
      }

      /* plot decimal cross */
      col += gap;
      vx[num] = row-0.1*hgt;
      vy[num] = col-0.2*h;
      num++;
      vx[num] = row-0.2*hgt;
      vy[num] = col-0.3*h;
      num++;
      vx[num] = row-0.2*hgt;
      vy[num] = col-0.2*h;
      num++;
      vx[num] = row-0.1*hgt;
      vy[num] = col-0.3*h;
      num++;
      col -= gap;
      /* half space for column of decimal cross */
      col -= 0.5*h;
   }

   while (--i >= 0)
   {
      if (lbl_str[i] != '-')
      {
         num += plot_digit_wierd (lbl_str[i], row, col, hr, hc, (vx+num), (vy+num));
         col -= h;
      }
      else
      {
         /* PLOT MINUS SIGN */
         vx[num] = row-0.5*hgt;
         vy[num] = col-0.4*h;
         num++;
         vx[num] = row-0.5*hgt;
         vy[num] = col;
         num++;
      }
   }

   return num;
}

/* MJK 12.10.98 end */
/*
 * Extract digits from a float to be used by the PLTDG subroutine
 * in zcont.f
 * Input:  x - the floating point number
 * Output: dig - number of digits to plot
 *         j   - integer to left of decimal point
 *         j1, j2, j3 - integer digits to right of decimal point
 *         sign - the sign of the number: 1 or -1
 */
static int extract_digits( float x, float *dig,
                           int *j, int *j1, int *j2, int *j3 )
{
   register int sign, k, k1, k2, k3;
   float digits;

   k = (int) x;
/* This line changed due to email from Morwenna Griffiths of Australia */
/*   if (k<0) { */
   if (x<0) {
      k = -k;
      x = -x;
      sign = -1;
      digits = 0.5;
   }
   else {
      sign = 1;
      digits = 0.0;
   }

   k1 = ( (int) (x * 10.0) ) % 10;
   k2 = ( (int) (x * 100.0) ) % 10;
   k3 = ( (int) (x * 1000.0) ) % 10;

   /* examine digits to left of decimal point */
   if (k>=100) {
      k1 = k2 = k3 = 0;
      digits += 3.0;
   }
   else if (k>=10) {
      k3 = 0;
      digits += 4.5;
      if (k2==0) {
         digits -= 1.0;
         if (k1==0)  digits -= 1.0;
      }
   }
   else {
      digits += 4.5;
      if (k3==0) {
         digits -= 1.0;
         if (k2==0) {
            digits -= 1.0;
            if (k1==0)  digits -= 1.0;
         }
      }
   }

   *j = k;
   *j1 = k1;
   *j2 = k2;
   *j3 = k3;
   if (digits<2.0)
     *dig = 2.0;
   else
     *dig = digits;

   return sign;
}



/*
 * Plot the digits for a contour label in a vector font format.
 *     Note: ROWS = XK TO XM, COLS = YK TO YM
 * Input:  gg - label value
 *         xk, yk, xm, ym - bounds for the label.
 *         vx, vy - arrays to put vertices into.
 * Output:  vx, vy - the vertices of the label's digits.
 * Return:  number of vertices put into vx,vy.
 */
static int plotdigits( float gg, float xk, float yk, float xm, float ym,
                       float vx[], float vy[] )
{
   static int lb[91] = { 0,
      105,102,80,20,02,05,27,87,105,85,103,3,1,5,87,105,102,80,
      60,7,0,87,105,102,80,70,52,54,52,30,20,2,5,27,104,57,50,100,0,
      100,107,67,62,40,20,2,5,27,80,102,105,87,27,5,2,20,30,52,57,
      107,100,4,105,102,80,70,52,55,37,27,5,2,20,30,52,55,77,87,105,
      27, 5,2,20,80,102,105,87,77,55,50 };
   static int lt[12] = { 0,
      1,10,15,22,35,40,49,60,63,80,91 };
   float xmk, ymk, hgt, h, dig;
   float row, col, hl, he;
   float rs, cs;
   int jg, j1, j2, j3, isign;
   int ib, ie, llin, llel, m;
   int i;
   int num = 0;

   /* extract digits from GG: */
   isign = extract_digits( gg, &dig, &jg, &j1, &j2, &j3 );

   xmk = ABS(xm-xk);
   ymk = ABS(ym-yk);

   hgt = xmk/1.2;
   h = ymk/(dig+0.2);
   if (h < hgt) hgt=h;
/*
   row = MIN2(xm,xk)+0.5*(xmk-hgt);
*/
   row = MAX2(xm,xk)-0.5*(xmk-hgt);
   col = MAX2(ym,yk)-0.5*(ymk-dig*hgt);
   h = hgt/10.0;

   rs = cs = 0.0;

   /* PLOT 1000THS */
   if (j3) {
      ib = lt[j3+1];
      ie = lt[j3+2]-1;
      for (i=ib;i<=ie;i++) {
         llin = lb[i]/10;
         llel = lb[i]-llin*10;
         hl = h*llin;
         he = h*llel;
         if (i != ib) {
            vx[num] = rs;
            vy[num] = cs;
            num++;
/*            vx[num] = row+hl;*/
            vx[num] = row-hl;
            vy[num] = col-he;
            num++;
         }
/*         rs = row+hl;*/
         rs = row-hl;
         cs = col-he;
      }
      /* SPACE FOR COLUMN OF DIGIT */
      col = col-hgt;
   }

   /* PLOT 100THS */
   if (j2 || j3) {
      ib = lt[j2+1];
      ie = lt[j2+2]-1;
      for (i=ib;i<=ie;i++) {
         llin = lb[i]/10;
         llel = lb[i]-llin*10;
         hl = h*llin;
         he = h*llel;
         if (i != ib) {
            vx[num] = rs;
            vy[num] = cs;
            num++;
/*            vx[num] = row+hl;*/
            vx[num] = row-hl;
            vy[num] = col-he;
            num++;
         }
/*         rs = row+hl;*/
         rs = row-hl;
         cs = col-he;
      }
      /* space for column of digit */
      col = col-hgt;
   }

   /* PLOT 10THS */
   if (j1 || j2 || j3) {
      /* PLOT DIGIT RIGHT OF DECIMAL */
      ib = lt[j1+1];
      ie = lt[j1+2]-1;
      for (i=ib;i<=ie;i++) {
         llin = lb[i]/10;
         llel = lb[i]-llin*10;
         hl = h*llin;
         he = h*llel;
         if (i != ib) {
            vx[num] = rs;
            vy[num] = cs;
            num++;
/*            vx[num] = row+hl;*/
            vx[num] = row-hl;
            vy[num] = col-he;
            num++;
         }
/*         rs = row+hl;*/
         rs = row-hl;
         cs = col-he;
      }
      /* space for column of digit */
      col = col-hgt;
      /* plot decimal cross */
/*      vx[num] = row+0.1*hgt;*/
      vx[num] = row-0.1*hgt;
      vy[num] = col-0.2*hgt;
      num++;
/*      vx[num] = row+0.2*hgt;*/
      vx[num] = row-0.2*hgt;
      vy[num] = col-0.3*hgt;
      num++;
/*      vx[num] = row+0.2*hgt;*/
      vx[num] = row-0.2*hgt;
      vy[num] = col-0.2*hgt;
      num++;
/*      vx[num] = row+0.1*hgt;*/
      vx[num] = row-0.1*hgt;
      vy[num] = col-0.3*hgt;
      num++;
      /* half space for column of decimal cross */
      col = col-0.5*hgt;
   }

   /* PLOT DIGITS LEFT OF DECIMAL */
   /* 100: */
   do {
      m = jg-(jg/10)*10;
      ib = lt[m+1];
      ie = lt[m+2]-1;
      for (i=ib;i<=ie;i++) {
         llin = lb[i]/10;
         llel = lb[i]-llin*10;
         hl = h*llin;
         he = h*llel;
         if (i != ib) {
            vx[num] = rs;
            vy[num] = cs;
            num++;
/*            vx[num] = row+hl;*/
            vx[num] = row-hl;
            vy[num] = col-he;
            num++;
         }
/*         rs = row+hl;*/
         rs = row-hl;
         cs = col-he;
      }
      jg = jg/10;
      /* SPACE FOR COLUMN OF DIGIT */
      col = col-hgt;
   } while (jg != 0);


   if (isign < 0) {
      /* PLOT MINUS SIGN */
/*      vx[num] = row+0.5*hgt;*/
      vx[num] = row-0.5*hgt;
      vy[num] = col-0.4*hgt;
      num++;
/*      vx[num] = row+0.5*hgt;*/
      vx[num] = row-0.5*hgt;
      vy[num] = col;
      num++;
   }

   return num;
}



/*
 * Compute contour lines for a 2-D array.  If the interval is negative,
 * then negative contour lines will be drawn as dashed lines.
 * The contour lines will be computed for all V such that:
 *           lowlimit <= V <= highlimit
 *     and   V = base + n*interval  for some integer n
 * Note that the input array, g, should be in column-major (FORTRAN) order.
 *
 * Input:  g - the 2-D array to contour.
 *         nr, nc - size of 2-D array in rows and columns.
 *         interval - the interval between contour lines.
 *         lowlimit - the lower limit on values to contour.
 *         highlimit - the upper limit on values to contour.
 *         base - base value to start contouring at.
 *         vx1, vy1 - arrays to put contour line vertices
 *         maxv1 - size of vx1, vy1 arrays
 *         numv1 - pointer to int to return number of vertices in vx1,vy1
 *         vx2, vy2 - arrays to put 'hidden' contour line vertices
 *         maxv2 - size of vx2, vy2 arrays
 *         numv2 - pointer to int to return number of vertices in vx2,vy2
 *         vx3, vy3 - arrays to put contour label vertices
 *         maxv3 - size of vx3, vy3 arrays
 *         numv3 - pointer to int to return number of vertices in vx3,vy3
 * Return:  1 = ok
 *          0 = error  (interval==0.0 or out of memory)
 */
int contour( Context ctx, float g[], int nr, int nc,
             float interval, float lowlimit, float highlimit,
             float base,
             float vx1[], float vy1[],  int maxv1, int *numv1,
             float vx2[], float vy2[],  int maxv2, int *numv2,
             float vx3[], float vy3[],  int maxv3, int *numv3 )
{
   register int ir, ic;
   int nrm, ncm, idash;
   int numc, il;
   int ffex, ffey, lr, lc, lc2, lrr, lr2, lcc;
   float xd, yd ,xx, yy;
   float clow, chi;
   float gg;
   float *vx, *vy;
   int *ipnt;
   int nump, ip;
   register int numv;
   char *mark;

   /* MJK 12.10.98 */
   char         lbl_str[10], lbl_fmt[40];
   int          lbl_len, lbl_dot;
   int use_resize;

   /* Dynamically allocate temp. arrays, to avoid using excessive
      stack space and crashing on some systems.  It's not going to be
      appreciably slower, since the caller (calc_hslice or
      calc_vslice) calls malloc at least 6 times anyway. calc_hslice
      and calc_vslice determine maxv1 & maxv2 based on an upper bound
      for the number of vertices derived from the code below. */
   const int maxtemp = maxv1 > maxv2 ? maxv1 : maxv2;
   vx = (float*) malloc(sizeof(float)*maxtemp);
   vy = (float*) malloc(sizeof(float)*maxtemp);
   ipnt = (int*) malloc(sizeof(int)*((nr-1)*(nc-1) + 1)); /* see below loop */
   if (!vx || !vy || !ipnt) {
      fprintf(stderr, "You do not have enough memory to create contours.\n");
      free(vx); free(vy); free(ipnt);
      return 0;
   }

   use_resize = 0;
   ffex = ffey = 0;

   if (ctx->dpy_ctx->ContFontFactorX != 0 ||
       ctx->dpy_ctx->ContFontFactorY != 0){
      use_resize = 1;
   }


   /* initialize vertex counts */
   *numv1 = *numv2 = *numv3 = 0;

   /* deduct 100 vertices from maxv3 now to save a later computation */
   maxv3 -= 100;

   if (interval==0.0) {
      /* bad contour interval */
      free(vx); free(vy); free(ipnt);
      return 0;
   }
   if (interval<0.0) {
      /* draw negative contour lines as dashed lines */
      interval = -interval;
      idash = 1;
   }
   else {
      idash = 0;
   }

   nrm = nr-1;
   ncm = nc-1;

   xd = (XMAX-XMIN)/(nr-1);
   yd = (YMAX-YMIN)/(nc-1);

   /*
    * set up mark array
    * mark= 0 if avail for label center,
    *       2 if in label, and
    *       1 if not available and not in label
    *
    * lr and lc give label size in grid boxes
    * lrr and lcc give unavailable radius
    */
   lr = 1+(nr-2)/50;
   lc = 1+(nc-2)/10;

   if (use_resize){
      ffex = ctx->dpy_ctx->ContFontFactorX;
      ffey = ctx->dpy_ctx->ContFontFactorY;

      if (ffey+lr > 0 && ffey+lr < nr-3 &&
          ffex+lc > 0 && ffex+lc < nc-3){
         lr += ffey;
         lc += ffex;
      }
   }

   lc2 = lc/2;
   lr2 = lr/2;
   lrr = 1+(nr-2)/8;
   lcc = 1+(nc-2)/8;

   /* MJK 12.10.98 */
   sprintf (lbl_str, "%f", interval);
   lbl_len = strlen (lbl_str);
   while (lbl_str[--lbl_len] == '0') if (lbl_len == 0) break;
   lbl_dot = 0;
   if (lbl_len > 0)
   {
      while (lbl_str[lbl_len--] != '.')
      {
         if (lbl_len < 0) break;
         lbl_dot++;
      }
   }
   sprintf (lbl_fmt, "%%.%df", lbl_dot);






   /* allocate mark array */
   mark = (char *) allocate( ctx, nr * nc * sizeof(char) );
   if (!mark) {
      free(vx); free(vy); free(ipnt);
      return 0;
   }

   /* initialize mark array to zeros */
   memset( mark, 0, nr*nc*sizeof(char) );

   /* set top and bottom rows to 1 */
   for (ic=0;ic<nc;ic++) {
      for (ir=0;ir<lr;ir++) {
         MARK(ir,ic) = 1;
/* WLH 12 Nov 98
         MARK(nr-ir-2,ic) = 1;
*/
         /* WLH 12 Nov 98 */
         MARK(nr-1-ir,ic) = 1;

      }
   }

   /* set left and right columns to 1 */
   for (ir=0;ir<nr;ir++) {
      for (ic=0;ic<lc;ic++) {
         MARK(ir,ic) = 1;
/* WLH 12 Nov 98
         MARK(ir,nc-ic-2) = 1;
*/
         /* WLH 12 Nov 98 */
         MARK(ir,nc-1-ic) = 1;

      }
   }
/*
   for (ir=0;ir<nr;ir++) {
      for (ic=0;ic<nc;ic++) {
         printf("%d ", (int) MARK(ir,ic) );
      }
      printf("\n");
   }
*/

   numv = nump = 0;
   /* compute contours */
   for (ir=0; ir<nrm && numv<maxtemp-8 && nump<2*maxtemp; ir++) {
      xx = xd*ir+XMIN;
      for (ic=0; ic<ncm && numv<maxtemp-8 && nump<2*maxtemp; ic++) {
         float ga, gb, gc, gd;
         float gv, gn, gx;
         register float tmp1, tmp2;

         /* save index of first vertex in this grid box */
         ipnt[nump++] = numv;

         yy = yd*ic+YMIN;

         /* get 4 corner values, skip box if any are missing */
         ga = G(ir,ic);
         if (ga > 1.e30 ) continue;
         gb = G(ir+1,ic);
         if (gb > 1.e30 ) continue;
         gc = G(ir,ic+1);
         if (gc > 1.e30 ) continue;
         gd = G(ir+1,ic+1);
         if (gd > 1.e30 ) continue;

         /* find average, min, and max of 4 corner values */
         gv = (ga+gb+gc+gd)/4.0;

         /*gn = MIN4(ga,gb,gc,gd);*/
         tmp1 = MIN2( ga, gb );
         tmp2 = MIN2( gc, gd );
         gn = MIN2( tmp1, tmp2 );

         /*gx = MAX4(ga,gb,gc,gd);*/
         tmp1 = MAX2( ga, gb );
         tmp2 = MAX2( gc, gd );
         gx = MAX2( tmp1, tmp2 );


         /* compute clow and chi, low and high contour values in the box */

         /* old method: */
         /* clow = interval*(NINT(gn/interval)-1);*/
/*         clow = base + interval*(NINT((gn-base)/interval)-1);*/
         tmp1 = (gn-base) / interval;
         clow = base + interval * (NINT(tmp1)-1);
         while (clow<gn) {
            clow += interval;
         }

         /* old method: */
         /*chi = interval*(NINT(gx/interval)+1);*/
/*         chi = base + interval*(NINT((gx-base)/interval)+1);*/
         tmp1 = (gx-base) / interval;
         chi = base + interval * (NINT(tmp1)+1);
         while (chi>gx) {
            chi -= interval;
         }

         /* how many contour lines in the box: */
/*         numc = 1+NINT((chi-clow)/interval);*/
         tmp1 = (chi-clow) / interval;
         numc = 1+NINT(tmp1);

         /* gg is current contour line value */
         gg = clow;

         for (il=0; il<numc && numv+8<maxtemp; il++, gg += interval) {
            float gba, gca, gdb, gdc;
            int ii;

            /* make sure gg is within contouring limits */
            if (gg < gn) continue;
            if (gg > gx) break;
            if (gg < lowlimit) continue;
            if (gg > highlimit) break;

            /* compute orientation of lines inside box */
            ii = 0;
            if (gg > ga) ii = 1;
            if (gg > gb) ii += 2;
            if (gg > gc) ii += 4;
            if (gg > gd) ii += 8;
            if (ii > 7) ii = 15 - ii;
            if (ii <= 0) continue;

            /* DO LABEL HERE*/
            if (MARK(ir,ic)==0) {
               int kc, kr, mc, mr, jc, jr;
               float xk, yk, xm, ym, value;

               /* Insert a label */

               /* BOX TO AVOID */
               kc = ic-lc2-lcc;
               kr = ir-lr2-lrr;
               mc = kc+2*lcc+lc-1;
               mr = kr+2*lrr+lr-1;
               /* OK here */
               for (jc=kc;jc<=mc;jc++) {
                  if (jc >= 0 && jc < nc) {
                     for (jr=kr;jr<=mr;jr++) {
                        if (jr >= 0 && jr < nr) {
                           if (MARK(jr,jc) != 2) {
                              MARK(jr,jc) = 1;
                           }
                        }
                     }
                  }
               }

               /* BOX TO HOLD LABEL */
               kc = ic-lc2;
               kr = ir-lr2;
               mc = kc+lc-1;
               mr = kr+lr-1;
               
               /* MJK 12.10.98 */
               sprintf (lbl_str, lbl_fmt, gg);
               lbl_len = strlen (lbl_str);


               /* MJK 2.22.99 */
               if (lbl_len+ffex >= 1){
                  lbl_len += ffex;
               }

               if (((lbl_dot) || (gg < 0.0)) && (lbl_len > 2)) lbl_len--;
               kc = ic - (lbl_len / 2);
               mc = kc + lbl_len - 1;




               for (jc=kc;jc<=mc;jc++) {
                  if (jc >= 0 && jc < nc) {
                     for (jr=kr;jr<=mr;jr++) {
                        if (jr >= 0 && jr < nr) {
                           MARK(jr,jc) = 2;
                        }
                     }
                  }
               }

               xk = xd*kr+XMIN;
               yk = yd*kc+YMIN;
               xm = xd*(mr+1.0)+XMIN;
               ym = yd*(mc+1.0)+YMIN;
               /* old method:  VALUE=CLOW+INTERVAL*FLOAT((NUMC-1)/2)*/
               value = gg;

               if (*numv3 < maxv3) {
                  /* if there's room in the array, plot the label */
                  /* MJK 12.10.98 */
                  if (use_resize){
                     *numv3 += plot_label_wierd( lbl_str, xk, yk, xm, ym,
                                        vx3+(*numv3), vy3+(*numv3) );
                  }
                  else{
                     *numv3 += plot_label( lbl_str, xk, yk, xm, ym,
                                        vx3+(*numv3), vy3+(*numv3) );
                  }
/*
                  int n;
                  n = plotdigits( value, xk, yk, xm, ym,
                                  vx3+(*numv3), vy3+(*numv3) );
                  *numv3 = *numv3 + n;
*/

               }
            }

            switch (ii) {
               case 1:
                  gba = gb-ga;
                  gca = gc-ga;
                  if (ABS(gba) < 0.0000001)
                    vx[numv] = xx;
                  else
                    vx[numv] = xx+xd*(gg-ga)/gba;
                  vy[numv] = yy;
                  numv++;
                  if (ABS(gca) < 0.0000001)
                     vy[numv] = yy;
                  else
                    vy[numv] = yy+yd*(gg-ga)/gca;
                  vx[numv] = xx;
                  numv++;
                  break;

               case 2:
                  gba = gb-ga;
                  gdb = gd-gb;
                  if (ABS(gba) < 0.0000001)
                    vx[numv] = xx;
                  else
                    vx[numv] = xx+xd*(gg-ga)/gba;
                  vy[numv] = yy;
                  numv++;
                  if (ABS(gdb) < 0.0000001)
                    vy[numv] = yy;
                  else
                    vy[numv] = yy+yd*(gg-gb)/gdb;
                  vx[numv] = xx+xd;
                  numv++;
                  break;

               case 3:
                  gca = gc-ga;
                  gdb = gd-gb;
                  if (ABS(gca) < 0.0000001)
                    vy[numv] = yy;
                  else
                    vy[numv] = yy+yd*(gg-ga)/gca;
                  vx[numv] = xx;
                  numv++;
                  if (ABS(gdb) < 0.0000001)
                    vy[numv] = yy;
                  else
                    vy[numv] = yy+yd*(gg-gb)/gdb;
                  vx[numv] = xx+xd;
                  numv++;
                  break;

              case 4:
                  gca = gc-ga;
                  gdc = gd-gc;
                  if (ABS(gca) < 0.0000001)
                    vy[numv] = yy;
                  else
                    vy[numv] = yy+yd*(gg-ga)/gca;
                  vx[numv] = xx;
                  numv++;
                  if (ABS(gdc) < 0.0000001)
                    vx[numv] = xx;
                  else
                    vx[numv] = xx+xd*(gg-gc)/gdc;
                  vy[numv] = yy+yd;
                  numv++;
                  break;

               case 5:
                  gba = gb-ga;
                  gdc = gd-gc;
                  if (ABS(gba) < 0.0000001)
                    vx[numv] = xx;
                  else
                    vx[numv] = xx+xd*(gg-ga)/gba;
                  vy[numv] = yy;
                  numv++;
                  if (ABS(gdc) < 0.0000001)
                    vx[numv] = xx;
                  else
                    vx[numv] = xx+xd*(gg-gc)/gdc;
                  vy[numv] = yy+yd;
                  numv++;
                  break;

               case 6:
                  gba = gb-ga;
                  gdc = gd-gc;
                  gca = gc-ga;
                  gdb = gd-gb;
                  if (ABS(gba) < 0.0000001)
                    vx[numv] = xx;
                  else
                    vx[numv] = xx+xd*(gg-ga)/gba;
                  vy[numv] = yy;
                  numv++;
                  /* here's a brain teaser*/
                  if ( (gg>gv) ^ (ga<gb) ) {  /*XOR*/
                     if (ABS(gca) < 0.0000001)
                        vy[numv] = yy;
                     else
                        vy[numv] = yy+yd*(gg-ga)/gca;
                     vx[numv] = xx;
                     numv++;
                     if (ABS(gdb) < 0.0000001)
                        vy[numv] = yy;
                     else
                        vy[numv] = yy+yd*(gg-gb)/gdb;
                     vx[numv] = xx+xd;
                     numv++;
                  }
                  else {
                     if (ABS(gdb) < 0.0000001)
                       vy[numv] = yy;
                     else
                       vy[numv] = yy+yd*(gg-gb)/gdb;
                     vx[numv] = xx+xd;
                     numv++;
                     if (ABS(gca) < 0.0000001)
                       vy[numv] = yy;
                     else
                       vy[numv] = yy+yd*(gg-ga)/gca;
                     vx[numv] = xx;
                     numv++;
                  }
                  if (ABS(gdc) < 0.0000001)
                     vx[numv] = xx;
                  else
                     vx[numv] = xx+xd*(gg-gc)/gdc;
                  vy[numv] = yy+yd;
                  numv++;
                  break;

               case 7:
                  gdb = gd-gb;
                  gdc = gd-gc;
                  if (ABS(gdb) < 0.0000001)
                     vy[numv] = yy;
                  else
                     vy[numv] = yy+yd*(gg-gb)/gdb;
                  vx[numv] = xx+xd;
                  numv++;
                  if (ABS(gdc) < 0.0000001)
                     vx[numv] = xx;
                  else
                     vx[numv] = xx+xd*(gg-gc)/gdc;
                  vy[numv] = yy+yd;
                  numv++;
                  break;

            } /* switch */

            /* If contour level is negative, make dashed line */
            if (gg < 0.0 && idash==1) {
               float vxa, vya, vxb, vyb;
               vxa = vx[numv-2];
               vya = vy[numv-2];
               vxb = vx[numv-1];
               vyb = vy[numv-1];
               vx[numv-2] = (3.0*vxa+vxb) * 0.25;
               vy[numv-2] = (3.0*vya+vyb) * 0.25;
               vx[numv-1] = (vxa+3.0*vxb) * 0.25;
               vy[numv-1] = (vya+3.0*vyb) * 0.25;
            }

         }  /* for il */    /* NOTE:  gg incremented in for statement */

      }  /* for ic */

   }  /* for ir */

   ipnt[nump] = numv;

   /* copy vertices from vx, vy arrays to either v1 or v2 arrays */
   ip = 0;
   for (ir=0;ir<nrm && ip<2*maxtemp;ir++) {
      for (ic=0;ic<ncm && ip<2*maxtemp;ic++) {
         int start, len;
         start = ipnt[ip];
         len = ipnt[ip+1] - start;
         if (len>0) {
            if (MARK(ir,ic)==2) {
               if (*numv2+len<maxv2) {
                  memcpy( vx2+(*numv2), vx+start, len*sizeof(float) );
                  memcpy( vy2+(*numv2), vy+start, len*sizeof(float) );
                  *numv2 += len;
               }
            }
            else {
               if (*numv1+len<maxv1) {
                  memcpy( vx1+(*numv1), vx+start, len*sizeof(float) );
                  memcpy( vy1+(*numv1), vy+start, len*sizeof(float) );
                  *numv1 += len;
               }
            }

         }
         ip++;
      }
   }

   /* deallocate mark array */
   deallocate( ctx, mark, nr * nc * sizeof(char) );

   free(vx); free(vy); free(ipnt);

   return 1;
}

