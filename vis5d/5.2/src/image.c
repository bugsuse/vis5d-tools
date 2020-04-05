/*  image.c */

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
/* Texture/image mapping */



#include <assert.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#ifdef SGI_GL
#  include <X11/Xlib.h>
#  include <gl/gl.h>
#endif
#ifdef OPENGL
#  include <GL/gl.h>
#  include <GL/glu.h>
#endif
#include "globals.h"
#include "graphics.h"
#include "rgb.h"



#ifdef SGI_GL
/*
 * Lighting and texturing properties:
 */
static float mat2[] = {
   AMBIENT, 0.5, 0.5, 0.5,   /* surface ambient color */
   DIFFUSE, 0.9, 0.9, 0.9,   /* surface diffuse color */
   LMNULL
};
static float lmodel2[] = {
   AMBIENT, 0.5, 0.5, 0.5,         /* ambient color */
   LOCALVIEWER, 0.0,               /* infinite viewpoint */
   TWOSIDE, 0,
   LMNULL
};
static float texprops[] = { TX_MINFILTER, TX_POINT,
                            TX_MAGFILTER, TX_POINT,
                            TX_NULL };
static float tevprops[] = { TV_MODULATE, TV_NULL };
static float subdiv_params[] = { 0.0, 0.0, 10.0 };
#endif



#ifdef MCIDAS
/*
 * McIDAS stuff
 */
/* API - not to globals.c:
   only one call to init_mcidas_kludge for all Vis5D contexts */
int uc[1000];
int neguc[200];
int ttyfd[10] = {1, 1, 1, 1, 1, 1, 1, 1, 1, 1};

extern void opnara_( int * );
extern void readd_( int *, int * );
extern void rhelp_( int *, int * );
extern void clsara_( int * );
extern void redara_( int *, int*, int *, int *, int *, void *);

#ifndef MCIDAS_SIDECAR
extern void kltwin_( void );
extern void lwinit_( void );

static void init_mcidas_kludge( void ) /* API - call once for all contexts */
{
  int i;

  for (i=0; i<1000; i++) uc[i] = 0;
  for (i=0; i<200; i++) neguc[i] = 0;
  kltwin_();
  lwinit_();
}
#endif /* end of ifndef MCIDAS_SIDECAR */


void zdest_( char *c, int *n, int len )
{
  int l;
  char s[100];

  l = (len < 100) ? len : 99;
  strncpy(s, c, l);
  s[l] = 0;
  printf("%s %d\n", s, *n);
}



/*
 * Load a mcidas area and store it in packed ABGR format.
 * Input:  area_num - number of mcidas area
 * Output:  width, height - size of image read
 *          data - pointer to image data
 * Return:  1 = ok, 0 = error.
 */
static int read_area( Display_Context dtx, int area_num,
                      int *width, int *height, unsigned char **data )
{
   int idir[64], bytes, e, band;
   unsigned char data1[80000];
   int x, y;
   int h, w;
   unsigned char *d;
   int ix;

#ifdef NAV
  float dx, dy;
  int qr, qc, i, j, status;
  float x, y, z;
  float lat, lon, hgt;
  float xline, xelem, xdum, aline, aelem, lpct, epct;
  static float zero = 0.0;
  int line, elem, lres, eres;

  /* get area navigation */
  if ((status = nvset_("AREA        ", &area_num, 12)) != 0) {
    if (status == -1) printf("no area %d\n", area_num);
    else if (status == -2) printf("gen_nav: no navigation for area %d\n", area_num);
    else printf("gen_nav: navigation problem for area %d %d\n", area_num, status);
    return 0; /* **** or just don't navigate area **** */
  }
#endif

   /*printf("area_num %d\n", area_num);*/
   readd_(&area_num, idir);
   if (idir[0] < 0) {
     /* unable to read area */
     printf("Error:  couldn't read AREA%04d, no areas read.\n",area_num);
     return 0;
   }

   opnara_(&area_num);

#ifdef NAV
  line = idir[5] + line * idir[11]; /* 5 - area line, 11 - area line res */
  elem = idir[6] + elem * idir[12]; /* 6 - area elem, 12 - area elem res */

  if (lres == 0) {
    printf("gen_nav: bad line res %d\n", lres);
    return 0;
  }
  else if (lres > 0) lres = idir[11] * lres;
  else lres = idir[11] / (-lres);

  if (eres == 0) {
    printf("gen_nav: bad elem res %d\n", eres);
    return 0;
  }
  else if (eres > 0) eres = idir[12] * eres;
  else eres = idir[12] / (-eres);
#endif

   *height = h = idir[8];
/*   if (h > 768) *height = h = 768; */
   *width = w = idir[9];
   bytes = idir[10];
   d = (unsigned char *) malloc( w * h * sizeof(unsigned char));
   *data = d;

   e = 0;
   band = (bytes == 1) ? 0 : 8;
   /*printf("h %d w %d bytes %d band %d\n", h, w, bytes, band);*/

   /* new area stuff */
   bytes = 1;
   rhelp_(&area_num, &bytes);

#ifdef NAV
  qr = dtx->qrows;
  qc = dtx->qcols;
  dx = (dtx->Xmax-dtx->Xmin) / (float) (qc-1);
  dy = (dtx->Ymax-dtx->Ymin) / (float) (qr-1);

  y = dtx->Ymax;
  for (i=0; i<qr; i++) {
    x = dtx->Xmin;
    for (j=0; j<qc; j++) {
      xyzPRIME_to_geo( dtx, -1, -1, x, y, 0.0, &lat, &lon, &hgt );

      if(nv1eas_(&lat, &lon, &zero, &xline, &xelem, &xdum) == 0) {
        aline = (xline - line) / lres;
        aelem = (xelem - elem) / eres;
        GET min and max line and elem
        USE to calculate area sector
        TRANSFORM area coordinates to 0.0 to 1.0 coordinates
      }
      else {

      }
      x += dx;
    }
    y -= dy;
  }
#endif

   ix = 0;
   for (y=0;y<h;y++) {
      /* int yy = (h-y-1) * w; */
      if (bytes == 1) {
         redara_(&area_num, &y, &e, width, &band, data1);
         for (x=0; x<w; x++) {
            d[ix++] = data1[x];
         }
      }
      else {
#ifdef LEAVEOUT
         redara_(&area_num, &y, &e, width, &band, data2);
         for (x=0; x<w; x+=3) {
            d[ix++] = data2[x] / 2;
            d[ix++] = data2[x+1] / 2;
            d[ix++] = data2[x+2] / 2;
            d[ix++] = 0;;
         }
#endif
      }
   }

   clsara_(&area_num);
   return 1;
}

#endif /* ifdef MCIDAS */



/*
 * Find the value nearest to n which is also a power of two.
 */
static int round2( int n )
{
   int m;

   for (m=1; m<n; m*=2)
     ;

   /* m>=n */
   if (m-n <= n-m/2) {
      return m;
   }
   else {
      return m/2;
   }
}



/**********************************************************************/
/**********************************************************************/
/*****                    Public Functions                        *****/
/**********************************************************************/
/**********************************************************************/



/*
 * Init global variables.
 */
void init_image( Display_Context dtx )
{
   int i;

   for (i=0;i<dtx->NumTimes;i++) {
      dtx->TexWidth[i] = dtx->TexHeight[i] = 0;
      dtx->TexComponents[i] = 0;
      free(dtx->TexImage[i]);
      dtx->TexImage[i] = NULL;
      dtx->TexImageNew[i] = 1; 
   }
}



/*
 * Define the texture to be used for the given timestep.  The image data
 * is not copied; just the pointer to it is saved.
 */
void define_texture( Display_Context dtx, int time, int width, int height,
                     int components, void *image )
{
   assert( time>=0 && time<=dtx->NumTimes );

   dtx->TexWidth[time] = width;
   dtx->TexHeight[time] = height;
   dtx->TexComponents[time] = components;
   if (dtx->TexImage[time]){
      free(dtx->TexImage[time]);
   }
   dtx->TexImage[time] = image;
}

/*
 * Read a texture map from the named file.  The image can be any SGI
 * .rgb file.
 * Input:  filename - name of image file
 * Return:  1 = ok, 0 = error.
 */
int read_texture_image( Display_Context dtx, char *filename )
{
   RGB_IMAGE *img;
   int width, height;
   int i;
   unsigned int *image;

   img = ReadRGB( filename );
   if (!img)
      return 0;

   width = img->sizeX;
   height = img->sizeY;
   if (width>1024) {
      /* too wide */
      FreeRGB( img );
      return 0;
   }

   image = (unsigned int *) img->data;

#ifdef OPENGL
   /* The texture size *MUST* be a power of two.  Rescale now if needed. */
   {
      int width2, height2, max;

      width2 = round2( width );
      height2 = round2( height );
      glGetIntegerv( GL_MAX_TEXTURE_SIZE, &max );
      if (width2>max)  width2 = max;
      if (height2>max)  height2 = max;
      if (width!=width2 || height!=height2) {
         unsigned int *image2;
         image2 = (unsigned int *) malloc( width2 * height2 * 4 );
         gluScaleImage( GL_RGBA,
                        width, height, GL_UNSIGNED_BYTE, image,
                        width2, height2, GL_UNSIGNED_BYTE, image2 );
         width = width2;
         height = height2;
         image = image2;
      }
   }
#endif  /*ifdef OPENGL*/

   /* use same texture for all timesteps */
   for (i=0;i<dtx->NumTimes;i++) {
      define_texture( dtx, i, width, height, 4, image );
   }

/*   FreeRGB(img);*/
   return 1;
}



/* WLH 11-25-94
 * Read a sequence of images from a file, to use for texture mapping.
 * Input:  name - name of file.
 * Return:  1 = ok, 0 = error.
 */
int read_texture_sequence( Display_Context dtx, char *name )
{
  int i, length, fd, head[3];
  int width, height, max;
  unsigned char *data;

  if ((fd = open(name, O_RDONLY, 0)) == -1) {
    return 0; /* cannot open file */
  }
  length = 3 * sizeof(int);
  if (read(fd, head, length) != length) {
    return 0; /* cannot read file header */
  }
  if (head[0] < dtx->NumTimes) {
    return 0; /* not enough time steps in file */
  }

  for (i=0;i<dtx->NumTimes;i++) {
    height = head[1];
    width = head[2];

    length = width * height * sizeof(unsigned char);
    data = (unsigned char *) malloc(length);

    if (read(fd, data, length) != length) {
      return 0; /* cannot read image from file */
    }

#ifdef OPENGL
    /* The texture size *MUST* be a power of two.  Rescale now if needed. */
    {
       int width2, height2;
       unsigned char *data2;
       width2 = round2( width );
       height2 = round2( height );
       glGetIntegerv( GL_MAX_TEXTURE_SIZE, &max );
       if (width2>max)  width2 = max;
       if (height2>max)  height2 = max;
       if (width!=width2 || height!=height2) {
          data2 = (unsigned char *) malloc( width2 * height2 );
          gluScaleImage( GL_LUMINANCE,
                         width, height, GL_UNSIGNED_BYTE, data,
                         width2, height2, GL_UNSIGNED_BYTE, data2 );
          free( data );
          width = width2;
          height = height2;
          data = data2;
       }
    }
#endif

    define_texture( dtx, i, width, height, 1, data );

  }
  return 1;
}



#ifdef MCIDAS
/*
 * Read a sequence of McIDAS area files to use for texture mapping.
 * Input:  first - number of first AREA file in the sequence.
 * Return:  1 = ok, 0 = error.
 */
int read_texture_areas( Display_Context dtx, int first )
{
   int i;
   int width, height;
   unsigned int *image;

#ifndef MCIDAS_SIDECAR
   init_mcidas_kludge();
#endif

   for (i=0;i<dtx->NumTimes;i++) {
      if (!read_area( dtx, first+i, &width, &height,
                      (unsigned char **) &image )) {
         /* error */
         return 0;
      }
#ifdef OPENGL
      /* The texture size *MUST* be a power of two.  Rescale now if needed. */
      {
         int width2, height2, max;
         unsigned int *image2;

         width2 = round2( width );
         height2 = round2( height );
         glGetIntegerv( GL_MAX_TEXTURE_SIZE, &max );
         if (width2>max)  width2 = max;
         if (height2>max)  height2 = max;
         if (width!=width2 || height!=height2) {
            unsigned int *image2;
            image2 = (unsigned int *) malloc( width2 * height2 * 4 );
            gluScaleImage( GL_LUMINANCE,
                           width, height, GL_UNSIGNED_BYTE, image,
                           width2, height2, GL_UNSIGNED_BYTE, image2 );
            free( image );
            width = width2;
            height = height2;
            image = image2;
         }
      }
#endif
      define_texture( dtx, i, width, height, 1, image );
   }
  return 1;
}
#endif  /* ifdef MCIDAS */




/*
 * Specify which texture to use.  If time==-1, disable texturing.
 */
int use_texture( Display_Context dtx, int time )
{

   assert( time>=0 && time<dtx->NumTimes );


   /* Do one-time initializations */
   if (dtx->init_flag) {  
#ifdef SGI_GL
      /* define special material and lighting model for texturing */
      lmdef( DEFMATERIAL, 11, 0, mat2 );
      lmdef( DEFLMODEL, 31, 0, lmodel2 );
#endif
#ifdef OPENGL
      glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE );
      glHint( GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST );
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
#endif
      dtx->init_flag = 0;
   } 

   /* Give the texture to the graphics library / hardware */
   if (dtx->TexImage[time]) {
      if (dtx->prev_time==-1 || dtx->TexImage[dtx->prev_time]!=dtx->TexImage[time] ||
          dtx->TexImageNew[time] == 1) {
#ifdef SGI_GL
         texdef2d( 1, dtx->TexComponents[time],
                   dtx->TexWidth[time], dtx->TexHeight[time],
                   (unsigned long *) dtx->TexImage[time],
                   5, texprops );
         tevdef( 1, 0, tevprops );
#endif
#ifdef OPENGL
         if (dtx->TexComponents[time]==1) {
            glTexImage2D( GL_TEXTURE_2D, 0,
                          1, dtx->TexWidth[time], dtx->TexHeight[time],
                          0, GL_LUMINANCE, GL_UNSIGNED_BYTE,
                          dtx->TexImage[time] );
         }
         else {
            glTexImage2D( GL_TEXTURE_2D, 0,
                          dtx->TexComponents[time],
                          dtx->TexWidth[time], dtx->TexHeight[time],
                          0, GL_RGBA, GL_UNSIGNED_BYTE, dtx->TexImage[time] );
         }
#endif
      dtx->TexImageNew[time] = 0;
      }
   }

   dtx->prev_time = time;

   return 0;
}



/*
 * Enable or disable texture mapping.
 */
static void enable_texture( int enable )
{
   
   if (enable) {
#ifdef SGI_GL
      texbind( TX_TEXTURE_0, 1 );
      tevbind( TV_ENV0, 1 );
      scrsubdivide( SS_DEPTH, subdiv_params );
      /* special lighting parameters */
      lmbind( MATERIAL, 11 );
      lmbind( LMODEL, 31 );
      /*lmcolor( LMC_AD );     Added on 4-11-95 by BEP */
      cpack( 0xffffffff );
#endif
#ifdef OPENGL
      glEnable( GL_TEXTURE_2D );
      glEnable( GL_LIGHTING );
      glColor4f( 1.0, 1.0, 1.0, 1.0 );
#endif
   }
   else {
      /* disable */
#ifdef SGI_GL
      texbind( TX_TEXTURE_0, 0 );
      tevbind( TV_ENV0, 0 );
      scrsubdivide( SS_OFF, subdiv_params );
      /* regular lighting */
      lmbind( MATERIAL, 10 );
      lmbind( LMODEL, 30 );
#endif
#ifdef OPENGL
      glDisable( GL_TEXTURE_2D );
      glDisable( GL_LIGHTING );
#endif
   }
}



/*
 * Draw a texture mapped quadrilateral mesh using the currently defined
 * texture.
 */
int texture_quadmeshnorm( int rows, int cols, float vert[][3],
                          float norm[][3], float texcoord[][2] )
{
   int i, j, base1, base2;

   /* turn on texture mapping */
   enable_texture(1);

   /* break mesh into strips */
   for (i=0;i<rows-1;i++) {
      base1 = i * cols;
      base2 = (i+1) * cols;
#ifdef SGI_GL
      if (norm) {
         bgnqstrip();
         for (j=0;j<cols;j++) {

            t2f( texcoord[base1+j] );
            n3f( norm[base1+j] );
            v3f( vert[base1+j] );

            t2f( texcoord[base2+j] );
            n3f( norm[base2+j] );
            v3f( vert[base2+j] );
         }
         endqstrip();
      }
      else {
         /* no normals */
         float n[3];
         n[0] = 0.0;  n[1] = 0.0;  n[2] = 1.0;
         n3f(n);
         bgnqstrip();
         for (j=0;j<cols;j++) {
            t2f( texcoord[base1+j] );
            v3f( vert[base1+j] );
            t2f( texcoord[base2+j] );
            v3f( vert[base2+j] );
         }
         endqstrip();
      }
#endif
#ifdef OPENGL
      glFinish();
      if (norm) {
         glBegin( GL_QUAD_STRIP );
         for (j=0;j<cols;j++) {
            glTexCoord2fv( texcoord[base1+j] );
            glNormal3fv( norm[base1+j] );
            glVertex3fv( vert[base1+j] );

            glTexCoord2fv( texcoord[base2+j] );
            glNormal3fv( norm[base2+j] );
            glVertex3fv( vert[base2+j] );
         }
         glEnd();
      }
      else {
         /* no normals */
         glNormal3f( 0.0, 0.0, 1.0 );
         glBegin( GL_QUAD_STRIP );
         for (j=0;j<cols;j++) {
            glTexCoord2fv( texcoord[base1+j] );
            glVertex3fv( vert[base1+j] );

            glTexCoord2fv( texcoord[base2+j] );
            glVertex3fv( vert[base2+j] );
         }
         glEnd();
      }
      glFinish();
#endif
   }

   /* turn off texture mapping */
   enable_texture(0);

   return 0;
}

