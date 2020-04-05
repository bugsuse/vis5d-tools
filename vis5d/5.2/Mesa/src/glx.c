/* $Id: glx.c,v 1.42 1996/02/26 16:33:26 brianp Exp $ */

/*
 * Mesa 3-D graphics library
 * Version:  1.2
 * Copyright (C) 1995-1996  Brian Paul  (brianp@ssec.wisc.edu)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */


/*
$Log: glx.c,v $
 * Revision 1.42  1996/02/26  16:33:26  brianp
 * changed version strings to 1.2.7
 *
 * Revision 1.41  1996/01/22  15:31:15  brianp
 * changed version strings to 1.2.6
 *
 * Revision 1.40  1996/01/07  22:50:23  brianp
 * improved glXGetConfig R,G,B,A-SIZE queries
 *
 * Revision 1.39  1995/12/18  17:29:11  brianp
 * use new GLdepth and GLaccum types
 *
 * Revision 1.38  1995/11/30  00:20:20  brianp
 * changed version strings to 1.2.5
 *
 * Revision 1.37  1995/11/20  20:49:47  brianp
 * check for HP color recovery atom before choosing 8-bit Pseudo over TrueColor
 *
 * Revision 1.36  1995/11/03  17:39:08  brianp
 * changed visual->class to visual->c_class for C++
 *
 * Revision 1.35  1995/11/01  15:13:40  brianp
 * renamed all class variables per Steven Spitz
 *
 * Revision 1.34  1995/10/14  16:31:52  brianp
 * don't make copy of XVisualInfo anymore
 * cleaned up some code, bumped version string to 1.2.4
 *
 * Revision 1.33  1995/09/20  18:21:15  brianp
 * support for 4-bit visuals added
 *
 * Revision 1.32  1995/09/15  18:52:32  brianp
 * updated version strings to 1.2.3
 *
 * Revision 1.31  1995/09/05  19:59:17  brianp
 * glXCreateContext() accepts non-glXChooseVisual() visuals per Wolfram Gloger
 *
 * Revision 1.30  1995/09/05  15:35:48  brianp
 * bitcount() was incorrect
 * changed find_glx_visual() to match IDs per Armin Liebchen
 *
 * Revision 1.29  1995/08/24  14:30:08  brianp
 * try to match root visual in get_visual(), per Asif Khan
 * pick 8-bit PseudoColor before 8-bit TrueColor in choose_x_visual()
 *
 * Revision 1.28  1995/08/01  20:56:39  brianp
 * lots of new GL/X visual handling code
 *
 * Revision 1.27  1995/07/24  21:38:36  brianp
 * changed choose_x_visual() to start with 8-bit CI depths
 *
 * Revision 1.26  1995/07/24  20:35:20  brianp
 * replaced memset() with MEMSET() and memcpy() with MEMCPY()
 *
 * Revision 1.25  1995/07/24  18:56:49  brianp
 * added GrayScale and StaticGray support
 * new choose_x_visual() algorithm
 * glXGetConfig(GLX_DEPTH_SIZE) returns bits, not bytes
 *
 * Revision 1.24  1995/07/06  20:14:01  brianp
 * added DirectColor support to glXChooseVisual()
 *
 * Revision 1.23  1995/06/12  15:29:06  brianp
 * search for CI visuals from shallowest to deepest per GLX_BUFFER_SIZE
 *
 * Revision 1.22  1995/06/09  21:48:22  brianp
 * changed version string to 1.2.1
 *
 * Revision 1.21  1995/05/24  13:00:15  brianp
 * updated version query functions to return 1.2
 *
 * Revision 1.20  1995/05/22  21:02:41  brianp
 * Release 1.2
 *
 * Revision 1.19  1995/05/22  16:44:48  brianp
 * added over/underlay error checking
 *
 * Revision 1.18  1995/05/19  14:22:18  brianp
 * added MESA_BACK_BUFFER environment variable
 *
 * Revision 1.17  1995/05/16  19:19:46  brianp
 * added casts to allow compiling with OpenGL's glx.h header
 * implemented GLX_color_SIZE attributes in glXGetConfig()
 *
 * Revision 1.16  1995/05/15  15:48:21  brianp
 * added share_list support to glXCreateContext
 *
 * Revision 1.15  1995/05/10  19:01:29  brianp
 * Better glXGetConfig() support from Armin Liebchen
 *
 * Revision 1.14  1995/04/17  14:40:07  brianp
 * Changed XMesaAttachTo... to XMesaBind...
 *
 * Revision 1.13  1995/04/17  14:31:26  brianp
 * GLXPixmaps implemented
 * uses new XMesaCreateContext() API
 *
 * Revision 1.12  1995/04/13  19:49:12  brianp
 * added SGI's multi-sample extension for GLX 1.1
 *
 * Revision 1.11  1995/04/11  13:40:35  brianp
 * better GLX visual handling
 * introduced GLX 1.1 functions
 *
 * Revision 1.10  1995/03/30  21:09:44  brianp
 * added 8-bit TrueColor test
 *
 * Revision 1.9  1995/03/27  20:33:12  brianp
 * added MESA_RGB_VISUAL, MESA_CI_VISUAL environment variable support
 *
 * Revision 1.8  1995/03/24  15:16:52  brianp
 * replaced ACCUM_BITS with ACC_TYPE
 *
 * Revision 1.7  1995/03/13  15:58:04  brianp
 * removed glXUseXFont stub
 *
 * Revision 1.6  1995/03/08  18:51:21  brianp
 * check if ctx is NULL in glXMakeCurrent per Thorsten Olh
 *
 * Revision 1.5  1995/03/07  19:10:19  brianp
 * look at color/index depth arguments when selecting the visual
 *
 * Revision 1.4  1995/03/04  19:29:44  brianp
 * 1.1 beta revision
 *
 * Revision 1.3  1995/03/04  19:18:17  brianp
 * new visual selection code
 *
 * Revision 1.2  1995/03/01  17:05:00  brianp
 * added error check to glXSwapBuffers
 *
 * Revision 1.1  1995/02/28  21:23:25  brianp
 * Initial revision
 *
 */


/*
 * A pseudo-GLX implementation to allow OpenGL/GLX programs to work with Mesa.
 *
 * Thanks to the contributors:
 *
 * Initial version:  Philip Brown (philb@CSUA.Berkeley.EDU)
 * Better glXGetConfig() support: Armin Liebchen (liebchen@asylum.cs.utah.edu)
 * Further visual-handling refinements: Wolfram Gloger
 *    (wmglo@Dent.MED.Uni-Muenchen.DE).
 */



#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "GL/gl.h"
#include "GL/glx.h"
#include "GL/xmesa.h"
#include "xmesaP.h"
#include "config.h"
#include "context.h"
#include "dd.h"



/*
 * Since we don't have a real GLX extension, the XVisualInfo doesn't
 * carry all the info we need such as double buffer mode, depth buffer, etc.
 * We use a table to associate this extra information with each XVisualInfo
 * structure we encounter in glXChooseVisual() or glXGetConfig().
 */
struct glx_visual {
	Display *dpy;
	XVisualInfo *visinfo;		/* The X information */
	GLboolean rgba_buffer_flag;	/* RGBA mode? */
	GLboolean double_buffer_flag;	/* double buffered? */
	GLboolean depth_buffer_flag;	/* depth buffer? */
};

#define MAX_VISUALS 100

static struct glx_visual VisualTable[MAX_VISUALS];
static int NumVisuals = 0;



/*
 * We also need to keep a list of Pixmaps created with glXCreateGLXPixmap()
 * so when glXMakeCurrent() is called we know if the drawable is a pixmap
 * or a window.
 */
#define MAX_PIXMAPS 10
static Pixmap PixmapList[MAX_PIXMAPS];
static int NumPixmaps = 0;






/*
 * Given an XVisualInfo and RGB, Double, and Depth buffer flags, save the
 * configuration in our list of GLX visuals.
 */
static void save_glx_visual( Display *dpy, XVisualInfo *vinfo,
                             GLboolean rgba, GLboolean dbl, GLboolean depth )
{
   if (NumVisuals+1<MAX_VISUALS) {
      VisualTable[NumVisuals].dpy = dpy;
      VisualTable[NumVisuals].visinfo = vinfo;
      VisualTable[NumVisuals].rgba_buffer_flag = rgba;
      VisualTable[NumVisuals].double_buffer_flag = dbl;
      VisualTable[NumVisuals].depth_buffer_flag = depth;
      NumVisuals++;
   }
   else {
      fprintf( stderr, "GLX Error: maximum number of visuals exceeded\n");
   }
}



/*
 * Find the GLX visual associated with an XVisualInfo then return the
 * RGBA, double-buffering, and depth-buffer flags.
 */
static GLboolean find_glx_visual( Display *dpy, XVisualInfo *vinfo,
				  GLboolean *rgba,
				  GLboolean *dbl,
				  GLboolean *depth )
{
   int i;

   /* First try to match pointers */
   for (i=0;i<NumVisuals;i++) {
      if (VisualTable[i].dpy==dpy && VisualTable[i].visinfo==vinfo) {
	 *rgba  = VisualTable[i].rgba_buffer_flag;
	 *dbl   = VisualTable[i].double_buffer_flag;
	 *depth = VisualTable[i].depth_buffer_flag;
	 return GL_TRUE;
      }
   }
   /* try to match visual id */
   for (i=0;i<NumVisuals;i++) {
      if (VisualTable[i].dpy==dpy
          && VisualTable[i].visinfo->visualid == vinfo->visualid) {
	 *rgba  = VisualTable[i].rgba_buffer_flag;
	 *dbl   = VisualTable[i].double_buffer_flag;
	 *depth = VisualTable[i].depth_buffer_flag;
	 return GL_TRUE;
      }
   }
   return GL_FALSE;
}



/*
 * Test if the given XVisualInfo is usable for Mesa rendering.
 */
static GLboolean is_usable_visual( XVisualInfo *vinfo )
{
#if defined(__cplusplus) || defined(c_plusplus)
   switch (vinfo->c_class) {
#else
   switch (vinfo->class) {
#endif
      case StaticGray:
      case GrayScale:
         /* Any StaticGray/GrayScale visual works in RGB or CI mode */
         return GL_TRUE;
      case StaticColor:
      case PseudoColor:
	 /* Any StaticColor/PseudoColor visual of at least 4 bits */
	 if (vinfo->depth>=4) {
	    return GL_TRUE;
	 }
	 else {
	    return GL_FALSE;
	 }
      case TrueColor:
      case DirectColor:
	 /* Any depth of TrueColor or DirectColor works in RGB mode */
	 return GL_TRUE;
      default:
	 /* This should never happen */
	 return GL_FALSE;
   }
}




/*
 * Try to get an X visual which matches the given arguments.
 */
static XVisualInfo *get_visual( Display *dpy, int scr,
			        unsigned int depth, int xclass )
{
   XVisualInfo temp;
   long mask;
   int n;
   int default_depth;
   int default_class;

   mask = VisualScreenMask | VisualDepthMask | VisualClassMask;
   temp.screen = scr;
   temp.depth = depth;
#if defined(__cplusplus) || defined(c_plusplus)
   temp.c_class = xclass;
#else
   temp.class = xclass;
#endif

   default_depth = DefaultDepth(dpy,scr);
#if defined(__cplusplus) || defined(c_plusplus)
   default_class = DefaultVisual(dpy,scr)->c_class;
#else
   default_class = DefaultVisual(dpy,scr)->class;
#endif

   if (depth==default_depth && xclass==default_class) {
      /* try to get root window's visual */
      temp.visualid = DefaultVisual(dpy,scr)->visualid;
      mask |= VisualIDMask;
   }

   return XGetVisualInfo( dpy, mask, &temp, &n );
}



/*
 * Retrieve the value of the given environment variable and find
 * the X visual which matches it.
 * Input:  dpy - the display
 *         screen - the screen number
 *         varname - the name of the environment variable
 * Return:  an XVisualInfo pointer to NULL if error.
 */
static XVisualInfo *get_env_visual( Display *dpy, int scr, char *varname )
{
   char *value;
   char type[100];
   int depth, xclass = -1;
   XVisualInfo *vis;

   value = getenv( varname );
   if (!value) {
      return NULL;
   }

   sscanf( value, "%s %d", type, &depth );

   if (strcmp(type,"TrueColor")==0)          xclass = TrueColor;
   else if (strcmp(type,"DirectColor")==0)   xclass = DirectColor;
   else if (strcmp(type,"PseudoColor")==0)   xclass = PseudoColor;
   else if (strcmp(type,"StaticColor")==0)   xclass = StaticColor;
   else if (strcmp(type,"GrayScale")==0)     xclass = GrayScale;
   else if (strcmp(type,"StaticGray")==0)    xclass = StaticGray;

   if (xclass>-1 && depth>0) {
      vis = get_visual( dpy, scr, depth, xclass );
      if (vis) {
	 return vis;
      }
   }

   fprintf( stderr, "Mesa: GLX unable to find visual class=%s, depth=%d.\n",
	    type, depth );
   return NULL;
}



/*
 * Select an X visual which satisfies the RGBA/CI flag and minimum depth.
 * Input:  dpy, screen - X display and screen number
 *         rgba - GL_TRUE = RGBA mode, GL_FALSE = CI mode
 *         min_depth - minimum visual depth
 * Return:  pointer to an XVisualInfo or NULL.
 */
static XVisualInfo *choose_x_visual( Display *dpy, int screen,
				     GLboolean rgba, int min_depth )
{
   XVisualInfo *vis;
   int xclass, visclass;
   int depth;

   if (rgba) {
      Atom hp_cr_maps = XInternAtom(dpy, "_HP_RGB_SMOOTH_MAP_LIST", True);
      /* First see if the MESA_RGB_VISUAL env var is defined */
      vis = get_env_visual( dpy, screen, "MESA_RGB_VISUAL" );
      if (vis) {
	 return vis;
      }
      /* Otherwise, search for a suitable visual */
      for (xclass=0;xclass<6;xclass++) {
	 switch (xclass) {
	    case 0:  visclass = TrueColor;    break;
	    case 1:  visclass = DirectColor;  break;
	    case 2:  visclass = PseudoColor;  break;
	    case 3:  visclass = StaticColor;  break;
	    case 4:  visclass = GrayScale;    break;
	    case 5:  visclass = StaticGray;   break;
	 }
	 if (min_depth==0) {
	    /* start with shallowest */
	    for (depth=0;depth<=24;depth++) {
               if (visclass==TrueColor && depth==8 && !hp_cr_maps) {
                  /* Special case:  try to get 8-bit PseudoColor before */
                  /* 8-bit TrueColor */
                  vis = get_visual( dpy, screen, 8, PseudoColor );
                  if (vis) {
                     return vis;
                  }
               }
	       vis = get_visual( dpy, screen, depth, visclass );
	       if (vis) {
		  return vis;
	       }
	    }
	 }
	 else {
	    /* start with deepest */
	    for (depth=24;depth>=min_depth;depth--) {
               if (visclass==TrueColor && depth==8 && !hp_cr_maps) {
                  /* Special case:  try to get 8-bit PseudoColor before */
                  /* 8-bit TrueColor */
                  vis = get_visual( dpy, screen, 8, PseudoColor );
                  if (vis) {
                     return vis;
                  }
               }
	       vis = get_visual( dpy, screen, depth, visclass );
	       if (vis) {
		  return vis;
	       }
	    }
	 }
      }
   }
   else {
      /* First see if the MESA_CI_VISUAL env var is defined */
      vis = get_env_visual( dpy, screen, "MESA_CI_VISUAL" );
      if (vis) {
	 return vis;
      }
      /* Otherwise, search for a suitable visual, starting with shallowest */
      for (xclass=0;xclass<4;xclass++) {
	 switch (xclass) {
	    case 0:  visclass = PseudoColor;  break;
	    case 1:  visclass = StaticColor;  break;
	    case 2:  visclass = GrayScale;    break;
	    case 3:  visclass = StaticGray;   break;
	 }
	 /* try 8-bit up through 16-bit */
	 for (depth=8;depth<=16;depth++) {
	    vis = get_visual( dpy, screen, depth, visclass );
	    if (vis) {
	       return vis;
	    }
	 }
	 /* try min_depth up to 8-bit */
	 for (depth=min_depth;depth<8;depth++) {
	    vis = get_visual( dpy, screen, depth, visclass );
	    if (vis) {
	       return vis;
	    }
	 }
      }
   }

   /* didn't find a visual */
   return NULL;
}


/*
 * Return the number of bits set in n.
 */
static int bitcount( unsigned long n )
{
   int bits;
   for (bits=0; n>0; n=n>>1) {
      if (n&1) {
         bits++;
      }
   }
   return bits;
}



XVisualInfo *glXChooseVisual( Display *dpy, int screen, int *list )
{
   int *parselist;
   XVisualInfo *vis;
   int min_depth = 0;
   int min_ci = 0;
   int min_red=0, min_green=0, min_blue=0, min_alpha=0;
   GLboolean rgba_flag = GL_FALSE;
   GLboolean double_flag = GL_FALSE;
   GLboolean depth_flag = GL_FALSE;

   parselist = list;

   while (*parselist) {

      switch (*parselist) {
	 case GLX_USE_GL:
	    /* ignore */
	    parselist++;
	    break;
	 case GLX_BUFFER_SIZE:
	    parselist++;
	    min_ci = *parselist++;
	    break;
	 case GLX_LEVEL:
	    /* ignore */
	    parselist++;
	    if (*parselist!=0) {
	       /* Overlays/underlays not supported */
	       return NULL;
	    }
	    parselist++;
	    break;
	 case GLX_RGBA:
	    rgba_flag = GL_TRUE;
	    parselist++;
	    break;
	 case GLX_DOUBLEBUFFER:
	    double_flag = GL_TRUE;
	    parselist++;
	    break;
	 case GLX_STEREO:
	    /* ignore */
	    parselist++;
	    break;
	 case GLX_AUX_BUFFERS:
	    /* ignore */
	    parselist++;
	    parselist++;
	    break;
	 case GLX_RED_SIZE:
	    parselist++;
	    min_red = *parselist++;
	    break;
	 case GLX_GREEN_SIZE:
	    parselist++;
	    min_green = *parselist++;
	    break;
	 case GLX_BLUE_SIZE:
	    parselist++;
	    min_blue = *parselist++;
	    break;
	 case GLX_ALPHA_SIZE:
	    /* ignore */
	    parselist++;
	    min_alpha = *parselist++;
	    break;
	 case GLX_DEPTH_SIZE:
	    depth_flag = GL_TRUE;
	    parselist++;
	    min_depth = *parselist++;
	    break;
	 case GLX_STENCIL_SIZE:
	    /* silently ignore */
	    parselist++;
	    parselist++;
	    break;
	 case GLX_ACCUM_RED_SIZE:
	 case GLX_ACCUM_GREEN_SIZE:
	 case GLX_ACCUM_BLUE_SIZE:
	 case GLX_ACCUM_ALPHA_SIZE:
	    /* silently ignore */
	    parselist++;
	    parselist++;
	    break;
	 case GLX_SAMPLES_SGIS:
	    /* silently ignored */
	    parselist++;
	    parselist++;
	    break;
	 case GLX_SAMPLE_BUFFER_SGIS:
	    /* silently ignored */
	    parselist++;
	    parselist++;
	    break;
	 case None:
	    break;
	 default:
	    /* undefined attribute */
	    return NULL;
      }
   }

   /*
    * Since we're only simulating the GLX extension this function will never
    * find any real GL visuals.  Instead, all we can do is try to find an RGB
    * or CI visual of appropriate depth.  Other requested attributes such as
    * double buffering, depth buffer, etc. will be associated with the X
    * visual and stored in the VisualTable[].
    */
   if (rgba_flag) {
      /* Get an RGB visual */
      int min_rgb = min_red + min_green + min_blue;
      if (min_rgb>1 && min_rgb<8) {
	 /* a special case to be sure we can get a monochrome visual */
	 min_rgb = 1;
      }
      vis = choose_x_visual( dpy, screen, rgba_flag, min_rgb );
   }
   else {
      /* Get a colormapped visual */
      vis = choose_x_visual( dpy, screen, rgba_flag, min_ci );
   }

   if (vis) {
      save_glx_visual( dpy, vis, rgba_flag, double_flag, depth_flag );
   }

   return vis;
}




GLXContext glXCreateContext( Display *dpy, XVisualInfo *visinfo,
			     GLXContext share_list, Bool direct )
{
   XMesaContext ctx;
   GLboolean ximage_flag = GL_TRUE;
   GLboolean rgba_flag, double_flag, depth_flag;

   if (!find_glx_visual(dpy, visinfo, &rgba_flag, &double_flag, &depth_flag)) {
      /* This visual wasn't found with glXChooseVisual() */
      if (is_usable_visual( visinfo )) {
         /* Configure this visual as RGB, double-buffered, depth-buffered. */
         /* This is surely wrong for some people's needs but what else */
         /* can be done?  They should use glXChooseVisual(). */
         rgba_flag = GL_TRUE;
         double_flag = GL_TRUE;
         depth_flag = GL_TRUE;
         save_glx_visual( dpy, visinfo, rgba_flag, double_flag, depth_flag );
      }
      else {
         fprintf(stderr,"Mesa: error in glXCreateContext: bad visual\n");
         return NULL;
      }
   }

   if (double_flag) {
      /* Check if the MESA_BACK_BUFFER env var is set */
      char *backbuffer = getenv("MESA_BACK_BUFFER");
      if (backbuffer) {
         if (backbuffer[0]=='p' || backbuffer[0]=='P') {
            ximage_flag = GL_FALSE;
         }
         else if (backbuffer[0]=='x' || backbuffer[0]=='X') {
            ximage_flag = GL_TRUE;
         }
         else {
            fprintf(stderr, "Mesa: invalid value for MESA_BACK_BUFFER ");
            fprintf(stderr, "environment variable, using an XImage.\n");
         }
      }
   }

   ctx = XMesaCreateContext( dpy, visinfo, rgba_flag, double_flag,
			        ximage_flag, (XMesaContext) share_list );
   return (GLXContext) ctx;
}



Bool glXMakeCurrent( Display *dpy, GLXDrawable drawable, GLXContext ctx )
{
   if (ctx && drawable) {
      /* determine if the drawable is a GLXPixmap */
      GLuint i;
      GLboolean pixmap_flag = GL_FALSE;
      for (i=0;i<NumPixmaps;i++) {
	 if (PixmapList[i]==drawable) {
	    pixmap_flag = GL_TRUE;
	    break;
	 }
      }
      /* Bind the XMesaContext to the pixmap or window */
      if (pixmap_flag) {
	 if (!XMesaBindPixmap( (XMesaContext) ctx, drawable )) {
	    return False;
	 }
      }
      else {
	 if (!XMesaBindWindow( (XMesaContext) ctx, drawable )) {
	    return False;
	 }
      }
      /* Make the XMesaContext the current one */
      if (XMesaMakeCurrent( (XMesaContext) ctx )) {
	 return True;
      }
      else {
	 return False;
      }
   }
   else if (!ctx && !drawable) {
      /* release current context w/out assigning new one. */
      XMesaMakeCurrent( NULL );
      return True;
   }
   else {
      /* either ctx or drawable is NULL, this is an error */
      return False;
   }
}



GLXPixmap glXCreateGLXPixmap( Display *dpy, XVisualInfo *visual, Pixmap pixmap )
{
   if (NumPixmaps==MAX_PIXMAPS) {
      fprintf( stderr, "Mesa: glXCreateGLXPixmap: too many pixmaps\n");
      return 0;
   }
   PixmapList[NumPixmaps] = pixmap;
   NumPixmaps++;
   return pixmap;
}


void glXDestroyGLXPixmap( Display *dpy, GLXPixmap pixmap )
{
   int i, j;

   for (i=0;i<NumPixmaps;i++) {
      if (PixmapList[i]==pixmap) {
	 for (j=i+1;j<MAX_PIXMAPS;j++) {
	    PixmapList[j-1] = PixmapList[j];
	 }
	 NumPixmaps--;
	 return;
      }
   }
   fprintf( stderr, "Mesa: glXDestroyGLXPixmap: invalid pixmap\n");
}


void glXCopyContext( Display *dpy, GLXContext src, GLXContext dst,
		     GLuint mask )
{
   XMesaContext xm_src, xm_dst;
   xm_src = (XMesaContext) src;
   xm_dst = (XMesaContext) dst;
   gl_copy_context( xm_src->gl_ctx, xm_dst->gl_ctx, mask );
}



Bool glXQueryExtension( Display *dpy, int *errorb, int *event )
{
   /* Mesa's GLX isn't really an X extension but we try to act like one. */
   return True;
}


void glXDestroyContext( Display *dpy, GLXContext ctx )
{
   XMesaDestroyContext( (XMesaContext) ctx );
}



Bool glXIsDirect( Display *dpy, GLXContext ctx )
{
   /* This isn't true but... */
   return True;
}



void glXSwapBuffers( Display *dpy, GLXDrawable drawable )
{
   XMesaContext ctx = XMesaGetCurrentContext();
   if (ctx->frontbuffer!=drawable) {
      fprintf( stderr,
	  "Warning: glXSwapBuffers drawable doesn't match current context\n");
   }
   else {
      XMesaSwapBuffers();
   }
}



Bool glXQueryVersion( Display *dpy, int *maj, int *min )
{
   /* Return GLX version, not Mesa version */
   *maj = 1;
   *min = 1;
   return True;
}



/*
 * Query the GLX attributes of the given XVisualInfo.
 */
int glXGetConfig( Display *dpy, XVisualInfo *visual,
		  int attrib, int *value )
{
   GLboolean rgba_flag, double_flag, depth_flag;
   int visclass;

   if (!find_glx_visual(dpy, visual, &rgba_flag, &double_flag, &depth_flag)) {
      /* this visual wasn't obtained with glXChooseVisual */
      if (is_usable_visual( visual )) {
	 /* Return "optimistic" values */
	 rgba_flag = GL_TRUE;
	 double_flag = GL_TRUE;
	 depth_flag = GL_TRUE;
         save_glx_visual( dpy, visual, rgba_flag, double_flag, depth_flag );
      }
      else {
	 /* this visual can't be used for GL rendering */
	 if (attrib==GLX_USE_GL) {
	    *value = (int) False;
	    return 0;
	 }
	 else {
	    /*fprintf( stderr, "Mesa: Error in glXGetConfig: bad visual\n");*/
	    return GLX_BAD_VISUAL;
	 }
      }
   }

   /* Get the visual class */
#if defined(__cplusplus) || defined(c_plusplus)
   visclass = visual->c_class;
#else
   visclass = visual->class;
#endif

   switch(attrib) {
      case GLX_USE_GL:
         *value = (int) True;
	 return 0;
      case GLX_BUFFER_SIZE:
	 *value = visual->depth;
	 return 0;
      case GLX_LEVEL:
	 *value = 0;
	 return 0;
      case GLX_RGBA:
	 if (rgba_flag) {
	    *value = True;
	 }
	 else {
	    *value = False;
	 }
	 return 0;
      case GLX_DOUBLEBUFFER:
	 *value = (int) double_flag;
	 return 0;
      case GLX_STEREO:
	 *value = (int) False;
	 return 0;
      case GLX_AUX_BUFFERS:
	 *value = (int) False;
	 return 0;
      case GLX_RED_SIZE:
         if (visclass==DirectColor || visclass==TrueColor) {
            *value = bitcount( visual->visual->red_mask );
         }
         else {
            /* a crude approximation */
            *value = visual->depth;
         }
	 return 0;
      case GLX_GREEN_SIZE:
         if (visclass==DirectColor || visclass==TrueColor) {
            *value = bitcount( visual->visual->green_mask );
         }
         else {
            *value = visual->depth;
         }
	 return 0;
      case GLX_BLUE_SIZE:
         if (visclass==DirectColor || visclass==TrueColor) {
            *value = bitcount( visual->visual->blue_mask );
         }
         else {
            *value = visual->depth;
         }
	 return 0;
      case GLX_ALPHA_SIZE:
         if ((visclass==DirectColor || visclass==TrueColor)
             && visual->depth==24) {
            /* if using an XImage back buffer 8-bit Alpha is available */
            *value = 8;
         }
         else {
            /* don't support alpha bitplanes otherwise */
            *value = 0;
         }
	 return 0;
      case GLX_DEPTH_SIZE:
	 if (depth_flag) {
	    *value = 8 * sizeof(GLdepth);
	 }
	 else {
	    *value = 0;
	 }
	 return 0;
      case GLX_STENCIL_SIZE:
	 *value = STENCIL_BITS;
	 return 0;
      case GLX_ACCUM_RED_SIZE:
      case GLX_ACCUM_GREEN_SIZE:
      case GLX_ACCUM_BLUE_SIZE:
      case GLX_ACCUM_ALPHA_SIZE:
	 *value = 8 * sizeof(GLaccum);
	 return 0;
      case GLX_SAMPLES_SGIS:
	 /* one sample per pixel */
	 *value = 1;
	 return 0;
      case GLX_SAMPLE_BUFFER_SGIS:
	 /* No multi-sample buffers available */
	 *value = 0;
	 return 0;
      default:
	 return GLX_BAD_ATTRIBUTE;
   }
}



GLXContext glXGetCurrentContext( void )
{
   return (GLXContext) XMesaGetCurrentContext();
}



GLXDrawable glXGetCurrentDrawable( void )
{
   XMesaContext ctx;

   ctx = XMesaGetCurrentContext();
   return ctx->frontbuffer;
}



void glXWaitGL( void )
{
   (*DD.flush)();
}



void glXWaitX( void )
{
   (*DD.flush)();
}




/*
 * New functions in GLX version 1.1
 */


const char *glXQueryExtensionsString( Display *dpy, int screen )
{
   static char *extensions = "";
   return extensions;
}



const char *glXQueryServerString( Display *dpy, int screen, int name )
{
   static char *extensions = "";
   static char *vendor = "Brian Paul";
   static char *version = "1.2.7 Mesa";

   switch (name) {
      case GLX_EXTENSIONS:
         return extensions;
      case GLX_VENDOR:
	 return vendor;
      case GLX_VERSION:
	 return version;
      default:
         return NULL;
   }
}



const char *glXGetClientString( Display *dpy, int name )
{
   static char *extensions = "";
   static char *vendor = "Brian Paul";
   static char *version = "1.2.7 Mesa";

   switch (name) {
      case GLX_EXTENSIONS:
         return extensions;
      case GLX_VENDOR:
	 return vendor;
      case GLX_VERSION:
	 return version;
      default:
         return NULL;
   }
}
