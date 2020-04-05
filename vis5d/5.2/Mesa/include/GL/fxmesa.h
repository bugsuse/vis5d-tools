/* $Id: fxmesa.h,v 1.14 1998/01/16 01:16:42 brianp Exp $ */

/*
 * Mesa 3-D graphics library
 * Version:  2.6
 * Copyright (C) 1995-1997  Brian Paul
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
 * $Log: fxmesa.h,v $
 * Revision 1.14  1998/01/16 01:16:42  brianp
 * added fxMesaUpdateScreenSize()
 *
 * Revision 1.13  1998/01/06 01:39:42  brianp
 * patched for BeOS
 *
 * Revision 1.12  1997/12/07 17:28:37  brianp
 * removed fxMesaTextureUsePalette() for v21 fxmesa driver (David B)
 *
 * Revision 1.11  1997/09/29 23:22:16  brianp
 * updated version to 2.5
 *
 * Revision 1.10  1997/09/18 23:09:49  brianp
 * set minor version number to 4
 *
 * Revision 1.9  1997/08/24 17:37:22  brianp
 * added fxMesaTextureUsePalette()
 *
 * Revision 1.8  1997/08/19 02:35:45  brianp
 * added fxQueryHardware() and fxCloseHardware()
 *
 * Revision 1.7  1997/07/08 02:15:19  brianp
 * removed USE_GLIDE_FULLSCREEN hack, no longer needed with Glide 2.3.1
 *
 * Revision 1.6  1997/06/26 14:28:42  brianp
 * set USE_GLIDE_FULLSCREEN by testing for __WIN32__
 *
 * Revision 1.5  1997/06/20 03:34:56  brianp
 * added attribList parameter to fxMesaCreateContext()
 * added FXMESA_* symbols
 *
 * Revision 1.4  1997/06/18 02:31:05  brianp
 * added comment about the USE_GLIDE_FULLSCREEN symbol
 *
 * Revision 1.3  1997/06/18 02:28:25  brianp
 * many changes for Linux support and 3Dfx Rush support
 *
 * Revision 1.2  1997/05/28 03:50:17  brianp
 * added FXMESA_MAJOR/MINOR_VERSION symbols
 *
 * Revision 1.1  1997/05/14 03:29:25  brianp
 * Initial revision
 *
 */


/*
 * FXMesa - 3Dfx Glide driver for Mesa.  Contributed by David Bucciarelli
 *
 * NOTE: This version requires Glide 2.3 or later.
 */


#ifndef FXMESA_H
#define FXMESA_H


#include <glide.h>


#ifdef __cplusplus
extern "C" {
#endif


#define FXMESA_MAJOR_VERSION 2
#define FXMESA_MINOR_VERSION 6


/*
 * Values for attribList parameter to fxMesaCreateContext():
 */
#define FXMESA_NONE		0	/* to terminate attribList */
#define FXMESA_DOUBLEBUFFER	10
#define FXMESA_ALPHA_SIZE	11      /* followed by an integer */
#define FXMESA_DEPTH_SIZE	12      /* followed by an integer */
#define FXMESA_STENCIL_SIZE	13      /* followed by an integer */
#define FXMESA_ACCUM_SIZE	14      /* followed by an integer */



typedef struct fx_mesa_context *fxMesaContext;


#if defined (__BEOS__)
#pragma export on
#endif


extern fxMesaContext fxMesaCreateContext(GLuint win, GrScreenResolution_t,
                                         GrScreenRefresh_t,
                                         const GLint attribList[]);

extern fxMesaContext fxMesaCreateBestContext(GLuint win,
                                             GLint width, GLint height,
                                             const GLint attribList[]);

extern void fxMesaDestroyContext(fxMesaContext ctx);

extern void fxMesaMakeCurrent(fxMesaContext ctx);

extern fxMesaContext fxMesaGetCurrentContext(void);

extern void fxMesaSwapBuffers(void);

extern void fxMesaSetNearFar(GLfloat nearVal, GLfloat farVal);

extern void fxMesaUpdateScreenSize(fxMesaContext ctx);

extern int fxQueryHardware(void);

extern int fxCloseHardware(void);


#if defined (__BEOS__)
#pragma export off
#endif


#ifdef __cplusplus
}
#endif


#endif
