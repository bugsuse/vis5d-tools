/* amesa.h */

/*
 * Mesa 3-D graphics library
 * Version:  1.2
 * Copyright (C) 1995  Brian Paul  (brianp@ssec.wisc.edu)
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
$Id: amesa.h,v 1.4 1995/05/22 17:03:21 brianp Exp $

$Log: amesa.h,v $
 * Revision 1.4  1995/05/22  17:03:21  brianp
 * Release 1.2
 *
 * Revision 1.3  1995/03/31  15:22:40  brianp
 * changed XMESA_H to AMESA_H
 *
 * Revision 1.2  1995/03/04  19:45:47  brianp
 * 1.1 beta revision
 *
 * Revision 1.1  1995/02/28  21:21:03  brianp
 * Initial revision
 *
 */


/*
 * Mesa/Amiga interface.  Warning:  the src/amesa.c file is out of date
 * and doesn't work at this time.
 */


/* Example usage:

1. Make a window using Intuition calls

2. Call AMesaCreateContext() to make a rendering context and attach it
   to the window made in step 1.

3. Call AMesaMakeCurrent() to make the context the active one.

4. Make gl* calls to render your graphics.

5. When exiting, call AMesaDestroyContext().

*/



#ifndef AMESA_H
#define AMESA_H


#include <intuition/intuition.h>
#include "GL/gl.h"


typedef struct amesa_context *AMesaContext;



/*
 * Attach a Mesa rendering context to an Amiga Intuition window.
 * Input:  window - the Intuition window ptr
 *         db_flag - should it be double buffered?
 *         rgb_flag - should RGB mode be simulated?
 */
extern AMesaContext AMesaCreateContext( struct Window *window,
				        GLboolean db_flag,
				        GLboolean rgb_flag );


/*
 * Destroy a context made by AMesaCreateContext.
 */
extern void AMesaDestroyContext( AMesaContext ctx );


/*
 * Make the specified context the current context.
 */
extern int AMesaMakeCurrent( AMesaContext ctx );


/*
 * Return a handle to the current context.
 */
extern AMesaContext AMesaGetCurrentContext( void );


/*
 * Copy the back buffer to the front.
 */
extern void AMesaSwapBuffers( void );


#endif
