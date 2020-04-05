/* memory.c */
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


/*
 * malloc/free wrappers to help with debugging.
 */

#include <stdlib.h>
#include <stdio.h>


void *MALLOC( size_t size )
{
   void *p;
   p = malloc( size );
/*
   if (size == 0) {
     printf("MALLOC: size = 0\n");
   }
   printf("MALLOC(%d) = 0x%x\n", size, p );
*/
   return p;
}


void FREE( void *ptr, int id )
{
/*   printf("FREE(0x%x) id=%d\n", ptr, id );*/
   free( ptr );
}
