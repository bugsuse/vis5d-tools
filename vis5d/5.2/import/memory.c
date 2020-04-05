/* memory.c */


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
