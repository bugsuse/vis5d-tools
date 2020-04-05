/* misc.c */


#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "v5d.h"


/*
 * This function works just like strdup() found on _most_ but not _all_
 * systems.
 */
char *str_dup( char *s )
{
   int len = strlen(s) + 1;
   char *s2 = (char *) malloc( len );
   if (s2) {
      memcpy( s2, s, len );
   }
   return s2;
}



void print_min_max( float *data, int n )
{
   int i;
   float min, max;
   int missing = 0;

   min = 1.0e30;
   max = -1.0e30;

   for (i=0;i<n;i++) {
      if (IS_MISSING( *data )) {
         /*printf("missing: %g %d\n", *data );*/
         missing++;
      }
      else {
         if (*data>max)  max = *data;
         if (*data<min)  min = *data;
      }
      data++;
   }

   printf("min=%g  max=%g  missing=%d\n", min, max, missing );
}
