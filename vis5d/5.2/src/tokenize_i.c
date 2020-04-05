/* tokenize.c */
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
 * Divide an input string into a sequence of tokens similar to how argc and
 * argv pass arguments to main().
 */



#include <stdlib.h>
#include <string.h>
#include "misc_i.h"



#define MAX_TOKENS  1000
#define MAX_TOKEN_LENGTH 1000



char **tokenize( char *str, int *ntokens )
{
   char temp[MAX_TOKEN_LENGTH];
   char **tokens;
   int i, j, n;


   /* allocate an array [MAX_TOKENS] of pointer to char */
   tokens = (char **) calloc( MAX_TOKENS, sizeof(char *) );
   if (!tokens) {
      return NULL;
   }

   i = j = n = 0;
   for (i=0;;i++) {
      if (str[i]==' ' || str[i]=='\t' || str[i]=='\n' || str[i]==0) {
         if (j>0) {
            /* end of a token */
            temp[j] = 0;
            if (n>=MAX_TOKENS) break;  /*done*/
            tokens[n++] = str_dup(temp);
            j = 0;
         }
         if (str[i]==0) break;  /*all done*/
      }
      else {
         if (j<MAX_TOKEN_LENGTH) {
            temp[j] = str[i];
            j++;
         }
      }
   }

   *ntokens = n;

   return tokens;
}



void free_tokens( char **tokens )
{
   int i;

   for (i=0;tokens[i] && i<MAX_TOKENS;i++) {
      free( tokens[i] );
   }

   free( tokens );
}


