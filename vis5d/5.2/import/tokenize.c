/* tokenize.c */


/*
 * Divide an input string into a sequence of tokens similar to how argc and
 * argv pass arguments to main().
 */



#include <stdlib.h>
#include <string.h>
#include "misc.h"



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


