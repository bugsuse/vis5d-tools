/* listfonts.c */

/* List all available GL fonts */


#include <stdio.h>
#include <fmclient.h>


void printname( str )
char *str;
{
   printf("%s\n", str );
}



main()
{
   fminit();
   fmenumerate( printname );
}

