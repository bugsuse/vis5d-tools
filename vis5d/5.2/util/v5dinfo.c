/* v5dinfo.c */



#include <stdio.h>
#include <stdlib.h>
#include "v5d.h"




main( int argc, char *argv[] )
{
   v5dstruct v;

   if (argc!=2) {
      printf("Usage:\n");
      printf("   v5dinfo file\n");
      exit(0);
   }

   if (!v5dOpenFile( argv[1], &v )) {
      printf("Error: couldn't open %s\n", argv[1] );
      exit(0);
   }

   v5dPrintStruct( &v );

   v5dCloseFile( &v );
   exit(0);
}
