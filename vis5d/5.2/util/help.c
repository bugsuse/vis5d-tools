/* help.c */

/*
 * Print help information about a VIS-5D utility by scanning the source
 * code file for special comment lines.
 */


/* VIS-5D version 4.0 */

/*
Copyright (C) 1990-1994  Bill Hibbard, Brian Paul and Dave Santek

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



#include <stdio.h>
#include <string.h>



#define F77_PREFIX        "C ?"
#define C_PREFIX        "?"


/*
 * Input:  path - path to search in
 *         program - program name
 *         suffix - either ".c" or ".f"
 * Return:  1 = success, 0 = error
 */
static int print_help( path, program, suffix )
char *path;
char *program;
char *suffix;
{
   FILE *f;
   char prefix[100];
   char filename[100];
   char line[1000];
   int prefix_len;

   if (strcmp(suffix,".c")==0) {
      strcpy( prefix, C_PREFIX );
   }
   else {
      strcpy( prefix, F77_PREFIX );
   }
   prefix_len = strlen( prefix );

   /* build path/file name */
   strcpy( filename, path );
   strcat( filename, program );
   strcat( filename, suffix );

   f = fopen(filename,"r");
   if (f) {
      while (fgets(line, 80, f) != NULL) {
         if (strncmp(line, prefix, prefix_len) == 0) {
            printf("%s", line+prefix_len);
         }
      }
      fclose(f);
      return 1;
   }
   else {
      return 0;
   }
}



main(argc,argv)
int argc;
char *argv[];
{
   if (argc!=2) {
      printf("Usage:\n");
      printf("  help program\n");
      printf("where program is the name of a VIS-5D utility\n");
      exit(0);
   }

   /* vis5d is a special case */
   if (strcmp(argv[1],"vis5d")==0) {
      printf("For help on vis5d usage just type vis5d\n");
      exit(0);
   }

   if (print_help( "./util", argv[1], ".f"))  exit(0);
   if (print_help( "./util", argv[1], ".c"))  exit(0);

   printf("no help for %s\n", argv[1]);
   exit(0);
}
