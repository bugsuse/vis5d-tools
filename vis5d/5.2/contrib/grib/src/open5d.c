#include <stdio.h>
#include <errno.h>
#include <fcntl.h>	/* open */
#include <unistd.h>	/* read, write, lseek, close */

#include "want.h"
#define PARMS 0666

extern long header[64];	/* gridfile header, first 64 words */
extern int fdo;	/* grid5d file number */
extern int narg;
extern char **varg ;
extern int bug;

int open5d (void)
{
 int i;	/* index */
 static int new_grid;
 int bytes_written, num_bytes;
 char cerror[120];

 if (bug > 3) printf("in open5d\n"); if (bug > 3) fflush(NULL); /* DEBUG */
 fdo = open (want.grid5d_file, O_RDWR, 0);
 if (fdo < 0)
  {fdo = open (want.grid5d_file, O_RDWR | O_CREAT, PARMS) ;
   if (bug > 3) printf (" open:create for read-write  fd= %4d\n",fdo);
   if (fdo < 0)
    {sprintf(cerror,"%s: open5d: open error o1 errno= %d file: %s\n",
            varg[0],errno,want.grid5d_file);
     perror(cerror); printf(cerror); return -1;}
   new_grid = 1;}
  else
  {new_grid = 0;
   if (bug > 3) printf("open:file exits for read-write\n");}
 if (bug > 3) printf ("output file is %d\n", fdo);

 for (i=0; i<64; i++) {header[i] = 0;}
 /* words 0 to 7 are identification (ascii characters) */
 for (i=0; i<8; i++) {header[i] = want.title[i];}
 /* word 8 is project number word 9 is date as YYDDD */
 header[8] = 1;
 /* word 10 is the maximum size of a 3-D grid in this file */
 header[10] = ig.nrow * ig.ncol * ig.nver;
 /* word 11 is the maximum number of grids */
 header[11] = ts_num * want.nvars;
 /* word 12 is the number of words reserved for headers (word 11 + 1) * 64 */
 header[12] = header[11] * 64 + 64;
 if (bug > 3) printf("header word 10: %d\n",header[10]);
 if (bug > 3) printf("header word 11: %d\n",header[11]);
 if (bug > 3) printf("header word 12: %d words reserved for headers\n",header[12]);

 num_bytes = 256;
 bytes_written = write (fdo, header, num_bytes);
 if (num_bytes != bytes_written && errno != 0)
  {sprintf(cerror,"%s: open5d: write error w1 errno= %d fdo: %d "
     " %d %d\n",varg[0],errno,fdo,num_bytes,bytes_written);
   printf(cerror); perror(cerror); }

 if (bug > 3) printf("exit open5d\n"); if (bug > 3) fflush(NULL); /* DEBUG */
 return 0;
}
