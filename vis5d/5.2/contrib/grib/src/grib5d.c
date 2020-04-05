#include <stdio.h>
#include <unistd.h>	/* read, write, lseek, close */

#define MAIN_R
#include "want.h"
#include "buffer.h"
#include "grid5d.h"
#include "gribinfo.h"
#include "ktypes.h"
#include "winds.h"

int assign5 (void);
int locate5 (int ixf);
int sort5 (void);
int fetch5 (int ixt);
int vrtintp5 (int ixt);
int open5d (void);
int write5d (int ixt, int ixv);
int vsort5 (int ixt);		/* sort by variable, time constant */
int vfetch5 (int ixv);
int vvrtint5 (int ixv);
int windcon (void);

int narg; char **varg ;
int bug;
char cerror[120];	/* for error messages */
int fdo;	/* grid5d file number */
long header[64];	/* grid header */

int main (int argc, char **argv)
{
 int statr;	/* return status */
 int ixf;	/* index for file loop */
 int ixt;	/* index for time loop */
 int ixv;	/* index for variable loop */

 printf("grib5d BETA version  17 Nov 93 reading file %s\n",argv[1]);
 varg = argv; narg = argc;
 statr = assign5 ();
 if (statr != 0) {printf("error return from assign5 %d\n",statr); return 1;}

 for (ixf=0; ixf<want.nfiles; ixf++)
  {
   statr = locate5 (ixf);
   if (statr != 0) {printf("locate5 failed\n");}
  }
 if (bug > 3) printf("from locate5 loop;  start sort5\n");

 statr = sort5 ();
 if (statr != 0) {printf("sort5 failed\n"); return 1;}

 statr = open5d ();
 if (statr != 0) {printf("open5d failed\n"); return 1;}

 if (bug > 3) printf("start time loop\n");
 /* loop through the forecast times */
 for (ixt=0; ixt<ts_num; ixt++)
  {
   if (bug > 0) {printf("process date time %5d %6d %8.2f\n",
     ts[ixt].date, ts[ixt].time, ts[ixt].valid_time);}
   ixv = 0;
   statr = fetch5 (ixt);
   if (statr != 0) {printf("fetch5 failed ts_num: %d\n", ixt); return 1;}
   statr = vrtintp5 (ixt);
   if (statr != 0) {printf("vrtintp5 failed ts_num: %d\n", ixt);}
   statr = write5d (ixt, ixv);
   if (statr != 0) {printf("write5d failed\n");}
   if (want.nvars == 0) continue;	/* height is the only variable */
   if (bug > 3) printf("process other variables\n");
   statr = vsort5 (ixt);
   for (ixv=1; ixv<want.nvars; ixv++)
    {
     if (vs[ixv].num < 1) continue;
     statr = vfetch5 (ixv);
     if (statr != 0) {printf("vfetch5 failed ts_num: %d\n", ixv);}
     statr = vvrtint5 (ixv);
     if (statr != 0) {printf("vvrtint5 failed ts_num: %d\n", ixv);}
     statr = write5d (ixt, ixv);
     if (statr != 0) {printf("write5d failed\n");}
    }
   statr = windcon ();
   if (statr != 0) {printf("windcon failed\n");}
  }

 close (fdo);
 printf("grib5d has finished\n");
 return 1;
}
