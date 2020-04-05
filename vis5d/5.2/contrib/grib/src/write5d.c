#include <stdio.h>
#include <errno.h>
#include <fcntl.h>	/* open */
#include <unistd.h>	/* read, write, lseek, close */

#include "want.h"
#include "grid5d.h"
#include "winds.h"

extern int narg;
extern char **varg ;
extern long header[64];
extern int fdo;
extern int bug;

extern int pvv_flag;

int write5d (int ixt, int ixv)
{
 int i;		/* index */
 static int ixgrid = 0;
 int lpos, loc_next;
 char cerror[120];	/* for error messages */
 long gridhead[64];	/* the header for a grid */
 int bytes_written;
 const long Wident = 0x57202020;	/* 'W   ' for w winds */

 if (bug > 3) printf("in write5d\n"); if (bug > 3) fflush(NULL); /* DEBUG */

 if (bug > 3) printf("ixgrid: %d %d %d\n",ixgrid, ixt, ixv);
 if (bug > 3) printf("grid id %d\n", want.vars[ixv]); /* DEBUG */
 for (i=0; i<64; i++) {gridhead[i] = 0;}
 gridhead[0] = ig.nrow * ig.ncol * ig.nver;
 gridhead[1] = ig.nrow;
 gridhead[2] = ig.ncol;
 gridhead[3] = ig.nver;
 gridhead[4] = header[12] + header[10]*ixgrid;
 gridhead[5] = ts[ixt].date;
 gridhead[6] = ts[ixt].time;
 gridhead[8] = want.var_name[ixv];
 if (pvv_flag) gridhead[8] = Wident;
 for (i=0; i<5; i++) {gridhead[21+i] = ig.h_latlon[i];}
 for (i=0; i<3; i++) {gridhead[30+i] = ig.h_height[i];}
 /* write out the grid header */
 loc_next = 256 * (ixgrid+1);
 if (bug > 3) printf("loc_next %d \n", loc_next);
 if (lpos = lseek(fdo, loc_next, 0) < 0)
  {sprintf(cerror,"%s: lseek error l1 errno= %d fdo: %d %d %d\n",
     varg[0],errno,fdo,loc_next,lpos);
   perror(cerror); printf(cerror); return -1;}
 bytes_written = write (fdo, gridhead, 256);
 if (bytes_written != 256 && errno != 0)
  {sprintf(cerror,"%s: write5d: write error w1 errno= %d fdo: %d "
     " %d %d\n",varg[0],errno,fdo,bytes_written,256);
   printf(cerror); perror(cerror); }
 /* write out the 3-D grid */
 errno = 0;
 loc_next = (header[12] + header[10]*ixgrid) * 4;
 if (bug > 3) printf("loc_next %d \n", loc_next);
 if (lpos = lseek(fdo, loc_next, 0) < 0)
  {sprintf(cerror,"%s: lseek error l2 errno= %d fdo: %d %d %d\n",
     varg[0],errno,fdo,loc_next,lpos);
   perror(cerror); printf(cerror); return -1;}
 if (ixv == 0)
  {bytes_written = write (fdo, zgrid5d, gridhead[0]*4);}
  else
  {bytes_written = write (fdo, vgrid5d, gridhead[0]*4);}
 if (bytes_written != gridhead[0]*4 && errno != 0)
  {sprintf(cerror,"%s: write5d: write error w2 errno= %d fdo: %d "
     " %d %d\n",varg[0],errno,fdo,bytes_written,gridhead[0]*4);
   printf(cerror); perror(cerror); }
 /* DEBUG print */
 if (bug > 3) {
 if (ixv == 0)
  {printf("%16.8e ",grid5d[0]);
   printf("%f ",grid5d[0]);
   printf("%08x ",*(long *)&grid5d[0]);
   printf("%08x \n",grid5d[0]); }
  else
  {printf("%16.8e ",vgrid5d[0]);
   printf("%f ",vgrid5d[0]);
   printf("%08x \n",vgrid5d[0]); }  }
 if (bug > 0) printf("wrote grid number %2d %2d %.4s\n",
    ixgrid,ixv,&gridhead[8]);
 ixgrid++;

 if (want.vars[ixv] == winds.u_id)
  {
   winds.uloc = loc_next;
   winds.nwind++;
   if (bug > 3) printf("write5d: u %d %d\n", winds.nwind,winds.uloc);
  }
 if (want.vars[ixv] == winds.v_id)
  {
   winds.vloc = loc_next;
   winds.nwind++;
   if (bug > 3) printf("write5d: v %d %d\n", winds.nwind,winds.vloc);
  }

 pvv_flag = 0;	/* set the pressure vertical velocity flag to off or false */
 return 0;
}
