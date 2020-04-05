/*
Variable winds.projtype will contain an integer which will designate the type
of the projection.  These projection types are
 -1 undefined
  0 nmc ktype not defined
  1 lat-lon defined by nmc ktype
  2 polarstereographic defined by nmc ktype
  3 lat-lon defined by grib Grid Description Section
*/

#include <stdio.h>
#include <math.h>
#include <errno.h>
#include <unistd.h>	/* read, write, lseek, close */

#include "winds.h"
#include "want.h"
#include "buffer.h"
#define vbuf ((float *)buf)

void w3fb00 (float lat, float lon, float mesh, float *i, float *j);
void w3fc02 (float ffid,float ffjd,float fgu,float fgv,float *dir,float *spd);

extern int narg;
extern char **varg ;
extern int bug;
extern fdo;
extern char cerror[120];	/* for error messages */

int windcon ()
{
 const double deg_to_rad = 3.1415926 / 180.0;
 float flat, flon, xi,xj;
 float dir, spd;
 float yi, yj;	/* DEBUG */
 int lpos, n_read, n_bytes;
 int ix, iy, iz;	/* indices for column, row, vertical */
 int k, kx, ixh;

 if (bug>3) printf("in windcon %d %d\n", winds.nwind,winds.projtype);
 if (bug>3) fflush (NULL); /* DEBUG */

 if (winds.projtype == -1) { return 0; }
 if (winds.projtype ==  0) { return 0; }
 if (winds.projtype ==  1) { return 0; }
 if (winds.projtype ==  3) { return 0; }
 if (winds.nwind < 0)      { return 0; }
 if (bug>3) printf("do wind converson\n"); /* DEBUG */

 n_bytes = ig.nrow * ig.ncol * ig.nver * 4;

 /* read in the u & v wind fields */
 errno = 0;
 if (lpos = lseek(fdo, winds.uloc, 0) < 0)
  {sprintf(cerror,"%s: windcon: lseek error l1 errno= %d fdo: %d %d %d\n",
     varg[0],errno,fdo,winds.uloc,lpos);
     perror(cerror); printf(cerror); return -1;}
 errno = 0;
 n_read = read (fdo, ubuf, n_bytes);
 if (n_read != n_bytes && errno != 0)
  {sprintf(cerror,"%s: windcon: read error r1 errno= %d fdo: %d "
     " %d %d\n",varg[0],errno,fdo,n_read,n_bytes);
   printf(cerror); perror(cerror); }

 errno = 0;
 if (lpos = lseek(fdo, winds.vloc, 0) < 0)
  {sprintf(cerror,"%s: windcon: lseek error l2 errno= %d fdo: %d %d %d\n",
     varg[0],errno,fdo,winds.uloc,lpos);
     perror(cerror); printf(cerror); return -1;}
 errno = 0;
 n_read = read (fdo, vbuf, n_bytes);
 if (n_read != n_bytes && errno != 0)
  {sprintf(cerror,"%s: windcon: read error r2 errno= %d fdo: %d "
     " %d %d\n",varg[0],errno,fdo,n_read,n_bytes);
   printf(cerror); perror(cerror); }

 /*return 0;*/ /* DEBUG */
 /* Recompute the u & v wind components */
 if (winds.projtype == 2)
  {
   ixh = ig.ncol * ig.nrow;
   flon = ig.wlon + ig.linc;
   k = -1;
   for (ix=0; ix<ig.ncol; ix++)
    {
     flon = flon - ig.linc;
     flat = ig.nlat + ig.linc;
     for (iy=0; iy<ig.nrow; iy++)
      {
       k++;
       flat = flat - ig.linc;
       w3fb00 (flat, flon+winds.rotat, winds.mesh, &xi, &xj);
       /* xi = winds.npi - xi;   xj = winds.npj - xj; */
       yi =  xi;   yj = xj;
       xi = -xi;   xj = -xj;
       for (iz=0; iz<ig.nver; iz++)
        {
         kx = k + ixh * iz;
         if (kx > BUFF) {printf("kx %d\n",kx); goto error;}
         w3fc02 (xi, xj, ubuf[kx], vbuf[kx], &dir, &spd);
         if (bug > 3) {
          if (k == 0) {printf("%6.1f %6.1f %6.1f %6.1f",
            ubuf[kx],vbuf[kx],dir,spd);}
          if(iy==0&&ix== 7){printf("%6.1f %6.1f %6.1f %6.1f",
            ubuf[kx],vbuf[kx],dir,spd);}
          if(iy==0&&ix==14){printf("%6.1f %6.1f %6.1f %6.1f",
            ubuf[kx],vbuf[kx],dir,spd);}  }
         ubuf[kx] = -spd * sin(dir*deg_to_rad);
         vbuf[kx] = -spd * cos(dir*deg_to_rad);
         if (bug > 3) {
          if (k == 0) {printf(" %6.1f %6.1f", ubuf[kx], vbuf[kx]);
      	    printf(" %5.1f %5.1f %5.1f %5.1f\n", flat,flon, xi, xj); }
          if (iy==0&&ix==7) {printf(" %6.1f %6.1f", ubuf[kx], vbuf[kx]);
  	    printf(" %5.1f %5.1f %5.1f %5.1f\n", flat,flon, xi, xj); }
          if (iy==0&&ix==14) {printf(" %6.1f %6.1f", ubuf[kx], vbuf[kx]);
    	    printf(" %5.1f %5.1f %5.1f %5.1f\n", flat,flon, xi, xj); } }
        }
      }
    }
  }

 /* write out the u & v wind fields */
 errno = 0;
 if (lpos = lseek(fdo, winds.uloc, 0) < 0)
  {sprintf(cerror,"%s: windcon: lseek error l3 errno= %d fdo: %d %d %d\n",
     varg[0],errno,fdo,winds.uloc,lpos);
     perror(cerror); printf(cerror); return -1;}
 errno = 0;
 n_read = write (fdo, ubuf, n_bytes);
 if (n_read != n_bytes && errno != 0)
  {sprintf(cerror,"%s: windcon: write error w1 errno= %d fdo: %d "
     " %d %d\n",varg[0],errno,fdo,n_read,n_bytes);
   printf(cerror); perror(cerror); }

 errno = 0;
 if (lpos = lseek(fdo, winds.vloc, 0) < 0)
  {sprintf(cerror,"%s: windcon: lseek error l4 errno= %d fdo: %d %d %d\n",
     varg[0],errno,fdo,winds.uloc,lpos);
     perror(cerror); printf(cerror); return -1;}
 errno = 0;
 n_read = write (fdo, vbuf, n_bytes);
 if (n_read != n_bytes && errno != 0)
  {sprintf(cerror,"%s: windcon: write error w2 errno= %d fdo: %d "
     " %d %d\n",varg[0],errno,fdo,n_read,n_bytes);
   printf(cerror); perror(cerror); }

 if (bug > 3) printf("exit windcon\n");
 if (bug > 2) fflush (NULL); /* DEBUG */
 return 0;
 error: return -1;
}
