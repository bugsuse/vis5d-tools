#include <stdio.h>

#include "want.h"
#include "buffer.h"
#include "gribinfo.h"
#include "grid5d.h"

void w3fb00 (float lat, float lon, float mesh, float *i, float *j);
float w3ft01 (float sti, float stj, float *fld, int ii, int jj, int ncyclk,
		int lin);

extern int bug;

int hrzintp5 (int ixg)
{
 int nc,nr;	/* column-row index */
 float lat,lon, xi,xj, fint;
 float min = 999.e9, max = -999.e9;
 int k;		/* grid5d index */
 int imiss = 0;	/* number of missing gridpoints, outside nmc grid domain */
 const long miss = 0x80808080;	/* missing flag */

 if (bug > 3) printf("in hrzintp5\n"); if (bug > 3) fflush(NULL); /* DEBUG */
 if (bug > 3) printf("level %2d %6.1f %6.1f %5.1f %3d %3d\n",
	ixg, ig.nlat, ig.wlon, ig.linc, ig.nrow, ig.ncol);
 if (bug > 3) printf("%3d %5d %5d %2d %8.1f\n",
	gi.projtype, gi.dim1,gi.dim2,gi.dimrxc,gi.mesh);
 /* Check for grid index exceeding size of buffer 'zgrid5d'. ixg is zero based.
    G5D is the dimension of 'zgrid5d'. */
 if (ig.nrow*ig.ncol*(ixg+1) > G5D)
  {printf("hrzintp5: nrow*ncol*(ixg+1) > G5D    %d * %d * %d > %d\n",
    ig.nrow, ig.ncol, ixg, G5D);
   return -1; }

 plevel[ixg] = gi.level;
 switch (gi.projtype) {
  case 2:                   /* projtype = 2 are NMC polarstereographic grids */
  k = ig.nrow * ig.ncol * ixg - 1;
  lon = ig.wlon + ig.linc;
  for (nc=0; nc<ig.ncol; nc++)
   {
    lon = lon - ig.linc;
    lat = ig.nlat + ig.linc;
    for (nr=0; nr<ig.nrow; nr++)
     {lat = lat - ig.linc;
      w3fb00 (lat, lon+gi.rotat, gi.mesh, &xi,  &xj);
      xi = gi.npi + xi;		xj = gi.npj + xj;
      if (xi < 1.0 || xi > gi.dim1) {grid5d[k] = miss; imiss++; continue;}
      if (xj < 1.0 || xj > gi.dim2) {grid5d[k] = miss; imiss++; continue;}
      fint = w3ft01 (xi, xj, ubuf, gi.dim1, gi.dim2, 0, 1);
      /* DEBUG */
      if (bug > 3) {
      if (nr == 0 && nc == 0) {
      printf("hrzintp5 %d %d %6.1 %6.1 %6.1 %6.1 %6.1 %6.1 %6.1 %6.1 %f \n",
	   nr,nc, lat,lon, xj,xi, gi.npj,gi.npi,gi.mesh,gi.rotat,fint); }
      } /* DEBUG */
      k++;
      grid5d[k] = fint;
      if (fint > max) max = fint;
      if (fint < min) min = fint;
     }
   }
  break;	/* end of case 2 */

  case 3:	/* projtype = 3 are longitude x latitude grids */
  if (bug > 3) {
  printf("3 %6.1f %6.1f %5.1f %6.1f %6.1f %5.1f %5.1f %d %d\n",	/* DEBUG */
   ig.nlat,ig.wlon,ig.linc,
   gi.lat1,gi.lon1, gi.dlat,gi.dlon, gi.dim1,gi.dim2); } /*DEBUG*/
  k = ig.nrow * ig.ncol * ixg - 1;
  lon = ig.wlon + ig.linc;
  for (nc=0; nc<ig.ncol; nc++)
   {
    lon = lon - ig.linc;
    lat = ig.nlat + ig.linc;
    for (nr=0; nr<ig.nrow; nr++)
     {lat = lat - ig.linc;
      xj = (gi.lat1 - lat) / gi.dlat + 1.0;
      xi = (gi.lon1 - lon) / gi.dlon + 1.0;
      /* check for wrap around -180. west */
      /*if (gi.lon1-lon < 0.0)
       {xi = (gi.lon1+360. - lon) / gi.dlon + 1.0;}*/
      if (lon > gi.lon1) {xi = (gi.lon1+360. - lon) / gi.dlon + 1.0;}
      if (xi < 1.0 || xi > gi.dim1) {grid5d[k] = miss; imiss++; k++; continue;}
      if (xj < 1.0 || xj > gi.dim2) {grid5d[k] = miss; imiss++; k++; continue;}
      fint = w3ft01 (xi, xj, ubuf, gi.dim1, gi.dim2, 0, 1);
      k++;
      grid5d[k] = fint;
      if (fint > max) max = fint;
      if (fint < min) min = fint;
     }
   }
  break;	/* end of case 3 */

  default:  printf("projection type not defined %d\n",gi.projtype); return -1;
 }	/* end switch */
 if (imiss > 0) {printf ("missing: %d\n", imiss);}
 if (bug > 3) printf("min_max %14.6e %14.6e\n",min,max);

 if (bug > 3) printf("exit hrzintp5\n"); if (bug > 3) fflush(NULL); /* DEBUG */
 return 0;
}
