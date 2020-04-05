/* Find the type of grid projection and then get the projection parameters.
Variable gi.projtype will contain an integer which will designate the type of 
the projection.  These projection types are
 -1 undefined
  0 nmc ktype not defined
  1 lat-lon defined by nmc ktype
  2 polarstereographic defined by nmc ktype
  3 lat-lon defined by grib Grid Description Section
*/

#include <stdio.h>

#include "gribinfo.h"
#include "ktypes.h"
#include "buffer.h"

long bword (void *, int loc, int n);	/* return n bytes at location loc */
long set_sign (long word, int nbytes);

extern int bug;

int gridproj ()
{
 int i;
 int data_rep;	/* data representation type in GDS */
 int nlat, nlon; /* number of points along a lat/lon circle */
 long la1,lo1, la2,lo2, di,dj, scan;

 gi.projtype = -1;	/* undefined projection type */
 if (gi.grid_id < 255)
  {for (i=0; i<PROJSIZ; i++)
    {if (iptype[i] != gi.grid_id) continue;
     gi.gridsize = ipgsiz[i];
     gi.projtype = igtype[i];
     gi.dimrxc = 0;	/* This is a column x row grid */
     gi.dim1 = ipc[i];
     gi.dim2 = ipr[i];
     gi.mesh = pmesh[i];	/* grid mesh length true at 60 lat */
     gi.rotat = protat[i];	/* grid rotation about 80W for polarstereo */
     gi.npi = fpi[i];		/* i location of north pole */
     gi.npj = fpj[i];
     return 0;
    }
   if(bug>3) printf("projection not found for ktype %d\n",gi.grid_id);
   if(bug>3) printf("try GDS %d\n",gi.lengds);	/* DEBUG */
   if (gi.lengds > 0)
    {gi.grid_id = 255;}
    else
    {return -1;}
  }
 if (gi.grid_id == 255)
  {
   data_rep = bword ( buf, gi.locgds+5, 1);
   if (data_rep == 0)		/* latitude-longitude grid */
    {nlat = bword ( buf, gi.locgds+6, 2);
     nlon = bword ( buf, gi.locgds+8, 2);
     gi.projtype = 3;
     gi.gridsize = nlat * nlon;
     la1 = bword ( buf, gi.locgds+10, 3);
     lo1 = bword ( buf, gi.locgds+13, 3);
     la2 = bword ( buf, gi.locgds+17, 3);
     lo2 = bword ( buf, gi.locgds+20, 3);
     di  = bword ( buf, gi.locgds+23, 2);
     dj  = bword ( buf, gi.locgds+25, 2);
     scan= bword ( buf, gi.locgds+27, 1);
     la2 =  set_sign (la2, 3);
     lo2 =  set_sign (lo2, 3);
     if (bug > 0) {
      printf("%5d %5d %6d %6d %6d %6d %5d %5d %6d\n",
        nlat,nlon,la1,lo1,la2,lo2,di,dj,scan);
      printf("%08x %08x %08x %08x %08x %08x %08x %08x %08x\n",
        nlat,nlon,la1,lo1,la2,lo2,di,dj,scan); }
     gi.dim1 = nlat;
     gi.dim2 = nlon;
     gi.lat1 = la1 * 0.001;
     gi.lon1 = lo1 * 0.001;
     gi.dlat = di * 0.001;
     gi.dlon = dj * 0.001;
     return 0;
    }
  }
 return -1;
}
