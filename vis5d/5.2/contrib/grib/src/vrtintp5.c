/* vrtintp5.c version 18 Nov 93 */
#include <stdio.h>
#include <math.h>

#include "grid5d.h"
#include "want.h"

extern int bug;

int vrtintp5 (int ixt)
{
 int i,j;	/* index */
 int hrzsiz;	/* horizontal grid size = nrow * ncol */
 int ixh;	/* index for gridpoints on horizontal */
 int ixp;	/* index for pressure levels */
 int ixz;	/* index for height levels */
 int kxz;	/* index for the lowest z level to start a search */
 float zlevs[NLEV];	/* height levels to interpolate to */
 int nplevs;	/* number of pressure levels available */
 float pcol[NLEV];	/* for each gridpoint the column is put here */
 float zcol[NLEV];
 float logp[NLEV];
 float zfromlogp;
 float z_logp_max, zabs;	/* for debugging */
 float min, max;
 float lower_min_z = 9.e9;	/* lowest level minimum */
 float lower_max_z =-9.e9;	/* lowest level maximum */
 static int once = 1;	/* DEBUG */

 z_logp_max = 0.0;	/* DEBUG */
 if (bug > 3) printf("in vrtintp5\n"); if (bug > 3) fflush(NULL); /* DEBUG */
 if (bug > 3) printf("in vrtintp5 %3d %3d %3d %8.0f %8.0f\n",
 	ig.nrow,ig.ncol,ig.nver,ig.base,ig.incv);
 if (bug > 3) {
  printf("number of levels: %d\n", ts[ixt].num);
  for(i=0;i<ts[ixt].num;i++) {printf("%6.0f ",plevel[i]);} printf("\n");  }
 if (bug > 3) { once = 1; } else { once = 0; }

 nplevs = ts[ixt].num;
 for (ixp=0; ixp<nplevs; ixp++) {logp[ixp] = log(plevel[ixp]);}

 if (nplevs >= NLEV)
  {printf("vrtintp5: nplevs > NLEV  %d > %d\n", nplevs, NLEV); return -1;}
 if (ig.nver >= NLEV)
  {printf("vrtintp5: ig.nver > NLEV  %d > %d\n", ig.nver, NLEV); return -1;}

 hrzsiz = ig.nrow * ig.ncol;
 for (i=0; i<hrzsiz*ig.nver; i++) {zgrid5d[i] = 0;}
 for (i=0; i<ig.nver; i++) {zlevs[i] = ig.base + ig.incv * i;}
 kxz = -1;

 for (ixh=0; ixh<hrzsiz; ixh++)
  {
   for (i=ixh,j=0; i<hrzsiz*nplevs; i=i+hrzsiz,j++) {pcol[j] = grid5d[i];}
   if (pcol[0] < lower_min_z) lower_min_z = pcol[0];  /* debug info */
   if (pcol[0] > lower_max_z) lower_max_z = pcol[0];  /* debug info */
   for (i=0; i<NLEV; i++) {zcol[i] = 0;}
   kxz = -1;
   /* find any zlevs below lowest plevel */
   for (ixz=0; ixz<ig.nver; ixz++)
    {
     if (pcol[0] < zlevs[ixz]) break;
     kxz = ixz;
     zfromlogp = logp[1] - (pcol[1]-zlevs[ixz]) *
		( (logp[1]-logp[0]) / (pcol[1]-pcol[0]) );
     zfromlogp = exp(zfromlogp);
     zcol[ixz] = zfromlogp;
    }
   /* find zlevs between pcol[0] and pcol[nplevs-1] */
   kxz++;
   for (ixp=1; ixp<nplevs; ixp++)
    {
     for (ixz=kxz; ixz<ig.nver; ixz++)
      {
       if (pcol[ixp] < zlevs[ixz]) continue;
       zcol[ixz] = plevel[ixp-1] - (pcol[ixp-1]-zlevs[ixz])
		* ( (plevel[ixp]-plevel[ixp-1]) / (pcol[ixp]-pcol[ixp-1]) );
       zfromlogp = logp[ixp] - (pcol[ixp]-zlevs[ixz]) *
		( (logp[ixp]-logp[ixp-1]) / (pcol[ixp]-pcol[ixp-1]) );
       zfromlogp = exp(zfromlogp);
       zabs = fabs(zfromlogp-zcol[ixz]);	/* DEBUG */
       zcol[ixz] = zfromlogp;
       if (zabs > z_logp_max) z_logp_max = zabs;	/* DEBUG */
       kxz++;
      }
    }
   /* find zlevs above pcol[nplevs-a] if any */
   if (kxz < ig.nver)
    {
     for (ixz=kxz; ixz<ig.nver; ixz++)
      {
       zcol[ixz] = plevel[nplevs-1] - (pcol[nplevs-1]-zlevs[ixz])
        * ( (plevel[nplevs]-plevel[nplevs-1]) / (pcol[nplevs]-pcol[nplevs-1]) );
       zfromlogp = logp[nplevs-1] - (pcol[nplevs-1]-zlevs[ixz]) *
	( (logp[nplevs-1]-logp[nplevs-2]) / (pcol[nplevs-1]-pcol[nplevs-2]) );
       zfromlogp = exp(zfromlogp);
       zabs = fabs(zfromlogp-zcol[ixz]);	/* DEBUG */
       zcol[ixz] = zfromlogp;
       if (zabs > z_logp_max) z_logp_max = zabs;	/* DEBUG */
       kxz++;
      }
    }
   /* put zcol into the column where pcol was taken from */
   for (i=ixh,j=0; i<hrzsiz*ig.nver; i=i+hrzsiz,j++) {zgrid5d[i] = zcol[j];}
   if (once) {
    for(i=0;i<nplevs;i++) {printf("%6.0f ",pcol[i]);}printf("\n"); /*DEBUG*/
    for(i=0;i<ig.nver;i++){printf("%6.1f ",zcol[i]);}printf("\n"); /*DEBUG*/
    once = 0;}	/* DEBUG */
  }
 
 /* Get layer min-max */
 if (bug > 3) printf("layer min-max\n");
 i = -1;
 for (ixz=0; ixz<ig.nver; ixz++)
  {max = -99.e9;  min = -max;
   for (ixh=0; ixh<hrzsiz; ixh++)
    {i++;
     if (zgrid5d[i] > max) {max = zgrid5d[i];}
     if (zgrid5d[i] < min) {min = zgrid5d[i];}
    }
   if (bug > 3) printf("%2d %14.6e %14.6e\n", ixz, min, max);
  }
 if (bug>3) printf("lowest level min_z %6.0f\n", lower_min_z);
 if (bug>3) printf("lowest level max_z %6.0f\n", lower_max_z);

 if (bug > 2) printf("exit vrtintp5\n"); if (bug > 2) fflush(NULL); /* DEBUG */
 return 0;
}
