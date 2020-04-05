/* vvrtint5.c version 18 Nov 93 */
#include <stdio.h>
#include <math.h>

#include "grid5d.h"
#include "want.h"

extern int bug;

int vvrtint5 (int ixv)
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
 float pvcol[NLEV];	/* variable on pressure surfaces */
 float zvcol[NLEV];	/* variable on height surfaces, to be computed */
 float logp[NLEV];
 float logpv;
 static int once = 1;	/* DEBUG */

 if(bug>3) printf("in vvrtint5\n"); if(bug>3) fflush(NULL); /* DEBUG */
 if (bug > 3) { once = 1; } else { once = 0; }

 nplevs = vs[ixv].num;
 for (ixp=0; ixp<nplevs; ixp++) {logp[ixp] = log((double)plevel[ixp]);}

 if (nplevs >= NLEV)
  {printf("vvrtint5: nplevs > NLEV  %d > %d\n", nplevs, NLEV); return -1;}
 if (ig.nver >= NLEV)
  {printf("vvrtint5: ig.nver > NLEV  %d > %d\n", ig.nver, NLEV); return -1;}

 for (i=0; i<ig.nver; i++) {zlevs[i] = ig.base + ig.incv * i;}
 hrzsiz = ig.nrow * ig.ncol;
 kxz = -1;

 for (ixh=0; ixh<hrzsiz; ixh++)
  {
   for (i=0; i<NLEV; i++) {zvcol[i] = 0;}
   for (i=ixh,j=0; i<hrzsiz*ig.nver; i=i+hrzsiz,j++) {pcol[j] = zgrid5d[i];}
   for (i=ixh,j=0; i<hrzsiz*nplevs;  i=i+hrzsiz,j++) {pvcol[j] = grid5d[i];}
   kxz = -1;
   /* find any zlevs below lowest plevel */
   for (ixz=0; ixz<ig.nver; ixz++)
    {
     if (plevel[0] > pcol[ixz]) break;
     kxz = ixz;
     /*zvcol[ixz] = pvcol[0] - (pcol[0]-zlevs[ixz]) * (pvcol[1]-pvcol[0])
						  / (pcol[1]-pcol[0]);*/
     logpv = log((double)pcol[ixz]);
     zvcol[ixz] = pvcol[0] - (logp[0]-logpv) * (pvcol[0]-pvcol[1])
						  / (logp[0]-logp[1]);
    }
   /* find pcol's between plevel[0] and plevel[nplevs-1] */
   kxz++;
   for (ixp=1; ixp<nplevs; ixp++)
    {
     for (ixz=kxz; ixz<ig.nver; ixz++)
      {
       if (plevel[ixp] > pcol[ixz]) continue;
       /*zvcol[ixz] = pvcol[ixp-1] - (pcol[ixp-1]-zlevs[ixz])
		* ( (pvcol[ixp]-pvcol[ixp-1]) / (pcol[ixp]-pcol[ixp-1]) );*/
       logpv = log((double)pcol[ixz]);
       zvcol[ixz] = pvcol[ixp] - (logp[ixp]-logpv) * (pvcol[ixp]-pvcol[ixp-1])
						  / (logp[ixp]-logp[ixp-1]);
       kxz++;
      }
    }
   /* find pcol's above plevel[nplevs-a] if any */
   for (ixz=kxz; ixz<ig.nver; ixz++)
    {
     logpv = log((double)pcol[ixz]);
     zvcol[ixz] = pvcol[nplevs-1] - (logp[nplevs-1]-logpv)
                   * (pvcol[nplevs-1]-pvcol[nplevs-2])
          		  / (logp[nplevs-1]-logp[nplevs-2]);
     kxz++;
    }
   /* put zvcol into the column where pcol was taken from */
   for (i=ixh,j=0; i<hrzsiz*ig.nver; i=i+hrzsiz,j++) {vgrid5d[i] = zvcol[j];}
  }
 if (once) {
  for(i=0;i<nplevs;i++) {printf("%6.0f ",plevel[i]);}printf("\n"); /*DEBUG*/
  for(i=0;i<nplevs;i++) {printf("%6.0f ",pvcol[i]);}printf("\n"); /*DEBUG*/
  for(i=0;i<ig.nver;i++){printf("%6.1f ",pcol[i]);}printf("\n"); /*DEBUG*/
  for(i=0;i<ig.nver;i++){printf("%6.1f ",zvcol[i]);}printf("\n"); /*DEBUG*/
  once = 0;}	/* DEBUG */
 if(bug>3) printf("exit vvrtint5\n"); if (bug>2) fflush(NULL); /* DEBUG */
 return 0;
}
