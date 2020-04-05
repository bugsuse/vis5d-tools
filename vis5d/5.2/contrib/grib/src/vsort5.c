#include <stdio.h>

#include "want.h"
#include "winds.h"

extern int bug;

int vsort5 (int ixt)
{
 int i,j;	/* indices */
 int ixv, ixf, ixg;
 int lxf, lxs;
 int n_var_sort;
 int var_sort[NFILES];	/* find files containing grids for fixed time */
 int sort[NGRID];

 if (bug > 3) printf("in vsort5\n"); if(bug>3) fflush(NULL); /* debug */
 /* Initialize the wind index to -1 to indicate no wind levels for this pass */
 winds.nwind = -1;
 winds.grid_id = -1;
 winds.projtype = -1;
 for (i=0; i<NVAR; i++) {vs[i].num = -1;}
 i = -1;
 if(bug>3) printf("%d %d %d \n",ixt,want.nfiles,want.times[ixt]); /* DEBUG */
 for (j=0; j<want.nfiles; j++)
  {if (want.times[ixt] == want.fcst_hr[j]) {i++; var_sort[i] = j;}
  }
 n_var_sort = i + 1;
 if (bug > 3) printf("n_var_sort %d\n",n_var_sort);

 for (ixv=1; ixv<want.nvars; ixv++)
  {
   if (bug > 3) printf("\nsort for variable type %3d %4d\n",ixv,want.vars[ixv]);
   sort[0] = -1; lxs = -1;
   for (ixf=0; ixf<n_var_sort; ixf++)
    {lxf = var_sort[ixf];
     if (bug>3) printf("sort file %d %d\n", lxf,floc[lxf].num_grids); /*DEBUG*/
     fflush(NULL);  	/* DEBUG */
     for (ixg=0; ixg<floc[lxf].num_grids; ixg++)
      {
       if (floc[lxf].type[ixg] != want.vars[ixv]) continue;
       lxs++;
       for (i=0; i<lxs; i++)
        {if (floc[lxf].level[ixg] <= sort[i]) continue;
         for (j=lxs; j>i; j--)
          {sort[j] = sort[j-1];
           vs[ixv].loc[j] = vs[ixv].loc[j-1];
           vs[ixv].len[j] = vs[ixv].len[j-1];
           vs[ixv].lev[j] = vs[ixv].lev[j-1];
           vs[ixv].fil[j] = vs[ixv].fil[j-1];
          }
         break;
        }
       sort[i] = floc[lxf].level[ixg];
       vs[ixv].loc[i] = floc[lxf].loc[ixg];
       vs[ixv].len[i] = floc[lxf].len[ixg];
       vs[ixv].lev[i] = floc[lxf].level[ixg];
       vs[ixv].fil[i] = lxf;
      }
    }
   if (bug>3) {for(i=0;i<lxs+1;i++) {printf("%6d\n", sort[i]);} }  /* DEBUG */
   vs[ixv].num = lxs + 1;
  }
 if (bug > 3) {
 for (ixv=0; ixv<want.nvars; ixv++)
  {printf("%2d %3d\n",ixv, vs[ixv].num);
   for (lxs=0; lxs<vs[ixv].num; lxs++)
    {printf ("%8d %6d %6d %4d\n",
      vs[ixv].loc[lxs], vs[ixv].len[lxs], vs[ixv].lev[lxs], vs[ixv].fil[lxs]);
    }
  }  }

 if(bug>3) printf("exit vsort5\n"); if(bug>3) fflush(NULL); /* debug */
 return 0;
}
