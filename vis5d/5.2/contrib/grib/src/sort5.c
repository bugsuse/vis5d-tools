#include <stdio.h>
#include <math.h>

#include "want.h"

void sgdate (int *day, int *mon, int *year, long serial);
int dyofyr (int day, long imon, int year);

extern int bug;

int sort5 (void)
{
 int ixf, ixg, ixt, ixs, ixv, i,j;	/* indices */
 int lxf, lxs;
 int sort[NGRID];
 int n_times;			/* number of time peroids */
 int time_file[NFILES];		/* files per time peroid */
 double dsort[NFILES];		/* for sorting serial date-times */
 double dip, dfp;		/* to get the integral & fractional part -modf*/
 long serial;			/* hold the serial date as long */
 int day, mon, year;		/* output from sgdate */

 if(bug>3) printf("in sort5\n"); if(bug>3) fflush(NULL); /* debug */
 if (bug > 3) {
   printf("number of files %3d\n",want.nfiles);
   for (ixf=0; ixf<want.nfiles; ixf++)
    {printf("%2d %12.4f %12.4f\n",
       ixf,floc[ixf].start_date,floc[ixf].valid_time);}
   for (ixf=0; ixf<want.nfiles; ixf++)
    {printf("number of grids %3d\n", floc[ixf].num_grids);
     if (bug > 3)
      {for (ixg=0; ixg<floc[ixf].num_grids; ixg++)
       {printf(" %8d %5d %3d %5d\n",
              floc[ixf].loc[ixg], floc[ixf].len[ixg],
              floc[ixf].type[ixg], floc[ixf].level[ixg]); } }
    }
   for (ixt=0; ixt<want.nfiles; ixt++)
    {printf("%2d ",want.fcst_hr[ixt]);}
   printf("\n");
   fflush (NULL);		/* DEBUG */
 }

 /* Sort on serial date times. */
 ixs = -1;
 for (ixf=0; ixf<want.nfiles; ixf++)
  {
   if (floc[ixf].valid_time <  1.0) break;	/* file not used */
   for (i=0; i<=ixs; i++)
    {if (floc[ixf].valid_time >  dsort[i]) continue;
     if (floc[ixf].valid_time == dsort[i]) goto outer_loop;
     for (j=ixs+1; j>i; j--) {dsort[j] = dsort[j-1];
                             want.times[j] = want.times[j-1];}
     break;
    }
   if (ixs >= TIMES-1)
    {printf("sort5: index for want.times exceeds TIMES %d\n",TIMES); break;}
   ixs++;
   dsort[i] = floc[ixf].valid_time;
   want.times[i] =  want.fcst_hr[ixf];
   outer_loop: ;
  }
 if (ixs < 0) {printf("There are no valid input files\n"); return -1;}
 n_times = ixs + 1;
 want.ntimes = n_times;
 if (bug > 3) {
  printf("\nthese are the sorted valid times;\n");
  for (i=0; i<n_times; i++) {printf("%12.4f\n",dsort[i]);} printf("\n");
  printf("these are the forecast hours;\n");
  for (i=0; i<n_times; i++) {printf("%2d ",want.times[i]);} printf("\n"); }
 for (ixt=0; ixt<n_times; ixt++)
  {i = -1;
   for (j=0; j<want.nfiles; j++)
    {if (dsort[ixt] == floc[j].valid_time)
      {
       if (i > NFILES-2) {printf("NFILES EXCEEDED %d\n",ixt); break;}
       i++;
       ts[ixt].files[i] = j;}
    }
   time_file[ixt] = i + 1;
   ts[ixt].num = i + 1;
  }
 if (bug > 1) {printf("    serial date&\n"
                      "    valid time   yyddd hhmmss file numbers\n");}
 for (ixt=0; ixt<n_times; ixt++)
  {
   dfp = modf(dsort[ixt], &dip);
   serial = (long)dsort[ixt];
   serial = (long)dip;
   sgdate (&day, &mon, &year, serial);
   ts[ixt].date = dyofyr (day, mon, year);
   ts[ixt].time = (long)(dfp * 24.0) * 10000;
   ts[ixt].serial = serial;
   ts[ixt].valid_time = dsort[ixt];;
   if (bug > 1) {
    printf ("%2d  ",ixt);
    printf ("%12.4f ", ts[ixt].valid_time);
    printf ("%5d %6d ", ts[ixt].date, ts[ixt].time);
    for(i=0;i<ts[ixt].num;i++){printf("%2d",ts[ixt].files[i]);}printf("\n");}
  }
 /* End of serial date sort */

 ixv = 0;	/* sort on pressure surfaces of heights */
 if (bug > 3) printf("\nsort for type %3d\n",ixv);
 for (ixt=0; ixt<n_times; ixt++)
  {
   if (bug > 3) printf("%2d %2d\n",ixt,want.times[ixt]);
   sort[0] = -1; lxs = -1;
   for (ixf=0; ixf<time_file[ixt]; ixf++)
    {lxf = ts[ixt].files[ixf];
     for (ixg=0; ixg<floc[lxf].num_grids; ixg++)
      {
       if (floc[lxf].type[ixg] != want.vars[ixv]) continue;
       if (lxs > NGRID-2)
        {printf("sort5: NGRID exceeded\n"); break;}
       lxs++;
       for (i=0; i<lxs; i++)
        {if (floc[lxf].level[ixg] <= sort[i]) continue;
         for (j=lxs; j>i; j--)
          {sort[j] = sort[j-1];
           ts[ixt].loc[j] = ts[ixt].loc[j-1];
           ts[ixt].len[j] = ts[ixt].len[j-1];
           ts[ixt].lev[j] = ts[ixt].lev[j-1];
           ts[ixt].fil[j] = ts[ixt].fil[j-1];
          }
         break;
        }
       sort[i] = floc[lxf].level[ixg];
       ts[ixt].loc[i] = floc[lxf].loc[ixg];
       ts[ixt].len[i] = floc[lxf].len[ixg];
       ts[ixt].lev[i] = floc[lxf].level[ixg];
       ts[ixt].fil[i] = lxf;
      }
    }
   if(bug>3) {for(i=0;i<lxs+1;i++) {printf("%6d\n",sort[i]);}} /* DEBUG */
   ts[ixt].num = lxs + 1;
  }
 ts_num = n_times;
 if (bug > 3) {
  for (ixt=0; ixt<n_times; ixt++)
   {printf("%2d %2d %3d\n",ixt,want.times[ixt], ts[ixt].num);
    for (lxs=0; lxs<ts[ixt].num; lxs++)
     {printf ("%8d %6d %6d %4d\n",
       ts[ixt].loc[lxs], ts[ixt].len[lxs], ts[ixt].lev[lxs], ts[ixt].fil[lxs]);
     }
   }  }

 if (bug > 3) printf("exit sort5\n"); if (bug > 3) fflush(NULL); /* debug */
 return 0;
}
