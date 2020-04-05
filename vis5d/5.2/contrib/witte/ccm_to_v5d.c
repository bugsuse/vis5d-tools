/*
Conversion of CCR-MENU output (of CCM model) to VIS5D file.
  version 8/3/1995
 Henk J.L. Witte
 Hugo de Vries-Laboratory, University of Amsterdam
 Kruislaan 318, 1098 SM Amsterdam, The Netherlands
 email: henk_witte@sara.nl
 
usage:
 ccm_to_v5d mode header datain dataout

read_header reads a header file describing the CCR-MENU file. The order in
which vars/levels/timesteps are mentioned in the header is the order in
which they are written to vis5d. So it is advisable to put the levels and
timesteps in increasing order.

Note that if you omit variables or timesteps or levels (and decrement the
number of levels for the variables) from the header file they will be
skipped when mode != 1 (mode = 1 may result in an error) and this is a
way to delete variables/timesteps from a file.

The header file has the following format:
 
     #to_vis5d
     number of variables, maximum number of levels, number of timesteps 
     number of rows, number of columns
     variable name[1] number of levels var. [1]
     variable name[2] number of levels var. [2]

     variable name[n] number of levels var. [n]
     level name[1]
     level name[2]


     level name[n]
     timestep name[1] value for timestep[1]
     timestep name[2] value for timestep[2]

     timestep name[n] value for timestep[n]

names should correspond EXACTLY to names in the CCR-MENU file, as they
are used to search the datafile for the correct timestep/variable/level!

     example:
     #to_vis5d
     4 4 12 40 48
     T 4
     T(1) 1
     T(2) 1
     T0 1
     0.9910
     850.0000
     500.0000
     250.0000
     january 95001
     february 95032
     march 95060
     april 95091
     may 95121
     june 95152
     july 95182
     august 95213
     september 95244
     october 95274
     november 95304
     december 95334
*/

#define WITHDATE

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "binio.h"
#include "v5d.h"
#include "ccrutil.h"
 
/* help with 3D array indexing: */
#define OFFST_NR (south_bound - north_bound +1)
#define OFFST_NC (east_bound - west_bound +1)
#define G(ROW,COLUMN,LEVEL) g[(ROW)+((COLUMN)+(LEVEL)*OFFST_NC)*OFFST_NR]
 
FILE *fpin,*fpout;
int i,Nr,Nc,NumVars,NumLev,NumTimes,Nl[MAXVARS],TimeStamp[MAXTIMES],
    DateStamp[MAXTIMES],CompressMode,Projection,Vertical,ires,
    west_bound,east_bound,north_bound,south_bound;
int flag_i;
float ProjArgs[100],VertArgs[100];
char VarName[MAXVARS][10],LevName[MAXLEVELS][10],TStepName[MAXTIMES][20];
char inStr[255];
 
int to_v5d(FILE *fpin, int NumVars, int NumLev,int NumTimes,int Nl[],int Nr,
   int Nc,char VarName[][10],char LevName[][10],char TStepName[][20],
   int east_bound, int west_bound, int north_bound, int south_bound,int flag_i)

/* Read the data and convert to Vis5D format (data sorted by timestep)*/
{
 
   int it,iv,ir,ic,il,skip;
   float val,*g;
   fpos_t fpin_pos;
   char inStr[255];
 
   rewind(fpin);
   fgetpos(fpin,&fpin_pos);
 
   g = (float *)malloc((south_bound - north_bound + 1)*
       (east_bound - west_bound + 1)*NumLev* sizeof(float));
   if(!g) {
      printf("out of memory\n");
      goto err;
   }
   for(it=0;it<NumTimes;it++) {                         /* for all timesteps */
      fsetpos(fpin,&fpin_pos);               /* reset to start last timestep */
      fpin = find_TS(fpin,TStepName[it]);
                                        /* find current timestep in datafile */
      if (fpin == NULL)
         goto err;
      fgetpos(fpin,&fpin_pos);                  /* start of current timestep */
 
      for(iv=0;iv<NumVars;iv++) {                       /* for all variables */
         for (il=0;il<Nl[iv] ;il++ ) {                     /* for all levels */
           fsetpos(fpin,&fpin_pos);       /* reset to start current timestep */
 
           fpin = find_VL(fpin,VarName[iv],LevName[il],TStepName[it]);
                                            /* find current variable & level */
           if (fpin == NULL)
              goto err;
 
           for(skip=1;skip<=14;skip++)               /* skip to end of data */
              fgets(inStr,255,fpin);
           for(ir=1;ir<=Nr;ir++)
              for(ic=1;ic<=Nc;ic++) {
                 fscanf(fpin," %f",&val);
                 if((ir <= south_bound)&&(ir >= north_bound)&&
                   (ic >= west_bound)&&(ic <= east_bound))
                    G(ir-north_bound,ic-west_bound,il) = val;
                                         /* macro G indexes array g[r,c,l] */
              }
           if(flag_i == 1)
              printf("processing: tstep[%s]\tvar[%s]\tlevel[%s]\n",
                 TStepName[it],VarName[iv],LevName[il]);
 
         } /* endfor levels*/
 
         if(!v5dWrite(it+1,iv+1,g)){               /*Write data to v5dfile */
            printf("error writing data to disk: disk full?\n");
            goto err;
         }
 
      } /* endfor variables*/
   } /* endfor timesteps*/
 
   v5dClose();
   return 0;
 
   err:
      return -1;
}
 
int to_v5d2(FILE *fpin, int NumVars, int NumLev,int NumTimes,int Nl[],int Nr,
   int Nc,char VarName[][10],char LevName[][10],char TStepName[][20],
   int east_bound, int west_bound, int north_bound, int south_bound,int flag_i)

/* Read the data and convert to Vis5D format (data order independent)*/
{
 
   int it,iv,ir,ic,il,skip;
   float val,*g;
   fpos_t fpin_pos;
   char inStr[255];
 
   rewind(fpin);
   fgetpos(fpin,&fpin_pos);
 
   g = (float *)malloc((south_bound - north_bound + 1)*
       (east_bound - west_bound + 1)*NumLev* sizeof(float));
   if(!g) {
      printf("out of memory\n");
      goto err;
   }
 
   for(it=0;it<NumTimes;it++) {                         /* for all timesteps */
      for(iv=0;iv<NumVars;iv++) {                       /* for all variables */
         for (il=0;il<Nl[iv] ;il++ ) {                     /* for all levels */
           fpin = find_TSVL(fpin,TStepName[it],VarName[iv],LevName[il]);
                                            /* find current variable & level */
           if (fpin == NULL)
              goto err;
 
           for(skip=1;skip<=14;skip++)                /* skip to end of data */
              fgets(inStr,255,fpin);
 
           for(ir=1;ir<=Nr;ir++)
              for(ic=1;ic<=Nc;ic++) {
                 fscanf(fpin," %f",&val);
                 if((ir <= south_bound)&&(ir >= north_bound)&&
                   (ic >= west_bound)&&(ic <= east_bound)) 
                    G(ir-north_bound,ic-west_bound,il) = val;
                                           /* macro G indexes array g[r,c,l] */
              }
 
           if(flag_i == 1)
              printf("processing: tstep[%s]\tvar[%s]\tlevel[%s]\n",
                 TStepName[it],VarName[iv],LevName[il]);
 
         } /* endfor levels*/
 
         if(!v5dWrite(it+1,iv+1,g)){                  /*Write data to v5dfile */
            printf("error writing data to disk: disk full?\n");
            goto err;
         }
 
      } /* endfor variables*/
   } /* endfor timesteps*/
 
   v5dClose();
   return 0;
 
   err:
      return -1;
}
 
main(int argc, char *argv[])
{
  if (argc!=5 && argc!=6) {
     printf("%s\n  Usage: mode, header, data,output (-i)\n",
       argv[0]);
     printf("mode = 1: faster, timestep ordered data only\n");
  }
  else {
     flag_i = 1;
     if (argc==6 && strcmp(argv[5],"-i")==0)
        flag_i = -1;
     fpin = fopen(argv[2],"r");
     if(fpin == NULL) {
        printf("cannot open %s\n",argv[2]);
        goto error;
     }
     if(read_header(fpin,&NumVars,&NumLev,&NumTimes,Nl,&Nr,&Nc,VarName,LevName,
        TStepName,DateStamp)!=0)
       goto error;

      printf("#variables [%i]\n#maxlevs[%i]\n#tsteps[%i]\n\n",NumVars,NumLev,
       NumTimes);

     printf("Set northern and western limit of box:\n");
     printf("(note: McIdas convention sets western limit at +180)\n");
     printf("North, West (default:[90][180])\n");
     gets(inStr);
     ires = sscanf(inStr,"%f%f",&ProjArgs[0],&ProjArgs[1]);

     if (ires != 2) {
       ProjArgs[0] = 90;
       ProjArgs[1] = 180;  /* West Vis5D = 180 instead of usual -180 */
     }

     printf("Set Row and column increments in degrees:\n");
     printf("Rowincrement, Colincrement (default:[4.5][7.5])\n");
     gets(inStr);
     ires = sscanf(inStr,"%f%f",&ProjArgs[2],&ProjArgs[3]);

     if (ires != 2) {
       ProjArgs[2] = 4.5;
       ProjArgs[3] = 7.5;
     }

     printf("select area:\n");
     printf("west east north south (default:[1][%i][1][%i])\n",Nc,Nr);
     gets(inStr);
     ires = sscanf(inStr,"%i%i%i%i",&west_bound,&east_bound,&north_bound,
                 &south_bound);

     if (ires != 4) {
       north_bound = 1;
       south_bound = Nr;
       west_bound = 1;
       east_bound = Nc;
     }
     if((north_bound < 1)||(north_bound > Nr)) {
        printf("error in area selection\n");
        goto error;
     }
     if((south_bound <= north_bound)||(south_bound > Nr)) {
        printf("error in area selection\n");
        goto error;
     }
     if((west_bound < 1)||(west_bound > Nc)) {
        printf("error in area selection\n");
        goto error;
     }
     if((east_bound <= west_bound)||(east_bound > Nc)) {
        printf("error in area selection\n");
        goto error;
     }

     printf("selected: %i %i %i %i\n",west_bound,east_bound,north_bound,
        south_bound);

     /* set up v5d header info, and call v5dCreate */
     /* set up CompressMode, Projection, ProjArgs, Vertical & VertArgs,  */
     /* Timestamps are set aribtrarily as CCR-MENU returns months*/
     /* or seasons only */
 
     CompressMode = 1;                                   /* bytes per grid */
 
     /* Projection: rectlinear, cilindrical equidistant */
     Projection = 1;
     ProjArgs[0] = ProjArgs[0] - ((north_bound-1) * ProjArgs[2]);
                                                   /* North Latitude Bound */
     ProjArgs[1] = ProjArgs[1] -  ((west_bound-1) * ProjArgs[3]);
                                                  /* West Longtitude Bound */
     printf("north: %f west: %f\n",ProjArgs[0],ProjArgs[1]);
     printf("south: %f east: %f\n",ProjArgs[0]- (4.5 *
       (south_bound - north_bound+1)),ProjArgs[1]- (7.5 *
       (east_bound - west_bound+1)));

     printf("\nhit return to start\n");
     gets(inStr);
 
     /* Vertical scale: unequally spaced, in km */
     Vertical = 2;
     VertArgs[0] = 0.0;                    /* Surface */
     VertArgs[1] = 1.2;                    /* 850 mb: c. 1.2 km*/
     VertArgs[2] = 5.0;                    /* 500 mb: c. 5 km*/
     VertArgs[3] = 10.0;                   /* 250 mb: c. 10 km*/
 
     for (i=0;i<=NumTimes-1 ;i++ )
        TimeStamp[i] = 120000;              /* arbitrary "timeofday" value */

     if(!v5dCreate(argv[4],NumTimes,NumVars,south_bound - north_bound + 1,
      east_bound - west_bound + 1,Nl,VarName,TimeStamp,DateStamp,
       CompressMode,Projection,ProjArgs,Vertical,VertArgs)) {
           printf("error: couldn create [%s]\n",argv[4]);
           goto error;
     }
 
     fclose(fpin);
     fpin = fopen(argv[3],"r");
     if(fpin == NULL) {
        printf("cannot open %s\n",argv[3]);
        goto error;
     }
     if (strcmp(argv[1],"1")==0) {
        if(to_v5d(fpin,NumVars,NumLev,NumTimes,Nl,Nr,Nc,VarName,LevName,
           TStepName,east_bound,west_bound,north_bound,south_bound,
           flag_i)!=0)
           goto error;
     }
     else {
        if(to_v5d2(fpin,NumVars,NumLev,NumTimes,Nl,Nr,Nc,VarName,LevName,
           TStepName,east_bound,west_bound,north_bound,south_bound,
           flag_i)!=0)
           goto error;
     }
  }
 
  error:
    exit(0);
}
