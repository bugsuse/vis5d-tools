/*
Difference between two CCR-MENU files
  version 8/3/1995
 Henk J.L. Witte
 Hugo de Vries-Laboratory, University of Amsterdam
 Kruislaan 318, 1098 SM Amsterdam, The Netherlands
 email: henk_witte@sara.nl
 
usage:
 diffccr mode header datain1 datain2 dataout (-i)

read_header reads a header file describing the CCR-MENU file. The order in
which vars/levels/timesteps are mentioned in the header is the order in
which they are processed.

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
     january 
     february 
     march 
     april 
     may 
     june 
     july 
     august 
     september 
     october 
     november 
     december 
*/
 
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "ccrutil.h"
 
#define MAXLEVELS 10
#define MAXVARS 100
#define MAXTIMES 12
 
FILE *fpin1,*fpin2,*fpout;
int i,Nr,Nc,NumVars,NumLev,NumTimes,Nl[MAXVARS],ires,
    west_bound,east_bound,north_bound,south_bound;
int flag_i;
char VarName[MAXVARS][10],LevName[MAXLEVELS][10],TStepName[MAXTIMES][20];
char inStr[255];
 
int difference(FILE *fpin1, FILE *fpin2,FILE *fpout, int NumVars, int NumLev,
   int NumTimes, int Nl[],int Nr, int Nc,char VarName[][10],char LevName[][10],
   char TStepName[][20], int east_bound, int west_bound, int north_bound,
   int south_bound,int flag_i)

/* Calculate difference between fields: data1 - data2 */
{
 
   int it,iv,ir,ic,il,skip,val_cnt;
   float val,val2;
   fpos_t fpin_pos,fpin_pos2;
   char inStr[255];
 
   rewind(fpin1);
   rewind(fpin2);
   fgetpos(fpin1,&fpin_pos);
   fgetpos(fpin2,&fpin_pos2);

   for(it=0;it<NumTimes;it++) {                         /* for all timesteps */
      fsetpos(fpin1,&fpin_pos);              /* reset to start last timestep */
      fsetpos(fpin2,&fpin_pos2);             /* reset to start last timestep */
      fpin1 = find_TS(fpin1,TStepName[it]);
      fpin2 = find_TS(fpin2,TStepName[it]);

                                        /* find current timestep in datafile */
      if ((fpin1 == NULL)||(fpin2 == NULL))
         goto err;
      fgetpos(fpin1,&fpin_pos);                  /* start of current timestep */
      fgetpos(fpin2,&fpin_pos2);                 /* start of current timestep */

 
      for(iv=0;iv<NumVars;iv++) {                       /* for all variables */
         for (il=0;il<Nl[iv] ;il++ ) {                     /* for all levels */
           fsetpos(fpin1,&fpin_pos);      /* reset to start current timestep */
           fsetpos(fpin2,&fpin_pos2);      /* reset to start current timestep */
 
           if(flag_i == 1)
             printf("[%s][%s][%s]\n",TStepName[it],VarName[iv],LevName[il]);
           fpin1 = find_VL(fpin1,VarName[iv],LevName[il],TStepName[it]);
           fpin2 = find_VL(fpin2,VarName[iv],LevName[il],TStepName[it]);

                                            /* find current variable & level */
      if ((fpin1 == NULL)||(fpin2 == NULL))
         goto err;
 
           for(skip=1;skip<=14;skip++) {                   /* process header */
              fgets(inStr,255,fpin1);
              fgets(inStr,255,fpin2);
              fprintf(fpout,"%s",inStr);
           }
           val_cnt = 0;
           for(ir=1;ir<=Nr;ir++) {
              for(ic=1;ic<=Nc;ic++) {
                 fscanf(fpin1," %f",&val);
                 fscanf(fpin2," %f",&val2);
                 if((ir <= south_bound)&&(ir >= north_bound)&&
                   (ic >= west_bound)&&(ic <= east_bound)) {
                    val_cnt++;
                    if (val_cnt >= 8) {
                       fprintf(fpout,"%.6e\n",val-val2);
                       val_cnt = 0;
                    }
                    else
                       fprintf(fpout,"%.6e ",val-val2);
                 }
              }
           }
           if (val_cnt != 0) {
              fprintf(fpout,"\n");
              val_cnt = 0;
           }
         } /* endfor levels*/
      } /* endfor variables*/
   } /* endfor timesteps*/
 
   return 0;
 
   err:
      return -1;
}
 
int difference2(FILE *fpin1,FILE *fpin2, FILE *fpout, int NumVars, int NumLev,
   int NumTimes, int Nl[],int Nr, int Nc,char VarName[][10],char LevName[][10],
   char TStepName[][20], int east_bound, int west_bound, int north_bound,
   int south_bound,int flag_i)

/* Calculate difference between fields: data1 - data2 */
{
 
   int it,iv,ir,ic,il,skip,val_cnt;
   float val,val2;
   char inStr[255];
 
   rewind(fpin1);
   rewind(fpin2);
 
   for(it=0;it<NumTimes;it++) {                         /* for all timesteps */
      for(iv=0;iv<NumVars;iv++) {                       /* for all variables */
         for (il=0;il<Nl[iv] ;il++ ) {                     /* for all levels */
           if(flag_i == 1)
             printf("[%s][%s][%s]\n",TStepName[it],VarName[iv],LevName[il]);
           fpin1 = find_TSVL(fpin1,TStepName[it],VarName[iv],LevName[il]);
           fpin2 = find_TSVL(fpin2,TStepName[it],VarName[iv],LevName[il]);
                                            /* find current variable & level */
           if ((fpin1 == NULL)||(fpin2 == NULL))
              goto err;
 
           for(skip=1;skip<=14;skip++) {                   /* process header */
              fgets(inStr,255,fpin1);
              fgets(inStr,255,fpin2);
              fprintf(fpout,"%s",inStr);
           }
           val_cnt = 0;
           for(ir=1;ir<=Nr;ir++) {
              for(ic=1;ic<=Nc;ic++) {
                 fscanf(fpin1," %f",&val);
                 fscanf(fpin2," %f",&val2);
                 if((ir <= south_bound)&&(ir >= north_bound)&&
                   (ic >= west_bound)&&(ic <= east_bound)) {
                    val_cnt++;
                    if (val_cnt >= 8) {
                       fprintf(fpout,"%.6e\n",val-val2);
                       val_cnt = 0;
                    }
                    else
                       fprintf(fpout,"%.6e ",val-val2);
                 }
              }
           }
           if (val_cnt != 0) {
              fprintf(fpout,"\n");
              val_cnt = 0;
           }
         } /* endfor levels*/
      } /* endfor variables*/
   } /* endfor timesteps*/

   return 0;
 
   err:
      return -1;
}
 
main(int argc, char *argv[])
{
  if (argc!=6 && argc!=7) {
     printf("%s\n  Usage: mode, header, data1, data2, output (-i)\n",
       argv[0]);
     printf("mode = 1: faster, timestep ordered data only\n");
  }
  else {
     flag_i = 1;
     if (argc==7 && strcmp(argv[6],"-i")==0)
        flag_i = -1;
     fpin1 = fopen(argv[2],"r");
     if(fpin1 == NULL) {
        printf("cannot open %s\n",argv[2]);
        goto error;
     }
     if(read_header(fpin1,&NumVars,&NumLev,&NumTimes,Nl,&Nr,&Nc,VarName,LevName,
        TStepName)!=0)
       goto error;

     printf("#variables [%i]\n#maxlevs[%i]\n#tsteps[%i]\n\n",NumVars,NumLev,
       NumTimes);

     printf("select area:\n");
     printf("west east north south (default:[1][%i][1][%i])\n",Nr,Nc);
     gets(inStr);
     ires = sscanf(inStr,"%i%i%i%i",&west_bound,&east_bound,&north_bound,
                 &south_bound);
 
     if (ires != 4) {
       north_bound = 1;
       south_bound = Nr;
       west_bound = 1;
       east_bound = Nc;
     }
     printf("selected: %i %i %i %i\n",west_bound,east_bound,north_bound,
        south_bound);
 
     printf("for header: Nr = [%i] Nc = [%i]\n",east_bound-west_bound+1,
       south_bound-north_bound+1);

     fclose(fpin1);
     fpin1 = fopen(argv[3],"r");
     if(fpin1 == NULL) {
        printf("cannot open %s\n",argv[3]);
        goto error;
     }
     fpin2 = fopen(argv[4],"r");
     if(fpin2 == NULL) {
        printf("cannot open %s\n",argv[4]);
        goto error;
     }
     fpout = fopen(argv[5],"w");
     if(fpout == NULL) {
        printf("cannot open %s\n",argv[5]);
        goto error;
     }
     if (strcmp(argv[1],"1")==0) {
        if(difference(fpin1,fpin2,fpout,NumVars,NumLev,NumTimes,Nl,Nr,Nc,
           VarName,LevName,TStepName,east_bound,west_bound,north_bound,
           south_bound,flag_i)!=0)
           goto error;
     }
     else {
        if(difference2(fpin1,fpin2,fpout,NumVars,NumLev,NumTimes,Nl,Nr,Nc,
           VarName,LevName,TStepName,east_bound,west_bound,north_bound,
           south_bound,flag_i)!=0)
           goto error;
     }
     fclose(fpout);
  }
 
  error:
    exit(0);
}
