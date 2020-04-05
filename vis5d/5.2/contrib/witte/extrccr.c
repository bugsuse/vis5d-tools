/*
 Area extraction of CCR-MENU output
  version 8/3/1995
 Henk J.L. Witte
 Hugo de Vries-Laboratory, University of Amsterdam
 Kruislaan 318, 1098 SM Amsterdam, The Netherlands
 email: henk_witte@sara.nl
 
usage:
 extrccr mode header datain dataout (-i)

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
 
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "ccrutil.h"

#define MAXLEVELS 10
#define MAXVARS 100
#define MAXTIMES 12
 
FILE *fpin,*fpout;
int i,Nr,Nc,NumVars,NumLev,NumTimes,Nl[MAXVARS],ires,
    west_bound,east_bound,north_bound,south_bound;
int flag_i;
char VarName[MAXVARS][10],LevName[MAXLEVELS][10],TStepName[MAXTIMES][20];
char inStr[255];
 
int extract(FILE *fpin, FILE *fpout, int NumVars, int NumLev,int NumTimes,
   int Nl[],int Nr, int Nc,char VarName[][10],char LevName[][10],
   char TStepName[][20], int east_bound, int west_bound, int north_bound,
   int south_bound,int flag_i)

/* Read the data and extract the area  */
{
 
   int it,iv,ir,ic,il,skip,val_cnt;
   float val;
   fpos_t fpin_pos;
   char inStr[255];
 
   rewind(fpin);
   fgetpos(fpin,&fpin_pos);
 
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
           if(flag_i == 1)
             printf("[%s][%s][%s]\n",TStepName[it],VarName[iv],LevName[il]);
                                            /* find current variable & level */
           if (fpin == NULL)
              goto err;
 
           for(skip=1;skip<=14;skip++) {                   /* process header */
              fgets(inStr,255,fpin);
              fprintf(fpout,"%s",inStr);
           }
           val_cnt = 0;
           for(ir=1;ir<=Nr;ir++) {
              for(ic=1;ic<=Nc;ic++) {
                 fscanf(fpin," %f",&val);
                 if((ir <= south_bound)&&(ir >= north_bound)&&
                   (ic >= west_bound)&&(ic <= east_bound)) {
                    val_cnt++;
                    if (val_cnt >= 8) {
                       fprintf(fpout,"%.6e\n",val);
                       val_cnt = 0;
                    }
                    else
                       fprintf(fpout,"%.6e ",val);
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
 
int extract2(FILE *fpin, FILE *fpout, int NumVars, int NumLev,int NumTimes,
   int Nl[],int Nr, int Nc,char VarName[][10],char LevName[][10],
   char TStepName[][20], int east_bound, int west_bound, int north_bound,
   int south_bound,int flag_i)

/* Read the data and extract the area  */
{
 
   int it,iv,ir,ic,il,skip,val_cnt;
   float val;
   char inStr[255];
 
   rewind(fpin);
 
 
   for(it=0;it<NumTimes;it++) {                         /* for all timesteps */
      for(iv=0;iv<NumVars;iv++) {                       /* for all variables */
         for (il=0;il<Nl[iv] ;il++ ) {                     /* for all levels */
           if(flag_i == 1)
             printf("[%s][%s][%s]\n",TStepName[it],VarName[iv],LevName[il]);
           fpin = find_TSVL(fpin,TStepName[it],VarName[iv],LevName[il]);
                                            /* find current variable & level */
           if (fpin == NULL)
              goto err;
 
           for(skip=1;skip<=14;skip++) {                   /* process header */
              fgets(inStr,255,fpin);
              fprintf(fpout,"%s",inStr);
           }
           val_cnt = 0;
           for(ir=1;ir<=Nr;ir++) {
              for(ic=1;ic<=Nc;ic++) {
                 fscanf(fpin," %f",&val);
                 if((ir <= south_bound)&&(ir >= north_bound)&&
                   (ic >= west_bound)&&(ic <= east_bound)) {
                    val_cnt++;
                    if (val_cnt >= 8) {
                       fprintf(fpout,"%.6e\n",val);
                       val_cnt = 0;
                    }
                    else
                       fprintf(fpout,"%.6e ",val);
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
  if (argc!=5 && argc!=6) {
     printf("%s\n  Usage: mode, header, data, output (-i)\n",
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
 
     printf("for header: Nr = [%i] Nc = [%i]\n",south_bound-north_bound+1,
       east_bound-west_bound+1);

     fclose(fpin);
     fpin = fopen(argv[3],"r");
     if(fpin == NULL) {
        printf("cannot open %s\n",argv[3]);
        goto error;
     }
     fpout = fopen(argv[4],"w");
     if(fpout == NULL) {
        printf("cannot open %s\n",argv[4]);
        goto error;
     }
     if (strcmp(argv[1],"1")==0) {
        if(extract(fpin,fpout,NumVars,NumLev,NumTimes,Nl,Nr,Nc,VarName,LevName,
           TStepName,east_bound,west_bound,north_bound,south_bound,flag_i)!=0)
           goto error;
     }
     else {
        if(extract2(fpin,fpout,NumVars,NumLev,NumTimes,Nl,Nr,Nc,VarName,LevName,
           TStepName,east_bound,west_bound,north_bound,south_bound,flag_i)!=0)
           goto error;
     }
     fclose(fpout);
  }
 
  error:
    exit(0);
}
