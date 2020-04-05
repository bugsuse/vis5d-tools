/*
CCRUTIL.H: common routines for ccr-utility programs

 Henk J.L. Witte
 Hugo de Vries-Laboratory, University of Amsterdam
 Kruislaan 318, 1098 SM Amsterdam, The Netherlands
 email: henk_witte@sara.nl
 
read_header: read header file describing CCR-MENU datafile content
find_TS: find requested timestep in CCR-MENU datafile
find_VL: find requested vraiable and level of current timestep in CCR-MENU datafile
find_TSVL: find timestep, variable and level in CCR-MENU datafile
*/

#ifdef WITHDATE
int read_header(FILE *fpin, int *NumVars, int *NumLev,int *NumTimes,int Nl[],
   int *Nr,int *Nc,char VarName[][10],char LevName[][10], char TStepName[][20],
   int DateStamp[])
#else
int read_header(FILE *fpin, int *NumVars, int *NumLev,int *NumTimes,int Nl[],
   int *Nr,int *Nc,char VarName[][10],char LevName[][10], char TStepName[][20])
#endif

/* Read the header describing CCR-MENU file   */
 
{
  int i,j;
#ifdef WITHDATE
#else
  int skip;
#endif
  char head_txt[30];
 
  if( fscanf(fpin,"%s",head_txt)== 0)
    goto err;
 
  if(strcmp(head_txt,"#to_vis5d")!=0)
     printf("header error\n");
  if( fscanf(fpin,"%i %i %i %i %i",NumVars,NumLev,NumTimes,Nr,Nc) != 5)
    goto err;
 
  for(i=0;i<*NumVars;i++) {
     if(fscanf(fpin," %s %i",VarName[i],&Nl[i])!=2)
        goto err;
  }
  for(i=0;i<*NumLev;i++) {
    if(fscanf(fpin," %s",LevName[i])!=1)
       goto err;
  }
  for(i=0;i<*NumTimes;i++) {
#ifdef WITHDATE
    if(fscanf(fpin," %s %i",TStepName[i],&DateStamp[i])!=2)
#else
    if(fscanf(fpin," %s %i",TStepName[i],&skip)!=2)
#endif
       goto err;
  }
 
  return 0;
 
  err:
     printf("error reading header file\n");
     return -1;
}
 
FILE *find_TS(FILE *fpin,char seek_step[20])
/* find specified timestep */
{
   int i;
   char inStr[255],cur_step[20],dummy[80];
   fpos_t the_pos;
 
   fgetpos(fpin,&the_pos);                            /* get start datablock */
   do {
      fsetpos(fpin,&the_pos);                 /* position at start datablock */
      for(i=1;i<=5;i++)                         /* skip to season descriptor */
         if(fgets(inStr,255,fpin)==NULL)
           goto err;
      if(fscanf(fpin,"PPF subfile %s",cur_step)!=1)
         goto err;
      if(strcmp(seek_step,cur_step)!=0) {
                             /* if not season requested: skip to next header */
         do {
            fgetpos(fpin,&the_pos);              /* get start next datablock */
            if(fgets(inStr,255,fpin)==NULL)
              goto  err;
         } while (sscanf(inStr,"TITLE(1) %s",dummy)!=1);
                                            /* enddo: start next block found */
      }
   } while (strcmp(seek_step,cur_step)!=0);     /* enddo: until season found */
 
   fsetpos(fpin,&the_pos);              /* position at start block requested */
   return fpin;
 
   err:
     printf("error finding tstep [%s]\n",seek_step);
     return NULL;
}
 
FILE *find_VL(FILE *fpin,char seek_var[10],char seek_vlevel[10],
   char check_step[20])
/* find specified variable and level, used after find_TS*/
{
   int i,found_vlevel;
   char inStr[255],cur_vlevel[10],cur_var[10],cur_step[20],dummy[80];
   fpos_t the_pos;
 
   found_vlevel = 0;
   fgetpos(fpin,&the_pos);                            /* get start datablock */
   do {
      fsetpos(fpin,&the_pos);                 /* position at start datablock */
      for(i=1;i<=5;i++)                         /* skip to season descriptor */
         if(fgets(inStr,255,fpin)==NULL)
           goto err;
      if(fscanf(fpin," PPF subfile %s",cur_step)!=1)
         goto err;
      if(strcmp(check_step,cur_step)!=0) {
         printf("error: wrong timestep[%s]: ",cur_step);
         goto err;
      }
      if(fscanf(fpin," VARIABLE %s ",cur_var)!=1)
         goto err;
      if(fscanf(fpin," PSLEVEL %s ",cur_vlevel)!=1)
         goto err;
 
      /* 0.9910, 1013.000 and Surface are equal */
      if ((strcmp(seek_vlevel,"0.9910")==0)||
     (strcmp(seek_vlevel,"1013.0000")==0)||(strcmp(seek_vlevel,"Surface")==0))
         if ((strcmp(cur_vlevel,"0.9910")==0)||
        (strcmp(cur_vlevel,"1013.0000")==0)||(strcmp(cur_vlevel,"Surface")==0))
           found_vlevel = 1;
         else
            found_vlevel = 0;
      else
         found_vlevel = (strcmp(seek_vlevel,cur_vlevel)==0) ? 1 : 0;
 
      if( (!found_vlevel)||(strcmp(seek_var,cur_var)!=0)) {
                          /* if not var/level requested: skip to next header */
         do {
            fgetpos(fpin,&the_pos);              /* get start next datablock */
            if(fgets(inStr,255,fpin)==NULL)
              goto err;
         } while (sscanf(inStr,"TITLE(1) %s",dummy)!=1);
                                            /* enddo: start next block found */
      }
   } while ( (!found_vlevel)||(strcmp(seek_var,cur_var)!=0));
                                            /* enddo: variable & field found */
 
   fsetpos(fpin,&the_pos);              /* position at start block requested */
   return fpin;
 
   err:
     printf("error finding variable [%s] level [%s]\n",seek_var,seek_vlevel);
     return NULL;
}
 
FILE *find_TSVL(FILE *fpin,char seek_step[20],char seek_var[10],
   char seek_vlevel[10])
/* find specified variable and level of timestep, used on unsorted file*/
{
   int i,found_vlevel;
   char inStr[255],cur_step[20],cur_vlevel[10],cur_var[10],dummy[80];
   fpos_t the_pos;
 
   rewind(fpin);
   fgetpos(fpin,&the_pos);                            /* get start datablock */
   do {
      fsetpos(fpin,&the_pos);                 /* position at start datablock */
      for(i=1;i<=5;i++)                         /* skip to season descriptor */
         if(fgets(inStr,255,fpin)==NULL)
           goto err;
 
      if(fscanf(fpin,"PPF subfile %s ",cur_step)!=1)
         goto err;
      if(fscanf(fpin,"VARIABLE %s ",cur_var)!=1)
         goto err;
      if(fscanf(fpin,"PSLEVEL %s ",cur_vlevel)!=1)
         goto err;
 
      /* 0.9910, 1013.000 and Surface are equal */
      if ((strcmp(seek_vlevel,"0.9910")==0)||
     (strcmp(seek_vlevel,"1013.0000")==0)||(strcmp(seek_vlevel,"Surface")==0))
         if ((strcmp(cur_vlevel,"0.9910")==0)||
        (strcmp(cur_vlevel,"1013.0000")==0)||(strcmp(cur_vlevel,"Surface")==0))
           found_vlevel = 1;
         else
            found_vlevel = 0;
      else
         found_vlevel = (strcmp(seek_vlevel,cur_vlevel)==0) ? 1 : 0;
 
 
      if( (!found_vlevel)||(strcmp(seek_var,cur_var)!=0)||
         (strcmp(seek_step,cur_step)!=0)) {
                   /* if not season/var/level requested: skip to next header */
         do {
            fgetpos(fpin,&the_pos);              /* get start next datablock */
            if(fgets(inStr,255,fpin)==NULL)
              goto err;
         } while (sscanf(inStr,"TITLE(1) %s",dummy)!=1);
                                            /* enddo: start next block found */
      }
   } while ( (!found_vlevel)||(strcmp(seek_var,cur_var)!=0)
        ||(strcmp(seek_step,cur_step)!=0)); /* enddo: variable & field found */
 
   fsetpos(fpin,&the_pos);              /* position at start block requested */
   return fpin;
 
   err:
     printf("error finding timestep [%s] variable [%s] level [%s]\n",
        seek_step,seek_var,seek_vlevel);
     return NULL;
}

