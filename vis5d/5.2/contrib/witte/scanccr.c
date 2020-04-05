/* List variables                                                            */
/* Henk J.L. Witte                                                           */
/* Hugo de Vries-Laboratory, University of Amsterdam                         */
/* Kruislaan 318, 1098 SM Amsterdam, The Netherlands                         */
/* email: henk_witte@sara.nl                                                 */
 
/*usage:                                                                     */
/* scanccr datain                                                            */
 
 
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
  
FILE *fpin;
int i;
char inStr[255],cur_step[20],cur_vlevel[10],cur_var[10],dummy[80];

main(int argc, char *argv[])
{
  if(argc!=2)
     printf("%s\n  Usage: data,\n",argv[0]);
  else {
     fpin = fopen(argv[1],"r");
     if(fpin == NULL) {
        printf("cannot open %s\n",argv[1]);
        goto err;
     }

    fgets(inStr,255,fpin);

     do {
        for(i=1;i<=4;i++)
           if(fgets(inStr,255,fpin)==NULL)
             goto err;
 
        if(fscanf(fpin,"PPF subfile %s ",cur_step)!=1)
          goto err;
        if(fscanf(fpin,"VARIABLE %s ",cur_var)!=1)
          goto err;
        if(fscanf(fpin,"PSLEVEL %s ",cur_vlevel)!=1)
          goto err;
        printf("variable [%s]\tlevel [%s]\ttimestep[%s]\n",cur_var,cur_vlevel,
           cur_step);
        do {
           if(fgets(inStr,255,fpin)==NULL)
             goto err;
        } while (sscanf(inStr,"TITLE(1) %s",dummy)!=1);
      } while ( !feof(fpin));

     fclose(fpin);
     exit(0);
 
   }
   err:
    printf("exit on error\n");
    exit(0);
}
