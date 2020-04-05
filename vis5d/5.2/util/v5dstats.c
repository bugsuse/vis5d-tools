/* v5dstats.c */


/*
 * Print statistics about data in a .v5d file.
 */


#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "v5d.h"



int main( int argc, char *argv[] )
{
   v5dstruct v;
   int time, var;

   if (argc!=2) {
      printf("Usage:\n");
      printf("   v5dstats file\n");
      exit(0);
   }

   if (!v5dOpenFile( argv[1], &v )) {
      printf("Error: couldn't open %s for reading\n", argv[1] );
      exit(0);
   }

   printf("Time  Variable Units      MinValue     MaxValue    MeanValue      Std Dev #Miss\n");
   printf("-------------------------------------------------------------------------------\n");
   
   for (time=0; time<v.NumTimes; time++) {

      for (var=0; var<v.NumVars; var++) {
         int i, nrncnl;
         float *data;
         float min, max, sum, sumsum;
         int missing, good;

         nrncnl = v.Nr * v.Nc * v.Nl[var];

         data = (float *) malloc( nrncnl * sizeof(float) );
         if (!v5dReadGrid( &v, time, var, data )) {
            printf("Error while reading grid (time=%d,var=%s)\n",
                   time+1, v.VarName[var] );
            exit(0);
         }

         min = MISSING;
         max = -MISSING;
         missing = 0;
         good = 0;
         sum = 0.0;
         sumsum = 0.0;

         for (i=0;i<nrncnl;i++) {
/*
            if (data[i]!=data[i]) {
               printf("bad: %g\n", data[i]);
            }
*/
            if ( IS_MISSING(data[i]) ) {
               missing++;
            }
            else {
               good++;
               if (data[i]<min) {
                  min = data[i];
               }
               if (data[i]>max) {
                  max = data[i];
               }
               sum += data[i];
               sumsum += data[i]*data[i];
            }
         }

         free( data );

         if (good==0) {
            /* all missing */
            printf("%4d  %-8s %-5s  all missing values\n",
                   time+1, v.VarName[var], v.Units[var] );
         }
         else {
            float mean = sum / good;
            float tmp = (sumsum - sum*sum/good) / (good-1);
            float sd;
            if (tmp<0.0) {
               sd = 0.0;
            }
            else {
               sd = sqrt( tmp );
            }
            printf("%4d  %-8s %-5s %13g%13g%13g%13g  %4d\n",
                   time+1, v.VarName[var], v.Units[var],
                   min,  max,  mean, sd,  missing );
         }

      }
      printf("\n");
   }

   v5dCloseFile( &v );
   return 0;
}
