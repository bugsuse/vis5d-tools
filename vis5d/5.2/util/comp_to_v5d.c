/* comp_to_v5d.c */

/* Convert old COMP file to new .v5d file */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "binio.h"
#include "v5d.h"



/*
 * Global vars describing the file header.
 */
static int NumTimes, NumVars;
static int Nr, Nc, Nl, nl[MAXVARS];
static float NorthLat[MAXTIMES];
static float WestLon[MAXTIMES];
static float LatInc, LonInc;
static char VarName[MAXVARS][10];
static float MinVal[MAXVARS], MaxVal[MAXVARS];
static float Bottom, HgtInc;
static int DateStamp[MAXTIMES], TimeStamp[MAXTIMES];
static int projection;
static float proj_args[4];
static int vertical;
static float vert_args[2];



#if defined(stellar) || defined(hp) || defined(sun)
   typedef char int_1;
#else
   typedef signed char int_1;
#endif


/*
 * Read the header info in the COMP* file and put into global vars.
 */
static int read_comp_header( int f )
{
   int id, gridtimes, gridvars, i, j, ip;

   /* read id */
   read_int4( f, &id );

   if (id==0x80808080 || id==0x80808081) {
      float top;

      if (id==0x80808080) {
         gridtimes = 300;
         gridvars = 20;
      }
      else {
         gridtimes = 400;
         gridvars = 30;
      }

      read_int4( f, &NumTimes );
      read_int4( f, &NumVars );
      read_int4( f, &Nr );
      read_int4( f, &Nc );
      read_int4( f, &Nl );

      read_float4( f, &NorthLat[0] );
      read_float4( f, &WestLon[0] );
      read_float4( f, &top );
      read_float4( f, &LatInc );
      read_float4( f, &LonInc );
      read_float4( f, &HgtInc );
      Bottom = top - HgtInc * (Nl-1);
      for (i=1;i<NumTimes;i++) {
         NorthLat[i] = NorthLat[0];
         WestLon[i] = WestLon[0];
      }

      /* read dates and times */
      read_int4_array( f, DateStamp, gridtimes );
      read_int4_array( f, TimeStamp, gridtimes );

      /* read parm names */
      for (i=0;i<gridvars;i++) {
         if ( read_bytes(f, VarName[i], 4)!=4 ) {
            printf("Error:  Unable to read parameter names.\n");
            return 0;
         }
         /* remove trailing spaces, if any */
         for (j=7;j>0;j--) {
            if (VarName[i][j]==' ' || VarName[i][j]==0)
              VarName[i][j] = 0;
            else
              break;
         }
      }
   }
   else if (id==0x80808082 || id==0x80808083) {
      float height[MAXLEVELS];

      read_int4( f, &gridtimes );

      read_int4( f, &NumVars );
      read_int4( f, &NumTimes );
      read_int4( f, &Nr );
      read_int4( f, &Nc );
      read_int4( f, &Nl );
      read_float4( f, &LatInc );
      read_float4( f, &LonInc );

      read_float4_array( f, height, Nl );
      Bottom = height[0];
      HgtInc = (height[Nl-1] - height[0]) / (Nl-1);

      for (ip=0;ip<NumVars;ip++) {
         if ( read_bytes(f, VarName[ip], 8)!=8 ) {
            printf("Error:  Unable to read parameter names.\n");
            return 0;
         }
         /* remove trailing spaces, if any */
         for (j=7;j>0;j--) {
            if (VarName[ip][j]==' ' || VarName[ip][j]==0)
              VarName[ip][j] = 0;
            else
              break;
         }
      }

      read_float4_array( f, MinVal, NumVars );
      read_float4_array( f, MaxVal, NumVars );
      read_int4_array( f, TimeStamp, gridtimes );
      read_int4_array( f, DateStamp, gridtimes );
      read_float4_array( f, NorthLat, gridtimes );
      read_float4_array( f, WestLon, gridtimes );
   }

   for (i=0;i<NumTimes;i++) {
      TimeStamp[i] = v5dSecondsToHHMMSS( TimeStamp[i] );
      DateStamp[i] = v5dDaysToYYDDD( DateStamp[i] );
   }
   return id;
}



/*
 * Read the next 3-D grid from the file.
 */
static int read_grid( int f, int id, float data[] )
{
   int compsize = ((Nr*Nc*Nl+3)/4)*4;
   float ga[MAXLEVELS], gb[MAXLEVELS];
   int_1 compdata[MAXROWS*MAXCOLUMNS*MAXLEVELS];
   int p, lev, i;
   float min, max;
   int missing_count;

   if (id==0x80808080 || id==0x80808081) {
      read_float4( f, &ga[0] );
      read_float4( f, &gb[0] );
      for (i=1;i<Nl;i++) {
         ga[i] = ga[0];
         gb[i] = gb[0];
      }
   }
   else if (id==0x80808082 || id==0x80808083) {
      int mcfile, mcgrid;
      if (id==0x80808083) {
         read_int4( f, &mcfile );
         read_int4( f, &mcgrid );
      }
      read_float4_array( f, ga, Nl );
      read_float4_array( f, gb, Nl );
   }

   if (read_bytes( f, compdata, compsize )!=compsize) {
      printf("read_bytes failed\n");
   }

   min = 10000.0;
   max = -min;
   missing_count = 0;

   /* decompress */
   p = 0;
   for (lev=0;lev<Nl;lev++) {
      float ga_inv = 1.0 / ga[lev];
      float gb_val = gb[lev];
      for (i=0;i<Nr*Nc;i++) {
         if (compdata[p] == 127) {
            data[p] = MISSING;
            missing_count++;
         }
         else {
            data[p] = ( (float) (int) compdata[p] - gb_val ) * ga_inv;
            if (data[p]>max)
               max = data[p];
            if (data[p]<min)
               min = data[p];
         }
         p++;
      }
   }

   printf("min=%g max=%g missing=%d\n", min, max, missing_count );
   return 1;
}




int main( int argc, char *argv[] )
{
   int infile;
   int id;
   int compressmode, i;

   if (argc==1) {
      printf("Usage:\n");
      printf("   comp_to_v5d compfile outfile.v5d\n");
      printf("Options:\n");
      printf("   -compress N\n");
      printf("      Specifies compress to N bytes per grid point where\n");
      printf("      N = 1, 2 or 4.\n");
      exit(0);
   }

   compressmode = 1; /* default */

   for (i=2;i<argc;i++) {
      if (strcmp(argv[i],"-compress")==0 && i+1<argc) {
         compressmode = atoi( argv[i+1] );
         i++;
      }
   }

   infile = open( argv[1], O_RDONLY );
   if (infile<0) {
      printf("Error:  couldn't open %s for reading\n", argv[1] );
      exit(0);
   }

   id = read_comp_header( infile );
   if (id) {
      for (i=0; i<NumVars; i++) nl[i] = Nl;
      projection = 1;
      proj_args[0] = NorthLat[0];
      proj_args[1] = WestLon[0];
      proj_args[2] = LatInc;
      proj_args[3] = LonInc;
      vertical = 1;
      vert_args[0] = Bottom;
      vert_args[1] = HgtInc;
      if (v5dCreate( argv[2], NumTimes, NumVars, Nr, Nc, nl,
                     (const char (*)[10]) VarName, TimeStamp, DateStamp,
                     compressmode,
                     projection, proj_args, vertical, vert_args)) {
/*
      if (v5dCreateSimple( argv[2], NumTimes, NumVars, Nr, Nc, Nl,
                           VarName, TimeStamp, DateStamp,
                           NorthLat[0], LatInc,
                           WestLon[0], LonInc,
                           Bottom, HgtInc )) {
*/

         int time, var;
         float *data;

         data = (float *) malloc( Nr * Nc * Nl * sizeof(float) );

         /* Read each 3-D grid, decompress it and write to output file */
         for (time=0;time<NumTimes;time++) {
            for (var=0;var<NumVars;var++) {
               read_grid( infile, id, data );

               v5dWrite( time+1, var+1, data );
            }
         }

         v5dClose();
      }
      else {
         printf("Error:  couldn't create output file: %s\n", argv[2] );
      }
   }

   return 0;
}

