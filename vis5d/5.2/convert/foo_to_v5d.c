/* foo_to_v5d.c */

/* Vis5D version 4.2 */

/*
 * A skeleton of a program for converting your data format to the
 * v5d format.  This version only allows the cylindrical equidistant
 * map projection and linear, equally-spaced vertical coordinate system.
 * Use foo2_to_v5d.c for other map projections.
 *
 *
 * Instructions:
 *   1. You'll probably want to rename this file to something more
 *      descriptive!
 *   2. Read this file from top to bottom.
 *   3. You must know how to read the file format of your data either
 *      using C's standard I/O functions or using a library you may
 *      have for your format (we assume the former case here).
 *   4. You have to provide code to read your file's header to initialize
 *      some variables, and code to read 3-D grids from your file.  Follow
 *      the steps outlined in the program below.
 *   5. The v5d* functions do a lot of error checking to help you.
 *   6. Rename the makefile named foo_to_v5d.c.m to something which
 *      corresponds to the name in (1).
 *   7. Edit the makefile to assign PROGRAM to the name you selected in (1).
 *      Also, check the CFLAGS variable.
 */




#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "binio.h"
#include "v5d.h"
/* If you're using a library to read your file, add the #include file here */




/*
 * This is the main conversion function.  The two filename arguments are
 * obtained from the command line.
 * Input:  infile - name of input file in your format
 *         outfile - name of output file in v5d format
 */
static int convert( char *infile, char *outfile )
{
   float *g;
   FILE *f;
   int it, iv, ir, ic, il;

   /** STEP 1:  The following variables must be initialized in STEP 2.  See
    ** the README file section describing the 'v5dCreateSimple' call for more
    ** information.
    **/
   int NumTimes;                        /* number of time steps */
   int NumVars;                         /* number of variables */
   int Nr, Nc, Nl;                      /* size of 3-D grids */
   char VarName[MAXVARS][10];           /* names of variables */
   int TimeStamp[MAXTIMES];             /* real times for each time step */
   int DateStamp[MAXTIMES];             /* real dates for each time step */
   float NorthLat;                      /* latitude of north bound of box */
   float LatInc;                        /* spacing between rows in degrees */
   float WestLon;                       /* longitude of west bound of box */
   float LonInc;                        /* spacing between columns in degs */
   float BottomHgt;                     /* height of bottom of box in km */
   float HgtInc;                        /* spacing between grid levels in km */


   /**
    ** STEP 2:  Open your file and read the header information to initialize
    ** the above variables.
    **/

   /* example using stdio: */
   f = fopen( infile, "r" );
   if (!f) {
      printf("Error: couldn't open %s for reading\n", infile );
      exit(1);
   }
   /* use fscanf(), fgets(), etc. to read your header if in ASCII, else use */
   /* fread() to read binary data. */



   /** END STEP 2 **/

   /* use the v5dCreateSimple call to create the v5d file and write header */
   if (!v5dCreateSimple( outfile, NumTimes, NumVars, Nr, Nc, Nl,
                         VarName, TimeStamp, DateStamp,
                         NorthLat, LatInc, WestLon, LonInc,
                         BottomHgt, HgtInc )) {
      printf("Error: couldn't create %s\n", outfile );
      exit(1);
   }
   /* allocate space for grid data */
   g = (float *) malloc( Nr * Nc * Nl * sizeof(float) );
   if (!g) {
      printf("Error: out of memory\n");
      exit(1);
   }


   for (it=0;it<NumTimes;it++) {

      for (iv=0;iv<NumVars;iv++) {

         /**
          ** STEP 3:  Read your 3-D grid data for timestep it and variable
          ** iv into the array g here.
          ** To help with 3-D array indexing we've defined a macro G.
          ** G(0,0,0) is the north-west-bottom corner, G(Nr-1,Nc-1,Nl-1) is
          ** the south-east-top corner.  If you want a value to be considered
          ** missing, assign it equal to the constant MISSING.  For example:
          ** G(ir,ic,il) = MISSING;
          **/
#define G(ROW, COLUMN, LEVEL)   g[ (ROW) + ((COLUMN) + (LEVEL) * Nc) * Nr ]





         /** END STEP 3 **/

         /* Write data to v5d file. */
         if (!v5dWrite( it+1, iv+1, g )) {
            printf("Error while writing grid.  Disk full?\n");
            exit(1);
         }
      }

   }

   v5dClose();
   fclose(f);
}



/*
 * Main.  You shouldn't have to touch this.
 */
int main( int argc, char *argv[] )
{
   if (argc==1) {
      printf("Usage:\n");
      printf("   %s infile outfile\n", argv[0]);
   }
   else {
      printf("Input file: %s\n", argv[1] );
      printf("Output file: %s\n", argv[2] );
      convert( argv[1], argv[2] );
   }
   return 0;
}

