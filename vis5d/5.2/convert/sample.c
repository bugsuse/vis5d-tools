/* sample.c */

/* Vis5D version 4.2 */

/* This file is a skeleton program for converting your data file format to
   McIDAS format.  Basically, you set a number of #defines to describe the
   size of your data set, fill in some arrays, and write the code to read
   your data into a 5-D array.  The 5-D array is then written out in the
   McIDAS format for you.

   The file "sample.c.m" is a an example of a Makefile for sample.c

   Typically, you will have to edit and recompile this file whenever the
   file you are converting from has a different size, number of variables,
   or timesteps, etc than the last one you converted.  The original copy
   of this file has example values for all the #defines and arrays to
   give you an idea of what's expected.

   Also, you should probably rename this file from "sample.c" to a more
   appropriate/descriptive name such as "myformat_to_mcidas.c".

   Usage:  To run this program you must give it two arguments:
      (1) the name of the file to read (convert from).
      (2) the number of the McIDAS file to write (convert to).
   If you run the program without any arguments, it will print a usage
   summary.

*/


#include <stdio.h>
#include <math.h>
#include "binio.h"


/*** SET THE FOLLOWING CONSTANTS AND VARIABLES TO YOUR VALUES ***/


/* Optional identification string */
char identification[33] = "This is a test grid";


/* Dimensions of grid: */
#define NR 30       /* Number of rows */
#define NC 30       /* Number of columns */
#define NL 30       /* Number of levels */
#define NTIMES 10   /* Number of time steps */
#define NPARAMS 2   /* Number of physical vars or parameters */


/* Geographics boundaries of the grid domain: */
#define NORTH_LATITUDE  45.0
#define SOUTH_LATITUDE  10.0
#define WEST_LONGITUDE 120.0
#define EAST_LONGITUDE  50.0
#define BOTTOM_ALTITUDE  0.0
#define TOP_ALTITUDE    18.0


/* Each physical variable/parameter has a 4-letter name and units label: */
char  param_name[NPARAMS][5] = { "THET", "T   " };  /* theta and temperature */
char  param_unit[NPARAMS][5] = { "GPCM", "K   " };  /* g/cm3 and Kelvin */


/* Each time step has a time (in HHMMSS format) and date (in YYDDD format): */
int  grid_time[NTIMES] = { 120000, 120500, 121000, 121500, 122000, 122500,
                           123000, 123500, 124000, 124500 };
int  grid_date[NTIMES] = { 92027, 92027, 92027, 92027, 92027, 92027, 92027,
                           92027, 92027, 92027 };


/* The 5-D grid of data:
   The first dimension is time.  The second dimension is the physical
   variable.  The third, fourth, and fifth dimensions correspond to
   levels, columns, and rows, respectively.

   Note that grid[t][p][0][0][0] corresponds to the north-west-bottom corner
   and grid[t][p][NL-1][NC-1][NR-1] corresponds to the south-east-top corner.
   A 'missing' data value should be stored as 1.0e35.
*/
float grid[NTIMES][NPARAMS][NL][NC][NR];



/* You must write the body of this function.  Its purpose is to open a
   data file (whatever format you use) and read the data set into the
   5-Dimensional grid array.
*/
int read_grid( filename )
char *filename;
{
   FILE *f;
   int ip, it, ir, ic, il;
   float dl, dc, dr;

   f = fopen( filename, "r" );
   if (!f) {
      printf("Error: Unable to open file %s for reading\n", filename);
      exit(0);
   }


   /* READ YOUR DATA HERE: */
   /* You'll probably need to use fread if your file is in a binary format */
   /* or use fscanf if your file is in an ASCII format. */



   /* THIS IS JUST AN EXAMPLE OF PUTTING SOME DATA INTO THE grid ARRAY. */
   /* (YOU SHOULD DELETE THIS) */
   /* In this case, we put values in such that isosurfaces will look like */
   /* spheres and contour lines will look like circles in Vis5D. */
   for (it=0;it<NTIMES;it++) {
      for (ip=0;ip<NPARAMS;ip++) {
         for (il=0;il<NL;il++) {
            dl = (il-NL/2);
            for (ic=0;ic<NC;ic++) {
               dc = (ic-NC/2);
               for (ir=0;ir<NR;ir++) {
                  dr = (ir-NR/2);
                  grid[it][ip][il][ic][ir] = sqrt(dl*dl + dc*dc + dr*dr) +it+ip;
               }
            }
         }
      }
   }
   /* END OF EXAMPLE OF PUTTING DATA INTO grid ARRAY. (DELETE THIS) */


   fclose(f);
}




/**********************************************************************/
/**********************************************************************/
/***    You shouldn't have to change anything beyond this point.    ***/
/**********************************************************************/
/**********************************************************************/


main( argc, argv )
int argc;
char *argv[];
{
   int i, n;
   char mcname[1000];

   if (argc!=3) {
      printf("Usage:\n");
      printf("   sample yourfile nnnn\n");
      printf("Where:\n");
      printf("   yourfile is the filename of your data set to read.\n");
      printf("   nnnn is the number of the McIDAS file to write.\n");
      printf("Example:\n");
      printf("   sample MODEL.DATA 104\n");
      printf("       This will convert your file MODEL.DATA to the McIDAS\n");
      printf("       grid file named GR3D0104.\n");
      exit(0);
   }

   /* fill the grid array with data: */
   read_grid( argv[1] );

   /* write the file: */
   n = atoi( argv[2] );
   if (n<1 || n>9999) {
      printf("Error: The McIDAS grid file number must be between 1 and 9999\n");
      exit(0);
   }

   sprintf(mcname, "GR3D%04d", n );

   write_gridfile( mcname );


   /* verify it by printing the header... */
   print_header( mcname );

   /* ...and printing the info describing the first 4 grids: */
   for (i=1;i<5;i++) {
      printf("\ngrid %d:\n", i);
      print_gridinfo( mcname, i );
   }

}



/*
  NOTES ON FILE FORMAT:
   char must be 1 byte.
   int must be 4 bytes.
   float must be 4 bytes.

   Overall structure:
      Let nt = number of time steps
      Let np = number of physical variables or parameters

      part 1:  A 5-D header (256 bytes)
      part 2:  nt*np 3-D grid info headers (256 bytes each)
      part 3:  extra space for future headers
      part 4:  nt*np 3-D grids of data (each data point is a 4-byte float)
*/


/* McIDAS 5-D grid file header:  this is the very first structure in the file.
   (size is 64 words or 256 bytes)
*/

struct header {
/*  1 */   char ident[32];       /* file description/identification */
/*  9 */   int  nproj;           /* Project number? */
/* 10 */   int  creation_date;   /* Date created in YYDDD format */
/* 11 */   int  maxsize;         /* number of data points in largest 3D grid */
/* 12 */   int  numgrid;         /* number of 3D grids in this file */
/* 13 */   int  firstgrid;       /* location of first grid's data as offset */
                                 /*   in 4-byte words from start of file */
/* 14 */   int  pad[51];
};



/* McIDAS 3-D grid info block:  one of these is associated with each
   3-D sub-grid in the 5-D file.  (size is 64 words or 256 bytes)
*/

struct gridinfo {
/*  1 */   int  size;            /* number of data points / words of data */
                                 /*   size should be equal to nr * nc * nl */
/*  2 */   int  nr;              /* number of rows */
/*  3 */   int  nc;              /* number of columns */
/*  4 */   int  nl;              /* number of levels */
/*  5 */   int  iword;           /* location of data as offset in 4-byte words
                                        from start of file */
/*  6 */   int  date;            /* grid date stamp in YYDDD format */
/*  7 */   int  time;            /* grid time stamp in HHMMSS format */
/*  8 */   int  pad1;
/*  9 */   char name[4];         /* 4-character variable or parameter name */
/* 10 */   char units[4];        /* 4-character units description */
/* 11 */   int  pad2[11];
/* 22 */   int  itype;           /* always 4 */
/* 23 */   int  latn;            /* north latitude * 10000 */
/* 24 */   int  lonw;            /* west longitude * 10000 */
/* 25 */   int  latinc;          /* latitude increment * 10000 */
/* 26 */   int  loninc;          /* longitude increment * 10000 */
/* 27 */   int  pad3[4];
/* 31 */   int  ihtype;          /* always 1 */
/* 32 */   int  top;             /* top altitude * 1000 */
/* 33 */   int  topinc;          /* altitude increment * 1000 */
/* 34 */   int  pad4[31];
};




/* This function writes a 5-D grid file given the info in the above
   global variables and constants.
*/
write_gridfile( filename )
char filename[];
{
   struct header head;
   struct gridinfo info;
   int f;
   int it, ip, n, bytes, c;
   float inc;

   /* initialize grid header */
   strncpy( head.ident, identification, 31 );
   head.nproj = 0;
   head.creation_date = 1;
   head.maxsize = NR * NC * NL;
   head.numgrid = NTIMES * NPARAMS;
   head.firstgrid = (NTIMES * NPARAMS + 1) * 64;

   /* Open file */
   f = open( filename, O_WRONLY | O_CREAT );
   if (f>=0) {
      /* Write grid file header */
#ifdef LITTLE
      flip4( &head, &head, 32/4 );  /* if little endian flip char string */
#endif
      if (write_int4_array( f, (int*) &head, 64 )!=64) {
         fprintf( stderr, "Error while writing to grid file.\n" );
         close(f);
         exit(-1);
      }

      /* Write grid info blocks */
      for (it=0;it<NTIMES;it++) {
         for (ip=0;ip<NPARAMS;ip++) {
            info.size = NR * NC * NL;
            info.nr = NR;
            info.nc = NC;
            info.nl = NL;
            info.iword = sizeof(struct header) / 4
                         + NTIMES * NPARAMS * sizeof(struct gridinfo) / 4
                         + it * NPARAMS * NR * NC * NL
                         + ip * NR * NC * NL;
            info.date = grid_date[it];
            info.time = grid_time[it];
            /* pad name and unit strings with spaces if needed */
            for (c=0;c<4;c++) {
               if (param_name[ip][c]==0)
                  param_name[ip][c] = ' ';
               if (param_unit[ip][c]==0)
                  param_unit[ip][c] = ' ';
            }
            strncpy( info.name, param_name[ip], 4 );
            strncpy( info.units, param_unit[ip], 4 );
            info.itype = 4;
            info.ihtype = 1;
            info.latn = NORTH_LATITUDE * 10000;
            info.lonw = WEST_LONGITUDE * 10000;
            info.top = TOP_ALTITUDE * 1000;
            inc = (NORTH_LATITUDE-SOUTH_LATITUDE) / (NR-1);
            info.latinc = (int) (inc * 10000.0);
            inc = (WEST_LONGITUDE-EAST_LONGITUDE) / (NC-1);
            info.loninc = (int) (inc * 10000.0);
            inc = (TOP_ALTITUDE-BOTTOM_ALTITUDE) / (NL-1);
            info.topinc = (int) (inc * 1000.0);

            /* Write gridinfo struct */
#ifdef LITTLE
            /* flip char strings */
            flip4( &info.name, &info.name, 1 );
            flip4( &info.units, &info.units, 1 );
#endif
            if (write_int4_array( f, (int*) &info, 64 )!=64) {
/*            if (n!=1) {*/
               fprintf(stderr,"Error while writing grid info.\n");
               close(f);
               exit(-1);
            }
         }
      }

      /* Write grid data */
      bytes = NR * NC * NL * 4;
      for (it=0;it<NTIMES;it++) {
         for (ip=0;ip<NPARAMS;ip++) {
            if (write_bytes( f, grid[it][ip], bytes)!=bytes) {
               fprintf( stderr, "Error while writing grid data.\n" );
               close(f);
               exit(-1);
            }
         }
      }

      /* all done */
      close(f);
   }
   else {
      fprintf( stderr, "Unable to open output file: %s\n", filename );
      exit(-1);
   }

}




/* This function prints the header information in a 5-D grid file given
   a filename.
*/
print_header( filename )
char *filename;
{
   int f;
   struct header  head;
   int b;

   f = open(filename, O_RDONLY);
   if (f>=0) {
      if (read_int4_array( f, (int*) &head, 64 )==64) {
         /* flip character strings if on little endian */
#ifdef LITTLE
         flip4( &head, &head, 32/4 );  /* if little endian flip char string */
#endif
         printf("       identifier: %32s\n", head.ident);
         printf("       project id: %d\n", head.nproj);
         printf("    creation date: %d\n", head.creation_date);
         printf("max 3-D grid size: %d\n", head.maxsize);
         printf("     no. of grids: %d\n", head.numgrid);
         printf("first grid offset: %d\n", head.firstgrid);
      }
      else {
         printf("error reading header.\n");
      }
      close(f);
   }
   else {
      printf("Unable to open %s\n", filename );
   }
}



/* This function prints the grid information corresponding to grid 'gridno'
   in a 5-D grid file.
*/
print_gridinfo( filename, gridno )
char *filename;
int gridno;
{
   int f;
   struct gridinfo info;
   int b, bytes, offset;

   f = open(filename, O_RDONLY);
   if (f>=0) {
      bytes = sizeof( struct gridinfo );
      offset = gridno * bytes;
      if (lseek( f, offset, 0)==0) {
         b = read_bytes( f, &info, bytes );
         if (b>0) {
            printf("   size: %d\n", info.size );
            printf("     nr: %d\n", info.nr );
            printf("     nc: %d\n", info.nc );
            printf("     nl: %d\n", info.nl );
            printf("  iword: %d\n", info.iword );
            printf("   date: %d\n", info.date);
            printf("   time: %d\n", info.time);
            printf("   name: %c%c%c%c\n",
                   info.name[0], info.name[1], info.name[2], info.name[3]);
            printf("  units: %c%c%c%c\n",
                   info.units[0], info.units[1], info.units[2], info.units[3]);
            printf("   LatN: %f\n", (float) info.latn / 10000.0 );        
            printf(" LatInc: %f\n", (float) info.latinc / 10000.0  );
            printf("   LonW: %f\n", (float) info.lonw / 10000.0  );
            printf(" LonInc: %f\n", (float) info.loninc / 10000.0 );
            printf("    Top: %f\n", (float) info.top / 1000.0 );
            printf(" TopInc: %f\n", (float) info.topinc / 1000.0 );
         }
         else {
            printf("error reading info.\n");
         }
      }
      else {
         printf("error seeking.\n");
      }
      close(f);
   }
   else {
      printf("Unable to open %s\n", filename );
   }
}
