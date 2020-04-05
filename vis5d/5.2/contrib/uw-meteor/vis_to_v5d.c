/* vis_to_v5d.c */


/*
   Convert NMS model VIS files to .v5d format.  This version handles unequally
   spaced vertical levels (i.e. variable delta Z).  Also, the +spd option has
   been removed because one can compute wind speed from U,V,[W] from within
   vis5d in Version 4 and later.

   Usage:
      vis_to_v5d [options] VISfile [VISfile ...] v5dfile

   where:
      [options] is zero or more of:
	 -<var> - Omit the specified variable from the output file

      VISfile [VISfile ...]  - the list of 1 or more files in the VIS
                  format.

      v5dfile  - the name of the .v5d file to create/append onto.

   output:
      The data in the VIS files will be compressed and APPENDED to the
      v5dfile.  If the v5dfile does not exist, it will be created
      using the information (grid size, geographic bounds, physical
      variables, etc) in the first VIS file.

   notes:
      usually, 1 file contains exactly 1 time step.
*/



#ifndef __DATE__
#  define __DATE__ "???"
#endif

#include <ctype.h>
#include <stdio.h>
#ifndef SEEK_SET
#  define SEEK_SET 0
#endif
#ifndef SEEK_END
#  define SEEK_END 2
#endif
#include <math.h>
#include "v5d.h"



static char VisFile[1000][50];		/* list of vis files to convert */
static int NumFiles;			/* how many files in visfile[] */
static char OmitVar[MAXVARS][20];	/* list of variables to omit */
static int NumOmit;			/* how many variables in OmitVar[] */
static char V5dFile[50];		/* name of output file */



/*
 * Determine if the given variable name is on the omit list.  Note that
 * the test is case sensitive.
 * Return:  1 = omit,  0 = don't omit
 */
static int test_omit( name )
char *name;
{
   int i;

   for (i=0;i<NumOmit;i++) {
      if (strcmp(OmitVar[i],name)==0) {
	 return 1;
      }
   }
   return 0;
}



/**********************************************************************/
/*****                      VIS file I/O                          *****/
/**********************************************************************/


static char vcscr[64] = {
                '0','1','2','3','4','5','6','7','8','9'
               ,'A','B','C','D','E','F','G','H','I','J'
               ,'K','L','M','N','O','P','Q','R','S','T'
               ,'U','V','W','X','Y','Z','a','b','c','d'
               ,'e','f','g','h','i','j','k','l','m','n'
               ,'o','p','q','r','s','t','u','v','w','x'
               ,'y','z','{','|'
};

static char inv_vcscr[256];


int *read_int_block( f, len )
FILE *f;
int *len;
{
   int words, bits, ch, val, i, j, k, count, bytes;
   float a, b;
   char line[80];
   int *buffer;

   /* read number of words, addfac, multfac */
   fscanf( f, "%d %d %f %f", &words, &bits, &a, &b);
   fgetc( f );  /* skip \n */

   buffer = (int *) malloc( words * sizeof(int) );
   bytes = (bits+5) / 6;

   count = 0;
   while (count<words) {

      /* read a line */
      fgets( line, 80, f );

      /* convert line of chars to integers */
      for (i=k=0;i<78/bytes && count<words;i++) {
	 for (j=val=0;j<bytes;j++) {
	    ch = inv_vcscr[line[k++]];
	    val = (val << 6) | ch;
	 }
/*	 buffer[count++] = val * (int) b + (int) a;*/
	 buffer[count++] = val * (int) b - (int) a;
      }

   }

   *len = words;
   return buffer;
}




float *read_float_block( f, len )
FILE *f;
int *len;
{
   int words, bits, ch, val, i, j, k, count, bytes;
   float a, b;
   char line[80];
   float *buffer;

   /* read number of words, addfac, multfac */
   fscanf( f, "%d %d %f %f", &words, &bits, &a, &b);
   fgetc( f );  /* skip \n */

   buffer = (float *) malloc( words * sizeof(float) );
   bytes = (bits+5) / 6;

   count = 0;
   while (count<words) {

      /* read a line */
      fgets( line, 80, f );

      /* convert line of chars to floats */
      for (i=k=0;i<78/bytes && count<words;i++) {
	 for (j=val=0;j<bytes;j++) {
	    ch = inv_vcscr[line[k++]];
	    val = (val << 6) | ch;
	 }
	 buffer[count++] = (float) val / b - a;
      }

   }

   *len = words;
   return buffer;
}



/*
 * Read the info in a VIS file to initialize the given v5dstruct
 */
int read_vis_header( visfile, v )
char visfile[];
v5dstruct *v;
{
   FILE *f;
   int i, var, *header, nl, numvars;
   float tophgt, hgtinc, hgt;
   int ch, vcs;

   f = fopen(visfile,"r");
   if (!f) {
      printf("Unable to open %s\n", visfile );
      exit(1);
   }

   fscanf( f, "%d", &numvars );
   if (numvars>MAXVARS) {
      printf("ERROR: %s contains too many variables, limit is %d\n",
	     visfile, MAXVARS );
   }

   v->NumVars = 0;
   v->NumTimes = 0;

   fscanf( f, "%d", &v->Nc );
   fscanf( f, "%d", &v->Nr );
   fscanf( f, "%d", &nl );
   for (i=0;i<v->NumVars;i++) {
      v->Nl[i] = nl;
   }
   fgetc( f );  /* skip \n */


   /* Added on 4 Jan 95 by BEP: */
   ch = getc(f);
   ungetc( ch, f );

   if (isalpha(ch)) {
      vcs = 1;
   }
   else {
      /* Read the height (in meters) of each grid level */
      for (i=0;i<nl;i++) {
	 fscanf( f, "%8f", &v->Height[i] );
	 v->Height[i] /= 1000.0;    /* convert from meters to km */
      }
      (void) getc(f);  /* get '\n' */
      vcs = 2;   /* unequally spaced km */
   }


   /* get parm names */
   for (var=0;var<numvars;var++) {
      int header_size, data_size;
      char parm[40];
      float *data;

      /* read variable name and remove trailing spaces */
      fgets( parm, 40, f );
      for (i=7;i>=0 && parm[i]==' ';i--) {
	 parm[i] = '\0';
      }
      parm[8] = 0;

      header = read_int_block( f, &header_size );

      /* if the variable name is not on the omit list... */
      if (!test_omit(parm)) {
	 strncpy( v->VarName[v->NumVars], parm, 8 );
	 v->Nl[v->NumVars] = nl;
	 v->NumVars++;

	 v->Projection = 1;  /* Cyl. Equidistant */
	 v->NorthBound = (float) header[22] / 10000.0;
	 v->WestBound = (float) header[23] / 10000.0;
	 v->RowInc = (float) header[24] / 10000.0;
	 v->ColInc = (float) header[25] / 10000.0;

	 v->VerticalSystem = vcs;  /* equally spaced km */
	 if (vcs==1) {
	    tophgt = (float) header[31] / 1000.0;
	    hgtinc = (float) header[32] / 1000.0;
	    v->BottomBound = tophgt - hgtinc * (nl-1);
	    v->LevInc = hgtinc;
	 }
	 else {
	    /* vcs==2 */
	    /* unequally spaced km */
	    /*memcpy( v->Height, height, sizeof(float)*nl );*/
	 }

      }

      free( header );

      /* read and skip data */
      data = read_float_block( f, &data_size );
      free( data );
   }
   fclose(f);

   v->NumTimes = 0;

   v->CompressMode = 1;

   return 1;
}



/*** convert **********************************************************
   Convert the VIS files named in the VisFile[] array to data appended
   to a VIS-5D compressed file.
**********************************************************************/
int convert()
{
   v5dstruct v;
   FILE *f;
   int i, j, k, var, nvars, nc, nr, nl, nrncnl;
   char parm[40];
   int *header;
   float *data;
   int ch, vcs;

   /* open v5dfile for updating */
   if (!v5dUpdateFile( V5dFile, &v )) {
      printf("Error:  couldn't open %s for updating\n", V5dFile );
      exit(0);
   }

   /* LOOP OVER FILES */
   for (i=0;i<NumFiles;i++) {

      printf("Reading: %s\n", VisFile[i] );

      /*** Open and read contents of VIS* file[i] ***/

      f = fopen(VisFile[i],"r");
      if (!f) {
	 printf("Unable to open %s\n", VisFile[i] );
	 exit(1);
      }

      fscanf( f, "%d", &nvars);
      fscanf( f, "%d", &nc );
      fscanf( f, "%d", &nr );
      fscanf( f, "%d", &nl );
      fgetc( f );  /* skip \n */
      if (nvars-NumOmit!=v.NumVars) {
	 printf("ERROR: Number of variables in %s is %d, expected %d\n",
		VisFile[i], nvars, v.NumVars );
	 exit(1);
      }
      if (nc!=v.Nc || nr!=v.Nr || nl!=v.Nl[0]) {
	 printf("ERROR: Grid size in %s is %d x %d x %d,");
	 printf(" expected %d x %d x %d\n",
		VisFile[i], nr, nc, nl, v.Nr, v.Nc, v.Nl );
	 exit(1);
      }

      ch = getc(f);
      ungetc( ch, f );

      if (isalpha(ch)) {
	 vcs = 1;
      }
      else {
	 /* read grid level heights and discard */
	 for (j=0;j<v.Nl[0];j++) {
	    float foo;
	    fscanf( f, "%8f", &foo );
	 }
	 (void) getc(f);  /* get '\n' */
	 vcs = 2;
      }

      nrncnl = v.Nr * v.Nc * v.Nl[0];

      v.NumTimes++;

      /* LOOP OVER VARIABLES */
      for (var=0;var<v.NumVars;var++) {
	 int omit;
	 int header_size, data_size, ii;
 
	 while (1) {
	    /* read next grid from vis file */
	    if (!fgets( parm, 40, f )) {
	       /* EOF */
	       printf("Error: variable %s not found in file %s\n",
		      v.VarName[var], VisFile[i] );
	       exit(1);
	    }
	    for (ii=7;ii>=0 && parm[ii]==' ';ii--) {
	       parm[ii] = '\0';
	    }
	    /* read header */
	    header = read_int_block( f, &header_size );
	    v.DateStamp[v.NumTimes-1] = v5dFromYYDDD( header[5] );
	    v.TimeStamp[v.NumTimes-1] = v5dFromHHMMSS( header[6] );
	    free( header );

	    /* read data */
	    data = read_float_block( f, &data_size );
	    if (data_size!=v.Nr*v.Nc*v.Nl[0]) {
	       printf("ERROR: number of data points: %d",data_size);
	       printf(" doesn't equal %d x %d x %d\n", v.Nr, v.Nc, v.Nl );
	       exit(1);
	    }

	    /* check if parm matches v.VarName[var] */
	    if (strncmp(parm,v.VarName[var],8)==0) {
	       /* Matches!  Write this grid to output file! */
	       printf("%s ", v.VarName[var]);
	       fflush(stdout);
	       v5dWriteGrid( &v, v.NumTimes-1, var, data );
	       break;  /* out of while */
	    }
	    else if (test_omit(parm)==0) {
	       /* parm is not on omit list, this is unexpected */
	       printf("Warning: variable %s unexpected\n", parm );
	       /* the while loop repeats... */
	    }
	    else {
	       /* The variable we just read is on the omit list.  */
	       /* This is OK.  The while loop repeats... */
	    }
	 }

	 /* free data buffer */
	 free(data);

      }  /* End of loop over variables */
      printf("\n");

      fclose(f);

   }  /* End of loop over time step / files */

   if (!v5dCloseFile( &v )) {
      printf("Error while closing file, out of disk space?\n");
   }

}




main( argc, argv )
int argc;
char *argv[];
{
   v5dstruct v;
   int i;

   /* init inv_vcscr array */
   for (i=0;i<64;i++) {
      inv_vcscr[ vcscr[i] ] = i;
   }

   if (argc==1) {
      printf("About:\n");
      printf("   Convert RAMS/NMS model VIS files to v5d format\n");
      printf("   Unequally spaced height levels are supported\n");
      printf("   This executable compiled on %s\n", __DATE__ );
      printf("Usage:\n");
      printf("   vis_to_v5d [options] VISfile [VISfile ...] v5dfile\n");
      printf("Options:\n");
      printf("   -<var>  omit the specified variable from output file\n");
      printf("Example:\n");
      printf("   vis_to_v5d -THETA VIS.0 VIS.1 VIS.2 outfile.v5d\n");
      exit(0);
   }

   /* check for options and get list of VIS* files */
   NumOmit = 0;
   for (i=1;i<argc-1;i++) {
      if (argv[i][0]!='-' && argv[i][0]!='+')
         strcpy( VisFile[NumFiles++], argv[i] );
      else if (argv[i][0]=='-') {
	 strcpy( OmitVar[NumOmit], argv[i]+1 );
	 printf("Omitting variable %s\n", OmitVar[NumOmit] );
	 NumOmit++;
      }
   }
   strcpy( V5dFile, argv[argc-1] );


   if (v5dOpenFile(V5dFile, &v)) {
      /* V5dFile already exists */
      if (NumOmit>0) {
	 printf("-{var} was not needed.\n");
      }
      printf("Appending new data to %s\n", V5dFile );
      v5dCloseFile( &v );
   }
   else {
      /* read info in first VISfile, construct header, create V5dFile */
      printf("Creating new data file %s\n", V5dFile );
      v5dInitStruct( &v );
      read_vis_header( VisFile[0], &v );
      if (!v5dCreateFile( V5dFile, &v )) {
	 printf("Error: couldn't create output file: %s\n", V5dFile );
	 exit(0);
      }
      v5dCloseFile( &v );
   }

   /* read, compress, and append VIS* files to the V5dFile */
   convert();

   printf("Done\n");
   return 0;
}
