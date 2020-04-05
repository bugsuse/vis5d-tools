/* v5dappend.c */

/*
 * Append a number of v5d files onto a target file.
 */


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "v5d.h"



/*
 * Test if 'str' is in the list of strings given by 'num' and 'list'.
 * Input:  str - the string to test for
 *         num - number of variables in 'list'
 *         list - the list of strings.
 * Return:  -1 = str is not in list
 *          else, returned value is position of str in list starting from 0.
 */
static int member( char *str, int num, char *list[] )
{
   int i;

   for (i=0;i<num;i++) {
      if (strcmp( str, list[i] )==0) {
         /* found */
         return i;
      }
   }
   /* str is not in list */
   return -1;
}




/*
 * Use the header information in 'file' to initialize the given v5dstruct.
 */
static void init_v5dstruct( char *file, v5dstruct *v, int num_omit,
                            char *omitvar[] )
{
   v5dstruct vtemp;
   int i;

   if (v5dOpenFile( file, &vtemp )) {
      memcpy( v, &vtemp, sizeof(v5dstruct) );
      v5dCloseFile( &vtemp );
      v->NumTimes = 0;

      /* setup variables */
      v->NumVars = 0;
      for (i=0;i<vtemp.NumVars;i++) {
         if (member( vtemp.VarName[i], num_omit, omitvar ) == -1 ) {
            strcpy( v->VarName[v->NumVars], vtemp.VarName[i] );
            v->MinVal[v->NumVars] = vtemp.MinVal[i];
            v->MaxVal[v->NumVars] = vtemp.MaxVal[i];
            v->Nl[v->NumVars] = vtemp.Nl[i];
            v->NumVars++;
         }
      }

   }
   else {
      printf("Error: couldn't open %s\n", file );
      exit(1);
   }
}



static void append( int num_infiles, char **infile, char *outfile,
                    v5dstruct *outv )
{
   int i, j, n;
   v5dstruct *inv;
   int error;
   int invar, outvar, intime, outtime;
   float *missing_grid;
   float *grid_buffer;

   /* Initialize missing grid */
   n = outv->Nr * outv->Nc * MAXLEVELS;
   missing_grid = (float *) malloc( n * sizeof(float) );
   grid_buffer = (float *) malloc( n * sizeof(float) );

   if (!missing_grid || !grid_buffer) {
      printf("Error: couldn't allocate temp arrays\n");
      exit(1);
   }

   /* init missing grid data */
   for (i=0;i<n;i++) {
      missing_grid[i] = MISSING;
   }

   for (i=0;i<num_infiles;i++) {
      char *inv_vars[MAXVARS];

      /* Open the next input file */
      inv = v5dOpenFile( infile[i], NULL );
      if (!inv) {
         printf("Error: couldn't open %s\n", infile[i]);
         v5dCloseFile( outv );
         exit(1);
      }

      printf("Appending %s onto %s\n", infile[i], outfile );

      /* Make sure the input file's header info agrees with outv. */
      error = 0;
      if (inv->Nr!=outv->Nr) {
         printf("Error: number of rows in %s (%d) doesn't match %s (%d)\n",
                 infile[i], inv->Nr, outfile, outv->Nr );
         error = 1;
      }
      if (inv->Nc!=outv->Nc) {
         printf("Error: number of column in %s (%d) doesn't match %s (%d)\n",
                 infile[i], inv->Nc, outfile, outv->Nc );
         error = 1;
      }
      if (error) {
         v5dCloseFile(outv);
         printf("Stopping.\n");
         exit(1);
      }


      /* Make a list of pointers to the input file's variables.  We need
       * this to be able to use the member() function later.
       */
      for (j=0;j<inv->NumVars;j++) {
         inv_vars[j] = inv->VarName[j];
      }

      /* append data */
      for (intime=0; intime<inv->NumTimes; intime++) {
         float ga[MAXLEVELS], gb[MAXLEVELS];

         outtime = outv->NumTimes;
         outv->TimeStamp[outtime] = inv->TimeStamp[intime];
         outv->DateStamp[outtime] = inv->DateStamp[intime];
         outv->NumTimes++;

         for (outvar=0; outvar < outv->NumVars; outvar++) {

            /* find corresponding invar */
            invar = member( outv->VarName[outvar], inv->NumVars, inv_vars );

            if (invar==-1) {
               /* the input file is missing the variable we want to write */
               v5dWriteGrid( outv, outtime, outvar, missing_grid );
            }
            else {
               if (inv->CompressMode==outv->CompressMode) {
                  /* faster */
                  v5dReadCompressedGrid( inv, intime, invar, ga, gb, grid_buffer );
                  v5dWriteCompressedGrid( outv, outtime, outvar, ga, gb, grid_buffer );
               }
               else {
                  /* slower */
                  v5dReadGrid( inv, intime, invar, grid_buffer );
                  v5dWriteGrid( outv, outtime, outvar, grid_buffer );
               }
               /* check if outv's min and max have to be updated */
               if (inv->MinVal[invar] < outv->MinVal[outvar]) {
                  outv->MinVal[outvar] = inv->MinVal[invar];
               }
               if (inv->MaxVal[invar] > outv->MaxVal[outvar]) {
                  outv->MaxVal[outvar] = inv->MaxVal[invar];
               }
            }
         } /* for outvar */
      } /* for intime */
   }

   v5dCloseFile(outv);
   printf("Done.\n");
}


#define MAX_INFILES 100


int main( int argc, char *argv[] )
{
   char *infile[MAX_INFILES], *outfile;
   int num_infiles;
   int i;
   v5dstruct v;
   char *omitvar[MAXVARS];
   int num_omit;

   if (argc<3) {
      printf("v5dappend:\n");
      printf("   Append a number of v5d files onto a target file.\n");
      printf("Usage:\n");
      printf("   v5dappend [-var] [...] file.v5d [...] target.v5d\n");
      printf("Arguments:\n");
      printf("   [-var] [...] is an optional list of variables to omit whe creating target.\n");
      printf("   file.v5d [...] is the list of input files.\n");
      printf("   target.v5d is the name of the file to append onto\n");
      return 0;
   }

   /* get arguments */
   num_omit = 0;
   num_infiles = 0;

   for (i=1;i<argc-1;i++) {
      if (argv[i][0]=='-') {
         /* variable to omit */
         omitvar[num_omit] = argv[i]+1;
         num_omit++;
      }
      else {
         if (num_infiles>=MAX_INFILES) {
            printf("Error: too many input files, limit is 100\n");
            break;
         }
         infile[num_infiles] = argv[i];
         num_infiles++;
      }
   }
   outfile = argv[argc-1];
   

   if (v5dUpdateFile( outfile, &v )) {
      /* just append onto outfile */
      printf("Appending onto %s\n", outfile );
      if (num_omit>0) {
         printf("Note: no need to specify variables to omit when appending!\n");
      }
   }
   else {
      /* outfile doesn't exits.  Use first input file to initialize outfile. */
      printf("Creating %s\n", outfile);
      init_v5dstruct( infile[0], &v, num_omit, omitvar );
      if (!v5dCreateFile( outfile, &v )) {
         return 1;
      }
   }

   append( num_infiles, infile, outfile, &v );

   return 0;
}
