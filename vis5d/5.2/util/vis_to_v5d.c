/* vis_to_v5d.c */


 TODO: add variable omit feature


/* Convert NMS model VIS files to .v5d format.

   Usage:
      vis_to_v5d [options] VISfile [VISfile ...] v5dfile

   where:
      [options] is zero or more of:
         +spd   - Calculate wind speed from U,V,W.  An error will result
                  if U,V,W data is not found.

      VISfile [VISfile ...]  - the list of 1 or more files in the VIS
                  format.

      v5dfile  - the name of the .v5d file to create/append onto.

   output:
      The data in the VIS files will be compressed and APPENDED to the
      v5dfile.  If the v5dfile does not exist, it will be created
      using the information (grid size, geographic bounds, physical
      variables, etc) in the first VIS file.

*/


#ifndef __DATE__
#  define __DATE__ "???"
#endif

#include <stdio.h>
#ifndef SEEK_SET
#  define SEEK_SET 0
#endif
#ifndef SEEK_END
#  define SEEK_END 2
#endif
#include <math.h>
#include "v5d.h"



static char visfile[1000][50];
static int numfiles;
static char v5dfile[50];
static int SpeedOpt=0;



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
/*         buffer[count++] = val * (int) b + (int) a;*/
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
   int i, var, *header, nl;
   float tophgt, hgtinc, hgt;

   f = fopen(visfile,"r");
   if (!f) {
      printf("Unable to open %s\n", visfile );
      exit(1);
   }

   fscanf( f, "%d", &v->NumVars );
   if (v->NumVars>MAXVARS) {
      printf("ERROR: %s contains too many variables, limit is %d\n",
             visfile, MAXVARS );
   }

   v->NumTimes = 0;

   fscanf( f, "%d", &v->Nc );
   fscanf( f, "%d", &v->Nr );
   fscanf( f, "%d", &nl );
   for (i=0;i<v->NumVars;i++) {
      v->Nl[i] = nl;
   }
   fgetc( f );  /* skip \n */

   /* get parm names */
   for (var=0;var<v->NumVars;var++) {
      int header_size, data_size;
      char parm[40];
      float *data;

      fgets( parm, 40, f );
      header = read_int_block( f, &header_size );
      strncpy( v->VarName[var], parm, 8 );
      for (i=7;i>=0 && v->VarName[var][i]==' ';i--) {
         v->VarName[var][i] = '\0';
      }
      v->Projection = 1;  /* Cyl. Equidistant */
      v->NorthBound = (float) header[22] / 10000.0;
      v->WestBound = (float) header[23] / 10000.0;
      v->RowInc = (float) header[24] / 10000.0;
      v->ColInc = (float) header[25] / 10000.0;
      v->VerticalSystem = 1;
      tophgt = (float) header[31] / 1000.0;
      hgtinc = (float) header[32] / 1000.0;
      v->BottomHgt = tophgt - hgtinc * (nl-1);
      v->LevInc = hgtinc;

      free( header );

      /* read and skip data */
      data = read_float_block( f, &data_size );
      free( data );
   }
   fclose(f);

   v->NumTimes = 0;
   if (SpeedOpt) {
      /* add wind speed variable */
      strncpy( v->VarName[v->NumVars], "SPD", 8 );
      v->NumVars++;
   }

   v->CompressMode = 1;

   return 1;
}






/*** convert **********************************************************
   Convert the VIS files named in the visfile[] array to data appended
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
   float *udata, *vdata, *wdata;

   /* open v5dfile for updating */
   if (!v5dUpdateFile( v5dfile, &v )) {
      printf("Error:  couldn't open %s for updating\n", v5dfile );
      exit(0);
   }

   /* LOOP OVER FILES */
   for (i=0;i<numfiles;i++) {

      printf("file: %s\n", visfile[i] );

      /*** Open and read contents of VIS* file[i] ***/

      f = fopen(visfile[i],"r");
      if (!f) {
         printf("Unable to open %s\n", visfile[i] );
         exit(1);
      }

      fscanf( f, "%d", &nvars);
      fscanf( f, "%d", &nc );
      fscanf( f, "%d", &nr );
      fscanf( f, "%d", &nl );
      fgetc( f );  /* skip \n */
      if (nvars+SpeedOpt!=v.NumVars) {
         printf("ERROR: Number of parameters in %s is %d, expected %d\n",
                visfile[i], nvars, v.NumVars );
         exit(1);
      }
      if (nc!=v.Nc || nr!=v.Nr || nl!=v.Nl[0]) {
         printf("ERROR: Grid size in %s is %d x %d x %d,");
         printf(" expected %d x %d x %d\n",
                visfile[i], nr, nc, nl, v.Nr, v.Nc, v.Nl );
         exit(1);
      }

      udata = vdata = wdata = NULL;

      nrncnl = v.Nr * v.Nc * v.Nl[0];

      v.NumTimes++;

      /* LOOP OVER PARAMETERS */
      for (var=0;var<v.NumVars;var++) {

         if (strncmp(v.VarName[var],"SPD",3)==0) {
            /* calculate wind speed variable */
            if (udata && vdata && wdata) {
               printf(" SPD");
               fflush(stdout);
               data = (float *) malloc( nrncnl*sizeof(float) );
               if (!data) {
                  printf("Out of memory\n");
                  exit(1);
               }
               for (k=0;k<nrncnl;k++) {
                  if (udata[k]>=MISSING || vdata[k]>=MISSING
                      || wdata[k]>=MISSING)
                    data[k] = MISSING;
                  else
                    data[k] = sqrt( udata[k]*udata[k] + vdata[k]*vdata[k]
                                       + wdata[k]*wdata[k] );
               }
            }
            else {
               printf("ERROR:  requested wind speed without U,V,W data\n");
               exit(1);
            }
         }
         else {
            /* This is not the wind speed variable */
            int header_size, data_size, ii;
            
            fgets( parm, 40, f );
            for (ii=7;ii>=0 && parm[ii]==' ';ii--) {
               parm[ii] = '\0';
            }
            
            /* check if parm matches v.VarName[var] */
            if (strncmp(parm,v.VarName[var],3)!=0) {
               printf("ERROR: found variable %s, expected %s\n",
                      parm, v.VarName[var] );
               exit(1);
            }
            printf("  %s", v.VarName[var] );
            fflush(stdout);
            
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
         }

         v5dWriteGrid( &v, v.NumTimes-1, var, data );

         /*** check for U, V, W variables ***/
         if (parm[0]=='U' && (parm[1]==0 || parm[1]==' '))
           udata = data;
         else if (parm[0]=='V' && (parm[1]==0 || parm[1]==' '))
           vdata = data;
         else if (parm[0]=='W' && (parm[1]==0 || parm[1]==' '))
           wdata = data;
         else /* throw away values */
           free(data);

      }  /* End of loop over parameters */
      printf("\n");

      fclose(f);

      /* throw away u,v,w now */
      if (udata)
         free(udata);
      if (vdata)
         free(vdata);
      if (wdata)
         free(wdata);
      udata = vdata = wdata = NULL;

   }  /* End of loop over time step / files */

   v5dCloseFile( &v );
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
      printf("   Convert model VIS files to .v5d format\n");
      printf("   This executable compiled on %s\n", __DATE__ );
      printf("Usage:\n");
      printf("   vis_to_v5d [+spd] VISfile [VISfile ...] V5dfile\n");
      printf("Note:\n");
      printf("   +spd is only needed when making a new VIS-5D file.\n");
      exit(0);
   }

   /* check for options and get list of VIS* files */
   for (i=1;i<argc-1;i++) {
      if (argv[i][0]!='-' && argv[i][0]!='+')
         strcpy( visfile[numfiles++], argv[i] );
      else if (strcmp(argv[i],"+spd")==0 || strcmp(argv[i],"-spd")==0)
         SpeedOpt = 1;
   }
   strcpy( v5dfile, argv[argc-1] );


   if (v5dOpenFile(v5dfile, &v)) {
      /* v5dfile already exists */
      if (SpeedOpt) {
         printf("+spd was not needed.\n");
      }
      printf("Appending new data to %s\n", v5dfile );
      v5dCloseFile( &v );
   }
   else {
      /* read info in first VISfile, construct header, create V5dfile */
      printf("Creating new data file %s\n", v5dfile );
      read_vis_header( visfile[0], &v );
      if (!v5dCreateFile( v5dfile, &v )) {
         printf("Error: couldn't create output file: %s\n", v5dfile );
         exit(0);
      }
      v5dCloseFile( &v );
   }

   /* read, compress, and append VIS* files to the v5dfile */
   convert();

   printf("done\n");
}
