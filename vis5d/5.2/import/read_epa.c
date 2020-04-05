/* read_epa.c */


/*
 * Functions for working with EPA files
 */



#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "file.h"
#include "grid.h"
#include "misc.h"
#include "model.h"
#include "proj.h"
#include "projlist.h"
#include "v5d.h"


#ifdef EPA


#define LATLON_FILE "latlon.dat"


/* This is needed by mm4.c: */
float mm4Sigma[MAXLEVELS];
int model_type;

/* Sigma value for each level in the 3-D grid for
   15 level RADM files (how general?) */
static float RadmSigma15[15] = {
   0.995,
   0.985,
   0.970,
   0.945,
   0.910,
   0.865,
   0.810,
   0.740,
   0.650,
   0.550,
   0.450,
   0.350,
   0.250,
   0.150,
   0.050
};

/* Sigma value for each level in the 3-D grid for
   6 level RADM files (how general?) */
static float RadmSigma6[6] = {
   0.99,
   0.955,
   0.885,
   0.720,
   0.450,
   0.150,
};



/*
 * Convert a date from the EPA date/time string format, which is
 * "mo/day/year  hour:min:sec", to VIS-5D format, which is two
 * integers giving the number of days since 1 Jan 1900 and the
 * number of seconds since midnight (usually GMT).
 */
static void convert_date( char *datestr, int *date, int *time )
{
  int mo, day, year, hour, min, sec, i;
  int days_per_month[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

  sscanf(datestr, "%d/%d/%d %d:%d:%d",
         &mo, &day, &year, &hour, &min, &sec);

/*
  printf("mo: %d  day: %d  year: %d  hour: %d  min: %d  sec: %d\n",
         mo, day, year, hour, min, sec);
*/

  if ((!(year % 4)) && (year % 100)) days_per_month[1] = 29;

  for (i=0; i<mo-1; i++) {
    day += days_per_month[i];
  }

/*
  printf("day: %d  year: %d\n", day, year);
*/

  *date = (year * 1461) / 4 + day;
  *time = hour * 60 * 60 + min * 60 + sec;
  return;
}





/*
 * Get the lat/lon/height information for an EPA dataset.
 * (This information exists in various formats depending on the
 * model.  For example, in some cases it is on a piece of paper
 * somewhere in the top right hand desk drawer in the modeler's
 * office.)  Also get spatial bunds for the model domain.
 *
 * Input:  nr, nc, nl - grid size
 *         numvars - number of variables
 *         varname - array of variable names
 *         projargs - buffer of 2*nr*nc floats
 *         vertargs - buffer of nl floats
 * Output:  proj_out - map projection struct pointer
 *          vcs_out - vertical coord system struct pointer
 * Return:  1 = ok, 0 = error
 */
static int get_epa_projection( struct grid_db *db,
                               int radm_fd,
                               int nr, int nc, int nl,
                               int numvars,
                               char varname[MAXVARS][10],
                               struct projection **proj_out,
                               struct vcs **vcs_out )
{
   float pargs[MAXROWS * MAXCOLUMNS * 2];
   float vargs[MAXLEVELS * 2];
#define Latitude(ROW,COL)   pargs[ ((ROW)*nc+(COL)) * 2 + 0 ]
#define Longitude(ROW,COL)  pargs[ ((ROW)*nc+(COL)) * 2 + 1 ]
#define Sigma(LEVEL)        vargs[ (LEVEL) ]
/*
   float Latitude[MAXROWS][MAXCOLUMNS], Longitude[MAXROWS][MAXCOLUMNS];
   float Sigma[MAXLEVELS];
*/
   FILE *f;
   int i, j, k, kk;
   float lat, lon;
   char        message[80];
   char species[80];
   char units[80];
   int guard;


   if (model_type == RADM) {
      /*
       * Read the (row,column) to (lat,lon) conversion information from
       * the LATLON_FILE:
       */
      if (nr == 38 && nc == 35) {
         f = fopen( LATLON_FILE, "r" );
         if (!f) {
            printf("Error:  unable to open %s\n", LATLON_FILE );
            return 0;
         }
         while (!feof(f)) {
            if (fscanf( f, "%d %d %f %f\n", &j, &i, &lat, &lon )<4) {
               break;
            }
            else {
               if (i>nr || j>nc) {
                  printf("BAD I,J: % %d\n", i, j );
               }
               /* note that we flip the rows from North to South -
                 with VIS-5D the first row is at the North, and
                 rows increase toward the South -
                 we also flip the data North to South after we
                 get it from model_get_data -
                 of course we could probably skip both of these flips
                 and let the resampling take care of it */
               Latitude(nr-i,j-1) = lat;
               Longitude(nr-i,j-1) = lon;
            }
         }
         fclose(f);
      }
      else if (nr == 61 && nc == 69) {
         float lowLat[MAXROWS][MAXCOLUMNS], lowLon[MAXROWS][MAXCOLUMNS];
         f = fopen( LATLON_FILE,"r");
         if (!f) {
            printf("Error:  unable to open %s\n", LATLON_FILE );
            exit(1);
         }
         while (!feof(f)) {
            if (fscanf( f, "%d %d %f %f\n", &j, &i, &lat, &lon )<4) {
               break;
            }
            else {
               if (i>38 || j>35) {
                  printf("BAD I,J: % %d\n", i, j );
               }
               lowLat[i-1][j-1] = lat;
               lowLon[i-1][j-1] = lon;
            }
         }
         for (i=0; i<nr; i++) {
            for (j=0; j<nc; j++) {
               float ra, ca;
               int ir, jc;

               ra = i / 4.0; /* covert 20 km res to 80 km res */
               ca = j / 4.0;
               ir = ra; /* get integer parts of 80 km grid coord */
               jc = ca;
               ra = ra - ir; /* get fractional part2 */
               ca = ca - jc;
               ir = 16 + ir; /* add offset for (1,1) -> (17,15) */
               jc = 14 + jc;

               /* bilinear interp of lat and lon */
               lat = (1.0 - ra) * ((1.0 - ca) * lowLat[ir][jc] +
                                          ca  * lowLat[ir][jc+1]) +
                            ra  * ((1.0 - ca) * lowLat[ir+1][jc] +
                                          ca  * lowLat[ir+1][jc+1]);
               lon = (1.0 - ra) * ((1.0 - ca) * lowLon[ir][jc] +
                                          ca  * lowLon[ir][jc+1]) +
                            ra  * ((1.0 - ca) * lowLon[ir+1][jc] +
                                          ca  * lowLon[ir+1][jc+1]);

               /* note that we flip the rows from North to South -
                 with VIS-5D the first row is at the North, and
                 rows increase toward the South -
                 we also flip the data North to South after we
                 get it from model_get_data -
                 of course we could probably skip both of these flips
                 and let the resampling take care of it */
               Latitude(nr-1-i,j) = lat;
               Longitude(nr-1-i,j) = lon;
            }
         }
         fclose(f);
      }
      else { /* not "low res" or "hi res", so put it in the ocean */
         for (i=0; i<nr; i++) {
            for (j=0; j<nc; j++) {
               Latitude(i,j) = 5.0 - 0.01 * i;
               Longitude(i,j) = 140.0 - 0.01 * j;
            }
         }
      }

      if(nl == 15) {
         for (k=0; k<nl; k++) Sigma(k) = RadmSigma15[k];
      }
      else if(nl == 6) {
         for (k=0; k<nl; k++) Sigma(k) = RadmSigma6[k];
      }
      else {
         printf("RADM number of sigma levels must be 6 or 15 (is %d)\n", nl);
         exit(1);
      }
   }
   else if (model_type == MM4) {
      /* Read (row,col) -> (lat,lon) mappings from the data file itself. */
      float *gridlat, *gridlon;
      int iplat = -9, iplon = -9, ip, status;

      for (ip=0;ip<numvars;ip++) {
         if (strcmp(varname[ip],"LATC")==0) iplat = ip;
         if (strcmp(varname[ip],"LONC")==0) iplon = ip;
      }

      /* printf("iplat: %d  iplon: %d\n", iplat, iplon); */

      if (iplat < 0 || iplon < 0) {
         printf("bad iplat (%d) or iplon (%d)\n", iplat, iplon);
         exit(1);
      }

      gridlat = (float *) malloc( nr * nc * nl * sizeof(float) );
      gridlon = (float *) malloc( nr * nc * nl * sizeof(float) );

      strcpy(species, varname[iplat]);
      strcpy(message, "");
      if (!(status = model_get_data(radm_fd, 1, iplat+1, gridlat,
                                    species, units, message))) {
         printf("Error while reading mm4 lat grid: %s\n", message);
         exit(1);
      }
/*
      printf("mm4_get_data gridlat. status: %d, message: %s\n",
            status, message);
*/

      strcpy(species, varname[iplon]);
      strcpy(message, "");
      if (!(status = model_get_data(radm_fd, 1, iplon+1, gridlon,
                                   species, units, message))) {
         printf("Error while reading mm4 lat grid: %s\n", message);
         exit(1);
      }
/*
      printf("mm4_get_data gridlon. status: %d, message: %s\n",
            status, message);
*/

/*
      for (i=0; i<nr*nc; i += 200) {
        printf("gridlat[%d] = %f, gridlon[%d] = %f\n",
              i, gridlat[i], i, gridlon[i]);
      }
*/

      for (i=0; i<nr; i++) {
         for (j=0; j<nc; j++) {
            /* note that we flip the rows from North to South -
            with VIS-5D the first row is at the North, and
            rows increase toward the South -
            we also flip the data North to South after we
            get it from model_get_data -
            of course we could probably skip both of these flips
            and let the resampling take care of it */
            Latitude((nr-1)-i, j)  = gridlat[(i*nc) + j];
            Longitude((nr-1)-i, j) = -gridlon[(i*nc) + j];
         }
      }

      free(gridlat);
      free(gridlon);

      for (k=0; k<nl; k++) {
         Sigma(k) = (mm4Sigma[k] + mm4Sigma[k+1]) / 2.0;
      }
   }
   else {
      printf("lat/lon not yet supported for model_type %d\n", model_type);
      exit(1);
   }

#undef Latitude
#undef Longitude
#undef Sigma


   /* return projection and vcs struct pointers */
   *proj_out = new_projection( db, PROJ_EPA, nr, nc, pargs );
   *vcs_out = new_vcs( db, VERT_EPA, nl, 0, vargs );

   return 1;
}

#endif /*EPA*/




/*
 * Scan the named file, createing a grid_info struct for each grid.  Store
 * the grid_info structs in the grid data base.
 * Input:  name - name of file to scan
 *         db - the grid data base
 * Return:  number of grids found.
 */
int get_epa_info( char *name, struct grid_db *db )
{
#ifdef EPA
   int fd;
   char message[1000];
   int nr, nc, nl, numtimes, numvars;
   char specstr[1000], datestr[1000];
   char varname[MAXVARS][10];
   int varnumber[MAXVARS];
   int i, j, k;
   int var, time;
   int datestamp[MAXTIMES], timestamp[MAXTIMES];
   int grids;
   struct projection *proj;
   struct vcs *vcs;

   if (!model_open( &fd, name, message)) {
      printf("%s\n", message);
      return 0;
   }

/*   printf("get mm4 info\n");*/

   if (!model_inquire( fd, message, &nc, &nr, &nl, &numtimes, &numvars,
                       specstr )) {
      printf("%s\n", message);
      close(fd);
      return 0;
   }


/*   printf("nr=%d nc=%d nl=%d  times=%d  vars=%d\n", nr, nc, nl, numtimes,
          numvars );
   printf("varnames: %s\n", specstr );
*/

   /* Get variable names */
   k = 0;
   for (var=0;var<numvars;var++) {
      /* skip garbage chars */
      while(!(isalnum(specstr[k]) || specstr[k] == ' ')) k++;
      /* copy good chars */
      i = 0;
      while(isalnum(specstr[k]) || specstr[k] == ' ') {
        varname[var][i++] = specstr[k++];
      }
      varname[var][i] = 0;

      /* remove trailing spaces */
/* CAN'T REMOVE TRAILING SPACES: IT CAUSES PROBLEMS IN THE mm4.c FUNCTIONS!
      if (i>0) {
         i--;
         while (varname[var][i]==' ') {
            varname[var][i] = 0;
            i--;
         }
      }
*/

/*      printf("Varname[%d] = %s.\n", var, varname[var] );*/
      varnumber[var] = var;
   }


   /* Get time/date stamps */
   for (time=0;time<numtimes;time++) {
      if (!model_get_date( fd, time+1, datestr, message)) {
         printf("%s\n", message );
         return 0;
      }
      convert_date( datestr, &datestamp[time], &timestamp[time] );
      timestamp[time] = v5dSecondsToHHMMSS( timestamp[time] );
      datestamp[time] = v5dDaysToYYDDD( datestamp[time] );
   }


   /* Get map projection and vertical coord system */
   if (!get_epa_projection( db, fd, nr, nc, nl, numvars, varname,
                            &proj, &vcs )) {
      close(fd);
      return 0;
   }


   /* Remove variables names which aren't used after first time step */
   /* This is done because the first timestep has some special "meta" */
   /* variables which we don't want to visualize.  This has to be done */
   /* after get_epa_projection() is called! */
#ifdef LEAVEOUT
   if (model_type == MM4 && numtimes > 1) {
      float *grid3d = (float *) malloc( nr * nc * nl * sizeof(float) );
      for (var=0;var<numvars;var++) {
         char species[100], units[100];
         strcpy( species, varname[var] );
         if (!model_get_data( fd, 2, var+1, grid3d, species, units, message)) {
            /*printf("%d: %s %s\n", var, species,  message );*/
            /* remove this variable */
            for (j=var;j<numvars-1;j++) {
               strcpy( varname[j], varname[j+1] );
               varnumber[j] = varnumber[j+1];
            }
            numvars--;
            var--;   /* this is tricky */
         }
      }
      free( grid3d );
   }
#endif

#ifdef LEAVEOUT
   if (model_type==MM4) {
      r->Guard = 2;
   }
   else if (model_type==RADM) {
      r->Guard = 1;
   }
   else {
      r->Guard = 1;
   }
#endif

/*   printf("numtimes=%d  numvars=%d\n", numtimes, numvars );*/
   /* Add grid_info nodes to grid list */
   grids = 0;
   for (time=0;time<numtimes;time++) {
      for (var=0;var<numvars;var++) {
         struct grid_info *info;

         info = alloc_grid_info();
         info->FileName = str_dup( name );
         info->Format = FILE_EPA;
         info->TimeStep = time;
         info->VarNum = varnumber[var];

         info->Nr = nr;
         info->Nc = nc;
         info->Nl = nl;

         info->DateStamp = datestamp[time];
         info->TimeStamp = timestamp[time];
         info->VarName = str_dup( varname[var] );

         /* map projection & vcs */
         info->Proj = proj;
         info->Vcs = vcs;

         append_grid( info, db );
         grids++;
      }
   }

/*   printf("read %d grids\n", grids );*/
   return grids;
#else
   printf("Warning:  Can't read EPA mm4 files on this system\n");
   return 0;
#endif
}




/*
 * Reverse the order of the rows in a 2-D array.
 */
static void flip_north_south( float data[], int rows, int columns )
{
   int i, nbytes;
   float temp[MAXCOLUMNS];

   nbytes = columns * sizeof(float);

   for (i=0;i<rows/2;i++) {
      memcpy( temp, data+i*columns, nbytes );
      memcpy( data+i*columns, data+(rows-i-1)*columns, nbytes );
      memcpy( data+(rows-i-1)*columns, temp, nbytes );
   }
}



/*
 * Change 2-D array from row-major to column-major.
 */
static void transpose( float in[], float out[], int rows, int columns )
{
   int i, j, p;
   static float *temp = NULL;
   static int tempsize = 0;

   /* allocate temporary buffer if needed or if current one is too small */
   if (!temp || tempsize < rows*columns) {
      if (temp)  free(temp);
      temp = (float *) malloc( rows * columns * sizeof(float) );
      tempsize = rows * columns;
   }

   p = 0;
   for (j=0;j<columns;j++) {
      for (i=0;i<rows;i++) {
         temp[ j*rows+i ] = in[i*columns+j];
      }
   }

   memcpy( out, temp, rows*columns*sizeof(float) );
}




/*
 * Get the grid data described by g.
 * Input:  g - pointer to a grid_info structure.  It tells us which grid
 *             in which file is needed.
 * Return:  pointer to grid data (can be free()'d when finished)
 *          OR return NULL if there's an error.
 */
float *get_epa_data( struct grid_info *g )
{
#ifdef EPA
   float *data;
   int count;
   char species[80], units[80], message[80];
   int fd;
   int i;

   /* open file */
   if (!model_open( &fd, g->FileName, message)) {
      printf("%s\n", message);
      return NULL;
   }

   /* allocate buffer */
   count = g->Nr * g->Nc * g->Nl;
   data = (float *) malloc( count * sizeof(float) );
   if (!data) {
      printf("Error:  out of memory in get_epa_data\n");
      close(fd);
      return NULL;
   }

   strcpy( species, g->VarName );
/*   printf("EPA get data: t=%d v=%d\n", g->TimeStep+1, g->VarNum+1 );*/

   if (!model_get_data( fd, g->TimeStep+1, g->VarNum+1, data,
                       species, units, message)) {
      printf("Error in get_epa_data:  %s\n", message );
      free(data);
      close(fd);
      return NULL;
   }

/*
   printf("begin\n");
   printf("spec: %s   units: %s\n", species, units );
   print_min_max( data, g->Nr * g->Nc * g->Nl );
*/

   if (model_type==RADM) {
      /* multiply all values by 1000.0 */
      for (i=0;i<count;i++) {
         data[i] *= 1000.0;
      }
   }

   /* flip north/south */
   for (i=0;i<g->Nl;i++) {
      flip_north_south( data + i*g->Nr*g->Nc, g->Nr, g->Nc );
   }

   if (model_type == MM4) {
      /* flip top/bottom */
      int size = g->Nr * g->Nc * sizeof(float);
      float *temp = (float *) malloc( size );
      for (i=0; i<g->Nl/2 ;i++) {
         memcpy( temp, data + i*g->Nr*g->Nc, size );
         memcpy( data + i*g->Nr*g->Nc, data + (g->Nl-1-i)*g->Nr*g->Nc, size );
         memcpy( data + (g->Nl-1-i)*g->Nr*g->Nc, temp, size );
      }
      free(temp);
   }

   /* change from row-major to column-major order */
   for (i=0; i<g->Nl; i++) {
      transpose( data + i * g->Nr * g->Nc,
                 data + i * g->Nr * g->Nc,
                 g->Nr, g->Nc );
   }

/*   print_min_max( data, g->Nr * g->Nc * g->Nl );*/

   close(fd);
   return data;
#else
   return NULL;
#endif
}
