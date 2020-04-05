/* extmain.c */
/* Vis5D version 5.2 */

/*
Vis5D system for visualizing five dimensional gridded data sets
Copyright (C) 1990 - 2000 Bill Hibbard, Johan Kellum, Brian Paul,
Dave Santek, and Andre Battaiola.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/* This is the wrapper for Vis5D external analysis functions. */



#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>     /* must be before sys/resource.h */
#include <sys/resource.h>
#include <sys/socket.h>
#include <sys/un.h>
#include "socketio.h"
#include "v5d.h"      /* To get MAXTIMES, etc */



static int Nr, Nc, Nl[MAXLEVELS], LowLev[MAXLEVELS], MaxNl;
static int NumTimes, NumVars;

static int Projection, Vertical;
static float Proj_Args[100], Vert_Args[MAXLEVELS];

static float ProbeRow, ProbeCol, ProbeLev;
static float ProbeLat, ProbeLon, ProbeHgt;
static float ProbeValue[MAXVARS];

#define NUMARGS 5
static float Argument[NUMARGS]; /* only use is commented out;
                                   argument function not called */


/* Define this if using McIDAS library */
#define LIB_MCIDAS



/*
 * Create a socket with the given Unix-domain name.
 * Input:  socket name
 * Return: socket number or -1 if error.
 */
static int create_socket( char *name )
{
   struct sockaddr_un addr;
   int len, sock;
   int tries;

   /* create socket */
   sock = socket( PF_UNIX, SOCK_STREAM, 0 );
   if (sock<0) {
      perror("function creating socket");
      return -1;
   }

   /* try to connect socket to given name */
   /*printf("EXTMAIN: creating socket named: %s\n", name );*/
   strcpy( addr.sun_path, name );
   addr.sun_family = AF_UNIX;
   len = strlen(addr.sun_path)+sizeof(addr.sun_family);
   for (tries=0;tries<5;tries++) {
      if (connect(sock, (struct sockaddr *) &addr, len) < 0) {
         perror("External Function Error: function connect failed");
         /*printf("   failed on try %d\n", tries );*/
      }
      else {
         /* success !*/
         return sock;
      }
   }

   /* error */
   return -1;
}




/*** call_user_function ***********************************************
   Get arguments from Vis5D, compute new grid data by calling user
   function and return results back to Vis5D.
   Input: sock - socket number
   Return:  1 = OK, 0 = error.
**********************************************************************/
static int call_user_function( int sock )
{
   int it, iv, i;
   float *ingrid, *outgrid;
   char names[MAXVARS][8];
   int error_flag;
   int outnl, outlowlev;

   /*** First we get the parameter which won't change for each timestep ***/

   /* get number of time steps and variables */
   receive_int( sock, &NumTimes );
   receive_int( sock, &NumVars );

   /*printf("EXTMAIN: times:%d vars:%d\n", NumTimes, NumVars);*/

   /* Receive size of grids */
   receive_int( sock, &Nr );
   receive_int( sock, &Nc );
   MaxNl = 0;
   for (iv=0;iv<NumVars;iv++) {
      receive_int( sock, &Nl[iv] );
      if (Nl[iv]>MaxNl) {
         MaxNl = Nl[iv];
      }
   }
   for (iv=0;iv<NumVars;iv++) {
      receive_int( sock, &LowLev[iv] );
   }

   /* Receive variable names */
   for (iv=0;iv<NumVars;iv++) {
      int i, j;
      char name[100];
      receive_str( sock, name );
      /* copy name into names[iv] */
      for (i=0;i<8 && name[i];i++)
        names[iv][i] = name[i];
      for (j=i;j<8;j++)
        names[iv][j] = ' ';
   }

   /* Receive map proj and vertical coord sys info */
   receive_int( sock, &Projection );
   for (i=0;i<100;i++) {
      receive_float( sock, &Proj_Args[i] );
   }
   receive_int( sock, &Vertical );
   for (i=0;i<MAXLEVELS;i++) {
      receive_float( sock, &Vert_Args[i] );
   }

   /* Receive probe location */
   receive_float( sock, &ProbeRow );
   receive_float( sock, &ProbeCol );
   receive_float( sock, &ProbeLev );
   receive_float( sock, &ProbeLat );
   receive_float( sock, &ProbeLon );
   receive_float( sock, &ProbeHgt );

   /* Receive user arguments */
/*
   for (i=0;i<NUMARGS;i++) {
      receive_float( sock, &Argument[i] );
   }
*/

   /* allocate dynamic arrays */
   /*printf("EXTMAIN: Nr=%d Nc=%d Nl=%d NumVars=%d\n", Nr, Nc, Nl, NumVars );*/
   ingrid = (float *) malloc( Nr*Nc*MaxNl*NumVars*sizeof(float) );
   outgrid = (float *) malloc( Nr*Nc*MaxNl*sizeof(float) );
   if (!ingrid || !outgrid) {
      printf("External Function Error: out of memory\n");
      send_int( sock, -1 );
      return 0;
   }

   /* the user function will probably be called more than once.... */
   while (1) {
      int mcfile, mcgrid;
      int date, time;
      int error;

      /* get which timestep to compute */
      receive_int( sock, &it );
      /*printf("EXTMAIN: it=%d\n",it);*/
      if (it<0) {
         /* all done */
         break;
      }

      printf("Computing time step %d\n", it );

      /*** Now receive parameters which change for each timestep */

      /* receive date and time info */
      receive_int( sock, &date );
      /*printf("EXTMAIN: date = %d\n", date );*/
      receive_int( sock, &time );
      /*printf("EXTMAIN: time = %d\n", time );*/

      /* Receive Probe values */
      for (iv=0;iv<NumVars;iv++) {
         receive_float( sock, &ProbeValue[iv] );
      }

      /*printf("EXTMAIN: foo\n");*/

      /*** get the ingrid data ***/
      error_flag = 0;
      for (iv=0;iv<NumVars;iv++) {

         receive_int( sock, &mcfile );
         receive_int( sock, &mcgrid );
/*         printf("EXTMAIN: file = %d\n", mcfile );*/
/*         printf("EXTMAIN: grid = %d\n", mcgrid );*/

         if (mcfile==0 && mcgrid==0) {
            /* Vis5D is sending us the grid data. */
            receive_data( sock, ingrid+iv*Nr*Nc*MaxNl,
                          Nr*Nc*Nl[iv]*sizeof(float) );
         }
         else {
            /* read grid data from the original McIDAS grid file. */
            if (!get_mcgrid( mcfile, mcgrid, ingrid+iv*Nr*Nc*Nl[iv], iv )) {
               /* error */
               printf("External Function Error: Couldn't read GR3D%04d, grid %d\n",
                      mcfile, mcgrid );
               error_flag = 1;
            }
         }
      }

      /*** call user function ***/
      if (!error_flag) {
         /*printf("EXTMAIN: Calling user function\n");*/
#ifdef UNDERSCORE

         error = userfunc_( outgrid, &outnl, &outlowlev,
                            ingrid, &Nr, &Nc, Nl, LowLev, &MaxNl, &NumVars, names,
                            &date, &time,
                            &Projection, Proj_Args, &Vertical, Vert_Args );
/*
                            &south, &west,
                            &deltalat, &deltalon, height );
*/
#else
         error = userfunc( outgrid, &outnl, &outlowlev,
                           ingrid, &Nr, &Nc, Nl, LowLev, &MaxNl, &NumVars, names,
                           &date, &time,
                           &Projection, Proj_Args, &Vertical, Vert_Args );
/*                           
 &south, &west,
                           &deltalat, &deltalon, height );
*/
#endif
         if (error!=0) {
            printf("Error in user function, return code was: %d\n", error );
         }
      }
      else {
         error = 1;
      }

/*      printf("EXTMAIN: error=%d\n", error );*/

      /* Return the error/ok code */
      send_int( sock, error );

      /* Return the results of the computation to Vis5D */
      if (error==0) {
         /* send back the new grid */
/*         printf("OutNl = %d\n", outnl );*/
         if (outnl>MaxNl)  outnl = MaxNl;
         send_int( sock, outnl );
         send_int( sock, outlowlev );
         /* we used to return compressed data...oh well */
         send_data( sock, outgrid, Nr*Nc*outnl*sizeof(float) );
      }

   }

   free(ingrid);
   free(outgrid);
   return 1;
}




/*
 *
 * main()
 *
 */
#ifdef LIB_MCIDAS
   extern int Argc;
   extern char **Argvector;
#  ifdef UNDERSCORE
      main0_()
#  else
      main0()
#  endif
   {
      int argc = Argc;
      char **argv = Argvector;
#else
   /* Not using McIDAS lib */
   main( int argc, char *argv[] )
   {
#endif
   int sock;

   if (argc!=2) {
      printf("Parameter Error: no socket name.\n");
      printf("This is a Vis5D external function.  It can only be used\n");
      printf("via vis5d.\n");
      exit(1);
   }

   sleep(1);

   sock = create_socket( argv[1] );
   if (sock>=0) {
      /* send an acknowledgment signal */
      send_int( sock, 0x1234 );
      /* call user function */
      call_user_function( sock );
      /* finish up */
      close(sock);
   }

   return 0;
}



/*** get_mcgrid *******************************************************
   Get a McIDAS grid.
   Input: file - number of the McIDAS grid file.
          grid - which grid in the file.
          data - pointer to buffer to put data.
          var - which variable this grid is.
   Output: data - read from the file.
   Return: 1 = ok, 0 = error.
**********************************************************************/
int get_mcgrid( int file, int grid, float *data, int var )
{
   int result, max, nr, nc, nl, table[64];

   printf("Getting McIDAS grid %d %d 0x%x\n", file, grid, data );
   if (file==0 && grid==0) {
      memset( data, 0, Nr*Nc*MaxNl*sizeof(float) );
      printf("Filling grid with zeros\n");
   }
   else {
      max = Nr*Nc*MaxNl;
      memset( data, 0, Nr*Nc*MaxNl*sizeof(float) );
#ifdef UNDERSCORE
      result = iggt3d_( &file, &grid, &max, data, &nr, &nc, &nl, table );
#else
      result = iggt3d( &file, &grid, &max, data, &nr, &nc, &nl, table );
#endif

      /*printf("result=%d nr=%d nc=%d nl=%d\n", result, nr, nc, nl );*/
      if (result!=0 || nr!=Nr || nc!=Nc || nl!=Nl[var]) {
         printf("External Function Error: get_mcgrid error: result=%d\n", result);
         return 0;
      }
   }
   return 1;
}



void pf_( float *x )
{
   printf("%g\n", *x);
}



/*
 * Return the current probe position in (row,column,level) and
 * (latitude, longitude, altitude km).
 */
#ifdef UNDERSCORE
void probepos_( float *row, float *col, float *lev,
                float *lat, float *lon, float *alt )
#else
void probepos( float *row, float *col, float *lev,
               float *lat, float *lon, float *alt )
#endif
{
   *row = ProbeRow + 1.0;
   *col = ProbeCol + 1.0;
   *lev = ProbeLev + 1.0;
   *lat = ProbeLat;
   *lon = ProbeLon;
   *alt = ProbeHgt;
}



/*
 * Return the value of a variable at the probe's location.
 * Input:  var - which variable  [1..NumVars]
 */
#ifdef UNDERSCORE
float probeval_( int *var )
#else
float probeval( int *var )
#endif
{
   if (*var>0 && *var<=NumVars) {
      return ProbeValue[ *var-1 ];
   }
   else {
      return 1.0e30;   /* MISSING VALUE */
   }
}



/*
 * Return the value of a user-specified argument.  The argument(s) are
 * entered by the user into a GUI form when invoking the function.
 * Input:  n - an argument number in [1..]
 * Return:  the value of the argument or 0 if n is bad.
 */
#ifdef UNDERSCORE
float argument_( int *n )
#else
float argument( int *n )
#endif
{
   if (*n<=0 || *n>NUMARGS) {
      return 0.0;
   }
   else {
      return Argument[*n-1];
   }
}

