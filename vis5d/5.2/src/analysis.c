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


#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/un.h>
#include <unistd.h>
#include "analysis.h"
#include "globals.h"
#include "grid.h"
#include "memory.h"
#include "proj.h"
#include "queue.h"
#include "socketio.h"


#define SOCK_NAME "/tmp/VIS-5D_socket"

#define TEMP_FILE "/tmp/VIS-5D_temp"



/*
 * Search for external Fortan analysis functions and return a list of
 * them.
 *
 * The search is done as follows:
 *     Get a directory listing of all files suffixed with .f
 *     For each file 'foo.f' in the directory {
 *          If there is a executable file named 'foo' add it to the list.
 *     }
 *  Input:  path - directory path to search
 * In/Out:  FuncName - array of function names
 * Return:  Number of functions found.
 */
int find_analysis_functions( char *path, char FuncName[][1000] )
{
   char command[1000];
   char fname[1000], fname2[1000];
   FILE *f, *g;
   int len;
   int numfuncs;

   numfuncs = 0;

   /* put a directory listing into a temporary file */
   sprintf( command, "ls > %s %s/*.f", TEMP_FILE, path );
   system( command );

   f = fopen( TEMP_FILE, "r" );
   if (f) {
      while (fgets(fname, 1000, f) && numfuncs<MAX_FUNCS) {
         /* truncate .f\n from each name */
         len = strlen(fname);
         if (len>3) {
            fname[len-3] = '\0';
            /* now see if executable function exists */
            g = fopen( fname, "r" );
            if (g) {
               /* remove path prefix from name */
               strcpy( fname2, fname+strlen(path)+1 );
               strcpy( FuncName[numfuncs], fname2 );
               numfuncs++;
               fclose(g);
            }
         }
      }
      fclose(f);
   }

   /* rm the temp file */
   unlink( TEMP_FILE );

   return numfuncs;
}





/*** unlink_socket_name ***********************************************
   Delete the named socket from the filesystem.  The reason we can't
   just use unlink() is because the IBM RS/6000 for some unknown reason
   truncates the socket filename by 1 character after it has been bound
   to its socket (dumb)!
**********************************************************************/
static int unlink_socket_name( char *name )
{
#ifdef ibm
   char name2[1000];

   strcpy( name2, name );
   name2[ strlen(name2)-1 ] = '\0';
   return unlink(name2);
#else
   return unlink(name);
#endif
}



/*** start_external_function ******************************************
   Start a new process (i.e. begin executing the named program)
   and return a socket number to use for communicating with it.
   Input:  progname - name of executable file to run.
   Return:  socket number to use for communication with the new
            process or -1 if error.
**********************************************************************/
static int start_external_function( Context ctx, char *progname )
{
   int s, sock, len;
   struct sockaddr_un addr, from;
   char command[1000];
   int l;

   /* first delete the socket's name if it exists */
   unlink_socket_name( SOCK_NAME );

   /* make the socket */
   s = socket( PF_UNIX, SOCK_STREAM, 0 );
   if (s<0) {
      perror("External Function Error: Couldn't create socket:");
      return -1;
   }

   /* bind the socket to its name */
   strcpy( addr.sun_path, SOCK_NAME );
   addr.sun_family = AF_UNIX;
   if ( bind( s, (struct sockaddr *) &addr,
              strlen(addr.sun_path)+sizeof(addr.sun_family) ) < 0 ) {
      perror("External Function Error: Couldn't bind socket to name:");
      return -1;
   }

   /* change permissions on the socket name in case it has to be rm'd later */
   chmod( SOCK_NAME, 0666 );  /* allow read/write by all */

   /* Start the external function via system() call */
   sprintf( command, "%s %s &", progname, SOCK_NAME );
   system( command );

   /* wait for external function to connect */
   l=listen( s, 20 );
   if (l!=0) {
        perror("External Function Error: Listen failed");
   }
   len = sizeof(from);
   if ( (sock = accept( s, (struct sockaddr *) &from, &len )) < 0) {
      perror("External Function Error: Accept failed");
      return -1;
   }

   close(s);  /* don't need this socket anymore */

   return sock;
}




/*** stop_external_function *******************************************
   Do any cleaning up that's necessary after the external function
   is done such as deleting the socket name and closing the socket.
   Input:  progname - name of executable program.
           sock - the socket used to communicate with ext program.
   Return:  nothing
**********************************************************************/
static int stop_external_function( char *progname, int sock )
{
   if (unlink_socket_name( SOCK_NAME )==-1) {
      /*perror("unlink failed [2]");*/
   };
   close( sock );
   return 0;
}



/*** compute_analysis_variable ****************************************
   Make a new variable which is computed from an external analysis
   function.
   Input: var - number of the variable we're computing.
          prognam - name of the executable program to call.
   Return:  1 if success.
            0 if error.
**********************************************************************/
int compute_analysis_variable( Context ctx, int var, char *progname )
{
   int sock, ack, time, iv, np, t, i, instances;
   float proj_args[100], vert_args[MAXLEVELS];

   /* Clear the error variable */
   ctx->ExtFuncErrorFlag = 0;

   if (NumThreads<=1)
      instances = 1;
   else
      instances = NumThreads - 1;

   /* We start a copy of the external function for each background thread */
   for (t=1;t<=instances;t++) {
      printf("Starting instance %d of %s\n", t, progname );
      sock = start_external_function( ctx, progname );
      if (sock<0) {
         /* there was an error, clean up and return 0 */
         for (i=1;i<t;i++) {
            stop_external_function( progname, ctx->ExtFuncSocket[i] );
         }
         return 0;
      }
      ctx->ExtFuncSocket[t] = sock;

      /* wait for an acknowledgment */
      receive_int( sock, &ack );

      /*
       * Send data which is invariant over all time steps...
       */

      /* send number of timesteps */
      send_int( sock, ctx->NumTimes );

      /* Number of variables:  all up to the one we're making */
      np = var;
      send_int( sock, np );    /* Number of variables */

      /* Send size of grids */
      send_int( sock, ctx->Nr );
      send_int( sock, ctx->Nc );
      /* Send the number of levels of every parameter */
      for (iv=0; iv<np; iv++) {
         send_int( sock, ctx->Nl[iv] );
      }
      /* Send the lowest level of every parameter */
      for (iv=0; iv<np; iv++) {
         send_int( sock, ctx->LowLev[iv] );
      }

      /* Send variable names */
      for (iv=0;iv<np;iv++) {
         send_str( sock, ctx->VarName[iv] );
      }

      /* Send map proj and vert coord sys info */
      send_int( sock, ctx->Projection );
      switch (ctx->Projection) {
         case PROJ_GENERIC:
         case PROJ_LINEAR:
            proj_args[0] = ctx->NorthBound;
            proj_args[1] = ctx->WestBound;
            proj_args[2] = ctx->RowInc;
            proj_args[3] = ctx->ColInc;
            break;
         case PROJ_MERCATOR:
            proj_args[0] = ctx->CentralLat;
            proj_args[1] = ctx->CentralLon;        
            proj_args[2] = ctx->RowIncKm;        
            proj_args[3] = ctx->ColIncKm;        
         case PROJ_ROTATED:
            /* WLH 4-21-95 */
            proj_args[0] = ctx->NorthBound;
            proj_args[1] = ctx->WestBound;
            proj_args[2] = ctx->RowInc;
            proj_args[3] = ctx->ColInc;
            proj_args[4] = ctx->CentralLat;
            proj_args[5] = ctx->CentralLon;
            proj_args[6] = ctx->Rotation;
            break;
         case PROJ_LAMBERT:
            proj_args[0] = ctx->Lat1;
            proj_args[1] = ctx->Lat2;
            proj_args[2] = ctx->PoleRow;
            proj_args[3] = ctx->PoleCol;
            proj_args[4] = ctx->CentralLon;
            proj_args[5] = ctx->ColInc;
            break;
         case PROJ_STEREO:
            proj_args[0] = ctx->CentralLat;
            proj_args[1] = ctx->CentralLon;
            proj_args[2] = ctx->CentralRow;
            proj_args[3] = ctx->CentralCol;
            proj_args[4] = ctx->ColInc;
            break;
      }
      for (i=0;i<100;i++) {
         send_float( sock, proj_args[i] );
      }
      send_int( sock, ctx->VerticalSystem );
      switch (ctx->VerticalSystem) {
         case VERT_GENERIC:
         case VERT_EQUAL_KM:
            vert_args[0] = ctx->BottomBound;
            vert_args[1] = ctx->LevInc;
            break;
         case VERT_NONEQUAL_KM:
         case VERT_NONEQUAL_MB:
            for (i=0;i<ctx->MaxNl;i++) {
               vert_args[i] = ctx->Height[i];
            }
            break;
      }
      for (i=0;i<MAXLEVELS;i++) {
         send_float( sock, vert_args[i] );
      }

      /* Send probe location */
      {
         float row, col, lev;
         float lat, lon, hgt;

         xyz_to_grid( ctx, ctx->dpy_ctx->CurTime, 0, ctx->dpy_ctx->CursorX, ctx->dpy_ctx->CursorY,
                      ctx->dpy_ctx->CursorZ, &row, &col, &lev);
         xyz_to_geo( ctx, ctx->dpy_ctx->CurTime, 0, ctx->dpy_ctx->CursorX, ctx->dpy_ctx->CursorY,
                     ctx->dpy_ctx->CursorZ, &lat, &lon, &hgt );

         send_float( sock, row );
         send_float( sock, col );
         send_float( sock, lev );
         send_float( sock, lat );
         send_float( sock, lon );
         send_float( sock, hgt );
         ctx->ProbeRow = row;
         ctx->ProbeCol = col;
         ctx->ProbeLev = lev;
      }

      /* Send user arguments */
/*
      for (i=0;i<NUMARG;i++) {
         send_float( sock, &Argument[i] );
      }
*/

   }

   /*
    * The external function(s) are now started.  Put a request in the
    * job queue to compute the external function for each time step.
    */
   for (time=0;time<ctx->NumTimes;time++) {
      request_ext_func( ctx, time, var );
   }

   /* Now wait for a signal that the grid for the last timestep
    * has been computed.
    */
   WAIT_SEM( ctx->ExtFuncDoneSem );

   /* Stop the external function(s) */
   for (t=1;t<=instances;t++) {
      send_int( ctx->ExtFuncSocket[t], -1 );  /* send bad timestep number */
      stop_external_function( progname, ctx->ExtFuncSocket[t] );
   }

   if (ctx->ExtFuncErrorFlag)
     return 0;
   else
     return 1;
}



/*
 * Call the currently running external function to compute a new grid
 * for a particular time step.  This function is called from work.c
 *
 * Input:  time - which timestep
 *         var - which variable we're computing
 *         threadnum - thread number in [1..NumThreads-1]
 * Return:  1 = ok, 0 = error.
 */
int calc_ext_func( Context ctx, int time, int var, int threadnum )
{
   int sock, iv, np, error;

   sock = ctx->ExtFuncSocket[threadnum];

   send_int( sock, time );
   /*printf("sending day: %d\n", ctx->DayStamp[time] );*/
   send_int( sock, ctx->DayStamp[time] );
   /*printf("sending time: %d\n", ctx->TimeStamp[time] );*/
   send_int( sock, ctx->TimeStamp[time] );

   /* Number of variables:  all up to the one we're computing */
   np = var;

   /* Send Probe values */
   for (iv=0;iv<np;iv++) {
      float value = interpolate_grid_value( ctx, time, iv,
                      ctx->ProbeRow, ctx->ProbeCol, ctx->ProbeLev );
      send_float( sock, value );
   }

   /*printf("np=%d\n", np );*/

   for (iv=0;iv<np;iv++) {
      /*printf("sending file number: %d\n", ctx->McFile[time][iv] );*/
      send_int( sock, ctx->McFile[time][iv] );
      /*printf("sending grid number: %d\n", ctx->McGrid[time][iv] );*/
      send_int( sock, ctx->McGrid[time][iv] );
      if (ctx->McFile[time][iv]==0 && ctx->McGrid[time][iv]==0) {
         /* Original McIDAS file data not available, so send */
         /* uncompressed data even though it's not too accurate */
         float *g;
         g = get_grid( ctx, time, iv );
         /* Send variable sized grids */
         send_data( sock, g, ctx->Nr*ctx->Nc*ctx->Nl[iv]*sizeof(float) );
         release_grid( ctx, time, iv, g );
      }
   }

   /* >>> At this point, the user's analysis function is running. <<< */

   /*printf("[%d]Waiting for return code\n", threadnum );*/
   receive_int( sock, &error );  /* wait for error code */
   /*printf("[%d]Received return code: %d\n", threadnum, error );*/

   if (error) {
      /* Error, print an error message */
      printf("External function failed: %d\n", error );
      /*stop_external_function( progname, sock );*/
      /* set the error flag */
      ctx->ExtFuncErrorFlag = 1;
   }
   else {
      /* No error, get the resulting grid */
      float *grid;
      int outNl, outLowLev;
      int nbytes;

      /* get number of levels in resulting grid */
      receive_int(sock, &outNl);
      receive_int(sock, &outLowLev);

/*      printf("Received outNl=%d\n", outNl);*/
      if (outNl>ctx->MaxNl)  outNl = ctx->MaxNl;

      ctx->Nl[var] = outNl;
      ctx->LowLev[var] = outLowLev;

      /* allocate space for resulting grid and receive the grid data */
      nbytes = ctx->Nr * ctx->Nc * outNl * sizeof(float);
      grid = (float *) allocate( ctx, nbytes );
      receive_data( sock, grid, nbytes );

      /* install the new grid */
      install_new_grid( ctx, time, var, grid, outNl, outLowLev );

      /* may discard data now */
      deallocate( ctx, grid, nbytes );
   }

   if (time==ctx->NumTimes-1) {
      /* Signal that we've computed the last time step */
      /*printf("Signalling upon last time step\n");*/
      SIGNAL_SEM( ctx->ExtFuncDoneSem );
   }

   if (error)
     return 0;
   else
     return 1;
}
