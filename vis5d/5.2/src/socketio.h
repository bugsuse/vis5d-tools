
/* Vis5D version 5.2 */

/*
Vis5D system for visualizing five dimensional gridded data sets
Copyright (C) 1990 - 2000 Bill Hibbard, Brian Paul, Dave Santek,
and Andre Battaiola.

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



/* socket communications module */


#ifndef SOCKETIO_H
#define SOCKETIO_H




/*** receive_data *****************************************************
   Receive a block of data by repeatedly reading from a socket.  This
   function will block the process if data is not available.
   Input:  socket - socket to read from
            buffer - pointer to data buffer
           bytes - how many bytes to read into buffer
   Return:  number of bytes received.  This will always either be
            the same as the bytes requested or 0 if the connection
            was lost.
**********************************************************************/
extern int receive_data( int socket, void *buffer, int bytes );



/*** send_data ********************************************************
   Send a block of data by repeatedly writing to a socket.
   Input:  socket - socket to write to
           buffer - pointer to data to send
           bytes - number of bytes to send from buffer
**********************************************************************/
extern void send_data( int socket, void *buffer, int bytes );



/*** receive_int ******************************************************
   Receive a 32-bit integer through a socket.  This function will
   block if the data is not available.
   Input:  socket - the socket to receive the integer from.
           i - pointer to integer to put result into.
   Output:  i will have the value received
   Return:  0 if the connection was lost
            non-zero if successful
**********************************************************************/
extern int receive_int( int socket, int *i );



/*** send_int *********************************************************
   Send a 32-bit integer through a socket.
   Input:  socket - the socket to send on.
           i - the integer to send
   Output:  none.
**********************************************************************/
extern void send_int( int socket, int i );



/*** receive_float ****************************************************
   Receive a 32-bit float through a socket.  This function will
   block if the data is not available.
   Input:  socket - the socket to receive the integer from.
           x - pointer to float to put result into.
   Output:  i will have the value received
   Return:  0 if the connection was lost
            non-zero if successful
**********************************************************************/
extern int receive_float( int socket, float *x );



/*** send_float *********************************************************
   Send a 32-bit float through a socket.
   Input:  socket - the socket to send on.
           x - the float to send
   Output:  none.
**********************************************************************/
extern void send_float( int socket, float x );



/*** receive_str ******************************************************
   Receive a character string.  This function will block if the data
   is not available.
   Input:  socket - the socket to receive the string from.
           str - pointer to char array to put result into.  It is
                 assumed to be of sufficient size.
   Output:  str will have the string received
   Return:  0 if the connection was lost
            non-zero if successful
**********************************************************************/
extern int receive_str( int socket, char str[] );



/*** send_str *********************************************************
   Send a character string through a socket.
   Input:  socket - the socket to send on.
           str - the string to send
   Output:  none.
**********************************************************************/
extern void send_str( int socket, char str[] );


#endif
