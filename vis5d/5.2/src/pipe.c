/* pipe.c */

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

Yo u should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/


#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
/* #include <bstring.h> */
#include <errno.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "gui.h"

static FILE* f = NULL;

  void check_pipe(char *pipe_name) {
    fd_set fdset;
    struct timeval timeout = {0, 10};
    int i;
    int fd;

    FD_ZERO(&fdset);

    if (f == NULL) {
/* blocks
      f = fopen(pipe_name,"r");
*/
      fd = open(pipe_name, O_RDONLY | O_NONBLOCK);
      f = fdopen(fd,"r");
      if(f == NULL) {
        perror(pipe_name);
        return;
      }
    }
    
    for(;;) {
      if(f != NULL) FD_SET(fileno(f), &fdset);
      switch(select(FD_SETSIZE, &fdset, NULL, NULL, &timeout)) {

        case -1:
          if(errno != EINTR) {
            perror("select");
            exit(1);
          }
          return;
          
        case 0:
          /* printf("timeout\n"); */
          return;
          
        default:
          if(f != NULL && FD_ISSET(fileno(f),&fdset)) {
            char line[1024];

            if(fgets(line,sizeof(line),f) == NULL) {
              struct stat s;
              /* EOF reached, if f was a pipe, reopen it */
              /* printf("EOF, reopening pipe"); */
              /* f = fopen(pipe_name,"r"); */
              /* printf("EOF, %s\n",pipe_name); */
              fclose(f);
              f = NULL;
              return;         
/* always re-open on call to check_pipe
              if(stat(pipe_name,&s) == -1) {
                perror(pipe_name);
                exit(1);
              }
          
              if(S_ISFIFO(s.st_mode)) {
                printf("pipe, %s\n",pipe_name);
                f = fopen(pipe_name,"r");
              }
*/
            }
            else {
              int index;
              int len = strlen(line);
              /* len = strlen(line); */
              if (len > 0) line[len-1] = 0;
/* WLH 6 Nov 98
              printf("%s: %s\n",pipe_name,line);
*/
              get_current_display(&index);
              run_script(index, line);
            }
          }
          else {
            /* printf("select default not FD_ISSET, %s\n",pipe_name); */
            return;
          }
      } /* end switch(select(FD_SETSIZE,&fdset,NULL,NULL,&timeout)) */
    } /* end for (;;) */
  }

