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

/* chrono.c */


#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "api.h"
#include "globals.h"
#include "chrono.h"
#include "vis5d.h"

int not_dup_timestep( Display_Group grp, int tcount)
{
   int icr, dex;
   int different;

   for (icr = 0; icr < tcount; icr++){
      different = 0;
      for (dex = 0; dex < grp->numofdpys; dex++){
         if (grp->TimeStep[tcount].ownerstimestep[dex] !=
             grp->TimeStep[icr].ownerstimestep[dex]){
            different = 1;
         }
      }
      if (!different){/* found a similar TimeStep */
         return 0;
      }
   }
   /* found NO similar timesteps */
   return 1;
}

void calculate_group_time_steps( Display_Group grp )
{
   int erly_day, erly_sec;
   int grpday, grpsec;
   int yo, spandex;
   int tmaxsec, tmaxday;
   int maxsec, maxday, best_day, best_sec;
   int dpy_loop, tyme, abs_sec, abs_day;
   int tempday, tempsec, closest_tyme, timecount;
   int netcolumn, dpy_numtimes[VIS5D_MAX_CONTEXTS];
   int grp_time_position[VIS5D_MAX_DPY_CONTEXTS];

   if (grp->numofdpys < 1){
      return;
   }

   /********************************************/
   /* initialize the dpy_time_postions to zero */
   /********************************************/
   for (yo=0; yo < VIS5D_MAX_DPY_CONTEXTS; yo++){
      grp_time_position[yo] = 0;
   }

   /******************************************/
   /* initialize numtimes for each vis5d dpy */
   /******************************************/
   maxday = maxsec = -1;
   for (yo=0; yo < grp->numofdpys; yo++){
      int tmaxday, tmaxsex;
      spandex = grp->dpyarray[yo]->dpy_context_index;
      vis5d_get_dtx_numtimes( spandex, &dpy_numtimes[spandex]);
      vis5d_get_dtx_time_stamp( spandex, dpy_numtimes[spandex]-1, &tmaxday, &tmaxsec);

      if ((tmaxday > maxday) || (tmaxday == maxday && tmaxsec > maxsec)){
         maxday = tmaxday;
         maxsec = tmaxsec;
      }
   }

   erly_day = erly_sec = 100000000;
   timecount = 0;

   netcolumn = -1069;
   /*******************************/
   /* do the main organizing loop */
   /*******************************/
   while(1){
      int thisone;
      /**********************************************/
      /* find the next earliest time stamp/step  NET*/
      /**********************************************/
      erly_day = erly_sec = 10000000;
      for (yo=0; yo < grp->numofdpys; yo++){
         spandex = grp->dpyarray[yo]->dpy_context_index;
         vis5d_get_dtx_time_stamp( spandex, grp_time_position[spandex],
                               &grpday, &grpsec);
         if((grpday < erly_day) || (( grpday== erly_day) && (grpsec < erly_sec))){
            if (grp_time_position[spandex] < dpy_numtimes[spandex]){
               erly_day = grpday;
               erly_sec = grpsec;
               netcolumn = yo;
               thisone = spandex;
            }
         }
      } /* erly_day/erly_sec should now be the NET */
      grp_time_position[thisone]++;
      if (netcolumn==-1069){
         grp->NumTimes = 1;
         return;
      }
      /* check for similar time stamps */
      for(yo=0; yo < grp->numofdpys; yo++){
         spandex = grp->dpyarray[yo]->dpy_context_index;
         vis5d_get_dtx_time_stamp( spandex, grp_time_position[spandex],
                               &grpday, &grpsec);
         if (grpday == erly_day && grpsec == erly_sec &&
             spandex != thisone ){
            grp_time_position[spandex]++;
         }
      }
      if (erly_day == maxday && erly_sec == maxsec){
         /***********************************/
         /* make last time step, then done! */
         /***********************************/
         for (yo = 0; yo < grp->numofdpys; yo++){
            int what;
            spandex = grp->dpyarray[yo]->dpy_context_index;
            grp->TimeStep[timecount].owners[yo] = spandex;
            grp->TimeStep[timecount].ownerstimestep[yo] = dpy_numtimes[spandex]-1;
         }
         if ( not_dup_timestep( grp, timecount) ){
            grp->NumTimes = timecount + 1;
         }
         else{
            grp->NumTimes = timecount;
         }
         /*{
            int dyo;
            for (dyo = 0; dyo < timecount; dyo++){
               for (yo = 0; yo < grp->numofdpys; yo++){
                  printf(" Timestep=%d owner%d=%d ownerstimestep%d=%d \n\n",
                      dyo, yo, grp->TimeStep[dyo].owners[yo],
                      yo, grp->TimeStep[dyo].ownerstimestep[yo]);
               }
            }
         }
         */  
         return;
      }
      
      /* now that we have  the Next Earliest Timestep */
      /* pretend to put a slide ruler at that location in time, then */
      /* loop through all the displays and find the timestep in that */
      /* display which is the closest to the imaginary time marker */
      /* if this time marker is exactly in between two timesteps */
      /* of a different display, choose the timestep which is in the future */

      for (dpy_loop = 0; dpy_loop < grp->numofdpys; dpy_loop++){
         spandex = grp->dpyarray[dpy_loop]->dpy_context_index;
         /*********************************************************/         
         /* now loop through the timesteps in this display        */
         /* to see which one is closest to the time marker or NET */
         /*********************************************************/
         closest_tyme = -7;
         best_day = best_sec = 10000000;
         for (tyme = 0; tyme < dpy_numtimes[spandex]; tyme++){
            vis5d_get_dtx_time_stamp( spandex, tyme, &tempday, &tempsec);
            /**************************************************/
            /* get the absolut value of the difference in the */
            /* two times, temp_time and erly_time             */
            /**************************************************/
            if (tempday < erly_day || (tempday == erly_day && tempsec < erly_sec)){
               /* temptime is in the past */
               if (tempsec > erly_sec){
                  abs_sec = 86400 - tempsec + erly_sec;
                  abs_day = erly_day - tempday - 1;
               }
               else if (tempsec <= erly_sec){
                  abs_sec = erly_sec - tempsec;    
                  abs_day = erly_day - tempday;
               }
            }
            else if (tempday > erly_day || (tempday == erly_day && tempsec >erly_sec)){
               /* temptime is in the future */
               if (tempsec >= erly_sec){
                  abs_sec = tempsec - erly_sec;
                  abs_day = tempday - erly_day;
               }
               else if (tempsec < erly_sec){
                  abs_sec = 86400 - erly_sec + tempsec;
                  abs_day = tempday - erly_day - 1;
               } 
            }
            else{
               /* temptime is in the present */
               abs_sec = 0;
               abs_day = 0;
            }
            
            if( abs_day < best_day || ( abs_day == best_day && abs_sec <= best_sec)){
               closest_tyme = tyme;
               best_day = abs_day;
               best_sec = abs_sec;
            }
         }
         grp->TimeStep[timecount].owners[dpy_loop] = spandex;
         grp->TimeStep[timecount].ownerstimestep[dpy_loop] = closest_tyme;
      }
      if ( not_dup_timestep( grp, timecount) ){
         timecount++;
      }
   }
}

