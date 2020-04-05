/* chrono.c */
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


int not_duplicate_timestep( Display_Context dtx, int tcount)
{
   int icr, dex;
   int different;

   for (icr = 0; icr < tcount; icr++){
      different = 0;
      for (dex = 0; dex < dtx->numofctxs; dex++){
         if (dtx->TimeStep[tcount].ownerstimestep[dex] !=
             dtx->TimeStep[icr].ownerstimestep[dex]){
            different = 1;
         }
      }
      for (dex = dtx->numofctxs; dex < dtx->numofitxs + dtx->numofctxs; dex++){
         if (dtx->TimeStep[tcount].ownerstimestep[dex] !=
             dtx->TimeStep[icr].ownerstimestep[dex]){
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



void create_elapsed_times( Display_Context dtx )
{
   int firstst, firstdt,yo, yikes, lastdt, lastst;
   int daytyme, sectyme;

   lastdt = lastst = -1;
   firstdt = 999999999;
   firstst = 999999999;
   for (yo = 0; yo < dtx->numofctxs; yo++){
      vis5d_get_ctx_time_stamp( dtx->TimeStep[0].owners[yo],
                            dtx->TimeStep[0].ownerstimestep[yo],
                            &daytyme, &sectyme);
      if (daytyme < firstdt && sectyme <= firstst){
         firstdt = daytyme;
         firstst = sectyme;
      }
   }
   for (yo = dtx->numofctxs; yo < dtx->numofitxs; yo++){
      vis5d_get_itx_time_stamp( dtx->TimeStep[0].owners[yo],
                            dtx->TimeStep[0].ownerstimestep[yo],
                            &daytyme, &sectyme);
      if (daytyme < firstdt && sectyme <= firstst){
         firstdt = daytyme;
         firstst = sectyme;
      }
   }

   for (yo = 0; yo < dtx->NumTimes; yo++){
      lastdt = lastst = -1;
      /* Find last timestamp out of all the owners */
      for(yikes = 0; yikes < dtx->numofctxs; yikes++){
         vis5d_get_ctx_time_stamp( dtx->TimeStep[yo].owners[yikes],
                               dtx->TimeStep[yo].ownerstimestep[yikes],
                               &daytyme, &sectyme);
         if (daytyme >= lastdt && sectyme > lastst ){
            lastdt = daytyme;
            lastst = sectyme;
         }
      }
      for(yikes = dtx->numofctxs; yikes < dtx->numofitxs; yikes++){
         vis5d_get_itx_time_stamp( dtx->TimeStep[yo].owners[yikes],
                               dtx->TimeStep[yo].ownerstimestep[yikes],
                               &daytyme, &sectyme);
         if (daytyme >= lastdt && sectyme > lastst ){
            lastdt = daytyme;
            lastst = sectyme;
         }
      }
      if (yo == 0){
         dtx->Elapsed[0]=0;
      }
      else{
         dtx->Elapsed[yo] = ((lastdt-firstdt)*24*60*60) +
                            (lastst-firstst);
      }
     
   }
}


void calculate_display_time_steps( Display_Context dtx )
{
   int erly_day, erly_sec;
   int ctxday, ctxsec;
   int yo, spandex;
   int tmaxsec, tmaxday;
   int maxsec, maxday, best_day, best_sec;
   int ctx_loop, tyme, abs_sec, abs_day;
   int tempday, tempsec, closest_tyme, timecount;
   int netcolumntype, netcolumn, ctx_numtimes[VIS5D_MAX_CONTEXTS];
   int ctx_time_position[VIS5D_MAX_DPY_CONTEXTS];
   int itx_numtimes[VIS5D_MAX_CONTEXTS];
   int itx_time_position[VIS5D_MAX_DPY_CONTEXTS];
   int itxloop, itxday, itxsec;


   /********************************************/
   /* initialize the ctx_time_postions to zero */
   /********************************************/
   for (yo=0; yo < VIS5D_MAX_CONTEXTS; yo++){
      ctx_time_position[yo] = 0;
      itx_time_position[yo] = 0;
   }

   /******************************************/
   /* initialize numtimes for each vis5d ctx */
   /******************************************/
   maxday = maxsec = -1;
   yo = 0;
   for (yo=0; yo < dtx->numofctxs; yo++){
      int tmaxday, tmaxsex;
      spandex = dtx->ctxarray[yo];
      vis5d_get_ctx_numtimes( spandex, &ctx_numtimes[spandex]);
      vis5d_get_ctx_time_stamp( spandex, ctx_numtimes[spandex]-1, &tmaxday, &tmaxsec);
      if ((tmaxday > maxday) || (tmaxday == maxday && tmaxsec > maxsec)){
         maxday = tmaxday;
         maxsec = tmaxsec;
      }
   }
   for (yo=0; yo < dtx->numofitxs; yo++){
      int tmaxday, tmaxsex;
      spandex = dtx->itxarray[yo];
      vis5d_get_itx_numtimes( spandex, &itx_numtimes[spandex]);
      vis5d_get_itx_time_stamp( spandex, itx_numtimes[spandex]-1,
                                 &tmaxday, &tmaxsec);
      if ((tmaxday > maxday) || (tmaxday == maxday && tmaxsec > maxsec)){
         maxday = tmaxday;
         maxsec = tmaxsec;
      }
   }


   erly_day = erly_sec = 10000000;
   timecount = 0;

   spandex = dtx->ctxarray[0];
   if (dtx->numofctxs == 1 && dtx->numofitxs == 0 && ctx_numtimes[spandex] == 1){
      dtx->NumTimes = 1;
      dtx->TimeStep[0].ownertype[0] = REGULAR_TYPE;
      dtx->TimeStep[0].owners[0] = spandex;
      dtx->TimeStep[0].ownerstimestep[0] = 0;
      create_elapsed_times(dtx);
      return;
   }
   spandex = dtx->itxarray[0];
   if (dtx->numofctxs == 0 && dtx->numofitxs == 1 && itx_numtimes[spandex] == 1){
      dtx->NumTimes = 1;
      dtx->TimeStep[0].ownertype[0] = IRREGULAR_TYPE;
      dtx->TimeStep[0].owners[0] = spandex;
      dtx->TimeStep[0].ownerstimestep[0] = 0;
      create_elapsed_times(dtx);
      return;
   }



   /*******************************/
   /* do the main organizing loop */
   /*******************************/
   while(1){
      int thisone;
      /**********************************************/
      /* find the next earliest time stamp/step  NET*/
      /**********************************************/
      erly_day = erly_sec = 10000000;
      /**************/
      /* check ctxs */
      /**************/
      for (yo=0; yo < dtx->numofctxs; yo++){
         spandex = dtx->ctxarray[yo];
         vis5d_get_ctx_time_stamp( spandex, ctx_time_position[spandex],
                               &ctxday, &ctxsec);
         if((ctxday<erly_day) || ((ctxday == erly_day) && (ctxsec <erly_sec))){
            if (ctx_time_position[spandex] < ctx_numtimes[spandex]){ 
               erly_day = ctxday;
               erly_sec = ctxsec;
               netcolumn = yo;
               netcolumntype = REGULAR_TYPE;
               thisone = spandex;
            }
         }
      } /* erly_day/erly_sec should now be the NET */
      /**************/      
      /* check itxs */
      /**************/
      for (yo=0; yo < dtx->numofitxs; yo++){
         spandex = dtx->itxarray[yo];
         vis5d_get_itx_time_stamp( spandex, itx_time_position[spandex],
                               &itxday, &itxsec);
         if((itxday<erly_day) || ((itxday == erly_day) && (itxsec <erly_sec))){
            if (itx_time_position[spandex] < itx_numtimes[spandex]){ 
               erly_day = itxday;
               erly_sec = itxsec;
               netcolumn = yo;
               netcolumntype = IRREGULAR_TYPE;
               thisone = spandex;
            }  
         }  
      } /* erly_day/erly_sec should now be the NET */


      if (netcolumntype == REGULAR_TYPE){
         ctx_time_position[thisone]++;
      }
      else{
         itx_time_position[thisone]++;
      }

      /*********************************/      
      /* check for similar time stamps */
      /*********************************/
      /**************/
      /* check ctxs */
      /**************/
      for(yo=0; yo < dtx->numofctxs; yo++){
         spandex = dtx->ctxarray[yo];
         vis5d_get_ctx_time_stamp( spandex, ctx_time_position[spandex],
                               &ctxday, &ctxsec);
         if (ctxday == erly_day && ctxsec == erly_sec &&
             spandex != thisone ){
            ctx_time_position[spandex]++;
         }
      }
      /**************/
      /* check itxs */
      /**************/
      for(yo=0; yo < dtx->numofitxs; yo++){
         spandex = dtx->itxarray[yo];
         vis5d_get_itx_time_stamp( spandex, itx_time_position[spandex],
                               &itxday, &itxsec);
         if (itxday == erly_day && itxsec == erly_sec &&
             spandex != thisone ){
            itx_time_position[spandex]++;
         }
      }


      if (erly_day == maxday && erly_sec == maxsec){
         /***********************************/         
         /* make last time step, then done! */
         /***********************************/
         /********/
         /* ctxs */
         /********/
         for (yo = 0; yo < dtx->numofctxs; yo++){
            int what;
            spandex = dtx->ctxarray[yo];
            dtx->TimeStep[timecount].ownertype[yo] = REGULAR_TYPE;            
            dtx->TimeStep[timecount].owners[yo] = spandex;
            dtx->TimeStep[timecount].ownerstimestep[yo] = ctx_numtimes[spandex]-1;
            dtx->DayStamp[timecount] = erly_day;
            dtx->TimeStamp[timecount] = erly_sec;
         }   
         /********/         
         /* itxs */
         /********/
         for (yo = dtx->numofctxs; yo < dtx->numofitxs + dtx->numofctxs; yo++){
/* MJK 1.8.00
         for (yo = dtx->numofctxs; yo < dtx->numofitxs; yo++){
*/
            int what;
            spandex = dtx->itxarray[yo];
            dtx->TimeStep[timecount].ownertype[yo] = IRREGULAR_TYPE;
            dtx->TimeStep[timecount].owners[yo] = spandex;
            dtx->TimeStep[timecount].ownerstimestep[yo] = itx_numtimes[spandex]-1;
            dtx->DayStamp[timecount] = erly_day;
            dtx->TimeStamp[timecount] = erly_sec;
         }
         if ( not_duplicate_timestep( dtx, timecount) ){
            dtx->NumTimes = timecount + 1;
         }
         else{
            dtx->NumTimes = timecount;
         }
         create_elapsed_times(dtx);
         return;
      }
      for (ctx_loop = 0; ctx_loop < dtx->numofctxs + dtx->numofitxs; ctx_loop++){
         /*******/
         /* ctx */
         /*******/
         if (ctx_loop < dtx->numofctxs){
            spandex = dtx->ctxarray[ctx_loop];
         }
         /*******/         
         /* itx */
         /*******/
         else{
            spandex = dtx->itxarray[ctx_loop - dtx->numofctxs];
         }
         closest_tyme = -7;
         best_day = best_sec = 10000000;
         if (ctx_loop < dtx->numofctxs){
            /*******/
            /* ctx */
            /*******/
            for (tyme = 0; tyme < ctx_numtimes[spandex]; tyme++){
               vis5d_get_ctx_time_stamp( spandex, tyme, &tempday, &tempsec);
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
            dtx->TimeStep[timecount].ownertype[ctx_loop] = REGULAR_TYPE;
            dtx->TimeStep[timecount].owners[ctx_loop] = spandex;
            dtx->TimeStep[timecount].ownerstimestep[ctx_loop] = closest_tyme;
            dtx->DayStamp[timecount] = erly_day;
            dtx->TimeStamp[timecount] = erly_sec;
         }
         else{
            /*******/         
            /* itx */
            /*******/         
            for (tyme = 0; tyme < itx_numtimes[spandex]; tyme++){
               vis5d_get_itx_time_stamp( spandex, tyme, &tempday, &tempsec);
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
            dtx->TimeStep[timecount].ownertype[ctx_loop] = IRREGULAR_TYPE;
            dtx->TimeStep[timecount].owners[ctx_loop] = spandex;
            dtx->TimeStep[timecount].ownerstimestep[ctx_loop] = closest_tyme;
            dtx->DayStamp[timecount] = erly_day;
            dtx->TimeStamp[timecount] = erly_sec;
         }
      }
      if ( not_duplicate_timestep( dtx, timecount) ){
         timecount++;
      }
   }
}
    





