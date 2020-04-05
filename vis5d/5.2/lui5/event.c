
#ifdef SCCS
static char sccsid[]="@(#)event.c        1.1 Stellar 89/02/09";
#endif
/*
                        Copyright 1989 by
                        Stellar Computer Inc.
                        All Rights Reserved
        
        This software comprises unpublished confidential information of
        Stellar Computer Inc. and may not be used, copied or made
        available to anyone, except in accordance with the license
        under which it is furnished.
*/


/*
 * LUI Event Routines
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

#include "lui.h"

#define USEC    1000000
#define TIME_X1 (1.5 * USEC)
#define TIME_X2 (2.5 * USEC)
#define GAIN_X1 1
#define GAIN_X2 5

#define MAX_LUI_EVENTS 3072
                       
typedef struct {
    Window        window;
    unsigned long mask;
    LUI_FNCP      func;
    int           lock;
    void          *widget;   /* Added by BEP on Jan 18, 1995 */
    int debug;
} LUI_EVENT;

static LUI_EVENT 
  LUI_EventTable[MAX_LUI_EVENTS];

static int   
  LUI_EventCount = 0,
  LUI_EventTimer = 0,
  LUI_EventLocked = 0;



void LUI_EventSetExitTime( int seconds )
{
    LUI_EventTimer = seconds;
}



static int LUI_EventTimmerTriggeredExit( void )
{
    struct timeval tp;
    static long start_sec, lapsed_usec;
    static int been_here = 0;
    float seconds;

    /* Synchronize the server */
    XSync(LUI_Display, 0);

    /* Get the current time */
    gettimeofday(&tp, (struct timezone *) 0);

    if (been_here) {
        lapsed_usec = ((tp.tv_sec - start_sec) * 1000000) + tp.tv_usec;
        seconds = lapsed_usec / 1000000.0;

        /* Shoot this goose */
        if (seconds > LUI_EventTimer) {
            XCloseDisplay(LUI_Display);

            fprintf(stderr, "LUI_EventTimmerTriggeredExit(): time's up at %d seconds\n", 
                    (int)seconds);

            exit(0);
        }
    }

    /* Initialize timer */
    else {
        been_here = 1;

        lapsed_usec = 0;
        start_sec = tp.tv_sec;
    }
}



void LUI_EventProcess( void )
{
    XEvent event;

    if (LUI_EventTimer) {
        for (;;) {
            if (XPending(LUI_Display)) {
                /* Grab the next event */
                XNextEvent(LUI_Display, &event);
            
                /* Dispatch the event */
                LUI_EventDispatch(&event);
            }
            
            /* Check the timer if it's on */
            if (LUI_EventTimer) 
              LUI_EventTimmerTriggeredExit();
        }
    }
    else {
         for (;;) {
            /* Grab the next event */
            XNextEvent(LUI_Display, &event);
            
            /* Dispatch the event */
            LUI_EventDispatch(&event);
        }
    }
}



/*
 * Pass an X event to a widget event handler.
 */

int LUI_EventDispatch( XEvent *event )
{
    register int     i;
    int processed = 0;

    /* Dispatch the event */
    for (i = 0; i < LUI_EventCount; ++i) {
        if (LUI_EventTable[i].window == event->xany.window) {
            if (LUI_EventLocked && !LUI_EventTable[i].lock) continue;
            if (LUI_EventTable[i].func) {
                /* Modified by BEP on Jan 18, 1995 */
                if (LUI_EventTable[i].widget) {
                   /* Pass pointer to widget if not null */
                   (*(LUI_EventTable[i].func))(LUI_EventTable[i].widget,event);
                }
                else {
                   /* Pass window ID */
                   (*(LUI_EventTable[i].func))(event->xany.window, event);
                }
                processed = 1;
            }
        }
    }
    return(processed);
}

/*****************************************************************************/

/* EventAdd modified by BEP on Jan 18, 1995 */


/*
 * Register an event handler with LUI.
 * Input:  window - the window ID
 *         mask - the event mask
 *         func - the callback function to invoke
 *         widget - pointer to the widget this event belongs to
 */
void LUI_EventAdd2( Window window, unsigned long mask, LUI_FNCP func,
                    void *widget )
{
    unsigned long new_mask;
    register int  i;

    /* Build a composite mask */
    for (new_mask = mask, i = 0; i < LUI_EventCount; ++i) {
        if (LUI_EventTable[i].window == window)
          new_mask |= LUI_EventTable[i].mask;
    }
        
    /* Select with composite mask */
    XSelectInput(LUI_Display, window, new_mask);

    LUI_EventTable[LUI_EventCount].window = window;
    LUI_EventTable[LUI_EventCount].mask   = mask;
    LUI_EventTable[LUI_EventCount].func   = func;
    LUI_EventTable[LUI_EventCount].lock   = 0;
    LUI_EventTable[LUI_EventCount].widget = widget;
    LUI_EventTable[LUI_EventCount].debug = LUI_EventCount;
    if (++LUI_EventCount == MAX_LUI_EVENTS) {
        fprintf(stderr, "LUI_EventAdd: Too many events in list\n");
        exit(-1);
    }
}


void LUI_EventAdd( Window window,  unsigned long mask, LUI_FNCP func )
{
   LUI_EventAdd2( window, mask, func, NULL );
}



void LUI_EventLock( Window window )
{
    Window root, parent, *children, child;
    unsigned int nchildren;
    register int i, j;

    /* If already locked, return */
    if (LUI_EventLocked && window) 
      return;

    /* Unlock events */
    if (!window) {
        LUI_EventLocked = 0;

        /* Reset all locks */
        for (i = 0; i < LUI_EventCount; ++i) 
          LUI_EventTable[i].lock = 0;

        XSync(LUI_Display, 0);

        return;
    }

    /* Get the descendents of this window */
    XQueryTree(LUI_Display, window,
               &root, &parent, &children, &nchildren);

    /* Check for each descendent, marking each one 'locked' */
    for (i = 0; i < nchildren; ++i) {
        child = *(children + i);

        for (j = 0; j < LUI_EventCount; ++j) 
          if (LUI_EventTable[j].window == child)
            LUI_EventTable[j].lock = 1;
    }

    /* Mark the parent as locked */
    for (j = 0; j < LUI_EventCount; ++j) 
      if (LUI_EventTable[j].window == window)
        LUI_EventTable[j].lock = 1;

    /* Turn on event locking */
    LUI_EventLocked = 1;
}


int LUI_EventButtonDownTimed( void )
{
    XEvent event;
    struct timeval tp;
    static int holding_down = 0, timing = 0;
    static long start_sec, lapsed_usec;
    int gain;

    /* Check for pending event: stop when get a ButtonRelease */
    LUI_EVENT_CHECK:
    if (XPending(LUI_Display)) {
        XPeekEvent(LUI_Display, &event);
        if (event.type == ButtonRelease || event.type == LeaveNotify) {
            timing       = 0;
            holding_down = 0;
            return(0);
        }
    }

    /* Setup a timer */
    if (!holding_down) {
        /* Get the time */
        if (gettimeofday(&tp, (struct timezone *)0)) {
            fprintf(stderr, "LUI_EventButtonDownTimed: gettimeofday botch!\n");
            goto LUI_EVENT_CHECK;
        }

        /* Check to see if lag time past */
        if (timing) {
            lapsed_usec = ((tp.tv_sec - start_sec) * USEC) + tp.tv_usec;
            if      (lapsed_usec > TIME_X2) gain = GAIN_X2;
            else if (lapsed_usec > TIME_X1) gain = GAIN_X1;
            else goto LUI_EVENT_CHECK;
        }
        else {
            timing     = 1;
            start_sec  = tp.tv_sec;
            gain = GAIN_X1;
        }
    }
    return(gain);
}


int LUI_EventButtonDown( void )
{
    XEvent event;

    /* Check for pending event: stop when get a ButtonRelease */
    if (XPending(LUI_Display)) {
        XPeekEvent(LUI_Display, &event);
        if (event.type == ButtonRelease || event.type == LeaveNotify) {
           return(0);
        }
    }
    return(1);
}




/*****************************************************************************/
/********* LUI DESTROY LOGIC * BILLH 10-4-91 *********************************/
/*****************************************************************************/


/*****************************************************************************/
/* added by billh 10-4-91 */

void delete_event( Window window )
{
  int n, m;

GOT_ONE:
  for (n=0; n<LUI_EventCount; n++) {
    if (LUI_EventTable[n].window == window) {
      for (m=n; m<LUI_EventCount-1; m++) {
        LUI_EventTable[m].window = LUI_EventTable[m+1].window;
        LUI_EventTable[m].mask = LUI_EventTable[m+1].mask;
        LUI_EventTable[m].func = LUI_EventTable[m+1].func;
        LUI_EventTable[m].lock = LUI_EventTable[m+1].lock;
        LUI_EventTable[m].widget = LUI_EventTable[m+1].widget;
        LUI_EventTable[m].debug = LUI_EventTable[m+1].debug;
      }
      LUI_EventCount--;
      goto GOT_ONE;
    }
  }
  return;
}



/*
 * Remove the event handlers for the given window.
 */
void LUI_EventRemove( Window window )
{
   delete_event( window );
}

