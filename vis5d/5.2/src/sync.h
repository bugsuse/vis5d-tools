
/* Vis5d version 5.2 */

/*
Vis5D system for visualizing five dimensional gridded data sets
Copyright (C) 1990 - 2000  Bill Hibbard, Brian Paul, Dave Santek,
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



/* A library of concurrency functions and macros usable in C on Stellar,
   SGI, CRAY and SunOS 5.x systems.

   Notes:

   1. when declaring LOCKs or SEMAPHOREs, only one should be declared
      per line.  For example:

        LOCK a;      this is correct
        LOCK b;

        LOCK a, b;   this is incorrect

   2. CRAY:  semaphores are binary, not counting semaphores.
*/


#ifndef SYNC_H
#define SYNC_H



/* Includes, types and prototypes: */

#ifdef cray
   struct sem {
      int val;
      int mutex;
      int ev;
      int evflag;
      int awaken;
   };
   extern void alloc_sem(), free_sem(), wait_sem(), signal_sem();
#endif

#ifdef stellar
   struct sem {
      int val;
      int mutex;
      int awaken;
   };
   extern void alloc_sem(), wait_sem(), signal_sem();
#endif

#ifdef sgi
#  include <ulocks.h>
   extern usptr_t *Arena;
#endif

#ifdef sequent
#  include <parallel/microtask.h>
#  include <parallel/parallel.h>
#else
#  define shared
#endif

#ifdef sunos5
#  include <synch.h>
#endif
#ifdef LTHREADS 
#  include<semaphore.h>
#endif




/*
 * This function should be called before using any of sychronization
 * primitives below.
 */
extern int init_sync( void );



/*
 * This function should be called prior to program termination to
 * release resources associated with the synchronization primitives.
 */
extern void term_sync( void );



/*** LOCKS ***/

#ifdef cray
#  define LOCK               int
#  define ALLOC_LOCK( L )    LOCKASGN( &(L) )
#  define FREE_LOCK( L )     LOCKREL( &(L) )
#  define LOCK_ON( L )       LOCKON( &(L) )
#  define LOCK_OFF( L )      LOCKOFF( &(L) )
#  define LOCK_TEST( L )     LOCKTEST( &(L) )
#endif

#ifdef stellar
#  define LOCK               int
#  define ALLOC_LOCK( L )    L = 0
#  define FREE_LOCK( L )     ;
#  define LOCK_ON( L )       wait_until_locked( &(L), 0 )
#  define LOCK_OFF( L )      atomic_bit_clear( &(L), 0 )
#  define LOCK_TEST( L )     atomic_bit_test( &(L), 0 )
#  define COND_LOCK( L )     (!atomic_bit_set( &(L), 0 ))
#endif

#ifdef sgi
#  define LOCK               ulock_t
#  define ALLOC_LOCK( L )    L = usnewlock( Arena )
#  define FREE_LOCK( L )     usfreelock( L, Arena )
#  define LOCK_ON( L )       ussetlock( L )
#  define LOCK_OFF( L )      usunsetlock( L )
#  define LOCK_TEST( L )     ustestlock( L )
#  define COND_LOCK( L )     uscsetlock( L, 1 )
#endif

#ifdef sequent
#  define LOCK               slock_t
#  define ALLOC_LOCK( L )    s_init_lock( &(L) )
#  define FREE_LOCK( L )     ;
#  define LOCK_ON( L )       s_lock( &(L) )
#  define LOCK_OFF( L )      s_unlock( &(L) )
#endif

#ifdef sunos5
#  define LOCK               rwlock_t
#  define ALLOC_LOCK( L )    rwlock_init( &L, USYNC_THREAD, 0 )
#  define FREE_LOCK( L )     rwlock_destroy( &L )
#  define LOCK_ON( L )       rw_wrlock( &L )
#  define LOCK_OFF( L )      rw_unlock( &L )
#  define COND_LOCK( L )     rw_trywrlock( &L )
#endif

/* otherwise: */
#ifndef LOCK
#  define LOCK               int
#  define ALLOC_LOCK( L )    ;
#  define FREE_LOCK( L )     ;
#  define LOCK_ON( L )       ;
#  define LOCK_OFF( L )      ;
#  define LOCK_TEST( L )     1
#  define COND_LOCK( L )     1
#endif



/*** SEMAPHORES ***/

#ifdef cray
#  define SEMAPHORE          struct sem
#  define ALLOC_SEM( S, N )  alloc_sem( &(S), N )
#  define FREE_SEM( S )      free_sem( &(S) )
#  define WAIT_SEM( S )      wait_sem( &(S) )
#  define SIGNAL_SEM( S )    signal_sem( &(S) )
#endif

#ifdef stellar
#  define SEMAPHORE          struct sem
#  define ALLOC_SEM( S, N )  alloc_sem( &(S), N )
#  define FREE_SEM( S )      ;
#  define WAIT_SEM( S )      wait_sem( &(S) )
#  define SIGNAL_SEM( S )    signal_sem( &(S) )
#endif

#ifdef sgi
#  define SEMAPHORE          usema_t *
#  define ALLOC_SEM( S, N )  S = usnewsema( Arena, N )
#  define FREE_SEM( S )      usfreesema( S, Arena )
#  define WAIT_SEM( S )      uspsema( S )
#  define SIGNAL_SEM( S )    usvsema( S )
#endif

#ifdef sequent   /* untested: */
#  define SEMAPHORE          int
#  define ALLOC_SEM( S, N )  S = alloc_sem( N )
#  define FREE_SEM( S )      free_sem( S )
#  define WAIT_SEM( S )      wait_sem( S )
#  define SIGNAL_SEM( S )    signal_sem( S )
#endif

#ifdef sunos5
#  define SEMAPHORE          sema_t
#  define ALLOC_SEM( S, N )  sema_init( &S, N, USYNC_THREAD, 0 )
#  define FREE_SEM( S )      sema_destroy( &S )
#  define WAIT_SEM( S )      sema_wait( &S )
#  define SIGNAL_SEM( S )    sema_post( &S )
#endif

#ifdef LTHREADS 
#  define SEMAPHORE          sem_t
#  define ALLOC_SEM( S, N )  sem_init( &S, N, 0 )
#  define FREE_SEM( S )      sem_destroy( &S )
#  define WAIT_SEM( S )      sem_wait( &S )
#  define SIGNAL_SEM( S )    sem_post( &S )
#endif


/* otherwise: */
#ifndef SEMAPHORE
#  define SEMAPHORE          int
#  define ALLOC_SEM( S, N )  ;
#  define FREE_SEM( S )      ;
#  define WAIT_SEM( S )      ;
#  define SIGNAL_SEM( S )    ;
#endif



/*** Read/Write locks ***/

extern void wait_read_lock( int * );
extern int cond_read_lock( int * );
extern void wait_write_lock( int * );
extern void done_read_lock( int * );
extern void done_write_lock( int * );


#endif
