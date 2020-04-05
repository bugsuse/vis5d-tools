/* memory.c */

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


#include <assert.h>
#ifdef stellar
#  include <malloc.h>
#  include <memory.h>
#else
#  include <stdlib.h>
#  include <string.h>
#endif
#include <stdio.h>
#include "globals.h"
#include "memory.h"
#include "misc.h"
#include "sync.h"



/* must be consistent with memory.h */
#define NULL_TYPE 0


struct mem {
   int        size;
   struct mem *prev;
   struct mem *next;
   short int        free, magic;
#ifdef DEBUG_MEM
   int type;
#endif
};


#define MEMSIZ sizeof(struct mem)
#define MAGIC 0x1234

static void check_memory( Context ctx );





/********************************************/
/****   Private functions                 ***/
/********************************************/


/*
 * Allocate a block of memory.
 * Input:  b - number of bytes to allocate
 *         permanent - if non-zero do a permanent allocation
 *         type - type of block (see list in memory.h)
 * Return:  address of block or NULL if unable to make allocation
 */
static void *alloc( Context ctx, int b, int permanent, int type )
{
   int bytes;
   struct mem *pos, *new;

#ifdef DEBUG_MEM
   printf("Allocate( %d )\n", b);
#endif

   /* round up bytes to multiple of sizeof(struct mem) */
   if (b<MEMSIZ) {
      bytes = MEMSIZ;
   }
   else {
      bytes = ( (b+MEMSIZ-1) / MEMSIZ ) * MEMSIZ;
   }
   /*
    * If we want to make a permanent allocation, try to do it at tail
    * of memory list.
    */
   if (permanent) {
      if (ctx->tail->size >= bytes) {
#ifdef DEBUG_MEM
         printf("permanent allocation of %d bytes.  old tail->size=%d",
                 bytes, ctx->tail->size );
#endif
         ctx->tail->size -= bytes;
         ctx->memory_used += bytes;
#ifdef DEBUG_MEM
         printf(".  new tail->size=%d\n", ctx->tail->size );
#endif
         return (char *) ctx->tail + MEMSIZ + ctx->tail->size;
      }
      /* couldn't allocate from tail; just do a normal allocation */
   }


   /*
    * Find a block of memory large enough to make the allocation from.
    */

   pos = NULL;
   if (ctx->guess) {
      /* Try a guess */
#ifdef DEBUG_MEM
      assert( ctx->guess->magic==MAGIC );
      assert( ctx->guess->free==1);
#endif
      if (ctx->guess->free && ctx->guess->size >= bytes + MEMSIZ) {
         /* a good guess! */
#ifdef DEBUG_MEM
         printf("Good guess!\n");
#endif
         pos = ctx->guess;
         ctx->guess = NULL;
      }
   }

   if (pos==NULL) {
      /* Find first fit */
#ifdef DEBUG_MEM
      int tries = 0;
#endif
      for (pos=ctx->head; pos; pos=pos->next) {
         if (pos->free && pos->size == bytes) {
            /* found exact fit! */
            break;
         }
         else if (pos->free && pos->size >= bytes + MEMSIZ) {
            /* found a block to split */
            break;
         }
#ifdef DEBUG_MEM
         tries++;
#endif
      }
#ifdef DEBUG_MEM
      printf("%d tries\n", tries);
#endif
   }


   if (!pos) {
      /* couldn't find block large enough, return NULL */
      ctx->guess = NULL;
      return NULL;
   }

   if (pos->size == bytes) {
      /* found a block of exact size! */
      pos->free = 0;
      ctx->memory_used += bytes;
      if (ctx->guess==pos)
        ctx->guess = NULL;
#ifdef DEBUG_MEM
      pos->type = type;
      printf("exact fit 0x%x 0x%x\n", pos, pos+1);
      check_memory( ctx );
#endif
      return (void *) (pos+1);
   }
   else {
      /*
       * 'pos' points to a block of sufficient size.  Split it into two
       * pieces, and return the address of the first part
       */
      new = (struct mem *) ( (char *) pos + MEMSIZ + bytes );
      /* init new fragment node */
      new->size = pos->size - bytes - MEMSIZ;
      new->prev = pos;
      new->next = pos->next;
      new->free = 1;
      new->magic = MAGIC;
      /* tail pointer */
      if (pos->next)
        pos->next->prev = new;
      else
        ctx->tail = new;
      /* links to pos */
      pos->next = new;
      pos->size = bytes;
      pos->free = 0;
      ctx->memory_used += bytes + MEMSIZ;
      /* reset guess */
      if (!ctx->guess) {
         ctx->guess = new;
      }
#ifdef DEBUG_MEM
      pos->type = type;
      printf("big fit 0x%x 0x%x\n", pos, pos+1);
      check_memory( ctx );
#endif
      return (void *) (pos+1);
   }
}




/*
 * Deallocate a block of memory.
 * Input:  addr - the address of the block to deallocate
 *         b - number of bytes in the block or -1 if unknown
 */
static void dealloc( Context ctx, void *addr, int b )
{
   int bytes;
   struct mem *pos, *pred, *succ;

#ifdef DEBUG_MEM
   printf("Deallocate( 0x%x, %d )\n", addr, b );
#endif
   if (addr==NULL) {
      printf("Warning:  deallocate(NULL)\n");
      return;
   }

   pos = (struct mem *) ( (char *) addr - MEMSIZ );

#ifdef DEBUG_MEM
   /* Sanity Checks: */
   assert( pos->magic==MAGIC );
   assert( pos->free==0 );
#endif

   if (b<0) {
      bytes = pos->size;
   }
   else if (b<MEMSIZ) {
      bytes = MEMSIZ;
   }
   else {
      /* round up bytes to multiple of sizeof(struct mem) */
      bytes = ( (b+MEMSIZ-1) / MEMSIZ ) * MEMSIZ;
      if (pos->size!=bytes) {
         printf("Warning:  wrong number of bytes in deallocate() %d vs %d\n",
                pos->size, bytes );
      }
   }

   /* mark as free */
   pos->free = 1;
   ctx->memory_used -= bytes;

   /* try to merge this block with successor */
   if (pos->next && pos->next->free==1) {
#ifdef DEBUG_MEM
      printf("Merge with successor\n");
#endif
      succ = pos->next;
      pos->size += MEMSIZ + succ->size;
      pos->next = succ->next;
      pos->free = 1;
      if (succ->next)
         succ->next->prev = pos;
      else
         ctx->tail = pos;
      /* update guess if necessary */
      if (succ==ctx->guess) {
         ctx->guess = pos;
      }
      ctx->memory_used -= MEMSIZ;
   }

   /* try to merge this block with predecessor */
   if (pos->prev && pos->prev->free==1) {
#ifdef DEBUG_MEM
      printf("Merge with predecessor\n");
#endif
      pred = pos->prev;
      pred->size += MEMSIZ + pos->size;
      pred->next = pos->next;
      if (pos->next)
         pos->next->prev = pred;
      else
         ctx->tail = pred;
      /* update guess if necessary */
      if (pos==ctx->guess) {
         ctx->guess = pred;
      }
      /* update pos */
      pos = pred;
      ctx->memory_used -= MEMSIZ;
   }

   /* reset guess */
   ctx->guess = pos;
#ifdef DEBUG_MEM
   check_memory(ctx);
#endif
}




/********************************************/
/***         DEBUGGING FUNCTIONS          ***/
/********************************************/



static void check_memory( Context ctx )
{
   struct mem *pos, *pred;

   pred = NULL;
   pos = ctx->head;
   while (pos) {
      if (pos->free!=1 && pos->free!=0) {
         printf("bad pos->free %d\n", pos->free);
      }
      if (pos->magic!=MAGIC) {
         printf("bad magic number in node 0x%x\n", pos );
      }
      if (pos->prev != pred) {
         printf("bad pred pointer 0x%x should be 0x%x\n", pos->prev, pred );
      }
      if (pos->next==NULL && ctx->tail!=pos) {
         die("bad tail\n");
      }
      if (pred && pred->free==1 && pos->free==1) {
         die("adjacent free blocks");
      }
      if (pred) {
         if ((char *) pred + pred->size + MEMSIZ != (char *) pos) {
            die("Bad size");
         }
      }

      pred = pos;
      pos = pos->next;
   }

   assert( ctx->tail->free==1 );

   if (ctx->guess)
     assert( ctx->guess->free == 1 );

}



static void dump_memory( Context ctx )
{
   struct mem *pos;

   pos = ctx->head;
   while (pos) {
      printf("node: 0x%x\n", pos );
      printf("  size: %d", pos->size );
      printf("  prev: 0x%x", pos->prev );
      printf("  next: 0x%x", pos->next );
#ifdef DEBUG_MEM
      printf("  type: %d", pos->type );
#endif
      printf("  free: %d\n", pos->free );
      pos = pos->next;
   }
   printf("tail = 0x%x\n", ctx->tail );
   printf("memory used: %d\n", ctx->memory_used);
}



/**********************************************************************/
/****                         PUBLIC FUNCTIONS                     ****/
/**********************************************************************/



/*
 * Initialize the memory management for a context.
 * Input:  ctx - the vis5d context
 *         bytes - size of the memory pool.
 * Return:  1 = success, 0 = error
 */
int init_memory( Context ctx, int bytes )
{
   struct mem *m;

   assert( bytes==0 || bytes>=1024*1024 );

   /*printf("init_memory( %d ) ctx = %d\n", bytes, ctx->context_index);*/

   ctx->memory_limit = bytes;

   if (bytes) {
      m = (struct mem *) malloc( bytes );
      if (!m) {
         printf("Error: unable to allocate %d bytes of memory.\n", bytes);
         printf("Either change MBS in vis5d.h or use -mbs option.\n");
         return 0;
      }

      m->size = bytes - sizeof(struct mem);
      m->prev = NULL;
      m->next = NULL;
      m->free = 1;
      m->magic = MAGIC;

      ctx->mempool = m;
      /* MJK 12.15.98 */
      ctx->head = ctx->tail = ctx->guess = m;
/*
      ctx->head = ctx->tail = m;
*/
      ctx->memory_used = MEMSIZ;
   }
   else {
      ctx->mempool = 0;
      ctx->memory_used = 0;
   }

   ALLOC_LOCK( ctx->memlock );
   ALLOC_LOCK( ctx->lrulock );

   ctx->meminited = 1;

   return 1;
}




/*
 * Define a shared memory area to use as the memory pool for a context.
 * Input:  ctx - the vis5d context
 *         bytes - size of the memory pool.
 * Return:  1 = success, 0 = error
 */
int init_shared_memory( Context ctx, void *start, int bytes )
{
   struct mem *m;

   ctx->memory_limit = bytes;

   m = start;
   m->size = bytes - sizeof(struct mem);
   m->prev = NULL;
   m->next = NULL;
   m->free = 1;
   m->magic = MAGIC;

   ctx->mempool = start;
   /* MJK 12.15.98 */
   ctx->head = ctx->tail = ctx->guess = m;
/*
   ctx->head = ctx->tail = m;
*/
   ctx->memory_used = MEMSIZ;

   ALLOC_LOCK( ctx->memlock );
   ALLOC_LOCK( ctx->lrulock );

   return 1;
}





/*
 * Reinitialize a memory pool so that it is completely unallocated.
 */
int reinit_memory( Context ctx )
{
   struct mem *m;

   if (ctx->memory_limit) {
      m = ctx->head;

      m->size = ctx->memory_limit - sizeof(struct mem);
      m->prev = NULL;
      m->next = NULL;
      m->free = 1;
      m->magic = MAGIC;

      ctx->head = ctx->tail = m;
      ctx->memory_used = MEMSIZ;
   }
   else {
      /* How do we free() all the malloc()s ?? - in case
         init_memory given bytes = 0 to flag use malloc */
      ctx->memory_used = 0;
   }

   return 1;
}



/*
 * Return the amount of available memory.
 * Input:  ctx - the vis5d context
 */
int mem_available( Context ctx )
{
   if (ctx->memory_limit==0)
      return 1024*1024*1024;  /* a Gig ought to be enough */
   else
      return ctx->memory_limit - ctx->memory_used;
}




/*
 * Allocate a block of memory.  If there is not enough memory to satisfy
 * the request, the least recently used graphics will be deallocated.
 * Input:  ctx - the vis5d context
 *         bytes - how many bytes to allocate
 * Return:  address of memory block or NULL if out of memory.
 */
void *allocate( Context ctx, int bytes )
{
   assert( bytes>=0 );

   if (ctx->memory_limit==0) {
      /* just malloc */
      return (void *) malloc( bytes );
   }
   else {
      void *addr;
      int ma, d;

      do {
         LOCK_ON( ctx->memlock );
         addr = alloc( ctx, bytes, 0, NULL_TYPE );
         LOCK_OFF( ctx->memlock );
         if (addr) {
            /* all done, return */
            return addr;
         }
         /* We didn't find a free block large enough, */
         /* try deallocating some graphics */
         ma = mem_available(ctx);
         LOCK_ON( ctx->lrulock );
         if (ma==mem_available(ctx)) {
            d = deallocate_lru(ctx);
         }
         LOCK_OFF( ctx->lrulock );
      } while (d>0);
      /* Couldn't deallocate anything, we're REALLY out of memory */
#ifdef DEBUG_MEM
      printf("Allocate %d failed\n", bytes );
      dump_memory(ctx);
#endif
      return NULL;
   }
}



/*
 * Allocate a block of memory.  If there is not enough memory to satisfy
 * the request, the least recently used graphics will be deallocated.
 * Input:  ctx - the vis5d context
 *         bytes - how many bytes to allocate
 *         type - type of block (see list in memory.h)
 * Return:  address of memory block or NULL if out of memory.
 */
void *allocate_type( Context ctx, int bytes, int type )
{
   assert( bytes>=0 );

   if (ctx->memory_limit==0) {
      /* just malloc */
      return (void *) malloc( bytes );
   }
   else {
      void *addr;
      int ma, d;

      do {
         LOCK_ON( ctx->memlock );
         addr = alloc( ctx, bytes, 0, type );
         LOCK_OFF( ctx->memlock );
         if (addr) {
            /* all done, return */
            return addr;
         }
         /* We didn't find a free block large enough, */
         /* try deallocating some graphics */
         ma = mem_available(ctx);
         LOCK_ON( ctx->lrulock );
         if (ma==mem_available(ctx)) {
            d = deallocate_lru(ctx);
         }
         LOCK_OFF( ctx->lrulock );
      } while (d>0);
      /* Couldn't deallocate anything, we're REALLY out of memory */
#ifdef DEBUG_MEM
      printf("Allocate %d failed\n", bytes );
      dump_memory(ctx);
#endif
      return NULL;
   }
}



/*
 * Permanent allocate.  Same as allocate, above, but used to allocate
 * memory which will NEVER be deallocated.
 * Input:  ctx - the vis5d context
 *         bytes - number of bytes to allocate
 */
void *pallocate( Context ctx, int bytes )
{
   if (ctx->memory_limit==0) {
      /* just malloc */
      return (void *) malloc( bytes );
   }
   else {
      void *addr;
      int ma, d;

      do {
         LOCK_ON( ctx->memlock );
         addr = alloc( ctx, bytes, 1, NULL_TYPE );
         LOCK_OFF( ctx->memlock );
         if (addr) {
            /* all done, return */
            return addr;
         }
         /* We didn't find a free block large enough, */
         /* try deallocating some graphics */
         ma = mem_available(ctx);
         LOCK_ON( ctx->lrulock );
         if (ma==mem_available(ctx)) {
            d = deallocate_lru(ctx);
         }
         LOCK_OFF( ctx->lrulock );
      } while (d>0);
      /* Couldn't deallocate anything, we're REALLY out of memory */
#ifdef DEBUG_MEM
      printf("Allocate %d failed\n", bytes );
      dump_memory(ctx);
#endif
      return NULL;
   }
}




/*
 * Deallocate a block of memory.
 * Input:  ctx - the vis5d context
 *         addr - address of block (if NULL, nothing happens)
 *         bytes - size of block (if <= zero, bytes is ignored)
 */
void deallocate( Context ctx, void *addr, int bytes )
{
   LOCK_ON( ctx->memlock );
   if (addr) {
      if (ctx->memory_limit==0) {
         free( addr );
      }
      else {
         dealloc( ctx, addr, bytes );
      }
   }
   LOCK_OFF( ctx->memlock );
}




/*
 * Return the amount of memory used in a context.
 */
int mem_used( Display_Context dtx )
{
   int m;
   int yo;

   m = 0;
   for (yo = 0; yo < dtx->numofctxs; yo++){
      if (dtx->ctxpointerarray[yo]->memory_limit != 0){
         m += dtx->ctxpointerarray[yo]->memory_used;
      }
   }      
   return m;
}
