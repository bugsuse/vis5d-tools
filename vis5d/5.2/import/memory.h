/* memory.h */


/*
 * malloc/free wrappers to help with debugging.
 */


#ifndef MEMORY_H
#define MEMORY_H


extern void *MALLOC( size_t  size );


extern void FREE( void *ptr, int id );


#endif

