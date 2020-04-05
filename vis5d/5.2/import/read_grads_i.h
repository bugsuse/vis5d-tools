/* read_grads.h */


/*
 * Functions for reading GRADS files.
 */


#ifndef READ_GRADS_H
#define READ_GRADS_H


#include "file.h"


/*
 * Read the header info in a GRADS file.
 */
extern int get_grads_info( char *name, struct grid_db *db );


/*
 * Get actual grid data.
 */
extern float *get_grads_data( struct grid_info *g );


#endif
