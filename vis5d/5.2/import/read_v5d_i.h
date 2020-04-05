/* read_v5d.h */


/*
 * Functions for reading .v5d files.
 */


#ifndef READ_V5D_H
#define READ_V5D_H


#include "file.h"



/*
 * Read the header info in a .v5d file.
 */
extern int get_v5d_info( char *name, struct grid_db *db );


/*
 * Get actual grid data.
 */
extern float *get_v5d_data( struct grid_info *g );


#endif

