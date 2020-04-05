/* read_grid.h */


/*
 * Functions for reading with McIDAS GRID files
 */


#ifndef READ_GRID_H
#define READ_GRID_H


#include "grid.h"



/*
 * Get a description of the contents of a McIDAS GRID file.
 */
extern int get_grid_info( char *name, struct grid_db *db );


/*
 * Get actual grid data.
 */
extern float *get_grid_data( struct grid_info *g );



#endif
