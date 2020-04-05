/* read_epa.h */


/*
 * Functions for reading with EPA files.
 */


#ifndef READ_EPA_H
#define READ_EPA_H


#include "file.h"


/*
 * Read the header info in an EPA file.
 */
extern int get_epa_info( char *name, struct grid_db *db );


/*
 * Get actual grid data.
 */
extern float *get_epa_data( struct grid_info *g );


#endif
