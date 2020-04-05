/* file.h */


#ifndef FILE_H
#define FILE_H


#include "grid.h"


/* Input File formats: */
#define FILE_UNKNOWN   0
#define FILE_GR3D      1
#define FILE_EPA       2
#define FILE_V5D       3
#define FILE_GRADS     4
#define FILE_UWVIS     5
#define FILE_GRID      6



/*
 * Return info about the named file.
 */
extern void get_file_info( char *name, struct grid_db *db );


extern float *get_file_data( struct grid_info *g );



#endif

