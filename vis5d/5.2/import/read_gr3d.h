/* read_gr3d.h */


/*
 * Functions for reading with McIDAS GR3D files
 */


#ifndef READ_GR3D_H
#define READ_GR3D_H


#include "grid.h"



/*
 * Get a description of the contents of a McIDAS GR3D file.
 */
extern int get_gr3d_info( char *name, struct grid_db *db );


/*
 * Get actual grid data.
 */
extern float *get_gr3d_data( struct grid_info *g );



#endif
