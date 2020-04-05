/* read_uwvis.h */


/*
 * Functions for reading UW NMS model VIS files.
 */


#ifndef READ_UWVIS_H
#define READ_UWVIS_H


#include "grid.h"




extern int get_uwvis_info( char *name, struct grid_db *db );


extern float *get_uwvis_data( struct grid_info *g );



#endif
