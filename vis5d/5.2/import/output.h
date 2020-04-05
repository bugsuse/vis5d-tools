/* output.h */


#ifndef OUTPUT_H
#define OUTPUT_H


#include "grid.h"
#include "v5d.h"


extern void make_output_file( struct grid_db *db, v5dstruct *v5d,
                              char *filename, int maxnl,
                              int average, int compressmode );


#endif
