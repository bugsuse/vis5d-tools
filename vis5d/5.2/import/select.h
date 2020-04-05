/* select.h */


#ifndef SELECT_H
#define SELECT_H


#include "grid.h"


extern void select_time( struct grid_db *db, int time, int state );


extern void select_variable( struct grid_db *db, int var, int state );


extern void select_projection( struct grid_db *db, int projnum,
                               int state );

extern void select_vcs( struct grid_db *db, int vcsnum, int state );


extern void select_all( struct grid_db *db, int bitmask, int state );


#endif

