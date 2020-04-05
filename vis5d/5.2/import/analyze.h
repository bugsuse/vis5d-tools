/* analyze.h */



#ifndef ANALYZE_H
#define ANALYZE_H


#include "grid.h"
#include "v5d.h"


extern void analyze_grids( struct grid_db *db );


extern void estimate_grid_levels( struct grid_db *db, int nl[] );


extern void compute_grid_levels( struct grid_db *db, struct vcs *outvcs,
                                 int lowlev[], int nl[] );


extern void find_default_vcs( struct grid_db *db, int max_out_nl,
                              int *vcs, float *vcsargs );


extern void setup_defaults( struct grid_db *db, v5dstruct *v,
                            int rowcol_flag, int proj_flag, int vert_flag );


#endif
