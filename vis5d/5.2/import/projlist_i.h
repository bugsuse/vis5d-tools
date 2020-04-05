/* projlist.h */


#ifndef PROJLIST_H
#define PROJLIST_H


#include "grid.h"


extern struct projection *new_projection( struct grid_db *db,
                                          int kind, int nr, int nc,
                                          float *args );


extern void free_projection( struct grid_db *db,
                             struct projection *proj );


extern int lookup_proj( struct grid_db *db, struct projection *proj );


extern char **sprint_projection_list( struct grid_db *db );


extern void print_projection_list( struct grid_db *db );


extern struct vcs *new_vcs( struct grid_db *db, int kind,
                            int nl, int lowlev, float *args );


extern void free_vcs( struct grid_db *db, struct vcs *vcs );


extern int make_vcs_list( struct vcs **vcslist, int max_vcs );


extern int lookup_vcs( struct grid_db *db, struct vcs *vcs );


extern char **sprint_vcs_list( struct grid_db *db );


extern void print_vcs_list( struct grid_db *db );


#endif
