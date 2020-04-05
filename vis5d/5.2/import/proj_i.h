/* proj.h */


/*
 * Map projection / coordinate transformation
 */


#ifndef PROJ_H
#define PROJ_H


extern int rowcol_to_latlon( float row, float col,
                             float *lat, float *lon,
                             struct projection *proj );



extern int latlon_to_rowcol( float lat, float lon,
                             float *row, float *col,
                             struct projection *proj );



extern float proj_resolution( struct projection *proj );


extern int height_to_level( float height, float *level,
                            struct vcs *vcs, float topo_elev );


extern int level_to_height( float level, float *height,
                            struct vcs *vcs, float topo_elev );


#endif
