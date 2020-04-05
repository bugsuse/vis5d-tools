/* topo.h */


#ifndef TOPO_H
#define TOPO_H


extern int load_topo( char *filename );

extern float elevation( float lat, float lon, int *water );

extern void free_topo( void );

extern void set_topo_sampling( float latres, float lonres );


#endif

