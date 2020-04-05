#define G5D 800000
#define NLEV 25

#ifdef MAIN_R
float plevel[NLEV];
float zgrid5d[G5D];
float vgrid5d[G5D];
float  grid5d[G5D];
#else
extern float plevel[NLEV];
extern float zgrid5d[G5D];
extern float vgrid5d[G5D];
extern float  grid5d[G5D];
#endif
