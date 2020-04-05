/* THESE ARE PORJECTION PARAMETERS FOR VARIOUS NMC GRID TYPES.  */
/* IPI, THE I COORDINATE OF THE NORTH POLE.     */
#define PROJSIZ 21
struct PParms { long pparms[7];};
#ifdef MAIN_R
/* Projection type, GRIB convention */
int iptype[PROJSIZ] =
     {  5,     6,    27,    28,    29,    30,    36,   100,
        1,    21,    51,   101,    22,    33,    34,    63,
       75,   104,   105,    26,    90};
/* Number of columns */
int ipc[PROJSIZ] =
     { 53,    53,    65,    65,   145,   145,    41,    83,
       73,    73,   129,   113,    73,   181,   181,    73,
      111,   147,    83,    53,    92};
/* Number of rows */
int ipr[PROJSIZ] =
     { 57,    45,    65,    65,    37,    37,    38,    83,
       23,    19,   129,    91,    19,    46,    46,    15,
      111,   110,    83,    45,   141};
/* type of grid, 1 lat-lon, 2 polarstereographic */
int igtype[PROJSIZ] =
     {  2,     2,     2,     2,     1,     1,     0,     2,
        0,     1,     2,     0,     1,     1,     1,     1,
        0,     0,     2,     2,     1};
/* grid size = ipr * ipc */
int ipgsiz[PROJSIZ] =
     {3021,  2385,  4225,  4225,  5365,  5365,  1558,  6889,
      1679,  1387, 16641, 10283,  1387,  8326,  8326,  1095,
     12321, 16170,  8669,  2385, 12972};
/* grid length (mesh) in km */
float pmesh[PROJSIZ] =
     {190.5, 190.5,  381.,  381.,   2.5,   2.5, 190.5,91.452,
        0.0,   5.0, 190.5,  90.5,   5.0,   2.0,   2.0,   5.0,
        0.0,90.75464,  90.7, 190.5, 0.0};
/* the rotation of the normal longitude from 80. West. */
float protat[PROJSIZ] =
     {-25.,  -25.,    0.,    0.,    0.,    0.,  -25.,  -25.,
       0.0,   0.0,  -25.,  -25.,    0.,    0.,   0.0,   0.0,
       0.0,  -25.,  -25.,  -25.,    0.};
/* column index of north pole */
float fpi[PROJSIZ] =
     { 27.,   27.,   33.,   33.,    1.,    1.,   19.,  40.5,
       0.0,   0.0,   65.,  57.5,   0.0,   0.0,   0.0,   0.0,
       0.0,  75.5,  40.5,   27.,    0.};
/* row index of north pole */
float fpj[PROJSIZ] =
     { 49.,   49.,   33.,   33.,    1.,    1.,   42.,  88.5,
       0.0,   0.0,   65.,  91.5,   0.0,   0.0,   0.0,   0.0,
       0.0, 109.5,  88.5,   49.,    0,};

/* projection parameters, McIdas format */
struct PParms Pparms[PROJSIZ] =
{2,   90000,  270000,  190500, 1050000,  600000,  600000,
 2,  -30000,  270000,  190500, 1050000,  600000,  600000,
 2,  330000,  330000,  381000,  800000,  600000,  600000,
 2,  330000,  330000,  381000,  800000, -600000, -600000,
 1,  900000,  000000,  000000, 3600000,   25000,   00000,
 1,   00000,  000000, -900000, 3600000,   25000,   00000,
 5,      00,      00,      00,      00,      00,      00,
 2,  -40000,  405000,   91452, 1050000,  600000,  600000,
 5,      00,      00,      00,      00,      00,      00,
 1,  900000,  000000,  000000, 3600000,   50000,   00000,
 2,  650000,  650000,  190500, 1050000,  600000,  600000,
 2,  -55000,  555000,   91452, 1050000,  600000,  600000,
 1,    0000,  000000, -900000, 3600000,   50000,   00000,
 1,  900000,  000000,    0000, 3600000,   20000,   00000,
 1,    0000,  000000, -900000, 3600000,   20000,   00000,
 1,  350000,  000000, -350000, 3600000,   50000,   00000,
 5,    0000,    0000,    0000,    0000,    0000,    0000,
 5,    0000,    0000,    0000,    0000,    0000,    0000,
 2,  -40000,  405000,   90755, 1050000,  600000,  600000,
 5,    0000,    0000,    0000,    0000,    0000,    0000,	/* 20 */
 5,    0000,    0000,    0000,    0000,    0000,    0000
};
#else
extern int   iptype[PROJSIZ];
extern int   ipc   [PROJSIZ];
extern int   ipr   [PROJSIZ];
extern int   igtype[PROJSIZ];
extern int   ipgsiz[PROJSIZ];
extern float  pmesh[PROJSIZ];
extern float protat[PROJSIZ];
extern float    fpi[PROJSIZ];
extern float    fpj[PROJSIZ];
extern struct PParms Pparms[PROJSIZ];
#endif
