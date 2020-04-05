#define NFILES 10
#define FILELEN 80
#define NVAR 10
#define NGRID 100
#define TIMES 12	/* The number of time peroids */

struct Want {
 int nvars;		/* number of variables in list 'vars' */
 int vars[NVAR];	/* list of variable codes to get */
 int lev_ind[NVAR];	/* level indicators */
 long var_name[NVAR];	/* list of variable names correspinding to vars */
 int nfiles;		/* number of gribfiles */
 int fcst_hr[NFILES];	/* the forecast time to get out of the file */
 int grid_id[NFILES];	/* the grid ident (type) for grids in this file */
 int ntimes;		/* number of times */
 int times[TIMES];	/* array of sorted times */
 char file[NFILES][FILELEN];
 char grid5d_file[FILELEN];	/* gridfile name (output) */
 long title[36];	/* output 3d gridfile title (use first 32 chars) */
 };

struct InterpGrid {
 float nlat;	/* north latitude */
 float wlon;	/* west longitude */
 float linc;	/* lat-lon incriment */
 float base;	/* base of vertical dimension */
 float incv;	/* vertical incriment */
 int   nrow;	/* number of rows */
 int   ncol;	/* number of columns */
 int   nver;	/* number of vertical levels */
 int   gridsize;/* 3d grid size */
 int   hrz_grid_size; /* size of horizontal 2-d grid */
 int   h_latlon[5];	/* grid header words 21-25 0b lat-lon bounds */
 int   h_height[3];	/* grid header words 30-32 0b heights */
 };

struct Floc {
 double start_date;	/* serial date from 1 Jan 1990 with hour as fraction */
 double valid_time;	/* start_date + forecast hour */
 int num_grids;		/* number of grids in file */
 int loc[NGRID];	/* byte location of grid in file */
 int len[NGRID];	/* length in bytes of grib */
 int type[NGRID];	/* type of grid */
 int level[NGRID];	/* level of grid */
 };

struct Ts {		/* time sorted & level sorted fields */
 int num;		/* number of grids for this time */
 int files[NFILES];	/* file numbers for this time */
 int loc[NGRID];	/* grid location (0 based byte) in file by fil */
 int len[NGRID];	/* grid lentgh in bytes */
 int lev[NGRID];	/* mb level of grid */
 int fil[NGRID];	/* pointer to file number in struct floc */
 int date;		/* the date for this time peroid (as YYDDD) */
 long time;		/* the time for this time peroid as HHMMSS valid time */
 long serial;		/* Serial date */
 double valid_time;	/* valid time from 1 Jan 1900 */
 };

struct Vs {		/* variable sorted & level sorted fields */
 int num;		/* number of grids for this variable */
 int loc[NGRID];	/* grid location (0 based byte) in file by fil */
 int len[NGRID];	/* grid lentgh in bytes */
 int lev[NGRID];	/* mb level of grid */
 int fil[NGRID];	/* pointer to file number in struct floc */
 };

#ifdef MAIN_R
 struct Want want;
 struct InterpGrid ig;
 struct Floc floc[NFILES];
 struct Ts ts[TIMES];
 struct Vs vs[NVAR];
 int ts_num;		/* number of forecast hours */
#else
 extern struct Want want;
 extern struct InterpGrid ig;
 extern struct Floc floc[NFILES];
 extern struct Ts ts[TIMES];
 extern struct Vs vs[NVAR];
 extern int ts_num;
#endif
