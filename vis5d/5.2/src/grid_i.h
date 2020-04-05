/* Vis5D version 5.2 */

/*
 Vis5D system for visualizing five dimensional gridded data sets
Copyright (C) 1990 - 2000 Bill Hibbard, Johan Kellum, Brian Paul,
Dave Santek, and Andre Battaiola.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/* grid.h */

/*
 * Generic grid structure, lists of grids, and functions
 */



#ifndef GRID_i_H
#define GRID_i_H


/*#include "v5d.h"*/


/*
 * Limits for v5dimport, limits for the output v5d file are probably
 * smaller and found in v5d.h
 */
#define IMAXVARS     70
#define IMAXTIMES  1000
#define IMAXPROJ    100



/* Map projections:  (Must agree w/ projections used in v5d files!!) */
#define PROJ_GENERIC   0  /* No specific units */
#define PROJ_LINEAR    1  /* Cylindrical-Equidistant (old vis5d) */
#define PROJ_LAMBERT   2  /* Lambert conformal */
#define PROJ_STEREO    3  /* Stereographic */
#define PROJ_ROTATED   4  /* Rotated equidistant */
#define PROJ_MERCATOR  5  /* Mercator */
#define PROJ_EPA      10  /* EPA RADM/MM4 system */
#define PROJ_CYLINDRICAL     20  /* Cylindrical projection of cyl-equid. */
#define PROJ_SPHERICAL       21  /* Spherical projection of cyl-equid. */


/* Vertical coordinate sytems:  (Must agree w/ vcs used in v5d files!!) */
#define VERT_GENERIC      0  /* No specific units */
#define VERT_EQUAL_KM     1  /* Equally spaced in kilometers */
#define VERT_UNEQUAL_KM   2  /* Non-equally spaced in kilometers */
#define VERT_UNEQUAL_MB   3  /* Non-equally spaced in millibars */
#define VERT_EPA         10  /* EPA RADM/MM4 system */



/*
 * Describes a map projection:
 */
struct projection {
   int Nr, Nc;             /* number of rows and columns */
   int Kind;               /* Kind of projection, one of PROJ_* */
   float *Args;            /* Array of projection parameters */
   float *AuxArgs;         /* Array of auxillary projection parameters */
};



/*
 * Describes a vertical coordinate system (VCS):
 */
struct vcs {
   int Nl;                 /* Number of levels */
   int Kind;               /* Kind of vcs, one of VERT_* */
   float *Args;            /* Array of vcs parameters */
   int LowLev;             /* Location of bottom-most grid level w.r.t. VCS */
};




/* Grid selection bits: */
#define TIME_BIT  1
#define VAR_BIT   2
#define PROJ_BIT  4
#define VCS_BIT   8
#define ALL_BITS  0xf



/*
 * Description of a single 3-D grid:
 */
struct grid_info {
        char *FileName;                  /* File name */
        int Format;                      /* Which file format (see file.h) */
        int TimeStep, VarNum, Position;  /* Location of grid within file */
        int FileNumber;                  /* For McIDAS GRID files */

        int Nr, Nc, Nl;                  /* Size of grid */
        int DateStamp;                   /* Date in YYDDD */
        int TimeStamp;                   /* Time in HHMMSS */
        char *VarName;                   /* Name of variable */
        char *Units;                     /* Physical units string or NULL */

        /* Map / Vert. Coord. Sys. arguments as used by v5dCreate functions */
        struct projection *Proj;         /* Map projection info */
        struct vcs *Vcs;                 /* Vert Coord Sys info */

        int byteswapped;                /* byteswapped flag for GRADS file */
        float MissingValue;             /* The missing data value */

        float *Data;                    /* Nr*Nc*Nl array of data or NULL */

        int SelectBits;                 /* Selection bits, OR of *_BIT flags */
        int NewSelState;                /* Set when GUI must be updated */

        struct grid_info *Next;         /* Next node in linked list */
        struct grid_info *Sibling;      /* Used by the table data structure */
};



/*
 * This "data base" structure bundles up all the grid information.
 */
struct grid_db {

   /*
    * Initially, we scan all the input files to build this linked list
    * of information about every 3-D grid:
    */
   int NumGrids;                   /* Number of grids in linked list */
   struct grid_info *FirstGrid;    /* Pointer to first grid */
   struct grid_info *LastGrid;     /* Pointer to last grid */

   /*
    * Next, we analysis the list of grids to get the following information:
    */
   int NumTimes;                  /* Number of distinct timesteps */
   int DateStamp[IMAXTIMES];      /* Array of dates in YYDDD format */
   int TimeStamp[IMAXTIMES];      /* Array of times in HHMMSS format */

   int NumVars;                   /* Number of distinct variables */
   char *VarNames[IMAXVARS];      /* Array of pointers to the variable names */
   char *Units[IMAXVARS];         /* Array of units strings, may be NULL */

   int NumProj;                           /* Number of map projections */
   struct projection *ProjList[IMAXPROJ]; /* Array of map projections */

   int NumVcs;                            /* Number of VCSs */
   struct vcs *VcsList[IMAXPROJ];         /* Array of VCSs */

   /*
    * We also organize the grids into a 2-D matrix indexed by timestep
    * and variable number:
    */
   struct grid_info *Matrix[IMAXTIMES][IMAXVARS];

   /*
    * Using the GUI, or text interface, the user can select which grids are
    * to be included in the output file according to timestep, variable,
    * map projection, or VCS.  These flag arrays indicate what's currently
    * selected:
    */
   int VarSelected[IMAXVARS];        /* Array of variable selection flags */
   int TimeSelected[IMAXTIMES];      /* Array of timestep selection flags */
   int ProjSelected[IMAXPROJ];       /* Array of projection selection flags */
   int VcsSelected[IMAXPROJ];        /* Array of vcs selection flags */

   /*
    * Misc.
    */
   int Sorted;
};




/*
 * A linked list of grids:
 */
#ifdef JUNK
struct grid_list {
        int NumGrids;                    /* Number of grids */
        struct grid_info *First, *Last;  /* Pointers to first and last grids */
};
#endif


extern struct grid_info *alloc_grid_info( void );


extern void free_grid_info( struct grid_info *info );


extern struct grid_db *alloc_grid_db( void );


extern void free_grid_db( struct grid_db *db );


extern void append_grid( struct grid_info *grid, struct grid_db *db );


extern int remove_grid( struct grid_info *grid, struct grid_db *db );


extern void free_all_grids( struct grid_db *db );


extern void print_grid_list( struct grid_db *db );


extern char **sprint_grid_list( struct grid_db *db );


extern int find_max_levels( struct grid_db *db );


#endif
