/* Vis5D version 5.2 */

/*
Vis5D system for visualizing five dimensional gridded data sets
Copyright (C) 1990-2000 Bill Hibbard, Brian Paul, Dave Santek,
and Andre Battaiola.

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



#ifndef GLOBALS_H
#define GLOBALS_H


#include <X11/Xlib.h>
#ifdef OPENGL
#  include "GL/glx.h"
#endif
#ifdef SGI_GL
#  include <fmclient.h>
#endif
#include "api.h"
#include "matrix.h"
#include "sync.h"
#include "v5d.h"
#include "vis5d.h"
#include "irregular_v5d.h"


#ifndef NULL
#  define NULL 0
#endif


/*** Data types ***/
#if defined(stellar) || defined(hp) || defined(sun)
  typedef char         int_1;     /* 1-byte signed integer */
#else
  typedef signed char  int_1;
#endif
typedef unsigned char  uint_1;    /* 1-byte unsigned integer */
typedef short int      int_2;     /* 2-byte signed integer */
typedef unsigned short uint_2;    /* 2-byte unsigned integer */
typedef unsigned int   uint_4;    /* 4-byte unsigned integer */


#define ISOSURF        0
#define HSLICE         1
#define VSLICE         2
#define CHSLICE        3
#define CVSLICE        4
#define HWIND          6
#define VWIND          7
#define TRAJ           8
#define HSTREAM        9
#define VSTREAM       10


#define MAXTRAJ 10000    /* May be increased */

#define MAX_THREADS 8

#define MAX_DISPLAY_WINDOWS 50
#define MAX_DISPLAY_ROWS 10
#define MAX_DISPLAY_COLS 10


#define VERTEX_SCALE 10000.0
#define NORMAL_SCALE 125.0


/*** Graphics Data Structures ***/

/* Info about isosurfaces */
struct isosurface {
   int     lock;        /* mutual exclusion lock */
   int     valid;       /* valid/initialized surface flag */
   float   isolevel;    /* the isolevel of the surface */
   int     numverts;    /* number of vertices */
   int_2   *verts;      /* array [numverts][3] of vertices */
   int_1   *norms;      /* array [numverts][3] of normals */
   uint_1  *colors;     /* array [numverts] of color table indexes */
   int     colorvar;    /* variable which is coloring the surface, or -1 */
   int     cvowner;     /* vis5d_ctx index the var belongs too */
   int     cvtime;      /* time step for the color var ctx */
   int     numindex;    /* number of indexes */
#ifdef BIG_GFX
   uint_4  *index;      /* array of indices into verts, norms arrays */
                        /* 4-bytes indices means LONG strips OK */
#else
   uint_2  *index;      /* array of indices into verts, norms arrays */
                        /* 2-byte indices means the largest possible */
                        /* isosurface has 65000 vertices. */
#endif
};



/* Everything and Anything having to do with the sounding window */
struct sound {
   Window soundwin;         /* the window where data and misc. graphics are drawn to */
   Window SoundCtrlWindow;  /* window where gui is drawn and parent of soundwin */
   Pixmap soundpix;         /* pixmap where theta, theta-e, w.. etc lines are drawn to */
   GC Tempgc;               /* GC for the temperature in the skew-t */
   GC Dewptgc;              /* GC for the dew point in the skew-t */
   GC rect_gc;              /* GC for a big black rectangle */
   GC var1_gc;              /* GC for var1 */
   GC var2_gc;              /* GC for var2 */
   GC var3_gc;              /* GC for var3 */
   GC box_gc;               /* GC for bordering box */
   GC vert_gc;              /* GC for vertical numbers */
   GC barb_gc;              /* GC for the barb lines   */
   GC barb2_gc;              /* GC for the white flags inside the barbs */
   float *vertdata;         /* this holds the data for the vertical system and # of levels */
   float *soundline;        /* this array holds the sounding data */
   float *uwindline;        /* this array holds the u wind data */
   float *vwindline;        /* this array holds the v wind data */
   float SndMinTemp;        /* minimum temp at 1012.5 mb plotted on skew-t */
   float SndMaxTemp;        /* maximum temp at 1012.5 mb plotted on skew-t */
   float currentX;          /* current X cursor grid position */
   float currentY;          /* current Y cursor grid position */
   float *tgrid;            /* grid array to hold temperature data */
   float *dgrid;            /* grid array to hold dew point tempreature data */
   float *ugrid;            /* grid array to hold u wind data */
   float *vgrid;            /* grid array to hold v wind data */
   float *var1grid;         /* grid array to hold var1 data   */
   float *var2grid;         /* grid array to hold var 2 data  */
   float *var3grid;         /* grid array to hold var  3 data */
   int currentTime;         /* current time step for sounding */
   int soundwin_width;      /* width of soundwin */
   int soundwin_height;     /* height of soundiwn */
   int sndheight;           /* dimensions of soundwin where the actual data */
   int sndwidth;            /* is drawn to, excludes borders, and misc. graphcis */
   int sndx, sndy;          /* position of the snd window */
   int wstatus;             /* if 1 draw constant mixing ratio (w) lines */
   int thtestatus;          /* if 1 draw the moist adiabat (theta-e) lines */
   int thtastatus;          /* if 1 draw the dry adiabt (theta) lines */
   int tickstatus;          /* if 1 draw horz. and vert. tick lines */
   int SoundTemp;           /* variable number for temperature */
   int SoundDewpt;          /* variable number for dew point */
   int SoundUWind;          /* variable number for the U component of the wind */
   int SoundVWind;          /* variable number for the V componetn of the wind */
   int SoundVar1;           /* variable number for var1 */
   int SoundVar2;           /* variable number for var2 */
   int SoundVar3;           /* variable number for var3 */
   struct vis5d_context *SoundTempOwner;
   struct vis5d_context *SoundDewptOwner;      
   struct vis5d_context *SoundUWindOwner;      
   struct vis5d_context *SoundVWindOwner;     
   struct vis5d_context *SoundVar1Owner;
   struct vis5d_context *SoundVar2Owner;
   struct vis5d_context *SoundVar3Owner;
   int PreviousSoundTemp;   /* variable number for previous temperature */
   int PreviousSoundDewpt;  /* variable number for previous dew point */
   int PreviousSoundUWind;  /* variable number for previous the U component of the wind */
   int PreviousSoundVWind;  /* variable number for previous the V componetn of the wind */
   int PreviousSoundVar1;   /* variable number for previous var1 */
   int PreviousSoundVar2;   /* variable number for previous var2 */
   int PreviousSoundVar3;   /* variable number for previous var3 */
   struct vis5d_context *PreviousSoundTempOwner;   
   struct vis5d_context *PreviousSoundDewptOwner;  
   struct vis5d_context *PreviousSoundUWindOwner;  
   struct vis5d_context *PreviousSoundVWindOwner;  
   struct vis5d_context *PreviousSoundVar1Owner;   
   struct vis5d_context *PreviousSoundVar2Owner;  
   struct vis5d_context *PreviousSoundVar3Owner;   
   int vertsys;             /* if 0 then kilometers not used for vertical system */
   int oceanonly;           /* if 1 then data is for ocean only */
   int get_vert_data;       /* if 0 then data for vertical system already retreived */
   int mainvarstep;         /* the number of pixels between ticks on the horz. */
   int otherdpy;             /* if 1 then there is an alternate gui display */
   int samestepflag;        /* if 1 then bottom ticks numbers use same scale and range */
   float samestepmax;        /* max value of all three verticla plot variables */
   float samestepmin;        /* min value of all three verticla plot variables */
   float var1step;           /* var1 data increment for ticks, used when resizing window */
   float var2step;           /* var2 data increment for ticks, used when resizing window */
   float var3step;           /* var3 data increment for ticks, used when resizing window */

   /* MJK 12.10.98 */
   int tempstatus;              /* if 1, draw temp lines */
   float TopHgt, BotHgt, DiffHgt;       /* hgts at top/bot of snd display */
   float TopPress, BotPress;
};         

struct dpy_timestep{
   int ownertype[VIS5D_MAX_CONTEXTS];
   int owners[VIS5D_MAX_CONTEXTS];
   int ownerstimestep[VIS5D_MAX_CONTEXTS];
};

struct textplot {
   int    lock;
   int    valid;
   int    numverts;
   int_2  *verts;
   float  size;
   float  spacing;
   float  fontx;
   float  fonty;
   float  fontspace;
   uint_1 *colors;
};

/* Info about horizontal contour slices */
struct hslice {
   int    lock;
   int    valid;             /* valid/initialized slice flag */
   float  interval;          /* contour line interval */
   float  lowlimit;          /* lowest level to contour */
   float  highlimit;         /* highest level to contour */
   float  level;             /* position of slice in grid levels */
   int    num1;              /* number of line segment vertices */
   int_2  *verts1;           /* array [num1][3] of int_2 vertices */
   int    num2;              /* number of 'hidden' line segment vertices */
   int_2  *verts2;           /* array [num2][3] of int_2 vertices */
   int    num3;              /* number of label line segment vertices */
   int_2  *verts3;           /* array [num3][3] of int_2 vertices */
   float  *boxverts;         /* array of vertices for bounding rectangle */
   int    numboxverts;       /* number of vertices in boxverts array */
};


/* Info about vertical contour slices */
struct vslice {
   int    lock;
   int    valid;             /* valid/initialized slice flag */
   float  interval;          /* contour line interval */
   float  lowlimit;          /* lowest level to contour */
   float  highlimit;         /* highest level to contour */
   float  r1, c1;            /* 1st corner position in [0,Nr-1],[0,Nc-1] */
   float  r2, c2;            /* 2nd corner position in [0,Nr-1],[0,Nc-1] */
   int    num1;              /* number of line segment vertices */
   int_2  *verts1;           /* array [num1][3] of int_2 vertices */
   int    num2;              /* number of 'hidden' line segment vertices */
   int_2  *verts2;           /* array [num2][3] of int_2 vertices */
   int    num3;              /* number of label line segment vertices */
   int_2  *verts3;           /* array [num3][3] of int_2 vertices */
   float  *boxverts;         /* array of vertices for bounding rectangle */
   int    numboxverts;       /* number of vertices in boxverts array */
};


/* Info about colored horizontal slices */
struct chslice {
   int     lock;
   int     valid;           /* valid/initialized slice flag */
   float   level;           /* position of slice in grid levels */
   int     rows, columns;   /* size of quadmesh */
   int_2   *verts;          /* array [rows*columns][3] of int_2 vertices */
   uint_1  *color_indexes;  /* quadmesh vertex color indexes */
};


/* Info about colored vertical slices */
struct cvslice {
   int    lock;
   int     valid;             /* valid/initialized slice flag */
   float   r1, c1;            /* 1st corner position in [0,Nr-1],[0,Nc-1] */
   float   r2, c2;            /* 2nd corner position in [0,Nr-1],[0,Nc-1] */
   int     rows, columns;     /* size of quadmesh */
   int_2   *verts;            /* array [rows*columns][3] of int_2 vertices */
   uint_1  *color_indexes;    /* quadmesh vertex color indexes */
   float   mark[2][3];        /* tiny marker at midpoint of top edge */
};

/* tiny struct for the clipping planes */
struct hclip {
   int lock;   
   int highlight;            /* 1= current or highlighted one 0= not current */
   float level;              /* position of plane in virtual grid coords */
   float *boxverts;          /* pointer to array of vertices for bounding box */
   float numboxverts;        /* number of vertices in boxverts array */
   float eqn[4];             /* coefficents of plane eqn Ax+By+Cx+D=0 */
};

struct vclip {
   int lock;
   int highlight;            /* 1= current or highlighted one 0= not current */
   float  r1,c1,r2,c2;       /* position in [0,Nr-1],[0,Nc-1] */
   float *boxverts;          /* pointer to array of vertices for bounding box */
   float numboxverts;        /* number of vertices in boxverts array */
   float eqn[4];             /* coefficents of plane eqn Ax+By+Cx+D=0 */
};

/* Info about a horizontal wind vector slice */
struct hwind {
   int    lock;
   int    valid;             /* valid/initialized slice flag */
   int    barbs;             /* flag for barbs rather than vectors */
   int    uvar, vvar, wvar;  /* variables used to compute the slice */
   int    uvarowner,
          vvarowner,
          wvarowner;         /* u, v, w ctx index owners */ 
   float  level;             /* position of slice in grid levels */
   float  density;       /* relative density of vectors (1.0 is nominal) */
   float  scale;         /* vector length scale factor (1.0 is nominal) */
   int    nvectors;      /* number of vectors */
   int_2  *verts;        /* array [nvectors*4][3] of int_2 vertices */
   float  *boxverts;     /* pointer to array of vertices for bounding box */
   int    numboxverts;   /* number of vertices in boxverts array */
};


/* Info about a vertical wind vector slice */
struct vwind {
   int    lock;
   int    valid;             /* valid/initialized slice flag */
   int    barbs;             /* flag for barbs rather than vectors */
   int    uvar, vvar, wvar;  /* variables used to compute the slice */
   int    uvarowner,
          vvarowner,
          wvarowner;         /* u, v, w ctx index owners */
   float  r1,c1,r2,c2;       /* position in [0,Nr-1],[0,Nc-1] */
   float  density;       /* relative density of vectors (1.0 is nominal) */
   float  scale;         /* vector length scale factor (1.0 is nominal) */
   int    nvectors;      /* number of vectors */
   int_2  *verts;        /* array [nvectors*4][3] of int_2 vertices */
   float  *boxverts;     /* pointer to array of vertices for bounding box */
   int    numboxverts;   /* number of vertices in boxverts array */
};


/* Info about a horizontal streamline slice */
struct hstream {
   int    lock;
   int    valid;         /* valid/initialized slice flag */
   int    uvar, vvar;    /* variables used to compute the slice */
   int    uvarowner,
          vvarowner;         /* u, v, w ctx index owners */
   float  level;         /* position of slice in grid levels */
   float  density;       /* relative density of vectors (1.0 is nominal) */
   int    nlines;        /* number of line segment vertices */
   int_2  *verts;        /* array [nlines][3] of int_2 vertices */
   float  *boxverts;     /* pointer to array of vertices for bounding box */
   int    numboxverts;   /* number of vertices in boxverts array */
};


/* Info about a vertical streamline slice */
struct vstream {
   int    lock;
   int    valid;         /* valid/initialized slice flag */
   int    uvar, vvar, wvar;  /* variables used to compute the slice */
   int    uvarowner,
          vvarowner,
          wvarowner;         /* u, v, w ctx index owners */
   float  r1,c1,r2,c2;       /* position in [0,Nr-1],[0,Nc-1] */
   float  density;       /* relative density of vectors (1.0 is nominal) */
   int    nlines;        /* number of line segment vertices */
   int_2  *verts;        /* array [nlines][3] of int_2 vertices */
   float  *boxverts;     /* pointer to array of vertices for bounding box */
   int    numboxverts;   /* number of vertices in boxverts array */
};


/* Info about a wind trajectory */
struct traj {
   int     lock;
   float   row, col, lev;  /* initial position of trajectory */
   int     timestep;       /* initial timestep of trajectory */
   float   stepmult;       /* user's integration step multiplier */
   float   lengthmult;     /* user's traj length multiplier */
   int     length;         /* total number of vertices */
   int_2   *verts;         /* array [length][3] of int_2 vertices */
   int_1   *norms;         /* array [length][3] of int_2 normals */
   uint_1  *colors;        /* array [length] of color table indexes */
   int     colorvar;       /* which variable is coloring the traj, or -1 */
   int     colorvarowner;  /* index of the vis5d_ctx the colorvar belongs too */
   uint_2  *start;         /* array [NumTimes] 1st vertex for each timestep */
   uint_2  *len;           /* array [NumTimes] of lengths for each time step */
   int     group;          /* trajectory group */
   int     kind;           /* type of trajectory:  0 = line, 1 = ribbon */
   int     ctx_owner;      /* ctx->index of the owner of these traj's */
};




/*
 * GRID CACHING
 *
 * An array of cache_rec structs is used to manage the contents of the cache.
 *
 * An array of grid_rec structs s used to determine if (and where)
 * a grid is in the cache given a timestep and variable.
 */

struct cache_rec {
   void *Data;          /* Pointer to grid data or NULL */
   int Locked;          /* 1 = position is in use, 0 = not in use */
   int Timestep;        /* which timestep */
   int Var;             /* which variable */
   int Age;             /* for LRU replacement (large Age == Newer) */
};

struct grid_rec {
   int CachePos;        /* Position of this grid in cache array or -1 */
   void *Data;          /* Pointer to grid data or NULL */
};


/* from box.c */
#define MAX_BOX_VERTS 2000



/* texture mapping stuff from image.c */
#define MAX_IMAGES MAXTIMES



/* from labels.c */
struct label {
   char text[MAX_LABEL];        /* the text of the label */
   int len;                     /* length of label */
   int x, y;                    /* position */
   int x1, y1, x2, y2;          /* bounds of drawn text */
   int state;                   /* 1=being edited, 0=static */
   struct label *next;          /* pointer to next label in list */
   int id;                      /* a unique handle ID */
};



/* From map.c */
/*#define MAXMAPVERT 30000
#define MAXMAPSEG  3000 */
/* WLH 6-7-95  for OUTLCO */
#define MAXMAPVERT 200000
#define MAXMAPSEG  30000

#define MAXTYPES 10




/*** True Global Variables ***/
extern int Quiet, Debug;     /* for debugging */
extern int NumThreads;       /* Number of threads of execution */

extern int DisplayRows;
extern int DisplayCols;
extern int Current_Display;

extern LOCK GfxLock;         /* To synchronize reads/writes to graphics: */
extern LOCK TrajLock;

struct varlink{
   int var;
   int vindex;
   int type;
};

/* row = var
   col = type
   lev = index;

   var_link[(MAXTYPES*index+type)*MAXVARS+var]
*/

extern struct varlink var_link[MAXVARS * MAXTYPES * VIS5D_MAX_CONTEXTS];
extern struct varlink group_var_link[MAXVARS * MAXTYPES * VIS5D_MAX_CONTEXTS];


struct display_context {
   int dpy_context_index;           /*index if this display context*/
   int group_index;                   /* index of group it belongs to, or zero if not */

#ifdef OPENGL
   GLdouble ModelMat[16], ProjMat[16];  /* ModelView and Projection matrices */
   struct {
      GLXContext gl_ctx;
      XFontStruct *font;
      GLuint fontbase;
   } gfx;
#endif
#ifdef SGI_GL
   Matrix ModelMat, ProjMat;
   struct {
      unsigned long backcolor;             /* Window background color */
      long winid;                          /* The GL window identifier */
      fmfonthandle font;                   /* Font Manager font ID */
      unsigned int CurColor;
      int init_flag;
   } gfx;
#endif
#ifdef PEX
   int use_db;
   Window db_win;
   int has_blending;

   /* MJK 12.10.98 */
   XFontStruct *FontStruct;
   GC FontGC;
#endif

   char Path[1000];          /* Directory path to topo, map files */


/*************************************************************************************/
/*************************************************************************************/
/*       You are looking at the display_context struct right now                     */
/*************************************************************************************/
/*************************************************************************************/

   /* probe stuff */
   int do_not_recalc_probe_text_width;
   int probe_text_width;

   /*** Stored frame animation from anim.c ***/
   XImage *cache[MAXTIMES];
   int all_invalid;

   /*** Topography File Data (use from topo.c only) ***/
   char TopoName[1000];
   int HiResTopo;
   float Topo_westlon, Topo_eastlon, Topo_northlat, Topo_southlat;
   int Topo_rows, Topo_cols;
   short *TopoData;
   int LatSample, LonSample;
   int TopoFlag;        /* is topography available? */

   /*** Topography Graphics (use from topo.c only ***/
                               /* Topography quadmesh data: */
   int qrows, qcols;           /* Rows and columns */
   float *TopoVertex;          /* Vertices */
   float *TopoNormal;          /* Normal vectors */
   float *TopoTexcoord;        /* Texture coordinates */
   float *TopoFlatVertex;      /* Flat topography vertices */

   int TopoColorVar;           /* which variable colors the topo (or -1) */
   int TopoColorVarOwner;      /* vis5d ctx where TopoColorVar is located */
   uint_1 *TopoIndexes[MAXTIMES+1];  /* Maps topo heights or data */
                                     /* values to color table indexes */

/*************************************************************************************/
/*************************************************************************************/
/*       You are looking at the display_context struct right now                     */
/*************************************************************************************/
/*************************************************************************************/

   int VolRender;
   float MinTopoHgt, MaxTopoHgt;        /* Min and max heights visible */
   int TextureFlag;                     /* Texture maps available? */
   char TextureName[100];               /* Name of texture image file */
   char SequenceName[100];              /* Name of texture sequence file */
   int FirstArea;                       /* Number of 1st McIDAS area file */

   /*** From map.c ***/
   char MapName[1000];
   float MapVert[MAXMAPVERT][3];         /* terrain-following map lines */
   float FlatMapVert[MAXMAPVERT][3];     /* flat map lines */
   int Start[MAXMAPSEG], Len[MAXMAPSEG]; /* start and length of polylines */
   int SegCount;           /* Number of elements in Start[], Len[] */
   int VertCount;          /* Number of elements in MapVert[], FlatMapVert[] */
   float ClipMin0, ClipMax0, ClipMin1, ClipMax1;        /* Clipping bounds */
   int MapFlag;                          /* is map available? */

   /*** Graphics Colors ***/
   unsigned int Color[MAXVARS*VIS5D_MAX_CONTEXTS][6];
   unsigned int HWindColor[VIS5D_WIND_SLICES];
   unsigned int VWindColor[VIS5D_WIND_SLICES];
   unsigned int HStreamColor[VIS5D_WIND_SLICES];
   unsigned int VStreamColor[VIS5D_WIND_SLICES];
   unsigned int TrajColor[VIS5D_TRAJ_SETS];
   unsigned int *CursorColor;
   unsigned int BoxColor;
   unsigned int BgColor;
   unsigned int LabelColor;
   unsigned int LightMapColor;
   unsigned int DarkMapColor;
   unsigned int TextPlotColor[MAXVARS*VIS5D_MAX_CONTEXTS];


/*************************************************************************************/
/*************************************************************************************/
/*       You are looking at the display_context struct right now                     */
/*************************************************************************************/
/*************************************************************************************/




   unsigned int IsoColors[MAXVARS*VIS5D_MAX_CONTEXTS][256]; /* isosurface color tables */
   unsigned int CHSliceColors[MAXVARS*VIS5D_MAX_CONTEXTS][256];  /* data value --> color LUT */
   unsigned int CVSliceColors[MAXVARS*VIS5D_MAX_CONTEXTS][256];  /* data value --> color LUT */
   unsigned int VolumeColors[MAXVARS*VIS5D_MAX_CONTEXTS][256];    /* data value --> color LUT */
   unsigned int TrajColors[MAXVARS*VIS5D_MAX_CONTEXTS][256];  /*trajectory color tables */
   unsigned int TopoColorTable[MAXVARS*VIS5D_MAX_CONTEXTS+1][256];
   unsigned int TextPlotColors[MAXVARS*VIS5D_MAX_CONTEXTS][256];

   /*** colorbar parameters for sine, cosine curves ***/
   float CHSliceParams[MAXVARS*VIS5D_MAX_CONTEXTS][10];
   float CVSliceParams[MAXVARS*VIS5D_MAX_CONTEXTS][10];
   float IsoColorParams[MAXVARS*VIS5D_MAX_CONTEXTS][10];
   float VolumeColorParams[MAXVARS*VIS5D_MAX_CONTEXTS][10];
   float TrajColorParams[MAXVARS*VIS5D_MAX_CONTEXTS][10];
   float TopoColorParams[MAXVARS*VIS5D_MAX_CONTEXTS+1][10];
   float TextPlotColorParams[MAXVARS*VIS5D_MAX_CONTEXTS][10];

   /*** 3-D bounding box ***/
   float Xmin, Xmax;                     /* West, East */
   float Ymin, Ymax;                     /* South, North */
   float Zmin, Zmax;                     /* Bottom, Top */
   float CursorX, CursorY, CursorZ;      /* in graphics coords */
   int CurvedBox;                        /* 0 = rectangular box, 1 = curved */
   float Ax, Ay, Az;                     /* Aspect ratios of box */
   float BoxVerts[MAX_BOX_VERTS][3];
   int NumBoxVerts;
   int TickMarks;
   float TopClip[4];                      /* coordinates for the clipping planes */
   float BottomClip[4];
   float RightClip[4];
   float LeftClip[4];
   float FrontClip[4];
   float BackClip[4];
   float LogoSize;

   struct hclip      HClipTable[2]; /* 0=front 1=back */
   struct vclip      VClipTable[4]; /* 0=top 1=bottom 2=left 3=right */

/*************************************************************************************/
/*************************************************************************************/
/*       You are looking at the display_context struct right now                     */
/*************************************************************************************/
/*************************************************************************************/

 
   float TrajStep, TrajLength;            /* internal values */
   float UserTrajStep, UserTrajLength;    /* user multipliers of above */
   int RibbonFlag;                        /* 2= sounding line 1=ribbons, 0=lines */
   int TrajColorVar[VIS5D_TRAJ_SETS];     /* which var colors the traj set */
   int TrajColorVarOwner[VIS5D_TRAJ_SETS];/*which ctx index the var belongs with*/

   /* MJK 12.02.98 */
   int CircleClock;
   
   int tick_do[12];
   int tick_num[12];
   int tick_type[12];

   struct hwind       HWindTable[VIS5D_WIND_SLICES][MAXTIMES];
   struct vwind       VWindTable[VIS5D_WIND_SLICES][MAXTIMES];
   struct hstream     HStreamTable[VIS5D_WIND_SLICES][MAXTIMES];
   struct vstream     VStreamTable[VIS5D_WIND_SLICES][MAXTIMES];
   struct traj        *TrajTable[MAXTRAJ];
   int NumTraj;


   float HWindLevel[VIS5D_WIND_SLICES];        /* in [0..Nl-1] */
   float HWindZ[VIS5D_WIND_SLICES];
   float HWindHgt[VIS5D_WIND_SLICES];
   float HWindDensity[VIS5D_WIND_SLICES];      /* around 1.0 */
   float HWindScale[VIS5D_WIND_SLICES];        /* around 1.0 */

   float VWindR1[VIS5D_WIND_SLICES];                /* in [0..Nr-1] */
   float VWindC1[VIS5D_WIND_SLICES];                /* in [0..Nr-1] */
   float VWindR2[VIS5D_WIND_SLICES];                /* in [0..Nr-1] */
   float VWindC2[VIS5D_WIND_SLICES];                /* in [0..Nr-1] */
   float VWindX1[VIS5D_WIND_SLICES], VWindY1[VIS5D_WIND_SLICES];
   float VWindX2[VIS5D_WIND_SLICES], VWindY2[VIS5D_WIND_SLICES];
   float VWindLat1[VIS5D_WIND_SLICES], VWindLon1[VIS5D_WIND_SLICES];
   float VWindLat2[VIS5D_WIND_SLICES], VWindLon2[VIS5D_WIND_SLICES];
   float VWindDensity[VIS5D_WIND_SLICES];           /* around 1.0 */
   float VWindScale[VIS5D_WIND_SLICES];             /* around 1.0 */

/*************************************************************************************/
/*************************************************************************************/
/*       You are looking at the display_context struct right now                     */
/*************************************************************************************/
/*************************************************************************************/


   float HStreamLevel[VIS5D_WIND_SLICES];            /* in [0..Nl-1] */
   float HStreamZ[VIS5D_WIND_SLICES];
   float HStreamHgt[VIS5D_WIND_SLICES];
   float HStreamDensity[VIS5D_WIND_SLICES];          /* around 1.0 */

   float VStreamR1[VIS5D_WIND_SLICES];                /* in [0..Nr-1] */
   float VStreamC1[VIS5D_WIND_SLICES];                /* in [0..Nr-1] */
   float VStreamR2[VIS5D_WIND_SLICES];                /* in [0..Nr-1] */
   float VStreamC2[VIS5D_WIND_SLICES];                /* in [0..Nr-1] */
   float VStreamX1[VIS5D_WIND_SLICES], VStreamY1[VIS5D_WIND_SLICES];
   float VStreamX2[VIS5D_WIND_SLICES], VStreamY2[VIS5D_WIND_SLICES];
   float VStreamLat1[VIS5D_WIND_SLICES], VStreamLon1[VIS5D_WIND_SLICES];
   float VStreamLat2[VIS5D_WIND_SLICES], VStreamLon2[VIS5D_WIND_SLICES];
   float VStreamDensity[VIS5D_WIND_SLICES];           /* around 1.0 */
   
   int CurrentVolume;        /* which variable is the current volume, or -1 */
   int CurrentVolumeOwner;

   int PointerX, PointerY;     /* location of mouse fake pointer */
 
   int DisplayHWind[VIS5D_WIND_SLICES];
   int DisplayVWind[VIS5D_WIND_SLICES];
   int DisplayHStream[VIS5D_WIND_SLICES];
   int DisplayVStream[VIS5D_WIND_SLICES];
   int DisplayTraj[VIS5D_TRAJ_SETS];

/*************************************************************************************/
/*************************************************************************************/
/*       You are looking at the display_context struct right now                     */
/*************************************************************************************/
/*************************************************************************************/


   /*** Texture data from images.c ****/
   int TexWidth[MAX_IMAGES];             /* Width of each image */
   int TexHeight[MAX_IMAGES];            /* Height of each image */
   int TexComponents[MAX_IMAGES];        /* Color components in each image */
   unsigned char *TexImage[MAX_IMAGES];  /* Array of images */
   int TexImageNew[MAX_IMAGES];          /* 0= not new, 1=new this is so */
                                         /* vis5d dosn't think it's and old texture */
   int init_flag;
   int prev_time;

   /*** Color legends ***/
   int LegendPosition;
   int LegendSize;
   int LegendMarginX;
   int LegendMarginY;

   /*** From traj.c ***/
   /* Scaling factors to convert U,V,W from meters/sec to grid boxes/sec */
   float Uscale[MAXROWS][MAXCOLUMNS];
   float Vscale[MAXROWS][MAXCOLUMNS];
   float Wscale[MAXLEVELS];

   /*** From misc.c ***/
   int RecentHWind[VIS5D_WIND_SLICES];
   int RecentVWind[VIS5D_WIND_SLICES];
   int RecentHStream[VIS5D_WIND_SLICES];
   int RecentVStream[VIS5D_WIND_SLICES];
   int RecentTraj[VIS5D_TRAJ_SETS];

   long start_sec;       /* timer variables */
   long start_usec;      /* timer variables */
   long lapsed_usec;     /* timer variables */
        
   struct label *FirstLabel;

   int numofctxs;               /* number of ctxs attatched to this display */
   int ctxarray[VIS5D_MAX_CONTEXTS]; /* array of ctx indexs attatched to this display */
   struct vis5d_context *ctxpointerarray[VIS5D_MAX_CONTEXTS]; 

   int numofitxs;               /* number of itxs attatched to this display */
   int itxarray[VIS5D_MAX_CONTEXTS]; /* array of itx indexs attatched to this display */
   struct irregular_context *itxpointerarray[VIS5D_MAX_CONTEXTS];

/*************************************************************************************/
/*************************************************************************************/
/*       You are looking at the display_context struct right now                     */
/*************************************************************************************/
/*************************************************************************************/


   int Nr, Nc, Nl;              /* Size of 3-D grids */
   int LowLev;                  /* Lowest level of 3-D grids */
   int MaxNl;                   /* Maximum of Nl+LowLev array */
   int WindNl;                 /* Min of Nl+LowLev for Uvar, Vvar, Wvar */
   int WindLow;                /* Max of LowLev[Uvar], LowLev[Vvar], LowLev[Wvar] */
   int NumTimes;                /* Number of time steps */

   int Uvar[VIS5D_WIND_SLICES];      /* U variables to use for wind slices */
   int Vvar[VIS5D_WIND_SLICES];      /* V variables to use for wind slices */
   int Wvar[VIS5D_WIND_SLICES];      /* W variables to use for wind slices */
   int TrajU, TrajV, TrajW;          /* Trajectory variables */
 
   int Uvarowner[VIS5D_WIND_SLICES]; /* the vis5d ctx indexs for the owners of the vars */
   int Vvarowner[VIS5D_WIND_SLICES];
   int Wvarowner[VIS5D_WIND_SLICES];
   int TrajUowner, TrajVowner, TrajWowner;
 
   struct dpy_timestep TimeStep[VIS5D_MAX_CONTEXTS*MAXTIMES];
   int TimeStamp[MAXTIMES];   /* Time of each timestep in sec since midnight */
   int DayStamp[MAXTIMES];    /* Day of each timestep in days since 1/1/1900 */
   int Elapsed[VIS5D_MAX_CONTEXTS*MAXTIMES]; /* time in seconds relative to first step */

   char DisplaySfcHWind[VIS5D_WIND_SLICES];     /* display surface winds */
   char DisplaySfcHStream[VIS5D_WIND_SLICES];   /* display surface strmlines */

/*************************************************************************************/
/*************************************************************************************/
/*       You are looking at the display_context struct right now                     */
/*************************************************************************************/
/*************************************************************************************/


   int NumVars;                 /* Number of variables */

   int CurTime;                 /* current time step in [0..NumTimes-1] */
   int Redraw;                  /* Used to trigger 3-D window redraw */

   int Animateing;              /* 1/-1 means this dtx is currently animating */
                                /* along side a bunch of other dtx's which are */
                                /* grouped together. */

   int init_cursor_flag;        /* if 1 then draw the primatives for */
                                /* the cursor, if 0 then they've already */
                                /* been drawn */
   int Projection;              /* One of PROJ_*, as above */
   int UserProjection;          /* Optional override of projection from file */
   float *UserProjArgs;         /* Optional projection args for override */
   float NorthBound;            /* See proj.c for how each of these */
   float SouthBound;            /* variables is used depending on the */
   float WestBound;             /* value of the Projection variable. */
   float EastBound;
   float RowInc;
   float ColInc;
   float Lat1, Lat2;
   float PoleRow, PoleCol;
   float CentralLat;
   float CentralLon;
   float CentralRow;
   float CentralCol;
   float Rotation;             /* radians */
   float Cone;
   float Hemisphere;
   float ConeFactor;
   float CosCentralLat, SinCentralLat;
   float StereoScale, InvScale;
   float CylinderScale;
   float RowIncKm;
   float ColIncKm; 

   int VerticalSystem;          /* One of VERT_*, as above */
   int UserVerticalSystem;      /* Optional override of vert. sys. from file */
   float *UserVertArgs;         /* Optional vert args for override */
   float LevInc;                /* Increment between levels */
   float BottomBound;           /* Same as Height[0] */
   float TopBound;              /* Same as Height[MaxNl-1] */
   float Height[MAXLEVELS];     /* Height of each grid layer */
   int LogFlag;                 /* set to indicate log display */
   float LogScale;              /* for VERT_LOG_KM: */
   float LogExp;                /* "" */
   float Ptop, Pbot;            /* "" */

/*************************************************************************************/
/*************************************************************************************/
/*       You are looking at the display_context struct right now                     */
/*************************************************************************************/
/*************************************************************************************/


   int GfxProjection;           /* 0 = parallel, 1 = perspective */
   float Zoom;                  /* View scale (zoom) factor */
   float FrntClip;             /* location of front clip plane */
   MATRIX  CTM;                 /* Current Transformation Matrix */
/* MJK 12.21.98 there are in graphics.h 
   Window BigWindow;
   int BigWinWidth, BigWinHeight;
*/



   Window GfxWindow;            /* the X window */
   /* MJK 11.19.98 */   
   Pixmap GfxPixmap;

   int WinWidth, WinHeight;     /* size of the window */
   float LineWidth;             /* width of lines in pixels */

   /* MJK 2.22.99 */
   int ContFontFactorX;          /* increase/decrease contour font by this in X dir */
   int ContFontFactorY;          /* increase/decrease contour font by this in Y dir*/

   char FontName[100];          /* Name of font */
   int FontHeight;              /* Height of font in pixels */
   int FontDescent;             /* Pixels from top to baseline */
   char SoundFontName[100];     /* Name of sounding window font */
   int AlphaBlend;              /* Do alpha blended transparency? */
   int DepthCue;                /* Do depth cueing? */
   int Perspective;

   /* MJK 12.02.98 */
   int UserTopoFlag;            /* use user func to read topo data */
   int UserMapsFlag;            /* use user func to read map data */
   

   int VolumeFlag;
   int ContnumFlag;      /* display contour numbers? */
   int DisplayTopo;      /* display topography? */
   int DisplayMap;       /* display map? */
   int DisplayTexture;   /* display texture images? */
   int CoordFlag;        /* coordinate display units:  0=geographic, 1=grid */
   int DisplayBox;       /* Display 3-D box? */
   int DisplayClock;     /* Display clock? */
   int PrettyFlag;       /* 'Pretty' rendering mode? */
   int DisplayInfo;      /* Display information? */
   int DisplayProbe;     /* Probe display flag? */
   int DisplayProbeOnTraj; /* Probe on display flag? */
   int DisplaySound;     /* Sounding display flag */
   int DisplayClips;     /* display clipping plane squares? */
   int DisplayCursor;    /* Display cursor? */
   int DisplayLegends;   /* Display colorbar legends? */
   int Reversed;         /* White Background? */
   int JulianDate;       /* Display Julian date (i.e., yyddd)? */
   int WindBarbs;        /* Display wind barbs rather than vectors? */


/*************************************************************************************/
/*************************************************************************************/
/*       You are looking at the display_context struct right now                     */
/*************************************************************************************/
/*************************************************************************************/


   int AnimRecord;             /* Record animation flag */

   /*** make TEMPORARY sounding struct ***/
   struct sound Sound;


   v5dstruct G;                 /* File header informations */


   /* MJK 12.02.98 begin */
   int DisplayTopoBase;         /* display a base under topography */
   float TopoBaseLev;           /* level value of the topo base */
   int_2 *TopoStripsVerts;      /* Topo triangle strip vertices */
   int_1 *TopoStripsNorms;      /* Topo triangle strip normals */
   char DisplaySfcMap;          /* display surface map */
   /* MJK 12.02.98 end */

};


struct display_group{
   int index;
   struct display_context *dpyarray[VIS5D_MAX_DPY_CONTEXTS];
   int numofdpys;
   struct dpy_timestep TimeStep[VIS5D_MAX_CONTEXTS*MAXTIMES];
   int NumTimes;
   int CurTime;
   int Animateing;
};


struct cache_irreg_rec {
   int    *DataType;
   double *Value;
   double *SoundingValue;
   float  *SoundingLevel;
   char   *CharData;

   int Locked;                     /* 1 = position is in use, 0 = not in use */
   int Timestep;
   int Rec;
   int Age;                         /* for LRU replacement (large Age == Newer) */
};

struct irreg_rec {
   int    *DataType;
   double *Value;
   double *SoundingValue;
   float  *SoundingLevel;
   char   *CharData;

   int CachePos;
};

struct rec_geo_position {
   float Latitude;
   float Longitude;
   float Altitude;
};

#define MAX_VAR_LENGTH 20

struct irregular_context {

   /*** Miscellaneous ***/
   int context_index;        /* index of this context */
   char ItxName[100];      /* Name of file we're vizing */
   char Path[1000];          /* Directory path to topo, map files */
   char DataFile[1000];      /* Name of file we're vizing */
   int MegaBytes;            /* Memory limit in megabytes */

   int Type;

   LOCK Mutex;
   struct cache_irreg_rec     *RecordCache;
   int CharPointer[MAXVARS];
   int CharArraySize;
   int SoundingPointer[MAXVARS];
   int CacheClock;

   struct rec_geo_position    *RecGeoPosition[MAXTIMES];

   int Levels;
   int NumRecs[MAXTIMES];
   int MaxCachedRecs;
   int NumCachedRecs;
   struct irreg_rec           *RecordTable[MAXTIMES];

   int *RecId[MAXTIMES];

   int PreloadCache;

   /*** Memory ***/
   void *mempool;
   struct mem *head, *tail;
   struct mem *guess;
   int memory_limit;
   int memory_used;
   LOCK memlock;
   LOCK lrulock;

   irregular_v5dstruct G;

   int NumTimes;                /* Number of time steps */
   int NumVars;                 /* Number of variables */
   int NumLocations;
   char VarName[MAXVARS][MAX_VAR_LENGTH];
   int  VarType[MAXVARS];       /* 0 if float 1 if char */
   int CharVarLength[MAXVARS];
   int CharArrayLength;
   float MinVal[MAXVARS];       /* Min value for each parameter */
   float MaxVal[MAXVARS];       /* Max value for each parameter */
   float RealMinVal[MAXVARS];       /* Min value for each parameter */
   float RealMaxVal[MAXVARS];       /* Max value for each parameter */
   int TimeStamp[MAXTIMES];   /* Time of each timestep in sec since midnight */
   int DayStamp[MAXTIMES];    /* Day of each timestep in days since 1/1/1900 */
   int Elapsed[MAXTIMES];     /* time in seconds relative to first step */
   int CurTime;


   float WestBound, EastBound;
   float NorthBound, SouthBound;
   float TopBound, BottomBound;

   /*** display_context pointer ***/
   struct display_context *dpy_ctx;

   int TextPlotVar;
   float TextPlotSpacing;
   float TextPlotFontX;
   float TextPlotFontY;
   float TextPlotFontSpace;

   int TextPlotColorStatus[MAXVARS];

   struct textplot    TextPlotTable[MAXTIMES];

   int DisplayTextPlot;

};




/*** NEW API stuff starts here ***/


struct vis5d_context {
   /*** Miscellaneous ***/
   int context_index;        /* index of this context */
   char DataFile[1000];      /* Name of file we're vizing */
   char Path[1000];          /* Directory path to topo, map files */
   int MegaBytes;            /* Memory limit in megabytes */
   int InsideInit;           /* Are we between init_begin & init_end? */
   char ContextName[100];     /* name of the context */
   int GridSameAsGridPRIME;    /* 1 grid=gridPRIME    0 grid != gridPRIME */
   /*** Data Set / Grid parameters ***/
   int Nr, Nc, Nl[MAXVARS];  /* Size of 3-D grids */
   int LowLev[MAXVARS];      /* Lowest level of 3-D grids */
   int MaxNl;                /* Maximum of Nl+LowLev array */
   int MaxNlVar;             /* Which variable has max Nl+LowLev */
   int WindNl;            /* Min of Nl+LowLev for Uvar, Vvar, Wvar */
   int WindLow;           /* Max of LowLev[Uvar], LowLev[Vvar], LowLev[Wvar] */
   int NumTimes;                /* Number of time steps */
   int NumVars;                 /* Number of variables */
   char VarName[MAXVARS][10];
   int VarType[MAXVARS];        /* one of VIS5D_* from api.h */
   int CloneTable[MAXVARS];     /* Clone-to-real var translation table */
   float MinVal[MAXVARS];       /* Min value for each parameter */
   float MaxVal[MAXVARS];       /* Max value for each parameter */
   float RealMinVal[MAXVARS];       /* Group Min value for each parameter */
   float RealMaxVal[MAXVARS];       /* Group Max value for each parameter */
   char Units[MAXVARS][20];     /* Physical units for each variable */

   int Uvar[VIS5D_WIND_SLICES]; /* U variables to use for wind slices */
   int Vvar[VIS5D_WIND_SLICES]; /* V variables to use for wind slices */
   int Wvar[VIS5D_WIND_SLICES]; /* W variables to use for wind slices */
   int TrajU, TrajV, TrajW;     /* Trajectory variables */
   float TrajStep, TrajLength;            /* internal values */

   int TimeStamp[MAXTIMES];   /* Time of each timestep in sec since midnight */
   int DayStamp[MAXTIMES];    /* Day of each timestep in days since 1/1/1900 */
   int Elapsed[MAXTIMES];     /* time in seconds relative to first step */


   /*** Type-in expressions ***/
   char ExpressionList[MAXVARS][500];

   /*** display_context pointer ***/
   struct display_context *dpy_ctx;

   /*** Current graphics parameters ***/
   float IsoLevel[MAXVARS];              /* in [MinVal..MaxVal] */

   float HSliceInterval[MAXVARS];
   float HSliceLowLimit[MAXVARS];        /* in [MinVal..MaxVal] */
   float HSliceHighLimit[MAXVARS];       /* in [MinVal..MaxVal] */
   float HSliceLevel[MAXVARS];           /* in [0..Nl-1] */
   float HSliceZ[MAXVARS];               /* position in Z coords */
   float HSliceHgt[MAXVARS];             /* position in hgt coords */

   float VSliceInterval[MAXVARS];
   float VSliceLowLimit[MAXVARS];        /* in [MinVal..MaxVal] */
   float VSliceHighLimit[MAXVARS];       /* in [MinVal..MaxVal] */
   float VSliceR1[MAXVARS];              /* in [0..Nr-1] */
   float VSliceC1[MAXVARS];              /* in [0..Nc-1] */
   float VSliceR2[MAXVARS];              /* in [0..Nr-1] */
   float VSliceC2[MAXVARS];              /* in [0..Nc-1] */
   float VSliceX1[MAXVARS], VSliceY1[MAXVARS];
   float VSliceX2[MAXVARS], VSliceY2[MAXVARS];
   float VSliceLat1[MAXVARS], VSliceLon1[MAXVARS];
   float VSliceLat2[MAXVARS], VSliceLon2[MAXVARS];

   float CHSliceLevel[MAXVARS];          /* in [0..Nl-1] */
   float CHSliceZ[MAXVARS];
   float CHSliceHgt[MAXVARS];

   float CVSliceR1[MAXVARS];                /* in [0..Nr-1] */
   float CVSliceC1[MAXVARS];                /* in [0..Nc-1] */
   float CVSliceR2[MAXVARS];                /* in [0..Nr-1] */
   float CVSliceC2[MAXVARS];                /* in [0..Nc-1] */
   float CVSliceX1[MAXVARS], CVSliceY1[MAXVARS];
   float CVSliceX2[MAXVARS], CVSliceY2[MAXVARS];
   float CVSliceLat1[MAXVARS], CVSliceLon1[MAXVARS];
   float CVSliceLat2[MAXVARS], CVSliceLon2[MAXVARS];

   struct volume *Volume;                /* The volume graphics stuff */

   int IsoColorVar[MAXVARS];
   int IsoColorVarOwner[MAXVARS];        /* which vis5d_ctx the colorvar belongs to */
   int SameIsoColorVarOwner[MAXVARS];    /* 0 = color var ctx isn't same as ctx
                                            1 = colr var ctx same as ctx */
   int WasSameIsoColorVarOwner[MAXVARS]; /* need this for memeory dealloc purposes */

   /*** User Interface parameters ***/
   int CurTime;                /* current time step in [0..NumTimes-1] */
   int AnimRecord;             /* Record animation flag */
   int Redraw;                 /* Used to trigger 3-D window redraw */


   /*** Graphics display flags ***/
   int DisplaySurf[MAXVARS];
   int DisplayHSlice[MAXVARS];
   int DisplayVSlice[MAXVARS];
   int DisplayCHSlice[MAXVARS];
   int DisplayCVSlice[MAXVARS];

   /*** Tables of graphics data ***/
   struct isosurface  SurfTable[MAXVARS][MAXTIMES];
   struct hslice      HSliceTable[MAXVARS][MAXTIMES];
   struct vslice      VSliceTable[MAXVARS][MAXTIMES];
   struct chslice     CHSliceTable[MAXVARS][MAXTIMES];
   struct cvslice     CVSliceTable[MAXVARS][MAXTIMES];

   /* MJK 12.02.98 */
   int UserDataFlag;            /* use user func to read data & header */


   /*** Map projection and vertical coordinate system ***/
   int Projection;              /* One of PROJ_*, as above */
   int UserProjection;          /* Optional override of projection from file */
   float *UserProjArgs;         /* Optional projection args for override */
   float NorthBound;            /* See proj.c for how each of these */
   float SouthBound;            /* variables is used depending on the */
   float WestBound;             /* value of the Projection variable. */
   float EastBound;
   float RowInc;
   float ColInc;
   float Lat1, Lat2;
   float PoleRow, PoleCol;
   float CentralLat;
   float CentralLon;
   float CentralRow;
   float CentralCol;
   float Rotation;             /* radians */
   float Cone;
   float Hemisphere;
   float ConeFactor;
   float CosCentralLat, SinCentralLat;
   float StereoScale, InvScale;
   float CylinderScale;
   float RowIncKm;
   float ColIncKm;

   int VerticalSystem;          /* One of VERT_*, as above */
   int UserVerticalSystem;      /* Optional override of vert. sys. from file */
   float *UserVertArgs;         /* Optional vert args for override */
   float LevInc;                /* Increment between levels */
   float BottomBound;           /* Same as Height[0] */
   float TopBound;              /* Same as Height[MaxNl-1] */
   float Height[MAXLEVELS];     /* Height of each grid layer */
   int LogFlag;                 /* set to indicate log display */
   float LogScale;              /* for VERT_LOG_KM: */
   float LogExp;                /* "" */
   float Ptop, Pbot;            /* "" */

   /*** Memory ***/
   void *mempool;
   struct mem *head, *tail;
   struct mem *guess;
   int memory_limit;
   int memory_used;
   LOCK memlock;
   LOCK lrulock;
   int meminited;

#ifdef OPENGL
   GLdouble ModelMat[16], ProjMat[16];  /* ModelView and Projection matrices */
   struct {
      GLXContext gl_ctx;
      XFontStruct *font;
      GLuint fontbase;
   } gfx;
#endif
#ifdef SGI_GL
   Matrix ModelMat, ProjMat;
   struct {
      unsigned long backcolor;             /* Window background color */
      long winid;                          /* The GL window identifier */
      fmfonthandle font;                   /* Font Manager font ID */
      unsigned int CurColor;
      int init_flag;
   } gfx;
#endif
#ifdef PEX
   int use_db;
   Window db_win;
   int has_blending;

#endif




   /*** External function info from analysis.c ****/
   int ExtFuncErrorFlag;
   SEMAPHORE ExtFuncDoneSem;
   int ExtFuncSocket[MAX_THREADS];
   float ProbeRow, ProbeCol, ProbeLev;


   /*** Grid info from grid.c ***/
   int McFile[MAXTIMES][MAXVARS];   /* Origin of each compressed grid */
   int McGrid[MAXTIMES][MAXVARS];   /*  as a McIDAS file and grid number. */
   float *Ga[MAXTIMES][MAXVARS];
   float *Gb[MAXTIMES][MAXVARS];
   int CompressMode;  /* compression mode (1, 2 or 4 bytes per grid point */
   v5dstruct G;       /* File header information */
   LOCK Mutex;        /* Mutual exclusion lock for grid/cache access */
   /* array of cache_rec structs is used to manage the contents of the cache */
   struct cache_rec *GridCache;     /* Dynamically allocated array */
   int MaxCachedGrids;              /* No. positionss in GridCache array */
   int NumCachedGrids;              /* Number of positions in use */
   int CacheClock;                  /* To implement LRU replacement */
   /* An array of grid_rec structs is used to determine if (and where)
      a grid is in the cache given a timestep and variable. */
   struct grid_rec GridTable[MAXTIMES][MAXVARS];
   int PreloadCache;        /* Preload cache with data? */
   int VeryLarge;           /* must sync graphics generation with rendering */



   /*** From labels.c ***/

   /*** From map.c ***/
   char MapName[1000];
   float MapVert[MAXMAPVERT][3];         /* terrain-following map lines */
   float FlatMapVert[MAXMAPVERT][3];     /* flat map lines */
   int Start[MAXMAPSEG], Len[MAXMAPSEG]; /* start and length of polylines */
   int SegCount;           /* Number of elements in Start[], Len[] */
   int VertCount;          /* Number of elements in MapVert[], FlatMapVert[] */
   float ClipMin0, ClipMax0, ClipMin1, ClipMax1;        /* Clipping bounds */
   int MapFlag;                          /* is map available? */

   /*** From traj.c ***/
   /* Scaling factors to convert U,V,W from meters/sec to grid boxes/sec */
   float Uscale[MAXROWS][MAXCOLUMNS];
   float Vscale[MAXROWS][MAXCOLUMNS];
   float Wscale[MAXLEVELS];

   /*** From misc.c ***/
   int RecentIsosurf[MAXVARS];
   int RecentHSlice[MAXVARS];
   int RecentVSlice[MAXVARS];
   int RecentCHSlice[MAXVARS];
   int RecentCVSlice[MAXVARS];

   /* MJK 12.01.98 */
   int HSliceLinkPrev[MAXVARS][2];      /* links to tie slices together */
   int HSliceLinkNext[MAXVARS][2];
   int VSliceLinkPrev[MAXVARS][2];
   int VSliceLinkNext[MAXVARS][2];
   int CHSliceLinkPrev[MAXVARS][2];
   int CHSliceLinkNext[MAXVARS][2];
   int CVSliceLinkPrev[MAXVARS][2];
   int CVSliceLinkNext[MAXVARS][2];
   int HWindLinkPrev[VIS5D_WIND_SLICES][2];
   int HWindLinkNext[VIS5D_WIND_SLICES][2];
   int VWindLinkPrev[VIS5D_WIND_SLICES][2];
   int VWindLinkNext[VIS5D_WIND_SLICES][2];
   int HStreamLinkPrev[VIS5D_WIND_SLICES][2];
   int HStreamLinkNext[VIS5D_WIND_SLICES][2];
   int VStreamLinkPrev[VIS5D_WIND_SLICES][2];
   int VStreamLinkNext[VIS5D_WIND_SLICES][2];

   /* MJK 12.02.98 */
   int ProbeNumVars;                    /* number of probe variables */
   int ProbeVar[MAXVARS];               /* probe varible display flags */

   /* MJK 12.04.98 */
   char DisplaySfcHSlice[MAXVARS];              /* display surface contours */

};

typedef struct vis5d_context *Context;
typedef struct display_context *Display_Context;
typedef struct display_group *Display_Group;
typedef struct irregular_context *Irregular_Context;

#endif
