/* Vis5D version 5.2 */

/*
Vis5D system for visualizing five dimensional gridded data sets
Copyright (C) 1990 - 2000 Bill Hibbard, Brian Paul, Dave Santek,
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


#ifndef API_H
#define API_H


#include <X11/Xlib.h>
#ifdef SGI_GL
#  include "gl/glws.h"
#endif
#ifdef OPENGL
#include "GL/gl.h"
#include "GL/glu.h"
#include "GL/glx.h"
#endif

/*
 * Including v5d.h here may seem like a kludge but the v5d.h file
 * actually defines a programmer's API too.
 */
#include "v5d.h"

/* MJK 12.07.98 */
#define VIS5D_IGNORE          -99999
#define VIS5D_SND_MIXRAT      944
#define VIS5D_SND_TEMP        945


/* Max number of simultaneous Vis5D API contexts: */
#define VIS5D_MAX_CONTEXTS 20 
#define VIS5D_MAX_DPY_CONTEXTS 20 

/* error codes for "vis5d_..." API functions */
#define VIS5D_BAD_CONTEXT       -1
#define VIS5D_BAD_CONSTANT      -2
#define VIS5D_BAD_MODE          -3
#define VIS5D_BAD_VALUE         -4
#define VIS5D_BAD_VAR_NUMBER    -5
#define VIS5D_BAD_TIME_STEP     -6
#define VIS5D_FAIL              -7
#define VIS5D_OUT_OF_MEMORY     -8


/*** Constants ***/
#define VIS5D_ISOSURF           0
#define VIS5D_HSLICE            1 
#define VIS5D_VSLICE            2 
#define VIS5D_CHSLICE           3
#define VIS5D_CVSLICE           4 
#define VIS5D_VOLUME            5
#define VIS5D_HWIND             6 
#define VIS5D_VWIND             7
#define VIS5D_HSTREAM           8
#define VIS5D_VSTREAM           9
#define VIS5D_TEXTPLOT          10
#define VIS5D_TRAJ              80
#define VIS5D_TOPO              90
#define VIS5D_BOX              100
#define VIS5D_BACKGROUND       110
#define VIS5D_LABEL            120
#define VIS5D_CLOCK            130
#define VIS5D_MAP              140
#define VIS5D_PERSPECTIVE      150
#define VIS5D_CONTOUR_NUMBERS  160
#define VIS5D_GRID_COORDS      170
#define VIS5D_PRETTY           180
#define VIS5D_INFO             200
#define VIS5D_PROBE            210
#define VIS5D_CURSOR           220
#define VIS5D_ANIMRECORD       230
#define VIS5D_NORTH            240
#define VIS5D_SOUTH            250
#define VIS5D_EAST             260
#define VIS5D_WEST             270
#define VIS5D_TOP              280
#define VIS5D_BOTTOM           290
#define VIS5D_TEXTURE          300
#define VIS5D_DEPTHCUE         310
#define VIS5D_LEGENDS          340
#define VIS5D_JULIAN           350
#define VIS5D_BARBS            360
#define VIS5D_RIGHT            370
#define VIS5D_LEFT             380
#define VIS5D_SOUND            390
#define VIS5D_SND_THTA         400
#define VIS5D_SND_THTE         410
#define VIS5D_SND_W            420
#define VIS5D_SND_TICKS        430
#define VIS5D_REVERSE          440
#define VIS5D_CLIP             450
#define VIS5D_FIRSTAREA        460
#define VIS5D_ALPHAMODE        470
#define VIS5D_HIRESTOPO        480
#define VIS5D_SAMESCALE        490
#define VIS5D_LEGENPOSITION    500
#define VIS5D_LEGENDSIZE       510
#define VIS5D_LIGHT_MAP        520		
#define VIS5D_DARK_MAP         530
#define VIS5D_PROBE_ON_TRAJ    540


/* These are the defines used in the function
   vis5d_set_dtx_projection */
#define SET_PROJ_Projection       1
#define SET_PROJ_NorthBound       2
#define SET_PROJ_WestBound        4
#define SET_PROJ_RowInc           6
#define SET_PROJ_ColInc           7
#define SET_PROJ_Lat1             8
#define SET_PROJ_Lat2             9
#define SET_PROJ_PoleRow          10
#define SET_PROJ_PoleCol          11
#define SET_PROJ_CentralLat       12
#define SET_PROJ_CentralLon       13
#define SET_PROJ_CentralRow       14
#define SET_PROJ_CentralCol       15
#define SET_PROJ_Rotation         16
#define SET_PROJ_CylinderScale    24
#define SET_PROJ_RowIncKm         25
#define SET_PROJ_ColIncKm         26

#define SET_PROJ_VerticalSystem   50
#define SET_PROJ_LevInc           51
#define SET_PROJ_BottomBound      52
#define SET_PROJ_Height           54

#define SET_PROJ_NumRows          60
#define SET_PROJ_NumCols          61
#define SET_PROJ_NumLevs          71

#define SET_PROJ_Done             100


/* Clipping Plane Parts */
#define TOP      1              
#define BOTTOM   2
#define LEFT     3
#define RIGHT    4
#define BACK     5
#define FRONT    6


/* Variable types: */
#define VIS5D_REGULAR         90
#define VIS5D_CLONE           91
#define VIS5D_EXT_FUNC        92
#define VIS5D_EXPRESSION      93
#define VIS5D_PUT             94

/* modes for vis5d_graphics_mode */
#define VIS5D_OFF           0
#define VIS5D_ON            1
#define VIS5D_TOGGLE        2
#define VIS5D_GET           3


/* Map projections and vertical coordinate systems: */
#define PROJ_GENERIC          0  /* No specific units */
#define PROJ_LINEAR           1  /* Cylindrical-Equidistant (old vis5d) */
#define PROJ_LAMBERT          2  /* Lambert conformal */
#define PROJ_STEREO           3  /* Stereographic */
#define PROJ_ROTATED          4  /* Rotated equidistant */
#define PROJ_MERCATOR         5  /* Mercator */
#define PROJ_CYLINDRICAL     20  /* Cylindrical projection of cyl-equid. */
#define PROJ_SPHERICAL       21  /* Spherical projection of cyl-equid. */

#define VERT_GENERIC          0 /* No specific units */
#define VERT_EQUAL_KM         1 /* Equally spaced in kilometers */
#define VERT_NONEQUAL_KM      2 /* Non-equally spaced in kilometers */
#define VERT_NONEQUAL_MB      3 /* Non-equally spaced in millibars */


/* Image save formats: */
#define VIS5D_RGB       1
#define VIS5D_GIF       2
#define VIS5D_XWD       4
#define VIS5D_PS        8
#define VIS5D_COLOR_PS 16
/* MJK 11.19.98 */
#define VIS5D_PPM      32
#define VIS5D_TGA      64
/* Number of trajectory sets */
#define VIS5D_TRAJ_SETS   8

/* Number of wind slices and streams */
#define VIS5D_WIND_SLICES 2


/* Color table parameters */
#define DEFAULT_CURVE     1.4
#define DEFAULT_BIAS      1.0
#define DEFAULT_ALPHAPOW  2.0

  /* **** These values must match the ones in lui/colorbar.h !!!! **** */
#define CURVE    0
#define BIAS     1
#define ALPHAPOW 2
#define ALPHAVAL 3
#define DRAWFLAG 4
#define MINALPHA 5
#define MAXALPHA 6
#define MAX_TABLE 1000

/* MJK 11.19.98 */
extern int off_screen_rendering;

extern int REVERSE_POLES;

extern int in_the_init_stage;

/* MJK 4.27.99 */
extern char Vis5dDataPath[1000];
extern char Vis5dFuncPath[1000];

/*
 * Initialization functions
 */


extern int vis5d_initialize( int cavemode );

extern int vis5d_terminate( int close_window );

extern int _dtx_ctx( int index, int vindex);

extern int vis5d_workers( int nworkers );

extern int vis5d_get_image_formats( int *formats );

extern int vis5d_create_display_context( int index );

extern int vis5d_alloc_data_context( void );

extern int vis5d_alloc_display_context( void );

extern int vis5d_alloc_irregular_data_context( void );

extern int vis5d_destroy_data_context( int index );

extern int vis5d_destroy_display_context( int index);

extern int vis5d_reset_display_context( int index );

extern int vis5d_init_begin( int index, int dindex );

/* New 5.2 */
extern int vis5d_init_data_end( int index );

extern int vis5d_get_num_of_ctxs_in_display( int index, int *number, int numarray[]);

extern int vis5d_get_num_of_dtxs_in_group( int index, int *number, int numarray[]);

extern int vis5d_map_3d_window( int index );

extern int vis5d_unmap_3d_window( int index );

extern int vis5d_get_display_window( int index, Window *win);

extern int vis5d_init_window( char *title,
                                     int x, int y, int width, int height );

extern int vis5d_get_ctx_values( int index, v5dstruct *v5d);

extern int vis5d_get_dtx_values( int index, v5dstruct *v5d);


extern int vis5d_assign_window_to_display( int display, Window win, int width,
                                           int height);
extern int vis5d_check_dtx_same_as_ctx( int dindex, int vindex );

extern int vis5d_set_dtx_values( int index, v5dstruct *v5d);

/* New 5.2 */
extern int vis5d_init_display_values( int index, int iindex, int display );

extern int vis5d_init_sndwindow( int index, char *title,
                                     int x, int y, int width, int height,
                                     Window scw, char *wdpy_name );
extern int vis5d_map_sndwindow( int index);

extern int vis5d_unmap_sndwindow( int index );

#ifdef SGI_GL
extern int vis5d_init_gl_window( int index, Display *dpy, Window window,
                                       long winid );

extern int vis5d_init_glx_window( int index, Display *dpy,
                                        Window window, GLXconfig *ctx );
#endif


#ifdef OPENGL
extern int vis5d_init_opengl_window( int index, Display *dpy,
                                           Window window, GLXContext ctx );
#endif


extern int vis5d_init_map( int index, char *mapname );

extern int vis5d_get_map( int index, char *mapname );

extern int vis5d_init_topo_and_map_ctx( int index, char *toponame, int highres_flag );

extern int vis5d_init_topo( int index, char *toponame,
                                   int highres_flag );

extern int vis5d_get_topo( int index, char *toponame);

extern int vis5d_init_clock( int index, int clock);

extern int vis5d_init_texture( int index, char *texturename );

extern int vis5d_get_texture( int index, char *texturename );

extern int vis5d_init_firstarea( int index, int area );

extern int vis5d_get_firstarea( int index );

extern int vis5d_init_sequence( int index, char *sequencename );

extern int vis5d_get_sequence( int index, char *sequencename );

extern int vis5d_init_log( int index, int flag, float scale, float exponent );

extern int vis5d_get_log( int index, int *flag, float *scale, float *exponent);

extern int vis5d_init_box( int index, float alon,
                                  float alat, float ahgt );

extern int vis5d_make_box( int index, float x, float y, float z);

extern int vis5d_init_projection( int index, int projection,
                                         float *projargs );

extern int vis5d_init_vertical( int index, int vertical,
                                       float *vertargs );



extern int vis5d_init_memory( int index, int mbs );

extern int vis5d_init_samescale( int index );

/* MJK 4.27.99 */
extern int vis5d_init_path( char *pathname );

extern int vis5d_get_v5dfilename( int index, char *name );

extern int vis5d_query_gridfile( char *name, v5dstruct *v);

extern int vis5d_open_gridfile( int index, char *name, int read_flag );



/*
 * Post-initialization functions
 */

extern int vis5d_get_levels( int index, int var);

/* Time functions */

extern int vis5d_get_dtx_numtimes( int index, int *numtimes );

extern int vis5d_get_grp_numtimes( int index, int *numtimes );

extern void get_timer(int which);

extern int vis5d_get_ctx_numtimes( int index, int *numtimes );

extern int vis5d_get_dtx_time_stamp( int index, int timestep,
                                     int *day, int *time);

extern int vis5d_get_ctx_time_stamp( int index, int timestep,
                                        int *day, int *time );

extern int vis5d_set_ctx_time_stamp( int index, int timestep,
                                        int day, int time );

extern int vis5d_set_grp_timestep( int index, int curtime );

extern int vis5d_signal_group_redraw( int index, int time);

extern int vis5d_get_grp_timestep( int index, int *curtime);

extern int vis5d_set_dtx_timestep( int index, int curtime );

extern int vis5d_get_dtx_timestep( int index, int *curtime);

extern int vis5d_get_ctx_timestep( int index, int *curtime );

extern int vis5d_get_itx_timestep( int index, int *curtime );

extern int vis5d_set_ctx_timestep( int index, int curtime );

extern int vis5d_get_ctx_display_index(int index, int *display_index);

extern int vis5d_get_itx_display_index(int index, int *display_index);


/*** Clipping Functions ***/

extern int vis5d_set_hclip( int index, int num, float level);

extern int vis5d_set_vclip( int index, int num, float r1, float c1,
                     float r2, float c2);

extern int vis5d_get_hclip( int index, int num, float *level );

extern int vis5d_get_vclip( int index, int num, float *r1, float *c1,
                     float *r2, float *c2);

extern int vis5d_set_clip_mode( int index, int clip, int mode );

extern int vis5d_get_clip_mode( int index, int clip, int *mode);


/* Variable functions */

extern int vis5d_get_ctx_numvars( int index, int *numvars );

extern int vis5d_get_itx_numvars( int index, int *numvars );

extern int vis5d_get_display_numvars( int index, int *numvars );

extern int vis5d_find_var( int index, char *name );

extern int vis5d_get_ctx_var_name( int index, int var, char *name );

extern int vis5d_get_itx_var_name( int index, int var, char *name );

extern int vis5d_get_itx_name( int index, char *name );

extern int vis5d_get_var_units( int index, int var, char *units );

extern int vis5d_get_var_type( int index, int var, int *type );

extern int vis5d_get_var_info( int index, int var, void *info );

extern int vis5d_get_ctx_var_range( int index, int var,
                                       float *min, float *max );
 
extern int vis5d_get_itx_var_range( int index, int var,
                                       float *min, float *max );

extern int vis5d_set_var_range( int index, int var,
                                       float min, float max );

extern int vis5d_set_sound_vars( int index, int tempowner, int temp, int dewptowner, int dewpt,
                                 int uwindowner, int uwind, int vwindowner, int vwind,
                                 int var1owner, int var1, int var2owner, int var2,
                                 int var3owner,  int var3);

extern int vis5d_get_sound_vars( int index, int *tempowner, int *temp, int *dewptowner, int *dewpt, 
                                 int *uwindowner, int *uwind, int *vwindowner, int *vwind,
                                 int *var1owner, int *var1, int *var2owner, int *var2, 
                                 int *var3owner,  int *var3);


extern int vis5d_get_wind_vars( int index, 
                                       int *owner1, int *uvar, int *owner2, 
                                       int *vvar, int *owner3, int *wvar,
                                       int *owner4, int *u2var, int *owner5,
                                       int *v2var, int *owner6, int *w2var,
                                       int *owner7, int *traju, int *owner8,
                                       int *trajv, int *owner9, int *trajw );

extern int vis5d_set_wind_vars( int index,
                                       int owner1, int uvar, int owner2, 
                                       int vvar, int owner3, int wvar,
                                       int owner4, int u2var, int owner5,
                                       int v2var, int owner6, int w2var,
                                       int owner7, int traju, int owner8,
                                       int trajv, int owner9, int trajw );


extern int vis5d_reset_var_graphics( int index, int var );


/* Grid functions */

extern int vis5d_get_sizePRIME( int index, int *nr, int *nc, int *nl,
                                  int *lowlev, 
                                  int *windnl, int *windlow );

extern int vis5d_get_size( int index, int *nr, int *nc, int nl[],
                                  int lowlev[], int *maxnl, int *maxnlvar,
                                  int *windnl, int *windlow );

extern int vis5d_get_grid( int index, int time, int var, float *data );

extern int vis5d_put_grid( int index, int time, int var, float *data );

extern int vis5d_get_grid_value( int index, int var,
                                 float row, float column, float level,
                                 float *value );


extern int vis5d_verylarge_mode( int index, int mode );

/* Map projection and VCS functions */

extern int vis5d_get_ctx_projection( int index, int *projection,
                                        float *projargs );

extern int vis5d_get_dtx_projection( int index, int *projection,
                                        float *projargs );

extern int vis5d_get_ctx_vertical( int index, int *vertical,
                                      float *vertargs );

extern int vis5d_get_dtx_vertical( int index, int *vertical,
                                      float *vertargs );

extern int vis5d_get_curved( int index, int *curved );

extern int vis5d_set_dtx_projection_and_vertsys( int index, int what, int type, float towhat);


/* Topograhy, map and texture functions */

extern int vis5d_load_topo_and_map( int index );

extern int vis5d_check_topo( int index, int *topoflag );

extern int vis5d_check_map( int index, int *mapflag );

extern int vis5d_check_texture( int index, int *textureflag );

extern int vis5d_get_topo_range( int index, float *min, float *max );

extern int vis5d_reset_topo_colors( int index );

extern int vis5d_texture_image( int index, int timestep,
                                int width, int height, int components,
                                void *image );

extern int vis5d_set_topo_color_var( int index, int colorvarctx, int colorvar );

extern int vis5d_get_topo_color_var( int index, int *colorvarctx, int *colorvar );


/* Cloning, Ext funcs, and expression functions */

extern int vis5d_make_clone_variable( int index, int var_to_clone,
                                             char *newname, int *newvar );

extern int vis5d_compute_ext_func( int index, char *funcname,
                                          int *newvar );

extern int vis5d_compute_expression( int index, char *expression,
                                            char *newname, int *newvar,
                                            int *recompute, char *mess );

extern int vis5d_make_expr_var( int index, char *expression, char *newname,
                         char *mess, int *newvarowner, int *newvar, int *recompute );

extern int vis5d_make_new_var( int index, char *newname, int nl,
                                       int lowlev, int *newvar );


/* Rendering functions */

extern int vis5d_signal_redraw( int index, int count );

extern int vis5d_check_redraw( int index, int *redraw );

extern int vis5d_draw_frame( int index, int animflag );

extern int vis5d_draw_3d_only( int index, int animflag );

extern int vis5d_draw_2d_only( int index );

extern int vis5d_draw_sounding_only( int index, int pixmapflag);

extern int vis5d_swap_frame( int index );

extern int vis5d_invalidate_grp_frames( int index );

extern int vis5d_invalidate_dtx_frames( int index );

extern int vis5d_set_pointer( int index, int x, int y );

extern int vis5d_set_sounding_graphics_mode( int index, int smode, int onoroff);

extern int vis5d_get_sounding_graphics_mode( int index, int smode, int *onoroff);

extern int vis5d_graphics_mode( int index, int type, int mode );

extern int vis5d_enable_graphics( int index, int type, int number,
                                         int mode );

extern int vis5d_check_ctx_volume( int index, int *volflag );

extern int vis5d_check_dtx_volume( int index, int *volflag );

extern int vis5d_set_volume( int index, int currentvolumeowner, int current_volume );

extern int vis5d_get_volume( int index, int *currentvolumeowner, int *current_voluem );

extern int vis5d_get_graphics_color_address( int index, int type,
                                       int number, unsigned int **color );

extern int vis5d_set_color( int index, int type, int number,
                                   float red, float green, float blue,
                                   float alpha );

extern int vis5d_get_color( int index, int type, int number,
                     float *red, float *green, float *blue, float *alpha );

extern int vis5d_get_color_table_address( int index, int type, int varonwer,
                                      int number, unsigned int **color );

extern int vis5d_set_cursor_color( int index, int traj_set );


extern int vis5d_get_color_table_params( int index, int graphic, int varowner, int var,
                                         float **params );

extern int vis5d_load_color_table( int index, int graphic, int varowner, int var,
                            int table_size, char *filename);

extern int vis5d_set_color_table_params( int index, int graphic, int vowner, int var,
                                         float params[] );


extern int vis5d_color_table_init_params( float params[],
                                          int rgb_flag, int alpha_flag );


extern int vis5d_color_table_recompute( unsigned int table[], int size,
                                        float params[],
                                        int rgb_flag, int alpha_flag );


extern int vis5d_color_table_set_alpha( float params[], float alpha );



extern int vis5d_alpha_mode( int index, int mode );

extern int vis5d_font(  int index, char *fontname, int size );

extern int vis5d_soundfont( int index, char *fontname );

extern vis5d_get_font( int index, char *fontname, int *size);

/* MJK 2.22.99 */
int vis5d_resize_contour_font( int index, int factorx, int factory);


/* WLH 8 Oct 98 */
extern vis5d_get_font_height( int index, int *height);

extern int vis5d_linewidth( int index, float width );

extern int vis5d_get_linewidth( int index, float *width );

extern int vis5d_gl_setup( int index, long win_id,
                                  int width, int height );



/* 3-D view functions */

extern int vis5d_set_matrix( int index, float ctm[4][4] );

extern int vis5d_get_matrix( int index, float ctm[4][4] );

extern int vis5d_set_ortho_view( int index, int view );

extern int vis5d_set_view( int index,
                                  float xrot, float yrot, float zrot,
                                  float scale,
                                  float xtrans, float ytrans, float ztrans );

extern int vis5d_get_view( int index,
                                 float *xrot, float *yrot, float *zrot,
                                 float *scale,
                                 float *xtrans, float *ytrans, float *ztrans );

extern int vis5d_set_camera( int index, int perspec, float front, float zoom );

extern int vis5d_get_camera( int index, int *perspec, float *front,
                            float *zoom );


extern int vis5d_get_box_bounds( int index, float *xmin, float *xmax,
                          float *ymin, float *ymax, float *zmin, float *zmax );



/* Isosurface, slice and trajectory functions */

extern int vis5d_make_isosurface( int index, int time, int var,
                                         int urgent );

extern int vis5d_set_isosurface( int index, int var, float isolevel );

extern int vis5d_get_isosurface( int index, int var, float *isolevel );

extern int vis5d_set_isosurface_color_var( int index, int iso_var,
                                           int cvowner, int colorvar );

extern int vis5d_get_isosurface_color_var( int index, int iso_var,
                                           int *cvowner, int *colorvar );


extern int vis5d_make_hslice( int index, int time, int var,
                                     int urgent );

extern int vis5d_ok_to_go_forward(int dindex);

extern int vis5d_set_hslice( int index, int var, float interval,
                                    float low, float high, float level );

extern int vis5d_get_hslice( int index, int var, float *interval,
                                    float *low, float *high, float *level );

extern int vis5d_make_vslice( int index, int time, int var,
                                     int urgent );

extern int vis5d_set_vslice( int index, int var,
                                    float interval, float low,
                                    float high, float row0, float col0,
                                    float row1, float col1 );

extern int vis5d_get_vslice( int index, int var,
                                    float *interval, float *low,
                                    float *high, float *row0, float *col0,
                                    float *row1, float *col1 );

extern int vis5d_make_chslice( int index, int time, int var,
                                      int urgent );

extern int vis5d_set_chslice( int index, int var, float level );

extern int vis5d_get_chslice( int index, int var, float *level );

extern int vis5d_make_cvslice( int index, int time, int var,
                                      int urgent );

extern int vis5d_set_cvslice( int index, int var,
                                     float row0, float col0,
                                     float row1, float col1 );

extern int vis5d_get_cvslice( int index, int var,
                                     float *row0, float *col0,
                                     float *row1, float *col1 );

extern int vis5d_make_hwindslice( int index, int time, int slice,
                                         int urgent );

extern int vis5d_set_hwindslice( int index, int slice, float density,
                                        float scale, float level );

extern int vis5d_get_hwindslice( int index, int slice, float *density,
                                        float *scale, float *level );

extern int vis5d_make_vwindslice( int index, int time, int slicee,
                                         int urgent );

extern int vis5d_set_vwindslice( int index, int slice, float density,
                                        float scale, float row0, float col0,
                                        float row1, float col1 );

extern int vis5d_get_vwindslice( int index, int slice, float *density,
                                        float *scale, float *row0, float *col0,
                                        float *row1, float *col1 );

extern int vis5d_make_hstreamslice( int index, int time, int slice,
                                         int urgent );

extern int vis5d_set_hstreamslice( int index, int slice, float density,
                                        float level );

extern int vis5d_get_hstreamslice( int index, int slice, float *density,
                                        float *level );

extern int vis5d_make_vstreamslice( int index, int time, int slice,
                                         int urgent );

extern int vis5d_set_vstreamslice( int index, int slice, float density,
                                   float row0, float col0,
                                   float row1, float col1 );

extern int vis5d_get_vstreamslice( int index, int slice, float *density,
                                   float *row0, float *col0,
                                   float *row1, float *col1 );

extern int vis5d_print_traj( int index, int traj_num, float lat[],
                      float lon[], float hgt[], float traj_value[]);

extern int vis5d_make_traj( int index, float row, float col, float lev,
                                   int time, int set );

extern int vis5d_set_traj( int index, float step, float length,
                                  int ribbon_flag );

extern int vis5d_get_traj( int index, float *step, float *length,
                                  int *ribbon_flag );

extern int vis5d_set_trajectory_color_var( int index, int traj_var,
                                           int cvowner, int colorvar );

extern int vis5d_get_trajectory_color_var( int index, int traj_var,
                                           int *cvowner, int *colorvar );

extern int vis5d_delete_last_traj( int index );

extern int vis5d_delete_traj_set( int index, int set );

extern int vis5d_get_num_traj( int index );

extern int vis5d_get_traj_info( int index, int trajnum,
                                float *row, float *column, float *level,
                                int *timestep, float *step, float *length,
                                int *group, int *ribbon );


extern int vis5d_make_timestep_graphics( int index, int time );

extern int vis5d_free_graphics( int index );



/* Text label functions */

extern int vis5d_make_label( int index, int x, int y, char *text );

extern int vis5d_new_label( int index, int x, int y, int *label_id );

extern int vis5d_edit_label( int index, char chr );

extern int vis5d_find_label( int index, int *x, int *y, int *label_id );

extern int vis5d_move_label( int index, int label_id, int x, int y );

extern int vis5d_delete_label( int index, int label_id );

extern int vis5d_get_label( int index, int n,
                                   int *x, int *y, char *label );



/* 3-D cursor functions */

extern int vis5d_set_cursor( int index, float x, float y, float z );

extern int vis5d_get_cursor( int index, float *x, float *y, float *z );

extern int vis5d_set_logo_size( int index, float size );


/*** Color Legend Function */

extern int vis5d_set_legends( int index, int position, int size, int marginx, int marginy );

extern int vis5d_get_legends( int index, int *position, int *size, int *marginx, int *marginy );

/*  Sounding  Functions */

extern int vis5d_get_sounding_window( int index, Window *window, int *width, int *height);

extern int vis5d_set_sounding_window( int index, Window window, int width, int height );


/* 3-D viewing window functions */

extern int vis5d_get_window( int index, Window *window,
                                    int *width, int *height );

extern int vis5d_resize_BIG_window(  int width, int height );

extern int vis5d_resize_3d_window( int index, int width, int height );

extern int vis5d_moveresize_BIG_window( int x, int y,
                                int width, int height );

extern int vis5d_moveresize_3d_window( int index, int x, int y,
                                int width, int height );

extern int vis5d_resize_sounding_window( int index, int width, int height, 
                                         int x, int y );

extern int vis5d_save_window(  char *filename, int format );

extern int vis5d_save_snd_window( int index, char *filename, int format );

extern int vis5d_save_to_v5dfile( int index, char *filename);

extern int vis5d_print_window( void );

extern int vis5d_print_snd_window( int index );


/* Functions useful for controlling Vis5D via -pipe */

/* WLH 15 Oct 98 */
extern int vis5d_locate_dtx(Window w, int x, int y, int *display_index);

extern int vis5d_name_ctx(char *name, int *context_index);


/* Coordinate conversion functions */

extern int vis5d_project( int index, float p[3],
                                 float *winx, float *winy );

extern int vis5d_unproject( int index, float winx, float winy,
                                   float point[3], float dir[3] );

extern int vis5d_grid_level_to_height( int index, int level, float *height);

extern int vis5d_xyzPRIME_to_gridPRIME( int index, int time, int var,
                                    float x, float y, float z,
                                    float *row, float *column, float *level );
extern int vis5d_geo_to_xyzPRIME( int index, int time, int var,
                                    float lat, float lon, float hgt,
                                    float *x, float *y, float *z);

extern int vis5d_xyz_to_grid( int index, int time, int var,
                                    float x, float y, float z,
                                    float *row, float *column, float *level );

extern int vis5d_gridPRIME_to_xyzPRIME( int index, int time, int var,
                                     float row, float column, float level,
                                     float *x, float *y, float *z );

extern int vis5d_grid_to_xyz( int index, int time, int var,
                                     float row, float column, float level,
                                     float *x, float *y, float *z );

extern int vis5d_gridPRIME_to_grid( int index, int time, int var,
                             float rPRIME, float cPRIME, float lPRIME,
                             float *r, float *c, float *l);

extern int vis5d_grid_to_gridPRIME( int index, int time, int var,
                             float r, float c, float l,
                             float *rPRIME, float *cPRIME, float *lPRIME);

extern int vis5d_xyz_to_geo( int index, int time, int var,
                                    float x, float y, float z,
                                    float *lat, float *lon, float *hgt );


extern int vis5d_xyzPRIME_to_geo( int index, int time, int var,
                                    float x, float y, float z,
                                    float *lat, float *lon, float *hgt );

extern int vis5d_rowcol_to_latlon( int index, int time, int var,
                                   float row, float col,
                                   float *lat, float *lon);

extern int vis5d_rowcolPRIME_to_latlon( int index, int time, int var,
                                   float row, float col,
                                   float *lat, float *lon);


extern int vis5d_grid_to_geo ( int index, int time, int var,
                               float row, float col, float lev,
                               float *lat, float *lon, float *hgt);


extern int vis5d_gridPRIME_to_geo( int index, int time, int var,
                                   float row, float col, float lev,
                                   float *lat, float *lon, float *hgt);

extern int vis5d_latlon_to_rowcol( int index, int time, int var,
                                   float lat, float lon,
                                   float *row, float *col);

extern int vis5d_latlon_to_rowcolPRIME( int index, int time, int var,
                                   float lat, float lon,
                                   float *row, float *col);

extern int vis5d_geo_to_grid( int index, int time, int var, 
                              float lat, float lon, float hgt,
                              float *row, float *col, float *lev);

extern int vis5d_geo_to_gridPRIME( int index, int time, int var,
                              float lat, float lon, float hgt,
                              float *row, float *col, float *lev);

extern int vis5d_gridlevel_to_height( int index, int time, int var,
                             float lev, float *hgt);

extern int vis5d_gridlevelPRIME_to_height( int index, int time, int var,
                             float lev, float *hgt);

extern int vis5d_height_to_gridlevel( int index, int time, int var,
                             float hgt, float *lev);

extern int vis5d_height_to_gridlevelPRIME( int index, int time, int var,
                             float hgt, float *lev);


extern int vis5d_geo_to_xyz( int index, int time, int var,
                                    float lat, float lon, float hgt,
                                    float *x, float *y, float *z );


/* Save and restore functions */

extern int vis5d_restore( int index, char *filename );



/*
 * Miscellaneous functions
 */

extern int vis5d_do_work( void );

extern int vis5d_check_work( int *pending_flag );

extern int vis5d_finish_work( void );


/* WLH 6 Oct 98 */
extern int vis5d_noexit(int noex);


/**** 5.0 stuff ******/
extern int vis5d_reset_display_timer( int index );

extern int vis5d_get_display_timer( int index, long *start_sec, long *start_usec);

extern int vis5d_get_context_name( int index, char *name);

extern int vis5d_add_ctx_to_dtx( int index, int index_of_ctx);

extern int vis5d_set_grp_var_values( int index );

extern int vis5d_set_display_group( int index, int index_of_grp );

extern int vis5d_get_display_group( int index, int *gindex );

extern int vis5d_toggle_display_group( int index, int index_of_grp);

extern int vis5d_set_display_border_color( int index, unsigned long cnumber);

extern int vis5d_remove_ctx_index_from_dtx( int index, int index_of_ctx);

extern int vis5d_assign_display_to_data( int index, int display_index);

extern int vis5d_assign_display_to_context( int index, int display_index);

extern int vis5d_load_v5dfile( int dindex, int mbs, char *filename, char *ctxname );

/* WLH 23 Oct 98 */
extern int vis5d_initialize_stuff( int index );

/* WLH 12 Nov 98 */
extern int vis5d_invalidate_isosurface(int index, int var, int time);
extern int vis5d_invalidate_hslice(int index, int var, int time);
extern int vis5d_invalidate_vslice(int index, int var, int time);
extern int vis5d_invalidate_chslice(int index, int var, int time);
extern int vis5d_invalidate_cvslice(int index, int var, int time);

extern int vis5d_invalidate_hwind( int index, int var, int time);
extern int vis5d_invalidate_vwind( int index, int var, int time);
extern int vis5d_invalidate_hstream( int index, int var, int time);
extern int vis5d_invalidate_vstream( int index, int var, int time);

extern int vis5d_create_group_links( int gindex );

extern int vis5d_get_group_graphic_link (int index, int type, int num,
                          int **p_next_index, int **p_next_type, int **p_next_num);

extern int vis5d_link_group_graphics (int index1, int type1, int number1,
                       int index2, int type2, int number2);

extern int vis5d_unlink_group_graphics (int index, int type, int number);

extern int vis5d_get_slice_link (int index, int type, int num,
                          int **p_next_index, int **p_next_type, int **p_next_num);

extern int vis5d_link_slices (int index1, int type1, int number1,
                       int index2, int type2, int number2);

extern int vis5d_unlink_slice (int index, int type, int number);

/* MJK 12.02.98 */
extern int vis5d_set_user_data_flag (int index, int user_data);
extern int vis5d_set_user_flags (int index, int user_topo, int user_maps);
extern int vis5d_set_probe_vars (int index, int numvars, int *varlist);

/* MJK 12.02.98 */
extern int vis5d_set_topo_base (int index, int state, float z_val);
extern int vis5d_set_busy_cursor( Display *dpy, Window win, int busy);
extern int vis5d_get_flatmap_level (int index, float *level);
extern int vis5d_set_flatmap_level (int index, float level);
extern int vis5d_set_view_scales (int index,
                               float scalex, float scaley, float scalez);
extern int vis5d_get_view_scales (int index,
                               float *scalex, float *scaley, float *scalez);


extern int vis5d_enable_sfc_map( int index, int mode );

/* MJK 12.04.98 */
extern int vis5d_enable_sfc_graphics (int index, int type, int number,
                                        int mode);
extern vis5d_init_cloned_var_colortables( int index, int varowner, int var );

/* MJK 12.10.98 */
extern int vis5d_set_view_scales (int index, float scalex, float scaley,
                                   float scalez);

extern int vis5d_get_vert_exaggeration (int index, float *scalez);

extern int vis5d_set_vert_exaggeration (int index, float scalez);

/**************************************************/
/*                  New 5.2 stuff                 */
/*  |       |       |          |       |     |    */
/* \|/     \|/     \|/        \|/     \|/   \|/   */
/**************************************************/

#define REGULAR_TYPE      0
#define IRREGULAR_TYPE    1
#define VIS5D_MAX_ITX_CONTEXTS 20

extern int vis5d_init_irregular_data_end( int index );

extern int vis5d_get_num_of_itxs_in_display( int index, int *number, int numarray[]);

extern int vis5d_get_num_of_ctxs_in_display( int index, int *number, int numarray[]);

extern int vis5d_get_num_of_data_sets_in_display( int index, int *number);

extern int vis5d_init_irregular_memory( int index, int mbs );

extern int vis5d_open_recordfile( int index, char *name, char *itxname, int read_flag );

extern int vis5d_get_itx_numtimes( int index, int *numtimes );

extern int vis5d_get_itx_time_stamp( int index, int timestep,
                                        int *day, int *time );
extern int vis5d_assign_display_to_irregular_data( int index, int display_index);

extern int vis5d_load_irregular_v5dfile( int dindex, int mbs, char *filename, char *ctxname );

extern int vis5d_set_text_plot( int index, int var, float spacing,
                                float fontx, float fonty, float fontspace);

extern int vis5d_get_text_plot( int index, int *var, float *spacing,
                                float *fontx, float *fonty, float *fontspace);

extern int vis5d_make_text_plot( int index, int time, int urgent);

extern int vis5d_invalidate_text_plot( int index, int time);

extern int vis5d_get_textplot_color_status( int index, int var, int *status);

extern int vis5d_set_textplot_color_status( int index, int var, int status);

extern int vis5d_enable_irregular_graphics( int index, int type, int mode );

extern int vis5d_destroy_irregular_data_context( int index );

#endif  /*API_H*/
