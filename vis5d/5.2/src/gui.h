/* VIS-5D version 5.2 */

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


#ifndef GUI_H
#define GUI_H


#include <stdio.h>
#include "../lui5/lui.h"
#include "matrix.h"
#include "vis5d.h"
#include "v5d.h"
#include "api.h"
#include "globals.h"

#define MAX_TITLES 100
#define CP_WIDTH  384

struct gui_context {
  int context_index;           /* index of this context */
  int GoTime;          /* animate flag (0=off, -1=backward, 1=forward) */
  int AnimRate;        /* minimum time between animation steps in msec */
   /* from gui.c */
  Window  mainwin;          /* the main 3-D viewing window */

#ifdef DENALI
  Window NPGLwin;        /* Window ID of NPGL window */
#endif
  Window  CpWindow;      /* the control panel window */
  Window  fakewin;       /* fake 3-D window */
  int fakewidth, fakeheight;  /* size of fake 3-D window */
  int MouseMode;
/***************************************/
/* new vars for all buttons in gui 5.0 */
/***************************************/
   LUI_BUTTON_PAD *lillypadBUTTON;
   LUI_LABEL *copyrightBUTTON;
   LUI_NEWBUTTON  *animateBUTTON;
   LUI_NEWBUTTON *stepBUTTON;
   LUI_NEWBUTTON *exitBUTTON;
   LUI_NEWBUTTON *newvarBUTTON;
   LUI_NEWBUTTON *textureBUTTON;
   LUI_NEWBUTTON *topBUTTON;
   LUI_NEWBUTTON *southBUTTON;
   LUI_NEWBUTTON *westBUTTON;
   LUI_NEWBUTTON *topoBUTTON;
   LUI_NEWBUTTON *boxBUTTON;
   LUI_NEWBUTTON *clockBUTTON;
   LUI_NEWBUTTON *saveBUTTON;
   LUI_NEWBUTTON *restoreBUTTON;
   LUI_NEWBUTTON *gridBUTTON;
   LUI_NEWBUTTON *contBUTTON;
   LUI_NEWBUTTON *animrecBUTTON;
   LUI_NEWBUTTON *reverseBUTTON;
   LUI_NEWBUTTON *savepicBUTTON;
   LUI_NEWBUTTON *scriptBUTTON;
   LUI_NEWBUTTON *interpBUTTON;
   LUI_NEWBUTTON *uvwvarsBUTTON;
   LUI_NEWBUTTON *legendsBUTTON;
   LUI_NEWBUTTON *importBUTTON;
   LUI_NEWBUTTON *iimportBUTTON;
   LUI_NEWBUTTON *displayBUTTON;
   LUI_NEWBUTTON *blank1BUTTON;
   LUI_NEWBUTTON *blank2BUTTON;
   LUI_NEWBUTTON *perspecBUTTON;
   LUI_BUTTON_PAD *radiopadBUTTON;
   LUI_BUTTON *normalBUTTON;
   LUI_BUTTON *trajectoryBUTTON;
   LUI_BUTTON *sliceBUTTON;
   LUI_BUTTON *labelBUTTON;
   LUI_BUTTON *probeBUTTON;
   LUI_BUTTON *soundingBUTTON;
   LUI_BUTTON *clippingBUTTON;

   LUI_NEWBUTTON *hwind1BUTTON;
   LUI_NEWBUTTON *vwind1BUTTON;
   LUI_NEWBUTTON *hstreamBUTTON;
   LUI_NEWBUTTON *hwind2BUTTON;
   LUI_NEWBUTTON *vwind2BUTTON;
   LUI_NEWBUTTON *vstreamBUTTON;

   LUI_NEWLABEL *colheading;



/*****************************************************************/
/* New 5.0 stuff that may or may not be relavent later           */
/*****************************************************************/

   int current_ctx;                        /* this is the current vis5d context, used */
                                           /* for example when a variable is clicked on */
                                           /* int the isosurface column, then the newsurf_cb */
                                           /* function will know what vis5d_context index is */
                                           /* being dealt with.                              */

   int current_itx;

   int how_many_regular_contexts;          /* this tells how many contexts there are */
                                           /* for this gui context                   */
   int how_many_irregular_contexts;

   int array_of_ctxs[VIS5D_MAX_CONTEXTS];  /* this gives the vis5d_context number     */
   int array_of_itxs[VIS5D_MAX_CONTEXTS]; 

   int total_ctx_numtimes;  
   int total_itx_numtimes; 
   int total_dtx_numtimes;

   int current_text_plot_iindex;
   int current_text_plot_var;
   float current_text_plot_spacing;
   float current_text_plot_fontx;
   float current_text_plot_fonty;
   float current_text_plot_fontspace;

   int total_numvars;
   int RegularNumVars[VIS5D_MAX_CONTEXTS];

   int NumLevels[VIS5D_MAX_CONTEXTS][MAXVARS];

   char RegularVarName[MAXVARS][VIS5D_MAX_CONTEXTS][10];
 
   int IrregularNumVars[VIS5D_MAX_CONTEXTS];
   char IrregularVarName[MAXVARS][VIS5D_MAX_CONTEXTS][MAX_VAR_LENGTH];
   char FileName[VIS5D_MAX_CONTEXTS][20];

   LUI_FIELD *TextPlotSpacingField;
   LUI_FIELD *TextPlotFontX;
   LUI_FIELD *TextPlotFontY;
   LUI_FIELD *TextPlotFontSpace;


   LUI_BUTTON_MATRIX *TextPlotButtonMatrix;
   Window TextPlotWindow;
 

   int u1owner, v1owner, w1owner,
       u2owner, v2owner, w2owner,
       tuowner, tvowner, twowner;        /* vis5d ctx indexes for the owners of the vars */

   int cb_ctx_index;                     /* when calling colorbar_cb, need this to know
                                            which vis5d_ctx index where dealing with */

   int group_index;   /* 0 if belongs to no group */

   int borderR;
   int borderG;
   int borderB;


   


/******************************************* end of new 5.0 stuff */
 
   



  /* Miscellaneous windows */
  Window CloneWindow;
  int CloneWidth, CloneHeight;
  int CpHeight, cpx, cpy;
  int Columns;   /* 5 or 6 button columns, button width */
  int RGBWidth, RGBHeight;

  /* Main window */
  LUI_NEWBUTTON *hwind_button[VIS5D_WIND_SLICES];
  LUI_NEWBUTTON *vwind_button[VIS5D_WIND_SLICES];
  LUI_NEWBUTTON *hstream_button[VIS5D_WIND_SLICES];
  LUI_NEWBUTTON *vstream_button[VIS5D_WIND_SLICES];

  LUI_NEWBUTTON *map_button;
  LUI_NEWBUTTON *perspec_button;

  LUI_BUTTON_MATRIX *ButtonMatrix;
  LUI_BUTTON_MATRIX *IrregularButtonMatrix;

  int ButtonMatrixTop;
  int IrregularButtonMatrixTop;
  int ButtonMatrixWidth, ButtonMatrixHeight;
  int IrregularButtonMatrixWidth;
  int IrregularButtonMatrixHeight;
  int IrregularColumns;

  int ColorRow, ColorCol;  /* Which button in the matrix to recolor */
  LUI_LABEL *ModeInfoLabel;

  /* Isolevel window */
  Window IsoWindow;
  int IsosurfHeight, IsosurfWidth;
  LUI_NEWSLIDER *IsoSlider;

  /* Horizontal slice contour interval window */
  Window HSliceWindow;
  LUI_NEWLABEL *hslice_label;
  LUI_FIELD *hslice_field;

  /* Vertical slice contour interval window */
  Window VSliceWindow;
  LUI_NEWLABEL *vslice_label;
  LUI_FIELD *vslice_field;

  /* H/V Wind Slice scale/density Window */
  Window WindWindow;
  LUI_NEWLABEL *windscale_label, *winddensity_label;
  LUI_FIELD *windscale_field, *winddensity_field;

  /* Type-in Expression window */
  Window ExprWindow;
  LUI_FIELD *expr_field;
  char Expression[500];   /* Current expression: */

  /* Save window: */
  Window SaveWindow;
  LUI_FIELD *SaveNameField;

  /* Save File window: */
  Window SaveFileWindow;
  LUI_FIELD *SaveFileNameField;

  /* Restore window: */
  Window RestoreWindow;
  LUI_FIELD *RestoreNameField;

  /* Save window image: */
  Window SavePicWindow;
  LUI_FIELD *SavePicField;
  LUI_RADIO *SavePicRadio;
  int SaveFormats[6];      /* image file format for each button displayed */

  /* Trajectory window */
  Window TrajWindow;
  LUI_FIELD *TrajStepField, *TrajLenField;
  LUI_BUTTON *TrajButton[VIS5D_TRAJ_SETS];
  LUI_NEWBUTTON *TrajRibbonButton;

  /* Sounding window */
  int othersnddpy;                 /* 1 if '-wdpy' switch is invoked                   */
  int vertsys;                     /* This describes the vertical coordinate system    */
                                   /* If it is 0 then it is not in kilometers          */
  int oceanonly;                   /* this say if the data is ocean data only or not   */
                                   /* if it is 0 then it is not just ocean data        */

  Window SoundCtrlWindow;          /* This is the main control window for the sounding */
  LUI_FIELD *snd_temp, *snd_dewpt,
            *snd_u_wind, *snd_v_wind,
            *snd_var1, *snd_var2,
            *snd_var3;
  LUI_NEWBUTTON *thta_button, *thte_button,
                *w_button, *tick_button,
                *sndapply;
  LUI_NEWLABEL *snd_tempLABEL, *var1LABEL,
               *snd_dewptLABEL, *var2LABEL,
               *snd_u_windLABEL, *var3LABEL,
               *snd_v_windLABEL;

  Window sm1, sm2, sm3;

 
  /* Verify and Alert windows */
  Window OKWindow, VerifyWindow, AlertWindow;
  int VerifyHeight, AlertHeight;
  LUI_NEWLABEL *OKLabel, *VerifyLabel, *AlertLabel;
  int cur_clip;

  /* Group window notifiers */
  Window GroupWindow;
  LUI_NEWLABEL *GroupLabel1,
               *GroupLabel2,
               *GroupLabel3,
               *GroupLabel4;

  /* RGB color sliders window */
  Window rgb_sliders_window;
  LUI_NEWLABEL *rgb_sliders_label;
  LUI_NEWSLIDER *rgb_red_slider;
  LUI_NEWSLIDER *rgb_green_slider;
  LUI_NEWSLIDER *rgb_blue_slider;
  LUI_NEWSLIDER *rgb_alpha_slider;

  /* Isosurface/Trajectory coloring window */
  Window isocolor_window;
  Window isocolor_subwin1;
  Window isocolor_subwin2;
  LUI_BUTTON_MATRIX *iso_button_matrix;
  LUI_NEWLABEL *isocolor_label;
  LUI_NEWSLIDER *iso_red_slider;
  LUI_NEWSLIDER *iso_green_slider;
  LUI_NEWSLIDER *iso_blue_slider;
  LUI_NEWSLIDER *iso_alpha_slider;
  LUI_COLORBAR *iso_colorbar;

  /* text plot color windows */
  Window tp_color_window;
  Window tp_color_subwin1;
  Window tp_color_subwin2;
  LUI_NEWLABEL *tp_color_label;
  LUI_NEWSLIDER *tp_red_slider;
  LUI_NEWSLIDER *tp_green_slider;
  LUI_NEWSLIDER *tp_blue_slider;
  LUI_NEWSLIDER *tp_alpha_slider;
  LUI_COLORBAR *tp_colorbar;
  LUI_NEWBUTTON *tp_monocolor;
  LUI_NEWBUTTON *tp_multicolor;

  /* error widget window */
  Window error_window;

  /* display widget window */
  Window display_window;

  /* UVW widget window */
  Window uvw_window;
  LUI_FIELD *traju_field, *trajv_field, *trajw_field;
  LUI_FIELD *u1_field, *v1_field, *w1_field;
  LUI_FIELD *u2_field, *v2_field, *w2_field;
  int traju_var, trajv_var, trajw_var;
  int u1_var, v1_var, w1_var;
  int u2_var, v2_var, w2_var;

  /* Color slice, volume, topo widget: */
  LUI_COLORBAR *Colorbar;

  /* The 'current' isosurf, slice, etc. is the one whose slider window,
     color widget window, etc. is currently displayed.  This is independent
     of whether a graphic is being displayed because you can, for example,
     change the color of a slice while it's not displayed.  */
  int cur_isosurf;
  int cur_isosurfvindex;
  int cur_isosurfmap; /* 0 = IsoWindow is not mapped 1= IsoWindow is mapped */
  int cur_hslice;
  int cur_hslicevindex;
  int cur_hslicemap;
  int cur_vslice;
  int cur_vslicevindex;
  int cur_vslicemap;
  int cur_trajset;
  int cur_hwind;
  int cur_hwindvindex;
  int cur_hwindmap;
  int cur_vwind;
  int cur_vwindvindex;
  int cur_vwindmap;
  int cur_hstream;
  int cur_hstreamvindex;
  int cur_hstreammap;
  int cur_vstream;
  int cur_vstreamvindex;
  int cur_vstreammap;
  int CurrentVolume;
  int CurrentVolumeOwner;

  int edit_topo;   /* non-zero when editing topo colors */
  int uvw_map;

  /* labels.c */
  int label_state;
  int label_start_x, label_start_y;

  /* from func1 */
  int widget_enable;

  /* from get_user_input */
  int  start_x, start_y, p1, p2, p3;
  MATRIX  ctm;
  float zoom;

  /* misc. graphics stuff */
  int needswap;

  /* external function stuff */
  char funcpath[1000];
  float size_of_logo;



  /* MJK 12.04.98 */
  LUI_NEWSLIDER *hslice_pos_slider;
  Window        CHSliceWindow;
  LUI_COLORBAR *CHSliceColorbar;
  LUI_NEWSLIDER *chslice_pos_slider;
  Window        HWindWindow;
  LUI_NEWLABEL *hwindscale_label, *hwinddensity_label;
  LUI_FIELD      *hwindscale_field, *hwinddensity_field;
  LUI_NEWSLIDER *hwind_pos_slider;
  Window        MapWindow;
  LUI_NEWSLIDER *map_pos_slider;
  LUI_NEWBUTTON *map_sfc_button;
  int AnimDwell;        /* dwell time between 1st and last frames in msec */
  LUI_NEWBUTTON *hslice_sfc_button;
  LUI_NEWBUTTON *hwind_sfc_button;
  LUI_NEWBUTTON *temp_button;
  Window        ColorbarWindow;


  LUI_NEWLABEL *Irregular_Heading;



};


typedef struct gui_context *GuiContext;



extern int cb_chvar[VIS5D_MAX_DPY_CONTEXTS];
extern int cb_chvindex[VIS5D_MAX_DPY_CONTEXTS];
extern int cb_chvvar[VIS5D_MAX_DPY_CONTEXTS];
extern int cb_dindex;

extern Display *GuiDpy;         /* The X display for the GUI */
extern int GuiScr;              /* The X screen number for the GUI */
extern Visual *GuiVisual;       /* The X visual for the GUI */
extern Colormap GuiColormap;    /* The X colormap for the GUI */
extern int GuiDepth;            /* The depth of the GUI visual */
extern Window GuiBigWin;        /* Big Fake Win for seperate dpy */
extern int GuiBigWinWidth;
extern int GuiBigWinHeight;
extern char the_gui_dpy_name[1000];
extern int top_margin;
extern int left_margin;
extern int bottom_margin;
extern int right_margin;
extern int number_of_titles;
extern char title_string[MAX_TITLES][200];
extern int title_x_position[MAX_TITLES];
extern int title_y_position[MAX_TITLES];
extern char title_font[MAX_TITLES][200];
extern int redo_the_gui; 

/* WLH 11 Nov 98 */
extern int redo_this_gui[VIS5D_MAX_DPY_CONTEXTS];

/* MJK 11.17.98 */
extern void make_watch_cursor( void );
extern void unmake_watch_cursor( void );




extern GuiContext get_gui_gtx( int index );

extern GuiContext get_gui_gtx2( int index );

extern GuiContext create_gui_context( int index );

extern void set_anim_rate( int index, int rate);

extern int make_another_gui( int index, int newgui);

extern int map_fake_windows( int onlyrecolor);

extern int make_gui_BigWin( char *wdpy_name);

extern void get_current_display(  int *cur );

extern void set_current_display( int cur );



/* MJK 12.07.98 */
extern int make_gui( int index, char *dataset, char *wdpy_name, char *geom_str,
                     int volflag );



/* MJK 12.15.98 */
extern int make_nodata_gui( int index, char *wdpy_name, char *geom_str);


extern int get_user_input( int block );

extern void update_var( int index, int newvar );

extern void turn_off_everything( int index );

extern void set_display_matrix( int rows, int cols );

extern void get_display_matrix( int *rows, int *cols );

extern void init_colortables( int index  );

extern void init_some_colortables( int dindex, int vindex );

extern void set_slice_alpha( int index, int graphic, int vowner, int var, int alpha );

extern int make_new_buttons_after_interp( int index, int number_of_new_buttons );

extern void show_error_widget( int index, char *message);

extern void unmap_gtx_CpWindow( int index );

extern void create_widgets( int index, int volflag, char *programname );

extern int get_button_ctx_index(int index, int row);

extern void map_all_windows( int onlyrecolor );

extern void unmap_all_windows( void );

extern void set_display_border_color( int index, int R, int G, int B);

extern void get_display_border_color( int index, int *R, int *G, int *B);

extern void mod_vpos_slider (int index, LUI_NEWSLIDER *s, int ivar,
                             int is_wind, float lev);

extern int get_button_gtx_index(int dindex, int vindex, int row);

extern int get_button_ctx_row( int index, int row);

extern int is_valid( int index, int vindex);

extern void hide_widgets( int index );

extern void turn_off_everything( int index );

extern void show_widgets( int index );

/* WLH 21 Oct 98 */
extern void recompute_graphics( int index, int numtrajs, float *lats,
   float *lons, float *alts, int *days, int *times, int *gps);

/* WLH 11 Nov 98 */
extern void recompute_graphics_var( int index, int numtrajs, float *lats,
   float *lons, float *alts, int *days, int *times, int *gps, int cvar);

extern int run_script( int index, char *file );

/* WLH 1 Oct 98 */
extern void iconify( int index );

/* WLH 1 Oct 98 */
extern void deiconify( int index );

/* WLH 13 Oct 98 */
extern void update_button_states( int index, int double_check );

extern int save_traj(int index, float **latsp, float **lonsp, float **altsp,
              int **daysp, int **timesp, int **gpsp);


/* MJK 12.04.98 */
extern int update_vpos_slider (int index, int type, int var, float level);

extern int set_mouse_mode (int index, int mode);

extern int get_animate (int index, int *state, int *rate, int *dwell);

extern int set_animate (int index, int state, int rate, int dwell);

extern int set_reverse_background (int index, int state);

extern int reinit_gui (int index);

extern int set_cursor_type (int index, int shape,
                              int fg_r, int fg_g, int fg_b,
                              int bg_r, int bg_g, int bg_b);


extern int set_cursor_props (int index, int shape,
                               int fg_r, int fg_g, int fg_b,
                               int bg_r, int bg_g, int bg_b);

extern int set_busy_cursor ( int busy);


/* MJK 12.07.98 */
extern int get_window_geometry (char *geom_str, int *width, int *height,
                             int *xpos, int *ypos);
extern int alrighty_value;
extern int alrighty( int index, char *prompt );

#endif
