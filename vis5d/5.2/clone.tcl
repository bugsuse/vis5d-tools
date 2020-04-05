# this clones a variable and generates two isosurfaces

vis5d_make_clone_variable 0 "T'" "T"
vis5d_set_isosurface 0 "T" 283.0
vis5d_make_isosurface 0 VIS5D_ALL_TIMES "T" 0
vis5d_set_isosurface 0 "T'" 293.0
vis5d_make_isosurface 0 VIS5D_ALL_TIMES "T'" 0
vis5d_set_color 0 VIS5D_ISOSURF "T" 0.753 1.000 0.753 1.000
vis5d_set_color 0 VIS5D_ISOSURF "T'" 1.000 1.000 1.000 1.000
vis5d_graphics_mode 0 VIS5D_MAP VIS5D_ON
vis5d_enable_graphics 0 VIS5D_ISOSURF "T" VIS5D_ON
vis5d_enable_graphics 0 VIS5D_ISOSURF "T'" VIS5D_ON
vis5d_set_topo_color_var 0 -1
vis5d_draw_frame 0
