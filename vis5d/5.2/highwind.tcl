# highwind.tcl

# Display a horizontal wind vector slice at the top of the domain
# with density = 0.5 and scale = 2.0, drawn in purple.


# You can change these:
set slice_num 0
set density 0.5
set scale 2.0

# The color specified in RGBA:
set red 1.0
set green 0.0
set blue 1.0
set alpha 1.0



# This is a bit tricky:  we need to know how many grid levels are
# present for the wind slice.  We query the variables used for
# hwind slice generation and then call vis5d_get_grid_levels to find out
# how many grid levels are present for the U component.
# Note: element 0 of the wind_vars list is the U variable for slice 0.
set wind_vars [ vis5d_get_wind_vars $ctx ]
set uvar [ lindex $wind_vars 0 ]
set num_levels [ vis5d_get_grid_levels $ctx $uvar ]

set level [ expr $num_levels - 1 ]


# set the color
vis5d_set_color $ctx VIS5D_HWIND $slice_num $red $green $blue $alpha

# setup slice parameters
vis5d_set_hwindslice $ctx $slice_num $density $scale $level

# compute the slice
vis5d_make_hwindslice $ctx VIS5D_ALL_TIMES $slice_num 0

# display it!
vis5d_enable_graphics $ctx VIS5D_HWIND $slice_num VIS5D_ON
