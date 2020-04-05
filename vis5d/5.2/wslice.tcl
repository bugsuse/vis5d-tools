# wslice.tcl

# Display a horizontal contour line slice of W (vertical wind) at the
# middle altitude using dashed lines for negative values.  Draw the
# slice in yellow.


# You can change these:
set wvar "W"
set interval -0.05

# The color specified in RGBA:
set red 1.0
set green 1.0
set blue 0.0
set alpha 1.0


# Get min and max W values
set minmax [ vis5d_get_var_range $ctx $wvar ]
set low_val  [ lindex $minmax 0 ]
set high_val [ lindex $minmax 1 ]

# Compute location of middle grid level
set num_levels [ vis5d_get_ctx_grid_levels $ctx $wvar ]
set level [ expr $num_levels / 2.0 ]


# set the color
vis5d_set_color $ctx VIS5D_HSLICE $ctx $wvar $red $green $blue $alpha

# setup slice parameters
vis5d_set_hslice $ctx $wvar $interval $low_val $high_val $level

# compute the slice
vis5d_make_hslice $ctx VIS5D_ALL_TIMES $wvar 0

# display it!
vis5d_enable_graphics $ctx VIS5D_HSLICE $wvar VIS5D_ON
