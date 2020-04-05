# movie2.tcl

# A Vis5d Tcl script for saving a whole time series of images
# with multiple data sets.

# You may need to change these:
set base "frame"
set ext ".gif"
set format VIS5D_GIF
set ctx0 0
set ctx1 1
set dtx0 0
set dtx1 1
set grp 1

vis5d_set_display_group 0 1
vis5d_set_display_group 1 1
set numtimes [ vis5d_get_grp_numtimes $grp ]

for {set time 1} {$time<=$numtimes} {set time [expr $time+1]} {
	vis5d_set_grp_timestep $grp [expr $time-1]
	vis5d_draw_frame $dtx0
        vis5d_draw_frame $dtx1
	set name $base$time$ext
	vis5d_save_window $name $format
}


# At this point you may want to invoke a Unix utility to convert a
# series of GIF files into an MPEG animation file.....
