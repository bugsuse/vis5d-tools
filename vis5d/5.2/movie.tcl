# movie.tcl

# A Vis5d Tcl script for saving a whole time series of images

# You may need to change these:
set base "frame"
set ext ".gif"
set format VIS5D_GIF

set numtimes [ vis5d_get_dtx_numtimes $ctx ]

for {set time 1} {$time<=$numtimes} {set time [expr $time+1]} {
	vis5d_set_dtx_timestep $ctx [expr $time-1]
	vis5d_draw_frame $ctx
	set name $base$time$ext
	vis5d_save_window $name $format
}


# At this point you may want to invoke a Unix utility to convert a
# series of GIF files into an MPEG animation file.....
