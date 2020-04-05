# label.tcl

# A Vis5D Tcl script for testing labels
# will crash at end of loop if there are an odd number of time steps

set base "frame"
set ext ".gif"
set format VIS5D_GIF


set numtimes [ vis5d_get_numtimes $ctx ]

for {set time 1} {$time<=$numtimes} {set time [expr $time+2]} {
	vis5d_make_label $ctx 400 400 "hey hey hey"
	vis5d_set_timestep $ctx [expr $time-1]
	vis5d_draw_frame $ctx
	set name $base$time$ext
	vis5d_save_window $ctx $name $format
	vis5d_delete_label $ctx 400 400
	vis5d_set_timestep $ctx [expr $time]
	vis5d_draw_frame $ctx
	vis5d_save_window $ctx $name $format
}

