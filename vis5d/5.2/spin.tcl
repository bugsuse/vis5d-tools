# spin.tcl

# Spin and zoom the 3-D box


# spin
for {set angle 0} {$angle <= 360} {set angle [expr $angle + 10]} {
	vis5d_set_view $ctx -60.0 0.0 $angle  1.0  0.0 0.0 0.0
	vis5d_draw_frame $ctx
	vis5d_sleep 100
}

# zoom in
for {set scale 1.0} {$scale <= 1.8} {set scale [expr $scale + 0.1]} {
	vis5d_set_view $ctx -60.0 0.0 0.0  $scale  0.0 0.0 0.0
	vis5d_draw_frame $ctx
	vis5d_sleep 100
}

# zoom out
for {set scale 1.8} {$scale >= 1.0} {set scale [expr $scale - 0.1]} {
	vis5d_set_view $ctx -60.0 0.0 0.0  $scale  0.0 0.0 0.0
	vis5d_draw_frame $ctx
	vis5d_sleep 100
}
