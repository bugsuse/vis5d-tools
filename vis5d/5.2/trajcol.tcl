# trajcol.tcl

# Generate a column of trajectories at the cursor's lat/lon
# for the current timestep.


# Which trajectory set should the trajectories be put in:
set trajset 0



# This is a bit tricky:  we need to know how many grid levels are
# present for trajectory tracing.  We query the variables used for
# trajectory tracing and then call vis5d_get_ctx_grid_levels to find out
# how many grid levels are present for the U rajectory component.
# Note: element 6 of the wind_vars list is the traj U variable.
set wind_vars [ vis5d_get_wind_vars $ctx ]
set traj_uvar [ lindex $wind_vars 6 ]
set traj_levs [ vis5d_get_ctx_grid_levels $ctx $traj_uvar ]


# Get current timestep
set curtime [ vis5d_get_dtx_timestep $ctx ]


# Get the current cursor position as (row,col).
set cursor_xyz [ vis5d_get_cursor $ctx ]
set cursor_rcl [ vis5d_xyz_to_grid $ctx $curtime $traj_uvar $cursor_xyz ]
set row [ lindex $cursor_rcl 0 ]
set col [ lindex $cursor_rcl 1 ]


# Loop over grid levels making a trajectory for each level.
for {set lev 0} {$lev < $traj_levs} {set lev [expr $lev+1]} {
	vis5d_make_traj $ctx $row $col $lev $curtime $trajset   
}
