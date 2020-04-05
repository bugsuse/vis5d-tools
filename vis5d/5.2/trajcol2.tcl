# trajcol.tcl

# Generate a column of trajectories at the cursor's lat/lon
# for the current timestep for both data sets which are in the
# same display.


# Which trajectory set should the trajectories be put in:
set trajset 0

set dtx 0
set ctx0 0
set ctx1 1

#set the wind vars for the first data set ctx0, assuming there number
#0 1 and 2 for U V and W. 
vis5d_set_wind_vars $dtx -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 0 0 0 1 0 2
 
# use the levels of the display grid (not data grid), so it's not
# dependent on the variable.
set traj_levs [ vis5d_get_dtx_grid_levels $dtx ]


# Get current timestep
set curtime [ vis5d_get_dtx_timestep $dtx ]


# Get the current cursor position in display grid coordinates 
# (not data grid coordinates) as (row,col).
set cursor_xyz [ vis5d_get_cursor $dtx ]
set cursor_rcl [ vis5d_xyzPRIME_to_gridPRIME $dtx $curtime 0 $cursor_xyz ]
set row [ lindex $cursor_rcl 0 ]
set col [ lindex $cursor_rcl 1 ]


# Loop over grid levels making a trajectory for each level.
for {set lev 0} {$lev < $traj_levs} {set lev [expr $lev+1]} {
	vis5d_make_traj $dtx $row $col $lev $curtime $trajset   
	puts "calculating trajset0 for ctx0 at level $lev"
}

#now set the wind vars for the second data set and repeat the above logic.
vis5d_set_wind_vars $dtx -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 1 0 1 1 1 2

set trajset 1
for {set lev 0} {$lev < $traj_levs} {set lev [expr $lev+1]} {
        vis5d_make_traj $dtx $row $col $lev $curtime $trajset
	puts "calculating trajset1 for ctx1 at level $lev"
}       

