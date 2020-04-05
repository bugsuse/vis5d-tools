!  program to create vis5d file from wrf output (or restart/init) data.
!  current version reads only wrf-netcdf file format

!  WCS, October 30 2000

!  time, filename, variable data


  program wrf_to_v5d

  USE module_wrf_to_v5d_util

  implicit none
  integer, parameter :: max_times = 100, max_variables = 100
  character (len=80)    :: data_files(max_times),     &
                            file_for_time(max_times), &
                            times(max_times),         &
                            input_file, vis5d_file

  integer :: file_time_index(max_times)

  character (len=20)    :: variables(max_variables-30),variables2d(30)

  logical :: debug, all_times

  integer, external:: iargc
  integer :: input_unit, output_times, number_of_variables, number_of_2dvar, &
             input_file_unit, number_of_files
  integer :: length_input, length_vis5d, num_v5d_levels, i, ii
  logical :: output_input_grid, use_lowest_heights
  real, allocatable, dimension(:) :: v5d_z_levels

  debug = .true. 
! debug = .false. 
  input_file_unit = 10

  if (iargc() /= 2) then
    write(6,*) ' usage: wrf_to_v5d input_file vis5d_file '
    stop
  else
    call getarg(1,input_file)
    length_input = len_trim(input_file)
    if(debug) write(6,*) ' input file is ',input_file(1:length_input)
    call getarg(2,vis5d_file)
    length_vis5d = len_trim(vis5d_file)
    if(debug) write(6,*) ' vis5d file is ',vis5d_file(1:length_vis5d)
  endif
  
  open( unit=input_file_unit,            &
        file=input_file(1:length_input), &
        form='formatted',                &
        status='old'                    )

!  here we get the information concerning the data files to use, the times
!  to put in the vis5d file, and the variables from the input file

  if(debug) write(6,*) ' calling read_time_list '
  call read_time_list     ( input_file_unit, times, max_times, output_times, &
                            all_times,                                    debug )
  if(debug) write(6,*) ' calling read_variable_list '
  call read_variable_list( input_file_unit, variables, max_variables,        &
                            number_of_variables,                             &
                            variables2d, number_of_2dvar,                 debug )
  if(debug) write(6,*) ' calling read_file_list '
  call read_file_list     ( input_file_unit, data_files, max_times,          &
                            number_of_files,                              debug )

  read(input_file_unit, * ) num_v5d_levels

  if(debug) write(6,*) ' num_v5d_levels in input file ',num_v5d_levels

  if( num_v5d_levels == 0) then    !  output  cartesian grid
      write(6,*) ' generic cartesian output grid '
      use_lowest_heights = .false.
      output_input_grid = .true.
      num_v5d_levels = 1
      allocate(v5d_z_levels(1))
  else if ( num_v5d_levels < 0) then                
      write(6,*) ' interp to z profile at lowest terrain column '
      use_lowest_heights = .true.          ! use z distribution from 
      output_input_grid = .true.           ! lowest point of terrain
      num_v5d_levels = 1
      allocate(v5d_z_levels(1))
  else
      write(6,*) ' reading in user supplied heights '
      allocate(v5d_z_levels(num_v5d_levels))  !  use user-input z distribution
      output_input_grid = .false.
      use_lowest_heights = .false. 
      do i=1, num_v5d_levels
        read(input_file_unit,*) ii,v5d_z_levels(i)
        if(debug) write(6,*) ' i, v5d_z_level(i) ',i,v5d_z_levels(i)
        write(6,*) ' i, v5d_z_level(i) ',i,v5d_z_levels(i)
      enddo
  endif

  if(debug) write(6,*) ' closing input file '
  close( unit=input_file_unit, status = 'keep')

! next, check each input file for times and produce list of
! times and files for the data

  if(debug) write(6,*) ' calling create_time_file_list '

  call create_time_file_list( data_files,         &
                              file_for_time,      &
                              file_time_index,    &
                              times,              &
                              max_times,          &
                              all_times,          &
                              output_times,       &
                              number_of_files,    &
                              debug              )


  if(debug) write(6,*) ' exited create_time_file_list '

  call create_vis5d ( vis5d_file, file_for_time, file_time_index, times,   &
                      output_times, variables, number_of_variables,        &
                      variables2d, number_of_2dvar,                        &
                      output_input_grid,  &
                      use_lowest_heights, &
                      v5d_z_levels,       &
                      num_v5d_levels,     &
                      debug )

  if(debug) write(6,*) ' exited create_vis5d '
  deallocate(v5d_z_levels)

  end program wrf_to_v5d

