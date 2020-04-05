MODULE module_wrf_to_v5d_util

  USE module_wrf_to_v5d_netcdf
  USE map_utils

CONTAINS

!---------------------------------------------------------------------------------

  subroutine create_vis5d( vis5d_file, file_for_time, file_time_index, times,     &
                           output_times, variables, number_of_variables,          &
                           variables2d, number_of_2dvar,                          &
                           output_input_grid, use_lowest_heights, v5d_z_levels,   &
                           num_v5d_levels, debug )

  implicit none

!------  some vis5d stuff

  include 'v5df.h'  ! in this include file are IMISSING, MISSING, 
                    ! MAXVARS and MAXLEVELS and MAXTIMES

!------

  integer, intent(in) :: output_times, number_of_variables, num_v5d_levels, number_of_2dvar
  logical, intent(in) :: debug, output_input_grid, use_lowest_heights  
  character (len=80), intent(in), dimension(output_times) :: file_for_time, times
  character (len=20), intent(in), dimension(number_of_variables) :: variables, variables2d
  character (len=80), intent(in) :: vis5d_file
  real, dimension( num_v5d_levels ), intent(in) :: v5d_z_levels

  integer, intent(in), dimension(output_times) :: file_time_index

  real, allocatable, dimension(:,:,:)   :: z, data_in, &
                                           data_out, data_out_z, z_v5d_order, &
                                           ph, phb, p, pb
  integer, dimension(output_times) :: timestamp, datestamp
  integer :: it, ndims, dims(4), k, j, i, rcode

  real :: dx, dy, xx, yy
  real :: proj_args(100)
  real :: vert_args(MAXLEVELS)
  integer :: nc, nr, nl(MAXLEVELS), compressmode, projection, ret
  integer :: vertical
  integer :: map_proj
  real    :: cen_lon, truelat1, truelat2, xlat_n, xlon_w, xlat_s, xlon_e, temp
  real, allocatable, dimension(:,:)   :: xlat, xlon
  character (len=10), dimension(number_of_variables+number_of_2dvar) :: varnam
  character (len=20) :: name, var_to_get, var_to_plot
  character (len=19) :: wrf_time
  integer :: ivar, length_var, length_plot, nlpp, number_of_levels
  integer, dimension(2) :: loc_of_min_z
  logical :: copy_grid
  type (proj_info) :: projst
  real  :: msf
  real  :: hemiflag
  real  :: pi, degtorad

   pi = 4.0*atan( 1.0 )
   degtorad = pi/180.0
!  first, set times and date from input times

  if(output_times > MAXTIMES ) then
    write(6,*) ' too many times for vis5d file '
    write(6,*) ' times requested     ',output_times
    write(6,*) ' max times for vis5d ',MAXTIMES
    stop
  endif
  if(number_of_variables > MAXVARS ) then
    write(6,*) ' too many variables for vis5d file '
    write(6,*) ' num variables requested ',number_of_variables
    write(6,*) ' max variables for vis5d ',MAXVARS
    stop
  endif

  do it = 1, output_times
    call time_calc(times(it), timestamp(it), datestamp(it), .true. )
  enddo

!  need to pull out some data to set up dimensions, etc.

  call get_dims_cdf( file_for_time(1), 'T', dims, ndims, debug )

  if(debug) write(6,*) ' dims ',dims(1), dims(2), dims(3), dims(4)

  allocate( z(dims(1), dims(2), dims(3)) )
  allocate( ph(dims(1), dims(2), dims(3)+1) )
  allocate( phb(dims(1), dims(2), dims(3)+1) )
  allocate( z_v5d_order(dims(2), dims(1), dims(3)) )

! get base and perturbation height at full eta levels:

  call get_var_3d_real_cdf( file_for_time(1),'PH',ph,  &
                            dims(1),dims(2),dims(3)+1,1,debug )
  call get_var_3d_real_cdf( file_for_time(1),'PHB',phb,  &
                            dims(1),dims(2),dims(3)+1,1,debug )

! compute Z at half levels:

  ph = (ph+phb)/9.81
  z = 0.5*(ph(:,:,1:dims(3))+ph(:,:,2:dims(3)+1))

  if(debug) write(6,*) z(dims(1)/2,dims(2)/2,:)

  call get_gl_att_real_cdf( file_for_time(1), 'DX', dx, debug )
  call get_gl_att_real_cdf( file_for_time(1), 'DY', dy, debug )


!  here are the mods for the number and specification of the output levels

  z = z/1000.  ! convert to kilometers
  copy_grid = .false.

  if( output_input_grid .and. (.not. use_lowest_heights)) then

    ! no interp, just put out computational grid

    vertical = 0
    number_of_levels = dims(3)
    vert_args(1) = 0.
    vert_args(2) = .8
    copy_grid = .true.

  else if( output_input_grid .and. (use_lowest_heights)) then 

    vertical = 2
    ! use lowest column for z heights of grids
    number_of_levels = dims(3)
    loc_of_min_z = minloc(z(:,:,1))
    vert_args(1:number_of_levels) = &
       z(loc_of_min_z(1),loc_of_min_z(2),1:number_of_levels)
    if (debug) write (6,*) ' grid point of lowest height ',loc_of_min_z(1),loc_of_min_z(2)

  else

    vertical = 2
    number_of_levels = num_v5d_levels
    vert_args(1:number_of_levels) = v5d_z_levels(1:number_of_levels)

  endif

  if(debug) then
    write(6,*) ' number of levels = ',number_of_levels
    do k=1, number_of_levels
      write(6,*) ' k, vert_args(k) ',k,vert_args(k)
    enddo
  end if

  nc = dims(1)
  nr = dims(2)

  compressmode = 1

! try to get map information

  call get_gl_att_int_cdf( file_for_time(1), 'MAP_PROJ', map_proj, debug )

  if ( map_proj /= 0 ) then
!    get more map parameters first
     call check_gl_att_real_cdf( file_for_time(1), 'STAND_LON', rcode, debug )
     if ( rcode == 0 ) then
       print*," We are dealing with Version 2 data"
       call get_gl_att_real_cdf( file_for_time(1), 'STAND_LON', cen_lon, debug )
     else
       print*," We are dealing with Version 1 data"
       call get_gl_att_real_cdf( file_for_time(1), 'CEN_LON', cen_lon, debug )
     endif

     call get_gl_att_real_cdf( file_for_time(1), 'TRUELAT1', truelat1, debug )
     call get_gl_att_real_cdf( file_for_time(1), 'TRUELAT2', truelat2, debug )
     allocate( xlat(dims(1), dims(2)) )
     allocate( xlon(dims(1), dims(2)) )
     call get_var_2d_real_cdf( file_for_time(1), 'XLAT', xlat,  &
                               dims(1), dims(2), 1, debug       )
     call get_var_2d_real_cdf( file_for_time(1), 'XLONG', xlon,  &
                               dims(1), dims(2), 1, debug       )
     xlat_n = xlat(1, dims(2))
     xlon_w = xlon(1, dims(2))
     xlat_s = xlat(1, 1)
     xlon_e = xlon(dims(1), dims(2))
     if (debug) write(6,*) ' corner values: ', xlat_n, xlon_w, xlat_s, xlon_e
  end if
  print *,' ******* UTIL map_proj ',map_proj

  if (map_proj == 0) then

      projection = 0
!     proj_args(1) = float(dims(2)-1)*dy/1000.
!     proj_args(2) = 0.
!     proj_args(3) = dy/1000.
!     proj_args(4) = -dx/1000.
      proj_args(1) = float(dims(2))
      proj_args(2) = 1.
      proj_args(3) = 1.
      proj_args(4) = -1.

  else if (map_proj == 1) then
!   lambert-conformal

      projection = 2

!  make sure truelat1 is the larger number

      if (truelat1 < truelat2) then
         if (debug) write (6,*) ' switching true lat values'
         temp = truelat1
         truelat1 = truelat2
         truelat2 = temp
      endif
      call make_lambert(truelat1,truelat2,cen_lon,xlat_n,xlon_w,dx,xx,yy)
      proj_args(1) = truelat1
      proj_args(2) = truelat2
      proj_args(3) = -yy
      proj_args(4) = -xx 
      proj_args(5) = -cen_lon
      proj_args(6) = 0.001*dx
      if (debug) write (6,*) (proj_args(it),it=1,6)

  elseif (map_proj == 2) then
    
    call map_set(PROJ_PS,xlat(1,1),xlon(1,1),dx,cen_lon,truelat1,truelat2, &
                 dims(1),dims(2),projst) 
    ! polar stereo
    projection = 3

    if (truelat1 .ge. 0.) then
      proj_args(1) = 90.
      hemiflag = 1.
    else
      proj_args(1) = -90.
      hemiflag = -1.
    endif
    proj_args(2) = -cen_lon 
    proj_args(3) = float(dims(2)) - projst%polej
    proj_args(4) = projst%polei
    msf  = (1.+hemiflag*sin(projst%truelat1*degtorad))/ &
           (1.+hemiflag*sin(proj_args(1)*degtorad))
    proj_args(5) = dx* 0.001/msf
   
  else if (map_proj == 3) then
!   mercator

! RGF: later versions of Vis5D have true Mercator, so don't need to fake it w/ cylindrical
!   equidistant projection anymore.  "was" marks lines that have been altered
      projection = 5   ! was 1
!  unneeded   call make_mercator(dims(1),dims(2),xlat_n,xlon_w,xlat_s,xlon_e,xx,yy)
      proj_args(1) = truelat1   ! was xlat_n
      proj_args(2) = cen_lon    ! was -xlon_w
      proj_args(3) = 0.001*dy   ! was abs(yy)
      proj_args(4) = 0.001*dx   ! was abs(xx)
! end RGF

  end if

  do it=1, number_of_variables
    nl(it) = number_of_levels
    name = variables(it)
    varnam(it) = name(1:10)
  enddo

  do it=1, number_of_2dvar
    nl(number_of_variables+it) = 1
    name = variables2d(it)
    varnam(number_of_variables+it) = name(1:10)
  enddo

! create the  vis5d file

  if(debug) then
    write(6,*) ' parameters in v5dcreate '
    write(6,*) ' number of variables to process    = ',number_of_variables
    write(6,*) ' number of 2d variables to process = ',number_of_2dvar
    do k=1,number_of_variables+number_of_2dvar
      write(6,*) k, varnam(k)
    enddo
    write(6,*) ' output times '
    do k=1,output_times
      write(6,*) k, timestamp(k)
    enddo
    write(6,*) ' horizontal dims ',nc,nr
    write(6,*) ' vertical dims nl '
    do k=1,number_of_variables
      write(6,*) k, nl(k)
    enddo
    do k=1+number_of_variables,number_of_2dvar+number_of_variables
      write(6,*) k, nl(k)
    enddo
    write(6,*) 'vert coord: ',vertical,vert_args(1:number_of_levels)
    write(6,*) 'projection: ',projection, proj_args(1:4)
  endif

  ret = v5dCreate( vis5d_file, output_times,                      &
                   number_of_variables+number_of_2dvar,           &
                   nr, nc, nl,                                    &
                   varnam, timestamp, datestamp,                  &
                   compressmode, projection,                      &
                   proj_args, vertical, vert_args                )

  if( ret == 0 ) then
    write(6,*)  ' v5dCreate failed ' 
    call exit(1)
  else
    write(6,*) 'Vis5D file created sucessfully '
  endif

!  now fill the vis5d file, loop over time, variables.
!  first, get some special variables (base-state-stuff, etc. )
!  NOTE the different order (y,x,z)

! allocate space for the other variables

  allocate( data_out_z(dims(2), dims(1), number_of_levels) ) ! for interp to correct level
  allocate( pb(dims(1), dims(2), dims(3)) )
  allocate( p(dims(1), dims(2), dims(3)) )

  call get_var_3d_real_cdf( file_for_time(1),'PB',pb,       &
                            dims(1),dims(2),dims(3),1,debug )

  print *,' '
  print *,'Begin time loop'
  print *,' '

  do it = 1, output_times

!  first, read ph, and compute z

    call get_dims_cdf( file_for_time(it), 'PH', dims, ndims, debug )
    call get_var_3d_real_cdf( file_for_time(it),'PH',ph,  &
                            dims(1),dims(2),dims(3),    &
                            file_time_index(it),debug )
    ph = (ph+phb)/9.81
    z = 0.5*(ph(:,:,1:dims(3)-1)+ph(:,:,2:dims(3)))

!   need to convert to kilometers for coordinate
    z = z/1000.

!  reorder the z data so we can interp in z before v5d write

    call reorder_data( z, z_v5d_order, 'Z         ',    &
                       dims(1), dims(2), dims(3)-1,       &
                       nr, nc, dims(3)-1,dims(3)-1, p, debug    )
!CB                       nr, nc, nl(1), nlpp, p, debug    )

    call get_dims_cdf( file_for_time(it), 'P', dims, ndims, debug )
    call get_var_3d_real_cdf( file_for_time(it),'P',p,  &
                              dims(1),dims(2),dims(3),  &
                              file_time_index(it),debug )
    p = p+pb

    nlpp = dims(3)

    do ivar = 1, number_of_variables 
      var_to_get = variables(ivar)
      var_to_plot = var_to_get

      call check_special_variable( var_to_get )

      length_var = len_trim(var_to_get)
      length_plot = len_trim(var_to_plot)

      if(debug) write(6,*) ' getting dims for ',var_to_get(1:length_var)

      call get_dims_cdf( file_for_time(it), var_to_get(1:length_var), &
                         dims, ndims, debug                         )

      allocate( data_in(dims(1), dims(2), dims(3)) )
      allocate( data_out(dims(2), dims(1), dims(3)) )  ! note different order for v5d output

      if(debug) write(6,*) ' getting data for ',var_to_get(1:length_var)
      call get_var_3d_real_cdf( file_for_time(it),var_to_get(1:length_var), &
                                data_in, dims(1),dims(2),dims(3),           &
                                file_time_index(it),debug  )
      if(debug) write(6,*) ' reorder for ',var_to_get(1:length_var)

      call reorder_data( data_in, data_out, var_to_plot, &
                         dims(1), dims(2), dims(3),      &
                         nr, nc, nlpp, nlpp, p, debug )
!CB                         nr, nc, nl(ivar), nlpp, p, debug )
      
      print *, 'nl(ivar)         = ', nl(ivar)
      print *, 'number_of_levels = ', number_of_levels
!CB      call interp_to_v5d_z( data_out  , nr, nc, nl(ivar),          &
      call interp_to_v5d_z( data_out  , nr, nc, nlpp,          &
                            data_out_z, nr, nc, number_of_levels,  &
                            z_v5d_order, vert_args, missing,       &
                            copy_grid, debug )

      if(debug) write(6,*) ' writing out variable, time ',ivar,it
      wrf_time = times(it) 
      write(6,*) ' time ',wrf_time,', output variable ',var_to_plot(1:length_plot)

      ret =  v5dwrite( it, ivar, data_out_z )
      if(ret == 1) then
        if(debug) write(6,*) ' variable ',var_to_plot(1:length_plot), &
                             ' output successfully at time ',it
      else
        write(6,*) ' error writing variable ', var_to_plot(1:length_plot), &
                   ' to vis5d file, ret = ',ret
        write(6,*) ' error stop '
        stop
      endif

      deallocate(data_in)
      deallocate(data_out)

    enddo

    do ivar = 1, number_of_2dvar
    
      var_to_get = variables2d(ivar)
      var_to_plot = var_to_get

      length_var = len_trim(var_to_get)
      length_plot = len_trim(var_to_plot)

      if(debug) write(6,*) ' getting dims for ',var_to_get(1:length_var)

      call get_dims_cdf( file_for_time(it), var_to_get(1:length_var), &
                         dims, ndims, debug                         )

      allocate( data_in(dims(1), dims(2), 1) )
      allocate( data_out(dims(2), dims(1), 1) )

      if(debug) write(6,*) ' getting data for ',var_to_get(1:length_var)

! RGF: file_for_time(1) changed to file_for_time(it), noticed by hmodzelewski..
      call get_var_2d_real_cdf( file_for_time(it),var_to_get(1:length_var), &
                                data_in, dims(1), dims(2),                 &
                                file_time_index(it), debug               )

      if(debug) write(6,*) ' reorder for ',var_to_get(1:length_var)

      call reorder_data( data_in, data_out, var_to_plot, &
                         dims(1), dims(2), 1,            &
                         nr, nc, nl(ivar+number_of_variables), &
                         nlpp, p, debug                  )
      
      print *, 'nl(ivar)         = ', nl(ivar+number_of_variables)
!     print *, 'number_of_levels = ', number_of_levels

      if(debug) write(6,*) ' writing out variable, time ',ivar+number_of_variables,it
      wrf_time = times(it) 
      write(6,*) ' time ',wrf_time,', output variable ',var_to_plot(1:length_plot)

      ret =  v5dwrite( it, ivar+number_of_variables, data_out )

      if(ret == 1) then
        if(debug) write(6,*) ' variable ',var_to_plot(1:length_plot), &
                             ' output successfully at time ',it
      else
        write(6,*) ' error writing variable ', var_to_plot(1:length_plot), &
                   ' to vis5d file, ret = ',ret
        write(6,*) ' error stop '
        stop
      endif

      deallocate(data_in)
      deallocate(data_out)

    enddo
  enddo

  deallocate(data_out_z)
  deallocate(z_v5d_order)

!  we're finished

  ret = v5dclose()
  if(ret == 1) then
    write(6,*) ' v5d file successfully closed '
  else
    write(6,*) ' error closing v5d file, ret =  ', ret
  endif

  end subroutine create_vis5d

!------------------------------------------------------------------------------

  subroutine read_time_list( io_unit, times, max_times, output_times,   &
                              all_times, debug                           )
  implicit none

  integer, intent(in)  :: max_times, io_unit
  integer, intent(out) :: output_times
  logical, intent(out) :: all_times
  logical, intent(in ) :: debug

  character (len=80), intent(out) :: times(max_times)
  character (len=80)              :: tmp_time

  integer :: itimes, length


  read(io_unit, *) output_times
  if(output_times > 0) then

    all_times = .false.
    do itimes = 1, output_times
      read(io_unit,fmt='(a80)') tmp_time
      length = max(1,index(tmp_time,' ')-1)
      times(itimes) = tmp_time(1:length)
      if(debug) write(6,*) ' output time ',itimes,' is ',times(1:length)
    enddo

  else

    all_times = .true.
    output_times = -output_times
    do itimes = 1, output_times
      read(io_unit,fmt='(a80)') tmp_time
      length = max(1,index(tmp_time,' ')-1)
      if(debug) write(6,*) ' tmp_time is ',tmp_time(1:length)
    enddo
    output_times = 0

  end if

  end subroutine read_time_list

!---------------------------------------------------------------------------------

  subroutine read_variable_list( io_unit, variables, max_variables, &
                                 number_of_variables,               &
                                 variables2d, number_of_2dvar, debug )

  implicit  none

  integer, intent(in)  :: max_variables, io_unit
  integer, intent(out) :: number_of_variables, number_of_2dvar
  character (len=20), intent(out) :: variables(max_variables-30), variables2d(30)
  character (len=20)              :: tmp_var
  integer :: length
  logical, intent(in) :: debug

  logical read_complete

  read_complete = .false.
  number_of_variables = 0
  number_of_2dvar     = 0

  do while( .not. read_complete )
    read(io_unit, fmt='(a20)') tmp_var
!   if(debug) write(6,*) ' variable ', tmp_var
    if(tmp_var(1:1) /= ' ' .and. tmp_var(1:20) /= 'end_of_variable_list') then
! RGF added 2D vars to this list, so they may be recognized and treated as 2D
      if(tmp_var(1:3) == 'TSK' .or. tmp_var(1:4) == 'RAIN' .or. tmp_var(2:3) == 'FX'.or. tmp_var(2:3) == '10'.or.tmp_var(2:2) =='2'.or.tmp_var(1:4) == 'PSFC'.or.tmp_var(1:3)=='SST'.or.tmp_var(1:3)=='HGT')then
        number_of_2dvar = number_of_2dvar + 1
        length = max(1,index(tmp_var,' ')-1)
        variables2d(number_of_2dvar) = tmp_var(1:length)
        if(debug) write(6,*) ' variable ', number_of_2dvar,' ', &
                             variables2d(number_of_2dvar)
      else
        number_of_variables = number_of_variables + 1
        length = max(1,index(tmp_var,' ')-1)
        variables(number_of_variables) = tmp_var(1:length)
        if(debug) write(6,*) ' variable ', number_of_variables,' ', &
                             variables(number_of_variables)
      end if
    else if(tmp_var(1:20) == 'end_of_variable_list') then
      read_complete = .true.
    end if
  enddo

  end subroutine read_variable_list

!---------------------------------------------------------------------------------

  subroutine  read_file_list( io_unit, files, max_files,  &
                              number_of_files, debug     )

  implicit none

  integer, intent(in)  :: max_files, io_unit
  integer, intent(out) :: number_of_files
  character (len=80), intent(out) :: files(max_files)  
  character (len=80)              :: file_in
  logical, intent(in) :: debug
  logical :: read_complete
  integer :: length

  read_complete = .false.
  number_of_files = 0

  do while( .not. read_complete )
    read(io_unit, fmt='(a80)') file_in
    if(file_in(1:16) /= 'end_of_file_list') then
      number_of_files = number_of_files + 1
      length = max(1,index(file_in,' ')-1)
      files(number_of_files) = file_in(1:length)
      if(debug) write(6,*) ' file ', number_of_files,' ', &
                           file_in(1:length)
    else 
      read_complete = .true.
    end if
  enddo

  end subroutine read_file_list

!---------------------------------------------------------------------------------

  subroutine create_time_file_list( data_files,      &
                                    file_for_time,   &
                                    file_time_index, &
                                    times,           &
                                    max_times,       &
                                    all_times,       &
                                    output_times,    &
                                    number_of_files, &
                                    debug           )

  implicit none

  integer, intent(in)  :: max_times, number_of_files
  integer, intent(out) :: output_times
  character (len=80) :: data_files(max_times),times(max_times), &
                        file_for_time(max_times)
  character (len=80) :: times_in_file(max_times)
  integer, intent(out), dimension(max_times) :: file_time_index
  logical, intent(in) :: debug, all_times

  character (len=80) :: input_time
  character (len=80) :: time1, time2, file_check
  character (len=19) :: wrf_time
  integer :: file_index, i, j, n_times, length,length1
  logical :: already_on_list

!  put together the list of "times" and "file_for_time".

!  for each file, find the times in that file and either
!  (1)  see if it corresponds to a required time - then set the
!       file for that time, or
!  (2)  if all times are desired, add if to the list if it does
!       not duplicate a time already in the list

  if( .not. all_times ) then
    do i=1,output_times
      file_for_time(i) = 'no_time_found'
    enddo
  end if

  loop_over_files : do file_index = 1, number_of_files

    if(debug) write(6,*) ' getting times for file ',data_files(file_index)
    call get_times_cdf( data_files(file_index), times_in_file, &
                        n_times, max_times, debug             )
    if( n_times <= 0 ) then
      write(6,*) ' no output times found in ',data_files(file_index)
      write(6,*) ' error stop '
      stop
    end if

    if(debug) then
      write(6,*) ' times from netcdf file '
      do i=1,n_times
        wrf_time = times_in_file(i)
        write(6,*) i,wrf_time
      enddo
    endif

    if(all_times) then

      if(debug) write(6,*) ' sorting all times '
      do i=1, n_times
!        length = max(1,index(times_in_file(i),' ')-1)
        length = 19
        already_on_list = .false.
        time1 = times_in_file(i)
        do j=1, output_times
          time2 = times(j)
          if( time1(1:length) == time2(1:length)) already_on_list = .true.
        enddo
        if(.not.already_on_list) then
          output_times = output_times+1
          times(output_times) = time1
          file_for_time(output_times) = data_files(file_index)
          file_time_index(output_times) = i
        endif
      enddo

    else

      do i=1, n_times
!        length = max(1,index(times_in_file(i),' ')-1)
        length = 19
        do j=1, output_times
          time1 = times_in_file(i)
          time2 = times(j)
          if( time1(1:length) == time2(1:length)) then
             file_for_time(j) = data_files(file_index)
             file_time_index(j) = i
          end if
        enddo
      enddo

    end if

  enddo loop_over_files

!  check here to see if we have data for all times if 
!  times were specified as input

  if( .not. all_times) then
    do i=1,output_times
      file_check = file_for_time(i)
      if( file_check(1:13) ==  'no_time_found') then
        write(6,*) ' no data found for time ',times(i)
        write(6,*) ' error stop '
        stop
      end if
    enddo
  end if

!  if(debug) then
    write(6,*) ' time and file list '
    do i=1,output_times
      time1 = times(i)
      file_check = file_for_time(i)
      length = max(1,index(file_check,' ')-1)
      length1 = 19
      write(6,*) i,time1(1:length1),' ',file_check(1:length),file_time_index(i)
    enddo
!  end if

  end subroutine create_time_file_list

!-------------------------------------------------------------------

  subroutine time_calc( time, timestamp, datestamp, debug )

  implicit none

  character (len=80), intent(in) ::  time
  integer, intent(out) :: timestamp, datestamp
  logical, intent(in) :: debug

  integer :: hours, minutes, seconds, year, month, day

  read(time(18:19),*) seconds
  read(time(15:16),*) minutes
  read(time(12:13),*) hours
  read(time(1:4),*)   year
  read(time(6:7),*)   month
  read(time(9:10),*)  day

  if(debug) write(6,*) ' day, month, year, hours, minutes, seconds '
  if(debug) write(6,*) day, month, year, hours, minutes, seconds 

  timestamp = seconds+100*minutes+10000*hours

  if((year > 1800) .and. (year < 2000)) year = year-1900
  if((year >= 2000)) year = year-2000

  if(month >= 2) day = day+31  ! add january
  if(month >= 3) day = day+28  ! add february
  if(month >= 4) day = day+31  ! add march
  if(month >= 5) day = day+30  ! add april
  if(month >= 6) day = day+31  ! add may
  if(month >= 7) day = day+30  ! add june
  if(month >= 8) day = day+31  ! add july
  if(month >= 9) day = day+31  ! add august
  if(month >= 10) day = day+30 ! add september
  if(month >= 11) day = day+31 ! add october
  if(month >= 12) day = day+30 ! add november
  if((month > 2) .and. (mod(year,4) == 0)) day = day+1  ! get leap year day

  datestamp = (year)*1000 + day
!  datestamp = (year+2100)*1000 + day

  if(debug) then
    write(6,*) ' time, timestamp, datestamp ',time(1:19),timestamp,datestamp
  endif

  end subroutine time_calc

!-------------------------------------------------------------------------

  subroutine reorder_data( data_in, data_out, var, &
                           d1_in, d2_in, d3_in,    &
                           d1_out, d2_out, d3_out, &
                           d3_p, p, debug  )
  implicit none

  integer, intent(in) ::  d1_in, d2_in, d3_in,    &
                          d1_out, d2_out, d3_out, &
                          d3_p

  real, intent(in ), dimension(d1_in ,d2_in ,d3_in ) :: data_in
  real, intent(out), dimension(d1_out,d2_out,d3_out) :: data_out
  real, intent(in),  dimension(d1_in ,d2_in ,d3_p)   :: p

  integer :: i,k,j
  character (len=10), intent(in) :: var
  logical, intent(in) :: debug

   REAL    , PARAMETER :: g = 9.81  ! acceleration due to gravity (m {s}^-2)

   REAL    , PARAMETER :: r_d          = 287.
   REAL    , PARAMETER :: r_v          = 461.6
   REAL    , PARAMETER :: cp           = 7.*r_d/2.
   REAL    , PARAMETER :: cv           = cp-r_d
   REAL    , PARAMETER :: cliq         = 4190.
   REAL    , PARAMETER :: cice         = 2106.
   REAL    , PARAMETER :: psat         = 610.78
   REAL    , PARAMETER :: rcv          = r_d/cv
   REAL    , PARAMETER :: rcp          = r_d/cp
   REAL    , PARAMETER :: c2           = cp * rcv
   REAL    , PARAMETER :: T0           = 273.16

   REAL    , PARAMETER :: p1000mb      = 100000.
   REAL    , PARAMETER :: cpovcv       = cp/(cp-r_d)
   REAL    , PARAMETER :: cvovcp       = 1./cpovcv
   REAL   :: pp

  if(debug) then
    write(6,*) ' reorder for variable ',var
    write(6,*) ' dimensions in  ',d1_in, d2_in, d3_in, d3_p
    write(6,*) ' dimensions out ',d1_out, d2_out, d3_out
  end if

  if(var == 'U ') then
    do k=1,d3_out
    do j=1,d2_out
    do i=1,d1_out
      data_out(i,j,k) = 0.5*( data_in(j,d1_out-i+1,k)    &
                             +data_in(j+1,d1_out-i+1,k))
    enddo
    enddo
    enddo
  else if(var == 'V ') then
    do k=1,d3_out
    do j=1,d2_out
    do i=1,d1_out
      data_out(i,j,k) = 0.5*( data_in(j,d1_out-i+1,k)    &
                             +data_in(j,d1_out-i+2,k))
    enddo
    enddo
    enddo
  else if(var == 'W ') then
    do k=1,d3_out
    do j=1,d2_out
    do i=1,d1_out
      data_out(i,j,k) = 0.5*( data_in(j,d1_out-i+1,k  )    &
                             +data_in(j,d1_out-i+1,k+1))
    enddo
    enddo
    enddo
  else if(var == 'THETA') then
    do k=1,d3_out
    do j=1,d2_out
    do i=1,d1_out
      data_out(i,j,k) = data_in(j,d1_out-i+1,k)+300.
    enddo
    enddo
    enddo
  else if(var == 'TK') then
    if(debug) write(6,*) 'working on TK'
    do k=1,d3_out
    do j=1,d2_out
    do i=1,d1_out
      data_out(i,j,k) = (data_in(j,d1_out-i+1,k)+300.)*(p(j,d1_out-i+1,k)/p1000mb)**rcp
    enddo
    enddo
    enddo
  else if(var == 'TC') then
    if(debug) write(6,*) 'working on TC'
    do k=1,d3_out
    do j=1,d2_out
    do i=1,d1_out
      data_out(i,j,k) = (data_in(j,d1_out-i+1,k)+300.)*(p(j,d1_out-i+1,k)/p1000mb)**rcp - T0
    enddo
    enddo
    enddo
  else 
    do k=1,d3_out
    do j=1,d2_out
    do i=1,d1_out
      data_out(i,j,k) = data_in(j,d1_out-i+1,k) 
    enddo
    enddo
    enddo
  end if

  end subroutine reorder_data

!------------------------------------------------------------------

  subroutine check_special_variable( var_to_get )

  implicit none
  character (len=20), intent(inout) :: var_to_get

  if(var_to_get(1:6) == 'THETA ' .or. var_to_get(1:6) == 'TC    ' &
     .or. var_to_get(1:6) == 'TK    ') then
    var_to_get(1:6) = 'T     '
  else if(var_to_get(1:2) == 'Z ') then
    var_to_get(1:6) = 'PH    '
  end if

  end subroutine check_special_variable

!--------------------------------------------------------

  subroutine interp_to_v5d_z( data_in , nx_in , ny_in , nz_in , &
                              data_out, nx_out, ny_out, nz_out, &
                              z_in, z_out, missing_value, copy, debug   )
  implicit none
  integer, intent(in) :: nx_in , ny_in , nz_in , nx_out, ny_out, nz_out
  real, intent(in) :: missing_value
  real, dimension(nx_in , ny_in , nz_in ), intent(in ) :: data_in, z_in
  real, dimension(nx_out, ny_out, nz_out), intent(out) :: data_out
  real, dimension(nz_out), intent(in)                  :: z_out
  logical, intent(in) :: copy, debug

  real, dimension(nz_in) :: data_in_z, zz_in
  real, dimension(nz_out) :: data_out_z

  integer :: i,j,k

  if(.not. copy) then

    do i=1,nx_in
    do j=1,ny_in

      do k=1,nz_in
        data_in_z(k) = data_in(i,j,k)
        zz_in(k) = z_in(i,j,k)
      enddo
      do k=1,nz_out
        data_out_z(k) = data_out(i,j,k)
      enddo

      call interp_1d( data_in_z, zz_in, nz_in, &
                      data_out_z, z_out, nz_out, missing_value )

      do k=1,nz_out
        data_out(i,j,k) = data_out_z(k)
      enddo

    enddo
    enddo

  else

    if(debug) write(6,*) ' copying data from data_in to data_out in interp_to_v5d_z '
    if(debug) write(6,*) ' dims diff ',nx_in-nx_out, ny_in-ny_out, nz_in-nz_out

    do k=1,nz_in
    do j=1,ny_in
    do i=1,nx_in
    
      data_out(i,j,k) = data_in(i,j,k)

    enddo
    enddo
    enddo

  endif

  end subroutine interp_to_v5d_z

!----------------------------------------------

  subroutine interp_1d( a, xa, na, &
                        b, xb, nb, missing_value )
  implicit none
  integer, intent(in) ::  na, nb
  real, intent(in), dimension(na) :: a, xa
  real, intent(in), dimension(nb) :: xb
  real, intent(out), dimension(nb) :: b
  real, intent(in) :: missing_value

  integer :: n_in, n_out
  logical :: interp
  real    :: w1, w2


  do n_out = 1, nb

    b(n_out) = missing_value
    interp = .false.
    n_in = 1

    do while ( (.not.interp) .and. (n_in < na) )

      if( (xa(n_in)   <= xb(n_out)) .and. &
          (xa(n_in+1) >= xb(n_out))        ) then
        interp = .true.
        w1 = (xa(n_in+1)-xb(n_out))/(xa(n_in+1)-xa(n_in))
        w2 = 1. - w1
        b(n_out) = w1*a(n_in) + w2*a(n_in+1)
      end if
      n_in = n_in +1

    enddo

  enddo

  end subroutine interp_1d

  subroutine make_lambert(truelat1,truelat2,cen_lon,xlatn,xlonw,dx,xx,yy)

!  imported from TOVIS5D for MM5

   real truelat1,truelat2,cen_lon,xlatn,xlonw,dx,xx,yy
   real radius, degtorad, alpha
   real theta, theta1, theta2, htheta, htheta1, htheta2, cone, dis, rad
   data radius /6371./

!  put map parameters in as defined by vis5d;
!    south is positive direction, so negate ypole
   pi = 4.0*atan( 1.0 )
   degtorad = pi/180.0

   dis = 0.001*dx

   theta  = (90.0-xlatn) * degtorad
   theta1 = (90.0-truelat1) * degtorad
   theta2 = (90.0-truelat2) * degtorad
   htheta = theta /2.0
   htheta1= theta1/2.0
   htheta2= theta2/2.0
! RGF: original code had problems since if truelat1=truelat2, cone = nan
! this resulted in wind vectors not plotting in V5D, tho u, v contours could be plotted
   if(truelat1.ne.truelat2)then   ! RGF
    cone   = alog(sin( theta1)/sin( theta2))/alog(tan(htheta1)/tan(htheta2)) ! orig line
   else ! tring something after RIP4/src/ripdp_wrf.f, line 2184
    cone =   cos(theta1) ! trial... seems okay... coast map lines up well with skin temp and topo fields
   endif 
! end RGF

   alpha  = cone * (xlonw - cen_lon) * degtorad
     rad  = radius * sin(theta1) * (tan(htheta)/tan(htheta1))**cone/cone

   xx = rad * sin(alpha) / dis
   yy = rad * cos(alpha) / dis

!  vis5d_numb_proj_par = 6

  end subroutine make_lambert

  subroutine make_mercator(ix,jy,xlatn,xlonw,xlats,xlone,latinc,loninc)

!  make pseudo mercator projection using vis5d 'cylindrical equidistant'
!    cylidrical equidistant should produce reasonably accurate approx to merc.
!  imported from TOVIS5D for MM5

   real    xlatn,xlonw,xlats,xlone, latinc, loninc
   integer ix,jy

!  subtract 2 because we are using cross pt, not dot pt grid

   loninc = (xlonw - xlone) / (ix - 2)
   latinc = (xlatn - xlats) / (jy - 2)

!  vis5d_numb_proj_par = 4

  end subroutine make_mercator

END MODULE module_wrf_to_v5d_util

