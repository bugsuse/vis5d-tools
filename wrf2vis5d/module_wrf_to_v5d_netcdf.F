MODULE module_wrf_to_v5d_netcdf

CONTAINS

!--------------------------------------------------------------------

  subroutine get_times_cdf( file, times, n_times, max_times, debug )
        
  implicit none

  include 'netcdf.inc'

  integer, intent(in)  ::  max_times  
  integer, intent(out) ::  n_times
  character (len=80), intent(in) :: file
  character (len=80), intent(out) :: times(max_times)
  logical, intent(in ) :: debug

  integer cdfid, rcode, id_time
  character (len=80) :: varnam, time1
  integer :: ndims, natts, idims(10), istart(10),iend(10), dimids(10)
  integer :: i, ivtype, length

  cdfid = ncopn(file, NCNOWRIT, rcode )
  length = max(1,index(file,' ')-1)
  if( rcode == 0) then
    if(debug) write(6,*) ' open netcdf file ',file(1:length)
  else
    write(6,*) ' error openiing netcdf file ',file(1:length)
    stop
  end if

  id_time = ncvid( cdfid, 'Times', rcode )

  rcode = nf_inq_var( cdfid, id_time, varnam, ivtype, ndims, dimids, natts )
  if(debug) then
    write(6,*) ' number of dims for Time ',ndims
  endif
  do i=1,ndims
    rcode = nf_inq_dimlen( cdfid, dimids(i), idims(i) )
    if(debug) write(6,*) ' dimension ',i,idims(i)
  enddo

!  get the times
  
  n_times = idims(2)
  do i=1,idims(2)
    istart(1) = 1
    iend(1) = idims(1)
    istart(2) = i
    iend(2) = 1
!    call ncvgt( cdfid,id_time,istart,iend,times(i),rcode)
    rcode = NF_GET_VARA_TEXT  ( cdfid, id_time,  &
                                istart, iend,    &
                                times(i)          )
    length = max(1,index(file,' ')-1)
    time1 = times(i)
    if(debug) write(6,*) file(1:length), time1(1:19)
  enddo

  write(6,*) ' exiting get_times_cdf '

  call ncclos(cdfid,rcode)

  end subroutine get_times_cdf

!-------------------------------------------------------------------------------

  subroutine get_dims_cdf( file, var, dims, ndims, debug )
        
  implicit none

  include 'netcdf.inc'

  character (len=80), intent(in) :: file
  character (len=*), intent(in) :: var
  logical, intent(in ) :: debug
  integer, intent(out), dimension(4) :: dims
  integer, intent(out) :: ndims

  integer cdfid, rcode, id_time
  character (len=80) :: varnam, time1
  integer :: natts, istart(10),iend(10), dimids(10)
  integer :: i, ivtype, length

  cdfid = ncopn(file, NCNOWRIT, rcode )
  length = max(1,index(file,' ')-1)
  if( rcode == 0) then
    if(debug) write(6,*) ' open netcdf file ',file(1:length)
  else
    write(6,*) ' error openiing netcdf file ',file(1:length)
    stop
  end if

  id_time = ncvid( cdfid, var, rcode )

  rcode = nf_inq_var( cdfid, id_time, varnam, ivtype, ndims, dimids, natts )
  if(debug) then
    write(6,*) ' number of dims for ',var,' ',ndims
  endif
  do i=1,ndims
    rcode = nf_inq_dimlen( cdfid, dimids(i), dims(i) )
    if(debug) write(6,*) ' dimension ',i,dims(i)
  enddo

  call ncclos(cdfid,rcode)

  end subroutine get_dims_cdf

!-------------------------------------------------------------------------------

  subroutine get_gl_att_int_cdf( file, att_name, value, debug )
        
  implicit none

  include 'netcdf.inc'

  character (len=80), intent(in) :: file
  character (len=*), intent(in) :: att_name
  logical, intent(in ) :: debug
  integer, intent(out) :: value

  integer cdfid, rcode, id_time, length

  cdfid = ncopn(file, NCNOWRIT, rcode )
  length = max(1,index(file,' ')-1)
  if( rcode == 0) then
    if(debug) write(6,*) ' open netcdf file ',file(1:length)
  else
    write(6,*) ' error openiing netcdf file ',file(1:length)
    stop
  end if

  rcode = NF_GET_ATT_INT(cdfid, nf_global, att_name, value )

  call ncclos(cdfid,rcode)

  if(debug) write(6,*) ' global attribute ',att_name,' is ',value

  end subroutine get_gl_att_int_cdf

!-------------------------------------------------------------------------------

  subroutine get_gl_att_real_cdf( file, att_name, value, debug )
        
  implicit none

  include 'netcdf.inc'

  character (len=80), intent(in) :: file
  character (len=*), intent(in) :: att_name
  logical, intent(in ) :: debug
  real,    intent(out) :: value

  integer cdfid, rcode, id_time, length

  cdfid = ncopn(file, NCNOWRIT, rcode )
  length = max(1,index(file,' ')-1)
  if( rcode == 0) then
    if(debug) write(6,*) ' open netcdf file ',file(1:length)
  else
    write(6,*) ' error openiing netcdf file ',file(1:length)
    stop
  end if

  rcode = NF_GET_ATT_REAL(cdfid, nf_global, att_name, value )

  call ncclos(cdfid,rcode)

  if(debug) write(6,*) ' global attribute ',att_name,' is ',value

  end subroutine get_gl_att_real_cdf


!-------------------------------------------------------------------------------

  subroutine check_gl_att_real_cdf( file, att_name, rcode_var, debug )
        
  implicit none

  include 'netcdf.inc'

  character (len=80), intent(in) :: file
  character (len=*), intent(in) :: att_name
  logical, intent(in ) :: debug
  real                 :: value

  integer cdfid, rcode, id_time, length, rcode_var

  cdfid = ncopn(file, NCNOWRIT, rcode )
  length = max(1,index(file,' ')-1)
  if( rcode == 0) then
    if(debug) write(6,*) ' open netcdf file ',file(1:length)
  else
    write(6,*) ' error openiing netcdf file ',file(1:length)
    stop
  end if

  rcode_var = NF_GET_ATT_REAL(cdfid, nf_global, att_name, value )

  call ncclos(cdfid,rcode)

  if(debug) write(6,*) ' global attribute ',att_name,' is ',value

  end subroutine check_gl_att_real_cdf


!--------------------------------------------------------------------

  subroutine get_var_3d_real_cdf( file, var, data, &
                                  i1, i2, i3, time, debug )
        
  implicit none

  include 'netcdf.inc'

  integer, intent(in)  ::  i1, i2, i3, time
  character (len=80), intent(in) :: file
  logical, intent(in ) :: debug
  character (len=*), intent(in) :: var
  real, dimension(i1,i2,i3), intent(out) :: data

  integer cdfid, rcode, id_data
  character (len=80) :: varnam, time1
  integer :: ndims, natts, idims(10), istart(10),iend(10), dimids(10)
  integer :: i, ivtype, length

  cdfid = ncopn(file, NCNOWRIT, rcode )
  length = max(1,index(file,' ')-1)
  if( rcode == 0) then
    if(debug) write(6,*) ' open netcdf file ',file(1:length)
  else
    write(6,*) ' error openiing netcdf file ',file(1:length)
    stop
  end if

  id_data = ncvid( cdfid, var, rcode )

  rcode = nf_inq_var( cdfid, id_data, varnam, ivtype, ndims, dimids, natts )
  if(debug) then
    write(6,*) ' number of dims for ',var,' ',ndims
  endif
  do i=1,ndims
    rcode = nf_inq_dimlen( cdfid, dimids(i), idims(i) )
    if(debug) write(6,*) ' dimension ',i,idims(i)
  enddo

!  check the dimensions

   if( (i1 /= idims(1)) .or.  &
       (i2 /= idims(2)) .or.  &
       (i3 /= idims(3)) .or.  &
       (time > idims(4))     )  then

     write(6,*) ' error in 3d_var_real read, dimension problem '
     write(6,*) i1, idims(1)
     write(6,*) i2, idims(2)
     write(6,*) i3, idims(3)
     write(6,*) time, idims(4)
     write(6,*) ' error stop '
     stop

   end if

!  get the data
  
    istart(1) = 1
    iend(1) = i1
    istart(2) = 1
    iend(2) = i2
    istart(3) = 1
    iend(3) = i3
    istart(4) = time
    iend(4) = 1

    call ncvgt( cdfid,id_data,istart,iend,data,rcode)

  call ncclos(cdfid,rcode)

  end subroutine get_var_3d_real_cdf

!--------------------------------------------------------------------

  subroutine get_var_2d_real_cdf( file, var, data, &
                                  i1, i2, time, debug )
        
  implicit none

  include 'netcdf.inc'

  integer, intent(in)  ::  i1, i2, time
  character (len=80), intent(in) :: file
  logical, intent(in ) :: debug
  character (len=*), intent(in) :: var
  real, dimension(i1,i2), intent(out) :: data

  integer cdfid, rcode, id_data
  character (len=80) :: varnam, time1
  integer :: ndims, natts, idims(10), istart(10),iend(10), dimids(10)
  integer :: i, ivtype, length

  cdfid = ncopn(file, NCNOWRIT, rcode )
  length = max(1,index(file,' ')-1)
  if( rcode == 0) then
    if(debug) write(6,*) ' open netcdf file ',file(1:length)
  else
    write(6,*) ' error openiing netcdf file ',file(1:length)
    stop
  end if

  id_data = ncvid( cdfid, var, rcode )

  rcode = nf_inq_var( cdfid, id_data, varnam, ivtype, ndims, dimids, natts )
  if(debug) then
    write(6,*) ' number of dims for ',var,' ',ndims
  endif
  do i=1,ndims
    rcode = nf_inq_dimlen( cdfid, dimids(i), idims(i) )
    if(debug) write(6,*) ' dimension ',i,idims(i)
  enddo

!  check the dimensions

   if( (i1 /= idims(1)) .or.  &
       (i2 /= idims(2)) .or.  &
       (time > idims(3))     )  then

     write(6,*) ' error in 2d_var_real read, dimension problem '
     write(6,*) i1, idims(1)
     write(6,*) i2, idims(2)
     write(6,*) time, idims(4)
     write(6,*) ' error stop '
     stop

   end if

!  get the data
  
    istart(1) = 1
    iend(1) = i1
    istart(2) = 1
    iend(2) = i2
    istart(3) = time
    iend(3) = 1

    call ncvgt( cdfid,id_data,istart,iend,data,rcode)

  call ncclos(cdfid,rcode)

  end subroutine get_var_2d_real_cdf

END MODULE module_wrf_to_v5d_netcdf
