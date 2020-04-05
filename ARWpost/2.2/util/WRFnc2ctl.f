!! This program read a WRF netcdf file and create the necessary .ctl 
!! files to display WRF netcdf directly with GrADS.
!!
!! Note, due to staggering, you get 4 .ctl files, one for mass points, one
!! for U points, one for V points and one for W points. 
!! They need to be open all at once in GrADS.
!!
!! When using this method to display WRF in GrADS, one cannot inpertolate 
!! to pressure/height levels, and no extra diagnostics are available
!
!=================================Make Executable============================
!  Make executable:
!    DEC Alpha
!      f90 WRFnc2ctl.f -L/usr/local/netcdf/lib -lnetcdf -lm  \
!      -I/usr/local/netcdf/include  -free  -o WRFnc2ctl
!
!   pgf90 flags 
!      pgf90 WRFnc2ctl.f -L/usr/local/netcdf/lib -lnetcdf -lm  \
!      -I/usr/local/netcdf/include  -Mfree  -o WRFnc2ctl
!
!   gfortran flags
!      gfortran WRFnc2ctl.f  -L/usr/local/netcdf/lib/ -lnetcdf -lm \
!      -I/usr/local/netcdf/include/ -ffree-form -ffree-line-length-none -o WRFnc2ctl
!
!   Sun flags
!      f90 WRFnc2ctl.f -L/usr/local/netcdf/lib -lnetcdf -lm  \
!      -I/usr/local/netcdf/include  -free  -o WRFnc2ctl
!
!   SGI flags
!      f90 WRFnc2ctl.f -L/usr/local/netcdf/lib -lnetcdf -lm  \
!      -I/usr/local/netcdf/include  -freeform  -o WRFnc2ctl
!
!   IBM flags
!      xlf WRFnc2ctl.f -L/usr/local/lib32/r4i4 -lnetcdf -lm  \
!      -I/usr/local/netcdf/include  -qfree=f90  -o WRFnc2ctl
!
!   Mac flags (with xlf compiler)
!      xlf WRFnc2ctl.f -L/usr/local/netcdf-xlf/lib -lnetcdf -lm  \
!      -I/usr/local/netcdf-xlf/include  -qfree=f90  -o WRFnc2ctl
!
!   If you extra compile flags for other computers - please send along
!
!=================================Run Program================================
!  Run program:
!
!          WRFnc2ctl   -i wrf_netcdf_input_file   -o ctl_output_name
!
!=================================Options====================================
!
! -help     : Print help information                           
! -h        : Print help information                           
! -i        : WRF input file (netcdf format)                   
! -o        : ctl output name                                  
! -debug    : Print some debug information
!
!============================================================================
!
!  December 2006
!  Cindy Bruyere
!
     program WRFnc2ctl

     implicit none
     include 'netcdf.inc'

  character (len=200)                                 :: input_file                  ! netcdf input file
  logical                                             :: have_vert_stag=.FALSE.      ! also need W.ctl file
  integer                                             :: cdfid                       ! input file unit number
  real                                                :: rcode                       ! return status code for netcdf
  integer                                             :: ndims, nvars, natts         ! number of dims, var and att in file
  integer                                             :: unlimdimid                  ! unlimited dims ID - not used

  character (len=80)                                  :: case                        ! ctl file name
  character (len=80)                                  :: ctl_U, ctl_V, ctl_W, ctl_M  ! ctl file name
  character (len=200), allocatable, dimension(:)      :: M_vars                      ! variables to ctl
  character (len=200),              dimension(10)     :: U_vars, V_vars, W_vars      ! variables to ctl
  integer                                             :: i_M, i_U, i_V, i_W          ! number of t/u/v/w variables
  character (len=200)                                 :: var_string                  ! tmp var string
  character (len=40)                                  :: tdef                        ! ctl tdef
  integer                                             :: ntimes                      ! number of times in input file
  integer                                             :: end_ctl_files               ! sometimes we will have 3, sometimes 4 .ctl files

  character (len=31), allocatable, dimension(:)       :: dnam                        ! name of netcdf DIMS
  integer,            allocatable, dimension(:)       :: dval                        ! value of netcdf DIMS
  character (len=40)                                  :: title                       ! global att from netcdf file
  character (len=1)                                   :: gridtype                    ! global att from netcdf file
  real                                                :: cen_lon, cen_lat            ! global att from netcdf file
  real                                                :: stand_lon                   ! global att from netcdf file
  real                                                :: truelat1, truelat2          ! global att from netcdf file
  real                                                :: dx, dy                      ! global att from netcdf file
  integer                                             :: map_proj                    ! global att from netcdf file

  character (len=80)                                  :: varnam                      ! var from netcdf file
  integer                                             :: idvar                       ! varnam id number     
  integer                                             :: ivtype                      ! varnam type          
  integer                                             :: idm                         ! varnam - number of dims
  integer                                             :: natt                        ! varnam attributes
  character (len=80)                                  :: description                 ! varnam description
  character (len=80)                                  :: units                       ! varnam units
  character (len=80)                                  :: varnam_small                ! lowecase of varnam - for ctl file
  character (len=80)                                  :: varout                      ! varnam + varnam_small - for ctl file

  integer                                             :: len_unt, len_title          ! length of character strings  
  integer                                             :: len_string, len_des         ! length of character strings  
  integer                                             :: len_var                     ! length of character strings  

  integer                                             :: dims(4), ishape(10)         ! netcdf array dims and shapes
  character,          allocatable, dimension(:,:,:,:) :: times                       ! Times array from netcdf file
  real,               allocatable, dimension(:,:,:,:) :: znu, znw                    ! arrays from netcdf file
  real,               allocatable, dimension(:,:,:,:) :: xlat_M, xlon_M              ! u-staggered lat/lon     
  real,               allocatable, dimension(:,:,:,:) :: xlat_U, xlon_U              ! u-staggered lat/lon     
  real,               allocatable, dimension(:,:,:,:) :: xlat_V, xlon_V              ! v-staggered lat/lon     
  real,                            dimension(4)       :: xlat_a, xlon_a              ! lat/lon corners
  real                                                :: lat_ll_u, lat_ll_v          ! lat/lon corners
  real                                                :: lat_ur_u, lat_ur_v          ! lat/lon corners
  real                                                :: lon_ll_u, lon_ll_v          ! lat/lon corners
  real                                                :: lon_ur_u, lon_ur_v          ! lat/lon corners
  integer                                             :: iweg, isng, ibtg            ! staggered dims in input_file
  real,                            dimension(16)      :: lats16, lons16              ! lat/lon corners
 
  real                                                :: xi, xj                      ! PS defs
  real                                                :: r, re                       ! PS defs
  real                                                :: ref_lat, ref_lon            ! PS defs
  real                                                :: rlat, rlong                 ! PS defs
  real                                                :: earthr, radpd               ! PS defs
  real                                                :: dx_ps                       ! PS defs
  integer                                             :: ipole                       ! PS defs

  integer                                             :: ilon                        ! flag indicating dateline in domain
  real                                                :: abslatmin, abslatmax        ! min/max lat - for Lambert def's
  real                                                :: abslonmin, abslonmax        ! min/max lon - for Lambert def's
  integer                                             :: ipoints, jpoints            ! points used for Lambert def's     
  real                                                :: dxll, slack                 ! Lambert area setup (xdef/ydef)    

  integer                                             :: itmp, i, k                  ! tmp values                   
  real                                                :: temp                        ! tmp values                   
  real,               allocatable, dimension(:,:,:,:) :: tmp_var                     ! tmp values            

  logical                                             :: mercator_defs               ! for better mercator projections
  logical                                             :: debug                       ! for debug



   write(*,*) "  "
   write(*,*) "================================================ "
   write(*,*) "WRFnc2ctl for ARW WRF files      "
   write(*,*) " Only works correctly for gradsnc from GrADS 1.9 "

  tdef = '1 linear 00z01jan2000  1hr'

! GET INPUT FILE AND OUTPUT CASE NAME FOR COMMAND LINE
  if (debug) write(*,*) "READ arguments from command line"
  call read_args(input_file,case,mercator_defs,debug)


!! OPEN THE INPUT FILE
   if (debug) write(*,*) "OPENING netcdf input file"           
   rcode = nf_open (input_file, NF_NOWRITE, cdfid)
   call handle_err ("Opening input netCDF file", rcode)
   rcode = nf_get_att_text(cdfid, nf_global, 'GRIDTYPE', gridtype)
   if ( trim(gridtype) .ne. 'C' ) then
      write(*,*) 'Can only deal with ARW data for now'
      STOP
   endif


!! GET BASIC INFORMTION ABOUT THE FILE
   if (debug) write(*,*) "READ global attributes from netcdf file"
   stand_lon = -99999.99   !!! backward compatibility
   rcode = nf_get_att_text(cdfid, nf_global, 'TITLE',     title)
   rcode = nf_get_att_real(cdfid, nf_global, 'DX',        dx)
   rcode = nf_get_att_real(cdfid, nf_global, 'DY',        dy)
   rcode = nf_get_att_real(cdfid, nf_global, 'CEN_LAT',   cen_lat)
   rcode = nf_get_att_real(cdfid, nf_global, 'CEN_LON',   cen_lon)
   rcode = nf_get_att_real(cdfid, nf_global, 'STAND_LON', stand_lon)
   rcode = nf_get_att_real(cdfid, nf_global, 'TRUELAT1',  truelat1)
   rcode = nf_get_att_real(cdfid, nf_global, 'TRUELAT2',  truelat2)
   rcode = nf_get_att_int (cdfid, nf_global, 'MAP_PROJ',  map_proj)
   if (stand_lon == -99999.99 ) stand_lon = cen_lat    !!! dealing with an old WRF file

   if (debug) write(*,*) "READ dims from netcdf file"
   rcode = nf_inq(cdfid, nDims, nVars, nAtts, unlimDimID)
   allocate (M_vars(nVars))
   allocate (dval(nDims))
   allocate (dnam(nDims))
   iweg = 1  
   isng = 1  
   ibtg = 1  
   do i = 1,nDims
     rcode = nf_inq_dim(cdfid, i, dnam(i), dval(i))
     if ( dnam(i)(1:9)  == 'west_east'          ) iweg = max(iweg,dval(i))
     if ( dnam(i)(1:11) == 'south_north'        ) isng = max(isng,dval(i))

     if ( dnam(i)(1:10) == 'bottom_top'         ) ibtg = max(ibtg,dval(i))
     if ( trim(dnam(i)) == 'num_metgrid_levels' ) ibtg = max(ibtg,dval(i))
     if ( dnam(i)(1:4)  == 'land'               ) ibtg = max(ibtg,dval(i))
     if ( dnam(i)(1:4)  == 'soil'               ) ibtg = max(ibtg,dval(i))
     if ( dnam(i)(1:5)  == 'month'              ) ibtg = max(ibtg,dval(i))
     if ( dnam(i)(1:5)  == 'z-dim'              ) ibtg = max(ibtg,dval(i))

     if ( dnam(i)(1:15) == 'bottom_top_stag'    ) have_vert_stag = .TRUE.   
   enddo


!! OPEN THE OUTPUT FILES
   write(*,*) 
   write(*,*) "CREATING .ctl files for you case: ", case
   write(*,*) 
   ctl_M = trim(case)//"_M.ctl"
   ctl_U = trim(case)//"_U.ctl"
   ctl_V = trim(case)//"_V.ctl"
   ctl_W = trim(case)//"_W.ctl"
   open (10, file=ctl_M)
   open (11, file=ctl_U)
   open (12, file=ctl_V)
   if (have_vert_stag) open (13, file=ctl_W)


  lat_ll_u = -999.99
  lat_ll_v = -999.99
  lat_ur_u = -999.99
  lat_ur_v = -999.99
  lon_ll_u = -999.99
  lon_ll_v = -999.99
  lon_ur_u = -999.99
  lon_ur_v = -999.99

!===========================================================================================

      i_U = 0
      i_V = 0
      i_W = 0
      i_M = 0

      DO idvar = 1,nVars        !! LOOP THROUGH ALL VARIABLES IN FILE
        rcode = nf_inq_var(cdfid, idvar, varnam, ivtype, idm, ishape, natt)
        call handle_err ("ERROR reading variable", rcode)
        if (debug) write(*,*) 
        if (debug) write(*,*) "Variable:",idvar,"out of",nVars
        if (debug) write(*,*) "DEALING with variable: ", trim(varnam)
        if (.not. debug ) write(*,'("   Variable ",i2," :  ",A10,$)') idvar,varnam
        if (.not. debug .and. idm .gt. 2 ) write(*,*) " (*)"
        if (.not. debug .and. idm .le. 2 ) write(*,*) "    "

!! GET THE DIMS FOR INPUT AND OUTPUT FROM THE SHAPE
        dims  = 1
        do i = 1,idm
          dims(i)  = dval(ishape(i))
        enddo
        if (debug) write(*,*) " DIMS: ", dims


!! GET THE UNITS AND DESCRIPTION OF THE FIELD
        units = "                                                                                "
        description = "                                                                                "
        rcode = NF_GET_ATT_TEXT(cdfid, idvar, "units", units )
        rcode = NF_GET_ATT_TEXT(cdfid, idvar, "description", description )
        len_var = len_trim(varnam)
        len_des = len_trim(description)
        len_unt = len_trim(units)
        if (debug) write(*,*) " DESCRIPTION: ", description(1:len_des)
        if (debug) write(*,*) " UNITS: ", units(1:len_unt)

!! GET THE LOWER CASE OF EACH varnam
        varnam_small = " " 
        do i = 1,len_var
          if (varnam(i:i) .ge. 'A' .and. varnam(i:i) .le. 'Z') then
            itmp = ichar(varnam(i:i)) + 32
            varnam_small(i:i) = achar(itmp)
          else
            varnam_small(i:i) = varnam(i:i)
          endif
        enddo
        write(varout,*) varnam(1:len_var)//"=>"//varnam_small(1:len_var)
        len_var = max(22,len_trim(varout))

!! BUILD THE CTL var LIST
        IF     ( idm == 4 ) THEN
          write(var_string,'(A,i4,"  t,z,y,x  ",A,"  (",A,")")') varout(1:len_var), dims(3), description(1:len_des), units(1:len_unt)
        ELSEIF ( idm == 3 ) THEN
          write(var_string,'(A,i4,"  t,y,x    ",A,"  (",A,")")') varout(1:len_var), 0, description(1:len_des), units(1:len_unt)
        ENDIF
        IF     ( idm .gt. 2 ) THEN
          len_string = len_trim(var_string)
          if     ( dims(1) == iweg ) then
               i_U = i_U + 1
               U_vars(i_U) = var_string(1:len_string)
          elseif ( dims(2) == isng ) then
               i_V = i_V + 1
               V_vars(i_V) = var_string(1:len_string)
          elseif ( dims(3) == ibtg .and. have_vert_stag) then
               i_W = i_W + 1
               W_vars(i_W) = var_string(1:len_string)
          else
               i_M = i_M + 1
               M_vars(i_M) = var_string(1:len_string)
          endif
          if (debug) write(*,*) " CTL: ",var_string(1:len_string)
        ENDIF


!!!!
!!!! WE NEED KEEP SOME EXTRA INFO BEFORE READING THE NEXT VARIABLE
!!!!


!! GET THE NUMBER OF TIMES IN FILE AND BUILD tdef
        IF (varnam == 'Times' ) THEN
           allocate (times(dims(1), dims(2), dims(3), dims(4)))
           rcode = nf_get_var_text(cdfid, idvar, times)
           ntimes = dims(2)
           call time_calc( times, ntimes, tdef, debug )
        ENDIF

!! GET VERTICAL INFORMATION FOR zdef, AND xlon/xlat FOR pdef
        IF (varnam == 'ZNU' ) THEN
           allocate (znu(dims(1), dims(2), dims(3), dims(4)))
           rcode = nf_get_var_real(cdfid, idvar, znu)
        ENDIF
        IF (varnam == 'ZNW' ) THEN
           allocate (znw(dims(1), dims(2), dims(3), dims(4)))
           rcode = nf_get_var_real(cdfid, idvar, znw)
        ENDIF
        IF (varnam == 'XLAT'  ) THEN
           allocate (xlat_M(dims(1), dims(2), dims(3), dims(4)))
           rcode = nf_get_var_real(cdfid, idvar, xlat_M)
        ENDIF
        IF (varnam == 'XLAT_M'  ) THEN
           allocate (xlat_M(dims(1), dims(2), dims(3), dims(4)))
           rcode = nf_get_var_real(cdfid, idvar, xlat_M)
        ENDIF
        IF (varnam == 'XLAT_U'  ) THEN
           allocate (xlat_U(dims(1), dims(2), dims(3), dims(4)))
           rcode = nf_get_var_real(cdfid, idvar, xlat_U)
        ENDIF
        IF (varnam == 'XLAT_V'  ) THEN
           allocate (xlat_V(dims(1), dims(2), dims(3), dims(4)))
           rcode = nf_get_var_real(cdfid, idvar, xlat_V)
        ENDIF
        IF (varnam == 'XLONG' ) THEN
           allocate (xlon_M(dims(1), dims(2), dims(3), dims(4)))
           rcode = nf_get_var_real(cdfid, idvar, xlon_M)
        ENDIF
        IF (varnam == 'XLONG_M' ) THEN
           allocate (xlon_M(dims(1), dims(2), dims(3), dims(4)))
           rcode = nf_get_var_real(cdfid, idvar, xlon_M)
        ENDIF
        IF (varnam == 'XLONG_U'  ) THEN
           allocate (xlon_U(dims(1), dims(2), dims(3), dims(4)))
           rcode = nf_get_var_real(cdfid, idvar, xlon_U)
        ENDIF
        IF (varnam == 'XLONG_V' ) THEN
           allocate (xlon_V(dims(1), dims(2), dims(3), dims(4)))
           rcode = nf_get_var_real(cdfid, idvar, xlon_V)
        ENDIF

!! GET SOME CORNER INFORMATION
        IF (varnam == 'LAT_LL_U' ) THEN
           allocate (tmp_var(dims(1), dims(2), dims(3), dims(4)))
           rcode = nf_get_var_real(cdfid, idvar, tmp_var)
           lat_ll_u = tmp_var(1,1,1,1)
           deallocate (tmp_var)
        ENDIF
        IF (varnam == 'LAT_UR_U' ) THEN
           allocate (tmp_var(dims(1), dims(2), dims(3), dims(4)))
           rcode = nf_get_var_real(cdfid, idvar, tmp_var)
           lat_ur_u = tmp_var(1,1,1,1)
           deallocate (tmp_var)
        ENDIF
        IF (varnam == 'LAT_LL_V' ) THEN
           allocate (tmp_var(dims(1), dims(2), dims(3), dims(4)))
           rcode = nf_get_var_real(cdfid, idvar, tmp_var)
           lat_ll_v = tmp_var(1,1,1,1)
           deallocate (tmp_var)
        ENDIF
        IF (varnam == 'LAT_UR_V' ) THEN
           allocate (tmp_var(dims(1), dims(2), dims(3), dims(4)))
           rcode = nf_get_var_real(cdfid, idvar, tmp_var)
           lat_ur_v = tmp_var(1,1,1,1)
           deallocate (tmp_var)
        ENDIF

        IF (varnam == 'LON_LL_U' ) THEN
           allocate (tmp_var(dims(1), dims(2), dims(3), dims(4)))
           rcode = nf_get_var_real(cdfid, idvar, tmp_var)
           lon_ll_u = tmp_var(1,1,1,1)
           deallocate (tmp_var)
        ENDIF
        IF (varnam == 'LON_UR_U' ) THEN
           allocate (tmp_var(dims(1), dims(2), dims(3), dims(4)))
           rcode = nf_get_var_real(cdfid, idvar, tmp_var)
           lon_ur_u = tmp_var(1,1,1,1)
           deallocate (tmp_var)
        ENDIF
        IF (varnam == 'LON_LL_V' ) THEN
           allocate (tmp_var(dims(1), dims(2), dims(3), dims(4)))
           rcode = nf_get_var_real(cdfid, idvar, tmp_var)
           lon_ll_v = tmp_var(1,1,1,1)
           deallocate (tmp_var)
        ENDIF
        IF (varnam == 'LON_UR_V' ) THEN
           allocate (tmp_var(dims(1), dims(2), dims(3), dims(4)))
           rcode = nf_get_var_real(cdfid, idvar, tmp_var)
           lon_ur_v = tmp_var(1,1,1,1)
           deallocate (tmp_var)
        ENDIF


     
      ENDDO       !! END OF VAR LOOP
      write(*,*) " "
      write(*,*) "      (*) = variables that will be available in CTL files"
      write(*,*) " "
      write(*,*) "DONE reading variables"
      if (debug) write(*,*) " "
!===========================================================================================
 
!! CREATE THE CTL FILES

    
   if (debug) write(*,*) "START writing to .ctl files"
   len_title = len_trim(title)
   end_ctl_files = 12
   if (have_vert_stag) end_ctl_files = 13
   do i = 10,end_ctl_files
      write(i,'("dset ^",A)') trim(input_file)
      write(i,'("dtype netcdf")') 
      write(i, '("undef 1.e37")')
      write(i,'("title ",A)') title(1:len_title)
   enddo


   
   IF (map_proj == 1) THEN                                     !! Lambert Projection
     if (debug) write(*,*) "We have Lambert Projection data"
      !PDEF for M
      write(10,'("pdef ",i4," ",i3," lcc ",f7.3," ",f8.3," ",     &
&          f8.3," ",f8.3," ",f9.5," ",f9.5," ",f10.5," ",         &
&          f10.3," ",f10.3)')                                     &
           iweg-1,isng-1,cen_lat,cen_lon,iweg/2.,isng/2.,         &
           truelat1,truelat2,stand_lon,dx,dy
      !PDEF for U
      write(11,'("pdef ",i4," ",i3," lcc ",f7.3," ",f8.3," ",     &
&          f8.3," ",f8.3," ",f9.5," ",f9.5," ",f10.5," ",         &
&          f10.3," ",f10.3)')                                     &
           iweg,isng-1,cen_lat,cen_lon,(iweg+1.)/2.,isng/2.,      &
           truelat1,truelat2,stand_lon,dx,dy
      !PDEF for V
      write(12,'("pdef ",i4," ",i3," lcc ",f7.3," ",f8.3," ",     &
&          f8.3," ",f8.3," ",f9.5," ",f9.5," ",f10.5," ",         &
&          f10.3," ",f10.3)')                                     &
           iweg-1,isng,cen_lat,cen_lon,iweg/2.,(isng+1.)/2.,      &
           truelat1,truelat2,stand_lon,dx,dy
      if (have_vert_stag) then
       !PDEF for W 
       write(13,'("pdef ",i4," ",i3," lcc ",f7.3," ",f8.3," ",    &
&           f8.3," ",f8.3," ",f9.5," ",f9.5," ",f10.5," ",        &
&           f10.3," ",f10.3)')                                    &
            iweg-1,isng-1,cen_lat,cen_lon,iweg/2.,isng/2.,        &
            truelat1,truelat2,stand_lon,dx,dy
      endif

      !make sure truelat1 is the larger number
       if (truelat1 < truelat2) then
          temp = truelat1
          truelat1 = truelat2
          truelat2 = temp
       endif

       xlon_a(1) = xlon_M(1,1,1,1)
       xlon_a(2) = xlon_M(1,isng-1,1,1)
       xlon_a(3) = xlon_M(iweg-1,1,1,1)
       xlon_a(4) = xlon_M(iweg-1,isng-1,1,1)

       !check for dateline
       ilon = 0
       if ( abs(xlon_a(1) - xlon_a(3)) .GT. 180. ) ilon = 1
       if ( abs(xlon_a(2) - xlon_a(4)) .GT. 180. ) ilon = 1

       abslatmin = minval(xlat_M)
       abslatmax = maxval(xlat_M)
       abslonmin =  99999.
       abslonmax = -99999.

       do i=1,4
         IF ( xlon_a(i) .lt. 0.0 .AND. ilon .eq. 1 ) THEN
           abslonmin=min(abslonmin,360.+xlon_a(i))
           abslonmax=max(abslonmax,360.+xlon_a(i))
         ELSE
           abslonmin=min(abslonmin,xlon_a(i))
           abslonmax=max(abslonmax,xlon_a(i))
         ENDIF
       enddo

       dxll = (dx/1000.)/111./2.
       slack = ((dx/1000.)*3.)/100.
       ipoints = int(((abslonmax-abslonmin)+(3.*slack))/dxll)
       jpoints = int(((abslatmax-abslatmin)+(3.*slack))/dxll)

       do i = 10,end_ctl_files
         write(i,'("xdef ",i4," linear ",f10.5," ",f12.8)') ipoints, &
                     (abslonmin-1.5*slack),dxll
         write(i,'("ydef ",i4," linear ",f10.5," ",f12.8)') jpoints, &
                     (abslatmin-1.5*slack),dxll
       enddo
   ENDIF   

   IF (map_proj == 2) THEN                                     !! Polar Stereo Projection
     if (debug) write(*,*) "We have Polar Stereo Projection data"

       ipole = 1
       if (truelat1 .lt. 0.0) ipole = -1
       radpd  = .01745329
       earthr = 6.3712E6
 
       dx_ps = dx/( (1.0+sin(ipole*TRUELAT1*radpd))/(1.0+sin(60*radpd)) ) !! true dx (meter) at 60degrees
       re    = ( earthr * (1.0 + sin(60*radpd)) ) / dx_ps          !!


       IF ( ipole == 1 ) THEN

         rlong  = (xlon_M(1,1,1,1) - stand_lon) * radpd
         rlat   = xlat_M(1,1,1,1) * radpd
         r    =  (re * cos(rlat)) / (1.0 + sin(rlat))
         xi   =  (1. - r * sin(rlong))
         xj   =  (1. + r * cos(rlong))

         !PDEF for M
         write(10,'("pdef ",i3," ",i3," nps ",f8.3," ",f8.3," ", &
&              f12.4," ",f12.7)') iweg-1, isng-1,                &
               xi, xj, stand_lon, dx_ps*0.001
         !PDEF for U
         write(11,'("pdef ",i3," ",i3," nps ",f8.3," ",f8.3," ", &
&              f12.4," ",f12.7)') iweg, isng-1,                  &
               xi, xj, stand_lon, dx_ps*0.001
         !PDEF for V
         write(12,'("pdef ",i3," ",i3," nps ",f8.3," ",f8.3," ", &
&              f12.4," ",f12.7)') iweg-1, isng,                  &
               xi, xj, stand_lon, dx_ps*0.001
         if (have_vert_stag) then
           !PDEF for W
           write(13,'("pdef ",i3," ",i3," nps ",f8.3," ",f8.3," ", &
&                f12.4," ",f12.7)') iweg-1, isng-1,                &
                 xi, xj, stand_lon, dx_ps*0.001
         endif  

       ELSE IF ( ipole == -1 ) THEN

         rlong  = (180.0 + xlon_M(1,1,1,1) - stand_lon) * radpd
         rlat   = ipole*xlat_M(1,1,1,1) * radpd
         r    =  (re * cos(rlat)) / (1.0 + sin(rlat))
         xi   =  (1. - ipole * r * sin(rlong))
         xj   =  (1. + r * cos(rlong))

         !PDEF for M
         write(10,'("pdef ",i3," ",i3," sps ",f8.3," ",f8.3," ", &
&              f12.4," ",f12.7)') iweg-1, isng-1,                &
               xi, xj, (180+stand_lon), -0.001*dx_ps
         !PDEF for U
         write(11,'("pdef ",i3," ",i3," sps ",f8.3," ",f8.3," ", &
&              f12.4," ",f12.7)') iweg, isng-1,                  &
               xi, xj, (180+stand_lon), -0.001*dx_ps
         !PDEF for V
         write(12,'("pdef ",i3," ",i3," sps ",f8.3," ",f8.3," ", &
&              f12.4," ",f12.7)') iweg-1, isng,                  &
               xi, xj, (180+stand_lon), -0.001*dx_ps
         if (have_vert_stag) then
         !PDEF for W
           write(13,'("pdef ",i3," ",i3," sps ",f8.3," ",f8.3," ", &
&                f12.4," ",f12.7)') iweg-1, isng-1,                &
                 xi, xj, (180+stand_lon), -0.001*dx_ps
         endif
 
       END IF


       abslonmin = minval(xlon_M)
       abslonmax = maxval(xlon_M)
       abslatmin = minval(xlat_M)
       abslatmax = maxval(xlat_M)

       dxll = (dx_ps/1000.)/111./2.
       ipoints = int(((abslonmax-abslonmin)+2.)/dxll)-2
       jpoints = int(((abslatmax-abslatmin)+2.)/dxll)-3

       ref_lon = abslonmin-1.
       IF ( abslonmax .ge. 175. .AND. abslonmax .le. 180.) THEN
         IF ( abslonmin .ge. -180. .AND. abslonmin .le. -175.) THEN
           ref_lon = 0.0
         END IF
       END IF

       ref_lat = abslatmin-1.
       IF ( ipole == -1 ) ref_lat = abslatmin


       do i = 10,end_ctl_files
         write(i,'("xdef ",i4," linear ",f6.1," ",f12.8)') ipoints, &
                     ref_lon,dxll
         write(i,'("ydef ",i4," linear ",f6.1," ",f12.8)') jpoints, &
                     ref_lat,dxll
       enddo

   ENDIF   

   IF (map_proj == 3) THEN                                     !! Mercator Projection
       if (debug) write(*,*) "We have Mercator Projection data"

       !check to see if we have the corner lat/lon
       if ( allocated(xlat_U) ) then
         lat_ll_u = xlat_U(1,1,1,1)
         lat_ur_u = xlat_U(iweg,isng-1,1,1)
       endif
       if ( allocated(xlat_V) ) then
         lat_ll_v = xlat_V(1,1,1,1)
         lat_ur_v = xlat_V(iweg-1,isng,1,1)
       endif
       if ( allocated(xlon_U) ) then
         lon_ll_u = xlon_U(1,1,1,1)
         lon_ur_u = xlon_U(iweg,isng-1,1,1)
       endif
       if ( allocated(xlon_V) ) then
         lon_ll_v = xlon_V(1,1,1,1)
         lon_ur_v = xlon_V(iweg-1,isng,1,1)
       endif

       if ( lat_ll_u==-999.99 .or. lat_ur_u==-999.99 .or. lat_ll_v==-999.99 .or. lat_ur_v==-999.99 .or. &
            lon_ll_u==-999.99 .or. lon_ur_u==-999.99 .or. lon_ll_v==-999.99 .or. lon_ur_v==-999.99 ) then ! try getting them from the global att
          if (debug) write(*,*) ' NO STAGGERED LAT/LON DATA AVAILBLE - try and get it from meta data '
          rcode = nf_get_att_real(cdfid, nf_global, 'corner_lons', lons16)
          if ( rcode == 0 ) then     
            rcode = nf_get_att_real(cdfid, nf_global, 'corner_lats', lats16)
            lat_ll_u = lats16( 5)
            lat_ur_u = lats16( 7)
            lat_ll_v = lats16( 9)
            lat_ur_v = lats16(11) 
            lon_ll_u = lons16( 5)
            lon_ur_u = lons16( 7)
            lon_ll_v = lons16( 9)
            lon_ur_v = lons16(11)
          else
             write(*,*) ' NO STAGGERED LAT/LON DATA AVAILBLE - FAKE IT'
            lat_ll_u = xlat_M(1,1,1,1)           - abs(xlat_M(2,1,1,1)-xlat_M(1,1,1,1))/2.0
            lat_ur_u = xlat_M(iweg-1,isng-1,1,1) + abs(xlat_M(2,1,1,1)-xlat_M(1,1,1,1))/2.0
            lat_ll_v = xlat_M(1,1,1,1)           - abs(xlat_M(1,2,1,1)-xlat_M(1,1,1,1))/2.0
            lat_ur_v = xlat_M(iweg-1,isng-1,1,1) + abs(xlat_M(1,2,1,1)-xlat_M(1,1,1,1))/2.0
            lon_ll_u = xlon_M(1,1,1,1)           - abs(xlon_M(2,1,1,1)-xlon_M(1,1,1,1))/2.0
            lon_ur_u = xlon_M(iweg-1,isng-1,1,1) + abs(xlon_M(2,1,1,1)-xlon_M(1,1,1,1))/2.0
            lon_ll_v = xlon_M(1,1,1,1)           - abs(xlon_M(1,2,1,1)-xlon_M(1,1,1,1))/2.0
            lon_ur_v = xlon_M(iweg-1,isng-1,1,1) + abs(xlon_M(1,2,1,1)-xlon_M(1,1,1,1))/2.0
          endif
       endif

       !check for dateline
       ilon = 0
       if ( abs(xlon_M(1,1,1,1)      - xlon_M(iweg-1,1,1,1))      .GT. 180. ) ilon = 1
       if ( abs(xlon_M(1,isng-1,1,1) - xlon_M(iweg-1,isng-1,1,1)) .GT. 180. ) ilon = 1

       IF ( ilon == 1 ) THEN

         !! For M
         WRITE(10,'("xdef ",i4," linear ",f9.4," ",f8.4)')   &
            iweg-1,xlon_M(1,1,1,1), &
            (abs(xlon_M(1,1,1,1)-(360.+xlon_M(iweg-1,isng-1,1,1)))/(iweg-2))
         !! For U
         WRITE(11,'("xdef ",i4," linear ",f9.4," ",f8.4)')   &
            iweg,lon_ll_u, &
            (abs(lon_ll_u-(360.+lon_ur_u))/(iweg-1))
         !! For V
         WRITE(12,'("xdef ",i4," linear ",f9.4," ",f8.4)')   &
            iweg-1,lon_ll_v, &
            (abs(lon_ll_v-(360.+lon_ur_v))/(iweg-2))
         if (have_vert_stag) then
           !! For W
           WRITE(13,'("xdef ",i4," linear ",f9.4," ",f8.4)')   &
              iweg-1,xlon_M(1,1,1,1), &
              (abs(xlon_M(1,1,1,1)-(360.+xlon_M(iweg-1,isng-1,1,1)))/(iweg-2))
         endif

       ELSE

         IF ( mercator_defs .AND. allocated(xlon_U) .AND. allocated(xlon_V) ) THEN
           WRITE(10,'("xdef ",i4," levels ")') iweg-1  !! M
           WRITE(11,'("xdef ",i4," levels ")') iweg    !! U
           WRITE(12,'("xdef ",i4," levels ")') iweg-1  !! V
           if (have_vert_stag) WRITE(13,'("xdef ",i4," levels ")') iweg-1  !! W
           DO i = 1,iweg-1
              WRITE(10,*) xlon_M(i,1,1,1)
           END DO
           DO i = 1,iweg
              WRITE(11,*) xlon_U(i,1,1,1)
           END DO
           DO i = 1,iweg-1
              WRITE(12,*) xlon_V(i,1,1,1)
           END DO
           if (have_vert_stag) then
             DO i = 1,iweg-1
                WRITE(13,*) xlon_M(i,1,1,1)
             END DO
           endif  
         ELSE
           !! For M
           WRITE(10,'("xdef ",i4," linear ",f9.4," ",f8.4)')   &
              iweg-1,xlon_M(1,1,1,1), &
              (abs(xlon_M(1,1,1,1)-(xlon_M(iweg-1,isng-1,1,1)))/(iweg-2))
           !! For U
           WRITE(11,'("xdef ",i4," linear ",f9.4," ",f8.4)')   &
              iweg,lon_ll_u, &
              (abs(lon_ll_u-(lon_ur_u))/(iweg-1))
           !! For V
           WRITE(12,'("xdef ",i4," linear ",f9.4," ",f8.4)')   &
              iweg-1,lon_ll_v, &
              (abs(lon_ll_v-(lon_ur_v))/(iweg-2))
           if (have_vert_stag) then
             !! For W
             WRITE(13,'("xdef ",i4," linear ",f9.4," ",f8.4)')   &
                iweg-1,xlon_M(1,1,1,1), &
                (abs(xlon_M(1,1,1,1)-(xlon_M(iweg-1,isng-1,1,1)))/(iweg-2))
           endif
         END IF

       ENDIF

       IF ( mercator_defs .AND. allocated(xlat_U) .AND. allocated(xlat_V) ) THEN
          WRITE(10,'("ydef ",i4," levels ")') isng-1  !! M
          WRITE(11,'("ydef ",i4," levels ")') isng-1  !! U
          WRITE(12,'("ydef ",i4," levels ")') isng    !! V
          if (have_vert_stag) WRITE(13,'("ydef ",i4," levels ")') isng-1  !! W
          DO i = 1,isng-1
            WRITE(10,*) xlat_M(1,i,1,1)
          END DO
          DO i = 1,isng-1
            WRITE(11,*) xlat_U(1,i,1,1)
          END DO
          DO i = 1,isng
            WRITE(12,*) xlat_V(1,i,1,1)
          END DO
          if (have_vert_stag) then
            DO i = 1,isng-1
              WRITE(13,*) xlat_M(1,i,1,1)
            END DO
          endif  
       ELSE
          !! For M
          WRITE(10,'("ydef ",i4," linear ",f9.4," ",f8.4)')   &
                    isng-1,xlat_M(1,1,1,1),(abs(xlat_M(1,1,1,1)-xlat_M(iweg-1,isng-1,1,1))/(isng-2))
          !! For U
          WRITE(11,'("ydef ",i4," linear ",f9.4," ",f8.4)')   &
                    isng-1,lat_ll_u,(abs(lat_ll_u-lat_ur_u)/(isng-2))
          !! For V
          WRITE(12,'("ydef ",i4," linear ",f9.4," ",f8.4)')   &
                    isng-1,lat_ll_v,(abs(lat_ll_v-lat_ur_v)/(isng-2))
          if (have_vert_stag) then
            !! For W
            WRITE(13,'("ydef ",i4," linear ",f9.4," ",f8.4)')   &
                      isng-1,xlat_M(1,1,1,1),(abs(xlat_M(1,1,1,1)-xlat_M(iweg-1,isng-1,1,1))/(isng-2))
          endif
       ENDIF

   ENDIF



   if (have_vert_stag) then
     !FOR M
       write(10,'("zdef  ",i3, " levels  ")') ibtg-1
       do k = 1,ibtg-1
          write(10,'(f10.5)') znu(k,1,1,1)
       enddo
     !FOR U
       write(11,'("zdef  ",i3, " levels  ")') ibtg-1
       do k = 1,ibtg-1
          write(11,'(f10.5)') znu(k,1,1,1)
       enddo
     !FOR V
       write(12,'("zdef  ",i3, " levels  ")') ibtg-1
       do k = 1,ibtg-1
          write(12,'(f10.5)') znu(k,1,1,1)
       enddo
     !FOR W
       write(13,'("zdef  ",i3, " levels  ")') ibtg
       do k = 1,ibtg
          write(13,'(f10.5)') znw(k,1,1,1)
       enddo
   else
     do i = 10,end_ctl_files
       write(i,'("zdef  ",i3," linear 1 1")') ibtg
     enddo
   endif


   !TDEF
   do i = 10,end_ctl_files
     write(i,'("tdef  ",A)') tdef
   enddo


   if (debug) write(*,*) "WRITING variables to .ctl files" 
   !VARS
   write(10,'("vars  ",i3)') i_M
   do i = 1,i_M
      write(10,'(A)') trim(M_vars(i))
   enddo
   write(11,'("vars  ",i3)') i_U
   do i = 1,i_U
      write(11,'(A)') trim(U_vars(i))
   enddo
   write(12,'("vars  ",i3)') i_V
   do i = 1,i_V
      write(12,'(A)') trim(V_vars(i))
   enddo
   if (have_vert_stag) then
     write(13,'("vars  ",i3)') i_W
     do i = 1,i_W
        write(13,'(A)') trim(W_vars(i))
     enddo
   endif


   do i = 10,end_ctl_files
      write(i,'("endvars")') 
   enddo

!===========================================================================================

  rcode = nf_close(cdfid)  

  write(*,*) "Successful "
  write(*,*) "================================================ "
  print*,"  "

  end program WRFnc2ctl
!------------------------------------------------------------------------------

  subroutine time_calc( times, ntimes, tdef, debug )

  character (len=19)             :: times(ntimes), time
  character (len=3)              :: mth, t_ind
  character (len=40)             :: tdef
  integer                        :: year,month,day,hh1,hh2,mn1,mn2,seconds,hourint,minsint,tdiff
  integer                        :: ntimes
  logical                        :: debug


!! Time comes in as YYYY-MM-DD_HH:MM:SS
!!                  1234 67 90 23 45 89
   time = times(1)
   read(time(1:4),*)   year
   read(time(6:7),*)   month
   read(time(9:10),*)  day
   read(time(12:13),*) hh1
   read(time(15:16),*) mn1

    if (month == 0) return      
    if (month == 1) mth = 'jan'
    if (month == 2) mth = 'feb'
    if (month == 3) mth = 'mar'
    if (month == 4) mth = 'apr'
    if (month == 5) mth = 'may'
    if (month == 6) mth = 'jun'
    if (month == 7) mth = 'jul'
    if (month == 8) mth = 'aug'
    if (month == 9) mth = 'sep'
    if (month ==10) mth = 'oct'
    if (month ==11) mth = 'nov'
    if (month ==12) mth = 'dec'

   if (debug) write(*,*) "Start date is: ",year,"-",month,"-",day,"_",hh1,":",mn1

   if (ntimes .ge. 2) then
     time = times(2)
     read(time(12:13),*) hh2
     read(time(15:16),*) mn2
     hourint = abs(hh2-hh1)
     minsint = abs(mn2-mn1)
     if (hourint == 0 ) then
       tdiff = minsint                  
       t_ind = 'mn'                       
     else
       tdiff = hourint                 
       t_ind = 'hr'                       
     endif
   else
     tdiff = 1                 
     t_ind = 'hr'                       
   endif

    tdef = '                               '
    write (tdef,'(i4," linear ",i2,"Z",i2,A,i4,"  ",i3,A)') ntimes, hh1, day, mth, year, tdiff, t_ind
    if (debug) write(*,*) "TDEF is set to: ",tdef

  end subroutine time_calc

!------------------------------------------------------------------------------

  subroutine help_info

  print*," "
  print*," WRFnc2ctl   -i wrf_netcdf_input_file   -o ctl_output_name"
  print*," "
  print*," Current options available are:"
  print*," -help     : Print this information"                           
  print*," -h        : Print this information"                           
  print*," -i        : WRF input file (netcdf format)"                  
  print*," -o        : ctl output name - default is ARWout"                                 
  print*," -mercator : Use if mercator projection is stretched"         
  print*," -debug    : Print some debug information"                    

  STOP

  end subroutine help_info

!------------------------------------------------------------------------------
  subroutine read_args(input_file,case,mercator_defs,debug) 

  implicit none
  character (len=200)   :: input_file                        
  character (len=80)    :: case                        
  logical               :: mercator_defs, debug

  integer, external     :: iargc
  integer               :: numarg, i
  character (len=80)    :: dummy


! set up some defaults first
  input_file = " "
  case       = "ARWout"
  numarg     = iargc()
  i          = 1
  mercator_defs = .FALSE.
  debug = .FALSE.


  if (numarg == 0) call help_info

  do while (i <= numarg)
    call getarg(i,dummy)

    if (dummy(1:1) == "-") then    ! We have an option, else it is the filename

      SELECTCASE (trim(dummy))
          CASE ("-help")
               call help_info
          CASE ("-h")
               call help_info
          CASE ("-i")
               i = i+1
               call getarg(i,dummy)         ! read input_file name
               input_file = dummy
          CASE ("-o")
               i = i+1
               call getarg(i,dummy)         ! read case name
               case = dummy
          CASE ("-mercator")
               mercator_defs = .TRUE.
          CASE ("-debug")
               debug = .TRUE.
          CASE DEFAULT
               call help_info
      END SELECT
    else
      input_file = dummy                     ! if option -i was not used for input file
    endif

      i = i+1

  enddo

  if (input_file == " ") call help_info


  end subroutine read_args

!------------------------------------------------------------------------------
      subroutine handle_err(message,nf_status)
      include "netcdf.inc"
      integer                 :: nf_status
      character (len=80)      :: message
      if (nf_status .ne. nf_noerr) then
         write(*,*)  'ERROR: ', trim(message)
         STOP 
      endif
      end subroutine handle_err
!------------------------------------------------------------------------------

