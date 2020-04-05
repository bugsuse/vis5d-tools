MODULE input_module

   USE gridinfo_module
   USE misc_definitions_module
   USE module_debug
   USE module_model_basics
   USE queue_module
#ifdef IO_BINARY
   USE module_internal_header_util
#endif
 
 
   ! WRF I/O API related variables
   integer                             :: handle
 
   type (queue)                        :: unit_desc
   integer                             :: num_calls, iatts
   character (len=200), dimension(200) :: catts
   integer                             :: bucket_mm, bucket_J

   CONTAINS
 
 
   SUBROUTINE input_init (file_number, istatus)
 
      implicit none

#include "wrf_io_flags.h"
#include "wrf_status_codes.h"
  
      ! Arguments
      integer, intent(in)  :: file_number
      integer, intent(out) :: istatus
  
      ! Local variables
      character (len=128)  :: input_fname
  
      IF ( debug_level .ge. 500 ) print*,"DEBUG: SUBROUTINE input_init"
      istatus = 0
      CALL arw_ioinit(istatus)
     
      istatus = 0
      input_fname = ' '
      input_fname = trim(input_file_names(file_number))
      CALL arw_open_for_read(input_fname, handle, istatus)
     
      CALL q_init(unit_desc)
  
      num_calls = 0
 
   END SUBROUTINE input_init
 
 
   SUBROUTINE read_next_field (domain_start, domain_end, &
                               cname, cunits, cdesc, memorder, &
                               stagger, dimnames, real_array, valid_date, istatus)
 
      implicit none

#include "wrf_io_flags.h"
#include "wrf_status_codes.h"
  
      ! Arguments
      integer, dimension(3)             :: domain_start, domain_end
      real, pointer, dimension(:,:,:)   :: real_array
      character (len=*), intent(out)    :: cname, memorder, stagger, cunits, cdesc
      character (len=128), dimension(3) :: dimnames
      integer, intent(inout)            :: istatus
  
      ! Local variables
      integer                           :: ndim, wrftype
      real, pointer, dimension(:,:,:)   :: real_domain
      character (len=19)                :: valid_date
      type (q_data)                     :: qd
  

      IF ( debug_level .ge. 500 ) print*,"DEBUG: SUBROUTINE read_next_field"
      
      num_calls = num_calls + 1


      CALL arw_get_next_var(handle, cname, istatus) 
      if (istatus /= 0) return
 
      istatus = 0
      domain_start = 1
      domain_end   = 1
      CALL arw_get_var_info(handle, cname, ndim, memorder, stagger, cdesc, cunits, &
                            domain_start, domain_end, wrftype, istatus)
      if (istatus /= 0) return
      if (ndim == 0) return
      if (ndim /= 3) then
         domain_start(3) = 1
         domain_end(3) = 1
      end if

      IF ( debug_level .ge. 300 ) THEN
        print*,"DEBUG: Read Variable " ,trim(cname)
      ENDIF

      IF ( ASSOCIATED(real_array) ) THEN
        DEALLOCATE(real_array)
        NULLIFY(real_array)
      ENDIF
      IF ( ASSOCIATED(real_domain) ) NULLIFY(real_domain)
      !!IF ( ASSOCIATED(real_domain) ) DEALLOCATE(real_domain)
      ALLOCATE(real_domain(domain_end(1), domain_end(2), domain_end(3)))
      CALL arw_read_field(handle, valid_date, cname, real_domain, wrftype, &
                          memorder, stagger, dimnames, domain_start, domain_end, istatus)
      IF (istatus /= 0) THEN
        IF ( debug_level .ge. 300 ) THEN
          print*,"       Variable not in file - return"
        ENDIF
        RETURN
      ENDIF

     
         if (io_form_input == BINARY) then
            qd = q_remove(unit_desc)
            cunits  = qd%units
            cdesc   = qd%description
         else
        
#ifdef IO_NETCDF
            if (io_form_input == NETCDF) then
               CALL ext_ncd_get_var_ti_char(handle, 'units', cname, cunits, istatus)
               if (iachar(cunits(1:1)) == 0 ) cunits = "-"
               CALL ext_ncd_get_var_ti_char(handle, 'description', cname, cdesc, istatus)
            end if
#endif
!!#ifdef IO_GRIB1
!!          if (io_form_input == GRIB1) then
!!             CALL ext_gr1_get_var_ti_char(handle, 'units', cname, cunits, istatus)
!!             CALL ext_gr1_get_var_ti_char(handle, 'description', cname, cdesc, istatus)
!!          end if
!!#endif

         end if

      IF (iachar(cdesc(1:1)) == 0 ) cdesc = cname   !!! Because .ctl must have a description

      real_array => real_domain

      IF ( debug_level .ge. 500 ) THEN
        print*,"DEBUG: Variable Information"
        print*,"       ndim     = ",ndim
        print*,"       memorder = ",trim(memorder)
        print*,"       stagger  = ",trim(stagger)
        print*,"       cdesc    = ",trim(cdesc)
        print*,"       cunits   = ",trim(cunits)
        print*,"       dims     = ",domain_end
        print*,"       type     = ",wrftype
        print*,"       min/max  = ",minval(real_array),"   ",maxval(real_array)
      ENDIF
      IF ( debug_level .ge. 900 ) THEN
        IF ( ndim == 1 ) &
        print*,"       sample vert. levels = ",real_array
        IF ( ndim == 3 ) &
        print*,"       sample vert. levels = ",real_array(domain_end(1)/2, domain_end(2)/2,:)
      ENDIF

 
   END SUBROUTINE read_next_field
 
   SUBROUTINE read_spec_field (domain_start, domain_end, cname, wrftype, memorder, &
                               stagger, dimnames, real_array, valid_date, istatus)
 
      implicit none

#include "wrf_io_flags.h"
#include "wrf_status_codes.h"
  
      ! Arguments
      integer, dimension(3)             :: domain_start, domain_end
      real, pointer, dimension(:,:,:)   :: real_array
      character (len=*)                 :: cname, memorder, stagger
      character (len=128)               :: cunits, cdesc
      character (len=128), dimension(3) :: dimnames
      integer, intent(inout)            :: istatus
  
      ! Local variables
      integer                           :: ndim, wrftype
      real, pointer, dimension(:,:,:)   :: real_domain
      character (len=19)                :: valid_date
      type (q_data)                     :: qd
  

      istatus = 0
      domain_start = 1
      domain_end   = 1
      CALL arw_get_var_info(handle, cname, ndim, memorder, stagger, cdesc, cunits, &
                            domain_start, domain_end, wrftype, istatus)
      if (istatus /= 0) return
      if (ndim == 0 .AND. trim(cname) /= 'P_TOP') return
      if (ndim /= 3) then
         domain_start(3) = 1
         domain_end(3) = 1
      end if

      IF ( debug_level .ge. 300 ) THEN
        print*,"DEBUG: Read Variable " ,trim(cname)
      ENDIF
   
      IF ( ASSOCIATED(real_array) ) THEN
        DEALLOCATE(real_array)
        NULLIFY(real_array)
      ENDIF
      IF ( ASSOCIATED(real_domain) ) NULLIFY(real_domain)
      !!IF ( ASSOCIATED(real_domain) ) DEALLOCATE(real_domain)
      ALLOCATE(real_domain(domain_end(1), domain_end(2), domain_end(3)))
      CALL arw_read_field(handle, valid_date, cname, real_domain, wrftype, &
                          memorder, stagger, dimnames, domain_start, domain_end, istatus)


      real_array => real_domain
 
   END SUBROUTINE read_spec_field
 
   
   SUBROUTINE read_global_attrs ()
 
      implicit none
  
      ! Local variables
      integer                          :: dyn_opt
      integer                          :: outcount, istatus
      character (len=19)               :: start_date
      character (len=128)              :: grid_type
      character (len=128)              :: cunits, cdesc, cstagger, mminlu
      type (q_data)                    :: qd
      real                             :: dum_r
      integer                          :: dum_i
  
      iatts = 0
      
      !! Any unknown program (including WRFSI) will be 0
      iprogram = 0
      CALL arw_get_gbl_att_char(handle, 'TITLE', title, istatus)
      IF ( INDEX(title,'OUTPUT FROM GEOGRID') /= 0 ) iprogram = 1 !! geogrid output
      IF ( INDEX(title,'OUTPUT FROM GRIDGEN') /= 0 ) iprogram = 1 !! old geogrid output
      IF ( INDEX(title,'OUTPUT FROM METGRID') /= 0 ) iprogram = 3 !! metgrid output
      IF ( INDEX(title,'OUTPUT FROM OBSGRID') /= 0 ) iprogram = 3 !! obsgrid output
      IF ( INDEX(title,'OUTPUT FROM REAL_EM') /= 0 ) iprogram = 6 !! real.exe output
      IF ( INDEX(title,'OUTPUT FROM WRF') /= 0 )     iprogram = 8 !! wrf.exe output
      IF ( iprogram == 0 ) THEN
         print*," "
         print*,"  WARNING --- I do not recognize this data."
         print*,"             ",trim(title)
         print*,"              Will make an attempt to read it."
         print*," "
      END IF

      CALL arw_get_gbl_att_char(handle, 'SIMULATION_START_DATE', start_date, istatus)
      CALL arw_get_gbl_att_char(handle, 'GRIDTYPE', grid_type, istatus)
      CALL arw_get_gbl_att_char(handle, 'MMINLU', mminlu, istatus)
      
      !! Make sure we are working with the unstaggered values here
      CALL arw_get_gbl_att_int_sca(handle, 'WEST-EAST_GRID_DIMENSION', west_east_dim, &
           1, outcount, istatus)
           west_east_dim = west_east_dim - 1
      CALL arw_get_gbl_att_int_sca(handle, 'SOUTH-NORTH_GRID_DIMENSION', south_north_dim, &
           1, outcount, istatus)
           south_north_dim = south_north_dim - 1
      CALL arw_get_gbl_att_int_sca(handle, 'BOTTOM-TOP_GRID_DIMENSION', bottom_top_dim, &
           1, outcount, istatus)
           IF ( iprogram .le. 1 ) bottom_top_dim = 24  !!!  to make room for any 3D datasets
           IF ( iprogram .ge. 6 ) bottom_top_dim = bottom_top_dim - 1

      CALL arw_get_gbl_att_int_sca(handle, 'DYN_OPT', dyn_opt, 1, outcount, istatus)
      CALL arw_get_gbl_att_int_sca(handle, 'MAP_PROJ', map_proj, 1, outcount, istatus)
      CALL arw_get_gbl_att_real_sca(handle, 'DX', dx, 1, outcount, istatus)
      CALL arw_get_gbl_att_real_sca(handle, 'DY', dy, 1, outcount, istatus)
      CALL arw_get_gbl_att_real_sca(handle, 'CEN_LAT', cen_lat, 1, outcount, istatus)
      CALL arw_get_gbl_att_real_sca(handle, 'CEN_LON', cen_lon, 1, outcount, istatus)
      CALL arw_get_gbl_att_real_sca(handle, 'TRUELAT1', truelat1, 1, outcount, istatus)
      CALL arw_get_gbl_att_real_sca(handle, 'TRUELAT2', truelat2, 1, outcount, istatus)
      CALL arw_get_gbl_att_real_sca(handle, 'MOAD_CEN_LAT', moad_cen_lat, 1, outcount, istatus)
      CALL arw_get_gbl_att_real_sca(handle, 'STAND_LON', stand_lon, 1, outcount, istatus)
      !!CALL arw_get_gbl_att_real_arr(handle, 'corner_lats', corner_lats, 16, outcount, istatus)
      !!CALL arw_get_gbl_att_real_arr(handle, 'corner_lons', corner_lons, 16, outcount, istatus)
      CALL arw_get_gbl_att_int_sca(handle, 'BUCKET_MM', bucket_mm, 1, outcount, istatus)
        IF ( istatus /= 0 ) bucket_mm = 0
        IF ( bucket_mm < 0 ) bucket_mm = 0
      CALL arw_get_gbl_att_int_sca(handle, 'BUCKET_J', bucket_J, 1, outcount, istatus)
        IF ( istatus /= 0 ) bucket_J = 0
        IF ( bucket_J < 0 ) bucket_J = 0 

      !! Just needed for meta data in the .ctl file
      !! No way of knowing all the attributes, so we just get some basic ones
      CALL arw_get_gbl_att_int_sca(handle, 'DYN_OPT', dum_i, 1, outcount, istatus)
      CALL arw_get_gbl_att_int_sca(handle, 'DIFF_OPT', dum_i, 1, outcount, istatus)
      CALL arw_get_gbl_att_int_sca(handle, 'KM_OPT', dum_i, 1, outcount, istatus)
      CALL arw_get_gbl_att_int_sca(handle, 'DAMP_OPT', dum_i, 1, outcount, istatus)
      CALL arw_get_gbl_att_real_sca(handle, 'KHDIF', dum_r, 1, outcount, istatus)
      CALL arw_get_gbl_att_real_sca(handle, 'KVDIF', dum_r, 1, outcount, istatus)
      CALL arw_get_gbl_att_int_sca(handle, 'MP_PHYSICS', dum_i, 1, outcount, istatus)
      CALL arw_get_gbl_att_int_sca(handle, 'RA_LW_PHYSICS', dum_i, 1, outcount, istatus)
      CALL arw_get_gbl_att_int_sca(handle, 'RA_SW_PHYSICS', dum_i, 1, outcount, istatus)
      CALL arw_get_gbl_att_int_sca(handle, 'SF_SFCLAY_PHYSICS', dum_i, 1, outcount, istatus)
      CALL arw_get_gbl_att_int_sca(handle, 'SF_SURFACE_PHYSICS', dum_i, 1, outcount, istatus)
      CALL arw_get_gbl_att_int_sca(handle, 'BL_PBL_PHYSICS', dum_i, 1, outcount, istatus)
      CALL arw_get_gbl_att_int_sca(handle, 'CU_PHYSICS', dum_i, 1, outcount, istatus)
      CALL arw_get_gbl_att_int_sca(handle, 'SURFACE_INPUT_SOURCE', dum_i, 1, outcount, istatus)
      CALL arw_get_gbl_att_int_sca(handle, 'SST_UPDATE', dum_i, 1, outcount, istatus)
      CALL arw_get_gbl_att_int_sca(handle, 'GRID_FDDA', dum_i, 1, outcount, istatus)
      CALL arw_get_gbl_att_int_sca(handle, 'UCMCALL', dum_i, 1, outcount, istatus)
      CALL arw_get_gbl_att_int_sca(handle, 'FEEDBACK', dum_i, 1, outcount, istatus)
      CALL arw_get_gbl_att_int_sca(handle, 'SMOOTH_OPTION', dum_i, 1, outcount, istatus)
      CALL arw_get_gbl_att_int_sca(handle, 'SWRAD_SCAT', dum_i, 1, outcount, istatus)
      CALL arw_get_gbl_att_int_sca(handle, 'W_DAMPING', dum_i, 1, outcount, istatus)
      CALL arw_get_gbl_att_int_sca(handle, 'PD_MOIST', dum_i, 1, outcount, istatus)
      CALL arw_get_gbl_att_int_sca(handle, 'PD_SCALAR', dum_i, 1, outcount, istatus)
      CALL arw_get_gbl_att_int_sca(handle, 'PD_TKE', dum_i, 1, outcount, istatus)
      CALL arw_get_gbl_att_int_sca(handle, 'OBS_NUDGE_OPT', dum_i, 1, outcount, istatus)
      CALL arw_get_gbl_att_int_sca(handle, 'GRID_ID', dum_i, 1, outcount, istatus)
      CALL arw_get_gbl_att_int_sca(handle, 'PARENT_ID', dum_i, 1, outcount, istatus)
      CALL arw_get_gbl_att_int_sca(handle, 'I_PARENT_START', dum_i, 1, outcount, istatus)
      CALL arw_get_gbl_att_int_sca(handle, 'J_PARENT_START', dum_i, 1, outcount, istatus)
      CALL arw_get_gbl_att_int_sca(handle, 'PARENT_GRID_RATIO', dum_i, 1, outcount, istatus)
      CALL arw_get_gbl_att_real_sca(handle, 'DT', dum_r, 1, outcount, istatus)
      CALL arw_get_gbl_att_int_sca(handle, 'ISWATER', dum_i, 1, outcount, istatus)
      CALL arw_get_gbl_att_int_sca(handle, 'ISICE', dum_i, 1, outcount, istatus)
      CALL arw_get_gbl_att_int_sca(handle, 'ISURBAN', dum_i, 1, outcount, istatus)
      CALL arw_get_gbl_att_int_sca(handle, 'ISOILWATER', dum_i, 1, outcount, istatus)


   END SUBROUTINE read_global_attrs
 
 
   SUBROUTINE input_close()
 
      implicit none
  
      ! Local variables
      integer :: istatus
  
      istatus = 0
#ifdef IO_BINARY
      if (io_form_input == BINARY) then
         CALL ext_int_ioclose(handle, istatus)
         CALL ext_int_ioexit(istatus)
      end if
#endif
#ifdef IO_NETCDF
      if (io_form_input == NETCDF) then
         CALL ext_ncd_ioclose(handle, istatus)
         CALL ext_ncd_ioexit(istatus)
      end if
#endif
#ifdef IO_GRIB1
      if (io_form_input == GRIB1) then
         CALL ext_gr1_ioclose(handle, istatus)
         CALL ext_gr1_ioexit(istatus)
      end if
#endif
 
      CALL q_destroy(unit_desc)
 
   END SUBROUTINE input_close
!------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------

   SUBROUTINE arw_ioinit (istatus)

      implicit none

      ! Arguments
      integer             :: istatus

#ifdef IO_BINARY
         if (io_form_input == BINARY) CALL ext_int_ioinit('sysdep info', istatus)
#endif
#ifdef IO_NETCDF
         if (io_form_input == NETCDF) CALL ext_ncd_ioinit('sysdep info', istatus)
#endif
#ifdef IO_GRIB1
         if (io_form_input == GRIB1) CALL ext_gr1_ioinit('sysdep info', istatus)
#endif
         CALL mprintf((istatus /= 0),ERROR,'Error in ext_pkg_ioinit')

   END SUBROUTINE arw_ioinit

!------------------------------------------------------------------------------------------

   SUBROUTINE arw_open_for_read (input_fname, handle, istatus)

      implicit none

      ! Arguments
      integer             :: handle, istatus
      character (len=128) :: input_fname

#ifdef IO_BINARY
         if (io_form_input == BINARY) &
            CALL ext_int_open_for_read(trim(input_fname), 1, 1, 'sysdep info', handle, istatus)
#endif
#ifdef IO_NETCDF
         if (io_form_input == NETCDF) &
            CALL ext_ncd_open_for_read(trim(input_fname), 1, 1, 'sysdep info', handle, istatus)
#endif
#ifdef IO_GRIB1
         if (io_form_input == GRIB1) &
            CALL ext_gr1_open_for_read(trim(input_fname), 1, 1, 'sysdep info', handle, istatus)
#endif
         CALL mprintf((istatus /= 0),ERROR,'Error in ext_pkg_open_for_read')

   END SUBROUTINE arw_open_for_read

!------------------------------------------------------------------------------------------

   SUBROUTINE arw_get_next_time (handle, datestr, istatus)

      implicit none

      ! Arguments
      integer             :: handle, istatus
      character (len=*)   :: datestr


#ifdef IO_BINARY
        if (io_form_input == BINARY) CALL ext_int_get_next_time(handle, datestr, istatus)
#endif
#ifdef IO_NETCDF
        if (io_form_input == NETCDF) CALL ext_ncd_get_next_time(handle, datestr, istatus)
#endif
#ifdef IO_GRIB1
        if (io_form_input == GRIB1)  CALL ext_gr1_get_next_time(handle, datestr, istatus)
#endif
        !!CALL mprintf((istatus /= 0),ERROR,'Error while reading next time .')

   END SUBROUTINE arw_get_next_time

!------------------------------------------------------------------------------------------

   SUBROUTINE arw_get_gbl_att_char (handle, att_string, att, istatus)

      implicit none

      ! Arguments
      integer             :: handle, istatus
      character*(*)       :: att_string
      character (len=*)   :: att


#ifdef IO_BINARY
        if (io_form_input == BINARY) CALL ext_int_get_dom_ti_char(handle, att_string, att, istatus)
#endif
#ifdef IO_NETCDF
        if (io_form_input == NETCDF) CALL ext_ncd_get_dom_ti_char(handle, att_string, att, istatus)
#endif
#ifdef IO_GRIB1
        if (io_form_input == GRIB1)  CALL ext_gr1_get_dom_ti_char(handle, att_string, att, istatus)
#endif
        !!CALL mprintf((istatus /= 0),ERROR,'Error while reading domain global charachter attribute.')
        IF ( istatus == 0 ) THEN
          iatts = iatts + 1
          WRITE(catts(iatts),'("@ global String comment ",A," = ",A)') trim(att_string), trim(att)
        END IF 

   END SUBROUTINE arw_get_gbl_att_char

!------------------------------------------------------------------------------------------

   SUBROUTINE arw_get_gbl_att_int_sca (handle, att_string, att, dim, outcount, istatus)

      implicit none

      ! Arguments
      integer             :: handle, dim, outcount, istatus
      character*(*)       :: att_string
      integer             :: att


#ifdef IO_BINARY
        if (io_form_input == BINARY) CALL ext_int_get_dom_ti_integer(handle, att_string, att, dim, outcount, istatus)
#endif
#ifdef IO_NETCDF
        if (io_form_input == NETCDF) CALL ext_ncd_get_dom_ti_integer(handle, att_string, att, dim, outcount, istatus)
#endif
#ifdef IO_GRIB1
        if (io_form_input == GRIB1)  CALL ext_gr1_get_dom_ti_integer(handle, att_string, att, dim, outcount, istatus)
#endif
        !!CALL mprintf((istatus /= 0),ERROR,'Error while reading domain global integer attribute.')
        IF ( istatus == 0 ) THEN
          iatts = iatts + 1
          WRITE(catts(iatts),'("@ global String comment ",A," = ",i5)') trim(att_string), att
        END IF

   END SUBROUTINE arw_get_gbl_att_int_sca

!------------------------------------------------------------------------------------------

   SUBROUTINE arw_get_gbl_att_real_sca (handle, att_string, att, dim, outcount, istatus)

      implicit none

      ! Arguments
      integer             :: handle, dim, outcount, istatus
      character*(*)       :: att_string
      real                :: att


#ifdef IO_BINARY
        if (io_form_input == BINARY) CALL ext_int_get_dom_ti_real(handle, att_string, att, dim, outcount, istatus)
#endif
#ifdef IO_NETCDF
        if (io_form_input == NETCDF) CALL ext_ncd_get_dom_ti_real(handle, att_string, att, dim, outcount, istatus)
#endif
#ifdef IO_GRIB1
        if (io_form_input == GRIB1)  CALL ext_gr1_get_dom_ti_real(handle, att_string, att, dim, outcount, istatus)
#endif
        !!CALL mprintf((istatus /= 0),ERROR,'Error while reading domain global real attribute.')
        IF ( istatus == 0 ) THEN
          iatts = iatts + 1
          WRITE(catts(iatts),'("@ global String comment ",A," = ",f12.2)') trim(att_string), att
        END IF

   END SUBROUTINE arw_get_gbl_att_real_sca

!------------------------------------------------------------------------------------------

   SUBROUTINE arw_get_gbl_att_real_arr (handle, att_string, att, dim, outcount, istatus)

      implicit none

      ! Arguments
      integer             :: handle, dim, outcount, istatus
      character*(*)       :: att_string
      real, dimension(*)  :: att


#ifdef IO_BINARY
        if (io_form_input == BINARY) CALL ext_int_get_dom_ti_real(handle, att_string, att, dim, outcount, istatus)
#endif
#ifdef IO_NETCDF
        if (io_form_input == NETCDF) CALL ext_ncd_get_dom_ti_real(handle, att_string, att, dim, outcount, istatus)
#endif
#ifdef IO_GRIB1
        if (io_form_input == GRIB1)  CALL ext_gr1_get_dom_ti_real(handle, att_string, att, dim, outcount, istatus)
#endif
        CALL mprintf((istatus /= 0),ERROR,'Error while reading domain global real attribute.')

   END SUBROUTINE arw_get_gbl_att_real_arr

!------------------------------------------------------------------------------------------

   SUBROUTINE arw_get_next_var (handle, cname, istatus)

      implicit none

      ! Arguments
      integer             :: handle, istatus, ios
      character*(*)       :: cname
      integer             :: ndim, wrftype
      character (LEN=3)   :: memorder
      character (LEN=128) :: stagger, cdesc, cunits


#ifdef IO_BINARY
      IF (io_form_input == BINARY) CALL ext_int_get_next_var(handle, cname, istatus) 
#endif
#ifdef IO_NETCDF
      IF (io_form_input == NETCDF) CALL ext_ncd_get_next_var(handle, cname, istatus) 
#endif
#ifdef IO_GRIB1
      !!IF (io_form_input == GRIB1) CALL ext_gr1_get_next_var(handle, cname, istatus) 
      IF (io_form_input == GRIB1) THEN
        found_next : DO
          READ (13,'(i3,5x,A3,4x,A1,5x,A13,A48,A15)',IOSTAT=ios) wrftype, memorder, stagger, cname, cdesc, cunits
          ndim =  LEN_trim(memorder)
          IF ( ndim > 3 ) ndim = 3
          IF ( iprogram == 1 .AND. ndim == 3 .AND. INDEX(memorder,'m') == 0 ) CYCLE found_next
          IF ( iprogram == 1 .AND. ndim == 1                                ) CYCLE found_next
          IF ( iprogram == 8 .AND. INDEX(memorder,'g') /= 0                 ) CYCLE found_next
          IF ( ios /= 0 ) THEN
             istatus = -1
          END IF
          EXIT found_next
        END DO found_next
      END IF
#endif

   END SUBROUTINE arw_get_next_var
 
!------------------------------------------------------------------------------------------

   SUBROUTINE arw_get_var_info (handle, cname, ndim, memorder, stagger, cdesc, cunits, domain_start, domain_end, wrftype, istatus)

      implicit none

      ! Arguments
      integer               :: handle, istatus, ios
      integer               :: ndim, wrftype
      integer, dimension(3) :: domain_start, domain_end
      character*(*)         :: cname, memorder, stagger, cdesc, cunits
      character (LEN=13)    :: cname_tmp
      CHARACTER (LEN=132)   :: command


      cunits  = ' '
      cdesc   = ' '
      stagger = ' '


#ifdef IO_BINARY
      IF (io_form_input == BINARY) &
            CALL ext_int_get_var_info(handle, cname, ndim, memorder, stagger, domain_start, &
                                      domain_end, wrftype, istatus)
#endif
#ifdef IO_NETCDF
      IF (io_form_input == NETCDF) &
            CALL ext_ncd_get_var_info(handle, cname, ndim, memorder, stagger, domain_start, &
                                      domain_end, wrftype, istatus)
#endif
#ifdef IO_GRIB1
      IF (io_form_input == GRIB1) THEN
        WRITE ( command , FMT='("grep ",A13," gribinfo.txt > .foo3")' ) cname
        CALL SYSTEM ( TRIM ( command ) )
        OPEN (15, file = ".foo3")

        found_field : DO

          READ (15,'(i3,5x,A3,4x,A1,5x,A13,A48,A15)',IOSTAT=ios) &
                     wrftype, memorder, stagger, cname_tmp, cdesc, cunits
          IF ( trim(cname) /= trim(cname_tmp) ) CYCLE found_field

          ndim =  LEN_trim(memorder) 
          IF (ndim > 3 ) ndim = 3
          IF      ( LEN_trim(memorder) == 3 ) THEN
                  domain_end(1) = west_east_dim
                  domain_end(2) = south_north_dim
                  domain_end(3) = bottom_top_dim
                  IF (stagger(1:1) == "X") domain_end(1) = domain_end(1) + 1
                  IF (stagger(1:1) == "Y") domain_end(2) = domain_end(2) + 1
                  IF (iprogram .ge. 6 .AND. stagger(1:1) == "Z") domain_end(3) = domain_end(3) + 1
                  IF ( INDEX(memorder,'m') /= 0 ) domain_end(3) = 12
                  IF ( INDEX(memorder,'u') /= 0 ) domain_end(3) = 24
                  memorder = "XYZ"
          ELSE IF ( LEN_trim(memorder) == 2 ) THEN
                  domain_end(1) = west_east_dim
                  domain_end(2) = south_north_dim
                  IF (stagger(1:1) == "X") domain_end(1) = domain_end(1) + 1
                  IF (stagger(1:1) == "Y") domain_end(2) = domain_end(2) + 1
                  memorder = "XY"
          ELSE IF ( LEN_trim(memorder) == 1 ) THEN
                  domain_end(1) = bottom_top_dim
                  IF (iprogram .ge. 6 .AND. stagger(1:1) == "Z") domain_end(1) = domain_end(1) + 1
                  memorder = "Z"
          END IF
          IF ( ios /= 0 ) THEN
             istatus = -1
          END IF
          CLOSE(15)
          CALL SYSTEM ( " rm .foo3 " )
          EXIT found_field

        END DO found_field

      END IF

#endif
     
      IF ( istatus /= 0 .AND. debug_level >= 300 ) &
        print*, 'In arw_get_var_info, problems getting field information for ', trim(cname)

   END SUBROUTINE arw_get_var_info
 
!------------------------------------------------------------------------------------------

   SUBROUTINE arw_read_field (handle, valid_date, cname, real_domain, wrftype, &
                             memorder, stagger, &
                             dimnames, domain_start, domain_end, istatus)

      implicit none

      ! Arguments
      integer                            :: handle, istatus
      integer                            :: ndim, wrftype
      integer, dimension(3)              :: domain_start, domain_end
      character*(*)                      :: cname, memorder, stagger, valid_date
      character (len=128), dimension(3)  :: dimnames
      real, pointer, dimension(:,:,:)    :: real_domain
      integer, pointer, dimension(:,:,:) :: int_domain
  
#include "wrf_io_flags.h"
#include "wrf_status_codes.h"
  

         if (wrftype == WRF_REAL) then
#ifdef IO_BINARY
            if (io_form_input == BINARY) then
               CALL ext_int_read_field(handle, valid_date, cname, real_domain, wrftype, &
                             1, 1, 0, memorder, stagger, &
                             dimnames, domain_start, domain_end, domain_start, domain_end, &
                             domain_start, domain_end, istatus)
            end if
#endif
#ifdef IO_NETCDF
            if (io_form_input == NETCDF) then
               CALL ext_ncd_read_field(handle, valid_date, cname, real_domain, wrftype, &
                             1, 1, 0, memorder, stagger, &
                             dimnames, domain_start, domain_end, domain_start, domain_end, &
                             domain_start, domain_end, istatus)
            end if
#endif
#ifdef IO_GRIB1
            if (io_form_input == GRIB1) then
               CALL ext_gr1_read_field(handle, valid_date, cname, real_domain, wrftype, &
                             1, 1, 0, memorder, stagger, &
                             dimnames, domain_start, domain_end, domain_start, domain_end, &
                             domain_start, domain_end, istatus)
            end if
#endif
         elseif (wrftype == WRF_INTEGER) then
         allocate(int_domain(domain_start(1):domain_end(1), domain_start(2):domain_end(2), domain_start(3):domain_end(3)))
#ifdef IO_BINARY
            if (io_form_input == BINARY) then
               CALL ext_int_read_field(handle, valid_date, cname, int_domain, wrftype, &
                             1, 1, 0, memorder, stagger, &
                             dimnames, domain_start, domain_end, domain_start, domain_end, &
                             domain_start, domain_end, istatus)
            end if
#endif
#ifdef IO_NETCDF
            if (io_form_input == NETCDF) then
               CALL ext_ncd_read_field(handle, valid_date, cname, int_domain, wrftype, &
                             1, 1, 0, memorder, stagger, &
                             dimnames, domain_start, domain_end, domain_start, domain_end, &
                             domain_start, domain_end, istatus)
            end if
#endif
#ifdef IO_GRIB1
            if (io_form_input == GRIB1) then
               CALL ext_gr1_read_field(handle, valid_date, cname, int_domain, wrftype, &
                             1, 1, 0, memorder, stagger, &
                             dimnames, domain_start, domain_end, domain_start, domain_end, &
                             domain_start, domain_end, istatus)
            end if
#endif
            real_domain = real(int_domain)
            deallocate(int_domain)
         end if 
#ifdef IO_GRIB1
         if (io_form_input == GRIB1 .AND. istatus == -5 ) RETURN
#endif
         IF ( istatus /= 0 ) print*, 'In arw_read_field, problems getting field for ', trim(cname)


   END SUBROUTINE arw_read_field
 
!------------------------------------------------------------------------------------------

END MODULE input_module
