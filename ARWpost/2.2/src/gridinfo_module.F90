!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! MODULE GRIDINFO_MODULE
!
! This module handles (i.e., acquires, stores, and makes available) all data
!   describing the model domains to be processed.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MODULE gridinfo_module

   USE misc_definitions_module
   USE module_debug
   USE module_model_basics
   USE module_get_file_names
 
   ! Variables
   integer                             :: interval_seconds, io_form_input
   integer                             :: tacc, debug_level
   character (len=19)                  :: start_date, end_date
   character (len=128)                 :: input_root_name, output_root_name
   character (len=128)                 :: output_title
   logical                             :: is_used, mercator_defs, grads_low_res
   logical                             :: split_output
   integer                             :: frames_per_outfile
   integer                             :: interp_method
   real, dimension(100)                :: interp_levels
   integer                             :: number_of_zlevs
   real, allocatable, dimension(:,:,:) :: vert_array
   character (len=4000)                :: plot_these_fields
   character (len=20)                  :: plot
   character (len=5)                   :: output_type
   logical                             :: extrapolate
 
   CONTAINS
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   ! Name: get_namelist_params
   ! Purpose: Read namelist parameters.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   SUBROUTINE get_namelist_params()
 
      implicit none
  
      ! Local variables
      integer              :: start_year, start_month, start_day, start_hour, &
                              end_year,   end_month,   end_day,   end_hour 
      integer              :: funit, i
      character (len=20)   :: dummy
      character (len=2000) :: fields,fields_file
      namelist /datetime/ start_date, start_year, start_month, start_day, start_hour,   &
                          end_date,   end_year,   end_month,   end_day,   end_hour,     &
                          interval_seconds, tacc, debug_level
      namelist /io/       io_form_input, input_root_name, output_root_name,             &
                          output_title, mercator_defs, grads_low_res,                   &
                          split_output, frames_per_outfile,                             &
                          plot, fields, fields_file, output_type
      namelist /interp/   interp_method, interp_levels, extrapolate
        
      ! Set defaults
      io_form_input    = -1
      start_date       = '0000-00-00_00:00:00'
      end_date         = '0000-00-00_00:00:00'
      tacc             = 0
      debug_level      = 0
      input_root_name  = './'
      output_root_name = './'
      output_title     = '  '
      ! all, basic, file, list, all_file, all_list, basic_file, basic_list, 
      ! list_file, all_list_file, basic_list_file
      plot             = 'all'
      interp_method    = 0
      vertical_type    = 'n'
      interp_levels    = -99999.
      extrapolate      = .FALSE.
      mercator_defs    = .FALSE.
      grads_low_res    = .FALSE.
      split_output     = .FALSE.
      frames_per_outfile = 1
      output_type      = 'grads'
  
  
      ! Read parameters from Fortran namelist
      DO funit=10,100
         inquire(unit=funit, opened=is_used)
         IF (.not. is_used) EXIT
      END DO
      OPEN(funit,file='namelist.ARWpost',status='old',form='formatted',err=1000)
      READ(funit,datetime)
      READ(funit,io)
      READ(funit,interp)
      CLOSE(funit)


      ! Get all the input file names
      CALL unix_ls ( input_root_name )
  

      ! Check for valid io_form_input
      IF ( &
#ifdef IO_BINARY
          io_form_input /= BINARY .and. & 
#endif
#ifdef IO_NETCDF
          io_form_input /= NETCDF .and. & 
#endif
#ifdef IO_GRIB1
          io_form_input /= GRIB1 .and. & 
#endif
          .true. ) then
         write(6,*) ' '
         write(6,*) 'Error: No valid value for io_form_input was specified in the namelist.'
         write(6,*) '       Valid io_form_input values are:'
#ifdef IO_BINARY
         write(6,*) '       ',BINARY,' (=BINARY)'
#endif
#ifdef IO_NETCDF
         write(6,*) '       ',NETCDF,' (=NETCDF)'
#endif
#ifdef IO_GRIB1
         write(6,*) '       ',GRIB1,' (=GRIB1)'
#endif
         write(6,*) ' '
         STOP
      END IF

#ifdef IO_GRIB1
      IF (io_form_input == GRIB1) THEN
        open (13, file = "gribinfo.txt")
      END IF
#endif
  
  
      IF (start_date == '0000-00-00_00:00:00') then
         ! Build starting date string
         WRITE(start_date, '(i4.4,a1,i2.2,a1,i2.2,a1,i2.2,a6)') &
               start_year,'-',start_month,'-',start_day,' ',start_hour,':00:00'
     
         ! Build ending date string
         WRITE(end_date, '(i4.4,a1,i2.2,a1,i2.2,a1,i2.2,a6)') &
               end_year,'-',end_month,'-',end_day,' ',end_hour,':00:00'
      END IF


      number_of_zlevs = 0
      IF (interp_method == -1 .or. interp_method == 0) interp_levels    = -99999.
      IF (abs(interp_method) == 1) vertical_type = 'z'
      IF (interp_method == 1 .and. interp_levels(1) .gt. 100.) vertical_type = 'p'
      IF (interp_method == 1) THEN
        DO
          IF (interp_levels(number_of_zlevs+1) == -99999.) EXIT
          number_of_zlevs = number_of_zlevs + 1
        END DO
      ENDIF
      IF ( extrapolate ) THEN
      IF ( vertical_type /= 'p'.AND. vertical_type /= 'z' ) THEN
         extrapolate = .FALSE.
          CALL mprintf(.true.,STDOUT, &
           '   WARNING: Can only extrapolate when interpolating to pressure/height fields' )
      ENDIF
      ENDIF


      !! Which fields do we want. Options are:  all, basic, file, list, all_file, 
      !! all_list, basic_file, basic_list, list_file, all_list_file, basic_list_file
      plot_these_fields = ','
      IF ( INDEX(plot,'basic') /= 0) &
         plot_these_fields = &
         ',HGT,HGT_M,LANDMASK,LU_INDEX,MU,MUB,P,PB,PBLH,PH,PHB,PSFC, &
&Q2,RAINC,RAINNC,SST,T,TT,T2,TH2,TMN,TSK,U,UU,U10,V,VV,V10,VEGFRA,W, &
&XLAND,XLAT,XLAT_M,XLONG,XLONG_M,'

      IF ( INDEX(plot,'list') /= 0) THEN
        DO i = 1 , len(fields)
          IF (fields(i:i) /= ' ' ) THEN
            plot_these_fields = trim(plot_these_fields)//fields(i:i)
          ENDIF
        END DO
        plot_these_fields = trim(plot_these_fields)//","
      END IF
      IF ( INDEX(plot,'file') /= 0) THEN
        DO funit=10,100
           inquire(unit=funit, opened=is_used)
           IF (.not. is_used) EXIT
        END DO
        OPEN(funit,file=fields_file,status='old',form='formatted',err=1001)
        DO 
          READ(funit,*,END=999) dummy
          plot_these_fields = trim(plot_these_fields)//trim(dummy)//","
        END DO
  999   CLOSE(funit)
      END IF


      IF ( INDEX(plot,'all') /= 0 .and. output_type == 'v5d' ) THEN
        CALL mprintf(.true.,ERROR,'ERROR: plot ALL not currently supported for VIS5D')
      END IF
  
      RETURN
  

 1000 CALL mprintf(.true.,ERROR,'Error opening file namelist.ARWpost')
      RETURN


 1001 CALL mprintf(.true.,STDOUT, &
      '   WARNING: Could not open fields input file: %s', s1=trim(fields_file) )
      CALL mprintf(.true.,STDOUT,' ')
      RETURN
 

   END SUBROUTINE get_namelist_params
  
END MODULE gridinfo_module
