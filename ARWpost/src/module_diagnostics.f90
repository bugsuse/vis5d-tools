!! Diagnostics
!! See moddule_diagnostic for which fields are available 

MODULE module_diagnostics

  USE output_module
  USE gridinfo_module
  USE module_interp
  USE module_arrays
  USE constants_module


  USE module_calc_pressure
  USE module_calc_height
  USE module_calc_theta
  USE module_calc_tk
  USE module_calc_tc
  USE module_calc_td
  USE module_calc_td2
  USE module_calc_rh
  USE module_calc_rh2
  USE module_calc_uvmet
  USE module_calc_wdir
  USE module_calc_wspd
  USE module_calc_slp
  USE module_calc_dbz
  USE module_calc_cape
  USE module_calc_clfr

  CONTAINS

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: process_diagnostics
   ! Purpose: All new calls to diagnostics routines go here
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   SUBROUTINE process_diagnostics ( valid_date )

      IMPLICIT NONE

      ! Local variables
      character (len=128)                    :: cname, cunits, cdesc, c_nm
      real, allocatable, dimension(:,:)      :: SCR2
      real, pointer, dimension(:,:,:)        :: SCR3
      real, pointer, dimension(:,:,:)        :: SCR, SCRa, SCRb, SCRc
      real, pointer, dimension(:,:,:)        :: data_out 
      integer                                :: nxout, nyout, nzout
      integer                                :: ii, jj, kk
      character (len=19)                     :: valid_date
      integer                                :: good_to_go, geo



!!! Calculate pressure in hPA
        IF ( INDEX(plot_these_fields,",pressure,") /= 0 ) THEN
          IF ( have_PRES ) THEN 
            IF ( ASSOCIATED(SCR) ) DEALLOCATE(SCR)
            ALLOCATE(SCR(west_east_dim,south_north_dim,bottom_top_dim))
            CALL calc_pressure(SCR, cname, cdesc, cunits) 
            CALL interp (SCR, west_east_dim, south_north_dim, bottom_top_dim, &
                         data_out, nxout, nyout, nzout, &
                         vert_array, interp_levels, number_of_zlevs,cname) 
            CALL write_dat (data_out, nxout, nyout, nzout, cname, cdesc, cunits)
            DEALLOCATE(SCR)
          END IF
        END IF


!!! Calculate height/geopt 
        IF ( INDEX(plot_these_fields,",geopt,") /= 0) THEN      
          IF ( have_GEOPT ) THEN 
            IF ( ASSOCIATED(SCR) ) DEALLOCATE(SCR)
            ALLOCATE(SCR(west_east_dim,south_north_dim,bottom_top_dim))
            CALL calc_height(SCR, cname, cdesc, cunits) 
            CALL interp (SCR, west_east_dim, south_north_dim, bottom_top_dim, &
                         data_out, nxout, nyout, nzout, &
                         vert_array, interp_levels, number_of_zlevs,cname) 
            CALL write_dat (data_out, nxout, nyout, nzout, cname, cdesc, cunits)
            DEALLOCATE(SCR)
          END IF
        END IF
        IF ( INDEX(plot_these_fields,",height,") /= 0 ) THEN 
          IF ( have_GEOPT ) THEN 
            IF ( ASSOCIATED(SCR) ) DEALLOCATE(SCR)
            ALLOCATE(SCR(west_east_dim,south_north_dim,bottom_top_dim))
            cname = "height"
            CALL calc_height(SCR, cname, cdesc, cunits) 
            CALL interp (SCR, west_east_dim, south_north_dim, bottom_top_dim, &
                         data_out, nxout, nyout, nzout, &
                         vert_array, interp_levels, number_of_zlevs,cname) 
            CALL write_dat (data_out, nxout, nyout, nzout, cname, cdesc, cunits)
            DEALLOCATE(SCR)
          END IF
        END IF


!!! Calculate temperature 
        IF ( INDEX(plot_these_fields,",tk,") /= 0 ) THEN
          IF ( have_TK ) THEN 
            IF ( ASSOCIATED(SCR) ) DEALLOCATE(SCR)
            ALLOCATE(SCR(west_east_dim,south_north_dim,bottom_top_dim))
            CALL calc_tk(SCR, cname, cdesc, cunits) 
            CALL interp (SCR, west_east_dim, south_north_dim, bottom_top_dim, &
                         data_out, nxout, nyout, nzout, &
                         vert_array, interp_levels, number_of_zlevs,cname) 
            CALL write_dat (data_out, nxout, nyout, nzout, cname, cdesc, cunits)
            DEALLOCATE(SCR)
          END IF
        END IF
        IF ( INDEX(plot_these_fields,",tc,") /= 0 ) THEN
          IF ( have_TK ) THEN 
            IF ( ASSOCIATED(SCR) ) DEALLOCATE(SCR)
            ALLOCATE(SCR(west_east_dim,south_north_dim,bottom_top_dim))
            CALL calc_tc(SCR, cname, cdesc, cunits) 
            CALL interp (SCR, west_east_dim, south_north_dim, bottom_top_dim, &
                         data_out, nxout, nyout, nzout, &
                         vert_array, interp_levels, number_of_zlevs,cname) 
            CALL write_dat (data_out, nxout, nyout, nzout, cname, cdesc, cunits)
            DEALLOCATE(SCR)
          END IF
        END IF


!!! Calculate theta
        IF ( INDEX(plot_these_fields,",theta,") /= 0 ) THEN
          good_to_go = 0
          IF ( .not. have_T ) CALL get_keep_array ( valid_date, good_to_go, "T" )
          IF ( good_to_go == 0 ) THEN 
            IF ( ASSOCIATED(SCR) ) DEALLOCATE(SCR)
            ALLOCATE(SCR(west_east_dim,south_north_dim,bottom_top_dim))
            CALL calc_theta(SCR, cname, cdesc, cunits) 
            CALL interp (SCR, west_east_dim, south_north_dim, bottom_top_dim, &
                         data_out, nxout, nyout, nzout, &
                         vert_array, interp_levels, number_of_zlevs,cname) 
            CALL write_dat (data_out, nxout, nyout, nzout, cname, cdesc, cunits)
            DEALLOCATE(SCR)
          END IF
        END IF


!!! Dewpoint temperature
        IF ( INDEX(plot_these_fields,",td,") /= 0 ) THEN
          good_to_go = 0
          IF ( .not. have_QV ) CALL get_keep_array ( valid_date, good_to_go, "QVAPOR" )
          IF ( good_to_go == 0 .AND. have_PRES ) THEN 
            IF ( ASSOCIATED(SCR) ) DEALLOCATE(SCR)
            ALLOCATE(SCR(west_east_dim,south_north_dim,bottom_top_dim))
            CALL calc_td(SCR, cname, cdesc, cunits) 
            CALL interp (SCR, west_east_dim, south_north_dim, bottom_top_dim, &
                         data_out, nxout, nyout, nzout, &
                         vert_array, interp_levels, number_of_zlevs,cname) 
            CALL write_dat (data_out, nxout, nyout, nzout, cname, cdesc, cunits)
            DEALLOCATE(SCR)
          END IF
        END IF


!!! Dewpoint temperature at 2m
        IF ( INDEX(plot_these_fields,",td2,") /= 0 ) THEN
          good_to_go = 0
          IF ( .not. have_T2 )   CALL get_keep_array ( valid_date, good_to_go, "T2" )
          IF ( .not. have_Q2 )   CALL get_keep_array ( valid_date, good_to_go, "Q2" )
          IF ( .not. have_PSFC ) CALL get_keep_array ( valid_date, good_to_go, "PSFC" )
          IF ( good_to_go == 0 ) THEN 
            IF ( ASSOCIATED(SCR) ) DEALLOCATE(SCR)
            ALLOCATE(SCR(west_east_dim,south_north_dim,1))
            CALL calc_td2(SCR, cname, cdesc, cunits) 
            CALL interp (SCR, west_east_dim, south_north_dim, 1, &
                         data_out, nxout, nyout, nzout, &
                         vert_array, interp_levels, number_of_zlevs,cname) 
            CALL write_dat (data_out, nxout, nyout, nzout, cname, cdesc, cunits)
            DEALLOCATE(SCR)
          END IF
        END IF


!!! Relative Humidity    
        IF ( INDEX(plot_these_fields,",rh,")   /= 0 .OR. &
             INDEX(plot_these_fields,",clfr,") /= 0 ) THEN
          good_to_go = 0
          IF ( .not. have_QV ) CALL get_keep_array ( valid_date, good_to_go, "QVAPOR" )
          IF ( good_to_go == 0 .AND. have_TK .AND. have_PRES ) THEN 
            IF ( ASSOCIATED(SCR) ) DEALLOCATE(SCR)
            ALLOCATE(SCR(west_east_dim,south_north_dim,bottom_top_dim))
            CALL calc_rh(SCR, cname, cdesc, cunits) 
            IF ( INDEX(plot_these_fields,",rh,")   /= 0 ) THEN
              CALL interp (SCR, west_east_dim, south_north_dim, bottom_top_dim, &
                           data_out, nxout, nyout, nzout, &
                           vert_array, interp_levels, number_of_zlevs,cname) 
              CALL write_dat (data_out, nxout, nyout, nzout, cname, cdesc, cunits)
            END IF

            !!!  Cloud fractions
            IF ( INDEX(plot_these_fields,",clfr,") /= 0 ) THEN
              IF ( ASSOCIATED(SCRa) ) DEALLOCATE(SCRa)
              ALLOCATE(SCRa(west_east_dim,south_north_dim,1))
              SCRa = 0.0
              IF ( ASSOCIATED(SCRb) ) DEALLOCATE(SCRb)
              ALLOCATE(SCRb(west_east_dim,south_north_dim,1))
              SCRb = 0.0
              IF ( ASSOCIATED(SCRc) ) DEALLOCATE(SCRc)
              ALLOCATE(SCRc(west_east_dim,south_north_dim,1))
              SCRc = 0.0
              CALL calc_clfr(SCRa, SCRb, SCRc, cname, cdesc, cunits, SCR) 
              cname = "clflo"
              cdesc = "Low Cloud Fraction"
              CALL interp (SCRa, west_east_dim, south_north_dim, 1, &
                           data_out, nxout, nyout, nzout, &
                           vert_array, interp_levels, number_of_zlevs,cname) 
              CALL write_dat (data_out, nxout, nyout, nzout, cname, cdesc, cunits)
              cname = "clfmi"
              cdesc = "Mid Cloud Fraction"
              CALL interp (SCRb, west_east_dim, south_north_dim, 1, &
                           data_out, nxout, nyout, nzout, &
                           vert_array, interp_levels, number_of_zlevs,cname) 
              CALL write_dat (data_out, nxout, nyout, nzout, cname, cdesc, cunits)
              cname = "clfhi"
              cdesc = "High Cloud Fraction"
              CALL interp (SCRc, west_east_dim, south_north_dim, 1, &
                           data_out, nxout, nyout, nzout, &
                           vert_array, interp_levels, number_of_zlevs,cname) 
              CALL write_dat (data_out, nxout, nyout, nzout, cname, cdesc, cunits)

              DEALLOCATE(SCRa)
              DEALLOCATE(SCRb)
              DEALLOCATE(SCRc)
            END IF
            DEALLOCATE(SCR)
          END IF
        END IF

        IF ( INDEX(plot_these_fields,",rh2,") /= 0 ) THEN
          good_to_go = 0
          IF ( .not. have_T2 )   CALL get_keep_array ( valid_date, good_to_go, "T2" )
          IF ( .not. have_Q2 )   CALL get_keep_array ( valid_date, good_to_go, "Q2" )
          IF ( .not. have_PSFC ) CALL get_keep_array ( valid_date, good_to_go, "PSFC" )
          IF ( good_to_go == 0 ) THEN 
            IF ( ASSOCIATED(SCR) ) DEALLOCATE(SCR)
            ALLOCATE(SCR(west_east_dim,south_north_dim,1))
            CALL calc_rh2(SCR, cname, cdesc, cunits) 
            CALL interp (SCR, west_east_dim, south_north_dim, 1, &
                         data_out, nxout, nyout, nzout, &
                         vert_array, interp_levels, number_of_zlevs,cname) 
            CALL write_dat (data_out, nxout, nyout, nzout, cname, cdesc, cunits)

            DEALLOCATE(SCR)
          END IF
        END IF


!!! Wind speed       
        IF ( INDEX(plot_these_fields,",wspd,") /= 0 ) THEN
          good_to_go = 0
          IF ( .not. have_UUU ) CALL get_keep_array ( valid_date, good_to_go, "U", "UU" )
          IF ( .not. have_VVV ) CALL get_keep_array ( valid_date, good_to_go, "V", "VV" )
          IF ( good_to_go == 0 ) THEN 
            IF ( ASSOCIATED(SCR) ) DEALLOCATE(SCR)
            ALLOCATE(SCR(west_east_dim,south_north_dim,bottom_top_dim))
            CALL calc_wspd(SCR, cname, cdesc, cunits, 1) 
            CALL interp (SCR, west_east_dim, south_north_dim, bottom_top_dim, &
                         data_out, nxout, nyout, nzout, &
                         vert_array, interp_levels, number_of_zlevs,cname) 
            CALL write_dat (data_out, nxout, nyout, nzout, cname, cdesc, cunits)
            DEALLOCATE(SCR)
          END IF
        END IF


!!! Wind direction       
        IF ( INDEX(plot_these_fields,",wdir,") /= 0 ) THEN
          good_to_go = 0
          IF ( .not. have_UUU ) CALL get_keep_array ( valid_date, good_to_go, "U", "UU" )
          IF ( .not. have_VVV ) CALL get_keep_array ( valid_date, good_to_go, "V", "VV" )
          IF ( good_to_go == 0 ) THEN 
            IF ( ASSOCIATED(SCR) ) DEALLOCATE(SCR)
            ALLOCATE(SCR(west_east_dim,south_north_dim,bottom_top_dim))
            CALL calc_wdir(SCR, cname, cdesc, cunits, 1) 
            CALL interp (SCR, west_east_dim, south_north_dim, bottom_top_dim, &
                         data_out, nxout, nyout, nzout, &
                         vert_array, interp_levels, number_of_zlevs,cname) 
            CALL write_dat (data_out, nxout, nyout, nzout, cname, cdesc, cunits)
            DEALLOCATE(SCR)
          END IF
        END IF


!!! Wind speed  - 10m      
        IF ( INDEX(plot_these_fields,",ws10,") /= 0 ) THEN
          good_to_go = 0
          IF ( .not. have_U10 ) CALL get_keep_array ( valid_date, good_to_go, "U10" )
          IF ( .not. have_V10 ) CALL get_keep_array ( valid_date, good_to_go, "V10" )
          IF ( good_to_go == 0 ) THEN 
            IF ( ASSOCIATED(SCR) ) DEALLOCATE(SCR)
            ALLOCATE(SCR(west_east_dim,south_north_dim,1))
            CALL calc_wspd(SCR, cname, cdesc, cunits, 0) 
            CALL interp (SCR, west_east_dim, south_north_dim, 1, &
                         data_out, nxout, nyout, nzout, &
                         vert_array, interp_levels, number_of_zlevs,cname) 
            CALL write_dat (data_out, nxout, nyout, nzout, cname, cdesc, cunits)
            DEALLOCATE(SCR)
          END IF
        END IF


!!! Wind direction  - 10m      
        IF ( INDEX(plot_these_fields,",wd10,") /= 0 ) THEN
          good_to_go = 0
          IF ( .not. have_U10 ) CALL get_keep_array ( valid_date, good_to_go, "U10" )
          IF ( .not. have_V10 ) CALL get_keep_array ( valid_date, good_to_go, "V10" )
          IF ( good_to_go == 0 ) THEN 
            IF ( ASSOCIATED(SCR) ) DEALLOCATE(SCR)
            ALLOCATE(SCR(west_east_dim,south_north_dim,1))
            CALL calc_wdir(SCR, cname, cdesc, cunits, 0) 
            CALL interp (SCR, west_east_dim, south_north_dim, 1, &
                         data_out, nxout, nyout, nzout, &
                         vert_array, interp_levels, number_of_zlevs,cname) 
            CALL write_dat (data_out, nxout, nyout, nzout, cname, cdesc, cunits)
            DEALLOCATE(SCR)
          END IF
        END IF


!!! Rotated U and V      
        IF ( INDEX(plot_these_fields,",umet,") /= 0 .OR. &
             INDEX(plot_these_fields,",vmet,") /= 0) THEN
          good_to_go = 0
          IF ( .not. have_UUU )   CALL get_keep_array ( valid_date, good_to_go, "U", "UU" )
          IF ( .not. have_VVV )   CALL get_keep_array ( valid_date, good_to_go, "V", "VV" )
          IF ( .not. have_XLAT )  CALL get_keep_array ( valid_date, good_to_go, "XLAT", "XLAT_M" )
          IF ( .not. have_XLONG ) CALL get_keep_array ( valid_date, good_to_go, "XLONG", "XLONG_M" )
          IF ( good_to_go == 0 ) THEN 

            IF ( ASSOCIATED(SCRa) ) DEALLOCATE(SCRa)
            IF ( ASSOCIATED(SCRb) ) DEALLOCATE(SCRb)
            ALLOCATE(SCRa(west_east_dim,south_north_dim,bottom_top_dim))
            ALLOCATE(SCRb(west_east_dim,south_north_dim,bottom_top_dim))
            SCRa = 0.0
            SCRb = 0.0
            CALL calc_uvmet(SCRa, SCRb, cname, cdesc, cunits, 1) 

            IF ( INDEX(plot_these_fields,",umet,") /= 0 ) THEN
              cname = "umet"
              CALL interp (SCRa, west_east_dim, south_north_dim, bottom_top_dim, &
                           data_out, nxout, nyout, nzout, &
                           vert_array, interp_levels, number_of_zlevs,cname) 
              CALL write_dat (data_out, nxout, nyout, nzout, cname, cdesc, cunits)
            END IF
            IF ( INDEX(plot_these_fields,",vmet,") /= 0) THEN
              cname = "vmet"
              CALL interp (SCRb, west_east_dim, south_north_dim, bottom_top_dim, &
                           data_out, nxout, nyout, nzout, &
                           vert_array, interp_levels, number_of_zlevs,cname) 
              CALL write_dat (data_out, nxout, nyout, nzout, cname, cdesc, cunits)
            END IF
            DEALLOCATE(SCRa)
            DEALLOCATE(SCRb)

          END IF
        END IF


!!! Rotated U10 and V10      
        IF ( INDEX(plot_these_fields,",u10m,") /= 0 .OR. &
             INDEX(plot_these_fields,",v10m,") /= 0) THEN
          good_to_go = 0
          IF ( .not. have_U10 )   CALL get_keep_array ( valid_date, good_to_go, "U10" )
          IF ( .not. have_V10 )   CALL get_keep_array ( valid_date, good_to_go, "V10" )
          IF ( .not. have_XLAT )  CALL get_keep_array ( valid_date, good_to_go, "XLAT", "XLAT_M" )
          IF ( .not. have_XLONG ) CALL get_keep_array ( valid_date, good_to_go, "XLONG", "XLONG_M" )
          IF ( good_to_go == 0 ) THEN 

            IF ( ASSOCIATED(SCRa) ) DEALLOCATE(SCRa)
            IF ( ASSOCIATED(SCRb) ) DEALLOCATE(SCRb)
            ALLOCATE(SCRa(west_east_dim,south_north_dim,1))
            ALLOCATE(SCRb(west_east_dim,south_north_dim,1))
            SCRa = 0.0
            SCRb = 0.0
            CALL calc_uvmet(SCRa, SCRb, cname, cdesc, cunits, 0) 

            IF ( INDEX(plot_these_fields,",u10m,") /= 0 ) THEN
              cname = "u10m"
              CALL interp (SCRa, west_east_dim, south_north_dim, 1, &
                           data_out, nxout, nyout, nzout, &
                           vert_array, interp_levels, number_of_zlevs,cname) 
              CALL write_dat (data_out, nxout, nyout, nzout, cname, cdesc, cunits)
            END IF
            IF ( INDEX(plot_these_fields,",v10m,") /= 0) THEN
              cname = "v10m"
              CALL interp( SCRb, west_east_dim, south_north_dim, 1, &
                           data_out, nxout, nyout, nzout, &
                           vert_array, interp_levels, number_of_zlevs,cname) 
              CALL write_dat (data_out, nxout, nyout, nzout, cname, cdesc, cunits)
            END IF
            DEALLOCATE(SCRa)
            DEALLOCATE(SCRb)

          END IF
        END IF

        !!! Clobber some wind arrays
        IF (ALLOCATED(UUU)) DEALLOCATE(UUU)
        have_UUU = .FALSE.
        IF (ALLOCATED(VVV)) DEALLOCATE(VVV)
        have_VVV = .FALSE.
        IF (ALLOCATED(U10)) DEALLOCATE(U10)
        have_UUU = .FALSE.
        IF (ALLOCATED(V10)) DEALLOCATE(V10)
        have_VVV = .FALSE.



!!! Sea Level Pressure  
        IF ( INDEX(plot_these_fields,",slp,") /= 0 ) THEN
          good_to_go = 0
          IF ( .not. have_QV )  CALL get_keep_array ( valid_date, good_to_go, "QVAPOR" )
          IF ( good_to_go == 0 .AND. have_TK .AND. have_PRES .AND. have_GEOPT ) THEN 
            IF ( ASSOCIATED(SCR) ) DEALLOCATE(SCR)
            ALLOCATE(SCR(west_east_dim,south_north_dim,1))
            CALL calc_slp(SCR, cname, cdesc, cunits) 
            CALL interp (SCR, west_east_dim, south_north_dim, 1, &
                         data_out, nxout, nyout, nzout, &
                         vert_array, interp_levels, number_of_zlevs,cname) 
            CALL write_dat (data_out, nxout, nyout, nzout, cname, cdesc, cunits)
            DEALLOCATE(SCR)
          END IF
        END IF


!!! Reflectivity             
        IF ( INDEX(plot_these_fields,",dbz,")     /= 0 .OR. &
             INDEX(plot_these_fields,",max_dbz,") /= 0) THEN
          good_to_go = 0
          IF ( .not. have_QS )  CALL get_keep_array ( valid_date, good_to_go, "QSNOW" )
          IF ( .not. have_QG )  CALL get_keep_array ( valid_date, good_to_go, "QGRAUP" )
          good_to_go = 0
          IF ( .not. have_QV )  CALL get_keep_array ( valid_date, good_to_go, "QVAPOR" )
          IF ( .not. have_QR )  CALL get_keep_array ( valid_date, good_to_go, "QRAIN" )
          IF ( good_to_go == 0 .AND. have_TK .AND. have_PRES ) THEN 
            IF ( ASSOCIATED(SCR) ) DEALLOCATE(SCR)
            ALLOCATE(SCR(west_east_dim,south_north_dim,bottom_top_dim))
            CALL calc_dbz(SCR, cname, cdesc, cunits) 

            IF ( INDEX(plot_these_fields,",dbz,") /= 0 ) THEN
              CALL interp (SCR, west_east_dim, south_north_dim, bottom_top_dim, &
                           data_out, nxout, nyout, nzout, &
                           vert_array, interp_levels, number_of_zlevs,cname) 
              CALL write_dat (data_out, nxout, nyout, nzout, cname, cdesc, cunits)
            END IF

            IF ( INDEX(plot_these_fields,",max_dbz,") /= 0) THEN
              ALLOCATE(SCR2(west_east_dim,south_north_dim))
              SCR2 = 0.0
              DO jj = 1,south_north_dim
                DO ii = 1,west_east_dim
                  DO kk = 1,bottom_top_dim
                    SCR2(ii,jj) = MAX( SCR2(ii,jj) , SCR(ii,jj,kk) )
                  END DO 
                END DO 
              END DO 
              IF (ASSOCIATED(SCR)) DEALLOCATE(SCR)
              ALLOCATE(SCR(west_east_dim,south_north_dim,1))
              SCR(:,:,1) = SCR2(:,:)
              cname = "max_dbz"
              cdesc    = "Max Reflectivity"
              CALL interp (SCR, west_east_dim, south_north_dim, 1, &
                           data_out, nxout, nyout, nzout, &
                           vert_array, interp_levels, number_of_zlevs,cname) 
              CALL write_dat (data_out, nxout, nyout, nzout, cname, cdesc, cunits)
              DEALLOCATE(SCR2)
            END IF
            DEALLOCATE(SCR)

          END IF
        END IF



!!! 3D CAPE/CIN 
        IF ( INDEX(plot_these_fields,",cape,") /= 0 .OR. &
             INDEX(plot_these_fields,",cin,")  /= 0) THEN
          good_to_go = 0
          IF ( .not. have_QV )   CALL get_keep_array ( valid_date, good_to_go, "QVAPOR" )
          IF ( .not. have_HGT )  CALL get_keep_array ( valid_date, good_to_go, "HGT" )
          IF ( .not. have_PSFC ) CALL get_keep_array ( valid_date, good_to_go, "PSFC" )
          IF ( good_to_go == 0 .AND. have_TK .AND. have_PRES .AND. have_GEOPT ) THEN 

            IF ( ASSOCIATED(SCRa) ) DEALLOCATE(SCRa)
            IF ( ASSOCIATED(SCRb) ) DEALLOCATE(SCRb)
            ALLOCATE(SCRa(west_east_dim,south_north_dim,bottom_top_dim))
            ALLOCATE(SCRb(west_east_dim,south_north_dim,bottom_top_dim))
            SCRa = 0.0
            SCRb = 0.0
            CALL calc_cape(SCRa, SCRb, cname, cdesc, cunits, 1) 

            IF ( INDEX(plot_these_fields,",cape,") /= 0 ) THEN
              cname = "cape"
              cdesc = "CAPE"
              cunits = "J/kg"
              CALL interp (SCRa, west_east_dim, south_north_dim, bottom_top_dim, &
                           data_out, nxout, nyout, nzout, &
                           vert_array, interp_levels, number_of_zlevs,cname) 
              CALL write_dat (data_out, nxout, nyout, nzout, cname, cdesc, cunits)
            END IF
            IF ( INDEX(plot_these_fields,",cin,") /= 0) THEN
              cname = "cin"
              cdesc = "CIN"
              cunits = "J/kg"
              CALL interp (SCRb, west_east_dim, south_north_dim, bottom_top_dim, &
                           data_out, nxout, nyout, nzout, &
                           vert_array, interp_levels, number_of_zlevs,cname) 
              CALL write_dat (data_out, nxout, nyout, nzout, cname, cdesc, cunits)
            END IF
            DEALLOCATE(SCRa)
            DEALLOCATE(SCRb)

          END IF
        END IF



!!! 2D CAPE/CIN & LCL/LFC
        IF ( INDEX(plot_these_fields,",mcape,") /= 0 .OR. &
             INDEX(plot_these_fields,",mcin,")  /= 0 .OR.  &
             INDEX(plot_these_fields,",lcl,")   /= 0 .OR.   &
             INDEX(plot_these_fields,",lfc,")   /= 0) THEN
          good_to_go = 0
          IF ( .not. have_QV )   CALL get_keep_array ( valid_date, good_to_go, "QVAPOR" )
          IF ( .not. have_HGT )  CALL get_keep_array ( valid_date, good_to_go, "HGT" )
          IF ( .not. have_PSFC ) CALL get_keep_array ( valid_date, good_to_go, "PSFC" )
          IF ( good_to_go == 0 .AND. have_TK .AND. have_PRES .AND. have_GEOPT ) THEN 

            IF ( ASSOCIATED(SCRa) ) DEALLOCATE(SCRa)
            IF ( ASSOCIATED(SCRb) ) DEALLOCATE(SCRb)
            ALLOCATE(SCRa(west_east_dim,south_north_dim,bottom_top_dim))
            ALLOCATE(SCRb(west_east_dim,south_north_dim,bottom_top_dim))
            SCRa = 0.0
            SCRb = 0.0
            CALL calc_cape(SCRa, SCRb, cname, cdesc, cunits, 0) 

            ALLOCATE(SCR(west_east_dim,south_north_dim,1))
            IF ( INDEX(plot_these_fields,",mcape,") /= 0 ) THEN
              cname = "mcape"
              cdesc = "MCAPE"
              cunits = "J/kg"
              SCR(:,:,1) = SCRa(:,:,1)
              CALL interp (SCR, west_east_dim, south_north_dim, 1, &
                           data_out, nxout, nyout, nzout, &
                           vert_array, interp_levels, number_of_zlevs,cname) 
              CALL write_dat (data_out, nxout, nyout, nzout, cname, cdesc, cunits)
            END IF
            IF ( INDEX(plot_these_fields,",mcin,") /= 0) THEN
              cname = "mcin"
              cdesc = "MCIN"
              cunits = "J/kg"
              SCR(:,:,1) = SCRa(:,:,2)
              CALL interp (SCR, west_east_dim, south_north_dim, 1, &
                           data_out, nxout, nyout, nzout, &
                           vert_array, interp_levels, number_of_zlevs,cname) 
              CALL write_dat (data_out, nxout, nyout, nzout, cname, cdesc, cunits)
            END IF
            IF ( INDEX(plot_these_fields,",lcl,") /= 0) THEN
              cname = "lcl"
              cdesc = "LCL"
              cunits = "meters AGL"
              SCR(:,:,1) = SCRa(:,:,3)
              CALL interp (SCR, west_east_dim, south_north_dim, 1, &
                           data_out, nxout, nyout, nzout, &
                           vert_array, interp_levels, number_of_zlevs,cname) 
              CALL write_dat (data_out, nxout, nyout, nzout, cname, cdesc, cunits)
            END IF
            IF ( INDEX(plot_these_fields,",lfc,") /= 0) THEN
              cname = "lfc"
              cdesc = "LFC"
              cunits = "meters AGL"
              SCR(:,:,1) = SCRa(:,:,4)
              CALL interp (SCR, west_east_dim, south_north_dim, 1, &
                           data_out, nxout, nyout, nzout, &
                           vert_array, interp_levels, number_of_zlevs,cname) 
              CALL write_dat (data_out, nxout, nyout, nzout, cname, cdesc, cunits)
            END IF
            DEALLOCATE(SCR)
            DEALLOCATE(SCRa)
            DEALLOCATE(SCRb)

          END IF
        END IF


       IF ( ASSOCIATED(SCR) ) DEALLOCATE(SCR)

   END SUBROUTINE process_diagnostics

END MODULE module_diagnostics
