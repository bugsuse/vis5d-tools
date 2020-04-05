MODULE module_basic_arrays

  USE gridinfo_module
  USE module_arrays
  USE module_pressure
  USE module_interp
  USE constants_module


  CONTAINS

   SUBROUTINE process_basic_arrays ( valid_date )

      IMPLICIT NONE

      ! Local variables
      character (len=128)                    :: cname, cunits, cdesc, c_nm
      real, pointer, dimension(:,:,:)        :: SCR3
      character (len=19)                     :: valid_date
      integer                                :: good_to_go 



!!! If we don't have PRES (Pa) already - calculate and keep
      IF ( .not. have_PRES ) THEN 

        IF ( iprogram >= 6 ) THEN   
          good_to_go = 0
          IF ( .not. have_P  ) CALL get_keep_array ( valid_date, good_to_go, "P" )
          IF ( .not. have_PB ) CALL get_keep_array ( valid_date, good_to_go, "PB" )
          IF ( good_to_go == 0 ) THEN
            ALLOCATE(SCR3(west_east_dim,south_north_dim,bottom_top_dim))
            SCR3 = P + PB
            c_nm = 'PRES'
            CALL keep_arrays(c_nm, SCR3)
            DEALLOCATE(SCR3)
          END IF
        END IF

        IF ( .not. have_PRES .AND. iprogram == 6 ) THEN   ! probably old wrfinput 
          good_to_go = 0
          IF ( .not. have_MU   ) CALL get_keep_array ( valid_date, good_to_go, "MU" )
          IF ( .not. have_MUB  ) CALL get_keep_array ( valid_date, good_to_go, "MUB" )
          IF ( .not. have_ZNU  ) CALL get_keep_array ( valid_date, good_to_go, "ZNU" )
          IF ( .not. have_ZNW  ) CALL get_keep_array ( valid_date, good_to_go, "ZNW" )
          IF ( .not. have_PTOP ) CALL get_keep_array ( valid_date, good_to_go, "P_TOP" )
          IF ( .not. have_QV   ) CALL get_keep_array ( valid_date, good_to_go, "QVAPOR" )
          IF ( good_to_go == 0 ) THEN
            CALL pressure(SCR3)
            c_nm = 'PRES'
            CALL keep_arrays(c_nm, SCR3)
            DEALLOCATE(SCR3)
          END IF
        END IF

        IF ( iprogram == 3 ) THEN 
          good_to_go = 0
          CALL get_keep_array ( valid_date, good_to_go, "PRES" )
        END IF

      END IF


!!! If we don't have TK (K) already - calculate and keep 
      IF ( .not. have_TK ) THEN 

        IF ( iprogram >= 6 ) THEN 
          good_to_go = 0
          IF ( .not. have_T  ) CALL get_keep_array ( valid_date, good_to_go, "T" )
          IF ( good_to_go == 0 .AND. have_PRES ) THEN
            ALLOCATE(SCR3(west_east_dim,south_north_dim,bottom_top_dim))
            c_nm = 'TK'
            IF (io_form_input == NETCDF) THEN
              SCR3 = (T+300.) * ( PRES / p0 )**RCP  !! GRIB files have this added already
            ELSE
              SCR3 = T * ( PRES / p0 )**RCP
            ENDIF
            CALL keep_arrays(c_nm, SCR3)
            DEALLOCATE(SCR3)
          END IF
        END IF

        IF ( iprogram == 3 ) THEN 
          good_to_go = 0
          CALL get_keep_array ( valid_date, good_to_go, "TT" )
        END IF

      END IF


!!! If we don't have GEOPT (m2/s2) already - calculate and keep 
      IF ( .not. have_GEOPT ) THEN 

        IF ( iprogram >= 6 ) THEN   
          good_to_go = 0
          IF ( .not. have_PH  ) CALL get_keep_array ( valid_date, good_to_go, "PH" )
          IF ( .not. have_PHB ) CALL get_keep_array ( valid_date, good_to_go, "PHB" )
          IF ( good_to_go == 0 ) THEN
            ALLOCATE(SCR3(west_east_dim,south_north_dim,bottom_top_dim))
            SCR3 = PH + PHB
            c_nm = 'GEOPT'
            CALL keep_arrays(c_nm, SCR3)
            DEALLOCATE(SCR3)
          END IF
        END IF

        IF ( iprogram == 3 ) THEN 
          good_to_go = 0
          CALL get_keep_array ( valid_date, good_to_go, "GHT" )
        END IF


      END IF

!!! get and keep a couple of other arrays
      IF ( .not. have_QV    ) CALL get_keep_array ( valid_date, good_to_go, "QVAPOR" )
      IF ( .not. have_PSFC  ) CALL get_keep_array ( valid_date, good_to_go, "PSFC" )
      IF ( .not. have_HGT   ) CALL get_keep_array ( valid_date, good_to_go, "HGT", "HGT_M" )
      IF ( .not. have_XLAT  ) CALL get_keep_array ( valid_date, good_to_go, "XLAT", "XLAT_M" )
      IF ( .not. have_XLONG ) CALL get_keep_array ( valid_date, good_to_go, "XLONG", "XLONG_M" )
      IF ( .not. have_PTOP  ) CALL get_keep_array ( valid_date, good_to_go, "P_TOP" )


   END SUBROUTINE process_basic_arrays

END MODULE module_basic_arrays
