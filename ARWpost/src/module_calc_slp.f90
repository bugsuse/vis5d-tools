!! Diagnostics: Sea Level Pressure

MODULE module_calc_slp

  CONTAINS
  SUBROUTINE calc_slp(SCR, cname, cdesc, cunits)

  USE constants_module
  USE module_model_basics

  !Arguments
  real, pointer, dimension(:,:,:)                                :: SCR
  character (len=128)                                            :: cname, cdesc, cunits

  !Local
  real, dimension(west_east_dim,south_north_dim,bottom_top_dim)  :: temp, p_tmp, z
  integer, dimension(west_east_dim,south_north_dim)              :: level
  real, dimension(west_east_dim,south_north_dim)                 :: slp
  real, dimension(west_east_dim,south_north_dim)                 :: t_surf, t_sea_level

  real, parameter                                                :: TC=273.16+17.5
  real, parameter                                                :: PCONST = 10000.
  logical, parameter                                             :: traditional_comp = .TRUE.

  integer                                                        :: i, j, k
  integer                                                        :: klo, khi
  real                                                           :: plo, phi, tlo, thi, zlo, zhi
  real                                                           :: p_at_pconst, t_at_pconst, z_at_pconst
  real                                                           :: z_half_lowest
  logical                                                        :: l1, l2, l3, found



  p_tmp    = PRES                  ! Pressure in Pa
  z        = GEOPT/G
  temp     = TK                    ! Temp in K


!     Find least zeta level that is PCONST Pa above the surface.  We later use this
!     level to extrapolate a surface pressure and temperature, which is supposed
!     to reduce the effect of the diurnal heating cycle in the pressure field.

  DO j = 1 , south_north_dim
    DO i = 1 , west_east_dim

      level(i,j) = -1
      k = 1
      found = .FALSE.
      DO WHILE( (.not. found) .and. (k.le.bottom_top_dim) )
        IF ( p_tmp(i,j,k) .LT. p_tmp(i,j,1)-PCONST ) THEN
          level(i,j) = k
          found = .TRUE.
        END IF
        k = k+1
      END DO

      IF ( level(i,j) .EQ. -1 ) THEN
        PRINT '(A,I4,A)','Troubles finding level ',   &
                    NINT(PCONST)/100,' above ground.'
        PRINT '(A,I4,A,I4,A)',                        &
              'Problems first occur at (',i,',',j,')'
        PRINT '(A,F6.1,A)',                           &
              'Surface pressure = ',p_tmp(i,j,1)/100,' hPa.'
        STOP 'Error_in_finding_100_hPa_up'
      END IF

    END DO
  END DO


!     Get temperature PCONST Pa above surface.  Use this to extrapolate
!     the temperature at the surface and down to sea level.

  DO j = 1 , south_north_dim
    DO i = 1 , west_east_dim

      klo = MAX ( level(i,j) - 1 , 1      )
      khi = MIN ( klo + 1        , bottom_top_dim - 1 )

      IF ( klo .EQ. khi ) THEN
         PRINT '(A)','Trapping levels are weird.'
         PRINT '(A,I3,A,I3,A)','klo = ',klo,', khi = ',khi, &
                      ': and they should not be equal.'
         STOP 'Error_trapping_levels'
      END IF

      plo = p_tmp(i,j,klo)
      phi = p_tmp(i,j,khi)
      tlo = temp(i,j,klo)*(1. + 0.608 * qv(i,j,klo) )
      thi = temp(i,j,khi)*(1. + 0.608 * qv(i,j,khi) )
      zlo = z(i,j,klo)
      zhi = z(i,j,khi)

      p_at_pconst = p_tmp(i,j,1) - pconst
      t_at_pconst = thi-(thi-tlo)*LOG(p_at_pconst/phi)*LOG(plo/phi)
      z_at_pconst = zhi-(zhi-zlo)*LOG(p_at_pconst/phi)*LOG(plo/phi)

      t_surf(i,j) = t_at_pconst*(p_tmp(i,j,1)/p_at_pconst)**(GAMMA*Rd/G)
      t_sea_level(i,j) = t_at_pconst+GAMMA*z_at_pconst

    END DO
  END DO


!     If we follow a traditional computation, there is a correction to the sea level
!     temperature if both the surface and sea level temperatures are *too* hot.

  IF ( traditional_comp ) THEN
    DO j = 1 , south_north_dim
      DO i = 1 , west_east_dim
        l1 = t_sea_level(i,j) .LT. TC
        l2 = t_surf     (i,j) .LE. TC
        l3 = .NOT. l1
        IF ( l2 .AND. l3 ) THEN
          t_sea_level(i,j) = TC
        ELSE
          t_sea_level(i,j) = TC - 0.005*(t_surf(i,j)-TC)**2
        END IF
      END DO
    END DO
  END IF


!     The grand finale

  DO j = 1 , south_north_dim
    DO i = 1 , west_east_dim
      z_half_lowest=z(i,j,1)
      slp(i,j) = p_tmp(i,j,1) *              &
                            EXP((2.*G*z_half_lowest)/   &
                            (Rd*(t_sea_level(i,j)+t_surf(i,j))))
      slp(i,j) = slp(i,j)*0.01

    END DO
  END DO


  SCR(:,:,1) = slp(:,:)
  cname      = "slp"
  cdesc      = "Sea Levelp Pressure"
  cunits     = "hPa"

  END SUBROUTINE calc_slp

END MODULE module_calc_slp

