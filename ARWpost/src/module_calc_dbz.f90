!! Diagnostics: Reflectivity
!! This routine - including comments were taken from the RIP4 program
!! The only change was the removing of the old "ivarint=0" option

MODULE module_calc_dbz

  CONTAINS
  SUBROUTINE calc_dbz (SCR, cname, cdesc, cunits)

!
!     This routine computes equivalent reflectivity factor (in dBZ) at
!     each model grid point.  In calculating Ze, the RIP algorithm makes
!     assumptions consistent with those made in an early version
!     (ca. 1996) of the bulk mixed-phase microphysical scheme in the MM5
!     model (i.e., the scheme known as "Resiner-2").  For each species:
!
!     1. Particles are assumed to be spheres of constant density.  The
!     densities of rain drops, snow particles, and graupel particles are
!     taken to be rho_r = rho_l = 1000 kg m^-3, rho_s = 100 kg m^-3, and
!     rho_g = 400 kg m^-3, respectively. (l refers to the density of
!     liquid water.)
!
!     2. The size distribution (in terms of the actual diameter of the
!     particles, rather than the melted diameter or the equivalent solid
!     ice sphere diameter) is assumed to follow an exponential
!     distribution of the form N(D) = N_0 * exp( lambda*D ).
!
!     3. If ivarint=0, the intercept parameters are assumed constant (as
!     in early Reisner-2), with values of 8x10^6, 2x10^7, and 4x10^6 m^-4,
!     for rain, snow, and graupel, respectively.  If ivarint=1, variable
!     intercept parameters are used, as calculated in Thompson, Rasmussen,
!     and Manning (2004, Monthly Weather Review, Vol. 132, No. 2, pp. 519-542.)
!
!     More information on the derivation of simulated reflectivity in RIP
!     can be found in Stoelinga (2005, unpublished write-up).  Contact
!     Mark Stoelinga (stoeling@atmos.washington.edu) for a copy.
!

  USE constants_module
  USE module_model_basics

  IMPLICIT NONE

  !Arguments
  real, pointer, dimension(:,:,:)                                :: SCR
  character (len=128)                                            :: cname, cdesc, cunits

  !Local Variables
  integer                                                        :: i, j, k
  real                                                           :: temp_c
  real                                                           :: gonv, ronv, sonv
  real                                                           :: factor_g, factor_r, factor_s
  real                                                           :: factorb_g, factorb_r, factorb_s
  real                                                           :: rhoair, z_e
  real, dimension(west_east_dim,south_north_dim,bottom_top_dim)  :: qvp, qra, qsn, qgr
  real, dimension(west_east_dim,south_north_dim,bottom_top_dim)  :: tmk, prs

!   Constants used to calculate variable intercepts
  real, parameter                                                :: r1 = 1.e-15
  real, parameter                                                :: ron = 8.e6
  real, parameter                                                :: ron2 = 1.e10
  real, parameter                                                :: son = 2.e7
  real, parameter                                                :: gon = 5.e7
  real, parameter                                                :: ron_min = 8.e6
  real, parameter                                                :: ron_qr0 = 0.00010
  real, parameter                                                :: ron_delqr0 = 0.25*ron_qr0
  real, parameter                                                :: ron_const1r = (ron2-ron_min)*0.5
  real, parameter                                                :: ron_const2r = (ron2+ron_min)*0.5
!   Other constants
  real, parameter                                                :: gamma_seven = 720.
  real, parameter                                                :: rho_r = RHOWAT           ! 1000. kg m^-3
  real, parameter                                                :: rho_s = 100.             ! kg m^-3
  real, parameter                                                :: rho_g = 400.             ! kg m^-3
  real, parameter                                                :: alpha = 0.224

  ALLOCATE(SCR(west_east_dim,south_north_dim,bottom_top_dim))


      prs      = PRES * 0.01                  ! pressure in hPa
      tmk      = TK                           ! temperature in K

      qvp = QV
      qra = QR
      qsn = 0.0
      qgr = 0.0
      IF ( have_QS ) qsn = QS
      IF ( have_QG ) qgr = QG
      IF ( maxval(qsn) == 0.0 .AND. maxval(qgr) == 0.0 ) THEN
        !! No ice in this run
        WHERE ( tmk .lt. CELKEL )
          qra = 0.0
          qsn = qr
        END WHERE
      END IF

      qvp = MAX(qvp, 0.0)
      qra = MAX(qra, 0.0)
      qsn = MAX(qsn, 0.0)
      qgr = MAX(qgr, 0.0)
        
      factor_r = gamma_seven * 1.e18 * (1./(PI*rho_r))**1.75
      factor_s = gamma_seven * 1.e18 * (1./(PI*rho_s))**1.75   &
                    * (rho_s/RHOWAT)**2 * alpha
      factor_g = gamma_seven * 1.e18 * (1./(PI*rho_g))**1.75   &
                    * (rho_g/RHOWAT)**2 * alpha
 
      DO k = 1,bottom_top_dim
        DO j = 1,south_north_dim
          DO i = 1,west_east_dim
 
            rhoair=prs(i,j,k)*100./ ( Rd*virtual(tmk(i,j,k),qvp(i,j,k)) )  ! air density
 
!      Adjust factor for brightband, where snow or graupel particle
!      scatters like liquid water (alpha=1.0) because it is assumed to
!      have a liquid skin.
 
            IF (tmk(i,j,k) .gt. CELKEL) THEN
              factorb_s=factor_s/alpha
              factorb_g=factor_g/alpha
            ELSE
              factorb_s=factor_s
              factorb_g=factor_g
            ENDIF
 
!      Calculate variable intercept parameters
 
            temp_c = amin1(-0.001, tmk(i,j,k)-CELKEL)
            sonv   = amin1(2.0e8, 2.0e6*exp(-0.12*temp_c))
 
            gonv = gon
            IF (qgr(i,j,k).gt.r1) THEN
              gonv = 2.38*(PI*rho_g/(rhoair*qgr(i,j,k)))**0.92
              gonv = max(1.e4, min(gonv,gon))
            ENDIF
 
            ronv = ron2
            IF (qra(i,j,k).gt. r1) THEN
               ronv = ron_const1r*tanh((ron_qr0-qra(i,j,k))     &
                      /ron_delqr0) + ron_const2r
            ENDIF
 
!      Total equivalent reflectivity factor (z_e, in mm^6 m^-3) is
!      the sum of z_e for each hydrometeor species:
 
            z_e =   factor_r  * (rhoair*qra(i,j,k))**1.75 /ronv**.75   &
                  + factorb_s * (rhoair*qsn(i,j,k))**1.75 /sonv**.75   &
                  + factorb_g * (rhoair*qgr(i,j,k))**1.75 /gonv**.75      

!      Adjust small values of Z_e so that dBZ is no lower than -30
            z_e = max(z_e,.001)

!      Convert to dBZ
            SCR(i,j,k) = 10. * log10(z_e)
 
          ENDDO
        ENDDO
      ENDDO


  cname    = "dbz"
  cdesc    = "Reflectivity"
  cunits   = "-"
 
  END SUBROUTINE calc_dbz

END MODULE module_calc_dbz

