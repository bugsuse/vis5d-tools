!! Diagnostics: Relative Humidity at 2m

MODULE module_calc_rh2

  CONTAINS
  SUBROUTINE calc_rh2(SCR, cname, cdesc, cunits)

  USE constants_module
  USE module_model_basics

  !Arguments
  real, pointer, dimension(:,:,:)                 :: SCR
  character (len=128)                             :: cname, cdesc, cunits

  !Local
  real, dimension(west_east_dim,south_north_dim)  :: tmp1, tmp2  
  

  tmp1     = 10.*0.6112*exp(17.67*(T2-T0)/(T2-29.65))
  tmp2     = EPS*tmp1/(0.01 * PSFC -  (1.-EPS)*tmp1)
  tmp1     = 100.*AMAX1(AMIN1(Q2/tmp2,1.0),0.0)

  SCR(:,:,1) = tmp1(:,:)
  cname      = "rh2"
  cdesc      = "Relative Humidity at 2m"
  cunits     = "%"
  
  END SUBROUTINE calc_rh2

END MODULE module_calc_rh2
