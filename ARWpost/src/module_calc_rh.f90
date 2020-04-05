!! Diagnostics: Relative Humidity

MODULE module_calc_rh

  CONTAINS
  SUBROUTINE calc_rh(SCR, cname, cdesc, cunits)

  USE constants_module
  USE module_model_basics

  !Arguments
  real, pointer, dimension(:,:,:)                                :: SCR
  character (len=128)                                            :: cname, cdesc, cunits

  !Local
  real, dimension(west_east_dim,south_north_dim,bottom_top_dim)  :: tmp1, tmp2  
  

  tmp1     = 10.*0.6112*exp(17.67*(TK-T0)/(TK-29.65))
  tmp2     = EPS*tmp1/(0.01 * PRES -  (1.-EPS)*tmp1)
  tmp1     = 100.*AMAX1(AMIN1(QV/tmp2,1.0),0.0)

  SCR      = tmp1
  cname    = "rh"
  cdesc    = "Relative Humidity"
  cunits   = "%"
  
  END SUBROUTINE calc_rh

END MODULE module_calc_rh
