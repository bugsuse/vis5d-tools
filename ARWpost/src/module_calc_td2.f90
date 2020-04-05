!! Diagnostics: Dewpoint Temperature at 2m

MODULE module_calc_td2

  CONTAINS
  SUBROUTINE calc_td2(SCR, cname, cdesc, cunits)

  USE module_model_basics
  USE constants_module

  !Arguments
  real, pointer, dimension(:,:,:)                 :: SCR
  character (len=128)                             :: cname, cdesc, cunits

  !Local
  real, dimension(west_east_dim,south_north_dim)  :: tmp1, tmp2, e
  

  ! First get RH2
  tmp1     = 10.*0.6112*exp(17.67*(T2-T0)/(T2-29.65))
  tmp2     = EPS*tmp1/(0.01 * PSFC -  (1.-EPS)*tmp1)
  tmp1     = AMAX1(AMIN1(Q2/tmp2,1.0),0.0)

  e = tmp1 * 0.6112*exp(17.67*(T2-T0)/(T2-29.65))     
  tmp1 = (116.9+ 237.3*alog(e)) / (16.78-alog(e))


  SCR(:,:,1) = tmp1(:,:)
  cname    = "td2"
  cdesc    = "Dewpoint Temperature at 2m"
  cunits   = "C"
  
  END SUBROUTINE calc_td2

END MODULE module_calc_td2
