!! Diagnostics: Dewpoint Temperature

MODULE module_calc_td

  CONTAINS
  SUBROUTINE calc_td(SCR, cname, cdesc, cunits)

  USE module_model_basics

  !Arguments
  real, pointer, dimension(:,:,:)                                :: SCR
  character (len=128)                                            :: cname, cdesc, cunits

  !Local
  real, dimension(west_east_dim,south_north_dim,bottom_top_dim)  :: tmp
  

  tmp      = QV*( PRES /100.)/(0.622+QV)
  tmp      = AMAX1(tmp,0.001)

  SCR      = (243.5*log(tmp)-440.8)/(19.48-log(tmp))
  cname    = "td"
  cdesc    = "Dewpoint Temperature"
  cunits   = "C"
  
  END SUBROUTINE calc_td

END MODULE module_calc_td
