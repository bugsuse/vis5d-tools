!! Diagnostics: Temperature (C)

MODULE module_calc_tc

  CONTAINS
  SUBROUTINE calc_tc(SCR, cname, cdesc, cunits)

  USE constants_module
  USE module_model_basics

  !Arguments
  real, pointer, dimension(:,:,:)       :: SCR
  character (len=128)                   :: cname, cdesc, cunits
  

  SCR      = TK - T0
  cname    = "tc"
  cdesc    = "Temperature"
  cunits   = "C"
  
  END SUBROUTINE calc_tc

END MODULE module_calc_tc
