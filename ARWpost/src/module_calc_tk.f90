!! Diagnostics: Temperature (Kelvin)

MODULE module_calc_tk

  CONTAINS
  SUBROUTINE calc_tk(SCR, cname, cdesc, cunits)

  USE constants_module
  USE module_model_basics

  !Arguments
  real, pointer, dimension(:,:,:)       :: SCR
  character (len=128)                   :: cname, cdesc, cunits
  

  SCR      = TK
  cname    = "tk"
  cdesc    = "Temperature"
  cunits   = "K"
  
  END SUBROUTINE calc_tk

END MODULE module_calc_tk
