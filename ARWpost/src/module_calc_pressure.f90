!! Diagnostics: Model Pressure in hPa

MODULE module_calc_pressure

  CONTAINS
  SUBROUTINE calc_pressure(SCR, cname, cdesc, cunits)

  USE module_model_basics

  !Arguments
  real, pointer, dimension(:,:,:)       :: SCR
  character (len=128)                   :: cname, cdesc, cunits
   

  SCR      = PRES * 0.01
  cname    = "pressure"
  cdesc    = "Model pressure"
  cunits   = "hPa"
  
  END SUBROUTINE calc_pressure

END MODULE module_calc_pressure
