!! Diagnostics: Potential Temperature

MODULE module_calc_theta

  CONTAINS
  SUBROUTINE calc_theta(SCR, cname, cdesc, cunits)

  USE module_model_basics

  !Arguments
  real, pointer, dimension(:,:,:)       :: SCR
  character (len=128)                   :: cname, cdesc, cunits
  

  SCR      = T + 300.
  cname    = "theta"
  cdesc    = "Potential Temperature"
  cunits   = "K"
  
  END SUBROUTINE calc_theta

END MODULE module_calc_theta
