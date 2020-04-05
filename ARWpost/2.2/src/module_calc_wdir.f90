!! Diagnostics: Wind Direction

MODULE module_calc_wdir

  CONTAINS
  SUBROUTINE calc_wdir(SCR, cname, cdesc, cunits, i3dflag)

  USE constants_module
  USE module_model_basics

  !Arguments
  real, pointer, dimension(:,:,:)                 :: SCR
  character (len=128)                             :: cname, cdesc, cunits

  !Local
  integer                                         :: i3dflag
  

  IF ( i3dflag == 1 ) THEN
    SCR      = 270. - ATAN2(VVV,UUU) * DEG_PER_RAD
    cname    = "wdir"
    cdesc    = "Wind Direction"
  ELSE
    SCR(:,:,1)      = 270. - ATAN2(V10(:,:),U10(:,:)) * DEG_PER_RAD
    cname    = "wd10"
    cdesc    = "Wind Direction at 10 M"
  ENDIF
  
  WHERE (SCR .gt. 360. ) 
    SCR    = SCR - 360.
  END WHERE

  cunits   = "Degrees"
  
  END SUBROUTINE calc_wdir

END MODULE module_calc_wdir

