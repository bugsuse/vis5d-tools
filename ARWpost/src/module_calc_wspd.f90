!! Diagnostics: Wind Speed

MODULE module_calc_wspd

  CONTAINS
  SUBROUTINE calc_wspd(SCR, cname, cdesc, cunits, i3dflag)

  USE module_model_basics

  !Arguments
  real, pointer, dimension(:,:,:)                 :: SCR
  character (len=128)                             :: cname, cdesc, cunits

  !Local Variables
  integer                                         :: i3dflag



  IF ( i3dflag == 1 ) THEN
    SCR      = SQRT(UUU*UUU + VVV*VVV)
    cname    = "wspd"
    cdesc    = "Wind Speed"
  ELSE
    SCR(:,:,1)      = SQRT(U10(:,:)*U10(:,:) + V10(:,:)*V10(:,:))
    cname    = "ws10"
    cdesc    = "Wind Speed at 10 M"
  ENDIF

  cunits   = "m s-1"
  
  END SUBROUTINE calc_wspd

END MODULE module_calc_wspd

