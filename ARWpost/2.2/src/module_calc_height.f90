!! Diagnostics: Height

MODULE module_calc_height

  CONTAINS
  SUBROUTINE calc_height(SCR, cname, cdesc, cunits)

  USE constants_module
  USE module_model_basics

  !Arguments
  real, pointer, dimension(:,:,:)       :: SCR
  character (len=128)                   :: cname, cdesc, cunits


  IF ( trim(cname) == "height" ) THEN
  
    SCR      = ( GEOPT / G ) / 1000.
    cname    = "height"
    cdesc    = "Model height"
    cunits   = "km"

  ELSE
  
    SCR      = GEOPT
    cname    = "geopt"
    cdesc    = "Geopotential"
    cunits   = "m2/s2"

  END IF
  
  END SUBROUTINE calc_height

END MODULE module_calc_height
