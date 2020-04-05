!! Diagnostics: low / mid / high cloud fraction
!! Code originally from MM5toGrADS

MODULE module_calc_clfr

  CONTAINS
  SUBROUTINE calc_clfr(SCRa, SCRb, SCRc, cname, cdesc, cunits, RH_in)

  USE module_model_basics

  IMPLICIT NONE

  !Arguments
  real, pointer, dimension(:,:,:)                :: SCRa, SCRb, SCRc
  real, pointer, dimension(:,:,:)                :: RH_in
  character (len=128)                            :: cname, cdesc, cunits

  ! Local variables
  integer                                        :: i, j, k, kclo, kcmi, kchi

  
   DO j = 1,south_north_dim   
     DO i = 1,west_east_dim
       DO k = 1,bottom_top_dim
          IF ( PRES(i,j,k) .gt. 97000. ) kclo=k
          IF ( PRES(i,j,k) .gt. 80000. ) kcmi=k
          IF ( PRES(i,j,k) .gt. 45000. ) kchi=k
       END DO
       DO k = 1,bottom_top_dim
          IF ( k .ge. kclo .AND. k .lt. kcmi ) &               !! low cloud
               SCRa(i,j,1) = AMAX1(RH_in(i,j,k),SCRa(i,j,1))
          IF ( k .ge. kcmi .AND. k .lt. kchi ) &               !! mid cloud
               SCRb(i,j,1) = AMAX1(RH_in(i,j,k),SCRb(i,j,1))
          IF ( k .ge. kchi ) &                                 !! high cloud
               SCRc(i,j,1) = AMAX1(RH_in(i,j,k),SCRc(i,j,1))
       END DO

       SCRa(i,j,1)=4.0*SCRa(i,j,1)/100.-3.0
       SCRb(i,j,1)=4.0*SCRb(i,j,1)/100.-3.0
       SCRc(i,j,1)=2.5*SCRc(i,j,1)/100.-1.5

       SCRa(i,j,1)=amin1(SCRa(i,j,1),1.0)
       SCRa(i,j,1)=amax1(SCRa(i,j,1),0.0)
       SCRb(i,j,1)=amin1(SCRb(i,j,1),1.0)
       SCRb(i,j,1)=amax1(SCRb(i,j,1),0.0)
       SCRc(i,j,1)=amin1(SCRc(i,j,1),1.0)
       SCRc(i,j,1)=amax1(SCRc(i,j,1),0.0)

     END DO 
   END DO 


  cname    = " "
  cdesc    = " "
  cunits   = "%"
  

  END SUBROUTINE calc_clfr

END MODULE module_calc_clfr
