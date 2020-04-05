!! Calculate pressure (Pa) from MU and MUB (wrfinput data)

MODULE module_pressure

  CONTAINS
  SUBROUTINE pressure(prs) 

  USE module_model_basics

  !Arguments
  real, pointer, dimension(:,:,:)       :: prs
  real, dimension(bottom_top_dim)       :: rdnw, rdn
  real                                  :: dnw, dn
  real                                  :: qvf1, qvf2
  real                                  :: p_base
  integer                               :: i, j, k
   

  ALLOCATE(prs(west_east_dim,south_north_dim,bottom_top_dim))



     DO k = 1, bottom_top_dim
        dnw=(ZNW(k+1) - ZNW(k))
        rdnw(k) = 1./dnw
     END DO
     DO k = 1, bottom_top_dim
        dn=.5 * ( 1./rdnw(k+1) + 1./rdnw(k))
        rdn(k)=1./dn
     END DO



     DO j = 1, south_north_dim
     DO i = 1, west_east_dim

!      Get pressure perturbation at model top
       k = bottom_top_dim  
       qvf1 = QV(i,j,k) * .001
       qvf2 = 1. / (1.+qvf1)
       qvf1 = qvf1 * qvf2
       prs(i,j,k) = - 0.5 * ( MU(i,j) + qvf1*MUB(i,j) ) / rdnw(k) / qvf2


!      Now get pressure perturbation at levels below
       DO k = 1, bottom_top_dim-1
          qvf1 = 0.5 * (QV(i,j,k)+QV(i,j,k+1)) * .001
          qvf2 = 1. / (1.+qvf1)
          qvf1 = qvf1 * qvf2
          prs(i,j,k) = prs(i,j,k+1) - ( MU(i,j) + qvf1*MUB(i,j) ) / qvf2 / rdn(k)
       END DO


!      Finally compute base state pressure and add to pressure perturbation
!      to get total pressure
       DO k = 1, bottom_top_dim
          p_base = ZNU(k) * MUB(i,j) + PTOP
          prs(i,j,k) =  prs(i,j,k) + p_base     ! Pa 
       END DO

     END DO
     END DO


  END SUBROUTINE pressure

END MODULE module_pressure
