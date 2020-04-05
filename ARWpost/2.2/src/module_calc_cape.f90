!! Diagnostics: CAPE / CIN / LCL / LFC
!! Depend on 2D/3D flag send in
!! Code originally from RIP4

MODULE module_calc_cape

  real, dimension(150)                                           :: buoy, zrel, benaccum
  real, dimension(150)                                           :: psadithte, psadiprs
  real, dimension(150,150)                                       :: psaditmk

  CONTAINS
  SUBROUTINE calc_cape(SCRa, SCRb, cname, cdesc, cunits, i3dflag)

!   If i3dflag=1, this routine calculates CAPE and CIN (in m**2/s**2,
!   or J/kg) for every grid point in the entire 3D domain (treating
!   each grid point as a parcel).  If i3dflag=0, then it
!   calculates CAPE and CIN only for the parcel with max theta-e in
!   the column, (i.e. something akin to Colman's MCAPE).  By "parcel",
!   we mean a 500-m deep parcel, with actual temperature and moisture
!   averaged over that depth.
!
!   In the case of i3dflag=0,
!   MCAPE, MCIN, LCL and LFC (2D fields are calculated)


  USE constants_module
  USE module_model_basics

  IMPLICIT NONE

  !Arguments
  real, pointer, dimension(:,:,:)                                :: SCRa, SCRb
  character (len=128)                                            :: cname, cdesc, cunits
  integer                                                        :: i3dflag

  ! Local variables
  integer                                                        :: i, j, k, kk, jt, ip, iustnlist
  integer                                                        :: kpar, kpar1, kpar2, kmax, klev, kel
  integer                                                        :: ilcl, klcl, klfc
  integer                                                        :: nprs, nthte
  real, dimension(west_east_dim,south_north_dim)                 :: ter
  real, dimension(west_east_dim,south_north_dim,bottom_top_dim)  :: prs, tmk, ght, qvp
  real, dimension(west_east_dim,south_north_dim,bottom_top_dim)  :: prsf, cape, cin
  real                                                           :: ethpari, zlcl, tvenv
  real                                                           :: p1, p2, pp1, pp2, pup, pdn
  real                                                           :: totprs, totqvp, totthe
  real                                                           :: eslift, ghtlift, qvplift, tmklift, tvlift
  real                                                           :: ghtpari, prspari, qvppari, tmkpari
  real                                                           :: tmkenv, qvpenv, tlcl
  real                                                           :: fac1, fac2, facden, th, deltap
  real                                                           :: benamin, davg, pavg, pressure, temp
  real                                                           :: e, eth, ethmax, q, dz, cpm
  character (len=20)                                             :: fname
  logical                                                        :: is_used

  
     ! Open psadilookup.dat     
     DO iustnlist = 10,100
        INQUIRE(unit=iustnlist, opened=is_used)
        if (.not. is_used) exit
     END DO
     fname = 'src/psadilookup.dat' 
     OPEN (unit=iustnlist,file=fname,form='formatted',status='old')

     DO i = 1,14
       READ (iustnlist,*)
     ENDDO
     READ (iustnlist,*) nthte,nprs
     IF ( nthte.ne.150 .OR. nprs.ne.150 ) then
       PRINT*, 'Number of pressure or theta_e levels in lookup table'
       PRINT*, '     file not = 150.  Check lookup table file.'
       STOP
     ENDIF
     READ (iustnlist,173) (psadithte(jt),jt=1,nthte) 
     READ (iustnlist,173) (psadiprs(ip),ip=1,nprs)
     READ (iustnlist,173) ((psaditmk(ip,jt),ip=1,nprs),jt=1,nthte)
 173 FORMAT (5e15.7)
     CLOSE (iustnlist)  


  !! Get fields we want from the ones we have
     ter      = HGT
     qvp      = QV
     prs      = PRES * 0.01                ! pressure in hPa
     tmk      = TK                         ! temperature in K
     ght      = GEOPT / G                  ! height in m


  !! First calculate a pressure array on FULL SIGMA levels
  !! Similar to the pfcalc.f routine from RIP4
  !! Top full sigma level is ommitted
     prsf(:,:,1) = PSFC(:,:)             !! Lowest full sigma set to surface pressure
     DO k = 2, bottom_top_dim
       prsf(:,:,k)=.5*(prs(:,:,k-1)+prs(:,:,k))
     END DO

     
     cape = 0.0
     cin  = 0.0

     DO j = 1,south_north_dim          !! BIG i/j loop
     DO i = 1,west_east_dim

       IF ( i3dflag == 1 ) THEN       !! 3D case

         kpar1=bottom_top_dim-1
         kpar2=1

       ELSE                           !! 2D case
 
!      Find parcel with max theta-e in lowest 3 km AGL.
         ethmax = -1.
         DO k = 1, bottom_top_dim
           IF ( ght(i,j,k)-ter(i,j) .lt. 3000. ) THEN
             q        = max(qvp(i,j,k),1.e-15)
             temp     = tmk(i,j,k)
             pressure = prs(i,j,k)
             e        = (q*pressure)/(EPS+q)
             tlcl     = TLCLC1/(log(temp**TLCLC2/e)-TLCLC3)+TLCLC4
             eth  =     temp*(1000./pressure)**( GAMMA_RIP*(1.+GAMMAMD*q) )*     &
                        exp( (THTECON1/tlcl-THTECON2)*q*(1.+THTECON3*q) )
             IF ( eth .gt. ethmax ) THEN
               klev=k
               ethmax=eth
             END IF
           END IF
         END DO
         kpar1=klev
         kpar2=klev
 
!      Establish average properties of that parcel
!      (over depth of approximately davg meters)
         davg = 500.
         pavg = davg*prs(i,j,kpar1)*G /                        &
                ( Rd*virtual(tmk(i,j,kpar1),qvp(i,j,kpar1)) )
         p2 = min(prs(i,j,kpar1)+.5*pavg,prsf(i,j,1))
         p1 = p2-pavg
         totthe = 0.
         totqvp = 0.
         totprs = 0.
         DO k = 1,bottom_top_dim-1
           IF ( prsf(i,j,k)   .le. p1 ) GOTO 35
           IF ( prsf(i,j,k+1) .ge. p2 ) GOTO 34
           pressure = prs(i,j,k)
           pup      = prsf(i,j,k)
           pdn      = prsf(i,j,k+1)
           q        = max(qvp(i,j,k),1.e-15)
           th       = tmk(i,j,k)*(1000./pressure)**(GAMMA_RIP*(1.+GAMMAMD*q))
           pp1      = max(p1,pdn)
           pp2      = min(p2,pup)
           IF ( pp2 .gt. pp1 ) THEN
             deltap = pp2-pp1
             totqvp = totqvp+q*deltap
             totthe = totthe+th*deltap
             totprs = totprs+deltap
           END IF
 34        CONTINUE
         END DO
 35      CONTINUE
         qvppari = totqvp/totprs
         tmkpari = (totthe/totprs)*(prs(i,j,kpar1)/1000.)**    &
                   (GAMMA_RIP*(1.+GAMMAMD*qvp(i,j,kpar1)))
       ENDIF

       !!!   END of 2D / 3D specific bits 


       DO kpar = kpar1,kpar2,-1                      !! This loop is done for both 2D / 3D
 
!   Calculate temperature and moisture properties of parcel

         IF ( i3dflag == 1 ) THEN    ! (Note, qvppari and tmkpari already calculated above for 2D case.)
           qvppari = qvp(i,j,kpar)
           tmkpari = tmk(i,j,kpar)
         END IF
         prspari = prs(i,j,kpar)
         ghtpari = ght(i,j,kpar)
         cpm     = cp*(1.+CPMD*qvppari)
 
         e       = max(1.e-20,qvppari*prspari/(EPS+qvppari))
         tlcl    = TLCLC1/(log(tmkpari**TLCLC2/e)-TLCLC3)+TLCLC4
         ethpari = tmkpari*(1000./prspari)**(GAMMA_RIP*(1.+GAMMAMD*qvppari))*   &
                   exp((THTECON1/tlcl-THTECON2)*qvppari*                    &
                   (1.+THTECON3*qvppari))
         zlcl    = ghtpari+(tmkpari-tlcl)/(G/cpm)

!   Calculate buoyancy and relative height of lifted parcel at
!   all levels, and store in bottom up arrays.  Add a level at the LCL,
!   and at all points where buoyancy is zero.
 
         kk = 0                    ! for arrays that go bottom to top
         ilcl = 0
         IF ( ghtpari .ge. zlcl ) THEN
           !! initial parcel already saturated or supersaturated.
           ilcl = 2
           klcl = 1
         END IF

         DO k = kpar,bottom_top_dim
 33        kk=kk+1                ! for arrays that go bottom to top

           IF ( ght(i,j,k) .lt. zlcl ) THEN ! model level is below LCL
             qvplift = qvppari
             tmklift = tmkpari-G/cpm*(ght(i,j,k)-ghtpari)
             tvenv   = virtual(tmk(i,j,k),qvp(i,j,k))
             tvlift  = virtual(tmklift,qvplift)
             ghtlift = ght(i,j,k)
           ELSE IF ( ght(i,j,k) .ge. zlcl .AND. ilcl .eq. 0 ) THEN
             !! This model level and previous model level straddle the LCL,
             !! so first create a new level in the bottom-up array, at the LCL.
             tmklift = tlcl
             qvplift = qvppari
             facden  = ght(i,j,k)-ght(i,j,k-1)
             fac1    = (zlcl-ght(i,j,k-1))/facden
             fac2    = (ght(i,j,k)-zlcl)/facden
             tmkenv  = tmk(i,j,k-1)*fac2+tmk(i,j,k)*fac1
             qvpenv  = qvp(i,j,k-1)*fac2+qvp(i,j,k)*fac1
             tvenv   = virtual(tmkenv,qvpenv)
             tvlift  = virtual(tmklift,qvplift)
             ghtlift = zlcl
             ilcl    = 1
           ELSE
             tmklift = tonpsadiabat(ethpari,prs(i,j,k))                                
             eslift  = EZERO*exp(eslcon1*(tmklift-CELKEL)/    &
                       (tmklift-eslcon2))
             qvplift = EPS*eslift/(prs(i,j,k)-eslift)
             tvenv   = virtual(tmk(i,j,k),qvp(i,j,k))
             tvlift  = virtual(tmklift,qvplift)
             ghtlift = ght(i,j,k)
           END IF

           buoy(kk) = G*(tvlift-tvenv)/tvenv  ! buoyancy
           zrel(kk) = ghtlift-ghtpari

           IF ( (buoy(kk)*buoy(kk-1).lt.0.0) .AND. (kk.gt.1) ) THEN
             !! Parcel ascent curve crosses sounding curve, so create a new level
             !! in the bottom-up array at the crossing.
             kk = kk+1
             buoy(kk)   = buoy(kk-1)
             zrel(kk)   = zrel(kk-1)
             buoy(kk-1) = 0.
             zrel(kk-1) = zrel(kk-2) + buoy(kk-2)/(buoy(kk-2)-buoy(kk))*  &
                          (zrel(kk)-zrel(kk-2))
           END IF

           IF (ilcl == 1) THEN
             klcl = kk
             ilcl = 2
             GOTO 33
           END IF

         END DO         !! END DO k = kpar,bottom_top_dim

         kmax = kk
         IF (kmax .gt. 150) THEN
           PRINT*, 'in calc_cape: kmax got too big. kmax=',kmax
           STOP
         ENDIF

 
!        Get the accumulated buoyant energy from the parcel's starting
!        point, at all levels up to the top level.

         benaccum(1) = 0.0
         benamin     = 9e9
         DO k = 2,kmax
           dz          = zrel(k)-zrel(k-1)
           benaccum(k) = benaccum(k-1)+.5*dz*(buoy(k-1)+buoy(k))
             IF ( benaccum(k) .lt. benamin ) THEN
               benamin = benaccum(k)
             END IF
         END DO


!        Determine equilibrium level (EL), which we define as the highest
!        level of non-negative buoyancy above the LCL. Note, this may be
!        the top level if the parcel is still buoyant there.

         DO k = kmax,klcl,-1
           IF ( buoy(k) .ge. 0. ) THEN
             kel = k   ! k of equilibrium level
             GOTO 50
           END IF
         END DO


!        If we got through that loop, then there is no non-negative
!        buoyancy above the LCL in the sounding.  In these situations,
!        both CAPE and CIN will be set to -0.1 J/kg.  Also, where CAPE is
!        non-zero, CAPE and CIN will be set to a minimum of +0.1 J/kg, so
!        that the zero contour in either the CIN or CAPE fields will
!        circumscribe regions of non-zero CAPE.
 
         cape(i,j,kpar) = -0.1
         cin(i,j,kpar)  = -0.1
         klfc = kmax
 
         GOTO 102
 
 50      CONTINUE

 

!        If there is an equilibrium level, then CAPE is positive.  We'll
!        define the level of free convection (LFC) as the point below the
!        EL, but at or above the LCL, where accumulated buoyant energy is a
!        minimum.  The net positive area (accumulated buoyant energy) from
!        the LFC up to the EL will be defined as the CAPE, and the net
!        negative area (negative of accumulated buoyant energy) from the
!        parcel starting point to the LFC will be defined as the convective
!        inhibition (CIN).
 
!        First get the LFC according to the above definition.
 
         benamin = 9e9
         klfc = kmax
         DO k = klcl,kel
           IF ( benaccum(k) .lt. benamin ) THEN
             benamin = benaccum(k)
             klfc = k
           END IF
         END DO
 
!        Now we can assign values to cape and cin
 
         cape(i,j,kpar) = max(benaccum(kel)-benamin,0.1)
         cin(i,j,kpar)  = max(-benamin,0.1)
 
!        CIN is uninteresting when CAPE is small (< 100 J/kg), so set
!        CIN to -.1 in that case.
 
         IF ( cape(i,j,kpar) .lt. 100. ) cin(i,j,kpar) = -0.1

 102     CONTINUE

       ENDDO          !! END of BIG 2D/3D loop


       IF ( i3dflag == 0 ) THEN
         SCRa(i,j,1) = cape(i,j,kpar1)
         SCRa(i,j,2) = cin(i,j,kpar1)
         SCRa(i,j,3) = zrel(klcl)+ghtpari-ter(i,j)   ! meters AGL (LCL)
         SCRa(i,j,4) = zrel(klfc)+ghtpari-ter(i,j)   ! meters AGL (LFC)
       ENDIF

 
     END DO
     END DO                !! END BIG i/j loop
 

  !! These will be set by module_diagnostics as we have more than 1 field

  IF ( i3dflag == 1 ) THEN
    SCRa = cape
    SCRb = cin
  ENDIF

  cname    = " "
  cdesc    = " "
  cunits   = " "
  

  END SUBROUTINE calc_cape


!*********************************************************************c
!*********************************************************************c
  FUNCTION tonpsadiabat (thte,prs)
 
  USE constants_module
 
!   This function gives the temperature (in K) on a moist adiabat
!   (specified by thte in K) given pressure in hPa.  It uses a
!   lookup table, with data that was generated by the Bolton (1980)
!   formula for theta_e.

 
!     First check if pressure is less than min pressure in lookup table.
!     If it is, assume parcel is so dry that the given theta-e value can
!     be interpretted as theta, and get temperature from the simple dry
!     theta formula.
 
      IF ( prs .le. psadiprs(150) ) THEN
        tonpsadiabat = thte*(prs/1000.)**GAMMA_RIP
        RETURN
      ENDIF
 

!     Otherwise, look for the given thte/prs point in the lookup table.
 
      DO jtch = 1,150-1
        IF ( thte.ge.psadithte(jtch) .AND. thte.lt.psadithte(jtch+1) ) THEN
          jt = jtch
          GOTO 213
        END IF
      END DO
      jt = -1
 213  CONTINUE

      DO ipch = 1,150-1
        if ( prs.le.psadiprs(ipch) .AND. prs.gt.psadiprs(ipch+1) ) THEN
          ip = ipch
          GOTO 215
        END IF
      ENDDO
      ip = -1
 215  CONTINUE


      IF ( jt.eq.-1 .OR. ip.eq.-1 ) THEN
        PRINT*, 'Outside of lookup table bounds. prs,thte=',prs,thte
        STOP 
      ENDIF


      fracjt  = (thte-psadithte(jt))/(psadithte(jt+1)-psadithte(jt))
      fracjt2 = 1.-fracjt
      fracip  = (psadiprs(ip)-prs)/(psadiprs(ip)-psadiprs(ip+1))
      fracip2 = 1.-fracip

      IF ( psaditmk(ip,jt  ).gt.1e9 .OR. psaditmk(ip+1,jt  ).gt.1e9 .OR.   &
           psaditmk(ip,jt+1).gt.1e9 .OR. psaditmk(ip+1,jt+1).gt.1e9 ) THEN
        PRINT*, 'Tried to access missing tmperature in lookup table.'
        PRINT*, 'Prs and Thte probably unreasonable. prs,thte=',prs,thte
        STOP
      ENDIF

      tonpsadiabat = fracip2*fracjt2*psaditmk(ip  ,jt  )+   &
                     fracip *fracjt2*psaditmk(ip+1,jt  )+   &
                     fracip2*fracjt *psaditmk(ip  ,jt+1)+   &
                     fracip *fracjt *psaditmk(ip+1,jt+1)
      

  END FUNCTION tonpsadiabat 


END MODULE module_calc_cape
