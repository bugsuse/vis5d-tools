!! Which arrays are we carrying around

MODULE module_arrays

  CONTAINS
  SUBROUTINE clobber_arrays()

  USE module_model_basics

     !!IF (ALLOCATED(XLAT)) DEALLOCATE(XLAT)
     !!have_XLAT = .FALSE.
     !!IF (ALLOCATED(XLONG)) DEALLOCATE(XLONG)
     !!have_XLONG = .FALSE.
     IF (ALLOCATED(HGT)) DEALLOCATE(HGT)
     have_HGT = .FALSE.

     IF (ALLOCATED(U10)) DEALLOCATE(U10)
     have_U10 = .FALSE.
     IF (ALLOCATED(V10)) DEALLOCATE(V10)
     have_V10 = .FALSE.
     IF (ALLOCATED(T2)) DEALLOCATE(T2)
     have_T2 = .FALSE.
     IF (ALLOCATED(Q2)) DEALLOCATE(Q2)
     have_Q2 = .FALSE.

     IF (ALLOCATED(PRES)) DEALLOCATE(PRES)
     have_PRES = .FALSE.
     IF (ALLOCATED(P)) DEALLOCATE(P)
     have_P = .FALSE.
     IF (ALLOCATED(PB)) DEALLOCATE(PB)
     have_PB = .FALSE.
     IF (ALLOCATED(PSFC)) DEALLOCATE(PSFC)
     have_PSFC = .FALSE.

     IF (ALLOCATED(MU)) DEALLOCATE(MU)
     have_MU = .FALSE.
     IF (ALLOCATED(MUB)) DEALLOCATE(MUB)
     have_MUB = .FALSE.
     IF (ALLOCATED(ZNU)) DEALLOCATE(ZNU)
     have_ZNU = .FALSE.
     IF (ALLOCATED(ZNW)) DEALLOCATE(ZNW)
     have_ZNW = .FALSE.
     
     have_PTOP = .FALSE.

     IF (ALLOCATED(PH)) DEALLOCATE(PH)
     have_PH = .FALSE.
     IF (ALLOCATED(PHB)) DEALLOCATE(PHB)
     have_PHB = .FALSE.
     IF (ALLOCATED(GEOPT)) DEALLOCATE(GEOPT)
     have_GEOPT = .FALSE.

     IF (ALLOCATED(UUU)) DEALLOCATE(UUU)
     have_UUU = .FALSE.
     IF (ALLOCATED(VVV)) DEALLOCATE(VVV)
     have_VVV = .FALSE.

     IF (ALLOCATED(TK)) DEALLOCATE(TK)
     have_TK = .FALSE.
     IF (ALLOCATED(T)) DEALLOCATE(T)
     have_T = .FALSE.

     IF (ALLOCATED(QV)) DEALLOCATE(QV)
     have_QV = .FALSE.
     IF (ALLOCATED(QR)) DEALLOCATE(QR)
     have_QR = .FALSE.
     IF (ALLOCATED(QS)) DEALLOCATE(QS)
     have_QS = .FALSE.
     IF (ALLOCATED(QG)) DEALLOCATE(QG)
     have_QG = .FALSE.

  END SUBROUTINE clobber_arrays




  SUBROUTINE keep_arrays(cname, real_array)

  USE gridinfo_module
  USE module_model_basics

! Arguments
  character (len=*)             :: cname
  real, pointer, dimension(:,:,:) :: real_array


  IF (trim(cname) == 'TMP_ARRAY') THEN
     IF (ALLOCATED(TMP_ARRAY)) DEALLOCATE(TMP_ARRAY)
     allocate(TMP_ARRAY(west_east_dim,south_north_dim))
     TMP_ARRAY = real_array(:,:,1)
  ELSE IF (trim(cname) == 'XLAT') THEN
     allocate(XLAT(west_east_dim,south_north_dim))
     XLAT = real_array(:,:,1)
     have_XLAT = .TRUE.
  ELSE IF (trim(cname) == 'XLONG') THEN
     allocate(XLONG(west_east_dim,south_north_dim))
     XLONG = real_array(:,:,1)
     have_XLONG = .TRUE.
  ELSE IF (trim(cname) == 'HGT') THEN
     allocate(HGT(west_east_dim,south_north_dim))
     HGT = real_array(:,:,1)
     have_HGT = .TRUE.

  ELSE IF (trim(cname) == 'U10') THEN
     allocate(U10(west_east_dim,south_north_dim))
     U10 = real_array(:,:,1)
     have_U10 = .TRUE.
  ELSE IF (trim(cname) == 'V10') THEN
     allocate(V10(west_east_dim,south_north_dim))
     V10 = real_array(:,:,1)
     have_V10 = .TRUE.
  ELSE IF (trim(cname) == 'T2') THEN
     allocate(T2(west_east_dim,south_north_dim))
     T2 = real_array(:,:,1)
     have_T2 = .TRUE.
  ELSE IF (trim(cname) == 'Q2') THEN
     allocate(Q2(west_east_dim,south_north_dim))
     Q2 = real_array(:,:,1)
     have_Q2 = .TRUE.

  ELSE IF (trim(cname) == 'PRES') THEN
     allocate(PRES(west_east_dim,south_north_dim,bottom_top_dim))
     PRES = real_array
     have_PRES = .TRUE.
  ELSE IF (trim(cname) == 'P') THEN
     allocate(P(west_east_dim,south_north_dim,bottom_top_dim))
     P = real_array
     have_P = .TRUE.
  ELSE IF (trim(cname) == 'PB') THEN
     allocate(PB(west_east_dim,south_north_dim,bottom_top_dim))
     PB = real_array
     have_PB = .TRUE.
  ELSE IF (trim(cname) == 'PSFC') THEN
     allocate(PSFC(west_east_dim,south_north_dim))
     PSFC = real_array(:,:,1)
     have_PSFC = .TRUE.

  ELSE IF (trim(cname) == 'MU') THEN
     allocate(MU(west_east_dim,south_north_dim))
     MU = real_array(:,:,1)
     have_MU = .TRUE.
  ELSE IF (trim(cname) == 'MUB') THEN
     allocate(MUB(west_east_dim,south_north_dim))
     MUB = real_array(:,:,1)
     have_MUB = .TRUE.
  ELSE IF (trim(cname) == 'ZNU') THEN
     allocate(ZNU(bottom_top_dim))
     ZNU = real_array(:,1,1)
     have_ZNU = .TRUE.
  ELSE IF (trim(cname) == 'ZNW') THEN
     allocate(ZNW(bottom_top_dim+1))
     ZNW = real_array(:,1,1)
     have_ZNW = .TRUE.
  ELSE IF (trim(cname) == 'P_TOP') THEN
     PTOP = real_array(1,1,1)
     have_PTOP = .TRUE.

  ELSE IF (trim(cname) == 'PH') THEN
     allocate(PH(west_east_dim,south_north_dim,bottom_top_dim))
     PH = 0.5*(real_array(:,:,1:bottom_top_dim)+real_array(:,:,2:bottom_top_dim+1))
     have_PH = .TRUE.
  ELSE IF (trim(cname) == 'PHB') THEN
     allocate(PHB(west_east_dim,south_north_dim,bottom_top_dim))
     PHB = 0.5*(real_array(:,:,1:bottom_top_dim)+real_array(:,:,2:bottom_top_dim+1))
     have_PHB = .TRUE.
  ELSE IF (trim(cname) == 'GEOPT') THEN
     allocate(GEOPT(west_east_dim,south_north_dim,bottom_top_dim))
     GEOPT = real_array
     have_GEOPT = .TRUE.
  ELSE IF (trim(cname) == 'GHT') THEN
     allocate(GEOPT(west_east_dim,south_north_dim,bottom_top_dim))
     GEOPT = real_array*9.81
     have_GEOPT = .TRUE.

  ELSE IF (trim(cname) == 'U' .or. trim(cname) == 'UU') THEN
     allocate(UUU(west_east_dim,south_north_dim,bottom_top_dim))
     UUU = 0.5*(real_array(1:west_east_dim,:,:)+real_array(2:west_east_dim+1,:,:))
     have_UUU = .TRUE.
  ELSE IF (trim(cname) == 'V' .or. trim(cname) == 'VV') THEN
     allocate(VVV(west_east_dim,south_north_dim,bottom_top_dim))
     VVV = 0.5*(real_array(:,1:south_north_dim,:)+real_array(:,2:south_north_dim+1,:))
     have_VVV = .TRUE.

  ELSE IF (trim(cname) == 'TT' .or. trim(cname) == 'TK') THEN
     allocate(TK(west_east_dim,south_north_dim,bottom_top_dim))
     TK = real_array
     have_TK = .TRUE.
  ELSE IF (trim(cname) == 'T') THEN
     allocate(T(west_east_dim,south_north_dim,bottom_top_dim))
     T = real_array
     have_T = .TRUE.

  ELSE IF (trim(cname) == 'QVAPOR') THEN
     allocate(QV(west_east_dim,south_north_dim,bottom_top_dim))
     QV = real_array
     have_QV = .TRUE.
  ELSE IF (trim(cname) == 'QRAIN') THEN
     allocate(QR(west_east_dim,south_north_dim,bottom_top_dim))
     QR = real_array
     have_QR = .TRUE.
  ELSE IF (trim(cname) == 'QSNOW') THEN
     allocate(QS(west_east_dim,south_north_dim,bottom_top_dim))
     QS = real_array
     have_QS = .TRUE.
  ELSE IF (trim(cname) == 'QGRAUP') THEN
     allocate(QG(west_east_dim,south_north_dim,bottom_top_dim))
     QG = real_array
     have_QG = .TRUE.
  END IF

  
  END SUBROUTINE keep_arrays

END MODULE module_arrays
