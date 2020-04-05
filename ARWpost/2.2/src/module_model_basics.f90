!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MODULE module_model_basics

   integer                              :: west_east_dim, south_north_dim, bottom_top_dim
   integer                              :: iprogram
   character (len=1)                    :: vertical_type

   integer                              :: map_proj
   real                                 :: dx, dy, cen_lat, moad_cen_lat, cen_lon, stand_lon
   real                                 :: truelat1, truelat2
   character (len=128)                  :: title

   real                                 :: PTOP
   real, allocatable, dimension(:)      :: ZNU, ZNW
   real, allocatable, dimension(:,:)    :: XLAT, XLONG, HGT
   real, allocatable, dimension(:,:)    :: U10, V10, T2, Q2
   real, allocatable, dimension(:,:)    :: PSFC
   real, allocatable, dimension(:,:)    :: MU, MUB
   real, allocatable, dimension(:,:)    :: TMP_ARRAY
   real, allocatable, dimension(:,:,:)  :: P, PB, PH, PHB, PRES, GEOPT  !! PRES is pressure in Pa
   real, allocatable, dimension(:,:,:)  :: UUU, VVV, T, TK              !! TK is temp in K, T is theta-300
   real, allocatable, dimension(:,:,:)  :: QV, QR, QS, QG

   logical                              :: have_PTOP
   logical                              :: have_ZNU, have_ZNW
   logical                              :: have_XLAT, have_XLONG, have_HGT
   logical                              :: have_U10, have_V10, have_T2, have_Q2
   logical                              :: have_PSFC
   logical                              :: have_MU, have_MUB
   logical                              :: have_P, have_PB, have_PH, have_PHB, have_PRES, have_GEOPT
   logical                              :: have_UUU, have_VVV, have_T, have_TK
   logical                              :: have_QV, have_QR, have_QS, have_QG

   CONTAINS

   FUNCTION virtual (tmp,rmix)
!      This function returns virtual temperature in K, given temperature
!      in K and mixing ratio in kg/kg.
 
     real                              :: tmp, rmix, virtual
 
     virtual=tmp*(0.622+rmix)/(0.622*(1.+rmix))

   END FUNCTION virtual


END MODULE module_model_basics
