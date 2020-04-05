!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! MODULE MISC_DEFINITIONS_MODULE
!
! This module defines various non-meteorological constants that are used 
!   by other modules for readability.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MODULE misc_definitions_module

   !!integer, parameter :: BINARY=1, NETCDF=2, GRIB1=3, HDF=4
   integer, parameter :: BINARY=1, NETCDF=2, GRIB1=5

   ! Projection codes for proj_info structure:
   INTEGER, PUBLIC, PARAMETER  :: PROJ_LATLON = 0
   INTEGER, PUBLIC, PARAMETER  :: PROJ_LC = 1
!   INTEGER, PUBLIC, PARAMETER  :: PROJ_PS = 2
   INTEGER, PUBLIC, PARAMETER  :: PROJ_MERC = 3
   !!INTEGER, PUBLIC, PARAMETER  :: PROJ_GAUSS = 4
   !!INTEGER, PUBLIC, PARAMETER  :: PROJ_ROTLL = 6

END MODULE misc_definitions_module
