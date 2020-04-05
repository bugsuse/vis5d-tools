!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! MODULE CONSTANTS_MODULE
!
! This module defines constants that are used by other modules 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MODULE constants_module

   real, parameter :: PI = 3.141592653589793
   real, parameter :: OMEGA_E = 7.292e-5 ! Angular rotation rate of the earth

   real, parameter :: DEG_PER_RAD = 180./PI
   real, parameter :: RAD_PER_DEG = PI/180.
 
   ! Mean Earth Radius in m.  The value below is consistent
   ! with NCEP's routines and grids.
   real, parameter :: EARTH_RADIUS_M = 6370000.   ! same as MM5 system
   real, parameter :: EARTH_CIRC_M = 2.*PI*EARTH_RADIUS_M

   real, parameter :: G = 9.81
   real, parameter :: Rd = 287.04
   real, parameter :: Rv = 461.6
   real, parameter :: Rm = .608 
   !real, parameter :: Cp = 1004.
   real, parameter :: Cp = 7.*Rd/2.
   real, parameter :: Cv = Cp-Rd
   real, parameter :: CPMD = 0.887
   real, parameter :: RCP = Rd/Cp
   real, parameter :: T0 = 273.16
   real, parameter :: p0 = 100000.
   real, parameter :: GAMMA = 0.0065
   real, parameter :: GAMMA_RIP = Rd/Cp 
   real, parameter :: GAMMAMD = Rm-CPMD

   real, parameter :: CELKEL = 273.15
   real, parameter :: RHOWAT = 1000.
   real, parameter :: EPS = 0.622
   real, parameter :: EZERO = 6.112

   real, parameter :: ESLCON1 = 17.67
   real, parameter :: ESLCON2 = 29.65
   real, parameter :: THTECON1 = 3376.
   real, parameter :: THTECON2 = 2.54
   real, parameter :: THTECON3 = 0.81
   real, parameter :: TLCLC1 = 2840.
   real, parameter :: TLCLC2 = 3.5
   real, parameter :: TLCLC3 = 4.805
   real, parameter :: TLCLC4 = 55.


END MODULE constants_module
