MODULE v5d_module

   USE gridinfo_module
   USE output_module
   USE map_utils
   USE misc_definitions_module
   USE module_interp


   CONTAINS

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: v5d_varnames     
   ! Purpose: Get info about the variables to send to v5d file
   ! 	      This routine assumes that the user only asked for variables
   !          that are available. If variables are not available v5d will no 
   !          be working correctly.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   SUBROUTINE v5d_varnames (v5d_nl)

     IMPLICIT NONE

     ! ------ some vis5d stuff, contain MAXVARS,MAXTIMES,MAXROWS,MAXCOLUMNS,MAXLEVELS
     include 'v5df.h'

     integer, dimension(MAXLEVELS)          :: v5d_nl
     character (len=4000)                   :: v5d_fields
     integer                                :: is_there, is_3d
     character (len=1000)                   :: known_3d_fields
     character (len=20)                     :: dummy

     
     known_3d_fields = 'P,PB,QCLOUD,QGRAUP,QICE,QNICE,QRAIN,QSNOW, &
&QVAPOR,T,TKE,TKE_MYJ,U,V,PH,PHB,W,cape,cin,dbz,height,geopt,pressure,rh, &
&tc,td,theta,tk,umet,vmet,wdir,wspd'


     varnames = ' '
     numvars = 0
     v5d_fields = plot_these_fields(2:len_trim(plot_these_fields))
     is_there = INDEX(v5d_fields,",")
     DO WHILE ( is_there > 1 ) 

       numvars = numvars  + 1
       varnames(numvars) = v5d_fields(1:is_there-1)
       dummy = ","//trim(varnames(numvars))//","

       IF ( trim(varnames(numvars)) == 'SH2O'  .OR.  &
            trim(varnames(numvars)) == 'SMOIS' .OR.  &
            trim(varnames(numvars)) == 'TSLB'  ) THEN
         print*,"  WARING:  We cannot deal with field ", trim(varnames(numvars))
         print*,"           It will be removed from the plot list"
         is_3d = INDEX(plot_these_fields,trim(dummy))
         plot_these_fields = plot_these_fields(1:is_3d)//plot_these_fields(is_3d+len_trim(dummy):len_trim(plot_these_fields))
           
         varnames(numvars) = ' ' 
         numvars = numvars - 1
       ELSE
         v5d_nl(numvars) = 1
         is_3d = INDEX(known_3d_fields,trim(varnames(numvars)))
         IF ( is_3d > 0 ) v5d_nl(numvars) = number_of_zlevs
       END IF  

       v5d_fields = v5d_fields(is_there+1:len_trim(v5d_fields))

       !!! Check for doubles
       is_there = INDEX(v5d_fields,trim(dummy))
       DO WHILE ( is_there /= 0 )
          v5d_fields = v5d_fields(1:is_there)//v5d_fields(is_there+len_trim(dummy):len_trim(v5d_fields))
          is_there = INDEX(v5d_fields,trim(dummy))
        END DO

       is_there = INDEX(v5d_fields,",") 

     END DO

  
   END SUBROUTINE v5d_varnames

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: v5d_times     
   ! Purpose: Get info about the variables to send to v5d file
   ! 	      This routine assumes that the user only asked for variables
   !          that are available. If variables are not available v5d will no 
   !          be working correctly.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   SUBROUTINE v5d_times (n_times, timestamp, datestamp)

     USE date_pack

     IMPLICIT NONE

     ! ------ some vis5d stuff, contain MAXVARS,MAXTIMES,MAXROWS,MAXCOLUMNS,MAXLEVELS
     include 'v5df.h'

     integer, dimension(MAXTIMES)           :: timestamp, datestamp
     character (len=19)                     :: next_date
     integer                                :: tt, n_times
     integer                                :: year, month, day, hours, minutes, seconds, YY


     datestamp = IMISSING
     timestamp = IMISSING

     DO tt = 1,n_times
       CALL geth_newdate(next_date, trim(start_date), (tt-1)*interval_seconds)

       read(next_date(3:4),*)   YY
       read(next_date(1:4),*)   year
       read(next_date(6:7),*)   month
       read(next_date(9:10),*)  day
       read(next_date(12:13),*) hours
       read(next_date(15:16),*) minutes
       read(next_date(18:19),*) seconds


       timestamp(tt) = hours*10000 + minutes*100 + seconds

       if(month >= 2) day = day+31  ! add january
       if(month >= 3) day = day+28  ! add february
       if(month >= 4) day = day+31  ! add march
       if(month >= 5) day = day+30  ! add april
       if(month >= 6) day = day+31  ! add may
       if(month >= 7) day = day+30  ! add june
       if(month >= 8) day = day+31  ! add july
       if(month >= 9) day = day+31  ! add august
       if(month >= 10) day = day+30 ! add september
       if(month >= 11) day = day+31 ! add october
       if(month >= 12) day = day+30 ! add november
       if((month > 2) .and. (mod(year,4) == 0)) day = day+1  ! get leap year day

       datestamp(tt) = (YY)*1000 + day

     END DO

   END SUBROUTINE v5d_times

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: v5d_proj     
   ! Purpose: Get map information needed to create v5d output file
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   SUBROUTINE v5d_proj (date, projection, proj_args)

     USE input_module

     IMPLICIT NONE

     character (len=19)                  :: date
     integer                             :: projection
     real, dimension(100)                :: proj_args
     real                                :: xlat_s, xlat_n, xlon_w, xlon_e, xlat1, xlon1
     real                                :: xx, yy, temp

     type (proj_info)                    :: projst
     real                                :: msf
     real                                :: hemiflag
     real                                :: degtorad
     integer                             :: good_to_go


      IF (debug_level >= 900) print*,"DEBUG: Subroutine v5d_proj"

      !!! Make sure we have XLAT and XLONG
      !!! If not treat all projections as map_proj=0 
      IF ( map_proj /= 0 ) THEN
        good_to_go = 0
        IF ( .not. have_XLAT )  CALL get_keep_array ( date, good_to_go, "XLAT", "XLAT_M" )
        IF ( .not. have_XLONG ) CALL get_keep_array ( date, good_to_go, "XLONG", "XLONG_M" )
        IF (debug_level >= 900) print*,"DEBUG: Looked for lat/lon ", good_to_go
        IF ( good_to_go == 0 ) THEN
          xlat_s = XLAT(1,1)
          xlat_n = XLAT(1,south_north_dim)
          xlat1  = XLAT(1,1)
          xlon_w = XLONG(1,south_north_dim)
          xlon_e = XLONG(west_east_dim,south_north_dim)
          xlon1  = XLONG(1,1)
        ELSE
          print*,"WARNING: Do not have map projection info, treating data as if map_proj=0"
          map_proj = 0
        ENDIF
      ENDIF


     IF ( MAP_PROJ == 0 ) THEN        !!! ideal cases

       projection = 0
       proj_args(1) = 1.
       proj_args(2) = 1.
       proj_args(3) = 1.
       proj_args(4) = 1.

     ELSE IF ( MAP_PROJ == 1 ) then    !!! Lambert

       projection = 2

       IF ( TRUELAT1 < TRUELAT2 ) THEN   !!! truelat1 must be bigger than truelat2
         temp = TRUELAT1
         TRUELAT1 = TRUELAT2
         TRUELAT2 = temp
       END IF

       CALL make_lambert(xlat_n,xlon_w,xx,yy)

       proj_args(1) = TRUELAT1
       proj_args(2) = TRUELAT2
       proj_args(3) = -yy
       proj_args(4) = -xx
       proj_args(5) = -STAND_LON
       proj_args(6) = 0.001*DX

       IF ( debug_level .ge. 200 ) THEN
          print*," DEBUG : Vis5D - Lambert Projection"
          print*,"         PROJ args = ", proj_args(1), proj_args(2), &
                                          proj_args(3), proj_args(4), &
                                          proj_args(5), proj_args(6)
       END IF


     ELSE IF ( MAP_PROJ == 2 ) THEN   !!! PS

       CALL map_set(PROJ_PS,xlat1,xlon1,DX,STAND_LON,TRUELAT1,TRUELAT2, &
                    west_east_dim, south_north_dim, projst)
       projection = 3

       IF (TRUELAT1 .ge. 0.) THEN
         proj_args(1) = 90.
         hemiflag = 1.
       ELSE
         proj_args(1) = -90.
         hemiflag = -1.
       END IF
       proj_args(2) = -STAND_LON
       proj_args(3) = float(south_north_dim) - projst%polej
       proj_args(4) = projst%polei
       msf  = (1.+hemiflag*sin(projst%TRUELAT1*degtorad))/ &
              (1.+hemiflag*sin(proj_args(1)*degtorad))
       proj_args(5) = DX* 0.001/msf
 
     ELSE IF ( MAP_PROJ == 3 ) THEN   !!! Mercator

       projection = 1

       proj_args(1) = xlat_n
       proj_args(2) = -xlon_w
       proj_args(3) = (xlat_n - xlat_s) / (south_north_dim -1)
       proj_args(4) = abs((xlon_w - xlon_e)) / (west_east_dim -1)

       IF ( debug_level .ge. 200 ) THEN
          print*," DEBUG : Vis5D - Mercator Projection"
          print*,"         XLAT s/n  = ", xlat_s, xlat_n
          print*,"         XLON w/e  = ", xlon_w, xlon_e
          print*,"         PROJ args = ", proj_args(1), proj_args(2), &
                                          proj_args(3), proj_args(4)
       END IF

     END IF
  
   END SUBROUTINE v5d_proj


  SUBROUTINE make_lambert(xlatn,xlonw,xx,yy)

!  imported from TOVIS5D for MM5

     USE input_module

     IMPLICIT NONE

     real                 :: xlatn, xlonw, xx, yy
     real                 :: radius, degtorad, alpha, pi
     real                 :: theta, theta1, theta2, htheta, htheta1, htheta2, cone, dis, rad

     data radius /6371./

!  put map parameters in as defined by vis5d;
!    south is positive direction, so negative ypole

     pi       = 4.0*atan( 1.0 )
     degtorad = pi/180.0
  
     dis      = 0.001*DX
  
     theta    = (90.0-xlatn) * degtorad
     theta1   = (90.0-TRUELAT1) * degtorad
     theta2   = (90.0-TRUELAT2) * degtorad
     IF ( theta1 == theta2 ) THEN    !! Just to ensure we do not have NaN for the cone
       theta1 = theta1+0.01
     END IF
     htheta   = theta /2.0
     htheta1  = theta1/2.0
     htheta2  = theta2/2.0

     cone     = alog(sin(theta1)/sin(theta2))/alog(tan(htheta1)/tan(htheta2))

     alpha    = cone * (xlonw - STAND_LON) * degtorad
     rad      = radius * sin(theta1) * (tan(htheta)/tan(htheta1))**cone/cone

     xx       = rad * sin(alpha) / dis
     yy       = rad * cos(alpha) / dis

  END SUBROUTINE make_lambert


END MODULE v5d_module
