MODULE output_module
   
   USE input_module
   USE module_model_basics
   USE module_arrays
   USE module_interp
   
   integer, parameter                  :: MAX_NVARS = 300    !!! maximum number of variables, increase if needed
   integer                             :: time, rec, ivars   !!! process time ; rec in .dat file ; variables in .dat file
   integer                             :: cunit, dunit       !!! .ctl and .dat output files
   character (len=128)                 :: ctlfile, datfile, v5dfile
   character (len=200), dimension(300) :: ctl_var_string     !!! List of fields for .ctl file
   character (len=2000)                :: could_not_find     !!! Keep list of what we wrote out
   character (len=19)                  :: tdef_date          !!! Output start date
   character (len=40)                  :: tdef               !!! tdef string in .ctl file
   character (len=10), dimension(300)  :: varnames 
   integer                             :: numvars

   CONTAINS

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: create_ctl     
   ! Purpose: Create the .ctl file                  
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   SUBROUTINE create_ctl ( valid_date )

      IMPLICIT NONE
  
      ! Local variables
      real, dimension(4)               :: xlat_a, xlon_a
      real                             :: dxll, slack, temp
      real                             :: abslatmin, abslonmax, abslonmin, abslatmax
      integer                          :: i, k, ipoints, jpoints, ilon 
      integer                          :: ipole
      real                             :: xi, r, re, xj, dx_ps, earthr, radpd
      real                             :: rlat, rlong, ref_lon, ref_lat
      integer                          :: file_start, file_end
      integer                          :: we_dim, sn_dim
      character (len=128)              :: tmp_file
      character (len=30)               :: options
      integer                          :: good_to_go
      character (len=19)               :: valid_date 


      
      tmp_file    = trim(output_root_name)//'.dat'
      IF ( split_output ) &
           tmp_file    = trim(output_root_name)//'_%y4-%m2-%d2_%h2:%n2.dat'

      file_start = INDEX(tmp_file,"/",BACK=.TRUE.)
      file_end   = len_trim(tmp_file)
      write(cunit,'("dset ^",A)') tmp_file(file_start+1:file_end)


      options = ' '
#ifdef bytesw
      options = trim(options)//' byteswapped '
#endif
      IF ( split_output ) options = trim(options)//' template '
      IF ( len_trim(options) .gt. 1 ) write(cunit, '("options ",A)') trim(options)


      write(cunit, '("undef 1.e30")')


      IF ( output_title /= '   ') THEN
        write(cunit,'("title ",A)') trim(output_title)
      ELSE
        write(cunit,'("title ",A)') trim(title)
      END IF


      IF ( debug_level >= 300 ) print*,"DEGUG: CTL file has been opened"
      !!! Make sure we have XLAT and XLONG
      !!! If not treat all projections as map_proj=0 
      IF ( map_proj /= 0 ) THEN
        IF ( debug_level >= 300 ) print*,"DEGUG: See if we have the XLAT/XLONG data required"
        good_to_go = 0
        IF ( .not. have_XLAT )  CALL get_keep_array ( valid_date, good_to_go, "XLAT", "XLAT_M" )
        IF ( .not. have_XLONG ) CALL get_keep_array ( valid_date, good_to_go, "XLONG", "XLONG_M" )
        IF ( good_to_go /= 0 ) THEN
          print*,"WARNING: Do not have map projection info, treating data as if map_proj=0"
          map_proj = 0
        ELSE
           IF ( debug_level >= 300 ) print*,"DEGUG: We have the required XLAT/XLONG"
        ENDIF
      ENDIF

      IF (map_proj == 1) THEN                                     !! Lambert Projection

        ! Make sure truelat1 is the larger number
         IF (truelat1 < truelat2) THEN
            temp = truelat1
            truelat1 = truelat2
            truelat2 = temp
         END IF

         !!WRITE(cunit,'("pdef ",i4," ",i3," lcc ",f9.5," ",f10.5," ", &   !! rounding off
         WRITE(cunit,'("pdef ",i4," ",i3," lcc ",f7.3," ",f8.3," ",  &    
&             f8.3," ",f8.3," ",f9.5," ",f9.5," ",f10.5," ",         &
&             f10.3," ",f10.3)')                                     &
              west_east_dim,south_north_dim,cen_lat,cen_lon,         &
              (west_east_dim+1)/2.,(south_north_dim+1)/2.,           &
              truelat1,truelat2,stand_lon,dx,dy


         xlon_a(1) = XLONG(1,1)
         xlon_a(2) = XLONG(1,south_north_dim)
         xlon_a(3) = XLONG(west_east_dim,1)
         xlon_a(4) = XLONG(west_east_dim,south_north_dim)

         !check for dateline
         ilon = 0
         if ( abs(xlon_a(1) - xlon_a(3)) .GT. 180. ) ilon = 1
         if ( abs(xlon_a(2) - xlon_a(4)) .GT. 180. ) ilon = 1

         abslatmin = minval(XLAT)
         abslatmax = maxval(XLAT)
         abslonmin =  99999.
         abslonmax = -99999.
         DO i=1,4
           IF ( xlon_a(i) .lt. 0.0 .AND. ilon .eq. 1 ) THEN
             abslonmin=min(abslonmin,360.+xlon_a(i))
             abslonmax=max(abslonmax,360.+xlon_a(i))
           ELSE
             abslonmin=min(abslonmin,xlon_a(i))
             abslonmax=max(abslonmax,xlon_a(i))
           ENDIF
         END DO

         IF ( grads_low_res ) THEN
           WRITE(cunit,'("xdef ",i4," linear ",f10.5," ",f12.8)') west_east_dim,   &
                        abslonmin,abs(abslonmax-abslonmin)/(west_east_dim-1)
           WRITE(cunit,'("ydef ",i4," linear ",f10.5," ",f12.8)') south_north_dim, &
                        abslatmin,abs(abslatmax-abslatmin)/(south_north_dim-1)
         ELSE
           dxll = (dx/1000.)/111./2.
           slack = ((dx/1000.)*3.)/100.
           ipoints = int(((abslonmax-abslonmin)+(3*slack))/dxll)
           jpoints = int(((abslatmax-abslatmin)+(3*slack))/dxll)
           WRITE(cunit,'("xdef ",i4," linear ",f10.5," ",f12.8)') ipoints, &
                        (abslonmin-1.5*slack),dxll
           WRITE(cunit,'("ydef ",i4," linear ",f10.5," ",f12.8)') jpoints, &
                        (abslatmin-1.5*slack),dxll
         ENDIF

      ENDIF

      IF (map_proj == 2) THEN                                     !! Polar Stereo Projection

         ipole = 1
         IF (truelat1 .lt. 0.0) ipole = -1
         radpd  = .01745329
         earthr = 6.3712E6


         dx_ps = dx/( (1.0+sin(ipole*TRUELAT1*radpd))/(1.0+sin(60*radpd)) ) !! true dx (meter) at 60degrees
         re    = ( earthr * (1.0 + sin(60*radpd)) ) / dx_ps          


         IF ( ipole == 1 ) THEN
           rlong  = (XLONG(1,1) - stand_lon) * radpd
           rlat   = XLAT(1,1) * radpd

           r    =  (re * cos(rlat)) / (1.0 + sin(rlat))
           xi   =  (1. - r * sin(rlong))
           xj   =  (1. + r * cos(rlong))
           
           write(cunit,'("pdef ",i3," ",i3," nps ",f8.3," ",f8.3," ", &
&                f12.4," ",f12.7)') west_east_dim, south_north_dim,   &
                 xi, xj, stand_lon, dx_ps*0.001

         ELSE IF ( ipole == -1 ) THEN
           rlong  = (180.0 + XLONG(1,1) - stand_lon) * radpd
           rlat   = ipole*XLAT(1,1) * radpd

           r    =  (re * cos(rlat)) / (1.0 + sin(rlat))
           xi   =  (1. - ipole * r * sin(rlong))
           xj   =  (1. + r * cos(rlong))

           write(cunit,'("pdef ",i3," ",i3," sps ",f8.3," ",f8.3," ", &
&                f12.4," ",f12.7)') west_east_dim, south_north_dim,   &
                 xi, xj, (180+stand_lon), -0.001*dx_ps
         END IF


         abslonmin = minval(XLONG)
         abslonmax = maxval(XLONG)
         abslatmin = minval(XLAT)
         abslatmax = maxval(XLAT)
   
         dxll = (dx_ps/1000.)/111./2.
         ipoints = int(((abslonmax-abslonmin)+2.)/dxll)-2
         jpoints = int(((abslatmax-abslatmin)+2.)/dxll)-3

         ref_lon = abslonmin-1.
         IF ( abslonmax .ge. 175. .AND. abslonmax .le. 180.) THEN
           IF ( abslonmin .ge. -180. .AND. abslonmin .le. -175.) THEN
             ref_lon = 0.0
           END IF
         END IF

         ref_lat = abslatmin-1.
         !!IF ( ipole == -1 ) ref_lat = abslatmax+1.
         IF ( ipole == -1 ) ref_lat = abslatmin
   
         IF ( grads_low_res ) THEN
           WRITE(cunit,'("xdef ",i4," linear ",f10.5," ",f12.8)') west_east_dim, &
                        abslonmin,abs(abslonmax-abslonmin)/(west_east_dim-1)
           WRITE(cunit,'("ydef ",i4," linear ",f10.5," ",f12.8)') south_north_dim, &
                        abslatmin,abs(abslatmax-abslatmin)/(south_north_dim-1)
         ELSE
           WRITE(cunit,'("xdef ",i4," linear ",f6.1," ",f12.8)') ipoints, &
                        ref_lon,dxll
           WRITE(cunit,'("ydef ",i4," linear ",f6.1," ",f12.8)') jpoints, &
                        ref_lat,dxll
         ENDIF

      ENDIF

      IF (map_proj == 3) THEN                         !! Mercator Projection
   
          !check for dateline
          ilon = 0
          IF ( abs(XLONG(1,1) - XLONG(west_east_dim,1)) .GT. 180. ) ilon = 1
          IF ( abs(XLONG(1,south_north_dim) - XLONG(west_east_dim,south_north_dim)) .GT. 180. ) ilon = 1
   
          IF ( mercator_defs .AND. map_proj == 3 ) THEN
            WRITE(cunit,'("xdef ",i4," levels ")') west_east_dim
            DO i = 1,west_east_dim
              WRITE(cunit,*) XLONG(i,1)
              !!IF ( XLONG(i,1) .lt. 0.0 ) THEN
                !!WRITE(cunit,*) XLONG(i,1) + 180.0
              !!ELSE
                !!WRITE(cunit,*) XLONG(i,1)
              !!END IF
            END DO
          ELSE
            IF ( ilon == 1 ) THEN
              WRITE(cunit,'("xdef ",i4," linear ",f9.4," ",f8.4)')   &
                    west_east_dim,XLONG(1,1), &
                    (abs(XLONG(1,1)-(360.+XLONG(west_east_dim,south_north_dim)))/(west_east_dim-1))
            ELSE
              WRITE(cunit,'("xdef ",i4," linear ",f9.4," ",f8.4)')   &
                    west_east_dim,XLONG(1,1), &
                    (abs(XLONG(1,1)-XLONG(west_east_dim,south_north_dim))/(west_east_dim-1))
            ENDIF
          ENDIF

          IF ( mercator_defs .AND. map_proj == 3 ) THEN
             WRITE(cunit,'("ydef ",i4," levels ")') south_north_dim
             DO i = 1,south_north_dim
               WRITE(cunit,*) XLAT(1,i)
             END DO
          ELSE
             WRITE(cunit,'("ydef ",i4," linear ",f9.4," ",f8.4)')   &
                   south_north_dim,XLAT(1,1), &
                   (abs(XLAT(1,1)-XLAT(west_east_dim,south_north_dim))/(south_north_dim-1))
          ENDIF


      ENDIF


      IF (map_proj == 6) THEN                         !! lat-lon
        we_dim = west_east_dim
        sn_dim = south_north_dim
        write (cunit, '("xdef  ",i4, " linear ",F7.2, F7.4)') &
               we_dim, XLONG(1,1), abs(XLONG(2,1)-XLONG(1,1))
        write (cunit, '("ydef  ",i4, " linear ",F7.2, F7.4)') &
               sn_dim,  XLAT(1,1), abs(XLAT(1,2)-XLAT(1,1))
      ENDIF


      IF (map_proj == 0) THEN                         !! Ideal
        we_dim = west_east_dim
        sn_dim = south_north_dim
        IF (we_dim == 2) we_dim = 1
        IF (sn_dim == 2) sn_dim = 1
        write (cunit, '("xdef  ",i4, " linear 0 0.0001")') we_dim
        write (cunit, '("ydef  ",i4, " linear 0 0.0001")') sn_dim
      ENDIF

      IF ( debug_level >= 300 ) print*,"DEGUG: Wrote def section for map projection ", map_proj
      !! END of projection specific section


      IF (iprogram .ge. 6 .and. (vertical_type=='p'.or.vertical_type=='z')) THEN
        WRITE(cunit,'("zdef  ",i3, " levels  ")') number_of_zlevs
        DO k = 1,number_of_zlevs
           WRITE(cunit,'(f10.5)') interp_levels(k)
        END DO
      ELSE
        WRITE(cunit,'("zdef ",i4," linear 1 1  ")') bottom_top_dim
      ENDIF

       IF (tdef_date == '0000-00-00_00:00:00') THEN
         tdef = '   1 linear 00z01jan2000  1hr'
       ELSE
         CALL tdef_calc ()
       END IF
       WRITE(cunit,'("tdef ",A)') tdef

       WRITE(cunit,'("VARS  ",i3)') ivars
       DO i = 1,ivars
          WRITE(cunit,'(A)') trim(ctl_var_string(i))
       ENDDO
       WRITE(cunit,'("ENDVARS")')
       
       DO i=1,iatts
         WRITE(cunit,'(A)') trim(catts(i))
       ENDDO


   END SUBROUTINE create_ctl

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: tdef_calc      
   ! Purpose: Make the tdef line for the .ctl file
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   SUBROUTINE tdef_calc ()

     IMPLICIT NONE

     character (len=3)              :: cmonth
     integer                        :: year,month,day,hour
  
     tdef = '                               '
  
     !! Time comes in as YYYY-MM-DD_HH:MM:SS
     !!                  1234 67 90 23 45 89
     READ(tdef_date(1:4),*)   year
     READ(tdef_date(6:7),*)   month
     READ(tdef_date(9:10),*)  day
     READ(tdef_date(12:13),*) hour

    IF (month == 1) cmonth = 'JAN'
    IF (month == 2) cmonth = 'FEB'
    IF (month == 3) cmonth = 'MAR'
    IF (month == 4) cmonth = 'APR'
    IF (month == 5) cmonth = 'MAY'
    IF (month == 6) cmonth = 'JUN'
    IF (month == 7) cmonth = 'JUL'
    IF (month == 8) cmonth = 'AUG'
    IF (month == 9) cmonth = 'SEP'
    IF (month ==10) cmonth = 'OCT'
    IF (month ==11) cmonth = 'NOV'
    IF (month ==12) cmonth = 'DEC'

    IF ( year < 100 ) year = 2000

    WRITE (tdef,'(i4," linear ",i2.2,"Z",i2.2,A,i4,"  ",i6,"MN")') time, hour, day, cmonth, year, interval_seconds/60

   END SUBROUTINE tdef_calc

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: write_dat
   ! Purpose: Write the data to the .dat file. Also keep record of what has been
   !          written for creation of the .ctl file
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   SUBROUTINE write_dat (data_out, nx, ny, nz, cname, cdesc, cunits)

      IMPLICIT NONE

      ! ------ some vis5d stuff, contain MAXVARS,MAXTIMES,MAXROWS,MAXCOLUMNS,MAXLEVELS
      include 'v5df.h'

      ! Arguments
      integer                        :: nx, ny, nz
      real, dimension(nx,ny,nz)      :: data_out
      character (len=128)            :: cname, cunits, cdesc

      ! Local variables
      integer                        :: ii, jj, kk
      integer                        :: is_there, ret, ivar
      character (len=20)             :: dummy
      real, dimension(ny,nx,nz)      :: v5d_data_out


      IF ( debug_level .ge. 300 ) THEN
        print*,"DEBUG: Write variable ",trim(cname), " to output file"
      ENDIF
      IF ( debug_level .ge. 700 ) THEN
        DO kk=1,nz
           print*,"       min/max at k=",kk,minval(data_out(:,:,kk)),maxval(data_out(:,:,kk))
        END DO
      ENDIF



      IF ( output_type == 'grads' ) THEN   !!! Create the grads file
        IF ( nx == 2 ) nx = 1
        IF ( ny == 2 ) ny = 1
        DO kk=1,nz
          rec = rec + 1
          WRITE(dunit,rec=rec) ((data_out(ii,jj,kk),ii=1,nx),jj=1,ny)
        END DO
      END IF


#ifdef V5D
      IF ( output_type == 'v5d' ) THEN   !!! Create the vis5d file
        DO kk = 1,nz
        DO jj = 1,nx
        DO ii = 1,ny
          v5d_data_out(ii,jj,kk) = data_out(jj,ny-ii+1,kk)
        END DO
        END DO
        END DO


        ivar = 0
        DO ii = 1,numvars
          IF ( trim(varnames(ii)) == trim(cname) ) THEN 
            ivar = ii
          END IF
        END DO 
        call v5dSetUnits ( ivar, cunits )
        ret = v5dwrite ( time+1, ivar, v5d_data_out )
      END IF
#endif


      IF (time == 0) THEN
        IF ( cname(1:3) == 'clf' ) then
          dummy = ",clfr,"
        ELSE
          dummy = ","//trim(cname)//","
        END IF
        is_there = INDEX(could_not_find,trim(dummy))
        DO WHILE ( is_there /= 0 )
          could_not_find = could_not_find(1:is_there)//could_not_find(is_there+len_trim(dummy):len_trim(could_not_find))
          is_there = INDEX(could_not_find,trim(dummy))
        END DO
        ivars = ivars + 1
        CALL mprintf(( ivars > MAX_NVARS), ERROR, &
             'Number of processed variables exceeds MAX_NVARS, please check and adjust MAX_NVARS in src/output_module.F90')
        WRITE(ctl_var_string(ivars),'(A,"  ",i4,"  0  ",A," (",A,")")') cname(1:10),nz,trim(cdesc),trim(cunits)
      ENDIF


   END SUBROUTINE write_dat

END MODULE output_module
