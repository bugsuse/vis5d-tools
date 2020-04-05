	SUBROUTINE MAIN0
C************************************************************************
C* MAIN0								*
C*									*
C* This routine will take data from a GEMPAK grid file and write it	*
C* to a file for VIS5D.							*
C*									*
C* Log:									*
C* S. Jacobs/SSAI	 3/92						*
C* K. Brill/NMC		12/93	Changes for layer diagnostics, user	*
C*				specification of GAREA, horizontal	*
C*				interpolation, and other minor changes	*
C* K. Brill/NMC		04/94	Added changes for GEMPAK user interface	*
C* K. Brill/NMC		 9/94	Changes to use just 2 pres levels	*
C* K. Brill/NMC		 9/94	Allow OMEG for W to be in a function	*
C* K. Brill/NMC		 9/94	CALL GD_RDAT for HGHT fields		*
C* K. Brill/NMC		 3/95	Changes for verification differences	*
C* K. Brill/NMC		 5/95	Make GDATIM bigger			*
C* K. Brill/NMC		10/95	Changes for un-interpolated grid output *
C************************************************************************
	INCLUDE		'GEMINC:GEMPRM.PRM'
	PARAMETER	( NTVAR = 32, NPMAX = 2, NZMAX = 40 )
        PARAMETER       ( RMISSV = 1.1e30, RMSS = 1.0e30 )
	PARAMETER	( KKMXGD = LLMXGD/2 )
C*
	CHARACTER	gdfile*72, gfunfn*72, gvcord*4, levels*32,
     +			garea*48, deltax*24, gdatim*128, gname*4
C*
	CHARACTER       parm*12, glevel*24, vldtm*20, fortm*20,
     +			gfunc(NTVAR)*72, pfunc*72, time(2)*20,
     +			timarr(LLMXTM)*48, cname(NTVAR)*4, gftmp*72,
     +			gdattm*48, gdcur*72
	CHARACTER	zfunc*32, errstr*32, cbuf*24, lsttim(2)*20
	CHARACTER*4	prj
	LOGICAL		lyrflg (NTVAR)
C*
	REAL            rlvl(LLMXLV), grid(LLMXGD), xgrel (KKMXGD),
     +			hght(KKMXGD*NPMAX), ygrel (KKMXGD),
     +			grdout(KKMXGD*NPMAX), zlvl (LLMXLV),
     +			grdoutz(KKMXGD*NZMAX), sfcz (KKMXGD),
     +			grdll(KKMXGD), grid2 (LLMXGD)
C*
	INTEGER         level(2), id(8), idir(64),
     +			intdft(3), kkxy (2), ighdr (128)
	REAL		rltln (4), zinpt (3)
	LOGICAL		respnd, done, proces, verify
C*
	INCLUDE		'GEMINC:ERMISS.FNC'
C------------------------------------------------------------------------
1000	FORMAT ( /,'Processing time : ', A )
1001	FORMAT ( '     Processing parameter : ', A )
C========================================================================
C-----------------------------------------------------------------------
        proces = .true.
C
C*	Set up for GEMPAK.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .eq. 0 )  THEN
	    mode = 1
	    CALL GG_INIT ( mode, iret )
	    done = .false.
	  ELSE
	    done = .true.
	END IF
	CALL IP_IDNT  ( 'GEMVIS', ier )
C
C*	Main processing loop.
C
	DO WHILE  ( .not. done )
C
C*	  Initialize IDIR for VIS5D
C
	  DO i = 1, 64
	    idir (i) = 0
	  END DO
C
C*	  Read user input.
C
          CALL GET_USR ( gdfile, gfunfn, garea, deltax, levels,
     +			 gvcord, gdatim, cname, lyrflg, nvar,
     +			 gfunc, gname, iret )
	  IF ( iret .ne. 0 ) proces = .false.
C
C*	  Compute size of 3D grid for VIS5D.
C
	  CALL ST_RLST ( deltax, ' ', 0., 1, dx, num, ier )
	  IF ( dx .eq. 0. ) dx = 2.
	  IF ( ier .eq. 0 .and. dx .gt. 0. ) THEN
	      CALL ST_RLST ( garea, ';', 0., 4, rltln, num, ier )
	      IF ( num .ne. 4 ) THEN
	      	iret = -8
	      	errstr = 'Invalid GAREA'
	      	CALL ER_WMSG ( 'GEMVIS', iret, errstr,ier )
	      	proces = .false.
	      END IF
	      kkxy (1) = NINT ( ABS ( rltln (2) - rltln (4) ) / dx )
     +			 + 1
	      IF ( kkxy (1) .lt. 12 ) kkxy (1) = 12
	      kkxy (2) = NINT ( ABS ( rltln (1) - rltln (3) ) / dx )
     +			 + 1
	      IF ( kkxy (2) .lt. 12 ) kkxy (2) = 12
	      IF ( kkxy (1) * kkxy (2) .gt. KKMXGD ) THEN
		iret = -9
		errstr = 'Invalid DELTAX -- grid too big'
		CALL ER_WMSG ( 'GEMVIS', iret, errstr, ier )
		proces = .false.
	      END IF
	      xlats = rltln (1)
	      xlatn = rltln (3)
	      xlonw = -rltln (2)
	      xlone = -rltln (4)
	      nlat  = kkxy (2)
	      nlon  = kkxy (1)
	      xlatin = (xlatn-xlats)/(nlat-1)
	      xlonin = (xlonw-xlone)/(nlon-1)
	  END IF
	  CALL ST_RLST ( levels, '-', 0., 3, zinpt, num, ier )
	  IF ( num .ne. 3 .or. zinpt (3) .eq. 0.0 ) THEN
	    iret = -7
	    errstr = 'Invalid LEVELS'
	    CALL ER_WMSG ( 'GEMVIS', iret, errstr, ier )
	    proces = .false.
	  END IF
C*
	  nhgt = ( zinpt (2) - zinpt (1) ) / zinpt (3)
	  IF ( nhgt .le. 0 .or. nhgt .gt. NZMAX ) THEN
	    iret = -7
	    errstr = 'Invalid LEVELS'
	    CALL ER_WMSG ( 'GEMVIS', iret, errstr, ier )
	    proces = .false.
	  END IF
	  xhgtb = zinpt (1)
	  xhgtt = xhgtb + nhgt * zinpt (3)
	  xhgtin = zinpt (3)
	  nhgt = nhgt + 1
C
C*	  Generate an array of output z levels.
C
	  zlvl (1) = xhgtb
	  DO k = 2, nhgt
	    zlvl (k) = xhgtb + FLOAT (k-1) * xhgtin
	  END DO
C
C*	  Set up for VIS-5D
C
	  DO  i = 1, 8
	    id(i) = LIT ('    ')
	  END DO
C
C*	  Open grid file.
C
	  iplus = INDEX ( gdfile, '+' )
	  IF ( iplus .ne. 0 ) THEN
	    verify = .true.
	    gdfile = gdfile ( 1: (iplus-1) )
	  ELSE
	    verify = .false.
	  END IF
	  gdcur = ' '
	  CALL GR_FILE  ( gdfile, .false., gdcur, igdfln, lsttim,
     +                    maxgrd, iret )
	  IF ( iret .ne. 0 ) THEN
	    iret = -4
	    errstr = ' GDFILE open failed.'
	    CALL ER_WMSG ( 'GEMVIS', iret, errstr, ier )
	    proces = .false.
	  END IF
C
C*	  If the grid is un-interpolated, get the grid dimensions.
C*	  In this case, lat/lon are faked.
C
	  IF ( dx .lt. 0.0 ) THEN
	    IF ( verify ) THEN
		iret = -14
		errstr = 'Cannot verify if lat/lon faked.'
	    	CALL ER_WMSG ( 'GEMVIS', iret, errstr, ier )
	    	proces = .false.
	    END IF
	    CALL GQGPRJ ( prj, a1, a2, a3, kkxy (1), kkxy (2),
     +			  dlatl, dlonl, dlatu, dlonu, ier )
	    IF ( ier .eq. 0 ) THEN
	      IF ( kkxy (1) * kkxy (2) .gt. KKMXGD ) THEN
		iret = -13
		errstr = 'GEMPAK grid too big'
		CALL ER_WMSG ( 'GEMVIS', iret, errstr, ier )
		proces = .false.
	      END IF
	      xlats = .10
	      xlatn = FLOAT ( kkxy (2) ) / 10.
	      xlonw = FLOAT ( kkxy (1) ) / 10.
	      xlone = .10
	      nlat  = kkxy (2)
	      nlon  = kkxy (1)
	      xlatin = .10
	      xlonin = .10
	    ELSE
	      iret = -12
	      errstr = 'GEMPAK grid navigation error'
	      CALL ER_WMSG ( 'GEMVIS', iret, errstr, ier )
	      proces = .false.
	    END IF
	  END IF
C
C*	  Do VIS5D file setup.
C
	  IF ( proces ) THEN
	      CALL ST_NUMB ( gname, igridf, ier )
	      IF ( igridf .le. 0 .or. igridf .gt. 9999 ) igridf = 1
	      isz3d = nlat*nlon*nhgt
	      isz2d = nlat*nlon
	      CALL IGMK3D ( igridf, id, isz3d )
	      IF  ( igridf .lt. 1 .or. igridf .gt. 9999 )  THEN
	        CALL EDEST ( 'Bad grid file number ',  igridf )
	        CALL EDEST ( 'Must be between 1 and 9999 ', 0 )
	        CALL EDEST ( 'Usage:  gemvis file#', 0 )
	        proces = .false.
	      END IF
	  END IF
C
C*	  Get all times in the GEMPAK file. Convert to Julian day.
C
	  CALL GDT_TIM ( igdfln, gdatim, ntimes, timarr, ier )
	  CALL TG_CTOI ( timarr(1), intdft, ier )
          intdft(3) = intdft(3) - 100000
	  CALL JUL_DAY ( intdft(1), jday, iret )
	  jtime2 = intdft(2) + intdft(3)
	  IF  ( jtime2 .ge. 2400 )  THEN
	    iyy = intdft (1) / 10000
            IF ( MOD ( iyy, 4 ) .eq. 0 ) THEN
	        maxday = 366
	    ELSE 
	        maxday = 365
	    END IF
	    jday2 = jtime2 / 2400 
	    jtime2 = jtime2 - jday2*2400
	    jday = jday + jday2
	    IF ( MOD ( jday, 1000 ) .gt. maxday ) THEN
		jday = jday - maxday + 1000
		IF ( jday .gt. 100000 ) jday = jday - 100000
	    END IF
	  END IF
	  jtime = jtime2 * 100
	  IF  ( ntimes .gt. 1 )  THEN
	    CALL TG_DIFF ( timarr(2), timarr(1), nmin, ier )
	  ELSE
	    nmin = 0
	  END IF
	  ihour = nmin / 60
	  imin  = MOD ( nmin, 60 )
	  jstep = ihour * 10000 + imin * 100
C*
	  iday  = IDAYS(jday)
	  isec  = ISECS(jtime)
C
C*	  Generate grid-relative positions of points on the
C*	  VIS5D lat/lon grid.
C
	  IF ( dx .gt. 0. .and. proces ) THEN
	      CALL SET_PNT ( xlatn, xlonw, xlatin, xlonin, nlat,
     +		             nlon, xgrel, ygrel, ier )
	  ELSE IF ( proces ) THEN
	      icnt = 0
	      DO j = 1, nlon
		DO i = nlat, 1, -1
		  icnt = icnt + 1
		  xgrel (icnt) = FLOAT (j)
		  ygrel (icnt) = FLOAT (i)
		END DO
	      END DO
	  END IF
C
C*	  Get top and bottom levels.
C
	  CALL ST_LCUC ( gvcord, gvcord, ier )
	  CALL LV_CORD ( gvcord, gvcord, ivcord, iret )
C*
	  IF ( iret .ne. 0 ) THEN
	    iret = -3
	    errstr = 'Invalid GVCORD'
	    CALL ER_WMSG ( 'GEMVIS', iret, errstr, ier )
	    proces = .false.
	  END IF
C
C*	  Set up for VIS5D header.
C
	  idir( 1) = isz3d
	  idir( 2) = nlat
	  idir( 3) = nlon
	  idir( 4) = nhgt
	  idir(22) = 4
	  idir(23) = NINT ( xlatn  * 10000. )
	  idir(24) = NINT ( xlonw  * 10000. )
	  idir(25) = NINT ( xlatin * 10000. )
	  idir(26) = NINT ( xlonin * 10000. )
	  idir(31) = 1
	  idir(32) = NINT ( xhgtt  *  1000. )
	  idir(33) = NINT ( xhgtin *  1000. )
C
C*	  Start time loop.
C
	  itm = 0
	  DO WHILE ( itm .lt. ntimes .and. proces )
	    itm = itm + 1
	    WRITE ( 6, 1000 ) timarr(itm)
	    gdattm = timarr(itm)
	    fortm = gdattm
	    idir(6) = IYYDDD(iday)
	    idir(7) = IHMS(isec)
C
C*	    At the first time, get the surface elevation and
C*	    set levels.
C
	    IF ( .not. verify .and. itm .eq. 1 ) THEN
        	CALL SET_LVS ( igdfln, gdattm, gvcord, xgrel, ygrel,
     +                         isz2d, sfcz, nlev, rlvl, ier )
	    END IF
C
C*	    Start parameter loop.
C
	    m = 1
	    ipass = 1
	    DO  WHILE ( m .le. nvar .and. proces )
		WRITE ( 6, 1001 ) cname(m)
	        IF ( verify ) THEN
		    IF ( ipass .eq. 1 ) THEN
			CALL GD_CLOS ( igdfln, ier )
	  		gdcur = ' '
	  		CALL GR_FILE  ( gdfile, .false., gdcur,
     +					igdfln, lsttim,
     +                    		maxgrd, ier )
			gdattm = fortm
	  		CALL SET_PNT ( xlatn, xlonw, xlatin, xlonin,
     +				       nlat, nlon, xgrel, ygrel, ier )
        		CALL SET_LVS ( igdfln, gdattm, gvcord,
     +				       xgrel, ygrel, isz2d, sfcz,
     +				       nlev, rlvl, ier )
		    ELSE
			CALL GD_CLOS ( igdfln, ier )
		    	CALL OPN_VFY ( fortm, vldtm, igdfln, ier )
		    	IF ( ier .ne. 0 ) THEN
		            iret = ier
		            errstr = ' Could not open FNL file.'
	    	            CALL ER_WMSG ( 'GEMVIS', iret,
     +					   errstr,ier )
		            proces = .false.
		        ELSE
	  		    CALL SET_PNT ( xlatn, xlonw,
     +					   xlatin, xlonin,
     +				           nlat, nlon, xgrel, ygrel,
     +					   ier )
        		    CALL SET_LVS ( igdfln, vldtm, gvcord,
     +				           xgrel, ygrel, isz2d, sfcz,
     +				           nlev, rlvl, ier )
			    gdattm = vldtm
			END IF
		    END IF
	        END IF
C
C*		Set output grid to missing.
C
		IF ( ipass .eq. 1 ) THEN
		    DO ijk = 1, isz3d
		        grdoutz (ijk) = RMISSV
		    END DO
		END IF
C
C*              Check for single level grid.
C
		iq = INDEX ( gfunc (m), '@' ) +
     +			INDEX ( gfunc (m), '%' )
                IF ( iq .ne. 0 .and. proces ) THEN
                    level2 = 0
                    CALL ST_INCH ( level2, glevel, iret )
C
C*                  Read GEMPAK data and set to a 3-D array.
C
                    CALL DG_GRID ( gdattm, glevel, gvcord,
     +                             gfunc(m), pfunc, grid, igx, igy,
     +                             time, level, ivc, parm, iret )
C*
                    IF  ( iret .ne. 0 )  THEN
C*
		    ELSE
C
C*			Interpolate the data to latlon output grid.
C
			CALL INT_DAT ( 1, xgrel, ygrel, isz2d, igx,
     +				       igy, grid, grdll, ier )
			ijk = 0
                    	DO k = 1 , nhgt
			    DO ij = 1, isz2d
				ijk = ijk + 1
				IF ( ipass .eq. 1 ) THEN
				    grdoutz (ijk) = grdll (ij)
				ELSE IF ( grdll (ij) .lt. RMSS
     +				    .and. grdoutz (ijk) .lt. RMSS )
     +				    THEN
				    grdoutz (ijk) = -grdoutz (ijk) -
     +					( grdll (ij) + 2. * offst )
				END IF
			    END DO
			END DO
		    END IF
                ELSE IF ( proces ) THEN
C
C*		    Compute multi-level parameters.
C
		    ndxout = 0
		    ndzout = 0
		    kstrt = 1
		    nprs = 0
		    IF ( lyrflg (m) ) kstrt = 2
		    DO  k = kstrt, nlev
		    	IF ( lyrflg (m) ) THEN
			    lvl1 = INT ( rlvl (k) )
			    lvl2 = INT ( rlvl (k-1) )
			    CALL ST_INLN ( lvl1, glevel, lnth, ier )
			    CALL ST_INLN ( lvl2, cbuf, nln, ier )
			    glevel = glevel (1:lnth) // ':' //
     +					cbuf (1:nln)
			    zfunc = 'LAV(HGHT)'
		    	ELSE
		            lvl1 = rlvl(k)
			    lvl2 = -1
		            CALL ST_INCH ( lvl1, glevel, iret )
			    zfunc = 'HGHT'
		    	END IF
C
C*		    	Read GEMPAK data and set to a 3-D array.
C
		    	CALL DG_GRID ( gdattm, glevel, gvcord,
     +				       gfunc(m), pfunc, grid,
     +				        igx, igy, time, level,
     +				        ivc, parm, iret )
		    	IF  ( iret .ne. 0 )  THEN
C
C*			    Do nothing.
C
			ELSE
			    iom = INDEX ( gfunc (m), 'OMEG' ) +
     +				  INDEX ( gfunc (m), 'omeg' )
		    	    IF  ( iom .ne. 0 .and.
     +					cname(m) .eq. 'W' ) THEN	
			        gftmp = 'DDEN(PRES,TEMP)'
		    	        CALL DG_GRID ( gdattm, glevel, gvcord,
     +				               gftmp, pfunc, grid2,
     +				               igx, igy, time, level,
     +				               ivc, parm, iret )
			        IF  ( iret .eq. 0 )  THEN
			    	    DO ij = 1, igx*igy
				      IF ( .not. ERMISS
     +						( grid (ij) )
     +						.and. .not.
     +				         	ERMISS
     +						( grid2 (ij) ) )
     +				     	    THEN
C
C*					Scale W to cm/s.
C*					OMEG is assumed to be in mb/s,
C*					hence two factors of 100.
C
				        grid(ij) = (-10000.*grid(ij)) /
     +					        (GRAVTY*grid2(ij))
				      ELSE
				    	grid(ij) = RMISSD
				      END IF
			            END DO
			        END IF
			    END IF
		            CALL INT_DAT ( 1, xgrel, ygrel, isz2d, igx,
     +				           igy, grid, grdll, ier )
C
C*			    Get the height on this pressure surface.
C
			    IF ( INDEX ( glevel, ':' ) .ne. 0 ) THEN
			        CALL DG_GRID ( gdattm, glevel, gvcord,
     +				               zfunc, pfunc, grid,
     +					       igx, igy, time, level,
     +					       ivc, parm, iret2 )
			    ELSE
				level (1) = lvl1
				level (2) = lvl2
				time (1) = gdattm
				time (2) = ' '
				CALL GD_RDAT ( igdfln, time, level,
     +					       ivcord, zfunc, grid,
     +					       igx, igy, ighdr,
     +					       iret2 )
			    END IF
			    iret = iret + ier + iret2
			    IF ( iret .eq. 0 ) THEN
			        CALL INT_DAT ( 1, xgrel, ygrel,
     +					       isz2d, igx, igy,
     +					       grid, grid2, ier )
				DO ij = 1, isz2d
				    ndzout = ndzout + 1
				    hght (ndzout) = grid2 (ij)
				END DO
			    	DO ij = 1, isz2d
			            ndxout = ndxout + 1
			            grdout (ndxout) = grdll (ij)
		    	    	END DO
			    	nprs = nprs + 1
C
C*				Interpolate to all Z levels between these
C*				two pressure surfaces.
C
				IF ( nprs .ge. 2 ) THEN
			    	    npl = 2
				ELSE
			    	    npl = 1
				END IF
				CALL PRS_HGT ( nlat, nlon, nhgt,
     +					       ipass, offst, zlvl,
     +				               hght, grdout, npl, sfcz,
     +				               grdoutz, ier )
				IF ( nprs .gt. 1 ) THEN
			    	    ndxout = ndxout / 2
			    	    ndzout = ndzout / 2
				END IF
			    END IF
			END IF
		    END DO
C*
                END IF
		IF ( verify ) THEN
		    CALL ENC_DEC ( ipass, isz3d, offst, grdoutz, ier )
		END IF
		IF ( proces .and. ( ( .not. verify ) .or.
     +		     ( ipass .eq. 2 .and. verify ) ) ) THEN
		    ipass = 1
C
C*		    Write to VIS5D file.
C
		    idir(9) = LIT ( cname(m) )
		    igrid   = m + nvar * (itm-1)
		    CALL IGPT3D ( igridf, -igrid, grdoutz,
     +			          nlat, nlon, nhgt, idir, igno )
		    m = m + 1
		ELSE IF ( proces ) THEN
		    ipass = 2
		END IF
	    END DO
C
C*	    Get next time step.
C
	    isec = isec + ISECS(jstep)
	    iday = iday + isec/(24*3600)
	    isec = MOD ( isec, 24*3600 )
	  END DO
C*
	  CALL IP_DYNM  ( done, ier )
	END DO
C*
	CALL GD_CLOS ( igdfln, ier )
C*
	CALL IP_EXIT  ( iret )
C*
	RETURN
	END
