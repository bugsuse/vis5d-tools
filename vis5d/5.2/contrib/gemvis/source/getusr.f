	SUBROUTINE GET_USR ( gdfile, gfunfn, garea, deltax, levels,
     +			     gvcord, gdatim, cname, lyrflg, nvar,
     +			     gfunc, gname, iret )
C************************************************************************
C* GET_USR								*
C*									*
C* GET_USR ( GDFILE, GFUNFN, GAREA, DELTAX, LEVELS, GVCORD, GDATIM,	*
C*	     CNAME, LYRFLG, NVAR, GFUNC, GNAME, IRET )			*
C*									*
C* This routine will get information from the user about the GEMPAK 	*
C* file and the parameters to use.					*
C*									*
C* If DELTAX is negative, the actual GEMPAK grid is used.  No		*
C* horizontal interpolation is done.  GAREA is ignored. Lat/lon is	*
C* faked.								*
C*									*
C* The information about what functions to compute is read from the	*
C* file whose name is in GFUNFN.  The following describes how 		*
C* GFUNC entries are made in that file:					*
C*									*
C* Each GFUNC entry appears as though it were being typed in a		*
C* GEMPAK program.  There is one entry per line in GFUNFN.		*
C*									*
C* The VIS5D name is either the first 4 characters of GFUNC or the	*
C* first 4 characters placed following an = after the GEMPAK GFUNC	*
C* specification.  For example, pvor(wnd)=potv will assign the		*
C* name potv to the VIS5D field of potential vorticity.			*
C*									*
C* If a function contains @ or % characters it is treated as a		*
C* single level diagnostic.  All single level functions must contain	*
C* either @ or %, or both.						*
C*									*
C* The VIS5D output file name is GR3Dnnnn where nnnn is a number 	*
C* between 1 and 9999 inclusive.  nnnn is specified in GNAME.		*
C*									*
C* Output parameters:							*
C*  GDFILE	CHAR(*)		GEMPAK file name			*
C*  GFUNFN	CHAR(*)		Name of file holding GFUNCs		*
C*  GAREA	CHAR(*)		Bounds of lat/lon output grid		*
C*  DELTAX	CHAR(*)		Increment (deg) on lat/lon output grid	*
C*  LEVELS	CHAR(*)		Z bottom ; Z top ; delta Z		*
C*  GVCORD	CHAR(*)		Vertical coordinate of GEMPAK grids	*
C*  GDATIM	CHAR(*)		Grid time range input			*
C*  CNAME (NVAR)CHAR(*)		Array of VIS5D names			*
C*  LYRFLG(NVAR)LOGICAL		Layer average flags			*
C*  NVAR	CHAR(*)		Number of variables			*
C*  GFUNC (NVAR)CHAR(*)		Array of function names			*
C*  GNAME	CHAR*		Output file number			*
C*  IRET	INTEGER		Return code				*
C*				  = 1  => no variables			*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	 3/92						*
C* K. Brill/NMC		12/93	Changes to input GAREA for subset and	*
C*				horizontal interpolation		*
C* K. Brill/NMC		 3/94	Changes for GEMPAK user interface	*
C* K. Brill/NMC		 4/94   Changed KXKY to DELTAX			*
C* K. Brill/NMC		 9/94	Added GNAME				*
C* K. Brill/NCEP	10/95	Documentation on DELTAX < 0.		*
C************************************************************************
        PARAMETER       ( NTVAR = 32, NPMAX = 40, NZMAX = 40 )
	CHARACTER*(*)	gdfile, gfunc(*), cname(*), gvcord, garea, 
     +			deltax, levels, gfunfn, gdatim, gname
	CHARACTER*32	errstr
	LOGICAL		lyrflg (*)
	CHARACTER*256	hold, hold2 (2)
C------------------------------------------------------------------------
999	FORMAT ( A )
C========================================================================
	iret = 0
C
C*	Get the user input from the GEMPAK user interface.
C
	CALL USR_INP ( gdfile, gfunfn, garea, deltax, levels, gvcord,
     +		       gdatim, gname, iret )
	IF ( iret .ne. 0 ) THEN
	    errstr = 'Error getting input'
            CALL ER_WMSG ( 'GEMVIS', iret, errstr, ier )
	    RETURN
	END IF
C
C*	Open the file for GFUNC inputs.
C
	CALL FL_SOPN ( gfunfn, lun, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -5
            errstr = 'Invalid GFUNC file name'
            CALL ER_WMSG ( 'GEMVIS', iret, errstr, ier )
	    RETURN
	END IF
	i = 0
	hold = 'x'
	DO  WHILE ( hold .ne. ' ' )
	  READ  ( lun,  999, iostat = iostat ) hold
	  IF ( i+1 .gt. NTVAR ) THEN
	    hold = ' '
	  END IF
	  IF ( iostat .ne. 0 ) hold = ' '
	  IF ( hold .ne. ' ' ) THEN
	    CALL ST_LCUC ( hold, hold, ier )
	    i = i + 1
	    CALL ST_RMBL ( hold, hold, leng, ier )
	    CALL ST_CLST ( hold, '=', ' ', 2, hold2, num, ier )
	    IF ( hold2 (2) .ne. ' ' ) THEN
		cname (i) = hold2 (2) (1:4)
	    ELSE
		cname (i) = hold (1:4)
	    END IF
	    gfunc(i) = hold2 (1)
	    CALL CHK_LAV ( gfunc (i), lyrflg (i), ier )
	    CALL ST_LSTR ( gfunc(i), ilens, ier )
C
C*	    Apply VIS5D standard naming conventions.
C
	    IF  ( gfunc(i) .eq. 'UN(WND)' )  cname(i) = 'U   '
	    IF  ( gfunc(i) .eq. 'UN(OBS)' )  cname(i) = 'U   '
C
	    IF  ( gfunc(i) .eq. 'VN(WND)' )  cname(i) = 'V   '
	    IF  ( gfunc(i) .eq. 'VN(OBS)' )  cname(i) = 'V   '
C
	    IF  ( gfunc(i) .eq. 'WWND' )  cname(i) = 'W   '
	    IF  ( gfunc(i) .eq. 'WREL' )  cname(i) = 'W   '
	    IF  ( gfunc(i) .eq. 'OMEG' )  cname(i) = 'W   '
	  END IF
	END DO
	nvar = i
	IF ( i .eq. 0 ) iret = 1
	CALL FL_CLOS ( lun, ier )
C*
	RETURN
	END
