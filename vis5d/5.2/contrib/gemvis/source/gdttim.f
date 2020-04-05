	SUBROUTINE GDT_TIM  ( iflno, gdatim, npts, timfnd, iret )
C************************************************************************
C* GDT_TIM								*
C*									*
C* This subroutine sets time information for a time series.  In		*
C* addition to the standard GEMPAK conventions for entering times,	*
C* a list of forecast times separated by semicolons is permitted.	*
C*									*
C* GDT_TIM  ( IFLNO, GDATIM, NPTS, TIMFND, IRET )			*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		Grid file number		*
C*	GDATIM		CHAR*		User input date/time		*
C*									*
C* Output parameters:							*
C*	NPTS		INTEGER		Number of points		*
C*	TIMFND (NPTS)	CHAR*		Time values			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -6 = no times found		*
C**									*
C* Log:									*
C* K. Brill/NMC		 4/94						*
C* K. Brill/NMC		 9/94	Check for single time			*
C* K. Brill/NMC		 5/95	Check for list of forecasts		*
C************************************************************************
	INCLUDE		'GEMINC:GEMPRM.PRM'
C*
	CHARACTER*(*)	gdatim, timfnd (*)
C*
	CHARACTER*36	time, timlst (LLMXTM), errstr
	CHARACTER*6	flist (LLMXTM)
C------------------------------------------------------------------------
	iret = 0
	npts = 0
C
C*	Get all times which might have data.
C
	CALL GD_GTIM  ( iflno, LLMXTM, timlst, ntimin, ier )
	IF  ( ntimin .eq. 0 .or. ier .ne. 0 )  THEN
	    iret = -6
	    errstr = 'Error getting GEMPAK time list.'
            CALL ER_WMSG ( 'GEMVIS', iret, errstr, ier )
	    RETURN
	END IF
C*
C
C*	Check to see if a single input time is in the list.
C
	CALL TG_FULL ( gdatim, timlst (1), timlst (ntimin),
     +		       time, ier )
	IF ( ier .eq. 0 ) THEN
	    timfnd (1) = time
	    npts = 1
	    RETURN
	END IF
C
C*	Narrow the list to that specified in TIME.
C
	icoln = INDEX ( gdatim, ';' )
	IF ( icoln .eq. 0 ) THEN
	    CALL TG_RANG  ( gdatim, ntimin, timlst, npts,
     +			    timfnd, ier )
	    IF  ( ier .ne. 0 )  THEN
	    	CALL ER_WMSG  ( 'TG', ier, ' ', ier1 )
	    	iret = -6
	    	RETURN
	    END IF
	ELSE
	    CALL ST_CLST ( gdatim, ';', ' ', LLMXTM, flist,
     +			   npts, ier )
	    IF ( ier .eq. 0 ) THEN
		DO i = 1, npts
		    CALL TG_FULL ( flist (i), timlst (1),
     +				   timlst (ntimin), timfnd (i),
     +				   ier )
		    IF ( ier .ne. 0 ) THEN
			CALL ER_WMSG ( 'TG', ier, ' ', ier1 )
			iret = -6
			RETURN
		    END IF
		END DO
	    ELSE
		iret = -6
		RETURN
	    END IF
	END IF
C*
	RETURN
	END
