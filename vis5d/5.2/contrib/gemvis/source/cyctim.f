	SUBROUTINE CYC_TIM  ( gdattm, vdattm, ymdh, iret )
C************************************************************************
C* CYC_TIM								*
C*									*
C* This subroutine converts a grid time to the cycle time, which has	*
C* the form YYMMDDHH.							*
C*									*
C* The input string must be a full grid time.  The input and output	*
C* strings may be the same.						*
C*									*
C* CYC_TIM  ( GDATTM, VDATTM, YMDH, IRET )				*
C*									*
C* Input parameters:							*
C*	GDATTM		CHAR*		Grid date/time			*
C*									*
C* Output parameters:							*
C*	VDATTM		CHAR*		Valid time			*
C*	YMDH		CHAR*		YYMMDDHH time			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -11 = time conversion error	*
C**									*
C* Log:									*
C* K. Brill/NMC		 3/95	Derive from TG_VALD in GEMPAK		*
C************************************************************************
	CHARACTER*(*)	gdattm, vdattm, ymdh
C*
	CHARACTER	ftime*12, ccc*12
	INTEGER		idtarr (5)
C------------------------------------------------------------------------
	iret = 0
C
C*	If the forecast type is V replace it with a blank time.
C
	IF  ( gdattm (12:12) .eq. 'V' )  THEN
	    ftime = ' '
	  ELSE
	    ftime = gdattm (13: )
	END IF
C
C*	Get the forecast range.  A sets the i in ihhhmm to zero.
C
	CALL TG_IFTM  ( 'A', ftime, ihhhmm, iret )
	ihour = ihhhmm / 100
	imin  = ihhhmm - ihour * 100
	IF  ( iret .ne. 0 )  THEN
	    iret = -11
	    ymdh = ' '
	    RETURN
	END IF
C
C*	Break the date/time into 5 integers.
C
	ccc = gdattm ( :11 )
	CALL TI_CTOI  ( ccc, idtarr, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret   = -11
	    ymdh = ' '
	    RETURN
	END IF
C
C*	Add forecast minutes.
C
	idtarr (5) = idtarr (5) + imin
	DO  WHILE  ( idtarr (5) .ge. 60 )
	    idtarr (5) = idtarr (5) - 60
	    idtarr (4) = idtarr (4) + 1
	END DO
C
C*	Add forecast hours.
C
	idtarr (4) = idtarr (4) + ihour
	DO  WHILE  ( idtarr (4) .ge. 24 )
	    idtarr (4) = idtarr (4) - 24
	    CALL TI_ADDD  ( idtarr, idtarr, ier )
	END DO
C
C*	Convert the date/time back to character.
C
	CALL TI_ITOC  ( idtarr, vdattm, ier )
	CALL ST_LSTR  ( vdattm, lenvld, ier )
	vdattm = vdattm (:lenvld) // 'F' // '000'
C
C*	Convert to YYMMDDHH format.
C
	islsh = INDEX ( vdattm, '/' )
	ism1 = islsh - 1
	isp1 = islsh + 1
	isp2 = islsh + 2
	ymdh = vdattm ( 1:ism1 ) // vdattm ( isp1:isp2 )
C*
	RETURN
	END
