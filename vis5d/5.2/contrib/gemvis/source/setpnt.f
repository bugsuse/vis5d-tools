        SUBROUTINE  SET_PNT ( xlatn, xlonw, xlatin, xlonin, nlat,
     +                        nlon, xgrel, ygrel, iret )
C************************************************************************
C* SET_PNT								*
C*									*
C* This subroutine computes the GEMPAK grid relative positions of	*
C* the points on a VIS5D lat/lon grid.					*
C*									*
C* SET_PNT ( XLATN, XLONW, XLATIN, XLONIN, NLAT, NLON, XGREL,		*
C*	      YGREL, IRET )						*
C*									*
C* Input parameters:							*
C*	XLATN		REAL		North latitude			*
C*	XLONW		REAL		West longitude (+ west)		*
C*	XLATIN		REAL		Latitude increment		*
C*	XLONIN		REAL		Longitude increment		*
C*	NLAT		INTEGER		Number of lat points		*
C*	NLON		INTEGER		Number of lon points		*
C*									*
C* Output parameters:							*
C*	XGREL		REAL		Array of x grid coordinates	*
C*	YGREL		REAL		Array of y grid coordinates	*
C*	IRET		INTEGER		Return code			*
C*									*
C** 									*
C* Log:									*
C* K. Brill/NMC		 1/94						*
C************************************************************************
	INCLUDE		'GEMINC:GEMPRM.PRM'
	REAL		xgrel (*), ygrel (*)
C*
	REAL		rlatx (LLMXGD), rlony (LLMXGD)
C-----------------------------------------------------------------------
	iret = 0
C*
	indx = 0
	DO j = 1, nlon
	    rlon = xlonw - FLOAT (j-1) * xlonin
	    DO i = 1, nlat
		rlat = xlatn - FLOAT (i-1) * xlatin
		indx = indx + 1
		rlatx (indx) = rlat
		rlony (indx) = -rlon
	    END DO
	END DO
C
C*	Transform lat/lon to grid relative positions.
C
	CALL GTRANS ( 'M', 'G', indx, rlatx, rlony, xgrel, ygrel,
     +		      iret )
C*
	RETURN
	END
