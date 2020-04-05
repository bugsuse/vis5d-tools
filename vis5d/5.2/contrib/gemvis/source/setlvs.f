	SUBROUTINE SET_LVS ( igdfln, gdattm, gvcord, xgrel, ygrel,
     +			     isz2d, sfcz, nlev, rlvl, iret )
C************************************************************************
C* SET_LVS								*
C*									*
C* This subroutine sets the surface height and the array of GVCORD 	*
C* levels for the interpolation into the VIS5D height coordinate.	*
C*									*
C* Input parameters:							*
C*	IGDFLN		INTEGER		GEMPAK grid file number		*
C*	GDATTM		CHAR*		GEMPAK grid date time		*
C*	GVCORD		CHAR*		GEMPAK vertical coordinate	*
C*	XGREL (ISZ2D)	REAL		X coords of output grid pts	*
C*	YGREL (ISZ2D)	REAL		Y coords of output grid pts	*
C*	ISZ2D		INTEGER		Number of pts on output grid	*
C*									*
C* Output parameters:							*
C*	SFCZ (ISZ2D)	REAL		Surface height on output grid	*
C*	NLEV		INTEGER		Number of input levels		*
C*	RLVL (NLEV)	REAL		Values on input levels		*
C*	IRET		INTEGER		Return code			*
C*									*
C* Log:									*
C* K. Brill/NMC		 3/95						*
C* K. Brill/NMC		 5/95	Pass time array of length 2 to GD_GLEV	*
C************************************************************************
	INCLUDE		'GEMINC:GEMPRM.PRM'
	PARAMETER	( RMISSV = 1.1e30 )
C*
	CHARACTER	gdattm*48, gvcord*4
	REAL		xgrel (*), ygrel (*), sfcz (*), rlvl (*)
C*
	CHARACTER	pfunc*32, time(2)*20, parm*12
	REAL            grid(LLMXGD)
C*
	INTEGER         level(2), levarr(2,LLMXLV)
C*
	INCLUDE		'GEMINC:ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0 
C*
        CALL DG_GRID ( gdattm, '0', 'NONE',
     +                 'HGHT', pfunc, grid, igx, igy,
     +                 time, level, ivc, parm, ier )
	IF ( ier .ne. 0 ) THEN
	    DO ij = 1, isz2d
		sfcz (ij) = RMISSV
	    END DO
	    time (1) = gdattm
	    time (2) = ' '
	ELSE
	    CALL INT_DAT ( 1, xgrel, ygrel, isz2d, igx,
     +		           igy, grid, sfcz, ier )
	END IF
C
C*	Get levels.
C
	CALL LV_CORD ( gvcord, gvcord, ivcord, iret )
	CALL GD_GLEV ( igdfln, time, ivcord, LLMXLV,
     +		       levarr, nlev, iret )
        IF (levarr(1,nlev) .eq. 0) nlev = nlev - 1
	DO  i = 1, nlev
	    rlvl(i) = FLOAT ( levarr(1,i) )
	END DO
	CALL LV_SORT ( ivcord, nlev, rlvl, iret )
C*
	RETURN
	END
