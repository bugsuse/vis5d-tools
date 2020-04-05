	SUBROUTINE OPN_VFY ( gdattm, validt, igdfln, iret )
C************************************************************************
C* OPN_VFY								*
C*									*
C* This subroutine uses the forecast time in GDATTM to construct the	*
C* path to a verifying final analysis stored in $M_FNL.  $M_FNL is an	*
C* environmental variable pointing to the directory containing the	*
C* final analyses.							*
C*									*
C* OPN_VFY ( GDATTM, VALIDT, IGDFLN, IRET )				*
C*									*
C* Input parameters:							*
C*	GDATTM		CHAR*		GEMPAK forecast time		*
C*									*
C* Output parameters:							*
C*	VALIDT		CHAR*		Valid time			*
C*	IGDFLN		INTEGER		GEMPAK grid file number		*
C*	IRET		INTEGER		Return code			*
C*					-10 = DG_OFIL failed		*
C*									*
C** 									*
C* Log:									*
C* K. Brill/NMC		 3/95						*
C************************************************************************
	CHARACTER*(*)	gdattm, validt
C*
	CHARACTER*8	ymdh
	CHARACTER*128	verfil, gdcur
	CHARACTER*20	lsttim (2)
C------------------------------------------------------------------------
	iret = 0
C*
	CALL CYC_TIM ( gdattm, validt, ymdh, iret )
	IF ( iret .ne. 0 ) RETURN
	verfil = '$M_FNL/fnl_' // ymdh
	gdcur = ' '
        CALL GR_FILE  ( verfil, .false., gdcur, igdfln, lsttim,
     +                  maxgrd, iret )
	IF ( iret .ne. 0 ) THEN
	    iret = -10
	    RETURN
	END IF
C*
	RETURN
	END
