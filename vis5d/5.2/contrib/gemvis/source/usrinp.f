        SUBROUTINE USR_INP ( gdfile, gfunc, garea, deltax, levels,
     +                       gvcord, gdatim, gname, iret )
C************************************************************************
C* USR_INP								*
C*									*
C* This subroutine gets the input parameters for GEMVIS.		*
C*									*
C* USR_INP ( GDFILE, GFUNC, GAREA, DELTAX, LEVELS, GVCORD, GDATIM,	*
C*	     GNAME, IRET )						*
C**									*
C* Log:									*
C* K. Brill/NMC		 4/94						*
C* K. Brill/NMC		 9/94	Added GNAME				*
C************************************************************************
	CHARACTER*(*)  gdfile, gfunc, garea, deltax, levels, gvcord,
     +		       gdatim, gname
C------------------------------------------------------------------------
	CALL IP_STR  ( 'GDFILE',  gdfile,  ier1 )
	CALL IP_STR  ( 'GFUNC',   gfunc,   ier2 )
	CALL IP_STR  ( 'GAREA',   garea,   ier3 )
	CALL IP_STR  ( 'DELTAX',  deltax,  ier4 )
	CALL IP_STR  ( 'LEVELS',  levels,  ier5 )
	CALL IP_STR  ( 'GVCORD',  gvcord,  ier6 )
	CALL IP_STR  ( 'GDATTIM', gdatim,  ier7 )
	CALL IP_STR  ( 'GNAME',   gname,   ier8 )
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6 + ier7 + ier8
	IF  ( iret .ne. 0 )  iret = -2
C*                         
	RETURN
	END
