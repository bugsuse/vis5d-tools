	SUBROUTINE JUL_DAY ( iyymmdd, iyyddd, iret )
C************************************************************************
C* JUL_DAY								*
C*									*
C* This routine converts time from YYMMDD format to YYDDD format.	*
C* (Taken from AOIPS code.)						*
C*									*
C* JUL_DAY ( IYYMMDD, IYYDDD, IRET )					*
C*									*
C* Input parameters:							*
C*	IYYMMDD		INTEGER		Year month day			*
C*									*
C* Output parameters:							*
C*	IYYDDD		INTEGER		Year Julian day			*
C*	IRET		INTEGER		Return code			*
C*									*
C** 									*
C* Log:									*
C* S. Jacobs/SSAI	 3/92						*
C* K. Brill/NMC		 3/94	Documentation; added return code	*
C************************************************************************
	INTEGER		month(12)
C*
	DATA		month /   0,  31,  59,  90, 120, 151,
     +				181, 212, 243, 273, 304, 334  /
C------------------------------------------------------------------------
	iret = 0
C
C*	Parse date in YYMMDD format.
C
	iday = MOD ( iyymmdd, 100 )
	itmp = iyymmdd / 100
	imon = MOD ( itmp, 100 )
	iyr  = itmp / 100
	IF  ( iyr .gt. 40 )  THEN
	    iyear = 1900 + iyr
	ELSE
	    iyear = 2000 + iyr
	END IF
C
C*	Convert to YYDDD format. Check for leap year.
C
	leapday = 0
	IF  ( MOD ( iyear, 4 ) .eq. 0 )  THEN
	    leapday = 1
	    IF  ( MOD ( iyear, 100 ) .eq. 0 )  THEN
		leapday = 0
		IF  ( MOD ( iyear, 400 ) .eq. 0 )  THEN
		    leapday = 1
		END IF
	    END IF
	END IF
	iyyddd = iyr * 1000 + month(imon)+ iday + leapday
C*
	RETURN
	END
