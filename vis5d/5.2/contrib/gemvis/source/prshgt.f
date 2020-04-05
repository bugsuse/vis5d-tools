	SUBROUTINE PRS_HGT ( nlat, nlon, nhgt, ipass, offst,
     +			      zlvl, hght, grdin,
     +			      nprs, sfcz, grdz, iret )
C************************************************************************
C* PRS_HGT								*
C*									*
C* This routine will interpolate from pressure to height coordinates.	*
C* If the Z level is not found, no change is made in GRDZ.		*
C*									*
C* If NPRS = 1, below ground values are stored.				*
C*									*
C* IF IPASS = 2, the interpolated value is subtracted from the value	*
C* in GRDZ.  In this case, the values in GRDZ are negative everwhere	*
C* because of the addition of 3*OFFST, where OFFST is the range of the	*
C* pass 1 data, followed by an additive inverse.  The differencing	*
C* operation using the second pass interpolated value ZVALUE is:	*
C*									*
C*                - GRDZ (i,j,k) - ( ZVALUE + 2 * OFFST )		*
C*									*
C* The resulting value is the difference plus OFFST, which, if OFFST	*
C* is chosen appropriately, will be positive everywhere.  This allows	*
C* missing values to be assigned at points where no interpolation	*
C* occurred during pass 2, because such points will still have negative	*
C* values.								*
C*									*
C* PRS_HGT ( NLAT, NLON, NHGT, IPASS, OFFST, ZLVL, HGHT, GRDIN,		*
C*	     NPRS, SFCZ, GRDZ, IRET )					*
C*									*
C* Input parameters:							*
C*	NLAT		INTEGER		Number of latitudes		*
C*	NLON		INTEGER		Number of longitudes		*
C*	NHGT		INTEGER		Number of heights		*
C*	IPASS		INTEGER		Pass number (if > 1 do a diff)	*
C*	OFFST		REAL		Offset value when differencing	*
C*	ZLVL		REAL		Array of output heights		*
C*	HGHT		REAL		3D grids of input heights on p	*
C*	GRDIN		REAL		3D grids of input parm on p	*
C*	NPRS		INTEGER		Number of p levels		*
C*	SFCZ		REAL		2D surface elevation		*
C*									*
C* Output parameters:							*
C*	GRDZ		REAL		3D output on z			*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	 4/92						*
C* K. Brill/NMC		01/94	Rewrote completely			*
C* K. Brill/NMC		 9/94	Load upper level down for NPRS = 2	*
C*				Set subsurface values from lowest lvl	*
C* K. Brill/NMC		 5/95	Changes for differences (ipass > 1)	*
C************************************************************************
	PARAMETER	( RMISSV = 1.1e30, RMSS = 1.0e30 )
	REAL		hght ( nlat, nlon, nprs ),
     +			grdin ( nlat, nlon, nprs ),
     +			grdz ( nlat, nlon, nhgt ),
     +			sfcz ( nlat, nlon ),
     +			zlvl ( nhgt )
	LOGICAL		found
C*
C------------------------------------------------------------------------
	iret = 0
	IF ( nprs .eq. 0 ) RETURN
	DO  j = 1, nlon
	    DO  i = 1, nlat
C*
		DO  k = 1, nhgt
		    IF ( nprs .eq. 1 .and. sfcz (i,j) .lt. RMSS .and.
     +			 zlvl (k) .lt. sfcz (i,j) ) THEN
			IF ( ipass .eq. 1 ) THEN
			    grdz (i,j,k) = grdin (i,j,1)
			ELSE
			    IF ( grdz (i,j,k) .lt. RMSS .and.
     +				 grdin (i,j,1) .lt. RMSS )
     +			    grdz (i,j,k) = -grdz(i,j,k) -
     +				( grdin (i,j,1) + 2. * offst )
			END IF
		    END IF
		    ipp = 2
		    ipm = 1
		    found = .false.
		    DO WHILE ( .not. found .and. ipp .le. nprs )
			IF ( hght (i,j,ipm) .lt. RMSS .and.
     +			     hght (i,j,ipp) .lt. RMSS .and.
     +			     zlvl (k) .ge. hght (i,j,ipm) .and.
     +			     zlvl (k) .le. hght (i,j,ipp) ) THEN
C
C*			    Do linear interpolation in z.
C
			    IF ( grdin (i,j,ipm) .lt. RMSS .and.
     +			         grdin (i,j,ipp) .lt. RMSS ) THEN
		    	    	rmfc = ( grdin (i,j,ipp)
     +			    	           - grdin (i,j,ipm) ) /
     +			    	( hght (i,j,ipp) - hght (i,j,ipm) )
				zvalue = grdin (i,j,ipm) +
     +					 rmfc * ( zlvl (k) -
     +				         hght (i,j,ipm) )	
				IF ( ipass .eq. 1 ) THEN
				    grdz (i,j,k) = zvalue
				ELSE
				    IF ( grdz (i,j,k) .lt. 0.0 )
     +				    grdz (i,j,k) = -grdz (i,j,k) -
     +					 ( zvalue + 2. * offst )
				END IF
				found = .true.
			    ELSE
				grdz (i,j,k) = RMISSV
				found = .true.
			    END IF
			ELSE
			    ipp = ipp + 1
			    ipm = ipm + 1
			END IF
		    END DO
		END DO
C*
	    END DO
	END DO
C
C*	If NPRS = 2, load upper level down.
C
	IF ( nprs .eq. 2 ) THEN
	    DO j = 1, nlon
		DO i = 1, nlat
		    grdin (i,j,1) = grdin (i,j,2)
		    hght  (i,j,1) = hght  (i,j,2)
		END DO
	    END DO
	END IF
C*
	RETURN
	END
