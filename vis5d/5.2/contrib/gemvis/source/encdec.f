	SUBROUTINE ENC_DEC ( ipass, isz3d, offst, grdz, iret )
C************************************************************************
C* ENC_DEC								*
C*									*
C* This subroutine encodes and decodes a 3D data field when difference	*
C* calculations are being done.  This is done so that the difference	*
C* field is assigned values only at places where values can be		*
C* interpolated in both pass 1 and pass 2 without having to use two 3D	*
C* data arrays.								*
C*									*
C* If IPASS = 1, the values are encoded by finding the range of the	*
C* data, this becomes the value of OFFST.  The values of GRDZ then	*
C* become:								*
C*									*
C*	            - ( GRDZ (ijk) + 3 * OFFST )			*
C*									*
C* IF IPASS = 2, the values in GRDZ are decoded.  Negative values	*
C* were not interpolated in pass 2 and are set to missing.  Positive	*
C* values are decoded as follows:					*
C*									*
C*		      GRDZ (ijk) - OFFST				*
C*									*
C* This is the difference value, because in PRS_HGT the differencing	*
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
C* ENC_DEC ( IPASS, ISZ3D, OFFST, GRDZ, IRET )				*
C*									*
C* Input parameters:							*
C*	IPASS		INTEGER		Pass number 			*
C*	ISZ3D		INTEGER		Number of 3D grid points	*
C*									*
C* Input and output parameters:						*
C*	OFFST		REAL		Offset value when differencing	*
C*	GRDZ		REAL		3D output on z			*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* K. Brill/NMC		 5/95						*
C************************************************************************
	PARAMETER	( RMISSV = 1.1e30, RMSS = 1.0e30 )
C*
	REAL		grdz ( * )
C*
C------------------------------------------------------------------------
	iret = 0
C*
	IF ( ipass .eq. 1 ) THEN
	    rmin = 1.E22
	    rmax = - 1.E22
	    DO ijk = 1, isz3d
		IF ( grdz (ijk) .lt. RMSS ) THEN
		    IF ( grdz (ijk) .lt. rmin ) rmin = grdz (ijk)
		    IF ( grdz (ijk) .gt. rmax ) rmax = grdz (ijk)
		END IF
	    END DO
	    offst = ( rmax - rmin ) * 1.1
	    DO ijk = 1, isz3d
		IF ( grdz (ijk) .lt. RMSS ) THEN
		    grdz (ijk) = - ( grdz (ijk) + 3. * offst )
		END IF
	    END DO
	END IF
C*
	IF ( ipass .eq. 2 ) THEN
	    DO ijk = 1, isz3d
		IF ( grdz (ijk) .lt. RMSS ) THEN
		    IF ( grdz (ijk) .lt. 0.0 ) THEN
			grdz (ijk) = RMISSV
		    ELSE
			grdz (ijk) = grdz (ijk) - offst
		    END IF
		END IF
	    END DO
	END IF
C*
	RETURN
	END
