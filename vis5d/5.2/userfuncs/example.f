C example.f
C
C An example of an external analysis function for VIS-5D version 4.0
C
C The parts in uppercase should be the same for all analysis functions,
C while the parts in lowercase are specific to this example (except
C comments).


C All VIS-5D analysis functions must be named USERFUNC and take the
C following arguments:
C
C   Output arguments:
C     OUTGRID - This is the array which the function computes
C     OUTNL - Number of levels in computed OUTGRID
C     OUTLOWLEV - Lowest level in computed OUTGRID
C
C   Input arguments:
C     INGRID -  Array of all 3-D grids for each available parameter
C     NR,NC - Number of rows and columns in all 3-D grids
C     NL - number of levels in *each* variable's 3-D grid.
C          An array of NL values is needed because each variable's
C          grid can contain a different number of levels.  However,
C          it's often the case that the number of grid levels is
C          the same for all grids.
C     LOWLEV - lowest level in *each* variable's 3-D grid.
C     MAXNL - maximum number of grid levels (max value in NL() array)
C     NVARS - number of physical variables
C     NAMES - names of each parameter
C     DATE - date of this grid in days since January 1, 1900
C            (use the IYYDDD function to convert to YYDDD format)
C     TIME - time of this grid in seconds since midnight
C            (use the IHMS function to convert to HHMMSS format)
C     PROJECTION - which map projection is being used.  See the README
C            file's sections on using the map projection and the
C            v5dCreate function to learn how this parameter and PROJ_ARGS
C            are used.
C     PROJ_ARGS - projection-specific parameters  (See README file)
C     VERTICAL - which vertical coordinate system is being used.  Again,
C            see the README file's documentation on v5dCreate to see how
C            this parameter and VERT_ARGS are used.
C     VERT_ARGS - vertical coordinate system specific parameters.
C
C Here is the row, column, level arrangement of the OUTGRID (and INGRID)
C arrays:
C     OUTGRID(1, 1, 1 ) = North-west-bottom corner
C     OUTGRID(NR,1, 1 ) = South-west-bottom corner
C     OUTGRID(1, NC,1 ) = North-east-bottom corner
C     OUTGRID(NR,NC,1 ) = South-east-bottom corner
C     OUTGRID(1, 1, NL) = North-west-top corner
C     OUTGRID(NR,1, NL) = South-west-top corner
C     OUTGRID(1, NC,NL) = North-east-top corner
C     OUTGRID(NR,NC,NL) = South-east-top corner
C
C The function should return 0 if it completed successfully or C a
C non-zero value if there was an error.
C
C
      INTEGER FUNCTION USERFUNC( OUTGRID, OUTNL, OUTLOWLEV,
     *                           INGRID, NR, NC, NL, LOWLEV, MAXNL,
     *                           NVARS, NAMES,
     *                           DATE, TIME,
     *                           PROJECTION, PROJ_ARGS,
     *                           VERTICAL, VERT_ARGS )
C     ARGUMENTS:
      REAL OUTGRID(NR,NC,MAXNL)
      INTEGER OUTNL, OUTLOWLEV
      REAL INGRID(NR,NC,MAXNL,NVARS)
      INTEGER NR, NC, NL(NVARS), LOWLEV(NVARS), MAXNL
      INTEGER NVARS
      CHARACTER*8 NAMES(NVARS)
      INTEGER DATE, TIME
      INTEGER PROJECTION
      REAL PROJ_ARGS(*)
      INTEGER VERTICAL
      REAL VERT_ARGS(*)

      REAL PROBEVAL

C     LOCAL VARS:
      integer iv, ir, ic, il
      integer itheta

C     Specify number of levels in OUTGRID
      OUTNL = MAXNL
      OUTLOWLEV = 0

C     look for the THETA variable
      itheta = -1
      do iv=1,nvars
         if (names(iv) .eq. 'THET') itheta = iv
      end do

C     if THETA not found, return error
      if (itheta .eq. -1) then
         print *, "Error: THET variable not found!"
         userfunc = 1
         return
      end if

C     let OUTGRID = theta / 2.0
      do ir=1,nr
         do ic=1,nc
            do il=1,maxnl
               if (ingrid(ir,ic,il,itheta) .ge. 1.0e30) then
C                 missing value
                  outgrid(ir,ic,il) = 1.0e35
               else
                  outgrid(ir,ic,il) = ingrid(ir,ic,il,itheta) / 2.0
               endif
            end do
         end do
      end do

C     RETURN 0 IF OK, OTHER NUMBER IF AN ERROR OCCURED
      USERFUNC = 0
      RETURN
      END



C NOTE:  The following two functions are available for you to use in
C your function:

C     Return the position of the probe in grid and Latitude, Longitude
C     and Altitude:
C          PROBEPOS( ROW, COL, LEV, LAT, LON, HGT )
C     where ROW is in [1,NR], COL is in [1,NC], LEV is in [1..MAXNL].

C     Return the value of the specified variable at the current probe
C     location:
C          VALUE = PROBEVAL( VAR )
C     where VAR is in [1,NVARS].

