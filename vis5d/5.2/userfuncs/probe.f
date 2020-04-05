C probe.f
C
C From within your function, you can get the current position of the
C probe and the value of each physical variable at that location.  This
C is an example of how to use the probe functions

C See example.f for more information about writing functions.


      INTEGER FUNCTION USERFUNC( OUTGRID, OUTNL, OUTLOWLEV,
     *                           INGRID, NR, NC, NL, LOWLEV, MAXNL,
     *                           NVARS, NAMES,
     *                           DATE, TIME,
     *                           PROJECTION, PROJ_ARGS,
     *                           VERTICAL, VERT_ARGS )
      IMPLICIT NONE
C     ARGUMENTS:
      INTEGER NVARS
      INTEGER NR, NC, NL(NVARS), LOWLEV(NVARS), MAXNL
      REAL OUTGRID(NR,NC,MAXNL)
      INTEGER OUTNL, OUTLOWLEV
      REAL INGRID(NR,NC,MAXNL,NVARS)
      CHARACTER*8 NAMES(NVARS)
      INTEGER DATE, TIME
      INTEGER PROJECTION
      REAL PROJ_ARGS(*)
      INTEGER VERTICAL
      REAL VERT_ARGS(*)

      REAL PROBEVAL

C     LOCAL VARS:
      integer iv, ir, ic, il
      real row, col, lev, lat, lon, alt, value

C     Specify number of levels in OUTGRID
      OUTNL = MAXNL
      OUTLOWLEV = 0

C     get probe position and print it
      call probepos( row, col, lev, lat, lon, alt )
      print *, "probe position:", row,col,lev, lat,lon,alt

C     print probe values
      do iv=1,nvars
         value = probeval( iv )
         print *, "probe value", iv, "=", value
      end do

C     let OUTGRID = first variable
      do ir=1,nr
         do ic=1,nc
            do il=1,maxnl
               if (ingrid(ir,ic,il,1) .ge. 1.0e30) then
C                 missing value
                  outgrid(ir,ic,il) = 1.0e35
               else
                  outgrid(ir,ic,il) = ingrid(ir,ic,il,1)
               endif
            end do
         end do
      end do

C     RETURN 0 IF OK, OTHER NUMBER IF AN ERROR OCCURED
      USERFUNC = 0
      RETURN
      END
