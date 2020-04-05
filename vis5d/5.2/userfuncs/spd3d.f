C spd3d.f
C
C VIS-5D analysis function to compute 3-D wind velocity from
C U, V, and W components.

C See example.f for more information about writing functions.


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

C     LOCAL VARS:
      integer var, ir, ic, il
      integer iu, iv, iw
      real u, v, w, spd

C     Specify number of levels in OUTGRID
      OUTNL = MAXNL
      OUTLOWLEV = 0

C     Find the U and V variables
      iu = -1
      iv = -1
      iw = -1
      do var=1,nvars
         if (names(var) .eq. 'U') iu = var
         if (names(var) .eq. 'V') iv = var
         if (names(var) .eq. 'W') iw = var
      end do

C     If U, V or W not found, return error 1
      if (iu .eq. -1 .or. iv .eq. -1 .or. iw .eq. -1) then
         print *, "Couldn't find U, V, and/or W variables!"
         userfunc = 1
         return
      endif

C     Compute 3-D wind speed
      do il=1,maxnl
         do ic=1,nc
            do ir=1,nr
               u = ingrid(ir,ic,il,iu)
               v = ingrid(ir,ic,il,iv)
               w = ingrid(ir,ic,il,iw)
C              Check for missing data
               if (u .ge. 1.0e30 .or. v .ge. 1.0e30 .or.
     *             w .ge. 1.0e30) then
                  spd = 1.0e35
               else
                  spd = sqrt( u*u + v*v + w*w )
               end if
               outgrid(ir,ic,il) = spd
            end do
         end do
      end do

      USERFUNC = 0
      RETURN
      END
