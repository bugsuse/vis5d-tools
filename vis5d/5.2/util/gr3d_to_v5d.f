C     gr3d_to_v5d.f

C     convert a McIDAS GR3Dnnn file to a .v5d file.
C
C     Usage:
C        gr3d_to_v5d n m outfile compress
C     Where:
C        n = McIDAS GR3D file number in 1..9999
C        m = number of GR3D files to read
C        outfile = name of .v5d output file
C        compress = bytes per grid point (default = 1)
C
C     Example:
C        gr3d_to_v5d 100 4 data.v5d
C     Converts the data in GR3D0100, GR3D0101, GR3D0102, GR3D0103 to data.v5d.


      SUBROUTINE MAIN0
      include "vis5d.h"

C     Common Blocks
      COMMON/JTIME/NTIMES,NPARMS,MR,MC,ML,
     *XLATN,XLONW,XHGTT,XLATIN,XLONIN,XHGTIN,
     *JDAY(NTIME),JTIME(NTIME),JPARM(NPARM)
      COMMON/NGRID/IGRIDF,NGRIDF,NGRID(NFILE)

C     Local Variables
      CHARACTER*12 CFILE,STRARG
      CHARACTER*10 VARNAME(100)
      CHARACTER*4 JJPARM(NPARM)
      REAL XHGTB
      INTEGER ICOMP, NL(NPARM)
      INTEGER PROJ, VERT
      REAL PARGS(4), VARGS(2)

      EQUIVALENCE (JPARM, JJPARM)

C     get command line args
      IGRIDF=IPP(1,IGCF3D(0))
      NGRIDF=IPP(2,1)
      cfile = strarg( 3 )
      ICOMP = IPP(4, 1)

C     read GR3D header
      CALL GET5D

C     Vertical
      XHGTB = XHGTT - XHGTIN * (ML-1)

C     Copy variable names
      DO I=1,NPARMS
         VARNAME(I) = JJPARM(I)
         NL(I) = ML
      ENDDO

C     Convert times to HHMMSS format and days to YYDDD format
      DO I=1,NTIMES
         JTIME(I) = IHMS(JTIME(I))
         JDAY(I) = IYYDDD(JDAY(I))
      ENDDO

      PROJ = 1
      PARGS(1) = XLATN
      PARGS(2) = XLONW
      PARGS(3) = XLATIN
      PARGS(4) = XLONIN
      VERT = 1
      VARGS(1) = XHGTB
      VARGS(2) = XHGTIN

C     create output file
      call v5dcreate( CFILE, NTIMES, NPARMS, MR, MC, NL,
     *                VARNAME, JTIME, JDAY, ICOMP,
     *                PROJ, PARGS, VERT, VARGS )
C      call v5dcreatesimple( CFILE, NTIMES, NPARMS, MR, MC, ML,
C     *                      VARNAME, JTIME, JDAY,
C     *                      XLATN, XLATIN, XLONW, XLONIN,
C     *                      XHGTB, XHGTIN )

      MRMCML = MR*MC*ML
      CALL COMPRS(MRMCML)

      call v5dclose
      RETURN
      END



C
C Read grids from McIDAS file and write to .v5d file
C
      SUBROUTINE COMPRS(MRMCML)
      include "vis5d.h"
C     Parameters
      integer mrmcml

C     Common Blocks
      COMMON/JTIME/NTIMES,NPARMS,MR,MC,ML,
     *  XLATN,XLONW,XHGTT,XLATIN,XLONIN,XHGTIN,
     *  JDAY(NTIME),JTIME(NTIME),JPARM(NPARM)
      COMMON/NGRID/IGRIDF,NGRIDF,NGRID(NFILE)

C     Local Variables
      REAL*4 GRIDK(200*200*100)

      DO IT=1,NTIMES
         DO IP=1,NPARMS

C           DETERMINE WHICH McIDAS FILE AND GRID WE'RE GETTING
            IIGRID = NPARMS * (IT-1) + IP
            IIFILE = 1
            DO WHILE ( (IIGRID .GT. NGRID(IIFILE))
     *                 .AND. (IIFILE .LE. NGRIDF) )
               IIGRID = IIGRID - NGRID(IIFILE)
               IIFILE = IIFILE + 1
            END DO
            IIFILE = IIFILE + IGRIDF - 1
C           WE'VE FOUND IIFILE AND IIGRID

C           Read grid data
            CALL GETGRD(IT,IP,NPARMS,GRIDK,MRMCML)

C           Write grid data
            call v5dwrite( it, ip, gridk )

            call v5dmcfile( it, ip, iifile, iigrid )

         ENDDO

      ENDDO

      RETURN
      END



