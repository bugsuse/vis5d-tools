#include "config.h"
INCLUDE 'v5d.incf'
!+ Conversion from grib model output to vis5d format
! 
PROGRAM grib2v5d  

! Description:
!   This is a conversion program for converting lambo data grib format
!   to VIS-5D's v5d format (which is directly read by vis5d).
! 
! Method: 
!   The program Vertically interpolates from model levels to height levels
!   Surface fields can be represented at one level
! 
! Input files:
!   Namelist file, default name vis5d.naml, read here.
!   Grib file, read here and in subroutine readgrib.
! Output files:
!   Vis5d file, written here by calling library subroutine v5dwrite
!
! Current Code Owner: Paolo Patruno, Davide Cesari
!
! History:
!
! Version   Date     Comment
! -------   ----     -------
! <version> <date>   Original code. <Your name>
!
! Code Description:
!   Language:           Fortran 90. 
!   Software Standards: "European Standards for Writing and  
!     Documenting Exchangeable Fortran 90 Code". 
!
! Declarations: 

! Modules used: 

  USE V5D_DEFINITIONS

  IMPLICIT NONE
 
!  INTEGER, EXTERNAL :: iargc  

! Local parameters: 
  INTEGER, PARAMETER :: idimg = 400000

! Local scalars: 
  INTEGER :: n, i, j, it, iv, il, ivc, iug, idrt, ija, nfreeatm, fth, &
   maxnl, projection, nr=imissing, nc=imissing, nrr, ncr, nmi=1, ami,soundlev
  REAL (rkind):: lastlat, firstlat, latinc, firstlon, lastlon, loninc, &
   alarot, alorot, rot, suop

  CHARACTER (LEN=100) :: namlname='v5d.naml', inname='', outname='', arg

! Local arrays: 
  INTEGER :: level (3), var (3), scad (4), data (3), ora (2), &
   datag (3), orag (2), dates (maxtimes), times (maxtimes), ml(1), &
   daymon(12) = (/31,28,31,30,31,30,31,31,30,31,30,31/), &
   ivetvar(maxvars)
  LOGICAL :: whichvar(maxvars), freeatm (maxvars) = .FALSE., interp = .TRUE.&
   , soundstat = .FALSE.
  REAL (rkind):: vcoord(maxlevels*2+4), proj_args (7) = missing, grib(idimg)
  REAL (rkind), ALLOCATABLE :: g(:,:,:,:), values(:,:), avgbuf(:,:), &
   cumbuff(:,:,:,:), cumextrabuff(:,:)
  CHARACTER (len=10) :: varnamep (maxvars)
  INTEGER outnl, outlowlev
  NAMELIST / names / inname, outname


!- End of header ---------------------------------------------------------------

  !-----------------------------------------------------------------------------
  ![1.0] Get input configuration from command line and from namelist file
  !-----------------------------------------------------------------------------
  ! Get file names from command line arguments
  i=1
  args: DO
    CALL getarg(i,arg)
    IF (arg(1:2) == '-n') THEN
      i=i+1
      CALL getarg(i,namlname)
    ELSE IF (arg(1:2) == '-i') THEN
      i=i+1
      CALL getarg(i,inname)
    ELSE IF (arg(1:3) == '-mi') THEN
      nmi=iargc()-i
      i=i+1
      CALL getarg(i,inname)
      ami=i
      EXIT args
    ELSE IF (arg(1:2) == '-o') THEN
      i=i+1
      CALL getarg(i,outname)
    ENDIF
    i=i+1
    IF (i > iargc()) EXIT args
  ENDDO args

  IF (inname == '') THEN
    ! Input grib file name not received from command line, try with namelist
    OPEN (UNIT = 10, FILE = namlname, STATUS = 'old')
    READ (10, NML=names)
    CLOSE (10)
  ENDIF

  IF (outname == '') THEN
    ! Compute output file name from input if not given
    i=INDEX(inname, '.', BACK=.TRUE.)
    j=INDEX(inname, '/', BACK=.TRUE.)
    IF (i == 0 .OR. i < j) THEN
      outname=TRIM(inname) // '.v5d'
    ELSE
      outname=inname(1:i) // 'v5d'
    ENDIF
  ENDIF

  PRINT '(2A)', 'Input file: ',TRIM(inname)
  PRINT '(2A)', 'Output file: ',TRIM(outname)

  ! Read the grib parameters namelist
  OPEN (UNIT = 10, FILE = namlname, STATUS = 'old')  
  READ (10, NML=config)
  CLOSE (10)  

  !-----------------------------------------------------------------------------
  ![2.0] Make some adjustments to input configuration
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  ![2.1] Compute numvars and numtimes if not provided
  !-----------------------------------------------------------------------------
  IF (numvars == imissing) THEN
    IF (ANY(tabvar == imissing)) THEN  ! Find first missing and subtract 1
      numvars=MINVAL( (/(i,i=1,maxvars)/), MASK=(tabvar == imissing)) - 1
    ELSE
      numvars = maxvars
    ENDIF
    PRINT '(I2,A)', numvars, ' variables found.'
  ENDIF
  IF (numtimes == imissing) THEN
    numtimes=0
    IF (ANY(tabfth == imissing)) THEN  ! Find first missing and subtract 1
      numtimes=MAX(MINVAL( (/(i,i=1,maxtimes)/), MASK=(tabfth == imissing)) - 1, &
       numtimes)
    ELSE
      numtimes = maxtimes
    ENDIF
    IF (ANY(tabftd == imissing)) THEN  ! Find first missing and subtract 1
      numtimes=MAX(MINVAL( (/(i,i=1,maxtimes)/), MASK=(tabftd == imissing)) - 1, &
       numtimes)
    ELSE
      numtimes = maxtimes
    ENDIF
    ! Set to null the unset elements
    WHERE (tabfth(1:numtimes) == imissing)
      tabfth(1:numtimes)=0
    END WHERE
    WHERE (tabftd(1:numtimes) == imissing)
      tabftd(1:numtimes)=0
    END WHERE
    tabftm=0  ! Minutes unsupported

    PRINT '(I2,A)', numtimes, ' time levels found.'
  ENDIF

  IF (nmi > numtimes) THEN
    WRITE(*,'(A,I3,A,I3,A)')'Warning: more input files (',nmi, &
     ') than time levels (',numtimes,') were specified.'
  ELSE IF (nmi > 1 .AND. nmi < numtimes) THEN
    WRITE(*,'(A,I3,A,I3,A)')'Error: less input files (',nmi, &
     ') than time levels (',numtimes,') were specified.'
    CALL EXIT(1)
  ENDIF
  !-----------------------------------------------------------------------------
  ![2.2] Set pressure at top, etas**-1 and orography to constant if they exist
  !-----------------------------------------------------------------------------
  IF (ipt>0) THEN
    const(ipt)=.TRUE.
    output(ipt)=.FALSE.
  ENDIF
  IF (ire>0) THEN
    const(ire)=.TRUE.
    output(ire)=.FALSE.
  ENDIF
  IF (igo>0) THEN
    const(igo)=.TRUE.
  ENDIF
  ! Keep constant only surface and not-output fields (interpolation 
  ! overwrites anyway upper air output fields)
  const(1:numvars)=const(1:numvars) .AND. &
   (nzlev(1:numvars) <= 1 .OR. .NOT. output(1:numvars))
!   (nl(1:numvars) <= 1 .OR. .NOT. output(1:numvars))

  !-----------------------------------------------------------------------------
  ![2.3] Compute the input level type
  !-----------------------------------------------------------------------------
  i = getvertlev(typel)
  IF (i /= typel) THEN
    IF (i == 0) THEN
      PRINT '(A,/,A,I4)', &
       'Error: input level type, if provided, should be an upper air level,', &
       'not a surface level like ', typel
      CALL EXIT(1)
    ELSE IF (i == imissing) THEN
      PRINT '(A,I4,A)', 'Error: input level type ', typel, ' not supported'
      CALL EXIT(1)
    ELSE
      typel = i
    ENDIF
  ENDIF

  !-----------------------------------------------------------------------------
  ![2.4] Check the correctness of key variable pointers
  !-----------------------------------------------------------------------------
  IF (itt > numvars .OR. iqq > numvars .OR. ivu > numvars .OR. &
   ivv > numvars .OR. ivw > numvars .OR. ips > numvars .OR. &
   ipt > numvars .OR. ipp > numvars .OR. igo > numvars .OR. &
   ire > numvars) THEN
    PRINT '(A,/,A,I2)', 'Error: at least one of the pointers to key variables i??', &
     'is greater than the number of variables ', numvars
    CALL EXIT(1)
  ENDIF

  !-----------------------------------------------------------------------------
  ![3.0] Open grib file and collect useful information from it
  !-----------------------------------------------------------------------------
  CALL pbopen (iug, inname, 'r', n)  
  IF (n.NE.0) THEN
    PRINT '(A,I3,2A)', 'Error in pbopen, code: ', n, &
     ' while opening ', TRIM(inname)
    CALL EXIT (1)
  ENDIF

  gribinfo: DO
    CALL getinfovep (iug, grib, idimg, data, ora, scad, level, var, &
     firstlat, lastlat, firstlon, lastlon, nr, nc, latinc, loninc, idrt, &
     alarot, alorot, rot, ija, maxlevels*2+4, il, vcoord, n)
    IF (n.NE.0) THEN
      PRINT '(A)', 'Error: no known upper air fields found in grib file'
      CALL EXIT (1)
    ENDIF

    IF ((typel /= imissing .AND. getvertlev(level(1)) == typel .AND. &
     ANY(level(2) == tablev(1:MAXVAL(nl(1:numvars) + lowlev(1:numvars))))) .OR. &  ! Ensure it's a requested level
     (typel == imissing .AND. ANY(level(1) == lfreeatm)) .OR. & ! or an upper air level
     ALL(nl(1:numvars) == 1)) THEN
!     ALL(lowlev(1:numvars) == 0)) THEN
      whichvar(1:numvars) = (var(3) == tabvar(1:numvars)) &
       .AND. (var(2) == tab2ver(1:numvars) .OR. tab2ver(1:numvars) < 0) &
       .AND. (gridtype(1:numvars) == 0)
      IF (ANY(whichvar)) THEN  ! and a known variable not on U or V points
        iv=MINVAL( (/(i,i=1,numvars)/), MASK=whichvar(1:numvars))
        it=MAXVAL(nl)
        typel = getvertlev(level(1))
        IF (il > 0 .AND. typel == 109) THEN  ! Hybrid coordinate
          IF (vctype < 0) THEN  ! Try to guess the type
            IF (il/2 == it .OR. il/2 == it-1) THEN
              vctype = 1
            ELSE IF (il-4 == it .OR. il-4 == it+1) THEN
              IF (vcoord(4) <= 1.) THEN
                vctype = 2
              ELSE
                vctype = 3
              ENDIF
            ELSE IF (il-10 == it .OR. il-10 == it+1) THEN
              il = il - 6 ! new style vertical coordinate parameters
              IF (vcoord(4) <= 1.) THEN
                vctype = 2
              ELSE
                vctype = 3
              ENDIF
            ELSE
              PRINT '(A)', 'Error: type of hybrid vertical coordinate unrecognized, please specify explicitly vctype'
              CALL EXIT (1)
            ENDIF
          ENDIF  ! vctype known at this point
          IF (vctype == 1) THEN
            PRINT '(A)', 'Simmonds/Burridge vertical coordinates found'
            IF (il/2 /= it .AND. il/2 /= it+1) THEN
              PRINT '(A,2I4)', 'Warning: different number of hybrid vertical' &
               //' levels in grib and in namelist:', il/2, it
            ENDIF
            etaa(1:il/2) = vcoord(il/2:1:-1)
            etab(1:il/2) = vcoord(il:il/2+1:-1)
          ELSE IF (vctype == 2 .OR. vctype == 3) THEN
            IF (vctype == 2) &
             PRINT '(A)', 'LM pressure based vertical coordinates found'
            IF (vctype == 3) &
             PRINT '(A)', 'LM height based vertical coordinates found'
            IF (il-4 /= it .AND. il-4 /= it+1) THEN
              PRINT '(A,2I4)', 'Warning: different number of hybrid vertical' &
               //' levels in grib and in namelist:', il-4, it
            ENDIF
            lm_vcoord%p0sl   = vcoord(1)  
            lm_vcoord%t0sl   = vcoord(2)  
            lm_vcoord%dt0lp  = vcoord(3) 
            lm_vcoord%vcflat = vcoord(4)
            etab(1:il-4)     = vcoord(il:5:-1)
          ELSE
            PRINT '(A,I2,A)', 'Error: type of hybrid vertical coordinate ', &
             vctype,' unrecognized'
            CALL EXIT (1)
          ENDIF
        ENDIF
        EXIT gribinfo  ! All information collected, can exit loop
      ENDIF
    ENDIF
  ENDDO gribinfo

  PRINT '(A,2(I2.2,''/''),I4.4,1X,I2,'':'',I2.2)', &
   'Model emission on ', data, ora  
  PRINT '(A,I3)', 'Vertical level type ', typel
  PRINT '(A,I4,A,I4)', 'Grid size: ', nc, 'x', nr
  PRINT '(A,4F9.3)', 'Grid limits: ', firstlon, lastlon, firstlat, lastlat
  IF (MOD(ija,64) >= 32) THEN
    PRINT '(A,I3,A)', 'Error: scanning mode ',ija, &
     ' not supported, only 0, 64, 128 and 192 are supported'
    CALL EXIT(1)
  ENDIF

  !-----------------------------------------------------------------------------
  ![4.0] Recompute grid size and resolution and slightly correct
  !model grid limits if grid reduction is requested
  !-----------------------------------------------------------------------------
  IF (redfact > 1) THEN
    nrr=nr/redfact
    ncr=nc/redfact
    IF (MOD(ija,128) < 64) THEN
      latinc = -latinc
    ENDIF
    IF (MOD(ija,256) >= 128) THEN
      loninc = -loninc
    ENDIF
    firstlat=firstlat+(redfact-1)*.5*latinc
    firstlon=firstlon+(redfact-1)*.5*loninc
    lastlat=lastlat-((redfact-1)*.5 + MOD(nr,redfact)) * latinc
    lastlon=lastlon-((redfact-1)*.5 + MOD(nc,redfact)) * loninc
    loninc=ABS(loninc)*redfact
    latinc=ABS(latinc)*redfact
    PRINT '(A,I2)', 'Input grid point number reduced by a factor of ', &
     redfact*redfact
    PRINT '(A,I4,A,I4)', 'New grid size: ', ncr, 'x', nrr
    PRINT '(A,4F9.3)', 'New grid limits: ', firstlon, lastlon, firstlat, lastlat
  ELSE
    nrr=nr
    ncr=nc
  ENDIF

  !-----------------------------------------------------------------------------
  ![5.0] Compute vis5d projection parameters
  !-----------------------------------------------------------------------------
  ! Scanning mode adjustment so that firstlat is south and firstlon is west
  IF (MOD(ija,128) < 64) THEN
    suop = firstlat
    firstlat = lastlat
    lastlat=suop
  ENDIF
  IF (MOD(ija,256) >= 128) THEN
    suop = firstlon
    firstlon = lastlon
    lastlon=suop
  ENDIF
  IF (noproj) THEN  ! Generic rectangular projection requested
    projection = 0
    firstlon = - firstlon  ! vis5d requires
    lastlon = - lastlon    ! USA-ish coordinates
    proj_args (1:4) =(/ lastlat, firstlon, latinc, loninc /)
  ELSE
    IF (idrt == 0 .OR. idrt == 4) THEN  ! Cylindrical equidistant projection
      projection = 1
      firstlon = - firstlon  ! vis5d requires
      lastlon = - lastlon    ! USA-ish coordinates
      proj_args (1:4) =(/ lastlat, firstlon, latinc, loninc /)
!!$  ELSE IF (idrt == 5) THEN  ! Polar stereographic
!!$    projection = 3
!!$    firstlon = - firstlon  ! vis5d requires
!!$    lastlon = - lastlon    ! USA-ish coordinates
!!$    IF (alarot < 0.01) THEN
!!$      alarot = 90.
!!$    ELSE
!!$      alarot = -90.
!!$    ENDIF
!!$    proj_args (1:5) =(/ alarot, 0., , , /)
    ELSE IF (idrt == 10 .OR. idrt == 14) THEN  ! Rotated grid
      projection = 4
      firstlon = - firstlon  ! vis5d requires
      lastlon = - lastlon    ! USA-ish coordinates
      alorot = - alorot
      alarot = alarot + 90.
      proj_args (1:7) =(/lastlat, firstlon, latinc, loninc, alarot, alorot, rot/)
    ELSE ! Only generic rectangular projection possible
      PRINT '(A,I4,A)', 'Warning, using non geo-referenced generic projection &
       &because geographic projection ',idrt, ' is not supported'
      projection = 0
      firstlon = - firstlon  ! vis5d requires
      lastlon = - lastlon    ! USA-ish coordinates
!      proj_args (1:4) =(/ lastlat, firstlon, latinc, loninc /)
      proj_args (1:4) =(/ 1., 1., 1., 1. /)
    ENDIF
  ENDIF

  !-----------------------------------------------------------------------------
  ![6.0] Disable vertical interpolation if output levels not requested
  !(i.e. should be same as input ones) and compute vis5d vertical arguments
  !according to upper air grib level type; otherwise set output vertical levels
  !-----------------------------------------------------------------------------
  IF (ALL(vert_args == missing)) THEN
!  IF (ALL(nzlev(1:numvars) == -1)) THEN
    nzlev(1:numvars) = nl(1:numvars)
    lowzlev(1:numvars) = lowlev(1:numvars)
    interp = .FALSE.
    PRINT '(A)','Vertical interpolation to z levels not requested'
    IF (typel == 100) THEN  ! Pressure --> pressure
      vertical = 3
      vert_args(1:) = tablev(1:)
      PRINT '(A)', 'Output level type: pressure'
    ELSE IF (typel == 103) THEN  ! Height --> height
      vertical = 2
      vert_args(1:) = tablev(1:) / 1000.
      PRINT '(A)', 'Output level type: height'
    ELSE IF (typel == 107 .OR. typel == 109 .OR. typel == 119 .OR. typel ==0) &
     THEN  ! Eta/sigma/surface --> generic
      vertical = 0
      vert_args(1:2) = (/0.,.1/)
      PRINT '(A)', 'Output level type: generic'
    ENDIF
  ELSE  ! Interpolation requested, set nzlev and lowzlev properly if not set
    il = COUNT(vert_args /= missing)
    DO i = 1, numvars
      IF (nzlev(i) == -1) THEN
        IF (nl(i) == 1) THEN
          nzlev(i) = 1
        ELSE
          nzlev(i) = il
        ENDIF
      ENDIF
      IF (lowzlev(i) == -1) lowzlev(i) = 0
    ENDDO
  ENDIF

  !-----------------------------------------------------------------------------
  ![7.0] Define dates=YYDDD and times=HHMMSS
  !-----------------------------------------------------------------------------
  DO it = 1, numtimes
    CALL JELADATA5(data(1), data(2), data(3), ora(1), ora(2), fth)
    CALL JELADATA6(datag(1), datag(2), datag(3), orag(1), orag(2), &
     fth + tabftm(it) + (tabfth(it) + MAX(0,fixedfth))*60 + &
     tabftd(it)*60*24 )
    dates (it) = MOD(datag(3), 100)*1000 + SUM(daymon(1:datag(2)-1)) + datag(1)
    IF (datag(2) > 2 .AND. MOD(datag(3), 4) == 0) &  !Y2.1K bug here
     dates(it) = dates(it) + 1
    times(it) = orag(1) * 10000 + orag(2) * 100
  ENDDO

  !-----------------------------------------------------------------------------
  ![8.0] Create the v5d file and set the lowest level for every variable
  !-----------------------------------------------------------------------------
  i=COUNT(output(1:numvars))
  !Some compilers refuse to inline the following in the call
  varnamep(1:i)=PACK(varname(1:numvars), output(1:numvars))
  n = v5dcreate (outname, numtimes, i, nrr, ncr, &
   PACK(nzlev(1:numvars), output(1:numvars)), &
   varnamep, &
   times, dates, compressmode, projection, proj_args, vertical, vert_args)
  IF (n == 0) THEN
    PRINT '(2A)', 'Error in creating vis5d file ', TRIM(outname)
    CALL EXIT (1)
  ENDIF
  PRINT '(A)', 'Vis-5d file created'

  n = v5dsetlowlev (PACK(lowzlev(1:numvars), output(1:numvars)))
  IF (n == 0) THEN
    PRINT '(A)', 'Error in setting lowest levels into vis5d file'
    CALL EXIT (1)
  ENDIF


#ifdef HAVE_LIBHIBU
    !!!!!!!!!!! SOUNDING !!!!!!!!!!!!!

  ivetvar(1:7) = (/ipp,izf,itt,iqq,ivw,ivu,ivv/)
  soundlev=MAXVAL(nzlev(1:numvars)+lowzlev(1:numvars))

  IF (sounding) THEN
    soundstat = .TRUE.
    CALL sound (-firstlon,lastlat,loninc,latinc,-alorot,alarot,nrr,ncr,maxnl &
     ,soundlev,lowlev,DATA,ora,tabfth,it,typelev,imissing,ivetvar &
     ,g,"o",n)
    PRINT*,"sound option open return code=",n
    IF (n /= 0) soundstat = .FALSE.

  ENDIF

    !!!!!!!!!!!!!!!!!!!!!!!!
#endif


  !  vert_args=vert_args*1000.

  !-----------------------------------------------------------------------------
  ![9.0] Allocate workspace
  !-----------------------------------------------------------------------------
  ! Maximum number of levels in input or output
  ! maxnl = MAX(MAXVAL(nl(1:numvars)+lowzlev(1:numvars)),) &
  maxnl = MAX(MAXVAL(nl(1:numvars)), &
   MAXVAL(nzlev(1:numvars)+lowzlev(1:numvars)))
  ALLOCATE (g (nrr, ncr, maxnl, numvars))
  ALLOCATE (values (nc, nr))
  IF (redfact > 1) ALLOCATE (avgbuf (redfact, redfact))
  n = COUNT(cumulate(1:numvars) /= 0)
  IF (n > 0) THEN
    i = MAXVAL(nzlev(1:numvars), MASK=(cumulate(1:numvars) /= 0))
    ALLOCATE (cumbuff(nrr, ncr, i, n))
    cumbuff = 0.
    IF (ANY(cumulate(1:numvars) < 0)) ALLOCATE (cumextrabuff(nrr,ncr))
  ENDIF

  !-----------------------------------------------------------------------------
  ![10.0] Read-write cycle over time levels
  !-----------------------------------------------------------------------------
  CALL pbseek (iug, 0, 0, n)  ! Rewind grib file
  DO it = 1, numtimes
    IF (fixedfth < 0) THEN  ! Fixed date and varying forecast time
      datag=data
      orag=ora
      fth=tabfth(it)+tabftd(it)*24
    ELSE  ! Varying date and fixed forecast time (analysis mode)
      CALL JELADATA5(data(1), data(2), data(3), ora(1), ora(2), fth)
      CALL JELADATA6(datag(1), datag(2), datag(3), orag(1), orag(2), &
       fth + tabftm(it) + tabfth(it)*60 + tabftd(it)*60*24 )
      fth=fixedfth
    ENDIF
    !-----------------------------------------------------------------------------
    ![10.1] Read variables from grib file
    !-----------------------------------------------------------------------------
    typelev = imissing
    DO iv = 1, numvars
      IF (input(iv).AND.(it == 1 .OR. .NOT.const(iv))) THEN
        CALL readgrid (iug, nr, nc, nrr, ncr, it, iv, datag, orag, fth, &
         idimg, grib, g (1, 1, 1, iv), values, avgbuf)
      ENDIF
    ENDDO
    PRINT '(A,I2)', 'Completed reading time level ', it

    !-----------------------------------------------------------------------------
    ![10.2] Vertical interpolation
    !-----------------------------------------------------------------------------
    CALL interpzlev (nrr, ncr, maxnl, interp, g, g)
    IF (interp) THEN
      PRINT '(A)', 'Interpolation completed'  
    ENDIF

    !-----------------------------------------------------------------------------
    ![10.3] Write the 3-D grid to the v5d file
    !-----------------------------------------------------------------------------
    i=0
    ivc=0
    DO iv = 1, numvars
      IF (cumulate(iv) /= 0) THEN
        ivc=ivc+1
        DO il = 1, nzlev(iv)  ! - lowzlev(iv)
          IF (cumulate(iv) > 0) THEN  ! cumulate from beginning of forecast
            WHERE (g(:,:,il,iv) == missing) g(:,:,il,iv) = 0.
            WHERE (cumbuff(:,:,il,ivc) == missing) cumbuff(:,:,il,ivc) = 0.
            g(:,:,il,iv) = g(:,:,il,iv) + cumbuff(:,:,il,ivc)
            cumbuff(:,:,il,ivc) = g(:,:,il,iv)
          ELSE  ! Subtract field from previous in time (need an extra buffer)
            cumextrabuff(:,:) = g(:,:,il,iv)
            WHERE (g(:,:,il,iv) /= missing .AND. cumbuff(:,:,il,ivc) /= missing)
              g(:,:,il,iv) = g(:,:,il,iv) - cumbuff(:,:,il,ivc)
            ELSEWHERE
              g(:,:,il,iv) = 0.
            ENDWHERE
            cumbuff(:,:,il,ivc) = cumextrabuff(:,:)
          ENDIF
        ENDDO
      ENDIF

!!$      IF (varname(iv) == 'RH' .AND. .NOT. input(iv) ) THEN
!!$        CALL RH(g(1,1,1,iv), OUTNL, OUTLOWLEV, &
!!$         g, nrr, ncr, nl, lowlev, maxnl, numvars, varname, &
!!$         dates(it), times(it), projection, proj_args,vertical, vert_args )
!!$        IF (outnl /= nl(iv) .OR. outlowlev /= lowlev(iv)) &
!!$         PRINT*,'RH',outnl, nl(iv), outlowlev, lowlev(iv)
!!$      ENDIF
!!$
!!$      IF (varname(iv) == 'THETAE' .AND. .NOT. input(iv) ) THEN
!!$        CALL THETAE(g(1,1,1,iv), OUTNL, OUTLOWLEV, &
!!$         g, nrr, ncr, nl, lowlev, maxnl, numvars, varname, &
!!$         dates(it), times(it), projection, proj_args,vertical, vert_args )
!!$        IF (outnl /= nl(iv) .OR. outlowlev /= lowlev(iv)) &
!!$         PRINT*,'THETAE',outnl, nl(iv), outlowlev, lowlev(iv)
!!$      ENDIF

      IF (output(iv)) THEN
        i=i+1
        n = v5dwrite (it, i, g (1, 1, 1, iv) )
        IF (n == 0) THEN
          PRINT '(A,I2,A,I2)', 'Error in writing vis5d file, time ', it, &
           ', variable ', iv
          CALL EXIT (1)
        ENDIF
      ENDIF
    ENDDO


#ifdef HAVE_LIBHIBU
    !!!!!!!!!!! SOUNDING !!!!!!!!!!!!!

    ! this is the last operation becouse the matrix g will be modified

    if (sounding.and.soundstat) then
!      soundlev=MAXVAL(nzlev(1:numvars)+lowzlev(1:numvars))
      
      call sound (-firstlon,lastlat,loninc,latinc,-alorot,alarot,nrr,ncr,maxnl &
       ,soundlev,lowlev,data,ora,tabfth,it,typelev,imissing,ivetvar &
       ,g,"w",n)
      
      print*,"sound option write return code=",n
    end if

    !!!!!!!!!!!!!!!!!!!!!!!!
#endif


    ! Multiple input files requested
    IF (nmi > 1 .AND. it < numtimes) THEN
      ami=ami+1
      CALL pbclose (iug, n)
      CALL GETARG(ami,inname)
      WRITE(*,'(A,I3,2A)')'Input file N.',it+1,': ', TRIM(inname)
      CALL pbopen (iug, inname, 'r', n)  
      IF (n.NE.0) THEN
        PRINT '(A,I3,2A)', 'Error in pbopen, code: ', n, &
         ' while opening ', TRIM(inname)
        CALL EXIT (1)
      ENDIF
    ENDIF

  ENDDO

  !-----------------------------------------------------------------------------
  ![11.0] Write topography file if requested
  !-----------------------------------------------------------------------------
  IF (igo > 0. .AND. noproj) THEN
    IF (tabvar(igo) == 6) THEN
      WHERE ( g(:, :, 1, igo) /= missing )
        g(:, :, 1, igo) = g(:, :, 1, igo)/9.8
      ENDWHERE
    ENDIF
    DEALLOCATE (values)
    ALLOCATE (values (nrr, ncr))
    values = g(1:nrr, ncr:1:-1, 1, igo)
    ! Build topo file name from input grib name
    i=INDEX(inname, '.', BACK=.TRUE.)
    IF (i == 0) THEN
      outname=TRIM(inname) // '.topo'
    ELSE
      outname=inname(1:i) // 'topo'
    ENDIF
    PRINT '(2A)', 'Writing topography file ',TRIM(outname)    
    PRINT*,' firstlon, lastlon, firstlat, lastlat', &
     firstlon, lastlon, firstlat, lastlat
    IF (write_topo( TRIM(outname), firstlon, lastlon, lastlat, firstlat, &
     nrr, ncr, values ) /= 1) THEN
      PRINT '(A)', 'Error in creating topography file'
    ENDIF
  ENDIF

  !-----------------------------------------------------------------------------
  ![12.0] Close the grib file
  !-----------------------------------------------------------------------------


#ifdef HAVE_LIBHIBU
    !!!!!!!!!!! SOUNDING !!!!!!!!!!!!!

  if (sounding.and.soundstat) then
    call sound (-firstlon,lastlat,loninc,latinc,-alorot,alarot,nrr,ncr,maxnl &
     ,soundlev,lowlev,data,ora,tabfth,it,typelev,imissing,ivetvar &
     ,g,"c",n)
    print*,"sound options close return code=",n
  end if

    !!!!!!!!!!!!!!!!!!!!!!!!
#endif


  CALL pbclose (iug, n)
  ! Close the v5d file
  n = v5dclose ()
  IF (n == 0) THEN
    PRINT*, 'Error in closing vis5d file'
    CALL EXIT (1)
  ELSE
    CALL EXIT (0)
  ENDIF

END PROGRAM grib2v5d


!+ Read all levels of a variable
!
SUBROUTINE readgrid (iug, nr, nc, nrr, ncr, it, iv, data, ora, fth, &
 idimg, grib, g, values, avgbuf)

! Description: 
!   This subroutine reads all levels of a variable inside a grib in
!   whatever order, provided scanning mode is 0, 64, 128 or 192
! 
! Method: 
!   <Say how it does it: include references to external documentation> 
!   <If this routine is divided into sections, be brief here,  
!        and put Method comments at the start of each section> 
! 
! Current Code Owner: <Name of person responsible for this code> 
! 
! History: 
! Version   Date     Comment 
! -------   ----     ------- 
! <version> <date>   Original code. <Your name> 
! 
! Code Description: 
!   Language:           Fortran 90. 
!   Software Standards: "European Standards for Writing and  
!     Documenting Exchangeable Fortran 90 Code". 
! 
! Declarations: 
! Modules used: 

USE V5D_DEFINITIONS

IMPLICIT NONE

! Subroutine arguments
! Scalar arguments with intent(in): 
INTEGER, INTENT(IN) :: iug, nr, nc, nrr, ncr, it, iv, data (3), ora (2), fth, idimg

! Array  arguments with intent(inout):
REAL (rkind), INTENT (INOUT) :: grib (idimg), g (nrr, ncr, nl(iv)+lowlev(iv)), &
 values (nc, nr), avgbuf(redfact, redfact)

! Local parameters:
! Local scalars:
INTEGER :: ic, ir, il, nrd, ncd, idrt, ija, n, bm, startl, endl, stepl
LOGICAL :: rewound, hybrid = .FALSE.

! Local arrays:
INTEGER :: level (3), var (3), scad (4), datad (3), orad (2), &
 scav (4), datav (3), orav (2), nmomatch
LOGICAL :: gmatch (nmatch), bestmatch (nmatch), mostmatch (nmatch)
REAL (rkind):: lastlat, firstlat, latinc, firstlon, lastlon, loninc, alarot, alorot, rot, navg
CHARACTER (LEN=20), PARAMETER :: matchname (nmatch) = (/ &
 'vertical level  ', &
 'table 2 version ', &
 'parameter       ', &
 'initial time    ', &
 'forecast time   ', &
 'grid size       '/)
!- End of header --------------------------------------------------------------- 

g (:,:,:) = missing  ! Initialise to missing values

IF (invertl) THEN
  startl=nl(iv)+lowlev(iv)
  endl=1+lowlev(iv)
  stepl=-1
ELSE
  startl=1+lowlev(iv)
  endl=nl(iv)+lowlev(iv)
  stepl=1
ENDIF

PRINT '(A,I3.3,2(A,I5.5),A,I3.3)', 'Searching for parameter ', tabvar(iv), &
 ' vertical levels ', tablev(startl), '-', tablev(endl), &
 ' +', tabfth(it)

levels: DO il = startl, endl, stepl
  rewound = .FALSE.
  bestmatch = .FALSE.
  mostmatch= .FALSE.
  nmomatch = 0

  search: DO
    CALL getinfo (iug, grib, idimg, datad, orad, scad, level, var, &
     firstlat, lastlat, firstlon, lastlon, nrd, ncd, latinc, loninc, idrt, &
     alarot, alorot, rot, ija, n)

    IF (n.NE.0) THEN  ! End of file
      IF (.NOT. rewound) THEN
        PRINT '(A)', 'Rewind'
        CALL pbseek (iug, 0, 0, n)  ! Rewind file once and continue search
        rewound = .TRUE.
        CYCLE search
      ELSE
        PRINT '(A,I3.3,A,I5.5,A,I3.3)', 'Warning: grib not found parameter ', &
         tabvar (iv), ' level ', tablev(il), ' +', tabfth (it)
        IF (.NOT. ALL(bestmatch)) THEN
          DO ir = 1, nmatch  ! Print fields that never matched in grib file
            IF (.NOT.bestmatch(ir)) &
             PRINT '(A)', TRIM(matchname(ir))
          ENDDO
          PRINT '(A)','for this field never matched in grib file.'
        ELSE
          ir = COUNT(.NOT.mostmatch)
          IF (ir > 0) THEN
            IF (ir == 1) THEN
              PRINT '(A)','-It seems that'
            ELSE              
              PRINT '(A,I1,A)','-It seems that ',nmatch-nmomatch,' of'
            ENDIF
            DO ir = 1, nmatch  ! Print fields that didn't match at bestmatch
              IF (.NOT.mostmatch(ir)) PRINT '(2A)', '--',TRIM(matchname(ir))
            ENDDO
            PRINT '(A)','-for this field did not match in grib file.'
          ENDIF
        ENDIF
        ! Now check for some important key-fields
        IF (iv == ips .AND. input (ips) .AND. vertical == 2) THEN
          PRINT '(A,2I3)','Warning: surface pressure required and missing ',&
           iv,it
        ENDIF
        IF (iv == igo .AND. input (igo) .AND. vertical == 2) THEN
          PRINT '(A,2I3)','Warning: surface orography required and missing ',&
           iv,it
        ENDIF
        EXIT search
      ENDIF
    ENDIF

    ! Check whether level, variable table, parameter,
    ! date/time (for non constant fields) and grid size match the query
    gmatch(1) = (getvertlev(level(1)) == typel .AND. level (2) == tablev(il)) &
     .OR. (getvertlev(level(1)) == 0 .AND. il == 1)
    gmatch(2) = var (2) == tab2ver (iv) .OR. tab2ver(iv) < 0
    gmatch(3) = var (3) == tabvar (iv)
    gmatch(4) = (ALL(datad == data) .AND. ALL(orad == ora)) .OR. const(iv)
    gmatch(5) = (MAX (scad (2), scad (3)) == fth) .OR. const(iv)

! Here we should optionally check just for verification time (use datag?)
!!$    CALL JELADATA5(data(1), data(2), data(3), ora(1), ora(2), fth)
!!$    CALL JELADATA6(datag(1), datag(2), datag(3), orag(1), orag(2), &
!!$     fth + tabftm(it) + (tabfth(it) + MAX(0,fixedfth))*60 + &
!!$     tabftd(it)*60*24 )

    gmatch(6) = nrd == nr .AND. ncd == nc
    bestmatch = bestmatch .OR. gmatch
    IF (COUNT(gmatch) > nmomatch) THEN
      nmomatch = COUNT(gmatch)
      mostmatch = gmatch
    ELSE IF (COUNT(gmatch) == nmomatch) THEN
      mostmatch = mostmatch .AND. gmatch
    ENDIF
    IF (.NOT. ALL(gmatch)) CYCLE search
    ! Found the right grib

    IF (il /= 1 .AND. typelev(iv) == imissing) THEN
      typelev(iv) = level(1)  ! store the level type
    ENDIF

    CALL getdata (grib, idimg, imissing, missing, values, nc * nr, bm, n)

    ! Warning: subroutine c2agrid (interpolation from U/V points to
    ! H points of a C grid) works only with scanning mode 64
    IF (ija == 64) CALL c2agrid (nc, nr, gridtype(iv), values, values, n)
    ! destaggering for E grid
#ifdef HAVE_LIBHIBU
    CALL destagfault (values, nc, nr, missing)
#endif
    IF (redfact > 1) THEN
      ! Average values matrix over redfact**2 points, values is overwritten
      navg=redfact*redfact
      DO ir=1,nrr
        DO ic=1,ncr
          avgbuf=values((ic-1)*redfact+1:ic*redfact,(ir-1)*redfact+1:ir*redfact)
          navg=COUNT(avgbuf /= missing)
          IF (navg > 0) THEN
            values(ic,ir)=SUM(avgbuf, MASK=(avgbuf /= missing)) / navg
          ELSE
            values(ic,ir)=missing
          ENDIF
        ENDDO
      ENDDO
    ENDIF

    ! Pour values into g exchanging lon and lat; Vis5d scanning mode is 32
    g(:, :, il) = TRANSPOSE(values(1:ncr,1:nrr))
    ! Swap meridionally if necessary
    IF (MOD(ija,128) >= 64) THEN
      g(:, :, il) = g(nrr:1:-1, :, il)
    ENDIF
    ! Swap zonally if necessary
    IF (MOD(ija,256) >= 128) THEN
      g(:, :, il) = g(:, ncr:1:-1, il)
    ENDIF

    EXIT search
  ENDDO search
ENDDO levels


END SUBROUTINE readgrid



!+ Vertical interpolation of all the fields
!
SUBROUTINE interpzlev (nr, nc, maxnl, interp, g, gg)

! Description:
!   Vertically interpolates all the fields at a given time level
!   column by column;
!   the input and the output 3-d matrices can be the same
! 
! Method:
!   For each vertical column computes z of input levels and
!   then interpolates on output z levels
! 
! Current Code Owner: PAolo Patruno, Davide Cesari
! 
! History: 
! Version   Date     Comment 
! -------   ----     ------- 
! <version> <date>   Original code. <Your name> 
! 
! Code Description: 
!   Language:           Fortran 90. 
!   Software Standards: "European Standards for Writing and  
!     Documenting Exchangeable Fortran 90 Code". 
! 
! Declarations: 
! Modules used: 

USE V5D_DEFINITIONS

IMPLICIT NONE 

! Subroutine arguments 
! Scalar arguments with intent(in): 
INTEGER, INTENT(IN) :: nr, nc, maxnl
LOGICAL, INTENT(IN) :: interp
! Array  arguments with intent(inout): 
REAL (rkind), INTENT(INOUT) :: g (nr, nc, maxnl, numvars)

! Array  arguments with intent(out): 
REAL (rkind), INTENT(OUT) :: gg (nr, nc, maxnl, numvars)

! Local parameters:
REAL (rkind), PARAMETER :: rog = 287.04 / 9.8, d608 = .608, h1=1.
! Local scalars:
INTEGER :: i, j, l, ll, lmh, low, iv, neta, nzeta
REAL (rkind) :: q, t, res, ps, pt, pd, u1, u2, v1, v2, w1, w2, y1, y2, pl, ph, &
  preslev, dx, dx12
REAL (rkind):: zgdrt, ztdbe, zbetf
! Local arrays:
REAL (rkind):: profw (maxlevels,maxvars)
REAL (rkind), TARGET :: zla (maxlevels), zlev (maxlevels+1), presla (maxlevels+1)
REAL (rkind), POINTER :: zlint(:)
!- End of header --------------------------------------------------------------- 


neta=MAXVAL(nl(1:numvars) + lowlev(1:numvars))
nzeta=MAXVAL(nzlev(1:numvars) + lowzlev(1:numvars))
! Lowest level for upper air fields
low = MINVAL(lowlev(1:numvars), MASK=(nl(1:numvars) > 1)) + 1
! Computation of model vertical coordinate suitable for interpolation
IF (typel == 107 .OR. typel == 119 .OR. (typel == 109 .AND. vctype == 1)) THEN
  IF (interp) THEN
    IF (itt <= 0) THEN
      PRINT '(A)','Error: temperature required for vertical interpolation'
      CALL EXIT(1)
    ELSE IF (ips <= 0) THEN
      PRINT '(A)','Error: surface pressure required for vertical interpolation'
      CALL EXIT(1)
    ELSE IF (igo <= 0) THEN
      PRINT '(A)','Error: surface orography required for vertical interpolation'
      CALL EXIT(1)
    ENDIF
  ELSE
    IF (itt <= 0 .OR. ips <= 0 .OR. igo <= 0 .OR. &
     .NOT. ANY(output(1:numvars) .AND. .NOT. input(1:numvars))) THEN
      CALL sbatlev (nr, nc, maxnl, g, gg)
      RETURN
    ENDIF
  ENDIF
  IF (typel /= 109) etab(1:neta) = tablev (1:neta) / 10000.
ELSE IF (typel == 109 .AND. (vctype == 2 .OR. vctype == 3)) THEN
  IF (interp) THEN
    IF (igo <= 0) THEN
      PRINT '(A)','Error: surface orography required for vertical interpolation'
      CALL EXIT(1)
    ENDIF
!  ELSE
!    CALL sbatlev (nr, nc, maxnl, g, gg)
!    RETURN
  ENDIF
  lmh = 1
  zgdrt = 1./(lm_vcoord%t0sl*rog)
  ztdbe = lm_vcoord%t0sl/lm_vcoord%dt0lp
  zbetf = 2.0*lm_vcoord%dt0lp*zgdrt/lm_vcoord%t0sl
ELSE IF (typel == 100) THEN
  etab(1:neta-low) = tablev (low+1:neta) * 100.  ! Convert to Pascal
ELSE IF (typel == 103) THEN
  etab(1:neta-low) = tablev (low+1:neta)
ENDIF

rows: DO i = 1, nr
  columns: DO j = 1, nc

    IF (typel == 107 .OR. typel == 119 .OR. &
     (typel == 109 .AND. vctype == 1)) THEN  ! Sigma or Eta or Smmnds/Brrdg hybrid
      ! Set key-variables to default value if missing
      IF (ipt > 0 .AND. input(ipt)) THEN  ! Pressure at top of atmosphere
        pt = g (i, j, 1, ipt)
        IF (pt == missing) THEN
          pt = ptdef
        ENDIF
      ELSE
        pt = ptdef
      ENDIF
      IF (ire > 0 .AND. input(ire)) THEN  ! etas**-1 for Mesinger-Eta
        res = g (i, j, 1, ire)
        IF (res == missing) THEN
          res = 1.
        ENDIF
      ELSE
        res = 1.
      ENDIF
      IF (ipp > 0 .AND. .NOT. input(ipp)) THEN
        typelev(ipp) = typel
      ENDIF

      zlev(1) = g (i, j, 1, igo)  ! Orography geopotential or height
      ps = g (i, j, 1, ips)  ! Surface pressure
      IF (zlev(1) == missing .OR. ps == missing) THEN
        ! Without orography or surface pressure no interpolation can be done
        DO iv = 1, numvars  
          IF (output(iv) .AND. nzlev(iv) >1 .AND. .NOT.const(iv) &
            .AND. interp) gg (i, j, 1:nzlev(iv), iv) = missing
        ENDDO
        CYCLE columns
      ENDIF
      IF (tabvar(igo) == 6 .OR. tabvar(igo) == 129) THEN  ! Z-->z
        zlev(1)=zlev(1)/9.8
      ENDIF
      IF (typel == 119) THEN
        ! Find first non-submerged level (Eta)
        lmh = neta
        DO l = low, neta
          IF (g(i,j,l,itt) /= missing) THEN
            IF (iqq <= 0) THEN
              lmh = l
              EXIT
            ELSE IF (g(i,j,l,iqq) /= missing) THEN  ! Safety code
              lmh = l
              EXIT
            ENDIF
          ENDIF
        ENDDO
        ! compute eta at that level
!!$        IF (ABS(etab(MAX(lmh-1,1)) - 1.) .LT. 1.E-6 .OR. lmh == 1) THEN
!!$          eta1 = 1.  ! sea
!!$        ELSE
!!$          eta1 = (etab (lmh) + etab (lmh - 1) ) / 2.  ! highlands
!!$        ENDIF
      ELSE
        lmh=2
      ENDIF
!      eta1=etab(lmh-1)
      zlev(lmh-1) = zlev(1)

      pd = ps - pt  ! Pressure difference
      ph = ps
      IF (ipp > 0 .AND. .NOT. input(ipp)) THEN
        g (i, j, 1, ipp) = ps
      ENDIF
      DO l = lmh, neta  ! Loop over layers (full levels)
        ! Computation of delta eta and update of eta1 at level
        pl = ph
        ph = etaa(l) + pd * etab(l) * res + pt
        ph=MAX(ph,1.)
!        deta = 2 * ( eta1 - etab (l))  
!        eta1 = eta1 - deta
        ! pressure at layer (full level)
!        presla = pd * etab (l) * res + pt
        IF (ipp > 0 .AND. .NOT. input(ipp)) THEN
          g (i, j, l, ipp) = ph
        ENDIF
        IF (iqq > 0) THEN
          q = g (i, j, l, iqq)
        ELSE
          q=0.
        ENDIF
        t = g (i, j, l, itt)
        IF (t == missing .OR. q == missing) CYCLE columns
        ! Hydrostatic computation of z at level (half level)
!        zlev(l) = t * (q * d608 + h1) / presla * pd * deta * rog + &
!         zlev(l-1)  ! occhio
        zlev(l) = t * (q * d608 + h1) * rog * LOG(pl/ph) + &
         zlev(l-1)  ! occhio
      ENDDO
      ! Interpolation of z at layers (full levels)
      zla(1) = zlev(1)
      zla (lmh:neta) = (zlev(lmh-1:neta-1) + zlev(lmh:neta)) * 0.5


    ELSE IF (typel == 109 .AND. vctype == 2) THEN  ! Hybrid
      ! Standard PS normalized vertical coordinate (LM)
      zlev(1) = g (i, j, 1, igo)  ! Orography geopotential (height)
      IF (zlev(1) == missing) THEN
        ! Without orography no interpolation can be done
        DO iv = 1, numvars  
          IF (output(iv) .AND. nzlev(iv) >1 .AND. .NOT.const(iv)) &
           gg (i, j, 1:nzlev(iv), iv) = missing
        ENDDO
        CYCLE columns
      ENDIF
      IF (tabvar(igo) == 6 .OR. tabvar(igo) == 129) THEN
        zlev(1)=zlev(1)/9.8
      ENDIF
      ! Compute the surface reference pressure from surface topography
      IF (lm_vcoord%dt0lp == 0.0) THEN
        ps = lm_vcoord%p0sl*EXP ( - zgdrt*zlev(1) )
      ELSE
        ps = lm_vcoord%p0sl*EXP ( - ztdbe &
         *(1.0 - SQRT(1.0- zbetf*zlev(1))))
      ENDIF
      presla(1) = ps
      DO l = 2, neta  ! Loop over levels (half levels)
        ! Calculate reference pressure at level (half level)
        IF( etab(l) <= lm_vcoord%vcflat ) THEN
          preslev = etab(l)*lm_vcoord%p0sl
        ELSE
          preslev = lm_vcoord%vcflat*lm_vcoord%p0sl* &
           (1.0 - etab(l))/(1.0 - lm_vcoord%vcflat) + &
           (etab(l) - lm_vcoord%vcflat)/(1.0 - lm_vcoord%vcflat)*ps
        ENDIF
        presla(l-1) = (presla(l-1) + preslev) * 0.5
        presla(l)   = preslev
        ! Compute the height of half level from the hydrostatic equation
        zlev(l) = rog*LOG(lm_vcoord%p0sl/preslev) &
         *(lm_vcoord%t0sl - 0.5*lm_vcoord%dt0lp*LOG(lm_vcoord%p0sl/preslev))

      ENDDO

      IF (ipp > 0 .AND. ipd > 0) THEN
         IF (input (ipd) .AND. .NOT. input(ipp)) THEN
            g (i, j, 1:neta-1, ipp) = g (i, j, 1:neta-1, ipd) + presla(1:neta-1)
         ENDIF
      ENDIF
      ! Interpolation of z at layers (full levels)
      zla(1) = zlev(1)
      zla(2:neta) = (zlev(1:neta-1) + zlev(2:neta)) * 0.5
      IF (izh > 0) THEN
        IF (output(izh) .AND. .NOT. input(izh)) THEN
          g (i, j, 1:neta, izh) = zlev(1:neta)
          typelev(izh) = 109
        ENDIF
      ENDIF
      IF (izf > 0) THEN
        IF (output(izf) .AND. .NOT. input(izf)) THEN
          g (i, j, 1:neta, izf) = zla(1:neta)
          typelev(izf) = 110
        ENDIF
      ENDIF

    ELSE IF (typel == 109 .AND. vctype == 3) THEN  ! Gal-Chen Hybrid
      ! Standard Zs normalized vertical coordinate (LM)
      zlev(1) = g (i, j, 1, igo)  ! Orography geopotential (height)
      IF (zlev(1) == missing) THEN
        ! Without orography no interpolation can be done
        DO iv = 1, numvars  
          IF (output(iv) .AND. nzlev(iv) >1 .AND. .NOT.const(iv)) &
           gg (i, j, 1:nzlev(iv), iv) = missing
        ENDDO
        CYCLE columns
      ENDIF
      IF (tabvar(igo) == 6 .OR. tabvar(igo) == 129) THEN
        zlev(1)=zlev(1)/9.8
      ENDIF

      DO l = 2, neta  ! Loop over levels (half levels)
        ! Calculate height at level (half level)
        IF( etab(l) >= lm_vcoord%vcflat ) THEN
          zlev(l) = etab(l)
        ELSE
          zlev(l) = etab(l) + (lm_vcoord%vcflat - etab(l))/lm_vcoord%vcflat*zlev(1)
        ENDIF
      ENDDO

      ! Interpolation of z at layers (full levels)
      zla(1) = zlev(1)
      zla(2:neta) = (zlev(1:neta-1) + zlev(2:neta)) * 0.5
      IF (izh > 0) THEN
        IF (output(izh) .AND. .NOT. input(izh)) THEN
          g (i, j, 1:neta, izh) = zlev(1:neta)
          typelev(izh) = 109
        ENDIF
      ENDIF
      IF (izf > 0) THEN
        IF (output(izf) .AND. .NOT. input(izf)) THEN
          g (i, j, 1:neta, izf) = zla(1:neta)
          typelev(izf) = 110
        ENDIF
      ENDIF

    ELSE IF (typel == 100) THEN
      ! Pressure levels
    ELSE IF (typel == 103) THEN
      ! Height levels
      zla = etab
    ENDIF

    IF (.NOT. interp) CYCLE columns
    ! Vertical interpolation
    vars: DO iv = 1, numvars
      ! Interpolate only upper air output fields
      IF (.NOT.output(iv)) CYCLE vars
      IF (nzlev (iv) == 1) THEN
        profw(1, iv) = g(i, j, 1 + lowlev(iv), iv)
        CYCLE vars
      ENDIF
      ! Choose between half and full levels
      IF (typelev(iv) == 107 .OR. typelev(iv) == 109 &
       .OR. typelev(iv) == 119) THEN
        zlint => zlev
      ELSE
        zlint => zla 
      ENDIF

      v5dlevs: DO ll = 1 + lowzlev (iv), nzlev (iv) + lowzlev (iv)  
        IF (lowlev(iv) == 0 .AND. fill_below) THEN
          profw (ll - lowzlev (iv), iv) = g(i ,j ,1 ,iv)
        ELSE
          profw (ll - lowzlev (iv), iv) = missing
        ENDIF
        ! Below lowest model level ?
        IF (vert_args (ll) * 1000. < zlint (1 + lowlev(iv)) ) CYCLE v5dlevs

        modlevs: DO l = MAX(1+lowlev(iv), lmh), nl(iv)+lowlev(iv)-1
          ! Between l and l+1 ?
          IF (vert_args (ll) * 1000. < zlint (l + 1) ) THEN
            dx = vert_args (ll) * 1000. - zlint (l)
            dx12 = zlint (l) - zlint (l + 1)
            ! Special treatment for wind (interpolate intensity)
            IF (iv == ivu .OR. iv == ivv) THEN
              u1 = g (i, j, l, ivu)
              u2 = g (i, j, l + 1, ivu)
              v1 = g (i, j, l, ivv)
              v2 = g (i, j, l + 1, ivv)
              IF (u1 /= missing .AND. u2 /= missing .AND. &
               v1 /= missing .AND. v2 /= missing) THEN
                w1 = SQRT(u1**2 + v1**2)
                w2 = SQRT(u2**2 + v2**2)
                u1 = (u1 - u2) / dx12 * dx + u1
                v1 = (v1 - v2) / dx12 * dx + v1
                w1 = (w1 - w2) / dx12 * dx + w1
                w2 = SQRT(u1**2 + v1**2)
                IF (w2 > 0.0001) THEN
                  w1 = w1 / w2
                ELSE
                  w1 = 0.
                ENDIF
                IF (iv == ivu) THEN
                  profw (ll - lowzlev (iv), iv) = u1 * w1
                ELSE
                  profw (ll - lowzlev (iv), iv) = v1 * w1
                ENDIF
              ENDIF
            ELSE  
              y1 = g (i, j, l, iv)
              y2 = g (i, j, l + 1, iv)
              IF (y1 /= missing.AND.y2 /= missing) THEN
                profw (ll - lowzlev (iv), iv) = (y1 - y2) / dx12 * dx + y1
              ENDIF
            ENDIF
            EXIT modlevs
          ENDIF

        ENDDO modlevs
      ENDDO v5dlevs

    ENDDO vars

    DO iv = 1, numvars  
      ! Pour interpolated column into output matrix
      IF (output(iv)) gg (i, j, 1:nzlev(iv), iv) = profw (1:nzlev(iv), iv)
    ENDDO

  ENDDO columns
ENDDO rows

IF (.NOT. interp) CALL sbatlev (nr, nc, maxnl, g, gg)

END SUBROUTINE interpzlev



SUBROUTINE sbatlev (nr, nc, maxnl, g, gg)

USE V5D_DEFINITIONS

IMPLICIT NONE 

! Subroutine arguments 
! Scalar arguments with intent(in): 
INTEGER, INTENT(IN) :: nr, nc, maxnl

! Array  arguments with intent(inout): 
REAL (rkind), INTENT(INOUT) :: g (nr, nc, maxnl, numvars)

! Array  arguments with intent(out): 
REAL (rkind), INTENT(OUT) :: gg (nr, nc, maxnl, numvars)

! Local scalars:
INTEGER :: iv

DO iv = 1, numvars
  ! Remap lowlev level to the lowest level
  IF (output(iv) .AND. lowlev(iv) >0) gg (:,:,1:nzlev(iv), iv) = &
   g (:,:,1+lowlev(iv):nzlev(iv)+lowlev(iv), iv)
ENDDO

END SUBROUTINE sbatlev


!+ Return vertical level type
! 
INTEGER FUNCTION getvertlev(lev)
 
! Description: 
!   Return the conventional grib vertical level type associatedo to lev
! 
! Method: 
!   <Say how it does it: include references to external documentation> 
!   <If this routine is divided into sections, be brief here,  
!        and put Method comments at the start of each section> 
! 
! Current Code Owner: Davide Cesari
! 
! History: 
! Version   Date     Comment 
! -------   ----     ------- 
! <version> <date>   Original code. <Your name> 
! 
! Code Description: 
!   Language:           Fortran 90. 
!   Software Standards: "European Standards for Writing and  
!     Documenting Exchangeable Fortran 90 Code". 
! 
! Declarations: 
! Modules used: 
 
USE V5D_DEFINITIONS, ONLY: &
! Imported Scalar Variables with intent (in): 
 imissing, &
! Imported Array Variables with intent (in): 
 lfreeatm, lsurf
 
IMPLICIT NONE
 
! Function arguments 
! Scalar arguments with intent(in): 
INTEGER :: lev

!- End of header ------------------------------------------------------------ 

IF (ANY(lev == lfreeatm)) THEN
  IF (lev == 110) THEN
    getvertlev = 109
  ELSE IF (lev == 108) THEN
    getvertlev = 107
  ELSE
    getvertlev = lev
  ENDIF
ELSE IF (ANY(lev == lsurf)) THEN
  getvertlev = 0
ELSE
  getvertlev = imissing
ENDIF
RETURN

END FUNCTION getvertlev


