c
c	PROGRAM grads_2_vis5d - Converts grads to vis5d grid files.
c 	 (Brian Soden 11/29/9)  bjs@gfdl.gov
c        (modified by Bill Hibbard 12/28/94)
c
c
c--------------------------------------------------------------------------
c
c
	PARAMETER(max_var=30, max_lev=40, max_time=400,max_3d=100000000)
        pointer (iptr, grid)
        pointer (iptrxy, xygrid)
        dimension grid(1), xygrid(1)
        real p(max_lev), z(max_lev)
	real proj_arg(5), vert_arg(max_lev)
	integer v5dcreate,v5dcreatesimple, v5dwrite, v5dclose 
	integer nl(max_var), itime_stamp(max_time),idate_stamp(max_time)
	character*80 cfile, cint, cdir, cjunk*1, f, cname(max_var)*10
	character*80 name

	data big/1.0e31/, H/7.2/, Po/1012.15/


c   -- Open GRADS CTL file and determine # of lines --
	if(iargc().ne.2) then
	 print*, 'Usage: <grads file> <vis5d file>'
	 stop
	endif
	call getarg(1,cfile)	
	call getarg(2,name)
	open(9,file=cfile)
	do nline=1,100
	 read(9,'(A)',end=99) cjunk
	enddo
99	nline=nline-1
	
c
c--------------- READ CTL FILE ---------------------
c
        line = 1
        do while (line .lt. nline)
	 call lgetarg(9,line,1,cint)
	 if (cint(1:4).eq.'DSET' .OR. cint(1:4).eq.'dset') then
	   call lgetarg(9,line,2,cint)
	   if(cint(1:1).eq.'^') then
	    call getdir(cdir,cfile)
	    f=cdir(1:last_nblank(cdir))//cint(2:last_nblank(cint))
	   else
	    f=cint(1:last_nblank(cint))
	   endif
	   print*, 'Data file: ', f(1:last_nblank(f))
	 elseif (cint(1:5).eq.'UNDEF' .OR. cint(1:5).eq.'undef') then
	   call lgetarg(9,line,2,cint)
	   read(cint,*) undef
           print*, 'undef: ', undef
	 elseif (cint(1:4).eq.'XDEF' .OR. cint(1:4).eq.'xdef') then
	   call lgetarg(9,line,2,cint)
	   read(cint,*) nlon
	   call lgetarg(9,line,4,cint)
	   read(cint,*) rlonw
	   call lgetarg(9,line,5,cint)
	   read(cint,*) rloninc
	   print*, 'nlon, rlonw, rloninc: ', nlon, rlonw, rloninc
	 elseif (cint(1:4).eq.'YDEF' .OR. cint(1:4).eq.'ydef') then
	   call lgetarg(9,line,2,cint)
	   read(cint,*) nlat
	   call lgetarg(9,line,4,cint)
	   read(cint,*) rlats
	   call lgetarg(9,line,5,cint)
	   read(cint,*) rlatinc
	   rlatn=rlats+rlatinc*(nlat-1)
	   print*, 'nlat, rlats, rlatinc: ', nlat, rlats, rlatinc
	 elseif (cint(1:4).eq.'ZDEF' .OR. cint(1:4).eq.'zdef') then
	   call lgetarg(9,line,2,cint)
	   read(cint,*) nlev
	   print*, 'nlev: ', nlev
	   call lgetarg(9,line,3,cint)
	   if (index(cint,'I').eq.0 .AND. index(cint,'i').eq.0) then
            jarg = 4
            nargs = largc(9,line)
	    do iarg=1,nlev	! P is "levels"
             if (jarg .gt. nargs) then
              line = line + 1
              jarg = 1
              nargs = largc(9,line)
             endif
	     call lgetarg(9,line,jarg,cint)
	     read(cint,*) p(iarg)
	     print*, 'iarg, p(iarg): ', iarg, p(iarg)
             jarg = jarg + 1
	    enddo
	   else			! P is "linear"
	    call lgetarg(9,line,4,cint)
	    read(cint,*) p_bot
	    call lgetarg(9,line,5,cint)
	    read(cint,*) p_inc
	    print*, 'p_bot, p_inc: ', p_bot, p_inc
	    do lev=1,nlev
	     p(lev)=p_bot+(lev-1)*p_inc
	    enddo
	   endif
	   p_bot=p(1)
	   p_top=p(nlev)
	   z(1)=max(H*log(Po/p_bot),1.0)
	   z(nlev)=H*log(Po/p_top)
	   if (nlev.le.1) then
	    zinc=0.0
	   else
	    zinc=(z(nlev)-z(1))/(nlev-1)
	   endif
           print*, 'nlev, zinc: ', nlev, zinc
	   do lev=1,nlev
	    z(lev)=H*log(Po/p(lev))
	    print*, 'z(lev), p(lev), lev: ', z(lev), p(lev), lev
	   enddo
	 elseif (cint(1:4).eq.'TDEF' .OR. cint(1:5).eq.'tdef') then
	   call lgetarg(9,line,2,cint)
	   read(cint,*) ntime
           print*, 'ntime: ', ntime
	   call lgetarg(9,line,4,cint)
           call get_time(cint,idate,itime)
           id = IDAYS(idate)
           it = ISECS(itime)
           print*, 'idate, itime, id, it: ', idate, itime, id, it
	   call lgetarg(9,line,5,cint)
    	   call get_tinc(cint, idys, iscs)
           print*, 'idys, iscs: ', idys, iscs
	   do i=1,ntime
	    idate_stamp(i)=IYYDDD(id)
	    itime_stamp(i)=IHMS(it)
            it = it + iscs
            ii = it / 86400
            it = it - 86400 * ii
            id = id + idys + ii
	   enddo
	 elseif (cint(1:4).eq.'VARS' .OR. cint(1:5).eq.'vars') then
	   call lgetarg(9,line,2,cint)
	   read(cint,*) nvar
	   do ivar=1,nvar
	    call lgetarg(9,line+ivar,1,cname(ivar))
	    call lgetarg(9,line+ivar,2,cint)
	    read(cint,*) nl(ivar)
            if (nl(ivar) .lt. 1) nl(ivar) = 1
            print*, 'ivar, cname, nl: ', ivar, cname(ivar), nl(ivar)
	   enddo
	 endif
         line = line+1
	enddo
c
c--------------- DONE READING CTL FILE ---------------------
c


c     - Projection must be linear, rectangular, cyl-equidistant -
	iproj=1
	proj_arg(1)=rlatn
	proj_arg(2)=-rlonw
	proj_arg(3)=rlatinc
	proj_arg(4)=rloninc

c     - Unequally spaced vertical coordinates in km -
	ivert=2
	do lev=1,nlev
	 vert_arg(lev)=z(lev)
	enddo

c     - compress to 1 byte
        icompress=1 

c    - Create VIS5D file -
        iflag=v5dcreate(name,ntime,nvar,nlat,nlon,nl,cname,
     &                  itime_stamp,idate_stamp,icompress,
     &                  iproj,proj_arg,ivert,vert_arg)
        if(iflag.ne.1) then
          print*, 'v5dcreate error: ', iflag
          stop
        endif

c   - Dynamically allocate memory for grids -
	nbytes=nlon*nlat*nlev*4
	nbytes_xy=nlon*nlat*4
     	iptr = malloc(nbytes)
     	iptrxy = malloc(nbytes_xy)

	open(10,file=f,access='direct',recl=nbytes_xy/4)

	irec=0
	igrid=0
        do itime=1,ntime

         do ivar=1,nvar
c WLH
          zmin = 1.0e10
          zmax = -1.0e10

	  do lev=1,nl(ivar)
	   irec=irec+1
	   read(10,rec=irec) (xygrid(i),i=1,nlon*nlat)
	   print*, 'read record', irec, ' out of', nvar*ntime*nlev
c          call flip(grid,xygrid,nlon,nlat,nl(ivar),lev,undef,big) WLH
           call flip(grid,xygrid,nlon,nlat,nl(ivar),lev,undef,big,
     *               zmin,zmax)
	  enddo
	  iflag=v5dwrite(itime,ivar,grid)
	  igrid=igrid+1
          write(*,'(''Wrote grid '',i4,''   status: '',i4)') igrid,iflag
	 enddo
	enddo

	iflag=v5dclose()

     	call free(iptrxy)
	stop
	end


c----------------------------------------------------------------------
c       Returns the number of arguments in "line".
c
	integer function largc(iunit,line)
	character*128 string

	num_arg=0
	rewind(iunit)	
	do iread=1,line
	 read(iunit,'(A)') string
	enddo
	last_char=last_nblank(string)
	if(string(1:1) .ne. ' ') num_arg=num_arg+1
	do ichar=1,last_char-1
	 if(string(ichar:ichar)    .eq.' '   .AND. 
     &      string(ichar+1:ichar+1).ne.' ') num_arg=num_arg+1
	enddo
	largc=num_arg
c        print*, 'line, num_arg: ', line, num_arg
	return
	end

c----------------------------------------------------------------------
c       Returns argument "iarg" from line number "line".
c
	subroutine lgetarg(iunit,line,iarg,arg)
	character*128 string
	character*(*) arg

c        print*, 'iunit, line, iarg: ', iunit, line, iarg

	do k=1,len(arg)
	 arg(k:k)=' '
	enddo

	num_arg=0
	rewind(iunit)	
	do iread=1,line
	 read(iunit,'(A)') string
	enddo
	last_char=last_nblank(string)

	if(string(1:1) .ne. ' ') then	!Check if first arg is desired
	  num_arg=num_arg+1
	  if(num_arg.eq.iarg) then
	   first_char=1
	   iend=0
	   do k=first_char,last_char
	    if(string(k:k).eq.' '.AND.iend.eq.0) iend=k-1
	   enddo
	   do k=first_char,iend   	!Copy string(first_char:iend) to arg
	    kk=k+1-first_char
	    arg(kk:kk)=string(k:k)
	   enddo
	   return
	  endif
	endif
	
	do ichar=1,last_char-1		
	 if(string(ichar:ichar)    .eq.' '   .AND. 
     &      string(ichar+1:ichar+1).ne.' ')  then

	   num_arg=num_arg+1		!Search for desired arg #
	   if(num_arg.eq.iarg) then
	    first_char=ichar+1
	    iend=0
	    do k=first_char,last_char
	     if(string(k:k).eq.' ' .AND. iend.eq.0) iend=k-1
	    enddo
	    if (iend.eq.0) iend=last_char
	    do k=first_char,iend	!Copy string(first_char:iend) to arg
	     kk=k+1-first_char
	     arg(kk:kk)=string(k:k)
	    enddo
	    return
	   endif

	 endif
	enddo


	return
	end

c----------------------------------------------------------------------
c       Returns the last non-blank character  of "string" 
c
        integer function last_nblank(string)
        character*(*) string
        do i=len(string),1,-1
          if (string(i:i) .ne. ' ') then
            last_nblank=i
            return
          endif
        enddo
        last_nblank=len(string)
        return
	end

c----------------------------------------------------------------------
c       Returns date and time increment as days and secs
c
        subroutine get_tinc(string, idys, iscs)
        character*(*) string

        i = num(string(1:1))
        i2 = num(string(2:2))
        if (i2 .ge. 0 .and. i2 .le. 9) then
          i = 10 * i + i2
          k = 3
        else
          k = 2
        endif
        if (last_nblank(string) .ne. k+1) then
          print*, 'bad tinc(1): ', string
          stop
        endif
        if (string(k:k+1) .eq. 'mn' .or.
     &      string(k:k+1) .eq. 'MN') then
          idys = 0
          iscs = 60 * i
        elseif (string(k:k+1) .eq. 'hr' .or.
     &          string(k:k+1) .eq. 'HR') then
          idys = 0
          iscs = 60 * 60 * i
        elseif (string(k:k+1) .eq. 'dy' .or.
     &          string(k:k+1) .eq. 'DY') then
          idys = i
          iscs = 0
        elseif (string(k:k+1) .eq. 'mo' .or.
     &          string(k:k+1) .eq. 'MO') then
          idys = 30 * i
          iscs = 60 * 60 * 10
        elseif (string(k:k+1) .eq. 'yr' .or.
     &          string(k:k+1) .eq. 'YR') then
          idys = 365 * i
          iscs = 0
        else
          print*, 'bad tinc(2): ', string
          stop
        endif
        return
        end

c----------------------------------------------------------------------
c       Returns start date and time as yyddd and hhmmss
c
        subroutine get_time(string, idate, itime)
        character*(*) string

        if (string(3:3) .eq. ':') then
          i = num(string(1:1))
          i2 = num(string(2:2))
          if (i .lt. 0 .or. i .gt. 9 .or.
     &        i2 .lt. 0 .or. i2 .gt. 9) then
            print*, 'bad date(1): ', string
            stop
          endif
          ihh = 10 * i + i2
          i = num(string(4:4))
          i2 = num(string(5:5))
          if (i .lt. 0 .or. i .gt. 9 .or.
     &        i2 .lt. 0 .or. i2 .gt. 9) then
            print*, 'bad date(2): ', string
            stop
          endif
          imm = 10 * i + i2
          if (string(6:6) .ne. 'Z' .and.
     &        string(6:6) .ne. 'z') then
            print*, 'bad date(3): ', string
            stop
          endif
          k = 7
        else if (string(3:3) .eq. 'Z' .or.
     &           string(3:3) .eq. 'z') then
          i = num(string(1:1))
          i2 = num(string(2:2))
          if (i .lt. 0 .or. i .gt. 9 .or.
     &        i2 .lt. 0 .or. i2 .gt. 9) then
            print*, 'bad date(4): ', string
            stop
          endif
          ihh = 10 * i + i2
          imm = 0
          k = 4
        else if (string(2:2) .eq. ':') then
          i = num(string(1:1))
          if (i .lt. 0 .or. i .gt. 9) then
            print*, 'bad date(1a): ', string
            stop
          endif
          ihh = i
          i = num(string(3:3))
          i2 = num(string(4:4))
          if (i .lt. 0 .or. i .gt. 9 .or.
     &        i2 .lt. 0 .or. i2 .gt. 9) then
            print*, 'bad date(2a): ', string
            stop
          endif
          imm = 10 * i + i2
          if (string(5:5) .ne. 'Z' .and.
     &        string(5:5) .ne. 'z') then
            print*, 'bad date(3a): ', string
            stop
          endif
          k = 6
        else if (string(2:2) .eq. 'Z' .or.
     &           string(2:2) .eq. 'z') then
          i = num(string(1:1))
          if (i .lt. 0 .or. i .gt. 9) then
            print*, 'bad date(4a): ', string
            stop
          endif
          ihh = i
          imm = 0
          k = 3
        else
          ihh = 0
          imm = 0
          k = 1
        endif
        itime = 100 * (100 * ihh + imm)

        idd = num(string(k:k))
        i2 = num(string(k+1:k+1))
        if (idd .lt. 0 .or. idd .gt. 9) then
          print*, 'bad date(5): ', string
          stop
        endif
        if (i2 .ge. 0 .and. i2 .le. 9) then
          idd = 10 * idd + i2
          k = k + 2
        else
          k = k + 1
        endif

c mmm in k to k+2
c yy in k+3 to k+4 or yyyy in k+3 to k+6

        if (last_nblank(string) .eq. k+4) then
          i = num(string(k+3:k+3))
          i2 = num(string(k+4:k+4))
          if (i .lt. 0 .or. i .gt. 9 .or.
     &        i2 .lt. 0 .or. i2 .gt. 9) then
            print*, 'bad date(6): ', string
            stop
          endif
          iyy = 10 * i + i2
        else if (last_nblank(string) .eq. k+6) then
          if (string(k+3:k+4) .ne. '19') then
            print*, 'bad date(7): ', string
            stop
          endif
          i = num(string(k+5:k+5))
          i2 = num(string(k+6:k+6))
          if (i .lt. 0 .or. i .gt. 9 .or.
     &        i2 .lt. 0 .or. i2 .gt. 9) then
            print*, 'bad date(8): ', string
            stop
          endif
          iyy = 10 * i + i2
        else
          print*, 'bad date(9): ', string, last_nblank(string), k
          stop
        endif

        if (mod(iyy, 4) .eq. 0) then
          leap = 1
        else
          leap = 0
        endif

        if (string(k:k+2) .eq. 'jan' .or.
     &      string(k:k+2) .eq. 'JAN') then
          idd = idd + 0
        else if (string(k:k+2) .eq. 'feb' .or.
     &           string(k:k+2) .eq. 'FEB') then
          idd = idd + 31
        else if (string(k:k+2) .eq. 'mar' .or.
     &           string(k:k+2) .eq. 'MAR') then
          idd = idd + 59 + leap
        else if (string(k:k+2) .eq. 'apr' .or.
     &           string(k:k+2) .eq. 'APR') then
          idd = idd + 90 + leap
        else if (string(k:k+2) .eq. 'may' .or.
     &           string(k:k+2) .eq. 'MAY') then
          idd = idd + 120 + leap
        else if (string(k:k+2) .eq. 'jun' .or.
     &           string(k:k+2) .eq. 'JUN') then
          idd = idd + 151 + leap
        else if (string(k:k+2) .eq. 'jul' .or.
     &           string(k:k+2) .eq. 'JUL') then
          idd = idd + 181 + leap
        else if (string(k:k+2) .eq. 'aug' .or.
     &           string(k:k+2) .eq. 'AUG') then
          idd = idd + 211 + leap
        else if (string(k:k+2) .eq. 'sep' .or.
     &           string(k:k+2) .eq. 'SEP') then
          idd = idd + 242 + leap
        else if (string(k:k+2) .eq. 'oct' .or.
     &           string(k:k+2) .eq. 'OCT') then
          idd = idd + 272 + leap
        else if (string(k:k+2) .eq. 'nov' .or.
     &           string(k:k+2) .eq. 'NOV') then
          idd = idd + 303 + leap
        else if (string(k:k+2) .eq. 'dec' .or.
     &           string(k:k+2) .eq. 'DEC') then
          idd = idd + 333 + leap
        else
          print*, 'bad date(10): ', string
          stop
        endif
        idate = 1000 * iyy + idd

        return
        end

c----------------------------------------------------------------------
c       Returns the directory of pathname.
c
	subroutine getdir(cdir,cpath)
	character*(*) cdir, cpath

	do i=1,len(cdir)
	 cdir(i:i)=' '
	enddo

	do i=len(cpath),1,-1
	 if(cpath(i:i).eq.'/') then
	  do j=1,i
	   cdir(j:j)=cpath(j:j)
	  enddo
	  return
	 endif
	enddo
	cdir='./'
	return
	end

c----------------------------------------------------------------------
c	This program flips grid from (x,y,z) to (y,x,z)
c
        subroutine flip(grid,xygrid,nlon,nlat,nlev,lev,undef,big,
     *                  zmin,zmax)
c        subroutine flip(grid,xygrid,nlon,nlat,nlev,lev,undef,big) WLH
        real*4 grid(nlat,nlon,nlev), xygrid(nlon,nlat)

        do j=1,nlat
         do i=1,nlon
	  if(xygrid(i,j).eq.undef) then
	   grid(nlat+1-j,i,lev)=big
	  else
	   grid(nlat+1-j,i,lev)=xygrid(i,j)
c WLH
           if (xygrid(i,j) .lt. zmin) then
            zmin = xygrid(i,j)
           endif
           if (xygrid(i,j) .gt. zmax) then
            zmax = xygrid(i,j)
           endif

	  endif
         enddo
	enddo

        return
        end

c----------------------------------------------------------------------
c       Converts a character to a digit
c
        integer function num(string)
        character*(*) string

        if (string .eq. '0') then
          num = 0
        elseif (string .eq. '1') then
          num = 1
        elseif (string .eq. '2') then
          num = 2
        elseif (string .eq. '3') then
          num = 3
        elseif (string .eq. '4') then
          num = 4
        elseif (string .eq. '5') then
          num = 5
        elseif (string .eq. '6') then
          num = 6
        elseif (string .eq. '7') then
          num = 7
        elseif (string .eq. '8') then
          num = 8
        elseif (string .eq. '9') then
          num = 9
        else
          num = -1
        endif
        return
        end


      FUNCTION ISECS(IHMS)
C $ FUNCTION ISECS(IHMS) (WLH)
C $  CONVERT HHMMSS TO SECONDS
C $ IHMS = INPUT (I) TIME
C $$ ISECS=COMPUTAT
      IH=IHMS/10000
      IS=IHMS-10000*IH
      IM=IS/100
      IS=IS-100*IM
      ISECS=3600*IH+60*IM+IS
      RETURN
      END

      FUNCTION IHMS(ISEC)
C $ FUNCTION IHMS(ISEC) (WLH)
C $ CONVERT SECONDS TO HHMMSS
C $ ISEC = INPUT (I) TIME
C $$ IHMS=COMPUTAT
      IH=ISEC/3600
      IS=ISEC-3600*IH
      IM=IS/60
      IS=IS-60*IM
      IHMS=10000*IH+100*IM+IS
      RETURN
      END

      FUNCTION IDAYS(IYD)
C $ FUNCTION IDAYS(IYD) (WLH)
C $ CONVERT FROM YYDDD TO DAYS SINCE JAN. 1, 1900
C $ IYD = INPUT (I) DATE
C $$ IDAYS=COMPUTAT
      IY=IYD/1000
      ID=IYD-1000*IY
      IDAYS=365*IY+(IY-1)/4+ID
      RETURN
      END

      FUNCTION IYYDDD(IDAY)
C $ FUNCTION IYYDDD(IDAY) (WLH)
C $ CONVERT FROM DAYS SINCE JAN. 1, 1900 TO YYDDD
C $ IDAY = INPUT (I) DATE
C $$ IYYDDD=COMPUTAT
      IY=(4*IDAY)/1461
      ID=IDAY-(365*IY+(IY-1)/4)
      IYYDDD=IY*1000+ID
      RETURN
      END

