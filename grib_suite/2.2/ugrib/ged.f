c    GED versione 1.04

c history
c 1.03
c        aumentata la dimensione del grib degribbato

c 1.04
c        adattata per nuova versione getinfo con iug=-1 enon piu' zero


        parameter (maxnl=1000,maxnp=10)
	integer np(maxnl)
	character stat(maxnl)*20,par(maxnp,maxnl)*40
	data stat/maxnl*' '/
	logical trace
	common /debug/trace

	character*80	riga

	character*10 c(10)

	CHARACTER*80 tmp,program,filein,fileout,word

	ier=9

	do i=2,2
	  call getarg(i,tmp)
	  if (tmp.eq.'-t'.or.tmp.eq.'-T')then
	    trace=.true.
          end if
	end do

	call getarg(1,tmp)
	if (tmp.eq.' ')then
	    write(*,*)'GED versione 1.03'
	    write(*,*)'usage:'
c	    write(*,*)'ged <file programma> <file in> <file out>'
	    write(*,*)'ged <file programma> [-t]'
	    write(*,*)' '
            goto 300
        end if

	program=tmp(1:(index(tmp,' ')-1))
	print*,'GED    ->> GRIB EDITOR LANGUAGE <<-'
	print*,'eseguo programma:',program

	ier=1
	open (file=program,unit=10,status='old',form='formatted',err=300)

c	ier=2
c	call pbopen(kunit,filein,'r',kret)
c	if(kret.ne.0)goto 300

c	ier=3
c	call pbopen(kunit,fileout,'w',kret)
c	if(kret.ne.0)goto 300

10	ier=4
	read (10,'(a80)',err=300,end=88)riga
c	print*,'letto riga',riga

	ier=5
	call spezza(riga,word,ier)
	if(ier.gt.0)goto 10
	read (word,*,err=300)nl

	ier=6
	if (nl.lt.1.or.nl.gt.maxnl)goto 300

	ier=7
	call spezza(riga,stat(nl),ier)
	if(ier.ne.0)goto 10

	ier=7

20	call spezza(riga,par(np(nl)+1,nl),ier)
	if(ier.ne.0)goto 10
	np(nl)=np(nl)+1
	goto 20

88	continue

	ier=0

	nl=0
77	continue
	nl=nl+1
c	do 99 nl=1,maxnl

	if (stat(nl).eq.' ')goto 99



	if (stat(nl).eq.'onerrorgoto' )then
		if (trace) print*,'eseguo linea > ',nl,' > '
     $          ,stat(nl),' ',(par(i,nl),i=1,np(nl))
		call onerrorgoto (par(1,nl),np(nl),nl,ier)
		goto 99
	end if

	if (ier.ne.0)goto 300

	if (trace) print*,'eseguo linea > ',nl,' > '
     $       ,stat(nl),' ',(par(i,nl),i=1,np(nl))

	if (stat(nl).eq.'goto' )then
		call goto  (par(1,nl),np(nl),nl,ier)		
	else if (stat(nl).eq.'gosub' )then
		call gosub  (par(1,nl),np(nl),nl,ier)
	else if (stat(nl).eq.'return' )then
		call return  (par(1,nl),np(nl),nl,ier)
	else if (stat(nl).eq.'exit' )then
		call gestexit  (par(1,nl),np(nl),ier)
	else if (stat(nl).eq.'open' )then
		call open  (par(1,nl),np(nl),ier)
	else if (stat(nl).eq.'close')then
		call close (par(1,nl),np(nl),ier)
	else if (stat(nl).eq.'print')then
		call print (par(1,nl),np(nl),ier)
	else if (stat(nl).eq.'read')then
		call read (par(1,nl),np(nl),ier)
	else if (stat(nl).eq.'write')then
		call write (par(1,nl),np(nl),ier)
	else if (stat(nl).eq.'rewind')then
		call rewind (par(1,nl),np(nl),ier)
	else if (stat(nl).eq.'setkey')then
		call setkey (par(1,nl),np(nl),ier)
	else if (stat(nl).eq.'find')then
		call find (par(1,nl),np(nl),ier)
	else if (stat(nl).eq.'let')then
		call let (par(1,nl),np(nl),ier)
	else if (stat(nl).eq.'printkey')then
		call printkey (par(1,nl),np(nl),ier)
	else if (stat(nl).eq.'printdati')then
		call printdati (par(1,nl),np(nl),ier)
	else if (stat(nl).eq.'testkey')then
		call testkey (par(1,nl),np(nl),ier)
	else
		ier=11
		goto 300
	end if


99	continue
	if (nl.lt.maxnl)goto 77


	ier=10

300	continue
	print*,'TERMINO  esecuzione programma. codice errore=',ier
c	stop 
	call exit (ier)
	end


