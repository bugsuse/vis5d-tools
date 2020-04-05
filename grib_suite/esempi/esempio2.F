comstart esempio_grib_util
cidx esempio di programma per uso di ngetpoint e altre routine
c 
c questo e' un programma di esempio per interpolare
c utilizzando ngetpoint un qualsiasi campo di un grib
c usando anche una eventuale maschera mare/terra
c Sono utilizzate findgrib getinfo getdata etc.
c

	parameter (MAXROWS=200,MAXCOLUMNS=200,idimg=100000)
	dimension rlsm(MAXROWS*MAXCOLUMNS)
	dimension u(MAXROWS*MAXCOLUMNS),v(MAXROWS*MAXCOLUMNS)
	integer data(3),ora(2)
	integer datag(3),orag(2)
	integer level(3),var(3),scad(4)
	integer levelg(3),varg(3),scadg(4)

	data imis,rmis/32767,-1.5e21/

	dimension grib(idimg)
	logical wind

c	leggo land sea mask

	data(1)=-1
	data(2)=-1
	data(3)=-1
	ora(1)=-1
	ora(2)=-1
	scad(1)=-1
	scad(2)=-1
	scad(3)=-1
	scad(4)=-1
	level(1)=-1
	level(2)=-1
	level(3)=-1
	var(1)=200
	var(2)=1
	var(3)=81

	call pbopen (iug,'grib.grb','r',ier)
	if(ier.ne.0)then
		print *,'codice errore pbopen',kret
		stop 1
	end if
	call findgrib(iug,grib,idimg,data,ora,scad,level,var,ier)
	if (ier.ne.0)then
		print*,'codice errore findgrib',ier
		stop 2
	end if

	call getinfo (-1,grib,idimg,datag,orag,scadg,levelg,varg
	1	,ala1,ala2,alo1,alo2,njg,nig,djg,dig,idrt
	1	,alarot,alorot,rot,ija,ier)
	if (ier.ne.0)then
		print*,'codice errore getinfo',ier
		stop 3
	end if

	imjm=MAXROWS*MAXCOLUMNS
	call getdata (grib,idimg,imis,rmis,rlsm,imjm,ibm,ier)
	if (ier.ne.0)then
		print*,'codice errore getdata',ier
		stop 4
	end if

	call destagfault (rlsm,nig,njg,rmis)

c	estraggo temperatura

	scad(1)=1
	scad(2)=24
	scad(3)=-1
	scad(4)=0
	level(1)=105
	level(2)=2
	level(3)=-1
	var(1)=200
	var(2)=1
	var(3)=11

	call findgrib(iug,grib,idimg,data,ora,scad,level,var,ier)
	if (ier.ne.0)then
		print*,'codice errore findgrib',ier
		stop 5
	end if

c	call getinfo (-1,grib,idimg,datag,orag,scadg,levelg,varg
c	1	,ala1,ala2,alo1,alo2,njg,nig,djg,dig,idrt
c	1	,alarot,alorot,rot,ija,ier)

	imjm=MAXROWS*MAXCOLUMNS
	call getdata (grib,idimg,imis,rmis,u,imjm,ibm,ier)
	if (ier.ne.0)then
		print*,'codice errore getdata',ier
		stop 6
	end if

	call grid_from_grib(u,imjm,rmis,alorot,alarot,
	1	idrt,ibm,
	1	tlm0d,tph0d,igrid,ier)

	if (ier.ne.0)then
	  stop 'errore data rep type'
	end if

	dig=abs(alo1-alo2)/float(nig-1)
	djg=abs(ala1-ala2)/float(njg-1)

	wind=.false.
	imod=1
	ls=1.

	rlonstaz=11.
	rlatstaz=45.

c	write(*,*)'u(1),v(1)'
c	write(*,*)u(1),v(1)
c	write(*,*)'rlsm(1)'
c	write(*,*)rlsm(1)
c	write(*,*)'imjm,nig,njg,alo1,ala1,dig,djg'
c	write(*,*)imjm,nig,njg,alo1,ala1,dig,djg
c	write(*,*)'igrid,ija'
c	write(*,*)igrid,ija
c	write(*,*)'tlm0d,tph0d'
c	write(*,*)tlm0d,tph0d
c	write(*,*)'imod,ls'
c	write(*,*)imod,ls

	call ngetpoint (rlonstaz,rlatstaz
	1	,u,v,rlsm,imjm,nig
	1	,njg,alo1,ala1,dig,djg
	1	,igrid,ija
	1	,tlm0d,tph0d
	1	,wind,imod,ls
	1	,urt,vrt
	1	,ier)
	if(ier.ne.0)then
		print*,'codice errore ngetpoint',ier
		stop 9
	end if

	write (*,*)'valori interpolati=',urt

	end

comend

