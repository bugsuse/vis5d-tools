
c    <HIBU, software di utilita' per  grid>
c    Copyright (C) 2000  <Paolo Patruno SMR ARPA>
c
c    $Date: 2002/11/27 14:39:01 $    $Revision: 1.9 $
c    $Id: ngetpoint.f,v 1.9 2002/11/27 14:39:01 patruno Exp $

c    Questo programma �� software  libero; �� lecito ridistribuirlo e/o
c    modificarlo secondo i  termini della Licenza Pubblica Generica SMR 
c    come pubblicata  da ARPA SMR ; riferirsi alla versione 1
c    della licenza o (a scelta) ad una versione successiva.

c    Questo programma �� distribuito  nella speranza che sia utile,  ma
c    SENZA  ALCUNA GARANZIA;  senza  neppure la  garanzia  implicita di
c    COMMERCIABILITA' o di APPLICABILITA' PER UN PARTICOLARE SCOPO.  Si
c    veda la Licenza Pubblica Generica SMR per avere maggiori dettagli.

c    Ognuno dovrebbe avere ricevuto una copia della Licenza Pubblica
c    Generica SMR insieme a questo programma; in caso contrario, la si
c    pu�� ottenere da Agenzia Regionale Prevenzione e Ambiente (ARPA)
c    Servizio Meteorologico Regionale (SMR), Viale Silvani 6, 40122
c    Bologna, Italia
c    http://www.smr.arpa.emr.it

c       importato in cvs
c       eliminate le scritte di debug e resa silente           il 27/8/1999
c
c	corretta per interpolazione in imod=0 su gligle ija<>0 il 5/12/97
c	riportare in comstart futuri aggiornamenti.
c
c	parameter imhv=21,jmhv=41,imjm=imhv*jmhv
c	dimension u(imhv,jmhv),v(imhv,jmhv),rlsm(imhv,jmhv)
c	
c	rlonstaz=20.4
c	rlatstaz=45.
c
c	do i=1,imhv
c	do j=1,jmhv
c	  u(i,j)=float(i-1)
c	  v(i,j)=float(j-1)
c	  rlsm(i,j)=0
c	end do
c	end do
c	alo11=10.
c	ala11=40.
c	dxhi=1.
c	dyhi=1.
c	igrid=0
c	ija=010
c	tlm0d=0.
c	tph0d=0.
c	imod=1
c	ls=1
c
c
c	call ngetpoint (rlonstaz,rlatstaz
c	1	,u,v,rlsm,imjm,imhv
c	1	,jmhv,alo11,ala11,dxhi,dyhi
c	1	,igrid,ija
c	1	,tlm0d,tph0d
c	1	,wind,imod,ls
c	1	,urt,vrt
c	1	,ier)
c
c
c	print*,	urt,vrt,ier
c
c	stop
c	end
c

	subroutine ngetpoint (rlonstaz,rlatstaz
	1	,u,v,rlsm,imjm,imhv
	1	,jmhv,alo11,ala11,dxhi,dyhi
	1	,igrid,ija
	1	,tlm0d,tph0d
	1	,wind,imod,ls
	1	,urt,vrt
	1	,ier)

comstart ngetpoint
c
c	subroutine ngetpoint (rlonstaz,rlatstaz
c	1	,u,v,rlsm,imjm,imhv
c	1	,jmhv,alo11,ala11,dxhi,dyhi
c	1	,igrid,ija
c	1	,tlm0d,tph0d
c	1	,wind,imod,ls
c	1	,urt,vrt
c	1	,ier)
c
c
c	corretta per interpolazione in imod=0 su gligle ija<>0 il 5/12/97
c
c	rlonstaz,rlatstaz	longitudine e latitudine (gradi.decimi....) del
c				punto su cui si vule interpolare
c
c	u(imjm)			matrice dei dati che si vogliono interpolare;
c				campo scalare o componente u del vento
c				risolta relativamente al grid definito
c				nel verso di x e y crescenti
c
c	v(imjm)			matrice dei dati che si vogliono interpolare;
c				componente v del vento
c				risolta relativamente al grid definito
c				nel verso di x e y crescenti
c				(utilizzata solo se (wind.eq.true)
c
c	rlsm(imjm)		maschera mare terra (0.=mare; 1.=terra)
c				corrispondente esattamente al vettore u
c
c	imjm			dimensione dei vettori dati
c
c	imhv,jmhv		numero dei punti in x e y della matrice u
c				comunque considerata regolare (destaggherata)
c
c	alo11,ala11		coordinate (longitudine e latitudine in
c				gradi.decimi...) del punto u(1,1)
c
c	dxhi,dyhi		passo di griglia (longitudine e latitudine in
c				gradi.decimi...) del grigliato condiderato
c				regolare
c
c	igrid			indicatore del tipo di grigliato
c				1	grigliato E staggherato punti H
c				2	grigliato E staggherato punti V
c				0	grigliato regolare (destaggherato)
c				-1	grigliato E staggherato punti H
c					con valori mancanti
c				-2	grigliato E staggherato punti V
c					con valori mancanti
c
c		i grigliati posso essere ruotati o no (dipende da tlm0d,tph0d)
c
c	ija		indicatore del modo di scansione
c			puo` essere espresso :
c			1) tre cifre binarie (0/1) indicanti il bit 1,2,3
c				 (es. ija=110)
c			2) numero decimale che imposta correttamente i bit
c				della variabile (es. ija=128)
c			bit 1	0	scansione dei punti nella direzione +i
c				1	scansione dei punti nella direzione -i
c			bit 2	0	scansione dei punti nella direzione -j
c				1	scansione dei punti nella direzione +j
c			bit 3	0	i punti adiacenti nella direzione i 
c					sono consecutivi
c			bit 3	1	i punti adiacenti nella direzione j
c					sono consecutivi
c			direzione i: da Owest a Est lungo un parallelo
c			direzione j: da Sud a Nord lungo un meridiano
c
c	tlm0d,tph0d		longitudine e latitudine dell'equatore di
c				rotazione (gradi.decimi...)
c				impostati a zero se non c'e` rotazione
c
c	imod			indicatore del tipo di interpolazione
c				0	punto piu` vicino
c				1	bilineare se ls<0
c					pesato    se ls=>0
c				-1	ripete l'interpolazione
c                                       tenendo costanti TUTTI i parametri 
c                                       in input ( salvo u e v )
c	wind	<logical>	true se in input sono state fornite le
c				componenti u e v del vento
c
c	ls			indicatore utilizzo maschera mare terra
c				<0	ignora maswchera mare terra
c				0	utilizza solo punti di mare
c				1	utilizza solo punti di terra
c
c	urt,vrt			valori interpolati ed eventualmente 
c				ricalcolate le componenti vettoriali
c				secondo le direzioni verso Est e verso Nord
c				(vrt restituita solo se wind.eq.true)
c
c	ier			condizione di errore:
c				0	tutto O.K.
c				1	parametri in ingresso errati
c				2	interpolazione fuori dal dominio dati
c				3	nei grigliati E i dxhi e dyhi non sono
c					uguali
c				4	tutti i punti intorno alla coordinata
c					d'interpolazione sono di tipo opposto
c					a quello richiesto (mare/terra)
c
c       i common:
c	common /point/ij1,ij2,ij3,ij4
c       integer ij1,ij2,ij3,ij4
c       riporta gli indici relativi ai 4 punti piu' vicini utilizzati per
c       l'interpolazione.
c
c	common /pesir/p1,p2,p3,p4,repeat
c       real p1,p2,p3,p4
c       logical repeat
c       riporta i pesi utilizzati per l'interpolazione in hbilin e ninterpmask
c       repeat segnala una ripetizione con i pesi gia' calcolati
c
comend

	dimension rlsm(imjm),u(imjm),v(imjm)
	logical wind,repeat
	common /point/ij1,ij2,ij3,ij4
	common /pesir/p1,p2,p3,p4,repeat
	integer imodsav,iersav
	save imodsav,dxhir,dyhir,xp,yp,/point/,/pesir/

	repeat=.false.

	if (imod.eq.-1)then
	   imod=imodsav
	   ier=iersav
	   repeat=.true.
	else
	   imodsav=imod
	end if
	if (repeat.and.iersav.ne.0)goto 99


	ier=0

	if(igrid.lt.-2.or.igrid.gt.2)then	!grigliato E o regular
	  ier=1
	  goto 99
	end if

	if(imod.lt.-1.or.imod.gt.1)then
	  ier=1 
	  goto 99
	end if

	if(ija.lt.0.or.ija.gt.224)then
	  ier=1
	  goto 99
	end if


	if (.not.repeat)then
	call coordmin(alo11,ala11,dxhi,dyhi,imhv,jmhv,ija
	1	,alominhirot,alaminhirot)
cd	print*,alominhirot			! origine lon 
cd	print*,alaminhirot			! origine lat


c	interpolazione

	ctph0=cosd(tph0d)
	stph0=sind(tph0d)
		
	call tlld(rlonstaz,rlatstaz,
	1	tlm0d,ctph0,stph0,tlond,tlatd)	!ruoto nel rif lambo

cd	print*,	tlond,tlatd,' coordinate della stazione ruotate'
	end if

	if (igrid.ne.0)goto 300

c	begin regular grid


	if (imod.eq.0)then
	  if (.not.repeat)then
	  ix=nint(abs((tlond-alominhirot)/dxhi))+1
	  iy=nint(abs((tlatd-alaminhirot)/dyhi))+1
	  if 	(ix.gt.imhv.or.iy.gt.jmhv.or.
	1	 ix.lt.1.or.iy.lt.1)then
c		print*,'errore fuori area'
		ier=2
		goto 99
	  end if
cd	  print*,'ix,iy',ix,iy,mindex(ix,iy,imhv,jmhv,ija)

	  ij1=mindex(ix,iy,imhv,jmhv,ija)

	  end if
	  ur=u(ij1)
	  if (wind)then
	    vr=v(ij1)
	    call rltlwd(rlonstaz,rlatstaz
	1	,ur,vr,tlm0d,ctph0,stph0	!ruoto wind
	1	,urt,vrt)
	  else
	    urt=ur
	  end if
	  goto 99
	end if


c       altri tipi di interpolazione

	if (.not.repeat) then

	ix=int(abs((tlond-alominhirot)/dxhi))+1
	iy=int(abs((tlatd-alaminhirot)/dyhi))+1
	if 	(ix.ge.imhv.or.iy.ge.jmhv.or.
	1	 ix.lt.1.or.iy.lt.1)then
c		print*,'errore fuori area'
		ier=2
		goto 99
	end if

	xp=tlond-(alominhirot+float(ix-1)*dxhi)
	yp=tlatd-(alaminhirot+float(iy-1)*dyhi)

c		qui devo gestire le diverse matrici
c		scritte mat(lat,lon) oppure mat(lon,lat)


	ij1=mindex(ix,iy,imhv,jmhv,ija)
	ij2=mindex(ix+1,iy,imhv,jmhv,ija)
	ij3=mindex(ix+1,iy+1,imhv,jmhv,ija)
	ij4=mindex(ix,iy+1,imhv,jmhv,ija)

	dxhir=dxhi
	dyhir=dyhi

c	end regular grid
	end if

	goto 400


300	continue
c			quadrati ruotati di 45 gradi

	if(dxhi.ne.dyhi)then
	  print*,'Fatal error dxhi not eq dyhi I cannot continue'
	  ier=3
	  goto 99
	end if

	if (.not.repeat) then

	ca=cosd(45.)
	sa=sind(45.)


	x1=tlond-alominhirot		! x,y dall'origine mat imhv jmhv
	y1=tlatd-alaminhirot


	x2= x1*ca+y1*sa		! rotazione di 45 gradi 
	y2=-x1*sa+y1*ca


	dxhir=dxhi*2.*ca
	dyhir=dxhir

	if(abs(igrid).eq.2)then
	  x2=x2+dxhir/2.		!traslo di mezzo passo di griglia per
	  y2=y2-dyhir/2.		!punti V
	end if
	end if

	if (imod.eq.0)then

	if (.not.repeat) then

	  i2=nint(x2/dxhir)		!calcolo indici per punto piu` vicino
	  j2=nint(y2/dyhir)
	  call nrotind(i2,j2,imhv,jmhv,ij1,igrid,ija,ier)	!1 punto
	  if (ier.ne.0)goto 99

	end if

	  ur=u(ij1)
	  if (abs(igrid).eq.2)then
	    vr=v(ij1)
	    call rltlwd(rlonstaz,rlatstaz
	1	,ur,vr,tlm0d,ctph0,stph0	!ruoto wind
	1	,urt,vrt)
	  else
	    urt=ur
	  end if

	  goto 99

	end if


	if (.not.repeat) then

	i2=int(x2/dxhir)		!calcolo indici
	j2=int(y2/dyhir)

	if (x2.lt.0.) i2=i2-1
	if (y2.lt.0.) j2=j2-1

	xp=x2-(dxhir*float(i2))	!distanza dal nodo origine
	yp=y2-(dyhir*float(j2))

CD	print*,xp,yp

c			inizio elaborazione interpolazione bilineare

	call nrotind(i2,j2,imhv,jmhv,ij1,igrid,ija,ier)	!4 punti
	if (ier.ne.0)goto 99
	call nrotind(i2+1,j2,imhv,jmhv,ij2,igrid,ija,ier)
	if (ier.ne.0)goto 99
	call nrotind(i2+1,j2+1,imhv,jmhv,ij3,igrid,ija,ier)
	if (ier.ne.0)goto 99
	call nrotind(i2,j2+1,imhv,jmhv,ij4,igrid,ija,ier)
	if (ier.ne.0)goto 99

	end if

400	continue

c		parte comune per gestione land-sea

c	print*,ij1,ij2,ij3,ij4,dxhir,dyhir,xp,yp

	if(ls.ge.0)then

cd	write (*,*)u(ij1),u(ij2),u(ij3),u(ij4)
cd	write (*,*)nint(rlsm(ij1)),nint(rlsm(ij2))
cd	1	,nint(rlsm(ij3)),nint(rlsm(ij4)),ls

	  call ninterpmask (u(ij1),u(ij2),u(ij3),u(ij4)
	1	,nint(rlsm(ij1)),nint(rlsm(ij2))
	1	,nint(rlsm(ij3)),nint(rlsm(ij4)),ls
	1	,0.,0.,dxhir,dyhir,xp,yp,ur,ier)
	  if (ier.ne.0)goto 99

	else
	  call hbilin (u(ij1),u(ij2),u(ij3),u(ij4)
	1	,0.,0.,dxhir,dyhir,xp,yp,ur)
	end if

	if (wind)then
	  if(ls.ge.0)then

cd	write (*,*)v(ij1),v(ij2),v(ij3),v(ij4)
	    call ninterpmask (v(ij1),v(ij2),v(ij3),v(ij4)
	1	,nint(rlsm(ij1)),nint(rlsm(ij2))
	1	,nint(rlsm(ij3)),nint(rlsm(ij4)),ls
	1	,0.,0.,dxhir,dyhir,xp,yp,vr,ier)
	    if (ier.ne.0)goto 99
	  else
	    call hbilin (v(ij1),v(ij2),v(ij3),v(ij4)
	1	,0.,0.,dxhir,dyhir,xp,yp,vr)
	  end if
	
	  call rltlwd(rlonstaz,rlatstaz
	1	,ur,vr,tlm0d,ctph0,stph0	!ruoto wind
	1	,urt,vrt)
	else
	  urt=ur
	end if

 99	iersav=ier
	return

	end

	subroutine nrotind(i2,j2,imhv,jmhv,ij,igrid,ija,ier)

	ier=0

	i=i2-j2+1			!ruoto gli indici indietro di 45 gradi
	j=j2+i2+1

	if(abs(igrid).eq.1)then
	  i=i				!punti H
	else if (abs(igrid).eq.2)then
	  i=i-1				!punti V (riequilibrio traslazione
					!mezzo passo di griglia ruotato)
	else
	  ier=1
	  return
	end if

cd	print*,'indici=',i,j

	if(i.le.0.or.i.gt.imhv)then
c		print*,'errore, indice i fuori area' !fuori area
		ier=2
		return
	end if
	if(j.le.0.or.j.gt.jmhv)then
c		print*,'errore, indice j fuori area' !fuori area
		ier=2
		return
	end if
	if (igrid.gt.0)then
	   ij=(mindex(i,j,imhv,jmhv,ija)+1)/2	!calcolo indice vettore
	else
	   ij=mindex(i,j,imhv,jmhv,ija)		!calcolo indice matrice
	end if

	return

	end


	subroutine hbilin (z1,z2,z3,z4,x1,y1,x3,y3,xp,yp,zp)
COMSTART HBILIN
C	subroutine hbilin (z1,z2,z3,z4,x1,y1,x3,y3,xp,yp,zp)
c
C	effettua interpolazione bilineare dati i valori nei punti
c	1,2,3,4 e le coordinate dei punti 1 e 3 oltre a quelle
c	del punto p dove viene valutato il campo.
C _____________________________________________________________
c				disposizione punti
c	4	3
c
c	  p
c
c	1	2
C _____________________________________________________________
c
c	common /pesir/p1,p2,p3,p4,repeat
c       real p1,p2,p3,p4
c       logical repeat
c       riporta i pesi utilizzati per l'interpolazione
c       repeat segnala una ripetizione con i pesi gia' calcolati
c
COMEND

	common /pesir/p1,p2,p3,p4,repeat
	logical repeat
	save /pesir/
c	data repeat/.false./

	if (.not.repeat)then
	  p2=((yp-y1)/(y3-y1))
	  p1=((xp-x1)/(x3-x1))
	end if

	  z5=(z4-z1)*p2+z1
	  z6=(z3-z2)*p2+z2

cD	print*,'z5,z6',z5,z6

	  zp=(z6-z5)*(p1)+z5

	return
	end


	subroutine ninterpmask (z1,z2,z3,z4,I1,I2,I3,I4,ls,
	1	x1,y1,x3,y3,xp,yp,zp,ier)

COMSTART NINTERPMASK
C	subroutine ninterpmask (z1,z2,z3,z4,I1,I2,I3,I4,ls,
C	1	x1,y1,x3,y3,xp,yp,zp,ier)
C
C	effettua interpolazione inversamente proporzionale alla distanza
c	e in funzione della maskera mare terra dati i valori nei punti
c	1,2,3,4 e le coordinate dei punti 1 e 3 oltre a quelle
c	del punto p dove viene valutato il campo.
C _____________________________________________________________
c				disposizione punti
c	4	3
c
c	  p
c
c	1	2
C _____________________________________________________________
c
c	common /pesir/p1,p2,p3,p4,repeat
c       real p1,p2,p3,p4
c       logical repeat
c       riporta i pesi utilizzati per l'interpolazione
c       repeat segnala una ripetizione con i pesi gia' calcolati
c
COMEND
c
cc	if((x1.eq.xp.or.x3.eq.xp).and.(y1.eq.yp.or.y3.eq.yp))
c	if(xp.eq.x1.and.yp.eq.y1)then
c		if(ls.ne.i1)stop 'punto coincide con punto mare/terra'
c		zp=z1
c		return
c	end if
c	if(xp.eq.x3.and.yp.eq.y1)then
c		if(ls.ne.i2)stop 'punto coincide con punto mare/terra'
c		zp=z2
c		return
c	end if
c	if(xp.eq.x3.and.yp.eq.y3)then
c		if(ls.ne.i3)stop 'punto coincide con punto mare/terra'
c		zp=z3
c		return
c	end if
c	if(xp.eq.x1.and.yp.eq.y3)then
c		if(ls.ne.i4)stop 'punto coincide con punto mare/terra'
c		zp=z4
c		return
c	end if
c
c	p1=1./sqr((xp-x1)**2+(yp-y1)**2)
c	p2=1./sqr((xp-x3)**2+(yp-y1)**2)
c	p3=1./sqr((xp-x3)**2+(yp-y3)**2)
c	p4=1./sqr((xp-x1)**2+(yp-y3)**2)


	common /pesir/p1,p2,p3,p4,repeat
	logical repeat
	save /pesir/
c	data repeat/.false./


	ier=0

	if (.not.repeat)then
c			raggio di influenza  
	rin=((x3-x1)**2+(y3-y1)**2)/3.

	p1=exp(-( (xp-x1)**2+(yp-y1)**2 )/rin )
	p2=exp(-( (xp-x3)**2+(yp-y1)**2 )/rin )
	p3=exp(-( (xp-x3)**2+(yp-y3)**2 )/rin )
	p4=exp(-( (xp-x1)**2+(yp-y3)**2 )/rin )

	if(ls.ne.i1)p1=0.
	if(ls.ne.i2)p2=0.
	if(ls.ne.i3)p3=0.
	if(ls.ne.i4)p4=0.

	end if

	if(p1.eq.0..and.p2.eq.0..and.p3.eq.0..and.p4.eq.0.)then
		ier=4
		return
	end if

	zp=(z1*p1+z2*p2+z3*p3+z4*p4)/(p1+p2+p3+p4)

	return
	end


	integer function mindex(id,jd,imhv,jmhv,ija)
c	mindex=indice verso 010

comstart mindex
cindex ricalcola l'indice di un vettore data una matrice comunque rigirata.
c	integer function mindex(id,jd,imhv,jmhv,ija)

c  Restituisce l'ndice di un vettore corrisponedente ad una matrice comunque 
c organizzata fornendo gli indici corrispondenti ad una matrice 
c in cui gli indici sono paragonabili alle coordinate di un sistema cartesiano
c di dimensioni imhv,jmhv quindi con scansione dei punti nella direzione 
c +i,+j con i punti adiacenti nella direzione i consecutivi 
c
c
c       id,jd      i*4  indici della matrice (i,j) ordinata nel verso ija=010
c                       (cartesiana)
c       imhv,jmhv  i*4  dimensioni della matrice come sopra (cartesiana) 
c	ija	   i*4	indicatore del modo di scansione
c			puo` essere espresso :
c			1) tre cifre binarie (0/1) indicanti il bit 1,2,3
c				 (es. ija=110)
c			2) numero decimale che imposta correttamente i bit
c				della variabile (es. ija=128)
c			bit 1	0	scansione dei punti nella direzione +i
c				1	scansione dei punti nella direzione -i
c			bit 2	0	scansione dei punti nella direzione -j
c				1	scansione dei punti nella direzione +j
c			bit 3	0	i punti adiacenti nella direzione i 
c					sono consecutivi
c			bit 3	1	i punti adiacenti nella direzione j
c					sono consecutivi
c			direzione i: da Owest a Est lungo un parallelo
c			direzione j: da Sud a Nord lungo un meridiano
comend

	character*3 cija
	dimension ib(3)

	write (cija,'(i3.3)',err=100)ija

	do i=1,3
	  if (cija(i:i).ne.'0'.and.cija(i:i).ne.'1')goto 100
	  read(cija(i:i),'(i1)')ib(i)
	end do
	goto 200

100	continue	
	ib(1)=ija/128
	ib(2)=MOD(ija,128)/64
	ib(3)=MOD(ija,64)/32

200	continue	

	ibb2=ib(2)*(-1)+1		!scambio 0 con 1

	IF (ib(3).EQ.0) THEN
	  mindex=id+ib(1)*(imhv-2*Jd+1)+imhv*(ibb2*(jmhv-2*jd+1)+jd-1)
	ELSE
	  mindex=jd+ibb2*(jmhv-2*jd+1)+jmhv*(ib(1)*(imhv-2*id+1)+id-1)
	ENDIF

cc	  mindex=(iiy-1)*imhv+iix

	return
	end

	subroutine coordmin(alo11,ala11,dxhi,dyhi,imhv,jmhv,ija
	1	,alomin,alamin)

	character*3 cija
	dimension ib(3)

	write (cija,'(i3.3)',err=100)ija

	do i=1,3
	  if (cija(i:i).ne.'0'.and.cija(i:i).ne.'1')goto 100
	  read(cija(i:i),'(i1)')ib(i)
	end do
	goto 200

100	continue	
	ib(1)=ija/128
	ib(2)=MOD(ija,128)/64
	ib(3)=MOD(ija,64)/32

200	continue	

      IF (ib(1).EQ.0) THEN
        alomin=alo11
      ELSE
        alomin=alo11-float(imhv-1)*dxhi
      ENDIF

      IF (ib(2).EQ.0) THEN
        alamin=ala11-float(jmhv-1)*dyhi
      ELSE
        alamin=ala11
      ENDIF

      IF (alomin.LT.-180.) THEN
        alomin=alomin+360.
      ELSE IF (alomin.GT.180.) THEN
        alomin=alomin-360.
      ENDIF

	return
	end
