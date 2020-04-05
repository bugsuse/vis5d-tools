c Copyright (C) 2000 

c Questo programma è software libero; è lecito ridistribuirlo e/o
c modificarlo secondo i termini della Licenza Pubblica Generica SMR come
c pubblicata da ARPA SMR ; o la versione 1 della licenza o (a scelta)
c una versione successiva.

c Questo programma è distribuito nella speranza che sia utile, ma SENZA
c ALCUNA GARANZIA; senza neppure la garanzia implicita di
c COMMERCIABILITÀ o di APPLICABILITÀ PER UN PARTICOLARE SCOPO. Si veda
c la Licenza Pubblica Generica SMR per avere maggiori dettagli.

c Ognuno dovrebbe avere ricevuto una copia della Licenza Pubblica
c Generica SMR insieme a questo programma; in caso contrario, la si può
c ottenere da Agenzia Regionale Prevenzione e Ambiente (ARPA) Servizio
c Meteorologico Regionale (SMR), Viale Silvani 6, 40122 Bologna, Italia





	subroutine leggikal(primo,IGG,IMM,IAA,iora,imin,ISTAZ,
	1	ipunt,iscad,obs,dmo,prev,x,p,key,ier)

COMSTART LEGGIKAL
C	subroutine leggikal(primo,IGG,IMM,IAA,iora,imin,ISTAZ,
C	1	ipunt,iscad,obs,dmo,prev,x,p,key,ier)
C
C	Legge il data base di kalman con accesso a chiave 
C	(Per CHIAVE 0 o CHIAVE 1 , AD ACCESSO EQ OPPURE GE)
C	o sequenziale. Gestisce il file shared ed il record locked
c	ed effettua immediato unlock del record.
c	
C input:	
C	PRIMO		LOGICAL	se =.TRUE. effettua accesso a chiave 
c				altrimenti sequenziale
c	IGG,IMM,IAA	I*4	giorno mese ed anno del record richiesto
c	IORA,IMIN	I*4	ora e minuti del record richiesto
C	ISTAZ		I*4	stazione del record richiesto
C	IPUNT		I*4	punto del grigliato del grid di ECMWF
c				contato	da sinistra veso destra ad iniziare
c				da Nord.
C	ISCAD		I*4	scadenza in ore del record richiesto
C
c	key		I*4	key=1	accesso con chiave 0   <EQ>
c				key=2	accesso con chiave 1   <EQ>
c				key=-1	accesso con chiave 0   <GE>
c				key=-2	accesso con chiave 1   <GE>
c output:
c	OBS		R*4	dato misura sulla stazione:
c				se la validita` della previsione (data del
c				record+scadenza) e` centrata alle ore 12 e` un
c				valore massimo, se e` centrata alle ore 00 e`
c				un valore minimo.
c				per il valore massimo si riferisce alla data
c				del record.
c				per il valore minimo si riferisce alla notte
c				tra la data del record e quella successiva.
C
C	DMO		R*4	valore del punto di griglia di ECMWF con 
c				data di emissione pari a quella del record
C	PREV		R*4	previsione del filtro di Kalman con validita`
c				data del record+scadenza.
c	X(im,1)		R*4	matrice del filtro utilizzata per la previsione
c	P(im,im)	R*4	matrice del filtro utilizzata per la previsione
c				con im=2
C	IER		I*4	condizione di errore
c				ier=0 tutto ok
c				ier=-2 record non trovato
c				ier=1 altri errori fatali
COMEND

	parameter im=2
	dimension x(im,1),p(im,im)
	byte chiave(16),chiave0(16),chiave1(16)
	equivalence (chiave,idat),(chiave(5),iscadc),
	1	(chiave(9),ipuntc),(chiave(13),istazc)
	INCLUDE '($FORIOSDEF)'
	logical primo

	ier=1
c
c	alloca unita' e apri il file
c
	if (Iunit.eq.0) then
	  call lib$get_lun(Iunit)
	  if (Iunit.lt.0) goto 999
	  open (unit=IUNIT,file='kal$dir:kal.ind',accesS='keyed',
	1	organization='indexed',type='old',READONLY,
	1	form='unformatted',shared,
	1	recordtype='fixed',err=999)
	end if

	if (primo)goto 55
					!accesso sequenziale

223	read(iunit,IOSTAT=IOS)
	1	chiave(4),chiave(3),chiave(2),chiave(1),
	1	chiave(8),chiave(7),chiave(6),chiave(5),
	1	chiave(12),chiave(11),chiave(10),chiave(9),
	1	chiave(16),chiave(15),chiave(14),chiave(13),
	1	obs,dmo,prev,x,p

	IF(IOS.EQ.FOR$IOS_SPERECLOC)THEN
C		file locked da altro utente
		CALL LIB$WAIT(1.)
		GOTO 223
	ELSE IF(IOS.EQ.FOR$IOS_ATTACCNON)THEN
		IER=-2			!il record non esiste
		GOTO 999
	ELSE IF (IOS.NE.0)THEN
		IER=1			!altri errori
		GOTO 999
	END IF

c	tutto O.K.
	UNLOCK IUNIT

	call jeladata6(igg,imm,iaa,iora,imin,idat)
	iscad=iscadc
	ipunt=ipuntc
	istaz=istazc

55	continue 		!accesso a chiave

	call jeladata5(igg,imm,iaa,iora,imin,idat)
	iscadc=iscad
	ipuntc=ipunt
	istazc=istaz

222	if(abs(key).eq.1)then

	  do i=1,4
		k=i
		chiave0(k)=chiave(5-i)
		k=i+4
		chiave0(k)=chiave(9-i)
		k=i+8
		chiave0(k)=chiave(13-i)
		k=i+12
		chiave0(k)=chiave(17-i)
	  end do

	  if(key.gt.0)then
		read(iunit,keyid=0,key=chiave0,IOSTAT=IOS)
	1	chiave(4),chiave(3),chiave(2),chiave(1),
	1	chiave(8),chiave(7),chiave(6),chiave(5),
	1	chiave(12),chiave(11),chiave(10),chiave(9),
	1	chiave(16),chiave(15),chiave(14),chiave(13),
	1	obs,dmo,prev,x,p

	  else
		read(iunit,keyid=0,keyge=chiave0,IOSTAT=IOS)
	1	chiave(4),chiave(3),chiave(2),chiave(1),
	1	chiave(8),chiave(7),chiave(6),chiave(5),
	1	chiave(12),chiave(11),chiave(10),chiave(9),
	1	chiave(16),chiave(15),chiave(14),chiave(13),
	1	obs,dmo,prev,x,p
		call jeladata6(igg,imm,iaa,iora,imin,idat)
		iscad=iscadc
		ipunt=ipuntc
		istaz=istazc
	  end if
	else

	  do i=1,4
		k=i
		chiave1(k)=chiave(17-i)
		k=i+4
		chiave1(k)=chiave(5-i)
		k=i+8
		chiave1(k)=chiave(9-i)
		k=i+12
		chiave1(k)=chiave(13-i)
	  end do

	  if(key.gt.0)then
		read(iunit,keyid=1,key=chiave1,IOSTAT=IOS)
	1	chiave(4),chiave(3),chiave(2),chiave(1),
	1	chiave(8),chiave(7),chiave(6),chiave(5),
	1	chiave(12),chiave(11),chiave(10),chiave(9),
	1	chiave(16),chiave(15),chiave(14),chiave(13),
	1	obs,dmo,prev,x,p

	  else

		read(iunit,keyid=1,keyge=chiave1,IOSTAT=IOS)
	1	chiave(4),chiave(3),chiave(2),chiave(1),
	1	chiave(8),chiave(7),chiave(6),chiave(5),
	1	chiave(12),chiave(11),chiave(10),chiave(9),
	1	chiave(16),chiave(15),chiave(14),chiave(13),
	1	obs,dmo,prev,x,p
		call jeladata6(igg,imm,iaa,iora,imin,idat)
		iscad=iscadc
		ipunt=ipuntc
		istaz=istazc
	  end if
	end if

	IF(IOS.EQ.FOR$IOS_SPERECLOC)THEN
C		file locked da altro utente
		CALL LIB$WAIT(1.)
		GOTO 222
	ELSE IF(IOS.EQ.FOR$IOS_ATTACCNON)THEN
		IER=-2			!il record non esiste
		GOTO 999
	ELSE IF (IOS.NE.0)THEN
		IER=1			!altri errori
		GOTO 999
	END IF

c	tutto O.K.

	UNLOCK IUNIT

	ier=0
	primo=.false.
C	close(IUNIT)
	return

999	CONTINUE

	obs=32767
	dmo=32767
	prev=32767

	iscad=iscadc
	ipunt=ipuntc
	istaz=istazc

C	close(IUNIT)
	return
	end	


	subroutine scrivikal(IGG,IMM,IAA,iora,imin,ISTAZ,
	1	ipunt,iscad,obs,dmo,prev,x,p,enarewrite,ier)


COMSTART SCRIVIKAL
C	subroutine scrivikal(IGG,IMM,IAA,iora,imin,ISTAZ,
C	1	ipunt,iscad,obs,dmo,prev,x,p,enarewrite,ier)
C
C	Scrive nel data base di kalman con accesso a chiave.
C	Gestisce il file shared e la riscrittura del record.
c	
C input:	
c	IGG,IMM,IAA	I*4	giorno mese ed anno del record
c	IORA,IMIN	I*4	ora e minuti del record
C	ISTAZ		I*4	stazione del record
C	IPUNT		I*4	punto del grigliato del grid di ECMWF
c				contato	da sinistra veso destra ad iniziare
c				da Nord.
C	ISCAD		I*4	scadenza in ore del record richiesto
C
c	OBS		R*4	dato misurato sulla stazione:
c				se la validita` della previsione (data del
c				record+scadenza) e` centrata alle ore 12 e` un
c				valore massimo, se e` centrata alle ore 00 e`
c				un valore minimo.
c				per il valore massimo si riferisce alla data
c				del record.
c				per il valore minimo si riferisce alla notte
c				tra la data del record e quella successiva.
C
C	DMO		R*4	valore del punto di griglia di ECMWF con 
c				data di emissione pari a quella del record
C	PREV		R*4	previsione del filtro di Kalman con validita`
c				data del record+scadenza.
c	X(im,1)		R*4	matrice del filtro utilizzata per la previsione
c	P(im,im)	R*4	matrice del filtro utilizzata per la previsione
c				con im=2
c	enarewrite	LOGICAL	abilita la riscrittura di un record gia
c				esistente se posto =.TRUE.
c output:
C	IER		I*4	condizione di errore
c				ier=0 tutto ok
c				ier=1 altri errori fatali
c				ier=2 errore nella rilettura del record se
c					gia  esistente
c				ier=3 errore nella rewrite del record gia
c					esistente
COMEND


	parameter im=2
	dimension x(im,1),p(im,im)
	byte chiave(16),chiave0(16)
	equivalence (chiave,idat),(chiave(5),iscadc),
	1	(chiave(9),ipuntc),(chiave(13),istazc)
	INCLUDE '($FORIOSDEF)'
	logical enarewrite

	ier=1
c
c	alloca unita' e apri il file
c
	if (Iunit.eq.0) then
	  call lib$get_lun(Iunit)
	  if (Iunit.lt.0) goto 999
	  open (unit=IUNIT,file='kal$dir:kal.ind',accesS='keyed',
	1	organization='indexed',type='old',
	1	form='unformatted',shared,
	1	recordtype='fixed',err=999)
	endif

	call jeladata5(igg,imm,iaa,iora,imin,idat)
	iscadc=iscad
	ipuntc=ipunt
	istazc=istaz

	write(iunit,IOSTAT=IOS)
	1	chiave(4),chiave(3),chiave(2),chiave(1),
	1	chiave(8),chiave(7),chiave(6),chiave(5),
	1	chiave(12),chiave(11),chiave(10),chiave(9),
	1	chiave(16),chiave(15),chiave(14),chiave(13),
	1	obs,dmo,prev,x,p
	IF(IOS.EQ.FOR$IOS_INCKEYCHG)THEN
C	record gia esistente
	if (.not.enarewrite)stop 'errore non abilitato riscrittura rec'
	  do i=1,4
		k=i
		chiave0(k)=chiave(5-i)
		k=i+4
		chiave0(k)=chiave(9-i)
		k=i+8
		chiave0(k)=chiave(13-i)
		k=i+12
		chiave0(k)=chiave(17-i)
	  end do
	  ier=2
	  read(iunit,keyid=0,key=chiave0,err=999)
	  rewrite(iunit,err=999)
	1	chiave(4),chiave(3),chiave(2),chiave(1),
	1	chiave(8),chiave(7),chiave(6),chiave(5),
	1	chiave(12),chiave(11),chiave(10),chiave(9),
	1	chiave(16),chiave(15),chiave(14),chiave(13),
	1	obs,dmo,prev,x,p

	ELSE IF (IOS.NE.0)THEN
	  ier=3			!altri errori
	  GOTO 999
	end if
	
	ier=0
c	close (iunit)
	return

999	continue
c	close (iunit)
	return

	end
