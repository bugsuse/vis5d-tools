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






	integer*4 function  get_variable (vars,
	1	name,
	1	descr,
	1	ccc,
	1	ttt,
	1	ppp,
	1	unarc,
	1	unvis,
	1	va,
	1	vb,
	1	aux1,
	1	aux2,
	1	aux3,
	1	aux4)  

        include         'grb$inc:grb_vars_va1.inc'
        integer*4 vars(3),ivar_un
	character name*10,descr*50,unarc*10,unvis*10,key_var*10
	byte ccc,ttt,ppp
	real*4 va,vb
	character aux1*20  ,aux2*20  ,aux3*20  ,aux4*20

	get_variable=999
	call lib$get_lun (ivar_un)
	if(ivar_un.lt.0)GOTO 8003

        open (unit=ivar_un,name='GRB$DAT:GRB_VARS.FIL',type='old',
	1	organization='indexed',access='keyed',form='unformatted',
	1	recordtype='fixed',shared,readonly,iostat=io)
	if (io.ne.0)goto 8003

	write (key_var,'(''_'',3i3.3)',err=4200)vars
	call getx (ivar_un,0,0,key_var,0,l_va1,va1,*4200,*8003)
	name=va1_nva
	descr=va1_dva
	unarc=va1_umar
	unvis=va1_umvi
	ccc=va1_ccc
	ttt=va1_ttt
	ppp=va1_ppp
	va=va1_a
	vb=va1_b
	aux1=va1_aux1
	aux2=va1_aux2
	aux3=va1_aux3
	aux4=va1_aux4
	get_variable = 0
4200	close (ivar_un)
	call lib$free_lun (ivar_un)
	return 
8003	get_variable = -999
	goto 4200
	end

