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





CC**********************************************************************CC
CC**********************************************************************CC
CC									CC
CC		SERVIZIO METEOROLOGICO REGIONE EMILA ROMAGNA		CC
CC				E.R.S.A.				CC
CC									CC
CC		    UFFICIO INFORMAZIONI E PREVISIONI			CC
CC									CC
CC	PAOLO PATRUNO							CC
CC		 				POMI LUCA		CC
CC	SELVINI ANDREA							CC
CC						BATTAGLIA FABRIZIO	CC
CC									CC
CC									CC
CC	BOLOGNA 1990							CC
CC**********************************************************************CC
CC**********************************************************************CC

	function c_e(var)
COMSTART C_E
c	function c_e(var)
c	Verifica la condizione di presenza o assenza del dato secondo
c	le specifiche meteodata restituendo una variabile logical .true.
c	se c'e` il dato (ossia esso e` diverso da 32767)
c
c	INPUT:
c
C	VAR	I*2	dato di cui verificare la presenza
c
c	OUTPUT:
C
C	C_E	LOGICAL	.TRUE.se il dato e` presente
c
COMEND

	logical c_e
	integer*2 var

	c_e=.true.
	if (var.eq.32767)c_e=.false.
	return
	end

	function j_c_e(var)

COMSTART J_C_E
c	function j_c_e(var)
c	Verifica la condizione di presenza o assenza del dato secondo
c	le specifiche meteodata restituendo una variabile logical .true.
c	se c'e` il dato (ossia esso e` < 32767)
c
c	INPUT:
c
C	VAR	I*4	dato di cui verificare la presenza
c
c	OUTPUT:
C
C	J_C_E	LOGICAL	.TRUE.se il dato e` presente
c
COMEND


	logical j_c_e,c_e
	integer*4 var
	integer*2 var1
	var1=MAX(MIN(var,32767),-32768)
	j_c_e=c_e(var1)

	return
	end


	function ch_c_e(var)
COMSTART CH_C_E
c	function ch_c_e(var)
c	Verifica la condizione di presenza o assenza del dato secondo
c	le specifiche meteodata restituendo una variabile logical .true.
c	se c'e` il dato (ossia esso e` diverso da 32767)
c
c	INPUT:
c
C	VAR	CHAR*(*)  	dato di cui verificare la presenza
c
c	OUTPUT:
C
C	CH_C_E	LOGICAL		.TRUE.se il dato e` presente
c
COMEND

	logical ch_c_e
	character*(*) var

	ch_c_e=.false.
	l=len(var)
	do j=1,l
		if (ichar(var(j:)).ne.255.AND.
	1	    ichar(var(j:)).ne.127)ch_c_e=.true.
	end do
	return
	end

