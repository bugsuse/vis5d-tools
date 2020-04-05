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





	SUBROUTINE DOUBLEMAT (MAT1,MAT2,I,J)

COMSTART DOUBLEMAT
C	SUBROUTINE DOUBLEMAT (MAT1,MAT2,I,J)

C	routine che raddoppia la densita' delle matrici
C	tramite una interpolazione bilineare
C	INPUT:
C	MAT1(i,j)		R*4		matrice da duplicare
C	I			I*4		prima dimensione mat1
C	J			I*4		seconda dimensione mat2
C	OUTPUT:
C	MAT2(i*2-1,j*2-1)	R*4		matrice duplicata
COMEND

	REAL MAT1(I,J),MAT2(I*2-1,J*2-1)

	DO IL=1,I
	    DO JL=1,J-1

		MAT2(IL*2-1,JL*2-1)=MAT1(IL,JL)
		MAT2(IL*2-1,JL*2)=(MAT1(IL,JL)+MAT1(IL,JL+1))/2.

	    END DO

	    MAT2(IL*2-1,J*2-1)=MAT1(IL,J)

	END DO

	DO JL=1,J*2-1
	    DO IL=1,I-1

		MAT2(IL*2,JL)=(MAT2(IL*2-1,JL)+MAT2(IL*2+1,JL))/2.

	    END DO
	END DO

	END
