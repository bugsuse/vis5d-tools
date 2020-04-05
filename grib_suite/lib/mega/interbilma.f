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





c_______CALL     INTERBILMA(6371.,ALONMN ,ALONMX ,ALATMN ,ALATMX ,DLAT,DLON,
c_____*         SURPRES, IEC, JEC, ALAT, ALON, VAL, IER)
C
      SUBROUTINE INTERBILMA(RT,ALONMIN,ALONMAX,ALATMIN,ALATMAX,DLAT,
     *DLON,
     *          VALB    ,NPX, NPY, U   , V   , VAL, IER)

COMSTART INTERBILMA	
C
C      SUBROUTINE INTERBILMA(RT,ALONMIN,ALONMAX,ALATMIN,ALATMAX,DLAT,
C     *DLON,
C     *          VALB    ,NPX, NPY, U   , V   , VAL, IER)
C
C
C========================================================================
C	CACCIAMANI CARLO
C       SMR/BOLOGNA  OTTOBRE 1988;  REVISED M.M.MARUCA SEPT. '92
C------------------------------------------------------------------------
C
c   esegue una interpolazione bilineare su un punto definito all'interno
c   di una area geografica utilizzando i 4 punti di griglia piu' vicini 
c   di una matrice di valori definita su un reticolato regolare, il cui 
c   primo elemento corrisponde al punto in basso a sx.
C
C      PARAMETRI IN INPUT    ............
C    --------------------------------------------------------------
C       RT             =  RAGGIO TERRESTRE            (    Km.   )
C	ALONMIN	       =  LONGITUDINE MINIMA          (GRADI CEN.)
C	ALONMAX        =  LONGITUDINE MASSIMA	        "     "
C       ALATMIN        =  LATITUDINE MINIMA             "     "
C       ALATMAX        =  LATITIDINE MASSIMA            "     "
C       DLAT           =  PASSO DI GRIGLIA LATITUDINE   "     "
C       DLON           =  PASSO DI GRIGLIA LONGITUDINE  "     "
C       VALB(NPY,NPX)  =  MATRICE DEI VALORI  DI INTERPOLAZIONE
C       NPX            =  MAX.PUNTI GRIGLIA IN LONGITUDINE
C       NPY            =  MAX   "      "       LATITUDINE 
C       U              =  LONGITUDINE PUNTO INTERPOLATO (GRADI CENT.)
C       V              =  LATITUDINE     "      "       "     "  
C
C      PARAMETRI IN OUTPUT
C    --------------------------------------------------------------
C
C        VAL = VALORE VARIABILE INTERPOLATA SUL PUNTO (UR,VR)
C        IER =-1 IL PUNTO SU CUI SI INTERPOLA E' ESTERNO
C               ALL'AREA GEOGRAFICA
C
C------------------------------------------------------------------------
C
C	definizione di un sistema di riferimento cartesiano
c	con origine nel punto (inx,iny),il piu' basso a sinistra del
c	punto osservazione
C  
C   asse y   |
C   ------   |
C            |
C (INX,INY+1)|                         (INX+1,INY+1)  
C   (x4,y4)  |..........................(x3,y3)
C            |                         :
C            |       (xp,yp)           :
C   (x5,y5)  |---------*---------------:(x6,y6)
C            |      (pt.oss.)          :
C            |                         :
C            |                         : 
C            |                         :            asse x
C            |___________________________________________________
C           (x1,y1)                (x2,y2)      
C          (INX,INY)            (INX+1,INY) 
C
C------------------------------------------------------------------------
COMEND
      DIMENSION VALB(NPX,NPY)
      REAL LATR(4),LONR(4)
C
C------------------------------------------------------------------------
C      inizializzazione  
C
      CONV=ACOS(-1.)/180.
C
      VAL=0.
      IER=0
      DY=V-ALATMIN
      DX=U-ALONMIN
c      type *,'dy ',dy,'dx ',dx
      DTETA=(ALATMAX-ALATMIN)
      DPHI=(ALONMAX-ALONMIN)
c      type *,'dteta',dteta,'dphi',dphi
      IF(DY.LT.0..OR.DX.LT.0.)GO TO 999 
      IF(DY.GT.DTETA.OR.DX.GT.DPHI)GO TO 999
C
      DLATR=DLAT*CONV
      DLONR=DLON*CONV
      NY=(alatmax-alatmin)/dlat+1
      NX=(alonmax-alonmin)/dlon+1
C      type *,' ny ',ny,' nx ',nx
C
      INY=DY/DLAT+1
      INX=DX/DLON+1
c      type *,'iny inx ',iny,inx
C
      UR=U*CONV
      VR=V*CONV
c                 print*,' UR VR ',ur,vr,' CONV ',conv
C
      LATR(1)=CONV*(ALATMIN+FLOAT(INY-1)*DLAT)
      LONR(1)=CONV*(ALONMIN+FLOAT(INX-1)*DLON)
C
      LATR(2)=CONV*(ALATMIN+FLOAT(INY-1)*DLAT)
      LONR(2)=CONV*(ALONMIN+FLOAT(INX)*DLON)
C
      LATR(3)=CONV*(ALATMIN+FLOAT(INY)*DLAT)
      LONR(3)=CONV*(ALONMIN+FLOAT(INX)*DLON)
C
      LATR(4)=CONV*(ALATMIN+FLOAT(INY)*DLAT)
      LONR(4)=CONV*(ALONMIN+FLOAT(INX-1)*DLON)
c
C------------------------------------------------------------------------
C      x=distanza asse x   ;   y=distanza asse y
C
      X1=0.
      Y1=0.
C
      X2=RT*COS(LATR(2))*DLONR      
      Y2=0.
C
      X3=RT*COS(LATR(3))*DLONR
      Y3=RT*(LATR(3)-LATR(2))
C
      X4=0.
      Y4=RT*(LATR(4)-LATR(1))
C
      X5=0.
      Y5=RT*(VR-LATR(1))
C      
      X6=RT*COS(VR)*DLONR
      Y6=RT*(VR-LATR(2))
C
      XP=RT*COS(VR)*(UR-LONR(1))
      YP=RT*(VR-LATR(1))
c
c          print*,' '
c          print*,' X1, X2, X3, X4, X5, X6, XP',X1,X2,X3,X4,X5,X6,XP
c          print*,' Y1, Y2, Y3, Y4, Y5, Y6, YP',Y1,Y2,Y3,Y4,Y5,Y6,YP
c          print*,' '
C------------------------------------------------------------------------
C      valori sui 4 punti di griglia piu vicini
C
      Z1=VALB(INX,INY)
      Z2=VALB(INX+1,INY)
      Z3=VALB(INX+1,INY+1)
      Z4=VALB(INX,INY+1)
C
C------------------------------------------------------------------------
C      interpolazione sui punti 5 e 6
C
      Z5=(Z4-Z1)*((Y5-Y1)/(Y4-Y1))+Z1
      Z6=(Z3-Z2)*((Y6-Y2)/(Y3-Y2))+Z2
c          print*,' Diff.',(Z4-Z1),' Peso ',(Y5-Y1)/(Y4-Y1)
c      TYPE *,'Z4 Z1  Z5 ',Z4,Z1,'  ',Z5
c          print*,' Diff.',(Z3-Z2),' Peso ',(Y6-Y2)/(Y3-Y2)
c      TYPE *,'Z3 Z2  Z6 ',Z3,Z2,'  ',Z6
C
C      interpolazione sul punto p
C
      VAL=(Z6-Z5)*((XP-X5)/(X6-X5))+Z5
c      type *,' val ',val
C------------------------------------------------------------------------
      RETURN
C
 999  CONTINUE
      IER=-1
C------------------------------------------------------------------------
      RETURN
      END                  !END OF SUBROUTINE INTERBILMA
C------------------------------------------------------------------------
