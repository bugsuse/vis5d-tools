      SUBROUTINE MAIN0
C
C igg3d program for managing and analyzing 3D gridded data sets
C Copyright (C) 1990  Bill Hibbard and Dave Santek
C
C This program is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published by
C the Free Software Foundation; either version 1, or (at your option)
C any later version.
C
C This program is distributed in the hope that it will be useful,
C but WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C GNU General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this program; if not, write to the Free Software
C Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
C
C ?
C ? IGG3D -- 3-D Grid utility    (DAS)
C ?      IGG3D LISt gridbeg gridend <keyword>
C ?      IGG3D INFo gridbeg gridend <keyword>
C ?      IGG3D DELete gridbeg gridend <keyword>
C ?      IGG3D GET sgridfile gridbeg gridend dgridfile gridbeg
C ?      IGG3D MAKe grid1 oper <grid2> <keyword>
C ?      IGG3D COPy grid1 grid2
C ?      IGG3D INTerp grid day time -SETDEL gstep ngrids -LAG uspd vspd
C ?      IGG3D W O-grid T-grid P-grid (W(m/s) from Omega(mb/s))
C ?      IGG3D RH Q-grid T-grid P-grid     (RH calculation)
C ?      IGG3D THE T-grid P-grid <offset>  (Theta calculation)
C ?      IGG3D LAG grid rday rtime uspd vspd  (Lagrangian coords)
C ?      IGG3D ADD grid1 grid2 ... grid6 <NAME=> (combine up to 6 grids)
C ?      IGG3D AVErage gridout gridin1 gridin2 day time 
C ? Parameters:
C ?      gridbeg | beginning grid number
C ?      gridend | ending grid number
C ?      sgridfile | source grid file number
C ?      dgridfile | destination grid file number
C ?      oper | option applied to grid, SPD, SUB, SUBL, EXT
C ?         SPD - wind speed from u & v
C ?         SUB - subrtract grids
C ?         SUBL- subtract logarithmicly
C ?         FIL value lev1 lev2  - fill selected levels of 3-D
C ?          with value. (if value=MISS, for missing value)
C ? Keywords:
C ?     -GR3DF 3-D-gridfile-number
C ?     -GRID output-grid-number     (def=next avail)
C ?     -V grid-number-for-v         (def=u grid number + 1)
C ?     -NAME parameter-name    for result of ADD,FILL,COPy option
C ?
C ? Examples:
C ?     igg3d list 1 10 -gr3df 2    Lists grids 1 through 10 from
C ?                                 file GR3D0002
C ?     igg3d info 1 10 -gr3df 2    Lists means and ranges for grids 1
C ?                                 through 10 from file GR3D0002
C ?
C * SSEC/MCIDAS USERS MANUAL - CHAP04
C
C
      IMPLICIT INTEGER (A,B,E-Z)
      IMPLICIT CHARACTER*12 (C)
      IMPLICIT REAL*8 (D)
      CHARACTER*4 CLIT
      REAL*4 DUSPD,DVSPD,DIFA,DIFB,DA,DB
      PARAMETER (MAXWDS=200000,MXGRDS=6)
      CHARACTER*3 COPT
      CHARACTER*8 FILNAM
      REAL*4 XVAL,GA(MAXWDS),GB(MAXWDS),GC(MAXWDS),XNULL,XTEST
      REAL*4 GG(MAXWDS,MXGRDS)
C      COMMON/GRIDXX/GA,GB,GC
      INTEGER TABLE(64),TABLE2(64)
      INTEGER IGG(MXGRDS)
      CHARACTER*12 CFI
      DATA HEDSIZ/64/,XTEST/1.E30/,XNULL/1.E35/
      DATA ENDMRK/'80808080'X/
C
      IGF=IKWP('GR3DF',1,IGCF3D(-1))
      COPT=CPP(1,' ')
      IF(COPT.EQ.'LIS') GO TO 10
      IF(COPT.EQ.'DEL') GO TO 20
      IF(COPT.EQ.'GET') GO TO 30
      IF(COPT.EQ.'MAK') GO TO 40
      IF(COPT.EQ.'INF') GO TO 50
      IF(COPT.EQ.'COP') GO TO 60
      IF(COPT.EQ.'INT') GO TO 70
      IF(COPT.EQ.'RH' ) GO TO 80
      IF(COPT.EQ.'THE') GO TO 100
      IF(COPT.EQ.'LAG') GO TO 110
      IF(COPT.EQ.'ADD') GO TO 120
      IF(COPT.EQ.'AVE') GO TO 130
      IF(COPT.EQ.'W')   GO TO 140
C
C-----INVALID FUNCTION
C
      CALL EDEST('INVALID FUNCTION '//COPT,0)
      RETURN
C
C-----LIST FUNCTION
C
 10   CONTINUE
      IF (OPENUP(IGF,FILNAM,MAXGRD).NE.0) GOTO 99
      LO=IPP(2,1)
      HI=IPP(3,LO)
      IF(NKWP(' ').EQ.1) HI=MAXGRD
      LO=MIN0(LO,MAXGRD)
      HI=MIN0(HI,MAXGRD)
      CALL LDEST('MAXGRD',MAXGRD)
      CALL LDEST('LO/HI '//CFI(LO)//CFI(HI),0)
 12   DO 14 NEXTI=LO,HI
         CALL LWI(FILNAM,NEXTI*HEDSIZ,HEDSIZ,TABLE)
C        This call swaps the order of the 4 characters if we're on a DEC:
         call swapchar( TABLE(9) )
         CALL PRGH3D(NEXTI,TABLE)
 14   CONTINUE
 15   CALL SDEST('  ---END OF LISTING',0)
      GOTO 99
C
C-----DELETE FUNCTION
C
 20   CONTINUE
      IF (OPENUP(IGF,FILNAM,MAXGRD).NE.0) GOTO 99
      LO=IPP(2,1)
      HI=IPP(3,LO)
      LO=MIN0(LO,MAXGRD)
      HI=MIN0(HI,MAXGRD)
 22   DO 25 NEXTI=LO,HI
         CALL LWI(FILNAM,HEDSIZ*NEXTI,1,EXISTS)
         IF (EXISTS.NE.ENDMRK) CALL LWO(FILNAM,HEDSIZ*NEXTI,1,-1)
 25   CONTINUE
 24   CALL SDEST('  ---DELETE DONE',0)
      GOTO 99
C
C-----GET FUNCTION
C
 30   CONTINUE
      FROMGF=IPP(2,IGF)
      TOGF=IPP(5,FROMGF)
      ISTART=IPP(6,1)
      IF (OPENUP(FROMGF,FILNAM,MAXGRD).NE.0) GOTO 99
      IF (OPENUP(TOGF,FILNAM,MAXGRD).NE.0) GOTO 99
      IOUT=ISTART
      LO=IPP(3,1)
      HI=IPP(4,LO)
      LO=MIN0(MAX0(LO,1),MAXGRD)
      HI=MIN0(MAX0(HI,LO),MAXGRD)
 32   DO 37 NEXTI=LO,HI
         IF (IGGT3D(FROMGF,NEXTI,MAXWDS,GA,NR,NC,NL,TABLE).NE.0)
     *        GO TO 37
 36      IF (IGPT3D(TOGF,-IOUT,GA,NR,NC,NL,TABLE,JOUT).LT.0) GOTO 91
         IOUT=JOUT+1
 37   CONTINUE
 38   CALL SDEST(' ---DONE',0)
      GOTO 99
C
C-----MAKE FUNCTION
C
 40   CONTINUE
      IGRID1=IPP(2,1)
      IGRID2=IKWP('V',1,IGRID1+1)
      COPER=CPP(3,' ')
      IGRID3=IPP(4,0)
      IF(COPER(:3).EQ.'SUB') IGRID2=IGRID3
      IF(IGGT3D(IGF,IGRID1,MAXWDS,GA,NR,NC,NL,TABLE).NE.0) THEN
         CALL EDEST('GRID NOT FOUND ',IGRID1)
         RETURN
      ENDIF
      IF(COPER.NE.'EXT'.AND.COPER.NE.'FIL') THEN
         IF(IGGT3D(IGF,IGRID2,MAXWDS,GB,NR,NC,NL,TABLE).NE.0) THEN
            CALL EDEST('GRID NOT FOUND ',IGRID2)
            RETURN
         ENDIF
      ENDIF
C
C
      IF(COPER.EQ.'SPD') THEN
         CALL SPEED(NR,NC,NL,GA,GB,GC)
         TABLE(9)=LIT('SPD ')
      ELSE IF(COPER(:3).EQ.'SUB') THEN
         LOG=0
         IF(COPER(4:4).EQ.'L') LOG=1
         CALL SUB(LOG,NR,NC,NL,GA,GB,GC)
      ELSE IF(COPER.EQ.'FIL') THEN
         XVAL=DPP(4,0.D0)
         LEV1=IPP(5,1)
         LEV2=IPP(6,NL)
         IF(CPP(4,' ').EQ.'MISS') XVAL=1.E35
         CALL FILGRD(GA,GC,NR,NC,NL,XVAL,LEV1,LEV2)
      ELSE
         CALL SDEST('INVALID OPTION '//COPER,0)
         RETURN
      ENDIF
      TABLE(9)=LIT(CKWP('NAME',1,CLIT(TABLE(9))))
      IGRID=IKWP('GRID ',1,0)
      IF (IGPT3D(IGF,-IGRID,GC,NR,NC,NL,TABLE,IOUT).LT.0) GOTO 91
      CALL SDEST(' RESULTS INTO GRID# ',IOUT)
      GOTO 99
C
C-----INFO OPTION
C
 50   CONTINUE
      IF (OPENUP(IGF,FILNAM,MAXGRD).NE.0) GOTO 99
      LO=IPP(2,1)
      HI=IPP(3,LO)
      IF(NKWP(' ').EQ.1) HI=MAXGRD
      LO=MIN0(LO,MAXGRD)
      HI=MIN0(HI,MAXGRD)
      CALL DDEST('LO/HI '//CFI(LO)//CFI(HI),0)
      CALL PRGSTA(IGF,LO,HI)
 55   CALL SDEST('  ---END OF LISTING',0)
      GOTO 99
C
C COPY OPTION
C
60    CONTINUE
      IGRID1=IPP(2,1)
      IGRID2=IPP(3,1)
      IF(IGGT3D(IGF,IGRID1,MAXWDS,GA,NR,NC,NL,TABLE).NE.0) THEN
         CALL EDEST('GRID NOT FOUND ',IGRID1)
         RETURN
      ENDIF
      TABLE(9)=LIT(CKWP('NAME',1,CLIT(TABLE(9))))
      IF (IGPT3D(IGF,-IGRID2,GA,NR,NC,NL,TABLE,IOUT).LT.0) GOTO 91
      CALL SDEST(' COPIED INTO GRID# ',IOUT)
      GOTO 99
C
C INTERP OPTION
C
70    CONTINUE
      IGRID=IPP(2,1)
      LDAY=IPP(3,0)
      LTIME=MPP(4,0)
      DUSPD=DKWP('LAG',1,0.0D0)
      DVSPD=DKWP('LAG',2,0.0D0)
      ISTEP=IKWP('SETDEL',1,1)
      NGRIDS=IKWP('SETDEL',2,2)
      CALL SCOUT('IGRID',IGRID,LDAY,LTIME,ISTEP,NGRIDS,0)
C
      KDAY=IDAYS(LDAY)
      KTIME=ISECS(LTIME)
      NLOOP=NGRIDS-1
      IF(IGGT3D(IGF,IGRID,MAXWDS,GA,JR,JC,JL,TABLE).NE.0) THEN
         CALL EDEST('GRID NOT FOUND ',IGRID)
         RETURN
      ENDIF
      JRJCJL=JR*JC*JL
      JDAY=IDAYS(TABLE(6))
      JTIME=ISECS(TABLE(7))
      CALL SCOUT('KDAY',KDAY,KTIME,NLOOP,JRJCJL,JDAY,JTIME)
      DO 75 ILOOP=1,NLOOP
      IGRID=IGRID+ISTEP
      IR=JR
      IC=JC
      IL=JL
      IRICIL=JRJCJL
      IDAY=JDAY
      ITIME=JTIME
      DO 71 I=1,IRICIL
71    GC(I)=GA(I)
      DO 72 I=1,64
72    TABLE2(I)=TABLE(I)
      IF(IGGT3D(IGF,IGRID,MAXWDS,GA,JR,JC,JL,TABLE).NE.0) THEN
         CALL EDEST('GRID NOT FOUND ',IGRID)
         RETURN
      ENDIF
      JRJCJL=JR*JC*JL
      JDAY=IDAYS(TABLE(6))
      JTIME=ISECS(TABLE(7))
      CALL SCOUT('ILOOP',ILOOP,IGRID,IDAY,ITIME,JDAY,JTIME)
      IF(KDAY .LT. IDAY) GO TO 75
      IF(KDAY .EQ. IDAY .AND. KTIME .LT. ITIME) GO TO 75
      IF(KDAY .GT. JDAY) GO TO 75
      IF(KDAY .EQ. JDAY .AND. KTIME .GT. JTIME) GO TO 75
      IF(IR .NE. JR .AND. IC .NE. JC .AND. IL .NE. JL) GO TO 79
      CALL LAG(JR,JC,JL,GA,GB,TABLE,LDAY,LTIME,DUSPD,DVSPD)
      CALL LAG(IR,IC,IL,GC,GA,TABLE2,LDAY,LTIME,DUSPD,DVSPD)
      DIFA=86400*(KDAY-IDAY)+KTIME-ITIME
      DIFB=86400*(JDAY-KDAY)+JTIME-KTIME
      DA=DIFB/(DIFA+DIFB)
      DB=DIFA/(DIFA+DIFB)
      CALL ROUT('DIFA',DIFA,DIFB,DA,DB,DUSPD,DVSPD)
      DO 74 I=1,IRICIL
      IF(GA(I) .GE. XTEST .OR. GB(I) .GE. XTEST) GO TO 73
      GC(I)=DA*GA(I)+DB*GB(I)
      GO TO 74
73    GC(I)=XNULL
74    CONTINUE
      TABLE(6)=LDAY
      TABLE(7)=LTIME
      IF (IGPT3D(IGF,0,GC,IR,IC,IL,TABLE,IOUT).LT.0) GOTO 91
      CALL SDEST(' INTERP INTO GRID# ',IOUT)
      GO TO 99
75    CONTINUE
      CALL EDEST('TIME NOT IN RANGE ',0)
      GO TO 99
79    CALL EDEST('GRID SIZES DO NOT MATCH ',0)
      GO TO 99
C
C-----RH GRID
C
80    CONTINUE
      IGRID1=IPP(2,1)
      IGRID2=IPP(3,1)
      IGRID3=IPP(4,1)
      IF(IGGT3D(IGF,IGRID1,MAXWDS,GA,NR,NC,NL,TABLE).NE.0) THEN
         CALL EDEST('GRID NOT FOUND ',IGRID1)
         RETURN
      ENDIF
      IF(IGGT3D(IGF,IGRID2,MAXWDS,GB,NRB,NCB,NLB,TABLE2).NE.0) THEN
         CALL EDEST('GRID NOT FOUND ',IGRID2)
         RETURN
      ENDIF
      IF(IGGT3D(IGF,IGRID3,MAXWDS,GC,NRC,NCC,NLC,TABLE2).NE.0) THEN
         CALL EDEST('GRID NOT FOUND ',IGRID3)
         RETURN
      ENDIF
      IF(NR .NE. NRB .OR. NC .NE. NCB .OR. NL .NE. NLB) GO TO 92
      NRNCNL=NR*NC*NL
      DO 82 I=1,NRNCNL
      IF(GA(I).GT.XTEST.OR.GB(I).GT.XTEST.OR.GC(I).GT.XTEST) GO TO 82
C
C----COMPUTE PARTIAL PRESSURE
C
      DPART=(6.11*(273.16/GB(I))**5.31)*EXP(25.22*(1-273.16/GB(I)))
C
C----CONVERT TO G/KG
C
      DW=1000.*(.62197*DPART/(GC(I)-DPART))
      GC(I)=100.*GA(I)/DW
82    CONTINUE
      TABLE(9)=LIT('RH  ')
      TABLE(10)=LIT('PCT ')
      IF (IGPT3D(IGF,0,GC,NR,NC,NL,TABLE,IOUT).LT.0) GOTO 91
      CALL SDEST(' RH INTO GRID# ',IOUT)
      GOTO 99
C
C-----THETA GRID
C
100   CONTINUE
      IGRID1=IPP(2,1)
      IGRID2=IPP(3,1)
      IOFF=IPP(4,0)
      IF(IGGT3D(IGF,IGRID1,MAXWDS,GA,NR,NC,NL,TABLE).NE.0) THEN
         CALL EDEST('GRID NOT FOUND ',IGRID1)
         RETURN
      ENDIF
      IF(IGGT3D(IGF,IGRID2,MAXWDS,GB,NRB,NCB,NLB,TABLE2).NE.0) THEN
         CALL EDEST('GRID NOT FOUND ',IGRID2)
         RETURN
      ENDIF
      IF(NR .NE. NRB .OR. NC .NE. NCB .OR. NL .NE. NLB) GO TO 92
      NRNCNL=NR*NC*NL
      DO 102 I=1,NRNCNL
      IF(GA(I).GT.XTEST.OR.GB(I).GT.XTEST) THEN
         GC(I)=1.E35
         GO TO 102
      ENDIF
      IF(IOFF.NE.0) THEN
         GC(I) = IOFF-GA(I)*(1000/GB(I))**.286
      ELSE
         GC(I) = GA(I)*(1000/GB(I))**.286
      ENDIF
102   CONTINUE
      TABLE(9)=LIT('THET')
      TABLE(10)=LIT('K   ')
      IF (IGPT3D(IGF,0,GC,NR,NC,NL,TABLE,IOUT).LT.0) GOTO 91
      CALL SDEST(' THETA INTO GRID# ',IOUT)
      GOTO 99
110   CONTINUE
      IGRID=IPP(2,1)
      KDAY=IPP(3,0)
      KTIME=MPP(4,0)
      DUSPD=DPP(5,0.0)
      DVSPD=DPP(6,0.0)
      IF(IGGT3D(IGF,IGRID,MAXWDS,GA,NR,NC,NL,TABLE).NE.0) THEN
         CALL EDEST('GRID NOT FOUND ',IGRID)
         RETURN
      ENDIF
C
      CALL LAG(NR,NC,NL,GA,GC,TABLE,KDAY,KTIME,DUSPD,DVSPD)
C
      IF (IGPT3D(IGF,0,GC,NR,NC,NL,TABLE,IOUT).LT.0) GOTO 91
      CALL SDEST(' LAGRANGE TRANSFORM INTO GRID# ',IOUT)
      GOTO 99
C
C-----ADD FUNCTION
C
 120  CONTINUE
      NGRIDS=NKWP(' ')-1
      IF (NGRIDS.LT.1.OR.NGRIDS.GT.MXGRDS) THEN
         CALL EDEST('ADD OPTION - INVALID NUMBER OF GRIDS = ',NGRIDS)
         RETURN
      ENDIF
      DO 121 I=1,NGRIDS
      IGG(I)=IPP(I+1,0)
      IF (IGGT3D(IGF,IGG(I),MAXWDS,GG(1,I),NR,NC,NL,TABLE).NE.0) THEN
         CALL EDEST('GRID NOT FOUND ',IGG(I))
         RETURN
      ENDIF
121   CONTINUE
      CALL ADD(NR,NC,NL,NGRIDS,GG,GA)
      TABLE(9)=LIT(CKWP('NAME',1,'    '))
      IF (IGPT3D(IGF,0,GA,NR,NC,NL,TABLE,IOUT).LT.0) GO TO 91
      CALL SDEST('RESULTS INTO GRID# ',IOUT)
      GO TO 99
C
C AVERAGE OPTION
C
130   CONTINUE
      IGRID=IPP(2,1)
      IGRIDA=IPP(3,1)
      IGRIDB=IPP(4,1)
      CALL SCOUT('IGRID',IGRID,IGRIDA,IGRIDB,0,0,0)
C
      IF(IGGT3D(IGF,IGRIDA,MAXWDS,GA,IR,IC,IL,TABLE).NE.0) THEN
         CALL EDEST('GRID NOT FOUND ',IGRIDA)
         RETURN
      ENDIF
      IRICIL=IR*IC*IL
      IDAY=IDAYS(TABLE(6))
      ITIME=ISECS(TABLE(7))
      DO 132 I=1,64
132   TABLE2(I)=TABLE(I)
      IF(IGGT3D(IGF,IGRIDB,MAXWDS,GB,JR,JC,JL,TABLE).NE.0) THEN
         CALL EDEST('GRID NOT FOUND ',IGRIDB)
         RETURN
      ENDIF
      JRJCJL=JR*JC*JL
      JDAY=IDAYS(TABLE(6))
      JTIME=ISECS(TABLE(7))
      CALL SCOUT('JI',JRJCJL,JDAY,JTIME,IRICIL,IDAY,ITIME)
C DEFAULT LDAY AND LTIME IS MIDWAY BETWEEN J AND I
      IF(JDAY .LT. IDAY) GO TO 135
      IF(JDAY .EQ. IDAY) THEN
        IF(JTIME .LE. ITIME) GO TO 135
        KDAY=IDAY
        KTIME=(ITIME+JTIME)/2
      ELSE
        KTIME=(ITIME+JTIME+86400*(JDAY-IDAY))/2
        KDAY=IDAY+KTIME/86400
        KTIME=MOD(KTIME,86400)
      ENDIF
      LDAY=IYYDDD(KDAY)
      LTIME=IHMS(KTIME)
C
      LDAY=IPP(5,LDAY)
      LTIME=MPP(6,LTIME)
      KDAY=IDAYS(LDAY)
      KTIME=ISECS(LTIME)
      CALL SCOUT('LDAY',LDAY,LTIME,KDAY,KTIME,0,0)
      IF(KDAY .LT. IDAY) GO TO 135
      IF(KDAY .EQ. IDAY .AND. KTIME .LT. ITIME) GO TO 135
      IF(KDAY .GT. JDAY) GO TO 135
      IF(KDAY .EQ. JDAY .AND. KTIME .GT. JTIME) GO TO 135
      IF(IR .NE. JR .AND. IC .NE. JC .AND. IL .NE. JL) GO TO 139
      DIFA=86400*(KDAY-IDAY)+KTIME-ITIME
      DIFB=86400*(JDAY-KDAY)+JTIME-KTIME
      DA=DIFB/(DIFA+DIFB)
      DB=DIFA/(DIFA+DIFB)
      CALL ROUT('DIFA',DIFA,DIFB,DA,DB,DUSPD,DVSPD)
      DO 134 I=1,IRICIL
      IF(GA(I) .GE. XTEST .OR. GB(I) .GE. XTEST) GO TO 133
      GC(I)=DA*GA(I)+DB*GB(I)
      GO TO 134
133   GC(I)=XNULL
134   CONTINUE
      TABLE(6)=LDAY
      TABLE(7)=LTIME
      IF (IGPT3D(IGF,-IGRID,GC,IR,IC,IL,TABLE,IOUT).LT.0) GOTO 91
      CALL SDEST(' INTERP INTO GRID# ',IOUT)
      GO TO 99
135   CONTINUE
      CALL EDEST('TIME NOT IN RANGE ',0)
      GO TO 99
139   CALL EDEST('GRID SIZES DO NOT MATCH ',0)
      GO TO 99
C   
C--- CALCULATE W
C 
 140  CONTINUE
      IGRID1=IPP(2,1)
      IGRID2=IPP(3,1)
      IGRID3=IPP(4,1)
      IF(IGGT3D(IGF,IGRID1,MAXWDS,GA,NR,NC,NL,TABLE).NE.0) THEN
         CALL EDEST('GRID NOT FOUND ',IGRID1)
         RETURN
      ENDIF
      IF(IGGT3D(IGF,IGRID2,MAXWDS,GB,NRB,NCB,NLB,TABLE2).NE.0) THEN
         CALL EDEST('GRID NOT FOUND ',IGRID2)
         RETURN
      ENDIF
      IF(IGGT3D(IGF,IGRID3,MAXWDS,GC,NRC,NCC,NLC,TABLE2).NE.0) THEN
         CALL EDEST('GRID NOT FOUND ',IGRID3)
         RETURN
      ENDIF
      IF(NR .NE. NRB .OR. NC .NE. NCB .OR. NL .NE. NLB) GO TO 92
      NRNCNL=NR*NC*NL
      DO 142 I=1,NRNCNL
      IF(GA(I).GT.XTEST.OR.GB(I).GT.XTEST.OR.GC(I).GT.XTEST)GO TO 142
      GC(I)=-(GA(I)*GB(I)*29.355)/GC(I) 
142   CONTINUE
      TABLE(9)=LIT('W   ')
      TABLE(10)=LIT('MPS ')
      IGRID=IKWP('GRID ',1,0)
      IF (IGPT3D(IGF,-IGRID,GC,NR,NC,NL,TABLE,IOUT).LT.0) GOTO 91
      CALL SDEST(' W INTO GRID# ',IOUT)
      GOTO 99

C
C-----ERRORS
C
 91   CALL SDEST(' ---NO MORE ROOM IN GRID FILE. ',0)
      GOTO 99
C
 92   CALL SDEST(' ---GRIDS DONT MATCH. ',0)
      GOTO 99
C
 99   CONTINUE
      RETURN
      END
C
C
C
C
      SUBROUTINE LAG(NR,NC,NL,GA,GC,TABLE,KDAY,KTIME,DUSPD,DVSPD)
      IMPLICIT CHARACTER*12 (C)
      IMPLICIT REAL*8 (D)
      CHARACTER*4 CLIT
      REAL*4 GA(NR,NC,NL),GC(NR,NC,NL),XNULL,XTEST
      INTEGER TABLE(64)
      DATA XTEST/1.E30/,XNULL/1.E35/
      DATA RPDG/0.01745329/,XMPDG/111137.0/
C
      IDAY=TABLE(6)
      ITIME=TABLE(7)
      IDIF=86400*(IDAYS(IDAY)-IDAYS(KDAY))+ISECS(ITIME)-ISECS(KTIME)
      IF(TABLE(22) .NE. 1) GO TO 10
      XRINC=TABLE(25)/10000.0
      XCINC=TABLE(25)/10000.0
      GO TO 20
10    IF(TABLE(22) .NE. 4) GO TO 999
      XRINC=TABLE(25)/10000.0
      XCINC=TABLE(26)/10000.0
20    XLAT=TABLE(23)/10000.0
      ZCINC=(DUSPD*IDIF)/(XMPDG*COS(RPDG*XLAT)*XCINC)
      ZRINC=-(DVSPD*IDIF)/(XMPDG*XRINC)
      X=0.0
      IF(CLIT(TABLE(9)) .EQ. 'U   ') X=DUSPD
      IF(CLIT(TABLE(9)) .EQ. 'V   ') X=DVSPD
      CALL ROUT('LAG',XRINC,XCINC,XLAT,ZCINC,ZRINC,X)
      CALL SCOUT('IDIF',IDIF,IDAY,ITIME,KDAY,KTIME,0)
      DO 100 I=1,NR
      ZI=I+ZRINC
      IF(ZI .LT. 0.99 .OR. ZI .GT. NR+0.01) GO TO 90
      IZ=ZI
      IF(IZ .LT. 1) IZ=1
      IF(IZ .GT. NR-1) IZ=NR-1
      BI=ZI-IZ
      AI=1.0-BI
      DO 80 J=1,NC
      ZJ=J+ZCINC
      IF(ZJ .LT. 0.99 .OR. ZJ .GT. NC+0.01) GO TO 70
      JZ=ZJ
      IF(JZ .LT. 1) JZ=1
      IF(JZ .GT. NC-1) JZ=NC-1
      BJ=ZJ-JZ
      AJ=1.0-BJ
      ZAA=AI*AJ
      ZAB=AI*BJ
      ZBA=BI*AJ
      ZBB=BI*BJ
      DO 60 K=1,NL
      ZZ=0.0
      ZG=0.0
      AA=GA(IZ,JZ,K)
      IF(AA .GE. XTEST) GO TO 30
      ZZ=ZZ+ZAA
      ZG=ZG+ZAA*AA
30    AB=GA(IZ,JZ+1,K)
      IF(AB .GE. XTEST) GO TO 35
      ZZ=ZZ+ZAB
      ZG=ZG+ZAB*AB
35    BA=GA(IZ+1,JZ,K)
      IF(BA .GE. XTEST) GO TO 40
      ZZ=ZZ+ZBA
      ZG=ZG+ZBA*BA
40    BB=GA(IZ+1,JZ+1,K)
      IF(BB .GE. XTEST) GO TO 45
      ZZ=ZZ+ZBB
      ZG=ZG+ZBB*BB
45    IF(ZZ .LT. 0.6) GO TO 50
      GC(I,J,K)=AI*(AJ*AA+BJ*AB)+BI*(AJ*BA+BJ*BB)-X
      GO TO 60
50    GC(I,J,K)=XNULL
60    CONTINUE
      GO TO 80
70    DO 75 K=1,NL
75    GC(I,J,K)=XNULL
80    CONTINUE
      GO TO 100
90    DO 95 J=1,NC
      DO 95 K=1,NL
95    GC(I,J,K)=XNULL
100   CONTINUE
      RETURN
999   CALL EDEST('MUST BE PSUEDO-MERCATOR GRID ',0)
      CALL ABORT()
      RETURN
      END
C
C
C
C
      SUBROUTINE EXTGRD(GA,NR,NC,NL,ITAB,ITAB2,ILEV,IGRID)
      CHARACTER*12 CFF
      REAL*4 GA(NR,NC,NL)
      INTEGER*4 ITAB(*),ITAB2(*),ILEV,IGRID(NR,NC)
      DATA NULL/'80808080'X/,XTEST/1.E30/,XNULL/1.E35/
      JLEV=ILEV
      DO 1 IR=1,NR
      DO 1 IC=1,NC
      IF(GA(IR,IC,JLEV).LT.XTEST) THEN
         IGRID(IR,IC)=NINT(GA(IR,IC,JLEV)*100.)
      ELSE
         IGRID(IR,IC)=NULL
      ENDIF
1     CONTINUE
      CALL ZEROW(64,ITAB2)
      ITAB2(1)=NR*NC
      ITAB2(2)=NR
      ITAB2(3)=NC
      CALL MOVW(4,ITAB(6),ITAB2(4))
      ITAB2(8)=2
      ITAB2(9)=ITAB(10)
      XHGTT=ITAB(32)/1000.
      XHGTIN=ITAB(33)/1000.
      XHGT=XHGTT-XHGTIN*(NL-ILEV)
      CALL SDEST('HEIGHT IS '//CFF(XHGT,3)//' KM',0)
      ITAB2(10)=NINT(XHGT)
      ITAB2(11)=0
      ITAB2(12)=LIT('KM  ')
      IF(ITAB(22) .NE. 1 .AND. ITAB(22) .NE. 4) GO TO 2
      CALL MOVW(3,ITAB(22),ITAB2(34))
      ITAB2(37)=ITAB(23)-ITAB(25)*(NR-1)
      ITAB2(38)=ITAB(24)-ITAB(26)*(NC-1)
      CALL MOVW(2,ITAB(25),ITAB2(39))
      RETURN
2     CALL MOVW(7,ITAB(22),ITAB2(34))
      RETURN
      END
C
C
C
C
      SUBROUTINE FILGRD(GA,GC,NR,NC,NL,XVAL,LEV1,LEV2)
      REAL*4 GA(NR,NC,NL),GC(NR,NC,NL)
      CALL MOVW(NR*NC*NL,GA,GC)
      DO 1 IL=LEV1,LEV2
      DO 1 IR=1,NR
      DO 1 IC=1,NC
      GC(IR,IC,IL)=XVAL
1     CONTINUE
      RETURN
      END
C
C
C
C
      FUNCTION OPENUP(GF,FILNAM,MAXGRD)
C-----OPEN THE GRID FILE; DISPLAY HEADER LINE ON CRT; RETURN
C-----FILE NAME IN FILNAM.
C-----FN VALUE IS 0 (OK) -1 (CANT OPEN; PROBABLY DOESNT EXIST)
C
      IMPLICIT INTEGER (A-Z)
      INTEGER LIN(13)
      CHARACTER*8 FILNAM
      CHARACTER*32 DOC
      CHARACTER*12 CFI,CGF
C
C
      OPENUP=IGOP3D(GF,FILNAM)
      IF (OPENUP.NE.0) GOTO 91
      CALL LWI(FILNAM,0,13,LIN)
      CALL MOVWC(LIN,DOC)
      CGF=CFI(GF)
      CALL SDEST('3-D GRID FILE: '//CGF(8:12)//' **'//DOC//'**',0)
      MAXGRD=LIN(12)
      RETURN
 91   CALL SDEST(' GRIDFILE DOES NOT EXIST ',GF)
      RETURN
      END
C
C
C
      SUBROUTINE PRGH3D(GRIDNO,TABLE)
C PRGH3D BARRET 1081 SVCLIB PRINT 1 LINE OF GRID HEADER INFO
C $ CALL PRGH3D(GRIDNO, TABLE)  (DAS)
C $ PRINT 1 LINE OF INFO FROM A GRID HEADER.  FIRST CALL DISPLAYS
C $   LISTING HEADINGS AS WELL.
C $ GRIDNO = (I) INPUT  GRID NUMBER
C $ TABLE = (I) INPUT  64 WORD GRID HEADER FOR GRIDNO
C $$ PRGH3D = GRID
C
      IMPLICIT INTEGER (A-W)
      INTEGER TABLE(*)
      CHARACTER*70 CLINE
C
      DATA INIT/0/
C
      IF (INIT.EQ.1) GOTO 2
         INIT=1
         CALL SDEST('  #  YYDDD HHMMSS NAME  NR  '//
     *'NC  NL   LLNW     TOP        INC',0)
         CALL SDEST(' --- ----- ------ ----  --  '//
     *'--  -- --------  -----  -------------',0)
         GOTO 2
 2    CONTINUE
      IF (TABLE(1).LE.0) GOTO 99
      ILATN=TABLE(23)/10000
      ILONW=TABLE(24)/10000
      ITOP=TABLE(32)/1000
      ZLATI=TABLE(25)/10000.
      ZLONI=TABLE(26)/10000.
      ZTOPI=TABLE(33)/1000.
C     CALL BLKA(35,LINE)
C     CALL ENCODE('(1X,I3,I6,I7,1X,A4,*3I4,I4,"/",I4,I5,'//
C    *'3X,F4.1,"/",F4.1,"/",F4.1)',
C    *LINE,GRIDNO,TABLE(6),TABLE(7),TABLE(9),TABLE(2),ILATN,ILONW,ITOP,
C    *ZLATI,ZLONI,ZTOPI)
C     CALL LTQ(LINE)
 90   FORMAT(I4,I6,I7,1X,A4,3I4,I4,'/',I4,I5,
     *3X,F4.1,'/',F4.1,'/',F4.1)
      WRITE (CLINE,90) GRIDNO,TABLE(6),TABLE(7),TABLE(9),
     *(TABLE(I),I=2,4),ILATN,ILONW,ITOP,ZLATI,ZLONI,ZTOPI
      CALL SDEST(CLINE,0)
 99   RETURN
      END
C
C
C
C
      SUBROUTINE PRGSTA(GFNO,ILO,IHI)
C $ CALL PRGSTA(GRIDNO, TABLE)  (DAS)
C $ PRINT 1 LINE OF STATS FROM A GRID.  FIRST CALL DISPLAYS
C $   LISTING HEADINGS AS WELL.
C $ GRIDNO = (I) INPUT  GRID NUMBER
C $ TABLE = (I) INPUT  64 WORD GRID HEADER FOR GRIDNO
C $$ PRGSTA = GRID
C
      IMPLICIT INTEGER (A-W)
      CHARACTER*132 CLINE
      PARAMETER (MAXWDS=300000)
      REAL*4 GRID(MAXWDS)
      INTEGER TABLE(64)
C
         CALL SDEST('  #  YYDDD HHMMSS NAME '//
     *'       MEAN +/- SD                 RANGE         MDATA',0)
         CALL SDEST(' --- ----- ------ ---- '//
     *'-------------------------  --------------------- -----',0)
C
      DO 99 GNO=ILO,IHI
      IF(IGGT3D(GFNO,GNO,MAXWDS,GRID,NR,NC,NL,TABLE).NE.0) GO TO 99
      CALL STATG(NR*NC*NL,GRID,XM,XSD,XLO,XHI,IMISS)
      WRITE(CLINE,701) GNO,TABLE(6),TABLE(7),TABLE(9),XM,XSD,XLO,XHI,
     *IMISS
 701  FORMAT(1X,I3,I6,I7,1X,A4,1X,G10.4,' +/- ',G10.4,
     *2X,G10.4,1X,G10.4,1X,I5)
      CALL LTQ(CLINE)
 99   CONTINUE
      RETURN
      END
C
C
C
C
      SUBROUTINE SPEED(LR,LC,LH,GA,GB,GC)
      REAL*4 GA(*),GB(*),GC(*)
      DATA XTEST/1.E30/,XNULL/1.E35/
      LOOP=LR*LC*LH
      DO 1 I=1,LOOP
      IF(GA(I).GT.XTEST.OR.GB(I).GT.XTEST) THEN
         GC(I)=XNULL
      ELSE
         GC(I)=SQRT(GA(I)*GA(I)+GB(I)*GB(I))
      ENDIF
1     CONTINUE
      RETURN
      END
C
C
C
C
      SUBROUTINE SUB(LOG,LR,LC,LH,GA,GB,GC)
      REAL*4 GA(*),GB(*),GC(*)
      DATA XTEST/1.E30/,XNULL/1.E35/
      LOOP=LR*LC*LH
      IF(LOG.EQ.0) THEN
         DO 1 I=1,LOOP
         IF(GA(I).GT.XTEST.OR.GB(I).GT.XTEST) THEN
            GC(I)=XNULL
         ELSE
            GC(I)=GA(I)-GB(I)
         ENDIF
 1       CONTINUE
      ELSE
         DO 2 I=1,LOOP
         IF(GA(I).GT.XTEST.OR.GB(I).GT.XTEST) THEN
            GC(I)=XNULL
         ELSE
            GC(I)=ALOG(GA(I)/GB(I))
         ENDIF
 2       CONTINUE
      ENDIF
      RETURN
      END



C     Compute statistics for a grid.
C     Input:  NPTS - number of elements in XBUF
C             XBUF - array of grid values
C     Output:  XM - the mean
C              SD - the standard deviation
C              XLO - min value
C              XHI - max value
C              IMISS - number of missing values
      SUBROUTINE STATG(NPTS,XBUF,XM,SD,XLO,XHI,IMISS)
      REAL*8 SUMX,SUMX2
      REAL*4 XBUF(NPTS)
      DATA XTEST/1.E30/
      SUMX=0.D0
      SUMX2=0.D0
      XM=0.0
      SD=0.0
      XLO=1.E8
      XHI=-1.E8
      MPTS=0
      IMISS=0
      CALL DDEST('BEGIN LOOP ',NPTS)
      DO 1 III=1,NPTS
         XVAL=XBUF(III)
         IF(XVAL.GE.XTEST) THEN
            IMISS=IMISS+1
         ELSE
            MPTS=MPTS+1
            IF(XVAL .LT. XLO) XLO=XVAL
            IF(XVAL .GT. XHI) XHI=XVAL
            SUMX=SUMX+XVAL
            SUMX2=SUMX2+XVAL*XVAL
         ENDIF
 1    CONTINUE

      CALL DDEST('DONE WITH LOOP ',MPTS)
      IF (MPTS.EQ.0) THEN
         XM=99.9999
         SD=99.9999
      ELSE
         XM=SUMX/MPTS
         SD=SQRT((SUMX2-SUMX*SUMX/MPTS)/(MPTS-1))
      ENDIF
      RETURN
      END
C
C
C
C
      SUBROUTINE ADD(NR,NC,NL,NGRIDS,GG,GA)
      PARAMETER (MAXWDS=200000,MXGRDS=6)
      REAL*4 XTEST,XNULL
      REAL*4 GG(MAXWDS,MXGRDS),GA(MAXWDS)
      DATA XTEST/1.E30/,XNULL/1.E35/
C-----
      NPTS=NR*NC*NL
      DO 100 I=1,NPTS
      GA(I)=0.0
      MD=0
      DO 50 J=1,NGRIDS
      IF (GG(I,J).LT.XTEST) THEN
         GA(I)=GA(I)+GG(I,J)
      ELSE
         MD=MD+1
      ENDIF
50    CONTINUE
      IF (MD.EQ.NGRIDS) GA(I)=XNULL
100   CONTINUE
      RETURN
      END
