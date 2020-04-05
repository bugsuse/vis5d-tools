C sdest.f

C VIS-5D version 4.0
C
C VIS-5D system for visualizing five dimensional gridded data sets
C Copyright (C) 1990-1994  Bill Hibbard and Dave Santek
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



      SUBROUTINE EDEST(C,I)
      CHARACTER*200 CTEXT
      CHARACTER *(*) C
      L=MIN0(200,LEN(C))
      CTEXT=C(1:L)
      CALL CDEST(CTEXT(1:L),I)
      END
C
      SUBROUTINE SDEST(C,I)
      CHARACTER*200 CTEXT
      CHARACTER*(*) C
      L=MIN0(200,LEN(C))
      CTEXT=C(1:L)
      CALL CDEST(CTEXT(1:L),I)
      RETURN
      END
C
      SUBROUTINE DDEST(C,I)
      CHARACTER *(*) C
C     CALL CDEST(C,I)
      RETURN
      END
C
      REAL*8 FUNCTION DPP(I,X)
      REAL*8 X
      CHARACTER*12  CFD,CVAL,CPP ,CX 
      CX=CFD(X,6)
      CVAL=CPP(I,CX)
      READ(CVAL,*) DPP
      RETURN
      END
C
      INTEGER FUNCTION MPP(I,M)
      MPP=IPP(I,M)
      RETURN
      END
C
      INTEGER FUNCTION IPP(I,M)
      INTEGER*4 M
      CHARACTER*12  CFI,CVAL,CPP ,CX 
      CX=CFI(M,6)
      CVAL=CPP(I,CX)
      READ(CVAL,*) IPP
      RETURN
      END
C
      CHARACTER*12 FUNCTION CPP(I,CDEFLT)
      IMPLICIT CHARACTER*12 (C)
      CHARACTER*(*) CDEFLT
      CPP=CKWP(' ',I,CDEFLT)
      RETURN
      END
C
      REAL*8 FUNCTION DKWPLL(CD,I,X)
      REAL*8 X,DKWP
      CHARACTER*(*) CD
      DKWPLL=DKWP(CD,I,X)
      RETURN
      END
C
      REAL*8 FUNCTION DKWP(CD,I,X)
      REAL*8 X
      CHARACTER*12  CVAL,CKWP ,CX 
      CHARACTER*(*) CD
      CX=' '
      CVAL=CKWP(CD,I,CX)
      IF (CVAL .EQ. ' ') THEN
        DKWP=X
      ELSE
        READ(CVAL,*) DKWP
      END IF
      RETURN
      END
C
      INTEGER FUNCTION IKWP(CD,I,M)
      INTEGER*4 M
      CHARACTER*12  CVAL,CKWP ,CX 
      CHARACTER*(*) CD
      CX=' '
      CVAL=CKWP(CD,I,CX)
      IF (CVAL .EQ. ' ') THEN
        IKWP=M
      ELSE
        READ(CVAL,*) IKWP
      END IF
      RETURN
      END
C
C     CHARACTER*12 FUNCTION CFI(II)
C     WRITE(CFI,'(I12)') II  
C     RETURN
C     END
C
      CHARACTER*12 FUNCTION CFI(II)
      I=II
      CFI='           0'
      IF(I .EQ. 0) RETURN
      I=IABS(I)
      DO 20 J=12,1,-1
      JJ=J-1
      K=I/10
      L=I-10*K
      I=K
      CFI(J:J)=CHAR(ICHAR('0')+L)
      IF(I .EQ. 0) GO TO 30
20    CONTINUE
30    IF(II .LT. 0 .AND. JJ .GT. 0) CFI(JJ:JJ)='-'
      RETURN
      END
C
      CHARACTER*12 FUNCTION CFJ(II)
      I=II
      CFJ='000000000000'
      IF(I .EQ. 0) RETURN
      I=IABS(I)
      DO 20 J=12,1,-1
      JJ=J-1
      K=I/10
      L=I-10*K
      I=K
      CFJ(J:J)=CHAR(ICHAR('0')+L)
      IF(I .EQ. 0) GO TO 30
20    CONTINUE
30    IF(II .LT. 0) CFJ(1:1)='-'
      RETURN
      END
C
      CHARACTER*12 FUNCTION CFE(X,I)
      CHARACTER*12 CFI,CFMT,CTEMP
      CFMT='(E12.N)'
      CTEMP=CFI(I)
      CFMT(6:6)=CTEMP(12:12)
      WRITE(CFE,CFMT) X
      RETURN
      END
C 
      CHARACTER*12 FUNCTION CFD(X,I)
      REAL*8 X
      CHARACTER*12 CFI,CFMT,CTEMP
      CFMT='(E12.N)'
      CTEMP=CFI(I)
      CFMT(6:6)=CTEMP(12:12)
      WRITE(CFD,CFMT) X
      RETURN
      END
C 
      CHARACTER*12 FUNCTION CFFF(X,I)
      CHARACTER*12 CFMT,CTEMP
      REAL*8 X
      WRITE(CFMT,701) I
 701  FORMAT('(F12.',I1,')')
      CTEMP=' '
      CALL SDEST('CFF'//CFMT//'**',IDNINT(X))
      Y=X 
      WRITE(CTEMP,CFMT) Y
      CFFF=CTEMP
C     J=X*10.0**I
C     CFF=CFI(J)
      RETURN
      END
C
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
C
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
C
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
C
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
C
      SUBROUTINE ZEROW(N,L)
      INTEGER L(*)
      DO 10 I=1,N
10    L(N)=0
      RETURN
      END
C
      SUBROUTINE MOVW(N,L,K)
      INTEGER L(*),K(*)
      DO 10 I=1,N
10    K(I)=L(I)
      RETURN
      END
C
      SUBROUTINE MOVWC(J,C)
      CHARACTER *(*) C
      I=LEN(C)
      CALL MOVB(I,J,C,0)
      RETURN
      END
C
      SUBROUTINE MOVCW(C,J)
      CHARACTER*(*) C
      I=LEN(C)
      CALL MOVB(I,C,J,0)
      RETURN
      END
C
      SUBROUTINE MOVC(N,SB,SO,DB,DO)
      IMPLICIT INTEGER (A-Z)
      CHARACTER*1 SB(*)
cc      INTEGER*1 SB(*)  
      CALL MOVB(N,SB(SO+1),DB,DO)
      RETURN
      END
C
      FUNCTION LIT(C)
      CHARACTER *(*) C
      CALL MOVB(4,C,J,0)
      LIT=J
      RETURN
      END
C
      CHARACTER*4 FUNCTION CLIT(I)
      CHARACTER*4 C
      CALL MOVWC(I,C)
      CLIT=C
      RETURN
      END
C 
      SUBROUTINE LTQ(IBUF)
      CHARACTER*80 CTEXT
      CALL MOVWC(IBUF,CTEXT)
      CALL SDEST(CTEXT,0)
      RETURN
      END
C
       CHARACTER*12 FUNCTION CFZ(L)
       IMPLICIT CHARACTER*12 (C)
       WRITE(C,1)L
 1     FORMAT(4X,Z8)
       CFZ=C
       RETURN
       END
      CHARACTER*12 FUNCTION CFF(DVALUE,IDEC)                            
C $ CFF(DVALUE, IDEC)  (RCD)                                            
C $ CONVERT REAL*8 TO CHARACTER*12 (EBCDIC) WITH LEADING ZEROS RATHER   
C $   THAN BLANKS (IN ROUNDED FORM TO F12.IDEC).                        
C $ DVALUE = (R*8) INPUT  VALUE THAT IS CONVERTED                       
C $ IDEC = (I) INPUT  NUMBER OF DESIRED DECIMAL PLACES                  
C $$ CFF = CONVERT, REAL, CHARACTER                                     
C                                                                       
      IMPLICIT CHARACTER*12 (C)                                         
      IMPLICIT REAL*8 (D)                                               
      PARAMETER (MXPREC=9)                                              
      PARAMETER (LENGTH=12)                                             
C                                                                       
C                                                                       
      JDEC=MIN0(MAX0(IDEC,0),MXPREC)                                    
C-----CHANGE TO DOUBLE PRECISION (FOR EXTRA ACCURACY IN CONVERSION)     
C-----   AND ROUND IN APPROPRIATE POSITION                              
      JEXP=10**JDEC                                                     
      DXVAL=DABS(DVALUE)*(10.D0**JDEC)+.5D0                             
      IF (DXVAL.GE.1.D09) GOTO 91                                       
      JVAL=IDINT(DXVAL)                                                 
      IVAL=JVAL/JEXP                                                    
      IF (DVALUE.LT.0) IVAL=-IVAL                                       
      IFRAC=MOD(JVAL,JEXP)+JEXP                                         
      JDECPT=LENGTH-JDEC                                                
      CTEMP=CFI(IFRAC)                                                  
      CFF(JDECPT:LENGTH)=CTEMP(JDECPT:LENGTH)                           
      CFF(JDECPT:JDECPT)='.'                                            
      CTEMP=CFI(IVAL)                                                   
      CFF(1:JDECPT-1)=CTEMP(JDEC+2:LENGTH)                              
      IF (IVAL.EQ.0.AND.DVALUE.LT.0) CFF(JDECPT-2:JDECPT-2)='-'         
      RETURN                                                            
C                                                                       
C-----MAGNITUDE ERROR; OUTPUT ASTERISKS.                                
C                                                                       
 91   CONTINUE                                                          
      CFF='***********'                                            
      RETURN                                                            
      END                                                               

      SUBROUTINE II(SIZE,VAL,STR,POS)                                   
C II     BARRET 1081 CONVERT INTEGER TO STRING (FIXED LENGTH FIELD)     
C $ SUBROUTINE II(SIZE, VAL, STR, POS)  (RCD)                           
C $ CONVERT INTEGER TO STRING (FIXED LENGTH FIELD)                      
C $ SIZE = (I) INPUT  LENGTH OF FIELD IN BYTES                          
C $ VAL = (I) INPUT  THE INTEGER TO CONVERT                             
C $ STR = (I) OUTPUT  THE OUTPUT ARRAY                                  
C $ POS = (I) OUTPUT  THE OFFSET (0-BASED) AT WHICH TO START THE OUTPUT 
C $$ II = CONVERT, INTEGER                                              
C                                                                       
C-----IF THE INTEGER (PLUS SIGN, IF NEC.) DOESNT FIT INTO STR,          
C-----AN ASTERISK IS PLACED IN THE HI-ORDER POSITION.                   
C                                                                       
      IMPLICIT INTEGER (A-Z)                                            
      DIMENSION STR(*)
      DATA ZERO/'30'X/
C                                                                       
C                                                                       
      IF (SIZE.LE.0.OR.POS.LT.0) GOTO 9                                 
      M=IABS(VAL)                                                       
C-----K ALWAYS POINTS AT THE NEXT POSITION IN THE OUTPUT FIELD,         
C-----   STARTING FROM THE RIGHT.                                       
      K=POS+SIZE-1                                                      
 2    CONTINUE                                                          
         CALL STC(MOD(M,10)+ZERO,STR,K)                                 
         M=M/10                                                         
         K=K-1                                                          
         IF (M.EQ.0) GOTO 3                                             
      IF (K.GE.POS) GOTO 2                                              
      GOTO 95                                                           
C-----CHECK FOR MINUS SIGN NECESSARY                                    
 3    IF (VAL.GE.0) GOTO 4                                              
         IF (K.LT.POS) GOTO 95                                          
         CALL STC(LIT('   -'),STR,K)
         K=K-1                                                          
C-----INSERT LEADING BLANKS                                             
 4    IF (K.LT.POS) GOTO 9                                              
         CALL STC(LIT('    '),STR,K)
         K=K-1                                                          
      GOTO 4                                                            
 9    RETURN                                                            
C-----INTEGER TOO BIG                                                   
 95   CALL STC(LIT('   *'),STR,POS)
      RETURN                                                            
      END                                                               

      INTEGER FUNCTION SKSECS(YYDDD,DHMS)                               
C $ FUNCTION SKSECS(YYDDD, DHMS)  (JOE)                                 
C $ CONVERT DATES TO SECONDS SINCE 0Z ON JAN 1,1972                     
C $ YYDDD = (I) INPUT  DATE TO BE CONVERTED. IF 0, DEFAULTS TO 72001.   
C $ DHMS = (I) INPUT  TIME (HHMMSS) TO BE CONVERTED                     
C $$ SKSECS = CONVERT, DATE, TIME                                       
C                                                                       
C                                                                       
      IMPLICIT INTEGER(A-Z)                                             
C                                                                       
C                                                                       
      YD=YYDDD                                                          
      IF (YD.EQ.0) YD=72001                                             
      YY=YD/1000                                                        
      DDD=MAX0(1,MOD(YD,1000))                                          
      IF (YY.GE.72.AND.YY.LE.99) GOTO 3                                 
      IF (YY.GE.0.AND.YY.LE.12) GOTO 2                                  
      YY=12                                                             
 2    YY=YY+100                                                         
 3    CONTINUE                                                          
      YDIF=YY-72                                                        
      D1=YDIF*365+(YDIF+3)/4+DDD-1                                      
      S1=D1*86400                                                       
      D2=DHMS/1000000                                                   
      HH=MOD(DHMS,1000000)/10000                                        
      MM=MOD(DHMS,10000)/100                                            
      SS=MOD(DHMS,100)                                                  
      SKSECS=S1+D2*86400+HH*3600+MM*60+SS                               
      RETURN                                                            
      END                                                               
      SUBROUTINE SKHMS(SECS,YD,HMS)                                     
C $ SUBROUTINE SKHMS(SECS, YD, HMS)  (JOE)                              
C $ CONVERT TIME FROM SECONDS SINCE 0Z ON 72001 TO DATE AND TIME        
C $ SECS = (I) INPUT  SECONDS FROM 0Z ON JAN 1, 1972                    
C $ YD = (I) OUTPUT  DATE, IN YYDDD FORMAT                              
C $ HMS = (I) OUTPUT  TIME OF DAY, IN HHMMSS FORMAT                     
C $$ SKHMS = CONVERT, DATE, TIME                                        
      IMPLICIT INTEGER (A-Z)                                            
C                                                                       
C                                                                       
      YD=72001                                                          
      HMS=0                                                             
      IF (SECS.LE.0) RETURN                                             
      DAYSEC=3600*24                                                    
      DAYS=SECS/DAYSEC                                                  
      YR4=4*365+1                                                       
      YY=DAYS/YR4*4                                                     
      DAYS4=MOD(DAYS,YR4)                                               
      IF (DAYS4.LT.366) DDD=DAYS4                                       
      IF (DAYS4.LT.366) GOTO 4                                          
      DAYS4=DAYS4-1                                                     
      YY=YY+DAYS4/365                                                   
      DDD=MOD(DAYS4,365)                                                
 4    YD=72001+YY*1000+DDD                                              
      REM=MOD(SECS,DAYSEC)                                              
      HMS=REM/3600*10000+MOD(REM,3600)/60*100+MOD(REM,60)               
      RETURN                                                            
      END                                                               
