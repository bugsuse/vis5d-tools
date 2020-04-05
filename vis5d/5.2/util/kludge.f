C kludge.f

C VIS-5D version 4.0

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


        FUNCTION IC(IBUF,IBYTE)
cc        INTEGER*1 IBUF(*)
        CHARACTER*1 IBUF(*)
C       MBYTE=4-MOD(IBYTE,4)+IBYTE/4*4
        MBYTE=IBYTE+1
cc        IC=IBUF(MBYTE)
        IC = ICHAR( IBUF(MBYTE) )
        RETURN
        END
C
        SUBROUTINE STC(IVAL,IBUF,IBYTE)
cc        INTEGER*1 IBUF(*),JVAL
        CHARACTER*1 IBUF(*)
        JVAL=IVAL
C       MBYTE=4-MOD(IBYTE,4)+IBYTE/4*4
        MBYTE=IBYTE+1
cc        IBUF(MBYTE)=JVAL
        IBUF(MBYTE) = CHAR(JVAL)
        RETURN
        END
C
C      SUBROUTINE GETTIM(I)
C      INTEGER IAR(3)
C      CALL ITIME(IAR)
C      I=IAR(1)*10000+IAR(2)*100+IAR(3)
C      RETURN
C      END
C
      SUBROUTINE GETDAY(I)
      IMPLICIT INTEGER (A-Z)
      INTEGER MTBL(12)
      DATA MTBL/0,31,59,90,120,151,181,212,243,273,304,334/
C
      CALL IDATE(MON,IDAY,IYEAR)
      IF (IDAY.LT.1.OR.IDAY.GT.31) RETURN
      IF (MON.LT.1.OR.MON.GT.12) RETURN
      IDDD=IDAY+MTBL(MON)
      IYY=MOD(IYEAR,100)
      IF (MOD(IYY,4).EQ.0.AND.IYY.NE.0.AND.MON.GT.2) IDDD=IDDD+1
      I=IYY*1000+IDDD
      RETURN
      END
C
        FUNCTION LWFILE(C)
        CHARACTER*(*) C
        LWFILE=1
        ISTAT=LWI(C,0,1,IVAL)
        IF(ISTAT.EQ.-1) LWFILE=0
        RETURN
        END
C
        FUNCTION ISCHAR(I)
cc        INTEGER*1 I(*)
        CHARACTER*1 I(*)
        ISCHAR=0
        DO 1 L=1,4
cc        IF(I(L).LT.'20'X .OR. I(L).GT. '7F'X) RETURN
           K = ICHAR( I(L) )
           IF( K .LT. '20'X .OR. K .GT. '7F'X) RETURN
 1      CONTINUE
        ISCHAR=1
        RETURN
        END
C 
        SUBROUTINE SFRAME(I)
        RETURN
        END
C
        INTEGER FUNCTION SQW(C,I,J)
        CHARACTER*(*) C
        CHARACTER*20 CTEXT
        CTEXT=C
        CALL SDEST(CTEXT,0)
        SQW=0
        RETURN
        END
C
      FUNCTION DLIT(C)
C $    FUNCTION DLIT(CC)   (DAS)
C $    DLIT  --  CHANGE CHARACTER*8 INTO INTEGER*8
C $    INPUT:
C $        CC  (C)  CHARACTER STRING
C $    FUNCTION VALUE:
C $        THE SAME BYTES, CHANGED TO TYPE REAL*8
       CHARACTER*(*) C
       CHARACTER*8 C1
       REAL*8 D,DLIT
       C1=C
       CALL MOVCW(C1,D)
       DLIT=D
       RETURN
       END

      FUNCTION ALIT(C)
C $    FUNCTION ALIT(CC)   (DAS)
C $    ALIT  --  CHANGE CHARACTER*4 INTO INTEGER*4
C $    INPUT:
C $        CC  (C)  CHARACTER STRING
C $    FUNCTION VALUE:
C $        THE SAME BYTES, CHANGED TO TYPE REAL*4
       CHARACTER*(*) C
       CHARACTER*4 C1
       C1=C
       CALL MOVCW(C1,A)
       ALIT=A
       RETURN
       END


      FUNCTION NDIGS(IVAL)
C $ NDIGS(IVAL)
C $ COUNTS AND RETURNS NUMBER OF DIGITS IN INTEGER IVAL
C $ IVAL = (I) INPUT  INTEGER
C $$ NDIGS = INTEGER
      NUM=0
      IF(IVAL.LT.0) NUM=NUM+1
      ITES=IABS(IVAL)
100   CONTINUE
      ITES=ITES/10
      NUM=NUM+1
      IF(ITES.NE.0) GO TO 100
      NDIGS=NUM
      RETURN
      END
C
        SUBROUTINE SPOUT(C)
        CHARACTER*(*) C
        CALL SDEST(C,0)
        RETURN
        END
C
        FUNCTION NUSER(I)
        I=LIT('STEL')
        NUSER=I
        RETURN
        END
C
        FUNCTION NPROJ(I)
        I=9999
        NPROJ=9999
        RETURN
        END
C
        FUNCTION LWCLOS(C)
        CHARACTER*(*) C
        CALL SDEST('LWCLOS NO-OP',0)
        LWCLOS=0
        RETURN
        END
C
        FUNCTION LWC(C)
        CHARACTER*(*) C
        CALL SDEST('LWC NO-OP',0)
        LWC=0
        RETURN
        END
C
        FUNCTION LWD(C)
        CHARACTER*(*) C
        CALL SDEST('LWD NO-OP',0)
        LWD=0
        RETURN
        END
C
        SUBROUTINE LWMOP
        RETURN        
        END
C
        SUBROUTINE TRMNL(I)
        I=1
        RETURN
        END
C
        FUNCTION KB1INI(CIN,COUT,IOPT)
        CHARACTER*(*) CIN,COUT
        INTEGER IOPT(*)
        KB1INI=-1
        RETURN
        END
C
        FUNCTION KB2INI(CIN,COUT,IOPT)
        CHARACTER*(*) CIN,COUT
        INTEGER IOPT(*)
        KB2INI=-1
        RETURN
        END
C
        FUNCTION KB3INI(CIN,COUT,IOPT)
        CHARACTER*(*) CIN,COUT
        INTEGER IOPT(*)
        KB3INI=-1
        RETURN
        END
C
        FUNCTION KB1CAL(PFX,IDIR,NVAL,IBAND,IBUF)
        INTEGER PFX(*),IDIR(*),IBUF(*)
        KB1CAL=-1
        RETURN
        END
C
        FUNCTION KB2CAL(PFX,IDIR,NVAL,IBAND,IBUF)
        INTEGER PFX(*),IDIR(*),IBUF(*)
        KB2CAL=-1
        RETURN
        END
C
        FUNCTION KB3CAL(PFX,IDIR,NVAL,IBAND,IBUF)
        INTEGER PFX(*),IDIR(*),IBUF(*)
        KB3CAL=-1
        RETURN
        END
C
        FUNCTION KBPREP(ISLOT,IPARM)
        INTEGER IPARM(*)
        KBPREP=-1
        RETURN
        END
C
        SUBROUTINE SFCLOC(CSTN,IX,IY)
        CHARACTER*(*) CSTN
        RETURN
        END
C
        FUNCTION ICGET(I,J)
        ICGET=-1
        RETURN
	END
