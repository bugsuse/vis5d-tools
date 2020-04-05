      SUBROUTINE DOUBT(C,N,L)
C
C VIS-5D version 3.4
C
C vis5d program for visualizing five dimensional gridded data sets
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
C $ SUBROUTINE DOUBT(C,N,L) (WLH)
C $ DOUBT WRITES THE CONTENTS OF AN ARRAY TO A DEBUGGING LOG FILE
C $ C = (C) INPUT LABEL FOR ARRAY DATA
C $ N = (I) INPUT LENGTH OF ARRAY
C $ L = (I) INPUT ARRAY OF INTEGERS
C $$ DOUBT = PRINT
      CHARACTER C*(*)
      CHARACTER*100 CS
      CHARACTER*12 CFI,CA
      INTEGER N,L(*)
      IF(C .EQ. 'NOPRINT') RETURN
      CALL LDEST(C,N)
      IF(N .LE. 0) RETURN
      J=0
1     I=J+1
      J=I+7
      IF(J .GT. N) J=N
      CS=' '
      M=1
      DO 2 K=I,J
      CA=CFI(L(K))
      IF(L(K) .EQ. 0) CA='           0'
      MM=M+10
      CS(1:MM)=CS(1:M)//CA(3:12)
2     M=MM
      CALL LDEST(CS,0)
      IF(J .LT. N) GO TO 1
      RETURN
      END
C
      SUBROUTINE ROUBT(C,N,Z)
C $ SUBROUTINE ROUBT(C,N,Z) (WLH)
C $ ROUBT WRITES THE CONTENTS OF AN ARRAY TO A DEBUGGING LOG FILE
C $ C = (C) INPUT LABEL FOR ARRAY DATA
C $ N = (I) INPUT LENGTH OF ARRAY
C $ Z = (R) INPUT ARRAY OF REALS
C $$ ROUBT = PRINT
      REAL*4 Z(*)
      CHARACTER C*(*)
      CHARACTER*100 CS
      CHARACTER*12 CFI,CA
      INTEGER N
      IF(C .EQ. 'NOPRINT') RETURN
      CALL LDEST(C,N)
      J=0
1     I=J+1
      J=I+7
      IF(J .GT. N) J=N
      CS=' '
      M=1
      DO 2 K=I,J
      IZ=100.0*Z(K)
      CA=CFI(IZ)
      IF(IZ .EQ. 0) CA='           0'
      MM=M+10
      CS(1:MM)=CS(1:M)//CA(3:12)
2     M=MM
      CALL LDEST(CS,0)
      IF(J .LT. N) GO TO 1
      RETURN
      END
C
      SUBROUTINE SCOUT(C,P,Q,R,S,T,U)
C $ SUBROUTINE SCOUT(C,P,Q,R,S,T,U) (WLH)
C $ DOUBT WRITES SIX INTEGERS TO A DEBUGGING LOG FILE
C $ C = (C) INPUT LABEL FOR INTEGERS
C $ P = (I) INPUT INTEGER TO WRITE
C $ Q = (I) INPUT INTEGER TO WRITE
C $ R = (I) INPUT INTEGER TO WRITE
C $ S = (I) INPUT INTEGER TO WRITE
C $ T = (I) INPUT INTEGER TO WRITE
C $ U = (I) INPUT INTEGER TO WRITE
C $$ SCOUT = PRINT
      CHARACTER C*(*)
      CHARACTER*12 CFI,C1,C2,C3,C4,C5,C6
      CHARACTER*8 CC
      INTEGER P,Q,R,S,T,U
      IF(C .EQ. 'NOPRINT') RETURN
      CC=C
      C1=CFI(P)
      C2=CFI(Q)
      C3=CFI(R)
      C4=CFI(S)
      C5=CFI(T)
      C6=CFI(U)
      CALL LDEST(CC//C1(4:12)//C2(4:12)//C3(4:12)//C4(4:12)//C5(4:12)//
     *C6(4:12),0)
      RETURN
      END
C
      SUBROUTINE ROUT(C,P,Q,R,S,T,U)
C $ SUBROUTINE ROUT(C,P,Q,R,S,T,U) (WLH)
C $ DOUBT WRITES SIX REALS TO A DEBUGGING LOG FILE
C $ C = (C) INPUT LABEL FOR REALS
C $ P = (R) INPUT REAL TO WRITE
C $ Q = (R) INPUT REAL TO WRITE
C $ R = (R) INPUT REAL TO WRITE
C $ S = (R) INPUT REAL TO WRITE
C $ T = (R) INPUT REAL TO WRITE
C $ U = (R) INPUT REAL TO WRITE
C $$ ROUT = PRINT
      CHARACTER C*(*)
      CHARACTER*12 CFF,C1,C2,C3,C4,C5,C6
      CHARACTER*8 CC
      REAL*8 DBLE
      REAL*4 P,Q,R,S,T,U
      IF(C .EQ. 'NOPRINT') RETURN
      CC=C
      C1=CFF(DBLE(P),2)
      C2=CFF(DBLE(Q),2)
      C3=CFF(DBLE(R),2)
      C4=CFF(DBLE(S),2)
      C5=CFF(DBLE(T),2)
      C6=CFF(DBLE(U),2)
      CALL LDEST(CC//C1(4:12)//C2(4:12)//C3(4:12)//C4(4:12)//C5(4:12)//
     *C6(4:12),0)
      RETURN
      END
C
      SUBROUTINE LDEST(C,II)
C $ SUBROUTINE LDEST(C,II) (WLH)
C $ LDEST WRITES TEXT AND AN INTEGER TO A DEBUGGING LOG FILE
C $ C = (C) INPUT TEXT MESSAGE TO WRITE
C $ II = (I) INPUT INTEGER TO WRITE
C $$ LDEST = PRINT
      CHARACTER C*(*)
      DATA IFLAG/0/
      IF(IFLAG .LT. 0) RETURN
      IF(IFLAG .GT. 0) GO TO 20
      IF(NKWP('DEBUG') .GT. 0) GO TO 10
      IFLAG=-1
      RETURN
10    IFLAG=1
20    CALL CDEST(C,II)
      RETURN
      END
