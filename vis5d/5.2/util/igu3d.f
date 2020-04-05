      SUBROUTINE MAIN0
C
C igu3d program for managing 3D grid files
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
C ? IGU3D -- 3-D Grid file utility        (DAS)
C ?      IGU3D LIST bgridf egridf <keywords>
C *** ?      IGU3D SET (gridf)
C ?      IGU3D COPY sgridf dgridf (RENUMBER)
C ?      IGU3D MAKE gridf maxsiz
C *** ?      IGU3D DEL gridf-1 gridf-2
C ?      IGU3D DIR gridf date project
C ? Parameters:
C ?      gridf | grid file number
C ?      bgridf | beginning grid file number
C ?      egridf | ending grid file number
C ?      sgridf | source grid file number
C ?      dgridf | destination grid file number
C ?      date  | creation date of file
C ?      project | project number
C ?      maxsiz | maximum size for a 3-d grid (nr*nc*nl)
C *** ?      "comment | comment appended to grid file directory
C ? Keywords:
C ?     -PROJ   list grid files with project number(s)
C ?     -DAY    list grid files with this date, YYDDD
C * SSEC/MCIDAS USERS MANUAL - CHAP04
C
C
      IMPLICIT INTEGER(A,B,D-Z)
      IMPLICIT CHARACTER*12 (C)
      PARAMETER (MAXPTS=100000)
      CHARACTER*3 COPT
      CHARACTER*33 COUT
      CHARACTER*4 CLIT
      CHARACTER*8 FILNAM
      CHARACTER*8 NAME2
      INTEGER BUF(MAXPTS)
      INTEGER IDENT(13)
      INTEGER HEADR(64)
      CHARACTER*105 CLINE
      CHARACTER*50 WMSG
      DATA MAXGF/9999/
      DATA MAXSIZ/9000/
      DATA YES/1/,NO/0/
      DATA WMSG/'XMC200I ==============> GRID WARNING <============'/
C
      PTR=1
      COPT=CPP(PTR,' ')
      IF(COPT.EQ.'MOV'.OR.COPT.EQ.'COP') GOTO 50
      IF(COPT.EQ.'MAK') GO TO 100
      IF(COPT.EQ.'DIR') GO TO 150
      IF(COPT.EQ.'LIS') GO TO 200
      IF(COPT.EQ.'DEL') GO TO 250
      IF(COPT.EQ.'SET') GO TO 300
C-----INVALID FUNCTION
 40   CONTINUE
      CALL EDEST(' INVALID OPTION '//COPT,0)
      RETURN
C
C        ****** COPY
C
 50   CONTINUE
      SOURCE=IPP(PTR+1,0)
      DEST=IPP(PTR+2,SOURCE)
      IF (SOURCE.EQ.DEST) THEN
         CALL EDEST('SOURCE AND DESTINATION MUST BE DIFFERENT',0)
         CALL ABORT()
      ENDIF
      RENUMB=NO
      CNUMB=CPP(PTR+3,' ')
      IF(CNUMB(:3).EQ.'REN') RENUMB=YES
      IF (IGOP3D(SOURCE,FILNAM).NE.0) GOTO 901
      CALL LWI(FILNAM,0,10,IDENT)
      CALL LWI(FILNAM,10,1,MAXSIZ)
      IF (MAXSIZ.GT.MAXPTS) THEN
         CALL EDEST('FILE EXCEEDS THE MAXIMUM ALLOWABLE SIZE',0)
         GO TO 9999
      ENDIF
 55   I=IGMK3D(DEST,IDENT,MAXSIZ)
      IF (I.LT.0) GOTO 905
      IF (I.EQ.1) GOTO 910
      I=IGOP3D(DEST,NAME2)
      CALL LWO(NAME2,8,2,IDENT(9))
      K=0
      DO 80 J=1,MAXGF
         IF (IGGT3D(SOURCE,J,MAXSIZ,BUF,NR,NC,NL,HEADR).NE.0) GOTO 80
         IF (RENUMB.EQ.NO) K=-J
         IF (IGPT3D(DEST,K,BUF,NR,NC,NL,HEADR,KACT).NE.0) GOTO 915
         K=KACT+1
 80   CONTINUE
      CALL SDEST(' DONE COPYING TO 3-D GRIDFILE # ',DEST)
      GOTO 9999
C        ********    GEN
C
 100  CONTINUE
      MAXSIZ=IPP(3,MAXSIZ)
      COUT=' '
C     CALL CQFLD(COUT)
      DO 85 I=1,8
85    IDENT(I)=LIT('    ')
C     CALL MOVCW(COUT(2:),IDENT)
      IF (IGMK3D(IPP(PTR+1,0),IDENT,MAXSIZ).NE.-1) GOTO 999
      CALL EDEST(' CANT CREATE 3-D GRID FILE ',IPP(PTR+1,0))
      GOTO 9999
C
C         ******   DIR
C
 150  CONTINUE
      SOURCE=IPP(PTR+1,0)
      IF(IGOP3D(SOURCE,FILNAM).NE.0) GO TO 901
      COUT=' '
C     CALL CQFLD(COUT)
      DO 86 I=1,8
86    IDENT(I)=LIT('    ')
C     CALL MOVCW(COUT(2:),IDENT)
      CALL LWO(FILNAM,0,8,IDENT)
      CALL LWO(FILNAM,8,1,IPP(PTR+3,LUC(1)))
      CALL GETDAY(IDAY)
      CALL LWO(FILNAM,9,1,IPP(PTR+2,IDAY))
C     CALL LWCLOS(FILNAM)
      GO TO 999
C
C        ***********   LIST
C
 200  CONTINUE
      PTR=PTR+1
      LO=IPP(PTR,1)
      HI=IPP(PTR+1,LO)
      IPLO=IKWP('PRO',1,0)
      IPHI=IKWP('PRO',2,IPLO)
      IF(IPHI.EQ.0) IPHI=999999
      IDLO=IKWP('DAY',1,0)
      IDHI=IKWP('DAY',2,IDLO)
      IF(IDHI.EQ.0) IDHI=999999
      BEGGF=1
      ENDGF=MAXGF
      BEGGF=MIN0(LO,MAXGF)
      ENDGF=MIN0(MAX0(HI,BEGGF),MAXGF)
      CALL SDEST(' 3-D GRIDFILE PROJ CREATED MAXSIZ MAXNUM IDENT',0)
      CALL SDEST(' ------------ ---- ------- ------ ------ ------'//
     *'-----------------',0)
      DO 240 J=BEGGF,ENDGF
         IF (IGOP3D(J,FILNAM).NE.0) GOTO 240
         CALL LWI(FILNAM,0,13,IDENT)
         PROJ=IDENT(9)
         DATE=IDENT(10)
         MAXSIZ=IDENT(11)
         MAXNUM=IDENT(12)
         IF(PROJ.LT.IPLO.OR.PROJ.GT.IPHI) GO TO 239
         IF(DATE.LT.IDLO.OR.DATE.GT.IDHI) GO TO 239
 90      FORMAT(I10,2I8,2I7,1X,8A4)
         WRITE (CLINE,90) J,PROJ,DATE,MAXSIZ,MAXNUM,(IDENT(I),I=1,8)
         CALL SDEST(CLINE,0)
 239     CONTINUE
 240     CONTINUE
C240     CALL LWCLOS(FILNAM)
      CALL SDEST('  ---END OF LISTING',0)
      GOTO 9999
C
C        ********    DEL
C
250   CONTINUE
      J=IPP(PTR+1,1)
      L=IPP(PTR+2,J)
C CHECK FOR TOO MANY FILES
      IF(L-J.GT.15) THEN
         CALL SDEST(WMSG,0)
         CC2=CFI(LUC(-16))
         CC3=CFI(LUC(0))
         CC4=CFI(J)
         CC5=CFI(L)
         WMSG='XMC200I '//CLIT(LUC(-17))//CC2(8:)//CC3(10:)//
     *   ' FILES='//CC4(8:)//CC5(8:)
         CALL SDEST(WMSG,0)
      ENDIF
      DO 275 I=J,L
      IF(IGOP3D(I,FILNAM).NE.0) GO TO 275
      CALL IGQT3D(I)
      CALL SDEST(' IGU3D DONE #',I)
 275  CONTINUE
      GOTO 999
C
C        ********    SET
C
 300  IVAL=IGCF3D(0)
      ICUR=IGCF3D(IPP(PTR+1,IVAL))
      CALL SDEST(' CURRENT 3-D GRID FILE IS ',ICUR)
      CALL SDEST('                     ,WAS ',IVAL)
      GOTO 9999
C
C        ********    ERROR MESSAGES
C
 901  CALL EDEST(' CANT OPEN 3-D GRID FILE ',SOURCE)
      GOTO 9999
 905  CALL EDEST(' CANT CREATE 3-D GRID FILE ',DEST)
      GOTO 9999
 910  CALL EDEST(' MUST FIRST QUIT DESTINATION 3-D GRID FILE  ',DEST)
      GOTO 9999
 915  CALL EDEST(' 3-D GRIDFILE FULL, # ',DEST)
      GOTO 9999
C   PRINT DONE MESSAGE
999   CALL EDEST('DONE',0)
 9999 CONTINUE
      RETURN
      END
