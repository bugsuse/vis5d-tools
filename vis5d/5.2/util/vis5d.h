C vis5d.h

C This file is used by a few of the Fortran utility files.


C Constants/Limits for VIS-5D Version 3.0
C We've increases the number of times steps to 400
C and the number of parameters to 30.


C Warning:  Changing these values may cause side-effects
C           including incompatibility with old data sets.


C GRID FILE NUMBER
C ?   PARAMETER (IGRIDF=1,ITRAG=1,LENSEC=15*60)

C NUMBER OF TIME STEPS, PARAMS AND RAW GRID FILES
      PARAMETER (NTIME=400,NPARM=30,NFILE=20)


