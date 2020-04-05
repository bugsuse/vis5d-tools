void gsdate (int day, int mon, int year, long *serial)
{
/* FROM VRDATEGS                                                                 
  DAY 1 JANUARY 1900 IS SERIAL DAY 0                                            
      PURPOSE :                                                                 
         THIS SUBROUTINE CAN BE USED FOR TWO PURPOSES :                         
      (1)  CONVERT A DATE FROM GREGORIAN TO A SERIAL FORM.                      
         ENTRY POINT 'GSDATE' INVOKED AS FOLLOWS:                               
            CALL DATEGS  (DAY,MONTH,YEAR,SERIAL)           (FORTRAN)            
         ALL ARGUMENTS ARE FULL/WORD INTEGERS. 'YEAR' SHOULD BE                 
      SPECIFIED RELATIVE TO 1900 (EG. -60 IS 1840)                              
         ENTRY POINT 'DATESG' INVOKED AS FOLLOWS:                               
      (2)  CONVERT FROM THE SERIAL FORM TO  THE GREGORIAN DATE.                 
            CALL DATESG  (DAY,MONTH,YEAR,SERIAL)           (FORTRAN)            
         ALL ARGUMENTS ARE FULL/WORD INTEGERS. 'YEAR' RETURNED RELATIVE         
      TO 1900 (EG. -10 IS YEAR 1890)                                            
      REMARKS :
    (1)  1/1/00   IS TAKEN AS DAY 0.
    (2)  SUBROUTINE IS VALID FROM  1/3/1800 TO 28/2/2100                        
    (3)  ADAPTED FROM ALGORITHM 199  CACM */
 int i;
 year = year + 100;
 if (mon > 2) {mon = mon - 3;}
         else {mon = mon + 9; year = year - 1;}
 year = year * 1461;
 year = year / 4;
 mon = mon * 153;
 mon = mon + 2;
 mon = mon / 5;
 i = year + mon + day;
 i = i - 36466;
 if (i > 59) i = i - 1;
 *serial = i;
 return;
}
void sgdate (int *day, int *mon, int *year, long serial)
{
 int j;
 if (serial >= 59) serial = serial + 1;
 serial = serial + 36466;
 serial = serial * 4;
 serial = serial - 1;
 *year = serial / 1461;
 j = serial % 1461;
 if (j < 0) {j = j + 1461; *year = *year - 1;}
 j = j / 4;
 j = j * 5;
 j = j + 2;
 *mon = j / 153;
 *day = j % 153;
 *day = *day / 5 + 1;
      if (*mon < 10) {*mon = *mon + 3;}
                else {*mon = *mon - 9; *year = *year + 1;}                         
 *year = *year - 100;
 return;                                                                    
}
