#include <stdio.h>
int dyofyr (int day, long imon, int year)
{
 int nday[12] = {0,31,59,90,120,151,181,212,243,273,304,334};
 int lday[12] = {0,31,60,91,121,152,182,213,244,274,305,335};
 long mon[12]  = {'JAN ','FEB ','MAR ','APR ','MAY ','JUN ',
            'JUL ','AUG ','SEP ','OCT ','NOV ','DEC '};
 int date = 0;
 int i;
 if (year > 1900) {year = year - 1900;}
 if ((imon < 1) || (imon >12))
  {for (i=0; i<12; i++) {if(imon==mon[i]) {imon = i+1;break;} } }
 imon--; /* input imon is (1 - 12) for internal use make it (0 - 11) */
 if (imon < 0 || imon >11) {imon=11;}
 if (year%4 == 0) {date = lday[imon];} else {date = nday[imon];}
 date = date + day + year*1000;
 return date;
}
