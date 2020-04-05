/* Vis5D version 5.2 */

/*
Vis5D system for visualizing five dimensional gridded data sets
Copyright (C) 1990 - 2000 Bill Hibbard, Johan Kellum, Brian Paul,
Dave Santek, and Andre Battaiola.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include <stdio.h>

mm4date2std(inputyear, inputmonth, inputday, inputhour, inputmin, inputsec, 
std_ext, str)
int inputyear, inputmonth, inputday, inputhour, inputmin, inputsec;
char *std_ext;
char *str;
{
int year, month, day, hour, min, sec;
static int days_per_month[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
int days_per_year;
register int i, j, k, found;

year = inputyear;
month = inputmonth;
day = inputday;
hour = inputhour;
min = inputmin;
sec = inputsec;

/* From Kernighan & Ritchie, p. 37
"... a year is a leap year if it is divisible by 4 but not by 400,
     except that years divisible by 400 ARE leap years."
*/

#define CHECK_LEAP_YEAR \
days_per_month[1] = 28; \
if ((!(year % 4)) && (year % 100))	/* leap year? */ \
	days_per_month[1] = 29; \
if (!(year % 400)) \
	days_per_month[1] = 29;

CHECK_LEAP_YEAR
	
while (hour >= 24)			/* catch day rollover */
	{
	++day;
	hour -= 24;
	if (day >= days_per_month[month-1])	/* catch year rollover */
		{
		if (month < 12)
			++month;
		else
			{
			month = 1;
			++year;
			CHECK_LEAP_YEAR
			}
		day = 1; 
		}
	}

sprintf(str, "%2d/%2d/%2d  %2d:%02d:%02d  %s",
		month, day, year - 1900, hour, min, sec, std_ext);
}

