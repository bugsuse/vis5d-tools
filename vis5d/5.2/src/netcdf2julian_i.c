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

netcdf2julian(record, sdate, stime, tstep, year, day, hour, min, sec)
int record;			/* record number from the data file */
int sdate;			/* Julian start date for record #1 */
int stime;			/* Julian start time for record #1 */
int tstep;			/* time step increment for each record */
int *year;
int *day;
int *hour;
int *min;
int *sec;
{

int time;

*year = sdate/1000;			/* extract year from Julian date */
*day = sdate - (*year * 1000);		/* extract day of year from Julian */

time = stime + (record - 1) * tstep;	/* get time of current record */ 

*hour = time/10000;
*min = (time - (*hour * 10000))/100;
*sec = time - (*hour * 10000) - (*min * 100);
}

