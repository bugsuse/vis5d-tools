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

