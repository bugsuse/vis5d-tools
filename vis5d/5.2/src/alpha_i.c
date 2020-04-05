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
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "netcdf.h"

#define SUCCESS 1
#define FAILURE 0
#define IOERROR -1

/*
Known Restrictions
------------
	(1) The netCDF library must exist on the platform of execution
	    (or at least the platform where compiles take place for the
	    platform of execution) 

	(2) The external variable from the globdef.c netCDF source file
	    should be set in this routine 

			from

		ncopts = (NC_FATAL | NC_VERBOSE);

			to

		ncopts = (NC_VERBOSE);

	     This allows AVS to attempt to open a file that is not a netCDF
	     file, resulting in a UNIX return with a message rather than the
	     default UNIX exit!  With the default exit, this AVS module will
	     die if a user selects a file name with the AVS file browser that 
	     is NOT a netCDF file. 

*/

/* alpha-prototype netCDF files always convert to GMT; will this change? */

#define STD_EXT "GMT"

static int ndims;		/* # dimensions in netCDF file */
static int nvars;		/* # variables in netCDF file */
static int ngatts;		/* # global attributes in netCDF file */
static int recdim;		/* id of unlimited dimension in netCDF file */
static int nvardims;		/* # dims for single variable in netCDF file */
static int nvaratts;		/* # atts for single variable in netCDF file */
static long dimsize;		/* size of an individual dimension */

static long sdate;		/* start date */
static long stime;		/* start time */
static long tstep;		/* time step increment */

static nc_type datatype;	/* netCDF data type */

static char dimname[32];	/* name of netCDF dimension */
static char varname[32];	/* name of netCDF variable */
static int vardim[32];		/* dimensions of netCDF variables */
static long start[32];		/* starting positions for extracting data */
static long count[32];		/* number of each dimension to extract */

#define MAXSPEC 32 

static int alphancol;			/* # columns in data grid */
static int alphanrow;			/* # rows in data grid */
static int alphanlayer;			/* # levels (layers) in grid */
static int alphanrecord;		/* # records in file */
static int alphanspecies;		/* # species in the input file */
static int alphafilesize;		/* file size in bytes */
static char alphaspecname[MAXSPEC*16+1];/* short name of all species */ 

/*******************************************************************/
/* alpha_open                                                      */
/* Function: open input file and determine filesize and filetype   */
/* On Error: return FAILURE and write Error string into message    */
/* If no Error: return SUCCESS                                     */
/*******************************************************************/
int alpha_open(fd, filename, message)
int *fd;				/* INPUT:  file descriptor */
char *filename;				/* INPUT:  file name */
char *message;				/* OUTPUT: returned Error message */
{
struct stat statbuf;			/* UNIX file status buffer */
register int istat;			/* I/O status falpha UNIX stat call */
register int status;			/* alpha_open function return status */
static char buffer[32];

status = FAILURE;			/* assume FAILURE */

ncopts = (NC_VERBOSE);
if ((*fd = open(filename, O_RDONLY)) != IOERROR)
	{
	if ((istat = stat(filename, &statbuf)) != IOERROR)
		{
		alphafilesize = statbuf.st_size;
		if (alphafilesize > 0)
			{
			if (read(*fd, buffer, 3) == 3)
				{
				if ((
					(buffer[0] == 'C') &&
	    				(buffer[1] == 'D') && 
	    				(buffer[2] == 'F')) )
					{
					close(*fd);
					if ((*fd = ncopen(filename,
						NC_NOWRITE)) != NC_SYSERR)
						{
						status = SUCCESS;
						}
					else
						{
						sprintf(message, "%s",
					 "Cannot access as netCDF file.");
						}
					}
				else
					{
					sprintf(message, "%s",
					 "File is not recognized as netCDF.");
					close(*fd);
					}
				}
			else
				{
				sprintf(message, "%s", 
				 "Cannot read first 3 characters of file.");
				close(*fd);
				}
			}
		else
			{
			sprintf(message, "%s", "File size of zero");
			close(*fd);
			}
		}
	else
		{
		sprintf(message, "%s", "Cannot get file status");
		close(*fd);
		}
	}

return(status);
}

int alpha_inquire(fd, message, ncol, nrow, nlayer, nrecord, nspecies, specname)
int fd;
char *message;
int *ncol;
int *nrow;
int *nlayer;
int *nrecord;
int *nspecies;
char *specname;
{
register int i, j;

if ((ncinquire(fd, &ndims, &nvars, &ngatts, &recdim)) == NC_SYSERR)
	{
	sprintf(message, "%s", "Error calling ncinquire");
	return(FAILURE);
	}
for (i = 0; i < ndims; ++i)
	{
	if ((ncdiminq(fd, i, dimname, &dimsize)) == NC_SYSERR)
		{
		sprintf(message, "%s", "Error calling ncdiminq");
		return(FAILURE);
		}
	if (!strcmp(dimname, "TSTEP"))
		alphanrecord = dimsize;
	else if (!strcmp(dimname, "LAY"))
		alphanlayer = dimsize;
	else if (!strcmp(dimname, "ROW"))
		alphanrow = dimsize;
	else if (!strcmp(dimname, "COL"))
		alphancol = dimsize;
	}
if ((alphanrecord == 0) || (alphanlayer == 0) || 
	(alphanrow == 0) || (alphancol == 0))
	{
	sprintf(message, "%s",
	"Unexpected dimensions! Expecting ROW-COL-LAYER-TIME grid values!\n");
	return(FAILURE);
	}
j = 0;
alphanspecies = 0;
for (i = 0; i < nvars; ++i)
	{
	if ((ncvarinq(fd, i, varname, &datatype, &nvardims,
		vardim, &nvaratts)) == NC_SYSERR)
		{
		sprintf(message, "%s", "Error calling ncvarinq");
		return(FAILURE);
		}
	if (strcmp(varname, "TFLAG"))	/* ignore TFLAG variables */
		{
		sprintf(alphaspecname+j, "%s:", varname);
		j = strlen(alphaspecname);
		++alphanspecies;
		}
	}
if (j >= 0)
	--j;
alphaspecname[j] = '\0';

*ncol = alphancol;
*nrow = alphanrow;
*nlayer = alphanlayer;
*nrecord = alphanrecord;
*nspecies = alphanspecies;
sprintf(specname, "%s", alphaspecname);

return(SUCCESS);
}
	
int alpha_get_date(fd, record_no, datestr, message)
int fd;
int record_no;
char *datestr;
char *message;
{
int year;
int day;
int hour;
int min;
int sec;

if
	(
	((ncattget(fd, NC_GLOBAL, "SDATE", &sdate)) == NC_SYSERR) ||
	((ncattget(fd, NC_GLOBAL, "STIME", &stime)) == NC_SYSERR) ||
	((ncattget(fd, NC_GLOBAL, "TSTEP", &tstep)) == NC_SYSERR) 
	)
	{
	sprintf(message, "%s", "Error calling ncattget");
	return(FAILURE);
	}

netcdf2julian(record_no, sdate, stime, tstep, &year, &day, &hour, &min, &sec);
julian2std(year, day, hour, min, sec, STD_EXT, datestr);
return(SUCCESS);
}

int alpha_get_data(fd, record_no, species_no, databuf, species, units, message)
int fd;
int record_no;
int species_no;
float *databuf;
char *species;
char *units;
char *message;
{
register int i, j;

count[0] = 1;
count[1] = alphanlayer;
count[2] = alphanrow;
count[3] = alphancol;
start[0] = record_no - 1;
start[1] = start[2] = start[3] = 0;

if ((i = ncvarid(fd, species)) == NC_SYSERR)
	{
	sprintf(message, "%s", "Error calling ncvarid");
	return(FAILURE);
	}
if ((ncvarget(fd, i, start, count, (void *) (databuf))) == NC_SYSERR)
	{
	sprintf(message, "%s", "Error calling ncvarget");
	return(FAILURE);
	}
if ((ncattget(fd, i, "units", units)) == NC_SYSERR)
	{
	sprintf(message, "%s",  "Error calling ncattget");
	return(FAILURE);
	}

	/* strip trailing blanks from units string */

j = strlen(units);
for (i = j-1; i >= 0; --i)
	{
	if (units[i] == ' ')
		units[i] = '\0';
	else
		break;
	}
 
/* The AVS read-field module will not work if the UNITS string has 
   characters other than: a-z, A-Z, 0-9, dash, underscore, blank space,
   period, forward slash

   This bug has been reported to AVS.  Meanwhile, characters that
   cause this problem in the UNITS string will be replaced with
   blanks.
*/
 
j = strlen(units);
for (i = 0; i < j; ++i)
	if ( !		/* if not an AVS read-field approved char */
		(
			((units[i] >= 'a') && (units[i] <= 'z')) ||
			((units[i] >= 'A') && (units[i] <= 'Z')) ||
			((units[i] >= '0') && (units[i] <= '9')) ||
			(units[i] == '-') || 
			(units[i] == '_') ||
			(units[i] == ' ') || 
			(units[i] == '.') ||
			(units[i] == '/')
		)
		)
		units[i] = ' ';		/* substitute a blank */

return(SUCCESS);
}
