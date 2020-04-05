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
#include "model_i.h"

extern int model_type;

#define SUCCESS 1
#define FAILURE 0

/*******************************************************************/
/* model_open                                                      */
/* Function: open input file and determine filesize and filetype   */
/* On error: return FAILURE and write error string into message    */
/* If no error: return SUCCESS                                     */
/*******************************************************************/
int model_open(fd, filename, message)
int *fd;				/* INPUT:  file descriptor */
char *filename;				/* INPUT:  file name */
char *message;				/* OUTPUT: returned error message */
{
register int status;

status = SUCCESS;

/* 
Since the file is opened and closed for EACH time step (because the module
may not be executed again), reduce testing for file type by first trying out
the value of model_type to see if it is the same as the previous call.
*/


switch(model_type)
	{
	case UNKNOWN:
		{
		status = FAILURE;
		break;
		}
	case UAM_IV: 
		{
		if (!uam_open(fd, filename, message))
			status = FAILURE;
		break;
		}
	case UAM_AREAL:
		{
		if (!areal_open(fd, filename, message))
			status = FAILURE;
		break;
		}
	case ROM:
		{
		if (!rom_open(fd, filename, message))
			status = FAILURE;
		break;
		}
	case RADM:
		{
		if (!radm_open(fd, filename, message))
			status = FAILURE;
		break;
		}
	case RADM_EMIS_LOW:
		{
		if (!radm_emis_open(fd, filename, message))
			status = FAILURE;
		break;
		}
	case ALPHA_PROTOTYPE:
		{
		if (!alpha_open(fd, filename, message))
			status = FAILURE;
		break;
		}
	case MM4:
		{
		if (!mm4_open(fd, filename, message))
			status = FAILURE;
		break;
		}
	}

/* The model type is still the same, so return success. */

if ((status == SUCCESS) && (model_type != UNKNOWN))
	return(status);

/* The model type has changed -- check to see if new type is valid. */

status = SUCCESS;
if (uam_open(fd, filename, message))
	model_type = UAM_IV;
else if (areal_open(fd, filename, message))
	model_type = UAM_AREAL;
else if (rom_open(fd, filename, message))
	model_type = ROM;
else if (radm_open(fd, filename, message))
	model_type = RADM;
else if (radm_emis_open(fd, filename, message))
	model_type = RADM_EMIS_LOW;
else if (alpha_open(fd, filename, message))
	model_type = ALPHA_PROTOTYPE;
else if (mm4_open(fd, filename, message))
	model_type = MM4;
else
	{
	model_type = UNKNOWN;
	sprintf(message, "Unknown model type.");
	status = FAILURE;
	}
return(status);
}


int model_inquire(fd, message, ncol, nrow, nlayer, nrecord, nspecies, specname)
int fd;
char *message;
int *ncol;
int *nrow;
int *nlayer;
int *nrecord;
int *nspecies;
char *specname;
{
switch(model_type)
	{
	case UAM_IV: 
		{
		return(uam_inquire(fd, message, ncol, nrow, nlayer,
			nrecord, nspecies, specname));
		}
	case UAM_AREAL:
		{
		return(areal_inquire(fd, message, ncol, nrow, nlayer,
			nrecord, nspecies, specname));
		}
	case ROM:
		{
		return(rom_inquire(fd, message, ncol, nrow, nlayer,
			nrecord, nspecies, specname));
		}
	case RADM:
		{
		return(radm_inquire(fd, message, ncol, nrow, nlayer,
			nrecord, nspecies, specname));
		}
	case RADM_EMIS_LOW:
		{
		return(radm_emis_inquire(fd, message, ncol, nrow, nlayer,
			nrecord, nspecies, specname));
		}
	case ALPHA_PROTOTYPE:
		{
		return(alpha_inquire(fd, message, ncol, nrow, nlayer,
			nrecord, nspecies, specname));
		}
	case MM4:
		{
		return(mm4_inquire(fd, message, ncol, nrow, nlayer,
			nrecord, nspecies, specname));
		}
	case UNKNOWN: 
		{
		sprintf(message, "Unknown model type."); 
		return(FAILURE); 
		}
	}
}

/*********************************************************************/
/* MODEL  Header Routine                                             */
/*********************************************************************/
int model_header(fd, message)
int fd;
char *message;		/* OUTPUT: return error string */
{
}

int model_get_date(fd, record_no, datestr, message)
int fd;
int record_no;
char *datestr;
char *message;
{
switch(model_type)
	{
	case UAM_IV:
		{
		return(uam_get_date(fd, record_no, datestr, message));
		}
	case UAM_AREAL:
		{
		return(areal_get_date(fd, record_no, datestr, message)); 
		}
	case ROM:
		{
		return(rom_get_date(fd, record_no, datestr, message)); 
		}
	case RADM:
		{
		return(radm_get_date(fd, record_no, datestr, message)); 
		}
	case RADM_EMIS_LOW:
		{
		return(radm_emis_get_date(fd, record_no, datestr, message)); 
		}
	case ALPHA_PROTOTYPE:
		{
		return(alpha_get_date(fd, record_no, datestr, message)); 
		}
	case MM4:
		{
		return(mm4_get_date(fd, record_no, datestr, message)); 
		}
	case UNKNOWN:
		{
		sprintf(message, "Unknown model type.");
		return (FAILURE);
		}
	}
}

int model_get_data(fd, record_no, species_no, databuf, species, units, message)
int fd;
int record_no;
int species_no;
float *databuf;
char *species;
char *units;
char *message;
{

switch(model_type)
	{
	case UAM_IV:
		{
		return(uam_get_data
		(fd, record_no, species_no, databuf, species, units, message));
		}
	case UAM_AREAL:
		{
		return(areal_get_data
		(fd, record_no, species_no, databuf, species, units, message));
		}
	case ROM:
		{
		return(rom_get_data
		(fd, record_no, species_no, databuf, species, units, message)); 
		}
	case RADM:
		{
		return(radm_get_data
		(fd, record_no, species_no, databuf, species, units, message)); 
		}
	case RADM_EMIS_LOW:
		{
		return(radm_emis_get_data
		(fd, record_no, species_no, databuf, species, units, message)); 
		}
	case ALPHA_PROTOTYPE:
		{
		return(alpha_get_data
		(fd, record_no, species_no, databuf, species, units, message)); 
		}
	case MM4:
		{
		return(mm4_get_data
		(fd, record_no, species_no, databuf, species, units, message)); 
		}
	case UNKNOWN:
		{
		sprintf(message, "Unknown model type.");
		return (FAILURE);
		}
	}
}
