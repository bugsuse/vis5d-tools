#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <fortran.h>

/* ... definitions ... */

#define IBMFILE 1			/* ROM IBM data file type */
#define VAXFILE 2			/* ROM VAX data file type */
#define UNKNOWN 0			/* neither IBM nor VAX file type */

#define CONV_SUCCESS 0			/* successful IBM2CRAY or VAX2CRAY
					   function return */ 

#define SUCCESS 1                       /* successful function return */
#define FAILURE 0                       /* unsuccessful function return */
#define IOERROR -1                      /* unsuccessful i/o function return */

#define MAXSPEC 39                      /* max # species in ROM data file */
#define MAXBUF 192                      /* max # bytes for ROM i/o buffer */
#define MAXCOL 128			/* max # of columns in ROM domain */

/* variables global to the ROM routines only */

static int romncol;			/* # columns in data grid */
static int romnrow;			/* # rows in data grid */
static int romnlayer;			/* # levels (layers) in grid */
static int romnrecord;			/* # records in file */
static int romnspecies;			/* # species in the input file */
static int romfilesize;			/* file size in bytes */
static int romfiletype;			/* file type: IBM or VAX */
static int romcollen;			/* # floats in a column of data */
static int romsteplen;			/* # bytes per time step */
static int romntext;			/* # text header records */
static int romreclen;			/* # bytes per ROM record */
static char romspecname[MAXSPEC*5+1];	/* short name of all species */ 
static float swlong;			/* lower left map x-coordinate */
static float swlat;			/* lower left map y-coordinate */
static float nelong;			/* upper right map x-coordinate */
static float nelat;			/* upper right map y-coordinate */

/*******************************************************************/
/* rom_open                                                        */
/* Function: open input file and determine filesize and filetype   */
/* On error: return FAILURE and write error string into message    */
/* If no error: return SUCCESS                                     */
/*******************************************************************/
int rom_open(fd, filename, message)
int *fd;				/* INPUT:  file descriptor */
char *filename;				/* INPUT:  file name */
char *message;				/* OUTPUT: returned error message */
{
struct stat statbuf;			/* UNIX file status buffer */
register int istat;			/* I/O status from UNIX stat call */
register int status;			/* rom_open function return status */

status = FAILURE;			/* assume FAILURE */

if ((*fd = open(filename, O_RDONLY)) != IOERROR)
	{
	if ((istat = stat(filename, &statbuf)) != IOERROR)
		{
		romfilesize = statbuf.st_size;
		if (romfilesize > 0)
			{
			if ((romfiletype = get_rom_filetype(*fd, message))
				 != UNKNOWN)
				status = SUCCESS;
			else
				{
				sprintf(message,
					 "File is not recognized as ROM"); 
				close(*fd);
				}
			}
		else
			{
			sprintf(message, "File size of zero");
			close(*fd);
			}
		}
	else
		{
		sprintf(message, "Cannot get file status");
		close(*fd);
		}
	}
else
	sprintf(message, "Cannot open file %s", filename);
return(status);
}

/**********************************************************************/
/* get_rom_filetype                                                   */
/* Function: read 2 VAX/IBM words from ROM file to determine filetype */
/* On error: return UNKNOWN and write error string into message       */
/* If no error: return IBMFILE or VAXFILE                             */
/**********************************************************************/
int get_rom_filetype(fd, message)
int fd;					/* INPUT:  file descriptor */
char *message;				/* OUTPUT: return error message */
{
static char buffer[32];
static char convbuf[32];
static char domainbuf[32];

/* It is necessary to pass these variables as addresses to FORTRAN routines
   to do an IBM or VAX text conversion when testing for ROM file type */
 
static int code = 6;                    /* 6 = text mode */
static int nbuf = 8;                    /* # characters to convert */
static int bitoff = 0;                  /* bit offset for conversion */
static int strd = 1;                    /* stride for conversion */

/* It is not enough to compare the IBM2CRAY or VAX2CRAY conversion of text
   to identify whether the input ROM file is IBM or VAX format because both 
   can return success.  A second test must be made to check the converted
   text to be sure that it matches one of the official ROM domains.
*/
 
#define NROM 65
static char *romtype[] = {
   "NEROS1  ","NEROS2  ","NEROS3  ","NEROS4  ","NEROS5  ","NEROS6  ","NEROS7  ",
   "SEROS1  ","SEROS2  ","SEROS3  ","SEROS4  ","SEROS5  ","SEROS6  ","SEROS7  ",
   "ROMNET1 ","ROMNET2 ","ROMNET3 ","ROMNET4 ","ROMNET5 ","ROMNET6 ","ROMNET7 ",
   "NEROXA1 ","NEROXA2 ","NEROXA3 ","NEROXA4 ","NEROXA5 ","NEROXA6 ","NEROXA7 ",
   "SEROXA1 ","SEROXA2 ","SEROXA3 ","SEROXA4 ","SEROXA5 ","SEROXA6 ","SEROXA7 ",
   "MIDROXA1","MIDROXA2","MIDROXA3","MIDROXA4","MIDROXA5","MIDROXA6","MIDROXA7",
   "SUPROXA1","SUPROXA2","SUPROXA3","SUPROXA4","SUPROXA5","SUPROXA6","SUPROXA7",
   "TEXROXA1","TEXROXA2","TEXROXA3","TEXROXA4","TEXROXA5","TEXROXA6","TEXROXA7",
   "CONTUS  ","RWNDQA  ","BIG     ","RAWIND  ","ROMNEX  ","ROMNETEX","BUOYNET ",
   "ULTRANET", "LUSE1   "
                        };

register int filetype;			/* return filetype */
register int found;
register int i;

filetype = UNKNOWN;		/* assume filetype cannot be determined */

if ((lseek(fd, (off_t) (6 * 8), SEEK_SET)) != IOERROR)
	{
	if ((read(fd, buffer, 8)) != IOERROR)
		{
		buffer[8] = '\0';
		if (IBM2CRAY(&code, &nbuf, buffer, &bitoff, convbuf,
				&strd, convbuf) == CONV_SUCCESS)
			{
			convbuf[8] = '\0';
			for (i = 0; i < NROM; ++i)
				{
				if (!strcmp(convbuf, romtype[i]))
					{
					filetype = IBMFILE;
					sprintf(domainbuf, "%s", romtype[i]);
					break;
					}
				}
			}
		if (filetype != IBMFILE)
			{
			if (VAX2CRAY(&code, &nbuf, buffer, &bitoff, convbuf,
					&strd, convbuf) == CONV_SUCCESS)
				{
				convbuf[8] = '\0';
				for (i = 0; i < NROM; ++i)
					{
					if (!strcmp(convbuf, romtype[i]))
						{
						filetype = VAXFILE;
						sprintf(domainbuf, "%s",
								 romtype[i]);
						break;
						}
					}
				}
			}
		}
 
	else
		sprintf(message, "Cannot read filetype");
	}
else
	sprintf(message, "Cannot seek file position to determine filetype");
/*
if (filetype)
	fprintf(stderr, "ROM DOMAIN: %s\n", domainbuf);
*/
return(filetype);
}

/*************************************************************************/
/* get_rom_buf (ROM buffer reader)				         */
/* Function: read ROM data into buffer and perform VAX or IBM conversion */
/* On error: return FAILURE and write error string into message   	 */
/* If no error: return SUCCESS                                           */
/*************************************************************************/
int get_rom_buf(fd, offset, buffer, nbytes, code, nbuf, bitoff, strd, 
	convbuf, message)
int fd;				/* file descriptor */
off_t offset;			/* file offset in bytes */
char *buffer;			/* character buffer for initial read */
int nbytes;			/* number of bytes to read */
int code;			/* conversion code; 2 = real; 6 = float */
int nbuf;			/* number of values to convert */
int bitoff;			/* bit offset on which to begin conversion */
int strd;			/* stride for conversion array */
char *convbuf;			/* character buffer for final conversion */
char *message;			/* return error message string */
{
register int status;
register int i;

/* This routine reads nbytes into a buffer and converts to real or character
   by calling one of the FORTRAN functions, VAX2CRAY or IBM2CRAY, checking
   the conversion status.
*/

status = SUCCESS;
if ((lseek(fd, offset, SEEK_SET)) != IOERROR)
	{
	if ((read(fd, buffer, nbytes)) == nbytes)
		{
		switch(romfiletype)
			{
			case VAXFILE:
				{
				i = VAX2CRAY(&code, &nbuf, buffer, &bitoff,
					convbuf, &strd, convbuf);
				break;
				}
			case IBMFILE:
				{
				i = IBM2CRAY(&code, &nbuf, buffer, &bitoff,
					convbuf, &strd, convbuf);
				break;
				}
			}
		if (i == CONV_SUCCESS)
			{
			if (code == 6)		/* if text, add NULL */
				convbuf[nbuf] = '\0';
			}
		else
			{
			status = FAILURE;
			sprintf(message,
				 "Error converting ROM data to CRAY format");
			}
		}
	else
		{
		status = FAILURE;
		sprintf(message, "Error reading ROM data into buffer");
		}
	}
else
	{
	status = FAILURE;
	sprintf(message, "Error seeking offset into ROM data");
	}
return(status);
}
	
/*********************************************************************/
/* ROM Header Routine                                               */
/* (set header information)                                          */
/*********************************************************************/

int rom_header(fd, message)
int fd;
char *message;		/* OUTPUT: return error string */
{
off_t offset;		/* file offset in bytes */
register int status;	/* routine return status */
register int i, j, k;	/* local loop variables */
char *buffer;		/* local character read buffer */
char *convbuf;		/* character conversion buffer (from IBM/VAX) */
char *str1;		/* local string variable */
char *str2;		/* local string variable */
int code;               /* conversion code (6 = text) */
int nbuf;	        /* # characters to convert */
int bitoff;             /* conversion bit offset */
int strd;               /* conversion mem storage increment */
int nbytes;		/* number of bytes to read */

status = SUCCESS;	/* assume SUCCESS */

if (	((buffer = ((char *) malloc(sizeof(char) * MAXBUF))) == NULL) ||
	((convbuf = ((char *) malloc(sizeof(char) * MAXBUF))) == NULL) ||
	((str1 = ((char *) malloc(sizeof(str1) * MAXBUF))) == NULL) ||
	((str2 = ((char *) malloc(sizeof(str2) * MAXBUF))) == NULL)
   )
	{
	status = FAILURE;
	sprintf(message, "Cannot allocate local memory reading header");
	}
else
	{
	code = 6;		/* code 6 = text conversion */
	nbuf = MAXBUF;
	bitoff = 0;
	strd = 1;	
	nbytes = MAXBUF;

/* 
 * Header TEXT Record # 1
 * var 1:	integer*4	file creation date as MMDDYY
 * var 2:	integer*4	file creation time as HHMMSS
 * var 3:	integer*4	Julian start date of scenario as YYDDD
 * var 4:	integer*4	start hour or scenario (00 - 23)
 * var 5:	integer*4	time step size for simulation
 * var 6:	integer*4	time to first step
 * var 7:	character*8	grid definition name
 * var 8:	real*4		longitude of southwest corner of grid
 * var 9:	real*4		latitude of southwest corner of grid
 * var 10:	real*4		longitude of northeast corner of grid
 * var 11:	real*4		latitude of northeast corner of grid
 * var 12:	real*4		grid cell longitudinal increment
 * var 13:	real*4		grid cell latitudinal increment
 * var 14:	integer*4	number of col,umns in grid
 * var 15:	integer*4	number of rows in grid
 * var 16:	integer*4	number of levels in the simulation
 * var 17:	integer*4	number of species in the CONC file
 * var 18:	integer*4	creation date of bmat file
 * var 19:	integer*4	creation time of bmat file
 * var 20:	integer*4	creation date of btrk file
 * var 21:	integer*4	creation time of btrk file
 * var 22:	integer*4	creation date of bcon file
 * var 23:	integer*4	creation time of bcon file
 * var 24	integer*4	creation date of initial conditions CONC file
 * var 25:	integer*4	creation time of initial conditions CONC file
 * var 26:	integer*4	number of text records
 */
	offset = 0;
	if (!get_rom_buf(fd, offset, buffer, nbytes, code, nbuf, bitoff,
		strd, convbuf, message))
		{
		status = FAILURE;
		}
	else
		{
		sscanf(convbuf, "%104c%4d%4d%4d%4d%64c%4d",
			str1, &romncol, &romnrow, &romnlayer,
			&romnspecies, str2, &romntext);

		sscanf(str1+56, "%g%g%g%g",
			 &swlong, &nelong, &swlat, &nelat);

		if (swlat > 0)
			swlat = -swlat;
		if (swlong > 0)
			swlong = -swlong;

		romreclen = romnlayer * romncol * 4;
		romsteplen = romreclen * (romnspecies * romnrow + 1);
		romnrecord = (romfilesize - (3 + romntext)
				 * romreclen)/romsteplen;  

		offset += romreclen;

/*
* Header TEXT Record #2
* var 1-N:	character*4		name of chemical species (1-N)
*
* Header TEXT Record #3
* var 1-N:	character*4		name of layer (1-N)
*
* Header TEXT Record #4 - #(3+N)
* var 1-N:	character*80		text string (lines 1-N)
*/
		if (!get_rom_buf(fd, offset, buffer, nbytes, code, nbuf,
			 bitoff, strd, convbuf, message))
			{
			status = FAILURE;
			}
		else
			{
			k = 0;
			for (i = 0; i < romnspecies; ++i)
				{
				for (j = 0; j < 4; ++j)
					romspecname[k++] =
						 convbuf[i*4+j];
				if (i != romnspecies - 1)
					{
					romspecname[k] = ':';
					++k;
					}
				}
			romspecname[k] = '\0';
			}
		}
	}
if (buffer != NULL)
	free((char *) buffer);
if (convbuf != NULL)
	free((char *) convbuf);
if (str1 != NULL)
	free((char * ) str1);
if (str2 != NULL)
	free((char *) str2);
return(status);
}

int rom_inquire(fd, message, ncol, nrow, nlayer, nrecord, nspecies, specname)
int fd;
char *message;
int *ncol;
int *nrow;
int *nlayer;
int *nrecord;
int *nspecies;
char *specname;
{
if (rom_header(fd, message))
	{
	*ncol = romncol;
	*nrow = romnrow;
	*nlayer = romnlayer;
	*nrecord = romnrecord;
	*nspecies = romnspecies;
	sprintf(specname, "%s", romspecname);
	return(SUCCESS);
	}
return(FAILURE);
}
	
int rom_get_date(fd, record_no, datestr, message)
int fd;
int record_no;
char *datestr;
char *message;
{
off_t offset;			/* file offset in bytes */
char *buffer;			/* character data buffer */
char *convbuf;			/* character data conversion buffer */
int header_year;		/* year in time step header */
int header_day;			/* day in time step header */
int header_hour;		/* hour in time step header */
int header_min;			/* minutes in time step header */
int header_sec;			/* seconds in time step header */
int nbytes;			/* number of bytes to read into buffer */
float *starthour;		/* beginning hour in time step header */
float *startdate;		/* beginning date in time step header */
register int i;			/* loop counter */
register int status;		/* return status */

/* These variables are passed as addresses to a FORTRAN conversion routine to
   translate characters or floats from IBM or VAX format to CRAY format. */

int code;			/* conversion code; 6 = text; 2 = float */
int strd;			/* stride if conversion array */
int bitoff;			/* bit offset for conversion */
int nbuf;			/* number of buffer values to convert */

status = SUCCESS;		/* assume SUCCESS */
if (	((buffer = ((char *) malloc(sizeof(char) * 64))) == NULL) || 
	((convbuf = ((char *) malloc(sizeof(char) * 64))) == NULL)	)
	{
	sprintf(message, "Cannot allocate local memory");
	status = FAILURE;
	}
else
	{
	code = 2;		/* code 2 = float */
	nbuf = 2;		/* convert  2 floating point numbers */
	bitoff = 0;		/* bit offset */
	strd = 1;		/* stride */
	nbytes = 8;		/* 2 floats = 2 * 4 bytes each = 8 bytes */

/*
 * Time Step Header Record
 * var 1:	real*4		current time step date as YYDDD
 * var 2:	real*4		current time step time as HHMMSS
 * var 3:	real*4		elapsed time since scenario start (s)
 * var 4:	real*4		step number of the CONC file
 *
 */
	offset = (3 + romntext) * romreclen + (record_no - 1) * romsteplen;
	if (get_rom_buf(fd, offset, buffer, nbytes, code, nbuf, bitoff, strd,
		convbuf, message))
		{
		startdate = (float *) convbuf;
		starthour = (float *) (convbuf + 8);
		header_year = ((int) *startdate)/1000;
		header_day = ((int) *startdate) - (header_year * 1000);
		header_hour = ((int) *starthour)/10000;
		header_min = ( ((int) *starthour) - (header_hour * 10000))/100;
		header_sec = ((int) *starthour) - (header_hour * 10000) -
				                    (header_min * 100);
		julian2std(header_year + 1900, header_day,
			 header_hour, header_min, header_sec, "EST", datestr);
		}
	else
		{
		status = FAILURE;
		sprintf(message, "Cannot read time step header");
		}
	}
if (buffer != NULL)
	free((char *) buffer);
if (convbuf != NULL)
	free((char *) convbuf);
return(status);
}

int rom_get_data(fd, record_no, species_no, databuf, species, units, message)
int fd;
int record_no;
int species_no;
float *databuf;
char *species;
char *units;
char *message;
{
char *buffer;			/* character read buffer */
char *convbuf;			/* floating point conversion buffer */
float *fbuf;			/* floating point pointer to single element */
off_t offset;			/* file offset in bytes */
int nbytes;			/* number of bytes to read into buffer */
register int i, j, k;		/* local loop variables */
register int index;		/* index into output field databuf */
register int ix, iy;		/* local loop variables for grid */
register int status;		/* routine return status */


/* These variables are passed as addresses to a FORTRAN conversion routine to
   translate characters or floats from IBM or VAX format to CRAY format. */

int code;			/* conversion code; 6 = text; 2 = float */
int strd;			/* stride if conversion array */
int bitoff;			/* bit offset for conversion */
int nbuf;			/* number of buffer values to convert */

status = SUCCESS;		/* assume SUCCESS */

if (	((buffer = ((char *) malloc(sizeof(char) * MAXCOL * 4))) == NULL) ||
	((convbuf = ((char *) malloc(sizeof(char) * MAXCOL * 8))) == NULL))
	{
	sprintf(message, "Cannot allocate local memory.");
	status = FAILURE;
	}
else
	{
/* beginning of time step */

	offset = (3 + romntext) * romreclen + (record_no - 1) * romsteplen;

/* skip time step header */

	offset += romreclen;

/* number of bytes in a column of data (4 bytes for each data point) */

	nbytes = romncol * 4;
	code = 2;
	nbuf = romncol;
	strd = 1;
	bitoff = 0;

	for (i = 0; ((i < romnrow) && (status == SUCCESS)); ++i)
		{
		for (j = 0; ((j < romnspecies) && (status == SUCCESS)); ++j)
			{
			for (k = 0; ((k < romnlayer) && (status == SUCCESS)); ++k)
				{
				if (j == species_no - 1)
					{
					if (get_rom_buf(fd, offset, buffer,
						nbytes, code, nbuf, bitoff,
						strd, convbuf, message))
						{
						for (ix = 0; ((ix < romncol)
						     && (status == SUCCESS));
							++ix)
							{
						index = k * romnrow * romncol +
							i * romncol + ix;
					fbuf = (float *) (convbuf + 8 * ix);
						databuf[index] = *fbuf;
							}
						}
					else
						{
						sprintf(message,
						   "Data column read error");
						status = FAILURE;
						break;
						}
					}
				offset += nbytes;
				}
			}
		}
	}
if (buffer != NULL)
	free((char *) buffer);
if (convbuf != NULL)
	free((char *) convbuf);
sprintf(units, "%s", "PPM");
return(status);
}

