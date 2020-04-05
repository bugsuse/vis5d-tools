#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

/* ... definitions ... */

#define HIGHRES 01			/* high resolution RADM file */
#define LOWRES 02			/* low resolution RADM file */
#define UNKNOWN 0			/* neither high nor low resolution */

#define cw_EOD 017			/* control word END-OF-DATA */
#define cw_EOF 016			/* control word END-OF-FILE */
#define cw_EOR 010			/* control word END-OF-RECORD */

#define SUCCESS 1                       /* successful function return */
#define FAILURE 0                       /* unsuccessful function return */
#define IOERROR -1                      /* unsuccessful i/o function return */
#define CONV_SUCCESS 0			/* IBM2CRAY conversion success code */

#define MAXSPEC 22                      /* max # species in RADM data file */
#define MAXBUF 8192                     /* max # bytes for RADM i/o buffer */

static char craybuffer[MAXBUF];
static char ibmbuffer[MAXBUF];
static int crayinteger[7];
static float crayfloat[MAXSPEC];

/* variables global to the RADM routines only */

static int radm_emisncol;			/* # columns in data grid */
static int radm_emisnrow;			/* # rows in data grid */
static int radm_emisnlayer;			/* # levels (layers) in grid */
static int radm_emisnrecord;			/* # records in file */
static int radm_emisnspecies;		/* # species in the input file */
static int radm_emisfilesize;		/* file size in bytes */
static int radm_emisresolution;		/* resolution: HIGHRES, LOWRES */
static int radm_emiscollen;			/* # floats in a column of data */

static int radm_emissteplen[1000];
static int radm_emis_nptsour[1000];

static int radm_emis_ksormax;
static int radm_emis_header_size;

static char radm_emisspecname[MAXSPEC*6+1];	/* short name of all species */ 

/*******************************************************************/
/* radm_emis_open                                                       */
/* Function: open input file and determine filesize and resolution */
/* On error: return FAILURE and write error string into message    */
/* If no error: return SUCCESS                                     */
/*******************************************************************/
int radm_emis_open(fd, filename, message)
int *fd;				/* INPUT:  file descriptor */
char *filename;				/* INPUT:  file name */
char *message;				/* OUTPUT: returned error message */
{
struct stat statbuf;			/* UNIX file status buffer */
register int istat;			/* I/O status from UNIX stat call */

if ((*fd = open(filename, O_RDONLY)) == IOERROR)
	{
	sprintf(message, "Cannot open %s", filename);
	return(FAILURE);
	}
if ((istat = stat(filename, &statbuf)) == IOERROR)
	{
	sprintf(message, "Cannot get status");
	close(*fd);
	return(FAILURE);
	}

radm_emisfilesize = statbuf.st_size;
if (radm_emisfilesize <= 0)
	{
	sprintf(message, "File size of zero");
	close(*fd);
	return(FAILURE);
	}
if (!radm_emis_header(*fd, message))
	{
	close(*fd);
	return(FAILURE);
	}
lseek(*fd, 0L, SEEK_SET);
return(SUCCESS);
}

/*********************************************************************/
/* RADM Header Routine                                               */
/* (set header information)                                          */
/*********************************************************************/

int radm_emis_header(fd, message)
int fd;
char *message;		/* OUTPUT: return error string */
{
off_t offset;		/* file offset in bytes */
register int status;	/* routine return status */
register int i, j;	/* local loop variables */
int code;
int nbuf;
int bitoff;
int strd;


code = 6;                               /* conversion code (6 = text) */
nbuf = 80;                       /* # characters to convert */
bitoff = 0;                             /* conversion bit offset */
strd = 1;                               /* conversion mem storage increment */
 
if ((read(fd, ibmbuffer, 80)) != 80)
	{
	sprintf(message, "Cannot read header record #1");
	return(FAILURE);
	}
if ((IBM2CRAY(&code, &nbuf, ibmbuffer, &bitoff,
			  craybuffer, &strd, craybuffer)) != CONV_SUCCESS)
	{
	sprintf(message, "Error converting header block");
	return(FAILURE);
	}

if (strstr(craybuffer, "EMIS") == NULL)
	{
	sprintf(message, "File is not recognized as RADM EMISSIONS.");
	return(FAILURE);
	}

if ((read(fd, ibmbuffer, 80)) != 80)
	{
	sprintf(message, "Cannot read header record #2");
	return(FAILURE);
	}
if ((IBM2CRAY(&code, &nbuf, ibmbuffer, &bitoff,
			  craybuffer, &strd, craybuffer)) != CONV_SUCCESS)
	{
	sprintf(message, "Error converting header block");
	return(FAILURE);
	}

sscanf(craybuffer+34, "%6d", &radm_emisnrow);
sscanf(craybuffer+44, "%6d", &radm_emisnlayer);
sscanf(craybuffer+54, "%6d", &radm_emisncol);
sscanf(craybuffer+65, "%5d", &radm_emisnspecies);
sscanf(craybuffer+77, "%4d", &radm_emisnrecord);

radm_emis_header_size = 80 + 80 + radm_emisnspecies * 80 + 20 * 80 + 7 * 4 +
	radm_emisnrecord  * 4 + (radm_emisnlayer + 1) * 4;

j = 0;
for (i = 0; i < radm_emisnspecies; ++i)
	{
	
	if ((read(fd, ibmbuffer, 80)) != 80)
		{
		sprintf(message, "Cannot read species record");
		return(FAILURE);
		}
	if ((IBM2CRAY(&code, &nbuf, ibmbuffer, &bitoff,
			  craybuffer, &strd, craybuffer)) != CONV_SUCCESS)
		{
		sprintf(message, "Error converting header block");
		return(FAILURE);
		}
	sscanf(craybuffer, "%5c", radm_emisspecname+j);
	if (i == radm_emisnspecies-1)
		radm_emisspecname[j+5] = '\0';
	else
		radm_emisspecname[j+5] = ':';
	j += 6;
	}

/* skip universal file header, file descriptor header */
/* skip species description headers */
/* skip 20 text headers */

offset = 80 + 80 + 80 * radm_emisnspecies + 20 * 80;
if ((lseek(fd, offset, SEEK_SET)) == IOERROR)
	{
	sprintf(message, "Cannot seek 7 integers in header.");
	return(FAILURE);
	}
if ((read(fd, ibmbuffer, 7 * 4)) != 7 * 4)
	{
	sprintf(message, "Cannot read 7-integer record");
	return(FAILURE);
	}
code = 1;
nbuf = 7;
if ((IBM2CRAY(&code, &nbuf, ibmbuffer, &bitoff,
		  crayinteger, &strd, crayinteger)) != CONV_SUCCESS)
	{
	sprintf(message, "Error converting header block");
	return(FAILURE);
	}
radm_emis_ksormax = crayinteger[4];

/* cannot assume all records have equal NPTSOUR values;
   therefore the length of each time step must be calculated separately */

offset = radm_emis_header_size + 88;
code = 1;
nbuf = 1;
for (i = 0; ((i < radm_emisnrecord) && (i < 1000)); ++i)
	{
	if ((lseek(fd, offset, SEEK_SET)) == IOERROR)
		{
		sprintf(message, "Cannot seek nptsour in step 1");
		return(FAILURE);
		}
	if ((read(fd, ibmbuffer, 4)) != 4)
		{
		sprintf(message, "Cannot read nptsour record");
		return(FAILURE);
		}
	if ((IBM2CRAY(&code, &nbuf, ibmbuffer, &bitoff,
			  crayinteger, &strd, crayinteger)) != CONV_SUCCESS)
		{
		sprintf(message, "Error converting header block");
		return(FAILURE);
		}
	radm_emis_nptsour[i] = crayinteger[0];

	radm_emissteplen[i] = 80 + (3 + radm_emis_ksormax * radm_emisncol  
		       		+ (radm_emis_nptsour[i] * (3 + radm_emisnspecies))
		   	      ) * 4;
	offset += radm_emissteplen[i];
	}

return(SUCCESS);
}

int radm_emis_inquire(fd, message, ncol, nrow, nlayer, nrecord, nspecies, specname)
int fd;
char *message;
int *ncol;
int *nrow;
int *nlayer;
int *nrecord;
int *nspecies;
char *specname;
{
if (radm_emis_header(fd, message))
	{
	*ncol = radm_emisncol;
	*nrow = radm_emisnrow;
	*nlayer = radm_emisnlayer;
	*nrecord = radm_emisnrecord;
	*nspecies = radm_emisnspecies;
	sprintf(specname, radm_emisspecname);
	return(SUCCESS);
	}
return(FAILURE);
}
	
int radm_emis_get_date(fd, record_no, datestr, message)
int fd;
int record_no;
char *datestr;
char *message;
{
off_t offset;
int header_year;
int header_day;
int header_hour;
int header_min;
int header_sec;
register int i;

int code;
int nbuf;
int bitoff;
int strd;

code = 6;                               /* conversion code (6 = text) */
nbuf = 80; 	                        /* # characters to convert */
bitoff = 0;                             /* conversion bit offset */
strd = 1;                               /* conversion mem storage increment */

offset = radm_emis_header_size;
for (i = 0; i < record_no - 1; ++i)
	offset += radm_emissteplen[i];
if ((lseek(fd, offset, SEEK_SET)) == IOERROR)
	{
	sprintf(message, "Cannot seek time step header.");
	return(FAILURE);
	}
if ((read(fd, ibmbuffer, 80)) != 80)
	{
	sprintf(message, "Cannot read time step header");
	return(FAILURE);
	}
if ((IBM2CRAY(&code, &nbuf, ibmbuffer, &bitoff,
			  craybuffer, &strd, craybuffer)) != CONV_SUCCESS)
	{
	sprintf(message, "Error converting header block");
	return(FAILURE);
	}
sscanf(craybuffer, "%2d%3d%2d%2d%2d", 
	&header_year, &header_day, &header_hour, &header_min, &header_sec);
julian2std(header_year + 1900, header_day, header_hour,
	 header_min, header_sec, "GMT", datestr);

return(SUCCESS);
}

int radm_emis_get_data(fd, record_no, species_no, databuf, species, units, message)
int fd;
int record_no;
int species_no;
float *databuf;
char *species;
char *units;
char *message;
{
off_t offset;
register int i, j, k;

int jpt;
int khtem;
int islb;
int index;

int code;
int nbuf;
int bitoff;
int strd;

bitoff = 0;                             /* conversion bit offset */
strd = 1;                               /* conversion mem storage increment */

/* zero entire buffer since some values may be missing */

index = 0;
for (i = 0; i < radm_emisnlayer; ++i)
	for (j = 0; j < radm_emisnrow; ++j)
		for (k = 0; k < radm_emisncol; ++k)
			databuf[index++] = 0.0;

offset = radm_emis_header_size;
for (i = 0; i < record_no - 1; ++i)
	offset += radm_emissteplen[i];
offset += 80 + (3 + radm_emis_ksormax * radm_emisncol) * 4;
if ((lseek(fd, offset, SEEK_SET)) == IOERROR)
	{
	sprintf(message, "Cannot seek species values");
	return(FAILURE);
	}
for (i = 0; i < radm_emis_nptsour[record_no-1]; ++i)
	{
	if ((read(fd, ibmbuffer, 3 * 4)) != 3 * 4)
		{
		sprintf(message, "Cannot read species indices");
		return(FAILURE);
		}

	code = 1;                              /* conversion code (1 = int*4) */
	nbuf = 3;                              /* # values to convert */

	if ((IBM2CRAY(&code, &nbuf, ibmbuffer, &bitoff,
			  crayinteger, &strd, crayinteger)) != CONV_SUCCESS)
		{
		sprintf(message, "Error converting species indices");
		return(FAILURE);
		}

	jpt = crayinteger[0] - 1;
	khtem = crayinteger[1] - 1;
	islb = crayinteger[2] - 1;
	
	if ((read(fd, ibmbuffer, radm_emisnspecies * 4)) != radm_emisnspecies * 4)
		{
		sprintf(message, "Cannot read species values");
		return(FAILURE);
		}

	code = 2;                               /* conv code (2 = real*4) */
	nbuf = radm_emisnspecies;                    /* # values to convert */

	if ((IBM2CRAY(&code, &nbuf, ibmbuffer, &bitoff,
			  crayfloat, &strd, crayfloat)) != CONV_SUCCESS)
		{
		sprintf(message, "Error converting species values");
		return(FAILURE);
		}
	for (j = 0; j < radm_emisnspecies; ++j)
		{
		if (j == species_no - 1)
			{
			index = khtem * radm_emisnrow * radm_emisncol + jpt * radm_emisncol + islb;
			databuf[index] = crayfloat[j];
			}
		}
	}
sprintf(units, "%s", "PPM");
return(SUCCESS);
}


