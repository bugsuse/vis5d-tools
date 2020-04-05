#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <fortran.h>

/* ... definitions ... */

#define IEEE 01				/* IEEE RADM file */
#define UNICOS 02			/* UNICOS RADM file */
#define UNKNOWN 0			/* neither IEEE nor UNICOS format */

#define cw_EOD 017			/* control word END-OF-DATA */
#define cw_EOF 016			/* control word END-OF-FILE */
#define cw_EOR 010			/* control word END-OF-RECORD */

#define SUCCESS 1                       /* successful function return */
#define FAILURE 0                       /* unsuccessful function return */
#define IOERROR -1                      /* unsuccessful i/o function return */

#define MAXSPEC 22                      /* max # species in RADM data file */
#define MAXBUF 4096                     /* max # bytes for RADM i/o buffer */

/* variables global to the RADM routines only */

static int radmncol;			/* # columns in data grid */
static int radmnrow;			/* # rows in data grid */
static int radmnlayer;			/* # levels (layers) in grid */
static int radmnrecord;			/* # records in file */
static int radmnspecies;		/* # species in the input file */
static int radmfilesize;		/* file size in bytes */
static int radmformat;			/* RADM format: IEEE, UNICOS */
static int radmcollen;			/* # floats in a column of data */

static int radmsteplen;			/* IEEE:# halfwords in time step */
					/* UNICOS: # words in time step */

static int radmdatablock1;		/* block # of data just beyond header */

static int radmdatapos1;		/* IEEE:halfword starting position */
					/* UNICOS: word starting position */

static char radmspecname[MAXSPEC*6+1];	/* short name of all species */ 

/*******************************************************************/
/* radm_open                                                       */
/* Function: open input file and determine filesize and format     */
/* On error: return FAILURE and write error string into message    */
/* If no error: return SUCCESS                                     */
/*******************************************************************/
int radm_open(fd, filename, message)
int *fd;				/* INPUT:  file descriptor */
char *filename;				/* INPUT:  file name */
char *message;				/* OUTPUT: returned error message */
{
struct stat statbuf;			/* UNIX file status buffer */
register int istat;			/* I/O status from UNIX stat call */
register int status;			/* radm_open function return status */

status = FAILURE;			/* assume FAILURE */

if ((*fd = open(filename, O_RDONLY)) != IOERROR)
	{
	if ((istat = stat(filename, &statbuf)) != IOERROR)
		{
		radmfilesize = statbuf.st_size;
		if (radmfilesize > 0)
			{
			if ((radmformat = radm_format(*fd, message))
				 != UNKNOWN)
				status = SUCCESS;
			else
				{
				sprintf(message,
					 "File is not recognized as RADM"); 
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

/***********************************************************************/
/* radm_format                                                         */
/* Function: read 2 CRAY words from RADM file to determine file format */
/* On error: return UNKNOWN and write error string into message        */
/* If no error: return IEEE or UNICOS                                  */
/***********************************************************************/
int radm_format(fd, message)
int fd;					/* INPUT:  file descriptor */
char *message;				/* OUTPUT: return error message */
{
#define ieeeline    "AX    69NSPEC   "	/* High Res IEEE CRAY words 18&19 */
#define ieee2line   "AX    35NSPEC   "	/* New Low Res IEEE CRAY words 18&19 */
#define unicosline  "15IMAX    35NSPE"	/* Old Low Res 15 layer CRAY words 18 & 19 */
#define unicos2line " 6IMAX    35NSPE"  /* Old Low Res 6 layer CRAY words 18&19*/
#define unicos3line  "25IMAX    35NSPE"	/* Old Low Res 25 layer CRAY words 18 & 19 */
#define unicos4line  "15IMAX    37NSPE"	/* 27 km file 15 layer CRAY words 18 & 19 */
#define unicoshighmetline  "15IMAX    69NSPE"	/* High Res 15 layer met in UNICOS format CRAY words 18 & 19 */

register int file_format;		/* return file_format */
static char buffer[64];

file_format = UNKNOWN;		/* assume file_format cannot be determined */

if ((lseek(fd, (off_t) (18 * 8), SEEK_SET)) != IOERROR)
	if ((read(fd, buffer, 16)) != IOERROR)
		{
		buffer[16] = '\0';
		if (!strcmp(buffer, ieeeline))
			file_format = IEEE;
		else if (!strcmp(buffer, ieee2line))
			file_format = IEEE;
		else if (!strcmp(buffer, unicosline))
			file_format = UNICOS;
		else if (!strcmp(buffer, unicos2line))
			file_format = UNICOS;
		else if (!strcmp(buffer, unicos3line))
			file_format = UNICOS;
		else if (!strcmp(buffer, unicos4line))
			file_format = UNICOS;
/* this doesn't seem to work! -- klp 11/10/94
		else if (!strcmp(buffer, unicoshighmetline))
			file_format = UNICOS;
*/
		}
	else
		sprintf(message, "Cannot read file_format");
else
	sprintf(message, "Cannot seek file position to determine file_format");
return(file_format);
}

/*******************************************************************/
/* get_radm_offset                                                 */
/* Function: get the file offset for nwords input                  */
/*           (nwords = CRAY half words for IEEE file_format files)  */
/*	     (nwords = CRAY whole words for CRAY file_format files ) */
/* On error: return FAILURE and write error string into message    */
/* If no error: return SUCCESS                                     */
/*******************************************************************/
int get_radm_offset(nwords, offset, message)
int nwords;		/* INPUT:  # of words/halfwords beyond header */ 
off_t *offset;		/* OUTPUT: file offset in bytes */
char *message;		/* OUTPUT: message buffer for error string */
{
register int status;

switch(radmformat)
	{
	case UNICOS:	
		{
		status = get_unicos_offset(nwords, offset);
		break;
		}
	case IEEE:
		{
		status = get_ieee_offset(nwords, offset); 
		break;
		}
	}
if (status == FAILURE)
	sprintf(message, "Error setting file offset");
return(status);
}

/*******************************************************************/
/* get_radm_buf                                                          */
/* Function: read nwords (CRAY words or halfwords) into 'buffer',  */
/*           reading from file 'fd' of 'file_format' size at        */
/*           'offset' position.                                    */
/* On error: return FAILURE and write error string into message    */
/* If no error: return SUCCESS                                     */
/*******************************************************************/
int get_radm_buf(fd, buffer, nwords, offset, message)
int fd;					/* INPUT:  file descriptor */
char *buffer;				/* OUTPUT: character data buffer */
int nwords;				/* INPUT:  # words/halfwords */
off_t offset;				/* INPUT:  file offset in bytes */
char *message;				/* OUTPUT: buffer for error string */
{
register int status;

switch(radmformat)
	{
	case UNICOS:
		{
		status = get_unicos_buf(fd, buffer, nwords, offset);
		break;
		}
	case IEEE:
		{
		status = get_ieee_buf(fd, buffer, nwords, offset);
		break;
		}
	}
if (status == FAILURE)
	sprintf(message, "Error reading data");
return(status);
}

/*******************************************************************/
/* getieee_offset                                               */
/* Function: calculate file offset in bytes                        */
/* On error: return FAILURE                                        */
/* If no error: return SUCCESS                                     */
/*******************************************************************/
int get_ieee_offset(nwords, offset)
int nwords;			/* INPUT:  # halfwords to skip for offset */
off_t *offset;			/* OUTPUT: return offset */
{
register int twords;		/* # halfwords yet to count toward offset */
register int pos;		/* halfword offset pos within offset block */
register int block;		/* 1024-halfword block containing offset pos */
register int status;		/* return status */

status = SUCCESS;		/* assume SUCCESS */

/* begin offset search at BOR just beyond last header record EOR */

block = radmdatablock1;
pos = radmdatapos1;

twords = nwords;		/* # halfwords to count */
while (twords + pos > 1023)	/* if # halfwords to count span blocks ... */
	{
	++block;		/* . increment block # */
	twords -= (1024 - pos);	/* . decrement # halfwords by block remainder */
	pos = 0;		/* . reset position within new block */
	}
pos += twords;			/* set position to # halfwords remaining */
*offset = (block * 1024 + pos) * 4;	/* convert halfword offset to bytes */
if (*offset >= radmfilesize)	/* if offset is beyond EOF return with error */
	status = FAILURE;
return(status );
}

/*******************************************************************/
/* get_ieee_buf                                                 */
/* Function: read nwords (CRAY halfwords) into 'buffer',           */
/*           reading from file 'fd' of 'file_format' size at        */
/*           'offset' position.                                    */
/* On error: return FAILURE and write error string into message    */
/* If no error: return SUCCESS                                     */
/*******************************************************************/
int get_ieee_buf(fd, buffer, nwords, file_offset)
int fd;				/* INPUT:  file descriptor */
char *buffer;			/* OUTPUT: character input buffer */
int nwords;			/* INPUT:  # halfwords to store in buffer */
off_t file_offset;		/* INPUT:  file offset */
{
register int gotbytes;		/* # bytes read so far */
register int getbytes;		/* # bytes to read into buffer */
register int n;			/* buffer byte position to store next read */
register int status;		/* return status */

off_t offset;			/* use local offset since it must be reset */
unsigned int cw;		/* control word */

status = SUCCESS;		/* assume SUCCESS */
offset = file_offset;		/* local offset */
gotbytes = n = 0;		/* no bytes read, buffer position = 0 */

/* continue until # requested halfwords read or until FATAL error occurs */
while ((gotbytes < nwords * 4) && (status == SUCCESS))
	{
	if ((lseek(fd, offset, SEEK_SET)) != IOERROR)
		{
		if ((read(fd, &cw, 8)) == 8)	/* get control word (8 bytes) */
			{
			getbytes = (cw >> 32);	/* # bytes in this record */

			/* do not get more than requested */
			if (getbytes + n > nwords * 4)
				getbytes = nwords * 4 - n;

			offset += 4;	/* add 1 halfword (4bytes) to offset */
			
			/* read # bytes available (if not more than requested)*/
			if ((lseek(fd, offset, SEEK_SET)) != IOERROR)
				{
				if ((read(fd, buffer+n, getbytes)) == getbytes)
					{			
					/* increase # bytes in buf by # read */
					gotbytes += getbytes;

					/* set buffer position accordingly */
					n = gotbytes;

					/* skip data read and END-OF-RECORD */
					offset += getbytes + 4;
					}
				else
					status = FAILURE;
				}
			else
				status = FAILURE;
			}
		else
			status = FAILURE;
		}
	else
		status = FAILURE;
	}
return(status);
}


/*******************************************************************/
/* get_unicos_offset                                               */
/* Function: get the file offset for nwords input                  */
/*	     (nwords = CRAY whole words for UNICOS file_format files ) */
/* On error: return FAILURE                                        */
/* If no error: return SUCCESS                                     */
/*******************************************************************/
int get_unicos_offset(nwords, offset)
int nwords;			/* INPUT:  # 8-byte CRAY words offset */
off_t *offset;			/* OUTPUT: file offset in bytes */
{
register int twords;		/* # words yet to count toward offset */
register int pos;		/* word offset position within offset block */
register int block;		/* 512-word block containing offset pos */
register int status;		/* return status */

status = SUCCESS;		/* assume SUCCESS */

/* begin offset search at EOR just beyond last header record */

block = radmdatablock1;
pos = radmdatapos1;

twords = nwords;		/* # words to count */
while (twords + pos > 511)	/* if # words to count span blocks ... */
	{
	++block;		/* . increment block # */
	twords -= (511 - pos);	/* . decrement # words by block remainder */
	pos = 0;		/* . reset position within new block */
	}
pos += twords;			/* set position to # words remaining */
*offset = (block * 512 + pos) * 8;	/* convert word offset to bytes */
if (*offset >= radmfilesize)	/* if offset is beyond EOF return with error */
	status = FAILURE;
return(status);
}

/*******************************************************************/
/* get_unicos_buf                                                  */
/* Function: read nwords (CRAY words) into 'buffer',               */
/*           reading from file 'fd' of 'file_format' size at        */
/*           'offset' position.                                    */
/* On error: return FAILURE                                        */
/* If no error: return SUCCESS                                     */
/*******************************************************************/
int get_unicos_buf(fd, buffer, nwords, offset)
int fd;				/* INPUT:  file descriptor */
char *buffer;			/* OUTPUT: character data buffer */
int nwords;			/* INPUT:  # words to store in buffer */
off_t offset;			/* INPUT:  beginning file position in bytes */
{
register int gotbytes;		/* # bytes read so far */
register int getbytes;		/* # bytes to read into buffer */
register int n;			/* buffer byte position to store next read */
register int status;		/* return status */

unsigned int cw;		/* control word */
int type;			/* control word type */

status = SUCCESS;		/* assume SUCCESS */

gotbytes = n = 0;		/* no bytes read, buffer position = 0 */

if ((lseek(fd, offset, SEEK_SET)) == IOERROR)
	status = FAILURE;

/* continue until # requested words read or until FATAL error encountered */

while ((gotbytes < nwords * 8)&& (status == SUCCESS))
	{
	if ((read(fd, &cw, 8)) == 8)	/* get control word (8 bytes) */
		{
		type = (cw) >> 60;	/* type of control word */	
		if ((type != cw_EOD) && (type != cw_EOF))
			{
			/* get # bytes to read in this record */
			getbytes = (cw & 0777) * 8;

			/* do not get more than requested */
			if (getbytes + n > nwords * 8)
				getbytes = nwords * 8 - n;

			/* read # bytes available/requested */
			if ((read(fd, buffer+n, getbytes)) == getbytes)
				{
				/* increment # bytes in buffer by # read */
				gotbytes += getbytes;

				/* set buffer position accordingly */
				n = gotbytes;
				}
			else
				status = FAILURE;
			}
		else
			status = FAILURE;
		}
	else
		status = FAILURE;
	}
return(status);
}

/*******************************************************************/
/* radm_point                                                      */
/* Function: convert/cast character data into single float value   */
/* On error: return FAILURE                                        */
/* If no error: return SUCCESS                                     */
/*******************************************************************/
int radm_point(buffer, bindex, fbuf)
char *buffer;				/* INPUT:  character buffer */
int bindex;				/* INPUT:  buffer index */
float *fbuf;				/* OUTPUT: floating point value */
{

/* The next three values must be declared as variables and passed with the
   &code, &nbuf, &bitoff addresses to the FORTRAN IEG2CRAY routine to convert
   IEEE floating point values to CRAY float.  This FORTRAN call is made only
   for IEEE file_format RADM files. */

static int code = 2;			/* IEEE conversion code: 2 = REAL */
static int nbuf = 1;			/* # of values to convert */
static int bitoff = 0;			/* bit offset for conversion */

float *xbuf;				/* floating point buffer value */

register int status;			/* return status */
status = SUCCESS;			/* assume SUCCESS */

switch(radmformat)
	{
	case IEEE:
		{
		if (IEG2CRAY(&code, &nbuf, buffer + bindex * 4, &bitoff, fbuf)
			 != 0)
			status = FAILURE;
		break;
		}
	case UNICOS:
		{
		xbuf = (float *) (buffer + bindex * 8);
		*fbuf = *xbuf;
		break;
		}
	}
return(status);
}

/*********************************************************************/
/* RADM Header Routine                                               */
/* (set header information)                                          */
/*********************************************************************/

int radm_header(fd, message)
int fd;
char *message;		/* OUTPUT: return error string */
{
off_t offset;		/* file offset in bytes */
register int status;	/* routine return status */
register int i, j, k;	/* local loop variables */
static char buffer[MAXBUF];

status = SUCCESS;	/* assume SUCCESS */

/* 
 * Universal File Header Record (1)
 * 10 CRAY words (80 characters)
 * 
 * a35	file dataset name
 * a6	file creation date (mmddyy)
 * a6	file creation time (hhmmss)
 * a10	origin system 
 * a10	origin processor 
 * a5	file application 
 * a3	file version 
 * a2	file logical 
 *
 * File Descriptor Record (1)
 * 10 CRAY words (80 characters)
 *
 * a5	file descriptor start date (yyddd)
 * a6	start time (hhmmss)
 * a6	step size expressed in seconds (integer, left justified, blank filled)
 * a8	grid name 
 * a1	test switch indicator 
 * a4	number of dimensions of data (integer, right justified, zero filled)
 * 5a10	dimension group labels (up to 5) [jmax, kmax, imax, nspec, ... ]
 *
 */
offset = 0;			/* at beginning of file ... */
if (radmformat == IEEE)
	i = 40;			/* # halfwords to read */
else
	i = 20;			/* # words to read */

/* get universal & file description headers */

if (!get_radm_buf(fd, buffer, i, offset, message))
	status = FAILURE;
else
	{
	sscanf(buffer+118, "%2d", &radmnrow);
	sscanf(buffer+128, "%2d", &radmnlayer);
	sscanf(buffer+138, "%2d", &radmncol);
	sscanf(buffer+148, "%2d", &radmnspecies);

	/* calculate # of values in a column of data */
		
	radmcollen = radmnspecies * radmnlayer * radmnrow;

	/* calculate # of values in a time step with header 
	   (BOR = beginning-of-record, EOR = end-of-record)
	 * HIGH RES: BOR + TIME STEP HEADER + EOR +
		     # columns * (BOR + # column values + EOR)
	 * LOW RES:  TIME STEP HEADER + EOR +
		     # columns * (# column values + EOR)
	 */
	
	if (radmformat == IEEE)
		radmsteplen = 1 + 20 + 1 + radmncol *
			 (1 + radmcollen + 1);
	else
		radmsteplen = 10 + 1 + radmncol * (radmcollen + 1);

/*
* Species Description Header Record
* (10 CRAY words [80 characters] for each species in file)
*
* a5	species short name
* a20	species long name
* a10	species units
* a4	mapping info for file order 
*
*/

	if (radmformat == IEEE)
		{	
/* skip BOR + 20 halfwords + EOF for 2 header records already read */
		offset = 44 * 4;	/* skip 44 halfwords */
		i = 20;			/* read 20 halfwords */
		}
	else
		{
/* skip CONTROL WORD + 10 words for 2 header records already read */
		offset = 22 * 8;	/* skip 22 words */
		i = 10;			/* read 10 words */
		}

/* get species description header and save 1st 5 characters of each species */

	if (!get_radm_buf(fd, buffer, radmnspecies*i, offset,message))
		status = FAILURE;
	else
		{
		k = 0;
		for (i = 0; i < radmnspecies; ++i)
			{
			for (j = 0; j < 5; ++j)
				{
				radmspecname[k] = buffer[i * 80 + j];
				++k;
				}
			if (i != radmnspecies - 1)
				{
				radmspecname[k] = ':';
				++k;
				}
			}
		radmspecname[k] = '\0';


/* set block and word positions just beyond header from which file offsets
will be calculated 

(if files with more than 22 species are considered, these 2 calculations
must be reworked)
*/

		radmdatablock1 = 0;
		if (radmformat == IEEE)
			{
/* skip [BOR + 20 halfwords + EOR] (= 22)
for Universal header, File descriptor header,
    ISPEC headers, 20 text headers
*/
			radmdatapos1 = (2 + radmnspecies + 20) * 22;

/* calculate approximate # of steps from filesize divided by 4 bytes per
halfword value, initial halfword data position, and step length in bytes
*/

			radmnrecord =
			 (radmfilesize/4 - radmdatapos1)/radmsteplen;
			}
		else
			{
/* skip [CW + 10 words] (= 11)
for Universal header, File descriptor header,
    ISPEC headers, 20 text headers
*/
			radmdatapos1 = (2 + radmnspecies + 20) * 11;

/* calculate approximate # of steps from radmfilesize divided by 8 bytes per
word value, initial word data position, and step length in bytes;
subtract 1 word for each 4096-byte (512-word) block used as BCW
(BLOCK CONTROL WORD)
*/
			radmnrecord =
(radmfilesize/8 - radmfilesize/4096 - radmdatapos1)/radmsteplen;
			}
		}
	}
return(status);
}

int radm_inquire(fd, message, ncol, nrow, nlayer, nrecord, nspecies, specname)
int fd;
char *message;
int *ncol;
int *nrow;
int *nlayer;
int *nrecord;
int *nspecies;
char *specname;
{
if (radm_header(fd, message))
	{
	*ncol = radmncol;
	*nrow = radmnrow;
	*nlayer = radmnlayer;
	*nrecord = radmnrecord;
	*nspecies = radmnspecies;
	sprintf(specname, "%s", radmspecname);
	return(SUCCESS);
	}
return(FAILURE);
}
	
int radm_get_date(fd, record_no, datestr, message)
int fd;
int record_no;
char *datestr;
char *message;
{
off_t offset;
int header_year;
int header_day;
int header_hour;
static char buffer[8];
register int i;
register int status;

status = SUCCESS;
i = (record_no - 1) * radmsteplen;
if (!get_radm_offset(i, &offset, message))
	{
	sprintf(message, "Cannot get record offset for date.");
	status = FAILURE;
	}
else
	{
	if (radmformat == IEEE)
		i = 2;		/* 2 CRAY half-words */
	else
		i = 1;		/* 1 CRAY word */
	if (!get_radm_buf(fd, buffer, i, offset, message))	
		{
		sprintf(message, "Cannot get buffer for date.");
		status = FAILURE;
		}
	else
		{
		buffer[7] = '\0';
		sscanf(buffer, "%2d%3d%2d",
			&header_year, &header_day, &header_hour);
		julian2std(header_year + 1900, header_day,
			 header_hour, 0, 0, "GMT", datestr);
		}
	}
return(status);
}

int radm_get_data(fd, record_no, species_no, databuf, species, units, message)
int fd;
int record_no;
int species_no;
float *databuf;
char *species;
char *units;
char *message;
{
char *colbuf;			/* character data column buffer */
off_t offset;			/* file offset in bytes */
float fbuf;			/* floating point data value */
register int i, j, k;		/* local loop variables */
register int bindex;		/* index into input data colbuf */
register int findex;		/* index into output field colbuf */
register int ix, iy;		/* local loop variables for grid */
register int status;		/* routine return status */

status = SUCCESS;		/* assume SUCCESS */

if (radmformat == IEEE)
	i = radmcollen * 4;	/* column bytes = column halfwords * 4 bytes */
else
	i = radmcollen * 8;	/* column bytes = column words * 8 bytes */


/* allocate a colbuf of the size of an entire column of data */

if (((colbuf = ((char *) malloc(sizeof(char) * i))) == NULL))
	{
	sprintf(message, "Cannot allocate local memory");
	status = FAILURE;
	}
else
	{

/* skip hours not requested and TIME STEP HEADER
 * HIGH RES (time step header = BOR + 20 halfwords + EOR)
 * LOW RES  (time step header = 10 words + Control Word)
 */
	if (radmformat == IEEE)
		k = (record_no - 1) * radmsteplen + 1 + 20 + 1;
	else
		k = (record_no - 1) * radmsteplen + 10 + 1;

/* for each column of data, read the column into the colbuf and process it */

	for (ix = 0; ((ix < radmncol) && (status == SUCCESS)); ++ix)
		{
		if (!get_radm_offset(k, &offset, message))
			status = FAILURE;
		else
			{
			if (!get_radm_buf(fd, colbuf, radmcollen, offset, message))	
				status = FAILURE;
			else
				{
				bindex = 0;  /* set input colbuf index to 0 */

				for (i = 0; ((i < radmnspecies) &&
					 (status == SUCCESS)); ++i)
					{
					for (j = 0; ((j < radmnlayer) && 
						 (status == SUCCESS)); ++j)
						{
						for (iy = 0; ((iy < radmnrow)
						 && (status == SUCCESS)); ++iy)
							{
							if (i == species_no-1)
								{
								if
					 (!radm_point(colbuf, bindex, &fbuf))
							status = FAILURE;
								else
									{

/* calculate output data field index and store input value into the field */

		findex = j * radmnrow * radmncol + iy * radmncol + ix;
							databuf[findex] = fbuf;
/*
		AVSinformation("[%2d][%2d][%2d]:[%4d]:%20.10f\n",
			ix, iy, j, findex, fbuf);
*/
									}
								}
							++bindex;
							}
						}
					}
				if (radmformat == IEEE)

					/* add BOR + COLUMN + EOR */
					k += 1 + radmcollen + 1;

				else
					/* add CW + COLUMN */
					k += radmcollen + 1;
				}
			}
		}
	free((char *) colbuf);
	}
sprintf(units, "%s", "PPM");
return(status);
}

