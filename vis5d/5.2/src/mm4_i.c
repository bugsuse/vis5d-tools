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

/***********************************************************/
/* Author:  Kathy Pearson, kathyp@mcnc.org, (919) 248-9240 */
/* Date:    March 22, 1994                                 */
/***********************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <fortran.h>

/* STUFF ADDED BY WLH 6-22-94 */

#define MAXLEVS 50

/* Table of sigma levels */
extern float mm4Sigma[MAXLEVS];




/* END OF STUFF ADDED BY WLH 6-22-94 */

/* ... definitions ... */


#define UNKNOWN 0			/* UNKNOWN file type */

#define cw_EOD 017                      /* control word END-OF-DATA */
#define cw_EOF 016                      /* control word END-OF-FILE */
#define cw_EOR 010                      /* control word END-OF-RECORD */

#define MM4_OUT 1			/* MM4 output file */
#define MM4_ZIGGY 2			/* MM4 ziggy output file*/

#define SUCCESS 1                       /* successful function return */
#define FAILURE 0                       /* unsuccessful function return */
#define IOERROR -1                      /* unsuccessful i/o function return */

#define MAXBUF 4096                     /* max # bytes for MM4 i/o buffer */
#define MAXSPEC 21			/* max # of species */

/* variables global to the MM4 routines only */

static int mm4nheaderwords;		/* words in initial header */
static int mm4date;			/* mm4 starting date */
static int mm4itgflg;			/* ground temp array present? */
static int mm4imoist;			/* cloud/rain water arrays present? */
static int mm4idry;			/* conv/stable precip arrays present? */
static int mm4ncol;			/* # columns in data grid */
static int mm4nrow;			/* # rows in data grid */
static int mm4nlayer;			/* # levels (layers) in grid */
static int mm4nrecord;			/* # records in file */
static int mm4nspecies;			/* # species in the input file */
static int PSTAR_species_no;		/* species number of PSTAR arry */
static int mm4n2d;			/* # 2d species in the input file */
static int mm4n3d;			/* # 3d species in the input file */
static int mm4headn2d;			/* # 2d species in the input file */
static int mm4filesize;			/* file size in bytes */
static int mm4filetype;			/* file type: CONC or other */
static int mm4levellen;			/* # floats in a layer of data */
static int mm4speclen;			/* # floats in a species of data */
static int mm4steplen;			/* # bytes per time step */
static int mm4ntext;			/* # text header records */
static int mm4reclen;			/* # bytes per MM4 record */
static int mm4datablock1;		/* MM4 starting data block # */
static int mm4datapos1;			/* MM4 starting data word position */
static int mm4hdatablock;		/* header starting data block # */
static int mm4hdatapos;			/* header starting data word position */
static int mm4ddatablock;		/* data starting data block # */
static int mm4ddatapos;			/* data starting data word position */
static char mm4specname[MAXSPEC*5+1];	/* short name of all species */ 
static int mm4nwords[MAXSPEC];		/* words for each species */
static float sw_utmx;			/* lower left map x-coordinate */
static float sw_utmy;			/* lower left map y-coordinate */
static float ne_utmx;			/* upper right map x-coordinate */
static float ne_utmy;			/* upper right map y-coordinate */

/*******************************************************************/
/* mm4_open                                                        */
/* Function: open input file and determine filesize and filetype   */
/* On error: return FAILURE and write error string into message    */
/* If no error: return SUCCESS                                     */
/*******************************************************************/
int mm4_open(fd, filename, message)
int *fd;				/* INPUT:  file descriptor */
char *filename;				/* INPUT:  file name */
char *message;				/* OUTPUT: returned error message */
{
struct stat statbuf;			/* UNIX file status buffer */
register int istat;			/* I/O status from UNIX stat call */
register int status;			/* mm4_open function return status */

status = FAILURE;			/* assume FAILURE */

if ((*fd = open(filename, O_RDONLY)) != IOERROR)
	{
	if ((istat = stat(filename, &statbuf)) != IOERROR)
		{
		mm4filesize = statbuf.st_size;
		if (mm4filesize > 0)
			{
			mm4filetype = get_mm4_filetype(*fd, message);
			if ((mm4filetype == MM4_OUT) 
				|| (mm4filetype == MM4_ZIGGY))
				status = SUCCESS;
			else
				{
				sprintf(message,
					 "File is not recognized as MM4"); 
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
/* get_mm4_filetype                                                   */
/* Function: read 1 words from MM4 file to determine filetype         */
/* On error: return UNKNOWN and write error string into message       */
/* If no error: return MM4_OUT or MM4_ZIGGY                           */
/**********************************************************************/
/* NOTE:  now this routine reads ONLY MM4OUT data                     */
/**********************************************************************/
int get_mm4_filetype(fd, message)
int fd;					/* INPUT:  file descriptor */
char *message;				/* OUTPUT: return error message */
{
static char buffer[32];

#define mm4out		"MMOUT   "	/* MM4 CRAY WORD 2 */
#define mm4ziggy 	"ZIGGY   " 	/* MM4 CRAY WORD 2 */

register int filetype;			/* return filetype */

filetype = UNKNOWN;		/* assume filetype cannot be determined */

if ((lseek(fd, (off_t) (2 * 8), SEEK_SET)) != IOERROR)
	{
	if ((read(fd, buffer, 8)) != IOERROR)
		{
		buffer[8] = '\0';
		if (!strcmp(buffer, mm4out))
			filetype = MM4_OUT;

/* Don't really check for ZIGGY, this is just an example ...
		else if (!strcmp(buffer, mm4ziggy)
			filetype = MM4_ZIGGY;
*/

		}
	else
		sprintf(message, "Cannot read filetype");
	}
else
	sprintf(message, "Cannot seek file position to determine filetype");
return(filetype);
}

/*******************************************************************/
/* mm4_inquire:                                                    */
/* Function: get header information                                */
/* On error: return FAILURE and write error string into message    */
/* If no error: return SUCCESS                                     */
/*******************************************************************/
int mm4_inquire(fd, message, ncol, nrow, nlayer, nrecord, nspecies, specname)
int fd;					/* INPUT:  file descriptor */
char *message;				/* OUTPUT: return message buffer */
int *ncol;				/* OUTPUT: number of columns */
int *nrow;				/* OUTPUT: number of rows */
int *nlayer;				/* OUTPUT: number of layers */
int *nrecord;				/* OUTPUT: number of records */
int *nspecies;				/* OUTPUT: number of species/arrays */
char *specname;				/* OUTPUT: species/array names */
{
if (mm4_header(fd, message))
	{
	*ncol = mm4ncol;
	*nrow = mm4nrow;
	*nlayer = mm4nlayer;
	*nrecord = mm4nrecord;
	*nspecies = mm4nspecies;
	sprintf(specname, "%s", mm4specname);
	return(SUCCESS);
	}
return(FAILURE);
}

/*******************************************************************/
/* get_mm4_offset                                                  */
/* Function: get the file offset for nwords input                  */
/*	     (nwords = CRAY whole words for low resolution files ) */
/* On error: return FAILURE and write error string into message    */
/* If no error: return SUCCESS                                     */
/*******************************************************************/
int get_mm4_offset(nwords, offset, message)
int nwords;		/* INPUT:  # of words beyond header */ 
off_t *offset;		/* OUTPUT: file offset in bytes */
char *message;		/* OUTPUT: message buffer for error string */
{
register int status;

status = get_unicos_mm4_offset(nwords, offset);
if (status == FAILURE)
	sprintf(message, "Error setting file offset");
return(status);
}

/*******************************************************************/
/* get_mm4_buf                                                     */
/* Function: read nwords (CRAY words) into 'buffer',               */
/*           reading from file 'fd' at 'offset' position.          */
/* On error: return FAILURE and write error string into message    */
/* If no error: return SUCCESS                                     */
/*******************************************************************/
int get_mm4_buf(fd, buffer, nwords, offset, message)
int fd;					/* INPUT:  file descriptor */
char *buffer;				/* OUTPUT: character data buffer */
int nwords;				/* INPUT:  # words/halfwords */
off_t offset;				/* INPUT:  file offset in bytes */
char *message;				/* OUTPUT: buffer for error string */
{
register int status;

status = get_unicos_mm4_buf(fd, buffer, nwords, offset);
if (status == FAILURE)
	sprintf(message, "Error reading data");
return(status);
}


/*******************************************************************/
/* get_unicos_mm4_offset                                           */
/* Function: get the file offset for nwords input                  */
/*	     (nwords = CRAY whole words for MM4 files )            */
/* On error: return FAILURE                                        */
/* If no error: return SUCCESS                                     */
/*******************************************************************/
int get_unicos_mm4_offset(nwords, offset)
int nwords;			/* INPUT:  # 8-byte CRAY words offset */
off_t *offset;			/* OUTPUT: file offset in bytes */
{
register int twords;		/* # words yet to count toward offset */
register int pos;		/* word offset position within offset block */
register int block;		/* 512-word block containing offset pos */
register int status;		/* return status */

status = SUCCESS;		/* assume SUCCESS */

/* begin offset search at EOR just beyond last header record */

block = mm4datablock1;
pos = mm4datapos1;

twords = nwords;		/* # words to count */
while (twords + pos > 511)	/* if # words to count span blocks ... */
	{
	++block;		/* . increment block # */
	twords -= (511 - pos);	/* . decrement # words by block remainder */
	pos = 0;		/* . reset position within new block */
	}
pos += twords;			/* set position to # words remaining */
*offset = (block * 512 + pos) * 8;	/* convert word offset to bytes */
if (*offset >= mm4filesize)	/* if offset is beyond EOF return with error */
	status = FAILURE;
return(status);
}

/*******************************************************************/
/* get_unicos_mm4_buf                                              */
/* Function: read nwords (CRAY words) into 'buffer',               */
/*           reading from file 'fd' at 'offset' position           */
/* On error: return FAILURE                                        */
/* If no error: return SUCCESS                                     */
/*******************************************************************/
int get_unicos_mm4_buf(fd, buffer, nwords, offset)
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
/*
		if ((type != cw_EOD) && (type != cw_EOF))

MM4 files have EOF marks WITHIN them, treated as ordinary CRAY control types,
        so it's not REALLY end of file!
*/

		if (1)
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

/*********************************************************************/
/* MM4  Header Routine                                               */
/*********************************************************************/
int mm4_header(fd, message)
int fd;
char *message;		/* OUTPUT: return error string */
{
off_t offset;		/* file offset in bytes */
register int status;	/* routine return status */
register int i, j, k;	/* local loop variables */
register int index;	/* index into CRAY word buffer */
register int nlv, ioldnlv;	/* array indices */
static char buffer[MAXBUF];
static char string[32];

int *ibuf = NULL;	/* integer value */
float *fbuf = NULL;	/* floating point value */

status = SUCCESS;	/* assume SUCCESS */

index = 0;

offset = 0;
k = 4;
if (get_mm4_buf(fd, buffer, k, offset, message))
	{
	ibuf = (int *) (buffer + index);	/* date */
/*
	fprintf(stderr, "date = %d\n", *ibuf);
*/
	mm4date = *ibuf;
	index += 8;
 
	for (i = 0; i < 8; ++i)
        	string[i] = buffer[index+i];
	string[i] = 0;
/*
	fprintf(stderr, "program = %s\n", string);
*/
	index += 8;
 
	for (i = 0; i < 8; ++i)
        	string[i] = buffer[index+i];
	string[i] = 0;
/*
	fprintf(stderr, "coord = %s\n", string);
*/
	index += 8;
 
	ibuf = (int *) (buffer+index);
/*
	fprintf(stderr, "nlv = %d\n", *ibuf);
*/
	index += 8;
	nlv = *ibuf;
	mm4nlayer = nlv - 1;
	}

else
	{
	sprintf(message, "Error reading file header record.");
	status = FAILURE;
	}

/* get buffer again with correct nlv values for vcoord */
/* then 30 MIF, 10 MRF, 10 MLF, 18 (JYR, JMO, JDY, JHR), IOLDPROG, IOLDCORD,
   IOLNLV */

k += nlv + 30 + 10 + 10 + 4 * 18 + 3;
offset = 0;
if ((status == SUCCESS) && (get_mm4_buf(fd, buffer, k, offset, message)))
	{
	for (i = 0; i < nlv; ++i)
		{
		fbuf = (float *) (buffer+index);
/*
		fprintf(stderr, "vcoord[%d] = %g\n", i, *fbuf);
*/

/* ADDED BY WLH 6-22-94 - note levels are flipped */
		if (i < MAXLEVS) mm4Sigma[(nlv - 1) - i] = *fbuf;
/* END OF ADDED BY WLH 6-22-94 */

		index += 8;
		}
	for (i = 0; i < 30; ++i)
		{
		ibuf = (int *) (buffer+index);
/*
		fprintf(stderr, "mif[%d] = %d\n", i, *ibuf);
*/
		if (i == 0)
			mm4nrecord = *ibuf;
		else if (i == 1)
			mm4nrow = *ibuf;
		else if (i == 2)
			mm4ncol = *ibuf;
		else if (i == 16)
			{
			for (j = 0; j < 8; ++j)
        			string[j] = buffer[index+j];
			string[j] = 0;
/* -- need this info to create NCAR map
			fprintf(stderr, "projection = %s\n", string);
*/
			}

		index += 8;
		}
	for (i = 0; i < 10; ++i)
		{
		fbuf = (float *) (buffer+index);
/*
		fprintf(stderr, "mrf[%d] = %g\n", i, *fbuf);
*/
/* -- need this info to create NCAR map

		if (i == 1)
			fprintf(stderr, "central lat = %g\n", *fbuf);
		if (i == 2)
			fprintf(stderr, "central lon = %g\n", *fbuf);
*/
		index += 8;
		}
	for (i = 0; i < 10; ++i)
		{
		ibuf = (int *) (buffer+index);
/*
		fprintf(stderr, "mlf[%d] = %d\n", i, *ibuf);
*/
		index += 8;
		}
	for (i = 0; i < 18; ++i)
		{
		ibuf = (int *) (buffer+index);
/*
		fprintf(stderr, "jyr[%d] = %d\n", i, *ibuf);
*/
		index += 8;
		}
	for (i = 0; i < 18; ++i)
		{
		ibuf = (int *) (buffer+index);
/*
		fprintf(stderr, "jmo[%d] = %d\n", i, *ibuf);
*/
		index += 8;
		}
	for (i = 0; i < 18; ++i)
		{
		ibuf = (int *) (buffer+index);
/*
		fprintf(stderr, "jda[%d] = %d\n", i, *ibuf);
*/
		index += 8;
		}
	for (i = 0; i < 18; ++i)
		{
		ibuf = (int *) (buffer+index);
/*
		fprintf(stderr, "jhr[%d] = %d\n", i, *ibuf);
*/
		index += 8;
		}
	 
	ibuf = (int *) (buffer+index);
/*
	fprintf(stderr, "ioldprog = %d\n", i, *ibuf);
*/
	index += 8;
	ibuf = (int *) (buffer+index);
/*
	fprintf(stderr, "ioldcord = %d\n", i, *ibuf);
*/
	index += 8;
	 
	ibuf = (int *) (buffer+index);
/*
	fprintf(stderr, "ioldnlv = %d\n", i, *ibuf);
*/
	index += 8;
	ioldnlv = *ibuf;
	}

else
	{
	sprintf(message, "Error reading file header record.");
	status = FAILURE;
	}

/* get buffer again with correct ioldnlv values for ivcoord */
 
k += ioldnlv + 15;
mm4nheaderwords = k;
offset = 0;
if ((status == SUCCESS) && (get_mm4_buf(fd, buffer, k, offset, message)))
	{

	for (i = 0; i < ioldnlv; ++i)
		{
		ibuf = (int *) (buffer+index);
/*
		fprintf(stderr, "ivcoord[%d] = %d\n", i, *ibuf);
*/
		index += 8;
		}
	 
	ibuf = (int *) (buffer+index);
/*
	fprintf(stderr, "ibltyp = %d\n", *ibuf);
*/
	index += 8;
	 
	ibuf = (int *) (buffer+index);
/*
	fprintf(stderr, "isfflx = %d\n", *ibuf);
*/
	index += 8;
	 
	ibuf = (int *) (buffer+index);
/*
	fprintf(stderr, "itgflg = %d\n", *ibuf);
*/
	mm4itgflg = *ibuf;
	index += 8;
	 
	ibuf = (int *) (buffer+index);
/*
	fprintf(stderr, "icdcon = %d\n", *ibuf);
*/
	index += 8;

	ibuf = (int *) (buffer+index);
/*
	fprintf(stderr, "isfpar = %d\n", *ibuf);
*/
	index += 8;
	 
	ibuf = (int *) (buffer+index);
/*
	fprintf(stderr, "ivmixm = %d\n", *ibuf);
*/
	index += 8;
	 
	ibuf = (int *) (buffer+index);
/*
	fprintf(stderr, "idry = %d\n", *ibuf);
*/
	mm4idry = *ibuf;
	index += 8;
	 
	ibuf = (int *) (buffer+index);
/*
	fprintf(stderr, "imoist = %d\n", *ibuf);
*/
	mm4imoist = *ibuf;
	index += 8;
	 
	ibuf = (int *) (buffer+index);
/*
	fprintf(stderr, "icloud = %d\n", *ibuf);
*/
	index += 8;
	 
	ibuf = (int *) (buffer+index);
/*
	fprintf(stderr, "iboudy = %d\n", *ibuf);
*/
	index += 8;

	ibuf = (int *) (buffer+index);
/*
	fprintf(stderr, "imoiav = %d\n", *ibuf);
*/
	index += 8;
	 
	ibuf = (int *) (buffer+index);
/*
	fprintf(stderr, "ifsnow = %d\n", *ibuf);
*/
	index += 8;
	 
	ibuf = (int *) (buffer+index);
/*
	fprintf(stderr, "icustb = %d\n", *ibuf);
*/
	index += 8;
	 
	ibuf = (int *) (buffer+index);
/*
	fprintf(stderr, "itqpbl = %d\n", *ibuf);
*/
	index += 8;
	 
	ibuf = (int *) (buffer+index);
/*
	fprintf(stderr, "ifrad = %d\n", *ibuf);
*/
	index += 8;
	 
/*
	fprintf(stderr, "ix = %d, kx = %d, jx = %d, nstep = %d\n", 
		mm4ncol, mm4nrow, mm4nlayer, mm4nrecord);
*/
	}
else
	{
	sprintf(message, "Error reading file header record.");
	status = FAILURE;
	}

/* This is an initial guess about species based on the header values of
imoist, idry, itgflg.  This module assumes that these values will NOT
change from time step to time step!
*/

/** header species **/

sprintf(mm4specname, "%s", "TER :XMFC:XMFD:F   :");
mm4nspecies = 4;	/* always TER, XMFC, XMFD, F */
if (mm4itgflg != 3)
	{
	sprintf(mm4specname+mm4nspecies*5, "%s", "TRES:");
	++mm4nspecies;
	}
sprintf(mm4specname+mm4nspecies*5, "%s", "LATC:LONC:LAND:SNOW:");
mm4nspecies += 4;	/* always XLATC, XLONC, XLAND, SNOW */

mm4headn2d = mm4nspecies;	/* all 2D */
for (i = 0; i < mm4nspecies; ++i)
	mm4nwords[i] =  mm4ncol * mm4nrow + 1;


/** time varying species **/

/* always U, V, T */
mm4n2d = 0;
mm4n3d = 3;

mm4nwords[mm4nspecies+0] = mm4ncol * mm4nrow * mm4nlayer + 1;
mm4nwords[mm4nspecies+1] = mm4ncol * mm4nrow * mm4nlayer + 1;
mm4nwords[mm4nspecies+2] = mm4ncol * mm4nrow * mm4nlayer + 1;

sprintf(mm4specname+5*mm4nspecies, "%s", "U   :V   :T   :");
mm4nspecies += 3;

if (mm4idry != 1)
	{
	sprintf(mm4specname+5*mm4nspecies, "%s", "Q   :");
	mm4nwords[mm4nspecies] = mm4ncol * mm4nrow * mm4nlayer + 1;
	++mm4nspecies;
	++mm4n3d;
	}
if ((mm4imoist == 2) || (mm4imoist == 4))
	{
	sprintf(mm4specname+mm4nspecies*5, "%s", "CLW :RNW :");
	mm4nwords[mm4nspecies] = mm4ncol * mm4nrow * mm4nlayer + 1;
	mm4nwords[mm4nspecies+1] = mm4ncol * mm4nrow * mm4nlayer + 1;
	mm4nspecies += 2;
	mm4n3d += 2;
	}
sprintf(mm4specname+mm4nspecies*5, "%s", "PS  :");
mm4nwords[mm4nspecies] = mm4ncol * mm4nrow + 1;
++mm4nspecies;		/* alway PS */
++mm4n2d;
PSTAR_species_no = mm4nspecies;

if (mm4itgflg != 3)
	{
	sprintf(mm4specname+mm4nspecies*5, "%s", "TGD :");
	mm4nwords[mm4nspecies] = mm4ncol * mm4nrow + 1;
	++mm4nspecies;
	++mm4n2d;
	}
if (mm4idry != 1)
	{
	sprintf(mm4specname+mm4nspecies*5, "%s", "RNC :RNN :");
	mm4nwords[mm4nspecies] = mm4ncol * mm4nrow + 1;
	mm4nwords[mm4nspecies+1] = mm4ncol * mm4nrow + 1;
	mm4nspecies += 2;
	mm4n2d += 2;
	}
mm4specname[mm4nspecies*5-1] = '\0';

/* data arrays start here ... */

i = mm4nheaderwords + 1 + mm4headn2d * (mm4ncol * mm4nrow + 1);
j = i/512;
k = i - (j * 511);	/* 1st word always a control word */
mm4datablock1 = mm4ddatablock = j;
mm4datapos1 = mm4ddatapos = k;

/* header data arrays start here ... */

i = mm4nheaderwords + 1;
j = i/512;
k = i - (j * 511);	/* 1st word always a control word */
mm4hdatablock = j;
mm4hdatapos = k;

/* header record (BOR + 6 values + EOR) 
	+ (2d data size + EOR) + (3d data size + EOR) */

mm4steplen = 8 + (mm4ncol * mm4nrow  + 1) * mm4n2d +
	(mm4ncol * mm4nrow * mm4nlayer + 1) * mm4n3d;

return(status);
}

/*********************************************************************/
/* mm4_get_date							     */
/*********************************************************************/
int mm4_get_date(fd, record_no, datestr, message)
int fd;				/* INPUT:  file descriptor */
int record_no;			/* INPUT:  record number */
char *datestr;			/* OUTPUT: date string */
char *message;			/* OUTPUT: return error message */
{
off_t offset;			/* offset in bytes */
int header_year;		/* year */
int header_month;		/* month */
int header_day;			/* day */
int header_hour;		/* hour */
float *starthour = NULL;	/* starting hour of MM4 run */
static char buffer[64];		/* character buffer for date */	
register int i;			/* local variable */
register int status;		/* return status */

status = SUCCESS;

i = (record_no - 1) * mm4steplen;
if (!get_mm4_offset(i, &offset, message))
	{
	sprintf(message, "Cannot get record offset for date.");
	status = FAILURE;
	}
else
	{
	i = offset/4096;
/*
	fprintf(stderr, "offset = %d, %d:%d\n", offset,
		i, (offset - (i * 4096))/8);
*/
	if (get_mm4_buf(fd, buffer, 1, offset, message))
		{
		starthour = (float *) (buffer);
                header_year = mm4date/1000000;
                header_month = (mm4date - (header_year * 1000000))/10000;
		header_day = (mm4date - (header_year * 1000000) -
			(header_month * 10000))/100;
		header_hour = (mm4date - (header_year * 1000000) -
			(header_month * 10000) - (header_day * 100))/100;
		header_hour = header_hour + ((int) (*starthour + .5));

/*
		fprintf(stderr, "mm4date = %d\n", mm4date);
		fprintf(stderr, "starthour = %g\n", *starthour);
		fprintf(stderr, "yr:%d, mo:%d, da:%d, hr:%d\n",
			header_year, header_month, header_day, header_hour);
*/
		mm4date2std(header_year + 1900, header_month, header_day,
			 header_hour, 0, 0, "GMT", datestr);
		}
	else
		{
		sprintf(message, "Cannot get buffer for date.");
		status = FAILURE;
		}
	}
return(status);
}

/*********************************************************************/
/* mm4_get_data							     */
/*********************************************************************/
int mm4_get_data(fd, record_no, species_no, databuf, species, units, message)
int fd;				/* INPUT:  file descriptor */
int record_no;			/* INPUT:  record number */
int species_no;			/* INPUT:  species number */
float *databuf;			/* OUTPUT: data buffer */
char *species;			/* OUTPUT: species/array name */
char *units;			/* OUTPUT: units label name for each species */
char *message;			/* OUTPUT: return error message */
{

char *speciesbuf = NULL;        /* character data layer buffer */
off_t offset;                   /* file offset in bytes */
float *fbuf = NULL;             /* floating point data value */
unsigned int *cw = NULL;	/* cray word used to check high order bits */
register int i, j, k;		/* local variables */
register int n;			/* index into input data buffer */
register int findex;            /* index into output field layerbuf */
register int ix, iy, iz;        /* local loop variables for grid */
register int status;            /* routine return status */
register int nz;		/* number of z layers for species */
register int header_species;	/* data from header? */
register int pstar;		/* should values be divided by pstar? */
 
status = SUCCESS;

if (!strcmp(species, "U   "))
	{
	sprintf(units, "%s", "m/s");
	nz = mm4nlayer;
	header_species = 0;
	pstar = 1;
	}
else if (!strcmp(species, "V   "))
	{
	sprintf(units, "%s", "m/s");
	nz = mm4nlayer;
	header_species = 0;
	pstar = 1;
	}
else if (!strcmp(species, "T   "))
	{
	sprintf(units, "%s", "K");
	nz = mm4nlayer;
	header_species = 0;
	pstar = 1;
	}
else if (!strcmp(species, "Q   "))
	{
	sprintf(units, "%s", "kg/kg");
	nz = mm4nlayer;
	header_species = 0;
	pstar = 1;
	}
else if (!strcmp(species, "CLW "))
	{
	sprintf(units, "%s", "kg/kg");
	nz = mm4nlayer;
	header_species = 0;
	pstar = 1;
	}
else if (!strcmp(species, "RNW "))
	{
	sprintf(units, "%s", "kg/kg");
	nz = mm4nlayer;
	header_species = 0;
	pstar = 1;
	}
else if (!strcmp(species, "PS  "))
	{
	sprintf(units, "%s", "cb");
	nz = 1;
	header_species = 0;
	pstar = 0;
	}
else if (!strcmp(species, "TGD "))
	{
	sprintf(units, "%s", "K");
	nz = 1;
	header_species = 0;
	pstar = 0;
	}
else if (!strcmp(species, "RNC "))
	{
	sprintf(units, "%s", "cm");
	nz = 1;
	header_species = 0;
	pstar = 0;
	}
else if (!strcmp(species, "RNN "))
	{
	sprintf(units, "%s", "cm");
	nz = 1;
	header_species = 0;
	pstar = 0;
	}
else if (!strcmp(species, "TER "))
	{
	sprintf(units, "%s", "m");
	nz = 1;
	header_species = 1;
	pstar = 0;
	}
else if (!strcmp(species, "XMFC"))
	{
	sprintf(units, "%s", " ");
	nz = 1;
	header_species = 1;
	pstar = 0;
	}
else if (!strcmp(species, "XMFD"))
	{
	sprintf(units, "%s", " ");
	nz = 1;
	header_species = 1;
	pstar = 0;
	}
else if (!strcmp(species, "F   "))
	{
	sprintf(units, "%s", "1/s");
	nz = 1;
	header_species = 1;
	pstar = 0;
	}
else if (!strcmp(species, "TRES"))
	{
	sprintf(units, "%s", "K");
	nz = 1;
	header_species = 1;
	pstar = 0;
	}
else if (!strcmp(species, "LATC"))
	{
	sprintf(units, "%s", "degrees");
	nz = 1;
	header_species = 1;
	pstar = 0;
	}
else if (!strcmp(species, "LONC"))
	{
	sprintf(units, "%s", "degrees");
	nz = 1;
	header_species = 1;
	pstar = 0;
	}
else if (!strcmp(species, "LAND"))
	{
	sprintf(units, "%s", "use");
	nz = 1;
	header_species = 1;
	pstar = 0;
	}
else if (!strcmp(species, "SNOW"))
	{
	sprintf(units, "%s", " ");
	nz = 1;
	header_species = 1;
	pstar = 0;
	}
if ((header_species) && (record_no > 1))
	{
	sprintf(message, "Header species is available ONLY for record 1.");
	return(FAILURE);
	}
	
k = mm4ncol * mm4nrow * nz;

if (	((speciesbuf = ((char *) malloc(sizeof(char) * k * 8))) != NULL)  )
	{
	if (header_species)
		{
		mm4datablock1 = mm4hdatablock;
		mm4datapos1 = mm4hdatapos;
/*
		fprintf(stderr, "header %d:%d\n",
			mm4datablock1, mm4datapos1);
*/
		i = 0;
		for (j = 1; j < species_no; ++j)
			if (j < mm4headn2d)
				i += mm4nwords[j-1];
		}
	else
		{
		i = (record_no - 1) * mm4steplen + 8;
		for (j = 1; j < species_no; ++j)
			if (j > mm4headn2d)
				i += mm4nwords[j-1];
/*
		fprintf(stderr, "species_no = %d\n", species_no);
*/
		}

	if (get_mm4_offset(i, &offset, message))
		{
		i = offset/4096;
/*
		fprintf(stderr, "got offset = %d, %d:%d\n", offset,
			i, (offset - (i * 4096))/8);
*/
		if (get_mm4_buf(fd, speciesbuf, k, offset, message))
			{
/*
		fprintf(stderr, "got buffer for cols %d, rows %d, layers %d\n",
			mm4ncol, mm4nrow, nz);
*/

			n = 0;
			for (iz = 0; iz < nz; ++iz)
/* in mm4, ix 1st dim = Y direction, jx 2nd dim = X direction */
				{ 
				for (ix = 0; ix < mm4ncol; ++ix)
					for (iy = 0; iy < mm4nrow; ++iy)
						{
					fbuf = (float *) (speciesbuf + n);
					cw = (unsigned int *) (speciesbuf + n);
					n += 8;
					findex = ix + iy * mm4ncol + iz * mm4ncol * mm4nrow;
					if (((*cw) >> 57) == 060)
						databuf[findex] = 0.0;
					else
						databuf[findex] = *fbuf;
/* -- need this info for LATC, LONC to get corner grid values for map --
			if ((ix == 0) && (iy == 0) && (iz == 0))
	fprintf(stderr, "%s[%d][%d][%d] = %g\n", species, ix, iy, iz, *fbuf);
			if ((ix == mm4ncol-1) && (iy == mm4nrow-1))
	fprintf(stderr, "%s[%d][%d][%d] = %g\n", species, ix, iy, iz, *fbuf);
*/
						}
				}
			for (iz = nz; iz < mm4nlayer; ++iz)
				{
				for (iy = 0; iy < mm4nrow; ++iy)
					for (ix = 0; ix < mm4ncol; ++ix)
						{
					findex = ix + iy * mm4ncol + iz * mm4ncol * mm4nrow;
						databuf[findex] = 0.0;
						}
				}

			}
		else
			{
			sprintf(message, "Error reading data buffer.");
			status = FAILURE;
			}
		}
	else
		{
		sprintf(message, "Error getting data offset.");
		status = FAILURE;
		}
	free ((char *) speciesbuf);
	}
else
	{
	sprintf(message, "Error allocating data buffer.");
	status = FAILURE;
	}
if (header_species)	/* restore initial position */
	{
	mm4datablock1 = mm4ddatablock;
	mm4datapos1 = mm4ddatapos;
	}
if (pstar)
	status = pstar_div(fd, record_no, databuf, species, message);
return(status);
}

/*********************************************************************/
/* pstar_div							     */
/*********************************************************************/
/* stuff pstar data into all Z layers to make for a simple divide!   */
/*********************************************************************/

pstar_div(fd, record_no, databuf, species, message)
int fd;				/* INPUT:  file descriptor */
int record_no;			/* INPUT:  record number */
float *databuf;			/* OUTPUT: data buffer */
char *species;			/* INPUT:  species/array name */
char *message;			/* OUTPUT: error message */
{
float *pstarbuf = NULL;		/* buffer for 3 layers of Pstar float values */
char *speciesbuf = NULL;	/* buffer to hold pstar input values read */
float *fbuf = NULL;		/* one floating point value from databuf */
off_t offset;                   /* file offset in bytes */
register int i, j, k, n;	/* local variables */
register int ix, iy, iz;	/* local grid variables */
unsigned int *cw = NULL;	/* cray word used to check high order bits */
register int findex;            /* index into databuf */
register int status;            /* routine return status */
 
status = SUCCESS;

k = mm4ncol * mm4nrow * mm4nlayer;
if (	((speciesbuf = ((char *) malloc(sizeof(char) * k * 8))) != NULL) && 
	((pstarbuf = ((float *) malloc(sizeof(float) * k))) != NULL)  )
	{
/*
	fprintf(stderr, "speciesbuf & pstarbuf allocated\n");
*/
	i = (record_no - 1) * mm4steplen + 8;
	for (j = 1; j < PSTAR_species_no; ++j)
		if (j > mm4headn2d)
			i += mm4nwords[j-1];

/*
	fprintf(stderr, "pstar species no = %d, words offset = %d\n",
		PSTAR_species_no, i);
*/
	if (get_mm4_offset(i, &offset, message))
		{
		i = offset/4096;
/*
		fprintf(stderr, "got offset = %d, %d:%d\n", offset,
			i, (offset - (i * 4096))/8);
*/
		j = mm4ncol * mm4nrow;	/* data input is only 2d */
		if (get_mm4_buf(fd, speciesbuf, j, offset, message))
			{
/*
		fprintf(stderr, "got buffer for cols %d, rows %d, layers %d\n",
			mm4ncol, mm4nrow, 1);
*/
			n = 0;
			for (ix = 0; ix < mm4ncol; ++ix)
				{
				for (iy = 0; iy < mm4nrow; ++iy)
					{
					fbuf = (float *) (speciesbuf + n);
					cw = (unsigned int *) (speciesbuf + n);
					n += 8;
					for (iz = 0; iz < mm4nlayer; ++iz)
						{
						findex = ix + iy * mm4ncol +
							iz * mm4ncol * mm4nrow;
						if (((*cw) >> 57) == 060)
							pstarbuf[findex] = 0.0;
						else
							pstarbuf[findex] = *fbuf;
						}
					}
				}
			if (!strcmp(species, "U   "))
				{
				if (!pstar_avg(databuf, message))
					status = FAILURE;
				}
			if (!strcmp(species, "V   "))
				{
				if (!pstar_avg(databuf, message))
					status = FAILURE;
				}
			for (i = 0; ((i < k) && (status == SUCCESS)); ++i)
				{
				if (pstarbuf[i] != 0.0)
					databuf[i] = databuf[i]/pstarbuf[i];
				else
					databuf[i] = 0.0;
				}
			}
		else
			{
			sprintf(message, "Error reading PSTAR data buffer.");
			status = FAILURE;
			}
		}
	else
		{
		sprintf(message, "Error getting data PSTAR offset.");
		status = FAILURE;
		}
	free ((char *) speciesbuf);
	free ((char *) pstarbuf);
	}
else
	{
	sprintf(message, "Error allocating PSTAR buffer.");
	status = FAILURE;
	}
return(status);
}

/*********************************************************************/
/* pstar_avg							     */
/*********************************************************************/
int pstar_avg(databuf, message)
float *databuf;			/* OUTPUT: data buffer averaged */
char *message;			/* OUTPUT: return error message */
{
register int ix, iy, iz;	/* local grid index variables */
register i, k;			/* local variables */
register int aindex;		/* index into averaged buffer */
register index1, index2, index3, index4;	/* index for each avg element */
register int status;		/* return status */

/* A separate avg buffer is needed since values cannot be replaced with their
   averages until ALL are calculated -- at least for a single Z plane */

float *avgbuf = NULL;		/* new average buffer to replace databuf */ 

#define D1 databuf[index1]
#define D2 databuf[index2]
#define D3 databuf[index3]
#define D4 databuf[index4]

status = SUCCESS;
k = mm4ncol * mm4nrow * mm4nlayer;
if (	((avgbuf = ((float *) malloc(sizeof(float) * k))) != NULL)	)
	{
	for (iz = 0; iz < mm4nlayer; ++iz)

	/* in mm4, ix 1st dim = Y direction, jx 2nd dim = X direction */

		{ 
		for (ix = 0; ix < mm4ncol; ++ix)
			{
			for (iy = 0; iy < mm4nrow; ++iy)
				{

	/* for each element in the Z plane, calculate an average cross point
	   from 4 surrounding dot points; zero final row/column */
				
				aindex = ix + iy * mm4ncol +
					iz * mm4ncol * mm4nrow;
				if ((ix == mm4ncol - 1) || (iy == mm4nrow -1))
					avgbuf[aindex] = 0.0;
				else
					{
 
					index1 = ix + iy * mm4ncol +
						iz * mm4ncol * mm4nrow;
					index2 = (ix + 1) + iy * mm4ncol +
						iz * mm4ncol * mm4nrow;
					index3 = ix + (iy + 1) * mm4ncol +
						iz * mm4ncol * mm4nrow;
					index4 = (ix + 1) + (iy + 1) * mm4ncol +
						iz * mm4ncol * mm4nrow;
					avgbuf[aindex] = (D1 + D2 + D3 + D4)/
							4.0;
					}
				}
			}
		}

	/* now, replace databuf with avgbuf */

	for (i = 0; i < k; ++i)
		databuf[i] = avgbuf[i];
	free ((char *) avgbuf);
	}
else
	{
	sprintf(message, "Error allocating average buffer.");
	status = FAILURE;
	}
return(status);
}

