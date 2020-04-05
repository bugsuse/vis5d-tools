/*
c    <picogrib software di decodifica grib>
c    Copyright (C) 2000  <SMR ARPA e Picodata SRL>
c
c    $Date: 2007-02-23 13:07:07 +0100 (ven, 23 feb 2007) $    $Revision: 61 $
c    $Id: griblib.c 61 2007-02-23 12:07:07Z cesari $

c    Originally written by Marco Mazzola and Massimo Bider
c    Picodata SRL mailto:m.bider@picodata.it

c    Revised by Davide Cesari and Paolo Patruno
c    ARPA SMR 
c    mailto:p.patruno@smr.arpa.emr.it
c    mailto:dinamici@smr.arpa.emr.it

c    Questo programma ÅË software  libero; ÅË lecito ridistribuirlo e/o
c    modificarlo secondo i  termini della Licenza Pubblica Generica SMR 
c    come pubblicata  da ARPA SMR ; riferirsi alla versione 1
c    della licenza o (a scelta) ad una versione successiva.

c    Questo programma ÅË distribuito  nella speranza che sia utile,  ma
c    SENZA  ALCUNA GARANZIA;  senza  neppure la  garanzia  implicita di
c    COMMERCIABILITA' o di APPLICABILITA' PER UN PARTICOLARE SCOPO.  Si
c    veda la Licenza Pubblica Generica SMR per avere maggiori dettagli.

c    Ognuno dovrebbe avere ricevuto una copia della Licenza Pubblica
c    Generica SMR insieme a questo programma; in caso contrario, la si
c    puÅÚ ottenere da Agenzia Regionale Prevenzione e Ambiente (ARPA)
c    Servizio Meteorologico Regionale (SMR), Viale Silvani 6, 40122
c    Bologna, Italia
c    http://www.smr.arpa.emr.it
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "constant.h"
#include "struct.h"



/* The next four functions read bytes, starting from the address *buffer_in.
   This address is passed by reference so it can be moved on the next byte
   to be read */

/* Read 'bytes_num' bytes from 'buffer_in' and write them in 'buffer_out' */ 
void read_bytes_in_bytes(
unsigned char **buffer_in,   /* address of input buffer (by reference) */
int             bytes_num,   /* number of bytes to copy */
char          *buffer_out)   /* output buffer where bytes have to be copied */
{
  memcpy(buffer_out,*buffer_in, bytes_num);
  *buffer_in += bytes_num;
}


/* Read 'bytes_num' bytes from 'buffer_in' and write them in 'buffer_out' 
   as an array of integer */ 
void read_bytes_in_ints(buffer_in,bytes_num,buffer_out)
unsigned char **buffer_in;   /* address of input buffer (by reference) */
int             bytes_num;   /* number of bytes to copy */
gribint       *buffer_out;   /* output buffer where integers have to be 
				written */
{for (;bytes_num>=1;bytes_num--)
  {*buffer_out = **buffer_in;
  ++(*buffer_in);
  ++buffer_out;}
}


/* Read an integer of 'bytes_num' bytes (<=4) from 'buffer_in' and 
   write it in value */ 
void read_var_len_int(
unsigned char **buffer_in,   /* address of input buffer (by reference) */
int bytes_num,               /* size in bytes of the integer number to 
				be read */
gribint *value)             /* integer destination variable */
{
  *value = 0;
  for (;bytes_num>=1;bytes_num--)
    {*value = (*value << 8) | **buffer_in;
    ++(*buffer_in);}
}


/* Read a signed gribinteger in the GRIB 2/3-octet format from 'buffer_in' and 
   write it in value */ 
void read_var_len_sint(
unsigned char **buffer_in,   /* address of input buffer (by reference) */
int bytes_num,               /* size in bytes of the integer number to 
				be read */
gribint *value)             /* integer destination variable */
{
  read_var_len_int(buffer_in,bytes_num,value);
  if (bytes_num == 3) {
    if (*value & 0x800000)
      {*value &= 0x7FFFFF;
      *value = - (*value);}
  }
  else if (bytes_num == 2) {
    if ((*value & 0xFFFF) == 0xFFFF)
      {*value = 0xFFFF;
      *value = - (*value);}
  }
}


/* Convert exponent and mantissa from the grib internal float representation
   (IBM?) to machine double */
float decode_float(gribint sign_and_exponent, gribint mantissa)
{
  return pow((double) -1, (double) (sign_and_exponent >> 7)) *
    mantissa * pow((double) 2,(double) -24) *
    pow((double) 16,(double) ((sign_and_exponent & 0x7F) - 64));
}


/* Load a GRIB from the file to 'grib_buffer' */
gribint load_grib_file(
FILE *grib_file,             /* input file pointer */
unsigned char **grib_buffer) /* buffer where bytes have to be copied */
{
  unsigned char aux_section0[8];
  unsigned char searched_byte;
  int grib_length;
  int i;
  int file_byte;

  file_byte = '.';
  searched_byte = 'G';
  while ( (file_byte != EOF) && (searched_byte != '*') )
    {file_byte = fgetc(grib_file);
    if (file_byte != searched_byte) searched_byte = 'G';
    if (file_byte == searched_byte)
      {
	switch (file_byte)
	  {
	  case 'G' : searched_byte = 'R'; break;
	  case 'R' : searched_byte = 'I'; break;
	  case 'I' : searched_byte = 'B'; break;
	  case 'B' : searched_byte = '*'; break;
	  }
      }
    }
  if (file_byte == EOF)  return ABORT_STATUS;

  memcpy(aux_section0, "GRIB", 4);
  i = fread(&aux_section0[4],sizeof(unsigned char),4,grib_file);
  grib_length = 0;
  for (i=4;i<=6;i++)
    grib_length = (grib_length << 8) + aux_section0[i];

  /* allocate memory for reading the grib file */
  *grib_buffer = (unsigned char *) malloc(grib_length);
  if (*grib_buffer == NULL) return ABORT_STATUS;

  /* store section0 */
  memcpy(*grib_buffer,aux_section0,8);

  /* read the remainder of the grib file */
  i = fread((*grib_buffer)+8,sizeof(unsigned char),grib_length-8,grib_file);

  for (i=1;i<=4;++i)
    if (*((*grib_buffer)+grib_length-i) != '7')
      {free(*grib_buffer);
      return ABORT_STATUS;}
  return NORMAL_STATUS;
}


/* Save a GRIB from 'grib_buffer' to a file */
gribint save_grib_file(
unsigned char *grib_buffer,  /* buffer where the GRIB message is */
gribint size,		     /* size of grib_buffer */
struct grib_type grib_msg)   /* structure of read GRIB data */
{
  FILE *grib_file;
  size_t written_bytes;
  char file_name[13];
  static int progr;
  int	 progr_aux;
  static char last_date[9] ="XXXXXXXX";
  char   new_date[9];
  char	 progr_s[4];
  char	 letter[1];
  
  sprintf(new_date,"%02ld%02ld%02ld%02ld",grib_msg.section1.month,
	  grib_msg.section1.day,grib_msg.section1.hour,
	  grib_msg.section1.minute);
  if (strcmp(new_date,last_date) != 0) {
    strcpy(last_date,new_date);
    progr = 0;
  }
  file_name[0] = '\0';
  ++progr;
  progr_aux = progr;
  progr_s[0] = '\0';
  if (progr_aux >= 26*26) {
    letter[0] = (char) (64+progr_aux/(26*26));
    strncat(progr_s,letter,1);
    progr_aux = progr_aux % (26*26);
  }
  if (progr_aux >= 26) {
    letter[0] = (char)(64+progr_aux/(26));
    strncat(progr_s,letter,1);
    progr_aux = progr_aux % (26);
  }
  letter[0] = (char)(64+progr_aux);
  strncat(progr_s,letter,1);

  sprintf(file_name,"%02ld%02ld%02ld%02ld.%s",grib_msg.section1.month,
	  grib_msg.section1.day,grib_msg.section1.hour,
	  grib_msg.section1.minute,progr_s);

  grib_file = fopen(file_name,"wb");

  /* write the loaded grib buffer in a file */
  written_bytes = fwrite(grib_buffer,sizeof(unsigned char),
			 (size_t) size, grib_file);

  fclose(grib_file);

  if (written_bytes != size) return ABORT_STATUS;
  return NORMAL_STATUS;
}


/* Write an array of values reading from a stream of bits and 
   using a bitmap */ 
void unpack_grib(
unsigned char *bit_stream,   /* input bit-stream buffer */
gribfloat no_bit_value,          /* value to be written in output array when
				bitmap specifies that no data have been
				taken for a position */
gribint ignore_bitmap,      /* flag that specifies if bitmap has to be
				considered or not */
struct grib_type *grib_msg)  /* structure of read GRIB data */
{
  ugribint value;
  gribint i,j;
  gribint bits_number;
  gribint bit_stream_bit;
  gribint bitmap_bit;
  gribint value_bit;
  gribfloat exponent;
  int  damned_condition;
  char *bitmap;
  void *unpacked_array;

  unpacked_array = grib_msg->section4.binary_data;
  bits_number = grib_msg->section4.bits_number;
  bitmap = grib_msg->section3.bitmap;
  exponent = pow( (double) 2, (double) grib_msg->section4.scale_factor);
  bit_stream_bit = 0x80;
  bitmap_bit = 0x80;
  for (i=1;i<=grib_msg->section4.unpacked_values;i++) {
    damned_condition = TRUE;
    if (!ignore_bitmap) {
      if ((*bitmap & bitmap_bit) == 0)
	damned_condition = FALSE;
      if ((bitmap_bit >>= 1) == 0 ) {
	bitmap_bit = 0x80;
	++bitmap;
      }
    }
    if (damned_condition) {
      value_bit = 1;
      value_bit <<= (bits_number-1);
      value = 0;
      for (j=1;j<=bits_number;j++) {
	if (*bit_stream & bit_stream_bit)
	  value += value_bit;
	if ((bit_stream_bit >>= 1) == 0 ) {
	  bit_stream_bit = 0x80;
	  ++bit_stream;
	}
	value_bit >>= 1;
      }
      if (grib_msg->section4.work_with_floats) {
	*(gribfloat *) unpacked_array =
	  value*exponent+grib_msg->section4.reference_value;
	unpacked_array = (gribfloat *) unpacked_array + 1;
      } else {
	*(gribint *) unpacked_array =
	  (gribint) (value*exponent+grib_msg->section4.reference_value);
	unpacked_array = (gribint *) unpacked_array + 1;
      }
    } else {
      if (grib_msg->section4.work_with_floats) {
	*(gribfloat *) unpacked_array = no_bit_value;
	unpacked_array = (gribfloat *) unpacked_array + 1;
      } else {
	*(gribint *) unpacked_array = (gribint) no_bit_value;
	unpacked_array = (gribint *) unpacked_array + 1;
      }
    }
  }
}


/* Read only the lengths of the sections */
gribint decode_lengths(
unsigned char **section0,    /* section 0 address (by reference) */
struct grib_type *grib_msg)  /* structure of read GRIB data */
{
  unsigned char *section_start;
  unsigned char *flag;

  section_start = (*section0)+4;
  read_var_len_int(&section_start,3,&grib_msg->section0.total_length);
  read_bytes_in_ints(&section_start,1,&grib_msg->section0.grib_edition);
  flag = section_start + 7;
  read_var_len_int(&section_start,3,&grib_msg->section1.section_length);
  section_start += grib_msg->section1.section_length - 3 ;
  if (*flag & 0x80) {
    read_var_len_int(&section_start,3,&grib_msg->section2.section_length);
    section_start += grib_msg->section2.section_length - 3 ;
  } else
    grib_msg->section2.section_length = 0;
  if (*flag & 0x40) {
    read_var_len_int(&section_start,3,&grib_msg->section3.section_length);
    section_start += grib_msg->section3.section_length - 3 ;
  } else
    grib_msg->section3.section_length = 0;
  read_var_len_int(&section_start,3,&grib_msg->section4.section_length);
  section_start += grib_msg->section4.section_length - 3 ;
  if (memcmp(section_start,"7777",4)) return ABORT_STATUS;
  else return NORMAL_STATUS;
}


/* Read values from the buffer 'section0' to the data structure */
void decode_section0(
unsigned char **section0,    /* section 0 address (by reference) */
struct grib_type *grib_msg)  /* structure where GRIB data have to be written */
{
  read_bytes_in_bytes(section0,4,grib_msg->section0.word_grib);
  read_var_len_int(section0,3,&grib_msg->section0.total_length);
  read_bytes_in_ints(section0,1,&grib_msg->section0.grib_edition);
}


/* Read values from the buffer 'section1' to the data structure */
void decode_section1(
unsigned char **section1,    /* section 1 address (by reference) */
struct grib_type *grib_msg)  /* structure where GRIB data have to be written */
{
  unsigned char *section1_start;

  section1_start = *section1;
  read_var_len_int(section1,3,&grib_msg->section1.section_length);
  read_bytes_in_ints(section1,7,&grib_msg->section1.table2_version);
  if (grib_msg->section1.level_type==101 || grib_msg->section1.level_type==104 ||
      grib_msg->section1.level_type==106 || grib_msg->section1.level_type==108 ||
      grib_msg->section1.level_type==110 || grib_msg->section1.level_type==112 ||
      grib_msg->section1.level_type==114 || grib_msg->section1.level_type==116 ||
      grib_msg->section1.level_type==120 || grib_msg->section1.level_type==121 ||
      grib_msg->section1.level_type==128 || grib_msg->section1.level_type==141) {
    read_var_len_int(section1,1,&grib_msg->section1.measure1);
    read_var_len_int(section1,1,&grib_msg->section1.measure2);
  } else {
    read_var_len_int(section1,2,&grib_msg->section1.measure1);
    grib_msg->section1.measure2 = 0;
  }
  read_bytes_in_ints(section1,6,&grib_msg->section1.year);
  read_var_len_int(section1,1,&grib_msg->section1.time_period1);
  read_var_len_int(section1,1,&grib_msg->section1.time_period2);
  read_bytes_in_ints(section1,1,&grib_msg->section1.time_range);
  if (grib_msg->section1.time_range == 10) {
    grib_msg->section1.time_period1 <<= 8;
    grib_msg->section1.time_period1 += grib_msg->section1.time_period2;
    grib_msg->section1.time_period2 = 0;
  } else if (grib_msg->section1.time_range == 1) {
    grib_msg->section1.time_period1 = 0;
    grib_msg->section1.time_period2 = 0;
  } else if (grib_msg->section1.time_range == 0) {
    grib_msg->section1.time_period2 = 0;
  }
  read_var_len_int(section1,2,&grib_msg->section1.number_in_average);
  read_bytes_in_ints(section1,3,&grib_msg->section1.number_not_in_average);
  read_var_len_int(section1,2,&grib_msg->section1.decimal_scale_factor);
  if (grib_msg->section1.section_length >= 40)
    read_bytes_in_ints(section1,12,grib_msg->section1.reserved2);
  if (grib_msg->section1.section_length >= 41)
    read_bytes_in_ints(section1,1,&grib_msg->section1.local_definition);

  switch (grib_msg->section1.centre) {
  case 128: /* ECMWF */
    if (grib_msg->section1.local_definition != 0) {
      read_bytes_in_ints(section1,2,&grib_msg->section1.class);
      read_var_len_int(section1,2,&grib_msg->section1.stream);
      read_var_len_int(section1,4,&grib_msg->section1.experiment_version);
      
      switch (grib_msg->section1.local_definition) {
      case 1:
	read_bytes_in_ints(section1,2,&grib_msg->section1.ensemble_forecast);
	break;
      case 2:
	read_bytes_in_ints(section1,4,&grib_msg->section1.cluster);
	read_var_len_int(section1,2,&grib_msg->section1.start_time_step);
	read_var_len_int(section1,2,&grib_msg->section1.end_time_step);
	read_var_len_sint(section1,3,&grib_msg->section1.northern_latitude);
	read_var_len_sint(section1,3,&grib_msg->section1.western_longitude);
	read_var_len_sint(section1,3,&grib_msg->section1.southern_latitude);
	read_var_len_sint(section1,3,&grib_msg->section1.eastern_longitude);
	read_bytes_in_ints(section1,3,&grib_msg->section1.operational_cluster);
	grib_msg->section1.forecasts_list =
	  malloc((int) grib_msg->section1.cluster_forecasts);
	read_bytes_in_bytes(section1,(int) grib_msg->section1.cluster_forecasts,
			    grib_msg->section1.forecasts_list);
	break;
      }
    }
  case 78: case 200: /* DWD, ARPA */
    switch (grib_msg->section1.local_definition) {
    case 254:
      read_bytes_in_ints(section1,11,grib_msg->section1.dwd_luse);
      read_var_len_int(section1,2,&grib_msg->section1.dwd_version);
      break;
    }
  }

  *section1 = section1_start+grib_msg->section1.section_length;
}


/* Read values from the buffer 'section2' to the data structure */
gribint decode_section2(
unsigned char **section2,   /* section 2 address (by reference) */
struct grib_type *grib_msg) { /* structure where GRIB data have to be written */

  unsigned char *section2_start;
  gribint mantissa, sign_and_exponent;
  char trash[8];
  int i;

  if (grib_msg->section1.flag & 0x80) {
    section2_start = *section2;
    read_var_len_int(section2,3,&grib_msg->section2.section_length);
    read_bytes_in_ints(section2,3,&grib_msg->section2.vertical_parameters);

    if (grib_msg->section2.data_representation < 50) { /* common */
      read_var_len_int(section2,2,&grib_msg->section2.parallel_points);
      read_var_len_int(section2,2,&grib_msg->section2.meridian_points);
      read_var_len_sint(section2,3,&grib_msg->section2.first_latitude);
      read_var_len_sint(section2,3,&grib_msg->section2.first_longitude);
      read_bytes_in_ints(section2,1,&grib_msg->section2.resolution_and_flags);

      if (grib_msg->section2.data_representation != 1 &&
	  grib_msg->section2.data_representation != 3 &&
	  grib_msg->section2.data_representation != 5 &&
	  grib_msg->section2.data_representation != 8) { /* common simple */
	read_var_len_sint(section2,3,&grib_msg->section2.last_latitude);
	read_var_len_sint(section2,3,&grib_msg->section2.last_longitude);
	read_var_len_sint(section2,2,&grib_msg->section2.parallel_increment);
	read_var_len_sint(section2,2,&grib_msg->section2.meridian_increment);
	read_bytes_in_ints(section2,1,&grib_msg->section2.scanning_mode);
	read_bytes_in_bytes(section2,4,trash);
      } else { /* more complex projections */
	if (grib_msg->section2.data_representation == 1) { /* Mercatore */
	  read_var_len_sint(section2,3,&grib_msg->section2.last_latitude);
	  read_var_len_sint(section2,3,&grib_msg->section2.last_longitude);
	  read_var_len_sint(section2,3,&grib_msg->section2.latin1);
	  read_bytes_in_bytes(section2,1,trash);
	  read_bytes_in_ints(section2,1,&grib_msg->section2.scanning_mode);
	  read_var_len_sint(section2,3,&grib_msg->section2.parallel_increment);
	  read_var_len_sint(section2,3,&grib_msg->section2.meridian_increment);
	  read_bytes_in_bytes(section2,8,trash);
	} else { /* common to Lambert/stereo/Albers */
	  read_var_len_sint(section2,3,&grib_msg->section2.lov);
	  read_var_len_sint(section2,3,&grib_msg->section2.parallel_increment);
	  read_var_len_sint(section2,3,&grib_msg->section2.meridian_increment);
	  read_bytes_in_ints(section2,1,&grib_msg->section2.centre_flag);
	  read_bytes_in_ints(section2,1,&grib_msg->section2.scanning_mode);
	  if (grib_msg->section2.data_representation == 3 ||
	      grib_msg->section2.data_representation == 8) { /* common to L/A */
	    read_var_len_sint(section2,3,&grib_msg->section2.latin1);
	    read_var_len_sint(section2,3,&grib_msg->section2.latin2);
	    read_var_len_sint(section2,3,&grib_msg->section2.southern_pole_latitude);
	    read_var_len_sint(section2,3,&grib_msg->section2.southern_pole_longitude);
	    read_bytes_in_bytes(section2,3,trash);
	  } else { /* stereo */
	    grib_msg->section2.latin1 = 0;
	    grib_msg->section2.latin2 = 0;
	    grib_msg->section2.southern_pole_latitude = 0;
	    grib_msg->section2.southern_pole_longitude = 0;
	    read_bytes_in_bytes(section2,4,trash);
	  }
	}
      }

      if (grib_msg->section2.section_length>32 && 
	  (grib_msg->section2.data_representation == 10 ||
	   grib_msg->section2.data_representation == 14 ||
	   grib_msg->section2.data_representation == 30 ||
	   grib_msg->section2.data_representation == 34)) { /* rotated */
	read_var_len_sint(section2,3,&grib_msg->section2.southern_pole_latitude);
	read_var_len_sint(section2,3,&grib_msg->section2.southern_pole_longitude);
	read_var_len_int(section2,1,&sign_and_exponent);
	read_var_len_int(section2,3,&mantissa);
	grib_msg->section2.rotation_angle = 
	  decode_float(sign_and_exponent, mantissa);
      }
      
      if ((grib_msg->section2.section_length>32 &&
	   (grib_msg->section2.data_representation == 20 ||
	    grib_msg->section2.data_representation == 24)) ||
	  (grib_msg->section2.section_length>42 &&
	   (grib_msg->section2.data_representation == 30 ||
	    grib_msg->section2.data_representation == 34))) { /* stretched */
	read_var_len_sint(section2,3,&grib_msg->section2.stretching_pole_latitude);
	read_var_len_sint(section2,3,&grib_msg->section2.stretching_pole_longitude);
	read_var_len_int(section2,1,&sign_and_exponent);
	read_var_len_int(section2,3,&mantissa);
	grib_msg->section2.stretching_factor = 
	  decode_float(sign_and_exponent, mantissa);
      }
    } else if (grib_msg->section2.data_representation == 192) { /* triangular */
      read_var_len_int(section2,2,&grib_msg->section2.triang_ni2);
      read_var_len_int(section2,2,&grib_msg->section2.triang_ni3);
      read_var_len_int(section2,3,&grib_msg->section2.triang_nd);
      read_var_len_int(section2,3,&grib_msg->section2.triang_ni);
    } else { /* unsupported */
      return ABORT_STATUS;
    }

  if (grib_msg->section2.list_position != 255 ) {  
    if (grib_msg->section2.list_position +
	grib_msg->section2.vertical_parameters*4 - 1 >
	grib_msg->section2.section_length)
      return ABORT_STATUS;

    *section2=section2_start+grib_msg->section2.list_position -1 ;

    for ( i=0; i < grib_msg->section2.vertical_parameters &&
	    i <= MAXLISTVERCORPAR ; i++ ) {
      read_var_len_int(section2,1,&sign_and_exponent);
      read_var_len_int(section2,3,&mantissa);

      grib_msg->section2.lisvercorpar[i] = 
	decode_float(sign_and_exponent, mantissa);
    }
  }
  }

  *section2 = section2_start+grib_msg->section2.section_length;
  return NORMAL_STATUS;
}


/* Read values from the buffer 'section3' to the data structure */
void decode_section3(
unsigned char **section3,    /* section 3 address (by reference) */
struct grib_type *grib_msg,  /* structure where GRIB data have to be written */
int full) /* partial or full decode */
{
  unsigned char *section3_start;

  if (grib_msg->section1.flag & 0x40)
  {
    section3_start = *section3;
    read_var_len_int(section3,3,&grib_msg->section3.section_length);
    read_bytes_in_ints(section3,1,&grib_msg->section3.unused_bits);
    read_var_len_int(section3,2,&grib_msg->section3.bitmap_code);
    if (full) {
      if (grib_msg->section3.bitmap_code == 0){
	grib_msg->section3.bitmap=malloc((int) grib_msg->section3.section_length-6);
	read_bytes_in_bytes(section3, (int) grib_msg->section3.section_length-6, grib_msg->section3.bitmap);
      } else {
	grib_msg->section3.bitmap = 0;
      }
    }
    *section3 = section3_start+grib_msg->section3.section_length;
  }
}


/* Read values from the buffer 'section4' to the data structure */
gribint decode_section4(
unsigned char **section4,    /* section 4 address (by reference) */
gribfloat no_bit_value,          /* structure of read GRIB data */
gribint ignore_bitmap,      /* flag that specifies if bitmap has to be
				considered or not */
struct grib_type *grib_msg,  /* structure where GRIB data have to be written */
int full) /* partial or full decode */
{
  gribint mantissa, sign_and_exponent;
  unsigned char *section4_start;

  section4_start = *section4;
  read_var_len_int(section4,3,&grib_msg->section4.section_length);
  read_bytes_in_ints(section4,1,&grib_msg->section4.flag_and_unused_bits);
  if (grib_msg->section4.flag_and_unused_bits & 0xC0) return ABORT_STATUS;
  read_var_len_int(section4,2,&grib_msg->section4.scale_factor);
  if (grib_msg->section4.scale_factor & 0x8000)
    grib_msg->section4.scale_factor = -(grib_msg->section4.scale_factor & 0x7FFF);
  read_var_len_int(section4,1,&sign_and_exponent);
  read_var_len_int(section4,3,&mantissa);
  grib_msg->section4.reference_value = 
    decode_float(sign_and_exponent, mantissa);
  read_bytes_in_ints(section4,1,&grib_msg->section4.bits_number);
  if (grib_msg->section4.bits_number > 0) {
    grib_msg->section4.packed_values =
      ((grib_msg->section4.section_length-11) * 8 -
       (grib_msg->section4.flag_and_unused_bits & 0x0F )) /
      (grib_msg->section4.bits_number);
  } else {
    grib_msg->section4.packed_values = grib_msg->section2.parallel_points *
      grib_msg->section2.meridian_points;
  }
  if (ignore_bitmap | !(grib_msg->section1.flag & 0x40)
      | (grib_msg->section3.bitmap_code != 0) )
    grib_msg->section4.unpacked_values = grib_msg->section4.packed_values;
  else
    grib_msg->section4.unpacked_values =
      ( (grib_msg->section3.section_length-6) * 8
	- grib_msg->section3.unused_bits );
  grib_msg->section4.work_with_floats = !(grib_msg->section4.flag_and_unused_bits & 0x20);
  if (full) {
    if (grib_msg->section4.work_with_floats)
      grib_msg->section4.binary_data = 
	malloc((int) (sizeof(gribfloat)*grib_msg->section4.unpacked_values));
    else
      grib_msg->section4.binary_data = 
	malloc((int) (sizeof(gribint)*grib_msg->section4.unpacked_values));
    unpack_grib(*section4, no_bit_value,
		ignore_bitmap | !(grib_msg->section1.flag & 0x40)
		| (grib_msg->section3.bitmap_code != 0),
		grib_msg);
  }
  *section4 = section4_start+grib_msg->section4.section_length;
  return NORMAL_STATUS;
}


/* Read values from the buffer 'section5' to the data structure and
   check for the end-of-message pattern */
gribint decode_section5(
unsigned char **section5,    /* section 5 address (by reference) */
struct grib_type *grib_msg)  /* structure where GRIB data have to be written */
{
  read_bytes_in_bytes(section5,4,grib_msg->section5.end_of_message);
  if (memcmp((*section5)-4,"7777",4)) return ABORT_STATUS;
  else return NORMAL_STATUS;
}


/* Read from 'grib_buffer' what is asked by 'operation' and 
   fill the data structure 'grib_msg' */
gribint degrib(
char operation,              /* code of information group to be extracted */
unsigned char **grib_buffer, /* loaded GRIB data block */
gribint ignore_bitmap,       /* flag that specifies if bitmap has to be
				considered or not */
struct grib_type *grib_msg,  /* structure where GRIB data have to be written */
gribfloat *no_bit_value)     /* value to be written in output array when
				bitmap specifies that no data have been
				taken for a position */

/* no_bit_value is passed by reference because I have problems with VAX linker
   when passing float variables by value between compiled objects */
{
  gribint status;

  memset(grib_msg,'\0',sizeof(*grib_msg));
  switch(operation)
    {
    case GRIB_LENGTH :
      status = decode_lengths(grib_buffer,grib_msg);
      return status;
    case GRIB_INFO :
      decode_section0(grib_buffer,grib_msg);
      decode_section1(grib_buffer,grib_msg);
      status = decode_section2(grib_buffer,grib_msg);
      return status;
    case GRIB_MOREINFO :
      decode_section0(grib_buffer,grib_msg);
      decode_section1(grib_buffer,grib_msg);
      status = decode_section2(grib_buffer,grib_msg);
      if (status != NORMAL_STATUS) return status;
      decode_section3(grib_buffer,grib_msg,0);
      status = decode_section4(grib_buffer,*no_bit_value,ignore_bitmap,grib_msg,0);
      return status;
    case GRIB_DECODE :
      decode_section0(grib_buffer,grib_msg);
      decode_section1(grib_buffer,grib_msg);
      status = decode_section2(grib_buffer,grib_msg);
      if (status != NORMAL_STATUS) return status;
      decode_section3(grib_buffer,grib_msg,1);
      status = decode_section4(grib_buffer,*no_bit_value,ignore_bitmap,grib_msg,1);
      if (status != NORMAL_STATUS) return status;
      status = decode_section5(grib_buffer,grib_msg);
      if (status != NORMAL_STATUS) return status;
      break;
    }
  return NORMAL_STATUS;
}


/* Print as asked by 'operation' the data structure 'grib_msg' */
void print_grib(
		char operation,              /* code of information group to be printed */
		struct grib_type grib_msg)   /* structure of read GRIB data */
{
  gribint i, max;

  switch(operation) {
  case GRIB_LENGTH :
    printf("%15ld     Total length of GRIB message\n",grib_msg.section0.total_length);
    printf("%15ld     Length of section 1\n",grib_msg.section1.section_length);
    printf("%15ld     Length of section 2\n",grib_msg.section2.section_length);
    printf("%15ld     Length of section 3\n",grib_msg.section3.section_length);
    printf("%15ld     length of section 4\n",grib_msg.section4.section_length);
    break;
  case GRIB_INFO :
    printf("%15ld     Total length of GRIB message\n",grib_msg.section0.total_length);
    printf("%15ld     GRIB edition number\n",grib_msg.section0.grib_edition);
    printf("\nS E C T I O N   1\n\n");
    printf("%15ld     Length of section 1\n",grib_msg.section1.section_length);
    printf("%15ld     Code table 2, parameter table, Version No.\n",grib_msg.section1.table2_version);
    printf("%15ld     Identification of centre\n",grib_msg.section1.centre);
    printf("%15ld     Generating process identification number\n",grib_msg.section1.process);
    printf("%15ld     Grid definition\n",grib_msg.section1.grid_definition);
    printf("%15ld     Flag\n",grib_msg.section1.flag);
    printf("%15ld     Indicator of parameter\n",grib_msg.section1.parameter);
    printf("%15ld     Indicator of type of level\n",grib_msg.section1.level_type);
    printf("%15ld     Heigth, pressure, etc. of levels\n",grib_msg.section1.measure1);
    printf("%15ld     \n",grib_msg.section1.measure2);
    printf("%15ld     Year of century\n",grib_msg.section1.year);
    printf("%15ld     Month\n",grib_msg.section1.month);
    printf("%15ld     Day\n",grib_msg.section1.day);
    printf("%15ld     Hour\n",grib_msg.section1.hour);
    printf("%15ld     Minute\n",grib_msg.section1.minute);
    printf("%15ld     Indicator of unit of time range\n",grib_msg.section1.time_range_unit);
    printf("%15ld     P1 - Period of time\n",grib_msg.section1.time_period1);
    if (grib_msg.section1.time_period2 != 0)
      printf("%15ld     P2 - Period of time\n",grib_msg.section1.time_period2);
    printf("%15ld     Time range indicator\n",grib_msg.section1.time_range);
    printf("%15ld     Number included in average\n",grib_msg.section1.number_in_average);
    printf("%15ld     Number missing from averages or accumulations\n",grib_msg.section1.number_not_in_average);
    printf("%15ld     Century of reference\n",grib_msg.section1.reference_century);
    printf("%15ld     Units decimal scale factor\n",grib_msg.section1.decimal_scale_factor);
    if (grib_msg.section1.local_definition == 1 ||
	grib_msg.section1.local_definition == 2)
      {
	printf("%15ld     Local definition number\n",grib_msg.section1.local_definition);
	printf("%15ld     Class\n",grib_msg.section1.class);
	printf("%15ld     Type\n",grib_msg.section1.type);
	printf("%15ld     Stream\n",grib_msg.section1.stream);
	printf("%15ld     Experiment version\n",grib_msg.section1.experiment_version);
	switch (grib_msg.section1.local_definition)
	  {
	  case 1 :
	    printf("%15ld     Ensemble forecast number\n",grib_msg.section1.ensemble_forecast);
	    printf("%15ld     Total number of forecasts in ensemble\n",grib_msg.section1.forecasts_total);
	    break;
	  case 2 :
	    printf("%15ld     Cluster number\n",grib_msg.section1.cluster);
	    printf("%15ld     Total number of of clusters\n",grib_msg.section1.clusters_total);
	    printf("%15ld     Clustering method\n",grib_msg.section1.clustering_method);
	    printf("%15ld     Start time step considered when clustering\n",grib_msg.section1.start_time_step);
	    printf("%15ld     End time step considered when clustering\n",grib_msg.section1.end_time_step);
	    printf("%15ld     Northern latitude of domain when clustering\n",grib_msg.section1.northern_latitude);
	    printf("%15ld     Western longitude of domain when clustering\n",grib_msg.section1.western_longitude);
	    printf("%15ld     Southern latitude of domain when clustering\n",grib_msg.section1.southern_latitude);
	    printf("%15ld     Eastern longitude of domain when clustering\n",grib_msg.section1.eastern_longitude);
	    printf("%15ld     Number of cluster to which operational cluster belongs\n",grib_msg.section1.operational_cluster);
	    printf("%15ld     Number of cluster to which control cluster belongs\n",grib_msg.section1.control_cluster);
	    printf("%15ld     Number of forecasts belonging to the cluster\n",grib_msg.section1.cluster_forecasts);
	    for (i=1;i<=grib_msg.section1.cluster_forecasts;++i)
	      {
		printf("%15ld     Ensemble forecast number %d\n",*grib_msg.section1.forecasts_list,i);
		++grib_msg.section1.forecasts_list;
	      }
	    break;
	  }
      }
    if (grib_msg.section2.section_length != 0) {
      printf("\nS E C T I O N   2\n\n");
      printf("%15ld     Length of section 2\n",grib_msg.section2.section_length);
      printf("%15ld     Number of vertical coordinate parameters\n",grib_msg.section2.vertical_parameters);
      printf("%15ld     Data representation type\n",grib_msg.section2.data_representation);
      if (grib_msg.section2.data_representation >= 50 &&
	  grib_msg.section2.data_representation != 192) {
	printf("DATA REPRESENTATION TYPE NOT IMPLEMENTED\n");
      } else if (grib_msg.section2.data_representation != 192) { /* common */
	printf("%15ld     Number of points along a parallel\n",grib_msg.section2.parallel_points);
	printf("%15ld     Number of points along a meridian\n",grib_msg.section2.meridian_points);
	printf("%15ld     Latitude of first grid point\n",grib_msg.section2.first_latitude);
	printf("%15ld     Longitude of first grid point\n",grib_msg.section2.first_longitude);
	printf("%15ld     Resolution and component flags\n",grib_msg.section2.resolution_and_flags);

	if (grib_msg.section2.data_representation != 3 &&
	    grib_msg.section2.data_representation != 5 &&
	    grib_msg.section2.data_representation != 8) { /* common simple + Mercatore*/
	  printf("%15ld     Latitude of last grid point\n",grib_msg.section2.last_latitude);
	  printf("%15ld     Longitude of last grid point\n",grib_msg.section2.last_longitude);
	  printf("%15ld     Parallel direction increment\n",grib_msg.section2.parallel_increment);
	  printf("%15ld     Meridian direction increment\n",grib_msg.section2.meridian_increment);
	  if (grib_msg.section2.data_representation == 1)
	    printf("%15ld     Latitude of intersection\n",grib_msg.section2.latin1);
	  printf("%15ld     Scanning mode\n",grib_msg.section2.scanning_mode);
	}

	if (grib_msg.section2.data_representation == 3 ||
	    grib_msg.section2.data_representation == 5 ||
	    grib_msg.section2.data_representation == 8) { /* common to Lambert/stereo/Albers */
	  printf("%15ld     Longitude of line of view\n",grib_msg.section2.lov);
	  printf("%15ld     Parallel direction increment\n",grib_msg.section2.parallel_increment);
	  printf("%15ld     Meridian direction increment\n",grib_msg.section2.meridian_increment);
	  printf("%15ld     Projection centre flag\n",grib_msg.section2.centre_flag);
	  printf("%15ld     Scanning mode\n",grib_msg.section2.scanning_mode);
	}

	if (grib_msg.section2.data_representation == 3 ||
	    grib_msg.section2.data_representation == 8) { /* common to L/A */
	  printf("%15ld     Latitude of first intersection\n",grib_msg.section2.latin1);
	  printf("%15ld     Latitude of second intersection\n",grib_msg.section2.latin2);
	  printf("%15ld     Latitude of the southern pole in millidegrees\n",grib_msg.section2.southern_pole_latitude);
	  printf("%15ld     Longitude of the southern pole in millidegrees\n",grib_msg.section2.southern_pole_longitude);
	}

	if (grib_msg.section2.data_representation == 10 ||
	    grib_msg.section2.data_representation == 14 ||
	    grib_msg.section2.data_representation == 30 ||
	    grib_msg.section2.data_representation == 34) { /* rotated */
	  printf("%15ld     Latitude of the southern pole in millidegrees\n",grib_msg.section2.southern_pole_latitude);
	  printf("%15ld     Longitude of the southern pole in millidegrees\n",grib_msg.section2.southern_pole_longitude);
	  printf("%15f     Angle of rotation\n",grib_msg.section2.rotation_angle);
	}

	if (grib_msg.section2.data_representation == 20 ||
	    grib_msg.section2.data_representation == 24 ||
	    grib_msg.section2.data_representation == 30 ||
	    grib_msg.section2.data_representation == 34) { /* stretched */
	  printf("%15ld     Latitude of pole of stretching in millidegrees\n",grib_msg.section2.stretching_pole_latitude);
	  printf("%15ld     Longitude of pole of stretching in millidegrees\n",grib_msg.section2.stretching_pole_longitude);
	  printf("%15f     Stretching factor\n",grib_msg.section2.stretching_factor);
	}
      } else {
	printf("%7ld %7ld     Triangular grid parameters\n",
	       grib_msg.section2.triang_ni2,grib_msg.section2.triang_ni3);
	printf("%15ld     Number of triangles\n",
	       grib_msg.section2.triang_nd);
	printf("%15ld     Number of points along a triangle side\n",
	       grib_msg.section2.triang_ni);
      }
      for (i=0; i< grib_msg.section2.vertical_parameters; i++)
	printf("%15f     Vertical Coordinate Parameter\n",grib_msg.section2.lisvercorpar[i]);
    } else
      printf("\nS E C T I O N   2   I S   N O T   P R E S E N T\n\n");
    break;
  case GRIB_DECODE :
    if (grib_msg.section3.section_length != 0) {
      printf("\nS E C T I O N   3\n\n");
      printf("%15ld     Length of section 3\n",grib_msg.section3.section_length);
      printf("%15ld     Predetermined bit map number\n",grib_msg.section3.bitmap_code);
    } else
      printf("\nS E C T I O N   3   I S   N O T   P R E S E N T\n\n");

    printf("\nS E C T I O N   4\n\n");
    printf("%15ld     Length of section 4\n",grib_msg.section4.section_length);
    printf("%15ld     Flag\n",grib_msg.section4.flag_and_unused_bits);
    printf("%15ld     Scale factor\n",grib_msg.section4.scale_factor);
    printf( "%15f     Reference value\n",grib_msg.section4.reference_value);
    printf("%15ld     Number of bits containing each packed value\n",grib_msg.section4.bits_number);
    printf("%15ld     Number of packed values\n",grib_msg.section4.packed_values);
    printf("%15ld     Number of unpacked values\n",grib_msg.section4.unpacked_values);
    max = grib_msg.section4.unpacked_values;
    if (max>10) max = 10;
    if (grib_msg.section4.work_with_floats)	{
      for (i=1;i<=max;++i) {
	printf("%15f     Unpacked value %d\n",* (gribfloat *) grib_msg.section4.binary_data,i);
	grib_msg.section4.binary_data = (gribfloat *) grib_msg.section4.binary_data + 1;
      }
    } else{
      for (i=1;i<=max;++i) {
	printf("%15ld     Unpacked value %d\n",* (gribint *) grib_msg.section4.binary_data,i);
	grib_msg.section4.binary_data = (gribint *) grib_msg.section4.binary_data + 1;
      }
    }
    break;
  }
  printf("\n\n");
}


/* The next four functions need an argument in degrees */

double sind(x)
double x;                    /* angle in degrees */
{
  double dummy;
  static double deg_to_rad = 0.017453292;

  dummy = sin(x*deg_to_rad);
  return dummy;
}

double cosd(x)
double x;                    /* angle in degrees */
{
  double dummy;
  static double deg_to_rad = 0.017453292;

  dummy = cos(x*deg_to_rad);
  return dummy;
}

double asind(x)
double x;                    /* value whose arcsine is to be calculated */
{
  double dummy;
  static double rad_to_deg = 57.29577951;

  dummy = rad_to_deg*asin(x);
  return dummy;
}

double acosd(x)
double x;                    /* value whose arccosine is to be calculated */
{
  double dummy;
  static double rad_to_deg = 57.29577951;

  dummy = rad_to_deg*acos(x); 
  return dummy;
}


void rot_grib_lambo(alo2,ala2,tlm0d,tph0d)    
gribfloat alo2,ala2;
gribfloat *tlm0d,*tph0d;
{

	*tph0d=acosd(-sind( (double) ala2));
	*tlm0d=alo2;

}


void rtlld(alo1,ala1,tlm0d,ctph0,
	   stph0,alo2,ala2)
gribfloat alo1,ala1,tlm0d,ctph0;
gribfloat stph0;
gribfloat *alo2,*ala2;
{

      gribfloat stph;
      gribfloat ctph;                     
      gribfloat ctlm;                      
      gribfloat stlm;                      
      gribfloat cph;                      

 
      stph=sind((double) ala1);
      ctph=cosd((double) ala1);                     
      ctlm=cosd((double) alo1);                      
      stlm=sind((double) alo1);                      

      *ala2=asind((double) (stph0 * ctph * ctlm + ctph0 * stph));
      cph=cosd((double) (*ala2));                        

      *alo2=tlm0d + asind((double) (stlm * ctph / cph));
}


/* Write in an ASCII file the values of the data structure 'grib_msg',
   if requested write also lat long coordinates */ 
void summarize_grib(grib_msg,show_lat_long,no_bit_value)
struct grib_type grib_msg;   /* structure of read GRIB data */
gribint show_lat_long;      /* flag that specifies if all GRIB data have
				to be written in the file with the 
				corresponding lat-long coordinate */
gribfloat *no_bit_value;         /* value to be written in output array when
				bitmap specifies that no data have been
				taken for a position */
{
  char file_name[13];
  FILE *print_file;
  static int progr;
  static char last_date[9] ="XXXXXXXX";
  char   new_date[9];

  
  sprintf(new_date,"%02ld%02ld%02ld%02ld",grib_msg.section1.month,
	  grib_msg.section1.day,grib_msg.section1.hour,
	  grib_msg.section1.minute);
  if (strcmp(new_date,last_date) != 0)
  {
    strcpy(last_date,new_date);
    progr = 0;
  }
  file_name[0] = '\0';
  ++progr;
  sprintf(file_name,"%02ld%02ld%02ld%02ld.%03d",grib_msg.section1.month,
	  grib_msg.section1.day,grib_msg.section1.hour,
	  grib_msg.section1.minute,progr);
  print_file = fopen(file_name,"w");
  fprintf(print_file,"%15ld     Year of century\n",grib_msg.section1.year);
  fprintf(print_file,"%15ld     Month\n",grib_msg.section1.month);
  fprintf(print_file,"%15ld     Day\n",grib_msg.section1.day);
  fprintf(print_file,"%15ld     Hour\n",grib_msg.section1.hour);
  fprintf(print_file,"%15ld     Minute\n",grib_msg.section1.minute);
  fprintf(print_file,"%15ld     P1 - Period of time\n",grib_msg.section1.time_period1);
  fprintf(print_file,"%15ld     P2 - Period of time\n",grib_msg.section1.time_period2);
  fprintf(print_file,"%15ld     Heigth, pressure, etc. of levels\n",grib_msg.section1.measure1);
  fprintf(print_file,"%15ld     \n",grib_msg.section1.measure2);
  fprintf(print_file,"%15ld     Indicator of parameter\n",grib_msg.section1.parameter);
  if (grib_msg.section2.section_length != 0)
  {
    if (grib_msg.section2.data_representation != 0 &&
	grib_msg.section2.data_representation != 10)
    {
      fprintf(print_file,"DATA REPRESENTATION TYPE NOT IMPLEMENTED\n");
    }
    else
    {
      fprintf(print_file,"%15ld     Number of points along a parallel\n",grib_msg.section2.parallel_points);
      fprintf(print_file,"%15ld     Number of points along a meridian\n",grib_msg.section2.meridian_points);
      fprintf(print_file,"%15ld     Latitude of first grid point\n",grib_msg.section2.first_latitude);
      fprintf(print_file,"%15ld     Longitude of first grid point\n",grib_msg.section2.first_longitude);
      fprintf(print_file,"%15ld     Latitude of last grid point\n",grib_msg.section2.last_latitude);
      fprintf(print_file,"%15ld     Longitude of last grid point\n",grib_msg.section2.last_longitude);
      fprintf(print_file,"%15ld     Parallel direction increment\n",grib_msg.section2.parallel_increment);
      fprintf(print_file,"%15ld     Meridian direction increment\n",grib_msg.section2.meridian_increment);
      fprintf(print_file,"%15ld     Latitude of the southern pole in millidegrees\n",grib_msg.section2.southern_pole_latitude);
      fprintf(print_file,"%15ld     Longitude of the southern pole in millidegrees\n",grib_msg.section2.southern_pole_longitude);
    }
  }
  else
  {
    fprintf(print_file,"\nS E C T I O N   2   I S   N O T   P R E S E N T\n\n");
  }
  if (show_lat_long)
  {
    gribint  nulo;
    gribint  nvla;
    gribfloat     rlam1;
    gribfloat     rlom1;
    /*
    gribfloat     rlam2;
    gribfloat     rlom2;
    */
    gribfloat     plon;
    gribfloat     plat;
    gribfloat     polon;
    gribfloat     polat;
    gribfloat     tlm0d;
    gribfloat     tph0d;
    gribfloat     stph0;
    gribfloat     ctph0;
    gribint  j,i;
    gribfloat     rla;
    gribfloat     rlo;
    gribint  damned_flag;
    gribfloat     ala2;
    gribfloat     alo2;
    gribfloat     float_dummy;
    gribint  long_dummy;


    nulo=grib_msg.section2.parallel_points;
    nvla=grib_msg.section2.meridian_points;
    rlam1=grib_msg.section2.first_latitude/1000.;
    rlom1=grib_msg.section2.first_longitude/1000.;
    /*
    rlam2=grib_msg.section2.last_latitude/1000.;
    rlom2=grib_msg.section2.last_longitude/1000.;
    fprintf(print_file," la1,lo1,la2,lo2   %9.3f  %9.3f  %9.3f  %9.3f\n",
	    rlam1,rlom1,rlam2,rlom2);
    */
    plon=grib_msg.section2.parallel_increment/1000.;
    plat=grib_msg.section2.meridian_increment/1000.;
    if ( (grib_msg.section2.scanning_mode & 0x80) != 0) plon= -plon;
    if ( (grib_msg.section2.scanning_mode & 0x40) == 0) plat= -plat;
/*  fprintf(print_file," Scan,plon,plat,nulo,nvla %02d %9.3f  %9.3f  %5d  %5d\n",
	    grib_msg.section2.scanning_mode,plon,plat,nulo,nvla); */

/*  set up conversion factors for isec2(1)=10 rotated lat/lon grids */

    if (grib_msg.section2.data_representation == 10)
    {

      polat=grib_msg.section2.southern_pole_latitude/1000.;
      polon=grib_msg.section2.southern_pole_longitude/1000.;
      rot_grib_lambo(polon,polat,&tlm0d,&tph0d);
      fprintf(print_file," Rotation parameters  %9.3f  %9.3f  %9.3f  %9.3f\n",
	    polon,polat,tlm0d,tph0d);
      stph0=sind((double) tph0d);
      ctph0=cosd((double) tph0d);
    }

    if ( (grib_msg.section2.scanning_mode & 0x20) == 0)
    {
      for (j=1;j<=nvla;++j)
      {
	rla=rlam1+(j-1)*plat;
	for (i=1;i<=nulo;++i)
	{
	  rlo=rlom1+(i-1)*plon;

	  if (no_bit_value != 0)
	    damned_flag = TRUE;
	  else
	  {
	    if (grib_msg.section4.work_with_floats)
	    {
	      float_dummy = (gribfloat) NO_BIT_VALUE_DEFAULT;
	      damned_flag = (* ((gribfloat *) grib_msg.section4.binary_data)) != 
			    float_dummy;
	    }
	    else
	    {
	      long_dummy = (gribint) NO_BIT_VALUE_DEFAULT;
	      damned_flag = (* ((gribint *) grib_msg.section4.binary_data)) != 
			    long_dummy;
	    }
	  }
	  if (damned_flag)
	  {
	    if (grib_msg.section2.data_representation == 10)
	    {
	      rtlld(rlo,rla,tlm0d,ctph0,stph0,&alo2,&ala2);
	      if (grib_msg.section4.work_with_floats)
		fprintf(print_file,"%9.3f  %9.3f  %9.3f  %9.3f  %12.3f\n",
			rla,rlo,ala2,alo2,
			(* (gribfloat *) grib_msg.section4.binary_data));
	      else
		fprintf(print_file,"%9.3f  %9.3f  %9.3f  %9.3f  %ld\n",
			rla,rlo,ala2,alo2,
			(* (gribint *) grib_msg.section4.binary_data));
	    }
	    else
	    {
	      if (grib_msg.section4.work_with_floats)
		fprintf(print_file,"%9.3f  %9.3f  %12.3f\n",
			rla,rlo,
			(* (gribfloat *) grib_msg.section4.binary_data));
	      else
		fprintf(print_file,"%9.3f  %9.3f  %ld\n",
			rla,rlo,
			(* (gribint *) grib_msg.section4.binary_data));
	    }
	  }
	  if (grib_msg.section4.work_with_floats)
	    grib_msg.section4.binary_data = (gribfloat *) grib_msg.section4.binary_data + 1  ;
	  else
	    grib_msg.section4.binary_data = (gribint *) grib_msg.section4.binary_data + 1  ;
	}
      }
    }
    else
    {
      for (i=1;i<=nulo;++i)
      {
	rlo=rlom1+(i-1)*plon;
	for (j=1;j<=nvla;++j)
	{
	  rla=rlam1+(j-1)*plat;
	  if (no_bit_value != 0)
	    damned_flag = TRUE;
	  else
	  {
	    if (grib_msg.section4.work_with_floats)
	    {
	      float_dummy = (gribfloat) NO_BIT_VALUE_DEFAULT;
	      damned_flag = (* ((gribfloat *) grib_msg.section4.binary_data)) != 
			    float_dummy;
	    }
	    else
	    {
	      long_dummy = (gribint) NO_BIT_VALUE_DEFAULT;
	      damned_flag = (* ((gribint *) grib_msg.section4.binary_data)) != 
			    long_dummy;
	    }
	  }

	  if (damned_flag)
	  {
	    if (grib_msg.section2.data_representation == 10)
	    {
	      rtlld(rlo,rla,tlm0d,ctph0,stph0,&alo2,&ala2);
	      if (grib_msg.section4.work_with_floats)
		fprintf(print_file,"%9.3f  %9.3f  %9.3f  %9.3f  %12.3f\n",
			rla,rlo,ala2,alo2,
			(* (gribfloat *) grib_msg.section4.binary_data));
	      else
		fprintf(print_file,"%9.3f  %9.3f  %9.3f  %9.3f  %ld\n",
			rla,rlo,ala2,alo2,
			(* (gribint *) grib_msg.section4.binary_data));
	    }
	    else
	    {
	      if (grib_msg.section4.work_with_floats)
		fprintf(print_file,"%9.3f  %9.3f  %12.3f\n",
			rla,rlo,
			(* (gribfloat *) grib_msg.section4.binary_data));
	      else
		fprintf(print_file,"%9.3f  %9.3f  %ld\n",
			rla,rlo,
			(* (gribint *) grib_msg.section4.binary_data));
	    }
	  }
	  if (grib_msg.section4.work_with_floats)
	    grib_msg.section4.binary_data = (gribfloat *) 
		grib_msg.section4.binary_data + 1 ;
	  else
	    grib_msg.section4.binary_data =
	    (gribint *) grib_msg.section4.binary_data + 1;
	}
      }
    }
  }
  fclose(print_file);
}


void grib_cformat(
int grib_counter,            /* progressive grib number in file */
unsigned char *section2_ptr, /* pointer to section2 in grib buffer */	
struct grib_type *grib_msg,  /* structure of read GRIB data */
char *format,
char *file_name,             /* where to place formatted string */
int max_len_fn)              /* its maximum length (sizeof) */
{
  int i, *var, tmpvar, len_form;
  char loc_form[MAXFORMLEN], *end_form, *c;

  for(c=format; *c!='\0'; c++) {
    if (*c == '%') {
      if ((end_form=strpbrk(c+1, FO_SET)) == NULL) {
	fprintf(stderr, "Warning, format %s unterminated, ignored\n", c);
	break;
      }
      len_form = end_form - c;
      if (len_form > MAXFORMLEN-2) {
	fprintf(stderr, "Warning, format %s too long, ignored\n", c);
	c = end_form;
	continue;
      }
      strncpy(loc_form, c, len_form);
      *(loc_form+len_form)='d';
      *(loc_form+len_form+1)='\0';

      var = NULL;
      switch (*end_form) {
      case 'o':
	var = &(grib_msg->section1.centre);
	break;
      case 'g':
	var = &(grib_msg->section1.process);
	break;
      case 'v':
	var = &(grib_msg->section1.table2_version);
	break;
      case 'p':
	var = &(grib_msg->section1.parameter);
	break;

      case 'y':
	var = &(grib_msg->section1.year);
	break;
      case 'Y':
	tmpvar=grib_msg->section1.year+(grib_msg->section1.reference_century-1)*100;
	var = &tmpvar;
	break;
      case 'm':
	var = &(grib_msg->section1.month);
	break;
      case 'd':
	var = &(grib_msg->section1.day);
	break;
      case 'h':
	var = &(grib_msg->section1.hour);
	break;
      case 'M':
	var = &(grib_msg->section1.minute);
	break;

      case 'u':
	var = &(grib_msg->section1.time_range_unit);
	break;
      case 't':
	var = &(grib_msg->section1.time_period1);
	break;
      case 'T':
	var = &(grib_msg->section1.time_period2);
	break;
      case 'c':
	if (grib_msg->section1.time_range == 13) /* DWD hack */
	  tmpvar = 0;
	else
	  tmpvar =
	    grib_msg->section1.time_period1 > grib_msg->section1.time_period2 ?
	    grib_msg->section1.time_period1 : grib_msg->section1.time_period2;
	var = &tmpvar;
	break;
      case 'i':
	var = &(grib_msg->section1.time_range);
	  break;

      case 'V':
	var = &(grib_msg->section1.level_type);
	break;
      case 'l':
	var = &(grib_msg->section1.measure1);
	break;
      case 'L':
	var = &(grib_msg->section1.measure2);
	break;

      case 's':
	var = &(grib_msg->section1.dwd_luse[5]);
	break;
      case 'N':
	var = &(grib_counter);
	break;
      case 'G':
	for (i=0; i<grib_msg->section2.section_length; i++) {  
	  snprintf(file_name+strlen(file_name), max_len_fn-strlen(file_name),
		   "%02hhx", section2_ptr[i]);
	}
	var = NULL;
	break;

      case '%':
	if (strlen(file_name) < max_len_fn) strncat(file_name, end_form, 1);
	break;
      }
      if (var != NULL) 
	snprintf(file_name+strlen(file_name), max_len_fn-strlen(file_name),
		 loc_form, *var);
      c = end_form;

    }
    else {
      if (strlen(file_name) < max_len_fn) strncat(file_name, c, 1);
    }
  }
}


void format_usage(char *prog) {
  /*      01234567890123456789012345678901234567890123456789012345678901234567890123456789 */
  printf("\n<format> e` un opportuno descrittore di formato per rendere unico il nome\n");
  printf("di ciascun file in uscita; esso puo` contenere espressioni del tipo:\n");
  printf("%%<C-format><char> in cui <char> puo` essere uno dei seguenti caratteri:\n");
  printf("o\tcentro di emissione\n");
  printf("g\tprocesso generatore\n");
  printf("v\tversione tabella 2\n");
  printf("p\tparametro\n");
  printf("y\tanno a 2 cifre\n");
  printf("Y\tanno a 4 cifre\n");
  printf("m\tmese\n");
  printf("d\tgiorno\n");
  printf("h\tora\n");
  printf("M\tminuto\n");
  printf("u\tunita` di misura time range\n");
  printf("t\tvalore di tempo 1\n");
  printf("T\tvalore di tempo 2\n");
  printf("c\tmassimo tra i valori di tempo 1 e 2\n");
  printf("i\tindicatore di time range\n");
  printf("V\ttipo di livello\n");
  printf("l\tvalore di livello 1\n");
  printf("L\tvalore di livello 2\n");
  printf("s\testensione immagini synsat (DWD)\n");
  printf("N\tnumero progressivo del GRIB analizzato\n");
  printf("G\thex dump della sezione 2 del GRIB analizzato\n");
  printf("%%\tcarattere %%\n");
  printf("\nOgni sequenza %%<C-format><char> nel descrittore di formato, viene sostituita\n");
  printf("con il corrispondente valore tratto dal record grib corrente, stampato\n");
  printf("utilizzando il descrittore di formato C %%<C-format>d.\n");
  printf("Le sequenze di caratteri che non ricadono nella definizione precedente\n");
  printf("vanno a comporre immutate il nome file di uscita.\n");
  printf("Il formato congeniale a cong (opzione -c) e` il seguente:\n");
  puts(FO_CONG);
}


/* Read from the GRIB message buffer 'kgrib' what is asked by 'hoper' and
   fill with data 'ksecn' and 'psecn' vectors */

#ifdef _CRAY
void F77_FUNC(gribex,GRIBEX) (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4,
		psec4,klenp,kgrib,kleng,kword,hoper,l1,kret)
#else
void F77_FUNC(gribex,GRIBEX) (ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4,
		psec4,klenp,kgrib,kleng,kword,hoper,kret,l1)
#endif
     gribint *ksec0;  /* integer data of section0 */
     gribint *ksec1;  /* integer data of section1 */
     gribint *ksec2;  /* integer data of section2 */
     gribint *ksec3;  /* integer data of section3 */
     gribint *ksec4;  /* integer data of section4 */
     gribfloat *psec2;     /* real data of section2 */
     gribfloat *psec3;     /* real data of section3 */
     gribfloat *psec4;     /* real data of section4 */
     gribint *klenp;  /* length of array psec4 */
     unsigned char *kgrib;      /* GRIB message buffer */
     gribint *kleng;  /* length of array kgrib */
     char *hoper;      /* requested function code */
     long l1;
     gribint *kret;   /* return status value */
     gribint *kword;  /* unused for decoding */
{
  unsigned char     *grib_buffer;
  struct grib_type  grib_msg;
  gribfloat		    no_bit_value;
  gribint	    ignore_bitmap;
  gribint	    i;
  char		    code;

  grib_buffer = kgrib;
  no_bit_value = psec3[1];
  ignore_bitmap = FALSE;
  switch (*hoper)
  {
    case 'D' : code = GRIB_DECODE; break;
    case 'J' : code = GRIB_MOREINFO; break;
    case 'I' : code = GRIB_INFO; break;
    case 'L' : code = GRIB_LENGTH; break;
  }

  *kret = degrib(code, &grib_buffer, ignore_bitmap,
		 &grib_msg, &no_bit_value);

  *klenp = grib_msg.section4.unpacked_values;

  ksec0[0] = grib_msg.section0.total_length;
  ksec0[1] = grib_msg.section0.grib_edition;

  if (*hoper == 'L') return;

  ksec1[0] =  grib_msg.section1.table2_version;
  ksec1[1] =  grib_msg.section1.centre;
  ksec1[2] =  grib_msg.section1.process;
  ksec1[3] =  grib_msg.section1.grid_definition;
  ksec1[4] =  grib_msg.section1.flag;
  ksec1[5] =  grib_msg.section1.parameter;
  ksec1[6] =  grib_msg.section1.level_type;
  ksec1[7] =  grib_msg.section1.measure1;
  ksec1[8] =  grib_msg.section1.measure2;
  ksec1[9] =  grib_msg.section1.year;
  ksec1[10] = grib_msg.section1.month;
  ksec1[11] = grib_msg.section1.day;
  ksec1[12] = grib_msg.section1.hour;
  ksec1[13] = grib_msg.section1.minute;
  ksec1[14] = grib_msg.section1.time_range_unit;
  ksec1[15] = grib_msg.section1.time_period1;
  ksec1[16] = grib_msg.section1.time_period2;
  ksec1[17] = grib_msg.section1.time_range;
  ksec1[18] = grib_msg.section1.number_in_average;
  ksec1[19] = grib_msg.section1.number_not_in_average;
  ksec1[20] = grib_msg.section1.reference_century;
  ksec1[21] = grib_msg.section1.reserved1;
  ksec1[22] = grib_msg.section1.decimal_scale_factor;
  ksec1[23] = 0;
  for (i=24;i<=35;i++)
    ksec1[i] = 0;
  if (grib_msg.section1.local_definition == 1 ||
      grib_msg.section1.local_definition == 2)

    {ksec1[36] = grib_msg.section1.local_definition;
    ksec1[37] = grib_msg.section1.class;
    ksec1[38] = grib_msg.section1.type;
    ksec1[39] = grib_msg.section1.stream;
    ksec1[40] = grib_msg.section1.experiment_version;

    switch (grib_msg.section1.local_definition)
      {case 1 :
	ksec1[41] = grib_msg.section1.ensemble_forecast;
        ksec1[42] = grib_msg.section1.forecasts_total;
	break;
      case 2 :
	ksec1[41] = grib_msg.section1.cluster;
	ksec1[42] = grib_msg.section1.clusters_total;
	ksec1[43] = grib_msg.section1.clustering_method;
	ksec1[44] = grib_msg.section1.start_time_step;
	ksec1[45] = grib_msg.section1.end_time_step;
	ksec1[46] = grib_msg.section1.northern_latitude;
	ksec1[47] = grib_msg.section1.western_longitude;
	ksec1[48] = grib_msg.section1.southern_latitude;
	ksec1[49] = grib_msg.section1.eastern_longitude;
	ksec1[50] = grib_msg.section1.operational_cluster;
	ksec1[51] = grib_msg.section1.control_cluster;
	ksec1[52] = grib_msg.section1.cluster_forecasts;
	for (i=53;i<(53+grib_msg.section1.cluster_forecasts);i++)
	  ksec1[i] = grib_msg.section1.forecasts_list[i-53];
	free(grib_msg.section1.forecasts_list); /*Free allocated memory*/
	break;
      }

    }

  ksec2[0] =  grib_msg.section2.data_representation;
  ksec2[1] =  grib_msg.section2.parallel_points;
  ksec2[2] =  grib_msg.section2.meridian_points;
  ksec2[3] =  grib_msg.section2.first_latitude;
  ksec2[4] =  grib_msg.section2.first_longitude;
  ksec2[5] =  (grib_msg.section2.resolution_and_flags & 0x80);
  ksec2[6] =  grib_msg.section2.last_latitude;
  ksec2[7] =  grib_msg.section2.last_longitude;
  ksec2[8] =  grib_msg.section2.parallel_increment;
  ksec2[9] =  grib_msg.section2.meridian_increment;
  if ((grib_msg.section2.parallel_increment & 0xFFFF) == 0xFFFF &&
      (grib_msg.section2.meridian_increment & 0xFFFF) == 0xFFFF)
    ksec2[5] =  0;
  ksec2[10] = grib_msg.section2.scanning_mode;
  ksec2[11] = grib_msg.section2.vertical_parameters;
  ksec2[12] = grib_msg.section2.southern_pole_latitude;
  ksec2[13] = grib_msg.section2.southern_pole_longitude;
  ksec2[14] = grib_msg.section2.stretching_pole_latitude;
  ksec2[15] = grib_msg.section2.stretching_pole_longitude;
  ksec2[16] = 0;
  ksec2[17] = (grib_msg.section2.resolution_and_flags & 0x40);
  ksec2[18] = (grib_msg.section2.resolution_and_flags & 0x08);
  ksec2[19] = 0;
  ksec2[20] = 0;
  ksec2[21] = 0;

  if (grib_msg.section2.data_representation == 1) { /* Mercatore */
    ksec2[8] = grib_msg.section2.latin1;
    ksec2[9] =  0;
    ksec2[12] = grib_msg.section2.parallel_increment;
    ksec2[13] = grib_msg.section2.meridian_increment;
  }

  if (grib_msg.section2.data_representation == 3 || /* Lambert */
      grib_msg.section2.data_representation == 5 || /* stereographic */
      grib_msg.section2.data_representation == 8) { /* Albers */
    ksec2[6] = grib_msg.section2.lov;
    ksec2[7] = 0;
    ksec2[12] = grib_msg.section2.centre_flag;
    ksec2[13] = grib_msg.section2.latin1;
    ksec2[14] = grib_msg.section2.latin2;
    ksec2[15] = 0;
    ksec2[19] = grib_msg.section2.southern_pole_latitude;
    ksec2[20] = grib_msg.section2.southern_pole_longitude;
  }

  psec2[0] = grib_msg.section2.rotation_angle;
  psec2[1] = grib_msg.section2.stretching_factor;
  for (i=2;i<10;i++)
    psec2[i] = 0;
  for (i=0;i<grib_msg.section2.vertical_parameters;i++)
    psec2[i+10] = grib_msg.section2.lisvercorpar[i];

  if (*hoper == 'I') return;

  ksec3[0] =  grib_msg.section3.bitmap_code;
  if (*hoper == 'D')
    if (grib_msg.section3.bitmap_code == 0) free(grib_msg.section3.bitmap);

  ksec4[0] =  grib_msg.section4.unpacked_values;
  ksec4[1] =  grib_msg.section4.bits_number;
  ksec4[2] =  (grib_msg.section4.flag_and_unused_bits & 0x80);
  ksec4[3] =  (grib_msg.section4.flag_and_unused_bits & 0x40);
  ksec4[4] =  (grib_msg.section4.flag_and_unused_bits & 0x20);
  ksec4[5] =  (grib_msg.section4.flag_and_unused_bits & 0x10);
  ksec4[6] =  0;
  ksec4[7] =  0;
  ksec4[8] =  0;
  ksec4[9] =  0;
  ksec4[10] = 0;
  for (i=11;i<42;i++)
    ksec4[i] = 0;

  if (*hoper == 'D') {
    if (grib_msg.section4.work_with_floats) {
      for (i=0;i<grib_msg.section4.unpacked_values;i++)
	psec4[i] = * ((gribfloat *) grib_msg.section4.binary_data + i);
    } else {
      for (i=0;i<grib_msg.section4.unpacked_values;i++)
	psec4[i] = * ((gribint *) grib_msg.section4.binary_data + i);
    }
    free(grib_msg.section4.binary_data);
  }
 }
