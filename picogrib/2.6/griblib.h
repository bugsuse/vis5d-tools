/*
c    <picogrib software di decodifica grib>
c    Copyright (C) 2000  <SMR ARPA>
c
c    $Date: 2005-11-24 15:28:03 +0100 (gio, 24 nov 2005) $    $Revision: 53 $
c    $Id: griblib.h 53 2005-11-24 14:28:03Z cvs $

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
void read_bytes_smr();
void read_bytes_in_ints();
void read_var_len_int();
void manage_new_sign();
long int load_grib_file();
long int save_grib_file();
void unpack_grib();
long int decode_lengths();
void decode_section0();
void decode_section1();
long int decode_section2();
void decode_section3();
long int decode_section4();
long int decode_section5();
long int degrib();
void print_grib();
double sind();
double cosd();
double asind();
double acosd();
void rot_grib_lambo();
void rtlld();
void summarize_grib();
void grib_cformat(int, unsigned char *, struct grib_type *, char *, char *, int);
void format_usage(char *);
void gribex();

/*

The next three functions read bytes, starting from the address *buffer_in.
This address is passed by reference so it can be moved on the next byte
to be read


Read 'bytes_num' bytes from 'buffer_in' and write them in 'buffer_out'

 void read_bytes_smr(buffer_in,bytes_num,buffer_out)
  unsigned char **buffer_in;    address of input buffer (by reference)
  int           bytes_num;      number of bytes to copy
  unsigned char *buffer_out;    output buffer where bytes have to be copied


Read 'bytes_num' bytes from 'buffer_in' and write them in 'buffer_out'
as an array of integer

 void read_bytes_in_ints(buffer_in,bytes_num,buffer_out)
  unsigned char **buffer_in;    address of input buffer (by reference)
  int bytes_num;                number of bytes to copy
  long int *buffer_out;         output buffer where integers have to be
				written


Read an integer of 'bytes_num' bytes from 'buffer_in' and
write it in value

 void read_var_len_int(buffer_in,bytes_num,value)
  unsigned char **buffer_in;    address of input buffer (by reference)
  int bytes_num;                size in bytes of the integer number to
				be read
  long int *value;              integer destination variable


Convert a signed long integer from the GRIB format

 void manage_new_sign(latlong_value)
  long int *latlong_value;      signed long integer number to be conterted


Load a GRIB from the file to 'grib_buffer'

 long int load_grib_file(grib_file,grib_buffer)
  FILE *grib_file;              input file pointer
  unsigned char **grib_buffer;  buffer where bytes have to be copied


Save a GRIB from 'grib_buffer' to a file

 long int save_grib_file(grib_buffer,size,grib_msg)
  unsigned char *grib_buffer;	buffer where the GRIB message is
  long int size;		size of grib_buffer
  struct grib_type grib_msg;	structure of read GRIB data


Write an array of values reading from a stream of bits and
using a bitmap

 void unpack_grib(bit_stream,no_bit_value,ignore_bitmap,grib_msg)
  unsigned char *bit_stream;    input bit-stream buffer
  float no_bit_value;           value to be written in output array when
				bitmap specifies that no data have been
				taken for a position
  long int ignore_bitmap;       flag that specifies if bitmap has to be
				considered or not
  struct grib_type *grib_msg;   structure of read GRIB data


Read only the lengths of the sections

 long int decode_lengths(section0,grib_msg)
  unsigned char **section0;     section 0 address (by reference)
  struct grib_type *grib_msg;   structure of read GRIB data


Read values from the buffer 'section0' to the data structure

 void decode_section0(section0,grib_msg)
  unsigned char **section0;     section 0 address (by reference)
  struct grib_type *grib_msg;   structure where GRIB data have to be written


Read values from the buffer 'section1' to the data structure

 void decode_section1(section1,grib_msg)
  unsigned char **section1;     section 1 address (by reference)
  struct grib_type *grib_msg;   structure where GRIB data have to be written


Read values from the buffer 'section2' to the data structure

 long int decode_section2(section2,grib_msg)
  unsigned char **section2;     section 2 address (by reference)
  struct grib_type *grib_msg;   structure where GRIB data have to be written


Read values from the buffer 'section3' to the data structure

 void decode_section3(section3,grib_msg)
  unsigned char **section3;     section 3 address (by reference)
  struct grib_type *grib_msg;   structure where GRIB data have to be written


Read values from the buffer 'section4' to the data structure

 long int decode_section4(section4,no_bit_value,ignore_bitmap,grib_msg)
  unsigned char **section4;     section 4 address (by reference)
  float no_bit_value;           structure of read GRIB data
  long int ignore_bitmap;       flag that specifies if bitmap has to be
				considered or not
  struct grib_type *grib_msg;   structure where GRIB data have to be written


Read values from the buffer 'section5' to the data structure and
check for the end-of-message pattern

 long int decode_section5(section5,grib_msg)
  unsigned char **section5;     section 5 address (by reference)
  struct grib_type *grib_msg;   structure where GRIB data have to be written


Read from 'grib_buffer' what is asked by 'operation' and
fill the data structure 'grib_msg'

 long int degrib(operation,grib_buffer,ignore_bitmap,grib_msg,no_bit_value)
  char operation;               code of information group to be extracted
  unsigned char **grib_buffer;  loaded GRIB data block
  long int ignore_bitmap;       flag that specifies if bitmap has to be
				considered or not
  struct grib_type *grib_msg;   structure where GRIB data have to be written
  float *no_bit_value;          value to be written in output array when
				bitmap specifies that no data have been
				taken for a position


Print as asked by 'operation' the data structure 'grib_msg'

 void print_grib(operation,grib_msg)
  char operation;               code of information group to be printed
  struct grib_type grib_msg;    structure of read GRIB data


The next four functions need an argument in degrees

 double sind(x)
  double x;                     angle in degrees


 double cosd(x)
  double x;                     angle in degrees


 double asind(x)
  double x;                     value whose arcsine is to be calculated


 double acosd(x)
  double x;                     value whose arccosine is to be calculated


 void rot_grib_lambo(alo2,ala2,tlm0d,tph0d)
  float alo2,ala2;
  float *tlm0d,*tph0d;


 void rtlld(alo1,ala1,tlm0d,ctph0,
	    stph0,alo2,ala2)
  float alo1,ala1,tlm0d,ctph0;
  float stph0;
  float *alo2,*ala2;


Write in an ASCII file the values of the data structure 'grib_msg',
if requested write also lat long coordinates

 void summarize_grib(grib_msg,show_lat_long,no_bit_value)
  struct grib_type grib_msg;    structure of read GRIB data
  long int show_lat_long;       flag that specifies if all GRIB data have
				to be written in the file with the 
				corresponding lat-long coordinate
  float *no_bit_value;          value to be written in output array when
				bitmap specifies that no data have been
				taken for a position




Read from the GRIB message buffer 'kgrib' what is asked by 'hoper' and
fill with data 'ksecn' and 'psecn' vectors

 void gribex(ksec0,ksec1,ksec2,psec2,ksec3,psec3,ksec4,
	     psec4,klenp,kgrib,kleng,kword,hoper,kret)
  long int *ksec0;		integer data of section0
  long int *ksec1;		integer data of section1
  long int *ksec2;		integer data of section2
  long int *ksec3;		integer data of section3
  long int *ksec4;		integer data of section4
  float *psec2;			real data of section2
  float *psec3;			real data of section3
  float *psec4;			real data of section4
  long int *klenp;		length of array psec4
  char *kgrib;			GRIB message buffer
  long int *kleng;		length of array kgrib
  char *hoper;			requested function code
  long int *kret;		return status value
  long int *kword;		unused


*/
