#include <stdio.h>

#include "winds.h"
#include "buffer.h"
#include "gribinfo.h"

long bword (char *, int loc, int n);	/* return n bytes at location loc */
int  x9ie32 (void *input, float *output, int number_of_words);
int lscale (unsigned short int);
int gridproj (void);

extern int bug;

int gribinfo ()
{
 static long indicator = 0x47524942;	/* GRIB */
 const int lenis = 8;	/* length of IS Indicator Section always 8 */
 long lenpds;		/* length of Product Definition Section in bytes PDS */
 long locpds;		/* location of Product Definition Section, bytes PDS */
 long lengds;		/* length of Grid Description Section in bytes GDS */
 long lenbds;		/* length of Binary Data Section in bytes BDS */
 int flag_gds_bms;
 unsigned short int scaledx;	/* decimal scaling factor with sign bit */
 unsigned short int scalebx;	/* binary scaling factor with sign bit */
 int dscale;	/* decimal scaling factor, negative is twos compliment */
 int bscale;	/* decimal scaling factor, negative is twos compliment */
 int qno;
 int stat;	/* status of call to x9ie32 & gribinfo */
 int n_bits;	/* number of bits for a packed data point */
 long ref_val_ibm;
 float ref_val;
 int i_param;	/* parameter, type of grid */
 long i_level;	/* indicator of level, 100 is pressure level in mb */

 locpds = lenis;
 gi.locpds = lenis;
 lenpds = bword ( (char *)buf, locpds, 3);
 flag_gds_bms = bword ( (char *)buf, locpds+7, 1);
 gi.lenpds = lenpds;
 gi.locgds = locpds + lenpds;
 if ((flag_gds_bms & 0x00000080) > 0)
  {lengds = bword ( (char *)buf, gi.locgds, 3);}
  else
  {lengds = 0;}	/* grid descriptor section not present */
 gi.lengds = lengds;
 gi.locbms = gi.locgds + lengds;
 if ((flag_gds_bms & 0x00000040) > 0)
  {gi.lenbms = bword ( (char *)buf, gi.locbms, 3);}
  else
  {gi.lenbms = 0;}	/* bit map section not present */
 gi.locbds = gi.locbms + gi.lenbms;
 lenbds = bword ( (char *)buf, gi.locbds, 3);
 gi.lenbds = bword ( (char *)buf, gi.locbds, 3);

 gi.grid_id = bword ( (char *)buf, locpds+6, 1);
 n_bits  = bword ( (char *)buf, gi.locbds+10, 1);
 scaledx = bword ( (char *)buf, locpds+26, 2);
 scalebx = bword ( (char *)buf, gi.locbds+4, 2);
 dscale = lscale(scaledx);
 bscale = lscale(scalebx);
 ref_val_ibm = bword ( (char *)buf, gi.locbds+6, 4);
 qno = 1;	/* number of words to convert from IBM to IEEE format */
 stat = x9ie32 (&ref_val_ibm,&ref_val,qno);
 if (stat != 0) {printf("x9ie32 error %d\n", stat); return -1;}
 if (bug > 3) {
  printf("%08x %08x %08x %08x %08x %08x %14.6e",
    scaledx,scalebx, dscale,bscale, ref_val_ibm,ref_val,ref_val);
  printf(" %08x ",ref_val); /* DEBUG */
  printf("%14.6e\n", ref_val); }
 stat = gridproj ();
 if (stat != 0) {printf("error return from gridproj %d\n",stat); return -2;}
 i_param = bword ( (char *)buf, locpds+8, 1);
 i_level = bword ( (char *)buf, locpds+9, 1);
 if (i_level == 100) {gi.level = bword ( (char *)buf, locpds+10, 2);}

 /* Determine if this is a wind field */
 if (i_param == winds.u_id  || i_param == winds.v_id)
  {gi.windflag = 1;}  else  {gi.windflag = 0;}
 return 0;
}
