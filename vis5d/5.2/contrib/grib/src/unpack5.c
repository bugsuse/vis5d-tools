#include <stdio.h>
#include <math.h>

#include "buffer.h"
#include "gribinfo.h"

long bword (char *, int loc, int n);	/* return n bytes at location loc */
long exbits (int byte_loc, int byte_len, void *buf);
int  x9ie32 (void *input, float *output, int number_of_words);
int lscale (unsigned short int);
int gribinfo(void);

extern int bug;
extern int pvv_flag;	/* presence of pvv field flag, set in vfetch5 */

int unpack5 (void)
{
 int i;		/* index */
 static long indicator = 0x47524942;	/* GRIB */
 const int lenis = 8;	/* length of IS Indicator Section always 8 */
 long lenpds;		/* length of Product Definition Section in bytes PDS */
 long locpds;		/* location of Product Definition Section, bytes PDS */
 long lengds;		/* length of Grid Description Section in bytes GDS */
 long locgds;		/* location of Grid Description Section in bytes GDS */
 long lenbds;		/* length of Binary Data Section in bytes BDS */
 long lenbms;		/* length of Bit Map Section in bytes BMS */
 long locbms, locbds;
 int flag_gds_bms;
 int grid_id;		/* grid identification byte 7 */
 unsigned short int scaledx;	/* decimal scaling factor with sign bit */
 unsigned short int scalebx;	/* binary scaling factor with sign bit */
 int dscale;	/* decimal scaling factor, negative is twos compliment */
 int bscale;	/* decimal scaling factor, negative is twos compliment */
 int qno;
 int stat;	/* status of call to x9ie32 & gribinfo */
 int n_bits;	/* number of bits for a packed data point */
 int is;	/* index for unpacking grid, byte based */
 long exw;	/* packed value for one gridpoint */
 long ref_val_ibm;
 float ref_val;
 float Min, Max;	/* min-max values of unpacked grid */
 float bsc, dsc;	/* binary and decimal scaling factors */
 float fexw;	/* floating point unpacked gridpoint vallue */
 const float rho = 1.225;	/* kg/m**3 */
 const float g   = 9.81;	/* m/s**2 */
 const float pvvscale = -1./(rho*g);
 
 if (bug>3) printf("in unpack5\n"); if(bug>3) fflush(NULL); /* DEBUG */
 if(buf[0] != indicator) {printf("incorrect indicator %.4s %08x\n",buf,buf[0]);}
 stat = gribinfo ();
 if (stat == -2) {printf("error return from gribinfo\n"); return -2;}
 if (stat !=  0) {printf("error return from gribinfo\n"); return -1;}
 locpds = lenis;
 lenpds = bword ( (char *)buf, locpds, 3);
 flag_gds_bms = bword ( (char *)buf, locpds+7, 1);
 locgds = locpds + lenpds;
 if ((flag_gds_bms & 0x00000080) > 0)
  {lengds = bword ( (char *)buf, locgds, 3);}
  else
  {lengds = 0;}	/* grid descriptor section not present */
 locbms = locgds + lengds;
 if ((flag_gds_bms & 0x00000040) > 0)
  {lenbms = bword ( (char *)buf, locbms, 3);}
  else
  {lenbms = 0;}	/* bit map section not present */
 locbds = locbms + lenbms;
 lenbds = bword ( (char *)buf, locbds, 3);

 grid_id = bword ( (char *)buf, locpds+6, 1);
 n_bits  = bword ( (char *)buf, locbds+10, 1);
 scaledx = bword ( (char *)buf, locpds+26, 2);
 scalebx = bword ( (char *)buf, locbds+4, 2);
 dscale = lscale(scaledx);
 bscale = lscale(scalebx);
 ref_val_ibm = bword ( (char *)buf, locbds+6, 4);
 qno = 1;	/* number of words to convert from IBM to IEEE format */
 stat = x9ie32 (&ref_val_ibm,&ref_val,qno);
 if (stat != 0) {printf("x9ie32 error \n"); return -1;}
 if (bug > 3) printf("%04x %04x %4d %4d %f\n",	 		/* DEBUG */
        scaledx,scalebx, dscale,bscale, ref_val); /* DEBUG */
 Min = 99999999e2;  Max = -Min;
 if(bscale != 0) {bsc = pow(2.0, (double)bscale);} else {bsc = 1.0;}
 if(dscale != 0) {dsc = pow(10., (double)dscale);} else {dsc = 1.0;}
 is = (locbds + 11) * 8;
 for (i=0; i<gi.gridsize; i++)
  {
   exw = exbits(is,n_bits,buf); is = is + n_bits;
   fexw = (float)exw;
   if (bscale != 0) {fexw = fexw * bsc;}
   fexw = (fexw + ref_val) / dsc;
   exw = (int)fexw;	/* should do scaling here */
   /*jbuff[i] = exw;*/
   ubuf[i] = fexw;
   if (fexw < Min) Min = fexw;  if (fexw > Max) Max = fexw;
  }
 if (bug > 3) printf("min-max %14.6e %14.6e %5d\n", Min, Max, gi.gridsize);
 if (pvv_flag)
  {for(i=0; i<gi.gridsize; i++) ubuf[i] = ubuf[i] * pvvscale;
   if (bug>3) printf("scaled by %f\n", pvvscale); }

 if (bug > 3) printf("exit unpack5\n"); if (bug > 3) fflush(NULL); /* DEBUG */
 return 0;
}
