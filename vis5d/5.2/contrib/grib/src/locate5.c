#include <stdio.h>
#include <string.h>
#include <fcntl.h>	/* open */
#include <unistd.h>	/* read, write, lseek, close */
#include <errno.h>
#include <math.h>

#include "want.h"
#include "buffer.h"

long bword (void *, int loc, int n);	/* return n bytes at location loc */
void gsdate (int  day, int  mon, int  year, long *serial); /* serial date */

extern int narg;
extern char **varg ;
extern int bug;
extern char cerror[120];	/* for error messages */

int locate5 (int ixf)
{
 static int fdi, n_bytes, n_read;
 static long lpos;
 static int eof_flag;	/* end of file flag =1 for no eof */
 static int i,j; /* indices */
 int igd = -1;		/* index for grids in file being saved */
 static int cur_byte;	/* current byte of file */
 int len;	/* length of current grib record */
 int loc;	/* location of current grib record */
 int loc_next;	/* location of next grib record */
 int loc_last=-1;	/* location of last grid read in.  for debugging */
 int len_last; 		/* length of last grid read in.  for debugging */
 int ix_grid = 0;	/* sequential grid number in file */
 const int lenis = 8;	/* length of IS Indicator Section always 8 */
 int lenpds;		/* length of Product Definition Section in bytes PDS */
 int locpds;		/* location of Product Definition Section, bytes PDS */
 int lengds;		/* length of Grid Description Section in bytes GDS */
 int lenbds;		/* length of Binary Data Section in bytes BDS */
 int lenbms;		/* length of Bit Map Section in bytes BMS */
 int locgds, locbms, locbds;
 int flag_gds_bms;
 int grid_id;		/* grid identification byte 7 */
 int i_param, i_level, level, year, month, day, hour, fcst_un, per1;
 int time_range;
 long serial_date;	/* serial date sence 1 Jan 1990 from gsdate */

 if (bug > 3) printf("in locate5 file number: %2d\n",ixf);
 if (bug > 3) printf("process file %s\n",want.file[ixf]);
 if (bug > 3) printf("grid id %d\n", want.grid_id[ixf]);  /* DEBUG */
 if (bug > 3) fflush(NULL); /* DEBUG */

 floc[ixf].start_date = -1.0;	/* initialize serial date */
 fdi = open (want.file[ixf], O_RDONLY, 0);
 if (fdi < 0)
  {sprintf(cerror,"%s: locate5: open error o1 errno= %d file: %s\n",
          varg[0],errno,want.file[ixf]);
   perror(cerror); printf(cerror); return -1;}
 cur_byte = 0;		/* set current byte to zero at start */
 eof_flag = 1;		/* set end of file flag to no end of file */

 if (bug > 3) {printf("in locate5\n");}
 while (eof_flag)
 {
  errno = 0;  n_bytes = BUFF * 4;
  n_read = read (fdi, buf, n_bytes);
  if (n_read != n_bytes && errno != 0)
   {sprintf(cerror,"%s: read error r1 errno= %d fdi: %d "
      " %d %d\n",varg[0],errno,fdi,n_read,n_bytes);
    printf(cerror); perror(cerror); }
  if (bug > 3) printf("read %d\n",n_read);  /* DEBUG */
  if(n_read < n_bytes) {eof_flag = 0;}
               /*end of file because not all bytes read*/
  for (j=0; j<n_read; j++)
   {if (strncmp((char *)buf+j, "GRIB", 4) == 0)
    {
     if (bug > 3 ) printf("found GRIB %d\n",j); /* DEBUG */
     /*if (cur_byte+j <42) continue;*/	/* avoid header BUT there is no header*/
     loc = cur_byte + j;
     len = bword (buf, (j+4), 3);
     loc_next = cur_byte + j + len;	/* location of next grib record */
     if (loc_last < 0) {loc_last = loc; len_last = 0;}
     if ( abs(loc_last+len_last-loc) > 4 )
      {printf("sync error %d %d %d %d\n", loc,len,loc_last,len_last);}
     loc_last = loc;	len_last = len;
     /* The current record (at location loc) plus 40 bytes must be in the 
        buffer.  If not then refill the buffer starting at the current record
        location.  Do not continue with processing as this current record will
        be processed after the read. */
     if ( (j+40) > n_read )
      {if (lpos = lseek(fdi, loc, 0) < 0)
        {sprintf(cerror,"%s: locate5: lseek error l1 errno= %d fdi: %d %d %d\n",
           varg[0],errno,fdi,loc,lpos);
         perror(cerror); printf(cerror); return -1;}
       cur_byte = loc;
       /*printf("lseek: %d\n", loc_next);*/ /* DEBUG */
       goto next_read;
      }
     /* Process this record.  Check whether it is needed */
     ix_grid++;
     locpds = j + lenis;
     lenpds = bword ( buf, locpds, 3);
     flag_gds_bms = bword ( (char *)buf, locpds+7, 1);
     locgds = locpds + lenpds;
     if ((flag_gds_bms & 0x00000080) > 0)
      {lengds = bword ( (char *)buf, locgds, 3);}
      else
      {lengds = 0;}	/* grid descriptor section not present */
     locbms = locgds + lengds;
     if ((flag_gds_bms & 0x00000040) > 0)
      {lenbms = bword ( (char *)buf, locgds, 3);}
      else
      {lenbms = 0;}	/* bit map section not present */
     locbds = locbms + lenbms;
     lenbds = bword ( (char *)buf, locbds, 3);
     if (flag_gds_bms != 0  &&  bug > 3)
        {printf("GDS or BMS present %08x %d %d\n",flag_gds_bms,lengds,lenbms);}
     grid_id = bword ( (char *)buf, locpds+6, 1);
     i_param = bword ( (char *)buf, locpds+8, 1); /*type of parameter*/
     i_level = bword ( (char *)buf, locpds+9, 1); /*indicator of type of level*/
     level   = bword ( (char *)buf, locpds+10,2);
     year    = bword ( (char *)buf, locpds+12,1);
     month   = bword ( (char *)buf, locpds+13,1);
     day     = bword ( (char *)buf, locpds+14,1);
     hour    = bword ( (char *)buf, locpds+15,1);
     fcst_un = bword ( (char *)buf, locpds+17,1);
     per1    = bword ( (char *)buf, locpds+18,1);
     time_range = bword ( (char *)buf, locpds+20,1);
     if (time_range == 10) {per1    = bword ( (char *)buf, locpds+18,2);}
     gsdate ( day, month, year, &serial_date);
     if (bug > 3)
      {printf("%3d %6d %8d %6d %8d ",ix_grid,j,loc,len,loc_next); /* DEBUG */
       printf("%.4s", (char *)buf+j); /* DEBUG */
       printf("%4d %4d %4d %5d ",grid_id,i_param,i_level,level);
       printf("%2d %2d %2d %3d %3d %3d\n",
            year,month,day,hour,fcst_un,per1); }	/* DEBUG */	
     if (want.grid_id[ixf] != grid_id) continue;
     /*if (grid_id != want.ktype) continue;*/	/* must be specified ktype */
     /*if (i_level != 100) continue;*/	/* assume only pressure levels */
     if (want.fcst_hr[ixf] != per1) continue;
     for (i=0; i<NVAR; i++)
      {if (i_param == want.vars[i]  &&  i_level == want.lev_ind[i])
        {if (bug>3) printf("want this grid %2d %3d %4d %8ld %8d %6d\n",
            i,i_param,level,serial_date,loc,len);
         if (floc[ixf].start_date < 0.0)
          {floc[ixf].start_date = (double)serial_date + (double)hour/24.0;
           floc[ixf].valid_time = floc[ixf].start_date + (double)per1/24.0;}
         if (igd > NGRID-2)
          {printf("NGRID exceeded in locate5\n"); close(fdi); return -2;}
         if (len > BUFF*4)
          {printf("locate: grid record size %d > %d read buffer\n",
            len, BUFF*4);
           continue; }
         igd++;
         floc[ixf].loc[igd] = loc;
         floc[ixf].len[igd] = len;
         floc[ixf].type[igd] = i_param;
         floc[ixf].level[igd] = level;
        }
      }
    }
   }
  /* read in next block. back up 4 bytes incase string GRIB is between blocks */
  cur_byte = cur_byte + n_read - 4;
  if (lpos = lseek(fdi, cur_byte, 0) < 0)
   {sprintf(cerror,"%s: locate5: lseek error l2 errno= %d fdi: %d %d %d\n",
      varg[0],errno,fdi,cur_byte,lpos);
    perror(cerror); printf(cerror); return -1;}
  next_read: ;
 }

 floc[ixf].num_grids = igd + 1;
 close (fdi);
 if (bug > 3) printf("exit locate5\n"); if (bug > 3) fflush(NULL); /* DEBUG */
 return 0;
}
