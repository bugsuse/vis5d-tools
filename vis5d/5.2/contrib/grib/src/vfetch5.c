#include <stdio.h>
#include <fcntl.h>	/* open */
#include <unistd.h>	/* read, write, lseek, close */
#include <errno.h>

#include "want.h"
#include "winds.h"
#include "buffer.h"
#include "gribinfo.h"

extern int narg;
extern char **varg ;
extern int bug;
extern char cerror[120];	/* for error messages */

int unpack5(void);
int vhrzint5(int ixg);

int pvv_flag;		/* NMC mod */

int vfetch5 (int ixv)
{
 int fdi;	/* file number */
 int stat;
 int ixg;	/* index for grib record loop */
 static int cur_file = -1;	/* index of current file open */
 static int n_bytes, n_read, lpos;
 const long PVV = 0x50565620;	/* ascii 'PVV ' */

 if (bug > 3) printf("in vfetch5 %d %.4s\n",ixv,&want.var_name[ixv]);
 if (bug>3)fflush(NULL); /*DEBUG*/
 /* NMC mod start */
 /*if (want.vars[ixv] == 39)*/
 if (want.var_name[ixv] == PVV)
  {if (bug>3) printf("PVV grids, will do scaling for NMC\n");
   pvv_flag = 1;	/* true or field present */
  }
  else {pvv_flag = 0;}
 /* NMC mod end */
 for (ixg=0; ixg<vs[ixv].num; ixg++)
  {
   if (cur_file != vs[ixv].fil[ixg])
    {if (cur_file > 0) { close (fdi); }
     cur_file = vs[ixv].fil[ixg];
     if (bug>3)printf ("change files %2d %s\n", cur_file,want.file[cur_file]);
     fdi = open (want.file[cur_file], O_RDONLY, 0);
     if (fdi < 0)
      {sprintf(cerror,"%s: vfetch5: open error o1 errno= %d file: %s\n",
              varg[0],errno,want.file[cur_file]);
       perror(cerror); printf(cerror); return -1;}
    }
   if (lpos = lseek(fdi, vs[ixv].loc[ixg], 0) < 0)
    {sprintf(cerror,"%s: vfetch5 lseek error l1 errno= %d fdi: %d %d %d\n",
       varg[0],errno,fdi,vs[ixv].loc[ixg],lpos);
     perror(cerror); printf(cerror); return -1;}
   errno = 0;  n_bytes = vs[ixv].len[ixg];
   n_read = read (fdi, buf, n_bytes);
   if ( (n_read != n_bytes && errno != 0) || (n_read != n_bytes) )
    {sprintf(cerror,"%s: read error r1 errno= %d fdi: %d "
       " %d %d\n",varg[0],errno,fdi,n_read,n_bytes);
     printf(cerror); perror(cerror); }
   stat = unpack5 ();
   if (stat != 0) {printf("error return from unpack5\n"); return -1;}
   stat = vhrzint5 (ixg);
   if (stat != 0) {printf("error return from vhrzint5\n"); return -1;}
   /* If this is a wind then save the projection type. */
   if (gi.windflag)
    {
     if (bug > 3) printf("wind %d\n", winds.grid_id);
     if (winds.grid_id == -1)
      {winds.grid_id = gi.grid_id;
       winds.projtype = gi.projtype;
       winds.npi = gi.npi;
       winds.npj = gi.npj;
       winds.mesh = gi.mesh;
       winds.rotat = gi.rotat;}
     else
      {if (winds.grid_id != gi.grid_id)
        {if(bug>3)printf("windgrid_idchange %d %d\n",winds.grid_id,gi.grid_id);}
      }
    }
  }

 close (fdi);
 cur_file = -1;	/* set to indicate that no file is open */
 if (bug > 3) {printf("exit fetch5\n"); fflush(NULL);} /* DEBUG */
 return 0;
}
