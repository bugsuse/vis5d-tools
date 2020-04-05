#include <stdio.h>
#include <fcntl.h>	/* open */
#include <unistd.h>	/* read, write, lseek, close */
#include <errno.h>

#include "want.h"
#include "buffer.h"

int unpack5(void);
int hrzintp5(int ixg);

extern int narg; extern char **varg ;
extern int bug;
extern char cerror[120];	/* for error messages */

int fetch5 (int ixt)
{
 int ixg;
 int fdi;	/* file number */
 int stat;
 static int cur_file = -1;	/* index of current file open */
 static int n_bytes, n_read, lpos;

 if(bug>3) printf("in fetch5 %d\n",ixt); if(bug>3) fflush(NULL); /* DEBUG */
 for (ixg=0; ixg<ts[ixt].num; ixg++)
  {
   if (ts[ixt].len[ixg] > BUFF*4)
    {printf("grid record size %d > %d read buffer\n", ts[ixt].len[ixg], BUFF*4);
     continue; }
   if (cur_file != ts[ixt].fil[ixg])
    {if (cur_file > 0) { close (fdi); }
     cur_file = ts[ixt].fil[ixg];
     if (bug>3) printf ("change files %2d %s\n", cur_file,want.file[cur_file]);
     fdi = open (want.file[cur_file], O_RDONLY, 0);
     if (fdi < 0)
      {sprintf(cerror,"%s: fetch5: open error o1 errno= %d file: %s\n",
              varg[0],errno,want.file[cur_file]);
       perror(cerror); printf(cerror); return -1;}
    }
   if (lpos = lseek(fdi, ts[ixt].loc[ixg], 0) < 0)
    {sprintf(cerror,"%s: lseek error l1 errno= %d fdi: %d %d %d\n",
       varg[0],errno,fdi,ts[ixt].loc[ixg],lpos);
     perror(cerror); printf(cerror); return -1;}
   errno = 0;  n_bytes = ts[ixt].len[ixg];
   n_read = read (fdi, buf, n_bytes);
   if ( (n_read != n_bytes && errno != 0) || (n_read != n_bytes) )
    {sprintf(cerror,"%s: read error r1 errno= %d fdi: %d "
       " %d %d\n",varg[0],errno,fdi,n_read,n_bytes);
     printf(cerror); perror(cerror); }
   if (bug > 3) {
     printf("%8d %8d %8d ", ts[ixt].loc[ixg], lpos, n_bytes);  /* DEBUG */
     printf("%08x %08x\n",buf[0],buf[1]);} /* DEBUG */
   stat = unpack5 ();
   if (stat != 0) {printf("error return from unpack5\n"); return -1;}
   stat = hrzintp5 (ixg);
   if (stat != 0) {printf("error return from hrzintp5\n"); return -1;}
  }

 close (fdi);
 if (bug > 3) printf("exit fetch5\n"); if (bug > 3) fflush(NULL); /* DEBUG */
 return 0;
}
