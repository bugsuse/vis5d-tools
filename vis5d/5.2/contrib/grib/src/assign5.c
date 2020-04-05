#include <stdio.h>
#include <string.h>

#include "want.h"
#include "winds.h"
#include "grid5d.h"

extern int narg;
extern char **varg ;
extern int bug;
extern char cerror[120];	/* for error messages */
#define CLINE 120		/* the length of cline */
#define VLEN 10			/* length of variable & code match arrays */
#define CHOLD 12

int assign5 (void)
{
 float lat, lon, inch;
 float base, incv;
 int i, j;		/* indices */
 int date, hour;	/* date & initial hour from input */
 int n_var = -1;	/* number of variables to collect */
 int n_files = -1;	/* number of grib files */
 int nrow, ncol, nver;	/* number of rows, columns, vertical levels */
 int str_len;		/* string length */
 int flag;
 FILE *fp;
 char cline[CLINE];	/* hold a line from input */
 char chold[CHOLD];
 /* var_nam will hold ascii character strings of 4 bytes.  These will be
    compared to input variable names to give the variable code, var_cod
    which is the code in the grib descriptor section GDS */
 long var_nam[VLEN] = {  0,  0, 0,  0,  0,  0,  0,  0,  0,  0};
 long var_lev[VLEN] = {100,100, 1,102,100,100,100,100,  1,  1};
 int  var_cod[VLEN] = {  7, 11, 1,  1, 33, 34, 52, 39, 11,  7};

 if (bug > 3) printf("in assign5\n"); if (bug > 3) fflush(NULL); /* DEBUG */
 strncpy ((char *)&var_nam[0], "Z   ",4);
 strncpy ((char *)&var_nam[1], "T   ",4);
 strncpy ((char *)&var_nam[2], "SFCP",4);
 strncpy ((char *)&var_nam[3], "MSLP",4);
 strncpy ((char *)&var_nam[4], "U   ",4);
 strncpy ((char *)&var_nam[5], "V   ",4);
 strncpy ((char *)&var_nam[6], "RH  ",4);	/* Relative Humidity */
 strncpy ((char *)&var_nam[7], "PVV ",4);	/* Pressure Vertical Velocity */
 strncpy ((char *)&var_nam[8], "SFCT",4);	/* Surface temperature */
 strncpy ((char *)&var_nam[9], "SFCZ",4);	/* Surface height */
 winds.u_id = 33;
 winds.v_id = 34;

 if (narg <2) {printf("file name not given as first command line parameter\n");
               return -1;}
 fp = fopen(varg[1],"r");

 /* read in the initial date (yyddd) and time (hour) (and (debug level) */
 if (fgets(cline,CLINE,fp) == NULL)
  {printf("missing line 1: YYDDD HOUR\n");}
 sscanf (cline, "%d %d %d", &date, &hour, &bug);
 if (bug > 3) printf ("date: %d hour: %d bug: %d\n",date,hour,bug);

 /* Read in the variables */
 /* Variables list in cline is in the format
    (optional space) (variable 1-4 characters) (spaces) (variable name) ..
    termanated by optional spaces and a new line (a \n).  Put each variable
    into chold left justified.  Then match with list of names in var_nam for
    the binary code recognized by grib. */
 if (fgets(cline,CLINE,fp) == NULL)
  {printf("missing line 2: the list of variables\n"); return -1;}
 if (bug >3) printf("%s", cline);
 str_len = strlen(cline);
 chold[0] = '\0';
 i = -1;	/* i is index for position in cline */
 while (1)
  {i++;
   if (i > str_len ) break;
   if (cline[i] == ' ' ) continue;	/* white space so skip */
   if (cline[i] == '\n') break;		/* end of string so break loop */
   j = 0;	/* found the start of a variable (non white space) */
   while (1)
    {
     if (j > CHOLD-2) {printf("string too long i,j %d %d\n",i,j); break;}
     chold[j] = cline[i];
     i++; j++;
     chold[j] = '\0';
     if (cline[i] <  ' ' ) break;	/* should not expect this */
     if (cline[i] == ' ' ) break;
    }
   if (bug > 3) printf("%s\n",chold);
   if (strlen(chold) > 4)
    {printf("string %s too long %d\n", chold, strlen(chold)); continue;}
   if (n_var >= NVAR-1)
    {printf("assign5 variable list exceeded (NVAR) %d\n",NVAR); break;}
   i--;		/* decriment i because it is incrimented at top of loop */
   /* search for a match of this variable with one in code match up list */
   for (j=0; j<VLEN; j++)
    {if (strncmp(chold, (char *)&var_nam[j], strlen(chold)) == 0)
      {if (bug>3) printf("variable match %s\n",chold);
       n_var++;
       want.vars[n_var] = var_cod[j];
       want.lev_ind[n_var] = var_lev[j];
       /* save the variable name as coded */
       want.var_name[n_var] = var_nam[j];
       break; }
    }
  }
 want.nvars = n_var + 1;
 if (bug > 0) printf ("number of variables %d\n", want.nvars);
 if (bug > 0) {
  for (i=0; i<want.nvars; i++)
     printf("%.4s %4d %4d\n",&want.var_name[i],want.vars[i],want.lev_ind[i]); }

 /* Read in number of rows, columns, vertical levels */
 if (fgets(cline,CLINE,fp) == NULL)
  {printf("missing line 3: rows-cols-n vert\n");}
 sscanf (cline, "%d %d %d", &nrow, &ncol, &nver);
 if (bug > 0)
  printf ("number of rows: %d columns: %d vertical levels %d\n",nrow,ncol,nver);
 ig.nrow = nrow;  ig.ncol = ncol;	ig.nver = nver;

 /* Read in northwest latitude, longitude, and incriment */
 if (fgets(cline,CLINE,fp) == NULL)
  {printf("missing line 4: lat-lon-inc\n");}
 sscanf (cline, "%f %f %f", &lat, &lon, &inch);
 if (bug > 0)
    printf ("latitude: %6.1f longitude: %6.1f incriment %6.1f\n",lat,lon,inch);
 ig.nlat = lat;  ig.wlon = lon;  ig.linc = inch;

 /* Read in vertical parameters, the base in meters and the incriment */
 if (fgets(cline,CLINE,fp) == NULL)
  {printf("missing line 5: base-inc\n");}
 sscanf (cline, "%f %f", &base, &incv);
 if (bug > 1) printf ("base: %6.0f incriment %6.0f\n",base,incv);
 ig.base = base;  	ig.incv = incv;

 /* read in the name of the 5d gridfile */
 if (fgets(cline,CLINE,fp) == NULL)
  {printf("missing 5-d gridfile name\n");}
  str_len = strlen(cline);
  cline[str_len-1] = '\0';
  if (bug > 1) printf("output 3_D gridfile: %s\n",cline);
  if (str_len>=FILELEN){printf("output file too long %d\n",str_len);return -1;}
  strcpy (want.grid5d_file, cline );

 /* Read in the title of the 3d gridfile 32 chars */
 if (fgets(cline,CLINE,fp) == NULL)
  {printf("missing 5-d gridfile title\n");}
 str_len = strlen(cline);
 cline[str_len-1] = '\0';
 strncpy((char *)want.title,cline,32);
 for (i=str_len; i<32; i++) {*(((char *)&want.title)+i) = ' ';}
 want.title[9] = 0;
 if (bug > 3) printf(">%s<\n", want.title);

 /* Read in the grib file names */
 if (bug > 3) printf ("List of gribfiles:\n");
 while (fgets(cline,140,fp) != NULL)
  {str_len = strlen(cline);
   if (str_len < 2) break; /* a zero length line is a terminator */
   if (n_files > NFILES-2)
    {printf("input gribfile list exceeded (NFILES)\n"); break;}
   n_files++;
   cline[str_len-1] = '\0';
   if (bug > 3) printf("%s\n",cline);
   i = -1;
   /* skip initial white space if any */
   flag = 1;	/* set error flag as true */
   while (1) {i++; if(i>str_len-1){flag=0;break;} if (cline[i] > ' ') break; }
   while (1) {if (cline[i] == ' ') break; else i++; }
   while (1) {i++; if (cline[i] > ' ') break; }
   while (1) {if (cline[i] == ' ') break; else i++; }
   while (1) {i++; if (cline[i] > ' ') break; }
   if (flag==0) {printf("error on input file line\n"); return -1;}
   str_len = strlen(&cline[i]);	/* length of the file name */
   if (str_len > FILELEN)
    {printf("for file %s the string length (%d) > cline,FILELEN\n", str_len);
     break; }
   strcpy (want.file[n_files], &cline[i] );
   sscanf (cline, "%d %d", &want.fcst_hr[n_files],&want.grid_id[n_files] );
  }
 want.nfiles = n_files + 1;	/* make one based */
 if (bug > 0) printf("number of input gribfiles files: %d\n",want.nfiles);
 if (bug > 0) {
  for (i=0; i<want.nfiles; i++)
   {printf ("%2d %3d %s\n", want.fcst_hr[i], want.grid_id[i], want.file[i]); }   }

 /* compute some grib header information */
 ig.h_latlon[0] = 4;
 ig.h_latlon[1] = ig.nlat * 10000.;
 ig.h_latlon[2] = ig.wlon * 10000.;
 ig.h_latlon[3] = ig.linc * 10000.;
 ig.h_latlon[4] = ig.linc * 10000.;
 ig.h_height[0] = 1;
 ig.h_height[1] = ig.base + ig.incv * (ig.nver-1);	/* top */
 ig.h_height[2] = ig.incv;

 /* Compute and check additional variables from basic variables */
 ig.hrz_grid_size = ig.nrow * ig.ncol;
 ig.gridsize = ig.hrz_grid_size * ig.nver;
 if (ig.gridsize > G5D)
  {printf("%d 3-D gridsize > G5D %d\n",ig.gridsize,G5D); return -1;}

 /* Check the validity of the input variables */
 if (ig.nrow < 2 || ig.nrow > 150)
    {printf("number of rows outside range 2-150 nrows %d\n",ig.nrow);
     return -1; }
 if (ig.ncol < 2 || ig.ncol > 150)
    {printf("number of rows outside range 2-150 ncols %d\n",ig.ncol);
     return -1; }

 fclose (fp);
 if (bug > 3) printf("exit assign5\n\n"); if (bug > 3) fflush(NULL); /* DEBUG */
 return 0;
}
