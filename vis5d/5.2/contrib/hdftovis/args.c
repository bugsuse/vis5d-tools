extern int Argc;
extern char **Argv;

/* routines accompanying hdftovis.F, so that mixed case file
names can be included on the command line (replaces cpp call)

program written by Paul J. Meyer NASA/ES44 and Karen A. Butler
NTI/ES44 at the Marshall Space Flight Center.

e-mail: butler@geosim.msfc.nasa.gov */

#include <string.h>

/* this routine argcnt returns the number of parameters
   passed to the program */

#ifdef ibm
int argcnt()
#else
int argcnt_()
#endif

{
   return(Argc);
}

/* argstring returns the character string from the command line
   specified by argnum (the position of the parameter on the command
   line desired). The returned character string is in cres. The Fortran
   compiler adds cres and len to fortran call. Note: this function
   does not convert to upper case.*/  

#ifdef ibm
void argstring(cres,len,argnum) 
#else
void argstring_(cres,len,argnum) 
#endif
 
   int *argnum,len;
   char *cres;
{
   strcpy(cres,Argv[*argnum]);
}
