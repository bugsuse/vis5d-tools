/* compal.c */


/* allocate three buffers of memory:
      gridk - 4 * mrmcml + 256 bytes
      gridl - 4 * mrmcml + 256 bytes
      igrid - mrmcml + 256 bytes
   and calls the fortran subroutine COMPRS with the adddresses of
   the 3 buffers.
*/
#ifdef UNDERSCORE
  compal_(mrmcml)
#else
  compal(mrmcml)
#endif
int *mrmcml;
{
   char *gridk, *gridl, *igrid;
   char *malloc();
   gridk = malloc(4 * (*mrmcml) + 256);
   gridl = malloc(4 * (*mrmcml) + 256);
   igrid = malloc(*mrmcml + 256);
#ifdef UNDERSCORE
     comprs_(gridk, gridl, igrid);
#else
     comprs(gridk, gridl, igrid);
#endif
}
