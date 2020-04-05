/*    SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    Q9IE32      CONVERT IBM370 F.P. TO IEEE F.P.
C   PRGMMR: R.E.JONES        ORG: W/NMC42    DATE: 90-06-04
C
C ABSTRACT: CONVERT IBM370 32 BIT FLOATING POINT NUMBERS TO IEEE
C   32 BIT TASK 754 FLOATING POINT NUMBERS.
C
C PROGRAM HISTORY LOG:
C   90-06-04  R.E.JONES   CHANGE TO SUN FORTRAN 1.3
C   90-07-14  R.E.JONES   CHANGE ISHFT TO LSHIFT OR LRSHFT
C   91-03-09  R.E.JONES   CHANGE TO SiliconGraphics FORTRAN
C
C USAGE:    CALL Q9IE32(A, B, N, ISTAT)
C   INPUT ARGUMENT LIST:
C     A        - REAL*4 ARRAY OF IBM370 32 BIT FLOATING POINT NUMBERS
C     N        - NUMBER OF POINTS TO CONVERT
C
C   OUTPUT ARGUMENT LIST:
C     B        - REAL*4 ARRAY OF IEEE 32 BIT FLOATING POINT NUMBERS
C     ISTAT    - NUMBER OF POINT GREATER THAN 10E+38, NUMBERS ARE SET TO
C                IEEE INFINITY, ONE IS ADDED TO ISTAT. NUMBERS LESS THAN
C                E-38 ARE SET TO ZERO , ONE IS  NOT ADDED TO ISTAT.
C
C REMARKS: SEE IEEE TASK 754 STANDARD FLOATING POINT ARITHMETIC
C   FOR MORE INFORMATION ABOUT IEEE F.P.
C
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.3 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25
C
*/
int x9ie32 (float a[], float b[], int n)
{
 union {
  long  i;		/* reference value */
  float f;} rf;
 const long  infin  = 0x7F800000;
 const long  maskfr = 0x007FFFFF;
 const long  masksn = 0x7FFFFFFF;
 const long  mask21 = 0x00200000;
 const long  mask22 = 0x00400000;
 const long  mask23 = 0x00800000;
 const long  sign   = 0x80000000;

 long itemp;	/* hold current ibm 370 float */
 long ltemp;
 long isign;
 long ieeexp;	/* hold the derived ieee exponent */
 long k;
 int istat;
 int i;		/* indices */

 if (n < 1) { return -1; }

 istat = 0;

 for (i=0; i<n; i++)
  {isign = 0;
   rf.f = a[i];
   itemp = rf.i;

   /*     TEST SIGN BIT */

   if (itemp == 0) {b[i] = 0.0; continue;} /* underflow, set to zero */
   if (itemp < 0)
    {isign = sign;
     itemp = itemp & masksn; }  /* set sign bit to zero */

/*          CONVERT IBM EXPONENT TO IEEE EXPONENT */

     ieeexp = ((itemp >> 24) - 64) * 4 + 126;

     k = 0;

/*        TEST BIT 23, 22, 21 */
/*        ADD UP NUMBER OF ZERO BITS IN FRONT OF IBM370 FRACTION */

     if ((itemp & mask23) != 0) goto done;
     k = k + 1;
     if ((itemp & mask22) != 0) goto done;
     k = k + 1;
     if ((itemp & mask21) != 0) goto done;
     k = k + 1;

     done: ;

/*        SUBTRACT ZERO BITS FROM EXPONENT */

     ieeexp = ieeexp - k;

/*        TEST FOR OVERFLOW */

     if (ieeexp > 254)
      {istat  = istat + 1;
       rf.i = infin | isign;  b[i] = rf.f;
       continue; }

/*        TEST FOR UNDERFLOW  */

     if (ieeexp < 1) {b[i] = 0.0;}

/*        SHIFT IEEE EXPONENT TO BITS 1 TO 8 */

     ltemp = ieeexp << 23;

/*        SHIFT IBM370 FRACTION LEFT K BIT, AND OUT BITS 0 - 8 */
/*        OR TOGETHER THE EXPONENT AND THE FRACTION */
/*        OR IN SIGN BIT */

     rf.i = ((((itemp<<k)&maskfr)|ltemp)|isign);
     b[i] = rf.f;

  }
 return istat;
}
