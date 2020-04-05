#include <math.h>

void w3fb00 (float alat, float along, float xmeshl, float *xi, float *xj)
{
    /* Initialized data */

    static float radpd = (float).01745329;
    static float earthr = (float)6371.2;

    /* Local variables */
 static double sinl, xlat, r, wlong, re;

/* $$$   SUBPROGRAM  DOCUMENTATION  BLOCK */

/* SUBPROGRAM: W3FB00         LATITUDE, LONGITUDE TO I,J */
/*   AUTHOR: HEERMANN,A.      ORG: W345       DATE: 90-06-04 */

/* ABSTRACT: CONVERTS THE COORDINATES OF A LOCATION ON EARTH FROM THE */
/*   NATURAL COORDINATE SYSTEM OF LATITUDE/LONGITUDE TO THE GRID (I,J) */
/*   COORDINATE SYSTEM OVERLAID ON THE POLAR STEREOGRAPHIC MAP PRO- */
/*   JECTION TRUE AT 60 N. A PREFERABLE, MORE FLEXIBLE SUBROUTINE TO */
/*   USE IS W3FB04. W3FB00 IS THE REVERSE OF W3FB01. */

/* PROGRAM HISTORY LOG: */
/*   69-08-01  A. HEERMANN */
/*   89-01-20  R.E.JONES   CHANGE TO MICROSOFT FORTRAN 4.10 */
/*   90-06-04  R.E.JONES   CHANGE TO SUN FORTRAN 1.3 */
/*   91-03-29  R.E.JONES  CONVERT TO SiliconGraphics FORTRAN */

/* USAGE:  CALL W3FB00 (ALAT, ALONG, XMESHL, XI, XJ) */

/*   INPUT VARIABLES: */
/*     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES */
/*     ------ --------- ----------------------------------------------- */

/*     ALAT   ARG LIST  LATITUDE IN DEG.  (-20.0(S. HEMIS) ) ALAT ) 90.0) 
*/
/*     ALONG            WEST LONGITUDE IN DEGREES */
/*     XMESHL ARG LIST  MESH LENGTH OF GRID IN KILOMETERS AT 60N */

/*   OUTPUT VARIABLES: */
/*     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES */
/*     ------ --------- ----------------------------------------------- */

/*     XI     ARG LIST  I OF THE POINT RELATIVE TO NORTH POLE */
/*     XJ     ARG LIST  J OF THE POINT RELATIVE TO NORTH POLE */

/*   SUBPROGRAMS CALLED: */
/*     NAMES                                                   LIBRARY */
/*     ------------------------------------------------------- -------- */

/*     COS SIN                                                 SYSLIB */

/*   REMARKS: THE GRID USED IN THIS SUBROUTINE HAS ITS ORIGIN (I=0,J=0) */

/*     AT THE NORTH POLE, SO IF THE USER'S GRID HAS ITS ORIGIN AT A */
/*     POINT OTHER THAN THE NORTH POLE, A TRANSLATION IS REQUIRED TO */
/*     GET I AND J. THE SUBROUTINE GRID IS ORIENTED SO THAT LONGITUDE */
/*     80W IS PARALLEL TO THE GRIDLINES OF I=CONSTANT. THE RADIUS OF */
/*     THE EARTH IS TAKEN TO BE 6371.2 KM. */
/*     ALL PARAMETERS IN THE CALL STATEMENT MUST BE REAL */

/* ATTRIBUTES: */
/*   LANGUAGE: SiliconGraphics 3.3 FORTRAN 77 */
/*   MACHINE:  SiliconGraphics IRIS-4D/25 */

/* $$$ */


    re = earthr * (float)1.86603 / xmeshl;
    xlat = alat * radpd;
    sinl = sin(xlat);
    wlong = (along + (float)100.) * radpd;
    r = re * cos(xlat) / (sinl + (float)1.);
    *xi = r * sin(wlong);
    *xj = r * cos(wlong);

    return;
} /* w3fb00_ */

