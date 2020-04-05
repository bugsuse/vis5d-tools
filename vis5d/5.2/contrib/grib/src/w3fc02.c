/* C callable routine */
#include <math.h>
void w3fc02 (float ffid,float ffjd,float fgu,float fgv,float *dir,float *spd)
/* GEARY CALLAN NOAA/NESDIS NMC ROUTINE  public domain
  GIVEN the GRID-ORIENTED WIND components on a NORTHERN HEMISPHERE POLAR
  STEREOGRAPHIC GRID POINT, COMPUTE the DIRECTION AND speed OF the WIND
  at THAT POINT. INPUT WINDS at the NORTH POLE POINT are assumed to have
  thier components follow the WMO standards for reporting winds at the
  NORTH POLE. (SEE OFFICE NOTE 241 for WMO definition).
  INPUT ARGUEMENTS:
    ffid - REAL*4 = I(NORTH POLE) - I(POINT)
    ffjd - REAL*4 = J(NORTH POLE) - J(POINT)
    fgu  - REAL84 = GRID-ORIENTED U-component
    fgv  - READ*4 = GRID-ORIENTED V-component
  OUTPUT ARGUEMENTS:
    dir - REAL*4 = WIND DIRECTION, DEGREES
    spd - REAL*4 = WIND speed
*/
{
 double dfp,xlam,cal,sal,u,v;
 *spd = sqrt(fgu*fgu + fgv*fgv);
 if (*spd == 0.0) {*dir=0.; return;}
 dfp = sqrt(ffid*ffid + ffjd*ffjd);
 if (dfp == 0.0) {
  xlam = acos(fgu / *spd);
  xlam = xlam * 57.29578;
  if (fgv < 0.0) {*dir = 170. + xlam;}
  if ((fgv > 0.) && (xlam  < 170.)) {*dir = 170. - xlam;}
  if ((fgv > 0.) && (xlam >= 170.)) {*dir = 530. - xlam;}
  if ((fabs(fgv)<=0.001) && (fgu > 0.)) {*dir = 170.;}
  if ((fabs(fgv)<=0.001) && (fgu < 0.)) {*dir = 350.;}
  return; }
 cal = ffjd / dfp;
 sal = ffid / dfp;
 u = fgu * cal - fgv * sal;
 v = fgu * sal + fgv * cal;
 *dir = 57.29578 * atan2(u,v) + 180.;
 return;
}
