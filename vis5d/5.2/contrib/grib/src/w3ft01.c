
float w3ft01 (float sti, float stj, float *fld, int ii, int jj, int ncyclk,
		int lin)
{
    /* System generated locals */
    int fld_dim1, fld_offset;

    /* Local variables */
    static float eras[4], xi2tm, xj2tm;
    static int i, j, k;
    static float xdeli, xdelj;
    static int j1;
    static float fi, fj;
    static int jy[4], icyclk, jcyclk, im1, ip1, ip2;
 float hi;	/* the interpolated value */

/* ...(STI,STJ) GRID (I,J) COORDINATES   REAL */
/* ...FLD IS NAME OF GRIDDED FIELD   REAL */
/* ...HI IS THE INTERPOLATED VALUE AT (STI,STJ) FROM FIELD FLD   REAL */
/* ...II,JJ ARE DIMENSIONS OF FIELD           INTEGER */
/* ...NCYCLK, INDICATOR TO SPECIFY IF GRID IS CYCLIC OR NON CYCLIC */
/* ...  O= NON-CYCLIC IN II, NON-CYCLIC IN JJ */
/* ...  1=     CYCLIC IN II, NON-CYCLIC IN JJ */
/* ...  2= NON-CYCLIC IN II,     CYCLIC IN JJ */
/* ...  3=     CYCLIC IN II,     CYCLIC IN JJ */
/* ...LIN INDICATOR TO SPECIFY INTERPOLATION METHOD */
/* ...  1= LINEAR INTERPOLATION */
/* ...  NE 1 QUADRATIC INTERPOLATION */
    /* Parameter adjustments */
    fld_dim1 = ii;
    fld_offset = fld_dim1 + 1;
    fld -= fld_offset;

    /* Function Body */
    i = sti;
    j = stj;
    fi = (float) i;
    fj = (float) j;
    xdeli = sti - fi;
    xdelj = stj - fj;
    ip2 = i + 2;
    im1 = i - 1;
    ip1 = i + 1;
    jy[3] = j + 2;
    jy[2] = j + 1;
    jy[1] = j;
    jy[0] = j - 1;
    xi2tm = (float)0.;
    xj2tm = (float)0.;
    if (lin == 1) {
	goto L12;
    }
    xi2tm = xdeli * (xdeli - (float)1.) * (float).25;
    xj2tm = xdelj * (xdelj - (float)1.) * (float).25;
L12:
    if (i < 2 || j < 2) {
	goto L10;
    }
    if (i > ii - 3 || j > jj - 3) {
	goto L10;
    }
/*   QUADRATIC (LINEAR TOO) OK WITHOUT FURTHER ADO SO GO TO 41 */
    goto L41;
L10:
    icyclk = 0;
    jcyclk = 0;
    if (ncyclk != 0) {
	goto L11;
    } else {
	goto L6;
    }
L11:
    if (ncyclk / 2 != 0) {
	jcyclk = 1;
    }
    if (ncyclk != 2) {
	icyclk = 1;
    }
    if (icyclk != 0) {
	goto L8;
    } else {
	goto L5;
    }
L8:
    if (i < 1) {
	goto L16;
    }
    if (i == ii - 1) {
	goto L17;
    }
    ip2 = i + 2;
    im1 = i - 1;
    goto L18;
L16:
    ip2 = 3;
    im1 = ii - 1;
    goto L18;
L17:
    ip2 = 2;
    im1 = ii - 2;
L18:
    ip1 = i + 1;
L5:
    if (jcyclk != 0) {
	goto L9;
    } else {
	goto L6;
    }
L9:
    if (j == 1) {
	goto L26;
    }
    if (j == jj - 1) {
	goto L27;
    }
    jy[3] = j + 2;
    jy[0] = j - 1;
    goto L28;
L26:
    jy[3] = 3;
    jy[0] = j - 1;
    goto L28;
L27:
    jy[3] = 2;
    jy[0] = jj - 2;
L28:
    jy[2] = j + 1;
    jy[1] = j;
L6:
    if (lin == 1) {
	goto L22;
    }
    if (icyclk != 0) {
	goto L21;
    } else {
	goto L20;
    }
L20:
    if (i < 2 || i >= ii - 1) {
	xi2tm = (float)0.;
    }
L21:
    if (jcyclk != 0) {
	goto L22;
    } else {
	goto L23;
    }
L23:
    if (j < 2 || j >= jj - 1) {
	xj2tm = (float)0.;
    }
L22:
/*   DO NOT ALLOW POINT OFF GRID CYCLIC OR NOT. */
    if (i < 1) {
	i = 1;
    }
    if (ip1 < 1) {
	ip1 = 1;
    }
    if (ip2 < 1) {
	ip1 = 1;
    }
    if (im1 < 1) {
	im1 = 1;
    }
    if (i > ii) {
	i = ii;
    }
    if (ip1 > ii) {
	ip1 = ii;
    }
    if (ip2 > ii) {
	ip2 = ii;
    }
    if (im1 > ii) {
	im1 = ii;
    }
L41:
    for (k = 1; k <= 4; ++k) {
	j1 = jy[k - 1];
/*   DO NOT ALLOW POINT OFF GRID CYCLIC OR NOT */
	if (j1 < 1) {
	    j1 = 1;
	}
	if (j1 > jj) {
	    j1 = jj;
	}
	eras[k - 1] = (fld[ip1 + j1 * fld_dim1] - fld[i + j1 * fld_dim1]) * 
		xdeli + fld[i + j1 * fld_dim1] + (fld[im1 + j1 * fld_dim1] - 
		fld[i + j1 * fld_dim1] - fld[ip1 + j1 * fld_dim1] + fld[ip2 + 
		j1 * fld_dim1]) * xi2tm;
/* L40: */
    }
    hi = eras[1] + (eras[2] - eras[1]) * xdelj + (eras[0] - eras[1] - eras[2]
	     + eras[3]) * xj2tm;
    return hi;
} /* w3ft01_ */

