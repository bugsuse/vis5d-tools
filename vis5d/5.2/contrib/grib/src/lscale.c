int lscale (unsigned short int u)
{int ii;
 unsigned short int sign_mask = 0x8000;
 unsigned short int scal_mask = 0x7fff;
 ii = u & scal_mask;
 if ((u & sign_mask) == sign_mask) {ii = ii * -1;}
 return ii;
}
