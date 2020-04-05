/* When this works put it in cutil directory */
/* extract a byte of length len from array buf, starting at bit nb*/
/* (0 based) return it in one word, right justified no sign extend */
long exbits (int nb, int len, char *buf)
{long i, ll, is, ib, ix, j;
 unsigned long mask = 0xffffffff;
 unsigned long h, l = 0;
 unsigned long ls;
 ll = len;
 i = nb / 8; ib = nb - (i*8); /* i is (0-based) byte location of starting bit */
 is = 32 - (8-ib); mask = mask >> is;
 if (ib > 0)
 {l = buf[i] & mask;
  ll = ll - (8 - ib);
  i++;
 }
 ix = ll / 8;
 for (j=0; j<ix; j++)
 {l = l<<8; h = buf[i]; l = l | h; i++;}
 j = ll - ix*8;
 if (j > 0)    /* was (ll > 0)  */
 {l = l<<j;
  h = buf[i];
  h = h >> (8-j);
  l = l | h;
 }
 else if (ll < 0)
 {j = -ll; l = l>>j;}  /* mbyte is contained within a byte but not right justified */
/* get sign extension */
ls = l;
ls = ls<<(32-len);
ls = ls>>(32-len);
return ls;
}
