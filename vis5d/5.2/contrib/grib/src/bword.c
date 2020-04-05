long bword (char *p, int bloc, int bnum)
{int i;
 int j = bloc;
 int k = bloc + bnum -1;
 int l = 4 - bnum;
 long word;
 char *q;

 word = 0; q = (char *)&word;
 /* for (i=j; i<=k; i++,l++) { *((char *)&word+l) = *(p+i);} */ /* also works */
 /* *((char *)&word[l] will not work because [] has precidence over (type). */
 for (i=j; i<=k; i++,l++) { *(q+l) = *(p+i);}
 return word;}
