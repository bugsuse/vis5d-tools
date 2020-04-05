long set_sign (long word, int nbytes)
{
 static mask_bit[4] = {0x00000080, 0x00008000, 0x00800000, 0x80000000};
 static mask_val[4] = {0x0000007f, 0x00007fff, 0x007fffff, 0x7fffffff};
 long newval;

 newval = word & mask_val[nbytes-1];
 if ( (word & mask_bit[nbytes-1]) > 0) newval = -newval;
 return newval;
}
