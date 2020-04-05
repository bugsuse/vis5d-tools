#define BUFF 68000

#ifdef MAIN_R
long buf[BUFF];
float ubuf[BUFF];
#else
extern long buf[BUFF];
extern float ubuf[BUFF];
#endif
