/* newslider.h */


#ifndef NEWSLIDER_H
#define NEWSLIDER_H



typedef struct lui_newslider {
   Window window;
   int x, y, width, height;

   char *label;
   int state, hilite, type;
   float low, high, value, old_value;
   int (*callback)( struct lui_newslider *, float );
   int context_index;                   /* for example, Vis5D context */
   int index;
   int button;           /* which mouse button's being used */

/* MJK 12.04.98 */
/* 22Sep97  Phil McDonald */
   char *units;
} LUI_NEWSLIDER;



extern LUI_NEWSLIDER *LUI_NewSliderCreate( Window parent, int x, int y,
                                           int width );



extern void LUI_NewSliderCallback( LUI_NEWSLIDER *s,
                                   int (*callback)( LUI_NEWSLIDER *, float) );

extern void LUI_NewSliderSetLabel( LUI_NEWSLIDER *s, char *label );

extern void LUI_NewSliderSetRange( LUI_NEWSLIDER *s,
                                   float min, float max );

extern void LUI_NewSliderSetValue( LUI_NEWSLIDER *s, float value );

/* MJK 12.04.98 */
/* 22Sep97  Phil McDonald	added units */
extern void LUI_NewSliderChange( LUI_NEWSLIDER *s, char *label, char *units,
                                 float min, float max, float value );
#ifdef JOHAN
extern void LUI_NewSliderChange( LUI_NEWSLIDER *s, char *label,
                                       float min, float max, float value );
#endif

extern void LUI_NewSliderDestroy( LUI_NEWSLIDER *s );



#endif

