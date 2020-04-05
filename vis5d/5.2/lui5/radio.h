/* radio.h */


/*
 * Radio button widget.  Exactly one of a set of buttons can be selected
 * at once.
 */


#ifndef LUI_RADIO_H
#define LUI_RADIO_H



typedef struct lui_radio {
   Window window;
   int x, y;
   int width, height;

   int numbuttons;                             /* how many radio buttons */
   char **labels;                              /* button labels */
   int current;                                /* current selection */

   int (*callback)( struct lui_radio *, int ); /* User callback function*/
   int context_index;                   /* for example, Vis5D context */
} LUI_RADIO;



extern LUI_RADIO *LUI_RadioCreate( Window parent,
                                   int x, int y, int width,
                                   int numbuttons, char **labels );


extern void LUI_RadioCallback( LUI_RADIO *r, int (*callback)() );


extern void LUI_RadioSetCurrent( LUI_RADIO *r, int current );


extern int LUI_RadioGetCurrent( LUI_RADIO *r );


extern void LUI_RadioDestroy( LUI_RADIO *r );



#endif
