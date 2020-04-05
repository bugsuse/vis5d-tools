/* newbutton.h */


#ifndef LUI_NEWBUTTON_H
#define LUI_NEWBUTTON_H



typedef struct lui_newbutton {
   Window window;
   int x, y;
   int width, height;
   int framewidth;

   int toggle;                  /* 0 = momentary button */
                                /* 1 = toggle button, no light */
                                /* 2 = toggle button with indicator light */
   char *label;                 /* text label */
   int state;                   /* 1 = on/pressed, 0 = off */
   int highlight;               /* 1 = pointer is in window, 0 = not */
   unsigned long color;         /* background pixel value when state=1 */
   float red, green, blue;      /* background color when state=1 */

   void *userdata;              /* pointer to user data */
   int (*callback)( struct lui_newbutton *, int ); /* user callback func*/
   int mousebutton;             /* which mouse button was pressed */
   int            indexowner;

   int context_index;           /* for example, Vis5D context */
   int index;                   /* user data */
   int special;          /* MJK, this is so the right mouse button can toggle
                            this if set to 1 */
} LUI_NEWBUTTON;




extern LUI_NEWBUTTON *LUI_PushButtonCreate( Window parent,
                int x, int y, int width, int height, char *label );


extern LUI_NEWBUTTON *LUI_ToggleButtonCreate( Window parent,
                int x, int y, int width, int height, char *label );

extern LUI_NEWBUTTON *LUI_CheckButtonCreate( Window parent,
                int x, int y, int width, int height, char *label );


extern void LUI_ButtonColor( LUI_NEWBUTTON *b,
                             double red, double green, double blue );


extern void LUI_ButtonSetState( LUI_NEWBUTTON *b, int state );


extern int LUI_ButtonGetState( LUI_NEWBUTTON *b );


extern void LUI_ButtonCallback( LUI_NEWBUTTON *b, int (*callback)() );


extern void LUI_ButtonIndex( LUI_NEWBUTTON *b, int index );

extern void LUI_ButtonContextIndex( LUI_NEWBUTTON *b, int index );

extern void LUI_NewButtonDestroy( LUI_NEWBUTTON *button );



#endif

