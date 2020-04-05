/* field.h */



#ifndef LUI_FIELD_H
#define LUI_FIELD_H



#define MAX_FIELD 1000


typedef struct lui_field {
   Window window;
   int x, y;
   int width, height;

   char text[MAX_FIELD];        /* the text */
   int curpos;                  /* cursor position */
   int columns;                 /* width of field in text columns */
   int scroll;                  /* horizontal scroll offset */
   int editing;                 /* currently being edited? */
   int modified;                /* has text changed since editing started? */
   void *userdata;              /* pointer to user data */
   int (*callback) ( struct lui_field *, char * ); /* user callback function */
   int context_index;           /* for example, Vis5D context */
   int display_index;           /* which display this belongs to */
   struct lui_field *warp_to;   /* next field when TAB is pressed */
   int len_limit;

} LUI_FIELD;



extern void LUI_FieldSetText( LUI_FIELD *field, char *text );

extern void LUI_FieldSetDouble( LUI_FIELD *field, double x );

extern void LUI_FieldSetNotDouble( LUI_FIELD *field, double x );

extern void LUI_FieldSetInt( LUI_FIELD *field, int i );


extern void LUI_FieldGetText( LUI_FIELD *field, char *text );

extern double LUI_FieldGetDouble( LUI_FIELD *field );

extern int LUI_FieldGetInt( LUI_FIELD *field );


extern void LUI_FieldCallback( LUI_FIELD *field, int (* callback)() );


extern void LUI_FieldLink( LUI_FIELD *from, LUI_FIELD *to );


extern LUI_FIELD *LUI_FieldCreate( Window parent, int x, int y,
                                   int width, int height );


extern void LUI_FieldDestroy( LUI_FIELD *field );


#endif

