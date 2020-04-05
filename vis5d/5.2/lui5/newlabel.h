/* newlabel.h */


#ifndef LUI_NEWLABEL_H
#define LUI_NEWLABEL_H


typedef struct lui_newlabel {
   Window window;
   int x, y;
   int width, height;

   char *label;         /* the text label */
   int lines;           /* how many lines are in the label */
} LUI_NEWLABEL;




extern LUI_NEWLABEL *LUI_NewLabelCreate( Window parent,
                                         int x, int y, int width, int height,
                                         char *label );


extern void LUI_NewLabelRefresh( LUI_NEWLABEL *l );

extern void LUI_NewLabelChangeText( LUI_NEWLABEL *l, char *label );


extern void LUI_NewLabelDestroy( LUI_NEWLABEL *l );



#endif

