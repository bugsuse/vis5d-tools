/* buttonmatrix.h */



#ifndef BUTTONMATRIX_H
#define BUTTONMATRIX_H



/* MJK 12.04.98 */
/* 08May98  Phil McDonald */
#  include      "../src/v5d.h"
#  define MAX_BM_ROWS MAXVARS
#define MAX_BM_COLS 10


typedef struct lui_button_matrix {
   Window mainwindow;
   int x, y;
   int width, height;
   int rows, columns;           /* how many rows and columns of buttons */
   int viewrows;                /* how many rows are visible */

   int bwidth, bheight;         /* size of each button in pixels */
   int maxchars;                /* max chars per button */
   int toprow;
   char *labels[MAX_BM_ROWS][MAX_BM_COLS];
   float color[MAX_BM_ROWS][MAX_BM_COLS][3];
   unsigned long pixel[MAX_BM_ROWS][MAX_BM_COLS];
   char state[MAX_BM_ROWS][MAX_BM_COLS];

   LUI_SCROLLBAR *scrollbar;

   int (*callback)( struct lui_button_matrix *, int row, int col, int button );
   int context_index;                   /* for example, Vis5D context */
} LUI_BUTTON_MATRIX;




extern LUI_BUTTON_MATRIX *LUI_ButtonMatrixCreate( Window parent,
       int x, int y, int width, int height, int columns );



extern void LUI_ButtonMatrixAddRow( LUI_BUTTON_MATRIX *bm,
       char *labels[], float *reds, float *greens, float *blues );


extern void LUI_ButtonMatrixChangeLabel( LUI_BUTTON_MATRIX *bm,
                                         int row, int column, char *label );


extern void LUI_ButtonMatrixCallback( LUI_BUTTON_MATRIX *bm,
                                      int (*callback)() );



extern void LUI_ButtonMatrixSetState( LUI_BUTTON_MATRIX *bm,
                                      int row, int col, int state );


extern int LUI_ButtonMatrixGetState( LUI_BUTTON_MATRIX *bm,
                                     int row, int col );


extern void LUI_ButtonMatrixSetColor( LUI_BUTTON_MATRIX *bm,
                                      int row, int col,
                                      double red, double green, double blue );


extern void LUI_ButtonMatrixShowBottom( LUI_BUTTON_MATRIX *bm );


extern void LUI_ButtonMatrixResize( LUI_BUTTON_MATRIX *bm,
                                    int width, int height );


extern void LUI_ButtonMatrixEmpty( LUI_BUTTON_MATRIX *bm );


extern void LUI_ButtonMatrixDestroy( LUI_BUTTON_MATRIX *bm );


extern void LUI_ButtonMatrixRedraw( LUI_BUTTON_MATRIX *bm);

#endif
