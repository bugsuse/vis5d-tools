/*                @(#)dialog.h        1.2 Stellar 89/02/17        */
/*
                        Copyright 1989 by
                        Stellar Computer Inc.
                        All Rights Reserved
        
        This software comprises unpublished confidential information of
        Stellar Computer Inc. and may not be used, copied or made
        available to anyone, except in accordance with the license
        under which it is furnished.
*/
/*
 * LUI Dialog box header
 */

#ifndef _LUI_DIALOG_HEADER

#include "label.h"

#ifdef LUI_DIALOG_DECLARE
# define LUI_DIALOG_GLOBAL  /* */
# define LUI_DIALOG_INIT(v) = v
#else
# define LUI_DIALOG_GLOBAL  extern
# define LUI_DIALOG_INIT(v) /* */
#endif

#define LUI_DIALOG_INPUT  0
#define LUI_DIALOG_OUTPUT 1

typedef struct lui_dialog {
    char           name[64];
    Window         parent;
    Window         window;
    int            x, y;
    int            width, height;
    int            border_size;
    int            type;
    unsigned long  page_color;
    unsigned long  background_color;
    unsigned long  border_color;
    int         ((*func)());
   int context_index;                   /* for example, Vis5D context */
    LUI_LABEL     *dialog_label, *prompt_label;
    char          *dialog_str, *prompt_str1, *prompt_str2;
    struct lui_dialog *np;
} LUI_DIALOG;

LUI_DIALOG_GLOBAL LUI_DIALOG
  *LUI_DialogHead LUI_DIALOG_INIT ( NULL ),
  *LUI_Dialog     LUI_DIALOG_INIT ( NULL );

LUI_DIALOG 
  *LUIdialog_create(),
  *LUIdialog_lookup_name();

int LUIdialog_process();
int LUIdialog_frame();

#endif  /*_LUI_DIALOG_HEADER*/
#define _LUI_DIALOG_HEADER 1
