/*                @(#)popup.h        1.1 Stellar 89/02/09        */
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
 * LUI Popup Header
 */

#ifndef _LUI_POPUP_HEADER

#ifdef LUI_POPUP_DECLARE
# define LUI_POPUP_GLOBAL  /* */
# define LUI_POPUP_INIT(v) = v
#else
# define LUI_POPUP_GLOBAL  extern
# define LUI_POPUP_INIT(v) /* */
#endif

#define LUI_POPUP_MAX      64
#define LUI_POPUP_WIDTH    120
#define LUI_POPUP_HEIGHT   20
#define LUI_POPUP_WIDTH_2  LUI_POPUP_WIDTH/2
#define LUI_POPUP_HEIGHT_2 LUI_POPUP_HEIGHT/2
#define LUI_POPUP_BORDER   0

typedef struct popup {
    char    name[64];
    Window  window;
    int     visible;
    int     items;
    int     ((*funcs[LUI_POPUP_MAX])());
   int context_index;                   /* for example, Vis5D context */
    LUI_LABEL *label[LUI_POPUP_MAX];
    LUI_LABEL *popup_label;
} LUI_POPUP;

#ifdef LUI_POPUP_DECLARE /*--- \/ INTERNAL VARIABLES \/ ---*/

static LUI_POPUP
  *LUI_PopupHead = NULL,
  *LUI_PopupMenues[LUI_POPUP_MAX],
  *LUI_PopupActive[LUI_POPUP_MAX];

static int
  LUI_PopupInd = 0,
  LUI_PopupActiveInd = 0;

static Window
  LUI_PopupMainWindow;

int 
  LUI_PopupWindowProcess(), 
  LUI_PopupEntreeProcess(),
  LUI_PopupDrawArrow();

#endif /*LUI_POPUP_DECLARE */

LUI_POPUP 
  *LUI_PopupCreate(),
  *LUI_PopupLookup();

#endif  /*_LUI_POPUP_HEADER*/
#define _LUI_POPUP_HEADER 1
