#ifdef SCCS
static char sccsid[]="@(#)dialog.c        1.3 Stellar 89/03/22";
#endif
/*
                        Copyright 1989 by
                        Stellar Computer Inc.
                        All Rights Reserved
        
        This software comprises unpublished confidential information of
        Stellar Computer Inc. and may not be used, copied or made
        available to anyone, except in accordance with the license
        under which it is furnished.
*/


#include <stdio.h>
#include <stdlib.h>


#define LUI_DIALOG_DECLARE
#include "lui.h"

#define SUBSTR(sub, str) (memcmp(sub, str, strlen(sub)) ? 0 : 1)

/******************************************************************************/
 LUI_DIALOG *
LUIdialog_create(name, type, parent, x, y, xs, ys, page_color, back_color)
    char *name;
    int type;
    Window parent;
    int x, y, xs, ys;
    unsigned long page_color, back_color;
{
    XSetWindowAttributes window_attributes;
    Window dialog_window;
    LUI_DIALOG *dialog;
    LUI_LABEL *label;
    int bs, ss, y1, y2, y3, x1, x2, x3, in, sp, h, h1, h2, h3;
    char name_str[128], **argv;
    int LUIdialog_mark_char_position();

    /* Allocate the dialog box */
    if (!(dialog = (LUI_DIALOG *)malloc(sizeof(LUI_DIALOG)))) {
        printf("LUIdialog_create(): can't allocate dialog box %s\n", name);
        return(NULL);
    }

    /* Setup button pad variables */
    strcpy(dialog->name, name);
    dialog->type             = type;
    dialog->x                = x;
    dialog->y                = y;
    dialog->width            = xs;
    dialog->height           = ys;
    dialog->parent           = parent;
    dialog->func             = NULL;
    dialog->border_size      = 20;
    dialog->page_color       = page_color;
    dialog->background_color = back_color;
    dialog->border_color     = 0xfb0f12;

    /* Add button pad to head of list */
    dialog->np = LUI_DialogHead;
    LUI_DialogHead  = dialog;

    if (type == LUI_DIALOG_INPUT) {
        /* Create the labels *************************************************************/
        sp = 10;                                       /* spacing between labels */
        h  = LUI_BUTTON_YS;
        h1 = (2 * h) + sp;
        h2 = h;
        h3 = h;
        in = 30;                                       /* indent from sides */
        bs = xs - (in * 2);                            /* big sized labels */
        ss = 80;                                       /* small label size */
        x1 = in;                                       /* left justified */
        x2 = in;                                       /* cancel label */
        x3 = xs - in - ss;                             /* ok label */
        y1 = (ys - (h1 + h2 + h3) + (2 * sp)) / 3.0;
        y2 = y1 + h1 + sp;
        y3 = y2 + h2 + sp;
    }
    else {
        /* Create the labels *************************************************************/
        sp = 10;                                       /* spacing between labels */
        h  = LUI_BUTTON_YS;
        h1 = (3 * h) + sp;
        h2 = h;
        h3 = h;
        in = 30;                                       /* indent from sides */
        bs = xs - (in * 2);                            /* big sized labels */
        ss = 80;                                       /* small label size */
        x1 = in;                                       /* left justified */
        x2 = in;                                       /* cancel label */
        x3 = xs - in - ss;                             /* ok label */
        y1 = (ys - (h1 + h3) + (2 * sp)) / 3.0;
        y2 = y1;
        y3 = y2 + h1 + sp;
    }
        
    /*******************************************************************/
    /* Create a window to work on                                      */
    /*******************************************************************/
    dialog_window = LUI_CreateWindowAt(parent, x, y, xs, ys);
    XSetWindowBackground(LUI_Display, dialog_window, dialog->page_color);
    if (parent == LUI_RootWindow) {
        window_attributes.override_redirect = True;
        XChangeWindowAttributes(LUI_Display, dialog_window, CWOverrideRedirect, &window_attributes);
    }
    dialog->window = dialog_window;
    
    LUI_EventAdd(dialog_window, ExposureMask, LUIdialog_frame);
    
    /*******************************************************************/
    /* Create the prompt label                                         */
    /*******************************************************************/
    sprintf(name_str, "LUIdialog_Prompt%s", name);
    label = LUI_LabelCreate(name_str, "foobar",
                            dialog_window, x1, y1, bs, h1, 0, LUIdialog_process, 0, 1);
    XSetWindowBackground(LUI_Display, label->window, dialog->page_color);
    dialog->prompt_label = label;
    if (!(argv = (char **)malloc(2 * sizeof(char *)))) {
        fprintf(stderr, "LUIdialog_create: can't malloc argv for dialog prompt (%s)\n",
                dialog->name);
        return(NULL);
    }
    label->argv = argv;
    if (!(label->argv[0] = (char *)malloc(2048))) {
        fprintf(stderr, "LUIdialog_create: can't malloc string for dialog prompt (%s)\n",
                dialog->name);
        return(NULL);
    }
    label->argsize[0] = 2048;
    dialog->prompt_str1 = label->argv[0];
    if (!(label->argv[1] = (char *)malloc(2048))) {
        fprintf(stderr, "LUIdialog_create: can't malloc string for dialog prompt (%s)\n",
                dialog->name);
        return(NULL);
    }
    label->argsize[1] = 2048;
    dialog->prompt_str2 = label->argv[1];
    label->owner = (char *)dialog;
    LUI_LabelDropShadow(name_str, 1);
    label->argc = 2;
    
    if (type == LUI_DIALOG_INPUT) {
        /*******************************************************************/
        /* Create the dialog label                                         */
        /*******************************************************************/
        sprintf(name_str, "LUIdialog_Dialog%s", name);
        label = LUI_LabelCreate(name_str, "fumble",
                                dialog_window, x1, y2, bs, h2, 0, LUIdialog_process, 0, 1);
        XSetWindowBackground(LUI_Display, label->window, dialog->page_color);
        dialog->prompt_label = label;
        if (!(label->argv[0] = (char *)malloc(2048))) {
            fprintf(stderr, "LUIdialog_create: can't malloc string for dialog dialog (%s)\n",
                    dialog->name);
            return(NULL);
        }
        label->argsize[0] = 2048;
        dialog->dialog_str = label->argv[0];
        *dialog->dialog_str = '\0';
        label->owner = (char *)dialog;
        LUI_LabelDropShadow(name_str, 1);
        LUI_LabelSetDrawFunc(label, LUIdialog_mark_char_position);
    }
    
    /*******************************************************************/
    /* Create the cancel label                                         */
    /*******************************************************************/
    sprintf(name_str, "LUIdialog_Cancel%s", name);
    label = LUI_LabelCreate(name_str, "Cancel",
                            dialog_window, x2, y3, ss, h3, 0, LUIdialog_process, 0, 1);
    XSetWindowBackground(LUI_Display, label->window, dialog->page_color);
    label->owner = (char *)dialog;
    LUI_LabelDropShadow(name_str, 1);
    
    /*******************************************************************/
    /* Create the ok     label                                         */
    /*******************************************************************/
    sprintf(name_str, "LUIdialog_Ok%s", name);
    label = LUI_LabelCreate(name_str, "  OK  ",
                            dialog_window, x3, y3, ss, h3, 0, LUIdialog_process, 0, 1);
    XSetWindowBackground(LUI_Display, label->window, dialog->page_color);
    label->owner = (char *)dialog;
    LUI_LabelDropShadow(name_str, 1);

    return(dialog);
}

/******************************************************************************/
 LUI_DIALOG *
LUIdialog_lookup_name(name)
    char *name;
{
    register LUI_DIALOG *p;

    for (p = LUI_DialogHead; p; p = p->np) 
      if (!(strcmp(p->name, name))) 
        return(p);
    return(NULL);
}

/******************************************************************************/
 LUI_DIALOG *
LUIdialog_lookup_window(window)
    Window window;
{
    register LUI_DIALOG *p;

    for (p = LUI_DialogHead; p; p = p->np) 
      if (p->window == window)
        return(p);
    return(NULL);
}
   
/******************************************************************************/
 char *
LUIdialog_request(name, prompt1, prompt2, func)
    char *name, *prompt1, *prompt2;
    LUI_FNCP func;
{
    LUI_DIALOG *dialog;

    if (!(dialog = LUIdialog_lookup_name(name))) {
        fprintf(stderr, "LUIdialog_request(): can't find dialog box %s\n", name);
        return(0);
    }

    dialog->func = func;
    dialog->context_index = context_index;

    /* Set the prompt string */
    strcpy(dialog->prompt_str1, prompt1);
    strcpy(dialog->prompt_str2, prompt2);
    LUI_LabelDraw(dialog->prompt_label);

    /* Make the dialog box window with labels */
    LUIdialog_visible(name, 1);

    XSync(LUI_Display, 0);
    LUI_EventLock(dialog->window);
    return 0;
}

/******************************************************************************/
LUIdialog_visible(name, on)
    char *name;
    int on;
{
    LUI_DIALOG *dialog;

    if (!(dialog = LUIdialog_lookup_name(name))) {
        fprintf(stderr, "LUIdialog_visible(): can't find dialog box %s\n", name);
        return(0);
    }

    if (on) 
      XMapRaised(LUI_Display, dialog->window);
    else 
      XUnmapWindow(LUI_Display, dialog->window);

    return(1);
}

/******************************************************************************/
LUIdialog_process(label)
    LUI_LABEL *label;
{
    LUI_DIALOG *dialog;
    int which, LUIdialog_input_ok();

    if (!label->owner) {
        fprintf(stderr, "LUIdialog_process(): can't find owner for label %s\n",
                label->name);
        return(0);
    }

    dialog = (LUI_DIALOG *)(label->owner);
    if (SUBSTR("LUIdialog_Prompt", label->name)) 
      which = 0;
    else if (SUBSTR("LUIdialog_Ok", label->name)) 
      which = 1;
    else if (SUBSTR("LUIdialog_Cancel", label->name)) 
      which = 2;
    else if (SUBSTR("LUIdialog_Dialog", label->name)) 
      which = 3;

    switch(label->event->type) {
      case EnterNotify:
        if (which == 3 || which == 2 || which == 1) 
          LUI_LabelHighlight(label->name, 1);
        break;
      case LeaveNotify:
        if (which == 3 || which == 2 || which == 1) 
          LUI_LabelHighlight(label->name, 0);
        break;
      case ButtonPress:
        switch (which) {
          case 1:
            if (dialog->type == LUI_DIALOG_INPUT) {
                LUIdialog_input_ok(dialog->dialog_str,label);
                break;
            }
            else if (dialog->type == LUI_DIALOG_OUTPUT) {
                if (dialog->func)
                  (*dialog->func)( NULL, 1 );  /*** BrianP  2-2-91 ***/
            }
            
            LUIdialog_visible(dialog->name, 0);
            LUI_EventLock(0);
            break;
          case 2:
            if (dialog->func)         /*** BrianP  2-20-91 ***/
               (*dialog->func)( NULL, 0 );
            LUIdialog_visible(dialog->name, 0);
            LUI_EventLock(0);
            break;
        }
        break;
      case KeyPress:
        if (which != 3)
          break;
        LUI_LabelUserInput(label, 0, LUIdialog_input_ok, label->event, 0);
        break;
    }
    
    return(1);
}

/******************************************************************************/
LUIdialog_input_ok(str,label)
char        *str;
LUI_LABEL *label;
{
    LUI_DIALOG *dialog = (LUI_DIALOG *)label->owner;
    if (dialog->func)
      (*dialog->func)(str, 1);   /*** BrianP 2-2-91 ***/

    LUIdialog_visible(dialog->name, 0);
    LUI_EventLock(0);
}

/******************************************************************************/
LUIdialog_frame(window, event)
    Window window;
    XEvent *event;
{
    LUI_DIALOG *dialog;

    if (!(dialog = LUIdialog_lookup_window(window))) {
        fprintf(stderr, "LUIdialog_frame(): can't find dialog window %d\n", window);
        return(0);
    }

    switch (event->type) {
      case Expose:
        if (event->xexpose.count > 0)
          break;
        LUI_DrawFlatBorderAt(dialog->window, 
                             0, 0, dialog->width, dialog->height, dialog->border_size,
                             dialog->background_color);

        LUI_DrawFlatBorderAt(dialog->window, 
                             0, 0, dialog->width, dialog->height, 4,
                             dialog->border_color);

        LUI_DrawDropShadow(dialog->window, 
                           dialog->border_size, dialog->border_size,
                           dialog->width - (2 * dialog->border_size),
                           dialog->height - (2 * dialog->border_size), 6, 0x000000);
    }
}

/******************************************************************************/
LUIdialog_mark_char_position(label, x, y, width, height)
    LUI_LABEL *label;
    int x, y, width, height;
{
    static XPoint up[3];
    int txs;
    LUI_DIALOG *dialog;
    
    dialog = (LUI_DIALOG *)label->owner;
    if (*dialog->dialog_str == '\0') 
      txs = 0;
    else 
      txs = XTextWidth(LUI_Font, dialog->dialog_str, strlen(dialog->dialog_str));
    
    up[0].x = txs;
    up[1].x = txs + 5;
    up[2].x = txs + 10;
    
    up[0].y = height - 2;
    up[1].y = height - 10;
    up[2].y = up[0].y;
    
    XFillPolygon(LUI_Display, label->window, 
                 LUI_GC_bottom, up, 3, Nonconvex, CoordModeOrigin);
}
