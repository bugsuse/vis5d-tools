#ifdef SCCS
static char sccsid[]="@(#)popup.c        1.1 Stellar 89/02/09";
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
/*
 * LUI Popup Menus
 */

#include <stdio.h>

#define LUI_POPUP_DECLARE
#include "lui.h"

/******************************************************************************/
LUI_PopupInitialize(window)
    Window window;
{
    LUI_PopupMainWindow = window;
}

/******************************************************************************/
 LUI_POPUP *
LUI_PopupCreate(popup_name, n, names, funcs)
    char *popup_name;
    int n;
    char *names[];
    LUI_FNCP funcs[];
{
    Window window;
    LUI_POPUP *popup;
    LUI_LABEL *label;
    XSetWindowAttributes window_attributes;
    char *name, vname[64], *c, *strchr();
    register int i;
    int y;
    float hue;

    /* Create a window which contains all popup labels */
    window = LUI_CreateWindow(LUI_RootWindow, LUI_POPUP_WIDTH, LUI_POPUP_HEIGHT);
    window_attributes.override_redirect = True;
    XChangeWindowAttributes(LUI_Display, window, CWOverrideRedirect, &window_attributes);

    LUI_EventAdd(window, ExposureMask | ButtonPressMask | ButtonReleaseMask | 
             EnterWindowMask | LeaveWindowMask, LUI_PopupWindowProcess);

    /* Allocate a popup struct */
    if (!(popup = (LUI_POPUP *)malloc(sizeof(LUI_POPUP)))) {
        fprintf(stderr, "LUI_PopupCreate(): can't allocate new popup %s\n", popup_name);
        return(NULL);
    }

    /* Initialize the popup */
    if (!LUI_PopupHead) 
      LUI_PopupHead = popup;
    popup->window  = window;
    popup->visible = 0;
    popup->items   = n;
    popup->popup_label = NULL;
    strcpy(popup->name, popup_name);
    LUI_PopupMenues[LUI_PopupInd++] = popup;
    
    /* Create popup labels (i = 0 is the title label */
    for (i = 0; i <= n; ++i) {
        y = i * LUI_POPUP_HEIGHT; 

        if (i) {
            name = names[i-1];
            hue = -1.0;
        }
        else {
            name = popup_name;
            hue = -1.0;
        }

        /* construct visible name */
        strcpy(vname, name);
        if (c = strchr(vname, '%')) 
          *c = '\0';

        label = LUI_LabelOpen(name, 1, LUI_MakeLabel(vname), window, 
                          0, y, LUI_POPUP_WIDTH, LUI_POPUP_HEIGHT, 
                          LUI_POPUP_BORDER, LUI_PopupEntreeProcess);
        LUI_LabelClose(name);
        LUI_LabelHighlight2(label, 0);
        LUI_LabelVisible(name, 1);
        LUI_LabelColor(name, hue);

        /* Set label and func */
        if (i) {
            popup->label[i-1] = label;
            popup->funcs[i-1] = funcs[i-1];
        }
    }
    popup->context_index = context_index;

    /* Resize window to contain all the labels */
    XResizeWindow(LUI_Display, window, LUI_POPUP_WIDTH, LUI_POPUP_HEIGHT * (n + 1));

    return(popup);
}

/******************************************************************************/
LUI_PopupBind(popup_name, label_name)
    char *popup_name;
    char *label_name;
{
    LUI_LABEL *label;
    LUI_POPUP *popup;

    if (!(label = LUI_LabelLookupName(label_name))) {
        fprintf(stderr, "LUI_PopupBind(): can't find label %s\n", label_name);
        return(0);
    }
    if (!(popup = LUI_PopupLookup(popup_name))) {
        fprintf(stderr, "LUI_PopupBind(): can't find popup %s\n", popup_name);
        return(0);
    }

    label->data = popup_name;
    label->draw_func = LUI_PopupDrawArrow;

    popup->popup_label = label;
    return(1);
}

/******************************************************************************/
 static
LUI_PopupWindowProcess(window, event)
    Window window;
    XEvent *event;
{
    switch(event->type) {
      case ButtonRelease:
        LUI_PopupVisible(NULL, 0, 0.0, 0.0);
        break;
      case LeaveNotify:
        if (LUI_PopupActive[LUI_PopupActiveInd - 1]->window == window)
          LUI_PopupVisible(LUI_PopupActive[LUI_PopupActiveInd - 1]->name, 0, 0.0, 0.0);
        break;
    }
}

/******************************************************************************/
LUI_PopupEntreeProcess(label)
    LUI_LABEL *label;
{
    LUI_POPUP *popup;
    LUI_FNCP func;
    register int i;

    if (!LUI_PopupActiveInd)
      return(0);

    popup = LUI_PopupActive[LUI_PopupActiveInd - 1];

    /* Find the func associated with this entree */
    for (func = NULL, i = 0; i < popup->items; ++i) {
        if (label == popup->label[i]) {
            func = popup->funcs[i];
            break;
        }
    }

    switch(label->event->type) {
      case EnterNotify:
        if (func)
          LUI_LabelHighlight(label, 1);
        break;
      case LeaveNotify:
        if (label->data && label->event->xbutton.y > 5 && 
            label->event->xbutton.y < LUI_POPUP_HEIGHT - 5) {

            Window dummy_window;
            int x, y;

            LUI_LabelHighlight(label, 1);
            
            XTranslateCoordinates(LUI_Display, 
                                  label->window,
                                  LUI_RootWindow,
                                  0, 0,
                                  &x, &y, &dummy_window);
            
            LUI_PopupVisible(label->data, 1,
                         x + LUI_POPUP_WIDTH_2, y + LUI_POPUP_HEIGHT_2);
        }
        else if (func)
          LUI_LabelHighlight(label, 0);

        break;
      case ButtonRelease:
        if (func) {
            label->context_index = popup->context_index;
            (*(func))(label);
            LUI_LabelHighlight(label, 0);
        }
        LUI_PopupVisible(NULL, 0, 0.0, 0.0);
        break;
    }
    return(1);
}

/******************************************************************************/
 LUI_POPUP *
LUI_PopupLookup(name)
    char *name;
{
    register int i;
    
    for (i = 0; i < LUI_PopupInd; ++i) 
      if (!(strcmp(name, LUI_PopupMenues[i]->name))) 
        return(LUI_PopupMenues[i]);

    return(NULL);
}

/******************************************************************************/
LUI_PopupVisible(name, on, x, y)
    char *name;
    int on, x, y;
{
    register int i;
    register LUI_POPUP *popup;

    if (!name) {
        for (i = 0; i < LUI_PopupInd; ++i) 
          XUnmapWindow(LUI_Display, LUI_PopupMenues[i]->window);
        LUI_PopupActiveInd = 0;
        XSync(LUI_Display, 0);
        XUngrabPointer(LUI_Display, CurrentTime);
        XSync(LUI_Display, 0);
        return(1);
    }

    if (!(popup = LUI_PopupLookup(name))) {
        fprintf(stderr, "LUI_PopupVisible(): can't find popup %s\n", name);
        return(0);
    }

    popup->visible = on;

    if (on) {
        LUI_PopupActive[LUI_PopupActiveInd++] = popup;
        XMoveWindow(LUI_Display, popup->window, x, y);
        XGrabPointer(LUI_Display,
                     LUI_PopupMainWindow,
                     True,
                     ButtonPressMask 
                     | ButtonReleaseMask 
                     | ButtonMotionMask 
                     | LeaveWindowMask
                     | EnterWindowMask,
                     GrabModeAsync,
                     GrabModeAsync,
                     NULL,
                     XCreateFontCursor(LUI_Display,38),
                     CurrentTime);
                     
        XMapWindow(LUI_Display, popup->window);
        XRaiseWindow(LUI_Display, popup->window);
    }
    else if (LUI_PopupActiveInd > 1) {
        --LUI_PopupActiveInd;
        XUnmapWindow(LUI_Display, popup->window);
        if (popup->popup_label)
          LUI_LabelHighlight(popup->popup_label, 0);
    }
    
    XFlush(LUI_Display);
    return(1);
}

/******************************************************************************/
LUI_PopupDrawArrow(label, x, y, w, h)
    LUI_LABEL *label;
    int x, y, w, h;
{
    XPoint arrow[4];

    arrow[0].x = x + w - 2;  arrow[0].y = y + h / 2;
    arrow[1].x = x + w - 10; arrow[1].y = y + 2;
    arrow[2].x = x + w - 10; arrow[2].y = y + h - 2;

    XFillPolygon(LUI_Display, label->window, 
                 LUI_GC_black, arrow, 3, Nonconvex, CoordModeOrigin);
}
