#ifdef SCCS
static char sccsid[]="@(#)help.c        1.2 Stellar 89/03/22";
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
 * LUI File Display 
 */

#include <stdio.h>

#include "lui.h"

#define MAX_LINES 128

static int
  Help_X, Help_Y, Help_Xs, Help_Ys, Help_Border;

static int
  Help_On = 0;

static unsigned long
  Help_Color;

static Window
  Help_Window;

static char
  *Help_File;

static Pixmap
  Help_BkgPixmap = 0;

/******************************************************************************/
LUI_HelpDisplayFile(name, filename, parent, x, y, xs, ys, fg, bg, border)
    char *name;
    char *filename;
    Window parent;
    int x, y, xs, ys, border;
    unsigned long fg, bg;
{
    Window window;
    int LUI_HelpProcess();
    LUI_LABEL *label;
    char buf[MAX_LINES], *c, *strchr(), **farg, *file_argv[MAX_LINES];
    register int i, argc;

    if (Help_On) 
      return;

    window = LUI_CreateWindowAt(parent, x, y, xs, ys);
    if (Help_BkgPixmap)
      XSetWindowBackgroundPixmap(LUI_Display, window, Help_BkgPixmap);
    else 
      XSetWindowBackground(LUI_Display, window, bg);
    
    LUI_Cursor(window, LUI_QUESTION_CURSOR);

    XMapRaised(LUI_Display, window);

    LUI_EventAdd(window, ButtonPressMask | ExposureMask | VisibilityChangeMask,
                 LUI_HelpProcess);

    Help_X  = x;
    Help_Y  = y;
    Help_Xs = xs;
    Help_Ys = ys;
    
    Help_Border = border;
    Help_Color = fg;

    Help_Window = window;
    Help_File = filename;

    Help_On = 1;

    return(1);
}

/******************************************************************************/
 static
LUI_HelpDrawFile(window, file)
    Window window;
    char *file;
{
    FILE *pf;
    char buf[128], *c, *strchr();
    int x, y, xs, ys, i, in, lines;

    /* Open the help file */
    if (!(pf = fopen(file, "r"))) {
        fprintf(stderr, "LUI_HelpReadFile: can't open File file (%s)\n", file);
        return(0);
    }
    
    LUI_SetColor(Help_Color);

    /* Read in the help file and stuff up the text buffers */
    in = Help_Border + 20;
    for (lines = 0, y = in; fgets(buf, 127, pf); y += LUI_Font_yoff + 5) {
        if (c = strchr(buf, '\n')) *c = '\0';
        for (x = in, c = buf; *c == '\t'; ++c) 
          x += 50;
        
        XDrawString(LUI_Display, window, LUI_Gc, x, y, c, strlen(buf));
        ++lines;
    }
    fclose(pf);

    /* Place close message */
    sprintf(buf, "<<< Press in this window to close >>>>");
    y = Help_Ys - (LUI_Font_yoff + 5) * 2;
    x = (Help_Xs - XTextWidth(LUI_Font, buf, strlen(buf))) / 2;
    if (x < 0)
      x = 10;
    
    LUI_SetColor(0x000000);
    XDrawString(LUI_Display, window, LUI_Gc, x, y, buf, strlen(buf));
    LUI_SetColor(0xff0000);
    XDrawString(LUI_Display, window, LUI_Gc, x+1, y+1, buf, strlen(buf));
    
    return(1);
}

/******************************************************************************/
LUI_HelpSetBackgroundPixmap(file)
    char *file;
{
    int w, h;

    if (file) 
      Help_BkgPixmap = LUI_ReadPixmap(file, 0, 0, &w, &h);
    else 
      Help_BkgPixmap = 0;
}

/******************************************************************************/
 static
LUI_HelpProcess(window, event)
    Window window;
    XEvent *event;
{
    switch(event->type) {
      case ButtonPress:
        XUnmapWindow(LUI_Display, window);
        XSync(LUI_Display, 0);
        XDestroyWindow(LUI_Display, window);
        Help_On = 0;
        break;
      case Expose:
        if (event->xexpose.count == 0) {
            LUI_HelpDrawFile(Help_Window, Help_File);
            
            LUI_DrawFlatBorderAt(window, 
                                 0, 0, Help_Xs, Help_Ys,
                                 Help_Border, Help_Color);
            LUI_DrawDropShadow(window, 
                               Help_Border, Help_Border, 
                               Help_Xs - (2 * Help_Border),
                               Help_Ys - (2 * Help_Border),
                               Help_Border / 2,
                               0x0);
            break;
        }
      case VisibilityNotify:
        XMapRaised(LUI_Display, window);
        LUI_HelpDrawFile(Help_Window, Help_File);
        
        LUI_DrawFlatBorderAt(window, 
                             0, 0, Help_Xs, Help_Ys,
                             Help_Border, Help_Color);
        
        LUI_DrawDropShadow(window, 
                           Help_Border, Help_Border, 
                           Help_Xs - (2 * Help_Border),
                           Help_Ys - (2 * Help_Border),
                           Help_Border / 2,
                           0x0);
        break;
    }
}
