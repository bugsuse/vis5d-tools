#ifdef SCCS
static char sccsid[]="@(#)label.c        1.3 Stellar 89/03/22";
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
 * LUI Labels
 */

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LUI_LABEL_DECLARE
#include "lui.h"

static LUI_LABEL *LUI_LabelHead = NULL;
static LUI_LABEL *LUI_Label     = NULL;
static Window LUI_LabelWindow = 0;


void LUI_LabelFrame( LUI_LABEL *label )
{
   /* OBSOLETE */
}


void LUI_LabelDrawText( LUI_LABEL *label )
{
    Window window;
    int i, bs;
    int xs, ys /*,txs, tys*/;
    
    window = label->window;
    
    bs = 0;

    if (label->drop_shadow == 1) {
        xs = label->width  - 4;
        ys = label->height - 4;

        LUI_SetColor(LUI_Color_black);
        XFillRectangle(LUI_Display, window, LUI_Gc, 4, 4, xs, ys);
    }
    else {
        xs = label->width;
        ys = label->height;
    }
    
    LUI_SetColor(label->background_color);
    XFillRectangle(LUI_Display, window, LUI_Gc, bs, bs, xs, ys);
    
    /* Center the text strings */
    for (i = 0; i < label->argc; ++i) {
        int   size, x, y, oy, yb  /*, char_size*/;
        char *line;
        
        line = label->argv[i];
        size = strlen(line);
        /*if (!size) continue;*/
        
#ifdef OLD
        /* Reset size of string if not enough space */
        txs = XTextWidth(label->font, line, size);
        if (txs > xs) {
            char_size = txs / size;
            size = xs / char_size;
        }
        x = xs/2.0 - XTextWidth(label->font, line, size) / 2.0 + 0.5;
#endif
        x = 5;
        
        /* y position based on size of block of text relative to size of window */
        yb = label->font_yoff * label->argc + (4 * (label->argc - 1));
        oy = (ys - yb) / 2.0 + bs;
        y  = oy + (label->font_yoff * (i+1)) + (4 * i) + 1;

        if (label->text_drop_shadow) {
            LUI_SetColor(LUI_Color_black);
            XDrawString(LUI_Display, window, LUI_Gc, x+1, y+1, line, size);
            LUI_SetColor(LUI_Color_white);
        }
        else {
            /* Always do the drop shadow -- unless the color is black */
            if (label->color[LUI_LABEL_COL_TEXT] != LUI_Color_black) {
                LUI_SetColor(LUI_Color_black);
                XDrawString(LUI_Display, window, LUI_Gc, x+1, y+1, line, size);
            }
            LUI_SetColor(label->color[LUI_LABEL_COL_TEXT]);
        }

        XDrawString(LUI_Display, window, LUI_Gc, x, y, line, size);

    }

    /* Call the label drawing function */
    if (label->draw_func)
      (*label->draw_func)(label, bs, bs, xs, ys);  
}


void LUI_LabelHighlight2( LUI_LABEL *label, int on )
{
    /* Set the background color */
    if (!on) {
      label->background_color = label->color[LUI_LABEL_COL_FRONT];
/*********Modified by Marief May 24 ,1989 *************/
/*       LUI_ColorHilite = LUI_Color_white;*/ /* BEP 2-13-92 */

      if (label->image) {
        label->image_on = 1;
        XClearWindow(LUI_Display, label->window);
        LUI_LabelFrame(label);
        return;
      }
      else
        LUI_LabelFrame(label);
    }
    else {
      if (label->image) 
        label->image_on = 0;

/*********Modified by Marief May 24 ,1989 *************/
      if (on == 1){
          label->background_color = label->color[LUI_LABEL_COL_HILITE1];
/*        LUI_ColorHilite = LUI_Color_white;*/ /* BEP 2-13-92 */
      }
      else if (on == 2)
        label->background_color = label->color[LUI_LABEL_COL_HILITE2];

      if (label->image) 
        label->image_on = 0;

      LUI_LabelFrame(label);
    }

    /* Draw it if it's visible */
    if (label->visible && !label->image_on)
      LUI_LabelDrawText(label);
}


LUI_LABEL *LUI_LabelLookupName( char *name )
{
    register LUI_LABEL *label;

    for (label = LUI_LabelHead; label; label = label->np) 
        if (!(strcmp(name, label->name))) return(label);

    return(NULL);
}




int LUI_LabelVisible( char *name, int on )
{
    register LUI_LABEL *label;

    if (!(label = LUI_LabelLookupName(name))) {
        fprintf(stderr, "LUI_LabelVisible: can't find Label %s\n", name);
        return(0);
    }

    if (on) {
        XMapWindow(LUI_Display, label->window);
        label->visible = 1;
    }
    else {
        XUnmapWindow(LUI_Display, label->window);
        label->visible = 0;
    }

    return(1);
}



LUI_LABEL *LUI_LabelCreate( char *name, char *str, Window parent,
                            int x, int y, int width, int height,
                            int border_size, int hilite, int vis,
                            LUI_FNCP func )
{
    LUI_LABEL *label;
    char tmpstr[4096], *pstr, *s[64], *c, **argv;
    int argc = 0, len, i;

    /* Parse string */
    strcpy(tmpstr, str);
    pstr = tmpstr;
    while (c = (char *)strchr(pstr, '\n')) {
        *c = '\0';
        len = strlen(pstr);
        if (!(s[argc] = (char *)malloc(len + 1))) {
            fprintf(stderr, "LUI_LabelCreate(): can't malloc string for label %s\n", name);
            return(NULL);
        }
        strcpy(s[argc], pstr);
        ++argc;
        pstr += len + 1;
    }
    len = strlen(pstr);
    if (!(s[argc] = (char *)malloc(len + 1))) {
        fprintf(stderr, "LUI_LabelCreate(): can't malloc string for label %s\n", name);
        return(NULL);
    }
    strcpy(s[argc], pstr);
    ++argc;

    /* Allocate array of string pointers */
    if (!(argv = (char **)malloc(argc * sizeof(char *)))) {
        fprintf(stderr, "LUI_LabelCreate(): can't malloc string for label %s\n", name);
        return(NULL);
    }

    /* Stuff array */
    for (i = 0; i < argc; ++i) 
      *(argv+i) = s[i];

    /* Auto-size the label if necessary */
    if (width == 0) {
        XWindowAttributes wattr;

        XGetWindowAttributes(LUI_Display, parent, &wattr);
        width = wattr.width - 2 * x;
    }
    if (height == 0) {
        XWindowAttributes wattr;

        XGetWindowAttributes(LUI_Display, parent, &wattr);
        height = wattr.height - 2 * y;
    }

    label = LUI_LabelOpen(name, argc, argv, parent, x, y, width, height, border_size, func);
    LUI_LabelClose(name);
    LUI_LabelVisible(name, vis);
    LUI_LabelHighlight2( label, hilite);
    
    return(label);
}   
    

void LUI_LabelDraw( LUI_LABEL *label )
{
    /* If not visible, don't draw it */
    if (!label->visible) 
      return;

    LUI_LabelFrame(label);

    if (!label->image_on)
      LUI_LabelDrawText(label);
}



static void LUI_LabelProcess( LUI_LABEL *label, XEvent *event )
{
    /* Set the label event */
    label->event = event;

    /* Process the event */
    switch (event->type) {

      /* The label's function handles all these events */
      case KeyPress:
      case KeyRelease:
      case ButtonPress:
      case ButtonRelease:
      case EnterNotify:
      case LeaveNotify:
        /* Dispatch the label function with its handle */
        if (label->func) 
           (*(label->func))(label);
        break;
    
      /* Exposure and Resizing events handled here */
      case Expose:
        /* Squelch multiple redraws */
        if (event->xexpose.count > 0) return;

        if (label->image) {
            XClearWindow(LUI_Display, label->window);
            LUI_LabelFrame(label);
            break;
        }

      case ResizeRequest:
        /* Draw the label */
        LUI_LabelDraw(label);
        break;
    }
}


LUI_LABEL *LUI_LabelOpen( char *name, int argc, char **argv, Window parent,
                          int x, int y, int width, int height, int border_size,
                          LUI_FNCP func )
{
    LUI_LABEL *label;

    /* Allocate the label */
    if (!(label = (LUI_LABEL *)malloc(sizeof(LUI_LABEL)))) {
        printf("LUI_LabelOpen: can't allocate label (%s)\n", name);
        return(NULL);
    }

    /* Create the label window */
    LUI_LabelWindow = LUI_CreateWindow(parent, 0, 0);

    /* Add Label events */
    LUI_EventAdd2( LUI_LabelWindow,
                   KeyPressMask    | KeyReleaseMask  |
                   EnterWindowMask | LeaveWindowMask |
                   ExposureMask    | ButtonPressMask | ButtonReleaseMask,
                   (LUI_FNCP) LUI_LabelProcess, label );

    /* Set Current label */
    LUI_Label = label;

    /* Setup label variables */
    strcpy(label->name, name);
    label->x           = x;
    label->y           = y;
    label->width       = width;
    label->height      = height;
    label->window      = LUI_LabelWindow;
    label->border_size = border_size;
    label->argc        = argc;
    label->argv        = argv;
    label->visible     = 0;
    label->func        = func;
    label->context_index = context_index;
    label->draw_func   = NULL;
    label->image       = 0;
    label->image_on    = 0;
    label->font        = LUI_Font;
    label->font_yoff   = LUI_Font_yoff;
    label->data        = NULL;
    label->drop_shadow = 0;
    label->text_drop_shadow = 0;
    label->background_color = LUI_Color_black;

    label->color[LUI_LABEL_COL_TEXT    ] = LUI_Color_black;
    label->color[LUI_LABEL_COL_LEFT    ] = LUI_Color_gray;  /* not used? */
    label->color[LUI_LABEL_COL_TOP     ] = LUI_Color_md_grey;  /* not used? */
    label->color[LUI_LABEL_COL_RIGHT   ] = LUI_Color_white;  /* not used? */
    label->color[LUI_LABEL_COL_BOTTOM  ] = LUI_Color_gray;  /* not used? */
    label->color[LUI_LABEL_COL_FRONT   ] = LUI_Color_gray;
    label->color[LUI_LABEL_COL_HILITE1 ] = LUI_Color_white;
    label->color[LUI_LABEL_COL_HILITE2 ] = LUI_Color_white; /*sl_grey*/

/*********Modified by Marief May 24 ,1989 *************/
/*This new variable allows the application to change the hilite color easely***/
 /*   LUI_ColorHilite=label->color[LUI_LABEL_COL_HILITE1 ]; */ /* BEP 2-13-92*/

    /* Dimension each of the arguments */
    if (argc > LUI_LABEL_MAX_ARGSIZE) {
        fprintf(stderr, "LUI_LabelOpen: label contains more than %d strings\n", LUI_LABEL_MAX_ARGSIZE);
        label->argc = LUI_LABEL_MAX_ARGSIZE;
    }

    /* Add label to head of list */
    label->np = LUI_LabelHead;
    LUI_LabelHead = label;

    /* Add to deletion list */
/*    LUI_AddWidgetToWindow( parent, label, LUI_LabelDestroy );*/

    return(label);
}


void LUI_LabelClose( char *name )
{
    int x, y, width, height;

    /* Check for errors */
    if (!LUI_Label) {
        fprintf(stderr, "LUI_LabelClose: Can't find open Label (%s)!\n", name);
        exit(-1);
    }

    /* Reset size of window and reposition it */
    x      = LUI_Label->x;
    y      = LUI_Label->y;
    width  = LUI_Label->width;
    height = LUI_Label->height;
    XMoveResizeWindow(LUI_Display, LUI_LabelWindow, x, y, width, height);

    /* Reset Label  Globals */
    LUI_Label = NULL;
}




int LUI_LabelDropShadow( char *name, int on )
{
    LUI_LABEL *label;

    if (!(label = LUI_LabelLookupName(name))) {
        fprintf(stderr, "LUI_LabelDropShadow(): can't find label %s\n", name);
        return(0);
    }

    label->drop_shadow = on;
    return(1);
}


int LUI_LabelSetDrawFunc( LUI_LABEL *label, LUI_FNCP func )
{
    label->draw_func = func;
    label->context_index = context_index;
    return(1);
}


int LUI_LabelTextColor( char  *name, float r, float g, float b )
{
    LUI_LABEL *label;

    if (!(label = LUI_LabelLookupName(name))) {
        fprintf(stderr, "LUI_LabelTextColor: can't find label %s\n", name);
        return(0);
    }

    label->color[LUI_LABEL_COL_TEXT] = LUI_MakeColor((int)(255.*r),
                                                (int)(255.*g), (int)(255.*b));
}


void LUI_LabelColor( char *name, int hue )
{
    LUI_LABEL *label;
    int r, g, b;

    if (!(label = LUI_LabelLookupName(name))) {
        fprintf(stderr, "LUI_LabelColor: can't find label %s\n", name);
        return;
    }


    LUI_RgbShade( hue, 0.40, &r, &g, &b );
    label->color[LUI_LABEL_COL_LEFT    ] = LUI_MakeColor(r, g, b);

    LUI_RgbShade( hue, 0.80, &r, &g, &b );
    label->color[LUI_LABEL_COL_TOP     ] = LUI_MakeColor(r, g, b);

    LUI_RgbShade( hue, 0.90, &r, &g, &b );
    label->color[LUI_LABEL_COL_RIGHT   ] = LUI_MakeColor(r, g, b);

    LUI_RgbShade( hue, 0.60, &r, &g, &b );
    label->color[LUI_LABEL_COL_BOTTOM  ] = LUI_MakeColor(r, g, b);

    LUI_RgbShade( hue, 0.70    /*.50*/ , &r, &g, &b );
    label->color[LUI_LABEL_COL_FRONT   ] = LUI_MakeColor(r, g, b);

    LUI_RgbShade( hue, 1.0, &r, &g, &b );
    label->color[LUI_LABEL_COL_HILITE1 ] = LUI_MakeColor(r, g, b);

    LUI_RgbShade( hue, 1.00, &r, &g, &b );
    label->color[LUI_LABEL_COL_HILITE2 ] = LUI_MakeColor(r, g, b);

    label->background_color = label->color[LUI_LABEL_COL_FRONT];
}



/******************************************************************************/
#ifdef LEAVEOUT
LUI_LabelSetImage(name, file, w, h)
    char *name, *file;
    int *w, *h;
{
    XSetWindowAttributes window_attributes;
    Pixmap pixmap, LUI_ReadPixmap();
    register LUI_LABEL *label;

    if (!(label = LUI_LabelLookupName(name))) {
        fprintf(stderr, "LUI_LabelVisible: can't find Label %s\n", name);
        return(0);
    }

    if (!(pixmap = LUI_ReadPixmap(file, 0, 0, w, h))) 
      return(0);

    /* Set the background pixmap */
    label->image    = 1;
    label->image_on = 1;
    label->color[LUI_LABEL_COL_HILITE1] = label->color[LUI_LABEL_COL_HILITE2] =
      label->color[LUI_LABEL_COL_FRONT];
    window_attributes.background_pixmap = pixmap;
    XChangeWindowAttributes(LUI_Display, label->window, 
                            CWBackPixmap, &window_attributes);

    return(1);
}
#endif



void LUI_LabelChangeText( LUI_LABEL *label, int argc, char **argv )
{
    label->argc = argc;
    label->argv = argv;

    LUI_LabelDrawText(label);
}


int LUI_LabelQuery( char *name, int *x, int *y, int *width, int *height )
{
    LUI_LABEL *label;

    if (!(label = LUI_LabelLookupName(name))) {
        fprintf(stderr, "LUI_LabelQuery: can't find Label %s\n", name);
        return(0);
    }

    *x      = label->x;
    *y      = label->y;
    *width  = label->width;
    *height = label->height;

    return(1);
}


void LUI_LabelHighlightText( LUI_LABEL *label, int on )
{
    label->text_drop_shadow = on;

    if (label->visible && !label->image_on)
      LUI_LabelDrawText(label);
}


void LUI_LabelHighlight( char *name, int on )
{
    LUI_LABEL *label;

    /* Lookup the label */
    if (!(label = LUI_LabelLookupName(name)))
      return;

    LUI_LabelHighlight2( label, on );
}


void LUI_LabelMoveResize( char *name, int x, int y, int width, int height,
                          int border_size )
{
    register LUI_LABEL *label;

    /* Lookup the label */
    if (!(label = LUI_LabelLookupName(name)))
      return;

    /* Reset size of window and reposition it */
    label->x       = x;
    label->y       = y;
    label->width   = width;
    label->height  = height;
    label->border_size = border_size;

    /* Issue resize request */
    XMoveResizeWindow(LUI_Display, label->window, x, y, width, height);

    /* Redraw the label */
    LUI_LabelDraw(label);
}


void LUI_LabelMove( char *name, int x, int y )
{
    register LUI_LABEL *label;

    /* Lookup the label */
    if (!(label = LUI_LabelLookupName(name)))
      return;

    /* Reset size of window and reposition it */
    label->x = x;
    label->y = y;

    /* Issue resize request */
    XMoveWindow(LUI_Display, label->window, x, y);

    /* Redraw the label */
    LUI_LabelDraw(label);
}


void LUI_LabelSetFont( char *name, char *fontname )
{
    fprintf(stderr, "LUI_LabelSetFont(): warning: function not supported\n");
}



LUI_LABEL *LUI_LabelLookupWindow( Window window )
{
    register LUI_LABEL *label;

    for (label = LUI_LabelHead; label; label = label->np) 
        if (label->window == window) return(label);

    return(NULL);
}
   


#define C_RETURN  0x0d
#define C_DELETE1 0x7f  /* delete    */
#define C_DELETE2 0x08  /* Control h */
#define C_CLEAR   0x15  /* Control u */


#ifdef LEAVEOUT
void LUI_LabelUserInput( LUI_LABEL *label, int line, LUI_FNCP func,
                         XEvent *event_ptr, int auto_erase )
{
    Window window;
    XEvent event;
    int totchr;
    char buf[16], str[128], c, *dialog, *dp;

    /* Get the window and display */
    window = label->window;

    /* Grab the string */
    dialog = label->argv[line];
    strcpy(str, dialog);
    totchr = strlen(str);

    /* Re-allocate some space if needed (bad place for this) */
    if (label->argsize[line] < 128) {
        if (!(dp = (char *)malloc(128))) {
            fprintf(stderr, "LUI_KeyboardInput: can't malloc string\n");
            exit(-1);
        }
        label->argsize[line] = 128;
        strcpy(dp, dialog);

        label->argv[line] = dialog = dp;
    }

    /* Grab key presses until out of window */
    for (;;) {
        if (event_ptr->type == KeyRelease) continue;

        if (XLookupString(&event_ptr->xkey, buf, 1, NULL, NULL) != 1)
          break;
            
        /* Not interested in special characters */
        c = buf[0];
        if (!(isalnum(c)     ||
              isspace(c)     || 
              ispunct(c)     || 
              c == C_RETURN  || 
              c == C_DELETE1 || 
              c == C_DELETE2 ||
              c == C_CLEAR))
          break;

        /* Carriage return: set the string, update the label, and leave */
        if (c == C_RETURN) {
            if (totchr) {
                strcpy(dialog, str);
                LUI_LabelDrawText(label);

                /* Dispatch the function with the current string */
                if (func) {  /* was (*func) */
                  label->context_index = context_index;
                  (*(func))(str,label);
                }
            }
            break;
        }

        /* Delete */
        else if (c == C_DELETE1 || c == C_DELETE2) {
            if (auto_erase) 
              totchr = 0;
            else
              if (totchr > 0) --totchr;
            str[totchr] = '\0';
        }
        else if (c == C_CLEAR) {
            totchr = 0;
            str[totchr] = '\0';
        }

        /* Anything else: add the character into the buffer */
        else {
            str[  totchr] = c;
            str[++totchr] = '\0';
        }

        strcpy(dialog, str);
        LUI_LabelDrawText(label);

        /* Grab the next event */
        XNextEvent(LUI_Display, &event);

        event_ptr = &event;

        if (event_ptr->xany.window != window ||
            event_ptr->type != KeyPress ||
            event_ptr->type != KeyRelease) {

            LUI_EventDispatch(&event);
            break;
        }
    }
}
#endif


/****************************************************************************/
/********** LUI DESTROY LOGIC * BILLH 10-4-91 *******************************/
/****************************************************************************/


/* added by billh 10-4-91 */
/* can (and should) be applied to any bp returned by
   LUI_LabelCreate or LUI_LabelOpen */
void LUI_LabelDestroy( LUI_LABEL *bp )
{
  LUI_LABEL *lbp, *nbp;

  lbp = NULL;
  nbp = LUI_LabelHead;
  while (nbp != NULL) {
    if (nbp == bp) {
      /* this is the label */
      /* delete from label list */
      if (lbp == NULL) LUI_LabelHead = bp->np;
      else lbp->np = bp->np;
      /* delete from event list */
      delete_event(bp->window);
      LUI_DestroyWindow( bp->window );
      /* free memory */

/* MJK 12.04.98 */
/* 24Nov97  Phil McDonald */
/*
      if (bp->argv)
      {
          while (bp->argc--) if (bp->argv[bp->argc]) free (bp->argv[bp->argc]);
          free (bp->argv);
      }
*/




      free(bp);
      return;
    }
    lbp = nbp;
    nbp = nbp->np;
  }
  /* not found */
  return;
}


/* added by billh 10-4-91 */
void LUI_LabelFindDestroy( char *owner )
{
  LUI_LABEL *bp;

  bp = LUI_LabelHead;
  while (bp != NULL) {
    if (bp->owner == owner) {
      LUI_LabelDestroy(bp);
      return;
    }
    bp = bp->np;
  }
  /* not found */
  return;
}



