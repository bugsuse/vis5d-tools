#ifdef SCCS
static char sccsid[]="@(#)button.c        1.2 Stellar 89/03/22";
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
 * LUI Button routines
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LUI_BUTTON_DECLARE
#include "lui.h"
#include "pixmaps.h"


static int LUI_ButtonPadWidth  = 0;
static int LUI_ButtonPadHeight = 0;
static Window LUI_ButtonWindow = 0;
static LUI_BUTTON_PAD *LUI_ButtonPadHead = NULL;
static LUI_BUTTON_PAD *LUI_ButtonPad = NULL;
static LUI_BUTTON *LUI_ButtonHead;


/*
 * Draw a button pad when an expose event is generated.
 */
static int LUI_ButtonPadDraw( LUI_BUTTON_PAD *bp, XEvent *event )
{
   Window window;

   window = event->xany.window;

   /* Squelch multiple redraws */
   if (event->xexpose.count > 0)
     return 0;

   LUI_FrameWindow2(window, bp->width, bp->height, bp->border_size );
   return 1;
}




LUI_BUTTON_PAD *LUI_ButtonPadOpenType( char *name, Window parent,
                                       int type, int x, int y, int border_size,
                                       unsigned long background_color )
{
    LUI_BUTTON_PAD *button_pad;
/*    char *LUI_GetPathName();*/

    button_pad = LUI_ButtonPadOpen(name, parent, x, y, border_size, background_color);
    button_pad->type = type;

    switch(type) {
      case LUI_BUTTON_RADIO:
/* Replaced:
        path = LUI_GetPathName(LUI_BUTTON_RADIO_PIXMAP_IN);
        button_pad->pixmap_in  = LUI_ReadPixmap(path, 0, 0, &w, &h);
        free(path);
        path = LUI_GetPathName(LUI_BUTTON_RADIO_PIXMAP_OUT);
        button_pad->pixmap_out = LUI_ReadPixmap(path, 0, 0, &w, &h);
        free(path);
   With:
*/
        button_pad->pixmap_in = LUI_PixmapRadioIn;
        button_pad->pixmap_out = LUI_PixmapRadioOut;
/* By:  Brian Paul  On:  3-15-91 */
        break;
      case LUI_BUTTON_OPTION:
/* Replaced:
        path = LUI_GetPathName(LUI_BUTTON_OPTION_PIXMAP_IN);
        button_pad->pixmap_in  = LUI_ReadPixmap(path,  0, 0, &w, &h);
        free(path);
        path = LUI_GetPathName(LUI_BUTTON_OPTION_PIXMAP_OUT);
        button_pad->pixmap_out = LUI_ReadPixmap(path, 0, 0, &w, &h);
        free(path);
   With:
*/
        button_pad->pixmap_in = LUI_PixmapOptionIn;
        button_pad->pixmap_out = LUI_PixmapOptionOut;
/* By:  Brian Paul  On:  3-15-91 */
        break;
    }

    return(button_pad);
}


LUI_BUTTON_PAD *LUI_ButtonPadOpen( char *name, Window parent,
                                   int x, int y, int border_size,
                                   unsigned long background_color )
{
    LUI_BUTTON_PAD *button_pad;

    /* Allocate the button pad */
    if (!(button_pad = (LUI_BUTTON_PAD *)malloc(sizeof(LUI_BUTTON_PAD)))) {
        printf("LUI_ButtonPadOpen(): can't allocate button pad %s\n", name);
        return(NULL);
    }

    /* Initialize placement variables */
    LUI_ButtonPadWidth  = 0;
    LUI_ButtonPadHeight = 0;

    /* Create the button pad window */
    LUI_ButtonWindow = LUI_CreateWindow(parent, 0, 0);

    XSetWindowBackground(LUI_Display, LUI_ButtonWindow, LUI_Color_gray);

    /* Set the cursor */
    LUI_Cursor(LUI_ButtonWindow, LUI_MAIN_CURSOR);

    /* Add Expose event */
    LUI_EventAdd2(LUI_ButtonWindow, ExposureMask,
                  (LUI_FNCP) LUI_ButtonPadDraw, button_pad);

    /* Set Current button pad */
    LUI_ButtonPad = button_pad;

    /* Setup button pad variables */
    strcpy(button_pad->name, name);
    button_pad->type        = 0;
    button_pad->x           = x;
    button_pad->y           = y;
    button_pad->window      = LUI_ButtonWindow;
    button_pad->border_size = border_size; 
    button_pad->hue         = LUI_DefaultHue;
    button_pad->background_color = LUI_Color_gray;

    /* Add button pad to head of list */
    button_pad->np = LUI_ButtonPadHead;
    LUI_ButtonPadHead  = button_pad;

    /* put this widget on delete list for window */
    /*LUI_AddWidgetToWindow( parent, button_pad,
                           (LUI_FNCP) LUI_ButtonPadDestroy );
     */

    return(button_pad);
}



LUI_BUTTON_PAD *LUI_ButtonPadReopen( char *name )
{
   register LUI_BUTTON_PAD *bp;

   /* find the button pad data struct */
   for (bp = LUI_ButtonPadHead; bp; bp = bp->np) {
      if (strcmp(bp->name, name)==0) {
         LUI_ButtonPad = bp;
         break;
      }
   }
   if (!LUI_ButtonPad) {
      return NULL;
   }

   LUI_ButtonWindow = bp->window;
   LUI_ButtonPadWidth = bp->width - bp->border_size;
   LUI_ButtonPadHeight = bp->height - bp->border_size;;

   return bp;
}



void LUI_ButtonPadClose( char *name )
{
    int x, y, width, height;

    /* Check for errors */
    if (!LUI_ButtonPad) {
        fprintf(stderr, "LUI_ButtonPadClose: Can't find open ButtonPad (%s)!\n", name);
        exit(-1);
    }

    /* Set width and height */
    LUI_ButtonPad->width  = LUI_ButtonPadWidth  + LUI_ButtonPad->border_size;
    LUI_ButtonPad->height = LUI_ButtonPadHeight + LUI_ButtonPad->border_size; /* + 1; */

    /* Reset size of window and reposition it */
    x      = LUI_ButtonPad->x;
    y      = LUI_ButtonPad->y;
    width  = LUI_ButtonPad->width;
    height = LUI_ButtonPad->height;
    XMoveResizeWindow(LUI_Display, LUI_ButtonWindow, x, y, width, height);

    /* Reset Button Pad Globals */
    LUI_ButtonPad = NULL;
}


void LUI_ButtonPadColor( char *name, float hue )
{
    register LUI_BUTTON_PAD *bp;

    for (bp = LUI_ButtonPadHead; bp; bp = bp->np) 
      if (!strcmp(bp->name, name)) break;

    if (bp) 
      bp->hue = hue;
}


LUI_ButtonPadMove( char *name, int x, int y )
{
    register LUI_BUTTON_PAD *bp;

    for (bp = LUI_ButtonPadHead; bp; bp = bp->np) 
      if (!strcmp(bp->name, name)) {
          XMoveWindow(LUI_Display, bp->window, x, y);
          return(1);
      }

    return(0);
}


void LUI_ButtonPadVisible( char *name, int on_off )
{
    register LUI_BUTTON_PAD *bp;

    for (bp = LUI_ButtonPadHead; bp; bp = bp->np) 
      if (!strcmp(bp->name, name)) break;

    if (bp) {
        if (on_off) {
          XMapWindow  (LUI_Display, bp->window);
          bp->visible = 1;
        }
        else {
          XUnmapWindow(LUI_Display, bp->window);
          bp->visible = 0;
        }
    }
    else 
        printf("LUI_ButtonPadVisible: can't find ButtonPad %s\n", name);
}


LUI_BUTTON_PAD *LUI_ButtonPadLookup( Window window )
{
    register LUI_BUTTON_PAD *bp;

    for (bp = LUI_ButtonPadHead; bp; bp = bp->np) 
        if (bp->window == window) return(bp);

    return(NULL);
}
   

void LUI_ButtonPadQuery( char *name, int *x, int *y, int *width, int *height )
{
    register LUI_BUTTON_PAD *bp;

    for (bp = LUI_ButtonPadHead; bp; bp = bp->np) 
        if (!strcmp(bp->name, name)) break;

    if (bp) {
        *x      = bp->x;
        *y      = bp->y;
        *width  = bp->width;
        *height = bp->height;
    }
    else 
      fprintf(stderr, "LUI_ButtonPadQuery: can't find ButtonPad %s\n", name);
}



void LUI_ButtonDraw( LUI_BUTTON *bp )
{
    LUI_LABEL *label;

    label = LUI_LabelLookupWindow(bp->window);

    if (!(bp->state)) {
       /* Button is turned off */
/*       LUI_LabelColor(label->name, LUI_DefaultHue);*/
       label->color[LUI_LABEL_COL_FRONT] = LUI_Color_md_grey;
       label->border_size = 5;
       label->text_drop_shadow = 0;
    }
    else {
       /* Button is turned on */
       label->color[LUI_LABEL_COL_HILITE1] = bp->hicolor;
       label->border_size = 10;
       label->text_drop_shadow = 0;

/* MJK 12.04.98 */
/* 03Feb98  Phil McDonald	use white on dark or blue-dominated colors */
       label->color[LUI_LABEL_COL_TEXT] = LUI_Color_black;
       if (((label->color[LUI_LABEL_COL_HILITE1] & 0x00ff0000) < 0x00990000) &&
           ((label->color[LUI_LABEL_COL_HILITE1] & 0x0000ff00) < 0x00009900))
           label->color[LUI_LABEL_COL_TEXT] = LUI_Color_white;
    }

    LUI_LabelHighlight2( label, bp->state);
}



static void LUI_ButtonProcess( LUI_LABEL *label )
{
    XEvent *event;
    register LUI_BUTTON_PAD *button_pad;
    register LUI_BUTTON *button, *bp;

    for (button = LUI_ButtonHead; button; button = button->np) 
        if (button->window == label->window)
          break;

    if (!button) return;
    if (!button->active) return;

    event = label->event;
    button->event = label->event;
    button_pad = button->button_pad;

    switch (event->type) {
      case Expose:
        LUI_ButtonDraw(button);
        break;

      case EnterNotify:
        if (button->state == LUI_BUTTON_OUT)
          LUI_LabelHighlightText(label, 1);
        break;

      case LeaveNotify:
        if (button->state == LUI_BUTTON_OUT)
          LUI_LabelHighlightText(label, 0);
        break;

      case ButtonPress:

        switch (event->xbutton.button) {
          case Button1:
          case Button2:
          case Button3:

            /* RADIO BUTTON : only current selection is on */
            if (button->group == LUI_BUTTON_RADIO) {
                /* Will be on - turn off all other radio buttons */
                if (button->state == LUI_BUTTON_OUT) {
                    /* Find all buttons selected for this group */
                    for (bp = LUI_ButtonHead; bp; bp = bp->np) {
                        if (bp->group == button->group && 
                            bp->state == LUI_BUTTON_IN &&
                            bp->button_pad == button_pad) {

                                /* Automatically turn it off */
                                bp->state = LUI_BUTTON_OUT;
                                LUI_ButtonDraw(bp);
                            }
                    }
                    /* Turn this button on */
                    button->state = LUI_BUTTON_IN;

                    /* Draw this button */
                    LUI_ButtonDraw(button);

                    /* Dispatch this button's function */
                    if (button->func) 
                      (*(button->func))(button);
                }
                
                /* Will be off */
                else{
                  LUI_ButtonDraw(button);
                  if (button->mousebutton != event->xbutton.button){
                     button->mousebutton = event->xbutton.button;
                     if (button->func){
                        (*(button->func))(button);
                     }
                  }
                }
                button->mousebutton = event->xbutton.button;
            }

            /* Just like a toggle */
            else if (button->group == LUI_BUTTON_OPTION) {
                if (button->state == LUI_BUTTON_OUT) button->state = LUI_BUTTON_IN;
                else button->state = LUI_BUTTON_OUT;
                
                LUI_ButtonDraw(button);

                /* Dispatch function */
                if (button->func) 
                  (*(button->func))(button);
            }

                /* If oneshot, press in now, out after function */
            else if (button->group == LUI_BUTTON_ONESHOT) {
                XSync(LUI_Display, 0);
                button->state = LUI_BUTTON_IN;
                LUI_ButtonDraw(button);

                XSync(LUI_Display, 0);
                if (button->func)
                  (*(button->func))(button);

                XSync(LUI_Display, 0);
                button->state = LUI_BUTTON_OUT;
                LUI_ButtonDraw(button);
            }

            /* If group is other than 0 */
            else if (button->group == LUI_BUTTON_FUNCTION) {
                /* Will be on - turn off all other radio buttons */
                if (button->state == LUI_BUTTON_OUT) {
                    /* Find all buttons selected for this group */
                    for (bp = LUI_ButtonHead; bp; bp = bp->np) {
                        if (bp->group == button->group && 
                            bp->state == LUI_BUTTON_IN &&
                            bp->button_pad == button_pad) {

                                /* Automatically turn it off */
                                bp->state = LUI_BUTTON_OUT;
                                LUI_ButtonDraw(bp);
                            }
                    }
                    /* Turn this button on */
                    button->state = LUI_BUTTON_IN;

                    /* Draw this button */
                    LUI_ButtonDraw(button);

                    /* Dispatch this button's function */
                    if (button->func) 
                      (*(button->func))(button);
                }
                
                /* Will be off */
                else 
                  LUI_ButtonDraw(button);
            }

            else if (button->group == LUI_BUTTON_TOGGLE) {
                if (button->state == LUI_BUTTON_OUT) button->state = LUI_BUTTON_IN;
                else button->state = LUI_BUTTON_OUT;
                

/**** Modified by Marief May 24 1989****/
/**** switch of button->func and LUI_ButtonDraw to allow the application to ***/
/**** change the button draw -- color for instance***/
                /* Dispatch function */
                if (button->func) 
                  (*(button->func))(button);

                LUI_ButtonDraw(button);
            }

            else if (button->group == LUI_BUTTON_CONFIG) {
                if (button->state == LUI_BUTTON_OUT) button->state = LUI_BUTTON_IN;
                else button->state = LUI_BUTTON_OUT;

                /* Dispatch function */
                if (button->func)
                  (*(button->func))(button);
            }

            else if (button->group == LUI_BUTTON_NODRAW) {
                /* Dispatch function */
                if (button->func)
                  (*(button->func))(button);
            }
            break;
        }
        break;
        
      case ButtonRelease:
        break;
    }
}




LUI_BUTTON *LUI_ButtonCreateType( char *name, int x, int y, int size,
                                  int index, LUI_FNCP func )
{
    LUI_BUTTON *button;

    button = LUI_ButtonCreate(name, LUI_ButtonPad->type, x, y, size,index, func);

    return(button);
}


static void LUI_DrawRadioButton( LUI_LABEL *label, int x, int y, int xs, int ys )
{
    LUI_BUTTON *button;

    button = (LUI_BUTTON *)label->owner;

    if (button->state == LUI_BUTTON_IN) {
        if (button->pixmap_in)
          XCopyArea(LUI_Display, button->pixmap_in, label->window, 
                    LUI_GC_fore, x, y, ys, ys, 0, 0);
          
    }
    else if (button->state == LUI_BUTTON_OUT) {
        if (button->pixmap_out)
          XCopyArea(LUI_Display, button->pixmap_out, label->window, 
                    LUI_GC_fore, x, y, ys, ys, 0, 0);
          
    }
}


static void LUI_DrawOptionButton( LUI_LABEL *label, int x, int y, int xs, int ys )
{    
    LUI_DrawRadioButton(label, x, y, xs, ys);
}



LUI_BUTTON *LUI_ButtonCreate( char *name, int type, int x, int y, int size,
                              int index, LUI_FNCP func )
{
    LUI_LABEL *label;
    register LUI_BUTTON *button;
    int width, height;
    char name_str[128];

    /* Get the size of the window */
    height = LUI_BUTTON_YS - 10;
    width  = size;

/******* added by Marief, June 21st 1989 ********/
/*    if (index==NULL) index= 0;*/

    /* Adjust position based on Button size */
    x = x * width  + LUI_ButtonPad->border_size;
    y = y * height + LUI_ButtonPad->border_size; /* + 1; */

    /* Adjust max width and height of Button Pad window */
    if ((x + width ) > LUI_ButtonPadWidth ) LUI_ButtonPadWidth  = x + width;
    if ((y + height) > LUI_ButtonPadHeight) LUI_ButtonPadHeight = y + height;

    /* Make a label */
    if (LUI_ButtonPad->type == LUI_BUTTON_RADIO || 
        LUI_ButtonPad->type == LUI_BUTTON_OPTION) 
      sprintf(name_str, "  %s", name);
    else 
      sprintf(name_str, "%s", name);

    label = LUI_LabelOpen(name, 1, (char **)LUI_MakeLabel(name_str), 
                              LUI_ButtonWindow, 
                              x, y, width, height, 
                              10, (LUI_FNCP) LUI_ButtonProcess);
    LUI_LabelClose(name);
    XSetWindowBackground(LUI_Display, label->window, LUI_ButtonPad->background_color);

    LUI_LabelVisible(name, 1);
    LUI_LabelColor(name, LUI_DefaultHue2);
    label->color[LUI_LABEL_COL_FRONT] = LUI_Color_md_grey;
    LUI_LabelHighlight2(label, 0);
    LUI_LabelDropShadow(name, 1);

    if (type == LUI_BUTTON_RADIO)
      LUI_LabelSetDrawFunc(label, (LUI_FNCP) LUI_DrawRadioButton);
    else if (type == LUI_BUTTON_OPTION)
      LUI_LabelSetDrawFunc(label, (LUI_FNCP) LUI_DrawOptionButton);

    /* Allocate a new button */
    if (!(button = (LUI_BUTTON *)malloc(sizeof(LUI_BUTTON)))) {
        fprintf(stderr, "LUI_ButtonCreate: can't allocate new button\n");
        return(NULL);
    }

    /* Initialize button */
    strcpy(button->label, name);
    button->state      = LUI_BUTTON_OUT;
    button->active     = 1;
    button->group      = type;
    button->x          = x;
    button->y          = y;
    button->width      = width;
    button->height     = height;
    /******* added by Marief, June 21st 1989 ********/
    button->index      = index;

    button->hicolor = LUI_Color_white;

    button->func       = (LUI_FNCP) func;
    button->context_index  = context_index;
    button->window     = label->window;
    button->button_pad = LUI_ButtonPad;
    button->pixmap_in  = LUI_ButtonPad->pixmap_in;
    button->pixmap_out = LUI_ButtonPad->pixmap_out;
    label->owner       = (char *)button;

    /* Add button to head of list */
    button->np = LUI_ButtonHead;
    LUI_ButtonHead = button;

    return(button);
}




/*
 * Set the state of a button.  Added on March 8, 1995 by BEP.
 * Input:  button - pointer to the LUI button
 *         state - 0 = off, non-zero = on
 */
int LUI_ButtonState( LUI_BUTTON *button, int state )
{
   if (state) {
      button->state = LUI_BUTTON_IN;
   }
   else {
      button->state = LUI_BUTTON_OUT;
   }
   LUI_ButtonDraw( button );
   return 1;
}

/* Set the button state, but don't redraw the button if
 * it is the same state that is wanted
 * Input:  button - pointer to the LUI button
 *         state - 0 = off, 1  = on
 */
int LUI_SetButtonState( LUI_BUTTON *button, int state )
{

   if (state) {
      if (button->state = state){
         return 1;
      }
      else {
         button->state = LUI_BUTTON_IN;
      }
   }
   else{
      if (button->state = state){
         return 1;
      }
      else {
         button->state = LUI_BUTTON_OUT;
      }
   }
   LUI_ButtonDraw( button );
   return 1;
}


/*
 * Set the highlight color of a button.  Added on March 8, 1995 by BEP.
 * Input:  button - pointer to the LUI button
 *         r, g, b - the color components in [0,255]
 */
int LUI_ButtonSetColor( LUI_BUTTON *button, int r, int g, int b )
{
   XColor xcol;

   xcol.red = r << 8;
   xcol.green = g << 8;
   xcol.blue = b << 8;
   if (!LUI_XAllocColor( LUI_Display, LUI_Colormap,
                         LUI_Visual->map_entries, &xcol )) {
      printf("xalloc color failed in lui_buttonsetcolor\n");
   }

   if (button->hicolor!=LUI_Color_white) {
      /* Dont' free the default high light color (white) because */
      /* it's used a lot. */
      LUI_FreeColor( button->hicolor );
/*      XFreeColors( LUI_Display, LUI_Colormap, &button->hicolor, 1, 0);*/
   }

   button->hicolor = xcol.pixel;

   LUI_ButtonDraw( button );
   return 1;
}



void LUI_ButtonChangeLabel( LUI_BUTTON *button, char *name )
{
    LUI_LABEL *label;

    label = LUI_LabelLookupWindow(button->window);
    
    LUI_LabelChangeText(label, 1, (char **)LUI_MakeLabel(name));
}



LUI_BUTTON *LUI_ButtonLookupWindow( Window window )
{
    register LUI_BUTTON *bp;

    for (bp = LUI_ButtonHead; bp; bp = bp->np) 
        if (bp->window == window)  
          return(bp);

    return(NULL);
}


LUI_BUTTON *LUI_ButtonLookupName( char *name )
{
    register LUI_BUTTON *bp;

    for (bp = LUI_ButtonHead; bp; bp = bp->np) 
        if (!(strcmp(bp->label, name))) 
          return(bp);

    return(NULL);
}


/* Added by BEP on 1-6-93 */
LUI_BUTTON_PAD *LUI_ButtonPadLookupName( char *name )
{
   LUI_BUTTON_PAD *bp;
   for (bp = LUI_ButtonPadHead; bp; bp = bp->np) {
      if (strcmp(bp->name, name)==0) {
         return bp;
      }
   }
   return NULL;
}




/****************************************************************************/
/******** LUI DESTROY LOGIC * BILLH 10-4-91 *********************************/
/****************************************************************************/



/* added by billh 10-4-91 */
void LUI_ButtonDestroy( LUI_BUTTON *bp )
{
  LUI_BUTTON *lbp, *nbp;

/*  LUI_LabelFindDestroy(bp); this causes the same window to be freed twice!*/
  lbp = NULL;
  nbp = LUI_ButtonHead;
  while (nbp != NULL) {
    if (nbp == bp) {
      /* this is the button */
      /* delete from button list */
      if (lbp == NULL) LUI_ButtonHead = bp->np;
      else lbp->np = bp->np;
      /* delete from event list */
      delete_event(bp->window);
      LUI_DestroyWindow( bp->window );
      /* free memory */
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
void LUI_ButtonFindDestroy( LUI_BUTTON_PAD *owner )
{
  LUI_BUTTON *bp;

GOT_ONE:
  bp = LUI_ButtonHead;
  while (bp != NULL) {
    if (bp->button_pad == owner) {
      LUI_ButtonDestroy(bp);
      goto GOT_ONE;
    }
    bp = bp->np;
  }
  /* not found */
  return;
}


/* added by billh 10-4-91 */
void LUI_ButtonPadDestroy( LUI_BUTTON_PAD *bp )
{
  LUI_BUTTON_PAD *lbp, *nbp;

  LUI_ButtonFindDestroy(bp);
  lbp = NULL;
  nbp = LUI_ButtonPadHead;
  while (nbp != NULL) {
    if (nbp == bp) {
      /* this is the button pad */
      /* delete from button pad list */
      if (lbp == NULL) LUI_ButtonPadHead = bp->np;
      else lbp->np = bp->np;
      LUI_DestroyWindow( bp->window );
      /* delete from event list */
      delete_event(bp->window);
      /* free memory */
      free(bp);
      return;
    }
    lbp = nbp;
    nbp = nbp->np;
  }
  /* not found */
  return;
}



