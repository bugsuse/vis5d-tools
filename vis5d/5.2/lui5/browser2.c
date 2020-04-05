/* browser.c */



#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include "lui.h"
#include "browser.h"
#include "../src/globals.h"
#include "../src/api.h"

#define MAX_ENTRIES 1000
#define MAX_CTX_ENTRIES 100



static char **read_ctx(  int *count, int display )
{
   char **entries;
   int i;
   int yo, yo2;
   int othercount;
   int whichones[VIS5D_MAX_CONTEXTS];
 
   entries = (char **) malloc( MAX_CTX_ENTRIES * sizeof(char *) );
   vis5d_get_num_of_ctxs_in_display( display, &yo, whichones);
   othercount = 0;
 
   for (i = 0; i < yo; i++){
      char str[1000], str2[1000], str3[1000];

      strcpy(str, "Context-");
      sprintf(str2, "%d", whichones[i]);
      strcat(str, str2);
      strcat(str, "  ");
      vis5d_get_context_name(whichones[i], str3);
      strcat(str, str3);
      entries[othercount] = strdup(str);
      othercount++;
   }

   vis5d_get_num_of_itxs_in_display( display, &yo, whichones);
   for (i = 0; i < yo; i++){
      char str[1000], str2[1000], str3[1000];

      strcpy(str, "I_Context-");
      sprintf(str2, "%d", whichones[i]);
      strcat(str, str2);
      strcat(str, "  ");
      vis5d_get_itx_name(whichones[i], str3);
      strcat(str, str3);
      entries[othercount] = strdup(str);
      othercount++;
   }

   *count = othercount;
   return entries;
}   


static void load_context_browser_list( LUI_BROWSER *b )
{
   char **entries;
   int count;

   entries = read_ctx( &count, b->display_number );
   LUI_ListLoad( b->list, count, entries, 1 );
   b->selected = -1;
}



static int context_select_cb( LUI_LIST *list, int entry )
{
   LUI_BROWSER *b;
   char *str;

   b = (LUI_BROWSER *) list->userdata;

   if (b->selected>=0) {
      LUI_ListSetStatus( b->list, b->selected, 0 );
   }

   str = b->list->strings[entry];
   if (str[strlen(str)-1]=='/') {
      strcat( b->current, str );
      LUI_FieldSetText( b->field, b->current );
      load_context_browser_list( b );
   }
   else {
      if (b->selected==entry) {
         b->selected = -1;
      }
      else {
         b->selected = entry;
      }
   }
   return 1;
}


LUI_BROWSER *LUI_ContextBrowserCreate( Window win, int x, int y,
                                       int width, int height, int display )
{
   LUI_BROWSER *b;
   int gutter = LUI_LayoutGetGutter();

   b = (LUI_BROWSER *) malloc( sizeof(LUI_BROWSER) );
   if (!b)  return NULL;

   b->window = LUI_CreateWindowAt( win, x, y, width, height ); 

   b->display_number = display;

   b->width = width;
   b->height = height;

   /* The file list */
   b->list = LUI_ListCreate( b->window, LUI_LEFT, LUI_TOP,
                             width - 2*gutter, height-2*gutter, 0 );
   b->list->userdata = (void *) b;
   b->list->display_number = display;

   LUI_ListCallback( b->list, context_select_cb );

   b->selected = -1;

   return b;
}

int LUI_ContextBrowserActivate( LUI_BROWSER *browser )
{
   XMapWindow( LUI_Display, browser->window );
   XRaiseWindow( LUI_Display, browser->window );

   load_context_browser_list( browser );
   return 1;
}

int LUI_ContextBrowserDeactivate( LUI_BROWSER *browser )
{
   XLowerWindow( LUI_Display, browser->window );
   XUnmapWindow( LUI_Display, browser->window );
 
   return 1;
}

