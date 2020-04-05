/* browser.c */



#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include "lui.h"


/*
 * Test if the given name is a directory.
 */
static int isdir( char *name )
{
   struct stat stat_buff;

   if (stat( name, &stat_buff )!=0) return 0;

   if (stat_buff.st_mode & (unsigned) S_IFDIR)
      return 1;
   else
      return 0;
}



/*
 * Test if the given name is valid file.
 */
static int isfile( char *name )
{
   struct stat stat_buff;

   if (stat( name, &stat_buff )!=0) return 0;

   if ( (stat_buff.st_mode & (unsigned) S_IFDIR)==0 )
      return 1;
   else
      return 0;
}



/*
 * Comparison function for qsort used in read_dir.
 */
static int compare_entries( const void *a, const void *b )
{
   char *s1, *s2;

   s1 = *((char **) a);
   s2 = *((char **) b);
   return strcmp( s1, s2 );
}





static char **read_dir( char *path, int *count )
{
   DIR *dirf;
   struct dirent *dentry;
   char **entries;
   int i;

   entries = (char **) malloc( MAX_ENTRIES * sizeof(char *) );

   dirf = opendir( path );
   if (!dirf) {
      /* bad directory path */
      *count = 0;
      return NULL;
   }

   for (i=0;i<MAX_ENTRIES;) {
      char str[1000], fullname[1000];
      dentry = readdir(dirf);
      if (!dentry)
         break;
      strcpy( str, dentry->d_name );
      /* don't list . or .. */
      if (strcmp(str,".")!=0 && strcmp(str,"..")!=0) {
         /* append a / to directory entries */
         strcpy( fullname, path );
         strcat( fullname, str );
         if (isdir(fullname))
           strcat( str, "/" );
         entries[i] = strdup(str);
         i++;
      }
   }

   closedir( dirf );

   qsort( entries, i, sizeof(char*), compare_entries );

   *count = i;
   return entries;
}


/*
 * Load the list widget with the directory entries found in the dir
 * named by b->current.
 */
static void load_browser_list( LUI_BROWSER *b )
{
   char **entries;
   int count;

/*   LUI_ListUnload( b->list );*/
   entries = read_dir( b->current, &count );
   LUI_ListLoad( b->list, count, entries, 1 );
   b->selected = -1;
}




/*
 * Called when user clicks on a directory entry in the list.
 */
static int select_cb( LUI_LIST *list, int entry )
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
      load_browser_list( b );
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



/*
 * Called when user clicks on the parent button.
 */
static int parent_cb( LUI_NEWBUTTON *button, int state )
{
   LUI_BROWSER *b;
   int len;
   int pos;

   b = (LUI_BROWSER *) button->userdata;

   /* remove last segment of path name */
   len = strlen( b->current );
   if (len==0) return 0;
   pos = len-1;
   if (b->current[pos]!='/') {
      pos--;
      /* remove filename part of string */
      while (pos>=0 && b->current[pos]!='/')
         pos--;
   }

   /* remove last directory part of string */
   pos--; /* skip '/' */
   while (pos>=0 && b->current[pos]!='/')
      pos--;

   if (pos>=0) {
      b->current[pos+1] = 0;
   }

   LUI_FieldSetText( b->field, b->current );
   load_browser_list( b );
   return 1;
}


/*
 * Called when user clicks on the "Home" button.
 */
static int home_cb( LUI_NEWBUTTON *button, int state )
{
   LUI_BROWSER *b;
   char *pwd;

   b = (LUI_BROWSER *) button->userdata;

   pwd = getenv("HOME");
   if (pwd) {
      strcpy( b->current, pwd );
      strcat( b->current, "/" );
      LUI_FieldSetText( b->field, b->current );
   }

   load_browser_list( b );
   return 1;
}



/*
 * Called when user clicks on the "Root" button.
 */
static int root_cb( LUI_NEWBUTTON *button, int state )
{
   LUI_BROWSER *b;

   b = (LUI_BROWSER *) button->userdata;

   strcpy( b->current, "/" );
   LUI_FieldSetText( b->field, b->current );

   load_browser_list( b );
   return 1;
}



/*
 * Called when user changes type-in directory field.
 */
static int typein_cb( LUI_FIELD *field, char *text )
{
   LUI_BROWSER *b;
   int len;

   b = (LUI_BROWSER *) field->userdata;

   strcpy( b->current, text );

   if (isdir(text)) {
      len = strlen(text);
      if (len>0 && text[len-1]!='/') {
         strcat( b->current, "/" );
         LUI_FieldSetText( b->field, b->current );
      }
      load_browser_list( b );
   }
   else if (isfile(text)) {
      XUnmapWindow( LUI_Display, b->window );
      XSync( LUI_Display, False );  /* force unmap now */
      if (b->callback) {
         (*b->callback)( b, text );
      }
   }
   else {
      /* nothing */
   }
   return 1;
}



/*
 * Called when user clicks on the OK button.
 */
static int ok_cb( LUI_NEWBUTTON *button, int state )
{
   LUI_BROWSER *b;

   b = (LUI_BROWSER *) button->userdata;

   if (b->selected>=0) {
      /* call user callback */
      XUnmapWindow( LUI_Display, b->window );
      XSync( LUI_Display, False );  /* force unmap now */
      if (b->callback) {
         char filepath[1000];
         char *pwd = getenv( "PWD" );
         if (pwd && strncmp( b->current, pwd, strlen(pwd))==0) {
            /* omit prefix if it matches current directory */
            strcpy( filepath, b->current+strlen(pwd)+1 );
         }
         else {
            /* return whole path from root */
            strcpy( filepath, b->current );
         }
         strcat( filepath, b->list->strings[b->selected] );
         (*b->callback)( b, filepath );
      }
   }
   return 1;
}


/*
 * Called when user clicks on the OK button.
 */
static int okay_cb( LUI_NEWBUTTON *button, int state )
{
   LUI_BROWSER *b;

   b = (LUI_BROWSER *) button->userdata;

   if (b->selected>=0) {
      /* call user callback */
      XUnmapWindow( LUI_Display, b->window );
      XSync( LUI_Display, False );  /* force unmap now */
      if (b->callback) {
         char filepath[1000];
         strcpy( filepath, b->current );
         strcat( filepath, b->list->strings[b->selected] );
         (*b->callback)( b, filepath );
      }
   }
   return 1;
}



/*
 * Called when user clicks on the "Cancel" button.
 */
static int cancel_cb( LUI_NEWBUTTON *button, int state )
{
   LUI_BROWSER *b;

   b = (LUI_BROWSER *) button->userdata;

   XUnmapWindow( LUI_Display, b->window );
   return 1;
}
/*
 * Create a ctx browser widget.
 */


/*
 * Create a file browser widget.  This does *not* result in anything being
 * mapped to the display.  To begin a browsing "session" call the
 * LUI_BrowserActivate function.
 */
LUI_BROWSER *LUI_BrowserCreate( int width, int height )
{
   LUI_BROWSER *b;
   int gutter = LUI_LayoutGetGutter();
   char *pwd;

   b = (LUI_BROWSER *) malloc( sizeof(LUI_BROWSER) );
   if (!b)  return NULL;

   b->window = LUI_CreateWindow( LUI_RootWindow, width, height );

   b->width = width;
   b->height = height;
   pwd = getenv( "PWD" );
   if (pwd) {
      strcpy( b->current, pwd );
      strcat( b->current, "/" );
   }
   else {
      strcpy( b->current, "/" );
   }

   /* The file list */
   b->list = LUI_ListCreate( b->window, LUI_LEFT, LUI_TOP,
                             width - 2*gutter, height-5*gutter-30-2*24, 0 );
   b->list->userdata = (void *) b;
   LUI_ListCallback( b->list, select_cb );

   /* Parent, Home, and Root buttons */
   b->parent_button = LUI_PushButtonCreate( b->window, LUI_LEFT, LUI_NEXT_Y,
                                 80, 24, "Parent .." );
   b->parent_button->userdata = (void *) b;
   LUI_ButtonCallback( b->parent_button, parent_cb );

   b->home_button = LUI_PushButtonCreate( b->window, LUI_NEXT_X, LUI_SAME_Y,
                                 80, 24, "Home  ~" );
   b->home_button->userdata = (void *) b;
   LUI_ButtonCallback( b->home_button, home_cb );

   b->root_button = LUI_PushButtonCreate( b->window, LUI_NEXT_X, LUI_SAME_Y,
                                 80, 24, "Root  /" );
   b->root_button->userdata = (void *) b;
   LUI_ButtonCallback( b->root_button, root_cb );

   /* The current path field */
   b->field = LUI_FieldCreate( b->window, LUI_LEFT, LUI_NEXT_Y,
                               width - 2*gutter, 30 );
   b->field->userdata = (void *) b;
   LUI_FieldCallback( b->field, typein_cb );

   /* OK and Cancel buttons */
   b->ok_button = LUI_PushButtonCreate( b->window, LUI_LEFT, LUI_NEXT_Y,
                                 80, 24, "OK" );
   b->ok_button->userdata = (void *) b;
   LUI_ButtonCallback( b->ok_button, ok_cb );
   b->cancel_button = LUI_PushButtonCreate( b->window, LUI_NEXT_X, LUI_SAME_Y,
                                            80, 24, "Cancel" );
   b->cancel_button->userdata = (void *) b;
   LUI_ButtonCallback( b->cancel_button, cancel_cb );

   b->selected = -1;

/* MJK 12.04.98 */
/* 24Nov97  Phil McDonald */
/* MJK 12.08.98 why need this?? */
/*
   LUI_AddWidgetToWindow( LUI_RootWindow, b, (LUI_FNCP) LUI_BrowserDestroy );
*/

   return b;
}



/*
 * Register the callback function to call when the user clicks on the
 * "OK" button.
 * Input:  browser - which browser
 *         callback - the function to call when user clicks on "OK".  The
 *                    function should be declared as:
 *                       int callback( LUI_BROWSER *browser, char *filepath )
 */
void LUI_BrowserCallback( LUI_BROWSER *browser, int (*callback)() )
{
   browser->callback = callback;
   browser->context_index = context_index;
}



/*
 * Map the given browser with the given initial path.
 * Input:  browser - which browser to display
 *         path - initial path
 */
int LUI_BrowserActivate( LUI_BROWSER *browser, char *path )
{
   XMapWindow( LUI_Display, browser->window );
   XRaiseWindow( LUI_Display, browser->window );

   /* remove filename part from current pathname */
   while (browser->current[0] && !isdir(browser->current)) {
      /* find last '/' character */
      char *lastslash = strrchr( browser->current, '/' );
      /* truncate from last slash */
      *lastslash = 0;
   }
   if (path != NULL) strcpy(browser->current, path); /* WLH 6-27-96 */
   if (browser->current[0]==0) {
      strcpy( browser->current, "." );
   }
   /* add a trailing slash if there isn't one already */
   {
      int len = strlen( browser->current );
      if (len>0 && browser->current[len-1]!='/') {
         strcat( browser->current, "/" );
      }
   }

   LUI_FieldSetText( browser->field, browser->current );
   load_browser_list( browser );
   return 1;
}



/* MJK 12.04.98 */
/* 24Nov97  Phil McDonald */
void LUI_BrowserDestroy( LUI_BROWSER *bp )
{

    delete_event(bp->window);
    LUI_DestroyWindow( bp->window );
    /* free memory */
    LUI_ListUnload (bp->list);
    free(bp);
    return;
}
