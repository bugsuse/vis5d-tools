/* browser.c */



#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include "lui.h"
#include "browser.h"
#include "newbrowser.h"



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
static void load_browser_list( LUI_NEWBROWSER *b )
{
   char **entries;
   int i, count;

/*   LUI_NEWListUnload( b->list );*/
   entries = read_dir( b->current, &count );
   LUI_NEWListLoad( b->list, count, entries, 1 );
   for (i = 0; i < MAX_NUM_SELECTED; i++){
      b->selected[i] = -1;
   }
   b->num_selected = 0;
   b->cur_selected = 0;
}

static void add_or_remove_entry_to_list( LUI_NEWBROWSER *b, int entry, int status)
{
    int i, j;

   if (status){
      b->selected[b->num_selected] = entry;
      b->num_selected++;
   }
   else{
      for (i = 0; i < b->num_selected; i++){
         if ( b->selected[i] == entry){
             for (j = i; j <  b->num_selected - 1; j++){
                b->selected[j] = b->selected[j+1];
             }
             b->selected[b->num_selected-1] = -1;
             b->num_selected--;
         }
      }
   }
}




/*
 * Called when user clicks on a directory entry in the list.
 */
static int select_cb( LUI_NEWLIST *list, int entry, int status )
{
   LUI_NEWBROWSER *b;
   char *str;
   int i;

   b = (LUI_NEWBROWSER *) list->userdata;


/*
   for (i = 0; i < b->num_selected; i++){
      LUI_NEWListSetStatus( b->list, b->selected[i], 1);
   }
*/

   str = b->list->strings[entry];
   if (b->num_selected == 0 && str[strlen(str)-1]=='/') {
      strcat( b->current, str );
      LUI_FieldSetText( b->field, b->current );
      load_browser_list( b );
   }
   else if (str[strlen(str)-1] != '/'){
      add_or_remove_entry_to_list( b, entry, status);
   }
   else{
      LUI_NEWListSetStatus( b->list, entry, 0);
   }
   return 1;
}



/*
 * Called when user clicks on the parent button.
 */
static int parent_cb( LUI_NEWBUTTON *button, int state )
{
   LUI_NEWBROWSER *b;
   int len;
   int pos;

   b = (LUI_NEWBROWSER *) button->userdata;

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
   LUI_NEWBROWSER *b;
   char *pwd;

   b = (LUI_NEWBROWSER *) button->userdata;

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
   LUI_NEWBROWSER *b;

   b = (LUI_NEWBROWSER *) button->userdata;

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
   LUI_NEWBROWSER *b;
   int len;
   char lentil[500][500];

   b = (LUI_NEWBROWSER *) field->userdata;

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
      strcpy(lentil[0], text);
      b->num_selected = 1;
      if (b->callback) {
         (*b->callback)( b, lentil );
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
   LUI_NEWBROWSER *b;
   int i;


   b = (LUI_NEWBROWSER *) button->userdata;

   if (b->num_selected > 0){
      /* call user callback */
      XUnmapWindow( LUI_Display, b->window );
      XSync( LUI_Display, False );  /* force unmap now */
      if (b->callback) {
         char filepaths[500][500];
         char *pwd = getenv( "PWD" );
         for (i = 0; i < b->num_selected; i++){
            if (pwd && strncmp( b->current, pwd, strlen(pwd))==0) {
               /* omit prefix if it matches current directory */
               strcpy( filepaths[i], b->current+strlen(pwd)+1 );
            }
            else {
               /* return whole path from root */
               strcpy( filepaths[i], b->current );
            }
            strcat( filepaths[i], b->list->strings[b->selected[i]] );
         }
         (*b->callback)( b, filepaths );
      }
   }
   return 1;
}


/*
 * Called when user clicks on the OK button.
 */
static int okay_cb( LUI_NEWBUTTON *button, int state )
{
   LUI_NEWBROWSER *b;
   int i;

   b = (LUI_NEWBROWSER *) button->userdata;

   if (b->num_selected > 0){
      /* call user callback */
      XUnmapWindow( LUI_Display, b->window );
      XSync( LUI_Display, False );  /* force unmap now */
      if (b->callback) {
         char filepaths[500][500];
         for (i = 0; i < b->num_selected; i++){
            strcpy( filepaths[i], b->current );
            strcat( filepaths[i], b->list->strings[b->selected[i]] );
         }
         (*b->callback)( b, filepaths );
      }
   }
   return 1;
}



/*
 * Called when user clicks on the "Cancel" button.
 */
static int cancel_cb( LUI_NEWBUTTON *button, int state )
{
   LUI_NEWBROWSER *b;

   b = (LUI_NEWBROWSER *) button->userdata;

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
LUI_NEWBROWSER *LUI_NEWBrowserCreate( int width, int height )
{
   LUI_NEWBROWSER *b;
   int i, gutter = LUI_LayoutGetGutter();
   char *pwd;

   b = (LUI_NEWBROWSER *) malloc( sizeof(LUI_NEWBROWSER) );
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
   b->list = LUI_NEWListCreate( b->window, LUI_LEFT, LUI_TOP,
                             width - 2*gutter, height-5*gutter-30-2*24, 0 );
   b->list->userdata = (void *) b;
   LUI_NEWListCallback( b->list, select_cb );

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

   for (i = 0; i < MAX_NUM_SELECTED; i++){
      b->selected[i] = -1;
   }
   b->num_selected = 0;
   b->cur_selected = 0;

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
 *                       int callback( LUI_NEWBROWSER *browser, char **filepath )
 */
void LUI_NEWBrowserCallback( LUI_NEWBROWSER *browser, int (*callback)() )
{
   browser->callback = callback;
   browser->context_index = context_index;
}



/*
 * Map the given browser with the given initial path.
 * Input:  browser - which browser to display
 *         path - initial path
 */
int LUI_NEWBrowserActivate( LUI_NEWBROWSER *browser, char *path )
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
void LUI_NEWBrowserDestroy( LUI_NEWBROWSER *bp )
{

    delete_event(bp->window);
    LUI_DestroyWindow( bp->window );
    /* free memory */
    LUI_NEWListUnload (bp->list);
    free(bp);
    return;
}
