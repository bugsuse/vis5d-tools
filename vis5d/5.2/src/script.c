/*  script.c */


/* Vis5D version 5.2 */

/*
Vis5D system for visualizing five dimensional gridded data sets
Copyright (C) 1990 - 2000 Bill Hibbard, Johan Kellum, Brian Paul,
Dave Santek, and Andre Battaiola.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/



/* Tcl script interface for Vis5D */

/* NOTE: Tcl 7.5 should not be used with Vis5D!  There's a bug in 7.5
 * which causes stdin/stdout to malfuntion after the first interpreter
 * is deleted.  Use Tcl 7.4 until 7.6 is released.
 */


#include <ctype.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <sys/resource.h>
#include "api.h"
#include "globals.h"
#include "graphics.h"
#include "gui.h"
#include "tclsave.h"



#define ABS(X)  ( (X) < 0 ? -(X) : (X) )


#ifdef TCL
#  include <tcl.h>
#else

/*
 * If Tcl isn't available we use the following replacement functions which
 * emulate Tcl's most basic features.  With these functions we can at
 * least parse and execute simple Vis5D "Tcl save" files.
 */

#define MAX_COMMANDS 250
#define MAX_ARGS     100
#define MAX_ARG_LEN  100
#define MAX_VARS     100

#define TCL_RESULT_SIZE 200

#define TCL_OK        0
#define TCL_ERROR     1
#define TCL_RETURN    2
#define TCL_BREAK     3
#define TCL_CONTINUE  4


typedef void *ClientData;


typedef struct {
   char            *result;      /* result string */
   struct private  *p;           /* private interpreter data */
} Tcl_Interp;


typedef int (Tcl_CmdProc) (ClientData clientData,
                           Tcl_Interp *interp,
                           int argc, char *argv[] );

typedef int *Tcl_Command;

typedef void (Tcl_CmdDeleteProc) (ClientData clientData);


struct private {
   int         num_cmd;                        /* number of commands */
   char        cmd_name[MAX_COMMANDS][100];    /* command names */
   Tcl_CmdProc *cmd_func[MAX_COMMANDS];        /* command C func pointers */
   ClientData  client_data[MAX_COMMANDS];      /* client's data */

   int         num_vars;                       /* number of variables */
   char        var_name[MAX_VARS][100];        /* names of variables */
   char        var_value[MAX_VARS][100];       /* values of variables */
};


static char TmpResult[TCL_RESULT_SIZE];

static int Tcl_EvalFile( Tcl_Interp *interp, char *filename );

/*
 * Assign a value to a variable.
 */
static void assign_var( Tcl_Interp *interp, char *varname, char *value )
{
   int i;

   for (i=0;i<interp->p->num_vars;i++) {
      if (strcmp(interp->p->var_name[i], varname)==0) {
         /* var already defined, replace its value */
         strcpy( interp->p->var_value[i], value );
         return;
      }
   }
   /* new variable */
   i = interp->p->num_vars;
   strcpy( interp->p->var_name[i], varname );
   strcpy( interp->p->var_value[i], value );
   interp->p->num_vars++;
}



/*
 * Replace a variable with its value.  Note that varname and result may
 * point to the same dataspace.
 */
static int eval_var( Tcl_Interp *interp, char *varname, char *result )
{
   int i;

   for (i=0;i<interp->p->num_vars;i++) {
      if (strcmp(interp->p->var_name[i],varname)==0) {
         strcpy( result, interp->p->var_value[i] );
         return 1;
      }
   }
   /* var not found */
   return 0;
}



/*
 * Implements Tcl's set command.
 */
static int cmd_set( ClientData client_data, Tcl_Interp *interp,
                    int argc, char *argv[] )
{
   if (argc==2) {
      /* print current value */
      if (eval_var( interp, argv[1], interp->result )) {
         return TCL_OK;
      }
      else {
         sprintf( interp->result,"can't read \"%s\": no such variable",
                  argv[1] );
         return TCL_ERROR;
      }
   }
   else if (argc==3) {
      assign_var( interp, argv[1], argv[2] );
      strcpy( interp->result, argv[2] );
      return TCL_OK;
   }
   else {
      sprintf( interp->result,
               "wrong # args: should be \"set varName ?newValue?\"" );
      return TCL_ERROR;
   }
}



/*
 * Implements Tcl's source command.
 */
static int cmd_source( ClientData client_data, Tcl_Interp *interp,
                       int argc, char *argv[] )
{
   if (argc!=2) {
      sprintf( interp->result, "Error: source requires a filename argument" );
      return TCL_ERROR;
   }
   return Tcl_EvalFile( interp, argv[1] );
}



/*
 * Create an interpreter
 */
static Tcl_Interp *Tcl_CreateInterp( void )
{
   Tcl_Interp *interp;
   interp = (Tcl_Interp *) calloc( 1, sizeof(Tcl_Interp) );
   if (interp) {
      interp->p = (struct private *) calloc( 1, sizeof(struct private) );
      /* register the set command */
      strcpy( interp->p->cmd_name[0], "set" );
      interp->p->cmd_func[0] = cmd_set;
      /* register the sourc command */
      strcpy( interp->p->cmd_name[1], "source" );
      interp->p->cmd_func[1] = cmd_source;
      interp->p->num_cmd = 2;
   }
   return interp;
}


/*
 * Delete an interpreter.
 */
static void Tcl_DeleteInterp( Tcl_Interp *interp )
{
   free( interp->p );
   free( interp );
}


/*
 * Evaluate a Tcl/Vis5D command.
 */
static int Tcl_Eval( Tcl_Interp *interp, char *cmd )
{
   int argc;
   char *argv[MAX_ARGS];
   char arg[MAX_ARGS][MAX_ARG_LEN];
   char error[1000];
   int i;
   char *cp;
   int inquote;  /* inside a quoted string? */
   int inlist;   /* inside a {...} list? */

   /* Init results string */
   interp->result = TmpResult;
   interp->result[0] = 0;

   if (cmd[0]=='#') {
      /* comment */
      return TCL_OK;
   }

   /* break cmd string into an argv list */

   cp = cmd;
   argc = 0;
   i = 0;
   inquote = 0;
   inlist = 0;
   while (*cp) {
      if (*cp=='\"') {
         if (!inquote) {
            /* just skip the opening quote */
            inquote = 1;
         }
         else {
            /* end of quoted string */
            if (i>0) {
               /* finish token */
               arg[argc][i] = 0;
               argc++;
               i = 0;
            }
            inquote = 0;
         }
      }
      else if (*cp=='{' && !inquote) {
         /* begining of a list */
         inlist = 1;
         /* ignore '{' char */
      }
      else if (*cp=='}' && !inquote) {
         /* end of list */
         if (i>0) {
            /* finish token */
            arg[argc][i] = 0;
            argc++;
            i = 0;
         }
         inlist = 0;
      }
      else if ((*cp==' ' || *cp=='\t' || *cp=='\n') && !inquote && !inlist) {
         if (i>0) {
            /* end of a token */
            arg[argc][i] = 0;
            argc++;
            i = 0;
         }
      }
      else {
         /* add char to current token */
         arg[argc][i++] = *cp;
      }
      cp++;
   }
   /* finish last token */
   if (i>0) {
      arg[argc][i] = 0;
      argc++;
   }

   if (argc==0) {
      /* no arguments is OK */
      return TCL_OK;
   }

   /* setup pointers */
   for (i=0;i<argc;i++) {
      argv[i] = arg[i];
   }

   /* Perform variable substitution for args with $ prefix */
   for (i=0;i<argc;i++) {
      if (arg[i][0]=='$') {
         if (!eval_var( interp, arg[i]+1, arg[i] )) {
            sprintf( interp->result,
                     "can't read \"%s\": no such variable", arg[i]+1 );
            return TCL_ERROR;
         }
      }
   }

   /* Now search for function which matches arg[0] */
   for (i=0;i<interp->p->num_cmd;i++) {
      if (strcmp(interp->p->cmd_name[i],arg[0])==0) {
         /* call the user-function */
         int code;
         code = (*interp->p->cmd_func[i])
                           ( interp->p->client_data[i], interp, argc, argv );
         return code;
      }
   }

   /* command not found! */
   sprintf( interp->result, "invalid command name \"%s\"", arg[0] );
   return TCL_ERROR;
}



/*
 * Evaluate Tcl/Vis5D commands from a text file.
 */
static int Tcl_EvalFile( Tcl_Interp *interp, char *filename )
{
#define MAXLINE 1000
   FILE *f;
   char line[MAXLINE];
   int len, code;

   f = fopen( filename, "r" );
   if (!f) {
      interp->result = TmpResult;
      interp->result[0] = 0;
      return TCL_ERROR;
   }

   while (!feof(f)) {
      fgets( line, MAXLINE, f );
      if (!feof(f)) {
         len = strlen(line);
         if (len>0) {
            /* if length > 0 then remove trailing newline and eval it */
            if (line[len-1]=='\n') {
               line[len-1] = 0;
            }
            code = Tcl_Eval( interp, line );
            if (code!=TCL_OK) {
               fclose(f);
               return code;
            }
         }
      }
   }

   fclose(f);
   return TCL_OK;
}


/*
 * Add a new Tcl command.
 */
static Tcl_Command Tcl_CreateCommand( Tcl_Interp *interp,
                                      char *cmdName,
                                      Tcl_CmdProc *proc,
                                      ClientData clientData,
                                      Tcl_CmdDeleteProc *deleteProc )
{
   if (interp->p->num_cmd<MAX_COMMANDS) {
      strcpy( interp->p->cmd_name[interp->p->num_cmd], cmdName );
      interp->p->cmd_func[interp->p->num_cmd] = proc;
      interp->p->num_cmd++;
   }
   else {
      printf("Fatal error in Tcl_CreateCommand!\n");
      abort();
   }
   return (Tcl_Command) proc;
}



static void Tcl_AppendElement( Tcl_Interp *interp, char *string )
{
   if (!interp->result) {
      interp->result = TmpResult;
      interp->result[0] = 0;
   }
   if (strlen(interp->result)+1+strlen(string)<TCL_RESULT_SIZE) {
      strcat( interp->result, " " );
      strcat( interp->result, string );
   }
   else {
      fprintf( stderr,
              "Error in Tcl_AppendResult: too long of result string\n");
   }
}


#endif /*TCL*/



/* WLH 8 Oct 98 */
/*
 * storage for name / value pairs
 */
typedef struct name_value {
  char *name;
  char *value;
  struct name_value *next;
} NameValue;

/* WLH 8 Oct 98 */
static NameValue *NameValueHead = NULL;


#define MAXPROJARGS 100



/*
 * Given a string which contains numbers separated by white space return
 * an array of those values as floats.
 * Input:  str - the input string
 *         max - size of x[] array
 * Output:  x - array of values
 * Return:  number of values found
 */
static int string_to_float_array( char *str, int max, float x[] )
{
   char buffer[100];
   int i, j, n;

   j = 0;
   n = 0;
   for (i=0;str[i];i++) {
      if (!isspace(str[i])) {
         buffer[j++] = str[i];
      }
      else {
         /* found white space */
         if (j>0) {
            buffer[j] = 0;
            x[n] = atof(buffer);
            n++;
            j = 0;
         }
      }
   }
   if (j>0) {
      buffer[j] = 0;
      x[n] = atof(buffer);
      n++;
   }
   return n;
}



/*
 * Like above function but return an array of integers.
 */
static int string_to_int_array( char *str, int max, int x[] )
{
   char buffer[100];
   int i, j, n;

   j = 0;
   n = 0;
   for (i=0;str[i];i++) {
      if (!isspace(str[i])) {
         buffer[j++] = str[i];
      }
      else {
         /* found white space */
         if (j>0) {
            buffer[j] = 0;
            x[n] = atoi(buffer);
            n++;
            j = 0;
         }
      }
   }
   if (j>0) {
      buffer[j] = 0;
      x[n] = atoi(buffer);
      n++;
   }
   return n;
}




/*
 * Return the variable number for s where s is either a variable name
 * or the number itself.  Return -1 if s is an unknown variable name.
 */
static int varnum( int index, char *s )
{
   if (isdigit(s[0])) {
      /* s is a number */
      return atoi( s );
   }
   else {
      /* s is a string */
      int var, numvars;
      vis5d_get_ctx_numvars( index, &numvars );
      for (var=0;var<numvars;var++) {
         char name[20];
         vis5d_get_ctx_var_name( index, var, name );
         if (strcmp(name,s)==0) {
            return var;
         }
      }
      return -1;
   }
}



/*
 * Check that the number of arguments is acceptable.
 * Input:  intepr - the Tcl interpreter
 *         func - name of calling function
 *         argc - number of arguments received, INCLUDING FUNCTION NAME!
 *         min - min number of args expected
 *         max - max number of args expected (1000+ == infinity)
 * Return:  1 = ok, 0 = wrong number of args.
 */
static int arg_check( Tcl_Interp *interp, char *func,
                      int argc, int min, int max )
{
   argc--;  /* skip the function name */

   if (argc>=min && argc<=max) {
      /* OK */
      return 1;
   }
   else if (min==max && min==1) {
      sprintf( interp->result, "Error in %s: one argument expected", func );
      return 0;
   }
   else if (min==max) {
      sprintf( interp->result, "Error in %s: %d arguments expected",
               func, min );
      return 0;
   }
   else if (min==max-1) {
      sprintf( interp->result, "Error in %s: %d or %d arguments expected",
               func, min, max );
      return 0;
   }
   else if (max<1000) {
      sprintf( interp->result, "Error in %s: from %d to %d arguments expected",
               func, min, max );
      return 0;
   }
   else {
      sprintf( interp->result,"Error in %s: at least %d arguments expected",
               func, min );
      return 0;
   }
}


/*
 * If errno is non-zero put an error message into the intepreter's
 * result string and return TCL_ERROR, else return TCL_OK.
 */
static int error_check( Tcl_Interp *interp, char *func, int errno )
{
   switch (errno) {
      case VIS5D_BAD_CONTEXT:
         sprintf( interp->result, "Error in %s: bad context", func );
         break;
      case VIS5D_BAD_CONSTANT:
         sprintf( interp->result, "Error in %s: undefined constant", func );
         break;
      case VIS5D_BAD_MODE:
         sprintf( interp->result, "Error in %s: bad mode parameter", func );
         break;
      case VIS5D_BAD_VALUE:
         sprintf( interp->result, "Error in %s: bad parameter value", func );
         break;
      case VIS5D_BAD_VAR_NUMBER:
         sprintf( interp->result, "Error in %s: invalid variable number",func);
         break;
      case VIS5D_BAD_TIME_STEP:
         sprintf( interp->result, "Error in %s: invalid timestep number",func);
         break;
      case VIS5D_FAIL:
         sprintf( interp->result, "Error in %s: function failed", func );
         break;
      case VIS5D_OUT_OF_MEMORY:
         sprintf( interp->result, "Error in %s: out of memory", func );
         break;
      default:
         return TCL_OK;
   }
   return TCL_ERROR;
}

/* gui functions */
static int cmd_set_mouse_mode( ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_gui_set_mouse_mode", argc, 2, 2)){
      return TCL_ERROR;
   }
   result = set_mouse_mode( atoi(argv[1]), atoi(argv[2]) );
   return error_check( interp, "vis5d_gui_set_mouse_mode", result );
}

static int cmd_get_animate( ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int state, rate, dwell,result;
   if (!arg_check( interp, "vis5d_gui_get_animate", argc, 1, 1)){
      return TCL_ERROR;
   }
   result = get_animate( atoi(argv[1]), &state, &rate, &dwell);
   sprintf( interp->result, "%d %d %d", state, rate, dwell);
   return error_check( interp, "vis5d_gui_get_animate", result );
}

static int cmd_set_animate( ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_gui_set_animate", argc, 4, 4)){
      return TCL_ERROR;
   }
   result = set_animate( atoi(argv[1]), atoi(argv[2]),atoi(argv[3]),atoi(argv[4]));
   return error_check( interp, "vis5d_gui_set_animate", result );
}

static int cmd_set_reverse_background( ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_gui_set_reverse_background", argc, 2, 2)){
      return TCL_ERROR;
   }
   result = set_reverse_background( atoi(argv[1]), atoi(argv[2]));
   return error_check( interp, "vis5d_gui_set_reverse_background", result );
}

static int cmd_set_title(ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_gui_set_title", argc, 4, 4)){
      return TCL_ERROR;
   }
   title_x_position[number_of_titles] = atoi( argv[1] );
   title_y_position[number_of_titles] = atoi( argv[2] );
   strcpy(title_font[number_of_titles], argv[3]);
   strcpy(title_string[number_of_titles], argv[4]);
   number_of_titles++;
   result = 1;
   return error_check( interp, "vis5d_gui_set_title", result );
}

static int cmd_set_margins(ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_gui_set_margins", argc, 4, 4 )){
      return TCL_ERROR;
   }
   top_margin = atoi( argv[1] );
   bottom_margin = atoi( argv[2] );
   left_margin = atoi( argv[3] );
   right_margin = atoi( argv[4] );
   result = 1;
   return error_check( interp, "vis5d_gui_set_margins", result );
}





/*** Initialization Functions ***/


static int cmd_initialize( ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int result;
   if (argc==1) {
      result = vis5d_initialize( 0 );
   }
   else {
      result = vis5d_initialize( atoi( argv[1] ) );
   }
   return error_check( interp, "vis5d_initialize", result );
}


static int cmd_terminate( ClientData client_data, Tcl_Interp *interp,
                          int argc, char *argv[] )
{
   int result;
   result = vis5d_terminate( 1 );
   exit(0);
   return error_check( interp, "vis5d_terminate", result );
}


static int cmd_workers( ClientData client_data, Tcl_Interp *interp,
                        int argc, char *argv[] )
{
   if (!arg_check( interp, "vis5d_workers", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   else {
      vis5d_workers( atoi( argv[1] ) );
      return TCL_OK;
   }
}


static int cmd_do_work( ClientData client_data, Tcl_Interp *interp,
                        int argc, char *argv[] )
{
   if (!arg_check( interp, "vis5d_do_work", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   vis5d_do_work();
   return TCL_OK;
}




/*** Context Initialization Functions ***/

static int cmd_alloc_context(ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_alloc_context", argc, 0, 0)){
      return TCL_ERROR;
   }
   result = vis5d_alloc_data_context();
   result = vis5d_alloc_display_context();
   sprintf(interp->result,"%d",result);
   return error_check( interp, "vis5d_alloc_context", result);
}

static int cmd_destroy_context(ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int result;

   /* WLH 11 Nov 98 */
   int dindex;

   if (!arg_check( interp, "vis5d_destroy_context", argc, 2, 2)){
      return TCL_ERROR;
   }
   result = vis5d_destroy_data_context(atoi(argv[1]));
   redo_the_gui = 1;

   /* WLH 11 Nov 98 */
   if (vis5d_get_ctx_display_index(atoi(argv[1]), &dindex) > 0) {
     if (dindex >= 0) redo_this_gui[dindex] = 1;
   }

   return error_check( interp, "vis5d_destroy_context", result );
}

static int cmd_alloc_data_context(ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_alloc_data_context", argc, 0, 0)){
      return TCL_ERROR;
   }
   result = vis5d_alloc_data_context();
   sprintf(interp->result,"%d",result);
   return error_check( interp, "vis5d_alloc_data_context", result);
}

static int cmd_alloc_irregular_data_context(ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{                          
   int result;
   if (!arg_check( interp, "vis5d_alloc_irregular_data_context", argc, 0, 0)){
      return TCL_ERROR;
   }  
   result = vis5d_alloc_irregular_data_context();
   sprintf(interp->result,"%d",result);
   return error_check( interp, "vis5d_alloc_irregular_data_context", result);
}  

static int cmd_alloc_display_context(ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_alloc_display_context", argc, 0, 0)){
      return TCL_ERROR;
   }
   result = vis5d_alloc_display_context();
   sprintf(interp->result,"%d",result);
   return error_check( interp, "vis5d_alloc_display_context", result);
}

static int cmd_reset_display_context(ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_reset_display_context", argc, 1, 1)){
      return TCL_ERROR;
   }
   result = vis5d_reset_display_context(atoi(argv[1]));
   return error_check( interp, "vis5d_reset_display_context", result );
}

static int cmd_destroy_data_context(ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int result;

   /* WLH 11 Nov 98 */
   int dindex;

   if (!arg_check( interp, "vis5d_destroy_data_context", argc, 1, 1)){
      return TCL_ERROR;
   }
   result = vis5d_destroy_data_context(atoi(argv[1]));
   redo_the_gui = 1;

   /* WLH 11 Nov 98 */
   if (vis5d_get_ctx_display_index(atoi(argv[1]), &dindex) > 0) {
     if (dindex >= 0) redo_this_gui[dindex] = 1;
   }

   return error_check( interp, "vis5d_destroy_data_context", result ); 
}

static int cmd_destroy_display_context(ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_destroy_display_context", argc, 1,1)){
      return TCL_ERROR;
   }
   result = vis5d_destroy_display_context(atoi(argv[1]));
   return TCL_OK;
}

static int cmd_init_begin( ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_init_begin", argc, 1, 2 )) {
      return TCL_ERROR;
   }
   if (argc>2){
      result = vis5d_init_begin( atoi(argv[1]), atoi(argv[2]) );
   }
   else{
      result = vis5d_init_begin( atoi(argv[1]), atoi(argv[1]) );
   }
   return error_check( interp, "vis5d_init_begin", result );
}


static int cmd_init_display_values( ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_init_display_values", argc, 2, 2)){
      return TCL_ERROR;
   }
   result = vis5d_init_display_values( atoi(argv[1]), -1, atoi(argv[2]) );
   return error_check( interp, "vis5d_init_display_values", result );
}

static int cmd_init_data_end( ClientData client_data, Tcl_Interp *interp,
                         int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_init_data_end", argc, 1,1 )) {
      return TCL_ERROR;
   }
   result = vis5d_init_data_end( atoi(argv[1]) );
   return error_check( interp, "vis5d_init_data_end", result );
}


static int cmd_init_irregular_data_end( ClientData client_data, Tcl_Interp *interp,
                         int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_init_irregular_data_end", argc, 1,1 )) {
      return TCL_ERROR;
   }
   result = vis5d_init_data_end( atoi(argv[1]) );
   return error_check( interp, "vis5d_init_irregular_data_end", result );
}


static int cmd_get_v5dfilename( ClientData client_data, Tcl_Interp *interp,
                              int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_get_v5dfilename", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   result = vis5d_get_v5dfilename( atoi(argv[1]), interp->result );
   return error_check( interp, "vis5d_get_v5dfilename", result );
}



/* WLH 7 Oct 98 */
static int cmd_get_context_name( ClientData client_data, Tcl_Interp *interp,
                                int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_get_context_name", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   result = vis5d_get_context_name( atoi(argv[1]), interp->result );
   return error_check( interp, "vis5d_get_context_name", result );
}

static int cmd_open_gridfile( ClientData client_data, Tcl_Interp *interp,
                              int argc, char *argv[] )
{
   int result;

   /* WLH 11 Nov 98 */
   int dindex;

/* WLH 11 Nov 98
   redo_the_gui = 1;
*/

   if (!arg_check( interp, "vis5d_open_gridfile", argc, 3, 3 )) {
      return TCL_ERROR;
   }
   result = vis5d_open_gridfile( atoi(argv[1]), argv[2], atoi(argv[3]) );

   /* WLH 11 Nov 98 */
   redo_the_gui = 1;
   if (vis5d_get_ctx_display_index(atoi(argv[1]), &dindex) > 0) {
     if (dindex >= 0) redo_this_gui[dindex] = 1;
   }

   return error_check( interp, "vis5d_open_gridfile", result );
}


static int cmd_open_recordfile( ClientData client_data, Tcl_Interp *interp,
                              int argc, char *argv[] )
{
   int dindex, result;


   if (!arg_check( interp, "vis5d_open_gridfile", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   result = vis5d_open_recordfile( atoi(argv[1]), argv[2], argv[3], atoi(argv[4]) );

   /* WLH 11 Nov 98 */
   redo_the_gui = 1;
   if (vis5d_get_itx_display_index(atoi(argv[1]), &dindex) > 0) {
     if (dindex >= 0) redo_this_gui[dindex] = 1;
   }

   return error_check( interp, "vis5d_open_recordfile", result );
}


static int cmd_init_window( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_init_window", argc, 5,5 )) {
      return TCL_ERROR;
   }
   else {
      if (argc>6){
         char *title = argv[2];
         int x = atoi(argv[3]);
         int y = atoi(argv[4]);
         int width = atoi(argv[5]);
         int height = atoi(argv[6]);
         int result;
         result = vis5d_init_window( title, x, y, width, height );
      }
      else{
         char *title = argv[1];
         int x = atoi(argv[2]);
         int y = atoi(argv[3]);
         int width = atoi(argv[4]);
         int height = atoi(argv[5]);
         int result;
         result = vis5d_init_window( title, x, y, width, height );
      }
      return error_check( interp, "vis5d_init_window", result );
   }
}

static int cmd_init_sndwindow( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   if (!arg_check( interp, "vis5d_init_sndwindow", argc, 8, 8 )) {
      return TCL_ERROR;
   }
   else {
      int index = atoi(argv[1]);
      char *title = argv[2];
      int x = atoi(argv[3]);
      int y = atoi(argv[4]);
      int width = atoi(argv[5]);
      int height = atoi(argv[6]);
      Window scw = atoi(argv[7]);
      int result;
      char *dpyname = argv[8];

      result = vis5d_init_sndwindow( index, title, x, y, width, height, scw, dpyname );
      return error_check( interp, "vis5d_init_sndwindow", result );
   }
}
static int cmd_map_sndwindow( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_map_sndwindow", argc, 1, 1)){
      return TCL_ERROR;
   }
   else {
      if (argc>2){
         if (atoi(argv[2])==1){
            result = vis5d_map_sndwindow( atoi(argv[1]));
         }
         else{
            result = vis5d_unmap_sndwindow( atoi(argv[1]));
         }
      }
      else{
         int index = atoi(argv[1]);
         int result;
         result = vis5d_map_sndwindow( index);
      }
      return error_check( interp, "vis5d_map_sndwindow", result);
   }
}


static int cmd_unmap_sndwindow( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   if (!arg_check( interp, "vis5d_unmap_sndwindow", argc, 1, 1)){
      return TCL_ERROR;
   }
   else {
      int index = atoi(argv[1]);
      int show = atoi(argv[2]);
      int result;

      result = vis5d_unmap_sndwindow( index);
      return error_check( interp, "vis5d_unmap_sndwindow", result);
   }
}

static int cmd_init_map( ClientData client_data, Tcl_Interp *interp,
                          int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_init_map", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   result = vis5d_init_map( atoi(argv[1]), argv[2] );
   if (result==0) {
     result = vis5d_load_topo_and_map( atoi(argv[1]) );
   }
   return error_check( interp, "vis5d_init_map", result );
}


static int cmd_init_samescale( ClientData client_data, Tcl_Interp *interp,
                          int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_init_samescale", argc, 1,1 )) {
      return TCL_ERROR;
   }
   result = vis5d_init_samescale( atoi(argv[1]));
   return error_check( interp, "vis5d_init_samescale", result );
}

static int cmd_init_topo( ClientData client_data, Tcl_Interp *interp,
                          int argc, char *argv[] )
{
   int result;
   int highres = 0;
   if (!arg_check( interp, "vis5d_init_topo", argc, 2, 3 )) {
      return TCL_ERROR;
   }

   if (argc==4) {
      highres = atoi(argv[3]);
   }
   result = vis5d_init_topo( atoi(argv[1]), argv[2], highres );
   if (result==0) {
     result = vis5d_load_topo_and_map( atoi(argv[1]) );
   }
   return error_check( interp, "vis5d_init_topo", result );
}

/* MJK 12.04.98 begin */
static int cmd_enable_sfc_graphics (ClientData client_data, Tcl_Interp *interp,
                                    int argc, char *argv[] )
{
   int index, what, mode, n, result, varflag;

   varflag = 0;

   if (!arg_check( interp, "vis5d_enable_sfc_graphics", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   if (strcmp(argv[2],"VIS5D_HSLICE")==0) {
      what = VIS5D_HSLICE;
      varflag = 1;
   }
   else if (strcmp(argv[2],"VIS5D_HWIND")==0) {
      what = VIS5D_HWIND;
   }
   else if (strcmp(argv[2],"VIS5D_HSTREAM")==0) {
      what = VIS5D_HSTREAM;
   }
   else if (strcmp(argv[2],"VIS5D_MAP")==0) {
      what = VIS5D_MAP;
   }
   else {
      interp->result = "Error in vis5d_enable_sfc_graphics: bad constant";
      return TCL_ERROR;
   }

   if (strcmp(argv[4],"VIS5D_ON")==0) {
      mode = VIS5D_ON;
   }
   else if (strcmp(argv[4],"VIS5D_OFF")==0) {
      mode = VIS5D_OFF;
   }
   else if (strcmp(argv[4],"VIS5D_TOGGLE")==0) {
      mode = VIS5D_TOGGLE;
   }
   else if (strcmp(argv[4],"VIS5D_GET")==0) {
      mode = VIS5D_GET;
   }
   else {
      interp->result = "Error in vis5d_enable_sfc_graphics: bad mode";
      return TCL_ERROR;
   }

   index = atoi(argv[1]);
   if (varflag) {
      n = varnum( index, argv[3] );
   }
   else {
      n = atoi( argv[3] );
   }

   result = vis5d_enable_sfc_graphics( index, what, n, mode );
   if (result >= 0)
   {
      interp->result = result ? "1" : "0";
      return TCL_OK;
   }
   return error_check( interp, "vis5d_enable_sfc_graphics", result );
}
/* MJK 12.04.98 end */

/* MJK 12.02.98 begin */
static int cmd_init_clock(ClientData client_data, Tcl_Interp *interp,
                             int argc, char *argv[] )
{
   int result;

   if (!arg_check( interp, "vis5d_init_clock", argc, 2, 2)){
      return TCL_ERROR;
   }
   result = vis5d_init_clock(atoi(argv[1]), atoi(argv[2]));
   return error_check( interp, "vis5d_init_clock", result );
}
/* MJK 12.02.98 end */


/* MJK 12.02.98 begin */
static int cmd_set_probe_vars( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int  result, numvars;
   int  i, index, *vars = NULL;

   if (!arg_check( interp, "vis5d_set_probe_vars", argc, 2, (MAXVARS+2) )) {
      return TCL_ERROR;
   }

   index = atoi(argv[1]);

   argc -= 3;
   numvars = atoi (argv[2]);
   if (numvars > argc) numvars = argc ;
   if (numvars >    0)
   {
       vars = (int *) malloc (numvars * sizeof (int));

       for (i = 0; i < numvars; i++)
       {
           vars[i] = varnum (index, argv[i+3]);
       }
   }

   result = vis5d_set_probe_vars( index, numvars, vars);

   if (vars != NULL) free (vars);

   return error_check( interp, "vis5d_set_probe_vars", result );
}
/* MJK 12.02.98 end */


static int cmd_init_texture( ClientData client_data, Tcl_Interp *interp,
                             int argc, char *argv[] )
{
   int result;

   if (!arg_check( interp, "vis5d_init_texture", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   result = vis5d_init_texture( atoi(argv[1]), argv[2] );
   if (result==0) {
     result = vis5d_load_topo_and_map( atoi(argv[1]) );
     redo_the_gui = 1;

     /* WLH 11 Nov 98 */
     if (atoi(argv[1]) >= 0) redo_this_gui[atoi(argv[1])] = 1;

   }

   return error_check( interp, "vis5d_init_texture", result );
}


static int cmd_init_firstarea( ClientData client_data, Tcl_Interp *interp,
                             int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_init_firstarea", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   result = vis5d_init_firstarea( atoi(argv[1]), atoi(argv[2]) );
   if (result==0) {
     result = vis5d_load_topo_and_map( atoi(argv[1]) );
     redo_the_gui = 1;

     /* WLH 11 Nov 98 */
     if (atoi(argv[1]) >= 0) redo_this_gui[atoi(argv[1])] = 1;

   }

   return error_check( interp, "vis5d_init_firstarea", result );
}


static int cmd_init_sequence( ClientData client_data, Tcl_Interp *interp,
                             int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_init_sequence", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   result = vis5d_init_sequence( atoi(argv[1]), argv[2] );
   if (result==0) {
     result = vis5d_load_topo_and_map( atoi(argv[1]) );
     redo_the_gui = 1;

     /* WLH 11 Nov 98 */
     if (atoi(argv[1]) >= 0) redo_this_gui[atoi(argv[1])] = 1;

   }

   return error_check( interp, "vis5d_init_sequence", result );
}


static int cmd_init_log( ClientData client_data, Tcl_Interp *interp,
                             int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_init_log", argc, 3, 3  )) {
      return TCL_ERROR;
   }
   result = vis5d_init_log( atoi(argv[1]), 1, atof(argv[2]), atof(argv[3]) ); 
   return error_check( interp, "vis5d_init_log", result );
}


static int cmd_init_box( ClientData client_data, Tcl_Interp *interp,
                         int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_init_box", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   result = vis5d_init_box( atoi(argv[1]), atof(argv[2]),
                            atof(argv[3]), atof(argv[4]) );
   return error_check( interp, "vis5d_init_box", result );
}


static int cmd_init_memory( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_init_memory", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   result = vis5d_init_memory( atoi(argv[1]), atoi(argv[2]) );
   return error_check( interp, "vis5d_init_memory", result );
}


static int cmd_init_irregular_memory( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_init_irregular_memory", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   result = vis5d_init_irregular_memory( atoi(argv[1]), atoi(argv[2]) );
   return error_check( interp, "vis5d_init_irregular_memory", result );
}


/* MJK 4.27.99
static int cmd_init_path( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_init_path", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   result = vis5d_init_path( atoi(argv[1]), argv[2] );
   return error_check( interp, "vis5d_init_path", result );
}
*/

static int cmd_init_path( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_init_path", argc, 1, 2 )) {
      return TCL_ERROR;
   }
   if (argc>2){
      result = vis5d_init_path( argv[2] );
   }
   else{
      result = vis5d_init_path( argv[1] );
   }
   return error_check( interp, "vis5d_init_path", result );
}


static int cmd_init_projection( ClientData client_data, Tcl_Interp *interp,
                                int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_init_projection", argc, 2, 1000 )) {
      return TCL_ERROR;
   }
   if (argc>3) {
      float projargs[MAXPROJARGS];
      int i;
      for (i=3;i<argc;i++) {
         projargs[i-3] = atof( argv[i] );
      }
      result = vis5d_init_projection( atoi(argv[1]), atoi(argv[2]), projargs );
   }
   else {
      result = vis5d_init_projection( atoi(argv[1]), atoi(argv[2]), NULL );
   }
   return error_check( interp, "vis5d_init_projection", result );
}


static int cmd_init_vertical( ClientData client_data, Tcl_Interp *interp,
                                int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_init_vertical", argc, 2, 1000 )) {
      return TCL_ERROR;
   }
   if (argc>3) {
      float vertargs[MAXPROJARGS];
      int i;
      for (i=3;i<argc;i++) {
         vertargs[i-3] = atof( argv[i] );
      }
      result = vis5d_init_vertical( atoi(argv[1]), atoi(argv[2]), vertargs );
   }
   else {
      result = vis5d_init_vertical( atoi(argv[1]), atoi(argv[2]), NULL );
   }
   return error_check( interp, "vis5d_init_vertical", result );
}


/*
 * Post-initialization functions
 */

/*** Time Functions ***/

static int cmd_get_numtimes( ClientData client_data, Tcl_Interp *interp,
                             int argc, char *argv[] )
{
   int numtimes, result;
   if (!arg_check( interp, "vis5d_get_numtimes", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   result = vis5d_get_ctx_numtimes( atoi(argv[1]), &numtimes );
   sprintf( interp->result, "%d", numtimes );
   return error_check( interp, "vis5d_get_numtimes", result );
}

static int cmd_get_ctx_numtimes( ClientData client_data, Tcl_Interp *interp,
                             int argc, char *argv[] )
{
   int numtimes, result;
   if (!arg_check( interp, "vis5d_get_ctx_numtimes", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   result = vis5d_get_ctx_numtimes( atoi(argv[1]), &numtimes );
   sprintf( interp->result, "%d", numtimes );
   return error_check( interp, "vis5d_get_ctx_numtimes", result );
}

static int cmd_get_itx_numtimes( ClientData client_data, Tcl_Interp *interp,
                             int argc, char *argv[] )
{
   int numtimes, result;
   if (!arg_check( interp, "vis5d_get_itx_numtimes", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   result = vis5d_get_itx_numtimes( atoi(argv[1]), &numtimes );
   sprintf( interp->result, "%d", numtimes );
   return error_check( interp, "vis5d_get_itx_numtimes", result );
}

static int cmd_get_dtx_numtimes( ClientData client_data, Tcl_Interp *interp,
                             int argc, char *argv[] )
{
   int numtimes, result;
   if (!arg_check( interp, "vis5d_get_dtx_numtimes", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   result = vis5d_get_dtx_numtimes( atoi(argv[1]), &numtimes );
   sprintf( interp->result, "%d", numtimes );
   return error_check( interp, "vis5d_get_dtx_numtimes", result );
}

static int cmd_get_grp_numtimes( ClientData client_data, Tcl_Interp *interp,
                             int argc, char *argv[] )
{
   int numtimes, result;
   if (!arg_check( interp, "vis5d_get_grp_numtimes", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   result = vis5d_get_grp_numtimes( atoi(argv[1]), &numtimes );
   sprintf( interp->result, "%d", numtimes );
   return error_check( interp, "vis5d_get_grp_numtimes", result );
}


static int cmd_get_time_stamp( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int date, time, result;
   if (!arg_check( interp, "vis5d_get_time_stamp", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   date = v5dDaysToYYDDD( date );
   time = v5dSecondsToHHMMSS( time );
   result = vis5d_get_ctx_time_stamp( atoi(argv[1]), atoi(argv[2]), &date, &time );
   sprintf( interp->result, "%05d %06d", date, time );
   return error_check( interp, "vis5d_get_time_stamp", result );
}

static int cmd_get_ctx_time_stamp( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int date, time, result;
   if (!arg_check( interp, "vis5d_get_ctx_time_stamp", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   date = v5dDaysToYYDDD( date );
   time = v5dSecondsToHHMMSS( time );
   result = vis5d_get_ctx_time_stamp( atoi(argv[1]), atoi(argv[2]), &date, &time );
   sprintf( interp->result, "%05d %06d", date, time );
   return error_check( interp, "vis5d_get_ctx_time_stamp", result );
}

static int cmd_get_itx_time_stamp( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int date, time, result;
   if (!arg_check( interp, "vis5d_get_itx_time_stamp", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   date = v5dDaysToYYDDD( date );
   time = v5dSecondsToHHMMSS( time );
   result = vis5d_get_itx_time_stamp( atoi(argv[1]), atoi(argv[2]), &date, &time );
   sprintf( interp->result, "%05d %06d", date, time );
   return error_check( interp, "vis5d_get_itx_time_stamp", result );
}

static int cmd_set_time_stamp( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int date, time, result;
   if (!arg_check( interp, "vis5d_set_time_stamp", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   date = v5dDaysToYYDDD(atoi(argv[3]));
   time = v5dSecondsToHHMMSS(atoi(argv[4]));
   result = vis5d_set_ctx_time_stamp( atoi(argv[1]), atoi(argv[2]),
                                  date, time );
   return error_check( interp, "vis5d_set_time_stamp", result );
}

static int cmd_get_dtx_time_stamp( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int date, time, result;
   if (!arg_check( interp, "vis5d_get_dtx_time_stamp", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   date = v5dDaysToYYDDD( date );
   time = v5dSecondsToHHMMSS( time );
   result = vis5d_get_dtx_time_stamp( atoi(argv[1]), atoi(argv[2]), &date, &time );
   sprintf( interp->result, "%05d %06d", date, time );
   return error_check( interp, "vis5d_get_dtx_time_stamp", result );
}


static int cmd_set_ctx_time_stamp( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int date, time, result;
   if (!arg_check( interp, "vis5d_set_ctx_time_stamp", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   date = v5dDaysToYYDDD(atoi(argv[3]));
   time = v5dSecondsToHHMMSS(atoi(argv[4]));
   result = vis5d_set_ctx_time_stamp( atoi(argv[1]), atoi(argv[2]),
                                  date, time );
   return error_check( interp, "vis5d_set_ctx_time_stamp", result );
}


static int cmd_set_timestep( ClientData client_data, Tcl_Interp *interp,
                             int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_set_timestep", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   result = vis5d_set_dtx_timestep( atoi(argv[1]), atoi(argv[2]) );
   return error_check( interp, "vis5d_set_timestep", result );
}

static int cmd_set_dtx_timestep( ClientData client_data, Tcl_Interp *interp,
                             int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_set_dtx_timestep", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   result = vis5d_set_dtx_timestep( atoi(argv[1]), atoi(argv[2]) );
   return error_check( interp, "vis5d_set_dtx_timestep", result );
}


static int cmd_set_grp_timestep( ClientData client_data, Tcl_Interp *interp,
                             int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_set_grp_timestep", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   result = vis5d_set_grp_timestep( atoi(argv[1]), atoi(argv[2]) );
   return error_check( interp, "vis5d_set_grp_timestep", result );
}

static int cmd_get_ctx_timestep( ClientData client_data, Tcl_Interp *interp,
                             int argc, char *argv[] )
{
   int time, result;
   if (!arg_check( interp, "vis5d_get_ctx_timestep", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   result = vis5d_get_ctx_timestep( atoi(argv[1]), &time );
   sprintf( interp->result, "%d", time );
   return error_check( interp, "vis5d_get_ctx_timestep", result );
}

static int cmd_get_itx_timestep( ClientData client_data, Tcl_Interp *interp,
                             int argc, char *argv[] )
{
   int time, result;
   if (!arg_check( interp, "vis5d_get_itx_timestep", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   result = vis5d_get_itx_timestep( atoi(argv[1]), &time );
   sprintf( interp->result, "%d", time );
   return error_check( interp, "vis5d_get_itx_timestep", result );
}

static int cmd_get_timestep( ClientData client_data, Tcl_Interp *interp,
                             int argc, char *argv[] )
{
   int time, result;
   if (!arg_check( interp, "vis5d_get_timestep", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   result = vis5d_get_dtx_timestep( atoi(argv[1]), &time );
   sprintf( interp->result, "%d", time );
   return error_check( interp, "vis5d_get_timestep", result );
}

static int cmd_get_dtx_timestep( ClientData client_data, Tcl_Interp *interp,
                             int argc, char *argv[] )
{
   int time, result;
   if (!arg_check( interp, "vis5d_get_dtx_timestep", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   result = vis5d_get_dtx_timestep( atoi(argv[1]), &time );
   sprintf( interp->result, "%d", time );
   return error_check( interp, "vis5d_get_dtx_timestep", result );
}


static int cmd_get_grp_timestep( ClientData client_data, Tcl_Interp *interp,
                             int argc, char *argv[] )
{
   int time, result;
   if (!arg_check( interp, "vis5d_get_grp_timestep", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   result = vis5d_get_grp_timestep( atoi(argv[1]), &time );
   sprintf( interp->result, "%d", time );
   return error_check( interp, "vis5d_get_grp_timestep", result );
}


/*** Variable functions ***/

static int cmd_get_ctx_numvars( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int numvars, result;
   if (!arg_check( interp, "vis5d_get_ctx_numvars", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   result = vis5d_get_ctx_numvars( atoi(argv[1]), &numvars );
   if (result==0) {
      sprintf( interp->result, "%d", numvars );
      return TCL_OK;
   }
   else {
      return error_check( interp, "vis5d_get_ctx_numvars", result );
   }
}

static int cmd_get_itx_numvars( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int numvars, result;
   if (!arg_check( interp, "vis5d_get_itx_numvars", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   result = vis5d_get_itx_numvars( atoi(argv[1]), &numvars );
   if (result==0) {
      sprintf( interp->result, "%d", numvars );
      return TCL_OK;
   }
   else {
      return error_check( interp, "vis5d_get_itx_numvars", result );
   }
}


static int cmd_get_ctx_var_name( ClientData client_data, Tcl_Interp *interp,
                             int argc, char *argv[] )
{
   int result, index, var;
   if (!arg_check( interp, "vis5d_get_ctx_var_name", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   var = varnum(index,argv[2]);
   result = vis5d_get_ctx_var_name( index, var, interp->result );
   return error_check( interp, "vis5d_get_ctx_var_name", result );
}

static int cmd_get_itx_var_name( ClientData client_data, Tcl_Interp *interp,
                             int argc, char *argv[] )
{
   int result, index, var;
   if (!arg_check( interp, "vis5d_get_itx_var_name", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   var = varnum(index,argv[2]);
   result = vis5d_get_itx_var_name( index, var, interp->result );
   return error_check( interp, "vis5d_get_itx_var_name", result );
}


static int cmd_get_var_units( ClientData client_data, Tcl_Interp *interp,
                              int argc, char *argv[] )
{
   int result, index, var;
   if (!arg_check( interp, "vis5d_get_var_units", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   var = varnum(index,argv[2]);
   result = vis5d_get_var_units( index, var, interp->result );
   return error_check( interp, "vis5d_get_var_units", result );
}


static int cmd_get_var_type( ClientData client_data, Tcl_Interp *interp,
                             int argc, char *argv[] )
{
   int type, result, index, var;
   if (!arg_check( interp, "vis5d_get_var_type", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   var = varnum( index, argv[2] );
   result = vis5d_get_var_type( index, var, &type );
   if (result==0) {
      if (type==VIS5D_REGULAR) {
         interp->result = "VIS5D_REGULAR";
      }
      else if (type==VIS5D_CLONE) {
         interp->result = "VIS5D_CLONE";
      }
      else if (type==VIS5D_EXT_FUNC) {
         interp->result = "VIS5D_EXT_FUNC";
      }
      else if (type==VIS5D_EXPRESSION) {
         interp->result = "VIS5D_EXPRESSION";
      }
      return TCL_OK;
   }
   else {
      return error_check( interp, "vis5d_get_var_type", result );
   }
}



static int cmd_get_ctx_var_range( ClientData client_data, Tcl_Interp *interp,
                              int argc, char *argv[] )
{
   float min, max;
   int result, var, index;
   if (!arg_check( interp, "vis5d_get_ctx_var_range", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   var = varnum(index, argv[2]);
   result = vis5d_get_ctx_var_range( index, var, &min, &max );
   if (result==0) {
      sprintf( interp->result, "%g %g", min, max );
      return TCL_OK;
   }
   else {
      return error_check( interp, "vis5d_get_ctx_var_range", result );
   }
}

static int cmd_get_itx_var_range( ClientData client_data, Tcl_Interp *interp,
                              int argc, char *argv[] )
{
   float min, max;
   int result, var, index;
   if (!arg_check( interp, "vis5d_get_itx_var_range", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   var = varnum(index, argv[2]);
   result = vis5d_get_itx_var_range( index, var, &min, &max );
   if (result==0) {
      sprintf( interp->result, "%g %g", min, max );
      return TCL_OK;
   }
   else {
      return error_check( interp, "vis5d_get_itx_var_range", result );
   }
}

static int cmd_set_view_scales( ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_set_view_scales", argc, 4, 4)){
   return TCL_ERROR;
   }
   result = vis5d_set_view_scales(atoi(argv[1]),atoi(argv[2]),atoi(argv[3]),atoi(argv[4]));
   return error_check( interp, "vis5d_set_view_scales", result );
}


static int cmd_set_var_range( ClientData client_data, Tcl_Interp *interp,
                              int argc, char *argv[] )
{
   float min, max;
   int result, var, index;
   if (!arg_check( interp, "vis5d_set_var_range", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   var = varnum(index, argv[2]);
   result = vis5d_set_var_range( index, var, atof(argv[3]), atof(argv[4]) );
   return error_check( interp, "vis5d_set_var_range", result );
}

static int cmd_set_grp_var_values( ClientData client_data, Tcl_Interp *interp,
                              int argc, char *argv[] )
{
   float min, max;
   int result, var, index;
   if (!arg_check( interp, "vis5d_set_grp_var_values", argc, 1, 1)){
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   result = vis5d_set_grp_var_values( index );
   return error_check( interp, "vis5d_set_grp_var_values", result );
}

static int cmd_set_display_group(ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_set_display_group", argc, 2, 2)){
      return TCL_ERROR;
   }
   result = vis5d_set_display_group( atoi(argv[1]), atoi(argv[2]));
   return error_check( interp, "vis5d_set_display_group", result);
}
 
static int cmd_get_display_group(ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int result, dindex, gindex;
   if (!arg_check( interp, "vis5d_get_display_group", argc, 1, 1)){
      return TCL_ERROR;
   }
   result = vis5d_get_display_group( atoi(argv[1]) , &gindex);
   sprintf( interp->result, "%d", gindex);
   return error_check( interp, "vis5d_get_display_group", 0);
}

static int cmd_load_v5dfile(ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_load_v5dfile", argc, 4, 4)){
      return TCL_ERROR;
   }
   result = vis5d_load_v5dfile( atoi(argv[1]), atoi(argv[2]), argv[3], argv[4]);
   if (result != VIS5D_FAIL){
      sprintf( interp->result, "%d", result);
   }
   redo_the_gui = 1;

   /* WLH 11 Nov 98 */
   if (atoi(argv[1]) >= 0) redo_this_gui[atoi(argv[1])] = 1;

   return error_check( interp, "vis5d_load_v5dfile", result);
}

static int cmd_load_irregular_v5dfile(ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_load_irregular_v5dfile", argc, 4, 4)){
      return TCL_ERROR;
   }
   result = vis5d_load_irregular_v5dfile( atoi(argv[1]), atoi(argv[2]), argv[3], argv[4]);
   if (result != VIS5D_FAIL){
      sprintf( interp->result, "%d", result);
   } 
   redo_the_gui = 1;

   /* WLH 11 Nov 98 */
   if (atoi(argv[1]) >= 0) redo_this_gui[atoi(argv[1])] = 1;

   return error_check( interp, "vis5d_load_irregular_v5dfile", result);
}

static int cmd_save_to_v5dfile( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_save_to_v5dfile", argc, 2, 2)){
      return TCL_ERROR;
   }
   result = vis5d_save_to_v5dfile(atoi(argv[1]), argv[2]);
   return error_check( interp, "vis5d_save_to_v5dfile", result );
}

static int cmd_set_user_data_flag( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_set_user_data_flag", argc, 2, 2)){
      return TCL_ERROR;
   }
   result = vis5d_set_user_data_flag( atoi(argv[1]), atoi(argv[2]));
   return error_check( interp, "vis5d_set_user_data_flag", result );
}

static int cmd_set_user_flags( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_set_user_flags", argc, 3, 3)){
      return TCL_ERROR;
   }
   result = vis5d_set_user_flags( atoi(argv[1]), atoi(argv[2]), atoi(argv[3]));
   return error_check( interp, "vis5d_set_user_flags", result );
}




static int cmd_assign_display_to_data(ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_assign_display_to_data", argc, 2, 2)){
      return TCL_ERROR;
   }
   result = vis5d_assign_display_to_data(atoi(argv[1]), atoi(argv[2]));
   return error_check( interp, "vis5d_assign_display_to_data", result);
}


static int cmd_assign_display_to_irregular_data(ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_assign_display_to_irregular_data", argc, 2, 2)){
      return TCL_ERROR;
   }
   result = vis5d_assign_display_to_irregular_data(atoi(argv[1]), atoi(argv[2]));
   return error_check( interp, "vis5d_assign_display_to_irregular_data", result);
}


static int cmd_set_legends( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int result;
   int what;

   if (!arg_check( interp, "vis5d_set_legends", argc, 3, 5)){
      return TCL_ERROR;
   }
   if (strcmp(argv[2], "VIS5D_TOP")==0){
      what = VIS5D_TOP;
   }
   else if (strcmp(argv[2], "VIS5D_BOTTOM")==0){
      what = VIS5D_BOTTOM;
   }
   else if (strcmp(argv[2], "VIS5D_RIGHT")==0){
      what = VIS5D_RIGHT;
   }
   else if (strcmp(argv[2], "VIS5D_LEFT")==0){
      what = VIS5D_LEFT;
   }
   else{
      what = atoi(argv[2]);
   }
   if (argv[4]){
      if (argv[5]){
         result = vis5d_set_legends(atoi(argv[1]), what, atoi(argv[3]), atoi(argv[4]),
                                    atoi(argv[5]));
      }
      else{
         result = vis5d_set_legends(atoi(argv[1]), what, atoi(argv[3]), atoi(argv[4]),0);
      }
   }
   else{
      result = vis5d_set_legends(atoi(argv[1]), what, atoi(argv[3]), 0, 0);
   }
   return error_check( interp, "vis5d_set_legends", result);
}

static int cmd_get_sound_vars( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int temp, dewpt, uwind,
       vwind, var1, var2, var3;
   int tempowner, dewptowner, uwindowner,
       vwindowner, var1owner, var2owner, var3owner;
   int result;

   if (!arg_check( interp, "vis5d_get_sound_vars", argc, 1, 1)) {
      return TCL_ERROR;
   }
   result = vis5d_get_sound_vars( atoi(argv[1]), &tempowner, &temp,
                                  &dewptowner, &dewpt, &uwindowner, &uwind,
                                  &vwindowner, &vwind, &var1owner, &var1,
                                  &var2owner,  &var2, &var3owner, &var3); 
   sprintf( interp->result, " %d %d %d %d %d %d %d",
            temp,  dewpt, uwind, 
             vwind, var1,  var2, var3 );
   return error_check( interp, "vis5d_get_sound_vars", result );
}

static int cmd_get_sound_vars_and_owners( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int temp, dewpt, uwind,
       vwind, var1, var2, var3;
   int tempowner, dewptowner, uwindowner,
       vwindowner, var1owner, var2owner, var3owner;
   int result;

   if (!arg_check( interp, "vis5d_get_sound_vars_and_owners", argc, 1, 1)) {
      return TCL_ERROR;
   }
   result = vis5d_get_sound_vars( atoi(argv[1]), &tempowner, &temp,
                                  &dewptowner, &dewpt, &uwindowner, &uwind,
                                  &vwindowner, &vwind, &var1owner, &var1,
                                  &var2owner,  &var2, &var3owner, &var3);
   sprintf( interp->result, "%d %d %d %d %d %d %d %d %d %d %d %d %d %d",
            tempowner, temp, dewptowner, dewpt, uwindowner, uwind,
            vwindowner, vwind, var1owner, var1, var2owner, var2, var3owner, var3 );
   return error_check( interp, "vis5d_get_sound_vars_and_owners", result );
}

static int cmd_set_sound_vars( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int temp, dewpt, uwind,
       vwind, var1, var2, var3;
   int tempowner, dewptowner, uwindowner,
       vwindowner, var1owner, var2owner, var3owner;
   int result;


   if (!arg_check( interp, "vis5d_set_sound_vars", argc, 8,8)) {
      return TCL_ERROR;
   }
   result = vis5d_set_sound_vars( atoi(argv[1]), atoi(argv[1]), atoi(argv[2]),
                                  atoi(argv[1]), atoi(argv[3]), atoi(argv[1]),
                                  atoi(argv[4]), atoi(argv[1]), atoi(argv[5]),
                                  atoi(argv[1]), atoi(argv[6]), atoi(argv[1]),
                                  atoi(argv[7]), atoi(argv[1]), atoi(argv[8]));
   return error_check( interp, "vis5d_set_sound_vars", result );
}

static int cmd_set_sound_vars_and_owners( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int temp, dewpt, uwind,
       vwind, var1, var2, var3;
   int tempowner, dewptowner, uwindowner,
       vwindowner, var1owner, var2owner, var3owner;
   int result;

      
   if (!arg_check( interp, "vis5d_set_sound_vars_and_owners", argc, 15, 15)) {
      return TCL_ERROR;
   } 
   result = vis5d_set_sound_vars( atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
                                  atoi(argv[4]), atoi(argv[5]), atoi(argv[6]),
                                  atoi(argv[7]), atoi(argv[8]), atoi(argv[9]),
                                  atoi(argv[10]), atoi(argv[11]), atoi(argv[12]),
                                  atoi(argv[13]), atoi(argv[14]), atoi(argv[15]));
   return error_check( interp, "vis5d_set_sound_vars_and_owners", result );
}

static int cmd_get_wind_vars( ClientData client_data, Tcl_Interp *interp,
                              int argc, char *argv[] )
{
   int uvarowner, vvarowner, wvarowner;
   int u2varowner, v2varowner, w2varowner;
   int trajuowner, trajvowner, trajwowner;
   int uvar, vvar, wvar;
   int u2var, v2var, w2var;
   int traju, trajv, trajw;
   int result;
   if (!arg_check( interp, "vis5d_get_wind_vars", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   result = vis5d_get_wind_vars( atoi(argv[1]), &uvarowner, &uvar,
                        &vvarowner,  &vvar, &wvarowner, &wvar,
                        &u2varowner, &u2var, &v2varowner, &v2var,
                        &w2varowner,  &w2var, &trajuowner,
                        &traju, &trajvowner, &trajv, &trajwowner, &trajw );
   sprintf( interp->result, "%d %d %d %d %d %d %d %d %d",
            uvar, vvar, wvar,u2var,
            v2var, w2var, traju, 
            trajv, trajw);
   return error_check( interp, "vis5d_get_wind_vars", result ); 
}

static int cmd_get_wind_vars_and_owners( ClientData client_data, Tcl_Interp *interp,
                              int argc, char *argv[] )
{
   int uvarowner, vvarowner, wvarowner;
   int u2varowner, v2varowner, w2varowner;
   int trajuowner, trajvowner, trajwowner;
   int uvar, vvar, wvar;
   int u2var, v2var, w2var;
   int traju, trajv, trajw;
   int result;
   if (!arg_check( interp, "vis5d_get_wind_vars_and_owners", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   result = vis5d_get_wind_vars( atoi(argv[1]), &uvarowner, &uvar,
                        &vvarowner,  &vvar, &wvarowner, &wvar,
                        &u2varowner, &u2var, &v2varowner, &v2var,
                        &w2varowner,  &w2var, &trajuowner,
                        &traju, &trajvowner, &trajv, &trajwowner, &trajw );
   sprintf( interp->result, "%d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d",
            uvarowner, uvar, vvarowner, vvar, wvarowner, wvar, u2varowner,u2var,
            v2varowner, v2var, w2varowner, w2var, trajuowner, traju,
            trajvowner, trajv, trajwowner, trajw);
   return error_check( interp, "vis5d_get_wind_vars_and_owners", result );
}

static int cmd_set_wind_vars( ClientData client_data, Tcl_Interp *interp,
                              int argc, char *argv[] )
{
   int uvarowner, vvarowner, wvarowner;
   int u2varowner, v2varowner, w2varowner;
   int trajuowner, trajvowner, trajwowner;
   int uvar, vvar, wvar;
   int u2var, v2var, w2var;
   int traju, trajv, trajw;
   int result;
   if (!arg_check( interp, "vis5d_set_wind_vars", argc, 10, 10 )) {
      return TCL_ERROR;
   }
   result = vis5d_set_wind_vars( atoi(argv[1]),atoi(argv[1]),atoi(argv[2]),
                                 atoi(argv[1]),atoi(argv[3]),atoi(argv[1]),
                                 atoi(argv[4]),atoi(argv[1]),atoi(argv[5]),
                                 atoi(argv[1]),atoi(argv[6]),
                                 atoi(argv[1]),atoi(argv[7]), atoi(argv[1]),
                                 atoi(argv[8]),atoi(argv[1]), atoi(argv[9]),
                                 atoi(argv[1]),atoi(argv[10]));
   return error_check( interp, "vis5d_set_wind_vars", result );
}

static int cmd_set_wind_vars_and_owners( ClientData client_data, Tcl_Interp *interp,
                              int argc, char *argv[] )
{
   int uvarowner, vvarowner, wvarowner;
   int u2varowner, v2varowner, w2varowner;
   int trajuowner, trajvowner, trajwowner;
   int uvar, vvar, wvar;
   int u2var, v2var, w2var;
   int traju, trajv, trajw;
   int result;
   if (!arg_check( interp, "vis5d_set_wind_vars_and_owners", argc, 19, 19 )) {
      return TCL_ERROR;
   }
   result = vis5d_set_wind_vars( atoi(argv[1]),atoi(argv[2]),atoi(argv[3]),
                                 atoi(argv[4]),atoi(argv[5]),atoi(argv[6]),
                                 atoi(argv[7]),atoi(argv[8]),atoi(argv[9]),
                                 atoi(argv[10]), atoi(argv[11]),
                                 atoi(argv[12]), atoi(argv[13]), atoi(argv[14]),
                                 atoi(argv[15]), atoi(argv[16]), atoi(argv[17]),
                                 atoi(argv[18]), atoi(argv[19]));
   return error_check( interp, "vis5d_set_wind_vars_and_owners", result ); 
}







/*** Grid functions ***/

/* MJK 6.9.99 */
static int cmd_get_grid_value( ClientData client_data, Tcl_Interp *interp,
                              int argc, char *argv[] )
{
   int result;
   float value;

   if (!arg_check( interp, "vis5d_get_grid_value", argc, 5, 5)){
      return TCL_ERROR;
   }
   result = vis5d_get_grid_value(atoi(argv[1]), atoi(argv[2]),
            atof(argv[3]), atof(argv[4]),atof(argv[5]), &value);
   sprintf( interp->result, "%f", value);
   return error_check( interp, "vis5d_get_grid_value", result );
}

static int cmd_get_grid_rows( ClientData client_data, Tcl_Interp *interp,
                              int argc, char *argv[] )
{
   int result, nr;
   if (!arg_check( interp, "vis5d_get_grid_rows", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   result = vis5d_get_size( atoi(argv[1]), &nr, NULL, NULL, NULL,
                            NULL, NULL, NULL, NULL );
   sprintf( interp->result, "%d", nr );
   return error_check( interp, "vis5d_get_grid_rows", result );
}

static int cmd_get_ctx_grid_rows( ClientData client_data, Tcl_Interp *interp,
                              int argc, char *argv[] )
{
   int result, nr;
   if (!arg_check( interp, "vis5d_get_ctx_grid_rows", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   result = vis5d_get_size( atoi(argv[1]), &nr, NULL, NULL, NULL,
                            NULL, NULL, NULL, NULL );
   sprintf( interp->result, "%d", nr );
   return error_check( interp, "vis5d_get_ctx_grid_rows", result );
}

static int cmd_get_dtx_grid_rows( ClientData client_data, Tcl_Interp *interp,
                              int argc, char *argv[] )
{
   int result, nr;
   if (!arg_check( interp, "vis5d_get_dtx_grid_rows", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   result = vis5d_get_sizePRIME( atoi(argv[1]), &nr, NULL, NULL, NULL,
                             NULL, NULL );
   sprintf( interp->result, "%d", nr );
   return error_check( interp, "vis5d_get_dtx_grid_rows", result );
}


static int cmd_get_grid_columns( ClientData client_data, Tcl_Interp *interp,
                                 int argc, char *argv[] )
{
   int result, nc;
   if (!arg_check( interp, "vis5d_get_grid_columns", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   result = vis5d_get_size( atoi(argv[1]), NULL, &nc, NULL, NULL,
                            NULL, NULL, NULL, NULL );
   sprintf( interp->result, "%d", nc );
   return error_check( interp, "vis5d_get_grid_columns", result );
}

static int cmd_get_ctx_grid_columns( ClientData client_data, Tcl_Interp *interp,
                                 int argc, char *argv[] )
{
   int result, nc;
   if (!arg_check( interp, "vis5d_get_ctx_grid_columns", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   result = vis5d_get_size( atoi(argv[1]), NULL, &nc, NULL, NULL,
                            NULL, NULL, NULL, NULL );
   sprintf( interp->result, "%d", nc );
   return error_check( interp, "vis5d_get_ctx_grid_columns", result );
}

static int cmd_get_dtx_grid_columns( ClientData client_data, Tcl_Interp *interp,
                                 int argc, char *argv[] )
{
   int result, nc;
   if (!arg_check( interp, "vis5d_get_dtx_grid_columns", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   result = vis5d_get_sizePRIME( atoi(argv[1]), NULL, &nc, NULL, NULL,
                             NULL, NULL );
   sprintf( interp->result, "%d", nc );
   return error_check( interp, "vis5d_get_dtx_grid_columns", result );
}


static int cmd_get_grid_levels( ClientData client_data, Tcl_Interp *interp,
                                int argc, char *argv[] )
{
   int result, nl[MAXVARS];
   int index, var;
   index = atoi(argv[1]);
   if (!arg_check( interp, "vis5d_get_grid_levels", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   var = varnum( index, argv[2] );
   result = vis5d_get_size( index, NULL, NULL, nl, NULL,
                            NULL, NULL, NULL, NULL );
   sprintf( interp->result, "%d", nl[var] );
   return error_check( interp, "vis5d_get_grid_levels", result );
}

static int cmd_get_ctx_grid_levels( ClientData client_data, Tcl_Interp *interp,
                                int argc, char *argv[] )
{
   int result, nl[MAXVARS];
   int index, var;
   index = atoi(argv[1]);
   if (!arg_check( interp, "vis5d_get_ctx_grid_levels", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   var = varnum( index, argv[2] );
   result = vis5d_get_size( index, NULL, NULL, nl, NULL,
                            NULL, NULL, NULL, NULL );
   sprintf( interp->result, "%d", nl[var] );
   return error_check( interp, "vis5d_get_ctx_grid_levels", result );
}

static int cmd_get_dtx_grid_levels( ClientData client_data, Tcl_Interp *interp,
                                int argc, char *argv[] )
{
   int result, nl;
   int index, var;
   index = atoi(argv[1]);
   if (!arg_check( interp, "vis5d_get_dtx_grid_levels", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   result = vis5d_get_sizePRIME( index, NULL, NULL, &nl, NULL,
                             NULL, NULL );
   sprintf( interp->result, "%d", nl );
   return error_check( interp, "vis5d_get_dtx_grid_levels", result );
}


static int cmd_set_dtx_grid_rows( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_set_dtx_grid_rows", argc, 2, 2)){
      return TCL_ERROR;
   }
   redo_the_gui = 1;
   result = vis5d_set_dtx_projection_and_vertsys( atoi(argv[1]), SET_PROJ_NumRows,
                                  0, atoi(argv[2]));

   /* WLH 11 Nov 98 */
   if (atoi(argv[1]) >= 0) redo_this_gui[atoi(argv[1])] = 1;

   return error_check( interp, "vis5d_set_dtx_grid_rows", result );
}

static int cmd_set_dtx_grid_columns( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_set_dtx_grid_columns", argc, 2, 2)){
      return TCL_ERROR;
   }
   redo_the_gui = 1;   

   /* WLH 11 Nov 98 */
   if (atoi(argv[1]) >= 0) redo_this_gui[atoi(argv[1])] = 1;

   result = vis5d_set_dtx_projection_and_vertsys( atoi(argv[1]), SET_PROJ_NumCols,
                                  0, atoi(argv[2]));
   return error_check( interp, "vis5d_set_dtx_grid_columns", result );
}

static int cmd_set_dtx_grid_levels( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_set_dtx_grid_levels", argc, 2, 2)){
      return TCL_ERROR;
   }
   redo_the_gui = 1;   

   /* WLH 11 Nov 98 */
   if (atoi(argv[1]) >= 0) redo_this_gui[atoi(argv[1])] = 1;

   result = vis5d_set_dtx_projection_and_vertsys( atoi(argv[1]), SET_PROJ_NumLevs,
                                  0, atoi(argv[2]));
   return error_check( interp, "vis5d_set_dtx_grid_levels", result );
}


/* NOTE: vis5d_get_size isn't in the script interface.  Instead, the
 * get_grid_rows, get_grid_columns, and get_grid_levels are offered.
 */


/* TODO: vis5d_get_grid  (should this be implemented?) */
/* TODO: vis5d_put_grid  (should this be implemented?) */


static int cmd_verylarge_mode( ClientData client_data, Tcl_Interp *interp,
                              int argc, char *argv[] )
{
   int result;
   int index, mode;
   index = atoi(argv[1]);
   if (!arg_check( interp, "vis5d_verylarge_mode", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   if (strcmp(argv[2], "VIS5D_ON")==0) {
      mode = VIS5D_ON;
   }
   else if (strcmp(argv[2], "VIS5D_OFF")==0) {
      mode = VIS5D_OFF;
   }
   else if (strcmp(argv[2], "VIS5D_TOGGLE")==0) {
      mode = VIS5D_TOGGLE;
   }
   else {
      error_check( interp, "vis5d_verylarge_mode", VIS5D_BAD_CONSTANT );
      return TCL_ERROR;
   }

   result = vis5d_verylarge_mode( index, mode );

   if (result) {
      sprintf( interp->result, "VIS5D_ON" );
   }
   else {
      sprintf( interp->result, "VIS5D_OFF" );
   }
   return TCL_OK;
}


/*** Map projection and VCS functions ***/
static int cmd_get_ctx_projection( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int index, result;
   int proj;
   float projargs[MAXPROJARGS];
   int i;
   char *projname;

   for (i=0;i<MAXPROJARGS;i++) {
      projargs[i] = -999.99;
   }

   if (!arg_check( interp, "vis5d_get_ctx_projection", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   result = vis5d_get_ctx_projection( index, &proj, projargs );
   if (result) {
      error_check( interp, "vis5d_get_ctx_projection", VIS5D_FAIL );
      return TCL_ERROR;
   }
   switch (proj) {
      case PROJ_GENERIC:
         projname = "PROJ_GENERIC";
         break;
      case PROJ_LINEAR:
         projname = "PROJ_LINEAR";
         break;
      case PROJ_MERCATOR:
         projname = "PROJ_MERCATOR";
         break;
      case PROJ_LAMBERT:
         projname = "PROJ_LAMBERT";
         break;
      case PROJ_STEREO:
         projname = "PROJ_STEREO";
         break;
      case PROJ_ROTATED:
         projname = "PROJ_ROTATED";
         break;
      case PROJ_CYLINDRICAL:
         projname = "PROJ_CYLINDRICAL";
         break;
      case PROJ_SPHERICAL:
         projname = "PROJ_SPHERICAL";
         break;
      default:
         projname = "unknown projection";
   }

   /* first element of result is projection type */
   Tcl_AppendElement( interp, projname );
   /* rest of elements are projection parameters */
   for (i=0;i<MAXPROJARGS;i++) {
      char val[100];
      if (projargs[i]==-999.99) {
         break;
      }
      sprintf( val, "%g", projargs[i] );
      Tcl_AppendElement( interp, val );
   }

   return TCL_OK;
}

static int cmd_get_projection( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int index, result;
   int proj;
   float projargs[MAXPROJARGS];
   int i;
   char *projname;

   for (i=0;i<MAXPROJARGS;i++) {
      projargs[i] = -999.99;
   }

   if (!arg_check( interp, "vis5d_get_projection", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   result = vis5d_get_ctx_projection( index, &proj, projargs );
   if (result) {
      error_check( interp, "vis5d_get_projection", VIS5D_FAIL );
      return TCL_ERROR;
   }
   switch (proj) {
      case PROJ_GENERIC:
         projname = "PROJ_GENERIC";
         break;
      case PROJ_LINEAR:
         projname = "PROJ_LINEAR";
         break;
      case PROJ_MERCATOR:
         projname = "PROJ_MERCATOR";
         break;
      case PROJ_LAMBERT:
         projname = "PROJ_LAMBERT";
         break;
      case PROJ_STEREO:
         projname = "PROJ_STEREO";
         break;
      case PROJ_ROTATED:
         projname = "PROJ_ROTATED";
         break;
      case PROJ_CYLINDRICAL:
         projname = "PROJ_CYLINDRICAL";
         break;
      case PROJ_SPHERICAL:
         projname = "PROJ_SPHERICAL";
         break;
      default:
         projname = "unknown projection";
   }

   /* first element of result is projection type */
   Tcl_AppendElement( interp, projname );
   /* rest of elements are projection parameters */
   for (i=0;i<MAXPROJARGS;i++) {
      char val[100];
      if (projargs[i]==-999.99) {
         break;
      }
      sprintf( val, "%g", projargs[i] );
      Tcl_AppendElement( interp, val );
   }

   return TCL_OK;
}


static int cmd_get_dtx_projection( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int index, result;
   int proj;
   float projargs[MAXPROJARGS];
   int i;
   char *projname;

   for (i=0;i<MAXPROJARGS;i++) {
      projargs[i] = -999.99;
   }

   if (!arg_check( interp, "vis5d_get_dtx_projection", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   result = vis5d_get_dtx_projection( index, &proj, projargs );
   if (result) {
      error_check( interp, "vis5d_get_dtx_projection", VIS5D_FAIL );
      return TCL_ERROR;
   }
   switch (proj) {
      case PROJ_GENERIC:
         projname = "PROJ_GENERIC";
         break;
      case PROJ_LINEAR:
         projname = "PROJ_LINEAR";
         break;
      case PROJ_MERCATOR:
         projname = "PROJ_MERCATOR";
         break;
      case PROJ_LAMBERT:
         projname = "PROJ_LAMBERT";
         break;
      case PROJ_STEREO:
         projname = "PROJ_STEREO";
         break;
      case PROJ_ROTATED:
         projname = "PROJ_ROTATED";
         break; 
      case PROJ_CYLINDRICAL:
         projname = "PROJ_CYLINDRICAL";
         break;
      case PROJ_SPHERICAL:
         projname = "PROJ_SPHERICAL";
         break;
      default:
         projname = "unknown projection";
   }

   /* first element of result is projection type */
   Tcl_AppendElement( interp, projname );
   /* rest of elements are projection parameters */
   for (i=0;i<MAXPROJARGS;i++) {
      char val[100];
      if (projargs[i]==-999.99) {
         break;
      }
      sprintf( val, "%g", projargs[i] );
      Tcl_AppendElement( interp, val );
   }

   return TCL_OK;
}


static int cmd_set_dtx_projection_and_vertical( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int result;
   int what, type;
   int towhat;

   type = 0;
   if (!arg_check( interp, "vis5d_set_dtx_projection_and_vertical", argc, 3, 4)){
      return TCL_ERROR;
   }
   if (strcmp(argv[2], "SET_PROJ_Projection")==0) {
      what = SET_PROJ_Projection;
      if (strcmp(argv[3], "PROJ_GENERIC")==0){
         towhat = PROJ_GENERIC;
      }
      else if (strcmp(argv[3], "PROJ_LINEAR")==0){
         towhat = PROJ_LINEAR;
      }
      else if (strcmp(argv[3], "PROJ_LAMBERT")==0){
         towhat = PROJ_LAMBERT;
      }
      else if (strcmp(argv[3], "PROJ_MERCATOR")==0){
         towhat = PROJ_MERCATOR;
      }
      else if (strcmp(argv[3], "PROJ_STEREO")==0){
         towhat = PROJ_STEREO;
      }
      else if (strcmp(argv[3], "PROJ_ROTATED")==0){
         towhat = PROJ_ROTATED;
      }
      else if (strcmp(argv[3], "PROJ_CYLINDRICAL")==0){
         towhat = PROJ_CYLINDRICAL;
      }
      else if (strcmp(argv[3], "PROJ_SPHERICAL")==0){
         towhat = PROJ_SPHERICAL;
      }
      else {
         towhat = atoi(argv[3]);
      }
   }
   else if (strcmp(argv[2], "SET_PROJ_NorthBound")==0) {
      what = SET_PROJ_NorthBound;
   }
   else if (strcmp(argv[2], "SET_PROJ_WestBound")==0) {
      what = SET_PROJ_WestBound;
   }
   else if (strcmp(argv[2], "SET_PROJ_RowInc")==0) {
      what = SET_PROJ_RowInc;
   }
   else if (strcmp(argv[2], "SET_PROJ_ColInc")==0) {
      what = SET_PROJ_ColInc;
   }
   else if (strcmp(argv[2], "SET_PROJ_Lat1")==0) {
      what = SET_PROJ_Lat1;
   }
   else if (strcmp(argv[2], "SET_PROJ_Lat2")==0) {
      what = SET_PROJ_Lat2;
   }
   else if (strcmp(argv[2], "SET_PROJ_PoleRow")==0) {
      what = SET_PROJ_PoleRow;
   }
   else if (strcmp(argv[2], "SET_PROJ_PoleCol")==0) {
      what = SET_PROJ_PoleCol;
   }
   else if (strcmp(argv[2], "SET_PROJ_CentralLat")==0) {
      what = SET_PROJ_CentralLat;
   }
   else if (strcmp(argv[2], "SET_PROJ_CentralLon")==0) {
      what = SET_PROJ_CentralLon;
   }
   else if (strcmp(argv[2], "SET_PROJ_CentralRow")==0) {
      what = SET_PROJ_CentralRow;
   }
   else if (strcmp(argv[2], "SET_PROJ_CentralCol")==0) {
      what = SET_PROJ_CentralCol;
   }
   else if (strcmp(argv[2], "SET_PROJ_Rotation")==0) {
      what = SET_PROJ_Rotation;
   }
   else if (strcmp(argv[2], "SET_PROJ_CylinderScale")==0) {   
      what = SET_PROJ_CylinderScale;   
   }   
   else if (strcmp(argv[2], "SET_PROJ_VerticalSystem")==0) {   
      what = SET_PROJ_VerticalSystem;   
      if (strcmp(argv[3], "VERT_GENERIC")==0){
         towhat = VERT_GENERIC;
      }
      else if (strcmp(argv[3], "VERT_EQUAL_KM")==0){
         towhat = VERT_EQUAL_KM;
      }
      else if (strcmp(argv[3], "VERT_NONEQUAL_KM")==0){
         towhat = VERT_NONEQUAL_KM;
      }
      else if (strcmp(argv[3], "VERT_NONEQUAL_MB")==0){
         towhat = VERT_NONEQUAL_MB;
      }
      else {
         towhat = atoi(argv[3]);
      }
   }   
   else if (strcmp(argv[2], "SET_PROJ_LevInc")==0) {   
      what = SET_PROJ_LevInc;   
   }   
   else if (strcmp(argv[2], "SET_PROJ_BottomBound")==0) {   
      what = SET_PROJ_BottomBound;   
   }   
   else if (strcmp(argv[2], "SET_PROJ_Height")==0) {
      what = SET_PROJ_Height;
      type = atoi(argv[3]);
   }
   else if (strcmp(argv[2], "SET_PROJ_Done")==0) {
      what = SET_PROJ_Done;
   }   
   if (what == SET_PROJ_Height){
      result = vis5d_set_dtx_projection_and_vertsys( atoi(argv[1]), what, type, atof(argv[4]));
   }
   else{
      float dyo;
      if (what==SET_PROJ_VerticalSystem || 
          what==SET_PROJ_Projection ){
         dyo = (float) (towhat);
      }
      else{
         dyo = atof(argv[3]);
      }
      result = vis5d_set_dtx_projection_and_vertsys( atoi(argv[1]), what, type, dyo);
   }
   redo_the_gui = 1;   

   /* WLH 11 Nov 98 */
   if (atoi(argv[1]) >= 0) redo_this_gui[atoi(argv[1])] = 1;

   return error_check( interp, "vis5d_set_dtx_projection_and_vertical", result );
}
   
static int cmd_get_ctx_vertical( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int index, result;
   int vert;
   float vertargs[MAXVERTARGS];
   int i;
   char *vertname;
   float bad_vertical = -999.99f;

   for (i=0;i<MAXVERTARGS;i++) {
      vertargs[i] = bad_vertical;
   }

   if (!arg_check( interp, "vis5d_get_ctx_vertical", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   result = vis5d_get_ctx_vertical( index, &vert, vertargs );
   if (result) {
      error_check( interp, "vis5d_get_ctx_vertical", VIS5D_FAIL );
      return TCL_ERROR;
   }
   switch (vert) {
      case VERT_GENERIC:
         vertname = "VERT_GENERIC";
         break;
      case VERT_EQUAL_KM:
         vertname = "VERT_EQUAL_KM";
         break;
      case VERT_NONEQUAL_KM:
         vertname = "VERT_NONEQUAL_KM";
         break;
      case VERT_NONEQUAL_MB:
         vertname = "VERT_NONEQUAL_MB";
         break;
      default:
         vertname = "unknown vertical coordinate system";
   }

   /* first element of result is VCS type */
   Tcl_AppendElement( interp, vertname );
   /* rest of elements are VCS parameters */
   for (i=0;i<MAXVERTARGS;i++) {
      char val[100];
      if (vertargs[i]==bad_vertical) {
         break;
      }
      sprintf( val, "%g", vertargs[i] );
      Tcl_AppendElement( interp, val );
   }

   return TCL_OK;
}
static int cmd_get_vertical( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int index, result;
   int vert;
   float vertargs[MAXVERTARGS];
   int i;
   char *vertname;
   float bad_vertical = -999.99f;

   for (i=0;i<MAXVERTARGS;i++) {
      vertargs[i] = bad_vertical;
   }

   if (!arg_check( interp, "vis5d_get_vertical", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   result = vis5d_get_ctx_vertical( index, &vert, vertargs );
   if (result) {
      error_check( interp, "vis5d_get_vertical", VIS5D_FAIL );
      return TCL_ERROR;
   }
   switch (vert) {
      case VERT_GENERIC:
         vertname = "VERT_GENERIC";
         break;
      case VERT_EQUAL_KM:
         vertname = "VERT_EQUAL_KM";
         break;
      case VERT_NONEQUAL_KM:
         vertname = "VERT_NONEQUAL_KM";
         break;
      case VERT_NONEQUAL_MB:
         vertname = "VERT_NONEQUAL_MB";
         break;
      default:
         vertname = "unknown vertical coordinate system";
   }

   /* first element of result is VCS type */
   Tcl_AppendElement( interp, vertname );
   /* rest of elements are VCS parameters */
   for (i=0;i<MAXVERTARGS;i++) {
      char val[100];
      if (vertargs[i]==bad_vertical) {
         break;
      }
      sprintf( val, "%g", vertargs[i] );
      Tcl_AppendElement( interp, val );
   }

   return TCL_OK;
}

static int cmd_get_dtx_vertical( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int index, result;
   int vert;
   float vertargs[MAXVERTARGS];
   int i;
   char *vertname;
   float bad_vertical = -999.99f;

   for (i=0;i<MAXVERTARGS;i++) {
      vertargs[i] = bad_vertical;
   }

   if (!arg_check( interp, "vis5d_get_dtx_vertical", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   result = vis5d_get_dtx_vertical( index, &vert, vertargs );
   if (result) {
      error_check( interp, "vis5d_get_dtx_vertical", VIS5D_FAIL );
      return TCL_ERROR;
   }
   switch (vert) {
      case VERT_GENERIC:
         vertname = "VERT_GENERIC";
         break;
      case VERT_EQUAL_KM:
         vertname = "VERT_EQUAL_KM";
         break;
      case VERT_NONEQUAL_KM:
         vertname = "VERT_NONEQUAL_KM";
         break;
      case VERT_NONEQUAL_MB:
         vertname = "VERT_NONEQUAL_MB";
         break;
      default:
         vertname = "unknown vertical coordinate system";
   }

   /* first element of result is VCS type */
   Tcl_AppendElement( interp, vertname );
   /* rest of elements are VCS parameters */
   for (i=0;i<MAXVERTARGS;i++) {
      char val[100];
      if (vertargs[i]==bad_vertical) {
         break;
      }
      sprintf( val, "%g", vertargs[i] );
      Tcl_AppendElement( interp, val );
   }

   return TCL_OK;
}



static int cmd_get_curved( ClientData client_data, Tcl_Interp *interp,
                          int argc, char *argv[] )
{
   int index, result, curved;

   if (!arg_check( interp, "vis5d_get_curved", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   result = vis5d_get_curved( index, &curved );
   if (curved) {
      sprintf( interp->result, "VIS5D_ON" );
   }
   else {
      sprintf( interp->result, "VIS5D_OFF" );
   }
   return TCL_OK;
}




/*** Topography, Map and Texture functions ***/

static int cmd_check_topo( ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int index, result, flag;

   if (!arg_check( interp, "vis5d_check_topo", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   result = vis5d_check_topo( index, &flag );
   if (flag) {
      sprintf( interp->result, "VIS5D_ON" );
   }
   else {
      sprintf( interp->result, "VIS5D_OFF" );
   }
   return TCL_OK;
}


static int cmd_check_map( ClientData client_data, Tcl_Interp *interp,
                          int argc, char *argv[] )
{
   int index, result, flag;

   if (!arg_check( interp, "vis5d_check_map", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   result = vis5d_check_map( index, &flag );
   if (flag) {
      sprintf( interp->result, "VIS5D_ON" );
   }
   else {
      sprintf( interp->result, "VIS5D_OFF" );
   }
   return TCL_OK;
}


static int cmd_set_flatmap_level( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int result;

   if (!arg_check( interp, "vis5d_set_flatmap_level", argc, 2, 2)){
      return TCL_ERROR;
   }
   result = vis5d_set_flatmap_level( atoi(argv[1]), atof(argv[2]));
   return error_check( interp, "vis5d_set_flatmap_level", result );
}

static int cmd_get_flatmap_level( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int result;
   float level;

   if (!arg_check( interp, "vis5d_get_flatmap_level", argc, 1, 1)){
      return TCL_ERROR;
   }
   result = vis5d_get_flatmap_level( atoi(argv[1]),  &level);
   sprintf( interp->result, "%g", level);
   return error_check( interp, "vis5d_get_flatmap_level", result );
}


static int cmd_check_texture( ClientData client_data, Tcl_Interp *interp,
                              int argc, char *argv[] )
{
   int index, result, flag;

   if (!arg_check( interp, "vis5d_check_texture", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   result = vis5d_check_texture( index, &flag );
   if (flag) {
      sprintf( interp->result, "VIS5D_ON" );
   }
   else {
      sprintf( interp->result, "VIS5D_OFF" );
   }
   return TCL_OK;
}


static int cmd_set_topo_base( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int result;

   if (!arg_check( interp, "vis5d_set_topo_base", argc, 3, 3)){
      return TCL_ERROR;
   }
   result = vis5d_set_topo_base( atoi(argv[1]), atoi(argv[2]), atof(argv[3]));
   return error_check( interp, "vis5d_set_topo_base", result );
}


static int cmd_get_topo_range( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int index, result;
   float min, max;

   if (!arg_check( interp, "vis5d_get_topo_range", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   result = vis5d_get_topo_range( index, &min, &max );
   sprintf( interp->result, "%g %g", min, max );
   return TCL_OK;
}



static int cmd_reset_topo_colors( ClientData client_data, Tcl_Interp *interp,
                                  int argc, char *argv[] )
{
   int index, result;

   if (!arg_check( interp, "vis5d_reset_topo_colors", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   result = vis5d_reset_topo_colors( index );
   return error_check( interp, "vis5d_reset_topo_colors", result );
}


/* TODO: vis5d_texture_image (How do you specify the binary image data??? */



static int cmd_set_topo_color_var( ClientData client_data, Tcl_Interp *interp,
                                   int argc, char *argv[] )
{
   int index, var, result;

   if (!arg_check( interp, "vis5d_set_topo_color_var", argc, 2,2 )) {
      return TCL_ERROR;
   }
   var = varnum( atoi(argv[1]), argv[2] );
   result = vis5d_set_topo_color_var( atoi(argv[1]), atoi(argv[1]), var);
   return error_check( interp, "vis5d_set_topo_color_var", result );
}

static int cmd_set_topo_color_var_and_owner( ClientData client_data, Tcl_Interp *interp,
                                   int argc, char *argv[] )
{
   int index, var, result;

   if (!arg_check( interp, "vis5d_set_topo_color_var_and_owner", argc, 3, 3 )) {
      return TCL_ERROR;
   }
   var = varnum( atoi(argv[2]), argv[3] );
   result = vis5d_set_topo_color_var( atoi(argv[1]), atoi(argv[2]), var);
   return error_check( interp, "vis5d_set_topo_color_var_and_owner", result );
}



/*** Cloning, Ext funcs, and expression functions ***/

static int cmd_make_clone_variable( ClientData client_data, Tcl_Interp *interp,
                                    int argc, char *argv[] )
{
   int index, var_to_clone, result, newvar;
   char *newname;

   /* WLH 11 Nov 98 */
   int dindex;

/* WLH 11 Nov 98
   redo_the_gui = 1;
*/

   if (!arg_check( interp, "vis5d_make_clone_variable", argc, 3, 3 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   newname = argv[2];
   var_to_clone = varnum( index, argv[3] );
   result = vis5d_make_clone_variable( index, var_to_clone, newname, &newvar );

   /* WLH 11 Nov 98 */
   redo_the_gui = 1;
   if (vis5d_get_ctx_display_index(index, &dindex) > 0) {
     if (dindex >= 0) redo_this_gui[dindex] = 1;
   }

   return error_check( interp, "vis5d_make_clone_variable", result );
}


static int cmd_compute_ext_func( ClientData client_data, Tcl_Interp *interp,
                                 int argc, char *argv[] )
{
   int index, result;
   int newvar;
   GuiContext gtx;
   char funcname[1000];

   if (!arg_check( interp, "vis5d_compute_ext_func", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   redo_the_gui = 1;
   index = atoi(argv[1]);
   gtx = get_gui_gtx(index);
   /* MJK 4.27.99
   if (gtx->funcpath[0]) {
      strcpy( funcname, gtx->funcpath );
   */
   if (Vis5dFuncPath[0]){
      strcpy( funcname, Vis5dFuncPath );
   }
   else {
      strcpy( funcname, FUNCTION_PATH );
   }
   strcat( funcname, "/" );
   strcat( funcname, argv[2] );

   result = vis5d_compute_ext_func( index, funcname, &newvar );

   /* WLH 11 Nov 98 */
   if (index >= 0) redo_this_gui[index] = 1;

   return error_check( interp, "vis5d_compute_ext_func", result );
}



static int cmd_make_expr_var( ClientData client_data, Tcl_Interp *interp,
                              int argc, char *argv[] )
{
   int index, result;
   int newvar, recompute, varowner;
   char newname[20], msg[2000];

   if (!arg_check( interp, "vis5d_make_expr_var", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   redo_the_gui = 1;
   index = atoi(argv[1]);
   result = vis5d_make_expr_var( index, argv[2], newname, msg, &varowner, &newvar,
                                 &recompute );

   /* WLH 11 Nov 98 */
   if (index >= 0) redo_this_gui[index] = 1;

   return error_check( interp, "vis5d_make_expr_var", result );
}




/*** Rendering functions ***/


/* TODO: is this really needed? */
static int cmd_signal_redraw( ClientData client_data, Tcl_Interp *interp,
                              int argc, char *argv[] )
{
   if (!arg_check( interp, "vis5d_signal_redraw", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   if (vis5d_signal_redraw( atoi(argv[1]), atoi(argv[2]) )==0) {
      return TCL_OK;
   }
   else {
      return TCL_ERROR;
   }
}


static int cmd_check_redraw( ClientData client_data, Tcl_Interp *interp,
                             int argc, char *argv[] )
{
   int redraw, result;
   if (!arg_check( interp, "vis5d_check_redraw", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   result = vis5d_check_redraw( atoi(argv[1]), &redraw );
   if (redraw) {
      interp->result = "1";
   }
   else {
      interp->result = "0";
   }
   return error_check( interp, "vis5d_check_redraw", result );
}


static int cmd_draw_frame( ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int dtx;
   if (!arg_check( interp, "vis5d_draw_frame", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   dtx = atoi(argv[1]);
   vis5d_signal_redraw( dtx, 1 );
   vis5d_draw_frame( dtx, 1 );
   vis5d_swap_frame( dtx );
   return TCL_OK;
}


static int cmd_draw_3d_only( ClientData client_data, Tcl_Interp *interp,
                             int argc, char *argv[] )
{
   if (!arg_check( interp, "vis5d_draw_3d_only", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   if (vis5d_draw_3d_only( atoi(argv[1]), atoi(argv[2]) )==0) {
      return TCL_OK;
   }
   else {
      return TCL_ERROR;
   }
}

static int cmd_draw_sounding_only( ClientData client_data, Tcl_Interp *interp,
                                   int argc, char *argv[] )
{
   if (!arg_check( interp, "vis5d_draw_3d_only", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   if (vis5d_draw_sounding_only( atoi(argv[1]), atoi(argv[2]) )==0) {
      return TCL_OK;
   }
   else {
      return TCL_ERROR;
   }
}

 


/* TODO: IS THIS REALLY NEEDED? */
static int cmd_swap_frame( ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   if (!arg_check( interp, "vis5d_swap_frame", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   if (vis5d_swap_frame( atoi(argv[1]) )==0) {
      return TCL_OK;
   }
   else {
      return TCL_ERROR;
   }
}



static int cmd_invalidate_dtx_frames( ClientData client_data, Tcl_Interp *interp,
                                  int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_invalidate_dtx_frames", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   result = vis5d_invalidate_dtx_frames( atoi(argv[1]) );
   return error_check( interp, "vis5d_invalidate_dtx_frames", result );
}


static int cmd_set_pointer( ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_set_pointer", argc, 3, 3 )) {
      return TCL_ERROR;
   }
   result = vis5d_set_pointer( atoi(argv[1]), atoi(argv[2]), atoi(argv[3]) );
   return error_check( interp, "vis5d_set_pointer", result );
}


static int cmd_graphics_mode( ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int what, mode, n;

   if (!arg_check( interp, "vis5d_graphics_mode", argc, 3, 3 )) {
      return TCL_ERROR;
   }
   if (strcmp(argv[2],"VIS5D_BOX")==0) {
      what = VIS5D_BOX;
   }
   else if (strcmp(argv[2],"VIS5D_CLOCK")==0) {
      what = VIS5D_CLOCK;
   }
   else if (strcmp(argv[2],"VIS5D_MAP")==0) {
      what = VIS5D_MAP;
   }
   else if (strcmp(argv[2],"VIS5D_TOPO")==0) {
      what = VIS5D_TOPO;
   }
   else if (strcmp(argv[2],"VIS5D_LEGENDS")==0) {
      what = VIS5D_LEGENDS;
   }
   else if (strcmp(argv[2],"VIS5D_PERSPECTIVE")==0) {
      what = VIS5D_PERSPECTIVE;
   }
   else if (strcmp(argv[2],"VIS5D_CONTOUR_NUMBERS")==0) {
      what = VIS5D_CONTOUR_NUMBERS;
   }
   else if (strcmp(argv[2],"VIS5D_SOUND")==0) {
      what = VIS5D_SOUND;
   }
   else if (strcmp(argv[2],"VIS5D_GRID_COORDS")==0) {
      what = VIS5D_GRID_COORDS;
   }
   else if (strcmp(argv[2],"VIS5D_PRETTY")==0) {
      what = VIS5D_PRETTY;
   }
   else if (strcmp(argv[2],"VIS5D_INFO")==0) {
      what = VIS5D_INFO;
   }
   else if (strcmp(argv[2],"VIS5D_PROBE")==0) {
      what = VIS5D_PROBE;
   }
   else if (strcmp(argv[2],"VIS5D_CURSOR")==0) {
      what = VIS5D_CURSOR;
   }
   else if (strcmp(argv[2],"VIS5D_ANIMRECORD")==0) {
      what = VIS5D_ANIMRECORD;
   }
   else if (strcmp(argv[2],"VIS5D_TEXTURE")==0) {
      what = VIS5D_TEXTURE;
   }
   else if (strcmp(argv[2],"VIS5D_DEPTHCUE")==0) {
      what = VIS5D_DEPTHCUE;
   }
   else if (strcmp(argv[2],"VIS5D_JULIAN")==0) {
      what = VIS5D_JULIAN;
   }
   else if (strcmp(argv[2],"VIS5D_BARBS")==0) {
      what = VIS5D_BARBS;
   }
   else if (strcmp(argv[2],"VIS5D_SND_THTA")==0) {
      what = VIS5D_SND_THTA;
   }
   else if (strcmp(argv[2],"VIS5D_SND_THTE")==0) {
      what = VIS5D_SND_THTE;
   }
   else if (strcmp(argv[2],"VIS5D_SND_W")==0) {
      what = VIS5D_SND_W;
   }
   else if (strcmp(argv[2],"VIS5D_SND_TICKS")==0) {
      what = VIS5D_SND_TICKS;
   }
   else if (strcmp(argv[2],"VIS5D_REVERSE")==0) {
      what = VIS5D_REVERSE;
   }
   else if (strcmp(argv[2],"VIS5D_ALPHAMODE")==0) {
      what = VIS5D_ALPHAMODE;
   }
   else if (strcmp(argv[2],"VIS5D_HIRESTOPO")==0) {
      what = VIS5D_HIRESTOPO;
   }
   else if (strcmp(argv[2],"VIS5D_SAMESCALE")==0) {
      what = VIS5D_SAMESCALE;
   }
/* MJK 6.9.99 */
   else if (strcmp(argv[2],"VIS5D_PROBE_ON_TRAJ")==0){
      what = VIS5D_PROBE_ON_TRAJ;
   }

   else {
      interp->result = "vis5d_graphics_mode: invalid argument";
      return TCL_ERROR;
   }
   
   if (strcmp(argv[3],"VIS5D_ON")==0) {
      mode = VIS5D_ON;
   }
   else if (strcmp(argv[3],"VIS5D_OFF")==0) {
      mode = VIS5D_OFF;
   }
   else if (strcmp(argv[3],"VIS5D_TOGGLE")==0) {
      mode = VIS5D_TOGGLE;
   }
   else if (strcmp(argv[3],"VIS5D_GET")==0) {
      mode = VIS5D_GET;
   }
   else {
      interp->result = "vis5d_graphics_mode: invalid argument";
      return TCL_ERROR;
   }

   n = vis5d_graphics_mode( atoi(argv[1]), what, mode );
   if (n>=0) {
      interp->result = n ? "1" : "0";
      return TCL_OK;
   }
   else {
      return error_check( interp, "vis5d_graphics_mode", n );
   }
}

static int cmd_enable_irregular_graphics( ClientData client_data, Tcl_Interp *interp,
                                int argc, char *argv[] )
{
  int index, what,  mode, result;

   if (!arg_check( interp, "vis5d_enable_irregular_graphics", argc, 3, 3 )){
      return TCL_ERROR;
   }

   if (strcmp(argv[2],"VIS5D_TEXTPLOT")==0) {
      what = VIS5D_TEXTPLOT;
   }
   else { 
      interp->result = "Error in vis5d_enable_irregular_graphics: bad constant";
      return TCL_ERROR;
   }

   if (strcmp(argv[3],"VIS5D_ON")==0) {
      mode = VIS5D_ON;
   }
   else if (strcmp(argv[3],"VIS5D_OFF")==0) {
      mode = VIS5D_OFF;
   }
   else if (strcmp(argv[3],"VIS5D_TOGGLE")==0) {
      mode = VIS5D_TOGGLE;
   }
   else {
      interp->result = "Error in vis5d_enable_graphics: bad mode";
      return TCL_ERROR;
   }

   index = atoi(argv[1]);
   result = vis5d_enable_irregular_graphics( index, what, mode );
   return error_check( interp, "vis5d_enable_irregular_graphics", result );
}

static int cmd_enable_graphics( ClientData client_data, Tcl_Interp *interp,
                                int argc, char *argv[] )
{
   int index, what, mode, n, result, varflag;

   varflag = 0;

   if (!arg_check( interp, "vis5d_graphics", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   if (strcmp(argv[2],"VIS5D_ISOSURF")==0) {
      what = VIS5D_ISOSURF;
      varflag = 1;
   }
   else if (strcmp(argv[2],"VIS5D_HSLICE")==0) {
      what = VIS5D_HSLICE;
      varflag = 1;
   }
   else if (strcmp(argv[2],"VIS5D_VSLICE")==0) {
      what = VIS5D_VSLICE;
      varflag = 1;
   }
   else if (strcmp(argv[2],"VIS5D_CHSLICE")==0) {
      what = VIS5D_CHSLICE;
      varflag = 1;
   }
   else if (strcmp(argv[2],"VIS5D_CVSLICE")==0) {
      what = VIS5D_CVSLICE;
      varflag = 1;
   }
   else if (strcmp(argv[2],"VIS5D_VOLUME")==0) {
      what = VIS5D_VOLUME;
      varflag = 1;
   }
   else if (strcmp(argv[2],"VIS5D_TRAJ")==0) {
      what = VIS5D_TRAJ;
   }
   else if (strcmp(argv[2],"VIS5D_HWIND")==0) {
      what = VIS5D_HWIND;
   }
   else if (strcmp(argv[2],"VIS5D_VWIND")==0) {
      what = VIS5D_VWIND;
   }
   else if (strcmp(argv[2],"VIS5D_HSTREAM")==0) {
      what = VIS5D_HSTREAM;
   }
   else if (strcmp(argv[2],"VIS5D_VSTREAM")==0) {
      what = VIS5D_VSTREAM;
   }
   else {
      interp->result = "Error in vis5d_enable_graphics: bad constant";
      return TCL_ERROR;
   }

   if (strcmp(argv[4],"VIS5D_ON")==0) {
      mode = VIS5D_ON;
   }
   else if (strcmp(argv[4],"VIS5D_OFF")==0) {
      mode = VIS5D_OFF;
   }
   else if (strcmp(argv[4],"VIS5D_TOGGLE")==0) {
      mode = VIS5D_TOGGLE;
   }
   else {
      interp->result = "Error in vis5d_enable_graphics: bad mode";
      return TCL_ERROR;
   }

   index = atoi(argv[1]);
   if (varflag) {
      n = varnum( index, argv[3] );
   }
   else {
      n = atoi( argv[3] );
   }

   result = vis5d_enable_graphics( index, what, n, mode );
   return error_check( interp, "vis5d_enable_graphics", result );
}


static int cmd_get_volume( ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int volowner, vol, result;
   if (!arg_check( interp, "vis5d_get_volume", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   result = vis5d_get_volume( atoi(argv[1]), &volowner, &vol );
   sprintf( interp->result, "%d",  vol );
   return error_check( interp, "vis5d_get_volume", result );
}


static int cmd_get_volume_and_owner( ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int volowner, vol, result;
   if (!arg_check( interp, "vis5d_get_volume_and_owner", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   result = vis5d_get_volume( atoi(argv[1]), &volowner, &vol ); 
   sprintf( interp->result, "%d %d", volowner, vol );
   return error_check( interp, "vis5d_get_volume_and_owner", result );
}


static int cmd_set_volume( ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int varowner, result, index, var;
   if (!arg_check( interp, "vis5d_set_volume", argc,2, 2 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   varowner = atoi(argv[1]);
   var = varnum( varowner, argv[2] );
   result = vis5d_set_volume( index, varowner, var ); 
   return error_check( interp, "vis5d_set_volume", result );
}

static int cmd_set_volume_and_owner( ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int varowner, result, index, var;
   if (!arg_check( interp, "vis5d_set_volume_and_owner", argc, 3, 3 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   varowner = atoi(argv[2]);
/* WLH 18 Nov 98
   var = varnum( varowner, argv[3] );
*/
   /* WLH 18 Nov 98 */
   if (strcmp(argv[3], "-1") == 0) {
     var = -1;
   }
   else {
     var = varnum( varowner, argv[3] );
   }

   result = vis5d_set_volume( index, varowner, var );
   return error_check( interp, "vis5d_set_volume_and_owner", result );
}


static int cmd_set_color( ClientData client_data, Tcl_Interp *interp,
                          int argc, char *argv[] )
{
   int what, result;
   float r, g, b, a;
   int varflag;
   int n;
   int index;
   /* MJK 7.19.99 
      this is for backward compatibility if
      someone with 4.3 VIS5D_MAP it will assign it
      to both VIS5D_DARK_MAP and VIS5D_LIGHT_MAP */
   int do_both_maps = 0;

   if (!arg_check( interp, "vis5d_set_color", argc, 7, 8 )) {
      return TCL_ERROR;
   }

   varflag = 0;

   if (strcmp(argv[2],"VIS5D_ISOSURF")==0) {
      what = VIS5D_ISOSURF;
      varflag = 1;
   }
   else if (strcmp(argv[2],"VIS5D_HSLICE")==0) {
      what = VIS5D_HSLICE;
      varflag = 1;
   }
   else if (strcmp(argv[2],"VIS5D_VSLICE")==0) {
      what = VIS5D_VSLICE;
      varflag = 1;
   }
   else if (strcmp(argv[2],"VIS5D_CHSLICE")==0) {
      what = VIS5D_CHSLICE;
      varflag = 1;
   }
   else if (strcmp(argv[2],"VIS5D_CVSLICE")==0) {
      what = VIS5D_CVSLICE;
      varflag = 1;
   }
   else if (strcmp(argv[2],"VIS5D_TRAJ")==0) {
      what = VIS5D_TRAJ;
   }
   else if (strcmp(argv[2],"VIS5D_HWIND")==0) {
      what = VIS5D_HWIND;
   }
   else if (strcmp(argv[2],"VIS5D_VWIND")==0) {
      what = VIS5D_VWIND;
   }
   else if (strcmp(argv[2],"VIS5D_HSTREAM")==0) {
      what = VIS5D_HSTREAM;
   }
   else if (strcmp(argv[2],"VIS5D_VSTREAM")==0) {
      what = VIS5D_VSTREAM;
   }
   else if (strcmp(argv[2],"VIS5D_BOX")==0) {
      what = VIS5D_BOX;
   }
   else if (strcmp(argv[2],"VIS5D_LIGHT_MAP")==0) {
      what = VIS5D_LIGHT_MAP;
   }
   else if (strcmp(argv[2],"VIS5D_DARK_MAP")==0) {
      what = VIS5D_DARK_MAP;
   }
   else if (strcmp(argv[2],"VIS5D_BACKGROUND")==0) {
      what = VIS5D_BACKGROUND;
   }
   else if (strcmp(argv[2],"VIS5D_LABEL")==0) {
      what = VIS5D_LABEL;
   }
   /* MJK 7.19.99 */
   else if (strcmp(argv[2],"VIS5D_MAP")==0){
      do_both_maps = 1;
   }
   else {
      interp->result = "error in vis5d_set_color: bad constant";
      return TCL_ERROR;
   }


   if (varflag && argc>8){
      r = atof( argv[5] );
      g = atof( argv[6] );
      b = atof( argv[7] );
      a = atof( argv[8] );
   }
   else{
      r = atof( argv[4] );
      g = atof( argv[5] );
      b = atof( argv[6] );
      a = atof( argv[7] );
   }

   index = atoi(argv[1]);

   if (varflag && argc>8) {
      int m;
      m = atoi( argv[3] );
      n = varnum(m , argv[4] );
      n = m*MAXVARS+n;
   }
   else if (varflag && argc <= 8){
      int m;
      m = atoi( argv[1] );
      n = varnum(m ,argv[3] );
      n = m*MAXVARS+n;
   }  
   else {
      n = atoi( argv[3] );
   }
   /* MJK 7.19.99 */
   if (do_both_maps){
      result = vis5d_set_color( index, VIS5D_LIGHT_MAP, n, r, g, b, a );
      result = vis5d_set_color( index, VIS5D_DARK_MAP,  n, r, g, b, a );
   }
   else{
      result = vis5d_set_color( index, what, n, r, g, b, a );
   }
   return error_check( interp, "vis5d_set_color", result );
}

static int cmd_get_color( ClientData client_data,
                                      Tcl_Interp *interp,
                                      int argc, char *argv[] )
{
   int result;
   float r, g, b, a;
   int n, what;
   int varflag;
 
   if (!arg_check( interp, "vis5d_get_color", argc, 3, 4)){
      return TCL_ERROR;
   }

   varflag = 0;

   if (strcmp(argv[2],"VIS5D_ISOSURF")==0) {
      what = VIS5D_ISOSURF;
      varflag = 1;
   }
   else if (strcmp(argv[2],"VIS5D_HSLICE")==0) {
      what = VIS5D_HSLICE;
      varflag = 1;
   }
   else if (strcmp(argv[2],"VIS5D_VSLICE")==0) {
      what = VIS5D_VSLICE;
      varflag = 1;
   }
   else if (strcmp(argv[2],"VIS5D_CHSLICE")==0) {
      what = VIS5D_CHSLICE;
      varflag = 1;
   }
   else if (strcmp(argv[2],"VIS5D_CVSLICE")==0) {
      what = VIS5D_CVSLICE;
      varflag = 1;
   }
   else if (strcmp(argv[2],"VIS5D_TRAJ")==0) {
      what = VIS5D_TRAJ;
   }
   else if (strcmp(argv[2],"VIS5D_HWIND")==0) {
      what = VIS5D_HWIND;
   }
   else if (strcmp(argv[2],"VIS5D_VWIND")==0) {
      what = VIS5D_VWIND;
   }
   else if (strcmp(argv[2],"VIS5D_HSTREAM")==0) {
      what = VIS5D_HSTREAM;
   }
   else if (strcmp(argv[2],"VIS5D_VSTREAM")==0) {
      what = VIS5D_VSTREAM;
   }
   else if (strcmp(argv[2],"VIS5D_BOX")==0) {
      what = VIS5D_BOX;
   }
   else if (strcmp(argv[2],"VIS5D_LIGHT_MAP")==0) {
      what = VIS5D_LIGHT_MAP;
   }
   else if (strcmp(argv[2],"VIS5D_DARK_MAP")==0) {
      what = VIS5D_DARK_MAP;
   }
   else if (strcmp(argv[2],"VIS5D_BACKGROUND")==0) {
      what = VIS5D_BACKGROUND;
   }
   else if (strcmp(argv[2],"VIS5D_LABEL")==0) {
      what = VIS5D_LABEL;
   }
   else {
      interp->result = "error in vis5d_get_color: bad constant";
      return TCL_ERROR;
   }

   if (varflag && argc>4){
      int m;
      m = atoi( argv[3] );
      n = varnum(m , argv[4] );
      n = m*MAXVARS+n;
   }
   else if (varflag && argc <= 4){
      int m;
      m = atoi( argv[1] );
      n = varnum(m ,argv[3] );
      n = m*MAXVARS+n;
   }
   else {
      n = atoi( argv[3] );
   }

   result = vis5d_get_color( atoi(argv[1]), what, n, 
                             &r, &g, &b, &a);

   sprintf(interp->result,"%f %f %f %f", r, g, b, a);
   return error_check( interp, "vis5d_get_color", result );
}
 

static int cmd_load_color_table( ClientData client_data,
                                      Tcl_Interp *interp,
                                      int argc, char *argv[] )
{
   int index, graphic;
   int var,varowner, tablesize;
   int result;




   if (!arg_check( interp, "vis5d_load_color_table", argc, 5,6)){
      return TCL_ERROR;
   }

   if (argc >6){
      index = atoi( argv[1] );
      graphic = atoi( argv[2]);
      varowner = atoi(argv[3]);
      var = varnum(index,argv[4]);
      tablesize = atoi( argv[5] );
      result = vis5d_load_color_table( index, graphic,varowner, var, tablesize, argv[6]);
   }
   else{
      index = atoi( argv[1] );
      graphic = atoi( argv[2]);
      varowner = atoi(argv[1]);
      var = varnum(index,argv[3]);
      tablesize = atoi( argv[4] );
      result = vis5d_load_color_table( index, graphic,varowner, var, tablesize, argv[5]);
   }

   error_check( interp, "vis5d_load_color_table", result );
}

static int cmd_set_color_table_entry( ClientData client_data,
                                      Tcl_Interp *interp,
                                      int argc, char *argv[] )
{
   int what, varowner, var, result, entry;
   int r, g, b, a;
   int index;
   unsigned int *ctable;

   if (!arg_check( interp, "vis5d_set_color_table_entry", argc, 8,9  )) {
      return TCL_ERROR;
   }

   if (strcmp(argv[2],"VIS5D_ISOSURF")==0) {
      what = VIS5D_ISOSURF;
   }
   else if (strcmp(argv[2],"VIS5D_CHSLICE")==0) {
      what = VIS5D_CHSLICE;
   }
   else if (strcmp(argv[2],"VIS5D_CVSLICE")==0) {
      what = VIS5D_CVSLICE;
   }
   else if (strcmp(argv[2],"VIS5D_TRAJ")==0) {
      what = VIS5D_TRAJ;
   }
   else if (strcmp(argv[2],"VIS5D_TOPO")==0) {
      what = VIS5D_TOPO;
   }
   else if (strcmp(argv[2],"VIS5D_VOLUME")==0) {
      what = VIS5D_VOLUME;
   }
   else {
      interp->result = "error in vis5d_set_color_table_entry: bad constant";
      return TCL_ERROR;
   }

   if (argc == 10){
      index = atoi( argv[1] );
      varowner = atoi( argv[3] );
      var = varnum( varowner, argv[4] );
      entry = atoi( argv[5] );
      r = atoi( argv[6] );
      g = atoi( argv[7] );
      b = atoi( argv[8] );
      a = atoi( argv[9] );
   }
   else{
      index = atoi( argv[1] );
      varowner = atoi( argv[1] );
      var = varnum( varowner, argv[3] );
      entry = atoi( argv[4] );
      r = atoi( argv[5] );
      g = atoi( argv[6] );
      b = atoi( argv[7] );
      a = atoi( argv[8] );
   }
   vis5d_get_color_table_address( index, what, varowner, var, &ctable ); 
   if (entry>=0 && entry<=255) {
      ctable[entry] = PACK_COLOR( r, g, b, a );
   }
   return TCL_OK;
}


static int cmd_set_color_table_params( ClientData client_data,
                                       Tcl_Interp *interp,
                                       int argc, char *argv[] )
{
   int index, varowner, var, what;
   float p[4];

   if (!arg_check( interp, "vis5d_set_color_table_params", argc, 7,8 )) {
      return TCL_ERROR;
   }

   if (strcmp(argv[2],"VIS5D_ISOSURF")==0) {
      what = VIS5D_ISOSURF;
   }
   else if (strcmp(argv[2],"VIS5D_CHSLICE")==0) {
      what = VIS5D_CHSLICE;
   }
   else if (strcmp(argv[2],"VIS5D_CVSLICE")==0) {
      what = VIS5D_CVSLICE;
   }
   else if (strcmp(argv[2],"VIS5D_TRAJ")==0) {
      what = VIS5D_TRAJ;
   }
   else if (strcmp(argv[2],"VIS5D_TOPO")==0) {
      what = VIS5D_TOPO;
   }
   else if (strcmp(argv[2],"VIS5D_VOLUME")==0) {
      what = VIS5D_VOLUME;
   }
   else {
      interp->result = "error in vis5d_set_color_table_params: bad constant";
      return TCL_ERROR;
   }
   if (argc==9){
      index = atoi( argv[1] );
      varowner = atoi( argv[3] );
      var = varnum( varowner, argv[4] );
      p[0] = atof( argv[5] );
      p[1] = atof( argv[6] );
      p[2] = atof( argv[7] );
      p[3] = atof( argv[8] );
   }
   else{
      index = atoi( argv[1] );
      varowner = atoi( argv[1] );
      var = varnum( varowner, argv[3] );
      p[0] = atof( argv[4] );
      p[1] = atof( argv[5] );
      p[2] = atof( argv[6] );
      p[3] = atof( argv[7] );
   }


   vis5d_set_color_table_params( index, what, varowner, var, p );
   return TCL_OK;
}
   

static int cmd_alpha_mode( ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_alpha_mode", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   result = vis5d_alpha_mode( atoi(argv[1]), atoi(argv[2]) );
   return error_check( interp, "vis5d_alpha_mode", result );
}


static int cmd_font( ClientData client_data, Tcl_Interp *interp,
                     int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_font", argc, 2,3 )) {
      return TCL_ERROR;
   }
   result = vis5d_font(  atoi(argv[1]), argv[2], atoi(argv[3])  ); 
   return error_check( interp, "vis5d_font", result );
}


/* WLH 8 Oct 98 */
static int cmd_get_font( ClientData client_data, Tcl_Interp *interp,
                         int argc, char *argv[] )
{
   int result, size;
   char fontname[200];
   if (!arg_check( interp, "vis5d_get_font", argc, 1,1 )) {
      return TCL_ERROR;
   }
   size = 0;
   result = vis5d_get_font( atoi(argv[1]), fontname, &size );
   sprintf(interp->result,"%s %d", fontname, size);
   return error_check( interp, "vis5d_get_font", result );
}

/* WLH 8 Oct 98 */
static int cmd_get_font_height( ClientData client_data, Tcl_Interp *interp,
                                int argc, char *argv[] )
{
   int result;
   int height;
   if (!arg_check( interp, "vis5d_get_font_height", argc, 1,1 )) {
      return TCL_ERROR;
   }
   result = vis5d_get_font_height( atoi(argv[1]), &height );
   sprintf(interp->result,"%d", height);
   return error_check( interp, "vis5d_get_font_height", result );
}

/* MJK 2.22.99 */
static int cmd_resize_contour_font( ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_resize_contour_font", argc, 3, 3)){
      return TCL_ERROR;
   }
   result = vis5d_resize_contour_font( atoi(argv[1]), atoi(argv[2]), atoi(argv[3]) );
   return error_check( interp, "vis5d_resize_contour_font", result );
}



static int cmd_linewidth( ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_linewidth", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   result = vis5d_linewidth( atoi(argv[1]), atof(argv[2]) );
   return error_check( interp, "vis5d_linewidth", result );
}




/*** 3-D View Functions ***/

static int cmd_set_matrix( ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int result;
   int i, j;
   float mat[4][4];

   if (!arg_check( interp, "vis5d_set_matrix", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   string_to_float_array( argv[2], 16, (float *) mat );

   result = vis5d_set_matrix( atoi(argv[1]), mat );
   return error_check( interp, "vis5d_set_matrix", result );
}


static int cmd_get_matrix( ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int result;
   int i, j;
   float mat[4][4];

   if (!arg_check( interp, "vis5d_get_matrix", argc, 1, 1 )) {
      return TCL_ERROR;
   }

   result = vis5d_get_matrix( atoi(argv[1]), mat );
   for (i=0;i<4;i++) {
      for (j=0;j<4;j++) {
         char val[100];
         sprintf( val, "%g", mat[i][j] );
         Tcl_AppendElement( interp, val );
      }
   }
   return error_check( interp, "vis5d_get_matrix", result );
}


static int cmd_set_ortho_view( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int result, view;

   if (!arg_check( interp, "vis5d_set_ortho_view", argc, 2, 2 )) {
      return TCL_ERROR;
   }

   if (strcmp(argv[2],"VIS5D_NORTH")==0) {
      view = VIS5D_NORTH;
   }
   else if (strcmp(argv[2],"VIS5D_SOUTH")==0) {
      view = VIS5D_SOUTH;
   }
   else if (strcmp(argv[2],"VIS5D_EAST")==0) {
      view = VIS5D_EAST;
   }
   else if (strcmp(argv[2],"VIS5D_WEST")==0) {
      view = VIS5D_WEST;
   }
   else if (strcmp(argv[2],"VIS5D_TOP")==0) {
      view = VIS5D_TOP;
   }
   else if (strcmp(argv[2],"VIS5D_BOTTOM")==0) {
      view = VIS5D_BOTTOM;
   }

   result = vis5d_set_ortho_view( atoi(argv[1]), view );
   return error_check( interp, "vis5d_set_ortho_view", result );
   return TCL_ERROR;
}


static int cmd_set_view( ClientData client_data, Tcl_Interp *interp,
                         int argc, char *argv[] )
{
   int n, r;
   int ctx;
   float xrot, yrot, zrot, scale, xtrans, ytrans, ztrans;

   if (!arg_check( interp, "vis5d_set_view", argc, 4, 8 )) {
      return TCL_ERROR;
   }
   ctx = atoi(argv[1]);
   xrot = atof(argv[2]);
   yrot = atof(argv[3]);
   zrot = atof(argv[4]);
   scale = (argc>5) ? atof(argv[5]) : 1.0;
   xtrans = (argc>6) ? atof(argv[6]) : 0.0;
   ytrans = (argc>7) ? atof(argv[7]) : 0.0;
   ztrans = (argc>8) ? atof(argv[8]) : 0.0;

   r = vis5d_set_view( ctx, xrot, yrot, zrot, scale, xtrans, ytrans, ztrans );
   return error_check( interp, "vis5d_set_view", r );
}



static int cmd_get_view( ClientData client_data, Tcl_Interp *interp,
                         int argc, char *argv[] )
{
   int result, n;
   int index;
   float xrot, yrot, zrot, scale, xtrans, ytrans, ztrans;

   if (!arg_check( interp, "vis5d_get_view", argc, 1, 1)) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   result = vis5d_get_view(index, &xrot, &yrot, &zrot, &scale, &xtrans, &ytrans, &ztrans );
   sprintf( interp->result, "%.0f %.0f %.0f %.0f %.0f %.0f %.0f", xrot, yrot, zrot, scale,
                                                    xtrans, ytrans, ztrans );
   return error_check( interp, "vis5d_get_view", result);
}

static int cmd_get_view_scales( ClientData client_data, Tcl_Interp *interp,
                         int argc, char *argv[] )
{
   int result;
   float scalex, scaley, scalez;
   if (!arg_check( interp, "vis5d_get_view_scales", argc, 1, 1)){
      return TCL_ERROR;
   }
   result = vis5d_get_view_scales(atoi(argv[1]), &scalex, &scaley, &scalez);
   sprintf( interp->result, "%.0f %.0f %.0f", scalex, scaley, scalez);
   return error_check( interp, "vis5d_get_view_scales", result);
}


static int cmd_set_camera( ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int n, r;
   int ctx, perspec;
   float front, zoom;

   if (!arg_check( interp, "vis5d_set_camera", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   ctx = atoi(argv[1]);
   perspec = atoi(argv[2]);
   front = atof(argv[3]);
   zoom = atof(argv[4]);

   r = vis5d_set_camera( ctx, perspec, front, zoom );
   return error_check( interp, "vis5d_set_camera", r );
}

static int cmd_get_camera( ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int n, r;
   int ctx, perspec;
   float front, zoom;

   if (!arg_check( interp, "vis5d_get_camera", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   ctx = atoi(argv[1]);
   r = vis5d_get_camera( ctx,  &perspec, &front, &zoom );
   sprintf( interp->result, "%u %g %g", perspec, front, zoom);
   return error_check( interp, "vis5d_get_camera", r);
}



/*** Coordinate conversion ***/

static int cmd_xyz_to_grid( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int n, index, time, var;
   float xyz[3], row, col, lev;
   if (!arg_check( interp, "vis5d_xyz_to_grid", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   time = atoi(argv[2]);
   var = varnum(index, argv[3] );
   string_to_float_array( argv[4], 3, xyz );
   n = vis5d_xyz_to_grid( index, time, var, xyz[0], xyz[1], xyz[2],
                          &row, &col, &lev );
   if (n) {
      return error_check( interp, "vis5d_xyz_to_grid", n );
   }
   else {
      sprintf( interp->result, "%f %f %f", row, col, lev );
      return 0;
   }
}


static int cmd_xyzPRIME_to_gridPRIME( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int n, index, time, var;
   float xyz[3], row, col, lev;
   if (!arg_check( interp, "vis5d_xyzPRIME_to_gridPRIME", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   time = atoi(argv[2]);
   var = varnum(index, argv[3] );
   string_to_float_array( argv[4], 3, xyz );
   n = vis5d_xyzPRIME_to_gridPRIME( index, time, var, xyz[0], xyz[1], xyz[2],
                          &row, &col, &lev );
   if (n) {
      return error_check( interp, "vis5d_xyzPRIME_to_gridPRIME", n );
   }
   else {
      sprintf( interp->result, "%f %f %f", row, col, lev );
      return 0;
   }
}



static int cmd_grid_to_xyz( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int n, index, time, var;
   float x, y, z, grid[3];
   if (!arg_check( interp, "vis5d_grid_to_xyz", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   time = atoi(argv[2]);
   var = varnum(index, argv[3] );
   string_to_float_array( argv[4], 3, grid );
   n = vis5d_grid_to_xyz( index, time, var, grid[0], grid[1], grid[2],
                          &x, &y, &z );
   if (n) {
      return error_check( interp, "vis5d_grid_to_xyz", n );
   }
   else {
      sprintf( interp->result, "%f %f %f", x, y, z );
      return 0;
   }
}


static int cmd_gridPRIME_to_xyzPRIME( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int n, index, time, var;
   float x, y, z, grid[3];
   if (!arg_check( interp, "vis5d_gridPRIME_to_xyzPRIME", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   time = atoi(argv[2]);
   var = varnum(index, argv[3] );
   string_to_float_array( argv[4], 3, grid );
   n = vis5d_gridPRIME_to_xyzPRIME( index, time, var, grid[0], grid[1], grid[2],
                          &x, &y, &z );
   if (n) {
      return error_check( interp, "vis5d_gridPRIME_to_xyzPRIME", n );
   }
   else {
      sprintf( interp->result, "%f %f %f", x, y, z );
      return 0;
   }
}




static int cmd_xyz_to_geo( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int n, index, time, var;
   float xyz[3], lat, lon, hgt;
   if (!arg_check( interp, "vis5d_xyz_to_geo", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   time = atoi(argv[2]);
   var = varnum(index, argv[3] );
   string_to_float_array( argv[4], 3, xyz );
   n = vis5d_xyz_to_geo( index, time, var, xyz[0], xyz[1], xyz[2],
                         &lat, &lon, &hgt );
   if (n) {
      return error_check( interp, "vis5d_xyz_to_geo", n );
   }
   else {
      sprintf( interp->result, "%f %f %f", lat, lon, hgt );
      return 0;
   }
}

static int cmd_xyzPRIME_to_geo( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int n, index, time, var;
   float xyz[3], lat, lon, hgt;
   if (!arg_check( interp, "vis5d_xyzPRIME_to_geo", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   time = atoi(argv[2]);
   var = varnum(index, argv[3] );
   string_to_float_array( argv[4], 3, xyz );
   n = vis5d_xyzPRIME_to_geo( index, time, var, xyz[0], xyz[1], xyz[2],
                         &lat, &lon, &hgt );
   if (n) {
      return error_check( interp, "vis5d_xyzPRIME_to_geo", n );
   }
   else { 
      sprintf( interp->result, "%f %f %f", lat, lon, hgt );
      return 0;
   }
}


static int cmd_grid_to_geo( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int n, index, time, var;
   float lat, lon, hgt, rcl[3];

   if (!arg_check( interp, "vis5d_grid_to_geo", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   time = atoi(argv[2]);
   var = varnum(index, argv[3] );
   string_to_float_array( argv[4], 3, rcl );
   n = vis5d_grid_to_geo( index, time, var, rcl[0], rcl[1], rcl[2],
                          &lat, &lon, &hgt);
   if (n) {
      return error_check( interp, "vis5d_grid_to_geo", n);
   }
   else {
      sprintf( interp->result, "%f %f %f", lat, lon, hgt);
      return 0;
   }
}


static int cmd_gridPRIME_to_geo( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int n, index, time, var;
   float lat, lon, hgt, rcl[3];

   if (!arg_check( interp, "vis5d_gridPRIME_to_geo", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   time = atoi(argv[2]);
   var = varnum(index, argv[3] );
   string_to_float_array( argv[4], 3, rcl );
   n = vis5d_gridPRIME_to_geo( index, time, var, rcl[0], rcl[1], rcl[2],
                          &lat, &lon, &hgt);
   if (n) {
      return error_check( interp, "vis5d_gridPRIME_to_geo", n);
   }
   else { 
      sprintf( interp->result, "%f %f %f", lat, lon, hgt);
      return 0;
   }
}




static int cmd_rowcol_to_latlon( ClientData client_data, Tcl_Interp *interp,
                                 int argc, char *argv[] )
{
   int n, index, time, var;
   float lat, lon, rc[2];

   if (!arg_check( interp, "vis5d_rowcol_to_latlon", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   time = atoi(argv[2]);
   var = varnum(index, argv[3] );
   string_to_float_array( argv[4], 2, rc );
   n = vis5d_rowcol_to_latlon( index, time, var, rc[0], rc[1],
                               &lat, &lon);
   if (n) {
      return error_check( interp, "vis5d_rowcol_to_latlon", n);
   }
   else {
      sprintf( interp->result, "%f %f", lat, lon);
      return 0;
   }
}


static int cmd_rowcolPRIME_to_latlon( ClientData client_data, Tcl_Interp *interp,
                                 int argc, char *argv[] )
{
   int n, index, time, var;
   float lat, lon, rc[2];

   if (!arg_check( interp, "vis5d_rowcolPRIME_to_latlon", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   time = atoi(argv[2]);
   var = varnum(index, argv[3] );
   string_to_float_array( argv[4], 2, rc );
   n = vis5d_rowcolPRIME_to_latlon( index, time, var, rc[0], rc[1],
                               &lat, &lon);
   if (n) {
      return error_check( interp, "vis5d_rowcolPRIME_to_latlon", n);
   }
   else {
      sprintf( interp->result, "%f %f", lat, lon);
      return 0;
   }
}



static int cmd_latlon_to_rowcol( ClientData client_data, Tcl_Interp *interp,
                                 int argc, char *argv[] )
{
   int n, index, time, var;
   float row, col, geo[2];

   if (!arg_check( interp, "vis5d_latlon_to_rowcol", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   time = atoi(argv[2]);
   var = varnum(index, argv[3] );
   string_to_float_array( argv[4], 2, geo );
   n = vis5d_latlon_to_rowcol( index, time, var, geo[0], geo[1],
                          &row, &col);
   if (n) {
      return error_check( interp, "vis5d_latlon_to_rowcol", n);
   }
   else {
      sprintf( interp->result, "%f %f", row, col);
      return 0;
   }
}


static int cmd_latlon_to_rowcolPRIME( ClientData client_data, Tcl_Interp *interp,
                                 int argc, char *argv[] )
{
   int n, index, time, var;
   float row, col, geo[2];

   if (!arg_check( interp, "vis5d_latlon_to_rowcolPRIME", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   time = atoi(argv[2]);
   var = varnum(index, argv[3] );
   string_to_float_array( argv[4], 2, geo );
   n = vis5d_latlon_to_rowcolPRIME( index, time, var, geo[0], geo[1],
                          &row, &col);
   if (n) {
      return error_check( interp, "vis5d_latlonPRIME_to_rowcol", n);
   }
   else {
      sprintf( interp->result, "%f %f", row, col);
      return 0;
   }
}



static int cmd_geo_to_grid( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int n, index, time, var;
   float row, col, lev, geo[3];

   if (!arg_check( interp, "vis5d_geo_to_grid", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   time = atoi(argv[2]);
   var = varnum(index, argv[3] );
   string_to_float_array( argv[4], 3, geo );
   n = vis5d_geo_to_grid( index, time, var, geo[0], geo[1], geo[2],
                          &row, &col, &lev);
   if (n) {
      return error_check( interp, "vis5d_geo_to_grid", n);
   }
   else {
      sprintf( interp->result, "%f %f %f", row, col, lev);
      return 0;
   }
}

static int cmd_geo_to_gridPRIME( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int n, index, time, var;
   float row, col, lev, geo[3];

   if (!arg_check( interp, "vis5d_geo_to_gridPRIME", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   time = atoi(argv[2]);
   var = varnum(index, argv[3] );
   string_to_float_array( argv[4], 3, geo );
   n = vis5d_geo_to_gridPRIME( index, time, var, geo[0], geo[1], geo[2],
                          &row, &col, &lev);
   if (n) {
      return error_check( interp, "vis5d_geo_to_gridPRIME", n);
   }
   else {
      sprintf( interp->result, "%f %f %f", row, col, lev);
      return 0;
   }
}
   


static int cmd_geo_to_xyz( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int n, index, time, var;
   float x, y, z, geo[3];
   if (!arg_check( interp, "vis5d_geo_to_xyz", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   time = atoi(argv[2]);
   var = varnum(index, argv[3] );
   string_to_float_array( argv[4], 3, geo );
   n = vis5d_geo_to_xyz( index, time, var, geo[0], geo[1], geo[2],
                         &x, &y, &z );
   if (n) {
      return error_check( interp, "vis5d_geo_to_xyz", n );
   }
   else {
      sprintf( interp->result, "%f %f %f", x, y, z );
      return 0;
   }
}


static int cmd_geo_to_xyzPRIME( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int n, index, time, var;
   float x, y, z, geo[3];
   if (!arg_check( interp, "vis5d_geo_to_xyzPRIME", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   time = atoi(argv[2]);
   var = varnum(index, argv[3] );
   string_to_float_array( argv[4], 3, geo );
   n = vis5d_geo_to_xyzPRIME( index, time, var, geo[0], geo[1], geo[2],
                         &x, &y, &z );
   if (n) {
      return error_check( interp, "vis5d_geo_to_xyzPRIME", n );
   }
   else {
      sprintf( interp->result, "%f %f %f", x, y, z );
      return 0;
   }
}

static int cmd_set_hclip( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_set_hclip", argc, 3, 3)){
      return TCL_ERROR;
   }
   result = vis5d_set_hclip( atoi(argv[1]), atoi(argv[2]), atof(argv[3]));
   return error_check( interp, "vis5d_set_hclip", result);
}
  
static int cmd_set_vclip( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_set_vclip", argc, 6,6)){
      return TCL_ERROR;
   }
   result = vis5d_set_vclip(atoi(argv[1]), atoi(argv[2]), atof(argv[3]),
                            atof(argv[4]), atof(argv[5]), atof(argv[6])); 
   return error_check( interp, "vis5d_set_vclip", result);
}

static int cmd_get_hclip( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int result;
   float lev;
   if (!arg_check( interp, "vis5d_get_hclip", argc, 2, 2)){
      return TCL_ERROR;
   }
   result = vis5d_get_hclip(atoi(argv[1]), atoi(argv[2]), &lev);
   sprintf(interp->result,"%f", lev);
   return error_check( interp, "vis5d_get_hclip", result);
}

static int cmd_get_vclip( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int result;
   float r1, r2, c1, c2;
   if (!arg_check( interp, "vis5d_get_vclip", argc, 2, 2)){
         return TCL_ERROR;
   }
   result = vis5d_get_vclip( atoi(argv[1]), atoi(argv[2]), &r1, &c1, &r2, &c2);
   sprintf(interp->result,"%f %f %f %f", r1, c1, r2, c2);
   return error_check( interp, "vis5d_get_vclip", result);
}

/* do we really need these two funcs in here?? */

static int cmd_set_clip_mode(ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_set_clip_mode", argc, 3, 3)){
         return TCL_ERROR;
   }
   result = vis5d_set_clip_mode( atoi(argv[1]), atoi(argv[2]), atoi(argv[3]));
   return error_check( interp, "vis5d_set_clip_mode", result);
}

static int cmd_get_clip_mode(ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int result;
   int mode;
   if (!arg_check( interp, "vis5d_get_clip_mode", argc, 2, 2)){
         return TCL_ERROR;
   }
   result = vis5d_get_clip_mode( atoi(argv[1]), atoi(argv[2]), &mode);
   sprintf(interp->result,"%d", mode);
   return error_check( interp, "vis5d_get_clip_mode", result);
}

static int cmd_get_num_of_ctxs_in_display(ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int yo, result;
   int number, numarray[1000];
   char val[100];
   if (!arg_check( interp, "vis5d_get_num_of_ctxs_in_display", argc, 1, 1)){
         return TCL_ERROR;
   }
   result = vis5d_get_num_of_ctxs_in_display( atoi(argv[1]), &number, numarray);
   sprintf( val, "%d", number );
   Tcl_AppendElement( interp, val );
   for (yo=0; yo < number; yo++){
      sprintf( val, "%d", numarray[yo] );
      Tcl_AppendElement( interp, val );
   }
   return TCL_OK;
}

static int cmd_get_num_of_itxs_in_display(ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int yo, result;
   int number, numarray[1000];
   char val[100];
   if (!arg_check( interp, "vis5d_get_num_of_itxs_in_display", argc, 1, 1)){
         return TCL_ERROR;
   }
   result = vis5d_get_num_of_itxs_in_display( atoi(argv[1]), &number, numarray);
   sprintf( val, "%d", number );
   Tcl_AppendElement( interp, val );
   for (yo=0; yo < number; yo++){
      sprintf( val, "%d", numarray[yo] );
      Tcl_AppendElement( interp, val );
   }
   return TCL_OK;
}

static int cmd_get_num_of_dtxs_in_group(ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int yo, result;
   int number, numarray[1000];
   char val[100];   
   if (!arg_check( interp, "vis5d_get_num_of_dtxs_in_group", argc, 1, 1)){
         return TCL_ERROR;
   }
   result = vis5d_get_num_of_dtxs_in_group( atoi(argv[1]), &number, numarray);
   sprintf( val, "%d", number );
   Tcl_AppendElement( interp, val );
   for (yo=0; yo < number; yo++){
      sprintf( val, "%d", numarray[yo] );
      Tcl_AppendElement( interp, val );
   }
   return TCL_OK;
}

/**** text plots ****/
static int cmd_set_text_plot(ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int n;
   if (!arg_check( interp, "vis5d_set_text_plot", argc, 6, 6)){
      return TCL_ERROR;
   }
   n = vis5d_set_text_plot(atoi(argv[1]), atoi(argv[2]), atof(argv[3]),
                           atof(argv[4]), atof(argv[5]),atof(argv[6]));
   return error_check( interp, "vis5d_set_text_plot", n );
}

static int cmd_get_text_plot(ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int n, var;
   float spacing, fontx, fonty, fontspace;
   
   if (!arg_check( interp, "vis5d_get_text_plot", argc, 1, 1)){
      return TCL_ERROR;
   }
   n = vis5d_get_text_plot(atoi(argv[1]), &var, &spacing, 
                           &fontx, &fonty, &fontspace);
   sprintf( interp->result, "%d %f %f %f %f", var, spacing, fontx, fonty, fontspace);
   return error_check( interp, "vis5d_get_text_plot", n);
}

static int cmd_make_text_plot( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int n;   
   if (!arg_check( interp, "vis5d_make_text_plot", argc, 3, 3)){
      return TCL_ERROR;
   }
   n = vis5d_make_text_plot(atoi(argv[1]), atoi(argv[2]), atoi(argv[3]));
   return error_check( interp, "vis5d_make_text_plot", n);
}

static int cmd_set_textplot_color_status(ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int status, n;
   if (!arg_check( interp, "vis5d_set_textplot_color_status", argc, 3, 3)){
      return TCL_ERROR;
   }
   if (strcmp(argv[3],"VIS5D_ON")==0){
      status = VIS5D_ON;
   }
   else if (strcmp(argv[3],"VIS5D_OFF")==0){
      status = VIS5D_OFF;
   }
   else{
      interp->result = "Error in vis5d_set_textplot_color_status";
      return TCL_ERROR;
   }
   n = vis5d_set_textplot_color_status(atoi(argv[1]), atoi(argv[2]), status);
   return error_check( interp, "vis5d_set_textplot_color_status", n);
}

static int cmd_get_textplot_color_status(ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int status, n;
   if (!arg_check( interp, "vis5d_get_textplot_color_status", argc, 2, 2)){
      return TCL_ERROR;
   }
   n = vis5d_get_textplot_color_status(atoi(argv[1]), atoi(argv[2]), &status);
   if (status == VIS5D_ON){
      sprintf( interp->result, "1");
   }
   else{
      sprintf( interp->result, "0");
   }
   return error_check( interp, "vis5d_get_textplot_color_status", n);
}



/*** Isosurface, Slice, and Trajectory Functions ***/

static int cmd_set_isosurface( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int n, index, var;
   if (!arg_check( interp, "vis5d_set_isosurface", argc, 3, 3 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   var = varnum(index, argv[2] );
   n = vis5d_set_isosurface( index, var, atof(argv[3]) );
   return error_check( interp, "vis5d_set_isosurface", n );
}


static int cmd_get_isosurface( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int n, index, var;
   float isolevel;
   if (!arg_check( interp, "vis5d_get_isosurface", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   var = varnum(index, argv[2] );
   n = vis5d_get_isosurface( index, var, &isolevel );
   sprintf( interp->result, "%g", isolevel );
   return error_check( interp, "vis5d_get_isosurface", n );
}

      
static int cmd_set_isosurface_color_var( ClientData client_data,
                                         Tcl_Interp *interp,
                                         int argc, char *argv[] )
{
   int n, index, var, colorvarowner, colorvar;
   if (!arg_check( interp, "vis5d_set_isosurface_color_var", argc, 3,3 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   var = varnum(index, argv[2]);
   colorvarowner = atoi(argv[1]);
   colorvar = varnum(colorvarowner, argv[3]);
   n = vis5d_set_isosurface_color_var( index, var, colorvarowner, colorvar ); 
   return error_check( interp, "vis5d_set_isosurface_color_var", n );
}

static int cmd_set_isosurface_color_var_and_owner( ClientData client_data,
                                         Tcl_Interp *interp,
                                         int argc, char *argv[] )
{
   int n, index, var, colorvarowner, colorvar;
   if (!arg_check( interp, "vis5d_set_isosurface_color_var_and_owner", argc, 4,4 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   var = varnum(index, argv[2]);
   colorvarowner = atoi(argv[3]);
   colorvar = varnum(colorvarowner, argv[4]);
   n = vis5d_set_isosurface_color_var( index, var, colorvarowner, colorvar );
   return error_check( interp, "vis5d_set_isosurface_color_var_and_owner", n );
}


static int cmd_make_isosurface( ClientData client_data, Tcl_Interp *interp,
                                int argc, char *argv[] )
{
   int n, index, var;
   if (!arg_check( interp, "vis5d_make_isosurface", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   var = varnum(index, argv[3] );

   /* WLH 15 Oct 98 */
   if (var < 0) return TCL_ERROR;

   if (strcmp(argv[2],"VIS5D_ALL_TIMES")==0) {
      int numtimes, i;
      vis5d_get_ctx_numtimes( index, &numtimes );
      for (i=0;i<numtimes;i++) {
         n = vis5d_make_isosurface( index, i, var, atoi(argv[4]) );
         if (n!=0) {
            break;
         }
      }
   }
   else {
      /* make one timestep */
      n = vis5d_make_isosurface( index, atoi(argv[2]), var, atoi(argv[4]) );
   }
   vis5d_finish_work();
   return error_check( interp, "vis5d_make_isosurface", n );
}

      
static int cmd_set_hslice( ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int n, index, var;
   float interval, low, high, level, min, max;
   if (!arg_check( interp, "vis5d_set_hslice", argc, 6, 6 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   var = varnum(index, argv[2] );
   interval = atof(argv[3]);
   low = atof(argv[4]);
   high = atof(argv[5]);
   level = atof(argv[6]);
   vis5d_get_ctx_var_range( index, var, &min, &max );
   /* If low and high are very nearly equal to min and max, then set */
   /* low and high equal to min and max.  This takes care of the problem */
   /* of lost precision when converting from binary to ASCII and back. */
   if (ABS(low-min)<ABS(min/100.0)) {
      low = min;
   }
   if (ABS(high-max)<ABS(max/100.0)) {
      high = max;
   }
   n = vis5d_set_hslice( index, var, interval, low, high, level );
   return error_check( interp, "vis5d_set_hslice", n );
}


static int cmd_get_hslice( ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int index, var, result;
   float interval, low, high, level;

   if (!arg_check( interp, "vis5d_get_hslice", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   var = varnum(index,argv[2]);
   result = vis5d_get_hslice( index, var, &interval, &low, &high, &level );
   if (result) {
      return error_check( interp, "vis5d_get_hslice", result );
   }
   sprintf( interp->result, "%g %g %g %g", interval, low, high, level );
   return TCL_OK;
}


static int cmd_make_hslice( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int n, index, var;
   if (!arg_check( interp, "vis5d_make_hslice", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   var = varnum(index, argv[3] );
   if (strcmp(argv[2],"VIS5D_ALL_TIMES")==0) {
      int numtimes, i;
      vis5d_get_dtx_numtimes( index, &numtimes );
      for (i=0;i<numtimes;i++) {
         n = vis5d_make_hslice( index, i, var, atoi(argv[4]) );
         if (n!=0) {
            break;
         }
      }
   }
   else {
      /* make one timestep */
      n = vis5d_make_hslice( index, atoi(argv[2]), var, atoi(argv[4]) );
   }
   vis5d_finish_work();
   return error_check( interp, "vis5d_make_hslice", n );
}



static int cmd_set_vslice( ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int n, index, var;
   float interval, low, high, min, max;
   if (!arg_check( interp, "vis5d_set_vslice", argc, 9, 9 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   var = varnum( index, argv[2] );
   interval = atof(argv[3]);
   low = atof(argv[4]);
   high = atof(argv[5]);
   vis5d_get_ctx_var_range( index, var, &min, &max );
   /* If low and high are very nearly equal to min and max, then set */
   /* low and high equal to min and max.  This takes care of the problem */
   /* of lost precision when converting from binary to ASCII and back. */
   if (ABS(low-min)<ABS(min/100.0)) {
      low = min;
   }
   if (ABS(high-max)<ABS(max/100.0)) {
      high = max;
   }
   n = vis5d_set_vslice( index, var, interval, low, high,
                         atof(argv[6]), atof(argv[7]),
                         atof(argv[8]), atof(argv[9]) );
   return error_check( interp, "vis5d_set_vslice", n );
}


static int cmd_get_vslice( ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int index, var, result;
   float interval, low, high, r0, c0, r1, c1;

   if (!arg_check( interp, "vis5d_get_vslice", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   var = varnum(index,argv[2]);
   result = vis5d_get_vslice( index, var, &interval, &low, &high, &r0, &c0,
                              &r1, &c1 );
   if (result) {
      return error_check( interp, "vis5d_get_vslice", result );
   }
   sprintf( interp->result, "%g %g %g %g %g %g %g", interval, low, high,
            r0, c0, r1, c1 );
   return TCL_OK;
}


static int cmd_make_vslice( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int n, index, var;
   if (!arg_check( interp, "vis5d_make_vslice", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   var = varnum(index, argv[3] );
   if (strcmp(argv[2],"VIS5D_ALL_TIMES")==0) {
      int numtimes, i;
      vis5d_get_dtx_numtimes( index, &numtimes );
      for (i=0;i<numtimes;i++) {
         n = vis5d_make_vslice( index, i, var, atoi(argv[4]) );
         if (n!=0) {
            break;
         }
      }
   }
   else {
      /* make one timestep */
      n = vis5d_make_vslice( index, atoi(argv[2]), var, atoi(argv[4]) );
   }
   vis5d_finish_work();
   return error_check( interp, "vis5d_make_vslice", n );
}


static int cmd_make_chslice( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int n, index, var;
   if (!arg_check( interp, "vis5d_make_chslice", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   var = varnum( index, argv[3] );
   if (strcmp(argv[2],"VIS5D_ALL_TIMES")==0) {
      int numtimes, i;
      vis5d_get_dtx_numtimes( index, &numtimes );
      for (i=0;i<numtimes;i++) {
         n = vis5d_make_chslice( index, i, var, atoi(argv[4]) );
         if (n) {
            break;
         }
      }
   }
   else {
      /* make one timestep */
      n = vis5d_make_chslice( index, atoi(argv[2]), var, atoi(argv[4]) );
   }
   vis5d_finish_work();
   return error_check( interp, "vis5d_make_chslice", n );
}


static int cmd_set_chslice( ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int n, index, var;
   if (!arg_check( interp, "vis5d_set_chslice", argc, 3, 3 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   var = varnum( index, argv[2] );
   n = vis5d_set_chslice( index, var, atof(argv[3]) );
   return error_check( interp, "vis5d_set_chslice", n );
}


static int cmd_get_chslice( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int index, var, result;
   float interval, low, high, level;

   if (!arg_check( interp, "vis5d_get_chslice", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   var = varnum(index,argv[2]);
   result = vis5d_get_chslice( index, var, &level );
   if (result) {
      return error_check( interp, "vis5d_get_chslice", result );
   }
   sprintf( interp->result, "%g", level );
   return TCL_OK;
}


static int cmd_make_cvslice( ClientData client_data, Tcl_Interp *interp,
                             int argc, char *argv[] )
{
   int n, index, var;
   if (!arg_check( interp, "vis5d_make_cvslice", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   var = varnum( index, argv[3] );
   if (strcmp(argv[2],"VIS5D_ALL_TIMES")==0) {
      int numtimes, i;
      vis5d_get_dtx_numtimes( index, &numtimes );
      for (i=0;i<numtimes;i++) {
         n = vis5d_make_cvslice( index, i, var, atoi(argv[4]) );
         if (n) {
            break;
         }
      }
   }
   else {
      /* make one timestep */
      n = vis5d_make_cvslice( index, atoi(argv[2]), var, atoi(argv[4]) );
   }
   vis5d_finish_work();
   return error_check( interp, "vis5d_make_cvslice", n );
}


static int cmd_set_cvslice( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int n, index, var;
   if (!arg_check( interp, "vis5d_set_cvslice", argc, 6, 6 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   var = varnum( index, argv[2] );
   n = vis5d_set_cvslice( index, var,
                          atof(argv[3]), atof(argv[4]),
                          atof(argv[5]), atof(argv[6]) );
   return error_check( interp, "vis5d_set_cvslice", n );
}


static int cmd_get_cvslice( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int index, var, result;
   float r0, c0, r1, c1;

   if (!arg_check( interp, "vis5d_get_cvslice", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   var = varnum(index,argv[2]);
   result = vis5d_get_cvslice( index, var, &r0, &c0, &r1, &c1 );
   if (result) {
      return error_check( interp, "vis5d_get_cvslice", result );
   }
   sprintf( interp->result, "%g %g %g %g", r0, c0, r1, c1 );
   return TCL_OK;
}



static int cmd_make_hwindslice( ClientData client_data, Tcl_Interp *interp,
                                int argc, char *argv[] )
{
   int n, index;
   if (!arg_check( interp, "vis5d_make_hwindslice", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   if (strcmp(argv[2],"VIS5D_ALL_TIMES")==0) {
      int numtimes, i;
      vis5d_get_dtx_numtimes( index, &numtimes );
      for (i=0;i<numtimes;i++) {
         n = vis5d_make_hwindslice( index, i, atoi(argv[3]), atoi(argv[4]) );
         if (n) {
            break;
         }
      }
   }
   else {
      /* make one timestep */
      n = vis5d_make_hwindslice( index, atoi(argv[2]), atoi(argv[3]),
                            atoi(argv[4]) );
   }
   vis5d_finish_work();
   return error_check( interp, "vis5d_make_hwindslice", n );
}


static int cmd_set_hwindslice( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int n, index;
   if (!arg_check( interp, "vis5d_set_hwindslice", argc, 5, 5 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   n = vis5d_set_hwindslice( index, atoi(argv[2]),
                             atof(argv[3]), atof(argv[4]),
                             atof(argv[5]) );
   return error_check( interp, "vis5d_set_hwindslice", n );
}


static int cmd_get_hwindslice( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int index, var, result;
   float density, scale, level;

   if (!arg_check( interp, "vis5d_get_hwindslice", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   var = atoi(argv[2]);
   result = vis5d_get_hwindslice( index, var, &density, &scale, &level );
   if (result) {
      return error_check( interp, "vis5d_get_hwindslice", result );
   }
   sprintf( interp->result, "%g %g %g", density, scale, level );
   return TCL_OK;
}


static int cmd_make_vwindslice( ClientData client_data, Tcl_Interp *interp,
                                int argc, char *argv[] )
{
   int n;
   if (!arg_check( interp, "vis5d_make_vwindslice", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   if (strcmp(argv[2],"VIS5D_ALL_TIMES")==0) {
      int numtimes, i;
      int ctx = atoi(argv[1]);
      vis5d_get_dtx_numtimes( ctx, &numtimes );
      for (i=0;i<numtimes;i++) {
         n = vis5d_make_vwindslice( ctx, i, atoi(argv[3]), atoi(argv[4]) );
         if (n) {
            break;
         }
      }
   }
   else {
      /* make one timestep */
      n = vis5d_make_vwindslice( atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
                            atoi(argv[4]) );
   }
   vis5d_finish_work();
   return error_check( interp, "vis5d_make_vwindslice", n );
}


static int cmd_set_vwindslice( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int n;
   if (!arg_check( interp, "vis5d_set_vwindslice", argc, 8, 8 )) {
      return TCL_ERROR;
   }
   n = vis5d_set_vwindslice( atoi(argv[1]), atoi(argv[2]),
                             atof(argv[3]), atof(argv[4]),
                             atof(argv[5]), atof(argv[6]),
                             atof(argv[7]), atof(argv[8]) );
   return error_check( interp, "vis5d_set_vwindslice", n );
}


static int cmd_get_vwindslice( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int index, var, result;
   float density, scale, r0, c0, r1, c1;

   if (!arg_check( interp, "vis5d_get_vwindslice", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   var = atoi(argv[2]);
   result = vis5d_get_vwindslice( index, var, &density, &scale,
                                  &r0, &c0, &r1, &c1 );
   if (result) {
      return error_check( interp, "vis5d_get_vwindslice", result );
   }
   sprintf( interp->result, "%g %g %g %g %g %g", density, scale,
            r0, c0, r1, c1 );
   return TCL_OK;
}



static int cmd_make_hstreamslice( ClientData client_data, Tcl_Interp *interp,
                                  int argc, char *argv[] )
{
   int n;
   if (!arg_check( interp, "vis5d_make_hstreamslice", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   if (strcmp(argv[2],"VIS5D_ALL_TIMES")==0) {
      int numtimes, i;
      int ctx = atoi(argv[1]);
      vis5d_get_dtx_numtimes( ctx, &numtimes );
      for (i=0;i<numtimes;i++) {
         n = vis5d_make_hstreamslice( ctx, i, atoi(argv[3]), atoi(argv[4]) );
         if (n) {
            break;
         }
      }
   }
   else {
      /* make one timestep */
      n = vis5d_make_hstreamslice( atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
                                  atoi(argv[4]) );
   }
   vis5d_finish_work();
   return error_check( interp, "vis5d_make_hstreamslice", n );
}


static int cmd_set_hstreamslice( ClientData client_data, Tcl_Interp *interp,
                                 int argc, char *argv[] )
{
   int n;
   if (!arg_check( interp, "vis5d_set_hstreamslice", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   n = vis5d_set_hstreamslice( atoi(argv[1]), atoi(argv[2]),
                              atof(argv[3]), atof(argv[4]));
   return error_check( interp, "vis5d_set_hstreamslice", n );
}


static int cmd_get_hstreamslice( ClientData client_data, Tcl_Interp *interp,
                                 int argc, char *argv[] )
{
   int index, var, result;
   float density, level;

   if (!arg_check( interp, "vis5d_get_hstreamslice", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   var = atoi(argv[2]);
   result = vis5d_get_hstreamslice( index, var, &density, &level );
   if (result) {
      return error_check( interp, "vis5d_get_hstreamslice", result );
   }
   sprintf( interp->result, "%g %g", density, level );
   return TCL_OK;
}



static int cmd_make_vstreamslice( ClientData client_data, Tcl_Interp *interp,
                                  int argc, char *argv[] )
{
   int n;
   if (!arg_check( interp, "vis5d_make_vstreamslice", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   if (strcmp(argv[2],"VIS5D_ALL_TIMES")==0) {
      int numtimes, i;
      int ctx = atoi(argv[1]);
      vis5d_get_dtx_numtimes( ctx, &numtimes );
      for (i=0;i<numtimes;i++) {
         n = vis5d_make_vstreamslice( ctx, i, atoi(argv[3]), atoi(argv[4]) );
         if (n) {
            break;
         }
      }
   }
   else {
      /* make one timestep */
      n = vis5d_make_vstreamslice( atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
                                  atoi(argv[4]) );
   }
   vis5d_finish_work();
   return error_check( interp, "vis5d_make_vstreamslice", n );
}


static int cmd_set_vstreamslice( ClientData client_data, Tcl_Interp *interp,
                                 int argc, char *argv[] )
{
   int n;
   if (!arg_check( interp, "vis5d_set_vstreamslice", argc, 7, 7 )) {
      return TCL_ERROR;
   }
   n = vis5d_set_vstreamslice( atoi(argv[1]), atoi(argv[2]),
                               atof(argv[3]), atof(argv[4]),
                               atof(argv[5]), atof(argv[6]),
                               atof(argv[7]) );

   return error_check( interp, "vis5d_set_vstreamslice", n );
}


static int cmd_get_vstreamslice( ClientData client_data, Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int index, var, result;
   float density, r0, c0, r1, c1;

   if (!arg_check( interp, "vis5d_get_vstreamslice", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   var = atoi(argv[2]);
   result = vis5d_get_vstreamslice( index, var, &density, &r0, &c0, &r1, &c1 );
   if (result) {
      return error_check( interp, "vis5d_get_vstreamslice", result );
   }
   sprintf( interp->result, "%g %g %g %g %g", density, r0, c0, r1, c1 );
   return TCL_OK;
}


static int cmd_print_traj( ClientData client_data, Tcl_Interp *interp,
                          int argc, char *argv[] )
{
   int i, result, numtimes;
   char val[200];
   float lat[MAXTIMES],
         lon[MAXTIMES],
         hgt[MAXTIMES],
         value[MAXTIMES];

   if (!arg_check( interp, "vis5d_print_traj", argc, 2, 2)){
      return TCL_ERROR;
   }
   result = vis5d_print_traj( atoi(argv[1]), atoi(argv[2]),
                              lat, lon, hgt, value);
   vis5d_get_dtx_numtimes( atoi(argv[1]), &numtimes);
   for (i=0; i < numtimes; i++){
      printf("%d %f %f %f %f\n", i, lat[i], lon[i], hgt[i], value[i]);
   }
   return TCL_OK;
}


static int cmd_make_traj( ClientData client_data, Tcl_Interp *interp,
                          int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_make_traj", argc, 6, 6 )) {
      return TCL_ERROR;
   }
   result = vis5d_make_traj( atoi(argv[1]), atof(argv[2]), atof(argv[3]),
                             atof(argv[4]), atoi(argv[5]), atoi(argv[6]) );
   if (NumThreads==1) {
      vis5d_do_work();
   }
   return error_check( interp, "vis5d_make_traj", result );
}


static int cmd_set_traj( ClientData client_data, Tcl_Interp *interp,
                         int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_set_traj", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   result = vis5d_set_traj( atoi(argv[1]), atof(argv[2]), atof(argv[3]),
                            atoi(argv[4]));
   return error_check( interp, "vis5d_set_traj", result );
}


static int cmd_get_traj( ClientData client_data, Tcl_Interp *interp,
                         int argc, char *argv[] )
{
   int index, ribbon, result;
   float step, length;

   if (!arg_check( interp, "vis5d_get_traj", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   result = vis5d_get_traj( index, &step, &length, &ribbon );
   if (result) {
      return error_check( interp, "vis5d_get_traj", result );
   }
   sprintf( interp->result, "%g %g %d", step, length, ribbon );
   return TCL_OK;
}


static int cmd_set_trajectory_color_var( ClientData client_data,
                                         Tcl_Interp *interp,
                                         int argc, char *argv[] )
{
   int result, index, varowner, var;
   if (!arg_check( interp, "vis5d_set_trajectory_color_var", argc, 3,3 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   varowner = atoi(argv[1]);
   var = varnum(varowner,argv[3]);
   result = vis5d_set_trajectory_color_var( index, atoi(argv[2]), varowner, var );
   return error_check( interp, "vis5d_set_trajectory_color_var", result );
}

static int cmd_set_trajectory_color_var_and_owner( ClientData client_data,
                                         Tcl_Interp *interp,
                                         int argc, char *argv[] )
{
   int result, index, varowner, var;
   if (!arg_check( interp, "vis5d_set_trajectory_color_var_and_owner", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   index = atoi(argv[1]);
   varowner = atoi(argv[3]);
   var = varnum(varowner,argv[4]);
   result = vis5d_set_trajectory_color_var( index, atoi(argv[2]), varowner, var );
   return error_check( interp, "vis5d_set_trajectory_color_var_and_owner", result );
}


static int cmd_delete_last_traj( ClientData client_data, Tcl_Interp *interp,
                                 int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_delete_last_traj", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   result = vis5d_delete_last_traj( atoi(argv[1]) );
   return error_check( interp, "vis5d_delete_last_traj", result );
}


static int cmd_delete_traj_set( ClientData client_data, Tcl_Interp *interp,
                                int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_delete_traj_set", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   result = vis5d_delete_traj_set( atoi(argv[1]), atoi(argv[2]) );
   return error_check( interp, "vis5d_delete_traj_set", result );
}



/*** Text Label Functions ***/


/* vis5d_make_label context x y text */
static int cmd_make_label( ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_make_label", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   result = vis5d_make_label( atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
                              argv[4] );
   return error_check( interp, "vis5d_make_label", result );
}


/* vis5d_delete_label context x y */
static int cmd_delete_label( ClientData client_data, Tcl_Interp *interp,
                             int argc, char *argv[] )
{
   int result, x, y, label_id;
   if (!arg_check( interp, "vis5d_delete_label", argc, 3, 3 )) {
      return TCL_ERROR;
   }
   x = atoi(argv[2]);
   y = atoi(argv[3]);
   result = vis5d_find_label( atoi(argv[1]), &x, &y, &label_id );
   if (result == VIS5D_FAIL) return TCL_OK;
   if (result != 0) return error_check( interp, "vis5d_delete_label", result );
   result = vis5d_delete_label( atoi(argv[1]), label_id);
   return error_check( interp, "vis5d_delete_label", result );
}







static int cmd_make_timestep_graphics( ClientData client_data,
                                       Tcl_Interp *interp,
                                       int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_make_timestep_graphics", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   result = vis5d_make_timestep_graphics( atoi(argv[1]), atoi(argv[2]) );
   vis5d_finish_work();
   return error_check( interp, "vis5d_make_timestep_graphics", result );
}


static int cmd_free_graphics( ClientData client_data,
                               Tcl_Interp *interp,
                               int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_free_graphics", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   vis5d_finish_work();
   result = vis5d_free_graphics( atoi(argv[1]) );
   return error_check( interp, "vis5d_free_graphics", result );
}





/*** 3-D Cursor Functions ***/

static int cmd_set_cursor( ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_set_cursor", argc, 4, 4 )) {
      return TCL_ERROR;
   }
   result = vis5d_set_cursor( atoi(argv[1]), atof(argv[2]), atof(argv[3]),
                              atof(argv[4]));
   return error_check( interp, "vis5d_set_cursor", result );
}


static int cmd_get_cursor( ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   float x, y, z;
   int result;
   if (!arg_check( interp, "vis5d_get_cursor", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   result = vis5d_get_cursor( atoi(argv[1]), &x, &y, &z );
   sprintf( interp->result, "%g %g %g", x, y, z );
   return error_check( interp, "vis5d_get_cursor", result );
}

static int cmd_set_logo_size( ClientData client_data, Tcl_Interp *interp,
                           int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_set_logo_size", argc, 2, 2)){
      return TCL_ERROR;
   }
   result = vis5d_set_logo_size(atoi(argv[1]), atof(argv[2]));
   return error_check( interp, "vis5d_set_logo_size", result );
}


/*** 3-D viewing window functions ***/

static int cmd_get_image_formats( ClientData client_data, Tcl_Interp *interp,
                                  int argc, char *argv[] )
{
   int formats;
   char result[1000];

   vis5d_get_image_formats(&formats);

   /* form a string list of image formats supported */
   strcpy( result, "{ " );
   if (formats & VIS5D_RGB) {
      strcat( result, "VIS5D_RGB " );
   }
   if (formats & VIS5D_GIF) {
      strcat( result, "VIS5D_GIF " );
   }
   if (formats & VIS5D_XWD) {
      strcat( result, "VIS5D_XWD " );
   }
   if (formats & VIS5D_PS) {
      strcat( result, "VIS5D_PS " );
   }
   if (formats & VIS5D_COLOR_PS) {
      strcat( result, "VIS5D_COLOR_PS " );
   }
   /* MJK 11.19.98 */      
   if (formats & VIS5D_PPM) {
      strcat( result, "VIS5D_PPM " );
   }
   strcat( result, "}" );
   interp->result = result;
   return TCL_OK;
}


static int cmd_save_window( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int format;
   int result;
   char *name; 
   if (!arg_check( interp, "vis5d_save_window", argc, 2, 3  )) {
      return TCL_ERROR;
   }
   else {
      if (argc==4){
         name = argv[2];
         if (strcmp(argv[3],"VIS5D_GIF")==0) {
            format = VIS5D_GIF;
         }
         else if (strcmp(argv[3],"VIS5D_RGB")==0) {
            format = VIS5D_RGB;
         }
         else if (strcmp(argv[3],"VIS5D_XWD")==0) {
            format = VIS5D_XWD;
         }
         /* MJK 11.19.98 */         
         else if (strcmp(argv[3],"VIS5D_PPM")==0) {
            format = VIS5D_PPM;
         }
         else if (strcmp(argv[3],"VIS5D_TGA")==0) {
            format = VIS5D_TGA;
         }
         else if (strcmp(argv[3],"VIS5D_PS")==0) {
            format = VIS5D_PS;
         }
         else if (strcmp(argv[3],"VIS5D_COLOR_PS")==0) {
            format = VIS5D_COLOR_PS;
         }
         
         else {
            interp->result = "vis5d_save_window: bad format";
            return TCL_ERROR;
         }
      }
      else{
         name = argv[1];
         if (strcmp(argv[2],"VIS5D_GIF")==0) {
            format = VIS5D_GIF;
         }
         else if (strcmp(argv[2],"VIS5D_RGB")==0) {
            format = VIS5D_RGB;
         }
         else if (strcmp(argv[2],"VIS5D_XWD")==0) {
            format = VIS5D_XWD;
         }
         else if (strcmp(argv[2],"VIS5D_PS")==0) {
            format = VIS5D_PS;
         }
         else if (strcmp(argv[2],"VIS5D_TGA")==0) {
            format = VIS5D_TGA;
         }
         else if (strcmp(argv[2],"VIS5D_COLOR_PS")==0) {
            format = VIS5D_COLOR_PS;
         }
         /* MJK 11.19.98 */            
         else if (strcmp(argv[2],"VIS5D_PPM")==0) {
            format = VIS5D_PPM;
         }         
         else {
            interp->result = "vis5d_save_window: bad format";
            return TCL_ERROR;
         }
      }

      result = vis5d_save_window(  name, format );
      return error_check( interp, "vis5d_save_window", result );
   }
}

static int cmd_save_snd_window( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   if (!arg_check( interp, "vis5d_save_snd_window", argc, 3,3  )) {
      return TCL_ERROR;
   }
   else {
      char *name = argv[2];
      int format;
      int result;
      if (strcmp(argv[3],"VIS5D_GIF")==0) {
         format = VIS5D_GIF;
      }
      else if (strcmp(argv[3],"VIS5D_RGB")==0) {
         format = VIS5D_RGB;
      }
      else if (strcmp(argv[3],"VIS5D_XWD")==0) {
         format = VIS5D_XWD;
      }
      else if (strcmp(argv[3],"VIS5D_PS")==0) {
         format = VIS5D_PS;
      }
      else if (strcmp(argv[3],"VIS5D_COLOR_PS")==0) {
         format = VIS5D_COLOR_PS;
      }
      else if (strcmp(argv[3],"VIS5D_PPM")==0) {
         format = VIS5D_PPM;
      }
      else if (strcmp(argv[3],"VIS5D_TGA")==0) {
         format = VIS5D_TGA;
      }
      else {
         interp->result = "vis5d_save_snd_window: bad format";
         return TCL_ERROR;
      }
      result = vis5d_save_snd_window(  atoi(argv[1]), name, format );
      return error_check( interp, "vis5d_save_snd_window", result );
   }
}


static int cmd_resize_BIG_window( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_resize_BIG_window", argc, 2, 2)){
      return TCL_ERROR;
   }
   result = vis5d_resize_BIG_window( atoi(argv[1]), atoi(argv[2]));
   return error_check( interp, "vis5d_resize_BIG_window", result );
}
 
static int cmd_resize_3d_window( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_resize_3d_window", argc, 3, 3)){
      return TCL_ERROR;
   }
   result = vis5d_resize_3d_window( atoi(argv[1]), atoi(argv[2]),
                                        atoi(argv[3]));
   return error_check( interp, "vis5d_resize_3d_window", result );
}

static int cmd_moveresize_BIG_window( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_moveresize_BIG_window", argc, 4, 4)){
      return TCL_ERROR;
   }
   result = vis5d_moveresize_BIG_window( atoi(argv[1]), atoi(argv[2]),
                                        atoi(argv[3]), atoi(argv[4]));
   return error_check( interp, "vis5d_moveresize_BIG_window", result );
}

static int cmd_moveresize_3d_window( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_moveresize_3d_window", argc, 5, 5)){
      return TCL_ERROR;
   }                        
   result = vis5d_moveresize_3d_window( atoi(argv[1]), atoi(argv[2]),
                                        atoi(argv[3]), atoi(argv[4]),
                                        atoi(argv[5]));
   return error_check( interp, "vis5d_moveresize_3d_window", result );
}     
   


static int cmd_print_window( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_print_snd_window", argc, 0,1 )) {
      return TCL_ERROR;
   }
   result = vis5d_print_window( );
   return error_check( interp, "vis5d_print_window", result );
}

static int cmd_print_snd_window( ClientData client_data, Tcl_Interp *interp,
                            int argc, char *argv[] )
{
   int result;
   if (!arg_check( interp, "vis5d_print_snd_window", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   result = vis5d_print_snd_window( atoi(argv[1]) );
   return error_check( interp, "vis5d_print_snd_window", result );
}

   

/*** Save and Restore Functions ***/

static int cmd_save( ClientData client_data, Tcl_Interp *interp,
                     int argc, char *argv[] )
{
   int n;
   if (!arg_check( interp, "vis5d_save", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   n = tcl_save( atoi(argv[1]), argv[2] );
   return error_check( interp, "vis5d_save", n );
}


static int cmd_restore( ClientData client_data, Tcl_Interp *interp,
                        int argc, char *argv[] )
{
   int n;
   if (!arg_check( interp, "vis5d_restore", argc, 2, 2 )) {
      return TCL_ERROR;
   }
   n = vis5d_restore( atoi(argv[1]), argv[2] );
   return error_check( interp, "vis5d_restore", n );
}



static int cmd_sleep( ClientData client_data, Tcl_Interp *interp,
                      int argc, char *argv[] )
{
   struct timeval tp;
   long time0, time;
   long milliseconds;

   if (!arg_check( interp, "vis5d_sleep", argc, 1, 1 )) {
      return TCL_ERROR;
   }
   milliseconds = atoi( argv[1] );

   /* Get the current time */
   gettimeofday( &tp, (struct timezone *) 0 );
   time0 = tp.tv_sec * 1000 + tp.tv_usec / 1000;

   /* Loop until milliseconds elapses.  This is a poor implementation
    * but I don't know a better way that's portable.
    */
   do {
      gettimeofday( &tp, (struct timezone *) 0 );
      time = tp.tv_sec * 1000 + tp.tv_usec / 1000;
   } while (time < time0 + milliseconds);

   return TCL_OK;
}

/*** Functions useful for controlling Vis5D via -pipe */

/* WLH 30 Sept 98 */
/* return $dtx and $grp based on a location in the BIG 3-D window */
static int cmd_locate_dtx( ClientData client_data, Tcl_Interp *interp,
                      int argc, char *argv[] )
{
  int x, y;
  int result;
  int display_index[1], group_index[1];
  char *dtx_string;
  char *cmd;
  int code;
  Window w;

  if (!arg_check( interp, "vis5d_locate_dtx", argc, 3, 3 )) {
    return TCL_ERROR;
  }
  /* WLH 15 Oct 98 */
  w = (Window) strtoul( argv[1], NULL, 10 );
  x = atoi( argv[2] );
  y = atoi( argv[3] );

  result = vis5d_locate_dtx(w, x, y, display_index);
  if (result==0) {
    /* derive group from display */
    vis5d_get_display_group(*display_index, group_index);
    if (*group_index > -1) {
      sprintf( interp->result, "%d %d", *display_index, *group_index );
    }
    else {
      sprintf( interp->result, "%d %d", *display_index, 0 );
    }
    return TCL_OK;
  }
  else {
    return error_check( interp, "vis5d_locate_dtx", result );
  }
}

/* WLH 30 Sept 98 */
/* return $ctx, $dtx and $grp based on ContextName */
static int cmd_name_ctx(ClientData client_data, Tcl_Interp *interp,
                      int argc, char *argv[] )
{
  int result;
  int context_index[1], display_index[1], group_index[1];
  char *ctx_string;
  char *cmd;
  int code;

  if (!arg_check( interp, "vis5d_name_ctx", argc, 1, 1 )) {
    return TCL_ERROR;
  }
  result = vis5d_name_ctx(argv[1], context_index);
  if (result==0) {
    /* derive display from data */
    vis5d_get_ctx_display_index(*context_index, display_index);
    if (*display_index > -1) {
      /* derive group from display */
      vis5d_get_display_group(*display_index, group_index);
      if (*group_index > -1) {
        sprintf( interp->result, "%d %d %d", *context_index,
                 *display_index, *group_index );
      }
      else {
        sprintf( interp->result, "%d %d %d", *context_index,
                 *display_index, 0 );
      }
    }
    else {
      sprintf( interp->result, "%d %d %d", *context_index, 0, 0 );
    }
    return TCL_OK;
  }
  else {
    return error_check( interp, "vis5d_name_ctx", result );
  }
}

/* WLH 1 Oct 98 */
/* iconify display given by arg, or current if arg is -1 */
static int cmd_iconify(ClientData client_data, Tcl_Interp *interp,
                       int argc, char *argv[] ) {
  int display_index, Kurrant, result;

  if (!arg_check( interp, "vis5d_iconify", argc, 1, 1 )) {
    return TCL_ERROR;
  }
  display_index = atoi( argv[1] );
  if (display_index < 0) {
    get_current_display( &Kurrant);
    iconify(Kurrant);
  }
  else {
    iconify(display_index);
  }
  return TCL_OK;
}

/* WLH 1 Oct 98 */
/* deiconify display given by arg, or current if arg is -1 */
static int cmd_deiconify(ClientData client_data, Tcl_Interp *interp,
                         int argc, char *argv[] ) {
  int display_index, Kurrant, result;

  if (!arg_check( interp, "vis5d_deiconify", argc, 1, 1 )) {
    return TCL_ERROR;
  }
  display_index = atoi( argv[1] );
  if (display_index < 0) {
    get_current_display( &Kurrant);
    deiconify(Kurrant);
  }
  else {
    deiconify(display_index);
  }
  return TCL_OK;
}

/* WLH 7 Oct 98 */
/* set size of matrix of 3-D displays */
static int cmd_set_display_matrix(ClientData client_data, Tcl_Interp *interp,
                                  int argc, char *argv[] ) {
  int rows, columns, result;

  /* WLH 11 Nov 98 */
  int i;

  if (!arg_check( interp, "vis5d_set_display_matrix", argc, 2, 2 )) {
    return TCL_ERROR;
  }
  rows = atoi( argv[1] );
  columns = atoi( argv[2] );
  if (rows < 1 || rows > 4 || columns < 1 || columns > 4) {
    return TCL_ERROR;
  }
  set_display_matrix(rows, columns);
  /* MJK 11.19.98 */                     
  if (off_screen_rendering){
      return TCL_OK;
  }

  redo_the_gui = 1;

  /* WLH 11 Nov 98 */
  for (i=0; i<VIS5D_MAX_DPY_CONTEXTS; i++) redo_this_gui[i] = 1;

  return TCL_OK;
}

/* WLH 7 Oct 98 */
/* return size of matrix of 3-D displays */
static int cmd_get_display_matrix(ClientData client_data, Tcl_Interp *interp,
                                  int argc, char *argv[] ) {
  int rows, columns, result;

  if (!arg_check( interp, "vis5d_get_display_matrix", argc, 0, 0 )) {
    return TCL_ERROR;
  }
  get_display_matrix(&rows, &columns);
  sprintf( interp->result, "%d %d", rows, columns );
  return TCL_OK;
}

/* WLH 8 Oct 98 */
static int cmd_set_name_value(ClientData client_data, Tcl_Interp *interp,
                              int argc, char *argv[] )
{
   int result, nlen, vlen;
   NameValue *nv;
   if (!arg_check( interp, "vis5d_set_name_value", argc, 2, 2)){
      return TCL_ERROR;
   }
   nlen = strlen(argv[1]) + 1;
   vlen = strlen(argv[2]) + 1;
   nv = NameValueHead;
   while (nv != NULL) {
     if (strcmp(nv->name, argv[1]) == 0) {
       free(nv->value);
       nv->value = malloc(vlen);
       strcpy(nv->value, argv[2]);
       return TCL_OK;
     }
     nv = nv->next;
   }
   nv = calloc( 1, sizeof(NameValue) );
   nv->name = malloc(nlen);
   nv->value = malloc(vlen);
   strcpy(nv->name, argv[1]);
   strcpy(nv->value, argv[2]);
   nv->next = NameValueHead;
   NameValueHead = nv;
   return TCL_OK;
}

/* WLH 8 Oct 98 */
static int cmd_get_name_value(ClientData client_data, Tcl_Interp *interp,
                              int argc, char *argv[] )
{
   int result, nlen;
   NameValue *nv;
   if (!arg_check( interp, "vis5d_get_name_value", argc, 1, 1)){
      return TCL_ERROR;
   }
   nlen = strlen(argv[1]) + 1;
   nv = NameValueHead;
   while (nv != NULL) {
     if (strcmp(nv->name, argv[1]) == 0) {
       sprintf( interp->result, "%s", nv->value );
       return TCL_OK;
     }
     nv = nv->next;
   }
   sprintf( interp->result, "" );
   return TCL_OK;
}


/* MJK 12.01.98 */
static int cmd_link_slices( ClientData client_data, Tcl_Interp *interp,
                          int argc, char *argv[] )
{
   int result;
   int index1, index2, varflag, what1, which1, what2, which2;

   if (!arg_check( interp, "vis5d_link_slices", argc, 6, 6 )) {
      return TCL_ERROR;
   }

   index1 = atoi(argv[1]);
   index2 = atoi(argv[4]);

   varflag = 0;

   if (strcmp(argv[2],"VIS5D_HSLICE")==0) {
      what1 = VIS5D_HSLICE;
      varflag = 1;
   }
   else if (strcmp(argv[2],"VIS5D_VSLICE")==0) {
      what1 = VIS5D_VSLICE;
      varflag = 1;
   }
   else if (strcmp(argv[2],"VIS5D_CHSLICE")==0) {
      what1 = VIS5D_CHSLICE;
      varflag = 1;
   }
   else if (strcmp(argv[2],"VIS5D_CVSLICE")==0) {
      what1 = VIS5D_CVSLICE;
      varflag = 1;
   }
   else if (strcmp(argv[2],"VIS5D_HWIND")==0) {
      what1 = VIS5D_HWIND;
   }
   else if (strcmp(argv[2],"VIS5D_VWIND")==0) {
      what1 = VIS5D_VWIND;
   }
   else if (strcmp(argv[2],"VIS5D_HSTREAM")==0) {
      what1 = VIS5D_HSTREAM;
   }
   else if (strcmp(argv[2],"VIS5D_VSTREAM")==0) {
      what1 = VIS5D_VSTREAM;
   }
   else {
      interp->result = "error in vis5d_link_slices: bad constant";
      return TCL_ERROR;
   }

   if (varflag) {
      which1 = varnum( index1, argv[3] );
   }
   else {
      which1 = atoi( argv[3] );
   }


   varflag = 0;

   if (strcmp(argv[5],"VIS5D_HSLICE")==0) {
      what2 = VIS5D_HSLICE;
      varflag = 1;
   }
   else if (strcmp(argv[5],"VIS5D_VSLICE")==0) {
      what2 = VIS5D_VSLICE;
      varflag = 1;
   }
   else if (strcmp(argv[5],"VIS5D_CHSLICE")==0) {
      what2 = VIS5D_CHSLICE;
      varflag = 1;
   }
   else if (strcmp(argv[5],"VIS5D_CVSLICE")==0) {
      what2 = VIS5D_CVSLICE;
      varflag = 1;
   }
   else if (strcmp(argv[5],"VIS5D_HWIND")==0) {
      what2 = VIS5D_HWIND;
   }
   else if (strcmp(argv[5],"VIS5D_VWIND")==0) {
      what2 = VIS5D_VWIND;
   }
   else if (strcmp(argv[5],"VIS5D_HSTREAM")==0) {
      what2 = VIS5D_HSTREAM;
   }
   else if (strcmp(argv[5],"VIS5D_VSTREAM")==0) {
      what2 = VIS5D_VSTREAM;
   }
   else {
      interp->result = "error in vis5d_link_slices: bad constant";
      return TCL_ERROR;
   }

   if (varflag) {
      which2 = varnum( index2, argv[6] );
   }
   else {
      which2 = atoi( argv[6] );
   }

   result = vis5d_link_slices (index1, what1, which1, index2, what2, which2);
   return error_check( interp, "vis5d_link_slices", result );
}


/* MJK 12.01.98 */
static int cmd_unlink_slice( ClientData client_data, Tcl_Interp *interp,
                          int argc, char *argv[] )
{
   int result;
   int index, varflag, what, which;

   if (!arg_check( interp, "vis3d_unlink_slice", argc, 3, 3 )) {
      return TCL_ERROR;
   }

   index = atoi(argv[1]);

   varflag = 0;

   if (strcmp(argv[2],"VIS5D_HSLICE")==0) {
      what = VIS5D_HSLICE;
      varflag = 1;
   }
   else if (strcmp(argv[2],"VIS5D_VSLICE")==0) {
      what = VIS5D_VSLICE;
      varflag = 1;
   }
   else if (strcmp(argv[2],"VIS5D_CHSLICE")==0) {
      what = VIS5D_CHSLICE;
      varflag = 1;
   }
   else if (strcmp(argv[2],"VIS5D_CVSLICE")==0) {
      what = VIS5D_CVSLICE;
      varflag = 1;
   }
   else if (strcmp(argv[2],"VIS5D_HWIND")==0) {
      what = VIS5D_HWIND;
   }
   else if (strcmp(argv[2],"VIS5D_VWIND")==0) {
      what = VIS5D_VWIND;
   }
   else if (strcmp(argv[2],"VIS5D_HSTREAM")==0) {
      what = VIS5D_HSTREAM;
   }
   else if (strcmp(argv[2],"VIS5D_VSTREAM")==0) {
      what = VIS5D_VSTREAM;
   }
   else {
      interp->result = "error in vis5d_unlink_slice: bad constant";
      return TCL_ERROR;
   }

   if (varflag) {
      which = varnum( index, argv[3] );
   }
   else {
      which = atoi( argv[3] );
   }

   result = vis5d_unlink_slice (index, what, which);
   return error_check( interp, "vis5d_unlink_slice", result );
}




static void register_vis5d_gui_commands( Tcl_Interp *interp )
{

#ifdef GUI_H
#define REGISTER( NAME, FUNC ) Tcl_CreateCommand(interp,NAME,FUNC,NULL,NULL)

   REGISTER( "vis5d_gui_set_mouse_mode", cmd_set_mouse_mode );
   REGISTER( "vis5d_gui_get_animate",cmd_get_animate );
   REGISTER( "vis5d_gui_set_animate",cmd_set_animate );
   REGISTER( "vis5d_gui_set_reverse_background", cmd_set_reverse_background );
   REGISTER( "vis5d_gui_set_title", cmd_set_title );
   REGISTER( "vis5d_gui_set_margins", cmd_set_margins );

#undef REGISTER
#endif
}


/*
 * Register/extend Tcl with the Vis5D API commands.
 */
static void register_api_commands( Tcl_Interp *interp )
{
#define REGISTER( NAME, FUNC ) Tcl_CreateCommand(interp,NAME,FUNC,NULL,NULL)

   REGISTER( "vis5d_initialize", cmd_initialize );
   REGISTER( "vis5d_terminate", cmd_terminate );
   REGISTER( "vis5d_workers", cmd_workers );
   REGISTER( "vis5d_do_work", cmd_do_work );
   REGISTER( "vis5d_get_image_formats", cmd_get_image_formats );

   /* Context init functions */
   REGISTER( "vis5d_alloc_data_context", cmd_alloc_data_context );
   REGISTER( "vis5d_alloc_irregular_data_context", cmd_alloc_irregular_data_context );
   REGISTER( "vis5d_alloc_context", cmd_alloc_context );
   REGISTER( "vis5d_alloc_display_context", cmd_alloc_display_context );
   REGISTER( "vis5d_reset_display_context", cmd_reset_display_context );
   REGISTER( "vis5d_destroy_data_context", cmd_destroy_data_context );
   REGISTER( "vis5d_destroy_display_context", cmd_destroy_display_context );
   REGISTER( "vis5d_init_begin", cmd_init_begin );
   REGISTER( "vis5d_init_display_values", cmd_init_display_values);
   REGISTER( "vis5d_init_data_end", cmd_init_data_end );
   REGISTER( "vis5d_init_irregular_data_end", cmd_init_irregular_data_end );
   REGISTER( "vis5d_init_window", cmd_init_window );
   REGISTER( "vis5d_init_sndwindow", cmd_init_sndwindow );
   REGISTER( "vis5d_map_sndwindow", cmd_map_sndwindow );
   REGISTER( "vis5d_unmap_sndwindow", cmd_unmap_sndwindow );
   REGISTER( "vis5d_open_gridfile", cmd_open_gridfile );
   REGISTER( "vis5d_open_recordfile", cmd_open_recordfile );
   REGISTER( "vis5d_init_map", cmd_init_map );
   REGISTER( "vis5d_init_samescale", cmd_init_samescale );
   REGISTER( "vis5d_init_topo", cmd_init_topo );

   /* MJK 12.02.98 */
   REGISTER( "vis5d_init_clock", cmd_init_clock );
   REGISTER( "vis5d_set_probe_vars", cmd_set_probe_vars );


   REGISTER( "vis5d_init_texture", cmd_init_texture );
   REGISTER( "vis5d_init_firstarea", cmd_init_firstarea );
   REGISTER( "vis5d_init_sequence", cmd_init_sequence );
   REGISTER( "vis5d_init_log", cmd_init_log );
   REGISTER( "vis5d_init_box", cmd_init_box );
   REGISTER( "vis5d_init_memory", cmd_init_memory );
   REGISTER( "vis5d_init_irregular_memory", cmd_init_irregular_memory );
   REGISTER( "vis5d_init_path", cmd_init_path );
   REGISTER( "vis5d_init_projection", cmd_init_projection );
   REGISTER( "vis5d_init_vertical", cmd_init_vertical );

   /* Time functions */
   REGISTER( "vis5d_get_numtimes", cmd_get_numtimes );
   REGISTER( "vis5d_get_ctx_numtimes", cmd_get_ctx_numtimes );
   REGISTER( "vis5d_get_itx_numtimes", cmd_get_itx_numtimes );
   REGISTER( "vis5d_get_dtx_numtimes", cmd_get_dtx_numtimes );
   REGISTER( "vis5d_get_grp_numtimes", cmd_get_grp_numtimes );
   REGISTER( "vis5d_get_time_stamp", cmd_get_time_stamp );
   REGISTER( "vis5d_get_ctx_time_stamp", cmd_get_ctx_time_stamp );
   REGISTER( "vis5d_get_itx_time_stamp", cmd_get_itx_time_stamp );
   REGISTER( "vis5d_get_dtx_time_stamp", cmd_get_dtx_time_stamp );
   REGISTER( "vis5d_set_time_stamp", cmd_set_time_stamp );
   REGISTER( "vis5d_set_ctx_time_stamp", cmd_set_ctx_time_stamp );
   REGISTER( "vis5d_get_ctx_timestep", cmd_get_ctx_timestep );
   REGISTER( "vis5d_get_itx_timestep", cmd_get_itx_timestep );
   REGISTER( "vis5d_get_timestep", cmd_get_timestep );
   REGISTER( "vis5d_get_dtx_timestep", cmd_get_dtx_timestep );
   REGISTER( "vis5d_get_grp_timestep", cmd_get_grp_timestep );
   REGISTER( "vis5d_set_timestep", cmd_set_timestep );
   REGISTER( "vis5d_set_dtx_timestep", cmd_set_dtx_timestep );
   REGISTER( "vis5d_set_grp_timestep", cmd_set_grp_timestep );

   /* Variable functions */
   REGISTER( "vis5d_get_numvars", cmd_get_ctx_numvars );
   REGISTER( "vis5d_get_ctx_numvars", cmd_get_ctx_numvars );
   REGISTER( "vis5d_get_itx_numvars", cmd_get_itx_numvars );
   REGISTER( "vis5d_get_var_name", cmd_get_ctx_var_name );
   REGISTER( "vis5d_get_ctx_var_name", cmd_get_ctx_var_name );
   REGISTER( "vis5d_get_itx_var_name", cmd_get_itx_var_name );
   REGISTER( "vis5d_get_var_units", cmd_get_var_units );
   REGISTER( "vis5d_get_var_type", cmd_get_var_type );
   REGISTER( "vis5d_get_var_range", cmd_get_ctx_var_range );
   REGISTER( "vis5d_get_ctx_var_range", cmd_get_ctx_var_range );
   REGISTER( "vis5d_get_itx_var_range", cmd_get_itx_var_range );
   REGISTER( "vis5d_set_var_range", cmd_set_var_range );
   REGISTER( "vis5d_set_grp_var_values", cmd_set_grp_var_values );
   REGISTER( "vis5d_set_legends", cmd_set_legends );
   REGISTER( "vis5d_get_sound_vars_and_owners", cmd_get_sound_vars_and_owners);
   REGISTER( "vis5d_get_sound_vars", cmd_get_sound_vars);   
   REGISTER( "vis5d_set_sound_vars_and_owners", cmd_set_sound_vars_and_owners);
   REGISTER( "vis5d_set_sound_vars", cmd_set_sound_vars);
   REGISTER( "vis5d_get_wind_vars_and_owners", cmd_get_wind_vars_and_owners );
   REGISTER( "vis5d_get_wind_vars", cmd_get_wind_vars );
   REGISTER( "vis5d_set_wind_vars_and_owners", cmd_set_wind_vars_and_owners );
   REGISTER( "vis5d_set_wind_vars", cmd_set_wind_vars );


   /* file commands */
   REGISTER( "vis5d_get_v5dfilename", cmd_get_v5dfilename );
   REGISTER( "vis5d_load_v5dfile", cmd_load_v5dfile );
   REGISTER( "vis5d_load_irregular_v5dfile", cmd_load_irregular_v5dfile );
   REGISTER( "vis5d_save_to_v5dfile", cmd_save_to_v5dfile );
   REGISTER( "vis5d_set_user_data_flag", cmd_set_user_data_flag );
   REGISTER( "vis5d_set_user_flags", cmd_set_user_flags );


   /* group functions */
   REGISTER( "vis5d_set_display_group", cmd_set_display_group );
   REGISTER( "vis5d_get_display_group", cmd_get_display_group );
  
   /* display functions */
   REGISTER( "vis5d_assign_display_to_data", cmd_assign_display_to_data );
   REGISTER( "vis5d_assign_display_to_irregular_data", cmd_assign_display_to_irregular_data );

   /* Grid functions */
/* MJK 6.9.99 */
   REGISTER( "vis5d_get_grid_value", cmd_get_grid_value );
   REGISTER( "vis5d_get_grid_rows", cmd_get_grid_rows );
   REGISTER( "vis5d_get_grid_columns", cmd_get_grid_columns );
   REGISTER( "vis5d_get_grid_levels", cmd_get_grid_levels );
   REGISTER( "vis5d_get_ctx_grid_rows", cmd_get_ctx_grid_rows );
   REGISTER( "vis5d_get_dtx_grid_rows", cmd_get_dtx_grid_rows );
   REGISTER( "vis5d_get_ctx_grid_columns", cmd_get_ctx_grid_columns );
   REGISTER( "vis5d_get_dtx_grid_columns", cmd_get_dtx_grid_columns );
   REGISTER( "vis5d_get_ctx_grid_levels", cmd_get_ctx_grid_levels );
   REGISTER( "vis5d_get_dtx_grid_levels", cmd_get_dtx_grid_levels );
   REGISTER( "vis5d_set_dtx_grid_rows", cmd_set_dtx_grid_rows );
   REGISTER( "vis5d_set_dtx_grid_columns", cmd_set_dtx_grid_columns );
   REGISTER( "vis5d_set_dtx_grid_levels", cmd_set_dtx_grid_levels );
   REGISTER( "vis5d_verylarge_mode", cmd_verylarge_mode );

   /* Map projection and vertical coordinate system */
   REGISTER( "vis5d_get_projection", cmd_get_ctx_projection );
   REGISTER( "vis5d_get_ctx_projection", cmd_get_ctx_projection );
   REGISTER( "vis5d_get_dtx_projection", cmd_get_dtx_projection );
   REGISTER( "vis5d_get_ctx_vertical", cmd_get_ctx_vertical );
   REGISTER( "vis5d_get_vertical", cmd_get_ctx_vertical );
   REGISTER( "vis5d_get_dtx_vertical", cmd_get_dtx_vertical );
   REGISTER( "vis5d_set_dtx_projection_and_vertical", cmd_set_dtx_projection_and_vertical );

   /* Topography and map functions */
   REGISTER( "vis5d_reset_topo_colors", cmd_reset_topo_colors );
   REGISTER( "vis5d_set_topo_color_var_and_owner", cmd_set_topo_color_var_and_owner );
   REGISTER( "vis5d_set_topo_color_var", cmd_set_topo_color_var );
   REGISTER( "vis5d_check_topo", cmd_check_topo );
   REGISTER( "vis5d_check_map", cmd_check_map );
   REGISTER( "vis5d_set_flatmap_level", cmd_set_flatmap_level);
   REGISTER( "vis5d_get_flatmap_level", cmd_get_flatmap_level);



   REGISTER( "vis5d_check_texture", cmd_check_texture );
   REGISTER( "vis5d_get_topo_range", cmd_get_topo_range );
   REGISTER( "vis5d_set_topo_base", cmd_set_topo_base );

   /* New variables */
   REGISTER( "vis5d_make_clone_variable", cmd_make_clone_variable );
   REGISTER( "vis5d_compute_ext_func", cmd_compute_ext_func );
   REGISTER( "vis5d_make_expr_var", cmd_make_expr_var );

   /* Rendering functions */
   REGISTER( "vis5d_signal_redraw", cmd_signal_redraw );
   REGISTER( "vis5d_check_redraw", cmd_check_redraw );
   REGISTER( "vis5d_draw_frame", cmd_draw_frame );
   REGISTER( "vis5d_draw_3d_only", cmd_draw_3d_only );
   REGISTER( "vis5d_draw_sounding_only", cmd_draw_sounding_only );
   REGISTER( "vis5d_swap_frame", cmd_swap_frame );
   REGISTER( "vis5d_invalidate_dtx_frames", cmd_invalidate_dtx_frames );
   REGISTER( "vis5d_set_pointer", cmd_set_pointer );
   REGISTER( "vis5d_set_view", cmd_set_view );
   REGISTER( "vis5d_set_view_scales", cmd_set_view_scales );
   REGISTER( "vis5d_get_view", cmd_get_view );
   REGISTER( "vis5d_get_view_scales", cmd_get_view_scales );
   REGISTER( "vis5d_set_camera", cmd_set_camera );
   REGISTER( "vis5d_get_camera", cmd_get_camera );

   REGISTER( "vis5d_graphics_mode", cmd_graphics_mode );
   REGISTER( "vis5d_enable_graphics", cmd_enable_graphics );
   REGISTER( "vis5d_enable_irregular_graphics", cmd_enable_irregular_graphics );
   REGISTER( "vis5d_set_volume", cmd_set_volume );
   REGISTER( "vis5d_set_volume_and_owner", cmd_set_volume_and_owner );
   REGISTER( "vis5d_get_volume", cmd_get_volume );
   REGISTER( "vis5d_get_volume_and_owner", cmd_get_volume_and_owner );
   REGISTER( "vis5d_set_color", cmd_set_color );
   REGISTER( "vis5d_get_color", cmd_get_color );
   REGISTER( "vis5d_load_color_table", cmd_load_color_table );
   REGISTER( "vis5d_set_color_table_entry", cmd_set_color_table_entry );
   REGISTER( "vis5d_set_color_table_params", cmd_set_color_table_params );
   REGISTER( "vis5d_alpha_mode", cmd_alpha_mode );
   REGISTER( "vis5d_linewidth", cmd_linewidth );
   REGISTER( "vis5d_font", cmd_font );

   /* new_clipping funcs */
   REGISTER( "vis5d_set_hclip", cmd_set_hclip );
   REGISTER( "vis5d_set_vclip", cmd_set_vclip );
   REGISTER( "vis5d_get_vclip", cmd_get_vclip );
   REGISTER( "vis5d_get_hclip", cmd_get_hclip );
   REGISTER( "vis5d_set_clip_mode", cmd_set_clip_mode );
   REGISTER( "vis5d_get_clip_mode", cmd_get_clip_mode );

   REGISTER( "vis5d_get_num_of_ctxs_in_display", cmd_get_num_of_ctxs_in_display);
   REGISTER( "vis5d_get_num_of_dtxs_in_group", cmd_get_num_of_dtxs_in_group);
   REGISTER( "vis5d_get_num_of_itxs_in_display", cmd_get_num_of_itxs_in_display);

   /* Text plots */
   REGISTER( "vis5d_set_text_plot", cmd_set_text_plot);
   REGISTER( "vis5d_get_text_plot", cmd_get_text_plot);
   REGISTER( "vis5d_make_text_plot", cmd_make_text_plot);
   REGISTER( "vis5d_set_textplot_color_status", cmd_set_textplot_color_status);
   REGISTER( "vis5d_get_textplot_color_status", cmd_get_textplot_color_status);

   /* Isosurface, Slice, and Trajectory Functions */
   REGISTER( "vis5d_make_isosurface", cmd_make_isosurface );
   REGISTER( "vis5d_set_isosurface", cmd_set_isosurface );
   REGISTER( "vis5d_get_isosurface", cmd_get_isosurface );
   REGISTER( "vis5d_set_isosurface_color_var_and_owner", cmd_set_isosurface_color_var_and_owner );
   REGISTER( "vis5d_make_hslice", cmd_make_hslice );
   REGISTER( "vis5d_set_hslice", cmd_set_hslice );
   REGISTER( "vis5d_get_hslice", cmd_get_hslice );
   REGISTER( "vis5d_make_vslice", cmd_make_vslice );
   REGISTER( "vis5d_set_vslice", cmd_set_vslice );
   REGISTER( "vis5d_get_vslice", cmd_get_vslice );
   REGISTER( "vis5d_make_chslice", cmd_make_chslice );
   REGISTER( "vis5d_set_chslice", cmd_set_chslice );
   REGISTER( "vis5d_get_chslice", cmd_get_chslice );
   REGISTER( "vis5d_make_cvslice", cmd_make_cvslice );
   REGISTER( "vis5d_set_cvslice", cmd_set_cvslice );
   REGISTER( "vis5d_get_cvslice", cmd_get_cvslice );
   REGISTER( "vis5d_make_hwindslice", cmd_make_hwindslice );
   REGISTER( "vis5d_set_hwindslice", cmd_set_hwindslice );
   REGISTER( "vis5d_get_hwindslice", cmd_get_hwindslice );
   REGISTER( "vis5d_make_vwindslice", cmd_make_vwindslice );
   REGISTER( "vis5d_set_vwindslice", cmd_set_vwindslice );
   REGISTER( "vis5d_get_vwindslice", cmd_get_vwindslice );
   REGISTER( "vis5d_make_hstreamslice", cmd_make_hstreamslice );
   REGISTER( "vis5d_set_hstreamslice", cmd_set_hstreamslice );
   REGISTER( "vis5d_get_hstreamslice", cmd_get_hstreamslice );
   REGISTER( "vis5d_make_vstreamslice", cmd_make_vstreamslice );
   REGISTER( "vis5d_set_vstreamslice", cmd_set_vstreamslice );
   REGISTER( "vis5d_get_vstreamslice", cmd_get_vstreamslice );
   REGISTER( "vis5d_print_traj", cmd_print_traj );
   REGISTER( "vis5d_make_traj", cmd_make_traj );
   REGISTER( "vis5d_set_traj", cmd_set_traj );
   REGISTER( "vis5d_get_traj", cmd_get_traj );
   REGISTER( "vis5d_set_trajectory_color_var", cmd_set_trajectory_color_var );
   REGISTER( "vis5d_set_trajectory_color_var_and_owner", cmd_set_trajectory_color_var_and_owner );
   REGISTER( "vis5d_delete_last_traj", cmd_delete_last_traj );
   REGISTER( "vis5d_delete_traj_set", cmd_delete_traj_set );

   REGISTER( "vis5d_make_timestep_graphics", cmd_make_timestep_graphics );
   REGISTER( "vis5d_free_graphics", cmd_free_graphics );


   /* Text Label Functions */
   REGISTER( "vis5d_make_label", cmd_make_label );
   REGISTER( "vis5d_delete_label", cmd_delete_label );

   /* 3-D Cursor Functions */
   REGISTER( "vis5d_set_cursor", cmd_set_cursor );
   REGISTER( "vis5d_get_cursor", cmd_get_cursor );

   /* Logo functions */
   REGISTER( "vis5d_set_logo_size", cmd_set_logo_size );

   /* 3-D window functions */
   REGISTER( "vis5d_save_window", cmd_save_window );
   REGISTER( "vis5d_save_snd_window", cmd_save_snd_window );
   REGISTER( "vis5d_print_window", cmd_print_window );
   REGISTER( "vis5d_print_snd_window", cmd_print_snd_window );
   REGISTER( "vis5d_resize_3d_window", cmd_resize_3d_window);
   REGISTER( "vis5d_moveresize_BIG_window", cmd_moveresize_BIG_window);
   REGISTER( "vis5d_moveresize_3d_window", cmd_moveresize_3d_window);

   /* 3-D View Functions */
   REGISTER( "vis5d_set_matrix", cmd_set_matrix );
   REGISTER( "vis5d_get_matrix", cmd_get_matrix );
   REGISTER( "vis5d_set_ortho_view", cmd_set_ortho_view );

   /* Coordinate Conversion Functions */
   REGISTER( "vis5d_xyzPRIME_to_gridPRIME", cmd_xyzPRIME_to_gridPRIME );
   REGISTER( "vis5d_xyz_to_grid", cmd_xyz_to_grid );
   REGISTER( "vis5d_grid_to_xyz", cmd_grid_to_xyz );
   REGISTER( "vis5d_gridPRIME_to_xyzPRIME", cmd_gridPRIME_to_xyzPRIME );
   REGISTER( "vis5d_xyz_to_geo", cmd_xyz_to_geo );
   REGISTER( "vis5d_xyzPRIME_to_geo", cmd_xyzPRIME_to_geo );
   REGISTER( "vis5d_geo_to_xyz", cmd_geo_to_xyz );
   REGISTER( "vis5d_geo_to_xyzPRIME", cmd_geo_to_xyzPRIME );
   REGISTER( "vis5d_grid_to_geo", cmd_grid_to_geo );
   REGISTER( "vis5d_gridPRIME_to_geo", cmd_gridPRIME_to_geo );
   REGISTER( "vis5d_rowcol_to_latlon", cmd_rowcol_to_latlon );
   REGISTER( "vis5d_rowcolPRIME_to_latlon", cmd_rowcolPRIME_to_latlon );
   REGISTER( "vis5d_latlon_to_rowcol", cmd_latlon_to_rowcol );
   REGISTER( "vis5d_latlon_to_rowcolPRIME", cmd_latlon_to_rowcolPRIME );
   REGISTER( "vis5d_geo_to_grid", cmd_geo_to_grid );
   REGISTER( "vis5d_geo_to_gridPRIME", cmd_geo_to_gridPRIME );

   /* Save and Restore Functions */
   REGISTER( "vis5d_save", cmd_save );
   REGISTER( "vis5d_restore", cmd_restore );

   REGISTER( "vis5d_sleep", cmd_sleep );

   /* Functions useful for controlling Vis5D via -pipe */
   REGISTER( "vis5d_locate_dtx", cmd_locate_dtx );
   REGISTER( "vis5d_name_ctx", cmd_name_ctx );

   /* WLH 1 Oct 98 */
   REGISTER( "vis5d_iconify", cmd_iconify );
   REGISTER( "vis5d_deiconify", cmd_deiconify );

   /* WLH 7 Oct 98 */
   REGISTER( "vis5d_set_display_matrix", cmd_set_display_matrix );
   REGISTER( "vis5d_get_display_matrix", cmd_get_display_matrix );
   REGISTER( "vis5d_get_context_name", cmd_get_context_name );

   /* MJK 2.22.99 */
   REGISTER( "vis5d_resize_contour_font", cmd_resize_contour_font );

   /* WLH 8 Oct 98 */
   REGISTER( "vis5d_get_font_height", cmd_get_font_height );
   REGISTER( "vis5d_get_font", cmd_get_font );
   REGISTER( "vis5d_set_name_value", cmd_set_name_value );
   REGISTER( "vis5d_get_name_value", cmd_get_name_value );

   /* MJK 12.01.98 */
   REGISTER( "vis5d_link_slices", cmd_link_slices);
   REGISTER( "vis5d_unlink_slice", cmd_unlink_slice);

   /* MJK 12.04.98 */
   REGISTER( "vis5d_enable_sfc_graphics", cmd_enable_sfc_graphics );


/*
   REGISTER( "vis5d_", cmd_ );
*/
#undef REGISTER
}


/*
 * Execute a Tcl Vis5D script from a file.
 * Input:  index - the context index
 *         filename - name of Tcl script
 * Return:  1 = success, 0 = error
 */
int execute_script( int index, char *filename )
{
   Tcl_Interp *interp;
   int code;
   char setup_cmd[100];

   /* WLH 11 Nov 98 */
   int i;

   /* WLH 12 Nov 98 */
   int chowmany, cwhichones[VIS5D_MAX_CONTEXTS];

   redo_the_gui = 0;

   /* WLH 11 Nov 98 */
   for (i=0; i<VIS5D_MAX_DPY_CONTEXTS; i++) redo_this_gui[i] = 0;

   /* Create an interpreter */
   interp = Tcl_CreateInterp();
  
   register_api_commands( interp );
   register_vis5d_gui_commands( interp );
/* WLH 12 Nov 98
   sprintf( setup_cmd, "set ctx %d", index );
   code = Tcl_Eval( interp, setup_cmd );
*/
   /* WLH 12 Nov 98 */
   sprintf( setup_cmd, "set dtx %d", index );
   code = Tcl_Eval( interp, setup_cmd );
   vis5d_get_num_of_ctxs_in_display( index, &chowmany, cwhichones);
   if (chowmany > 0) {
     sprintf( setup_cmd, "set ctx %d", cwhichones[0] );
     code = Tcl_Eval( interp, setup_cmd );
   }
   else {
     sprintf( setup_cmd, "set ctx 0" );
     code = Tcl_Eval( interp, setup_cmd );
   }

   /* Execute the script */
   code = Tcl_EvalFile( interp, filename );
   if (interp->result && interp->result[0]) {
      printf("Tcl_EvalFile result: %s\n", interp->result);
   }

   Tcl_DeleteInterp( interp );

   if (code==TCL_OK && !redo_the_gui) {
      return 1;
   }
   if (code==TCL_OK && redo_the_gui) {
      return -1;
   }
   else {
      return 0;
   }
}



int interpret( int index )
{
   Tcl_Interp *interp = NULL;
   char setup_cmd[100];
   int code;
   FILE *f;

   /* WLH 11 Nov 98 */
   int i;

   /* WLH 12 Nov 98 */
   int chowmany, cwhichones[VIS5D_MAX_CONTEXTS];

   /* Create an interpreter */
   interp = Tcl_CreateInterp();


   register_api_commands( interp );
   register_vis5d_gui_commands( interp );
/* WLH 12 Nov 98
   sprintf( setup_cmd, "set ctx %d", index );
   code = Tcl_Eval( interp, setup_cmd );
*/
   /* WLH 12 Nov 98 */
   sprintf( setup_cmd, "set dtx %d", index );
   code = Tcl_Eval( interp, setup_cmd );
   vis5d_get_num_of_ctxs_in_display( index, &chowmany, cwhichones);
   if (chowmany > 0) {
     sprintf( setup_cmd, "set ctx %d", cwhichones[0] );
     code = Tcl_Eval( interp, setup_cmd );
   }
   else {
     sprintf( setup_cmd, "set ctx 0" );
     code = Tcl_Eval( interp, setup_cmd );
   }

   /* Execute the TCL_STARTUP_FILE, if it exists. */
   f = fopen( TCL_STARTUP_FILE, "r" );
   if (f) {
      fclose(f);
      code = Tcl_EvalFile( interp, TCL_STARTUP_FILE );
      if (code!=TCL_OK) {
         if (interp->result && interp->result[0]) {
            printf("Error executing %s: %s\n",
                   TCL_STARTUP_FILE, interp->result);
         }
         else {
            printf("Couldn't execute %s\n", TCL_STARTUP_FILE);
         }
      }
   }

   printf("Vis5D interpreter: type exit when finished.\n");
#ifndef TCL
   printf("Note: using built-in mini-interpreter, not real Tcl.\n");
#endif
   redo_the_gui = 0;

   /* WLH 11 Nov 98 */
   for (i=0; i<VIS5D_MAX_DPY_CONTEXTS; i++) redo_this_gui[i] = 0;


   while (1) {
      char cmd[1000];
      printf("vis5d> ");
      fflush(stdout);
      cmd[0] = 0;
      gets( cmd );
      if (feof(stdin) || strcmp(cmd,"exit")==0) {
         printf("Interpreter closed.\n");
         break;
      }
      (void) Tcl_Eval( interp, cmd );
      if (interp->result && interp->result[0]) {
         printf("%s\n", interp->result);
      }
   }

   Tcl_DeleteInterp( interp );

   if (redo_the_gui){
      return -1;
   }
   else{
      return 1;
   }
}
