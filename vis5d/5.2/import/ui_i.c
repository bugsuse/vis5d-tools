/* ui.c */

/*
 * Text-based user interface for Vis5D data import program.
 */


#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "analyze.h"
#include "file.h"
#include "grid.h"
#include "output.h"
#include "projlist.h"
#include "select.h"
#include "tokenize.h"
#include "v5d.h"



static int MaxNl;
extern char *path;


extern start_vis5d( char *filename );


/*
 * Print help information
 */
static void do_help( char *subject )
{
#define P printf
   if (subject[0]==0) {
      /* no subject */
      P("Help is available on the following topics:\n");
      P("   intro list keep omit read make visualize exit\n");
      P("   info rows columns levels projection vertical\n");
      P("Type 'help <topic>'\n");
   }
   else if (strcmp(subject,"intro")==0) {
      P("Introduction:\n");
      P("   This program is used for converting various formats of 3-D\n");
      P("gridded data files to the v5d format used by Vis5D.  In addition\n");
      P("to data conversion, resampling and remapping of data is performed.\n");
      P("The general approach to using this program is:\n");
      P("  1. Start the program, specifying one or more input files on\n");
      P("     the command line.\n");
      P("  2. Use the list command to get a summary of the input data\n");
      P("  3. Use the keep and omit commands to select which grids to\n");
      P("     put in the output file.\n");
      P("  4. Use the info command to see the parameters of the output file.\n");
      P("  5. Adjust the output file parameters with the rows, columns,\n");
      P("     levels, projection, and vertical commands.\n");
      P("  6. Use the make command to generate the output file, OR ...\n");
      P("     Use the visualize command to generate and visualize the output file.\n");
      P("  7. exit.\n");
      P("If you only want to convert one file to v5d format without any\n");
      P("resampling or remapping you would probably only do steps 1, 6 and 7.\n");
      P("Currently, the following input file formats are understood:\n");
      P("   McIDAS GR3D and GRID\n");
      P("   Vis5D v5d and comp5d\n");
      P("   EPA MM4 and RADM\n");
      P("   GRADS\n");
      P("   UW-AOS model VIS files\n");
   }
   else if (strcmp(subject,"list")==0) {
      P("The list command is used to show information about the grids\n");
      P("currently in memory.  There are several list options:\n");
      P("   list         lists all the grids\n");
      P("   list vars    lists all variables\n");
      P("   list times   lists all timesteps\n");
      P("   list proj    lists all projections\n");
      P("   list vcs     lists all vertical coord systems\n");
      P("List entries preceded by a * indicate that the grid will be\n");
      P("included in the output file.\n");
   }
   else if (strcmp(subject,"keep")==0) {
      P("The keep command is used to indicate which input grids should be\n");
      P("included in the output file.  There are many options:\n");
      P("  keep all                    include all grids in output\n");
/*
      P("  keep grid <n>               include grid number n in output\n");
      P("  keep grids <n> <m>          include grids n through m in output\n");
*/
      P("  keep var <name>             include grids by variable <name>\n");
      P("  keep time <yyddd> <hhmmss>  include grids by the specified time\n");
      P("  keep times <yyddd> <hhmmss> <yyddd> <hhmmss>\n");
      P("                              include grids between the given times\n");
      P("  keep timestep <n>           include the nth timestep\n");
      P("  keep timesteps <n> <m>      include the nth through mth timesteps\n");
      P("  keep proj <n>               keep grids projection number <n>\n");
      P("  keep vcs <n>                keep grids with VCS number <n>\n");
      P("Use the list command to see which grids are marked as 'keep'\n");
   }
   else if (strcmp(subject,"omit")==0) {
      P("The omit command is used to indicate which input grids should NOT\n");
      P("be included in the output file.  There are many options:\n");
      P("  omit all                    omit all grids from output\n");
/*
      P("  omit grid <n>               omit grid number n from output\n");
      P("  omit grids <n> <m>          omit grids n through m from output\n");
*/
      P("  omit var <name>             omit grids by variable <name>\n");
      P("  omit time <yyddd> <hhmmss>  omit grids by the specified time\n");
      P("  omit times <yyddd> <hhmmss> <yyddd> <hhmmss>\n");
      P("                              omit grids between the given times\n");
      P("  omit timestep <n>           omit the nth timestep\n");
      P("  omit timesteps <n> <m>      omit the nth through mth timesteps\n");
      P("  omit proj <n>               omit grids projection number <n>\n");
      P("  omit vcs <n>                omit grids with VCS number <n>\n");
      P("Use the list command to see which grids are marked as 'keep'\n");
   }
   else if (strcmp(subject,"make")==0) {
      P("The make command causes the output file to be generated.  All\n");
      P("grids marked as 'keep' (*) will be included in the output file.\n");
      P("The input grids will be resampled and remapped to the dimensions\n");
      P("and projection as reported by the 'info' command.  There is one\n");
      P("argument to the make command:\n");
      P("  make <filename>      where filename is a .v5d file.\n");
   }
   else if (strcmp(subject,"visualize")==0) {
      P("The visualize command causes the output file to be generated and\n");
      P("Vis5D to be started.  All grids marked as 'keep' (*) will be included\n");
      P("in the output file.  The input grids will be resampled and remapped\n");
      P("to the dimensions and projection as reported by the 'info' command.\n");
      P("There is one optional argument to the make command:\n");
      P("  visualize <filename>      where filename is a .v5d file.\n");
   }
   else if (strcmp(subject,"exit")==0) {
      P("The exit command is used to exit this program.  The command quit\n");
      P("does the same thing.\n");
   }
   else if (strcmp(subject,"read")==0) {
      P("The read command is used to read another input file.  The initial\n");
      P("input files are specified on the command line when this program\n");
      P("is started.  The read command has one argument:\n");
      P("  read <filename>      where filename is the file to read\n");
   }
   else if (strcmp(subject,"info")==0) {
      P("The info command shows the parameters of the output file including\n");
      P("the size of the output grid, the map projection and the vertical\n");
      P("coordinate system.  The info command has no arguments.  The\n");
      P("default output file parameters are obtained from the first input\n");
      P("file.  The following commands are used to change the output file\n");
      P("parameters:\n");
      P("   rows, columns, levels, projection, vertical\n");
   }
   else if (strcmp(subject,"rows")==0) {
      P("The rows command is used to specify how many rows are to be in\n");
      P("the output file.  Syntax:\n");
      P("   rows <n>     specify n rows in output file\n");
      P("Use the 'info' command to see the current value for rows.\n");
/*
      P("The map projection will be automatically adjusted to the new rows.\n");
*/
   }
   else if (strncmp(subject,"col",3)==0) {
      P("The columns command is used to specify how may columns are to be\n");
      P("in the output file.  Syntax:\n");
      P("   columns <n>  specify n columns in output file\n");
      P("Use the 'info' command to see the current value for columns.\n");
/*
      P("The map projection will be automatically adjusted to the new value.\n");
*/
   }
   else if (strncmp(subject,"lev",3)==0) {
      P("The levels command is used to specify how many levels are to be\n");
      P("in the output file either for all variables or for individual\n");
      P("variables.  There are two syntaxes:\n");
      P("  levels <n>          set number of grid levels for all variables\n");
      P("  levels <var> <n>    set number of grid levels for one variable\n");
      P("Use the 'info' command to see the current grid level values.\n");
   }
   else if (strncmp(subject,"proj",4)==0) {
      P("The projection command is used to specify the projection of the\n");
      P("output file.  There are several options:\n");
      P("  proj <n>      Use projection number <n> (see list proj)\n");
      P("  proj generic <north> <west> <rowinc> <colinc>\n");
      P("  proj linear <north> <west> <rowinc> <colinc>\n");
      P("  proj lambert <lat1> <lat2> <polerow> <polecol> <longitude> <colinc>\n");
      P("  proj polar <centlat> <centlon> <centrow> <centcol> <colinc>\n");
   }
   else if (strcmp(subject,"vertical")==0) {
      P("The vertical command is used to specify the vertical coordinate\n");
      P("system (VCS) of the output file.  There are several options:\n");
      P("  vert <n>      Use VCS number <n> (see list vcs)\n");
      P("  vert generic <bottom> <increment>\n");
      P("  vert equal <bottom> <increment>\n");
      P("  vert unequal [<height 1> <height 2> ...]\n");
      P("  vert pressure [<pressure 1> <pressure 2> ...]\n");
      P("In the last 2 cases, if the height or pressure values are not specified\n");
      P("on the command line you will be prompted to enter them one at a time.\n");
   }


   /* etc.. */

   else {
      P("Unknown command: %s\n", subject);
   }
   P("\n");

#undef P
}



/*
 * List the grids
 * Input:  list - the list of grids
 *         what - 0 = list all grids
 *                1 = list variables
 *                2 = list timesteps
 *                3 = list projections
 *                4 = list VCSs
 */
static void do_list( struct grid_db *db, int what )
{
   char *varnames[MAXVARS];
   int time[MAXTIMES], date[MAXTIMES];
   int i, numvars, numtimes;

   assert( what>=0 && what<=4 );

   if (db->NumGrids==0) {
      printf("No grids have been read.\n");
      return;
   }

   switch (what) {
      case 0:
         /* list all grids */
         print_grid_list( db );
         break;
      case 1:
         /* list all variables */
         printf("Variables:\n");
         for (i=0;i<db->NumVars;i++) {
            if (db->VarSelected[i]) {
               printf("* ");
            }
            else {
               printf("  ");
            }
            printf("%2d: %s\n", i+1, db->VarNames[i] );
         }
         printf("*=include variable in output file\n");
         break;
      case 2:
         /* list all timesteps */
         printf("Timesteps:\n");
         for (i=0;i<db->NumTimes;i++) {
            if (db->TimeSelected[i]) {
               printf("* ");
            }
            else {
               printf("  ");
            }
            printf("%2d: %05d %06d\n", i+1,
                   db->DateStamp[i], db->TimeStamp[i] );
         }
         printf("*=include timestep in output file\n");
         break;
      case 3:
         /* list all projections */
         printf("Map projections:\n");
         print_projection_list( db );
         break;
      case 4:
         /* list all VCSs */
         printf("Vertical Coordinate Systems: (VCS)\n");
         print_vcs_list( db );
         break;
   }
}




static void do_select_var( struct grid_db *db, char *varname, int flag )
{
   int var;
   char tmpvar[100];
   int i;

   for (var=0;var<db->NumVars;var++) {
      /* tmpvar = VarNames[var] without trailing spaces */
      strcpy( tmpvar, db->VarNames[var] );
      i = strlen(tmpvar) - 1;
      while (i>0 && tmpvar[i]==' ') {
         tmpvar[i] = 0;
         i--;
      }
      if (strcmp(tmpvar,varname)==0) {
         break;
      }
   }
   if (var==db->NumVars) {
      printf("No such variable\n");
      return;
   }

   select_variable( db, var, flag );
}



/*
 * Select grids according to timestep number
 * Input:  list - grid list
 *         ts0, ts1 - range of timestep numbers (starting at 1)
 *         flag - 1=keep, 0=omit
 */
static void do_select_timesteps( struct grid_db *db, int ts0, int ts1,
                                 int flag )
{
   int time;

   for (time=ts0;time<=ts1;time++) {
      select_time( db, time-1, flag );
   }
}



static void do_select_times( struct grid_db *db,
                             int date0, int time0,
                             int date1, int time1,
                             int flag )
{
   int i;

   for (i=0;i<db->NumTimes;i++) {
      if ( (db->DateStamp[i]>date0
             || (db->DateStamp[i]==date0 && db->TimeStamp[i]>=time0))
          && (db->DateStamp[i]<date1
             || (db->DateStamp[i]==date1 && db->TimeStamp[i]<=time1)) ) {
         select_time( db, i, flag );
      }
   }
}



#ifdef LEAVEOUT
/*
 * Select a range of grids by number [n..m] starting at 1.
 */
static void do_select_grids( db, n, m, flag )
struct grid_db *db;
int n, m;
int flag;
{
   struct grid_info *g;
   int time, var;

   g = nth_grid( GridList, n );

   if (table_find_gridptr( Table, g, &time, &var )) {
      table_mark_grid( Table, time, var, g, ALL_BITS, flag );
   }
}
#endif


static int compute_maxnl( v5dstruct *v5d )
{
   int i, maxnl = 0;
   for (i=0;i<MAXVARS;i++) {
      if (v5d->Nl[i]>maxnl) {
         maxnl = v5d->Nl[i];
      }
   }
   return maxnl;
}



static void do_read( struct grid_db *db, v5dstruct *v5dout, char *filein )
{
   char **gridtext;
   int i, oldnumgrids;
   char filename[1000];

   if (filein[0]==0) {
      printf("No filename given!\n");
      return;
   }
   if (filein[0] == '/' || path == NULL) {
     strcpy(filename, filein);
   }
   else {
     int len;
     strcpy(filename, path);
     /* add a trailing slash to path if there isn't one already */
     len = strlen(filename);
     if (len>0 && filename[len-1]!='/') {
       strcat(filename, "/");
     }
     strcat(filename, filein);
   }

   printf("Read: %s\n", filename );

   oldnumgrids = db->NumGrids;

   get_file_info( filename, db );

   analyze_grids( db );

   select_all( db, ALL_BITS, 1 );

   if (oldnumgrids==0 && db->NumGrids>0) {
      /* Now we can setup defaults for the output file */
      setup_defaults( db, v5dout, 1,1,1 );
      MaxNl = compute_maxnl( v5dout );
   }
}



/*
 * Make the output file.
 * Input:  db - the grid database
 *         v5dout - describes the output file
 *         filename - name of output .v5d file
 */
static void do_make( struct grid_db *db, v5dstruct *v5dout, char *filein )
{
   int average = 0;
   int maxnl = MaxNl;
   int compressmode = 1;
   char filename[1000];

   if (filein[0]==0) {
      printf("No filename given!\n");
      return;
   }
   if (filein[0] == '/' || path == NULL) {
     strcpy(filename, filein);
   }
   else {
     int len;
     strcpy(filename, path);
     /* add a trailing slash to path if there isn't one already */
     len = strlen(filename);
     if (len>0 && filename[len-1]!='/') {
       strcat(filename, "/");
     }
     strcat(filename, filein);
   }

   printf("Making %s...\n", filename );

   make_output_file( db, v5dout, filename, maxnl, average, compressmode );

   printf("Done\n");
}



/*
 * Make the output file and start Vis5D.
 * Input:  db - the grid database
 *         v5dout - describes the output file
 *         name - name of output .v5d file
 */
static void do_go( struct grid_db *db, v5dstruct *v5dout, char *name )
{
   int average = 0;
   int maxnl = MaxNl;
   int compressmode = 1;
   char filein[1000], filename[1000];

   if (name == NULL) {
     char *user = getenv("USER");
     if (user == NULL) user = "user";
     strcpy(filein, user);
     strcat(filein, ".v5d");
   }
   else {
     strcpy(filein, name);
   }
   if (filein[0] == '/' || path == NULL) {
     strcpy(filename, filein);
   }
   else {
     int len;
     strcpy(filename, path);
     /* add a trailing slash to path if there isn't one already */
     len = strlen(filename);
     if (len>0 && filename[len-1]!='/') {
       strcat(filename, "/");
     }
     strcat(filename, filein);
   }

   printf("Making %s...\n", filename );

   make_output_file( db, v5dout, filename, maxnl, average, compressmode );

   start_vis5d(filename);
   exit(0);
}



/*
 * Parse the given command and execute it.
 * Input:  command - the command string
 *         db - the grid database
 *         v5dout - describes the output file
 */
static void parse_command( char *command, struct grid_db *db,
                           v5dstruct *v5dout )
{
   char **token;
   int n;

   /* convert command to tokens */
   token = tokenize( command, &n );

   if (n==0) {
      free_tokens( token );
      return;
   }

   /*
    * Process the command
    */
   if (strcmp(token[0],"exit")==0 || strcmp(token[0],"quit")==0) {
      exit(0);
   }
   else if (strcmp(token[0],"help")==0) {
      if (n==1) {
         do_help("");
      }
      else {
         do_help( token[1] );
      }
   }
   else if (strcmp(token[0],"list")==0) {
      if (n==1) {
         do_list( db, 0 );
      }
      else {
         if (strncmp(token[1],"var",3)==0) {
            do_list( db, 1 );
         }
         else if (strncmp(token[1],"time",4)==0) {
            do_list( db, 2 );
         }
         else if (strncmp(token[1],"proj",4)==0) {
            do_list( db, 3 );
         }
         else if (strncmp(token[1],"vcs",4)==0) {
            do_list( db, 4 );
         }
         else {
            printf("Error: bad argument to list: %s\n", token[1] );
         }
      }
   }
   else if (strcmp(token[0],"rows")==0) {
      /* Set number of output rows */
      int nr = atoi( token[1] );
      if (nr>1) {
#ifdef LEAVEOUT
         /* adjust projection to match new rows */
         switch (v5dout->Projection) {
            case 0:
            case 1:  /* rectilinear*/
               v5dout->ProjArgs[2] *= (float) v5dout->Nr / (float) nr;
               break;
            case 2:  /* lambert */
               v5dout->ProjArgs[2] *= (float) v5dout->Nr / (float) nr;
               break;
            case 3:  /* polar stereographic */
               v5dout->ProjArgs[2] *= (float) v5dout->Nr / (float) nr;
               break;
            default:
               /* ??? */
               ;
         }
#endif
         v5dout->Nr = nr;
      }
   }
   else if (strncmp(token[0],"col",3)==0) {
      /* Set number of output columns */
      int nc = atoi( token[1] );
      if (nc>1) {
#ifdef LEAVEOUT
         /* adjust projection */
         switch (v5dout->Projection) {
            case 0:
            case 1:  /* rectilinear */
               v5dout->ProjArgs[3] *= (float) v5dout->Nc / (float) nc;
               break;
            case 2:  /* lambert */
               v5dout->ProjArgs[3] *= (float) v5dout->Nc / (float) nc;
               v5dout->ProjArgs[5] *= (float) v5dout->Nc / (float) nc;
               break;
            case 3:  /* polar stereographic */
               v5dout->ProjArgs[3] *= (float) v5dout->Nc / (float) nc;
               v5dout->ProjArgs[4] *= (float) v5dout->Nc / (float) nc;
               break;
            default:
               /* ??? */
               ;
         }
#endif
         v5dout->Nc = nc;
      }
   }
   else if (strncmp(token[0],"lev",3)==0) {
      /* Set number of output levels */
      int levels, i;
      if (n!=2) {
         printf("Error: one argument expected\n");
      }
      else {
         levels = atoi( token[1] );
         if (levels>1 && levels<=MAXLEVELS) {
            MaxNl = levels;
         }
      }

#ifdef LEAVEOUT
      if (n==2) {
         int maxnl, i;
         levels = atoi( token[1] );
         /* find current maxnl */
         maxnl = v5dout->Nl[0];
         for (i=1;i<v5dout->NumVars;i++) {
            if (maxnl<v5dout->Nl[i])  maxnl = v5dout->Nl[i];
         }
         /* adjust vert. coord. sys. */
         switch (v5dout->VerticalSystem) {
            case 0:
            case 1:
               v5dout->VertArgs[1] *= (float) maxnl / (float) levels;
               break;
            case 2:
               /* compute more height values */
               if (levels>maxnl) {
                  float delta = v5dout->VertArgs[maxnl-1]
                              - v5dout->VertArgs[maxnl-2];
                  for (i=maxnl;i<levels;i++) {
                     v5dout->VertArgs[i] = v5dout->VertArgs[i-1] + delta;
                  }
               }
               break;
            default:
               /* nothing??? */
               ;
         }
         /* set number of levels for all vars */
         for (i=0;i<v5dout->NumVars;i++) {
            v5dout->Nl[i] = levels;
         }
      }
      else if (n==3) {
         /* number of levels for one particular variable */
         levels = atoi( token[2] );
         for (i=0;i<v5dout->NumVars;i++) {
            if (strcmp(token[1],v5dout->VarName[i])==0) {
               v5dout->Nl[i] = levels;
            }
         }
      }
#endif
   }
   else if (strcmp(token[0],"info")==0) {
      int i;
      printf("Output file parameters:\n");
      printf("  Rows: %d\n", v5dout->Nr );
      printf("  Columns: %d\n", v5dout->Nc );
      printf("  Max Levels: %d\n", MaxNl );
/*
      for (i=0;i<v5dout->NumVars;i++) {
         printf("    %-10s %d\n", v5dout->VarName[i], v5dout->Nl[i] );
      }
*/
      printf("  Projection: ");
      switch (v5dout->Projection) {
         case 0:
            printf("Generic\n");
            printf("    North Bound: %g\n", v5dout->ProjArgs[0] );
            printf("    West Bound: %g\n", v5dout->ProjArgs[1] );
            printf("    Row Increment: %g\n", v5dout->ProjArgs[2] );
            printf("    Column Increment: %g\n", v5dout->ProjArgs[3] );
            break;
         case 1:
            printf("Linear\n");
            printf("    North Bound: %g deg\n", v5dout->ProjArgs[0] );
            printf("    West Bound: %g deg\n", v5dout->ProjArgs[1] );
            printf("    Row Increment: %g deg\n", v5dout->ProjArgs[2] );
            printf("    Column Increment: %g deg\n", v5dout->ProjArgs[3] );
            break;
         case 2:
            printf("Lambert Conformal\n");
            printf("    Standard Latitude 1: %g deg\n", v5dout->ProjArgs[0] );
            printf("    Standard Latitude 2: %g deg\n", v5dout->ProjArgs[1] );
            printf("    North/South Pole row: %g\n", v5dout->ProjArgs[2] );
            printf("    North/South Pole column: %g\n", v5dout->ProjArgs[3] );
            printf("    Central Longitude: %g\n", v5dout->ProjArgs[4] );
            printf("    Column increment: %g km\n", v5dout->ProjArgs[5] );
            break;
         case 3:
            printf("Polar Stereographic\n");
            printf("    Center Latitude: %g deg\n", v5dout->ProjArgs[0] );
            printf("    Center Longitude: %g deg\n", v5dout->ProjArgs[1] );
            printf("    Center Grid Row: %g deg\n", v5dout->ProjArgs[2] );
            printf("    Center Grid Column: %g deg\n", v5dout->ProjArgs[3] );
            printf("    Column increment: %g km\n", v5dout->ProjArgs[4] );
            break;
         default:
            printf("Undefined\n");
      }
      printf("  Vertical Coordinate System: ");
      switch (v5dout->VerticalSystem) {
         case 0:
            printf("Generic\n");
            printf("    BottomBound: %f\n", v5dout->VertArgs[0] );
            printf("    LevInc: %f\n", v5dout->VertArgs[1] );
            break;
         case 1:
            printf("Equally spaced km\n");
            printf("    BottomBound: %f\n", v5dout->VertArgs[0] );
            printf("    LevInc: %f\n", v5dout->VertArgs[1] );
            break;
         case 2:
            printf("Unequally spaced km\n");
            {
               for (i=0;i<MaxNl;i++) {
                  printf("    Level %d:  %.3f km\n", i+1, v5dout->VertArgs[i] );
               }
            }
            break;
         case 3:
            printf("Unequally spaced mb\n");
            {
               for (i=0;i<MaxNl;i++) {
                  printf("    Level %d:  %.3f km\n", i+1,
                         height_to_pressure(v5dout->VertArgs[i]) );
               }
            }
         default:
            printf("Undefined\n");
      }
   }
   else if (strcmp(token[0],"read")==0) {
      /* read another input file */
      if (n==2) {
         do_read( db, v5dout, token[1] );
      }
      else {
         printf("Error: missing filename argument to read command\n");
      }
   }
   else if (strcmp(token[0],"keep")==0 || strcmp(token[0],"omit")==0) {
      /* Keep/Omit Grids */
      if (n>1) {
         int flag;
         if (strcmp(token[0],"keep")==0) {
            flag = 1;
         }
         else {
            flag = 0;
         }
         if (strcmp(token[1],"all")==0) {
            select_all( db, ALL_BITS, 1 );
         }
#ifdef LEAVEOUT
         else if (strncmp(token[1],"grid",4)==0) {
            if (n>3) {
               /* select a range */
               do_select_grids( atoi(token[2]), atoi(token[3]), flag );
            }
            else {
               /* select one */
               do_select_grids( atoi(token[2]), atoi(token[2]), flag );
            }
         }
#endif
         else if (strncmp(token[1],"var",3)==0) {
            if (n<3) {
               printf("Error: must specify a variable name\n");
            }
            else {
               do_select_var( db, token[2], flag );
            }
         }
         else if (strncmp(token[1],"timestep",8)==0) {
            if (n==3) {
               do_select_timesteps( db, atoi(token[2]), atoi(token[2]), flag );
            }
            else if (n==4) {
               do_select_timesteps( db, atoi(token[2]), atoi(token[3]), flag );
            }
            else {
               printf("Error: wrong number of arguments\n");
               printf("Either:  keep/omit timestep <timestep>\n");
               printf("    or:  keep/omit timesteps <timestep> <timestep>\n");
            }
         }
         else if (strncmp(token[1],"time",4)==0) {
            if (n==4) {
               do_select_times( db, atoi(token[2]), atoi(token[3]),
                                    atoi(token[2]), atoi(token[3]), flag );
            }
            else if (n==6) {
               do_select_times( db, atoi(token[2]), atoi(token[3]),
                                    atoi(token[4]), atoi(token[5]), flag );
            }
            else {
               printf("Error: wrong number of arguments\n");
               printf("Either:  keep/omit time <date> <time>\n");
               printf("    or:  keep/omit times <date> <time>  <date> <time>\n");
            }
         }
         else if (strncmp(token[1],"proj",4)==0) {
            if (n==3) {
               int p = atoi(token[2])-1;
               if (p<0 || p>=db->NumProj) {
                  printf("Error: bad projection number.\n");
               }
               else {
                  select_projection( db, p, flag );
               }
            }
            else {
               printf("Error: specify a projection number.\n");
            }
         }
         else if (strncmp(token[1],"vcs",3)==0) {
            if (n==3) {
               int v = atoi(token[2])-1;
               if (v<0 || v>=db->NumVcs) {
                  printf("Error: bad VCS number.\n");
               }
               else {
                  select_vcs( db, v, flag );
               }
            }
            else {
               printf("Error: specify a projection number\n");
            }
         }
         else {
            printf("Error: bad argument to keep/omit: %s\n", token[1] );
         }
      }
      else {
         printf("Keep/omit what?  Try 'help keep' or 'help omit'\n");
      }
   }
   else if (strcmp(token[0],"make")==0) {
      if (n<2) {
         printf("Error: missing filename argument to make\n");
      }
      else {
         do_make( db, v5dout, token[1] );
      }
   }
   else if (strcmp(token[0],"visualize")==0) {
      if (n<2) {
         do_go( db, v5dout, NULL );
      }
      else {
         do_go( db, v5dout, token[1] );
      }
   }
   else if (strncmp(token[0],"proj",4)==0) {
      if (n==1) {
         printf("Error: missing arguments to projection\n");
      }
      else if (n==2) {
         int p = atoi(token[1])-1;
         if (p<0 || p>=db->NumProj) {
            printf("Error: bad projection number\n");
         }
         else {
            v5dout->Projection = db->ProjList[p]->Kind;
            memcpy( v5dout->ProjArgs, db->ProjList[p]->Args,
                    MAXPROJARGS*sizeof(float) );
         }
      }
      else if (strcmp(token[1],"generic")==0) {
         if (n<6) {
            printf("Error: missing arguments to generic projection\n");
            printf("Syntax:  projection generic <north> <west> <rowinc> <colinc>\n");
         }
         else {
            v5dout->Projection = 0;
            v5dout->ProjArgs[0] = atof( token[2] );
            v5dout->ProjArgs[1] = atof( token[3] );
            v5dout->ProjArgs[2] = atof( token[4] );
            v5dout->ProjArgs[3] = atof( token[5] );
         }
      }
      else if (strcmp(token[1],"linear")==0) {
         if (n<6) {
            printf("Error: missing arguments to linear projection\n");
            printf("Syntax:  projection linear <north> <west> <rowinc> <colinc>\n");
         }
         else {
            v5dout->Projection = 1;
            v5dout->ProjArgs[0] = atof( token[2] );
            v5dout->ProjArgs[1] = atof( token[3] );
            v5dout->ProjArgs[2] = atof( token[4] );
            v5dout->ProjArgs[3] = atof( token[5] );
         }
      }
      else if (strcmp(token[1],"lambert")==0) {
         if (n<8) {
            printf("Error: missing arguments to Lambert Conformal projection\n");
            printf("Syntax:  projection lambert <lat1> <lat2> <polerow> <polecol> <longitude> <column-increment>\n");
         }
         else {
            v5dout->Projection = 2;
            v5dout->ProjArgs[0] = atof( token[2] );
            v5dout->ProjArgs[1] = atof( token[3] );
            v5dout->ProjArgs[2] = atof( token[4] );
            v5dout->ProjArgs[3] = atof( token[5] );
            v5dout->ProjArgs[4] = atof( token[6] );
            v5dout->ProjArgs[5] = atof( token[7] );
         }
      }
      else if (strcmp(token[1],"polar")==0) {
         if (n<7) {
            printf("Error: missing arguments to Polar Stereographic projection\n");
            printf("Syntax:  projection polar <lat> <lon> <row> <col> <column-increment>\n");
         }
         else {
            v5dout->Projection = 3;
            v5dout->ProjArgs[0] = atof( token[2] );
            v5dout->ProjArgs[1] = atof( token[3] );
            v5dout->ProjArgs[2] = atof( token[4] );
            v5dout->ProjArgs[3] = atof( token[5] );
            v5dout->ProjArgs[4] = atof( token[6] );
         }
      }
      else {
         printf("Error: bad argument to projection: %s\n", token[1] );
      }
   }
   else if (strncmp(token[0],"vert",4)==0) {
      if (n==1) {
         printf("Error: missing arguments to vertical\n");
      }
      else if (strcmp(token[1],"generic")==0) {
         if (n<4) {
            printf("Error: missing arguments to generic V.C.S.\n");
            printf("Syntax:  vertical generic <bottom> <increment>\n");
         }
         else {
            v5dout->VerticalSystem = 0;
            v5dout->VertArgs[0] = atof( token[2] );
            v5dout->VertArgs[1] = atof( token[3] );
         }
      }
      else if (strncmp(token[1],"equal",5)==0) {
         if (n<4) {
            printf("Error: missing arguments to Equally-spaced KM V.C.S.\n");
            printf("Syntax:  vertical equal <bottom> <increment>\n");
         }
         else {
            v5dout->VerticalSystem = 1;
            v5dout->VertArgs[0] = atof( token[2] );
            v5dout->VertArgs[1] = atof( token[3] );
         }
      }
      else if (strcmp(token[1],"unequal")==0) {
         v5dout->VerticalSystem = 2;
         if (n==2) {
            int i;
            printf("Enter height values in km:\n");
            for (i=0;i<MaxNl;i++) {
               printf("Level %d: ", i+1);
               scanf("%f", &v5dout->VertArgs[i] );
            }
         }
         else {
            int i;
            for (i=2;i<n;i++) {
               v5dout->VertArgs[i-2] = atof( token[i] );
            }
         }
      }
      else if (strcmp(token[1],"pressure")==0) {
         v5dout->VerticalSystem = 3;
         if (n==2) {
            int i;
            float pressure;
            printf("Enter pressure values in mb:\n");
            for (i=0;i<MaxNl;i++) {
               printf("Level %d: ", i+1);
               scanf("%f", &pressure );
               v5dout->VertArgs[i] = pressure_to_height(pressure);
            }
         }
         else {
            int i;
            for (i=2;i<n;i++) {
               v5dout->VertArgs[i-2] = pressure_to_height(atof( token[i] ));
            }
         }
      }
      else if (n==2) {
         int v = atoi(token[1])-1;
         if (v<0 || v>=db->NumVcs) {
            printf("Error:  bad VCS number.\n");
         }
         else {
            v5dout->VerticalSystem = db->VcsList[v]->Kind;
            memcpy( v5dout->VertArgs, db->VcsList[v]->Args,
                    MAXVERTARGS * sizeof(float) );
         }
      }
      else {
         printf("Error: missing arguments to vertical command\n");
      }
   }

   /* etc... */

   else {
      printf("Unknown command: %s\n", token[0]);
   }

   free_tokens( token );
}




/*
 * The main loop for command-line interface:
 *     1. get a command (from std input)
 *     2. parse/execute command
 *     3. goto 1
 * Input:  list - list of grids read during initialization
 *         v5dout - a v5dstruct which describes the output file.
 */
void ui_loop( struct grid_db *db, v5dstruct *v5dout )
{
   char command[2000];
   int script_mode;

   /* check if reading commands from a script */
   script_mode = !isatty(fileno(stdin));

   select_all( db, ALL_BITS, 1 );

   MaxNl = compute_maxnl( v5dout );

   while (1) {
      if (script_mode) {
         gets( command );
         printf(">> %s\n", command );
      }
      else {
         /* print prompt */
         printf(">> ");
         fflush( stdout );

         /* get command string */
         gets( command );
      }

      if (command[0]!=0 && command[0]!='#') {
         parse_command( command, db, v5dout );
      }

      /* this obscurity makes dbx nicer: */
      command[0] = 0;
   }
}

