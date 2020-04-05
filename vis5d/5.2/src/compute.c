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


/* VIS-5D Expression evaluator */

/* **** WLH: eventually parallelize like analysis.c **** */


#include <ctype.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <X11/Xlib.h>
#include <string.h>
#include "api.h"
#include "globals.h"
#include "grid.h"
#include "memory.h"



/* function types:
   (order must be consisent with the numargs string) */
#define SQRT_FUNC 0
#define EXP_FUNC 1
#define LOG_FUNC 2
#define SIN_FUNC 3
#define COS_FUNC 4
#define TAN_FUNC 5
#define ATAN_FUNC 6
#define ABS_FUNC 7
#define MIN_FUNC 8
#define MAX_FUNC 9
static char *funcnames[] =
  { "sqrt", "exp", "log", "sin", "cos", "tan", "atan", "abs",
    "min", "max",
    "SQRT", "EXP", "LOG", "SIN", "COS", "TAN", "ATAN", "ABS",
    "MIN", "MAX" }; /* constant */
static int functypes[] =
  { SQRT_FUNC, EXP_FUNC, LOG_FUNC, SIN_FUNC, COS_FUNC, TAN_FUNC,
    ATAN_FUNC, ABS_FUNC, MIN_FUNC, MAX_FUNC,
    SQRT_FUNC, EXP_FUNC, LOG_FUNC, SIN_FUNC, COS_FUNC, TAN_FUNC,
    ATAN_FUNC, ABS_FUNC, MIN_FUNC, MAX_FUNC }; /* constant */
#define NUMFUNCS  (sizeof funcnames / sizeof funcnames[0])
static int numargs[] = /* order must match ._FUNC defines */
  { 1, 1, 1, 1, 1, 1, 1, 1, 2, 2 }; /* constant */

/* token types: */
#define END_TOKEN 0
#define NUM_TOKEN 1
#define OP_TOKEN 2
#define NAME_TOKEN 3
#define ERROR_TOKEN 4

/* operator types:
   (order must be consisent with opchars string) */
#define ADD_OP 0
#define SUB_OP 1
#define MUL_OP 2
#define DIV_OP 3
#define LEFT_PAREN_OP 4
#define RIGHT_PAREN_OP 5
#define COMMA_OP 6
#define EQUALS_OP 7
static char opchars[] = "+-*/(),="; /* order must match ._OP defines */ /* constant */

#define POWER_OP 11
#define PUSH_VAR_OP 21
#define PUSH_NUM_OP 22
#define NEGATE_OP 23
#define FUNC_OP 24

#define MAXOPS 100

#define USETIME -1234 

struct compute_state {
  /* "program" for computing new variable */
  int curop; /* ointer into ops, args and nums */
  int ops[MAXOPS]; /* operator types (FUNC_OP, ADD_OP, SUB_OP, etc) */
  int args[MAXOPS]; /* index to variable for PUSH_VAR_OP, function type for FUNC_OP */
  int args2[MAXOPS]; /* this handles varowner info */
  int args3[MAXOPS]; /* this handles the time of the var */
  float nums[MAXOPS]; /* floating point number for PUSH_NUM_OP */
  /* list of variables appearing in expression */
  int varlist[MAXVARS];
  int varownerlist[MAXVARS];
  int numvars;
  /* "stack" for executing "program" */
  int stop; /* number of entries on stack */
  float *sgrid[MAXOPS]; /* points to grid */
};

int found_a_time;

/*
 * NOTES:
 *
 * Suppose we compute A = X + Y and then compute B = A + Z.  If we
 * change the expression for A and recompute it, should B be recomputed
 * automatically too?
 *
 * The call the compute_var in gui.c is commented out...
 *
 * When building the tree and resolving references to variable names,
 * use find_variable() to find the index of a variable by name.
 */



/*
 * Return the number of the named variable or -1 if not found.  If name
 * is NULL, return -1.
 */
static int find_variable( Context ctx, char *name )
{
   int var;

   if (!name)
      return -1;

   if ((strcmp(name,"TIME")==0 ||
       strcmp(name,"time")==0) &&
       found_a_time){
      found_a_time = 0;
      return USETIME;
   }
   for (var=0;var<ctx->NumVars;var++) {
      if (strcmp( ctx->VarName[var], name )==0) {
         return var;
      }
   }
   return -1;
}
static int find_variable_time( Display_Context dtx, char *name)
{
   char numbher[10];
   char othername[50];
   int yo, t, q, thenumber;

   found_a_time = 0;
   if (name[0] && name[1] && name[2] &&
       name[3] && name[4]){
      if ((name[0] == 't' &&
          name[1] == 'i' &&
          name[2] == 'm' &&
          name[3] == 'e' &&
          name[4] == '(') ||
          (name[0] == 'T' &&
          name[1] == 'I' &&
          name[2] == 'M' &&
          name[3] == 'E' &&
          name[4] == '(')){
         t = 0;
         while( name[t+5] != ',' && t < 10){
            name[t] = name[t+5];
            t++;
         }
         name[t] = 0;
         if (t == 0 || t == 10){
            return -1;
         }
         t = t + 6;
         q = 0;
         while( name[t+q] != ')' && q < 6){
            numbher[q] = name[t+q];
            q++;
         }
         if (q == 0 || q == 6){
            return -1;
         }
         numbher[q] = 0;
         thenumber = atoi(numbher);
         found_a_time = 1;
         return thenumber;
      }
   }
   return 0;
}
 
                    
   
static int find_variable_owner( Display_Context dtx, char *name)
{
   int i, ssec, vowner;
   char wstr[100];

   if (dtx->numofctxs == 1){
      vowner =  dtx->ctxpointerarray[0]->context_index;
   }
  
   ssec = 0;
   while(name[ssec] != 0){
      ssec++;
   }
   if (name[ssec-1] == '.'){
      wstr[0] = name[ssec];
      wstr[1] = 0;
      vowner = atoi( wstr );
      name[ssec-1] = 0;
      name[ssec] = 0;
   }
   else if (name[ssec-2] == '.'){
      wstr[0] = name[ssec-1];
      wstr[1] = name[ssec];
      wstr[2] = 0;
      vowner = atoi( wstr );
      name[ssec-2] = 0;
      name[ssec-1] = 0;
      name[ssec] = 0;
   }
   else{
      vowner =  dtx->ctxpointerarray[0]->context_index;
   }
   for (i = 0; i < dtx->numofctxs; i++){
      if (vowner == dtx->ctxpointerarray[i]->context_index){
         return vowner;
      }
   }
   return -1;
}


/*
 * Find next token in string *s.
 * Input:  s - pointer to string containing user's expression
 * Output:  s - pointer to string at character after token
 *          index - index of operator for OP_TOKEN type
 *          fval - value of number for NUM_TOKEN type
 *          name - variable name string for NAME_TOKEN type
 * Return:  token type
 */
int get_token( char **s, int *index, float *fval, char *name )
{
  char *ss;
  char *endp;
  int i;
  double strtod (const char *nptr, char **endptr);

  ss = *s;

  /* remove leading blanks and tabs */
  while (ss[0] == ' ' || ss[0] == '\t') ss++;

  /* check if token is end of string */
  if (ss[0] == 0) {
    *s = ss;
    return END_TOKEN;
  }

  /* check if token is an operator */
  if (ss[0] == '*' && ss[1] == '*') {
    *index = POWER_OP;
    *s = ss + 2;
    return OP_TOKEN;
  }
  for (i=0; opchars[i]!=0; i++) {
    if (ss[0] == opchars[i]) {
      *index = i;
      *s = ss + 1;
      return OP_TOKEN;
    }
  }

  /* check if token is a number */
  if (isdigit(ss[0]) || ss[0] == '.') {
    *fval = strtod(ss, &endp);
    if (ss != endp) {
      *s = endp;
      return NUM_TOKEN;
    }
  }

  /* check if it is Time(blah, blah) */
  if ((ss[0] == 't' &&
      ss[1] == 'i' &&
      ss[2] == 'm' &&
      ss[3] == 'e' &&
      ss[4] == '(' ) ||
      (ss[0] == 'T' &&
      ss[1] == 'I' &&
      ss[2] == 'M' &&
      ss[3] == 'E' &&
      ss[4] == '(')){
    i = 0;
    while (ss[0] != ')' && i < 21){
       name[i] = ss[0];
       i++;
       ss++;
    }
    name[i] = ')';
    name[i+1] = 0;
    ss++;
    *s = ss;
    if (i == 21 || i == 0){
      return ERROR_TOKEN;
    } 
    return NAME_TOKEN;
  }      

  /* check if token is a variable */
  if (isalpha(ss[0])) {
    for (i=0; (isalnum(ss[0]) || ss[0] == '\'' || ss[0] == '_') && i<99; i++, ss++) {
      name[i] = ss[0];
      name[i+1] = 0;
    }
    *s = ss;
    return NAME_TOKEN;
  }

  
 
  *s = ss;
  return ERROR_TOKEN;
}




static int get_exp0( Context ctx, struct compute_state *state,
                     char **s, char mess[100] );
static int get_exp1( Context ctx, struct compute_state *state,
                     char **s, char mess[100] );
static int get_exp2( Context ctx, struct compute_state *state,
                     char **s, char mess[100] );
static int get_exp3( Context ctx, struct compute_state *state,
                     char **s, char mess[100] );


/*
 * Expression syntax:
 *
 * exp3 = exp2
 *      = - exp2
 *      = exp2 + - exp2 + - ... + - exp2
 *      = - exp2 + - exp2 + - ... + - exp2
 * exp2 = exp1
 *      = exp1 * / exp1 * / ... * / exp1
 * exp1 = exp0
 *      = exp0 ** exp0
 * exp0 = var
 *      = num
 *      = ( exp3 )
 *      = name ( exp3 , ... , exp3 )
 *
 * exp3 terminated by , ) or END_TOKEN
 * exp2 terminated by + - , ) or END_TOKEN
 */

/*
 * parse an exp3 in string *s.
 * Input:  s - pointer to string containing expression
 * Output:  s - pointer to string at character after expression
 * Return:  0 if OK or -1 if error
 *
 * exp3 = exp2
 *      = - exp2
 *      = exp2 + - exp2 + - ... + - exp2
 *      = - exp2 + - exp2 + - ... + - exp2
 *
 * exp3 terminated by , ) or END_TOKEN
 */
static int get_exp3( Context ctx, struct compute_state *state,
                     char **s, char mess[100] )
{
  int type;
  int index;
  float fval;
  char name[100];
  char *ssave;
  int negate_flag;

  ssave = *s;
  type = get_token(s, &index, &fval, name);
  if (type == OP_TOKEN && index == SUB_OP) {
    negate_flag = 1;
  }
  else {
    negate_flag = 0;
    *s = ssave;
  }

  if (get_exp2(ctx, state, s, mess) < 0) return -1;

  if (negate_flag) {
    if (state->curop >= MAXOPS - 1) {
      strcpy(mess, "Error:  expression too long");
      return -1;
    }
    state->ops[state->curop] = NEGATE_OP;
    state->curop++;
  }

  while (1 == 1) {
    ssave = *s;
    type = get_token(s, &index, &fval, name);
    if (type == OP_TOKEN && (index == ADD_OP || index == SUB_OP)) {
      if (get_exp2(ctx, state, s, mess) < 0) return -1;

      if (state->curop >= MAXOPS - 1) {
        strcpy(mess, "Error:  expression too long");
        return -1;
      }
      state->ops[state->curop] = index;
      state->curop++;
    }
    else {
      *s = ssave;
      break;
    }
  }
  return 0;
}



/*
 * parse an exp2 in string *s.
 * Input:  s - pointer to string containing expression
 * Output:  s - pointer to string at character after expression
 * Return:  0 if OK or -1 if error
 *
 * exp2 = exp1
 *      = exp1 * / exp1 * / ... * / exp1
 *
 * exp2 terminated by + - , ) or END_TOKEN
 */
static int get_exp2( Context ctx, struct compute_state *state,
                     char **s, char mess[100] )
{
  int type;
  int index;
  float fval;
  char name[100];
  char *ssave;

  if (get_exp1(ctx, state, s, mess) < 0) return -1;

  while (1 == 1) {
    ssave = *s;
    type = get_token(s, &index, &fval, name);
    if (type == OP_TOKEN && (index == MUL_OP || index == DIV_OP)) {
      if (get_exp1(ctx, state, s, mess) < 0) return -1;

      if (state->curop >= MAXOPS - 1) {
        strcpy(mess, "Error:  expression too long");
        return -1;
      }
      state->ops[state->curop] = index;
      state->curop++;
    }
    else {
      *s = ssave;
      break;
    }
  }
  return 0;
}


/*
 * parse an exp1 in string *s.
 * Input:  s - pointer to string containing expression
 * Output:  s - pointer to string at character after expression
 * Return:  0 if OK or -1 if error
 *
 * exp1 = exp0
 *      = exp0 ** exp0
 *
 */
static int get_exp1( Context ctx, struct compute_state *state,
                     char **s, char mess[100] )
{
  int type;
  int index;
  float fval;
  char name[100];
  char *ssave;

  if (get_exp0(ctx, state, s, mess) < 0) return -1;

  ssave = *s;
  type = get_token(s, &index, &fval, name);
  if (type == OP_TOKEN && index == POWER_OP) {
    if (get_exp0(ctx, state, s, mess) < 0) return -1;

    if (state->curop >= MAXOPS - 1) {
      strcpy(mess, "Error:  expression too long");
      return -1;
    }
    state->ops[state->curop] = index;
    state->curop++;
  }
  else {
    *s = ssave;
  }
  return 0;
}



/*
 * parse an exp0 in string *s.
 * Input:  s - pointer to string containing expression
 * Output:  s - pointer to string at character after expression
 * Return:  0 if OK or -1 if error
 *
 * exp0 = var
 *      = num
 *      = ( exp3 )
 *      = name ( exp3 , ... , exp3 )
 */
static int get_exp0( Context ctx, struct compute_state *state,
                     char **s, char mess[100] )
{
  int type1, type2;
  int yo, index1, index2;
  float fval1, fval2;
  char name1[100], name2[100];
  Display_Context dtx;

  char *ssave1, *ssave2;
  int var, arg_count, func_index, i, flag;

  dtx = ctx->dpy_ctx;
  ssave1 = *s;
  type1 = get_token(s, &index1, &fval1, name1);
  if (type1 == END_TOKEN) {
    strcpy(mess, "Error:  syntax");
    *s = ssave1;
    return -1;
  }

  ssave2 = *s;
  type2 = get_token(s, &index2, &fval2, name2);

  if (type1 == NAME_TOKEN &&
      type2 == OP_TOKEN && index2 == LEFT_PAREN_OP) {

     /* must be exp = name ( exp , exp , ... , exp ) */

    func_index = -1;
    for (i=0; i<NUMFUNCS; i++) {
      if (strcmp(name1, funcnames[i]) == 0) {
        func_index = i;
        break;
      }
    }
    if (func_index < 0) {
      sprintf(mess, "Error:  illegal function name: %s", name1);
      return -1;
    }
    arg_count = 0;

    do {
      if (get_exp3(ctx, state, s, mess) < 0) return -1;
      type2 = get_token(s, &index2, &fval2, name2);
      arg_count++;
    } while (type2 == OP_TOKEN && index2 == COMMA_OP);

    if (type2 != OP_TOKEN || index2 != RIGHT_PAREN_OP) {
      sprintf(mess, "Error:  missing right paren in call to %s", name1);
      return -1;
    }

    if (arg_count != numargs[functypes[func_index]]) {
      sprintf(mess, "Error:  wrong number of arguments in call to %s", name1);
      return -1;
    }

    if (state->curop >= MAXOPS - 1) {
      strcpy(mess, "Error:  expression too long");
      return -1;
    }

    state->ops[state->curop] = FUNC_OP;
    state->args[state->curop] = functypes[func_index];
    state->curop++;
  }
  else if (type1 == OP_TOKEN && index1 == LEFT_PAREN_OP) {

    /* must be exp = ( exp ) */

    *s = ssave2; /* "unget" start of inner expression */

    if (get_exp3(ctx, state, s, mess) < 0) return -1;

    type1 = get_token(s, &index1, &fval1, name1);
    if (type1 != OP_TOKEN || index1 != RIGHT_PAREN_OP) {
      strcpy(mess, "Error:  missing right paren");
      return -1;
    }
  }
  else if (type1 == NAME_TOKEN) {
    Context cctx;
    char wstr[100];
    int numofctx, varowner, vartime;

    /* must be exp = var */

    *s = ssave2; /* "unget" second token - not part of expression */

    if (state->curop >= MAXOPS - 1) {
      strcpy(mess, "Error:  expression too long");
      return -1;
    }

    vartime = find_variable_time( ctx->dpy_ctx, name1);
    varowner = find_variable_owner( ctx->dpy_ctx, name1);
    if (varowner==-1){
      sprintf(mess, "Error:  bad variable owner");
      return -1;
    }
    else{
       int ahh;
       for (ahh=0; ahh< ctx->dpy_ctx->numofctxs; ahh++){
          if (varowner == ctx->dpy_ctx->ctxpointerarray[ahh]->context_index){
             cctx = ctx->dpy_ctx->ctxpointerarray[ahh];
             ahh = ctx->dpy_ctx->numofctxs;
          }
       }
    }
    var = find_variable(cctx, name1);
    if (var >= 0 || var == USETIME) {
      state->ops[state->curop] = PUSH_VAR_OP;
      state->args[state->curop] = var;
      state->args2[state->curop] = varowner;
      state->args3[state->curop] = vartime;
      state->curop++;

      /* add var to varlist (if its not already there) */
      flag = 0;
      for (i=0; i<state->numvars; i++) {
        if (var == state->varlist[i] &&
            varowner == state->varownerlist[i]) flag = 1;
      }
      if (!flag) {
        state->varlist[state->numvars] = var;
        state->varownerlist[state->numvars] = varowner;
        state->numvars++;
      }
    }
    else {
      sprintf(mess, "Error:  bad variable name: %s", name1);
      return -1;
    }
  }
  else if (type1 == NUM_TOKEN) {

    /* must be exp = num */

    *s = ssave2; /* "unget" second token - not part of expression */

    if (state->curop >= MAXOPS - 1) {
      strcpy(mess, "Error:  expression too long");
      return -1;
    }

    state->ops[state->curop] = PUSH_NUM_OP;
    state->nums[state->curop] = fval1;
    state->curop++;
  }
  else {

    /* not a valid expression */

    *s = ssave1;
    strcpy(mess, "Error:  bad expression");
    return -1;
  }
  return 0;
}




/*
 * Parse expression of form "name = ... " and return index
 * of new variable and "program" for computing it in the globals
 * ops, args and curop.
 * Input:  expression - the character string expression as type in by
 *                      the user.   Ex: "SPD = SQRT( U*U + V*V + W*W )"
 * Output: var - index for new variable
 *         recompute - flag indicating that variable is being recomputed
 * Return:  0 if OK or -1 if error
 */
static int parse( Display_Context dtx, struct compute_state *state,
                  char *expression, char *namevar,
                  int *varowner, int *var, int *recompute,
                  char mess[100] )
{
  char *string;
  int type;
  int index;
  float fval;
  char name[100];
  Context ctx;

  string = expression;

  type = get_token(&string, &index, &fval, namevar);
  if (type != NAME_TOKEN) {
    strcpy(mess, "Error:  must start with name of new variable");
    return -1;
  }

  /* Determine if the LHS of the expression names an existing variable */
  /* or a new one. */
  *varowner = find_variable_owner( dtx, namevar );

  if (*varowner == -1){
      sprintf(mess, "Error:  Bad destination variable ");
      return -1;
  }
  else{
     int ahh;
     for (ahh=0; ahh< dtx->numofctxs; ahh++){
        if (*varowner == dtx->ctxpointerarray[ahh]->context_index){
           ctx = dtx->ctxpointerarray[ahh];
           ahh = dtx->numofctxs;
        }
     }
  }
  *var = find_variable(ctx, namevar);

  if (*var >= 0) {
    if (ctx->VarType[*var] != VIS5D_EXPRESSION) {
      sprintf(mess, "Error:  destination variable name  %s  already used",
              namevar);
      return -1;
    }
    else {
      *recompute = 1;
    }
  }
  else {
    *recompute = 0;
  }

  type = get_token(&string, &index, &fval, name);
  if (type != OP_TOKEN || index != EQUALS_OP) {
    strcpy(mess, "Error:  missing equals sign");
    return -1;
  }

  /* initialize pointer to start of "program" */
  state->curop = 0;
  state->numvars = 0;

  /* parse expression for computing new variable */
  if (get_exp3(ctx, state, &string, mess) < 0) return -1;

  type = get_token(&string, &index, &fval, name);
  if (type != END_TOKEN) {
    strcpy(mess, "Error:  syntax");
    return -1;
  }

  if (*recompute == 0) {
    /* Allocate the new variable */
    *var = allocate_computed_variable(ctx, namevar);
    if (*var < 0) {
      strcpy(mess, "Error:  Max number of variables reached");
      return -1;
    }
  }
  else {
    min_max_init(ctx, *var);
  }
  return 0;
}


static int return_closes_timestep( Context ctx, int tothisday, int tothissec)
{
   int i, day, sec;
   int abs_sec, abs_day;
   int closest_tyme, best_day, best_sec;

   best_day = 1000000;
   best_sec = 1000000;
   for(i=0; i < ctx->NumTimes; i++){
      vis5d_get_ctx_time_stamp( ctx->context_index, i, &day, &sec);
      if (day < tothisday || (day == tothisday && sec < tothissec)){
         if (i == ctx->NumTimes - 1){
            return -1;
         }
         else if (sec > tothissec){
            abs_sec = 86400 - sec + tothissec;
            abs_day = tothisday - day - 1;
         }
         else if (sec <= tothissec){
            abs_sec = tothissec - sec;
            abs_day = tothisday - day;
         }
      }
      else if (day > tothisday || (day == tothisday && sec > tothissec)){
         if (i == 0){
            return -1;
         }
         else if (sec >= tothissec){
            abs_sec = sec - tothissec;
            abs_day = day - tothisday;
         }
         else if (sec < tothissec){
            abs_sec = 86400 - tothissec + sec;
            abs_day = day - tothisday - 1;
         }
      }
      else{
         abs_day = 0;
         abs_sec = 0;
      }
      if( abs_day < best_day || ( abs_day == best_day && abs_sec <= best_sec)){
         closest_tyme = i;
         best_day = abs_day;
         best_sec = abs_sec;
      }
   }
   return closest_tyme;
}
          


/*
 * Compute a new variable by evaluating an expression in terms of other
 * variables.  This is called from gui.c after the user has typed in
 * an expression.
 * Input:  expression - the character string expression as type in by
 *                      the user.   Ex: "SPD = SQRT( U*U + V*V + W*W )"
 * Return:  number of variable computed or -1 if error
 */
int compute_var( Display_Context dtx, char *expression, int *expressionowner,
                 char name[100], char mess[100], int *recompute )
{
  int time, var, nl, lowlev, toplev, length, layer, offset, i;
  float *g, *a, *b, *f[MAXOPS];
  int numops;
  int ftype, arg_count;
  Context ctx;

  struct compute_state fixed_state, *state;
  state = &fixed_state;

  strcpy(mess, "");

  /* parse the expression to build a tree, return -1 if error */
  if (parse(dtx, state, expression, name, expressionowner, &var, recompute, mess) < 0){
    return -1;
  }
  {
     int ahh;
     for (ahh=0; ahh< dtx->numofctxs; ahh++){
        if (*expressionowner == dtx->ctxpointerarray[ahh]->context_index){
           ctx = dtx->ctxpointerarray[ahh];
           ahh = dtx->numofctxs;
        }
     }
  }

  numops = state->curop; /* length of "program" to compute new variable */

  /* Determine how many levels in output grid (nl) */
  toplev = ctx->MaxNl;
  lowlev = 0;
  /* just look at the var that belong to that data ctx though */
  for (i=0; i<state->numvars; i++) {
    if (state->varlist[i] == USETIME){
       toplev = ctx->MaxNl;
       lowlev = 0;
    }
    else if (state->varownerlist[i] == ctx->context_index){
      if (ctx->Nl[state->varlist[i]] + ctx->LowLev[state->varlist[i]] < toplev) {
        toplev = ctx->Nl[state->varlist[i]] + ctx->LowLev[state->varlist[i]];
      }
      if (ctx->LowLev[state->varlist[i]] > lowlev) {
        lowlev = ctx->LowLev[state->varlist[i]];
      }
    }
  }
  nl = toplev - lowlev;
  if (nl < 1) {
    strcpy(mess, "Error:  grids don't overlap in the vertical");
    return -1;
  }
  layer = ctx->Nr * ctx->Nc;
  length = layer * nl;

  /* Evaluate the tree for each timestep */
  for (time=0;time<ctx->NumTimes;time++) {
    printf(" Creating Variable %s for Time %d\n", name, time); 
    /* run program for one time step */
    state->stop = 0;
    for (state->curop=0; state->curop<numops; state->curop++) {
      switch (state->ops[state->curop]) {
        case PUSH_VAR_OP:
          if (state->args2[state->curop] != ctx->context_index){
            int numtimes1, numtimes2,  dd1, tt1, dd2, tt2;
            int ahh, time2, tloop;
            Context tctx;
            for (ahh=0; ahh< ctx->dpy_ctx->numofctxs; ahh++){
              if (state->args2[state->curop] ==
                 ctx->dpy_ctx->ctxpointerarray[ahh]->context_index){
                 tctx = ctx->dpy_ctx->ctxpointerarray[ahh];
                 ahh = ctx->dpy_ctx->numofctxs;
              }
            }
            vis5d_get_ctx_time_stamp( ctx->context_index, time, &dd1, &tt1);
            time2 = return_closes_timestep( tctx, dd1, tt1);
            if (time2 < 0){
              a = (float *) allocate(ctx, length * sizeof(float));
              for (i=0; i<length; i++){
                 a[i] = MISSING;
              }
            }
            else{      
              g = get_grid2( ctx, tctx, time2, state->args[state->curop], nl);
              a = (float *) allocate(ctx, length * sizeof(float));
              for (i=0; i<length; i++){
                 a[i] = g[i];
              }
              release_grid2(ctx, time2, state->args[state->curop], nl, g);
            }
            state->sgrid[state->stop] = a;
            state->stop++;
          }
          else{
            if (state->args3[state->curop] != 0){
               if (time+state->args3[state->curop] < ctx->NumTimes &&
                   time+state->args3[state->curop] >= 0){
                  if (state->args[state->curop] == USETIME){
                     int jj, daze, minz;
                     a = (float *) allocate(ctx, length * sizeof(float));
                     /* get time in seconds since first time step*/
                     if (time+state->args3[state->curop] == 0){
                        jj = 0;
                     }
                     else{
                        daze = ctx->DayStamp[time+state->args3[state->curop]] -
                                     ctx->DayStamp[0];
                        minz = ctx->TimeStamp[time+state->args3[state->curop]] -
                                     ctx->TimeStamp[0];
                        jj = (86400*daze)+minz;
                     }
                     for (i=0; i<length; i++){
                        a[i] = (float) jj;
                     }
                     state->sgrid[state->stop] = a;
                     state->stop++;
                  }
                  else{
                     g = get_grid(ctx, time+state->args3[state->curop],
                                  state->args[state->curop]);
                     a = (float *) allocate(ctx, length * sizeof(float));
                     /* adjust to align lowest levels of grids */
                     offset = (lowlev - ctx->LowLev[state->args[state->curop]]) * layer;
                     for (i=0; i<length; i++) a[i] = g[i + offset];
                     release_grid(ctx, time+state->args3[state->curop],
                                  state->args[state->curop], g);
                     state->sgrid[state->stop] = a;
                     state->stop++;
                  }
               }
               else{
                  a = (float *) allocate(ctx, length * sizeof(float));
                  for (i=0; i<length; i++){
                     a[i] = MISSING;
                  }
                  state->sgrid[state->stop] = a;
                  state->stop++;
               }
            }
            else{
               if (state->args[state->curop] == USETIME){
                  int jj, daze, minz;
                  /* get time in seconds since first time step*/
                  a = (float *) allocate(ctx, length * sizeof(float));
                  if (time+state->args3[state->curop] == 0){
                     jj = 0;
                  }
                  else{
                     daze = ctx->DayStamp[time+state->args3[state->curop]] -
                            ctx->DayStamp[0];
                     minz = ctx->TimeStamp[time+state->args3[state->curop]] -
                            ctx->TimeStamp[0];
                     jj = (86400*daze)+minz;
                  }
                  for (i=0; i<length; i++){
                     a[i] = (float) jj;
                  }
                  state->sgrid[state->stop] = a;
                  state->stop++;
               }
               else{
                  g = get_grid(ctx, time, state->args[state->curop]);
                  a = (float *) allocate(ctx, length * sizeof(float));
                  /* adjust to align lowest levels of grids */
                  offset = (lowlev - ctx->LowLev[state->args[state->curop]]) * layer;
                  for (i=0; i<length; i++) a[i] = g[i + offset];
                  release_grid(ctx, time, state->args[state->curop], g);
                  state->sgrid[state->stop] = a;
                  state->stop++;
               }
            }
          }
          break;
        case PUSH_NUM_OP:
          a = (float *) allocate(ctx,length * sizeof(float));
          for (i=0; i<length; i++) a[i] = state->nums[state->curop];
          state->sgrid[state->stop] = a;
          state->stop++;
          break;
        case ADD_OP:
          a = state->sgrid[state->stop - 1];
          b = state->sgrid[state->stop - 2];
          for (i=0; i<length; i++) {
            if (IS_MISSING(a[i]) || IS_MISSING(b[i])) a[i] = MISSING;
            else a[i] = a[i] + b[i];
          }
          deallocate(ctx,b, length * sizeof(float));
          state->sgrid[state->stop - 2] = a;
          state->stop--;
          break;
        case SUB_OP:
          a = state->sgrid[state->stop - 1];
          b = state->sgrid[state->stop - 2];
          for (i=0; i<length; i++) {
            if (IS_MISSING(a[i]) || IS_MISSING(b[i])) a[i] = MISSING;
            else a[i] = b[i] - a[i];
          }
          deallocate(ctx,b, length * sizeof(float));
          state->sgrid[state->stop - 2] = a;
          state->stop--;
          break;
        case MUL_OP:
          a = state->sgrid[state->stop - 1];
          b = state->sgrid[state->stop - 2];
          for (i=0; i<length; i++) {
            if (IS_MISSING(a[i]) || IS_MISSING(b[i])) a[i] = MISSING;
            else a[i] = a[i] * b[i];
          }
          deallocate(ctx,b, length * sizeof(float));
          state->sgrid[state->stop - 2] = a;
          state->stop--;
          break;
        case DIV_OP:
          a = state->sgrid[state->stop - 1];
          b = state->sgrid[state->stop - 2];
          for (i=0; i<length; i++) {
            if (IS_MISSING(a[i]) || IS_MISSING(b[i])) a[i] = MISSING;
            else a[i] = b[i] / a[i];
          }
          deallocate(ctx,b, length * sizeof(float));
          state->sgrid[state->stop - 2] = a;
          state->stop--;
          break;
        case POWER_OP:
          a = state->sgrid[state->stop - 1];
          b = state->sgrid[state->stop - 2];
          for (i=0; i<length; i++) {
            if (IS_MISSING(a[i]) || IS_MISSING(b[i])) a[i] = MISSING;
            else a[i] = pow(b[i], a[i]);
          }
          deallocate(ctx,b, length * sizeof(float));
          state->sgrid[state->stop - 2] = a;
          state->stop--;
          break;
        case NEGATE_OP:
          a = state->sgrid[state->stop - 1];
          for (i=0; i<length; i++) {
            if (IS_MISSING(a[i])) a[i] = MISSING;
            else a[i] = -a[i];
          }
          state->sgrid[state->stop - 1] = a;
          break;
        case FUNC_OP:
          ftype = state->args[state->curop];
          arg_count = numargs[ftype];
          for (i=0; i<arg_count; i++) {
            state->stop--;
            f[arg_count - 1 - i] = state->sgrid[state->stop];
          }
          a = f[0];
          switch (ftype) {
            case SQRT_FUNC:
              for (i=0; i<length; i++) {
                if (IS_MISSING(a[i])) a[i] = MISSING;
                else a[i] = sqrt(a[i]);
              }
              break;
            case EXP_FUNC:
              for (i=0; i<length; i++) {
                if (IS_MISSING(a[i])) a[i] = MISSING;
                else a[i] = exp(a[i]);
              }
              break;
            case LOG_FUNC:
              for (i=0; i<length; i++) {
                if (IS_MISSING(a[i])) a[i] = MISSING;
                else a[i] = log(a[i]);
              }
              break;
            case SIN_FUNC:
              for (i=0; i<length; i++) {
                if (IS_MISSING(a[i])) a[i] = MISSING;
                else a[i] = sin(a[i]);
              }
              break;
            case COS_FUNC:
              for (i=0; i<length; i++) {
                if (IS_MISSING(a[i])) a[i] = MISSING;
                else a[i] = cos(a[i]);
              }
              break;
            case TAN_FUNC:
              for (i=0; i<length; i++) {
                if (IS_MISSING(a[i])) a[i] = MISSING;
                else a[i] = tan(a[i]);
              }
              break;
            case ATAN_FUNC:
              for (i=0; i<length; i++) {
                if (IS_MISSING(a[i])) a[i] = MISSING;
                else a[i] = atan(a[i]);
              }
              break;
            case ABS_FUNC:
              for (i=0; i<length; i++) {
                if (IS_MISSING(a[i])) a[i] = MISSING;
                else a[i] = fabs(a[i]);
              }
              break;
            case MIN_FUNC:
              b = f[1];
              for (i=0; i<length; i++) {
                if (IS_MISSING(a[i]) || IS_MISSING(b[i])) a[i] = MISSING;
                else a[i] = a[i] < b[i] ? a[i] : b[i];
              }
              break;
            case MAX_FUNC:
              b = f[1];
              for (i=0; i<length; i++) {
                if (IS_MISSING(a[i]) || IS_MISSING(b[i])) a[i] = MISSING;
                else a[i] = a[i] > b[i] ? a[i] : b[i];
              }
              break;
            default:
              strcpy(mess, "Error:  illegal program step");
              return -1;
          }

          for (i=1; i<arg_count; i++) {
            deallocate(ctx,f[i], length * sizeof(float));
          }
          state->sgrid[state->stop] = a;
          state->stop++;
          break;
        default:
          strcpy(mess, "Error:  illegal program step");
          return -1;
      }
    }
    if (state->stop != 1) return -1;

    /* save/compress the result grid */
    install_new_grid( ctx, time, var, state->sgrid[0], nl, lowlev );
    deallocate(ctx,state->sgrid[0], length * sizeof(float));
  }
  return var;
}


