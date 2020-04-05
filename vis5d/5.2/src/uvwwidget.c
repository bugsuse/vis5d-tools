/* uvwwidget.c */

/* Vis5D version 5.2 */

/*
Vis5D system for visualizing five dimensional gridded data sets
Copyright ( C ) 1990 - 2000 Bill Hibbard, Johan Kellum, Brian Paul,
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


/*
 * U, V, W variable widget window.
 *
 * This widget is used to specify which variables to use for the trajectory,
 * and wind slices' U, V, and W components.
 */



#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include "../lui5/lui.h"
#include "api.h"
#include "gui.h"



static int current_ctx;

int return_ctxindex( int gindex, char *varname )
{
   int yo;
   char thenumber[10];
   GuiContext gtx = get_gui_gtx( gindex );

   if (gtx->how_many_regular_contexts == 1){
      return gtx->array_of_ctxs[0];
   }   
   thenumber[0] = '\0';
   for (yo = 0; yo < 18; yo ++){

      /* WLH 20 Oct 98 */
      if (varname[yo] == '\0') break;

      if (varname[yo] == '.'){
/* WLH 20 Oct 98
         if (varname[yo+1] >= '0' || varname[yo+1] <= '9'){
*/
         /* WLH 20 Oct 98 */
         if (varname[yo+1] >= '0' && varname[yo+1] <= '9'){

            thenumber[0] = varname[yo+1];
            thenumber[1] = '\0';
            if (yo < 17 && varname[yo+2] >= '0' && varname[yo+2] <= '9' ){
               thenumber[1] = varname[yo+2];
               thenumber[2] = '\0';
               if (yo < 16 && varname[yo+3] >= '0' && varname[yo+3] <= '9'){
                  thenumber[2] = varname[yo+3];
                  thenumber[3] = '\0';
               }
            }
         }
         yo = 69;
      }
   }
   if (thenumber[0] == '\0'){
      return -1;
   }
   else{
      return atoi(thenumber);
   }
}

char *return_ctx_var( char *name)
{
   int yo;
   char thevar[20];

   for (yo = 0; yo < 18; yo ++){

/* WLH 16 Oct 98
      if (name[yo] == '.'){
*/
      /* WLH 16 Oct 98 */
      if (name[yo] == '.' || name[yo] == '\0'){

         thevar[yo] = '\0';
         return thevar;
      }
      thevar[yo] = name[yo];
   }
   return thevar;
}

char *return_var_and_index( char *vname, int index )
{
   int yo;
/* WLH 14 Oct 98 - function cannot return a string from the stack
   char whole[20];
*/
   /* WLH 14 Oct 98 */
   char *whole;

   char num[20];
   
   /* WLH 14 Oct 98 */
   whole = malloc(20);

   if (index <0 || vname[0] == 0){
      whole[0] = 0;
      return whole;
   }
   for (yo = 0; yo < 17; yo++){
      if (vname[yo] == '\0' || vname[yo] == ' '){
         yo -=1;
         whole[yo+1] = '.';
         sprintf(num, "%d\n", index );
         if (index > 99 && yo < 15){
            whole[yo+2] = num[0];
            whole[yo+3] = num[1];
            whole[yo+4] = num[2];
            whole[yo+5] = '\0';
            return whole;
         }
         else if ( index > 9 && yo < 16){
            whole[yo+2] = num[0];
            whole[yo+3] = num[1];
            whole[yo+4] = '\0';
            return whole;
         }
         else{
            whole[yo+2] = num[0];
            whole[yo+3] = '\0';
            return whole;
         }
      }
      whole[yo] = vname[yo];
   }
   whole[yo] = '\0';
   return whole;
}
      
         
  

static void read_uvw_widgets( GuiContext gtx )
{
   int index = gtx->context_index;
   char varname[30];
   int u1, v1, w1, u2, v2, w2, traju, trajv, trajw;
   int lp;
   char rap[30];

   LUI_FieldGetText( gtx->u1_field, varname );
   if (varname[0] != 0){
      gtx->u1owner = return_ctxindex(index, varname);
      lp = return_ctxindex(index, varname);
      strcpy(rap, return_ctx_var(varname)); 
      u1 = vis5d_find_var( lp, rap );
   }
   else{
      gtx->u1owner = -1;
   }
   LUI_FieldGetText( gtx->v1_field, varname );
   if (varname[0] != 0){
      gtx->v1owner = return_ctxindex(index, varname);
      lp = return_ctxindex(index, varname);
      strcpy(rap, return_ctx_var(varname)); 
      v1 = vis5d_find_var( lp, rap );
   }
   else {
      gtx->v1owner = -1;
   }
   LUI_FieldGetText( gtx->w1_field, varname );
   if (varname[0] != 0){
      gtx->w1owner = return_ctxindex(index,varname);
      lp = return_ctxindex(index, varname);
      strcpy(rap, return_ctx_var(varname)); 
      w1 = vis5d_find_var( lp, rap );
   }
   else {
      gtx->w1owner= -1;
   }
   LUI_FieldGetText( gtx->u2_field, varname );
   if (varname[0] != 0){
      gtx->u2owner = return_ctxindex(index,varname);
      lp = return_ctxindex(index, varname);
      strcpy(rap, return_ctx_var(varname)); 
      u2 = vis5d_find_var( lp, rap );
   }
   else {
      gtx->u2owner= -1;
   }
   LUI_FieldGetText( gtx->v2_field, varname );
   if (varname[0] != 0){
      gtx->v2owner = return_ctxindex(index,varname);
      lp = return_ctxindex(index, varname);
      strcpy(rap, return_ctx_var(varname)); 
      v2 = vis5d_find_var( lp, rap );
   }
   else {
      gtx->v2owner= -1;
   }
   LUI_FieldGetText( gtx->w2_field, varname );
   if (varname[0] != 0){
      gtx->w2owner = return_ctxindex(index,varname);
      lp = return_ctxindex(index, varname);
      strcpy(rap, return_ctx_var(varname)); 
      w2 = vis5d_find_var( lp, rap );
   }

   else {
      gtx->w2owner= -1;
   }
   LUI_FieldGetText( gtx->traju_field, varname );
   if (varname[0] != 0){
      gtx->tuowner = return_ctxindex(index,varname);
      lp = return_ctxindex(index, varname);
      strcpy(rap, return_ctx_var(varname)); 
      traju = vis5d_find_var( lp, rap );
   }
   else {
      gtx->tuowner= -1;
   }
   LUI_FieldGetText( gtx->trajv_field, varname );
   if (varname[0] != 0){
      gtx->tvowner = return_ctxindex(index,varname);
      lp = return_ctxindex(index, varname);
      strcpy(rap, return_ctx_var(varname)); 
      trajv = vis5d_find_var( lp, rap );
   }
   else {
      gtx->tvowner= -1;
   }
   LUI_FieldGetText( gtx->trajw_field, varname );
   if (varname[0] != 0){
      gtx->twowner = return_ctxindex(index,varname);
      lp = return_ctxindex(index, varname);
      strcpy(rap, return_ctx_var(varname)); 
      trajw = vis5d_find_var( lp, rap );
   }
   else {
      gtx->twowner = -1;
   }


   vis5d_set_wind_vars( index, gtx->u1owner, u1, gtx->v1owner, v1,
                               gtx->w1owner, w1, gtx->u2owner, u2,
                               gtx->v2owner, v2, gtx->w2owner, w2,
                               gtx->tuowner, traju, gtx->tvowner, trajv,
                               gtx->twowner, trajw );

   gtx->cur_hwind = gtx->cur_vwind = -1;
   gtx->cur_hstream = gtx->cur_vstream -1;

}




/* called when Cancel button is pressed:  just close window */
static int cancel_cb( LUI_NEWBUTTON *b, int state )
{
   int index = b->context_index;
   GuiContext gtx = get_gui_gtx( index );

   XUnmapWindow( GuiDpy, gtx->uvw_window );
   gtx->uvw_map = 0;
   return 0;
}



/* called when apply button is pressed:  use new variables, keep window open */
static int apply_cb( LUI_NEWBUTTON *b, int state )
{
   int index = b->context_index;
   GuiContext gtx = get_gui_gtx( index );

   read_uvw_widgets( gtx );

   /* recompute graphics here??? */
   return 0;
}


/* called when ok button is pressed:  use new vars, close window */
static int ok_cb( LUI_NEWBUTTON *b, int state )
{
   int index = b->context_index;
   GuiContext gtx = get_gui_gtx( index );

   read_uvw_widgets( gtx );

   XSync(GuiDpy, 0);

   /* recompute graphics here??? */
   XUnmapWindow( GuiDpy, gtx->uvw_window );
   gtx->uvw_map = 0;

   return 0;
}




/* called when the text in a field changes */
static int field_cb( LUI_FIELD *f, char *text )
{
#ifdef LEAVEOUT
   int index = f->context_index;
   GuiContext gtx = get_gui_gtx( index );
   int var, numvars, *varptr;
   char varname[20];

   if (f==gtx->traju_field) {
      varptr = &gtx->traju_var;
   }
   else if (f==gtx->trajv_field) {
      varptr = &gtx->trajv_var;
   }
   else if (f==gtx->trajw_field) {
      varptr = &gtx->trajw_var;
   }
   else if (f==gtx->u1_field) {
      varptr = &gtx->u1_var;
   }
   else if (f==gtx->v1_field) {
      varptr = &gtx->v1_var;
   }
   else if (f==gtx->w1_field) {
      varptr = &gtx->w1_var;
   }
   else if (f==gtx->u2_field) {
      varptr = &gtx->u2_var;
   }
   else if (f==gtx->v2_field) {
      varptr = &gtx->v2_var;
   }
   else if (f==gtx->w2_field) {
      varptr = &gtx->w2_var;
   }
   else {
      /* this should never happen! */
      abort();
   }

   /* Be sure the varname entered is valid and determine the corresponding
    * var number.
    */
   var = vis5d_find_var( index, text );
   if (var>=0) {
      *varptr = var;

      /* recompute graphics? */

      return 0;
   }

   /* If we get here, that means the user entered an invalid varname so
    * restore previous name.
    */
   if (*varptr>=0) {
      vis5d_get_ctx_var_name( index, *varptr, varname );
      LUI_FieldSetText( f, varname );
   }
   else {
      LUI_FieldSetText( f, "" );
   }

#endif
   return 0;
}




/*
 * Make the U,V,W widget window.
 */
void make_uvw_widget( GuiContext gtx )
{
   LUI_NEWBUTTON *apply, *ok, *cancel;
   Window w;

   current_ctx = gtx->context_index;

   w = LUI_CreateWindowAt( LUI_RootWindow, 150, 400, 210, 310 );
   gtx->uvw_window = w;

   LUI_BorderWidth( 2 );

   LUI_NewLabelCreate( w, LUI_LEFT, LUI_TOP, 380, 20, "Trajectory Vars:" );
   LUI_NewLabelCreate( w, LUI_LEFT, LUI_NEXT_Y, 120, 20, "   East/West:" );
   gtx->traju_field = LUI_FieldCreate( w, LUI_NEXT_X, LUI_SAME_Y, 80, 20 );
   LUI_NewLabelCreate( w, LUI_LEFT, LUI_NEXT_Y, 120, 20, "   North/South:" );
   gtx->trajv_field = LUI_FieldCreate( w, LUI_NEXT_X, LUI_SAME_Y, 80, 20 );
   LUI_NewLabelCreate( w, LUI_LEFT, LUI_NEXT_Y, 120, 20, "   Vertical:" );
   gtx->trajw_field = LUI_FieldCreate( w, LUI_NEXT_X, LUI_SAME_Y, 80, 20 );

   LUI_NewLabelCreate( w, LUI_LEFT, LUI_NEXT_Y, 380, 20, "Wind, Stream slice 1:" );
   LUI_NewLabelCreate( w, LUI_LEFT, LUI_NEXT_Y, 120, 20, "   East/West:" );
   gtx->u1_field = LUI_FieldCreate( w, LUI_NEXT_X, LUI_SAME_Y, 80, 20 );
   LUI_NewLabelCreate( w, LUI_LEFT, LUI_NEXT_Y, 120, 20, "   North/South:" );
   gtx->v1_field = LUI_FieldCreate( w, LUI_NEXT_X, LUI_SAME_Y, 80, 20 );
   LUI_NewLabelCreate( w, LUI_LEFT, LUI_NEXT_Y, 120, 20, "   Vertical:" );
   gtx->w1_field = LUI_FieldCreate( w, LUI_NEXT_X, LUI_SAME_Y, 80, 20 );

   LUI_NewLabelCreate( w, LUI_LEFT, LUI_NEXT_Y, 380, 20, "Wind, Stream slice 2:" );
   LUI_NewLabelCreate( w, LUI_LEFT, LUI_NEXT_Y, 120, 20, "   East/West:" );
   gtx->u2_field = LUI_FieldCreate( w, LUI_NEXT_X, LUI_SAME_Y, 80, 20 );
   LUI_NewLabelCreate( w, LUI_LEFT, LUI_NEXT_Y, 120, 20, "   North/South:" );
   gtx->v2_field = LUI_FieldCreate( w, LUI_NEXT_X, LUI_SAME_Y, 80, 20 );
   LUI_NewLabelCreate( w, LUI_LEFT, LUI_NEXT_Y, 120, 20, "   Vertical:" );
   gtx->w2_field = LUI_FieldCreate( w, LUI_NEXT_X, LUI_SAME_Y, 80, 20 );

   /* Link fields together for TABing */
   LUI_FieldLink( gtx->traju_field, gtx->trajv_field );
   LUI_FieldLink( gtx->trajv_field, gtx->trajw_field );
   LUI_FieldLink( gtx->trajw_field, gtx->u1_field );
   LUI_FieldLink( gtx->u1_field, gtx->v1_field );
   LUI_FieldLink( gtx->v1_field, gtx->w1_field );
   LUI_FieldLink( gtx->w1_field, gtx->u2_field );
   LUI_FieldLink( gtx->u2_field, gtx->v2_field );
   LUI_FieldLink( gtx->v2_field, gtx->w2_field );

   LUI_NewLabelCreate( w, LUI_LEFT, LUI_NEXT_Y, 10, 5, "" );  /* spacer */

   apply = LUI_PushButtonCreate( w, LUI_LEFT, LUI_NEXT_Y, 66, 20, "Apply" );
   LUI_ButtonCallback( apply, apply_cb );
   ok = LUI_PushButtonCreate( w, LUI_NEXT_X, LUI_SAME_Y, 66, 20, "OK" );
   LUI_ButtonCallback( ok, ok_cb );
   cancel = LUI_PushButtonCreate( w, LUI_NEXT_X, LUI_SAME_Y, 66, 20, "Cancel");
   LUI_ButtonCallback( cancel, cancel_cb );

   apply->context_index = gtx->context_index;
   ok->context_index = gtx->context_index;
   cancel->context_index = gtx->context_index;
}




/*
 * Update all the text fields with the names of the current U,V,W variables.
 */
static void load_uvw_widgets( GuiContext gtx )
{
   int index = gtx->context_index;
   char varname[20];
   int u1, v1, w1, u2, v2, w2, traju, trajv, trajw;
   int owner[9];
   char rap[30];

   vis5d_get_wind_vars( index, &owner[0], &u1, &owner[1], &v1, &owner[2], &w1,
                               &owner[3], &u2, &owner[4], &v2, &owner[5], &w2,
                               &owner[6], &traju, &owner[7], &trajv, &owner[8], &trajw );


   /*** Traj vars ***/
   if (traju>=0) {
      vis5d_get_ctx_var_name( owner[6], traju, varname );
   }
   else {
      varname[0] = 0;
   }
   strcpy( rap, return_var_and_index(varname, owner[6]));
   LUI_FieldSetText( gtx->traju_field, rap );

   if (trajv>=0) {
      vis5d_get_ctx_var_name( owner[7], trajv, varname );
   }
   else {
      varname[0] = 0;
   }
   strcpy( rap, return_var_and_index(varname, owner[7]));
   LUI_FieldSetText( gtx->trajv_field, rap);

   if (trajw>=0) {
      vis5d_get_ctx_var_name( owner[8], trajw, varname );
   }
   else {
      varname[0] = 0;
   }
   strcpy( rap, return_var_and_index(varname, owner[8]));   
   LUI_FieldSetText( gtx->trajw_field, rap );

   /*** First set of slices ****/
   if (u1>=0) {
      vis5d_get_ctx_var_name( owner[0], u1, varname );
   }
   else {
      varname[0] = 0;
   }
   strcpy( rap, return_var_and_index(varname, owner[0]));   
   LUI_FieldSetText( gtx->u1_field, rap);

   if (v1>=0) {
      vis5d_get_ctx_var_name( owner[1], v1, varname );
   }
   else {
      varname[0] = 0;
   }
   strcpy( rap, return_var_and_index(varname, owner[1]));   
   LUI_FieldSetText( gtx->v1_field, rap);

   if (w1>=0) {
      vis5d_get_ctx_var_name( owner[2], w1, varname );
   }
   else {
      varname[0] = 0;
   }
   strcpy( rap, return_var_and_index(varname, owner[2]));   
   LUI_FieldSetText( gtx->w1_field, rap);

   /*** First set of slices ****/
   if (u2>=0) {
      vis5d_get_ctx_var_name( owner[3], u2, varname );
   }
   else {
      varname[0] = 0;
   }
   strcpy( rap, return_var_and_index(varname, owner[3]));   
   LUI_FieldSetText( gtx->u2_field, rap);

   if (v2>=0) {
      vis5d_get_ctx_var_name( owner[4], v2, varname );
   }
   else {
      varname[0] = 0;
   }
   strcpy( rap, return_var_and_index(varname, owner[4]));   
   LUI_FieldSetText( gtx->v2_field, rap);

   if (w2>=0) {
      vis5d_get_ctx_var_name( owner[5], w2, varname );
   }
   else {
      varname[0] = 0;
   }
   strcpy( rap, return_var_and_index(varname, owner[5]));   
   LUI_FieldSetText( gtx->w2_field, rap);
}



void show_uvw_widget( GuiContext gtx )
{
   int index = gtx->context_index;

   /* create the widget if needed */
   if (!gtx->uvw_window) {
      make_uvw_widget( gtx );
   }

   /* map the window */
   XMapWindow( GuiDpy, gtx->uvw_window );
   gtx->uvw_map = 1;

   /* update the text fields */
   load_uvw_widgets( gtx );
   read_uvw_widgets( gtx );
}
