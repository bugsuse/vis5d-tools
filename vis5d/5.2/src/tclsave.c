/* tclsave.c */

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

/*
 * Tcl-based save and restore.
 */



#include <stdio.h>
#include <string.h>
#include "globals.h"
#include "graphics.h"
#include "gui.h"





/*
 * Return the parameters which describe a colorbar curve.
 * If one or more color table entries were 'drawn' by the user, rather
 * than computed from the parameters, return 1.  Return 0 if the user
 * has not drawn the curves.
 * Input:  index - context index
 *         graphic - one of:  VIS5D_ISOSURF, VIS5D_CHSLICE, VIS5D_CVSLICE,
 *                   VIS5D_VOLUME, VIS5D_TRAJ or VIS5D_TOPO.
 *         var - variable number if type isn't VIS5D_TOPO
 * Output: params - pointer to address of parameter array
 */
static int get_colorbar_params( int index, int graphic, int vindex, int var,
                                float params[] )
{
   float *p;
   int i;
   unsigned int temptable[256], *table;
   int same;

   vis5d_get_color_table_params( index, graphic, vindex, var, &p ); 
   for (i=0; i<7; i++) params[i] = p[i];

   /*
    * This is tricky.  Compare the graphic's color table to one computed
    * from its parameters.  If they're different return 1, else return 0.
    * This is used by the SAVE function to determine if all the color
    * table entries have to be written or just the parameters which
    * describe the curves.
    */
   vis5d_get_color_table_address( index, graphic, vindex, var, &table ); 
   vis5d_color_table_recompute( temptable, 256, params, 1, 1 );
   same = 1;
   for (i=0;i<255;i++) {
      if (table[i]!=temptable[i]) {
         same = 0;
         break;
      }
   }

   if (same) {
      return 0;
   }
   else {
      return 1;
   }
}





/*
 * Save current graphics and colors to the 'savefile' as a Tcl script.
 * Input:  savefile - filename to save to.
 * Return:  0 for success,
 *          VIS5D_BAD_VALUE if unable to open file
 *          VIS5D_FAIL if error while writing file.
 */
int tcl_save( int index, char *savefile )
{
   FILE *f;
   int cyo, chowmany, cwhichones[VIS5D_MAX_CONTEXTS];
   int time, var, i, k, set;
   int numvars;
   float r, g, b, a;
   char varname[20];

   vis5d_get_num_of_ctxs_in_display( index, &chowmany, cwhichones);

   f = fopen(savefile,"w");
   if (!f) {
      return VIS5D_BAD_VALUE;
   }  

   /* Prolog */ 
   fprintf(f,"#Vis5D 4.3 Tcl save file\n\n");


   for ( cyo = 0; cyo < chowmany; cyo++){
      int vindex = cwhichones[cyo];

      vis5d_get_ctx_numvars( cwhichones[cyo], &numvars );
      /* misc colors */
      fprintf(f,"\n#Box color\n");
      vis5d_get_color( index, VIS5D_BOX, 0, &r, &g, &b, &a );
      fprintf(f,"vis5d_set_color $dtx VIS5D_BOX 0 %5.3f %5.3f %5.3f %5.3f\n",
              r,g,b,a );

      fprintf(f,"\n#Light map color\n");
      vis5d_get_color( index, VIS5D_LIGHT_MAP, 0, &r, &g, &b, &a );
      fprintf(f,"vis5d_set_color $dtx VIS5D_LIGHT_MAP 0 %5.3f %5.3f %5.3f %5.3f\n",
              r,g,b,a );

      fprintf(f,"\n#Dark map color\n");
      vis5d_get_color( index, VIS5D_DARK_MAP, 0, &r, &g, &b, &a );
      fprintf(f,"vis5d_set_color $dtx VIS5D_DARK_MAP 0 %5.3f %5.3f %5.3f %5.3f\n",
              r,g,b,a );

      fprintf(f,"\n#Background color\n");
      vis5d_get_color( index, VIS5D_BACKGROUND, 0, &r, &g, &b, &a );
      fprintf(f,
              "vis5d_set_color $dtx VIS5D_BACKGROUND 0 %5.3f %5.3f %5.3f %5.3f\n",
              r,g,b,a );

      /* Text labels */
      fprintf(f,"\n#Text labels\n");
      {
         int i = 1;
         int x, y;
         char label[1000];
         while (vis5d_get_label( index, i, &x, &y, label )==0) {
            fprintf(f,"vis5d_make_label $dtx %d %d \"%s\"\n", x, y, label );
            i++;
         }
      }

      
      /* View matrix */
      fprintf(f,"\n#Viewing matrix\n");
      {
         float mat[4][4];
         int i, j;
         vis5d_get_matrix( index, mat );
         fprintf(f,"vis5d_set_matrix $dtx {");
         for (i=0;i<4;i++) {
            for (j=0;j<4;j++) {
               fprintf(f," %g", mat[i][j] );
            }
         }
         fprintf(f," }\n");
      }

      /* Camera */
      fprintf(f,"\n#Camera\n");
      {
         int perspec;
         float front, zoom;
         vis5d_get_camera( index, &perspec, &front, &zoom );
         fprintf(f,"vis5d_set_camera $dtx %d %g %g\n", perspec, front, zoom );
      }

      /* Cloned and computed physical variables */
      fprintf(f,"\n#Cloned or computed variables\n");
      for (var=0; var<numvars; var++) {
         int type;
         vis5d_get_var_type( cwhichones[cyo], var, &type );
         if (type==VIS5D_CLONE) {
            int vartoclone;
            char origname[20], clonename[20];
            vis5d_get_ctx_var_name( cwhichones[cyo], var, origname );
            vis5d_get_var_info( cwhichones[cyo], var, (void*) &vartoclone );
            vis5d_get_ctx_var_name( cwhichones[cyo], vartoclone, clonename );
            if (cyo == 0) {
              fprintf(f,"vis5d_make_clone_variable $ctx \"%s\" \"%s\"\n",
                      origname, clonename );
            }
            else {
              fprintf(f,"vis5d_make_clone_variable %d \"%s\" \"%s\"\n",
                      cwhichones[cyo], origname, clonename );
            }
         }
         else if (type==VIS5D_EXT_FUNC) {
            char funcname[100];
            vis5d_get_var_info( cwhichones[cyo], var, (void*) funcname );
            fprintf(f,"vis5d_compute_ext_func $dtx \"%s\"\n", funcname );
         }
         else if (type==VIS5D_EXPRESSION) {
            char expr[100];
            vis5d_get_var_info( cwhichones[cyo], var, (void*) expr );
            fprintf(f,"vis5d_make_expr_var $dtx \"%s\"\n", expr );
         }
      }

      /* Isosurfaces */
      fprintf(f,"\n#Isosurfaces\n");
      for (var=0; var<numvars; var++) {
         float isolevel, min, max;
         int colorvarowner, colorvar;
         vis5d_get_isosurface( cwhichones[cyo], var, &isolevel );
         vis5d_get_ctx_var_range( cwhichones[cyo], var, &min, &max );
         vis5d_get_isosurface_color_var( cwhichones[cyo], var, &colorvarowner, &colorvar ); 
         vis5d_get_ctx_var_name( cwhichones[cyo], var, varname );
         if (isolevel!=min) {
            if (cyo == 0) {
              fprintf(f,"vis5d_set_isosurface $ctx \"%s\" %g\n", varname, isolevel);
            }
            else {
              fprintf(f,"vis5d_set_isosurface %d \"%s\" %g\n", cwhichones[cyo],
                      varname, isolevel);
            }
            if (colorvar>-1) {
               char colorvarname[20];
               vis5d_get_ctx_var_name( colorvarowner, colorvar, colorvarname );
               if (cyo == 0) {
                 fprintf(f,"vis5d_set_isosurface_color_var_and_owner $ctx \"%s\" %d  \"%s\"\n",
                         varname, colorvarowner, colorvarname );
               }
               else {
                 fprintf(f,"vis5d_set_isosurface_color_var_and_owner %d \"%s\" %d  \"%s\"\n",
                         cwhichones[cyo], varname, colorvarowner, colorvarname );
               }
            }
            /* command to recompute the isosurface */
            if (cyo == 0) {
              fprintf(f,"vis5d_make_isosurface $ctx VIS5D_ALL_TIMES \"%s\" 0\n",
                      varname );
            }
            else {
              fprintf(f,"vis5d_make_isosurface %d VIS5D_ALL_TIMES \"%s\" 0\n",
                      cwhichones[cyo], varname );
            }
         }
         if (vis5d_enable_graphics(cwhichones[cyo],VIS5D_ISOSURF,var,VIS5D_GET)){
            if (cyo == 0) {
              fprintf(f,"vis5d_enable_graphics $ctx VIS5D_ISOSURF %d VIS5D_ON\n", var);
            }
            else {
              fprintf(f,"vis5d_enable_graphics %d VIS5D_ISOSURF %d VIS5D_ON\n",
                         cwhichones[cyo], var);
            }
         }
         /* WLH 10 Nov 98 */
         else {
            if (cyo == 0) {
              fprintf(f,"vis5d_enable_graphics $ctx VIS5D_ISOSURF %d VIS5D_OFF\n", var);
            }
            else {
              fprintf(f,"vis5d_enable_graphics %d VIS5D_ISOSURF %d VIS5D_OFF\n",
                         cwhichones[cyo], var);
            }
         }

      }

      /* Horizontal contour slices */
      fprintf(f,"\n#Horizontal contour slices\n");
      for (var=0;var<numvars;var++) {
         float interval, low, high, level;
         vis5d_get_hslice( vindex, var, &interval, &low, &high, &level );
         vis5d_get_ctx_var_name( vindex, var, varname );
         if (vindex == cwhichones[0]) {
           fprintf(f,"vis5d_set_hslice $ctx \"%s\" %g %g %g %g\n", varname,
                   interval, low, high, level );
         }
         else {
           fprintf(f,"vis5d_set_hslice %d \"%s\" %g %g %g %g\n",vindex, varname,
                   interval, low, high, level );
         }
         if (vis5d_enable_graphics(vindex, VIS5D_HSLICE, var, VIS5D_GET)){
            if (vindex == cwhichones[0]) {
              fprintf(f,"vis5d_enable_graphics $ctx VIS5D_HSLICE %d VIS5D_ON\n", var);
              fprintf(f,"vis5d_make_hslice $ctx VIS5D_ALL_TIMES %d 1\n", var );
            }
            else {
              fprintf(f,"vis5d_enable_graphics %d VIS5D_HSLICE %d VIS5D_ON\n",
                         vindex, var);
              fprintf(f,"vis5d_make_hslice %d VIS5D_ALL_TIMES %d 1\n", vindex, var );
            }
         }
         /* WLH 10 Nov 98 */
         else {
            if (vindex == cwhichones[0]) {
              fprintf(f,"vis5d_enable_graphics $ctx VIS5D_HSLICE %d VIS5D_OFF\n", var);
            }
            else {
              fprintf(f,"vis5d_enable_graphics %d VIS5D_HSLICE %d VIS5D_OFF\n",
                         vindex, var);
            }
         }
         /* MJK 12.04.98 begin */
         if (vis5d_enable_sfc_graphics (index, VIS5D_HSLICE, var, VIS5D_GET)== VIS5D_ON){
            if (vindex == cwhichones[0]) {
/* MJK 3.29.99 
               fprintf (f,"vis5d_enable_sfc_graphics $ctx VIS5D_HSLICE %s VIS5D_ON\n",var);
*/
               fprintf (f,"vis5d_enable_sfc_graphics $ctx VIS5D_HSLICE %d VIS5D_ON\n",var);
            }
         }
         else{
            if (vindex == cwhichones[0]) {
/* MJK 3.29.99                
               fprintf (f,"vis5d_enable_sfc_graphics $ctx VIS5D_HSLICE %s VIS5D_OFF\n",var);
*/
               fprintf (f,"vis5d_enable_sfc_graphics $ctx VIS5D_HSLICE %d VIS5D_OFF\n",var);
            }
         }
         /* MJK 12.04.98 end */
      }

      /* Vertical contour slices */
      fprintf(f,"\n#Vertical contour slices\n");
      for (var=0;var<numvars;var++) {
         float interval, low, high, r0, c0, r1, c1;
         vis5d_get_vslice( vindex, var, &interval, &low, &high, &r0,&c0, &r1,&c1 );
         vis5d_get_ctx_var_name( vindex, var, varname );
         if (vindex == cwhichones[0]) {
           fprintf(f,"vis5d_set_vslice $ctx \"%s\" %g %g %g  %g %g  %g %g\n",
                   varname, interval, low, high, r0,c0, r1,c1 );
         }
         else {
           fprintf(f,"vis5d_set_vslice %d \"%s\" %g %g %g  %g %g  %g %g\n", vindex,
                   varname, interval, low, high, r0,c0, r1,c1 );
         }
         if (vis5d_enable_graphics(vindex, VIS5D_VSLICE, var, VIS5D_GET)){
            if (vindex == cwhichones[0]) {
              fprintf(f,"vis5d_enable_graphics $ctx VIS5D_VSLICE %d VIS5D_ON\n", var);
              fprintf(f,"vis5d_make_vslice $ctx VIS5D_ALL_TIMES %d 1\n", var );
            }
            else {
              fprintf(f,"vis5d_enable_graphics %d VIS5D_VSLICE %d VIS5D_ON\n",
                         vindex, var);
              fprintf(f,"vis5d_make_vslice %d VIS5D_ALL_TIMES %d 1\n", vindex, var );
            }
         }      
         /* WLH 10 Nov 98 */
         else {
            if (vindex == cwhichones[0]) {
              fprintf(f,"vis5d_enable_graphics $ctx VIS5D_VSLICE %d VIS5D_OFF\n", var);
            }
            else {
              fprintf(f,"vis5d_enable_graphics %d VIS5D_VSLICE %d VIS5D_OFF\n",
                         vindex, var);
            }
         }

      }

      /* Horizontal colored slices */
      fprintf(f,"\n#Horizontal colored slices\n");
      for (var=0;var<numvars;var++) {
         float level;
         vis5d_get_chslice( vindex, var, &level );
         vis5d_get_ctx_var_name( vindex, var, varname );
         if (vindex == cwhichones[0]) {
           fprintf(f,"vis5d_set_chslice $ctx \"%s\" %g\n", varname, level );
         }
         else {
           fprintf(f,"vis5d_set_chslice %d \"%s\" %g\n",vindex, varname, level );
         }
         if (vis5d_enable_graphics(vindex, VIS5D_CHSLICE, var, VIS5D_GET)){
            if (vindex == cwhichones[0]) {
              fprintf(f,"vis5d_enable_graphics $ctx VIS5D_CHSLICE %d VIS5D_ON\n", var);
              fprintf(f,"vis5d_make_chslice $ctx VIS5D_ALL_TIMES %d 1\n", var );
            }
            else {
              fprintf(f,"vis5d_enable_graphics %d VIS5D_CHSLICE %d VIS5D_ON\n",
                         vindex, var);
              fprintf(f,"vis5d_make_chslice %d VIS5D_ALL_TIMES %d 1\n", vindex, var );
            }
         }      
         /* WLH 10 Nov 98 */
         else {
            if (vindex == cwhichones[0]) {
              fprintf(f,"vis5d_enable_graphics $ctx VIS5D_CHSLICE %d VIS5D_OFF\n", var);
            }
            else {
              fprintf(f,"vis5d_enable_graphics %d VIS5D_CHSLICE %d VIS5D_OFF\n",
                         vindex, var);
            }
         }

      }
      
      /* Vertical colored slices */
      fprintf(f,"\n#Vertical colored slices\n");
      for (var=0;var<numvars;var++) {
         float level, r0, c0, r1, c1;
         vis5d_get_cvslice( vindex, var, &r0, &c0, &r1, &c1 );
         vis5d_get_ctx_var_name( vindex, var, varname );
         if (vindex == cwhichones[0]) {
           fprintf(f,"vis5d_set_cvslice $ctx \"%s\" %g %g %g %g\n",
                   varname, r0, c0, r1, c1 );
         }
         else {
           fprintf(f,"vis5d_set_cvslice %d \"%s\" %g %g %g %g\n",vindex,
                   varname, r0, c0, r1, c1 );
         }
         if (vis5d_enable_graphics(vindex, VIS5D_CVSLICE, var, VIS5D_GET)){
            if (vindex == cwhichones[0]) {
              fprintf(f,"vis5d_enable_graphics $ctx VIS5D_CVSLICE %d VIS5D_ON\n", var);
              fprintf(f,"vis5d_make_cvslice $ctx VIS5D_ALL_TIMES %d 1\n", var );
            }
            else {
              fprintf(f,"vis5d_enable_graphics %d VIS5D_CVSLICE %d VIS5D_ON\n",
                         vindex, var);
              fprintf(f,"vis5d_make_cvslice %d VIS5D_ALL_TIMES %d 1\n", vindex, var );
            }
         }
         /* WLH 10 Nov 98 */
         else {
            if (vindex == cwhichones[0]) {
              fprintf(f,"vis5d_enable_graphics $ctx VIS5D_CVSLICE %d VIS5D_OFF\n", var);
            }
            else {
              fprintf(f,"vis5d_enable_graphics %d VIS5D_CVSLICE %d VIS5D_OFF\n",
                         vindex, var);
            }
         }

      }
                                                                               
      /* Current Display Volume */
      fprintf(f, "\n#Current Display Volume\n");
      {
         int current_vol_owner, current_vol;
         vis5d_get_volume(index, &current_vol_owner, &current_vol);
         if (current_vol_owner > -1 && current_vol > -1){
/* WLH 16 Nov 98
            fprintf(f,"vis5d_set_volume_and_owner $dtx %d %d\n", current_vol_owner,
                       current_vol);
*/
            /* WLH 16 Nov 98 */
            if (current_vol_owner == cwhichones[0]) {
              fprintf(f,"vis5d_set_volume_and_owner $dtx $ctx %d\n", current_vol);
            }
            else {
              fprintf(f,"vis5d_set_volume_and_owner $dtx %d %d\n", current_vol_owner,
                         current_vol);
            }

         }
         else{
           fprintf(f,"vis5d_set_volume_and_owner $dtx -1 -1\n");
         }
      }

      /* Horizontal wind vector slices */
      fprintf(f,"\n#Horizontal wind vector slices\n");
      for (i=0;i<VIS5D_WIND_SLICES;i++) {
         float density, scale, level;
         vis5d_get_hwindslice( index, i, &density, &scale, &level );
         fprintf(f,"vis5d_set_hwindslice $dtx %d  %g %g %g\n", i, density,
                 scale, level );
         if (vis5d_enable_graphics(cwhichones[0], VIS5D_HWIND, i, VIS5D_GET)){
            fprintf(f,"vis5d_enable_graphics $ctx VIS5D_HWIND %d VIS5D_ON\n", i);
            fprintf(f,"vis5d_make_hwindslice $dtx VIS5D_ALL_TIMES %d 1\n", i);
         }
         /* WLH 10 Nov 98 */
         else {
            fprintf(f,"vis5d_enable_graphics $ctx VIS5D_HWIND %d VIS5D_OFF\n", i);
         }
         /* MJK 12.04.98 begin */
         if (vis5d_enable_sfc_graphics (vindex, VIS5D_HWIND, i, VIS5D_GET)== VIS5D_ON){
            fprintf (f,"vis5d_enable_sfc_graphics $ctx VIS5D_HWIND %d VIS5D_ON\n",i);
         }
         else{
            fprintf (f,"vis5d_enable_sfc_graphics $ctx VIS5D_HWIND %d VIS5D_OFF\n",i);
         }
         /* MJK 12.04.98 end */

      }

      /* Vertical wind vector slices */
      fprintf(f,"\n#Vertical wind vector slices\n");
      for (i=0;i<VIS5D_WIND_SLICES;i++) {
         float density, scale, r0, c0, r1, c1;
         vis5d_get_vwindslice( index, i, &density, &scale, &r0, &c0, &r1, &c1 );
         fprintf(f,"vis5d_set_vwindslice $dtx %d  %g %g  %g %g %g %g\n", i,
                 density, scale, r0, c0, r1, c1 );
         if (vis5d_enable_graphics(cwhichones[0], VIS5D_VWIND, i, VIS5D_GET)){
            fprintf(f,"vis5d_enable_graphics $ctx VIS5D_VWIND %d VIS5D_ON\n", i);
            fprintf(f,"vis5d_make_vwindslice $dtx VIS5D_ALL_TIMES %d 1\n", i);         
         }
         /* WLH 10 Nov 98 */
         else {
            fprintf(f,"vis5d_enable_graphics $ctx VIS5D_VWIND %d VIS5D_OFF\n", i);
         }

      }

      /* Horizontal wind stream slices */
      fprintf(f,"\n#Horizontal stream slices\n");
      for (i=0;i<VIS5D_WIND_SLICES;i++) {
         float density, level;
         vis5d_get_hstreamslice( index, i, &density, &level );
         fprintf(f,"vis5d_set_hstreamslice $dtx %d  %g %g\n", i, density,
                 level );
         if (vis5d_enable_graphics(cwhichones[0], VIS5D_HSTREAM, i, VIS5D_GET)){
            fprintf(f,"vis5d_enable_graphics $ctx VIS5D_HSTREAM %d VIS5D_ON\n", i);      
            fprintf(f,"vis5d_make_hstreamslice $dtx VIS5D_ALL_TIMES %d 1\n", i);         
         }            
         /* WLH 10 Nov 98 */
         else {
            fprintf(f,"vis5d_enable_graphics $ctx VIS5D_HSTREAM %d VIS5D_OFF\n", i);
         }
         /* MJK 12.04.98 begin */
         if (vis5d_enable_sfc_graphics (vindex, VIS5D_HSTREAM, i, VIS5D_GET)== VIS5D_ON){
            fprintf (f,"vis5d_enable_sfc_graphics $ctx VIS5D_HWIND %d VIS5D_ON\n",i);
         }
         else{
            fprintf (f,"vis5d_enable_sfc_graphics $ctx VIS5D_HSTREAM %d VIS5D_OFF\n",i);
         }
         /* MJK 12.04.98 end */
      }

      /* Vertical wind stream slices */
      fprintf(f,"\n#Vertical stream slices\n");
      for (i=0;i<VIS5D_WIND_SLICES;i++) {
         float density, r0, c0, r1, c1;
         vis5d_get_vstreamslice( index, i, &density, &r0, &c0, &r1, &c1 );
         fprintf(f,"vis5d_set_vstreamslice $dtx %d  %g %g %g %g %g\n", i,
                 density, r0, c0, r1, c1 );
         if (vis5d_enable_graphics(cwhichones[0], VIS5D_VSTREAM, i, VIS5D_GET)){
            fprintf(f,"vis5d_enable_graphics $ctx VIS5D_VSTREAM %d VIS5D_ON\n", i);
            fprintf(f,"vis5d_make_vstreamslice $dtx VIS5D_ALL_TIMES %d 1\n", i);                
         }      
         /* WLH 10 Nov 98 */
         else {
            fprintf(f,"vis5d_enable_graphics $ctx VIS5D_VSTREAM %d VIS5D_OFF\n", i);
         }

      }


      /* Trajectories */
      fprintf(f,"\n#Trajectories\n");
      {
         float prevstep = 0.0, prevlength = 0.0;
         int prevribbon = 0;
         int numtraj = vis5d_get_num_traj( index );
         for (i=0;i<numtraj;i++) {
            float row, col, lev, step, length;
            int timestep, group, ribbon;
            vis5d_get_traj_info( index, i, &row, &col, &lev, &timestep,
                                 &step, &length, &group, &ribbon );
            if (i==0 || step!=prevstep || length!=prevlength
                || ribbon!=prevribbon) {
               fprintf(f,"vis5d_set_traj $dtx %g %g %d\n", step, length, ribbon );
            }
            prevstep = step;
            prevlength = length;
            prevribbon = ribbon;
            
            fprintf(f,"vis5d_make_traj $dtx %g %g %g %d %d\n", row, col, lev,
                    timestep, group );
         }
         for (i=0; i<VIS5D_TRAJ_SETS; i++){
            if (vis5d_enable_graphics(cwhichones[0], VIS5D_TRAJ, i, VIS5D_GET)){
               fprintf(f,"vis5d_enable_graphics $ctx VIS5D_TRAJ %d VIS5D_ON\n", i);      
            }
         }            
      }

      /* Isosurface colors */
      fprintf(f,"\n#Isosurface colors\n");
      for (var=0;var<numvars;var++) {
         vis5d_get_color( index, VIS5D_ISOSURF, vindex*MAXVARS+var, &r, &g, &b, &a );
         vis5d_get_ctx_var_name( vindex, var, varname );
         if (vindex == cwhichones[0]) {
           fprintf(f,
             "vis5d_set_color $dtx VIS5D_ISOSURF $ctx \"%s\" %5.3f %5.3f %5.3f %5.3f\n",
             varname, r,g,b,a );
         }
         else {
           fprintf(f,
             "vis5d_set_color $dtx VIS5D_ISOSURF %d \"%s\" %5.3f %5.3f %5.3f %5.3f\n",
             vindex, varname, r,g,b,a );
         }
      }

      /* HSlice colors */
      fprintf(f,"\n#Horizontal contour slice colors\n");
      for (var=0;var<numvars;var++) {
         vis5d_get_color( index, VIS5D_HSLICE, vindex*MAXVARS+var, &r, &g, &b, &a );
         vis5d_get_ctx_var_name( vindex, var, varname );
         if (vindex == cwhichones[0]) {
           fprintf(f,
               "vis5d_set_color $dtx VIS5D_HSLICE $ctx \"%s\" %5.3f %5.3f %5.3f %5.3f\n",
               varname, r,g,b,a );
         }
         else {
           fprintf(f,
               "vis5d_set_color $dtx VIS5D_HSLICE %d \"%s\" %5.3f %5.3f %5.3f %5.3f\n",
               vindex, varname, r,g,b,a );
         }
      }

      /* VSlice colors */
      fprintf(f,"\n#Vertical contour slice colors\n");
      for (var=0;var<numvars;var++) {
         vis5d_get_color( index, VIS5D_VSLICE, vindex*MAXVARS+var, &r, &g, &b, &a );
         vis5d_get_ctx_var_name( vindex, var, varname );
         if (vindex == cwhichones[0]) {
           fprintf(f,
              "vis5d_set_color $dtx VIS5D_VSLICE $ctx \"%s\" %5.3f %5.3f %5.3f %5.3f\n",
              varname, r,g,b,a );
         }
         else {
           fprintf(f,
              "vis5d_set_color $dtx VIS5D_VSLICE %d \"%s\" %5.3f %5.3f %5.3f %5.3f\n",
              vindex, varname, r,g,b,a );
         }
      }

      /* Colored HSlice colors */
      fprintf(f,"\n#Horizontal colored slice tickmark colors\n");
      for (var=0;var<numvars;var++) {
         vis5d_get_color( index, VIS5D_CHSLICE, vindex*MAXVARS+var, &r, &g, &b, &a );
         vis5d_get_ctx_var_name( vindex, var, varname );
         if (vindex == cwhichones[0]) {
           fprintf(f,
             "vis5d_set_color $dtx VIS5D_CHSLICE $ctx \"%s\" %5.3f %5.3f %5.3f %5.3f\n",
             varname, r,g,b,a );
         }
         else {
           fprintf(f,
             "vis5d_set_color $dtx VIS5D_CHSLICE %d \"%s\" %5.3f %5.3f %5.3f %5.3f\n",
             vindex, varname, r,g,b,a );
         }
      }

      /* Colored VSlice colors */
      fprintf(f,"\n#Vertical colored slice tickmark colors\n");
      for (var=0;var<numvars;var++) {
         vis5d_get_color( index, VIS5D_CVSLICE, vindex*MAXVARS+var, &r, &g, &b, &a );
         vis5d_get_ctx_var_name( vindex, var, varname );
         if (vindex == cwhichones[0]) {
           fprintf(f,
            "vis5d_set_color $dtx VIS5D_CVSLICE $ctx \"%s\" %5.3f %5.3f %5.3f %5.3f\n",
            varname, r,g,b,a );
         }
         else {
           fprintf(f,
            "vis5d_set_color $dtx VIS5D_CVSLICE %d \"%s\" %5.3f %5.3f %5.3f %5.3f\n",
            vindex, varname, r,g,b,a );
         }
      }

      /* HWind colors */
      fprintf(f,"\n#Horizontal wind slice colors\n");
      for (i=0;i<VIS5D_WIND_SLICES;i++) {
         vis5d_get_color( index, VIS5D_HWIND, i, &r, &g, &b, &a );
         fprintf(f,
                 "vis5d_set_color $dtx VIS5D_HWIND %d %5.3f %5.3f %5.3f %5.3f\n",
                 i, r,g,b,a );
      }

      /* VWind colors */
      fprintf(f,"\n#Vertical wind slice colors\n");
      for (i=0;i<VIS5D_WIND_SLICES;i++) {
         vis5d_get_color( index, VIS5D_VWIND, i, &r, &g, &b, &a );
         fprintf(f,
                 "vis5d_set_color $dtx VIS5D_VWIND %d %5.3f %5.3f %5.3f %5.3f\n",
                 i, r,g,b,a );
      }

      /* Horizontal Stream colors */
      fprintf(f,"\n#Horizontal stream slice colors\n");
      for (i=0;i<VIS5D_WIND_SLICES;i++) {
         vis5d_get_color( index, VIS5D_HSTREAM, i, &r, &g, &b, &a );
         fprintf(f,
                 "vis5d_set_color $dtx VIS5D_HSTREAM %d %5.3f %5.3f %5.3f %5.3f\n",
                 i, r,g,b,a );
      }

      /* Vertical Stream colors */
      fprintf(f,"\n#Vertical stream slice colors\n");
      for (i=0;i<VIS5D_WIND_SLICES;i++) {
         vis5d_get_color( index, VIS5D_VSTREAM, i, &r, &g, &b, &a );
         fprintf(f,
                 "vis5d_set_color $dtx VIS5D_VSTREAM %d %5.3f %5.3f %5.3f %5.3f\n",
                 i, r,g,b,a );
      }

      /* Trajectory colors */
      fprintf(f,"\n#Trajectory colors\n");
      for (i=0;i<VIS5D_TRAJ_SETS;i++) {
         int colorvarowner, colorvar;
         vis5d_get_color( index, VIS5D_TRAJ, i, &r, &g, &b, &a );
         fprintf(f,"vis5d_set_color $dtx VIS5D_TRAJ %d %5.3f %5.3f %5.3f %5.3f\n",
                 i, r,g,b,a );
         vis5d_get_trajectory_color_var( index, i, &colorvarowner, &colorvar ); 
         if (colorvar>=0) {
            char varname[20];
            vis5d_get_ctx_var_name( colorvarowner, colorvar, varname );
            fprintf(f,"vis5d_set_trajectory_color_var_and_owner $dtx %d %d \"%s\"\n",
                    i, colorvarowner, varname );
         }
      }  


/* TO HERE */


      /* Isosurface color tables */
      fprintf(f,"\n#Isosurface color tables\n");
      for (var=0;var<numvars;var++) {
         unsigned int *ctable;
         char varname[20];
         float params[8];

         vis5d_get_ctx_var_name( index, var, varname );

         k = get_colorbar_params( index, VIS5D_ISOSURF, vindex, var, params );

         if (vindex == cwhichones[0]) {
           fprintf(f,"vis5d_set_color_table_params $dtx VIS5D_ISOSURF $ctx");
         }
         else {
           fprintf(f,"vis5d_set_color_table_params $dtx VIS5D_ISOSURF %d", vindex);
         }
         fprintf(f," \"%s\" %.3f %.3f %.3f %.3f\n", varname, params[0], params[1],
                 params[2], params[3] );

         if (k) {
            /* the color table can't be described by the parameters alone */
            /* save each individual table entry */
            vis5d_get_color_table_address( index, VIS5D_ISOSURF, vindex, var, &ctable ); 
            for (i=0;i<256;i++) {
               if (vindex == cwhichones[0]) {
                 fprintf(f,"vis5d_set_color_table_entry $dtx VIS5D_ISOSURF $ctx");
               }
               else {
                 fprintf(f,"vis5d_set_color_table_entry $dtx VIS5D_ISOSURF %d", vindex);
               }
               fprintf(f," \"%s\" %d %d %d %d %d\n", varname, i, 
                       UNPACK_RED(ctable[i]), UNPACK_GREEN(ctable[i]),
                       UNPACK_BLUE(ctable[i]), UNPACK_ALPHA(ctable[i]) );
            }
            fprintf(f,"\n");
         }
      }         


      /* Horizontal color slice color tables */
      fprintf(f,"\n#Horizontal color slice color tables\n");
      for (var=0;var<numvars;var++) {
         unsigned int *ctable;
         char varname[20];
         float params[8];

         vis5d_get_ctx_var_name( index, var, varname );

         k = get_colorbar_params( index, VIS5D_CHSLICE, vindex, var, params );

         if (vindex == cwhichones[0]) {
           fprintf(f,"vis5d_set_color_table_params $dtx VIS5D_CHSLICE $ctx");
         }
         else {
           fprintf(f,"vis5d_set_color_table_params $dtx VIS5D_CHSLICE %d ", vindex);
         }
         fprintf(f," \"%s\" %.3f %.3f %.3f %.3f\n", varname, params[0], params[1],
                 params[2], params[3] );

         if (k) {
            /* the color table can't be described by the parameters alone */
            /* save each individual table entry */
            vis5d_get_color_table_address( index, VIS5D_CHSLICE, vindex, var, &ctable ); 
            for (i=0;i<256;i++) {
               if (vindex == cwhichones[0]) {
                 fprintf(f,"vis5d_set_color_table_entry $dtx VIS5D_CHSLICE $ctx");
               }
               else {
                 fprintf(f,"vis5d_set_color_table_entry $dtx VIS5D_CHSLICE %d", vindex);
               }
               fprintf(f," \"%s\" %d %d %d %d %d\n", varname, i, 
                       UNPACK_RED(ctable[i]), UNPACK_GREEN(ctable[i]),
                       UNPACK_BLUE(ctable[i]), UNPACK_ALPHA(ctable[i]) );
            }
            fprintf(f,"\n");
         }
      }         

      /* Vertical color slice color tables */
      fprintf(f,"\n#Vertical color slice color tables\n");
      for (var=0;var<numvars;var++) {
         unsigned int *ctable;
         char varname[20];
         float params[8];

         vis5d_get_ctx_var_name( index, var, varname );

         k = get_colorbar_params( index, VIS5D_CVSLICE, vindex, var, params );

         if (vindex == cwhichones[0]) {
           fprintf(f,"vis5d_set_color_table_params $dtx VIS5D_CVSLICE $ctx");
         }
         else {
           fprintf(f,"vis5d_set_color_table_params $dtx VIS5D_CVSLICE %d", vindex);
         }
         fprintf(f," \"%s\" %.3f %.3f %.3f %.3f\n", varname, params[0], params[1],
                 params[2], params[3] );

         if (k) {
            /* the color table can't be described by the parameters alone */
            /* save each individual table entry */
            vis5d_get_color_table_address( index, VIS5D_CVSLICE, vindex, var, &ctable ); 
            for (i=0;i<256;i++) {
               if (vindex == cwhichones[0]) {
                 fprintf(f,"vis5d_set_color_table_entry $dtx VIS5D_CVSLICE $ctx");
               }
               else {
                 fprintf(f,"vis5d_set_color_table_entry $dtx VIS5D_CVSLICE %d", vindex);
               }
               fprintf(f," \"%s\" %d %d %d %d %d\n", varname, i, 
                       UNPACK_RED(ctable[i]), UNPACK_GREEN(ctable[i]),
                       UNPACK_BLUE(ctable[i]), UNPACK_ALPHA(ctable[i]) );
            }
            fprintf(f,"\n");
         }
      }         

      /* Volume color tables */
      fprintf(f,"\n#Volume color tables\n");
      for (var=0;var<numvars;var++) {
         unsigned int *ctable;
         char varname[20];
         float params[8];

         vis5d_get_ctx_var_name( index, var, varname );

         k = get_colorbar_params( index, VIS5D_VOLUME, vindex, var, params );

         if (vindex == cwhichones[0]) {
           fprintf(f,"vis5d_set_color_table_params $dtx VIS5D_VOLUME $ctx");
         }
         else {
           fprintf(f,"vis5d_set_color_table_params $dtx VIS5D_VOLUME %d", vindex);
         }
         fprintf(f," \"%s\" %.3f %.3f %.3f %.3f\n", varname, params[0], params[1],
                 params[2], params[3] );

         if (k) {
            /* the color table can't be described by the parameters alone */
            /* save each individual table entry */
            vis5d_get_color_table_address( index, VIS5D_VOLUME, vindex, var, &ctable ); 
            for (i=0;i<256;i++) {
               if (vindex == cwhichones[0]) {
                 fprintf(f,"vis5d_set_color_table_entry $dtx VIS5D_VOLUME $ctx");
               }
               else {
                 fprintf(f,"vis5d_set_color_table_entry $dtx VIS5D_VOLUME %d", vindex);
               }
               fprintf(f," \"%s\" %d %d %d %d %d\n", varname, i, 
                       UNPACK_RED(ctable[i]), UNPACK_GREEN(ctable[i]),
                       UNPACK_BLUE(ctable[i]), UNPACK_ALPHA(ctable[i]) );
            }
            fprintf(f,"\n");
         }
      }         

      /* Trajectory color tables */
      fprintf(f,"\n#Trajectory color tables\n");
      for (var=0;var<numvars;var++) {
         unsigned int *ctable;
         float params[8];

         vis5d_get_ctx_var_name( index, var, varname );
         k = get_colorbar_params( index, VIS5D_TRAJ, vindex, var, params );

         if (vindex == cwhichones[0]) {
           fprintf(f,"vis5d_set_color_table_params $dtx VIS5D_TRAJ $ctx");
         }
         else {
           fprintf(f,"vis5d_set_color_table_params $dtx VIS5D_TRAJ %d", vindex);
         }
         fprintf(f," \"%s\" %.3f %.3f %.3f %.3f\n", varname, params[0], params[1],
                 params[2], params[3] );

         if (k) {
            /* the color table can't be described by the parameters alone */
            /* save each individual table entry */
            vis5d_get_color_table_address( index, VIS5D_TRAJ, vindex, var, &ctable ); 
            for (i=0;i<256;i++) {
               if (vindex == cwhichones[0]) {
                 fprintf(f,"vis5d_set_color_table_entry $dtx VIS5D_TRAJ $ctx");
               }
               else {
                 fprintf(f,"vis5d_set_color_table_entry $dtx VIS5D_TRAJ %d", vindex);
               }
               fprintf(f," \"%s\" %d %d %d %d %d\n", varname, i, 
                       UNPACK_RED(ctable[i]), UNPACK_GREEN(ctable[i]),
                       UNPACK_BLUE(ctable[i]), UNPACK_ALPHA(ctable[i]) );
            }
            fprintf(f,"\n");
         }
      }         


      /* MJK 12.04.98 begin */
      if (vis5d_enable_sfc_map (index, VIS5D_GET) == VIS5D_ON){
         fprintf (f,"vis5d_enable_sfc_map $dtx VIS5D_ON\n");
      }
      else{
         fprintf (f,"vis5d_enable_sfc_map $dtx VIS5D_OFF\n");
      }
      /* MJK 12.04.98 end */


      /* Topography color tables */
      fprintf(f,"\n#Topography color tables\n");
      for (var=-1;var<numvars;var++) {
         unsigned int *ctable;
         float params[8];

         if (var>=0) {
            vis5d_get_ctx_var_name( index, var, varname );
            k = get_colorbar_params( index, VIS5D_TOPO, vindex, var, params );
         }
         else {
            int j;
            for (j=0; j<8; j++) params[j] = 0;
            strcpy( varname, "-1" );
            k = 1;
         }
         if (vindex == cwhichones[0]) {
           fprintf(f,"vis5d_set_color_table_params $dtx VIS5D_TOPO $ctx");
         }
         else {
           fprintf(f,"vis5d_set_color_table_params $dtx VIS5D_TOPO %d", vindex);
         }
         fprintf(f," \"%s\" %.3f %.3f %.3f %.3f\n", varname, params[0],
                     params[1], params[2], params[3] );

         if (k) {
            /* the color table can't be described by the parameters alone */
            /* save each individual table entry */
            vis5d_get_color_table_address( index, VIS5D_TOPO, vindex, var, &ctable ); 
            for (i=0;i<256;i++) {
               if (vindex == cwhichones[0]) {
                 fprintf(f,"vis5d_set_color_table_entry $dtx VIS5D_TOPO $ctx");
               }
               else {
                 fprintf(f,"vis5d_set_color_table_entry $dtx VIS5D_TOPO %d", vindex);
               }
               fprintf(f," \"%s\" %d %d %d %d %d\n", varname, i, 
                       UNPACK_RED(ctable[i]), UNPACK_GREEN(ctable[i]),
                       UNPACK_BLUE(ctable[i]), UNPACK_ALPHA(ctable[i]) );
            }
            fprintf(f,"\n");
         }
      }
   }         
   {
      int colorvar, colorvarowner;
      vis5d_get_topo_color_var( index, &colorvarowner, &colorvar ); 
      fprintf(f, "\n");
      fprintf(f, "vis5d_set_topo_color_var_and_owner $dtx %d %d\n", colorvarowner, colorvar );
   }

   /* MJK 12.04.98 begin */
   /* TODO */
   /*
   if (vis5d_graphics_mode (index, VIS5D_BOX, VIS5D_GET) == VIS5D_ON)
      fprintf (f, "vis5d_graphics_mode $ctx VIS5D_BOX VIS5D_ON\n");
   if (vis5d_graphics_mode (index, VIS5D_CLOCK, VIS5D_GET) == VIS5D_ON)
      fprintf (f, "vis5d_graphics_mode $ctx VIS5D_CLOCK VIS5D_ON\n");
   if (vis5d_graphics_mode (index, VIS5D_MAP, VIS5D_GET) == VIS5D_ON)
      fprintf (f, "vis5d_graphics_mode $ctx VIS5D_MAP VIS5D_ON\n");
   if (vis5d_graphics_mode (index, VIS5D_TOPO, VIS5D_GET) == VIS5D_ON)
      fprintf (f, "vis5d_graphics_mode $ctx VIS5D_TOPO VIS5D_ON\n");
   if (vis5d_graphics_mode (index, VIS5D_LEGENDS, VIS5D_GET) == VIS5D_ON)
      fprintf (f, "vis5d_graphics_mode $ctx VIS5D_LEGENDS VIS5D_ON\n");
   if (vis5d_graphics_mode (index, VIS5D_PERSPECTIVE, VIS5D_GET) == VIS5D_ON)
      fprintf (f, "vis5d_graphics_mode $ctx VIS5D_PERSPECTIVE VIS5D_ON\n");
   if (vis5d_graphics_mode (index, VIS5D_CONTOUR_NUMBERS, VIS5D_GET) == VIS5D_ON)
      fprintf (f, "vis5d_graphics_mode $ctx VIS5D_CONTOUR_NUMBERS VIS5D_ON\n");
   if (vis5d_graphics_mode (index, VIS5D_GRID_COORDS, VIS5D_GET) == VIS5D_ON)
      fprintf (f, "vis5d_graphics_mode $ctx VIS5D_GRID_COORDS VIS5D_ON\n");
   if (vis5d_graphics_mode (index, VIS5D_PRETTY, VIS5D_GET) == VIS5D_ON)
      fprintf (f, "vis5d_graphics_mode $ctx VIS5D_PRETTY VIS5D_ON\n");
   if (vis5d_graphics_mode (index, VIS5D_INFO, VIS5D_GET) == VIS5D_ON)
      fprintf (f, "vis5d_graphics_mode $ctx VIS5D_INFO VIS5D_ON\n");
   if (vis5d_graphics_mode (index, VIS5D_PROBE, VIS5D_GET) == VIS5D_ON)
      fprintf (f, "vis5d_graphics_mode $ctx VIS5D_PROBE VIS5D_ON\n");
   if (vis5d_graphics_mode (index, VIS5D_SOUND, VIS5D_GET) == VIS5D_ON)
      fprintf (f, "vis5d_graphics_mode $ctx VIS5D_SOUND VIS5D_ON\n");
   if (vis5d_graphics_mode (index, VIS5D_CURSOR, VIS5D_GET) == VIS5D_ON)
      fprintf (f, "vis5d_graphics_mode $ctx VIS5D_CURSOR VIS5D_ON\n");
   if (vis5d_graphics_mode (index, VIS5D_ANIMRECORD, VIS5D_GET) == VIS5D_ON)
      fprintf (f, "vis5d_graphics_mode $ctx VIS5D_ANIMRECORD VIS5D_ON\n");
   if (vis5d_graphics_mode (index, VIS5D_TEXTURE, VIS5D_GET) == VIS5D_ON)
      fprintf (f, "vis5d_graphics_mode $ctx VIS5D_TEXTURE VIS5D_ON\n");
   if (vis5d_graphics_mode (index, VIS5D_DEPTHCUE, VIS5D_GET) == VIS5D_ON)
      fprintf (f, "vis5d_graphics_mode $ctx VIS5D_DEPTHCUE VIS5D_ON\n");
   if (vis5d_graphics_mode (index, VIS5D_JULIAN, VIS5D_GET) == VIS5D_ON)
      fprintf (f, "vis5d_graphics_mode $ctx VIS5D_JULIAN VIS5D_ON\n");
   if (vis5d_graphics_mode (index, VIS5D_BARBS, VIS5D_GET) == VIS5D_ON)
      fprintf (f, "vis5d_graphics_mode $ctx VIS5D_BARBS VIS5D_ON\n");
   if (vis5d_graphics_mode (index, VIS5D_SND_THTA, VIS5D_GET) == VIS5D_ON)
      fprintf (f, "vis5d_graphics_mode $ctx VIS5D_SND_THTA VIS5D_ON\n");
   if (vis5d_graphics_mode (index, VIS5D_SND_THTE, VIS5D_GET) == VIS5D_ON)
      fprintf (f, "vis5d_graphics_mode $ctx VIS5D_SND_THTE VIS5D_ON\n");
   if (vis5d_graphics_mode (index, VIS5D_SND_W, VIS5D_GET) == VIS5D_ON)
      fprintf (f, "vis5d_graphics_mode $ctx VIS5D_SND_W VIS5D_ON\n");
   if (vis5d_graphics_mode (index, VIS5D_SND_TICKS, VIS5D_GET) == VIS5D_ON)
      fprintf (f, "vis5d_graphics_mode $ctx VIS5D_SND_TICKS VIS5D_ON\n");
   if (vis5d_graphics_mode (index, VIS5D_SND_MIXRAT, VIS5D_GET) == VIS5D_ON)
      fprintf (f, "vis5d_graphics_mode $ctx VIS5D_SND_MIXRAT VIS5D_ON\n");
   if (vis5d_graphics_mode (index, VIS5D_SND_TEMP, VIS5D_GET) == VIS5D_ON)
      fprintf (f, "vis5d_graphics_mode $ctx VIS5D_SND_TEMP VIS5D_ON\n");
   */
   


   fprintf(f, "\nvis5d_draw_frame %d\n", index );

   fclose(f);
   return 0;
}


