/* read_gr3d.c */


/*
 * Functions for reading McIDAS GR3D files
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "binio.h"
#include "file.h"
#include "grid.h"
#include "misc.h"
#include "proj.h"
#include "projlist.h"
#include "../src/v5d.h"



/*
 * Scan the named file, createing a grid_info struct for each grid.  Store
 * the grid_info structs in the grid data base.
 * Input:  name - name of McIDAS GR3D file.
 *         db - the grid data base
 * Return:  number of grids found in the file
 */
int get_gr3d_info( char *name, struct grid_db *db )
{
   struct grid_info *info;
   int f;
   int i, numgrids, firstgridpos;
   int header[64];
   int grids = 0;
   float args[20];

   /* Open the file */
   f = open( name, O_RDONLY );
   if (f<0) {
      return 0;
   }

   /* Read the file header (64 4-byte words) */
   read_int4_array( f, header, 64 );

   /* Determine how many 3-D grids are in the file */
   numgrids = header[11];
   firstgridpos = header[12];

   /* Read the description of each 3-D grid */
   for (i=0;i<numgrids;i++) {
      int mapproj, vertsys;

      if (lseek( f, 256 + i*256, SEEK_SET ) < 0) {
         /* error */
         continue;
      }
      read_int4_array( f, header, 64 );

      /* If the grid size is valid */
      if (   header[1]>=2 && header[1]<=MAXROWS
          && header[2]>=2 && header[2]<=MAXCOLUMNS
          && header[3]>=2 && header[3]<=MAXLEVELS) {

         info = alloc_grid_info();
         info->FileName = str_dup( name );
         info->Format = FILE_GR3D;
         info->Position = header[4]*4;  /* offset to start of data, in bytes */

         info->Nr = header[1];
         info->Nc = header[2];
         info->Nl = header[3];

         info->DateStamp = header[5];  /* Date in YYDDD format */
         info->TimeStamp = header[6];  /* Time in HHMMSS format */
         { /* Get variable name, remove trailing spaces */
            char varname[10];
            int j;
            /* Do this instead of memcpy because it works on Crays */
            varname[0] = (char) ((header[8] >> 24) & 0xff);
            varname[1] = (char) ((header[8] >> 16) & 0xff);
            varname[2] = (char) ((header[8] >> 8) & 0xff);
            varname[3] = (char) ( header[8] & 0xff);
            varname[4] = 0;
            for (j=3;j>=0 && varname[j]==' ';j--) {
               varname[j] = 0;
            }
            info->VarName = str_dup( varname );
         }

         mapproj = header[21];
         if (mapproj==4) {
            /* Rectilinear lat/lon */
            args[0] = (float) header[22] / 10000.0;  /* North Lat */
            args[1] = (float) header[23] / 10000.0;  /* West Lon */
            args[2] = (float) header[24] / 10000.0;  /* Lat Inc */
            args[3] = (float) header[25] / 10000.0;  /* Lon Inc */
            info->Proj = new_projection( db, PROJ_LINEAR, info->Nr, info->Nc,
                                         args );
         }
         else {
            /* other map projections not implemented */
         }

         vertsys = header[30];       /* vertical coord system */
         if (vertsys==1) {
            float top = (float) header[31] / 1000.0;   /* Top in km */
            float inc = (float) header[32] / 1000.0;   /* Level increment */
            args[0] = top - inc * (info->Nl-1);
            args[1] = inc;
            info->Vcs = new_vcs( db, VERT_EQUAL_KM, info->Nl, 0, args );
         }
         else {
            /* other vertical coord systems */
         }

         append_grid( info, db );
         grids++;
      }
   }

   return grids;
}




/*
 * Get the grid data described by g.
 * Input:  g - pointer to a grid_info structure.  It tells us which grid
 *             in which file is needed.
 * Return:  pointer to grid data (can be free()'d when finished)
 *          OR return NULL if there's an error.
 */
float *get_gr3d_data( struct grid_info *g )
{
   float *data;
   int f, count, read;

   /* open file, seek to grid position */
   f = open( g->FileName, O_RDONLY );
   if (f<0) return NULL;

   if (lseek( f, g->Position, SEEK_SET ) != g->Position) {
      /* seek failed */
      printf("Error:  seek grid data from %s failed\n", g->FileName );
      close(f);
      return NULL;
   }

   /* allocate buffer */
   count = g->Nr * g->Nc * g->Nl;
   data = (float *) malloc( count * sizeof(float) );
   if (!data) {
      printf("Error:  out of memory in get_gr3d_data\n");
      return NULL;
   }

   read = read_float4_array( f, data, count );
   if (read<count) {
      printf("Error:  read grid data from %s failed\n", g->FileName );
      free( data );
      close(f);
      return NULL;
   }

/*   print_min_max( data, count );*/

   close(f);
   return data;
}



