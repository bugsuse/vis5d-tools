/* projlist.c */


/*
 * Manage a list of map projections and vertical coordinate systems (vcs).
 * Basically each grid_info struct (see grid.h) will point to a
 * map projection and vcs structure kept in a list here.
 */


#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "grid.h"
#include "memory.h"
#include "projlist.h"
#include "v5d.h"


#ifndef M_PI
#  define M_PI 3.1415926
#endif
#define DEG2RAD       (M_PI/180.0)
#define EARTH_RADIUS  6371.23



/*
 * Test if two floats are nearly equal, i.e. within some epsilon of
 * each other.
 */
static int equal( float x, float y )
{
   float diff = x - y;

   if (diff<0.001 && diff>-0.001) {
      return 1;
   }
   else {
      return 0;
   }
}




/* Return the sign of x */
static float sign( float x )
{
   if (x<0.0) {
      return -1.0;
   }
   else if (x>0.0) {
      return 1.0;
   }
   else {
      return 0.0;
   }
}



/*
 * When doing (row,column) to (lat,lon) conversions it's sometimes useful
 * to have some auxillary values around to simplify the calculations.  This
 * function computes those auxillary values, if any, for a projection and
 * stores them in the projection's auxargs array.
 * Input:  proj - the projection
 */
static void compute_aux_proj_args( struct projection *proj )
{
   float lat1, lat2;

   switch (proj->Kind) {

      case PROJ_LAMBERT:   /* Lambert conformal */
#define Lat1 proj->Args[0]
#define Lat2 proj->Args[1]
#define PoleRow proj->Args[2]
#define PoleCol proj->Args[3]
#define CentralLon proj->Args[4]
#define ColInc proj->Args[5]
#define Hemisphere proj->AuxArgs[0]
#define ConeFactor proj->AuxArgs[1]
#define Cone proj->AuxArgs[2]
         proj->AuxArgs = (float *) MALLOC( 3 * sizeof(float) );
         if (Lat1==Lat2) {
            /* polar stereographic? */
            if (Lat1>0.0) {
               lat1 = (90.0 - Lat1) * DEG2RAD;
               /* Cone = 1.0; */
            }
            else {
               lat1 = (90.0 + Lat1) * DEG2RAD;
               /* Cone = -1.0; */
            }
            Cone = cos(lat1); /* WLH 9-27-96 */
            Hemisphere = 1.0;
         }
         else {
            /* general Lambert conformal */
            float a, b;
            if (sign(Lat1) != sign(Lat2)) {
               printf("Error: standard latitudes must have the same sign.\n");
               exit(1);
            }
            if (Lat1<Lat2) {
               printf("Error: Lat1 must be >= Lat2\n");
               exit(1);
            }
            Hemisphere = 1.0;
            lat1 = (90.0 - Lat1) * DEG2RAD;
            lat2 = (90.0 - Lat2) * DEG2RAD;
            a = log( sin(lat1) ) - log( sin(lat2) );
            b = log( tan(lat1/2.0) ) - log( tan(lat2/2.0) );
            Cone = a / b;
         }

         /* Cone is in [-1,1] */
         ConeFactor = EARTH_RADIUS * sin(lat1)
                          / (ColInc * Cone * pow(tan(lat1/2.0), Cone) );
#undef Lat1
#undef Lat2
#undef PoleRow
#undef PoleCol
#undef CentralLon
#undef ColInc
#undef Hemisphere
#undef ConeFactor
#undef Cone
         break;

      default:
         /* No auxillary args */
         proj->AuxArgs = NULL;
   }

}



/*
 * Return a pointer to a map projection structure given the arguments
 * below.  If this is a new projection we'll allocate a new projection
 * struct and return a pointer to it.  If this projection is already in
 * the list, just return a pointer to it.
 * Input:  kind - the type of projection, one of PROJ_*
 *         nr, nc - number of rows and columns of data
 *         args - array of projection parameters
 * Return:  pointer to a projection struct.
 */
struct projection *new_projection( struct grid_db *db, int kind,
                                   int nr, int nc, float *args )
{
   int p, i, nargs;

   /* determine how many arguments are in the args array */
   switch (kind) {
      case PROJ_GENERIC:  nargs = 4;        break;
      case PROJ_LINEAR:   nargs = 4;        break;
      case PROJ_LAMBERT:  nargs = 6;        break;
      case PROJ_STEREO:   nargs = 5;        break;
      case PROJ_ROTATED:  nargs = 7;        break;
      case PROJ_EPA:      nargs = nr*nc*2;  break;
      default:
         printf("Fatal error in new_projection!\n");
         exit(-1);
   }

   /* Search projection list for a possible match */
   for (p=0; p<db->NumProj; p++) {
      if (   db->ProjList[p]->Kind==kind
          && db->ProjList[p]->Nr==nr
          && db->ProjList[p]->Nc==nc) {
         int same = 1;
         for (i=0;i<nargs;i++) {
            if ( !equal(args[i], db->ProjList[p]->Args[i]) ) {
               same = 0;
               break;
            }
         }
         if (same) {
            return db->ProjList[p];
         }
      }
   }

   /* if we get here, the projection is not in the list, make a new one */
   if (db->NumProj<IMAXPROJ) {
      struct projection *newp;
      newp = (struct projection *) calloc( 1, sizeof(struct projection) );
      newp->Kind = kind;
      newp->Nr = nr;
      newp->Nc = nc;
      newp->Args = (float *) MALLOC( nargs * sizeof(float) );
      for (i=0;i<nargs;i++) {
         newp->Args[i] = args[i];
      }
      /* compute extra, optional proj args */
      compute_aux_proj_args( newp );
      /* add to end of list */
      db->ProjList[db->NumProj] = newp;
      db->NumProj++;
      return newp;
   }
   else {
      printf("Error:  too many map projections, %d is limit\n", IMAXPROJ );
      return NULL;
   }
}



/*
 * Deallocate a map projection structure.
 */
void free_projection( struct grid_db *db, struct projection *proj )
{
   int i, j;

   assert( db );
   assert( proj );

   for (i=0; i<db->NumProj; i++) {
      if (db->ProjList[i]==proj) {
         /* found it, remove from list */
         for (j=i; j<db->NumProj-1; j++) {
            db->ProjList[j] = db->ProjList[j+1];
         }
         db->NumProj--;
         break;
      }
   }
   free( proj->Args );
   free( proj );
}




/*
 * Given a pointer to a map projection struct, return the numeric position of
 * it in the linked list.  The first projection being number 1.
 * Input:  proj - pointer to a map projection struct
 * Return:  position of proj in list starting at one, else 0 if not in list
 */
int lookup_proj( struct grid_db *db, struct projection *proj )
{
   int i;

   for (i=0;i<db->NumProj;i++) {
      if (db->ProjList[i]==proj) {
         return i+1;
      }
   }
   return 0;
}





#define MAX_PROJ_CHARS 1000

char **sprint_projection_list( struct grid_db *db )
{
   struct projection *p;
   int i;
   char **list;

   /* construct array of pointers to strings */
   if (db->NumProj==0) {
      return NULL;
   }

   list = (char **) MALLOC( db->NumProj * sizeof(char *) );
   for (i=0; i<db->NumProj; i++) {
      p = db->ProjList[i];
      list[i] = (char*) MALLOC( MAX_PROJ_CHARS );
      switch (p->Kind) {
         case PROJ_GENERIC:
            sprintf( list[i], "%3d Generic Linear    %4d %4d   %g %g %g %g",
                     i+1, p->Nr, p->Nc, p->Args[0], p->Args[1], p->Args[2],
                     p->Args[3] );
            break;
         case PROJ_LINEAR:
            sprintf( list[i], "%3d Cyl. Equidistant  %4d %4d   %g %g %g %g",
                     i+1, p->Nr, p->Nc, p->Args[0], p->Args[1], p->Args[2],
                     p->Args[3] );
            break;
         case PROJ_LAMBERT:
            sprintf( list[i],
                     "%3d Lambert Conformal %4d %4d   %g %g %g %g %g %g",
                     i+1, p->Nr, p->Nc, p->Args[0], p->Args[1], p->Args[2],
                     p->Args[3], p->Args[4], p->Args[5] );
            break;
         case PROJ_STEREO:
            sprintf( list[i], "%3d Stereographic     %4d %4d   %g %g %g %g %g",
                     i+1, p->Nr, p->Nc, p->Args[0], p->Args[1], p->Args[2],
                     p->Args[3], p->Args[4] );
            break;
         case PROJ_ROTATED:
            sprintf( list[i],
                     "%3d Rotated           %4d %4d   %g %g %g %g %g %g %g",
                     i+1, p->Nr, p->Nc, p->Args[0], p->Args[1], p->Args[2],
                     p->Args[3], p->Args[4], p->Args[5], p->Args[6] );
            break;
         case PROJ_EPA:
            sprintf( list[i], "%3d EPA               %4d %4d",
                     i+1, p->Nr, p->Nc );
            break;
         default:
            sprintf( list[i], "Error!" );
      }
   }

   return list;
}


void print_projection_list( struct grid_db *db )
{
   struct projection *p;
   int i;
   char **list;

   /* construct array of pointers to strings */
   for (i=0; i<db->NumProj; i++) {
      p = db->ProjList[i];
      if (db->ProjSelected[i]) {
         printf("* ");
      }
      else {
         printf("  ");
      }

      switch (p->Kind) {
         case PROJ_GENERIC:
             printf( "%3d Generic Linear    %4d %4d   %g %g %g %g\n",
                     i+1, p->Nr, p->Nc, p->Args[0], p->Args[1], p->Args[2],
                     p->Args[3] );
            break;
         case PROJ_LINEAR:
            printf( "%3d Cyl. Equidistant  %4d %4d   %g %g %g %g\n",
                     i+1, p->Nr, p->Nc, p->Args[0], p->Args[1], p->Args[2],
                     p->Args[3] );
            break;
         case PROJ_LAMBERT:
            printf( "%3d Lambert Conformal %4d %4d   %g %g %g %g %g %g\n",
                    i+1, p->Nr, p->Nc, p->Args[0], p->Args[1], p->Args[2],
                    p->Args[3], p->Args[4], p->Args[5] );
            break;
         case PROJ_STEREO:
            printf( "%3d Stereographic     %4d %4d   %g %g %g %g %g\n",
                     i+1, p->Nr, p->Nc, p->Args[0], p->Args[1], p->Args[2],
                     p->Args[3], p->Args[4] );
            break;
         case PROJ_ROTATED:
            printf( "%3d Rotated           %4d %4d   %g %g %g %g %g %g %g\n",
                     i+1, p->Nr, p->Nc, p->Args[0], p->Args[1], p->Args[2],
                     p->Args[3], p->Args[4], p->Args[5], p->Args[6] );
            break;
         case PROJ_EPA:
            printf( "%3d EPA               %4d %4d\n",
                     i+1, p->Nr, p->Nc );
            break;
         default:
            assert(1==0);
      }
   }
}


#ifdef LEAVEOUT
void print_projection_list( db )
struct grid_db *db;
{
   int i;
   struct projection *p;

   printf("Projection list:\n");

   for (i=0;i<db->NumProj;i++) {
      p = db->ProjList[i];
      switch (p->Kind) {
         case PROJ_GENERIC:  printf("Generic ");  break;
         case PROJ_LINEAR:   printf("Linear  ");  break;
         case PROJ_LAMBERT:  printf("Lambert ");  break;
         case PROJ_STEREO:   printf("Stereo  ");  break;
         case PROJ_EPA:      printf("Epa     ");  break;
         default:            printf("Error!  ");
      }
      printf("%d x %d ", p->Nr, p->Nc );
      printf("%g %g %g\n", p->Args[0], p->Args[1], p->Args[2] );
   }
}
#endif



/*
 * Return a pointer to a vcs structure given the arguments below.  If
 * this is a new projection we'll allocate a new vcs struct and return a
 * pointer to it.  If this vertical coordinate system is already in the
 * list, just return a pointer to it.
 * Input:  kind - the type of vcs, one of VERT_*
 *         nl - total number of levels
 *         lowlev - location of first grid level, below is missing data
 *                  Note: nl-lowlev = actual number of grid levels
 *         args - array of vcs parameters
 * Return:  pointer to a vcs struct.
 */
struct vcs *new_vcs( struct grid_db *db, int kind, int nl, int lowlev,
                     float *args )
{
   int v, i, nargs;

   assert(db);
   assert(args);

   /* determine how many arguments are in the args array */
   switch (kind) {
      case VERT_GENERIC:     nargs = 2;   break;
      case VERT_EQUAL_KM:    nargs = 2;   break;
      case VERT_UNEQUAL_KM:  nargs = nl;  break;
#if V5D_VERSION >= 42
      case VERT_UNEQUAL_MB:  nargs = nl;  break;
#endif
      case VERT_EPA:         nargs = nl;  break;
      default:
         printf("Fatal error in new_vcs!\n");
         exit(-1);
   }

   /* verify arguments are ok */
   if (kind==VERT_UNEQUAL_KM) {
      for (i=1;i<nargs;i++) {
         if (args[i]<=args[i-1]) {
            printf("Error in VCS, heights should increase:");
            printf(" hgt[%d]=%g hgt[%d]=%g\n", i-1, args[i-1], i, args[i] );
            return NULL;
         }
      }
   }
   if (kind==VERT_UNEQUAL_MB) {
      for (i=1;i<nargs;i++) {
         if (args[i]<=args[i-1]) {
            printf("Error in VCS, pressures should increase:");
            printf(" hgt[%d]=%g hgt[%d]=%g\n", i-1,
                   height_to_pressure(args[i-1]), i,
                   height_to_pressure(args[i]) );
            return NULL;
         }
      }
   }
   else if (kind==VERT_EQUAL_KM) {
      if (args[1]<0.0) {
         printf("Error in VCS, increment can't be negative: %g\n", args[1]);
         return NULL;
      }
   }

   /* Search list for this vcs */
   for (v=0; v<db->NumVcs; v++) {
      if (   db->VcsList[v]->Kind==kind
          && db->VcsList[v]->Nl==nl
          && db->VcsList[v]->LowLev==lowlev) {
         int same = 1;
         for (i=0;i<nargs;i++) {
            if (!equal(args[i],db->VcsList[v]->Args[i])) {
               same = 0;
               break;
            }
         }
         if (same) {
            return db->VcsList[v];
         }
      }
   }

   /* if we get here, the vcs is not in the list, make a new one */
   if (db->NumVcs<IMAXPROJ) {
      struct vcs *newv;
      newv = (struct vcs *) calloc( 1, sizeof(struct vcs) );
      newv->Kind = kind;
      newv->Nl = nl;
      newv->LowLev = lowlev;
      newv->Args = (float *) MALLOC( nargs * sizeof(float) );
      for (i=0;i<nargs;i++) {
         newv->Args[i] = args[i];
      }
      /* add to end of list */
      db->VcsList[db->NumVcs] = newv;
      db->NumVcs++;
      return newv;
   }
   else {
      printf("Error: too many vertical coordinate systems, %d is limit\n",
             IMAXPROJ );
      return NULL;
   }
}



/*
 * Deallocate a VCS struct.
 */
void free_vcs( struct grid_db *db, struct vcs *vcs )
{
   int i, j;

   assert( db );
   assert( vcs );

   for (i=0; i<db->NumVcs; i++) {
      if (db->VcsList[i]==vcs) {
         /* found it, remove from list */
         for (j=i; j<db->NumVcs-1; j++) {
            db->VcsList[j] = db->VcsList[j+1];
         }
         db->NumVcs--;
         break;
      }
   }
   free( vcs->Args );
   free( vcs );
}



char **sprint_vcs_list( struct grid_db *db )
{
   struct vcs *v;
   int i;
   char **list;

   /* construct array of pointers to strings */
   if (db->NumVcs==0) {
      return NULL;
   }

   list = (char **) MALLOC( db->NumVcs * sizeof(char *) );
   for (i=0; i<db->NumVcs; i++) {
      v = db->VcsList[i];
      list[i] = (char*) MALLOC( MAX_PROJ_CHARS );
      switch (v->Kind) {
         case VERT_GENERIC:
            sprintf( list[i], "%3d Generic Linear             %4d   %g %g",
                     i+1, v->Nl, v->Args[0], v->Args[1] );
            break;
         case VERT_EQUAL_KM:
            sprintf( list[i], "%3d Equally-spaced Linear km   %4d   %g %g",
                     i+1, v->Nl, v->Args[0], v->Args[1] );
            break;
         case VERT_UNEQUAL_KM:
            sprintf( list[i], "%3d Unequally-spaced Linear km %4d   %g %g",
                     i+1, v->Nl, v->Args[0], v->Args[1] );
            break;
         case VERT_UNEQUAL_MB:
            sprintf( list[i], "%3d Unequally-spaced Pressure mb %4d   %g %g",
                     i+1, v->Nl, height_to_pressure(v->Args[0]),
                     height_to_pressure(v->Args[1]) );
            break;
         default:
            sprintf( list[i], "Error!" );
      }
   }

   return list;
}




void print_vcs_list( struct grid_db *db )
{
   struct vcs *v;
   int i;

   /* construct array of pointers to strings */
   for (i=0; i<db->NumVcs; i++) {
      v = db->VcsList[i];
      if (db->VcsSelected[i]) {
         printf("* ");
      }
      else {
         printf("  ");
      }

      switch (v->Kind) {
         case VERT_GENERIC:
            printf( "%3d Generic Linear             %4d   %g %g\n",
                    i+1, v->Nl, v->Args[0], v->Args[1] );
            break;
         case VERT_EQUAL_KM:
            printf( "%3d Equally-spaced Linear km   %4d   %g %g\n",
                    i+1, v->Nl, v->Args[0], v->Args[1] );
            break;
         case VERT_UNEQUAL_KM:
            printf( "%3d Unequally-spaced Linear km %4d   %g %g\n",
                    i+1, v->Nl, v->Args[0], v->Args[1] );
            break;
         case VERT_UNEQUAL_MB:
            printf( "%3d Unequally-spaced Pressure mb %4d   %g %g\n",
                    i+1, v->Nl, height_to_pressure(v->Args[0]),
                    height_to_pressure(v->Args[1]) );
            break;
         case VERT_EPA:
            printf( "%3d EPA                        %4d\n",
                    i+1, v->Nl );
            break;
         default:
            assert(1==0);
      }
   }
}



/*
 * Given a pointer to a vcs struct, return the numeric position of
 * it in the linked list.  The first vcs being number 1.
 * Input:  db - the grid data base
 *         proj - pointer to a vcs struct
 * Return:  position of vcs in list starting at one, else 0 if not in list
 */
int lookup_vcs( struct grid_db *db, struct vcs *vcs )
{
   int i;

   for (i=0; i<db->NumVcs; i++) {
      if (db->VcsList[i]==vcs) {
         return i+1;
      }
   }
   return 0;
}



/*#define MAXLEVELS 100*/

/*
 * Combine all 1-level VCSs into one new coordinate system
 */
struct vcs *combine_vcs( struct grid_db *db, int kind )
{
   struct vcs *v;
   int count;
   float height[MAXLEVELS];
   struct vcs *vcsentry[MAXLEVELS];
   int i, j;

   count = 0;
   for (i=0; i<db->NumVcs; i++) {
      v = db->VcsList[i];
      if (v->Kind==kind && v->Nl==1) {
         height[count] = v->Args[0];
         vcsentry[count] = v;
         count++;
      }
   }

   printf("level  height\n");
   for (i=0;i<count;i++) {
      printf("%3d    %7g\n", i, height[i] );
   }

   /* bubble sort the heights */
   for (i=0;i<count-1;i++) {
      for (j=i+1;j<count;j++) {
         if (height[i]>height[j]) {
            float tmp;
            struct vcs *u;
            tmp = height[i];
            height[i] = height[j];
            height[j] = tmp;
            u = vcsentry[i];
            vcsentry[i] = vcsentry[j];
            vcsentry[j] = u;
         }
      }
   }

   printf("level  height\n");
   for (i=0;i<count;i++) {
      printf("%3d    %7g\n", i, height[i] );
   }

   if (kind==VERT_GENERIC) {
      return new_vcs( db, VERT_GENERIC, count, 0, height );
   }
   else if (kind==VERT_EQUAL_KM || kind==VERT_UNEQUAL_KM) {
      return new_vcs( db, VERT_UNEQUAL_KM, count, 0, height );
   }
#if V5D_VERSION >= 42
   else if (kind==VERT_UNEQUAL_MB) {
      return new_vcs( db, VERT_UNEQUAL_MB, count, 0, height );
   }
#endif
   else {
      printf("problem in combine_vcs()!\n");
      return NULL;
   }
}
