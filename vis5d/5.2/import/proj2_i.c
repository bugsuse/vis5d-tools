/* proj2.c */



/* Map projections: */
#define PROJ_GENERIC                 0  /* No specific units */
#define PROJ_LINEAR                 1  /* Cylindrical-Equidistant (old vis5d) */
#define PROJ_LAMBERT                 2  /* Lambert conformal */
#define PROJ_STEREO                 3  /* Stereographic */
#define PROJ_EPA                10  /* Weird EPA system */



/*
 * This structure describes the projection of an input grid AND information
 * on how to remap data to the output projection.
 */
struct projection {
   int Type;                        /* One of PROJ_* as seen above */

   /* if type==PROJ_EPA */
        /* Obtained from input file or default: */
        float *Latitude;        /* Latitude[inR][inC] */
        float *Longitude;        /* Longitude[inR][inC] */
        float *Sigma;                /* Sigma[inL] */

        /* Used to resample to output grid: */
        int *RectRow;                /* RectRow[outR][outC] */
        int *RectCol;                /* RectCol[outR][outC] */
        float *Coef_s;                /* Coef_s[outR][outC] */
        float *Coef_t;                /* Coef_t[outR][outC] */
        int *Level;                /* Level[outR][outC][outL] */
        float *Coef;                /* Coef[outR][outC][outL] */
   /* endif */
};



/*
 * This struct is used to do resampling.  It contains all data needed
 * to do the (output grid row,col,level) to (input grid row,col,level)
 * conversion.  We perform vertical resampling first, then horizontal
 * resampling.
 */
struct resampler {
        int Initialized;

        /*
         * These values indicate the size of the dynamic arrays below:
         */
        int inR, inC;                /* size of input grid in rows, columns */
        int outR, outC, outL;        /* size of output grid */

        /*
         * For a given level in the output grid, Level indicates which
         * data values to sample in the input grid.  The Coef values are
         * the weights to use in the resampling.  The arrays are also
         * indexed by (row,col) to allow sigma coordinates (relative to
         * topography).
         */
        int DoVertical;                /* Is vertical resampling needed? */
        int *Level;                /* Level[outL][inR][inC] */
        float *Coef;                /* Coef[outL][inR][inC] */

        /*
         * For a given (row,col) in the output grid, RectRow and RectCol
         * indicates which data values to sample in the input grid.  The
         * Coef_s and Coef_t values are the weights to use in the sampling.
         */
        int DoHorizontal;        /* Is horizontal resampling needed? */
        int *RectRow;                /* RectRow[outR][outC] */
        int *RectCol;                /* RectCol[outR][outC] */
        float *Coef_s;                /* Coef_s[outR][outC] */
        float *Coef_t;                /* Coef_t[outR][outC] */
};




/*
 * Return a pointer to an empty resampler struct.  Typically, one
 * resampler will be shared by all grids from the same file.
 */
struct resampler *alloc_resampler()
{
   struct resampler *r;

   r = (struct resampler *) malloc( sizeof(struct resampler) );
   if (r) {
      /* Initialize all fields to 0 */
      memset( r, 0, sizeof(struct resampler) );
   }
   return r;
}



/* Useful macros for indexing the resampling matrices: */
#define LEVEL(R,C,L)   Level[ (C) + ((R) + (L) * r->inR) * r->inC ]
#define COEF(R,C,L)     Coef[ (C) + ((R) + (L) * r->inR) * r->inC ]

#define RECTROW(R,C)   RectRow[ (C) + (R) * r->outC ]
#define RECTCOL(R,C)   RectCol[ (C) + (R) * r->outC ]
#define COEF_S(R,C)     Coef_s[ (C) + (R) * r->outC ]
#define COEF_T(R,C)     Coef_t[ (C) + (R) * r->outC ]


/*
 * Compute all the information needed to resample a grid from the form
 * specifed by g to the grid specified by outrows, outcols, etc.
 */
void init_resampler( r, g, outrows, outcols, outlevels,
                     outproj, outprojargs,
                     outvert, outvertargs )
struct resampler *r;
struct grid_info *g;
int outrows, outcols, outlevels;
int outproj;
float *outprojargs;
int outvert;
float *outvertargs;
{
   assert( r!=NULL );
   assert( g!=NULL );

   r->inR = g->Nr;
   r->inC = g->Nc;
   r->outR = outrows;
   r->outC = outcols;
   r->outL = outlevels;

   if (!same_vertical()) {
      r->DoVertical = 1;
      r->Level = (int *) malloc( g->Nr * g->Nc * outlevels * sizeof(int) );
      r->Coef = (float *) malloc( g->Nr * g->Nc * outlevels * sizeof(float) );

      /* COMPUTE STUFF */

   }

   if (!same_horizontal()) {
      r->DoHorizontal = 1;
      r->RectRow = (int *) malloc( outrows * outcols * sizeof(int) );
      r->RectCol = (int *) malloc( outrows * outcols * sizeof(int) );
      r->Coef_s = (float *) malloc( outrows * outcols * sizeof(float) );
      r->Coef_t = (float *) malloc( outrows * outcols * sizeof(float) );

      /* COMPUTE STUFF */

   }

   r->Initialized = 1;
}
