struct Winds {
 int uloc;	/* location of u component in 3-d gridfile */
 int vloc;	/* location of v component in 3-d gridfile */
 int u_id;	/* identification number of u component (33) */
 int v_id;	/* identification number of u component (33) */
 int grid_id;	/* grid type 1-254 for defined type, 255 for GDS definition
		   -1 for not yet defined */
 int projtype;	/* Projection type defined in gridproj.c */
 int nwind;	/* number of winds stored in this structure */
 float mesh;
 float rotat;
 float npi;
 float npj;
};

#ifdef MAIN_R
 struct Winds winds;
#else
 extern struct Winds winds;
#endif
