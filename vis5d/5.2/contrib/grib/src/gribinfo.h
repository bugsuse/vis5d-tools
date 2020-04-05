struct GribInfo {
 int lenpds;	/* length of Product Definition Section in bytes PDS :gribinfo*/
 int locpds;	/* location of Product Definition Section, bytes PDS :gribinfo*/
 int lengds;	/* length of Grid Description Section in bytes GDS :gribinfo*/
 int locgds;	/* location of Grid Description Section in bytes GDS :gribinfo*/
 int lenbds;	/* length of Binary Data Section in bytes BDS :gribinfo*/
 int locbds;	/* location of Binary Data Section in bytes BDS :gribinfo*/
 int lenbms;	/* length of Bit Map Section in bytes BMS :gribinfo*/
 int locbms;	/* location of Bit Map Section in bytes BMS :gribinfo*/
 int grid_id;	/* type of grid, projection from PDS filled: gridinfo */
 int gridsize;	/* number of gridpoints in the grid filled: gridproj */
 int projtype;	/* projection type 1 latlon 2 polarstereographic : gridproj */
 int dim1;	/* the first dimension */
 int dim2;	/* the second dimension */
 int dimrxc;	/* 0 if dim1 is a column, 1 for a row index */
 float mesh;	/* grid mesh length (km at 60 N for nmc grids) */
 float rotat;	/* grid longitude rotation from 80W, NMC polarstereo grids */
 float npi;	/* i location of north pole, NMC convention ln 1 at bottom */
 float npj;	/* j location of north pole */
 float lat1;	/* latitude of northwest corner first gridpoint */
 float lon1;	/* longitude of northwest corner first gridpoint */
 float dlat;	/* incriment of latitude */
 float dlon;	/* incriment of longitude */
 int level;	/* pressure level of grid */
 int windflag;	/* 0 - not a wind field, 1 - a u or v wind */
};

#ifdef MAIN_R
struct GribInfo gi;
#else
extern struct GribInfo gi;
#endif
