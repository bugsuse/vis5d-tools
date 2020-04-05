/* Program to convert a vector mapfile to the binary (McIdas) map format
   used by Vis5D. 
   Based on the newmap.c program that is part of:

      vis5d program for visualizing five dimensional gridded data sets
      Copyright (C) 1990, 1991, 1992, 1993  Bill Hibbard, Brian Paul,
      Dave Santek, and Andre Battaiola.

   vec_to_map written by Henk Witte
                         University of Amsterdam
                         The Netherlands
   the vector file follows normal convention, i.e. east is negative.
   Longtitudes are sign changed before they are written to McIdas
   format. The vector file is a sequence of lines, each line starts
   with a linenumber (which is ignored) and number of vertices in the
   line. Following are the specified number of vertices as pairs of
   longtitudes and latitudes. Longtitudes and latitudes are integers
   of lon (or lat) * 10000 (i.e. 180.00 should be given as 1800000)
*/


#include <stdio.h>
#include "../src/binio.h"


struct polyline {
	int	minlat;
	int	maxlat;
	int	minlon;
	int	maxlon;
	int	start;
	int	len;
};


#define MAXLINES 100000
struct polyline MapLine[MAXLINES];
int NumLines = 0;



struct vertex {
	int	lat, lon;
};


#define MAXVERTS 100000
struct vertex VertexList[MAXVERTS];
int NumVertices = 0;


main( argc, argv )
int argc;
char *argv[];
{
   int flag,i;

   if (argc!=2 && argc!=3) {
      printf("Usage:\n");
      printf("    vectormap McIdasmap\n");
      exit(0);
   }

   read_mapfile( argv[1] );

   if (argc==3 && strcmp(argv[2],"-p")==0)
	flag = 1;
   else
	flag = 0;

   /* update polyline offsets */
   for(i=0;i<NumLines;i++)  {
      MapLine[i].start = (MapLine[i].start * 2 + 1 + NumLines * 6);
      printf("%i %i\n",i,MapLine[i].start);
   }
   
   write_mapfile( argv[2] );

   exit(0);
}

read_mapfile( filename )
char *filename;
{
   FILE *f;
   int i,j,start_vert,skip,minlat,maxlat,minlon,maxlon,LineVert;

   f = fopen( filename, "r" );
   if (!f) {
      printf("Error: Unable to open %s for reading\n", filename );
      exit(0);
   }
   start_vert = 0;
   do {
      if(fscanf(f," %i %i",&skip,&LineVert)!=2) {
         close(f);
         return 0;
      }
      NumVertices += LineVert;
      fscanf(f," %i %i",&VertexList[start_vert].lon,
         &VertexList[start_vert].lat);
      minlat = maxlat = VertexList[start_vert].lat;
      minlon = maxlon = VertexList[start_vert].lon; 
      for(i=start_vert+1;i<start_vert+LineVert;i++) {
        fscanf(f," %i %i",&VertexList[i].lon,&VertexList[i].lat);

/* McIdas convention: East is positive, usual East is negative */
        VertexList[i].lon = -VertexList[i].lon;

        if(VertexList[i].lat<minlat)
           minlat = VertexList[i].lat;
        if(VertexList[i].lat>maxlat)
           maxlat = VertexList[i].lat;
        if(VertexList[i].lon<minlon)
           minlon = VertexList[i].lon;
        if(VertexList[i].lon>maxlon)
           maxlon = VertexList[i].lon;
      }
      MapLine[NumLines].minlat = minlat;
      MapLine[NumLines].maxlat = maxlat;
      MapLine[NumLines].minlon = minlon;
      MapLine[NumLines].maxlon = maxlon;
      MapLine[NumLines].start = start_vert;
      MapLine[NumLines].len = LineVert*2;
      start_vert += LineVert;
      NumLines++;
   } while(!feof(f));

   close(f);
}

write_mapfile( filename )
char *filename;
{
   int f;

   f = open( filename, O_WRONLY | O_CREAT );
   if (!f) {
      printf("Error:  unable to open %s for writing\n", filename );
      exit(0);
   }

   write_int4( f, NumLines );

   if (write_int4_array( f, MapLine, 6*NumLines ) < 6*NumLines) {
      printf("Error:  bad file write (disk full?)\n");
      exit(0);
   }

   if (write_int4_array( f, VertexList, 2*NumVertices ) < 2*NumVertices) {
      printf("Error:  bad file write (disk full?)\n");
      exit(0);
   }

   close( f );
}


