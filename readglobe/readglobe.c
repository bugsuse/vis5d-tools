/*

    Copyright (C) 2001  Davide Cesari

    Questo programma è software libero; è lecito ridistribuirlo e/o
    modificarlo secondo i termini della Licenza Pubblica Generica SMR
    come pubblicata da ARPA SMR versione 1.
    
    Questo programma è distribuito nella speranza che sia utile, ma
    SENZA ALCUNA GARANZIA; senza neppure la garanzia implicita di
    COMMERCIABILITÀ o di APPLICABILITÀ PER UN PARTICOLARE SCOPO. Si
    veda la Licenza Pubblica Generica SMR per avere maggiori dettagli.
    
    Ognuno dovrebbe avere ricevuto una copia della Licenza Pubblica
    Generica SMR insieme a questo programma; in caso contrario, la si
    può ottenere da Agenzia Regionale Prevenzione e Ambiente (ARPA)
    Servizio Meteorologico Regionale (SMR), Viale Silvani 6, 40122
    Bologna, Italia
    


    Readglobe: this program reads the GLOBE elevation dataset and writes
    a Vis5d topography file on an arbitrary area of the world.

    For any information about this program please contact:
    Davide Cesari <dcesari@smr.arpa.emr.it>

 */


#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include <math.h>

#ifdef LITTLE
#define MYENDIAN 'I' /*Intel-like, little endian*/
#else
#define MYENDIAN 'M' /*Motorola-like, big endian*/
#endif

char* rbbuff;
double abundo=1.0, deficio=0.;

struct usgshdr /*Use double for accuracy*/
{int nlon, nlat, nbytes, nblon, nodata;
  double lonmin, lonmax, latmin, latmax, dlon, dlat;
  char file[81], endian;
};

struct myhdr /*Use double for accuracy*/
{int nlon, nlat, xstart, xpos, xend, ystart, ypos, yend;
  double lonmin, lonmax, latmin, latmax, dlon, dlat;
};


int gettopodata(struct usgshdr *ihdr, struct myhdr *ohdr,
		float* data, float seaval)
{FILE* datafd;
 int i,j,k,l, bystart, byend;
 
 if ((datafd = fopen(ihdr->file,"rb")) == NULL)
   {perror (ihdr->file);
   return 0;}

 bystart=ohdr->xstart*ihdr->nbytes;
 byend=ohdr->xend*ihdr->nbytes;
 l=ohdr->ypos;
 for(j=0; j<ohdr->yend; j++)
   {if (fread(rbbuff, 1, ihdr->nblon, datafd) <=0)
     {perror ("fread");
     return 0;}
   if (j>=ohdr->ystart)
     {/*k=(l+j-ohdr->ystart)*ohdr->nlon+ohdr->xpos;*/
     k=l*ohdr->nlon+ohdr->xpos;
     if (ihdr->nbytes == 2)
       {if (MYENDIAN != ihdr->endian)
	 flip2((short int *) rbbuff+ohdr->xstart,
	       (short int *) rbbuff+ohdr->xstart,
	       ohdr->xend-ohdr->xstart);
       for(i=bystart; i<byend; i+=2)
         {*(data+k)=(float) (*(short int *)(rbbuff+i));
	 if (abs(*(data+k) - ihdr->nodata) < 1.) *(data+k) = seaval;
	 k++;}
       }
     else if (ihdr->nbytes == 1)
       for(i=bystart; i<byend; i++)
	 {*(data+k)=(float) ((char) *(rbbuff+i));
	 if (abs(*(data+k) - ihdr->nodata) < 1.) *(data+k) = seaval;
	 k++;}
     l++;
     }
   }
 close(datafd);
}



int useful (struct usgshdr *ihdr, struct myhdr *ohdr, int adjust)
{int i;
 double b, epsx, epsy;
 if (ohdr->lonmin < ihdr->lonmax && ohdr->lonmax > ihdr->lonmin &&
     ohdr->latmin < ihdr->latmax && ohdr->latmax > ihdr->latmin)
   {epsx=ihdr->dlon/10.;
   epsy=ihdr->dlat/10.;
   if (adjust) /*Adjust output grid extremes to fit to input*/

     {i=(ohdr->lonmin-ihdr->lonmin)/ihdr->dlon;
     b=ihdr->lonmin+i*ihdr->dlon;
     if (b > ohdr->lonmin)
       ohdr->lonmin = b - abundo*ihdr->dlon;
     else if (b < ohdr->lonmin)
       ohdr->lonmin = b + deficio*ihdr->dlon;

     i=(ohdr->lonmax-ihdr->lonmin)/ihdr->dlon;
     b=ihdr->lonmin+i*ihdr->dlon;
     if (b < ohdr->lonmax)
       ohdr->lonmax = b + abundo*ihdr->dlon;
     else if (b > ohdr->lonmax)
       ohdr->lonmax = b - deficio*ihdr->dlon;

     i=(ohdr->latmin-ihdr->latmin)/ihdr->dlat;
     b=ihdr->latmin+i*ihdr->dlat;
     if (b > ohdr->latmin)
       ohdr->latmin = b - abundo*ihdr->dlat;
     else if (b < ohdr->latmin)
       ohdr->latmin = b + deficio*ihdr->dlat;;

     i=(ohdr->latmax-ihdr->latmin)/ihdr->dlat;
     b=ihdr->latmin+i*ihdr->dlat;
     if (b < ohdr->latmax)
       ohdr->latmax = b + abundo*ihdr->dlat;
     else if (b > ohdr->latmax)
       ohdr->latmax = b - deficio*ihdr->dlat;

     ohdr->dlon = ihdr->dlon;
     ohdr->dlat = ihdr->dlat;
     ohdr->nlon = (ohdr->lonmax - ohdr->lonmin + epsx)/ihdr->dlon + 1;
     ohdr->nlat = (ohdr->latmax - ohdr->latmin + epsy)/ihdr->dlat + 1;
     }

   ohdr->xstart = (ohdr->lonmin-ihdr->lonmin+epsx) / ihdr->dlon;
   if (ohdr->xstart < 0)
     {ohdr->xpos = 1 - ohdr->xstart;
     ohdr->xstart = 0;}
   else
     {ohdr->xpos = 0;}
   ohdr->xend = (ohdr->lonmax-ihdr->lonmin+epsx) / ihdr->dlon + 1;
   if (ohdr->xend > ihdr->nlon) ohdr->xend=ihdr->nlon;

   /*   ohdr->ystart = (ohdr->latmin-ihdr->latmin+epsy) / ihdr->dlat;
   if (ohdr->ystart < 0)
     {ohdr->ypos = 1 - ohdr->ystart;
     ohdr->ystart = 0;}
   else
     {ohdr->ypos = 0;}
   ohdr->yend = (ohdr->latmax-ihdr->latmin+epsy) / ihdr->dlat + 1;
   if (ohdr->yend > ihdr->nlat) ohdr->yend=ihdr->nlat;*/

   ohdr->ystart = (ihdr->latmax-ohdr->latmax+epsy) / ihdr->dlat;
   if (ohdr->ystart < 0)
     {ohdr->ypos = 1 - ohdr->ystart;
     ohdr->ystart = 0;}
   else
     {ohdr->ypos = 0;}
   ohdr->yend = (ihdr->latmax-ohdr->latmin+epsy) / ihdr->dlat + 1;
   if (ohdr->yend > ihdr->nlat) ohdr->yend=ihdr->nlat;

   printf("X: %d %d %d\n",ohdr->xstart,ohdr->xend,ohdr->xpos);
   printf("Y: %d %d %d\n",ohdr->ystart,ohdr->yend,ohdr->ypos);
   return 1;
   }
 else
   printf("Unused\n");
 return 0;
}

int getusgshdr (struct usgshdr *hdr, char* hdrfile)
{FILE* hdrfd;
 char bufin[BUFSIZ], key[41], *p;
 /*Set defaults*/
 hdr->endian='I';/*I=Intel,M=Motorola?!*/
 hdr->nlon=0;
 hdr->nlat=0;
 hdr->nbytes=2;
 hdr->nblon=0;
 hdr->lonmin=0.;
 hdr->lonmax=0.;
 hdr->latmin=0.;
 hdr->latmax=0.;
 hdr->dlon=0.;
 hdr->dlat=0.;
 /*Read header file*/
 if ((hdrfd = fopen(hdrfile,"r")) == NULL)
   {perror (hdrfile);
   return 0;}
 while (fgets(bufin,BUFSIZ,hdrfd) != NULL)
   {sscanf(bufin, "%40s", key);
   if (!strcmp(key,"BYTEORDER"))
     sscanf(bufin, "%40s %c", key,&hdr->endian);
   else if (!strcmp(key,"NCOLS"))
     sscanf(bufin, "%40s %d", key,&hdr->nlon);
   else if (!strcmp(key,"NROWS"))
     sscanf(bufin, "%40s %d", key,&hdr->nlat);
   else if (!strcmp(key,"NBITS"))
     {sscanf(bufin, "%40s %d", key,&hdr->nbytes);
     hdr->nbytes = hdr->nbytes/8;}
   else if (!strcmp(key,"BANDROWBYTES"))
     sscanf(bufin, "%40s %d", key,&hdr->nblon);
   else if (!strcmp(key,"NODATA"))
     sscanf(bufin, "%40s %d", key,&hdr->nodata);
   else if (!strcmp(key,"ULXMAP"))
     sscanf(bufin, "%40s %lf", key,&hdr->lonmin);
   else if (!strcmp(key,"ULYMAP"))
     sscanf(bufin, "%40s %lf", key,&hdr->latmax);
   else if (!strcmp(key,"XDIM"))
     sscanf(bufin, "%40s %lf", key,&hdr->dlon);
   else if (!strcmp(key,"YDIM"))
     sscanf(bufin, "%40s %lf", key,&hdr->dlat);
   else if (!strcmp(key,"FILE"))
     sscanf(bufin, "%40s %80s", key,&hdr->file);
   }
 close(hdrfd);
 /*Really required*/
 if (hdr->dlon == 0. || hdr->dlat == 0.) return 0;
 /*Other types unsupported*/
 if (hdr->nbytes < 1 || hdr-> nbytes > 2 )return 0;
 /*Additional computations*/
 if ((p=strstr(hdr->file,".hdr")) != NULL) *p='\0'; /*bug workaround*/
 hdr->lonmax = hdr->lonmin + (hdr->nlon-1)*hdr->dlon;
 hdr->latmin = hdr->latmax - (hdr->nlat-1)*hdr->dlat;
 if (hdr->nblon == 0) hdr->nblon = hdr->nlon * hdr->nbytes;
 /* hdr->nbrows = hdr->nrows * hdr->nbytes;*/
 /*Print report*/
 printf("\nHeader file: %s --> data file: %s\n",hdrfile,hdr->file);
 printf("Boundaries: %lf %lf %lf %lf\n",hdr->lonmin,hdr->lonmax,
	hdr->latmin,hdr->latmax);
 printf("Size: %d %d\n",hdr->nlon,hdr->nlat);

 return 1;
}


int main(int argc, char* argv[])
{int i,j,k,l,ox,oy, argcc, adjust = 1, average = 1, av2, errstat = 2;
 struct usgshdr ihdr;
 struct myhdr ohdr, oihdr;
 float *odata, *oidata, av, seaval = 0.; /*Use float for space*/
 char ofile[81];
 FILE* ofilep;

 strcpy(ofile,"EARTH.TOPO");
 argcc=1;
 /*Parse arguments and set ohdr*/
 while (argcc<argc)
   {if (!strcmp("-b",argv[argcc]))
     {argcc++;
     sscanf(argv[argcc],"%lf",&ohdr.lonmin);
     argcc++;
     sscanf(argv[argcc],"%lf",&ohdr.lonmax);
     argcc++;
     sscanf(argv[argcc],"%lf",&ohdr.latmin);
     argcc++;
     sscanf(argv[argcc],"%lf",&ohdr.latmax);
     errstat--;
     }
   else if (!strcmp("-o",argv[argcc])) /*Output file name*/
     {argcc++;
     strcpy(ofile,argv[argcc]);}
   else if (!strcmp("-s",argv[argcc])) /*Shrink area*/
     {abundo=0.0;
     deficio=1.0;}
   else if (!strcmp("-a",argv[argcc])) /*Average over n*n points*/
     {argcc++;
     sscanf(argv[argcc],"%d",&average);}
   else if (!strcmp("-m",argv[argcc])) /*Value for sea (missing/mare) points*/
     {argcc++;
     sscanf(argv[argcc],"%f",&seaval);}
   else
     break;
   argcc++;
   }

 if (argcc<argc) errstat--; /* Are there still arguments available? */

 if (errstat)
   {printf("Error in command line, syntax:\n\n");
   printf("%s -b <lonmin> <lonmax> <latmin> <latmax> [options] \\\n", argv[0]);
   printf(" file1.hdr [file2.hdr ...]\n\n");

   printf("options currently available are:\n");
   printf("-o <output file> name differently the output file (default EARTH.TOPO)\n");
   printf("-s               shrink the requested area to the innner closest gridpoints\n");
   printf("                 rather than enlarging it (as done by default)\n");
   printf("-a <n>           average the data over nXn points boxes\n");
   printf("-m <z>           set the height of the missing (sea) points to <z>, default 0.\n");
   exit(1);}

 /*Loop over header files*/
 for (; argcc<argc; argcc++)
   {if (getusgshdr (&ihdr, argv[argcc]) > 0) /*Read header file*/
     if (useful(&ihdr, &ohdr, adjust) > 0) /*Check whether we need it*/
       {if (adjust) /*First time allocate space*/
	 {adjust=0;
	 if ((odata=malloc(sizeof(float)*ohdr.nlon*ohdr.nlat)) == NULL)
	   {perror("malloc");
	   exit (1);}
	 if ((rbbuff=malloc(ihdr.nblon)) == NULL)
	   {perror("malloc");
	   exit (1);}
	 }
       gettopodata(&ihdr, &ohdr, odata, seaval); /*Read into matrix*/
       }
   }
 free(rbbuff);
 printf("\nOutput grid:\n");
 printf("Boundaries: %lf %lf %lf %lf\n",ohdr.lonmin,ohdr.lonmax,
	ohdr.latmin,ohdr.latmax);
 printf("Size: %d %d\n",ohdr.nlon,ohdr.nlat);

 if (average > 1)
   {oihdr.nlon  = ohdr.nlon/average; /*Define averaged grid*/
   oihdr.nlat   = ohdr.nlat/average;
   oihdr.lonmin = ohdr.lonmin+(average-1)*ohdr.dlon*0.5;
   oihdr.latmax = ohdr.latmax-(average-1)*ohdr.dlat*0.5;
   oihdr.dlon   = ohdr.dlon*average;
   oihdr.dlat   = ohdr.dlat*average;
   oihdr.lonmax = oihdr.lonmin+oihdr.dlon*(oihdr.nlon-1);
   oihdr.latmin = oihdr.latmax-oihdr.dlat*(oihdr.nlat-1);
   if ((oidata=malloc(sizeof(float)*oihdr.nlon*oihdr.nlat)) == NULL)
     {perror("malloc");
     exit (1);}
   av2 = average*average;
   for (j=0;j<oihdr.nlat;j++)  /*Average loop*/
     for (i=0;i<oihdr.nlon;i++)
       {av=0.;
       ox=i*average;
       oy=j*average;
       for (l=oy;l<oy+average;l++)
	 for (k=ox;k<ox+average;k++)
	   av=av+*(odata+l*ohdr.nlon+k);
       *(oidata+j*oihdr.nlon+i)=av/av2;}

   printf("\nAveraged grid:\n");
   printf("Boundaries: %lf %lf %lf %lf\n",oihdr.lonmin,oihdr.lonmax,
	  oihdr.latmin,oihdr.latmax);
   printf("Size: %d %d\n",oihdr.nlon,oihdr.nlat);
   }
 else
   {memcpy(&oihdr, &ohdr, sizeof(ohdr));
   oidata=odata;
   }

 printf("Output file: %s\n",ofile);

 write_topo(ofile, -oihdr.lonmin, -oihdr.lonmax, oihdr.latmax, oihdr.latmin,
	    oihdr.nlat, oihdr.nlon, oidata);

 /* internal use
 if ((ofilep = fopen("mnt.bin","w")) == NULL) perror("readglobe");
 fwrite (&oihdr.nlon, sizeof(int), 1,ofilep);
 fwrite (&oihdr.nlat, sizeof(int), 1,ofilep);
 av=average*1000.*cos((oihdr.latmin+oihdr.latmax)*.5*M_PI/180.);
 printf("%f\n",av);
 fwrite (&av, sizeof(float), 1,ofilep);
 av=average*1000.;
 printf("%f\n",av);
 fwrite (&av, sizeof(float), 1,ofilep);

 for (i=0;i<oihdr.nlat;i++)
   fwrite (oidata+oihdr.nlon*(oihdr.nlat-i-1), sizeof(float), oihdr.nlon,ofilep);
 fclose(ofilep);
  */
}

