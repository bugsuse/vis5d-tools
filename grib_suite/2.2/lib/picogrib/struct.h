/*
c    <picogrib software di decodifica grib>
c    Copyright (C) 2000  <SMR ARPA>
c
c    $Date: 2000/04/28 12:54:33 $    $Revision: 1.5 $
c    $Id: struct.h,v 1.5 2000/04/28 12:54:33 lambo Exp $

c    Questo programma ÅË software  libero; ÅË lecito ridistribuirlo e/o
c    modificarlo secondo i  termini della Licenza Pubblica Generica SMR 
c    come pubblicata  da ARPA SMR ; riferirsi alla versione 1
c    della licenza o (a scelta) ad una versione successiva.

c    Questo programma ÅË distribuito  nella speranza che sia utile,  ma
c    SENZA  ALCUNA GARANZIA;  senza  neppure la  garanzia  implicita di
c    COMMERCIABILITA' o di APPLICABILITA' PER UN PARTICOLARE SCOPO.  Si
c    veda la Licenza Pubblica Generica SMR per avere maggiori dettagli.

c    Ognuno dovrebbe avere ricevuto una copia della Licenza Pubblica
c    Generica SMR insieme a questo programma; in caso contrario, la si
c    puÅÚ ottenere da Agenzia Regionale Prevenzione e Ambiente (ARPA)
c    Servizio Meteorologico Regionale (SMR), Viale Silvani 6, 40122
c    Bologna, Italia
c    http://www.smr.arpa.emr.it
*/

#define MAXLISTVERCORPAR 300
/*gribint should be at least 4 bytes long and match ForTran default integer type*/
#ifdef DOS
typedef long int gribint;
typedef unsigned long ugribint;
/*#elif defined _CRAY
typedef short gribint;
typedef unsigned short ugribint;*/
#else
typedef int gribint;
typedef unsigned ugribint;
#endif

#ifdef _CRAY
typedef double gribfloat;
#else
typedef float gribfloat;
#endif

struct section0_type {
    char       word_grib[4];    /* 'GRIB' token */
    gribint   total_length;    /* Total length of GRIB message */
    gribint   grib_edition;    /* GRIB edition number */
};

struct section1_type {
    gribint   section_length;  /* Length of section 1 */
    gribint   table2_version;  /* Code table 2, parameter table, Version No. */
    gribint   centre;          /* Identification of centre */
    gribint   process;         /* Generating process identification number */
    gribint   grid_definition; /* Grid definition */
    gribint   flag;            /* Flag */
    gribint   parameter;       /* Indicator of parameter */
    gribint   level_type;      /* Indicator of type of level */
    gribint   measure1;        /* Heigth, pressure, etc. of levels */
    gribint   measure2;        /* Heigth, pressure, etc. of levels */
    gribint   year;            /* Year of century */
    gribint   month;           /* Month */
    gribint   day;             /* Day */
    gribint   hour;            /* Hour */
    gribint   minute;          /* Minute */
    gribint   time_range_unit; /* Indicator of unit of time range */
    gribint   time_period1;    /* P1 - Period of time */
    gribint   time_period2;    /* P2 - Period of time */
    gribint   time_range;	/* Time range indicator */
    gribint   number_in_average;     /* Number included in average */
    gribint   number_not_in_average; /* Number missing from averages or 
					 accumulations */
    gribint   reference_century;     /* Century of reference */
    gribint   reserved1;             /* Reserved */
    gribint   decimal_scale_factor;  /* Units decimal scale factor */
    gribint   reserved2[12];         /* Reserved */
    gribint   local_definition;      /* Local definition number */
    gribint   class;                 /* Class */
    gribint   type;                  /* Type */
    gribint   stream;                /* Stream */
    gribint   experiment_version;    /* Experiment version */
    gribint   ensemble_forecast;     /* Ensemble forecast number */
    gribint   forecasts_total;       /* Total number of forecasts in ensemble */
    gribint   cluster;               /* Cluster number */
    gribint   clusters_total;        /* Total number of of clusters */
    gribint   reserved3;             /* Reserved */
    gribint   clustering_method;     /* Clustering method */
				      /* The next six values are considered
					 when clustering */
    gribint   start_time_step;       /* Start time step */
    gribint   end_time_step;         /* End time step */
    gribint   northern_latitude;     /* Northern latitude of domain */
    gribint   western_longitude;     /* Western longitude of domain */
    gribint   southern_latitude;     /* Southern latitude of domain */
    gribint   eastern_longitude;     /* Eastern longitude of domain */
    gribint   operational_cluster;   /* Number of cluster which operational 
					 cluster belongs to */
    gribint   control_cluster;       /* Number of cluster which control 
					 cluster belongs to */
    gribint   cluster_forecasts;     /* Number of forecasts belonging to 
					 the cluster */
    char       *forecasts_list;       /* Ensemble forecast list */
};

struct section2_type {
    gribint   section_length;        /* Length of section 2 */
    gribint   vertical_parameters;   /* Number of vertical coordinate 
					 parameters */
    gribint   list_position;         /* List file offset */
    gribint   data_representation;   /* Data representation type */
    gribint   parallel_points;       /* Number of points along a parallel */
    gribint   meridian_points;	      /* Number of points along a meridian */
    gribint   first_latitude;        /* Latitude of first grid point */
    gribint   first_longitude;       /* Longitude of first grid point */
    gribint   resolution_and_flags;  /* Resolution and component flags */
    gribint   last_latitude;         /* Latitude of last grid point */
    gribint   last_longitude;        /* Longitude of last grid point */
    gribint   parallel_increment;    /* Parallel direction increment */
    gribint   meridian_increment;    /* Meridian direction increment */
    gribint   scanning_mode;         /* Scanning mode */
    gribint   reserved1[3];          /* Reserved */
    gribint   southern_pole_latitude;      /* Latitude of the southern pole 
					       in millidegrees */
    gribint   southern_pole_longitude;     /* Longitude of the southern pole 
					       in millidegrees */
    float      rotation_angle;              /* Angle of rotation */
    gribint   stretching_pole_latitude;    /* Latitude of pole of stretching 
					       in millidegrees */
    gribint   stretching_pole_longitude;   /* Longitude of pole of stretching 
					       in millidegrees */
    float      stretching_factor;           /* Stretching factor */

    float      lisvercorpar[MAXLISTVERCORPAR];  /* Vertical Coordinate Parameter*/


};

struct section3_type {
    gribint   section_length;  /* Length of section 3 */
    gribint   unused_bits;     /* Unused bits at end of section 3 */
    gribint   bitmap_code;     /* Predetermined bit map number */
    char       *bitmap;         /* Bitmap with a bit to data point 
				   correspondence */
};

struct section4_type {
    gribint   section_length;        /* Length of section 4 */
    gribint   flag_and_unused_bits;  /* Flag */
    gribint   scale_factor;          /* Scale factor */
    float      reference_value;       /* Reference value */
    gribint   bits_number;           /* Number of bits containing each 
					 packed value */
    gribint   packed_values;         /* Number of packed values */
    gribint   unpacked_values;       /* Number of unpacked values */
    gribint   work_with_floats;      /* Working in floating point flag */
    void       *binary_data;          /* Unpacked values */
};

struct section5_type {
    char       end_of_message[4];     /* '7777' token */
};

struct grib_type {
    struct section0_type  section0;
    struct section1_type  section1;
    struct section2_type  section2;
    struct section3_type  section3;
    struct section4_type  section4;
    struct section5_type  section5;
};
