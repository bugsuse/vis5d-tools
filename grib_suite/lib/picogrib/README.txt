PICOGRIB 1.1

This software package was developed to supply a multi-platform GRIB C-language
(FORTRAN callable) decoding package compatible to some extent with ECMWF GRIBEX routine.

The kernel of the package was originally written by M.Mazzola and M.Bider at PICOdata 
s.r.l. Milano, Italy and included:

DUMPGRIB a program that allows to view all information contained in GRIB message
         informative sections and the first ten actual  values of the data grid 
SHOWGRIB a program that allows to decode a file of GRIB messages and to  create,
         for each field, a file containing main information and all decompressed data
         associated with their coordinates.
PARTGRIB a program that allows to divide a single file made up of several GRIB messages
         in many files each consisting of a single GRIB message.
GRIBLIB  a routines library used by SHOWGRIB, DUMPGRIB and PARTGRIB to support GRIB
         decoding functions
GRIBEX   a C-language routine emulating the GRIB decoding section of ECMWF GRIBEX FORTAN
         subroutine

This kernel was then revised and integrated by D.Cesari and P.Patruno at ARPA SMR 
Bologna, Italy. Revision resulted in many bugs fixing and in the expansion of the
decoding capabilities of section 1 and 2 in GRIB messages. Integration resulted in the
delopment of module PBUTILS, emulating ECMWF PBIO utilities, which handle GRIB message 
file I/O.

PICOGRIB 1.1 is released under GPL (Generic Public License) by ARPA SMR (See file
licenza.txt for license terms; license.txt is the original GNU license
from witch it derive).

Some documentation in Italian language of PICOGRIB 1.1 is directly available in text file
gribita.doc 

English speaking users can use for reference special sections of WINWORD format file 
gribeng20.doc, which includes documentation of a more recent version (PICOGRIB V2.0) of
the package, which includes, besides old utilities:
 
GRB2SRF a program that allows to decode a file of GRIB messages and to create, for each
        field, a file all decompressed data associated with their coordinates, both
        lat/long and UTM. Created files have long names and can be directly imported in a
        Golden Software SURFER application. It is possible to supply a met variable 
        decoding file to convert data to different units of measure.
        Met variable decoding file includes also a brief  variable tag that can be used
        instead of variable number to enhance long file name user-friendlines.
	(This utility de facto substitutes SHOWGRIB).
PGRIB32 a program that allows to divide a single file made up of several GRIB messages in
        many files each consisting of a single GRIB message. Created files have long names,
        that can contain variable tags in names.
	(This utility de facto substitutes PARTGRIB).
GRIBLIB32 a routines library used by GRB2SRF and PGRIB32 functions.
METSURF a Golden Software GS Scripter procedure that automates the generation of isolines
        plots of meteo fields under Golden Software Surfer.
        The daily plotting sequence is defined in an initialization file, thar lists 
        variables, forecast deadlines, plotting styles and options.

PICOGRIB 2.0 is a supported software package that you can buy from PICOdata s.r.l.

_____________________________

Massimo Bider
c/o PICOdata s.r.l.
Via Hayez, 8
20129 MILANO

m.bider@picodata.it
_____________________________

Paolo Patruno & Davide Cesari
c/o ARPA/SMR
Viale Silvani, 6
40122 BOLOGNA

p.patruno@smr.arpa.emr.it
borsisti@smr.arpa.emr.it
