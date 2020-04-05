<http://sourceforge.net>
------------------------------------------------------------------------


  PICOGRIB

This software package was developed to supply a multi-platform GRIB
C-language (FORTRAN callable) decoding package compatible to some extent
with ECMWF <http://www.ecmwf.int> GRIBEX routine.

The kernel of the package was originally written by M.Mazzola and
M.Bider at PICOdata s.r.l. Milano, Italy and included:

    * DUMPGRIB a program that allows to view all information contained
      in GRIB message informative sections and the first ten actual
      values of the data grid
    * SHOWGRIB a program that allows to decode a file of GRIB messages
      and to create, for each field, a file containing main information
      and all decompressed data associated with their coordinates
    * PARTGRIB a program that allows to divide a single file made up of
      several GRIB messages in many files each consisting of a single
      GRIB message
    * GRIBLIB a library of routines used by SHOWGRIB, DUMPGRIB and
      PARTGRIB to support GRIB decoding functions
    * GRIBEX a C-language routine emulating the GRIB decoding section of
      ECMWF GRIBEX FORTRAN subroutine 

This kernel was then revised and integrated (1999-2004) by Davide Cesari
and Paolo Patruno at ARPA SMR Bologna, Italy. Revision resulted in many
bug fixes and in the expansion of the decoding capabilities of section 1
and 2 in GRIB messages. Integration resulted in the development of
module PBUTILS, emulating ECMWF PBIO utilities, which handle GRIB
message file I/O.

PICOGRIB 1.1 is released under GPL (Generic Public License) by ARPA SMR
(See file licenza.txt for license terms; license.txt is the original GNU
license from which it derives). Some documentation of PICOGRIB 1.1 in
Italian is directly available in text file gribita.doc English speaking
users can use, for reference, special sections of WORD format file
gribeng20.doc, which includes documentation of a more recent version
(PICOGRIB V2.0) of the package, which includes, besides old utilities:

    * GRB2SRF a program that allows to decode a file of GRIB messages
      and to create, for each field, a file with all decompressed data
      associated with their coordinates, both lat/lon and UTM. Created
      files have long names and can be directly imported in /Golden
      Software SURFER/ application. It is possible to supply a met
      variable decoding file to convert data to different units. Met
      variable decoding file includes also a brief variable tag that can
      be used instead of variable number to enhance long file name
      user-friendly. (This utility de facto substitutes SHOWGRIB)
    * PGRIB32 a program that allows to divide a single file made up of
      several GRIB messages in many files each consisting of a single
      GRIB message. Created files have long names, that can contain
      variable tags in names. (This utility de facto substitutes PARTGRIB)
    * GRIBLIB32 a library of routines used by GRB2SRF and PGRIB32
      functions.
    * METSURF a /Golden Software GS Scripter/ procedure that automates
      the generation of isoline plots of meteo fields under Golden
      Software Surfer. The daily plotting sequence is defined in an
      initialization file, that lists variables, forecast ranges,
      plotting styles and options. 

PICOGRIB 2.0 is a supported software package that you can buy from
PICOdata s.r.l.


  Links

    * Project summary <http://sourceforge.net/projects/picogrib/>
    * PICOdata s.r.l. <http://www.picodata.it>
    * meteodata <http://www.meteodata.it>
    * ARPA-SIM home page <http://www.arpa.emr.it/sim>
    * ECMWF home page <http://www.ecmwf.int>

------------------------------------------------------------------------
Massimo Bider
PICOdata s.r.l.
Via Hayez, 8
20129 MILANO
ITALIA
mbider at users dot sf dot net
------------------------------------------------------------------------
Paolo Patruno & Davide Cesari
ARPA/SIM
Viale Silvani, 6
40122 BOLOGNA
ITALIA
pat1 at users dot sf dot net
dcesari at users dot sf dot net
