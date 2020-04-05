C mapfunc.f
C
C VIS-5D version 3.1 */
C
C vis5d program for visualizing five dimensional gridded data sets
C Copyright (C) 1990, 1991, 1992, 1993  Bill Hibbard, Brian Paul,
C Dave Santek, and Andre Battaiola.
C
C This program is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published by
C the Free Software Foundation; either version 1, or (at your option)
C any later version.
C
C This program is distributed in the hope that it will be useful,
C but WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C GNU General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this program; if not, write to the Free Software
C Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
C
C
C     FORTRAN subroutine to transform map line vertices.  This is
C     called from the newmap.c program.
C
C
      SUBROUTINE TRANSFORM( OLDLAT, OLDLON, NEWLAT, NEWLON )
C     Input/Output arguments are in degrees:
      REAL*4 OLDLAT, OLDLON, NEWLAT, NEWLON

C     Some local variables:
      REAL*4 LAT, LON, LAT2, LON2, ANGLE

C     Example:  Rotate a map by 10 degrees clockwise about an origin
C     in Wisconsin.

C     Move Wisconsin to origin:
      LAT = OLDLAT - 43.0
      LON = OLDLON - 90.0

C     Rotate by 10 degrees:
      ANGLE = 10 * 3.14159 / 180.0
      LAT2 = LON * SIN(ANGLE) + LAT * COS(ANGLE)
      LON2 = LON * COS(ANGLE) - LAT * SIN(ANGLE)

C     Translate back to Wisconsin
      NEWLAT = LAT2 + 43.0
      NEWLON = LON2 + 90.0

      RETURN
      END
