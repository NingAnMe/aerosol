C--------------------------------------------------------------------
C  Copyright (C) 2002,  Space Science and Engineering Center, 
C  University C  of Wisconsin-Madison, Madison WI.
C      
C  This program is free software; you can redistribute it 
C  and/or modify it under the terms of the GNU General 
C  Public License as published by the Free Software Foundation; 
C  either version 2 of the License, or (at your option) any 
C  later version.
C
C  This program is distributed in the hope that it will be 
C  useful, but WITHOUT ANY WARRANTY; without even the implied 
C  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
C  See the  GNU General Public License for more details.
C
C  You should have received a copy of the GNU General Public 
C  License along with this program; if not, write to the Free 
C  Software Foundation, Inc., 59 Temple Place, Suite 330, 
C  Boston, MA  02111-1307 USA
C--------------------------------------------------------------------
C
C
      INTEGER FUNCTION GET_MODIS_BAND_INDEX(BAND)

c ... Returns the index of the specified MODIS band relative to the
c ... total stored number of MODIS bands, i.e.
c ... 2 x 250 meter: 1,2
c ... 5 x 500 meter: 3,4,5,6,7
c ... 15 x 1000 meter: 8,9,10,11,12,13lo,13hi,14lo,14hi,15,16,17,18,19,26
c ... 16 x 1000 meter: 20,21,22,23,24,25,27,28,29,30,31,32,33,34,35,36

      implicit none
      integer band
      integer band_index(36)

      data band_index/ 1,  2,  3,  4,  5,  6,  7,  8,  9, 10,
     &                11, 12, 13, 15, 17, 18, 19, 20, 21, 23,
     &                24, 25, 26, 27, 28, 22, 29, 30, 31, 32,
     &                33, 34, 35, 36, 37, 38/

      get_modis_band_index = band_index(band)
      
      END
