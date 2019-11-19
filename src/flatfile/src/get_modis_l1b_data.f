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
      SUBROUTINE GET_MODIS_L1B_DATA(VARID, START, STRIDE, EDGE,
     &  SCALE, OFFSET, BAD_VALUE, MAX_PIXEL, MAX_LINE, MAX_BAND, IMAGE)
      
      implicit none

c ... Arguments
      integer varid, start(*), stride(*), edge(*),
     &  max_pixel, max_line, max_band
      real scale(*), offset(*), bad_value
      real image(max_pixel, max_line, max_band)

c ... Local variables
      integer*2 buffer(max_pixel * max_line * max_band)
      
      integer rtn, pixel, line, band, index
      
c ... External functions
      integer sfrdata
      external sfrdata
      
c ... Get the scaled integer data
      rtn = sfrdata(varid, start, stride, edge, buffer) 
      if (rtn .ne. 0) then
        print *, 'Error reading scaled integer data'
        stop
      endif
      
c ... Convert valid scaled integers to unscaled image data
      index = 1
      do band = 1, edge(3)
        do line = 1, edge(2)
          do pixel = 1, edge(1)
            if (buffer(index) .ge. 0) then
              image(pixel, line, band) =
     &          scale(band) * (real(buffer(index)) - offset(band))
            else
              image(pixel, line, band) = bad_value
            endif
            index = index + 1
          end do
        end do
      end do

      END                
