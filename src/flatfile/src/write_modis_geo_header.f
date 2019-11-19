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
      SUBROUTINE WRITE_MODIS_GEO_HEADER(HDR_FILE, TEXT, NUM_PIXELS,
     &  NUM_LINES, BAD_VALUE, PLATFORM)
      
      implicit none
      
c ... Arguments
      character*(*) hdr_file, text, platform
      integer num_pixels, num_lines
      real bad_value
      
c ... Local variables
      integer outlun
      parameter (outlun = 51)
      
c ... External functions
      integer string_length
      external string_length

      logical big_endian
      external big_endian
      
      integer iostat
      
c ... Open the header file
      open(outlun, file=hdr_file, iostat=iostat,
     &  status='unknown', access='sequential', form='formatted')
      if (iostat .ne. 0) then
        print *, 'Could not open output header file for writing: ', hdr_file
        call exit(-1)
      endif
      
c ... Write the keywords and values
      write(outlun, '(''ENVI'')')
      
      if (string_length(text) .gt. 0)
     &  write(outlun, '(''description = {'', a, ''}'')')
     &  text(1 : string_length(text))

      write(outlun, '(''platform = '', a)') platform

      write(outlun, '(''samples = '', i6)') num_pixels

      write(outlun, '(''lines = '', i6)') num_lines

      write(outlun, '(''bands = 8'')')

      write(outlun, '(''data type = 4'')')

      write(outlun, '(''header offset = 0'')')

      write(outlun, '(''interleave = bil'')')

      if (big_endian()) then
        write(outlun, '(''byte order = 1'')')
      else
        write(outlun, '(''byte order = 0'')')
      endif

      write(outlun, '(''band names = {'', 7(a, '', ''), a, ''}'')')  
     &  'Latitude', 'Longitude', 'SensorZenith', 'SensorAzimuth',
     &  'SolarZenith', 'SolarAzimuth', 'Elevation', 'LandSea'

      write(outlun, '(''bad value = '', f10.2)') bad_value

c ... Close the header file
      close(outlun)
      
      END
