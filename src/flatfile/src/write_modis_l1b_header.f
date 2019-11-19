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
      SUBROUTINE WRITE_MODIS_L1B_HEADER(HDR_FILE, TEXT, NUM_PIXELS,
     &  NUM_LINES, NUM_BANDS, BAND_LIST, UNIT_LIST, BAD_VALUE,
     &  PLATFORM, DESTRIPE)
      
      implicit none
      
c ... Arguments
      character*(*) hdr_file, text, platform
      integer num_pixels, num_lines, num_bands, band_list(*), destripe
      character*3 unit_list(*)
      real bad_value
      
c ... Local variables
      integer outlun
      parameter (outlun = 51)
      
      integer data_type
      parameter (data_type = 4)

c ... External functions
      integer string_length
      external string_length

      logical big_endian
      external big_endian
      
      integer index, length
      
      integer iostat
      
      character*500 buffer
      
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

      write(outlun, '(''bands = '', i6)') num_bands

      write(outlun, '(''data type = '', i6)') data_type

      write(outlun, '(''header offset = 0'')')

      write(outlun, '(''interleave = bil'')')

      if (big_endian()) then
        write(outlun, '(''byte order = 1'')')
      else
        write(outlun, '(''byte order = 0'')')
      endif

      if (destripe .eq. 1) 
     &    write(outlun, '(''destripe = band26'')')
     
      buffer(1 : len(buffer)) = ' '
      write(buffer, '(50(i3, '',''))') (band_list(index), index = 1, num_bands)
      length = string_length(buffer) - 1
      write(outlun, '(''band names = {'', a, ''}'')') buffer(1 : length)

      buffer(1 : len(buffer)) = ' '
      write(buffer, '(50(a3, '',''))') (unit_list(index), index = 1, num_bands)
      length = string_length(buffer) - 1
      write(outlun, '(''band units = {'', a, ''}'')') buffer(1 : length)

      write(outlun, '(''bad value = '', f10.2)') bad_value

c ... Close the header file
      close(outlun)
      
      END
