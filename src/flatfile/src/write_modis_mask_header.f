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
      SUBROUTINE WRITE_MODIS_MASK_HEADER(HDR_MASK, HDR_MASKQA, TEXT_MASK, 
     &  NUM_PIXELS, NUM_LINES, BAD_VALUE, PLATFORM)
      
      implicit none
      
c ... Arguments
      character*(*) hdr_mask, hdr_maskqa, text_mask, platform
      integer num_pixels, num_lines
      byte bad_value
      
c ... Local variables
      integer outlun1, outlun2
      parameter (outlun1 = 51, outlun2 = 52)
      
c ... External functions
      integer string_length
      external string_length

      integer iostat
      
c ... First the cloud mask header file
c ---------------------------------------------------------------
c ... Open the mask header file
      open(outlun1, file=hdr_mask, iostat=iostat,
     &  status='unknown', access='sequential', form='formatted')
      if (iostat .ne. 0) then
        print *, 'Could not open mask header file for writing: ', hdr_mask
        call exit(-1)
      endif
      
c ... Write the keywords and values
      write(outlun1, '(''ENVI'')')
      
      if (string_length(text_mask) .gt. 0)
     &  write(outlun1, '(''description = {'', a, ''}'')')
     &  text_mask(1 : string_length(text_mask))

      write(outlun1, '(''platform = '', a)') platform

      write(outlun1, '(''samples = '', i6)') num_pixels

      write(outlun1, '(''lines = '', i6)') num_lines

      write(outlun1, '(''bands = 6'')')

      write(outlun1, '(''data type = 1'')')

      write(outlun1, '(''header offset = 0'')')

      write(outlun1, '(''interleave = bsq'')')

      write(outlun1, '(''byte order = 1'')')

      write(outlun1, '(''band names = {'', 5(a, '', ''), a, ''}'')')  
     &  'byte 1', 'byte 2', 'byte 3', 'byte 4',
     &  'byte 5', 'byte 6'

      write(outlun1, '(''band units = {'', 5(a, '', ''), a, ''}'')')  
     &  'flg', 'flg', 'flg', 'flg', 'flg', 'flg'

      write(outlun1, '(''bad value = '', I5)') bad_value

c ... Close the mask header file
      close(outlun1)
c ---------------------------------------------------------------
      
c ... Second the cloud mask QA header file
c ---------------------------------------------------------------
c ... Open the mask QA header file
      open(outlun2, file=hdr_maskqa, iostat=iostat,
     &  status='unknown', access='sequential', form='formatted')
      if (iostat .ne. 0) then
        print *, 'Could not open mask QA header file for writing: ', hdr_mask
        call exit(-1)
      endif
      
c ... Write the keywords and values
      write(outlun2, '(''ENVI'')')
      
      if (string_length(text_mask) .gt. 0)
     &  write(outlun2, '(''description = {'', a, ''}'')')
     &  text_mask(1 : string_length(text_mask))

      write(outlun2, '(''platform = '', a)') platform

      write(outlun2, '(''samples = '', i6)') num_pixels

      write(outlun2, '(''lines = '', i6)') num_lines

      write(outlun2, '(''bands = 10'')')

      write(outlun2, '(''data type = 1'')')

      write(outlun2, '(''header offset = 0'')')

      write(outlun2, '(''interleave = bsq'')')

      write(outlun2, '(''byte order = 1'')')

      write(outlun2, '(''band names = {'', 9(a, '', ''), a, ''}'')')  
     &  'byte 1', 'byte 2', 'byte 3', 'byte 4', 'byte 5', 'byte 6' ,
     &  'byte 7', 'byte 8', 'byte 9', 'byte 10'

      write(outlun2, '(''band units = {'', 9(a, '', ''), a, ''}'')')  
     &  'flg', 'flg', 'flg', 'flg', 'flg', 'flg', 'flg', 
     &  'flg', 'flg', 'flg'

      write(outlun2, '(''bad value = '', I5)') bad_value

c ... Close the mask header file
      close(outlun2)
c ---------------------------------------------------------------
      
      END
