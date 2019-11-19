C--------------------------------------------------------------------
C  Copyright (C) 2004,  Space Science and Engineering Center, 
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
      integer function db_mod04_write_flat_file(
     &			    	        req_scan,	 	
     &				        req_samples,
     &				        req_lines,
     &				        req_band,
     &				        fil_lun,
     &				        hdr_flg,
     &				        hdr_lun,
     &                                  fil_desc,
     &				        fil_datatype,
     &				        fil_interleave,
     &				        fil_resolution,
     &                                  fil_error,
     &				        fil_offset,
     &				        fil_samples,
     &				        fil_lines,
     &				        fil_bands,
     &				        fil_bandnames,
     &				        fil_bandunits,
     &				        out_maxsamples,
     &				        out_maxlines,
     &				        out_buff,
     &                                  out_flag,
     &				        out_samples,
     &				        out_lines
     &                                )
      implicit none
      save

      include		'mod04.inc'
c --- parameters
      integer		req_scan	 ! scan number	
      integer		req_samples	 ! request number of elems/line
      integer		req_lines	 ! request number of lines
      integer		req_band	 ! request band to write
      integer		fil_lun		 ! file logical unit
      integer		hdr_lun		 ! header file logical unit
      integer		hdr_flg	         ! header file flag 
c            where (0 = don't write header, 1 = write header)
      character*(*)	fil_desc         ! file description
      integer		fil_datatype     ! file data type 
      character*(*)	fil_interleave   ! file data interleave 
      integer		fil_resolution   ! file data resolution
      real		fil_error	 ! file data error value 
      integer		fil_offset	 ! file header offset
      integer		fil_samples	 ! file number of elems/line
      integer		fil_lines	 ! file number of lines
      integer		fil_bands	 ! file number of bands
      character*(*)	fil_bandnames(*) ! file band names
      character*(*)	fil_bandunits(*) ! file band units
      integer		out_maxsamples	 ! output array max elem size
      integer		out_maxlines	 ! output array max line size
      real		out_buff(*)	 ! output data array
      byte		out_flag(*)	 ! output data array
      integer		out_samples	 ! output array elem size
      integer		out_lines	 ! output array line size

c --- internal variables
      character*80      bandstore
      character*3       bnamestore
      integer		record
      integer		beg_record
      integer		end_record
      integer		i,j,ij,ii
      integer		iostat
      integer		bptr
      integer		eptr
      integer		iband
      integer		reql
      integer		lines
      integer		length
      logical           init
      logical           remove_all        

c ... external functions
      external          big_endian
      logical           big_endian

c ... data statements
      data init /.true./
      data remove_all /.false./
      


c --- check datatype
      if( fil_datatype.ne.4 ) then
         db_mod04_write_flat_file = -1
         return
      endif

c --- check interleave
      if( fil_interleave(1:3).ne.'bil' ) then
         db_mod04_write_flat_file = -2
         return
      endif

c --- check resolution
      if( fil_resolution.ne.10 ) then
         db_mod04_write_flat_file = -3
         return
      endif

c --- check offset (currently can't do this)
      if( fil_offset.gt.0 ) then
         db_mod04_write_flat_file = -4
         return
      endif

c --- scan the output flag array for error status
      do i = 1, NUMCELLS
        do j = 1, MAX_SAMP_LINE
           ij = i*j
           if(out_flag(ij).eq. 1) then
              out_buff(ij) = fil_error
           endif
        enddo
      enddo

c ... Write header file only if hdr_flg is set to one (1)
      if (hdr_flg .eq. 1) then
          write(hdr_lun,'(''ENVI'')')
          call strcompress(fil_desc,remove_all,length)
          write(hdr_lun,'(''description = {'',A, 
     &                      ''}'')') fil_desc(1:length)
          write(hdr_lun,'(''samples = '',I5)') fil_samples
          write(hdr_lun,'(''lines = '',I5)') fil_lines
          write(hdr_lun,'(''bands = '',I3)') fil_bands
          write(hdr_lun,'(''data type = '',I2)') fil_datatype
          write(hdr_lun,'(''header offset = '',I3)') fil_offset
          write(hdr_lun,'(''interleave = '',A4)') fil_interleave
          if (big_endian()) then
              write(hdr_lun,'(''byte order = 1'')')
          else
              write(hdr_lun,'(''byte order = 0'')')
          endif
          write(hdr_lun,'(''band names = {'')')
          do ii = 1 , fil_bands
            bandstore = fil_bandnames(ii)
            call strcompress(bandstore,remove_all,length)
            if (ii .ne. fil_bands) then
              write(hdr_lun,'(''band '', I2, '': '', A, '','')')
     &                          ii, bandstore(1:length)
            else
              write(hdr_lun,'(''band '', I2, '': '', A)')
     &                          ii, bandstore(1:length)
            endif
          enddo
          write(hdr_lun,'(''}'')')
          write(hdr_lun,'(''band units = {'')')
          do ii = 1 , fil_bands
            bnamestore = fil_bandunits(ii)
            call strcompress(bnamestore,remove_all,length)
            if (ii .ne. fil_bands) then
              write(hdr_lun,'(''band '', I2, '': '', A, '','')')
     &                      ii, bnamestore(1:length)
            else
              write(hdr_lun,'(''band '', I2, '': '', A )')
     &                      ii, bnamestore(1:length)
            endif
          enddo
          write(hdr_lun,'(''}'')')
          write(hdr_lun,'(''bad value = '',f7.2)') fil_error
          init = .false.
      endif

c --- initialize the buffer pointers
      bptr = 1
      eptr = bptr + req_samples - 1
      out_lines = 0

c --- make sure we are not running off element bounds
      if( eptr.gt.out_maxsamples ) then
         db_mod04_write_flat_file = -5
         return
      endif

c ... Required lines
      reql = req_lines

c ... Lines in File
      lines = fil_lines

      iband = req_band

c --- initialize the record pointer
      beg_record = ((req_scan-1) * MAX_SAMP_LINE * fil_bands) + iband
      end_record = beg_record + ((MAX_SAMP_LINE-1) * fil_bands)

c --- loop through the file
      do record = beg_record, end_record, fil_bands

c ---   write a record (line) to the file
        WRITE( 
     &         UNIT=fil_lun,
     &         REC=record, 
     &         ERR=9999,
     &         IOSTAT=iostat
     &       )  (out_buff(i), i = bptr,eptr)  


c ---   increment the number of lines
        out_lines = out_lines+1
        if( out_lines.gt.out_maxlines ) then
           db_mod04_write_flat_file = -6
           return
        endif

c ---   increment the buffer pointer
        bptr = eptr + 1
        eptr = bptr + req_samples - 1

      enddo

c --- SUCCESS: all records for band were written
      out_samples = fil_samples
      db_mod04_write_flat_file = 0
      return

c --- FAILED: did not write all records 
9999  continue
      db_mod04_write_flat_file = -7
      return

      end
