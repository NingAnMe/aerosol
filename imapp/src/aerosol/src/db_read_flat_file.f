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
      integer function db_read_flat_file(
     &			  	      req_scan,	 	
     &				      req_resolution,
     &				      req_bandname,
     &				      req_bandunit,
     &				      fil_lun,
     &				      fil_datatype,
     &				      fil_interleave,
     &				      fil_resolution,
     &                                fil_error,
     &				      fil_offset,
     &				      fil_samples,
     &				      fil_lines,
     &				      fil_bands,
     &				      fil_bandnames,
     &				      fil_bandunits,
     &				      out_maxsamples,
     &				      out_maxlines,
     &				      out_buff,
     &				      out_flag,
     &				      out_samples,
     &				      out_lines
     &                               )
      implicit none
      save

c --- parameters
      integer		req_scan	 ! request scan number	
      integer		req_resolution	 ! request resolution 
      character*(*)	req_bandname	 ! request band 
      character*(*)	req_bandunit	 ! request unit
      integer		fil_lun		 ! file logical unit
      integer		fil_datatype     ! file data type (=4)
      character*(*)	fil_interleave   ! file data interleave (=BIL)
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
      byte		out_flag(*)	 ! output vaild flag array
      integer		out_samples	 ! output array elem size
      integer		out_lines	 ! output array line size

c --- internal variables
      character*255	mod06_band
      character*255	mod06_bands
      integer		beg_record
      integer		end_record
      integer		record
      integer		i
      integer		iostat
      integer		bptr
      integer		eptr
      integer		iband
      integer		len_req
      integer		len_fil
      integer		header
      integer		MAX_LINE
      real		res
      integer		ires
      logical		remove_all

c --- check if request band is contained in the file
      res = 1.0
      ires = 1

      remove_all = .TRUE.
      mod06_band = req_bandname
      call strcompress( mod06_band, remove_all, len_req )
      do i = 1, fil_bands
      mod06_bands = fil_bandnames(i)
      call strcompress( mod06_bands, remove_all, len_fil )
      if(mod06_band(1:len_req).eq.mod06_bands(1:len_fil)) then
         iband = i
         goto 10
      endif
      enddo
      db_read_flat_file = -1
      return
10    continue

C anning 设置最大的读取行数
c ... Set max line = 10
      MAX_LINE = 10

c --- check the units 
      if( fil_bandunits(iband)(1:1).ne.' ' ) then
         call strcompress( req_bandunit, remove_all, len_req )
         call strcompress( fil_bandunits(iband), remove_all, len_fil )
         if(req_bandunit(1:len_req).ne.
     &      fil_bandunits(iband)(1:len_fil) ) then
            db_read_flat_file = -2
            print *, "db_read_flat_file ==", db_read_flat_file
            return
         endif
      endif

c --- check the resolution 
      if( req_resolution.ne.fil_resolution ) then
         db_read_flat_file = -3
         print *, "db_read_flat_file ==", db_read_flat_file
         return
      endif
      res = req_resolution

c --- check the data type
      if( fil_datatype.ne.4 ) then
         db_read_flat_file = -4
         print *, "db_read_flat_file ==", db_read_flat_file
         return
      endif
 
c --- check the interleave
      if( fil_interleave.ne.'bil' ) then
         db_read_flat_file = -5
         print *, "db_read_flat_file ==", db_read_flat_file
         return
      endif

c --- Get the number of lines to read
      res = sqrt(real(fil_resolution))
      ires = int(res)

c --- initialize the record pointer
!     print *, req_scan, MAX_LINE, res, fil_bands, iband
!     beg_record = ((req_scan-1) * MAX_LINE * res * fil_bands) + iband
      beg_record = ((req_scan-1) * 1 * res * fil_bands) + iband
      end_record = beg_record + (((10*res) - 1)  * fil_bands)

!     print *, beg_record, end_record

      if( end_record.gt.(fil_lines*fil_bands) ) then
         db_read_flat_file = -6
         print *, end_record, fil_lines, fil_bands
         print *, "db_read_flat_file ==", db_read_flat_file
         return
      endif

c --- check for a file header: just throw it away for now
      if( fil_offset.gt.0 ) then
         do record = 1, fil_offset
         READ(
     &         UNIT=fil_lun,
     &         REC=record,
     &         ERR=9998,
     &         IOSTAT=iostat 
     &       ) header
         enddo
      endif

c --- loop through the file
      bptr = 1
      out_lines = 0
      do record = beg_record, end_record, fil_bands    
      eptr = bptr + fil_samples - 1

c --- make sure that we have room for the line
      if( (eptr-bptr+1).gt. out_maxsamples ) then
         db_read_flat_file = -7
         print *, "db_read_flat_file ==", db_read_flat_file
         return
      endif

c --- read a record (line) from the file
      READ( 
     &      UNIT=fil_lun,
     &      REC=record, 
     &      ERR=9999,
     &      IOSTAT=iostat
     &    ) (out_buff(i), i = bptr,eptr)  

c --- increment the number of lines
      out_lines = out_lines+1
      if( out_lines.gt.out_maxlines ) then
         db_read_flat_file = -8
         print *, "db_read_flat_file ==", db_read_flat_file
         return
      endif

c --- scan data values for invalid code
      do i = bptr,eptr 
      if( out_buff(i).eq.fil_error ) then
         out_flag(i) = -1
      else
         out_flag(i) =  0
      endif
      enddo

c --- increment the buffer pointer
      bptr = eptr+1

      enddo

c --- SUCCESS: all records for band were read
      out_samples = fil_samples
      db_read_flat_file = 0
      return

c --- FAILED: did not read header
9998  continue
      db_read_flat_file = -8
      return

c --- FAILED: did not read all records 
9999  continue
      db_read_flat_file = -9
      return

      end
