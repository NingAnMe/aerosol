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
      integer function db_read_mask_file(
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

      integer MAX_LINE
      parameter(MAX_LINE = 10)

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
      byte		out_buff(*)	 ! output data array
      byte		out_flag(*)	 ! output vaild flag array
      integer		out_samples	 ! output array elem size
      integer		out_lines	 ! output array line size

c --- internal variables
      character*255	mod35_band
      character*255	mod35_bands
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
      logical		remove_all

c --- check if request band is contained in the file
      remove_all = .TRUE.
      mod35_band = req_bandname
      call strcompress( mod35_band, remove_all, len_req )
      do i = 1, fil_bands
      mod35_bands = fil_bandnames(i)
      call strcompress( mod35_bands, remove_all, len_fil )
      if(mod35_band(1:len_req).eq.mod35_bands(1:len_fil)) then
         iband = i
         goto 10
      endif
      enddo
      db_read_mask_file = -1
      return
10    continue

c --- check the units 
      if( fil_bandunits(iband)(1:1).ne.' ' ) then
         call strcompress( req_bandunit, remove_all, len_req )
         call strcompress( fil_bandunits(iband), remove_all, len_fil )
         if(req_bandunit(1:len_req).ne.
     &      fil_bandunits(iband)(1:len_fil) ) then
            db_read_mask_file = -2
            return
         endif
      endif

c --- check the resolution 
      if( req_resolution.ne.fil_resolution ) then
         db_read_mask_file = -3
         return
      endif

c --- check the data type
      if( fil_datatype.ne.1 ) then
         db_read_mask_file = -4
         return
      endif
 
c --- check the interleave
      if( fil_interleave.ne.'bsq' ) then
         db_read_mask_file = -5
         return
      endif

c --- initialize the record pointer
      beg_record = ((iband-1)*fil_lines) + ((req_scan-1)*MAX_LINE) + 1
      end_record = beg_record + (MAX_LINE-1)

      if( end_record.gt.(fil_lines*fil_bands) ) then
         db_read_mask_file = -6
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
      do record = beg_record, end_record, 1
        eptr = bptr + fil_samples - 1

c ---   make sure that we have room for the line
        if( (eptr-bptr+1).gt. out_maxsamples ) then
           db_read_mask_file = -7
           return
        endif

c ---   read a record (line) from the file
        READ( 
     &        UNIT=fil_lun,
     &        REC=record, 
     &        ERR=9999,
     &        IOSTAT=iostat
     &      ) (out_buff(i), i = bptr,eptr)  

c ---   increment the number of lines
        out_lines = out_lines+1
        if( out_lines.gt.out_maxlines ) then
           db_read_mask_file = -8
           return
        endif

c ---   scan data values for invalid code
        do i = bptr,eptr
          if( out_buff(i).eq.fil_error ) then
            out_flag(i) = -1
          else
            out_flag(i) =  0
          endif
        enddo

c ---   increment the buffer pointer
        bptr = eptr+1

      enddo

c --- SUCCESS: all records for band were read
      out_samples = fil_samples
      db_read_mask_file = 0
      return

c --- FAILED: did not read header
9998  continue
      db_read_mask_file = -8
      return

c --- FAILED: did not read all records 
9999  continue
      db_read_mask_file = -9
      return

      end
