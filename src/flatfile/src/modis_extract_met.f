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
      PROGRAM MODIS_EXTRACT_MET

c-----------------------------------------------------------------------
c Purpose:
c     To extract a pixel-interleaved flat file of MODIS 1 km metadata
c     from a DAAC or IMAPP format MOD021KM HDF file.
c
c     The flat file contains 8-bit byte data, organized as
c     2 parameters (bands) per earth scan. The parameters are:
c       (1) Scan type (1=Day, 0=Night)
c       (2) Mirror Side (0 or 1)
c
c     In addition to the flat file containing the metadata,
c     a header file is created in ENVI-compatible format. The header file
c     describes the size and contents of the flat file, and it is required
c     by the IMAPP Level-2 science algorithms (e.g. cloudmask). It also
c     allows the image data to be read easily using Freelook or ENVI,
c     which are available from http://www.researchsystems.com/
c
c Usage:
c     modis_extract_met in_hdf sat_name out_img out_hdr
c     where
c     in_hdf:  name of input MOD021KM HDF file'
c     sat_name: platform name of 1km HDF file (Terra or Aqua)'
c     out_img: name of output image file (.img extension)'
c     out_hdr: name of output header file (.hdr extension)'
c
c To compile on SGI (note that HDF4.1 is required):
c
c   set HDFHOME = /usr/local/hdf4.1r3
c   f77 -n32 -O2 -extend_source -bytereclen \
c     -o modis_extract_met modis_extract_met.f \
c     string_length.f big_endian.f \
c     -I$HDFHOME/include -L$HDFHOME/lib -ldf -ljpeg -lz
c
c Author:
c     Liam.Gumley@ssec.wisc.edu
c-----------------------------------------------------------------------

      implicit none

c ... Include files
      include 'hdf.inc'

c ... Local variables

      integer num_arg
      
      integer file_id, num_dds_block, rtn

      character*5 sat_name
      character*5 platform
      character*132 in_file, out_file, hdr_file

      integer outlun
      parameter (outlun = 31)

      integer num_values
      parameter (num_values = 2)
      
      integer vdata_ref, vdata_id
      
      integer max_scan
      parameter (max_scan = 1000)
      
      integer num_scans

      integer record_index
      
      integer mirror_side(max_scan)

      character*1 scan_type(4, max_scan)

      integer scan

      character*1 out_data(num_values)

      integer iostat
                              
c ... External functions
      integer  hopen, vfstart, vsffnd, vsfatch, vsfsfld, vsfelts,
     &  vsfseek, vsfrd, vsfrdc, vsfdtch, vfend, hclose
      external hopen, vfstart, vsffnd, vsfatch, vsfsfld, vsfelts, 
     &  vsfseek, vsfrd, vsfrdc, vsfdtch, vfend, hclose

      integer iargc
      
      integer string_length
      external string_length
      
      logical big_endian
      external big_endian
      
c-----------------------------------------------------------------------
c     GET INPUT ARGUMENTS
c-----------------------------------------------------------------------

c ... Check number of arguments
      num_arg = iargc()
      if (num_arg .ne. 4) then
        print *, 'Usage: modis_extract_met in_hdf sat_name out_img out_hdr'
        print *, 'where'
        print *, 'in_hdf:  name of input MOD021KM HDF file'
        print *, 'sat_name: MODIS satellite platform name (Aqua or Terra)'
        print *, 'out_img: name of output image file (.img extension)'
        print *, 'out_hdr: name of output header file (.hdr extension)'
        call exit(-1)
      endif

c ... Extract arguments
      call getarg(1, in_file)
      call getarg(2, sat_name)
      call getarg(3, out_file)
      call getarg(4, hdr_file)
          
c-----------------------------------------------------------------------
c     OPEN FILES
c-----------------------------------------------------------------------

c ... Print the input and output filenames
      print '(''Input MOD021KM HDF file: '', a)', in_file(1: string_length(in_file))
      print '(''Input Satellite Platform Name: '', a)', sat_name(1: string_length(sat_name))
      print '(''Output image  file: '', a)', out_file(1 : string_length(out_file))
      print '(''Output header file: '', a)' ,hdr_file(1 : string_length(hdr_file))

c ... Check input platform name to make sure it is either Aqua or Terra
      platform = ' '
      if (sat_name(1:5) .eq. 'Terra'   .or.
     +    sat_name(1:5) .eq. 'terra'   .or.
     +    sat_name(1:5) .eq. 'TERRA') then
          platform = 'Terra'
      elseif (sat_name(1:4) .eq. 'Aqua'   .or.
     +    sat_name(1:4) .eq. 'aqua'   .or.
     +    sat_name(1:4) .eq. 'AQUA') then
          platform = 'Aqua'
      else
          print *, 'Error: Incorrect Satellite Platform name entered'
          call exit(-1)
      endif

c ... Open the input HDF file for read only
c ... (DFACC_READ is defined in hdf.inc)
      num_dds_block = 0
      file_id = hopen(in_file, DFACC_READ, num_dds_block)
      if (file_id .eq. -1) then
        print *, 'Error: Could not open input MOD021KM HDF file for reading'
        call exit(-1)
      endif
      
c ... Open the output file
      open(outlun, file=out_file, iostat=iostat, status='unknown',
     &  access='direct', form='unformatted', recl=num_values)
      if (iostat .ne. 0) then
        print *, 'Error: Could not open output data file for writing'
        call exit(-1)
      endif

c-----------------------------------------------------------------------
c     READ THE METADATA FROM HDF INPUT
c-----------------------------------------------------------------------

c ... Start vdata interface
      rtn = vfstart(file_id)
      if (rtn .eq. -1) then
        print *, 'Error: Could not start vdata interface'
        call exit(-1)
      endif

c ... Get reference number of vdata containing scan type and mirror side
      vdata_ref = vsffnd(file_id, 'Level 1B Swath Metadata')
      if (vdata_ref .eq. 0) then
        print *, 'Error: Input file is not a MOD021KM HDF file'
        call exit(-1)
      endif

c ... Attach to vdata
      vdata_id = vsfatch(file_id, vdata_ref, 'r')
      if (vdata_id .eq. -1) then
        print *, 'Error: Could not attach to vdata'
        call exit(-1)
      endif

c ... Get number of records (scans)
      num_scans = vsfelts(vdata_id)
      print '(''Scans  = '', i6)', num_scans
           
c ... Read mirror side data
      record_index = 0
      rtn = vsfseek(vdata_id, record_index)
      rtn = vsfsfld(vdata_id, 'Mirror Side')
      rtn = vsfrd(vdata_id, mirror_side, num_scans, FULL_INTERLACE)
      if (rtn .ne. num_scans) then
        print *, 'Error: Could not read mirror side data'
        call exit(-1)
      endif
      
c ... Read scan type data
      record_index = 0
      rtn = vsfseek(vdata_id, record_index)
      rtn = vsfsfld(vdata_id, 'Scan Type')
      rtn = vsfrdc(vdata_id, scan_type, num_scans, FULL_INTERLACE)
      if (rtn .ne. num_scans) then
        print *, 'Error: Could not read scan type data'
        call exit(-1)
      endif

c ... Detach from vdata
      rtn = vsfdtch(vdata_id)
      
c ... End vdata interface
      rtn = vfend(file_id)
      
c ... Close the input file
      rtn = hclose(file_id)

c-----------------------------------------------------------------------
c     WRITE THE OUTPUT DATA FILE
c-----------------------------------------------------------------------

c ... Loop over each scan
      do scan = 1, num_scans

c ...   Scan type is 1 for day, 0 for night
        if (scan_type(1, scan) .eq. 'D') then
          out_data(1) = char(1)
        else
          out_data(1) = char(0)
        endif

c ...   Mirror side is either 0 or 1        
        out_data(2) = char(mirror_side(scan))

c ...   Write data for this scan to output data file
        write(outlun, rec=scan, iostat=iostat) out_data
        if (iostat .ne. 0) then
          print *, 'Error: Write to output data file failed'
          call exit(-1)
        endif
        
      end do

c ... Close output data file
      close(outlun)      

c-----------------------------------------------------------------------
c     WRITE THE OUTPUT HEADER FILE
c-----------------------------------------------------------------------

c ... Open output header file
      open(outlun, file=hdr_file, iostat=iostat,
     &  status='unknown', access='sequential', form='formatted')
      if (iostat .ne. 0) then
        print *, 'Could not open output header file for writing: ', hdr_file
        call exit(-1)
      endif
      
c ... Write the keywords and values
      write(outlun, '(''ENVI'')')
      
      write(outlun, '(''description = {'', a, ''}'')') 
     &  in_file(1 : string_length(in_file))

      write(outlun, '(''platform = '', a)') platform

      write(outlun, '(''samples = 1'')')

      write(outlun, '(''lines = '', i6)') num_scans

      write(outlun, '(''bands = 2'')')

      write(outlun, '(''data type = 1'')')

      write(outlun, '(''header offset = 0'')')

      write(outlun, '(''interleave = bip'')')

      if (big_endian()) then
        write(outlun, '(''byte order = 1'')')
      else
        write(outlun, '(''byte order = 0'')')
      endif

      write(outlun, '(''band names = {scan type, mirror side}'')')
      
c ... Close output header file
      close(outlun)
            
      END
