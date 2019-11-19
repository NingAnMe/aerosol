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

      PROGRAM MODIS_EXTRACT_HKM

c-----------------------------------------------------------------------
c Purpose:
c     To extract a band-interleaved flat file of MODIS 500 m image data
c     from a DAAC or IMAPP format MOD02HKM HDF file.
c
c     The flat file contains 32-bit floating point data for bands 1-7
c     in at-sensor reflectance units. A 'bad value' of -1.0 is used
c     for pixels that were marked as bad in the HDF file.
c
c     In addition to the flat file containing the image data, an
c     header file is created in ENVI-compatible format. The header file
c     describes the size and contents of the flat file, and it is required
c     by the IMAPP Level-2 science algorithms (e.g. cloudmask). It also
c     allows the image data to be read easily using Freelook or ENVI,
c     which are available from http://www.researchsystems.com/
c
c Usage:
c     modis_extract_hkm in_hdf sat_name out_img out_hdr
c     where
c     in_hdf:  name of input MOD02HKM HDF file'
c     sat_name: platform name of hkm HDF file (Terra or Aqua)'
c     out_img: name of output image file (.img extension)'
c     out_hdr: name of output header file (.hdr extension)'
c
c Author:
c     Liam.Gumley@ssec.wisc.edu
c-----------------------------------------------------------------------

      implicit none

c ... Include files
      include 'hdf.inc'

c ... Local variables
      integer file_id, varid_250, varid_500
      integer rtn, destripe
      integer rank, dimsizes(3), data_type, num_attrs
      character*100 sds_name 
      integer num_pixels, num_lines, num_scans
      
      integer lines_hkm
      parameter (lines_hkm = 20)
      
      integer num_250, num_500
      parameter (num_250 = 2, num_500 = 5)
      
      real scale_250(num_250), offset_250(num_250)
      real scale_500(num_500), offset_500(num_500)
      
      integer scan
      integer start(3), stride(3), edge(3)

      integer max_pixel, max_line
      parameter (max_pixel = 3000, max_line = 20)
      
      integer max_band
      parameter (max_band = num_250 + num_500)
      
      real image(max_pixel, max_line, max_band)
      
      integer dim
      
      integer outlun
      parameter (outlun = 31)
      
      integer outrec
      
      integer pixel, line, band
      
      real buffer(max_pixel)
      
      character*5 sat_name
      character*5 platform
      character*132 in_file, out_file, hdr_file

      real bad_value
      parameter (bad_value = -1.0)
      
      integer this_band, this_index
      
      character*3 unit_list(max_band)
      
      integer num_arg
      
      integer iostat
      
c-----These should be input arguments
      integer num_bands
      parameter (num_bands = 7)
      integer band_list(num_bands)
c-----      

c ... External functions
      integer sfstart, sfselect, sfn2index, sfginfo, 
     &  sfendacc, sfend, sffattr, sfrnatt
      external sfstart, sfselect, sfn2index, sfginfo,
     &  sfendacc, sfend, sffattr, sfrnatt

      integer iargc
      
      integer string_length
      external string_length
      
c ... Data statements
      data band_list / 1,  2,  3,  4,  5,  6,  7 /

c-----------------------------------------------------------------------
c     GET INPUT ARGUMENTS
c-----------------------------------------------------------------------

c ... Check number of arguments
      num_arg = iargc()
      if (num_arg .ne. 4) then
        print *, 'Usage: modis_extract_hkm in_hdf sat_name out_img out_hdr'
        print *, 'where'
        print *, 'in_hdf:  name of input MOD02HKM HDF file'
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
      print '(''Input MOD02HKM HDF file: '', a)', in_file(1: string_length(in_file))
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
      file_id = sfstart(in_file, DFACC_READ)
      if (file_id .eq. -1) then
        print *, 'Error: Could not open input MOD02HKM HDF file for reading'
        call exit(-1)
      endif
      
c ... Get the SDS id for each image array
      varid_500 = sfselect(file_id, sfn2index(file_id, 'EV_500_RefSB'))
      if (varid_500 .eq. -1) then
        print *, 'Error: Input file is not a MOD02HKM HDF file'
        call exit(-1)
      endif
      varid_250 = sfselect(file_id, sfn2index(file_id, 'EV_250_Aggr500_RefSB'))
      
c ... Get the number of pixels, lines, and scans
      rtn = sfginfo(varid_500, sds_name, rank, dimsizes, data_type, num_attrs) 
      num_pixels = dimsizes(1)
      num_lines  = dimsizes(2)
      num_scans  = num_lines / lines_hkm
      print '(''Pixels = '', i6)', num_pixels
      print '(''Lines  = '', i6)', num_lines
      print '(''Scans  = '', i6)', num_scans

c ... Create the unit list
      do band = 1, num_bands
        this_band = band_list(band)
        if (this_band .le. 19 .or. this_band .eq. 26) then
          unit_list(band) = 'ref'
        else
          unit_list(band) = 'rad'
        endif
      end do

c ... Check to see if file has been destriped
      destripe = 0
      if (sffattr(file_id,'IMAPP_DESTRIPE').ne. -1) then
          destripe = 1
      endif
        
c ... Open the output file
      open(outlun, file=out_file, iostat=iostat, status='unknown',
     &  access='direct', form='unformatted', recl=(num_pixels * 4))
      if (iostat .ne. 0) then
        print *, 'Error: Could not open output image file for writing'
        call exit(-1)
      endif
                 
c ... Create the header file
      call write_modis_l1b_header(hdr_file, in_file,
     &  num_pixels, num_lines, num_bands, band_list,
     &  unit_list, bad_value, platform, destripe)
            
c-----------------------------------------------------------------------
c     READ THE IMAGE DATA FROM HDF INPUT AND WRITE TO BINARY OUTPUT
c-----------------------------------------------------------------------

c ... Get the scales and offsets for each image array
      rtn = sfrnatt(varid_250, sffattr(varid_250, 'reflectance_scales'),  scale_250)
      rtn = sfrnatt(varid_250, sffattr(varid_250, 'reflectance_offsets'), offset_250)
      rtn = sfrnatt(varid_500, sffattr(varid_500, 'reflectance_scales'),  scale_500)
      rtn = sfrnatt(varid_500, sffattr(varid_500, 'reflectance_offsets'), offset_500)
      
c ... Set start vector (static values only)
      start(1) = 0
      start(3) = 0

c ... Set stride vector
      stride(1) = 1
      stride(2) = 1
      stride(3) = 1
      
c ... Set edge vector (static values only)
      edge(1) = num_pixels
      edge(2) = lines_hkm

c ... Initialize output record counter
      outrec = 1
      
c ... Loop over each scan
      do scan = 0, (num_scans - 1), 1

c ...   Report progress on every 10th scan
        if (mod(scan, 10) .eq. 0) print '(''Scan: '', i6)', scan

c ...   Set start vector
        start(2) = scan * lines_hkm
        
c ...   Read bands 1-2
        edge(3) = num_250
        dim = 1
        call get_modis_l1b_data(varid_250, start, stride, edge,
     &    scale_250, offset_250, bad_value,
     &    max_pixel, max_line, max_band, image(1, 1, dim))

c ...   Read bands 3-7
        edge(3) = num_500
        dim = num_250 + 1
        call get_modis_l1b_data(varid_500, start, stride, edge,
     &    scale_500, offset_500, bad_value,
     &    max_pixel, max_line, max_band, image(1, 1, dim))

c ...   Write image data to output file (band interleaved by line)
        do line = 1, lines_hkm
          do band = 1, num_bands
            this_index = band_list(band)
            do pixel = 1, num_pixels
              buffer(pixel) = image(pixel, line, this_index)
            end do
            write(outlun, rec=outrec, iostat=iostat)
     &        (buffer(pixel), pixel = 1, num_pixels)
            if (iostat .ne. 0) then
              print *, 'Error: Write to output image file failed'
              call exit(-1)
            endif
            outrec = outrec + 1
          end do
        end do
        
      end do
           
c-----------------------------------------------------------------------
c     CLEANUP AND EXIT
c-----------------------------------------------------------------------

c ... End access to the image arrays
      rtn = sfendacc(varid_250)
      rtn = sfendacc(varid_500)

c ... Close the input file
      rtn = sfend(file_id)

c ... Close the output file
      close(outlun)

      END
