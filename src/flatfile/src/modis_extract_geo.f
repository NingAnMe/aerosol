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
      PROGRAM MODIS_EXTRACT_GEO

c-----------------------------------------------------------------------
c Purpose:
c     To extract a band-interleaved flat file of MODIS 1 km geolocation
c     data from a DAAC or IMAPP format MOD03 HDF file.
c
c     The flat file contains 32-bit floating point data, organized as
c     8 parameters (bands) per scan line. The parameters are:
c       (1) Geodetic latitude (degrees, -90S to +90N)
c       (2) Geodetic longitude (degrees, -180W to +180E)
c       (3) Sensor zenith (degrees, pixel to sensor)
c       (4) Sensor azimuth (degrees, pixel to sensor)
c       (5) Solar zenith (degrees, pixel to sun)
c       (6) Solar azimuth (degrees, pixel to sun)
c       (7) Terrain height above geoid (meters)
c       (8) Land/Sea mask (mask codes are explained below)
c               0: Shallow Ocean (Ocean <5k from coast OR <50m deep).
c               1: Land (not anything else).
c               2: Ocean Coastlines and Lake Shorelines.
c               3: Shallow Inland Water (Inland Water < 5km from shore
c                          OR < 50m deep).
c               4: Ephemeral (intermittent) Water.
c               5: Deep Inland Water (Inland water > 5km from shoreline
c                          AND > 50m deep).
c               6: Moderate or Continental Ocean (Ocean > 5km from coast
c                          AND > 50m deep AND < 500m deep).
c               7: Deep Ocean (Ocean > 500m deep).
c             255: Missing
c
c     In addition to the flat file containing the geolocation data,
c     a header file is created in ENVI-compatible format. The header file
c     describes the size and contents of the flat file, and it is required
c     by the IMAPP Level-2 science algorithms (e.g. cloudmask). It also
c     allows the image data to be read easily using Freelook or ENVI,
c     which are available from http://www.researchsystems.com/
c
c Usage:
c     modis_extract_geo in_hdf sat_name out_img out_hdr
c     where
c     in_hdf:  name of input MOD03 HDF file'
c     sat_name: platform name of 1km HDF file (Terra or Aqua)'
c     out_img: name of output image file (.img extension)'
c     out_hdr: name of output header file (.hdr extension)'
c
c To compile on SGI (note that HDF4.1 is required):
c
c   set HDFHOME = /usr/local/hdf4.1r3
c   f77 -n32 -O2 -extend_source -bytereclen \
c     -o modis_extract_geo modis_extract_geo.f \
c     write_modis_geo_header.f string_length.f big_endian.f \
c     -I$HDFHOME/include -L$HDFHOME/lib -lmfhdf -ldf -ljpeg -lz
c
c Author:
c     Liam.Gumley@ssec.wisc.edu
c-----------------------------------------------------------------------

      implicit none

c ... Include files
      include 'hdf.inc'

c ... Local variables
      integer file_id
      integer varid_lat, varid_lon, varid_senz, varid_sena,
     &  varid_solz, varid_sola, varid_elev, varid_land
      integer rtn
      integer rank, dimsizes(2), data_type, num_attrs
      character*100 sds_name 
      integer num_pixels, num_lines, num_scans
      
      integer lines_1km
      parameter (lines_1km = 10)
      
      integer start(2), stride(2), edge(2)

      integer max_pixel, max_param
      parameter (max_pixel = 1500, max_param = 8)

      real image(max_pixel, max_param)
            
      real float_buffer(max_pixel)
      integer*2 short_buffer(max_pixel)
      character*1 char_buffer(max_pixel)
            
      integer dim
      
      integer outlun
      parameter (outlun = 31)
      
      integer outrec
      
      integer pixel, line
      
      character*5 sat_name
      character*5 platform
      character*132 in_file, out_file, hdr_file

      real bad_value
      parameter (bad_value = -999.0)
      
      integer num_arg
      
      integer iostat
      
c ... External functions
      integer sfstart, sfselect, sfn2index, sfginfo, sfrdata,
     &  sfendacc, sfend
      external sfstart, sfselect, sfn2index, sfginfo, sfrdata,
     &  sfendacc, sfend

      integer iargc

      integer string_length
      external string_length
      
c-----------------------------------------------------------------------
c     GET INPUT ARGUMENTS
c-----------------------------------------------------------------------

c ... Check number of arguments
      num_arg = iargc()
      if (num_arg .ne. 4) then
        print *, 'Usage: modis_extract_geo in_hdf sat_name out_img out_hdr'
        print *, 'where'
        print *, 'in_hdf:  name of input MOD03 HDF file'
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
      print '(''Input MOD03 HDF file: '', a)', in_file(1: string_length(in_file))
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
        print *, 'Error: Could not open input MOD03 HDF file for reading'
        call exit(-1)
      endif
      
c ... Get the SDS id for each image array
      varid_lat  = sfselect(file_id, sfn2index(file_id, 'Latitude'))
      varid_lon  = sfselect(file_id, sfn2index(file_id, 'Longitude'))
      varid_senz = sfselect(file_id, sfn2index(file_id, 'SensorZenith'))
      varid_sena = sfselect(file_id, sfn2index(file_id, 'SensorAzimuth'))
      varid_solz = sfselect(file_id, sfn2index(file_id, 'SolarZenith'))
      varid_sola = sfselect(file_id, sfn2index(file_id, 'SolarAzimuth'))
      varid_elev = sfselect(file_id, sfn2index(file_id, 'Height'))
      varid_land = sfselect(file_id, sfn2index(file_id, 'Land/SeaMask'))
      if (varid_lat  .eq. -1 .or.
     &    varid_lon  .eq. -1 .or.
     &    varid_senz .eq. -1 .or.
     &    varid_sena .eq. -1 .or.
     &    varid_solz .eq. -1 .or.
     &    varid_sola .eq. -1 .or.
     &    varid_elev .eq. -1 .or.
     &    varid_land .eq. -1) then
        print *, 'Error: Input file is not a MOD03 HDF file'
        call exit(-1)
      endif
      
c ... Get the number of pixels, lines, and scans
      rtn = sfginfo(varid_lat, sds_name, rank, dimsizes, data_type, num_attrs) 
      num_pixels = dimsizes(1)
      num_lines  = dimsizes(2)
      num_scans  = num_lines / lines_1km
      print '(''Pixels = '', i6)', num_pixels
      print '(''Lines  = '', i6)', num_lines
      print '(''Scans  = '', i6)', num_scans

c ... Open the output file
      open(outlun, file=out_file, iostat=iostat, status='unknown',
     &  access='direct', form='unformatted', recl=(num_pixels * 4))
      if (iostat .ne. 0) then
        print *, 'Error: Could not open output image file for writing'
        call exit(-1)
      endif

c ... Create the header file
      call write_modis_geo_header(hdr_file, in_file,
     &  num_pixels, num_lines, bad_value, platform)

c-----------------------------------------------------------------------
c     READ THE IMAGE DATA FROM HDF INPUT AND WRITE TO BINARY OUTPUT
c-----------------------------------------------------------------------
                 
c ... Initialize output record counter
      outrec = 1
      
c ... Loop over each line
      do line = 0, (num_lines - 1), 1

c ...   Report progress on every 10th scan
        if (mod(line, 100) .eq. 0) print '(''Scan: '', i6)', line / 10

c ...   Set start vector
        start(1) = 0
        start(2) = line

c ...   Set stride vector
        stride(1) = 1
        stride(2) = 1
      
c ...   Set edge vector
        edge(1) = num_pixels
        edge(2) = 1

c ...   Read latitude data (float values, no scaling)
        rtn = sfrdata(varid_lat, start, stride, edge, float_buffer)
        if (rtn .eq. -1) then
          print *, 'Error reading latitude data'
          call exit(-1)
        endif

c ...   Copy to output buffer          
        dim = 1
        do pixel = 1, num_pixels
          if (float_buffer(pixel) .ge. -90.0) then
            image(pixel, dim) = float_buffer(pixel)
          else
            image(pixel, dim) = bad_value
          endif
        end do
        
c ...   Read longitude data (float values, no scaling)
        rtn = sfrdata(varid_lon, start, stride, edge, float_buffer)
        if (rtn .eq. -1) then
          print *, 'Error reading longitude data'
          call exit(-1)
        endif

c ...   Copy to output buffer          
        dim = 2
        do pixel = 1, num_pixels
          if (float_buffer(pixel) .ge. -180.0) then
            image(pixel, dim) = float_buffer(pixel)
          else
            image(pixel, dim) = bad_value
          endif
        end do

c ...   Read sensor zenith data  (signed short values, scaled by 0.01)
        rtn = sfrdata(varid_senz, start, stride, edge, short_buffer)
        if (rtn .eq. -1) then
          print *, 'Error reading sensor zenith data'
          call exit(-1)
        endif

c ...   Copy to output buffer          
        dim = 3
        do pixel = 1, num_pixels
          if (short_buffer(pixel) .ne. -32767) then
            image(pixel, dim) = real(short_buffer(pixel)) * 0.01
          else
            image(pixel, dim) = bad_value
          endif
        end do

c ...   Read sensor azimuth data (signed short values, scaled by 0.01)
        rtn = sfrdata(varid_sena, start, stride, edge, short_buffer)
        if (rtn .eq. -1) then
          print *, 'Error reading sensor azimuth data'
          call exit(-1)
        endif

c ...   Copy to output buffer          
        dim = 4
        do pixel = 1, num_pixels
          if (short_buffer(pixel) .ne. -32767) then
            image(pixel, dim) = real(short_buffer(pixel)) * 0.01
          else
            image(pixel, dim) = bad_value
          endif
        end do

c ...   Read solar zenith data (signed short values, scaled by 0.01)
        rtn = sfrdata(varid_solz, start, stride, edge, short_buffer)
        if (rtn .eq. -1) then
          print *, 'Error reading solar zenith data'
          call exit(-1)
        endif

c ...   Copy to output buffer          
        dim = 5
        do pixel = 1, num_pixels
          if (short_buffer(pixel) .ne. -32767) then
            image(pixel, dim) = real(short_buffer(pixel)) * 0.01
          else
            image(pixel, dim) = bad_value
          endif
        end do

c ...   Read solar azimuth data (signed short values, scaled by 0.01)
        rtn = sfrdata(varid_sola, start, stride, edge, short_buffer)
        if (rtn .eq. -1) then
          print *, 'Error reading solar azimuth data'
          call exit(-1)
        endif

c ...   Copy to output buffer          
        dim = 6
        do pixel = 1, num_pixels
          if (short_buffer(pixel) .ne. -32767) then
            image(pixel, dim) = real(short_buffer(pixel)) * 0.01
          else
            image(pixel, dim) = bad_value
          endif
        end do

c ...   Read terrain height data (signed short values, no scaling)
        rtn = sfrdata(varid_elev, start, stride, edge, short_buffer)
        if (rtn .eq. -1) then
          print *, 'Error reading terrain height data'
          call exit(-1)
        endif

c ...   Copy to output buffer          
        dim = 7
        do pixel = 1, num_pixels
          if (short_buffer(pixel) .ne. -32767) then
            image(pixel, dim) = real(short_buffer(pixel))
          else
            image(pixel, dim) = bad_value
          endif
        end do

c ...   Read land/sea mask data (byte values, no scaling)
        rtn = sfrdata(varid_land, start, stride, edge, char_buffer)
        if (rtn .eq. -1) then
          print *, 'Error reading land/sea mask data'
          call exit(-1)
        endif

c ...   Copy to output buffer          
        dim = 8
        do pixel = 1, num_pixels
          if (ichar(char_buffer(pixel)) .ne. 255) then
            image(pixel, dim) = ichar(char_buffer(pixel))
          else
            image(pixel, dim) = bad_value
          endif
        end do

c ...   Write the image data to the output file
        do dim = 1, max_param
          write(outlun, rec=outrec, iostat=iostat)
     &      (image(pixel, dim), pixel = 1, num_pixels)
          if (iostat .ne. 0) then
            print *, 'Error: Write to output geolocation file failed'
            call exit(-1)
          endif
          outrec = outrec + 1
        end do
        
      end do

c-----------------------------------------------------------------------
c     CLEANUP AND EXIT
c-----------------------------------------------------------------------

c ... End access to the image arrays
      rtn = sfendacc(varid_lat)
      rtn = sfendacc(varid_lon)
      rtn = sfendacc(varid_senz)
      rtn = sfendacc(varid_sena)
      rtn = sfendacc(varid_solz)
      rtn = sfendacc(varid_sola)
      rtn = sfendacc(varid_elev)
      rtn = sfendacc(varid_land)

c ... Close the input file
      rtn = sfend(file_id)

c ... Close the output file
      close(outlun)

      END
