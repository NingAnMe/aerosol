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
      subroutine db_mod04_write_products(out_handle,hdr_lun,scan,nscans,
     +                                  nboxes,SDSLAT,SDSLON,
     +                                  SDS_Tau_Land_Ocean,
     +                                  SDS_ratio_small_Land_Ocean,
     +                                  SDSTAU_corrected,SDSTAU_average)

c-----------------------------------------------------------------------
c
c!F77
c
c!Description:
c    Write MOD04 retrieval products to the output file.
c
c!Input Parameters:
c    OUT_HANDLE    LUN for MOD04 output file
c    HDR_LUN       LUN for product header output file
c    SCAN          Scan number within L1B granule
c    NSCANS        Total number of scans in this data set
c    NBOXES        Number of retrieval boxes along the scan
c                  (e.g. 1354 pixels, 5x5 sampling gives 1354/5 boxes)
c    SDSLAT              Real latitude at 10 km resolution
c    SDSLON              Real longitude at 10 km resolution
c    SDS_Tau_Land_Ocean  Best Aerosol Optical Depth Parameter 
c                         for both Land and Water
c    SDS_ratio_small_Land_Ocean  Corrected Optical depth ratio small
c                         mode for both Land and Water
c    SDSTAU_corrected     Corrected optical depth land only
c                         (3 wavelengths)
c    SDSTAU_average       Average Optical Depth ocean only 
c                         (7 wavelenths)
c    
c!Output Parameters:
c    None
c
c!Team-unique Header:
c     Developed by the MODIS Group, CIMSS/SSEC, UW-Madison.
c
c!End
c
c-----------------------------------------------------------------------
      
      implicit none

      include 'mod04.inc'

c ... arguments

      integer out_handle, scan, nscans, nboxes, hdr_lun
      real      SDSLAT(NUMCELLS), SDSLON(NUMCELLS)
      integer*2 SDS_Tau_Land_Ocean(NUMCELLS),
     +          SDS_ratio_small_Land_Ocean(NUMCELLS),
     +          SDSTAU_corrected(NUMCELLS,Land_Sol2),
     +          SDSTAU_average(NUMCELLS,NWAV_S)

c ... Parameters - Fill values
      integer FILL_INT
      parameter (FILL_INT = -9999)
      real FILL_REAL
      parameter (FILL_REAL = -999.0)
c ... Scale factor for products
      real scale
      parameter (scale = .001)

c ... local variables

      integer i, j, ii

      real rtau(NUMCELLS,MAX_SAMP_LINE), sds_tcor(NUMCELLS,Land_Sol2),
     +     rratio(NUMCELLS,MAX_SAMP_LINE), sds_tavg(NUMCELLS,NWAV_S),
     +     lat(NUMCELLS,MAX_SAMP_LINE), lon(NUMCELLS,MAX_SAMP_LINE),
     +     rtau2(NUMCELLS,MAX_SAMP_LINE)

c ... local arrays
      character*3 b_unit
      character*9 Band(7)

c ... External functions
      integer string_length
      external string_length

c ... Write each product array for this scan to output file

c ... Convert to real value - make sure Fill values are correct
      do i = 1, MAX_SAMP_LINE
         do j = 1 , NUMCELLS
            if (SDS_Tau_Land_Ocean(j) .eq. FILL_INT) then
               rtau(j,i) = FILL_REAL
            else
               rtau(j,i) = real(SDS_Tau_Land_Ocean(j)) * scale
            endif
            if (SDS_ratio_small_Land_Ocean(j) .eq. FILL_INT) then
               rratio(j,i) = FILL_REAL
            else
               rratio(j,i) = real(SDS_ratio_small_Land_Ocean(j)) * scale
            endif
            lat(j,i) = SDSLAT(j)
            lon(j,i) = SDSLON(j)
         enddo
      enddo
      do i = 1 , Land_Sol2
         do j = 1 , NUMCELLS
            if (SDSTAU_corrected(j,i) .eq. FILL_INT) then
                sds_tcor(j,i) = FILL_REAL
            else
                sds_tcor(j,i) = real(SDSTAU_corrected(j,i)) * scale
            endif
         enddo
      enddo
      do i = 1 , NWAV_S
         do j = 1 , NUMCELLS
            if (SDSTAU_average(j,i) .eq. FILL_INT) then
                sds_tavg(j,i) = FILL_REAL
            else
                sds_tavg(j,i) = real(SDSTAU_average(j,i)) * scale
            endif
         enddo
      enddo

      ii = 1
      b_unit = 'deg'
      call write_output( out_handle, hdr_lun, scan, nscans, nboxes, ii,
     +  lat, 'Latitude', b_unit )

      ii = ii + 1
      b_unit = 'deg'
      call write_output( out_handle, hdr_lun, scan, nscans, nboxes, ii,
     +  lon, 'Longitude', b_unit )

      ii = ii + 1
      b_unit = 'NA'
      call write_output( out_handle, hdr_lun, scan, nscans, nboxes, ii,
     +  rtau, 'Optical_Depth_Land_And_Ocean', b_unit )

      ii = ii+1
      b_unit = 'NA'
      call write_output( out_handle, hdr_lun, scan, nscans, nboxes, ii,
     +  rratio, 'SDS_ratio_small_Land_Ocean', b_unit )

      Band(1) = '.47micron'
      Band(2) = '.55micron'
      Band(3) = '.66micron'
      b_unit = 'NA'
      do i = 1, Land_Sol2
        ii = ii + 1
        call write_output(out_handle, hdr_lun, scan, nscans, nboxes, ii,
     +    sds_tcor(1,i), 'Corrected_Optical_Depth_Land_'
     +    // Band(i) , b_unit )
      end do

      Band(1) = '.47micron'
      Band(2) = '.55micron'
      Band(3) = '.66micron'
      Band(4) = '.86micron'
      Band(5) = '1.2micron'
      Band(6) = '1.6micron'
      Band(7) = '2.1micron'
      b_unit = 'NA'
      do i = 1, NWAV_S
        ii = ii + 1
        call write_output(out_handle, hdr_lun, scan, nscans, nboxes, ii,
     +    sds_tavg(1,i), 'Effective_Optical_Depth_Average_Ocean_'
     +    // Band(i) , b_unit )
      end do

      end

c-----------------------------------------------------------------------

      subroutine write_output(out_handle,hdr_lun,scan,nscans,nboxes,
     &                        req_band,array,arrnm,b_unit_name)
c-----------------------------------------------------------------------
c!F77
c
c!Description:
c    Write MOD04 retrieval products to the output file.
c
c!Input Parameters:
c    OUT_HANDLE    LUN for MOD06CT output file
c    HDR_LUN       LUN for product header output file
c    SCAN          MODIS scan number
c    NSCANS        Total number of scans in this data set
c    NBOXES        Number of 5x5 retrieval boxes for this scan
c    REQ_BAND      Output file band number
c    ARRAY         Array of product values
c                  (Bad values should be set to BAD_VALUE in the calling
c                  routine: see mod06uw_data.inc for BAD_VALUE value)
c    ARRNM         Name of the array (SDS name) in the output file
c
c!Output Parameters:
c    None
c
c!Team-unique Header:
c     Developed by the MODIS Group, CIMSS/SSEC, UW-Madison.
c
c!End
c-----------------------------------------------------------------------

      implicit none
      save

      include 'mod04.inc'
      
c ... arguments

      integer out_handle, scan, nboxes, nscans, req_band, hdr_lun
      real array(NUMCELLS,MAX_SAMP_LINE)
      character*(*) arrnm, b_unit_name
      logical init

c ... local variables
            
      integer i, j, out, ii, out_samples, out_lines, rtn, fil_datatype,
     &        fil_lun, fil_resolution, fil_offset, fil_samples, 
     &        fil_lines, fil_bands, out_maxsamples, out_maxlines,
     &        req_lines, req_samples, hdr_flg
      

      real fill_float, fil_error

c ... Local arrays
      byte out_flag(NUMCELLS*MAX_SAMP_LINE)
      character*4 fil_interleave
      character*72 text
      character*3 fil_bandunits(OUT_BAND)
      character*80 fil_bandnames(OUT_BAND)
      character*256 fil_desc
      real float_data( NUMCELLS * MAX_SAMP_LINE )

c ... external routines
      external db_mod04_write_flat_file
      integer db_mod04_write_flat_file

      integer string_length
      external string_length

      data init /.true./, ii /0/

 
      if (init) then
c ...   Set output file parameters
        req_samples = nboxes
        req_lines = MAX_SAMP_LINE

        fil_lun = out_handle
        fil_datatype = 4
        fil_interleave = 'bil'
        fil_resolution = 10
        fil_desc = 'Direct Broadcast Aerosol Product'
        fil_error = -327.68
        fil_offset = 0
        fil_samples = nboxes
        fil_lines = nscans * MAX_SAMP_LINE
        fil_bands = OUT_BAND
        out_maxsamples = NUMCELLS
        out_maxlines = MAX_SAMP_LINE
        init = .false.
      endif

c ... Fill in band and band unit information
      ii = ii + 1
      if (ii .le. OUT_BAND) then
        fil_bandnames(ii) = arrnm
        fil_bandunits(ii) = b_unit_name(1:string_length(b_unit_name))
      endif

c ... If we have gathered all the band names, then write header
      if (ii .eq. OUT_BAND) then
          hdr_flg = 1
      else
          hdr_flg = 0
      endif

      out = 1
c ... initialize the validity flag holder
      do j = 1 , MAX_SAMP_LINE
        do i = 1 , nboxes
           out_flag(out) = 0
        enddo
        out = out + 1
      enddo

c ... Get the fill value
      
      fill_float = -327.68
      
c ... Create the scaled output array

      out = 1
      
      do j = 1, MAX_SAMP_LINE

        do i = 1, nboxes

c ...     Set default output value (fill value)
        
          float_data( out ) = fill_float

c ...     If array value is good, scale it and store it
        
          if ( array( i, j ) .gt. -990.0 ) then

               float_data( out ) =  array( i, j ) 

          endif
          
          out = out + 1

        end do

      end do

c ... Write the output array and header to the output files

c ------ write the mask array to a flat output file
      rtn = db_mod04_write_flat_file(
     &                         scan,
     &                         req_samples,
     &                         req_lines,
     &                         req_band,
     &                         fil_lun,
     &                         hdr_flg,
     &                         hdr_lun,
     &                         fil_desc,
     &                         fil_datatype,
     &                         fil_interleave,
     &                         fil_resolution,
     &                         fil_error,
     &                         fil_offset,
     &                         fil_samples,
     &                         fil_lines,
     &                         fil_bands,
     &                         fil_bandnames,
     &                         fil_bandunits,
     &                         out_maxsamples,
     &                         out_maxlines,
     &                         float_data,
     &                         out_flag,
     &                         out_samples,
     &                         out_lines
     &                       )

      if ( rtn .ne. 0 ) then
        write(text, '(''Write failed on output array '',a)') arrnm
        call message( 'db_mod04_write_products', text ,
     &   0, 2 )
      endif
                
      end
c-----------------------------------------------------------------------
