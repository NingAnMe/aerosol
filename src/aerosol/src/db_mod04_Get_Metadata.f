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
      subroutine db_mod04_Get_Metadata(header_1km,mask_hdr,
     &           nscans,npixels,datatype_1km,datat_mask,
     &           interleave_1km,interl_mask,resolution_1km,
     &           resol_mask,offset_1km,offset_mask,samples_1km,
     &           sampl_mask,lines_1km,lines_mask,error_1km,
     &           bands_1km,bands_mask,bandnames_1km,bnames_mask,
     &           bandunits_1km)
      implicit none
      save

C-----------------------------------------------------------------------
C !F77
C
C !PURPOSE:
C    Extract the information needed to begin processing the 
C     data for the Aerosol software (MOD04) that creates the
C      MODIS Direct Broadcast product.
C
C !DESCRIPTION:
C    Extracts information from both the the Geolocation and
C    L1b header files as well as the cloud mask file.
C
C !INPUT PARAMETERS:
C    header_1km      L1b 1km file header information.
C    mask_hdr        Input cloud mask file header information
C  
C !OUTPUT PARAMETERS:
C    nscans          Total number of 10 line scans in the l1b data set
C    npixels         Total number of pixels in a line of data
C    datatype_1km    Format of data in L1b file
C    datat_mask      Format of data in cloud mask file
C    interleave_1km  Order data is saved in L1b file
C    interl_mask     Order data is saved in cloud mask file
C    resolution_1km  L1b file data resolution
C    resol_mask      Cloud mask file data resolution
C    offset_1km      Offset, if any, of 1km L1B data
C    offset_mask     Offset, if any, of cloud mask file data
C    samples_1km     Number of elements in 1km L1b file
C    sampl_mask      Number of elements in cloud mask file
C    lines_1km       Number of lines of data in 1km L1b file
C    lines_mask      Number of lines of data in cloud mask file
C    error_1km       Bad data value of data in 1km L1b file
C    bands_1km       Number of bands in 1km L1b file
C    bands_mask      Number of bands in cloud mask file
C    bandnames_1km   Band numbers in 1km L1b file
C    bnames_mask     Band numbers in cloud mask file
C    bandunits_1km   Units for each band in the 1km L1b data file
C
C !REVISION HISTORY
C
C !TEAM-UNIQUE HEADER:
C    Developed by the MODIS Group, CIMSS/SSEC, UW-Madison.
C
C !END
C----------------------------------------------------------------------

      include           'COMMONS.inc'
      include           'db_mod04uw_debug.inc'

c ... array arguments 
      character*(*) header_1km,mask_hdr,
     +              interleave_1km,interl_mask,
     +              bandnames_1km(*),bnames_mask(*),bandunits_1km(*)

c ... scalar arguments
      integer nscans,npixels,datatype_1km,datat_mask,
     +        resolution_1km,resol_mask,offset_1km,offset_mask,
     +        samples_1km,sampl_mask,lines_1km,lines_mask,
     +        bands_1km,bands_mask

      real error_1km

c --- external functions
      integer hdrgetkeydbl
      integer hdrgetkeystr
      integer hdrgetkeyint

c --- internal variables
      character*(255) filename
      character*80 keyname,function
      double precision d_value
      integer keyindex,status,len
      logical remove_all


c --- initalize strings
      function = 'db_mod4_get_metadata'

      remove_all = .TRUE.

c ... initialize variables
      d_value = 0.0d0


C ---------------- 1KM -------------------

c --- copy file name to internal string
      call strcompress( header_1km, remove_all, len )
      filename = header_1km(1:len)

c --- datatype  
      keyname = "data type"
      keyindex = 1 
      status=hdrgetkeyint(filename,keyname,keyindex,
     &                    datatype_1km) 
      if( status.lt.0 ) then
         call message( function, 'FAILED - 1km datatype', 0, 3) 
         return
      endif

c --- interleave  
      keyname = "interleave"
      keyindex = 1 
      status=hdrgetkeystr(filename,keyname,keyindex,
     &                    interleave_1km) 
      if( status.lt.0 ) then
         call message( function, 'FAILED - 1km interleave', 0, 3) 
         return
      endif

c --- resolution  
      keyname = "resolution"
      keyindex = 1 
      status=hdrgetkeyint(filename,keyname,keyindex,
     &                    resolution_1km) 
      if( status.lt.0 ) resolution_1km = 1

c --- offset  
      keyname = "offset"
      keyindex = 1 
      status=hdrgetkeyint(filename,keyname,keyindex,
     &                    offset_1km) 
      if( status.lt.0 ) then
         call message( function, 'FAILED - 1km offset', 0, 3) 
         return
      endif

c --- error  
      keyname = "bad value"
      keyindex = 1 
      status=hdrgetkeydbl(filename,keyname,keyindex,
     &                    d_value) 
      if( status.lt.0 ) then
         call message( function, 'FAILED - 1km error', 0, 3) 
         return
      endif
      error_1km = REAL(d_value)

c --- get the number of scans in the image  
      keyname = "lines"
      keyindex = 1 
      status=hdrgetkeyint(filename,keyname,keyindex,
     &                    lines_1km) 
      if( status.lt.0 ) then
         call message( function, 'FAILED - 1km lines', 0, 3) 
         return
      endif
      nscans = lines_1km / No_Lines_Per_Scan

c --- get the maximum number of pixels per scan
      keyname = "samples"
      keyindex = 1 
      status=hdrgetkeyint(filename,keyname,keyindex,
     &                    samples_1km) 
      if( status.lt.0 ) then
         call message( function, 'FAILED - 1km samples', 0, 3) 
         return
      endif
      npixels = samples_1km

c --- get the number of bands
      keyname = "bands"
      keyindex = 1 
      status=hdrgetkeyint(filename,keyname,keyindex,
     &                    bands_1km) 
      if( status.lt.0 ) then
         call message( function, 'FAILED - 1km bands', 0, 3) 
         return
      endif

c --- get the band names and units
      do keyindex = 1, bands_1km
      keyname = "band names"
      status=hdrgetkeystr(filename,keyname,keyindex,
     &                    bandnames_1km(keyindex)) 
      if( status.lt.0 ) then
         call message(function,'FAILED - 1km band names',0,3) 
         return
      endif
      keyname = "band units"
      status=hdrgetkeystr(filename,keyname,keyindex,
     &                    bandunits_1km(keyindex)) 
      if( status.lt.0 ) then
         call message(function,'FAILED - 1km band units',0,3) 
         return
      endif
      enddo

C ---------------- CLOUD MASK  -------------------

c --- copy file name to internal string
c ... If you can't access the file header information,
c ...  then rely on defaults and keep processing
      call strcompress( mask_hdr, remove_all, len )
      filename = mask_hdr(1:len)

c --- datatype  
      keyname = "data type"
      keyindex = 1 
      status=hdrgetkeyint(filename,keyname,keyindex,
     &                    datat_mask) 
      if( status.lt.0 ) then
         call message( function, 'Can-t read mask datatype from ' //
     &                 'header.  Will use default of 1.', 0, 3) 
         datat_mask = 1
      endif

c --- interleave  
      keyname = "interleave"
      keyindex = 1 
      status=hdrgetkeystr(filename,keyname,keyindex,
     &                    interl_mask) 
      if( status.lt.0 ) then
         call message( function, 'Can-t read mask interleave type' //
     &     'from header.  Will use default of bsq.', 0, 3) 
         interl_mask = 'bsq'
      endif

c --- resolution  
      keyname = "resolution"
      keyindex = 1 
      status=hdrgetkeyint(filename,keyname,keyindex,
     &                    resol_mask) 
      if( status.lt.0 ) resol_mask = 1

c --- offset  
      keyname = "offset"
      keyindex = 1 
      status=hdrgetkeyint(filename,keyname,keyindex,
     &                    offset_mask) 
      if( status.lt.0 ) then
         call message( function, 'Can-t read mask offset value ' //
     &     'from header.  Will use default of 0.', 0, 3) 
         offset_mask = 0
      endif

c --- get the number of scans in the image  
      keyname = "lines"
      keyindex = 1 
      status=hdrgetkeyint(filename,keyname,keyindex,
     &                    lines_mask) 
      if( status.lt.0 ) then
         call message( function, 'Can-t read mask # lines value ' //
     &     'from header.  Will use value from L1B file. ', 0, 3) 
         lines_mask = lines_1km
      endif

c --- get the maximum number of pixels per scan
      keyname = "samples"
      keyindex = 1 
      status=hdrgetkeyint(filename,keyname,keyindex,
     &                    sampl_mask) 
      if( status.lt.0 ) then
         call message( function, 'Can-t read mask # pixels value ' //
     &     'from header.  Will use value from L1B file. ', 0, 3) 
         sampl_mask = samples_1km
      endif

c --- get the number of bands
      keyname = "bands"
      keyindex = 1 
      status=hdrgetkeyint(filename,keyname,keyindex,
     &                    bands_mask) 
      if( status.lt.0 ) then
         call message( function, 'Can-t read mask # bands value ' //
     &     'from header.  Will use a default value of 6. ', 0, 3) 
         bands_mask = 6
      endif

c --- get the band names and units
      do keyindex = 1, bands_mask
      keyname = "band names"
      status=hdrgetkeystr(filename,keyname,keyindex,
     &                    bnames_mask(keyindex)) 
      if( status.lt.0 ) then
         call message( function, 'Can-t read mask band name ' //
     &     'from header.  Will use a default value. ', 0, 3) 
      endif
      enddo
      if (status .lt. 0) then
          bnames_mask(1) = 'byte 1'
          bnames_mask(2) = 'byte 2'
          bnames_mask(3) = 'byte 3'
          bnames_mask(4) = 'byte 4'
          bnames_mask(5) = 'byte 5'
          bnames_mask(6) = 'byte 6'
      endif

c ---------------------------------------------------------------------
      if (debug .gt. 0) then
        WRITE(h_output,'(10x,'' Processing dimensions '')')
        WRITE(h_output,'(10x,'' Scans in Image    Pixels per Scan'','//
     &     '/,5x,I15,5x,I15/)') nscans, npixels
      endif
c ---------------------------------------------------------------------

      return
      end
