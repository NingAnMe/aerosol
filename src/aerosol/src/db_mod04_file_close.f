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
      subroutine db_mod04_file_close( 
     &                                l1b_1km_lun,
     &                                l1b_hkm_lun,
     &                                l1b_qkm_lun,
     &                                geo_1km_lun,
     &                                mask_lun,
     &                                mask_qa_lun,
     &                                scan_1km_lun,
     &                                anc_met_lun,
     &                                ozone_lun,
     &                                handle_LUT466,
     &                                handle_LUT553,
     &                                handle_LUT644,
     &                                handle_LUT213,
     &                                handle_LUTMAP,
     &                                handle_INSCI,
     &                                handle_S,
     &                                handle_L,
     &                                mod04_lun,
     &                                hdr_lun)

      implicit none
      save

C-----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION:
c Program which will close files used in the DB Atmospheric  
c Profiles software.
C
C!INPUT PARAMETERS:   
C
C   l1b_1km_lun        LUN for open 1km L1B data file
C   l1b_hkm_lun        LUN for open half km L1B data file
C   l1b_qkm_lun        LUN for open quarter km L1B data file
C   geo_1km_lun        LUN for open geo file
C   mask_lun           LUN for open Cloud Mask file
C   mask_qa_lun        LUN for open Cloud Mask QA file
C   scan_1km_lun       LUN for scan based metadata file
C   anc_met_lun        LUN for open met data file
C   ozone_lun          LUN for ozone ancillary data file
C   handle_LUT466      LUN for look up table 466 coefficient file
C   handle_LUT553      LUN for look up table 553 coefficient file
C   handle_LUT644      LUN for look up table 644 coefficient file
C   handle_LUT213      LUN for look up table 213 coefficient file
C   handle_LUTMAP      LUN for aerosol map look up table
C   handle_INSCI       LUN for ASCII input ocean look up table.
C   handle_S           LUN's for small mode sea coeffiecient files
C   handle_L           LUN's for large mode sea coeffiecient files
C   mod04_lun          LUN for MODIS Aerosol binary product output file
C   hdr_lun            LUN for MODIS Aerosol header output file
C   h_output           LUN for text debug output file (from include file)
C
C!OUTPUT PARAMETERS:   None.
C
C
C!REVISION HISTORY:
c
C!TEAM-UNIQUE HEADER:
C
C!REFERENCES AND CREDITS
C
C EXTERNALS:
C
C       NAMED CONSTANTS:
C
C
C!END-------------------------------------------------------------------

      include 'db_mod04uw_debug.inc'

c --- arguments
      integer handle_S(3), handle_L(3)
      integer l1b_1km_lun, l1b_hkm_lun, l1b_qkm_lun, geo_1km_lun,
     +        mask_lun, mask_qa_lun, scan_1km_lun, anc_met_lun,
     +        ozone_lun, mod04_lun, hdr_lun, 
     +        handle_LUT466, handle_LUT553, handle_LUT644,
     +        handle_LUT213, handle_LUTMAP, handle_INSCI, i

c ... close files
      close(l1b_1km_lun)
      close(l1b_hkm_lun)
      close(l1b_qkm_lun)
      close(geo_1km_lun)
      if (anc_met_lun .gt. 0) close(anc_met_lun)
      if (ozone_lun .gt. 0) close(ozone_lun)
      close(mask_lun)
      close(mask_qa_lun)
      close(scan_1km_lun)
      close(mod04_lun)
      close(hdr_lun)
      do i = 1 , 3
         if (handle_S(i) .gt. 0) close(handle_S(i))
         if (handle_L(i) .gt. 0) close(handle_L(i))
      enddo
      if (handle_LUT466 .gt. 0) close(handle_LUT466)
      if (handle_LUT553 .gt. 0) close(handle_LUT553)
      if (handle_LUT644 .gt. 0) close(handle_LUT644)
      if (handle_LUT213 .gt. 0) close(handle_LUT213)
      if (handle_LUTMAP .gt. 0) close(handle_LUTMAP)
      if (handle_INSCI .gt. 0) close(handle_INSCI)

      if (debug .ne. 0) close(h_output)

      return
      end
