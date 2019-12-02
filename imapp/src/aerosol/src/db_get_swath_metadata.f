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
      integer function db_get_swath_metadata(
     &                                     l1b_scan_lun,
     &                                     scan,
     &                                     scan_flag,
     &                                     mirror_side
     &                                   )
      implicit none
      save

C!F77------------------------------------------------------------------
C
C!DESCRIPTION:  Extracts Swath metadata for the given scan from the
c               direct broadcast scan based metadata file.
C
C!INPUT PARAMETERS:   
C
C       l1b_1km_lun        LUN for open 1km L1B data file
c       scan               Scan number within L1B granule
c
C!OUTPUT PARAMETERS: 
C
C       scan_flag          Day or night flag for this scan (1-Day,
C                                                           0-Other)
C       mirror_side        Mirror Side (1 or 0)
C
C!REVISION HISTORY:  
C
C!TEAM-UNIQUE HEADER: 
c This software is developed by the MODIS Science Data Support
c Team for the National Aeronautics and Space Administration,
c Goddard Space Flight Center, under contract NAS5-32373.
c
C!REFERENCES and CREDITS:
C
C!DESIGN NOTES:
C
C!END-----------------------------------------------------------------

c --- scalar arguments
      integer l1b_scan_lun,scan,scan_flag,mirror_side

c --- internal scalars
      byte b_value1,b_value2

c --- Common Block for debugging code
      integer debug,h_output
      common / mod06_debug / debug, h_output

c --- Read a scan record
      READ(
     &      UNIT=l1b_scan_lun,
     &      REC=scan,
     &      ERR=9991
     &     ) b_value1, b_value2

c --- fill parameters
      scan_flag = b_value1
      mirror_side = b_value2

c ....................................................................
      if (debug .gt. 0) then
        write(h_output,'(/,15x,'' Scan Level Metadata '')')
        write(h_output,'(/,2x,'' Scan Type  Mirror Side'')')
        write(h_output,'(7x,I1,11x,I1/)')scan_flag,mirror_side
      endif
c ....................................................................

      db_get_swath_metadata = 0
      return

9991  continue
      db_get_swath_metadata = -1
      return 

      end
