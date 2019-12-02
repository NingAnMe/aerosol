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
      subroutine DB_READ_ANC_DATA(Lat,Lon,anc_met_lun,ozone_lun,
     +                            sfctmp,ugrd,vgrd,pwat,ozone)

C-----------------------------------------------------------------------
C !F77
C
C !DESCRIPTION:
C
C      Read NCEP ancillary data 
C
C !INPUT PARAMETERS:
C
C      Lat             Latitude
C      Lon             Longitude
C      anc_met_lun     LUN for open met data file
C      ozone_lun       LUN for ozone ancillary data file
C
C !OUTPUT PARAMETERS:
C
C      sfctmp  surface temperature in degree K
C      ugrd    wind speed in x direction in m/sec
C      vgrd    wind speed in y direction in m/sec
C      pwat    total precipitable water in kg/m^2 
C      ozone   total ozone in dubson 
C
C !REVISION HISTORY:
C
C!TEAM-UNIQUE HEADER:
C
C Developed by the MODIS Group, CIMSS/SSEC, UW-Madison.
C
C!END
C-----------------------------------------------------------------------

      implicit none
      save

      include 'db_mod04uw_debug.inc'
      
c ... A test driver for the UW-MODIS Version 2 ancillary data reader.
c ... Revised 06-AUG-1997 Liam Gumley, CIMSS/SSEC

c ... scalar arguments
      integer anc_met_lun, ozone_lun
      real lat, lon

c ... array arguments
      real pres( 0:15), temp( 0:15 ), mixr( 0:15 ),
     +     sfctmp, pwat, ugrd, vgrd, ozone

c ... local scalars
      integer nise, k, status, lun_icec, lun_sst, lun_nisen, lun_nises
      real land, prmsl, icec, sst

c ... external routines
      integer get_ancillary_aer
      external get_ancillary_aer
 
c ... Set lun's that we aren't using to zero.  In this code, only use
c ...  NCEP met grid and NCEP ozone.
      lun_icec = 0
      lun_sst = 0
      lun_nisen = 0
      lun_nises = 0

c ... initialize variables
      status = -1
      do k = 0, 15
         pres( k ) = -999.0
         temp( k ) = -999.0
         mixr( k ) = -999.0
      end do
      land = -999.0
      sfctmp = -999.0
      prmsl = -999.0
      ozone = -999.0
      nise = -999
      sst = -999.0
      icec = -999.0

      status=get_ancillary_aer(anc_met_lun,ozone_lun,lun_icec,
     +  lun_sst,lun_nisen,lun_nises,lat,lon,pres,temp,mixr,land,
     +  sfctmp,prmsl,pwat,ugrd,vgrd,ozone,icec,sst,nise)
      if (status .lt. 0) then
          call message( 'db_get_ancillary',
     +                  'Error reading ancillary files.',
     +                   0, 2 )
      endif

c ... debug statement ............................................
      if( debug.gt.1) then
        write(h_output,'(/''Lat = '', f7.1, '' Lon = '', f7.1)') lat,lon
        write(h_output,'(''Presssure Profile (hPa)'')')
        write(h_output,'(16f7.1)') pres
        write(h_output,'(''Temperature Profile (K)'')')
        write(h_output,'(16f7.1)') temp
        write(h_output,'(''Mixing Ratio Profile (g/kg)'')')
        write(h_output,'(16f7.3)') mixr
        write(h_output,'(''ugrd, vgrd sfctmp'')')
        write(h_output,'(3f9.3)') ugrd, vgrd,sfctmp
      endif
c ................................................................

       return
       end
