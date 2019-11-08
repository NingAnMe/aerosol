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
      INTEGER FUNCTION EZLH_CONVERT( GRID, LAT, LON, COL, ROW )

c-----------------------------------------------------------------------
c !F77
c
c !DESCRIPTION:
c    For a given latitude and longitude, compute the column and row
c    indices in the NSIDC EASE-Grid. Information on this grid is at
c    http://www-nsidc.colorado.edu/NASA/GUIDE/EASE/ease_maps_info.html
c
c !INPUT PARAMETERS:
c    GRID    String indicating grid name in the format '[NSM][lh]'
c            'N' = Northern Hemisphere azimuthal projection
c            'S' = Southern Hemisphere azimuthal projection
c            'M' = Mercator global projection
c            'l' = Low resolution (25 km)
c            'h' = High resolution (12.5 km)
c    LAT     Latitude (degrees, -90S to +90N)
c    LON     Longitude (degrees, -180W to 180E, Greenwich=0)
c
c !OUTPUT PARAMETERS:
c    EZLH_CONVERT    Return status
c                     0 => Success
c                    -1 => Latitude not in range -90 to 90
c                    -2 => Longitude not in range -180 to 180
c                    -3 => GRID string not recognized
c    COL             Column index in grid (minimum value 1)
c    ROW             Row index in grid (minimum value 1)
c
c !REVISION HISTORY:
c
c     Original version was obtained from
c     ftp://sidads.colorado.edu/pub/GENERIC_SOFTWARE/ezlhconv.f
c
c !TEAM-UNIQUE HEADER:
c     Developed by the MODIS Group, CIMSS/SSEC, UW-Madison.
c
c !END
c-----------------------------------------------------------------------

      IMPLICIT NONE
      
c ... Input arguments

      CHARACTER*(*) grid
      REAL lat, lon
      
c ... Output arguments

      INTEGER col, row

c ... Local variables

      INTEGER cols, rows, scale
      REAL lam, phi, r0, rg, rho, s0, t, r, s

c ... Radius of the earth (km), authalic sphere based on International datum 

      REAL re_km
      PARAMETER ( re_km = 6371.228 )

c ... Nominal cell size in kilometers

      REAL cell_km
      PARAMETER ( cell_km = 25.067525 )

c ... Scale factor for standard parallels at +/-30.00 degrees

      REAL cos_phi1
      PARAMETER ( cos_phi1 = 0.86602540 )

      REAL pi
      PARAMETER ( PI = 3.1415927 )

c ... Statement functions

      REAL rad
      rad( t ) = t * ( PI / 180.0 )

c ... Check input arguments

      if ( lat .lt. -90.0 .or. lat .gt. 90.0 ) then
        ezlh_convert = -1
        return
      endif
      
      if ( lon .lt. -180.0 .or. lon .gt. 180.0 ) then
        ezlh_convert = -2
        return
      endif

c ... Get grid type
      
      if ((grid(1:1).eq.'N').or.(grid(1:1).eq.'S')) then
        cols = 721
        rows = 721
      else if (grid(1:1).eq.'M') then
        cols = 1383
        rows = 586
      else
        ezlh_convert = -3
        return
      endif

      if (grid(2:2).eq.'l') then
        scale = 1
      else if (grid(2:2).eq.'h') then
        scale = 2
      else
        ezlh_convert = -3
        return
      endif

c ... r0,s0 are defined such that cells at all scales 
c ... have coincident center points

      r0 = (cols-1)/2. * scale
      s0 = (rows-1)/2. * scale

c ... Compute grid index

      phi = rad(lat)
      lam = rad(lon)
      Rg = scale * RE_km/CELL_km

      if (grid(1:1).eq.'N') then
        rho = 2 * Rg * sin(PI/4. - phi/2.)
        r = r0 + rho * sin(lam)
        s = s0 + rho * cos(lam)
      else if (grid(1:1).eq.'S') then
        rho = 2 * Rg * cos(PI/4. - phi/2.)
        r = r0 + rho * sin(lam)
        s = s0 - rho * cos(lam)
      else if (grid(1:1).eq.'M') then
        r = r0 + Rg * lam * COS_PHI1
        s = s0 - Rg * sin(phi) / COS_PHI1
      endif

c ... Compute final results and return

      col = nint( r )
      row = nint( s )
      ezlh_convert = 0

      END
