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
      subroutine bl_int(grid, i, j, lat, lon, t, ret)

      IMPLICIT NONE
      SAVE

c-----------------------------------------------------------------------
c
c!F77
c
c!Description:
c   Bi-linear interpolation scheme used with Reynolds blended 
c   SST data.
c
c!Input Parameters:
c    grid   Real array containg SST grid values (360x180)
c    i      Grid index in the x direction
c    j      Grid index in the y direction
c    lat    Pixel latitude value
c    lon    Pixel longitude value
c
c!Output Parameters:
c    t      Interpolated SST value
c    ret    Return flag (0 = successful)
c
c!Revision History:
c 06/04 Collection 5  R. Frey   Original version.
c
c!Team-unique Header:
c
c!End
c
c-----------------------------------------------------------------------


c ... Input arguments

      REAL grid, lat, lon
      INTEGER i, j

c ... Output arguments

      REAL t
      INTEGER ret

c ... Parameters

      INTEGER npoints_x, npoints_y
      PARAMETER ( npoints_x = 360 )
      PARAMETER ( npoints_y = 180 )
     
      REAL dx, dy
      PARAMETER ( dx = 1.0)
      PARAMETER ( dy = 1.0)

      DIMENSION grid(0:npoints_x-1, 0:npoints_y-1)

c ... Local variables

      REAL p, ip0, ip1, jp0, jp1, t00, t01, t0,
     *     t10, t11, t1, ldi, ldj
      INTEGER i1, j1 
      LOGICAL y_interp

      ret = 0

      if( j .lt. 0 .or. j .gt. (npoints_y - 1) ) then
        ret = -1
      end if
      if( i .lt. 0 .or. i .gt. (npoints_x - 1) ) then
        ret = -1
      end if

      if( ret .eq. 0 ) then

        y_interp = .true.

        ldi = lon - i
        if(ldi .ge. 0.5) then
          i1 = i + 1
          if( i .eq. (npoints_x - 1) ) i1 = 0
        else
          i1 = i - 1
          if( i .eq. 0 ) i1 = npoints_x - 1
        end if

        ldj = lat - (j - 90.0) 
        if(ldj .ge. 0.5) then
          j1 = j + 1
          if( j .eq. (npoints_y - 1) ) y_interp = .false.
        else
          j1 = j - 1
          if( j .eq. 0 ) y_interp = .false.
        end if

        p = lon - (i1 + 0.5)
        if( p .lt. -1.0) p = p + 360.0
        if( p .gt. 1.0) p = p - 360.0
        ip0 = abs( p / dx)
        ip1 = dx - ip0

        t00 = grid( i, j)
        t01 = grid( i1, j)
        t0 = (ip0 * t00) + (ip1 * t01)

        if( y_interp ) then
 
          p = lat - (j1 - 90.0 + 0.5)
          jp0 = abs( p / dy )
          jp1 = dy - jp0
        
          t10 = grid(i, j1)
          t11 = grid(i1, j1)
          t1 = (ip0 * t10) + (ip1 * t11)

          t = (jp0 * t0) + (jp1 * t1)

        else

          t = t0

        end if

      end if

      return 
      end
