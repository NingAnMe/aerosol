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
      real function ppv(T)

C-----------------------------------------------------------------------
C !F77
C
C !DESCRIPTION:
C Compute saturation vapor pressure over water for a given temperature.
C   Marco Matricardi. ECMWF. January 2004. 
C
C !INPUT PARAMETERS:
C REAL            TEMP        Temperature (K)
C
C !OUTPUT PARAMETERS:
C REAL            SVPWAT      Saturation vapor pressure over water (mb)
C
C !REVISION HISTORY:
C
c!Team-Unique Header:
c    Developed by the MODIS Group, CIMSS/SSEC, UW-Madison.
C
C !END
C-----------------------------------------------------------------------
      implicit none
      save


C     Calculate saturation water vapour pressure 

C     Temperature [K] 
C     Saturation water vapour pressure [mb]
      real     T
      real     esi,esw
      real     ti,t00,e00

c     ------------------------------------------------------------------
c     1. Constants
c     ------------------------------------------------------------------
      e00 = 611.21
      t00 = 273.16
      ti  = t00 - 23.

c     ------------------------------------------------------------------
c     2. Saturation vapour pressure
c     ------------------------------------------------------------------

      esw     = e00*exp(17.502*(t-t00)/(t-32.19))
      esi     = e00*exp(22.587*(t-t00)/(t+0.7  ))

C     Water phase
      if(T.gt.t00)then
        ppv = esw
C     Mixed phase
      elseif(t.gt.ti.and.t.le.t00)then
        ppv = esi+(esw-esi)*((t-ti)/(t00-ti))**2
C     Ice phase
      elseif(t.le.ti)then
        ppv = esi
      endif

C     Conversion from [Pascal] to [mb]
      ppv=ppv/100.

      return
      end


