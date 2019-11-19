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
      integer function get_ancillary_aer( 
     &                                lun_met,
     &                                lun_ozone,
     &                                lun_icec,
     &                                lun_sst,
     &                                lun_nisen,
     &                                lun_nises,
     &                                lat, 
     &                                lon, 
     &                                met_pres, 
     &                                met_temp, 
     &                                met_mixr, 
     &                                met_land,
     &                                met_sfctmp, 
     &                                met_prmsl, 
     &                                met_pwat, 
     &                                met_ugrd, 
     &                                met_vgrd, 
     &                                ozone, 
     &                                icec, 
     &                                sst, 
     &                                nise
     &                              )
      implicit none
      save

c-----------------------------------------------------------------------
c !F77
c
c !DESCRIPTION:
c      Retrieve ancillary data items for a given latitude and longitude.
c
c !INPUT PARAMETERS:
c      LAT       Latitude (degrees, -90S to +90.0N)
c      LON       Longitude (degrees, -180W to +180E, Greenwich=0)
c
c !OUTPUT PARAMETERS:
c      PRES      Array of pressure levels (hPa)
c      TEMP      Array of atmospheric temperatures (K) at PRES(0:15)
c      MIXR      Array of water vapor mixing ratios (g/kg) at PRES(0:15)
c      LAND      Land mask (0=water, 1=land)
c      SFCTMP    Surface temperature (K)
c      PRMSL     Pressure (hPa) at mean sea level
c      PWAT      Precipitable water (g/cm**2)
c      UGRD      Surface wind u component (m/s)
c      VGRD      Surface wind v component (m/s)
c      OZONE     Total ozone (Dobsons)
c      ICEC      Ice concentration (fraction)
c      SST       Sea surface temperature (K) - valid over ocean only
c      NISE      NSIDC NISE snow/ice extent (see read_nise.f)
c    
c      The missing value for output parameters is MISSING (see below).
c
c !REVISION HISTORY:
c
c !TEAM-UNIQUE HEADER:
c      Developed by the MODIS Group, CIMSS/SSEC, UW-Madison.
c
c !DESIGN NOTES:
c      (1) On the first call, this subroutine unpacks and reads 4 files:
c          NCEP GDAS1 meteorological analysis data,
c          NCEP TOVS total ozone data,
c          NCEP SSMI ice concentration data,
c          NCEP sea surface temperature data,
c          NSIDC NISE snow/ice extent data.
c          On subsequent calls, data is obtained from SAVEd arrays.
c
c      (2) This subroutine will not cause an exit. If errors are
c          detected (e.g. missing or bad input file), the subroutine
c          will write a 'Recoverable error' message to the LogStatus
c          file, and will return missing value(s) for the parameter(s)
c          it failed to read.
c
c      (3) No checking of data validity is done within this routine.
c          Missing data values are used only where the input file was
c          either missing or bad. The user is responsible for checking
c          that ancillary data values (e.g. SST) are within an
c          acceptable range for user's application.
c
c !END
c-----------------------------------------------------------------------

c --- constants
      real		MISSING
      parameter 	(MISSING = -999.0)

      integer		GRIDSIZE
      parameter		(GRIDSIZE = 721)      

c --- parameters 
      real		lat
      real		lon
      real		met_pres(0:25)
      real		met_temp(0:25)
      real		met_mixr(0:25)
      real		met_land  
      real		met_sfctmp
      real		met_prmsl
      real		met_pwat
      real		met_ugrd
      real		met_vgrd
      real		ozone
      real		icec
      real		sst_bl
      real		sst
      integer		lun_met
      integer		lun_ozone
      integer		lun_icec
      integer		lun_sst
      integer		lun_nisen
      integer		lun_nises
      integer	 	nise
      
c --- external functions 
      integer 		ezlh_convert
      integer 		read_int
      real 		ppv
      real              read_real
      external 		ezlh_convert
      external          bl_int
      external          blint_met
      external          read_int
      external          read_real

      
c --- internal variables
      character*1       nise_north( GRIDSIZE, GRIDSIZE )
      character*1       nise_south( GRIDSIZE, GRIDSIZE )
      character*160     errmsg
      integer           xsize, ysize
      integer		i
      integer		j
      integer		k
      integer		ret
      integer		iret
      integer		level
      integer		ios
      integer           status
c ... Temporary sst holding variables
      integer  iword,iyrst,imst,idst,iyrnd,imnd,idnd,ndays,index,temp1
      real		x
      real		x0
      real		dx
      real		y
      real		y0
      real		dy
      real		p(0:25)
      real		satmix
      real		xlon
      real		met_grid( 0:359, 0:180, 0:53 )
      real		ozn_grid( 0:359, 0:180 )
      real		ice_grid( 0:719, 0:359 )
      real		sst_grid( 0:359, 0:179 )
      logical 		init
      logical		met_success
      logical		ozn_success
      logical		ice_success
      logical		sst_success
      logical		nisen_success      
      logical		nises_success      
      
c ... Temperature and moisture profile pressure levels (hPa)

      data p / 1000.0, 975.0, 950.0, 925.0, 900.0, 850.0, 800.0,
     &   750.0, 700.0, 650.0, 600.0, 550.0, 500.0, 450.0, 400.0,
     &   350.0, 300.0, 250.0, 200.0, 150.0, 100.0,  70.0,  50.0,
     &    30.0,  20.0,  10.0 /
                  
c --- Initialization flag
      data  		init/ .TRUE. /

c-----------------------------------------------------------------------
c --- initialization

c --- set function status to FAILED
      get_ancillary_aer = -1

c --- Open and read input data files if this is the first call
      if( init ) then

c ------ Set data ingest success/fail flags
         met_success   = .FALSE.
         ozn_success   = .FALSE.
         ice_success   = .FALSE.
         sst_success   = .FALSE.
         nisen_success = .FALSE.
         nises_success = .FALSE.


c ------ Read the met file
         if( lun_met.gt.0 ) then

            READ ( 
     &             UNIT=lun_met,
     &             REC=1,
     &             IOSTAT=ios
     &           ) met_grid

            if( ios.ne.0 ) then
               level = 1
               call message( 'get_ancillary_aer', 
     &                       'Error opening met file',
     &                       ios, level )
            else
               met_success = .true.
            endif

            CLOSE ( UNIT=lun_met )

         endif
 
       
c ------ Read the ozone file
         if ( lun_ozone.gt.0 ) then

            READ ( 
     &             UNIT=lun_ozone, 
     &             REC=1,  
     &             IOSTAT=ios 
     &           ) ozn_grid

            if( ios.ne.0 ) then
               level = 1
               call message( 'get_ancillary_aer', 
     &                       'Error reading ozone file',
     &                       ios, level )
            else
               ozn_success = .TRUE.
            endif

            CLOSE ( UNIT=lun_ozone )

         endif


c ------ Read the ice file
         if( lun_icec.gt.0 ) then

            READ( 
     &            UNIT=lun_icec,
     &            REC=1, 
     &            IOSTAT=ios
     &          ) ice_grid

            if( ios.ne.0 ) then
               level = 1
               call message( 'get_ancillary_aer', 
     &                       'Error reading ice file',
     &                       ios, level )
            else
               ice_success = .TRUE.
            endif  

            CLOSE ( UNIT=lun_icec )

         endif

c ------ Read the sst file
         if( lun_sst.gt.0 ) then

c --------- close the NISE file
            CLOSE ( UNIT=lun_nisen )
         endif


         if( lun_nises.gt.0 ) then
            xsize = GRIDSIZE
            ysize = GRIDSIZE

c --------- read the Southern grid
            READ (
     &             UNIT=lun_nises,
     &             REC=1,
     &             IOSTAT=ios 
     &           ) nise_south  

            if ( ios .ne. 0 ) then
               level = 1
               call message( 'get_ancillary_aer', 
     &                       'Error reading Southern NISE file.', 
     &                       ios, level )
            else
               nises_success = .TRUE.
            endif 

c           Call routine to fill in snow/ice info for coastal regions as
c           much as possible (northern hemisphere).
c           call massage_snowice(nise_south,xsize,ysize)

c --------- close the NISE file
            CLOSE ( UNIT=lun_nises )

         endif
   
c ------ Unset initialization flag
         init = .false.
        
      endif

c-----------------------------------------------------------------------

c --- SET MISSING VALUES
      ozone      = missing
      icec       = missing
      sst        = missing
      sst_bl     = missing
      nise       = int( missing )
      
c-----------------------------------------------------------------------

c --- MET DATA
      if( met_success ) then
      
c ------ Compute cell coordinates in met and ozn grids
         x = min( max( lon,  -179.99 ), 179.99 )
         if( x .lt. 0.0 ) x = x + 360.0
         x0 = 0.0
         dx = 1.0
         i = int( ( x - x0 + 0.5*dx ) / dx )
         if( i .eq. 360 ) i = 0
 
         y = min( max( lat, -89.99 ), 89.99 )
         y0 = 90.0
         dy = -1.0
         j = int( ( y - y0 + 0.5*dy ) / dy )

c ...    Set met return values to missing
         do k = 0, 25
           met_pres( k ) = missing
           met_temp( k ) = missing
           met_mixr( k ) = missing
         enddo
         met_land   = missing
         met_sfctmp = missing
         met_prmsl  = missing
         met_pwat   = missing
         met_ugrd   = missing
         met_vgrd   = missing


c ...    Save output pressure levels

          do k = 0, 25
            met_pres( k ) = p( k )
          end do

c ...     Save output met data
c ...     (note that water vapor profile is relative humidity (%))

          do k = 0, 25
            met_temp( k ) = met_grid( i, j, k )
          end do
          do k = 0, 20
            met_mixr( k ) = met_grid( i, j, k + 26 )
          end do
          met_land   = met_grid( i, j, 47 )

          call blint_met(met_grid(0,0,48), i, j, y, x, met_sfctmp, ret)
          if(ret .lt. 0) then
            level = 2
            status = -1
            write( errmsg,'(''Problem in blint_met '')')
            call message( 'get_ancillary_aer', errmsg ,
     &           status, level )
          end if

          call blint_met(met_grid(0,0,49), i, j, y, x, met_prmsl, ret)
          if(ret .lt. 0) then
            level = 2
            status = -1
            write( errmsg,'(''Problem in blint_met '')')
            call message( 'get_ancillary_aer', errmsg,
     &           status, level )
          end if
          met_prmsl = met_prmsl * 0.01

          met_pwat   = met_grid( i, j, 50 )
          met_ugrd   = met_grid( i, j, 51 )
          met_vgrd   = met_grid( i, j, 52 )

c ...     Convert relative humidity profile (%) to mixing ratio (g/kg)

          do k = 0, 20

c ...       Compute mixing ratio at 100% relative humidity

            satmix = 622.0 * ppv( met_temp( k ) ) / met_pres( k )

c ...       Convert relative humidity to mixing ratio

            met_mixr( k ) = satmix * 0.01 * met_mixr( k )

          end do

c ...     Extrapolate mixing ratio profile from 100 hPa to 10 hPa

          do k = 20, 25
            met_mixr(k) = max(met_mixr(20),0.003)*(met_pres(k)/100.0)**3
            met_mixr(k) = max(met_mixr(k), 0.003)
          end do

      endif

c-----------------------------------------------------------------------

c --- OZONE DATA
      if ( ozn_success ) then
      
c ------ Compute cell coordinates in met and ozn grids
         x = min( max( lon,  -179.99 ), 179.99 )
         if( x .lt. 0.0 ) x = x + 360.0
         x0 = 0.0
         dx = 1.0
         i = int( ( x - x0 + 0.5*dx ) / dx )
         if( i .eq. 360 ) i = 0
 
         y = min( max( lat, -89.99 ), 89.99 )
         y0 = 90.0
         dy = -1.0
         j = int( ( y - y0 + 0.5*dy ) / dy )

c ------ Save output ozone data
         ozone = ozn_grid( i, j )
        
      endif


c-----------------------------------------------------------------------

c --- ICE DATA
      if ( ice_success ) then
      
c ------ Compute cell coordinates in ice grid
         x = min( max( lon, -179.99 ), 179.99 )
         if( x .lt. 0.0 ) x = x + 360.0
         x0 = 0.25
         dx = 0.5
         i = int( ( x - x0 + 0.5*dx ) / dx )
         if( i .eq. 720 ) i = 0
 
         y = min( max( lat, -89.99 ), 89.99 )
         y0 = 89.75
         dy = -0.5
         j = int( ( y - y0 + 0.5*dy ) / dy )

c ------ Save output ice data
         icec = ice_grid( i, j )

      endif


c-----------------------------------------------------------------------

c --- SST DATA
      if( sst_success ) then
c ...   Compute cell coordinates in sst grid

c       Binary SST data is shifted 180 degrees relative to ASCII data.
        if (lon .lt. 0.0) then
          xlon = lon + 360.0
        else
          xlon = lon
        end if
        x = min( max( xlon, 0.00 ),  359.99 )
        x0 = 0.5

        dx = 1.0
        i = int( ( x - x0 + 0.5*dx ) / dx )

        y = min( max( lat, -89.99 ), 89.99 )
        y0 = -89.5
        dy = 1.0
        j = int( ( y - y0 + 0.5*dy ) / dy )

c ...   Bi-linearly interpolate SST

        call bl_int(sst_grid, i, j, lat, xlon, sst_bl, ret)
        if(ret .lt. 0) then
          level = 2
          status = -1
          write( errmsg,'(''Problem in bl_int '')')
          call message( 'get_ancillary_aer', errmsg ,
     &         status, level )
        end if

c ...   Save output sst data

        sst = sst_bl + 273.15

      endif


c-----------------------------------------------------------------------

c --- NISE DATA
      if( nisen_success .or. nises_success ) then

c ------- lat/lon bound checking
          x = min( max( lon, -179.99 ), 179.99 )
          y = min( max( lat,  -89.99 ),  89.99 )

c ------- Convert earth coordinates to NISE grid coordinates
          if ( y .lt. 0.0 ) then
             status = ezlh_convert( 'Sl', y, x, i, j )
          else
             status = ezlh_convert( 'Nl', y, x, i, j )
          endif

          if( status.ne.0 ) then
             level = 1
             call message( 'get_ancillary_aer', 
     &                     'Error converting lat,lon to NISE col,row',
     &                     status, level )

c ------- Save output NISE data for southern or northern hemisphere
          else
             if ( y .lt. 0.0 .and. nises_success ) then
                nise = ichar( nise_south( i, j ) )
             elseif ( y .ge. 0.0 .and. nisen_success ) then
                     nise = ichar( nise_north( i, j ) )
             endif
          endif

      endif

c-----------------------------------------------------------------------

c --- set function status to SUCCESS
      get_ancillary_aer = 0
         
      return     
      end
