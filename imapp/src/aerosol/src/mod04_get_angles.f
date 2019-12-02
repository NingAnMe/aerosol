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
      integer function mod04_get_angles(
     &                                  cube,
     &                                  lun_geo,
     &                                  datatype_geo,
     &                                  interleave_geo,
     &                                  resolution_geo,
     &                                  error_geo,
     &                                  offset_geo,
     &                                  samples_geo,
     &                                  lines_geo,
     &                                  bands_geo,
     &                                  bandnames_geo,
     &                                  bandunits_geo,
     &                                  rlat,
     &                                  rlon,
     &                                  view_zen,
     &                                  solar_zen,
     &                                  sens_azim,
     &                                  solar_azim,
     &                                  rel_azim,
     &                                  elev
     &                                 )
      implicit none
      save

c----------------------------------------------------------------------
c!F77
c
c!Description:
c     Routine for reading and putting MODIS cloud mask geometry
c     values into arrays for processing.
c
c!Input Parameters:
c cube          Scan cube counter
c max_sol       Maximum solar zenith angle of this granule
c min_sol       Minimum solar zenith anlge of this granule
c buf_geo_size  Line and element granule size
c
c!Output Parameters:
c
c rlat          Array containing scan cube of 1km pixel latitude
c               values
c rlon          Array containing scan cube of 1km pixel longitude
c               values
c view_zen      Array containing scan cube of 1km pixel viewing
c               zenith angles
c solar_zen     Array containing scan cube of 1km pixel solar
c               zenith angles
c view_azim     Array containing scan cube of 1km pixel viewing
c               azimuth angles
c solar_azim    Array containing scan cube of 1km pixel solar
c               azimuth angles
c rel_azim      Array containing scan cube of 1km pixel relative
c               azimuth angles
c elev          Array containing scan cube of 1km pixel elevation
c               values
c
c!Revision History:
c
c!Team-Unique Header:
c
c!References and Credits:
c See Cloud Mask ATBD-MOD-06.
c Revision 01.00  1996/2/13 08:20:37
c K. Strabala (kathys@ssec.wisc.edu)
c Original version.
c
c!END
c----------------------------------------------------------------------

      include 'mod04.inc'
      include 'db_mod04uw_debug.inc'

c --- parameters
      character*4 	interleave_geo
      character*80	bandnames_geo(20)
      character*80      bandunits_geo(20)
      integer 		cube
      integer		lun_geo
      integer		datatype_geo
      integer		resolution_geo
      integer		offset_geo
      integer		samples_geo
      integer		lines_geo
      integer		bands_geo
      real 		max_sol
      real		min_sol
      real		error_geo
      real              rlat(ISWATH,ILINE)
      real		rlon(ISWATH,ILINE) 
      real		view_zen(ISWATH,ILINE)
      real		solar_zen(ISWATH,ILINE) 
      real              elev(ISWATH,ILINE)
      real              sens_azim(ISWATH,ILINE)
      real              solar_azim(ISWATH,ILINE)
      real              rel_azim(ISWATH,ILINE)

c --- external functions
      integer 		db_read_flat_file
      intrinsic         abs
      intrinsic         nint

c --- internal variables
      character*80	req_bandname
      character*80	req_bandunit
      byte 		flag(ISWATH*ILINE)
      integer		req_resolution
      integer 		status
      integer 		ElementSize
      integer 		LineSize
      integer 		i
      integer 		j
      integer 		k
      integer 		data_size(2)
      real 		buf(ISWATH*ILINE)


c --- set function status to FAILED
      mod04_get_angles = -1

c --- debug
      if (debug .gt. 3) then
         write(h_output,'(10x/,''Within get_angles routine '',/)')
      endif


c --- initialize output paramaters
      data_size(1) = 0
      data_size(2) = 0
      ElementSize = samples_geo
      LineSize = lines_geo

      do i = 1 , ISWATH
      do j = 1 , ILINE
         rlat(i,j)       = FV_GEO
         rlon(i,j)       = FV_GEO
         elev(i,j)       = FV_GEO
         view_zen(i,j)   = FV_GEO
         solar_zen(i,j)  = FV_GEO
         sens_azim(i,j)  = FV_GEO
         solar_azim(i,j) = FV_GEO
         rel_azim(i,j)   = FV_GEO
      enddo
      enddo


c ------------------------------------------------------------------
c --- Read the latitude values
      req_resolution = 1
      req_bandname   = 'Latitude'
      req_bandunit   = 'deg'

      do i = 1, ISWATH*ILINE
      buf(i)  = FV_GEO
      enddo

      status = db_read_flat_file(
     &  	               cube,	 	
     &			       req_resolution,
     &			       req_bandname,
     &			       req_bandunit,
     &			       lun_geo,
     &			       datatype_geo,
     &			       interleave_geo,
     &			       resolution_geo,
     &                         error_geo,
     &			       offset_geo,
     &			       samples_geo,
     &			       lines_geo,
     &			       bands_geo,
     &			       bandnames_geo,
     &			       bandunits_geo,
     &			       ElementSize,
     &			       LineSize,
     &			       buf,
     &			       flag,
     &			       data_size(1),
     &			       data_size(2)
     &                       )

c --- check for error
      if( status.ne.0 )then
         call message ('get_angles',
     &   'Failed to extract latitude from Geo. file',
     &   0, 2 )
         return
      endif

c --- Place the data into the holding arrays
      k = 1
      do i = 1 , data_size(2)
      do j = 1 , data_size(1)
      if( buf(k).ge.-90.0 .and.buf(k).le.90.0 ) then
         rlat(j,i) = buf(k)
      endif
      k = k+1
      enddo
      enddo


c ------------------------------------------------------------------


c ------------------------------------------------------------------
c --- Read the longitude values
      req_resolution = 1
      req_bandname   = 'Longitude'
      req_bandunit   = 'deg'

      do i = 1, ISWATH*ILINE
      buf(i)  = FV_GEO
      enddo

      status = db_read_flat_file(
     &  	               cube,	 	
     &			       req_resolution,
     &			       req_bandname,
     &			       req_bandunit,
     &			       lun_geo,
     &			       datatype_geo,
     &			       interleave_geo,
     &			       resolution_geo,
     &                         error_geo,
     &			       offset_geo,
     &			       samples_geo,
     &			       lines_geo,
     &			       bands_geo,
     &			       bandnames_geo,
     &			       bandunits_geo,
     &			       ElementSize,
     &			       LineSize,
     &			       buf,
     &			       flag,
     &			       data_size(1),
     &			       data_size(2)
     &                       )

c --- check for error
      if( status.ne.0 )then
         call message ('get_angles',
     &   'Failed to extract longitude from Geo. file',
     &   0, 2 )
         return
      endif

c --- Place the data into the holding arrays
      k = 1
      do i = 1 , data_size(2)
      do j = 1 , data_size(1)
      if( buf(k).ge.-180.0 .and.buf(k).le.180.0 ) then
         rlon(j,i) = buf(k)
      endif
      k = k+1
      enddo
      enddo

c ------------------------------------------------------------------


c ------------------------------------------------------------------
c --- Read elevation values
      req_resolution = 1
      req_bandname   = 'Elevation'
      req_bandunit   = 'm  '

      do i = 1, ISWATH*ILINE
      buf(i)  = FV_GEO
      enddo

      status = db_read_flat_file(
     &  	               cube,	 	
     &			       req_resolution,
     &			       req_bandname,
     &			       req_bandunit,
     &			       lun_geo,
     &			       datatype_geo,
     &			       interleave_geo,
     &			       resolution_geo,
     &                         error_geo,
     &			       offset_geo,
     &			       samples_geo,
     &			       lines_geo,
     &			       bands_geo,
     &			       bandnames_geo,
     &			       bandunits_geo,
     &			       ElementSize,
     &			       LineSize,
     &			       buf,
     &			       flag,
     &			       data_size(1),
     &			       data_size(2)
     &                       )

c --- check for error
      if( status.ne.0 )then
         call message ('get_angles',
     &   'Failed to extract elevation from Geo. file',
     &   0, 2 )
         return
      endif

c --- Place the data into the holding arrays
      k = 1
      do i = 1 , data_size(2)
      do j = 1 , data_size(1)
      if( buf(k).ge.-500.0 .and.buf(k).le.9000.0 ) then
         elev(j,i) = buf(k)
      endif
      k = k+1
      enddo
      enddo

c ------------------------------------------------------------------


c ------------------------------------------------------------------
c --- Read the solar zenith angle information
      req_resolution = 1
      req_bandname   = 'SolarZenith'
      req_bandunit   = 'deg'

      do i = 1, ISWATH*ILINE
      buf(i)  = FV_GEO
      enddo

      status = db_read_flat_file(
     &  	               cube,	 	
     &			       req_resolution,
     &			       req_bandname,
     &			       req_bandunit,
     &			       lun_geo,
     &			       datatype_geo,
     &			       interleave_geo,
     &			       resolution_geo,
     &                         error_geo,
     &			       offset_geo,
     &			       samples_geo,
     &			       lines_geo,
     &			       bands_geo,
     &			       bandnames_geo,
     &			       bandunits_geo,
     &			       ElementSize,
     &			       LineSize,
     &			       buf,
     &			       flag,
     &			       data_size(1),
     &			       data_size(2)
     &                       )

c --- check for error
      if( status.ne.0 )then
         call message ('get_angles',
     &   'Failed to extract solar zenith from Geo. file',
     &   0, 2 )
         return
      endif

c --- Place the data into the holding arrays
      k = 1
      do i = 1 , data_size(2)
      do j = 1 , data_size(1)
      if( buf(k).ge.0.0 .and.buf(k).le.180.0 ) then
         solar_zen(j,i) = buf(k)

c ------ Determine max and min solar zenith angles of granule
         if( nint(solar_zen(j,i)).ne.-32767 .and.
     &       solar_zen(j,i).gt.max_sol)  max_sol = solar_zen(j,i)
         if (nint(solar_zen(j,i)) .ne. -32767 .and.
     &       solar_zen(j,i).lt.min_sol)  min_sol = solar_zen(j,i)

      endif
      k = k+1
      enddo
      enddo

c ------------------------------------------------------------------


c ------------------------------------------------------------------
c --- Read the sensor zenith angle information
      req_resolution = 1
      req_bandname   = 'SensorZenith'
      req_bandunit   = 'deg'

      do i = 1, ISWATH*ILINE
      buf(i)  = FV_GEO
      enddo

      status = db_read_flat_file(
     &  	               cube,	 	
     &			       req_resolution,
     &			       req_bandname,
     &			       req_bandunit,
     &			       lun_geo,
     &			       datatype_geo,
     &			       interleave_geo,
     &			       resolution_geo,
     &                         error_geo,
     &			       offset_geo,
     &			       samples_geo,
     &			       lines_geo,
     &			       bands_geo,
     &			       bandnames_geo,
     &			       bandunits_geo,
     &			       ElementSize,
     &			       LineSize,
     &			       buf,
     &			       flag,
     &			       data_size(1),
     &			       data_size(2)
     &                       )

c --- check for error
      if( status.ne.0 )then
         call message ('get_angles',
     &   'Failed to extract sensor zenith from Geo. file',
     &   0, 2 )
         return
      endif

c --- Place the data into the holding arrays
      k = 1 
      do i = 1 , data_size(2)
      do j = 1 , data_size(1)
      if( buf(k).ge.0.0 .and.buf(k).le.180.0 ) then
         view_zen(j,i) = buf(k)
      endif
      k = k+1
      enddo
      enddo

c ------------------------------------------------------------------

c ------------------------------------------------------------------
c ------ Read the sensor azimuth angle information
         req_resolution = 1
         req_bandname   = 'SensorAzimuth'
         req_bandunit   = 'deg'

         do i = 1, ISWATH*ILINE
         buf(i)  = FV_GEO
         enddo

         status = db_read_flat_file(
     &  	                  cube,	 	
     &		 	          req_resolution,
     &			          req_bandname,
     &			          req_bandunit,
     &			          lun_geo,
     &			          datatype_geo,
     &			          interleave_geo,
     &			          resolution_geo,
     &                            error_geo,
     &			          offset_geo,
     &			          samples_geo,
     &			          lines_geo,
     &			          bands_geo,
     &			          bandnames_geo,
     &			          bandunits_geo,
     &			          ElementSize,
     &			          LineSize,
     &			          buf,
     &			          flag,
     &			          data_size(1),
     &			          data_size(2)
     &                          )

c ------ check for error
         if( status.ne.0 )then
            call message ('get_angles',
     &      'Failed to extract sensor azimuth from Geo. file',
     &      0, 2 )
         endif

c ------ Place the data into the holding arrays
         k = 1
         do i = 1 , data_size(2)
         do j = 1 , data_size(1)
         if( buf(k).ge.-180.0 .and.buf(k).le.180.0 ) then
            sens_azim(j,i) = buf(k)
         endif
         k = k+1
         enddo
         enddo

c ------------------------------------------------------------------


c ------------------------------------------------------------------
c ------ Read the solar azimuth angle information
         req_resolution = 1
         req_bandname   = 'SolarAzimuth'
         req_bandunit   = 'deg'

         do i = 1, ISWATH*ILINE
         buf(k)  = FV_GEO
         enddo

         status = db_read_flat_file(
     &  	                  cube,	 	
     &		 	          req_resolution,
     &			          req_bandname,
     &			          req_bandunit,
     &			          lun_geo,
     &			          datatype_geo,
     &			          interleave_geo,
     &			          resolution_geo,
     &                            error_geo,
     &			          offset_geo,
     &			          samples_geo,
     &			          lines_geo,
     &			          bands_geo,
     &			          bandnames_geo,
     &			          bandunits_geo,
     &			          ElementSize,
     &			          LineSize,
     &			          buf,
     &			          flag,
     &			          data_size(1),
     &			          data_size(2)
     &                          )

c ------ check for error
         if( status.ne.0 )then
            call message ('get_angles',
     &      'Failed to extract solar azimuth from Geo. file',
     &      0, 2 )
            return
         endif

c ------ Place the data into the holding arrays
         k = 1
         do i = 1 , data_size(2)
         do j = 1 , data_size(1)
         if( buf(k).ge.-180.0 .and.buf(k).le.180.0 ) then
            solar_azim(j,i) = buf(k)
         endif
         k = k+1
         enddo
         enddo

c ------------------------------------------------------------------

c ------------------------------------------------------------------
c ------ compute the relative azimuth angle
         do i = 1 , data_size(2)
         do j = 1 , data_size(1)
            if(nint(solar_azim(j,i))  .ne. nint(FV_GEO) .and.
     +         nint(sens_azim(j,i))  .ne. nint(FV_GEO)) then

              rel_azim(j,i) = abs(sens_azim(j,i)-solar_azim(j,i))
              rel_azim(j,i) = amod(sens_azim(j,i),360.0)
              if (rel_azim(j,i) .gt. 180.0)  
     +          rel_azim(j,i) = 360.0 - rel_azim(j,i)
            endif
         enddo
         enddo
c ------------------------------------------------------------------


c ... debug statement ............................................
      if( debug.gt.1) then
        write(h_output,'(15x,'' ANGLE VALUES: '',2i10)') data_size(1),
     *        data_size(2)
        write(h_output,'(2x,'' Latitude'',/,20(5f10.4/))')
     +       ((rlat(j,i),j=1,10),i=1,10)
        write(h_output,'(2x,'' Longitude'',/,20(5f10.4/))')
     +       ((rlon(j,i),j=1,10),i=1,10)
        write(h_output,'(2x,'' Elevation'',/,20(5f10.4/))')
     +       ((elev(j,i),j=1,10),i=1,10)
        write(h_output,'(2x,'' Solar Zenith'',/,20(5f10.4/))')
     +       ((solar_zen(j,i),j=1,10),i=1,10)
        write(h_output,'(2x,'' Viewing Zenith'',/,20(5f10.4/))')
     +       ((view_zen(j,i),j=1,10),i=1,10)
        write(h_output,'(2x,'' Solar Azimuth '',/,20(5f10.4/))')
     +       ((solar_azim(j,i),j=1,10),i=1,10)
        write(h_output,'(2x,'' Sensor Azimuth '',/,20(5f10.4/))')
     +       ((sens_azim(j,i),j=1,10),i=1,10)
        write(h_output,'(2x,'' Relative Azimuth'',/,20(5f10.4/))')
     +       ((rel_azim(j,i),j=1,10),i=1,10)
      endif
c ................................................................

c --- set function status to SUCCESS
      mod04_get_angles = 0

      return
      end
