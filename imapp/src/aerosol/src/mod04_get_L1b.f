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
      integer function mod04_get_L1b(l1b_1km_lun, l1b_hkm_lun,
     +        l1b_qkm_lun, datatype_1km, interleave_1km,
     +        resolution_1km, offset_1km, samples_1km, lines_1km,    
     +        error_1km, bands_1km, bandnames_1km, bandunits_1km,
     +        cube, Buf_Size1, Buf_Size2, Refl_1, Refl_2, Refl_3,
     +        Refl_4, Refl_5, Refl_6, Refl_7, Refl_9, Refl_12,
     +        Refl_13, Refl_16, Refl_26, Rad_20, Rad_31, Rad_32,
     +        NUMSQ,data_size)
      implicit none
      save

C----------------------------------------------------------------------
C!F77
C
C!Description:
C     Routine for reading MODIS level 1B radiances and relfectances
C     and placing in appropriate arrays for processing.
C
c!Input Parameters:
C    l1b_1km_lun        LUN for open 1km L1B data file
C    l1b_hkm_lun        LUN for open half km L1B data file
C    l1b_qkm_lun        LUN for open quarter km L1B data file
C    datatype_1km       Format of data in L1b file
C    interleave_1km     Order data is saved in L1b file
C    resolution_1km     L1b file data resolution
C    offset_1km         Offset, if any, of 1km L1B data
C    samples_1km        Number of elements in 1km L1b file
C    lines_1km          Number of lines of data in 1km L1b file
C    error_1km          Bad data value of data in 1km L1b file
C    bands_1km          Number of bands in 1km L1b file
C    bandnames_1km      Band numbers in 1km L1b file
C    bandunits_1km      Units for each band in the 1km L1b data file
C    cube               Scan cube number
C    Buf_Size1          Buffer size for first array index
C    Buf_Size2          Buffer size for second array index
C
C!Output Parameters:
C    REAL     Refl_1(4x,4y) Storage buffer for band 1 reflectances.
C    REAL     Refl_2(4x,4y) Storage buffer for band 2 reflectances.
C    REAL     Refl_3(2x,2y) Storage buffer for band 3 reflectances.
C    REAL     Refl_4(2x,2y) Storage buffer for band 4 reflectances.
C    REAL     Refl_5(2x,2y) Storage buffer for band 5 reflectances.
C    REAL     Refl_6(2x,2y) Storage buffer for band 6 reflectances.
C    REAL     Refl_7(2x,2y) Storage buffer for band 7 reflectances.
C    REAL     Refl_9(x,y)   Storage buffer for band 9 reflectances.
C    REAL     Refl_12(x,y)  Storage buffer for band 12 reflectances.
C    REAL     Refl_13(x,y)  Storage buffer for band 13 reflectances.
C    REAL     Refl_16(x,y)  Storage buffer for band 16 reflectances.
C    REAL     Refl_26(x,y)  Storage buffer for band 26 reflectances.
C    REAL     Rad_20(x,y)   Storage buffer for band 20 radiances.
C    REAL     Rad_31(x,y)   Storage buffer for band 31 radiances.
C    REAL     Rad_32(x,y)   Storage buffer for band 32 radiances.
c    NUMSQ    Not sure what this means, but it seems to be important
c    data_size  Integer array storing actual x and y values
c
c!Revision History:
c
c!Team-Unique Header:
c
c!References and Credits:
c See Cloud Mask ATBD-MOD-06.
c
c!END
c----------------------------------------------------------------------

      include 'mod04.inc'
      include 'db_mod04uw_debug.inc'

c ... scalar arguments
      integer datatype_1km, resolution_1km, offset_1km, samples_1km,
     +        lines_1km, bands_1km, cube, l1b_1km_lun, NUMSQ,
     +        l1b_hkm_lun, l1b_qkm_lun, Buf_Size1, Buf_Size2
      real error_1km

c ... array arguments
      integer       data_size(2)
      character*(*) interleave_1km,  bandnames_1km(*),
     +              bandunits_1km(*)
      Real  Refl_1(Buf_Size1,Buf_Size2),
     +      Refl_2(Buf_Size1,Buf_Size2),
     +      Refl_3(Buf_Size1,Buf_Size2),
     +      Refl_4(Buf_Size1,Buf_Size2),
     +      Refl_5(Buf_Size1,Buf_Size2),
     +      Refl_6(Buf_Size1,Buf_Size2),
     +      Refl_7(Buf_Size1,Buf_Size2),
     +      Refl_9(Buf_Size1,Buf_Size2),
     +      Refl_12(Buf_Size1,Buf_Size2),
     +      Refl_13(Buf_Size1,Buf_Size2),
     +      Refl_16(Buf_Size1,Buf_Size2),
     +      Refl_26(Buf_Size1,Buf_Size2),
     +      Rad_20(Buf_Size1,Buf_Size2),
     +      Rad_31(Buf_Size1,Buf_Size2),Rad_32(Buf_Size1,Buf_Size2)


c --- 250m file swath metadata
      integer	        datatype_250m
      character*(4)     interleave_250m
      integer           resolution_250m
      real		error_250m
      integer	        offset_250m
      integer           samples_250m
      integer           lines_250m
      integer           bands_250m
      character*80      bandnames_250m(2)
      character*80      bandunits_250m(2)

c --- 500m file swath metadata
      integer	        datatype_500m
      character*(4)     interleave_500m
      integer           resolution_500m
      real		error_500m
      integer	        offset_500m
      integer           samples_500m
      integer           lines_500m
      integer           bands_500m
      character*80      bandnames_500m(7)
      character*80      bandunits_500m(7)

c --- external subroutines
      integer 		db_read_flat_file
      external 		message

c --- internal variables
      byte              v1km_flag(Buf_Size1*Buf_Size2)
      byte              v250m_flag(Buf_Size1*Buf_Size2*4*4)
      byte              v500m_flag(Buf_Size1*Buf_Size2*4)
      character*80      req_bandname
      character*80	req_bandunit
      integer		req_resolution
      integer 		ll
      integer 		kk
      integer 		mm
      integer 		nn
      integer 		ii
      integer 		jj
      integer 		ElementSize
      integer 		LineSize
      integer		i
      integer		j
      integer		k
      integer           status
      integer		len
      logical 		rtn
      real              v1km_buf(Buf_Size1*Buf_Size2)
      real              v500m_buf(Buf_Size1*Buf_Size2*4)
      real              v250m_buf(Buf_Size1*Buf_Size2*4*4)

c --- set inital function status to FAILED
      mod04_get_L1b = -1
      debug = 3
c ... Initialize output arrays

      do i = 1 , Buf_Size2
         do j = 1 , Buf_Size1
            Refl_1(j,i) = FV_L1B
            Refl_2(j,i) = FV_L1B
            Refl_3(j,i) = FV_L1B
            Refl_4(j,i) = FV_L1B
            Refl_5(j,i) = FV_L1B
            Refl_6(j,i) = FV_L1B
            Refl_7(j,i) = FV_L1B
            Refl_9(j,i) = FV_L1B
            Refl_12(j,i) = FV_L1B
            Refl_13(j,i) = FV_L1B
            Refl_16(j,i) = FV_L1B
            Refl_26(j,i) = FV_L1B
            Rad_20(j,i) = FV_L1B
            Rad_31(j,i) = FV_L1B
            Rad_32(j,i) = FV_L1B
         enddo
      enddo

c ...   250 M DATA ................................................
C     -----------------
C     Read MODIS band 1
C     -----------------

c ... Initialize output variables
      data_size(1) = 0
      data_size(2) = 0

c ... Initialize input variables
      req_resolution = 1
      req_bandname = '1'
      req_bandunit = 'ref'
      ElementSize = Buf_Size1
      LineSize = Buf_Size2

c --------- Read a 1km scan cube-s worth of data out of the granule
            status = db_read_flat_file(
     &			  	      cube,
     &				      req_resolution,
     &				      req_bandname,
     &				      req_bandunit,
     &				      l1b_1km_lun,
     &				      datatype_1km,
     &				      interleave_1km,
     &				      resolution_1km,
     &                                error_1km,
     &				      offset_1km,
     &				      samples_1km,
     &				      lines_1km,
     &				      bands_1km,
     &				      bandnames_1km,
     &				      bandunits_1km,
     &				      ElementSize,
     &				      LineSize,
     &				      v1km_buf,
     &				      v1km_flag,
     &				      data_size(1),
     &				      data_size(2)
     &                               )

             if(status .lt.0 ) then
                call message('mod04_get_L1b',
     &           'Failed to extract 1km L1B data for band '//req_bandname
     &            ,0, 3 )
             endif

c ---------- Place the data into the 1km holding arrays
             k = 1
             do ii = 1 , data_size(2)
               do jj = 1 , data_size(1)
                 if (v1km_flag(k) .eq. 0)
     +               Refl_1(jj,ii) = v1km_buf(k)
                 k = k+1
               enddo
             enddo

C     -----------------
C     Read MODIS band 2
C     -----------------

c ... Initialize output variables
      data_size(1) = 0
      data_size(2) = 0

c ... Initialize input variables
      req_resolution = 1
      req_bandname = '2'
      req_bandunit = 'ref'
      ElementSize = Buf_Size1
      LineSize = Buf_Size2

c --------- Read a 1km scan cube-s worth of data out of the granule
            status = db_read_flat_file(
     &			  	      cube,
     &				      req_resolution,
     &				      req_bandname,
     &				      req_bandunit,
     &				      l1b_1km_lun,
     &				      datatype_1km,
     &				      interleave_1km,
     &				      resolution_1km,
     &                                error_1km,
     &				      offset_1km,
     &				      samples_1km,
     &				      lines_1km,
     &				      bands_1km,
     &				      bandnames_1km,
     &				      bandunits_1km,
     &				      ElementSize,
     &				      LineSize,
     &				      v1km_buf,
     &				      v1km_flag,
     &				      data_size(1),
     &				      data_size(2)
     &                               )

             if(status .lt.0 ) then
                call message('mod04_get_L1b',
     &           'Failed to extract 1km L1B data for band '//req_bandname
     &            ,0, 3 )
             endif

c ---------- Place the data into the 1km holding arrays
             k = 1
             do ii = 1 , data_size(2)
               do jj = 1 , data_size(1)
                 if (v1km_flag(k) .eq. 0)
     +               Refl_2(jj,ii) = v1km_buf(k)
                 k = k+1
               enddo
             enddo


c ...   500 M DATA ................................................

C     -----------------
C     Read MODIS band 3
C     -----------------

c ... Initialize output variables
      data_size(1) = 0
      data_size(2) = 0

c ... Initialize input variables
      req_resolution = 1
      req_bandname = '3'
      req_bandunit = 'ref'
      ElementSize = Buf_Size1
      LineSize = Buf_Size2

c --------- Read a 1km scan cube-s worth of data out of the granule
            status = db_read_flat_file(
     &			  	      cube,
     &				      req_resolution,
     &				      req_bandname,
     &				      req_bandunit,
     &				      l1b_1km_lun,
     &				      datatype_1km,
     &				      interleave_1km,
     &				      resolution_1km,
     &                                error_1km,
     &				      offset_1km,
     &				      samples_1km,
     &				      lines_1km,
     &				      bands_1km,
     &				      bandnames_1km,
     &				      bandunits_1km,
     &				      ElementSize,
     &				      LineSize,
     &				      v1km_buf,
     &				      v1km_flag,
     &				      data_size(1),
     &				      data_size(2)
     &                               )

             if(status .lt.0 ) then
                call message('mod04_get_L1b',
     &           'Failed to extract 1km L1B data for band '//req_bandname
     &            ,0, 3 )
             endif

c ---------- Place the data into the 1km holding arrays
             k = 1
             do ii = 1 , data_size(2)
               do jj = 1 , data_size(1)
                 if (v1km_flag(k) .eq. 0)
     +               Refl_3(jj,ii) = v1km_buf(k)
                 k = k+1
               enddo
             enddo

C     -----------------
C     Read MODIS band 4
C     -----------------

c ... Initialize output variables
      data_size(1) = 0
      data_size(2) = 0

c ... Initialize input variables
      req_resolution = 1
      req_bandname = '4'
      req_bandunit = 'ref'
      ElementSize = Buf_Size1
      LineSize = Buf_Size2

c --------- Read a 1km scan cube-s worth of data out of the granule
            status = db_read_flat_file(
     &			  	      cube,
     &				      req_resolution,
     &				      req_bandname,
     &				      req_bandunit,
     &				      l1b_1km_lun,
     &				      datatype_1km,
     &				      interleave_1km,
     &				      resolution_1km,
     &                                error_1km,
     &				      offset_1km,
     &				      samples_1km,
     &				      lines_1km,
     &				      bands_1km,
     &				      bandnames_1km,
     &				      bandunits_1km,
     &				      ElementSize,
     &				      LineSize,
     &				      v1km_buf,
     &				      v1km_flag,
     &				      data_size(1),
     &				      data_size(2)
     &                               )

             if(status .lt.0 ) then
                call message('mod04_get_L1b',
     &           'Failed to extract 1km L1B data for band '//req_bandname
     &            ,0, 3 )
             endif

c ---------- Place the data into the 1km holding arrays
             k = 1
             do ii = 1 , data_size(2)
               do jj = 1 , data_size(1)
                 if (v1km_flag(k) .eq. 0)
     +               Refl_4(jj,ii) = v1km_buf(k)
                 k = k+1
               enddo
             enddo


C     -----------------
C     Read MODIS band 5
C     -----------------

c ... Initialize output variables
      data_size(1) = 0
      data_size(2) = 0

c ... Initialize input variables
      req_resolution = 1
      req_bandname = '5'
      req_bandunit = 'ref'
      ElementSize = Buf_Size1
      LineSize = Buf_Size2

c --------- Read a 1km scan cube-s worth of data out of the granule
            status = db_read_flat_file(
     &			  	      cube,
     &				      req_resolution,
     &				      req_bandname,
     &				      req_bandunit,
     &				      l1b_1km_lun,
     &				      datatype_1km,
     &				      interleave_1km,
     &				      resolution_1km,
     &                                error_1km,
     &				      offset_1km,
     &				      samples_1km,
     &				      lines_1km,
     &				      bands_1km,
     &				      bandnames_1km,
     &				      bandunits_1km,
     &				      ElementSize,
     &				      LineSize,
     &				      v1km_buf,
     &				      v1km_flag,
     &				      data_size(1),
     &				      data_size(2)
     &                               )

             if(status .lt.0 ) then
                call message('mod04_get_L1b',
     &           'Failed to extract 1km L1B data for band '//req_bandname
     &            ,0, 3 )
             endif

c ---------- Place the data into the 1km holding arrays
             k = 1
             do ii = 1 , data_size(2)
               do jj = 1 , data_size(1)
                 if (v1km_flag(k) .eq. 0)
     +               Refl_5(jj,ii) = v1km_buf(k)
                 k = k+1
               enddo
             enddo

C     -----------------
C     Read MODIS band 6
C     -----------------

c ... Initialize output variables
      data_size(1) = 0
      data_size(2) = 0

c ... Initialize input variables
      req_resolution = 1
      req_bandname = '6'
      req_bandunit = 'ref'
      ElementSize = Buf_Size1
      LineSize = Buf_Size2

c --------- Read a 1km scan cube-s worth of data out of the granule
            status = db_read_flat_file(
     &			  	      cube,
     &				      req_resolution,
     &				      req_bandname,
     &				      req_bandunit,
     &				      l1b_1km_lun,
     &				      datatype_1km,
     &				      interleave_1km,
     &				      resolution_1km,
     &                                error_1km,
     &				      offset_1km,
     &				      samples_1km,
     &				      lines_1km,
     &				      bands_1km,
     &				      bandnames_1km,
     &				      bandunits_1km,
     &				      ElementSize,
     &				      LineSize,
     &				      v1km_buf,
     &				      v1km_flag,
     &				      data_size(1),
     &				      data_size(2)
     &                               )

             if(status .lt.0 ) then
                call message('mod04_get_L1b',
     &           'Failed to extract 1km L1B data for band '//req_bandname
     &            ,0, 3 )
             endif

c ---------- Place the data into the 1km holding arrays
             k = 1
             do ii = 1 , data_size(2)
               do jj = 1 , data_size(1)
                 if (v1km_flag(k) .eq. 0)
     +               Refl_6(jj,ii) = v1km_buf(k)
                 k = k+1
               enddo
             enddo

C     -----------------
C     Read MODIS band 7
C     -----------------

c ... Initialize output variables
      data_size(1) = 0
      data_size(2) = 0

c ... Initialize input variables
      req_resolution = 1
      req_bandname = '7'
      req_bandunit = 'ref'
      ElementSize = Buf_Size1
      LineSize = Buf_Size2

c --------- Read a 1km scan cube-s worth of data out of the granule
            status = db_read_flat_file(
     &			  	      cube,
     &				      req_resolution,
     &				      req_bandname,
     &				      req_bandunit,
     &				      l1b_1km_lun,
     &				      datatype_1km,
     &				      interleave_1km,
     &				      resolution_1km,
     &                                error_1km,
     &				      offset_1km,
     &				      samples_1km,
     &				      lines_1km,
     &				      bands_1km,
     &				      bandnames_1km,
     &				      bandunits_1km,
     &				      ElementSize,
     &				      LineSize,
     &				      v1km_buf,
     &				      v1km_flag,
     &				      data_size(1),
     &				      data_size(2)
     &                               )

             if(status .lt.0 ) then
                call message('mod04_get_L1b',
     &           'Failed to extract 1km L1B data for band '//req_bandname
     &            ,0, 3 )
             endif

c ---------- Place the data into the 1km holding arrays
             k = 1
             do ii = 1 , data_size(2)
               do jj = 1 , data_size(1)
                 if (v1km_flag(k) .eq. 0)
     +               Refl_7(jj,ii) = v1km_buf(k)
                 k = k+1
               enddo
             enddo


C ...   1KM DATA ................................................
C     -----------------
C     Read MODIS band 9
C     -----------------

c ... Initialize output variables
      data_size(1) = 0
      data_size(2) = 0

c ... Initialize input variables
      req_resolution = 1
      req_bandname = '9'
      req_bandunit = 'ref'
      ElementSize = Buf_Size1
      LineSize = Buf_Size2

c --------- Read a 1km scan cube-s worth of data out of the granule
            status = db_read_flat_file(
     &			  	      cube,
     &				      req_resolution,
     &				      req_bandname,
     &				      req_bandunit,
     &				      l1b_1km_lun,
     &				      datatype_1km,
     &				      interleave_1km,
     &				      resolution_1km,
     &                                error_1km,
     &				      offset_1km,
     &				      samples_1km,
     &				      lines_1km,
     &				      bands_1km,
     &				      bandnames_1km,
     &				      bandunits_1km,
     &				      ElementSize,
     &				      LineSize,
     &				      v1km_buf,
     &				      v1km_flag,
     &				      data_size(1),
     &				      data_size(2)
     &                               )

             if(status .lt.0 ) then
                call message('mod04_get_L1b',
     &           'Failed to extract 1km L1B data for band '//req_bandname
     &            ,0, 3 )
             endif

c ---------- Place the data into the 1km holding arrays
             k = 1
             do ii = 1 , data_size(2)
               do jj = 1 , data_size(1)
                 if (v1km_flag(k) .eq. 0)
     +               Refl_9(jj,ii) = v1km_buf(k)
                 k = k+1
               enddo
             enddo

C     ------------------
C     Read MODIS band 12
C     ------------------

c ... Initialize output variables
      data_size(1) = 0
      data_size(2) = 0

c ... Initialize input variables
      req_resolution = 1
      req_bandname = '12'
      req_bandunit = 'ref'
      ElementSize = Buf_Size1
      LineSize = Buf_Size2

c --------- Read a 1km scan cube-s worth of data out of the granule
            status = db_read_flat_file(
     &                                cube,
     &                                req_resolution,
     &                                req_bandname,
     &                                req_bandunit,
     &                                l1b_1km_lun,
     &                                datatype_1km,
     &                                interleave_1km,
     &                                resolution_1km,
     &                                error_1km,
     &                                offset_1km,
     &                                samples_1km,
     &                                lines_1km,
     &                                bands_1km,
     &                                bandnames_1km,
     &                                bandunits_1km,
     &                                ElementSize,
     &                                LineSize,
     &                                v1km_buf,
     &                                v1km_flag,
     &                                data_size(1),
     &                                data_size(2)
     &                               )

             if(status .lt.0 ) then
                call message('mod04_get_L1b',
     &           'Failed to extract 1km L1B data for band '//req_bandname
     &            ,0, 3 )
             endif

c ---------- Place the data into the 1km holding arrays
             k = 1
             do ii = 1 , data_size(2)
               do jj = 1 , data_size(1)
                 if (v1km_flag(k) .eq. 0)
     +               Refl_12(jj,ii) = v1km_buf(k)
                 k = k+1
               enddo
             enddo

C     ------------------
C     Read MODIS band 12
C     ------------------

c ... Initialize output variables
      data_size(1) = 0
      data_size(2) = 0

c ... Initialize input variables
      req_resolution = 1
      req_bandname = '12'
      req_bandunit = 'ref'
      ElementSize = Buf_Size1
      LineSize = Buf_Size2

c --------- Read a 1km scan cube-s worth of data out of the granule
            status = db_read_flat_file(
     &                                cube,
     &                                req_resolution,
     &                                req_bandname,
     &                                req_bandunit,
     &                                l1b_1km_lun,
     &                                datatype_1km,
     &                                interleave_1km,
     &                                resolution_1km,
     &                                error_1km,
     &                                offset_1km,
     &                                samples_1km,
     &                                lines_1km,
     &                                bands_1km,
     &                                bandnames_1km,
     &                                bandunits_1km,
     &                                ElementSize,
     &                                LineSize,
     &                                v1km_buf,
     &                                v1km_flag,
     &                                data_size(1),
     &                                data_size(2)
     &                               )

             if(status .lt.0 ) then
                call message('mod04_get_L1b',
     &           'Failed to extract 1km L1B data for band '//req_bandname
     &            ,0, 3 )
             endif

c ---------- Place the data into the 1km holding arrays
             k = 1
             do ii = 1 , data_size(2)
               do jj = 1 , data_size(1)
                 if (v1km_flag(k) .eq. 0)
     +               Refl_12(jj,ii) = v1km_buf(k)
                 k = k+1
               enddo
             enddo

C     ------------------
C     Read MODIS band 13
C     ------------------

c ... Initialize output variables
      data_size(1) = 0
      data_size(2) = 0

c ... Initialize input variables
      req_resolution = 1
      req_bandname = '13'
      req_bandunit = 'ref'
      ElementSize = Buf_Size1
      LineSize = Buf_Size2

c --------- Read a 1km scan cube-s worth of data out of the granule
            status = db_read_flat_file(
     &                                cube,
     &                                req_resolution,
     &                                req_bandname,
     &                                req_bandunit,
     &                                l1b_1km_lun,
     &                                datatype_1km,
     &                                interleave_1km,
     &                                resolution_1km,
     &                                error_1km,
     &                                offset_1km,
     &                                samples_1km,
     &                                lines_1km,
     &                                bands_1km,
     &                                bandnames_1km,
     &                                bandunits_1km,
     &                                ElementSize,
     &                                LineSize,
     &                                v1km_buf,
     &                                v1km_flag,
     &                                data_size(1),
     &                                data_size(2)
     &                               )

             if(status .lt.0 ) then
                call message('mod04_get_L1b',
     &           'Failed to extract 1km L1B data for band '//req_bandname
     &            ,0, 3 )
             endif

c ---------- Place the data into the 1km holding arrays
             k = 1
             do ii = 1 , data_size(2)
               do jj = 1 , data_size(1)
                 if (v1km_flag(k) .eq. 0)
     +               Refl_13(jj,ii) = v1km_buf(k)
                 k = k+1
               enddo
             enddo

C     ------------------
C     Read MODIS band 16
C     ------------------

c ... Initialize output variables
      data_size(1) = 0
      data_size(2) = 0

c ... Initialize input variables
      req_resolution = 1
      req_bandname = '16'
      req_bandunit = 'ref'
      ElementSize = Buf_Size1
      LineSize = Buf_Size2

c --------- Read a 1km scan cube-s worth of data out of the granule
            status = db_read_flat_file(
     &                                cube,
     &                                req_resolution,
     &                                req_bandname,
     &                                req_bandunit,
     &                                l1b_1km_lun,
     &                                datatype_1km,
     &                                interleave_1km,
     &                                resolution_1km,
     &                                error_1km,
     &                                offset_1km,
     &                                samples_1km,
     &                                lines_1km,
     &                                bands_1km,
     &                                bandnames_1km,
     &                                bandunits_1km,
     &                                ElementSize,
     &                                LineSize,
     &                                v1km_buf,
     &                                v1km_flag,
     &                                data_size(1),
     &                                data_size(2)
     &                               )

             if(status .lt.0 ) then
                call message('mod04_get_L1b',
     &           'Failed to extract 1km L1B data for band '//req_bandname
     &            ,0, 3 )
             endif

c ---------- Place the data into the 1km holding arrays
             k = 1
             do ii = 1 , data_size(2)
               do jj = 1 , data_size(1)
                 if (v1km_flag(k) .eq. 0)
     +               Refl_16(jj,ii) = v1km_buf(k)
                 k = k+1
               enddo
             enddo

C     ------------------
C     Read MODIS band 26
C     ------------------

c ... Initialize output variables
      data_size(1) = 0
      data_size(2) = 0

c ... Initialize input variables
      req_resolution = 1
      req_bandname = '26'
      req_bandunit = 'ref'
      ElementSize = Buf_Size1
      LineSize = Buf_Size2

c --------- Read a 1km scan cube-s worth of data out of the granule
            status = db_read_flat_file(
     &                                cube,
     &                                req_resolution,
     &                                req_bandname,
     &                                req_bandunit,
     &                                l1b_1km_lun,
     &                                datatype_1km,
     &                                interleave_1km,
     &                                resolution_1km,
     &                                error_1km,
     &                                offset_1km,
     &                                samples_1km,
     &                                lines_1km,
     &                                bands_1km,
     &                                bandnames_1km,
     &                                bandunits_1km,
     &                                ElementSize,
     &                                LineSize,
     &                                v1km_buf,
     &                                v1km_flag,
     &                                data_size(1),
     &                                data_size(2)
     &                               )

             if(status .lt.0 ) then
                call message('mod04_get_L1b',
     &           'Failed to extract 1km L1B data for band '//req_bandname
     &            ,0, 3 )
             endif

c ---------- Place the data into the 1km holding arrays
             k = 1
             do ii = 1 , data_size(2)
               do jj = 1 , data_size(1)
                 if (v1km_flag(k) .eq. 0)
     +               Refl_26(jj,ii) = v1km_buf(k)
                 k = k+1
               enddo
             enddo

C     ------------------
C     Read MODIS band 20
C     ------------------

c ... Initialize output variables
      data_size(1) = 0
      data_size(2) = 0

c ... Initialize input variables
      req_resolution = 1
      req_bandname = '20'
      req_bandunit = 'rad'
      ElementSize = Buf_Size1
      LineSize = Buf_Size2

c --------- Read a 1km scan cube-s worth of data out of the granule
            status = db_read_flat_file(
     &                                cube,
     &                                req_resolution,
     &                                req_bandname,
     &                                req_bandunit,
     &                                l1b_1km_lun,
     &                                datatype_1km,
     &                                interleave_1km,
     &                                resolution_1km,
     &                                error_1km,
     &                                offset_1km,
     &                                samples_1km,
     &                                lines_1km,
     &                                bands_1km,
     &                                bandnames_1km,
     &                                bandunits_1km,
     &                                ElementSize,
     &                                LineSize,
     &                                v1km_buf,
     &                                v1km_flag,
     &                                data_size(1),
     &                                data_size(2)
     &                               )

             if(status .lt.0 ) then
                call message('mod04_get_L1b',
     &           'Failed to extract 1km L1B data for band '//req_bandname
     &            ,0, 3 )
             endif

c ---------- Place the data into the 1km holding arrays
             k = 1
             do ii = 1 , data_size(2)
               do jj = 1 , data_size(1)
                 if ((v1km_flag(k) .eq. 0) .and. (v1km_buf(k).gt.0.0
     +              .and. v1km_buf(k).lt.1000.0))
     +               Rad_20(jj,ii) = v1km_buf(k)
                 k = k+1
               enddo
             enddo

C     ------------------
C     Read MODIS band 31
C     ------------------

c ... Initialize output variables
      data_size(1) = 0
      data_size(2) = 0

c ... Initialize input variables
      req_resolution = 1
      req_bandname = '31'
      req_bandunit = 'rad'
      ElementSize = Buf_Size1
      LineSize = Buf_Size2

c --------- Read a 1km scan cube-s worth of data out of the granule
            status = db_read_flat_file(
     &                                cube,
     &                                req_resolution,
     &                                req_bandname,
     &                                req_bandunit,
     &                                l1b_1km_lun,
     &                                datatype_1km,
     &                                interleave_1km,
     &                                resolution_1km,
     &                                error_1km,
     &                                offset_1km,
     &                                samples_1km,
     &                                lines_1km,
     &                                bands_1km,
     &                                bandnames_1km,
     &                                bandunits_1km,
     &                                ElementSize,
     &                                LineSize,
     &                                v1km_buf,
     &                                v1km_flag,
     &                                data_size(1),
     &                                data_size(2)
     &                               )

             if(status .lt.0 ) then
                call message('mod04_get_L1b',
     &           'Failed to extract 1km L1B data for band '//req_bandname
     &            ,0, 3 )
             endif

c ---------- Place the data into the 1km holding arrays
             k = 1
             do ii = 1 , data_size(2)
               do jj = 1 , data_size(1)
                 if ((v1km_flag(k) .eq. 0) .and. (v1km_buf(k).gt.0.0
     +              .and. v1km_buf(k).lt.1000.0))
     +               Rad_31(jj,ii) = v1km_buf(k)
                 k = k+1
               enddo
             enddo

C     ------------------
C     Read MODIS band 32
C     ------------------

c ... Initialize output variables
      data_size(1) = 0
      data_size(2) = 0

c ... Initialize input variables
      req_resolution = 1
      req_bandname = '32'
      req_bandunit = 'rad'
      ElementSize = Buf_Size1
      LineSize = Buf_Size2

c --------- Read a 1km scan cube-s worth of data out of the granule
            status = db_read_flat_file(
     &                                cube,
     &                                req_resolution,
     &                                req_bandname,
     &                                req_bandunit,
     &                                l1b_1km_lun,
     &                                datatype_1km,
     &                                interleave_1km,
     &                                resolution_1km,
     &                                error_1km,
     &                                offset_1km,
     &                                samples_1km,
     &                                lines_1km,
     &                                bands_1km,
     &                                bandnames_1km,
     &                                bandunits_1km,
     &                                ElementSize,
     &                                LineSize,
     &                                v1km_buf,
     &                                v1km_flag,
     &                                data_size(1),
     &                                data_size(2)
     &                               )

             if(status .lt.0 ) then
                call message('mod04_get_L1b',
     &           'Failed to extract 1km L1B data for band '//req_bandname
     &            ,0, 3 )
             endif

c ---------- Place the data into the 1km holding arrays
             k = 1
             do ii = 1 , data_size(2)
               do jj = 1 , data_size(1)
                 if ((v1km_flag(k) .eq. 0) .and. (v1km_buf(k).gt.0.0
     +              .and. v1km_buf(k).lt.1000.0))
     +               Rad_32(jj,ii) = v1km_buf(k)
                 k = k+1
               enddo
             enddo
C anning NUMSQ控制行循环的次数，NUMSQ=(data_size(1)/IGRIDX)*(data_size(2)/IGRIDY)
!            NUMSQ=(data_size(1)/IGRIDX)*(data_size(2)/IGRIDY)
             NUMSQ = 2048
!            print *, NUMSQ, data_size(1), IGRIDX, data_size(2), IGRIDY, 'NUMSQNUMSQNUMSQNUMSQNUMSQ'


c ------- debug statement
          if (debug .gt. 3) then
            write(h_output,'(15x,'' CONTEXT FOR 250 CUBE '',3I10)')cube,
     &              data_size(1), data_size(2)
         write(h_output,'(2x,'' vis250_band 1(.66um)'',/,40(10f10.6/))')
     &              ((Refl_1(i,j),i=696,705),j=1,40)
         write(h_output,'(2x,'' vis250_band 2(.87um)'',/,40(10f10.6/))')
     &              ((Refl_2(i,j),i=696,705),j=1,40)
      endif

          if (debug .gt. 3) then
            write(h_output,'(15x,'' CONTEXT FOR 500 CUBE '',3I10)')cube,
     &              data_size(1), data_size(2)
         write(h_output,'(2x,'' vis500_band 3(.66um)'',/,40(10f10.6/))')
     &              ((Refl_3(i,j),i=696,705),j=1,20)
         write(h_output,'(2x,'' vis250_band 7(.87um)'',/,40(10f10.6/))')
     &              ((Refl_7(i,j),i=696,705),j=1,20)
      endif

      if (debug .gt. 3) then
         write(h_output,'(15x,'' CONTEXT FOR 1km CUBE '', 3I10)') cube,
     &                   data_size(1), data_size(2)
         write(h_output,'(15x,'' Pixels 696 - 705'')')
         write(h_output,'(2x,'' rad_band 20 (3.7um)'',/,20(5f10.4/))')
     &       ((Rad_20(i,j),i=696,705),j=1,10)
         write(h_output,'(2x,'' rad_band 31 (11um)'',/,20(5f10.4/))')
     &       ((Rad_31(i,j),i=696,705),j=1,10)
         write(h_output,'(2x,'' rad_band 9 (.44um)'',/,20(5f10.4/))')
     &       ((Refl_9(i,j),i=696,705),j=1,10)
         write(h_output,'(2x,'' rad_band 13 (.65um)'',/,20(5f10.4/))')
     &       ((Refl_13(i,j),i=696,705),j=1,10)
         write(h_output,'(2x,'' rad_band 26 (1.38um)'',/,20(5f10.4/))')
     &       ((Refl_26(i,j),i=696,705),j=1,10)
      endif

c ................................................................

c --- set function status to SUCCESS
      mod04_get_L1b = 0

      return
      end
