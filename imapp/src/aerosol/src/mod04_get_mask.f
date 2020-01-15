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
      integer function mod04_get_mask(mask_lun, mask_qa_lun,
     +        datat_mask, interl_mask, resol_mask, offset_mask,
     +        sampl_mask, lines_mask, bands_mask, bnames_mask,
     +        cube, Buf_Size1, Buf_Size2, Buf_cldmsk, Buf_cldmsk_QA, 
     +        Cloud, QA_Cloud)

      implicit none
      save

C----------------------------------------------------------------------
C!F77
C
C!Description:
C     Routine for reading MODIS cloud mask and cloud mask qa binary
C     files and placing them in appropriate arrays for processing.
C
c!Input Parameters:
C    mask_lun           LUN for open Cloud Mask file
C    mask_qa_lun        LUN for open Cloud Mask QA file
C    datat_mask         Format of data in cloud mask file
C    interl_mask        Order data is saved in cloud mask file
C    resol_mask         Cloud mask file data resolution
C    offset_mask        Offset, if any, of cloud mask file data
C    sampl_mask         Number of elements in cloud mask file
C    lines_mask         Number of lines of data in cloud mask file
C    bands_mask         Number of bands in cloud mask file
C    bnames_mask        Band numbers in cloud mask file
C    cube               Scan cube number
C    Buf_Size1          Buffer size for first array index
C    Buf_Size2          Buffer size for second array index
C    Buf_cldmsk         Number of byte of cloud mask storage (=6)
C    Buf_cldmsk_QA      Number of byte of cloud mask QA storage (=10)
C
C!Output Parameters:
C    Cloud              Array containing scan of cloud mask data
C    QA_Count           Array containing scan of cloud mask QA data
C
c!Revision History:
c
c!Team-Unique Header:
c
c!References and Credits:
c See Cloud Mask ATBD-MOD-06.
c
c!END
c----------------------------------------------------------------------

      include 'db_mod04uw_debug.inc'

c ... scalar arguments
      integer datat_mask, resol_mask, offset_mask, sampl_mask,
     +        lines_mask, bands_mask, cube, mask_lun, mask_qa_lun, 
     +        Buf_cldmsk, Buf_cldmsk_QA, Buf_Size1, Buf_Size2
      character*(*) bnames_mask(*), interl_mask

c ... array arguments
      byte Cloud(Buf_cldmsk,Buf_Size1,Buf_Size2),
     +     QA_Cloud(Buf_cldmsk_QA,Buf_Size1,Buf_Size2)

c ... local scalars
      character*80 req_bandunit, req_band, req_bandname
      integer i, j, ii, jj, k, status, LineSize, ElementSize, m,
     +        req_resolution, bands_maskqa, mask_byte
      real error_mask

c ... local arrays
      character*80 bnames_maskqa(10),
     +             bandunits_mask(6), bandunits_maskqa(10)
      byte mask_flag(Buf_cldmsk*Buf_Size1*Buf_Size2),
     +     mask_buf(Buf_cldmsk*Buf_Size1*Buf_Size2),
     +     maskqa_flag(Buf_cldmsk_QA*Buf_Size1*Buf_Size2),
     +     maskqa_buf(Buf_cldmsk_QA*Buf_Size1*Buf_Size2)
      integer data_size(2)

c ... external functions
      external db_read_mask_file
      integer db_read_mask_file


      do i = 1 , Buf_Size2
         do j = 1 , Buf_Size1
            do k = 1 , Buf_cldmsk
               Cloud(k,j,i) = 0
            enddo
         enddo
      enddo
            
      do i = 1 , Buf_Size2
         do j = 1 , Buf_Size1
            do k = 1 , Buf_cldmsk_QA
               QA_Cloud(k,j,i) = 0
            enddo
         enddo
      enddo

c ... initialize output parameter
      mod04_get_mask = -1

c ... CLOUD MASK DATA ................................................
C     -----------------------------
C     Read MODIS Cloud Mask 48 bits
C     ----------------------------
      data_size(1) = 0
      data_size(2) = 0

      req_resolution = 1
      req_bandunit = '  '
      ElementSize = Buf_Size1
      LineSize = Buf_Size2
      error_mask = 0.0

      bandunits_mask(1) = '  '
      bandunits_mask(2) = '  '
      bandunits_mask(3) = '  '
      bandunits_mask(4) = '  '
      bandunits_mask(5) = '  '
      bandunits_mask(6) = '  '

      do i = 1 , Buf_cldmsk
c ...   Initialize input variables
        write(req_bandname,*) i
        req_band = 'byte ' // req_bandname(1:len(req_bandname))

        do m = 1, Buf_Size1*Buf_Size2
           mask_buf(m) = 0
           mask_flag(m) = -1
        enddo

c ------Read a 1km scan cube-s worth of data out of the granule
        status = db_read_mask_file(
     &	                           cube,	 	
     &				   req_resolution,
     &				   req_band,
     &				   req_bandunit,
     &				   mask_lun,
     &				   datat_mask,
     &				   interl_mask,
     &				   resol_mask,
     &                             error_mask,
     &                             offset_mask,
     &                             sampl_mask,
     &                             lines_mask,
     &                             bands_mask,
     &                             bnames_mask,
     &                             bandunits_mask,
     &                             ElementSize,
     &                             LineSize,
     &                             mask_buf,
     &                             mask_flag,
     &                             data_size(1),
     &                             data_size(2)
     &                            )

        if(status .lt.0 ) then
          call message('mod04_get_mask',
     &     'Failed to extract cloudmask data for band '//req_band
     &       ,0, 3 )
        endif

c ----- Place the data into the mask holding array
        k = 1
        do ii = 1 , data_size(2)
          do jj = 1 , data_size(1)
            if (mask_flag(k) .eq. 0)
     +          Cloud(i,jj,ii) = mask_buf(k)
            k = k+1
          enddo
        enddo
c ... end cloud mask acquisition loop
      enddo

c ... CLOUD MASK QA DATA ...........................................
C     -------------------------------------
C     Read MODIS Cloud Mask 10 byte QA file
C     -------------------------------------
c ... Initialize output variables
!     data_size(1) = 0
!     data_size(2) = 0
!!     bands_maskqa = Buf_cldmsk_QA
!     req_resolution = 1
!     req_bandunit = '  '
!     ElementSize = Buf_Size1
!     LineSize = Buf_Size2
!     error_mask = 0.0
!     bnames_maskqa(1) = 'byte 1'
!     bnames_maskqa(2) = 'byte 2'
!     bnames_maskqa(3) = 'byte 3'
!     bnames_maskqa(4) = 'byte 4'
!     bnames_maskqa(5) = 'byte 5'
!     bnames_maskqa(6) = 'byte 6'
!     bnames_maskqa(7) = 'byte 7'
!     bnames_maskqa(8) = 'byte 8'
!     bnames_maskqa(9) = 'byte 9'
!     bnames_maskqa(10) = 'byte 10'
!!     bandunits_maskqa(1) = '  '
!     bandunits_maskqa(2) = '  '
!     bandunits_maskqa(3) = '  '
!     bandunits_maskqa(4) = '  '
!     bandunits_maskqa(5) = '  '
!     bandunits_maskqa(6) = '  '
!     bandunits_maskqa(7) = '  '
!     bandunits_maskqa(8) = '  '
!     bandunits_maskqa(9) = '  '
!     bandunits_maskqa(10) = '  '
!!     do i = 1 , Buf_cldmsk_QA
!       write(req_bandname,*) i
!       req_band = 'byte ' // req_bandname(1:len(req_bandname))
!!       do m = 1, Buf_Size1*Buf_Size2
!          maskqa_buf(m) = 0
!          maskqa_flag(m) = -1
!       enddo
!! ------Read a 1km scan cube-s worth of data out of the granule
!       status = db_read_mask_file(
!    &	                           cube,
!    &				   req_resolution,
!    &				   req_band,
!    &				   req_bandunit,
!    &				   mask_qa_lun,
!    &				   datat_mask,
!    &				   interl_mask,
!    &				   resol_mask,
!    &                             error_mask,
!    &                             offset_mask,
!    &                             sampl_mask,
!    &                             lines_mask,
!    &                             bands_maskqa,
!    &                             bnames_maskqa,
!    &                             bandunits_maskqa,
!    &                             ElementSize,
!    &                             LineSize,
!    &                             maskqa_buf,
!    &                             maskqa_flag,
!    &                             data_size(1),
!    &                             data_size(2)
!    &                            )
!!       if(status .lt.0 ) then
!         call message('mod04_get_mask',
!    &     'Failed to extract cloudmask QA data for band '//req_band
!    &       ,0, 3 )
!       endif
!! ----- Place the data into the mask holding array
!       k = 1
!       do ii = 1 , data_size(2)
!         do jj = 1 , data_size(1)
!           if (maskqa_flag(k) .eq. 0)
!    +          QA_Cloud(i,jj,ii) = maskqa_buf(k)
!           k = k+1
!         enddo
!       enddo
! ... end cloud mask acquisition loop
!     enddo

c ------------------------------------------------------------------

c ... debug statement ............................................
      if( debug.gt.1) then
        write(h_output,'(15x,'' MASK VALUES: '',2i10)') data_size(1),
     *        data_size(2)
        write(h_output,'(2x,'' Cloud Mask'',/,20(5I10/))')
     +       ((Cloud(1,j,k),j=1,10),k=1,10)
        write(h_output,'(2x,'' Cloud Mask QA'',/,20(5I10/))')
     +       ((QA_Cloud(1,j,k),j=1,10),k=1,10)
      endif
c ................................................................

c --- set function status to SUCCESS
      mod04_get_mask= 0

      return
      end
