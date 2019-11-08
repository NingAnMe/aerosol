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
      subroutine db_mod04_chk_input(nscans,npixels,beg_lin,nlins,
     +                              beg_ele,neles,beg_scan,ibes,
     +                              out_lines_10km,out_elements_10km)

      implicit none
      save

c ... Include files 
      include 'db_mod04uw_debug.inc'
      include 'COMMONS.inc'

c     Satellite identifier from L1b file.
      include 'platform_name.inc'

c-------------------------------------------------------------------
C!F77 
C 
C!DESCRIPTION:
C     Routine for checking consistency of input parameters.  
C
c!Input Parameters:
c nscans        Number of scans for this granule
c npixels       Number of pixels in this granule
c beg_lin       Beginning line number to process
c nlins         Number of lines to process
c beg_ele       Beginning element number to process
c neles         Number of elements to process
c
c!Output Parameters:
c beg_scan          Beginning scan number to process baeed upon
c                    user inputs
c nscans            Updated Number of scans to process based upon
c                     user inputs
c npixels           Updated number of pixesl to process based upon
c                     user inputs
c ibes              Beginning pixel number to process
c out_lines_10km    Number of 10 km lines in this granule
c out_elements_10km  Number of 10 km pixels in this granule
c
c!Revision History:
c
c!Team-unique Header:
c
c!References and Credits:
c See Cloud Mask ATBD-MOD-06.
c Externals:  error messaging routine   -  message.f
c
c!END
c-------------------------------------------------------------------

c     scalar arguments
      integer nscans,beg_lin,nlins,beg_ele,neles,beg_scan,ibes,
     +        out_lines_10km,out_elements_10km,npixels

c     local scalars
      integer out_lines_1km,out_elements_1km,num_lines,num_eles
    
c     external subroutines
      external message


c ... Define processing intervals
c ... nscans will be number of scans to process
c ... ibes is beginning pixel number, npixels will be number of pixesl
c ...  to process.  
      beg_scan = (beg_lin - 1) / No_Lines_Per_Scan + 1
      out_lines_1km = nscans * No_Lines_Per_Scan
      out_elements_1km = npixels
      num_lines = beg_lin + nlins - 1
      num_eles = beg_ele + neles - 1
      out_lines_10km = out_lines_1km / 10
      out_elements_10km = out_elements_1km  / 10

      if (num_lines .gt. out_lines_1km .or. num_eles .gt. npixels)
     +  call message( 'mod06_chk_input',
     +    'Error: Number of lines or elements to process was .gt. max'
     +    // char(10) // ' Check runtime parameters '
     +    // 'which define processing interval. ', 0, 2 )

      if (beg_lin .eq. 0 .and. beg_ele .eq. 0) then
          ibes = 1
          beg_scan = 1
      else
          ibes = beg_ele
          nscans = (num_lines -1) / No_Lines_Per_Scan + 1
          npixels = neles + ibes - 1
c ...     Check if at least one 5x5 pixel can be processed
          if (nlins .lt. 5 .or. neles .lt. 5)
     +    call message( 'mod06_chk_input',
     +     'Error: Number of lines or elements to process was lt 5'
     +     // char(10) // ' Check runtime parameters '
     +     // 'which define processing interval. ', 0, 2 )
      endif

      if(nscans .le. 0 .or. npixels .le. 0 .or. ibes .le. 0 .or.
     &   ibes .le. 0) then
        call message( 'mod06_chk_input',
     &  'Error: Number of lines or elements to process was .le. 0.'
     +    // char(10) // ' Check runtime parameters '
     +    // 'which define processing interval. ',  0, 2 )
      endif

c ....................................................................
      if(debug .gt. 0 .and. beg_lin .eq. 0) then
         write(h_output,fmt=
     + '(1x,''  Number of scans to Process '',i10, '' and elements '',
     +          i10,/)') nscans, neles
      elseif (debug .gt. 0 .and. beg_lin .ne. 0) then
         write(h_output,fmt=
     + '(1x,''  Number of scans to Process '',i10, '' and elements '',
     +          i10,/)') nscans-beg_scan+1 , neles
      endif
c ....................................................................

      return
      end
