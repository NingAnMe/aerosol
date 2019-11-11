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
      subroutine db_mod04_get_rp(  
     &                           cfgname,
     &                           rp_debug,
     &                           l1b_1km_hdr,
     &                           l1b_hkm_hdr,
     &                           l1b_qkm_hdr,
     &                           l1b_geo_hdr,
     &                           mask_hdr,
     &                           mask_qa_hdr,
     &                           beg_lin,
     &                           nlins,
     &                           beg_ele,
     &                           neles
     &                           )

      implicit none
      save

c!F77 ************************************************************
c ...
c!Description:
c     Routine which gets the runtime debug parameters and 
c      and data header file names for the MODIS MOD04 aerosol direct
c      broadcast product.
c      debug values: 0 = no debug writes
c                    1-4 = levels of debug output, from little to reams
c
c!Input parameters:
c None
c
c!Output Parameters:
c cfgname      Configuration file name
c rp_debug     Debug value - 0 or 1
c l1b_1km_hdr  Header file name for 1km data
c l1b_hkm_hdr  Header file name for 500m data
c l1b_qkm_hdr  Header file name for 250m data
c l1b_geo_hdr  Header file name for geolocation data
c mask_hdr     Header file name for cloud mask data
c mask_qa_hdr  Header file name for cloud mask QA data
c beg_lin      Beginning line number to process
c nlins        Number of lines to process
c beg_ele      Beginning element number to process
c neles        Number of elements to process
c
c!Revision History:
c
c!Team-Unique Header:
c    Developed by the MODIS Group, CIMSS/SSEC, UW-Madison.
c
c!References and Credits:
c
c!Design Notes:
c
c!END****************************************************************

c --- constants
      character*80 routine_name
      parameter (routine_name =  'db_mod04_get_rp')

c --- parameters
      character*(*) l1b_1km_hdr, l1b_geo_hdr, mask_hdr, mask_qa_hdr,
     &              cfgname, l1b_hkm_hdr, l1b_qkm_hdr
      integer rp_debug, beg_lin, nlins, beg_ele, neles

c --- external functions
      integer hdrgetkeyint, hdrgetkeystr

c --- internal variables
      character*80  keyname
      character*255 filename
      integer keyindex, status, len_name
      logical remove_all

c --- initialize function status
      remove_all = .FALSE.

c ... initialize filename
      filename = ' '

c --- get debug value
      keyname = 'debug'
      keyindex = 1
      status = hdrgetkeyint( cfgname, keyname, keyindex, rp_debug )
      if( status.lt.0 ) then
           write(*,*)'Could not get rp_debug', rp_debug
           rp_debug = 0
      endif

c --- get the name of the L1B geo header file
      keyname = 'GEO_1KM_HDR'
      keyindex = 1
      status = hdrgetkeystr( cfgname, keyname, keyindex, filename )
      if( status.lt.0 ) then
         call message( routine_name,
     &                 'FAILED: No L1B Geo header file name',
     &                 0, 2 )
         return
      else
         call strcompress( filename, remove_all, len_name )
         l1b_geo_hdr = filename(1:len_name)
      endif

c --- get the name of the L1B 1km header file
      keyname = 'L1B_1KM_HDR'
      keyindex = 1
      status = hdrgetkeystr( cfgname, keyname, keyindex, filename )
      if( status.lt.0 ) then
         call message( routine_name,
     &                 'FAILED: No L1B 1KM header file name',
     &                 0, 2 )
         return
      else
         call strcompress( filename, remove_all, len_name )
         l1b_1km_hdr = filename(1:len_name)
      endif

c --- Retrieve beginning line number
      keyname = 'bline'
      keyindex = 1
      status=hdrgetkeyint(filename(1:len_name),keyname,keyindex,beg_lin)
      if( status.lt.0 ) beg_lin = 0
      if( beg_lin.lt.0 ) then
         call message( routine_name,
     &                 'FAILED: invalid beginning 1km line number =',
     &                 beg_lin, 2 )
         return
      endif

c --- Retrieve number of lines to process
      keyname = 'lines'
      keyindex = 1
      status=hdrgetkeyint(filename(1:len_name),keyname,keyindex,nlins)
      if( status.lt.0 ) then
         call message( routine_name,
     &                 'FAILED: invalid number of 1km lines =',
     &                 nlins, 2 )
         return
      endif

c --- Retrieve beginning element number
      keyname = 'belem'
      keyindex = 1
      status=hdrgetkeyint(filename(1:len_name),keyname,keyindex,beg_ele)
      if( status.lt.0 ) beg_ele = 0
      if( beg_ele.lt.0 .or. beg_ele.gt.2048) then
         call message( routine_name,
     &                 'FAILED: invalid beginning 1km element = ',
     &                 beg_ele, 2 )
         return
      endif

c --- Retrieve number of elements to process
      keyname = 'samples'
      keyindex = 1
      status=hdrgetkeyint(filename(1:len_name),keyname,keyindex,neles)
      if( status.lt.0 ) neles = -1
      if( neles.lt.0 .or. neles.gt.2048) then
         call message( routine_name,
     &                 'FAILED: invalid number of samples/line = ',
     &                  neles, 2 )
         return
      endif

c --- get the name of the L1B 500m header file
      keyname = 'L1B_500M_HDR'
      keyindex = 1
      status = hdrgetkeystr( cfgname, keyname, keyindex, filename )
      if( status.lt.0 ) then
         call message( routine_name,
     &                 'FAILED: No L1B 500m header file name',
     &                 0, 2 )
         return
      else
         call strcompress( filename, remove_all, len_name )
         l1b_hkm_hdr = filename(1:len_name)
      endif

c --- get the name of the L1B 1km header file
      keyname = 'L1B_250M_HDR'
      keyindex = 1
      status = hdrgetkeystr( cfgname, keyname, keyindex, filename )
      if( status.lt.0 ) then
         call message( routine_name,
     &                 'FAILED: No L1B 250m header file name',
     &                 0, 2 )
         return
      else
         call strcompress( filename, remove_all, len_name )
         l1b_qkm_hdr = filename(1:len_name)
      endif

c --- get the name of the Cloud mask input header file
      keyname = 'MOD35_MASK_HDR'
      keyindex = 1
      status = hdrgetkeystr( cfgname, keyname, keyindex, filename )
      if( status.lt.0 ) then
         call message( routine_name,
     &                 'FAILED: No Cloud Mask header file name',
     &                 0, 3 )
         return
      else
         call strcompress( filename, remove_all, len_name )
         mask_hdr = filename(1:len_name)
      endif

c --- get the name of the Cloud mask QA input header file
      keyname = 'MOD35_QA_HDR'
      keyindex = 1
      status = hdrgetkeystr( cfgname, keyname, keyindex, filename )
      if( status.lt.0 ) then
         call message( routine_name,
     &                 'FAILED: No Cloud Mask QA header file name',
     &                 0, 3 )
         return
      else
         call strcompress( filename, remove_all, len_name )
         mask_qa_hdr = filename(1:len_name)
      endif

      return
      end
