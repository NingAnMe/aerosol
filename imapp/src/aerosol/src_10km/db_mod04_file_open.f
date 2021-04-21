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
      subroutine db_mod04_file_open( 
     &                                filename,
     &                                choice_flag,
     &                                l1b_1km_neles,
     &                                l1b_1km_lun,
     &                                l1b_hkm_lun,
     &                                l1b_qkm_lun,
     &                                geo_1km_lun,
     &                                mask_lun,
     &                                mask_qa_lun,
     &                                scan_1km_lun,
     &                                anc_met_lun,
     &                                ozone_lun,
     &                                handle_LUT466,
     &                                handle_LUT553,
     &                                handle_LUT644, 
     &                                handle_LUT213, 
     &                                handle_LUTMAP,
     &                                handle_INSCI,
     &                                handle_S,
     &                                handle_L,
     &                                mod04_lun,
     &                                hdr_lun,
     &                                debug_lun,
     &                                RTN_NCEP
     &                              )
      implicit none
      save

      include 'aerosol.inc'
      include 'db_mod04uw_debug.inc'

C-----------------------------------------------------------------------
C!F77
C
C!DESCRIPTION:
c Program which will open files and return unit numbers for 
c all files used in the DB Atmospheric Profiles processing.
C
C!INPUT PARAMETERS:    None.
C
C!OUTPUT PARAMETERS:
C
C   filename           Configuration file name
C   choice_flag        Definition of which mode to run (currently MA)
C   l1b_1km_neles      Number of elements per scan line of data
C   l1b_1km_lun        LUN for open 1km L1B data file
C   l1b_hkm_lun        LUN for open half km L1B data file
C   l1b_qkm_lun        LUN for open quarter km L1B data file
C   geo_1km_lun        LUN for open geo file
C   mask_lun           LUN for open Cloud Mask file
C   mask_qa_lun        LUN for open Cloud Mask QA file
C   scan_1km_lun       LUN for scan based metadata file
C   anc_met_lun        LUN for open met data file
C   ozone_lun          LUN for ozone ancillary data file
C   handle_LUT466      LUN for look up table 466 coefficient file
C   handle_LUT553      LUN for look up table 553 coefficient file
C   handle_LUT644      LUN for look up table 644 coefficient file
C   handle_LUT213      LUN for look up table 213 coefficient file
C   handle_LUTMAP      LUN for aerosol map look up table
C   handle_INSCI       LUN for ASCII input ocean look up table.
C   handle_S           LUN's for small mode sea coeffiecient files
C   handle_L           LUN's for large mode sea coeffiecient files
C   mod04_lun          LUN for MODIS Aerosol binary product output file
C   hdr_lun            LUN for MODIS Aerosol header output file
C   debug_lun          LUN for text debug output file
C   RTN_NCEP           Status for open GDAS file 0=opened
C                                                1=failed
C
C!REVISION HISTORY:
c
C!TEAM-UNIQUE HEADER:
C
C!REFERENCES AND CREDITS
C
C EXTERNALS:
C
C       NAMED CONSTANTS:
C
C       FUNCTIONS AND SUBROUTINES:
C               hdrgetkeystr.f
c               message.f
C COMMENTS:
C     ALL OF THE UNIT NUMBERS AND ACCESS INFORMATION IS STORED WITHIN
C     THE aerosol.inc FILE.
C
C!END-------------------------------------------------------------------

c --- constants
      character*18 FUNCTION
      parameter (FUNCTION = 'db_mod04_file_open')

c --- arguments
      integer handle_S(3), handle_L(3)
      integer l1b_1km_neles, l1b_1km_lun, geo_1km_lun, scan_1km_lun, 
     +        anc_met_lun, mask_lun, ozone_lun, mod04_lun, 
     +        hdr_lun, l1b_qkm_lun, l1b_hkm_lun, mask_qa_lun, debug_lun,
     +        handle_LUT466, handle_LUT644, handle_LUT553,handle_LUT213,
     +        handle_LUTMAP,  handle_INSCI, RTN_NCEP
      character*(*) filename, choice_flag

c --- external functions
      integer hdrgetkeystr
      external hdrgetkeystr

c --- internal variables
      character*12 req
      character*80 keyname
      character*255 file
      integer len_file, keyindex, status, io_err, level, len, i
      logical  remove_all

c --- initialize the lun
      l1b_1km_lun  = -1
      l1b_qkm_lun  = -1
      l1b_hkm_lun  = -1
      geo_1km_lun  = -1
      mask_lun	   = -1
      scan_1km_lun = -1
      anc_met_lun  = -1
      debug_lun	= -1
      hdr_lun	= -1
      mod04_lun = -1
      ozone_lun = -1
c ... coefficient file LUN's
      handle_LUT466 = -1
      handle_LUT553 = -1
      handle_LUT644 = -1
      handle_LUT213 = -1
      handle_LUTMAP = -1
      handle_INSCI = -1

      do i = 1, 3
         handle_S(i) = -1
         handle_L(i) = -1
      end do

      remove_all = .true.

c --- initialize dummy variables
      req = ' '
      file = ' '

c ... input check
      if (choice_flag.ne.'MA' .and. choice_flag.ne.'ML' .and.
     &    choice_flag.ne.'MS' .and. choice_flag.ne.'NS' .and.
     &    choice_flag.ne.'NL') then
          call message(FUNCTION,
     &         'Invalid input for choice_flag',0, 3)
          return
      end if

c------------------------------------------------------------
c ... Open MODIS MOD_PR04 Land Lookup table files:
c------------------------------------------------------------

      if (choice_flag.eq.'MA' .or. choice_flag.eq.'ML') then

c ...     Open and check the band .466 coefficient file
          file = '  '
          keyname = 'LUT466_NAME'
          keyindex = 1
          status = hdrgetkeystr( filename, keyname, keyindex, file )
          if( status.lt.0 ) then
            call message(FUNCTION,'Failed reading lut466 coef file',
     &                 0, 2)
          else
            handle_LUT466 = LUT466_UNIT
            open(FILE=file,UNIT=handle_LUT466,STATUS=LUT466_STATUS,
     &           FORM=LUT466_FORM,ACCESS=LUT466_ACCESS,
     &           IOSTAT=io_err)
            if (io_err .ne. 0) then
             call message(FUNCTION,'Failed opening lut466 coef file',
     &                 0, 2)
            endif
          endif

c ...     Open and check the band .553 coefficient file
          file = '  '
          keyname = 'LUT553_NAME'
          keyindex = 1
          status = hdrgetkeystr( filename, keyname, keyindex, file )
          if( status.lt.0 ) then
            call message(FUNCTION,'Failed reading lut553 coef file',
     &                 0, 2)
          else
            handle_LUT553 = LUT553_UNIT
            open(FILE=file,UNIT=handle_LUT553,STATUS=LUT553_STATUS,
     &           FORM=LUT553_FORM,ACCESS=LUT553_ACCESS,
     &           IOSTAT=io_err)
            if (io_err .ne. 0) then
             call message(FUNCTION,'Failed opening lut553 coef file',
     &                 0, 2)
            endif
          endif

c ...     Open and check the band .644 coefficient file
          file = '  '
          keyname = 'LUT644_NAME'
          keyindex = 1
          status = hdrgetkeystr( filename, keyname, keyindex, file )
          if( status.lt.0 ) then
            call message(FUNCTION,'Failed reading lut644 coef file',
     &                 0, 2)
          else
            handle_LUT644 = LUT644_UNIT
            open(FILE=file,UNIT=handle_LUT644,STATUS=LUT644_STATUS,
     &           FORM=LUT644_FORM,ACCESS=LUT644_ACCESS,
     &           IOSTAT=io_err)
            if (io_err .ne. 0) then
             call message(FUNCTION,'Failed opening lut644 coef file',
     &                 0, 2)
            endif
          endif

c ...     Open and check the band 2.13 coefficient file
          file = '  '
          keyname = 'LUT213_NAME'
          keyindex = 1
          status = hdrgetkeystr( filename, keyname, keyindex, file )
          if( status.lt.0 ) then
            call message(FUNCTION,'Failed reading lut213 coef file',
     &                 0, 2)
          else
            handle_LUT213 = LUT213_UNIT
            open(FILE=file,UNIT=handle_LUT213,STATUS=LUT213_STATUS,
     &           FORM=LUT213_FORM,ACCESS=LUT213_ACCESS,
     &           IOSTAT=io_err)
            if (io_err .ne. 0) then
             call message(FUNCTION,'Failed opening lut213 coef file',
     &                 0, 2)
            endif
          endif

c ...     Open and check the aerosol map LUT file
          file = '  '
          keyname = 'LUTMAP_NAME'
          keyindex = 1
          status = hdrgetkeystr( filename, keyname, keyindex, file )
          if( status.lt.0 ) then
            call message(FUNCTION,'Failed reading lutmap coef file',
     &                 0, 2)
          else
            handle_LUTMAP = LUTMAP_UNIT
            open(FILE=file,UNIT=handle_LUTMAP,STATUS=LUTMAP_STATUS,
     &           FORM=LUTMAP_FORM,ACCESS=LUTMAP_ACCESS,
     &           IOSTAT=io_err)
            if (io_err .ne. 0) then
             call message(FUNCTION,'Failed opening lutmap coef file',
     &                 0, 2)
            endif
          endif
      endif

c------------------------------------------------------------
c ... Open non-MODIS files:
c ... MOD_PR04 land :'modaerland.input' and 'modaerland.output'
c ... MOD_PR04 sea :'modaeroceanv2.input' and 'modaeroceanv2.output'
c------------------------------------------------------------
      if (choice_flag(1:1).eq.'N') then
c ...     Open and check the INPUT Science coefficient file
          file = '  '
          keyname = 'INSCI_NAME'
          keyindex = 1
          status = hdrgetkeystr( filename, keyname, keyindex, file )
          if( status.lt.0 ) then
            call message(FUNCTION,'Failed reading insci coef file',
     &                 0, 2)
          else
            handle_LUTMAP = INSCI_UNIT
            open(FILE=file,UNIT=handle_INSCI,STATUS=INSCI_STATUS,
     &           FORM=INSCI_FORM,ACCESS=INSCI_ACCESS,
     &           IOSTAT=io_err)
            if (io_err .ne. 0) then
             call message(FUNCTION,'Failed opening insci coef file',
     &                 0, 2)
            endif
          endif
      endif

c ------------------------------------------------------------
c ... Open MODIS MOD_PR04 Sea files:
c ... 'modaeroceanv0.ext','smallcase1-6', and 'largecase1-6'.
c ------------------------------------------------------------

      if (choice_flag.eq.'MA' .or. choice_flag.eq.'MS') then

c ...     Open and check the coefficient files
          file = '  '
          keyname = 'SMALL1_NAME'
          keyindex = 1
          status = hdrgetkeystr( filename, keyname, keyindex, file )
          if( status.lt.0 ) then
            call message(FUNCTION,'Failed reading small1 coef file',
     &                 0, 2)
          else
            handle_S(1) = SMALL1_UNIT
            open(FILE=file,UNIT=handle_S(1),STATUS=SMALL1_STATUS,
     &           FORM=SMALL1_FORM,ACCESS=SMALL1_ACCESS,
     &           IOSTAT=io_err)
            if (io_err .ne. 0) then
             call message(FUNCTION,'Failed opening small1 coef file',
     &                 0, 2)
            endif
          endif

          file = '  '
          keyname = 'SMALL2_NAME'
          keyindex = 1
          status = hdrgetkeystr( filename, keyname, keyindex, file )
          if( status.lt.0 ) then
            call message(FUNCTION,'Failed reading small2 coef file',
     &                 0, 2)
          else
            handle_S(2) = SMALL2_UNIT
            open(FILE=file,UNIT=handle_S(2),STATUS=SMALL2_STATUS,
     &           FORM=SMALL2_FORM,ACCESS=SMALL2_ACCESS,
     &           IOSTAT=io_err)
            if (io_err .ne. 0) then
             call message(FUNCTION,'Failed opening small2 coef file',
     &                 0, 2)
            endif
          endif

          file = '  '
          keyname = 'SMALL3_NAME'
          keyindex = 1
          status = hdrgetkeystr( filename, keyname, keyindex, file )
          if( status.lt.0 ) then
            call message(FUNCTION,'Failed reading small3 coef file',
     &                 0, 2)
          else
            handle_S(3) = SMALL3_UNIT
            open(FILE=file,UNIT=handle_S(3),STATUS=SMALL3_STATUS,
     &           FORM=SMALL3_FORM,ACCESS=SMALL3_ACCESS,
     &           IOSTAT=io_err)
            if (io_err .ne. 0) then
             call message(FUNCTION,'Failed opening small3 coef file',
     &                 0, 2)
            endif
          endif

          file = '  '
          keyname = 'BIG1_NAME'
          keyindex = 1
          status = hdrgetkeystr( filename, keyname, keyindex, file )
          if( status.lt.0 ) then
            call message(FUNCTION,'Failed reading big1 coef file',
     &                 0, 2)
          else
            handle_L(1) = BIG1_UNIT
            open(FILE=file,UNIT=handle_L(1),STATUS=BIG1_STATUS,
     &           FORM=BIG1_FORM,ACCESS=BIG1_ACCESS,
     &           IOSTAT=io_err)
            if (io_err .ne. 0) then
             call message(FUNCTION,'Failed opening big1 coef file',
     &                 0, 2)
            endif
          endif

          file = '  '
          keyname = 'BIG2_NAME'
          keyindex = 1
          status = hdrgetkeystr( filename, keyname, keyindex, file )
          if( status.lt.0 ) then
            call message(FUNCTION,'Failed reading big2 coef file',
     &                 0, 2)
          else
            handle_L(2) = BIG2_UNIT
            open(FILE=file,UNIT=handle_L(2),STATUS=BIG2_STATUS,
     &           FORM=BIG2_FORM,ACCESS=BIG2_ACCESS,
     &           IOSTAT=io_err)
            if (io_err .ne. 0) then
             call message(FUNCTION,'Failed opening big2 coef file',
     &                 0, 2)
            endif
          endif

          file = '  '
          keyname = 'BIG3_NAME'
          keyindex = 1
          status = hdrgetkeystr( filename, keyname, keyindex, file )
          if( status.lt.0 ) then
            call message(FUNCTION,'Failed reading big3 coef file',
     &                 0, 2)
          else
            handle_L(3) = BIG3_UNIT
            open(FILE=file,UNIT=handle_L(3),STATUS=BIG3_STATUS,
     &           FORM=BIG3_FORM,ACCESS=BIG3_ACCESS,
     &           IOSTAT=io_err)
            if (io_err .ne. 0) then
             call message(FUNCTION,'Failed opening big3 coef file',
     &                 0, 2)
            endif
          endif

      end if

C 250 m Level 1B MODIS DATA FILES

c --- get the file requirement
      keyname = 'L1B_250M_REQ'
      keyindex = 1
      status = hdrgetkeystr( filename, keyname, keyindex, req )
      if( status.lt.0 ) req = 'OPTIONAL'
      level = 3
      if( req(1:3).eq.'MAN' ) level = 2

c --- get file access information 
      keyname = 'L1B_250M_NAME'
      keyindex = 1
      status = hdrgetkeystr( filename, keyname, keyindex, file )

c --- check for error reading info file
      if( status.lt.0 ) then 
         call message( 
     &                 FUNCTION,
     &                'FAILED: Read of 250 m file info . ',
     &                 0, level 
     &               )

      else
                   
c ------ issue the OPEN request
         OPEN( 
     &         FILE=file,
     &         UNIT=L1B_250M_UNIT,
     &         FORM=L1B_250M_FORM,
     &         ACCESS=L1B_250M_ACCESS,
     &         STATUS=L1B_250M_STATUS,
     &         RECL=l1b_1km_neles*4*4,
     &         IOSTAT=io_err, 
     &         ERR=1000
     &       ) 

         call message(
     &                 '--',
     &                'Using 250 m L1B file: '// file ,
     &                 0, 4
     &               )

         l1b_qkm_lun = L1B_250M_UNIT
         goto 100

c ------ Failed to open 
1000     continue 
         call message( 
     &                 FUNCTION,
     &                'FAILED: Could not open 250 m L1B file. ', 
     &                 0, level 
     &               )

      endif
100   continue
      if( debug.ne.0 ) then
         call strcompress( file, remove_all, len_file )
         write(*,*) '  L1B 250 m file: ',file(1:len_file)
      endif
              
C 500m Level 1B MODIS DATA FILES

c --- get the file requirement
      keyname = 'L1B_500M_REQ'
      keyindex = 1
      status = hdrgetkeystr( filename, keyname, keyindex, req )
      if( status.lt.0 ) req = 'OPTIONAL'
      level = 3
      if( req(1:3).eq.'MAN' ) level = 2

c --- get file access information 
      keyname = 'L1B_500M_NAME'
      keyindex = 1
      status = hdrgetkeystr( filename, keyname, keyindex, file )

c --- check for error reading info file
      if( status.lt.0 ) then 
         call message( 
     &                 FUNCTION,
     &                'FAILED: Read of 500 m file info . ',
     &                 0, level 
     &               )

      else
                   
c ------ issue the OPEN request
         OPEN( 
     &         FILE=file,
     &         UNIT=L1B_500M_UNIT,
     &         FORM=L1B_500M_FORM,
     &         ACCESS=L1B_500M_ACCESS,
     &         STATUS=L1B_500M_STATUS,
     &         RECL=l1b_1km_neles*4*2,
     &         IOSTAT=io_err, 
     &         ERR=1025
     &       ) 

         call message(
     &                 '--',
     &                'Using 500 m L1B file: '// file ,
     &                 0, 4
     &               )

         l1b_hkm_lun = L1B_500M_UNIT
         goto 125

c ------ Failed to open 
1025     continue 
         call message( 
     &                 FUNCTION,
     &                'FAILED: Could not open 500 m L1B file. ', 
     &                 0, level 
     &               )

      endif
125   continue
      if( debug.ne.0 ) then
         call strcompress( file, remove_all, len_file )
         write(*,*) '  L1B 500 m file: ',file(1:len_file)
      endif
              

C 1KM Level 1B MODIS DATA FILES

c --- get the file requirement
      keyname = 'L1B_1KM_REQ'
      keyindex = 1
      status = hdrgetkeystr( filename, keyname, keyindex, req )
      if( status.lt.0 ) req = 'OPTIONAL'
      level = 3
      if( req(1:3).eq.'MAN' ) level = 2

c --- get file access information 
      keyname = 'L1B_1KM_NAME'
      keyindex = 1
      status = hdrgetkeystr( filename, keyname, keyindex, file )

c --- check for error reading info file
      if( status.lt.0 ) then 
         call message( 
     &                 FUNCTION,
     &                'FAILED: Read of 1km file info . ',
     &                 0, level 
     &               )

      else
                   
c ------ issue the OPEN request
         OPEN( 
     &         FILE=file,
     &         UNIT=L1B_1KM_UNIT,
     &         FORM=L1B_1KM_FORM,
     &         ACCESS=L1B_1KM_ACCESS,
     &         STATUS=L1B_1KM_STATUS,
     &         RECL=l1b_1km_neles*4,
     &         IOSTAT=io_err, 
     &         ERR=1050
     &       ) 

         call message(
     &                 '--',
     &                'Using 1km L1B file: '// file ,
     &                 0, 4
     &               )

         l1b_1km_lun = L1B_1KM_UNIT
         goto 150

c ------ Failed to open 
1050     continue 
         call message( 
     &                 FUNCTION,
     &                'FAILED: Could not open 1km L1B file. ', 
     &                 0, level 
     &               )

      endif
150   continue
      if( debug.ne.0 ) then
         call strcompress( file, remove_all, len_file )
         write(*,*) '  L1B 1km file: ',file(1:len_file)
      endif
              


C GEOLOCATION FILES: LATITUDE, LONGITUDE, HEIGHT, ...

c --- get the file requirement
      keyname = 'GEO_1KM_REQ'
      keyindex = 1
      status = hdrgetkeystr( filename, keyname, keyindex, req )
      if( status.lt.0 ) req = 'OPTIONAL'
      level = 3
      if( req(1:3).eq.'MAN' ) level = 2

c --- get file access information 
      keyname = 'GEO_1KM_NAME'
      keyindex = 1
      status = hdrgetkeystr( filename, keyname, keyindex, file )

c --- check for error reading info file
      if( status.lt.0 ) then 
         call message( 
     &                 FUNCTION,
     &                'FAILED: Read of Geo file info . ',
     &                 0, level 
     &               )

      else
                   
c ------ issue the OPEN request
         OPEN( 
     &         FILE=file,
     &         UNIT=GEO_1KM_UNIT,
     &         STATUS=GEO_1KM_STATUS,
     &         FORM=GEO_1KM_FORM,
     &         ACCESS=GEO_1KM_ACCESS,
     &         RECL=l1b_1km_neles*4,
     &         IOSTAT=io_err, 
     &         ERR=1075
     &       ) 

         call message(
     &                 '--',
     &                'Using 1km GEO file: '// file ,
     &                 0, 4
     &               )

         geo_1km_lun = GEO_1KM_UNIT
         goto 175

c ------ Failed to open 
1075     continue 
         call message( 
     &                 FUNCTION,
     &                'FAILED: Could not open GEO file. ',
     &                 0, level 
     &               )

      endif
175   continue
      if( debug.ne.0 ) then
         call strcompress( file, remove_all, len_file )
         write(*,*) '  GEO file: ',file(1:len_file)
      endif
              


C SCAN METADATA FILE: TYPE, MIRROR SIDE

c --- get the file requirement
      keyname = 'SCAN_1KM_REQ'
      keyindex = 1
      status = hdrgetkeystr( filename, keyname, keyindex, req )
      if( status.lt.0 ) req = 'OPTIONAL'
      level = 3
      if( req(1:3).eq.'MAN' ) level = 2

c --- get file access information 
      keyname = 'SCAN_1KM_NAME'
      keyindex = 1
      status = hdrgetkeystr( filename, keyname, keyindex, file )

c --- check for error reading info file
      if( status.lt.0 ) then 
         call message( 
     &                 FUNCTION,
     &                'FAILED: Read of Scan Metadata file info . ',
     &                 0, level 
     &               )

      else

c ------ issue the OPEN request
         OPEN( 
     &         FILE=file,
     &         UNIT=SCAN_1KM_UNIT,
     &         STATUS=SCAN_1KM_STATUS,
     &         FORM=SCAN_1KM_FORM,
     &         ACCESS=SCAN_1KM_ACCESS,
     &         RECL=SCAN_1KM_RECL,
     &         IOSTAT=io_err, 
     &         ERR=2000
     &       ) 

         call message(
     &                 '--',
     &                'Using Meta file: '// file ,
     &                 0, 4
     &               )

         scan_1km_lun = SCAN_1KM_UNIT
         goto 200

c ------ Failed to open 
2000     continue 
         call message( 
     &                 FUNCTION,
     &                'FAILED: Could not open Scan Metadata file. ',
     &                 0, level 
     &               )

      endif
200   continue
      if( debug.ne.0 ) then
         call strcompress( file, remove_all, len_file )
         write(*,*) '  Scan Metadata file: ',file(1:len_file)
      endif
      



C ANCILLARY FILES: MET

c --- get the file requirement
      keyname = 'ANC_MET_REQ'
      keyindex = 1
      status = hdrgetkeystr( filename, keyname, keyindex, req )
      if( status.lt.0 ) req = 'OPTIONAL'
      level = 3
      if( req(1:3).eq.'MAN' ) level = 2

c --- get file access information 
      keyname = 'ANC_MET_NAME'
      keyindex = 1
      status = hdrgetkeystr( filename, keyname, keyindex,file )

c --- check for error reading info file
      if( status.lt.0 ) then 
         call message( 
     &                 FUNCTION,
     &                'FAILED: Read of Met Obs file info. Will not' //
     &                       ' use the file for processing.',
     &                 0, level 
     &               )

      else
                   
c ------ issue the OPEN request
         OPEN( 
     &         FILE=file,
     &         UNIT=ANC_MET_UNIT,
     &         STATUS=ANC_MET_STATUS,
     &         FORM=ANC_MET_FORM,
     &         ACCESS=ANC_MET_ACCESS,
     &         RECL=ANC_MET_RECL,
     &         IOSTAT=io_err, 
     &         ERR=2025
     &       ) 

         call message(
     &                 '--',
     &                'Using GDAS Analysis file: '// file ,
     &                 0, 4
     &               )

         anc_met_lun = ANC_MET_UNIT
         RTN_NCEP = 0
         goto 225

c ------ Failed to open 
2025     continue 
         call message( 
     &                 FUNCTION,
     &                'FAILED: Could not open GDAS1 file. ',
     &                 0, level 
     &               )

      endif
225   continue
      if( debug.ne.0 ) then
         call strcompress( file, remove_all, len_file )
         write(*,*) '  Met Obs. file: ',file(1:len_file)
      endif
      

C ANCILLARY FILES: OZONE

c --- get the file requirement
      keyname = 'ANC_OZONE_REQ'
      keyindex = 1
      status = hdrgetkeystr( filename, keyname, keyindex, req )
      if( status.lt.0 ) req = 'OPTIONAL'
      level = 3
      if( req(1:3).eq.'MAN' ) level = 2

c --- get file access information 
      keyname = 'ANC_OZONE_NAME'
      keyindex = 1
      status = hdrgetkeystr( filename, keyname, keyindex,file )

c --- check for error reading info file
      if( status.lt.0 ) then 
         call message( 
     &                 FUNCTION,
     &                'FAILED: Read of Ozone file info. Will not' //
     &                       ' use the file for processing.',
     &                 0, level 
     &               )

      else
                   
c ------ issue the OPEN request
         OPEN( 
     &         FILE=file,
     &         UNIT=ANC_OZONE_UNIT,
     &         STATUS=ANC_OZONE_STATUS,
     &         FORM=ANC_OZONE_FORM,
     &         ACCESS=ANC_OZONE_ACCESS,
     &         RECL=ANC_OZONE_RECL,
     &         IOSTAT=io_err, 
     &         ERR=2050
     &       ) 

         call message(
     &                 '--',
     &                'Using Ozone Analysis file: '// file ,
     &                 0, 4
     &               )

         ozone_lun = ANC_OZONE_UNIT
         goto 250

c ------ Failed to open 
2050     continue 
         call message( 
     &                 FUNCTION,
     &                'FAILED: Could not open Ozone file. ',
     &                 0, level 
     &               )

      endif
250   continue
      if( debug.ne.0 ) then
         call strcompress( file, remove_all, len_file )
         write(*,*) ' Using Ozone file: ',file(1:len_file)
      endif
      


C INPUT FILES: CLOUD MASK 

c --- get the file requirement
      keyname = 'MOD35_MASK_REQ'
      keyindex = 1
      status = hdrgetkeystr( filename, keyname, keyindex, req )
      if( status.lt.0 ) req = 'OPTIONAL'
      level = 3
      if( req(1:3).eq.'MAN' ) level = 2

c --- get file access information 
      keyname = 'MOD35_MASK_NAME'
      keyindex = 1
      status = hdrgetkeystr( filename, keyname, keyindex,file )
      call strcompress( file, remove_all, len ) 


c --- check for error reading info file
      if( status.lt.0 ) then 
         call message( 
     &                 FUNCTION,
     &                'FAILED: Read of Cloud Mask file info . ',
     &                 0, level 
     &               )

      else
                   
c ------ issue the OPEN request
         OPEN( 
     &         FILE=file(1:len),
     &         UNIT=MOD35_MASK_UNIT,
     &         STATUS=MOD35_MASK_STATUS,
     &         ACCESS=MOD35_MASK_ACCESS,
     &         RECL=l1b_1km_neles,
     &         IOSTAT=io_err, 
     &         ERR=2075
     &       ) 

         call message(
     &                 '--',
     &                'Using Cloud Mask file: '// file ,
     &                 0, 4
     &               )


         mask_lun = MOD35_MASK_UNIT
         goto 275

c ------ Failed to open 
2075     continue 
         call message( 
     &                 FUNCTION,
     &                'FAILED: Could not open Cloud Mask file. ',
     &                 0, level 
     &               )

      endif
275   continue
      if( debug.ne.0 ) then
         call strcompress( file, remove_all, len_file )
         write(*,*) '  Cloud Mask file: ',file(1:len_file)
      endif

C INPUT FILES: CLOUD MASK QC FILE

c --- get the file requirement
      keyname = 'MOD35_QA_REQ'
      keyindex = 1
      status = hdrgetkeystr( filename, keyname, keyindex, req )
      if( status.lt.0 ) req = 'OPTIONAL'
      level = 3
      if( req(1:3).eq.'MAN' ) level = 2

c --- get file access information 
      keyname = 'MOD35_QA_NAME'
      keyindex = 1
      status = hdrgetkeystr( filename, keyname, keyindex,file )
      call strcompress( file, remove_all, len ) 


c --- check for error reading info file
      if( status.lt.0 ) then 
         call message( 
     &                 FUNCTION,
     &                'FAILED: Read of Cloud Mask QC file info . ',
     &                 0, level 
     &               )

      else
                   
c ------ issue the OPEN request
         OPEN( 
     &         FILE=file(1:len),
     &         UNIT=MOD35_QA_UNIT,
     &         STATUS=MOD35_QA_STATUS,
     &         ACCESS=MOD35_QA_ACCESS,
     &         RECL=l1b_1km_neles,
     &         IOSTAT=io_err, 
     &         ERR=3000
     &       ) 

         call message(
     &                 '--',
     &                'Using Cloud Mask QC file: '// file ,
     &                 0, 4
     &               )


         mask_qa_lun = MOD35_QA_UNIT
         goto 300

c ------ Failed to open 
3000     continue 
         call message( 
     &                 FUNCTION,
     &                'FAILED: Could not open Cloud Mask QC file. ',
     &                 0, level 
     &               )

      endif
300   continue
      if( debug.ne.0 ) then
         call strcompress( file, remove_all, len_file )
         write(*,*) '  Cloud Mask QC file: ',file(1:len_file)
      endif

C OUTPUT FILES: AEROSOL OUTPUT BINARY FILE

c --- get the file requirement
      keyname = 'MOD04_REQ'
      keyindex = 1
      status = hdrgetkeystr( filename, keyname, keyindex, req )
      if( status.lt.0 ) req = 'OPTIONAL'
      level = 3
      if( req(1:3).eq.'MAN' ) level = 2

c --- get file access information
      keyname = 'MOD04_NAME'
      keyindex = 1
      status = hdrgetkeystr( filename, keyname, keyindex,file )
      call strcompress( file, remove_all, len )


c --- check for error reading info file
      if( status.lt.0 ) then
         call message(
     &                 FUNCTION,
     &                'FAILED: Read of Aerosol output file info . ',
     &                 0, level
     &               )

      else

c ------ issue the OPEN request
         OPEN(
     &         FILE=file(1:len),
     &         UNIT=MOD04_UNIT,
     &         STATUS=MOD04_STATUS,
     &         ACCESS=MOD04_ACCESS,
     &         RECL= (l1b_1km_neles / 10) * 4,
     &         IOSTAT=io_err,
     &         ERR=3050
     &       )

         call message(
     &                 '--',
     &                'Creating Profiles Output file: '// file ,
     &                 0, 4
     &               )

         mod04_lun = MOD04_UNIT
         goto 350

c ------ Failed to open
3050     continue
         call message(
     &                 FUNCTION,
     &                'FAILED: Could not open Aerosol output file. ',
     &                 0, level
     &               )

      endif
350   continue
      if( debug.ne.0 ) then
         call strcompress( file, remove_all, len_file )
         write(*,*) '  Aerosol Output file: ',file(1:len_file)
      endif

C OUTPUT FILES: AEROSOL OUTPUT HEADER FILE

c --- get the file requirement
      keyname = 'MOD04_REQ'
      keyindex = 1
      status = hdrgetkeystr( filename, keyname, keyindex, req )
      if( status.lt.0 ) req = 'OPTIONAL'
      level = 3
      if( req(1:3).eq.'MAN' ) level = 2

c --- get file access information
      keyname = 'MOD04_HDR'
      keyindex = 1
      status = hdrgetkeystr( filename, keyname, keyindex,file )
      call strcompress( file, remove_all, len )


c --- check for error reading info file
      if( status.lt.0 ) then
         call message(
     &                 FUNCTION,
     &                'FAILED: Read of Aerosol output header info . ',
     &                 0, level
     &               )

      else

c ------ issue the OPEN request
         OPEN(
     &         FILE=file(1:len),
     &         UNIT=MOD04_HDR_UNIT,
     &         STATUS=MOD04_HDR_STATUS,
     &         ACCESS=MOD04_HDR_ACCESS,
     &         IOSTAT=io_err,
     &         ERR=3075
     &       )

         hdr_lun = MOD04_HDR_UNIT
         goto 375

c ------ Failed to open
3075     continue
         call message(
     &                 FUNCTION,
     &                'FAILED: Could not open output Aerosol header file. ',
     &                 0, level
     &               )

      endif
375   continue
      if( debug.ne.0 ) then
         call strcompress( file, remove_all, len_file )
         write(*,*) '  Aerosol Output header file: ',file(1:len_file)
      endif

C RUNTIME DEBUG FILE

      if (debug .ne. 0) then
c ---   get the file requirement
        keyname = 'DEBUG_REQ'
        keyindex = 1
        status = hdrgetkeystr( filename, keyname, keyindex, req )
        if( status.lt.0 ) req = 'OPTIONAL'
        level = 3
        if( req(1:3).eq.'MAN' ) level = 2

c ---   get file access information
        keyname = 'DEBUG_NAME'
        keyindex = 1
        status = hdrgetkeystr( filename, keyname, keyindex, file )
        call strcompress( file, remove_all, len )

c ---   check for error reading info file
        if( status.lt.0 ) then
         call message(
     &                 FUNCTION,
     &                'FAILED: Read of RunTime Debug file info. ',
     &                 0, level
     &               )

        else

c ------ issue the OPEN request
           OPEN(
     &           FILE=file(1:len),
     &           UNIT=DEBUG_UNIT,
     &           STATUS=DEBUG_STATUS,
     &           FORM=DEBUG_FORM,
     &           ACCESS=DEBUG_ACCESS,
     &           IOSTAT=io_err,
     &           ERR=4000
     &         )

           debug_lun = DEBUG_UNIT
           goto 400

c ------   Failed to open
4000       continue
           call message(
     &                 FUNCTION,
     &                'FAILED: Open RunTime Debug file. ',
     &                 0, level
     &                 )
        endif
400     continue
      endif


      if( debug.ne.0 ) then
         call strcompress( file, remove_all, len_file )
         write(*,*) '  Debug file: ',file(1:len_file)
      endif


      return
      end
