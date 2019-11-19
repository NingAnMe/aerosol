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
      integer function hdrgetkeystr(
     &                                filename,
     &                                keyname,
     &                                keyindex,
     &                                keystring
     &                              )
      implicit none
      save

c --- constants
      integer		HDR_UNIT
      parameter		(HDR_UNIT=1)

c --- parameters
      character*(*)	filename
      character*(*)	keyname
      integer		keyindex
      character*(*)	keystring

c --- external functions

c --- internal variables
      character*512	record
      character*512	records(200)
      character*128     parms(100)
      character*256	file
      character*256	lastfile
      character*256     string
      character*256	key
      integer		i
      integer		j, jj
      integer		init
      integer		len_file
      integer		len_key
      integer		len_parms
      integer		len_string
      integer		len_record
      integer		len_records
      integer		io_err
      integer		num_records
      integer		num_parms
      integer		equal
      integer		found
      integer		nrec
      integer		left_bracket, right_bracket
      logical		remove_all
      logical 		inparm
      logical 		alist
      logical 		complete

      data		init/0/

c --- funtion initialization
      hdrgetkeystr = -1
      remove_all = .FALSE.

c --- copy the file
      file = filename
      call strcompress( file, remove_all, len_file )

c --- check initialization
      complete = .TRUE.
      if( file.ne.lastfile ) then

         OPEN( 
     &         FILE=file,
     &         UNIT=HDR_UNIT,
     &         IOSTAT=io_err, 
     &         ERR=9991
     &       )

         num_records= 0
1        continue

c ------ read a record
         READ(
     &         UNIT=HDR_UNIT,
     &         FMT='(A512)',
     &         ERR=9991,
     &         END=100  
     &       ) record


c ------ compress the record
         call strcompress( record, remove_all, len_record )

c ------ If this is a null-string, then grap the next line.
         if (len_record .le. 0) go to 1

c ------ scan for list notation
         left_bracket = index( record, '{' )
         right_bracket = index( record, '}' )

c ------ check for a left bracket
         if( left_bracket.gt.0 ) then
            num_records = num_records+1
            records(num_records) = record(1:len_record)
            if( right_bracket.gt.0 ) then
               complete = .TRUE.
            else
               complete = .FALSE.
            endif
            
         else
            if( .NOT.complete ) then
               call strcompress( records(num_records), 
     &                           remove_all,
     &                           len_records
     &                         )
               records(num_records) = 
     &            records(num_records)(1:len_records)//' '//
     &            record(1:len_record)
               if( right_bracket.gt.0 ) complete = .TRUE.
            else
               num_records = num_records+1
               records(num_records) = record(1:len_record)
            endif

         endif
 
         goto 1

c ------ done reading records
100      continue

c ------ close the file
         CLOSE(
     &         UNIT=HDR_UNIT,
     &         STATUS='KEEP'
     &        )

c ------ save the name
         lastfile = file

c ------ check for records
         if( num_records.eq.0 ) goto 9991

      endif

c --- local copy of key name
      key = keyname
      call strcompress( key, remove_all, len_key )

c --- loop through records looking for key name
      found = 0
      nrec = 0
1111  continue

      nrec = nrec+1
      if( nrec.le.num_records ) then

         if( records(nrec)(1:4).ne.'ENVI' .and.
     &       records(nrec)(1:1).ne.'#'          ) then
         
c --------- does key exist in the record
            if( index( records(nrec), key(1:len_key) ).ne.0 ) then
 
c ------------ found the key, locate the '=' character           
               equal = index( records(nrec), '=' ) 
               call strcompress( records(nrec), remove_all, len_record )

c ------------ check for brackets
               left_bracket = index( records(nrec), '{' )
               right_bracket = index( records(nrec), '}' )
               if( left_bracket.gt.0 ) then 
                  alist = .TRUE.
                  complete = .TRUE.
               endif

c ------------ split the name from the record
               string = records(nrec)(equal+1:len_record)
               call strcompress( string, remove_all, len_string )

c -----------  scan for number of parameters
               num_parms = 0
               inparm = .FALSE.

               j = 1
               do jj = 1, len_string
               if( string(jj:jj).eq.',' .and. inparm ) then
                  num_parms = num_parms+1
                  parms(num_parms) = string(j:jj-1)
                  call strcompress
     &               (parms(num_parms),remove_all,len_parms)
                  inparm = .FALSE.
               else
                  if( .NOT.inparm ) then
                     j = jj
                     inparm = .TRUE.
                  endif
               endif
               enddo

c ------------ end of line check
               if( inparm ) then
                  num_parms = num_parms+1
                  if( string(jj:jj).eq.',' ) jj = jj-1
                  parms(num_parms) = string(j:jj)
                  call strcompress
     &               (parms(num_parms),remove_all,len_parms)
                  inparm = .FALSE.
               endif

c ------------ set found flag 
               if( found.eq.0 ) found = nrec

            endif

         endif
         goto 1111
    
      endif

c --- check if the key was found
      if( found.gt.0 ) then
       
c ------ return the requested 
         if( keyindex.le.num_parms ) then
            len_parms = LEN( parms(keyindex) )
            do i = 1, len_parms
            if( parms(keyindex)(i:i).eq.'{' .or.
     &          parms(keyindex)(i:i).eq.'}'
     &        ) parms(keyindex)(i:i) = ' '
            enddo
            if( parms(keyindex)(1:1).eq.' ' ) then
               parms(keyindex) = parms(keyindex)(2:)
            endif
            call strcompress( parms(keyindex),remove_all,len_parms )
            keystring = parms(keyindex)
            hdrgetkeystr = num_parms
         else
            WRITE (*,*) 'invalid index for ', keyname
            keystring = ' '
         endif
            
      else
         keystring = ' '
      endif
      return
         
9991  continue
      return

      end   
