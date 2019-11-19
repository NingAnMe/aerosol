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
      integer function hdrgetkeydbl( filename,
     &                                keyname,
     &                                keyindex,
     &                                keyvalue )
      implicit none
      save
 
c --- parameters     
      character*(*)  	filename
      character*(*)  	keyname
      integer	     	keyindex
      double precision 	keyvalue

c --- external functions
      integer	     	hdrgetkeystr

c --- internal variables
      character*512  	string
      double precision	d_value
      integer	     	status
      integer	     	len_keyname
      logical	     	remove_all

c --- get the length of the keyname
      remove_all = .FALSE.
      call strcompress( keyname, remove_all, len_keyname)

c --- check if the key exists and if so, get number of parameters
      status = hdrgetkeystr( filename, keyname, keyindex, string )
      if( status.lt.0 ) then
         keyvalue = 0 
         hdrgetkeydbl = -1

      else
         READ(string,*) d_value
         keyvalue = d_value
         hdrgetkeydbl = 0

      endif

      return
      end
