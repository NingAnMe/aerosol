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
      subroutine message( nam, txt, val, lvl )
      implicit none
      save
 
c --- parameters
      character*(*)  nam
      character*(*)  txt
      integer	     val
      integer	     lvl

c --- internal variables
      character*512  string
      character*256  name
      character*256  text
      integer	     len_name
      integer	     len_text
      integer	     len_string
      logical	     remove_all
      integer        itmp

      remove_all = .FALSE.

      name = nam
      call strcompress( name, remove_all, len_name )
      text = txt
      call strcompress( text, remove_all, len_text )
      itmp = val

      string = name(1:len_name)//':'//
     &         text(1:len_text)
      call strcompress( string, remove_all, len_string )

      WRITE(*,*) string(1:len_string)

      if( lvl.eq.2 ) call exit(1)
    
      return
      end
