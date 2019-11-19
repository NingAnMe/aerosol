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
      SUBROUTINE STRCOMPRESS( STRING, REMOVE_ALL, LENGTH )

C-------------------------------------------------------------------
C !F77
C
C !DESCRIPTION:
C     This routine returns a string with all whitespace
C     (spaces and tabs) compressed to a single space,
C     or optionally all whitespace completely removed.
C
C !INPUT PARAMETERS:
C     STRING        On input, the string to be compressed.
C     REMOVE_ALL    Set this .TRUE. to remove all whitespace.
C                   Normally all whitespace (spaces and tabs) is
C                   compressed to a single space.
C
C !OUTPUT PARAMETERS:
C     STRING        On output, the compressed string.
C     LENGTH        The true length of the compressed string
C                   (i.e. the index of the last character).
C
C !REVISION HISTORY:
C
C !TEAM-UNIQUE HEADER:
C     Developed by the MODIS Group, CIMSS/SSEC, UW-Madison.
C
C !DESIGN NOTES:
C     Original version by Liam.Gumley@ssec.wisc.edu
C
C !END
C--------------------------------------------------------------------

      IMPLICIT NONE
      SAVE
      
c ... Input arguments  

      CHARACTER*(*) string
      LOGICAL remove_all

c ... Output arguments

      INTEGER length
      
c ... Local scalars     

      INTEGER i, j, space
      CHARACTER*1 tempchar
      
c ... Loop through the string to find valid text

      j = 1
      space = 0
      
      do i = 1, len( string )

        if( string( i : i ) .ne. char( 32 ) .and.
     &      string( i : i ) .ne. char( 9 ) ) then

          tempchar = string( i : i )
          string( j : j ) = tempchar
          space = 0
          j = j + 1

        elseif( .not. remove_all .and. space .eq. 0 ) then

          tempchar = string( i : i )
          string( j : j ) = tempchar
          space = 1
          j = j + 1

        endif

      end do
      
      length = j - 1
      if( space .eq. 1 ) length = length - 1
      
c ... Set the rest of the string to blank characters

      do i = length + 1, len( string )
        string( i : i ) = char( 32 )
      end do
            
      END
