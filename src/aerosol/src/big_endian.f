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
        logical function big_endian()

c 
c       Hal Wolf gets the credit for writing this piece of
c        code which will tell you whether you are working
c        on a big or little endian machine.
c
c       INPUTS:  NONE
c       OUTPUT:  big_endian - true - big_endian machine
c                             false - little_endian machine
c        

        implicit none
        integer long(1)
        integer*2 short(2)
        equivalence (long,short)
        
        save

        long(1)  = 0
        short(1) = 0
        short(2) = 1
        if(long(1) .eq. 1) then
           big_endian = .true.
        else
           big_endian = .false.
        endif
        end
